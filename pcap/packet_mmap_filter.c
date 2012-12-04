#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/mman.h>
#include <linux/if_packet.h>
#include <poll.h>
#include <net/ethernet.h> /* the L2 protocols */
#include <netinet/ip.h>
#include <netinet/tcp.h>
#include <linux/filter.h>
#include <sys/ioctl.h>
#include <string.h>

#include <netinet/in.h>
#include <arpa/inet.h>

#define PER_PACKET_SIZE 2048

/*
    here socket use raw, and not setsocketopt net card, so get raw ip
*/
int mode = SOCK_RAW;

void CallBackPacket(uint len, uint caplen, char *data)
{
    const struct sll_header *sll;
    const struct ether_header *ether_header;
    const struct ip *ip;
    unsigned short packet_type;

    if (mode == SOCK_RAW) {
        packet_type = ETHERTYPE_IP; //This is raw ip
        ip = (const struct ip *) data;
    } else if (mode == SOCK_DGRAM) {
        ether_header = (struct ether_header *) data;
        packet_type = ntohs(ether_header->ether_type);
        ip = (const struct ip *) (data + sizeof(struct ether_header));
    }

    char src[16], dst[16], *addr = NULL;
    char incoming;
                
    addr = inet_ntoa(ip->ip_src);
    strncpy(src, addr, 15);
    src[15] = '\0';
                
    addr = inet_ntoa(ip->ip_dst);
    strncpy(dst, addr, 15);
    dst[15] = '\0';

    printf("Recv A Packet %u %u %s %s.\n", len , caplen, src, dst);
}

int main()
{
    int fd = 0, ret = 0;
    char *buff = NULL;

    fd = socket(PF_PACKET, mode, htons(ETH_P_ALL));
    if(fd<0)
    {
        perror("socket");
        goto failed_2;
    }

    struct tpacket_req req;
    const int BUFFER_SIZE = 1024*1024*16; //16MB的缓冲区

    req.tp_block_size = 4096;
    req.tp_block_nr = BUFFER_SIZE/req.tp_block_size;
    req.tp_frame_size = PER_PACKET_SIZE;
    req.tp_frame_nr = BUFFER_SIZE/req.tp_frame_size;

    ret = setsockopt(fd, SOL_PACKET, PACKET_RX_RING, (void *)&req, sizeof(req));
    if(ret<0)
    {
        perror("setsockopt");
        goto failed_2;
    }

    /* filter
        tcpdump -dd 'tcp port 3306'
    */

    struct sock_filter BPF_code[] = {
        { 0x28, 0, 0, 0x0000000c },
        { 0x15, 0, 6, 0x000086dd },
        { 0x30, 0, 0, 0x00000014 },
        { 0x15, 0, 15, 0x00000006 },
        { 0x28, 0, 0, 0x00000036 },
        { 0x15, 12, 0, 0x00000cea },
        { 0x28, 0, 0, 0x00000038 },
        { 0x15, 10, 11, 0x00000cea },
        { 0x15, 0, 10, 0x00000800 },
        { 0x30, 0, 0, 0x00000017 },
        { 0x15, 0, 8, 0x00000006 },
        { 0x28, 0, 0, 0x00000014 },
        { 0x45, 6, 0, 0x00001fff },
        { 0xb1, 0, 0, 0x0000000e },
        { 0x48, 0, 0, 0x0000000e },
        { 0x15, 2, 0, 0x00000cea },
        { 0x48, 0, 0, 0x00000010 },
        { 0x15, 0, 1, 0x00000cea },
        { 0x6, 0, 0, 0x0000ffff },
        { 0x6, 0, 0, 0x00000000 }
    };
    struct sock_fprog Filter; 
    Filter.len = 20;
    Filter.filter = BPF_code;

     if(setsockopt(fd, SOL_SOCKET, SO_ATTACH_FILTER, &Filter, sizeof(Filter))<0){
         perror("setsockopt");
         close(fd);
         exit(1);
       }

    buff = (char *)mmap(0, BUFFER_SIZE, PROT_READ|PROT_WRITE, MAP_SHARED, fd, 0);
    if(buff == MAP_FAILED)
    {
        perror("mmap");
        goto failed_2;
    }

    int nIndex=0, i=0;
    while(1)
    {
        //这里在poll前先检查是否已经有报文被捕获了
        struct tpacket_hdr* pHead = (struct tpacket_hdr*)(buff+ nIndex*PER_PACKET_SIZE);
        //如果frame的状态已经为TP_STATUS_USER了，说明已经在poll前已经有一个数据包被捕获了，如果poll后不再有数据包被捕获，那么这个报文不会被处理，这就是所谓的竞争情况。
        if(pHead->tp_status == TP_STATUS_USER)
            goto process_packet;

        //poll检测报文捕获
        struct pollfd pfd;
        pfd.fd = fd;
        //pfd.events = POLLIN|POLLRDNORM|POLLERR;
        pfd.events = POLLIN;
        pfd.revents = 0;
        ret = poll(&pfd, 1, -1);
        if(ret<0)
        {
            perror("poll");
            goto failed_1;
        }

process_packet:
        //尽力的去处理环形缓冲区中的数据frame，直到没有数据frame了
        for(i=0; i<req.tp_frame_nr; i++)
        {
            struct tpacket2_hdr* pHead = (struct tpacket2_hdr*)(buff+ nIndex*PER_PACKET_SIZE);

            //XXX: 由于frame都在一个环形缓冲区中，因此如果下一个frame中没有数据了，后面的frame也就没有frame了
            if(pHead->tp_status == TP_STATUS_KERNEL)
                break;

            //处理数据frame
            CallBackPacket(pHead->tp_len, pHead->tp_snaplen, (char*)pHead + pHead->tp_net);

            //重新设置frame的状态为TP_STATUS_KERNEL
            pHead->tp_len = 0;
            pHead->tp_status = TP_STATUS_KERNEL;

            //更新环形缓冲区的索引，指向下一个frame
            nIndex++;
            nIndex%=req.tp_frame_nr;
        }
    }

success:
    close(fd);
    munmap(buff, BUFFER_SIZE);
    return 0;

failed_1:
    munmap(buff, BUFFER_SIZE);
    
failed_2:
    close(fd);
    return -1;
}
