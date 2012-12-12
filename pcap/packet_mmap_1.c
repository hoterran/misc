#include <stdio.h>
#include <pcap.h>
#include <linux/types.h>
#include <linux/filter.h>

#include <sys/socket.h>
#include <string.h>
#include <linux/if_packet.h>
#include <net/if.h>
#include <sys/ioctl.h>      //ioctl()
#include <arpa/inet.h>
#include <unistd.h>
#include <linux/if_ether.h>

#include <sys/poll.h>
#include <sys/mman.h>

#ifndef P
#define P(format, ...) do \
    { \
        fprintf(stdout, "%s %s %d " format "\n", __FILE__, __FUNCTION__, __LINE__, ##__VA_ARGS__); \
        fflush(stdout); \
    } \
    while (0);
#endif

void PrintPacket(const char* ip)
{
    //do something
}

bool ExpressionToFilter(const char* exp, struct sock_fprog* f)
{
    pcap_t* pHandle = pcap_open_dead(DLT_EN10MB, 1500);
    if (NULL==pHandle)
    {
        P("pcap_open_dead error");
        return false;
    }
    struct bpf_program st_bpf;
    if (-1==pcap_compile(pHandle, &st_bpf, (char*)exp, 1, 0x00ffffff))
    {
        pcap_close(pHandle);
        P("compile error");
        return false;
    }
    f->len = (unsigned short)st_bpf.bf_len;
    f->filter = (struct sock_filter*)st_bpf.bf_insns;
    pcap_close(pHandle);
    return true;
}

void FreeFilter(struct sock_fprog* f)
{
    if (NULL!=f->filter)
    {
        struct bpf_program st_bpf;
        st_bpf.bf_len = f->len;
        st_bpf.bf_insns = (struct bpf_insn*)f->filter;
        pcap_freecode(&st_bpf); //注意：这种转换方法很不安全，很可能导致程序崩溃
        f->filter = NULL;
    }
}

bool SetPromisc(int sock, const char* dev)
{
    struct ifreq ethreq;        //在<net/if.h>中定义
    strncpy(ethreq.ifr_name, dev, IFNAMSIZ);
    if (ioctl(sock, SIOCGIFFLAGS, &ethreq)==-1)    //SIOCGIFFLAGS 获取标志，在<linux/sockios.h>中定义
    {
        P("ioctl error");
        return false;
    }
    ethreq.ifr_flags |= IFF_PROMISC;
    if (ioctl(sock, SIOCSIFFLAGS, &ethreq)==-1) 
    {
        P("ioctl error");
        return false;
    }
    return true;
}

bool BindDevice(int sock, const char* dev)
{
    struct sockaddr_ll addr;    //#include <linux/if_packet.h>
    struct ifreq ifr;           //#include <net/if.h>
    memset(&ifr, 0, sizeof(ifr));
    strncpy(ifr.ifr_name, dev, sizeof(ifr.ifr_name));
    if (ioctl(sock, SIOCGIFINDEX, &ifr) == -1) 
    {
        P("ioctl error");
        return false;
    }
    /* bind the packet socket */
    memset(&addr, 0, sizeof(addr));
    addr.sll_family = AF_PACKET;
    addr.sll_protocol = htons(0x03);
    addr.sll_ifindex = ifr.ifr_ifindex;
    addr.sll_hatype = 0;
    addr.sll_pkttype = 0;
    addr.sll_halen = 0;
    if (-1 == bind(sock, (struct sockaddr *)&addr, sizeof(addr))) 
    {
        P("bind error");
        return false;
    }
    return true;
}

void test(const char* exp, const char* dev)
{
    int sock = socket(PF_PACKET, SOCK_RAW, htons(ETH_P_IP));
    if (sock<=0)
    {
        P("socket error");
        return;
    }
    //设置缓冲区
    #define PER_PACKET_SIZE 2048
    const int BUFFER_SIZE = 1024*1024*16; //16MB的缓冲区
    struct tpacket_req req;
    req.tp_block_size = 4096;
    req.tp_block_nr = BUFFER_SIZE/req.tp_block_size;
    req.tp_frame_size = PER_PACKET_SIZE;
    req.tp_frame_nr = BUFFER_SIZE/req.tp_frame_size;
    if (-1==setsockopt(sock, SOL_PACKET, PACKET_RX_RING, &req, sizeof(struct tpacket_req)))
    {
        perror("setsockopt");
        P("set PACKET_RX_RING error");
        close(sock);
        return;
    }
    //映射缓冲区
    char* pBuffer = (char*)mmap(0, BUFFER_SIZE, PROT_READ|PROT_WRITE, MAP_SHARED, sock, 0);
    if (MAP_FAILED==pBuffer)
    {
        P("mmap error");
        close(sock);
        return;
    }
    //注意：一定要先映射后再绑定，否则会有问题
    // 问题的详细描述可参考：https://lists.linux-foundation.org/pipermail/bugme-new/2003-October/009110.html
    if (!SetPromisc(sock, dev))
    {
        P("SetPromisc [%s] error", dev);
        memset(&req, 0, sizeof(req));
        if (-1==setsockopt(sock, SOL_PACKET, PACKET_RX_RING, &req, sizeof(req)))
        {
            P("set map buffer to 0 error!");
        }
        close(sock);
        return;
    }
    if (!BindDevice(sock, dev))
    {
        P("bind [%s] error", dev);
        memset(&req, 0, sizeof(req));
        if (-1==setsockopt(sock, SOL_PACKET, PACKET_RX_RING, &req, sizeof(req)))
        {
            P("set map buffer to 0 error!");
        }
        close(sock);
        return;
    }
    //设置过滤器
    int nExpLen = strlen(exp);
    struct sock_fprog Filter;
    memset(&Filter, 0, sizeof(struct sock_fprog));
    if (nExpLen>0)
    {
        if (ExpressionToFilter(exp, &Filter))
        {
            if (setsockopt(sock, SOL_SOCKET, SO_ATTACH_FILTER,
                    &Filter, sizeof(Filter))<0)
            {
                perror("setsockopt");
            }
        }
    }
    //
    struct pollfd pfd;
    int nIndex = 0;
    for (; ; )
    {
        for (; ; )
        {
            struct tpacket_hdr* pHead = (struct tpacket_hdr*)(pBuffer + nIndex*PER_PACKET_SIZE);
            if (pHead->tp_len<34 || pHead->tp_len>1614)
            {
                break;
            }
            PrintPacket((char*)pHead+pHead->tp_net);
            pHead->tp_len = 0;
            pHead->tp_status = TP_STATUS_KERNEL;
            //注意：pHead->tp_status这个变量并不能真正反应出包的处理状态，
            //在有的服务器上 TP_STATUS_USER(1)代表包可用, TP_STATUS_KERNEL(0)代表包不可用
            //但是在有的服务器上，这个标志变量无效
            //对于这个问题我还没找到原因
            nIndex++;
            if (nIndex>=BUFFER_SIZE/PER_PACKET_SIZE)
            {
                nIndex = 0;
            }
        }
        //
        pfd.fd = sock;
        pfd.events = POLLIN | POLLERR;
        pfd.revents = 0;
        switch (poll(&pfd, 1, 1000))
        {
        case -1:
            perror("poll");
            P("poll error");
            goto EndWhile;
            break;
        case 0:
            P("time out");
            continue;
            break;
        }
    }
EndWhile:
    if (-1==munmap(pBuffer, BUFFER_SIZE))
    {
        P("unmap error!");
    }
    memset(&req, 0, sizeof(req));
    if (-1==setsockopt(sock, SOL_PACKET, PACKET_RX_RING, &req, sizeof(req)))
    {
        P("set map buffer to 0 error!");
    }
    close(sock);
    sock = -1;
    //
    if (nExpLen>0)
    {
        FreeFilter(&Filter);
    }
}

int main()
{
    test("tcp or udp", "eth1");
    return 1;
}

/*code end here*/
