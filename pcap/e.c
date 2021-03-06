#include <pcap.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netinet/if_ether.h>

#define SIZE_ETHERNET   14

struct sniff_ethernet {
        u_char ether_dhost[ETHER_ADDR_LEN]; /* Destination host address */
        u_char ether_shost[ETHER_ADDR_LEN]; /* Source host address */
        u_short ether_type; /* IP? ARP? RARP? etc */
};

/* IP header */
struct sniff_ip {
        u_char ip_vhl;          /* version << 4 | header length >> 2 */
        u_char ip_tos;          /* type of service */
        u_short ip_len;         /* total length */
        u_short ip_id;          /* identification */
        u_short ip_off;         /* fragment offset field */
#define IP_RF 0x8000            /* reserved fragment flag */
#define IP_DF 0x4000            /* dont fragment flag */
#define IP_MF 0x2000            /* more fragments flag */
#define IP_OFFMASK 0x1fff       /* mask for fragmenting bits */
        u_char ip_ttl;          /* time to live */
        u_char ip_p;            /* protocol */
        u_short ip_sum;         /* checksum */
        struct in_addr ip_src,ip_dst; /* source and dest address */
};
#define IP_HL(ip)               (((ip)->ip_vhl) & 0x0f)
#define IP_V(ip)                (((ip)->ip_vhl) >> 4)

struct sniff_tcp {
        uint16_t th_sport;      /* source port */
        uint16_t th_dport;      /* destination port */
        uint32_t th_seq;        /* sequence number */
        uint32_t th_ack;        /* acknowledgement number */

        u_char th_offx2;        /* data offset, rsvd */
#define TH_OFF(th)      (((th)->th_offx2 & 0xf0) >> 4)
        u_char th_flags;
#define TH_FIN 0x01
#define TH_SYN 0x02
#define TH_RST 0x04
#define TH_PUSH 0x08
#define TH_ACK 0x10
#define TH_URG 0x20
#define TH_ECE 0x40
#define TH_CWR 0x80
#define TH_FLAGS (TH_FIN|TH_SYN|TH_RST|TH_ACK|TH_URG|TH_ECE|TH_CWR)
        u_short th_win;         /* window */
        u_short th_sum;         /* checksum */
        u_short th_urp;         /* urgent pointer */
};

const struct sniff_ethernet *ethernet; /* The ethernet header */
const struct sniff_ip *iphdr; /* The IP header */
const struct sniff_tcp *tcphdr; /* The TCP header */
const char *payload; /* Packet payload */

u_int size_iphdr;
u_int size_tcphdr;

void my_callback(u_char* useless, const struct pcap_pkthdr* pkthdr,
const u_char* s) {
	static int count = 1;

	ethernet = (struct sniff_ethernet*)(s);

        iphdr = (struct sniff_ip *)(s + SIZE_ETHERNET);
        size_iphdr = IP_HL(iphdr)*4;
        if (size_iphdr < 20) {
                printf("   * Invalid IP header length: %u bytes\n", size_iphdr);
                return;
        }

        tcphdr = (struct sniff_tcp *)(s + SIZE_ETHERNET + size_iphdr);
        size_tcphdr = TH_OFF(tcphdr)*4;
        if (size_tcphdr < 20) {
                printf("   * Invalid TCP header length: %u bytes\n", size_tcphdr);
                return;
        }

	fprintf(stdout, "%d-%s-%s-%d-%d\n", count, s, useless, pkthdr->caplen, pkthdr->len);
	fflush(stdout);
	count++;
}

int main(int argc, char** argv)
{
	int i;
	char *dev;
	char errbuf[PCAP_ERRBUF_SIZE];
	pcap_t *descr;
	const u_char *packet;
	struct pcap_pkthdr hdr;
	struct ether_header *epthr;
	struct bpf_program fp;
	bpf_u_int32	maskp;
	bpf_u_int32	netp;

	dev = pcap_lookupdev(errbuf);

	pcap_lookupnet(dev, &netp, &maskp, errbuf);

	descr = pcap_open_live("any", 100, 0, -1, errbuf);

	pcap_compile(descr, &fp, argv[1], 0, netp);

	pcap_setfilter(descr, &fp);

	pcap_loop(descr, -1, my_callback, "kkk");

	fprintf(stdout, "aoaoa");
	
	return 0;
}
