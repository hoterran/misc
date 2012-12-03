#include <pcap.h>
#include <pcap-bpf.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netinet/if_ether.h>

#ifdef BPF_MAJOR_VERSION
#undef BPF_MAJOR_VERSION 
#endif

struct bpf_program myFilter;

int user_filter(const char *exp) {

    pcap_t *p = pcap_open_dead(DLT_LINUX_SLL, 65535);

    printf("%d\n", pcap_compile(p, &myFilter, exp, 1, 0x00ffffff));

    pcap_close(p);

    return 0;
}

void my_callback(u_char* useless, const struct pcap_pkthdr* pkthdr,
const u_char* packet) {

	static int count = 1;
    if (bpf_filter(myFilter.bf_insns, (unsigned char*)packet, pkthdr->len, pkthdr->caplen)) {
        fprintf(stdout, "1-%d-%s-%s-%d-%d\n", count, packet, useless, pkthdr->caplen, pkthdr->len);
        fflush(stdout);
    } else {
        fprintf(stdout, "2-%d-%s-%s-%d-%d\n", count, packet, useless, pkthdr->caplen, pkthdr->len);
        fflush(stdout);
    }
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

	descr = pcap_open_live("any", 200, 0, -1, errbuf);

	pcap_compile(descr, &fp, "host 10.250.7.14", 0, netp);

	pcap_setfilter(descr, &fp);

    user_filter("port 3306");

	pcap_loop(descr, -1, my_callback, "kkk");

	fprintf(stdout, "aoaoa");
	
	return 0;
}
