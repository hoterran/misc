#include <pcap.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netinet/if_ether.h>

void my_callback(u_char* useless, const struct pcap_pkthdr* pkthdr,
const u_char* packet) {
	static int count = 1;
	fprintf(stdout, "%d-%s-%s-%d-%d-%ld\n", count, packet, useless, pkthdr->caplen, pkthdr->len, pkthdr->ts.tv_sec);
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
	printf("%s", dev);
	fflush(stdout);
	pcap_lookupnet(dev, &netp, &maskp, errbuf);

	descr = pcap_open_live("any", 200, 0, -1, errbuf);

	pcap_compile(descr, &fp, argv[1], 0, netp);

	pcap_setfilter(descr, &fp);

	pcap_loop(descr, -1, my_callback, "kkk");

	fprintf(stdout, "aoaoa");
	
	return 0;
}
