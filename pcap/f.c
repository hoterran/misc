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

	if (pkthdr == NULL) {
		count++;
		return;
	}

	fprintf(stdout, "%d-%s-%s-%d-%d\n", count, packet, useless, pkthdr->caplen, pkthdr->len);
	fflush(stdout);
	count++;
}

/*
 * argv1 is filter
 */
int main(int argc, char** argv)
{
	int i;
	char dev[64] = "lo";
	char errbuf[PCAP_ERRBUF_SIZE];
	pcap_t *descr;
	const u_char *packet;
	struct pcap_pkthdr hdr;
	struct ether_header *epthr;
	struct bpf_program fp;
	bpf_u_int32	netmask;
	bpf_u_int32	localnet;

	pcap_lookupnet(dev, &localnet, &netmask, errbuf);

	descr = pcap_open_live(dev, 65535, 0, -1, errbuf);

	pcap_compile(descr, &fp, argv[1], 0, netmask);

	pcap_setfilter(descr, &fp);

	pcap_dispatch(descr, -1, my_callback, "kkk");

	fprintf(stdout, "aoaoa");
	
	return 0;
}
