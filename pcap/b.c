#include <pcap.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netinet/if_ether.h>

/*
    test pcap_one_live to_ms parameter
*/
void my_callback(u_char* useless, const struct pcap_pkthdr* pkthdr,
    const u_char* packet) {
	static int count = 1;
	fprintf(stdout, "%d count:%d-packet:[%s]-[%s]-snaplen:%d-totallen:%d\n", 
        time(NULL), count, packet, useless, pkthdr->caplen, pkthdr->len);
	count++;
}

int main(int argc, char* argv[]) {
	char errbuf[PCAP_ERRBUF_SIZE];
	pcap_t *descr;
    struct pcap_pkthdr pkthdr;

	descr = pcap_open_live("any",100, 0, atoi(argv[1]), errbuf);

    const char *packet = NULL;
    while(1) { 
        packet = pcap_next(descr, &pkthdr); 
        my_callback("kkk", &pkthdr, packet);
    }
	return 0;
}
