#include <pcap.h>
#include <netinet/in.h>

int main()
{
	char* dev, errbuf[PCAP_ERRBUF_SIZE];

	bpf_u_int32 net;
	bpf_u_int32 mask;

	//net card
	dev = pcap_lookupdev(errbuf);

	printf("Device %s\n", dev);
	//network and host address
	pcap_lookupnet(dev, &net, &mask, errbuf);

	struct in_addr d;
	d.s_addr = net;

	char* z = inet_ntoa(d);

	d.s_addr = mask;

	char* z2 = inet_ntoa(d);

	printf("net - %s - %s\n", z, z2);

	return 0;
}
