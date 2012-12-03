#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include <pcap.h>
#include <netinet/in.h>
#include <arpa/inet.h>


/*

struct pcap_addr {
    struct pcap_addr *next; 
    struct sockaddr *addr;      // address 
    struct sockaddr *netmask;   // netmask for that address 
    struct sockaddr *broadaddr; // broadcast address for that 
    struct sockaddr *dstaddr;   //P2P destination address for that  address 
}; 

struct pcap_if {
    struct pcap_if *next;                                     
    char *name;     // name to hand to "pcap_open_live()" 
    char *description;  // textual description of interface,  or NULL 
    struct pcap_addr *addresses;
    bpf_u_int32 flags;  // PCAP_IF_ interface flags 
};
*/

/* research pcap_if function */
int main() {

    pcap_if_t *devlist, *curr;
    pcap_addr_t *addr;
    char errbuf[PCAP_ERRBUF_SIZE];

    pcap_findalldevs(&devlist, errbuf);

    for (curr = devlist; curr; curr = curr->next) {

        //without lo
//        if (curr->flags & PCAP_IF_LOOPBACK)
 //           continue;
        printf("%s-%s-%p-%d\n", curr->name, curr->description, 
            curr->addresses, curr->flags);
        for (addr = curr->addresses; addr; addr = addr->next) {
            printf("\t%s\n",
                inet_ntoa(((struct sockaddr_in *) (addr->addr))->sin_addr));
        }
    }

    return 0;
}
