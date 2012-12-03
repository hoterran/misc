#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

int main()
{

	struct in_addr d;

	inet_aton("127.0.0.1", &d);

}

