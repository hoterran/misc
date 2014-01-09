#include <time.h>
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>

int main(int argc ,char *argv[])
{
	time_t t = atoi(argv[1]);

	printf("%s",ctime(&t));
	
	return 1;
}
