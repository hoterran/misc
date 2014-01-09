#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "tpl.h"

#include <unistd.h>
#include <sys/stat.h> 
#include <sys/types.h>  
#include <stdio.h>
#include <stdlib.h> 
#include <dirent.h> 

void
time_sub(struct timeval *out, struct timeval *in)                                                                                                                      
{
    if ( (out->tv_usec -= in->tv_usec) < 0)
    {   
	--out->tv_sec;
	out->tv_usec += 1000000;
    }   
    out->tv_sec -= in->tv_sec;
}

int main() {
    tpl_node *tn;
    int i,j=-1;
    void *addr;
    size_t sz;

    tn = tpl_map("i",&i);
    i=1;
    tpl_pack(tn,0);
    tpl_dump(tn,TPL_MEM,&addr,&sz);
    tpl_free(tn);

    tn = tpl_map("i",&j);
    tpl_load(tn,TPL_MEM,addr,sz);
    tpl_unpack(tn,0);
    printf("j is %d\n", j);
    tpl_free(tn);
    free(addr);

    /* file */
    
    tn = tpl_map("i", &i);
    i=100;
    tpl_pack(tn, 0);
    tpl_dump(tn, TPL_FILE, "file.mytest");
    tpl_free(tn);

    tn = tpl_map("i", &j);
    tpl_load(tn, TPL_FILE, "file.mytest");
    tpl_unpack(tn, 0);
    printf("j is %d\n", j);
    tpl_free(tn);

    /* array file */
    int z[] = {1, 2, 3 ,4};
    int x = 0;
    int y = z[x];
    tn = tpl_map("A(i)", &y);
    for(x = 0; x < sizeof(z) / sizeof(int);x++,y=z[x]) {
       tpl_pack(tn, 1);
    }
    tpl_dump(tn, TPL_FILE, "file.mytest.array");
    tpl_free(tn);

    tn = tpl_map("A(i)", &y);
    tpl_load(tn, TPL_FILE, "file.mytest.array");
    while(tpl_unpack(tn, 1) > 0) {
	printf("%d - ", y);
    }
    printf("\n");
    tpl_free(tn);

    /* big file */
    struct timeval start_val, end_val; 

    gettimeofday(&start_val, NULL);
    gettimeofday(&end_val, NULL);
    time_sub(&end_val, &start_val);
    printf(" %ld \n", end_val.tv_sec * 1000000 + end_val.tv_usec);

    x = 0;
    tn = tpl_map("A(i)", &x);
    gettimeofday(&start_val, NULL);
    for(x = 0; x < 10 ;x++) {
       tpl_pack(tn, 1);
    }
    gettimeofday(&end_val, NULL);
    time_sub(&end_val, &start_val);
    printf(" tpl pack %ld \n", end_val.tv_sec * 1000000 + end_val.tv_usec);

    gettimeofday(&start_val, NULL);
    tpl_dump(tn, TPL_FILE, "/home/file.mytest.bigarray");
    gettimeofday(&end_val, NULL);
    time_sub(&end_val, &start_val);
    printf(" tpl dump %ld \n", end_val.tv_sec * 1000000 + end_val.tv_usec);

    tpl_free(tn);

    tn = tpl_map("A(i)", &x);
    gettimeofday(&start_val, NULL);
    tpl_load(tn, TPL_FILE, "/home/file.mytest.bigarray");
    gettimeofday(&end_val, NULL);
    time_sub(&end_val, &start_val);
    printf(" tpl load %ld \n", end_val.tv_sec * 1000000 + end_val.tv_usec);

    printf("len %d \n", tpl_Alen(tn, 1));
    char *s1 = tpl_peek(TPL_FILE, "/home/file.mytest.bigarray");
    printf(" string %s \n", s1);
    free(s1);s1=NULL;
    
    gettimeofday(&start_val, NULL);
    while(tpl_unpack(tn, 1) > 0) {
	//printf("%d - ", x);
    }
    gettimeofday(&end_val, NULL);
    time_sub(&end_val, &start_val);
    printf(" tpl unpack %ld \n", end_val.tv_sec * 1000000 + end_val.tv_usec);
    
    printf("\n");
    tpl_free(tn);

    /* struct array */ 

    struct T {
	char* name;
	int id;	
    };
   
    struct T m[100], n; 
    n.name = malloc(20);
    n.id = 200;

    tn = tpl_map("A(S(si))", &n);
    for(x = 0; x < sizeof(m) / sizeof(struct T);x++) {
       n.id = n.id + x;
       snprintf(n.name, 20, "xxx-%d", n.id);
       tpl_pack(tn, 1);
    }
    tpl_dump(tn, TPL_FILE, "file.mytest.struct.array");
    tpl_free(tn);
    free(n.name);
    n.name = NULL;

    tn = tpl_map("A(S(si))", &n);
    tpl_load(tn, TPL_FILE, "file.mytest.struct.array");
    while(tpl_unpack(tn, 1) > 0) {
	printf("%d - %s\n", n.id, n.name);
        free(n.name);
	n.name = NULL;
    }
    printf("\n");
    tpl_free(tn);

    /* struct nest and fix array */

    typedef struct _Item {
        long seq;
        time_t startTime;
        long latency;
        long num;
        char srcIp[16];
        char db[32];
        char user[32];
        //char value[10240]; //too big we need adjust
        char *value; 
        char sql[200]; 
        char rawSql[200];
	
        unsigned int hash;
        int size; 
	
    } Item;

    typedef struct _TplItem {
        unsigned int cid;
        Item item;
    } TplItem;

    TplItem it;
    memset(&it, 0, sizeof(it));
    it.cid = 100;
    snprintf(it.item.user, sizeof(it.item.user), "%s" , "root");
    it.item.value = malloc(20);
    snprintf(it.item.value, 20, "%s", "kkksdfasdfadfasfdadfasdfadfasdfsafasfasdfasfda");

    tn = tpl_map("A(S(u$(IIIIc#c#c#sc#c#ui)))", &it
	    ,
	    sizeof(it.item.srcIp),
    	    sizeof(it.item.db),
    	    sizeof(it.item.user),
    	    sizeof(it.item.value),
    	    sizeof(it.item.sql),
    	    sizeof(it.item.rawSql)
    	    );

    for(x = 0; x < 100;x++) {
	it.item.num = x;
	it.item.seq = x + 100;
	snprintf(it.item.db, sizeof(it.item.db), "yyy-%d", x);
	snprintf(it.item.srcIp, sizeof(it.item.srcIp), "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx-%d", x);
	snprintf(it.item.sql, sizeof(it.item.sql), "select * from order asdfasf asdf sad fas df as dfa sdfsadfsafd a fas f sa fas fasdfasfas");
	tpl_pack(tn, 1);
    }
    tpl_dump(tn, TPL_FILE, "file.mytest.nest.struct.array");
    tpl_free(tn);
    free(it.item.value);
    it.item.value=NULL;

    TplItem it2;
    tn = tpl_map("A(S(u$(IIIIc#c#c#sc#c#ui)))", &it2
	    , 
	    sizeof(it.item.srcIp),
    	    sizeof(it.item.db),
    	    sizeof(it.item.user),
    	    sizeof(it.item.value),
    	    sizeof(it.item.sql),
    	    sizeof(it.item.rawSql)
    	    );

    tpl_load(tn, TPL_FILE, "file.mytest.nest.struct.array");
    while(tpl_unpack(tn, 1) > 0) {
	printf("%u - %ld - %s - %s - %s -%s - %s \n", it2.cid, it2.item.num, it2.item.user, it2.item.srcIp, it2.item.sql, it2.item.db, it2.item.value);
	free(it2.item.value);
	it2.item.value = NULL;
//	printf("%u - yyy -  %ld - %ld \n", it.cid, it.item.num, it.item.seq);
    }
    printf("\n");
    free(it2.item.value);
    it2.item.value=NULL;
    tpl_free(tn);

    /* dump is will truncate  */

    tn = tpl_map("A(i)", &i);
    i=100;
    tpl_pack(tn, 1);
    printf("pack1 size - %d\n", tpl_Alen(tn, 1));
    tpl_dump(tn, TPL_FILE, "file.mytest.append.array");
    printf("pack dump size - %d\n", tpl_Alen(tn, 1));
    tpl_free(tn);

    tn = tpl_map("A(i)", &i);
    i=101;
    tpl_pack(tn, 1);
    printf("pack1 size - %d\n", tpl_Alen(tn, 1));
    tpl_pack(tn, 1);
    printf("pack2 size - %d\n", tpl_Alen(tn, 1));
    tpl_dump(tn, TPL_FILE, "file.mytest.append.array");
    printf("pack dump size - %d\n", tpl_Alen(tn, 1));
    tpl_free(tn);

    tn = tpl_map("i", &j);

    tpl_free(tn);
    tn = tpl_map("A(i)", &j);
    tpl_load(tn, TPL_FILE, "file.mytest.append.array");
    while(tpl_unpack(tn, 1) > 0) {
        printf("array j is %d\n", j);
    }
    tpl_free(tn);

    /* file rotate */
    i=0;
    j=0;
    tn = tpl_map("A(i)", &j);
    size_t len;
    int tag=0;
    char filename[100];
    char *s;
    while(1) {
	// j is data , i is counter
	i++;
	j++;
        tpl_pack(tn, 1);
	len = tpl_Alen(tn, 1);
	if (len >= 100000) {
	    snprintf(filename, sizeof(filename), "file-rotate-%d-name", ++tag);
	    tpl_dump(tn, TPL_FILE, filename);
	    tpl_free(tn);
	    s = tpl_peek(TPL_FILE, filename);
	    tn = tpl_map(s, &i);
	    free(s);
	    s = NULL;
	    if (tag>=5)
		break;
	}
    }

    tpl_free(tn);

    i = 0;
    tag = 0;
    /* group file pack */
    DIR *dir;
    struct dirent *fileInfo = NULL;
    dir = opendir("."); 
    tn = tpl_map("A(i)", &j);
    while((fileInfo = readdir(dir)) != NULL) {
	if (fileInfo->d_type & DT_REG) {
	    if (strstr(fileInfo->d_name, "file-rotate")) {
		tag++;
		printf("%s\n", fileInfo->d_name);
		tpl_load(tn, TPL_FILE, fileInfo->d_name);
		while(tpl_unpack(tn, 1) > 0) {
		    i++;
		    //printf("rotate j is %d\n", j);
		}
	    }
	}
    }
    tpl_free(tn);
    closedir(dir);
    printf(" total write %d, file %d\n", i, tag);
    printf(" total read %d, file %d\n", i, tag);
    return(0);
}

