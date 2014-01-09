#include <stdio.h>
#include <unistd.h>
#include <assert.h>
#include <stdlib.h>
#include <stdint.h>

//#define zmalloc malloc
//#define zfree   free

#include "dict.h"

typedef struct Key_t 
{
    uint32_t laddr, raddr;
    uint16_t lport, rport;
} Key_t;

typedef struct Val_t
{
    char *v; 
} Val_t;

static unsigned long
hash_fun(uint32_t laddr, uint32_t raddr, uint16_t lport, uint16_t rport) 
{
    unsigned long ret;
    
    ret = laddr ^ raddr;
    ret ^= (lport << 16) | rport;
 
    return ret;
}

unsigned int testHashFunction(const void *key) 
{
    Key_t *k1 = (Key_t*)key;
    return  hash_fun(k1->laddr, k1->raddr, k1->lport, k1->rport);
};

int testHashKeyCompare(void *privdata, const void *key1, const void *key2)
{
    Key_t *k1 = (Key_t*)key1;
    Key_t *k2 = (Key_t*)key2;

    if ((k1->laddr == k2->laddr) &&
        (k1->raddr == k2->raddr) &&
        (k1->rport == k2->rport) &&
        (k1->lport == k2->lport))
        return 1;
    return 0;
};

void testHashKeyDestructor(void *privdata, void *key)
{
    free(key);
};

void testHashValDestructor(void *privdata, void *val)
{
    free(val);
};

dictType testDictType  = {
    testHashFunction,               /* hash */
    NULL,
    NULL,
    testHashKeyCompare,             /* key compare */
    testHashKeyDestructor,          /* key destructor */
    testHashValDestructor           /* value destructor */    
};

int main(int argc, char *argv[]) 
{
    int ret;
    dict *d = dictCreate(&testDictType, NULL);
    assert(d);
    Key_t *k = (Key_t*)malloc(sizeof(*k)); 
    k->laddr = 112;
    k->raddr = 112;
    k->lport = 1123;
    k->rport = 3306;

    Val_t *v = (Val_t*)malloc(sizeof(*v)); 
    v->v = malloc(100);
    snprintf(v->v, 100, "%s", "abcdefg");
    
    ret = dictAdd(d, k, v);
    assert(ret == DICT_OK);

    Val_t *v2 = dictFetchValue(d, k);

    assert(0 == strcmp(v2->v, v->v));

    printf("%d-%s-%s\n", ret, v->v, v2->v);

    dictPrintStats(d);

    dictDelete(d, k);

    dictPrintStats(d);

    dictRelease(d);

    return 0;
}

