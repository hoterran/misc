#include <stdio.h>
#include <unistd.h>
#include <assert.h>
#include <stdlib.h>

//#define zmalloc malloc
//#define zfree   free

#include "dict.h"

typedef struct Key_t 
{
    int k;
} Key_t;

typedef struct Val_t
{
    char *v;   
} Val_t;

unsigned int testHashFunction(const void *key) 
{
    Key_t *k1 = (Key_t*)key;
    return k1->k;
};

int testHashKeyCompare(void *privdata, const void *key1, const void *key2)
{
    Key_t *k1 = (Key_t*)key1;
    Key_t *k2 = (Key_t*)key2;
    return (k1->k == k2->k);
};

void testHashKeyDestructor(void *privdata, void *key)
{
    free(key);
};

void testHashValDestructor(void *privdata, void *val)
{
    Val_t *v1 = (Val_t*) val;
    free(v1->v);
    v1->v = NULL;
    free(v1);
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
    k->k = 1;
    Val_t *v = (Val_t*)malloc(sizeof(*v)); 
    v->v = malloc(100);
    snprintf(v->v, 100, "%s", "abcdefg");
    
    ret = dictAdd(d, k, v);
    assert(ret == DICT_OK);

    Val_t *v2 = dictFetchValue(d, k);

    assert(0 == strcmp(v2->v, v->v));

    printf("%d-%s-%s\n", ret, v->v, v2->v);

    //ret = dictDelete(d, k);

    //v2 = dictFetchValue(d, k);

    //assert(v2 == NULL);

    dictRelease(d);

    return 0;
}

