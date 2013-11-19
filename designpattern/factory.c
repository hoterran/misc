#include <stdio.h>
#include <assert.h>

typedef struct _Shoe  
{  
    int type;  
    void (*print_shoe)(struct _Shoe*);  
} Shoe;  

void print_leather_shoe(struct _Shoe* pShoe)  
{  
    assert(NULL != pShoe);  
    printf("This is a leather show!\n");  
}  
  
void print_rubber_shoe(struct _Shoe* pShoe)  
{  
    assert(NULL != pShoe);  
    printf("This is a rubber shoe!\n");  
}

#define LEATHER_TYPE 0X01
#define RUBBER_TYPE 0X02

Shoe* factory(int type) {
    assert(LEATHER_TYPE == type || RUBBER_TYPE == type);

    Shoe *s = malloc(sizeof(*s));
    s->type = type;
    
    if (LEATHER_TYPE == type) {
        s->print_shoe = print_leather_shoe;
    } else if (RUBBER_TYPE == type) {
        s->print_shoe = print_rubber_shoe;
    }
    return s;
}

int main() {
    Shoe *s1 = factory(LEATHER_TYPE);
    Shoe *s2 = factory(RUBBER_TYPE);
    s1->print_shoe(s1);
    s2->print_shoe(s2);

}
