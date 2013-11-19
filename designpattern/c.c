#include <stdio.h>

struct _Parent;
struct _Child;

typedef void (*Func)(struct _Parent *p);

typedef struct _Parent {
    int data;
    Func f;                     // 
} Parent;

typedef struct _Child {
    Parent parent;              // 
    int childData;
} Child;

// caller
void go(Parent *p) {
    p->f(p);
}

void ParentF(Parent *p) {
    printf("%d\n", p->data);
}

void ChildF(Parent *p) {
    Child *c = (Child*)p;
    printf("%d\n", c->childData);
}

int main() {
    Parent p;
    p.data = 1;
    p.f = ParentF;

    go(&p);

    Child c;
    c.childData = 2;
    c.parent = p;

    //
    c.parent.f = ChildF;

    go((Parent*)&c);
}

