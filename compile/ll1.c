#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>

enum Symbols {
    /* the symbols */

    /* 1. Terminal symbols: */
    TS_L_PARENS,    // (
    TS_R_PARENS,    // )
    TS_A,           // a
    TS_PLUS,        // +
    TS_EOS,         // $, in this case corresponds to '\0'
    TS_INVALID,     // invalid token

    /* 2. Non-terminal symbols: */
    NTS_S,          // S
    NTS_F           // F
};

enum Symbols lexer(char c) {       
    switch(c)
    {       
        case '(':  return TS_L_PARENS;
        case ')':  return TS_R_PARENS;
        case 'a':  return TS_A;
        case '+':  return TS_PLUS;
        case '\0': return TS_EOS; // end of stack: the $ terminal symbol
        default:   return TS_INVALID;
    }
}

typedef struct _Stack{
    int *data;
    int pos;
    int size;
} Stack;

#define SIZE 100

Stack *stackInit(int size) {
    Stack* s = malloc(sizeof(*s));
    s->data = malloc(size);
    s->pos = 0;
    s->size = size;
    return s;
}

#define stackTop(s) (s->data[s->pos - 1])

int stackPop(Stack *s) {
    if (s->pos > 0) {
        s->data[s->pos--] = 0;
        return 0;
    }
    return 1;
}

int stackPush(Stack* s, int e) {
    if (s->pos < s->size) {
        s->data[s->pos++] = e;
        return 0;
    }
    return 1;
}

/*

=============
1. S → F
2. S → ( S + F )
3. F → a

============
    (   )   a   +   $
S   2   -   1   -   -
F   -   -   3   -   -

*/
int main (int argc, char **argv) {
    if (argc < 2) {
        printf("need input stream");
        return 1;
    }

    int table[8][8] = {};
    table[NTS_S][TS_L_PARENS] = 2;
    table[NTS_S][TS_A] = 1;
    table[NTS_F][TS_A] = 3;
     
    Stack *s = stackInit(SIZE);
    stackPush(s, TS_EOS);
    stackPush(s, NTS_S);

    char *input = &argv[1][0];
   
    while(s->pos) {
        if (lexer(*input) == stackTop(s)) {
            printf("Matched %d\n", lexer(*input));
            input++;
            stackPop(s);
        } else {
            printf("Rule %d\n", table[stackTop(s)][lexer(*input)]); 
            switch(table[stackTop(s)][lexer(*input)]) {
                case 1:
                    stackPop(s);
                    stackPush(s, NTS_F);
                    break;
                case 2:
                    stackPop(s);
                    stackPush(s, TS_R_PARENS);
                    stackPush(s, NTS_F);
                    stackPush(s, TS_PLUS);
                    stackPush(s, NTS_S);
                    stackPush(s, TS_L_PARENS);
                    break;
                case 3:
                    stackPop(s);
                    stackPush(s, TS_A);
                    break;
                default:
                    printf("failure\n");
                    return 0;
                    break;
            }
        }
    }
    printf("finish\n");
}
