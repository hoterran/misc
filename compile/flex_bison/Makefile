all: fb1 a fb2 fb3 fb4 fb5 fb2-1

fb1:
	flex fb1-1.l
	gcc -g lex.yy.c -o fb1-1 -lfl
a:
	flex a.l
	gcc lex.yy.c -o a -lfl

fb2:
	flex fb1-2.l
	gcc lex.yy.c -o fb1-2 -lfl

fb3:
	flex fb1-3.l
	gcc lex.yy.c -o fb1-3 -lfl

fb4:
	flex fb1-4.l
	gcc lex.yy.c -o fb1-4 -lfl

fb5: fb1-5.y fb1-5.l
	bison -d fb1-5.y
	flex fb1-5.l
	gcc -o fb1-5 fb1-5.tab.c lex.yy.c 

fb2-1: fb2-1.l lex.yy.c
	flex fb2-1.l
	gcc -g lex.yy.c -o fb2-1 

fb2-2: fb2-2.l lex.yy.c
	flex fb2-2.l
	gcc -g lex.yy.c -o fb2-2

fb2-5: fb2-5.l lex.yy.c
	flex fb2-5.l
	gcc -g lex.yy.c -o fb2-5
