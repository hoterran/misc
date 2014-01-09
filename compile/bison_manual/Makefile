
all:rpcalc calc

rpcalc:
	bison rpcalc.y
	bison rpcalc2.y
	gcc -g -lm -o rpcalc rpcalc.tab.c
	gcc -g -lm -o rpcalc2 rpcalc2.tab.c

calc:
	bison calc.y
	bison calc_no_prec.y
	gcc -g -lm -o calc calc.tab.c
	gcc -g -lm -o calc_no_prec calc_no_prec.tab.c

clean:
	@rm *.tab.c
	@rm rpcalc calc rpcalc2 calc_no_prec
