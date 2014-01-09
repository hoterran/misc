#
#		Makefile for Top-Down Unger Parser in C
#

N =		4
GRAMMAR =	gram$(N)

RESULT =	res$(N)E
RESULT =	res$(N)P

#			set UNIX/DOS dependency here
# GENERAL
CC =		gcc
CFLAGS =	-ansi -Wstrict-prototypes -Wmissing-prototypes \
			-Wmissing-declarations -Wredundant-decls

# DOS version
EXE =		.exe
LEX =		flex
LINT =		echo No lint under MSDOS
TIME =		time
SLEEP =		rem
SYSTEM =	MSDOS
SUBSYSTEM =	MSDOS

# UNIX version
EXE =		#
LEX =		lex
LINT =		lint -ansi -xh
TIME =		time
SLEEP =		sleep
SYSTEM =	UNIX
SUBSYSTEM =	UNIX
#			end of UNIX/DOS dependency

test:		TD_parser$(EXE)
		./TD_parser$(EXE)

compare:	TD_parser$(EXE) Makefile $(RESULT)
		./TD_parser | tee newres | cmp - $(RESULT)

SRC =		TD_parser.c $(GRAMMAR).c
OBJ =		TD_parser.o $(GRAMMAR).o

TD_parser$(EXE):$(OBJ)
		$(CC) $(OBJ) -o TD_parser$(EXE)

lint:		$(SRC)
		lint -ansi -xh $(SRC)

tex:		TD_parserE.c.tex TD_diff.c.tex

.PHONY:		clean fresh
clean:
		rm -f *.o \
			TD_parser TD_parser$(EXE) TD_diff.c \
			TD_parser.c.tex TD_diff.c.tex \
			a.out newres core

fresh:		clean

TD_parserE.c.tex: TD_parserE.c ../prog2tex
TD_diff.c.tex: TD_diff.c ../prog2tex
