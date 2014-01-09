# ======== Makefile for Wood's CS parser, CACM 13(7)1970, 437-445 ========
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

GRAM =		gram1

test:		parser$(EXE)
		./parser$(EXE)

SRC =		parser.c $(GRAM).c
OBJ =		parser.o $(GRAM).o

parser$(EXE):	$(OBJ)
		$(CC) $(OBJ) -o $@

lint:		$(SRC)
		$(LINT) $(SRC)

.PHONY:		clean fresh
clean:
		rm -f *.o a.out core parser$(EXE)

fresh:		clean

parser.o: global.h
gram1.o: global.h
gram2.o: global.h
