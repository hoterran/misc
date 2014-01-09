# ======== Makefile for producing original sentences from a CF grammar ========
#			set UNIX/DOS dependency here
# GENERAL
CC =		gcc -g
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

GRAM =		gram4
GRAM =		gram1

test:		los$(EXE)
		./los$(EXE)

SRC =		los.c $(GRAM).c
OBJ =		los.o $(GRAM).o

los$(EXE):	$(OBJ) Makefile
		$(CC) $(OBJ) -o $@

lint:		$(SRC)
		$(LINT) $(SRC)

.PHONY:		clean fresh
clean:
		rm -f a.out los$(EXE) core *.o los

fresh:		clean

los.o: global.h st_char.h

