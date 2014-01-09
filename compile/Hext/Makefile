# ======== Makefile for the Do"mo"lki parser as described by Hext ========
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

GRAM =		gram1

test:		parser$(EXE)
		./parser$(EXE)

SRC =		driver.c parser.c hardware.c print.c $(GRAM).c
OBJ =		driver.o parser.o hardware.o print.o $(GRAM).o

parser$(EXE):	$(OBJ)
		$(CC) $(OBJ) -o $@

lint:
		$(LINT) $(SRC)

.PHONY:		clean fresh
clean:
		rm -f a.out parser$(EXE) core *.o

fresh:		clean

# DO NOT DELETE

driver.o: driver.h
driver.o: hardware.h
driver.o: parser.h
driver.o: print.h
gram1.o: driver.h
gram2.o: driver.h
gram3.o: driver.h
gram4.o: driver.h
hardware.o: hardware.h
parser.o: driver.h
parser.o: hardware.h
parser.o: parser.h
parser.o: print.h
print.o: hardware.h
print.o: parser.h
print.o: print.h
