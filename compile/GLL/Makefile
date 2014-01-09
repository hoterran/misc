# ======== Makefile for a Generalized LL parser ========
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

help:
		@echo Calls are:
		@echo make test_parser
		@echo make all

all:		test_parser

GRAMMAR =	gram03
GRAMMAR =	gram11

TEST_OPTIONS =	
TEST_INPUT =	

# ==== PARSER
test_parser:	parser$(EXE) $(TEST_INPUT)
		./parser$(EXE) $(TEST_OPTIONS) $(TEST_INPUT)

PARSER_SRC =	parser.c $(GRAMMAR).c LLtables.c print.c
PARSER_OBJ =	parser.o $(GRAMMAR).o LLtables.o print.o
parser$(EXE):	$(PARSER_OBJ) Makefile
		$(CC) $(PARSER_OBJ) -o $@

# ==== Auxilairies
figures:		

lint:		$(PARSER_SRC) Makefile
		$(LINT) $(PARSER_SRC)

.PHONY:		clean fresh
clean:
		rm -f *.o core a.out *.?txt \
			parser parser$(EXE)

fresh:		clean

# DO NOT DELETE THIS LINE -- make depend depends on it.

LLtables.o: parser.h
LLtables.o: LLtables.h
demo1.o: parser.h
demo2.o: parser.h
gram00.o: parser.h
gram00a.o: parser.h
gram01.o: parser.h
gram02.o: parser.h
gram03.o: parser.h
gram04.o: parser.h
gram04a.o: parser.h
gram05.o: parser.h
gram06.o: parser.h
gram07.o: parser.h
gram08.o: parser.h
gram09.o: parser.h
gram10.o: parser.h
gram11.o: parser.h
parser.o: parser.h
parser.o: LLtables.h
parser.o: print.h
print.o: parser.h
print.o: print.h
