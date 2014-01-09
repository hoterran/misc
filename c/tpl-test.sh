cc  -c -I../src -g -pedantic -Wall -O0 ../src/tpl.c
rm tpl_test
cc -I../src -g -pedantic -Wall -O0 -o tpl_test tpl_test.c tpl.o

./tpl_test
