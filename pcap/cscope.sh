find . -name "*.h" -o -name "*.c" -o -name "*.py"> cscope.files
cscope -bkq -i cscope.files
ctags -R 
ctags -I __THROW --langmap=c:+.h --languages=c --c-kinds=+p --if0=yes --exclude=java --file-scope=yes -f systemtags /usr/include/* /usr/include/sys/*
