OBJS=parse.o y.tab.o
LIBES=
CFLAGS= -g
#CFLAGS= -g -DDEBUG #only minor debug info provided by DEBUG flag

cogno: y.tab.o parse.o
	cc ${CFLAGS} ${OBJS} ${LIBES} -o cogno

y.tab.c: cog.y
	yacc -vd cog.y
