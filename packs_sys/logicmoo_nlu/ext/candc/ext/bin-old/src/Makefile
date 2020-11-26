#	gSOAP 2 Makefile by Robert van Engelen, Florida State Univerity
#	Requires Lex and Yacc
#	Linkage is static by default (gcc -static). Remove -static if necessary

CC=gcc
CPP=g++
LIBS=-ly -ll
COFLAGS=-O2
CWFLAGS=
CIFLAGS=
CMFLAGS=
CFLAGS= $(CWFLAGS) $(COFLAGS) $(CIFLAGS) $(CMFLAGS)
soapcpp2:	soapcpp2.h y.tab.o symbol2.o error2.o lex.yy.o init2.o soapcpp2.o
		$(CC) -s symbol2.o error2.o y.tab.o lex.yy.o init2.o soapcpp2.o -static $(LIBS) -o $@
y.tab.h:	soapcpp2.y soapcpp2.h error2.h
		yacc -d -v soapcpp2.y
y.tab.c:	soapcpp2.y soapcpp2.h error2.h
		yacc -d -v soapcpp2.y
lex.yy.c:	y.tab.c soapcpp2.l
		lex soapcpp2.l
.c.o:		soapcpp2.h y.tab.h error2.h
		$(CC) $(CFLAGS) -c $<
clean:		
		rm -f *.o
distclean:		
		rm -f soapcpp2 *.o lex.yy.* soapcpp2.tab.* y.tab.* *.output
