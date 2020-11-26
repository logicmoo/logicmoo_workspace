CC=gcc
CPP=g++
LIBS=
COFLAGS=-O2
CWFLAGS=-Wall
CIFLAGS=
CMFLAGS=
CFLAGS= $(CWFLAGS) $(COFLAGS) $(CIFLAGS) $(CMFLAGS)
all:		quote quote2 quote3 quotex dimeclt dimesrv who-is mybubble localtime listing glossary uddi polytest.cgi magic magicserver.cgi luclient luserver.cgi mtmagicserver.cgi ck.cgi ckclient googleapi
stdsoap2.o:	stdsoap2.h stdsoap2.cpp
		$(CPP) -c $(CFLAGS) stdsoap2.cpp
quote:		quote.h quote.c stdsoap2.c
		./soapcpp2 -c quote.h
		$(CC) $(CFLAGS) -o quote quote.c stdsoap2.c soapC.c soapClient.c $(LIBS)
quote2:		quote2.h quote2.c stdsoap2.c
		./soapcpp2 -c quote2.h
		$(CC) $(CFLAGS) -o quote2 quote2.c stdsoap2.c soapC.c soapClient.c $(LIBS)
quote3:		quote3.h quote3.cpp stdsoap2.o
		./soapcpp2 quote3.h
		$(CPP) $(CFLAGS) -o quote3 quote3.cpp stdsoap2.o soapC.cpp soapClient.cpp $(LIBS)
quotex:		quotex.h quotex.cpp stdsoap2.o
		./soapcpp2 quotex.h
		$(CPP) $(CFLAGS) -o quotex quotex.cpp stdsoap2.o soapC.cpp soapClient.cpp soapServer.cpp $(LIBS)
dimeclt:	dime.h dimeclt.cpp stdsoap2.o
		./soapcpp2 dime.h
		$(CPP) $(CFLAGS) -o dimeclt dimeclt.cpp stdsoap2.o soapC.cpp soapClient.cpp $(LIBS)
dimesrv:	dime.h dimesrv.cpp stdsoap2.o
		./soapcpp2 dime.h
		$(CPP) $(CFLAGS) -o dimesrv dimesrv.cpp stdsoap2.o soapC.cpp soapServer.cpp -lpthread $(LIBS)
who-is:		whois.h whois.cpp stdsoap2.o
		./soapcpp2 whois.h
		$(CPP) $(CFLAGS) -o who-is whois.cpp stdsoap2.o soapC.cpp soapClient.cpp $(LIBS)
mybubble:	mybubble.h mybubble.c stdsoap2.c
		./soapcpp2 -c mybubble.h
		$(CC) $(CFLAGS) -o mybubble mybubble.c stdsoap2.c soapC.c soapClient.c $(LIBS)
localtime:	localtime.h localtime.c stdsoap2.c
		./soapcpp2 -c localtime.h
		$(CC) $(CFLAGS) -o localtime localtime.c stdsoap2.c soapC.c soapClient.c $(LIBS)
listing:	listing.h listing.cpp stdsoap2.o
		./soapcpp2 listing.h
		$(CPP) $(CFLAGS) -o listing listing.cpp stdsoap2.o soapC.cpp soapClient.cpp $(LIBS)
glossary:	glossary.h glossary.cpp stdsoap2.o
		./soapcpp2 glossary.h
		$(CPP) $(CFLAGS) -o glossary glossary.cpp stdsoap2.o soapC.cpp soapClient.cpp $(LIBS)
uddi:		uddi.h uddi.cpp stdsoap2.o
		./soapcpp2 uddi.h
		$(CPP) $(CFLAGS) -o uddi uddi.cpp stdsoap2.o soapC.cpp soapClient.cpp $(LIBS)
polytest.cgi:	polymorph.h polymorph.cpp stdsoap2.o
		./soapcpp2 polymorph.h
		$(CPP) $(CFLAGS) -o polytest.cgi polymorph.cpp stdsoap2.o soapC.cpp soapClient.cpp soapServer.cpp $(LIBS)
magic:		magic.h magic.cpp stdsoap2.o
		./soapcpp2 magic.h
		$(CPP) $(CFLAGS) -o magic magic.cpp stdsoap2.o soapC.cpp soapClient.cpp $(LIBS)
magicserver.cgi:magic.h magicserver.cpp stdsoap2.o
		./soapcpp2 magic.h
		$(CPP) $(CFLAGS) -o magicserver.cgi magicserver.cpp stdsoap2.o soapC.cpp soapServer.cpp $(LIBS)
mtmagicserver.cgi:magic.h mtmagicserver.cpp stdsoap2.o
		./soapcpp2 magic.h
		$(CPP) $(CFLAGS) -o mtmagicserver.cgi mtmagicserver.cpp stdsoap2.o soapC.cpp soapServer.cpp -lpthread $(LIBS)
luclient:	lu.h luclient.cpp stdsoap2.o
		./soapcpp2 lu.h
		$(CPP) $(CFLAGS) -o luclient luclient.cpp lumat.cpp stdsoap2.o soapC.cpp soapClient.cpp $(LIBS)
luserver.cgi:	lu.h luserver.cpp stdsoap2.o
		./soapcpp2 lu.h
		$(CPP) $(CFLAGS) -o luserver.cgi luserver.cpp lumat.cpp stdsoap2.o soapC.cpp soapServer.cpp $(LIBS)
ck.cgi:		ck.h ckserver.cpp stdsoap2.cpp
		./soapcpp2 ck.h
		$(CPP) $(CFLAGS) -DWITH_COOKIES -o ck.cgi ckserver.cpp stdsoap2.cpp soapC.cpp soapServer.cpp $(LIBS)
ckclient:	ck.h ckclient.cpp stdsoap2.cpp
		./soapcpp2 ck.h
		$(CPP) $(CFLAGS) -DWITH_COOKIES -o ckclient ckclient.cpp stdsoap2.cpp soapC.cpp soapClient.cpp $(LIBS)
googleapi:	googleapi.h googleapi.c stdsoap2.cpp
		./soapcpp2 -c googleapi.h
		$(CPP) $(CFLAGS) -o googleapi googleapi.c stdsoap2.c soapC.c soapClient.c $(LIBS)
clean:		
		rm -f *.o soapH.h soapStub.h soapC.cpp soapC.c soapClient.cpp soapClient.c soapServer.cpp soapServer.c
distclean:		
		rm -f *.o *.wsdl. *.xsd *.nsmap soapH.h soapStub.h soapC.cpp soapC.c soapClient.cpp soapClient.c soapServer.cpp soapServer.c
		rm -f quote quote2 quote3 quotex dimeclt dimesrv who-is mybubble localtime listing glossary uddi polytest.cgi magic magicserver.cgi luclient luserver.cgi mtmagicserver.cgi googleapi
