//gsoap ns1 service namespace:	urn:xmethods-delayed-quotes
int ns1__getQuote(char *symbol, float &result);

//gsoap ns2 service namespace:	urn:xmethods-CurrencyExchange
int ns2__getRate(char *country1, char *country2, float &result);

//gsoap ns3 service name:	quotex
//gsoap ns3 service location:	http://www.cs.fsu.edu/~engelen
//gsoap ns3 service executable:	quotex.cgi
//gsoap ns3 service namespace:	urn:quotex
int ns3__getQuote(char *symbol, char *country, float &result);
