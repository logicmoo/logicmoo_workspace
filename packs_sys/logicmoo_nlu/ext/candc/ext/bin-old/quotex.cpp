/*	quotex.cpp
	This example is both a SOAP service and a client application.
	As a CGI program, it will serve currency-converted stock quote requests.
	As a client, it will return the currency-converted stock quote given as
	arguments to the program on the command-line. For example
	> quotex AOL uk
*/

#include "soapH.h"	// include generated proxy and SOAP support
#include "quotex.nsmap"	// include generated namespace map file

int main(int argc, char **argv)
{ struct soap *soap = soap_new();
  float q;
  if (argc <= 2)
    soap_serve(soap);	// serve requests
  else if (soap_call_ns3__getQuote(soap, "http://websrv.cs.fsu.edu/~engelen/quotex.cgi", NULL, argv[1], argv[2], q) == 0)
    printf("\nCompany %s: %f (%s)\n", argv[1], q, argv[2]);
  else
  { soap_print_fault(soap, stderr);
    soap_print_fault_location(soap, stderr);
  }
  soap_end(soap);
  free(soap);
  return 0;
}

int ns3__getQuote(struct soap *soap, char *symbol, char *country, float &result)
{ float q, r;
  if (soap_call_ns1__getQuote(soap, "http://services.xmethods.net/soap", NULL, symbol, q) == 0 &&
      soap_call_ns2__getRate(soap, "http://services.xmethods.net/soap", NULL, "us", country, r) == 0)
  { result = q*r;
    return SOAP_OK;
  }
  else
    return SOAP_FAULT;	// pass soap fault messages on to the client of this app
}

/*	Since this app is a combined client-server, it is easy to put it together with
 * 	one header file that describes all remote methods. However, as a consequence we
 *	have to implement the methods that are not ours. Since these implementations are
 * 	never called, we can make them dummies.
 */

int ns1__getQuote(struct soap *soap, char *symbol, float &result)
{ return SOAP_NO_METHOD; } // dummy: will never be called
int ns2__getRate(struct soap *soap, char *country1, char *country2, float &result)
{ return SOAP_NO_METHOD; } // dummy: will never be called

