/*	getQuote example with asynchronous SOAP messaging
*/
#include "soapH.h"

int main()
{ struct soap soap;
  struct ns__getQuoteResponse r;
  soap_init(&soap);
  if (soap_send_ns__getQuote(&soap, "http://services.xmethods.net/soap", NULL, "AOL"))
    soap_print_fault(&soap, stderr);
  else if (soap_recv_ns__getQuoteResponse(&soap, &r))
    soap_print_fault(&soap, stderr);
  else
    printf("AOL: %f\n", r.result);
  soap_end(&soap);
  return 0;
}

struct Namespace namespaces[] =
{
  {"SOAP-ENV", "http://schemas.xmlsoap.org/soap/envelope/"},	/* MUST be first */
  {"SOAP-ENC", "http://schemas.xmlsoap.org/soap/encoding/"},	/* MUST be second */
  {"xsi", "http://www.w3.org/1999/XMLSchema-instance"},		/* MUST be third */
  {"xsd", "http://www.w3.org/1999/XMLSchema"},
  {"ns", "urn:xmethods-delayed-quotes"},	/* Method namespace URI */
  {NULL, NULL}
};


