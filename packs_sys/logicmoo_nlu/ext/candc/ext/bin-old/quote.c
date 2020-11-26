#include "soapH.h"	/* include generated proxy and SOAP support */

int main(int argc, char **argv)
{ struct soap soap;
  float q;
  char *sym;
  if (argc > 1)
    sym = argv[1];
  else
  { fprintf(stderr, "Usage: quote <ticker>\n");
    return -1;
  }
  soap_init(&soap);
  if (soap_call_ns__getQuote(&soap, "http://services.xmethods.net/soap", NULL, sym, &q) == 0)
    printf("\nCompany - %s    Quote - %f\n", sym, q);
  else
    soap_print_fault(&soap, stderr);
  return 0;
}

/* The namespace mapping table is required and associates namespace prefixes with namespace names: */
struct Namespace namespaces[] =
{
  {"SOAP-ENV", "http://schemas.xmlsoap.org/soap/envelope/"},	/* MUST be first */
  {"SOAP-ENC", "http://schemas.xmlsoap.org/soap/encoding/"},	/* MUST be second */
  {"xsi", "http://www.w3.org/1999/XMLSchema-instance", "http://www.w3.org/*/XMLSchema-instance"},	/* MUST be third */
  {"xsd", "http://www.w3.org/1999/XMLSchema", "http://www.w3.org/*/XMLSchema"},
  {"ns", "urn:xmethods-delayed-quotes"},	/* Method namespace URI */
  {NULL, NULL}
};


