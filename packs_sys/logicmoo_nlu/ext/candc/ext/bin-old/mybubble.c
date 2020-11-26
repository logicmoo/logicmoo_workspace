/* When installed as CGI, takes stock ticker name from query string
   and displays HTML result from service.
   Call from command line using: mybubble StockProfile <ticker>
   See http://www.xmethods.com "mybubble" Company Profile service
*/
#include "soapH.h"	/* include generated proxy and SOAP support */

int main(int argc, char **argv)
{ struct soap soap;
  char *serviceName, *inputText, *html;
  if (argc < 3) /* Installed as CGI: get stock ticker from query string and display HTML result */
  { serviceName = "StockProfile";
    inputText = getenv("QUERY_STRING");
    printf("Content-type: text/html\r\n\r\n");
  }
  else
  { serviceName = argv[1];
    inputText = argv[2];
  }
  soap_init(&soap);
  if (soap_call_ns__getServiceResponsePublic(&soap, "http://www.mybubble.com:8080/soap/servlet/rpcrouter", "", serviceName, inputText, &html) == 0)
    printf("%s\n", html);
  else
    soap_print_fault(&soap, stderr);
  soap_end(&soap);
  return 0;
}

/* The namespace mapping table is required and associates namespace prefixes with namespace names: */
struct Namespace namespaces[] =
{
  {"SOAP-ENV", "http://schemas.xmlsoap.org/soap/envelope/"},	/* MUST be first */
  {"SOAP-ENC", "http://schemas.xmlsoap.org/soap/encoding/"},	/* MUST be second */
  {"xsi", "http://www.w3.org/2001/XMLSchema-instance"},		/* MUST be third */
  {"xsd", "http://www.w3.org/2001/XMLSchema"},
  {"ns",  "urn:MyBubble-SoapServices"},			/* Method namespace URI */
  {NULL, NULL}
};
