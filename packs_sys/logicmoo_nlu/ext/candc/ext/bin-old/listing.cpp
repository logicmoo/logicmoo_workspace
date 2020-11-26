#include "soapH.h"
#include <iostream.h>

int
fignore(struct soap *soap, const char *tag)
{ return SOAP_OK; // accept any unknown XML element tag
  // return SOAP_MUSTUNDERSTAND; // to throw an exception in case of an unknown XML element tag
}

int main(int argc, char** argv)
{ struct soap *soap = soap_new();
  struct ArrayOfSOAPService result;
  printf("Content-type: text/html\r\n\r\n<html><h1>Most Recent Xmethods Service Listing</h1><pre>\n");
  // soap->fignore = fignore; // use this callback to test the 'ignoring' of unknown XML elements
  if (soap_call_ns__getAllSOAPServices(soap, "http://www.xmethods.net:80/soap/servlet/rpcrouter", "urn:xmethodsServicesManager#getAllSOAPServices", NULL, result) == 0)
    for (int i = 0; i < result.__size; i++)
      printf("#%d: <A HREF=\"http://www.xmethods.com/detail.html?id=%d\">%s</A> Homepage: <A HREF=\"%s\">%s</A>\n", (result.__ptr)[i].ID, (result.__ptr)[i].ID, (result.__ptr[i].name), (result.__ptr)[i].homepageURL, (result.__ptr)[i].homepageURL);
  else
  { soap_print_fault(soap, stderr);
    soap_print_fault_location(soap, stderr);
  }
  printf("</pre></html>\n");
  soap_end(soap);
  free(soap);
  return 0;
}

struct Namespace namespaces[] =
{
  { "SOAP-ENV", "http://schemas.xmlsoap.org/soap/envelope/" },	// must be first
  { "SOAP-ENC","http://schemas.xmlsoap.org/soap/encoding/" },	// must be second
  { "xsi", "http://www.w3.org/1999/XMLSchema-instance" },
  { "xsd", "http://www.w3.org/1999/XMLSchema" },
  { "ns", "urn:xmethodsServicesManager" },
  { "ns3", "http://www.xmethods.net/XMethodsListingsService.xsd" },
  { NULL, NULL }
};


