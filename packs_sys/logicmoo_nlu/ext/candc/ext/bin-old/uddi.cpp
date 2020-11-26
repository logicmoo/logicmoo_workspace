#include "soapH.h"
#include <ctype.h>

xsd__boolean UDDITest = true; // set to true when using UDDI test site

int main(int argc, char **argv)
{ struct soap *soap;
  xsd__string s;
  xsd__boolean b;
  struct ArrayOfString a;
  if (argc < 4)
  { fprintf(stderr, "Usage:\tuddi find <business> <service>\n\tuddi list <business> <service>\n\tuddi query <business> <service> <query>\n\tuddi add <business> <service> <desc> <accesspoint> <acpdesc> <login> <passwd>\n\tuddi remove <business> <service> <login> <passwd>\n\tuddi delete <business> <service> <accesspoint> <login> <passwd>\n");
    fprintf(stderr, "For more info: http://erwin.dstc.edu.au/UDDIProxyService");
    return 0;
  }
  soap = soap_new();
  switch (tolower(*argv[1]))
  { case 'f':
      if (!soap_call_ns__FindServiceAccessPoint(soap, "http://erwin.dstc.edu.au/UDDIProxyService/UDDIProxy.asmx", "http://dstc.edu.au/FindServiceAccessPoint", argv[1], argv[2], UDDITest, s))
        printf("Access point = %s\n", s);
      break;
    case 'l':
      if (!soap_call_ns__FindServiceAccessPoints(soap, "http://erwin.dstc.edu.au/UDDIProxyService/UDDIProxy.asmx", "http://dstc.edu.au/FindServiceAccessPoints", argv[1], argv[2], UDDITest, a))
        for (int i = 0; i < a.__size; i++)
	  printf("Access point = %s\n", a.__ptr[i]);
      break;
    case 'q':
      if (argc < 5)
      { fprintf(stderr, "uddi query: missing argument\n");
        return -1;
      }
      if (!soap_call_ns__FindServiceAccessPointsQuery(soap, "http://erwin.dstc.edu.au/UDDIProxyService/UDDIProxy.asmx", "http://dstc.edu.au/FindServiceAccessPointsQuery", argv[1], argv[2], argv[3], UDDITest, a))
        for (int i = 0; i < a.__size; i++)
	  printf("Access point = %s\n", a.__ptr[i]);
      break;
    case 'a':
      if (argc < 9)
      { fprintf(stderr, "uddi add: missing argument(s)\n");
        return -1;
      }
      if (!soap_call_ns__AddService(soap, "http://erwin.dstc.edu.au/UDDIProxyService/UDDIProxy.asmx", "http://dstc.edu.au/AddService", argv[1], argv[2], argv[3], argv[4], argv[5], argv[6], argv[7], UDDITest, b))
        if (b)
	  printf("Service added\n");
        else
	  printf("Service could not be added\n");
      break;
    case 'r':
      if (argc < 6)
      { fprintf(stderr, "uddi remove: missing argument(s)\n");
        return -1;
      }
      if (!soap_call_ns__RemoveService(soap, "http://erwin.dstc.edu.au/UDDIProxyService/UDDIProxy.asmx", "http://dstc.edu.au/RemoveService", argv[1], argv[2], argv[3], argv[4], UDDITest, b))
        if (b)
	  printf("Service removed\n");
        else
	  printf("Service could not be removed\n");
      break;
    case 'd':
      if (argc < 7)
      { fprintf(stderr, "uddi delete: missing argument(s)\n");
        return -1;
      }
      if (!soap_call_ns__RemoveServiceAccessPoint(soap, "http://erwin.dstc.edu.au/UDDIProxyService/UDDIProxy.asmx", "http://dstc.edu.au/RemoveAccessPointService", argv[1], argv[2], argv[3], argv[4], argv[5], UDDITest, b))
        if (b)
	  printf("Service access point deleted\n");
        else
	  printf("Service access point could not be deleted\n");
      break;
    default:
      fprintf(stderr, "uddi: unknown command '%s'\n", argv[1]);
      return -1;
  }
  if (soap->error)
  { soap_print_fault(soap, stderr);
    soap_print_fault_location(soap, stderr);
  }
  soap_end(soap);
  free(soap);
}

struct Namespace namespaces[] =
{ {"SOAP-ENV", "http://schemas.xmlsoap.org/soap/envelope/"},
  {"SOAP-ENC", "http://schemas.xmlsoap.org/soap/encoding/"},
  {"xsi", "http://www.w3.org/2001/XMLSchema-instance"},
  {"xsd", "http://www.w3.org/2001/XMLSchema"},
  {"ns", "http://dstc.edu.au"},
  {NULL, NULL}
};
