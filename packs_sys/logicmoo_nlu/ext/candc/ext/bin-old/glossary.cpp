#include "soapH.h"

int main(int argc, char **argv)
{ struct soap *soap;
  struct ArrayOfString r;
  if (argc < 2)
  { fprintf(stderr, "Usage: glossary <word>\n");
    return 0;
  }
  soap = soap_new();
  if (soap_call_ns__definitions(soap, "http://www.luhala.com/cgi-bin/glossary.pl", "http://www.luhala.com/Glossary#definitions", argv[1], r))
  { soap_print_fault(soap, stderr);
    soap_print_fault_location(soap, stderr);
    soap_end(soap); // clean up array and strings;
    free(soap);
    return -1;
  }
  else
  { printf("\nDefinitions:\n");
    for (int i = 0; i < r.__size; i++)
      printf(" %d: %s\n", i+1, r.__ptr[i]);
  }
  soap_end(soap); // clean up array and strings;
  if (soap_call_ns__synonyms(soap, "http://www.luhala.com/cgi-bin/glossary.pl", "http://www.luhala.com/Glossary#synonyms", argv[1], r))
  { soap_print_fault(soap, stderr);
    soap_print_fault_location(soap, stderr);
    soap_end(soap); // clean up array and strings;
    free(soap);
    return -1;
  }
  else
  { printf("\nSynonyms:\n");
    for (int i = 0; i < r.__size; i++)
      printf(" %d: %s\n", i+1, r.__ptr[i]);
  }
  soap_end(soap); // clean up array and strings;
  free(soap);
  return 0;
}

struct Namespace namespaces[] =
{
  {"SOAP-ENV", "http://schemas.xmlsoap.org/soap/envelope/"},	// MUST be first
  {"SOAP-ENC", "http://schemas.xmlsoap.org/soap/encoding/"},	// MUST be second
  {"xsi", "http://www.w3.org/1999/XMLSchema-instance"},		// MUST be third
  {"xsd", "http://www.w3.org/1999/XMLSchema"},
  {"ns", "http://www.luhala.com/Glossary"},	// Method namespace URI
  {NULL, NULL}
};
