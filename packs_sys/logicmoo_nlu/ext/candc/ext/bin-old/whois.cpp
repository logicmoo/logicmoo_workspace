#include "soapH.h"
#include <iostream.h>

class WhoIsReport: public getWhoIsResponse
{ public:
    void print()
    { cout << "Registrar: " << ns1__return->Registrar << endl
           << "Server: " << ns1__return->WhoIsServer << endl
           << "Referral URL: " << ns1__return->ReferralURL << endl
           << "NameServer 1: " << ns1__return->NameServer1 << endl
           << "NameServer 2: " << ns1__return->NameServer2 << endl
           << "Update Date: " << ns1__return->UpdateDate << endl
           << "Last Update Date: " << ns1__return->LastUpdateDate << endl;
    };
};

int main(int argc, char** argv)
{ struct soap *soap = soap_new();
  struct WhoIsReport result;
  if (argc <= 1 )
  { fprintf(stderr, "Usage: who-is <domain-name>\n");
    return -1;
  }
  if (soap_call_getWhoIs(soap, "http://webservices.matlus.com/scripts/whoiswebservice.dll/soap/IWhoIs", "urn:WhoIsIntf-IWhoIs#GetWhoIs", argv[1], &result) == SOAP_OK)
    result.print();
  else
  { soap_print_fault(soap, stderr);
    soap_print_fault_location(soap, stderr);
  }
  soap_end(soap);
  free(soap);
  return 0;
}

struct Namespace namespaces[] =
{
  { "SOAP-ENV", "http://schemas.xmlsoap.org/soap/envelope/" },
  { "SOAP-ENC", "http://schemas.xmlsoap.org/soap/encoding/"},
  { "xsi", "http://www.w3.org/1999/XMLSchema-instance" },
  { "xsd", "http://www.w3.org/1999/XMLSchema" },
  { "ns1", "urn:WhoIsIntf-IWhoIs"},
  { "ns2", "http://www.w3.org/2001/XMLSchema"},
  { NULL, NULL }
};
