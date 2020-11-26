#include "soapH.h"
#include <iostream.h>

const char luserver[] = "http://websrv.cs.fsu.edu/~engelen/luserver.cgi";

int main(int argc, char **argv)
{ struct soap *soap = soap_new();
  const char *endpoint;
  matrix a(soap, 3); // matrix with 3 rows created in current soap env.
  // set up matrix by specifying non-zero elements only (this is optional)
  a[1].resize(1,2); // 2-element vector indexed from 1 to 2
  a[1][1] = 2;
  a[1][2] = 1;
  a[2].resize(1,3); // 3-element vector
  a[2][1] = 1;
  a[2][2] = 2;
  a[2][3] = 1;
  a[3].resize(2,3); // 2-element vector indexed from 2 to 3
  a[3][2] = 1;
  a[3][3] = 2;
  cout << "* Demonstration example *" << endl;
  cout << "Matrix:" << endl;
  a.print();
  vector b(soap, 3);
  b[1] = 1;
  b[2] = 2;
  b[3] = 3;
  cout << "Vector:" << endl;
  b.print();
  vector x(soap);
  if (argc < 2)
    endpoint = luserver;
  else
    endpoint = argv[1];
  /* solve ax=b */
  if (soap_call_ns1__lusol(soap, endpoint, "", &a, &b, &x))
  { soap_print_fault(soap, stderr);
    soap_print_fault_location(soap, stderr);
  }
  else
  { cout << "Solution vector from service:" << endl;
    x.print();
  }
  matrix a1(soap);
  if (soap_call_ns1__luinv(soap, endpoint, "", &a, &a1))
  { soap_print_fault(soap, stderr);
    soap_print_fault_location(soap, stderr);
  }
  else
  { cout << "Inverse matrix matrix from service:" << endl;
    a1.print();
  }
  soap_destroy(soap);
  soap_end(soap);
  free(soap);
  return 0;
}

struct Namespace namespaces[] =
{// "ns-prefix", "ns-name", "ns-pattern"
  {"SOAP-ENV", "http://schemas.xmlsoap.org/soap/envelope/"},
  {"SOAP-ENC", "http://schemas.xmlsoap.org/soap/encoding/"},
  {"xsi", "http://www.w3.org/2001/XMLSchema-instance", "http://www.w3.org/*/XMLSchema-instance"},
  {"xsd", "http://www.w3.org/2001/XMLSchema", "http://www.w3.org/*/XMLSchema"},
  {"ns1", "urn:lu"},
  {NULL, NULL}
};
