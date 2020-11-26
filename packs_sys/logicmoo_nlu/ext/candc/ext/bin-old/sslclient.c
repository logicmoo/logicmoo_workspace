#include "soapH.h"
#include "ssl.nsmap"

int main()
{ struct soap soap;
  int in, out;
  double a, b, result;
  a = 10.0;
  b = 20.0;
  soap_init(&soap);
  soap.keyfile = "client.pem"; /* may be omitted when server does not require client authentication */
  soap.password = "password"; /* may be omitted when server does not require client authentication */
  soap.cafile = "cacert.pem"; /* may be omitted when server does not require client authentication */
  if (soap_call_ns__add(&soap, "https://linprog1.cs.fsu.edu:18081", "", a, b, &result)== SOAP_OK)
    fprintf(stdout,"Result = %f\n", result);
  else
    soap_print_fault(&soap, stderr);
  return 0;
}
