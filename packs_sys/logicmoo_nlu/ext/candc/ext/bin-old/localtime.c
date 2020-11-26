/* This client invokes a remote method of a service implemented in .NET with SOAP doc/lit encoding */
/* Change the US zipcode in the subroutine call to obtain your local time */
#include "soapH.h"
#include "localtime.nsmap"
int main()
{ struct soap soap;
  char *t;
  soap_init(&soap);
  if (soap_call_ns__LocalTimeByZipCode(&soap, "http://www.alethea.net/webservices/LocalTime.asmx", "http://www.alethea.net/webservices/LocalTimeByZipCode", "32306", &t))
    soap_print_fault(&soap, stderr);
  else
    printf("Time = %s\n", t);
  return 0;
}
