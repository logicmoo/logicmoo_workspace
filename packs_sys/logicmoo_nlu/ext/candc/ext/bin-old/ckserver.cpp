#include "soapH.h"
#include "ck.nsmap"

////////////////////////////////////////////////////////////////////////////////
//
//	Example cookie server
//
////////////////////////////////////////////////////////////////////////////////

// Install as CGI application (ck.cgi).
// Alternatively, run from command line to start a stand-alone server
// $ ck.cgi <port> &
// where <port> is a port number
// Please see the ckclient.cpp file for cookie-related details.

int main(int argc, char **argv)
{ int m, s;
  struct soap soap;
  soap_init(&soap);
  //soap.cookie_domain = "www.cs.fsu.edu"; // must be the current host name (CGI/standalone)
  soap.cookie_domain = "linprog2.cs.fsu.edu"; // stand-alone machine
  soap.cookie_path = "/"; // the path which is used to filter/set cookies with this destination
  if (argc < 2)
  { soap_getenv_cookies(&soap); // CGI app: grab cookies from 'HTTP_COOKIE' env var
    soap_serve(&soap);
  }
  else
  { m = soap_bind(&soap, NULL, atoi(argv[1]), 100);
    if (m < 0)
      exit(-1);
    fprintf(stderr, "Socket connection successful %d\n", m);
    for (int i = 1; ; i++)
    { // soap.keep_alive = 1;	// try to keep connection open (see below)
      s = soap_accept(&soap);
      if (s < 0)
        exit(-1);
      fprintf(stderr, "%d: accepted %d IP=%d.%d.%d.%d ... ", i, s, (int)(soap.ip>>24)&0xFF, (int)(soap.ip>>16)&0xFF, (int)(soap.ip>>8)&0xFF, (int)soap.ip&0xFF);
      soap_serve(&soap); // process RPC skeletons, delete this line and use these 2 lines:
      // while (soap_serve(&soap) == SOAP_OK && soap.keep_alive)	// to enable keep-alive support
        // soap_free_cookies(&soap);
      fprintf(stderr, "served\n");
      soap_end(&soap);		// clean up 
      soap_free_cookies(&soap);	// remove all old cookies from database so no interference when new requests with new cookies arrive
      // Note: threads can have their own cookie DB which they need to cleanup before they terminate
    }
  }
  return 0;
}

////////////////////////////////////////////////////////////////////////////////
//
//	Demo cookie
//
////////////////////////////////////////////////////////////////////////////////

int ck__demo(struct soap *soap, char **r)
{ int n;
  char *s, buf[16];
  // The host and path are set by soap_cookie_domain and soap_cookie_path
  // which MUST be the current domain and path of the CGI app in order
  // to accept cookies intended for this service
  s = soap_cookie_value(soap, "demo", NULL, NULL); // cookie returned by client?
  if (s)
    n = atoi(s)+1; // yes: increment int value as demo example session
  else
    n = 1; // no: return cookie with value 1 to client to start session
  sprintf(buf, "%d", n);
  soap_set_cookie(soap, "demo", buf, NULL, NULL);
  soap_set_cookie_expire(soap, "demo", 5, NULL, NULL); // cookie expires in 5 seconds
  if ((*r = (char*)soap_malloc(soap, strlen(buf)+1)))
    strcpy(*r, buf);
  return SOAP_OK;
}

