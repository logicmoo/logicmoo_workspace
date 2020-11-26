#include "soapH.h"
#include "ssl.nsmap"
#include <pthread.h>
#include <signal.h>

void *process_request(void *);

void sigpipe_handle(int);

main()
{ int m, s; /* master and slave sockets */
  int r;
  pthread_t tid;
  struct soap soap, *tsoap;
  signal(SIGPIPE, sigpipe_handle);
  soap_init(&soap);
  soap.keyfile = "server.pem";
  soap.password = "password";
  soap.cafile = "cacert.pem";
  soap.dhfile = "dh512.pem";
  m = soap_bind(&soap, NULL, 18081, 100);
  if (m < 0)
  { soap_print_fault(&soap, stderr);
    exit(-1);
  }
  fprintf(stderr, "Socket connection successful: master socket = %d\n", m);
  for ( ; ; )
  { s = soap_accept(&soap);
    fprintf(stderr, "Socket connection successful: slave socket = %d\n", s);
    if (s < 0)
    { soap_print_fault(&soap, stderr);
      exit(-1);
    } 
    if (soap_ssl_accept(&soap))
    { soap_print_fault(&soap, stderr);
      exit(-1);
    }
    tsoap = soap_new();
    if (!tsoap)
      exit(-1);
    tsoap->socket = soap.socket;	/* set by soap_accept */
    tsoap->ssl = soap.ssl;		/* set by soap_ssl_accept */
    tsoap->bio = soap.bio;		/* set by soap_ssl_accept */
    pthread_create(&tid, NULL, &process_request, (void*)tsoap);
  }
} 

void *process_request(void *soap)
{ pthread_detach(pthread_self());
  soap_serve((struct soap*)soap);
  soap_end((struct soap*)soap);
  free(soap);
  return NULL;
}

void sigpipe_handle(int x) { }

int ns__add(struct soap *soap, double a, double b, double *result)
{ *result = a + b;
  return SOAP_OK;
} 
