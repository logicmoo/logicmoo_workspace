//	dimesrv.cpp
//	Example simple image server using DIME
//	Runs as CGI (not multi-threaded) or multi-threaded stand-alone
//	Web service

// For stand-alone Web service functionality, run from the command line with
// port number as command line argument, e.g.
// > dimesrv 18085 &
// Use port 80 to install and run as HTTP Web server accessible over the Web
// NOTE: THE SERVER WILL ONLY SEND FILES THAT ARE IN THE CURRENT DIR
// THE AUTHOR IS NOT RESPONSIBLE FOR ANY DAMAGES RESULTING FROM THE USE OF
// THIS PROGRAM
// Change the 'endpoint' in 'dimeclt.cpp' to
// endpoint="http://machine:18085"
// where 'machine' is the host name of the machine on which the service runs.

// The service is multi-threaded. Multi-threading is not required, but can
// improve QoS. Remove the pthread code to obtain a non-multi-threaded service.

#include "soapH.h"
#include "dime.nsmap"
#include <pthread.h>	// use Pthreads

#define BACKLOG (100)		// Max. request backlog
#define MAX_FILE_SIZE (10000)	// Max. file size

void *process_request(void*);

int main(int argc, char **argv)
{ struct soap soap;
  soap_init(&soap);
  if (argc < 2)		// no args: this is a CGI application
  { soap_set_recv_logfile(&soap, "dimesrv.log");	// compile stdsoap2.cpp with -DDEBUG: keep log of incoming requests
    soap_set_sent_logfile(&soap, NULL);
    soap_set_test_logfile(&soap, NULL);
    soap_serve(&soap);	// serve request
    soap_destroy(&soap);// cleanup class instances
    soap_end(&soap);	// cleanup
  }
  else
  { struct soap *tsoap;
    pthread_t tid;
    int port;
    int m, s, i;
    port = atoi(argv[2]);	// port is first command line argument
    m = soap_bind(&soap, NULL, port, BACKLOG);
    if (m < 0)
      exit(-1);
    fprintf(stderr, "Socket connection successful %d\n", m);
    for (i = 1; ; i++)
    { s = soap_accept(&soap);
      if (s < 0)
      { soap_print_fault(&soap, stderr);
        break;
      }
      fprintf(stderr, "Thread %d accepts socket %d connection from IP %d.%d.%d.%d\n", i, s, (int)(soap.ip>>24)&0xFF, (int)(soap.ip>>16)&0xFF, (int)(soap.ip>>8)&0xFF, (int)soap.ip&0xFF);
      tsoap = soap_new();
      tsoap->socket = s;
      pthread_create(&tid, NULL, (void*(*)(void*))process_request, (void*)tsoap);
    }
  }
  return 0;
}

void *process_request(void *soap)
{ pthread_detach(pthread_self());
  soap_serve((struct soap*)soap);	// serve pending request
  soap_destroy((struct soap*)soap);	// remove class instances
  soap_end((struct soap*)soap);		// clean up
  free(soap);
  return NULL;
}

int ns__getImage(struct soap *soap, char *name, xsd__base64Binary &image)
{ if (name)
  { FILE *fd = NULL;
    int i, c;
    if (!strchr(name, '/') && !strchr(name, '\\') && !strchr(name, ':'))
      fd = fopen(name, "r");
    if (!fd)
    { soap_fault(soap);
      soap->fault->faultstring = "Cannot open file";
      return SOAP_FAULT;
    }
    image.__ptr = (unsigned char*)soap_malloc(soap, MAX_FILE_SIZE);
    for (i = 0; i < MAX_FILE_SIZE; i++)
    { if ((c = fgetc(fd)) == EOF)
        break;
      image.__ptr[i] = c;
    }
    fclose(fd);
    image.__size = i;
    image.type = "image/jpeg";
    image.options = soap_dime_option(soap, 0, "My picture");
  }
  else
  { soap_fault(soap);
    soap->fault->faultstring = "Name required";
    return SOAP_FAULT;
  }
  return SOAP_OK;
}
