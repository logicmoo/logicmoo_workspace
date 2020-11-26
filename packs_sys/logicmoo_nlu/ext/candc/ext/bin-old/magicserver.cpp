#include "soapH.h"

////////////////////////////////////////////////////////////////////////////////
//
//	Magic Squares Server
//
////////////////////////////////////////////////////////////////////////////////

// Install as a CGI application.
// Alternatively, run from command line with arguments IP (which must be the
// IP of the current machine you are using) and PORT to run this as a
// stand-alone server on a port. For example:
// > magicserver.cgi machine 18081 &
// To let 'magic' talk to this service, change the URL in magic.cpp into
// "http://machine:18081"
// where "machine" is the name of your machine or e.g. "localhost"

int main(int argc, char **argv)
{ struct soap soap;
  int m, s;
  soap_init(&soap);
  // soap.accept_timeout = 60; // die if no requests are made within 1 minute
  if (argc < 3)
  { soap_serve(&soap);
    soap_destroy(&soap);
    soap_end(&soap);	// clean up 
  }
  else
  { m = soap_bind(&soap, argv[1], atoi(argv[2]), 100);
    if (m < 0)
      exit(-1);
    fprintf(stderr, "Socket connection successful %d\n", m);
    for (int i = 1; ; i++)
    { s = soap_accept(&soap);
      if (s < 0)
        exit(-1);
      fprintf(stderr, "%d: accepted %d IP=%d.%d.%d.%d ... ", i, s, (int)(soap.ip>>24)&0xFF, (int)(soap.ip>>16)&0xFF, (int)(soap.ip>>8)&0xFF, (int)soap.ip&0xFF);
      soap_serve(&soap);	// process RPC skeletons
      fprintf(stderr, "served\n");
      soap_destroy(&soap);
      soap_end(&soap);	// clean up 
    }
  }
  return 0;
}

////////////////////////////////////////////////////////////////////////////////
//
//	Magic Square Algorithm
//
////////////////////////////////////////////////////////////////////////////////

int ns1__magic(struct soap *soap, int n, matrix *square)
{ int i, j, k, l, key = 2;
  if (n < 1)
  { soap_fault(soap); /* make sure we have a place to store the fault */
    soap->fault->faultstring = "Negative or zero size";
    soap->fault->detail = "The input parameter must be positive";
    return SOAP_FAULT;
  }
  if (n > 100)
  { soap_fault(soap); /* make sure we have a place to store the fault */
    soap->fault->faultstring = "size > 100";
    soap->fault->detail = "The input parameter must not be too large";
    return SOAP_FAULT;
  }
  square->resize(n, n);
  for (i = 0; i < n; i++)
    for (j = 0; j < n; j++)
      (*square)[i][j] = 0;
  i = 0;
  j = (n-1)/2;
  (*square)[i][j] = 1;
  while (key <= n*n)
  { if (i-1 < 0)
      k = n-1;
    else
      k = i-1;
    if (j-1 < 0)
      l = n-1;
    else
      l = j-1;
    if ((*square)[k][l])
      i = (i+1) % n;
    else
    { i = k;
      j = l;
    }
    (*square)[i][j] = key;
    key++;
  }
  return SOAP_OK;
}

////////////////////////////////////////////////////////////////////////////////
//
//	Class vector Methods
//
////////////////////////////////////////////////////////////////////////////////

vector::vector()
{ __ptr = 0;
  __size = 0;
}

vector::vector(int n)
{ __ptr = (int*)soap_malloc(soap, n*sizeof(int));
  __size = n;
}

vector::~vector()
{ soap_unlink(soap, this); // not required, but just to make sure if someone calls delete on this
}

void vector::resize(int n)
{ int *p;
  if (__size == n)
    return;
  p = (int*)soap_malloc(soap, n*sizeof(int));
  if (__ptr)
  { for (int i = 0; i < (n <= __size ? n : __size); i++)
      p[i] = __ptr[i];
    soap_unlink(soap, __ptr);
    free(__ptr);
  }
  __size = n;
  __ptr = p;
}

int& vector::operator[](int i)
{ if (!__ptr || i < 0 || i >= __size)
    fprintf(stderr, "Array index out of bounds\n");
  return (__ptr)[i];
}

////////////////////////////////////////////////////////////////////////////////
//
//	Class matrix Methods
//
////////////////////////////////////////////////////////////////////////////////

matrix::matrix()
{ __ptr = 0;
  __size = 0;
}

matrix::matrix(int rows, int cols)
{ __ptr = soap_new_vector(soap, rows);
  for (int i = 0; i < cols; i++)
    __ptr[i].resize(cols);
  __size = rows;
}

matrix::~matrix()
{ soap_unlink(soap, this); // not required, but just to make sure if someone calls delete on this
}

void matrix::resize(int rows, int cols)
{ int i;
  vector *p;
  if (__size != rows)
  { if (__ptr)
    { p = soap_new_vector(soap, rows);
      for (i = 0; i < (rows <= __size ? rows : __size); i++)
      { if (this[i].__size != cols)
          (*this)[i].resize(cols);
	(p+i)->__ptr = __ptr[i].__ptr;
	(p+i)->__size = cols;
      }
      for (; i < rows; i++)
        __ptr[i].resize(cols);
    }
    else
    { __ptr = soap_new_vector(soap, rows);
      for (i = 0; i < rows; i++)
        __ptr[i].resize(cols);
      __size = rows;
    }
  }
  else
    for (i = 0; i < __size; i++)
      __ptr[i].resize(cols);
}

vector& matrix::operator[](int i)
{ if (!__ptr || i < 0 || i >= __size)
    fprintf(stderr, "Array index out of bounds\n");
  return __ptr[i];
}

////////////////////////////////////////////////////////////////////////////////
//
//	Namespace Definition Table
//
////////////////////////////////////////////////////////////////////////////////

struct Namespace namespaces[] =
{ { "SOAP-ENV", "http://schemas.xmlsoap.org/soap/envelope/" }, // must be first
  { "SOAP-ENC", "http://schemas.xmlsoap.org/soap/encoding/" }, // must be second
  { "xsi", "http://www.w3.org/1999/XMLSchema-instance", "http://www.w3.org/*/XMLSchema-instance" },
  { "xsd", "http://www.w3.org/1999/XMLSchema",          "http://www.w3.org/*/XMLSchema" },
  { "ns1", "urn:MagicSquare" },		// "ns1" namespace prefix
  { NULL, NULL }
};
