#include "soapH.h"

////////////////////////////////////////////////////////////////////////////////
//
//	Magic Squares Client
//
////////////////////////////////////////////////////////////////////////////////

// To access a stand-alone server on a port: magicserver[] = "IP:PORT";
// use "http://" to include HTTP header: magicserver[] = "http://IP:PORT";
// const char magicserver[] = "linprog2.cs.fsu.edu:18081";
// const char magicserver[] = "http://diablo.cs.fsu.edu:18081";
// const char magicserver[] = "http://";
 const char magicserver[] = "http://websrv.cs.fsu.edu/~engelen/magicserver.cgi";

int main(int argc, char **argv)
{ struct soap soap;
  int r;
  soap_init(&soap);
  matrix *A = soap_new_matrix(&soap, -1);
  if (argc <= 1)
  { char *s = getenv("QUERY_STRING");
    if (!s || (r = atoi(s)) == 0)
      r = rand()%20;
  }
  else
    r = atoi(argv[1]);
  printf("Content-type: text/html\r\n\r\n<html><h1>Magic Square of Rank %d</h1><pre>\n", r);
  if (soap_call_ns1__magic(&soap, magicserver, NULL, r, A))
  { soap_print_fault(&soap, stderr);
    soap_print_fault_location(&soap, stderr);
  }
  else
  { for (int i = 0; i < (*A).__size; i++)
    { for (int j = 0; j < (*A)[i].__size; j++)
        printf("%4d", (*A)[i][j]);
      printf("\n");
    }
  }
  printf("</pre></html>\n");
  soap_destroy(&soap);
  soap_end(&soap);
  return 0;
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

vector::vector(int size)
{ __ptr = (int*)soap_malloc(soap, size*sizeof(int));
  __size = size;
}

vector::~vector()
{ soap_unlink(soap, this); // not required, but just to make sure if someone calls delete on this
}

void vector::resize(int size)
{ int *p;
  if (__size == size)
    return;
  p = (int*)soap_malloc(soap, size*sizeof(int));
  if (__ptr)
  { for (int i = 0; i < (size <= __size ? size : __size); i++)
      p[i] = __ptr[i];
    soap_unlink(soap, __ptr);
    free(__ptr);
  }
  __ptr = p;
  __size = size;
}

int& vector::operator[](int idx)
{ if (!__ptr || idx < 0 || idx >= __size)
    fprintf(stderr, "Array index out of bounds\n");
  return __ptr[idx];
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

matrix::~matrix()
{ soap_unlink(soap, this); // not required, but just to make sure if someone calls delete on this
}

matrix::matrix(int rows, int cols)
{ 
  __ptr = soap_new_vector(soap, rows);
  for (int i = 0; i < cols; i++)
    __ptr[i].resize(cols);
  __size = rows;
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
    { 
      __ptr = soap_new_vector(soap, rows);
      for (i = 0; i < rows; i++)
        __ptr[i].resize(cols);
      __size = rows;
    }
  }
  else
    for (i = 0; i < __size; i++)
      __ptr[i].resize(cols);
}

vector& matrix::operator[](int idx)
{ if (!__ptr || idx < 0 || idx >= __size)
    fprintf(stderr, "Array index out of bounds\n");
  return __ptr[idx];
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
  { "xsd", "http://www.w3.org/1999/XMLSchema", "http://www.w3.org/*/XMLSchema" },
  { "ns1", "urn:MagicSquare" },
  { NULL, NULL }
};
