/* This is both a client and CGI-based server to demonstrate
   object exchange (derived types + overriding)
   Install as CGI
   From the command line:
   polytest.cgi o
   polytest.cgi s
   polytest.cgi q
   polytest.cgi l
*/

#include "soapH.h"
#include <iostream.h>

const char *server = "http://www.cs.fsu.edu/~engelen/polytest2.cgi";

int main(int argc, char **argv)
{ struct soap soap;
  ns__polytestResponse r;
  soap_init(&soap);
  if (argc <= 1)
    soap_serve(&soap); // if no args, act as a CGI Web service
  else
  { switch (*argv[1])
    { case 'o':
      { ns__Object o("SOAP");
        cout << "Sending: ";
	o.print();
        soap_call_ns__polytest(&soap, server, "", &o, r);
        break;
      }
      case 's':
      { ns__Shape s("Triangle", 3);
        cout << "Sending: ";
	s.print();
        soap_call_ns__polytest(&soap, server, "", &s, r);
        break;
      }
      case 'q':
      { ns__Shape q("Cubicle", 2);
        cout << "Sending: ";
	q.print();
        soap_call_ns__polytest(&soap, server, "", &q, r);
        break;
      }
      case 'l':
      { ns__List l(4);
	l[0] = new ns__Object("SOAP");
	l[1] = new ns__Shape("Triangle", 3);
	l[2] = new ns__Square("Cubicle", 2);
	ns__List l2(1);
        l[3] = &l2;
	l2[0] = new ns__Object("End");
        cout << "Sending: ";
	l.print();
        soap_call_ns__polytest(&soap, server, "", &l, r);
        break;
      }
      default:
        fprintf(stderr, "Unknown command\nPlease use 'o', 's', 'q', or 'l'\n");
	return -1;
    }
    if (soap.error)
      soap_print_fault(&soap, stderr);
    else
    { cout << "Received: ";
      r.out->print();
    }
  }
  return 0;
}

ns__Object::ns__Object()
{ name = "Object";
}

ns__Object::ns__Object(const char *name)
{ this->name = (char*)name;
}

ns__Object::~ns__Object()
{
}

void ns__Object::print()
{ cout << "Object: " << name << endl;
}

ns__Shape::ns__Shape()
{ name = "Shape";
  sides = 0;
}

ns__Shape::ns__Shape(const char *name, int sides)
{ this->name = (char*)name;
  this->sides = sides;
}

ns__Shape::~ns__Shape()
{
}

void ns__Shape::print()
{ cout << "Shape: " << name << " sides=" << sides << endl;
}

ns__Square::ns__Square()
{ name = "Square";
  ns__Shape::sides = 4;
}

ns__Square::ns__Square(const char *name, int size)
{ this->name = (char*)name;
  this->size = size;
  ns__Shape::sides = 4;
}

ns__Square::~ns__Square()
{
}

void ns__Square::print()
{ cout << "Square: " << name << " size=" << size << endl;
}

ns__List::ns__List()
{ __ptr = 0;
  __size = 0;
}

ns__List::ns__List(int size)
{ __ptr = (ns__Object**)malloc(size*sizeof(ns__Object*));
  __size = size;
}

ns__List::~ns__List()
{
}

ns__Object*& ns__List::operator[](int i)
{ return __ptr[i];
}

void ns__List::print()
{ cout << "List: [" << endl;
  for (int i = 0; i < __size; i++)
    __ptr[i]->print();
  cout << "]" << endl;
}

// Web service remote method implementation:

int ns__polytest(struct soap *soap, ns__Object *in, struct ns__polytestResponse &result)
{ result.out = in;
  return SOAP_OK;
}

struct Namespace namespaces[] =
{
  {"SOAP-ENV", "http://schemas.xmlsoap.org/soap/envelope/"},	// MUST be first
  {"SOAP-ENC", "http://schemas.xmlsoap.org/soap/encoding/"},	// MUST be second
  {"xsi", "http://www.w3.org/2000/XMLSchema-instance"},		// MUST be third
  {"xsd", "http://www.w3.org/2000/XMLSchema"},
  {"ns", "urn:copy"},
  {NULL, NULL}
};

