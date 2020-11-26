#include <iostream>
#include "soapH.h"
#include "quote.nsmap"

class Stock
{ private:
    struct soap *soap;
    char *symbol;
  public:
    Stock()
    { soap = soap_new();
      symbol = NULL;
    };
    Stock(char *s)
    { soap = soap_new();
      symbol = (char*)soap_malloc(soap, strlen(s)+1);
      strcpy(symbol, s);
    }
    ~Stock()
    { soap_end(soap);
      soap_done(soap);
      free(soap);
    };
    float quote()
    { float quote;
      if (soap_call_ns__getQuote(soap, "http://services.xmethods.net/soap", "", symbol, quote))
        /* handle exception */;
      return quote;
    }
};

int main()
{ Stock AOL("AOL");
  cout << AOL.quote() << endl;
}
