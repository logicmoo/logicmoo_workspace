typedef char *xsd__string;
struct ArrayOfString
{ xsd__string *__ptr;
  int __size;
};
// According the WSDL description, the name of the output parameter
// should be 'result'. However, the element that I get is a SOAP-ENC:Array.
// Therefore, the remote method declarations are:
ns__definitions(xsd__string word, struct ArrayOfString &SOAP_ENC__Array);
ns__synonyms(xsd__string word, struct ArrayOfString &SOAP_ENC__Array);
