gSOAP WSDL Importer tool 0.9
============================

INSTALLATION
============

1. Install an XML parser for Java, e.g. Apache Xerces 2.0.1
   (xercesImpl.jar and xmlParserAPIs.jar) or 1.4.4 (xerces.jar),
   and change the classpath accordingly. The wsdlcpp.java source
   of the WSDL import tool is pre-configured for the Xerces Dom
   parser. To use another Java DOM parser, please change the
   Dom parser class import in wsdlcpp.java.

2. javac wsdlcpp.java

USAGE
=====

The creation of proxies (stubs) from WSDL is a two step process.

1. Execute: java wsdlcpp <file>.wsdl

   This generates the following files:
   
   <file>.h	the header file declarations for the gSOAP compiler
   <file>.c	a client program template

   The header file is to be processed by the gSOAP compiler:

2. Execute: soapcpp2 <file>.h

   This generates the following files:

   soapStub.h		similar to <file>.h, but with data type annotations
   soapH.h		header file of soapC.cpp
   soapC.cpp		SOAP/XML (de)serializers for C/C++ data types
   soapClient.cpp	proxy stub routines for remote method calls
   soapServer.cpp	skeleton routines for service implementation
			(not required for client applications)
   soap<srv>.wsdl	a WSDL file, where <srv> is the name of the service
   soap<srv>.nsmap	a namespace mapping table for the client application

   Modify the client program template <file>.c to suit your needs.

   Compile the client program as explained in the gSOAP documentation, e.g.
   g++ <file>.c stdoap2.cpp soapC.cpp soapClient.cpp

LIMITATIONS
===========

The following limitations are specific to the WSDL importer tool. The
limitations are not general limitations of the gSOAP toolkit and the gSOAP
stub and skeleton compiler. Future releases of the WSDL import tool will
address these limitations.

1. No <import> (WSDL must be self-contained)
2. No support for SOAP Header and Fault messages
   If Header processing is required, this will need to be added by hand to
   the generated header file.
3. To ensure compatibility to C, the current WSDL importer generates struct
   declarations. These can be changed into class declarations in the
   generated header file when necessary.

