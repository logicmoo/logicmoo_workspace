
The gSOAP 2 compiler sources are Copyright (C) 2000-2002 Robert A. van Engelen,
Florida State University. All rights reserved.

THIS PACKAGE IS INTENDED TO SUPPORT THE MIGRATION OF gSOAP TO DIFFERENT
PLATFORMS. The code has not been cleaned. No documentation is enclosed.
Because Web service technology and protocols such as SOAP and WSDL
are changing rapidly, periodic updates will be provided. As a consequence, the
use of this code as part of a larger work cannot be guaranteed to work
with future releases of this software and will most likely fail with
future additions. For questions, please contact the author of the software.

The gSOAP 2 compiler sources are covered by MPL 1.1, while the gSOAP runtime
libraries are covered by the gSOAP public license which was derived from the
MPL 1.1 public license with additions to cover the generated source codes.

See COPYING.txt for conditions of use.

The terms and conditions of use of this software do not allow for the removal
of the copyright notice from the main program for visual display. For
integration with other software, a similar copyright notice must be produced
that is visible to users of the software.

The distribution contains the following files in the "src" directory:
README.txt	This file
COPYING.txt	Conditions of use
license.html	License
Makefile	Unix makefile
MakefileLinux	Linux Makefile
soapcpp2.h	Main header file
soapcpp2.c	Main application
soapcpp2.c.bak	Running Flex may overwrite soapcpp2.c, so a backup is provided
symbol2.c	Symbol table handling and code generation module
error2.h	Header file for error2.c
error2.c	Error handling routines
init2.c		Compiler symbol table initialization
soapcpp2.l	Flex/Lex tokens
soapcpp2.y	Yacc/Bison grammar

The "soapcpp-linux" directory contains the gSOAP distribution in source code
form with the gSOAP compiler in binary form for Linux. Replace this compiler
"soapcpp2" with your port.

Bison (or Yacc) and Flex (or Lex) are required to build the compiler.

Type 'make -f MakefileLinux' in the "src" directory to build the compiler on
Linux-based systems with Bison.
Type 'make' to build the compiler on Unix with Yacc.

