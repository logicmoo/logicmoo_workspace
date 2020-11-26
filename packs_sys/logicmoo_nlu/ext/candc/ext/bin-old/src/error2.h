/*
	error2.h

The contents of this file are subject to the Mozilla Public License Version 1.1
(the "License"); you may not use this file except in compliance with the
License. You may obtain a copy of the License at
http://www.cs.fsu.edu/~engelen/gsoapcompilerlicense.html
Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
for the specific language governing rights and limitations under the License.

The Original Code is ''gSOAP compiler'' consisting of:
error2.c, error2.h, init2.c, soapcpp2.c, soapcpp2.h, soapcpp2.l, soapcpp2.y, symbol2.c.
The Initial Developer of the Original Code is Robert A. van Engelen.
Copyright (C) 2000-2002 Robert A. van Engelen. All Rights Reserved.

*/

extern char errbuf[];

#ifdef WIN32
extern soapcpp2error(char*);
#else
extern yyerror(char*);
#endif

extern lexerror(const char*);
extern synerror(const char *);
extern semerror(const char *);
extern semwarn(const char *);
extern typerror(const char*);
extern execerror(const char*);
extern progerror(const char*, const char*, int);
extern errstat();
