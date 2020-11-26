/*
	symbol2.c
	Module for symbol table handling and code generation

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

#include "soapcpp2.h"
/* Use of Bison requires the inclusion of soapcpp2.tab.h instead of y.tab.h: */
#ifdef WITH_BISON
#include "soapcpp2.tab.h"
#else
#include "y.tab.h"
#endif

static	Symbol *symlist = (Symbol*) 0;	/* pointer to linked list of symbols */
static	Symbol *nslist = (Symbol*) 0;	/* pointer to linked list of namespace prefix symbols */

static	Tnode *Tptr[TYPES];

Service *services = NULL;

FILE *fout, *fhead, *fclient, *fserver, *fheader;

static int typeNO = 0;	/* unique no. assigned to all types */

/*
install - add new symbol
*/
Symbol *
install(const char *name, Token token)
{ Symbol *p;
  p = (Symbol*)emalloc(sizeof(Symbol));
  p->name = emalloc(strlen(name)+1);
  strcpy(p->name, name);
  p->token = token;
  p->next = symlist;
  symlist = p;
  return p;
}

/*
lookup - search for an identifier's name. If found, return pointer to symbol table entry. Return pointer 0 if not found.
*/
Symbol *
lookup(const char *name)
{ Symbol *p;
  for (p = symlist; p != (Symbol*) 0; p = p->next)
    if (strcmp(p->name, name) == 0)
      return p;
  return (Symbol*)0;
}

/*
gensym - generate new symbol from base name
*/
Symbol *
gensym(const char *base)
{ static int num = 1;
  int i;
  char *b, buf[512];
  strcpy(buf, base);
  b = buf+strlen(base);
  for (i = 0; i < num; i++);
    *b++ = '_';
  *b = '\0';
  num++;
  return install(buf, ID);
}

/*
mktable - make a new symbol table incorporating a pointer to a previous table
*/
Table*
mktable(Table *table)
{	Table	*p;
	p = (Table*)emalloc(sizeof(Table));
	p->sym = lookup("/**/");
	p->list = (Entry*) 0;
	if (table == (Table*) 0)
		p->level = INTERNAL;
	else	p->level = table->level+1;
	p->prev = table;
	return p;
}

/*
mkmethod - make a new method by calling mktype
*/
Tnode*
mkmethod(Tnode *ret, Table *args)
{	FNinfo *fn = (FNinfo*)emalloc(sizeof(FNinfo));
	fn->ret = ret;
	fn->args = args;
	return mktype(Tfun, fn, 0);
}

/*
freetable - free space by removing a table
*/
void
freetable(Table *table)
{	Entry	*p, *q;
	if (table == (Table*) 0)
		return;
	for (p = table->list; p != (Entry*) 0; p = q) {
		q = p->next;
		free(p);
	}
	free(table);
}

/*
unlinklast - unlink last entry added to table
*/
Entry *
unlinklast(Table *table)
{	Entry	**p, *q;
	if (table == (Table*)0)
		return (Entry*)0;
	for (p = &table->list; *p != (Entry*)0 && (*p)->next != (Entry*)0;
	     p = &(*p)->next);
	q = *p;
	*p = (Entry*)0;
	return q;
}

/*
enter - enter a symbol in a table. Error if already in the table
*/
Entry	*
enter(table, sym)
Table	*table;
Symbol	*sym;
{ Entry	*p, *q = (Entry*) 0;
  for (p = table->list; p != (Entry*) 0; q = p, p = p->next)
    if (p->sym == sym && p->info.typ->type != Tfun)
    { sprintf(errbuf, "Duplicate declaration for %s (line %d)", sym->name, p->lineno);
      semerror(errbuf);
      return p;
    }
  p = (Entry*) emalloc(sizeof(Entry));
  p->sym = sym;
  p->info.typ = (Tnode*)0;
  p->info.sto = Snone;
  p->info.hasval = False;
  p->info.minOccurs = 1;
  p->info.maxOccurs = 1;
  p->info.offset = 0;
  p->level = table->level;
  p->lineno = yylineno;
  p->next = (Entry*) 0;
  if (q == (Entry*) 0)
    table->list = p;
  else	q->next = p;
  return p;
}

/*
entry - return pointer to table entry of a symbol
*/
Entry	*entry(table, sym)
Table	*table;
Symbol	*sym;
{
  Table	*t;
  Entry	*p;
  for (t = table; t != (Table *) 0; t = t->prev)
    for (p = t->list; p != (Entry *) 0; p = p->next)
      if (p->sym == sym)
	return p;
  return (Entry *) 0;
}

Entry *
enumentry(Symbol *sym)
{ Table	*t;
  Entry	*p, *q;
  for (t = enumtable; t; t = t->prev)
    for (p = t->list; p; p = p->next)
      if (q = entry(p->info.typ->ref, sym))
	return q;
  return NULL;
}

char *c_ident(Tnode*);
char *c_storage(Storage);
char *c_init(Entry*);
char *c_type(Tnode*);
char *c_type_id(Tnode*, char*);
char *xsi_type(Tnode*);
char *the_type(Tnode*);
char *wsdl_type(Tnode*, char*);
char *xml_tag(Tnode*);
char *ns_convert(char*);
char *ns_remove(char*);
char *ns_overridden(Table*, Entry*);

int has_ns(Tnode*);
int has_ns_eq(char*, char*);
int has_offset(Tnode*);
int is_response(Tnode*);
Entry *get_response(Tnode*);
int is_primitive(Tnode*);
Entry *is_dynamic_array(Tnode*);
int is_binary(Tnode*);
int is_hexBinary(Tnode*);
int is_string(Tnode*);
int is_wstring(Tnode*);
int get_dimension(Tnode*);
char *has_soapref(Tnode*);

char *xsi_type_Tarray(Tnode*);    
char *xsi_type_Darray(Tnode*);    

void xml_def_table(Table *,Tnode *);
void xml_out_generate(Tnode *);
int no_of_var(Tnode*);
char *pointer_stuff(Tnode*);
void xml_def_pointer(Tnode*,Tnode*);
void in_defs(Table*);
void in_defs2(Table*);
void out_defs(Table*);
void in_attach(Table*);
void out_attach(Table*);
void serialize(Tnode*);
void mark(Tnode*);
void defaults(Tnode*);
void soap_put(Tnode*);
void soap_out(Tnode*);
void soap_out_Darray(Tnode *);
void soap_get(Tnode*);
void soap_in(Tnode*); 
void soap_in_Darray(Tnode *);
void soap_instan_class(Tnode *);
int get_Darraydims(Tnode *typ);

void function_input_output(Table*);
void generate_proto(Table*, Entry*);
void generate_call(Table*, Entry*);
void generate_server(Table*, Entry*);
void generate_header(Table*);
void generate_schema(Table*);
void gen_schema(FILE*,Table*,char*,char*,int,int,char*);
void gen_wsdl(FILE*,Table*,char*,char*,char*,char*,char*,char*);

/*
mktype - make a (new) type with a reference to additional information and the
width in bytes required to store objects of that type. A pointer to the
type is returned which can be compared to check if types are identical.
*/
Tnode *
mktype(Type type, void *ref, int width)
{	Tnode	*p;
	if (type != Tstruct && type != Tclass)
	for (p = Tptr[type]; p != (Tnode*) 0; p = p->next)
		if (p->ref == ref && p->sym == (Symbol*) 0 && p->width == width && p->transient == transient)
			return p;	/* type alrady exists in table */
	p = (Tnode*)emalloc(sizeof(Tnode));	/* install new type */
	p->type = type;
	p->ref = ref;
	p->id = lookup("/**/");
	p->sym = (Symbol*)0;
	p->response = (Entry*)0;
	p->width = width;
	p->generated = False;
	p->wsdl = False;
	p->next = Tptr[type];
	p->transient = transient;
	Tptr[type] = p;
	return p;
}

Tnode *
mksymtype(Tnode *typ, Symbol *sym)
{	Tnode *p;
	p = (Tnode*)emalloc(sizeof(Tnode));	/* install new type */
	p->type = typ->type;
	p->ref = typ->ref;
	p->id = typ->id;
	p->sym = sym;
	p->width = typ->width;
	p->generated = False;
	p->wsdl = False;
	p->next = Tptr[typ->type];
	p->transient = transient;
	Tptr[typ->type] = p;
	return p;
}

/*	DO NOT REMOVE OR ALTER (SEE LICENCE AGREEMENT AND COPYING.txt)	*/
void
copyrightnote(FILE *fd, char *fn)
{ fprintf(fd, "/* %s\n   Generated by the gSOAP Stub and Skeleton Compiler for C and C++ "VERSION"\n   Copyright (C) 2001-2002 Robert A. van Engelen, Florida State University.\n   All rights reserved.\n*/", fn);
}

void
compile(Table *table)
{	Entry *p;
	char *s;
	char base[1024];
	char soapStub[1024];
	char soapH[1024];
	char soapC[1024];
	char soapClient[1024];
	char soapServer[1024];
	char soapMsg[1024];
    
	DBGLOG(fprintf(stderr,"\n IN dumptable."));
	typeNO = 0;

	s = "";
	if (*dirpath && *(s = dirpath+strlen(dirpath)-1) != '/' && *s != '\\')
#ifdef WIN32
			s = "\\";
#else
			s = "/";
#endif
	strcpy(base, dirpath);
	strcat(base, s);
	strcat(base, prefix);
	if (cflag)
		s = ".c";
	else
		s = ".cpp";
	strcpy(soapStub, base);
	strcat(soapStub, "Stub.h");
	strcpy(soapH, base);
	strcat(soapH, "H.h");
	strcpy(soapC, base);
	strcat(soapC, "C");
	strcat(soapC, s);
	strcpy(soapClient, base);
	strcat(soapClient, "Client");
	strcat(soapClient, s);
	strcpy(soapServer, base);
	strcat(soapServer, "Server");
	strcat(soapServer, s);
	strcpy(soapMsg, base);
	strcat(soapMsg, "Msg");
	strcat(soapMsg, s);
	fprintf(stderr, "Saving %s\n", soapStub);
	fheader=fopen(soapStub, "w");
	if (!fheader)
		execerror("Cannot write to file");
	copyrightnote(fheader, soapStub);
	fprintf(stderr, "Saving %s\n", soapH);
	fhead=fopen(soapH,"w");
	if (!fhead)
		execerror("Cannot write to file");
	copyrightnote(fhead, soapH);
	fprintf(stderr, "Saving %s\n", soapC);
	fout=fopen(soapC,"w");
	if (!fout)
		execerror("Cannot write to file");
	copyrightnote(fout, soapC);
	fprintf(stderr, "Saving %s\n", soapClient);
        fclient=fopen(soapClient,"w");
	if (!fclient)
		execerror("Cannot write to file");
	copyrightnote(fclient, soapClient);
	fprintf(stderr, "Saving %s\n", soapServer);
        fserver=fopen(soapServer,"w");
	if (!fserver)
		execerror("Cannot write to file");
	copyrightnote(fserver, soapServer);

	fprintf(fhead,"\n#ifndef %sH_H\n#define %sH_H", prefix, prefix);
	fprintf(fhead,"\n#include \"stdsoap2.h\"");
	fprintf(fhead,"\n#include \"%sStub.h\"", prefix);
	if (cflag)
	  fprintf(fhead,"\n#ifdef __cplusplus\nextern \"C\" {\n#endif");
	fprintf(fheader,"\n#ifndef %sStub_H\n#define %sStub_H", prefix, prefix);
	if (cflag)
	  fprintf(fheader,"\n#ifdef __cplusplus\nextern \"C\" {\n#endif");
	generate_header(table);
	generate_schema(table);
	fprintf(fout,"\n#include \"%sH.h\"", prefix);
	if (cflag)
	  fprintf(fout,"\n#ifdef __cplusplus\nextern \"C\" {\n#endif");
        fprintf(fclient,"\n#include \"%sH.h\"", prefix);
	if (cflag)
	  fprintf(fclient,"\n#ifdef __cplusplus\nextern \"C\" {\n#endif");
        fprintf(fserver,"\n#include \"%sH.h\"", prefix);
	if (cflag)
	  fprintf(fserver,"\n#ifdef __cplusplus\nextern \"C\" {\n#endif");

	DBGLOG(fprintf(stderr,"\n Calling function_input_output( )."));
        function_input_output(table);
	DBGLOG(fprintf(stderr,"\n Completed function_input_output( )."));

        if (entry(classtable, lookup("SOAP_ENV__Header"))->info.typ->type == Tstruct)
	  fprintf(fout,"\n\nSOAP_FMAC1 void SOAP_FMAC2 soap_serializeheader(struct soap *soap)\n{\n\tsoap_serialize_SOAP_ENV__Header(soap, soap->header);\n}");
	else
	  fprintf(fout,"\n\nSOAP_FMAC1 void SOAP_FMAC2 soap_serializeheader(struct soap *soap)\n{\n\tif (soap->header)\n\t\tsoap->header->soap_serialize(soap);\n}");
	fprintf(fout,"\n\nSOAP_FMAC1 void SOAP_FMAC2 soap_putheader(struct soap *soap)\n{\n\tif (soap->header)\n\t{\tsoap->is_in_header = 1;\n\t\tsoap_out_SOAP_ENV__Header(soap, \"SOAP-ENV:Header\", 0, soap->header, NULL);\n\t\tsoap->is_in_header = 0;\n\t}\n}");
	fprintf(fout,"\n\nSOAP_FMAC1 int SOAP_FMAC2 soap_getheader(struct soap *soap)\n{\n\tsoap->is_in_header = 1;\n\tsoap->header = soap_get_SOAP_ENV__Header(soap, NULL, \"SOAP-ENV:Header\", NULL);\n\tsoap->is_in_header = 0;\n\treturn soap->header == NULL;\n}");
        if (entry(classtable, lookup("SOAP_ENV__Fault"))->info.typ->type == Tstruct)
	{ fprintf(fout,"\n\nSOAP_FMAC1 void SOAP_FMAC2 soap_fault(struct soap *soap)\n{\n\tif (!soap->fault)\n\t{\tsoap->fault = (struct SOAP_ENV__Fault*)soap_malloc(soap, sizeof(struct SOAP_ENV__Fault));\n\t\tsoap_default_SOAP_ENV__Fault(soap, soap->fault);\n\t}\n}");
	  fprintf(fout,"\n\nSOAP_FMAC1 void SOAP_FMAC2 soap_serializefault(struct soap *soap)\n{\n\tsoap_serialize_SOAP_ENV__Fault(soap, soap->fault);\n}");
	}
	else
	{ fprintf(fout,"\n\nSOAP_FMAC1 void SOAP_FMAC2 soap_fault(struct soap *soap)\n{\n\tif (!soap->fault)\n\t{\tsoap->fault = soap_new_SOAP_ENV__Fault(soap, -1);\n\t\tsoap->fault->soap_default(soap);\n\t}\n}");
	  fprintf(fout,"\n\nSOAP_FMAC1 void SOAP_FMAC2 soap_serializefault(struct soap *soap)\n{\n\tif (soap->fault)\n\t\tsoap->fault->soap_serialize(soap);\n}");
	}
	fprintf(fout,"\n\nSOAP_FMAC1 void SOAP_FMAC2 soap_putfault(struct soap *soap)\n{\n\tsoap_out_SOAP_ENV__Fault(soap, \"SOAP-ENV:Fault\", 0, soap->fault, NULL);\n}");
	fprintf(fout,"\n\nSOAP_FMAC1 int SOAP_FMAC2 soap_getfault(struct soap *soap)\n{\n\treturn (soap->fault = soap_get_SOAP_ENV__Fault(soap, NULL, \"SOAP-ENV:Fault\", NULL)) == NULL;\n}");
	fprintf(fout,"\n\nSOAP_FMAC1 const char ** SOAP_FMAC2 soap_faultcode(struct soap *soap)\n{\n\tsoap_fault(soap);\n\treturn (const char**)&soap->fault->faultcode;\n}");
	fprintf(fout,"\n\nSOAP_FMAC1 const char ** SOAP_FMAC2 soap_faultstring(struct soap *soap)\n{\n\tsoap_fault(soap);\n\treturn (const char**)&soap->fault->faultstring;\n}");
	fprintf(fout,"\n\nSOAP_FMAC1 const char ** SOAP_FMAC2 soap_faultdetail(struct soap *soap)\n{\n\tsoap_fault(soap);\n\treturn (const char**)&soap->fault->detail;\n}");

	fprintf(fout,"\n\nSOAP_FMAC1 int SOAP_FMAC2 soap_getindependent(struct soap *soap)\n{");
	fprintf(fout,"\n\tfor (;;)");
        fprintf(fout,"\n\t{\tif (soap_peek_element(soap) || !*soap->id)\n\t\t\tbreak;");
	fprintf(fout,"\n\t\tswitch (soap_lookup_type(soap, soap->id))\n\t\t{");
	DBGLOG(fprintf(stderr,"\n Calling in_defs( )."));
	in_defs(table);
	DBGLOG(fprintf(stderr,"\n Completed in_defs( )."));
        fprintf(fout,"\n\t\tdefault:");
	in_defs2(table);
        fprintf(fout,"\n\t\t}");
        fprintf(fout,"\n\t\tif (soap->error)\n\t\t\tbreak;");
        fprintf(fout,"\n\t}");
        fprintf(fout,"\n\tif (soap->error == SOAP_NO_TAG)");
        fprintf(fout,"\n\t\tsoap->error = SOAP_OK;");
        fprintf(fout,"\n\treturn soap->error;");
        fprintf(fout,"\n}");

	fprintf(fout,"\n\nSOAP_FMAC1 void SOAP_FMAC2 soap_putindependent(struct soap *soap)\n{\n\tint i;\n\tstruct soap_plist *pp;");
	fprintf(fout,"\n\tif (soap->enable_embedding || soap->disable_href)\n\t\treturn;");
	fprintf(fout,"\n\tfor (i = 0; i < SOAP_PTRHASH; i++)");
	fprintf(fout,"\n\t\tfor (pp = soap->pht[i]; pp; pp = pp->next)");
	fprintf(fout,"\n\t\t\tif (soap->counting ? pp->mark1 == 2 : pp->mark2 == 2)");
	fprintf(fout,"\n\t\t\t\tswitch (pp->type & 0x3FF)\n\t\t\t\t{");
        out_defs(table);
	fprintf(fout,"\n\t\t\t\t}\n\t\t\n\t\n}");

	fprintf(fout,"\n\nSOAP_FMAC1 int SOAP_FMAC2 soap_getattachments(struct soap *soap)\n{");
	fprintf(fout,"\n\tif (!soap->dime)\n\t\treturn SOAP_OK;");
	fprintf(fout,"\n\twhile (soap->dime_flags&SOAP_DIME_CF)\n\t{\tif (soap_getdimehdr(soap))\n\t\t\treturn soap->error;\n\t\tif (soap_move(soap, soap->dime_size))\n\t\t\treturn soap->error = SOAP_EOF;\n\t}");
	fprintf(fout,"\n\tif (soap_move(soap, ((soap->dime_size+3)&-4)-soap_tell(soap)))\n\t\treturn soap->error = SOAP_EOF;");
	fprintf(fout,"\n\tfor (;;)");
        fprintf(fout,"\n\t{\tif (soap_getdime(soap) || !soap->dime_id)\n\t\t\tbreak;");
	fprintf(fout,"\n\t\tswitch (soap_lookup_type(soap, soap->dime_id))\n\t\t{");
        in_attach(table);
        fprintf(fout,"\n\t\tdefault:\n\t\t\tsoap->error = SOAP_DIME_ERROR;");
        fprintf(fout,"\n\t\t}");
        fprintf(fout,"\n\t\tif (soap->error)\n\t\t\tbreak;");
        fprintf(fout,"\n\t}");
        fprintf(fout,"\n\tif (soap->error == SOAP_EOD)");
        fprintf(fout,"\n\t\tsoap->error = SOAP_OK;");
        fprintf(fout,"\n\treturn soap->error;");
        fprintf(fout,"\n}");

	fprintf(fout,"\n\nSOAP_FMAC1 void SOAP_FMAC2 soap_putattachments(struct soap *soap)\n{\n\tint i;\n\tstruct soap_plist *pp;");
	fprintf(fout,"\n\tif (!soap->dime)\n\t\treturn;");
	fprintf(fout,"\n\tsoap_send_raw(soap, \"\\0\\0\\0\", -(int)soap->count&3);");
	fprintf(fout,"\n\tfor (i = 0; i < SOAP_PTRHASH; i++)");
	fprintf(fout,"\n\t\tfor (pp = soap->pht[i]; pp; pp = pp->next)");
	fprintf(fout,"\n\t\t\tif (pp->mark2 == 3)");
	fprintf(fout,"\n\t\t\t\tswitch (pp->type & 0x3FF)\n\t\t\t\t{");
        out_attach(table);
	fprintf(fout,"\n\t\t\t\t}\n\t\t\n\t\n}");

	fprintf(fout,"\n\nSOAP_FMAC1 void * SOAP_FMAC2 soap_instantiate(struct soap *soap, int t, const char *type, const char *arrayType)\n{\n\tswitch (t)\n\t{");
	if (classtable)
	  for (p = classtable->list; p; p = p->next)
	    if (p->info.typ->type == Tclass && !is_transient(p->info.typ))
	      fprintf(fout,"\n\tcase SOAP_%s:\n\t\treturn (void*)soap_instantiate_%s(soap, -1, type, arrayType);", p->info.typ->id->name, p->info.typ->id->name);

	fprintf(fout,"\n\t}\n\treturn NULL;\n}");

	fprintf(fout,"\n\nSOAP_FMAC1 void SOAP_FMAC2 soap_delete(struct soap *soap, void *p, int t, int n)\n{\n\tswitch (t)\n\t{");
	if (classtable)
	  for (p = classtable->list; p; p = p->next)
	    if (p->info.typ->type == Tclass && !is_transient(p->info.typ))
	    { fprintf(fout,"\n\tcase SOAP_%s:\n\t\tDBGLOG(TEST, SOAP_MESSAGE(fdebug, \"\\nsoap_delete(%s, %%d)\", n));", c_type(p->info.typ), c_type(p->info.typ));
	      fprintf(fout,"\n\t\tif (n<0)\n\t\t\tdelete (%s*)p;\n\t\telse\n\t\t\tdelete[] (%s*)p;\n\t\tbreak;", c_ident(p->info.typ), c_type(p->info.typ), c_type(p->info.typ));
	    }
	fprintf(fout,"\n\t}\n}");

	DBGLOG(fprintf(stderr,"\n Calling xml_def_table( )."));
        xml_def_table(table,0);
	DBGLOG(fprintf(stderr,"\n Completed xml_def_table( )."));
	if (cflag)
	  fprintf(fout,"\n#ifdef __cplusplus\n}\n#endif");
	fprintf(fout, "\n\n/* end of %s */\n", soapC);
        fclose(fout);
	if (cflag)
	  fprintf(fhead,"\n#ifdef __cplusplus\n}\n#endif");
	fprintf(fhead, "\n#endif");
	fprintf(fhead, "\n\n/* end of %s */\n", soapH);
	fclose(fhead);
	if (cflag)
	  fprintf(fheader,"\n#ifdef __cplusplus\n}\n#endif");
	fprintf(fheader, "\n#endif");
	fprintf(fheader, "\n\n/* end of %s */\n", soapStub);
	fclose(fheader);
	if (cflag)
	  fprintf(fclient,"\n#ifdef __cplusplus\n}\n#endif");
	fprintf(fclient, "\n\n/* end of %s */\n", soapClient);
        fclose(fclient);
	if (cflag)
	  fprintf(fserver,"\n#ifdef __cplusplus\n}\n#endif");
	fprintf(fserver, "\n\n/* end of %s */\n", soapServer);
 	fclose(fserver);
}

void
gen_class(FILE *fd, Tnode *typ)
{ Table *Tptr;
  Entry *Eptr;
  char *x = xsi_type(typ);
  typ->generated = True;
  if (is_hexBinary(typ))
    fprintf(fheader, "/* hexBinary schema type: */\n");
  else if (is_binary(typ))
    fprintf(fheader, "/* Base64 schema type: */\n");
  else if (is_primclass(typ))
    fprintf(fheader, "/* Primitive %s schema type: */\n", x);
  else if (is_dynamic_array(typ))
    if (has_ns(typ) || is_untyped(typ))
      fprintf(fheader, "/* Vector %s schema type: */\n", x);
    else
      fprintf(fheader, "/* Array of %s schema type: */\n", x);
  else if (is_transient(typ) && typ->ref)
    fprintf(fheader, "/* transient: */\n");
  else if (!strcmp(typ->id->name, "SOAP_ENV__Header"))
    fprintf(fheader, "/* SOAP Header: */\n#ifndef WITH_NOHEADER\n#define WITH_NOHEADER\n");
  else if (!strcmp(typ->id->name, "SOAP_ENV__Fault"))
    fprintf(fheader, "/* SOAP Fault: */\n#ifndef WITH_NOFAULT\n#define WITH_NOFAULT\n");
  if (typ->type == Tstruct)
  { { DBGLOG(fprintf(stderr,"\nstruct %s\n", typ->id->name));
      if (typ->ref)
      { fprintf(fd, "struct %s\n{", typ->id->name );
        for (Eptr = ((Table*)typ->ref)->list; Eptr; Eptr = Eptr->next)
        { fprintf(fd, "\n\t%s", c_storage(Eptr->info.sto));
	  if (Eptr->info.typ->type == Tclass && Eptr->info.typ->generated == False || Eptr->info.typ->ref && ((Tnode*)Eptr->info.typ->ref)->type == Tclass && ((Tnode*)Eptr->info.typ->ref)->generated == False)
	    fprintf(fd, "class ");
          fprintf(fd, "%s;", c_type_id(Eptr->info.typ,Eptr->sym->name));
	  if (Eptr->info.sto & Sconst)
	    fprintf(fd, "\t/* const field cannot be deserialized */");
	  else if (is_transient(Eptr->info.typ))
	    fprintf(fd, "\t/* transient */");
	  else if (Eptr->info.sto & SmustUnderstand)
	    fprintf(fd, "\t/* mustUnderstand */");
	  if (!is_dynamic_array(typ) && !is_primclass(typ))
	  { if (!strncmp(Eptr->sym->name, "__size", 6))
	    { if (!Eptr->next || Eptr->next->info.typ->type != Tpointer)
              { sprintf(errbuf, "Field '%s' with leading __ not followed by a pointer field in struct '%s'", Eptr->sym->name, typ->id->name);
                semwarn(errbuf);
	      }
	    }
	    else if (!strncmp(Eptr->sym->name, "__", 2))
            { sprintf(errbuf, "Field '%s' with leading __ not used in array or binary type struct '%s'", Eptr->sym->name, typ->id->name);
              semwarn(errbuf);
	    }
	  }
	}
        fprintf(fd, "\n};");
      }
      else if (strcmp(typ->id->name, "soap"))
      { sprintf(errbuf, "struct '%s' is empty", typ->id->name);
        semwarn(errbuf);
      }
    }
  }
  else
  { DBGLOG(fprintf(stderr,"\nclass %s\n", typ->id->name));
    if (typ->ref)
    { fprintf(fd,"class SOAP_CMAC %s", typ->id->name );
      Tptr =  ((Table *)typ->ref);
      if(Tptr->prev)
        fprintf(fd," : public %s", Tptr->prev->sym->name);
        fprintf(fd,"\n{ public:");
      for (Eptr = ((Table*)typ->ref)->list; Eptr; Eptr = Eptr->next)
      { fprintf(fd,"\n\t%s", c_storage(Eptr->info.sto));
	if (Eptr->info.typ->type == Tclass && Eptr->info.typ->generated == False || Eptr->info.typ->ref && ((Tnode*)Eptr->info.typ->ref)->type == Tclass && ((Tnode*)Eptr->info.typ->ref)->generated == False)
	  fprintf(fd, "class ");
	fprintf(fd,"%s", c_type_id(Eptr->info.typ,Eptr->sym->name));
	if (Eptr->info.sto & Sconstobj)
	  fprintf(fd, " const;");
	if (Eptr->info.sto & Sabstract)
	  fprintf(fd, " = 0;");
	else
	  fprintf(fd, ";");
	if (Eptr->info.sto & Sconst)
	  fprintf(fd, "\t/* const field cannot be deserialized */");
	else if (is_transient(Eptr->info.typ))
	  fprintf(fd, "\t/* transient */");
	else if (Eptr->info.sto & SmustUnderstand)
	  fprintf(fd, "\t/* mustUnderstand */");
	if (!is_dynamic_array(typ) && !is_primclass(typ))
	{ if (!strncmp(Eptr->sym->name, "__size", 6))
	  { if (!Eptr->next || Eptr->next->info.typ->type != Tpointer)
            { sprintf(errbuf, "Field '%s' with leading __ not followed by a pointer field in class '%s'", Eptr->sym->name, typ->id->name);
              semwarn(errbuf);
	    }
	  }
	  else if (!strncmp(Eptr->sym->name, "__", 2))
          { sprintf(errbuf, "Field '%s' with leading __ not used in array or binary type class '%s'", Eptr->sym->name, typ->id->name);
            semwarn(errbuf);
	  }
        }
      }
      if (!is_transient(typ))
      { fprintf(fd,"\n\tvirtual void soap_default(struct soap*); ");
        fprintf(fd,"\n\tvirtual void soap_serialize(struct soap*) const;");
        fprintf(fd,"\n\tvirtual void soap_mark(struct soap*) const;");
        fprintf(fd,"\n\tvirtual void soap_put(struct soap*, const char*, const char*) const;");
        fprintf(fd,"\n\tvirtual void soap_out(struct soap*, const char*, int, const char*) const;");
        fprintf(fd,"\n\tvirtual void *soap_get(struct soap*, const char*, const char*);");
        fprintf(fd,"\n\tvirtual void *soap_in(struct soap*, const char*, const char*); ");
      }
      fprintf(fd,"\n};");
    }
    else
    { sprintf(errbuf, "class '%s' is empty", typ->id->name);
      semwarn(errbuf);
    }
  }
  if (!strcmp(typ->id->name, "SOAP_ENV__Header") || !strcmp(typ->id->name, "SOAP_ENV__Fault"))
    fprintf(fheader, "\n#endif");
}

void
generate_header(Table *t)
{ Entry *p, *q;
  fprintf(fheader, "\n/* Enumerations */");
  if (enumtable)
    for (p = enumtable->list; p; p = p->next)
    { fprintf(fheader, "\n");
      if (is_mask(p->info.typ))
        fprintf(fheader, "\n/* Bitmask: */");
      fprintf(fheader, "\nenum %s {", p->info.typ->id->name);
      if ((Table*)p->info.typ->ref)
      { q = ((Table*)p->info.typ->ref)->list;
        if (q)
        { fprintf(fheader, "%s = %lld", q->sym->name, q->info.val.i);
          for (q = q->next; q; q = q->next)
            fprintf(fheader, ", %s = %lld", q->sym->name, q->info.val.i);
        }
      }
      fprintf(fheader, "};");
    }
  fprintf(fheader, "\n\n/* Classes and Structs */");
  if (classtable)
    for (p = classtable->list; p; p = p->next)
    { fprintf(fheader, "\n\n");
      gen_class(fheader, p->info.typ);
    }
  fprintf(fheader, "\n\n/* Typedefs */");
  if (typetable)
    for (p = typetable->list; p; p = p->next)
      fprintf(fheader,"\n\n%s%s;", c_storage(p->info.sto), c_type_id(p->info.typ, p->sym->name));
  fprintf(fheader, "\n\n/* Variables */");
  if (t)
    for (p = t->list; p; p = p->next)
      if (p->info.typ->type != Tfun)
        fprintf(fheader,"\n\nextern %s%s;", c_storage(p->info.sto), c_type_id(p->info.typ, p->sym->name));
  fflush(fheader);
}

void
get_namespace_prefixes()
{ Symbol *p, *q;
  int i, n;
  char buf[256];
  if (nslist)
    return;
  for (p = symlist; p; p = p->next)
  { if (*p->name != '~')
    { n = strlen(p->name)-2;
      for (i = 1; i < n; i++)
      { if (p->name[i] == '_' && p->name[i+1] == '_' && p->name[i+2] && p->name[i+2] != '_')
        { strncpy(buf, p->name, i);
          buf[i] = '\0';
	  if (!strcmp(buf, "SOAP_ENV") || !strcmp(buf, "SOAP_ENC") || !strcmp(buf, "xsd"))
	    goto nsnext;
          for (q = nslist; q; q = q->next)
            if (!strcmp(q->name, buf))
              goto nsnext;
          q = (Symbol*)emalloc(sizeof(Symbol));
          q->name = (char*)emalloc(i+1);
	  strcpy(q->name, buf);
	  q->name[i] = '\0';
	  q->next = nslist;
	  nslist = q;
        }
      }
    }
nsnext:
    ;
  }
}

void
generate_schema(Table *t)
{ Entry *p;
  Symbol *ns, *ns1;
  char *name = NULL;
  char *URL = NULL;
  char *executable = NULL;
  char *URI = NULL;
  char *encoding = NULL;
  Service *sp, *sq;
  char buf[256];
  FILE *fd;
  get_namespace_prefixes();
  for (ns = nslist; ns; ns = ns->next)
  { for (sp = services; sp; sp = sp->next)
      if (!strcmp(sp->ns, ns->name))
	{	name = sp->name;
		URL = sp->URL;
		executable = sp->executable;
		URI = sp->URI;
		encoding = sp->encoding;
		if (name)
		  fprintf(stderr, "Using %s service name: %s\n", ns->name, name);
		if (URL)
		  fprintf(stderr, "Using %s service location: %s\n", ns->name, URL);
		if (executable)
		  fprintf(stderr, "Using %s service executable: %s\n", ns->name, executable);
		if (URI)
		  fprintf(stderr, "Using %s service namespace: %s\n", ns->name, URI);
		if (encoding)
		  fprintf(stderr, "Using %s service encoding: %s\n", ns->name, encoding);
		break;
  	}
    if (!name)
  	name = "%{Service}%";
    if (!URL)
  	URL = "%{URL}%";
    if (!executable)
    { executable = emalloc(strlen(name)+5);
      strcpy(executable, name);
      strcat(executable, ".cgi");
    }
    if (!URI)
  	URI = "%{URI}%";
    if (t)
      for (p = t->list; p; p = p->next)
      { if (p->info.typ->type == Tfun && has_ns_eq(ns->name, p->sym->name))
        { if (sp && sp->name)
	    sprintf(buf, "%s.wsdl", name);
	  else
	    sprintf(buf, "%s.wsdl", ns_convert(ns->name));
	  fprintf(stderr, "Saving %s Web Service description\n", buf);
          fd = fopen(buf, "w");
	  if (!fd)
	    execerror("Cannot write to WSDL file");
          gen_wsdl(fd, t, ns->name, name, URL, executable, URI, encoding);
          fclose(fd);
          if (sp && sp->name)
	    sprintf(buf, "%s.nsmap", name);
	  else
	    sprintf(buf, "%s.nsmap", ns_convert(ns->name));
	  fprintf(stderr, "Saving %s namespace mapping table\n", buf);
          fd = fopen(buf, "w");
	  if (!fd)
	    execerror("Cannot write to nsmap file");
          fprintf(fd, "\nstruct Namespace namespaces[] =\n{ {\"SOAP-ENV\", \"http://schemas.xmlsoap.org/soap/envelope/\"},\n  {\"SOAP-ENC\", \"http://schemas.xmlsoap.org/soap/encoding/\"},\n  {\"xsi\", \"http://www.w3.org/2001/XMLSchema-instance\", \"http://www.w3.org/*/XMLSchema-instance\"},\n  {\"xsd\", \"http://www.w3.org/2001/XMLSchema\", \"http://www.w3.org/*/XMLSchema\"},\n  {\"%s\", \"%s\"},\n", ns_convert(ns->name), URI);
          for (ns1 = nslist; ns1; ns1 = ns1->next)
            if (ns1 != ns)
	    { for (sq = services; sq; sq = sq->next)
		if (!strcmp(sq->ns, ns1->name) && sq->URI)
		  break;
	      if (sq)
		fprintf(fd, "  {\"%s\", \"%s\"},\n", ns_convert(ns1->name), sq->URI);
	      else
		fprintf(fd, "  {\"%s\", \"%s/%s.xsd\"},\n", ns_convert(ns1->name), URL, ns_convert(ns1->name)); 
	    }
          fprintf(fd, "  {NULL, NULL}\n};\n");
          fclose(fd);
	  break;
        }
      }
    sprintf(buf, "%s.xsd", ns_convert(ns->name));
    fprintf(stderr, "Saving %s.xsd XML Schema description\n", ns_convert(ns->name));
    fd = fopen(buf, "w");
    if (!fd)
      execerror("Cannot write to schema file");
    fprintf(fd, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<!--\n");
    copyrightnote(fd, ns->name);
    fprintf(fd, "-->\n");
    gen_schema(fd, t, ns->name, ns->name, 1, 0, URL);
    fclose(fd);
  }
}

char *
nsofhdr(char *part)
{ Entry *p;
  Service *sp;
  char *s;
  p = entry(classtable, lookup("SOAP_ENV__Header"));
  for (p = ((Table*)p->info.typ->ref)->list; p; p = p->next)
  { s = strstr(p->sym->name, "__");
    if (!strcmp(part, p->sym->name) || s && strcmp(part, s+2))
    { for (sp = services; sp; sp = sp->next)
        if (sp->URI && s && !strncmp(sp->ns, p->sym->name, s-p->sym->name))
	  return sp->URI;
      sprintf(errbuf, "Cannot save header part reference in WSDL: SOAP_ENV__Header \"%s\" field has no namespace", p->sym->name);
      semwarn(errbuf);
      return "";
    }
  }
  sprintf(errbuf, "Cannot save header part reference in WSDL: SOAP_ENV__Header has no \"%s\" field", part);
  semwarn(errbuf);
  return "";
}      

void
gen_wsdl(FILE *fd, Table *t, char *ns, char *name, char *URL, char *executable, char *URI, char *encoding)
{ Entry *p, *q;
  Symbol *s;
  Service *sp;
  fprintf(fd, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
  fprintf(fd, "<definitions name=\"%s\"\n xmlns=\"http://schemas.xmlsoap.org/wsdl/\"\n xmlns:SOAP-ENV=\"http://schemas.xmlsoap.org/soap/envelope/\"\n xmlns:SOAP-ENC=\"http://schemas.xmlsoap.org/soap/encoding/\"\n xmlns:SOAP=\"http://schemas.xmlsoap.org/wsdl/soap/\"\n xmlns:WSDL=\"http://schemas.xmlsoap.org/wsdl/\"\n xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\"\n", name);
  for (sp = services; sp; sp = sp->next)
    if (!strcmp(sp->ns, ns) && sp->URI)
      break;
  if (sp)
    if (sp->WSDL)
      fprintf(fd, " targetNamespace=\"%s\"\n xmlns:tns=\"%s\"", sp->WSDL, sp->WSDL);
    else
      fprintf(fd, " targetNamespace=\"%s\"\n xmlns:tns=\"%s\"", sp->URI, sp->URI);
  else
    fprintf(fd, " targetNamespace=\"%s/%s.wsdl\"\n xmlns:tns=\"%s/%s.wsdl\"", URL, name, URL, name);
  for (s = nslist; s; s = s->next)
  { for (sp = services; sp; sp = sp->next)
      if (!strcmp(sp->ns, s->name) && sp->URI)
        break;
    if (sp)
      fprintf(fd, "\n xmlns:%s=\"%s\"", ns_convert(s->name), sp->URI);
    else
      fprintf(fd, "\n xmlns:%s=\"%s/%s.xsd\"", ns_convert(s->name), URL, ns_convert(s->name));
  }
  fprintf(fd, ">\n\n");
  fprintf(fd, "<types>\n");
  for (s = nslist; s; s = s->next)
    gen_schema(fd, t, ns, s->name, !strcmp(s->name, ns), 1, URL);
  fprintf(fd, "</types>\n\n");
  fflush(fd);
  if (t)
  { for (p = t->list; p; p = p->next)
      if (p->info.typ->type == Tfun && has_ns_eq(ns, p->sym->name))
      { fprintf(fd, "<message name=\"%sRequest\">\n", ns_remove(p->sym->name));
        fflush(fd);
  	q = entry(classtable, p->sym);
  	if (q)
	  for (q = ((Table*)q->info.typ->ref)->list; q; q = q->next)
	    if (is_XML(q->info.typ))
	      fprintf(fd, " <part name=\"body\" element=\"%s\"/>\n", ns_convert(p->sym->name));
	    else if (!is_transient(q->info.typ) && q->info.typ->type != Tfun)
	      fprintf(fd, " <part name=\"%s\" type=\"%s\"/>\n", ns_convert(q->sym->name), wsdl_type(q->info.typ, ns));
        fprintf(fd, "</message>\n\n");
        fflush(fd);
	q = (Entry*)p->info.typ->ref;
	if (q && is_transient(q->info.typ))
	  ;
	else if (q && !is_response(q->info.typ))
	  if (is_XML(q->info.typ->ref))
	    fprintf(fd, "<message name=\"%sResponse\">\n <part name=\"body\" element=\"%sResponse\"/>\n</message>\n\n", ns_remove(p->sym->name), ns_convert(p->sym->name));
	  else
	    fprintf(fd, "<message name=\"%sResponse\">\n <part name=\"%s\" type=\"%s\"/>\n</message>\n\n", ns_remove(p->sym->name), ns_convert(q->sym->name), wsdl_type(q->info.typ, ns));
        else if (q && q->info.typ->wsdl == False)
	{ q->info.typ->wsdl = True;
	  fprintf(fd, "<message name=\"%s\">\n", ns_remove(((Tnode*)q->info.typ->ref)->id->name));
	  if (((Tnode*)q->info.typ->ref)->ref)
	  { for (q = ((Table*)((Tnode*)q->info.typ->ref)->ref)->list; q; q = q->next)
	      if (is_XML(q->info.typ))
	        fprintf(fd, " <part name=\"body\" element=\"%s\"/>\n", ns_convert(((Tnode*)((Entry*)p->info.typ->ref)->info.typ->ref)->id->name));
	      else if (!is_transient(q->info.typ) && q->info.typ->type != Tfun)
	        fprintf(fd, " <part name=\"%s\" type=\"%s\"/>\n", ns_convert(q->sym->name), wsdl_type(q->info.typ, ns));
	  }
          fprintf(fd, "</message>\n\n");
	}
        fflush(fd);
      }
    if (custom_header)
    { fprintf(fd, "<message name=\"%sHeader\">\n", name);
      for (q = ((Table*)entry(classtable, lookup("SOAP_ENV__Header"))->info.typ->ref)->list; q; q = q->next)
	if (!is_transient(q->info.typ) && q->info.typ->type != Tfun)
	  fprintf(fd, " <part name=\"%s\" type=\"%s\"/>\n", ns_remove(q->sym->name), wsdl_type(q->info.typ, ns));
      fprintf(fd, "</message>\n\n");
    }
    if (custom_fault)
    { fprintf(fd, "<message name=\"%sFault\">\n", name);
      for (q = ((Table*)entry(classtable, lookup("SOAP_ENV__Fault"))->info.typ->ref)->list; q; q = q->next)
	if (!is_transient(q->info.typ) && q->info.typ->type != Tfun)
	  fprintf(fd, " <part name=\"%s\" type=\"%s\"/>\n", ns_remove(q->sym->name), wsdl_type(q->info.typ, ns));
      fprintf(fd, "</message>\n\n");
    }
    fflush(fd);
    fprintf(fd, "<portType name=\"%sPortType\">\n", name);
    for (p = t->list; p; p = p->next)
      if (p->info.typ->type == Tfun && has_ns_eq(ns, p->sym->name))
      { fprintf(fd, " <operation name=\"%s\">\n  <documentation>Service definition of function %s</documentation>\n  <input message=\"tns:%sRequest\"/>\n", ns_remove(p->sym->name), p->sym->name, ns_remove(p->sym->name));
	q = (Entry*)p->info.typ->ref;
	if (q && is_transient(q->info.typ))
	  ;
	else if (q && !is_response(q->info.typ))
	  fprintf(fd, "  <output message=\"tns:%sResponse\"/>\n", ns_remove(p->sym->name));
        else if (q)
	  fprintf(fd, "  <output message=\"tns:%s\"/>\n", ns_remove(((Tnode*)q->info.typ->ref)->id->name));
        fprintf(fd, " </operation>\n");
      }
    fprintf(fd, "</portType>\n\n");
    if (encoding && !strcmp(encoding, "literal"))
      fprintf(fd, "<binding name=\"%sBinding\" type=\"tns:%sPortType\">\n <SOAP:binding style=\"document\" transport=\"http://schemas.xmlsoap.org/soap/http\"/>\n", name, name);
    else
      fprintf(fd, "<binding name=\"%sBinding\" type=\"tns:%sPortType\">\n <SOAP:binding style=\"rpc\" transport=\"http://schemas.xmlsoap.org/soap/http\"/>\n", name, name);
    fflush(fd);
    for (p = t->list; p; p = p->next)
      if (p->info.typ->type == Tfun && has_ns_eq(ns, p->sym->name))
      { fprintf(fd, " <operation name=\"%s\">\n", ns_remove(p->sym->name));
        if (encoding && !strcmp(encoding, "literal"))
          fprintf(fd, "  <SOAP:operation soapAction=\"%s#%s\" style=\"document\"/>\n", URI, ns_remove(p->sym->name));
	else
          fprintf(fd, "  <SOAP:operation soapAction=\"%s#%s\"/>\n", URI, ns_remove(p->sym->name));
  	q = entry(classtable, p->sym);
  	if (encoding && !strcmp(encoding, "literal") || q && (q = (((Table*)q->info.typ->ref)->list)) && q && is_XML(q->info.typ))
	  fprintf(fd, "  <input>\n   <SOAP:body use=\"literal\"/>\n");
        else if (encoding)
	  fprintf(fd, "  <input>\n   <SOAP:body use=\"encoded\" namespace=\"%s\" encodingStyle=\"%s\"/>\n", URI, encoding);
        else
	  fprintf(fd, "  <input>\n   <SOAP:body use=\"encoded\" namespace=\"%s\" encodingStyle=\"http://schemas.xmlsoap.org/soap/encoding/\"/>\n", URI);
	if (custom_header)
	{ Method *m = NULL;
	  int f = 0;
	  char *s;
	  if (sp && (s = strstr(p->sym->name, "__")))
	    for (m = sp->list; m; m = m->next)
	      if (!strcmp(m->name, s+2) && (m->mess&HDRIN))
	      { f = 1;
	        fprintf(fd, "   <SOAP:header use=\"encoded\" namespace=\"%s\" encodingStyle=\"http://schemas.xmlsoap.org/soap/encoding/\" message=\"tns:%sHeader\" part=\"%s\"/>\n", nsofhdr(m->part), name, ns_remove(m->part));
	      }
	  if (!f && !sp)
	    fprintf(fd, "   <SOAP:header use=\"encoded\" encodingStyle=\"http://schemas.xmlsoap.org/soap/encoding/\" message=\"tns:%sHeader\"/>\n", name);
	}
	if (custom_fault)
	  fprintf(fd, "   <SOAP:fault use=\"encoded\" encodingStyle=\"http://schemas.xmlsoap.org/soap/encoding/\" message=\"tns:%sFault\"/>\n", name);
	fprintf(fd, "  </input>\n");
	q = (Entry*)p->info.typ->ref;
	if (!q || !q->info.typ->ref)
	{ fprintf(fd, " </operation>\n");
	  continue;
	}
	if (encoding && !strcmp(encoding, "literal") || is_XML(q->info.typ->ref))
	  fprintf(fd, "  <output>\n   <SOAP:body use=\"literal\"/>\n");
	else if (encoding)
	  fprintf(fd, "  <output>\n   <SOAP:body use=\"encoded\" namespace=\"%s\" encodingStyle=\"%s\"/>\n", URI, encoding);
	else
	  fprintf(fd, "  <output>\n   <SOAP:body use=\"encoded\" namespace=\"%s\" encodingStyle=\"http://schemas.xmlsoap.org/soap/encoding/\"/>\n", URI);
	if (custom_header)
	{ Method *m = NULL;
	  int f = 0;
	  char *s;
	  if (sp && (s = strstr(p->sym->name, "__")))
	    for (m = sp->list; m; m = m->next)
	      if (!strcmp(m->name, s+2) && (m->mess&HDROUT))
	      { f = 1;
	        fprintf(fd, "   <SOAP:header use=\"encoded\" namespace=\"%s\" encodingStyle=\"http://schemas.xmlsoap.org/soap/encoding/\" message=\"tns:%sHeader\" part=\"%s\"/>\n", nsofhdr(m->part), name, ns_remove(m->part));
	      }
	  if (!f & !sp)
	    fprintf(fd, "   <SOAP:header use=\"encoded\" encodingStyle=\"http://schemas.xmlsoap.org/soap/encoding/\" message=\"tns:%sHeader\"/>\n", name);
	}
	if (custom_fault)
	  fprintf(fd, "   <SOAP:fault use=\"encoded\" encodingStyle=\"http://schemas.xmlsoap.org/soap/encoding/\" message=\"tns:%sFault\"/>\n", name);
	fprintf(fd, "  </output>\n </operation>\n");
        fflush(fd);
      }
    fprintf(fd, "</binding>\n\n");
  }
  fprintf(fd, "<service name=\"%s\">\n <documentation>gSOAP "VERSION" generated service definition</documentation>\n <port name=\"%sPort\" binding=\"tns:%sBinding\">\n  <SOAP:address location=\"%s/%s\"/>\n </port>\n</service>\n\n</definitions>\n", name, name, name, URL, executable);
}

void
gen_schema(FILE *fd, Table *t, char *ns1, char *ns, int all, int wsdl, char *URL)
{ int i, d;
  char cbuf[4];
  Entry *p, *q;
  Tnode *n;
  Symbol *s;
  Service *sp;
  fprintf(fd, " <schema\n  xmlns=\"http://www.w3.org/2001/XMLSchema\"\n  xmlns:SOAP-ENV=\"http://schemas.xmlsoap.org/soap/envelope/\"\n  xmlns:SOAP-ENC=\"http://schemas.xmlsoap.org/soap/encoding/\"\n");
  for (sp = services; sp; sp = sp->next)
    if (!strcmp(sp->ns, ns) && sp->URI)
      break;
  if (sp)
    fprintf(fd, "  targetNamespace=\"%s\"", sp->URI);
  else
    fprintf(fd, "  targetNamespace=\"%s/%s.xsd\"", URL, ns_convert(ns));
  for (s = nslist; s; s = s->next)
  { for (sp = services; sp; sp = sp->next)
      if (!strcmp(sp->ns, s->name) && sp->URI)
        break;
    if (sp)
      fprintf(fd, "\n  xmlns:%s=\"%s\"", ns_convert(s->name), sp->URI);
    else
      fprintf(fd, "\n  xmlns:%s=\"%s/%s.xsd\"", ns_convert(s->name), URL, ns_convert(s->name));
  }
  fprintf(fd, ">\n\n");
  if (typetable)
    for (p = typetable->list; p; p = p->next)
    { if (!is_transient(p->info.typ) && ((has_ns_eq(ns, p->sym->name))))
      { fprintf(fd, "  <simpleType name=\"%s\">\n   <restriction base=\"xsd:%s\">\n", ns_remove(p->sym->name), the_type(p->info.typ));
        fprintf(fd, "   </restriction>\n  </simpleType>\n\n");
      }
    }
  if (enumtable)
    for (p = enumtable->list; p; p = p->next)
    { if (!is_transient(p->info.typ) && (!has_ns(p->info.typ) && all || has_ns_eq(ns, p->sym->name)))
      { if (is_mask(p->info.typ))
        { fprintf(fd, "  <simpleType name=\"%s\">\n   <list>\n    <restriction base=\"xsd:string\">\n", wsdl_type(p->info.typ, NULL));
          if ((Table*)p->info.typ->ref)
            for (q = ((Table*)p->info.typ->ref)->list; q; q = q->next)
              fprintf(fd, "     <enumeration value=\"%s\"/>\n", ns_convert(q->sym->name));
          fprintf(fd, "    </restriction>\n   </list>\n  </simpleType>\n\n");
	}
	else
        { fprintf(fd, "  <simpleType name=\"%s\">\n   <restriction base=\"xsd:string\">\n", wsdl_type(p->info.typ, NULL));
          if ((Table*)p->info.typ->ref)
            for (q = ((Table*)p->info.typ->ref)->list; q; q = q->next)
              fprintf(fd, "    <enumeration value=\"%s\"/>\n", ns_convert(q->sym->name));
          fprintf(fd, "   </restriction>\n  </simpleType>\n\n");
        }
      }
    }
  if (classtable)
    for (p = classtable->list; p; p = p->next)
      if (p->info.typ->ref && !is_transient(p->info.typ) && !is_binary(p->info.typ) && !is_primclass(p->info.typ))
      { q = ((Table*)p->info.typ->ref)->list;
        if (entry(t, p->sym) && q && !is_XML(q->info.typ))
          ;
        else if (is_dynamic_array(p->info.typ))
          if (!has_ns(p->info.typ) && !is_untyped(p->info.typ))
          { if (all)
	      if (wsdl)
	      { d = get_Darraydims(p->info.typ)-1;
	        for (i = 0; i < d; i++)
	          cbuf[i] = ',';
	        cbuf[i] = '\0';
	        fprintf(fd, "  <complexType name=\"%s\">\n   <complexContent>\n    <restriction base=\"SOAP-ENC:Array\">\n     <element name=\"%s\" type=\"%s\" maxOccurs=\"unbounded\"/>\n     <attribute ref=\"SOAP-ENC:arrayType\" WSDL:arrayType=\"%s[%s]\"/>\n    </restriction>\n   </complexContent>\n  </complexType>\n\n", wsdl_type(p->info.typ, NULL), q->sym->name[5]?ns_convert(q->sym->name+5):"item", wsdl_type(q->info.typ, ns1), wsdl_type(q->info.typ, ns1), cbuf);
	      }
              else
	        fprintf(fd, "  <complexType name=\"%s\">\n   <complexContent>\n    <restriction base=\"SOAP-ENC:Array\">\n     <element name=\"%s\" type=\"%s\" maxOccurs=\"unbounded\"/>\n    </restriction>\n   </complexContent>\n  </complexType>\n\n", wsdl_type(p->info.typ, NULL), q->sym->name[5]?ns_convert(q->sym->name+5):"item", wsdl_type(q->info.typ, ns1));
          }
	  else
	  { fprintf(fd, "  <complexType name=\"%s\">\n   <sequence>\n    <element name=\"%s\" type=\"%s\" minOccurs=\"0\" maxOccurs=\"unbounded\" nillable=\"true\"/>\n   </sequence>\n  </complexType>\n\n", ns_remove(p->sym->name), q->sym->name[5]?ns_convert(q->sym->name+5):"item", wsdl_type(q->info.typ, ns1));
	  }
        else if (p->info.typ->type == Tstruct && (!has_ns(p->info.typ) && all || has_ns_eq(ns, p->sym->name)))
        { if (p->info.typ->ref)
          { fprintf(fd, "  <complexType name=\"%s\">\n   <all>\n", ns_remove(p->sym->name));
            for (q = ((Table*)p->info.typ->ref)->list; q; q = q->next)
	      if (is_transient(q->info.typ) || q->info.typ->type == Tfun)
	        continue;
	      else if (is_repetition(q))
              { if (((Tnode*)q->next->info.typ->ref)->type == Tpointer)
	          if (q->info.maxOccurs == 1)
		    fprintf(fd, "    <element name=\"%s\" type=\"%s\" minOccurs=\"0\" maxOccurs=\"unbounded\" nillable=\"true\"/>\n", ns_convert(q->next->sym->name), wsdl_type(q->next->info.typ->ref, ns1));
                  else
		    fprintf(fd, "    <element name=\"%s\" type=\"%s\" minOccurs=\"%d\" maxOccurs=\"%d\" nillable=\"true\"/>\n", ns_convert(q->next->sym->name), wsdl_type(q->next->info.typ->ref, ns1), q->info.minOccurs, q->info.maxOccurs);
                else
	          if (q->info.maxOccurs == 1)
	            fprintf(fd, "    <element name=\"%s\" type=\"%s\" minOccurs=\"0\" maxOccurs=\"unbounded\"/>\n", ns_convert(q->next->sym->name), wsdl_type(q->next->info.typ->ref, ns1));
		  else
	            fprintf(fd, "    <element name=\"%s\" type=\"%s\" minOccurs=\"%d\" maxOccurs=\"%d\"/>\n", ns_convert(q->next->sym->name), wsdl_type(q->next->info.typ->ref, ns1), q->info.minOccurs, q->info.maxOccurs);
	        q = q->next;
	      }
              else if (q->info.typ->type == Tpointer || q->info.typ->type == Tarray || is_dynamic_array(q->info.typ))
                fprintf(fd, "    <element name=\"%s\" type=\"%s\" minOccurs=\"%d\" maxOccurs=\"%d\" nillable=\"true\"/>\n", ns_convert(q->sym->name), wsdl_type(q->info.typ, ns1), q->info.minOccurs, q->info.maxOccurs);
	      else
                fprintf(fd, "    <element name=\"%s\" type=\"%s\" minOccurs=\"%d\" maxOccurs=\"%d\"/>\n", ns_convert(q->sym->name), wsdl_type(q->info.typ, ns1), q->info.minOccurs, q->info.maxOccurs);
            fprintf(fd, "   </all>\n  </complexType>\n\n");
          }
        }
        else if (p->info.typ->type == Tclass && (!has_ns(p->info.typ) && all || has_ns_eq(ns, p->sym->name)))
        { if (p->info.typ->ref)
            if (((Table*)p->info.typ->ref)->prev && !is_transient(entry(classtable, ((Table*)p->info.typ->ref)->prev->sym)->info.typ))
            { fprintf(fd, "  <complexType name=\"%s\">\n   <complexContent>\n    <extension base=\"%s\">\n     <sequence>\n", ns_remove(p->sym->name), ns_convert(((Table*)p->info.typ->ref)->prev->sym->name));
              for (q = ((Table*)p->info.typ->ref)->list; q; q = q->next)
	        if (is_transient(q->info.typ) || q->info.typ->type == Tfun)
	          continue;
	        else if (is_repetition(q))
                { if (((Tnode*)q->next->info.typ->ref)->type == Tpointer)
	            if (q->info.maxOccurs == 1)
                      fprintf(fd, "     <element name=\"%s\" type=\"%s\" minOccurs=\"0\" maxOccurs=\"unbounded\" nillable=\"true\"/>\n", ns_overridden(p->info.typ->ref, q->next), wsdl_type(q->next->info.typ->ref, ns1));
		    else
                      fprintf(fd, "     <element name=\"%s\" type=\"%s\" minOccurs=\"%d\" maxOccurs=\"%d\" nillable=\"true\"/>\n", ns_overridden(p->info.typ->ref, q->next), wsdl_type(q->next->info.typ->ref, ns1), q->info.minOccurs, q->info.maxOccurs);
		  else
	            if (q->info.maxOccurs == 1)
                      fprintf(fd, "     <element name=\"%s\" type=\"%s\" minOccurs=\"0\" maxOccurs=\"unbounded\"/>\n", ns_overridden(p->info.typ->ref, q->next), wsdl_type(q->next->info.typ->ref, ns1));
		    else
                      fprintf(fd, "     <element name=\"%s\" type=\"%s\" minOccurs=\"%d\" maxOccurs=\"%d\"/>\n", ns_overridden(p->info.typ->ref, q->next), wsdl_type(q->next->info.typ->ref, ns1), q->info.minOccurs, q->info.maxOccurs);
	          q = q->next;
	        }
                else if (q->info.typ->type == Tpointer || q->info.typ->type == Tarray || is_dynamic_array(q->info.typ))
                  fprintf(fd, "      <element name=\"%s\" type=\"%s\" minOccurs=\"%d\" maxOccurs=\"%d\" nillable=\"true\"/>\n", ns_overridden(p->info.typ->ref, q), wsdl_type(q->info.typ, ns1), q->info.minOccurs, q->info.maxOccurs);
	        else
	          fprintf(fd, "      <element name=\"%s\" type=\"%s\" minOccurs=\"%d\" maxOccurs=\"%d\"/>\n", ns_overridden(p->info.typ->ref, q), wsdl_type(q->info.typ, ns1), q->info.minOccurs, q->info.maxOccurs);
              fprintf(fd, "     </sequence>\n    </extension>\n   </complexContent>\n  </complexType>\n\n");
	    }
	    else
            { fprintf(fd, "  <complexType name=\"%s\">\n   <sequence>\n", ns_remove(p->sym->name));
              for (q = ((Table*)p->info.typ->ref)->list; q; q = q->next)
	        if (is_transient(q->info.typ) || q->info.typ->type == Tfun)
	          continue;
	        else if (is_repetition(q))
                { if (((Tnode*)q->next->info.typ->ref)->type == Tpointer)
                    fprintf(fd, "    <element name=\"%s\" type=\"%s\" minOccurs=\"0\" maxOccurs=\"unbounded\" nillable=\"true\"/>\n", ns_convert(q->next->sym->name), wsdl_type(q->next->info.typ->ref, ns1));
		  else
                    fprintf(fd, "    <element name=\"%s\" type=\"%s\" minOccurs=\"0\" maxOccurs=\"unbounded\"/>\n", ns_convert(q->next->sym->name), wsdl_type(q->next->info.typ->ref, ns1));
	          q = q->next;
	        }
                else if (q->info.typ->type == Tpointer || q->info.typ->type == Tarray || is_dynamic_array(q->info.typ))
                  fprintf(fd, "    <element name=\"%s\" type=\"%s\" minOccurs=\"%d\" maxOccurs=\"%d\" nillable=\"true\"/>\n", ns_convert(q->sym->name), wsdl_type(q->info.typ, ns1), q->info.minOccurs, q->info.maxOccurs);
	        else
                  fprintf(fd, "    <element name=\"%s\" type=\"%s\" minOccurs=\"%d\" maxOccurs=\"%d\"/>\n", ns_convert(q->sym->name), wsdl_type(q->info.typ, ns1), q->info.minOccurs, q->info.maxOccurs);
              fprintf(fd, "   </sequence>\n  </complexType>\n\n");
            }
        }
      }
  for (n = Tptr[Tarray]; n; n = n->next)
  { if (is_transient(n))
      continue;
    else if (wsdl)
      fprintf(fd, "  <complexType name=\"%s\">\n   <complexContent>\n    <restriction base=\"SOAP-ENC:Array\">\n     <attribute ref=\"SOAP-ENC:arrayType\" WSDL:arrayType=\"%s\"/>\n    </restriction>\n   </complexContent>\n  </complexType>\n\n", c_ident(n), xsi_type(n));
    else
      fprintf(fd, "  <complexType name=\"%s\">\n   <complexContent>\n    <restriction base=\"SOAP-ENC:Array\">\n     <element name=\"item\" type=\"%s\" maxOccurs=\"unbounded\"/>\n    </restriction>\n   </complexContent>\n  </complexType>\n\n", c_ident(n), xsi_type(n->ref));
  }
  fprintf(fd, " </schema>\n");
}

char *
emalloc(unsigned int n)
{ char	*p;
  if ((p = (char*)malloc(n)) == (char*)0)
    execerror("out of memory");
  return p;
}

void
function_input_output(Table *table)
{
  int i=1;
  Entry * p;
  fprintf(fserver,"\n\nSOAP_FMAC1 int SOAP_FMAC2 soap_serve(struct soap *soap)\n{"); 
  fprintf(fserver,"\n\tsoap_begin(soap);");
  fprintf(fserver,"\n\tsoap_fault(soap);");

  fprintf(fserver,"\n\tif (soap_begin_recv(soap) || soap_envelope_begin_in(soap) || soap_recv_header(soap) || soap_body_begin_in(soap))\n\t\treturn soap_send_fault(soap);");

  for (p = table->list; p != (Entry*) 0; p = p->next)
    {
      if(p->info.typ->type==Tfun)
	{              
	  if(i == 1){
	    /* First method to be called */
	    fprintf(fserver,"\n\n\tsoap->error = soap_serve_%s(soap);", p->sym->name);   
	  }
	  else{
	    /* Methods following other methods */
	    fprintf(fserver,"\n\tif (soap->error == SOAP_NO_METHOD)");
	    fprintf(fserver,"\n\t\tsoap_serve_%s(soap);", p->sym->name);
	  }
	  i++;
	}
    }
  
  fprintf(fserver,"\n\tif (soap->error)");
  fprintf(fserver,"\n\t\treturn soap_send_fault(soap);");
  fprintf(fserver,"\n\treturn soap->error;");
  fprintf(fserver,"\n}");
  
  fprintf(fheader, "\n\n/* Remote Methods */");
  for (p = table->list; p != (Entry*) 0; p = p->next)
    if(p->info.typ->type==Tfun)
	generate_proto(table, p);
  fprintf(fheader, "\n\n/* Stubs */");
  for (p = table->list; p != (Entry*) 0; p = p->next)
    if(p->info.typ->type==Tfun)
	generate_call(table, p);
  fprintf(fheader, "\n\n/* Skeletons */");
  for (p = table->list; p != (Entry*) 0; p = p->next)
    if(p->info.typ->type==Tfun)
	generate_server(table, p);
}

void
generate_proto(Table *table, Entry *param)
{ Entry *pin,*q,*pout;
  Table *output,*t;
  q=entry(table, param->sym);
  if (q)
    pout = (Entry*)q->info.typ->ref;
  else	fprintf(stderr, "Internal error: no table entry\n");
  q=entry(classtable, param->sym);
  output=(Table*) q->info.typ->ref;
  fprintf(fheader, "\n\nSOAP_FMAC1 int SOAP_FMAC2 %s(struct soap*",param->sym->name);
  for(t=output;t!=(Table*) 0;t=t->prev)
  {

    for (pin = t->list; pin != (Entry*) 0; pin = pin->next)
    {	if (pin->info.typ->type == Treference)
    	{	sprintf(errbuf, "Cannot pass input argument \"%s\" of remote method function \"%s\" by reference: use pointer", pin->sym->name, param->sym->name);
		semerror(errbuf);
	}
	  fprintf(fheader,", %s%s",c_storage(pin->info.sto), c_type(pin->info.typ));
    }
  }
  if (is_transient(pout->info.typ))
    fprintf(fheader,");");
  else
    fprintf(fheader,", %s%s);", c_storage(pout->info.sto), c_type(pout->info.typ));
}

void
generate_call(Table *table, Entry *param)
{ Service *sp;
  Entry *pin,*q,*pout,*response=NULL;
  Tnode *temp;
  Table *output,*t;
  int cardinality, element_width, i, flag = 0;
  q=entry(table, param->sym);
  if (q)
    pout = (Entry*)q->info.typ->ref;
  else	fprintf(stderr, "Internal error: no table entry\n");
  q=entry(classtable, param->sym);
  output=(Table*) q->info.typ->ref;

  if (!is_response(pout->info.typ))
  { response = get_response(param->info.typ);
  }
  
  /* soapStub.h*/
  if (is_transient(pout->info.typ))
  { fprintf(fheader, "\n\nSOAP_FMAC1 int SOAP_FMAC2 soap_send_%s(struct soap*, const char*, const char*",param->sym->name);
    fprintf(fclient, "\n\nSOAP_FMAC1 int SOAP_FMAC2 soap_send_%s(struct soap *soap, const char *URL, const char *action",param->sym->name);
  }
  else
  { fprintf(fheader, "\n\nSOAP_FMAC1 int SOAP_FMAC2 soap_call_%s(struct soap*, const char*, const char*",param->sym->name);
    fprintf(fclient, "\n\nSOAP_FMAC1 int SOAP_FMAC2 soap_call_%s(struct soap *soap, const char *URL, const char *action",param->sym->name);
  }
  fflush(fheader);
  fflush(fclient);

  /* Parameters being passed */
  for(t=output;t!=(Table*) 0;t=t->prev)
  { for (pin = t->list; pin != (Entry*) 0; pin = pin->next)
    { fprintf(fclient,", %s%s",c_storage(pin->info.sto), c_type_id(pin->info.typ,pin->sym->name));
      fprintf(fheader,", %s%s",c_storage(pin->info.sto), c_type(pin->info.typ));
    }
  }

  /* Return value */
  fflush(fclient);
  if (!is_transient(pout->info.typ))
  { fprintf(fclient, ", %s%s)\n{", c_storage(pout->info.sto), c_type_id(pout->info.typ, pout->sym->name));
    fprintf(fheader,", %s%s);", c_storage(pout->info.sto), c_type(pout->info.typ));
  }
  else
  { fprintf(fclient, ")\n{");
    fprintf(fheader,");");
  }
  fflush(fclient);
  
  fprintf(fclient,"\n\tstruct %s soap_tmp_%s;",param->sym->name,param->sym->name);
  if (!is_response(pout->info.typ) && response)
  { fprintf(fclient,"\n\tstruct %s *soap_tmp_%s;",c_ident(response->info.typ), c_ident(response->info.typ));
  } 
  for (sp = services; sp; sp = sp->next)
    if (has_ns_eq(sp->ns, param->sym->name))
      if (sp->encoding && sp->URI)
      { flag = 1;
        fprintf(fclient, "\n\tconst char *soap_tmp_encodingStyle = soap->encodingStyle;");
        fprintf(fclient, "\n\tconst char *soap_tmp_defaultNamespace = soap->defaultNamespace;");
        fprintf(fclient, "\n\tint soap_tmp_disable_href = soap->disable_href;");
        if (!strcmp(sp->encoding, "literal"))
        { fprintf(fclient, "\n\tsoap->encodingStyle = NULL;");
          fprintf(fclient, "\n\tsoap->defaultNamespace = \"%s\";", sp->URI);
	}
        else
        { fprintf(fclient, "\n\tsoap->encodingStyle = \"%s\";", sp->encoding);
          fprintf(fclient, "\n\tsoap->defaultNamespace = \"%s\";", sp->URI);
	}
        fprintf(fclient, "\n\tsoap->disable_href = 1;");
	break;
      }
  fflush(fclient);
  for(t=output;t!=(Table*) 0;t=t->prev) 
    for (pin = t->list; pin != (Entry*) 0; pin = pin->next)
      {
	if(pin->info.typ->type==Tarray)
	  {
	    temp = pin->info.typ;
	    cardinality = 0;
	    while(temp->type == Tarray){
	      cardinality ++;
	      temp = temp->ref;
	    }
	    element_width = temp->width;
	    
	    fprintf(fclient,"\n\tmemcpy(&(soap_tmp_%s.%s",
		    param->sym->name,pin->sym->name);
	    for(i=0;i<cardinality;i++){
	      fprintf(fclient,"[0]");
	    }
	    fprintf(fclient,"), ");
            fprintf(fclient,"&(%s", pin->sym->name);
            for(i=0;i<cardinality;i++){
              fprintf(fclient,"[0]");
            }
            fprintf(fclient,"), %d*sizeof(%s));", (pin->info.typ->width)/(element_width), c_type(temp));      
	  }			
	else fprintf (fclient,"\n\tsoap_tmp_%s.%s=%s;",
		      param->sym->name,pin->sym->name,pin->sym->name);
      }	
  fprintf(fclient,"\n\tsoap_begin(soap);");
  fprintf(fclient,"\n\tsoap_serializeheader(soap);");
  fprintf(fclient,"\n\tsoap_serialize_%s(soap, &soap_tmp_%s);", param->sym->name,param->sym->name);
  fprintf(fclient,"\n\tif (!soap->disable_request_count)");
  fprintf(fclient,"\n\t{\tsoap_begin_count(soap);");
  fprintf(fclient,"\n\t\tsoap_envelope_begin_out(soap);");
  fprintf(fclient,"\n\t\tsoap_putheader(soap);");
  fprintf(fclient,"\n\t\tsoap_body_begin_out(soap);");
  fprintf(fclient,"\n\t\tsoap_put_%s(soap, &soap_tmp_%s, \"%s\", \"\");", param->sym->name,param->sym->name, ns_convert(param->sym->name)); 
  fprintf(fclient,"\n\t\tsoap_body_end_out(soap);");
  fprintf(fclient,"\n\t\tsoap_envelope_end_out(soap);");
  fprintf(fclient,"\n\t}");
  fprintf(fclient,"\n\tsoap_begin_send(soap);");
  fprintf(fclient,"\n\tif (soap_connect(soap, URL, action))\n\t{");
  if (flag)
  { fprintf(fclient, "\n\tsoap->encodingStyle = soap_tmp_encodingStyle;");
    fprintf(fclient, "\n\tsoap->defaultNamespace = soap_tmp_defaultNamespace;");
    fprintf(fclient, "\n\tsoap->disable_href = soap_tmp_disable_href;");
  }
  fprintf(fclient, "\n\t\treturn soap->error;");
  fprintf(fclient, "\n\t}");
  fprintf(fclient,"\n\tsoap_envelope_begin_out(soap);");
  fprintf(fclient,"\n\tsoap_putheader(soap);");
  fprintf(fclient,"\n\tsoap_body_begin_out(soap);");
  fprintf(fclient,"\n\tsoap_put_%s(soap, &soap_tmp_%s, \"%s\", \"\");", param->sym->name,param->sym->name,ns_convert(param->sym->name)); 
  fprintf(fclient,"\n\tsoap_body_end_out(soap);");
  fprintf(fclient,"\n\tsoap_envelope_end_out(soap);");
  fprintf(fclient,"\n\tsoap_putattachments(soap);");
  fprintf(fclient,"\n\tsoap_end_send(soap);");
  if (flag)
  { fprintf(fclient, "\n\tsoap->encodingStyle = soap_tmp_encodingStyle;");
    fprintf(fclient, "\n\tsoap->defaultNamespace = soap_tmp_defaultNamespace;");
    fprintf(fclient, "\n\tsoap->disable_href = soap_tmp_disable_href;");
  }
  fflush(fclient);
  
  if (is_transient(pout->info.typ))
  { fprintf(fclient, "\n\treturn soap->error;\n}");
    fprintf(fheader, "\n\nSOAP_FMAC1 int SOAP_FMAC2 soap_recv_%s(struct soap*, ",param->sym->name);
    fprintf(fclient, "\n\nSOAP_FMAC1 int SOAP_FMAC2 soap_recv_%s(struct soap *soap, ",param->sym->name);
    fprintf(fheader,"struct %s*);\n",param->sym->name);
    fprintf(fclient,"struct %s *%s)\n{",param->sym->name,pout->sym->name);
    fprintf(fclient,"\n\tsoap_default_%s(soap, %s);", param->sym->name,pout->sym->name);
    fprintf(fclient,"\n\tsoap_begin(soap);");
  }
  else if(pout->info.typ->type == Tarray)
    fprintf(fclient,"\n\tsoap_default_%s(soap, %s);", c_ident(pout->info.typ), pout->sym->name);
  else if (pout->info.typ->type == Treference && ((Tnode *) pout->info.typ->ref)->type == Tclass)
    fprintf(fclient,"\n\tif (&%s)\n\t\t%s.soap_default(soap);", pout->sym->name, pout->sym->name);
  else if( ((Tnode *) pout->info.typ->ref)->type == Tclass)
    fprintf(fclient,"\n\tif (%s)\n\t\t%s->soap_default(soap);", pout->sym->name, pout->sym->name);
  else if (pout->info.typ->type == Treference && is_XML(pout->info.typ->ref))
    fprintf(fclient,"\n\t%s = NULL;", pout->sym->name);
  else if (pout->info.typ->type == Tpointer && is_XML(pout->info.typ->ref))
    fprintf(fclient,"\n\t*%s = NULL;", pout->sym->name);
  else if (pout->info.typ->type == Treference)
    fprintf(fclient,"\n\tsoap_default_%s(soap, &%s);", c_ident((Tnode*)pout->info.typ->ref), pout->sym->name);
  else if (!is_void(pout->info.typ))
    fprintf(fclient,"\n\tsoap_default_%s(soap, %s);", c_ident((Tnode*)pout->info.typ->ref), pout->sym->name);
  fflush(fclient);

  fprintf(fclient,"\n\tif (soap_begin_recv(soap))\n\t\treturn soap->error;");
  fprintf(fclient,"\n\tif (soap_envelope_begin_in(soap))");
  fprintf(fclient,"\n\t\treturn soap->error;");
  fprintf(fclient,"\n\tif (soap_recv_header(soap))");
  fprintf(fclient,"\n\t\treturn soap->error;");
  fprintf(fclient,"\n\tif (soap_body_begin_in(soap))");
  fprintf(fclient,"\n\t\treturn soap->error;");
  fflush(fclient);

  if (is_transient(pout->info.typ))
  {
  fprintf(fclient,"\n\tsoap_get_%s(soap, %s, \"%s\", NULL);", param->sym->name, pout->sym->name, ns_convert(param->sym->name));
  fprintf(fclient,"\n\tif (soap->error == SOAP_TAG_MISMATCH && soap->level == 2)\n\t\tsoap->error = SOAP_NO_METHOD;");
  fprintf(fclient,"\n\tif (soap->error)");
  fprintf(fclient,"\n\t\treturn soap->error;");
  fprintf(fclient,"\n\t");
  fprintf(fclient,"\n\tif (soap_body_end_in(soap))\n\t\treturn soap->error;");
  fprintf(fclient,"\n\tif (soap_envelope_end_in(soap))\n\t\treturn soap->error;");
  fprintf(fclient,"\n\tif (soap_getattachments(soap))\n\t\treturn soap->error;");
  fprintf(fclient,"\n\tsoap_end_recv(soap);");
  fprintf(fclient,"\n\treturn soap->error;\n}");
  fflush(fclient);
  return;
  }
  
  if(pout->info.typ->type == Treference && ((Tnode *) pout->info.typ->ref)->type == Tclass && !is_dynamic_array(pout->info.typ->ref))
    fprintf(fclient,"\n\tif (&%s)\n\t\t%s.soap_get(soap, \"%s\", \"%s\");\n\telse\n\t\tsoap_get_%s(soap, &%s, \"%s\", \"%s\");", pout->sym->name,
    pout->sym->name, xml_tag(pout->info.typ), xsi_type(pout->info.typ), c_ident(pout->info.typ->ref), pout->sym->name, xml_tag(pout->info.typ), xsi_type(pout->info.typ));
  else if(pout->info.typ->type == Tpointer && ((Tnode *) pout->info.typ->ref)->type == Tclass && !is_dynamic_array(pout->info.typ->ref))
    fprintf(fclient,"\n\tif (%s)\n\t\t%s->soap_get(soap, \"%s\", \"%s\");\n\telse\n\t\tsoap_get_%s(soap, %s, \"%s\", \"%s\");", pout->sym->name,
    pout->sym->name, xml_tag(pout->info.typ), xsi_type(pout->info.typ), c_ident(pout->info.typ->ref), pout->sym->name, xml_tag(pout->info.typ), xsi_type(pout->info.typ));
  else if(pout->info.typ->type == Treference && ((Tnode *) pout->info.typ->ref)->type == Tstruct && !is_dynamic_array(pout->info.typ->ref))
    fprintf(fclient,"\n\tsoap_get_%s(soap, &%s, \"%s\", \"%s\");", c_ident(pout->info.typ->ref), pout->sym->name, xml_tag(pout->info.typ), xsi_type(pout->info.typ));
  else if(pout->info.typ->type == Tpointer && ((Tnode *) pout->info.typ->ref)->type == Tstruct && !is_dynamic_array(pout->info.typ->ref))
    fprintf(fclient,"\n\tsoap_get_%s(soap, %s, \"%s\", \"%s\");", c_ident(pout->info.typ->ref), pout->sym->name, xml_tag(pout->info.typ), xsi_type(pout->info.typ));
  else
  {
    fprintf(fclient,"\n\tsoap_tmp_%s = soap_get_%s(soap, NULL, \"%s\", \"%s\");", c_ident(response->info.typ), c_ident(response->info.typ), xml_tag(response->info.typ), xsi_type(response->info.typ));
  fflush(fclient);
  }
  fflush(fclient);
  fprintf(fclient,"\n\tif (soap->error){");
  {
  fprintf(fclient,"\n\t\tif (soap->error == SOAP_TAG_MISMATCH && soap->level == 2)\n\t\t\tsoap_recv_fault(soap);");
  }
  fprintf(fclient,"\n\t\treturn soap->error;");
  fprintf(fclient,"\n\t}");

  fprintf(fclient,"\n\tif (soap_body_end_in(soap))\n\t\treturn soap->error;");
  fprintf(fclient,"\n\tif (soap_envelope_end_in(soap))\n\t\treturn soap->error;");
  fprintf(fclient,"\n\tif (soap_getattachments(soap))\n\t\treturn soap->error;");
  fprintf(fclient,"\n\tsoap_closesock(soap);");
  fprintf(fclient,"\n\tsoap_end_recv(soap);");
  if (!is_response(pout->info.typ))
  { if (pout->info.typ->type == Tarray)
	{    temp = pout->info.typ;
	    cardinality = 0;
	    while(temp->type == Tarray){
	      cardinality ++;
	      temp = temp->ref;
	    }
	    element_width = temp->width;
	    
	    fprintf(fclient,"\n\tmemcpy(&(%s", pout->sym->name);
	    for(i=0;i<cardinality;i++){
	      fprintf(fclient,"[0]");
	    }
	    fprintf(fclient,"), ");
            fprintf(fclient,"&(soap_tmp_%s->%s", c_ident(response->info.typ), pout->sym->name);
            for(i=0;i<cardinality;i++){
              fprintf(fclient,"[0]");
            }
            fprintf(fclient,"), %d*sizeof(%s));", (pout->info.typ->width)/(element_width), c_type(temp));      
	}
    else if (pout->info.typ->type == Treference)
      fprintf(fclient,"\n\t%s = soap_tmp_%s->%s;", pout->sym->name, c_ident(response->info.typ), pout->sym->name);
    else
    { fprintf(fclient,"\n\tif (soap_tmp_%s->%s)", c_ident(response->info.typ), pout->sym->name);
      fprintf(fclient,"\n\t\t*%s = *soap_tmp_%s->%s;", pout->sym->name, c_ident(response->info.typ), pout->sym->name);
    }
  }
  fprintf(fclient,"\n\treturn soap->error;");
  fprintf(fclient ,"\n}");
}

void
generate_server(Table *table, Entry *param)
{ Service *sp;
  int flag = 0;
  Entry *q,*pin, *pout, *response;
  Table *t, *input;
  q=entry(table, param->sym);
  if (q)
    pout = (Entry*)q->info.typ->ref;
  else
    fprintf(stderr, "Internal error: no table entry");
  if (!is_response(pout->info.typ))
    response = get_response(param->info.typ);
  fprintf(fheader, "\n\nSOAP_FMAC1 int SOAP_FMAC2 soap_serve_%s(struct soap*);",param->sym->name);
  fprintf(fserver, "\n\nSOAP_FMAC1 int SOAP_FMAC2 soap_serve_%s(struct soap *soap)",param->sym->name);
  fprintf(fserver, "\n{\tstruct %s soap_tmp_%s;", param->sym->name, param->sym->name);
  if (!is_transient(pout->info.typ))
  for (sp = services; sp; sp = sp->next)
    if (has_ns_eq(sp->ns, param->sym->name))
      if (sp->encoding && sp->URI)
      { flag = 1;
        fprintf(fserver, "\n\tconst char *soap_tmp_encodingStyle = soap->encodingStyle;");
        fprintf(fserver, "\n\tconst char *soap_tmp_defaultNamespace = soap->defaultNamespace;");
        fprintf(fserver, "\n\tshort soap_tmp_disable_href = soap->disable_href;");
	break;
      }
  fflush(fserver);
  if (is_transient(pout->info.typ))
    ;
  else if(pout->info.typ->type == Tarray){
    /* ARRAY */
    fprintf(fserver,"\n\tstruct %s soap_tmp_%s;", c_ident(response->info.typ), c_ident(response->info.typ));
    fprintf(fserver,"\n\tsoap_default_%s(soap, &soap_tmp_%s);", c_ident(response->info.typ),c_ident(response->info.typ));
  }else if(((Tnode *)pout->info.typ->ref)->type == Tclass && !is_dynamic_array(pout->info.typ->ref)){
    /* CLASS */
    fprintf(fserver, "\n\t%s;", c_type_id((Tnode*)pout->info.typ->ref, pout->sym->name));
    fprintf(fserver,"\n\t%s.soap_default(soap);", pout->sym->name);
  }else if(((Tnode *)pout->info.typ->ref)->type == Tstruct && !is_dynamic_array(pout->info.typ->ref)){
    fprintf(fserver, "\n\t%s;", c_type_id((Tnode*)pout->info.typ->ref, pout->sym->name));
    fprintf(fserver,"\n\tsoap_default_%s(soap, &%s);", c_ident((Tnode *)pout->info.typ->ref),pout->sym->name);  
  } else if (pout->info.typ->type == Tpointer) {
    fprintf(fserver,"\n\tstruct %s soap_tmp_%s;", c_ident(response->info.typ), c_ident(response->info.typ));
    fprintf(fserver,"\n\t%s soap_tmp_%s;", c_type(pout->info.typ->ref), c_ident(pout->info.typ->ref));
    fprintf(fserver,"\n\tsoap_default_%s(soap, &soap_tmp_%s);", c_ident(response->info.typ),c_ident(response->info.typ));
    if (((Tnode*)pout->info.typ->ref)->type == Tclass)
      fprintf(fserver,"\n\tsoap_tmp_%s.soap_default(soap);", c_ident(pout->info.typ->ref));
    else
      fprintf(fserver,"\n\tsoap_default_%s(soap, &soap_tmp_%s);", c_ident(pout->info.typ->ref),c_ident(pout->info.typ->ref));
    fprintf(fserver,"\n\tsoap_tmp_%s.%s = &soap_tmp_%s;", c_ident(response->info.typ),pout->sym->name, c_ident(pout->info.typ->ref));
  } else  {
    fprintf(fserver,"\n\tstruct %s soap_tmp_%s;", c_ident(response->info.typ), c_ident(response->info.typ));
    fprintf(fserver,"\n\tsoap_default_%s(soap, &soap_tmp_%s);", c_ident(response->info.typ),c_ident(response->info.typ));
  }
  fprintf(fserver,"\n\tsoap_default_%s(soap, &soap_tmp_%s);", param->sym->name,param->sym->name);
  fflush(fserver);
  fprintf(fserver,"\n\tsoap_get_%s(soap, &soap_tmp_%s, \"%s\", NULL);", param->sym->name, param->sym->name, ns_convert(param->sym->name));
  fprintf(fserver,"\n\tif (soap->error == SOAP_TAG_MISMATCH && soap->level == 2)\n\t\tsoap->error = SOAP_NO_METHOD;");
  fprintf(fserver,"\n\tif (soap->error)");
  fprintf(fserver,"\n\t\treturn soap->error;");
  fprintf(fserver,"\n\t");
  fprintf(fserver,"\n\tif (soap_body_end_in(soap))\n\t\treturn soap->error;");
  fprintf(fserver,"\n\tif (soap_envelope_end_in(soap))\n\t\treturn soap->error;");
  fprintf(fserver,"\n\tif (soap_getattachments(soap))\n\t\treturn soap->error;");
  fprintf(fserver,"\n\tif (soap_end_recv(soap))\n\t\treturn soap->error;");
  fprintf(fserver, "\n\tsoap->error = %s(soap",param->sym->name);
  fflush(fserver);
  q=entry(classtable, param->sym);
  input=(Table*) q->info.typ->ref;

  for (t=input; t!=(Table*)0; t=t->prev) 
    for (pin = t->list; pin != (Entry*) 0; pin = pin->next)
      fprintf(fserver,", soap_tmp_%s.%s",param->sym->name, pin->sym->name);

  if (is_transient(pout->info.typ))
    fprintf(fserver, ");");
  else  if(pout->info.typ->type == Tarray){
    fprintf(fserver, ", soap_tmp_%s.%s);", c_ident(response->info.typ), pout->sym->name);
  }else if(pout->info.typ->type == Treference && (((Tnode*)pout->info.typ->ref)->type == Tstruct || ((Tnode*)pout->info.typ->ref)->type == Tclass) && !is_dynamic_array(pout->info.typ->ref)) {
    fprintf(fserver, ", %s);", pout->sym->name);
  }else if((((Tnode*)pout->info.typ->ref)->type == Tstruct || ((Tnode*)pout->info.typ->ref)->type == Tclass) && !is_dynamic_array(pout->info.typ->ref)) {
    fprintf(fserver, ", &%s);", pout->sym->name);
  }else if(pout->info.typ->type == Treference){
    fprintf(fserver, ", soap_tmp_%s.%s);", c_ident(response->info.typ), pout->sym->name);
  }else{
    fprintf(fserver, ", &soap_tmp_%s);", c_ident(pout->info.typ->ref));
  }
  fprintf(fserver,"\n\tif (soap->error)\n\t\treturn soap->error;");
  
  if (!is_transient(pout->info.typ))
  {
  if (flag)
  {     if (!strcmp(sp->encoding, "literal"))
        { fprintf(fserver, "\n\tsoap->encodingStyle = NULL;");
          fprintf(fserver, "\n\tsoap->defaultNamespace = \"%s\";", sp->URI);
	}
        else
        { fprintf(fserver, "\n\tsoap->encodingStyle = \"%s\";", sp->encoding);
          fprintf(fserver, "\n\tsoap->defaultNamespace = \"%s\";", sp->URI);
	}
        fprintf(fserver, "\n\tsoap->disable_href = 1;");
  }
  fprintf(fserver,"\n\tsoap_serializeheader(soap);");
  DBGLOG(fprintf(stderr,"\n##%d##\n",pout->info.typ->type == Tarray));
  if(pout->info.typ->type == Tarray){
    fprintf(fserver, "\n\tsoap_serialize_%s(soap, &soap_tmp_%s);", c_ident(response->info.typ), c_ident(response->info.typ));
  }else if(((Tnode *)pout->info.typ->ref)->type == Tclass && !is_dynamic_array(pout->info.typ->ref)){
    fprintf(fserver, "\n\t%s.soap_serialize(soap);", pout->sym->name);
  }else if(((Tnode *)pout->info.typ->ref)->type == Tstruct && !is_dynamic_array(pout->info.typ->ref)){
    fprintf(fserver, "\n\tsoap_serialize_%s(soap, &%s);", c_ident((Tnode*)pout->info.typ->ref), pout->sym->name);
  }else{
    fprintf(fserver, "\n\tsoap_serialize_%s(soap, &soap_tmp_%s);", c_ident(response->info.typ), c_ident(response->info.typ));
  }
  fprintf(fserver, "\n\tif (!soap->disable_response_count)");
  fprintf(fserver, "\n\t{\tsoap_begin_count(soap);");
  fprintf(fserver, "\n\t\tsoap_envelope_begin_out(soap);");
  fprintf(fserver,"\n\t\tsoap_putheader(soap);");
  fprintf(fserver,"\n\t\tsoap_body_begin_out(soap);");
  if(pout->info.typ->type == Tarray){
    fprintf(fserver,"\n\t\tsoap_put_%s(soap, &soap_tmp_%s.%s, \"%s\", \"\");",
	    c_ident(response->info.typ), c_ident(response->info.typ), pout->sym->name, xml_tag(pout->info.typ->ref));
  }else if(((Tnode *)pout->info.typ->ref)->type == Tclass && !is_dynamic_array(pout->info.typ->ref)){
    fprintf(fserver, "\n\t\t%s.soap_put(soap, \"%s\", \"\");", pout->sym->name, xml_tag(pout->info.typ->ref));
  }else if(((Tnode *)pout->info.typ->ref)->type == Tstruct && !is_dynamic_array(pout->info.typ->ref)){
    fprintf(fserver, "\n\t\tsoap_put_%s(soap, &%s, \"%s\", \"\");",
	    c_ident((Tnode*)pout->info.typ->ref), pout->sym->name, xml_tag(pout->info.typ->ref));
  }else 
  { 
    fprintf(fserver,"\n\t\tsoap_put_%s(soap, &soap_tmp_%s, \"%s\", \"\");",
	    c_ident(response->info.typ), c_ident(response->info.typ), xml_tag(response->info.typ));
  }
  fprintf(fserver,"\n\t\tsoap_body_end_out(soap);");
  fprintf(fserver,"\n\t\tsoap_envelope_end_out(soap);");
  fprintf(fserver,"\n\t};");

  /* PRINTING PHASE */
  fprintf(fserver, "\n\tsoap_begin_send(soap);");
  fprintf(fserver,"\n\tsoap_response(soap, SOAP_OK);");
  fprintf(fserver,"\n\tsoap_envelope_begin_out(soap);");
  fprintf(fserver,"\n\tsoap_putheader(soap);");
  fprintf(fserver,"\n\tsoap_body_begin_out(soap);");
  if(pout->info.typ->type == Tarray){
    /* ARRAY */
    fprintf(fserver,"\n\tsoap_put_%s(soap, &soap_tmp_%s.%s, \"%s\", \"\");",
	    c_ident(response->info.typ), c_ident(response->info.typ), pout->sym->name, xml_tag(pout->info.typ->ref));
  }else if(((Tnode *)pout->info.typ->ref)->type == Tclass && !is_dynamic_array(pout->info.typ->ref)){
    /* CLASS */
    fprintf(fserver, "\n\t%s.soap_put(soap, \"%s\", \"\");", pout->sym->name, xml_tag(pout->info.typ->ref));
  }else if(((Tnode *)pout->info.typ->ref)->type == Tstruct && !is_dynamic_array(pout->info.typ->ref)){
    fprintf(fserver, "\n\tsoap_put_%s(soap, &%s, \"%s\", \"\");",
	    c_ident((Tnode*)pout->info.typ->ref), pout->sym->name, xml_tag(pout->info.typ->ref));
  }else 
  { 
    fprintf(fserver,"\n\tsoap_put_%s(soap, &soap_tmp_%s, \"%s\", \"\");",
	    c_ident(response->info.typ), c_ident(response->info.typ), xml_tag(response->info.typ));
  }
  fprintf(fserver,"\n\tsoap_body_end_out(soap);");
  fprintf(fserver,"\n\tsoap_envelope_end_out(soap);");
  fprintf(fserver, "\n\tsoap_putattachments(soap);");
  fprintf(fserver, "\n\tsoap_end_send(soap);");
  }

  if (flag)
  { fprintf(fserver, "\n\tsoap->encodingStyle = soap_tmp_encodingStyle;");
    fprintf(fserver, "\n\tsoap->defaultNamespace = soap_tmp_defaultNamespace;");
    fprintf(fserver, "\n\tsoap->disable_href = soap_tmp_disable_href;");
  }
  fprintf(fserver, "\n\tsoap_closesock(soap);");
  fprintf(fserver,"\n\treturn SOAP_OK;");
  fprintf(fserver, "\n}");
  fflush(fserver);
}

int
is_XML(Tnode *p)
{ return p->sym && (is_string(p) || is_wstring(p)) && !strcmp(p->sym->name, "XML");
}

int
is_response(Tnode *p)
{ return (p->type == Tpointer || p->type == Treference) && p->ref && ((((Tnode*)p->ref)->type == Tstruct || ((Tnode*)p->ref)->type == Tclass) && !is_dynamic_array(p->ref));
}

Entry*
get_response(Tnode *p)
{ if (p->type == Tfun)
    return p->response;
  return 0;
}

int
is_unmatched(Symbol *sym)
{ return *sym->name == '_';
}

int
is_untyped(Tnode *typ)
{ Tnode *p;
  if (typ->sym)
    return is_unmatched(typ->sym);
  if (typ->type == Tstruct || typ->type == Tclass)
    if (is_dynamic_array(typ) && !has_ns(typ) && !is_binary(typ))
    { p = ((Table*)typ->ref)->list->info.typ->ref;
      if (p->type == Tpointer)
        if (is_primitive(p->ref) || is_string(p->ref) || is_wstring(p->ref))
	  return 0;
        else
          return is_untyped(p);
      else if (is_primitive(p) || is_string(p) || is_wstring(p))
        return 0;
      else
        return is_untyped(p);
    }
    else
      return is_unmatched(typ->id);
  if (typ->type == Tpointer || typ->type == Treference || typ->type == Tarray)
    return is_untyped(typ->ref);
  if ((is_primitive(typ) || is_string(typ) || is_wstring(typ)) && typ->type != Tenum)
    return 1;
  return 0;
}

int
is_primclass(Tnode *typ)
{ Table *t;
  if (typ->type == Tclass)
  { for (t = (Table*)typ->ref; t; t = t->prev)
      if (t->list && !strcmp(t->list->sym->name, "__item"))
        return 1;
  }
  else if (typ->type == Tpointer || typ->type == Treference)
    return is_primclass(typ->ref);
  return 0;
}

int
is_mask(Tnode *typ)
{ return (typ->type == Tenum && typ->width == 8);
}

int
is_void(Tnode *typ)
{ if (typ->type == Tvoid)
    return 1;
  if (typ->type == Tpointer)
    return is_void(typ->ref); 
  if (typ->type == Treference)
    return is_void(typ->ref); 
  if (typ->type == Tarray)
    return is_void(typ->ref); 
  return 0;
}

int
is_transient(Tnode *typ)
{ if (typ->transient < 0)
    return 0;
  if (typ->transient || typ->type == Tvoid || typ->type == Tunion || typ->type == Tnone)
    return 1;
  switch (typ->type)
  { case Tpointer:
    case Treference:
    case Tarray:
      return is_transient(typ->ref);
    case Tstruct:
      return typ->id == lookup("soap");
  }
  return 0;
}

int
is_repetition(Entry *p)
{ if (p)
    return p->next && p->next->info.typ->type == Tpointer && (p->info.typ->type == Tint || p->info.typ->type == Tuint) && !strncmp(p->sym->name, "__size", 6);
  return 0;
} 

int
has_ns(Tnode *typ)
{ char *s;
  if (typ->type == Tstruct || typ->type == Tclass || typ->type == Tenum)
  { s = strstr(typ->id->name, "__");
    return s && s[2] && s[2] != '_';
  }
  return 0;
}

int
has_ns_eq(char *ns, char *name)
{ int n;
  if ((n = strlen(ns)) < strlen(name))
    return name[n] == '_' && name[n+1] == '_' && !strncmp(ns, name, n);
  return 0;
}

char *
ns_overridden(Table *t, Entry *p)
{ Entry *q;
  Symbol *s = t->sym;
  char *n;
  if (s)
    while (t = t->prev)
      for (q = t->list; q; q = q->next)
        if (!strcmp(q->sym->name, p->sym->name))
        { n = (char*)emalloc(strlen(s->name)+strlen(p->sym->name)+2);
	  strcpy(n, s->name);
	  strcat(n, ".");
	  strcat(n, p->sym->name);
	  return ns_convert(n);
        }
  return ns_convert(p->sym->name);
}

/*c_ident gives the names of the function for a type*/

char *
c_ident(Tnode *typ)
{ char *p;
  
  /*if( dumptypename(typetable,typ)!=0)
    {
      p=(char*) malloc(strlen(dumptypename(typetable,typ))*sizeof(char));
      strcpy(p,dumptypename(typetable,typ));
      DBGLOG(fprintf(stderr,"\n Return before switch in c_ident() %s\n", p));
      return p;
    }*/
  if (typ->sym)
    return typ->sym->name;

  switch(typ->type){
  case Tnone :
    return "";
  case Tvoid :
    return "void";
  case Tchar :
    return "byte";
  case Twchar :
    return "wchar";
  case Tshort :
    return "short";
  case Tint  :
    return "int";
  case Tlong  :
    return "long";
  case Tllong  :
    return "LONG64";
  case Tfloat:
    return "float";
  case Tdouble:
    return "double";
  case Tuchar:
    return "unsignedByte";
  case Tushort:
    return "unsignedShort";
  case Tuint:
    return "unsignedInt";
  case Tulong:
    return "unsignedLong";
  case Tullong:
    return "unsignedLONG64";
  case Ttime:
    return "time";
  case Tstruct:
  case Tclass:
  case Tunion:
  case Tenum:
    if (typ->ref == booltable)
      return "bool";
    return typ->id->name;
  case Treference:
    return c_ident(typ->ref);
  case Tpointer:
    if(((Tnode*)typ->ref)->type == Tchar) 
	return "string";
    if(((Tnode*)typ->ref)->type == Twchar) 
	return "wstring";
    p=(char*) malloc((10+strlen(c_ident(typ->ref)))*sizeof(char));
    strcpy(p,"PointerTo");
    strcat(p,c_ident(typ->ref));
    return p;
  case Tarray :p=(char*) malloc((16+strlen(c_ident(typ->ref)))*sizeof(char));
    sprintf(p,"Array%dOf%s",typ->width / ((Tnode*) typ->ref)->width,c_ident(typ->ref));
    return p;
  }
  return "anyType";
}

char *
ns_convert(char *tag)
{ char *t, *s;
  int i, n;
  if (*tag == '_')
    tag++;
  for (n = strlen(tag); n > 0; n--)
    if (tag[n-1] != '_')
      break;
  s = t = (char*)malloc(n+1);
  for (i = 0; i < n; i++)
  { if (tag[i] == '_')
      if (tag[i+1] == '_')
        break;
      else if (!strncmp(tag+i, "_DOT_", 5))
      { *s++ = '.';
        i += 4;
      }
      else if (!strncmp(tag+i, "_USCORE_", 8))
      { *s++ = '_';
        i += 7;
      }
      else if (!strncmp(tag+i, "_CDOT_", 6))
      { *s++ = 0xB7;
        i += 5;
      }
      else
        *s++ = '-';
    else
      *s++ = tag[i];
  }
  if (i < n)
  { *s++ = ':';
    for (i += 2; i < n; i++)
      if (tag[i] == '_')
        if (!strncmp(tag+i, "_DOT_", 5))
        { *s++ = '.';
          i += 4;
        }
        else if (!strncmp(tag+i, "_USCORE_", 8))
        { *s++ = '_';
          i += 7;
        }
        else if (!strncmp(tag+i, "_CDOT_", 6))
        { *s++ = 0xB7;
          i += 5;
        }
	else
	  *s++ = '-';
      else
        *s++ = tag[i];
  }
  *s = '\0';
  return t;
}

char *
ns_remove(char *tag)
{ char *t, *s = ns_convert(tag);
  if (t = strchr(s, ':'))
    return t+1;
  return s;
}

char *
xsi_type(Tnode *typ)
{ if (!typ)
    return "NULL";
  if (is_untyped(typ))
    return "";
  if (is_dynamic_array(typ) && !has_ns(typ))
    return xsi_type_Darray(typ);
  if (typ->sym)
    if (!strncmp(typ->sym->name, "SOAP_ENV__", 10))
      return "";
    else
      return ns_convert(typ->sym->name);
  if (is_string(typ) || is_wstring(typ))
    return "string";
  switch(typ->type){
  case Tchar :
    return "byte";
  case Twchar :
    return "wchar";
  case Tshort :
    return "short";
  case Tint  :
    return "int";
  case Tlong  :
  case Tllong  :
    return "long";
  case Tfloat:
    return "float";
  case Tdouble:
    return "double";
  case Tuchar:
    return "unsignedByte";
  case Tushort:
    return "unsignedShort";
  case Tuint:
    return "unsignedInt";
  case Tulong:
  case Tullong:
    return "unsignedLong";
  case Ttime:
    return "dateTime";
  case Tpointer:
  case Treference:
    return xsi_type(typ->ref);
  case Tarray:
    return xsi_type_Tarray(typ);
  case Tenum:
    if (typ->ref == booltable)
      return "boolean";
  case Tstruct:
  case Tclass:
    if (!has_ns(typ) || !strncmp(typ->id->name, "SOAP_ENV__", 10))
      return "";
    return ns_convert(typ->id->name);
  }
  return "";
}

char *
xml_tag(Tnode *typ)
{ if (!typ)
    return "NULL";
  if (typ->type == Tpointer || typ->type == Treference)
    return xml_tag(typ->ref);
  if (typ->sym)
    return ns_convert(typ->sym->name);
  return the_type(typ);
}

char *
wsdl_type(Tnode *typ, char *ns)
{ int d;
  char *s, *t;
  if (!typ)
    return "NULL";
  if (typ->sym)
    if (ns)
      return ns_convert(typ->sym->name);
    else
      return ns_remove(typ->sym->name);
  if (is_string(typ) || is_wstring(typ))
    if (ns)
      return "xsd:string";
    else
      return "string";
  if (is_dynamic_array(typ) && !has_ns(typ) && !is_untyped(typ))
  { s = ns_remove(wsdl_type(((Table*)typ->ref)->list->info.typ, NULL));
    if (ns)
    { t = (char*)malloc(strlen(s)+strlen(ns)+13);
      strcpy(t, ns_convert(ns));
      strcat(t, ":");
      strcat(t, "ArrayOf");
    }
    else
    { t = (char*)malloc(strlen(s)+8);
      strcpy(t, "ArrayOf");
    }
    strcat(t, s);
    d = get_Darraydims(typ);
    if (d)
      sprintf(t+strlen(t), "%dD", d);
    return t;
  }
  switch(typ->type){
  case Tchar :
    if (ns)
      return "xsd:byte";
    else
      return "byte";
  case Twchar :
    if (ns)
      return "xsd:wchar";
    else
      return "wchar";
  case Tshort :
    if (ns)
      return "xsd:short";
    else
      return "short";
  case Tint  :
    if (ns)
      return "xsd:int";
    else
      return "int";
  case Tlong  :
  case Tllong  :
    if (ns)
      return "xsd:long";
    else
      return "long";
  case Tfloat:
    if (ns)
      return "xsd:float";
    else
      return "float";
  case Tdouble:
    if (ns)
      return "xsd:double";
    else
      return "double";
  case Tuchar:
    if (ns)
      return "xsd:unsignedByte";
    else
      return "unsignedByte";
  case Tushort:
    if (ns)
      return "xsd:unsignedShort";
    else
      return "unsignedShort";
  case Tuint:
    if (ns)
      return "xsd:unsignedInt";
    else
      return "unsignedInt";
  case Tulong:
  case Tullong:
    if (ns)
      return "xsd:unsignedLong";
    else
      return "unsignedLong";
  case Ttime:
    if (ns)
      return "xsd:dateTime";
    else
      return "dateTime";
  case Tpointer:
  case Treference:
    return wsdl_type(typ->ref, ns);
  case Tarray:
    if (ns)
    { s = (char*)malloc((strlen(ns)+strlen(c_ident(typ))+2)*sizeof(char));
      strcpy(s, ns_convert(ns));
      strcat(s, ":");
      strcat(s, c_ident(typ));
      return s;
    }
    else
      return c_ident(typ);
  case Tenum:
    if (typ->ref == booltable)
      if (ns)
        return "xsd:boolean";
      else
        return "boolean";
  case Tstruct:
  case Tclass:
    if (!has_ns(typ) && ns)
    { s = (char*)malloc((strlen(ns)+strlen(typ->id->name)+2)*sizeof(char));
      strcpy(s, ns_convert(ns));
      strcat(s, ":");
      strcat(s, ns_convert(typ->id->name));
      return s;
    }
    else if (ns)
      return ns_convert(typ->id->name);
    else
      return ns_remove(typ->id->name);
  }
  return "";
}

char *
the_type(Tnode *typ)
{ if (!typ)
    return "NULL";
  if (typ->type == Tarray || is_dynamic_array(typ) && !has_ns(typ) && !is_untyped(typ))
    return "SOAP-ENC:Array";
  if (is_string(typ) || is_wstring(typ))
    return "string";
  switch(typ->type){
  case Tchar :
    return "byte";
  case Twchar :
    return "wchar";
  case Tshort :
    return "short";
  case Tint  :
    return "int";
  case Tlong  :
  case Tllong  :
    return "long";
  case Tfloat:
    return "float";
  case Tdouble:
    return "double";
  case Tuchar:
    return "unsignedByte";
  case Tushort:
    return "unsignedShort";
  case Tuint:
    return "unsignedInt";
  case Tulong:
  case Tullong:
    return "unsignedLong";
  case Ttime:
    return "dateTime";
  case Tpointer:
  case Treference:
    return the_type(typ->ref);
  case Tarray:
    return "SOAP-ENC:Array";
  case Tenum:
    if (typ->ref == booltable)
      return "boolean";
  case Tstruct:
  case Tclass:
    return ns_convert(typ->id->name);
  }
  return "";
}

/* c_type returns the type to be used in parameter declaration*/
char *
c_type(Tnode *typ)
{
  char *p, tempBuf[10];
  Tnode *temp;
  
  if (typ==0)
    return "NULL";
  
  switch(typ->type){
  case Tnone :
    return "";
  case Tvoid :
    return "void";
  case Tchar :
    return "char";
  case Twchar :
    return "wchar_t";
  case Tshort :
    return "short";
  case Tint   :
    return "int";
  case Tlong   :
    return "long";
  case Tllong   :
    return "LONG64";
  case Tfloat :
    return "float";
  case Tdouble:
    return "double";
  case Tuchar :
    return "unsigned char";
  case Tushort:
    return "unsigned short";
  case Tuint :
    return "unsigned int";
  case Tulong:
    return "unsigned long";
  case Tullong:
    return "ULONG64";
  case Ttime:
    return "time_t";
  case Tstruct:p=(char*) malloc((8+strlen(typ->id->name)) *sizeof(char));
    strcpy(p,"struct ");
    strcat(p,typ->id->name);
    break;
  case Tclass:
   p = typ->id->name;
   break;
  case Tunion: p=(char*) malloc((7+strlen(typ->id->name)) *sizeof(char));
    strcpy(p,"union ");
    strcat(p,typ->id->name);
    break;
  case Tenum:
    if (typ->ref == booltable)
      return "bool";
    p=(char*) malloc((6+strlen(typ->id->name)) *sizeof(char));
    strcpy(p,"enum ");
    strcat(p,typ->id->name);
    break;
  case Tpointer:
    p = c_type_id(typ->ref, "*");
    break;
  case Treference:
    p = c_type_id(typ->ref, "&");
    break;
  case Tarray :p=(char*) malloc((12+strlen(c_type(typ->ref))) *sizeof(char));

    temp = typ;
    while(((Tnode*) (typ->ref))->type==Tarray){
      typ = typ->ref;
    }

    if (((Tnode*)typ->ref)->type == Tpointer)
      sprintf(p,"%s",c_type(typ->ref));
    else
      strcpy(p, c_type(typ->ref));
     
    typ = temp;
    
    while(typ->type==Tarray){
      sprintf(tempBuf,"[%d]",(typ->width / ((Tnode*) typ->ref)->width));
      strcat(p,tempBuf);
      typ = typ->ref;
    }
    break;

  default :
    return "UnknownType";   
  }
  return p;
}

char *
c_storage(Storage sto)
{ char *p;
  static char buf[256];
  if (sto & Sconst)
  { p = c_storage(sto & ~Sconst);
    strcat(p, "const ");
    return p;
  }
  if (sto & Sauto)
  { p = c_storage(sto & ~Sauto);
    strcat(p, "auto ");
    return p;
  }
  if (sto & Sregister)
  { p = c_storage(sto & ~Sregister);
    strcat(p, "register ");
    return p;
  }
  if (sto & Sstatic)
  { p = c_storage(sto & ~Sstatic);
    strcat(p, "static ");
    return p;
  }
  if (sto & Sextern)
  { p = c_storage(sto & ~Sextern);
    return p;
  }
  if (sto & Stypedef)
  { p = c_storage(sto & ~Stypedef);
    strcat(p, "typedef ");
    return p;
  }
  if (sto & Svirtual)
  { p = c_storage(sto & ~Svirtual);
    strcat(p, "virtual ");
    return p;
  }
  if (sto & Sinline)
  { p = c_storage(sto & ~Sinline);
    strcat(p, "inline ");
    return p;
  }
  buf[0]= '\0';
  return buf;
}
/*
char *
c_init(Entry *e)
{ static char buf[256];
  buf[0] = '\0';
  if (e->info.hasval)
    switch (e->info.typ->type)
    { case Tchar:
      case Twchar:
      case Tuchar:
      case Tshort:
      case Tushort:
      case Tint:
      case Tuint:
      case Tlong:
      case Tllong:
      case Tulong:
      case Tullong:
        sprintf(buf, " = %lld", e->info.val.i);
	break;
      case Tfloat:
      case Tdouble:
        sprintf(buf, " = %f", e->info.val.r);
	break;
      default:
        sprintf(buf, " = \"%s\"", e->info.val.s);
	break;
    }
  return buf;
}
*/
/* c_type_id returns the arraytype to be used in parameter declaration
   Allows you to specify the identifier that acts acts as teh name of teh
   type of array */
char *
c_type_id(Tnode *typ, char *ident)
{
  char *p,*q,tempBuf[10];
  Tnode *temp;
  Entry *e;

  if (!typ)
    return "NULL";
  
  switch(typ->type)
  {
  case Tnone:
    p = ident;
    break;
  case Tvoid :
    p = (char*)malloc(6+strlen(ident));
    strcpy(p, "void ");
    strcat(p, ident);
    break;
  case Tchar :
    p = (char*)malloc(6+strlen(ident));
    strcpy(p, "char ");
    strcat(p, ident);
    break;
  case Twchar :
    p = (char*)malloc(9+strlen(ident));
    strcpy(p, "wchar_t ");
    strcat(p, ident);
    break;
  case Tshort :
    p = (char*)malloc(7+strlen(ident));
    strcpy(p, "short ");
    strcat(p, ident);
    break;
  case Tint   :
    p = (char*)malloc(5+strlen(ident));
    strcpy(p, "int ");
    strcat(p, ident);
    break;
  case Tlong   :
    p = (char*)malloc(6+strlen(ident));
    strcpy(p, "long ");
    strcat(p, ident);
    break;
  case Tllong   :
    p = (char*)malloc(8+strlen(ident));
    strcpy(p, "LONG64 ");
    strcat(p, ident);
    break;
  case Tfloat :
    p = (char*)malloc(7+strlen(ident));
    strcpy(p, "float ");
    strcat(p, ident);
    break;
  case Tdouble:
    p = (char*)malloc(8+strlen(ident));
    strcpy(p, "double ");
    strcat(p, ident);
    break;
  case Tuchar :
    p = (char*)malloc(15+strlen(ident));
    strcpy(p, "unsigned char ");
    strcat(p, ident);
    break;
  case Tushort:
    p = (char*)malloc(16+strlen(ident));
    strcpy(p, "unsigned short ");
    strcat(p, ident);
    break;
  case Tuint :
    p = (char*)malloc(14+strlen(ident));
    strcpy(p, "unsigned int ");
    strcat(p, ident);
    break;
  case Tulong:
    p = (char*)malloc(15+strlen(ident));
    strcpy(p, "unsigned long ");
    strcat(p, ident);
    break;
  case Tullong:
    p = (char*)malloc(9+strlen(ident));
    strcpy(p, "ULONG64 ");
    strcat(p, ident);
    break;
  case Ttime:
    p = (char*)malloc(8+strlen(ident));
    strcpy(p, "time_t ");
    strcat(p, ident);
    break;
  case Tstruct:
    p=(char*) malloc((9+strlen(typ->id->name)+strlen(ident)) *sizeof(char));
    strcpy(p,"struct ");
    strcat(p,typ->id->name);
    strcat(p, " ");
    strcat(p,ident);
    break;
  case Tclass:
    p=(char*) malloc((2+strlen(typ->id->name)+strlen(ident)) *sizeof(char));
    strcpy(p, typ->id->name);
    strcat(p, " ");
    strcat(p, ident);
    break;
  case Tunion:
    p=(char*) malloc((8+strlen(typ->id->name)+strlen(ident)) *sizeof(char));
    strcpy(p,"union ");
    strcat(p,typ->id->name);
    strcat(p, " ");
    strcat(p, ident);
    break;
  case Tenum:
    if (typ->ref == booltable)
    { p = (char*)malloc((strlen(ident)+6)*sizeof(char));
      strcpy(p, "bool ");
      strcat(p, ident);
      return p;
    }
    p=(char*) malloc((7+strlen(typ->id->name)+strlen(ident)) *sizeof(char));
    strcpy(p, "enum ");
    strcat(p, typ->id->name);
    strcat(p, " ");
    strcat(p, ident);
    break;
  case Tpointer:
    p = (char*)malloc(strlen(ident)+2);
    strcpy(p+1, ident);
    p[0] = '*';
    p = c_type_id(typ->ref, p);
    break;
  case Treference:
    p = (char*)malloc(strlen(ident)+2);
    strcpy(p+1, ident);
    p[0] = '&';
    p = c_type_id(typ->ref, p);
    break;
  case Tarray :

    temp = typ;
    while(((Tnode*) (typ->ref))->type==Tarray){
      typ = typ->ref;
    }

    p=(char*) malloc((12+strlen(c_type_id(typ->ref, ident))) *sizeof(char));
    if (((Tnode*)typ->ref)->type == Tpointer)
      sprintf(p,"%s",c_type_id(typ->ref, ident));
    else
      strcpy(p, c_type_id(typ->ref, ident));
     
    typ = temp;
    
    while(typ->type==Tarray){
      sprintf(tempBuf,"[%d]",(typ->width / ((Tnode*) typ->ref)->width));
      strcat(p,tempBuf);
      typ = typ->ref;
    }

    
    /*if(((Tnode*) (typ->ref))->type==Tarray){
      sprintf(p,"%s [%d]",c_type(typ->ref),(typ->width / ((Tnode*) typ->ref)->width));
    }else
    sprintf(p,"%s a[%d]",c_type(typ->ref),(typ->width /((Tnode*) typ->ref)->width));*/
    break;

  case Tfun:
    if (strncmp(ident, "operator ", 9))
      q = c_type_id(((FNinfo*)typ->ref)->ret, ident);
    else
      q = ident;
    p = (char*)malloc(256*sizeof(char));
    strcpy(p, q);
    strcat(p, "(");
    for (e = ((FNinfo*)typ->ref)->args->list; e; e = e->next)
    { strcat(p, c_storage(e->info.sto));
      strcat(p, c_type_id(e->info.typ, e->sym->name));
      if (e->next)
        strcat(p, ", ");
    }
    strcat(p, ")");
    break;
  default : return "UnknownType";   
  }
  return p;
}

char *
xsi_type_Tarray(Tnode *typ)
{ Tnode *t;
  int cardinality;
  char *p, *s;
  t = typ->ref;
  cardinality = 1;
  while (t->type == Tarray || (is_dynamic_array(t) && !has_ns(t) && !is_untyped(typ)))
  { if( t->type == Tarray)
      t = t->ref;
    else
      t = ((Table*)t->ref)->list->info.typ->ref;
    cardinality++;
  }
  if (is_untyped(t))
    s = wsdl_type(t, "xsd");
  else
    s = xsi_type(t);
  if (!*s)
  p = (char*)malloc(strlen(s)+cardinality*7+1);
  strcpy(p, s);
  if (cardinality > 1)
  { strcat(p, "[");
    for (; cardinality > 2; cardinality--)
      strcat(p, ",");
    strcat(p, "]");
  }
  /*
  for (; cardinality; cardinality--)
  { t = typ;
    for (i = 1; i < cardinality; i++)
      t = t->ref;
    sprintf(temp,"[%d]",get_dimension(t));
    strcat(p, temp);
  }
  */
  return p;
}

char *
xsi_type_Darray(Tnode *typ)
{ Tnode *t;
  int cardinality;
  char *p, *s;
  if (!typ->ref)
    return "";
  t = ((Table*)typ->ref)->list->info.typ->ref;
  cardinality = 1;
  while (t->type == Tarray || (is_dynamic_array(t) && !has_ns(t) && !is_untyped(typ)))
  { if( t->type == Tarray)
      t = t->ref;
    else
      t = ((Table*)t->ref)->list->info.typ->ref;
    cardinality++;
  }
  if (is_untyped(t))
    s = wsdl_type(t, "xsd");
  else
    s = xsi_type(t);
  p = (char*)malloc(strlen(s)+cardinality*2+1);
  strcpy(p, s);
  if (cardinality > 1)
  { strcat(p, "[");
    for (; cardinality > 2; cardinality--)
      strcat(p, ",");
    strcat(p, "]");
  }
  return p;
}

void
xml_out_generate(typ)
Tnode *typ;
{
	if (is_transient(typ) || typ->type == Twchar || is_XML(typ))
	  return;
	if (is_primitive(typ) || is_string(typ) || is_wstring(typ))	
	{	      typeNO++;
	 		if (typeNO>=1024)
			execerror("Too many user-defined data types");	
	              fprintf(fhead,"\n\n#ifndef SOAP_%s",c_ident(typ));	
		      fprintf(fhead,"\n#define SOAP_%s (%d)",c_ident(typ),typeNO);	
		      fprintf(fhead,"\n#endif");	
			defaults(typ);
			serialize(typ);
			mark(typ);
			soap_put(typ);
			soap_out(typ);
			soap_get(typ);
			soap_in(typ);
	  return;
	}
        switch(typ->type)
        {
	case Tenum:
        case Tpointer:
        case Tarray :
        case Tstruct:
	 case Tclass: typeNO++;
	 		if (typeNO>=1024)
			execerror("Too many user-defined data types");	
		      fprintf(fhead,"\n\n#ifndef SOAP_%s",c_ident(typ));	
		      fprintf(fhead,"\n#define SOAP_%s (%d)",c_ident(typ),typeNO);	
		      fprintf(fhead,"\n#endif");	
                      serialize(typ);  
                      mark(typ);
		      defaults(typ);	
		      soap_put(typ);
		      soap_out(typ);
		      soap_get(typ);	
		      soap_in(typ);
		      if (typ->type == Tclass)
		        soap_instan_class(typ);
                      break;
              default:break;
         }
}

/*his function is called first it first generates all routines
  and then in the second pass calls all routines to generate
  xml_out for the table*/                                   
void
xml_def_table(table,typ)  
Table *table;  
Tnode *typ; 
{ 
  int i;  
  Tnode *p; 
  for(i=0;i<TYPES;i++)
    for(p=Tptr[i];p!=(Tnode*) 0;p=p->next)
	xml_out_generate(p);
}

         
int no_of_var(typ)
Tnode * typ;
{
  Entry *p;
  Table *t;
  int i=0;
  if(typ->type==Tstruct || typ->type==Tclass)
    {
      t=typ->ref;
      for (p = t->list; p != (Entry*) 0; p = p->next) {
	if(p->info.typ->type==Tpointer)
	  i++;
      }
    }
  if((((Tnode *)(typ->ref))->type==Tstruct) ||
     (((Tnode *)(typ->ref))->type==Tclass) )
    {
      t=((Tnode*)(typ->ref))->ref;
      for (p = t->list; p != (Entry*) 0; p = p->next) {
	if(p->info.typ->type==Tpointer)
	  i++;
      }
    }
  return i;
}      

void
in_defs(Table *table)
{
  int i;  
  Tnode *p;
  for (i=0;i<TYPES;i++)
  { for(p=Tptr[i];p!=(Tnode*) 0;p=p->next)
    { if (!is_transient(p) && p->type != Twchar && p->type != Tfun && p->type != Treference && !is_XML(p))
      { fprintf(fout,"\n\t\tcase SOAP_%s:\n\t\t\tsoap_in_%s(soap, NULL, NULL, \"%s\");\n\t\t\tbreak;",c_ident(p), c_ident(p), xsi_type(p));
      }
    }
  }
}

void
in_defs2(Table *table)
{ int i;
  Tnode *p;
  char *s;
  int a=0;
  for(i=0;i<TYPES;i++){
    DBGLOG(fprintf(stderr,"\n A %d %d A", i, TYPES));
    for(p=Tptr[i];p!=(Tnode*) 0;p=p->next){
      DBGLOG(fprintf(stderr,"\n B  B "));

      if (!is_transient(p) && p->type != Twchar && p->type != Tfun && p->type != Tpointer && p->type != Treference && !is_XML(p) || is_string(p) && !is_XML(p))
      { if(a==0) 
	  { DBGLOG(fprintf(stderr,"\n C  C 0 %s ", c_ident(p)));
	      s = xsi_type(p);
	      if (*s)
	      { fprintf(fout,"\n\t\t\tsoap_in_%s(soap, NULL, NULL, \"%s\");",c_ident(p),s);
	        a=1;
	      }
	  }
	else
	  { DBGLOG(fprintf(stderr,"\n D D  %s", c_ident(p)));
	      s = xsi_type(p);
	      if (*s)
	      fprintf(fout,"\n\t\t\tif (soap->error == SOAP_TYPE_MISMATCH && soap->level == 2)\n\t\t\t\tsoap_in_%s(soap, NULL, NULL, \"%s\");", c_ident(p),s);
	  }
      }
    }
  }
  fprintf(fout,"\n\t\t\tif (soap->error)\n\t\t\t{\tsoap->peeked = 1;\n\t\t\t\tsoap_ignore_element(soap);\n\t\t\t}");
}

void
out_defs(Table *table)
{ int i;  
  Tnode *p;
  for (i = 0; i < TYPES; i++)
  { for (p = Tptr[i]; p; p = p->next)
    { DBGLOG(fprintf(stderr,"\n B  B "));
      if (is_transient(p) || is_primclass(p) || is_XML(p))
        continue;
      if (p->type == Tarray)
      { fprintf(fout,"\n\t\t\t\tcase SOAP_%s:\n\t\t\t\t\tsoap_out_%s(soap, \"id\", pp->id, (%s)pp->ptr, \"%s\");\n\t\t\t\t\tbreak;",c_ident(p), c_ident(p),c_type_id(p->ref, "(*)"), xsi_type(p));
      }
      else if(p->type == Tclass)
      { fprintf(fout,"\n\t\t\t\tcase SOAP_%s:\n\t\t\t\t\t((%s)pp->ptr)->soap_out(soap, \"id\", pp->id, \"%s\");\n\t\t\t\t\tbreak;",c_ident(p), c_type_id(p, "*"), xsi_type(p));
      }
      else if(p->type != Tnone && p->type != Twchar && !is_void(p) && p->type != Tfun && p->type != Tpointer && p->type != Treference && p->type != Tunion)
      { fprintf(fout,"\n\t\t\t\tcase SOAP_%s:\n\t\t\t\t\tsoap_out_%s(soap, \"id\", pp->id, (const %s)pp->ptr, \"%s\");\n\t\t\t\t\tbreak;",c_ident(p), c_ident(p),c_type_id(p, "*"), xsi_type(p));
      }
      else if (is_string(p))
      { fprintf(fout,"\n\t\t\t\tcase SOAP_%s:\n\t\t\t\t\tsoap_out_string(soap, \"id\", pp->id, (char**)&pp->ptr, \"%s\");\n\t\t\t\t\tbreak;",c_ident(p), xsi_type(p));
      }
      else if (is_wstring(p))
        fprintf(fout,"\n\t\t\t\tcase SOAP_%s:\n\t\t\t\t\tsoap_out_wstring(soap, \"%s\", pp->id, (wchar_t**)&pp->ptr, \"%s\");\n\t\t\t\t\tbreak;",c_ident(p), xml_tag(p),xsi_type(p));
    }
  }
}

void
in_attach(Table *table)
{ int i;  
  Tnode *p;
  for (i = 0; i < TYPES; i++)
    for (p = Tptr[i]; p; p = p->next)
      if (is_attachment(p))
        if (p->type == Tclass)
	  fprintf(fout,"\n\t\tcase SOAP_%s:\n\t\t{\t%s a;\n\t\t\ta = (%s)soap_class_id_enter(soap, soap->dime_id, NULL, SOAP_%s, NULL, NULL);\n\t\t\tif (a)\n\t\t\t{\ta->__ptr = (unsigned char*)soap->dime_ptr;\n\t\t\t\ta->__size = soap->dime_size;\n\t\t\t\ta->id = soap->dime_id;\n\t\t\t\ta->type = soap->dime_type;\n\t\t\t\ta->options = soap->dime_options;\n\t\t\t}\n\t\t\tbreak;\n\t\t}", c_ident(p), c_type_id(p, "*"), c_type_id(p, "*"), c_ident(p));
	else
	  fprintf(fout,"\n\t\tcase SOAP_%s:\n\t\t{\t%s a;\n\t\t\ta = (%s)soap_id_enter(soap, soap->dime_id, NULL, SOAP_%s, sizeof(%s), 0);\n\t\t\tif (a)\n\t\t\t{\ta->__ptr = (unsigned char*)soap->dime_ptr;\n\t\t\t\ta->__size = soap->dime_size;\n\t\t\t\ta->id = soap->dime_id;\n\t\t\t\ta->type = soap->dime_type;\n\t\t\t\ta->options = soap->dime_options;\n\t\t\t}\n\t\t\tbreak;\n\t\t}", c_ident(p), c_type_id(p, "*"), c_type_id(p, "*"), c_ident(p), c_type(p));
      else if (is_binary(p))
        if (p->type == Tclass)
	  fprintf(fout,"\n\t\tcase SOAP_%s:\n\t\t{\t%s a;\n\t\t\ta = (%s)soap_class_id_enter(soap, soap->dime_id, NULL, SOAP_%s, NULL, NULL);\n\t\t\tif (a)\n\t\t\t{\ta->__ptr = (unsigned char*)soap->dime_ptr;\n\t\t\t\ta->__size = soap->dime_size;\n\t\t\t}\n\t\t\tbreak;\n\t\t}", c_ident(p), c_type_id(p, "*"), c_type_id(p, "*"), c_ident(p));
	else
	  fprintf(fout,"\n\t\tcase SOAP_%s:\n\t\t{\t%s a;\n\t\t\ta = (%s)soap_id_enter(soap, soap->dime_id, NULL, SOAP_%s, sizeof(%s), 0);\n\t\t\tif (a)\n\t\t\t{\ta->__ptr = (unsigned char*)soap->dime_ptr;\n\t\t\t\ta->__size = soap->dime_size;\n\t\t\t}\n\t\t\tbreak;\n\t\t}", c_ident(p), c_type_id(p, "*"), c_type_id(p, "*"), c_ident(p), c_type(p));
      else if (is_string(p) && !is_XML(p))
	fprintf(fout,"\n\t\tcase SOAP_%s:\n\t\t{\t%s a;\n\t\t\ta = (%s)soap_id_enter(soap, soap->dime_id, NULL, SOAP_%s, sizeof(%s), 0);\n\t\t\tif (a)\n\t\t\t\t*a = soap->dime_ptr;\n\t\t\tbreak;\n\t\t}", c_ident(p), c_type_id(p, "*"), c_type_id(p, "*"), c_ident(p), c_type(p));
}

void
out_attach(Table *table)
{ int i;  
  char *s;
  Tnode *p;
  for (i = 0; i < TYPES; i++)
    for (p = Tptr[i]; p; p = p->next)
      if (is_attachment(p))
      { s = c_type_id(p, "*");
        fprintf(fout,"\n\t\t\t\tcase SOAP_%s:\n\t\t\t\t\tsoap_putdime(soap, pp->id, ((%s)pp->ptr)->id, ((%s)pp->ptr)->type, ((%s)pp->ptr)->options, ((%s)pp->ptr)->__ptr, ((%s)pp->ptr)->__size);\n\t\t\t\t\tbreak;", c_ident(p), s, s, s, s, s);
      }
}

void
soap_instan_class(Tnode *typ)
{ Table *Tptr;
  Entry *Eptr;
  int derclass = 0;
  char *s;
  
  fprintf(fhead,"\n\n#define soap_new_%s(soap, n) soap_instantiate_%s(soap, n, NULL, NULL)", c_ident(typ), c_ident(typ));
  fprintf(fhead,"\n\nSOAP_FMAC1 %s * SOAP_FMAC2 soap_instantiate_%s(struct soap*, int, const char*, const char*);", c_type(typ), c_ident(typ));
  fprintf(fout,"\n\nSOAP_FMAC1 %s * SOAP_FMAC2 soap_instantiate_%s(struct soap *soap, int n, const char *type, const char *arrayType)", c_type(typ), c_ident(typ));
  fprintf(fout,"\n{");
  fprintf(fout, "\nDBGLOG(TEST, SOAP_MESSAGE(fdebug, \"\\nsoap_instantiate_%s(%%d, %%s, %%s)\", n, type?type:\"\", arrayType?arrayType:\"\"));", c_type(typ));
  fprintf(fout,"\n\tsoap->alloced = 1;");

  fprintf(fout,"\n\tstruct soap_clist *cp = (struct soap_clist*)malloc(sizeof(struct soap_clist));");
  fprintf(fout,"\n\tif (!cp)\n\t\treturn NULL;");
  for (Eptr = classtable->list; Eptr; Eptr = Eptr->next)
  {
    Tptr = ((Table *) Eptr->info.typ->ref);
    if(Tptr == ((Table *) typ->ref)){
      continue;
    }
    
    derclass = 0;
    while(Tptr)
    {
      if(Tptr == typ->ref){
	derclass = 1;
      }

      Tptr = Tptr->prev;
    }

    if(derclass == 1 && !is_transient(Eptr->info.typ)){
      if (is_dynamic_array(Eptr->info.typ) && !is_binary(Eptr->info.typ) && !has_ns(Eptr->info.typ) && !is_untyped(Eptr->info.typ))
        fprintf(fout,"\n\tif (arrayType && !soap_match_tag(soap, arrayType, \"%s\"))", xsi_type(Eptr->info.typ));
      else
        fprintf(fout,"\n\tif (type && !soap_match_tag(soap, type, \"%s\"))", the_type(Eptr->info.typ));
      fprintf(fout,"\n\t{\tcp->next = soap->clist;");
      fprintf(fout,"\n\t\tcp->type = SOAP_%s;", c_ident(Eptr->info.typ));
      fprintf(fout,"\n\t\tcp->size = n;");
      fprintf(fout,"\n\t\tsoap->clist = cp;");
      fprintf(fout,"\n\t\tif (n < 0)");
      fprintf(fout,"\n\t\t{\tcp->ptr = (void*)new %s;", c_type(Eptr->info.typ));
      if ((s = has_soapref(Eptr->info.typ)))
        fprintf(fout,"\n\t\t\t((%s*)cp->ptr)->%s = soap;", c_type(Eptr->info.typ), s);
      fprintf(fout,"\n\t\t}\n\t\telse");
      fprintf(fout,"\n\t\t{\tcp->ptr = (void*)new %s[n];", c_type(Eptr->info.typ));
      if (s)
        fprintf(fout,"\n\t\t\tfor (int i = 0; i < n; i++)\n\t\t\t\t((%s*)cp->ptr)[i].%s = soap;", c_type(Eptr->info.typ), s);
      fprintf(fout,"\n\t\t}\n\t\treturn (%s*)cp->ptr;", c_type(Eptr->info.typ));
      fprintf(fout,"\n\t}");

      derclass = 0;
    }
  }

      fprintf(fout,"\n\tcp->next = soap->clist;");
      fprintf(fout,"\n\tcp->type = SOAP_%s;", c_ident(typ));
      fprintf(fout,"\n\tcp->size = n; ");
      fprintf(fout,"\n\tsoap->clist = cp;");
      fprintf(fout,"\n\tif (n < 0)");
      fprintf(fout,"\n\t{\tcp->ptr = (void*)new %s;", c_type(typ));
      if ((s = has_soapref(typ)))
        fprintf(fout,"\n\t\t((%s*)cp->ptr)->%s = soap;", c_type(typ), s);
      fprintf(fout,"\n\t}\n\telse");
      fprintf(fout,"\n\t{\tcp->ptr = (void*)new %s[n];", c_type(typ));
      if (s)
        fprintf(fout,"\n\t\tfor (int i = 0; i < n; i++)\n\t\t\t((%s*)cp->ptr)[i].%s = soap;", c_type(typ), s);
      fprintf(fout,"\n\t}\n\treturn (%s*)cp->ptr;", c_type(typ));
  
  fprintf(fout,"\n}");
}


void
serialize(Tnode *typ) 
{ if(typ->type==Tarray){
    /* ARRAY */
    fprintf(fhead,"\nSOAP_FMAC1 void SOAP_FMAC2 soap_serialize_%s(struct soap*, %s);", c_ident(typ), c_type_id(typ, "const"));
    fprintf(fout,"\n\nSOAP_FMAC1 void SOAP_FMAC2 soap_serialize_%s(struct soap *soap, %s)", c_ident(typ), c_type_id(typ, "const a"));
    fprintf(fout,"\n{\tif (!soap_reference(soap, a, SOAP_%s))",c_ident(typ));
    fprintf(fout,"\n\t\tsoap_mark_%s(soap, a);\n}",c_ident(typ));
      fflush(fout);
    return;   
  }
  if (is_dynamic_array(typ))
    if (typ->type == Tclass)
    { fprintf(fout,"\n\nvoid %s::soap_serialize(struct soap *soap) const\n{",c_ident(typ));
      fprintf(fout,"\n\tthis->soap_mark(soap);\n}");
      fflush(fout);
      return;
    }
    else
    { fprintf(fhead,"\nSOAP_FMAC1 void SOAP_FMAC2 soap_serialize_%s(struct soap*, %s);",c_ident(typ),c_type_id(typ, "const*")); 
      fprintf(fout,"\n\nSOAP_FMAC1 void SOAP_FMAC2 soap_serialize_%s(struct soap *soap, %s)\n{",c_ident(typ),c_type_id(typ, "const*a")); 
      fprintf(fout,"\n\tsoap_mark_%s(soap, a);\n}", c_ident(typ));
      fflush(fout);
      return;
    }
  if(typ->type==Tclass){
    fprintf(fout,"\n\nvoid %s::soap_serialize(struct soap *soap) const\n{", typ->id->name);
    if (!is_primclass(typ))
      fprintf(fout,"\n\tif (!soap_reference(soap, this, SOAP_%s))",c_ident(typ));
    fprintf(fout,"\n\t\tthis->soap_mark(soap);\n}",c_ident(typ));
      fflush(fout);
    return;
  }

  fprintf(fhead,"\nSOAP_FMAC1 void SOAP_FMAC2 soap_serialize_%s(struct soap*, %s);",c_ident(typ),c_type_id(typ, "const*"));
  fprintf(fout,"\n\nSOAP_FMAC1 void SOAP_FMAC2 soap_serialize_%s(struct soap *soap, %s)\n{", c_ident(typ),c_type_id(typ, "const*a"));
  if (is_primitive(typ))
    fprintf(fout,"\n\tsoap_reference(soap, a, SOAP_%s);\n}",c_ident(typ));
  else if (is_string(typ) || is_wstring(typ))
  { fprintf(fout,"\n\tsoap_reference(soap, *a, SOAP_%s);\n}",c_ident(typ));
  }
  else
  { if (!is_primclass(typ))
      fprintf(fout,"\n\tif (!soap_reference(soap, a, SOAP_%s))",c_ident(typ));
    fprintf(fout,"\n\t\tsoap_mark_%s(soap, a);\n}",c_ident(typ));
  }
}

int
get_dimension(Tnode *typ)
{ return typ->width / ((Tnode*) typ->ref)->width;
}


void
mark(Tnode *typ)
{ int d;
  Table *table,*t;
  Entry *p;
  Tnode* temp;
  int cardinality;

    if (p = is_dynamic_array(typ))
    { if (typ->transient < 0)
        return;
      if (typ->type == Tclass)
      { fprintf(fout,"\n\nvoid %s::soap_mark(struct soap *soap) const\n{",c_ident(typ));
        if (is_binary(typ))
	{
            fprintf(fout,"\n\tif (this->__ptr)\n\t\tsoap_array_reference(soap, &this->%s, this->__size, SOAP_%s);\n}", p->sym->name, c_ident(typ));
      fflush(fout);
      return;
	}
	else
	{
      fprintf(fout,"\n\tint i;");
      d = get_Darraydims(typ);
      if (d)
      { fprintf(fout,"\n\tif (this->%s && !soap_array_reference(soap, &thisi->%s, soap_size(this->__size, %d), SOAP_%s))", p->sym->name, p->sym->name, d, c_ident(typ));
        fprintf(fout,"\n\t\tfor (i = 0; i < soap_size(this->__size, %d); i++)", d);
      }
      else
      { fprintf(fout,"\n\tif (this->%s && !soap_array_reference(soap, &this->%s, this->__size, SOAP_%s))", p->sym->name, p->sym->name, c_ident(typ));
        fprintf(fout,"\n\t\tfor (i = 0; i < this->__size; i++)");
      }
      fprintf(fout,"\n\t\t{\tsoap_embedded(soap, this->%s + i, SOAP_%s);", p->sym->name, c_ident(p->info.typ->ref));
      if (((Tnode*)p->info.typ->ref)->type == Tclass)
        fprintf(fout,"\n\t\t\tthis->%s[i].soap_mark(soap);", p->sym->name);
      else if (!is_primitive(p->info.typ->ref))
        fprintf(fout,"\n\t\t\tsoap_mark_%s(soap, this->%s + i);", c_ident(p->info.typ->ref), p->sym->name);
      fprintf(fout,"\n\t\t}\n}");
      return;
      }
      }
      else
      { fprintf(fhead,"\nSOAP_FMAC1 void SOAP_FMAC2 soap_mark_%s(struct soap*, %s);",c_ident(typ),c_type_id(typ, "const*")); 
        fprintf(fout,"\n\nSOAP_FMAC1 void SOAP_FMAC2 soap_mark_%s(struct soap *soap, %s)\n{",c_ident(typ),c_type_id(typ, "const*a")); 
        if (is_binary(typ))
	{
          fprintf(fout,"\n\tif (a->__ptr)\n\t\tsoap_array_reference(soap, &a->__ptr, a->__size, SOAP_%s);\n}", c_ident(typ));
      fflush(fout);
      return;
	}
	else
	{
      fprintf(fout,"\n\tint i;");
      d = get_Darraydims(typ);
      if (d)
      { fprintf(fout,"\n\tif (a->%s && !soap_array_reference(soap, &a->%s, soap_size(a->__size, %d), SOAP_%s))", p->sym->name, p->sym->name, d, c_ident(typ));
        fprintf(fout,"\n\t\tfor (i = 0; i < soap_size(a->__size, %d); i++)", d);
      }
      else
      { fprintf(fout,"\n\tif (a->%s && !soap_array_reference(soap, &a->%s, a->__size, SOAP_%s))", p->sym->name, p->sym->name, c_ident(typ));
        fprintf(fout,"\n\t\tfor (i = 0; i < a->__size; i++)");
      }
      fprintf(fout,"\n\t\t{\tsoap_embedded(soap, a->%s + i, SOAP_%s);", p->sym->name, c_ident(p->info.typ->ref));
      if (((Tnode*)p->info.typ->ref)->type == Tclass)
        fprintf(fout,"\n\t\t\ta->%s[i].soap_mark(soap);", p->sym->name);
      else if (!is_primitive(p->info.typ->ref))
        fprintf(fout,"\n\t\t\tsoap_mark_%s(soap, a->%s + i);", c_ident(p->info.typ->ref), p->sym->name);
      fprintf(fout,"\n\t\t}\n}");
      fflush(fout);
      return;
      }
      }
      }
  switch(typ->type)
    {	
    case Tstruct:

      if (!typ->ref)
        return;
      fprintf(fhead,"\nSOAP_FMAC1 void SOAP_FMAC2 soap_mark_%s(struct soap*, const %s);",c_ident(typ),c_type_id(typ, "*")); 
      if (typ->transient < 0)
        return;
      fprintf(fout,"\n\nSOAP_FMAC1 void SOAP_FMAC2 soap_mark_%s(struct soap *soap, const %s)\n{",c_ident(typ),c_type_id(typ, "*a")); 
      /* DYNAMIC ARRAY */
      
      table=(Table*)typ->ref;
      for (t = table; t != (Table *) 0; t = t->prev) { 
	for (p = t->list; p != (Entry*) 0; p = p->next) {
	  if (p->info.sto & Sconst)
	    fprintf(fout, "\n\t/* const %s skipped */", p->sym->name);
	  else
	  if (is_transient(p->info.typ))
	    fprintf(fout, "\n\t/* transient %s skipped */", p->sym->name);
	  else if (is_repetition(p))
	  { 
      fprintf(fout,"\n\tif (a->%s)", p->next->sym->name);
      fprintf(fout,"\n\t{\tint i;\n\t\tfor (i = 0; i < a->%s; i++)", p->sym->name);
      fprintf(fout,"\n\t\t{\tsoap_embedded(soap, a->%s + i, SOAP_%s);", p->next->sym->name, c_ident(p->next->info.typ->ref));
      if (((Tnode*)p->next->info.typ->ref)->type == Tclass)
        fprintf(fout,"\n\t\t\ta->%s[i].soap_mark(soap);", p->next->sym->name);
      else if (!is_primitive(p->next->info.typ->ref))
        fprintf(fout,"\n\t\t\tsoap_mark_%s(soap, a->%s + i);", c_ident(p->next->info.typ->ref), p->next->sym->name);
      fprintf(fout,"\n\t\t}\n\t}");
          p = p->next;
	  }
	  else if(p->info.typ->type==Tarray)
	    {
	      fprintf(fout,"\n\tsoap_embedded(soap, a->%s, SOAP_%s);", p->sym->name,c_ident(p->info.typ));
	      fprintf(fout,"\n\tsoap_mark_%s(soap, a->%s);", c_ident(p->info.typ),p->sym->name);
	    }
	  else if(p->info.typ->type==Tclass)
	    {
	      /* CLASS ( within STRUCT ) */
	      fprintf(fout,"\n\tsoap_embedded(soap, &a->%s, SOAP_%s);", p->sym->name,c_ident(p->info.typ));
	      fprintf(fout,"\n\ta->%s.soap_mark(soap);",p->sym->name);
	    }
	  else if (p->info.typ->type != Tfun && !is_void(p->info.typ) && !is_XML(p->info.typ))
	    {
	      fprintf(fout,"\n\tsoap_embedded(soap, &a->%s, SOAP_%s);", p->sym->name,c_ident(p->info.typ));
	      if (!is_primitive(p->info.typ))
	        fprintf(fout,"\n\tsoap_mark_%s(soap, &a->%s);", c_ident(p->info.typ),p->sym->name);
	    }
	}
      }
      fprintf(fout,"\n}");	 
      break;
      
    case Tclass:
      /* CLASS */
      if (typ->transient < 0)
        return;
      table=(Table*)typ->ref;
      /*      fprintf(fhead,"\nvirtual void %s::soap_mark();",
	      ((Table *) typ->ref)->sym->name ,
	      c_ident(typ),c_type(typ) ); */
      fprintf(fout,"\n\nvoid %s::soap_mark(struct soap *soap) const\n{", typ->id->name); 
      for (t = table; t != (Table *) 0; t = t->prev) { 
	for (p = t->list; p != (Entry*) 0; p = p->next) {
	  if (p->info.sto & Sconst)
	    fprintf(fout, "\n\t/* const %s skipped */", p->sym->name);
	  else
	  if (is_transient(p->info.typ))
	    fprintf(fout, "\n\t/* transient %s skipped */", p->sym->name);
	  else if (is_repetition(p))
	  { 
      fprintf(fout,"\n\tif (((%s*)this)->%s)", t->sym->name, p->next->sym->name);
      fprintf(fout,"\n\t{\tint i;\n\t\tfor (i = 0; i < ((%s*)this)->%s; i++)", t->sym->name, p->sym->name);
      fprintf(fout,"\n\t\t{\tsoap_embedded(soap, ((%s*)this)->%s + i, SOAP_%s);", t->sym->name, p->next->sym->name, c_ident(p->next->info.typ->ref));
      if (((Tnode*)p->next->info.typ->ref)->type == Tclass)
        fprintf(fout,"\n\t\t\t((%s*)this)->%s[i].soap_mark(soap);", t->sym->name, p->next->sym->name);
      else if (!is_primitive(p->next->info.typ->ref))
        fprintf(fout,"\n\t\t\tsoap_mark_%s(soap, ((%s*)this)->%s + i);", c_ident(p->next->info.typ->ref), t->sym->name, p->next->sym->name);
      fprintf(fout,"\n\t\t}\n\t}");
          p = p->next;
	  }
	  else if(p->info.typ->type==Tarray)
	    {
	      /* ARRAY */
	      fprintf(fout,"\n\tsoap_embedded(soap, ((%s*)this)->%s, SOAP_%s);", t->sym->name, p->sym->name,c_ident(p->info.typ));
	      fprintf(fout,"\n\tsoap_mark_%s(soap, ((%s*)this)->%s);", c_ident(p->info.typ),t->sym->name, p->sym->name);
	    }
	  else if(p->info.typ->type==Tclass)
	    {
	      /* CLASS ( within CLASS ) */
	      fprintf(fout,"\n\tsoap_embedded(soap, &((%s*)this)->%s, SOAP_%s);", t->sym->name, p->sym->name, c_ident(p->info.typ));
	      fprintf(fout,"\n\t((%s*)this)->%s.soap_mark(soap);", t->sym->name, p->sym->name );
	    }
	  else if (p->info.typ->type != Tfun && !is_void(p->info.typ) && !is_XML(p->info.typ)) {
	    fprintf(fout,"\n\tsoap_embedded(soap, &((%s*)this)->%s, SOAP_%s);", t->sym->name,p->sym->name,c_ident(p->info.typ));
	    if (!is_primitive(p->info.typ))
	      fprintf(fout,"\n\tsoap_mark_%s(soap, &((%s*)this)->%s);", c_ident(p->info.typ),t->sym->name,p->sym->name);
	  }
	}
      }
      fprintf(fout,"\n}");	 
      break;
    case Tpointer:
      if(is_string(typ) || is_wstring(typ)) 
	{ fprintf(fhead,"\nSOAP_FMAC1 void SOAP_FMAC2 soap_mark_%s(struct soap*, %s);", c_ident(typ),c_type_id(typ, "const*"));
      if (typ->transient < 0)
        return;
	fprintf(fout,"\n\nSOAP_FMAC1 void SOAP_FMAC2 soap_mark_%s(struct soap *soap, %s)\n{", c_ident(typ),c_type_id(typ, "const*a"));
	fprintf(fout,"\n\tsoap_reference(soap, *a, SOAP_%s);\n}", c_ident(typ));
	return;
	}

      /* If pointer points to a CLASS */
      if(((Tnode *) typ->ref)->type == Tclass){
	fprintf(fhead,"\nSOAP_FMAC1 void SOAP_FMAC2 soap_mark_%s(struct soap*, %s);", c_ident(typ),c_type_id(typ, "const*"));
      if (typ->transient < 0)
        return;
	fprintf(fout,"\n\nSOAP_FMAC1 void SOAP_FMAC2 soap_mark_%s(struct soap *soap, %s)\n{", c_ident(typ),c_type_id(typ, "const*a"));
	fprintf(fout,"\n\tif (!soap_reference(soap, *a, SOAP_%s))", c_ident(typ->ref));
	fprintf(fout,"\n\t\t(*a)->soap_mark(soap);\n}");
	break;	
      }
      else{
	fprintf(fhead,"\nSOAP_FMAC1 void SOAP_FMAC2 soap_mark_%s(struct soap*, %s);", c_ident(typ),c_type_id(typ, "const*"));
      if (typ->transient < 0)
        return;
	
	fprintf(fout,"\n\nSOAP_FMAC1 void SOAP_FMAC2 soap_mark_%s(struct soap *soap, %s)\n{", c_ident(typ),c_type_id(typ, "const*a"));
	if (is_primitive(typ->ref))
	  fprintf(fout,"\n\tsoap_reference(soap, *a, SOAP_%s);\n}", c_ident(typ->ref));
	else
	{ fprintf(fout,"\n\tif (!soap_reference(soap, *a, SOAP_%s))", c_ident(typ->ref));
	  fprintf(fout,"\n\t\tsoap_mark_%s(soap, *a);\n}", c_ident(typ->ref));
	}
	break;
      }
	
    case Tarray :
      fprintf(fhead,"\nSOAP_FMAC1 void SOAP_FMAC2 soap_mark_%s(struct soap*, %s);", c_ident(typ),c_type_id(typ, "const"));
      if (typ->transient < 0)
        return;
      fprintf(fout,"\n\nSOAP_FMAC1 void SOAP_FMAC2 soap_mark_%s(struct soap *soap, %s)", c_ident(typ),c_type_id(typ, "const a"));
      if (is_primitive(typ->ref))
        fprintf(fout, "\n{");
      else
      { fprintf(fout,"\n{\tint i;");
        fprintf(fout,"\n\tfor(i = 0; i < %d; i++)", get_dimension(typ));
	
        temp=typ->ref;;
        cardinality = 1;
        while(temp->type==Tarray)
	{
	  temp=temp->ref;
	  cardinality++;
	}
        fprintf(fout,"\n\t{\tsoap_embedded(soap, a", c_ident(typ->ref));
        if(cardinality > 1){
	  fprintf(fout,"[i]");
        }else {
	  fprintf(fout,"+i");
        }
      
        fprintf(fout,", SOAP_%s);",c_ident(typ->ref));

	if (((Tnode *)typ->ref)->type == Tclass)
      	{	fprintf(fout,"\n\ta[i].soap_mark(soap)");
	}
	else if (!is_primitive(typ->ref))
      	{	fprintf(fout,"\n\tsoap_mark_%s(soap, a",c_ident(typ->ref));
      		if(cardinality > 1){
		fprintf(fout,"[i])");
      		}else {
	  	fprintf(fout,"+i)");
      		}
	}
        fprintf(fout,";\n\t}");
      }
      fprintf(fout,"\n}");
      break;	
    default:     break;
    }
}

void
defaults(typ)
Tnode* typ;
{ int i, d;
  Table *table,*t;
  Entry *p;
  Tnode *temp;
  char *s;
  int cardinality;
  if (p = is_dynamic_array(typ))
  {   if (typ->transient < 0)
        return;
    if (typ->type == Tclass)
      { fprintf(fout,"\n\nvoid %s::soap_default(struct soap *soap)\n{", c_ident(typ)); 
        fprintf(fout,"\n\tstatic %s a;\n\tmemcpy(this, &a, sizeof(%s));", c_ident(typ), c_ident(typ));
        if ((s = has_soapref(typ)))
          fprintf(fout,"\n\tthis->%s = soap;", s);
	d = get_Darraydims(typ);
        if (d)
	{ fprintf(fout,"\n\tthis->%s = NULL;", p->sym->name);
	  for (i = 0; i < d; i++)
	  { fprintf(fout,"\n\tthis->__size[%d] = 0;", i);
            if (has_offset(typ) && (((Table*)typ->ref)->list->next->next->info.sto & Sconst) == 0)
              fprintf(fout, "\n\tthis->__offset[%d] = 0;", i);
	  }
	}
	else
	{ fprintf(fout,"\n\tthis->__size = 0;\n\tthis->%s = NULL;", p->sym->name);
          if (has_offset(typ) && (((Table*)typ->ref)->list->next->next->info.sto & Sconst) == 0)
            fprintf(fout, "\n\tthis->__offset = 0;");
	}
	if (is_attachment(typ))
          fprintf(fout,"\n\tthis->id = NULL;\n\tthis->type = NULL;\n\tthis->options = NULL;");
        fprintf(fout,"\n}");
      }
      else
      { fprintf(fhead,"\nSOAP_FMAC1 void SOAP_FMAC2 soap_default_%s(struct soap*, %s);",c_ident(typ),c_type_id(typ, "*")); 
        fprintf(fout,"\n\nSOAP_FMAC1 void SOAP_FMAC2 soap_default_%s(struct soap *soap, %s)\n{", c_ident(typ),c_type_id(typ, "*a")); 
	d = get_Darraydims(typ);
        if (d)
	{ fprintf(fout,"\n\ta->%s = NULL;", p->sym->name);
	  for (i = 0; i < d; i++)
	  { fprintf(fout,"\n\ta->__size[%d] = 0;", i);
            if (has_offset(typ) && (((Table*)typ->ref)->list->next->next->info.sto & Sconst) == 0)
              fprintf(fout, "\n\ta->__offset[%d] = 0;", i);
	  }
	}
	else
	{ fprintf(fout,"\n\ta->__size = 0;\n\ta->%s = NULL;", p->sym->name);
          if (has_offset(typ) && (((Table*)typ->ref)->list->next->next->info.sto & Sconst) == 0)
            fprintf(fout, "\n\ta->__offset = 0;");
	}
	if (is_attachment(typ))
          fprintf(fout,"\n\ta->id = NULL;\n\ta->type = NULL;\n\ta->options = NULL;");
        fprintf(fout,"\n}");
      }
      fflush(fout);
      return;
  }
  if (is_primitive(typ))
  {   fprintf(fhead,"\nSOAP_FMAC1 void SOAP_FMAC2 soap_default_%s(struct soap*, %s);",c_ident(typ),c_type_id(typ, "*"));
      if (typ->transient < 0)
        return;
      fprintf(fout,"\n\nSOAP_FMAC1 void SOAP_FMAC2 soap_default_%s(struct soap *soap, %s)\n{\n#ifdef SOAP_DEFAULT_%s\n\t*a = SOAP_DEFAULT_%s;\n#else\n\t*a = (%s)0;\n#endif\n}",c_ident(typ),c_type_id(typ, "*a"), c_ident(typ), c_ident(typ), c_type(typ));
      return;
  }
  switch(typ->type)
    {
    case Tclass:
      /* CLASS */
      if (typ->transient < 0)
        return;
      table=(Table*)typ->ref;
      fprintf(fout,"\n\nvoid %s::soap_default(struct soap *soap)\n{", typ->id->name ); 
      fprintf(fout,"\n\tstatic %s a;\n\tmemcpy(this, &a, sizeof(%s));", typ->id->name, typ->id->name);
      if ((s = has_soapref(typ)))
        fprintf(fout,"\n\tthis->%s = soap;", s);
	
      /*      fprintf(fhead,"\nvirtual void %s::soap_default();",
	     ((Table *) typ->ref)->sym->name ); */
      for (t = table; t != (Table *) 0; t = t->prev) { 
	if(t==(Table *) 0 ) return;
	for (p = t->list; p != (Entry*) 0; p = p->next)
	  if (p->info.sto & Sconst)
	    fprintf(fout, "\n\t/* const %s skipped */", p->sym->name);
	  else if (is_transient(p->info.typ))
	    fprintf(fout, "\n\t/* transient %s skipped */", p->sym->name);
	  else if (is_repetition(p))
	  { fprintf(fout, "\n\t((%s*)this)->%s = 0;\n\t((%s*)this)->%s = NULL;", t->sym->name, p->sym->name, t->sym->name, p->next->sym->name);
	    p = p->next;
	  }
	  else
	  {
	  if(p->info.typ->type==Tarray){
	    fprintf(fout,"\n\tsoap_default_%s(soap, ((%s*)this)->%s);", c_ident(p->info.typ),t->sym->name,p->sym->name);
	  }
	  else if(p->info.typ->type==Tclass){
	    /* CLASS (within CLASS) */
	    fprintf(fout,"\n\t((%s*)this)->%s.%s::soap_default(soap);",t->sym->name, p->sym->name, c_ident(p->info.typ));
	  }
	  else if (p->info.hasval){
	    switch (p->info.typ->type)
	    { case Tfloat:
	      case Tdouble:
	        fprintf(fout,"\n\t((%s*)this)->%s = %f;", t->sym->name,p->sym->name,p->info.val.r);
	        break;
	      case Tpointer:
	        fprintf(fout,"\n\t((%s*)this)->%s = \"%s\";", t->sym->name,p->sym->name,p->info.val.s);
	        break;
	      default:
	        fprintf(fout,"\n\t((%s*)this)->%s = %lld;", t->sym->name,p->sym->name,p->info.val.i);
	    }
	  }
	  else if (p->info.typ->type != Tfun && !is_void(p->info.typ) && !is_XML(p->info.typ)) {
	    fprintf(fout,"\n\tsoap_default_%s(soap, &((%s*)this)->%s);", c_ident(p->info.typ),t->sym->name,p->sym->name);
	  }
	}
      }
      fprintf(fout,"\n}");	 
      break;
      
    case Tstruct:
      table=(Table*)typ->ref;

      fprintf(fhead,"\nSOAP_FMAC1 void SOAP_FMAC2 soap_default_%s(struct soap*, %s);",c_ident(typ),c_type_id(typ, "*")); 
      if (typ->transient < 0)
        return;
      fprintf(fout,"\n\nSOAP_FMAC1 void SOAP_FMAC2 soap_default_%s(struct soap *soap, %s)\n{", c_ident(typ),c_type_id(typ, "*a")); 
      /* DYNAMIC ARRAY */

      for (t = table; t != (Table *) 0; t = t->prev) { 
	if(t==(Table *) 0 ) return;
	for (p = t->list; p != (Entry*) 0; p = p->next)
	  if (p->info.sto & Sconst)
	    fprintf(fout, "\n\t/* const %s skipped */", p->sym->name);
	  else if (is_transient(p->info.typ))
	    fprintf(fout, "\n\t/* transient %s skipped */", p->sym->name);
	  else if (is_repetition(p))
	  { fprintf(fout, "\n\ta->%s = 0;\n\ta->%s = NULL;", p->sym->name, p->next->sym->name);
	    p = p->next;
	  }
	  else
	  {
	  if(p->info.typ->type==Tarray)
	    fprintf(fout,"\n\tsoap_default_%s(soap, a->%s);",
		    c_ident(p->info.typ),p->sym->name);
	  else if(p->info.typ->type==Tclass){
	    /* CLASS (within STRUCT) */
	    fprintf(fout,"\n\ta->%s.%s::soap_default(soap);",p->sym->name, c_ident(p->info.typ));
	  }
	  else if (p->info.hasval){
	    switch (p->info.typ->type)
	    { case Tfloat:
	      case Tdouble:
	        fprintf(fout,"\n\ta->%s = %f;", p->sym->name,p->info.val.r);
	        break;
	      case Tpointer:
	        fprintf(fout,"\n\ta->%s = \"%s\";", p->sym->name,p->info.val.s);
	        break;
	      default:
	        fprintf(fout,"\n\ta->%s = %lld;", p->sym->name,p->info.val.i);
	    }
	  }
	  else if (p->info.typ->type != Tfun && !is_void(p->info.typ) && !is_XML(p->info.typ))
	    fprintf(fout,"\n\tsoap_default_%s(soap, &a->%s);", c_ident(p->info.typ),p->sym->name);
	}
      }
      fprintf(fout,"\n}");	 
      break;
      
    case Tarray:
      fprintf(fhead,"\nSOAP_FMAC1 void SOAP_FMAC2 soap_default_%s(struct soap*, %s);",c_ident(typ),c_type(typ));
      if (typ->transient < 0)
        return;
      fprintf(fout,"\n\nSOAP_FMAC1 void SOAP_FMAC2 soap_default_%s(struct soap *soap, %s)\n{", c_ident(typ),c_type_id(typ, "a"));
      fprintf(fout,"\n\tint i;");
      fprintf(fout,"\n\tfor (i = 0; i < %d; i++)",get_dimension(typ));
      temp = typ->ref;
      cardinality = 1;
      while(temp->type==Tarray)
	{
	  temp=temp->ref;
	  cardinality++;
	}
	if (((Tnode *)typ->ref)->type == Tclass)
	{
      	if(cardinality>1){
		fprintf(fout,"a[i].%s::soap_default(soap)", c_ident(typ->ref));
     	 }else {
		fprintf(fout,"(a+i)->soap_default(soap)");
      		}
	}
      else
      {
      	fprintf(fout,"\n\tsoap_default_%s(soap, a",c_ident(typ->ref));
      	if(cardinality>1){
		fprintf(fout,"[i])");
     	 }else {
		fprintf(fout,"+i)");
      	}
      }
      fprintf(fout,";\n}");
      break;	
      
    case Tpointer:
      
      fprintf(fhead,"\nSOAP_FMAC1 void SOAP_FMAC2 soap_default_%s(struct soap*, %s);",c_ident(typ),c_type_id(typ, "*"));
      if (typ->transient < 0)
        return;
      fprintf(fout,"\n\nSOAP_FMAC1 void SOAP_FMAC2 soap_default_%s(struct soap *soap, %s)\n{\n\t*a = NULL;\n}",c_ident(typ),c_type_id(typ, "*a"));
      break;
      
    default    :break;
    }
  
}

void
soap_put(Tnode *typ)
{ int d;
  Entry *p;
  char *ci = c_ident(typ);
  char *ct = c_type(typ);
  char *cta = c_type_id(typ, "a");
  char *ctp = c_type_id(typ, "*");
  char *ctpa = c_type_id(typ, "*a");
  char *xt = xsi_type(typ);
  if(typ->type==Tarray)
    {
      fprintf(fhead,"\nSOAP_FMAC1 void SOAP_FMAC2 soap_put_%s(struct soap*, %s, const char*, const char*);", ci,ct);
      fprintf(fout,"\n\nSOAP_FMAC1 void SOAP_FMAC2 soap_put_%s(struct soap *soap, %s, const char *tag, const char *type)\n{", ci,cta);
    }
  else if(typ->type==Tclass)
    {
      fprintf(fout,"\n\nvoid %s::soap_put(struct soap *soap, const char *tag, const  char *type) const\n{", ct);
    }
  else
    {
      fprintf(fhead,"\nSOAP_FMAC1 void SOAP_FMAC2 soap_put_%s(struct soap*, %s, const char*, const char*);", ci,ctp);
      fprintf(fout,"\n\nSOAP_FMAC1 void SOAP_FMAC2 soap_put_%s(struct soap *soap, %s, const char *tag, const char *type)\n{", ci,ctpa);
    }
  
  fflush(fout);
  fprintf(fout,"\n\tint i;");
  fprintf(fout,"\n\tstruct soap_plist *pp;");
  if (p = is_dynamic_array(typ))
  { d = get_Darraydims(typ);
    if (typ->type == Tclass)
      if (d)
        fprintf(fout,"\n\tif ((i = soap_array_pointer_lookup(soap, &this->%s, soap_size(this->__size, %d), SOAP_%s, &pp)))", p->sym->name, d, ci);
      else
        fprintf(fout,"\n\tif ((i = soap_array_pointer_lookup(soap, &this->%s, this->__size, SOAP_%s, &pp)))", p->sym->name, ci);
    else if (d)
      fprintf(fout,"\n\tif ((i = soap_array_pointer_lookup(soap, &a->%s, soap_size(a->__size, %d), SOAP_%s, &pp)))", p->sym->name, d, ci);
    else
      fprintf(fout,"\n\tif ((i = soap_array_pointer_lookup(soap, &a->%s, a->__size, SOAP_%s, &pp)))", p->sym->name, ci);
  }
  else if (typ->type == Tclass)
    fprintf(fout,"\n\tif ((i = soap_pointer_lookup(soap, this, SOAP_%s, &pp)))", ci);
  else
    fprintf(fout,"\n\tif ((i = soap_pointer_lookup(soap, a, SOAP_%s, &pp)))", ci);
  fprintf(fout,"\n\t\tif (soap_is_embedded(soap, pp))");
  fprintf(fout,"\n\t\t\tsoap_element_ref(soap, tag, 0, i);");
  fprintf(fout,"\n\t\telse if (soap_is_single(soap, pp))");
  fflush(fout);
 
  if(typ->type == Tarray){
    /* ARRAY */
    fprintf(fout,"\n\t\t\tsoap_out_%s(soap, tag, 0, a, type);", ci);
  }
  else if(typ->type == Tclass){
    /* CLASS */
    fprintf(fout,"\n\t\t\tthis->soap_out(soap, tag, 0, type);");
  }
  else{
    fprintf(fout,"\n\t\t\tsoap_out_%s(soap, tag, 0, a, type);", ci);
  }
  
  fprintf(fout,"\n\t\telse\n\t\t{");
  fprintf(fout,"\n\t\t\tsoap_set_embedded(soap, pp);");   
  fflush(fout);
  if(typ->type  == Tarray){
    /* ARRAY */
    fprintf(fout,"\n\t\t\tsoap_out_%s(soap, tag, i, a, type);\n\t\t}", ci);
  }
  else if(typ->type == Tclass){
    /* CLASS */
    fprintf(fout,"\n\t\t\tthis->soap_out(soap, tag, i, type);\n\t\t}");
  }
  else{
    fprintf(fout,"\n\t\t\tsoap_out_%s(soap, tag, i, a, type);\n\t\t}", ci);
  }
  fflush(fout);
  
  if(typ->type == Tarray){
    /* ARRAY */
    fprintf(fout,"\n\telse\n\t\tsoap_out_%s(soap, tag, 0, a, type);", ci);
   
  }
  else if(typ->type == Tclass){
    /* CLASS */
    fprintf(fout,"\n\telse\n\t\tthis->soap_out(soap, tag, 0, type);");
  }
  else{
    fprintf(fout,"\n\telse\n\t\tsoap_out_%s(soap, tag, 0, a, type);", ci);
  }
  fprintf(fout,"\n\tsoap_putindependent(soap);\n}");
  fflush(fout);
}

Entry *
is_dynamic_array(Tnode *typ)
{ Entry *p;
  Table *t;
  if ((typ->type == Tstruct || typ->type == Tclass) && typ->ref)
  { for (t = (Table*)typ->ref; t; t = t->prev)
    { p = t->list;
      if (p && p->info.typ->type == Tpointer && !strncmp(p->sym->name, "__ptr", 5))
        if (p->next && (p->next->info.typ->type == Tint || p->next->info.typ->type == Tuint || p->next->info.typ->type == Tarray && (((Tnode*)p->next->info.typ->ref)->type == Tint || ((Tnode*)p->next->info.typ->ref)->type == Tuint)) && !strcmp(p->next->sym->name, "__size"))
	  return p;
    }
  }
  return 0;
}

int
get_Darraydims(Tnode *typ)
{ Entry *p;
  Table *t;
  if ((typ->type == Tstruct || typ->type == Tclass) && typ->ref)
  { for (t = (Table*)typ->ref; t; t = t->prev)
    { p = t->list;
      if (p && p->info.typ->type == Tpointer && !strncmp(p->sym->name, "__ptr", 5))
        if (p->next && p->next->info.typ->type == Tarray && (((Tnode*)p->next->info.typ->ref)->type == Tint || ((Tnode*)p->next->info.typ->ref)->type == Tuint) && !strcmp(p->next->sym->name, "__size"))
          return get_dimension(p->next->info.typ);
    }
  }
  return 0;
}

int
has_offset(Tnode *typ)
{ Entry *p;
  Table *t;
  if (typ->type == Tstruct || typ->type == Tclass)
  { for (t = (Table*)typ->ref; t; t = t->prev)
    { for (p = t->list; p; p = p->next)
      { if ((p->info.typ->type == Tint || p->info.typ->type == Tarray && ((Tnode*)p->info.typ->ref)->type == Tint) && !strcmp(p->sym->name, "__offset"))
          return 1;
      }
    }
  }
  return 0;
}

int
is_hexBinary(Tnode *typ)
{ Entry *p;
  Table *t;
  if ((typ->type == Tstruct || typ->type == Tclass) && !strcmp(typ->id->name, "xsd__hexBinary"))
  { for (t = (Table*)typ->ref; t; t = t->prev)
    { p = t->list;
      if (p && p->info.typ->type == Tpointer && ((Tnode*)p->info.typ->ref)->type == Tuchar && !strcmp(p->sym->name, "__ptr"))
      { p = p->next;
        return p && (p->info.typ->type == Tint || p->info.typ->type == Tuint) && !strcmp(p->sym->name, "__size");
      }
    }
  }
  return 0;
}

int
is_binary(Tnode *typ)
{ Entry *p;
  Table *t;
  if (typ->type == Tstruct || typ->type == Tclass) 
  { for (t = (Table*)typ->ref; t; t = t->prev)
    { p = t->list;
      if (p && p->info.typ->type == Tpointer && ((Tnode*)p->info.typ->ref)->type == Tuchar && !strcmp(p->sym->name, "__ptr"))
      { p = p->next;
        return p && (p->info.typ->type == Tint || p->info.typ->type == Tuint) && !strcmp(p->sym->name, "__size");
      }
    }
  }
  return 0;
}

is_attachment(Tnode *typ)
{ Entry *p;
  Table *t;
  if (!is_binary(typ))
    return 0;
  for (t = (Table*)typ->ref; t; t = t->prev)
  { for (p = t->list; p; p = p->next)
    { if (is_string(p->info.typ) && !strcmp(p->sym->name, "id"))
      { p = p->next;
        if (!p || !is_string(p->info.typ) || strcmp(p->sym->name, "type"))
          break;
        p = p->next;
        if (!p || !is_string(p->info.typ) || strcmp(p->sym->name, "options"))
          break;
        return 1;
      }
    }
  }
  return 0;
}

char *
has_soapref(Tnode *typ)
{ Entry *p;
  Table *t;
  if (typ->type == Tstruct || typ->type == Tclass) 
  { for (t = (Table*)typ->ref; t; t = t->prev)
    { for (p = t->list; p; p = p->next)
        if (p->info.typ->type == Tpointer && ((Tnode*)p->info.typ->ref)->type == Tstruct && ((Tnode*)p->info.typ->ref)->id == lookup("soap"))
          return p->sym->name;
    }
  }
  return 0;
}

int
is_primitive(Tnode *typ)
{ return typ->type <= Ttime; 
}

int
is_string(Tnode *typ)
{ return typ->type == Tpointer && ((Tnode*)typ->ref)->type == Tchar;
}

int
is_wstring(Tnode *typ)
{ return typ->type == Tpointer && ((Tnode*)typ->ref)->type == Twchar;
}

int
reflevel(Tnode *typ)
{ int level;
  for (level = 0; typ->type == Tpointer; level++)
    typ = (Tnode*)typ->ref;
  return level;
}

void
soap_out(Tnode *typ)
{ Table *table,*t;
  Entry *p;
  int cardinality,i,j;
  Tnode *n;
  if (is_dynamic_array(typ))
  { soap_out_Darray(typ);
    return;
  }
  if (is_primitive(typ) && typ->type != Tenum)
  {
    fprintf(fhead,"\nSOAP_FMAC1 void SOAP_FMAC2 soap_out_%s(struct soap*, const char*, int, const %s, const char*);", c_ident(typ),c_type_id(typ, "*")); 
      if (typ->transient < 0)
        return;
    fprintf(fout,"\n\nSOAP_FMAC1 void SOAP_FMAC2 soap_out_%s(struct soap *soap, const char *tag, int id, const %s, const char *type)\n{", c_ident(typ),c_type_id(typ, "*a")); 
    if (typ->type == Tllong || typ->type == Tullong)
      fprintf(fout,"\n\tsoap_out%s(soap, tag, id, a, type, SOAP_%s);\n}", c_type(typ),c_ident(typ)); 
    else
      fprintf(fout,"\n\tsoap_out%s(soap, tag, id, a, type, SOAP_%s);\n}", the_type(typ),c_ident(typ)); 
    return;
  } else if (is_string(typ))
  {
    fprintf(fhead,"\nSOAP_FMAC1 void SOAP_FMAC2 soap_out_%s(struct soap*, const char*, int, char*const*, const char*);", c_ident(typ));
      if (typ->transient < 0)
        return;
    fprintf(fout,"\n\nSOAP_FMAC1 void SOAP_FMAC2 soap_out_%s(struct soap *soap, const char *tag, int id, char *const*a, const char *type)\n{", c_ident(typ));
    fprintf(fout,"\n\tsoap_outstring(soap, tag, id, a, type, SOAP_%s);\n}", c_ident(typ)); 
    return;
  } else if (is_wstring(typ))
  {
    fprintf(fhead,"\nSOAP_FMAC1 void SOAP_FMAC2 soap_out_%s(struct soap*, const char*, int, wchar_t*const*, const char*);", c_ident(typ));
      if (typ->transient < 0)
        return;
    fprintf(fout,"\n\nSOAP_FMAC1 void SOAP_FMAC2 soap_out_%s(struct soap *soap, const char *tag, int id, wchar_t *const*a, const char *type)\n{", c_ident(typ));
    fprintf(fout,"\n\tsoap_outwstring(soap, tag, id, a, type, SOAP_%s);\n}", c_ident(typ)); 
    return;
  }
  switch(typ->type)
    {
    case Tstruct:
    table=(Table*)typ->ref;
      /* DYNAMIC ARRAY */
      
      fprintf(fhead,"\nSOAP_FMAC1 void SOAP_FMAC2 soap_out_%s(struct soap*, const char*, int, const %s, const char*);", c_ident(typ),c_type_id(typ, "*")); 
      if (typ->transient < 0)
        return;
      fprintf(fout,"\n\nSOAP_FMAC1 void SOAP_FMAC2 soap_out_%s(struct soap *soap, const char *tag, int id, const %s, const char *type)\n{", c_ident(typ),c_type_id(typ, "*a")); 
      if (!is_primclass(typ))
        fprintf(fout,"\n\tsoap_element_begin_out(soap, tag, soap_embedded_id(soap, id, a, SOAP_%s), type);",c_ident(typ));
      fflush(fout);
      for (t = table; t != (Table *) 0; t = t->prev) { 

	for (p = t->list; p != (Entry*) 0; p = p->next) {
	  if ((p->info.sto & SmustUnderstand) && !(p->info.sto & Sconst) && !is_transient(p->info.typ) && !is_void(p->info.typ) && p->info.typ->type != Tfun)
	    fprintf(fout, "\n\tsoap->mustUnderstand = 1;");
	  if (p->info.sto & Sconst)
	    fprintf(fout, "\n\t/* const %s skipped */", p->sym->name);
	  else
	  if (is_transient(p->info.typ))
	    fprintf(fout, "\n\t/* transient %s skipped */", p->sym->name);
	  else if (is_repetition(p))
	  { 
      fprintf(fout,"\n\tif (a->%s)", p->next->sym->name);
      fprintf(fout,"\n\t{\tint i;\n\t\tfor (i = 0; i < a->%s; i++)", p->sym->name);
      if (((Tnode*)p->next->info.typ->ref)->type == Tclass)
        fprintf(fout,"\n\t\t\ta->%s[i].soap_out(soap, \"%s\", -1, \"%s\");", p->next->sym->name, ns_convert(p->next->sym->name),xsi_type(p->next->info.typ->ref));
      else
        fprintf(fout,"\n\t\t\tsoap_out_%s(soap, \"%s\", -1, a->%s + i, \"%s\");", c_ident(p->next->info.typ->ref), ns_convert(p->next->sym->name), p->next->sym->name, xsi_type(p->next->info.typ->ref));
      fprintf(fout,"\n\t}");
          p = p->next;
	  }
	  else if(p->info.typ->type==Tarray)
	    fprintf(fout,"\n\tsoap_out_%s(soap, \"%s\", -1, a->%s, \"%s\");",
	    c_ident(p->info.typ),ns_convert(p->sym->name),p->sym->name,xsi_type(p->info.typ));
	  else if(p->info.typ->type==Tclass){
	    /* CLASS ( within struct )*/
	    fprintf(fout,"\n\ta->%s.soap_out(soap, \"%s\", -1, \"%s\");", p->sym->name,ns_convert(p->sym->name),xsi_type(p->info.typ));
	  }
	  else if (is_XML(p->info.typ) && is_string(p->info.typ))
	    fprintf(fout,"\n\tsoap_outliteral(soap, \"%s\", &a->%s);", ns_convert(p->sym->name),p->sym->name);
	  else if (is_XML(p->info.typ) && is_wstring(p->info.typ))
	    fprintf(fout,"\n\tsoap_outwliteral(soap, \"%s\", &a->%s);", ns_convert(p->sym->name),p->sym->name);
	  else if (p->info.typ->type != Tfun && !is_void(p->info.typ))
	    fprintf(fout,"\n\tsoap_out_%s(soap, \"%s\", -1, &a->%s, \"%s\");",
	    c_ident(p->info.typ),ns_convert(p->sym->name),p->sym->name, xsi_type(p->info.typ));
	}
      }	
      if (!is_primclass(typ))
        fprintf(fout,"\n\tsoap_element_end_out(soap, tag);");
      fprintf(fout,"\n}");	 
      fflush(fout);
      break;
      
     case Tclass:
       /* CLASS */
      if (typ->transient < 0)
        return;
       table=(Table*)typ->ref;
       /*       fprintf(fhead,"\nvirtual void %s::soap_out(char *,int);",
	       ((Table *) typ->ref)->sym->name); */
       fprintf(fout,"\n\nvoid %s::soap_out(struct soap *soap, const char *tag, int id, const char *type) const", typ->id->name); 
       fprintf(fout,"\n{\n\tsoap_out_%s(soap, tag, id, this, type);\n}", typ->id->name); 
       fprintf(fhead,"\n\nSOAP_FMAC1 void SOAP_FMAC2 soap_out_%s(struct soap*, const char*, int, const %s, const char*);", typ->id->name, c_type_id(typ, "*")); 
       fprintf(fout,"\n\nSOAP_FMAC1 void SOAP_FMAC2 soap_out_%s(struct soap *soap, const char *tag, int id, const %s, const char *type)\n{", typ->id->name, c_type_id(typ, "*a")); 
      fflush(fout);
       if (is_primclass(typ))
       {
	for (table = (Table*)typ->ref; table; table = table->prev)
	{ p = table->list;
	  if (p && !strcmp(p->sym->name, "__item"))
	    break;
        }
	  if ((p->info.sto & SmustUnderstand) && !(p->info.sto & Sconst) && !is_transient(p->info.typ) && !is_void(p->info.typ) && p->info.typ->type != Tfun)
	    fprintf(fout, "\n\tsoap->mustUnderstand = 1;");
	  if (p->info.sto & Sconst)
	    fprintf(fout, "\n\t/* const %s skipped */", p->sym->name);
	  else
	  if (is_transient(p->info.typ))
	    fprintf(fout, "\n\t/* transient %s skipped */", p->sym->name);
	  else if(p->info.typ->type==Tarray){
	    fprintf(fout,"\n\tsoap_out_%s(soap, tag, id, ((%s*)a)->%s, \"%s\");", c_ident(p->info.typ), table->sym->name,p->sym->name, xsi_type(typ));
	  }
	  else if(p->info.typ->type==Tclass){
	    /* CLASS ( within CLASS ) */
	    fprintf(fout,"\n\t(((%s*)a)->%s).soap_out(soap, tag, id, \"%s\");", table->sym->name, p->sym->name,xsi_type(typ));
	  }
	  else if (is_XML(p->info.typ) && is_string(p->info.typ))
	    fprintf(fout,"\n\tsoap_outliteral(soap, \"%s\", &(((%s*)a)->%s);", ns_convert(p->sym->name),table->sym->name,p->sym->name);
	  else if (is_XML(p->info.typ) && is_wstring(p->info.typ))
	    fprintf(fout,"\n\tsoap_outwliteral(soap, \"%s\", &(((%s*)a)->%s);", ns_convert(p->sym->name),table->sym->name,p->sym->name);
	  else if (p->info.typ->type != Tfun && !is_void(p->info.typ)) {
	    fprintf(fout,"\n\tsoap_out_%s(soap, tag, id, &(((%s*)a)->%s), \"%s\");", c_ident(p->info.typ), table->sym->name,p->sym->name,xsi_type(typ));
	  }
       }
       else
       { fprintf(fout,"\n\tsoap_element_begin_out(soap, tag, soap_embedded_id(soap, id, a, SOAP_%s), \"%s\");", c_ident(typ), xsi_type(typ));
  	fflush(fout);
      
      i=0;
      /* Get the depth of the inheritance hierarchy */
      for (t = table; t != (Table *) 0; t = t->prev) { 
	/*if(t==(Table *) 0 ) return;*/
	i++;
      }

      /* Call routines to output the member data of the class */
      /* Data members of the Base Classes are outputed first
	 followed by the data members of the Derived classes.
	 Over written data members are outputed twice once for the base class
	 they are defined in and once for the derived class that overwrites
	 them */
      
      for(;i>0;i--){
	t = table;
	for(j=0;j<i-1;j++){
	  t = t->prev;
	}
	
	for (p = t->list; p != (Entry*) 0; p = p->next) {
	  if ((p->info.sto & SmustUnderstand) && !(p->info.sto & Sconst) && !is_transient(p->info.typ) && !is_void(p->info.typ) && p->info.typ->type != Tfun)
	    fprintf(fout, "\n\tsoap->mustUnderstand = 1;");
	  if (p->info.sto & Sconst)
	    fprintf(fout, "\n\t/* const %s skipped */", p->sym->name);
	  else
	  if (is_transient(p->info.typ))
	    fprintf(fout, "\n\t/* transient %s skipped */", p->sym->name);
	  else if (is_repetition(p))
	  { 
      fprintf(fout,"\n\tif (((%s*)a)->%s)", table->sym->name, p->next->sym->name);
      fprintf(fout,"\n\t{\tint i;\n\t\tfor (i = 0; i < ((%s*)a)->%s; i++)", table->sym->name, p->sym->name);
      if (((Tnode*)p->next->info.typ->ref)->type == Tclass)
        fprintf(fout,"\n\t\t\t((%s*)a)->%s[i].soap_out(soap, \"%s\", -1, \"%s\");", table->sym->name, p->next->sym->name, ns_overridden(t, p->next),xsi_type(p->next->info.typ->ref));
      else
        fprintf(fout,"\n\t\t\tsoap_out_%s(soap, \"%s\", -1, ((%s*)a)->%s + i, \"%s\");", c_ident(p->next->info.typ->ref), ns_overridden(t, p->next), table->sym->name, p->next->sym->name, xsi_type(p->next->info.typ->ref));
      fprintf(fout,"\n\t}");
          p = p->next;
	  }
	  else if(p->info.typ->type==Tarray){
	    fprintf(fout,"\n\tsoap_out_%s(soap, \"%s\", -1, ((%s*)a)->%s, \"%s\");", c_ident(p->info.typ),ns_overridden(t, p), t->sym->name,p->sym->name, xsi_type(p->info.typ));
	  }
	  else if(p->info.typ->type==Tclass){
	    /* CLASS ( within CLASS ) */
	    fprintf(fout,"\n\t(((%s*)a)->%s).soap_out(soap, \"%s\", -1, \"%s\");", t->sym->name, p->sym->name,ns_overridden(t, p),xsi_type(p->info.typ));
	  }
	  else if (is_XML(p->info.typ) && is_string(p->info.typ))
	    fprintf(fout,"\n\tsoap_outliteral(soap, \"%s\", &(((%s*)a)->%s);", ns_overridden(t, p),t->sym->name,p->sym->name);
	  else if (is_XML(p->info.typ) && is_wstring(p->info.typ))
	    fprintf(fout,"\n\tsoap_outwliteral(soap, \"%s\", &(((%s*)a)->%s);", ns_overridden(t, p),t->sym->name,p->sym->name);
	  else if (p->info.typ->type != Tfun && !is_void(p->info.typ)) {
	    fprintf(fout,"\n\tsoap_out_%s(soap, \"%s\", -1, &(((%s*)a)->%s), \"%s\");", c_ident(p->info.typ),ns_overridden(t, p), t->sym->name,p->sym->name,xsi_type(p->info.typ));
	  }
  	fflush(fout);
	}
      }
      
         fprintf(fout,"\n\tsoap_element_end_out(soap, tag);");
      }
      fprintf(fout,"\n}");	 
  	fflush(fout);
      break;
      
    case Tpointer:
      fprintf(fhead,"\nSOAP_FMAC1 void SOAP_FMAC2 soap_out_%s(struct soap*, const char *, int, %s, const char *);", c_ident(typ),c_type_id(typ, "const*")); 
      if (typ->transient < 0)
        return;
      fprintf(fout,"\n\nSOAP_FMAC1 void SOAP_FMAC2 soap_out_%s(struct soap *soap, const char *tag, int id, %s, const char *type)\n{", c_ident(typ),c_type_id(typ, "const*a")); 
      fprintf(fout,"\n\tint i;");
      fprintf(fout,"\n\tstruct soap_plist *pp;");   
      fprintf(fout,"\n\tid = soap_embedded_id(soap, id, a, SOAP_%s);",c_ident(typ));
      fprintf(fout,"\n\tif(*a == NULL)");
      fprintf(fout,"\n\t         soap_element_null(soap, tag, id, type);");
      fprintf(fout,"\n\telse\n\t{");
      fprintf(fout,"\n\t         i = soap_pointer_lookup(soap, *a, SOAP_%s, &pp);",c_ident(typ->ref));
      fprintf(fout,"\n\t         if (id > 0)");
      fprintf(fout,"\n\t		{"); 
      fprintf(fout,"\n\t	        if (i)");
      fprintf(fout,"\n\t	              if (soap_is_embedded(soap, pp))");
      fprintf(fout,"\n\t			   soap_element_ref(soap, tag, id, i);");
      fprintf(fout,"\n\t                   else if (soap_is_single(soap, pp))");

      if(((Tnode *) typ->ref)->type == Tclass){
	/* Pointer points to a CLASS */
	fprintf(fout,"\n\t 		           (*a)->soap_out(soap, tag, 0, type);");
	fprintf(fout,"\n\t                   else\n\t{");
	fprintf(fout,"\n\t			   soap_set_embedded(soap, pp);");
	fprintf(fout,"\n\t                       (*a)->soap_out(soap, tag, i, type);");
	fprintf(fout,"\n\t                   }");
	fprintf(fout,"\n\t                   else   (*a)->soap_out(soap, tag, soap_pointer_enter(soap, *a, SOAP_%s, &pp), type);",c_ident(typ->ref));	
	fprintf(fout,"\n\t              }\n\t              else if (i)");
	fprintf(fout,"\n\t                     if (soap_is_embedded(soap, pp))");
	fprintf(fout,"\n\t                          soap_element_ref(soap, tag, 0, i);");
	fprintf(fout,"\n\t                     else if (soap_is_single(soap, pp))");
	fprintf(fout,"\n\t                             (*a)->soap_out(soap, tag, 0, type);");
	fprintf(fout,"\n\t                      else\n\t{");
	fprintf(fout,"\n\t                          soap_set_embedded(soap, pp);");
	fprintf(fout,"\n\t                          (*a)->soap_out(soap, tag, i, type);");
	fprintf(fout,"\n\t                     }");
	fprintf(fout,"\n\t              else    (*a)->soap_out(soap, tag, soap_pointer_enter(soap, *a, SOAP_%s, &pp), type);", c_ident(typ->ref));
	fprintf(fout,"\n\t}\n}");
      }
      else {
	fprintf(fout,"\n\t 		           soap_out_%s(soap, tag, 0, *a, type);",c_ident(typ->ref));
	fprintf(fout,"\n\t                   else\n\t{");
	fprintf(fout,"\n\t			   soap_set_embedded(soap, pp);");
	fprintf(fout,"\n\t                       soap_out_%s(soap, tag, i, *a, type);",c_ident(typ->ref));
	fprintf(fout,"\n\t                   }");
	fprintf(fout,"\n\t                   else   soap_out_%s(soap, tag, soap_pointer_enter(soap, *a, SOAP_%s, &pp), *a, type);",c_ident(typ->ref),c_ident(typ->ref));	
	fprintf(fout,"\n\t              }\n\t              else if (i)");
	fprintf(fout,"\n\t                     if (soap_is_embedded(soap, pp))");
	fprintf(fout,"\n\t                          soap_element_ref(soap, tag, 0, i);");
	fprintf(fout,"\n\t                     else if(soap_is_single(soap, pp))");
	fprintf(fout,"\n\t                             soap_out_%s(soap, tag, 0, *a, type);",c_ident(typ->ref));
	fprintf(fout,"\n\t                      else\n\t{");
	fprintf(fout,"\n\t                          soap_set_embedded(soap, pp);");
	fprintf(fout,"\n\t                          soap_out_%s(soap, tag, i, *a, type);",c_ident(typ->ref));
	fprintf(fout,"\n\t                     }");
	fprintf(fout,"\n\t              else    soap_out_%s(soap, tag, soap_pointer_enter(soap, *a, SOAP_%s, &pp),*a, type);",c_ident(typ->ref), c_ident(typ->ref));
	fprintf(fout,"\n\t}\n}");
      }
      
      break;

    case Tarray:
      fprintf(fhead,"\nSOAP_FMAC1 void SOAP_FMAC2 soap_out_%s(struct soap*, const char*, int, %s, const char*);", c_ident(typ),c_type_id(typ, "const")); 
      if (typ->transient < 0)
        return;
      fprintf(fout,"\n\nSOAP_FMAC1 void SOAP_FMAC2 soap_out_%s(struct soap *soap, const char *tag, int id, %s, const char *type)\n{", c_ident(typ),c_type_id(typ, "const a")); 
      fprintf(fout,"\n\tint i;");
      if (is_untyped(typ->ref))
        fprintf(fout,"\n\tsoap_array_begin_out(soap, tag, soap_embedded_id(soap, id, a, SOAP_%s), \"%s[%d]\", 0);",c_ident(typ), wsdl_type(typ->ref, "xsd"), get_dimension(typ));
      else
        fprintf(fout,"\n\tsoap_array_begin_out(soap, tag, soap_embedded_id(soap, id, a, SOAP_%s), \"%s[%d]\", 0);",c_ident(typ), xsi_type_Tarray(typ), get_dimension(typ));
      n=typ->ref;
      cardinality = 1;
      while(n->type==Tarray)
	{
	  n=n->ref;
	  cardinality++;
	}

      fprintf(fout,"\n\tfor (i = 0; i < %d; i++)\n\t{",get_dimension(typ));
     if (((Tnode *)typ->ref)->type == Tclass)
     { if(cardinality>1)
         fprintf(fout,"\n\t\ta[i].soap_out(soap, \"item\", -1, \"%s\")", xsi_type(typ->ref));
       else fprintf(fout,"\n\t\t(a+i)->soap_out(soap, \"item\", -1, \"%s\")", xsi_type(typ->ref));
     }
     else
     { if(((Tnode *)typ->ref)->type != Tarray)
       { if(((Tnode *)typ->ref)->type == Tpointer)
	  fprintf(fout,"\n\t\tsoap->position = 1;\n\t\tsoap->positions[0] = i;\n\t\tsoap_out_%s(soap, \"item\", -1, a", c_ident(typ->ref));
	 else
	  fprintf(fout,"\n\t\tsoap_out_%s(soap, \"item\", -1, a",c_ident(typ->ref));
       }
       else
         fprintf(fout,"\n\t\tsoap_out_%s(soap, \"item\", -1, a",c_ident(typ->ref));
       if(cardinality>1)
         fprintf(fout,"[i], \"%s\")", xsi_type(typ->ref));
       else
         fprintf(fout,"+i, \"%s\")", xsi_type(typ->ref));
      }
      if(((Tnode *)typ->ref)->type == Tpointer)
        fprintf(fout,";\n\t}\n\tsoap->position = 0;\n\tsoap_element_end_out(soap, tag);\n}");		
      else
        fprintf(fout,";\n\t}\n\tsoap_element_end_out(soap, tag);\n}");		
      break;

    case Tenum:
      fprintf(fhead, "\nSOAP_FMAC1 void SOAP_FMAC2 soap_out_%s(struct soap*, const char*, int, const %s, const char*);", c_ident(typ), c_type_id(typ, "*"));
      if (typ->transient < 0)
        return;
      fprintf(fout, "\n\nSOAP_FMAC1 void SOAP_FMAC2 soap_out_%s(struct soap *soap, const char *tag, int id, const %s, const char *type)", c_ident(typ), c_type_id(typ, "*a"));
      if (is_mask(typ))
      { fprintf(fout, "\n{\tLONG64 i;\n\tsoap_element_begin_out(soap, tag, soap_embedded_id(soap, id, a, SOAP_%s), type);", c_ident(typ));
        fprintf(fout, "\n\tfor (i = 1; i; i <<= 1)");
        fprintf(fout, "\n\t\tswitch ((LONG64)*a & i)\n\t\t{");
        for (t = (Table*)typ->ref; t; t = t->prev)
        { for (p = t->list; p; p = p->next)
	    fprintf(fout, "\n\t\t\tcase %lld: soap_send(soap, \"%s \"); break;", p->info.val.i, ns_convert(p->sym->name));
        }	
        fprintf(fout, "\n\t\t}");
      }
      else
      { fprintf(fout, "\n{\tsoap_element_begin_out(soap, tag, soap_embedded_id(soap, id, a, SOAP_%s), type);", c_ident(typ));
        fprintf(fout, "\n\tswitch (*a)\n\t{");
        for (t = (Table*)typ->ref; t; t = t->prev)
        { for (p = t->list; p; p = p->next)
	    fprintf(fout, "\n\t\tcase %s: soap_send(soap, \"%s\"); break;", p->sym->name, ns_convert(p->sym->name));
        }	
        fprintf(fout, "\n\t\tdefault:\n\t\t\tsprintf(soap->tagbuf, \"%%lld\", (LONG64)*a);\n\t\t\tsoap_send(soap, soap->tagbuf);\n\t\t\tbreak;");
        fprintf(fout, "\n\t}");
      }
      fprintf(fout, "\n\tsoap_element_end_out(soap, tag);\n}");

    default: break;
    }
}	  

void
soap_out_Darray(Tnode *typ)
{ int i, j, d;
  Table *table;
  Entry *p, *q;

  table=(Table*)typ->ref;
  q = table->list;

  p = is_dynamic_array(typ);
  
  fprintf(fhead,"\nSOAP_FMAC1 void SOAP_FMAC2 soap_out_%s(struct soap*, const char*, int, const %s, const char*);", c_ident(typ),c_type_id(typ, "*")); 
      if (typ->transient < 0)
        return;
  if (typ->type == Tclass)
  { fprintf(fout,"\n\nvoid %s::soap_out(struct soap *soap, const char *tag, int id, const char *type) const", c_type(typ)); 
    fprintf(fout,"\n{\tsoap_out_%s(soap, tag, id, this, type);\n}", c_ident(typ)); 
  }
  fflush(fout);
  fprintf(fout,"\n\nSOAP_FMAC1 void SOAP_FMAC2 soap_out_%s(struct soap *soap, const char *tag, int id, const %s, const char *type)\n{", c_ident(typ),c_type_id(typ, "*a")); 
  fprintf(fout,"\n\tint i;\n\tstruct soap_plist *pp;");
  d = get_Darraydims(typ);
  if (d)
    fprintf(fout,"\n\tint n = soap_size(a->__size, %d);", d);
  else
    fprintf(fout,"\n\tint n = a->__size;");
  if (has_ns(typ) || is_untyped(typ) || is_binary(typ))
    fprintf(fout,"\n\tif (n != 0 && !a->%s)\n\t{\tsoap_element_null(soap, tag, id, type);\n\t\treturn;\n\t}", p->sym->name);
  else
  { if (is_untyped(p->info.typ))
      if (typ->type == Tclass)
      { if (has_offset(typ))
          if (d)
	    fprintf(fout,"\n\tchar *t = soap_putsizesoffsets(soap, \"%s\", a->__size, a->__offset, %d);", wsdl_type(p->info.typ, "xsd"), d); 
	  else
	    fprintf(fout,"\n\tchar *t = soap_putsize(soap, \"%s\", n + a->__offset);", wsdl_type(p->info.typ, "xsd"));
        else if (d)
	  fprintf(fout,"\n\tchar *t = soap_putsizes(soap, \"%s\", a->__size, %d);", wsdl_type(p->info.typ, "xsd"), d);
        else
	  fprintf(fout,"\n\tchar *t = soap_putsize(soap, \"%s\", a->__size);", wsdl_type(p->info.typ, "xsd"));
      }
      else
      { if (has_offset(typ))
          if (d)
	    fprintf(fout,"\n\tchar *t = soap_putsizesoffsets(soap, type, a->__size, a->__offset, %d);", d); 
	  else
	    fprintf(fout,"\n\tchar *t = soap_putsize(soap, type, n + a->__offset);");
        else if (d)
	  fprintf(fout,"\n\tchar *t = soap_putsizes(soap, type, a->__size, %d);", d);
        else
	  fprintf(fout,"\n\tchar *t = soap_putsize(soap, type, a->__size);");
      }
    else if (typ->type == Tclass)
    { if (has_offset(typ))
        if (d)
          fprintf(fout,"\n\tchar *t = soap_putsizesoffsets(soap, \"%s\", a->__size, a->__offset, %d);", xsi_type(typ), d);
        else
          fprintf(fout,"\n\tchar *t = soap_putsize(soap, \"%s\", n + a->__offset);",xsi_type(typ));
      else if (d)
        fprintf(fout,"\n\tchar *t = soap_putsizes(soap, \"%s\", a->__size, %d);", xsi_type(typ),d);
      else
        fprintf(fout,"\n\tchar *t = soap_putsize(soap, \"%s\", a->__size);" ,xsi_type(typ));
    }
    else if (has_offset(typ))
      if (d)
        fprintf(fout,"\n\tchar *t = soap_putsizesoffsets(soap, type, a->__size, a->__offset, %d);", d); 
      else
        fprintf(fout,"\n\tchar *t = soap_putsize(soap, type, n + a->__offset);"); 
    else if (d)
      fprintf(fout,"\n\tchar *t = soap_putsizes(soap, type, a->__size, %d);", d); 
    else
      fprintf(fout,"\n\tchar *t = soap_putsize(soap, type, a->__size);"); 
    fprintf(fout,"\n\tif (n != 0 && !a->%s)\n\t{\tsoap_element_null(soap, tag, id, t);\n\t\treturn;\n\t}", p->sym->name);
  }
  fprintf(fout,"\n\ti = soap_array_pointer_lookup(soap, &a->%s, n, SOAP_%s, &pp);",p->sym->name,c_ident(typ));
  if (is_attachment(typ))
  { fprintf(fout,"\n\tif (a->id || a->type)\n\t{\tif (i && !a->id)\n\t\t{\tsprintf(soap->tagbuf, soap->dime_id_format, i);\n\t\t\tsoap_element_href(soap, tag, id, soap->tagbuf);\n\t\t\tsoap_set_attached(soap, pp, soap->tagbuf, a->type, a->options, n);\n\t\t}\n\t\telse\n\t\t{\tsoap_element_href(soap, tag, id, a->id);\n\t\t\tif (i)\n\t\t\t\tsoap_set_attached(soap, pp, a->id, a->type, a->options, n);\n\t\t}\n\t\treturn;\n\t}");
  }
  fprintf(fout,"\n\tif (i)\n\t{\tif (soap_is_embedded(soap, pp))\n\t\t{\tsoap_element_ref(soap, tag, 0, i);\n\t\t\treturn;\n\t\t}\n\t\tif (soap_is_single(soap, pp))\n\t\t\ti = 0;\n\t}");
  if (has_ns(typ) || is_untyped(typ) || is_binary(typ))
    fprintf(fout,"\n\tsoap_element_begin_out(soap, tag, id, \"%s\");", xsi_type(typ));
  else if (has_offset(typ))
    if (d)
      fprintf(fout,"\n\tsoap_array_begin_out(soap, tag, id, t, soap_putoffsets(soap, a->__offset, %d));", d);
    else
      fprintf(fout,"\n\tsoap_array_begin_out(soap, tag, id, t, soap_putoffset(soap, a->__offset));");
  else
    fprintf(fout,"\n\tsoap_array_begin_out(soap, tag, id, t, NULL);");
  fprintf(fout,"\n\tif (i)\n\t\tsoap_set_embedded(soap, pp);");
  if (is_binary(typ) && !is_hexBinary(typ))
    fprintf(fout, "\n\tsoap_putbase64(soap, a->__ptr, a->__size);");
  else
  { fprintf(fout,"\n\tfor (i = 0; i < n; i++)\n\t{");
    if (is_hexBinary(typ))
      fprintf(fout, "\n\t\tsoap_puthex(soap, (a->__ptr)[i]);");
    else if (((Tnode *)p->info.typ->ref)->type == Tclass)
      fprintf(fout,"\n\t\ta->%s[i].soap_out(soap, \"item\", -1, \"%s\");", p->sym->name, xsi_type(((Tnode *)p->info.typ->ref)));
    else if (!has_ns(typ) && !is_untyped(typ) && ((Tnode *)p->info.typ->ref)->type == Tpointer)
    { if (d)
      { fprintf(fout,"\n\t\tsoap->position = %d;", d);
        for (i = 0; i < d; i++)
	{ fprintf(fout, "\n\t\tsoap->positions[%d] = i", i);
          for (j = i+1; j < d; j++)
	    fprintf(fout, "/a->__size[%d]", j);
	  fprintf(fout, "%%a->__size[%d];", i);
        }
	fprintf(fout, "\n\t\tsoap_out_%s(soap, \"%s\", 0, &a->%s[i], \"%s\");",c_ident(((Tnode *)p->info.typ->ref)), 
	p->sym->name[5]?ns_convert(p->sym->name+5):"item", p->sym->name,
	xsi_type(((Tnode *)p->info.typ->ref)));
      }
      else
        fprintf(fout,"\n\t\tsoap->position = 1;\n\t\tsoap->positions[0] = i;\n\t\tsoap_out_%s(soap, \"%s\", 0, &a->%s[i], \"%s\");",c_ident(((Tnode *)p->info.typ->ref)), 
	p->sym->name[5]?ns_convert(p->sym->name+5):"item", p->sym->name,
	xsi_type(((Tnode *)p->info.typ->ref)));
    }
    else
      fprintf(fout,"\n\t\tsoap_out_%s(soap, \"%s\", 0, &a->%s[i], \"%s\");",c_ident(((Tnode *)p->info.typ->ref)), 
	p->sym->name[5]?ns_convert(p->sym->name+5):"item", p->sym->name,
      xsi_type(((Tnode *)p->info.typ->ref)));
  }
  if (!has_ns(typ) && !is_untyped(typ) && ((Tnode *)p->info.typ->ref)->type == Tpointer)
    fprintf(fout,"\n\t}\n\tsoap->position = 0;\n\tsoap_element_end_out(soap, tag);");
  else if (is_binary(typ) && !is_hexBinary(typ))
    fprintf(fout,"\n\tsoap_element_end_out(soap, tag);");
  else
    fprintf(fout,"\n\t}\n\tsoap_element_end_out(soap, tag);");
  fprintf(fout,"\n}");	 
}

void
soap_get(Tnode *typ)
{
  Tnode *temp;
  
  if(typ->type==Tarray)
    {
      /* ARRAY */
      temp = typ;
      while(temp->type == Tarray){
	temp = temp->ref;
      }
      fprintf(fhead,"\nSOAP_FMAC1 %s * SOAP_FMAC2 soap_get_%s(struct soap*, %s, const char*, const char*);", c_type(temp),c_ident(typ),c_type(typ));
      fprintf(fout,"\nSOAP_FMAC1 %s * SOAP_FMAC2 soap_get_%s(struct soap *soap, %s, const char *tag, const char *type)", c_type(temp),c_ident(typ),c_type_id(typ, "a"));
      fprintf(fout,"\n{\t%s;",c_type_id(temp, "(*p)"));
      fprintf(fout,"\n\tif ((p = soap_in_%s(soap, tag, a, type)))", c_ident(typ));
    }
  else if(typ->type==Tclass)
    {
      /* CLASS  */
      fprintf(fout,"\n\nvoid *%s::soap_get(struct soap *soap, const char *tag, const char *type)", c_type(typ));
      fprintf(fout,"\n{\n\treturn soap_get_%s(soap, this, tag, type);\n}", c_ident(typ));
      fprintf(fhead,"\nSOAP_FMAC1 %s SOAP_FMAC2 soap_get_%s(struct soap*, %s, const char*, const char*);", c_type_id(typ, "*"),c_ident(typ),c_type_id(typ, "*"));
      fprintf(fout,"\n\nSOAP_FMAC1 %s SOAP_FMAC2 soap_get_%s(struct soap *soap, %s, const char *tag, const char *type)\n{", c_type_id(typ, "*"),c_ident(typ),c_type_id(typ, "*p"));
      fprintf(fout,"\n\tif ((p = soap_in_%s(soap, tag, p, type)))", c_ident(typ));
    }
  else 
    {
      fprintf(fhead,"\nSOAP_FMAC1 %s SOAP_FMAC2 soap_get_%s(struct soap*, %s, const char*, const char*);", c_type_id(typ, "*"),c_ident(typ),c_type_id(typ, "*"));
      fprintf(fout,"\n\nSOAP_FMAC1 %s SOAP_FMAC2 soap_get_%s(struct soap *soap, %s, const char *tag, const char *type)\n{", c_type_id(typ, "*"),c_ident(typ),c_type_id(typ, "*p"));
      fprintf(fout,"\n\tif ((p = soap_in_%s(soap, tag, p, type)))", c_ident(typ));
    }
  fprintf(fout,"\n\t\tsoap_getindependent(soap);");
  fprintf(fout,"\n\treturn p;\n}");
  fflush(fout);
}

void
soap_in(Tnode *typ)
{
  Entry *p;
  Table *table,*t;
  int total,a, cardinality,i,j;
  Tnode *n, *temp;
  if (is_dynamic_array(typ))
  { soap_in_Darray(typ);
    return;
  }
  if (is_primitive(typ) && typ->type != Tenum || is_string(typ) || is_wstring(typ))
  {
    fprintf(fhead,"\nSOAP_FMAC1 %s * SOAP_FMAC2 soap_in_%s(struct soap*, const char*, %s, const char*);", c_type(typ), c_ident(typ),c_type_id(typ, "*")); 
      if (typ->transient < 0)
        return;
    fprintf(fout,"\n\nSOAP_FMAC1 %s * SOAP_FMAC2 soap_in_%s(struct soap *soap, const char *tag, %s, const char *type)\n{", c_type(typ), c_ident(typ),c_type_id(typ, "*a")); 
    if (typ->type == Tllong || typ->type == Tullong)
      fprintf(fout,"\n\treturn soap_in%s(soap, tag, a, type, SOAP_%s);\n}", c_type(typ), c_ident(typ));
    else if (is_wstring(typ))
      fprintf(fout,"\n\treturn soap_inwstring(soap, tag, a, type, SOAP_%s);\n}", c_ident(typ));
    else
      fprintf(fout,"\n\treturn soap_in%s(soap, tag, a, type, SOAP_%s);\n}", the_type(typ), c_ident(typ));
    return;
  }
  switch(typ->type)
    {
    case Tstruct:

      
      fprintf(fhead,"\nSOAP_FMAC1 %s SOAP_FMAC2 soap_in_%s(struct soap*, const char*, %s, const char*);", c_type_id(typ, "*"),c_ident(typ),c_type_id(typ, "*"));
      if (typ->transient < 0)
        return;
      fprintf(fout,"\n\nSOAP_FMAC1 %s SOAP_FMAC2 soap_in_%s(struct soap *soap, const char *tag, %s, const char *type)\n{", c_type_id(typ, "*"),c_ident(typ),c_type_id(typ, "*a"));
      table=(Table *)typ->ref;
      if (!is_primclass(typ))
      { a=0;
      for (t = table; t; t = t->prev)
	for (p = table->list; p; p = p->next)
	{ if (!(p->info.sto & Sconst) && p->info.typ->type != Tfun && !is_void(p->info.typ) && !is_transient(p->info.typ) && !is_repetition(p))
	    if(a==0)
	    { fprintf(fout,"\n\tshort soap_flag_%s = 1", p->sym->name);
	      a=1;
            }
	    else
	      fprintf(fout,", soap_flag_%s = 1", p->sym->name);
	}
	fprintf(fout,";");
        fprintf(fout,"\n\tif (soap_element_begin_in(soap, tag))\n\t\treturn NULL;");
        fprintf(fout,"\n\tif (*soap->type && soap_match_tag(soap, soap->type, type))");
        fprintf(fout,"\n\t{\tsoap->error = SOAP_TYPE_MISMATCH;\n\t\tsoap_revert(soap);");
        fprintf(fout,"\n\t\treturn NULL;\n\t}");
        fprintf(fout,"\n\tif (soap->null)\n\t{\tif (soap->enable_null)");
        fprintf(fout,"\n\t\t{\tsoap->error = SOAP_NULL;");
        fprintf(fout,"\n\t\t\treturn NULL;");
        fprintf(fout,"\n\t\t}\n\t\telse");
        fprintf(fout,"\n\t\t\treturn a;");
        fprintf(fout,"\n\t}");
        fprintf(fout,"\n\tif (soap->body && !*soap->href)");
        fprintf(fout,"\n\t{\ta = (%s)soap_id_enter(soap, soap->id, a, SOAP_%s, sizeof(%s), 0);",c_type_id(typ, "*"), c_ident(typ), c_type(typ));
      }
      else
        fprintf(fout,"\n\t{\ta = (%s)soap_id_enter(soap, 0, a, SOAP_%s, sizeof(%s), 0);",c_type_id(typ, "*"), c_ident(typ), c_type(typ));
      fprintf(fout,"\n\t\tif (!a)\n\t\t\treturn NULL;");
      fprintf(fout,"\n\t\tif (soap->alloced)");
      fprintf(fout,"\n\t\t\tsoap_default_%s(soap, a);",c_ident(typ));
      if (!is_primclass(typ))
        fprintf(fout,"\n\t\tfor (;;)");
      fprintf(fout,"\n\t\t{");
      if (!is_primclass(typ))
        fprintf(fout,"\tsoap->error = SOAP_TAG_MISMATCH;");

      a=0;
      for (t = table; t; t = t->prev) { 
	for (p = t->list; p; p = p->next) 
	  if (p->info.sto & Sconst)
	    fprintf(fout, "\n\t\t/* const %s skipped */", p->sym->name);
	  else if (is_transient(p->info.typ))
	    fprintf(fout, "\n\t\t/* transient %s skipped */", p->sym->name);
	  else if (is_repetition(p))
	  { 
    fprintf(fout,"\n\t\t\tif (soap_flag_%s && soap->error == SOAP_TAG_MISMATCH)",p->next->sym->name);
    fprintf(fout,"\n\t\t\t{\t%s;\n\t\t\t\tsoap_new_block(soap);", c_type_id(p->next->info.typ, "p"));
    fprintf(fout,"\n\t\t\t\tfor (a->%s = 0; ; a->%s++)", p->sym->name, p->sym->name);
    fprintf(fout,"\n\t\t\t\t{\tp = (%s)soap_push_block(soap, sizeof(%s));", c_type(p->next->info.typ), c_type(p->next->info.typ->ref));
    if (((Tnode*)p->next->info.typ->ref)->type == Tclass)
      fprintf(fout,"\n\t\t\t\t\tp->%s::soap_default(soap);", c_type(p->next->info.typ->ref));
    else
      fprintf(fout,"\n\t\t\t\t\tsoap_default_%s(soap, p);", c_ident(p->next->info.typ->ref));
    fprintf(fout,"\n\t\t\t\t\tif (!soap_in_%s(soap, \"%s\", p, \"%s\"))", c_ident(p->next->info.typ->ref), ns_convert(p->next->sym->name), xsi_type(p->next->info.typ->ref));
    fprintf(fout,"\n\t\t\t\t\t\tbreak;");
    fprintf(fout,"\n\t\t\t\t}");
    fprintf(fout,"\n\t\t\t\tsoap_pop_block(soap);");
    if (((Tnode*)p->next->info.typ->ref)->type == Tclass)
      fprintf(fout,"\n\t\t\t\tif (soap->blist->size)\n\t\t\t\t\ta->%s = soap_instantiate_%s(soap, soap->blist->size/sizeof(%s), NULL, NULL);\n\t\t\t\telse\n\t\t\t\t\ta->%s = NULL;", p->next->sym->name, c_type(p->next->info.typ->ref), c_type(p->next->info.typ->ref), p->next->sym->name);
    else
      fprintf(fout,"\n\t\t\t\ta->%s = (%s)soap_malloc(soap, soap->blist->size);", p->next->sym->name, c_type(p->next->info.typ));
    fprintf(fout,"\n\t\t\t\tsoap_store_block(soap, (char*)a->%s);", p->next->sym->name);
    fprintf(fout,"\n\t\t\t\tsoap_flag_%s = 0;", p->next->sym->name);
    fprintf(fout,"\n\t\t\t\tif (soap->error == SOAP_TAG_MISMATCH)\n\t\t\t\t\tcontinue;\n\t\t\t}");
          p = p->next;
	  }
	  else
	  { 
	    if (!is_primclass(typ) && p->info.typ->type != Tfun && !is_void(p->info.typ))
	      fprintf(fout,"\n\t\t\tif (soap_flag_%s && soap->error == SOAP_TAG_MISMATCH)",p->sym->name);
	   if (is_unmatched(p->sym))
	   {
	    if (is_XML(p->info.typ) && is_string(p->info.typ)) {
	      fprintf(fout,"\n\t\t\t\tif (soap_inliteral(soap, NULL, &a->%s))", p->sym->name);
	    } else if (is_XML(p->info.typ) && is_wstring(p->info.typ)) {
	      fprintf(fout,"\n\t\t\t\tif (soap_inwliteral(soap, NULL, &a->%s))", p->sym->name);
	    /*} else if (is_string(p->info.typ)) {
	      fprintf(fout,"\n\t\t\t\tif (soap_in_string(soap, NULL, &a->%s, \"%s\"))", p->sym->name,xsi_type(p->info.typ));
	    } else if (is_wstring(p->info.typ)) {
	      fprintf(fout,"\n\t\t\t\tif (soap_in_wstring(soap, NULL, &a->%s, \"%s\"))", p->sym->name,xsi_type(p->info.typ));
	    */} else if(p->info.typ->type==Tarray) {
	      fprintf(fout,"\n\t\t\t\tif (soap_in_%s(soap, NULL, a->%s, \"%s\"))", c_ident(p->info.typ),p->sym->name,xsi_type(p->info.typ));
	    } else if(p->info.typ->type==Tclass) {
	      fprintf(fout,"\n\t\t\t\tif (a->%s.soap_in(soap, NULL, \"%s\"))", p->sym->name,xsi_type(p->info.typ));
	    } else if (p->info.typ->type != Tfun && !is_void(p->info.typ)) {
	      fprintf(fout,"\n\t\t\t\tif (soap_in_%s(soap, NULL, &a->%s, \"%s\"))", c_ident(p->info.typ),p->sym->name,xsi_type(p->info.typ));
	    }
	   }
	   else
	   {
	    if (is_XML(p->info.typ) && is_string(p->info.typ)) {
	      fprintf(fout,"\n\t\t\t\tif (soap_inliteral(soap, \"%s\", &a->%s))", ns_convert(p->sym->name), p->sym->name);
	    } else if (is_XML(p->info.typ) && is_wstring(p->info.typ)) {
	      fprintf(fout,"\n\t\t\t\tif (soap_inwliteral(soap, \"%s\", &a->%s))", ns_convert(p->sym->name), p->sym->name);
	    /*} else if (is_string(p->info.typ)) {
	      fprintf(fout,"\n\t\t\t\tif (soap_in_string(soap, \"%s\", &a->%s, \"%s\"))", ns_convert(p->sym->name),p->sym->name,xsi_type(p->info.typ));
	    } else if (is_wstring(p->info.typ)) {
	      fprintf(fout,"\n\t\t\t\tif (soap_in_wstring(soap, \"%s\", &a->%s, \"%s\"))", ns_convert(p->sym->name),p->sym->name,xsi_type(p->info.typ));
	    */} else if(p->info.typ->type==Tarray) {
	      fprintf(fout,"\n\t\t\t\tif (soap_in_%s(soap, \"%s\", a->%s, \"%s\"))", c_ident(p->info.typ),ns_convert(p->sym->name),p->sym->name,xsi_type(p->info.typ));
	    } else if(p->info.typ->type==Tclass) {
	      fprintf(fout,"\n\t\t\t\tif (a->%s.soap_in(soap, \"%s\", \"%s\"))", p->sym->name,ns_convert(p->sym->name),xsi_type(p->info.typ));
	    } else if (p->info.typ->type != Tfun && !is_void(p->info.typ)) {
	      fprintf(fout,"\n\t\t\t\tif (soap_in_%s(soap, \"%s\", &a->%s, \"%s\"))", c_ident(p->info.typ),ns_convert(p->sym->name),p->sym->name,xsi_type(p->info.typ));
	    }
	   }
	    if (!is_primclass(typ) && p->info.typ->type != Tfun && !is_void(p->info.typ))
	    { fprintf(fout,"\n\t\t\t\t{\tsoap_flag_%s = 0;", p->sym->name);
	      fprintf(fout,"\n\t\t\t\t\tcontinue;");
	      fprintf(fout,"\n\t\t\t\t}");
	    }
	fflush(fout);
	}
/*	  
	  if(a==0) 
	    {
	      if (is_string(p->info.typ))
		fprintf(fout,"\n\t\t\tif (!soap_in_string(\"%s\", &a->%s, \"%s\"))",
			ns_convert(p->sym->name),p->sym->name,xsi_type(p->info.typ));
	      else if(p->info.typ->type==Tarray)
		fprintf(fout,"\n\t\t\tif (!soap_in_%s(\"%s\", a->%s, \"%s\"))",
			c_ident(p->info.typ),ns_convert(p->sym->name),p->sym->name,xsi_type(p->info.typ));
	      else if(p->info.typ->type==Tclass) {
		fprintf(fout,"\n\t\t\tif (!(a->%s.soap_in(\"%s\", \"%s\")))",
			p->sym->name,ns_convert(p->sym->name),xsi_type(p->info.typ));
	      }
	  	else if (p->info.typ->type != Tfun && !is_void(p->info.typ))
		fprintf(fout,"\n\t\t\tif (!soap_in_%s(\"%s\", &a->%s, \"%s\"))",c_ident(p->info.typ),ns_convert(p->sym->name),p->sym->name, xsi_type(p->info.typ));
	      a=1;
	    }
	  else
	    {
	      if (is_string(p->info.typ))
		fprintf(fout,"\n\t\t\tif (soap_error == SOAP_TAG_MISMATCH && !soap_in_string(\"%s\", &a->%s, \"%s\"))",
		ns_convert(p->sym->name),p->sym->name,xsi_type(p->info.typ));
	      else if(p->info.typ->type==Tarray)
		fprintf(fout,"\n\t\t\tif (soap_error == SOAP_TAG_MISMATCH && !soap_in_%s(\"%s\", a->%s, \"%s\"))",
		c_ident(p->info.typ),ns_convert(p->sym->name),p->sym->name,xsi_type(p->info.typ));
	      else if(p->info.typ->type==Tclass) {
		fprintf(fout,"\n\t\t\tif (soap_error == SOAP_TAG_MISMATCH && !(a->%s.soap_in(\"%s\", \"%s\")))",
		p->sym->name,ns_convert(p->sym->name),p->sym->name,xsi_type(p->info.typ));
	      }
	  	else if (p->info.typ->type != Tfun && !is_void(p->info.typ))
		fprintf(fout,"\n\t\t\tif (soap_error == SOAP_TAG_MISMATCH && !soap_in_%s(\"%s\", &a->%s, \"%s\"))",c_ident(p->info.typ),ns_convert(p->sym->name),p->sym->name, xsi_type(p->info.typ));
	      a=1;
	    }
        }
*/
      }
      if (!is_primclass(typ))
      { fprintf(fout,"\n\t\t\tif (soap->error == SOAP_TAG_MISMATCH)");
        fprintf(fout,"\n\t\t\t\tsoap->error = soap_ignore_element(soap);");
        fprintf(fout,"\n\t\t\tif (soap->error == SOAP_NO_TAG)");
        fprintf(fout,"\n\t\t\t\tbreak;");
      }
      else
        fprintf(fout,"\n\t\t\t\t;");
      fprintf(fout,"\n\t\t\tif (soap->error)\n\t\t\t{\treturn NULL;\n\t\t\t}");
      fprintf(fout,"\n\t\t}");
      if (!is_primclass(typ))
        fprintf(fout,"\n\t\tif (soap_element_end_in(soap, tag))\n\t\t\treturn NULL;");
      
      fprintf(fout,"\n\t}");
      if (!is_primclass(typ))
      { fprintf(fout,"\n\telse\n\t{\ta = (%s)soap_id_forward(soap, soap->href, (void**)soap_id_enter(soap, soap->id, a, SOAP_%s, sizeof(%s), 0), SOAP_%s, sizeof(%s));",c_type_id(typ, "*"), c_ident(typ), c_type(typ), c_ident(typ), c_type(typ));
        fprintf(fout,"\n\t\tif (soap->alloced)\n\t\t\tsoap_default_%s(soap, a);",c_ident(typ));
        fprintf(fout,"\n\t\tif (soap->body && soap_element_end_in(soap, tag))\n\t\t\treturn NULL;");
      }
      fprintf(fout,"\n\t}");
      fprintf(fout, "\n\treturn a;\n}");
      break;
    
     case Tclass:

       /* CLASS ( Dynamic binding ) */
       /*       fprintf(fhead,"\nvirtual %s * %s::soap_in(char *);",
	      ((Table *) typ->ref)->sym->name ,c_type(typ),c_ident(typ));*/

      /* CLASS ( Static binding ) */
      /* Used on the receiving side if we are expecting a pointer to a Class
	 and memory has not been allocated for the Class ( that the pointer
	 points to ) before the get routine was called */ 
      /* Different from the dynamic binding method only until the object
	 pointed to by the pointer is allocated. */
       
      fprintf(fhead,"\nSOAP_FMAC1 %s SOAP_FMAC2 soap_in_%s(struct soap*, const char*, %s, const char*);", c_type_id(typ, "*"),c_ident(typ),c_type_id(typ, "*"));
      if (typ->transient < 0)
        return;
       fprintf(fout,"\n\nvoid *%s::soap_in(struct soap *soap, const char *tag, const char *type)", c_type(typ));
       fprintf(fout,"\n{\treturn soap_in_%s(soap, tag, this, type);\n}",c_ident(typ));
	fflush(fout);
      fprintf(fout,"\n\nSOAP_FMAC1 %s SOAP_FMAC2 soap_in_%s(struct soap *soap, const char *tag, %s, const char *type)\n{", c_type_id(typ, "*"),c_ident(typ),c_type_id(typ, "*a"));
      /*fprintf(fout,"\n\tvoid *p = soap->alist;");
      fprintf(fout,"\n\tshort f;");
      */
      if (is_primclass(typ))
      {
        fprintf(fout, "\n\tif (soap_peek_element(soap))\n\t\treturn NULL;");
        fprintf(fout,"\n\t{\tif (!(a = (%s)soap_class_id_enter(soap, soap->id, a, SOAP_%s, soap->type, NULL)))\n\t\t{\tsoap->error = SOAP_TAG_MISMATCH;\n\t\t\treturn NULL;\n\t\t}", c_type_id(typ, "*"), c_ident(typ));
        fprintf(fout,"\n\t\t*soap->id = '\\0';");
        fprintf(fout,"\n\t\tif (soap->alloced)");
        fprintf(fout,"\n\t\t{\ta->soap_default(soap);",c_ident(typ));
        fprintf(fout,"\n\t\t\tif (soap->clist->type != SOAP_%s)", c_ident(typ));
        fprintf(fout,"\n\t\t\t\treturn (%s)a->soap_in(soap, tag, type);", c_type_id(typ, "*"));
        fprintf(fout,"\n\t\t}");

      fflush(fout);
	for (table = (Table*)typ->ref; table; table = table->prev)
	{ p = table->list;
	  if (p && !strcmp(p->sym->name, "__item"))
	    break;
        }
	  if (p->info.sto & Sconst)
	    fprintf(fout, "\n\t\t\t/* const %s skipped */", p->sym->name);
	  else if (is_transient(p->info.typ))
	    fprintf(fout, "\n\t\t\t/* transient %s skipped */", p->sym->name);
	  else
	  {
	    if (is_XML(p->info.typ) && is_string(p->info.typ))
	    { fprintf(fout,"\n\t\tsoap_inliteral(soap, tag, &(((%s*)a)->%s), \"%s\");", table->sym->name,p->sym->name,xsi_type(typ));
	    }
	    else if (is_XML(p->info.typ) && is_wstring(p->info.typ))
	    { fprintf(fout,"\n\t\tsoap_inwliteral(soap, tag, &(((%s*)a)->%s), \"%s\");", table->sym->name,p->sym->name,xsi_type(typ));
	    }
	    else if(p->info.typ->type==Tarray) {
	      fprintf(fout,"\n\t\tsoap_in_%s(soap, tag, ((%s*)a)->%s, \"%s\");",
		      c_ident(p->info.typ),table->sym->name,p->sym->name,xsi_type(typ));
	    }
	    else if(p->info.typ->type==Tclass) {
	      /*CLASS IN CLASS */
	      fprintf(fout,"\n\t\t(((%s*)a)->%s).soap_in(soap, tag, \"%s\");",
		     table->sym->name,p->sym->name,xsi_type(p->info.typ));
	    }
	    else if (p->info.typ->type != Tfun && !is_void(p->info.typ)) {
	      fprintf(fout,"\n\t\tif (soap_in_%s(soap, tag, &(((%s*)a)->%s), \"%s\"))",
		      c_ident(p->info.typ),table->sym->name,p->sym->name,xsi_type(typ));
	    }
            /*fprintf(fout,"\n\t\tif (soap->error)\n\t\t\t{\tif (f)\n\t\t\t\t\tdelete a;\n\t\t\t\tsoap_dealloc(soap, * p);\n\t\t\t\treturn NULL;\n\t\t\t}");*/
           fprintf(fout,"\n\t\tif (soap->error)\n\t\t\treturn NULL;");
	  }
      }
      else
      { fprintf(fout,"\n\tif (soap_element_begin_in(soap, tag))\n\t\treturn NULL;");
        fprintf(fout,"\n\tif (soap->null)\n\t\tif (soap->enable_null)");
        fprintf(fout,"\n\t\t{\tsoap->error = SOAP_NULL;");
        fprintf(fout,"\n\t\t\treturn NULL;");
        fprintf(fout,"\n\t\t}\telse");
        fprintf(fout,"\n\t\t\treturn a;");
        fprintf(fout,"\n\tif (soap->body && !*soap->href)");
        fprintf(fout,"\n\t{\tif (!(a = (%s)soap_class_id_enter(soap, soap->id, a, SOAP_%s, soap->type, soap->arrayType)))\n\t\t{\tsoap->error = SOAP_TAG_MISMATCH;\n\t\t\treturn NULL;\n\t\t}", c_type_id(typ, "*"), c_ident(typ));
        fprintf(fout,"\n\t\tif (soap->alloced)");
        fprintf(fout,"\n\t\t{\ta->soap_default(soap);",c_ident(typ));
        fprintf(fout,"\n\t\t\tif (soap->clist->type != SOAP_%s)", c_ident(typ));
        fprintf(fout,"\n\t\t\t{\tsoap_revert(soap);");
        fprintf(fout,"\n\t\t\t\t*soap->id = '\\0';");
        fprintf(fout,"\n\t\t\t\treturn (%s)a->soap_in(soap, tag, type);", c_type_id(typ, "*"));
        fprintf(fout,"\n\t\t\t}\n\t\t}");
        fflush(fout);
       
      i=0;
      table=(Table *)typ->ref;
      /* Get the depth of the inheritance hierarchy */
      for (t = table; t; t = t->prev)
	i++;
      
      /* Declaring the flags to be used later */
      a=0;
      for(;i>0;i--){
	t = table;
	for(j=0;j<i-1;j++){
	  t = t->prev;
	}
	{for (p = t->list; p != (Entry*) 0; p = p->next)
	  { if (!(p->info.sto & Sconst) && p->info.typ->type != Tfun && !is_void(p->info.typ) && !is_transient(p->info.typ) && !is_repetition(p))
	    if(a==0)
	    { fprintf(fout,"\n\t\tshort soap_flag_%s%d = 1", p->sym->name, i );
	      a = 1;
            }
	    else
	      fprintf(fout,", soap_flag_%s%d = 1", p->sym->name, i );
	  }
        }
      }
      fprintf(fout,";"); 
      fflush(fout);
      fprintf(fout,"\n\t\tfor(;;)");
      fprintf(fout,"\n\t\t{");
        fprintf(fout,"\n\t\t\tsoap->error = SOAP_TAG_MISMATCH;\n"); 
	fflush(fout);
      table=(Table *)typ->ref;
      a=0;
      
      i=0;
      /* Get the depth of the inheritance hierarchy */
      for (t = table; t != (Table *) 0; t = t->prev) { 
	/*if(t==(Table *) 0 ) return;*/
	i++;
      }
      
      /* Call routines to output the member data of the class */
      /* Data members of the Base Classes are outputed first
	 followed by the data members of the Derived classes.
	 Over written data members are outputed twice once for the base class
	 they are defined in and once for the derived class that overwrites
	 them */
      
       
      for(;i>0;i--){
	t = table;
	for(j=0;j<i-1;j++){
	  t = t->prev;
	}
	
	for (p = t->list; p != (Entry*) 0; p = p->next)
	  if (p->info.sto & Sconst)
	    fprintf(fout, "\n\t\t\t/* const %s skipped */", p->sym->name);
	  else if (is_transient(p->info.typ))
	    fprintf(fout, "\n\t\t\t/* transient %s skipped */", p->sym->name);
	  else if (is_repetition(p))
	  { 
    fprintf(fout,"\n\t\t\tif (soap_flag_%s%d && soap->error == SOAP_TAG_MISMATCH)",p->next->sym->name,i);
    fprintf(fout,"\n\t\t\t{\t%s;\n\t\t\t\tsoap_new_block(soap);", c_type_id(p->next->info.typ, "p"));
    fprintf(fout,"\n\t\t\t\tfor (((%s*)a)->%s = 0; ; ((%s*)a)->%s++)", t->sym->name, p->sym->name, t->sym->name, p->sym->name);
    fprintf(fout,"\n\t\t\t\t{\tp = (%s)soap_push_block(soap, sizeof(%s));\n\t\t\t\t\tif (!p)\n\t\t\t\t\t\treturn NULL;", c_type(p->next->info.typ), c_type(p->next->info.typ->ref));
    if (((Tnode*)p->next->info.typ->ref)->type == Tclass)
      fprintf(fout,"\n\t\t\t\t\tp->%s::soap_default(soap);", c_type(p->next->info.typ->ref));
    else
      fprintf(fout,"\n\t\t\t\t\tsoap_default_%s(soap, p);", c_ident(p->next->info.typ->ref));
    fprintf(fout,"\n\t\t\t\t\tif (!soap_in_%s(soap, \"%s\", p, \"%s\"))", c_ident(p->next->info.typ->ref), ns_overridden(t, p->next), xsi_type(p->next->info.typ->ref));
    fprintf(fout,"\n\t\t\t\t\t\tbreak;");
    fprintf(fout,"\n\t\t\t\t}");
    fprintf(fout,"\n\t\t\t\tsoap_pop_block(soap);");
    if (((Tnode*)p->next->info.typ->ref)->type == Tclass)
      fprintf(fout,"\n\t\t\t\tif (soap->blist->size)\n\t\t\t\t\t((%s*)a)->%s = soap_instantiate_%s(soap, soap->blist->size/sizeof(%s), NULL, NULL);\n\t\t\t\telse\n\t\t\t\t\t((%s*)a)->%s = NULL;", t->sym->name, p->next->sym->name, c_type(p->next->info.typ->ref), c_type(p->next->info.typ->ref), t->sym->name, p->next->sym->name);
    else
      fprintf(fout,"\n\t\t\t\t((%s*)a)->%s = (%s)soap_malloc(soap, soap->blist->size);", t->sym->name, p->next->sym->name, c_type(p->next->info.typ));
    fprintf(fout,"\n\t\t\t\tsoap_store_block(soap, (char*)((%s*)a)->%s);", t->sym->name, p->next->sym->name);
    fprintf(fout,"\n\t\t\t\tsoap_flag_%s%d = 0;", p->next->sym->name,i);
    fprintf(fout,"\n\t\t\t\tif (soap->error == SOAP_TAG_MISMATCH)\n\t\t\t\t\tcontinue;\n\t\t\t}");
          p = p->next;
	  }
	  else
	  {
	    if (!is_primclass(typ) && p->info.typ->type != Tfun && !is_void(p->info.typ))
	      fprintf(fout,"\n\t\t\tif (soap_flag_%s%d && soap->error == SOAP_TAG_MISMATCH)",p->sym->name,i);
	   if (is_unmatched(p->sym))
	   { 
	    if (is_XML(p->info.typ) && is_string(p->info.typ)) {
	      fprintf(fout,"\n\t\t\t\tif (soap_inliteral(soap, NULL, &(((%s*)a)->%s))", t->sym->name, p->sym->name);
	    } else if (is_XML(p->info.typ) && is_wstring(p->info.typ)) {
	      fprintf(fout,"\n\t\t\t\tif (soap_inwliteral(soap, NULL, &(((%s*)a)->%s))", t->sym->name, p->sym->name);
	    }
	    /*else if (is_string(p->info.typ)) {
	      fprintf(fout,"\n\t\t\t\tif (soap_in_string(soap, NULL, &(((%s*)a)->%s), \"%s\"))", t->sym->name,p->sym->name,xsi_type(p->info.typ));
	    } else if (is_wstring(p->info.typ))
	    { fprintf(fout,"\n\t\t\t\tif (soap_in_wstring(soap, NULL, &(((%s*)a)->%s), \"%s\"))", t->sym->name,p->sym->name,xsi_type(p->info.typ));
	    }*/
	    else if(p->info.typ->type==Tarray) {
	      fprintf(fout,"\n\t\t\t\tif (soap_in_%s(soap, NULL, ((%s*)a)->%s, \"%s\"))",
		      c_ident(p->info.typ),t->sym->name,p->sym->name,xsi_type(p->info.typ));
	    } else if(p->info.typ->type==Tclass) {
	      /*CLASS IN CLASS */
	      fprintf(fout,"\n\t\t\t\tif ((((%s*)a)->%s).soap_in(soap, NULL, \"%s\"))",
		     t->sym->name,p->sym->name,xsi_type(p->info.typ));
	    } else if (p->info.typ->type != Tfun && !is_void(p->info.typ)) {
	      fprintf(fout,"\n\t\t\t\tif (soap_in_%s(soap, NULL, &(((%s*)a)->%s), \"%s\"))",
		      c_ident(p->info.typ),t->sym->name,p->sym->name,xsi_type(p->info.typ));
	    }
           }
	   else
	   { 
	    if (is_XML(p->info.typ) && is_string(p->info.typ)) {
	      fprintf(fout,"\n\t\t\t\tif (soap_inliteral(soap, \"%s\", &(((%s*)a)->%s))", ns_overridden(t, p), t->sym->name,p->sym->name);
	    } else if (is_XML(p->info.typ) && is_wstring(p->info.typ)) {
	      fprintf(fout,"\n\t\t\t\tif (soap_inwliteral(soap, \"%s\", &(((%s*)a)->%s))", ns_overridden(t, p), t->sym->name,p->sym->name);
	    }/*
	    else if (is_string(p->info.typ)) {
	      fprintf(fout,"\n\t\t\t\tif (soap_in_string(soap, \"%s\", &(((%s*)a)->%s), \"%s\"))", ns_overridden(t, p),t->sym->name,p->sym->name,xsi_type(p->info.typ));
	    } else if (is_wstring(p->info.typ))
	    { fprintf(fout,"\n\t\t\t\tif (soap_in_wstring(soap, \"%s\", &(((%s*)a)->%s), \"%s\"))", ns_overridden(t, p),t->sym->name,p->sym->name,xsi_type(p->info.typ));
	    }
	    */
	    else if(p->info.typ->type==Tarray) {
	      fprintf(fout,"\n\t\t\t\tif (soap_in_%s(soap, \"%s\", ((%s*)a)->%s, \"%s\"))", c_ident(p->info.typ),ns_overridden(t, p),t->sym->name,p->sym->name,xsi_type(p->info.typ));
	    } else if(p->info.typ->type==Tclass) {
	      /*CLASS IN CLASS */
	      fprintf(fout,"\n\t\t\t\tif ((((%s*)a)->%s).soap_in(soap, \"%s\", \"%s\"))", t->sym->name,p->sym->name,ns_overridden(t, p),xsi_type(p->info.typ));
	    } else if (p->info.typ->type != Tfun && !is_void(p->info.typ)) {
	      fprintf(fout,"\n\t\t\t\tif (soap_in_%s(soap, \"%s\", &(((%s*)a)->%s), \"%s\"))", c_ident(p->info.typ),ns_overridden(t, p),t->sym->name,p->sym->name,xsi_type(p->info.typ));
	    }
           }
	    a=1;
	    	    
	    if (!is_primclass(typ) && p->info.typ->type != Tfun && !is_void(p->info.typ))
	    { fprintf(fout,"\n\t\t\t\t{\tsoap_flag_%s%d = 0;", p->sym->name, i);
	      fprintf(fout,"\n\t\t\t\t\tcontinue;");
	      fprintf(fout,"\n\t\t\t\t}\n");
	    }
	fflush(fout);
	  }
	
      }
        fprintf(fout,"\n\t\t\tif (soap->error == SOAP_TAG_MISMATCH)");
        fprintf(fout,"\n\t\t\t\tsoap->error = soap_ignore_element(soap);");
        fprintf(fout,"\n\t\t\tif (soap->error == SOAP_NO_TAG)");
        fprintf(fout,"\n\t\t\t\tbreak;");
      fprintf(fout,"\n\t\t\tif (soap->error)\n\t\t\treturn NULL;");
      fprintf(fout,"\n\t\t}");
      fprintf(fout, "\n\t\tif (soap_element_end_in(soap, tag))\n\t\t\treturn NULL;");
      
      fprintf(fout,"\n\t}\n\telse\n\t{\ta = (%s)soap_id_forward(soap, soap->href, (void**)soap_class_id_enter(soap, soap->id, a, SOAP_%s, soap->type, soap->arrayType), SOAP_%s, sizeof(%s));",c_type_id(typ, "*"), c_ident(typ), c_ident(typ), c_type(typ));
      fprintf(fout,"\n\t\tif (soap->alloced)\n\t\t\ta->soap_default(soap);",c_ident(typ));
      fprintf(fout, "\n\t\tif (soap->body && soap_element_end_in(soap, tag))\n\t\t\treturn NULL;");
      }
      fprintf(fout, "\n\t}");
      fprintf(fout,"\n\treturn a;\n}");

      break;   
           
    case Tpointer:
      
      fprintf(fhead,"\nSOAP_FMAC1 %s SOAP_FMAC2 soap_in_%s(struct soap*, const char*, %s, const char*);", c_type_id(typ, "*"),c_ident(typ),c_type_id(typ, "*"));
      if (typ->transient < 0)
        return;
      fprintf(fout,"\n\nSOAP_FMAC1 %s SOAP_FMAC2 soap_in_%s(struct soap *soap, const char *tag, %s, const char *type)\n{", c_type_id(typ, "*"),c_ident(typ),c_type_id(typ, "*a"));
      if (is_primclass(typ))
      { if(((Tnode *) typ->ref)->type == Tclass)
	{ fprintf(fout, "\n\ta = (%s)soap_id_enter(soap, \"\", a, SOAP_%s, sizeof(%s), 0);", c_type_id(typ, "*"), c_ident(typ), c_type(typ));
	  fprintf(fout, "\n\tif (a)");
	  fprintf(fout, "\n\t{\tif (soap->alloced || !*a)");
	  fprintf(fout, "\n\t\t{\tif (soap_peek_element(soap))\n\t\t\t\treturn NULL;");
	  fprintf(fout, "\n\t\t\t*a = (%s)soap_instantiate_%s(soap, -1, soap->type, NULL);\n\t\t}", c_type(typ), c_ident(typ->ref));
	  fprintf(fout, "\n\t\tif (!*a)");
	  fprintf(fout, "\n\t\t\ta = NULL;");
	  fprintf(fout, "\n\t\telse");
	  fprintf(fout, "\n\t\t{\t(*a)->soap_default(soap);");
	  fprintf(fout, "\n\t\t\tif (!(*a)->soap_in(soap, tag, NULL))"); 
	  fprintf(fout, "\n\t\t\t\ta = NULL;");
	  fprintf(fout, "\n\t\t}");
	  fprintf(fout, "\n\t}");
	  fprintf(fout, "\n\treturn a;\n}");
	}
	else
        { fprintf(fout, "\n\ta = (%s)soap_id_enter(soap, \"\", a, SOAP_%s, sizeof(%s), 1);", c_type_id(typ, "*"), c_ident(typ), c_type(typ));
	  fprintf(fout, "\n\tif (a)");
	  fprintf(fout, "\n\t\tif (!soap_in_%s(soap, tag, *a, NULL))\n\t\t\ta = NULL;", c_ident(typ->ref));
	  fprintf(fout, "\n\treturn a;\n}");
	}
        break;
      }
      if(((Tnode *) typ->ref)->type != Tclass)
        fprintf(fout,"\n\t%s;",c_type_id(typ, "p"));
      fprintf(fout,"\n\tif (soap_element_begin_in(soap, tag))");
      fprintf(fout,"\n\t\treturn NULL;");

      if(((Tnode *) typ->ref)->type == Tclass){
	fprintf(fout,"\n\tif (soap->null)");
	fprintf(fout,"\n\t{\ta = (%s)soap_id_enter(soap, soap->id, a, SOAP_%s, sizeof(%s), %d);", c_type_id(typ, "*"), c_ident(typ), c_type(typ), reflevel(typ));
        fprintf(fout,"\n\t\tif (a)");
	fprintf(fout,"\n\t\t\t*a = NULL;");
	fprintf(fout,"\n\t\tif (soap->body && soap_element_end_in(soap, tag))\n\t\t\treturn NULL;");
	fprintf(fout,"\n\t}\n\telse if (soap->body && !*soap->href)");
	/*fprintf(fout,"\n\t{    if(soap_match_tag(soap_type, \"%s\")==0)",soap_param(typ));
	fprintf(fout,"\n\t     {     a=(%s *)soap_id_enter(soap_id,a,sizeof(%s));",
		c_type(typ),c_type(typ));
	fprintf(fout,"\n\t           *a=NULL;");
	

	fprintf(fout,"\n\t     soap_peeked = 1;\n\t   soap_level--;");
	fprintf(fout,"\n\t           if(p->soap_in(tag,*a))",
		c_ident(typ->ref));
	fprintf(fout,"\n\t                *a=p;");
	fprintf(fout,"\n\t           else    a=NULL;");
	fprintf(fout,"\n\t           soap_element_end_in(tag);");

	fprintf(fout,"\n\t}\n\telse\n\t{");

	fprintf(fout,"\n\t     if((p = (%s *) soap_class_id_enter(soap_id, NULL,\"%s\",soap_type))==NULL)",
		c_type(typ->ref), c_type(typ->ref));
	fprintf(fout,"\n\t      return NULL;");
	*/
	fprintf(fout,"\n\t{\tsoap_revert(soap);");
	fprintf(fout, "\n\t\ta = (%s)soap_id_enter(soap, \"\", a, SOAP_%s, sizeof(%s), 0);", c_type_id(typ, "*"), c_ident(typ), c_type(typ));
	fprintf(fout, "\n\t\tif (a)");
	fprintf(fout, "\n\t\t{\tif (soap->alloced || !*a)");
	fprintf(fout, "\n\t\t\t\t*a = (%s)soap_instantiate_%s(soap, -1, soap->type, soap->arrayType);", c_type(typ), c_ident(typ->ref));
	fprintf(fout, "\n\t\t\tif (!*a)");
	fprintf(fout, "\n\t\t\t\ta = NULL;");
	fprintf(fout, "\n\t\t\telse");
	fprintf(fout, "\n\t\t\t{\t(*a)->soap_default(soap);");
	fprintf(fout, "\n\t\t\t\tif (!(*a)->soap_in(soap, tag, NULL))"); 
	fprintf(fout, "\n\t\t\t\t\ta = NULL;");
	fprintf(fout, "\n\t\t\t}");
	fprintf(fout, "\n\t\t}");
	/*
	fprintf(fout,
		"\n\t{\tsoap_peeked = 1;\
		\n\t\tsoap_level--;\
		\n\t\ta = (%s)soap_id_enter(\"\", a, SOAP_%s, sizeof(%s), 0);\
		\n\t\tif (a)\
		\n\t\t{\tif (soap_alloced || !*a)\
		\n\t\t\t\t*a = (%s)soap_instantiate_%s(-1, soap_type);\
		\n\t\t\tif (!*a)\
		\n\t\t\t\ta = NULL;\
		\n\t\t\telse\
		\n\t\t\t{\t(*a)->soap_default();\
		\n\t\t\t\tif (!(*a)->soap_in(tag, \"%s\"))\
		\n\t\t\t\t\ta = NULL;\
		\n\t\t\t}\
		\n\t\t}", c_type_id(typ, "*"), c_ident(typ), c_type(typ), c_type(typ), c_ident(typ->ref), xsi_type(typ->ref));
		*/
	/*
        fprintf(fout,"\n\t      if(soap_alloced)");
	fprintf(fout,"\n\t p->soap_default();",c_ident(typ->ref));
	fprintf(fout,"\n\t     if(p->soap_in(tag)==NULL)");
	  fprintf(fout,"\n\t      return NULL;");
	 
	fprintf(fout,"\n\t     if(a){");
	fprintf(fout,"\n\t           *a = p;\n\t }");
	fprintf(fout,"\n\t     else ");
	fprintf(fout,"\n\t       {a=(%s*)soap_id_enter(\"\",NULL,sizeof(%s));",
		c_type(typ),c_type(typ));
	fprintf(fout,"\n\t             *a=p;");
	fprintf(fout,"\n\t       }");

	
	/*fprintf(fout,"\n\t     if(a)\n\t       {if(p=soap_in_%s(tag,*a))",
		c_ident(typ->ref));
	fprintf(fout,"\n\t               *a = p;\n\t        else a=NULL;\n\t}");
	fprintf(fout,"\n\t       else   if(p=soap_in_%s(tag,NULL))",
		c_ident(typ->ref));
	fprintf(fout,"\n\t       {     a=(%s*)soap_id_enter(\"\",NULL,sizeof(%s));",
		c_type(typ),c_type(typ));
	fprintf(fout,"\n\t             *a=p;");
	fprintf(fout,"\n\t       }");*/
	
	fprintf(fout,"\n\t}\n\telse\n\t{\ta = (%s)soap_id_lookup(soap, soap->href, (void**)soap_id_enter(soap, soap->id, a, SOAP_%s, sizeof(%s), %d), SOAP_%s, sizeof(%s), %d);", c_type_id(typ, "*"), c_ident(typ), c_type(typ), reflevel(typ), c_ident(typ->ref), c_type(typ->ref), reflevel(typ->ref) );
	fprintf(fout,"\n\tif (soap->body && soap_element_end_in(soap, tag))\n\t\treturn NULL;");
	fprintf(fout,"\n\t}\n\treturn a;\n}");
      }
      else {
	fprintf(fout,"\n\tif (soap->null)");
	fprintf(fout,"\n\t{\ta = (%s)soap_id_enter(soap, soap->id, a, SOAP_%s, sizeof(%s), %d);", c_type_id(typ, "*"), c_ident(typ), c_type(typ),reflevel(typ));
        fprintf(fout,"\n\t\tif (a)");
	fprintf(fout,"\n\t\t\t*a = NULL;");
	fprintf(fout,"\n\t\tif (soap->body && soap_element_end_in(soap, tag))\n\t\t\treturn NULL;");
	fprintf(fout,"\n\t}\n\telse if (soap->body && !*soap->href)");
	/*fprintf(fout,"\n\t{    if(soap_match_tag(soap_type, \"%s\")==0) {",soap_param(typ));
	fprintf(fout,"\n\t          a=(%s *)soap_id_enter(soap_id,a,sizeof(%s));",
		c_type(typ),c_type(typ));
	fprintf(fout,"\n\t           *a=NULL;");
	
	fprintf(fout,"\n\t     soap_peeked = 1;\n\t   soap_level--;");
	fprintf(fout,"\n\t           if(p=soap_in_%s(tag,*a))",
		c_ident(typ->ref));
	fprintf(fout,"\n\t                *a=p;");
	fprintf(fout,"\n\t           else    a=NULL;");
	fprintf(fout,"\n\t           soap_element_end_in(tag);");
	fprintf(fout,"\n\t}\n\telse\n\t{");
	*/
	fprintf(fout,"\n\t{\tsoap_revert(soap);");
	fprintf(fout,"\n\t\tif (a)\n\t\t{\tif ((p = soap_in_%s(soap, tag, *a, type)))", c_ident(typ->ref));
	fprintf(fout,"\n\t\t\t\t*a = p;\n\t\t\telse\n\t\t\t\ta = NULL;\n\t\t}");

	fprintf(fout,"\n\t\telse if ((p = soap_in_%s(soap, tag, NULL, type)))",c_ident(typ->ref));
	fprintf(fout,"\n\t\t{\ta = (%s)soap_id_enter(soap, \"\", NULL, SOAP_%s, sizeof(%s), 0);", c_type_id(typ, "*"), c_ident(typ), c_type(typ));
        fprintf(fout,"\n\t\t\tif (!a)\n\t\t\t\treturn NULL;");
	fprintf(fout,"\n\t\t\t*a = p;");
	fprintf(fout,"\n\t\t}");
	fprintf(fout,"\n\t}\n\telse\n\t{\ta = (%s)soap_id_lookup(soap, soap->href, (void**)soap_id_enter(soap, soap->id, a, SOAP_%s, sizeof(%s), %d), SOAP_%s, sizeof(%s), %d);", c_type_id(typ, "*"), c_ident(typ), c_type(typ), reflevel(typ), c_ident(typ->ref), c_type(typ->ref), reflevel(typ->ref) );
	fprintf(fout,"\n\tif (soap->body && soap_element_end_in(soap, tag))\n\t\treturn NULL;");
	fprintf(fout,"\n\t}\n\treturn a;\n}");
      }
    
      break;
  
    case Tarray:
      temp = typ;
      while(temp->type == Tarray){
	temp = temp->ref;
      }
      fprintf(fhead,"\nSOAP_FMAC1 %s SOAP_FMAC2 soap_in_%s(struct soap*, const char*, %s, const char*);",c_type_id(temp, "*"),c_ident(typ),c_type(typ));  
      if (typ->transient < 0)
        return;
      fprintf(fout,"\n\nSOAP_FMAC1 %s SOAP_FMAC2 soap_in_%s(struct soap *soap, const char *tag, %s, const char *type)\n{",c_type_id(temp, "*"),c_ident(typ),c_type_id(typ, "a"));  
      fprintf(fout,"\n\tint i;");
      fprintf(fout,"\n\tif (soap_element_begin_in(soap, tag))");	
      fprintf(fout,"\n\t   return NULL;");
      fprintf(fout,"\n\tif (soap_match_array(soap, type))");
      fprintf(fout,"\n\t{     soap->error = SOAP_TYPE_MISMATCH;\n\tsoap_revert(soap);\n\t    return NULL;\n\t}");
      fprintf(fout,"\n\tif (soap->null)\n\t\tif (soap->enable_null)");
      fprintf(fout,"\n\t\t{\tsoap->error = SOAP_NULL;\n\t\t\t\treturn NULL;\n\t\t}\n\t\telse\n\t\t\treturn (%s)a;", c_type_id(temp, "*"));
      fprintf(fout,"\n\tif (soap->body && !*soap->href)");
      fprintf(fout,"\n\t{\ta = (%s)soap_id_enter(soap, soap->id, a, SOAP_%s, sizeof(%s), 0);", c_type_id(typ->ref, "(*)"), c_ident(typ), c_type(typ));
      fprintf(fout,"\n\t      if (!a)\n\t\treturn NULL;");
      fprintf(fout,"\n\t    if (soap->alloced)");
      fprintf(fout,"\n\t         soap_default_%s(soap, a);",c_ident(typ));
      total=get_dimension(typ);  
      n=typ->ref;
      cardinality = 1;
      while(n->type==Tarray)
	{
	  total=total*get_dimension(n);
	  n = n->ref;
	  cardinality++;
	}
      fprintf(fout,"\n\t\tfor (i = 0; i < %d; i++)",get_dimension(typ));
  fprintf(fout,"\n\t\t{\tsoap_peek_element(soap);\n\t\t\tif (soap->position)\n\t\t\t{\ti = soap->positions[0];\n\t\t\t\tif (i < 0 || i >= %d)\n\t\t\t\t{\tsoap->error = SOAP_IOB;\n\t\t\t\treturn NULL;\n\t\t\t\t}\n\t\t\t}", get_dimension(typ));
	fprintf(fout,"\n\t\t\tif (!soap_in_%s(soap, NULL, a", c_ident(typ->ref));

      if(cardinality > 1){
	fprintf(fout,"[i]");
      }else {
	fprintf(fout,"+i");
      }
      fprintf(fout,", \"%s\"))", xsi_type(typ->ref));
      fprintf(fout,"\n\t\t\t\tif (soap->error == SOAP_NO_TAG)");
      fprintf(fout,"\n\t\t\t\t{\tsoap->error = SOAP_OK;");
      fprintf(fout,"\n\t\t\t\t\tbreak;\n\t\t\t\t}");
      fprintf(fout,"\n\t\t\t\telse\n\t\t\t\t\treturn NULL;");
      fprintf(fout,"\n\t\t}");
      fprintf(fout,"\n\t\tif (soap->enable_array_overflow)\n\t\t\twhile (soap_element_end_in(soap, tag) == SOAP_SYNTAX_ERROR)\n\t\t\t{\tsoap->peeked = 1;\n\t\t\t\tsoap_ignore_element(soap);\n\t\t\t}");
      fprintf(fout,"\n\t\telse if (soap_element_end_in(soap, tag))\n\t\t{\tif (soap->error == SOAP_SYNTAX_ERROR)\n\t\t\t\tsoap->error = SOAP_IOB;\n\t\t\treturn NULL;\n\t\t}");
      fprintf(fout,"\n\t}\n\telse\n\t{");
      fprintf(fout,"\n\t\ta = (%s)soap_id_forward(soap, soap->href, (void**)soap_id_enter(soap, soap->id, a, SOAP_%s, sizeof(%s), 0), SOAP_%s, sizeof(%s));", c_type_id(typ->ref, "(*)"), c_ident(typ), c_type(typ), c_ident(typ), c_type(typ));
      fprintf(fout,"\n\t\tif (soap->alloced)");
      fprintf(fout,"\n\t\t\tsoap_default_%s(soap, a);",c_ident(typ));
      fprintf(fout,"\n\t\tif (soap->body && soap_element_end_in(soap, tag))\n\t\t\treturn NULL;");
      fprintf(fout,"\n\t}\n\treturn (%s)a", c_type_id(temp, "*"));
      /*
      for(i=1;i<=cardinality;i++){
          fprintf(fout,"[0]");
      }	      
      */
      fprintf(fout,";\n}");
      break;

    case Tenum:
      fprintf(fhead,"\nSOAP_FMAC1 %s SOAP_FMAC2 soap_in_%s(struct soap*, const char*, %s, const char*);",c_type_id(typ, "*"),c_ident(typ),c_type_id(typ, "*"));  
      if (typ->transient < 0)
        return;
      fprintf(fout,"\n\nSOAP_FMAC1 %s SOAP_FMAC2 soap_in_%s(struct soap *soap, const char *tag, %s, const char *type)",c_type_id(typ, "*"),c_ident(typ),c_type_id(typ, "*a"));  
      fprintf(fout,"\n{\tchar *s;");
      if (is_mask(typ))
        fprintf(fout,"\n\tLONG64 i;");
      fprintf(fout,"\n\tif (soap_element_begin_in(soap, tag))");	
      fprintf(fout,"\n\t\treturn NULL;");
      fprintf(fout,"\n\tif (*soap->type && soap_match_tag(soap, soap->type, type))");
      fprintf(fout,"\n\t{\tsoap->error = SOAP_TYPE_MISMATCH;\n\tsoap_revert(soap);\n\t\treturn NULL;\n\t}");
      fprintf(fout,"\n\tif (soap->null)\n\t{\tif (soap->enable_null)");
      fprintf(fout,"\n\t\t{\tsoap->error = SOAP_NULL;\n\t\t\treturn NULL;\n\t\t}\n\t\telse\n\t\t\treturn a;\n\t}");
      fprintf(fout,"\n\tif (soap->body && !*soap->href)");
      fprintf(fout,"\n\t{\ta = (%s)soap_id_enter(soap, soap->id, a, SOAP_%s, sizeof(%s), 0);", c_type_id(typ, "*"), c_ident(typ), c_type(typ));
      fprintf(fout,"\n\t\tif (!a)\n\t\t\treturn NULL;");
      if (is_mask(typ))
      { fprintf(fout,"\n\t\ti = 0;\n\t\twhile (*(s = soap_value(soap)))\n\t\t\t");
        for (t = (Table*)typ->ref; t; t = t->prev)
        { for (p = t->list; p; p = p->next)
	    fprintf(fout, "if (!strcmp(s, \"%s\"))\n\t\t\t\ti |= (LONG64)%s;\n\t\t\telse ", ns_convert(p->sym->name), p->sym->name);
        }	
        fprintf(fout, "\n\t\t\t{\tsoap->error = SOAP_TYPE_MISMATCH;\n\t\t\t\treturn NULL;\n\t\t\t}");
        fprintf(fout, "\n\t\t*a = (%s)i;", c_type(typ));
      }
      else
      { fprintf(fout,"\n\t\ts = soap_value(soap);\n\t\t");
        for (t = (Table*)typ->ref; t; t = t->prev)
        { for (p = t->list; p; p = p->next)
	    fprintf(fout, "if (!strcmp(s, \"%s\"))\n\t\t\t*a = %s;\n\t\telse ", ns_convert(p->sym->name), p->sym->name);
        }	
        fprintf(fout, "\n\t\t{\t*a = (%s)strtol(s, &s, 10);\n\t\t\tif (*s)\n\t\t\t{\tsoap->error = SOAP_TYPE_MISMATCH;\n\t\t\t\treturn NULL;\n\t\t\t}\n\t\t}", c_type(typ));
      }
      fprintf(fout, "\n\t\tif (soap_element_end_in(soap, tag))\n\t\t\treturn NULL;", c_type(typ));
      fprintf(fout, "\n\t}\n\telse\n\t{\ta = (%s)soap_id_forward(soap, soap->href, (void**)soap_id_enter(soap, soap->id, a, SOAP_%s, sizeof(%s), 0), SOAP_%s, sizeof(%s));", c_type_id(typ, "*"), c_ident(typ), c_type(typ), c_ident(typ), c_type(typ));
      fprintf(fout,"\n\t\tif (soap->alloced)\n\t\t\tsoap_default_%s(soap, a);",c_ident(typ));
      fprintf(fout, "\n\t\tif (soap->body && soap_element_end_in(soap, tag))\n\t\t\treturn NULL;");
      fprintf(fout,"\n\t}\n\treturn a;\n}");
      break;

    default: break;
    }
    fflush(fout);
}


void
soap_in_Darray(Tnode *typ)
{ int i, j, d;
  Entry *p, *q;
  Table *table;

  table=(Table *)typ->ref;
  q = table->list;
  p = is_dynamic_array(typ);
  d = get_Darraydims(typ);
  
  fprintf(fhead,"\nSOAP_FMAC1 %s SOAP_FMAC2 soap_in_%s(struct soap*, const char*, %s, const char*);", c_type_id(typ, "*"),c_ident(typ),c_type_id(typ, "*"));
      if (typ->transient < 0)
        return;
  if (typ->type == Tclass)
  { fprintf(fout,"\n\nvoid *%s::soap_in(struct soap *soap, const char *tag, const char *type)", c_type(typ));
    fprintf(fout,"\n{\treturn soap_in_%s(soap, tag, this, type);\n}", c_ident(typ));
  }
  fflush(fout);
  fprintf(fout,"\n\nSOAP_FMAC1 %s SOAP_FMAC2 soap_in_%s(struct soap *soap, const char *tag, %s, const char *type)", c_type_id(typ, "*"),c_ident(typ),c_type_id(typ, "*a"));
  if (is_hexBinary(typ))
    fprintf(fout,"\n{\tint i, n;\n\tchar *s;");
  else if (is_binary(typ))
    fprintf(fout,"\n{");
  else if (d)
    fprintf(fout,"\n{\tint i, j, n;\n\t%s;", c_type_id(p->info.typ, "p"));
  else
    fprintf(fout,"\n{\tint i, j;\n\t%s;", c_type_id(p->info.typ, "p"));
  fprintf(fout,"\n\tif (soap_element_begin_in(soap, tag))\n\t\treturn NULL;");
  if (has_ns(typ) || is_untyped(typ))
    if (is_hexBinary(typ))
      fprintf(fout,"\n\tif (*soap->type && soap_match_tag(soap, soap->type, type))");
    else if (is_binary(typ))
      fprintf(fout,"\n\tif (*soap->type && soap_match_tag(soap, soap->type, type) && soap_match_tag(soap, soap->type, \"SOAP-ENC:base64\") && soap_match_tag(soap, soap->type, \"SOAP-ENC:base64Binary\"))");
    else
      fprintf(fout,"\n\tif (*soap->type && soap_match_tag(soap, soap->type, type))");
  else
    fprintf(fout,"\n\tif (soap_match_array(soap, type))");
  fprintf(fout,"\n\t{\tsoap->error = SOAP_TYPE_MISMATCH;\n\t\tsoap_revert(soap);\n\t\treturn NULL;\n\t}");
  fprintf(fout,"\n\tif (soap->null)");
  if (typ->type == Tclass)
  { fprintf(fout,"\n\t{\tif ((a = (%s)soap_class_id_enter(soap, soap->id, a, SOAP_%s, soap->type, soap->arrayType)))",c_type_id(typ, "*"), c_ident(typ)); 
    fprintf(fout,"\n\t\t\ta->soap_default(soap);");
    fprintf(fout,"\n\t\t\tif (soap->body && soap_element_end_in(soap, tag))\n\t\t\t\treturn NULL;");
    fprintf(fout,"\n\t}\n\telse if (soap->body && !*soap->href)");
    fprintf(fout,"\n\t{\tif (!(a = (%s)soap_class_id_enter(soap, soap->id, a, SOAP_%s, soap->type, soap->arrayType)))",c_type_id(typ, "*"), c_ident(typ)); 
  }
  else
  { fprintf(fout,"\n\t{\tif ((a = (%s)soap_id_enter(soap, soap->id, a, SOAP_%s, sizeof(%s), 0)))",c_type_id(typ, "*"), c_ident(typ), c_type(typ));
    fprintf(fout,"\n\t\t\tsoap_default_%s(soap, a);", c_ident(typ));
    fprintf(fout,"\n\t\t\tif (soap->body && soap_element_end_in(soap, tag))\n\t\t\t\treturn NULL;");
    fprintf(fout,"\n\t}\n\telse if (soap->body && !*soap->href)");
    fprintf(fout,"\n\t{\tif (!(a = (%s)soap_id_enter(soap, soap->id, a, SOAP_%s, sizeof(%s), 0)))",c_type_id(typ, "*"), c_ident(typ), c_type(typ));
  }
  fprintf(fout,"\n\t\t\treturn NULL;");
  fprintf(fout,"\n\t\tif (soap->alloced)");
  if (typ->type == Tclass)
    fprintf(fout,"\n\t\t\ta->soap_default(soap);");
  else
    fprintf(fout,"\n\t\t\tsoap_default_%s(soap, a);",c_ident(typ));
  
  if (is_hexBinary(typ))
  { fprintf(fout,"\n\t\tsoap_new_block(soap);");
    fprintf(fout,"\n\t\tdo\n\t\t{\tif (!(s = (char*)soap_push_block(soap, SOAP_BLKLEN)))\n\t\t\t\treturn NULL;");
    fprintf(fout,"\n\t\t\tfor (i = 0; i < SOAP_BLKLEN; i++)\n\t\t\t{\tif ((n = soap_gethex(soap)) < 0)\n\t\t\t\t\tbreak;\n\t\t\t\ts[i] = (char)n;\n\t\t\t}");
    fprintf(fout,"\n\t\t}\n\t\twhile (i >= SOAP_BLKLEN);");
    fprintf(fout,"\n\t\ta->__size = soap->blist->size = soap->blist->size - SOAP_BLKLEN + i;");
    if (mflag && typ->type == Tclass)
      fprintf(fout,"\n\t\tif (soap->blist->size)\n\t\t\ta->__ptr = (%s)malloc(soap->blist->size);\n\t\telse\n\t\t\ta->__ptr = NULL;", c_type(p->info.typ));
    else
      fprintf(fout,"\n\t\ta->__ptr = (unsigned char*)soap_malloc(soap, soap->blist->size);");
    fprintf(fout,"\n\t\tsoap_store_block(soap, (char*)a->__ptr);");
    fprintf(fout,"\n\t\tif (soap_element_end_in(soap, tag))\n\t\t\treturn NULL;");
  }
  else if (is_binary(typ))
  { if (mflag && typ->type == Tclass)
      fprintf(fout,"\n\t\ta->__ptr = soap_getbase64(soap, (size_t*)&a->__size, 1);");
    else
      fprintf(fout,"\n\t\ta->__ptr = soap_getbase64(soap, (size_t*)&a->__size, 0);");
    fprintf(fout,"\n\t\tif (soap_element_end_in(soap, tag))\n\t\t\treturn NULL;");
  }
  else
  { if (d)
    { fprintf(fout,"\n\t\tn = soap_getsizes(soap->arraySize, a->__size, %d);", d);
      if (has_offset(typ))
        fprintf(fout,"\n\t\tn -= j = soap_getoffsets(soap->offset, a->__size, a->__offset, %d);", d);
      else
        fprintf(fout,"\n\t\tn -= j = soap_getoffsets(soap->offset, a->__size, NULL, %d);", d);
      fprintf(fout,"\n\t\tif (j >= 0 && n >= 0)");
      if (((Tnode*)p->info.typ->ref)->type == Tclass)
      { fprintf(fout,"\n\t\t{\ta->%s = soap_instantiate_%s(soap, n, NULL, NULL);", p->sym->name, c_ident(p->info.typ->ref));
        fprintf(fout, "\n\t\t\tfor (i = 0; i < n; i++)\n\t\t\t\t(a->%s+i)->%s::soap_default(soap);", p->sym->name, c_type(p->info.typ->ref));
      }
      else
        { fprintf(fout,"\n\t\t{\ta->%s = (%s)soap_malloc(soap, n*sizeof(%s));", p->sym->name, c_type_id(p->info.typ->ref, "*"),  c_type(p->info.typ->ref));
        fprintf(fout, "\n\t\t\tfor (i = 0; i < n; i++)\n\t\t\t\tsoap_default_%s(soap, a->%s+i);", c_ident(p->info.typ->ref), p->sym->name);
      }
      if (has_offset(typ))
        fprintf(fout,"\n\t\t\tfor (i = 0; i < n; i++)");
      else
        fprintf(fout,"\n\t\t\tfor (i = j; i < j+n; i++)");
      fprintf(fout,"\n\t\t\t{\tsoap_peek_element(soap);\n\t\t\t\tif (soap->position == %d)", d);
      fprintf(fout,"\n\t\t\t\t{\ti = ");
	for (i = 0; i < d; i++)
	{ fprintf(fout,"soap->positions[%d]", i);
	  for (j = 1; j < d-i; j++)
	    fprintf(fout,"*a->__size[%d]", j);
	  if (i < d-1)
	    fprintf(fout,"+");
	}
	fprintf(fout,"-j;");
	fprintf(fout,"\n\t\t\t\t\tif (i < 0 || i >= n)\n\t\t\t\t\t{\tsoap->error = SOAP_IOB;\n\t\t\t\t\t\treturn NULL;\n\t\t\t\t\t}\n\t\t\t\t}");
        fprintf(fout,"\n\t\t\t\tif (!soap_in_%s(soap, NULL, a->%s+i, \"%s\"))", c_ident(p->info.typ->ref), p->sym->name, xsi_type(p->info.typ->ref));
      fprintf(fout,"\n\t\t\t\t\tif (soap->error == SOAP_NO_TAG)");
      fprintf(fout,"\n\t\t\t\t\t{\tsoap->error = SOAP_OK;");
      fprintf(fout,"\n\t\t\t\t\t\tbreak;");
      fprintf(fout,"\n\t\t\t\t\t}\n\t\t\t\t\telse\n\t\t\t\t\t\treturn NULL;");
    }
    else
    { fprintf(fout,"\n\t\ta->__size = soap_getsize(soap->arraySize, soap->offset, &j);");
      if (has_offset(typ) && (p->next->next->info.sto & Sconst) == 0)
      { fprintf(fout,"\n\t\ta->__offset = j;");
      }
      fprintf(fout,"\n\t\tif (j >= 0 && a->__size >= 0)");
      if (((Tnode*)p->info.typ->ref)->type == Tclass)
      { fprintf(fout,"\n\t\t{\ta->%s = soap_instantiate_%s(soap, a->__size, NULL, NULL);", p->sym->name, c_ident(p->info.typ->ref));
        fprintf(fout, "\n\t\t\tfor (i = 0; i < a->__size; i++)\n\t\t\t\t(a->%s+i)->%s::soap_default(soap);", p->sym->name, c_type(p->info.typ->ref));
      }
      else
        {
	if (mflag && typ->type == Tclass)
          fprintf(fout,"\n\t\t{\ta->%s = (%s)malloc(sizeof(%s) * a->__size);", p->sym->name, c_type_id(p->info.typ->ref, "*"),  c_type(p->info.typ->ref));
        else
          fprintf(fout,"\n\t\t{\ta->%s = (%s)soap_malloc(soap, sizeof(%s) * a->__size);", p->sym->name, c_type_id(p->info.typ->ref, "*"),  c_type(p->info.typ->ref));
        fprintf(fout, "\n\t\t\tfor (i = 0; i < a->__size; i++)\n\t\t\t\tsoap_default_%s(soap, a->%s+i);", c_ident(p->info.typ->ref), p->sym->name);
      }
      if (has_offset(typ))
        fprintf(fout,"\n\t\t\tfor (i = 0; i < a->__size; i++)");
      else
        fprintf(fout,"\n\t\t\tfor (i = j; i < j+a->__size; i++)");
      fprintf(fout,"\n\t\t\t{\tsoap_peek_element(soap);\n\t\t\t\tif (soap->position)\n\t\t\t\t{\ti = soap->positions[0]-j;\n\t\t\t\t\tif (i < 0 || i >= a->__size)\n\t\t\t\t\t{\tsoap->error = SOAP_IOB;\n\t\t\t\t\treturn NULL;\n\t\t\t\t\t}\n\t\t\t\t}");
        fprintf(fout,"\n\t\t\t\tif (!soap_in_%s(soap, NULL, a->%s+i, \"%s\"))", c_ident(p->info.typ->ref), p->sym->name, xsi_type(p->info.typ->ref));
      fprintf(fout,"\n\t\t\t\t\tif (soap->error == SOAP_NO_TAG)");
      fprintf(fout,"\n\t\t\t\t\t{\tsoap->error = SOAP_OK;");
      fprintf(fout,"\n\t\t\t\t\t\tbreak;");
      fprintf(fout,"\n\t\t\t\t\t}\n\t\t\t\t\telse\n\t\t\t\t\t\treturn NULL;");
    }
    fprintf(fout,"\n\t\t\t}\n\t\t}\n\t\telse");
    fprintf(fout,"\n\t\t{\tsoap_new_block(soap);");
    if (d)
      fprintf(fout,"\n\t\t\tfor (a->__size[0] = 0; ; a->__size[0]++)");
    else
      fprintf(fout,"\n\t\t\tfor (a->__size = 0; ; a->__size++)");
    fprintf(fout,"\n\t\t\t{\tp = (%s)soap_push_block(soap, sizeof(%s));\n\t\t\t\tif (!p)\n\t\t\t\t\treturn NULL;", c_type(p->info.typ), c_type(p->info.typ->ref));
    if (((Tnode*)p->info.typ->ref)->type == Tclass)
      fprintf(fout,"\n\t\t\t\tp->%s::soap_default(soap);", c_type(p->info.typ->ref));
    else
      fprintf(fout,"\n\t\t\t\tsoap_default_%s(soap, p);", c_ident(p->info.typ->ref));
      fprintf(fout,"\n\t\t\t\tif (!soap_in_%s(soap, NULL, p, \"%s\"))", c_ident(p->info.typ->ref), xsi_type(p->info.typ->ref));
    fprintf(fout,"\n\t\t\t\t\tif (soap->error == SOAP_NO_TAG)");
    fprintf(fout,"\n\t\t\t\t\t{\tsoap->error = SOAP_OK;");
    fprintf(fout,"\n\t\t\t\t\t\tbreak;");
    fprintf(fout,"\n\t\t\t\t\t}\n\t\t\t\t\telse\n\t\t\t\t\t\treturn NULL;");
    fprintf(fout,"\n\t\t\t}");
    fprintf(fout,"\n\t\t\tsoap_pop_block(soap);");
    if (((Tnode*)p->info.typ->ref)->type == Tclass)
      fprintf(fout,"\n\t\t\tif (soap->blist->size)\n\t\t\t\ta->%s = soap_instantiate_%s(soap, soap->blist->size/sizeof(%s), NULL, NULL);\n\t\t\telse\n\t\t\t\ta->%s = NULL;", p->sym->name, c_type(p->info.typ->ref), c_type(p->info.typ->ref), p->sym->name);
    else
    if (mflag && typ->type == Tclass)
      fprintf(fout,"\n\t\t\tif (soap->blist->size)\n\t\t\t\ta->%s = (%s)malloc(soap->blist->size);\n\t\t\telse\n\t\t\t\ta->%s = NULL;", p->sym->name, c_type(p->info.typ), p->sym->name);
    else
      fprintf(fout,"\n\t\t\ta->%s = (%s)soap_malloc(soap, soap->blist->size);", p->sym->name, c_type(p->info.typ));
    fprintf(fout,"\n\t\t\tsoap_store_block(soap, (char*)a->%s);", p->sym->name);
    fprintf(fout,"\n\t\t}");
    fprintf(fout,"\n\t\tif (soap_element_end_in(soap, tag))\n\t\t\treturn NULL;");
  }
  if (typ->type == Tclass)
  { fprintf(fout,"\n\t}\n\telse\n\t{\ta = (%s)soap_id_forward(soap, soap->href, (void**)soap_class_id_enter(soap, soap->id, a, SOAP_%s, soap->type, soap->arrayType), SOAP_%s, sizeof(%s));", c_type_id(typ, "*"), c_ident(typ), /*xsi_type(typ),*/ c_ident(typ), c_type(typ));
    fprintf(fout,"\n\t\tif (soap->alloced)\n\t\t\ta->soap_default(soap);");
  }
  else
  { fprintf(fout,"\n\t}\n\telse\n\t{\ta = (%s)soap_id_forward(soap, soap->href, (void**)soap_id_enter(soap, soap->id, a, SOAP_%s, sizeof(%s), 0), SOAP_%s, sizeof(%s));", c_type_id(typ, "*"), c_ident(typ), c_type(typ), c_ident(typ), c_type(typ));
    fprintf(fout,"\n\t\tif (soap->alloced)\n\t\t\tsoap_default_%s(soap, a);",c_ident(typ));
  }
  fprintf(fout,"\n\t\tif (soap->body && soap_element_end_in(soap, tag))\n\t\treturn NULL;");
  fprintf(fout,"\n\t}\n\treturn a;\n}");
}

