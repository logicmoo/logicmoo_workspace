/*	stdsoap2.h 2.1.6

The contents of this file are subject to the gSOAP Public License Version 1.0
(the "License"); you may not use this file except in compliance with the
License. You may obtain a copy of the License at
http://www.cs.fsu.edu/~engelen/soaplicense.html
Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
for the specific language governing rights and limitations under the License.

The Initial Developer of the Original Code is Robert A. van Engelen.
Copyright (C) 2000-2002 Robert A. van Engelen. All Rights Reserved.

*/

#ifdef WITH_SOAPDEFS_H
#include "soapdefs.h"
#endif

#ifndef _THREAD_SAFE
#define _THREAD_SAFE
#endif

#ifndef _REENTRANT
#define _REENTRANT
#endif

#ifndef SOAP_FMAC1
#define SOAP_FMAC1
#endif

#ifndef SOAP_FMAC2
#define SOAP_FMAC2
#endif

#ifndef SOAP_CMAC
#define SOAP_CMAC
#endif

#ifdef UNDER_CE
#ifndef WIN32
#define WIN32
#endif
#endif

#ifdef __BORLANDC__
#ifdef __WIN32__
#ifndef WIN32
#define WIN32
#endif
#endif
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <limits.h>

#ifndef UNDER_CE
#include <errno.h>
#include <sys/types.h>
#include <time.h>
#endif

#if !defined(WIN32)
#include <sys/socket.h>
#include <strings.h>		/* AIX */
#include <sys/socketvar.h>	/* AIX */
#include <sys/time.h>
#include <netinet/tcp.h>	/* for TCP_NODELAY */
#include <arpa/inet.h>
#endif

#ifdef WITH_FASTCGI
#include <fcgi_stdio.h>
#endif

#ifdef WITH_OPENSSL
#include <openssl/ssl.h>
#include <openssl/err.h>
#ifndef ALLOW_OLD_VERSIONS
#if (OPENSSL_VERSION_NUMBER < 0x00905100L)
#error "Must use OpenSSL 0.9.6 or later"
#endif
#endif
#endif

#include <math.h>	/* for isnan(): remove if NaN and INF support is not required */

#ifndef STDSOAP
#define STDSOAP

/* #define DEBUG */ /* Uncomment to debug sending (in file SENT.log) receiving (in file RECV.log) and messages (in file TEST.log) */

#ifdef __cplusplus
extern "C" {
#endif

#ifdef WIN32
#ifndef UNDER_CE
#include <io.h>
#endif
#include <winsock.h>
#else
#include <netdb.h>
#include <netinet/in.h>
#include <unistd.h>
#include <fcntl.h>
#define closesocket(n) close(n)
#endif

#ifndef _MATH_H
#define isnan(_) (0)
#else
extern struct soap_double_nan { unsigned int n1, n2; } soap_double_nan;
#endif

#ifndef WIN32
#define LONG64 long long
#define ULONG64 unsigned LONG64
#endif

#ifdef __BORLANDC__
#ifdef SYSMAC_H
#define LONG64 ::LONG64
#define ULONG64 ::ULONG64
#endif
#endif

#ifdef WIN32
#define SOAP_EINTR WSAEINTR
#define SOAP_EAGAIN WSAEWOULDBLOCK
#define SOAP_EWOULDBLOCK WSAEWOULDBLOCK
#define SOAP_EINPROGRESS WSAEINPROGRESS
#else
#define SOAP_EINTR EINTR
#define SOAP_EAGAIN EAGAIN
#define SOAP_EWOULDBLOCK EWOULDBLOCK
#define SOAP_EINPROGRESS EINPROGRESS
#endif

#ifdef UNDER_CE
#define soap_errno GetLastError()
#else
#define soap_errno errno
#endif

#define SOAP_BUFLEN    8192 /* buffer length for socket packets, also used for gethostbyname buffer so don't make this too small */
#define SOAP_PTRHASH   1024 /* size of pointer analysis hash table (must be power of 2) */
#define SOAP_IDHASH      16 /* size of hash table for receiving element id/href's */
#define SOAP_BLKLEN     256 /* size of blocks to collect long strings */
#define SOAP_TAGLEN     256 /* maximum length of XML tag/attribute/element names + 1 */
#define SOAP_MAXDIMS	 16 /* maximum array dimensions (array nestings) */

#define SOAP_MAXLOGS	  3 /* max number of debug logs per soap environment */
#define SOAP_INDEX_RECV	  0
#define SOAP_INDEX_SENT	  1
#define SOAP_INDEX_TEST	  2

#ifndef FLT_NAN
#ifdef _MATH_H
#define FLT_NAN (*(float*)&soap_double_nan)
#else
#define FLT_NAN (0.0)
#endif
#endif
#ifndef FLT_PINFTY
#ifdef FLT_MAX
#define FLT_PINFTY FLT_MAX
#else
#ifdef HUGE_VAL
#define FLT_PINFTY (float)HUGE_VAL
#else
#ifdef FLOAT_MAX
#define FLT_PINFTY FLOAT_MAX
#else
#define FLT_PINFTY (3.40282347e+38)
#endif
#endif
#endif
#endif
#ifndef FLT_NINFTY
#define FLT_NINFTY (-FLT_PINFTY)
#endif

#ifndef DBL_NAN
#ifdef _MATH_H
#define DBL_NAN (*(double*)&soap_double_nan)
#else
#define DBL_NAN (0.0)
#endif
#endif
#ifndef DBL_PINFTY
#ifdef DBL_MAX
#define DBL_PINFTY DBL_MAX
#else
#ifdef HUGE_VAL
#define DBL_PINFTY (double)HUGE_VAL
#else
#ifdef DOUBLE_MAX
#define DBL_PINFTY DOUBLE_MAX
#else
#define DBL_PINFTY (1.7976931348623157e+308)
#endif
#endif
#endif
#endif
#ifndef DBL_NINFTY
#define DBL_NINFTY (-DBL_PINFTY)
#endif

/* gSOAP error codes */

#define SOAP_EOF EOF
#define SOAP_OK 0
#define SOAP_CLI_FAULT 1
#define SOAP_SVR_FAULT 2
#define SOAP_TAG_MISMATCH 3
#define SOAP_TYPE_MISMATCH 4
#define SOAP_SYNTAX_ERROR 5
#define SOAP_NO_TAG 6
#define SOAP_IOB 7
#define SOAP_MUSTUNDERSTAND 8
#define SOAP_NAMESPACE 9
#define SOAP_OBJ_MISMATCH 10
#define SOAP_FATAL_ERROR 11
#define SOAP_FAULT 12
#define SOAP_NO_METHOD 13
#define SOAP_EOM 14
#define SOAP_NULL 15
#define SOAP_MULTI_ID 16
#define SOAP_MISSING_ID 17
#define SOAP_HREF 18
#define SOAP_TCP_ERROR 19
#define SOAP_HTTP_ERROR 20
#define SOAP_SSL_ERROR 21
#define SOAP_DIME_ERROR 22
#define SOAP_EOD 23
#define SOAP_VERSIONMISMATCH 24
#define SOAP_DIME_VERSIONMISMATCH 25

/* gSOAP DIME */

#define SOAP_DIME_CF 0x1
#define SOAP_DIME_ME 0x2
#define SOAP_DIME_MB 0x4
#define SOAP_DIME_VERSION 0x8 /* DIME version 1 */
#define SOAP_DIME_MEDIA 0x10
#define SOAP_DIME_ABSURI 0x20

/* DEBUG macros */

#ifndef UNDER_CE
#ifdef DEBUG
#ifndef SOAP_DEBUG
#define SOAP_DEBUG
#endif
#endif
#endif

#ifdef SOAP_DEBUG
#ifndef SOAP_MESSAGE
#define SOAP_MESSAGE fprintf
#endif
#define DBGLOG(DBGFILE, DBGCMD) \
{ if (soap)\
  { if (!soap->fdebug[SOAP_INDEX_##DBGFILE])\
      soap_open_logfile(soap, SOAP_INDEX_##DBGFILE);\
    if (soap->fdebug[SOAP_INDEX_##DBGFILE])\
    { FILE *fdebug = soap->fdebug[SOAP_INDEX_##DBGFILE];\
      DBGCMD;\
      fflush(fdebug);\
    }\
  }\
}
#else
#define DBGLOG(DBGFILE, DBGCMD)
#endif

typedef long wchar; /* for compatibility */

struct Namespace
{ const char *id;
  const char *ns;
  const char *in;
  char *out;
};

struct soap_nlist
{ struct soap_nlist *next;
  char *id;
  int level;
  int index;
};

struct soap_blist
{ struct soap_blist *next;
  char *ptr;
  size_t size;
};

struct soap_ilist
{ struct soap_ilist *next;
  int type;
  size_t size;
  void *link;
  void *copy;
  void *ptr;
  int level;
  char id[4]; /* the actual string value overflows into allocated region below this struct */
};

struct soap_plist
{ struct soap_plist *next;
  void *ptr;
  int type;
  int id;
  char mark1;
  char mark2;
};

struct soap_clist
{ struct soap_clist *next;
  void *ptr;
  int type;
  size_t size;
};

#ifdef WITH_COOKIES
struct soap_cookie
{ struct soap_cookie *next;
  char *name;
  char *value;
  char *domain;
  char *path;
  long expire;		/* client-side: local time to expire; server-side: seconds to expire */
  unsigned int version;
  short secure;
  short session;	/* server-side */
  short env;		/* server-side: got cookie from client */
  short modified;	/* server-side: client cookie was modified */
};
#endif

struct soap
{ const char *float_format;	/* points to user-definable format string for floats (<1024 chars) */
  const char *double_format;	/* points to user-definable format string for doubles (<1024 chars) */
  const char *dime_id_format;	/* points to user-definable format string for integer DIME id (<SOAP_TAGLEN chars) */
  const char *http_version;	/* default = "1.0" */
  const char *encodingStyle;	/* default = NULL which means that SOAP encoding is used */
  const char *defaultNamespace;	/* default = NULL which means that no default namespace is set */
  const char *actor;
  short keep_alive;		/* when !=0, attempt to keep connections open */
  short chunked_transfer;	/* when !=0, use chunked transfer (soap.count will be incorrect) */
  short disable_href;		/* when !=0, disables hrefs so objects are duplicated on the output */
  short enable_embedding;	/* when !=0, enable hrefs within embedded elements */
  short enable_null;		/* when !=0, always sends null elements */
  short enable_utf_string;	/* when !=0, assume strings are UTF8/16 encoded and just emit them */
  short disable_request_count;	/* when !=0, do not count message length for requests */
  short disable_response_count; /* when !=0, do not count message length for responses */
  short enable_array_overflow;  /* when !=0, ignore elements that do not fit in a fixed-size array */ 
  int recv_timeout;		/* when > 0, gives socket recv timeout in seconds, < 0 in usec */
  int send_timeout;		/* when > 0, gives socket send timeout in seconds, < 0 in usec */
  int connect_timeout;		/* when > 0, gives socket connect() timeout in seconds, < 0 in usec */
  int accept_timeout;		/* when > 0, gives socket accept() timeout in seconds, < 0 in usec */
  struct Namespace *namespaces;	/* Namespace mapping table */
  struct soap_nlist *nlist;	/* namespace stack */
  struct soap_blist *blist;	/* block allocation stack */
  struct soap_clist *clist;	/* class instance allocation list */
  void *alist;			/* memory allocation list */
  struct soap_ilist *iht[SOAP_IDHASH];
  struct soap_plist *pht[SOAP_PTRHASH];
  struct SOAP_ENV__Header *header;
  struct SOAP_ENV__Fault *fault;
  void *user;			/* reserved for callbacks to pass user-defined data */
  int (*fpost)(struct soap*, const char*, const char*, const char*, const char*, size_t);
  int (*fresponse)(struct soap*, int, size_t);
  int (*fparse)(struct soap*);
  int (*fopen)(struct soap*, const char*, const char*, int);
  int (*fclose)(struct soap*);
  int (*fsend)(struct soap*, const char*, size_t);
  size_t (*frecv)(struct soap*, char*, size_t);
  int (*fignore)(struct soap*, const char*);
  int master;
  int socket;
#ifndef UNDER_CE
  int sendfd;
  int recvfd;
#else
  FILE *sendfd;
  FILE *recvfd;
  char errorstr[256];
  wchar_t werrorstr[256];
#endif
  short buffering;
  size_t bufidx;
  size_t buflen;
  char buf[SOAP_BUFLEN];/* send and receive buffer */
  char msgbuf[1024];	/* buffer for (error) messages */
  char tmpbuf1[256];	/* buffer for array size attributes */
  char tmpbuf2[256];	/* buffer for array offset attributes */
  char tmpbuf3[256];	/* buffer for array position attributes */
  char tagbuf[1024];	/* buffer for combining XML element name and attributes */
  size_t count;		/* message length counter */
  short counting;
  int level;
  short body;
  char tag[SOAP_TAGLEN];
  char id[SOAP_TAGLEN];
  char href[SOAP_TAGLEN];
  char type[SOAP_TAGLEN];
  char arrayType[SOAP_TAGLEN];
  char arraySize[SOAP_TAGLEN];
  char offset[SOAP_TAGLEN];
  short other;
  short root;
  short position;
  int positions[SOAP_MAXDIMS];
  int mustUnderstand;
  short null;
  short ns;
  short is_in_header;
  short chunked;
  short alloced;
  short peeked;
  short cdata;
  short dime_transfer;
  short dime;
  size_t dime_count;
  int dime_flags;
  size_t dime_size;
  size_t dime_chunksize;
  size_t dime_buflen;
  char *dime_ptr;
  char *dime_id;
  char *dime_type;
  char *dime_options;
  int chunksize;
  size_t chunkbuflen;
  char path[SOAP_TAGLEN];
  char host[SOAP_TAGLEN];
  char endpoint[SOAP_TAGLEN];
  char *action;
  int port;
  const char *proxy_host;
  int proxy_port;
  int error;
  int errmode;
  int errnum;
  int idnum;
  wchar ahead1;
  wchar ahead2;
  unsigned long ip;
  const char *logfile[SOAP_MAXLOGS];
  FILE *fdebug[SOAP_MAXLOGS];
#ifdef WITH_COOKIES
  struct soap_cookie *cookies;
  const char *cookie_domain;
  const char *cookie_path;
  int cookie_max;
#endif
#ifdef WITH_OPENSSL
  BIO *bio;
  SSL *ssl;
  short require_server_auth;
  const char *keyfile;
  const char *password;
  const char *dhfile;
  const char *cafile;
#endif
  short dot_net_bug;
};

extern struct Namespace namespaces[];

SOAP_FMAC1 int SOAP_FMAC2 soap_serve(struct soap*);
SOAP_FMAC1 int SOAP_FMAC2 soap_connect(struct soap*, const char*, const char*);
SOAP_FMAC1 int SOAP_FMAC2 soap_bind(struct soap*, const char*, int, int);
SOAP_FMAC1 int SOAP_FMAC2 soap_accept(struct soap*);
SOAP_FMAC1 int SOAP_FMAC2 soap_ssl_accept(struct soap*);

#ifndef WITH_NOGLOBAL
SOAP_FMAC1 void* SOAP_FMAC2 soap_instantiate(struct soap*, int t, const char*, const char*);
SOAP_FMAC1 void SOAP_FMAC2 soap_delete(struct soap*, void*, int, int);
SOAP_FMAC1 void SOAP_FMAC2 soap_fault(struct soap*);
SOAP_FMAC1 const char** SOAP_FMAC2 soap_faultcode(struct soap*);
SOAP_FMAC1 const char** SOAP_FMAC2 soap_faultstring(struct soap*);
SOAP_FMAC1 const char** SOAP_FMAC2 soap_faultdetail(struct soap*);

SOAP_FMAC1 void SOAP_FMAC2 soap_serializeheader(struct soap*);
SOAP_FMAC1 void SOAP_FMAC2 soap_putheader(struct soap*);
SOAP_FMAC1 int SOAP_FMAC2 soap_getheader(struct soap*);

SOAP_FMAC1 void SOAP_FMAC2 soap_serializefault(struct soap*);
SOAP_FMAC1 void SOAP_FMAC2 soap_putfault(struct soap*);
SOAP_FMAC1 int SOAP_FMAC2 soap_getfault(struct soap*);

SOAP_FMAC1 void SOAP_FMAC2 soap_putindependent(struct soap*);
SOAP_FMAC1 int SOAP_FMAC2 soap_getindependent(struct soap*);

SOAP_FMAC1 void SOAP_FMAC2 soap_putattachments(struct soap*);
SOAP_FMAC1 int SOAP_FMAC2 soap_getattachments(struct soap*);
#endif

SOAP_FMAC1 int SOAP_FMAC2 soap_send_raw(struct soap*, const char*, size_t);
SOAP_FMAC1 int SOAP_FMAC2 soap_send(struct soap*, const char*);
SOAP_FMAC1 int SOAP_FMAC2 soap_recv(struct soap*);

SOAP_FMAC1 int SOAP_FMAC2 soap_puthex(struct soap*, int);
SOAP_FMAC1 int SOAP_FMAC2 soap_gethex(struct soap*);

SOAP_FMAC1 int SOAP_FMAC2 soap_pututf8(struct soap*, wchar);
SOAP_FMAC1 wchar SOAP_FMAC2 soap_getutf8(struct soap*);

SOAP_FMAC1 int SOAP_FMAC2 soap_putbase64(struct soap*, const unsigned char*, size_t);
SOAP_FMAC1 unsigned char* SOAP_FMAC2 soap_getbase64(struct soap*, size_t*, int);

SOAP_FMAC1 int SOAP_FMAC2 soap_pointer_lookup(struct soap*, const void *p, int t, struct soap_plist**);
SOAP_FMAC1 int SOAP_FMAC2 soap_array_pointer_lookup(struct soap*, const void *p, int n, int t, struct soap_plist**);
SOAP_FMAC1 int SOAP_FMAC2 soap_pointer_lookup_id(struct soap*, void *p, int t, struct soap_plist**);
SOAP_FMAC1 int SOAP_FMAC2 soap_pointer_enter(struct soap*, const void *p, int t, struct soap_plist**);
SOAP_FMAC1 int SOAP_FMAC2 soap_array_pointer_enter(struct soap*, const void *p, int t, struct soap_plist**);

SOAP_FMAC1 void SOAP_FMAC2 soap_begin_count(struct soap*);
SOAP_FMAC1 void SOAP_FMAC2 soap_begin_send(struct soap*);
SOAP_FMAC1 int SOAP_FMAC2 soap_end_send(struct soap*);

SOAP_FMAC1 void SOAP_FMAC2 soap_embedded(struct soap*, const void *p, int t);
SOAP_FMAC1 int SOAP_FMAC2 soap_reference(struct soap*, const void *p, int t);
SOAP_FMAC1 int SOAP_FMAC2 soap_array_reference(struct soap*, const void *p, int n, int t);
SOAP_FMAC1 int SOAP_FMAC2 soap_embedded_id(struct soap*, int id, const void *p, int t);
SOAP_FMAC1 int SOAP_FMAC2 soap_is_embedded(struct soap*, struct soap_plist*);
SOAP_FMAC1 int SOAP_FMAC2 soap_is_single(struct soap*, struct soap_plist*);
SOAP_FMAC1 int SOAP_FMAC2 soap_is_multi(struct soap*, struct soap_plist*);
SOAP_FMAC1 void SOAP_FMAC2 soap_set_embedded(struct soap*, struct soap_plist*);
SOAP_FMAC1 void SOAP_FMAC2 soap_set_attached(struct soap*, struct soap_plist*, const char*, const char*, const char*, size_t);

SOAP_FMAC1 int SOAP_FMAC2 soap_getline(struct soap*, char*, int);
SOAP_FMAC1 int SOAP_FMAC2 soap_begin_recv(struct soap*);
SOAP_FMAC1 int SOAP_FMAC2 soap_end_recv(struct soap*);

SOAP_FMAC1 int SOAP_FMAC2 soap_send_namespaces(struct soap*);

SOAP_FMAC1 void* SOAP_FMAC2 soap_malloc(struct soap*, size_t);
SOAP_FMAC1 void SOAP_FMAC2 soap_dealloc(struct soap*, void *);
SOAP_FMAC1 void SOAP_FMAC2 soap_unlink(struct soap*, void *);
SOAP_FMAC1 void SOAP_FMAC2 soap_free(struct soap*);
SOAP_FMAC1 void SOAP_FMAC2 soap_destroy(struct soap*);

SOAP_FMAC1 int SOAP_FMAC2 soap_lookup_type(struct soap*, const char *id);

SOAP_FMAC1 void* SOAP_FMAC2 soap_id_lookup(struct soap*, const char *id, void **p, int t, size_t n, int k);
SOAP_FMAC1 void* SOAP_FMAC2 soap_id_forward(struct soap*, const char *id, void *p, int t, size_t n);
SOAP_FMAC1 void* SOAP_FMAC2 soap_id_enter(struct soap*, const char *id, void *p, int t, size_t n, int k);
SOAP_FMAC1 void* SOAP_FMAC2 soap_class_id_enter(struct soap*, const char*, void*, int, const char*, const char*);

SOAP_FMAC1 int SOAP_FMAC2 soap_size(const int *, int);
SOAP_FMAC1 int SOAP_FMAC2 soap_getoffsets(const char *, const int *, int *, int);
SOAP_FMAC1 int SOAP_FMAC2 soap_getsize(const char *, const char *, int *);
SOAP_FMAC1 int SOAP_FMAC2 soap_getsizes(const char *, int *, int);
SOAP_FMAC1 int SOAP_FMAC2 soap_getposition(const char *, int *);

SOAP_FMAC1 char* SOAP_FMAC2 soap_putsize(struct soap*, const char *, int);
SOAP_FMAC1 char* SOAP_FMAC2 soap_putsizesoffsets(struct soap*, const char *, const int *, const int *, int);
SOAP_FMAC1 char* SOAP_FMAC2 soap_putsizes(struct soap*, const char *, const int *, int);
SOAP_FMAC1 char* SOAP_FMAC2 soap_putoffset(struct soap*, int);
SOAP_FMAC1 char* SOAP_FMAC2 soap_putoffsets(struct soap*, const int *, int);
SOAP_FMAC1 char* SOAP_FMAC2 soap_putposition(struct soap*);
 
SOAP_FMAC1 int SOAP_FMAC2 soap_ignore_element(struct soap*);

SOAP_FMAC1 int SOAP_FMAC2 soap_closesock(struct soap*);

SOAP_FMAC1 struct soap *SOAP_FMAC2 soap_new();
SOAP_FMAC1 void SOAP_FMAC2 soap_init(struct soap*);
SOAP_FMAC1 void SOAP_FMAC2 soap_done(struct soap*);
SOAP_FMAC1 void SOAP_FMAC2 soap_begin(struct soap*);
SOAP_FMAC1 void SOAP_FMAC2 soap_end(struct soap*);

SOAP_FMAC1 void SOAP_FMAC2 soap_set_recv_logfile(struct soap*, const char*);
SOAP_FMAC1 void SOAP_FMAC2 soap_set_sent_logfile(struct soap*, const char*);
SOAP_FMAC1 void SOAP_FMAC2 soap_set_test_logfile(struct soap*, const char*);
SOAP_FMAC1 void SOAP_FMAC2 soap_close_logfiles(struct soap*);
SOAP_FMAC1 void SOAP_FMAC2 soap_open_logfile(struct soap*, int);

SOAP_FMAC1 char* SOAP_FMAC2 soap_value(struct soap*);

SOAP_FMAC1 wchar SOAP_FMAC2 soap_advance(struct soap*);
SOAP_FMAC1 wchar SOAP_FMAC2 soap_skip(struct soap*);
SOAP_FMAC1 int SOAP_FMAC2 soap_move(struct soap*, int);
SOAP_FMAC1 size_t SOAP_FMAC2 soap_tell(struct soap*);

SOAP_FMAC1 int SOAP_FMAC2 soap_match_tag(struct soap*, const char*, const char *);

SOAP_FMAC1 int SOAP_FMAC2 soap_match_array(struct soap*, const char*);

SOAP_FMAC1 int SOAP_FMAC2 soap_element_begin_out(struct soap*, const char *tag, int id, const char *type);
SOAP_FMAC1 int SOAP_FMAC2 soap_array_begin_out(struct soap*, const char *tag, int id, const char *type, const char *offset);

SOAP_FMAC1 int SOAP_FMAC2 soap_element_end_out(struct soap*, const char *tag);

SOAP_FMAC1 int SOAP_FMAC2 soap_element_ref(struct soap*, const char *tag, int id, int href);

SOAP_FMAC1 int SOAP_FMAC2 soap_element_href(struct soap*, const char *tag, int id, const char *href);

SOAP_FMAC1 int SOAP_FMAC2 soap_element_null(struct soap*, const char *tag, int id, const char *type);

SOAP_FMAC1 int SOAP_FMAC2 soap_element_begin_in(struct soap*, const char *tag);

SOAP_FMAC1 int SOAP_FMAC2 soap_element_end_in(struct soap*, const char *tag);

SOAP_FMAC1 int SOAP_FMAC2 soap_peek_element(struct soap*);

SOAP_FMAC1 void SOAP_FMAC2 soap_revert(struct soap*);

SOAP_FMAC1 int SOAP_FMAC2 soap_ignore_element(struct soap*);

SOAP_FMAC1 int SOAP_FMAC2 soap_convert_string_out(struct soap*, const char *s);
SOAP_FMAC1 int SOAP_FMAC2 soap_convert_wstring_out(struct soap*, const wchar_t *s);

SOAP_FMAC1 int SOAP_FMAC2 soap_match_namespace(struct soap*, const char *, const char*, int n1, int n2);

SOAP_FMAC1 void SOAP_FMAC2 soap_pop_namespace(struct soap*);
SOAP_FMAC1 int SOAP_FMAC2 soap_push_namespace(struct soap*, const char *,const char *);

SOAP_FMAC1 int SOAP_FMAC2 soap_new_block(struct soap*);
SOAP_FMAC1 void* SOAP_FMAC2 soap_push_block(struct soap*, size_t);
SOAP_FMAC1 void SOAP_FMAC2 soap_pop_block(struct soap*);
SOAP_FMAC1 void SOAP_FMAC2 soap_store_block(struct soap*, char *);

SOAP_FMAC1 int SOAP_FMAC2 soap_envelope_begin_out(struct soap*);
SOAP_FMAC1 int soap_envelope_end_out(struct soap*);

SOAP_FMAC1 int SOAP_FMAC2 soap_envelope_begin_in(struct soap*);
SOAP_FMAC1 int SOAP_FMAC2 soap_envelope_end_in(struct soap*);

SOAP_FMAC1 int SOAP_FMAC2 soap_body_begin_out(struct soap*);
SOAP_FMAC1 int SOAP_FMAC2 soap_body_end_out(struct soap*);

SOAP_FMAC1 int SOAP_FMAC2 soap_body_begin_in(struct soap*);
SOAP_FMAC1 int SOAP_FMAC2 soap_body_end_in(struct soap*);

SOAP_FMAC1 int SOAP_FMAC2 soap_recv_header(struct soap*);

SOAP_FMAC1 int SOAP_FMAC2 soap_response(struct soap*, int);

SOAP_FMAC1 int SOAP_FMAC2 soap_send_fault(struct soap*);

SOAP_FMAC1 int SOAP_FMAC2 soap_recv_fault(struct soap*);

SOAP_FMAC1 void SOAP_FMAC2 soap_print_fault(struct soap*, FILE*);
SOAP_FMAC1 void SOAP_FMAC2 soap_print_fault_location(struct soap*, FILE*);

SOAP_FMAC1 int SOAP_FMAC2 soap_outint(struct soap*, const char *tag, int id, const int *p, const char *, int);
SOAP_FMAC1 int* SOAP_FMAC2 soap_inint(struct soap*, const char *tag, int *p, const char *, int);

SOAP_FMAC1 int SOAP_FMAC2 soap_outbyte(struct soap*, const char *tag, int id, const char *p, const char *, int);
SOAP_FMAC1 char* SOAP_FMAC2 soap_inbyte(struct soap*, const char *tag, char *p, const char *, int);

SOAP_FMAC1 int SOAP_FMAC2 soap_outlong(struct soap*, const char *tag, int id, const long *p, const char *, int);
SOAP_FMAC1 long* SOAP_FMAC2 soap_inlong(struct soap*, const char *tag, long *p, const char *, int);

SOAP_FMAC1 int SOAP_FMAC2 soap_outLONG64(struct soap*, const char *tag, int id, const LONG64 *p, const char *, int);
SOAP_FMAC1 LONG64* SOAP_FMAC2 soap_inLONG64(struct soap*, const char *tag, LONG64 *p, const char *, int);

SOAP_FMAC1 int SOAP_FMAC2 soap_outshort(struct soap*, const char *tag, int id, const short *p, const char *, int);
SOAP_FMAC1 short* SOAP_FMAC2 soap_inshort(struct soap*, const char *tag, short *p, const char *, int);

SOAP_FMAC1 int soap_outfloat(struct soap*, const char *tag, int id, const float *p, const char *, int);
SOAP_FMAC1 float* SOAP_FMAC2 soap_infloat(struct soap*, const char *tag, float *p, const char *, int);

SOAP_FMAC1 int SOAP_FMAC2 soap_outdouble(struct soap*, const char *tag, int id, const double *p, const char *, int);
SOAP_FMAC1 double* SOAP_FMAC2 soap_indouble(struct soap*, const char *tag, double *p, const char *, int);

SOAP_FMAC1 int SOAP_FMAC2 soap_outunsignedByte(struct soap*, const char *tag, int id, const unsigned char *p, const char *, int);
SOAP_FMAC1 unsigned char* SOAP_FMAC2 soap_inunsignedByte(struct soap*, const char *tag, unsigned char *p, const char *, int);

SOAP_FMAC1 int SOAP_FMAC2 soap_outunsignedShort(struct soap*, const char *tag, int id, const unsigned short *p, const char *, int);
SOAP_FMAC1 unsigned short* SOAP_FMAC2 soap_inunsignedShort(struct soap*, const char *tag, unsigned short *p, const char *, int);

SOAP_FMAC1 int SOAP_FMAC2 soap_outunsignedInt(struct soap*, const char *tag, int id, const unsigned int *p, const char *, int);
SOAP_FMAC1 unsigned int* SOAP_FMAC2 soap_inunsignedInt(struct soap*, const char *tag, unsigned int *p, const char *, int);

SOAP_FMAC1 int SOAP_FMAC2 soap_outunsignedLong(struct soap*, const char *tag, int id, const unsigned long *p, const char *, int);
SOAP_FMAC1 unsigned long* SOAP_FMAC2 soap_inunsignedLong(struct soap*, const char *tag, unsigned long *p, const char *, int);

SOAP_FMAC1 int SOAP_FMAC2 soap_outULONG64(struct soap*, const char *tag, int id, const ULONG64 *p, const char *, int);
SOAP_FMAC1 ULONG64* SOAP_FMAC2 soap_inULONG64(struct soap*, const char *tag, ULONG64 *p, const char *, int);

SOAP_FMAC1 int SOAP_FMAC2 soap_outstring(struct soap*, const char *tag, int id, char *const*p, const char *, int);
SOAP_FMAC1 char** SOAP_FMAC2 soap_instring(struct soap*, const char *tag, char **p, const char *, int);

SOAP_FMAC1 int SOAP_FMAC2 soap_outwstring(struct soap*, const char *tag, int id, wchar_t *const*p, const char *, int);
SOAP_FMAC1 wchar_t** SOAP_FMAC2 soap_inwstring(struct soap*, const char *tag, wchar_t **p, const char *, int);

SOAP_FMAC1 int SOAP_FMAC2 soap_outliteral(struct soap*, const char *tag, char *const*p);
SOAP_FMAC1 char** SOAP_FMAC2 soap_inliteral(struct soap*, const char *tag, char **p);

SOAP_FMAC1 int SOAP_FMAC2 soap_outwliteral(struct soap*, const char *tag, wchar_t *const*p);
SOAP_FMAC1 wchar_t** SOAP_FMAC2 soap_inwliteral(struct soap*, const char *tag, wchar_t **p);

SOAP_FMAC1 int SOAP_FMAC2 soap_outdateTime(struct soap*, const char *tag, int id, const time_t *p, const char *, int);
SOAP_FMAC1 time_t* SOAP_FMAC2 soap_indateTime(struct soap*, const char *tag, time_t *p, const char *, int);

SOAP_FMAC1 char* SOAP_FMAC2 soap_dime_option(struct soap*, unsigned short, const char*);
SOAP_FMAC1 int SOAP_FMAC2 soap_getdimehdr(struct soap*);
SOAP_FMAC1 int SOAP_FMAC2 soap_getdime(struct soap*);
SOAP_FMAC1 int SOAP_FMAC2 soap_putdimehdr(struct soap*);
SOAP_FMAC1 int SOAP_FMAC2 soap_putdime(struct soap*, int, char*, char*, char*, void*, size_t);

#ifdef WITH_COOKIES
SOAP_FMAC1 extern struct soap_cookie* SOAP_FMAC2 soap_set_cookie(struct soap*, const char*, const char*, const char*, const char*);
SOAP_FMAC1 extern struct soap_cookie* SOAP_FMAC2 soap_cookie(struct soap*, const char*, const char*, const char*);
SOAP_FMAC1 extern char* SOAP_FMAC2 soap_cookie_value(struct soap*, const char*, const char*, const char*);
SOAP_FMAC1 extern long SOAP_FMAC2 soap_cookie_expire(struct soap*, const char*, const char*, const char*);
SOAP_FMAC1 extern int SOAP_FMAC2 soap_set_cookie_expire(struct soap*, const char*, long, const char*, const char*);
SOAP_FMAC1 extern int SOAP_FMAC2 soap_set_cookie_session(struct soap*, const char*, const char*, const char*);
SOAP_FMAC1 extern int SOAP_FMAC2 soap_clr_cookie_session(struct soap*, const char*, const char*, const char*);
SOAP_FMAC1 extern void SOAP_FMAC2 soap_clr_cookie(struct soap*, const char*, const char*, const char*);
SOAP_FMAC1 extern int SOAP_FMAC2 soap_getenv_cookies(struct soap*);
SOAP_FMAC1 extern void SOAP_FMAC2 soap_free_cookies(struct soap*);
#endif

#ifdef __cplusplus
}
#endif

#endif

