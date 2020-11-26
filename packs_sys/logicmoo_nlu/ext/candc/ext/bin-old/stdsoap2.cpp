/*	stdsoap2.c[pp] 2.1.6

The contents of this file are subject to the gSOAP Public License Version 1.0
(the "License"); you may not use this file except in compliance with the
License. You may obtain a copy of the License at
http://www.cs.fsu.edu/~engelen/soaplicense.html
Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
for the specific language governing rights and limitations under the License.

The Initial Developer of the Original Code is Robert A. van Engelen.
Copyright (C) 2000-2002 Robert A. van Engelen. All Rights Reserved.

Note:

Win32 build needs winsock.dll (Visual C++ "wsock32.lib")
To do this in Visual C++ 6.0, go to "Project", "settings", select the "Link"
tab (the project file needs to be selected in the file view) and add
"wsock32.lib" to the "Object/library modules" entry

*/

#include "stdsoap2.h"

#ifdef __cplusplus
extern "C" {
#endif

/*      EOF=-1 */
#define LT (-2)	/* XML character '<' */
#define TT (-3) /* XML character '</' */
#define GT (-4) /* XML character '>' */
#define QT (-5) /* XML character ''' */
#define AP (-6) /* XML character '"' */

#define blank(c) ((c)>=0 && (c)<=32)
#define notblank(c) ((c)>32)
#define hash_ptr(p) (((long)(p)>>3)&(SOAP_PTRHASH-1))
#define soap_get1(soap) ((soap->bufidx>=soap->buflen && soap_recv(soap)) ? EOF : (unsigned char)soap->buf[soap->bufidx++])
#define soap_unget(soap, c) (soap->ahead1 = c)
#define soap_unget2(soap, c) (soap->ahead2 = c)

static struct soap_ilist *lookup(struct soap*, const char*);
static struct soap_ilist *enter(struct soap*, const char*);
static void soap_update_ptrs(struct soap*, char*, char*, long);
static void soap_resolve_ptr(struct soap_ilist*);
static void soap_resolve_copy(struct soap*, struct soap_ilist*);
static int soap_putdimefield(struct soap*, const char*, int);
static char *soap_getdimefield(struct soap*, int);
static int soap_position(struct soap*);
static int soap_mustUnderstand(struct soap*);
static int soap_puthttphdr(struct soap*, size_t);

static int http_post(struct soap*, const char*, const char*, const char*, const char*, size_t);
static int http_response(struct soap*, int, size_t);
static int http_parse(struct soap*);
static int tcp_connect(struct soap*, const char*, const char*, int);
static int tcp_disconnect(struct soap*);
static int fsend(struct soap*, const char*, size_t);
static size_t frecv(struct soap*, char*, size_t);
static int fignore(struct soap*, const char*);

#ifdef _MATH_H
struct soap_double_nan soap_double_nan = {0xFFFFFFFF, 0xFFFFFFFF};
#endif

struct code_map
{ int code;
  const char *string;
};

static struct code_map h_errno_codes[] = {
#ifdef HOST_NOT_FOUND   
    { HOST_NOT_FOUND, "Host not found" },
#endif
#ifdef TRY_AGAIN
    { TRY_AGAIN, "Try Again" },
#endif
#ifdef NO_RECOVERY  
    { NO_RECOVERY, "No Recovery" },
#endif
#ifdef NO_DATA
    { NO_DATA, "No Data" },
#endif
#ifdef NO_ADDRESS
    { NO_ADDRESS, "No Address" },
#endif
    {0, NULL}
};

#ifdef WITH_OPENSSL
static SSL_CTX *soap_ssl_ctx = NULL;
static const char *soap_ssl_pw = NULL;
#endif

#ifdef WIN32
static int tcp_done = 0;
#endif

/******************************************************************************/
static int
SOAP_FMAC2
fsend(struct soap *soap, const char *s, size_t n)
{ int nwritten;
  while (n > 0)
  {
#ifdef WITH_OPENSSL
    if (soap->ssl)
      nwritten = SSL_write(soap->ssl, s, n);
    else
#endif
    if (soap->socket >= 0)
    { if (soap->send_timeout)
      { struct timeval timeout;
        fd_set fd;
        if (soap->send_timeout > 0)
	{ timeout.tv_sec = soap->send_timeout;
          timeout.tv_usec = 0;
	}
	else
	{ timeout.tv_sec = -soap->send_timeout/1000000;
          timeout.tv_usec = -soap->send_timeout%1000000;
	}
        FD_ZERO(&fd);
        FD_SET(soap->socket, &fd);
        for (;;)
        { if (select(soap->socket + 1, NULL, &fd, NULL, &timeout) > 0)
            break;
          if (soap_errno != SOAP_EINTR)
            return SOAP_EOF;
        }
      }
      nwritten = send(soap->socket, s, n, 0);
    }
    else
#ifdef WITH_FASTCGI
    { nwritten = fwrite(s, 1, n, stdout);
      fflush(stdout);
    }
#else
#ifdef UNDER_CE
      nwritten = fwrite(s, 1, n, soap->sendfd);
#else
      nwritten = write(soap->sendfd, s, n);
#endif
#endif
    if (nwritten <= 0)
    {
#ifdef WITH_OPENSSL
      if (SSL_get_error(soap->ssl, nwritten) != SSL_ERROR_NONE)
        return SOAP_EOF;
      else
#endif
      if (soap_errno == SOAP_EINTR || soap_errno == SOAP_EWOULDBLOCK || soap_errno == SOAP_EAGAIN)
        nwritten = 0;  /* and call write() again */
      else
        return SOAP_EOF;
    }
    n -= nwritten;
    s += nwritten;
  }
  return SOAP_OK;
}

/******************************************************************************/
SOAP_FMAC1
int
SOAP_FMAC2
soap_flush(struct soap *soap)
{ if (soap->bufidx == 0)
    return SOAP_OK;
  if (soap->chunked)
  { char tmp[12];
    sprintf(tmp, "%X\r\n", (unsigned int)soap->bufidx);
    if ((soap->error = soap->fsend(soap, tmp, strlen(tmp))))
      return soap->error;
  }
  if ((soap->error = soap->fsend(soap, soap->buf, soap->bufidx)))
    return soap->error;
  if (soap->chunked)
    if ((soap->error = soap->fsend(soap, "\r\n", 2)))
      return soap->error;
  soap->bufidx = 0;
  return SOAP_OK;
}

/******************************************************************************/
SOAP_FMAC1
int
SOAP_FMAC2
soap_send_raw(struct soap *soap, const char *s, size_t n)
{ register size_t i;
  if (soap->counting)
  { soap->count += n;
    return SOAP_OK;
  }
  DBGLOG(SENT, for (i = 0; i < n; i++) fputc(s[i], fdebug));
  if (soap->buffering) /* send by packets of size SOAP_BUFLEN */
  { char *b;
    while (n)
    { i = soap->bufidx;
      if (n >= SOAP_BUFLEN-i)
      { n -= SOAP_BUFLEN-i;
        for (b = soap->buf+i; i < SOAP_BUFLEN; i++)
          *b++ = *s++;
	soap->bufidx = SOAP_BUFLEN;
        if (soap_flush(soap))
	  return soap->error;
      }
      else
      { soap->bufidx += n;
        for (b = soap->buf+i; n; n--)
          *b++ = *s++;
        break;
      }
    }
    return SOAP_OK;
  }
  return soap->error = soap->fsend(soap, s, n);
}

/******************************************************************************/
SOAP_FMAC1
int
SOAP_FMAC2
soap_send(struct soap *soap, const char *s)
{ return soap_send_raw(soap, s, strlen(s));
}

/******************************************************************************/
static size_t
frecv(struct soap *soap, char *s, size_t n)
{ int r;
#ifdef WITH_OPENSSL
  if (soap->ssl)
  { r = SSL_read(soap->ssl, s, n);
    if (SSL_get_error(soap->ssl, r) == SSL_ERROR_NONE)
      return r;
    return 0;
  }
#endif
  if (soap->socket >= 0)
  { if (soap->recv_timeout)
    { struct timeval timeout;
      fd_set fd;
      if (soap->recv_timeout > 0)
      { timeout.tv_sec = soap->recv_timeout;
        timeout.tv_usec = 0;
      }
      else
      { timeout.tv_sec = -soap->recv_timeout/1000000;
        timeout.tv_usec = -soap->recv_timeout%1000000;
      }
      FD_ZERO(&fd);
      FD_SET(soap->socket, &fd);
      for (;;)
      { if (select(soap->socket + 1, &fd, NULL, NULL, &timeout) > 0)
          break;
        if (soap_errno != SOAP_EINTR)
          return 0;
      }
    }
    r = recv(soap->socket, s, n, 0);
    if (r < 0)
      return 0;
    return r;
  }
#ifdef WITH_FASTCGI
  return fread(s, 1, n, stdin);
#else
#ifdef UNDER_CE
  return fread(s, 1, n, soap->recvfd);
#else
  return read(soap->recvfd, s, n);
#endif
#endif
}

/******************************************************************************/
static int
soap_recv_raw(struct soap *soap)
{ size_t ret;
#ifdef SOAP_DEBUG
  int i;
#endif
  if (soap->chunked)	/* read HTTP chunked transfer */
  { if (soap->chunksize > 0)
    { if (soap->chunksize > SOAP_BUFLEN)
        soap->buflen = ret = soap->frecv(soap, soap->buf, SOAP_BUFLEN);
      else
        soap->buflen = ret = soap->frecv(soap, soap->buf, soap->chunksize);
      DBGLOG(TEST, SOAP_MESSAGE(fdebug,"\nGetting chunk: read %d bytes", ret));
      soap->bufidx = 0;
      soap->chunksize -= ret;
    }
    else
    { char tmp[8], *t;
      t = tmp;
      if (!soap->chunkbuflen)
      { soap->chunkbuflen = ret = soap->frecv(soap, soap->buf, SOAP_BUFLEN);
        DBGLOG(TEST, SOAP_MESSAGE(fdebug,"\nRead %d bytes", ret));
        soap->bufidx = 0;
	if (!ret)
	  return EOF;
      }
      soap->buflen = soap->chunkbuflen;
      DBGLOG(TEST, SOAP_MESSAGE(fdebug,"\nGetting chunk size from buffered HTTP"));
      while (soap->bufidx < soap->buflen && !isxdigit((int)soap->buf[soap->bufidx]))
        soap->bufidx++;
      while (soap->bufidx < soap->buflen && t-tmp < 7 && isxdigit((int)soap->buf[soap->bufidx]))
        *t++ = soap->buf[soap->bufidx++];
      while (soap->bufidx < soap->buflen && soap->buf[soap->bufidx] != '\n')
        soap->bufidx++;
      if (soap->bufidx >= soap->buflen || soap->buf[soap->bufidx] != '\n')
      { soap->buflen = soap->chunkbuflen = ret = soap->frecv(soap, soap->buf, SOAP_BUFLEN);
        DBGLOG(TEST, SOAP_MESSAGE(fdebug,"\nRead %d bytes", ret));
        soap->bufidx = 0;
	if (!ret)
	  return EOF;
        while (soap->bufidx < soap->buflen && t-tmp < 7 && isxdigit((int)soap->buf[soap->bufidx]))
          *t++ = soap->buf[soap->bufidx++];
        while (soap->bufidx < soap->buflen && soap->buf[soap->bufidx] != '\n')
          soap->bufidx++;
      }
      *t = '\0';
      soap->bufidx++;
      soap->chunksize = strtol(tmp, &t, 16);
      if (soap->chunksize <= 0)
      { soap->bufidx = 0;
        soap->buflen = 0;
        soap->chunkbuflen = 0;
	return EOF;
      }
      else
        soap->buflen = soap->bufidx + soap->chunksize;
      DBGLOG(TEST, SOAP_MESSAGE(fdebug,"\nMoving buf len to %d", soap->buflen));
      if (soap->buflen > soap->chunkbuflen)
      { soap->buflen = soap->chunkbuflen;
	soap->chunksize -= soap->buflen - soap->bufidx;
        soap->chunkbuflen = 0;
        DBGLOG(TEST, SOAP_MESSAGE(fdebug,"\nPassed end of buffer for chunked HTTP"));
      }
      ret = soap->buflen - soap->bufidx;
    }
  }
  else
  { soap->buflen = ret = soap->frecv(soap, soap->buf, SOAP_BUFLEN);
    DBGLOG(TEST, SOAP_MESSAGE(fdebug,"\nRead %d bytes", ret));
    soap->bufidx = 0;
  }
  DBGLOG(RECV, for (i = soap->bufidx; i < soap->buflen; i++) fputc(soap->buf[i], fdebug));
  soap->count += ret;
  return !ret;
}

/******************************************************************************/
SOAP_FMAC1
int
SOAP_FMAC2
soap_recv(struct soap *soap)
{ if (soap->dime)
  { if (soap->dime_buflen)
    { char *s;
      int i;
      unsigned char tmp[12];
      DBGLOG(TEST, SOAP_MESSAGE(fdebug,"\nDIME hdr for chunked DIME is in buffer"));
      soap->count += soap->dime_buflen - soap->buflen;
      soap->buflen = soap->dime_buflen;
      DBGLOG(TEST, SOAP_MESSAGE(fdebug,"\nSkip padding (%u bytes)", -(int)soap->dime_size&3));
      for (i = -(int)soap->dime_size&3; i > 0; i--)
      { soap->bufidx++;
        if (soap->bufidx >= soap->buflen)
          if (soap_recv_raw(soap))
	    return EOF;
      }
      DBGLOG(TEST, SOAP_MESSAGE(fdebug,"\nGet DIME hdr for next chunk"));
      s = (char*)tmp;
      for (i = 12; i > 0; i--)
      { *s++ = soap->buf[soap->bufidx++];
        if (soap->bufidx >= soap->buflen)
          if (soap_recv_raw(soap))
	    return EOF;
      }
      soap->dime_flags = tmp[0]&0x7;
      soap->dime_size = tmp[8]<<24 | tmp[9]<<16 | tmp[10]<<8 | tmp[11];
      DBGLOG(TEST, SOAP_MESSAGE(fdebug,"\nGet DIME chunk (%u bytes)", soap->dime_size));
      if (soap->dime_flags&SOAP_DIME_CF)
      { DBGLOG(TEST, SOAP_MESSAGE(fdebug,"\nMore chunking"));
        soap->dime_chunksize = soap->dime_size;
        if (soap->buflen - soap->bufidx >= soap->dime_size)
        { soap->dime_buflen = soap->buflen;
          soap->buflen = soap->bufidx + soap->dime_chunksize;
        }
        else
          soap->dime_chunksize -= soap->buflen - soap->bufidx;
      }
      else
      { DBGLOG(TEST, SOAP_MESSAGE(fdebug,"\nLast chunk"));
        soap->dime_buflen = 0;
        soap->dime_chunksize = 0;
      }
      soap->count = soap->buflen - soap->bufidx;
      DBGLOG(TEST, SOAP_MESSAGE(fdebug,"\n%d bytes remaining", soap->count));
      return SOAP_OK;
    }
    else if (soap->dime_chunksize)
    { DBGLOG(TEST, SOAP_MESSAGE(fdebug,"\nGet next DIME hdr for chunked DIME (%u bytes chunk)", soap->dime_chunksize));
      if (soap_recv_raw(soap))
        return EOF;
      if (soap->buflen - soap->bufidx >= soap->dime_chunksize)
      { soap->dime_buflen = soap->buflen;
        soap->count -= soap->buflen - soap->bufidx - soap->dime_chunksize;
        soap->buflen = soap->bufidx + soap->dime_chunksize;
      }
      else
        soap->dime_chunksize -= soap->buflen - soap->bufidx;
      DBGLOG(TEST, SOAP_MESSAGE(fdebug,"\n%d bytes remaining, count=%d", soap->buflen-soap->bufidx, soap->count));
      return SOAP_OK;
    }
  }
  return soap_recv_raw(soap);
}

/******************************************************************************/
SOAP_FMAC1
wchar
SOAP_FMAC2
soap_get2(struct soap *soap)
{ register wchar c;
  if (soap->ahead1)
  { c = soap->ahead1;
    soap->ahead1 = 0;
    return c;
  }
  if (soap->ahead2)
  { c = soap->ahead2;
    soap->ahead2 = 0;
    return c;
  }
  return soap_get1(soap);
}

/******************************************************************************/
SOAP_FMAC1
int
SOAP_FMAC2
soap_tag_cmp(const char *s, const char *t)
{ for (; *s; s++, t++)
    if (tolower(*s) != tolower(*t))
      if (*t != '-')
      { if (*t != '*')
	  return 1;
	else if (*++t)
	{ int c = tolower(*t);
	  for (; *s; s++)
	  { if (tolower(*s) == c)
	      if (!soap_tag_cmp(s+1, t+1))
	        return 0;
          }
	}
        else
	  return 0;
      }
  if (*t == '*' && !t[1])
    return 0;
  return *t;
}

/******************************************************************************/
SOAP_FMAC1
wchar
SOAP_FMAC2
soap_char(struct soap *soap)
{ int i, c;
  char tmp[8], *t = tmp;
  for (i = 0; i < 7; i++)
  { c = soap_get1(soap);
    if (c == ';' || c == EOF)
      break;
    else
      *t++ = c;
  }
  *t = '\0';
  if (*tmp == '#')
  { if (tmp[1] == 'x' || tmp[1] == 'X')
      return strtol(tmp+2, &t, 16)|0x80000000;
    else
      return atoi(tmp+1)|0x80000000;
  }
  if (!soap_tag_cmp(tmp, "lt"))
    return '<';
  if (!soap_tag_cmp(tmp, "gt"))
    return '>';
  if (!soap_tag_cmp(tmp, "amp"))
    return '&';
  if (!soap_tag_cmp(tmp, "quot"))
    return '"';
  if (!soap_tag_cmp(tmp, "apos"))
    return '\'';
  return '?';
}

/******************************************************************************/
SOAP_FMAC1
wchar
SOAP_FMAC2
soap_get(struct soap *soap)
{ register wchar c;
  if (soap->ahead1)
  { c = soap->ahead1;
    soap->ahead1 = 0;
    return c;
  }
  if (soap->ahead2)
  { c = soap->ahead2;
    soap->ahead2 = 0;
    return c;
  }
redo:
  c = soap_get1(soap);
  if (soap->cdata)
  { if (c == ']')
    { c = soap_get1(soap);
      if (c == ']')
      { soap->cdata = 0;
        c = soap_get1(soap); /* skip > */
        goto redo;
      }
      else
      { soap->ahead2 = c;
        return ']';
      }
    }
    else
      return c;
  }
  switch (c)
  { case '<':
      do c = soap_get1(soap);
      while (blank(c));
      if (c == '!' || c == '?' || c == '%')
      { if (c == '!')
        { c = soap_get1(soap);
	  if (c == '[')
	  { do c = soap_get1(soap);
	    while (c != EOF && c != '[');
	    if (c == EOF)
	      return EOF;
	    soap->cdata = 1;
            return soap_get1(soap);
	  }
	  if (c == '-' && (c = soap_get1(soap)) == '-')
	  { do
	    { c = soap_get1(soap);
	      if (c == '-' && (c = soap_get1(soap)) == '-')
	        break;
	    } while (c != EOF);
	  }
	}
        while (c != EOF && c != '>')
          c = soap_get1(soap);
        if (c == '>')
          goto redo;
        return EOF;
      }
      if (c == '/')
        return TT;
      soap->ahead2 = c;
      return LT;
    case '>':
      return GT;
    case '"':
      return QT;
    case '\'':
      return AP;
    case '&':
      c = soap_char(soap);
  }
  return c;
}

/******************************************************************************/
SOAP_FMAC1
wchar
SOAP_FMAC2
soap_advance(struct soap *soap)
{ wchar c;
  while (((c = soap_get(soap)) != EOF) && c != LT && c != TT)
    ;
  return c;
}

/******************************************************************************/
SOAP_FMAC1
wchar
SOAP_FMAC2
soap_skip(struct soap *soap)
{ register wchar c;
  do c = soap_get(soap);
  while (blank(c));
  return c;
}

/******************************************************************************/
SOAP_FMAC1
int
SOAP_FMAC2
soap_move(struct soap *soap, int n)
{ DBGLOG(TEST, SOAP_MESSAGE(fdebug,"\nmoving %d bytes forward", n));
  for (; n > 0; n--)
    if (soap_get2(soap) == EOF)
      return SOAP_EOF;
  return SOAP_OK;
}

/******************************************************************************/
SOAP_FMAC1
size_t
SOAP_FMAC2
soap_tell(struct soap *soap)
{ return soap->count - soap->buflen + soap->bufidx - (soap->ahead1 || soap->ahead2);
}

/******************************************************************************/
SOAP_FMAC1
int
SOAP_FMAC2
soap_pututf8(struct soap *soap, wchar c)
{ char tmp[16];
  if (c < 0x80)
  { tmp[0] = (char)c;
    tmp[1] = '\0';
  }
  else
    sprintf(tmp, "&#%lu;", (unsigned long)c);
  return soap_send(soap, tmp);
}

/******************************************************************************/
SOAP_FMAC1
wchar
SOAP_FMAC2
soap_getutf8(struct soap *soap)
{ register wchar c;
  c = soap_get2(soap);
  if (c < 0x80)
    return c;
  if (c < 0xC0)
    return ((wchar)(c&0x1F)<<6)|(wchar)(soap_get1(soap)&0x3F);
  if (c < 0xF0)
    return ((wchar)(c&0x0F)<<12)|((wchar)(soap_get1(soap)&0x3F)<<6)|(wchar)(soap_get1(soap)&0x3F);
  if (c < 0xF8)
    return ((wchar)(c&0x07)<<18)|((wchar)(soap_get1(soap)&0x3F)<<12)|((wchar)(soap_get1(soap)&0x3F)<<6)|(wchar)(soap_get1(soap)&0x3F);
  if (c < 0xFA)
    return ((wchar)(c&0x07)<<24)|((wchar)(soap_get1(soap)&0x3F)<<18)|((wchar)(soap_get1(soap)&0x3F)<<12)|((wchar)(soap_get1(soap)&0x3F)<<6)|(wchar)(soap_get1(soap)&0x3F);
  return ((wchar)(c&0x07)<<30)|((wchar)(soap_get1(soap)&0x3F)<<24)|((wchar)(soap_get1(soap)&0x3F)<<18)|((wchar)(soap_get1(soap)&0x3F)<<12)|((wchar)(soap_get1(soap)&0x3F)<<6)|(wchar)(soap_get1(soap)&0x3F);
}

/******************************************************************************/
SOAP_FMAC1
int
SOAP_FMAC2
soap_puthex(struct soap *soap, int n)
{ char d[3];
  d[0] = (n>>4) + (n > 159 ? '7' : '0');
  n &= 0x0F;
  d[1] = n + (n > 9 ? '7' : '0');
  d[2] = '\0';
  return soap_send(soap, d);
}

/******************************************************************************/
SOAP_FMAC1
int
SOAP_FMAC2
soap_gethex(struct soap *soap)
{ wchar c;
  char d1, d2;
  if ((c = soap_get(soap)) < 0)
  { soap_unget(soap, c);
    return EOF;
  }
  d1 = (char)c;
  if ((c = soap_get(soap)) < 0)
  { soap_unget(soap, c);
    return EOF;
  }
  d2 = (char)c;
  return ((d1 >= 'A' ? (d1&0x7) + 9 : d1 - '0') << 4) + (d2 >= 'A' ? (d2&0x7) + 9 : d2 - '0');
}

/******************************************************************************/
SOAP_FMAC1
int
SOAP_FMAC2
soap_putbase64(struct soap *soap, const unsigned char *s, size_t n)
{ register size_t i;
  register unsigned long m;
  char d[5];
  static const char base64[65] = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
  if (!s)
    return SOAP_OK;
  d[4] = '\0';
  for (; n > 2; n -= 3, s += 3)
  { m = (s[0]<<16) | (s[1]<<8) | s[2];
    for (i = 4; i > 0; m >>= 6)
      d[--i] = base64[m&0x3F];
    if (soap_send(soap, d))
      return soap->error;
  }
  if (n > 0)
  { m = 0;
    for (i = 0; i < n; i++)
      m = (m<<8) | *s++;
    for (; i < 3; i++)
      m <<= 8;
    for (i++; i > 0; m >>= 6)
      d[--i] = base64[m&0x3F];
    for (i = 3; i > n; i--)
      d[i] = '=';
    if (soap_send(soap, d))
      return soap->error;
  }
  return SOAP_OK;
}

/******************************************************************************/
SOAP_FMAC1
unsigned char*
SOAP_FMAC2
soap_getbase64(struct soap *soap, size_t *n, int malloc_flag)
{ static const char base64[81] = "\76XXX\77\64\65\66\67\70\71\72\73\74\75XXXXXXX\00\01\02\03\04\05\06\07\10\11\12\13\14\15\16\17\20\21\22\23\24\25\26\27\30\31XXXXXX\32\33\34\35\36\37\40\41\42\43\44\45\46\47\50\51\52\53\54\55\56\57\60\61\62\63";
  register int i, j;
  register wchar c;
  register unsigned long m;
  unsigned char *p;
  char *s;
  if (soap_new_block(soap))
    return NULL;
  for (;;)
  { s = (char*)soap_push_block(soap, 3*SOAP_BLKLEN);	/* must be multiple of 3 */
    for (i = 0; i < SOAP_BLKLEN; i++)
    { m = 0;
      j = 0;
      while (j < 4)
      { c = soap_get(soap);
        if (c == '=' || c < 0)
        { soap->blist->size += 3*(i-SOAP_BLKLEN);
	  switch (j)
	  { case 2:
              *s++ = (char)((m >> 4) & 0xFF);
	      soap->blist->size++;
	      break;
	    case 3:
              *s++ = (char)((m >> 10) & 0xFF);
              *s++ = (char)((m >> 2) & 0xFF);
	      soap->blist->size += 2;
          }
	  *n = soap->blist->size;
	  if (soap->blist->size > 0)
	    if (malloc_flag)
	      p = (unsigned char*)malloc(soap->blist->size);
	    else
	      p = (unsigned char*)soap_malloc(soap, soap->blist->size);
          else
	    p = NULL;
	  soap_store_block(soap, (char*)p);
	  if (c >= 0)
	    c = soap_advance(soap);
	  soap_unget(soap, c);
	  return p;
	}
        c -= '+';
        if (c >= 0 && c <= 79)
        { m = (m << 6) + base64[c];
	  j++;
        }
      }
      *s++ = (char)((m >> 16) & 0xFF);
      *s++ = (char)((m >> 8) & 0xFF);
      *s++ = (char)(m & 0xFF);
    }
  }
}

/******************************************************************************/
static char*
soap_strerror(struct soap *soap, int soaperror)
{
#ifndef UNDER_CE
  return strerror(soaperror);
#else
  FormatMessage(
    FORMAT_MESSAGE_FROM_SYSTEM |
    FORMAT_MESSAGE_IGNORE_INSERTS,
    NULL,
    soaperror,
    0,
    (LPTSTR) &soap->werrorstr,
    256,
    NULL
  );
  wcstombs(soap->errorstr, soap->werrorstr, 256);
  return soap->errorstr;
#endif
}

/******************************************************************************/
static void
soap_set_error(struct soap *soap, const char *faultcode, const char *faultstring, const char *faultdetail, int soaperror)
{ *soap_faultcode(soap) = faultcode;
  *soap_faultstring(soap) = faultstring;
  *soap_faultdetail(soap) = faultdetail;
  soap->error = soaperror;
}

/******************************************************************************/
SOAP_FMAC1
int
SOAP_FMAC2
soap_new_block(struct soap *soap)
{ struct soap_blist *p;
  DBGLOG(TEST, SOAP_MESSAGE(fdebug,"\nNew block sequence (prev=%p)", soap->blist));
  if (!(p = (struct soap_blist*)malloc(sizeof(struct soap_blist))))
    return SOAP_EOM;   
  p->next = soap->blist; 
  p->ptr = NULL;
  p->size = 0;
  soap->blist = p;
  return SOAP_OK;
}

/******************************************************************************/
SOAP_FMAC1
void*
SOAP_FMAC2
soap_push_block(struct soap *soap, size_t n)
{ char *p;
  DBGLOG(TEST, SOAP_MESSAGE(fdebug,"\nPush block %d (%d)", (int)n, soap->blist->size));
  if (!(p = (char*)malloc(n + sizeof(char*) + sizeof(size_t))))
    return NULL;
  *((char**)p) = soap->blist->ptr;
  *((size_t*)(p + sizeof(char*))) = n;
  soap->blist->ptr = p;
  soap->blist->size += n;
  return p + sizeof(char*) + sizeof(size_t);
}

/******************************************************************************/
SOAP_FMAC1
void
SOAP_FMAC2
soap_pop_block(struct soap *soap)
{ DBGLOG(TEST, SOAP_MESSAGE(fdebug,"\nPop block"));
  if (!soap->blist->ptr)
    return;
  soap->blist->size -= *((size_t*)(soap->blist->ptr + sizeof(char*)));
}

/******************************************************************************/
static void
soap_update_ptrs(struct soap *soap, char *start, char *end, long offset)
{ int i;
  struct soap_ilist *ip;
  void *p, **q;
  for (i = 0; i < SOAP_IDHASH; i++)
    for (ip = soap->iht[i]; ip; ip = ip->next)
      { if (ip->ptr && (char*)ip->ptr >= start && (char*)ip->ptr < end)
          ip->ptr = (char*)ip->ptr + offset;
        DBGLOG(TEST, SOAP_MESSAGE(fdebug,"\nTraversing link chain to update %s type %d at level %d", ip->id, ip->type, ip->level));
        for (q = &ip->link; q; q = (void**)p)
        { p = *q;
	  if (p && (char*)p >= start && (char*)p < end)
          { DBGLOG(TEST, SOAP_MESSAGE(fdebug,"Link update %p", p));
	    *q = (char*)p + offset;
          }
        }
        DBGLOG(TEST, SOAP_MESSAGE(fdebug,"\nTraversing copy chain to update %s type %d at level %d", ip->id, ip->type, ip->level));
        for (q = &ip->copy; q; q = (void**)p)
        { p = *q;
          DBGLOG(TEST, SOAP_MESSAGE(fdebug,"\nCheck location %p", p));
	  if (p && (char*)p >= start && (char*)p < end)
          { DBGLOG(TEST, SOAP_MESSAGE(fdebug,"\nCopy update %p", p));
	    *q = (char*)p + offset;
          }
        }
      }
  DBGLOG(TEST, SOAP_MESSAGE(fdebug,"\nPointers updated"));
}

/******************************************************************************/
static void
soap_resolve_ptr(struct soap_ilist *ip)
{ void *p, **q;
  q = (void**)ip->link;
  ip->link = NULL;
  while (q)
  { p = *q;
    *q = ip->ptr;
    q = (void**)p;
  }
}

/******************************************************************************/
static void
soap_resolve_copy(struct soap *soap, struct soap_ilist *ip)
{ void *p, **q;
  q = (void**)ip->copy;
  DBGLOG(TEST, if (q) SOAP_MESSAGE(fdebug,"\nTraversing copy chain to resolve type %s", ip->id));
  ip->copy = NULL;
  while (q) 
  { DBGLOG(TEST, SOAP_MESSAGE(fdebug,"\nstep... move %p -> %p (%d bytes)", ip->ptr, q, ip->size));
    p = *q;
    soap_update_ptrs(soap, (char*)q, (char*)q + ip->size, (char*)q - (char*)ip->ptr);
    memcpy(q, ip->ptr, ip->size);
    q = (void**)p;
  }
}

/******************************************************************************/
SOAP_FMAC1
void
SOAP_FMAC2
soap_store_block(struct soap *soap, char *p)
{ size_t n;
  int i;
  char *q, *s;
  struct soap_blist *bp;
  struct soap_ilist *ip;
  DBGLOG(TEST, SOAP_MESSAGE(fdebug,"\nStore all blocks, size = %d (%p->%p)", soap->blist->size, soap->blist->ptr, p));
  if (soap->blist->ptr)
  { n = *((size_t*)(soap->blist->ptr + sizeof(char*)));
    if (p)
    { if (n)
        n = soap->blist->size % n;
      p += soap->blist->size - n;
  DBGLOG(TEST, SOAP_MESSAGE(fdebug,"\nNo fit: start at the end (%p) remainder = %d", p, n));
      s = soap->blist->ptr + sizeof(char*) + sizeof(size_t);
      DBGLOG(TEST, SOAP_MESSAGE(fdebug,"\nCopy %d (%p->%p)", n, s, p));
      if (n)
      { soap_update_ptrs(soap, s, s+n, p-s);
        memcpy(p, s, n);
      }
      q = *((char**)soap->blist->ptr);
      free(soap->blist->ptr);
      DBGLOG(TEST, SOAP_MESSAGE(fdebug,"\nCopying into contiguous area..."));
      for (soap->blist->ptr = q; soap->blist->ptr; soap->blist->ptr = q)
      { q = *((char**)soap->blist->ptr);
        n = *((size_t*)(soap->blist->ptr + sizeof(char*)));
        p -= n;
        s = soap->blist->ptr + sizeof(char*) + sizeof(size_t);
        DBGLOG(TEST, SOAP_MESSAGE(fdebug,"\nCopy %d (%p->%p)", n, s, p));
        soap_update_ptrs(soap, s, s+n, p-s);
        memcpy(p, s, n);
        free(soap->blist->ptr);
      }
    }
    DBGLOG(TEST, SOAP_MESSAGE(fdebug,"\nEnd of block sequence"));
  }
  bp = soap->blist;
  soap->blist = soap->blist->next;
  free(bp);
  DBGLOG(TEST, if (soap->blist) SOAP_MESSAGE(fdebug,"\nRestore previous block sequence size=%d", soap->blist->size));
  if (!soap->blist)
  { /* get "delayed" backward pointers resolved NOW, instead of later */
    for (i = 0; i < SOAP_IDHASH; i++)
      for (ip = soap->iht[i]; ip; ip = ip->next)
        if (ip->ptr)
          soap_resolve_ptr(ip);
  }
}

/******************************************************************************/
SOAP_FMAC1
int
SOAP_FMAC2
soap_push_namespace(struct soap *soap, const char *id, const char *ns)
{ int i;
  struct soap_nlist *np = (struct soap_nlist*)malloc(sizeof(struct soap_nlist));
  if (!np)
    return SOAP_EOM;
  np->level = soap->level;
  np->next = soap->nlist;
  soap->nlist = np;
  np->id = (char*)malloc(strlen(id)+1);
  if (!np->id)
    return SOAP_EOM;
  strcpy(np->id, id);
  np->index = -1;
  DBGLOG(TEST, SOAP_MESSAGE(fdebug,"\nPush namespace binding (level=%d) '%s' '%s'", soap->level, id, ns));
  for (i = 0; soap->namespaces[i].id; i++)
  { if (soap->namespaces[i].ns)
      if (!soap_tag_cmp(ns, soap->namespaces[i].ns))
        break;
    if (soap->namespaces[i].in)
      if (!soap_tag_cmp(ns, soap->namespaces[i].in))
      { if (soap->namespaces[i].out)
          free(soap->namespaces[i].out);
        if ((soap->namespaces[i].out = (char*)malloc(strlen(ns)+1)))
          strcpy(soap->namespaces[i].out, ns);
        break;
      }
  }
  if (soap->namespaces[i].id)
  { DBGLOG(TEST, SOAP_MESSAGE(fdebug,"\nPush OK (match=%s)", soap->namespaces[i].id));
    np->index = i;
  }
  else
  { DBGLOG(TEST, SOAP_MESSAGE(fdebug,"\nPush NOT OK: no match found in namespace mapping table"));
  }
  return SOAP_OK;
}

/******************************************************************************/
SOAP_FMAC1
void
SOAP_FMAC2
soap_pop_namespace(struct soap *soap)
{ struct soap_nlist *np;
  while (soap->nlist && soap->nlist->level == soap->level)
  { np = soap->nlist->next;
    DBGLOG(TEST, SOAP_MESSAGE(fdebug,"\nPopped namespace binding (level=%d) '%s'", soap->level, soap->nlist->id));
    free(soap->nlist->id);
    free(soap->nlist);
    soap->nlist = np;
  }
}

/******************************************************************************/
SOAP_FMAC1
int
SOAP_FMAC2
soap_match_namespace(struct soap *soap, const char *id1, const char *id2, int n1, int n2) 
{ struct soap_nlist *np = soap->nlist;
  while (np && (strncmp(np->id, id1, n1) || np->id[n1]))
    np = np->next;
  if (np)
  { if (np->index < 0 || (np->index >= 0 && soap->namespaces[np->index].id && (strncmp(soap->namespaces[np->index].id, id2, n2) || soap->namespaces[np->index].id[n2])))
      return SOAP_NAMESPACE;
    return SOAP_OK;
  }
  return SOAP_SYNTAX_ERROR; 
}

/******************************************************************************/
SOAP_FMAC1
int
SOAP_FMAC2
soap_match_tag(struct soap *soap, const char *tag1, const char *tag2)
{ const char *s, *t;
  if (tag1 == NULL || tag2 == NULL)
    return SOAP_OK;
  s = strchr(tag1, ':');
  t = strchr(tag2, ':');
  if (s)
  { if (t)
    { if (soap_match_namespace(soap, tag1, tag2, s-tag1, t-tag2))
      { DBGLOG(TEST, SOAP_MESSAGE(fdebug,"\nNamespace mismatch in tag comparison: '%s' '%s'", tag1, tag2));
	return SOAP_TAG_MISMATCH;
      }
      t++;
    } 
    else
      t = tag2;
    s++;
  }
  else if (t)
  { s = tag1;
    t++;
  }
  else
  { s = tag1;
    t = tag2;
  }
  if (soap_tag_cmp(s, t))
    return SOAP_TAG_MISMATCH;
  DBGLOG(TEST, SOAP_MESSAGE(fdebug,"\nTags match: '%s' '%s'", tag1, tag2));
  return SOAP_OK;
}

/******************************************************************************/
SOAP_FMAC1
int
SOAP_FMAC2
soap_match_array(struct soap *soap, const char *type)
{ if (*soap->type && *soap->arrayType)
    if (soap_match_tag(soap, soap->arrayType, type)
     && soap_match_tag(soap, soap->arrayType, "xsd:anyType")
     && soap_match_tag(soap, soap->arrayType, "xsd:ur-type")
    )
    { DBGLOG(TEST, SOAP_MESSAGE(fdebug,"\nArray type mismatch: '%s' '%s'", soap->arrayType, type));
      return SOAP_TAG_MISMATCH;
    }
  return SOAP_OK;
}

/******************************************************************************/

#ifdef WITH_OPENSSL
/******************************************************************************/
static int
ssl_init()
{ static int done = 0;
  if (done)
    return 0;
  done = 1;
  SSL_library_init();
  SSL_load_error_strings();
  soap_ssl_ctx = SSL_CTX_new(SSLv23_method());
  return !soap_ssl_ctx;
}

/******************************************************************************/
static const char *
ssl_error(struct soap *soap, int ret)
{ switch (SSL_get_error(soap->ssl, ret))
  { 
#ifdef SSL_ERROR_NONE
    case SSL_ERROR_NONE:
      return "SSL_ERROR_NONE";
#endif
#ifdef SSL_ERROR_ZERO_RETURN
    case SSL_ERROR_ZERO_RETURN:
      return "SSL_ERROR_ZERO_RETURN";
#endif
#ifdef SSL_ERROR_WANT_READ
    case SSL_ERROR_WANT_READ:
      return "SSL_ERROR_WANT_READ";
#endif
#ifdef SSL_ERROR_WANT_WRITE
    case SSL_ERROR_WANT_WRITE:
      return "SSL_ERROR_WANT_WRITE";
#endif
#ifdef SSL_ERROR_WANT_CONNECT
    case SSL_ERROR_WANT_CONNECT:
      return "SSL_ERROR_WANT_CONNECT";
#endif
#ifdef SSL_ERROR_WANT_ACCEPT
    case SSL_ERROR_WANT_ACCEPT:
      return "SSL_ERROR_WANT_ACCEPT";
#endif
#ifdef SSL_ERROR_WANT_X509_LOOKUP
    case SSL_ERROR_WANT_X509_LOOKUP:
      return "SSL_ERROR_WANT_X509_LOOKUP";
#endif
#ifdef SSL_ERROR_SYSCALL
    case SSL_ERROR_SYSCALL:
      return "SSL_ERROR_SYSCALL";
#endif
#ifdef SSL_ERROR_SSL
    case SSL_ERROR_SSL:
      return "SSL_ERROR_SSL";
#endif
  }
  return "Unknown SSL error";
}

/******************************************************************************/
static int
fpassword(char *buf, int num, int rwflag, void *userdata)
{ if (num < strlen(soap_ssl_pw)+1)
    return(0);
  strcpy(buf, soap_ssl_pw);
  return strlen(soap_ssl_pw);
}

/******************************************************************************/
static int
ssl_auth_init(struct soap *soap)
{ ssl_init();
  if (soap->keyfile)
    if (!(SSL_CTX_use_certificate_chain_file(soap_ssl_ctx, soap->keyfile)))
    { *soap_faultcode(soap) = "SOAP-ENV:Server";
      *soap_faultstring(soap) = "Can't read certificate file";
      return SOAP_SSL_ERROR;
    }
  if (soap->password && soap->keyfile)
  { soap_ssl_pw = soap->password;
    SSL_CTX_set_default_passwd_cb(soap_ssl_ctx, fpassword);
    if (!(SSL_CTX_use_PrivateKey_file(soap_ssl_ctx, soap->keyfile, SSL_FILETYPE_PEM)))
    { *soap_faultcode(soap) = "SOAP-ENV:Server";
      *soap_faultstring(soap) = "Can't read key file";
      return SOAP_SSL_ERROR;
    }
  }
  if (soap->cafile)
    if (!(SSL_CTX_load_verify_locations(soap_ssl_ctx, soap->cafile, 0)))
    { *soap_faultcode(soap) = "SOAP-ENV:Server";
      *soap_faultstring(soap) = "Can't read CA list";
      return SOAP_SSL_ERROR;
    }
  if (soap->dhfile)
  { DH *r = 0;
    BIO *bio;
    bio = BIO_new_file(soap->dhfile, "r");
    if (!bio)
    { *soap_faultcode(soap) = "SOAP-ENV:Server";
      *soap_faultstring(soap) = "Can't read DH file";
      return SOAP_SSL_ERROR;
    }
    r = PEM_read_bio_DHparams(bio, NULL, NULL, NULL);
    BIO_free(bio);
    if (SSL_CTX_set_tmp_dh(soap_ssl_ctx, r) < 0)
    { *soap_faultcode(soap) = "SOAP-ENV:Server";
      *soap_faultstring(soap) = "Can't set DH parameters";
      return SOAP_SSL_ERROR;
    }
  }
#if (OPENSSL_VERSION_NUMBER < 0x00905100L)
  SSL_CTX_set_verify_depth(soap_ssl_ctx, 1); 
#endif  
  return SOAP_OK;
}

/******************************************************************************/
SOAP_FMAC1
int
SOAP_FMAC2
soap_ssl_accept(struct soap *soap)
{ int r;
  soap->error = ssl_auth_init(soap);
  if (soap->error)
    return soap->error;
  if (soap->socket <= 0)
  { *soap_faultcode(soap) = "SOAP-ENV:Server";
    *soap_faultstring(soap) = "No socket in soap_ssl_accept()";
    return soap->error = SOAP_SSL_ERROR;
  } 
  soap->ssl = SSL_new(soap_ssl_ctx);
  if (!soap->ssl)
  { *soap_faultcode(soap) = "SOAP-ENV:Server";
    *soap_faultstring(soap) = "SSL_new failed in soap_ssl_accept()";
    return soap->error = SOAP_SSL_ERROR;
  }
  soap->bio = BIO_new_socket(soap->socket, BIO_NOCLOSE);
  SSL_set_bio(soap->ssl, soap->bio, soap->bio);
  r = SSL_accept(soap->ssl);
  if (r <= 0)
  { *soap_faultcode(soap) = "SOAP-ENV:Server";
    *soap_faultstring(soap) = "SSL_accept failed in soap_ssl_accept()";
    *soap_faultdetail(soap) = ssl_error(soap, r);
    return soap->error = SOAP_SSL_ERROR;
  }
  return SOAP_OK;
}

/******************************************************************************/
#endif

/******************************************************************************/
static int
tcp_init(struct soap *soap)
{ soap->errmode = 1;
#ifdef WIN32
  if (tcp_done)
    return 0;
  tcp_done = 1;
  { WSADATA w;
    if (WSAStartup(MAKEWORD(1, 1), &w))
    { tcp_done = 0;
      return -1;
    }
  }
#endif
  return 0;
}

/******************************************************************************/
SOAP_FMAC1
void
SOAP_FMAC2
soap_done(struct soap *soap)
{ soap_closesock(soap);
  if (soap->master >= 0)
  { closesocket(soap->master);
    soap->master = -1;
  }
  soap->fpost = http_post;
  soap->fresponse = http_response;
  soap->fparse = http_parse;
  soap->fopen = tcp_connect;
  soap->fclose = tcp_disconnect;
  soap->fsend = fsend;
  soap->frecv = frecv;
  soap->fignore = fignore;
#ifdef WIN32
  if (!tcp_done)
    return;
  tcp_done = 0;
  WSACleanup();
#endif
}

/******************************************************************************/
static const char*
tcp_error(struct soap *soap)
{ const char *msg = NULL;
  switch (soap->errmode)
  { case 0:
      msg = soap_strerror(soap, soap->errnum);
      break;
    case 1:
      msg = "WSAStartup failed";
      break;
    case 2:
    { struct code_map *map = h_errno_codes;
      while (map->code && map->code != soap->errnum)
	map++;
      if (map->code)
	msg = map->string;
      else
      { sprintf(soap->msgbuf, "Unknown TCP error code %d\n", soap->errnum);
	msg = soap->msgbuf;
      }
    }
  }
  return msg;
}

/******************************************************************************/
static int
soap_gethost(struct soap *soap, const char *addr, struct in_addr *inaddr)
{ unsigned long iadd;
  struct hostent hostent, *host = &hostent;
  iadd = inet_addr(addr);
  if ((int)iadd != -1)
  { memcpy(inaddr, &iadd, sizeof(iadd));
    return 0;
  }
#ifdef __GLIBC__
  if (gethostbyname_r(addr, &hostent, soap->buf, SOAP_BUFLEN, &host, &soap->errnum) < 0)
    host = NULL;
#else
#if defined(WIN32) || defined(_AIXVERSION_431) || defined(__APPLE__)
  if (!(host = gethostbyname(addr)))
    soap->errnum = h_errno;
#else
  host = gethostbyname_r(addr, &hostent, soap->buf, SOAP_BUFLEN, &soap->errnum);
#endif
#endif
  if (!host)
  { DBGLOG(TEST, SOAP_MESSAGE(fdebug,"\nHost name not found"));
    return -1;
  }
  memcpy(inaddr, host->h_addr, host->h_length);
  return 0;
}

/******************************************************************************/
static int
tcp_connect(struct soap *soap, const char *endpoint, const char *hostname, int port)
{ struct sockaddr_in sockaddr;
  int len = SOAP_BUFLEN;
  int set = 1;
  if (tcp_init(soap))
  { soap_set_error(soap, "SOAP-ENV:Client", tcp_error(soap), "TCP initialization failed in tcp_connect()", SOAP_TCP_ERROR);
    return -1;
  }
  if (soap->socket >= 0)
    closesocket(soap->socket);
  soap->errmode = 0;
  if ((soap->socket = socket(AF_INET, SOCK_STREAM, 0)) < 0)
  { soap->errnum = soap_errno;
    soap_set_error(soap, "SOAP-ENV:Client", tcp_error(soap), "TCP socket failed in tcp_connect()", SOAP_TCP_ERROR);
    return -1;
  }
  if (soap->keep_alive && setsockopt(soap->socket, SOL_SOCKET, SO_KEEPALIVE, (char*)&set, sizeof(int)))
  { soap->errnum = soap_errno;
    soap_set_error(soap, "SOAP-ENV:Client", tcp_error(soap), "TCP setsockopt SO_KEEPALIVE failed in tcp_connect()", SOAP_TCP_ERROR);
    return -1;
  }
#ifndef UNDER_CE
  if (setsockopt(soap->socket, SOL_SOCKET, SO_SNDBUF, (char*)&len, sizeof(int)))
  { soap->errnum = soap_errno;
    soap_set_error(soap, "SOAP-ENV:Client", tcp_error(soap), "TCP setsockopt SO_SNDBUF failed in tcp_connect()", SOAP_TCP_ERROR);
    return -1;
  }
  if (setsockopt(soap->socket, SOL_SOCKET, SO_RCVBUF, (char*)&len, sizeof(int)))
  { soap->errnum = soap_errno;
    soap_set_error(soap, "SOAP-ENV:Client", tcp_error(soap), "TCP setsockopt SO_RCVBUF failed in tcp_connect()", SOAP_TCP_ERROR);
    return -1;
  }
#endif
#ifdef TCP_NODELAY
  if (setsockopt(soap->socket, IPPROTO_TCP, TCP_NODELAY, (char*)&set, sizeof(int)))
  { soap->errnum = soap_errno;
    soap_set_error(soap, "SOAP-ENV:Client", tcp_error(soap), "TCP setsockopt TCP_NODELAY failed in tcp_connect()", SOAP_TCP_ERROR);
    return -1;
  }
#endif
  memset(&sockaddr, 0, sizeof(sockaddr));
  sockaddr.sin_family = AF_INET;
  DBGLOG(TEST,SOAP_MESSAGE(fdebug,"\nOpen socket: %d to hostname '%s'", soap->socket, hostname));
  soap->errmode = 2;
  if (soap->proxy_host)
  { if (soap_gethost(soap, soap->proxy_host, &sockaddr.sin_addr))
    { soap_set_error(soap, "SOAP-ENV:Client", tcp_error(soap), "TCP get proxy host by name failed in tcp_connect()", SOAP_TCP_ERROR);
      return -1;
    }
    sockaddr.sin_port = htons((short)soap->proxy_port);
  }
  else
  { if (soap_gethost(soap, hostname, &sockaddr.sin_addr))
    { soap_set_error(soap, "SOAP-ENV:Client", tcp_error(soap), "TCP get host by name failed in tcp_connect()", SOAP_TCP_ERROR);
      return -1;
    }
    sockaddr.sin_port = htons((short)port);
  }
  soap->errmode = 0;
  if (soap->connect_timeout)
#ifdef WIN32
  { u_long nonblocking = 1;
    ioctlsocket(soap->socket, FIONBIO, &nonblocking);
  }
#else
    fcntl(soap->socket, F_SETFL, fcntl(soap->socket, F_GETFL)|O_NONBLOCK);
#endif
  for (;;)
  { if (connect(soap->socket, (struct sockaddr*)&sockaddr, sizeof(sockaddr)))
    { if (soap_errno == SOAP_EINPROGRESS && soap->connect_timeout)
      { struct timeval timeout;
        fd_set fd;
        if (soap->connect_timeout > 0)
        { timeout.tv_sec = soap->connect_timeout;
          timeout.tv_usec = 0;
        }
        else
        { timeout.tv_sec = -soap->connect_timeout/1000000;
          timeout.tv_usec = -soap->connect_timeout%1000000;
        }
        FD_ZERO(&fd);
        FD_SET(soap->socket, &fd);
        for (;;)
        { if (select(soap->socket + 1, NULL, &fd, NULL, &timeout) > 0)
            break;
          if (soap_errno != SOAP_EINTR)
          { soap->errnum = soap_errno;
            DBGLOG(TEST, SOAP_MESSAGE(fdebug, "\nCould not connect to host"));
            soap_closesock(soap);
            soap_set_error(soap, "SOAP-ENV:Client", "Timeout", "TCP connect failed in tcp_connect()", SOAP_TCP_ERROR);
            return -1;
          }
        }
	break;
      }
      else if (soap_errno != SOAP_EINTR)
      { soap->errnum = soap_errno;
        DBGLOG(TEST, SOAP_MESSAGE(fdebug, "\nCould not connect to host"));
        soap_closesock(soap);
        soap_set_error(soap, "SOAP-ENV:Client", tcp_error(soap), "TCP connect failed in tcp_connect()", SOAP_TCP_ERROR);
        return -1;
      }
    }  
    else
      break;
  }
  if (soap->connect_timeout)
#ifdef WIN32
  { u_long blocking = 0;
    ioctlsocket(soap->socket, FIONBIO, &blocking);
  }
#else
    fcntl(soap->socket, F_SETFL, fcntl(soap->socket, F_GETFL)&~O_NONBLOCK);
#endif
#ifdef WITH_OPENSSL
  if (!strncmp(endpoint, "https:", 6))
  { int r;
    if (ssl_auth_init(soap))
    { soap_set_error(soap, "SOAP-ENV:Client", "", "SSL initialization failed in tcp_connect()", SOAP_SSL_ERROR);
      return -1;
    }
    soap->ssl = SSL_new(soap_ssl_ctx);
    if (!soap->ssl)
    { soap->error = SOAP_SSL_ERROR;
      return -1;
    }
    soap->bio = BIO_new_socket(soap->socket, BIO_NOCLOSE);
    SSL_set_bio(soap->ssl, soap->bio, soap->bio);
    if ((r = SSL_connect(soap->ssl)) <= 0)
    { soap_set_error(soap, "SOAP-ENV:Client", ssl_error(soap, r), "SSL connect failed in tcp_connect()", SOAP_SSL_ERROR);
      return -1;
    }
    if (soap->require_server_auth)
    { X509 *peer;
      if (SSL_get_verify_result(soap->ssl) != X509_V_OK)
      { soap_set_error(soap, "SOAP-ENV:Client", "", "SSL certificate cannot be verified in tcp_connect()", SOAP_SSL_ERROR);
	return -1;
      }
      peer = SSL_get_peer_certificate(soap->ssl);
      X509_NAME_get_text_by_NID(X509_get_subject_name(peer), NID_commonName, soap->msgbuf, 1024);
      if (strcasecmp(soap->msgbuf, hostname))
      { soap_set_error(soap, "SOAP-ENV:Client", "", "SSL certificate host name mismatch in tcp_connect()", SOAP_SSL_ERROR);
	return -1;
      }
    }
  }
#endif
  soap->buffering = 1;
  return soap->socket;
}

/******************************************************************************/
SOAP_FMAC1
int
SOAP_FMAC2
soap_bind(struct soap *soap, const char *hostname, int port, int backlog)
{ struct sockaddr_in sockaddr;
  int len = SOAP_BUFLEN;
  int set = 1;
  soap->master = -1;
  soap->socket = -1;
  soap->errmode = 1;
  if (tcp_init(soap))
  { soap_set_error(soap, "SOAP-ENV:Server", tcp_error(soap), "TCP init failed in soap_bind()", SOAP_TCP_ERROR);
    return -1;
  }
  soap->errmode = 0;
  if ((soap->master = socket(AF_INET, SOCK_STREAM, 0)) < 0)
  { soap->errnum = soap_errno;
    soap_set_error(soap, "SOAP-ENV:Server", tcp_error(soap), "TCP socket failed in soap_bind()", SOAP_TCP_ERROR);
    return -1;
  }
  if (setsockopt(soap->master, SOL_SOCKET, SO_REUSEADDR, (char*)&set, sizeof(int)))
  { soap->errnum = soap_errno;
    soap_set_error(soap, "SOAP-ENV:Client", tcp_error(soap), "TCP setsockopt SO_REUSEADDR failed in soap_bind()", SOAP_TCP_ERROR);
    return -1;
  }
  if (soap->keep_alive && setsockopt(soap->master, SOL_SOCKET, SO_KEEPALIVE, (char*)&set, sizeof(int)))
  { soap->errnum = soap_errno;
    soap_set_error(soap, "SOAP-ENV:Client", tcp_error(soap), "TCP setsockopt SO_KEEPALIVE failed in soap_bind()", SOAP_TCP_ERROR);
    return -1;
  }
#ifndef UNDER_CE
  if (setsockopt(soap->master, SOL_SOCKET, SO_SNDBUF, (char*)&len, sizeof(int)))
  { soap->errnum = soap_errno;
    soap_set_error(soap, "SOAP-ENV:Client", tcp_error(soap), "TCP setsockopt SO_SNDBUF failed in soap_bind()", SOAP_TCP_ERROR);
    return -1;
  }
  if (setsockopt(soap->master, SOL_SOCKET, SO_RCVBUF, (char*)&len, sizeof(int)))
  { soap->errnum = soap_errno;
    soap_set_error(soap, "SOAP-ENV:Client", tcp_error(soap), "TCP setsockopt SO_RCVBUF failed in soap_bind()", SOAP_TCP_ERROR);
    return -1;
  }
#endif
#ifdef TCP_NODELAY
  if (setsockopt(soap->master, IPPROTO_TCP, TCP_NODELAY, (char*)&set, sizeof(int)))
  { soap->errnum = soap_errno;
    soap_set_error(soap, "SOAP-ENV:Client", tcp_error(soap), "TCP setsockopt TCP_NODELAY failed in soap_bind()", SOAP_TCP_ERROR);
    return -1;
  }
#endif
  memset(&sockaddr, 0, sizeof(sockaddr));
  sockaddr.sin_family = AF_INET;
  soap->errmode = 2;
  if (hostname)
  { if (soap_gethost(soap, hostname, &sockaddr.sin_addr))
    { soap_set_error(soap, "SOAP-ENV:Server", tcp_error(soap), "TCP get host by name failed in soap_bind()", SOAP_TCP_ERROR);
      return -1;
    }
  }
  else
    sockaddr.sin_addr.s_addr = htonl(INADDR_ANY);
  sockaddr.sin_port = htons((short)port);
  soap->errmode = 0;
  if (bind(soap->master, (struct sockaddr*)&sockaddr, sizeof(sockaddr)) || listen(soap->master, backlog))
  { soap->errnum = soap_errno;
    DBGLOG(TEST, SOAP_MESSAGE(fdebug, "\nCould not bind to host"));
    soap_closesock(soap);
    soap_set_error(soap, "SOAP-ENV:Server", tcp_error(soap), "TCP bind failed in soap_bind()", SOAP_TCP_ERROR);
    return -1;
  }  
  return soap->master;
}

/******************************************************************************/
SOAP_FMAC1
int
SOAP_FMAC2
soap_accept(struct soap *soap)
{ struct sockaddr_in sockaddr;
  int len = SOAP_BUFLEN;
  int set = 1;
#if defined(__socklen_t_defined) || defined(_SOCKLEN_T)
  socklen_t n = sizeof(struct sockaddr_in);
#else
  int n = sizeof(struct sockaddr_in);
#endif
  memset(&sockaddr, 0, sizeof(sockaddr));
  soap->socket = -1;
  soap->errmode = 0;
  if (soap->master > 0)
  { for (;;)
    { if (soap->accept_timeout)
      { struct timeval timeout;
        fd_set fd;
        if (soap->accept_timeout > 0)
	{ timeout.tv_sec = soap->accept_timeout;
          timeout.tv_usec = 0;
	}
	else
	{ timeout.tv_sec = -soap->accept_timeout/1000000;
          timeout.tv_usec = -soap->accept_timeout%1000000;
	}
        FD_ZERO(&fd);
        FD_SET(soap->master, &fd);
        for (;;)
        { if (select(soap->master + 1, &fd, &fd, NULL, &timeout) > 0)
            break;
          if (soap_errno != SOAP_EINTR)
          { soap->errnum = soap_errno;
            soap_set_error(soap, "SOAP-ENV:Server", "Timeout", "TCP accept failed in soap_accept()", SOAP_TCP_ERROR);
	    return -1;
	  }
        }
#ifdef WIN32
        { u_long nonblocking = 1;
          ioctlsocket(soap->master, FIONBIO, &nonblocking);
        }
#else
        fcntl(soap->master, F_SETFL, fcntl(soap->master, F_GETFL)|O_NONBLOCK);
#endif
      }
      if ((soap->socket = accept(soap->master, (struct sockaddr*)&sockaddr, &n)) >= 0)
      { soap->ip = ntohl(sockaddr.sin_addr.s_addr);
        if (soap->keep_alive && setsockopt(soap->socket, SOL_SOCKET, SO_KEEPALIVE, (char*)&set, sizeof(int)))
        { soap->errnum = soap_errno;
          soap_set_error(soap, "SOAP-ENV:Server", tcp_error(soap), "TCP setsockopt SO_KEEPALIVE failed in soap_accept()", SOAP_TCP_ERROR);
          return -1;
        }
#ifndef UNDER_CE
        if (setsockopt(soap->socket, SOL_SOCKET, SO_SNDBUF, (char*)&len, sizeof(int)))
        { soap->errnum = soap_errno;
          soap_set_error(soap, "SOAP-ENV:Server", tcp_error(soap), "TCP setsockopt SO_SNDBUF failed in soap_accept()", SOAP_TCP_ERROR);
          return -1;
        }
        if (setsockopt(soap->socket, SOL_SOCKET, SO_RCVBUF, (char*)&len, sizeof(int)))
        { soap->errnum = soap_errno;
          soap_set_error(soap, "SOAP-ENV:Server", tcp_error(soap), "TCP setsockopt SO_RCVBUF failed in soap_accept()", SOAP_TCP_ERROR);
          return -1;
        }
#endif
#ifdef TCP_NODELAY
        if (setsockopt(soap->socket, IPPROTO_TCP, TCP_NODELAY, (char*)&set, sizeof(int)))
        { soap->errnum = soap_errno;
          soap_set_error(soap, "SOAP-ENV:Server", tcp_error(soap), "TCP setsockopt TCP_NODELAY failed in soap_accept()", SOAP_TCP_ERROR);
          return -1;
        }
#endif
        return soap->socket;
      }
      else if (soap_errno != SOAP_EINTR && soap_errno != SOAP_EWOULDBLOCK)
      { soap->errnum = soap_errno;
        soap_set_error(soap, "SOAP-ENV:Server", tcp_error(soap), "TCP accept failed in soap_accept()", SOAP_TCP_ERROR);
        return -1;
      }
    }
  }
  else
  { soap_set_error(soap, "SOAP-ENV:Server", "", "TCP no master socket in soap_accept()", SOAP_TCP_ERROR);
    return -1;
  }
}

/******************************************************************************/
static int
tcp_disconnect(struct soap *soap)
{
#ifdef WITH_OPENSSL
  if (soap->ssl)
  { int r = SSL_shutdown(soap->ssl);
    if (!r)
    { if (soap->socket >= 0)
        shutdown(soap->socket, 1);
      r = SSL_shutdown(soap->ssl);
    }
    DBGLOG(TEST, if (r != 1) SOAP_MESSAGE(fdebug, "\nShutdown failed: %d", SSL_get_error(soap->ssl, r)));
    SSL_free(soap->ssl);
    soap->ssl = NULL;
    if (r != 1)
      return SOAP_SSL_ERROR;
  }
#endif
  if (soap->socket >= 0)
  { DBGLOG(TEST,SOAP_MESSAGE(fdebug,"\nClosing socket %d", soap->socket));
    closesocket(soap->socket);
    soap->socket = -1;
  }
  return SOAP_OK;
}

/******************************************************************************/
SOAP_FMAC1
int
SOAP_FMAC2
soap_closesock(struct soap *soap)
{ if (!soap->keep_alive)
    return soap->error = soap->fclose(soap);
  return SOAP_OK;
}

/******************************************************************************/
int
hash_id(const char *s)
{ register int h = 0;
  while (*s)
    h += *s++&0x1F;
  return h%SOAP_IDHASH;
}

/******************************************************************************/
SOAP_FMAC1
void
SOAP_FMAC2
soap_init_pht(struct soap *soap)
{ int i;
  DBGLOG(TEST, SOAP_MESSAGE(fdebug, "\nInitializing pointer hashtable"));
  for (i = 0; i < SOAP_PTRHASH; i++)
    soap->pht[i] = NULL;
}

/******************************************************************************/
SOAP_FMAC1
struct soap*
SOAP_FMAC2
soap_new()
{ struct soap *soap = (struct soap*)malloc(sizeof(struct soap));
  if (soap)
    soap_init(soap);
  return soap;
}

/******************************************************************************/
SOAP_FMAC1
void
SOAP_FMAC2
soap_free_pht(struct soap *soap)
{ struct soap_plist *pp, *next;
  int i;
  for (i = 0; i < SOAP_PTRHASH; i++)
  { for (pp = soap->pht[i]; pp; pp = next)
    { next = pp->next;
      free(pp);
    }
    soap->pht[i] = NULL;
  }
}

/******************************************************************************/
SOAP_FMAC1
int
SOAP_FMAC2
soap_pointer_lookup(struct soap *soap, const void *p, int type, struct soap_plist **ppp)
{ struct soap_plist *pp;
  *ppp = NULL;
  if (!p)
    return 0;
  for (pp = soap->pht[hash_ptr(p)]; pp; pp = pp->next)
    if (pp->ptr == p && pp->type == type)
    { *ppp = pp;
      DBGLOG(TEST, SOAP_MESSAGE(fdebug, "\nLookup location=%p type=%d id=%d", p, type, pp->id));
      return pp->id;
    }
  DBGLOG(TEST, SOAP_MESSAGE(fdebug, "\nLookup location=%p type=%d: not found", p, type));
  return 0;
}

/******************************************************************************/
SOAP_FMAC1
int
SOAP_FMAC2
soap_array_pointer_lookup(struct soap *soap, const void *p, int n, int type, struct soap_plist **ppp)
{ struct soap_plist *pp;
  *ppp = NULL;
  if (!p || !*(void**)p)
    return 0;
  type |= 1024;
  for (pp = soap->pht[hash_ptr(*(void**)p)]; pp; pp = pp->next)
    if (pp->type == type && *(void**)pp->ptr == *(void**)p && *((int*)pp->ptr+1) == n)
    { *ppp = pp;
      DBGLOG(TEST, SOAP_MESSAGE(fdebug, "\nArray lookup location=%p type=%d id=%d", p, type, pp->id));
      return pp->id;
    }
  DBGLOG(TEST, SOAP_MESSAGE(fdebug, "\nArray lookup location=%p type=%d: not found", p, type));
  return 0;
}

/******************************************************************************/
SOAP_FMAC1
int
SOAP_FMAC2
soap_pointer_enter(struct soap *soap, const void *p, int type, struct soap_plist **ppp)
{ int h;
  struct soap_plist *pp;
  if (!p)
  { *ppp = NULL;
    return 0;
  }
  DBGLOG(TEST, SOAP_MESSAGE(fdebug,"\nEnter location=%p type=%d id=%d", p, type, soap->idnum+1));
  *ppp = pp = (struct soap_plist*)malloc(sizeof(struct soap_plist));
  if (pp)
  { h = hash_ptr(p);
    pp->next = soap->pht[h];
    pp->type = type;
    if (soap->disable_href || soap->is_in_header)
    { pp->mark1 = 0;
      pp->mark2 = 0;
    }
    else
    { pp->mark1 = 1;
      pp->mark2 = 1;
    }
    pp->ptr = (void*)p;
    pp->id = ++soap->idnum;
    soap->pht[h] = pp;
  }
  else
    return 0;
  return pp->id;
}

/******************************************************************************/
SOAP_FMAC1
int
SOAP_FMAC2
soap_array_pointer_enter(struct soap *soap, const void *p, int type, struct soap_plist **ppp)
{ int h;
  struct soap_plist *pp;
  if (!p || !*(void**)p)
  { *ppp = NULL;
    return 0;
  }
  DBGLOG(TEST, SOAP_MESSAGE(fdebug,"\nArray enter location=%p type=%d id=%d", p, type|1024, soap->idnum+1));
  pp = (struct soap_plist*)malloc(sizeof(struct soap_plist));
  *ppp = pp;
  if (pp)
  { h = hash_ptr(*(void**)p);
    pp->next = soap->pht[h];
    pp->type = type|1024;
    if (soap->disable_href || soap->is_in_header)
    { pp->mark1 = 0;
      pp->mark2 = 0;
    }
    else
    { pp->mark1 = 1;
      pp->mark2 = 1;
    }
    pp->ptr = (void*)p;
    pp->id = ++soap->idnum;
    soap->pht[h] = pp;
  }
  else
    return 0;
  return pp->id;
}

/******************************************************************************/
SOAP_FMAC1
void
SOAP_FMAC2
soap_begin_count(struct soap *soap)
{ soap->counting = 1;
  soap->count = 0;
  soap->ns = 0;
  soap->null = 0;
  soap->position = 0;
  soap->mustUnderstand = 0;
  soap->dime_count = 0;	/* count # of attachments */
  soap->dime_size = 0;	/* accumulate total size of attachments */
  DBGLOG(TEST, SOAP_MESSAGE(fdebug,"\nBegin count"));
}

/******************************************************************************/
SOAP_FMAC1
void
SOAP_FMAC2
soap_begin_send(struct soap *soap)
{ soap->chunked = 0;
  soap->counting = 0;
  soap->ns = 0;
  if (soap->socket >= 0)
    soap->buffering = 1;
  soap->bufidx = 0;
  soap->null = 0;
  soap->position = 0;
  soap->mustUnderstand = 0;
  DBGLOG(TEST, SOAP_MESSAGE(fdebug,"\nBegin send (socket=%d buffered=%d chunked=%d)", soap->socket, (int)soap->buffering, (int)soap->chunked));
}

/******************************************************************************/
SOAP_FMAC1
void
SOAP_FMAC2
soap_embedded(struct soap *soap, const void *p, int t)
{ struct soap_plist *pp;
  DBGLOG(TEST, SOAP_MESSAGE(fdebug,"\nembedded %p type=%d", p, t));
  if (soap_pointer_lookup(soap, p, t, &pp))
  { pp->mark1 = 1;
    pp->mark2 = 1;
    DBGLOG(TEST, SOAP_MESSAGE(fdebug,"\nembedded %p type=%d set to %d", p, t, (int)pp->mark1));
  }
}

/******************************************************************************/
SOAP_FMAC1
int
SOAP_FMAC2
soap_reference(struct soap *soap, const void *p, int t)
{ int i;
  struct soap_plist *pp;
  if (!p)
    return 1;
  i = soap_pointer_lookup(soap, p, t, &pp);
  if (i)
  { if (pp->mark1 == 0)
    { pp->mark1 = 2;
      pp->mark2 = 2;
    }
  }
  else
  { soap_pointer_enter(soap, p, t, &pp);
    pp->mark1 = 0;
    pp->mark2 = 0;
  }
  DBGLOG(TEST, SOAP_MESSAGE(fdebug,"\nreference %p type = %d (%d %d)", p, t, (int)pp->mark1, (int)pp->mark2));
  return pp->mark1;
}

/******************************************************************************/
SOAP_FMAC1
int
SOAP_FMAC2
soap_array_reference(struct soap *soap, const void *p, int n, int t)
{ int i;
  struct soap_plist *pp;
  if (!p)
    return 1;
  i = soap_array_pointer_lookup(soap, p, n, t, &pp);
  if (i)
  { if (pp->mark1 == 0)
    { pp->mark1 = 2;
      pp->mark2 = 2;
    }
  }
  else
  { soap_array_pointer_enter(soap, p, t, &pp);
    pp->mark1 = 0;
    pp->mark2 = 0;
  }
  DBGLOG(TEST, SOAP_MESSAGE(fdebug,"\narray reference %p type = %d (%d %d)", p, t, (int)pp->mark1, (int)pp->mark2));
  return pp->mark1;
}

/******************************************************************************/
SOAP_FMAC1
int
SOAP_FMAC2
soap_embedded_id(struct soap *soap, int id, const void *p, int t)
{ struct soap_plist *pp;
  DBGLOG(TEST, SOAP_MESSAGE(fdebug,"\nembedded_id %p type=%d id=%d", p, t, id));
  if (!soap->enable_embedding && !soap->disable_href && !soap->is_in_header)
  { if (id < 0)
    { id = soap_pointer_lookup(soap, p, t, &pp);
      if (id > 0 && pp)
      { if (soap->counting)
          pp->mark1 = 2;
        else
          pp->mark2 = 2;
      }
      return -1;
    }
    return id;
  }
  if (id < 0)
    id = soap_pointer_lookup(soap, p, t, &pp);
  else
    soap_pointer_lookup(soap, p, t, &pp);
  if (id > 0 && pp)
  { if (soap->counting)
      pp->mark1 = 1;
    else
      pp->mark2 = 1;
  DBGLOG(TEST, SOAP_MESSAGE(fdebug,"\nembedded_id id=%d %p type=%d = (%d %d)", id, p, t, (int)pp->mark1, (int)pp->mark2));
  }
  return id;
}

/******************************************************************************/
SOAP_FMAC1
int
SOAP_FMAC2
soap_is_embedded(struct soap *soap, struct soap_plist *pp)
{ DBGLOG(TEST, SOAP_MESSAGE(fdebug,"\nis embedded? %d %d", (int)pp->mark1, (int)pp->mark2));
  if (!soap->enable_embedding && !soap->disable_href && !soap->is_in_header)
  { if (soap->counting)
      return pp->mark1 != 0;
    else
      return pp->mark2 != 0;
  }
  else if (soap->counting)
    return pp->mark1 == 1;
  else
    return pp->mark2 == 1;
}

/******************************************************************************/
SOAP_FMAC1
int
SOAP_FMAC2
soap_is_single(struct soap *soap, struct soap_plist *pp)
{ if (soap->disable_href || soap->is_in_header)
    return 1;
  else if (soap->counting)
    return pp->mark1 == 0;
  else
    return pp->mark2 == 0;
}

/******************************************************************************/
SOAP_FMAC1
int
SOAP_FMAC2
soap_is_multi(struct soap *soap, struct soap_plist *pp)
{ if (soap->counting)
    return pp->mark1 == 2;
  else
    return pp->mark2 == 2;
}

/******************************************************************************/
SOAP_FMAC1
void
SOAP_FMAC2
soap_set_embedded(struct soap *soap, struct soap_plist *pp)
{ if (soap->counting)
    pp->mark1 = 1;
  else
    pp->mark2 = 1;
}

/******************************************************************************/
SOAP_FMAC1
void
SOAP_FMAC2
soap_set_attached(struct soap *soap, struct soap_plist *pp, const char *id, const char *type, const char *options, size_t size)
{ if (soap->counting)
  { if (pp->mark1 != 3)
    { pp->mark1 = 3;
      soap->dime_count++;	/* one more attachment found */
      soap->dime_size += 12;	/* increase total size (DIME fields) */
      if (id)
        soap->dime_size += (strlen(id)+3)&-4;
      if (type)
        soap->dime_size += (strlen(type)+3)&-4;
      if (options)
        soap->dime_size += 4 + (((((unsigned char)options[2]<<8 | (unsigned char)options[3]))+3)&-4);
      soap->dime_size += (size+3)&-4;
    }
  }
  else if (pp->mark2 != 3)
    pp->mark2 = 3;
  soap->dime = 1;		/* mark to ensure DIME is used */
}

/******************************************************************************/
SOAP_FMAC1
void
SOAP_FMAC2
soap_init_iht(struct soap *soap)
{ int i;
  DBGLOG(TEST, SOAP_MESSAGE(fdebug, "\nInitializing ID hashtable"));
  for (i = 0; i < SOAP_IDHASH; i++)
    soap->iht[i] = NULL;
}

/******************************************************************************/
SOAP_FMAC1
void
SOAP_FMAC2
soap_free_iht(struct soap *soap)
{ int i;
  struct soap_ilist *ip, *next;
  for (i = 0; i < SOAP_IDHASH; i++)
  { for (ip = soap->iht[i]; ip; ip = next)
    { next = ip->next;
      free(ip);
    }
    soap->iht[i] = NULL;
  }
}

/******************************************************************************/
static struct soap_ilist*
lookup(struct soap *soap, const char *id)
{ struct soap_ilist *ip;
  for (ip = soap->iht[hash_id(id)]; ip; ip = ip->next)
    if (!strcmp(ip->id, id))
      return ip;
  return NULL;
}

/******************************************************************************/
static struct soap_ilist *
enter(struct soap *soap, const char *id)
{ int h;
  struct soap_ilist *ip;
  ip = (struct soap_ilist*)malloc(sizeof(struct soap_ilist)+strlen(id)-3);
  if (ip)
  { h = hash_id(id);
    strcpy(ip->id, id);
    ip->next = soap->iht[h];
    soap->iht[h] = ip;
    return ip;
  }
  return NULL;
}

/******************************************************************************/
SOAP_FMAC1
void*
SOAP_FMAC2
soap_malloc(struct soap *soap, size_t n)
{ char *p;
  if (!n)
    return NULL;
  if (!soap)
    return malloc(n);
  n += (-(int)n) & 7;
  if (!(p = (char*)malloc(n + 16)))
  { soap->error = SOAP_EOM;
    return NULL;
  }
  DBGLOG(TEST, SOAP_MESSAGE(fdebug,"\nMallocing %d bytes", n));
  /* keep chain of alloced cells for later destruction */
  soap->alloced = 1;
  *(void**)(p + n) = soap->alist;
  *(size_t*)(p + n + 8) = n;
  soap->alist = p + n;
  DBGLOG(TEST, SOAP_MESSAGE(fdebug," -- location=%p", p));
  return p;
}

/******************************************************************************/
SOAP_FMAC1
void
SOAP_FMAC2
soap_dealloc(struct soap *soap, void *p)
{ if (!soap)
    return;
  if (p)
  { char **q;
    struct soap_clist **cp;
    for (q = (char**)&soap->alist; *q; q = *(char***)q)
    { if (p == (void*)(*q - *(size_t*)(*q + 8)))
      { *q = **(char***)q;
	free(p);
        DBGLOG(TEST, SOAP_MESSAGE(fdebug,"\nFreed data %p", p));
        return;
      }
    }
    for (cp = &soap->clist; *cp; cp = &(*cp)->next)
    { if (p == (*cp)->ptr)
      { soap_delete(soap, p, (*cp)->type, (*cp)->size);
        DBGLOG(TEST, SOAP_MESSAGE(fdebug,"\nDeleted class instance %p", p));
        p = (void*)*cp;
        *cp = (*cp)->next;
        free(p);
        return;
      }
    }
    DBGLOG(TEST, SOAP_MESSAGE(fdebug,"\nCould not dealloc data %p: address not in list", p));
  }
  else
  { char *q;
    while (soap->alist)
    { q = (char*)soap->alist;
      soap->alist = *(void**)q;
      q -= *(size_t*)(q + 8);
      free(q);
      if (q == (char*)soap->fault)
        soap->fault = NULL;	/* this was deallocated */
      else if (q == (char*)soap->header)
        soap->header = NULL;	/* this was deallocated */
    }
    DBGLOG(TEST, SOAP_MESSAGE(fdebug,"\ndealloced all data ok"));
  }
}

/******************************************************************************/
SOAP_FMAC1
void
SOAP_FMAC2
soap_destroy(struct soap *soap)
{ struct soap_clist *cp;
  DBGLOG(TEST, SOAP_MESSAGE(fdebug,"\ndestroying class instances"));
  while (soap->clist)
  { cp = soap->clist;
    soap->clist = cp->next;
    if (cp->ptr)
    { DBGLOG(TEST, SOAP_MESSAGE(fdebug,"\ndeleting class instance %p", cp->ptr));
      soap_delete(soap, cp->ptr, cp->type, cp->size);
    }
    free(cp);
  }
  DBGLOG(TEST, SOAP_MESSAGE(fdebug,"\ndestruction ok"));
}

/******************************************************************************/
SOAP_FMAC1
void
SOAP_FMAC2
soap_unlink(struct soap *soap, void *p)
{ char **q;
  struct soap_clist **cp;
  if (!soap)
    return;
  for (q = (char**)&soap->alist; *q; q = *(char***)q)
  { if (p == (void*)(*q - *(size_t*)(*q + 8)))
    { *q = **(char***)q;
      DBGLOG(TEST, SOAP_MESSAGE(fdebug,"\nunlinked data %p", p));
      return;
    }
  }
  for (cp = &soap->clist; *cp; cp = &(*cp)->next)
  { if (p == (*cp)->ptr)
    { DBGLOG(TEST, SOAP_MESSAGE(fdebug,"\nunlinked class instance %p", p));
      p = (void*)*cp;
      *cp = (*cp)->next;
      free(p);
      return;
    }
  }
}

/******************************************************************************/
SOAP_FMAC1
int
SOAP_FMAC2
soap_lookup_type(struct soap *soap, const char *id)
{ struct soap_ilist *ip;
  if (*id)
  { ip = lookup(soap, id);
    if (ip)
    { DBGLOG(TEST, SOAP_MESSAGE(fdebug,"\nsoap_lookup_type = %d", ip->type));
      return ip->type;
    }
  }
  DBGLOG(TEST, SOAP_MESSAGE(fdebug,"\nINTERNAL ERROR: soap_lookup_type NOT FOUND!"));
  return 0;
}

/******************************************************************************/
SOAP_FMAC1
void*
SOAP_FMAC2
soap_id_lookup(struct soap *soap, const char *id, void **p, int t, size_t n, int k)
{ struct soap_ilist *ip;
  void *q;
  if (*id == '\0')
  { DBGLOG(TEST, SOAP_MESSAGE(fdebug,"INTERNAL ERROR: lookup with empty id"));
    return NULL;
  }
  soap->alloced = 0;
  if (!p)
    p = (void**)soap_malloc(soap, sizeof(void*));
  ip = lookup(soap, id); /* lookup pointer to hash table entry for string id */
  if (!ip)
  { ip = enter(soap, id); /* new hash table entry for string id */
    DBGLOG(TEST, SOAP_MESSAGE(fdebug,"Forwarding first href=%s (%d bytes)", id, n));
    ip->type = t;
    ip->size = n; 
    ip->link = p;
    ip->copy = NULL;
    ip->ptr = NULL;
    ip->level = k;
    *p = NULL;
  }
  else if (!soap->blist && ip->ptr)
  { DBGLOG(TEST, SOAP_MESSAGE(fdebug,"Resolved %s (%d bytes)", id, n));
    if (ip->type != t)
    { soap->error = SOAP_HREF;
      return NULL;
    }
    while (ip->level < k)
    { q = soap_malloc(soap, sizeof(void*));  
      *p = q;
      p = (void**)q;
      k--;
      DBGLOG(TEST, SOAP_MESSAGE(fdebug,"\ndescending one level..."));
    }
    *p = ip->ptr;
  }
  else if (ip->level > k)
  { DBGLOG(TEST, SOAP_MESSAGE(fdebug,"\nResolving level %d pointers to %s", ip->level, id));
    while (ip->level > k)
    { void *t, **r = &ip->link;
      q = ip->link;
      while (q)
      { *r = soap_malloc(soap, sizeof(void*));
	t = *(void**)q;
	*(void**)q = *r;
	r = *(void***)q;
        q = t;
      }
      *r = NULL;
      ip->size = n; 
      ip->copy = NULL;
      ip->level = ip->level-1;
      DBGLOG(TEST, SOAP_MESSAGE(fdebug,"\ndescending one level..."));
    }
    q = ip->link;
    ip->link = p;
    *p = q;
  }
  else
  { DBGLOG(TEST, SOAP_MESSAGE(fdebug, "\nForwarded %s (%d bytes)", id, n));
    while (ip->level < k)
    { q = soap_malloc(soap, sizeof(void*));  
      *p = q;
      p = (void**)q;
      k--;
      DBGLOG(TEST, SOAP_MESSAGE(fdebug, "\ndescending one level..."));
    }
    q = ip->link;
    ip->link = p;
    *p = q;
  }
  return p;
}

/******************************************************************************/
SOAP_FMAC1
void*
SOAP_FMAC2
soap_id_forward(struct soap *soap, const char *href, void *p, int t, size_t n)
{ struct soap_ilist *ip;
  if (!*href)
    return p;
  ip = lookup(soap, soap->href); /* lookup pointer to hash table entry for string id */
  if (!ip)
  { if (n >= sizeof(void*))
    { ip = enter(soap, href); /* new hash table entry for string id */
      ip->type = t;
      ip->size = n;
      ip->link = NULL;
      ip->copy = p;
      *(void**)p = NULL;
      ip->ptr = NULL;
      ip->level = 0;
      DBGLOG(TEST, SOAP_MESSAGE(fdebug, "\nforwarding first copying address %p for type %d href=%s", p, t, href));
      return p;
    }
    soap->error = SOAP_HREF;
    return NULL;
  }
  else if (ip->ptr)
  { if (ip->size == n)
      memcpy(p, ip->ptr, n);
    else
    { DBGLOG(TEST, SOAP_MESSAGE(fdebug, "\nReference to object of different size (ref = %d, object = %d)", n, ip->size));
      soap->error = SOAP_HREF;
      return NULL;
    }
  }
  else if (n >= sizeof(void*))
  { DBGLOG(TEST, SOAP_MESSAGE(fdebug, "\nforwarding copying address %p for type %d href=%s", p, t, href));
    *(void**)p = ip->copy; /* alignment problem? */
    ip->copy = p;
    return p;
  }
  else
  { DBGLOG(TEST, SOAP_MESSAGE(fdebug, "\nforwarding problem: copying location %p too small for href=%s", p, href));
    soap->error = SOAP_HREF; /* href to object too small to hold pointer */
    return NULL;
  }
  return ip->ptr;
}

/******************************************************************************/
SOAP_FMAC1
void*
SOAP_FMAC2
soap_id_enter(struct soap *soap, const char *id, void *p, int t, size_t n, int k)
{ struct soap_ilist *ip;
  DBGLOG(TEST,SOAP_MESSAGE(fdebug,"\nsoap_id_enter(%s, %p, %d, %d)", id, p, (int)n, k));
  soap->alloced = 0;
  if (*id == '\0')
  { if (!p)
      return soap_malloc(soap, n);
    else
      return p;
  }
  ip = lookup(soap, id); /* lookup pointer to hash table entry for string id */
  DBGLOG(TEST,SOAP_MESSAGE(fdebug, "\nlookup"));
  if (!ip)
  { ip = enter(soap, id); /* new hash table entry for string id */
    DBGLOG(TEST,SOAP_MESSAGE(fdebug, "\nenter"));
    ip->type = t;
    ip->size = n;
    ip->link = NULL;
    ip->copy = NULL;
    if (!p)
      p = soap_malloc(soap, n);
    ip->ptr = p;
    ip->level = k;
  }
  else if (ip->ptr) /* storage address was forwarded */
  { DBGLOG(TEST, SOAP_MESSAGE(fdebug, "\nMultiply defined id='%s'", id));
    if (p)
    { soap->error = SOAP_MULTI_ID;
      return NULL;
    }
  }
  else /* if (e->size == n) --- since we deal with pointers, e->size is 4 */
  { if (!p)
      p = soap_malloc(soap, n);
    ip->ptr = p;
    if (!soap->blist)
      soap_resolve_ptr(ip);
  }
  return ip->ptr;
}

/******************************************************************************/
SOAP_FMAC1
int
SOAP_FMAC2
soap_end_send(struct soap *soap)
{ DBGLOG(TEST,SOAP_MESSAGE(fdebug, "\nEnd send"));
  if (soap->buffering) /* need to flush the remaining data in buffer */
  { if (soap_flush(soap))
      return soap->error;
    if (soap->chunked)
      if (soap->fsend(soap, "0\r\n\r\n", 7))
        return SOAP_EOF;
  }
  soap->bufidx = 0;
  soap->buffering = 0;
  DBGLOG(TEST,SOAP_MESSAGE(fdebug, "\nEnd of send message ok"));
  return SOAP_OK;
}

/******************************************************************************/
SOAP_FMAC1
int
SOAP_FMAC2
soap_end_recv(struct soap *soap)
{ int i, flag = 0;
  struct soap_ilist *ip;
  for (i = 0; i < SOAP_IDHASH; i++)
    for (ip = soap->iht[i]; ip; ip = ip->next)
    { if (ip->ptr)
        soap_resolve_ptr(ip);
      else if (*ip->id == '#')
        flag = 1;
    }
  for (i = 0; i < SOAP_IDHASH; i++)
    for (ip = soap->iht[i]; ip; ip = ip->next)
      if (ip->ptr)
        soap_resolve_copy(soap, ip);
  DBGLOG(TEST,SOAP_MESSAGE(fdebug, "\nEnd of receive message ok"));
  if (flag)
    return soap->error = SOAP_MISSING_ID;
  return SOAP_OK;
}

/******************************************************************************/
SOAP_FMAC1
void*
SOAP_FMAC2
soap_class_id_enter(struct soap *soap, const char *id, void *p, int t, const char *type, const char *arrayType)
{ struct soap_ilist *ip;
  DBGLOG(TEST,SOAP_MESSAGE(fdebug, "\nclass id enter name = %d and type = %s", t, type?type:""));	
  soap->alloced = 0;
  if (*id == '\0')
  { if (!p)
      return soap_instantiate(soap, t, type, arrayType);
    else
      return p;
  }
  ip = lookup(soap, id); /* lookup pointer to hash table entry for string id */
  DBGLOG(TEST,SOAP_MESSAGE(fdebug,"\nlookup"));
  if (!ip)
  { ip = enter(soap, id); /* new hash table entry for string id */
    DBGLOG(TEST,SOAP_MESSAGE(fdebug,"\nenter"));
    /* e->size = n; */ /* size of "type" */
    ip->link = NULL;
    ip->copy = NULL;
    if (!p)
      p = soap_instantiate(soap, t, type, arrayType);
    ip->ptr = p;
    ip->level = 0;
  }
  else if (ip->ptr) /* storage address was forwarded */
  { DBGLOG(TEST, SOAP_MESSAGE(fdebug,"\nMultiply defined id='%s'", id));
    if (p)
    { soap->error = SOAP_MULTI_ID;
      return NULL;
    }
  }
  else /* if (e->size <= size of "type") */
  { if (!p)
      p = soap_instantiate(soap, t, type, arrayType);
    /* e->size = size of "type" */
    ip->ptr = p;
    ip->level = 0;
    if (!soap->blist)
      soap_resolve_ptr(ip);
  }
  return ip->ptr;
}

/******************************************************************************/
SOAP_FMAC1
void
SOAP_FMAC2
soap_free(struct soap *soap)
{ struct soap_nlist *np;
  struct soap_blist *bp;
  struct Namespace *ns;
  char *p;
  DBGLOG(TEST, SOAP_MESSAGE(fdebug,"\nFree namespace stack"));
  while (soap->nlist)
  { np = soap->nlist->next;
    free(soap->nlist->id);
    free(soap->nlist);
    soap->nlist = np;
  }
  DBGLOG(TEST, SOAP_MESSAGE(fdebug,"\nFree any remaining temp blocks"));
  while (soap->blist)
  { bp = soap->blist->next;
    while (soap->blist->ptr)
    { p = *((char**)soap->blist->ptr);
      free(soap->blist->ptr);
      soap->blist->ptr = p;
    }
    free(soap->blist);
    soap->blist = bp;
  }
  DBGLOG(TEST, SOAP_MESSAGE(fdebug,"\nFree pointer hashtable"));
  soap_free_pht(soap);
  DBGLOG(TEST, SOAP_MESSAGE(fdebug,"\nFree ID hashtable"));
  soap_free_iht(soap);
  if (soap->action)
  { free(soap->action);
    soap->action = NULL;
  }
  ns = soap->namespaces;
  if (ns)
    for (; ns->id; ns++)
    { if (ns->out)
      { free(ns->out);
        ns->out = NULL;
      }
    }
}

/******************************************************************************/
static void
soap_init_logs(struct soap *soap)
{ int i;
  for (i = 0; i < SOAP_MAXLOGS; i++)
  { soap->logfile[i] = NULL;
    soap->fdebug[i] = NULL;
  }
#ifdef SOAP_DEBUG  
  soap_set_recv_logfile(soap, "RECV.log");
  soap_set_sent_logfile(soap, "SENT.log");
  soap_set_test_logfile(soap, "TEST.log");
#endif
}

/******************************************************************************/
SOAP_FMAC1
void
SOAP_FMAC2
soap_open_logfile(struct soap *soap, int i)
{ if (soap->logfile[i])
    soap->fdebug[i] = fopen(soap->logfile[i], "a");
}

/******************************************************************************/
static void
soap_close_logfile(struct soap *soap, int i)
{ if (soap->fdebug[i])
  { fclose(soap->fdebug[i]);
    soap->fdebug[i] = NULL;
  }
}

/******************************************************************************/
SOAP_FMAC1
void
SOAP_FMAC2
soap_close_logfiles(struct soap *soap)
{ int i;
  for (i = 0; i < SOAP_MAXLOGS; i++)
    soap_close_logfile(soap, i);
}

/******************************************************************************/
static void
soap_set_logfile(struct soap *soap, int i, const char *logfile)
{ char *s = NULL;
  if (logfile)
  { s = (char*)malloc(strlen(logfile)+1);
    if (s)
      strcpy(s, logfile);
  }
  if (soap->logfile[i])
    free((void*)soap->logfile[i]);
  soap->logfile[i] = s;
  soap_close_logfile(soap, i);
}

/******************************************************************************/
SOAP_FMAC1
void
SOAP_FMAC2
soap_set_recv_logfile(struct soap *soap, const char *logfile)
{ soap_set_logfile(soap, SOAP_INDEX_RECV, logfile);
}

/******************************************************************************/
SOAP_FMAC1
void
SOAP_FMAC2
soap_set_sent_logfile(struct soap *soap, const char *logfile)
{ soap_set_logfile(soap, SOAP_INDEX_SENT, logfile);
}

/******************************************************************************/
SOAP_FMAC1
void
SOAP_FMAC2
soap_set_test_logfile(struct soap *soap, const char *logfile)
{ soap_set_logfile(soap, SOAP_INDEX_TEST, logfile);
}

/******************************************************************************/
SOAP_FMAC1
void
SOAP_FMAC2
soap_init(struct soap *soap)
{ soap->user = NULL;
  soap->fpost = http_post;
  soap->fresponse = http_response;
  soap->fparse = http_parse;
  soap->fopen = tcp_connect;
  soap->fclose = tcp_disconnect;
  soap->fsend = fsend;
  soap->frecv = frecv;
  soap->fignore = fignore;
  soap->float_format = "%.9G";		/* .9G preserves single FP precision */
  soap->double_format = "%.18G";	/* .18G preserves double FP precision */
  soap->dime_id_format = "cid:id%d";	/* default DIME id format */
  soap->http_version = "1.0";
  soap->encodingStyle = "http://schemas.xmlsoap.org/soap/encoding/";
  soap->defaultNamespace = NULL;
  soap->actor = NULL;
  soap->keep_alive = 0;
  soap->chunked_transfer = 0;
  soap->dime_transfer = 0;
  soap->disable_href = 0;
  soap->enable_embedding = 0;
  soap->enable_null = 0;
  soap->enable_utf_string = 0;
  soap->disable_request_count = 0;
  soap->disable_response_count = 0;
  soap->recv_timeout = 0;
  soap->send_timeout = 0;
  soap->connect_timeout = 0;
  soap->accept_timeout = 0;
#ifndef WITH_NONAMESPACES
  soap->namespaces = namespaces;
#else
  soap->namespaces = NULL;
#endif
  soap->nlist = NULL;
  soap->blist = NULL;
  soap->clist = NULL;
  soap->alist = NULL;
  soap->header = NULL;
  soap->fault = NULL;
  soap->master = -1;
  soap->socket = -1;
#ifndef UNDER_CE
  soap->recvfd = 0;
  soap->sendfd = 1;
#else
  soap->recvfd = stdin;
  soap->sendfd = stdout;
#endif 
  *soap->host = '\0';
  soap->port = 0;
  *soap->endpoint = '\0';
  *soap->path = '\0';
  soap->action = NULL;
  soap->proxy_host = NULL;
  soap->proxy_port = 8080;
#ifdef WITH_COOKIES
  soap->cookies = NULL;
  soap->cookie_domain = NULL;
  soap->cookie_path = NULL;
  soap->cookie_max = 32;
#endif
#ifdef WITH_OPENSSL
  soap->bio = NULL;
  soap->ssl = NULL;
  soap->require_server_auth = 0;
  soap->keyfile = NULL;
  soap->cafile = NULL;
  soap->dhfile = NULL;
  soap->password = NULL;
#endif
  soap_init_logs(soap);
  soap_init_iht(soap);
  soap_init_pht(soap);
  soap->dot_net_bug = 0;
}

/******************************************************************************/
SOAP_FMAC1
void
SOAP_FMAC2
soap_begin(struct soap *soap)
{ DBGLOG(TEST, SOAP_MESSAGE(fdebug,"\nInitializing"));
  soap->buflen = 0;
  soap->bufidx = 0;
  soap->null = 0;
  soap->position = 0;
  soap->mustUnderstand = 0;
  soap->buffering = 0;
  soap->ns = 0;
  soap->is_in_header = 0;
  soap->alloced = 0;
  soap->counting = 0;
  soap->ip = 0;
  soap->cdata = 0;
  soap->error = SOAP_OK;
  soap->peeked = 0;
  soap->ahead1 = 0;
  soap->ahead2 = 0;
  soap->idnum = 0;
  soap->level=0;
  soap_free(soap);
  soap->nlist = NULL;
  soap->blist = NULL;
  *soap->endpoint = '\0';
  *soap->path = '\0';
  if (soap->namespaces)
  { soap->namespaces[0].id = "SOAP-ENV";	/* override to make sure */
    soap->namespaces[1].id = "SOAP-ENC";	/* override to make sure */
    soap->namespaces[2].id = "xsi";	/* override to make sure */
  }
  soap->dime = soap->dime_transfer;
  soap->dime_chunksize = 0;
  soap->dime_buflen = 0;
  soap->dot_net_bug = 0;
}

/******************************************************************************/
SOAP_FMAC1
void
SOAP_FMAC2
soap_end(struct soap *soap)
{ struct soap_clist *cp;
  soap_free(soap);
  soap_dealloc(soap, NULL);
  while (soap->clist)
  { cp = soap->clist->next;
    free(soap->clist);
    soap->clist = cp;
  }
  soap_closesock(soap);
  soap_close_logfiles(soap);
}

/******************************************************************************/
static int
soap_position(struct soap *soap)
{ if (soap_send(soap, " SOAP-ENC:position=\"") || soap_send(soap, soap_putposition(soap)) || soap_send(soap, "\""))
    return soap->error;
  return SOAP_OK;
}

/******************************************************************************/
static int
soap_mustUnderstand(struct soap *soap)
{ if (soap->actor)
  { if (soap_send(soap, " SOAP-ENV:actor=\"") || soap_send(soap, soap->actor) || soap_send(soap, "\""))
      return soap->error;
  }
  if (soap_send(soap, " SOAP-ENV:mustUnderstand=\"1\""))
    return soap->error;
  soap->mustUnderstand = 0;
  return SOAP_OK;
}

/******************************************************************************/
SOAP_FMAC1
int
SOAP_FMAC2
soap_element_begin_out(struct soap *soap, const char *tag, int id, const char *type)
{ DBGLOG(TEST, SOAP_MESSAGE(fdebug,"\nsoap_element_begin_out(%s, %d, %s)", tag, id, type?type:""));
  if (soap_send(soap, "<") || soap_send(soap, tag))
    return soap->error;
  if (id > 0)
  { sprintf(soap->tagbuf, " id=\"_%d\"", id);
    if (soap_send(soap, soap->tagbuf))
      return soap->error;
  }
  if (soap->null && soap->position > 0)
    if (soap_position(soap))
      return soap->error;
  if (type && *type)
  { if (soap_send(soap, " xsi:type=\"") || soap_send(soap, type) || soap_send(soap, "\""))
      return soap->error;
  }
  if (soap->mustUnderstand)
    if (soap_mustUnderstand(soap))
      return soap->error;
  soap->null = 0;
  return soap_send_namespaces(soap);
}

/******************************************************************************/
SOAP_FMAC1
int
SOAP_FMAC2
soap_array_begin_out(struct soap *soap, const char *tag, int id, const char *type, const char *offset)
{ DBGLOG(TEST, SOAP_MESSAGE(fdebug,"\nsoap_array_begin_out(%s, %d, %s, %s)", tag, id, type?type:"", offset?offset:""));
  if (soap_send(soap, "<") || soap_send(soap, tag) || soap_send(soap, " xsi:type=\"SOAP-ENC:Array\""))
    return soap->error;
  if (id > 0)
  { sprintf(soap->tagbuf, " id=\"_%d\"", id);
    if (soap_send(soap, soap->tagbuf))
      return soap->error;
  }
  if (soap->null && soap->position > 0)
    if (soap_position(soap))
      return soap->error;
  if (offset)
  { if (soap_send(soap, " SOAP-ENC:offset=\"") || soap_send(soap, offset) || soap_send(soap, "\""))
      return soap->error;
  }
  if (type && *type)
  { if (soap_send(soap, " SOAP-ENC:arrayType=\"") || soap_send(soap, type) || soap_send(soap, "\""))
      return soap->error;
  }
  if (soap->mustUnderstand)
    if (soap_mustUnderstand(soap))
      return soap->error;
  soap->null = 0;
  return soap_send_namespaces(soap);
}

/******************************************************************************/
SOAP_FMAC1
int
SOAP_FMAC2
soap_element_end_out(struct soap *soap, const char *tag)
{ DBGLOG(TEST, SOAP_MESSAGE(fdebug,"\nsoap_element_end_out(%s)", tag));
  if (soap_send(soap, "</") || soap_send(soap, tag) || soap_send(soap, ">"))
    return soap->error;
  return SOAP_OK;
}

/******************************************************************************/
SOAP_FMAC1
int
SOAP_FMAC2
soap_element_ref(struct soap *soap, const char *tag, int id, int href)
{ DBGLOG(TEST, SOAP_MESSAGE(fdebug,"\nsoap_element_ref(%s, %d, %d)", tag, id, href));
  if (soap_send(soap, "<") || soap_send(soap, tag))
    return soap->error;
  if (id > 0)
  { sprintf(soap->tagbuf, " id=\"_%d\"", id);
    if (soap_send(soap, soap->tagbuf))
      return soap->error;
  }
  if (soap->null && soap->position > 0)
    if (soap_position(soap))
      return soap->error;
  if (soap->mustUnderstand)
    if (soap_mustUnderstand(soap))
      return soap->error;
  sprintf(soap->tagbuf, " href=\"#_%d\"/>", href);
  if (soap_send(soap, soap->tagbuf))
    return soap->error;
  soap->null = 0;
  return SOAP_OK;
}

/******************************************************************************/
SOAP_FMAC1
int
SOAP_FMAC2
soap_element_href(struct soap *soap, const char *tag, int id, const char *href)
{ if (soap_send(soap, "<") || soap_send(soap, tag))
    return soap->error;
  if (id > 0)
  { sprintf(soap->tagbuf, " id=\"_%d\"", id);
    if (soap_send(soap, soap->tagbuf))
      return soap->error;
  }
  if (soap->null && soap->position > 0)
    if (soap_position(soap))
      return soap->error;
  if (soap->mustUnderstand)
    if (soap_mustUnderstand(soap))
      return soap->error;
  if (soap_send(soap, " href=\"") || soap_send(soap, href) || soap_send(soap, "\"/>"))
    return soap->error;
  soap->null = 0;
  return SOAP_OK;
}

/******************************************************************************/
SOAP_FMAC1
int
SOAP_FMAC2
soap_element_null(struct soap *soap, const char *tag, int id, const char *type)
{ DBGLOG(TEST, SOAP_MESSAGE(fdebug,"\nsoap_element_null(%s, %d, %s)", tag, id, type?type:""));
  if (id > 0 || soap->enable_null)
  { if (soap_send(soap, "<") || soap_send(soap, tag) || soap_send(soap, " xsi:nil=\"true\""))
      return soap->error;
    if (id > 0)
    { sprintf(soap->tagbuf, " id=\"_%d\"", id);
      if (soap_send(soap, soap->tagbuf))
        return soap->error;
    }
    if (type && *type)
    { if (soap_send(soap, " xsi:type=\"") || soap_send(soap, type) || soap_send(soap, "\"/>"))
        return soap->error;
    }
    else if (soap_send(soap, "/>"))
      return soap->error;
  }
  else
    soap->null = 1;
  soap->mustUnderstand = 0;
  return SOAP_OK;
}

/******************************************************************************/
SOAP_FMAC1
int
SOAP_FMAC2
soap_element_begin_in(struct soap *soap, const char *tag)
{ if (!(soap->error = soap_peek_element(soap)))
  { if (soap->other)
      return soap->error = SOAP_TAG_MISMATCH;
    if (!(soap->error = soap_match_tag(soap, soap->tag, tag)))
    { soap->peeked = 0;
      DBGLOG(TEST, SOAP_MESSAGE(fdebug,"\nBegin element (%d) %s=%s", soap->level, soap->tag, tag?tag:"" ));
      if (soap->body)
        soap->level++;
    }
  }
  return soap->error;
}

/******************************************************************************/
SOAP_FMAC1
int
SOAP_FMAC2
soap_element_end_in(struct soap *soap, const char *tag)  
{ int c;
  char *s;
  const char *t;
  soap->error = SOAP_OK;
  soap->level--;
  soap_pop_namespace(soap);
  if (soap->peeked)
  { if (*soap->tag == '\0')
      soap->peeked = 0;
    else
      return soap->error = SOAP_SYNTAX_ERROR;
  }
  else
  { c = soap_advance(soap);
    if (c == EOF)
      return soap->error = SOAP_EOF;
    if (c != TT)
      return soap->error = SOAP_SYNTAX_ERROR;
  }
  s = soap->tag;
  c = soap_skip(soap);
  do
  { *s++ = c;
    c = soap_get(soap);
  } while (notblank(c));
  if (c == EOF)
    return soap->error = SOAP_EOF;
  *s = '\0';
  if ((s = strchr(soap->tag, ':')))
    s++;
  else
    s = soap->tag;
  t = tag;
  if (t && (t = strchr(t, ':')))
    t++;
  else if (t && (t = strchr(t, '_')) && t[1] == '_')
    t += 2;
  if (blank(c))
    c = soap_skip(soap);
  if (c != GT)
    return soap->error = SOAP_SYNTAX_ERROR;
  if (!t || !soap_tag_cmp(s, t))
    return SOAP_OK;
  return soap->error = SOAP_SYNTAX_ERROR;
}

/******************************************************************************/
SOAP_FMAC1
int
SOAP_FMAC2
soap_peek_element(struct soap *soap)
{ char attribute_name[SOAP_TAGLEN], attribute_value[SOAP_TAGLEN];
  char *s;
  int i;
  wchar c, d;
  if (soap->peeked)
  { if (*soap->tag == '\0')
      return SOAP_NO_TAG;
    return SOAP_OK;
  }
  soap->peeked = 1;
  if ((c = soap_advance(soap)) == EOF)
    return SOAP_EOF;
  if (c == TT)
  { *soap->tag = '\0';
    return SOAP_NO_TAG;     /* ending tag found */
  }
  s = soap->tag;
  c = soap_skip(soap);
  i = SOAP_TAGLEN;
  while (c != '/' && notblank(c))
  { if (--i > 0)
      *s++ = (char)c;
    c = soap_get(soap);
  }
  *s = '\0';
  *soap->id = '\0';
  *soap->href = '\0';
  *soap->type = '\0';
  *soap->arrayType = '\0';
  *soap->arraySize = '\0';
  soap->other = 0;
  soap->root = -1;
  *soap->offset = '\0';
  soap->position = 0;
  soap->null = 0;
  soap->mustUnderstand = 0;
  while (blank(c))
    c = soap_get(soap);
  do
  { s = attribute_name;
    *s = '\0';
    if (c == EOF || c == GT || c == '/')
      break;
    i = SOAP_TAGLEN;
    while (c != '=' && c != '/' && notblank(c))
    { if (--i > 0)
        *s++ = (char)c;
      c = soap_get(soap);
    }
    *s = '\0';
    s = attribute_value;
    while (blank(c))
      c = soap_get(soap);
    if (c == '=')
    { c = soap_skip(soap);
      if (c == QT || c == AP)
      { d = c;
        i = SOAP_TAGLEN;
        while ((c = soap_get(soap)) != EOF && c != d)
          if (--i > 0)
	    *s++ = (char)c;
        if (c == d)
          c = soap_get(soap);
      }
    }
    *s = '\0';
    while (blank(c))   
    c = soap_get(soap);
    if (soap_match_tag(soap, attribute_name, "id") == 0)
    { if (!soap->is_in_header || !soap->dot_net_bug)
      { *soap->id = '#';
        strncpy(soap->id+1, attribute_value, SOAP_TAGLEN-1);
      }
    }
    else if (soap_match_tag(soap, attribute_name, "href") == 0 || soap_match_tag(soap, attribute_name, "ref") == 0)
      strcpy(soap->href, attribute_value);
    else if (soap_match_tag(soap, attribute_name, "type") == 0)
      strcpy(soap->type, attribute_value);
    else if (soap_match_tag(soap, attribute_name, "arrayType") == 0)
    { s = strrchr(attribute_value, '[');
      if (s)
      { strncpy(soap->arrayType, attribute_value, s-attribute_value);
        soap->arrayType[s-attribute_value] = '\0';
        strcpy(soap->arraySize, s);
      }
      else
        strcpy(soap->arrayType, attribute_value);
    }
    else if (soap_match_tag(soap, attribute_name, "itemType") == 0)
      strcpy(soap->arrayType, attribute_value);
    else if (soap_match_tag(soap, attribute_name, "arraySize") == 0)
      strcpy(soap->arraySize, attribute_value);
    else if (soap_match_tag(soap, attribute_name, "offset") == 0)
      strcpy(soap->offset, attribute_value);
    else if (soap_match_tag(soap, attribute_name, "position") == 0)
      soap->position = soap_getposition(attribute_value, soap->positions);
    else if (soap_match_tag(soap, attribute_name, "root") == 0)
    { if (strcmp(attribute_value, "1") ==0 || strcmp(attribute_value, "true") == 0)
        soap->root = 1;
      else
        soap->root = 0;
    }
    else if (soap_match_tag(soap, attribute_name, "actor") == 0)
    { if ((soap->actor && strcmp(soap->actor, attribute_value))
       || strcmp(attribute_value, "http://schemas.xmlsoap.org/soap/actor/next"))
        soap->other = 1;
    }
    else if (soap_match_tag(soap, attribute_name, "mustUnderstand") == 0 && (strcmp(attribute_value, "1") == 0 || strcmp(attribute_value, "true") == 0 ))
      soap->mustUnderstand = 1;
    else if ((soap_match_tag(soap, attribute_name, "null") == 0 || soap_match_tag(soap, attribute_name, "nil") == 0) && (strcmp(attribute_value, "1") == 0 || strcmp(attribute_value, "true") == 0))
      soap->null = 1;
    else if (strncmp(attribute_name, "xmlns:", 6) == 0)
    { if (soap_push_namespace(soap, attribute_name+6, attribute_value))
        return SOAP_EOM;
    }
  } while (c != EOF && c != GT && c != '/' && notblank(c));
  if (c == EOF)
    return SOAP_EOF;
  if (!(soap->body = (c != '/')))
    c = soap_skip(soap);
  return SOAP_OK;
}

/******************************************************************************/
SOAP_FMAC1
void
SOAP_FMAC2
soap_revert(struct soap *soap)
{ soap->peeked = 1;
  if (soap->body)
    soap->level--;
}

/******************************************************************************/
SOAP_FMAC1
int
SOAP_FMAC2
soap_ignore_element(struct soap *soap)
{ if (!(soap->error = soap_peek_element(soap)) || soap->error == SOAP_TAG_MISMATCH)
  { soap->peeked = 0;
    DBGLOG(TEST, SOAP_MESSAGE(fdebug, "\nIGNORING element '%s' (level=%d, %d)\n", soap->tag, soap->level, soap->body));
    if (soap->mustUnderstand && !soap->other)
      return soap->error = SOAP_MUSTUNDERSTAND;
    soap->error = soap->fignore(soap, soap->tag);
    if (!soap->error && soap->body)
    { soap->level++;
      while (!soap_ignore_element(soap))
        ;
      if (soap->error == SOAP_NO_TAG)
        soap_element_end_in(soap, NULL);
    }
  }
  return soap->error;
}

/******************************************************************************/
static int
fignore(struct soap *soap, const char *tag)
{ return SOAP_OK;
}

/******************************************************************************/
SOAP_FMAC1
int
SOAP_FMAC2
soap_convert_string_out(struct soap *soap, const char *s)
{ const char *t;
  char tmp[16];
  int c;
  while ((c = *s++))
  { switch (c)
    { 
    case '&':
      t = "&amp;";
      break;
    case '<':
      t = "&lt;";
      break;
    case '>':
      t = "&gt;";
      break;
    case '"':
      t = "&quot;";
      break;
    case '\'':
      t = "&apos;";
      break;
    default:
      if (c >= 32 || (c < 0 && soap->enable_utf_string))
      { tmp[0] = c;
        tmp[1] = '\0';
      }	
      else
        sprintf(tmp, "&#%d;", (unsigned char)c);
      t = tmp;
    }
    if (soap_send(soap, t))
      return soap->error;
  }
  return SOAP_OK;
}

/******************************************************************************/
SOAP_FMAC1
int
SOAP_FMAC2
soap_convert_wstring_out(struct soap *soap, const wchar_t *s)
{ const char *t;
  char tmp[16];
  wchar c;
  while ((c = *s++))
  { switch (c)
    { 
    case '&':
      t = "&amp;";
      break;
    case '<':
      t = "&lt;";
      break;
    case '>':
      t = "&gt;";
      break;
    case '"':
      t = "&quot;";
      break;
    case '\'':
      t = "&apos;";
      break;
    default:
      if (c < 32)
      { sprintf(tmp, "&#%ld;", c);
        t = tmp;
      }
      else if (soap_pututf8(soap, c))
        return soap->error;
      else
        continue;
    }
    if (soap_send(soap, t))
      return soap->error;
  }
  return SOAP_OK;
}

/******************************************************************************/
SOAP_FMAC1
int
SOAP_FMAC2
soap_outint(struct soap *soap, const char *tag, int id, const int *p, const char *type, int n)
{ if (soap_element_begin_out(soap, tag, soap_embedded_id(soap, id, p, n), type))
    return soap->error;
  sprintf(soap->tagbuf, "%d", *p);
  if (soap_send(soap, soap->tagbuf))
    return soap->error;
  return soap_element_end_out(soap, tag);
}

/******************************************************************************/
SOAP_FMAC1
int *
SOAP_FMAC2
soap_inint(struct soap *soap, const char *tag, int *p, const char *type, int t)
{ char *s;
  if (soap_element_begin_in(soap, tag))
    return NULL;
  if (soap->null)
  { if (soap->enable_null)
    { soap->error = SOAP_NULL;
      return NULL;
    }
    else
      return p;
  }
  if (*soap->type != '\0'
      && soap_match_tag(soap, soap->type, type)
      && soap_match_tag(soap, soap->type, "int")
      && soap_match_tag(soap, soap->type, "short")
      && soap_match_tag(soap, soap->type, "byte")
      && soap_match_tag(soap, soap->type, "unsignedInt")
      && soap_match_tag(soap, soap->type, "unsignedShort")
      && soap_match_tag(soap, soap->type, "unsignedByte"))
  { soap->error = SOAP_TYPE_MISMATCH;
    soap_revert(soap);
    return NULL;
  }
  if (soap->body && !*soap->href)
  { p = (int*)soap_id_enter(soap, soap->id, p, t, sizeof(int), 0);
    if (!p)
      return NULL;
    s = soap_value(soap);
    *p = strtol(s, &s, 10);
    if (*s)
    { soap->error = SOAP_TYPE_MISMATCH;
      return NULL;;
    }
  }
  else
    p = (int*)soap_id_forward(soap, soap->href, p, t, sizeof(int));
  if (soap->body && soap_element_end_in(soap, tag))
    return NULL;
  return p;
}

/******************************************************************************/
SOAP_FMAC1
int
SOAP_FMAC2
soap_outlong(struct soap *soap, const char *tag, int id, const long *p, const char *type, int n)
{ if (soap_element_begin_out(soap, tag, soap_embedded_id(soap, id, p, n), type))
    return soap->error;
  sprintf(soap->tagbuf, "%ld", *p);
  if (soap_send(soap, soap->tagbuf))
    return soap->error;
  return soap_element_end_out(soap, tag);
}

/******************************************************************************/
SOAP_FMAC1
long *
SOAP_FMAC2
soap_inlong(struct soap *soap, const char *tag, long *p, const char *type, int t)
{ char *s;
  if (soap_element_begin_in(soap, tag))
    return NULL;
  if (soap->null)
  { if (soap->enable_null)
    { soap->error = SOAP_NULL;
      return NULL;
    }
    else
      return p;
  }
  if (*soap->type != '\0'
      && soap_match_tag(soap, soap->type, type)
      && soap_match_tag(soap, soap->type, "long")
      && soap_match_tag(soap, soap->type, "int")
      && soap_match_tag(soap, soap->type, "short")
      && soap_match_tag(soap, soap->type, "byte")
      && soap_match_tag(soap, soap->type, "unsignedLong")
      && soap_match_tag(soap, soap->type, "unsignedInt")
      && soap_match_tag(soap, soap->type, "unsignedShort")
      && soap_match_tag(soap, soap->type, "unsignedByte"))
  { soap->error = SOAP_TYPE_MISMATCH;
    soap_revert(soap);
    return NULL;
  }
  if (soap->body && !*soap->href)
  { p = (long*)soap_id_enter(soap, soap->id, p, t, sizeof(long), 0);
    if (!p)
      return NULL;
    s = soap_value(soap);
    *p = strtol(s, &s, 10);
    if (*s)
    { soap->error = SOAP_TYPE_MISMATCH;
      return NULL;;
    }
  }
  else
    p = (long*)soap_id_forward(soap, soap->href, p, t, sizeof(long));
  if (soap->body && soap_element_end_in(soap, tag))
    return NULL;
  return p;
}

/******************************************************************************/
SOAP_FMAC1
int
SOAP_FMAC2
soap_outLONG64(struct soap *soap, const char *tag, int id, const LONG64 *p, const char *type, int n)
{ if (soap_element_begin_out(soap, tag, soap_embedded_id(soap, id, p, n), type))
    return soap->error;
#ifdef win32
  sprintf(soap->tagbuf, "%I64d", *p);
#else
  sprintf(soap->tagbuf, "%lld", *p);
#endif
  if (soap_send(soap, soap->tagbuf))
    return soap->error;
  return soap_element_end_out(soap, tag);
}

/******************************************************************************/
SOAP_FMAC1
LONG64 *
SOAP_FMAC2
soap_inLONG64(struct soap *soap, const char *tag, LONG64 *p, const char *type, int t)
{ char *s;
  if (soap_element_begin_in(soap, tag))
    return NULL;
  if (soap->null)
  { if (soap->enable_null)
    { soap->error = SOAP_NULL;
      return NULL;
    }
    else
      return p;
  }
  if (*soap->type != '\0'
      && soap_match_tag(soap, soap->type, type)
      && soap_match_tag(soap, soap->type, "integer")
      && soap_match_tag(soap, soap->type, "positiveInteger")
      && soap_match_tag(soap, soap->type, "negativeInteger")
      && soap_match_tag(soap, soap->type, "nonPositiveInteger")
      && soap_match_tag(soap, soap->type, "nonNegativeInteger")
      && soap_match_tag(soap, soap->type, "long")
      && soap_match_tag(soap, soap->type, "int")
      && soap_match_tag(soap, soap->type, "short")
      && soap_match_tag(soap, soap->type, "byte")
      && soap_match_tag(soap, soap->type, "unsignedLong")
      && soap_match_tag(soap, soap->type, "unsignedInt")
      && soap_match_tag(soap, soap->type, "unsignedShort")
      && soap_match_tag(soap, soap->type, "unsignedByte"))
  { soap->error = SOAP_TYPE_MISMATCH;
    soap_revert(soap);
    return NULL;
  }
  if (soap->body && !*soap->href)
  { p = (LONG64*)soap_id_enter(soap, soap->id, p, t, sizeof(LONG64), 0);
    if (!p)
      return NULL;
    s = soap_value(soap);
#ifdef win32
    if (!sscanf(s, "%I64d", p))
#else
    if (!sscanf(s, "%lld", p))
#endif
    { soap->error = SOAP_TYPE_MISMATCH;
      return NULL;;
    }
  }
  else
    p = (LONG64*)soap_id_forward(soap, soap->href, p, t, sizeof(LONG64));
  if (soap->body && soap_element_end_in(soap, tag))
    return NULL;
  return p;
}

/******************************************************************************/
SOAP_FMAC1
int
SOAP_FMAC2
soap_outbyte(struct soap *soap, const char *tag, int id, const char *p, const char *type, int n)
{ if (soap_element_begin_out(soap, tag, soap_embedded_id(soap, id, p, n), type))
    return soap->error;
  sprintf(soap->tagbuf, "%d", *p);
  if (soap_send(soap, soap->tagbuf))
    return soap->error;
  return soap_element_end_out(soap, tag);
}

/******************************************************************************/
SOAP_FMAC1
char *
SOAP_FMAC2
soap_inbyte(struct soap *soap, const char *tag, char *p, const char *type, int t)
{ char *s;
  if (soap_element_begin_in(soap, tag))
    return NULL;
  if (soap->null)
  { if (soap->enable_null)
    { soap->error = SOAP_NULL;
      return NULL;
    }
    else
      return p;
  }
  if (*soap->type != '\0'
      && soap_match_tag(soap, soap->type, type)
      && soap_match_tag(soap, soap->type, "byte")
      && soap_match_tag(soap, soap->type, "unsignedByte"))
  { soap->error = SOAP_TYPE_MISMATCH;
    soap_revert(soap);
    return NULL;
  }
  if (soap->body && !*soap->href)
  { p = (char*)soap_id_enter(soap, soap->id, p, t, sizeof(char), 0);
    if (!p)
      return NULL;
    s = soap_value(soap);
    *p = (char)atoi(s);
  }
  else
    p = (char*)soap_id_forward(soap, soap->href, p, t, sizeof(char));
  if (soap->body && soap_element_end_in(soap, tag))
    return NULL;
  return p;
}

/******************************************************************************/
SOAP_FMAC1
int
SOAP_FMAC2
soap_outshort(struct soap *soap, const char *tag, int id, const short *p, const char *type, int n)
{ if (soap_element_begin_out(soap, tag, soap_embedded_id(soap, id, p, n), type))
    return soap->error;
  sprintf(soap->tagbuf, "%d", (int)*p);
  if (soap_send(soap, soap->tagbuf))
    return soap->error;
  return soap_element_end_out(soap, tag);
}

/******************************************************************************/
SOAP_FMAC1
short *
SOAP_FMAC2
soap_inshort(struct soap *soap, const char *tag, short *p, const char *type, int t)
{ char *s;
  if (soap_element_begin_in(soap, tag))
    return NULL;
  if (soap->null)
  { if (soap->enable_null)
    { soap->error = SOAP_NULL;
      return NULL;
    }
    else
      return p;
  }
  if (*soap->type != '\0'
      && soap_match_tag(soap, soap->type, type)
      && soap_match_tag(soap, soap->type, "short")
      && soap_match_tag(soap, soap->type, "byte")
      && soap_match_tag(soap, soap->type, "unsignedShort")
      && soap_match_tag(soap, soap->type, "unsignedByte"))
  { soap->error = SOAP_TYPE_MISMATCH;
    soap_revert(soap);
    return NULL;
  }
  if (soap->body && !*soap->href)
  { p = (short*)soap_id_enter(soap, soap->id, p, t, sizeof(short), 0);
    if (!p)
      return NULL;
    s = soap_value(soap);
    if (!sscanf(s, "%hd", p))
    { soap->error = SOAP_TYPE_MISMATCH;
      return NULL;;
    }
  }
  else
    p = (short*)soap_id_forward(soap, soap->href, p, t, sizeof(short));
  if (soap->body && soap_element_end_in(soap, tag))
    return NULL;
  return p;
}

/******************************************************************************/
SOAP_FMAC1
int
SOAP_FMAC2
soap_outfloat(struct soap *soap, const char *tag, int id, const float *p, const char *type, int n)
{ const char *t;
  if (soap_element_begin_out(soap, tag, soap_embedded_id(soap, id, p, n), type))
    return soap->error;
  if (isnan(*p))
    t = "NaN";
  else if (*p >= FLT_PINFTY)
    t = "INF";
  else if (*p <= FLT_NINFTY)
    t = "-INF";
  else
  { sprintf(soap->tagbuf, soap->float_format, *p);
    t = soap->tagbuf;
  }
  if (soap_send(soap, t))
    return soap->error;
  return soap_element_end_out(soap, tag);
}

/******************************************************************************/
SOAP_FMAC1
float *
SOAP_FMAC2
soap_infloat(struct soap *soap, const char *tag, float *p, const char *type, int t)
{ char *s;
  if (soap_element_begin_in(soap, tag))
    return NULL;
  if (soap->null)
  { if (soap->enable_null)
    { soap->error = SOAP_NULL;
      return NULL;
    }
    else
      return p;
  }
  if (*soap->type != '\0'
      && soap_match_tag(soap, soap->type, type)
      && soap_match_tag(soap, soap->type, "float")
      && soap_match_tag(soap, soap->type, "decimal")
      && soap_match_tag(soap, soap->type, "integer")
      && soap_match_tag(soap, soap->type, "positiveInteger")
      && soap_match_tag(soap, soap->type, "negativeInteger")
      && soap_match_tag(soap, soap->type, "nonPositiveInteger")
      && soap_match_tag(soap, soap->type, "nonNegativeInteger")
      && soap_match_tag(soap, soap->type, "long")
      && soap_match_tag(soap, soap->type, "int")
      && soap_match_tag(soap, soap->type, "short")
      && soap_match_tag(soap, soap->type, "byte")
      && soap_match_tag(soap, soap->type, "unsignedLong")
      && soap_match_tag(soap, soap->type, "unsignedInt")
      && soap_match_tag(soap, soap->type, "unsignedShort")
      && soap_match_tag(soap, soap->type, "unsignedByte"))
  { soap->error = SOAP_TYPE_MISMATCH;
    soap_revert(soap);
    return NULL;
  }
  if (soap->body && !*soap->href)
  { p = (float*)soap_id_enter(soap, soap->id, p, t, sizeof(float), 0);
    if (!p)
      return NULL;
    s = soap_value(soap);
    if (!soap_tag_cmp(s, "INF"))
      *p = FLT_PINFTY;
    else if (!soap_tag_cmp(s, "+INF"))
      *p = FLT_PINFTY;
    else if (!soap_tag_cmp(s, "-INF"))
      *p = FLT_NINFTY;
    else if (!soap_tag_cmp(s, "NaN"))
      *p = FLT_NAN;
    else
    { *p = strtod(s, &s);
      if (*s)
      { soap->error = SOAP_TYPE_MISMATCH;
        return NULL;;
      }
    }
  }
  else
    p = (float*)soap_id_forward(soap, soap->href, p, t, sizeof(float));
  if (soap->body && soap_element_end_in(soap, tag))
    return NULL;
  return p;
}

/******************************************************************************/
SOAP_FMAC1
int
SOAP_FMAC2
soap_outdouble(struct soap *soap, const char *tag, int id, const double *p, const char *type, int n)
{ const char *t;
  if (soap_element_begin_out(soap, tag, soap_embedded_id(soap, id, p, n), type))
    return soap->error;
  if (isnan(*p))
    t = "NaN";
  else if (*p >= DBL_PINFTY)
    t = "INF";
  else if (*p <= DBL_NINFTY)
    t = "-INF";
  else
  { sprintf(soap->tagbuf, soap->double_format, *p);
    t = soap->tagbuf;
  }
  if (soap_send(soap, t))
    return soap->error;
  return soap_element_end_out(soap, tag);
}

/******************************************************************************/
SOAP_FMAC1
double *
SOAP_FMAC2
soap_indouble(struct soap *soap, const char *tag, double *p, const char *type, int t)
{ char *s;
  if (soap_element_begin_in(soap, tag))
    return NULL;
  if (soap->null)
  { if (soap->enable_null)
    { soap->error = SOAP_NULL;
      return NULL;
    }
    else
      return p;
  }
  if (*soap->type != '\0'
      && soap_match_tag(soap, soap->type, type)
      && soap_match_tag(soap, soap->type, "float")
      && soap_match_tag(soap, soap->type, "double")
      && soap_match_tag(soap, soap->type, "decimal")
      && soap_match_tag(soap, soap->type, "integer")
      && soap_match_tag(soap, soap->type, "positiveInteger")
      && soap_match_tag(soap, soap->type, "negativeInteger")
      && soap_match_tag(soap, soap->type, "nonPositiveInteger")
      && soap_match_tag(soap, soap->type, "nonNegativeInteger")
      && soap_match_tag(soap, soap->type, "long")
      && soap_match_tag(soap, soap->type, "int")
      && soap_match_tag(soap, soap->type, "short")
      && soap_match_tag(soap, soap->type, "byte")
      && soap_match_tag(soap, soap->type, "unsignedLong")
      && soap_match_tag(soap, soap->type, "unsignedInt")
      && soap_match_tag(soap, soap->type, "unsignedShort")
      && soap_match_tag(soap, soap->type, "unsignedByte"))
  { soap->error = SOAP_TYPE_MISMATCH;
    soap_revert(soap);
    return NULL;
  }
  if (soap->body && !*soap->href)
  { p = (double*)soap_id_enter(soap, soap->id, p, t, sizeof(double), 0);
    if (!p)
      return NULL;
    s = soap_value(soap);
    if (!soap_tag_cmp(s, "INF"))
      *p = DBL_PINFTY;
    else if (!soap_tag_cmp(s, "+INF"))
      *p = DBL_PINFTY;
    else if (!soap_tag_cmp(s, "-INF"))
      *p = DBL_NINFTY;
    else if (!soap_tag_cmp(s, "NaN"))
      *p = DBL_NAN;
    else
    { *p = strtod(s, &s);
      if (*s)
      { soap->error = SOAP_TYPE_MISMATCH;
        return NULL;;
      }
    }
  }
  else
    p = (double*)soap_id_forward(soap, soap->href, p, t, sizeof(double));
  if (soap->body && soap_element_end_in(soap, tag))
    return NULL;
  return p;
}

/******************************************************************************/
SOAP_FMAC1
int
SOAP_FMAC2
soap_outunsignedByte(struct soap *soap, const char *tag, int id, const unsigned char *p, const char *type, int n)
{ if (soap_element_begin_out(soap, tag, soap_embedded_id(soap, id, p, n), type))
    return soap->error;
  sprintf(soap->tagbuf, "%u", (unsigned int)*p);
  if (soap_send(soap, soap->tagbuf))
    return soap->error;
  return soap_element_end_out(soap, tag);
}

/******************************************************************************/
SOAP_FMAC1
unsigned char *
SOAP_FMAC2
soap_inunsignedByte(struct soap *soap, const char *tag, unsigned char *p, const char *type, int t)
{ char *s;
  if (soap_element_begin_in(soap, tag))
    return NULL;
  if (soap->null)
  { if (soap->enable_null)
    { soap->error = SOAP_NULL;
      return NULL;
    }
    else
      return p;
  }
  if (*soap->type != '\0'
      && soap_match_tag(soap, soap->type, type)
      && soap_match_tag(soap, soap->type, "unsignedByte"))
  { soap->error = SOAP_TYPE_MISMATCH;
    soap_revert(soap);
    return NULL;
  }
  if (soap->body && !*soap->href)
  { p = (unsigned char*)soap_id_enter(soap, soap->id, p, t, sizeof(unsigned char), 0);
    if (!p)
      return NULL;
    s = soap_value(soap);
    *p = (unsigned char)atoi(s);
  }
  else
    p = (unsigned char*)soap_id_forward(soap, soap->href, p, t, sizeof(unsigned char));
  if (soap->body && soap_element_end_in(soap, tag))
    return NULL;
  return p;
}

/******************************************************************************/
SOAP_FMAC1
int
SOAP_FMAC2
soap_outunsignedShort(struct soap *soap, const char *tag, int id, const unsigned short *p, const char *type, int n)
{ if (soap_element_begin_out(soap, tag, soap_embedded_id(soap, id, p, n), type))
    return soap->error;
  sprintf(soap->tagbuf, "%u", (unsigned int)*p);
  if (soap_send(soap, soap->tagbuf))
    return soap->error;
  return soap_element_end_out(soap, tag);
}

/******************************************************************************/
SOAP_FMAC1
unsigned short *
SOAP_FMAC2
soap_inunsignedShort(struct soap *soap, const char *tag, unsigned short *p, const char *type, int t)
{ char *s;
  if (soap_element_begin_in(soap, tag))
    return NULL;
  if (soap->null)
  { if (soap->enable_null)
    { soap->error = SOAP_NULL;
      return NULL;
    }
    else
      return p;
  }
  if (*soap->type != '\0'
      && soap_match_tag(soap, soap->type, type)
      && soap_match_tag(soap, soap->type, "unsignedShort")
      && soap_match_tag(soap, soap->type, "unsignedByte"))
  { soap->error = SOAP_TYPE_MISMATCH;
    soap_revert(soap);
    return NULL;
  }
  if (soap->body && !*soap->href)
  { p = (unsigned short*)soap_id_enter(soap, soap->id, p, t, sizeof(unsigned short), 0);
    if (!p)
      return NULL;
    s = soap_value(soap);
    if (!sscanf(s, "%hu", p))
    { soap->error = SOAP_TYPE_MISMATCH;
      return NULL;;
    }
  }
  else
    p = (unsigned short*)soap_id_forward(soap, soap->href, p, t, sizeof(unsigned short));
  if (soap->body && soap_element_end_in(soap, tag))
    return NULL;
  return p;
}

/******************************************************************************/
SOAP_FMAC1
int
SOAP_FMAC2
soap_outunsignedInt(struct soap *soap, const char *tag, int id, const unsigned int *p, const char *type, int n)
{ if (soap_element_begin_out(soap, tag, soap_embedded_id(soap, id, p, n), type))
    return soap->error;
  sprintf(soap->tagbuf, "%u", *p);
  if (soap_send(soap, soap->tagbuf))
    return soap->error;
  return soap_element_end_out(soap, tag);
}

/******************************************************************************/
SOAP_FMAC1
unsigned int *
SOAP_FMAC2
soap_inunsignedInt(struct soap *soap, const char *tag, unsigned int *p, const char *type, int t)
{ char *s;
  if (soap_element_begin_in(soap, tag))
    return NULL;
  if (soap->null)
  { if (soap->enable_null)
    { soap->error = SOAP_NULL;
      return NULL;
    }
    else
      return p;
  }
  if (*soap->type != '\0'
      && soap_match_tag(soap, soap->type, type)
      && soap_match_tag(soap, soap->type, "unsignedInt")
      && soap_match_tag(soap, soap->type, "unsignedShort")
      && soap_match_tag(soap, soap->type, "unsignedByte"))
  { soap->error = SOAP_TYPE_MISMATCH;
    soap_revert(soap);
    return NULL;
  }
  if (soap->body && !*soap->href)
  { p = (unsigned int*)soap_id_enter(soap, soap->id, p, t, sizeof(unsigned int), 0);
    if (!p)
      return NULL;
    s = soap_value(soap);
    *p = strtoul(s, &s, 10);
    if (*s)
    { soap->error = SOAP_TYPE_MISMATCH;
      return NULL;;
    }
  }
  else
    p = (unsigned int*)soap_id_forward(soap, soap->href, p, t, sizeof(unsigned int));
  if (soap->body && soap_element_end_in(soap, tag))
    return NULL;
  return p;
}

/******************************************************************************/
SOAP_FMAC1
int
SOAP_FMAC2
soap_outunsignedLong(struct soap *soap, const char *tag, int id, const unsigned long *p, const char *type, int n)
{ if (soap_element_begin_out(soap, tag, soap_embedded_id(soap, id, p, n), type))
    return soap->error;
  sprintf(soap->tagbuf, "%lu", *p);
  if (soap_send(soap, soap->tagbuf))
    return soap->error;
  return soap_element_end_out(soap, tag);
}

/******************************************************************************/
SOAP_FMAC1
unsigned long *
SOAP_FMAC2
soap_inunsignedLong(struct soap *soap, const char *tag, unsigned long *p, const char *type, int t)
{ char *s;
  if (soap_element_begin_in(soap, tag))
    return NULL;
  if (soap->null)
  { if (soap->enable_null)
    { soap->error = SOAP_NULL;
      return NULL;
    }
    else
      return p;
  }
  if (*soap->type != '\0'
      && soap_match_tag(soap, soap->type, type)
      && soap_match_tag(soap, soap->type, "unsignedLong")
      && soap_match_tag(soap, soap->type, "unsignedInt")
      && soap_match_tag(soap, soap->type, "unsignedShort")
      && soap_match_tag(soap, soap->type, "unsignedByte"))
  { soap->error = SOAP_TYPE_MISMATCH;
    soap_revert(soap);
    return NULL;
  }
  if (soap->body && !*soap->href)
  { p = (unsigned long*)soap_id_enter(soap, soap->id, p, t, sizeof(unsigned long), 0);
    if (!p)
      return NULL;
    s = soap_value(soap);
    *p = strtoul(s, &s, 10);
    if (*s)
    { soap->error = SOAP_TYPE_MISMATCH;
      return NULL;;
    }
  }
  else
    p = (unsigned long*)soap_id_forward(soap, soap->href, p, t, sizeof(unsigned long));
  if (soap->body && soap_element_end_in(soap, tag))
    return NULL;
  return p;
}

/******************************************************************************/
SOAP_FMAC1
int
SOAP_FMAC2
soap_outULONG64(struct soap *soap, const char *tag, int id, const ULONG64 *p, const char *type, int n)
{ if (soap_element_begin_out(soap, tag, soap_embedded_id(soap, id, p, n), type))
    return soap->error;
#ifdef win32
  sprintf(soap->tagbuf, "%I64u", *p);
#else
  sprintf(soap->tagbuf, "%llu", *p);
#endif
  if (soap_send(soap, soap->tagbuf))
    return soap->error;
  return soap_element_end_out(soap, tag);
}

/******************************************************************************/
SOAP_FMAC1
ULONG64 *
SOAP_FMAC2
soap_inULONG64(struct soap *soap, const char *tag, ULONG64 *p, const char *type, int t)
{ char *s;
  if (soap_element_begin_in(soap, tag))
    return NULL;
  if (soap->null)
  { if (soap->enable_null)
    { soap->error = SOAP_NULL;
      return NULL;
    }
    else
      return p;
  }
  if (*soap->type != '\0'
      && soap_match_tag(soap, soap->type, type)
      && soap_match_tag(soap, soap->type, "positiveInteger")
      && soap_match_tag(soap, soap->type, "nonNegativeInteger")
      && soap_match_tag(soap, soap->type, "unsignedLong")
      && soap_match_tag(soap, soap->type, "unsignedInt")
      && soap_match_tag(soap, soap->type, "unsignedShort")
      && soap_match_tag(soap, soap->type, "unsignedByte"))
  { soap->error = SOAP_TYPE_MISMATCH;
    soap_revert(soap);
    return NULL;
  }
  if (soap->body && !*soap->href)
  { p = (ULONG64*)soap_id_enter(soap, soap->id, p, t, sizeof(ULONG64), 0);
    if (!p)
      return NULL;
    s = soap_value(soap);
#ifdef win32
    if (!sscanf(s, "%I64u", p))
#else
    if (!sscanf(s, "%llu", p))
#endif
    { soap->error = SOAP_TYPE_MISMATCH;
      return NULL;;
    }
  }
  else
    p = (ULONG64*)soap_id_forward(soap, soap->href, p, t, sizeof(ULONG64));
  if (soap->body && soap_element_end_in(soap, tag))
    return NULL;
  return p;
}

/******************************************************************************/
SOAP_FMAC1
int
SOAP_FMAC2
soap_outstring(struct soap *soap, const char *tag, int id, char *const*p, const char *type, int n) 
{ if (!*p)
  { if (soap_element_null(soap, tag, id, type))
      return soap->error;
  }
  else
  { struct soap_plist *pp;
    int i = soap_pointer_lookup(soap, *p, n, &pp);
    if (id > 0)
    { if (i)
      { if (soap_element_begin_out(soap, tag, id, type) || soap_convert_string_out(soap, *p) || soap_element_end_out(soap, tag))
          return soap->error;
	soap_set_embedded(soap, pp);
      }
      else
      { i = soap_pointer_enter(soap, *p, n, &pp);
        if (soap_element_begin_out(soap, tag, id, type) || soap_convert_string_out(soap, *p) || soap_element_end_out(soap, tag))
	  return soap->error;
        if (soap->counting)
          pp->mark1 = 0;
        else
          pp->mark2 = 0;
      }
    }
    else if (i)
    { if (soap_is_embedded(soap, pp))
      { if (soap_element_ref(soap, tag, 0, i))
	  return soap->error;
      }
      else if (soap_is_single(soap, pp))
      { if (soap_element_begin_out(soap, tag, 0, type) || soap_convert_string_out(soap, *p) || soap_element_end_out(soap, tag))
          return soap->error;
      }
      else
      { if (soap_element_begin_out(soap, tag, i, type) || soap_convert_string_out(soap, *p) || soap_element_end_out(soap, tag))
          return soap->error;
	soap_set_embedded(soap, pp);
      }
    }
    else
    { soap_pointer_enter(soap, *p, n, &pp);
      if (soap_element_begin_out(soap, tag, id, type) || soap_convert_string_out(soap, *p) || soap_element_end_out(soap, tag))
        return soap->error;
      if (soap->counting)
        pp->mark1 = 0;
      else
        pp->mark2 = 0;
    }
  }
  return SOAP_OK;
}

/******************************************************************************/
SOAP_FMAC1
char **
SOAP_FMAC2
soap_instring(struct soap *soap, const char *tag, char **p, const char *type, int t)
{ int i, c, n;
  char *s;
  if (soap_element_begin_in(soap, tag))
    return NULL;
  if (soap->null)
  { p = (char**)soap_id_enter(soap, soap->id, p, t, sizeof(char**), 0);
    if (p)
      *p = NULL;
  }
  else if (soap->body && !*soap->href)
  { if (soap_match_tag(soap, soap->type, "PointerTostring") == 0)
    { p = (char**)soap_id_enter(soap, soap->id, p, t, sizeof(char**), 0);
      p = (char**)soap_instring(soap, "string", p, type, t);
    }
    else
    { if (!p)
      { if ((p = (char**)soap_id_enter(soap, "", p, t, sizeof(char**), 0)) == NULL)
	  return NULL;
      }
      n = 0;
      if (soap_new_block(soap))
        return NULL;
      for (;;)
      { if (!(s = (char*)soap_push_block(soap, SOAP_BLKLEN)))
	  return NULL;
	for (i = 0; i < SOAP_BLKLEN; i++)
        { c = soap_get(soap);
	  switch (c)
	  {
	  case EOF:
            goto soap_str_end;
	  case TT:
	    if (n == 0)
	      goto soap_str_end;
	    n--;
	    *s++ = '<';
	    soap_unget(soap, '/');
	    break;
	  case LT:
	    n++;
	    *s++ = '<';
	    break;
	  case GT:
	    *s++ = '>';
	    break;
	  case QT:
	    *s++ = '"';
	    break;
	  case AP:
	    *s++ = '\'';
	    break;
	  case '/':
	    c = soap_get(soap);
	    if (c == GT)
	      n--;
	    *s++ = '/';
	    soap_unget(soap, c);
	    break;
	  default:
	    if (c >= 0x80 && !soap->enable_utf_string)
	      *s++ = ((c&0x03)<<6)|((char)soap_get(soap)&0x3F);
            else if (c < 0 && soap->enable_utf_string)
  	    { c &= 0xFFFF;
	      if (c < 0x0800)
	      { *s++ = 0xA0|((c>>6)&0x1F);
                soap_unget(soap, 0x80|(c&0x3F));
              }
	      else
	      { *s++ = 0xC0|((c>>12)&0x0F);
                soap_unget(soap, 0x80|((c>>6)&0x3F));
                soap_unget2(soap, 0x80|(c&0x3F));
	      }
	    }
	    else
	      *s++ = c;
          }
        }
      }
soap_str_end:
      soap_unget(soap, c);
      *s = '\0';
      soap->blist->size = soap->blist->size - SOAP_BLKLEN + i + 1;
      *p = (char*)soap_id_enter(soap, soap->id, NULL, t, soap->blist->size, 0);
      if (*p)
        soap_store_block(soap, *p);
      else
        return NULL;
    }
  }
  else
    p = (char**)soap_id_lookup(soap, soap->href, (void**)p, t, sizeof(char*), 0);
  if (soap->body && soap_element_end_in(soap, tag))
    return NULL;
  return p;
}

/******************************************************************************/
SOAP_FMAC1
int
SOAP_FMAC2
soap_outwstring(struct soap *soap, const char *tag, int id, wchar_t *const*p, const char *type, int n) 
{ if (!*p)
  { if (soap_element_null(soap, tag, id, type))
      return soap->error;
  }
  else
  { struct soap_plist *pp;
    int i = soap_pointer_lookup(soap, *p, n, &pp);
    if (id > 0)
    { if (i)
      { if (soap_element_begin_out(soap, tag, id, type) || soap_convert_wstring_out(soap, *p) || soap_element_end_out(soap, tag))
          return soap->error;
	soap_set_embedded(soap, pp);
      }
      else
      { i = soap_pointer_enter(soap, *p, n, &pp);
        if (soap_element_begin_out(soap, tag, id, type) || soap_convert_wstring_out(soap, *p) || soap_element_end_out(soap, tag))
	  return soap->error;
        if (soap->counting)
          pp->mark1 = 0;
        else
          pp->mark2 = 0;
      }
    }
    else if (i)
    { if (soap_is_embedded(soap, pp))
      { if (soap_element_ref(soap, tag, 0, i))
          return soap->error;
      }
      else if (soap_is_single(soap, pp))
      { if (soap_element_begin_out(soap, tag, 0, type) || soap_convert_wstring_out(soap, *p) || soap_element_end_out(soap, tag))
          return soap->error;
      }
      else
      { if (soap_element_begin_out(soap, tag, i, type) || soap_convert_wstring_out(soap, *p) || soap_element_end_out(soap, tag))
          return soap->error;
	soap_set_embedded(soap, pp);
      }
    }
    else
    { if (soap_element_begin_out(soap, tag, id, type) || soap_convert_wstring_out(soap, *p) || soap_element_end_out(soap, tag))
        return soap->error;
      if (soap->counting)
        pp->mark1 = 0;
      else
        pp->mark2 = 0;
    }
  }
  return SOAP_OK;
}

/******************************************************************************/
SOAP_FMAC1
wchar_t **
SOAP_FMAC2
soap_inwstring(struct soap *soap, const char *tag, wchar_t **p, const char *type, int t)
{ int i, c, n;
  wchar_t *s;
  if (soap_element_begin_in(soap, tag))
    return NULL;
  if (soap->null)
  { p = (wchar_t**)soap_id_enter(soap, soap->id, p, t, sizeof(wchar_t**), 0);
    if (p)
      *p = NULL;
  }
  else if (soap->body && !*soap->href)
  { if (soap_match_tag(soap, soap->type, "PointerTostring") == 0)
      p = (wchar_t**)soap_inwstring(soap, "string", (wchar_t**)soap_id_enter(soap, soap->id, p, t, sizeof(wchar_t**), 0), type, t);
    else
    { if (!p)
      { if (!(p = (wchar_t**)soap_id_enter(soap, "", p, t, sizeof(wchar_t**), 0)))
	  return NULL;
      }
      n = 0;
      if (soap_new_block(soap))
        return NULL;
      for (;;)
      { if (!(s = (wchar_t*)soap_push_block(soap, sizeof(wchar_t)*SOAP_BLKLEN)))
          return NULL;
        for (i = 0; i < SOAP_BLKLEN; i++)
        { c = soap_getutf8(soap);
	  switch (c)
	  {
	  case EOF:
            goto soap_str_end;
	  case '&':
	    *s++ = (wchar_t)(soap_char(soap)&0x7FFFFFFF);
	    break;
	  case '<':
	    c = soap_getutf8(soap);
	    if (c == '/')
	      if (n == 0)
	      { c = TT;
                goto soap_str_end;
	      }
	      else
	        n--;
	    else
	      n++;
	    *s++ = '<';
	    soap_unget(soap, c);
	    break;
          case '/':
	    c = soap_getutf8(soap);
	    if (c == '>')
	      n--;
	    *s++ = '/';
	    soap_unget(soap, c);
	    break;
	  default:
            *s++ = c&0x7FFFFFFF;
	  }
        }
      }
soap_str_end:
      soap_unget(soap, c);
      *s = '\0';
      soap->blist->size = soap->blist->size - sizeof(wchar_t)*(SOAP_BLKLEN-i-1);
      *p = (wchar_t*)soap_id_enter(soap, soap->id, NULL, t, soap->blist->size, 0);
      if (*p)
        soap_store_block(soap, (char*)*p);
      else
        return NULL;
    }
  }
  else
    p = (wchar_t**)soap_id_lookup(soap, soap->href, (void**)p, t, sizeof(wchar_t*), 0);
  if (soap->body && soap_element_end_in(soap, tag))
    return NULL;
  return p;
}

#ifndef UNDER_CE
/******************************************************************************/
SOAP_FMAC1
int
SOAP_FMAC2
soap_outdateTime(struct soap *soap, const char *tag, int id, const time_t *p, const char *type, int n)
{ struct tm T;
  if (soap_element_begin_out(soap, tag, soap_embedded_id(soap, id, p, n), type))
    return soap->error;
#if defined(WIN32) || defined(__APPLE__)
  strftime(soap->tagbuf, 1024, "%Y-%m-%dT%H:%M:%SZ", localtime(p));
#else
  strftime(soap->tagbuf, 1024, "%Y-%m-%dT%H:%M:%SZ", localtime_r(p, &T));
#endif
  if (soap_send(soap, soap->tagbuf))
    return soap->error;
  return soap_element_end_out(soap, tag);
}

/******************************************************************************/
SOAP_FMAC1
time_t *
SOAP_FMAC2
soap_indateTime(struct soap *soap, const char *tag, time_t *p, const char * type, int t)
{ char *s;
  struct tm T;
  if (soap_element_begin_in(soap, tag))
    return NULL;
  if (soap->null)
  { if (soap->enable_null)
    { soap->error = SOAP_NULL;
      return NULL;
    }
    else
      return p;
  }
  if (*soap->type != '\0'
      && soap_match_tag(soap, soap->type, type)
      && soap_match_tag(soap, soap->type, "dateTime"))
  { soap->error = SOAP_TYPE_MISMATCH;
    soap_revert(soap);
    return NULL;
  }
  if (soap->body && !*soap->href)
  { p = (time_t*)soap_id_enter(soap, soap->id, p, t, sizeof(time_t), 0);
    if (!p)
      return NULL;
    s = soap_value(soap);
    T.tm_hour = 0;
    T.tm_min = 0;
    T.tm_sec = 0;
    sscanf(s, "%d-%d-%dT%d:%d:%d", &T.tm_year, &T.tm_mon, &T.tm_mday, &T.tm_hour, &T.tm_min, &T.tm_sec);
    if (T.tm_year <= 1901)
      *p = INT_MIN;
    else if (T.tm_year >= 2038)
      *p = INT_MAX;
    else
    { T.tm_year -= 1900;
      T.tm_mon--;
      T.tm_isdst = 0;
      *p = mktime(&T);
    }
  }
  else
    p = (time_t*)soap_id_forward(soap, soap->href, p, t, sizeof(time_t));
  if (soap->body && soap_element_end_in(soap, tag))
    return NULL;
  return p;
}
#endif

/******************************************************************************/
SOAP_FMAC1
int
SOAP_FMAC2
soap_outliteral(struct soap *soap, const char *tag, char *const*p)
{ int i;
  const char *t;
  if ((t = strchr(tag, ':')))
  { strncpy(soap->tagbuf, tag, t-tag);
    soap->tagbuf[t-tag] = '\0';
    for (i = 0; soap->namespaces[i].id; i++)
      if (!strcmp(soap->tagbuf, soap->namespaces[i].id))
        break;
    sprintf(soap->tagbuf, "<%s xmlns=\"%s\">", t+1, soap->namespaces[i].ns ? soap->namespaces[i].ns : "");
  }
  else
    sprintf(soap->tagbuf, "<%s>", tag);
  if (soap_send(soap, soap->tagbuf))
    return soap->error;
  if (p && *p)
  { if (soap_send(soap, *p))
      return soap->error;
  }
  if (t)
    sprintf(soap->tagbuf, "</%s>", t+1);
  else
    sprintf(soap->tagbuf, "</%s>", tag);
  return soap_send(soap, soap->tagbuf);
}

/******************************************************************************/
SOAP_FMAC1
char **
SOAP_FMAC2
soap_inliteral(struct soap *soap, const char *tag, char **p)
{ int i, c, n;
  char *s;
  if (soap_element_begin_in(soap, tag))
    return NULL;
  if (!p)
    if (!(p = (char**)soap_malloc(soap, sizeof(char*))))
      return NULL;
  if (soap->null)
    *p = NULL;
  else if (soap->body)
  { n = 0;
      if (soap_new_block(soap))
        return NULL;
      for (;;)
      { if (!(s = (char*)soap_push_block(soap, SOAP_BLKLEN)))
          return NULL;
        for (i = 0; i < SOAP_BLKLEN; i++)
        { c = soap_get2(soap);
	  switch (c)
	  {
	  case EOF:
            goto soap_str_end;
	  case '<':
	    c = soap_get2(soap);
	    if (c == '/')
	      if (n == 0)
	      { c = TT;
                goto soap_str_end;
	      }
	      else
	        n--;
	    else
	      n++;
	    *s++ = '<';
	    soap_unget(soap, c);
	    break;
          case '/':
	    c = soap_get2(soap);
	    if (c == '>')
	      n--;
	    *s++ = '/';
	    soap_unget(soap, c);
	    break;
	  default:
            *s++ = c;
	  }
        }
      }
soap_str_end:
      soap_unget(soap, c);
      *s = '\0';
      soap->blist->size = soap->blist->size - SOAP_BLKLEN + i + 1;
      *p = (char*)soap_malloc(soap, soap->blist->size);
      if (*p)
        soap_store_block(soap, (char*)*p);
      else
        return NULL;
  }
  else
    *p = NULL;
  if (soap->body && soap_element_end_in(soap, tag))
    return NULL;
  return p;
}

/******************************************************************************/
SOAP_FMAC1
int
SOAP_FMAC2
soap_outwliteral(struct soap *soap, const char *tag, wchar_t *const*p)
{ int i;
  const char *t;
  wchar_t c;
  const wchar_t *s;
  if ((t = strchr(tag, ':')))
  { strncpy(soap->tagbuf, tag, t-tag);
    soap->tagbuf[t-tag] = '\0';
    for (i = 0; soap->namespaces[i].id; i++)
      if (!strcmp(soap->tagbuf, soap->namespaces[i].id))
        break;
    sprintf(soap->tagbuf, "<%s xmlns=\"%s\">", t+1, soap->namespaces[i].ns ? soap->namespaces[i].ns : "");
  }
  else
    sprintf(soap->tagbuf, "<%s>", tag);
  if (soap_send(soap, soap->tagbuf))
    return soap->error;
  if (p)
  { s = *p;
    while ((c = *s++))
      if (soap_pututf8(soap, c))
        return soap->error;
  }
  if (t)
    sprintf(soap->tagbuf,"</%s>", t+1);
  else
    sprintf(soap->tagbuf,"</%s>", tag);
  return soap_send(soap, soap->tagbuf);
}

/******************************************************************************/
SOAP_FMAC1
wchar_t **
SOAP_FMAC2
soap_inwliteral(struct soap *soap, const char *tag, wchar_t **p)
{ int i, c, n;
  wchar_t *s;
  if (soap_element_begin_in(soap, tag))
    return NULL;
  if (!p)
    if (!(p = (wchar_t**)soap_malloc(soap, sizeof(wchar_t*))))
      return NULL;
  if (soap->null)
    *p = NULL;
  else if (soap->body)
  { n = 0;
      if (soap_new_block(soap))
        return NULL;
      for (;;)
      { if (!(s = (wchar_t*)soap_push_block(soap, sizeof(wchar_t)*SOAP_BLKLEN)))
          return NULL;
        for (i = 0; i < SOAP_BLKLEN; i++)
        { c = soap_getutf8(soap);
	  switch (c)
	  {
	  case EOF:
            goto soap_str_end;
	  case '<':
	    c = soap_getutf8(soap);
	    if (c == '/')
	      if (n == 0)
              { c = TT;
	        goto soap_str_end;
              }
	      else
	        n--;
	    else
	      n++;
	    *s++ = '<';
	    soap_unget(soap, c);
	    break;
          case '/':
	    c = soap_getutf8(soap);
	    if (c == '>')
	      n--;
	    *s++ = '/';
	    soap_unget(soap, c);
	    break;
	  default:
            *s++ = c&0x7FFFFFFF;
	  }
        }
      }
soap_str_end:
      soap_unget(soap, c);
      *s = '\0';
      soap->blist->size = soap->blist->size - sizeof(wchar_t)*(SOAP_BLKLEN-i-1);
      *p = (wchar_t*)soap_malloc(soap, soap->blist->size);
      if (*p)
        soap_store_block(soap, (char*)*p);
      else
        return NULL;
  }
  else
    *p = NULL;
  if (soap->body && soap_element_end_in(soap, tag))
    return NULL;
  return p;
}

/******************************************************************************/
SOAP_FMAC1
char *
SOAP_FMAC2
soap_value(struct soap *soap)
{ size_t i;
  wchar c = 0;
  char *s = soap->tagbuf;
  for (i = 0; i < sizeof(soap->tagbuf)-1; i++)
  { c = soap_get(soap);
    if (c == TT || c == EOF || blank(c))
      break;
    *s++ = (char)c;
  }
  while (blank(c))
    c = soap_get(soap);
  soap_unget(soap, c);
  *s = '\0';
  return soap->tagbuf;
}

/******************************************************************************/
SOAP_FMAC1
int
SOAP_FMAC2
soap_getline(struct soap *soap, char *s, int len)
{ int i;
  wchar c = 0;
  for (i = 1; i < len; i++)
  { c = soap_get2(soap);
    if (c == '\r' || c == '\n' || c == EOF)
      break;
    *s++ = (char)c;
  }
  *s = '\0';
  while (c != '\n' && c != EOF)
    c = soap_get2(soap);
  if (c == EOF)
    return SOAP_EOF;
  return SOAP_OK;
}

/******************************************************************************/
SOAP_FMAC1
wchar
SOAP_FMAC2
soap_getlineto(struct soap *soap, char *s, int len, const char *stop)
{ int i;
  wchar c = 0, c1;
  for (i = 1; i < len; i++)
  { if ((c = soap_get2(soap)) == EOF)
      return EOF;
    if (c == '\r' || c == '\n' || strchr(stop, (int)c))
      break;
    *s++ = (char)c;
  }
  *s = '\0';
  if (c == '\r')
    c = soap_get2(soap);
  if (c != '\n' && c != EOF) /* skip blanks but not \n after stop char */
  { do c1 = soap_get2(soap);
    while (blank(c1) && c1 != '\n' && c1 != EOF);
    if (c1 != EOF)
      soap_unget(soap, c1);
  }
  else if (c == '\n')
    soap_unget(soap, c);
  return c; /* return char stopped at or \n or EOF */
}

/******************************************************************************/
static int
soap_putdimefield(struct soap *soap, const char *s, int n)
{ if (soap_send_raw(soap, s, n))
    return soap->error;
  return soap_send_raw(soap, "\0\0\0", -n&3);
}

/******************************************************************************/
SOAP_FMAC1
char *
SOAP_FMAC2
soap_dime_option(struct soap *soap, unsigned short type, const char *option)
{ int n;
  char *s = NULL;
  if (option)
  { n = strlen(option);
    s = (char*)soap_malloc(soap, n+5);
    if (s)
    { s[0] = type>>8;
      s[1] = type&0xFF;
      s[2] = n>>8;
      s[3] = n&0xFF;
      strcpy(s+4, option);
    }
  }
  return s;
}

/******************************************************************************/
SOAP_FMAC1
int
SOAP_FMAC2
soap_putdimehdr(struct soap *soap)
{ unsigned char tmp[12];
  int optlen = 0, idlen = 0, typelen = 0;
  DBGLOG(TEST, SOAP_MESSAGE(fdebug,"\nput DIME header"));
  if (soap->dime_options)
    optlen = ((unsigned char)soap->dime_options[2]<<8 | (unsigned char)soap->dime_options[3]) + 4;
  if (soap->dime_id)
    idlen = strlen(soap->dime_id);
  if (soap->dime_type)
    typelen = strlen(soap->dime_type);
  tmp[0] = SOAP_DIME_VERSION|(soap->dime_flags&0x7);
  tmp[1] = soap->dime_flags&0xF0;
  tmp[2] = optlen>>8;
  tmp[3] = optlen&0xFF;
  tmp[4] = idlen>>8;
  tmp[5] = idlen&0xFF;
  tmp[6] = typelen>>8;
  tmp[7] = typelen&0xFF;
  tmp[8] = soap->dime_size>>24;
  tmp[9] = soap->dime_size>>16&0xFF;
  tmp[10] = soap->dime_size>>8&0xFF;
  tmp[11] = soap->dime_size&0xFF;
  if (soap_send_raw(soap, (char*)tmp, 12))
    return soap->error;
  if (soap_putdimefield(soap, soap->dime_options, optlen) || soap_putdimefield(soap, soap->dime_id, idlen) || soap_putdimefield(soap, soap->dime_type, typelen))
    return soap->error;
  return SOAP_OK;
}

/******************************************************************************/
SOAP_FMAC1
int
SOAP_FMAC2
soap_putdime(struct soap *soap, int i, char *id, char *type, char *options, void *ptr, size_t size)
{ if (id)
    soap->dime_id = id;
  else
  { sprintf(soap->tagbuf, soap->dime_id_format, i);
    soap->dime_id = soap->tagbuf;
  }
  soap->dime_type = type;
  soap->dime_options = options;
  soap->dime_size = size;
  soap->dime_flags = SOAP_DIME_VERSION|SOAP_DIME_MEDIA;
  if (--soap->dime_count == 0)
    soap->dime_flags |= SOAP_DIME_ME;
  if (soap_putdimehdr(soap) || soap_send_raw(soap, (char*)ptr, size) || soap_send_raw(soap, "\0\0\0", -(int)size&3))
    return soap->error;
  return SOAP_OK;
}

/******************************************************************************/
static char *
soap_getdimefield(struct soap *soap, int n)
{ register wchar c;
  register int i;
  register char *s;
  char *p = NULL;
  if (n)
  { p = (char*)soap_malloc(soap, n+1);
    if (p)
    { s = p;
      for (i = n; i > 0; i--)
      { if ((c = soap_get1(soap)) == EOF)
        { soap->error = SOAP_EOF;
	  return NULL;
        }
        *s++ = (char)c;
      }
      *s = '\0';
      if ((soap->error = soap_move(soap, -n&3)))
        return NULL;
    }
    else
      soap->error = SOAP_EOM;
  }
  return p;
}

/******************************************************************************/
SOAP_FMAC1
int
SOAP_FMAC2
soap_getdimehdr(struct soap *soap)
{ register wchar c;
  register char *s;
  register int i;
  unsigned char tmp[12];
  int optlen, idlen, typelen;
  if (!soap->dime)
    return soap->error = SOAP_EOD;
  DBGLOG(TEST, SOAP_MESSAGE(fdebug,"\nget DIME header"));
  if (soap->dime_buflen || soap->dime_chunksize)
  { if (soap_move(soap, soap->dime_size-soap_tell(soap)))
      return soap->error = SOAP_EOF;
    soap_unget(soap, soap_get2(soap)); /* skip padding and get hdr */
    DBGLOG(TEST, SOAP_MESSAGE(fdebug,"\nfrom chunked"));
    return SOAP_OK;
  }
  s = (char*)tmp;
  for (i = 12; i > 0; i--)
  { if ((c = soap_get2(soap)) == EOF)
      return soap->error = SOAP_EOF;
    *s++ = (char)c;
  }
  if ((tmp[0]&0xF8) != SOAP_DIME_VERSION)
    return soap->error = SOAP_DIME_VERSIONMISMATCH;
  soap->dime_flags = (tmp[0]&0x7) | (tmp[1]&0xF0);
  optlen = tmp[2]<<8 | tmp[3];
  idlen = tmp[4]<<8 | tmp[5];
  typelen = tmp[6]<<8 | tmp[7];
  soap->dime_size = tmp[8]<<24 | tmp[9]<<16 | tmp[10]<<8 | tmp[11];
  DBGLOG(TEST, SOAP_MESSAGE(fdebug,"\nDIME size=%u flags=0x%X", soap->dime_size, soap->dime_flags));
  soap->dime_options = soap_getdimefield(soap, optlen);
  if (soap->error)
    return soap->error;
  soap->dime_id = soap_getdimefield(soap, idlen);
  if (soap->error)
    return soap->error;
  soap->dime_type = soap_getdimefield(soap, typelen);
  if (soap->error)
    return soap->error;
  DBGLOG(TEST, SOAP_MESSAGE(fdebug,"\nDIME id=%s, type=%s, options=%s", soap->dime_id?soap->dime_id:"", soap->dime_type?soap->dime_type:"", soap->dime_options?soap->dime_options:""));
  if (soap->dime_flags&SOAP_DIME_ME)
    soap->dime = 0;
  return SOAP_OK;
}

/******************************************************************************/
SOAP_FMAC1
int
SOAP_FMAC2
soap_getdime(struct soap *soap)
{ if (soap_getdimehdr(soap))
    return soap->error;
  if (soap->dime_flags&SOAP_DIME_CF)
  { char *s, *id, *type, *options;
    int i, c;
    if (soap_new_block(soap))
      return SOAP_EOM;
    id = soap->dime_id;
    type = soap->dime_type;
    options = soap->dime_options;
    for (;;)
    { s = (char*)soap_push_block(soap, soap->dime_size);
      if (!s)
        return soap->error = SOAP_EOM;
      for (i = soap->dime_size; i > 0; i--)
      { if ((c = soap_get1(soap)) == EOF)
          return soap->error = SOAP_EOF;
        *s++ = c;
      }
      if (soap_move(soap, -(int)soap->dime_size&3))
        return soap->error = SOAP_EOF;
      if (!(soap->dime_flags&SOAP_DIME_CF))
        break;
      if (soap_getdimehdr(soap))
        return soap->error;
    }
    soap->dime_size = soap->blist->size;
    soap->dime_ptr = (char*)soap_malloc(soap, soap->dime_size+1);
    if (!soap->dime_ptr)
      return soap->error = SOAP_EOM;
    soap_push_block(soap, 0); /* dummy block with size 0 required to combine blocks of different length */
    soap_store_block(soap, soap->dime_ptr);
    soap->dime_ptr[soap->dime_size] = '\0'; /* make 0-terminated */
    soap->dime_id = id;
    soap->dime_type = type;
    soap->dime_options = options;
  }
  else
  { soap->dime_ptr = soap_getdimefield(soap, soap->dime_size);
    if (soap->error)
      return soap->error;
  }
  return SOAP_OK;
}

/******************************************************************************/

#ifdef WITH_COOKIES
/******************************************************************************/
SOAP_FMAC1
struct soap_cookie*
SOAP_FMAC2
soap_cookie(struct soap *soap, const char *name, const char *domain, const char *path)
{ struct soap_cookie *p;
  int n;
  if (!domain)
    domain = soap->cookie_domain;
  if (!path)
    path = soap->cookie_path;
  if (*path == '/')
   path++;
  n = strlen(path);
  for (p = soap->cookies; p; p = p->next)
    if (!strcmp(p->name, name) && !strcmp(p->domain, domain) && !strncmp(p->path, path, n))
      break;
  return p;
}

/******************************************************************************/
SOAP_FMAC1
struct soap_cookie*
SOAP_FMAC2
soap_set_cookie(struct soap *soap, const char *name, const char *value, const char *domain, const char *path)
{ struct soap_cookie **p, *q;
  int n;
  if (!domain)
    domain = soap->cookie_domain;
  if (!path)
    path = soap->cookie_path;
  if (*path == '/')
   path++;
  q = soap_cookie(soap, name, domain, path);
  if (!q)
  { if ((q = (struct soap_cookie*)malloc(sizeof(struct soap_cookie))))
    { if ((q->name = (char*)malloc(strlen(name)+1)))
        strcpy(q->name, name);
      q->value = NULL;
      q->domain = NULL;
      q->path = NULL;
      q->expire = -1;
      q->version = 0;
      q->secure = 0;
      q->env = 0;
      q->modified = 0;
      for (p = &soap->cookies, n = soap->cookie_max; *p && n; p = &(*p)->next, n--)
        if (!strcmp((*p)->name, name) && (*p)->path && strcmp((*p)->path, path) < 0)
          break;
      if (n)
      { q->next = *p;
        *p = q;
      }
      else
      { free(q->name);
        free(q);
	q = NULL;
      }
    }
  }
  else
    q->modified = 1;
  if (q)
  { if (q->value)
      free(q->value);
    if (q->domain)
      free(q->domain);
    if (q->path)
      free(q->path);
    if (value && (q->value = (char*)malloc(strlen(value)+1)))
      strcpy(q->value, value);
    if ((q->domain = (char*)malloc(strlen(domain)+1)))
      strcpy(q->domain, domain);
    if ((q->path = (char*)malloc(strlen(path)+1)))
      strcpy(q->path, path);
    q->session = 1;
  }
  return q;
}

/******************************************************************************/
SOAP_FMAC1
void
SOAP_FMAC2
soap_clr_cookie(struct soap *soap, const char *name, const char *domain, const char *path)
{ struct soap_cookie **p, *q;
  if (!domain)
    domain = soap->cookie_domain;
  if (!path)
    path = soap->cookie_path;
  if (*path == '/')
   path++;
  for (p = &soap->cookies, q = *p; q; p = &q->next, q = *p)
    if (!strcmp(q->name, name) && !strcmp(q->domain, domain) && !strncmp(q->path, path, strlen(q->path)))
    { if (q->value)
        free(q->value);
      if (q->domain)
        free(q->domain);
      if (q->path)
        free(q->path);
      *p = q->next;
      free(q);
    }
}

/******************************************************************************/
SOAP_FMAC1
char *
SOAP_FMAC2
soap_cookie_value(struct soap *soap, const char *name, const char *domain, const char *path)
{ struct soap_cookie *p;
  if ((p = soap_cookie(soap, name, domain, path)))
    return p->value;
  return NULL;
}

/******************************************************************************/
SOAP_FMAC1
long
SOAP_FMAC2
soap_cookie_expire(struct soap *soap, const char *name, const char *domain, const char *path)
{ struct soap_cookie *p;
  if ((p = soap_cookie(soap, name, domain, path)))
    return p->expire;
  return -1;
}

/******************************************************************************/
SOAP_FMAC1
int
SOAP_FMAC2
soap_set_cookie_expire(struct soap *soap, const char *name, long expire, const char *domain, const char *path)
{ struct soap_cookie *p;
  if ((p = soap_cookie(soap, name, domain, path)))
  { p->expire = expire;
    p->modified = 1;
    return SOAP_OK;
  }
  return SOAP_EOF;
}

/******************************************************************************/
SOAP_FMAC1
int
SOAP_FMAC2
soap_set_cookie_session(struct soap *soap, const char *name, const char *domain, const char *path)
{ struct soap_cookie *p;
  if ((p = soap_cookie(soap, name, domain, path)))
  { p->session = 1;
    p->modified = 1;
    return SOAP_OK;
  }
  return SOAP_EOF;
}

/******************************************************************************/
SOAP_FMAC1
int
SOAP_FMAC2
soap_clr_cookie_session(struct soap *soap, const char *name, const char *domain, const char *path)
{ struct soap_cookie *p;
  if ((p = soap_cookie(soap, name, domain, path)))
  { p->session = 0;
    p->modified = 1;
    return SOAP_OK;
  }
  return SOAP_EOF;
}

/******************************************************************************/
SOAP_FMAC1
int
SOAP_FMAC2
soap_putsetcookies(struct soap *soap)
{ struct soap_cookie *p;
  for (p = soap->cookies; p; p = p->next)
    if (p->modified || !p->env)
    { if (soap_send(soap, "Set-Cookie: ") || soap_send(soap, p->name))
        return soap->error;
      if (p->value)
      { if (soap_send(soap, "=\"") || soap_send(soap, p->value) || soap_send(soap, "\""))
          return soap->error;
      }
      if (p->domain)
      { if (soap_send(soap, ";Domain=\"") || soap_send(soap, p->domain) || soap_send(soap, "\""))
          return soap->error;
      }
      else if (soap->cookie_domain)
      { if (soap_send(soap, ";Domain=\"") || soap_send(soap, soap->cookie_domain) || soap_send(soap, "\""))
          return soap->error;
      }
      if (p->path)
      { if (soap_send(soap, ";Path=\"/") || soap_send(soap, p->path) || soap_send(soap, "\""))
          return soap->error;
      }
      else if (soap->cookie_path)
      { if (soap_send(soap, ";Path=\"") || soap_send(soap, soap->cookie_path) || soap_send(soap, "\""))
          return soap->error;
      }
      else if (soap_send(soap, ";Path=\"/\""))
        return soap->error;
      if (p->version > 0)
      { sprintf(soap->tagbuf, ";Version=\"%u\"", p->version);
        if (soap_send(soap, soap->tagbuf))
	  return soap->error;
      }
      if (p->expire >= 0)
      { sprintf(soap->tagbuf, ";Max-Age=\"%ld\"", p->expire);
        if (soap_send(soap, soap->tagbuf))
	  return soap->error;
      }
      if (p->secure)
        if (soap_send(soap, ";Secure"))
	  return soap->error;
      if (soap_send(soap, "\r\n"))
        return soap->error;
    }
  return SOAP_OK;
}

/******************************************************************************/
SOAP_FMAC1
int
SOAP_FMAC2
soap_putcookies(struct soap *soap, const char *domain, const char *path, int secure)
{ struct soap_cookie **p, *q;
  unsigned int version = 0;
  time_t now = time(NULL);
  p = &soap->cookies;
  while ((q = *p))
  { if (q->expire && now > q->expire)
    { if (q->value)
        free(q->value);
      if (q->domain)
        free(q->domain);
      if (q->path)
        free(q->path);
      *p = q->next;
      free(q);
    }
    else if ((!q->domain || !strcmp(q->domain, domain))
          && (!q->path || !strncmp(q->path, path, strlen(q->path)))
          && (!q->secure || secure))
    { if (soap_send(soap, "Cookie: "))
        return soap->error;
      if (q->version != version)
      { sprintf(soap->tagbuf, "$Version=\"%u\";", q->version);
        if (soap_send(soap, soap->tagbuf))
	  return soap->error;
        version = q->version;
      }
      if (soap_send(soap, q->name))
        return soap->error;
      if (q->value)
      { if (soap_send(soap, "=\"") || soap_send(soap, q->value) || soap_send(soap, "\""))
          return soap->error;
      }
      if (q->path)
      { if (soap_send(soap, ";$Path=\"/") || soap_send(soap, q->path) || soap_send(soap, "\""))
          return soap->error;
      }
      if (q->domain)
      { if (soap_send(soap, ";$Domain=\"") || soap_send(soap, q->domain) || soap_send(soap, "\""))
          return soap->error;
      }
      if (soap_send(soap, "\r\n"))
        return soap->error;
      p = &q->next;
    }
    else
      p = &q->next;
  }
  return SOAP_OK;
}

/******************************************************************************/
SOAP_FMAC1
char *
SOAP_FMAC2
soap_getcookievalue(struct soap *soap, char *buf, int len, wchar stop)
{ char *s = NULL, *t;
  if (stop == ';' || stop == ',' || stop == '\n' || stop == EOF)
    return NULL;
  switch (soap_getlineto(soap, buf, len, "'\";,"))
  { case '\'':
      soap_getlineto(soap, buf, len, "'");
      if ((s = (char*)malloc(strlen(buf)+1)))
        strcpy(s, buf);
      soap_getlineto(soap, buf, len, ";,");
      break;
    case '"':
      soap_getlineto(soap, buf, len, "\"");
      if ((s = (char*)malloc(strlen(buf)+1)))
        strcpy(s, buf);
      soap_getlineto(soap, buf, len, ";,");
      break;
    case ';':
    case ',':
      if (stop != (wchar)'=')
      { for (s = buf; *s; s++)
	  if (*s == '=')
	  { s++;
	    break;
          }
      }
      else
        s = buf;
      for (; *s; s++)
        if (notblank(*s))
          break;
      for (t = s; *t; t++)
        if (*t == ';' || *t == ',' || blank(*t))
          break;
      *t = '\0';
  }
  return s;
}

/******************************************************************************/
SOAP_FMAC1
void
SOAP_FMAC2
soap_getcookies(struct soap *soap)
{ struct soap_cookie *p = NULL, *q;
  char *s, buf[4096]; /* cookie size is up to 4096 bytes [RFC2109] */
  wchar c;
  char *domain = NULL;
  char *path = NULL;
  unsigned int version = 0;
  time_t now = time(NULL);
  for (;;)
  { c = soap_getlineto(soap, buf, 4096, "\t =;,"); 
    if (c == '\n' || c == EOF)
      break;
    if (!soap_tag_cmp(buf, "$Version"))
    { if ((s = soap_getcookievalue(soap, buf, 4096, c)))
        (p?p->version:version) = atoi(s);
    }
    else if (!soap_tag_cmp(buf, "$Path"))
    { if (p)
        p->path = soap_getcookievalue(soap, buf, 4096, c);
      else
      { if (path)
          free(path);
        path = soap_getcookievalue(soap, buf, 4096, c);
      }
    }
    else if (!soap_tag_cmp(buf, "$Domain"))
    { if (p)
        p->domain = soap_getcookievalue(soap, buf, 4096, c);
      else
      { if (domain)
          free(domain);
        domain = soap_getcookievalue(soap, buf, 4096, c);
      }
    }
    else if (p && !soap_tag_cmp(buf, "Path"))
      p->path = soap_getcookievalue(soap, buf, 4096, c);
    else if (p && !soap_tag_cmp(buf, "Domain"))
      p->domain = soap_getcookievalue(soap, buf, 4096, c);
    else if (p && !soap_tag_cmp(buf, "Version"))
    { if ((s = soap_getcookievalue(soap, buf, 4096, c)))
        p->version = (unsigned int)atoi(s);
    }
    else if (p && !soap_tag_cmp(buf, "Max-Age"))
    { if ((s = soap_getcookievalue(soap, buf, 4096, c)))
        p->expire = now + atol(s);
    }
    else if (p && !soap_tag_cmp(buf, "Secure"))
      p->secure = 1;
    else
    { if (p)
      { if ((q = soap_set_cookie(soap, p->name, p->value, p->domain, p->path)))
        { q->version = p->version;
	  q->expire = p->expire;
	  q->secure = p->secure;
	  q->env = 1;
        }
        if (p->name)
	  free(p->name);
        if (p->value)
	  free(p->value);
        if (p->domain)
	  free(p->domain);
        if (p->path)
	  free(p->path);
        free(p);
      }
      if ((p = (struct soap_cookie*)malloc(sizeof(struct soap_cookie))));
      { p->name = (char*)malloc(strlen(buf)+1);
        strcpy(p->name, buf);
        p->value = soap_getcookievalue(soap, buf, 4096, c);
	p->expire = 0;
	p->secure = 0;
        p->version = version;
      }
    }
  }
  soap_getline(soap, buf, 4096);
  if (p)
  { if ((q = soap_set_cookie(soap, p->name, p->value, p->domain, p->path)))
    { q->version = p->version;
      q->expire = p->expire;
      q->secure = p->secure;
    }
    if (p->name)
      free(p->name);
    if (p->value)
      free(p->value);
    if (p->domain)
      free(p->domain);
    if (p->path)
      free(p->path);
    free(p);
  }
  if (domain)
    free(domain);
  if (path)
    free(path);
}

/******************************************************************************/
static const char *
soap_decode_string(const char *s, char *t, int len)
{ while (*s && *s != '=' && *s != ';' && --len)
    switch (*s)
    { case '+':
        *t++ = ' ';
      case ' ':
	s++;
        break;
      case '%':
        *t++ = ((s[1] >= 'A' ? (s[1]&0x7) + 9 : s[1] - '0') << 4) + (s[2] >= 'A' ? (s[2]&0x7) + 9 : s[2] - '0');
        s += 3;
        break;
      default:
        *t++ = *s++;
    }
  *t = '\0';
  return s;
}

/******************************************************************************/
SOAP_FMAC1
int
SOAP_FMAC2
soap_getenv_cookies(struct soap *soap)
{ struct soap_cookie *p;
  const char *s;
  char buf[4096]; /* cookie size is up to 4096 bytes [RFC2109] */
  if (!(s = getenv("HTTP_COOKIE")))
    return SOAP_EOF;
  do
  { s = soap_decode_string(s, buf, 4096);
    p = soap_set_cookie(soap, buf, NULL, NULL, NULL);
    if (*s == '=')
    { s = soap_decode_string(s+1, buf, 4096);
      if (p && *buf)
      { p->value = (char*)malloc(strlen(buf)+1);
        strcpy(p->value, buf);
      }
    }
    if (p)
      p->env = 1;
    if (*s == ';')
      s++;
  } while (*s);
  return SOAP_OK;
}

/******************************************************************************/
SOAP_FMAC1
void
SOAP_FMAC2
soap_free_cookies(struct soap *soap)
{ struct soap_cookie *p;
  for (p = soap->cookies; p; p = soap->cookies)
  { soap->cookies = p->next;
    free(p->name);
    if (p->value)
      free(p->value);
    if (p->domain)
      free(p->domain);
    if (p->path)
      free(p->path);
    free(p);
  }
}

/******************************************************************************/
#endif

/******************************************************************************/
SOAP_FMAC1
int
SOAP_FMAC2
soap_begin_recv(struct soap *soap)
{ wchar c;
  soap->buflen = 0;
  soap->bufidx = 0;
  soap->chunked = 0;
  soap->cdata = 0;
  soap->ahead1 = 0;
  soap->ahead2 = 0;
  soap->count = 0;
  soap->dime = soap->dime_transfer;
  soap->dime_chunksize = 0;
  soap->dime_buflen = 0;
  do c = soap_get(soap);
  while (blank(c) && (c&0xFFFC) != (SOAP_DIME_VERSION|SOAP_DIME_MB));
  if (c == EOF)
    return soap->error = SOAP_EOF;
  soap_unget(soap, c);
  if (c != LT)
  { if ((c&0xFFFC) == (SOAP_DIME_VERSION|SOAP_DIME_MB))
      soap->dime = 1;
    else if ((soap->error = soap->fparse(soap)))
      return soap->error;
  }
  if (soap->dime)
  { if (soap_getdimehdr(soap))
      return soap->error;
    if (soap->dime_flags&SOAP_DIME_CF)
    { DBGLOG(TEST, SOAP_MESSAGE(fdebug,"\nChunked DIME SOAP message"));
      soap->dime_chunksize = soap->dime_size;
      if (soap->buflen - soap->bufidx >= soap->dime_chunksize)
      { soap->dime_buflen = soap->buflen;
        soap->buflen = soap->bufidx + soap->dime_chunksize;
      }
      else
        soap->dime_chunksize -= soap->buflen - soap->bufidx;
    }
    soap->count = soap->buflen - soap->bufidx;
  }
  return SOAP_OK;
}

/******************************************************************************/
static int
http_parse(struct soap *soap)
{ char s[SOAP_TAGLEN], status[SOAP_TAGLEN], *t;
  int connection_keep_alive = 0, connection_close = 0;
  DBGLOG(TEST, SOAP_MESSAGE(fdebug,"\nWaiting for response..."));
  soap->dime = 0;
  do
  { if (soap_getline(soap, status, SOAP_TAGLEN) != SOAP_OK)
      return soap->error = SOAP_EOF;
    DBGLOG(TEST,SOAP_MESSAGE(fdebug,"\n**** HTTP status: %s", status));
    for (;;)
    { if (soap_getlineto(soap, s, SOAP_TAGLEN, ":") == EOF)
        return soap->error = SOAP_EOF;
      DBGLOG(TEST,SOAP_MESSAGE(fdebug,"\n**** HTTP header: %s", s));
      if (!soap_tag_cmp(s, "Content-Type"))
      { soap_getline(soap, s, SOAP_TAGLEN);
        soap->dime = !soap_tag_cmp(s, "*application/dime*");
      }
      else if (!soap_tag_cmp(s, "Transfer-Encoding"))
      { soap_getline(soap, s, SOAP_TAGLEN);
        soap->chunked = !soap_tag_cmp(s, "chunked*");
        DBGLOG(TEST,SOAP_MESSAGE(fdebug,"\n**** using chunked encoding ****"));
      }
      else if (!soap_tag_cmp(s, "Connection"))
      { soap_getline(soap, s, SOAP_TAGLEN);
        if (!soap_tag_cmp(s, "Keep-Alive*"))
        { connection_keep_alive = 1;
          DBGLOG(TEST,SOAP_MESSAGE(fdebug,"\n**** Keep-Alive ****"));
        }
        else if (!soap_tag_cmp(s, "close*"))
        { connection_close = 1;
          DBGLOG(TEST,SOAP_MESSAGE(fdebug,"\n**** close ****"));
        }
      }
      else if (!soap_tag_cmp(s, "SOAPAction"))
      { soap_getline(soap, s, SOAP_TAGLEN);
        if (soap->action)
          free(soap->action);
        if (*s && (soap->action = (char*)malloc(strlen(s)-1)))
        { s[strlen(s)-1] = '\0';
          strcpy(soap->action, s+1);
        }
        DBGLOG(TEST,SOAP_MESSAGE(fdebug,"\n**** SOAPaction:%s ****", s));
      }
      else if (!soap_tag_cmp(s, "Server"))
      { soap_getline(soap, s, SOAP_TAGLEN);
        if (!soap_tag_cmp(s, "Microsoft-IIS*"))
	  soap->dot_net_bug = 1;
      }
      else if (!soap_tag_cmp(s, "User-Agent"))
      { soap_getline(soap, s, SOAP_TAGLEN);
        if (!soap_tag_cmp(s, "*.NET CLR*") || !soap_tag_cmp(s, "*MS Web Services Client Protocol*"))
	  soap->dot_net_bug = 1;
      }
#ifdef WITH_COOKIES
      else if (!soap_tag_cmp(s, "Cookie"))
      { DBGLOG(TEST,SOAP_MESSAGE(fdebug,"\n**** cookie ****"));
        soap_getcookies(soap);
      }
      else if (!soap_tag_cmp(s, "Set-Cookie"))
      { DBGLOG(TEST,SOAP_MESSAGE(fdebug,"\n**** set-cookie ****"));
        soap_getcookies(soap);
      }
#endif
      else if (*s)
        soap_getline(soap, s, SOAP_TAGLEN);
      else
      { soap_get2(soap); /* skip \n at the end of the HTTP header */
	break;
      }
    }
    t = strchr(status, ' ');
  } while (t && !strncmp(t, " 100", 4));
  if (soap->chunked)
  { soap->chunkbuflen = soap->buflen;
    soap->buflen = soap->bufidx;
    soap->chunksize = 0;
  }
  if (connection_close || (!connection_keep_alive && (status[7] != '1' || soap->http_version[2] != '1')))
    soap->keep_alive = 0;
  DBGLOG(TEST,SOAP_MESSAGE(fdebug,"\nfinished HTTP header parsing"));
  if (!t || !strncmp(status, "POST ", 5) || !strncmp(t, " 200", 4) || !strncmp(t, " 500", 4))
    return SOAP_OK;
  t = (char*)soap_malloc(soap, strlen(status)+1);
  strcpy(t, status);
  soap_set_error(soap, "SOAP-ENV:Server", "HTTP Error", t, SOAP_HTTP_ERROR);
  return soap->error = SOAP_HTTP_ERROR; 
}

/******************************************************************************/
SOAP_FMAC1
int
SOAP_FMAC2
soap_envelope_begin_out(struct soap *soap)
{ if (soap_send(soap, "<?xml version=\"1.0\" encoding=\"UTF-8\"?><SOAP-ENV:Envelope"))
    return soap->error;
  return soap_send_namespaces(soap);
}

/******************************************************************************/
SOAP_FMAC1
int
SOAP_FMAC2
soap_send_namespaces(struct soap *soap)
{ struct Namespace *ns;
  if (!soap->ns && (ns = soap->namespaces))
  { for (; ns->id; ns++)
    { *soap->tagbuf = '\0';
      if (*ns->id)
      { if (ns->out)
	  sprintf(soap->tagbuf," xmlns:%s=\"%s\"", ns->id, ns->out);
        else if (ns->ns)
	  sprintf(soap->tagbuf," xmlns:%s=\"%s\"", ns->id, ns->ns);
      }
      else if (!soap->defaultNamespace && ns->ns)
        sprintf(soap->tagbuf," xmlns=\"%s\"", ns->ns);
      if (soap_send(soap, soap->tagbuf))
        return soap->error;
    }   
    if (soap->defaultNamespace)
    { sprintf(soap->tagbuf, " xmlns=\"%s\"", soap->defaultNamespace);
      if (soap_send(soap, soap->tagbuf))
        return soap->error;
    }
    if (soap->encodingStyle)
    { sprintf(soap->tagbuf, " SOAP-ENV:encodingStyle=\"%s\"", soap->encodingStyle);
      if (soap_send(soap, soap->tagbuf))
        return soap->error;
    }
    soap->ns = 1;
  }
  return soap_send(soap, ">");
}

/******************************************************************************/
SOAP_FMAC1
int
SOAP_FMAC2
soap_envelope_end_out(struct soap *soap)
{ return soap_send(soap, "</SOAP-ENV:Envelope>"); 
} 

/******************************************************************************/
SOAP_FMAC1
int
SOAP_FMAC2
soap_envelope_begin_in(struct soap *soap)
{ if (soap_element_begin_in(soap, "SOAP-ENV:Envelope"))
    return soap->error = SOAP_VERSIONMISMATCH;
  return SOAP_OK;
}

/******************************************************************************/
SOAP_FMAC1
int
SOAP_FMAC2
soap_envelope_end_in(struct soap *soap)
{ return soap_element_end_in(soap, "SOAP-ENV:Envelope");
}

/******************************************************************************/
SOAP_FMAC1
int
SOAP_FMAC2
soap_body_begin_out(struct soap *soap)
{ return soap_send(soap, "<SOAP-ENV:Body>");
}

/******************************************************************************/
SOAP_FMAC1
int
SOAP_FMAC2
soap_body_end_out(struct soap *soap)
{ return soap_send(soap, "</SOAP-ENV:Body>");
}

/******************************************************************************/
SOAP_FMAC1
int
SOAP_FMAC2
soap_body_begin_in(struct soap *soap)
{ return soap_element_begin_in(soap, "SOAP-ENV:Body");
}

/******************************************************************************/
SOAP_FMAC1
int
SOAP_FMAC2
soap_body_end_in(struct soap *soap)
{ return soap_element_end_in(soap, "SOAP-ENV:Body");
}

/******************************************************************************/
SOAP_FMAC1
int
SOAP_FMAC2
soap_recv_header(struct soap *soap)
{ if (soap_getheader(soap) && soap->error == SOAP_TAG_MISMATCH)
    soap->error = SOAP_OK;
  return soap->error;
}

/******************************************************************************/
SOAP_FMAC1
int
SOAP_FMAC2
soap_connect(struct soap *soap, const char *endpoint, const char *action)
{ char host[SOAP_TAGLEN];
  const char *s;
  int j, i, n;
  int port_no = 80;
  size_t count;
  if (!endpoint || !*endpoint)
    return SOAP_OK;
#ifdef WITH_OPENSSL
  if (!strncmp(endpoint, "https:", 6))
    port_no = 443;
#endif
  strcpy(soap->endpoint, endpoint);
  strcpy(soap->path, "");
  s = strchr(endpoint, ':');
  if (s && s[1] == '/' && s[2] == '/')
    s += 3;
  else
    s = endpoint;
  n = strlen(s);
  if (n >= SOAP_TAGLEN)
    n = SOAP_TAGLEN;
  for (i = 0; i < n; i++)
  { host[i] = s[i];
    if (s[i] == '/' || s[i] == ':')
      break; 
  }
  host[i] = '\0';
  if (s[i] == ':')
  { port_no = atoi(s + i + 1);
    for (i++; i < n; i++)
      if (s[i] == '/')
        break;
  }
  i++;
  for (j = i; j < n; j++)
    soap->path[j-i] = s[j];
  soap->path[j-i]='\0';
  if (*host)
  { soap_closesock(soap);
    if (soap->socket < 0 || strcmp(soap->host, host) || soap->port != port_no)
    { DBGLOG(TEST,SOAP_MESSAGE(fdebug,"\nConnect to %s %s %d",host,soap->path,port_no));
      if (soap->fopen(soap, endpoint, host, port_no) < 0)
        return soap->error;
    }
  }
  strcpy(soap->host, host);
  soap->port = port_no;
  count = soap->count;
  if (soap->dime)
  { char tmp[SOAP_TAGLEN];
    sprintf(tmp, soap->dime_id_format, 0);
    soap->dime_id = tmp;
    if (soap->namespaces)
      soap->dime_type = (char*)soap->namespaces[0].ns;
    soap->dime_options = NULL;
    count = soap->dime_size + 12 + ((soap->count+3)&-4) + ((strlen(soap->dime_id)+3)&-4) + ((strlen(soap->dime_type)+3)&-4);
    soap->dime_size = soap->count;
    if (soap->dime_count)
      soap->dime_flags = SOAP_DIME_MB|SOAP_DIME_ABSURI;
    else
      soap->dime_flags = SOAP_DIME_MB|SOAP_DIME_ME|SOAP_DIME_ABSURI;
  }
  if ((soap->error = soap->fpost(soap, endpoint, soap->host, soap->path, action, count)))
    return soap->error;
  if (soap->chunked_transfer)
  { if (soap_flush(soap))
      return soap->error;
    soap->chunked = 1;
  }
  if (soap->dime)
    return soap_putdimehdr(soap);
  return SOAP_OK;
}

/******************************************************************************/
static int
soap_puthttphdr(struct soap *soap, size_t count)
{ if (soap->dime)
  { if (soap_send(soap, "Content-Type: application/dime; charset=us-ascii\r\n"))
      return soap->error;
  }
  else if (soap_send(soap, "Content-Type: text/xml; charset=utf-8\r\n"))
    return soap->error;
  if (soap->buffering && soap->chunked_transfer)
  { if (soap_send(soap, "Transfer-Encoding: chunked\r\n"))
      return soap->error;
  }
  else if (count > 0)
  { sprintf(soap->tagbuf, "Content-Length: %u\r\n", (unsigned int)count);
    if (soap_send(soap, soap->tagbuf))
      return soap->error;
  }
  if (soap->keep_alive)
    if (soap_send(soap, "Connection: Keep-Alive\r\n"))
      return soap->error;
  return SOAP_OK;
}

/******************************************************************************/
static int
http_post(struct soap *soap, const char *endpoint, const char *host, const char *path, const char *action, size_t count)
{ if (strncmp(endpoint, "http:", 5) && strncmp(endpoint, "https:", 6))
    return SOAP_OK;
#ifdef WITH_OPENSSL
  if (soap->ssl && soap->proxy_host)
    sprintf(soap->tagbuf, "CONNECT %s:%d HTTP/%s\r\nHost: %s\r\n", soap->proxy_host, soap->proxy_port, soap->http_version, host);
  else
#endif
  if (soap->proxy_host)
    sprintf(soap->tagbuf, "POST %s HTTP/%s\r\nHost: %s\r\n", endpoint, soap->http_version, host);
  else
    sprintf(soap->tagbuf, "POST /%s HTTP/%s\r\nHost: %s\r\n", path, soap->http_version, host);
  if (soap_send(soap, soap->tagbuf))
    return soap->error;
  if (soap_send(soap, "User-Agent: gSOAP/2.1\r\n"))
    return soap->error;
  if (soap_puthttphdr(soap, count))
    return soap->error;
#ifdef WITH_COOKIES
#ifdef WITH_OPENSSL
  if (soap_putcookies(soap, host, path, soap_ssl != NULL))
    return soap->error;
#else
  if (soap_putcookies(soap, host, path, 0))
    return soap->error;
#endif
#endif
  if (!action)
    return soap_send(soap, "SOAPAction: \"\"\r\n\r\n");
  sprintf(soap->tagbuf,"SOAPAction: \"%s\"\r\n\r\n", action);
  return soap_send(soap, soap->tagbuf);
}

/******************************************************************************/
static int
http_response(struct soap *soap, int soap_error, size_t count)
{ if (!soap_error)
  { DBGLOG(TEST, SOAP_MESSAGE(fdebug,"\nOK 200"));
    if (soap->master >= 0 || soap->socket >= 0)		/* standalone application */
    { sprintf(soap->tagbuf, "HTTP/%s 200 OK\r\n", soap->http_version);
      if (soap_send(soap, soap->tagbuf))
        return soap->error;
    }
    else if (soap_send(soap, "Status: 200 OK\r\n"))
      return soap->error;
  }
  else
  { DBGLOG(TEST, SOAP_MESSAGE(fdebug,"\nError 500"));
    if (soap->master >= 0 || soap->socket >= 0)		/* standalone application */
    { sprintf(soap->tagbuf, "HTTP/%s 500 Internal Server Error\r\n", soap->http_version);
      if (soap_send(soap, soap->tagbuf))
        return soap->error;
    }
    else if (soap_send(soap, "Status: 500 Internal Server Error\r\n"))
      return soap->error;
  }
  if (soap_send(soap, "Server: gSOAP/2.1\r\n"))
    return soap->error;
  if (soap_puthttphdr(soap, count))
    return soap->error;
#ifdef WITH_COOKIES
  if (soap_putsetcookies(soap))
    return soap->error;
#endif
  return soap_send(soap, "\r\n");
}

/******************************************************************************/
SOAP_FMAC1
int
SOAP_FMAC2
soap_response(struct soap *soap, int soap_error)
{ size_t count;
  count = soap->count;
  if (soap->dime)
  { char tmp[SOAP_TAGLEN];
    sprintf(tmp, soap->dime_id_format, 0);
    soap->dime_id = tmp;
    if (soap->namespaces)
      soap->dime_type = (char*)soap->namespaces[0].ns;
    soap->dime_options = NULL;
    count = soap->dime_size + 12 + ((soap->count+3)&-4) + ((strlen(soap->dime_id)+3)&-4) + ((strlen(soap->dime_type)+3)&-4);
    soap->dime_size = soap->count;
    if (soap->dime_count)
      soap->dime_flags = SOAP_DIME_MB|SOAP_DIME_ABSURI;
    else
      soap->dime_flags = SOAP_DIME_MB|SOAP_DIME_ME|SOAP_DIME_ABSURI;
  }
  if ((soap->error = soap->fresponse(soap, soap_error, count)))
    return soap->error;
  if (soap->chunked_transfer)
  { if (soap_flush(soap))
      return soap->error;
    soap->chunked = 1;
  }
  if (soap->dime)
    return soap_putdimehdr(soap);
  return SOAP_OK;
}

/******************************************************************************/
SOAP_FMAC1
void
SOAP_FMAC2
soap_set_fault(struct soap *soap)
{ if (!*soap_faultcode(soap))
    *soap_faultcode(soap) = "SOAP-ENV:Client";
  if (!*soap_faultdetail(soap))
    *soap_faultdetail(soap) = soap->endpoint;
  switch (soap->error)
  { case SOAP_CLI_FAULT:
      *soap_faultstring(soap) = "CLI";
      break;
    case SOAP_SVR_FAULT:
      *soap_faultcode(soap) = "SOAP-ENV:Server";
      *soap_faultstring(soap) = "SVR";
      break;
    case SOAP_TAG_MISMATCH:
      sprintf(soap->msgbuf, "Tag mismatch: element '%s' does not correspond to expected element", soap->tag);
      *soap_faultstring(soap) = soap->msgbuf;
      break;
    case SOAP_TYPE_MISMATCH:
      sprintf(soap->msgbuf, "Data type '%s' mismatch in element '%s'", soap->type, soap->tag);
      *soap_faultstring(soap) = soap->msgbuf;
      break;
    case SOAP_SYNTAX_ERROR:
      *soap_faultstring(soap) = "SOAP/XML syntax error";
      break;
    case SOAP_NO_TAG:
      *soap_faultstring(soap) = "No XML tag found";
      break;
/*	case SOAP_UNKNOWN_TAG:
	  *soap_faultstring() = "Unknown XML tag";
	  break; */
    case SOAP_MUSTUNDERSTAND:
      *soap_faultcode(soap) = "SOAP-ENV:MustUnderstand";
      sprintf(soap->msgbuf, "The data in element '%s' must be understood but cannot be handled", soap->tag);
      *soap_faultstring(soap) = soap->msgbuf;
      break;
    case SOAP_VERSIONMISMATCH:
      *soap_faultcode(soap) = "SOAP-ENV:VersionMismatch";
      *soap_faultstring(soap) = "SOAP version mismatch or invalid SOAP message";
      break;
    case SOAP_DIME_VERSIONMISMATCH:
      *soap_faultstring(soap) = "DIME version mismatch";
      break;
    case SOAP_NAMESPACE:
      *soap_faultstring(soap) = "Namespace mismatch";
      break;
    case SOAP_OBJ_MISMATCH:
      *soap_faultstring(soap) = "Object mismatch";
      break;
    case SOAP_FATAL_ERROR:
      *soap_faultcode(soap) = "SOAP-ENV:Server";
      *soap_faultstring(soap) = "Fatal error";
      break;
    case SOAP_NO_METHOD:
      sprintf(soap->msgbuf, "Method '%s' not implemented", soap->tag);
      *soap_faultstring(soap) = soap->msgbuf;
      break;
    case SOAP_EOM:
      *soap_faultcode(soap) = "SOAP-ENV:Server";
      *soap_faultstring(soap) = "Out of memory";
      break;
    case SOAP_IOB:
      *soap_faultstring(soap) = "Array index out of bounds";
      break;
    case SOAP_NULL:
      *soap_faultcode(soap) = "SOAP-ENV:Server";
      sprintf(soap->msgbuf, "Cannot create nilable object for type '%s' in element '%s'", soap->type, soap->tag);
      *soap_faultstring(soap) = soap->msgbuf;
      *soap_faultdetail(soap) = "The object is not nilable because the XML schema type for this element is not nilable";
      break;
    case SOAP_MULTI_ID:
      *soap_faultstring(soap) = "Non-unique id attribute";
      break;
    case SOAP_MISSING_ID:
      *soap_faultstring(soap) = "Missing id: referenced data is missing or had to be ignored";
      break;
    case SOAP_HREF:
      *soap_faultstring(soap) = "Invalid XML: object reference with href attribute is incompatible with actual object referred to";
      break;
    case SOAP_FAULT:
      break;
    case SOAP_TCP_ERROR:
      *soap_faultstring(soap) = tcp_error(soap);
      break;
    case SOAP_HTTP_ERROR:
      *soap_faultstring(soap) = "HTTP Server Error";
      *soap_faultdetail(soap) = "The server responded with an HTTP error code";
      break;
    case SOAP_SSL_ERROR:
      if (!*soap_faultstring(soap))
        *soap_faultstring(soap) = "SSL error";
      break;
    case SOAP_DIME_ERROR:
      *soap_faultstring(soap) = "DIME error";
      break;
    case SOAP_EOD:
      *soap_faultstring(soap) = "End of DIME error";
      break;
    case SOAP_EOF:
      *soap_faultcode(soap) = "SOAP-ENV:Server";
      *soap_faultstring(soap) = "End of file or no input";
      if (soap->errnum)
        *soap_faultdetail(soap) = soap_strerror(soap, soap->errnum);
      else if (soap_errno)
        *soap_faultdetail(soap) = soap_strerror(soap, soap_errno);
      break;
    default:
      *soap_faultcode(soap) = "SOAP-ENV:Server";
      *soap_faultstring(soap) = "Unknown error code";
    }
}

/******************************************************************************/
SOAP_FMAC1
int
SOAP_FMAC2
soap_send_fault(struct soap *soap)
{ int soap_error = soap->error;
  DBGLOG(TEST,SOAP_MESSAGE(fdebug,"\nSending back fault struct for error code %d", soap->error));
  soap_set_fault(soap);
  soap->error = SOAP_OK;
  soap_begin(soap);
  soap_serializeheader(soap);
  soap_serializefault(soap);
  if (!soap->disable_response_count)
  { soap_begin_count(soap);
    soap_envelope_begin_out(soap);
    soap_putheader(soap);
    soap_body_begin_out(soap);
    soap_putfault(soap);
    soap_body_end_out(soap);
    soap_envelope_end_out(soap);
  }
  soap_begin_send(soap);
  if (soap_response(soap, soap_error) || soap_envelope_begin_out(soap))
    return soap->error;
  soap_putheader(soap);
  if (soap_body_begin_out(soap))
    return soap->error;
  soap_putfault(soap);
  if (soap_body_end_out(soap) || soap_envelope_end_out(soap))
    return soap->error;
  if (soap->dime)
    if (soap_send_raw(soap, "\0\0\0", -(int)soap->count&3))
      return soap->error;
  soap_end_send(soap);
  soap_closesock(soap);
  return soap->error = soap_error;
}

/******************************************************************************/
SOAP_FMAC1
int
SOAP_FMAC2
soap_recv_fault(struct soap *soap)
{ int soap_error;
  DBGLOG(TEST,SOAP_MESSAGE(fdebug,"\nsoap_recv_fault"));
  soap_error = soap->error;
  if (soap_getfault(soap))
  { DBGLOG(TEST,SOAP_MESSAGE(fdebug,"\nERROR soap_get_soapfault() failed"));
     soap->error = soap_error;
     *soap_faultcode(soap) = "SOAP-ENV:Client";
     soap_set_fault(soap);
     return soap->error;
  }
  /* Use the soap fault received to set soap->error to the appropriate value */
  if (soap_match_tag(soap, *soap_faultcode(soap), "SOAP-ENV:Server") == 0)
    soap_error = SOAP_SVR_FAULT; 
  else if (soap_match_tag(soap, *soap_faultcode(soap), "SOAP-ENV:Client") == 0)
    soap_error = SOAP_CLI_FAULT;
  else if (soap_match_tag(soap, *soap_faultcode(soap), "SOAP-ENV:MustUnderstand") == 0)
    soap_error = SOAP_MUSTUNDERSTAND;
  else if (soap_match_tag(soap, *soap_faultcode(soap), "SOAP-ENV:VersionMismatch") == 0)
    soap_error = SOAP_VERSIONMISMATCH;
  else
  { DBGLOG(TEST,SOAP_MESSAGE(fdebug,"\nfault code %s", *soap_faultcode(soap) ));
    soap_error = SOAP_FAULT;
  }
  DBGLOG(TEST,SOAP_MESSAGE(fdebug,"\nSOAP FAULT: soap_recv_fault()"));
  soap_closesock(soap); 
  soap_end_recv(soap); 
  return soap->error = soap_error;
}

/******************************************************************************/
SOAP_FMAC1
void
SOAP_FMAC2
soap_print_fault(struct soap *soap, FILE *fd)
{ if (soap->error)
  { if (!*soap_faultcode(soap))
      soap_set_fault(soap);
    if (!*soap_faultstring(soap))
      *soap_faultstring(soap) = "";
    fprintf(fd, "SOAP FAULT: %s\n\"%s\"\n", *soap_faultcode(soap), *soap_faultstring(soap));
    if (*soap_faultdetail(soap))
      fprintf(fd, "Detail: %s\n", *soap_faultdetail(soap));
  }
}
 
/******************************************************************************/
SOAP_FMAC1
void
SOAP_FMAC2
soap_print_fault_location(struct soap *soap, FILE *fd)
{ int c;
  if (soap->error && soap->buflen > 0)
  { if (soap->bufidx == 0)
      soap->bufidx = 1;
    c = soap->buf[soap->bufidx-1];
    soap->buf[soap->bufidx-1] = '\0';
    soap->buf[soap->buflen-1] = '\0';
    if (soap->bufidx < soap->buflen)
      fprintf(fd, "%s%c\n** HERE **\n%s\n", soap->buf, c, soap->buf+soap->bufidx);
    else
      fprintf(fd, "%s%c\n** HERE **\n", soap->buf, c);
  }
}
 
/******************************************************************************/
SOAP_FMAC1
int
SOAP_FMAC2
soap_size(const int *size, int dim)
{ int i, n = size[0];
  for (i = 1; i < dim; i++)
    n *= size[i];
  return n;
}

/******************************************************************************/
SOAP_FMAC1
int
SOAP_FMAC2
soap_getoffsets(const char *attr, const int *size, int *offset, int dim)
{ int i, j = 0;
  if (offset)
    for (i = 0; i < dim && attr && *attr; i++)
    { attr++;
      j *= size[i];
      j += offset[i] = atoi(attr);
      attr = strchr(attr, ',');
    }
  else
    for (i = 0; i < dim && attr && *attr; i++)
    { attr++;
      j *= size[i];
      j += atoi(attr);
      attr = strchr(attr, ',');
    }
  return j;
}

/******************************************************************************/
SOAP_FMAC1
char *
SOAP_FMAC2
soap_putsize(struct soap *soap, const char *type, int size)
{ if (!type)
    return NULL;
  sprintf(soap->tmpbuf1, "%s[%d]", type, size);
  return soap->tmpbuf1;
}

/******************************************************************************/
SOAP_FMAC1
char *
SOAP_FMAC2
soap_putsizesoffsets(struct soap *soap, const char *type, const int *size, const int *offset, int dim)
{ int i;
  if (!type)
    return NULL;
  sprintf(soap->tmpbuf1, "%s[%d", type, size[0]+offset[0]);
  for (i = 1; i < dim; i++)
    sprintf(soap->tmpbuf1+strlen(soap->tmpbuf1), ",%d", size[i]+offset[i]);
  strcat(soap->tmpbuf1, "]");
  return soap->tmpbuf1;
}

/******************************************************************************/
SOAP_FMAC1
char *
SOAP_FMAC2
soap_putsizes(struct soap *soap, const char *type, const int *size, int dim)
{ int i;
  if (!type)
    return NULL;
  sprintf(soap->tmpbuf1, "%s[%d", type, size[0]);
  for (i = 1; i < dim; i++)
    sprintf(soap->tmpbuf1+strlen(soap->tmpbuf1), ",%d", size[i]);
  strcat(soap->tmpbuf1, "]");
  return soap->tmpbuf1;
}
/******************************************************************************/
SOAP_FMAC1
char *
SOAP_FMAC2
soap_putoffset(struct soap *soap, int offset)
{ sprintf(soap->tmpbuf2, "[%d]", offset);
  return soap->tmpbuf2;
}

/******************************************************************************/
SOAP_FMAC1
char *
SOAP_FMAC2
soap_putoffsets(struct soap *soap, const int *offset, int dim)
{ int i;
  sprintf(soap->tmpbuf2, "[%d", offset[0]);
  for (i = 1; i < dim; i++)
    sprintf(soap->tmpbuf2+strlen(soap->tmpbuf2), ",%d", offset[i]);
  strcat(soap->tmpbuf2, "]");
  return soap->tmpbuf2;
}

/******************************************************************************/
SOAP_FMAC1
int
SOAP_FMAC2
soap_getsize(const char *attr1, const char *attr2, int *j)
{ int n, k;
  char *s;
  *j = 0;
  if (!*attr1)
    return -1;
  n = 1;
  do
  { attr1++;
    k = (int)strtol(attr1, &s, 10);
    if (s == attr1)
      return -1;
    attr1 = s;
    n *= k;
    attr1 = strchr(attr1, ',');
    if (attr2 && *attr2)
    { attr2++;
      *j *= k;
      *j += (int)strtol(attr2, &s, 10);
      attr2 = s;
    }
  } while (attr1 && *attr1 != ']');
  return n - *j;
}

/******************************************************************************/
SOAP_FMAC1
int
SOAP_FMAC2
soap_getsizes(const char *attr, int *size, int dim)
{ int i, n;
  if (!*attr)
    return -1;
  for (i = strlen(attr); i > 0; i--)
    if (attr[i] == ']')
      break;
  n = 1;
  do
  { for (i = i-1; i >= 0; i--)
      if (attr[i] == '[' || attr[i] == ',')
        break;
    if (i >= 0)
      n *= size[--dim] = atoi(attr+i+1);
  } while (i >= 0 && attr[i] != '[');
  if (i >= 0)
    return n;
  else
    return 0;
}

/******************************************************************************/
SOAP_FMAC1
int
SOAP_FMAC2
soap_getposition(const char *attr, int *pos)
{ int i, n;
  if (!*attr)
    return -1;
  n = 0;
  i = 1;
  do
  { pos[n++] = atoi(attr+i);
    while (attr[i] && attr[i] != ',' && attr[i] != ']')
      i++;
    if (attr[i] == ',')
      i++;
  } while (n < 32 && attr[i] && attr[i] != ']');
  return n;
}

/******************************************************************************/
SOAP_FMAC1
char *
SOAP_FMAC2
soap_putposition(struct soap *soap)
{ int i;
  sprintf(soap->tmpbuf3, "[%d", soap->positions[0]);
  for (i = 1; i < soap->position; i++)
    sprintf(soap->tmpbuf3+strlen(soap->tmpbuf3), ",%d", soap->positions[i]);
  strcat(soap->tmpbuf3, "]");
  return soap->tmpbuf3;
}

#ifdef __cplusplus
}
#endif

