/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2016, CWI Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

#define MAIN	     // we want build_sin()
#define SOCK_ERRORS  // we will use verbose socket errors
#define PL_ARITY_AS_SIZE

#include <iostream>
#include <math.h>
#include <assert.h>
#include <mutex>
#include <vector>
#include <sisocks.h>
#include <Rconnection.h>
#include <SWI-Stream.h>
#include <SWI-cpp.h>

		 /*******************************
		 *	       SYMBOL		*
		 *******************************/

#define R_DESTROYED	0x0001		/* Was destroyed by user  */
#define R_OPEN_ONCE	0x0002		/* Reuse alias */

typedef struct Rref
{ Rconnection   *rc;			/* Connection handle */
  atom_t         symbol;		/* associated symbol */
  atom_t	 name;			/* alias name */
  int	         flags;			/* flags */
} Rref;


		 /*******************************
		 *	      ALIAS		*
		 *******************************/

#define NULL_ATOM (atom_t)0

typedef struct alias_cell
{ atom_t	name;
  atom_t	symbol;
  struct alias_cell *next;
} alias_cell;

#define ALIAS_HASH_SIZE 64

std::mutex alias_lock;
static unsigned int alias_size = ALIAS_HASH_SIZE;
static alias_cell *alias_entries[ALIAS_HASH_SIZE];

static unsigned int
atom_hash(atom_t a)
{ return (unsigned int)(a>>7) % alias_size;
}

static atom_t
get_alias(atom_t name)
{ for(alias_cell *c = alias_entries[atom_hash(name)];
      c;
      c = c->next)
  { if ( c->name == name )
      return c->symbol;
  }

  return NULL_ATOM;
}

static void
alias(atom_t name, atom_t symbol)
{ unsigned int key = atom_hash(name);

  alias_lock.lock();
  if ( !get_alias(name) )
  { alias_cell *c = (alias_cell *)malloc(sizeof(*c));

    c->name   = name;
    c->symbol = symbol;
    c->next   = alias_entries[key];
    alias_entries[key] = c;
    PL_register_atom(c->name);
    PL_register_atom(c->symbol);
    alias_lock.unlock();
  } else
  { alias_lock.unlock();
    throw PlPermissionError("alias", "rserve", PlTerm(name));
  }
}

static void
unalias(atom_t name)
{ unsigned int key = atom_hash(name);
  alias_cell *c, *prev=NULL;

  alias_lock.lock();
  for(c = alias_entries[key]; c; prev=c, c = c->next)
  { if ( c->name == name )
    { if ( prev )
	prev->next = c->next;
      else
	alias_entries[key] = c->next;
      PL_unregister_atom(c->name);
      PL_unregister_atom(c->symbol);
      free(c);

      break;
    }
  }
  alias_lock.unlock();
}


		 /*******************************
		 *	 SYMBOL REFERENCES	*
		 *******************************/

static int
write_R_ref(IOSTREAM *s, atom_t symbol, int flags)
{ Rref **refp = (Rref **)PL_blob_data(symbol, NULL, NULL);
  Rref *ref = *refp;
  (void)flags;

  Sfprintf(s, "<Rserve>(%p)", ref);
  return TRUE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
GC an rserve connection from the atom garbage collector.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
release_R_ref(atom_t symbol)
{ Rref **refp = (Rref **)PL_blob_data(symbol, NULL, NULL);
  Rref *ref   = *refp;
  Rconnection *rc;

  assert(ref->name == NULL_ATOM);

  if ( (rc=ref->rc) )
  { ref->rc = NULL;
    delete rc;
  }
  PL_free(ref);

  return TRUE;
}


static int
save_R_ref(atom_t symbol, IOSTREAM *fd)
{ Rref **refp = (Rref **)PL_blob_data(symbol, NULL, NULL);
  Rref *ref   = *refp;
  (void)fd;

  return PL_warning("Cannot save reference to <Rserve>(%p)", ref);
}


static atom_t
load_R_ref(IOSTREAM *fd)
{ (void)fd;

  return PL_new_atom("<saved-Rserve-ref>");
}


static PL_blob_t R_blob =
{ PL_BLOB_MAGIC,
  PL_BLOB_UNIQUE,
  (char*)"rserve",
  release_R_ref,
  NULL,
  write_R_ref,
  NULL,
  save_R_ref,
  load_R_ref
};


static int
unify_R_ref(term_t t, Rref *ref)
{ if ( ref->name )
  { if ( !ref->symbol )
    { PlTerm tmp;

      if ( PL_unify_blob(tmp, &ref, sizeof(ref), &R_blob) &&
	   PL_get_atom(tmp, &ref->symbol) )
      { alias(ref->name, ref->symbol);
      } else
      { assert(0);
      }
    }
    return PL_unify_atom(t, ref->name);
  } else if ( ref->symbol )
  { return PL_unify_atom(t, ref->symbol);
  } else
  { return ( PL_unify_blob(t, &ref, sizeof(ref), &R_blob) &&
	     PL_get_atom(t, &ref->symbol)
	   );
  }
}


static Rref*
symbol_Rref(atom_t symbol)
{ void *data;
  size_t len;
  PL_blob_t *type;

  if ( (data=PL_blob_data(symbol, &len, &type)) && type == &R_blob )
  { Rref **erd = (Rref **)data;
    return *erd;
  }

  return (Rref*)NULL;
}


static int
get_Rref(term_t t, Rref **erp, int warn=TRUE)
{ atom_t a = NULL_ATOM;

  if ( PL_get_atom(t, &a) )
  { for(int i=0; a && i<2; i++)
    { Rref *ref;

      if ( (ref=symbol_Rref(a)) )
      { if ( !(ref->flags & R_DESTROYED) )
	{ *erp = ref;
	  return TRUE;
	} else if ( warn )
	{ throw PlExistenceError("Rserve", t);
	}
      }

      if ( !(a = get_alias(a)) )
      { PlTerm r;
	PlTermv av(PlTerm(t), r);

	if ( !PlCall("rserve", "r_open_hook", av) ||
	     !PL_get_atom(r, &a) )
	  break;
      }
    }

    throw PlExistenceError("Rserve", t);
  }

  if ( warn )
    throw PlTypeError("Rserve", t);

  return FALSE;
}

		 /*******************************
		 *	      UTIL		*
		 *******************************/

static const char *
sisocks_msg(int rc)
{ static char msg[128];				/* TBD: thread safety */

  sockerrorchecks(msg, sizeof(msg), -1);

  return msg;
}

class SISocksError : public PlException
{
public:
  SISocksError(int rc) :
    PlException(PlCompound("error",
			   PlTermv(PlCompound("sisocks_error",
					      PlTermv((long)rc,
						      sisocks_msg(rc))),
				   PlTerm())))
  {
  }
};


static void
sisocks_ok(int status)
{ if ( status )
    throw SISocksError(status);
}


class RError : public PlException
{
public:
  RError(int status) :
    PlException(PlCompound("error",
			   PlTermv(PlCompound("r_error",
					      PlTermv(PlTerm((long)status))),
				   PlTerm())))
  {
  }
};


static void
rok(int status)
{ if ( status )
    throw RError(status);
}


/* get_string() gets a C++ string from a Prolog term.  As R does not handle
 * strings with nul-bytes, we need to check for that.
 */

static void
get_string(const PlTerm &t, std::string &str)
{ char *s;
  size_t len;

  if ( PL_get_nchars(t, &len, &s,
		     CVT_ATOM|CVT_STRING|CVT_NUMBER|CVT_EXCEPTION|REP_UTF8) )
  { if ( strlen(s) == len )
    { str.assign(s, len);
      return;
    }
    throw PlDomainError("nul_terminated_string", t);
  }

  PlException().cppThrow();
}


typedef enum dtype
{ D_UNKNOWN = 0,
  D_BOOLEAN,
  D_INTEGER,
  D_DOUBLE,
  D_STRING
} dtype;


static const PlAtom ATOM_false("false");
static const PlAtom ATOM_true("true");

class PlRExp
{
public:
  Rexp *exp = (Rexp*)NULL;
  dtype type = D_UNKNOWN;
  std::vector<int> iv;
  std::vector<double> dv;
  std::string sv;

  PlRExp()
  {
  }

  ~PlRExp()
  { if ( exp )
      delete exp;
  }

  void
  promote(dtype t)
  { if ( t > type )
    { switch(t)
      { case D_BOOLEAN:
	  sv.reserve(16);
	  sv.append(sizeof(int), 0);
	  break;
	case D_INTEGER:
	  iv.reserve(16);
	  break;
        case D_DOUBLE:
	  dv.reserve(16);
	  if ( type == D_INTEGER )
	  { for(size_t i=0; i<iv.size(); i++)
	      dv.push_back((double)iv[i]);
	    iv.resize(0);
	  }
	  break;
	case D_STRING:
	  sv.reserve(16);
					/* FIXME: Promote integers and doubles */
	  break;
      }
      type = t;
    }
  }

  void append(const PlTerm &t)
  { switch(type)
    { case D_UNKNOWN:
	switch(PL_term_type(t))
	{ case PL_VARIABLE:
	    throw PlInstantiationError();
	  case PL_INTEGER:
	    promote(D_INTEGER);
	    goto case_i;
	  case PL_FLOAT:
	    promote(D_DOUBLE);
	    goto case_d;
	  case PL_ATOM:
	  { atom_t a;

	    if ( PL_get_atom(t, &a) &&
		 (ATOM_true == a || ATOM_false == a) )
	    { promote(D_BOOLEAN);
	      goto case_b;
	    }
	  }
	  /*FALLTHROUGH*/
	  case PL_STRING:
	    promote(D_STRING);
	    goto case_s;
	  break;
	}
      case D_BOOLEAN:
      case_b:
      { int i;

	if ( PL_get_bool_ex(t, &i) )
	{ sv.push_back(i != 0);
	  break;
	}
	PlException().cppThrow();	/* FIXME: Promote */
      }
      case D_INTEGER:
      case_i:
      { int i;
	double d;

	if ( PL_get_integer(t, &i) )
	{ iv.push_back(i);
	} else if ( PL_get_float(t, &d) )
	{ promote(D_DOUBLE);
	  dv.push_back(d);
	} else
	  throw PlTypeError("numeric", t);
        break;
      }
      case D_DOUBLE:
      case_d:
	dv.push_back(t);
        break;
      case D_STRING:
      case_s:
      { std::string s;
	get_string(t, s);
	sv += s;
	sv.push_back(0);
	break;
      }
    }
  }

  void finish(const PlTerm &t)
  { switch(type)
    { case D_UNKNOWN:
	throw PlTypeError("R-expression", t);
      case D_BOOLEAN:
      { unsigned int *lenptr = (unsigned int*)sv.data();
	*lenptr = (unsigned int)(sv.size()-sizeof(unsigned int));
	exp = new Rboolean((unsigned char*)sv.data(), sv.size());
	break;
      }
      case D_INTEGER:
	exp = new Rinteger(iv.data(), iv.size());
	break;
      case D_DOUBLE:
	exp = new Rdouble(dv.data(), dv.size());
	break;
      case D_STRING:
      { sv.push_back(1);		/* s1\000s2\000...sn\000\001 */
	exp = new Rstrings(sv);
	break;
      }
    }
  }
};

static void
list_to_rexp(const PlTerm &t, PlRExp *exp)
{ PlTail list(t);
  PlTerm head;

  for(size_t i=0; list.next(head); i++)
    exp->append(head);

  exp->finish(t);
}


static PlRExp *
term_to_rexp(const PlTerm &t)
{ PlRExp *exp = new PlRExp;

  switch(PL_term_type(t))
  { case PL_LIST_PAIR:
    { list_to_rexp(t, exp);
      break;
    }
    case PL_INTEGER:
    { int i = t;
      exp->exp = new Rinteger(&i, 1);
      break;
    }
    case PL_FLOAT:
    { double f = t;
      exp->exp = new Rdouble(&f, 1);
      break;
    }
    case PL_ATOM:
    { atom_t a;

      if ( PL_get_atom(t, &a) &&
	   (ATOM_true == a || ATOM_false == a) )
      { struct { unsigned int len; unsigned char data[1]; } b;
	b.len = 1;
        b.data[0] = (ATOM_true == a);
	exp->exp = new Rboolean((unsigned char*)&b,
				sizeof(unsigned int)+sizeof(unsigned char));
	break;
      }
    }
    /*FALLTHROUGH*/
    case PL_STRING:
    { std::string s;

      get_string(t, s);
      s.push_back(0);
      s.push_back(1);
      exp->exp = new Rstrings(s);
      break;
    }
    case PL_TERM:
    { const char *nm = t.name();
      if ( nm && strcmp(nm, "c") == 0 )
      { ARITY_T len = t.arity();

	for(ARITY_T i=1; i<= len; i++)
	  exp->append(t[i]);
	exp->finish(t);
	break;
      } else if ( nm && strcmp(nm, "+") == 0 && t.arity() == 1)
      { std::string s;

	get_string(t[1], s);
	s.push_back(0);
	s.push_back(1);
	exp->exp = new Rstrings(s);
	break;
      }
    }
    /*FALLTHROUGH*/
    default:
      delete(exp);
      throw PlTypeError("R-expression", t);
  }

  return exp;
}


static const PlAtom ATOM_null("null");
static const PlAtom ATOM_clos("<XT_CLOS>");
static const PlAtom ATOM_unknown("<XT_UNKNOWN>");

static int
unify_exp(const PlTerm &t, const Rexp *exp)
{ switch(exp->type)
  { case XT_NULL:
      return PL_unify_atom(t, ATOM_null.handle);
    case XT_ARRAY_INT:
    { Rinteger *ri = (Rinteger*)exp;
      Rsize_t len = ri->length();
      PlTail tail(t);
      PlTerm h;

      for(Rsize_t i=0; i<len; i++)
      { if ( !PL_put_integer(h, ri->intAt(i)) ||
	     !tail.append(h) )
	  return FALSE;
      }
      return tail.close();
    }
    case XT_ARRAY_BOOL:
    { Rboolean *rs = (Rboolean*)exp;
      Rsize_t len = rs->length();
      PlTail tail(t);
      PlTerm h;

      for(Rsize_t i=0; i<len; i++)
      { if ( !PL_put_bool(h, rs->boolAt(i)) ||
	     !tail.append(h) )
	  return FALSE;
      }
      return tail.close();
    }
    case XT_ARRAY_DOUBLE:
    { Rdouble *rd = (Rdouble*)exp;
      Rsize_t len = rd->length();
      PlTail tail(t);
      PlTerm h;
      int allints = TRUE;

      for(Rsize_t i=0; i<len; i++)
      { double f = rd->doubleAt(i);
	if ( nearbyint(f) != f )
	{ allints = FALSE;
	  break;
	}
      }

      if ( allints )
      { for(Rsize_t i=0; i<len; i++)
	{ if ( !PL_put_int64(h, (int64_t)rd->doubleAt(i)) ||
	       !tail.append(h) )
	    return FALSE;
	}
      } else
      { for(Rsize_t i=0; i<len; i++)
	{ if ( !PL_put_float(h, rd->doubleAt(i)) ||
	       !tail.append(h) )
	    return FALSE;
	}
      }
      return tail.close();
    }
    case XT_ARRAY_STR:
    { Rstrings *rs = (Rstrings*)exp;
      Rsize_t len = rs->length();
      PlTail tail(t);
      PlTerm h;

      for(Rsize_t i=0; i<len; i++)
      { if ( !PL_put_variable(h) ||
	     !PL_unify_chars(h, PL_STRING|REP_UTF8, -1, rs->stringAt(i)) ||
	     !tail.append(h) )
	  return FALSE;
      }
      return tail.close();
    }
    case XT_VECTOR:
    { const Rexp *e;
      PlTail tail(t);
      PlTerm h;

      for(int i=0; e=(const Rexp*)((Rvector*)exp)->elementAt(i); i++)
      { PL_put_variable(h);
	if ( !unify_exp(h, e) ||
	     !tail.append(h) )
	  return FALSE;
      }
      return tail.close();
    }
    case XT_STR:
    { Rstring *rs = (Rstring*)exp;
      return PL_unify_chars(t, PL_STRING|REP_UTF8, -1, rs->string());
    }
    case XT_SYMNAME:
    { Rstring *rs = (Rstring*)exp;
      return PL_unify_chars(t, PL_ATOM|REP_UTF8, -1, rs->string());
    }
    case XT_CLOS:
    { return PL_unify_atom(t, ATOM_clos.handle);
    }
    case XT_UNKNOWN:
    { return PL_unify_atom(t, ATOM_unknown.handle);
    }
    default:
      Sdprintf("Rexp of type %d\n", exp->type);
      return FALSE;
  }
}


		 /*******************************
		 *	    PREDICATES		*
		 *******************************/

static const PlAtom ATOM_alias("alias");
static const PlAtom ATOM_open("open");
static const PlAtom ATOM_once("once");
static const PlAtom ATOM_host("host");
static const PlAtom ATOM_port("port");

PREDICATE(r_open, 2)
{ Rref *ref;
  atom_t alias = NULL_ATOM;
  int once = FALSE;
  const char *host = "127.0.0.1";
  int port = default_Rsrv_port;

  PlTail tail(A2);
  PlTerm opt;
  while(tail.next(opt))
  { atom_t name;
    size_t arity;

    if ( PL_get_name_arity(opt, &name, &arity) && arity == 1 )
    { if ( ATOM_host == name )
      { host = opt[1];
      } else if	( ATOM_port == name )
      { port = opt[1];
      } else if ( ATOM_alias == name )
      { if ( !PL_get_atom_ex(opt[1], &alias) )
	  return FALSE;
	once = TRUE;
      } else if ( ATOM_open == name )
      { atom_t open;

	if ( !PL_get_atom_ex(opt[1], &open) )
	  return FALSE;
	if ( ATOM_once == open )
	  once = TRUE;
	else
	  throw PlDomainError("open_option", opt[1]);
      }
    }
  }

  if ( alias && once )
  { atom_t existing;

    if ( (existing=get_alias(alias)) )
    { Rref *eref;

      if ( (eref=symbol_Rref(existing)) &&
	   (eref->flags&R_OPEN_ONCE) )
	return PL_unify_atom(A1, existing);
    }
  }

  ref = (Rref *)PL_malloc(sizeof(*ref));
  memset(ref, 0, sizeof(*ref));

  ref->rc = new Rconnection(host, port);
  sisocks_ok(ref->rc->connect());
  ref->name = alias;
  if ( once )
    ref->flags |= R_OPEN_ONCE;

  return unify_R_ref(A1, ref);
}


PREDICATE(r_close, 1)
{ Rref *ref;

  get_Rref(A1, &ref);
  Rconnection *rc = ref->rc;

  ref->rc = NULL;
  ref->flags |= R_DESTROYED;
  if ( ref->name )
  { unalias(ref->name);
    ref->name = NULL_ATOM;
  }

  delete rc;
  return TRUE;
}


PREDICATE(r_assign_, 3)
{ Rref *ref;
  size_t len;
  const char *vname = A2;

  get_Rref(A1, &ref);
  PlRExp *exp = term_to_rexp(A3);
  try
  { ref->rc->assign(vname, exp->exp);
  } catch(...)
  { delete(exp);
    throw;
  }
  delete(exp);
  return TRUE;
}


PREDICATE(r_eval, 2)
{ Rref *ref;
  const char *command = A2;
  int rc;
  int status = 0;

  get_Rref(A1, &ref);
  ref->rc->eval(command, &status, 1);
  if ( status == 0 )
    return TRUE;
  throw RError(status);
}


PREDICATE(r_eval, 3)
{ Rref *ref;
  const char *command = A2;
  int rc;
  int status = 0;

  get_Rref(A1, &ref);
  Rexp *result = ref->rc->eval(command, &status);
  if ( result )
  { try
    { rc = unify_exp(A3, result);
    } catch(...)
    { delete(result);
      throw;
    }
    delete result;
    return rc;
  }

  throw RError(status);
}


PREDICATE(r_read_file, 3)
{ Rref *ref;
  const char *filename = A2;
  char buf[4096];
  std::string data;
  int rc;

  get_Rref(A1, &ref);
  rok(ref->rc->openFile(filename));
  data.reserve(sizeof(buf));
  while((rc=ref->rc->readFile(buf, sizeof(buf))) > 0)
  { data.append(buf, (size_t)rc);
  }
  rok(ref->rc->closeFile());

  return PL_unify_chars(A3, PL_STRING, data.size(), data.data());
}


PREDICATE(r_remove_file, 2)
{ Rref *ref;
  const char *filename = A2;
  int rc;

  get_Rref(A1, &ref);
  rok(ref->rc->removeFile(filename));

  return TRUE;
}


PREDICATE(r_login, 3)
{ Rref *ref;
  const char *user = A2;
  const char *password = A3;

  get_Rref(A1, &ref);
  rok(ref->rc->login(user, password));

  return TRUE;
}


static int
hex_byte(int val)
{ if ( val < 10 )
    return '0'+val;
  else
    return 'a'+val-10;
}

static void
tohex(char *to, const char *from, int len)
{ for(int i=0; i<len; i++)
  { *to++ = hex_byte((from[i]>>4)&0xf);
    *to++ = hex_byte(from[i]&0xf);
  }
  *to = '\0';
}


static int
unhexb(int c)
{ if ( c >= '0' && c <= '9' )
    return c-'0';
  if ( c >= 'a' && c <= 'f' )
    return c-'a'+10;
  if ( c >= 'A' && c <= 'F' )
    return c-'A'+10;
  return -1;
}

static int
unhex(char *to, const char *from, int size)
{ for(; *from; from += 2)
  { if ( --size < 0 || !from[1] )
      return -1;

    int d1 = unhexb(from[0]);
    int d2 = unhexb(from[1]);
    if ( d1 < 0 || d2 < 0 )
      return -1;

    *to++ = (d1<<4) + d2;
  }

  return 0;
}


PREDICATE(r_detach_, 2)
{ Rref *ref;
  int status = 0;

  get_Rref(A1, &ref);
  Rsession *session = ref->rc->detach(&status);

  if ( session )
  { char hkey[65];
    int rc;

    tohex(hkey, session->key(), 32);
    rc = (A2 = PlCompound("r_session", PlTermv(session->host(),
					       (long)session->port(),
					       hkey)));
    delete session;
    return rc;
  }

  throw RError(status);
}


static const PlFunctor FUNCTOR_r_session3("r_session", 3);

PREDICATE(r_resume, 3)
{ if ( PL_is_functor(A2, FUNCTOR_r_session3.functor) )
  { char key[32];
    char *host = A2[1];
    int   port = (int)A2[2];
    char *hkey = A2[3];
    Rref *ref;
    atom_t alias = NULL_ATOM;

    if ( !PL_is_variable(A3) &&
	 !PL_get_atom_ex(A3, &alias) )
      return FALSE;

    if ( unhex(key, hkey, sizeof(key)) < 0 )
      throw PlDomainError("r_session_key", A2[3]);
    Rsession session(host, port, key);

    ref = (Rref *)PL_malloc(sizeof(*ref));
    memset(ref, 0, sizeof(*ref));

    ref->rc = new Rconnection(&session);
    sisocks_ok(ref->rc->connect());
    ref->name = alias;

    return unify_R_ref(A1, ref);
  }

  throw PlTypeError("r_session", A2);
}


#ifdef CMD_ctrl
PREDICATE(r_server_eval, 2)
{ Rref *ref;
  const char *command = A2;

  get_Rref(A1, &ref);
  rok(ref->rc->serverEval(command));

  return TRUE;
}

PREDICATE(r_server_source, 2)
{ Rref *ref;
  const char *filename = A2;

  get_Rref(A1, &ref);
  rok(ref->rc->serverEval(filename));

  return TRUE;
}

PREDICATE(r_server_shutdown, 1)
{ Rref *ref;

  get_Rref(A1, &ref);
  rok(ref->rc->serverShutdown());

  return TRUE;
}
#endif
