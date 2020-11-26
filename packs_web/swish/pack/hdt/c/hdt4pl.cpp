/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2017, VU University Amsterdam
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

#define PL_ARITY_AS_SIZE 1
#include <SWI-Stream.h>
#include <SWI-cpp.h>
#include <iostream>
#include <HDTManager.hpp>
#include <assert.h>

using namespace std;
using namespace hdt;

static void	deleteHDT(HDT *hdt);
static int	get_triple_role(term_t t, TripleComponentRole *role);

#define CATCH_HDT \
	catch (char *e)				\
	{ return hdt_error(e);			\
	} catch (const char *e)			\
	{ return hdt_error(e);			\
	} catch (std::exception& e)		\
	{ return hdt_error(e.what());		\
	}

extern "C" {

#define URL_xsd		  "http://www.w3.org/2001/XMLSchema#"
#define URL_xsdString     URL_xsd "string"
#define URL_xsdDouble     URL_xsd "double"

static atom_t ATOM_mapping;
static atom_t ATOM_max_id;
static atom_t ATOM_max_object_id;
static atom_t ATOM_max_predicate_id;
static atom_t ATOM_max_subject_id;
static atom_t ATOM_objects;
static atom_t ATOM_predicates;
static atom_t ATOM_shared;
static atom_t ATOM_subjects;
static atom_t ATOM_elements;
static atom_t ATOM_subject;
static atom_t ATOM_predicate;
static atom_t ATOM_object;
static atom_t ATOM_access;
static atom_t ATOM_indexed;
static atom_t ATOM_map;
static atom_t ATOM_load;
static atom_t ATOM_header;
static atom_t ATOM_content;
static atom_t ATOM_base_uri;

static functor_t FUNCTOR_rdftype2;
static functor_t FUNCTOR_rdflang2;

typedef struct hdt_wrapper
{ atom_t	symbol;			/* Associated symbol */
  HDT	       *hdt;
} hdt_wrapper;


static void
acquire_hdt(atom_t symbol)
{ hdt_wrapper *symb = (hdt_wrapper*)PL_blob_data(symbol, NULL, NULL);
  symb->symbol = symbol;
}


static int
release_hdt(atom_t symbol)
{ hdt_wrapper *symb = (hdt_wrapper*)PL_blob_data(symbol, NULL, NULL);

  if ( symb->hdt )
  { deleteHDT(symb->hdt);
    symb->hdt = NULL;
  }
  PL_free(symb);

  return TRUE;
}

static int
compare_hdts(atom_t a, atom_t b)
{ hdt_wrapper *ara = (hdt_wrapper*)PL_blob_data(a, NULL, NULL);
  hdt_wrapper *arb = (hdt_wrapper*)PL_blob_data(b, NULL, NULL);

  return ( ara > arb ?  1 :
	   ara < arb ? -1 : 0
	 );
}


static int
write_hdt(IOSTREAM *s, atom_t symbol, int flags)
{ hdt_wrapper *symb = (hdt_wrapper*)PL_blob_data(symbol, NULL, NULL);

  Sfprintf(s, "<hdt>(%p)", symb);

  return TRUE;
}

static PL_blob_t hdt_blob =
{ PL_BLOB_MAGIC,
  PL_BLOB_NOCOPY,
  (char*)"hdt",
  release_hdt,
  compare_hdts,
  write_hdt,
  acquire_hdt
};


static int
get_hdt(term_t t, hdt_wrapper **symb_ptr)
{ PL_blob_t *type;
  void *data;

  if ( PL_get_blob(t, &data, NULL, &type) && type == &hdt_blob)
  { hdt_wrapper *symb = (hdt_wrapper*)data;

    if ( !symb->hdt )
      return PL_existence_error("hdt", t);
    *symb_ptr = symb;

    return TRUE;
  }

  return PL_type_error("hdt", t);
}


#define MKATOM(a) ATOM_ ## a = PL_new_atom(#a)

install_t
install_hdt4pl(void)
{ MKATOM(mapping);
  MKATOM(max_id);
  MKATOM(max_object_id);
  MKATOM(max_predicate_id);
  MKATOM(max_subject_id);
  MKATOM(objects);
  MKATOM(predicates);
  MKATOM(shared);
  MKATOM(subjects);
  MKATOM(elements);
  MKATOM(subject);
  MKATOM(predicate);
  MKATOM(object);
  MKATOM(access);
  MKATOM(indexed);
  MKATOM(map);
  MKATOM(load);
  MKATOM(content);
  MKATOM(header);
  MKATOM(base_uri);

  FUNCTOR_rdftype2 = PL_new_functor(PL_new_atom("^^"), 2);
  FUNCTOR_rdflang2 = PL_new_functor(PL_new_atom("@"), 2);
}

}/* end extern "C" */

static void
deleteHDT(HDT *hdt)
{ delete hdt;
}

static int
hdt_error(const char *e)
{ PlCompound f("hdt_error", PlTermv(e));

  return PL_raise_exception(PlCompound("error", PlTermv(f, PlTerm())));
}


		 /*******************************
		 *	     PREDICATES		*
		 *******************************/


PREDICATE(hdt_open, 3)
{ HDT *hdt;
  atom_t access = ATOM_map;
  int indexed = TRUE;
  PlTail options(A3);
  PlTerm opt;
  char *name;

  while(options.next(opt))
  { atom_t name;
    size_t arity;

    if ( PL_get_name_arity(opt, &name, &arity) && arity == 1 )
    { PlTerm ov = opt[1];

      if ( name == ATOM_access )
      { if ( !PL_get_atom_ex(ov, &access) )
	  return FALSE;
      } else if ( name == ATOM_indexed )
      { if ( !PL_get_bool_ex(ov, &indexed) )
	  return FALSE;
      }
    } else
      return PL_type_error("option", opt);
  }

  if ( !PL_get_file_name(A2, &name, PL_FILE_EXIST) )
    return FALSE;

  try
  { if ( access == ATOM_map )
    { if ( indexed )
	hdt = HDTManager::mapIndexedHDT(name);
      else
	hdt = HDTManager::mapHDT(A2);
    } else if ( access == ATOM_load )
    { if ( indexed )
	hdt = HDTManager::loadIndexedHDT(name);
      else
	hdt = HDTManager::loadHDT(A2);
    } else
    { PlTerm ex;

      PL_put_atom(ex, access);
      return PL_domain_error("hdt_access", ex);
    }
  } CATCH_HDT;

  hdt_wrapper *symb = (hdt_wrapper*)PL_malloc(sizeof(*symb));
  memset(symb, 0, sizeof(*symb));
  symb->hdt = hdt;

  return PL_unify_blob(A1, symb, sizeof(*symb), &hdt_blob);
}


PREDICATE(hdt_close, 1)
{ hdt_wrapper *symb;

  if ( !get_hdt(A1, &symb) )
    return FALSE;

  deleteHDT(symb->hdt);				/* FIXME: Thread safety */
  symb->hdt = NULL;

  return TRUE;
}


#define S_S 0x01
#define S_P 0x02
#define S_O 0x04

typedef struct
{ unsigned flags;
  IteratorTripleString *it;
} search_it;


static int
get_search_string(term_t t, char **s, unsigned flag, unsigned *flagp)
{ if ( PL_is_variable(t) )
  { *s = (char*)"";
    *flagp |= flag;
    return TRUE;
  } else
  { size_t len;

    return PL_get_nchars(t, &len, s, CVT_ATOM|CVT_STRING|CVT_EXCEPTION|REP_UTF8);
  }
}

static int
unify_string(term_t t, const char *s)
{ return PL_unify_chars(t, PL_ATOM|REP_UTF8, (size_t)-1, s);
}


static int
unify_object(term_t t, const char *s)
{ if ( s[0] == '"' )
  { const char *e = s+strlen(s)-1;

    for(;; e--)
    { while( e>s && *e != '"' )
	e--;
      if ( e > s )
      { if ( e[1] == '\0' )		/* No type nor lang??  In header ... */
	{ term_t av = PL_new_term_refs(2);
	  int rc;

	  s++;
	  rc = PL_unify_chars(t, PL_STRING|REP_UTF8, e-s, s);
	  return rc;
	} else if ( strncmp(e+1, "^^<", 3) == 0 )
	{ term_t av = PL_new_term_refs(2);
	  int rc;

	  s++;
	  rc = PL_unify_chars(av+0, PL_STRING|REP_UTF8, e-s, s);
	  e += 4;
	  rc = rc && PL_unify_chars(av+1, PL_ATOM|REP_UTF8, strlen(e)-1, e);
	  rc = rc && PL_cons_functor_v(av, FUNCTOR_rdftype2, av);
	  rc = rc && PL_unify(t, av);
	  return rc;
	} else if ( strncmp(e+1, "@", 1) == 0 )
	{ term_t av = PL_new_term_refs(2);
	  int rc;

	  s++;
	  rc = PL_unify_chars(av+0, PL_STRING|REP_UTF8, e-s, s);
	  e += 2;
	  rc = rc && PL_unify_chars(av+1, PL_ATOM|REP_UTF8, (size_t)-1, e);
	  rc = rc && PL_cons_functor_v(av, FUNCTOR_rdflang2, av);
	  rc = rc && PL_unify(t, av);
	  return rc;
	}
      } else
      { assert(0);
	return FALSE;
      }
    }
  }

  return PL_unify_chars(t, PL_ATOM|REP_UTF8, (size_t)-1, s);
}


/** hdt_search(+HDT, +Where, ?S, ?P, ?O)
*/

PREDICATE_NONDET(hdt_search, 5)
{ hdt_wrapper *symb;
  search_it ctx_buf = {0};
  search_it *ctx;
  int rc;

  switch(PL_foreign_control(handle))
  { case PL_FIRST_CALL:
    { char *s, *p, *o;
      atom_t where;

      ctx = &ctx_buf;
      if ( !get_hdt(A1, &symb) )
	return FALSE;
      if ( !PL_get_atom_ex(A2, &where) ||
	   !get_search_string(A3, &s, S_S, &ctx->flags) ||
	   !get_search_string(A4, &p, S_P, &ctx->flags) ||
	   !get_search_string(A5, &o, S_O, &ctx->flags) )
	return FALSE;

      try
      { if ( where == ATOM_content )
	  ctx->it = symb->hdt->search(s,p,o);
	else if ( where == ATOM_header )
	  ctx->it = symb->hdt->getHeader()->search(s,p,o);
	else
	  return PL_domain_error("hdt_where", A2);
      } CATCH_HDT;

      goto next;
    }
    case PL_REDO:
      ctx = (search_it*)PL_foreign_context_address(handle);
    next:
    { if ( ctx->it->hasNext() )
      { TripleString *t = ctx->it->next();

	if ( (!(ctx->flags&S_S) || unify_string(A3, t->getSubject().c_str())) &&
	     (!(ctx->flags&S_P) || unify_string(A4, t->getPredicate().c_str())) &&
	     (!(ctx->flags&S_O) || unify_object(A5, t->getObject().c_str())) )
	{ if ( ctx == &ctx_buf )
	  { ctx = (search_it*)PL_malloc(sizeof(*ctx));
	    *ctx = ctx_buf;
	  }
	  PL_retry_address(ctx);
	}
      }
      rc = FALSE;
      goto cleanup;
    }
    case PL_PRUNED:
      ctx = (search_it*)PL_foreign_context_address(handle);
      rc = TRUE;
    cleanup:
      if ( ctx->it )
	delete ctx->it;
      if ( ctx != &ctx_buf )
	PL_free(ctx);
      return rc;
  }

  return FALSE;
}


/** hdt_suggestions(+HDT, +From, +Role, +MaxCount, -Suggestions)
*/

PREDICATE(hdt_suggestions, 5)
{ hdt_wrapper *symb;
  TripleComponentRole role;
  char *from;
  size_t len;
  int max_count;
  std::vector<string> out;

  if ( !get_hdt(A1, &symb) ||
       !PL_get_nchars(A2, &len, &from,
		      CVT_ATOM|CVT_STRING|CVT_EXCEPTION|REP_UTF8) ||
       !get_triple_role(A3, &role) ||
       !PL_get_integer_ex(A4, &max_count) )
    return FALSE;

  try
  { symb->hdt->getDictionary()->getSuggestions(from, role, out, max_count);
  } CATCH_HDT;

  term_t tail = PL_copy_term_ref(A5);
  term_t head = PL_new_term_ref();
  for(std::vector<string>::iterator it = out.begin();
      it != out.end();
      ++it)
  { if ( !PL_unify_list(tail,head,tail) ||
	 !(role == OBJECT ? unify_object(head, it->c_str())
			  : unify_string(head, it->c_str())) )
      return FALSE;
  }

  return PL_unify_nil(tail);
}


		 /*******************************
		 *      DICTIONARY ACCESS	*
		 *******************************/

PREDICATE(hdt_property_, 2)
{ hdt_wrapper *symb;
  atom_t name; size_t arity;

  if ( !get_hdt(A1, &symb) )
    return FALSE;

  if ( PL_get_name_arity(A2, &name, &arity) )
  { PlTerm a = A2[1];

    try
    { Dictionary *dict = symb->hdt->getDictionary();

      if ( name == ATOM_mapping )
	return (a = (long)dict->getMapping());
      else if ( name == ATOM_max_id )
	return (a = (long)dict->getMaxID());
      else if ( name == ATOM_max_object_id )
	return (a = (long)dict->getMaxObjectID());
      else if ( name == ATOM_max_predicate_id )
	return (a = (long)dict->getMaxPredicateID());
      else if ( name == ATOM_max_subject_id )
	return (a = (long)dict->getMaxSubjectID());
      else if ( name == ATOM_objects )
	return (a = (long)dict->getNobjects());
      else if ( name == ATOM_predicates )
	return (a = (long)dict->getNpredicates());
      else if ( name == ATOM_shared )
	return (a = (long)dict->getNshared());
      else if ( name == ATOM_subjects )
	return (a = (long)dict->getNsubjects());
      else if ( name == ATOM_elements )
	return (a = (long)dict->getNumberOfElements());
      else
	return PL_domain_error("hdt_property", A2);
    } CATCH_HDT;
  }

  return PL_type_error("compound", A2);
}


PREDICATE_NONDET(hdt_column_, 3)
{ IteratorUCharString *it;

  switch(PL_foreign_control(handle))
  { case PL_FIRST_CALL:
    { hdt_wrapper *symb;
      atom_t a;

      if ( !get_hdt(A1, &symb) ||
	   !PL_get_atom_ex(A2, &a) )
	return FALSE;

      try
      { Dictionary *dict = symb->hdt->getDictionary();
	if ( a == ATOM_subject )
	  it = dict->getSubjects();
	else if ( a == ATOM_predicate )
	  it = dict->getPredicates();
	else if ( a == ATOM_shared )
	  it = dict->getShared();
	else if ( a == ATOM_object )
	  it = dict->getObjects();
	else
	  return PL_domain_error("hdt_column", A2);
      } CATCH_HDT;

      goto next;
    }
    case PL_REDO:
      it = (IteratorUCharString*)PL_foreign_context_address(handle);
    next:
      if ( it->hasNext() )
      { unsigned char *s = it->next();
	int rc;

	rc = PL_unify_chars(A3, PL_ATOM|REP_UTF8, (size_t)-1, (const char*)s);
	it->freeStr(s);
	if ( rc )
	  PL_retry_address((void*)it);
      }
      delete it;
      return FALSE;
    case PL_PRUNED:
      it = (IteratorUCharString*)PL_foreign_context_address(handle);
      delete it;
      return TRUE;
  }
}


PREDICATE_NONDET(hdt_object_, 2)
{ IteratorUCharString *it;
  uintptr_t mask = 0;

  switch(PL_foreign_control(handle))
  { case PL_FIRST_CALL:
    { hdt_wrapper *symb;
      atom_t a;

      if ( !get_hdt(A1, &symb) )
	return FALSE;

      try
      { it = symb->hdt->getDictionary()->getObjects();
      } CATCH_HDT;
      goto next;
    }
    case PL_REDO:
      it = (IteratorUCharString*)PL_foreign_context_address(handle);
    next:
      if ( it->hasNext() )
      { unsigned char *s = it->next();
	int rc;

	rc = unify_object(A2, (const char*)s);
	it->freeStr(s);
	if ( rc )
	  PL_retry_address((void*)it);
      }
      delete it;
      return FALSE;
    case PL_PRUNED:
      it = (IteratorUCharString*)PL_foreign_context_address(handle);
      delete it;
      return TRUE;
  }
}


static int
get_triple_role(term_t t, TripleComponentRole *role)
{ atom_t name;

  if ( !PL_get_atom_ex(t, &name) )
    return FALSE;
  if ( name == ATOM_subject )
    *role = SUBJECT;
  else if ( name == ATOM_predicate )
    *role = PREDICATE;
  else if ( name == ATOM_object )
    *role = OBJECT;
  else
    return PL_domain_error("hdt_role", t);

  return TRUE;
}


/** hdt_string_id(+HDT, +Role, ?String, ?Id)
*/

PREDICATE(hdt_string_id, 4)
{ hdt_wrapper *symb;
  TripleComponentRole roleid;
  size_t len; char *s;

  if ( !get_hdt(A1, &symb) ||
       !get_triple_role(A2, &roleid) )
    return FALSE;

  try
  { Dictionary *dict = symb->hdt->getDictionary();

    if ( !PL_is_variable(A3) )
    { if ( PL_get_nchars(A3, &len, &s,
			 CVT_ATOM|CVT_STRING|REP_UTF8|CVT_EXCEPTION) )
      { std::string str(s);
	size_t id = dict->stringToId(str, roleid);

	if ( id )
	  return (A4 = (long)id);	/* signed/unsigned mismatch */
      }
    } else
    { std::string str = dict->idToString((size_t)(long)A4, roleid);

      if ( !str.empty() )
	return PL_unify_chars(A3, PL_ATOM|REP_UTF8, (size_t)-1, str.c_str());
    }
  } CATCH_HDT;

  return FALSE;
}


typedef struct
{ unsigned flags;
  IteratorTripleID *it;
} searchid_it;


static int
get_search_id(term_t t, size_t *id, unsigned flag, unsigned *flagp)
{ if ( PL_is_variable(t) )
  { *id = 0;
    *flagp |= flag;
    return TRUE;
  } else
  { size_t i;

    if ( PL_get_size_ex(t, &i) )
    { *id = i;
      return TRUE;
    }
  }

  return FALSE;
}



/** hdt_search_id(+HDT, ?S, ?P, ?O)
*/

PREDICATE_NONDET(hdt_search_id, 4)
{ hdt_wrapper *symb;
  searchid_it ctx_buf = {0};
  searchid_it *ctx;
  int rc;

  switch(PL_foreign_control(handle))
  { case PL_FIRST_CALL:
    { size_t s, p, o;

      ctx = &ctx_buf;
      if ( !get_hdt(A1, &symb) ||
	   !get_search_id(A2, &s, S_S, &ctx->flags) ||
	   !get_search_id(A3, &p, S_P, &ctx->flags) ||
	   !get_search_id(A4, &o, S_O, &ctx->flags) )
	return FALSE;

      try
      { TripleID t(s,p,o);
	ctx->it = symb->hdt->getTriples()->search(t);
      } CATCH_HDT;

      goto next;
    }
    case PL_REDO:
      ctx = (searchid_it*)PL_foreign_context_address(handle);
    next:
    { if ( ctx->it->hasNext() )
      { TripleID *t = ctx->it->next();

	if ( (!(ctx->flags&S_S) || PL_unify_integer(A2, t->getSubject())) &&
	     (!(ctx->flags&S_P) || PL_unify_integer(A3, t->getPredicate())) &&
	     (!(ctx->flags&S_O) || PL_unify_integer(A4, t->getObject())) )
	{ if ( ctx == &ctx_buf )
	  { ctx = (searchid_it*)PL_malloc(sizeof(*ctx));
	    *ctx = ctx_buf;
	  }
	  PL_retry_address(ctx);
	}
      }
      rc = FALSE;
      goto cleanup;
    }
    case PL_PRUNED:
      ctx = (searchid_it*)PL_foreign_context_address(handle);
      rc = TRUE;
    cleanup:
      if ( ctx->it )
	delete ctx->it;
      if ( ctx != &ctx_buf )
	PL_free(ctx);
      return rc;
  }

  return FALSE;
}


/** hdt_search_cost_id(+HDT, ?S, ?P, ?O, -Cost)
*/

PREDICATE(hdt_search_cost_id, 5)
{ hdt_wrapper *symb;
  unsigned int flags=0;
  size_t s, p, o;

  if ( !get_hdt(A1, &symb) ||
       !get_search_id(A2, &s, S_S, &flags) ||
       !get_search_id(A3, &p, S_P, &flags) ||
       !get_search_id(A4, &o, S_O, &flags) )
    return FALSE;

  try
  { TripleID t(s,p,o);
    IteratorTripleID *it = symb->hdt->getTriples()->search(t);
    long numResults = it->estimatedNumResults();
    delete it;
    return (A5 = numResults);
  } CATCH_HDT;
}


		 /*******************************
		 *	      GENERATE		*
		 *******************************/

/**
 * hdt_create_from_file(+HDTFile, +RDFFile, +Options)
 *
 * @tbd Fill HDTSpecification
 * @tbd Allow additional header triples
 */

PREDICATE(hdt_create_from_file, 3)
{ char *hdt_file, *rdf_file;
  HDTSpecification spec;
  char *base_uri = (char*)"http://example.org/base";

  if ( !PL_get_file_name(A1, &hdt_file, PL_FILE_OSPATH) ||
       !PL_get_file_name(A2, &rdf_file, PL_FILE_OSPATH|PL_FILE_READ) )
    return FALSE;

  PlTail options(A3);
  PlTerm opt;
  while(options.next(opt))
  { atom_t name;
    size_t arity;

    if ( PL_get_name_arity(opt, &name, &arity) && arity == 1 )
    { PlTerm ov = opt[1];

      if ( name == ATOM_base_uri )
      { size_t len;

	if ( !PL_get_nchars(ov, &len, &base_uri,
			    CVT_ATOM|CVT_STRING|CVT_EXCEPTION|REP_UTF8) )
	  return FALSE;
      }
    } else
      return PL_type_error("option", opt);
  }

  try
  { HDT *hdt = HDTManager::generateHDT(rdf_file, base_uri, NTRIPLES, spec);

    //Header *header = hdt->getHeader();
    //header->insert("myResource1", "property", "value");

    hdt->saveToHDT(hdt_file);

    delete hdt;
  } CATCH_HDT

  return TRUE;
}


