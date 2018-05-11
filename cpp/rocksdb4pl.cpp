/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2016, VU University Amsterdam
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

#include <assert.h>
#include <mutex>
#include <rocksdb/db.h>
#include <rocksdb/env.h>
#include <rocksdb/write_batch.h>
#include <rocksdb/merge_operator.h>
#define PL_ARITY_AS_SIZE 1
#include <SWI-Stream.h>
#include <SWI-Prolog.h>
#include <SWI-cpp.h>

using namespace rocksdb;

		 /*******************************
		 *	       SYMBOL		*
		 *******************************/

#define DB_DESTROYED	0x0001		/* Was destroyed by user  */
#define DB_OPEN_ONCE	0x0002		/* open(once) option */

typedef enum
{ BLOB_ATOM = 0,			/* UTF-8 string as atom */
  BLOB_STRING,				/* UTF-8 string as string */
  BLOB_BINARY,				/* Byte string as string */
  BLOB_INT32,				/* 32-bit native integer */
  BLOB_INT64,				/* 64-bit native integer */
  BLOB_FLOAT32,				/* 32-bit IEEE float */
  BLOB_FLOAT64,				/* 64-bit IEEE double */
  BLOB_TERM				/* Arbitrary term */
} blob_type;

typedef struct dbref
{ rocksdb::DB	*db;			/* DB handle */
  atom_t         symbol;		/* associated symbol */
  atom_t	 name;			/* alias name */
  int	         flags;			/* flags */
  record_t	 merger;		/* merge option */
  struct
  { blob_type key;
    blob_type value;
  } type;
} dbref;


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
rocks_get_alias(atom_t name)
{ for(alias_cell *c = alias_entries[atom_hash(name)];
      c;
      c = c->next)
  { if ( c->name == name )
      return c->symbol;
  }

  return NULL_ATOM;
}

static void
rocks_alias(atom_t name, atom_t symbol)
{ unsigned int key = atom_hash(name);

  alias_lock.lock();
  if ( !rocks_get_alias(name) )
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
    throw PlPermissionError("alias", "rocksdb", PlTerm(name));
  }
}

static void
rocks_unalias(atom_t name)
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
write_rocks_ref(IOSTREAM *s, atom_t eref, int flags)
{ dbref **refp = (dbref **)PL_blob_data(eref, NULL, NULL);
  dbref *ref = *refp;
  (void)flags;

  Sfprintf(s, "<rocksdb>(%p)", ref);
  return TRUE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
GC an rocks from the atom garbage collector.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
release_rocks_ref(atom_t aref)
{ dbref **refp = (dbref **)PL_blob_data(aref, NULL, NULL);
  dbref *ref   = *refp;
  rocksdb::DB *db;

  assert(ref->name == NULL_ATOM);

  if ( (db=ref->db) )
  { ref->db = NULL;
    delete db;
  }
  if ( ref->merger )
  { PL_erase(ref->merger);
    ref->merger = 0;
  }
  PL_free(ref);

  return TRUE;
}


static int
save_rocks(atom_t aref, IOSTREAM *fd)
{ dbref **refp = (dbref **)PL_blob_data(aref, NULL, NULL);
  dbref *ref   = *refp;
  (void)fd;

  return PL_warning("Cannot save reference to <rocksdb>(%p)", ref);
}


static atom_t
load_rocks(IOSTREAM *fd)
{ (void)fd;

  return PL_new_atom("<saved-rocksdb-ref>");
}


static PL_blob_t rocks_blob =
{ PL_BLOB_MAGIC,
  PL_BLOB_UNIQUE,
  (char*)"rocksdb",
  release_rocks_ref,
  NULL,
  write_rocks_ref,
  NULL,
  save_rocks,
  load_rocks
};


static int
unify_rocks(term_t t, dbref *ref)
{ if ( ref->name )
  { if ( !ref->symbol )
    { PlTerm tmp;

      if ( PL_unify_blob(tmp, &ref, sizeof(ref), &rocks_blob) &&
	   PL_get_atom(tmp, &ref->symbol) )
      { rocks_alias(ref->name, ref->symbol);
      } else
      { assert(0);
      }
    }
    return PL_unify_atom(t, ref->name);
  } else if ( ref->symbol )
  { return PL_unify_atom(t, ref->symbol);
  } else
  { return ( PL_unify_blob(t, &ref, sizeof(ref), &rocks_blob) &&
	     PL_get_atom(t, &ref->symbol)
	   );
  }
}


static dbref*
symbol_dbref(atom_t symbol)
{ void *data;
  size_t len;
  PL_blob_t *type;

  if ( (data=PL_blob_data(symbol, &len, &type)) && type == &rocks_blob )
  { dbref **erd = (dbref **)data;
    return *erd;
  }

  return (dbref*)NULL;
}


static int
get_rocks(term_t t, dbref **erp, int warn=TRUE)
{ atom_t a;

  if ( PL_get_atom(t, &a) )
  { for(int i=0; i<2; i++)
    { dbref *ref;

      if ( (ref=symbol_dbref(a)) )
      { if ( !(ref->flags & DB_DESTROYED) )
	{ *erp = ref;
	  return TRUE;
	} else if ( warn )
	{ throw PlExistenceError("rocksdb", t);
	}
      }

      a=rocks_get_alias(a);
    }

    throw PlExistenceError("rocksdb", t);
  }

  if ( warn )
    throw PlTypeError("rocksdb", t);

  return FALSE;
}


		 /*******************************
		 *	      UTIL		*
		 *******************************/

class RocksError : public PlException
{
public:
  RocksError(const rocksdb::Status &status) :
    PlException(PlCompound("error",
			   PlTermv(PlCompound("rocks_error",
					      PlTerm(status.ToString().c_str())),
				   PlTerm())))
  {
  }
};


static int
ok(const rocksdb::Status &status)
{ if ( status.ok() )
    return TRUE;
  if ( status.IsNotFound() )
    return FALSE;
  throw RocksError(status);
}

class PlSlice : public Slice
{
public:
  int must_free = 0;
  union
  { int i32;
    int64_t i64;
    float f32;
    double f64;
  } v;

  ~PlSlice()
  { if ( must_free )
      PL_erase_external((char*)data_);
  }
};


#define CVT_IN	(CVT_ATOM|CVT_STRING|CVT_LIST)

static void
get_slice(term_t t, PlSlice &s, blob_type type)
{ char *str;
  size_t len;

  switch(type)
  { case BLOB_ATOM:
    case BLOB_STRING:
      if ( PL_get_nchars(t, &len, &str, CVT_IN|CVT_EXCEPTION|REP_UTF8) )
      { s.data_ = str;
	s.size_ = len;
	return;
      }
      throw(PlException(PL_exception(0)));
    case BLOB_BINARY:
      if ( PL_get_nchars(t, &len, &str, CVT_IN|CVT_EXCEPTION) )
      { s.data_ = str;
	s.size_ = len;
	return;
      }
      throw(PlException(PL_exception(0)));
    case BLOB_INT32:
    { if ( PL_get_integer_ex(t, &s.v.i32) )
      { s.data_ = (char*)&s.v.i32;
	s.size_ = sizeof(s.v.i32);
	return;
      }
      throw(PlException(PL_exception(0)));
    }
    case BLOB_INT64:
    { if ( PL_get_int64_ex(t, &s.v.i64) )
      { s.data_ = (char*)&s.v.i64;
	s.size_ = sizeof(s.v.i64);
	return;
      }
      throw(PlException(PL_exception(0)));
    }
    case BLOB_FLOAT32:
    { double d;

      if ( PL_get_float_ex(t, &d) )
      { s.v.f32 = (float)d;
	s.data_ = (char*)&s.v.f32;
	s.size_ = sizeof(s.v.f32);
	return;
      }
      throw(PlException(PL_exception(0)));
    }
    case BLOB_FLOAT64:
    { if ( PL_get_float_ex(t, &s.v.f64) )
      { s.data_ = (char*)&s.v.f64;
	s.size_ = sizeof(s.v.f64);
	return;
      }
      throw(PlException(PL_exception(0)));
    }
    case BLOB_TERM:
      if ( (str=PL_record_external(t, &len)) )
      { s.data_ = str;
	s.size_ = len;
	s.must_free = TRUE;
	return;
      }
      throw(PlException(PL_exception(0)));
    default:
      assert(0);
  }
}


static const PlAtom ATOM_("");


static int
unify(term_t t, const Slice &s, blob_type type)
{ switch(type)
  { case BLOB_ATOM:
      return PL_unify_chars(t, PL_ATOM|REP_UTF8, s.size_, s.data_);
    case BLOB_STRING:
      return PL_unify_chars(t, PL_STRING|REP_UTF8, s.size_, s.data_);
    case BLOB_BINARY:
      return PL_unify_chars(t, PL_STRING|REP_ISO_LATIN_1, s.size_, s.data_);
    case BLOB_INT32:
    { int i;

      memcpy(&i, s.data_, sizeof(i));
      return PL_unify_integer(t, i);
    }
    case BLOB_INT64:
    { int64_t i;

      memcpy(&i, s.data_, sizeof(i));
      return PL_unify_int64(t, i);
    }
    case BLOB_FLOAT32:
    { float f;

      memcpy(&f, s.data_, sizeof(f));
      return PL_unify_float(t, f);
    }
    case BLOB_FLOAT64:
    { double f;

      memcpy(&f, s.data_, sizeof(f));
      return PL_unify_float(t, f);
    }
    case BLOB_TERM:
    { PlTerm tmp;

      return ( PL_recorded_external(s.data_, tmp) &&
	       PL_unify(tmp, t)
	     );
    }
    default:
      assert(0);
      return FALSE;
  }
}

static int
unify(term_t t, const Slice *s, blob_type type)
{ if ( s == (Slice*)NULL )
  { switch(type)
    { case BLOB_ATOM:
	return PL_unify_atom(t, ATOM_.handle);
      case BLOB_STRING:
      case BLOB_BINARY:
	return PL_unify_chars(t, PL_STRING, 0, "");
      case BLOB_INT32:
      case BLOB_INT64:
	return PL_unify_integer(t, 0);
      case BLOB_FLOAT32:
      case BLOB_FLOAT64:
	return PL_unify_float(t, 0.0);
      case BLOB_TERM:
	return PL_unify_nil(t);
      default:
	assert(0);
        return FALSE;
    }
  }

  return unify(t, *s, type);
}

static int
unify(term_t t, const std::string &s, blob_type type)
{ Slice sl(s.data(), s.length());

  return unify(t, sl, type);
}


		 /*******************************
		 *	       MERGER		*
		 *******************************/

static const PlAtom ATOM_partial("partial");
static const PlAtom ATOM_full("full");

static int
log_exception(Logger* logger)
{ PlException ex(PL_exception(0));

  Log(logger, "%s", (char*)ex);
  return false;
}

class engine
{ int tid = 0;

public:
  engine()
  { if ( PL_thread_self() == -1 )
    { if ( (tid=PL_thread_attach_engine(NULL)) < 0 )
      { term_t ex;

	if ( (ex = PL_exception(0)) )
	  PlException(ex).cppThrow();
	else
	  throw PlResourceError("memory");
      }
    }
  }

  ~engine()
  { if ( tid )
      PL_thread_destroy_engine();
  }
};

static int
call_merger(const dbref *ref, PlTermv av, std::string* new_value,
	    Logger* logger)
{ static predicate_t pred_call6 = NULL;

  if ( !pred_call6 )
    pred_call6 = PL_predicate("call", 6, "system");

  try
  { PlQuery q(pred_call6, av);
    if ( q.next_solution() )
    { PlSlice answer;

      get_slice(av[5], answer, ref->type.value);
      new_value->assign(answer.data(), answer.size());
      return true;
    } else
    { Log(logger, "merger failed");
      return false;
    }
  } catch(PlException &ex)
  { Log(logger, "%s", (char*)ex);
    return false;
  }
}


class PrologMergeOperator : public MergeOperator
{ const dbref *ref;
public:
  PrologMergeOperator(const dbref *reference) : MergeOperator()
  { ref = reference;
  }

  virtual bool
  FullMerge(const Slice& key,
	    const Slice* existing_value,
	    const std::deque<std::string>& operand_list,
	    std::string* new_value,
	    Logger* logger) const override
  { engine e;
    PlTermv av(6);
    PlTail list(av[4]);
    PlTerm tmp;

    for (const auto& value : operand_list)
    { PL_put_variable(tmp);
      unify(tmp, value, ref->type.value);
      list.append(tmp);
    }
    list.close();

    if ( PL_recorded(ref->merger, av[0]) &&
	 (av[1] = ATOM_full) &&
	 unify(av[2], key, ref->type.key) &&
	 unify(av[3], existing_value, ref->type.value) )
      return call_merger(ref, av, new_value, logger);
    else
      return log_exception(logger);
  }

  virtual bool
  PartialMerge(const Slice& key,
	       const Slice& left_operand,
	       const Slice& right_operand,
	       std::string* new_value,
	       Logger* logger) const override
  { engine e;
    PlTermv av(6);

    if ( PL_recorded(ref->merger, av[0]) &&
	 (av[1] = ATOM_partial) &&
	 unify(av[2], key, ref->type.key) &&
	 unify(av[3], left_operand, ref->type.value) &&
	 unify(av[4], right_operand, ref->type.value) )
      return call_merger(ref, av, new_value, logger);
    else
      return log_exception(logger);
  }

  virtual const char*
  Name() const override
  { return "PrologMergeOperator";
  }
};



		 /*******************************
		 *	    PREDICATES		*
		 *******************************/

static PlAtom ATOM_key("key");
static PlAtom ATOM_value("value");
static PlAtom ATOM_alias("alias");
static PlAtom ATOM_merge("merge");

static PlAtom ATOM_atom("atom");
static PlAtom ATOM_string("string");
static PlAtom ATOM_binary("binary");
static PlAtom ATOM_int32("int32");
static PlAtom ATOM_int64("int64");
static PlAtom ATOM_float("float");
static PlAtom ATOM_double("double");
static PlAtom ATOM_term("term");
static PlAtom ATOM_open("open");
static PlAtom ATOM_once("once");
static PlAtom ATOM_mode("mode");
static PlAtom ATOM_read_only("read_only");
static PlAtom ATOM_read_write("read_write");

static void
get_blob_type(PlTerm t, blob_type *key_type)
{ atom_t a;

  if ( PL_get_atom(t, &a) )
  {      if ( ATOM_atom   == a ) *key_type = BLOB_ATOM;
    else if ( ATOM_string == a ) *key_type = BLOB_STRING;
    else if ( ATOM_binary == a ) *key_type = BLOB_BINARY;
    else if ( ATOM_int32  == a ) *key_type = BLOB_INT32;
    else if ( ATOM_int64  == a ) *key_type = BLOB_INT64;
    else if ( ATOM_float  == a ) *key_type = BLOB_FLOAT32;
    else if ( ATOM_double == a ) *key_type = BLOB_FLOAT64;
    else if ( ATOM_term   == a ) *key_type = BLOB_TERM;
    else throw PlDomainError("rocks_type", t);

    return;
  }

  throw PlTypeError("atom", t);
}


PREDICATE(rocks_open_, 3)
{ dbref *ref;
  rocksdb::Options options;
  options.create_if_missing = true;
  char *fn;
  blob_type key_type   = BLOB_ATOM;
  blob_type value_type = BLOB_ATOM;
  atom_t alias = NULL_ATOM;
  record_t merger = 0;
  int once = FALSE;
  int read_only = FALSE;

  if ( !PL_get_file_name(A1, &fn, PL_FILE_OSPATH) )
    return FALSE;
  PlTail tail(A3);
  PlTerm opt;
  while(tail.next(opt))
  { atom_t name;
    size_t arity;

    if ( PL_get_name_arity(opt, &name, &arity) && arity == 1 )
    { if ( ATOM_key == name )
	get_blob_type(opt[1], &key_type);
      else if ( ATOM_value == name )
	get_blob_type(opt[1], &value_type);
      else if ( ATOM_merge == name )
	merger = PL_record(opt[1]);
      else if ( ATOM_alias == name )
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
      } else if ( ATOM_mode == name )
      { atom_t a;

	if ( PL_get_atom(opt[1], &a) )
	{      if ( ATOM_read_write == a )
	    ;
	  else if ( ATOM_read_only == a )
	    read_only = TRUE;
	  else
	    PlDomainError("mode_option", opt[1]);
	}

	PlTypeError("atom", opt[1]);
      }
    } else
      PlTypeError("option", opt);
  }

  if ( alias && once )
  { atom_t existing;

    if ( (existing=rocks_get_alias(alias)) )
    { dbref *eref;

      if ( (eref=symbol_dbref(existing)) &&
	   (eref->flags&DB_OPEN_ONCE) )
	return PL_unify_atom(A2, existing);
    }
  }

  ref = (dbref *)PL_malloc(sizeof(*ref));
  memset(ref, 0, sizeof(*ref));
  ref->merger = merger;
  ref->type.key   = key_type;
  ref->type.value = value_type;
  ref->name = alias;
  if ( once )
    ref->flags |= DB_OPEN_ONCE;

  try
  { rocksdb::Status status;

    if ( ref->merger )
      options.merge_operator.reset(new PrologMergeOperator(ref));
    if ( read_only )
      status = DB::OpenForReadOnly(options, fn, &ref->db);
    else
      status = DB::Open(options, fn, &ref->db);
    ok(status);
    return unify_rocks(A2, ref);
  } catch(...)
  { PL_free(ref);
    throw;
  }
}


PREDICATE(rocks_close, 1)
{ dbref *ref;

  get_rocks(A1, &ref);
  DB* db = ref->db;

  ref->db = NULL;
  ref->flags |= DB_DESTROYED;
  if ( ref->name )
  { rocks_unalias(ref->name);
    ref->name = NULL_ATOM;
  }

  delete db;
  return TRUE;
}


PREDICATE(rocks_put, 3)
{ dbref *ref;
  PlSlice key, value;

  get_rocks(A1, &ref);
  get_slice(A2, key,   ref->type.key);
  get_slice(A3, value, ref->type.value);

  ok(ref->db->Put(WriteOptions(), key, value));

  return TRUE;
}

PREDICATE(rocks_merge, 3)
{ dbref *ref;
  PlSlice key, value;

  get_rocks(A1, &ref);
  if ( !ref->merger )
    throw PlPermissionError("merge", "rocksdb", A1);

  get_slice(A2, key,   ref->type.key);
  get_slice(A3, value, ref->type.value);

  ok(ref->db->Merge(WriteOptions(), key, value));

  return TRUE;
}

PREDICATE(rocks_get, 3)
{ dbref *ref;
  PlSlice key;
  std::string value;

  get_rocks(A1, &ref);
  get_slice(A2, key, ref->type.key);
  return ( ok(ref->db->Get(ReadOptions(), key, &value)) &&
	   unify(A3, value, ref->type.value)
	 );
}

PREDICATE(rocks_delete, 2)
{ dbref *ref;
  PlSlice key;

  get_rocks(A1, &ref);
  get_slice(A2, key, ref->type.key);

  return ok(ref->db->Delete(WriteOptions(), key));
}

typedef struct
{ Iterator *it;
  dbref    *ref;
} enum_state;


PREDICATE_NONDET(rocks_enum, 3)
{ enum_state state_buf;
  enum_state *state = &state_buf;

  switch(PL_foreign_control(handle))
  { case PL_FIRST_CALL:
      get_rocks(A1, &state->ref);
      state->it = state->ref->db->NewIterator(ReadOptions());
      state->it->SeekToFirst();
      goto next;
    case PL_REDO:
      state = (enum_state*)PL_foreign_context_address(handle);
    next:
    { PlFrame fr;
      for(; state->it->Valid(); state->it->Next())
      { if ( unify(A2, state->it->key(), state->ref->type.key) &&
	     unify(A3, state->it->value(), state->ref->type.value) )
	{ state->it->Next();
	  if ( state->it->Valid() )
	  { if ( state == &state_buf )
	    { state = (enum_state*)malloc(sizeof(*state));
	      *state = state_buf;
	    }
	    PL_retry_address(state);
	  } else
	  { delete state->it;
	    if ( state != &state_buf )
	      free(state);
	    return TRUE;
	  }
	}
	fr.rewind();
      }
      delete state->it;
      if ( state != &state_buf )
	free(state);
      return FALSE;
    }
    case PL_PRUNED:
      state = (enum_state*)PL_foreign_context_address(handle);
      delete state->it;
      free(state);
      return TRUE;
  }
  PL_fail;
}

static PlAtom ATOM_delete("delete");
static PlAtom ATOM_put("put");

static void
batch_operation(const dbref *ref, WriteBatch &batch, PlTerm e)
{ atom_t name;
  size_t arity;

  if ( PL_get_name_arity(e, &name, &arity) )
  { if ( ATOM_delete == name && arity == 1 )
    { PlSlice key;

      get_slice(e[1], key, ref->type.key);
      batch.Delete(key);
    } else if ( ATOM_put == name && arity == 2 )
    { PlSlice key, value;

      get_slice(e[1], key,   ref->type.key);
      get_slice(e[2], value, ref->type.value);
      batch.Put(key, value);
    } else
    { PlDomainError("rocks_batch_operation", e);
    }
  } else
  { PlTypeError("compound", e);
  }
}


PREDICATE(rocks_batch, 2)
{ dbref *ref;

  get_rocks(A1, &ref);
  WriteBatch batch;
  PlTail tail(A2);
  PlTerm e;

  while(tail.next(e))
  { batch_operation(ref, batch, e);
  }

  return ok(ref->db->Write(WriteOptions(), &batch));
}


static PlAtom ATOM_estimate_num_keys("estimate_num_keys");

PREDICATE(rocks_property, 3)
{ dbref *ref;
  atom_t prop;

  get_rocks(A1, &ref);

  if ( PL_get_atom(A2, &prop) )
  { if ( ATOM_estimate_num_keys == prop )
    { uint64_t value;

      return ( ref->db->GetIntProperty("rocksdb.estimate-num-keys", &value) &&
	       PL_unify_int64(A3, value) );
    } else
      throw PlDomainError("rocks_property", A2);
  }

  throw PlTypeError("atom", A2);
}
