/*  Part of Assertion Reader for SWI-Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/assertions
    Copyright (C): 2017, Process Design Center, Breda, The Netherlands.
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

#ifndef __FOREIGN_SWIPL_H__
#define __FOREIGN_SWIPL_H__

#include <foreign_interface.h>
#define PL_ARITY_AS_SIZE
#include <SWI-Prolog.h>


#ifndef PL_KERNEL
#define ATOM_dict	(_PL_atoms()[2]) /* <dict> */
#endif /*PL_KERNEL*/

#if (defined(__RTCHECK__))
#define __rtctype(__call, __term, __type) ({			\
	int __result = (__call);				\
	if (!(__result)) {					\
	    if (!PL_exception(0)) {				\
		term_t __except = PL_new_term_ref();		\
		if(!PL_unify_term(__except,			\
				  PL_FUNCTOR_CHARS,		\
				  "error", 2,			\
				  PL_FUNCTOR_CHARS,		\
				  "type_error", 2,		\
				  PL_CHARS, "" # __type,	\
				  PL_TERM, __term,		\
				  PL_FUNCTOR_CHARS,		\
				  "context", 2,			\
				  PL_FUNCTOR_CHARS,		\
				  ":", 2,			\
				  PL_CHARS, __FILE__,		\
				  PL_INT, __LINE__,		\
				  PL_FUNCTOR_CHARS,		\
				  "->", 2,			\
				  PL_CHARS, __FUNCTION__,	\
				  PL_CHARS, # __call		\
		       ))					\
		    __result =  FALSE;				\
		__result = PL_raise_exception(__except);	\
	    }							\
	}							\
	__result;						\
    })

#else
#define __rtctype(__call, __term, __type) __call
#endif

#define FI_get_integer(_, t, i) PL_get_integer(t, i)
#define FI_get_long(_, t, l) PL_get_long(t, l)
#define FI_get_float(_, t, f)   PL_get_float(t, f)
#define FI_get_float_t(_, t, f) ({			\
	    double d;					\
	    int __result = PL_get_float(t, &d);		\
	    *f = (float)d;				\
	    __result;					\
	})
#define FI_get_pointer(_, t, p) PL_get_pointer(t, p)
#define FI_get_chrs(_, t, v)    PL_get_atom_chars(t, v)
#define FI_get_string(_, t, v)  PL_get_string_chars(t, v, NULL)
#define FI_get_char_code(_, t, c) ({			\
	    int i;					\
	    int __result = PL_get_integer(t, &i);	\
	    *c = (char)i;				\
	    __result;					\
	})
#define FI_get_char(_, t, c) ({				\
	    char *s;					\
	    int __result = PL_get_atom_chars(t, &s);	\
	    if (__result) {				\
		*c = s[0];				\
	    }						\
	    __result;					\
	})

#define FI_unify_integer(t, p)   PL_unify_integer(t, p)
#define FI_unify_long(t, p)      PL_unify_integer(t, p) /* there is no PL_unify_long */
#define FI_unify_float(t, p)     PL_unify_float(t, p)
#define FI_unify_float_t(t, p)   PL_unify_float(t, (double)(p))
#define FI_unify_pointer(t, p) ({			\
	    int __result = TRUE;			\
	    if (p!=NULL)				\
		__result = PL_unify_pointer(t, p);	\
	    __result;					\
	})
#define FI_unify_chrs(t, v) ({				\
	    int __result = TRUE;			\
	    if (v!=NULL)				\
		__result = PL_unify_atom_chars(t, v);	\
	    __result;					\
	})
#define FI_unify_string(t, v) ({                        \
            if (v!=NULL)                                \
                PL_unify_string_chars(t, v);            \
            TRUE;                                       \
        })

#define FI_unify_char_code(t, c) PL_unify_integer(t, c)
#define FI_unify_char(t, c) ({		\
	    char s[2];			\
	    s[0] = c;			\
	    s[1] = '\0';		\
	    PL_unify_atom_chars(t, s);	\
	})

#define __rtc_FI_unify(__type, __term, __value)	({			\
      __rtcpass(__rtctype(FI_unify_##__type(__term, __value), __term, __type)); \
    })

#define __rtc_FI_get(__type, __term, __value) ({			\
      __rtcpass(__rtctype(FI_get_##__type(__root, __term, __value), __term, __type)); \
    })

#define FI_get_list(__FI_get_elem, __term, __value) ({	\
      term_t __term##_ = PL_new_term_ref();			\
      term_t __tail = PL_copy_term_ref(__term);			\
      size_t __length = 0;					\
      while(PL_get_list(__tail, __term##_, __tail))		\
	__length++;						\
      __rtcheck(PL_get_nil(__tail));				\
      __tail = PL_copy_term_ref(__term);			\
      FI_new_array(__length, *__value);				\
      typeof (*__value) _c_##__term##_ = *__value;		\
      while(PL_get_list(__tail, __term##_, __tail)) {		\
	__FI_get_elem;						\
	_c_##__term##_++;					\
      };							\
    })

#define FI_get_inout_list(__FI_get_elem, __term, __value) ({	\
      if(PL_is_variable(__term)) {				\
	*__value = NULL;					\
      }								\
      else {							\
	FI_get_list(__FI_get_elem, __term, __value);		\
      }								\
    })

#define FI_get_in_list(__FI_get_elem, __term, __value) ({		\
      __rtcpass(__rtctype(!PL_is_variable(__term), __term, list));	\
      FI_get_list(__FI_get_elem, __term, __value);			\
    })

#define FI_get_ptr(__FI_get_elem, __term, __value) ({	\
      term_t __term##_ = __term;			\
      FI_new_value(*__value);				\
      typeof (*__value) _c_##__term##_ = *__value;	\
      __FI_get_elem;					\
    })

#define FI_get_inout_ptr(__FI_get_elem, __term, __value) ({	\
      if(PL_is_variable(__term)) {				\
	*__value = NULL;					\
      }								\
      else {							\
	FI_get_ptr(__FI_get_elem, __term, __value);		\
      }								\
    })

#define FI_get_in_ptr(__FI_get_elem, __term, __value) \
    FI_get_inout_ptr(__FI_get_elem, __term, __value)

/*
#define FI_get_in_ptr(__FI_get_elem, __term, __value) {			\
	__rtcpass(__rtctype(!PL_is_variable(__term), __term, ptr));	\
	FI_get_ptr(__FI_get_elem, __term, __value);			\
    }
*/

#define FI_unify_list(__FI_unify_elem, __term, __value) ({	\
      if (__value!=NULL) {					\
	term_t l = PL_copy_term_ref(__term);			\
	term_t __term##_ = PL_new_term_ref();			\
	size_t __index;						\
	size_t __length = FI_array_length(__value);		\
	for(__index = 0; __index < __length; __index++)	{	\
	  typeof (*__value) _c_##__term##_ = __value[__index];	\
	  __rtcheck(PL_unify_list(l, __term##_, l));		\
	  __FI_unify_elem;					\
	}							\
	__rtcheck(PL_unify_nil(l));				\
      }								\
    })

#define FI_unify_ptr(__FI_unify_elem, __term, __value) ({	\
      if(__value!=NULL) {					\
	term_t __term##_ = __term;				\
	typeof (*__value) _c_##__term##_ = *__value;		\
	__FI_unify_elem;					\
      }								\
    })

#define FI_get_inout(__getter, __term, __value) ({	\
      if(PL_is_variable(__term)) {			\
	*__value = NULL;				\
      }							\
      else {						\
	FI_new_value(*__value);				\
	__rtc_FI_get(__getter, __term, *__value);	\
      }							\
    })

#define FI_unify_inout(__unifier, __term, __value) {	 \
	if (__value!=NULL) {				 \
	    __rtc_FI_unify(__unifier, __term, *__value); \
	}						 \
    }

#define FI_unify_inout_type(__unifier, __term, __value) { \
	if (__value!=NULL) {				  \
	    __rtc_FI_unify(__unifier, __term, __value);	  \
	}						  \
    }

#define FI_get_inout_chrs(__term, __value) {		\
	if(PL_is_variable(__term)) {			\
	    *__value = NULL;				\
	}						\
	else {						\
	    __rtc_FI_get(chrs, __term, __value);	\
	}						\
    }

#define FI_get_inout_string(__term, __value) {		\
	if(PL_is_variable(__term)) {			\
	    *__value = NULL;				\
	}						\
	else {						\
	    __rtc_FI_get(string, __term, __value);	\
	}						\
    }

#define FI_unify_inout_chrs(__term, __value) {		\
	if (__value!=NULL) {				\
	    __rtc_FI_unify(chrs, __term, __value);	\
	}						\
    }

#define FI_unify_inout_string(__term, __value) {        \
	if (__value!=NULL) {				\
	    __rtc_FI_unify(string, __term, __value);	\
	}						\
    }

extern predicate_t __system_dict_create;
extern predicate_t __system_put_dict;
extern predicate_t __system_get_dict;
extern predicate_t __system_clause;
extern predicate_t __foreign_generator_call_idx;

#define FI_get_dict_t(__unifier, __data, __value) {			\
	term_t __dict = PL_new_term_refs(3);				\
	PL_put_term(__dict+2, __data);					\
	__rtcheck(__rtctype(PL_call_predicate(NULL, PL_Q_NORMAL,	\
					      __system_dict_create, __dict), \
			    __data, "valid data for " # __unifier));	\
	term_t __args = PL_new_term_refs(3);				\
	PL_put_term(__args+1, __dict);					\
	qid_t __p = PL_open_query(NULL, PL_Q_NORMAL, __system_get_dict, \
				  __args);				\
	term_t __k = __args+0;						\
	term_t __v = __args+2;						\
	while (PL_next_solution(__p)) {					\
	    __rtcpass(get_pair_##__unifier(__root, __k, __v, __value));	\
	}								\
	PL_close_query(__p);						\
    }

#define FI_get_keyid_index(__pred, __keyid, __index) ({			\
      term_t __args = PL_new_term_refs(2);				\
      PL_put_term(__args, __keyid);					\
      __rtcheck(__rtctype(PL_call_predicate(NULL, PL_Q_NORMAL, __pred, __args), \
			__keyid, "valid key of " # __pred));		\
      __rtcheck(PL_get_integer(__args+1, &__index));			\
    })

#define FI_init_dict_t(__dict, __tag) ({			\
      term_t __args = PL_new_term_refs(3);			\
      PL_put_atom_chars(__args+1, __tag);			\
      PL_put_nil(__args+2);					\
      __rtcheck(PL_call_predicate(NULL, PL_Q_NORMAL,		\
				__system_dict_create, __args));	\
      __dict = __args;						\
    })

#define FI_put_desc(__tail, __key, __value) ({				\
	term_t __kv = PL_new_term_ref();				\
	__rtcheck(PL_unify_list(__tail, __kv, __tail));			\
	__rtcheck(PL_unify_term(__kv, PL_FUNCTOR_CHARS, "-", 2,		\
				PL_CHARS, __key,			\
				PL_TERM,  __value));			\
    })

#define FI_dict_create(__dict, __tag, __desc) ({			\
      term_t __args = PL_new_term_refs(3);				\
      PL_put_atom_chars(__args+1, __tag);				\
      PL_put_term(__args+2, __desc);					\
      __rtcheck(PL_call_predicate(NULL, PL_Q_NORMAL,			\
				  __system_dict_create, __args));	\
      __rtcheck(PL_unify(__dict, __args));				\
    })

#endif // __FOREIGN_SWIPL_H__
