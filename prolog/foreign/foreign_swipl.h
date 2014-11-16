#ifndef __FOREIGN_SWIPL_H__
#define __FOREIGN_SWIPL_H__

#include <foreign_interface.h>
#include <SWI-Prolog.h>

#if (defined(__RTCHECK__))
#define __rtcheck_type(__call, __term, __type) {		\
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
		    return FALSE;				\
		return PL_raise_exception(__except);		\
	    }							\
	    return __result;					\
	}							\
    }

#else
#define __rtcheck_type(__call, __term, __type) __call
#endif

#define __rtc_PL_unify(__type, __term, __value)				\
    __rtcheck_type(PL_unify_##__type(__term, __value), __term, __type)

#define __rtc_PL_get(__type, __term, __value)				\
    __rtcheck_type(PL_get_##__type(__term, __value), __term, __type)

#define __rtc_PL_get_t(__type, __term, __value)				\
    __rtcheck_type(PL_get_##__type(__root, __term, __value), __term, __type)

#define PL_get_array(__PL_get_elem, __term, __value) {			\
	term_t __term##_ = PL_new_term_ref();				\
	term_t __tail = PL_copy_term_ref(__term);			\
	size_t __length = 0;						\
	while( PL_get_list(__tail, __term##_, __tail) ) __length++;	\
	__tail = PL_copy_term_ref(__term);				\
	FI_new_array(__length, __value);				\
	typeof (__value) _c_##__term##_ = __value;			\
	while(PL_get_list(__tail, __term##_, __tail)) {			\
	    __PL_get_elem;						\
	    _c_##__term##_++;						\
	};								\
	__rtcheck(PL_get_nil(__tail));					\
    }

#define PL_unify_array(__PL_unify_elem, __term, __value) {		\
	term_t l = PL_copy_term_ref(__term);				\
	term_t __term##_ = PL_new_term_ref();				\
	size_t __index;							\
	size_t __length = FI_array_length(__value);			\
	for(__index = 0; __index < __length; __index++)	{		\
	    typeof (*__value) _c_##__term##_ = __value[__index];	\
	    __rtcheck(PL_unify_list(l, __term##_, l));			\
	    __PL_unify_elem;						\
	}								\
	__rtcheck(PL_unify_nil(l));					\
    }

#define PL_get_inout(__getter, __term, __value) {	\
	if(PL_is_variable(__term)) {			\
	    __value = NULL;				\
	}						\
	else {						\
	    FI_new_value(__value);			\
	    __rtc_PL_get(__getter, __term, __value);	\
	}						\
    }

#define PL_get_inout_chrs(__term, __value) {		\
	if(PL_is_variable(__term)) {			\
	    *__value = NULL;				\
	}						\
	else {						\
	    __rtc_PL_get(chrs, __term, __value);	\
	}						\
    }

#define PL_get_inout_t(__getter, __term, __value) {	\
	if(PL_is_variable(__term)) {			\
	    __value = NULL;				\
	}						\
	else {						\
	    FI_new_value(__value);			\
	    __rtc_PL_get_t(__getter, __term, __value);	\
	}						\
    }

#define PL_unify_inout(__unifier, __term, __value) {		\
	if (__value!=NULL) {					\
	    __rtc_PL_unify(__unifier, __term, *__value);	\
	}							\
    }

#define PL_unify_inout_chrs(__term, __value) {		\
	if (__value!=NULL) {				\
	    __rtc_PL_unify(chrs, __term, __value);	\
	}						\
    }

#define PL_get_chrs(__term, __value)   PL_get_atom_chars(__term, __value)
#define PL_unify_chrs(__term, __value) PL_unify_atom_chars(__term, __value)

#define PL_get_dict_t(__unifier, __term, __value) {			\
	if ( PL_is_variable(__term) ) {					\
	    __value = NULL;						\
	}								\
	else {								\
	    int index, arity, pairs;					\
	    __rtcheck(PL_get_name_arity(__term, NULL, &arity));		\
            for(index=1; index < arity;) {				\
		term_t __k = PL_new_term_ref();				\
		term_t __v = PL_new_term_ref();				\
		__rtcheck(PL_get_arg(++index, __term, __k));		\
		__rtcheck(PL_get_arg(++index, __term, __v));		\
		__rtcpass(get_pair_##__unifier(__k, __v, __root, __value)); \
	    }								\
	}								\
    }

 /* else {								\ */
 /* 	term_t m = PL_new_term_ref();					\ */
 /* 	term_t tag  = PL_new_term_ref();				\ */
 /* 	if ( PL_get_dict_ex(__term, tag, m, DICT_GET_PAIRS) )		\ */
 /* 	    return PL_unify(__term, m);					\ */
 /*    } */

#endif // __FOREIGN_SWIPL_H__


