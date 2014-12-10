#ifndef __FOREIGN_SWIPL_H__
#define __FOREIGN_SWIPL_H__

#include <foreign_interface.h>
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
#define FI_get_float(_, t, f)   PL_get_float(t, f)
#define FI_get_pointer(_, t, p) PL_get_pointer(t, p)
#define FI_get_chrs(_, t, v)    PL_get_atom_chars(t, v)
#define FI_get_char_code(_, t, c) {		\
	int i;					\
	int __result = PL_get_integer(t, &i);	\
	*c = (char)i;				\
	__result;				\
    }
#define FI_get_char(_, t, c) {				\
	char *s;					\
	int __result = PL_get_atom_chars(t, &s);	\
	if (__result) {					\
	    *c = s[0];					\
	}						\
	__result;					\
    }

#define FI_unify_integer(t, p)   PL_unify_integer(t, p)
#define FI_unify_float(t, p)     PL_unify_float(t, p)
#define FI_unify_pointer(t, p)   PL_unify_pointer(t, p)
#define FI_unify_chrs(t, v)      PL_unify_atom_chars(t, v)
#define FI_unify_char_code(t, c) PL_unify_integer(t, c)
#define FI_unify_char(t, c) {		\
	char s[2];			\
	s[0] = c;			\
	s[1] = '\0';			\
	PL_unify_atom_chars(t, s);	\
    }

#define __rtc_FI_unify(__type, __term, __value)				\
    __rtcpass(__rtctype(FI_unify_##__type(__term, __value), __term, __type))

#define __rtc_FI_get(__type, __term, __value)				\
    __rtcpass(__rtctype(FI_get_##__type(__root, __term, __value), __term, __type))

#define FI_get_list(__FI_get_elem, __term, __value) {			\
	if(PL_is_variable(__term)) {					\
	    *__value = NULL;						\
	}								\
	else {								\
	    term_t __term##_ = PL_new_term_ref();			\
		term_t __tail = PL_copy_term_ref(__term);		\
		size_t __length = 0;					\
		while(PL_get_list(__tail, __term##_, __tail))		\
		    __length++;						\
		__tail = PL_copy_term_ref(__term);			\
		FI_new_array(__length, *__value);			\
		typeof (*__value) _c_##__term##_ = *__value;		\
		    while(PL_get_list(__tail, __term##_, __tail)) {	\
			__FI_get_elem;					\
			_c_##__term##_++;				\
		    };							\
		    __rtcheck(PL_get_nil(__tail));			\
	}								\
    }

#define FI_get_ptr_nv(__FI_get_elem, __term, __value) {	\
	term_t __term##_ = __term;			\
	FI_new_value(*__value);				\
	typeof (*__value) _c_##__term##_ = *__value;	\
	__FI_get_elem;					\
}

#define FI_get_ptr(__FI_get_elem, __term, __value) {		\
	if(PL_is_variable(__term)) {				\
	    *__value = NULL;					\
	}							\
	else {							\
	    FI_get_ptr_nv(__FI_get_elem, __term, __value);	\
	}							\
    }

#define FI_unify_list(__FI_unify_elem, __term, __value) {		\
	if (__value!=NULL) {						\
	    term_t l = PL_copy_term_ref(__term);			\
	    term_t __term##_ = PL_new_term_ref();			\
		size_t __index;						\
		size_t __length = FI_array_length(__value);		\
		for(__index = 0; __index < __length; __index++)	{	\
		    typeof (*__value) _c_##__term##_ = __value[__index]; \
			__rtcheck(PL_unify_list(l, __term##_, l));	\
			__FI_unify_elem;				\
		}							\
		__rtcheck(PL_unify_nil(l));				\
	}								\
    }

#define FI_unify_ptr(__FI_unify_elem, __term, __value) {	\
	if(__value!=NULL) {					\
	    term_t __term##_ = __term;				\
	    typeof (*__value) _c_##__term##_ = *__value;	\
	    __FI_unify_elem;					\
	}							\
    }

#define FI_get_inout(__getter, __term, __value) {	\
	if(PL_is_variable(__term)) {			\
	    *__value = NULL;				\
	}						\
	else {						\
	    FI_new_value(*__value);			\
	    __rtc_FI_get(__getter, __term, *__value);	\
	}						\
    }

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

#define FI_unify_inout_chrs(__term, __value) {		\
	if (__value!=NULL) {				\
	    __rtc_FI_unify(chrs, __term, __value);	\
	}						\
    }

extern predicate_t __system_dict_create;

#define FI_get_dict_t(__unifier, __desc, __value) {			\
	int index, arity;						\
	atom_t name;							\
	term_t __term = PL_new_term_refs(3);				\
	__rtcheck(PL_unify(__term+2, __desc));				\
	__rtcheck(PL_call_predicate(NULL, PL_Q_NORMAL, __system_dict_create, __term)); \
	__rtcheck(PL_get_name_arity(__term, &name, &arity));		\
	__doifrtc(name==ATOM_dict);					\
	for(index=1; index < arity;) {					\
	    term_t __k = PL_new_term_ref();				\
	    term_t __v = PL_new_term_ref();				\
	    __rtcheck(PL_get_arg(++index, __term, __k));		\
	    __rtcheck(PL_get_arg(++index, __term, __v));		\
	    __rtcpass(get_pair_##__unifier(__root, __k, __v, __value)); \
	}								\
    }

#define FI_get_keyid_index(__pred, __keyid, __index) {			\
	term_t __args = PL_new_term_refs(2);				\
	PL_put_term(__args, __keyid);					\
	__rtcheck(__rtctype(PL_call_predicate(NULL, PL_Q_NORMAL, __pred, __args), \
			    __keyid, "valid key of " # __pred));		\
	__rtcheck(PL_get_integer(__args+1, &__index));			\
    }

#define FI_unify_dict_t(__pred, __term, __tag) {			\
	int length=index;						\
	__rtcheck(PL_unify_functor(__term, PL_new_functor(ATOM_dict, 2*length+1))); \
	term_t __tag_t = PL_new_term_ref();				\
	__rtcheck(PL_unify_atom(__tag_t, PL_new_atom(__tag)));		\
	__rtcheck(PL_unify_arg(1, __term, __tag_t));			\
	for(index=0; index < length;) {					\
	    term_t __k;							\
	    term_t __v=dict_args+index;					\
	    FI_get_index_keyid(__pred, indexes[index], __k);		\
	    index++;							\
	    __rtcheck(PL_unify_arg(2*index,   __term, __k));		\
	    __rtcheck(PL_unify_arg(2*index+1, __term, __v));		\
	}								\
    }

#define FI_get_index_keyid(__pred, __index, __keyid) {			\
	term_t __args = PL_new_term_refs(2);				\
	__rtcheck(PL_unify_integer(__args+1, __index));			\
	__rtcheck(PL_call_predicate(NULL, PL_Q_NORMAL, __pred, __args)); \
	__keyid = __args;						\
    }

#endif // __FOREIGN_SWIPL_H__
