#ifndef __FOREIGN_INTERFACE_H__
#define __FOREIGN_INTERFACE_H__

#include <SWI-Prolog.h>
#include <stdio.h>

#define __RTCHECK__

#if (defined(__RTCHECK__))
#define __rtcheck(__call)						\
    if (!(__call)) {							\
	fprintf(stderr, "ERROR: %s:%d: (%s) run-time check failure: " # __call "\n",	\
		__FILE__, __LINE__, __FUNCTION__);			\
	return FALSE;							\
    }

#define __rtcheck_warn(__call)						\
    if (!(__call)) {							\
	fprintf(stderr, "Warning: %s:%d: (%s) run-time check failure: " # __call "\n", \
		__FILE__, __LINE__, __FUNCTION__);			\
    }

#define __rtcheck_prop(__call) __rtcheck(__call)

#define __rtcheck_type(__call, __term, __type)		\
    if (!(__call)) {					\
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
    }

#else
#pragma GCC diagnostic ignored "-Wunused-result"
#define __rtcheck(__call) __call
#define __rtcheck_prop(__call)
#define __rtcheck_type(__call, __term, __type) __call
#endif

#define __rtc_PL_unify(__type, __term, __value)				\
    __rtcheck_type(PL_unify_##__type(__term, __value), __term, __type)

#define __rtc_PL_get(__type, __term, __value)				\
    __rtcheck_type(PL_get_##__type(__term, __value), __term, __type)

#define __rtc_PL_get_t(__type, __term, __value)				\
    __rtcheck_type(PL_get_##__type(__root, __term, __value), __term, __type)


/* TODO: http://en.wikipedia.org/wiki/Region-based_memory_management */
/* #define __REGION_BASED_MEMORY_MANAGEMENT__ */

#define __LINKED_NODES_MEMORY_MANAGEMENT__

#ifdef __LINKED_NODES_MEMORY_MANAGEMENT__
#define __realloc(__root, size, value) FI_realloc_mc(__root, realloc, size, value)
#define __malloc( __root, size, value) FI_malloc_mc( __root,  malloc, size, value)
#define __mkroot(__root)  void *__root_h=NULL, **__root = &__root_h
#define __delroot(__root) FI_destroy_mc(__root, free)
#else
#define __realloc(__root, size, value) {value = realloc(value, size);}
#define __malloc( __root, size, value) {value = malloc(size);}
#define __mkroot(__root)
#define __delroot(__root)
#endif

#define FI_prev_mc_ptr(value) ((void **)value - 1)
#define FI_prev_mc(value)     (*FI_prev_mc_ptr(value))

#define FI_destroy_mc(__root, __free) {		\
	void *__prev_mc;			\
	while(*__root) {			\
	    __prev_mc = FI_prev_mc(*__root);	\
	    __free(FI_prev_mc_ptr(*__root));	\
	    *__root = __prev_mc;		\
	}					\
    }

#define FI_realloc_mc(__root, __realloc, __size, __value) {		\
	void **__curr_mc_ptr = __root;					\
	void * __mem = NULL;						\
	while(*__curr_mc_ptr) {						\
	    if (*__curr_mc_ptr == __value) {				\
		void * __mem = __realloc(FI_prev_mc_ptr(*__curr_mc_ptr), \
					 sizeof(void *) + __size)	\
		    + sizeof(void *);					\
		*__curr_mc_ptr = __mem;					\
		break;							\
	    }								\
	    __curr_mc_ptr = FI_prev_mc_ptr(*__curr_mc_ptr);		\
	}								\
	__value = __mem;						\
    }

#define FI_malloc_mc(__root, __alloc, __size, __value) {	\
	void *__mem = __alloc(sizeof(void *) + __size);		\
	*((void **)__mem) = *__root;				\
	*__root = (void **)__mem + 1;				\
	__value = *__root;					\
    }

#define FI_alloc_array(__root, __alloc, __length, __value) {		\
	void *__mem_a=NULL;						\
	__alloc(__root, sizeof(size_t) + __length*sizeof(*__value), __mem_a); \
	*((size_t *)__mem_a) = __length;				\
	__value = __mem_a + sizeof(size_t);				\
    }

#define FI_realloc_array(__root, __realloc, __length, __value) {	\
	void *__mem = FI_array_length_ptr(__value);			\
	__realloc(__root, sizeof(size_t)+__length*sizeof(*__value), __mem); \
	*((size_t *)__mem) = __length;					\
	__value = __mem + sizeof(size_t);				\
    }

#define FI_alloc_value(__root, __alloc, __value) {	\
	__alloc(__root, sizeof(__value), __value);	\
    }

#define FI_new_value(value)            FI_alloc_value(  __root, __malloc,          value)
#define FI_new_array(length, value)    FI_alloc_array(  __root, __malloc,  length, value)
#define FI_resize_array(length, value) FI_realloc_array(__root, __realloc, length, value)

#define FI_array_length_ptr(__value)   ((size_t *)__value-1)
#define FI_array_length(__value)       (*FI_array_length_ptr(__value))

#define PL_get_array(__PL_get_elem, __term, __value) {			\
	term_t __term##_ = PL_new_term_ref();				\
	term_t __tail = PL_copy_term_ref(__term);			\
	size_t __length = 0;						\
	while( PL_get_list(__tail, __term##_, __tail) ) __length++;	\
	__tail = PL_copy_term_ref(__term);				\
	FI_new_array(__length, *__value);				\
	typeof (*__value) _c_##__term##_ = *__value;			\
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
	for(__index = 0; __index < __length; __index++)			\
	{								\
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

#endif // __FOREIGN_INTERFACE_H__
