#ifndef __FOREIGN_INTERFACE_H__
#define __FOREIGN_INTERFACE_H__

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define __RTCHECK__

#ifndef TRUE
#define TRUE 1
#define FALSE 0
#endif

#if (defined(__RTCHECK__))
#define __rtcheck(__call) {						\
	int __result = (__call);					\
	if (!__result) {						\
	    fprintf(stderr, "ERROR: %s:%d: (%s) run-time check failure: " # __call "\n", \
		    __FILE__, __LINE__, __FUNCTION__);			\
	    return __result;						\
	}								\
    }

#define __doifrtc(__call) __rtcheck(__call)

#define __rtcwarn(__call) {						\
	if (!(__call)) {						\
	    fprintf(stderr, "Warning: %s:%d: (%s) run-time check failure: " # __call "\n", \
		    __FILE__, __LINE__, __FUNCTION__);			\
	}								\
    }

#define __rtcpass(__call) {	\
	int __result = (__call);	\
	if (!__result)			\
	    return __result;		\
    }

#else
#pragma GCC diagnostic ignored "-Wunused-result"
#define __rtcheck(__call) __call
#define __doifrtc(__call)
#define __rtcheck_prop(__call)
#endif

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
	__alloc(__root, sizeof(*__value), __value);	\
    }

#define FI_new_value(value)            FI_alloc_value(  __root, __malloc,          value)
#define FI_new_array(length, value)    FI_alloc_array(  __root, __malloc,  length, value)
#define FI_resize_array(length, value) FI_realloc_array(__root, __realloc, length, value)

#define FI_array_length_ptr(__value)   ((size_t *)__value-1)
#define FI_array_length(__value)       (*FI_array_length_ptr(__value))

#endif // __FOREIGN_INTERFACE_H__
