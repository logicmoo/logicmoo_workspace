#ifndef __FOREIGN_INTERFACE_H__
#define __FOREIGN_INTERFACE_H__

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define __RTCHECK__

#ifndef TRUE
#define TRUE  1
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

#ifdef __DEBUG_MALLOC__

#include <assert.h>

#define _malloc  debug_malloc
#define _realloc debug_realloc
#define _free    debug_free
#define _noleaks assert(__balance==0)
int __balance;

#define debug_malloc(__size) ({				\
	    void *__result=malloc(__size);		\
	    fprintf(stderr, "%d malloc(%ld)=%p\n",	\
		    __balance, __size, __result);	\
	    __balance++;				\
	    __result;					\
	})

#define debug_realloc(__value, __size) ({			\
	    void *__result=realloc(__value, __size);		\
	    fprintf(stderr, "%d realloc(%p, %ld)=%p\n",		\
		    __balance, __value, __size, __result);	\
	    __result;						\
	})

#define debug_free(__value) ({						\
	    free(__value);						\
	    __balance--;						\
	    fprintf(stderr, "%d free(%p)\n", __balance, __value);	\
	})

#else
#define _malloc  malloc
#define _realloc realloc
#define _free    free
#define _noleaks
#endif

#define __LINKED_NODES_MEMORY_MANAGEMENT__

#ifdef __LINKED_NODES_MEMORY_MANAGEMENT__
#define __realloc(__root, size, value) FI_realloc_mc(__root, _realloc, size, value)
#define __malloc( __root, size, value) FI_malloc_mc( __root,  _malloc, size, value)

#ifdef __GLOBAL_ROOT__
void *__root_h;
void **__root;
#define __mkroot(__root)
#else
#define __mkroot(__root)  void *__root_h=NULL, **__root = &__root_h

#endif

#define __delroot(__root) FI_destroy_mc(__root, _free)

#else
#define __realloc(__root, size, value) {value = _realloc(value, size);}
#define __malloc( __root, size, value) {value = _malloc(size);}
#define __mkroot(__root)
#define __delroot(__root)
#endif

typedef struct __leaf_s __leaf_t;

struct __leaf_s {
    __leaf_t *parent;
    void *value;
    size_t size;
};

// TODO: delete memset/3 when finish debugging

#define FI_malloc_mc(__root, __alloc, __size, __value) {	\
	__leaf_t *__leaf = __alloc(sizeof(__leaf_t));		\
	void **__mem = __alloc(sizeof(void *)+(__size));	\
	__leaf->value = __mem;					\
	__leaf->size  = (__size);				\
	*__mem = __leaf;					\
	__leaf->parent = *__root;				\
	*__root = __leaf;					\
	__value = (void *)(__mem+1);				\
	memset(__value, 0, (__size));				\
    }

#define FI_realloc_mc(__root, __realloc, __size, __value) {		\
	void **__mem=(void **)__value-1;				\
	__leaf_t *__leaf = *__mem;					\
	__mem = __realloc(__mem, sizeof(void *)+(__size));		\
	__leaf->value = __mem;						\
	__value = (void *)(__mem+1);					\
	if (__leaf->size < (__size)) {					\
	    memset((void *)__value+__leaf->size, 0, (__size)-__leaf->size); \
	}								\
	__leaf->size = (__size);					\
    }

#define FI_destroy_mc(__root, __free) {		\
	__leaf_t *__leaf;			\
	while (*__root) {			\
	    __leaf = *__root;			\
	    __free(__leaf->value);		\
	    *__root = __leaf->parent;		\
	    __free(__leaf);			\
	}					\
	_noleaks;				\
    }

#define FI_alloc_array(__root, __alloc, __length, __value) {		\
	void *__mem_a=NULL;						\
	__alloc(__root, sizeof(size_t) + (__length)*sizeof(*__value), __mem_a); \
	*((size_t *)__mem_a) = (__length);				\
	__value = __mem_a + sizeof(size_t);				\
    }

#define FI_realloc_array(__root, __realloc, __length, __value) {	\
	void *__mem_a = FI_array_length_ptr(__value);			\
	__realloc(__root, sizeof(size_t)+(__length)*sizeof(*__value), __mem_a); \
	*((size_t *)__mem_a) = (__length);				\
	__value = __mem_a + sizeof(size_t);				\
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
