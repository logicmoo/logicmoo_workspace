#ifndef __FOREIGN_INTERFACE_H__
#define __FOREIGN_INTERFACE_H__

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

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

#define __rtcpass(__call) {		\
	int __result = (__call);	\
	if (!__result)			\
	    return __result;		\
    }

#define __rtcvoid(__call) {	\
	if (!__call)		\
	    return;		\
    }

#else
#pragma GCC diagnostic ignored "-Wunused-result"
#define __rtcheck(__call) __call
#define __doifrtc(__call)
#define __rtcwarn(__call)
#endif

/* TODO: http://en.wikipedia.org/wiki/Region-based_memory_management */
/* #define __REGION_BASED_MEMORY_MANAGEMENT__ */

#ifdef __DEBUG_MALLOC__

#include <assert.h>

#define _malloc  debug_malloc
#define _realloc debug_realloc
#define _free    debug_free
#define _noleaks {fprintf(stderr, "balance=%d\n", __balance);assert(__balance==0);}
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

void debug_free(void *);

#else
#define _malloc  malloc
#define _realloc realloc
#define _free    free
#define _noleaks
#endif

#define __LINKED_NODES_MEMORY_MANAGEMENT__

#ifdef __LINKED_NODES_MEMORY_MANAGEMENT__

typedef struct __leaf_s __leaf_t;

void __FI_free(void *, void (*)(void *));
void __FI_destroy(__leaf_t **, void (*)(void *));

#define __realloc(size, value)        FI_realloc_mc(_realloc, size, value)
#define __malloc(__root, size, value) FI_malloc_mc( __root,  _malloc, size, value)
#define __free(value)                 __FI_free(value, _free)
#define __size(value)                 FI_size_mc(value)
#ifdef __GLOBAL_ROOT__
void *__root_h;
void **__root;
#define __mkroot(__root)
#else
#define __mkroot(__root)  __leaf_t *__root_h=NULL; void **__root = (void *)&__root_h
/* #define __mkroot(__root)  void **__root = NULL; */

#endif

#define __delroot(__root) { __FI_destroy((__leaf_t **)(__root), _free); _noleaks; }

#else
#define __realloc(size, value) {value = _realloc(value, size);}
#define __malloc( __root, size, value) {value = _malloc(size);}
#define __free(value) _free(value)
#define __size(value) malloc_usable_size(value)
#define __mkroot(__root)
#define __delroot(__root)
#endif

struct __leaf_s {
    __leaf_t *parent;
    void *value;
    size_t size;
};

// TODO: delete memset/3 when finish debugging

#define FI_malloc_mc(__root, __alloc, __size, __value) ({	\
	    __leaf_t *__leaf = __alloc(sizeof(__leaf_t));	\
	    void **__mem = __alloc(2*sizeof(void *)+(__size));	\
	    __leaf->value = __mem;				\
	    __leaf->size  = (__size);				\
	    __mem[0] = __leaf;					\
	    __mem[1] = NULL;					\
	    __leaf->parent = *__root;				\
	    *__root = __leaf;					\
	    __value = (void *)(__mem+2);			\
	    fprintf(stderr, "FI_malloc_mc(%p, %p) (__leaf=%p, __mem=%p)\n", __root, __value, __leaf, __mem); \
	    (typeof (__value))memset(__value, 0, (__size));	\
	})

#define FI_realloc_mc(__realloc, __size, __value) {			\
	void **__mem=(void **)__value-2;				\
	__leaf_t *__leaf = __mem[0];					\
	__mem = __realloc(__mem, 2*sizeof(void *)+(__size));		\
	__leaf->value = __mem;						\
	__value = (void *)(__mem+2);					\
	if (__leaf->size < (__size)) {					\
	    memset((void *)__value+__leaf->size, 0, (__size)-__leaf->size); \
	}								\
	__leaf->size = (__size);					\
    }

#define FI_size_mc(__value) ((*((__leaf_t **)__value-2))->size)

#define FI_alloc_value(__root, __alloc, __value) ({	\
	    __alloc(__root, sizeof(*__value), __value);	\
	})

#define FI_alloc_array(__root, __alloc, __length, __value) {		\
	void *__mem_a=NULL;						\
	__alloc(__root, sizeof(size_t)+(__length)*sizeof(*(__value)), __mem_a); \
	*((size_t *)__mem_a) = (__length);				\
	(__value) = __mem_a + sizeof(size_t);				\
    }

#define FI_realloc_array_(__realloc, __length, __value) {		\
	void *__mem_a = FI_ptr(__value);				\
	__realloc(sizeof(size_t)+(__length)*sizeof(*(__value)), __mem_a); \
	*((size_t *)__mem_a) = (__length);				\
	(__value) = __mem_a + sizeof(size_t);				\
    }

#define FI_resize_array_(__size, __length, __value) {			\
	void *__mem_a = FI_ptr(__value);				\
	assert(sizeof(size_t)+(__length)*sizeof(*(__value))<=__size(__mem_a)); \
	*((size_t *)__mem_a) = (__length);				\
    }

#define FI_foreachi(__index, __value, __array, __sentence) ({	\
	    size_t __index, __count = FI_array_length(__array);	\
	    typeof (__array) __value;				\
	    for (__index = 0; __index < __count; __index++) {	\
		__value = &(__array)[__index];			\
		__sentence;					\
	    }							\
	})

#define FI_foreach(__value, __array, __sentence) \
    FI_foreachi(__index, __value, __array, __sentence)

#ifdef __GLOBAL_ROOT__
#define FI_new_child_value(parent, value) FI_new_value(value)
#define FI_new_child_array(parent, length, value) FI_new_array(length, value)
#else

#define FI_new_child_value(parent, value) \
    FI_alloc_value(((void **)(parent)-1), __malloc, value)

#define FI_new_child_array(parent, length, value) \
    FI_alloc_array(((void **)FI_ptr(parent)-1), __malloc, length, value)

#endif

#define FI_new_value(value)              FI_alloc_value(__root, __malloc,          value)
#define FI_new_array(length, value)      FI_alloc_array(__root, __malloc,  length, value)
#define FI_realloc_array(length, value)  FI_realloc_array_(__realloc, length, value)
#define FI_resize_array(length, value)   FI_resize_array_(__size, length, value)
#define FI_delete_value(value)           __free(value)
#define FI_delete_array(__free, __value) __free(FI_ptr(__value))

#define FI_ptr(__value)                ((size_t *)__value-1)
#define FI_array_length_ptr(__value)   FI_ptr(__value)
#define FI_array_length(__value)       (*FI_array_length_ptr(__value))

#endif // __FOREIGN_INTERFACE_H__
