#ifndef __FOREIGN_INTERFACE_H__
#define __FOREIGN_INTERFACE_H__

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <malloc.h>

#define __RTCHECK__

#ifndef TRUE
# define TRUE  1
# define FALSE 0
#endif

#if (defined(__RTCHECK__))
# define __rtcheck(__call) ({						\
	    int __result = (__call);					\
	    if (!__result) {						\
		fprintf(stderr, "ERROR: %s:%d: (%s) run-time check failure: " # __call "\n", \
			__FILE__, __LINE__, __FUNCTION__);		\
		return __result;					\
	    }								\
	})

# define __rtcvoid(__call) ({						\
	    if (!(__call)) {						\
		fprintf(stderr, "ERROR: %s:%d: (%s) run-time check failure: " # __call "\n", \
			__FILE__, __LINE__, __FUNCTION__);		\
		return;							\
	    }								\
	})

# define __rtcnull(__call) ({						\
	    if (!(__call)) {						\
		fprintf(stderr, "ERROR: %s:%d: (%s) run-time check failure: " # __call "\n", \
			__FILE__, __LINE__, __FUNCTION__);		\
		return NULL;						\
	    }								\
	})

# define __rtcexit(__call) ({						\
	    int __result = (__call);					\
	    if (!__result) {						\
		fprintf(stderr, "ERROR: %s:%d: (%s) run-time check failure: " # __call "\n", \
			__FILE__, __LINE__, __FUNCTION__);		\
	    }								\
	    return __result;						\
	})

# define __doifrtc(__call) __rtcheck(__call)

# define __rtcwarn(__call) ({						\
	    if (!(__call)) {						\
		fprintf(stderr, "Warning: %s:%d: (%s) run-time check failure: " # __call "\n", \
			__FILE__, __LINE__, __FUNCTION__);		\
	    }								\
	})

#else
# pragma GCC diagnostic ignored "-Wunused-result"
# define __rtcheck(__call) (__call)
# define __rtcvoid(__call) (__call)
# define __rtcnull(__call) (__call)
# define __rtcexit(__call) {return (__call);}
# define __doifrtc(__call)
# define __rtcwarn(__call)
#endif

#define __rtcpass(__call) ({		\
	    int __result = (__call);	\
	    if (!__result)		\
		return __result;	\
	})

#ifdef __DEBUG_MALLOC__

# include <assert.h>

# define _malloc  debug_malloc
# define _realloc debug_realloc
# define _free    debug_free
# define _noleaks {fprintf(stderr, "balance=%d\n", __balance);assert(__balance==0);}
int __balance;

# define debug_malloc(__size) ({						\
	    void *__result=malloc(__size);				\
	    fprintf(stderr, "%s:%d: %d malloc(%ld)=%p\n",		\
		    __FILE__, __LINE__, __balance, __size, __result);	\
	    __balance++;						\
	    __result;							\
	})

# define debug_realloc(__value, __size) ({				\
	    void *__result=realloc(__value, __size);			\
	    fprintf(stderr, "%s:%d: %d realloc(%p, %ld)=%p\n",		\
		    __FILE__, __LINE__, __balance, __value, __size, __result); \
	    __result;							\
	})

void debug_free(void * __value);

#else
# define _malloc  malloc
# define _realloc realloc
# define _free    free
# define _noleaks
#endif // __DEBUG_MALLOC__

/* TODO: http://en.wikipedia.org/wiki/Region-based_memory_management */
/* #define __REGION_BASED_MEMORY_MANAGEMENT__ */

//#define __LINKED_NODES_MEMORY_MANAGEMENT__

#ifdef __LINKED_NODES_MEMORY_MANAGEMENT__

typedef struct __leaf_s __leaf_t;
typedef __leaf_t * root_t;

void __FI_free(void *, void (*)(void *));
void __FI_destroy(__leaf_t *, void (*)(void *));

# define __realloc(size, value)        FI_realloc_mc(_realloc, size, value)
# define __malloc(__root, size, value) FI_malloc_mc( __root,  _malloc, size, value)
# define __free(value)                 __FI_free(value, _free)
# define __size(value)                 FI_size_mc(value)
# ifdef __GLOBAL_ROOT__
__leaf_t   __root_v;
__leaf_t  *__root;
#  define __mkroot(__root)
# else
#  define __mkroot(__root)	   \
  __leaf_t __root_v={NULL,NULL},   \
    *__root=&__root_v;

# endif // __GLOBAL_ROOT__

# define __delroot(__root) {	\
   __FI_destroy(__root, _free);	\
   _noleaks;			\
    }

struct __leaf_s {
    __leaf_t *next;
    __leaf_t *prev;
    void *data[];
};

# define LF_ROOT(__leaf) ((__leaf_t *)&((__leaf)->data))
# define LF_DATA(__leaf) ((void *)(LF_ROOT(__leaf)+1))
# define LF_PTR(__value) ((__leaf_t *)(__value)-2)
# define RESERVE_LEAFS 2

# ifndef __GLOBAL_ROOT__
#  define __FI_destroy_childs(__leaf, __free) __FI_destroy(LF_ROOT(__leaf), __free)
# else
#  define __FI_destroy_childs(__leaf, __free)
# endif // __GLOBAL_ROOT__

# define FI_destroy(__way, __p, __free) ({		\
	    __leaf_t *__tmp_leaf, *__leaf;		\
	    __leaf = (__p)->__way;			\
	    while(__leaf) {				\
		__FI_destroy_childs(__leaf, __free);	\
		__tmp_leaf = __leaf->__way;		\
		__free(__leaf);				\
		__leaf = __tmp_leaf;			\
	    }						\
	    (__p)->__way=NULL;				\
	})

// TODO: delete memset/3 when finish debugging

# define FI_malloc_mc(__head, __alloc, __size, __value) ({		\
	    __leaf_t *__leaf = __alloc(RESERVE_LEAFS*sizeof(__leaf_t) + (__size)); \
	    LF_ROOT(__leaf)->prev=NULL;					\
	    LF_ROOT(__leaf)->next=NULL;					\
	    __value = (typeof (__value))(LF_DATA(__leaf));		\
	    FI_link(__head, __leaf);					\
	    (typeof (__value))memset(__value, 0, (__size));		\
	})

# define FI_link(__head, __leaf) ({				\
	    __leaf->prev = (__head);				\
	    __leaf->next = (__head)->next;			\
	    if ((__head)->next) (__head)->next->prev=__leaf;	\
	    (__head)->next = __leaf;				\
	})

# define FI_ulink(__leaf) ({			\
	    __leaf_t *next = __leaf->next;	\
	    next->prev = __leaf->prev;		\
	    __leaf->prev->next = next;		\
	})

# define FI_realloc_mc(__realloc, __size, __value) ({			\
	    __leaf_t *__leaf=LF_PTR(__value);				\
	    size_t __old_size = FI_size_mc(__value);			\
	    __leaf = __realloc(__leaf, RESERVE_LEAFS*sizeof(__leaf_t) + (__size)); \
	    if (__leaf->next) __leaf->next->prev = __leaf;		\
	    if (__leaf->prev) __leaf->prev->next = __leaf;		\
	    if (LF_ROOT(__leaf)->next) LF_ROOT(__leaf)->next->prev=LF_ROOT(__leaf); \
	    if (LF_ROOT(__leaf)->prev) LF_ROOT(__leaf)->prev->next=LF_ROOT(__leaf); \
	    __value = (typeof (__value))(LF_DATA(__leaf));		\
	    if (__old_size < (__size)) {				\
		memset((void *)__value+__old_size, 0, (__size)-__old_size); \
	    }								\
	    __value;							\
	})

# define FI_size_mc(__value) (malloc_usable_size(LF_PTR(__value))-RESERVE_LEAFS*sizeof(__leaf_t))

# ifdef __GLOBAL_ROOT__
#  define FI_new_child_value(parent, value)         FI_new_value(value)
#  define FI_new_child_array(parent, length, value) FI_new_array(length, value)
# else

# define FI_new_child_value(parent, value) ({		\
	    __leaf_t *__r = LF_ROOT(LF_PTR(parent));	\
	    FI_alloc_value(__r, __malloc, value);	\
	    value;							\
	})

# define FI_new_child_array(parent, length, value) ({			\
	    __leaf_t *__r = LF_ROOT(LF_PTR(FI_array_ptr(parent)));	\
	    FI_alloc_array(__r, __malloc,  length, value);		\
	    value;							\
	})

# endif // __GLOBAL_ROOT__

#else
typedef void * root_t;
# define __realloc(size, value) ({value = _realloc(value, size);})
# define __malloc( __root, size, value) ({value = _malloc(size);})
# define __free(value) _free(value)
# define __size(value) malloc_usable_size(value)
# define __mkroot(__root) root_t __root = NULL;
# define __delroot(__root)
#endif // __LINKED_NODES_MEMORY_MANAGEMENT__


#define FI_alloc_value(__root, __alloc, __value) ({	\
	    __alloc(__root, sizeof(*__value), __value);	\
	})

#define FI_alloc_array(__head, __alloc, __length, __value) ({		\
	    void *__mem_a=NULL;						\
	    __alloc(__head, sizeof(size_t)+(__length)*sizeof(*(__value)), __mem_a); \
	    *((size_t *)__mem_a) = (__length);				\
	    (__value) = __mem_a + sizeof(size_t);			\
	})

#define FI_realloc_array_(__realloc, __length, __value) ({		\
	    void *__mem_a = FI_array_ptr(__value);			\
	    __realloc(sizeof(size_t)+(__length)*sizeof(*(__value)), __mem_a); \
	    *((size_t *)__mem_a) = (__length);				\
	    (__value) = __mem_a + sizeof(size_t);			\
	})

#define FI_resize_array_(__size, __length, __value) ({			\
	    void *__mem_a = FI_array_ptr(__value);			\
	    assert(sizeof(size_t)+(__length)*sizeof(*(__value))<=__size(__mem_a)); \
	    *((size_t *)__mem_a) = (__length);				\
	})

#define FI_foreachi(__index, __value, __array, __sentence) ({	\
	    size_t __index, __count = FI_array_length(__array);	\
	    typeof (__array) __value, __pointer;		\
	    for (__index = 0, __pointer = (__array);		\
		 __index < __count; __index++) {		\
		__value = __array + __index;			\
		__sentence;					\
	    }							\
	})

#define FI_foreach(__value, __array, __sentence) ({			\
	    size_t __count = FI_array_length(__array);			\
	    typeof (__array) __value, __upper;				\
	    for (__value = (__array), __upper = __value + __count;	\
		 __value < __upper; __value++) {			\
		__sentence;						\
	    }								\
	})

#ifdef __DEBUG_ARRAY_BOUNDS__
# define FI_array_item(__array, __index) ({				\
 	    if (!((__index)>=0&&(__index)<FI_array_length(__array)))	\
 		fprintf(stderr, "%s:%d: Warning: Array index out of bounds (%zd)\n", \
 			__FILE__, __LINE__, (ssize_t)(__index));	\
 	    assert((__index)>=0);					\
 	    assert((__index)<FI_array_length(__array));			\
 	    (__array)+(__index);					\
 	})
#else
# define FI_array_item(__array, __index) ((__array)+(__index))
#endif

#define FI_array_frst(__array) FI_array_item(__array, 0)
#define FI_array_last(__array) FI_array_item(__array, FI_array_length(__array) - 1)

#define FI_new_value(value)              FI_new_array(1, value)
#define FI_new_array(length, value)      FI_alloc_array(__root, __malloc, length, value)
#define FI_realloc_array(length, value)  FI_realloc_array_(__realloc, length, value)
#define FI_resize_array(length, value)   FI_resize_array_(__size, length, value)
#define FI_delete_value(value)           FI_delete_array(value)
#define FI_delete_array(__free, __value) __free(FI_ptr(__value))

#define FI_array_ptr(__value)            ((size_t *)__value-1)
#define FI_array_length_ptr(__value)     FI_array_ptr(__value)
#define FI_array_length(__value)         (*FI_array_length_ptr(__value))

#endif // __FOREIGN_INTERFACE_H__
