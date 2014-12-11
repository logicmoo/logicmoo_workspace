#include "foreign_interface.h"

#ifdef __DEBUG_MALLOC__
int __balance=0;

void debug_free(void * __value) {
    free(__value);
    __balance--;
    fprintf(stderr, "%d free(%p)\n", __balance, __value);
}

#endif

#ifdef __GLOBAL_ROOT__
void *__root_h=NULL;
void **__root=&__root_h;
#endif

#ifdef __LINKED_NODES_MEMORY_MANAGEMENT__

void __FI_free(void *__value, void (*__free_)(void *)) {
    fprintf(stderr, "-__FI_free(%p)\n", __value);
    __leaf_t **__mem=(__leaf_t **)__value-2;
    __FI_destroy(__mem+1, __free_);
    (*__mem)->value = NULL;
    (*__mem)->size  = 0;
    __free_(__mem);
}

void __FI_destroy(__leaf_t **ptr, void (*__free_)(void *)) {
    fprintf(stderr, "__FI_destroy(%p)\n", *ptr);
    __leaf_t *__tmp_leaf, *__leaf = *ptr;
    while(__leaf) {
	if(__leaf->value!=NULL) {
#ifndef __GLOBAL_ROOT__
            __FI_destroy((__leaf_t **)(__leaf->value)+1, __free_);
#endif
	    __free_(__leaf->value);
	    /* __FI_free((void **)(__leaf->value)+2, __free_); */
	    /* fprintf(stderr, "+__FI_free(%p)\n", __leaf->value+2); */
	}
	__tmp_leaf = __leaf->parent;
	fprintf(stderr, "+__free_(%p)\n", __leaf);
	__free_(__leaf);
	__leaf = __tmp_leaf;
    }
    *ptr = __leaf;
}

#endif
