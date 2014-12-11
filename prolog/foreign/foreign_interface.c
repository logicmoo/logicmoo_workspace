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
__leaf_t  __root_v={NULL,NULL,NULL};
__leaf_t  *__root=&__root_v;
#endif

#ifdef __LINKED_NODES_MEMORY_MANAGEMENT__

void __FI_free(void *__value, void (*__free_)(void *)) {
    fprintf(stderr, "-__FI_free(%p)\n", __value);
    __leaf_t **__mem=(void *)__value-2;
    __FI_destroy(*((__leaf_t **)(__mem+1)), __free_);
    (*__mem)->value = NULL;
    __free_(__mem);
}

void __FI_destroy(__leaf_t *ptr, void (*__free_)(void *)) {
    fprintf(stderr, "__FI_destroy(%p)\n", ptr);
    FI_destroy(prev, ptr, __free_);
    FI_destroy(next, ptr, __free_);
}

#endif
