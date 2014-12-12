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
    __leaf_t *__leaf=(__leaf_t *)__value-1;
    __FI_destroy_childs(__leaf->root, __free_);
    fprintf(stderr, "-__FI_free(%p)\n", __value);
    __leaf_t *prev = __leaf->next;
    prev->prev = __leaf->prev;
    __leaf->prev->next = prev;
    __free_(__leaf);
}

void __FI_destroy(__leaf_t *ptr, void (*__free_)(void *)) {
    fprintf(stderr, "__FI_destroy(%p)\n", ptr);
    FI_destroy(next, ptr, __free_);
    /* FI_destroy(prev, ptr, __free_); */
}

#endif
