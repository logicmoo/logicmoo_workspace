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
    __leaf_t *__leaf=LF_PTR(__value);
    __FI_destroy_childs(__leaf, __free_);
    FI_ulink(__leaf);
    __free_(__leaf);
}

void __FI_destroy(__leaf_t *ptr, void (*__free_)(void *)) {
    FI_destroy(next, ptr, __free_);
}

#endif
