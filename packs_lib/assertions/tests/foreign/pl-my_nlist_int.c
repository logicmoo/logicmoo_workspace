#include <stdio.h>
#include <stdlib.h>
#include <foreign_interface.h>
#include <SWI-Prolog.h>
#include "lib_my_nlist_int_impl.h"

void test_nlist(root_t __root, my_nlist_int* const inp, my_nlist_int *out) {
    /* fprintf(stderr, "inp=%p\n", inp); */
    out->utype = inp->utype;
    switch (inp->utype) {
    case my_nlist_int_node:
    {
        out->node = inp->node;
        /* fprintf(stderr, "node=%d\n", out->node); */
        break;
    }
    case my_nlist_int_list:
    {
        size_t i;
        /* fprintf(stderr, "inp->list=%p\n", inp->list); */
        size_t length = FI_array_length(inp->list);
        /* fprintf(stderr, "length=%lu\n", length); */
        /* fprintf(stderr, "+out->list=%p\n", out->list); */
        FI_new_array(length, out->list);
        /* fprintf(stderr, "-out->list=%p\n", out->list); */
        for (i = 0; i < length; i++)
            test_nlist(__root, inp->list+i, out->list+i);
        break;
    }
    }
}
