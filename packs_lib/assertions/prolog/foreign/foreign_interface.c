/*  Part of Assertion Reader for SWI-Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/assertions
    Copyright (C): 2017, Process Design Center, Breda, The Netherlands.
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

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
