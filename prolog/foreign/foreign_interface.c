#include "foreign_interface.h"

#ifdef __DEBUG_MALLOC__
int __balance=0;
#endif

#ifdef __GLOBAL_ROOT__
void *__root_h=NULL;
void **__root=&__root_h;
#endif
