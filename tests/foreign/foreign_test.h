#ifndef __foreign_test_H
#define __foreign_test_H

typedef int positive_t;

typedef int negative_t;

#define PL_get_positive_t(__root, __term, __value) {	\
    PL_get_integer(__term, __value) &&			\
      is_positive_t(*__value);				\
  }

#define PL_unify_positive_t(__term, __value) {		\
    PL_unify_integer(__term, *__value) &&		\
      is_positive_t(*__value);				\
  }

#define PL_get_negative_t(__root, __term, __value) {	\
    PL_get_integer(__term, __value) &&			\
      is_negative_t(__value);				\
  }

#define PL_unify_negative_t(__term, __value) {		\
    PL_unify_integer(__term, *__value) &&		\
      is_negative_t(__value);				\
  }


#endif
