#include <stdio.h>
#include <stdlib.h>
#include <foreign_interface.h>
#include "foreign_test.h"
#include "foreign_test_i_impl.h"

void c_enum_example(enum enum_example_s const in, enum enum_example_s** io,
                    enum enum_example_s* ou, int *ix) {
    *ou = enum_example_s_element_f_g_h;
    *io = ou;
    *ix = in;
};

void c_union_example(struct uniond_s* const in, struct uniond_s** io,
                     struct uniond_s* ou, int* ix) {
    FI_new_child_array(in, 3, ou->d.dict.value2);
    ou->utype = uniond_s_d;
    ou->d.dict.value1 = "a";
    ou->d.dict.value2[0] = "b";
    ou->d.dict.value2[1] = "c";
    ou->d.dict.value2[2] = "d";
    *io = ou;
    *ix = in->utype;
}

void c_setof_enum(setof_enum_s const in, setof_enum_s **io, setof_enum_s *ou, long *ix) {
    *ou = 1<<enum_example_s_element_a | 1<<enum_example_s_element_f_g_h;
    *io = ou;
    *ix = in;
}

void  extend(int* const in, int** out) {
    int i, n = FI_array_length(in);
    *out = in;
    for (i=0;i<n;i++) {
	FI_realloc_array(n+i+1, *out);
	(*out)[n+i] = in[i] * 2;
    }
}

extern void fortran1_(const double *x, double *result);

void fortran1(double const x, double *result) {
    fortran1_(&x, result);
}

void fd1(struct d_t* const A, char **B, char **C, int **D) {
    *B=A->value1;
    *C=A->value2;
    *D=A->listv+1;
}

void fd2(root_t __root, struct d_t* A, char* const B, char* const C, int const D) {
    A->value1 = B;
    A->value2 = C;
    FI_new_array(2, A->listv);
    A->listv[0] = D;
    A->listv[1] = D*D;
}

void fd3(root_t __root, struct d_t** A, char** B, char** C, int** D) {
    FI_new_value(*A);
    (*A)->value1 = *B;
    (*A)->value2 = *C;
    (*A)->listv = *D;
}

int c_f(root_t __root, struct field_t ** field) {
    __rtcheck(field!=NULL);
    FI_new_value((*field)->sum);
    *(*field)->sum = (*field)->a + (*field)->b;
    printf("NOTE: Sum=%d\n", *(*field)->sum);
    return TRUE;
}

void c_a(struct position_t * const list_position, struct position_t * const position) {
    // nothing to do
}

void c_aa(struct position_t * const ip, struct position_t *op) {
    printf("position(%d, %d)\n", ip->x, ip->y);
    op->x = 10*ip->x;
    op->y = 10*ip->y;
}

void c_eq(int i, int *o) {
  *o = i;
}

void c_idx(double * const A, int const B, double *C) {
    *C = A[B];
}

void c_numl(root_t __root, int const A, double ** B) {
    int index;
    FI_new_array(A, *B);
    for (index = 0; index < A; index++) {
	(*B)[index]=index+1;
    }
    // nothing to do yet
}

void c_get_arrays(root_t __root, int const size, double ****A, int ***B, int **C) {
    int index1, index2, index3;
    FI_new_array(size, *A);
    FI_new_array(size, *B);
    FI_new_array(size, *C);
    for (index1 = 0; index1 < size; index1++) {
	FI_new_array(size, (*A)[index1]);
	FI_new_array(size, (*B)[index1]);
	(*C)[index1] = index1;
	for (index2 = 0; index2 < size; index2 ++) {
	    FI_new_array(size, ((*A)[index1])[index2]);
	    (*B)[index1][index2] = index1*size+index2;
	    for (index3 = 0; index3 < size; index3 ++) {
		(*A)[index1][index2][index3] = (index1*size+index2)*size+index3;
	    }
	}
    }
}

void show_arrays(double *** const A, int ** const B, int * const C) {
    int index1, index2, index3;
    printf("\n");
    for (index1 = 0; index1 < FI_array_length(A); index1 ++) {
	for (index2 = 0; index2 < FI_array_length(A[index1]); index2++) {
	    for (index3 = 0; index3 < FI_array_length(A[index1][index2]); index3++)
		printf("A[%d,%d,%d]=%lf ",index1,index2,index3,A[index1][index2][index3]);
            printf("\n");
        }
    }
    for (index1 = 0; index1 < FI_array_length(B); index1 ++) {
	for (index2 = 0; index2 < FI_array_length(B[index1]); index2++)
	    printf("B[%d,%d]=%d ",index1,index2,B[index1][index2]);
        printf("\n");
    }
    for (index1 = 0; index1 < FI_array_length(C); index1 ++) {
	printf("C[%d]=%d ",index1,C[index1]);
    }
    printf("\n");
}

void c_io(int ** I) {
    if (*I==NULL) {
	printf("NOTE: io/1 called with a null\n");
    } else {
	printf("NOTE: io/1 called with %d\n", **I);
    }
}

int c_sio(root_t __root, int **I) {
    if (*I==NULL) {
	FI_new_value(*I);
	**I = 510;
	return TRUE;
    } else
	return FALSE;    
}

void  c_pq(struct position_t** A) {
  // nothing to do
}

int is_positive_t(int const A) {
  return A>0;
}

int is_negative_t(negative_t const A) {
    return A<0;
}

void  fce(struct contain_extern_t* const A, struct contain_extern_t* B) {
  B->idx=A->idx;
  B->value=A->value - 2;
  printf("ce[%d, %d]\n", A->idx, A->value);
}

void  fco(struct contain_opaque_t* const A, struct contain_opaque_t* B) {
  B->idx=A->idx;
  B->value=A->value + 2;
  printf("co[%d, %d]\n", A->idx, A->value);
}

void test_ireverse1(root_t __root, int* const var_1, int** var_2) {
  ireverse1(__root, var_1, var_2);
}


void test_ireverse2(int* const var_1, int** var_2) {
  ireverse2(var_1, var_2);
}
