#include <stdio.h>
#include <stdlib.h>
#include <foreign_interface.h>
#include "foreign_test_i_impl.h"

int c_f(field_t ** field) {
    __rtcheck(field!=NULL);
    (*field)->sum = malloc(sizeof(*(*field)->sum));
    *(*field)->sum = (*field)->a + (*field)->b;
    printf("NOTE: Sum=%d\n", *(*field)->sum);
    return TRUE;
}

void c_a(position_t * const list_position, position_t * const position) {
    // nothing to do
}

void c_aa(position_t * const ip, position_t *op) {
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

void c_numl(void **__root, int const A, double ** B) {
    int index;
    FI_new_array(A, *B);
    for (index = 0; index < A; index++) {
	(*B)[index]=index+1;
    }
    // nothing to do yet
}

void c_get_arrays(void **__root, int const size, double ****A, int ***B, int **C) {
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
    for (index1 = 0; index1 < FI_array_length(A); index1 ++) {
	for (index2 = 0; index2 < FI_array_length(A[index1]); index2++)
	    for (index3 = 0; index3 < FI_array_length(A[index1][index2]); index3++)
		printf("A[%d,%d,%d]=%lf\n",index1,index2,index3,A[index1][index2][index3]);
    }
    for (index1 = 0; index1 < FI_array_length(B); index1 ++) {
	for (index2 = 0; index2 < FI_array_length(B[index1]); index2++)
	    printf("B[%d,%d]=%d\n",index1,index2,B[index1][index2]);
    }
    for (index1 = 0; index1 < FI_array_length(C); index1 ++) {
	printf("C[%d]=%d\n",index1,C[index1]);
    }
}

void c_io(int ** I) {
    if (*I==NULL) {
	printf("NOTE: io/1 called with a null\n");
    } else {
	printf("NOTE: io/1 called with %d\n", **I);
    }
}

int c_sio(void **__root, int **I) {
    if (*I==NULL) {
	FI_new_value(*I);
	**I = 510;
	return TRUE;
    } else
	return FALSE;    
}

void  c_pq(position_t** A) {
  // nothing to do
}
