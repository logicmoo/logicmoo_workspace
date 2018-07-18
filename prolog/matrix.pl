:- module(matrix,
          [ matrix_write/1,
            matrix_new/3,
            matrix_new/4,
            matrix_mul/3,
            matrix_dims/2,
            matrix_size/2,
            matrix_type/2,
            matrix_to_list/2,
            matrix_get/3,
            matrix_set/3,
            matrix_set_all/2,
            matrix_add/3,
            matrix_inc/2,
            matrix_inc/3,
            matrix_dec/2,
            matrix_dec/3,
            matrix_max/2,
            matrix_min/2,
            matrix_sum/2,
            matrix_transpose/2,
            matrix_maxarg/2,
            matrix_minarg/2,
            matrix_agg_lines/2,
            matrix_agg_cols/2,
            matrix_op/4,
            matrix_op_to_all/4,
            matrix_select/4,
            matrix_createZeroes/3,
            matrixSetZeroes/3,
            matrixSetOnes/3,
            matrix_read/2,
            matrix_setAllNumbers/3,
            matrix_eye/3,
            matrix_equal/2,
            matrix_map/3,
            matrix_foreach/2,
            matrix_foldl/3,
            matrix_from_list/2
          ]).

:- use_module(library(ffi)).
:- use_module(library(lambda)).



% https://www.dcc.fc.up.pt/~vsc/Yap/documentation.html#matrix
:- c_import("#include <atlas/cblas.h>",
            [libblas],
            
            [ cblas_dgemm(int,
                          int,
                          int,
                          int,
                          int,
                          int,
                          double,
                          *double,
                          int,
                          *double,
                          int,
                          double,
                          *double,
                          int)
            ]).

:- (multifile user:term_expansion/2).
:- (dynamic user:term_expansion/2).
                

user:term_expansion((:-import),  (:-Import)) :-
    current_prolog_flag(arch, Arch),
    atom_concat('../lib/', Arch, A1),
    atom_concat(A1, '/matrixNative', A2),
    Import=c_import("#include \"../matrixNative.h\"", [A2], [matrixSetAll(int, int, *double, double), matrixEye(int, int, *double)]).


:- import.
    
%!  matrix_write(+M1)
% Print elements of a matrix
matrix_write(matrix(_, [NRows, NColumns], Matrix)) :-
    write_matrix(Matrix, NRows, NColumns).

write_matrix(Matrix, NRows, NColumns) :-
    write_matrix(Matrix, 0, 0, NRows, NColumns).

write_matrix(_, NRows, _, NRows, _) :-
    !.

write_matrix(MatrixElements, CurrentRow, NColumns, NRows, NColumns) :-
    !,
    writeln(''),
    CurrentRow1 is CurrentRow+1,
    write_matrix(MatrixElements, CurrentRow1, 0, NRows, NColumns).

write_matrix(MatrixElements, CurrentRow, CurrentColumn, NRows, NColumns) :-
    !,
    Index is CurrentRow*NColumns+CurrentColumn,
    c_load(MatrixElements[Index], Element),
    write(Element),
    write(' '),
    CurrentColumn1 is CurrentColumn+1,
    write_matrix(MatrixElements, CurrentRow, CurrentColumn1, NRows, NColumns).

%!  matrix_new(+Type,+Dimension,-M1)
% Create a new matrix. In the matrix it must be specified Type (ints,floats,doubles) and the dimension with Nrows and Ncolumns. The matrix will not be initialised.
matrix_new(doubles, [NRows, NColumns], matrix(doubles, [NRows, NColumns], Matrix)) :-
    NRows>0,
    NColumns>0,
    Nelements is NRows*NColumns,
    c_alloc(Matrix, double[Nelements]).

%!  matrix_new(+Type,+Dimension,+Elements,-M1)
% Create a new matrix. In the matrix it must be specified Type (ints,floats,doubles) and the dimension with NRows and NColumns. The matrix will be initialised with elements specified in a list.
matrix_new(doubles, [NRows, NColumns], ListElements, matrix(doubles, [NRows, NColumns], Matrix)) :-
    NRows>0,
    NColumns>0,
    NElements is NRows*NColumns,
    length(ListElements, NElements),
    c_alloc(Matrix, double[]=ListElements).

%!  matrix_mul(+M1,+M2,-M3)
% Multiplication between two matrices using "cblas_dgemm" function from "cblas.h"
matrix_mul(matrix(Type, [NRowsMatrix1, NColumnsMatrix1], Matrix1), matrix(Type, [NColumnsMatrix1, NColumnsMatrix2], Matrix2), matrix(Type, [NRowsMatrix1, NColumnsMatrix2], Matrix3)) :-
    matrix_new(Type,
               [NRowsMatrix1, NColumnsMatrix2],
               matrix(Type, [NRowsMatrix1, NColumnsMatrix2], Matrix3)),
    cblas_dgemm(102,
                111,
                111,
                NRowsMatrix1,
                NColumnsMatrix2,
                NColumnsMatrix1,
                1,
                Matrix1,
                NRowsMatrix1,
                Matrix2,
                NColumnsMatrix1,
                0,
                Matrix3,
                NRowsMatrix1).

%!  matrix_dims(+M1,-Dimension)
% Calculates the total number of elements in the matrix. The value is contained in Dimension.
matrix_dims(matrix(_, Dimension, _), Dimension).

%!  matrix_size(+M1,-Nelements)
% Calculates the number of elements in the matrix. We have to pass the number of the rows and the number of the column.
matrix_size(matrix(_, [NRows, NColumns], _), Nelements) :-
    Nelements is NRows*NColumns.

%!  matrix_type(+M1,-Type)
% Function that return the Type of the matrix passed. 
matrix_type(matrix(Type, _, _), Type).

%!  matrix_to_list(+M1,-List)
% Function that transforms the matrix passed in input in a list. 
to_list(_, NRows, _, NRows, _, List) :-
    !,
    List=([]).

to_list(MatrixElements, CurrentRow, NColumns, Nrows, NColumns, List) :-
    !,
    CurrentRow1 is CurrentRow+1,
    to_list(MatrixElements, CurrentRow1, 0, Nrows, NColumns, List).

to_list(MatrixElements, CurrentRow, CurrentColumn, NRows, NColumns, [Element|MoreElements]) :-
    Index is CurrentRow*NColumns+CurrentColumn,
    c_load(MatrixElements[Index], Element),
    CurrentColumn1 is CurrentColumn+1,
    to_list(MatrixElements, CurrentRow, CurrentColumn1, NRows, NColumns, MoreElements).

matrix_to_list(matrix(_, [NRows, NColumns], MatrixElements), List) :-
    to_list(MatrixElements, 0, 0, NRows, NColumns, List).

%!  matrix_get(+M1,+Position,-Element)
% matrix_get(M,[Row,Column],E) unifies E with the element of M at position [Row, Column] 
matrix_get(matrix(_, [_, NColumns], MatrixElements), [Row, Column], Element) :-
    Index is Row*NColumns+Column,
    c_load(MatrixElements[Index], Element).

%!  matrix_set(+Type,+Position,-Element)
% matrix_set(M,[Row,Column],E) update the element of M at position [Row, Column] with the element specified in Element.
matrix_set(matrix(_, [_, NColumns], MatrixElements), [Row, Column], Element) :-
    Index is Row*NColumns+Column,
    c_store(MatrixElements[Index], Element).

%!  matrix_map(+Predicate,+M1,-M2)
% Function that return true if Predicate can successfully be applied on all elements of M1. 
matrix_map(Predicate, matrix(Type, [NRows, NColumns], MatrixElements1), matrix(Type, [NRows, NColumns], MatrixElements2)) :-
    matrix_new(Type,
               [NRows, NColumns],
               matrix(_, _, MatrixElements2)),
    matrix_map(Predicate, NRows, NColumns, MatrixElements1, MatrixElements2, 0, 0).

matrix_map(_, NRows, _, _, _, NRows, _) :-
    !.

matrix_map(Predicate, NRows, NColumns, MatrixElements1, MatrixElements2, CurrentRow, NColumns) :-
    !,
    CurrentRow1 is CurrentRow+1,
    matrix_map(Predicate, NRows, NColumns, MatrixElements1, MatrixElements2, CurrentRow1, 0).

matrix_map(Predicate, NRows, NColumns, MatrixElements1, MatrixElements2, CurrentRow, CurrentColumn) :-
    Index is CurrentRow*NColumns+CurrentColumn,
    c_load(MatrixElements1[Index], E1),
    call(Predicate, E1, E2),
    c_store(MatrixElements2[Index], E2),
    CurrentColumn1 is CurrentColumn+1,
    matrix_map(Predicate,
               NRows,
               NColumns,
               MatrixElements1,
               MatrixElements2,
               CurrentRow,
               CurrentColumn1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%       TWO FUNCTIONS USED BY MATRIX FOREACH       %%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Unify the element with the number specified.
myset(_, Number, Number).

myset(_, 0).
% This function find the max number between two number.
findMax(FirstNumber, SecondNumber, FirstNumber) :-
    FirstNumber>=SecondNumber,
    !.

findMax(_, SecondNumber, SecondNumber).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%!  matrix_foreach(+M1,+Predicate)
% For each element E of M1, enumerated by rows, calls Predicate(+E,-F) and replaces E with F.
matrix_foreach(matrix(_, [NRows, NColumns], MatrixElements), Predicate) :-
    matrix_foreach(Predicate, NRows, NColumns, MatrixElements, 0, 0).

matrix_foreach(_, NRows, _, _, NRows, _) :-
    !.

matrix_foreach(Predicate, NRows, NColumns, MatrixElements, CurrentRow, NColumns) :-
    !,
    CurrentRow1 is CurrentRow+1,
    matrix_foreach(Predicate, NRows, NColumns, MatrixElements, CurrentRow1, 0).

matrix_foreach(Predicate, NRows, NColumns, MatrixElements, CurrentRow, CurrentColumn) :-
    Index is CurrentRow*NColumns+CurrentColumn,
    c_load(MatrixElements[Index], E),
    call(Predicate, E, E1),
    c_store(MatrixElements[Index], E1),
    CurrentColumn1 is CurrentColumn+1,
    matrix_foreach(Predicate,
                   NRows,
                   NColumns,
                   MatrixElements,
                   CurrentRow,
                   CurrentColumn1).

%!  matrix_foldl(+M1,+Predicate,-Max)
% Fold a matrix, using arguments of the matrix as left argument.
matrix_foldl(matrix(_, [NRows, NColumns], MatrixElements), Predicate, Max) :-
    c_load(MatrixElements[0], TmpMax),
    matrix_start_foldl(Predicate,
                       NRows,
                       NColumns,
                       MatrixElements,
                       TmpMax,
                       Max).

matrix_start_foldl(_, 1, 1, _, Max, Max) :-
    !.
matrix_start_foldl(Predicate, NRows, NColumns, MatrixElements, Tmp, Max) :-
    NColumns>1,
    !,
    matrix_foldl(Predicate,
                 NRows,
                 NColumns,
                 MatrixElements,
                 0,
                 1,
                 Tmp,
                 Max).

matrix_start_foldl(Predicate, NRows, NColumns, MatrixElements, Tmp, Max) :-
    matrix_foldl(Predicate,
                 NRows,
                 NColumns,
                 MatrixElements,
                 1,
                 0,
                 Tmp,
                 Max).

matrix_foldl(_, NRows, _, _, NRows, _, Max, Max) :-
    !.

matrix_foldl(Predicate, NRows, NColumns, MatrixElements, CurrentRow, NColumns, TmpMax, Max) :-
    !,
    CurrentRow1 is CurrentRow+1,
    matrix_foldl(Predicate,
                 NRows,
                 NColumns,
                 MatrixElements,
                 CurrentRow1,
                 0,
                 TmpMax,
                 Max).

matrix_foldl(Predicate, NRows, NColumns, MatrixElements, CurrentRow, CurrentColumn, TmpMax, Max) :-
    Index is CurrentRow*NColumns+CurrentColumn,
    c_load(MatrixElements[Index], E),
    call(Predicate, TmpMax, E, TmpMax1),
    CurrentColumn1 is CurrentColumn+1,
    matrix_foldl(Predicate,
                 NRows,
                 NColumns,
                 MatrixElements,
                 CurrentRow,
                 CurrentColumn1,
                 TmpMax1,
                 Max).

%!  matrix_set_all(+Number,+M1)
% matrix_set_all set all element of the matrix to Element.
set_all(_, NRows, _, _, NRows, _) :-
    !.

set_all(Number, NRows, NColumns, MatrixElements, CurrentRow, NColumns) :-
    !,
    CurrentRow1 is CurrentRow+1,
    set_all(Number, NRows, NColumns, MatrixElements, CurrentRow1, 0).

set_all(Number, NRows, NColumns, MatrixElements, CurrentRow, CurrentColumn) :-
    Index is CurrentRow*NColumns+CurrentColumn,
    c_store(MatrixElements[Index], Number),
    CurrentColumn1 is CurrentColumn+1,
    set_all(Number, NRows, NColumns, MatrixElements, CurrentRow, CurrentColumn1).

matrix_set_all(number(Type, Number), matrix(Type, [NRows, NColumns], Element)) :-
    set_all(Number, NRows, NColumns, Element, 0, 0).

%!  matrix_add(+M1,Position,+Number)
% matrix_add(M,[Row,Column],Number) add Number to the element of matrix M in position [Row,Column].
matrix_add(matrix(_, [_, NColumns], MatrixElements), [Row, Column], Number) :-
    Index is Row*NColumns+Column,
    c_load(MatrixElements[Index], Tmp),
    NewElement is Tmp+Number,
    c_store(MatrixElements[Index], NewElement).

%!  matrix_inc(+M1,+Position)
% matrix_inc(M,[Row,Column]) add 1 (one) to the element of matrix M in position [Row,Column].
matrix_inc(matrix(_, [_, NColumns], MatrixElements), [Row, Column]) :-
    Index is Row*NColumns+Column,
    c_load(MatrixElements[Index], Tmp),
    E is Tmp+1,
    c_store(MatrixElements[Index], E).

%!  matrix_inc(+M1,+Position,-NewElement)
% matrix_inc(M,[Row,Column],NewElement) add 1 (one) to the element of matrix M in position [Row,Column] and unify the new element with NewElement.
matrix_inc(matrix(_, [_, NColumns], MatrixElements), [Row, Column], NewElement) :-
    Index is Row*NColumns+Column,
    c_load(MatrixElements[Index], Tmp),
    NewElement is Tmp+1,
    c_store(MatrixElements[Index], NewElement).

%!  matrix_dec(+M1,+Position)
% matrix_dec(M,[Row,Column]) decrement 1 (one) to the element of matrix M in position [Row,Column].
matrix_dec(matrix(_, [_, NColumns], MatrixElements), [Row, Column]) :-
    Index is Row*NColumns+Column,
    c_load(MatrixElements[Index], Tmp),
    NewElement is Tmp-1,
    c_store(MatrixElements[Index], NewElement).

%!  matrix_dec(+M1,+Position,-NewElement)
% matrix_dec(M,[Row,Column],NewElement) decrement 1 (one) to the element of matrix M in position [Row,Column] and unify the new element with NewElement.
matrix_dec(matrix(_, [_, NColumns], MatrixElements), [Row, Column], NewElement) :-
    Index is Row*NColumns+Column,
    c_load(MatrixElements[Index], Tmp),
    NewElement is Tmp-1,
    c_store(MatrixElements[Index], NewElement).

%!  matrix_max(+M1,-Max)
% matrix_max(M,Max) unify Max with the maximum element of the matrix M.
max_matrix(NRows, _, _, NRows, _, Max, Max) :-
    !.

max_matrix(NRows, NColumns, MatrixElements, CurrentRow, NColumns, TmpMax, Max) :-
    !,
    CurrentRow1 is CurrentRow+1,
    max_matrix(NRows, NColumns, MatrixElements, CurrentRow1, 0, TmpMax, Max).

max_matrix(NRows, NColumns, MatrixElements, CurrentRow, CurrentColumn, TmpMax, Max) :-
    Index is CurrentRow*NColumns+CurrentColumn,
    c_load(MatrixElements[Index], E),
    CurrentColumn1 is CurrentColumn+1,
    (   E>TmpMax
    ->  TmpMax1 is E
    ;   TmpMax1 is TmpMax
    ),
    max_matrix(NRows,
               NColumns,
               MatrixElements,
               CurrentRow,
               CurrentColumn1,
               TmpMax1,
               Max).

matrix_max(matrix(_, [NRows, NColumns], MatrixElements), Max) :-
    c_load(MatrixElements[0], TmpMax),
    max_matrix(NRows, NColumns, MatrixElements, 0, 0, TmpMax, Max).

%!  matrix_min(+M1,-Min)
% matrix_min(M,Min) unify Min with the minimum element of the matrix M.
matrix_min(Matrix, Min) :-
    matrix_foldl(Matrix,
                 \X^Y^Z^(X=<Y->Z=X;Z=Y),
                 Min).

%!  matrix_sum(+M1,-Sum)
% matrix_sum(M,Sum) unify Sum with the sum of the elements in M.
matrix_sum(Matrix, Sum) :-
    matrix_foldl(Matrix,
                 \CurrentSum^Element^NewSum^(NewSum is CurrentSum+Element),
                 Sum).

%!  matrix_transpose(+M1,-M2)
% matrix_transpose(M1,M2) transpose matrix M1 to M2.
transpose_matrix(NRows, _, _, _, NRows, _) :-
    !.

transpose_matrix(NRows, NColumns, MatrixElements1, MatrixElements2, CurrentRow, NColumns) :-
    !,
    CurrentRow1 is CurrentRow+1,
    transpose_matrix(NRows, NColumns, MatrixElements1, MatrixElements2, CurrentRow1, 0).

transpose_matrix(NRows, NColumns, MatrixElements1, MatrixElements2, CurrentRow, CurrentColumn) :-
    Index is CurrentRow*NColumns+CurrentColumn,
    IndexMatrix2 is CurrentColumn*NRows+CurrentRow,
    c_load(MatrixElements1[Index], E),
    c_store(MatrixElements2[IndexMatrix2], E),
    CurrentColumn1 is CurrentColumn+1,
    transpose_matrix(NRows,
                     NColumns,
                     MatrixElements1,
                     MatrixElements2,
                     CurrentRow,
                     CurrentColumn1).


matrix_transpose(matrix(Type, [NRows, NColumns], MatrixElements1), Matrix2) :-
    matrix_new(Type, [NColumns, NRows], Matrix2),
    Matrix2=matrix(_, _, MatrixElements2),
    transpose_matrix(NRows, NColumns, MatrixElements1, MatrixElements2, 0, 0).

%!  matrix_maxarg(+M1,-MaxPosition)
% matrix_maxarg(M,[NRowMax, NColumnMax]) unify [NRowsMax, NColumnMax] with the position of the maximum in matrix M. 
maxarg_matrix(NRows, _, _, NRows, _, _, [NRowsMax, NColumnsMax], [NRowsMax, NColumnsMax]) :-
    !.

maxarg_matrix(NRows, NColumns, MatrixElements, CurrentRow, NColumns, TmpMax, [NRowTmp, NColumnTmp], [NRowMax, NColomnMax]) :-
    !,
    CurrentRow1 is CurrentRow+1,
    maxarg_matrix(NRows,
                  NColumns,
                  MatrixElements,
                  CurrentRow1,
                  0,
                  TmpMax,
                  [NRowTmp, NColumnTmp],
                  [NRowMax, NColomnMax]).

                maxarg_matrix(NRows, NColumns, MatrixElements, CurrentRow, CurrentColumn, TmpMax, [NRowTmp, NColumnTmp], [NRowMax, NColomnMax]) :-
    Index is CurrentRow*NColumns+CurrentColumn,
    c_load(MatrixElements[Index], Element),
    CurrentColumn1 is CurrentColumn+1,
    (   Element>=TmpMax
    ->  ( TmpMax1 is Element,
          NRowTmp1 is CurrentRow,
          NColumnTmp1 is CurrentColumn
        ),
        maxarg_matrix(NRows,
                      NColumns,
                      MatrixElements,
                      CurrentRow,
                      CurrentColumn1,
                      TmpMax1,
                      [NRowTmp1, NColumnTmp1],
                      [NRowMax, NColomnMax])
    ;   TmpMax1 is TmpMax,
        NRowTmp1 is NRowTmp,
        NColumnTmp1 is NColumnTmp,
        maxarg_matrix(NRows,
                      NColumns,
                      MatrixElements,
                      CurrentRow,
                      CurrentColumn1,
                      TmpMax1,
                      [NRowTmp1, NColumnTmp1],
                      [NRowMax, NColomnMax])
    ).

matrix_maxarg(matrix(_, [NRows, NColumns], MatrixElements), [NRowMax, NColomnMax]) :-
    c_load(MatrixElements[0], TmpMax),
    maxarg_matrix(NRows,
                  NColumns,
                  MatrixElements,
                  0,
                  0,
                  TmpMax,
                  [0, 0],
                  [NRowMax, NColomnMax]).

%!  matrix_minarg(+M1,-MinPosition)
% matrix_minarg(M,[NRowMax, NColumnMax]) unify [NRowsMax, NColumnMax] with the position of the minimum in matrix M. 
minarg_matrix(NRows, _, _, NRows, _, _, [NRowTmp, NColumnTmp], [NRowTmp, NColumnTmp]) :-
    !.

minarg_matrix(NRows, NColumns, MatrixElements, CurrentRow, NColumns, TmpMax, [NRowTmp, NColumnTmp], [NRowMax, NColumnMax]) :-
    !,
    CurrentRow1 is CurrentRow+1,
    minarg_matrix(NRows,
                  NColumns,
                  MatrixElements,
                  CurrentRow1,
                  0,
                  TmpMax,
                  [NRowTmp, NColumnTmp],
                  [NRowMax, NColumnMax]).

                minarg_matrix(NRows, NColumns, MatrixElements, CurrentRow, CurrentColumn, TmpMax, [NRowTmp, NColumnTmp], [NRowMax, NColumnMax]) :-
    Index is CurrentRow*NColumns+CurrentColumn,
    c_load(MatrixElements[Index], Element),
    CurrentColumn1 is CurrentColumn+1,
    (   Element=<TmpMax
    ->  TmpMax1 is Element,
        NRowTmp1 is CurrentRow,
        NColumnTmp1 is CurrentColumn
    ;   TmpMax1 is TmpMax,
        NRowTmp1 is NRowTmp,
        NColumnTmp1 is NColumnTmp
    ),
    minarg_matrix(NRows,
                  NColumns,
                  MatrixElements,
                  CurrentRow,
                  CurrentColumn1,
                  TmpMax1,
                  [NRowTmp1, NColumnTmp1],
                  [NRowMax, NColumnMax]).

matrix_minarg(matrix(_, [NRows, NColumns], MatrixElements), [NRowMax, NColumnMax]) :-
    c_load(MatrixElements[0], TmpMax),
    minarg_matrix(NRows,
                  NColumns,
                  MatrixElements,
                  0,
                  0,
                  TmpMax,
                  [0, 0],
                  [NRowMax, NColumnMax]).

%!  matrix_agg_lines(+M1,+AggregationMatrix)
% matrix_agg_lines (M,AggregationMatrix) sum all the elements of every row of M and the new value will be insert in the AggregationMatrix.
agg_lines_matrix(NRows, _, _, _, NRows, _, _) :-
    !.

agg_lines_matrix(NRows, NColumns, MatrixElements1, MatrixElements2, CurrentRow, NColumns, SumRow) :-
    !,
    c_store(MatrixElements2[CurrentRow], SumRow),
    SumRow1 is 0,
    CurrentRow1 is CurrentRow+1,
    agg_lines_matrix(NRows,
                     NColumns,
                     MatrixElements1,
                     MatrixElements2,
                     CurrentRow1,
                     0,
                     SumRow1).

agg_lines_matrix(NRows, NColumns, MatrixElements1, MatrixElements2, CurrentRow, CurrentColumn, SumRow) :-
    Index is CurrentRow*NColumns+CurrentColumn,
    c_load(MatrixElements1[Index], Element),
    SumRow1 is SumRow+Element,
    CurrentColumn1 is CurrentColumn+1,
    agg_lines_matrix(NRows,
                     NColumns,
                     MatrixElements1,
                     MatrixElements2,
                     CurrentRow,
                     CurrentColumn1,
                     SumRow1).


matrix_agg_lines(matrix(Type, [NRows, NColumns], MatrixElements1), AggregationMatrix) :-
    matrix_new(Type, [NRows, 1], AggregationMatrix),
    AggregationMatrix=matrix(_, _, MatrixElements2),
    agg_lines_matrix(NRows, NColumns, MatrixElements1, MatrixElements2, 0, 0, 0).

%!  matrix_agg_cols(+M1,+AggregationMatrix)
% matrix_agg_cols (M,AggregationMatrix) sum all the elements of every column of M and the new value will be insert in the AggregationMatrix.
agg_cols_matrix(_, NColumns, _, _, _, NColumns, _) :-
    !.

agg_cols_matrix(NRows, NColumns, MatrixElements1, MatrixElements2, NRows, CurrentColumn, SumRow) :-
    !,
    c_store(MatrixElements2[CurrentColumn], SumRow),
    SumRow1 is 0,
    CurrentColumn1 is CurrentColumn+1,
    agg_cols_matrix(NRows,
                    NColumns,
                    MatrixElements1,
                    MatrixElements2,
                    0,
                    CurrentColumn1,
                    SumRow1).

agg_cols_matrix(NRows, NColumns, MatrixElements1, MatrixElements2, CurrentRow, CurrentColumn, SumRow) :-
    Index is CurrentRow*NColumns+CurrentColumn,
    c_load(MatrixElements1[Index], Element),
    SumRow1 is SumRow+Element,
    CurrentRow1 is CurrentRow+1,
    agg_cols_matrix(NRows,
                    NColumns,
                    MatrixElements1,
                    MatrixElements2,
                    CurrentRow1,
                    CurrentColumn,
                    SumRow1).


matrix_agg_cols(matrix(Type, [NRows, NColumns], MatrixElements), AggregationMatrix) :-
    matrix_new(Type, [1, NColumns], AggregationMatrix),
    AggregationMatrix=matrix(_, _, MatrixElements2),
    agg_cols_matrix(NRows, NColumns, MatrixElements, MatrixElements2, 0, 0, 0).

%!  matrix_op(+M1,+M2,+Operator,-M3)
% matrix_op(M1,M2,Op,M3) apply Op an operator entered in input (+,-,*,/) element by element of matrix M1 and M2 and the result will be insert in M3.
matrix_map(Predicate, matrix(Type, [NRows, NColumns], MatrixElements1), matrix(Type, [NRows, NColumns], MatrixElements2), matrix(Type, [NRows, NColumns], MatrixElements3)) :-
    matrix_new(Type,
               [NRows, NColumns],
               matrix(_, _, MatrixElements3)),
    matrix_map(Predicate,
               NRows,
               NColumns,
               MatrixElements1,
               MatrixElements2,
               MatrixElements3,
               0,
               0).

matrix_map(_, NRows, _, _, _, _, NRows, _) :-
    !.

matrix_map(Predicate, NRows, NColumns, MatrixElements1, MatrixElements2, MatrixElements3, CurrentRow, NColumns) :-
    !,
    CurrentRow1 is CurrentRow+1,
    matrix_map(Predicate,
               NRows,
               NColumns,
               MatrixElements1,
               MatrixElements2,
               MatrixElements3,
               CurrentRow1,
               0).

matrix_map(Predicate, NRows, NColumns, MatrixElements1, MatrixElements2, MatrixElements3, CurrentRow, CurrentColumn) :-
    Index is CurrentRow*NColumns+CurrentColumn,
    c_load(MatrixElements1[Index], Element1),
    c_load(MatrixElements2[Index], Element2),
    call(Predicate, Element1, Element2, Element3),
    c_store(MatrixElements3[Index], Element3),
    CurrentColumn1 is CurrentColumn+1,
    matrix_map(Predicate,
               NRows,
               NColumns,
               Element1,
               Element2,
               Element3,
               CurrentRow,
               CurrentColumn1).

matrix_op(matrix(Type, [NRows, NColumns], Element1), matrix(Type, [NRows, NColumns], Element2), Operator, Matrix3) :-
    LambdaExpression= \E1^E2^E3^(Predicate=..[Operator, E1, E2], E3 is Predicate),
    matrix_map(LambdaExpression,
               matrix(Type, [NRows, NColumns], Element1),
               matrix(Type, [NRows, NColumns], Element2),
               Matrix3).

%!  matrix_op_to_all(+M1,+Operator,+Operand,-M2)
% matrix_op_to_all(M1,Operator,Operand,M2) apply Operator (+,-,*,/) and Operand (an integer, a float...) element by element at matrix M1 and the result will be insert in M2. Example: input (+3) sum 3 to each elements of M1.
matrix_op_to_all(matrix(Type, [NRows, NColumns], MatrixElements), Operator, Operand, Matrix2) :-
    LambdaExpression= \E1^E2^(Predicate=..[Operator, E1, Operand], E2 is Predicate),
    matrix_map(LambdaExpression,
               matrix(Type, [NRows, NColumns], MatrixElements),
               Matrix2).

% function that verifies that the inserted rows or columns are valid
function_call(_, []) :-
    !.
function_call(P, [H|T]) :-
    call(P, H),
    function_call(P, T).

function_call_lesser(L, M) :-
    P= \H^(H<M),
    function_call(P, L).

% matrix_select_rows(M1,RowList,M2) select a list of row (RowList) from M1 and this rows will be copied in M2.
matrix_select_rows(matrix(Type, [NRows, NColumns], MatrixElements1), RowList, matrix(Type, [NRows2, NColumns], MatrixElements2)) :-
    function_call_lesser(RowList, NRows),
    length(RowList, NRows2),
    matrix_new(Type,
               [NRows2, NColumns],
               matrix(_, _, MatrixElements2)),
    matrix_select_rows(NColumns, MatrixElements1, MatrixElements2, RowList, 0, 0).

matrix_select_rows(_, _, _, [], _, _) :-
    !.
matrix_select_rows(NColumns, MatrixElements1, MatrixElements2, [_|MoreRows], RowIndex, NColumns) :-
    !,
    RowIndex1 is RowIndex+1,
    matrix_select_rows(NColumns, MatrixElements1, MatrixElements2, MoreRows, RowIndex1, 0).

matrix_select_rows(NColumns, MatrixElements1, MatrixElements2, [Row|MoreRows], RowIndex, ColumnIndex) :-
    Index1 is Row*NColumns+ColumnIndex,
    c_load(MatrixElements1[Index1], E),
    Index2 is RowIndex*NColumns+ColumnIndex,
    c_store(MatrixElements2[Index2], E),
    ColumnIndex1 is ColumnIndex+1,
    matrix_select_rows(NColumns,
                       MatrixElements1,
                       MatrixElements2,
                       [Row|MoreRows],
                       RowIndex,
                       ColumnIndex1).

% matrix_select_cols(M1,ColList,M2) select a list of columns (ColList) from M1 and this columns will be copied in M2.
matrix_select_cols(matrix(Type, [NRows, NColumnsMatrix1], MatrixElements1), List, matrix(Type, [NRows, NColumnsMatrix2], MatrixElements2)) :-
    function_call_lesser(List, NColumnsMatrix1),
    length(List, NColumnsMatrix2),
    matrix_new(Type,
               [NRows, NColumnsMatrix2],
               matrix(_, _, MatrixElements2)),
    matrix_select_cols(NRows,
                       NColumnsMatrix1,
                       NColumnsMatrix2,
                       MatrixElements1,
                       MatrixElements2,
                       List,
                       0,
                       0).

matrix_select_cols(_, _, _, _, _, [], _, _) :-
    !.
matrix_select_cols(NRows, NColumnsMatrix1, NColumnsMatrix2, MatrixElements1, MatrixElements2, [_|MoreCols], NRows, ColumnIndex) :-
    !,
    NewNColumns is ColumnIndex+1,
    matrix_select_cols(NRows,
                       NColumnsMatrix1,
                       NColumnsMatrix2,
                       MatrixElements1,
                       MatrixElements2,
                       MoreCols,
                       0,
                       NewNColumns).

matrix_select_cols(NRows, NColumnsMatrix1, NColumnsMatrix2, MatrixElements1, MatrixElements2, [Cols|MoreCols], RowIndex, ColumnIndex) :-
    Index1 is RowIndex*NColumnsMatrix1+Cols,
    c_load(MatrixElements1[Index1], E),
    Index2 is RowIndex*NColumnsMatrix2+ColumnIndex,
    c_store(MatrixElements2[Index2], E),
    RowIndex1 is RowIndex+1,
    matrix_select_cols(NRows,
                       NColumnsMatrix1,
                       NColumnsMatrix2,
                       MatrixElements1,
                       MatrixElements2,
                       [Cols|MoreCols],
                       RowIndex1,
                       ColumnIndex).

%!  matrix_select(+M1,+Number,+List,+Result)
% matrix_select(M1,Number,List,Result) select from M1 a list of rows or columns (to select rows Number will be 1, to select columns Number will be 2) and these will be copied in Result. 
matrix_select(matrix(Type, [NRows, NColumns], MatrixElements), 1, List, Result) :-
    matrix_select_rows(matrix(Type, [NRows, NColumns], MatrixElements),
                       List,
                       Result).

matrix_select(matrix(Type, [NRows, NColumns], MatrixElements), 2, List, Result) :-
    matrix_select_cols(matrix(Type, [NRows, NColumns], MatrixElements),
                       List,
                       Result).

%!  matrix_createZeroes(+Type,+Dimension,-M1)
% matrix_createZeroes(M1) create a new matrix with all elements set to 0. 
matrix_createZeroes(Type, [NRows, NColumns], M1) :-
    matrix_new(Type, [NRows, NColumns], M1),
    M1=matrix(Type, [NRows, NColumns], Elements),
    matrixSetZeroes(NRows, NColumns, Elements).

%!  matrix_read(+Dimension,-M1)
%   Read from keyboard values and these will be insert in M1. 
matrix_read([NRows, NColumns], M1) :-
    matrix_new(doubles, [NRows, NColumns], M1),
    matrix_foreach(M1, \_^Y^read(Y)).

%!  matrixSetZeroes(+NRows,+NColumns,+Elements)
% matrixSetZeroes sets to 0 all its argument matrix's elements.
matrixSetZeroes(NRows, NColumns, Elements) :-
    matrixSetAll(NRows, NColumns, Elements, 0).

%!  matrixSetOnes(+NRows,+NColumns,+Elements)
% matrixSetOnes(M1) create a new matrix with all elements set to 1. 
matrixSetOnes(NRows, NColumns, Elements) :-
    matrixSetAll(NRows, NColumns, Elements, 1).

%!  matrix_setAllNumbers(+NRows,+NColumns,+Elements)
% matrix_setAll sets to Number given in input all the matrix's elements.
matrix_setAllNumbers(NRows, NColumns, Elements) :-
    read(Number),
    matrixSetAll(NRows, NColumns, Elements, Number).

%!  matrix_eye(+NRows,+NColumns,+Elements)
% matrix_eye create a identity matrix
matrix_eye(NRows, NColumns, Elements) :-
    matrixEye(NRows, NColumns, Elements).

%!  matrix_equal(+M1,+M2)
% matrix_equal(M1,M2) compares two matrix (M1 and M2).
equal_matrix(NRows, _, _, _, NRows, _) :-
    !.

equal_matrix(NRows, NColumns, MatrixElements1, MatrixElements2, CurrentRow, NColumns) :-
    !,
    CurrentRow1 is CurrentRow+1,
    equal_matrix(NRows, NColumns, MatrixElements1, MatrixElements2, CurrentRow1, 0).

equal_matrix(NRows, NColumns, MatrixElements1, MatrixElements2, CurrentRow, CurrentColumn) :-
    Index is CurrentRow*NColumns+CurrentColumn,
    c_load(MatrixElements1[Index], Element1),
    c_load(MatrixElements2[Index], Element2),
    CurrentColumn1 is CurrentColumn+1,
    Element1==Element2,
    equal_matrix(NRows, NColumns, MatrixElements1, MatrixElements2, CurrentRow, CurrentColumn1).

matrix_equal(matrix(Type, [NRows, NColumns], MatrixElements1), matrix(Type, [NRows, NColumns], MatrixElements2)) :-
    equal_matrix(NRows, NColumns, MatrixElements1, MatrixElements2, 0, 0).

%!  matrix_from_list(+List,+M1)
% matrix_from_list(List,M1) sect the elements of the matrix M1 with new element passed in a List.
from_list(List, Elements) :-
    from_list(List, 0, Elements).

from_list([], _, _) :-
    !.
from_list([Element|MoreElements], Index, Elements) :-
    c_store(Elements[Index], Element),
    Index1 is Index+1,
    from_list(MoreElements, Index1, Elements).

matrix_from_list(List, matrix(_, [_, _], Elements)) :-
    from_list(List, Elements).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%       START TESTS       %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- begin_tests(matrix).

    test(matrix_mul1) :-
    matrix_new(doubles, [2, 2], [1, 2, 3, 4], Matrix1),
    matrix_new(doubles, [2, 2], [4, 3, 2, 1], Matrix2),
    matrix_new(doubles, [2, 2], [13.0, 20.0, 5.0, 8.0], Expected),
    matrix_mul(Matrix1, Matrix2, Matrix3),
    matrix_equal(Matrix3, Expected).

    test(matrix_dims1) :-
    matrix_new(doubles, [3, 3], [1, 2, 3, 4, 5, 6, 7, 8, 9], Matrix1),
    matrix_dims(Matrix1, [3, 3]).

    test(matrix_size1) :-
    matrix_new(doubles, [3, 3], [1, 2, 3, 4, 5, 6, 7, 8, 9], Matrix1),
    matrix_size(Matrix1, 9).
    
    test(matrix_type1) :-
    matrix_new(doubles, [3, 3], [1, 2, 3, 4, 5, 6, 7, 8, 9], Matrix1),
    matrix_type(Matrix1, doubles).

    test(matrix_to_list1) :-
    matrix_new(doubles, [3, 3], [1, 2, 3, 4, 5, 6, 7, 8, 9], Matrix1),
    matrix_to_list(Matrix1, [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0]).

    test(matrix_get1) :-
    matrix_new(doubles, [3, 3], [1, 2, 3, 4, 5, 6, 7, 8, 9], Matrix1),
    matrix_get(Matrix1, [0, 1], 2.0).

    test(matrix_set1) :-
    matrix_new(doubles, [3, 3], [1, 2, 3, 4, 5, 6, 7, 8, 9], Matrix1),
    matrix_set(Matrix1, [1, 2], 5),
    matrix_get(Matrix1, [1, 2], 5.0).

    test(matrix_set_all1) :-
    matrix_new(doubles, [2, 2], Matrix1),
    matrix_new(doubles, [2, 2], [7.0, 7.0, 7.0, 7.0], Expected),
    matrix_set_all(number(doubles, 7), Matrix1),
    matrix_equal(Matrix1, Expected).

    test(matrix_add1) :-
    matrix_new(doubles, [3, 3], [1, 2, 3, 4, 5, 6, 7, 8, 9], Matrix1),
    matrix_add(Matrix1, [1, 0], 7),
    matrix_get(Matrix1, [1, 0], 11.0).

    test(matrix_inc1) :-
    matrix_new(doubles, [3, 3], [1, 2, 3, 4, 5, 6, 7, 8, 9], Matrix1),
    matrix_inc(Matrix1, [0, 1], 3.0).

    test(matrix_dec1) :-
    matrix_new(doubles, [3, 3], [1, 2, 3, 4, 5, 6, 7, 8, 9], Matrix1),
    matrix_dec(Matrix1, [0, 1], 1.0).

    test(matrix_max1) :-
    matrix_new(doubles, [3, 3], [1, 2, 3, 11, 5, 6, 7, 8, 9], Matrix1),
    matrix_max(Matrix1, 11.0).

    test(matrix_min1) :-
    matrix_new(doubles, [3, 3], [3, 2, 1, 11, 5, 6, 7, 8, 9], Matrix1),
    matrix_min(Matrix1, 1.0).

    test(matrix_sum1) :-
    matrix_new(doubles, [3, 3], [1, 2, 3, 11, 5, 6, 7, 8, 9], Matrix1),
    matrix_sum(Matrix1, 52.0).

    test(matrix_transpose1) :-
    matrix_new(doubles, [2, 3], [1, 2, 3, 11, 5, 6], Matrix1),
    matrix_new(doubles, [3, 2], [1, 11, 2, 5, 3, 6], Expected),
    matrix_transpose(Matrix1, Matrix2),
    matrix_equal(Matrix2, Expected).

    test(matrix_maxarg1) :-
    matrix_new(doubles, [2, 3], [1, 2, 3, 11, 5, 6], Matrix1),
    matrix_maxarg(Matrix1, [1, 0]).

    test(matrix_minarg1) :-
    matrix_new(doubles, [2, 3], [13, 2, 3, 11, 1, 6], Matrix1),
    matrix_minarg(Matrix1, [1, 1]).

    test(matrix_agg_lines1) :-
    matrix_new(doubles, [2, 3], [13, 2, 3, 10, 1, 6], Matrix1),
    matrix_new(doubles, [2, 1], [18, 17], Expected),
    matrix_agg_lines(Matrix1, AggregationMatrix),
    matrix_equal(AggregationMatrix, Expected).

    test(matrix_agg_cols1) :-
    matrix_new(doubles, [2, 3], [13, 2, 3, 10, 1, 6], Matrix1),
    matrix_new(doubles, [1, 3], [23, 3, 9], Expected),
    matrix_agg_cols(Matrix1, AggregationMatrix),
    matrix_equal(AggregationMatrix, Expected).

    test(matrix_op1) :-
    matrix_new(doubles, [2, 2], [13, 2, 3, 10], Matrix1),
    matrix_new(doubles, [2, 2], [2, 8, 13, 9], Matrix2),
    matrix_write(Matrix1),
    matrix_op(Matrix1, Matrix2, +, Matrix3),
    matrix_new(doubles, [2, 2], [15, 10, 16, 19], Expected),
    matrix_equal(Matrix3, Expected).

    test(matrix_op_to_all1) :-
    matrix_new(doubles, [2, 3], [13, 2, 3, 10, 1, 6], Matrix1),
    matrix_new(doubles, [2, 3], [16, 5, 6, 13, 4, 9], Expected),
    matrix_op_to_all(Matrix1, +, 3, Matrix2),
    matrix_equal(Matrix2, Expected).

    test(matrix_select1) :-
    matrix_new(doubles, [3, 2], [13, 2, 3, 10, 1, 6], Matrix1),
    matrix_new(doubles, [1, 2], [13, 2], Expected),
    matrix_select(Matrix1, 1, [0], Matrix2),
    matrix_equal(Matrix2, Expected).

    test(matrix_select2) :-
    matrix_new(doubles, [3, 2], [13, 2, 3, 10, 1, 6], Matrix1),
    matrix_new(doubles, [2, 2], [13, 2, 3, 10], Expected),
    matrix_select(Matrix1, 1, [0, 1], Matrix2),
    matrix_equal(Matrix2, Expected).

    test(matrix_select3) :-
    matrix_new(doubles, [2, 3], [13, 2, 3, 10, 1, 6], Matrix1),
    matrix_new(doubles, [2, 2], [13, 3, 10, 6], Expected),
    matrix_select(Matrix1, 2, [0, 2], Matrix2),
    matrix_equal(Matrix2, Expected).

    test(matrixCreateZeroes1) :-
    matrix_createZeroes(doubles, [3, 3], Matrix1),
    matrix_new(doubles, [3, 3], [0, 0, 0, 0, 0, 0, 0, 0, 0], Expected),
    matrix_equal(Matrix1, Expected).

    test(matrixSetZeroes1) :-
    matrix_new(doubles, [3, 3], Matrix1),
    Matrix1=matrix(_, [3, 3], Elements),
    matrix_new(doubles, [3, 3], [0, 0, 0, 0, 0, 0, 0, 0, 0], Expected),
    matrixSetZeroes(3, 3, Elements),
    matrix_equal(Matrix1, Expected).

    test(matrixSetOnes1) :-
    matrix_new(doubles, [3, 3], Matrix1),
    Matrix1=matrix(_, [3, 3], Elements),
    matrix_new(doubles, [3, 3], [1, 1, 1, 1, 1, 1, 1, 1, 1], Expected),
    matrixSetOnes(3, 3, Elements),
    matrix_equal(Matrix1, Expected).

    test(matrix_setAllNumbers1) :-
    matrix_new(doubles, [3, 3], Matrix1),
    Matrix1=matrix(_, [3, 3], Elements),
    Number is 5.0,
    matrixSetAll(3, 3, Elements, Number),
    matrix_new(doubles, [3, 3], [5.0, 5.0, 5.0, 5.0, 5.0, 5.0, 5.0, 5.0, 5.0], Expected),
    matrix_equal(Matrix1, Expected).

    test(matrix_eye1) :-
    matrix_new(doubles, [3, 3], Matrix1),
    Matrix1=matrix(_, [3, 3], Elements),
    matrix_eye(3, 3, Elements),
    matrix_new(doubles, [3, 3], [1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0], Expected),
    matrix_equal(Matrix1, Expected).

    test(matrix_equal1) :-
    matrix_new(doubles, [3, 3], [1, 2, 3, 4, 5, 6, 7, 8, 9], Matrix1),
    matrix_new(doubles, [3, 2], [1, 3, 4, 6, 7, 9], Expected),
    matrix_select(Matrix1, 2, [0, 2], Matrix2),
    matrix_equal(Matrix2, Expected).

    test(matrix_map1) :-
    matrix_new(doubles, [2, 3], [1, 2, 3, 11, 5, 6], Matrix1),
    matrix_map(\E1^E2^(E2 is E1*2),
               Matrix1,
               Matrix2),
    matrix_new(doubles, [2, 3], [2, 4, 6, 22, 10, 12], Expected),
    matrix_equal(Matrix2, Expected).

    test(matrix_foreach1) :-
    matrix_new(doubles, [3, 3], [1, 2, 3, 4, 5, 6, 7, 8, 9], Matrix1),
    matrix_new(doubles, [3, 3], [3, 3, 3, 3, 3, 3, 3, 3, 3], Expected),
    matrix_foreach(Matrix1,
                   \X^Y^myset(X, Y, 3)),
    matrix_equal(Matrix1, Expected).

    test(matrix_foldl1) :-
    matrix_new(doubles, [3, 3], [1, 2, 3, 4, 5, 6, 7, 8, 9], Matrix1),
    matrix_foldl(Matrix1, findMax, 9.0).

    test(matrix_from_list1) :-
    matrix_new(doubles, [3, 3], [1, 2, 3, 4, 5, 6, 7, 8, 9], M1),
    matrix_from_list([11, 12, 13, 14, 15, 16, 17, 18, 19], M1),
    matrix_new(doubles,
               [3, 3],
               [11.0, 12.0, 13.0, 14.0, 15.0, 16.0, 17.0, 18.0, 19.0],
               Expected),
    matrix_equal(M1, Expected).

end_tests(matrix).
