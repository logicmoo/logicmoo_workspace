# matrix

SWI-Prolog pack for matrix operations. 
Impemented operations:
 - sum
 - difference
 - multiplication
 - Cholesky decomposition https://en.wikipedia.org/wiki/Cholesky_decomposition
 - determinant for positive semi-definite matrices (using Cholesky decomposition)
 - inversion for positive semi-definite matrices (using Cholesky decomposition)
 - inversion for lower triangular matrices 

The library was developed for dealing with multivariate Gaussian distributions, 
that's the reason for the focus on positive semi-definite matrices

Example of use
---------------

    $ swipl
    ?- use_module(library(matrix)).
    ?- determinant([[2,-1,0],[-1,2,-1],[0,-1,2]],D).
    D = 3.999999999999999.

