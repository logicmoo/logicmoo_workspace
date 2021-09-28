% called with
% mpirun -np 4 swipl hello.pl
%

:- use_module(library(lam_mpi)).


main:-
       mpi_init,
       mpi_comm_size(Sz),
       mpi_comm_rank(Rank),
       mpi_version(V0,V1),
       format('MPI ~d.~d workers=~d id=~d~n', [V0,V1,Sz, Rank]),
       mpi_finalize.

:- main,halt.
