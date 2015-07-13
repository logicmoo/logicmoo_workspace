# mpi
Porting of the [LAMMPI library](http://sourceforge.net/p/yap/yap-6.3/ci/master/tree/library/lammpi/) of Yap Prolog to SWI-Prolog.

The authors of LAMMPI are [Nuno A. Fonseca](http://www.ebi.ac.uk/about/people/nuno-fonseca) and [Vitor Santos Costa](http://www.dcc.fc.up.pt/~vsc/). The porting was done by  [Fabrizio Riguzzi](http://ds.ing.unife.it/~friguzzi/).

## Requirements
To compile and run the library you need an MPI framework, such as [OpenMPI](http://www.open-mpi.org/), installed in your system. 

## Example of use
    $ cd <pack>/mpi/prolog/examples/trains
    $ mpirun -np 2 swipl wait.pl

## Examples
The directory `prolog/examples` contains the examples of use available in Yap.

## Manual
Besides the examples, you can look at http://www.dcc.fc.up.pt/~vsc/yap/d4/dca/group___l_a_m.html.

