TRILL
=====

TRILL is a tableau reasoner able to compute probability of queries from probabilistic knowledge bases.
 
You can try it online at http://trill.lamping.unife.it/

Installation
------------
This is a SWI-Prolog (http://www.swi-prolog.org/) pack.

It can be installed with `pack_install/1`

    $ swipl
    ?- pack_install(cplint).

The pack uses a foreign library and contains the library binaries for 32 and 64 bits Linux and 32 and 64 bits Windows. 
If you want to recompile the foreign library you can use

    ?- pack_rebuild(cplint).

On 32 and 64 bits Linux this should work out of the box. On 32 and 64 bits Windows the library must be rebuilt by hand. 
First run `pack_rebuild(cplint)`. This typically fails but produces the file `buildenv.sh` in the root folder. 
You can modify this file looking at the example files `buildenvmingw32.sh` and `buildenvmingw64.sh`. 
Then you can run

    $ source buildenv.sh
    $ source configure
    $ make install


Example of use
---------------

    $ cd <pack>/trill/prolog/examples
    $ swipl
    ?- [peoplePets].
    ?- prob_instanceOf('natureLover','Kevin',Prob).
