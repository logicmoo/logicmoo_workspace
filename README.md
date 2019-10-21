TRILL
=====

TRILL is a tableau reasoner able to compute probability of queries from probabilistic knowledge bases.

You can find the manual at https://github.com/rzese/trill/blob/master/doc/help-trill.pdf
 
You can try it online at http://trill-sw.eu

Installation
------------
This is a SWI-Prolog (http://www.swi-prolog.org/) pack.

It can be installed with `pack_install/1`

    $ swipl
    ?- pack_install(trill).

Requirements
-------------
It requires the packs

 * `bddem` https://github.com/friguzzi/bddem
 
 They are installed automatically when installing pack `trill` or can installed manually as

    $ swipl
    ?- pack_install(bddem).

`bddem` uses a foreign library and contains the library binaries for 32 and 64 bits Linux and 64 bits Windows. If you want to recompile the foreign library you can use

    ?- pack_rebuild(bdeem).

On 32 and 64 bits Linux this should work out of the box. On 64 bits Windows the library must be rebuilt by hand, see the pack page https://github.com/friguzzi/bddem

You can upgrade the pack with

    $ swipl
    ?- pack_upgrade(trill).

Note that the packs on which `trill` depends are not upgraded automatically in this case so they need to be upgraded manually.


Example of use
---------------

    $ cd <pack>/trill/prolog/examples
    $ swipl
    ?- [peoplePets].
    ?- prob_instanceOf('natureLover','Kevin',Prob).

Testing the installation
------------------------

    $ swipl
    ?- [library(trill_test/test)].
    ?- test.

Support
-------

Use the Google group https://groups.google.com/forum/#!forum/trill-system
