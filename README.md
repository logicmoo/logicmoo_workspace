phil
======
phil is a parameter learning algorithm that learns the parameters of Hierarchical Probabilistic Logic Programs (HPLP) applying gradient descent (dphil) or Expectation Maximization (emphil).

Installation
------------
This is a SWI-Prolog (http://www.swi-prolog.org/) pack.

It can be installed with pack_install/1

	$ swipl
	?- pack_install(phil).

The pack uses a foreign library and contains the library binaries for 32 and 64 bits Linux and 32 and 64 bits Windows. If you want to recompile the foreign library you can use

	?- pack_rebuild(phil).

On 32 and 64 bits Linux this should work out of the box. On 32 and 64 bits Windows the library must be rebuilt by hand. First run pack_rebuild(phil). This typically fails but produces the file buildenv.sh in the root folder. You can modify this file looking at the example files buildenvmingw32.sh and buildenvmingw64.sh. Then you can run

	$ source buildenv.sh
	$ source configure
	$ make install


Requirements
-------------
It requires packs auc and matrix:

	$ swipl
	?- pack_install(auc).
	?- pack_install(matrix).

Example of use
-------------
Datasets are available in pack cplint_datasets (https://github.com/ArnaudFadja/phil_datasets)
Install the phil_datasets with  pack_install/1

	$ swipl
	?- pack_install(phil_datasets).

Then

	$ cd <pack>/phil/prolog/
	$ swipl
	?- [uwcselearndeep].
	?- induce_par([ai,graphics,language,systems,theory],P),test(P,[ai],LL,AUCROC,ROC,AUCPR,PR).

