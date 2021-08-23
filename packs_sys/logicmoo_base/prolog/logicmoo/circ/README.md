# Circumscription
An implemented prototype of two different variants of Circumscription formalized in the [PIE system](http://cs.christophwernhard.com/pie/) of Christoph Wernhard.

## Requirements

-   Installed [PIE system](http://cs.christophwernhard.com/pie/) in this folder
-   Prover9/mace4 (see PIE install instructions)

## Build

The PIE system outputs pdf files, but can also be run in interactive mode.

1.  Update the relative path to your PIE installation in the Makefile
2.  Try `make final` or `make interactive` to go into prolog console.
    The pdf file can be found in `/tmp/tmp_ppl.pdf`.


## View the results

A compiled version is available in the file `On_Circumscription.pdf`.
If your installation is set up correctly, you should obtain the same results. 
