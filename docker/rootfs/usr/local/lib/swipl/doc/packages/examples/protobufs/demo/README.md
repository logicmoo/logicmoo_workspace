# Examples for handling Google's Protocol Buffers

## Makefile

The Makefile contains a grab-bag of simple rules, to make running some
of the examples easier. The default goal is `check`. Other convenience
goals exist - see the `.PHONY` rule to see what they are

## Installing protobuf (on Ubuntu)

You can use the Ubuntu package `protobuf-compiler`, but it's dated Jul
31, 2018.  Instead, you can clone from
https://github.com/protocolbuffers/protobuf and build using the
instructions in `protobuf/src/README.md`. For the `./configure`
command you may wish to use `./configure --prefix=$HOME/.local` and
`make -j4` (where "4" should be replaced by the number of cores on
your machine).

To install the Python support: 'python3 -m pip install protobuf`; you
might wish to add `--user`, or do this in a virtual env.

## vector_demo.pl

Contains code snippets that correspond to the documentation
in ../protobufs_overview.md. There are also some basic tests
in here.

TODO: separate out the tests into a proper test suite.
