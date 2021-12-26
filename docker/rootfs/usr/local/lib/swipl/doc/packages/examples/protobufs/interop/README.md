# Inter-operatbility

This directory contains some tests for verifying inter-operability
between Prolog protobufs and other languages. The initial version of
this uses Python, but it is anticipated that other languages will be
added in the future (e.g., C++, JavaScript).

The tests are *not* part of the standard release process because they
require having the protobuf compiler (`protoc`) installed and also a
version of Python 3. There are tests in `CMakeList.txt` to not run the
tests (and emit a warning) if the prerequisite files aren't available.

To run the tests:
```
make test
make clean
```
You can specify these additional variables:
```
make PROTOC=/path/to/protoc PYTHON=/path/to/python3 SWIPL=/path/to/swipl test
```

The tests require the Python protobuf support. One way of installing it is:
```
python3 -mpip install protobuf
```
But you should check the version: it should be at least 3.18.1 (which you can check
by: `python3 -mpip show protobuf`), although it's possible that anything after
3.8.0  will work.
You might be tempted to install using `sudo apt install python3-protobuf`,
but its version (as of 2021-10-12) is 3.6.1, which is too old.

You will also need the protobuf compiler. This can be installed on
Debian (Ubuntu) by:
```
sudo apt install protobuf-compiler
```
Any version 3.6.1 or later should work, although it's best to get the
latest one (3.18.1, as of 2021-10-12).

If you install protoc other than in `/usr/bin` (which is where the Debian/Ubuntu
package puts it), you might need to modify the tests that have C++ code (currently
rules `test_read` and `test_write`, plus `foo` in `../demo/Makefile`).
There are comments in the Makefiles.

To trun these tests under the top-level SWI-Prolog `ctest` (assuming your
sources are in `$HOME/src` and you install a local version to
`$HOME/.local`). Note `-DTEST_PROTOBUFS_PROTOC=ON`, which adds
the tests in this directory (and also the tests in `../demo` and `../bootstrap`).
```
cd $HOME/src/swipl-devel
rm -rf build
mkdir -p build
cd build
# Ensure all the targets get rebuilt:
rm -rf packages/protobufs \
       home/library/protobufs \
       home/doc/packages/examples/protobufs
# Optional: -DCMAKE_BUILD_TYPE=PGO
cmake -DCMAKE_INSTALL_PREFIX=$HOME/.local -DTEST_PROTOBUFS_PROTOC=ON -G Ninja ..
ninja
ctest -j4 -R protobufs  # or: ctest -V -R protobufs:interop
# Optional:
# ninja install
```

## .proto Sources

When the protobuf compiler is installed, the following additional
files are also installed:

```
include/google/protobuf/any.proto
include/google/protobuf/api.proto
include/google/protobuf/compiler/plugin.proto
include/google/protobuf/descriptor.proto
include/google/protobuf/duration.proto
include/google/protobuf/empty.proto
include/google/protobuf/field_mask.proto
include/google/protobuf/source_context.proto
include/google/protobuf/struct.proto
include/google/protobuf/timestamp.proto
include/google/protobuf/type.proto
include/google/protobuf/wrappers.proto
```

Missing from this is (where `$SRC` is the top-level source directory, into which
`git@github.com:protocolbuffers/protobuf.git` is cloned):
```
$SRC/protobuf/src/google/protobuf/unittest.proto
$SRC/protobuf/src/google/protobuf/unittest_import.proto
$SRC/protobuf/src/google/protobuf/unittest_import_public.proto
```

So, a copy of these files hav been made here.
They related to the "golden_message" test in `../test_protobufs.pl`.

## addressbook.proto, addressbook2.proto

These are from the [Google protobuf
tutorial](https://developers.google.com/protocol-buffers/docs/tutorials),
mainly from the [Python
tutorial](https://developers.google.com/protocol-buffers/docs/pythontutorial).

They have been slightly modified to put the `TimeStamps` message into
a second `.proto` file, in order to test a few things.

## Future work

These tests write into the current directory. This prevents using a
read-only source tree, amongst other things. The tests should be
modified to use a separate "build" directory.
