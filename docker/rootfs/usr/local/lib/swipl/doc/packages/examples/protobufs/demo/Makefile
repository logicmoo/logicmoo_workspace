# Demo of protobufs.

# To run this, you must install protobufs first. The easiest way is:
#    sudo apt install protobuf-compiler
# Or you can get the latest source and buid it:
#    https://github.com/protocolbuffers/protobuf/blob/master/src/README.md

# The demo is run by:
#   make test

# Assumption: protoc and swipl are in your $PATH ... you can override
# these by setting PROTOC= or SWIPL= when envoking "make".

# If you're having problems with the gcc flags, you might need to set
# PKGCONFIG_PATH. For example, if you install the protobuf compiler in
# $HOME/.local (when building with ./configure --prefix=$HOME/.local),
# then set PKG_CONFIG_PATH=$HOME/.local/lib/pkgconfig and possibly
# also set LD_LIBRARY_PATH=$HOME/.local/lib or change the rule for
# "foo" to be: $(CXX) -static -o $@ ...

# This demo was tested on Ubuntu 20.04.2 with protoc versions 3.6.1
# (installed using `sudo apt install protobuf-compiler`) and 3.17.3
# (from the sources).

# Many of the rules at the end of this Makefile are for development
# and will eventually be removed.

# Naming conventions - see README.md section "descriptor.proto and friends"

include ../bootstrap/common.mk

.DELETE_ON_ERROR: # Any non-zero return code deletes the target file(s)

.PHONY: FORCE check clean test_all interop_test
.PHONY: test_all test_basic_usage test_send_command test_send_precompiled_command

.DEFAULT_GOAL=test

test: check test_basic_usage test_send_command test_send_precompiled_command

# These files don't need to be kept, but can be useful for debugging:
.PRECIOUS: %.pb.cc %.pb.h %_pb2.py

# Protobuf code generator for C++
%.pb.cc %.pb.h: %.proto
	$(PROTOC) $(PROTOC_I) --cpp_out=. $<

foo.o: pb_vector.pb.h foo.cpp

# foo: foo.cpp pb_vector.pb.cc pb_vector.pb.h
foo: pb_vector.pb.o foo.o
	@# To run, you might require setting LD_LIBRARY_PATH or specify -static
	@# or for non-static: -Wl,-rpath=$(PROTOC_LIB)
	@# $(CXX) -Wl,-rpath=$(PROTOC_LIB) -o $@ $^ $(CXXFLAGS) $(CPPFLAGS) $(LDFLAGS)
	@# $(CXX) -static -o $@ $^ $(CXXFLAGS) $(CPPFLAGS) $(LDFLAGS)
	$(CXX) -o $@ $^ $(CXXFLAGS) $(CPPFLAGS) $(LDFLAGS)

tmp99.tmp: vector_demo.pl ../eventually_implies.pl
	$(SWIPL) -s vector_demo.pl -g make_tmp99 --

check: foo tmp99.tmp FORCE
	./foo <tmp99.tmp
	$(PROTOC) --decode_raw <tmp99.tmp
	$(PROTOC) --decode=Vector pb_vector.proto <tmp99.tmp

# Run the basic_usage example
test_basic_usage: vector_demo.pl FORCE
	$(SWIPL) -g test_basic_usage -g halt vector_demo.pl

test_send_command: vector_demo.pl FORCE
	$(SWIPL) -g test_send_command -g halt vector_demo.pl

test_send_precompiled_command: vector_demo.pl FORCE
	$(SWIPL) -g test_send_precompiled_command -g halt vector_demo.pl

clean:
	@# TODO: special handling for descriptor.* files
	$(RM) -r foo *.tmp *.o *.pb.cc *.pb.h *_pb2.py
	git clean -dxf  # Should find nothing.

# Assume you've cloned git@github.com:protocolbuffers/protobuf.git (fetch) to $(HOME)/src.
# Note that ../golden_message.2.5.0 is the same as $(HOME)/src/protobuf/python/compatibility_tests/v2.5.0/tests/google/protobuf/internal/golden_message
dump_golden:
	$(PROTOC) $(PROTOC_I) \
		--decode=protobuf_unittest.TestAllTypes \
		google/protobuf/unittest.proto \
		<../golden_message.2.5.0

interop_test:
	@# "ctest -V -R protobufs" is envoked by "test_protobufs"
	cd ../../../build && ninja && ctest -V -R protobufs
	$(MAKE) -C ../interop test

.PHONY: testx
testx:
	cd ../../../build && ninja
	make SWIPL=$(HOME)/src/swipl-devel/build/src/swipl $(HOME)/src/swipl-devel/packages/protobufs/demo test_all
