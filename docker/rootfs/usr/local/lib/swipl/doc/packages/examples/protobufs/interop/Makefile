# Protobuf interoperability tests

# See comments in ../demo/Makefile about PKG_CONFIG_PATH

.DEFAULT_GOAL=test
.PHONY=test clean FORCE
# SHELL:=/bin/bash
# These files don't need to be kept, but can be useful for debugging:
.PRECIOUS: %.pb.cc %.pb.h %_pb.h

include ../bootstrap/common.mk

# unittest.proto is copied from here (see README.md):
#   UNITTEST_PROTO=$(SRC_PROTOBUF)/src/google/protobuf/unittest.proto
UNITTEST_PROTO=google/protobuf/unittest.proto
UNITTEST_IMPORT_PROTO=google/protobuf/unittest_import.proto

.SUFFIXES: .proto .py .pl .wire

FORCE:

# TODO: replicate the addressbook sample from the protobufs tutorial
test: test_interop_prolog \
	scalars1_wire test_read_cc test_read_python \
	addressbook_pb.pl addressbook2_pb.pl

.PHONY: run_addressbook
run_addressbook: addressbook.pl addressbook_pb.pl addressbook2_pb.pl \
		google/protobuf/timestamp_pb.pl FORCE
	$(RM) addressbook.msg
	$(SWIPL) -g test_write -g test_write -g test_read -t halt addressbook.pl

# Protobuf code generator for Python and C++
%_pb2.py %.pb.h %.pb.cc: %.proto
	$(PROTOC) -Igoogle/protobuf $(PROTOC_I) --python_out=. --cpp_out=. $?

%_pb.pl: %.proto ../bootstrap/protoc-gen-swipl
	@# PATH=".:$$PATH" $(PROTOC) $(PROTOC_I) --swipl_out=. $<
	@# s/OUT_PARAMETER/all/ ? (see comment in protoc-gen-swipl
	@#                         with Request.parameter)
	PATH=$(PATH_WITH_SWIPL) $(PROTOC) $(PROTOC_I) --swipl_out='OUT_PARAMETER':. \
		--plugin=protoc-gen-swipl=$(realpath ../bootstrap/protoc-gen-swipl) $<

google/protobuf/timestamp_pb.pl:  # in ~/.local/include/google/protobuf or similar
	PATH=$(PATH_WITH_SWIPL) $(PROTOC) $(PROTOC_I) --swipl_out='OUT_PARAMETER':. \
		--plugin=protoc-gen-swipl=$(realpath ../bootstrap/protoc-gen-swipl) \
		google/protobuf/timestamp.proto

.PHONY: run_test_read  # a convenience label
run_test_read \
golden_message.2.5.0_from_prolog.wire \
packed1a_from_prolog_template.wire \
packed1a_from_prolog.wire \
repeated1a_from_prolog_template.wire \
repeated1a_from_prolog.wire \
scalars1a_from_prolog_template.wire \
scalars1a_from_prolog.wire \
scalars1b_from_prolog_template.wire \
scalars1b_from_prolog.wire \
	: \
		test_pb.pl test2_pb.pl test2b_pb.pl \
		test_interop.pl test_templates.pl \
		google/protobuf/unittest_pb.pl \
		google/protobuf/unittest_import_pb.pl \
		google/protobuf/unittest_import_public_pb.pl \
		../golden_message.2.5.0 ../test_protobufs.pl \
		scalars1a_from_cc.wire scalars1b_from_cc.wire scalars1c_from_cc.wire \
		scalars1a_from_python.wire scalars1b_from_python.wire scalars1c_from_python.wire\
		repeated1a_from_python.wire \
		packed1a_from_python.wire
	@# "LANG=C" is for testing that we're doing the right thing with encodings:
	LANG=C $(SWIPL) test_interop.pl

# google/protobuf/unittest*.proto are copied from here (see README.md):
#    $(SRC_PROTOBUF)/src/google/protobuf/unittest.proto
# google/protobuf/unittest_pb.pl google/protobuf/unittest_import_pb.pl \
# google/protobuf/unittest_pb2.py google/protobuf/unittest_import_pb2.py: google/protobuf/unittest.proto google/protobuf/unittest_import.proto ../bootstrap/protoc-gen-swipl
# 	PATH=$(PATH_WITH_SWIPL) $(PROTOC) -I google/protobuf $(PROTOC_I) --swipl_out=. --python_out=. --plugin=protoc-gen-swipl=$(realpath ../bootstrap/protoc-gen-swipl) unittest_import_public.proto

.PHONY: test_interop_prolog
test_interop_prolog: test_interop.pl \
			scalars1a_from_cc.wire scalars1b_from_cc.wire scalars1c_from_cc.wire \
			scalars1a_from_python.wire scalars1b_from_python.wire scalars1c_from_python.wire \
			repeated1a_from_python.wire \
			packed1a_from_python.wire \
			google/protobuf/unittest_pb.pl \
			google/protobuf/unittest_import_pb.pl \
			google/protobuf/unittest_import_public_pb.pl \
			test_pb.pl test2_pb.pl test2b_pb.pl ../golden_message.2.5.0 ../test_protobufs.pl
	@# "LANG=C" is for testing that we're doing the right thing with encodings
	LANG=C $(SWIPL) test_interop.pl
	@# $(PROTOC) -I. --decode=test.Scalars1 test.proto <scalars1a_from_prolog.wire
	@# $(PROTOC) -I. --decode=test.Scalars1 test.proto <scalars1b_from_prolog.wire

.PHONY: test_read_python
test_read_python: test_read.py test_pb2.py \
		scalars1a_from_prolog.wire scalars1b_from_prolog.wire
	$(PYTHON) test_read.py

.PHONY: test_read_cc
test_read_cc: test_read scalars1a_from_prolog.wire scalars1b_from_prolog.wire
	./test_read

.PHONY: scalars1_wire
scalars1_wire: scalars1a_from_prolog.wire scalars1b_from_prolog.wire \
	scalars1a_from_prolog_template.wire scalars1b_from_prolog_template.wire \
	scalars1a_from_cc.wire scalars1b_from_cc.wire \
	scalars1a_from_python.wire scalars1b_from_python.wire \
	repeated1a_from_python.wire \
	packed1a_from_python.wire \
	repeated1a_from_prolog_template.wire \
	packed1a_from_prolog_template.wire
	@# scalars1a_from_prolog.wire etc have the fields in a different order,
	@# so they fail the diff. Their "equivalence" is tested elsewhere.
	diff scalars1a_from_prolog_template.wire scalars1a_from_cc.wire
	diff scalars1a_from_prolog_template.wire scalars1a_from_python.wire
	diff scalars1a_from_python.wire scalars1a_from_cc.wire
	diff scalars1b_from_prolog_template.wire scalars1b_from_cc.wire
	diff scalars1b_from_prolog_template.wire scalars1b_from_python.wire
	diff scalars1b_from_python.wire scalars1b_from_cc.wire
	diff repeated1a_from_prolog_template.wire repeated1a_from_python.wire
	diff packed1a_from_prolog_template.wire packed1a_from_python.wire

scalars1a_from_cc.wire scalars1b_from_cc.wire: test_write
	$(RM) scalars1a_from_cc.wire scalars1b_from_cc.wire
	./test_write
	@# $(PROTOC) -I. --decode=test.Scalars1 test.proto <scalars1a_from_cc.wire
	@# $(PROTOC) -I. --decode=test.Scalars1 test.proto <scalars1b_from_cc.wire

scalars1a_from_python.wire scalars1b_from_python.wire \
oneof1_from_python.wire map1_from_python.wire \
repeated1a_from_python.wire \
packed1a_from_python.wire: \
			test_write.py test_pb2.py test2_pb2.py
	$(RM) scalars1a_from_python.wire scalars1b_from_python.wire \
		oneof1_from_python.wire map1_from_python.wire \
		repeated1a_from_python.wire \
		packed1a_from_python.wire
	$(PYTHON) test_write.py
	@# $(PROTOC) -I. --decode=test.Scalars1 test.proto <scalars1a_from_python.wire
	@# $(PROTOC) -I. --decode=test.Scalars1 test.proto <scalars1b_from_python.wire

# Can use type -p with bash:
PROTOC_WHICH:=$(shell which protoc)
PROTOC_DIR:=$(shell dirname $(PROTOC_WHICH))
PROTOC_LIB:=$(shell realpath $(PROTOC_DIR)/../lib)

test_write: test.pb.o test_write.o
	@# See comment about -static with "foo" rule in ../demo/Makefile
	@# for non-static: -Wl,-rpath=$(PROTOC_LIB)
	@# $(CXX) -Wl,-rpath=$(PROTOC_LIB) -o $@ $^ $(CXXFLAGS) $(CPPFLAGS) $(LDFLAGS)
	$(CXX) -o $@ $^ $(CXXFLAGS) $(CPPFLAGS) $(LDFLAGS)

test_write.o: test.pb.h test.pb.cc test_write.cc

test_read: test.pb.o test_read.o
	@# See comment about -static with "foo" rule in ../demo/Makefile
	@# for non-static: -Wl,-rpath=$(PROTOC_LIB)
	@# $(CXX) -Wl,-rpath=$(PROTOC_LIB) -o $@ $^ $(CXXFLAGS) $(CPPFLAGS) $(LDFLAGS)
	$(CXX) -o $@ $^ $(CXXFLAGS) $(CPPFLAGS) $(LDFLAGS)

test_read.o: test.pb.h test.pb.cc test_read.cc

clean:
	$(RM) -r *_pb2.py *.wire *.o *_pb.pl *.pb.h *.pb.cc test_write test_read __pycache__ \
		google/protobuf/*_pb.pl google/protobuf/*_pb2.py addressbook.wire
	git clean -ndxf  # Should find nothing.

.PHONY: tar tkdiff
tar:
	$(MAKE) -C ../bootstrap tar

tkdiff:
	$(MAKE) -C ../bootstrap tkdiff

coverage:
	$(SWIPL) -g "show_coverage(run_tests, [dir(cov), annotate(true), ext('.cov'), modules([protobufs])])" -t halt test_interop.pl ../test_protobufs.pl
	@echo '****************************************************************'
	@echo '*                                                              *'
	@echo "* Be sure to comment out ':-use_module(library(apply_macros))' *"
	@echo '*                                                              *'
	@echo '****************************************************************'

# coverage_:
# 	cd .. && \
# 	$(SWIPL) -g "show_coverage(run_tests, [dir(cov), annotate(true), ext('.cov'), modules([protobufs])])" -t halt interop/test_interop.pl test_protobufs.pl
