.SUFFIXES: .proto .cpp .cc .o

.DEFAULT: foo

CC=gcc
CPLUSPLUSC=g++
PROTOC=protoc
RM=rm
SWIPL=swipl

CFLAGS=-Wall -O3
LDFLAGS=-lprotobuf -lpthread

.cpp.o:
	$(CPLUSPLUSC) -c $(CFLAGS) $*.cpp

.cc.o:
	$(CPLUSPLUSC) -c $(CFLAGS) $*.cc

.c.o:
	$(CC) -c $(CFLAGS) $*.c

pb-vector.pb.o: pb-vector.proto
	$(PROTOC) --cpp_out=. $?
	$(CPLUSPLUSC) -c $(CFLAGS) $*.cc

OBJS=pb-vector.pb.o foo.o

foo: $(OBJS)
	$(CPLUSPLUSC) -o $@ $(OBJS) $(LDFLAGS)

tmp99.tmp: vector_demo.pl
	$(SWIPL) -s vector_demo.pl -g make_tmp99 --

check: foo tmp99.tmp
	foo <tmp99.tmp

clean:
	$(RM) foo $(OBJS) *.pb.cc *.pb.h *.tmp
