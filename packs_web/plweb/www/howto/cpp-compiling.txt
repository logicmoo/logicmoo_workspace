---+ How to compile/link/run C++ code with the C++ interface (for GNU/Linux)?

Well, this is actually quite easy, especially if you are used to the C interface. There are a few tricks though. In this HOWTO, we assume that you have never used the C interface, and that you are a beginner with the C++ interface.

---++ Files and tools necessary

---+++ The header =SWI-cpp.h=
For the developper, the C++ interface is merely one header: =SWI-cpp.h=. Its location depends on your version and distribution. For example, on Fedora 21, 64-bits, I found it in /usr/lib64/swipl-6.4.1/. Note that depending on your distribution, you may need to install an additional package to have it. For example under Fedora, =swipl= is provided by the package =pl= but the header is provided by =pl-devel=.

---+++ The linker and compiler wrapper =swipl-ld=
=swipl-ld= will help you compile and link your code without having to worry for the location of library files, or the compilation options.

---+++ The pkgconfig specification file =swipl.pc=
This file will help you to get the compilation options right.

---++ Makefile example

Here is a real world Makefile. Note that I need to give =swipl-ld= the option -ld g++ because otherwise it uses the C compiler which is unable to link with the C++ libraries. 

==
CXXFLAGS=--std=c++11 
CXXFLAGS+=$(shell pkg-config swipl --cflags)
LD=swipl-ld
LDFLAGS=--std=c++11 -shared -ld g++
SRC=$(wildcard *.cpp)
OBJ=$(SRC:.cpp=.o)
EXEC=interp.so
DESTDIR=..

all : $(EXEC)

$(EXEC) : $(OBJ)
	$(LD) $(LDFLAGS) -o $@ $^
	mv $(EXEC) $(DESTDIR)

database.o: database.cpp table.h table-kfdesc.h table-kprocs.h \
 table-kfs.h update-add-entry.h update.h update-delete-entry.h filemode.h \
 table-kmem.h database.h update-change-value.h
# ... a lot more of dependencies specification here

.PHONY : clean

clean :
	rm -rf *.o

mrproper : clean
	rm -f $(EXEC)
==

---++ FAQ

---+++ _How do I use the library? consult/1 doesn't work..._
Indeed. You have to use load_foreign_library/1. For example, if your library is =libs/mylib.so=, you have to do ?- load_foreign_library('libs/mylib'). 

---+++ _My projects compiles but Prolog complains about missing objects when I load it. Why?_
You have others, non-prolog, symbols that are still unresolved after loading the library in Prolog. You can remove the option -shared from =swipl-ld= to see those. The linker will complain about the missing function =main= (which is normal) and missing prolog symbols (which is normal). You can filter the output against the pattern "PL_" to get rid of the latter and see what are the others symbols that could not be resolved (which is NOT normal). 
