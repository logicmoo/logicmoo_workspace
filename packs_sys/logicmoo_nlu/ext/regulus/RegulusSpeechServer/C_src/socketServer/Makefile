

ifeq ($(MACHINE_TYPE), win32)
TARGET = win32
else
TARGET = solaris
endif

default: $(TARGET)
	

win32:
	make -f Makefile.win32


solaris:
	/unige/gnu/bin/make -f Makefile.solaris

