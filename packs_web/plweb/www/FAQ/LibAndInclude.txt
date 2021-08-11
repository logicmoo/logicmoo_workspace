---+ Where are SWI-Prolog.h and the library files?

=|SWI-Prolog.h|= is in the include directory of the distribution. The
distribution is normally installed in C:\Program Files\pl on Windows and
/usr/local/lib/swipl-<version> on Unix.

On Unix, by default only the library for static linking libswipl.a is
installed in the =|lib/<arch>|= subdirectory of the installation. On
Windows, the import library swipl.lib is in the lib subfolder and
swipl.dll is in the bin subfolder.

To compile C/C++ objects you need to add the include subfolder to the
include directories of your compiler or you can use the =|swipl-ld|= utility.
The command below creates file.obj (Windows) or file.o (Unix), passing
the proper include directory to the C-compiler. This requires the
SWI-Prolog programs to be available from the PATH environment variable.

==
swipl-ld -c file.c
==

To link C/C++ objects to a shared-object (DLL) you need to links against
swipl.lib on Windows. On ELF based Unix platforms (most today), you do
not need an import library. Often, it is much easier to let =|swipl-ld|= do the
job as in the command below, which creates file.dll or file.so depending
on the platform.

==
swipl-ld -shared -o file file.o
==

To embed SWI-Prolog in a C/C++ main program you need to link against the
swipl library on all platforms. Again, =|swipl-ld|= simplifies the process.
The first command embeds the Prolog kernel in a C main program and the second attaches a Prolog state to the result, creating a stand-alone executable.

==
% swipl-ld -o myexe -nostate file.o ...
% swipl-ld -o myexe file.o ... file.pl ...
==

---++ Learning from swipl-ld.

If you don't want to use swipl-ld, you can learn from it by adding the -v
option to the commandline, which causes the program to print the
commands it runs to do its job.

@see   Current Unix installations also provide datafiles for =|pkg-config|=.
       see pkg-config(1) for details.
