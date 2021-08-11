---+ Creating executables on Unix/Linux

Creating an executable that runs on the computer on which Prolog is installed is rather simple.  Below, we create =myexe= from =|file.pl|= and make it execute
=|hello/0|=.  The contents of =|hello.pl|= is given below

  ==
  % swipl -o myexe -g hello -c hello.pl
  % ./myexe
  Hello world!
  ==

---+++ hello.pl

  ==
  hello :-
      format('Hello world!~n'),
      halt.
  ==

---++ Stand-alone executables

The above creates a shell-script that calls the locally installed swipl executable.  Using the option =|--stand_alone=true|=, the executable 
becomes a copy of =|swipl|= with the state attached to it.  If the
SWI-Prolog kernel is statically linked (default on Linux/i386) and
the state does not use external packages that provide shared objects,
you are done.  Otherwise, you must make the shared objects available
and findable to make the program usable on another computer.

On linux, you find the dependent shared objects using =|ldd|=, e.g.,
the example below says that (among many system libraries), myexe
requires =|libswipl.so.5.11.15|=.

  ==
  % ldd myexe
  ...
  libswipl.so.5.11.15 => /home/jan/lib/swipl/lib/x86_64-linux/libswipl.so.5.11.15
  ...
  ==

=|libswipl.so.5.11.15|= must be bundled with your application and =|myexe|=
must be told where to find this file.  This is typically resolved either
by wrapping =myexe= in a shell-script that sets the environment variable
=|LD_LIBRARY_PATH|= or using chrpath(1) to change the location where
=myexe= looks for.

---++ Foreign packages

Many of the packages include a =|.so|= file that extends Prolog.  If your
program depends on that, you need to distribute the .so files used with
your application.  You can find the objects used with current_foreign_library/2.
By default, the objects are searched for with the alias =foreign= (see
file_search_path/2 and absolute_file_name/3).  You can alter this path
using the option =|-p foreign=/path/to/dir|=



