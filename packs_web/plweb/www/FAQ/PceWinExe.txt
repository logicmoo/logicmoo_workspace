---+ Making an executable for XPCE/SWI-Prolog applications in MS-Windows

Such an executable consists of:

    * xpce-stub.exe followed by a saved-state. See also PceStubExe
    * at least the following DLLs
          * libpl.dll
          * pthreadVC.dll
          * pl2xpce.dll 
    * Any additional DLLs required by the program. You can find out
    which dlls are used by the program using current_foreign_library/2
    after loading the program. 

---++ Images and other resources

GUI applications often require image files and other resources. Normally
these files are stored in some directory, say icons. First of all, we
define this the location for our icons using

==
:- pce_image_directory(icons).
==

Next, we define images we want to use as SWI-Prolog resources. See
resource/3 in the SWI-Prolog reference manual for details. Assume we
have print.xpm in the icons directory and we want to make an image
thereof. This is done using the code below. The first fact declares the
resource print as a resource of type image whose data is in the file
image('print.xpm'). The file argument follows the files-specification
rules defined by absolute_file_name/3. The directive
pce_image_directory/1 above adds icons to the image search path as well
as to the XPCE search-path for images. Finally, resource(print) defines
the resource named print the source for the image.

==
resource(print, image, image('print.xpm')).

	...,
	new(X, image(resource(print)),
	...
==

Using these declarations your program can use image and other resource
data as usual. You can find many examples in the XPCE libraries by
searching for resource. You'll be happy to find out that images load
considerably faster from a saved-state than from files, so your
single-file executable starts quickly.

---++ Creating the EXE file

Finally, you must load all code into Prolog and create a saved-state.
The example below is the contents of the file save.pl used to create a
saved-state of a program normally loaded through load.pl. The
pce_autoload_all/0 directives load classes defined with :-
pce_autoload(Class, File). The emulator specifies the usage of the
PceStubExe emulator to avoid showing the SWI-Prolog console.

==
:- [load].

main :-
	pce_main_loop(main).

main(Argv) :-
	start application here, using passed arguments from Argv

save(Exe) :-
	pce_autoload_all,
	pce_autoload_all,
	qsave_program(Exe,
		      [ emulator(swi('bin/xpce-stub.exe')),
			stand_alone(true),
			goal(main)
		      ]).
==

For details on pce_main_loop/1, see the library(pce_main)

---++ Distributing the application

Normally, you want to create a folder C:\Program Files\MyApp on the
target machine holding MyApp.exe and the DLL files mentioned above. You
can distribute this as a ZIP file or use an installer. We use the NSIS
installer. This is an Open Source installer available from
http://nsis.sourceforge.net/

---++ Changing the shell icon

There are many ways to change the shell icon of an executable. Attached
you find one program I found on the web. It has no copyright statement
and I failed to reach the author. You can use this to replace the icon
in your executable with a Windows .ICO file:

==
chicon.exe saved.exe myapp.ico myapp.exe
==

  * [[chicon.exe][<chicon.exe>]]
  * [[chicon.c][<chicon.c>]]
