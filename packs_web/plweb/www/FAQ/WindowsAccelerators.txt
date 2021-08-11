---+ Windows keyboard accelerators in XPCE

XPCE and SWI-Prolog started in the Unix/X11 and Free Software
environment, when `Emacs' was the dominant standard for keyboard
accelerators.

As of SWI-Prolog 5.0.9/XPCE 6.0.9 the system provides infra-structure
for platform-dependant accelerators and runtime switching between
defined schemas. In these versions the default on Windows is to use CUA
bindings and on Unix/X11 to use Emacs bindings. If you want to use CUA
on Unix or Emacs on Windows you can set the class-variable
=|key_binding.style|= in your =|~/.xpce/Defaults|= file. The following
defines you want emacs mode, regardless of the platform.

==
key_binding.style: emacs 
==

@see PceFonts.txt for more info on changing XPCE defaults.
