---+ Library not loaded: /usr/X11/lib/libX11.6.dylib

System: MacOSX

When starting xpce (through e.g., help/0, gtrace/0, emacs/0, etc.), a message
like below appears.  This message is often the first of a long list.

  ==
  ERROR: /opt/local/lib/swipl-5.10.2/xpce/prolog/boot/pce_principal.pl:96:
        '$open_shared_object'/3: dlopen(/opt/local/lib/swipl-5.10.2/xpce/lib/ 
i386-darwin9.8.0/pl2xpce.dylib, 1): Library not loaded: /usr/X11/lib/ 
libX11.6.dylib
   Referenced from: /opt/local/lib/swipl-5.10.2/xpce/lib/i386- 
darwin9.8.0/pl2xpce.dylib
   Reason: Incompatible library version: pl2xpce.dylib requires  
version 10.0.0 or later, but libX11.6.dylib provides version 9.0.0
  ==

---++ Why?

There are number of X11 implementations around for the Mac and some of these use different versions of the X11 libraries.

---++ What to do?

There are two options:

   1. Build SWI-Prolog from the source, either using Macports or from the
   source tar-ball or git repository.  It is a bit of work the first time,
   but if you plan to use SWI-Prolog for a longer time it will probably
   pay off.

   2. Install a matching version of X11.  The download page and installers
   describe the version of X11 that is expected.
