---+ How do I change the fonts in PceEmacs and the debugger?

All graphical development tools are built using the XPCE graphics
library for SWI-Prolog. This system defines symbolic fontnames. For the
tools two fonts are important. The *fixed* font is used for running text
in editors and the *normal* font is used for pretty much everything
else.

To change anything, you have to create and adjust a personal XPCE
=Defaults= file. The easiest way is to launch PceEmacs using the
command

==
?- emacs.
==

And then select *|Edit/Editor preferences|*. If you do not have a
preferences file it will ask to create it and copy a default template to
the new file. Now you are ready for editing.

---++ Changing font sizes globally

Near the bottom of the default template you find

==
!font.scale: 1.4
==

You change the scaling factor for all fonts by removing the comment
character (!) and change the value 1.4. Bigger values give bigger fonts.

PceEmacs supports interactive rescaling of the editor using Control-,
Control+ and Control=.


---++ Changing specific fonts

Alternatively you may uncomment the entire *|display.system_fonts|*
block and change one or more of the definitions. You can start the XPCE
management window using =|?- manpce.|=, select *|File/Demo|* and then
*FontViewer* to see what fonts are by default available. You can also
mount any font available in the system using a system-specific font
identifier. This is totally different for X11 and MS-Windows and you are
requested to consult the XPCE User Guide for details, specifically
Specifying fonts and Accessing Windows Fonts.
