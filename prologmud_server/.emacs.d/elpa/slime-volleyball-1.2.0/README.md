slime-volleyball.el
===================

This is Emacs Slime Volleyball.

![Emacs Slime Volleyball screenshot](emacs-slime-volleyball.png?raw=true)

Requirements
------------

* A fairly recent version of GNU Emacs, compiled with librsvg support

  I tested on GNU Emacs 24.3.50.1 (i686-pc-linux-gnu)

* A graphical Emacs session

* EMMS for sound support (optional, disabled by default)

* Fast graphics and CPU

Installation
------------

Add the following to your initialization file:

    (add-to-list 'load-path "/path/to/slime-volleyball")
    (require 'slime-volleyball)

Running
-------

First make sure the Emacs frame is fairly large; for example, press F11 to make
it fullscreen.  Then:

    M-x slime-volleyball

Have fun!
