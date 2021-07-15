# -*- encoding: utf-8 -*-
#	$Id: README,v 1.1.6.2 2018/03/08 06:10:11 doug Exp $	

The intents of cpio-mode are the following:
• It should be as much like dired as possible.¹
  (The current keymap effectively duplicated dired's.)
• It should be easy to add new archive formats to it.
  That is, in particular, it should admit, for any inode-like archive format,
  an absract interface that leaves the the UI intact
  and independent of the underlying format.
  This should, for example, allow 
  for converting between and among archive formats nearly trivially.

It should also allow for dired-like functions.
Specifically, those things that dired within emacs allows.
That includes things like
• Adding/deleting entries
• Editing entries
• Changing an entry's attributes (chown, chgrp, chmod, etc.).

To add a new archive type a devloper should be able to do so
merely by being able to parse an entry and
write a parsed entry back to a file.
 
Right now (2018 May 11), the cpio-mode package supports the above
for the "newc" format of cpio archives.
However, the internal structure of cpio-mode implements
all of the manipulation code in terms of parsed headers
(which look much like inodes), so adding new formats should be
relatively easy.
See the documentation in cpio.el
for a slightly more detailed description of this structure.

There's also a package of Affiliated Buffers included
that should be independent.
(And one day it will be published that way.)

________________
¹Yes, this is a terrible requirement.
 However, it does allow for incremental development relatively easily.
