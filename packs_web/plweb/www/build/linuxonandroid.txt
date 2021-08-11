# Building SWI-Prolog on Android using LinuxOnAndroid

There are currently two ways for running SWI-Prolog on Android.  Using
[Termux](https://termux.com/) we can do so without rooting the device.
This is probably the best and the subject of [this page](</build/Termux.html>)

This page describes the using [LinuxOnAndroid](http://linuxonandroid.org/),
which provides a complete Linux userland on your Android device running in
a _chroot_ environment alongside Android.  The installation requires
_rooting_ your device.

This procedure was tested on a Samsung GT-P5110 (Tab 2 10.1) running Android
4.1.2.

## Install Linux onto the target

I used [LinuxOnAndroid](http://linuxonandroid.org/).  This first requires you
to root your device, which took most of the time.  Of course, all the usual
`at your own risk' applies.  I installed Ubuntu 12.10.

## Install the requirements

This is very similar a normal build on [Debian/Ubuntu](</build/Debian.html>).
I left out JPL (Java), ODBC, ssl and utf8proc, but I see no reason why 
this would fail.

## Find a place for SWI-Prolog

The Ubuntu image itself is rather full (although it will fit).  Using the
external sdcard will not work because it is formatted
as FAT32 and mounted with the =noexec= flag.  If you have enough space, 
use a directory on the internal sdcard.  Else, what I did, was to make
2 partitions on the external sdcard.  The first is a FAT32 one that will
be used by Android.  The second can be formatted with a Linux filesystem
and mounted.  This is the easiest way to get more Linux native space that
I could find as it does not require any changes to the Android system
initialization.

P.s. With the above packages stripped and without C debug symbols, you
need about 300Mb free space.

## Building SWI-Prolog

There should be no issues building 6.4.x or 6.5.x.  Just follow the standard
procedure.  You can set =|MAKE=make --jobs=2|= to use the two cores of your
tablet.

## Tips

  - LinuxOnAndroid advices the use of androidVNC for accessing the Ubuntu subsystem.
  This is quite cumbersome for interaction with PceEmacs because Alt and Ctrl do not
  work.  I installed [Jump](http://jumpdesktop.com/).
  - [Hacker's keyboard](http://code.google.com/p/hackerskeyboard/) is also quite
  useful.
  - To do some real work, you'll need a real mouse and external keyboard.

## Conclusions

With an external mouse and keyboard, it works quite ok.  Speed on CHAT80 is about
1 million inferences/sec, or about 1/8 of my Intel i7 desktop.  You can run the
HTTP services and thus develop HTML5 apps that run entirely on your Android. I
think this should work on any device on which you can install Linux alongside
Android.

@see Termux.md for an Android port without rooting.

