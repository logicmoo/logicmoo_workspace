# SWI-Prolog SNAP images

[Snap](https://snapcraft.io)  is a  way  to distribute  applications  as
self-contained images.  This  allows  a  snap  to  work  on   any  Linux
distribution  that  has  `snapd`  installed    and   runs  a  compatible
architecture (currently amd64).

[Snaps for SWI-Prolog](https://snapcraft.io/swi-prolog)   are  available
since version 8.1.28. Snaps are distributed in the following _channels_:

  - __stable__ is the default and installs the latest stable release,
    currently 8.2.x.
  - __candidate__ and __beta__ will be used for the next stable cycle
    (8.4.0) to prepare the final next stable
  - __edge__ will follow the roughly biweekly releases of the
    _development_ cycle (8.3.x).

Thus, typically one would use

    snap install swi-prolog		# install stable release
    snap install --edge swi-prolog	# install devel release

The `swi-prolog` snap provides two commands,   the first being a console
application and the second the Qt window version.

    swi-prolog.swipl
    swi-prolog.swipl-win

You can get the usual command names `swipl` and `swipl-win` installed in
`/snap/bin` using these commands

    snap alias swi-prolog.swipl swipl
    snap alias swi-prolog.swipl-win swipl-win

Snaps have advantages and disadvantages. It should be easy enough to
find details about that. The snap is probably the most convenient way
for the casual user who wants a fairly up-to-date version and doesn't
like to go through the build process.

## Confined snap

The current snap is a _confined_ snap, which means it has only limited
access to your computer. This keeps the snap small as it is layered on
top of other snaps and brings some additional security, but also implies
(for example) that you cannot run arbitrary programs from the snap and
you cannot install [add-ons](</pack/list>) that contain components written
in e.g., C.


## Font issue

There is a glitch in current snap infrastructure for handling the
fontconfig library which is used by the xpce based IDE tools. You are a
victim if running `?- emacs.` results in the error below:


```
?- emacs.
[PCE fatal: @helvetica_roman_12/font: Xopen failed on @display/display
        in:     <No exception goal>
]
        [13] M @helvetica_roman_12/font ->_xopen(@display/display)
        [12] M @helvetica_roman_12/font ->_xopen(@display/display)
...
```

This can be resolved by cleaning the fontconfig global cache. Doing so
has no consequences except for the first application to use fontconfig
starting slow.

```
sudo rm /var/cache/fontconfig/*
```

## Status of the snap images

Snap images for SWI-Prolog are new.  We do not know whether or not there
are issues with them and we may change the packaging and confinement.
