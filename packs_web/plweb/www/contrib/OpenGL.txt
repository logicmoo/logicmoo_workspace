---+ Interface to OpenGL

There are multiple interfaces to OpenGL.

---++ plOpenGL by Jan Tatham

  | Home      | http://www.sebity.com		   |
  | Binaries  | http://www.sebity.com/downloads.html |
  | Source  | https://github.com/sebity/plOpenGL |
  | License   | LGPL			           |
  | Platforms | MacOS, Linux, Windows		   |
  | Version    | 0.5.0		   |

---++ Copying from a message on the mailinglist

If anyone is interested, I have some dormant/old code for an OpenGL
binding to SWI.

The code hasn't been worked on for a year and I don't think I'll get
around to working on it much. However, there is enough to support
visual/glut window creation, drawing of standard shapes and some stuff
for resizing, etc. However, most open GL primitives, GLU and GLUT
functions are mapped to prolog. Needless to say there isn't any
documentation and the design could be improved ...

If anyone is interested in seeing this library completed please let me
know - I wasn't planning to, but if there is a need ...

Alternatively, if anyone wants to offer help / take over, feel free to
grab the source which is here:

    * http://www.ecs.soton.ac.uk/~pjt/Prolog/Prolog_OpenGL.tar.gz

The file below is a copy of the link above after removing the platform
specific (.so) file.

    * [[Prolog_OpenGL.tgz][<Prolog_OpenGL.tgz>]] (copied: Jul 8, 2009)

@author Phillip J Turner
