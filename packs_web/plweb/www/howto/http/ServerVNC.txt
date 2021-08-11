---+ Running the server under VNC (Unix)

A nice way to deploy the SWI-Prolog based servers is by using
[[VNC][http://en.wikipedia.org/wiki/Virtual_Network_Computing]]. We
refer to the Wikipedia page because there are various implementations of
VNC around. We use TightVNC, but the others will probably work fine as
well. VNC is a virtual desktop to which you can connect using a thin
client called =vncviewer=.  Starting the server is automatically done
in two steps:

  1. initd starts a VNC server under an unprivileged user.
  2. the .vnc/xtartup file of this user creates terminals in which
  the service(s) run.

The nice part is that you can connect to the server using =vncviewer=,
and access the Prolog toplevel to reload or debug the server.  You can
even run the graphical debugger, although tracing will be complicated
on a heavily loaded server.

---++ Setting up VNC on Debian/Ubuntu

Unfortunately, the setup requires root privileges and some Unix skills.
Below, we outline the steps for Debian and Debian derived systems (e.g.,
Ubuntu). This can serve as a starting point for all Unix-based systems.
We assume TightVNC, root privileges and these steps create a user
=wwwswi=.

  1. Install VNC if not already done.  On a Debian/Ubuntu system:

    ==
    % sudo apt-get install tightvncserver xtightvncviewer
    ==

  2. Make a new user.  Either using the GUI in modern systems or using
  =useradd=.  E.g.

    ==
    % sudo useradd -c "SWI-Prolog HTTP servers" -m -r -s /bin/bash wwwswi
    ==

  3. Setup VNC:

    ==
    % sudo -iu wwwswi bash
    $ unset XAUTHORITY
    $ vncserver

    You will require a password to access your desktops.
    Password:
    Verify:

    Would you like to enter a view-only password (y/n)? n
    ...
    New 'X' desktop is ct:1
    ==

    The =|unset XAUTHORITY|= is not really needed, but avoids a long
    timeout trying to establish X11 authorization with the original
    user.

  4. Connect to the server from any system using the command below.
     The =1= is the display returned by _|New 'X' desktop is ct:1|_

    ==
    % vncviewer <host>:1
    ==

    [[alert.gif]] Verify security and access restrictions.  VNC uses
    port 5900+display.  Login verification is challenge-response based,
    but further communication is not encrypted.  There are many ways to
    improve that, e.g., by using tunneling over SSH.

    [[alert.gif]] Since a long time, the default Gnome
    desktop in VNC has an annoying bug: typing a *d* closes all
    windows. Open "System/Preferences/Keyboard shortcuts" and disable or
    change the shortcut. I needed to stop and restart the server.
    Stopping can be done from the shell in (3) using =|vncserver -kill
    :1|=

  5. Install the server application in the new environment using the
     VNC desktop, for example installing ClioPatria.

    ==
    % git clone git://www.swi-prolog.org/home/pl/git/ClioPatria.git
    % mkdir test
    % cd test
    % ../ClioPatria/configure
    ==

    Optionally, you may wish to add a =run= script to restart the server
    in the case that it crashes.  E.g.

    ==
    #!/bin/bash
    cd $HOME/test
    while true; do
	./run.pl
	sleep 1
    done
    ==

  6. Further configure the application (may also be done later)

  7. Add the application to the startup of the server. You do this by
     editing $HOME/.vnc/xstartup, adding a line

    ==
    gnome-terminal -t "Demo Server" -e $HOME/test/run
    ==

    You can now test the restart by going to the shell you created in
    (3) and running these commands to stop and start the server.  You
    may need to adjust the display in the first command.

    ==
    % vncserver -kill :1
    % vncserver
    ==

    If everything works fine, the server nicely restarts and you can
    access it from your browser (ClioPatria runs on port 3020 by
    default, so the address is http://localhost:3020).  You can also
    contact the desktop using =vcnviewer= to type commands in the
    Prolog window.

  8. This last step makes the server start at boot-time.  For that you
     have to play with the system init scripts. Unfortunately, the
     details vary a lot between systems.  We'll limit this description
     to the three steps that are necessary in most of these systems.
     In the script fragments we use two environment variables: $VNCUSER
     is the user and $VNC_DISPLAY is the desired (fixed) display number.

	1. Status: detect whether a server is running.  We use a simple
	script called =lsvnc= for that.  Here is the content:

	==
	#!/bin/sh
	ps aux | \
		grep Xtightvnc | \
		grep ' :[0-9][0-9]* ' | \
		sed 's/\(^[a-z][a-z]*\).* \(:[0-9][0-9]*\) .*/\1 \2/g'
	==

	Using this, we can verify that the server is running using this
	bash fragment:

	==
	check()
	{ /usr/local/bin/lsvnc | grep -qw $VNCUSER
	}
	==

	2. Start: Start the server.  The first line removes stale VNC
	named pipes that sometimes survive on hard system crashes. The
	name varies a bit between VNC versions.

	==
	rm -f /tmp/.X11-unix/X$VNC_DISPLAY
	su $VNCUSER -c "cd /home/$VNCUSER && bin/server :$VNC_DISPLAY"
	==

	3. Stop: Stop the server.

	==
	su $VNC_USER -c "vncserver -kill :$VNC_DISPLAY"
	==

@see [[Example startup script used in Debian Squeeze][<vnc-ecdemo>]]
@see ServerInetd.txt for using the inet superdaemon
@see You probably want your server to appear on port 80,
[[here][Access80.txt]] is how.
