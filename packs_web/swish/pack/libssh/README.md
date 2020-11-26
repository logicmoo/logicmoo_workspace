# A SWI-Prolog embedded SSH server

This    pack    provides    an    SSH      server     based    on    the
[libssh](https://libssh.org) library. On a connection the server creates
two threads, one pure C thread that is responsible for the communication
and a Prolog thread that connects to   the created _pseudo terminal_ and
runs  the  normal  Prolog  toplevel  prolog/0.  If  possible,  i.e.,  if
library(libedit) is used for  command  line   editing),  a  command line
editor is activated inside the thread   providing  the usual editing and
history.

## Authentication

The  library  currently   provides   SSH    public   key   and  password
authentication. The file examples/password.pl   illustrates how password
authentication can be set up. By default,   the system approves the keys
from ``~/.ssh/authorized_keys``, which allows  anyone   with  a key that
grants access to this acount to also access the Prolog server.

By default the server  binds  __only__   to  the  `localhost` interface.
Public access requires the option `bind_address(*)`.

## Host keys

SSH relies on a _host key_ to guarantee you are always connecting to the
same server. By default these keys are   found in `etc/ssh`. The default
configuration creates a set of keys if these keys do not exist using the
following commands

    mkdir -p etc/ssh
    ssh-keygen -A -f .

## Installation

The server may be installed using

    ?- pack_install(libssh).

## Prerequisites

  - CMake and the common build essentials
  - libssh development files:
    - Ubuntu: ``sudo apt install libshh-dev``
    - Fedora: ``sudo dnf install libshh-devel``
    - MacOS:  ``sudo port install libssh``
