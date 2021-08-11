# Using SSH to debug services and embedded Prolog

Many of us have experienced this problem: you have some process (for
example a web server) running as a service. For a modern Linux
distribution that implies it was started using `systemctl`. It runs
under some designated user id and its output is sent to the system log
infrastructure. You can look at this output using `journalctl`. What do
you do if it misbehaves?

If you are lucky, the log output gives a clue or produces enough
information to make an educated guess on how to trigger the same issue
while running the same service interactively on your development
machine. Then you can fix the software, upload it to the server machine
and use ``systemctl restart myservice`` to activate the patched version.
The only price paid is a short down time for the restart. The effect of
this may vary from zero impact if there are redundant services to a
serious down time if there is no redundancy and the start time is long.

## How can we improve?

Well, SWI-Prolog can recompile modified sources [safely into a running
process](https://www.swi-prolog.org/pldoc/man?section=loadrunningcode).
If the service was started with
[library(http/http_unix_daemon)](https://www.swi-prolog.org/pldoc/man?section=httpunixdaemon),
the default is to handle `SIGHUP` to run make/0 to reload modified
source files.  We can use that for two purposes:

  - If the log files do not provide sufficient detail, add debug/3
    statements to the code and run `systemctl reload myservice`
    to get more detailed information.  Do not forget to activate
    the debug topic by adding this to the modified source file

        :- debug(mytopic).

  - After we patched the sources locally or using the scenario
    above, use ``systemctl reload myservice`` to restart without
    downtime.

This may get clumsy though, in particular if we want to examine some
state or run some query in the context of the service. It can be done by
misusing directions as the above debug/1 call and capture the output in
the log files.  There must be something better, no?

## Using SSH

The Prolog pack [libssh](https://www.swi-prolog.org/pack/list?p=libssh)
allows you to create an SSH server in the Prolog process. Once that is
installed we can login to the server at any time. We are protected by
the de-facto standard and secure remote login protocol provided by SSH.
As we login to the server the server starts a thread that provides a
normal Prolog toplevel. If the (default)
[editline](https://www.swi-prolog.org/pldoc/man?section=editline)
command line editor is compiled into the server we have a fully
functional Prolog toplevel including command editing, completion,
history and color output.  This implies

  - We can easily examine global status such as global dynamic
    predicates, shared tables, etc.
  - We can run test queries against any predicate.  Such
    calls are executed in the context of the life service.  The
    full context of the server may be hard to reproduce on a
    development machine.  Surely it is wise to setup your
    development and maintenance system to be able to reproduce
    this environment as precisely as possible.  This may be costly
    or impossible though.
  - We can examine threads using threads/0 or tbacktrace/1.
  - We can kill a thread misbehaves using thread_signal/2 sending
    abort/0 as signal.
  - We can easily reload modified sources using make/0 doing the
    same as the above ``systemctl reload myservice`` in a bit more
    friendly way.
  - We can (de-)activate debug/3 statements by calling debug/1.
  - ... and much more ...

## Setting up ssh to allow login to a server

The first step to make all this magic possible is to install the
`libssh` package. To do so, you first need to install the dependencies.
On a Debian based Linux machine this implies:

    apt install build-essential ssh-client libssh-dev

Now start Prolog and install the ssh pack by running ``?-
pack_install(libssh).`` Below is the output of the install session. Make
sure there are no errors.

```
swipl
?- pack_install(libssh).
% Contacting server at https://www.swi-prolog.org/pack/query ... ok
Install libssh@0.9.1 from https://github.com/JanWielemaker/libssh/archive/V0.9.1.zip Y/n?
% Contacting server at https://www.swi-prolog.org/pack/query ... ok
% "V0.9.1.zip" was downloaded 16 times
Package:                libssh
Title:                  Provide an embedded SSH server
Installed version:      0.9.1
Author:                 Jan Wielemaker <jan@swi-prolog.org>
Home page:              https://github.com/JanWielemaker/libssh
Download URL:           https://github.com/JanWielemaker/libssh/archive/*.zip
Install "libssh-0.9.1.zip" (20,931 bytes) Y/n?
% -- The C compiler identification is GNU 10.2.0
% -- The CXX compiler identification is GNU 10.2.0
% -- Check for working C compiler: /usr/bin/X11/cc
% -- Check for working C compiler: /usr/bin/X11/cc -- works
% -- Detecting C compiler ABI info
% -- Detecting C compiler ABI info - done
% -- Detecting C compile features
% -- Detecting C compile features - done
% -- Check for working CXX compiler: /usr/bin/X11/c++
% -- Check for working CXX compiler: /usr/bin/X11/c++ -- works
% -- Detecting CXX compiler ABI info
% -- Detecting CXX compiler ABI info - done
% -- Detecting CXX compile features
% -- Detecting CXX compile features - done
% -- Found PkgConfig: /home/jan/bin/pkg-config (found version "0.29.2")
% -- Found LIBSWIPL: /usr/lib/libswipl.so
% -- Found LIBSSH: /usr/lib/x86_64-linux-gnu/libssh.so
% -- Looking for include file pty.h
% -- Looking for include file pty.h - found
% -- Looking for include file util.h
% -- Looking for include file util.h - not found
% -- Looking for gettid
% -- Looking for gettid - found
% -- Configuring done
% -- Generating done
% -- Build files have been written to: /home/jan/.local/share/swi-prolog/pack/libssh/build
% make[1]: Entering directory '/home/jan/.local/share/swi-prolog/pack/libssh/build'
% make[2]: Entering directory '/home/jan/.local/share/swi-prolog/pack/libssh/build'
% Scanning dependencies of target sshd4pl
% make[2]: Leaving directory '/home/jan/.local/share/swi-prolog/pack/libssh/build'
% make[2]: Entering directory '/home/jan/.local/share/swi-prolog/pack/libssh/build'
% [ 50%] Building C object CMakeFiles/sshd4pl.dir/c/sshd4pl.c.o
% [100%] Linking C shared module sshd4pl.so
% make[2]: Leaving directory '/home/jan/.local/share/swi-prolog/pack/libssh/build'
% [100%] Built target sshd4pl
% make[1]: Leaving directory '/home/jan/.local/share/swi-prolog/pack/libssh/build'
% make[1]: Entering directory '/home/jan/.local/share/swi-prolog/pack/libssh/build'
% make[2]: Entering directory '/home/jan/.local/share/swi-prolog/pack/libssh/build'
% make[2]: Leaving directory '/home/jan/.local/share/swi-prolog/pack/libssh/build'
% [100%] Built target sshd4pl
% make[1]: Leaving directory '/home/jan/.local/share/swi-prolog/pack/libssh/build'
% Install the project...
% -- Install configuration: ""
% -- Installing: /home/jan/.local/share/swi-prolog/pack/libssh/lib/x86_64-linux/sshd4pl.so
true.
```

## Preparing your ssh setup

We need to configure your ssh setup to be able to login to our server.
SSH manages this setup in the directory ``~/.ssh``. This directory must
contain a _private key_ and a matching _public key_ in the file
`authorized_keys`. If you use ssh regularly you probably have a key pair
and you only have to ensure the public key is also in `authorized_keys`.
You can do this with e.g.,

    cat .ssh/id_rsa.pub >> .ssh/authorized_keys

If you never used ssh you must create the setup from scratch, for
example using the commands below. Typically it is wise to set a
passphrase on the key.

    cd ~
    mkdir .ssh
    chmod 700 .ssh
    ssh-keygen
    <default answers, you may add a passphrase or not>
    cat .ssh/id_rsa.pub >> .ssh/authorized_keys

## Integrating SSHD into your Prolog server

The server is started with
[ssh_server/1](https://www.swi-prolog.org/pack/file_details/libssh/prolog/ssh_server.pl)
after loading `library(ssh_server)`. By default it listens to port 2020
on the `localhost` (`127.0.0.1`) interface and only allows for SSH
public key based login. Options may be used to change this. Thus,
normally the following suffices:

    :- use_module(library(ssh_server)).
    :- initialization(ssh_server([]), program).

A more elaborate example can be found with SWISH in
`config-available/sshd.pl`. The code is bwlow. The listen/2 method is
used to start the SSH server after the HTTP daemon software as prepared
all the steps to become a service process, notably it may have fork()ed
to become a background process. In this example we allow login from any
place that can access port 3250 on this machine and we provide an
explicit `authorized_keys` file holding all the public keys of sysadmins
and developers that are allowed to login to this service.


```
:- use_module(library(ssh_server)).
:- use_module(library(broadcast)).

:- listen(http(pre_server_start),
          start_sshd).

start_sshd :-
    ssh_server([ port(3250),
                 bind_address(*),
                 authorized_keys_file('etc/ssh/authorized_keys')
               ]).
```

## Login and logout

After all is setup, simply login using the command below.  Adjust the port
and host as required.

    ssh -p 2020 localhost

Here is a session with a login to [swish](https://swish.swi-prolog.org).
It shows the login to non-localhost using a passphrase. Then shows the
running threads and a problem: it appears possible that the thread that
garbage collects idle HTTP sessions can die, probably as the service
temporarily ran out of resources. Near the bottom we examine the status
of a thread by asking a backtrace for it.

To log out, use the end-of-file character (normally Control-D) or type

    ?- end_of_file.

Do __not__ use halt/0!


```
2_> ssh -p 3250 swish
Enter passphrase for key '/home/janw/.ssh/id_rsa':
X11 forwarding request failed on channel 0
Welcome to SWI-Prolog (threaded, 64 bits, version 8.3.17-6-g8b7f725d4)
SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software.
Please run ?- license. for legal details.

For online help and background, visit https://www.swi-prolog.org
For built-in help, use ?- help(Topic). or ?- apropos(Word).

?- threads.
%                       Thread Status       Time    Stack use    allocated
% ------------------------------------------------------------------------
%                         main running   192.053       76,776      186,176
%                           gc running 29040.846        1,200       87,872
%                  'sshd@3250' running     0.002          872      120,640
%                 'httpd@80_1' running 37484.365       10,352       87,872
%                 'httpd@80_2' running 38823.186       10,352       87,872
%                 'httpd@80_3' running 37921.349       10,352       87,872
%                 'httpd@80_4' running 38052.851       10,352       87,872
%                    'http@80' running   939.398       13,080      120,640
%                  swish_stats running  1646.630      439,104    1,333,056
%           '__http_scheduler' running   214.600       19,472      120,640
%                           12 running     0.000        2,224      120,640
%          '__http_session_gc' exception(error(resource_error(no_memory),context(system:thread_create/3,Resource temporarily unavailable)))
%                   swish_chat running   483.052      429,576      677,696
%      '__thread_pool_manager' running   290.363    1,803,656    2,676,544
%                           29 running     0.027       50,032      120,640
%                           48 running     0.001       11,024      120,640
%  <thread>(53,0x5650b53f46e0) exception(abort_query)
%                           72 running     0.001        3,568      120,640
%                           96 running     0.004       12,008      120,640
% <thread>(113,0x5650b53f4af0) exception(abort_query)
%                          137 running     0.000        2,224      120,640
%                          170 running     0.009       30,360      120,640
%                          190 running     0.001       12,008      120,640
%                          203 running     0.000        8,352      120,640
%                          234 running     0.005        7,144      120,640
%                          247 running     0.001        2,896      120,640
%                          253 running     0.008       56,896      120,640
%             'httpd@80_24424' running     1.762       10,576       87,872
%                          279 running     0.001        3,568      120,640
%                          289 running     0.634   79,887,088   88,088,544
%             'httpd@80_24489' running     0.953       10,576       87,872
%             'httpd@80_24503' running     1.008       10,576       87,872
%                          343 running     0.014        3,016       87,872
%                          349 running     0.017        3,088       87,872
%                          359 running     0.008       30,576      120,640
%              'swish@ssh/389' running     0.002       14,728       87,872
%                          406 running     0.009       11,024      120,640
%                          421 running     0.010       26,528      120,640
%             'httpd@80_24466' running    18.470       10,584       87,872
%             'httpd@80_24488' running     0.834       10,576       87,872
%                          461 running     0.001        3,568      120,640
%                          686 running     0.013       32,952      120,640
%             'httpd@80_24474' running     0.197       10,576       87,872
%                          750 running     0.000        2,224      120,640
% <thread>(752,0x5650b53f6cb0) exception(abort_query)
%             'httpd@80_24479' running     0.106       10,576       87,872
%                          795 running     0.018       69,672      120,640
%             'httpd@80_24469' running     3.198       10,576       87,872
%                          846 running     0.007       27,552      120,640
%                          855 running     0.000        2,224      120,640
%                          860 running     0.005       30,520      120,640
%                          874 running     0.000        2,224      120,640
true.

?- tbacktrace('httpd@80_24424').
   [6] thread_get_message('httpd@80',_24210,[timeout(10)])
   [5] thread_httpd:get_work('httpd@80',_24268,10) at /home/swish/lib/swipl/library/http/thread_httpd.pl:667
   [4] <meta call>
   [3] thread_idle(thread_httpd:get_work('httpd@80',_24332,10),long) <foreign>
   [2] thread_httpd:thread_repeat_wait(thread_httpd:get_work('httpd@80',_24392,10)) at /home/swish/lib/swipl/library/http/thread_httpd.pl:985
   [1] thread_httpd:http_worker([max_idle_time(10),...|...]) at /home/swish/lib/swipl/library/http/thread_httpd.pl:635
   [0] <meta call>
true.

?- ^D
Connection to swish.lxc closed.
```

### Future

Some things on the wish list

  - Open a break/0 environment in an arbitrary thread
  - Allow tracing some arbitrary thread, optionally using X11 forwarding
    to use the graphical debugger.
  - Support extensions such as `sftp` to update sources.

# Take home

The `libssh` package provides a secure mechanism to debug and maintain
Prolog programs that are not running interactively. This blog post
describes how to install and configure the library to get access to a
service process. You can also use these features to get access to a
Prolog process that is _embedded_ into some C++/Java/Python/...
application.

