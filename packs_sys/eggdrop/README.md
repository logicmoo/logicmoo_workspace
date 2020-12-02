
# Eggdrop IRC/client

This pack provides library(eggdrop), which allows prolog code to use an existing IRC Client called Eggdrop.

The Eggdrop written in TCL/C passes IRC server events along to your event handles that you write in prolog.

Sorry this is not very user friendly but I use it in 3 places so I had to packify it

Such as the IRC chatroom ##prolog to let people test their basic code

```

[11:01] <dmiles_afk> ?- predicate_property(with_resource_limit(Call),imported_from(W)).
[11:01] <PrologMUD> dmiles_afk: Call=_, W=eggdrop.
[11:01] <PrologMUD> dmiles_afk: det(Yes,1)

```


 There is another very important IRC bot called yesbot (https://github.com/eazar001/yesbot) and that bot was designed to be more useable. 

Using this pack after it is installed...

1) Install.

 You must set up an Eggdrop (HOW TO: Setup up Eggdrop IRC bot easily http://forum.ircspeed.info/showthread.php?tid=73 or http://ubuntuforums.org/showthread.php?t=1028042 )

```

root@gitlab:~# apt-get install eggdrop

The following extra packages will be installed:
  eggdrop-data libtcl8.5 tcl8.5
Suggested packages:
  tcl-tclreadline
The following NEW packages will be installed:
  eggdrop eggdrop-data libtcl8.5 tcl8.5
0 upgraded, 4 newly installed, 0 to remove and 609 not upgraded.
Need to get 1,543 kB of archives.
After this operation, 5,711 kB of additional disk space will be used.
Do you want to continue? [Y/n] y

Processing triggers for menu (2.1.46ubuntu1) ...

```
2) Configure 

 Select a user to run the eggdrop (here I used 'dmiles') and 
 Edit the lib/swipl/pack/eggdrop/conf/eggdrop.conf file
 Edit the lib/swipl/pack/eggdrop/prolog/eggdrop.pl file (Like if you changes the port number)

3) Run..

```


 

dmiles@gitlab:~$ cd lib/swipl/pack/eggdrop
dmiles@gitlab:~/lib/swipl/pack/eggdrop$ eggdrop -m eggdrop.conf
dmiles@gitlab:~/lib/swipl/pack/eggdrop$ eggdrop -m eggdrop.conf

Eggdrop v1.6.19+SSL (C) 1997 Robey Pointer (C) 2008 Eggheads
[09:20] --- Loading eggdrop v1.6.19+SSL (Sun Nov  8 2015)
[09:20] Listening at telnet port 3334 (all).
[09:20] Module loaded: dns
[09:20] Module loaded: channels
[09:20] Module loaded: server
[09:20] Module loaded: irc
[09:20] Module loaded: notes            (with lang support)
[09:20] Module loaded: seen
[09:20] Module loaded: blowfish
[09:20] Module loaded: assoc            (with lang support)
[09:20] Module loaded: wire             (with lang support)
[09:20] Module loaded: uptime
[09:20] Userinfo TCL v1.07 loaded (URL BF GF IRL EMAIL DOB PHONE ICQ).
[09:20] use '.help userinfo' for commands.
[09:20] Creating channel file


STARTING BOT IN USERFILE CREATION MODE.
Telnet to the bot and enter 'NEW' as your nickname.
OR go to IRC and type:  /msg PrologMUD hello
This will make the bot recognize you as the master.

[09:20]  PrologMUD: 0 channels, 0 users.
Launched into the background  (pid: 27302)

dmiles@gitlab:~/lib/swipl/pack/eggdrop$ telnet localhost 3334
Trying 127.0.0.1...
Connected to localhost.
Escape character is '^]'.


PrologMUD  (Eggdrop v1.6.19+SSL (C) 1997 Robey Pointer (C) 2008 Eggheads)

Please enter your nickname.
(If you are new, enter 'NEW' here.)


```  
Type NEW and when it asks for a username you would use: swipl 
```


NEW
This is the telnet interface to PrologMUD, an eggdrop bot.
Don't abuse it, and it will be open for all your friends, too.
You now get to pick a nick to use on the bot,
and a password so nobody else can pretend to be you.
Please remember both!
Enter the nickname you would like to use.
swipl
*** POOF! ***
You are now a master on this bot.
### POOF! ###
You are now a botnet master on this bot.
@@@ POOF! @@@
You are now an OWNER of this bot.
From now on, you don't need to use the -m option to start the bot.
Enjoy !!
Okay, now choose and enter a password:
(Only the first 15 letters are significant.)

```
Make a password for the prolog client like: top5ecret
```

top5ecret

Remember that!  You'll need it next time you log in.
You now have an account on PrologMUD...



Connected to PrologMUD, running eggdrop v1.6.19+SSL
     ____                __
    / __/___ _ ___ _ ___/ /____ ___   ___
   / _/ / _ `// _ `// _  // __// _ \ / _ \
  /___/ \_, / \_, / \_,_//_/   \___// .__/
       /___/ /___/                 /_/

Hey swipl!  My name is PrologMUD and I am running eggdrop v1.6.19+SSL, on Linux 3.16.0-30-generic.

Local time is now 09:27
You are an owner of this bot. Only +n users can see this! For more info,
see .help set motd. Please edit the motd file in your bot's 'text'
directory.
Use .help for basic help.
Use .help <command> for help on a specific command.
Use .help all to get a full command list.
Use .help *somestring* to list any help texts containing "somestring".

Have fun.

Commands start with '.' (like '.quit' or '.help')
Everything else goes out to the party line.

get2react(['chon',"swipl","12"]).
### You have the following note(s) waiting:
   1. PrologMUD (Nov 08 09:24)
   2. PrologMUD (Nov 08 09:27)
### Use '.notes read' to read them.

*** swipl joined the party line.


```
Type: .quit 
```

.quit
get2react(['chof',"swipl","12"]).
*** Ja mata!
[09:27] DCC connection closed (swipl!telnet@localhost)
*** swipl left the party line.
Connection closed by foreign host.
dmiles@gitlab:~/lib/swipl/pack/eggdrop/conf$

```

Type: .quit Run Swi-Prolog

```
Welcome to SWI-Prolog (Multi-threaded, 64 bits, Version 7.3.10)
Copyright (c) 1990-2015 University of Amsterdam, VU Amsterdam
SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software,
and you are welcome to redistribute it under certain conditions.
Please visit http://www.swi-prolog.org for details.

For help, use ?- help(Topic). or ?- apropos(Word).

?- use_module(library(eggdrop)).
% /user/dmiles/lib/swipl/pack/logicmoo_base/prolog/logicmoo_utils.pl:204
% Adding logicmoo/utils to autoload path
true.

?- egg_go.
true.

?- % PrologMUD  (Eggdrop v1.6.19+SSL (C) 1997 Robey Pointer (C) 2008 Eggheads)
% Please enter your nickname.
% (If you are new, enter 'NEW' here.)
% Enter your password.ÿû
% ÿü
% Connected to PrologMUD, running eggdrop v1.6.19+SSL
%      ____                __
%     / __/___ _ ___ _ ___/ /____ ___   ___
%    / _/ / _ `// _ `// _  // __// _ \ / _ \
%   /___/ \_, / \_, / \_,_//_/   \___// .__/
%        /___/ /___/                 /_/
% Hey swipl!  My name is PrologMUD and I am running eggdrop v1.6.19+SSL, on Linux 3.13.0-32-generic.
% Local time is now 11:32
% You are an owner of this bot. Only +n users can see this! For more info,
% see .help set motd. Please edit the motd file in your bot's 'text'
% Use .help for basic help.
% Use .help <command> for help on a specific command.
% Use .help all to get a full command list.
% Use .help *somestring* to list any help texts containing "somestring".
% Have fun.
% Commands start with '.' (like '.quit' or '.help')
% Everything else goes out to the party line.
% maybe_call(get2react([chon,"swipl","12"])).
% You have no messages.
% *** swipl joined the party line.
% Echo turned off.
% Set your console to #logicmoo: - (none).

?- threads.
% Thread Status       Time    Stack use
% -------------------------------------
%   main running     0.914        6,168
%    pce running     0.007        1,088
% egg_go running     0.007       29,024
true.


```


 # Hopefully not *too much* goes wrong if tried.

# Some TODOs

Document this pack!

Write tests

Untangle the 'pack' install deps 
(Moving predicates over here from logicmoo_base)


# Not _obligated_ to maintain a git fork just to contribute

Dislike having tons of forks that are several commits behind the main git repo?

Be old school - Please ask to be added to logicmoo and Contribute directly !

Still, we wont stop you from doing it the Fork+PullRequest method

# [BSD 2-Clause License](LICENSE.md)

Copyright (c) 2017, 
logicmoo and Douglas Miles <logicmoo@gmail.com> 
All rights reserved.

