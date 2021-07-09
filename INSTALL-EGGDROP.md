#!/bin/false

exit 0

# Hacky webinstaller (Dont use)

```bash
source <(curl -sS https://logicmoo.org/gitlab/logicmoo/logicmoo_workspace/-/raw/master/web_install.sh)
```

# Eggdrop (Unsupported)
# ```bash

cd /opt/logicmoo_workspace#
chmod 777 ~prologmud_server/.bash_history
chmod 777 ~prologmud_server
USER=prologmud_server ; sudo chown -R $USER:$(id -gn $USER) /home/prologmud_server/.config ; echo $USER

(base) root@gitlab:/opt/logicmoo_workspace# su - prologmud_server
cd /opt/logicmoo_workspace/packs_sys/eggdrop/conf
prologmud_server@gitlab:/opt/logicmoo_workspace/packs_sys/eggdrop/conf$ eggdrop -m

Eggdrop v1.6.21 (C) 1997 Robey Pointer (C) 2011 Eggheads
[11:28:39] --- Loading eggdrop v1.6.21 (Mon Nov 16 2020)
[11:28:39] Listening at telnet port 3334 (users).
[11:28:39] Module loaded: dns
[11:28:39] Module loaded: channels
[11:28:39] Module loaded: server
[11:28:39] Module loaded: irc
[11:28:39] Module loaded: notes            (with lang support)
[11:28:39] Module loaded: seen
[11:28:39] Module loaded: blowfish
[11:28:39] Module loaded: assoc            (with lang support)
[11:28:39] Module loaded: wire             (with lang support)
[11:28:39] Module loaded: uptime
[11:28:39] Userinfo TCL v1.07 loaded (URL BF GF IRL EMAIL DOB PHONE ICQ).
[11:28:39] use '.help userinfo' for commands.
[11:28:39] Creating channel file


STARTING BOT IN USERFILE CREATION MODE.
Telnet to the bot and enter 'NEW' as your nickname.
OR go to IRC and type:  /msg PrologMUD hello
This will make the bot recognize you as the master.

[11:28:39] === PrologMUD: 0 channels, 0 users.
Launched into the background  (pid: 20926)

prologmud_server@gitlab:/opt/logicmoo_workspace/packs_sys/eggdrop/conf$ telnet localhost 3334
Trying 127.0.0.1...
Connected to localhost.
Escape character is '^]'.
PrologMUD  (Eggdrop v1.6.21 (C) 1997 Robey Pointer (C) 2011 Eggheads)

Please enter your nickname.
(If you are new, enter 'NEW' here.)
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

YOU ARE THE MASTER/OWNER ON THIS BOT NOW
From now on, you don't need to use the -m option to start the bot.
Enjoy !!
Okay, now choose and enter a password:
(Only the first 15 letters are significant.)
123456

Remember that!  You'll need it next time you log in.
You now have an account on PrologMUD...



Connected to PrologMUD, running eggdrop v1.6.21
     ____                __
    / __/___ _ ___ _ ___/ /____ ___   ___
   / _/ / _ `// _ `// _  // __// _ \ / _ \
  /___/ \_, / \_, / \_,_//_/   \___// .__/
       /___/ /___/                 /_/

Hey swipl!  My name is PrologMUD and I am running eggdrop v1.6.21, on Linux 5.4.0-53-generic.

Local time is now 11:31
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

get2react(['chon',"swipl","10"]).
### You have the following note(s) waiting:
   1. PrologMUD (Nov 16 11:31)
   2. PrologMUD (Nov 16 11:31)
### Use '.notes read' to read them.
*** swipl joined the party line.
.quit
get2react(['chof',"swipl","10"]).
*** Ja mata!
[11:31:58] DCC connection closed (swipl!telnet@127.0.0.1)
*** swipl left the party line.
Connection closed by foreign host.
prologmud_server@gitlab:/opt/logicmoo_workspace/packs_sys/eggdrop/conf$ telnet localhost 3334
Trying 127.0.0.1...
Connected to localhost.
Escape character is '^]'.
PrologMUD  (Eggdrop v1.6.21 (C) 1997 Robey Pointer (C) 2011 Eggheads)
Please enter your nickname.
(If you are new, enter 'NEW' here.)
swipl

Enter your password.
123456

Connected to PrologMUD, running eggdrop v1.6.21
     ____                __
    / __/___ _ ___ _ ___/ /____ ___   ___
   / _/ / _ `// _ `// _  // __// _ \ / _ \
  /___/ \_, / \_, / \_,_//_/   \___// .__/
       /___/ /___/                 /_/

Hey swipl!  My name is PrologMUD and I am running eggdrop v1.6.21, on Linux 5.4.0-53-generic.

Local time is now 11:32
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

get2react(['chon',"swipl","10"]).
### You have the following note(s) waiting:
   1. PrologMUD (Nov 16 11:31)
   2. PrologMUD (Nov 16 11:31)
### Use '.notes read' to read them.
*** swipl joined the party line.
.save
[11:32:29] #swipl# save
Saving user file...
.+chan #logicmoo
[11:32:43] #swipl# +chan #logicmoo
get2react(['join',"PrologMUD","~PrologMUD@c-73-67-179-188.hsd1.wa.comcast.net","*","#logicmoo"]).
[11:32:43] PrologMUD joined #logicmoo.
get2react(['topc',"*","*","*","#logicmoo","OpenCYC and https://t.me/LogicMoo  http://www.logicmoo.org | https://docs.google.com/document/d/1XCDnWMjNh4y2Um_1B-HE4x5Q34oxHRxBl-Oy9eNUzTI/edit#  https://drive.google.com/drive/folders/0B0QA19UX0ehlV1ZEaXEzc3hjTWM   http://www.csee.umbc.edu/~finin/prolog/pfc/man/pfc.pdf"]).
.+chan ##prolog
[11:32:54] #swipl# +chan ##prolog
get2react(['join',"PrologMUD","~PrologMUD@c-73-67-179-188.hsd1.wa.comcast.net","*","##prolog"]).
[11:32:55] PrologMUD joined ##prolog.
get2react(['topc',"*","*","*","##prolog"," ##prolog : PROgrammation en LOGique: | SLOW MOTION CHANNEL: Ask, wait; check back | Tutorial: Learn Prolog Now! <http://cs.union.edu/~striegnk/learn-prolog-now/> | <http://www.pathwayslms.com/swipltuts/student/index.html> | Try online: <http://swish.swi-prolog.org> | Channel bot help: ?help | Suggested coding practices: https://arxiv.org/pdf/0911.2899.pdf\"."]).
get2react(['notc',"ChanServ","ChanServ@services.","*","[##prolog] Please see the ##prolog FAQ at <http://www.pathwayslms.com/swipltuts/student/index.html>. Some more tutorials are available at <http://www.pathwayslms.com/swipltuts/>.","PrologMUD"]).
[11:32:56] -ChanServ (ChanServ@services.)- [##prolog] Please see the ##prolog FAQ at <http://www.pathwayslms.com/swipltuts/student/index.html>. Some more tutorials are available at <http://www.pathwayslms.com/swipltuts/>.
.save
[11:32:58] #swipl# save
Saving user file...
.reha[11:33:00] ##prolog is active but has no ops :(
sh
[11:33:01] #swipl# rehash
Rehashing.
[11:33:01] Rehashing ...
[11:33:01] Listening at telnet port 3334 (users).
[11:33:01] Userinfo TCL v1.07 loaded (URL BF GF IRL EMAIL DOB PHONE ICQ).
[11:33:01] use '.help userinfo' for commands.
[11:33:01] Userfile loaded, unpacking...
.quit
get2react(['chof',"swipl","10"]).
*** Ja mata!
[11:33:05] DCC connection closed (swipl!telnet@127.0.0.1)
*** swipl left the party line.
Connection closed by foreign host.
prologmud_server@gitlab:/opt/logicmoo_workspace/packs_sys/eggdrop/conf$
```
