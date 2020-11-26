/* File irc_chat_client_foofy.pl   

:- pack_install(eggdrop). 

% Read README @ https://github.com/TeamSPoon/eggdrop

:- use_module(pack('eggdrop/t/foofy_bot')).

*/

:- module(foofy_bot,[go/0,wave/0]).

:- use_module(library(eggdrop)).

:- X is random(666),atom_concat('foof_',X,Nick),set_irc_nick(Nick).

:- set_irc_serv("irc.freenode.net":6667).

install_hello:- 
    reg_irc_hook(on_irc_msg(Channel,Nick,"hello"), 
              ( downcase_atom(Nick,DCNick), greet(DCNick,Channel) )).

greet(Nick,Channel):- say(["hello",Nick,"welcome to",Channel]).

irc_hooks:on_irc_msg(_, _,"bye"):- wave.  % confirm wave/0 is usable 
irc_hooks:on_irc_msg(_, _,"byebye"):- wave2.  % confirm wave2/0 is usable 

wave:- irc_action("waves").
wave2:- irc_action("waves2").

irc_hooks:on_irc_msg(_, _,"foofy"):- say("That is my name!").

:- reg_irc_hook((
                 on_irc_connect(_):- 
                   join("#foof_fun"),
                   say("I have arrived")
                )).

:- if(exists_source(eggdrop_fun(jokes))).
:- ensure_loaded(eggdrop_fun(jokes)).
:- endif.

go:- irc_connect,install_hello.

:- listing(irc_hooks:_).



/*

:- dynamic on_irc_connect/1.
:- multifile on_irc_connect/1.

on_irc_connect(_) :-
        foofy_bot:join("#foof_fun"),
        foofy_bot:say("I have arrived").

:- dynamic on_irc_msg/3.
:- multifile on_irc_msg/3.
:- module_transparent on_irc_msg/3.

on_irc_msg(_, _, _) :-
        eggdrop:fail.
on_irc_msg(_, _, "bye") :-
        foofy_bot:wave.
on_irc_msg(_, _, "byebye") :-
        foofy_bot:wave2.
on_irc_msg(_, _, "foofy") :-
        foofy_bot:say("That is my name!").

:- meta_predicate reg_irc_hook(:).

reg_irc_hook(B:(A:-C)) :-
        ain((irc_hooks:A:-B:C)).

:- multifile'$mode'/2.

'$mode'(on_irc_msg(+any, +any, +any), semidet).

:- multifile'$pldoc'/4.
:- module_transparent'$pldoc'/4.

'$pldoc'(on_irc_msg/3, '/mnt/gggg/logicmoo_workspace/pack/eggdrop/prolog/irc_hooks.pl':13, "registerable hooks.", "%% on_irc_msg(+Chan,+Nick,+Text) is semidet.\n% registerable hooks\n% @depricated use reg_irc_hook/2").
'$pldoc'(on_irc_msg/3, '/mnt/gggg/logicmoo_workspace/pack/eggdrop/prolog/eggdrop.pl':497, "Hook To [irc_hooks:on_irc_msg/3] For Module Eggdrop.", "%% irc_hooks:on_irc_msg( ?Channel, ?User, ?Stuff) is det.\n%\n% Hook To [irc_hooks:on_irc_msg/3] For Module Eggdrop.\n% Irc Event Hooks.\n%").

:- meta_predicate reg_irc_hook(+,:).

reg_irc_hook(A, B:C) :-
        ain((irc_hooks:A:-B:C)).
true.


*/



