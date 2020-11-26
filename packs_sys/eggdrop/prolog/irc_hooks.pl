/* File irc_hooks.pl  */

:- module(irc_hooks,
            [on_irc_msg/3,
             on_irc_connect/1,
             reg_irc_hook/1,
             reg_irc_hook/2]).

:- meta_predicate 
   reg_irc_hook(:),
   reg_irc_hook(+,:).

%% on_irc_msg(+Chan,+Nick,+Text) is semidet.
% registerable hooks
% @depricated use reg_irc_hook/2

:- multifile(on_irc_msg/3). 
:- dynamic(on_irc_msg/3).
:- discontiguous(on_irc_msg/3).

:- multifile(on_irc_connect/1). 
:- dynamic(on_irc_connect/1).
:- discontiguous(on_irc_connect/1).

% arity 1
reg_irc_hook(M:(H:-B)):- ain(irc_hooks:H  :-  M:B).

% arity 2 version
reg_irc_hook(Name,M:Body):- ain(irc_hooks:Name  :-  M:Body).

