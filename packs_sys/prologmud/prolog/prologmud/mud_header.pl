/* * module * 
% All modules are declared here so that this next lines dont have to be pasted into every file.
% Since this list will need at least 160 entries to cover the obj classes rooms and commands, 
% we add the modules here to not waste 160^2 lines of text and having to not 
% update 160+ files whenever a new module is used
%
% Logicmoo Project PrologMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/

% :- '$set_source_module'(baseKB).

% :- include(logicmoo(mpred/'mpred_header.pi')).

% :- '$current_source_module'(M),once(M==baseKB;on_x_log_cont(add_import_module(baseKB,M,end))).
:- use_module(library(pfc_lib)).

%:- kb_shared(get_session_id/1).
:- enable_mpred_expansion.
:- 
 current_prolog_flag(access_level,Was),
 set_prolog_flag(access_level,system),
 op(1190,xfx,('::::')),
 op(1180,xfx,('==>')),
 op(1170,xfx,'<==>'),  
 op(1160,xfx,('<-')),
 op(1150,xfx,'=>'),
 op(1140,xfx,'<='),
 op(1130,xfx,'<=>'), 
 op(600,yfx,'&'), 
 op(600,yfx,'v'),
 op(350,xfx,'xor'),
 op(300,fx,'~'),
 op(300,fx,'-'),
 op(1199,fx,('==>')),
 set_prolog_flag(access_level,Was).

:- style_check(-discontiguous).


