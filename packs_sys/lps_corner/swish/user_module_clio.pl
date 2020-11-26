%

:- multifile(prolog:message/3).
:- dynamic(prolog:message/3).

:- use_module(library(lists)).

%:- open('mylog.txt',write,S), assert(mylogFile(S)).

%mylog_s(autoload).
%mylog_s(load_file).
%mylog_s([error,existence_error]).
mylog_s([load_file,start]). 
mylog_s([load_file,done]). 
mylog_s([include_file,start]).
mylog_s([include_file,done]).
mylog_s([autoload,:]).
mylog_s([codewalk,reiterate]).
mylog_s([pldoc,invalid_comment]).
mylog_s([pack,attached]). 
mylog_s([check,undefined_procedures]).
mylog_s([make,library_index]).
mylog_s([frame,backtrace,[]]). 
mylog_s([declare_module,prolog_autoload]):- dumpST.
mylog_s([discontiguous|_]):- dumpST.
mylog_s([error,socket_error]):- dumpST.

%$skip_mylog([check,:,phil,setting_hplp(_128350,_128352),(meta_predicate setting_hplp(:,-))]):- dumpST.

mylog(M):- mylog_s(M),!.
mylog(M):- mylog1(M).

nonlist_compound(M):- compound(M), \+ is_list(M).

mylog1(M) :- nonlist_compound(M), M=..L,!,mylog1(L).
mylog1([M|R]):- nonlist_compound(M), M=..L,append(L,R,O),!,mylog1(O).
mylog1([F,M|R]):- nonlist_compound(M), M=..L,append([F|L],R,O),!,mylog1(O).
mylog1([F1,F2,M|R]):- nonlist_compound(M), M=..L,append([F1,F2|L],R,O),!,mylog1(O).
mylog1([F1,F2,F3,M|R]):- nonlist_compound(M), M=..L,append([F1,F2,F3|L],R,O),!,mylog1(O).
mylog1(M):- mylog2(M).

mylog2([S1,S2|R]):- (mylog_s([S1,S2]);mylog_s(S1);mylog_s(S2);mylog_s([S1|R])),!.
mylog2([S1,S2|R]) :- S=user_error, thread_self(T),format(S,'~N% ~q. ', [mylog_s([S1,S2])]),format(S,' ~q.    (~w) ~n', [mylog_s([S1|R]),T]),flush_output(S),!.
mylog2(M):- S=user_error, thread_self(T),format(S,'~N ~q.  (~w) ~n', [mylog_s(M),T]),flush_output(S).

sandbox:safe_primitive(user:mylog(_M)).
sandbox:safe_primitive(user:mylog2(_M)).


:- use_module(library(prolog_autoload)).
:- if( \+ exists_source(library(sldnfdraw))).
:- attach_packs('/opt/logicmoo_workspace/packs_lib').
:- endif.
:- if( \+ exists_source(library(lps_syntax))).
:- attach_packs('/opt/logicmoo_workspace/packs_web').
:- endif.


% :- use_module(library(logicmoo_utils_all)).
% :- autoload_all.


%:- asserta((prolog:message(A,_B,_C) :-  once(catch(mylog(A),_,true)), fail)).
%:- asserta((prolog:message(A,_B,_C) :-  mylog(A), fail)).
:- if( \+ clause(user:message_hook(A,_B,_C), (  mylog(A), fail))).
%:- asserta((user:message_hook(A,_B,_C) :-  mylog(A), fail)).
:- endif.

:- if( \+ current_module(lps_server_UI) ).
:- [user_module_file].
:- endif.

:- if( \+ current_module(swish_and_clio) ).
swish_and_clio:is_swish_and_clio.
:- ['../../swish/server'], call(server:server).
:- endif.

