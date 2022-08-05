%:- if(( ( \+ ((current_prolog_flag(logicmoo_include,Call),Call))) )).
%:- module(mud_telnet,[]).

:- export((
         prolog_tnet_server/2,
         setup_streams/2,
         player_connect_menu/4,
         look_brief/1,
         cmdShowRoomGrid/1,
         get_session_id_local/1,
         inst_label/2,
         display_grid_labels/0,
         telnet_repl_writer/4,
         telnet_repl_obj_to_string/3,
         start_mud_telnet/0,
         start_mud_telnet/1,         
         get_session_io/2,
         kill_naughty_threads/0,
         set_player_telnet_options/1,
         register_player_stream_local/3,
         fmtevent/2,
         run_session/0,
         run_session/2,
         login_and_run_nodbg/0,
         login_and_run_debug/0,
         login_and_run_xhtml/0,
         login_and_run/0,
         login_and_run/2,
         session_loop/2,
         start_telnet/0
      )).

% Initial Telnet/Text console
% ALL telnet client business logic is here (removed from everywhere else!)
%
% Logicmoo Project PrologMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
:- include(prologmud(mud_header)).

:- use_module(mud_http_hmud).
%:- kb_shared(get_session_id/1).

 
:- dynamic((lmcache:agent_session/2,
      lmcache:session_agent/2,
      lmcache:session_io/4,
      lmcache:agent_session/2,
      lmcache:session_agent/2,
      lmcache:session_io/4)).

:- volatile((lmcache:agent_session/2,
      lmcache:session_agent/2,
      lmcache:session_io/4,
      lmcache:agent_session/2,
      lmcache:session_agent/2,
      lmcache:session_io/4)).

:- ain(mtProlog(mud_telnet)).
% UNDO % :- add_import_module(mud_telnet,world,end).

% learnLaterWhenToCallProceedure(What):- ... code ...

%:-ain(learnLaterWhenToCallProceedure(kill_naughty_threads)).

%:-ain(unimpledTODO(learnLaterWhenToCallProceedure)).
%:-ain(unimpledTODO(codeWithTODONextToIt)).

% instanceRecognizedBy(codeWithTODONextToIt,grovelSourceCodeLookingForComment).



kill_naughty_threads:-forall(thread_property(_,alias(ID)),sanify_thread(ID)).
% ignore main thread
sanify_thread(main):-!.
sanify_thread(ID):- ( \+ atom_concat('httpd',_,ID)),!,
   ignore(( thread_statistics(ID,local,Size),MSize is 200 * 1024, Size>MSize, dmsg(big_thread(ID,local,Size)))).
sanify_thread(ID):-
   ignore(( thread_statistics(ID,local,Size),MSize is 200 * 1024, Size>MSize,
     % thread_signal(ID,abort) maybe
     dmsg(killing_big_thread(ID,local,Size)), thread_exit(ID) )).


:- meta_predicate show_room_grid_single(*,*,0).

% :- include(prologmud(mud_header)).

% :- disable_mpreds_in_current_file.

% :- register_module_type (utility).

% :-  use_module(library(threadutil)).

% ===========================================================
% TELNET REPL + READER
% ===========================================================
start_mud_telnet :-  app_argv1('--nonet'),!.
start_mud_telnet:- 
  logicmoo_base_port(Base),
  WebPort is Base, % + 1000,
  whenever(run_network,start_mud_telnet(WebPort)).

port_busy(Port):-
    tcp_socket(ServerSocket),
    tcp_setopt(ServerSocket, reuseaddr),
    catch((tcp_bind(ServerSocket, Port),tcp_listen(ServerSocket, 5)),Error,true),    
    tcp_close_socket(ServerSocket),
    !,nonvar(Error).

:- dynamic(started_mud_telnet/1).
start_mud_telnet(_):- started_mud_telnet(_),!.
start_mud_telnet(Port):- port_busy(Port),!, NewPort is Port+100, start_mud_telnet(NewPort).
start_mud_telnet(Port):-      
      asserta(started_mud_telnet(Port)),
      start_tnet(login_and_run_nodbg , Port  , "MUD Server"),
      start_tnet(login_and_run_debug,  Port+1  , "MUD Debug"),
      start_tnet(login_and_run_xhtml,  Port+2  , "MUDLET Telnet"),
      start_tnet(               repl,  Port+3  , "WAM-CL Telnet"),
      % Port+4 = srv_mu
      % Port+23 = "screen -rx"
      start_tnet(             prolog,  Port+25 , "PROLOG Telnet"),
      !.

golorpish:- nodebugx(golorp).

:- dynamic(lmcache:main_thread_error_stream/1).
:- volatile(lmcache:main_thread_error_stream/1).

save_error_stream:- lmcache:main_thread_error_stream(_),!.
save_error_stream:- ignore((thread_self_main,(quintus:current_stream(2, write, Err),asserta(lmcache:main_thread_error_stream(Err))))).
:- initialization(save_error_stream,restore).
:- save_error_stream.

get_main_thread_error_stream(ES):-lmcache:main_thread_error_stream(ES),!.
get_main_thread_error_stream(Main_error):- stream_property(Main_error, file_no(2)).

get_session_io(In,Out):-
  get_session_id_local(O),
  thread_self(Id),
  lmcache:session_io(O,In,Out,Id),!.

get_session_io(In,Out):-
  must(get_session_id_local(O)),
  thread_self(Id),
  current_input(In),
  current_output(Out),
  asserta(lmcache:session_io(O,In,Out,Id)),!.

:- set_prolog_flag(debug_threads,false).
:- set_prolog_flag(debug_threads,true).

login_and_run_xhtml :- login_and_run.
%login_and_run_xhtml :- login_and_run_debug.

% login_and_run_nodbg:- current_prolog_flag(debug_threads,true),!,login_and_run.
login_and_run_nodbg:- 
   nodebugx(login_and_run),!.

login_and_run_debug:- \+ getenv('DISPLAY',_),login_and_run.
login_and_run_debug:- 
   thread_self(Self),
   tdebug(Self),% debug, % guitracer,
   % fav_debug,!,
   must_det(login_and_run),!.

get_session_id_local(O):- must(baseKB:get_session_id(O)),!.


ensure_player_attached(In,Out,P):-
  call_u((
    current_agent(P)->true;player_connect_menu(In,Out,_,P))).

player_connect_menu(In,Out,Wants,P):-
 setup_call_cleanup(set_local_modules(baseKB,Undo),
  must_det((   
   get_session_id_local(O),
   format('~N~nHello session ~q!~n',[O]),
   baseKB:foc_current_agent(Wants),
   % must((foc_current_agent(P),sanity(nonvar(P)))),
   must((baseKB:foc_current_agent(P),nonvar(P))),
   ain(isa(P,tHumanControlled)),
   register_player_stream_local(P,In,Out),
   format('~N~nWelcome to the MUD ~w!~n',[P]),
   format(Out,'~N~nThe stream ~w!~n',[Out]),
   colormsg([blink,fg(red)],"this is not blinking red!"),!)),Undo),!.

login_and_run_html:-
  login_and_run.

login_and_run:-
   get_session_io(In,Out),!,
   login_and_run(In,Out).

set_player_telnet_options(P):-
     ain(repl_writer(P,telnet_repl_writer)),
     ain(repl_to_string(P,telnet_repl_obj_to_string)),
   get_session_id_local(O),
   asserta(t_l:telnet_prefix(O,[P,wants,to])).

unset_player_telnet_options(P):-
     get_session_id_local(O),
     retractall(t_l:telnet_prefix(O,[P,wants,to])),
     clr(repl_writer(P,_)),
     clr(repl_to_string(P,_)).

goodbye_player:-
     call_u(foc_current_agent(P3)),
     deliver_event(P3,goodBye(P3)).

run_session:-
   get_session_io(In,Out),
   run_session(In,Out).

login_and_run(In,Out):-
  player_connect_menu(In,Out,_,_),!,
  run_session(In,Out).

set_local_modules(BaseKB,Undo):-
  '$current_typein_module'(WasTM),'$current_source_module'(WasSM),
  fileAssertMt(WasFileMt),
  module(BaseKB),'$set_typein_module'(BaseKB),'$set_source_module'(BaseKB),  
  set_fileAssertMt(BaseKB),!,
  Undo = (set_fileAssertMt(WasFileMt),'$set_typein_module'(WasTM),'$set_source_module'(WasSM)),!.
  % set_defaultAssertMt(BaseKB),
  

run_session(In,Out):-
 setup_call_cleanup(set_local_modules(baseKB,Undo),
  must_det((((     
     get_session_id_local(O),
     ensure_player_attached(In,Out,P),
     call(retractall,lmcache:wants_logout(O)))),!,
     register_player_stream_local(P,In,Out),
     call_u((repeat,
         once(session_loop(In,Out)),
         call(retract,lmcache:wants_logout(O)))),!,
      % this leaves the session
      call(retractall,lmcache:wants_logout(O)),
      ignore(current_agent(Agnt)->true;Agnt=P),
      deregister_player_stream_local(Agnt,In,Out))),Undo).


session_loop(In,Out):-
  must_det((((
  get_session_id_local(O),
  ensure_player_attached(In,Out,P),
  call_u(start_agent_action_thread),
  ignore(look_brief(P)),!,
  (t_l:telnet_prefix(O,Prefix)->(sformat(Prompt,'~w ~w>',[P,Prefix]));sformat(Prompt,'~w> ',[P])),
  prompt_read_telnet(In,Out,Prompt,List),!,
  enqueue_session_action(P,List,O))))).


:-export(register_player_stream_local/3).
register_player_stream_local(P,In,Out):-
  must_det((((
   set_player_telnet_options(P),
   get_session_id_local(O),thread_self(Id),
   retractall(lmcache:session_io(_,_,_,Id)),
   retractall(lmcache:session_io(O,_,_,_)),
   asserta_new(lmcache:session_io(O,In,Out,Id)),
  %  wdmsg(asserta_new(lmcache:session_io(O,In,Out,Id))),
   retractall(lmcache:session_agent(O,_)),
   asserta_new(lmcache:session_agent(O,P)),
   retractall(lmcache:agent_session(_,O)),
   asserta_new(lmcache:agent_session(P,O)),
   nop(check_console(Id,In,Out,_Err)))))).

deregister_player_stream_local(P,In,Out):-
  must_det((((
   unset_player_telnet_options(P),
   get_session_id_local(O),thread_self(Id),
   retractall(lmcache:session_io(_,_,_,Id)),
   retractall(lmcache:session_io(O,_,_,_)),
   %  wdmsg(asserta_new(lmcache:session_io(O,In,Out,Id))),
   retractall(lmcache:session_agent(O,_)),
   retractall(lmcache:agent_session(_,O)),
   nop(check_console(Id,In,Out,_Err)))))).

check_console(Id,In,Out,Err):-
    (thread_self_main->get_main_thread_error_stream(Err); Err=Out),
     (call(call,thread_util:has_console(Id,In, Out,Err))->true;
       ((call(retractall,thread_util:has_console(Id,_,_,_)),
          call(asserta,thread_util:has_console(Id,In,Out,Err))))).


:-export(enqueue_session_action/3).

enqueue_session_action(_A,[+, Text],_S):- string(Text), must(if_defined(assert_text(tWorld,Text))).
%enqueue_session_action(A,[W0,W1|WL],S):- string(Text),!,enqueue_session_action(A,[actSay,[W0,W1|WL]],S).
enqueue_session_action(A,L,S):- show_call(must(call_u(enqueue_agent_action(A,L,S)))),!.

setup_streams:-
  get_session_io(In,Out),
  setup_streams(In, Out),
  dmsg(call(call(listing,thread_util:has_console/4))).

setup_streams(In,Out):- var(In),!,current_input(In),setup_streams(In,Out).
setup_streams(In,Out):- var(Out),!,current_output(Out),setup_streams(In,Out).
setup_streams(In,Out):- thread_self(Id),
   thread_setup_streams(Id,In,Out).

thread_setup_streams(Id,In,Out):- memberchk(Id,[0,main]),thread_self_main,!,
   stream_property(Err,file_no(2)),
   call(retractall,thread_util:has_console(Id, _, _, _)),
   thread_at_exit(call(retractall,thread_util:has_console(Id, _, _, _))),
   call(asserta,thread_util:has_console(Id, In, Out, Err)),!.
thread_setup_streams(Id,In,Out):- 
   set_prolog_IO(In, Out, Out), thread_setup_streams(Id,In,Out,user_error).

thread_setup_streams(Id,In,Out,Err):-
 must_det_l((
   set_prolog_flag(color_term,true),
   set_prolog_flag(tty_control, true),   
   setup_stream_props(current_input,In),
   setup_stream_props(current_ouput,Out),   
   setup_error_stream(Id,Err))).

setup_stream_props(Name,Stream):-  
 must_det_l((
   set_stream_ice(Stream, close_on_exec(false)),
   set_stream_ice(Stream, close_on_abort(false)),
   %set_stream_ice(Stream, alias(Name)),
   current_prolog_flag(encoding, Enc),set_stream_ice(Stream, encoding(Enc)),
   set_stream_ice(Stream, type(text)),
   %stream_property(Stream,mode(_Dir)),
   %set_stream_ice(Stream, type(text)),
   %set_stream_ice(Stream, representation_errors(warn)),
   %set_stream_ice(Stream, write_errors(warn)),   
   %set_stream_ice(Stream, eof_action(eof_code)),
   %set_stream_ice(Stream, buffer_size(1)),   
   (stream_property(Stream,input)->set_stream_ice(Stream, newline(detect));true),
   set_stream_ice(Stream, tty(true)),
   nop(forall(stream_property(Stream,Prop),dmsg(stream_info(Name,Stream,Prop)))))).
   
   
find_err_from_out(Out,Err):-
 quintus:current_stream(N,write,Out),
 dif(Out,Err), 
 ((quintus:current_stream(N,write,Err),stream_property(Err,alias(user_error))) -> true ;
   ((quintus:current_stream(N,write,Err), \+ stream_property(Err,buffer(full)), \+ stream_property(Err,buffer(line))))).


setup_error_stream(Id,Err):-
  must_det_l((
   set_thread_error_stream(Id,Err),!,
   %current_prolog_flag(encoding, Enc),set_stream_ice(Err, encoding(Enc)),   
   %atom_concat(user_error_,Id,StreamName),set_stream_ice(Err, alias(StreamName)),
   %set_stream_ice(Err, alias(user_error)),
   set_stream_ice(Err, close_on_exec(false)),
   set_stream_ice(Err, close_on_abort(false)),
   set_stream_ice(Err, buffer(none)),
   set_stream_ice(Err, newline(dos)))),!.


fmtevent(Out,NewEvent):-string(NewEvent),!,format(Out,'~s',[NewEvent]).
fmtevent(Out,NewEvent):-format(Out,'~N~q.~n',[NewEvent]).

:-thread_local(t_l:telnet_prefix/2).

% :-set_tty_control(true).

:-export(prompt_read/4).
prompt_read_telnet(In,Out,Prompt,Atom):-
      get_session_id_local(O),
      prompt_read(In,Out,Prompt,IAtom),
      (IAtom==end_of_file -> (call(assert,lmcache:wants_logout(O)),Atom='quit') ; IAtom=Atom),!.

prompt_read(In,Out,Prompt,Atom):-
     with_output_to(Out,color_format([reset,hfg(white),bold],'~w',[Prompt])),flush_output(Out),
     repeat,read_code_list_or_next_command_with_prefix(In,Atom),!.

local_to_words_list(Atom,Words):-var(Atom),!,Words = Atom.
local_to_words_list(end_of_file,end_of_file):-!.
local_to_words_list([],[]):-!.
local_to_words_list(Atom,Words):-to_word_list(Atom,Words),!.

maybe_prepend_prefix(Words,Words).

read_code_list_or_next_command_with_prefix(In,Words):- read_code_list_or_next_command(In,Atom),
  add_history(Atom),
  %ignore(prolog:history(In, Atom);prolog:history(user_input, Atom);thread_signal(main,ignore(prolog:history(user_input, Atom)))),
  show_call(local_to_words_list(Atom,WordsM)),!,maybe_prepend_prefix(WordsM,Words).


read_code_list_or_next_command(Atom):-current_input(In),read_code_list_or_next_command(In,Atom),!.

read_code_list_or_next_command(In,end_of_file):- at_end_of_stream(In),!.
read_code_list_or_next_command(In,Atom):-
 (var(In)->current_input(In);true), catch(wait_for_input([In], Ready, 1),_,fail),!,  member(In,Ready),
  read_pending_input(In,CodesL,[]),!,is_list(CodesL),CodesL\==[],
   ((last(CodesL,EOL),member(EOL,[10,13])) -> code_list_to_next_command(CodesL,Atom);
    (read_line_to_codes(In,CodesR), (is_list(CodesR)-> (append(CodesL,CodesR,NewCodes),code_list_to_next_command(NewCodes,Atom)); Atom=CodesR))),!.

read_code_list_or_next_command(In,Atom):-
  read_pending_input(In,CodesL,[]),is_list(CodesL),CodesL\==[],
   ((last(CodesL,EOL),member(EOL,[10,13])) -> code_list_to_next_command(CodesL,Atom);
    (read_line_to_codes(In,CodesR), (is_list(CodesR)-> (append(CodesL,CodesR,NewCodes),code_list_to_next_command(NewCodes,Atom)); Atom=CodesR))),!.

code_list_to_next_command(end_of_file,end_of_file).
code_list_to_next_command(NewCodes,Atom):-append(Left,[EOL],NewCodes),EOL<33,!,code_list_to_next_command(Left,Atom).
code_list_to_next_command( [EOL|NewCodes],Atom):-EOL<33,!,code_list_to_next_command(NewCodes,Atom).
code_list_to_next_command( [],"l").
code_list_to_next_command( [91|REST],TERM):- on_x_fail((atom_codes(A,[91|REST]),atom_to_term(A,TERM,[]))),!.
code_list_to_next_command(NewCodes,Atom):-atom_codes(Atom,NewCodes),!.

:-export(scan_src_updates/0).

tick_tock:-
         scan_src_updates,!,fmt('tick tock',[]),sleep(0.1),!.

scan_src_updates:- !.
scan_src_updates:- ignore((thread_self_main,ignore((catch(mmake,E,dmsg(E)))))).


% ===========================================================
% DEFAULT TELNET "LOOK"
% ===========================================================

telnet_repl_writer(_TL,call,ftTerm,Goal):-!,ignore(on_x_debug(Goal)).
telnet_repl_writer( TL,text,Type,[V]):-telnet_repl_writer(TL,text,Type,V).
telnet_repl_writer( TL,text,Type,V):- is_list(V),merge_elements(V,L),V\=@=L,!,telnet_repl_writer( TL,text,Type,L).
telnet_repl_writer(_TL,text,Type,V):-copy_term(Type,TypeO),ignore(TypeO=t),fmt('text(~q).~n',[V]).
telnet_repl_writer(_TL,N,Type,V):-copy_term(Type,TypeO),ignore(TypeO=t),fmt('~q=(~w)~q.~n',[N,TypeO,V]).

telnet_repl_obj_to_string(O,_TypeHint,O):-!.
telnet_repl_obj_to_string(O,_TypeHint,S):- must(object_string(O,S)),!.
telnet_repl_obj_to_string(O,Type,toString(TypeO,O)):-copy_term(Type,TypeO),ignore(TypeO=s).


% ===========================================================
% DEFAULT TEXT
% ===========================================================
:- dynamic(baseKB:mudLastCommand/2).
look_brief(Agent):- prop(Agent,mudLastCommand,X),nonvar(X),functor(X,actLook,_),!.

look_brief(Agent):- !,call_u(look_as(Agent)),!.
look_brief(Agent):- \+ prop(Agent,mudNeedsLook,vTrue),!.
look_brief(Agent):- must(prop(Agent,mudNeedsLook,vTrue)),call_u(look_as(Agent)),!.

merge_elements(V,V):-not(is_list((V))),!.
merge_elements([],[]):-!.
merge_elements([E],[E]):-!.
merge_elements(V,V).
% merge_elements(V,M):-list_to_set(V,[E|More]),maplist(simply_ )..

% Display what the agent sees in a form which
% makes sense to me

write_pretty([]).
write_pretty(Percepts) :-
	write_pretty_aux(Percepts, Rest, 0),!,
	nl,
	write_pretty(Rest),!.

write_pretty_aux(Rest,Rest,5).
write_pretty_aux([[]|Tail],Return,Column) :-
	Ctemp is Column + 1,
	typeHasGlyph(Obj,0),
	write(Obj), write(' '),
	write_pretty_aux(Tail,Return,Ctemp).
write_pretty_aux([[vDark]|Tail],Return,Column) :-
	Ctemp is Column + 1,
	write('dk '),
	write_pretty_aux(Tail,Return,Ctemp).
write_pretty_aux([[Head]|Tail], Return, Column) :-
	Ctemp is Column + 1,
	typeHasGlyph(Map,Head),
	write(Map), write(' '),
	write_pretty_aux(Tail, Return, Ctemp).
write_pretty_aux([[Agent]|Tail],Return,Column) :-
	Ctemp is Column + 1,
	isa(Agent,tAgent),
	write('Ag'), write(' '),
	write_pretty_aux(Tail,Return,Ctemp).
write_pretty_aux([[_|_]|Tail],Return,Column) :-
	Ntemp is Column + 1,
	write('A+'), write(' '),
	write_pretty_aux(Tail,Return,Ntemp).




cmdShowRoomGrid(Room) :- ignore(show_room_grid_new(Room)),!.
% cmdShowRoomGrid(Room) :-show_room_grid_old(Room),!.

% ===================================================================
% show_room_grid_new(Room)
% ===================================================================
:-export(show_room_grid_new/1).
show_room_grid_new(Room):-
 call_u((
   grid_size(Room,Xs,Ys,_Zs),
   Ys1 is Ys+1,Xs1 is Xs+1,
   forall(between(0,Ys1,Y),
   ((nl,
   forall(between(0,Xs1,X),
   ((loc_to_xy(Room,X,Y,LOC),
   write(' '),
   OutsideTest = (not(between(1,Xs,X));not(between(1,Ys,Y))),
   once(show_room_grid_single(Room,LOC,OutsideTest)))))))))),!,nl.
show_room_grid_new(_):-nl.

door_label(R,Dir,'  '):- pathBetween_call(R,Dir,SP),atomic(SP).

asserted_atloc_for_map(O,L):-asserted_atloc(O,L),O\=apathFn(_,_).
asserted_atloc(O,L):-is_asserted(mudAtLoc(O,L)).

show_room_grid_single(Room, xyzFn(Room,X,Y,Z),OutsideTest):- call_u((call(OutsideTest), doorLocation(Room,X,Y,Z,Dir),door_label(Room,Dir,Label))),write(Label),!.
show_room_grid_single(_Room,_LOC,OutsideTest):- call(OutsideTest),!,write('[]'),!.
show_room_grid_single(_Room,LOC,_OutsideTest):- asserted_atloc_for_map(Obj,LOC),inst_label(Obj,Label), write(Label), !.
show_room_grid_single(_Room,LOC,_OutsideTest):- asserted_atloc_for_map(_Obj,LOC),write('..'), !.
show_room_grid_single(_Room,_LOC,_OutsideTest):- write('--'), !.

atom_label(SLabel,SLab2):- atom_concat('NPC0',L,SLabel),!,atom_label(L,SLab2),!.
atom_label(SLabel,SLab2):- atom_concat('NPC',L,SLabel),!,atom_label(L,SLab2),!.
atom_label(SLabel,SLab2):- once(i_name(SLabel,L)),L\=SLabel,atom_label(L,SLab2),!.
%atom_label(SLabel,SLab2):- sub_atom(SLabel,2,2,_,SLab2),!.
atom_label(SLabel,SLab2):- sub_atom(SLabel,1,2,_,SLab2),!.
atom_label(SLabel,SLab2):- sub_atom(SLabel,0,2,_,SLab2),!.

inst_label(Obj,Label):-  typeHasGlyph(Obj,Label),!.
inst_label(Obj,SLab2):-  atom(Obj),atom_label(Obj,SLab2).
inst_label(Obj,SLab2):-  term_to_atom(Obj,SLabel),atom_label(SLabel,SLab2).
inst_label(Obj,Label):-  iprops(Obj,nameString(Val)),Val\=Obj,inst_label(Val,Label),!.
inst_label(Obj,Label):-  iprops(Obj,mudNamed(Val)),Val\=Obj,!,inst_label(Val,Label),!.
inst_label(Obj,Label):-  iprops(Obj,isa(Val)),Val\=Obj,inst_label(Val,Label),!.
inst_label(_Obj,'&&').

% ===================================================================
% show_room_grid_old(Room)
% ===================================================================
% Display world
show_room_grid_old(Room) :-
	call_u(gridValue(Room,1,G,_)),
	length(G,N),
	M is N + 1,
	cmdShowRoomGrid(Room,1,1,M),!.

cmdShowRoomGrid(Room,Old,N,N) :-
	New is Old + 1,
	\+ call_u(gridValue(Room,New,N,_)),
	nl,
	!.

cmdShowRoomGrid(Room,Old,N,N) :-
	New is Old + 1,
	nl,
	!,
	cmdShowRoomGrid(Room,New,1,N).
cmdShowRoomGrid(Room,Y,X,N) :-
      loc_to_xy(Room,X,Y,LOC),
	asserted_atloc(Obj,LOC),
        props(Obj,isa(tAgent)),
	list_agents(Agents),
	obj_memb(Agent,Agents),
	asserted_atloc(Agent,LOC),
	write('Region1+'), write(' '),
	XX is X + 1,
	!,
	cmdShowRoomGrid(Room,Y,XX,N).
cmdShowRoomGrid(Room,Y,X,N) :-
        loc_to_xy(Room,X,Y,LOC),
	asserted_atloc(Obj,LOC),
        (isa(Obj,Class),
	typeHasGlyph(Label,Class)),!,
	write(Label), write(' '),
	XX is X + 1,
	!,
	cmdShowRoomGrid(Room,Y,XX,N).
cmdShowRoomGrid(Room,Y,X,N) :-
      loc_to_xy(Room,X,Y,LOC),
	asserted_atloc(Agent,LOC),
	isa(Agent,tAgent),!,
	write('Ag'), write(' '),
	XX is X + 1,
	!,
	cmdShowRoomGrid(Room,Y,XX,N).


% Used to display the labels of the grid locations. (the key to the map).
% Used at end of run.
display_grid_labels :-
	findall([Label,Name],typeHasGlyph(Name,Label),List),
	forall(prop_memb([Label,Name],List),
	           (write(Label), write('='), write(Name), write(' '))),
		   nl.






% :- include(prologmud(mud_footer)).



:- use_module(library(socket)).

%%	prolog_tnet_server(?Port, +Options)
%
%	Create a TCP/IP based server  on  the   given  Port,  so you can
%	telnet into Prolog and run an  interactive session. This library
%	is intended to provide access for   debugging  and management of
%	embedded servers.
%
%	Currently defined options are:
%
%		* allow(IP)
%		Allow access from IP, a term of the format ip(A,B,C,D).
%		Multiple of such terms can exist and access is granted
%		if the peer IP address unifies to one of them.  If no
%		allow option is provided access is only granted from
%		ip(127,0,0,1) (localhost).
%
%	For example:
%
%		==
%		?- prolog_tnet_server(4000, []).
%
%		% telnet localhost 4000
%		Welcome to the SWI-Prolog server on thread 3
%
%		1 ?-
%		==
%
%	@bug As the connection does not involve a terminal, command history
%	and completion are not provided. Neither are interrupts
%	(Control-C).  To terminate the Prolog shell one must enter the
%	command "end_of_file."

start_tnet(Call,Port,Description):- PortNum is Port,
  must(prolog_tnet_server(PortNum, [allow(_),call(Call),description(Description)])),!.

prolog_tnet_server(Port, Options):-  
 \+ member(alias(_),Options),
 option(call(Call),Options,mud_telnet),
 atomic_list_concat([Call,'_',Port],Alias),!, 
 prolog_tnet_server(Port, [alias(Alias)|Options]).

prolog_tnet_server(_Port, Options) :- 
  member(alias(Alias),Options),thread_property(Base, status(running)),Base==Alias,!.

prolog_tnet_server(Port, Options) :-
    tcp_socket(ServerSocket),
    tcp_setopt(ServerSocket, reuseaddr),
    tcp_bind(ServerSocket, Port),
    tcp_listen(ServerSocket, 5),
    option(alias(Alias),Options,prolog_tnet_server),
    option(description(Desc),Options,Alias),
    dmsg(Port=Desc),
    thread_create(mud_server_loop(ServerSocket, Options), _,
                  [ alias(Alias)
                  ]),!.

peer_to_host(Peer,Host):- catch(tcp_host_to_address(Host, Peer),_,fail),!.
peer_to_host(Peer,Host):- atom(Peer),Peer=Host,!.
peer_to_host(Peer,Host):- compound(Peer),catch((Peer=..PeerL,atomic_list_concat(PeerL,'.',Host)),_,fail),!.
peer_to_host(Peer,Host):- term_to_atom(Peer,Host),!.


mud_server_loop(ServerSocket, Options) :-
    tcp_accept(ServerSocket, ClientSock, Peer),
    tcp_open_socket(ClientSock, In, Out),
    set_stream(In, close_on_abort(false)),
    set_stream(Out, close_on_abort(false)),
    peer_to_host(Peer,Host),
    gensym(inst_,Num),
    option(alias(ServerAlias),Options,prolog_tnet_server),
    atomic_list_concat(['client_',Host,'_',Num, '@', ServerAlias], Alias),
    

    catch(thread_create(
              call_service_mud_client(Host, Alias, ClientSock, In, Out, Peer, Options),
              _,
              [ alias(Alias),detached(true)
              ]),
          error(permission_error(create, thread, Alias), _),
          fail),
    !,
    mud_server_loop(ServerSocket, Options).


call_service_mud_client(Host, Alias, ClientSock, In, Out, Peer, Options):-
  call(call,service_mud_client(Host, Alias, ClientSock, In, Out, Peer, Options)).

service_mud_client(Host,Alias,ClientSock,In,Out,Peer,Options) :-
    stream_property(Main_error, file_no(2)),
    option(allow(PeerAllow),Options,ip(127,0,0,1))-> PeerAllow=Peer,
    !,
    thread_self(Id),
    set_prolog_flag(tty_control, true),
    set_prolog_IO(In, Out, Out),    
    set_stream(In, tty(true)),
    % TODO figure out how to get immedate results
    % set_stream(In, buffer_size(1)),
    set_stream(user_output, tty(true)),
    set_stream(user_error, tty(true)),
    set_thread_error_stream(Id,user_error),
    current_prolog_flag(encoding, Enc),
    set_stream(user_input, encoding(Enc)),
    set_stream(user_output, encoding(Enc)),
    set_stream(user_error, encoding(Enc)),
    set_stream(user_input, newline(detect)),
    set_stream(user_output, newline(dos)),
    set_stream(user_error, newline(dos)),

    call(retractall,thread_util:has_console(Id, _, _, _)),
    thread_at_exit(call(retractall,thread_util:has_console(Id, _, _, _))),
    call(asserta,thread_util:has_console(Id, In, Out, Out)),

    option(call(Call), Options, prolog),
    format(Main_error,'~N~n~q~n~n',[service_mud_client_call(Call,Id,Alias,ClientSock,In,Out,Host,Peer,Options)]),
    format(user_error,
           'Welcome to the SWI-Prolog LogicMOO ~q on thread ~w~n~n',
           [Call,Id]),
    call_cleanup(Call,
                 ( close(In),
                   close(Out),
                   thread_detach(Id))).

service_mud_client(Host,Alias,ClientSock,In,Out,Peer,Options):-
    thread_self(Id),option(call(Call), Options, prolog),
    format(main_error,'~N~n~q~n~n',[rejecting(Call,Id,Alias,ClientSock,In,Out,Host,Peer,Options)]),    
    format(Out, 'Bye!!~n', []),
    close(In),
    close(Out),
    thread_detach(Id).


make_client_alias(Host,Alias):- thread_self(Prefix),make_client_alias3(Prefix,Host,Alias).

make_client_alias3(Prefix,Host,AliasH):- is_list(Host),must(atomic_list_concat([Prefix,'client'| Host], '.', AliasH)),!.
make_client_alias3(Prefix,Host,AliasH):- compound(Host),Host=..HostL,make_client_alias3(Prefix,HostL,AliasH).
make_client_alias3(Prefix,Host,AliasH):- term_to_atom(Host,AHost),must(atomic_list_concat([Prefix,'client', AHost], '_', AliasH)).


call_close_and_detatch(In, Out, Id, Call):-
    call_cleanup(call(Call),( close_connection(In, Out),ignore(thread_detach(Id)))).



close_connection(In, Out) :-
        call(retractall,thread_util:has_console(_,In,Out,_)),
        ignore(catch(close(In, [force(true)]),_,true)),
        ignore(catch(close(Out, [force(true)]),_,true)).

strm_info(Out,Name,Strm):-nl,write(Out,Name = Strm),forall(stream_property(Strm,P),'format'(Out,', ~q',[P])),nl(Out).


set_stream_ice(Stream, Alias, NV):- catch(set_stream(Alias,NV),_,catch(set_stream(Stream,NV),E,nop(dmsg(E)))).
set_stream_ice(Stream, NV):- catch(set_stream(Stream,NV),E,(dmsg(set_stream(Stream,NV,E)))).



baseKB:deliver_event_hooks(A,Event):-
   subst(Event,reciever,you,NewEventM),
   subst(NewEventM,A,you,NewEvent),
   foreach(no_repeats(call_u(get_agent_sessions(A,O))),
     foreach(no_repeats(lmcache:session_io(O,_In,Out,_Id)),
      fmtevent(Out,NewEvent))).

% correct_o_stream:-current_error(E),set_stream_ice(E).

start_telnet :-  app_argv('--notelnet'),!.
start_telnet :-
      % add_import_module(mud_telnet,baseKB,end),
      must(start_mud_telnet),
      nop(must(golorp_start)).


:- all_source_file_predicates_are_transparent.
:- fixup_exports.

golorp_start :- !.
golorp_start :- app_argv('--nogolorp'),!.
golorp_start:- logicmoo_base_port(Port),
    ensure_loaded('/opt/logicmoo_workspace/packs_xtra/logicmoo_packages/prolog/golorp/load'),
    start_tnet(golorpish,  Port+25 , "GOLORP Telnet").

:- after_boot(start_telnet).


