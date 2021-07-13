/*
% NomicMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
% Bits and pieces:
%
% LogicMOO, Inform7, FROLOG, Guncho, PrologMUD and Marty's Prolog Adventure Prototype
%
% Copyright (C) 2004 Marty White under the GNU GPL
% Sept 20, 1999 - Douglas Miles
% July 10, 1996 - John Eikenberry
%
% Logicmoo Project changes:
%
% Main file.
%
*/
% Marty's Tokenizer/Scanner/Lexer, written in Prolog.
:- '$set_source_module'(mu).
/*
:- module(adv_io, [
 read_line_to_tokens/4,
 clear_already_consumed_input/1,
 is_main_console/0,
 overwrote_prompt/1, ensure_has_prompt/1,
 player_format/2,
 player_format/3,
 dbug/2,
 dbug/1,
 dbug/3,
 with_tty/2,
 pprint/2,
 init_logging/0,
 stop_logging/0,
 bug/1,
 agent_to_input/2,
 agent_to_output/2,
 get_already_consumed_input/2,
 reshow_already_consumed_input/1,
 %setup_console/0,
 setup_console/1,

 get_current_portray_level/1,

 current_error_io/1, adv_set_error/1, redirect_error_to_string/2

   /*post_message/1,
   post_message/2,
   sv_message/2,
   svo_message/3,
   svi_message/3,
   svoi_message/4, */ ]).
*/
:- dynamic(mu_global:wants_quit/3).
:- dynamic(mu_global:console_tokens/2).
:- dynamic(mu_global:console_io_player/3).
:- volatile(mu_global:console_io_player/3).



current_error_io(Stream) :- stream_property(Stream, alias(user_error)), !. % force det.
current_error_io(Stream) :- stream_property(Stream, alias(current_error)), !. % force det.
current_error_io(Stream) :- stream_property(Stream, file_no(2)), !. % force det.
adv_set_error(Stream) :- set_stream(Stream, alias(user_error)).

:- meta_predicate redirect_error_to_string(0, -).
redirect_error_to_string(Goal, String) :-
  current_error_io(OldErr),
  new_memory_file(Handle),
  setup_call_cleanup(
   open_memory_file(Handle, write, Err),
   setup_call_cleanup(
    adv_set_error(Err),
    (once(Goal),
     flush_output(Err)),
    adv_set_error(OldErr)),
   close(Err)),
  memory_file_to_string(Handle, String).


%user:
setup_console :- current_input(In), setup_console(In).

:- dynamic(mu_global:has_setup_setup_console/1).
:- volatile(mu_global:has_setup_setup_console/1).

setup_console(In):- mu_global:has_setup_setup_console(In), !.
setup_console(In):-
 assert(mu_global:has_setup_setup_console(In)),
 set_prolog_flag(color_term, true),
 ensure_loaded(library(prolog_history)),
 %(current_prolog_flag(readline, X)-> ensure_loaded(library(X));ensure_loaded(library(editline))),
 %ensure_loaded(library(editline)),
 '$toplevel':(

  setup_colors,
  setup_history,
  setup_readline), !.


:- dynamic(mu_global:input_log/1).
init_logging :- !.
init_logging :-
 get_time(StartTime),
 convert_time(StartTime, StartTimeString),
 open('~/.nomic_mu_input.log', append, FH),
 format(FH, '\n==== ADVENTURE INPUT, ~w\n', [StartTimeString]),
 asserta(mu_global:input_log(FH)).
stop_logging :-
 mu_global:input_log(FH) -> close(FH) ; true.

% :- dynamic(bugs/1). % Types of logging output.
%bugs([general, printer, planner, autonomous]).
bug_contexts([always, general, planner, autonomous, telnet, general, parser, printer]).
:- bug_contexts(List), foreach(member(E, List), debug(adv(E))).
:- debug(adv_skip(printer)).
:- debug(adv(unknown)).
:- debug(adv(planner)).
:- nodebug(adv(unknown)).
:- debug(adv(all)).
%bugs([general, autonomous]).

bug(B) :- (B == always;B==general), !.
bug(B) :- debugging(B, false), !, fail.
bug(B) :- debugging(adv_skip(B), true), !, fail.
bug(_) :- debugging(adv_skip(all), true), !, fail.
bug(_) :- debugging(adv(all)), !.
bug(B) :- debugging(adv(B), YN), !, YN.
bug(_) :- debugging(adv(unknown), YN), !, YN.

:- thread_local(pretty_tl:in_pretty_tree/0).
:- thread_local(pretty_tl:in_pretty_tree_rec/0).

prolog_pprint_tree(Term):- \+ pretty_tl:in_pretty_tree, !,
  setup_call_cleanup(asserta(pretty_tl:in_pretty_tree, Ref), \+ \+ print_tree(Term), erase(Ref)).
prolog_pprint_tree(Term):- \+ pretty_tl:in_pretty_tree_rec, !,
  setup_call_cleanup(asserta(pretty_tl:in_pretty_tree_rec, Ref), prolog_pprint(Term, [portray_goal(print_tree)]), erase(Ref)).
prolog_pprint_tree(Term):-  prolog_pprint(Term), !.


adv_term_to_pretty_string(L, LinePrefix, SO):-
  string_concat("\n", LinePrefix, SC),
  sformat(S, '~@', [prolog_pprint_tree(L)]),
  split_string(S, "", "\s\t\n", [SS]),
  replace_in_string("\n", SC, SS, SSS),
  string_concat(LinePrefix, SSS, SO).

/*
:- export(prolog_pprint/2).
prolog_pprint(Term):- prolog_pprint(Term, []).
prolog_pprint(Term, Options):-
   \+ \+ ((portray_vars:pretty_numbervars(Term, Term2), prolog_pprint_0(Term2, Options))), !.


% prolog_pprint_0(Term, Options):- Options ==[], pprint_ecp_cmt(blue, Term), !.

% prolog_pprint_0(Term, Options):- memberchk(portray(true), Options), \+ is_list(Term), \+ memberchk(portray_goal(_), Options), print_tree(Term, Options), !.
prolog_pprint_0(Term, Options):-    \+ memberchk(right_margin(_), Options), !, prolog_pprint_0(Term, [right_margin(60)|Options]).
prolog_pprint_0(Term, Options):-    \+ memberchk(portray(_), Options), !, prolog_pprint_0(Term, [portray(true)|Options]).
prolog_pprint_0(Term, Options):- prolog_pretty_print:print_term(Term, [output(current_output)|Options]).
*/
:- thread_local(t_l:no_english/0).


atom_occurs(SS,Sep,N):- atomic_list_concat([_|L],Sep,SS), length(L,N).

print_tree_at_depth(Tree,N):- 
  wots(SSS,print_tree(Tree,[html_depth(N)])),
  prepend_trim(SSS,SS),write(SS).


%:- mu:ensure_loaded(adv_debug).
pprint(Term):- xnotrace(pprint_2(Term, always)).
pprint(Term, When):- xnotrace(pprint_2(Term, When)).
pprint_2(Term, When):- ignore(\+ pprint_3(Term, When)).
pprint_3(Term, When) :- bug(When), !,
 setup_call_cleanup(
  flag('english', ELevel, ELevel+0), % put a little English on it
  player_format('~N~@~N', [maybe_bfly_html(dbug1_2(Term))]),
  flag('english', _, ELevel)), !.

dbug1_2(Tree):-
  wots(SSS,in_bfly(f,print_tree_at_depth(Tree,0))),
  prepend_trim(SSS,SS), 
  atom_occurs(SS,'\n',N),
  (N =< 2 -> bugout4("", '~N~@~n', [mu:print_tree_at_depth(Tree,0)], always);
             bugout4("", '~N~@~n', [mu:print_tree_at_depth(Tree,2)], always)).

dbug1_1(Tree):-
  wots(SSS,in_bfly(f,print_tree_at_depth(Tree,0))),
  prepend_trim(SSS,SS), 
  atom_occurs(SS,'\n',N),
  (N =< 1 -> bugout4("", '~N/* ~@ */~n', [mu:print_tree_at_depth(Tree,0)], always);
             bugout4("", '~N/* ~@ */~n', [mu:print_tree_at_depth(Tree,1)], always)).


dbug1(_):- notrace(current_prolog_flag(dmsg_level, never)), !.
dbug1(Fmt) :- compound(Fmt), compound_name_arity(Fmt,F,_), debugging(F,false), !.
dbug1(Fmt) :-
 quietly(( \+ \+
   ((mu:simplify_dbug(Fmt, FmtSS),     
     portray_vars:pretty_numbervars(FmtSS, FmtS),
     locally_tl(no_english,maybe_bfly_html(dbug1_1(FmtS))))))).


dbug(DebugDest, Fmt) :-
  xnotrace((compound(Fmt) -> dbug_3(DebugDest, '~p', Fmt) ; dbug_3(DebugDest, Fmt, []))).

dbug(DebugDest, Fmt, Args0):-
 xnotrace(dbug_3(DebugDest, Fmt, Args0)).

dbug_3(DebugDest, Fmt, Args0) :-
  listify(Args0, Args),
  \+ \+
     ((must_maplist(mu:simplify_dbug, Args, ArgsS),
       portray_vars:pretty_numbervars(ArgsS, ArgsSO),
       bugout4("", Fmt, ArgsSO, DebugDest))).


bugout4(Prefix, Fmt, Args, DebugDest) :- toplevel_pp(bfly), bug(DebugDest), !,
 (empty_str(Prefix) -> true; color_format([fg(cyan)], '~N~w', [Prefix])),
 format(Fmt, Args), overwrote_prompt, !.
bugout4(Prefix, Fmt, Args, DebugDest) :- bug(DebugDest), !,
 color_format([fg(cyan)], '~N~w', [Prefix]),
 color_format([fg(cyan)], Fmt, Args), overwrote_prompt, !.
bugout4(_, _, _, _).


%:- set_stream(user_input, buffer_size(1)).
%:- set_stream(user_input, buffer(none)).
%:- set_stream(user_input, timeout(0.1)).



:- export(stdio_player/1).
stdio_player(Agent):- nonvar(Agent), !, stdio_player(AgentWas), !, AgentWas == Agent.
stdio_player(Agent):- stream_property(InStream, fileno(0)), mu_global:console_io_player(InStream, _, Agent), !.
stdio_player(Agent):-
 current_player(Agent),
 ignore(( \+ mu_global:console_io_player(_, _, Agent))).

:- thread_local(mu_global:current_agent_tl/1).
current_player(Agent):- mu_current_agent(AgentWas), !, AgentWas= Agent.

mu_current_agent(Agent):- current_agent_(AgentWas), !, AgentWas= Agent.
:- export(mu_current_agent/1).
current_agent_(Agent):- mu_global:current_agent_tl(Agent), !.
current_agent_(Agent):- current_input(InStream), mu_global:console_io_player(InStream, _, Agent).
current_agent_(Agent):- current_output(OutStream), mu_global:console_io_player(_, OutStream, Agent).
%current_agent_(Agent):- thread_self(Id), mu_global:console_host_io_history_unused(Id, _Alias, _InStream, _OutStream, _Host, _Peer, Agent).
current_agent_(x(player, 'i1')).

:- dynamic(mu_global:need_redraw/1).
overwrote_prompt(Agent):-
  retractall(mu_global:need_redraw(Agent)), asserta(mu_global:need_redraw(Agent)), !.
overwrote_prompt:- retractall(mu_global:need_redraw(x(player, 'i1'))), asserta(mu_global:need_redraw(x(player, 'i1'))), !.

ensure_has_prompt(Agent):-
 ignore((retract(mu_global:need_redraw(Agent)),
  flush_output,
  (get_agent_prompt(Agent, Prompt)->true;Prompt = [does]),
  player_format(Agent, '~N~w@~w> ', [Agent, Prompt]), retractall(mu_global:need_redraw(Agent)))),
  ttyflush.


player_format(Fmt, List):-
 mu_current_agent(Agent) ->
 xnotrace(player_format(Agent, Fmt, List)).

player_format(Agent, Fmt, List):-
 agent_to_output(Agent, OutStream),
 must_det(format(OutStream, Fmt, List)), !,
 overwrote_prompt(Agent).
player_format(Agent, Fmt, List):- must_det(format(Fmt, List)),
 overwrote_prompt(Agent).



%user:portray(ItemToPrint) :- print_item_list(ItemToPrint). % called by print.



identifer_code(Char) :- char_type(Char, csym).
identifer_code(Char) :- char_type(Char, to_lower('~')).
identifer_code(Char) :- memberchk(Char, `-'`).

punct_code(Punct) :- memberchk(Punct, `, .?;:!&\"`), !. % '
punct_code(Punct) :- \+ identifer_code(Punct), char_type(Punct, graph).

% -- Split a list of chars into a leading identifier_mw and the rest.
% Fails if list does not start with a valid identifier_mw.
identifier_mw([-1|_String], _, _) :- !, fail. % char_type pukes on -1 (EOF)
identifier_mw([Char|String], [Char|Tail], Rest) :-
 identifer_code(Char),
 identifier1(String, Tail, Rest).

identifier1(String, Id, Rest) :-
 identifier_mw(String, Id, Rest), !.
identifier1(String, [], String).

% -- Split a list of chars into a leading token_mw and the rest.
% Fails if list does not start with a valid token_mw.
token_mw(String, Token, Rest) :-
 identifier_mw(String, Token, Rest), !. % Is it an identifier_mw?
%token_mw(String, id(Atom), Rest) :-
% identifier_mw(String, Text, Rest), !, atom_codes(Atom, Text).
token_mw([Punct|Rest], [Punct], Rest) :-
 %char_type(Punct, punct), !. % Is it a single char token_mw?
 punct_code(Punct), !.

% -- Completely tokenize_mw a string.
% Ignores unrecognized characters.
tokenize_mw([], []) :- !.
tokenize_mw([-1], [`quit`]) :- !.
tokenize_mw(String, [Token|Rest]) :-
 token_mw(String, Token, Tail),
 !,
 tokenize_mw(Tail, Rest).
tokenize_mw([_BadChar|Tail], Rest) :-
 !,
 tokenize_mw(Tail, Rest).

log_codes([-1]).
log_codes(_) :- \+ mu_global:input_log(_), !.
log_codes(LineCodes) :-
 ignore(xnotrace(catch((atom_codes(Line, LineCodes),
 mu_global:input_log(FH),
 format(FH, '>~w\n', [Line])), _, true))).


%! skip_to_nl_mw(+Input) is det.
%
% Read input after the term. Skips white space and %... comment
% until the end of the line or a non-blank character.

skip_to_nl_mw(In) :-
 repeat,
 peek_char(In, C),
 ( C == '%'
 -> skip(In, '\n')
 ; char_type(C, space)
 -> get_char(In, _),
  C == '\n'
 ; true
 ),
 !.


:- meta_predicate with_tty(+, 0).
with_tty(In, Goal):-
 stream_property(In, tty(Was)),
 stream_property(In, timeout(TWas)),
 New = '', % format(atom(New), '~w@spatial> ', [Agent]),
 setup_call_cleanup((
 set_stream(In, tty(true)), set_stream(In, timeout(infinite))),
  setup_call_cleanup(prompt(Old, New),
  (%skip_to_nl_mw(In),
  Goal), prompt(_, Old)),
 (set_stream(In, timeout(TWas)), set_stream(In, tty(Was)))), !.

% -- Input from stdin, convert to a list of atom-tokens.

read_line_to_tokens(Agent, In, Prev, Tokens):-
 setup_console(In),
 with_tty(In,
            (read_line_to_codes(In, LineCodesR), read_pending_input(In, _, []))),
 append(Prev, LineCodesR, LineCodes),
 NegOne is -1,
 overwrote_prompt(Agent),
 must_det(line_to_tokens(LineCodes, NegOne, Tokens0)), !,
 clean_tokens(Tokens0, Tokens1),
 must_det(Tokens1=Tokens).

clean_tokens(Tokens0, Tokens1):- is_list(Tokens0), exclude(=(' '), Tokens0, Tokens1), !.
clean_tokens(Tokens0, Tokens0).


safety_for_lreader([],[]):-!.
safety_for_lreader([C|Rest],Out):-  char_code('"',C),!,double_quoted_string(Str,[C|Rest],Next),safety_for_lreader(Next,R),string_codes(Str,Codes),flatten([`"`,Codes,`"`,R],Out).
safety_for_lreader([C|Rest],Out):- char_code('\'',C),!,single_quoted_string(Str,[C|Rest],Next),safety_for_lreader(Next,R),string_codes(Str,Codes),flatten([`{"`,Codes,`"}`,R],Out).
safety_for_lreader([C|Rest],Out):- safety_for_lreader0(C,C0),!,safety_for_lreader(Rest,R0),flatten([C0,R0],Out).
safety_for_lreader0(C,Out):- char_type(C, punct),!,append(` "~+~`,[C|`" `],Out).
safety_for_lreader0(A,A).

name_the_vars(N=V):- add_var_to_env(N,V),!.
name_the_vars(N=V):- ignore(V='$VAR'(N)).



fixup_stokens(List,List):- (var(List);[]==List;number(List)),!.
fixup_stokens([A|List],[R|RList]):- !, fixup_stokens(A,R), fixup_stokens(List,RList).
fixup_stokens('$STRING'(Text),R):- text_to_string(Text,String), to_string_or_symbol(String,R).
fixup_stokens(String,Atom):- string(String), !, string_to_atom(String,Mid), fixup_stokens(Mid,Atom),!.
fixup_stokens(String,Atom):- \+ atomic(String),!,Atom=String.
fixup_stokens(String,Number):- atom(String), atom_number(String, Number), !.
fixup_stokens(List,List).

to_string_or_symbol(String,Symbol):- string_concat("~+~",Sym,String),!,string_to_atom(Sym,Symbol).
to_string_or_symbol(String,String):-!.

line_to_ptokens(LineCodes, Tokens):- 
 current_prolog_flag(allow_variable_name_as_functor,AVAF),
 set_prolog_flag(allow_variable_name_as_functor,true),
 call_cleanup(
  xnotrace(catch((read_term_from_codes(LineCodes, Term, [syntax_errors(quiet), var_prefix(false),
  variable_names(Vars), cycles(true), dotlists(true), singletons(_)])), _, fail)),
  set_prolog_flag(allow_variable_name_as_functor,AVAF)),
 maplist(name_the_vars,Vars),
 Tokens=Term, !.


line_to_tokens(Line, Tokens):- string_codes(Line,LineCodes), line_to_tokens(LineCodes, _, Tokens).

line_to_tokens([], _, []):-!.
line_to_tokens(LineCodes, NegOne, Tokens):- var(NegOne),!, NegOne is -1, line_to_tokens(LineCodes, NegOne, Tokens).
line_to_tokens(NegOne, NegOne, end_of_file):-!.
line_to_tokens(end_of_file, _NegOne, end_of_file):-!.
line_to_tokens([NegOne], NegOne, end_of_file):-!.
line_to_tokens(LineCodes, NegOne, Tokens) :-
 append([L], NewLineCodes, LineCodes),
 member(L, [10, 13, 32]), !,
 line_to_tokens(NewLineCodes, NegOne, Tokens).
line_to_tokens(LineCodes, NegOne, Tokens) :-
 append(NewLineCodes, [L], LineCodes),
 member(L, [10, 13, 32]), !,
 line_to_tokens(NewLineCodes, NegOne, Tokens).
line_to_tokens(LineCodes, _, Tokens):-
 ignore(log_codes(LineCodes)), !,
 line_to_stokens(LineCodes, Tokens), !,
 nop(save_to_history(LineCodes)),
 !.


line_to_stokens(LineCodes, Tokens) :- last(LineCodes, L), memberchk(L, [46, 41|`.)`]),
 line_to_ptokens(LineCodes, Tokens), !.
  
line_to_stokens(LineCodes, Tokens):- 
 notrace((
  current_predicate(parse_sexpr_string/2),  
  safety_for_lreader(LineCodes,LineCodes2),   
  sformat(S,'( ~s )',[LineCodes2]), 
  skipping_buffer_codes(parse_sexpr_string(S,Tokens0)), 
  to_untyped(Tokens0,Tokens1), 
  fixup_stokens(Tokens1,Tokens), 
  dmsg(parse_sexpr_string(Tokens)),
  !)).

line_to_stokens(LineCodes, Tokens):- line_to_ptokens(LineCodes, Tokens), !.

line_to_stokens(LineCodes, Tokens):- tokenize_mw(LineCodes, TokenCodes), !,
 % Convert list of list of codes to list of atoms:
 findall(Atom, (member(Codes, TokenCodes), atom_codes(Atom, Codes)), Tokens),!.




:- multifile(prolog:history/2).
save_to_history(LineCodes):-
 ignore(xnotrace((
 atom_string(AtomLineCodes, LineCodes), % dmsg(LineCodes->AtomLineCodes),
 current_input(In),
 ignore(catch(prolog:history(In, add(AtomLineCodes)), _, true))))).

% Immediate `ready` return on waitable_stream(s)
has_data_ready(_NonIntZero, Stream):-
 catch(wait_for_input([Stream], StreamsReady, 0.0), error(domain_error(waitable_stream, _), _), fail), !,
 StreamsReady = [Stream].
% read_pending_codes/3 might do the job
has_data_ready(_NonIntZero, Stream):- ttyflush,
  read_pending_codes(Stream, Chars, []),
  Chars\==[], nop((write(read_pending_codes(Chars)), nl)),
  % this is saved elsewhere via asserta/1
  add_prepended_input_assert(Stream, Chars).
% Single char infinite timeout on `user_input` (works fine)
has_data_ready( 0, Stream):- is_user_input_stream(Stream), !,
  get_single_char(Char), add_prepended_input_assert(Stream, [Char]).

% Crazy workarrounds that probly dont even work
has_data_ready(_NonIntZero, Stream) :-
    once((
        stream_property(Stream, buffer(Was)),
        stream_property(Stream, buffer_size(Sz)),
        stream_property(Stream, position(LinePos)),
        call_cleanup(catch((
                               % set_stream(Stream, buffer_size(1)),
                               % set_stream(Stream, buffer(false)),
                               ( (read_pending_codes(Stream, Chars, []), Chars\==[] )
                                ; (%set_stream(Stream, buffer(full)),
                                   peek_code(Stream, Char), Chars=[Char] ) )
                           ), Err,
                           ( writeln(Err), ttyflush,
                             read_pending_codes(Stream, Chars, [])
                           )),
                     ( set_stream(Stream, buffer(Was)),
                       set_stream(Stream, buffer_size(Sz))
                     ))
     )),
    (  (Chars\==[],
          nop((write(read_pending_codes(Chars)), nl)),
          add_prepended_input_assert(Stream, Chars))
      ; (stream_property(Stream, position(LinePosNew)),
         LinePosNew\==LinePos )
    ), !.


is_user_input_stream(Stream):- is_stream(Stream), !, stream_property(Stream, file_no(0)).
is_user_input_stream(Agent):- into_real_stream(Agent, Stream), !, stream_property(Stream, file_no(0)).

% Mostly non-Windows
wait_for_input_safe(ListOfStream, StreamsReady, Time):-
  catch(wait_for_input(ListOfStream, StreamsReady, Time), error(domain_error(waitable_stream, _), _), fail), !.
% infinite timeout
wait_for_input_safe(ListOfStream, StreamsReady, Time):- (Time==0; Time==infinite; Time is inf), !,
     repeat, sleep(0.01), include(has_data_ready(0), ListOfStream, StreamsReady), StreamsReady\==[], !.
% Wait until a tine
wait_for_input_safe(ListOfStream, StreamsReady, Time):-
  get_time(Now), Until is Now+Time,
  wait_for_input_safe_until(ListOfStream, StreamsReady, Until).

% Ping the ready list
wait_for_input_safe_until( ListOfStream, StreamsReady, _Until):- include(has_data_ready(0.01), ListOfStream, StreamsReady)-> (StreamsReady\==[], !).
% Timed out?
wait_for_input_safe_until(_ListOfStream, StreamsReady, Until):- get_time(Now), Now>Until, !, StreamsReady=[].
% Try for a little longer
wait_for_input_safe_until( ListOfStream, StreamsReady, Until):- sleep(0.05), wait_for_input_safe_until(ListOfStream, StreamsReady, Until).


into_real_stream(In, Stream):- atom(In), !, must(stream_property(Stream, alias(In));agent_to_input(In, Stream)), !.
into_real_stream(In, Stream):- must(is_stream(In)), Stream=In.

stream_pairs(In, Out):- mu_global:console_io_player(In, Out, _Agent).
stream_pairs(In, Out):- nonvar(In), var(Out), stream_property(In, file_no(F)), F > 2, stream_property(Out, file_no(F)), stream_property(Out, output), !.
stream_pairs(In, Out):- nonvar(Out), var(In), stream_property(Out, file_no(F)), F > 2, stream_property(In, file_no(F)), stream_property(In, input), !.
stream_pairs(In, Out):- var(In), !, stream_property(Out, input), \+ stream_property(Out, file_name(_)), once(stream_pairs(In, Out)), \+ using_stream_in(In, _OtherAgent).
%stream_pairs(In, Out):- var(Out), !, stream_property(Out, output), \+ stream_property(Out, fileno(2)), once(stream_pairs(In, Out)), \+ using_stream_in(In, _OtherAgent).

using_stream_in(Stream, OtherAgent):- mu_global:console_io_player(Stream, _, OtherAgent).
using_stream_in(Stream, OtherAgent):- mu_global:console_io_conn_history(Id, Alias, Stream, _OutStream, _Host, _Peer, Agent),
  once(OtherAgent = Agent ;  Alias = OtherAgent ; Id = OtherAgent).

using_stream(Stream, OtherAgent):- using_stream_in(Stream, OtherAgent).
using_stream(Stream, OtherAgent):- mu_global:console_io_player(_, Stream, OtherAgent).

agent_to_output( Agent, Stream):- notrace(agent_to_output0( Agent, Stream)).
agent_to_output0( Agent, Stream):- mu_global:console_io_player(_, Stream, Agent), !.
agent_to_output0( Agent, Stream):- mu_global:console_io_player(InStream, _, Agent), stream_pairs(InStream, Stream), !.
agent_to_output0(_Agent, Stream):- current_output(Stream), \+ using_stream(Stream, _Other), !.
agent_to_output0(_Agent, Stream):- stream_property(Stream, file_no(1)), \+ using_stream(Stream, _Other), !.
agent_to_output0( Agent, Stream):- fail, agent_to_input(Agent, In), stream_property(In, file_no(F)), F > 2, stream_property(Stream, file_no(F)), stream_property(Stream, write), !.
agent_to_output0( Agent, Stream):- % break, 
   dmsg(throw(agent_io(Agent, agent_to_output(Agent, Stream)))), stream_property(Stream, file_no(2)), !.
%agent_to_output(Agent, Stream):- mu_global:console_host_io_history_unused(_Id, _Alias, _In, Stream, _Host, _Peer, Agent), !.

% agent_to_input(Agent, In):- mu_global:already_consumed_input(Agent, _SoFar), In=Agent,
agent_to_input( Agent, Stream):- notrace(agent_to_input0( Agent, Stream)).
agent_to_input0(StreamNotAgent, Stream):- is_stream(StreamNotAgent), stream_property(StreamNotAgent, input), !, Stream=StreamNotAgent.
agent_to_input0(StreamNotAgent, Stream):- is_stream(StreamNotAgent), using_stream(StreamNotAgent, OtherAgent), !, using_stream_in(Stream, OtherAgent).
agent_to_input0( Agent, Stream):- using_stream_in(Stream, Agent), !.
agent_to_input0(_Agent, Stream):- current_input(Stream), \+ using_stream(Stream, _Other), !.
agent_to_input0(_Agent, Stream):- stream_property(Stream, file_no(0)), \+ using_stream(Stream, _Other), !.
agent_to_input0( Agent, Stream):- fail, agent_to_output(Agent, Stream), stream_property(Stream, file_no(F)), stream_property(Stream, file_no(F)), stream_property(Stream, read), !.
agent_to_input0( Agent, Stream):- break, dmsg(throw(agent_io(Agent, agent_to_input(Agent, Stream)))).
%agent_to_input( OtherAgent, Stream):- agent_to_input( Agent, Stream).
%agent_to_input(Agent, Stream):- mu_global:console_host_io_history_unused(_Id, _Alias, Stream, _Out, _Host, _Peer, Agent), !.

is_main_console:- current_input(Stream), stream_property(Stream, file_no(0)).

:- dynamic(mu_global:already_consumed_input/2).
:- volatile(mu_global:already_consumed_input/2).
get_already_consumed_input( Agent, SoFar):- agent_to_input(Agent, In), mu_global:already_consumed_input(In, SoFar).
get_already_consumed_input(_Agent, []).
clear_already_consumed_input(Agent):- into_real_stream(Agent, In), retractall(mu_global:already_consumed_input(In, _SoFar)).
reshow_already_consumed_input(Agent):- into_real_stream(Agent, In), mu_global:already_consumed_input(In, SoFar), format('~s', [SoFar]).
add_prepended_input_assert(Agent, C):- \+ is_list(C), !, add_prepended_input_assert(Agent, [C]).
add_prepended_input_assert(Agent, Chars):- into_real_stream(Agent, In),
  (retract(mu_global:already_consumed_input(In, SoFar));SoFar=[]), !,
  append(SoFar, Chars, New),
  assert(mu_global:already_consumed_input(In, New)).

% showing debug info for Agent's IO streams
user:ci:- ci('telnet_X1').
ci(Agent):-
 agent_to_input(Agent, In), forall(stream_property(In, P), dbug(ci, ins(P))),
 % line_position(In, LIn), dbug(ci, ins(line_position(In, LIn))),
 listing(mu_global:already_consumed_input/2),
 agent_to_output(Agent, Out), forall(stream_property(Out, P), dbug(ci, outs(P))),
 line_position(Out, LInOut), dbug(ci, outs(line_position(Out, LInOut))), !.


wordlist(List) --> optional_ws, wordlist1(List), optional_ws.
optional_ws --> whitespace.
optional_ws --> {true}.
wordlist1(List) --> wordlist2(List).
wordlist1([]) --> {true}.
wordlist2([X|Y]) --> word(X), whitespace, wordlist2(Y).
wordlist2([X]) --> word(X).
%wordlist([X|Y]) --> word(X), whitespace, wordlist(Y).
%wordlist([X]) --> whitespace, wordlist(X).
%wordlist([X]) --> word(X).
%wordlist([X]) --> word(X), whitespace.

%word(W) --> charlist(X), {name(W, X)}.
word(W) --> charlist(X), {atom_codes(W, X)}.

charlist([X|Y]) --> chr(X), charlist(Y).
charlist([X]) --> chr(X).

chr(X) --> [X], {X>=48}.

whitespace --> whsp, whitespace.
whitespace --> whsp.

whsp --> [X], {X<48}.

:- initialization(setup_console, program).

:- initialization(setup_console, restore).


