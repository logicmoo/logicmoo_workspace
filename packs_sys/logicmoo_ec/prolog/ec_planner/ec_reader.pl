/*
% NomicMUD: A MUD server written in Prolog
%
% Some parts used Inform7, Guncho, PrologMUD and Marty's Prolog Adventure Prototype
% 
% July 10, 1996 - John Eikenberry 
% Copyright (C) 2004 Marty White under the GNU GPL
% 
% Dec 13, 2035 - Douglas Miles
%
%
% Logicmoo Project changes:
%
% Main file.
%
*/

/*
  
 ec_reader:   
    Converts Eric Muellers DEC Reasoner files  (IBM ".e" files)
    To a Prolog readable ".e.pl" which may be maintained by hand
    

*/
%:- include(ec_reader_no_lps).

%end_of_file.

:- module(ec_reader,[convert_e/1, set_ec_option/2, verbatum_functor/1, builtin_pred/1, s_l/2,
   with_e_file/3, 
   convert_e/2,
   echo_format/1, 
   e_reader_test/0,
   e_reader_test/1,
   e_reader_testf/0,
   e_reader_testf/1,
   echo_format/2]).

:- meta_predicate convert_e(1,+,+).
:- meta_predicate trans_e(*,*,1,?,+,*).
:- meta_predicate with_e_file_write2(1,?,+).


:- use_module(library(logicmoo/portray_vars)).


set_ec_option(N,V):- retractall(etmp:ec_option(N,_)),asserta(etmp:ec_option(N,V)).


% used by ec_reader
verbatum_functor(function).  verbatum_functor(event). 
verbatum_functor(predicate).  verbatum_functor(fluent).
verbatum_functor(next_axiom_uses).

is_reified_sort(S):- S==belief.

non_list_functor(P):- pel_directive(P).
non_list_functor(sort).
non_list_functor(next_axiom_uses).
non_list_functor(reified_sort).

pel_directive(ignore).
pel_directive(manualrelease).
%non_list_functor(belief).
pel_directive(reified).
pel_directive(noninertial).
pel_directive(mutex).
pel_directive(completion).

pel_directive(range).
pel_directive(option).
pel_directive(load).
pel_directive(include).
is_non_sort(xor).

is_non_sort(P):- pel_directive(P).
is_non_sort(P):- verbatum_functor(P).
is_non_sort(NoListF):- non_list_functor(NoListF).

builtin_pred(initiates).
builtin_pred(terminates).
builtin_pred(releases).
builtin_pred(holds).
builtin_pred(happens).
builtin_pred(declipped).
builtin_pred(clipped).
builtin_pred(b).
builtin_pred(before).
builtin_pred(after).
builtin_pred(sort).
builtin_pred(initially).

is_quantifier_type(thereExists,( & )):- use_some.
is_quantifier_type(forAll,all).
is_quantifier_type(thereExists,exists).
is_quantifier_type(forsome,exists).
is_quantifier_type(X,Y):- atom(X), is_quantifier_type(_,X),Y=X.
is_quantifier_type(forall,all).

% used by ec_loader

:- meta_predicate with_e_file(1,+,+), with_e_file(1,+,+).
:- meta_predicate map_callables(2,*,*).
:- meta_predicate process_e_stream(1,*).
:- meta_predicate ec_on_read(1,*).
:- meta_predicate e_io(1,*).
:- meta_predicate upcased_functors(0).
:- meta_predicate read_stream_until_true(*,*,1,*).
:- meta_predicate process_e_stream_token(1,*,*).
:- meta_predicate continue_process_e_stream_too(1,*,*,*).
:- meta_predicate process_e_token_with_string(1,*,*).
:- meta_predicate continue_process_e_stream(1,*,*,*).

:- thread_local(t_l:each_file_term/1).
:- thread_local(t_l:block_comment_mode/1).
:- thread_local(t_l:echo_mode/1).

%:- meta_predicate now_doing(1, ?).
%:- meta_predicate each_doing(1, ?).
%:- meta_predicate doing(1, *).
  
:- meta_predicate 
   with_e_sample_tests(1),
   raise_translation_event(1,*,*).

:- use_module(library(logicmoo_common)).
%:- use_module(library(logicmoo/filestreams)).

:- export(e_reader_test/0).
e_reader_test:- with_e_sample_tests(convert_e(user_output)).
:- export(e_reader_test/1).
e_reader_test(Files):- with_abs_paths(convert_e(user_output),Files).

:- export(e_reader_testf/0).
e_reader_testf:- with_e_sample_tests(convert_e(outdir('.', ep))).
:- export(e_reader_testf/1).
e_reader_testf(Files):- with_abs_paths(convert_e(outdir('.', ep)),Files).



:- export(with_e_sample_tests/1).
with_e_sample_tests(Out) :- 
  retractall(etmp:ec_option(load(_), _)),
%  call(Out, 'ectest/*.e'),  
%  call(Out, 'examples/AkmanEtAl2004/ZooWorld.e'),  
  %call(Out, 'ecnet/RTSpace.e'),
  %call(Out, 'ectest/ec_reader_test_ecnet.e'),
  %call(Out, 'ecnet/Kidnapping.e'),
  %call(Out, 'ecnet/SpeechAct.e'),
  % call(Out, 'ecnet/Diving.e'),
   %call(Out, 'examples/Mueller2006/Exercises/MixingPaints.e'),
   call(Out, [ec('*/*/*/*.e'),ec('*/*/*.e'),ec('*/*.e')]),
  
%  call(Out, 'examples/Mueller2006/Chapter11/HungryCat.e'),
  !.
%:- initialization(e_reader_test, main).


% 
% :- meta_predicate ec_reader:must(0).

raise_translation_event(Proc1,What,OutputName):-  call(Proc1,:- call_pel_directive(translate(What,OutputName))).

:- set_ec_option(overwrite_translated_files,false).

:- export(should_update/1).
should_update(OutputName):- is_filename(OutputName), \+ exists_file(OutputName), !.
should_update(_):- etmp:ec_option(overwrite_translated_files,never),!,fail.
should_update(_):- etmp:ec_option(overwrite_translated_files,always),!.
should_update(_):- !.



chop_e(InputNameE,InputName):- atom_concat(InputName,'.e',InputNameE),!.
chop_e(InputName,InputName).

:- export(calc_where_to/3).
calc_where_to(outdir(Dir, Ext), InputNameE, OutputFile):- 
    chop_e(InputNameE,InputName),
    (atomic_list_concat([InputNameE, '.', Ext], OutputName0),exists_file(OutputName0) -> 
      OutputName =OutputName0 ;
       atomic_list_concat([InputName, '.', Ext, '.pl'], OutputName)),
    make_directory_path(Dir),
    absolute_file_name(OutputName, OutputFile, [relative_to(Dir)]).

:- export(include_e/1).
include_e(F):- with_e_file(do_convert_e, current_output, F).


:- export(convert_e/1).
convert_e(F):- convert_e(outdir('.', ep), F).
:- export(convert_e/2).
convert_e(Out, F):- with_e_file(do_convert_e, Out, F).
:- export(convert_e/3).
convert_e(Proc1, Out, F):- with_e_file(Proc1, Out, F).

  
%with_e_file(Proc1, Out, F):- dmsg(with_e_file(Proc1, Out, F)), fail.

with_e_file(Proc1, OutputName, Ins):- wdmsg(with_e_file(Proc1, OutputName, Ins)),fail.

with_e_file(Proc1, Out, F):- compound(Out), Out=outdir(Dir), !, with_e_file(Proc1, outdir(Dir, ep), F).

% wildcard input file  "./foo*.e"
with_e_file(Proc1, Out, F):- atom(F), \+ is_stream(F), \+ is_filename(F), 
   expand_file_name(F, L), L\==[], [F]\==L, !, maplist(with_e_file(Proc1, Out), L).

% wildcard input file  logical(./foo*.e).
with_e_file(Proc1, Out, F):-  \+ is_stream(F), \+ is_filename(F),
   findall(N, absolute_file_name(F, N, [file_type(txt), file_errors(fail), expand(false), solutions(all)]), L), 
   L\=[F], !, maplist(with_e_file(Proc1, Out), L).


with_e_file(Proc1, Out, F):- nonvar(F), quietly_needs_resolve_local_files(F, L), !, maplist(with_e_file(Proc1, Out), L).  

% Out is a misdirected stream
with_e_file(Proc1, Outs, Ins):- 
   atomic(Outs), is_stream(Outs),
   assertion(stream_property(Outs, output)), 
   \+ current_output(Outs), !,
   with_output_to(Outs, 
    with_e_file(Proc1,current_output, Ins)),!.

% Out is a filename not neding update
with_e_file(Proc1, OutputName, _Ins):- is_filename(OutputName), 
   \+ should_update(OutputName),
   raise_translation_event(Proc1,skipped,OutputName),
   raise_translation_event(Proc1,ready,OutputName), !.
   
% Out is like a wildcard stream (but we have a real filename)
with_e_file(Proc1, outdir(Dir, Ext), F):- is_filename(F), !, 
   calc_where_to(outdir(Dir, Ext), F, OutputName),
   with_e_file(Proc1, OutputName, F).

with_e_file(Proc1, Out, F):- is_filename(F), !, 
  quietly(absolute_file_name(F,AF)),
    locally(b_setval('$ec_input_file',AF),
      setup_call_cleanup(
        open(F, read, Ins),    
         with_e_file(Proc1, Out, Ins),
        close(Ins))),!.
        
% Out is like a wildcard stream (calc a real filename)
with_e_file(Proc1, outdir(Dir, Ext), Ins):- must(is_stream(Ins)), !, 
   must(stream_property(Ins, file(InputName))),
   calc_where_to(outdir(Dir, Ext), InputName, OutputName),
   with_e_file(Proc1, OutputName, Ins).



% Out is a filename not currently loadable 
with_e_file(MProc1, OutputName, Ins):- \+ is_stream(OutputName), 
  assertion(is_stream(Ins)), assertion(stream_property(Ins, input)),
  with_e_file_write1(MProc1, OutputName, Ins).

% with_e_file(MProc1, OutputName, Ins):- with_e_file_write2(MProc1, OutputName, Ins).

with_e_file(Proc1, Out, Ins):- 
      assertion(current_output(Out)),       
      e_io(Proc1, Ins).

:- nb_setval('$ec_input_file',[]).
:- nb_setval('$ec_input_stream',[]).


with_e_file_write1(MProc1, OutputName, Ins):-  \+ is_stream(OutputName), 
  assertion(is_stream(Ins)), assertion(stream_property(Ins, input)),
  must(should_update(OutputName)),
 strip_module(MProc1,Mod,Proc1),
 t_l:is_ec_cvt(FileType),!,
 flag('$ec_translate_depth', Was, Was),
 %ignore((Was==0 -> retractall(etmp:ec_option(load(_), _)))),
 retractall(etmp:ec_option(load(_), _)),
 setup_call_cleanup(flag('$ec_translate_depth', Was, Was+1),
   setup_call_cleanup(open(OutputName, write, Outs),
    setup_call_cleanup(b_setval('$ec_output_stream',Outs),
      locally(b_setval('$ec_input_stream',Ins),
        with_output_to(Outs,trans_e(FileType,Mod,Proc1,OutputName,Outs,Ins))),
      b_setval('$ec_output_stream',[])),
    close(Outs)),flag('$ec_translate_depth', _, Was)).

trans_e(FileType,Mod,Proc1,OutputName,Outs,Ins):- 
   assertion(is_outputing_to_file),
   raise_translation_event(Proc1,unskipped,OutputName),
   format(Outs,'~N~q.~n',[( :- include(library('ec_planner/ec_test_incl')))]),
   ignore((filetype_to_dialect(FileType,Dialect)->
     format(Outs,'~N~q.~n',[ :- expects_dialect(Dialect)]))),
   raise_translation_event(Proc1,begining,OutputName),
   ignore((FileType\==pel,get_date_atom(DateAtom),format(Outs,'% ~w File: ~w',[DateAtom,Ins]))),
   locally(t_l:is_ec_cvt(FileType), with_output_to(Outs,with_e_file(Mod:Proc1,Outs,Ins))),
   raise_translation_event(Proc1,ending,OutputName),!.

with_e_file_write2(Proc1, OutputName, Ins):-  \+ is_stream(OutputName),  !,
   assertion(is_stream(Ins)), assertion(stream_property(Ins, input)),
   must(should_update(OutputName)),
   raise_translation_event(Proc1,unskipped,OutputName),
   setup_call_cleanup(
     open(OutputName, write, Outs),
     with_output_to(Outs, 
       (raise_translation_event(Proc1,begining,OutputName),
         nb_setval('$ec_output_stream',Outs),
         format(Outs,'~N~q.~n',[:- expects_dialect(ecalc)]),
         with_e_file(Proc1, current_output, Ins),
          raise_translation_event(Proc1,ending,OutputName))),
     (nb_setval('$ec_output_stream',[]),close(Outs))),
   raise_translation_event(Proc1,ready,OutputName).

        
%e_io(Proc1, Ins):- dmsg(e_io(Proc1, Ins)), fail.
e_io(Proc1, Ins):-  
  repeat, 
  locally(b_setval('$ec_input_stream',Ins),once(process_e_stream(Proc1, Ins))), 
  notrace(at_end_of_stream(Ins)), !.
  


removed_one_ws(S):-
  peek_code(S, W), char_type(W, white), get_code(S, W), echo_format('~s', [[W]]).

removed_n_chars(_S, N):- N<1, !.
removed_n_chars(S, N):- get_code(S, _), Nm1 is N-1, removed_n_chars(S, Nm1).

trim_off_whitepace(S):- repeat, \+ removed_one_ws(S).



read_n_save_vars(Type, Codes):- read_some_vars(Codes, Vars),
  asserta(etmp:temp_varnames(Type, Vars)).

read_some_vars(Codes, Vars):-
 maybe_o_s_l,
  must(e_read3(Codes, VarNames)), !, 
  varnames_as_list(VarNames, Vars).

varnames_as_list( A,[A]):- (atom(A);string(A)), !.
varnames_as_list( A, [A]):- var(A),!.
varnames_as_list('$VAR'(A), Vars):- !, varnames_as_list(A,Vars).
varnames_as_list({A},Vars):- !, varnames_as_list(A,Vars).
varnames_as_list([A],Vars):- !, varnames_as_list(A,Vars).
varnames_as_list([A|B],Vars):- !,varnames_as_list(A,Vars1),varnames_as_list(B,Vars2),append(Vars1,Vars2,Vars).
varnames_as_list((A,B),Vars):- !,varnames_as_list(A,Vars1),varnames_as_list(B,Vars2),append(Vars1,Vars2,Vars).

upcased_functors(G):- 
 notrace((allow_variable_name_as_functor = N, 
   current_prolog_flag(N, Was))), !, 
   setup_call_cleanup(notrace(set_prolog_flag(N, true)), 
      G, 
      notrace(set_prolog_flag(N, Was))).


%% process_e_stream(Proc1, ?S) is det.
%
% Process file stream input
%
process_stream_comment(S) :- (peek_string(S, 3, W);peek_string(S, 2, W);peek_string(S, 1, W)), clause(process_stream_peeked213(S, W),Body),!,once(Body).
process_stream_peeked213(S, "#!"):- !, read_line_to_string_echo(S, _).
process_stream_peeked213(S, ";:-"):- !, 
   ( ( nb_current(last_e_string, axiom)) -> (echo_format('~N~n~n',[]), mention_s_l) ; true),
   get_char(S, ';'), read_term(S, Term, []),!, 
      portray_clause(Term),nl,
   nb_setval(last_e_string, axiom).

process_stream_peeked213(S,  ";"):- !, 
   ( ( nb_current(last_e_string, axiom)) -> (echo_format('~N~n~n',[]), mention_s_l) ; true),
   echo_format('%'), read_line_to_string_echo(S, _),!, 
   nb_setval(last_e_string, cmt).
process_stream_peeked213(S, "["):- !, 
  locally(b_setval(e_echo, nil), read_stream_until(S, [], `]`, Codes)),
   ( (\+ nb_current(last_e_string, cmt), \+ nb_current(last_e_string, vars) ) -> (echo_format('~N~n~n',[]), mention_s_l) ; true),
   echo_format('% ~s~N',[Codes]),
   read_n_save_vars(universal, Codes),
   nb_setval(last_e_string, vars).
process_stream_peeked213(S, "{"):- mention_s_l, echo_format('% '), !, read_stream_until(S, [], `}`, Codes), read_n_save_vars(existential, Codes).


%process_e_stream(Proc1, S):- assertion(stream_property(S, input)).
process_e_stream(Proc1, S):- notrace(at_end_of_stream(S)), !, mention_s_l, call(Proc1, end_of_file).
process_e_stream(_, S) :- removed_one_ws(S), !.
process_e_stream(_, S):- process_stream_comment(S), !.

process_e_stream(Proc1, S):-   
   OR = [to_lower('.'), to_lower('('), end_of_line, to_lower('='),to_lower('>'), space, to_lower(':')], 
   locally(b_setval(e_echo, nil),           
         read_stream_until_true(S, [], char_type_inverse(Was, or(OR)), Text)), 
   unpad_codes(Text, Codes), 
   maybe_o_s_l,
   ttyflush, 
   must(continue_process_e_stream(Proc1, S, Codes, Was)), !.
process_e_stream(Proc1, S):- read_line_to_string(S, Comment), echo_format('~N%RROOR: ~w: ~s~n', [Proc1, Comment]), break.


% continue_process_e_stream(Proc1, _S, [], space):- !.
continue_process_e_stream(_Proc1, _S, [], _):- !.
continue_process_e_stream(_Proc1, _S, [], end_of_line):- !.
continue_process_e_stream(Proc1, S, NextCodes, CanBe ):- ttyflush,
  continue_process_e_stream_too(Proc1, S, NextCodes, CanBe ),!.

continue_process_e_stream_too(Proc1, _S, Codes, to_lower(':')):- 
  append(Delta, [_], Codes), 
  text_to_string(Delta,DeltaS),
  normalize_space(atom(Term),DeltaS),
  nb_setval(last_e_string, delta),
  echo_format('~N~n'),maybe_mention_s_l(0), echo_format('% ~s ', [Codes]),
  ec_on_read(Proc1, directive(Term)),!.
continue_process_e_stream_too(Proc1, S, Codes, space):- last(Codes, Last), 
   once([Last]=`!`;char_type(Last, alpha)), !, 
   trim_off_whitepace(S), !, 
   atom_codes(Token, Codes),  
   nb_setval(last_e_string, kw),
   echo_format('~N~n'),maybe_mention_s_l(0), echo_format('% ~s ', [Codes]),
   process_e_stream_token(Proc1, Token, S), ttyflush, !.
continue_process_e_stream_too(Proc1, S, NextCodes, _CanBe ):-  !, 
  ( \+ nb_current(last_e_string, vars) -> (echo_format('~N~n~n',[]), mention_s_l) ; true),
   maybe_mention_s_l(2), echo_format('% ~s', [NextCodes]),
   last(NextCodes, Last), cont_one_e_compound(S, NextCodes, Last, Term), ec_on_read(Proc1, Term).

unpad_codes(Text, Codes):- text_to_string(Text, String), 
   normalize_space(codes(Codes0), String),
   trim_eol_comment(Codes0,Codes).

trim_eol_comment(Codes,Left):- append(Left,[59|_Cmt], Codes),!.
trim_eol_comment(Codes,Codes).
  
  
e_from_atom(String, Term):- e_read1(String, Term, _).   

set_e_ops(M):- 
   op(1150, yfx, M:'->'),
   op(1150, xfx, M:'->'),
   op(1150, xfy, M:'->'),
   % op(1125, xfy, M:'thereExists'), 
   op(1100, xfy, M:'<->'),
   op(1075, xfx, M:'thereExists'),
   op(1050, xfy, M:'|'),
   op(950, xfy, M:'&'),
   op(900, fx, M:'!'),
   op(400, yfx, M:'%'),
   op(1,fx,(M:($))).

e_read3(String, Term):- 
   M = ecread,
   forall(current_op(_,fx,OP),
    op(0,fx,(M:OP))),    
    set_e_ops(M),
       upcased_functors(notrace(((catch(
        (read_term_from_atom(String, Term, 
            [var_prefix(true),variable_names(Vars), module(M)])), _, fail))))), !, 
  maplist(ignore, Vars).

:- dynamic(etmp:temp_varnames/2).
:- dynamic(etmp:ec_option/2).


insert_vars(Term, [], Term, []).
insert_vars(_, [V|_], _, _):- assertion(ground(V)),fail.
insert_vars(Term0, [V|LL], Term, [V=VV|Has]):- var(V),
  insert1_var(Term0, V, VV, Term1), 
  insert_vars(Term1, LL, Term, Has).
insert_vars(Term0, ['$VAR'(V)|LL], Term, Has):-
  insert_vars(Term0, [V|LL], Term, Has).
insert_vars(Term0, [V|LL], Term, Has):-
  assertion(atomic(V)),
  atom_string(Vs,V), svar('$VAR'(Vs), PV),
  notrace((subst(Term0,'$VAR'(Vs), '$VAR'(PV), Term1),
  subst(Term1,'$VAR'(V), '$VAR'(PV), Term2),
  subst(Term2, V, '$VAR'(PV), Term9))),
  insert_vars(Term9, LL, Term, Has).

insert1_var(Term0, V, VV, Term1):- 
  debug_var(V, VV), 
  subst(Term0, V, VV, Term1).

map_callables(_, Term0, Term):- \+ callable(Term0), !, Term0=Term.
map_callables(_, Term0, Term):- []== Term0, !, Term =[].
map_callables(Call, Term0, Term):- atom(Term0), !, call(Call, Term0, Term).
map_callables(_Call, Term0, Term):- \+ compound(Term0), !, Term0=Term.
map_callables(Call, Compound=Value, Term):- fail, compound(Compound), 
  append_term(Compound, Value, Term0), map_callables(Call, Term0, Term).
map_callables(_, '$VAR'(HT), '$VAR'(HT)):-!.
map_callables(Call, [H|T], [HTerm|TTerm]):- !, map_callables(Call, H, HTerm), map_callables(Call, T, TTerm), !.
map_callables(Call, '$'(F, A), '$'(FF, AA)):- A==[], [] = AA, !, call(Call, F, FF).
%map_callables(Call, '$'(F, [A]), '$'(F, [AA])):- \+ special_directive(F), !, map_callables(Call, A, AA).
map_callables(Call, '$'(F, A), '$'(FF, AA)) :- call(Call, F, FF), maplist(map_callables(Call), A, AA), !.
map_callables(Call, HT, HTTerm):- !, 
 compound_name_arguments(HT, F, L), 
 map_callables(Call, '$'(F, L), '$'(FF, LL)), 
 compound_name_arguments(HTTerm, FF, LL).


:- export(fix_predname/2).

fix_predname('!', 'not').
fix_predname('~', 'not').

fix_predname(';', ';').
fix_predname('\\/', ';').
fix_predname('v', ';').
fix_predname('or', ';').
fix_predname('|', ';').
fix_predname('xor', 'xor').

fix_predname(',', ',').
fix_predname('^', ',').
fix_predname('and', ',').
fix_predname('&', ',').
fix_predname('/\\', ',').

fix_predname('equiv','<->').
fix_predname('iff', '<->').
fix_predname('<->', '<->').
fix_predname('<=>', '<->').

fix_predname('->', '->').
fix_predname('implies', '->').
fix_predname('=>', '->').
fix_predname('if', '->').

fix_predname(/**/holds, /**/holds).
fix_predname(holdsat, /**/holds).
fix_predname(holds_at, /**/holds).
%fix_predname(happens, happens_at).
fix_predname(happens_at, happens ).
fix_predname(initiates, initiates_at).
fix_predname(terminates, terminates_at).
fix_predname(releases, releases_at).
fix_predname(releasedat, released_at).
fix_predname(at, at_loc).
fix_predname(holds, pred_holds).
fix_predname(is, pred_is).

fix_predname(Happens, Happens):- builtin_pred(Happens).

fix_predname(F, New):- downcase_atom(F, DC), F\==DC, !, fix_predname(DC, New).


system:call_pel_directive(B):- pprint_ecp_cmt(red,call_pel_directive(B)).



my_unCamelcase(X, Y):- atom(X), fix_predname(X, Y), !.
my_unCamelcase(X, Y):- atom(X), upcase_atom(X, X), !, downcase_atom(X, Y).
my_unCamelcase(X, Y):- unCamelcase(X, Y), !.

neg_quant(NotVs,Vars):- compound(NotVs), NotVs=[Not,Vars], member(Not,[neg,(!),not]),!.
neg_quant(NotVs,Vars):- compound(NotVs), NotVs=[Not|Vars], member(Not,[neg,(!),not]),!.
neg_quant(NotVs,Vars):- compound(NotVs), NotVs=..[Not,Vars], member(Not,[neg,(!),not]),!.

:- export(e_to_pel/2).
e_to_pel(C, C):- \+ callable(C), !.
e_to_pel('$VAR'(HT), '$VAR'(HT)):-!.
e_to_pel(X, Y):- \+ compound(X), !, must(my_unCamelcase(X, Y)).
e_to_pel(X, Y):- compound_name_arity(X, F, 0), !, my_unCamelcase(F, FF), compound_name_arity(Y, FF, 0).
e_to_pel(not(Term),not(Term)):- var(Term),!.
e_to_pel(not(/**/holds(Term,Time)),/**/holds(O,Time)):-  !, e_to_pel(not(Term), O).
e_to_pel(not(Term),not(O)):- !, e_to_pel(Term, O).

e_to_pel(Prop,O):-
  Prop =.. [ThereExists,NotVs,_Term0],
  is_quantifier_type(ThereExists,_Exists),
  atom(NotVs), insert_vars(Prop, [NotVs], QProp, _Has),
  e_to_pel(QProp,O).

e_to_pel(Prop,O):- 
  Prop =.. [ThereExists,NotVs,Term0],
  is_quantifier_type(ThereExists,_Exists),
  is_list(NotVs), NotVs=[Vars],
  QProp =.. [ThereExists,Vars,Term0],
  e_to_pel(QProp,O).

e_to_pel(Prop,O):- 
  Prop =.. [ThereExists,NotVs,Term0],
  is_quantifier_type(ThereExists,_Exists),
  neg_quant(NotVs,Vars),
  QProp =.. [ThereExists,Vars,Term0],
  e_to_pel(not(QProp),O).


e_to_pel(Prop,O):- 
  Prop =.. [ThereExists,NotVs,Term0],
  is_quantifier_type(ThereExists,_Exists),
  neg_quant(NotVs,Vars),
  QProp =.. [ThereExists,Vars,Term0],
  e_to_pel(not(QProp),O).

e_to_pel(Prop,O):- 
  Prop =.. [ThereExists,NotVarZ,Term0],
  is_quantifier_type(ThereExists,_Exists),
  conjuncts_to_list(NotVarZ,NotVarsL), select(NotVs,NotVarsL,Rest),
  compound(NotVs), NotVs=..[Not,Vars], member(Not,[neg,(!),not]),
  is_list(Vars),%forall(member(E,Vars),ground(E)),!,
  (Rest==[]->Term1= Term0 ; list_to_conjuncts(Rest,NotVarsRest),conjoin(NotVarsRest,Term0,Term1)), 
  QProp =.. [ThereExists,Vars,Term1], 
  e_to_pel(not(QProp),O).

e_to_pel(Prop,O):- 
  Prop =.. [ThereExists,Vars,Term0], 
  is_quantifier_type(ThereExists,Exists),
  is_list(Vars), forall(member(E,Vars),atom(E)),
  QProp =.. [Exists,Vars,Term0],
  insert_vars(QProp, Vars, Term, _Has),
  e_to_pel(Term,O),!.

%e_to_pel(X, Y):- e_to_ax(X, Y),X\=@=Y,!,e_to_pel(X, Y).
%e_to_pel(/**/not(C),O):-e_to_pel(/**/holds(/**/not(N),V),O):- compound(C),/**/holds(N,V)=C,
%e_to_pel(/**/not(/**/holds(N,V)),O):-e_to_pel((/**/holds(/**/not(N),V)),O).
e_to_pel(t(X, [Y]), O):- nonvar(Y), !, e_to_pel(t(X, Y), O).
e_to_pel(load(X), load(X)):-!.
e_to_pel(include(X), include(X)):-!.
e_to_pel(option([N, V]), O):- !, e_to_pel(option(N, V), O).
e_to_pel(range([N, V, H]), O):- !, e_to_pel(range(N, V, H), O).

e_to_pel(t(X, Y), O):- atom(X), is_non_sort(X), !, SS=..[X, Y], e_to_pel(SS, O).
e_to_pel(t(X, Y), O):- atom(X), is_list(Y), is_non_sort(X), SS=..[X|Y], e_to_pel(SS, O).
e_to_pel(t(X, Y), O):- atom(X), is_list(Y), SS=..[X, Y], e_to_pel(SS, O).
e_to_pel(sort(col([S1, S2])), O):- !, e_to_pel(subsort(S1, S2), O).
e_to_pel(function(F, [M]), O):- e_to_pel(function(F, M), O).
%e_to_pel(Compound=Value, equals(Compound,Value)).
/*
e_to_pel(Term1, Term):- 
%  map_callables(my_unCamelcase, Term1, HTTermO),
%  Term1\=@=HTTermO,!,
%  e_to_pel(HTTermO, Term). 
*/
e_to_pel(HT, HTTermO):- !, 
 compound_name_arguments(HT, F, L), 
 maplist(e_to_pel,L,LL),
 compound_name_arguments(HTTerm, F, LL),
 map_callables(my_unCamelcase, HTTerm, HTTermO).


vars_verbatum(Term):- \+ compound_gt(Term, 0), !.
vars_verbatum(Term):- compound_name_arity(Term, F, A), (verbatum_functor(F);verbatum_functor(F/A)), !.

add_ec_vars(Term0, Term, Vs):- vars_verbatum(Term0), !, Term0=Term, Vs=[].
add_ec_vars(Term0, Term, Vs):-   
       
  get_vars(universal, UniVars),
  get_vars(existential,ExtVars),
  insert_vars(Term0, UniVars, Term1, VsA),!,  
  add_ext_vars(VsA, ExtVars, Term1, Term, Vs), !.

add_ext_vars(Vs, [], Term, Term, Vs):- !.
add_ext_vars(VsA, LLS, Term0, Term, Vs):-  use_some,
  insert_vars((some(LLS), Term0), LLS, Term, VsB), !,
  append(VsA,VsB,Vs),!.
add_ext_vars(VsA, LLS, Term0, Term, Vs):-  
  insert_vars(exists(LLS, Term0), LLS, Term, VsB), !,
  append(VsA,VsB,Vs),!.

use_some :- fail.

get_vars(Type,LLS):- findall(E, (etmp:temp_varnames(Type,L), member(E, L)), LL), sort(LL, LLS),!.


e_read1(String, Term, Vs):- 
   e_read2(String, Term0), !, 
   add_ec_vars(Term0, Term1, Vs), !,
   retractall(etmp:temp_varnames(_,_)),
   e_to_pel(Term1, Term), !.

if_string_replace(T, B, A, NewT):-   
   atomics_to_string(List, B, T), List=[_,_|_], !,
   atomics_to_string(List, A, NewT). 


e_read2(Txt, Term):- \+ string(Txt), text_to_string(Txt, T),!, e_read2(T, Term).
e_read2(T, Term):- if_string_replace(T, '!=', (\=), NewT), !, e_read2(NewT, Term).
e_read2(T, Term):- if_string_replace(T, '%', (/), NewT), !, e_read2(NewT, Term).
e_read2(T, Term):- use_some,
  if_string_replace(T,  '{', ' some( ', T1), 
  if_string_replace(T1, '}', ' ) & ', NewT), 
  e_read2(NewT, Term).
e_read2(T, Term):- 
  if_string_replace(T, '{', ' [ ', T1), 
  if_string_replace(T1, '}', ' ] thereExists ', NewT),    
  e_read2(NewT, Term).
%e_read2(T, Term):- if_string_replace(T, '[', ' forAll( ', NewT), !, e_read2(NewT, Term).
%e_read2(T, Term):- if_string_replace(T, ']', ') quantz ', NewT), !, e_read2(NewT, Term).
e_read2(T, Term):- e_read3(T, Term), !.
e_read2(T, Term):- 
   must(e_read3(T, Term)), !.
   
   

cleanout(Orig, B, E, MidChunk, RealRemainder):-
 text_to_string(Orig, Str), 
 AfterFirstB=[_|_],
 atomic_list_concat([BeforeB|AfterFirstB], B, Str), 
         atomics_to_string(  AfterFirstB, B, AfterB),
 Remainder=[_|_],
 atomic_list_concat([Mid|Remainder], E, AfterB),
 atomics_to_string( Remainder, E, AfterE),
 atomics_to_string( [BeforeB,' ', AfterE], RealRemainder),
 atomics_to_string( [B, Mid, E], MidChunk).


read_one_e_compound(S, Term):- 
   read_stream_until_true(S, [], char_type_inverse(_Was, or([to_lower('.'), end_of_line])), Text), 
   unpad_codes(Text, Codes), last(Codes, Last), 
   cont_one_e_compound(S, Codes, Last, Term).

cont_one_e_compound(_S, Text, Last, Term):- char_type(Last, to_lower('.')),
   unpad_codes(Text, Codes), e_from_atom(Codes, Term), nb_setval(last_e_string, axiom).

cont_one_e_compound(_S, Text, Last, Term):- char_type(Last, to_lower(')')),
   \+ (member(T, `>&|`), member(T, Text)),
   unpad_codes(Text, Codes), e_from_atom(Codes, Term), nb_setval(last_e_string, axiom).

cont_one_e_compound(S, InCodes, WasLast, Term):- process_stream_comment(S), !, cont_one_e_compound(S, InCodes, WasLast, Term).
cont_one_e_compound(S, InCodes, WasLast, Term):- 
   (WasLast\==40-> echo_format('% ') ; true), 
   read_stream_until_true(S, InCodes, char_type_inverse(_Was, or([to_lower('.'), end_of_line])), Text), 
   unpad_codes(Text, Codes), last(Codes, Last), 
   cont_one_e_compound(S, Codes, Last, Term).


%ec_on_read(S):- ec_on_read(on_load_ele, S).

:- meta_predicate ec_on_each_read(1,*,*).

ec_on_read(Proc1, EOF):- EOF == end_of_file, !,  must(call(Proc1, EOF)).
ec_on_read(Proc1, SL):- e_to_pel(SL, SO) -> SL\=@=SO, !, ec_on_read(Proc1, SO).
ec_on_read(Proc1, Cmp):- compound_gt(Cmp, 0), 
  Cmp =.. [NonlistF, List], is_list(List), non_list_functor(NonlistF),!, 
  maplist(ec_on_each_read(Proc1,NonlistF), List).
ec_on_read(Proc1, SL):- e_to_pel2(SL,SO) -> SL\=@=SO, !, ec_on_read(Proc1, SO).
ec_on_read(Proc1, S):- must(glean_data(Proc1, S)), must(call(Proc1, S)).

e_to_pel2(X,Y):- compound(X),compound_name_arguments(X,N,[_A|_Args]),N=translate,!,Y= (:- call_pel_directive(X)).
e_to_pel2(X,Y):- compound(X),compound_name_arguments(X,N,[_A|_Args]),pel_directive(N),!,Y= (:- call_pel_directive(X)).
e_to_pel2(X,X).

:- use_module(library(logicmoo/misc_terms)).

ec_on_each_read(Proc1, NonlistF, E):- univ_safe(Cmp , [NonlistF, E]), ec_on_read(Proc1, Cmp).

%must(G):- tracing, !, notrace(G).
%must(G):- call(G)->true;(trace,ignore(rtrace(G)),break).

on_convert_ele(Var):- var(Var), !, throw(var_on_convert_ele(Var)).
on_convert_ele(translate(Event, Outfile)):- !, must((mention_s_l, echo_format('~N% translate: ~w  File: ~w ~n',[Event, Outfile]))).
on_convert_ele(include(S0)):- resolve_local_files_quietly(S0,SS), !, maplist(include_e, SS), !.
%on_convert_ele(load(S0)):- resolve_local_files_quietly(S0,SS), !, maplist(load_e, SS), !.  
on_convert_ele(end_of_file):-!.
on_convert_ele(SS):- must(echo_format('~N')), must(pprint_ecp(e,SS)).


do_convert_e(SS):- on_convert_ele(SS).


glean_data(Pred1, SL):- \+ compound(SL), !, dmsg(warn(glean_data(Pred1, SL))).
glean_data(Pred1, subsort(S1, S2)):- !, glean_data(Pred1, sort(S1)), glean_data(Pred1, sort(S2)), assert_gleaned(Pred1, subsort(S1, S2)).
glean_data(Pred1, sort(S)):- !, assert_gleaned(Pred1, sort(S)).
glean_data(Pred1, isa(E, S)):- !, assert_gleaned(Pred1, isa(E, S)).
glean_data(Pred1, SL):- SL=..[S, L], 
  \+ is_non_sort(S), is_list(L), !, 
  glean_data(Pred1, sort(S)), 
  maplist(glean_data(Pred1, hasInstance(S)), L).
glean_data(_, _).

%assert_gleaned(Pred1, sort(S)):-  !, call(Pred1, gleaned(sort(S))).
assert_gleaned(_Pred1, SS):-  asserta_if_new(gleaned(SS)).
%assert_gleaned(Pred1, SS):-  call(Pred1, gleaned(SS)).

glean_data(Pred1, hasInstance(S), E):- !, glean_data(Pred1, isa(E, S)).



process_e_stream_token(Proc1, Atom, S):- atom_concat(New, '!', Atom), !, process_e_stream_token(Proc1, New, S).
process_e_stream_token(Proc1, Type, S):- normalize_space(atom(A), Type), A\==Type, !, process_e_stream_token(Proc1, A, S).
process_e_stream_token(Proc1, Text, S):- \+ atom(Text), !, text_to_string(Text, String), atom_string(Atom,String), process_e_stream_token(Proc1, Atom, S).
process_e_stream_token(Proc1, function, S):- !, read_stream_until(S, [], `:`, Text), read_line_to_string_echo(S, String), 
  append(TextL, [_], Text), 
  e_read1(TextL, Value, _), 
  token_stringsss(String, Type), 
   ec_on_read(Proc1, (function(Value, Type))).

process_e_stream_token(Proc1, Type, S):- downcase_atom(Type, Event), (memberchk(Event, [fluent, predicate, event]);is_reified_sort(Event)), !, 
   read_one_e_compound(S, Value), ec_on_read(Proc1, t(Event, Value)).

process_e_stream_token(Proc1, reified, S):- !, read_stream_until(S, [], ` `, Text), 
   text_to_string(Text, St), atom_concat('reified_', St, Type), !, process_e_stream_token(Proc1, Type, S).

process_e_stream_token(Proc1, Type, S):- read_line_to_string_echo(S, String), process_e_token_with_string(Proc1, Type, String).

process_e_token_with_string(Proc1, Type, String):- \+ is_non_sort(Type), 
 % \+ atom_contains(String,"("),
  atomics_to_string(VList, ',', String), VList \= [_], !, 
  maplist(process_e_token_with_string(Proc1, Type), VList).
process_e_token_with_string(_, _, ""):-!.
process_e_token_with_string(Proc1, Type, String):- token_stringsss(String, Out), ec_on_read(Proc1, t(Type, Out)).

token_stringsss("", []):-!.
token_stringsss(T, Out) :- if_string_replace(T, '  ', ' ', NewT), !, token_stringsss(NewT, Out).
token_stringsss(T, Out) :- if_string_replace(T, ': ', ':', NewT), !, token_stringsss(NewT, Out).
token_stringsss(T, Out) :- if_string_replace(T, ' :', ':', NewT), !, token_stringsss(NewT, Out).
token_stringsss(String, Out):- normalize_space(string(S), String), S\==String, !, token_stringsss(S, Out).
token_stringsss(String, VVList):- atomics_to_string(VList, ',', String), VList \= [_], remove_blanks_col(VList, VVList), !.
token_stringsss(String, col(VVList)):- atomics_to_string(VList, ':', String), VList \= [_], remove_blanks(VList, VVList), !.
token_stringsss(String, VVList):- atomics_to_string(VList, ' ', String), remove_blanks(VList, VVList), !.

remove_blanks_col(I, O):- remove_blanks(I, M),maplist(token_cols, M, O).

token_cols(String, col(VVList)):- atomics_to_string(VList, ':', String), VList \= [_], remove_blanks(VList, VVList), !.
token_cols(String,String).

remove_blanks([], []).
remove_blanks([''|I], O):- !, remove_blanks(I, O).
remove_blanks([E|I], O):- string(E), normalize_space(string(EE), E), E\==EE, !, remove_blanks([EE|I], O).
remove_blanks([E|I], O):- atom(E), normalize_space(atom(EE), E), E\==EE, !, remove_blanks([EE|I], O).
remove_blanks([E|I], O):- to_atomic_value(E, EE), E\==EE, !, remove_blanks([EE|I], O).
remove_blanks([E|I], [E|O]):- remove_blanks(I, O).


to_atomic_value(A, N):- number(A), !, N=A.
to_atomic_value(A, N):- normalize_space(atom(S), A), S\==A, !, to_atomic_value(S, N).
to_atomic_value(A, N):- atom_number(A, N).
to_atomic_value(A, A).

:- meta_predicate(read_stream_until(+,+,*,-)).
read_stream_until(S, Buffer, [Until], Codes):- !, name(N, [Until]), char_code(N, UntilCode), !, 
 read_stream_until_true(S, Buffer, ==(UntilCode), Codes).
read_stream_until(S, Buffer, UntilCode, Codes):- integer(UntilCode), !, 
 read_stream_until_true(S, Buffer, ==(UntilCode), Codes).
read_stream_until(S, Buffer, Until, Codes):- atom(Until), atom_length(Until, 1), char_code(Until, UntilCode), !, 
 read_stream_until_true(S, Buffer, ==(UntilCode), Codes).
read_stream_until(S, Buffer, Until, Codes):- read_stream_until_true(S, Buffer, Until, Codes).

char_type_inverse(Type, or(TypeList), Code):- !, member(E, TypeList), char_type_inverse(Type, E, Code).
char_type_inverse(Type, [Spec], Code):- !, char_type_inverse(Type, Spec, Code).
char_type_inverse(Type, [Spec|List], Code):- !, char_type_inverse(_, Spec, Code), char_type_inverse(Type, List, Code).
char_type_inverse(Type, Spec, Code):- char_type(Code, Spec), Type=Spec.

read_stream_until_true(S, Buffer, Proc1, Buffer):- at_end_of_stream(S), !, ignore(call(Proc1, 10)).
read_stream_until_true(S, Buffer, Proc1, Codes):- get_code(S, Char), 
  (nb_current(e_echo,nil) -> true; put_out(Char)),
  (call(Proc1, Char) -> notrace(append(Buffer, [Char], Codes)) ; 
  (notrace(append(Buffer, [Char], NextBuffer)), read_stream_until_true(S, NextBuffer, Proc1, Codes))).


/*
process_e_stream(Proc1, S):- must((read_term(S, T, [variable_names(Vs)]), put_variable_names( Vs))), 
  call(b_setval, '$variable_names', Vs), b_setval('$term', T), 
  (t_l:echo_mode(skip(items)) -> true ; write_stream_item(user_error, T)), !, 
  ttyflush(user_error), 
  must(visit_script_term(T)), !, 
  echo_format('~N', []), !.

write_stream_item(Out, T):- 
  ttyflush, 
  format(Out, '~N~n', []), 
  must(with_output_to(Out, portray_clause_w_vars(T))), 
  format(Out, '~N~n', []), !, ttyflush(Out).


*/
   


till_eof(In) :-
        repeat, 
            (   at_end_of_stream(In)
            ->  !
            ;   (read_pending_codes(In, Chars, []), 
                (t_l:echo_mode(echo_file) ->
                  echo_format('~s', [Chars]);
                  true), 
                fail)
            ).


:- fixup_exports.

end_of_file.            

exists(T, 
  ~/**/holds(positivelyBuoyant(Diver), T) & 
  ~/**/holds(neutrallyBuoyant(Diver), T) & 
  ~/**/holds(negativelyBuoyant(Diver), T)).
 implies that is a T that the Diver is not in the water.

all([Diver:diver,T:time], 
  (/**/holds(~positivelyBuoyant(Diver), T) & 
   /**/holds(~neutrallyBuoyant(Diver), T) & 
   /**/holds(~negativelyBuoyant(Diver), T)).
 -> /**/holds(~inWater(Diver),T)).

all([Diver:diver,T:time], 
  (/**/holds(~positivelyBuoyant(Diver), T) & 
   /**/holds(~neutrallyBuoyant(Diver), T) & 
   /**/holds(~negativelyBuoyant(Diver), T)).
 -> ~/**/holds(inWater(Diver),T)).


 implies that is a T that the Diver is not in the water.


exists(T, 
  ~/**/holds(positivelyBuoyant(Diver), T) & 
  ~/**/holds(neutrallyBuoyant(Diver), T) & 
  ~/**/holds(negativelyBuoyant(Diver), T)).

