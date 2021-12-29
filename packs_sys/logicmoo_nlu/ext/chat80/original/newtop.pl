/*

 _________________________________________________________________________
|       Copyright (C) 1982                                                |
|                                                                         |
|       David Warren,                                                     |
|               SRI International, 333 Ravenswood Ave., Menlo Park,       |
|               California 94025, USA;                                    |
|                                                                         |
|       Fernando Pereira,                                                 |
|               Dept. of Architecture, University of Edinburgh,           |
|               20 Chambers St., Edinburgh EH1 1JZ, Scotland              |
|                                                                         |
|       This program may Be used, copied, altered or included in other    |
|       programs only for academic purposes and provided that the         |
|       authorship of the initial program is aknowledged.                 |
|       Use for commercial purposes without the previous written          |
|       agreement of the authors is forbidden.                            |
|_________________________________________________________________________|


*/


%:- ensure_loaded(readin).
% :- ensure_loaded('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/logicmoo_nlu/parser_tokenize').


:-thread_local t_l:old_text/0.

t_l:old_text:- throw(t_l:old_text).
% TODO dont use open marker use []
use_open_marker.

/*
words_to_w2(U,W2):- words_to_w22(U,W2),!.

words_to_w22(U,W2):-var(U),must(W2=U).
words_to_w22([],W2):- !, must(W2=[]).
words_to_w22([W|WL],[W2|W2L]):- !, w_to_w2(W,W2),words_to_w2(WL,W2L).
words_to_w22(U,W2):- convert_to_atoms_list(U,List),!,words_to_w2(List,W2).
%words_to_w2(U,W2):- compound(U),must(W2=U).




w_to_w2(W,W):-t_l:old_text,!.
w_to_w2(Var,Var):-var(Var),!.
w_to_w2(w(Txt,Props),w(Txt,Props)):-!.
% w_to_w2([Prop,Txt],w(Txt,[Prop])):-!.
w_to_w2(w(X),w(X,[])):-!.
w_to_w2(S,w(A,open)):-use_open_marker,atomic(S),atom_string(A,S),!.
w_to_w2(S,w(S,open)):-use_open_marker,!.
w_to_w2(S,w(A,[])):-atomic(S),atom_string(A,S),!.
w_to_w2(U,w(U,[])):-compound(U),!.
w_to_w2(X,w(X,[])):-!.

w2_to_w(w(Txt,_),Txt):-!.
w2_to_w(Txt,Txt).

*/

% Chat-80 : A small subset of English for database querying.

:-public hi80/0, hi80/1, quote80/1.

:- op(400,xfy,&).
:- op(200,xfx,--).
:- [chatops].

/* ----------------------------------------------------------------------
	Simple Access to demonstrations
   ---------------------------------------------------------------------- */

demo(Type) :- demo(Type,L), ignore(control80(L)).

demo(Type,List) :- chat_80_ed( List, Type).
demo(main,List) :- chat_80_ed(_,List,_).

inform(L) :- nl, write('Question/Statement: '), inform1(L), nl, !.

inform1([]).
inform1([H|T]) :- write(H), put(32), inform1(T).


/* ----------------------------------------------------------------------
	Top level processing for verification and performance analysis
   ---------------------------------------------------------------------- */


system:test_chat80 :- make, setenv('CMD',timeout), test_chat80(_,on),!,show_results80.


system:test_c80 :- make, setenv('CMD',timeout),
	forall(chat_80_ed(N,Sentence,CorrectAnswer),
    ignore(baseKB_test_chat80_mpred(N, Sentence, on, CorrectAnswer))),
  show_results80.


:- share_mp(test_chat80/1).
test_chat80(N):- test_chat80(N, on), !.
%test_chat80(L):- ignore(control80(L)).
test_chat80(U):- nonvar(U), \+ number(U),
 locally(t_l:tracing80_nop,
           locally(t_l:chat80_interactive_nop,
            locally_hide(t_l:useOnlyExternalDBs,
             locally_hide(thglobal:use_cyc_database,
              ignore(control80(U)))))).

test_chat80(N, OnOff) :- (number(N);var(N)),!,
	(var(N)->show_title80_title ;true),
	forall(chat_80_ed(N,Sentence,CorrectAnswer),
   ignore(mpred_test(baseKB:test_chat80_mpred(N, Sentence, OnOff, CorrectAnswer)))).

test_chat80(Sentence, OnOff) :-
  test_chat80(0, Sentence, OnOff, _CorrectAnswer).

test_chat80(N, Sentence, OnOff, CorrectAnswer):-  
  ignore((baseKB:test_chat80_mpred(N, Sentence, OnOff, CorrectAnswer))),!,
  kill_junit_tee.

%dumpST_ERR:- !. %,fail.
dumpST_ERR:- catch(dump_st,_,fail),!. 
dumpST_ERR:- on_x_fail(dumpST),!.

:- use_module(library(logicmoo_test)).

baseKB_test_chat80_mpred(N, Sentence, _OnOff, CorrectAnswer) :-
    once(wotso(report_item(print_test,Sentence))),
	  once(wotso(process5(test,Sentence,CorrectAnswer,Status,Times))),
    assert_if_new(tmp:chat80results(N,Sentence,Status,Times)),!,
	  once(wotso(show_results80(N,Sentence,Status,Times))),!.

baseKB_test_chat80_mpred_missing(_, Sentence, OnOff, _CorrectAnswer) :-
    OnOff \= off,
    tracing ~= OnOff,!,
    wotso(process(normal,Sentence)),!.

% register so works in unit tests
/*
:- prolog_load_context(module,M),
  assert_if_new(baseKB:test_chat80_mpred(N, Sentence, OnOff, CorrectAnswer):- 
                     M:baseKB_test_chat80_mpred(N, Sentence, OnOff, CorrectAnswer)).
*/
baseKB:test_chat80_mpred(N, Sentence, OnOff, CorrectAnswer):- 
        parser_chat80:baseKB_test_chat80_mpred(N, Sentence, OnOff, CorrectAnswer),!.

c80test :-
	time(rtest_chats(30)).

					% added JW
rtest_chats(0) :- !.
rtest_chats(N) :-
	rtest_chat(1),
	NN is N - 1,
	rtest_chats(NN).

rtest_chat(N) :-
	chat_80_ed(N,Sentence,CorrectAnswer), !,
	  process5(test,Sentence,CorrectAnswer,Status,_Times),
	  (   Status == true
	  ->  true
	  ;   (format(user_error, 'Test ~w failed!~n', [N]),dumpST_ERR)
	  ),
	NN is N + 1,
	rtest_chat(NN).
rtest_chat(_).

show_title80_title:-
	format('Chat Natural Language Question Anwering Test~n~n',[]),
  show_title80.
  
show_title80 :-
	show_format(F),
	format(F, ['Test','Passed','Parse','Semantics','Planning','Reply','TOTAL','Sentence']),
	nl.

:- dynamic(tmp:chat80results/4).
:- dynamic(tmp:test80_result/4).
test80_results:- 
 forall(tmp:test80_result(A,into_lexical_segs,_,_), 
  (nl,
   format('=========================================================~n'),
   findall(r(A,B,C,D),tmp:test80_result(A,B,C,D),L),
   predsort(test80_r_sort,L,S),
   forall(member(E,S),(print_test80_result(E),nl)),
   show_title80,
   forall(tmp:chat80results(N,A,Status,Times), 
     show_results80(N,A,Status,Times)))).

test80_r_sort(O,r(A1,B1,C1,D1),r(A2,B2,C2,D2)):- 
 ((compare(O,A1,A2),O \== (=) );
  (compare_test_key(O,B1,B2),O \== (=) ) ; 
  (clause_num(tmp:test80_result(A1,B1,C1,D1),CN1),
   clause_num(tmp:test80_result(A2,B2,C2,D2),CN2),
   compare(O,CN1,CN2)); 
    compare(O,r(A1,B1,C1,D1),r(A2,B2,C2,D2))),!.

test_keyn(1,text_to_corenlp_tree). % (SS,_)),
test_keyn(2,into_lexical_segs). % (SS,Lex)),
test_keyn(3,sentence80). % (Lex,Tree)),
test_keyn(4,i_sentence). % (Tree,QT)),
test_keyn(5,clausify80). % (QT,UE)),  
test_keyn(6,simplify80). % (UE,Query)),
test_keyn(7,qplan). % (UE,Query)),
test_keyn(8,results80). % (Query,Answer)),
test_keyn(a,any_to_ace_str). % (S,SACE)),
test_keyn(b,try_ace_drs). % (SACE,Ace)),
test_keyn(c,try_ace_fol).
test_keyn(d,try_ace_eng). % (Ace,_Eng)),

compare_test_key(O,B1,B2):- test_keyn(A1,B1),test_keyn(A2,B2),!,compare(O,A1,A2).
clause_num(Cl,CN):- copy_term(Cl,Cl1),clause(Cl1,true,Ref),clause(ClC,true,Ref),ClC=@=Cl,!,nth_clause(ClC,CN,Ref).

print_test80_result(E):- sub_var(failure,E), !, ansicall(red,print_tree_nl(E)).
print_test80_result(E):- print_tree_nl(E),!.
show_results80:- 
 test80_results,
 %tell('CHAT80.txt'),test80_results,told,
 show_title80_title,
 forall(tmp:chat80results(N,Sentence,Status,Times),
   show_results80(N,Sentence,Status,Times)),
 show_title80.

show_results80(N,Sentence,Status,Times) :-  
	show_format(F),
  append([N,Status|Times],[Sentence],Args),
	format(F, Args),
	( Status = true ->
		nl
	; true ->
		tab(2), write(Status), nl
	).

show_format( '~N~t~w~10+|~t~w~10+~t~w~12+~t~w~10+~t~w~10+~t~w~10+~t ~w~10+~t | ~t~w~10+' ).



% Version of answer80/1 from TALKR which returns answer80
answer80((answer80([]):-E),[B]) :- !, holds_truthvalue(E,B).
answer80((answer80([X]):-E),S) :- !, seto(X,E,S).
answer80((answer80(X):-E),S) :- seto(X,E,S).

check_answer(_Sentence,A,B,true) :- close_answer(A,B),!.
check_answer(Sentence,A,B,'wrong answer'):-
  dumpST_ERR,
  pprint_ecp_cmt(red,check_answer(Sentence,A,B,'wrong answer')),  
  tracing ~= on,
  once(process(debug,Sentence)).

close_answer(A,A).
close_answer(A,B):- number(A),number(B),!, X is integer(A*100),Y is integer(A*100),!, X=Y.
close_answer([A],B):- !, close_answer(A,B).
close_answer(A,[B]):- !, close_answer(A,B).
close_answer(X,Y):- X=@=Y, !.
close_answer(X,Y):- string(X), words_of(X,XX), !, close_answer(XX,Y).
close_answer(X,Y):- string(Y), words_of(Y,YY), !, close_answer(X,YY).
close_answer(X,Y):- is_list(X),is_list(Y),maplist(close_answer,X,Y),!.
close_answer(A,B):- is_list(A), sort(A,AA), A\==AA, !, close_answer(AA,B).
close_answer(B,A):- is_list(A), sort(A,AA), A\==AA, !, close_answer(B,AA).
close_answer(A,B):- 
  compound(A),compound(B),
  compound_name_arguments(A,AA,AAA),
  compound_name_arguments(B,AA,BBB),!,
  maplist(close_answer,AAA,BBB).
close_answer(B,A):- atom(B), downcase_atom(B,BB),B\==BB, close_answer(BB,A).
close_answer(A,B):- atom(B), downcase_atom(B,BB),B\==BB, close_answer(A,BB).

/* ----------------------------------------------------------------------
	Top level for runtime version, and interactive demonstrations
   ---------------------------------------------------------------------- */

runtime_entry(start) :-
   version,
   format(user,'~nChat Demonstration Program~n~n',[]),
   hi80.

:-share_mp(hi80/0).
hi80 :-
%   tracing ~= on,
%   tell('hi_out.txt'),
   hi80(user)
%   ,told
   .

hi1 :-
   tracing ~= on,
%   tell('hi_out.txt'),
   chat80_test_q1(P),
   control80(P)
%   ,fail
%   ,told.
  .

hi2 :-
   tracing ~= on,
%   tell('hi_out.txt'),
   chat80_test_q2(P),
   control80(P)
%   ,fail
%   ,told.
  .

:-share_mp(hi80/1).
hi80(File):-
   repeat,
      ask80(File,P),
      control80(P), !,
      end80(File).

ask80(user,P) :- !,
   write('Question: '),
   ttyflush,
   read_in(P).
   %originally: read_in(P).
ask80(File,P) :-
   seeing(Old),
   see(File),
   read_in(P),
   nl,
   % pprint_ecp_cmt(yellow,read_in(P)),
   doing80(P,0),
   nl,
   see(Old).


print_test(X):- notrace(doing80(X ,0)),!.
print_test(X):- write(X).

doing80([],_) :- !. %,nl.
doing80([X|L],N0) :-
   out80(X),
   advance80(X,N0,N),
   doing80(L,N),!.

out80(w(X,_)) :- nonvar(X), !,
   reply(X).
out80(span(X)) :- nonvar(X), !.
out80(nb(X)) :- nonvar(X), !,
   reply(X).
out80(A) :-
   reply(A).

advance80(X,N0,N) :-
   uses80(X,K),
   M is N0+K,
 ( M>72, !,
     nl,
      N is 0;
     N is M+1,
      put(" ")).

uses80(nb(X),N) :- nonvar(X), !,
   chars80(X,N).
uses80(X,N) :-
   chars80(X,N).

chars80(X,N) :- atomic(X), !,
   name(X,L),
   length(L,N).
chars80(_,2).

end80(user) :- !.
end80(F) :- 
 seeing(F) -> seen ; true. % close(F).

chat80_test(L):- ignore(control80(L)).

:-share_mp(control80/1).
control80(L):- check_words(L,S)-> L\==S, !, control80(S).
control80([bye,'.']) :- !,
   nl, nl,
   write('Cheerio.'),
   nl.
control80([x,'.']) :- !,
   halt.
control80([trace,'.']) :- !,
   tracing ~= on,
   write('Tracing from now on!'), nl, fail.
control80([do,not,trace,'.']) :- !,
   tracing ~= off,
   write('No longer tracing.'), nl, fail.
control80([do,mini,demo,'.']) :- !,
   write('Executing mini demo...'), nl,
   demo(mini), fail.
control80([#|Text]) :- write(Text), nl, !, fail.
control80([do,Main,demo,'.']) :- !,
   write('Executing main demo...'), nl,
   demo(Main), fail.
control80([test,chat,'.']) :- !,
   test_chat80, fail.
control80(U) :-   
   process(normal, U),
   fail.

:- share_mp(trace_chat80/1).
trace_chat80(U):-
 locally(t_l:tracing80,
           locally(t_l:chat80_interactive,
            locally_hide(t_l:useOnlyExternalDBs,
             locally_hide(thglobal:use_cyc_database,
              ignore(control80(U)))))).



process5(How,Sentence,CorrectAnswer,Status,Times) :-
	process4(How,Sentence,Answer,Times),
	!,
	check_answer(Sentence,Answer,CorrectAnswer,Status),!.
process5(_How,_,_,failed,[0,0,0,0,0]):- fail,once(dumpST_ERR),!.

process(How,Sentence) :-
  process4(How,Sentence,Answer,_Times), Answer\==failed, !.
process(normal,U) :-
   nl, nl,
   write('I don''t understand! '+U), nl,on_x_fail(dumpST_ERR).
process(_,_).

eng_to_logic(U,S):- sentence80(E,U,[],[],[]), sent_to_prelogic(E,S).



%is_lexical_segs(Sent,Sent):- is_list(Sent),maplist(parser_penn_trees:is_word_or_span,Sent),!.
is_lexical_segs([I|_Rest]):- is_w2(I),!.

%into_lexical_segs(I,O):- into_text80_string(I, Text80), spacy_lexical_segs(Text80,O).
into_lexical_segs(I,I):- is_lexical_segs(I).
into_lexical_segs(I,O):- into_text80_string(I, Text80), flair_lexical_segs(Text80,OM),include_is_w2(OM,O).
%into_lexical_segs(I,O):- into_text80_string(I, Text80), allen_srl_lexical_segs(Text80,O).

old_into_lexical_segs(I,I):- is_lexical_segs(I).
old_into_lexical_segs(Sent,O):- notrace(into_chat80_segs0(Sent,U)),include_is_w2(U,O).
%into_lexical_segs(Sent,  WordsA):- enotrace((into_text80( Sent,  Words),into_combines(Words,WordsA))),!.

into_chat80_segs0(I,I):- is_lexical_segs(I).
into_chat80_segs0(Sent,UO):-  
 maplist(must_or_rtrace,
  [into_text80_string(Sent, Text80),
   into_chat80_merged_segs(Text80,U),
   lex_winfo(U,UO),
   %into_chat80_merged_segs(Text80,U),
   !]),!.
into_combines(Words,WordsO):- must_maplist(parser_tokenize:any_nb_to_atom, Words, WordsA), do_txt_rewrites(WordsA,WordsB),
  must_maplist(t_to_w2,WordsB,WordsO).

w2_to_t(w(Txt,_),Txt):-!.
w2_to_t(Txt,Txt).

/*into_lexical_segs(Sentence, WordsA):-
   into_lexical_segs(Sentence, Words),
   must_maplist(any_to_atom, Words, WordsA), !.
*/


%t_to_w2(W,W):-t_l:old_text,!.
t_to_w2(Var,Var):-var(Var),!.
t_to_w2(w(Txt,Props),w(Txt,Props)):-!.
% t_to_w2([Prop,Txt],w(Txt,[Prop])):-!.
%t_to_w2(w(X),w(X,[])):-!.

t_to_w2(S,w(A,open)):-atomic(S),atom_string(A,S),!.
t_to_w2(S,w(S,open)):-!.
%t_to_w2(U,w(U,[])):-compound(U),!.
%t_to_w2(S,w(A,[])):-atomic(S),atom_string(A,S),!.
%t_to_w2(X,w(X,[])):-!.
from_wordlist_atoms( Sentence,  Words):- enotrace((must_maplist(w2_to_t,Sentence, Words))).

%into_chat80_segs_pt2(Sentence,U):- check_words(Sentence,U).
:- export(text_to_chat80_tree/2).
text_to_chat80_tree(Sentence,Tree):-
  into_lexical_segs(Sentence,U),
  deepen_pos(sentence80(Tree,U,[],[],[])).

process4a(How,Sentence,U,S1,Times) :- 
  Times = [ParseTime,SemTime,TimePlan,_TimeAns,_TotalTime],
  report(How,Sentence,'Sentence',0,tree),
  ignore(on_x_fail((into_text80_string(Sentence,Text80),s61(Text80)))),

  quietly(( runtime(StartSeg),
   mpred_test_mok(into_lexical_segs(Sentence,U)),!,
   runtime(StopSeg),
   SegTime is StopSeg - StartSeg,
   assert_if_new(tmp:test80_result(Sentence,into_lexical_segs,U,SegTime)),
   (report(always,U,'segs',SegTime,print_tree_nl)))),!,
 
 ((runtime(StartParse),   
   
 ((debug_chat80_if_fail(deepen_pos(no_repeats(E,sentence80(E,U,[],[],[])))),
    
   notrace((runtime(StopParse),

    ParseTime is StopParse - StartParse,
    report(always,E,'Parse',ParseTime,print_tree_nl)
    % !, %%%%%%%%%%%%%%%% added by JPO but breaks "london"
    )))),
   runtime(StartSem),
   debug_chat80_if_fail(mpred_test_mok(deepen_pos(i_sentence(E,E1)))),
   runtime(EndI),
   TotalI is EndI - StartSem,
   debug_chat80_if_fail((simplify80(E1,E11),simplify80(E11,E12))),
   report(always,E1,'i_sentence',TotalI,cmt),
   debug_chat80_if_fail(mpred_test_mok(deepen_pos(clausify80(E12,E2)))))),
   !,
  % report(How,E2,'clausify80',ParseTime,cmt),
   debug_chat80_if_fail((simplify80(E2,E3),simplify80(E3,S))),
   runtime(StopSem),
   SemTime is StopSem - StartSem,
   %report(How,S,'Semantics',SemTime,expr),
   runtime(_StartPlan),
  ((
   guess_pretty(S),
   debug_chat80_if_fail(qplan(S,S1)),
   assert_if_new(tmp:test80_result(Sentence,sentence80,E,ParseTime)),
   assert_if_new(tmp:test80_result(Sentence,i_sentence,E1,TotalI)),
   assert_if_new(tmp:test80_result(Sentence,clausify80,E2,SemTime)),
   assert_if_new(tmp:test80_result(Sentence,qplan,S1,SemTime)),
   guess_pretty(S1),
   %pprint_ecp_cmt(green,S),
   runtime(StopPlan),
   TimePlan is StopPlan - StartSem,

   report(always,Sentence+S1,'Planning',TimePlan,expr),
   !)).

process4(How,Sentence,Answer,Times):- (How == test;How==on),
  Times = [_ParseTime,_SemTime,_TimePlan,_TimeAns,TotalTime],
  runtime(StartAns),
  c8_test(Sentence,Answer),
  runtime(EndAns),
  TotalTime is EndAns - StartAns,
  %should_learn(Answer),
  (is_list(Answer)->!;Answer \== failure).

process4(How,Sentence,Answer,Times) :-
   process4a(How,Sentence,U,S1,Times),!, 
   process4b(How,Sentence,U,S1,Answer,Times).

process4b(How,Sentence,U,S1,Answer,Times) :-
   Times = [ParseTime,SemTime,TimePlan,TimeAns,TotalTime],
   report(How,S1,'Planning',TimePlan,expr),
   runtime(StartAns),
   ((
   debug_chat80_if_fail(results80(S1,Answer)), !,
   runtime(StopAns),
   TimeAns is StopAns - StartAns,
   assert_if_new(tmp:test80_result(Sentence,results80,Answer,TimeAns)),
   TotalTime is ParseTime+SemTime+TimePlan+TimeAns,
   report(How,U,'Question',TotalTime,print_test),
   ignore((report(always,Answer,'Reply',TimeAns,print_tree_nl))))),!.
   
compile80(S1,G,S):- answer8o2_g(S1,G,S),!.
compile80(S1,(G,respond(S))):- compile80(S1,G,S).

results80(X,_):- var(X),!,fail.
results80((S1,S2),(G1,G2)):- !,nonvar(S1), results80(S1,G1), results80(S2,G2).
results80(S1,Results):- 
  nonvar(S1),
  findall(Res,deepen_pos((answer802(S1,Res),Res\=[])),Results),!.

report(How,Item,Label,Time,Mode):- wotso(report0(How,Item,Label,Time,Mode)),!.

report0(none,_,_,_,_):- !.
report0(test,_,_,_,_):- !.
report0(How,Item,Label,Time,Mode) :-
   ((tracing =: on); How==debug; How==always), !,
   nl, in_cmt(block, (nl, write(Label), write(': '), write(Time), write('msec.'), nl,nl,
   \+ \+ report_item(Mode,Item))),nl,!.
report0(_,_,_,_,_).

report_item(none,_).
report_item(Tree,Item):- copy_term(Item,Nat), guess_pretty(Nat),report_item0(Tree,Nat),!.
report_item(_,_).

%report_item(_,Item):- pprint_ecp_cmt(yellow,Item),!.
report_item0(respond,Item) :- !,
   respond(Item), nl.
report_item0(print_test,Item) :- !,
   write('?- test_chat80("'),print_test(Item),write('").'), nl.

report_item0(_,Item) :- print_tree_with_final(Item,'.'),!.

report_item0(expr,Item) :- !,
  \+ \+ print_tree80(Item), nl.
   % \+ \+ write_tree(Item), nl.

%report_item0(_,Item) :- !, \+ \+ write_tree(Item), nl.

report_item0(_,Item) :- print_tree_with_final(Item,'.'),!.
report_item0(tree,Item) :- \+ \+ print_tree80(Item),!, nl.

report_item0(cmt,Item) :-
    in_color(yellow, print_tree_with_final(Item,'.')),!,nl.
report_item0(cmt,Item) :- pprint_ecp_cmt(yellow,Item),!.
report_item0(P,Item) :-
   must_or_rtrace(call(P,Item)),!.


runtime(MSec) :-
   statistics(runtime,[MSec,_]).

quote80(A&R) :-
   atom(A), !,
   quote_amp(R).
quote80(_-_).
quote80(_--_).
quote80(_+_).
quote80(verb(_,_,_ExtraMods,_,_,_)).
quote80(wh(_)).
quote80(name(_)).
quote80(nameOf(_Var,_)).
quote80(prep(_)).
quote80(det(_)).
quote80(quantV(_,_)).
quote80(wh_det(_Kind,_)).

quote_amp(F):- compound(F), compound_name_arity(F,'$VAR',1),!.
quote_amp(R) :-
   quote80(R).

sent_to_prelogic(S0,S) :-
   i_sentence(S0,S1),
   clausify80(S1,S2),
   simplify80(S2,S3),
   simplify80(S3,S4),
   reduce1(S4,S5),
   reduce1(S5,S).

%reduce1(P,P):-!.
reduce1(P,Q):- \+ compound(P),!, Q=P.
reduce1(slot(Syntax,_Type,Var,_Mode,_SlotI),slot_i(Syntax,Var)):-!.
reduce1((P,Q),PQ):- P == Q,!,reduce1(P,PQ).



reduce1((P,Q),PQ):- (true) == Q,!,reduce1(P,PQ).
reduce1((Q,P),PQ):- (true) == Q,!,reduce1(P,PQ).
reduce1(mg(Q),Q):- !.

reduce1(bE(Named,Q,P),true):- Named==named, P==Q, !.
reduce1(bE(Named,Q,P),true):- Named==named, P=Q, !.
reduce1(bE(is,Q,P),bE(is,Q,P)):-!.
%reduce1(bE(_,Q,P),true):- var(P),var(Q),P=Q, !.
reduce1(same_values(Q,P),true):- P=Q,!.

reduce1('^'(Q,P),P):- ground(Q).
reduce1('&'(Q,P),PQ):- compound(Q),compound(P), reduce1((Q,P),PQ).

%reduce1(( bE(_,Num_Num10,N) , P), Q):- var(Num_Num10),subst(P,Num_Num10,N,Q).

%reduce1(qualifiedBy(Var,Num,_&_,V),true):- (atom(V);atom(Num)), V=Num,!.
reduce1(np_head(X,generic,[adj(ace_var(self,Name))],A),O):-
  reduce1(resultFn(X,(ti(A,X),ace_var(X,Name))),O).
/*
reduce1(qualifiedBy(Var,BE_QualifiedBy,_Np_head,np_head(Var,Some,[],Place_here)),ti(Place_here,Var)):-
  Some==some.
*/

reduce1(qualifiedBy(Var,_,_,S),R):- sub_term(E,S), compound(E), E = np_head(Var,_,_,_), R= E.
reduce1(qualifiedBy(Var,X,P,S),R):- fail, OR = qualifiedBy(Var,X,P,S), qualifiedBy_LF2(Var,xxx,X,P,S,R)-> OR\==R,!.
reduce1(qualifiedBy(Var,X,P,S),R):- qualifiedBy_LF(Var,reduce1,X,P,S,R),!.

reduce1('`'(A),R):- reduce1(A,R).
%reduce1(ace_var(C,N),true):- var(C),nonvar(N),C='$VAR'(N),!.

%reduce1(Ex^(ti(Type,Ex1),bE(is,Ex2,Inst)),Ex^(ti(Type,Inst)&Ex=Inst)):- Ex==Ex1, Ex1==Ex2,!.
reduce1(Ex^(exceeds(Value1, Ex1) & exceeds(Value2, Ex2)),exceeds(Value2, Value1)):- Ex==Ex1, Ex1==Ex2,!.
reduce1(Ex^(exceeds(Value1, Ex1), exceeds(Value2, Ex2)),exceeds(Value2, Value1)):- Ex==Ex1, Ex1==Ex2,!.
reduce1(Ex^(exceeds(X,Y),exceeds(A,B)),exceeds(X,B)):- Ex==Y, Y==A,!.
reduce1(Ex^(exceeds(A,B),exceeds(X,Y)),exceeds(X,B)):- Ex==Y, Y==A,!.
reduce1(P,Q):- compound_name_arguments(P,F,A), \+ dont_reduce1(F),
   maplist(reduce1,A,AA), 
   compound_name_arguments(Q,F,AA).

dont_reduce1(qualifiedBy).

clausify_simplify80(QT,Plan):- 
  simplify80(QT,QT0), clausify80(QT0,UE),!,should_learn(UE),
  once((simplify80(UE,Query),qplan(Query,Plan))),
  should_learn(Plan),!.

simplify80(C,C0):-var(C),dmsg(var_simplify(C,C0)),!,C=C0.
simplify80(C,(P:-RR)) :- !,
   unequalise(C,(P:-Q)),
   simplify80(Q,R,true),
   reduce1(R,RR).
simplify80(C,C0,C1):-var(C),dmsg(var_simplify(C,C0,C1)),fail.
simplify80(C,C,R):-var(C),!,R=C.

simplify80(setOf(X,P0,S),R,R0) :- !,
   simplify80(P0,P,true),
   revand(R0,setOf(X,P,S),R).

simplify80(P,R,R0):-
  reduce1(P,Q)-> P\==Q, !,
  simplify80(Q,R,R0).
simplify80(R,P,R0):-
  reduce1(P,Q)-> P\==Q, !,
  simplify80(R,Q,R0).

simplify80((P,Q),R,R0) :-
   simplify80(Q,R1,R0),
   simplify80(P,R,R1).
simplify80(true,R,R) :- !.
simplify80(X^P0,R,R0) :- !,
   simplify80(P0,P,true),
   revand(R0,X^P,R).
simplify80(numberof(X,P0,Y),R,R0) :- !,
   simplify80(P0,P,true),
   revand(R0,numberof(X,P,Y),R).

simplify80(\+P0,R,R0) :- !,
   simplify80(P0,P1,true),
   simplify_not(P1,P),
   revand(R0,P,R),!.
simplify80(P,R,R0) :-
   revand(R0,P,R),!.

simplify_not(P,\+P):- var(P),!.
simplify_not(\+P,P) :- !.
simplify_not(P,NP):- negate_inward(P,NP).

revand(true,P,P) :- !.
revand(P,true,P) :- !.
revand(P,Q,(Q,P)).

unequalise(C00,C) :-
   unnumbervars(C00,C0),
   numbervars80(C0,1,N),
   functor(V,v,N),
   functor(M,v,N),
   inv_map_enter(C0,V,M,C),!.

inv_map_enter(C0,V,M,C):- catch(inv_map(C0,V,M,C),too_deep(Why),(dmsg(Why),dtrace(inv_map(C0,V,M,C)))),!.

inv_map(Var,V,M,T) :- stack_depth(X), X> 400, throw(too_deep(inv_map(Var,V,M,T))).
inv_map(Var,V,M,T) :- stack_check(500), var(Var),dmsg(var_inv_map(Var,V,M,T)),!,Var==T.
inv_map('$VAR'(I),V,_,X) :- !,
   arg(I,V,X).
inv_map(A=B,V,M,T) :- !,
   drop_eq(A,B,V,M,T).
inv_map(X^P0,V,M,P) :- !,
   inv_map(P0,V,M,P1),
   exquant(X,V,M,P1,P).
inv_map(A,_,_,A) :- atomic(A), !.
inv_map(T,V,M,R) :-
   functor(T,F,K),
   functor(R,F,K),
   inv_map_list(K,T,V,M,R).

inv_map_list(0,_,_,_,_) :- !.
inv_map_list(K0,T,V,M,R) :-
   arg(K0,T,A),
   arg(K0,R,B),
   inv_map(A,V,M,B),
   K is K0-1,
   inv_map_list(K,T,V,M,R).

drop_eq('$VAR'(I),'$VAR'(J),V,M,true) :- !,
 ( I=\=J, !,
   irev(I,J,K,L),
   arg(K,M,L),
   arg(K,V,X),
   arg(L,V,X)
   ;
   true).
drop_eq('$VAR'(I),T,V,M,true) :- !,
   deref(I,M,J),
   arg(J,V,T),
   arg(J,M,0).
drop_eq(T,'$VAR'(I),V,M,true) :- !,
   deref(I,M,J),
   arg(J,V,T),
   arg(J,M,0).
drop_eq(X,Y,_,_,X=Y).

deref(I,M,J) :-
   arg(I,M,X),
  (var(X), !, I=J;
   deref(X,M,J)).

exquant('$VAR'(I),V,M,P0,P) :-
   arg(I,M,U),
 ( var(U), !,
   arg(I,V,X),
   P=(X^P0);
   P=P0 ).

irev(I,J,I,J) :- I>J, !.
irev(I,J,J,I).

%:- mode check_words(+,-).
check_words(NonList,Out):-
 \+ is_list(NonList),
 into_text80_atoms(NonList,M),!,
 check_words(M,Out).
check_words(X,X):-!.
check_words([],[]).
check_words([Word1,Word2|Words],RevWords) :- atomic(Word1),atomic(Word2),atomic_list_concat([Word1,'_',Word2],Word),chk_word(Word),!,
  check_words([Word|Words],RevWords).
check_words([Word1,'_',Word2|Words],RevWords) :- atomic(Word1),atomic(Word2),atomic_list_concat([Word1,'_',Word2],Word),!,
  check_words([Word|Words],RevWords).
check_words([Word|Words],[RevWord|RevWords]) :-
   check_word(Word,RevWord),
   check_words(Words,RevWords).

%:- mode check_word(+,-).
%check_word(Word,w(NewWord,open)):- \+ compound(Word), check_word0(Word,NewWord),!.
check_word(X,X).

check_word0(Word,Word) :- number(Word),!.
check_word0(Word,Word) :- chk_word(Word), !.
check_word0(Word,Word):- compound(Word),!.
check_word0(Word,NewWord) :-
   % write('? '), write(Word), write(' -> (!. to abort) '), ttyflush, read(NewWord0), NewWord0 \== !,
   % check_word(NewWord0,NewWord)
   ignore(NewWord=Word),
   !.

%:- mode ~=(+,+), =+(+,-), =:(+,?).

Var ~= Val :-
 ( recorded(Var,val(_),P), erase(P)
 ; true), !,
 recordz(Var,val(Val),_).

Var =+ Val :-
 ( recorded(Var,val(Val0),P), erase(P)
 ; Val0 is 0), !,
   Val is Val0+1,
   recordz(Var,val(Val),_).

Var =: Val :-
   recorded(Var,val(Val),_).


get_sentence_level_adverbs(FN,InfoL):- findall(Info,t_l:was_sentence_level(FN,Info),InfoL).

maybe_modalize(slot,_,P,P):-!.
maybe_modalize(Scope,PN,P,PP):-  maybe_modalize0(_Obj,Scope,P,PN,P,PP),!.
maybe_modalize(Scope,U,P, maybe_modalize_failed(Scope,U,P)).

not_true_or_qtrue(O):- O\==true, O\=='`'(true),!.
not_true_or_qtrue(_).

maybe_modalize1(Obj,Scope,O,V,P,PP):- maybe_modalize0(Obj,Scope,O,V,P,PP),!,nop(wdmsg(maybe_modalize0(Obj,Scope,O,V,P,PP))).
maybe_modalize1(_Obj,Scope,_,V,P,PP):- PP = failed_modalize(Scope,V,P),!.

skip_over_modalize(X):- var(X),!,fail.
skip_over_modalize('`'(X)):-!,skip_over_modalize(X).
skip_over_modalize(ti(_,_)).
%skip_over_modalize(database80(X)):-!,skip_over_modalize(X).
%skip_over_modalize(bE(_,_,_)).
skip_over_modalize(true).

maybe_modalize0(Obj,Scope,O,V,P,PP):- var(V),frozen(V,freeze(V, parser_chat80:ignore(X=Y))),!,maybe_modalize0(Obj,Scope,O,X+Y,P,PP).
maybe_modalize0(_Obj,_Scope,O,V,P,P):- var(V),!,not_true_or_qtrue(O).
maybe_modalize0(Obj,Scope,O, V,P,PP):- var(P),!,maybe_modalize1(Obj,Scope,O, V,call(P),PP).

maybe_modalize0(Obj,Scope,O, V,(ti(T,I),P),(ti(T,I),PP)):- !, maybe_modalize1(Obj,Scope,O, V,P,PP).

maybe_modalize0(Obj,Scope,O, V,numberof(X,P,L),numberof(X,PP,L)):- !, maybe_modalize1(Obj,Scope,O, V,P,PP).
maybe_modalize0(Obj,Scope,O, V,numberOf(X,P,L),numberOf(X,PP,L)):- !, maybe_modalize1(Obj,Scope,O, V,P,PP).
maybe_modalize0(Obj,Scope,O, V,setof(X,P,L),setof(X,PP,L)):- !, maybe_modalize1(Obj,Scope,O, V,P,PP).
maybe_modalize0(Obj,Scope,O, V,seto(X,P,L),seto(X,PP,L)):- !, maybe_modalize1(Obj,Scope,O, V,P,PP).
maybe_modalize0(Obj,Scope,O, V,setOf(X,P,L),setOf(X,PP,L)):- !, maybe_modalize1(Obj,Scope,O, V,P,PP).
maybe_modalize0(Obj,Scope,O, V,setof_oR_nil(X,P,L),setof_oR_nil(X,PP,L)):- !, maybe_modalize1(Obj,Scope,O, V,P,PP).
maybe_modalize0(Obj,Scope,O, V,:-(X,P),:-(X,PP)):- !, maybe_modalize1(Obj,Scope,O, V,P,PP).

%maybe_modalize0(Obj,Scope,O, V,'`'(P),'`'(PP)):- !, maybe_modalize1(Obj,Scope,O, V,P,PP).
%maybe_modalize0(Obj,Scope,O, V,'database80'(P),'database80'(PP)):- !, maybe_modalize1(Obj,Scope,O, V,P,PP).
%maybe_modalize0(Obj,Scope,O, V,^(X,P),^(X,PP)):- var(X), !, maybe_modalize1(Obj,Scope,O, V,P,PP).
%maybe_modalize0(Obj,Scope,O, V,(X,Y,P),(X,PP)):- skip_over_modalize(X), !, maybe_modalize1(Obj,Scope,O, V, (Y, P),PP).
maybe_modalize0(Obj,Scope,O, V,(X,P),(X,PP)):- skip_over_modalize(X), !, maybe_modalize1(Obj,Scope,O, V,P,PP).
%maybe_modalize0(Obj,Scope,O, V,'&'(X,P),'&'(X,PP)):- skip_over_modalize(X), !, maybe_modalize1(Obj,Scope,O, V,P,PP).
maybe_modalize0(Obj,Scope,O, V,XP,XPP):- fail, compound(XP),compound_name_arguments(XP,F,[X,P]),skip_over_modalize(X),
  compound_name_arguments(XPP,F,[X,PP]), !, maybe_modalize1(Obj,Scope,O, V,P,PP).

maybe_modalize0(_Obj,_Scope,_, adj(lf(P1)),P,(P1,P)).
maybe_modalize0(Obj,Scope,O,  adj(WithSelf),P,PP):- compound(WithSelf),subst(WithSelf,self,Obj,P1),
  maybe_modalize0(Obj,Scope,O, adj(lf(P1)),P,PP).
maybe_modalize0(Obj,Scope,O, PN+L,P,PP):- maybe_modalize1(Obj,Scope,O,PN,P,PM), maybe_modalize1(Obj,Scope,O,L,PM,PP).
maybe_modalize0(Obj,Scope,O,tense(L,_),P,PP):- !, maybe_modalize1(Obj,Scope,O,L,P,PP).
maybe_modalize0(Obj,Scope,O,[L|PN],P,PP):- nonvar(PN), !, maybe_modalize1(Obj,Scope,O,PN,P,PM), maybe_modalize1(Obj,Scope,O,L,PM,PP).
maybe_modalize0(Obj,Scope,O, past, P, PP):-!,maybe_modalize0(Obj,Scope,O,in_past, P, PP).
maybe_modalize0(Obj,Scope,O, being, P, PP):-!,maybe_modalize0(Obj,Scope,O,currently, P, PP).
maybe_modalize0(_Obj,_Scope,_,will, P, PP):- subst(P,in_past,will,PP),PP\==P,!.
maybe_modalize0(_Obj,_Scope,_,aux(_,[pres+fin]), P, PP):- subst(P,in_past,currently,PP),PP\==P,!.

maybe_modalize0(_Obj,_Scope,_,in_past, P, P):- (Past=will;Past=currently),sub_term(E,P),compound(E), E=modalized(Past,_),!.
maybe_modalize0(_Obj,_Scope,_,Past, P, P):- sub_term(E,P),compound(E), E=modalized(Past,_),!.
maybe_modalize0(_Obj,_Scope,_,true, P, P):-!.


maybe_modalize0(_Obj,_Scope,_,root, P, P):-!.


%maybe_modalize0(Obj,_Scope,_,info(Traits), P, PP):- member(pos(vbd),Traits),
maybe_modalize0(_Obj,_Scope,_,Fin, P, P):- skip_modalizing(Fin).
maybe_modalize0(_Obj,_Scope,_,voidQ, P, P):-!.

maybe_modalize0(_Obj,scope,_,cond( Because, S), P, cond_pred(Because,P,SP)):- i_s(S,SP),!.
%maybe_modalize0(Obj,slot, O,notP(Modal), P, PP):- !, not_true_or_qtrue(O),!,maybe_modalize1(Obj,slot,O,Modal, P, PP).
maybe_modalize0(Obj,Scope,O,identityQ(Modal), P, PP):- !, maybe_modalize1(Obj,Scope,O,Modal, P, PP).
maybe_modalize0(Obj,Scope,O,adv(Modal), P, PP):- !, maybe_modalize1(Obj,Scope,O,Modal, P, PP).
maybe_modalize0(Obj,Scope,O,aux(_,Modal), P, PP):- !, maybe_modalize1(Obj,Scope,O,Modal, P, PP).
maybe_modalize0(Obj,Scope,O,t(Modal,_,_), P, PP):- !, maybe_modalize1(Obj,Scope,O,Modal, P, PP).
maybe_modalize0(_Obj,_Scope,_,[],P,P).
maybe_modalize0(Obj,Scope,O,frame(FN),P,PP):- must(get_sentence_level_adverbs(FN,InfoL)),
  maybe_modalize0(Obj,Scope,O,[was_framed(FN)|InfoL],P,PP).
  

maybe_modalize0(_Obj,_Scope,_,negP, P, \+ P):-!.
maybe_modalize0(_Obj,_Scope,_,not,  P, \+ P).
maybe_modalize0(Obj,scope,O,notP(Modal), P, \+ PP):- !, nop(not_true_or_qtrue(O)),!,maybe_modalize1(Obj,scope,O,Modal, P, PP).
maybe_modalize0(Obj,Scope,O, negP(X),P,PP):-!, maybe_modalize0(Obj,Scope,O,[negP,X],P,PP).

%maybe_modalize0(Obj,_Scope,O,Modal, P, PP):- not_true_or_qtrue(O), atom(Modal),!,PP=..[Modal,P].
maybe_modalize0(_Obj,_Scope,_,M,P,modalized(M,P)).

skip_modalizing(info(_)).
skip_modalizing(tv). skip_modalizing(tv). skip_modalizing(dv(_)).
skip_modalizing(activeV). skip_modalizing(passiveV).
skip_modalizing(pres). skip_modalizing(part).
skip_modalizing(pl). skip_modalizing(sg).
skip_modalizing(main). skip_modalizing(aux).
skip_modalizing(inf). skip_modalizing(fin).
skip_modalizing(arg).


skip_modalizing(progresiveV).
skip_modalizing(X):- integer(X).


negate_inward(Q,NQ):- maybe_modalize(scope,not,Q,NQ),!.


smerge_segsl:- 

  A = [  w(how,
      [  pos(wrb),
         loc(1),
         lnks(3),
         txt("how"),
         link(1,'WHNP','WHNP#1'),
         link(2,'SBARQ','SBARQ#1'),
         link(3,'S1','S1#1')  ]),
   w(many,
      [  pos(jj),
         loc(2),
         lnks(3),
         txt("many"),
         link(1,'WHNP','WHNP#1'),
         link(2,'SBARQ','SBARQ#1'),
         link(3,'S1','S1#1')  ]),
   w(countries,
      [  pos(nns),
         loc(3),
         lnks(3),
         txt("countries"),
         link(1,'WHNP','WHNP#1'),
         link(2,'SBARQ','SBARQ#1'),
         link(3,'S1','S1#1')  ]),
   span([  seg(1,3),
           size(3),
           lnks(2),
           #('WHNP#1'),
           txt( ["how","many","countries"]),
           phrase('WHNP'),
           link(1,'SBARQ','SBARQ#1'),
           link(2,'S1','S1#1')  ]),
   w(does,
      [  pos(aux),
         loc(4),
         lnks(3),
         txt("does"),
         link(1,'SQ','SQ#1'),
         link(2,'SBARQ','SBARQ#1'),
         link(3,'S1','S1#1')  ]),
   w(the,
      [  pos(dt),
         loc(5),
         lnks(4),
         txt("the"),
         link(1,'NP','NP#1'),
         link(2,'SQ','SQ#1'),
         link(3,'SBARQ','SBARQ#1'),
         link(4,'S1','S1#1')  ]),
   w(danube,
      [  pos(nn),
         loc(6),
         lnks(4),
         txt("danube"),
         link(1,'NP','NP#1'),
         link(2,'SQ','SQ#1'),
         link(3,'SBARQ','SBARQ#1'),
         link(4,'S1','S1#1')  ]),
   span([  seg(5,6),
           size(2),
           lnks(3),
           #('NP#1'),
           txt( ["the","danube"]),
           phrase('NP'),
           link(1,'SQ','SQ#1'),
           link(2,'SBARQ','SBARQ#1'),
           link(3,'S1','S1#1')  ]),
   w(flow,
      [  pos(vb),
         loc(7),
         lnks(4),
         txt("flow"),
         link(1,'VP','VP#1'),
         link(2,'SQ','SQ#1'),
         link(3,'SBARQ','SBARQ#1'),
         link(4,'S1','S1#1')  ]),
   w(through,
      [  pos(rp),
         loc(8),
         lnks(5),
         txt("through"),
         link(1,'PRT','PRT#27'),
         link(2,'VP','VP#1'),
         link(3,'SQ','SQ#1'),
         link(4,'SBARQ','SBARQ#1'),
         link(5,'S1','S1#1')  ]),
   span([  seg(4,8),
           size(5),
           lnks(2),
           #('SQ#1'),
           txt( ["does","the","danube","flow","through"]),
           phrase('SQ'),
           link(1,'SBARQ','SBARQ#1'),
           link(2,'S1','S1#1')  ]),
   span([  seg(7,8),
           size(2),
           lnks(3),
           #('VP#1'),
           txt( ["flow","through"]),
           phrase('VP'),
           link(1,'SQ','SQ#1'),
           link(2,'SBARQ','SBARQ#1'),
           link(3,'S1','S1#1')  ]),
   span([  seg(8,8),
           size(1),
           lnks(4),
           #('PRT#27'),
           txt( ["through"]),
           phrase('PRT'),
           link(1,'VP','VP#1'),
           link(2,'SQ','SQ#1'),
           link(3,'SBARQ','SBARQ#1'),
           link(4,'S1','S1#1')  ]),
   w(?,
      [  pos('.'),
         loc(9),
         lnks(2),
         txt("?"),
         link(1,'SBARQ','SBARQ#1'),
         link(2,'S1','S1#1')  ]),
   span([  seg(1,9),
           size(9),
           lnks(0),
           #('S1#1'),
           txt( ["how","many","countries","does","the","danube","flow","through","?"]),
           phrase('S1')  ])  ],


   B = [  w(how,
      [  pos(wrb),
         root(how),
         loc(1),
         txt("how"),
         truecase('INIT_UPPER'),
         lnks(3),
         link(1,'WHADJP','WHADJP#25'),
         link(2,'WHNP','WHNP#1'),
         link(3,'SBARQ','SBARQ#1')  ]),
   w(many,
      [  pos(jj),
         root(many),
         loc(2),
         txt("many"),
         truecase('LOWER'),
         lnks(3),
         link(1,'WHADJP','WHADJP#25'),
         link(2,'WHNP','WHNP#1'),
         link(3,'SBARQ','SBARQ#1')  ]),
   span([  seg(1,2),
           size(2),
           lnks(2),
           #('WHADJP#25'),
           txt( ["how","many"]),
           phrase('WHADJP'),
           link(1,'WHNP','WHNP#1'),
           link(2,'SBARQ','SBARQ#1')  ]),
   w(countries,
      [  pos(nns),
         root(country),
         loc(3),
         txt("countries"),
         truecase('LOWER'),
         lnks(2),
         link(1,'WHNP','WHNP#1'),
         link(2,'SBARQ','SBARQ#1')  ]),
   span([  seg(1,3),
           size(3),
           lnks(1),
           #('WHNP#1'),
           txt( ["how","many","countries"]),
           phrase('WHNP'),
           link(1,'SBARQ','SBARQ#1')  ]),
   w(does,
      [  pos(vbz),
         root(do),
         loc(4),
         txt("does"),
         truecase('LOWER'),
         lnks(3),
         link(1,'VP','VP#1'),
         link(2,'SQ','SQ#1'),
         link(3,'SBARQ','SBARQ#1')  ]),
   w(the,
      [  pos(dt),
         root(the),
         loc(5),
         txt("the"),
         truecase('LOWER'),
         lnks(4),
         link(1,'NP','NP#1'),
         link(2,'VP','VP#1'),
         link(3,'SQ','SQ#1'),
         link(4,'SBARQ','SBARQ#1')  ]),
   w(danube,
      [  pos(nn),
         root(danube),
         loc(6),
         txt("danube"),
         truecase('INIT_UPPER'),
         lnks(4),
         link(1,'NP','NP#1'),
         link(2,'VP','VP#1'),
         link(3,'SQ','SQ#1'),
         link(4,'SBARQ','SBARQ#1')  ]),
   w(flow,
      [  pos(nn),
         root(flow),
         loc(7),
         txt("flow"),
         truecase('LOWER'),
         lnks(4),
         link(1,'NP','NP#1'),
         link(2,'VP','VP#1'),
         link(3,'SQ','SQ#1'),
         link(4,'SBARQ','SBARQ#1')  ]),
   span([  seg(5,7),
           size(3),
           lnks(3),
           #('NP#1'),
           txt( ["the","danube","flow"]),
           phrase('NP'),
           link(1,'VP','VP#1'),
           link(2,'SQ','SQ#1'),
           link(3,'SBARQ','SBARQ#1'),
           corefed,
           #(2),
           type('NOMINAL'),
           numb('SINGULAR'),
           gender('NEUTRAL'),
           animacy('INANIMATE'),
           repm(@(true))  ]),
   w(through,
      [  pos(in),
         root(through),
         loc(8),
         txt("through"),
         truecase('LOWER'),
         lnks(4),
         link(1,'PP','PP#1'),
         link(2,'VP','VP#1'),
         link(3,'SQ','SQ#1'),
         link(4,'SBARQ','SBARQ#1')  ]),
   span([  seg(4,8),
           size(5),
           lnks(1),
           #('SQ#1'),
           txt( ["does","the","danube","flow","through"]),
           phrase('SQ'),
           link(1,'SBARQ','SBARQ#1')  ]),
   span([  seg(4,8),
           size(5),
           lnks(2),
           #('VP#1'),
           txt( ["does","the","danube","flow","through"]),
           phrase('VP'),
           link(1,'SQ','SQ#1'),
           link(2,'SBARQ','SBARQ#1')  ]),
   span([  seg(8,8),
           size(1),
           lnks(3),
           #('PP#1'),
           txt( ["through"]),
           phrase('PP'),
           link(1,'VP','VP#1'),
           link(2,'SQ','SQ#1'),
           link(3,'SBARQ','SBARQ#1')  ]),
   w(?,
      [  pos('.'),
         root(?),
         loc(9),
         txt("?"),
         lnks(1),
         link(1,'SBARQ','SBARQ#1')  ]),
   span([  seg(1,9),
           size(9),
           lnks(0),
           #('SBARQ#1'),
           txt( ["how","many","countries","does","the","danube","flow","through","?"]),
           phrase('SBARQ')  ])  ],
  smerge_segs(A,B,C),
  print_tree(C).

:- fixup_exports.
