% =================================================================
% %%%%%%%%%%%%%%%%%%%%%%% DCG UTILS %%%%%%%%%%%%%%%%%%%%%%%
% =================================================================
% plt --> quietly(([], {plt})).

% iterative deeping flag
plt(A, A):- notrace(plt).


tag(Frame, Tag, iza(Frame, TAG)) --> {atomic(Tag)}, !, [$, TAG], {atom(TAG),downcase_atom(TAG, Start), atom_concat(Tag, _, Start)}.
tag(Frame, Cmp, N) --> {compound(Cmp),functor(Cmp, F, _)}, tag(Frame, F, N).


:- use_module(library(logicmoo/dcg_must)).

% Push a new term onto DCG stack
dcg_push_w2(List, S, ListS):- is_list(List), !, t_to_w2(List,ListO), append(ListO, S, ListS).
dcg_push_w2(A, S, [B|S]):- t_to_w2(A,B).

theText1(IC, A, B) :- notrace(as_w2_segs(A, C)),!,theText1(IC, C, B).
theText1(IC)-->notrace(theText11(IC)).

w2txt001(IC, A, B) :- notrace(as_w2_segs(A, C)),!,w2txt001(IC, C, B).
w2txt001(Cmp) --> consume_spans_hack_e2c, % next_as_word_e2c,
   {!},[Cmp],{assertion(nonvar(Cmp))},consume_spans_hack_e2c.

theTextW2(Text,L) --> w2txt001(Cmp),{W2=w(Text,L),(compound(Cmp)->W2=Cmp;(Text=Cmp,L=open)),parser_e2c:add_prev_w2(W2)}.

w2txt01(Text) --> theTextW2(Text,_L),!.
%w2txt01(Text) --> w2txt001(A), {atomic(A),!,A=Text}.
%w01(Text) --> w001(A), {Text=A}.

consume_spans_hack_e2c --> [span(NV)],{nonvar(NV),parser_e2c:add_prev_span(span(NV)),!},consume_spans_hack_e2c.
consume_spans_hack_e2c --> [].

move_ahead_spans_e2c(S1,S2):- partition(\=(span(_)),S1,W,S),!,append(W,S,S2).

next_as_word_e2c(S1,S2):- select(W2,S1,SM),compound(W2),W2=w(_,_),!,S2=[W2|SM].
next_as_word_e2c(S1,S1).

w2txt(Text) --> consume_spans_hack_e2c,w2txt01(Text),consume_spans_hack_e2c.
%w2txt(W0)--> [w(W0,_)],!.
%w2txt(IC)--> [W0],{!,\+compound(W0),IC=W0}.


theText11(IC)--> {var(IC),!},w2txt(W0),{notrace((assertion(nonvar(W0);nonvar(IC)), the_text_unif(IC,W0)))}.
theText11([])--> !, [].
theText11(IC)--> {atomic(IC),!},w2txt(W0),{the_text_unif(IC,W0)},!.
theText11([H|T])--> theText11(H),!,theText11(T).
%theText1(Txt)--> [w(Txt,_)].

theBaseForm(Tex) --> theText1(Text),{atomic(Tex),atomic(Text),atom_concat(Tex,_,Text)}.

optionalText1(X) --> theText1(X),!.
optionalText1(_) --> [].

dcg_call(DCG1,Arg)--> {notrace(append_term(DCG1,Arg,DCG0))},phrase(DCG0).
dcg_call(DCG2,Arg1,Arg2,S,E):-
   notrace((append_term(DCG2, Arg1, DCG1),
            append_term(DCG1, Arg2, DCG0))),
  phrase(DCG0,S,E).

dcg_thru_2args(DCG2, In, Out) -->
   dcg_call(DCG2,In,Mid),
   dcg_thru_2args(DCG2, Mid, Out).
dcg_thru_2args(_, Same, Same) --> [].

% nvd(N, X) --> quietly(( [], {nvd(N, X)})).
nvd(N, X, A, A):- notrace(nvd(N, X)).

% =================================================================
% %%%%%%%%%%%%%%%%%%%%%  UTILS %%%%%%%%%%%%%%%%
% =================================================================
nvd(N, X):- var(N), var(X), !.
nvd(N, X):- var(N), nonvar(X), !, nvd(X, N), !.
nvd(_, X):- nonvar(X), !.
nvd('&'(N , _), X):- nonvar(N), !, nvd(N, X).    
nvd(N, X):- compound(N), N=.. [z, F|_], may_debug_var([F, '_Frame'], X).
nvd(N, X):- atom(N),  name(N, Name), last(Name, C), \+ char_type(C, digit), !, gensym(N, NN), !, may_debug_var(NN, X), !.
nvd(N, X):- may_debug_var(N, X), !.

into_isa3(I,C,ISA):- notrace(into_isa3_0(I,C,ISA)).

into_isa3_0(X, Y, iza(X, YY)):- atom(Y), (atom_concat('t', Name, Y) -> YY=Y; (maybe_toPropercase(Y, YY), Name=YY)), gensym(Name, VarName), nvd(VarName, X).
into_isa3_0(X, Y, iza(X,  Y)):- structureless(Y),!.
into_isa3_0(_, timeFn(infpl), true).
into_isa3_0(_, timeFn(infinitive), true).
into_isa3_0(X, Y, iza(X, YY)):- compound(Y), Y=.. [Fn, Word], atom_concat(_, 'Fn', Fn), any_to_string(Word, Str), !, YY=.. [Fn, Str].
into_isa3_0(X, Y, iza(X, Y)):-!.

% maybe_toPropercase(X,Y):- downcase_atom(X,DC),DC\==X,!,X=Y.
maybe_toPropercase(X,Y):- upcase_atom(X,U),X==U,toPropercase(X,Y),!.
maybe_toPropercase(X,Y):- first_char_to_upper(X,Y).


conc([], L, L).
conc([H|T], L, [H|R]) :- conc(T, L, R).


