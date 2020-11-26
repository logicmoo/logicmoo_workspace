% =================================================================
% %%%%%%%%%%%%%%%%%%%%%%% DCG UTILS %%%%%%%%%%%%%%%%%%%%%%%
% =================================================================
% plt --> quietly(([], {plt})).

% iterative deeping flag
plt(A, A):- notrace(plt).

quietly(DCG, S, E):- setup_call_cleanup(quietly(phrase(DCG, S, E)),true,true).
% quietly(DCG,S,E):- quietly(phrase(DCG,S,E)).
notrace(DCG,S,E):- quietly(DCG,S,E). %notrace(phrase(DCG,S,E)).
must(DCG,S,E):- must(phrase(DCG,S,E)).
ignore_must(DCG,S,E):- ignore_must(phrase(DCG,S,E)).

dcg_peek(DCG,S,S):- phrase(DCG,S,_).

tag(Frame, Tag, iza(Frame, TAG)) --> {atomic(Tag)}, !, [$, TAG], {atom(TAG),downcase_atom(TAG, Start), atom_concat(Tag, _, Start)}.
tag(Frame, Cmp, N) --> {compound(Cmp),functor(Cmp, F, _)}, tag(Frame, F, N).

%dcg_must_each_det(G, S, E):- phrase(G, S, E), !.

dcg_must_each_det(_, S, _):- S == [], !, fail.
dcg_must_each_det((G1, G2), S, E):- !, must(phrase(G1, S, M)), !, dcg_must_each_det(G2, M, E), !.
dcg_must_each_det(G, S, E):- !, must(phrase(G, S, E)), !.

dcg_and(DCG1, DCG2, S, E) :- dcg_condition(DCG1, S, E), phrase(DCG2, S, E), !.
dcg_unless(DCG1, DCG2, S, E) :- \+ dcg_condition(DCG1, S, _), !, phrase(DCG2, S, E).
dcg_when(DCG1, DCG2, S, E) :- dcg_condition(DCG1, S, _),!, phrase(DCG2, S, E).
dcg_length(Len,S,E):- \+ var(Len) -> (length(L,Len), append(L,E,S)); 
   (length(S,Full),between(Full,0,Len),length(L,Len), append(L,E,S)).
dcg_from_right(DCG1, DCG2, S, E) :- length(S,Full), between(Full,0,Start), dcg_scan(DCG1,Start,DCG2,S,E).
dcg_from_left(DCG1,  DCG2, S, E) :- length(S,Full), between(0,Full,Start), dcg_scan(DCG1,Start,DCG2,S,E).

dcg_scan(DCG1,Start2,DCG2,S,E):- 
  length(Before,Start2), append(Before,Mid,S), \+ \+ phrase(DCG2, Mid, _), 
  phrase(DCG1, Before, []), phrase(DCG2, Mid, E).

dcg_condition([], S, _):- S \== [], !, fail.
dcg_condition(DCG, S, E):- phrase(DCG, S, E).

% Push a new term onto DCG stack
dcg_push(List, S, ListS):- is_list(List), !, t_to_w2(List,ListO), append(ListO, S, ListS).
dcg_push(A, S, [B|S]):- t_to_w2(A,B).

theText1(IC)-->notrace(theText11(IC)).

w2txt(W0)--> [w(W0,_)],!.
w2txt(W0)--> [W0],!.

theText11(IC)--> {var(IC),!},w2txt(W0),notrace(({parser_tokenize:any_nb_to_atom(W0,W1), downcase_atom(W1,IC)})).
theText11([])--> !, [].
theText11(IC)--> {atomic(IC),downcase_atom(IC,DC)},w2txt(W0),{parser_tokenize:any_nb_to_atom(W0,W1),(W1=DC;downcase_atom(W1,DC))},!.
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
nvd(N, X):- atom(N), name(N, Name), last(Name, C), \+ char_type(C, digit), !, gensym(N, NN), !, may_debug_var(NN, X), !.
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

