% =================================================================
% %%%%%%%%%%%%%%%%%%%%%%% CLAUSIFY %%%%%%%%%%%%%%%%%%%%%%%
% =================================================================
ignore_quant(LFOut)--> 
  put_attr_if_missing(LFOut,'$quant_needed',false), 
  put_attr_if_missing(LFOut,'$quant_marker',ignore).
put_attr_if_missing(LFOut,_Name,_Value):- nonvar(LFOut),!.
put_attr_if_missing(LFOut, Name, Value):- get_attr(LFOut,Name,_Was)-> true;put_attr(LFOut,Name,Value).
put_attr_if_missing(LFOut, Name, Value,S,S):-put_attr_if_missing(LFOut, Name, Value). 


into_split(verb,'Event').
into_split(np_head,'').
into_split(np,'').


into_args80('Event',_,A,Out):- atom(A),!, must(to_evt_name(A,Out)).
into_args80(_Mode,_X, A, wazVar(A)):- var(A),!.
into_args80(_Mode, X,quant(same,(N)),countOf(X,same,N)):-!.
into_args80( Mode,_X,A,Out):- atom(A),downcase_atom(A,A), toPropercase(A,APC),atom_concat(APC,Mode,Out),!.
into_args80(_,_X,A,A).

is_captitalized(A):-any_to_string(A,S),name(S,[N,_]), code_type(N,to_lower(L)),!,N\==L.




:- assert_if_new(baseKB:mpred_prop(parser_e2c,e2c_clausify,2,prologOnly)).

e2c_clausify(C, F):- e2c_clausify(C, F, _), !.

% Vars
e2c_clausify(A, A, []):- structureless(A),!.
% Universals
e2c_clausify( q(all, X, F0), F, [X|V]) :- !, e2c_clausify(F0, F, V).
% Implications
e2c_clausify((A0 => C0), (A => C), V) :- !, clausify_antecedent(A0, A, V)-> e2c_clausify(C0, C).
% Conjunctions
e2c_clausify(      A0C0,    ACOut, []) :- 
  compound(A0C0), functor(A0C0,CONJ,_), is_junct(CONJ,NCONJ), 
  show_failure(pred_juncts_to_list(CONJ,A0C0,List)), 
  must(filter_lits(NCONJ,List,ListF)),
  must_maplist(e2c_clausify,ListF,NewList),
  must(list_to_conjuncts(NCONJ,NewList,ACOut)),!.
% Literals
e2c_clausify(C0, C, []):-  clausify_literal(C0, C).

is_junct(',','&').
is_junct('&','&').
is_junct(';','v').
is_junct('v','v').


filter_lit(_NCONJ,Var):- var(Var),!, fail.
filter_lit(_NCONJ,H):- H=@= denotableBy(_,'3person').
filter_lit(_NCONJ,H):- H==[].
filter_lit(&,H):- H==true.                     
filter_lit(_NCONJ,H):- functor(H,denotableBy,_).

filter_lits(_NCONJ,Var,Var):- structureless(Var),!.
filter_lits(_NCONJ,[],[]):- !.
filter_lits(NCONJ,H&T,O):- filter_lit(NCONJ,H),!, filter_lits(NCONJ,T,O).
filter_lits(NCONJ,H&T,H&O):- !, filter_lits(NCONJ,T,O).
filter_lits(NCONJ,[H|T],O):- filter_lit(NCONJ,H),!, filter_lits(NCONJ,T,O).
filter_lits(NCONJ,[H|T],[H|O]):- filter_lits(NCONJ,T,O).

% Conjunctions
clausify_antecedent(      A0C0,    ACOut, Vars) :- compound(A0C0), functor(A0C0,CONJ,_), is_junct(CONJ,NCONJ), !, 
  must(pred_juncts_to_list(CONJ,A0C0,List)),
  must(filter_lits(NCONJ,List,ListF)),
  must_maplist(clausify_antecedent,ListF,NewList,VarListS),
  append(VarListS,VarList),list_to_set( VarList,Vars),
  must(list_to_conjuncts(NCONJ,NewList,ACOut)).
% clausify_antecedent(E0 & F0, (E , F), V) :-  clausify_antecedent(E0, E, V0), clausify_antecedent(F0, F, V1), conc(V0, V1, V).

% Existentials
clausify_antecedent(q(NotAll, X, F0), F, [X|Vs]) :- NotAll\==all, clausify_antecedent(F0, F, Vs).
% Literals
clausify_antecedent(L0, L, []):-  clausify_literal(L0, L).

% Verbatum
clausify_literal(S,O):- simplify_lf(S,O),!.
clausify_literal(L, L).

% make_object(Frame, Obj, Written, Y, MadeObj):- to_evt_name(Written, Proper), atom_concat(Obj, Proper, Pred), MadeObj=.. [Pred, Frame, Y].
%     to_evt_name(Writing, ProperEvent),
%     make_object(Frame, Writing, 'To', Y, MadeObj).
%     make_object(Frame, 'obj(_K)', Written, Z, MadeOblique).

% OLD
make_time_info(Frame, Time, z(Writing, X, Y), Out):- nonvar(Writing), !,
  to_evt_name(Writing, ProperEvent),
  make_time_info(Frame, Time, iza(Frame, ProperEvent) & doer(Frame, X) & patient(Frame, Y), Out).

make_time_info(Frame, Time, z(Writing, X), Out):- nonvar(Writing), !,
  to_evt_name(Writing, ProperEvent),
  make_time_info(Frame, Time, iza(Frame, ProperEvent) & doer(Frame, X), Out).

make_time_info(Frame, Time, LF, Out):- Time\==nonfinite, Time\== pres+fin, nvd(LF, Frame),
 conjoin_lf(LF , iza(Frame, timeFn(Time)), Out).
make_time_info(Frame, _Time, LF, LF):- nvd(LF, Frame).

expand_1arg(_,A,B):- structureless(A),!,A=B.
expand_1arg(t,List,Out):- is_list(List),List=[Obj|Written],atomic(Obj),must_maplist(toPropercase,Written,WrittenO),i_name(Obj,WrittenO,Out).
expand_1arg(_,str(Name), Out):- any_to_string(Name,Out),!.
expand_1arg(iza,timeFn(Time), timeFn('vPast')):-  (Time== past+fin ; Time==pp). % Time\==nonfinite,
expand_1arg(_,A,A).

expand_args(_,_,A,B):- structureless(A),!,A=B.
%expand_args(_,_,[t|Args],Out):- !, must_maplist(expand_1arg(t),Args,ArgsO),Out =..[t|ArgsO].
expand_args(_,_,[a,Type,Name], Out):- i_name(Type,Name,Out),!.
expand_args(_,_,[T|Args],Out):- atom(T),must_maplist(expand_1arg(T),Args,ArgsO),Out =..[T|ArgsO].
expand_args(C,_,[P|ARGS],Out):- 
  must_maplist(expand_lf(C,0),ARGS,ARGSO),
   Out =..[P|ARGSO].


expand_lf(_,_,A,B):- structureless(A),!,A=B.
expand_lf(C,N,A & Rest,B & List):- expand_lf(C,N,A,B),!,expand_lf(C,N,Rest,List).
expand_lf(C,N,[A|Rest],[B|List]):- expand_lf(C,N,A,B),!,expand_lf(C,N,Rest,List).
expand_lf(_,_,t(DoesWrite, X, Y ), Out):- atom(DoesWrite),
   Out=..[DoesWrite, X, Y].
expand_lf(C,N, z(Writing, X ), Out):- 
  to_evt_name(Writing, ProperEvent),
  expand_lf(C,N, iza(Frame, ProperEvent) & doer(Frame, X), Out).

expand_lf(_,_, iza(_,timeFn(XX)), true):- XX==pres+fin,!.
expand_lf(_,_, iza(_,timeFn(XX)), true):- XX==nonfinite,!.
expand_lf(_,_, quant(X, _Type), true):- nonvar(X),!.
expand_lf(_,_, quant(X, Type), true):- 
((get_attr(X,'$quant_marker',Old),Type==Old)->true;
   (put_attr(X,'$quant_marker',Type),put_attr(X,'$quant_needed',true))).

expand_lf(C,N, z(Writing, X, Y ), Out):- 
  to_evt_name(Writing, ProperEvent),
  expand_lf(C,N, iza(Frame, ProperEvent) & doer(Frame, X) & patient(Frame, Y), Out).

expand_lf(C,N,A,B):- compound_name_arity(A,P,0),!,expand_lf(C,N,P,BP),compound_name_arguments(B,BP,0),!.

expand_lf(C,N,A,Out):- 
  compound_name_arguments(A,P,ARGS),
   expand_args(C,N,[P|ARGS],Out).


expand_quants(A,B):-expand_lf(A,A0), term_variables(A0,Vars),add_quants(Vars,A,AB),expand_lf(AB,B).
simplify_lf(A,B):-expand_lf(simplify,0,A,B).
expand_lf(A,B):-expand_lf(lf,0,A,B).


add_quants([],A,A):-!.
add_quants([X|T],A,C):- var(X),
   get_attr(X,'$quant_needed',Need),Need\==false,  
   put_attr(X,'$quant_needed',false),
  (get_attr(X,'$quant_marker',Was);Was='exists'),!,
   det_quantify(Was,  X, A,   Quanted),
   add_quants(T,Quanted,C).
add_quants([_|T],A,C):- add_quants(T,A,C).

assertion_callable(C):- callable(C),!.
assertion_callable(_):- dumpST.

conjoin_lf(LF1, LF2, Out):- notrace((expand_lf(LF1,LF1A),LF1\==LF1A)),!,conjoin_lf(LF1A,LF2,Out).
conjoin_lf(LF2, LF1, Out):- notrace((expand_lf(LF1,LF1A),LF1\==LF1A)),!,conjoin_lf(LF2,LF1A,Out).
conjoin_lf(LF1, LF2, Out):- nonvar(LF2),LF2=(LF2a & LF2b),
                            conjoin_lf(LF1, LF2a, M),conjoin_lf(M, LF2b, Out).
conjoin_lf(LF1, LF2, Out):- notrace(conjoin_lf0(LF1, LF2, Out)).
conjoin_lf(LF1, LF2, Out, L, L):- notrace(conjoin_lf0(LF1, LF2, Out)).

%conjoin_lf0(LF1, LF2, Out):- assertion(nonvar(LF2)), assertion(nonvar(LF1)), fail.

conjoin_lf00(LF1, LF2, Out):- LF2==true, !, Out=LF1.
                                 
conjoin_lf0(LF1, LF2, Out):- LF1==LF2, !, Out=LF1.

%conjoin_lf0(LF1, LF2, LF1 & LF2):- var(LF1),var(LF2), !.
conjoin_lf0(LF1, LF2, LF1 & LF2):- var(LF2), !.
conjoin_lf0(LF1, LF2, LF2 & LF1):- var(LF1), !.

conjoin_lf0(LF1, LF2, _):- assertion_callable(LF1), assertion_callable(LF2),fail.

conjoin_lf0(LF1, LF2, Out):- compound(LF2), (LF2 = (LF2a & LF2b)), !, conjoin_lf0(LF1, LF2a, M), conjoin_lf0(M, LF2b, Out).

%conjoin_lf0(LF1, LF2, Out):- expand_lf(LF1, LF11),LF1\==LF11,!,conjoin_lf0(LF11, LF2, Out).
%conjoin_lf0(LF1, LF2, Out):- expand_lf(LF2, LF22),LF2\==LF22,!,conjoin_lf0(LF1, LF22, Out).

conjoin_lf0(LF1, LF2, Out):- conjoin_lf00(LF1, LF2, Out), !.
conjoin_lf0(LF1, LF2, Out):- conjoin_lf00(LF2, LF1, Out), !.
conjoin_lf0(precond(LF1),  LF2, Out):- !, conjoin_lf1(LF2, precond(LF1), Out).
conjoin_lf0(traits(X,LF1),  LF2, Out):- !, conjoin_lf1(LF2, traits(X,LF1), Out).
conjoin_lf0(quant(X,LF1),  LF2, Out):- !, conjoin_lf1(LF2, quant(X,LF1), Out).
conjoin_lf0(LF1, LF2, Out):- conjoin_lf1(LF1, LF2, Out), !.

det_quantify(no,     X, Found,~(q(exists,X, Found))).
det_quantify(exists, X, Found,  q(exists,X, Found)).
det_quantify(every,  X, Found,    q(all, X, Found)).
det_quantify(ignore,_X, Found,              Found ).
%det_quantify(any,   _X, Found,              Found ).
det_quantify(Q,      X, Found,    q(Q,   X, Found)).

del_e2c_attributes(Term):- % leave in $frame_conjunction?
  remove_term_attr_type(Term, ['$quant_needed','$quant_marker','$root']).

'$root':attr_unify_hook(_,_) :- !.
'$quant_marker':attr_unify_hook(_,_) :- !.
'$quant_needed':attr_unify_hook(_,_) :- !.
'$frame_conjunction':attr_unify_hook(_,_) :- !.

conjoin_lf1(LF1, traits(X, Traits), Out):- nonvar(Traits), !, add_traits(X, Traits, LF1, Out).  
conjoin_lf1(LF1, quant(X, _Type), LF1):- nonvar(X),!.
conjoin_lf1(LF1, quant(X, Type), Out):-  
  (get_attr(X,'$quant_marker',_)-> Out=LF1 ; 
   (put_attr(X,'$quant_marker',Type),put_attr(X,'$quant_needed',false), det_quantify(Type,X,LF1,Out))),!.

conjoin_lf1(LF1, quant(X, Type), Out):-  
 (nb_current('$quant_marker',F);F=[]), 
    (sub_var(X,F) -> Out = LF1   ; ( nb_setval('$quant_marker',[X|F]), det_quantify(Type,X,LF1,Out))),!.
conjoin_lf1(LF1, quant(X, Type), Out):- conjoin_lf0(LF1, v(X), Found), (LF1==Found -> Out = LF1 ; det_quantify(Type,X,Found,Out)).

conjoin_lf1(LF1, precond(q(Q, X, LF2)), Out):- !, conjoin_lf1(LF1, q(Q, X, precond(LF2)), Out).
%conjoin_lf1(LF1,           precond(q(Q, X, LF2)), q(Q,  X, Out)):- conjoin_lf0(LF1,precond(LF2), Out).
conjoin_lf1(LF1,                   q(Q, X, LF2),  q(Q,  X, Out)):- conjoin_lf0(LF1, LF2, Out).
conjoin_lf1(q(Q1, Y, LF1),                 LF2,   q(Q1, Y, Out)):- conjoin_lf0(LF1, LF2, Out).
conjoin_lf1((LF1 => Post), precond(        LF2),  (Out => Post)):- conjoin_lf0(LF1, LF2, Out).
conjoin_lf1(LF1          , precond(        LF2),   (LF2 => LF1)).
conjoin_lf1((LF1 => Post),                 LF2,    (LF1 => Out)):- conjoin_lf0(Post, LF2, Out).

conjoin_lf1(LF1, LF2, Out):- conjoin_lf4(LF1, LF2, Out), !.
conjoin_lf1(LF1, LF2, Out):- conjoin_lf4(LF2, LF1, Out), !.
% conjoin_lf0(LF2, LF1, Out):- compound(LF2), (LF2 = (LF2a & LF2b)), !, conjoin_lf0(LF1, LF2a, M), conjoin_lf0(M, LF2b, Out).
conjoin_lf1(LF1, LF2, LF1 & LF2):-!.



conjoin_lf4(LF1, LF2, Out):- compound(LF1), subst(LF1, '_addto', NEW, Out), LF1\==Out, NEW = (LF2 & '_addto'), !.



add_traits( X, T, LF, Out, L, L):- notrace(add_traits0( X, T, LF, Out)).
add_traits( X, T, LF, Out):- notrace(add_traits0( X, T, LF, Out)).

add_traits0( X, V, LF, traits(X,V) & LF):- var(V),!.
add_traits0(_X, V, LF, LF):- (V == true ; V==[]) , !.
add_traits0( X, [H|List], LF, LFO):- !,
  add_traits0(X, H, LF, LFM),
  add_traits0(X, List, LFM, LFO).
add_traits0( X, (H,List), LF, LFO):- !,
  add_traits0(X, H, LF, LFM),
  add_traits0(X, List, LFM, LFO).
add_traits0( X, H;List, LF, LF & (LFM;LFO)):- !,
  add_traits0(X, H, true, LFM),
  add_traits0(X, List, true, LFO).
add_traits0( X, H v List, LF, LF & (LFM v LFO)):- !,
  add_traits0(X, H, true, LFM),
  add_traits0(X, List, true, LFO).
add_traits0( X, H & List, LF, LFO):- !,
  add_traits0(X, H, LF, LFM),
  add_traits0(X, List, LFM, LFO).
add_traits0( X, T, LF, Out):- var_1trait( X, T, TLF), !, conjoin_lf(LF, TLF, Out).
add_traits0(_X, TLF, LF, Out):- conjoin_lf(LF, TLF, Out).

% var_1trait(X, sg, atMost(X, 1)).

%var_1trait(X, Prop, Out):- var(Prop), !, fail, Out = Prop.
var_1trait(X, Prop, call(X^Prop)):- var(Prop), !.
var_1trait(_, True, true):- True == true.
%var_1trait(X, sg, v(X)).
var_1trait(_, v_arg(_), true).
var_1trait(_, sg, true).
var_1trait(X, pl, ~numberOf(X, 1)).
% var_1trait(_, pl, true). %
var_1trait(_, infpl, true).
var_1trait(_, fin, true).
%var_1trait(X, the(X), true).
var_1trait(X, past, iza(X,timeFn(vPast))).
var_1trait(X, pres, iza(X,timeFn(vNow))).

%var_1trait(X, np_head(A,B,C), Out):- must_maplist(into_args80(np,X),[A,B,C],ABC),add_traits( X, ABC, true, Out).
var_1trait(X, reduced_rel(X,Data), Out):- rewrite_result( X, verb,X,Data,Out).
var_1trait(X, rel(X,Data), Out):- rewrite_result( _, verb,_Frm,Data,Out).
var_1trait(X, prep_phrase(prep(With),Data), prep(With,X,Y) & Out):-  
  add_traits(Y,Data,true,Out).
  

var_1trait(_X, wh(Subj305,Subj304), true):- =(Subj305,Subj304),!.
var_1trait(_X, wh(Subj305,Subj304), =(Subj305,Subj304)):- !.

% var_1trait(X, Atom, Out):- atom(Atom), i_name(t, Atom, Value), into_isa3(X, Value, Out).
var_1trait(X, Atom, Out):- atom(Atom), into_isa3(X, Atom, Out).
var_1trait(X, A+B, AA_BB):- number(A),!, var_1trait(X, person(A)+B, AA_BB).
var_1trait(X, A+B, AA & BB):- !, var_1trait(X, A, AA), var_1trait(X, B, BB).
var_1trait(_, pos(Var), true):- var(Var),!.
var_1trait(X, iza(Value), Prop):- !, var_1trait(X, Value, Prop).
var_1trait(X, det(Value), quantV(X,Value)):- !.
var_1trait(X, adj(Value), Out):- !, into_isa3(X, adjFn(Value), Out). 
var_1trait(_, gender(Var), true):- var(Var).
var_1trait(X, gender(masc), Prop):- into_isa3(X, tMale, Prop).
var_1trait(X, gender(fem), Prop):- into_isa3(X, tFemale, Prop).
var_1trait(X, gender(neut), Prop):- into_isa3(X, tInanimateObject, Prop).
var_1trait(X, pronoun(Var), Prop):- !, var_1trait(X, gender(Var), Prop).
var_1trait(_, person(Var), true):- var(Var).
var_1trait(X, person(N), denotableBy(X,'agreementFn'(Str))):- to_person(N,Str).
var_1trait(X, denote(Any), denotableBy(X, Str)):- any_to_string(Any, Str).
var_1trait(X, nameOf(Any), nameOf(X,Str)):- any_to_string(Any, Str).
var_1trait(VF,varg(dir,DATA),(directObject(VF,DIR) & O)):- nvd('Obj',DIR), !, add_traits(DIR,DATA,true,O).


var_1trait(X, Data, O):- 
 compound_name_arguments(Data,F,Args), into_split(F,Mode),!,
 must_maplist(into_args80(Mode,X),Args,Args80),
 add_traits(X,Args80,true,O).

var_1trait(X, Str, denotableBy(X, Str)):- string(Str), nvd(Str,X).

var_1trait(X, Prop, denotableBy(X, XProp)):- compound(Prop), functor(Prop,F,A), atom_concat(_,'Fn',F),
  arg(A,Prop,Arg2), must_or_rtrace((any_to_string(Arg2,Str),i_name(i,F,FF),XProp=.. [FF,Str],!,nvd(Str,X))).
var_1trait(X, Prop, Prop):- compound(Prop), sub_var(X, Prop).  % arg(2, Prop, _).
var_1trait(X, Prop, ZProp):- compound(Prop), Prop=.. [F, Arg2], XProp=.. [F, X, Arg2], !,
 (var_1trait(X, XProp, ZProp) -> true ; ZProp = XProp).


to_person(1,'1st').
to_person(2,'2nd').
to_person(3,'3rd').
to_person(N,N).

is_kr_functor(denotableBy).
is_kr_functor(directObject).
is_kr_functor(subject).
is_kr_functor(iza).
is_kr_functor(AFn):- downcase_atom(AFn,DC),DC\==AFn.
is_kr_functor(AFn):- atom_concat(_,'Fn',AFn).

rewrite_result(_SF,_Mode,_VF,I,I):- \+compound(I),!.
rewrite_result(_SF,_Mode,_VF,I,I):- functor(I,KR,_), is_kr_functor(KR),!.
rewrite_result( SF, Mode, VF, A & B, AB):- !, rewrite_result( SF, Mode, VF, [A,B],AB).
rewrite_result( SF, Mode, VF, [A], AA):- !, rewrite_result( SF, Mode, VF, A,AA).
rewrite_result( SF, Mode, VF, [A|B], AA & BB):- !, rewrite_result( SF, Mode, VF, A,AA),rewrite_result( SF, Mode, VF, B,BB).
rewrite_result( SF, Mode, VF,[]^X,Out):-!,rewrite_result( SF, Mode, VF,X,Out).
rewrite_result( SF, Mode, VF,Vars^X,Vars^Out):-!,rewrite_result( SF, Mode, VF,X,Out).
rewrite_result( SF, Mode, VF,:-(assertion80(X), Ass),assertion80(Out)):-!, rewrite_result( SF, Mode, VF,X^Ass,Out).
rewrite_result( SF, Mode, VF,:-(answer80(X), Ass),answer80(Out)):-!, rewrite_result( SF, Mode, VF,X^Ass,Out).
rewrite_result( SF, Mode, VF, decl(S),O):-!,rewrite_result( SF, Mode, VF,S,O).
rewrite_result( _SF,_WazMode, VF,s80(S,V,O,R), Out):-
  add_traits(NewFrame,V,true,Step0),
  add_traits(VF,subframe(NewFrame),Step0,Step1),  
  conjoin_lf(Step1,theSubject(NewFrame,X),Step2),
  add_traits(X,S,Step2,Step3),
  add_traits(NewFrame,O,Step3,Step4),
  add_traits(NewFrame,R,Step4,Out),!.
/*
rewrite_result(_SF,_Mode, VF,verb(V,_Active,Time,[],pos(_Pos_Ret)),Out):- !, add_traits( VF, [V,verbFn(V),Time], true, Out).
rewrite_result( SF, Mode, VF,varg(subj,DATA),(theSubject(VF,DIR) & O)):- nvd('Subj',DIR), !, rewrite_result( SF, Mode, DIR,DATA,O).
rewrite_result(_SF,_Mode, VF,quant(same,nb(N)),countOf(VF,same,N)).
rewrite_result(_SF,_Mode, VF,S,):- 
 compound_name_arguments(S,F,Args), (into_split_o(F);into_split(F)),!,
 must_maplist(into_args80(VF),Args,Args80),
 add_traits(VF,Args80,true,O).
rewrite_result( SF, Mode, VF,S,O):- 
 compound_name_arguments(S,F,Args), into_split(F),!,
 must_maplist(rewrite_result( SF, F, VF),Args,ArgsO),
 rewrite_result( SF, Mode, VF,ArgsO,O).
*/

rewrite_result(_SF,_Mode,_VF,S,O):- arg(_,S,E),var(E),!, S=O.

rewrite_result( SF, Mode, VF,S,O):- 
 compound_name_arguments(S,F,Args),
 must_maplist(rewrite_result( SF, Mode,VF),Args,ArgsO),
 compound_name_arguments(M,F,ArgsO),
 ((Args==ArgsO;true) -> M = O ; rewrite_result( SF, Mode, VF,M,O)).

