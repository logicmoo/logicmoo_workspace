
%:- use_module(library(logicmoo_clif)).
%:- use_module(library(logicmoo/typesystem/mpred_type_constraints)).

:- use_module(library(logicmoo/dcg_meta)).
:- use_module(library(logicmoo/util_bb_frame)).

:- ensure_loaded(library(cgp_lib/cgp_swipl)).
:- ensure_loaded(library(pfc_lib)).

:- multifile_data(cg_test_data/2). 
%:- multifile_data(skip_cg_test_data/2). 
:- multifile_data(cg/2).

%:- current_op(X,Y,'->'),push_operators([op(X,Y,'<-')]).

debug_fvar(N,_):- (\+ ground(N) ; number(N)),!.

debug_fvar(N,V):- debug_var(N,V).

cg_new(CG):- must(push_frame_info([], CG)),!.
push_frame_info(Info,Frame):- atom(Frame),must(nb_current_no_nil(Frame,CG)),!,push_frame_info(Info,CG).
push_frame_info(Info,Frame):- push_frame(Info,Frame).
push_frame_info(Info,Frame,S,S):-push_frame_info(Info,Frame).

push_frame_concept(C,Frame):-atom(Frame),must(nb_current_no_nil(Frame,CG)),!,push_frame_concept(C,CG).
push_frame_concept(C,Frame):-nop(push_frame(C,Frame)).
push_frame_concept(C,Frame,S,S):-push_frame_concept(C,Frame).

nb_set_add(X,Y,S,S):- must(nb_set_add(X,Y)).


cg_demo :- make, forall((cg_test_data(TstAtts,X), \+ memberchk(failing,TstAtts)),must(do_cg_test(TstAtts,X))).
cg_reader_tests :- make, forall(cg_test_data(TstAtts,X), must(do_cg_test(TstAtts,X))).

do_cg_test( TstAtts,_):- memberchk(skip,TstAtts),!.
do_cg_test( TstAtts,X):- select(cg_dialect(What),TstAtts,NewTstAtts),!, 
  locally_setval(cg_dialect,What,do_cg_test( NewTstAtts,X)).
do_cg_test( TstAtts,X):- memberchk(xcall,TstAtts),!, must(ignore(call_cg(cg_text(X)))).
do_cg_test(_TstAtts,X):- must(ignore(assert_cg(cg_text(X)))).

subset_loose(S1,S2):- \+ is_list(S2),!,member(S2,S1),!.
subset_loose(S1,S2):- subset(S1,S2).

cg_test_data(TstAtts,X):- nonvar(TstAtts), var(X), !,
  forall((cg_test_data(WasTstAtts,X),subset_loose(WasTstAtts,TstAtts)),
     cg_test_data( WasTstAtts,X)).
cg_test_data(TstAtts,X):- nonvar(TstAtts), nonvar(X), !, 
   exclude(=(skip),TstAtts,NTstAtts),
   do_cg_test(NTstAtts,X).


%assert_cg(X):- nb_current_no_nil(named_graph,ID),!,pred_cg(assert_cg_real,X),!.
assert_cg(X):- newId(Id),!,locally(nb_setval(named_graph,anonymous(Id)), pred_cg(assert_cg_real,X)),!.

assert_cg_real_now(named_graph(Id,X)):- !, assert_cg_real_now(Id,X).
assert_cg_real_now(X):- nb_current_no_nil(named_graph,Id),  assert_cg_real_now(Id,X).

assert_cg_real_now(anonymous(_),[named_graph(Id,X)]):- nonvar(Id), !, assert_cg_real_now(Id,X).
%assert_cg_real_now(anonymous(Id),X):- nonvar(Id), !, assert_cg_real_now(Id,X).
assert_cg_real_now(Id,X):- frmprint(named_graph(Id, X)), ain(cg(Id,X)).

assert_cg_real(X):- assert_cg_real_now(X),!.
assert_cg_real(X):- is_list(X),list_to_conjuncts(X,J),!,wdmsg(J).



call_cg(X):- pred_cg(call_cg_real,X).
call_cg_real(X):- frmprint(call_cg(X)),!.
call_cg_real(X):- is_list(X),list_to_conjuncts(X,J),!,wdmsg(J).


locally_setval(A,B,C,S,E):- locally_setval(A,B,phrase(C,S,E)).


pred_cg(Pred, Error):- var(Error),!, trace_or_throw(pred_cg(Pred, Error)).
pred_cg(Pred, X):- string(X),!,pred_cg(Pred, cg_text(X)).
pred_cg(Pred, [Int|Codes]):- notrace(catch(text_to_string([Int|Codes],X),_,fail)),pred_cg(Pred, cg_text(X)).
pred_cg(Pred, X):- is_list(X), !, maplist(pred_cg(Pred),X).
pred_cg(Pred, cg_text(A)):- any_to_string(A,X),
 locally_setval(cg_text,X,(( 
  format('~N~n~n```~n% ===========================================~n% ?- pred_cg(~q,"~w").~n% ===========================================~n~n',[Pred,X]),
  ignore((cg_df_to_term(X,Y),ignore((nb_current_no_nil(named_graph,Id), ain(cg_text(Id,X)))), !, pred_cg(Pred, Y))), 
  format("~N```~n",[])))),!.


pred_cg(Pred, tOkS(Toks)):- !, (parse_ncg(CG,Toks,[])-> pred_cg(Pred, cg(CG)) ; (format("
% Failed Parse
?- rtrace( 
    ~q  
   ). ~n",[pred_cg(Pred, tOkS(Toks))]))).

pred_cg(Pred, cg(CG)):- nop(wdmsg(call(Pred,CG))), !, call(Pred,CG).
pred_cg(Pred, X):- wdmsg(pred_cg(Pred, X)), fail.
%pred_cg(Pred, X):- term_to_cg(X, Y),!, pred_cg(Pred, Y),!.
pred_cg(_, _):- !.
pred_cg(Pred, Error):- trace_or_throw(pred_cg(Pred, Error)).

print_cg(X):- is_list(X),!, maplist(print_cg,X).
print_cg(X):- !, frmprint(X).
print_cg(X):- nl,wdmsg(display(X)),nl.

fixcase_atom(Name,NameR):- atom(Name), upcase_atom(Name,Name), \+ downcase_atom(Name,Name), to_case_break_atoms(Name,Atoms),
 maplist(to_titlecase,Atoms,PAtoms),sUbst(PAtoms,'-','_',PSAtoms),atomic_list_concat(PSAtoms,NameR),!.
fixcase_atom(Name,Name).

:- use_module(library(dcg/basics)).

end_symbol--> `-`, !, end_symbol.
end_symbol-->  [C],!, { \+code_type(C, prolog_identifier_continue) }.
end_symbol--> \+ [_].
prolog_id_conted([])--> dcg_peek(end_symbol),!.
prolog_id_conted([C|T])--> [C], !,prolog_id_conted(T).

tokenize_cg_w(HT)--> maybe_to_codes,!,tokenize_cg_w(HT).
tokenize_cg_w(HT)--> blank,!,tokenize_cg_w(HT).
tokenize_cg_w(Name) --> dcg_peek(`'`),!,single_quoted_string(Str),{atom_codes(Name,Str)}.
tokenize_cg_w(String) --> dcg_peek(`"`),!,double_quoted_string(String).
tokenize_cg_w(Op)--> {sent_op_chars(Op,Chars)},Chars,!.
tokenize_cg_w('?'(UNAME)) --> `?`,!,prolog_id_conted(CL),{ atom_codes(Name, CL)},!,{upcase_atom(Name,UNAME)}.
tokenize_cg_w(T)--> dcg_basics:number(T),!.
tokenize_cg_w(Name)--> [C], {\+code_type(C, prolog_identifier_continue),atom_codes(Name, [C])},!.
tokenize_cg_w(Name)--> prolog_id_conted(CL), !,{atom_codes(NameR, CL),fixcase_atom(NameR,Name)},!.
tokenize_cg_w(Name)--> [C],{ atom_codes(Name, [C])},!.

tokenize_cg(HT)--> maybe_to_codes,!,tokenize_cg(HT).
tokenize_cg(HT)--> dcg_basics:blank,!,tokenize_cg(HT).
tokenize_cg([],S,E):- S==[],!,E=[].
tokenize_cg([H|T])--> tokenize_cg_w(H),!,tokenize_cg(T).
tokenize_cg([])-->[],!.                                             



%find_var(V)--> cic('*'), cw(VL),cic(']'),!,{upcase_atom(VL,V)},!.
%parse_var_concept(V,typeof(Rel, ?(V)))-->  cic('['), cw(Rel), cic(':'),cic('*'), cw(VL),cic(']'),!,{upcase_atom(VL,V)},!.
%parse_var_concept(V,C)-->  cic('['),dcg_beforeSeq(LeftSkipped,find_var(V)), {append(['['|LeftSkipped],[']'],CS), concept(C,CS,[])},!.

parse_rel(Logic) -->  cic('('),carg(Rel),
  dcg_list_of(carg,Args), cic(')'),!,{fixcase_atom(Rel,RelD),Logic=..[cg_holds,RelD|Args]},!.


parse_decl(Type, [cg_type(Arg,RelD),cg_decl(Type,Arg)],Arg) -->  cw(Rel), cic('('), carg(Arg), cic(')'),!,{fixcase_atom(Rel,RelD)},!.



carg(W)-->dcg_peek(cic('[')),concept(W),!.
carg(W)-->dcg_peek(cic('(')),parse_rel(W),!.
%carg(_)-->[CI],{sent_op(CI)},!,{fail}.
carg(W) --> cw(W),!.
carg(V)-->cvalue(W),{concept_var(V,W)}.


dcg_list_of( _,['*']) --> cic('*'), ending(_),!.
dcg_list_of(_Cw,[]) --> ending(_),!.
dcg_list_of( Cw,[H|Frame]) --> cic('|'), !, {append_term(Cw,H,CwH)}, CwH, dcg_list_of(Cw,Frame).
dcg_list_of( Cw,[H|Frame]) --> cic(','), !, {append_term(Cw,H,CwH)}, CwH, dcg_list_of(Cw,Frame).
dcg_list_of( Cw,[H|Frame]) --> {append_term(Cw,H,CwH)}, CwH, dcg_list_of(Cw,Frame).

ending(R) --> {sent_op_pair(_,R)},dcg_peek(cic(R)),!.
ending(_) --> \+[_].


push_incr(State,Type,Amount):- (get_attr(State,Type,Prev);Prev=0),New is Amount+ Prev,!, put_attr(State,Type,New).
get_incrs(State,Type,Amount):- sent_op_pair(Type,_), get_attr(State,Type,Amount).
unbalanced_incr(State):- sent_op_pair_q(S),get_incrs(State,S,V),V>0.
can_incr(State,_S):- \+ unbalanced_incr(State), !. % sent_op_pair(S,S), O\==S, get_incrs(State,S,V),V>0.

ballance([],_):-!.
ballance([H|T],State):- sent_op_pair_q(S),H=S,!,
 ((get_incrs(State,S,V),V>0) -> push_incr(State,S,-1);push_incr(State,S,+1)),
 ballance(T,State).
ballance([H|T],State):- sent_op_pair(S,_), H=S, can_incr(State,S), !, push_incr(State,S,+1),ballance(T,State).
ballance([H|T],State):- sent_op_pair(S,E), H=E, can_incr(State,S), !, push_incr(State,S,-1),ballance(T,State).
ballance([_|T],State):- ballance(T,State).

ballanced(L):- notrace((ballance(L,R),\+ ( get_incrs(R,_,V),V\=0))).

dcg_beforeSeq(Skipped,Mid,S,E):-
  append(Skipped,MidS,S),ballanced(Skipped), phrase(Mid,MidS,E).

maybe_codes_to_tokens(_,_):- !, fail.
maybe_codes_to_tokens(S,_):- is_list(S), notrace(catch((atom_codes(_,S),fail),_,true)),!,fail.
maybe_codes_to_tokens(S,Toks):- \+ is_list(S),!,any_to_atom(S,Atom),atom_codes(Atom,Codes), maybe_codes_to_tokens(Codes,Toks).
maybe_codes_to_tokens(S,Toks):- tokenize_cg(Toks,S,[]),!.


named_graph_starter(Name)--> cw(Name), (cic('::');(cic(':'),cic(':'))).


cg_clause_connective--> (cic('.');cic(',');cic(':-');cic(';')).

maybe_to_codes(S,_):- is_list(S), notrace(catch(atom_codes(_,S),_,fail)),!,fail.
maybe_to_codes(S,Codes):- any_to_string(S,Str),atom_codes(Str,Codes),!.

% parse_ncg(CG) --> maybe_codes_to_tokens,!,parse_ncg(CG).
% parse_ncg(named_graph(Name,PCG)) --> cw(Name), (cic('::');(cic(':'),cic(':'))),parse_ncg(PCG).
parse_ncg(CG)--> {var(CG)},!, parse_cg(['.'],CG0), {resolve_frame_constants(CG0,CG)},!,dcgOptional(cic('.')).
parse_ncg(CG)--> parse_ncg(CG0),!,{push_frame_info(CG0,CG)}.

parse_cg(StopAt,CG) --> parse_cg0(StopAt,CG).

parse_cg0(StopAt,CG,S,E) :- var(CG), \+ attvar(CG), !,cg_new(CG), must( \+ ((var(CG), \+ attvar(CG)))),!,locally_setval(cgframe,CG,parse_cg0(StopAt,CG,S,E)).


parse_cg0(_StopAt,CG)--> [':-'], !, parse_cg_list([','],PCG), { merge_simular_graph_vars(CG,PCG), 
  push_frame_info(preconds(PCG),CG)}.
parse_cg0(StopAt,_) --> ends_cg(StopAt),!.

parse_cg0(_StopAt,CG) --> named_graph_starter(Name),
  {once(nb_current_no_nil(named_graph,WasName);WasName=[])},
   locally_setval(named_graph,Name,parse_cg0((cg_clause_connective),PCG)), 
  {WasName==Name-> push_frame_info(PCG,CG);push_frame_info(named_graph(Name,PCG),CG)}.

parse_cg0(_StopAt,CG)--> {member(Type,[type,individual])}, cic(Type), !, zalwayz(parse_decl(Type,Expr,_X)), !, zalwayz(cic('Is')),!, 
 zalwayz(parse_ncg(CG0)),!, {push_frame_info(CG0,CG),push_frame_info(Expr,CG)},!.
parse_cg0(StopAt,CG) --> parse_rel(H), {push_frame_info([H],CG)}, parse_cg0(StopAt,CG).
parse_cg0(StopAt,CG) --> concept(S), cont_graph(StopAt,S,S,CG),!,parse_cg0(StopAt,CG).

ends_cg(   _   ) --> \+ [_],!.
ends_cg(StopAt ) --> StopAt,!.
ends_cg(   _   ) --> {sent_op_pair(_,R)},dcg_peek(cic(R)),!.
ends_cg(   _   ) --> cic('.'),!.


parse_cg_list(StopAt,[] ) --> ends_cg(StopAt),!.
parse_cg_list(StopAt,[CG|More])--> parse_cg0(StopAt,CG),parse_cg_list(StopAt,More).

s_e(S,E,S,E).

cont_graph(_StopAt,_Parent,_Subj,CG) --> [':-'], !, parse_cg_list([','],PCG), { merge_simular_graph_vars(CG,PCG), 
  push_frame_info(preconds(PCG),CG)}.

cont_graph(StopAt,_,_,_) --> ends_cg(StopAt),!.
cont_graph(StopAt,Parent,_Subj,CG) --> cic(','),!,cont_graph(StopAt,Parent,Parent,CG).
cont_graph(StopAt,Parent,_Subj,CG) --> cic('-'),dcg_peek(cic('-');cic('<-');cic('->')),!,cont_graph((dcg_peek(\+ cic(',')), StopAt),Parent,Parent,CG).

cont_graph(StopAt,Parent,Subj,CG) --> rel_right2(Rel),!,concept(Obj),push_frame_info(cg_holds(Rel,Subj,Obj),CG),cont_graph(StopAt,Parent,Obj,CG).
cont_graph(StopAt,Parent,Subj,CG) -->  rel_right(Rel),!,concept(Obj),push_frame_info(cg_holds(Rel,Subj,Obj),CG),cont_graph(StopAt,Parent,Obj,CG).

cont_graph(StopAt,Parent,Obj,CG) --> rel_left2(Rel),!,concept(Subj),push_frame_info(cg_holds(Rel,Subj,Obj),CG),cont_graph(StopAt,Parent,Subj,CG).
cont_graph(StopAt,Parent,Obj,CG) -->  rel_left(Rel),!,concept(Subj),push_frame_info(cg_holds(Rel,Subj,Obj),CG),cont_graph(StopAt,Parent,Subj,CG).

cont_graph(StopAt,_Parent,_Subj,CG) --> parse_cg(StopAt,CG).

rel_right(Rel)-->cic('-'),rel(Rel),cic('->').
rel_right2(Rel)-->cic('->'),rel(Rel),cic('->').
rel_right2(Rel)-->cic('->'),rel(Rel),cic('-').
rel_left(Rel)-->cic('<-'),rel(Rel),cic('-').
rel_left2(Rel)-->cic('<-'),rel(Rel),cic('<-').

rel(C)--> ['('],word_tok_loose(C),[')'].
rel(C)--> ['<'],word_tok_loose(C),['>'].
rel(C)--> word_tok_loose(C).

word_tok_loose(DC)-->[C],{atom(C),C\=='', fixcase_atom(C,DC)}.

nonword_tok(X):- atom(X),upcase_atom(X,UC),downcase_atom(X,DC),!,UC==DC. 

word_tok('#'(X))--> ['#'],word_tok(X),!.
word_tok('#')--> ['#'], !.
word_tok('*'(X))--> ['*'],word_tok(X),!.
word_tok('*')--> ['*'], !.
word_tok(?(Var)) --> [?(Var)],!.
word_tok(Value) --> [Value],{number(Value)},!.
word_tok(*)--> [*], !.
word_tok(?(Var)) --> [?(Var)],!.
word_tok(X)--> [X], !, {atom(X), \+ nonword_tok(X)}.

quant(X) --> [X], {nonword_tok(X)}.

                                                  
concept(CG) --> maybe_codes_to_tokens,!,concept(CG).
concept(V,S,E):- concept0(C,S,E),concept_var(V,C).

% concept0('*')--> [*], !.
concept0([?(Var)]) --> [?(Var)],!.
% concept(vc(V, C))-->parse_var_concept(V,C),!.

concept0(C)-->concept2(C),(([I],{integer(I)})->{nb_set_add(C,'#'(I))};[]).

%concept1(C)--> cic('['), dcg_peek([P1,P2]), concept_innerds_3a(P1,P2,C),!.
concept2(C)--> cic('['), concept_innerds(C), cic(']'),!.
concept2(C)--> word_tok(C),!.

concept_var(V,C):- \+ is_list(C),concept_var(V,[C]).
concept_var(V,C):- maplist(concept_var(V,C),C).

% val(X_Cat, [?('X')])
concept_var(V, _Props,      Var  ):- var(Var),!, V=Var.
concept_var(_, _Props,       []  ):- !.
concept_var(V,  Props,    '#'(N) ):-  atomic(N), member(cg_type(Type),Props),!,atomic_list_concat([Type,'#',N],Val),!,
  concept_var(V,  Props,   cg_equal(Val)).
concept_var(V, _Props,    '@'(E) ):- !,debug_fvar(E,V), push_frame_info(cg_quantz(E,V),cgframe).
%concept_var(V, Props, '$VAR'(N),'$VAR'(N) ):- !.
concept_var(V, Props, '$VAR'(N) ):-  !,concept_var(V, Props, '?'(N) ).
concept_var(V, Props, 'cg_name'(N) ):-  atom(N), downcase_atom(N,N),!,concept_var(V, Props, '*'(N) ).
concept_var(V, _Props,    '?'(N) ):- !, upcase_atom(N,U),debug_fvar(N,V),!,push_frame_info(cg_quantz(e,V),cgframe),!,V='$VAR'(U).
concept_var(V, _Props,    '*'(N) ):- !,debug_fvar(N,V),upcase_atom(N,NU),push_frame_info(frame_var(NU,V),cgframe),!.

concept_var(V, _Props,        M  ):- atomic(M),!,V=M.

%concept_var(V, _Props,   = ):- atomic_list_concat([Type,'_'],M),gensym(M,V),
%  push_frame_info(cg_type(V,Type),cgframe),push_frame_info(same_values(V,Value),cgframe).

concept_var(V, _Props,    IZA ):- compound(IZA),compound_name_arguments(IZA,I,[Z|A]),compound_name_arguments(IZAOUT,I,[V,Z|A]),!,
  push_frame_info(IZAOUT,cgframe),debug_fvar(Z,V).
concept_var(_, _Props, _):-!.

 
cvalue_cont([])--> (cic('.'); ending(_)),!.
cvalue_cont([]) --> {sent_op(Op)},dcg_peek(cic(Op)),!.
cvalue_cont([H|T]) --> cw(H), cvalue_cont(T).


cvalue('GRAPH'(W))--> dcg_peek(cic('[')), zalwayz(parse_ncg(W)).
cvalue(=(REL))--> dcg_peek(cic('(')), zalwayz(parse_rel(REL)),!.
cvalue(=(X,REL))--> dcg_peek((cw(_),cic('('))), parse_decl(instance,REL,X),!.
cvalue(textOf(String))--> [String], {string(String)},!.
cvalue('='(W))--> cic('='), cvalue(W).
cvalue('^'(W))--> cic('^'), cvalue(W).
cvalue('~'(W))--> cic('~'), cvalue(W).
cvalue('*'(W))--> cic('*'), cw(W).
cvalue('?'(W))--> [?(W)],!.
% #?Quotient
cvalue('='(W))--> cic('#'), [?(W)], {atomic(W)}.
cvalue('#'(W))--> cic('#'), [W].
% cvalue(cg_count(W,W))--> cic('@'), [W], { number(W) },!.
cvalue('@'(W))--> cic('@'), cw(W).
cvalue(cg_count(0,0))--> cic('{'),  cic('}'),!.
cvalue(TstAtts)--> cic('{'), {TstAtts=['@'(set)]}, dcg_list_of(cic,Atts), cic('}'), !, {into_set(Atts,TstAtts)}.
cvalue(cg_name(W))--> cw(H), cvalue_cont(T), {maplist(term_to_unquoted_atom,[H|T],HT),atomic_list_concat(HT,'_',W)}.

into_set(['*'],TstAtts):- nb_set_add(TstAtts,cg_count(1,_)),!.
into_set(Atts,TstAtts):- append(List,['*'],Atts),length(List,Count), nb_set_add(TstAtts,cg_count(Count,_)),nb_set_add(TstAtts,cg_values(List)).
into_set(List,TstAtts):- length(List,Count), nb_set_add(TstAtts,cg_count(Count,_)),nb_set_add(TstAtts,cg_values(List)).


term_to_unquoted_atom(Atom,Atom):- atom(Atom),!.
term_to_unquoted_atom(Term,Atom):- term_to_atom(Term,Atom).

concept_innerds([cg_type(C)|Cont]) --> cw(C),cic(','),!, concept_innerds(Cont).
concept_innerds([cg_type(C)|Cont]) --> cw(C), dcgOptional(cic(':')),!,concept_innerds_cont(Cont).
concept_innerds(Cont) --> concept_innerds_cont(Cont).

concept_innerds_cont([])--> ending(_).
concept_innerds_cont(HT) --> cvalue(H), !, concept_innerds_cont(T),{flatten([H,T],HT)}.


sent_op_chars(Op,Chars):- sent_op(Op),atom_codes(Op,Chars).

% these must be before:
sent_op('::'). sent_op(':-'). sent_op('->'). sent_op('<-').
% these
sent_op('-').  sent_op(':').  
% then..
sent_op(A):- sent_op_pair(A,_).
sent_op(A):- sent_op_pair(_,A).
sent_op(A):- sent_op_pair_q(A).
sent_op(',').  sent_op(';'). 
sent_op('|').
sent_op('.').  sent_op('='). sent_op('@').  sent_op('#').
sent_op('^').  sent_op('*'). sent_op('~').  sent_op('$').

sent_op --> [C],{sent_op(C)}.

sent_op_pair('<','>').
sent_op_pair('{','}'). 
sent_op_pair('[',']').
sent_op_pair('(',')'). 

sent_op_pair_q('"'). 
sent_op_pair_q('\''). 


cw(H,[H|T],T):- notrace(( \+ sent_op(H))).

cic(CI)-->[C],{notrace((atom(C),fixcase_atom(C,UC),(CI=UC->true; fixcase_atom(CI,UC))))}.


cg_df_to_term(In,Out):- any_to_string(In,Str),
  % replace_in_string(['('='{',')'='}'],Str,Str0),
  replace_in_string(['\r'='\n'],Str,Str0),
  atom_codes(Str0,Codes),!,
  must_or_rtrace(tokenize_cg(Toks,Codes,[])),!,
  Out = tOkS(Toks).


:- set_dcg_meta_reader_options(file_comment_dialect=cg_lf, cg_comment_expr).
cg_comment_expr(X) --> cspace,!,cg_comment_expr(X).
cg_comment_expr('$COMMENT'(Expr,I,CP)) --> comment_expr_5(Expr,I,CP),!.
comment_expr_5(T,N,CharPOS) --> `/*`, !, my_lazy_list_location(file(_,_,N,CharPOS)),!, zalwayz(read_string_until_no_esc(S,`*/`)),!,
  {text_to_string_safe(S,T)},!.
comment_expr_5(T,N,CharPOS) -->  {cg_cmt_until_eoln(Text)},Text,!, my_lazy_list_location(file(_,_,N,CharPOS)),!,zalwayz(read_string_until_no_esc(S,eoln)),!,
 {text_to_string_safe(S,T)},!.
cg_cmt_until_eoln(`//`).
cg_cmt_until_eoln(`;;`).
cg_cmt_until_eoln(`%`).


  
%:- pop_operators.

%:- ensure_loaded(library(cgprolog_inline_reader)).
      
:- fixup_exports.

