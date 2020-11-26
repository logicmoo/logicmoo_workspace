

:- use_module(library(logicmoo/dcg_meta)).
:- use_module(library(logicmoo/util_bb_frame)).

:- ensure_loaded(library(cgprolog_swipl)).

:- multifile_data(cg_test_data/2). 
%:- multifile_data(skip_cg_test_data/2). 
:- multifile_data(cg/2).

%:- current_op(X,Y,'->'),push_operators([op(X,Y,'<-')]).

debug_fvar(N,_):- (\+ ground(N) ; number(N)),!.

debug_fvar(N,V):- debug_var(N,V).

cg_new(CG):- must(push_frame_info([], CG)),!.
push_frame_info(Info,Frame):- atom(Frame),must(nb_current(Frame,CG)),!,push_frame_info(Info,CG).
push_frame_info(Info,Frame):- must(push_frame(Info,Frame)).
push_frame_info(Info,Frame,S,S):-push_frame_info(Info,Frame).
push_frame_concept(C,Frame):-atom(Frame),must(nb_current(Frame,CG)),!,push_frame_concept(C,CG).
push_frame_concept(C,Frame):-nop(push_frame(C,Frame)).
push_frame_concept(C,Frame,S,S):-push_frame_concept(C,Frame).
nb_set_add(X,Y,S,S):- must(nb_set_add(X,Y)).


cg_demo :- make, forall((cg_test_data(TstAtts,X), \+ memberchk(failing,TstAtts)),must(do_cg_test(TstAtts,X))).
cg_reader_tests :- make, forall(cg_test_data(TstAtts,X), must(do_cg_test(TstAtts,X))).

do_cg_test( TstAtts,_):- memberchk(skip,TstAtts),!.
do_cg_test( TstAtts,X):- select(cg_dialect(What),TstAtts,NewTstAtts),!, 
  locally_setval(cg_dialect,What,do_cg_test( NewTstAtts,X)).
do_cg_test( TstAtts,X):- memberchk(xcall,TstAtts),!, must(ignore(call_cg(xtext(X)))).
do_cg_test(_TstAtts,X):- must(ignore(assert_cg(xtext(X)))).

subset_loose(S1,S2):- \+ is_list(S2),!,member(S2,S1),!.
subset_loose(S1,S2):- subset(S1,S2).

cg_test_data(TstAtts,X):- nonvar(TstAtts), var(X), !,
  forall((cg_test_data(WasTstAtts,X),subset_loose(WasTstAtts,TstAtts)),
     cg_test_data( WasTstAtts,X)).
cg_test_data(TstAtts,X):- nonvar(TstAtts), nonvar(X), !, 
   exclude(=(skip),TstAtts,NTstAtts),
   do_cg_test(NTstAtts,X).


assert_cg(X):- newId(Id),!,locally(nb_setval(cgid,Id), pred_cg(assert_cg_real,X)),!.
assert_cg_real(X):- is_list(X),list_to_conjuncts(X,J),!,wdmsg(J).
assert_cg_real(X):- !,  frmprint(X).
assert_cg_real(X):- nb_current(cgid,Id), print_cg(Id:X),  ain(cg(Id,X)).

call_cg(X):- pred_cg(call_cg_real,X).
call_cg_real(X):- print_cg(X),call(cg(_,X)).


pred_cg(Pred, Error):- var(Error),!, trace_or_throw(pred_cg(Pred, Error)).
pred_cg(Pred, X):- string(X),!,pred_cg(Pred, xtext(X)).
pred_cg(Pred, [Int|Codes]):- notrace(catch(text_to_string([Int|Codes],X),_,fail)),pred_cg(Pred, xtext(X)).
pred_cg(Pred, X):- is_list(X), !, maplist(pred_cg(Pred),X).
pred_cg(Pred, xtext(X)):- locally_setval(cg_text,X,(( 
  format('~N~n~n```~n% ===========================================~n?- pred_cg(~q,"~w").~n% ===========================================~n~n',[Pred,X]),
  cg_df_to_term(X,Y), !, pred_cg(Pred, Y), format("~N```~n",[])))),!.


pred_cg(Pred, tOkS(Toks)):- !, (parse_cg(CG,Toks,[])-> pred_cg(Pred, cg(CG)) ; (format("
% Failed Parse
?- rtrace( 
    ~q  
   ). ~n",[pred_cg(Pred, tOkS(Toks))]))).

pred_cg(Pred, cg(CG)):- nop(wdmsg(call(Pred,CG))), !, call(Pred,CG).
pred_cg(Pred, X):- wdmsg(pred_cg(Pred, X)), fail.
%pred_cg(Pred, X):- term_to_cg(X, Y),!, pred_cg(Pred, Y),!.
pred_cg(_, _):- !.
pred_cg(Pred, Error):- trace_or_throw(pred_cg(Pred, Error)).

print_cg(X):- !, frmprint(X).
print_cg(X):- is_list(X),!, maplist(print_cg,X).
print_cg(X):- nl,wdmsg(display(X)),nl.

fixcase_atom(Name,NameR):- atom(Name), upcase_atom(Name,Name),to_case_break_atoms(Name,Atoms),
 maplist(to_titlecase,Atoms,PAtoms),sUbst(PAtoms,'-','_',PSAtoms),atomic_list_concat(PSAtoms,NameR),!.
fixcase_atom(Name,Name).

:- use_module(library(dcg/basics)).

end_symbol--> `-`, !, end_symbol.
end_symbol-->  [C],!, { \+code_type(C, prolog_identifier_continue) }.
end_symbol--> \+ [_].
prolog_id_conted([])--> dcg_peek(end_symbol),!.
prolog_id_conted([C|T])--> [C], !,prolog_id_conted(T).

tokenize_cg_w(HT)--> blank,!,tokenize_cg_w(HT).

tokenize_cg_w(Name) --> dcg_peek(`'`),!,single_quoted_string(Str),{atom_codes(Name,Str)}.
tokenize_cg_w(String) --> dcg_peek(`"`),!,double_quoted_string(String).
tokenize_cg_w(Op)--> {sent_op_chars(Op,Chars)},Chars,!.
tokenize_cg_w('?'(UNAME)) --> `?`,!,prolog_id_conted(CL),{ atom_codes(Name, CL)},!,{upcase_atom(Name,UNAME)}.
tokenize_cg_w(T)--> dcg_basics:number(T),!.
tokenize_cg_w(Name)--> prolog_id_conted(CL), !,{atom_codes(NameR, CL),fixcase_atom(NameR,Name)},!.
tokenize_cg_w(Name)--> [C],{ atom_codes(Name, [C])},!.

tokenize_cg(HT)--> blank,!,tokenize_cg(HT).
tokenize_cg([],S,E):- S=[],!,E=[].
tokenize_cg([H|T])--> tokenize_cg_w(H),!,tokenize_cg(T).
tokenize_cg([])-->[],!.                                             



%find_var(V)--> ci('*'), cw(VL),ci(']'),!,{upcase_atom(VL,V)},!.
%parse_var_concept(V,typeof(Rel, ?(V)))-->  ci('['), cw(Rel), ci(':'),ci('*'), cw(VL),ci(']'),!,{upcase_atom(VL,V)},!.
%parse_var_concept(V,C)-->  ci('['),dcg_beforeSeq(LeftSkipped,find_var(V)), {append(['['|LeftSkipped],[']'],CS), concept(C,CS,[])},!.

parse_rel(Logic) -->  ci('('),carg(Rel),
  dcg_list_of(carg,Args), ci(')'),!,{fixcase_atom(Rel,RelD),Logic=..[cg_holds,RelD|Args]},!.


parse_decl(Type, [cg_type(Arg,RelD),cg_quantz(Type,Arg)],Arg) -->  cw(Rel), ci('('), carg(Arg), ci(')'),!,{fixcase_atom(Rel,RelD)},!.



carg(W)-->dcg_peek(ci('[')),concept(W),!.
carg(W)-->dcg_peek(ci('(')),parse_rel(W),!.
%carg(_)-->[CI],{sent_op(CI)},!,{fail}.
carg(W) --> cw(W),!.
carg(V)-->cvalue(W),{concept_var(V,W)}.


dcg_list_of( _,['*']) --> ci('*'), ending(_),!.
dcg_list_of( Cw,[H|Frame]) --> ci('|'), !, {append_term(Cw,H,CwH)}, CwH, dcg_list_of(Cw,Frame).
dcg_list_of( Cw,[H|Frame]) --> ci(','), !, {append_term(Cw,H,CwH)}, CwH, dcg_list_of(Cw,Frame).
dcg_list_of( Cw,[H|Frame]) --> {append_term(Cw,H,CwH)}, CwH, dcg_list_of(Cw,Frame).
dcg_list_of(_Cw,[]) --> ending(_).

ending(R) --> {sent_op_pair(_,R)},dcg_peek(ci(R)),!.
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

codes_to_tokens(S,Toks):- length(S,Len), Len> 2, notrace(catch(atom_codes(_,S),_,fail)), tokenize_cg(Toks,S,[]),!.


parse_cg(CG) --> codes_to_tokens,!,parse_cg(CG).
parse_cg(CG)--> parse_cg0(CG0), {must(var(CG)),resolve_frame_constants(CG0,CG)},!,dcgOptional(ci('.')).


parse_cg0(CG,S,E) :- var(CG), \+ attvar(CG),cg_new(CG),!,locally_setval(cgframe,CG,parse_cg0(CG,S,E)).
parse_cg0(CG) --> cw(Name), (ci('::');(ci(':'),ci(':'))),parse_cg(CG), push_frame_info([lbl=Name],CG).

parse_cg0(CG)--> [':-'], !,parse_cg(PCG), { merge_simular_graph_vars(CG,PCG), push_frame_info(preconds(PCG),CG)}.

parse_cg0(CG)--> {member(Type,[type,individual])}, ci(Type), !, zalwayz(parse_decl(Type,Expr,_X)), !, zalwayz(ci('Is')),!, 
 zalwayz(parse_cg(CG0)),!, {push_frame_info(CG0,CG),push_frame_info(Expr,CG)},!.

parse_cg0(CG) --> parse_rel(H), {push_frame_info([H],CG)}, parse_cg0(CG).
parse_cg0(CG) --> concept(S), cont_graph(S,S,CG),!,parse_cg0(CG).
parse_cg0(_) --> {sent_op_pair(_,R)},dcg_peek(ci(R)),!.
parse_cg0(_) --> ci('.'),!.
parse_cg0(_) --> \+ [_],!.

s_e(S,E,S,E).

cont_graph(Parent,Subj,CG) --> rel_right2(Rel),!,concept(Obj),push_frame_info(cg_holds(Rel,Subj,Obj),CG),cont_graph(Parent,Obj,CG).
cont_graph(Parent,Subj,CG) --> rel_right(Rel),!,concept(Obj),push_frame_info(cg_holds(Rel,Subj,Obj),CG),cont_graph(Parent,Obj,CG).
cont_graph(Parent,Obj,CG) --> rel_left2(Rel),!,concept(Subj),push_frame_info(cg_holds(Rel,Subj,Obj),CG),cont_graph(Parent,Subj,CG).
cont_graph(Parent,Obj,CG) --> rel_left(Rel),!,concept(Subj),push_frame_info(cg_holds(Rel,Subj,Obj),CG),cont_graph(Parent,Subj,CG).
cont_graph(Parent,_Subj,CG) --> ci(','),!,cont_graph(Parent,Parent,CG).
cont_graph(Parent,_Subj,CG) --> ci('-'),!,cont_graph(Parent,Parent,CG).
cont_graph(_Parent,_Subj,_) --> dcg_peek(ci(']')),!.
cont_graph(_Parent,_Subj,_) --> ci('.'),!.
cont_graph(_Parent,_,_)--> \+ [_],!.
cont_graph(_Parent,_Subj,CG) --> parse_cg0(CG).

rel_right(Rel)-->ci('-'),rel(Rel),ci('->').
rel_right2(Rel)-->ci('->'),rel(Rel),ci('->').
rel_right2(Rel)-->ci('->'),rel(Rel),ci('-').
rel_left(Rel)-->ci('<-'),rel(Rel),ci('-').
rel_left2(Rel)-->ci('<-'),rel(Rel),ci('<-').

rel(C)--> ['('],word_tok_loose(C),[')'].
rel(C)--> ['<'],word_tok_loose(C),['>'].
rel(C)--> word_tok_loose(C).

word_tok_loose(DC)-->[C],{atom(C),fixcase_atom(C,DC)}.

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

                                                  
concept(CG) --> codes_to_tokens,!,concept(CG).
concept(V,S,E):- concept0(C,S,E),concept_var(V,C).

% concept0('*')--> [*], !.
concept0([?(Var)]) --> [?(Var)],!.
% concept(vc(V, C))-->parse_var_concept(V,C),!.

concept0(C)-->concept2(C),(([I],{integer(I)})->{nb_set_add(C,'#'(I))};[]).

%concept1(C)--> ci('['), dcg_peek([P1,P2]), concept_innerds_3a(P1,P2,C),!.
concept2(C)--> ci('['), concept_innerds(C), ci(']'),!.
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

 
cvalue_cont([])--> (ci('.'); ending(_)),!.
cvalue_cont([]) --> {sent_op(Op)},dcg_peek(ci(Op)),!.
cvalue_cont([H|T]) --> cw(H), cvalue_cont(T).


cvalue('GRAPH'(W))--> dcg_peek(ci('[')), zalwayz(parse_cg(W)).
cvalue(=(REL))--> dcg_peek(ci('(')), zalwayz(parse_rel(REL)),!.
cvalue(=(X,REL))--> dcg_peek((cw(_),ci('('))), parse_decl(instance,REL,X),!.
cvalue(textOf(String))--> [String], {string(String)},!.
cvalue('='(W))--> ci('='), cvalue(W).
cvalue('^'(W))--> ci('^'), cvalue(W).
cvalue('~'(W))--> ci('~'), cvalue(W).
cvalue('*'(W))--> ci('*'), cw(W).
cvalue('?'(W))--> [?(W)],!.
% #?Quotient
cvalue('='(W))--> ci('#'), [?(W)], {atomic(W)}.
cvalue('#'(W))--> ci('#'), [W].
% cvalue(cg_count(W,W))--> ci('@'), [W], { number(W) },!.
cvalue('@'(W))--> ci('@'), cw(W).
cvalue(cg_count(0,0))--> ci('{'),  ci('}'),!.
cvalue(TstAtts)--> ci('{'), {TstAtts=['@'(set)]}, dcg_list_of(ci,Atts), ci('}'), !, {into_set(Atts,TstAtts)}.
cvalue(cg_name(W))--> cw(H), cvalue_cont(T), {maplist(term_to_unquoted_atom,[H|T],HT),atomic_list_concat(HT,'_',W)}.

into_set(['*'],TstAtts):- nb_set_add(TstAtts,cg_count(1,_)),!.
into_set(Atts,TstAtts):- append(List,['*'],Atts),length(List,Count), nb_set_add(TstAtts,cg_count(Count,_)),nb_set_add(TstAtts,cg_values(List)).
into_set(List,TstAtts):- length(List,Count), nb_set_add(TstAtts,cg_count(Count,_)),nb_set_add(TstAtts,cg_values(List)).


term_to_unquoted_atom(Atom,Atom):- atom(Atom),!.
term_to_unquoted_atom(Term,Atom):- term_to_atom(Term,Atom).

concept_innerds([cg_type(C)|Cont]) --> cw(C),ci(','),!, concept_innerds(Cont).
concept_innerds([cg_type(C)|Cont]) --> cw(C), dcgOptional(ci(':')),!,concept_innerds_cont(Cont).
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

ci(CI)-->[C],{notrace((atom(C),upcase_atom(C,UC),(CI=UC->true; upcase_atom(CI,UC))))}.


cg_df_to_term(In,Out):- any_to_string(In,Str),
  % replace_in_string(['('='{',')'='}'],Str,Str0),
  replace_in_string(['\r'='\n'],Str,Str0),
  atom_codes(Str0,Codes),
  must_or_rtrace(tokenize_cg(Toks,Codes,[])),
  Out = tOkS(Toks).


:- set_dcg_meta_reader_options(file_comment_dialect=cg_lf, cg_comment_expr).
cg_comment_expr(X) --> cspace,!,cg_comment_expr(X).
cg_comment_expr('$COMMENT'(Expr,I,CP)) --> comment_expr_5(Expr,I,CP),!.
comment_expr_5(T,N,CharPOS) --> `/*`, !, my_lazy_list_location(file(_,_,N,CharPOS)),!, zalwayz(read_string_until_no_esc(S,`*/`)),!,
  {text_to_string_safe(S,T)},!.
comment_expr_5(T,N,CharPOS) -->  {cmt_until_eoln(Text)},Text,!, my_lazy_list_location(file(_,_,N,CharPOS)),!,zalwayz(read_string_until_no_esc(S,eoln)),!,
 {text_to_string_safe(S,T)},!.
cmt_until_eoln(`//`).
cmt_until_eoln(`;;`).
cmt_until_eoln(`%`).


  
%:- pop_operators.

%:- ensure_loaded(library(cgprolog_inline_reader)).
      
:- fixup_exports.


cg_test_data([cg_dialect([df,plcg])], "
[PERSON: x] :- [CITIZEN : x].").


cg_test_data([cg_dialect([df,plcg])], "
[CITIZEN : x]<-memberOf-[COUNTRY : Oz] :- 
     [PERSON: x]<-AGNT-[Being_Born]-LOC->[COUNTRY : Oz].").


cg_test_data([cg_dialect([df,plcg])], "
[CITIZEN : x]<-memberOf-[COUNTRY : Oz] :- 
     [PERSON: ?x]<-childOf-[PERSON: y], 
     [CITIZEN : y]<-memberOf-[COUNTRY : Oz].").


cg_test_data([cg_dialect([df,plcg])], "
[CITIZEN : x]<-memberOf-[COUNTRY : Oz] :- 
     [PERSON : x]<-RCPT-[NATURALIZE]-LOC->[COUNTRY : Oz].").


cg_test_data([cg_dialect([df,plcg])], "
[PERSON : Tinman]-
	      -childOf->[GIRL : Dorothy],
	      <-AGNT-[Being_Born]-LOC->[COUNTRY : Oz].").

% end_of_file.



cg_test_data([cg_dialect([lf,sowa])],"[Mat]1-(Attrib)->[Color #1]").
cg_test_data([cg_dialect([lf,sowa])],"[Mat]1-(Attrib)->[Color]2").

cg_test_data([cg_dialect([df])],"[CAT_QUANT:@every]-(On)->[Mat]").
cg_test_data([cg_dialect([df])],"[A_CAT]->(On)->[Mat]").
cg_test_data([cg_dialect([df])],"[THE_CAT:#666]->(On)->[Mat]").
cg_test_data([cg_dialect([df])],"[NAMED_CAT:Moris]->(On)->[Mat]").
cg_test_data([cg_dialect([df])],"[LENGTH:@5ft]<-(SizeOf)-[Mat]").
cg_test_data([cg_dialect([df])],"[LENGTH:@5ft.]<-(SizeOf)-[Mat]").
cg_test_data([cg_dialect([df])],"[CAT_SET_NONE:{}]-(On)->[Mat]").
cg_test_data([cg_dialect([df])],"[CATS_ONE_OR_MORE:{*}]-(On)->[Mat]").
cg_test_data([cg_dialect([df])],"[CAT_FIVE:{*}@5]-(On)->[Mat]").
clacg_test_data([cg_dialect([df])],"[CAT_M:{Moris}]-(On)->[Mat]").
cg_test_data([cg_dialect([df])],"[CAT_FM:{Felix,Moris}]-(On)->[Mat]").
cg_test_data([cg_dialect([df])],"[CAT_SET_MIN_TWO:{Felix,Moris,*}]-(On)->[Mat]").
cg_test_data([cg_dialect([df])],"[CAT_SET_FIVE:{Felix,Moris,*}@5]-(On)->[Mat]").

cg_test_data([cg_dialect([df]), group(1)], "['Man':imad]<-agnt-['Drive']-obj->['Car']").
cg_test_data([cg_dialect([df]), group(1)], "[Cat#1]-(On)->[Mat #1]-(Attrib)->[Color #1]").
cg_test_data([cg_dialect([df]), group(1)], "[Cat: ?x]-(Attrib)->[C1]->(On)->[Mat]").
cg_test_data([cg_dialect([df]), group(1)], "[Cat: ?x]-(On)->[Mat]").
cg_test_data([cg_dialect([df]), group(1)], "[Cat: ?x]-(On)->[*MatC]").
cg_test_data([cg_dialect([df]), group(1)], "[Cat: ?x]-(On)->[Mat: *MatC]").
cg_test_data([cg_dialect([df]), group(1)], "[Man:karim]<-agnt-[Drink]-obj->[Water]").
cg_test_data([cg_dialect([df]), group(1)], "[Mat #1]<- (on)- [Cat #1]").
cg_test_data([cg_dialect([df]), group(1)], "[Mat]<-(On)-[Cat: ?x]").
cg_test_data([cg_dialect([df]), group(1)], "[Color #1]<-(Attrib)-[Mat #1]").
cg_test_data([cg_dialect([df]), group(2)], "[Cat #1]-(On)->[Mat #1]-(Attrib)->[Color #1]").
cg_test_data([cg_dialect([df]), group(2)], "[Man:karim]<-agnt-[Drink]-obj->[Water]").
cg_test_data([cg_dialect([df]), group(2)], "[Color #1] <- (Attrib) -[Mat #1]<- (on)- [Cat#1]").
cg_test_data([cg_dialect([df]), group(3)], "[Cat: @every]->(On)->[Mat]").
cg_test_data([cg_dialect([df]), group(3)], "[CAT]->(STAT)->[SIT]->(LOC)->[MAT].").
cg_test_data([cg_dialect([df]), group(3)], "[CAT]->(STAT)->[SIT]->(LOC)->[MAT]").

cg_test_data([cg_dialect([df]), group(3)], "
   [Drive *x] [Person: Bob] [City: \"St. Louis\"] [Chevy *y]
   (Agnt ?x Bob) (Dest ?x \"St. Louis\") (Thme ?x ?y) (Poss Bob ?y)").


cg_test_data([cg_dialect([df]), group(3)], "  
   [A_CAT] -> (KnowsAbout) ->
   [THE_CAT: #666]  -> (KnowsAbout) ->
   [NAMED_CAT: Moris]  -> (KnowsAbout) ->
   [LENGTH: @ 5ft]  -> (KnowsAbout) ->
   [CAT_SET:{*}]  -> (KnowsAbout) ->
   [CAT5:{*} @ 5 ]  -> (KnowsAbout) -> 
   [CATS_TWO:{Moris, Felix}]  -> (KnowsAbout) ->
   [CATS_ONE_OR_MORE:{Moris,*}]").


cg_test_data([skip,cg_dialect([df]), group(3)], "(IntegerDivide [Integer: *x] [Integer: 7] | [*u] [*v])").

cg_test_data([skip,cg_dialect([df]), group(3)], "
[Function: *Quotient] [Function: *Remainder]
[[@every*x1] [@every*x2] [@every*x3] [@every*x4]
[Equiv: [Iff: (IntegerDivide ?x1 ?x2 | ?x3 ?x4)]
        [Iff: (#?Quotient ?x1 ?x2 | ?x3) (#?Remainder ?x1 ?x2 | ?x4)]]]").

cg_test_data([cg_dialect([df]), group(3)], "[Relation: *r] (Familial ?r) (#?r Bob Sue)").

cg_test_data([skip,cg_dialect([df]), group(3)], "(exists ((r Relation)) (and (Familial r) (r Bob Sue)))").

cg_test_data([cg_dialect([df]), group(3)], "
[SIT]-
  <-(STAT)<-[CAT]
  ->(LOC)->[MAT],.").


cg_test_data([xcall,easy,cg_dialect([df])], "?x -(Attrib)-> [Color #1]").

cg_test_data([xcall,easy,cg_dialect([df])], "?x -(On)->[Mat #1]-(Attrib)->[Color #1]").
cg_test_data([xcall,easy,cg_dialect([df])], "?x -(On)->[Mat #1]").
cg_test_data([xcall,easy,cg_dialect([df])], "[?x] -(Attrib)-> [Color #1]").
cg_test_data([xcall,easy,cg_dialect([df])], "[?x]-(On)->[Mat #1]-(Attrib)->[Color #1]").
cg_test_data([xcall,easy,cg_dialect([df])], "[Mat ?x]-(Attrib)->[Color #1]").
cg_test_data([xcall, group(0)], "[Cat: ?x]-(On)->[Mat #1]-(Attrib)->[Color #2]").

cg_test_data([cg_dialect([df]), group(3)], "[a] - (belives) -> [statement: [Cat: @every]->(On)->[Mat] ]").
cg_test_data([cg_dialect([df]), group(3)], "[a] - (belives) -> [statement2= [Cat: @every]->(On)->[Mat] ]").

cg_test_data([cg_dialect([df]), group(4)], "

[Go]- -
   (Agnt)->[Person: John] -
   (Dest)->[City: Boston] -
   (Inst)->[Bus]").

cg_test_data([skip, cg_dialect([df]), group(4)], "
// ontology required (to load first): aminePlatform/samples/ontology/ManOntology2.xml
[Eat #0] -
   - obj->[Apple],
   - manr->[Fast],
   - agnt->[Man]").


cg_test_data([cg_dialect([df]), group(4)], "
   [Person: John2] <- (Agnt) - 
   [City: Boston2] <- (Dest) -
   [Bus2] <- (Inst) - [Go2]").


cg_test_data([cg_dialect([df]), group(3)], "
[Begin]-
        -obj->[Session],
        -srce->[Proposition = 
                   [Press] -
                      -obj -> [Key : enter] -partOf-> [Keyboard],
                      -agnt -> [Person : John] 
               ],
        -agnt->[Person : John]").


cg_test_data([cg_dialect([df]), group(3)], "
 [a] - (belives) -> 
 [statement = [Go2]
   - (Agnt)->[Person: John2]
   - (Dest)->[City: Boston2]
   - (Inst)->[Bus2]  ]").


cg_test_data([cg_dialect([df]), group(3)], "[Go*x][Person:'John'*y][City:'Boston'*z][Bus*w](Agnt?x?y)(Dest?x?z)(Inst?x?w)").

cg_test_data([skip, cg_dialect([df]), group(4)], "
// ontology required (to load first): aminePlatform/samples/ontology/ManOntology2.xml
[Eat #0] -
   - obj->[Apple],
   - manr->[Fast],
   - agnt->[Man]").

cg_test_data([cg_dialect([df]), group(3)], "[Woman:red]<-knows-[Man:karim]<-agnt-[Eat]-obj->[Apple]-(on)->[table]").

cg_test_data([cg_dialect([df]), group(3)], "[?x]<-(Agnt)-[Marry]-(Thme)->[Sailor]").

cg_test_data([cg_dialect([df]), group(3)], "
[Person: Mary *x]<-(Expr)-[Want]-(Thme)->
     [Situation:  [?x]<-(Agnt)-[Marry]-(Thme)->[Sailor] ]").

cg_test_data([cg_dialect([df]), group(3)], "
[Proposition: [Person: Mary *x]<-(Expr)-[Want]-(Thme)->
     [Situation:  [?x]<-(Agnt)-[Marry]-(Thme)->[Sailor] ]]").

cg_test_data([cg_dialect([df]), group(4)], "
[Person: Tom]<-(Expr)-[Believe]-(Thme)->
     [Proposition:  [Person: Mary *x]<-(Expr)-[Want]-(Thme)->
     [Situation:  [?x]<-(Agnt)-[Marry]-(Thme)->[Sailor] ]]").

cg_test_data([failing,cg_dialect([df]), group(4)], "
[Person: Tom]<-(Expr)<-[Believe]->(Thme)-
     [Proposition:  [Person: Mary *x]<-(Expr)<-[Want]->(Thme)-
     [Situation:  [?x]<-(Agnt)<-[Marry]->(Thme)->[Sailor] ]]").

%%Date: Thu, 28 May 92 08:45:26 -0500
%%From: esch%email.sp.unisys.com@metro.ucc.su.OZ.AU (John Esch)
%%To: cg@cs.umn.edu
%%Subject: CG TEST4, individuals

%%The following is used for regression testing of CONSTRUCT.

%%It contains the first set of type and individual examples from Conceptual Structures 
%%page 119 & 120.

%%CANON RESERVATIONS-AND-ELEPHANTS .

cg_test_data([cg_dialect([lf,sowa]), group(1)], 
"TYPE ARRIVAL-DATE(a) IS [UNIV:*a].").

cg_test_data([cg_dialect([lf,sowa]), group(1)], 
"TYPE CIRCUS(c) IS [UNIV:*c].").

cg_test_data([cg_dialect([lf,sowa]), group(1)], 
"TYPE CIRCUS(c) IS [UNIV:*c]").

cg_test_data([cg_dialect([lf,sowa]), group(1)], 
"[ELEPHANT:*c]<-(AGNT)<-[PERFORM]->(LOC)->[CIRCUS]").                              
                              
cg_test_data([cg_dialect([lf,sowa]), group(1)], "
[Go *x] (Agnt ?x [Person: John]) (Dest ?x [City: Boston]) (Inst ?x [Bus]) 
").

cg_test_data([cg_dialect([lf,sowa]), group(1)], 
"TYPE CIRCUS-ELEPHANT(C) IS
 [ELEPHANT:*C]<-(AGNT)<-[PERFORM]->(LOC)->[CIRCUS].").

cg_test_data([cg_dialect([lf,sowa]), group(1)], 
"TYPE CIRCUS-ELEPHANT(C) IS
 [ELEPHANT:*C]<-(AGNT)<-[PERFORM]->(LOC)->[CIRCUS].").

cg_test_data([cg_dialect([lf,sowa]), group(1)], 
"TYPE DEPARTURE-DATE(d) IS [UNIV:*d].").

cg_test_data([cg_dialect([lf,sowa]), group(1)], 
"TYPE ELEPHANT(e) IS [UNIV:*e].").

cg_test_data([cg_dialect([lf,sowa]), group(1)], 
"TYPE HOTEL(h) IS [UNIV:*h].").

cg_test_data([cg_dialect([lf,sowa]), group(1)], 
"TYPE HOTEL-RESERVATION(RESERVATION-NO) IS
[RESERVATION:*RESERVATION-NO]-
  ->(RCPT)->[PERSON]
  ->(OBJ)->[ROOM]->(LOC)->[HOTEL]
  ->(DUR)->[TIME-PERIOD]-
             ->(STRT)->[ARRIVAL-DATE]
             ->(UNTL)->[DEPARTURE-DATE],,.").

cg_test_data([cg_dialect([lf,sowa]), group(1)], 
"TYPE PERFORM(p) IS [UNIV:*p].").

cg_test_data([cg_dialect([lf,sowa]), group(1)], 
"TYPE PERSON(p) IS [UNIV:*p].").

cg_test_data([cg_dialect([lf,sowa]), group(1)], 
"TYPE PROPOSITION(p) IS [UNIV:*p].").

cg_test_data([cg_dialect([lf,sowa]), group(1)], 
"TYPE RESERVATION(r) IS [UNIV:*r].").

cg_test_data([cg_dialect([lf,sowa]), group(1)], 
"TYPE ROOM(r) IS [UNIV:*r].").

cg_test_data([cg_dialect([lf,sowa]), group(1)], 
"TYPE TIME-PERIOD(t) IS [UNIV:*t].").

cg_test_data([cg_dialect([lf,sowa]), group(1)], "
[RESERVATION #316209]-
  ->(RCPT)->[PERSON:JOHN SOWA]
  ->(OBJ)->[ROOM:Q2]->(LOC)->[HOTEL:Shelburne]
  ->(DUR)->[TIME-PERIOD:@4 NIGHTS]-
             ->(STRT)->[ARRIVAL-DATE:MARCH 14 1983]
             ->(UNTL)->[DEPARTURE-DATE:MARCH 18 1983]").

cg_test_data([cg_dialect([lf,sowa]), group(1)], "
[RESERVATION:#316209]-
  ->(RCPT)->[PERSON:JOHN SOWA]
  ->(OBJ)->[ROOM:Q2]->(LOC)->[HOTEL:Shelburne]
  ->(DUR)->[TIME-PERIOD:@4 NIGHTS]-
             ->(STRT)->[ARRIVAL-DATE:MARCH 14 1983]
             ->(UNTL)->[DEPARTURE-DATE:MARCH 18 1983]").

cg_test_data([cg_dialect([lf,sowa]), group(1)], "
INDIVIDUAL HOTEL-RESERVATION(#316209) IS
[RESERVATION:#316209]-
  ->(RCPT)->[PERSON:JOHN SOWA]
  ->(OBJ)->[ROOM:Q2]->(LOC)->[HOTEL:Shelburne]
  ->(DUR)->[TIME-PERIOD:@4 NIGHTS]-
             ->(STRT)->[ARRIVAL-DATE:MARCH 14 1983]
             ->(UNTL)->[DEPARTURE-DATE:MARCH 18 1983],,.").

cg_test_data([cg_dialect([lf,sowa]), group(1)], "
INDIVIDUAL HOTEL-RESERVATION(#316210) IS
[RESERVATION:#316210]-
  ->(RCPT)->[PERSON:JOHN ESCH]
  ->(OBJ)->[ROOM:Q3]->(LOC)->[HOTEL:Sidney]
  ->(DUR)->[TIME-PERIOD:@7 NIGHTS]-
             ->(STRT)->[ARRIVAL-DATE:MARCH 12 1983]
             ->(UNTL)->[DEPARTURE-DATE:MARCH 19 1983],,.").

cg_test_data([cg_dialect([lf,sowa]), group(1)], "
INDIVIDUAL CIRCUS-ELEPHANT(#BUMBO) IS
[ELEPHANT:#BUMBO]<-(AGNT)<-[PERFORM: {*}]->(LOC)->[CIRCUS:Flying Tigers].").

cg_test_data([skip,cg_dialect([lf,sowa]), group(1)], "
INDIVIDUAL CIRCUS-ELEPHANT(#JUMBO) IS
[ELEPHANT:#JUMBO]<-(AGNT)<-[PERFORM: {*}]->(LOC)->[CIRCUS:Barnum & Bailey].").


