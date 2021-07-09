/* Translation of XGs */

:- op(1001,xfy,( ... )).
:- op(1200,xfx,( '--->')).


:-thread_local tlxgproc:current_xg_module/1.
:-thread_local tlxgproc:current_xg_filename/1.
:-dynamic xgproc:current_xg_pred/4.
:-multifile xgproc:current_xg_pred/4.


abolish_xg(Prop):- ignore(tlxgproc:current_xg_module(M)),
  doall((xgproc:current_xg_pred(M,F,N,Props),member(Prop,Props),member(Prop,Props),
                 ignore((memberchk(xg_pred=P,Props),dmsg(abolising(current_xg_pred(M,F,N,Props))),predicate_property(P,number_of_clauses(NC)),flag(xg_assertions,A,A-NC))),
                 abolish(F,N),retractall(xgproc:current_xg_pred(M,F,N,_)))).

maybe_share_mp(M:F/A):- 
    MFA=M:F/A,!, % FA = F/A,   
   (M:multifile(MFA)), 
   (M:module_transparent(MFA)),
   (M:dynamic(MFA)),
   (M:export(MFA)),
   (M:public(MFA)), !. 
maybe_share_mp(FA):- strip_module(FA,M,_),!,share_mp(M:FA).

 
new_pred(P):- must(tlxgproc:current_xg_module(M)),new_pred(M,P).
new_pred(M,P0):- functor(P0,F,A),functor(P,F,A),new_pred(M,P,F,A),!.

new_pred(M,_,F,A):- xgproc:current_xg_pred(M,F,A,_),!.
new_pred(_,P,_,_):- recorded(P,'xg.pred',_), !.
new_pred(M,P,F,A) :-   
   maybe_share_mp(M:F/A),
   findall(K=V,(((K=xg_source,tlxgproc:current_xg_filename(V));(prolog_load_context(K,V), \+ (member(K,[stream,directory,variable_names])));((seeing(S),member(G,[(K=file,P=file_name(V)),(K=position,P=position(V))]),G,stream_property(S,P))))),Props),
   assert_if_new(xgproc:current_xg_pred(M,F,A,[xg_source=F,xg_ctx=M,xg_fa=(F/A),xg_pred=P|Props])),
   recordz(P,'xg.pred',_),
   recordz('xg.pred',P,_).

:- module_transparent(new_pred/4).
:- system:import(new_pred/4).

is_file_ext(Ext):-prolog_load_context(file,F),file_name_extension(_,Ext,F).
:-thread_local tlxgproc:do_xg_process_te/0.
:-export(xg_process_te_clone5/5).

processing_xg :- is_file_ext(xg),!.
processing_xg :- tlxgproc:do_xg_process_te,!.

xg_process_te_clone5(L,R,_Mode,P,Q):- expandlhs(L,S0,S,H0,H,P), expandrhs(R,S0,S,H0,H,Q).  %new_pred(P),usurping(Mode,P),!.

:-export(xg_process_te_clone/3).
xg_process_te_clone((H ... T --> R),Mode,((P :- Q))) :- !, xg_process_te_clone5((H ... T),R,Mode,P,Q).
xg_process_te_clone((L ---> R),Mode,((P :- Q))) :- !,xg_process_te_clone5(L,R,Mode,P,Q).
xg_process_te_clone((L --> R),Mode,PQ) :- wdmsg(warn(xg_process_te_clone((L --> R)))), !, xg_process_te_clone((L ---> R),Mode,PQ).

%chat80_term_expansion(In,Out):- compound(In),functor(In,'-->',_),trace,  must(xg_process_te_clone(In,+,Out)).
chat80_term_expansion((H ... T ---> R),((P :- Q))) :- must( xg_process_te_clone5((H ... T),R,+,P,Q)).
chat80_term_expansion((L ---> R), ((P :- Q))) :- must(xg_process_te_clone5(L,R,+,P,Q)).


chat80_term_expansion_now(( :- ain(_)) ,_ ):-!,fail.
%chat80_term_expansion_now(( :- _) ,_ ):-!,fail.
chat80_term_expansion_now(H,Into):- chat80_term_expansion(H,O),!, Into = ( :- ain(O)).

system:term_expansion(H, O):- (processing_xg->chat80_term_expansion_now(H,O)),!.

load_plus_xg_file(File):- strip_module(File,CM,F),must(load_plus_xg_file(CM,F)),!.

load_plus_xg_file(CM,F) :- fail, 
 locally(tlxgproc:current_xg_module(CM),
   locally(tlxgproc:do_xg_process_te, CM:ensure_loaded_no_mpreds(F))),!.

% was +(F).
load_plus_xg_file(CM,F) :-
   see(user),
   locally(tlxgproc:current_xg_module(CM),consume0(F,+)),
   seen.

% was -(F).
load_minus_xg_file(CM,F) :-
   see(user),
   locally(tlxgproc:current_xg_module(CM),consume0(F,-)),
   seen.


consume0(F0,Mode) :- 
   Stat_key = clauses,
   seeing(Old),
%   statistics(heap,[H0,Hf0]),
    statistics(Stat_key,H0),
    absolute_file_name(F0,F),
   see(F),
   abolish_xg(xg_source=F),
   locally(tlxgproc:current_xg_filename(F),tidy_consume(F,Mode)),
 ( (seeing(User2),User2=user), !; seen ),
   see(Old),
%   statistics(heap,[H,Hf]),
 statistics(Stat_key,H),
%   U is H-Hf-H0+Hf0,
    U is H-H0,
   dfmt('~N** Grammar from file ~w: ~w words .. **~n~n',
    [F,U]).


tidy_consume(F,Mode) :-
   consume_xg(F,Mode),
   fail.
tidy_consume(_,_).

consume_xg(F,Mode) :-
   flag(read_terms,_,0),
   repeat,
      read(X),
    ( (X=end_of_file, !, xg_complete(F));
      ((flag(read_terms,T,T+1),xg_process(X,Mode)),
         fail )).

xg_process((L ---> R),Mode) :- !,
   expandlhs(L,S0,S,H0,H,P),
   expandrhs(R,S0,S,H0,H,Q),
   new_pred(P),
   usurping(Mode,P),
   xg_assertz((P :- Q)), !.

xg_process((L-->R),Mode) :- 
   wdmsg(warn(xg_process((L --> R)))), !,
   xg_process((L ---> R),Mode).

xg_process(( :- G),_) :- !, call(G).
xg_process(( ?- G),_) :- !, forall(call(G),true).

xg_process((P :- Q),Mode) :-
   usurping(Mode,P),
   new_pred(P),
   xg_assertz((P :- Q)), !.
xg_process(P,Mode) :-
   usurping(Mode,P),
   new_pred(P),
   xg_assertz(P), !.

xg_assertz(P):- flag(xg_assertions,A,A+1),must((tlxgproc:current_xg_module(M),nop(dmsg(M:xg_assertz(P))),M:assertz(P))),!.

xg_complete(_F) :-
   recorded('xg.usurped',P,R0), erase_safe(recorded('xg.usurped',P,R0),R0),
   recorded(P,'xg.usurped',R1), erase_safe(recorded(P,'xg.usurped',R1),R1),
   fail.
xg_complete(F):- flag(read_terms,T,T),dmsg(info(read(T,F))),nl,nl.

usurping(+,_) :- !.
usurping(-,P) :-
   recorded(P,'xg.usurped',_), !.
usurping(-,P) :-
   functor(P,F,N),
   functor(Q,F,N),
   retractrules(Q),
   recordz(Q,'xg.usurped',_),
   recordz('xg.usurped',Q,_).

retractrules(Q) :-
   clause(Q,B),
   retractrule(Q,B),
   fail.
retractrules(_).

retractrule(_,virtual(_,_,_)) :- !.
retractrule(Q,B) :- retract((Q :- B)), !.

/* Rule ---> Clause */

expandlhs(T,S0,S,H0,H1,Q) :-
   xg_flatten0(T,[P|L],[]),
   front(L,H1,H),
   tag(P,S0,S,H0,H,Q).

xg_flatten0(X,L0,L) :- nonvar(X),!,
   xg_flatten(X,L0,L).
xg_flatten0(_,_,_) :-
   dmsg(warn('! Variable as a non-terminal in the lhs of a grammar rule')),
   fail.

xg_flatten((X...Y),L0,L) :- !,
   xg_flatten0(X,L0,[gap|L1]),
   xg_flatten0(Y,L1,L).
xg_flatten( ','(X,Y),L0,L) :- !,
   xg_flatten0(X,L0,[nogap|L1]),
   xg_flatten0(Y,L1,L).
xg_flatten(X,[X|L],L).

front([],H,H).
front([K,X|L],H0,H) :-
   case(X,K,H1,H),
   front(L,H0,H1).

case([T|Ts],K,H0,x(K,terminal,T,H)) :- !,
   unwind(Ts,H0,H).
case(Nt,K,H,x(K,nonterminal,Nt,H)) :- virtualrule(Nt).

virtualrule(X) :-
   functor(X,F,N),
   functor(Y,F,N),
   tag(Y,S,S,Hx,Hy,P),
 ( clause(P,virtual(_,_,_)), !;
      new_pred(P),
      asserta((P :- virtual(Y,Hx,Hy))) ).

expandrhs(X,S0,S,H0,H,Y) :- var(X),!,
   tag(X,S0,S,H0,H,Y).
expandrhs( ','(X1,X2),S0,S,H0,H,Y) :- !,
   expandrhs(X1,S0,S1,H0,H1,Y1),
   expandrhs(X2,S1,S,H1,H,Y2),
   and(Y1,Y2,Y).
expandrhs((X1;X2),S0,S,H0,H,(Y1;Y2)) :- !,
   expandor(X1,S0,S,H0,H,Y1),
   expandor(X2,S0,S,H0,H,Y2).
expandrhs({X},S,S,H,H,X) :- !.
expandrhs(L,S0,S,H0,H,G) :- islist(L), !,
   expandlist(L,S0,S,H0,H,G).
expandrhs(X,S0,S,H0,H,Y) :-
   tag(X,S0,S,H0,H,Y).

expandor(X,S0,S,H0,H,Y) :-
   expandrhs(X,S0a,S,H0a,H,Ya),
 ( S\==S0a, !, S0=S0a, Yb=Ya; and(S0=S0a,Ya,Yb) ),
 ( H\==H0a, !, H0=H0a, Y=Yb; and(H0=H0a,Yb,Y) ).

expandlist([],S,S,H,H,true).
expandlist([X],S0,S,H0,H,terminal(X,S0,S,H0,H) ) :- !.
expandlist([X|L],S0,S,H0,H,(terminal(X,S0,S1,H0,H1),Y)) :-
   expandlist(L,S1,S,H1,H,Y).

tag(P,A1,A2,A3,A4,QQ) :- var(P),!,
 QQ = phraseXG(P,A1,A2,A3,A4).

tag(P,A1,A2,A3,A4,Q) :-
   P=..[F|Args0],
   conc_gx(Args0,[A1,A2,A3,A4],Args),
   Q=..[F|Args].

and(true,P,P) :- !.
and(P,true,P) :- !.
and(P,Q, ','(P,Q)).

islist([_|_]).
islist([]).

unwind([],H,H) :- !.
unwind([T|Ts],H0,x(nogap,terminal,T,H)) :-
   unwind(Ts,H0,H).

conc_gx([],L,L) :- !.
conc_gx([X|L1],L2,[X|L3]) :-
   conc_gx(L1,L2,L3).

xg_listing(File) :-
   telling(Old),
   tell(File),
   list_xg_clauses,
   told,
   tell(Old).

compile_xg_clauses :- recorded('xg.pred',P,_),functor(P,F,N),share_mp(F/N),fail.
% compile_xg_clauses :- recorded('xg.pred',P,_),functor(P,F,N),compile_predicates([F/N]),fail.
compile_xg_clauses :- !.
compile_xg_clauses:- 'newg.pl' = F, xg_listing(F),[F].
%compile_xg_clauses:- tmp_file_stream(text, File, Stream), xg_listing(Stream),[File].

list_xg_clauses :-
   recorded('xg.pred',P,_),
   functor(P,F,N),
   listing(F/N),
   nl,
   fail.
list_xg_clauses.

:-export(load_xg/0).

load_xg:-
  load_plus_xg_file('clone.xg'),
  load_plus_xg_file('lex.xg'),
  compile_xg_clauses.

go_xg :- load_xg, xg_listing('newg.pl').

:- fixup_exports.

end_of_file.

