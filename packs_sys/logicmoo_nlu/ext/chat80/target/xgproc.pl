/* Translation of XGs */

:- op(1001,xfy,( ... )).
:- op(1200,xfx,( '--->')).


:-thread_local tlxgproc:current_xg_module/1.
:-thread_local tlxgproc:current_xg_filename/1.
:-dynamic user:current_xg_pred/4.
:-multifile user:current_xg_pred/4.

:-nodebug(xgproc).

xg_msg(M):- (debugging(xgproc);debugging(logicmoo(nlu))),!,dmsg_pretty(M).
xg_msg(_):- prolog_load_context(reloading,true),!.
xg_msg(M):- nop(dmsg(M)).

abolish_xg(Prop):- 
  % ignore(tlxgproc:current_xg_module(M)),
  doall((user:current_xg_pred(M,F,A,Props),
                 (\+ \+ member(Prop,Props)),        
                 ignore((memberchk(xg_pred=P,Props),
                         xg_msg(abolising(current_xg_pred(M,F,A,Props))),
                         predicate_property(P,number_of_clauses(NC)),flag(xg_assertions,XAC,XAC-NC),
                 ignore(abolish(M:F,A)),
                 xg_msg(abolising(current_xg_pred(M,F,A,Props))),
                 (var(P)->functor(P,F,A);true),
                 % dynamic(M:F/A),
                 forall(recorded(P,'xg.pred',E),erase(E)),
                 forall(recorded('xg.pred',P,E),erase(E)),
                 retractall(user:current_xg_pred(M,F,A,_WasProps)))))).

to_mp(M0,(P0:-_),M,P):- !,to_mp(M0,P0,M,P).
to_mp(_,((M0:F)/A),M,P):- !, to_mp(M0,(F/A),M,P).
to_mp(M0,(F/A),M,P):- must(integer(A)),functor(P0,F,A),to_mp(M0,P0,M,P).
to_mp(_,M0:P0,M,P):- !, to_mp(M0,P0,M,P).
to_mp(M0,P0,M,P):- functor(P0,F,A),functor(P,F,A),M=M0.

re_mp(M0,P0,M,P):- to_mp(M0,P0,M,P),!, (M\=@=M0 ; P\=@=P0 ).

new_pred(P):- must(tlxgproc:current_xg_module(M)),new_pred(M,P).
new_pred(M0,P0):- re_mp(M0,P0,M,P), !, new_pred(M,P).
new_pred(M,P):-
  functor(P,F,A),
  dynamic(M:F/A),
  multifile(M:F/A),
  new_pred(M,P,F,A),!.

new_pred(M,_,F,A):- user:current_xg_pred(M,F,A,_),!.
new_pred(_,P,_,_):- recorded(P,'xg.pred',_), !.
new_pred(M,P,F,A) :-   
   share_mp(M:F/A),
   findall(K=V,all_kv_props(K,V),Props),
   assert_if_new(user:current_xg_pred(M,F,A,[xg_source=F,xg_ctx=M,xg_fa=(F/A),xg_pred=P|Props])),
   recordz(P,'xg.pred',_),
   recordz('xg.pred',P,_).

all_kv_props(xg_source,V):- tlxgproc:current_xg_filename(V).
all_kv_props(K,V):- prolog_load_context(K,V), \+ member(K,[stream,directory,variable_names]).
all_kv_props(K,V):- (seeing(S);current_input(S)),
    \+ \+ stream_property(S,file_name(_)), !,
   member(G,[(K=file,P=file_name(V)),(K=position,P=position(V))]),
   call(G),
   stream_property(S,P).


is_file_ext(Ext):-prolog_load_context(file,F),file_name_extension(_,Ext,F).
:-thread_local tlxgproc:do_xg_process_te/0.
:-export(xg_process_te_clone/5).

processing_xg :- is_file_ext(xg),!.
processing_xg :- tlxgproc:do_xg_process_te,!.

xg_process_te_clone(L,R,_Mode,P,Q):- expandlhs(L,S0,S,H0,H,P), expandrhs(R,S0,S,H0,H,Q).  %new_pred(P),usurping(Mode,P),!.

:-export(xg_process_te_clone/3).
xg_process_te_clone((H ... T --> R),Mode,((P :- Q))) :- !, xg_process_te_clone((H ... T),R,Mode,P,Q).
xg_process_te_clone((L --> R),Mode,((P :- Q))) :- !,xg_process_te_clone(L,R,Mode,P,Q).
xg_process_te_clone((L ---> R),Mode,((P :- Q))) :- !,xg_process_te_clone(L,R,Mode,P,Q).

chat80_term_expansion(In,Out):- compound(In),functor(In,'-->',_), fail, trace, must(xg_process_te_clone(In,+,Out)).
chat80_term_expansion((H ... T ---> R),((P :- Q))) :- must( xg_process_te_clone((H ... T),R,+,P,Q)).
chat80_term_expansion((L ---> R), ((P :- Q))) :- must(xg_process_te_clone(L,R,+,P,Q)).


chat80_term_expansion_now(( :- _) ,_ ):-!,fail.
chat80_term_expansion_now(H,':-'(ain(O))):- fail, trace, chat80_term_expansion(H,O),!.

system:term_expansion(H, O):- processing_xg->chat80_term_expansion_now(H,O).


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
 ( (seeing(User2),User2==user), !; seen ),
   see(Old),
%   statistics(heap,[H,Hf]),
 statistics(Stat_key,H),
%   U is H-Hf-H0+Hf0,
    U is H-H0,
    flag(xg_assertions,XAC,XAC),
   dmsg(call(dfmt('~N** Grammar from file ~w: ~w words .. time ~w **~n~n',[F,Mode:XAC,U]))).


tidy_consume(F,Mode) :-
   consume(F,Mode),
   fail.
tidy_consume(_,_).

consume(F,Mode) :-
   flag(read_terms,_,0),
   repeat,
      read_conv(X),
    ( (X==end_of_file, !, xg_complete(F));
      ((flag(read_terms,T,T+1),xg_process_now(X,Mode)),
         fail )).

do_renames80(X,Y):- current_predicate(do_renames/2),do_renames(X,Y),!.
do_renames80(X,X).

read_conv(X):- read(Y),once(do_renames80(Y,X)).

:- module_transparent(xg_process_now/2).
xg_process_now(X,Mode):- catch(xg_process(X,Mode),Error,(xg_msg(Error),fail)),!.
xg_process_now(X,Mode):- rtrace(xg_process(X,Mode)),!.
xg_process_now(X,Mode):- wdmsg(failed(xg_process(X,Mode))).

:- module_transparent(xg_process/2).
xg_process((L ---> R),Mode) :- !,
   expandlhs(L,S0,S,H0,H,P),
   expandrhs(R,S0,S,H0,H,Q),
   new_pred(P),
   usurping(Mode,P),
   xg_assertz((P :- Q)), !.

xg_process((L-->R),Mode) :- !,
   expandlhs(L,S0,S,H0,H,P),
   expandrhs(R,S0,S,H0,H,Q),
   new_pred(P),
   usurping(Mode,P),
   xg_assertz((P :- Q)), !.

xg_process(( :- G),_) :- !, call(G).

xg_process((P :- Q),Mode) :-
   usurping(Mode,P),
   new_pred(P),
   xg_assertz((P :- Q)).
xg_process(P,Mode) :-
   usurping(Mode,P),
   new_pred(P),
   xg_assertz(P).

xg_assertz(P):- flag(xg_assertions,A,A+1),
  must((tlxgproc:current_xg_module(M),
        xg_msg(xg_assertz(M:P)),
        assertz_dynamic(M,P))),!.

assertz_dynamic(M,P):-
  cvt_to_dynamic(M,P),
  M:assertz(P).

:- meta_predicate(cvt_to_dynamic(:)).

cvt_to_dynamic(M0,P0):- re_mp(M0,P0,M,P),!,cvt_to_dynamic(M,P).
cvt_to_dynamic(M,P):- predicate_property(M:P,dynamic),!.
cvt_to_dynamic(M,P):- functor(P,F,A),functor(H,F,A), 
    findall((H:-B),M:clause(H,B),AssertList),!,
    abolish(M:F,A),
    dynamic(M:F/A),
    maplist(assertz,AssertList).

cvt_to_dynamic(M:P):- cvt_to_dynamic(M,P).

xg_complete(_F) :-
   recorded('xg.usurped',P,R0), erase_safe(recorded('xg.usurped',P,R0),R0),
   recorded(P,'xg.usurped',R1), erase_safe(recorded(P,'xg.usurped',R1),R1),
   fail.
xg_complete(F):- flag(read_terms,T,T),xg_msg(info(read(T,F))),nl,nl.

%:- listing(xg_complete/1).

%:- cvt_to_dynamic(xg_complete/1).

%:- listing(xg_complete/1).

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
   flatten0(T,[P|L],[]),
   front(L,H1,H),
   tag(P,S0,S,H0,H,Q).

flatten0(X,L0,L) :- nonvar(X),!,
   flatten_xg(X,L0,L).
flatten0(_,_,_) :-
   xg_msg(warn('! Variable as a non-terminal in the lhs of a grammar rule')),
   fail.

flatten_xg((X...Y),L0,L) :- !,
   flatten0(X,L0,[gap|L1]),
   flatten0(Y,L1,L).
flatten_xg((X,Y),L0,L) :- !,
   flatten0(X,L0,[nogap|L1]),
   flatten0(Y,L1,L).
flatten_xg(X,[X|L],L).

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
expandrhs((X1,X2),S0,S,H0,H,Y) :- !,
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
and(P,Q,(P,Q)).

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
   list_clauses,
   told,
   tell(Old).

compile_xg_clauses :- recorded('xg.pred',P,_),functor(P,F,N),share_mp(F/N),fail.
% compile_xg_clauses :- recorded('xg.pred',P,_),functor(P,F,N),compile_predicates([F/N]),fail.
compile_xg_clauses.

list_clauses :-
   recorded('xg.pred',P,_),
   functor(P,F,N),
   listing(F/N),
   nl,
   fail.

list_clauses.

:-export(load_xg/0).

load_xg:-
  load_plus_xg_file('clone.xg'),
  load_plus_xg_file('lex.xg'),
  compile_xg_clauses.

go_xg :- load_xg, xg_listing('newg.pl').


end_of_file.

