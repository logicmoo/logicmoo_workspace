:- encoding(iso_latin_1).
/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/

:- include(kaggle_arc_header).

:- meta_predicate(print_grid(+,+,+,+)).
:- meta_predicate(print_grid(+,+,+)).


%:- autoload(library(http/html_write),[html/3,print_html/1]).

is_debugging(M):- \+ \+ debugging(M),!.
%is_debugging(_):- menu_or_upper('B').

debug_m(_,Tiny):- display_length(Tiny,Len),Len<30,!,pp(Tiny).
debug_m(M,_):- \+ is_debugging(M),!.
%debug_m(_,List):- is_list(List),!,print_ss(List).
debug_m(_,Term):- pp(Term).
debug_c(M,_):- \+ is_debugging(M),!.
debug_c(_,C):- call(C),!.
debug_c(M,C):- wots_hs(S,C),debug_m(M,S),!.

:- meta_predicate(wno(0)).
wno(G):-
 locally(b_setval(print_collapsed,10), G).

:- meta_predicate(print_collapsed(0)).
print_collapsed(Size,G):- 
 locally(b_setval(print_collapsed,Size), print_collapsed0(Size,G)).

:- meta_predicate(print_collapsed0(0)).
print_collapsed0(Size,G):- Size<10, !, call(G). 
% print_collapsed(Size,G):-  call(G). 
print_collapsed0(Size,G):- Size>=10, !, wots_hs(_S,G).
print_collapsed0(_,G):- wots_vs(S,G),write(S).

tersify(I,O):- tracing,!,I=O.
%tersify(I,O):- term_variables(I,Vs), \+ ( member(V,Vs), attvar(V)),!,I=O.
tersify(I,O):- tersify23(I,O),!.
tersify(X,X):-!.

tersify23(I,O):- quietly((tersify2(I,M),tersify3(M,O))),!.

%srw_arc(I,O):- is_grid(I),!, wots_hs(O,(write('"'),print_grid(I),write('"'))).
%srw_arc(I,O):- compound(I),!, wots_hs(O,(write(ppt(I)))).
/*
srw_arc(I,O):- is_grid(I),!, wots_hs(O,(write('"'),print_grid(I),write('"'))).
*/
srw_arc(I,O):- is_vm_map(I),!, O='..vvmm..'.
srw_arc(I,O):- is_grid(I),!, O='..grid..'.
/*
srw_arc(List,O):- current_prolog_flag(dmsg_len,Three),
  is_list(List),length(List,L),L>Three,
   append([A,B,C],[F|_],List),F \='...'(_), !, 
  simplify_goal_printed([A,B,C,'....'(L>Three)],O).
*/
%srw_arc(gridFn(_),gridFn):-!.
%srw_arc(I,O):- is_points_list(I), length(I,N),N>10,!,O='..lo_points..'(N),!.
%srw_arc(I,O):- is_list(I), length(I,N),N>10,!,O='..lo_points..'(N),!.
srw_arc(I,O):- tersify(I,O),!,I\==O,!.

:- multifile(dumpst_hook:simple_rewrite/2).
:- dynamic(dumpst_hook:simple_rewrite/2).

dumpst_hook:simple_rewrite(I,O):- fail, notrace(catch(arc_simple_rewrite(I,O),_,fail)).

arc_simple_rewrite(I,O):- 
  \+ current_prolog_flag(never_pp_hook, true), nb_current(arc_can_portray,t),
  current_predicate(bfly_startup/0),
  current_predicate(is_group/1), 
  b_setval(arc_can_portray,nil),
  locally(b_setval(arc_can_portray,nil),once((compound(I), lock_doing(srw_arc,I,srw_arc(I,O))))), I\==O, I\=@=O, !, \+ I=O,
  b_setval(arc_can_portray,t).


%:- set_prolog_flag(never_pp_hook, true).


portray_terse:- true,!.

:- discontiguous arc_portray/2. 


arc_portray(S,_):- term_is_ansi(S), !, write_keeping_ansi_mb(S).
arc_portray(_,_):- \+ \+ current_prolog_flag(never_pp_hook, true), nb_current(arc_can_portray,t), !, fail.
arc_portray(Map,TF):- get_map_pairs(Map,Type,Pairs),!, arc_portray_pairs(Type,TF,Pairs). 

arc_portray_t(G, _):- is_vm_map(G), !, write_map(G,'arc_portray_t').
arc_portray_t(G, _):- is_grid(G),  !, data_type(G,W),writeq(grid(W)).
arc_portray_t(G, _):- print(G),!.

arc_portray(G, _):- is_vm_map(G),  !, write_map(G,'arc_portray').
arc_portray(G, TF):- TF == true, portray_terse, arc_portray_t(G, TF),!.
arc_portray(G, TF):- catch(arc_portray_nt(G, TF),E,(writeln(E),never_let_arc_portray_again,fail)),!.
%arc_portray(G, _TF):- writeq(G),!.

% Portray In Debugger

arc_portray_nt(G, false):- is_grid(G), print_grid(G),!.
%arc_portray_nt([G|L],_False):- is_object(G), !, pp([G|L]).
%arc_portray_nt(G0, true):- is_group(G0), ppt(G0),!.
%arc_portray_nt(G0, false):- is_group(G0), ppt(G0),!.
arc_portray_nt(G0, Tracing):- is_group(G0), into_list(G0,G), length(G,L),% L>1, !,
   maplist(tersify,G0,GG), write(GG),
   if_t(Tracing==false,
    in_cmt((
     dash_chars, 
     once(((why_grouped(_TestID,Why,WG),WG=@=G,fail);(Why = (size2D=L)))),!,
     print_grid(Why,G),nl_now,
     
     %underline_print(writeln(Why)),
     %print_info_l(G),
     dash_chars))).


arc_portray_nt(G,_False):- is_object(G), wots(S,writeg(G)),
  global_grid(G,GG),!,
  print_grid(GG),
  write(S),!. % show_indiv(S,G).
  %object_grid(G,OG), 
  %neighbor_map(OG,NG), !,
  %print_grid(object_grid,NG),nl_now,
  %underline_print(print_info(G)),

arc_portray_nt(G,false):- via_print_grid(G),!, grid_size(G,H,V),!,H>0,V>0, print_grid(H,V,G).

% Portray In tracer
arc_portray_nt(G,true):- is_object(G),underline_print((ppt(G))).
arc_portray_nt(G,true):- via_print_grid(G),write_nbsp,underline_print((ppt(G))),write_nbsp.
arc_portray_nt(G,true):- tersify(G,O),write_nbsp,writeq(O),write_nbsp.
arc_portray_nt(G0, _):- \+ is_gridoid(G0),!,print(G0).


arc_portray_pairs(Type,TF,Pairs):- 
  length(Pairs,N),
  writeln(arc_portray_pairs(Type,TF,len(N))),
  swap_kv(Pairs,VKPairs),
  keysort(VKPairs,SVKPairs),
  my_maplist(arg(2),SVKPairs,SVKPairs2),
  arc_portray_type_pairs(TF,SVKPairs2).

arc_portray_type_pairs(TF,Pairs):- append(Left,[K1-V1,K2-V2|Right],Pairs),is_grid(V1),is_grid(V2),!,
  append(Left,[call-print_side_by_side(yellow,V1,K1,_,V2,K2)|Right],PairsM),
  arc_portray_type_pairs(TF,PairsM).
arc_portray_type_pairs(TF,Pairs):- 
  forall(member(K-V,Pairs),arc_portray_pair(Pairs,K,V,TF)).

swap_kv([_-V|Pairs],VKPairs):- plain_var(V),!, swap_kv(Pairs,VKPairs). 
swap_kv([K-V|Pairs],['-'(Type,K-V)|VKPairs]):- 
  data_type(V,Type),
  swap_kv(Pairs,VKPairs). 
swap_kv([],[]).


arc_portray_pair(Ps,K,Val,TF):- 
 nl_if_needed,
 arc_portray_1_pair(Ps,K,Val,TF),
 nl_if_needed_ansi.

arc_portray_1_pair(_Ps,call,Val,_TF):- !, call(Val).
arc_portray_1_pair(Ps,K,Val,TF):- 
 (via_print_grid(Val) -> print_grid(K,Val) 
   ;  (print(K),write('= '),once(arc_portray(Val,TF);print(Val)))),
 ignore(arc_portray_pair_optional(Ps,K,Val,TF)),!.

arc_portray_pair_optional(Ps,K,Val,TF):-
 once(( Val\==[], is_list(Val),my_maplist(is_object,Val),
  print_info(Val),
  Val \= [_], 
  compare_objects(Val,Diffs),  
  color_print(cyan,call(arc_portray_pair(Ps,diffs(K),Diffs,TF))))).


% arc_portray(G):- \+ \+ catch((wots_hs(S,( tracing->arc_portray(G,true);arc_portray(G,false))),write(S),ttyflush),_,fail).
arc_portray(G):- \+ compound(G),fail.
arc_portray(G):- is_vm(G), !, write('..VM..').
arc_portray(G):- \+ nb_current(arc_portray,t),\+ nb_current(arc_portray,f),is_print_collapsed,!, 
  locally(nb_setval(arc_portray,t),arc_portray1(G)).
arc_portray(G):- \+ nb_current(arc_portray,f),!, locally(nb_setval(arc_portray,t),arc_portray1(G)).
arc_portray(G):- locally(nb_setval(arc_portray,f),arc_portray1(G)).

arc_portray1(G):-
 flag(arc_portray_current_depth,X,X), X < 3,
 \+ \+ 
  setup_call_cleanup(flag(arc_portray_current_depth,X,X+1),catch(((tracing->arc_portray(G,true);
  arc_portray(G,false)),ttyflush),E,(fail,format(user_error,"~N~q~n",[E]),fail)),flag(arc_portray_current_depth,_,X)).

  
%via_print_grid(G):- tracing,!,fail.
via_print_grid(G):- is_points_list(G). %,!,fail,grid_size(G,H,V),number(H),number(V),H>1,V>1.
via_print_grid(G):- is_grid(G).
via_print_grid(G):- is_obj_props(G),!,fail.
via_print_grid(G):- is_object(G).
via_print_grid(G):- is_group(G).
via_print_grid(G):- is_gridoid(G).

 

terseA(_,[],[]):- !.
terseA(_,L,'... attrs ...'(N)):- is_list(L),length(L,N),N>10,!.
terseA(I,[A|L],[B|LL]):-terseA(I,A,B),terseA(I,L,LL),!.
terseA(I,dif(A,B),B):-A==I,!.
terseA(I,dif(B,A),B):-A==I,!.
terseA(_,put_attr(_,B,A),A):- B==ci,!.
terseA(_,put_attr(_,B,A),B=A):-!.
terseA(_,A,A):-!.


simple_enough(I):- plain_var(I).
simple_enough(I):- atomic(I).
simple_enough(I):- \+ compound(I),!.
simple_enough(_*_):-!.
simple_enough(_+_):-!.
simple_enough(A):- functor(A,_,1),arg(1,A,E),!,simple_enough(E).
%simple_enough(I):- number(I).
%simple_enough(I):- atom(I).

tersify0(I,O):- simple_enough(I),!,I=O.
tersify0(I,av(C,Others)):- attvar(I),copy_term(I,C,Attrs),terseA(C,Attrs,Others),!.
tersify0(I,I):- var(I),!.


%tersifyC(D):- is_vm_map(D),!.
tersifyC(av(_,_)).
tersifyC(objFn(_,_)).
tersifyC(groupFn(_,_)).
tersifyC(objFn(_)).
tersifyC(groupFn(_)).

tersify1(I,O):- simple_enough(I),!,I=O.
tersify1(av(_,Blue), -(Blue)):-!.
tersify1(I,O):- compound(I), tersifyC(I),!,I=O.
tersify1(gridFn(I),gridFn(I)):-!. % tersifyG(I,O).
%tersify1(gridFn(I),gridFn(O)):-tersifyG(I,O).
tersify1(Nil,[]):- Nil == [],!.
tersify1(I,gridFn(S)):- is_grid(I), into_gridnameA(I,O),!,sformat(S,'~w',[O]).
tersify1(I,gridFn(O)):- is_grid(I),tersifyG(I,O),!.
tersify1(I,groupFn(O,List)):- is_group(I), mapgroup(tersify1,I,List),mapgroup(obj_to_oid,I,OIDs),length(List,N), !,ignore((get_current_test(TestID),is_why_grouped(TestID,N,Why,OIDs),!,O=Why)).

tersify1(I,Q):- is_object(I),object_ref_desc(I,Q),!.
tersify1(I,O):- is_vm_map(I), get_kov(objs,I,_),!, O='$VAR'('VM').
tersify1(I,O):- is_vm_map(I), get_kov(pairs,I,_),!, O='$VAR'('Training').


tersifyG(I,O):- tersifyL(I,O),numbervars(O,1,_,[attvar(bind),singletons(false)]),!.

%tersifyL(I,I):- is_ftVar(I),!.
%tersifyL(I,I):- \+ compound(I),!.
tersifyL(I,O):- \+ is_cons(I),!,O=I.
tersifyL([H|I],[HH|I]):- \+ is_list(I),!,tersify(H,HH).
tersifyL([H|I],O):- nonvar(H), \+ is_group(I), display_length(I,N) , N>170, 
  length(I,LL),tersify(H,HH),(('...'(HH,LL,'...'(N)))=O),!.
tersifyL(I,O):- tersify0(I,O),!.
tersifyL([H|TT],[HH|TT]):- tersify(H,HH),!,tersifyL(TT,TT),!.
tersifyL(I,O):- tersify1(I,O),!.
tersifyL(I,I).

tersify2(I,O):- compound(I),(I=(N=V)),tersify2(N,NN),tersify2(V,VV),!,O=(NN=VV).
tersify2(I,O):- simple_enough(I),!,I=O.
tersify2(I,O):- compound(I),tersify1(I,O),!.
tersify2(I,O):- tersify0(I,O),!.
tersify2(I,O):- is_list(I), !, my_maplist(tersify2,I,O).
tersify2(I,O):- compound(I), !, compound_name_arguments(I,F,IA), my_maplist(tersify,IA,OA), compound_name_arguments(O,F,OA).
tersify2(I,I).

tersify3(I,O):- compound(I),(I=(N=V)),tersify3(N,NN),tersify3(V,VV),!,O=(NN=VV).
tersify3(I,O):- simple_enough(I),!,I=O.
tersify3(I,O):- compound(I),tersify1(I,O),!.
tersify3(I,O):- tersify0(I,O),!.
tersify3([H|I],O):- is_list(I), ((display_length(I,N), N>170) -> 
  (length(I,LL),tersify(H,HH),(('...'(HH,LL,'...'(N)))=O)); I=O),!.
tersify3(I,O):- is_list(I), !, my_maplist(tersify3,I,O).
tersify3(I,O):- compound(I), !, compound_name_arguments(I,F,IA), my_maplist(tersify,IA,OA), compound_name_arguments(O,F,OA).
tersify3(I,I).

write_map(G,Where):- is_vm(G), !, write('...VM_'),write(Where),write('...').
write_map(G,Where):- is_vm_map(G), !, write('...Map_'),write(Where),write('...').
write_map(G,Where):- is_dict(G), !, write('...Dict_'),write(Where),write('...').
write_map(_G,Where):- write('...'),write(Where),write('...').



non_empty_wqs_c(V):- \+ empty_wqs_c(V).
empty_wqs_c(V):- var(V),!,fail.
empty_wqs_c(A):- atom(A),atom_string(A,S),!,empty_wqs_c(S).
empty_wqs_c([]).
empty_wqs_c("").
empty_wqs_c("&nbsp;").
empty_wqs_c(" ").
empty_wqs_c("\n").

is_writer_goal(H):- \+ callable(H),!,fail.
is_writer_goal(H):- is_list(H),!,fail.
is_writer_goal(A):- atom(A),!,is_writer_goal_f(A).
is_writer_goal(H):- \+ compound(H),!,fail.
%is_writer_goal((C1,C2)):- !, (is_writer_goal(C1);is_writer_goal(C2)).
is_writer_goal(C):- compound_name_arity(C,F,_),once(is_writer_goal_f(F);(arg(_,C,E),is_writer_goal(E))).


is_writer_goal_f(wqs_c).
is_writer_goal_f(F):- is_writer_goal_l(F),!.
is_writer_goal_f(F):- \+ atom(F),!, term_to_atom(F,A),is_writer_goal_f(A).
is_writer_goal_f(F):- not_writer_goal_r(R),atom_concat(_,R,F),!,fail.
is_writer_goal_f(F):- is_writer_goal_l(L),atom_concat(L,_,F),!.
is_writer_goal_f(F):- is_writer_goal_l(R),atom_concat(_,R,F),!.
not_writer_goal_r(test). is_writer_goal_l(msg). is_writer_goal_l(call). 
is_writer_goal_l(nl).  is_writer_goal_l(format). is_writer_goal_l(with_).  
is_writer_goal_l(locally).

is_writer_goal_l(html).  is_writer_goal_l(ptcol).  is_writer_goal_l(wots).
is_writer_goal_l(print). is_writer_goal_l(flush_output).  is_writer_goal_l(wqs).
is_writer_goal_l(pp). is_writer_goal_l(write).  is_writer_goal_l(dash_).


maybe_color(SS,_):- term_contains_ansi(SS),!, write_nbsp, write(SS).
maybe_color(SS,P):- term_contains_ansi(P),!, write_nbsp, write(SS).
maybe_color(SS,P):- pp_msg_color(P,C), ansicall(C,is_maybe_bold(P,write(SS))),!.

write_atom(S):- \+ atom(S),!,wqs(S).
write_atom(S):- atom_contains(S,'~'),!,notrace(catch(format(S,[]),_,maybe_write_atom_link(S))).
write_atom(S):- maybe_write_atom_link(S),!.
write_atom(S):- into_title_str(S,TS),write(TS),!.

:- meta_predicate(into_title_str(+,-)).
into_title_str(Term,Str):- string(Term),!,Str=Term.
into_title_str(Term,Str):- plain_var(Term),sformat(Str,'~p',[Term]),!.
into_title_str(Term,Str):- var(Term),tersify0(Term,Terse), sformat(Str,'~p',[Terse]),!.
into_title_str(Term,Str):- term_is_ansi(Term), wots(Str,write_keeping_ansi_mb(Term)),!.
into_title_str(Term,Str):- (is_codelist(Term);is_charlist(Term)),catch(sformat(Str,'~s',[Term]),_,sformat(Str,'~p',[Term])),!.
into_title_str(Term,Str):- is_list(Term),my_maplist(into_title_str,Term,O3),atomics_to_string(O3," ",Str),!.
into_title_str([H|T],Str):- into_title_str(H,A),into_title_str(T,B),atomics_to_string([A,B]," ",Str),!.
into_title_str(Term,Str):- \+ callable(Term),sformat(Str,'~p',[Term]),!.
into_title_str(format(Fmt,Args),Str):- sformat(Str,Fmt,Args),!.
into_title_str(Term,""):- empty_wqs_c(Term),!.
into_title_str(out,"Output").
into_title_str(in,"Input").
into_title_str(i,"IN").
into_title_str(o,"OUT").
into_title_str(Term,Str):- atom(Term),is_valid_linkid(Term,Kind,_),Term\=@=Kind,into_title_str(Kind,KS),sformat(Str,'~w (~w)',[Term,KS]),!.
into_title_str(Term,Str):- atom(Term), atom_contains(Term,'_'), \+ atom_contains(Term,' '),  to_case_breaks(Term,T),
 include(\=(xti(_,punct)),T,O),my_maplist(arg(1),O,O1),my_maplist(toProperCamelAtom,O1,O2),
  atomics_to_string(O2," ",Str),!.
into_title_str(Term,Str):- has_short_id(Term,Kind,ID),Term\=@=Kind,into_title_str(Kind,KS),sformat(Str,'~w (~w)',[ID,KS]),!.

into_title_str(T-U,Str):- into_title_str([some(T),"..to..",some(U)],Str).
into_title_str(T*U,Str):- into_title_str([some(T),"(",some(U),")"],Str).
into_title_str(T+U,Str):- into_title_str(T,S1), number(U), N is U+1, sformat(Str,'~w #~w',[S1,N]).
into_title_str(T+U,Str):- var(U), into_title_str(T,S1), sformat(Str,'~w(s)',[S1]).
into_title_str(title(Term),Str):- !, into_title_str(Term,Str),!.
into_title_str(some(Var),"Some"):- var(Var),!.
into_title_str(some(Var),Str):- !, into_title_str(Var,Str).
into_title_str(User:Term,Str):- User == user, !, into_title_str(Term,Str).
into_title_str(trn,"Training Pair").
into_title_str(tst,"EVALUATION TEST").
%into_title_str(Term,Str):- tersify23(Term,Terse),Term\=@=Terse,!,into_title_str(Terse,Str).
into_title_str(Term,Str):- callable_arity(Term,0),is_writer_goal(Term),catch(notrace(wots(Str,call_e_dmsg(Term))),_,fail),!.
into_title_str(Term,Str):- catch(sformat(Str,'~p',[Term]),_,term_string(Term,Str)),nonvar(Str),atom_length(Str,E50),E50<180,!.
into_title_str(Term,Str):- compound(Term), compound_name_arguments(Term,Name,Args),
   %include(not_p1(plain_var),Args,Nonvars),
   Args=Nonvars,
   my_maplist(tersify,Nonvars,ArgsT), into_title_str([Name,"(",ArgsT,")"],Str),!.
into_title_str(Term,Str):- catch(sformat(Str,'~p',[Term]),_,term_string(Term,Str)).

has_short_id(TestID,testid,UUID):- is_valid_testname(TestID),test_id_atom(TestID,UUID).
has_short_id(Obj,object,OID):- is_object(Obj),obj_to_oid(Obj,OID).
has_short_id(Grid,grid,GID):- is_grid(Grid),grid_to_gid(Grid,GID).


is_valid_linkid(ID,testid,TestID):- atom_id(ID,TestID),is_valid_testname(TestID),!.
is_valid_linkid(ID,object,Obj):- known_object(ID,Obj),!.
is_valid_linkid(ID,grid,Grid):- known_grid(ID,Grid),!.
% individuate(complete, two(v_1d398264_trn_0_in, v_1d398264_trn_0_out))
is_valid_linkid(ID,group,Grp):- get_current_test(TestID),is_why_grouped_g(TestID,_Count,ID,Grp).


wqs_c(S):- term_is_ansi(S), !, write_keeping_ansi_mb(S).
wqs_c(S):- (string(S);is_codelist(S);is_charlist(S)),catch(format('~s',[S]),_,writeq(S)).
wqs_c(S):- empty_wqs_c(S),!.
wqs_c(S):- var(S),!,write(var(S)).
wqs_c(S):- atom(S),into_title_str(S,TS),write(TS),!.
wqs_c(S):- atom(S),write_atom(S),!.
%wqs_c(S):- atom(S),write(S),!.
wqs_c(S):- \+compound(S),!,notrace(catch(format('~p',[S]),_,write(S))).
wqs_c(title(S)):- !, wqs_c(S).
wqs_c(H+T):- !, wqs_c(H),write_nbsp,wqs_c(T).
wqs_c(S):- is_grid(S), print_grid(S),!.
wqs_c(S):- is_vm(S), pp(S) ,!.
wqs_c(L):- is_list(L), include(non_empty_wqs_c,L,LL),!,wqs_c_l(LL).
wqs_c([H|T]):- pp([H|T]),!.
wqs_c(H):- callable_arity(H,0),is_writer_goal(H),catch(call_e_dmsg(H),_,fail),!.
%wqs_c(H):- callable_arity(H,0),call(H),!.
wqs_c(H):- locally(t_l:wqs_fb(pp_no_nl),wqs(H)),!.

wqs_c_l([]):-!.
wqs_c_l([H]):- wqs_c(H),!.
wqs_c_l([H|T]):- wqs_c(H),write_nbsp,wqs_c_l(T),!.





ppt(_):- is_print_collapsed,!.
ppt(G):- stack_check_or_call(4000,writeq(G)),!.
ppt(G):- is_vm_map(G), !, write_map(G,'ppt').
ppt(S):- term_is_ansi(S), !, write_keeping_ansi_mb(S).
%ppt(P):- compound(P),wqs1(P),!.

ppt(P):- \+ ansi_main, wants_html,!,ptcol_html(P),write_br.
ppt(P):- \+ \+ ((tersify(P,Q),!,pp(Q))),!.
ppt(Color,P):- \+ ansi_main, wants_html,!,with_color_span(Color,ptcol_html(P)),write_br.
ppt(Color,P):- \+ \+ ((tersify(P,Q),!,pp(Color,Q))),!.


write_br:- ansi_main,!,nl.
write_br:- write('<br>').

ptc(Color,Call):- pp(Color,call(Call)).

:- meta_predicate(ppnl(+)).
ppnl(Term):- is_list(Term),!,g_out(wqs(Term)).
ppnl(Term):- nl_if_needed,format('~q',[Term]),nl_if_needed_ansi.

:- meta_predicate(pp(+)).
pp(Color,P):- \+ ansi_main, wants_html,!,with_color_span(Color,pp(P)),write_br.
pp(Color,P):- ignore((quietlyd((wots_hs(S,pp(P)),!,color_print(Color,S))))).

pp(_):- is_print_collapsed,!.
%pp(Term):- is_toplevel_printing(Term), !, nl_if_needed, pp_no_nl(Term),!,nl_if_needed_ansi.
pp(_Term):- nl_if_needed, fail.
pp(Term):- \+ ansi_main, wants_html,!, wots_vs(SS,ptcol_html_scrollable(Term)),write(SS),write_br.
pp(Term):- \+ nb_current(arc_can_portray,_),!,locally(nb_setval(arc_can_portray,t),print(Term)).
pp(Term):- az_ansi(pp_no_nl(Term)),!,nl_if_needed_ansi.

/*
ptcol(P):- wants_html,!,ptcol_html(P).
ptcol(call(P)):- callable(P),!,call(P).
ptcol(P):- pp(P).
*/

%ptcol_html(P):- ptcol_html_0(P).
ptcol_html(P):- ptcol_html_scrollable_0(P).
ptcol_html_scrollable(P):- with_tag_ats(div,scrollable,ptcol_html_scrollable_0(P)).


ptcol_html_0(P):- with_tag(pre,ptcol_html_wo_pre(P)).
ptcol_html_wo_pre(call(P)):- callable(P),!, in_pp_html(call(P)).
ptcol_html_wo_pre(P):- in_pp_html(print_tree_no_nl(P)).
ptcol_html_scrollable_0(P):- ptcol_html_wo_pre(P).


pp_wcg(G):- wants_html,!,ptcol_html_scrollable(G).
pp_wcg(G):- pp_safe(call((locally(nb_setval(arc_can_portray,t),print(G))))),!.

wqln(Term):- ppnl(Term).
wqnl(G):- pp_safe(call((locally(nb_setval(arc_can_portray,nil),print(G))))),!.

pp_safe(_):- nb_current(pp_hide,t),!.
pp_safe(call(W)):- !, nl_if_needed,nl_now,call(W),nl_now.
pp_safe(W):- nl_if_needed,nl_now,writeq(W),nl_now.
pp_safe(C,W):- color_print(C,call(pp_safe(W))).


%p_p_t_no_nl(Term):- is_toplevel_printing(Term), !, print_tree_no_nl(Term).

p_p_t_no_nl(P):- \+ ansi_main, wants_html,!,ptcol_html(P).
p_p_t_no_nl(Term):- az_ansi(print_tree_no_nl(Term)).

ppt_no_nl(P):- \+ ansi_main, wants_html,!,ptcol_html(P).
ppt_no_nl(P):- tersify(P,Q),!,pp_no_nl(Q).

is_toplevel_printing(_):- \+ is_string_output, line_position(current_output,N),  N<2, fail.

pp_no_nl(P):- var(P),!,pp(var_pt(P)),nop((dumpST,ibreak)).
pp_no_nl(S):- term_is_ansi(S), !, write_keeping_ansi_mb(S).
pp_no_nl(P):- atom(P),atom_contains(P,'~'),!,format(P).
pp_no_nl(G):- is_vm_map(G), !, write_map(G,'pp').
%pp_no_nl(S):- term_is_ansi(S), !, write_keeping_ansi_mb(S).
pp_no_nl(P):- \+ \+ (( pt_guess_pretty(P,GP),ptw(GP))).
%pp(P):-!,writeq(P).
%ptw(P):- quietlyd(p_p_t_nl(P)),!.
%ptw(_):- nl_if_needed,fail.
ptw(P):- var(P),!,ptw(var_ptw(P)),nop((dumpST,ibreak)).
ptw(G):- is_vm_map(G), !, write_map(G,'ptw').
ptw(S):- term_is_ansi(S), !, write_keeping_ansi_mb(S).
ptw(P):- p_p_t_no_nl(P),!.

%ptw(P):- quietlyd(write_term(P,[blobs(portray),quoted(true),quote_non_ascii(false), portray_goal(print_ansi_tree),portray(true)])),!.
print_ansi_tree(S,_):- term_is_ansi(S), !, write_keeping_ansi_mb(S).
print_ansi_tree(P,_):- catch(arc_portray(P),_,(never_let_arc_portray_again,fail)),!.
print_ansi_tree(P,_OL):- catch(p_p_t_no_nl(P),_,(never_let_arc_portray_again,fail)),!.

%p_p_t_nl(T):- az_ansi(print_tree_nl(T)).
%p_p_t(T):- az_ansi(print_tree(T)).

pt_guess_pretty(P,O):- \+ nb_current(in_pt_guess_pretty,t), locally(nb_setval(in_pt_guess_pretty,t),pt_guess_pretty_1(P,O)).
pt_guess_pretty(O,O).

upcase_atom_var_l(IntL,NameL):- upcase_atom_var(IntL,NameL).
upcase_atom_var_l(IntL,NameL):- is_list(IntL),!,my_maplist(upcase_atom_var_l,IntL,NameL).

pt_guess_pretty_1(P,O):- copy_term(P,O,_),
  ignore((sub_term(Body,O), compound(Body), Body=was_once(InSet,InVars),upcase_atom_var_l(InSet,InVars))),
  ignore(pretty1(O)),ignore(pretty_two(O)),ignore(pretty_three(O)),ignore(pretty_final(O)),!,
  ((term_singletons(O,SS),numbervars(SS,999999999999,_,[attvar(skip),singletons(true)]))).

:- dynamic(pretty_clauses:pp_hook/3).
:- multifile(pretty_clauses:pp_hook/3).
:- module_transparent(pretty_clauses:pp_hook/3).
pretty_clauses:pp_hook(FS,Tab,S):- \+ current_prolog_flag(never_pp_hook, true), nb_current(arc_can_portray,t),  notrace(catch(arc_pp_hook(FS,Tab,S),_,fail)).

arc_pp_hook(_,Tab,S):- term_is_ansi(S), !,prefix_spaces(Tab), write_keeping_ansi_mb(S).
%arc_pp_hook(_,Tab,S):- is_vm(S),!,prefix_spaces(Tab),!,write('..VM..').
%arc_pp_hook(_,  _,_):- \+ \+ current_prolog_flag(never_pp_hook, true), nb_current(arc_can_portray,t),!,fail.
arc_pp_hook(FS,_  ,G):- \+ current_prolog_flag(never_pp_hook, true), nb_current(arc_can_portray,t),
  current_predicate(is_group/1),
   locally(b_setval(pp_parent,FS),
     print_with_pad(pp_hook_g(G))),!.

pp_parent(PP):- nb_current(pp_parent,PP),!.
pp_parent([]):-!.

%:- meta_predicate(lock_doing(+,+,0)).
:- meta_predicate(lock_doing(+,+,:)).
lock_doing(Lock,G,Goal):- 
 (nb_current(Lock,Was);Was=[]), !, 
  \+ ((member(E,Was),E==G)),
  locally(nb_setval(Lock,[G|Was]),Goal).

never_let_arc_portray_again:- set_prolog_flag(never_pp_hook, true),!.
arc_can_portray:- \+ current_prolog_flag(never_pp_hook, true), nb_current(arc_can_portray,t).

arcp:will_arc_portray:- 
   \+ current_prolog_flag(never_pp_hook, true), 
   \+ nb_current(arc_can_portray,f),
   %nb_current(arc_can_portray,t), 
   current_prolog_flag(debug,false),
   \+ tracing,
   flag(arc_portray_current_depth,X,X),X<3,
   current_predicate(bfly_startup/0).

user:portray(Grid):- 
  arcp:will_arc_portray, \+ \+ catch(quietly(arc_portray(Grid)),_,fail),!, flush_output.


pp_hook_g(S):- term_is_ansi(S), !, write_keeping_ansi_mb(S).
pp_hook_g(_):- \+ \+ current_prolog_flag(never_pp_hook, true), nb_current(arc_can_portray,t),!,fail.
pp_hook_g(S):- term_contains_ansi(S), !, write_nbsp, pp_hook_g0(S).
pp_hook_g(G):- \+ plain_var(G), lock_doing(in_pp_hook_g,G,pp_hook_g0(G)).

pp_hook_g0(S):- term_is_ansi(S), !, write_nbsp, write_keeping_ansi_mb(S).
pp_hook_g0(_):- \+ \+ current_prolog_flag(never_pp_hook, true), nb_current(arc_can_portray,t),!,fail.
pp_hook_g0(_):- in_pp(bfly),!,fail.
pp_hook_g0(G):- wots_hs(S,in_bfly(f,pp_hook_g10(G))),write(S).

mass_gt1(O1):- into_obj(O1,O2),mass(O2,M),!,M>1.

% Pretty printing
pp_hook_g10(G):- \+ plain_var(G), current_predicate(pp_hook_g1/1), lock_doing(in_pp_hook_g10,G,pp_hook_g1(G)).

%as_grid_string(O,SSS):- is_grid(O),wots_vs(S,print_grid(O)), sformat(SSS,'{  ~w}',[S]).
as_grid_string(O,SSS):- wots_vs(S,show_indiv(O)), sformat(SSS,'{  ~w}',[S]).
as_pre_string(O,SS):- wots_hs(S,show_indiv(O)), strip_vspace(S,SS).


pretty_grid(O):-
  catch(
  (wots_hs(S,print_grid(O)),strip_vspace(S,SS),
   ptc(orange,(format('"  ~w  "',[SS])))),
  _,fail),!.
/*
pretty_grid(O):-
  catch(
  (wots_hs(S,print_grid(O)),strip_vspace(S,SS),
   ptc(orange,(format('"  ~w  "',[SS])))),
  _,(never_let_arc_portray_again,fail)).
*/
pp_hook_g1(O):-  plain_var(O), !, fail.

pp_hook_g1(O):-  attvar(O), !, is_colorish(O), data_type(O,DT), writeq('...'(DT)),!.
pp_hook_g1(S):-  term_is_ansi(S), !, write_nbsp, write_keeping_ansi_mb(S).
%pp_hook_g1(S):-  term_contains_ansi(S), !, fail, write_nbsp, write_keeping_ansi_mb(S).
pp_hook_g1(rhs(O)):- write_nbsp,nl,bold_print(print(r_h_s(O))),!.
pp_hook_g1(O):-  is_grid(O), /* \+ (sub_term(E,O),compound(E),E='$VAR'(_)), */ pretty_grid(O).


pp_hook_g1(O):- is_object(O), into_solid_grid(O,G), wots(SS,pretty_grid(G)),write(og(SS)),!.

pp_hook_g1(shape_rep(grav,O)):- is_points_list(O), as_grid_string(O,S), wotsq(O,Q), print(shape_rep(grav,S,Q)),!.
pp_hook_g1(vals(O)):- !, writeq(vals(O)),!.
%pp_hook_g1(grp(O)):- into_solid_grid_strings(grp(O),Str),Str\=@=grp(O),print_term_no_nl(Str),!.
pp_hook_g1(localpoints(O)):- is_points_list(O), as_grid_string(O,S), wotsq(O,Q), print(localpoints(S,Q)),!.
pp_hook_g1(C):- compound(C), compound_name_arguments(C,F,[O]),is_points_list(O), length(O,N),N>2, as_grid_string(O,S), compound_name_arguments(CO,F,[S]), print(CO),!.

pp_hook_g1(O):-  is_points_list(O),as_grid_string(O,S),write(S),!.
pp_hook_g1(O):-  is_real_color(O), color_print(O,call(writeq(O))),!.
pp_hook_g1(O):-  is_colorish(O), data_type(O,DT), writeq('...'(DT)),!.

pp_hook_g1(_):-  \+ in_pp(ansi),!, fail.


pp_hook_g1(Grp):- current_predicate(pp_ilp/1),is_rule_mapping(Grp),pp_ilp(Grp),!.

pp_hook_g1(O):- atom(O), atom_contains(O,'o_'), pp_parent([LF|_]), \+ (LF==lf;LF==objFn), 
  resolve_reference(O,Var), O\==Var, \+ plain_var(Var),!, 
  write_nbsp, writeq(O), write(' /* '), show_indiv(Var), write(' */ ').

pp_hook_g1(O):-  is_object(O),pp_no_nl(O), !.
pp_hook_g1(O):-  is_group(O),pp_no_nl(O), !.

%pp_hook_g1(change_obj(N,O1,O2,Sames,Diffs)):-  showdiff_objects5(N,O1,O2,Sames,Diffs),!.

pp_hook_g1(O):-  is_vm_map(O),data_type(O,DT), writeq('..map.'(DT)),!.
pp_hook_g1(O):-  is_gridoid(O),show_indiv(O), !.
%pp_hook_g1(O):-  O = change_obj( O1, O2, _Same, _Diff),  with_tagged('h5',w_section(object,[O1, O2],pp(O))).
%pp_hook_g1(O):-  O = change_obj( O1, O2, _Same, _Diff), w_section(showdiff_objects(O1,O2)),!.
%pp_hook_g1(O):-  O = change_obj( O1, O2, _Same, _Diff),  w_section(object,[O1, O2],with_tagged('h5',pp(O))).
%pp_hook_g1(O):-  O = diff(A -> B), (is_gridoid(A);is_gridoid(B)),!, p_c_o('diff', [A, '-->', B]),!.
pp_hook_g1(O):-  O = showdiff( O1, O2), !, showdiff(O1, O2).
%pp_hook_g1(O):- compound(O),wqs1(O), !.
pp_hook_g1(O):- \+ compound(O),fail.
pp_hook_g1(G):- '@'(pp_hook_g1a(G),user).

pp_hook_g1a(G):- \+ current_prolog_flag(debug,true),
  current_predicate(pp_hook_g2/1), lock_doing(in_pp_hook_g3,any,pp_hook_g2(G)),!.
pp_hook_g1a(G):- fch(G),!.

%pp_hook_g2(O):- current_predicate(colorize_oterms/2),colorize_oterms(O,C), notrace(catch(fch(C),_,fail)),! .

fch(O):- wqs1(O).
%fch(O):- pp_no_nl(O).
%fch(O):- print(O).
%fch(O):- p_p_t_no_nl(O).

wotsq(O,Q):- wots_hs(Q,wqnl(O)).
has_goals(G):- term_attvars(G,AV),AV\==[].
has_goals(G):- term_variables(G,TV),term_singletons(G,SV),TV\==SV.

maybe_term_goals(Term,TermC,Goals):- 
  term_attvars(Term,Attvars), Attvars\==[],!,
  term_variables(Term,Vars),
  include(not_in(Attvars),Vars,PlainVars),   
  copy_term((Attvars+PlainVars+Term),(AttvarsC+PlainVarsC+TermC),Goals),
  numbervars(PlainVarsC,10,Ten1,[singletons(true),attvar(skip)]),
  numbervars(AttvarsC+Goals,Ten1,_Ten,[attvar(bind),singletons(false)]).

maybe_replace_vars([],SGoals,TermC,SGoals,TermC):-!.
maybe_replace_vars([V|VarsC],SGoals,TermC,RSGoals,RTermC):-
   my_partition(sub_var(V),SGoals,Withvar,WithoutVar),
   Withvar=[OneGoal], 
   freeze(OneGoal,(OneGoal\==null,OneGoal \== @(null))),
   findall(_,sub_var(V,TermC),LL),LL=[_],!,
   subst([WithoutVar,TermC],V,{OneGoal},[SGoalsM,TermCM]),
   maybe_replace_vars(VarsC,SGoalsM,TermCM,RSGoals,RTermC).
maybe_replace_vars([_|VarsC],SGoals,TermC,RSGoals,RTermC):-
  maybe_replace_vars(VarsC,SGoals,TermC,RSGoals,RTermC).


src_sameish(Orig,Find):- copy_term(Orig,COrig),Find=Orig,Orig=@=COrig.

number_vars_calc_goals(Term,SSRTermC,[1|SRSGoals]):- 
  term_singletons(Term,Singles),
  term_attvars(Term,Vars),
  copy_term(Term+Vars+Singles,TermC+VarsC+SinglesC,Goals),
  notrace(catch(numbervars(TermC+Goals,0,_Ten1,[singletons(false),attvar(skip)]),_,fail)),
  sort_goals(Goals,VarsC,SGoals),
  maybe_replace_vars(VarsC,SGoals,TermC,RSGoals,RTermC),
  include(not_sub_var(RSGoals),SinglesC,KSingles),
  length(KSingles,SL),length(VSingles,SL),my_maplist(=('$VAR'('__')),VSingles),
  subst_2L(KSingles,VSingles,[RTermC,RSGoals],[SRTermC,SRSGoals]),
  subst_1L_p2(src_sameish,[
    {dif('$VAR'('__'),RED)}=dif(RED),
    {cbg('$VAR'('__'))}=cbg],
     SRTermC,SSRTermC),!.

number_vars_calc_goals(Term,SRTermC,[2|RSGoals]):-
  term_attvars(Term,AVars),
  copy_term(Term+AVars,TermC+VarsC,GoalsI), 
  term_attvars(GoalsI,GAttvars), copy_term(GoalsI+GAttvars,_+GAttvarsC,GoalsGoals),
  append(GoalsI,GoalsGoals,Goals),
  append([VarsC,GAttvarsC,AVars,GAttvars],SortVars),
  numbervars(TermC+Goals,0,_Ten1,[singletons(false),attvar(bind)]),
  sort_goals(Goals,SortVars,SGoals),
  maybe_replace_vars(SortVars,SGoals,TermC,RSGoals,RTermC),
  subst_1L_p2(src_sameish,[
    {dif('$VAR'('___'),RED)}=dif(RED),
    {cbg('$VAR'('___'))}=cbg],
     RTermC,SRTermC),!.

number_vars_calc_goals(Term,SSRTermC,[3|SRSGoals]):-
  term_singletons(Term,Singles),
  term_attvars(Term,Vars),
  copy_term(Term+Vars+Singles,TermC+VarsC+SinglesC,Goals),
  numbervars(TermC+Goals,0,_Ten1,[singletons(false),attvar(bind)]),
  sort_goals(Goals,VarsC,SGoals),
  maybe_replace_vars(VarsC,SGoals,TermC,RSGoals,RTermC),
  include(not_sub_var(RSGoals),SinglesC,KSingles),
  length(KSingles,SL),length(VSingles,SL),my_maplist(=('$VAR'('__')),VSingles),
  subst_2L(KSingles,VSingles,[RTermC,RSGoals],[SRTermC,SRSGoals]),
  subst(SRTermC,{cbg('_')},cbg,SSRTermC),!.

number_vars_calc_goals(Term,TermC,[4|SGoals]):-
  term_variables(Term,Vars),
  term_attvars(Term,Attvars),
  copy_term(Term+Vars+Attvars,TermC+VarsC+AttvarsC,Goals),
  notrace(catch(numbervars(TermC+Goals,0,_Ten1,[singletons(true)]),_,fail)),
  append([AttvarsC,VarsC,AttvarsC,Vars],Sorted),
  sort_goals(Goals,Sorted,SGoals),!.

number_vars_calc_goals(Term,TermC,[5|SGoals]):-
  term_variables(Term,Vars),
  term_attvars(Term,Attvars),
  copy_term(Term+Vars+Attvars,TermC+VarsC+AttvarsC,Goals),
  numbervars(TermC+Goals,0,_Ten1,[singletons(false),attvar(skip)]),
  append([AttvarsC,VarsC,Attvars,Vars],Sorted),
  sort_goals(Goals,Sorted,SGoals),!.



writeg(Term):- ignore( \+ notrace(catch(once(writeg0(Term);ppa(Term)),E,(pp(E),ppa(Term))))),!.

writeg0(Term):- term_attvars(Term,Attvars),Attvars\==[],!,
  must_det_ll(((number_vars_calc_goals(Term,TermC,Goals),
  writeg5(TermC)),!,
  if_t(Goals\==[],(nl_if_needed, 
    write(' goals='), call_w_pad_prev(3,az_ansi(print_tree_no_nl(Goals))))))),!.

writeg0(Term):- \+ ground(Term), 
 \+ \+ must_det_ll((  
  numbervars(Term,0,_Ten1,[singletons(true),attvar(skip)]), writeg5(Term))).
writeg0(Term):- writeg5(Term),!.

writeg5(X):- is_ftVar(X),!,write_nbsp,write_nbsp,print(X),write_nbsp.
writeg5(N=V):- is_simple_2x2(V),!,print_grid(N,V),writeln(' = '),call_w_pad_prev(2,writeg9(V)).
writeg5(N=V):- is_gridoid(V),!,print_grid(N,V),writeln(' = '),call_w_pad_prev(2,writeg9(V)).
writeg5(N=V):- nl_if_needed,nonvar(N), pp_no_nl(N),writeln(' = '), !, call_w_pad_prev(2,writeg5(V)). 
writeg5(_):- write_nbsp, fail.
writeg5(V):- writeg9(V).

writeg8(X):- is_ftVar(X),!,print(X).
writeg8(X):- var(X),!,print(X).
writeg8(X):- writeq(X).

writeg9(V):- is_simple_2x2(V),!,print_simple_2x2(writeg8,V).
writeg9(V):- is_list(V),nl_if_needed,write('['),!,my_maplist(writeg5,V),write(']').
writeg9(_):- write_nbsp,write(' \t '),fail.
writeg9(X):- is_ftVar(X),!,write_nbsp,write_nbsp,print(X).
writeg9(V):- pp_no_nl(V).


/*
writeg5(V):- is_simple_2x2(V),!,print_simple_2x2(writeg8,V).
writeg5(V):- is_gridoid(V),!,call_w_pad_prev(2,writeg9(V)).
writeg5(V):- is_list(V),nl_if_needed,write('['),my_maplist(writeg5,V),write(']').
*/
arg1_near(Vars,Goal,Nth):- arg(1,Goal,PreSort),nth1(Nth,Vars,E),E==PreSort,!.
arg1_near(_VarsC,Goal,PreSort):- arg(1,Goal,PreSort),!.
arg1_near(_VarsC,Goal,Goal).

sort_goals(Goals,VarsC,SGoals):- predsort(sort_on(arg1_near(VarsC)),Goals,SGoals).

/*

writeg0(Obj):- is_object(Obj),pp(Obj),!.
writeg0(O):- writeg00(O).

writeg00(Term):-
  maybe_term_goals(Term,TermC,Goals),
  writeg00(TermC), call_w_pad(2,writeg00(Goals)),!.
writeg00(N=V):- nl_if_needed,nonvar(N), pp_no_nl(N),writeln(' = '), !, call_w_pad(2,writeg00(V)).
writeg00(O):- compound(O),compound_name_arguments(O,F,[A]),!,call_w_pad(2,((writeq(F),write('('),writeg3(A),write(')')))).
writeg00(S):- term_contains_ansi(S), !, write_keeping_ansi_mb(S).
writeg00([H|T]):- compound(H),H=(_=_), my_maplist(writeg0,[H|T]).
writeg00([H|T]):- is_list(T),call_w_pad(2,((nl,write('['),writeg2(H),my_maplist(writeg0,T),write(']'),nl))).
%writeg0(Term):- \+ ground(Term),!, \+ \+ (numbervars(Term,99799,_,[singletons(true)]),
%   subst(Term,'$VAR'('_'),'$VAR'('_____'),TermO), writeg0(TermO)).
%writeg0(V):- \+ is_list(V),!,writeq(V),nl_now.
writeg00(V):- \+ is_list(V),!,pp(V).
writeg00(X):- call_w_pad(2,pp(X)).

writeg1(N=V):- is_gridoid(V),!,print_grid(N,V),call_w_pad(2,(my_maplist(writeg1,V))).
writeg1(X):- nl_if_needed,writeg2(X),!,write_nbsp,!.
writeg2(S):- term_contains_ansi(S), !, write_keeping_ansi_mb(S).
writeg2(X):- is_ftVar(X),!,print(X).
writeg2(X):- write_term(X,[quoted(true),quote_non_ascii(true),portrayed(false),nl(false),numbervars(true)]),!.
%writeg2(X):- write_term(X,[quoted(true),quote_non_ascii(true),portrayed(false),nl(false),numbervars(false)]),!.
%writeg1(X):- nl_if_needed,writeg(X).
writeg2(S):- term_is_ansi(S), !, write_keeping_ansi_mb(S).
writeg2(X):- writeq(X),!.
writeg3(X):- is_list(X),X\==[],X=[_,_|_],!,writeg(X).
writeg3(X):- writeg2(X).
*/

% Nov 9th, 1989
/*
pp_hook_g1(T):- 
 nb_current('$portraying',Was)
   ->  ((member(E,Was), T==E) -> ptv2(T) ; locally(b_setval('$portraying',[T|Was]),ptv0(T))) 
   ; locally(b_setval('$portraying',[T]),ptv0(T)).
*/

%pp_hook_g(G):- compound(G),ppt(G),!.
%pp_hook_g(G):- ppt(G),!.


strip_vspace(S,Stripped):- string_concat(' ',SS,S),!,strip_vspace(SS,Stripped).
strip_vspace(S,Stripped):- string_concat(SS,' ',S),!,strip_vspace(SS,Stripped).
strip_vspace(S,Stripped):- string_concat('\n',SS,S),!,strip_vspace(SS,Stripped).
strip_vspace(S,Stripped):- string_concat(SS,'\n',S),!,strip_vspace(SS,Stripped).
strip_vspace(S,Stripped):- string_concat('\t',SS,S),!,strip_vspace(SS,Stripped).
strip_vspace(S,Stripped):- string_concat(SS,'\t',S),!,strip_vspace(SS,Stripped).

strip_vspace(S,Stripped):- replace_in_string([" \n"="\n","(   "="(  ","(\n"="( "],S,S2),S2\==S,!,strip_vspace(S2,Stripped).
%strip_vspace(S,Stripped):- split_string(S, "", "\t\r\n", [Stripped]).
strip_vspace(S,S).


print_nl(P):- nl_if_needed, wots_hs(SS,pp_no_nl(P)), maybe_color(SS,P),nl_if_needed.

color_write(S):- term_is_ansi(S), !, write_keeping_ansi_mb(S).
color_write(P):- wots_hs(SS,write(P)), maybe_color(SS,P).

write_keeping_ansi_mb(P):- is_maybe_bold(P,write_keeping_ansi(P)).

is_maybe_bold(P):- sformat(S,'~w',[P]),atom_contains(S,'stOF').

is_maybe_bold(P,G):- is_maybe_bold(P),!, underline_print(bold_print(G)).
is_maybe_bold(_P,G):- call(G).

pp_msg_color(P,C):- compound(P),pc_msg_color(P,C),!.
pp_msg_color(P,C):- must_det_ll(mesg_color(P,C)).
pc_msg_color(iz(P),C):- pp_msg_color(P,C).
pc_msg_color(link(P,_,_),C):- pp_msg_color(P,C).
pc_msg_color(link(P,_),C):- pp_msg_color(P,C).
pc_msg_color((_->P),C):- pp_msg_color(P,C).
pc_msg_color([P|_],C):- pp_msg_color(P,C).
pc_msg_color(diff(P),C):- pp_msg_color(P,C).

%:- meta_predicate(wots_hs(0)).
%wots_hs(G):- wots_hs(S,G),write(S).

:- meta_predicate(wots_ansi(-,0)).
wots_ansi(S,Goal):- wots(S,woto_ansi(Goal)).
:- meta_predicate(wots_ansi(-,0)).
wots_html(S,Goal):- wots(S,woto_html(Goal)).

:- meta_predicate(woto_ansi(0)).
woto_ansi(Goal):- with_toplevel_pp(ansi,Goal).
:- meta_predicate(woto_html(0)).
woto_html(Goal):- with_toplevel_pp(http,Goal).

:- meta_predicate(wots_hs(-,0)).
%wots_hs(S,G):- \+ wants_html,!,wots(S,G).
%wots_hs(S,G):- wots(S,G),!.
wots_hs(S,G):- wots(SS,G),notrace(remove_huge_spaces(SS,S)). 
:- meta_predicate(wots_vs(-,0)).
wots_vs(OOO,G):- wots(S,G),notrace(fix_vspace(S,OOO)).

fix_vspace(S,OOO):-
 strip_vspace(S,SS), (atom_contains(SS,'\n') -> 
   wots_hs(SSS,(nl_now,write('   '),write(SS),nl_now));SSS=SS),
   remove_huge_spaces(SSS,OOO).


write_tall(L):- is_list(L),!,my_maplist(write_tall,L).
write_tall(E):- wots_vs(S,wqs_c(E)),writeln(S).
write_wide(L):- is_list(L),!,my_maplist(write_wide,L).
write_wide(E):- wots_vs(S,wqs_c(E)),write(S),write_nbsp.

p_to_br(S,SS):- fix_br_nls(S,S0),
  cr_to_br(S0,SSS),
  replace_in_string(['<p>'='<br>','<br/>'='<br>','</p>'=' ','<p/>'='<br>','<br><br>'='<br>'],SSS,SSSS),
  cr_to_br(SSSS,SS).

cr_to_br(S,SSS):- wants_html,!,cr_to_br_html(S,SSS).
cr_to_br(S,SSS):- cr_to_br_ansi(S,SSS).

cr_to_br_html(S,SSS):- replace_in_string(['\r\n'='<br>','\r'='<br>','\n'='<br>'],S,SSS).
cr_to_br_ansi(S,SSS):- replace_in_string(['<br>'='\n','&nbsp;'=' '],S,SSS).

fix_br_nls(S,O):- replace_in_string(
 ['<br/>\n'='<br/>','<br>\n'='<br>','</p>\n'='</p>','<p/>\n'='<p/>','<p>\n'='<p>',
  '\n<br>'='<br>','\n<br/>'='<br/>','\n</p>'='</p>','\n<p/>'='<p/>','\n<p>'='<p>'],S,O).

remove_huge_spaces(S,O):- notrace((fix_br_nls(S,SS),!,p_to_br(SS,O))),!.
/*
remove_huge_spaces(S,O):- fix_br_nls(S,S0),
  replace_in_string(['          '='     ',
    '                                                                          '='     ',
    '                                                                          '='     ',
    '                                                                                                                                                                                                                                                                                                                                                                                                               '='  ',
    '                                                                                                                                                                                                                                                                                   '='   ',
    '                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        '='    ',
    '                                                                          '='     ',
    '\t'='  ',
    '                         '='     '],S0,SS),p_to_br(SS,O).
*/


wqs_l(H):- \+ is_list(H),!, wqs(H).
wqs_l(H):- wqs(H).

wqs(P):- wots_hs(SS,wqs0(P)), maybe_color(SS,P).
wqs(C,P):- ansicall(C,wqs0(P)),!.

wqs0(X):- plain_var(X), !, wqs(plain_var(X)), ibreak.
wqs0(S):- term_is_ansi(S), !, write_keeping_ansi_mb(S).
wqs0(C):- is_colorish(C),color_print(C,C),!.
wqs0(G):- is_vm_map(G), !, write_map(G,'wqs').
wqs0(X):- var(X), !, get_attrs(X,AVs),!,writeq(X),write('/*{'),print(AVs),write('}*/').
wqs0(X):- attvar(X), !, wqs(attvar(X)). 
wqs0(nl_now):- !, nl_now. wqs0(X):- X=='', !. wqs0(X):- X==[], !. 
wqs0(X):- is_grid(X), !, print_grid(X).
wqs0(G):- compound(G), G = call(C),callable(C),!,call(C).
wqs0([T]):- !, wqs(T).
wqs0([H|T]):- string(H), !, write(H), write_nbsp, wqs(T).
wqs0([H|T]):- compound(H),skip(_)=H, !,wqs(T).
wqs0([H|T]):- wqs(H), need_nl(H,T), wqs(T), !.
wqs0(X):- is_object(X), tersify1(X,Q), X\==Q,!, wqs(Q).
wqs0(X):- is_object(X), show_shape(X),!.
wqs0(X):- string(X), atom_contains(X,'~'), catch((sformat(S,X,[]),color_write(S)),_,fail),!.
wqs0(X):- string(X), !, color_write(X).
%wqs([H1,H2|T]):- string(H1),string(H2),!, write(H1),write_nbsp, wqs([H2|T]).
%wqs([H1|T]):- string(H1),!, write(H1), wqs(T).
%wqs([H|T]):- compound(H),!, writeq(H), wqs(T).

wqs0(call(C)):- !, call(C).
wqs0(X):- \+ compound(X),!, write_nbsp, write(X).
wqs0(C):- compound(C),wqs1(C),!.
wqs0(C):- wqs2(C).
%wqs(S):- term_contains_ansi(S), !, write_nbsp, write_keeping_ansi_mb(S).

wqs2(S):- term_contains_ansi(S), !, write_nbsp, write_keeping_ansi_mb(S).
%wqs2(P):- wants_html,!,pp(P).

:- thread_local(t_l:wqs_fb/1).
wqs2(X):- t_l:wqs_fb(P1),call(P1,X),!.
%wqs2(X):- with_wqs_fb(writeq,X).
wqs2(X):- with_wqs_fb(writeq,print(X)),!.
%wqs2(X):- with_wqs_fb(writeq,((write_nbsp,write_term(X,[quoted(true)])))).

with_wqs_fb(FB,Goal):-
  locally(t_l:wqs_fb(FB),Goal).


as_arg_str(C,S):- wots_vs(S,print(C)).

arg_string(S):- string(S),!.
arg_string(S):- term_contains_ansi(S),!.

wqs1(C):- \+ compound(C),!,wqs0(C).
wqs1(S):- term_is_ansi(S), !, write_keeping_ansi_mb(S).

wqs1(format(C,N)):- catch((sformat(S,C,N),color_write(S)),_,fail),!.
wqs1(writef(C,N)):- !, writef(C,N).
wqs1(q(C)):-  \+ arg_string(C),wots_hs(S,writeq(C)),color_write(S),!.
wqs1(g(C)):-  \+ arg_string(C),wots_vs(S,bold_print(wqs1(C))),print(g(S)),!.
wqs1(print_ss(C)):-  \+ arg_string(C), wots_vs(S,print_ss(C)),wqs1(print_ss(S)),!.
wqs1(b(C)):-  \+ arg_string(C), wots_vs(S,bold_print(wqs1(C))),color_write(S).
wqs1(norm(C)):- writeq(norm(C)),!.
wqs1(grid_rep(norm,C)):- writeq(grid_rep(norm,C)),!.
wqs1(grid(C)):- writeq(grid(C)),!.
wqs1(rhs(RHS)):- nl_now,wqnl(rhs(RHS)),nl_now.
%wqs1(grid_ops(norm,C)):- writeq(norm(C)),!.
%norm_grid

wqs1(pp(P)):-  wots_vs(S,pp_no_nl(P)),write((S)).
wqs1(ppt(P)):- wots_vs(S,ppt_no_nl(P)),write((S)).
wqs1(wqs(P)):- wots_vs(S,wqs(P)),write((S)).
wqs1(wqs(C,P)):- wots_vs(S,wqs(P)),color_print(C,S).

wqs1(vals(C)):- writeq(vals(C)),!.
%wqs1(colors_cc(C)):- \+ arg_string(C), as_arg_str(C,S),wqs(colorsz(S)).
wqs1(io(C)):-  \+ arg_string(C),wots_vs(S,bold_print(wqs(C))),write(io(S)).

wqs1(uc(C,W)):- !, write_nbsp, color_print(C,call(underline_print(format("\t~@",[wqs(W)])))).
wqs1(cc(C,N)):- is_color(C),!,color_print(C,call(writeq(cc(C,N)))).
wqs1(write_nav_cmd(C,N)):- !, write_nav_cmd(C,N).

wqs1(-(C,N)):- is_color(C),!,color_print(C,call(writeq(C))), write('-'), wqs(N).
wqs1(cc(C,N)):- N\==0,attvar(C), get_attrs(C,PC), !, wqs(ccc(PC,N)).
wqs1(cc(C,N)):- N\==0,var(C), sformat(PC,"~p",[C]), !, wqs(ccc(PC,N)).
wqs1(cc(C,N)):- \+ arg_string(C), wots_hs(S,color_print(C,C)), wqs(cc(S,N)).
wqs1(color_print(C,X)):- is_color(C), !, write_nbsp, color_print(C,X).
wqs1(color_print(C,X)):- \+ plain_var(C), !, write_nbsp, color_print(C,X).
wqs1(X):- into_f_arg1(X,_,Arg),is_gridoid(Arg),area_or_len(Arg,Area),Area<5,writeq(X),!.
% wqs1(C):- callable(C), is_wqs(C),wots_vs(S,catch(C,_,fail)),write((S)).
wqs1(X):- is_gridoid_arg1(X), print_gridoid_arg1(X).

into_f_arg1(X,F,Arg):- compound(X), compound_name_arguments(X,F,[Arg]), compound(Arg).

is_gridoid_arg1(X):- into_f_arg1(X,_F,Arg),is_gridoid(Arg).
print_gridoid_arg1(X):- into_f_arg1(X,F,Arg),print_gridoid_arg1(F,Arg).

print_gridoid_arg1(F,Arg):- \+ wants_html,!, wots_vs(VS,wqs(Arg)), writeq(F),write('(`'),!, print_with_pad(write(VS)),write('`)').
print_gridoid_arg1(F,Arg):- wots_vs(VS,wqs(Arg)),
 with_tag_style(span,"display: inline; white-space: nowrap",(writeq(F),write('({'),!,write(VS),write('})'))).


nl_needed(N):- line_position(current_output,L1),L1>=N.

nl_now :- wants_html,!,nl_if_needed_ansi.
nl_now :- nl.

ansi_in_pre:- current_predicate(in_pre/0),in_pre.
nl_if_needed :- ansi_main,!, format('~N').
nl_if_needed :- ansi_in_pre,ignore((nl_needed(11),write('<br/>'))),!.
nl_if_needed :- wants_html,!,ignore((nl_needed(11),write('<br/>\n'))).
nl_if_needed :- format('~N').
nl_if_needed_ansi :- \+ ansi_main, wants_html,!.
nl_if_needed_ansi :- nl_if_needed.

write_nbsp:- ansi_main,!,write(' ').
write_nbsp:- wants_html,!,write('&nbsp;').
write_nbsp:- write(' ').

is_breaker(P):- compound(P),functor(P,_,A), A>=3.

last_f(H,F):- \+ compound(H),data_type(H,F).
last_f(H,F/A):- compound(H),!,functor(H,F,A).

need_nl(_,_):- line_position(current_output,L1),L1<40,!.
need_nl(_,_):- line_position(current_output,L1),L1>160,!,nl_if_needed.
need_nl(H0,[H1,H2|_]):- H1\=cc(_,_), last_f(H0,F0),last_f(H1,F1),last_f(H2,F2), F0\==F1, F1==F2,!,format('~N  ').
%need_nl(H0,[H1|_]):- last_f(H0,F0),last_f(H1,F1), F0==F1, !, write_nbsp.
need_nl(_,_).
/*
need_nl(_Last,[H|_]):- last_f(H,F), 
 once(nb_current(last_h,cc(LF,C));(LF=F,C=0)), 
   (LF==F-> (write_nbsp, plus(C,1,CC), nb_setval(last_h,cc(F,CC))) ; ((C>2 -> nl_now ; write_nbsp), nb_setval(last_h,cc(F,0)))).

need_nl(_,_):- wants_html,!,write_nbsp.
%need_nl(_,_):- !,write_nbsp.
need_nl(H,[P|_]):- \+ is_breaker(H),is_breaker(P),line_position(user_output,L1),L1>80,nl_now,bformatc1('\t\t').
need_nl(_,_):- line_position(user_output,L1),L1>160,nl_now,bformatc1('\t\t').
need_nl(_,_).
*/

dash_chars:- wants_html,!,section_break.
dash_chars:- dash_chars(40),!.

dash_chars(_):- wants_html,!,section_break.
dash_chars(H):- integer(H), dash_border(H).
dash_chars(S):- nl_if_needed,dash_chars(60,S),nl_if_needed_ansi.
dash_chars(_,_):- wants_html,!,section_break.
dash_chars(H,_):- H < 1,!.
dash_chars(H,C):- forall(between(0,H,_),bformatc1(C)).

%section_break:- wants_html,!,write('<p><hr></p>').
section_break.
%dash_uborder_no_nl_1:-  line_position(current_output,0),!, bformatc1('\u00AF\u00AF\u00AF ').
%dash_uborder_no_nl_1:-  line_position(current_output,W),W==1,!, bformatc1('\u00AF\u00AF\u00AF ').
dash_uborder_no_nl_1:- bformatc1('\u00AF\u00AF\u00AF ').
dash_uborder_no_nl_1:- uborder(Short,Long),!, bformatc1(Short),bformatc1(Long),write_nbsp.
dash_uborder_no_nl(1):- !, dash_uborder_no_nl_1.
dash_uborder_no_nl(Width):- WidthM1 is Width-1, uborder(Short,Long),write_nbsp, write(Short),dash_chars(WidthM1,Long),!.
dash_uborder_no_nl(Width):- WidthM1 is Width-1, write_nbsp, bformat('\u00AF'),dash_chars(WidthM1,'\u00AF\u00AF'),!.
dash_uborder_no_nl(Width):- nl_if_needed, WidthM1 is Width-1, bformatc1(' \u00AF'),dash_chars(WidthM1,'\u00AF\u00AF').

dash_uborder(Width):- nl_if_needed,dash_uborder_no_nl(Width),nl_now.

uborder('-','--'):- stream_property(current_output,encoding(utf8)),!.
uborder('\u00AF','\u00AF\u00AF'):- !. %stream_property(current_output,encoding(text)).
%uborder('-','--').

dash_border_no_nl_1:-  line_position(current_output,0),!, bformatc1(' ___ ').
dash_border_no_nl_1:-  line_position(current_output,W),W==1,!, bformatc1('___ ').
dash_border_no_nl_1:- bformatc1(' ___ ').

%dash_border_no_nl(Width):- write(''),dash_chars(Width,'_'),write_nbsp,!.

dash_border_no_nl(Width):- nl_if_needed, WidthM1 is Width-1, bformatc1(' _'),dash_chars(WidthM1,'__').

dash_border(Width):- !, dash_border_no_nl(Width),nl_now,!.

functor_test_color(pass,green).
functor_test_color(fail,red).
functor_test_color(warn,yellow).

arcdbg(G):- is_vm_map(G), !, write_map(G,'arcdbg').
arcdbg(G):- compound(G), compound_name_arity(G,F,_),functor_test_color(F,C),
  wots_hs(S,print(G)),color_print(C,S),!,nl_if_needed_ansi.
arcdbg(G):- u_dmsg(G).


%user:portray(Grid):- ((\+ tracing, is_group(Grid),print_grid(Grid))).
%user:portray(Grid):- quietlyd((is_object(Grid),print_grid(Grid))).
n_times(N,Goal):- forall(between(1,N,_),ignore(Goal)).
banner_lines(Color):- banner_lines(Color,1).
banner_lines(Color,N):- wants_html,!,format('\n<hr style="border: ~wpx solid ~w">\n',[N,Color]),!.
banner_lines(Color,N):- 
 must_det_ll((nl_if_needed,
  n_times(N,color_print(Color,'-------------------------------------------------')),nl_now,
  n_times(N,color_print(Color,'=================================================')),nl_now,
  n_times(N,color_print(Color,'-------------------------------------------------')),nl_now,
  n_times(N,color_print(Color,'=================================================')),nl_now,
  n_times(N,color_print(Color,'-------------------------------------------------')),nl_now)),!.

print_sso(A):- ( \+ compound(A) ; \+ (sub_term(E,A), is_gridoid(E))),!, u_dmsg(print_sso(A)),!.
print_sso(A):- grid_footer(A,G,W),writeln(print_sso(W)), print_grid(W,G),!.
print_sso(A):- must_det_ll(( nl_if_needed, into_ss_string(A,SS),!,
  SS = ss(L,Lst),
  writeln(print_sso(l(L))), 
  forall(member(S,Lst),writeln(S)),nl_if_needed)),!.

var_or_number(V):- var(V),!.
var_or_number(V):- integer(V),!.

caret_to_list(Test=(A^B),OUT):- caret_to_list((Test=A)^B,OUT).
caret_to_list(A^B,[A,B]):-B\=(_^_), !.
caret_to_list(A^B,[A|BB]):-!,caret_to_list(B,BB).
caret_to_list(A,[A]).

list_to_caret(A,A):- is_grid(A),!.
list_to_caret([A],A):-!.
list_to_caret([A,B],A^B):-!.
list_to_caret([A|More],A^B):-!,list_to_caret(More,B).

print_ss(G1,G2):- print_side_by_side([G1,G2]).
print_ss(IH,IV,NGrid):- var_or_number(IH),var_or_number(IV),!, \+ \+ print_grid(IH,IV,NGrid).
print_ss(A,B,C):- \+ \+ print_side_by_side(A,B,C),!.
%print_ss(Color,G1,WG,G2):- is_color(Color),var_or_number(WG),!,print_side_by_side(Color,G1,WG,G2).
print_ss(IH,IV,Title,NGrid):- must_det_ll((var_or_number(IH),var_or_number(IV))),!,print_grid(IH,IV,Title,NGrid).
print_ss(A,B,C,D,E):- print_side_by_side(A,B,C,D,E).
print_ss(A,B,C,D,E,F):- print_side_by_side(A,B,C,D,E,F).


g_display(I,[O]):- is_grid(I),!,I=O.
g_display(I,[O]):- is_points_list(I),!,I=O.
g_display(I,[O]):- is_list(I), member(was_oid(OID),I), oid_to_obj(OID,Obj),!,g_display2(Obj,O).
g_display(obj(I),[O]):- member(was_oid(OID),I), oid_to_obj(OID,Obj),!,g_display2(Obj,O).
g_display(I,O):- is_list(I),!, my_maplist(g_display2,I,O).
g_display(I,[O]):- g_display2(I,O).

g_display2(I,O):- is_grid(I),!,I=O.
g_display2(N=I,N=O):- g_display2(I,O),!.
g_display2(I,O):- is_points_list(I),!,I=O.
%g_display2(I,O):- is_list(I),!, my_maplist(g_display2,I,O).
g_display2(I,Desc=G):- vis2D(I,H,V), object_grid_g_display2(I,H,V,G,Desc),!.
g_display2(I,print_grid(H,V,[I])):- sub_term(E,I),compound(E),E=globalpoints(_O),grid_size(I,H,V),!.
g_display2(I,I):- !. 

object_grid_g_display2(I,H,V,G,Desc):- (H=<3;V=<3), !, global_grid(I,GPs),object_glyph(I,Glyph),mapgrid(add_g_texture(Glyph),GPs,G),obj_to_oid(I,Desc).

object_grid_g_display2(I,H,V,G,Desc):- object_grid(I,GPs),!, object_glyph(I,Glyph),mapgrid(add_g_texture(Glyph),GPs,G),  
  obj_to_oid(I,GOID), loc2D(I,X,Y), 
  nop((Desc = obj(at(X,Y),viz(H,V),GOID))),
  Desc = GOID.

add_g_texture(Glyph,G,Glyph-G):- is_real_color(G),!.
add_g_texture(_,G,G).

%print_sso(G,W):- is_gridoid(G),!,must_det_ll(print_grid(W,G)).

print_ss(VAR):- print_side_by_side(VAR),!.

%print_ss(Title):- is_gridoid(Title),print_side_by_side([Title]).

:- meta_predicate(print_side_by_side(+)).

print_side_by_side(V):- var(V),!, writeq(print_side_by_side(V)).
print_side_by_side(g(H,V,Grid)):- nonvar(Grid),!,print_grid(H,V,Grid).
print_side_by_side(A^B):- caret_to_list(A^B,List),print_side_by_side(List),!.
print_side_by_side(call(P)):- !, call(P,Ret),!,print_side_by_side_l(1,Ret).
print_side_by_side(List):- is_obj_props(List),!,wqs(List).
print_side_by_side(G):- is_grid(G),!,print_grid(G).
print_side_by_side([G|L]):- is_grid(G),maplist(is_grid,L),!,print_side_by_side_l(1,[G|L]).
print_side_by_side(G):- is_object(G),!,print_grid([G]).
print_side_by_side(Title=Value):- !, format('~N'),print_title(Title),write('==>'),print_side_by_side(Value).
print_side_by_side(GF):- grid_footer(GF,G,W),is_gridoid(G),!,print_grid(W,G).
%print_side_by_side(Title=(A^B)):- print_side_by_side((Title=A)^B).

print_side_by_side(P):- is_list(P), print_side_by_side_l(1,P), !,nop((length(P,PL),PL>5,writeln(pss=PL))).
print_side_by_side(P):- \+ is_list(P), !, ignore((print_side_by_side_l(1,[P]))),!.

/*
print_side_by_side(P):- \+ is_list(P), compound(P), callable_arity(P,0), \+ callable_arity(P,1), !,p
rint_side_by_side_l(1,call(P)).
print_side_by_side(P):- \+ is_list(P), compound(P), callable_arity(P,1), \+ callable_arity(P,0), call(P,Ret),!,print_side_by_side_l(1,Ret).
*/

print_side_by_side(List):- pp(List),!.

print_side_by_side_l(N,Nil):- Nil == [], !, (N<2->writeln('Nil');true).
print_side_by_side_l(N,List):- fail, member(Size,[15,10,8,6,4,3]), once((length(Left,Size), append(Left,Rest,List), reverse(Left,RLeft),
   sorted_by_vertical_size(RLeft), list_print_length(RLeft,Len))), Len < 200, !, print_side_by_side_three(N,Left), print_side_by_side_l(N,Rest).
%print_side_by_side([A,B,C,D|Rest]):- wots_hs(AB,print_side_by_side(A,B)),wots_hs(AC,print_side_by_side(C,D)),!,print_side_by_side(AB,AC),print_side_by_side(Rest).
%print_side_by_side([A,B,C|Rest]):- wots_hs(AB,print_side_by_side(A,B)),wots_hs(AC,print_grid(C)),print_side_by_side(AB,AC),!,print_side_by_side(Rest).

print_side_by_side_l(N,List):- wants_html,!,print_card_list(N,List).
print_side_by_side_l(N,[A,B,C|Rest]):- Left=[A,B,C], print_side_by_side_three(N,Left), !,nminus( N3, N ,3), print_side_by_side_l(N3,Rest).
print_side_by_side_l(N,[A,B|Rest]):- print_side_by_side(A,B),nl_if_needed,!, nminus( N2, N ,2),print_side_by_side_l(N2,Rest),!.
print_side_by_side_l(N,[A|Rest]):- print_grid(N,A), nminus( N1, N ,1), print_side_by_side_l(N1,Rest),!.

vertical_grid_size_with_key(Grid-N,V+H+N+F):- always_grid_footer(Grid,GG,F),grid_size(GG,H,V).


sorted_by_vertical_size(List):- sort_by_vertical_size(List,Sorted),!,List=@=Sorted.
sort_by_vertical_size(List,Sorted):- lists:number_list(List, 1, Numbered),
  predsort_on(vertical_grid_size_with_key,Numbered,SortedKeys),my_maplist(arg(1),SortedKeys,Sorted),!.

grid_with_footer_string(N,C,CGS):- always_grid_footer(C,CG,CF),wots_vs(CGS,print_grid(CF:N,CG)).

print_side_by_side_three(N,List):- \+ wants_html, wants_html, !, with_toplevel_pp(http,print_side_by_side(N,List)).
print_side_by_side_three(N,List):- wants_html,!,print_card_list(N,List).

print_side_by_side_three(_,[]):-!.
print_side_by_side_three(N,[B|C]):-  nminus( N1, N ,1), print_ss(N=B),!, print_side_by_side_three(N1,C).
print_side_by_side_three(N,[B,C]):- !, 
  nminus( N1, N ,1),
  grid_with_footer_string(N,B,BGS),
  grid_with_footer_string(N1,C,CGS),
  print_side_by_side0(BGS,_,CGS).



  %print_side_by_side_three(N,[L|List]):- is_object(L), print_side_by_side([L|List]),!, % length(List,L),
%  nop((length(Left,5),append(Left,Rest,List),length(Rest,NN),NN>1,!, 
%  print_side_by_side_three(N,[L|Left]),print_side_by_side_three(N,Rest))),!.
print_side_by_side_three(N,[A|BC]):-  
  grid_with_footer_string(N,A,AGS),
  nminus(N1, N , 1), 
  wots_vs(BCGS,print_side_by_side_three(N1,BC)),
  print_side_by_side0(AGS,_,BCGS),!.


nminus(N2,N,R):- number(N), N2 is N + R,!.
nminus(N2:M,N:M,R):- number(N), N2 is N + R,!.
nminus(N:R,N,R).

g_nonvar(A,AA):- nonvar(A);nonvar(AA).

%print_side_by_side2(A,B):- (unsized_grid(A);unsized_grid(B)),!, print_sso(A),print_sso(B),!.
%print_side_by_side2(A,B):- g_smaller_than(A,B),!, print_sso(A),print_sso(B),!.
%print_side_by_side2(A,B):-  print_sso(A),print_sso(B),!.
always_grid_footer(A,GG,FF):- grid_footer(A,GG,GF),!,into_wqs_string(GF,FF).
always_grid_footer(A,A,"").

ensure_grid_footer(A,GGGF):- always_grid_footer(A,GG,GF),add_grid_label(GG,GF,GGGF).

is_wqs_f(A):- \+ atom(A),!,fail. is_wqs_f(wqs). is_wqs_f(print_grid). is_wqs_f(format). is_wqs_f(call). 
is_wqs_f(A):- is_wqs_ff(PP),atom_concat(PP,_,A). is_wqs_ff('print'). is_wqs_ff('pp'). is_wqs_ff('wq'). is_wqs_ff('write').
is_wqs(M):-compound(M),compound_name_arity(M,F,_),is_wqs_f(F),!.
into_wqs(M,WQS):- is_wqs(M),!,WQS=M.
into_wqs(M,wqs(C,ppt(M))):- pp_msg_color(M,C),!.

into_wqs_string(S,S):- term_is_ansi(S), !.
into_wqs_string(N1,NS1):- compound(N1), N1 = (A + B), !, into_wqs_string(A,AS),into_wqs_string(B,BS),sformat(NS1,'~w ~w',[AS,BS]).
into_wqs_string(N1,NS1):- string(N1),!,NS1=N1.
into_wqs_string(N1,NS1):- into_wqs(N1,WQS1),wots_vs(NS1,call(WQS1)).

add_grid_label(I,M,IM):- into_wqs_string(M,WQS),IM=..[-,I,WQS]. 

print_side_by_side(G1N1,G2N2):-
   always_grid_footer(G1N1,G1,N1),
   always_grid_footer(G2N2,G2,N2),
   pp_msg_color(N1,TitleColor),!,
   print_side_by_side_pref(TitleColor,G1,N1,_LW,G2,N2),!.

%print_ss(A,B,C):- is_color(A),!,print_side_by_side(A,B,C).
%print_ss(A,B,C):-  is_grid(B), \+ is_gridoid(C),!, print_grid(wqs(A,C),B).
print_side_by_side(Info,G1N1,G2N2):- is_gridoid(G1N1),!,
  print_side_by_side_msg(Info,G1N1,G2N2).
print_side_by_side(TitleColor,G1N1,G2N2):- is_color(TitleColor),!,
  always_grid_footer(G1N1,G1,N1),
  always_grid_footer(G2N2,G2,N2),  
  print_side_by_side_pref(TitleColor,G1,N1,_LW,G2,N2).
print_side_by_side(X,Y,Z):- (var(X);number(X)),!, g_out((nl_now,print_side_by_side0(X,Y,Z))),!.

print_side_by_side(Info,G1N1,G2N2):- 
  print_side_by_side_msg(Info,G1N1,G2N2).

print_side_by_side_msg(Info,G1N1,G2N2):-
  always_grid_footer(G1N1,G1,N1),
  always_grid_footer(G2N2,G2,N2),
  pp_msg_color(Info,TitleColor),
  into_wqs_string(Info,String),
  print_side_by_side_pref(TitleColor,G1,String+N1,_LW,G2,N2),
  nop(pp(info(Info))).

:- meta_predicate(print_side_by_side(+,+,+,+,+)).
print_side_by_side(TitleColor,G1,N1,G2,N2):- 
  print_side_by_side_pref(TitleColor,G1,N1,_LW,G2,N2).

:- meta_predicate(print_side_by_side(+,+,+,?,+,+)).
print_side_by_side(TitleColor,G1,N1,LW,G2,N2):- 
  print_side_by_side_pref(TitleColor,G1,N1,LW,G2,N2).

:- meta_predicate(print_side_by_side_pref(+,+,+,?,+,+)).


print_side_by_side_pref(TitleColor,G1,N1,LW,G2,N2):- wants_html,!,  print_ss_html(TitleColor,G1,N1,LW,G2,N2).
print_side_by_side_pref(TitleColor,G1,N1,LW,G2,N2):- print_side_by_side_ansi(TitleColor,G1,N1,LW,G2,N2),!.


print_side_by_side_ansi(TitleColor,G1,N1,LW0,G2,N2):-
 must_det_ll((
   data_type(G1,S1), data_type(G2,S2),
   print_side_by_side0(G1,LW0,G2),!,
   print_side_by_side_footer(TitleColor,S1,N1,LW0,S2,N2))).
   %write(F1),write(' '),write(F2))),!.
   /*
   print_grid(,G1),
   print_grid(format_footer(TitleColor,NS2,S2),G2),
   %print_side_by_side0(G1,LW,G2),
   !)). %print_side_by_side_footer(TitleColor,S1,NS1,LW,S2,NS2))).
*//*
print_side_by_side_ansi(TitleColor,G1,N1,_LW,G2,N2):-
   g_out((nl_now,
   data_type(G1,S1), data_type(G2,S2),
   into_wqs_string(N1,NS1), into_wqs_string(N2,NS2),
   %print_side_by_side0(G1,LW,G2),
   print_grid(format_footer(TitleColor,NS1,S1),G1),
   print_grid(format_footer(TitleColor,NS2,S2),G2),
   %print_side_by_side0(G1,LW,G2),
   !)). %print_side_by_side_footer(TitleColor,S1,NS1,LW,S2,NS2))).
*/
% SWAP
print_side_by_side_footer(TitleColor,S1,N1,LW0,S2,N2):- number(LW0), LW0 < 0, LW is -LW0, !, 
   print_side_by_side_footer(TitleColor,S2,N2,LW,S1,N1).

print_side_by_side_footer(TitleColor,S1,N1,_LW,S2,N2):- 
%   into_wqs_string(N1,NS1), into_wqs_string(N2,NS2),
%   wots(F1,format_footer(TitleColor,NS1,S1)),wots(F2,format_footer(TitleColor,NS2,S2)),
   nl_if_needed, write('\t'),format_footer(TitleColor,N1,S1),write('\t\t'),format_footer(TitleColor,N2,S2),write('\n'),!.


unsized_grid(A):- is_gridoid(A),!,fail.
unsized_grid(A):- grid_footer(A,Grid,_Text), !, \+ is_gridoid(Grid),!.
unsized_grid(A):- \+ is_gridoid(A),!.

grid_footer(G,_,_):- \+ compound(G),!,fail.
grid_footer(GFGG:M,GG,GF:M):-grid_footer(GFGG,GG,GF),!.
grid_footer((GF=GG),GG,GF):- !, is_gridoid(GG).
grid_footer([GFGG],GG,GF):- compound(GFGG),(GFGG=(GF=GG)),grid_footer(GF=GG,GG,GF).
grid_footer(Obj,GG,GF):- fail, is_object(Obj), %vis2D(Obj,H,V),localpoints(Obj,Ps),points_to_grid(H,V,Ps,GG), 
  global_grid(Obj,GG),
  object_ref_desc(Obj,GF),!.
%grid_footer(GF,GG,wqs(GF)):- is_obj_props(GF),!,contains_enough_for_print(GF,GG),!.
grid_footer(print_grid(GF,GG),GG,GF):-!.
grid_footer(print_grid(_,_,GF,GG),GG,GF):-!.
grid_footer((GG-GF),GG,GF):- is_grid(GG), !.
grid_footer((GF-GG),GG,GF):- is_grid(GG), !.
grid_footer(print_ss(GGFF),GG,GF):- !,grid_footer(GGFF,GG,GF).
grid_footer((GG-GF),GG,GF):- is_gridoid(GG), !.
grid_footer((GF-GG),GG,GF):- is_gridoid(GG), !.
grid_footer((GG-wqs(GF)),GG,wqs(GF)):- nonvar(GF),!.
grid_footer((GF-GG),[['?']],GF):- GG==[], !.
grid_footer((GG-GF),[['?']],GF):- GG==[], !.
grid_footer((GF-GG),GG,GF):- \+ is_gridoid(GF), !.
grid_footer((GG-GF),GG,GF):- \+ is_gridoid(GF), !.
grid_footer((GG-GF),GG,GF).


g_smaller_than(A,B):- grid_footer(A,AA,_),!,g_smaller_than(AA,B).
g_smaller_than(B,A):- grid_footer(A,AA,_),!,g_smaller_than(B,AA).
g_smaller_than(A,B):- is_gridoid(A),is_gridoid(A),!, vis2D(A,_,AV),vis2D(B,_,BV), BV>AV.

gridoid_size(G,30,30):- \+ compound(G),!.
gridoid_size(print_grid(H,V,_),H,V):- nonvar(H),nonvar(V),!.
gridoid_size(print_grid(H,V,_,_),H,V):- nonvar(H),nonvar(V),!.
gridoid_size(print_grid0(H,V,_),H,V):- nonvar(H),nonvar(V),!.
gridoid_size(print_grid0(H,V,_,_),H,V):- nonvar(H),nonvar(V),!.
gridoid_size(G,H,V):- compound_name_arity(G,print_grid,A),arg(A,G,GG),gridoid_size(GG,H,V).
gridoid_size(G,H,V):- is_gridoid(G),!,grid_size(G,H,V).

print_side_by_side0([],_,[]):-!.
%print_side_by_side0(A,_,B):- (unsized_grid(A);unsized_grid(B)),!, writeln(unsized_grid), print_sso(A),print_sso(B),!.
% % % print_side_by_side0(A,_,B):- g_smaller_than(A,B),!, writeln(g_smaller_than), print_sso(A),print_sso(B),!.
/*
print_side_by_side0(C1-call(wqs(S1)),LW,C2-call(wqs(S2))):- nonvar(S1),!,
  print_side_by_side0(C1,LW,C2),nl_if_needed,
  print_side_by_side0(wqs(S1),LW,wqs(S2)).
print_side_by_side0(C1-A,LW,C2-B):- nonvar(A),!,
  print_side_by_side0(C1,LW1,C2),nl_if_needed,
  print_side_by_side0(A,LW2,B),
  ignore(max_min(LW1,LW2,LW,_)).
*/

print_side_by_side0(Nil,Lw,G2):- Nil==[],!,print_side_by_side0([[_nil]],Lw,G2).
print_side_by_side0(G1,Lw,Nil):- Nil==[],!,print_side_by_side0(G1,Lw,[[_nil]]).

print_side_by_side0(C1,LW,C2):- var(LW), fail, gridoid_size(C1,H1,V1),gridoid_size(C2,H2,V2),!,    
    ((V2 > V1) -> LW is -(H2 * 2 + 12) ; LW is (H1 * 2 + 12)),!, print_side_by_side0(C1,LW,C2).
print_side_by_side0(C1,LW,C2):-  var(LW), LW=30,print_side_by_side0(C1,LW,C2),!.

print_side_by_side0(C1,W0,C2):- number(W0), W0 < 0, LW is -W0, !, print_side_by_side0(C2,LW,C1).

print_side_by_side0(C1,W0,C2):- number(W0), LW is floor(abs(W0)),
  locally(nb_setval(print_sbs,left),into_ss_string(C1,ss(W1,L1))),
  locally(nb_setval(print_sbs,right),into_ss_string(C2,ss(_,L2))),!,
  with_style("font-size:100%",print_side_by_side_lists_1st(L1,W1,L2,LW)).

is_side(RL):- nb_current(print_sbs,RL),RL\==[].

trim_rs([],[]):-!.
trim_rs(S,SS):- member(WS, ["\s","\t","\n","\r"," "]), string_concat(Left,WS,S),
  trim_rs(Left,SS),!.
trim_rs(SS,SS).
  
extend_len(Need,S,New):- 
 must_det_ll((
  trim_rs(S,SS),
  display_length(SS,Spaces),Makeup is Need -Spaces, 
  ( Makeup<1 -> New=SS ;  (make_spaces(Makeup,Pre), (SS==[] -> SSS='' ; SSS=SS), atomics_to_string([SSS,Pre,'\n'],New))))).
   
make_spaces(Spaces,S):-
 wots_hs(S,(write_nbsp,forall(between(2,Spaces,_),write_nbsp),write_nbsp)),!.

maybe_exend_len(_L1,_L2,_,_):-!,fail.
maybe_exend_len(L1,L2,Lst1,L2):-  (is_grid(L1) ; \+ is_list(L1)),
  into_ss_string(L1,ss(_,Lst1)), !.
maybe_exend_len(L1,L2,L1,Lst2):-  (is_grid(L2) ; \+ is_list(L2)),
  into_ss_string(L2,ss(_,Lst2)), !.

%maybe_exend_len(L1,L2,L2L,L1):- length(L1,N1),length(L2,N2), N2 > N1, !,append(['Swapped2'],L2,L2L).

%maybe_exend_len(L1,L2,L2,L1):- L1==[], L2\==[],!.
maybe_exend_len(L1,L2,NL1,L2):-
  length(L1,N1),
  length(L2,N2),
  N2>N1, N1>=2,
 must_det_ll((
  nth1(1,L1,E1), display_length(E1,Spaces1),
  nth1(2,L1,E2), display_length(E2,Spaces2),
  max_min(Spaces1,Spaces2,Spaces,_Min),
  make_spaces(Spaces,S),
  Needs is N2-N1, make_list(S,Needs,AppendL1), 
  append(L1,AppendL1,NL0),
  my_maplist(extend_len(Spaces),NL0,NL1))),!.

maybe_exend_len(L1,L2,L2L,L1):- length(L1,N1),length(L2,N2), N2 > N1, !,append(['Swapped'],L2,L2L).

maybe_exend_len(L1,L2,NL1,L2):-
  length(L1,N1),
  length(L2,N2),
  N2>N1, 
  Needs is ((N2-N1)+1),
  wots_hs(S,write('\n')),
  make_list(S,Needs,AppendL1),
  append(L1,AppendL1,NL1),!.


maybe_exend_len(L1,L2,L2,L1):- length(L1,N1), length(L2,N2), N2 > N1, !.


print_side_by_side_lists_1st([],_,[],_):-!.

print_side_by_side_lists_1st(L1,W1,L2,LW):- maybe_exend_len(L1,L2,NL1,NL2),!, 
  print_side_by_side_lists_1st(NL1,W1,NL2,LW).

print_side_by_side_lists_1st([E1,E2|L1],W1,L2,LW):- !,
  wots_hs(S,(write(E2),write('\t '),dash_chars(W1,' ' ))),
  display_length(S,Pre),
  print_side_by_side_lists(Pre,[E1,E2|L1],W1,L2,LW).

 /*
print_side_by_side_lists_1st([E2|L1],W1,L2,LW):- !,
  wots_hs(S,(write(E2),write('\t '),dash_chars(W1,' ' ))),
  display_length(S,Pre),
  print_side_by_side_lists(Pre,[E2|L1],W1,L2,LW).
*/
spc_len(E2,SSS):- display_length(E2,L),wots_hs(SSS,forall(between(1,L,_),write_nbsp)).


print_side_by_side_lists(_,A,_,B,_):- wants_html,!, 
  print_card_list([card(my_maplist(writeln,A),true),card(my_maplist(writeln,B),true)]).


print_side_by_side_lists(Pre,[E1,A],W1,[E2,B|L2],W2):- L2 \==[],!,
  spc_len(E1,NewE1),
  %(E1==NewE1 -> EE1=E1 ; string_concat(' ',EE1,E1)),
  string_concat(' ',E2,EE2),
  write_padding(E1,W1,EE2,W2),  
  print_side_by_side_lists(Pre,[NewE1,A],W1,[B|L2],W2).

print_side_by_side_lists(Pre,[E1,A|L1],W1,[E2,B],W2):- L1 \==[],!,
  spc_len(E2,NewE2),  
  write_padding(E1,W1,E2,W2), 
  print_side_by_side_lists(Pre,[A|L1],W1,[NewE2,B],W2).


print_side_by_side_lists(Pre,[E1|L1],W1,[E2|L2],W2):-!,
  % .... 
  write_padding(E1,W1,E2,W2), 
  print_side_by_side_lists(Pre,L1,W1,L2,W2).
  
print_side_by_side_lists(_Pre,[],W1,[],W2):- !, nop(write_padding([],W1,[],W2)),!.

print_side_by_side_lists(Pre,[E1|L1],W1,[],W2):- !,
  write_padding(E1,W1,[],W2),
  print_side_by_side_lists(Pre,L1,W1,[],W2).
  
print_side_by_side_lists(Pre,[],W1,[E2|L2],W2):-
  with_output_to(atom(S),dash_chars(Pre,' ')), write_padding(S,W1,E2,W2),
  print_side_by_side_lists(Pre,[],W1,L2,W2).

desc(A,B):- wots_hs(S1,A),wots_hs(S2,B),nl_if_needed,format('~n'),dash_chars,write(S1),nl_if_needed,write(S2),nl_if_needed_ansi.

write_padding(E1,_W1,E2,LW):- %write_nbsp,
    W1 = LW,
   nl_if_needed,as_str(E1,S1), as_str(E2,S2), 
   write(S1), pre_s2(W1,S2), nl_if_needed_ansi.

pre_s2(_,S2):- atom_contains(S2,'_'), write('    '),write(S2).
pre_s2(_,S2):- atom_contains(S2,'\u00AF'), write('    '),write(S2).
pre_s2(_,S2):- atom_contains(S2,'|'), write('   '),write(S2).
pre_s2(W1,S2):- line_position(user_output,L1), Pad1 is W1 - L1, (dash_chars(Pad1, ' ')),write('  '),write(S2).

as_str(C,S):- plain_var(C),!,sformat(S,' var(~p)',[C]).
as_str([],""):-!.
as_str(S,A):- atom(S),!,atom_string(S,A).
as_str(S,S):- term_is_ansi(S), !.
as_str(call(C),S):- !, wots_hs(S,C).
as_str(S,A):- \+ string(S), sformat(A,'~p',[S]),!.
as_str(S,S).

list_print_length(S,Sum):- my_maplist(print_length,S,LL),!,sumlist(LL,Sum).
print_length(S,Sum):- grid_footer(S,G,_),G\=@=S,!,print_length(G,Sum).
print_length(S,Sum):- is_gridoid(S),into_grid(S,G),!,grid_size(G,H,_), Sum is H*2 + 5.
print_length(S,L):- as_str(S,A),atom_codes(A,C), include(uses_space,C,SS),length(SS,L).


append_term_safe(Type,PairName,NameIn):- append_term(Type,PairName,NameIn),!.
append_term_safe(Type,PairName,append_term(Type,PairName)).

show_pair(_,_,_,_, _,_,_,_):- is_print_collapsed,!.
show_pair(IH,IV,OH,OV,Type,PairName,In,Out):-
  %show_pair_grid(TitleColor,IH,IV,OH,OV,Type,PairName,In,Out),
  %show_pair_info(IH,IV,OH,OV,Type,PairName,In,Out),
  show_pair_diff(IH,IV,OH,OV,in(Type),out(Type),PairName,In,Out).

show_pair_no_i(IH,IV,OH,OV,Type,PairName,In,Out):-
  ignore(IH=1),
  LW is (IH * 2 + 12),
  append_term_safe(Type,PairName,NameIn),
  append_term_safe(Type,PairName,NameOut),
  wots_hs(U1, print_grid(IH,IV,NameIn+fav(PairName),In)),
  wots_hs(U2, print_grid(OH,OV,NameOut+fav(PairName),Out)),
  print_side_by_side(U1,LW,U2),!.

show_pair_grid(_,_,_,_,_, _,_,_,_):- is_print_collapsed,!.
show_pair_grid(TitleColor,IH,IV,OH,OV,NameIn,NameOut,PairName,In,Out):-
  toUpperC(NameIn,NameInU),toUpperC(NameOut,NameOutU),
 % ignore(IH=1),
  %LW is (IH * 2 + 12),
%  wots_hs(U1, print_grid(IH,IV,In)),
%  wots_hs(U2, print_grid(OH,OV,Out)),
  INFO = [grid_dim,mass,length,unique_color_count,colors_cc],
%  print_side_by_side(U1,LW,U2),
  print_side_by_side(TitleColor,print_grid(IH,IV,In),NameInU,LW,print_grid(OH,OV,Out),NameOutU),
  print_side_by_side0(
     call(describe_feature(In,[call(ppnl(NameInU+fav(PairName)))|INFO])),LW,
    call(describe_feature(Out,[call(ppnl(NameOutU+fav(PairName)))|INFO]))),!.


toUpperC(A,AU):- A==[],!,AU='  []  '.
toUpperC(A,AU):- string(A),!,AU=A.
toUpperC(A,A):-!.
toUpperC(A,AU):- atom(A),toPropercase(A,AU),!.
toUpperC(A,AU):- atomic(A),upcase_atom(A,AU),!.
toUpperC(A,AU):- is_list(A),my_maplist(toUpperC,A,AU),!.
toUpperC(I,O):- compound(I), !, compound_name_arguments(I,F,IA), my_maplist(toUpperC,IA,OA), compound_name_arguments(O,F,OA),!.
toUpperC(A,AU):- term_to_atom(A,AU).

show_pair_diff(IH,IV,OH,OV,NameIn,NameOut,PairName,In,Out):-
  toUpperC(NameIn,NameInU),toUpperC(NameOut,NameOutU),
  show_pair_grid(cyan,IH,IV,OH,OV,NameIn,NameOut,PairName,In,Out),
  if_t(\+ nb_current(menu_key,'i'),
  locally(nb_setval(show_indiv,t),
   ((is_group(In),is_group(Out))-> once(showdiff(In,Out));
    ((ignore((is_group(In), desc(ppnl(NameInU+fav(PairName)), print_info(In)))),
      ignore((is_group(Out),desc(ppnl(NameOutU+fav(PairName)), print_info(Out))))))))),!.


uses_space(C):- code_type(C,print).

:- meta_predicate(into_ss_string(+,-)).

into_ss_string(C, X):- var(C),!, must_det_ll(X=ss(1,["var_into_ss_string"])).
into_ss_string(C,_):- plain_var(C),!,throw(var_into_ss_string(C)).
into_ss_string(Props,SS):- is_obj_props(Props),!,into_ss_call(wqs(Props),SS).
into_ss_string(print_grid(G),SS):- !,into_ss_grid(G,SS).
into_ss_string(print_grid0(G),SS):- !,into_ss_grid(G,SS).
into_ss_string(print_grid(X,Y,G),SS):- !, into_ss_grid(X,Y,G,SS).
into_ss_string(print_grid0(X,Y,G),SS):- !, into_ss_grid(X,Y,G,SS).
into_ss_string(wqs(W),SS):- !,into_ss_call(wqs(W),SS).
into_ss_string(print_ss(W),SS):- !,into_ss_call(print_ss(W),SS).
into_ss_string(wqs(C,W),SS):- !,into_ss_call(wqs(C,W),SS). 

into_ss_string(ss(Len,L),ss(Len,L)):-!.
into_ss_string(uc(W),SS):- !, into_ss_string(uc(yellow,W),SS).
into_ss_string(uc(C,W),SS):- !,into_ss_call(color_print(C,call(underline_print(format("\t~@",[wqs(W)])))),SS).
into_ss_string(call(C),SS):- !,into_ss_call(C,SS),!.


into_ss_string(GG, SS):- is_grid(GG),!,into_ss_grid(GG,SS).
into_ss_string(GG, SS):- is_group(GG),!,into_ss_grid(GG,SS).
into_ss_string(GG, SS):- is_points_list(GG),!,into_ss_grid(GG,SS).
into_ss_string(GG, SS):- is_gridoid(GG),!,into_ss_grid(GG,SS).
into_ss_string(GG, SS):- is_object(GG),!,into_ss_grid(GG,SS).
into_ss_string(GG, SS):- is_point(GG),!,into_ss_grid([GG],SS).
into_ss_string(GG, SS):- known_grid(GG,G),G\==GG,!,into_ss_grid(G, SS).

into_ss_string(AB, SS):- grid_footer(AB,A,B),!,into_ss_concat(print_grid(A),wqs(B),SS).

into_ss_string(A+B,SS):- into_ss_concat(A,B,SS),!.
into_ss_string(A^B,SS):- into_ss_concat(A,B,SS),!.
into_ss_string(A-B,SS):- into_ss_concat(A,B,SS),!.

into_ss_string(Str,SS):- string(Str), !,  string_into_ss(Str,SS).
into_ss_string(LL, SS):- is_list(LL),  my_maplist(stringy_string,LL,SL), find_longest_len(SL,Len),!,SS=ss(Len,SL).

into_ss_string(NCT,SS):- \+ callable(NCT), !, into_ss_call(wqs(NCT),SS).
into_ss_string(LL, SS):- is_list(LL), !, into_ss_call(wqs(LL),SS).
%into_ss_string(LL, SS):- is_list(LL), find_longest_len(LL,Len),!,SS=ss(Len,LL).
%into_ss_string(H,SS):- callable_arity(H,0),is_writer_goal(H),wots(catch(call_e_dmsg(H),_,fail),S), string_into_ss(S,SS).

into_ss_string(Goal,SS):-  callable_arity(Goal,0), into_ss_call(Goal,SS).
into_ss_string(Goal,SS):-  callable_arity(Goal,1), call(Goal,R1), into_ss_grid(R1,SS).
into_ss_string(IntoG,SS):- into_grid(IntoG,GR),is_grid(GR),!,into_ss_grid(GR,SS).
into_ss_string(Goal,SS):- into_ss_call(Goal,SS).

stringy_string(Str,SS):- notrace(catch(sformat(SS,'~s',[Str]),_,fail)).
string_into_ss(Str,SS):- stringy_string(Str,S), atomics_to_string(LL,'\n',S),!,find_longest_len(LL,Len),!,SS=ss(Len,LL).

%as_string(S,SS):- wots_hs(SS,write(S)).
into_ss_grid(G,SS):- into_ss_call(print_grid(G),SS).
into_ss_grid(H,V,G,SS):- into_ss_call(print_grid0(H,V,G),SS).
%into_ss_call(C,SS):- wots_hs(Str,catch((C->true;writeq(failed(C))),E,true)),var(E)
into_ss_call(C,SS):- 
  wots_hs(Str,catch((C->true;writeq(failed(C))),E,true)), 
  (nonvar(E)->(throw(E);must_not_error(C));true), 
  string_into_ss(Str,SS).

into_ss_concat(A,B,ss(LenAB,ABL)):- !,into_ss_string(A,ss(LenA,LA)),
                                      into_ss_string(B,ss(LenB,LB)),
                                      append(LA,[""|LB],ABL), max_min(LenA,LenB,LenAB,_).


find_longest_len(SL,L):- find_longest_len(SL,10,L),!.
find_longest_len([],L,L).
find_longest_len([S|SS],N,L):- print_length(S,N2),max_min(N,N2,NM,_),
  find_longest_len(SS,NM,L).

:- meta_predicate( print_with_pad(0)).
:- export( print_with_pad/1).
/*print_with_pad(Goal):- 

  (line_position(current_output,O);O=0),!, 
  O1 is O+1,
  call_w_pad(O1,Goal).
*/
print_with_pad(Goal):-(line_position(current_output,O);O=0),!,  O1 is O+1,wots(S,Goal),print_w_pad(O1,S).
  

into_s(Text,S):- notrace(catch(text_to_string(Text,S),_,fail)),!.
into_s(Obj,S):- wots_hs(S,pp(Obj)),!.

print_w_pad(Pad,Text):- into_s(Text,S), atomics_to_string(L,'\n',S)-> my_maplist(print_w_pad0(Pad),L).
print_w_pad0(Pad,S):- nl_if_needed,dash_chars(Pad,' '), write(S).


:- meta_predicate(call_w_pad_prev(+,0)).
call_w_pad_prev(Pad,Goal):- wots_hs(S,Goal), print_w_pad(Pad,S).

%call_w_pad(N,Goal):- wants_html,!,format('<span style="margin-left:~w0%;">',[N]),call_cleanup(call(Goal),write('</span>')).
:- meta_predicate(call_w_pad(+,0)).
call_w_pad(_N,Goal):- wants_html,!,format('<span style="margin-left:10px;">',[]),call_cleanup(call(Goal),write('</span>')).
call_w_pad(N,Goal):- nl_if_needed,wots_hs(S,dash_chars(N,' ')),!,pre_pend_each_line(S,Goal).
maybe_print_pre_pended(Out,Pre,S):- atomics_to_string(L,'\n',S), maybe_print_pre_pended_L(Out,Pre,L).
maybe_print_pre_pended_L(Out,_,[L]):- write(Out,L),!,flush_output(Out).
maybe_print_pre_pended_L(Out,Pre,[H|L]):- write(Out,H),nl(Out),!,write(Out,Pre),maybe_print_pre_pended_L(Out,Pre,L).

%pre_pend_each_line(_,Goal):- !,ignore(Goal).
:- meta_predicate(pre_pend_each_line(+,0)).
pre_pend_each_line(Pre,Goal):- write(Pre),pre_pend_each_line0(Pre,Goal).
pre_pend_each_line0(Pre,Goal):-
  current_output(Out),
  current_predicate(predicate_streams:new_predicate_output_stream/2),!,
  call(call,predicate_streams:new_predicate_output_stream([Data]>>maybe_print_pre_pended(Out,Pre,Data),Stream)),
  arc_set_stream(Stream,tty(true)),
  %arc_set_stream(Stream,buffer(false)),
  %undo(ignore(catch(close(Stream),_,true))),!,
  setup_call_cleanup(true,
   (with_output_to_each(Stream,once(Goal)),flush_output(Stream)),
    ignore(catch(close(Stream),_,true))),!.
pre_pend_each_line0(Pre,Goal):-
  with_output_to_each(string(Str),Goal)*->once((maybe_print_pre_pended(current_output,Pre,Str),nl_if_needed)).



print_equals(_,N,V):- \+ compound(V),ppnl(N=V).
print_equals(Grid,N,Ps):- is_object(Ps),grid_size(Grid,H,V),print_grid(H,V,N,Ps),!.
%print_equals(Grid,N,PL):- is_group(PL), grid_size(Grid,H,V), locally(grid_nums(PL),print_list_of_points(N,H,V,[[]])).
print_equals(_,N,G):- print_equals(N,G).


print_equals(N,V):- \+ compound(V),ppnl(N=V).
print_equals(N,V):- is_grid(V),!,ppnl(N),print_grid(V).
print_equals(N,[G|L]):-
  is_grid(G),is_list(L),my_maplist(is_grid,L),!,
  length([G|L],Len), 
  grid_size(G,H,_),
  ppnl(N=len(Len)),  
  dash_chars(H,"-"),
  forall(member(E,[G|L]),(print_grid(E),dash_chars(H,"-"),nl_now)).
print_equals(N,V):- better_value(V,BV)-> BV\=@=V, !,print_equals(N,BV).
print_equals(N,[S|L]):- string(S),writeq(N),write('= '),write(S),my_maplist(commawrite,L),nl_now.
print_equals(Name,json(JSON)):-!, print_equals(Name,JSON).
print_equals(Name,trn=Y):- !, print_equals(Name,Y).
print_equals(Name,X->Y):- !, print_equals(in(Name),X), print_equals(out(Name),Y).
print_equals(colors_cc,XY):-print_equals(cc,XY).
%print_equals(Name,cc(C,N)):-print_equals(Name,cc(C,N)).
print_equals(Name,X=Y):- !, print_equals(Name=X,Y).
%print_equals(Name,[H|L]):- !, my_maplist(print_equals(Name),[H|L]).
print_equals(Name,Val):- is_list(Val),forall(nth0(N,Val,E),print_equals(Name:N,E)).
print_equals(Name,Val):- pp(Name=Val).

commawrite(S):- write(','),write(S).



as_color(cc(Count,Num),List):- color_name(Num,Name),wots_hs(List,color_print(Num,Name=Count)).
better_value(V,List):- is_list(V), my_maplist(as_color,V,List).
better_value([G|V],List):- 
  is_group([G|V]),
  my_maplist(points_to_grid,[G|V],List),
  [G|V] \=@= List.


fix_grid_pg(SIndvOut,_InOutL,_G,_PG):- is_grid(SIndvOut),!,fail.
fix_grid_pg(SIndvOut,PGP,G,GP):- compound(SIndvOut), SIndvOut=(GG,PGG),!,listify(PGG,PGL),listify(GG,G),!,append(PGL,PGP,GP).
fix_grid_pg(SIndvOut,PGP,SIndvOut,[PGP]):- \+ is_list(PGP),!.
fix_grid_pg(SIndvOut,PGP,[SIndvOut],PGP):- \+ is_list(SIndvOut),!.

:- meta_predicate(with_glyph_index(+,0)).
with_glyph_index(Print,Goal):- Print==[],!,with_glyph_index([e],Goal).
with_glyph_index(Print,Goal):- listify(Print,PrintL),
  locally(b_setval(glyph_index,PrintL),Goal).

:- meta_predicate(with_color_index(+,0)).
with_color_index(Print,Goal):- Print==[],!,with_color_index([e],Goal).
with_color_index(Print,Goal):- listify(Print,PrintL),
  locally(nb_setval(color_index,PrintL),Goal).
use_row_db :- fail.

is_print_collapsed:- \+ nb_current(arc_portray,t), luser_getval(print_collapsed,N),N\==[],N==t.

print_grid(GridStr):- GridStr==[],!,writeln('NilGroup'),!.
print_grid(GridStr):- GridStr==[[]],!,writeln('ZeroSizeGrid'),!.
print_grid(_):- is_print_collapsed,!.
print_grid(GridStr):- compound(GridStr),grid_footer(GridStr,Grid,Str),!,print_grid(Str,Grid).
print_grid(Grid):- use_row_db, is_grid(Grid),!, grid_to_tid(Grid,TID),print_grid(TID).

print_grid(Grid):- make_bg_visible(Grid,GGrid),  quietly(print_grid0(_,_,GGrid)),!.

:- meta_predicate(print_grid(+,+)).
print_grid(Str,Grid):- Grid==[],!, wots(S,wqs_c(Str)), write(nil_grid(S)).
print_grid(Str,Grid):- Grid==[[]],!, wots(S,wqs_c(Str)), write(zero_size_grid(S)).

%print_grid(Str,Grid):- wants_html,!,print_table([[print_grid(Grid)],[ppt(Str)]]).
print_grid(Str,Grid):-  make_bg_visible(Grid,GGrid), ignore((print_grid(_,_,Str,GGrid))),!.
%print_grid(Str,G):- compound(G), maybe_make_bg_visible(G,GG),!,print_grid0(H,V,GG).
%print_grid0(Grid):-  ignore(print_grid0(_,_,Grid)),!.

%print_grid0(Grid):- plain_var(Grid),!, throw(var_print_grid(Grid)).

format_u(TitleColor,Format,Args):- quietlyd( ignore((underline_print(color_print(TitleColor,call(format(Format,Args))))))).

%format_footer(TitleColor,Name,SS)
format_footer(TitleColor,W1,W2):- wots(S1,wqs_c(W1)), W2==string,!,format_u(TitleColor,'~w',[S1]).
format_footer(TitleColor,W1,W2):- wots(S1,wqs_c(W1)), format_u(TitleColor,'~w (~w)',[S1,W2]).


print_grid(_,_,_,_):- is_print_collapsed,!.

print_grid(OH,OV,Name,Grid):- 
 must_det_ll((
  ((
   data_type(Grid,SS), 
   mesg_color(SS,TitleColor),
   print_tb_card(print_grid0(OH,OV,Grid),
                 format_footer(TitleColor,Name,SS)))))),
 if_thread_main(print_title(Name)).
/*
print_grid(OH,OV,Name,Grid):- 
 quietly(( make_bg_visible(Grid,Out),
  ignore((print_grid0(OH,OV,Out))),!,nl_if_needed,format('  '),
  ignore((data_type(Out,SS), toUpperC(Name,NameU),
  mesg_color(SS,TitleColor),
  format_footer(TitleColor,NameU,SS))))).
*/
%print_grid(H,V,Grid):- use_row_db, grid_to_tid(Grid,TID),!,print_grid0(H,V,TID).

print_grid(_,_,_):- is_print_collapsed,!.
print_grid(H,V,Grid):-  make_bg_visible(Grid,GGrid), ignore(quietly(print_grid0(H,V,GGrid))).

print_grid0(_,_,_):- is_print_collapsed,!.
print_grid0(H,V,G):- G==[],number(H),number(V),!,make_grid(H,V,GG),!,print_grid0(H,V,GG).
% print_grid0(_H,_V,G):- G==[],!,make_grid(H,V,GG),!,print_grid0(H,V,GG).

print_grid0(H,V,D):- is_vm_map(D),ignore(H = D.h),ignore(V = D.v),
  vm_to_printable(D,R),D\==R,!,print_grid0(H,V,R).

print_grid0(H,V,Grid):- \+ callable(Grid),!,write('not grid: '),
  GG= nc_print_grid(H,V,Grid), pp(GG),!,nop(trace_or_throw(GG)).

print_grid0(H,V,G):- compound(G), G=(GG-PP),is_grid(GG),!,print_grid(H,V,PP,GG).
print_grid0(H,V,SIndvOut):- compound(SIndvOut),SIndvOut=(G-GP), \+ is_ncpoint(GP),!, 
  with_glyph_index(G,with_color_index(GP,print_grid0(H,V,G))),!.
%print_grid0(H,V,Grid):- is_points_list(Grid), points_to_grid(H,V,Grid,PGrid),!,print_grid0(H,V,PGrid).
print_grid0(H,V,G):- is_empty_grid(G), %atrace, arcST,
 u_dmsg(is_empty_grid(H,V)),!,
 make_grid(H,V,Empty),
 print_grid0(H,V,Empty),!. 

%print_grid0(H,V,Grid):- \+ is_gridoid(Grid), into_grid(Grid,G), G\=@=Grid, !, print_grid0(H,V,G).
print_grid0(H,V,Grid):- print_grid0(1,1,H,V,Grid),!.

%print_grid(SH,SV,EH,EV,Grid):- nop(print_grid(SH,SV,EH,EV,Grid)),!.
print_grid(SH,SV,EH,EV,Grid):- quietlyd(print_grid0(SH,SV,EH,EV,Grid)),!.

print_grid0(SH,SV,EH,EV,NCPs):- is_ncpoints_list(NCPs),my_maplist(append_term(-(fg)),NCPs,Grid),!,print_grid0(SH,SV,EH,EV,Grid).
print_grid0(SH,SV,EH,EV,Grid):- is_list(Grid), \+ is_grid(Grid), Grid=[G],!,print_grid0(SH,SV,EH,EV,G).
print_grid0(_SH,_SV,_EH,_EV,GridFooter):- grid_footer(GridFooter,Grid,Footer),!,print_grid(Footer,Grid).
print_grid0(_SH,_SV,_EH,_EV,print_ss(Grid)):- !, print_ss(Grid).
print_grid0(SH,SV,EH,EV,Grid):- Grid= A^B,!,print_grid0(SH,SV,EH,EV,A),print_grid0(SH,SV,EH,EV,B).
print_grid0(_SH,_SV,_EH,_EV,Grid):- \+ is_printable_gridoid(Grid), !, not_printable_gridoid(Grid).
print_grid0(SH,SV,EH,EV,Grid):-  
  \+ \+ print_grid1(SH,SV,EH,EV,Grid),!,nl_if_needed_ansi.


  
not_printable_gridoid(H):- callable_arity(H,0),is_writer_goal(H),catch(call_e_dmsg(H),_,fail),!.
not_printable_gridoid(Grid):- catch(wqs(Grid),_,writeln(\+ is_printable_gridoid(Grid))).


print_grid1(SH,SV,EH,EV,Grid):- is_object(Grid),
  object_grid(Grid,Points),!,print_grid1(SH,SV,EH,EV,Points).

print_grid1(SH,SV,EH,EV,Grid):-
 nl_if_not_side_by_side,
 %backtrace(10),
 (line_position(current_output,O);O=0),!, O1 is O+1,
 print_grid_pad(O1,SH,SV,EH,EV,Grid), 
 nl_if_not_side_by_side,nl_if_needed_ansi.

nl_if_not_side_by_side:- ignore(( \+ in_side_by_side, nl_if_needed_ansi)).

in_side_by_side:- current_output(Out), \+ stream_property(Out,alias(user_output)).

print_grid_pad(_,SH,SV,EH,EV,Grid):- wants_html,!, with_toplevel_pp(http,print_grid_html(SH,SV,EH,EV,Grid)).
print_grid_pad(O1,SH,SV,EH,EV,Grid):-
  into_color_name_always(Grid,GridI),
  call_w_pad(O1,print_grid2(SH,SV,EH,EV,GridI)).
  


print_grid2(SH,SV,EH,EV,GridII):- atomic(GridII), once(into_grid(GridII,GridIII)), \+ atomic(GridIII),!,
  print_grid2(SH,SV,EH,EV,GridIII).

print_grid2(SH,SV,EH,EV,ObjProps):- is_obj_props(ObjProps),!,print_grid2(SH,SV,EH,EV,[obj(ObjProps)]).
print_grid2(SH,SV,EH,EV,[ObjProps|More]):- is_obj_props(ObjProps),!,
  maplist(print_grid2(SH,SV,EH,EV),[ObjProps|More]).
print_grid2(SH,SV,EH,EV,GridI):- arc_html,!,print_grid_html(SH,SV,EH,EV,GridI).
print_grid2(SH,SV,EH,EV,GridI):- in_pp(ansi),!,ignore(print_grid_ansi(SH,SV,EH,EV,GridI)).
print_grid2(SH,SV,EH,EV,GridI):- in_pp(bfly),!,az_ansi(ignore(print_grid_ansi(SH,SV,EH,EV,GridI))).
print_grid2(SH,SV,EH,EV,GridI):- \+ in_pp(bfly), \+ in_pp(ansi), wants_html,!, with_toplevel_pp(http,print_grid_html(SH,SV,EH,EV,GridI)),nl_now.
print_grid2(SH,SV,EH,EV,GridI):- !, bfly_html_pre(print_grid_ansi(SH,SV,EH,EV,GridI)).
print_grid2(SH,SV,EH,EV,GridI):- ignore(print_grid_ansi(SH,SV,EH,EV,GridI)).


w_out(S):- toplevel_pp(bfly),!,correct_nbsp(S,SO),our_pengine_output(SO),!.
w_out(S):- is_webui,!,correct_nbsp(S,SO),our_pengine_output(SO),!.
w_out(S):- nl_if_needed,write(S).
%w_out(SO):- pengines:pengine_output('</pre>'),pengines:pengine_output(SO),pengines:pengine_output('<pre class="console">'),!.

:- meta_predicate(g_out(0)).
:- export(g_out/1).
g_out(G):- !,ignore(call(G)).
g_out(G):- wants_html,!,once(ignore(G)).
g_out(G):- wants_html,!,once(ignore(G)).
g_out(G):- is_side(_),!,call(G).
g_out(G):- \+ wants_html,!,nl_if_needed,call(G),nl_if_needed_ansi.
g_out(G):- nb_current(in_g_out,t),!,nl_if_needed,call(G),nl_if_needed_ansi.
g_out(G):- locally(nb_setval(in_g_out,t), gg_out(G)).

gg_out(G):- !,ignore(call(G)).
gg_out(G):- wants_html,!,once(ignore(G)).
gg_out(G):- \+ toplevel_pp(ansi),!,bfly_html_goal(G).
gg_out(G):- call(G).
%gg_out(G):- call(G).
%gg_out(G):- \+ toplevel_pp(bfly),!,gg_out2(G).
%gg_out(G):- bfly_html_goal(gg_out2(G)).

gg_out2(G):- !,ignore(call(G)).
gg_out2(G):-
  wots_hs(S0,call(G)),correct_nbsp(S0,S), !,
  sformat(SO,'<pre style="overflow-x: visible;"><font size="+0">~w</font></pre>',[S]),
  w_out(SO).

g_out_style(C,G):- wots_hs(S0,g_out(G)),correct_nbsp(S0,S),
 mbfy(color_print(C,S)).

%mbfy(G):- in_pp(ansi),!,call(G).
mbfy(G):- in_pp(http),!,call(G).
mbfy(G):- in_pp(bfly),!,bfly_html_goal(G).
mbfy(G):- !,call(G).

correct_nbsp(S0,S):-!,S0=S.
correct_nbsp(S0,S):- replace_in_string([" &nbsp;"="&nbsp;","&nbsp; "="&nbsp;"],S0,S).

%ansi_format_real(Ansi,Format,Args):- wants_html,!,sformat(S,Format,Args),!,color_print_webui(Ansi,S).
%ansi_format_real(Ansi,Format,Args):- ansicall(Ansi,format(Format,Args)),!.
ansi_format_real(fg(fg(Ansi)),Format,Args):- !, ansi_format(fg(Ansi),Format,Args).
ansi_format_real(Ansi,Format,Args):- ansi_format(Ansi,Format,Args).


color_print_webui(C,G):- ansi_main,!,color_print(C,G).
color_print_webui(C,G):- mbfy(with_color_span(C,G)).


cpwui0(C,G):-
  flag('$in_cpwui',X,X),
  ((X>6)
   ->bformatc_or_at(G); 
   setup_call_cleanup(flag('$in_cpwui',_,X+1),once(with_color_span(C,G)),flag('$in_cpwui',_,X))).

with_color_span(_,G):- G==' ',!,write_nbsp.

with_color_span(C,G):- C==black,!,cpwui0([black,black],G).

with_color_span(C,G):- multivalued_peek_color(C,W),cpwui0(W,G).
with_color_span(C,G):- is_bg_sym_or_var_ui(C),!,cpwui0(wbg,G).

with_color_span([],G):- !, bformatc(G).
with_color_span([C|CC],G):- !,wots_hs(S,cpwui0(C,G)),cpwui0(CC,S).
%cpwui0(_C,G):- \+ wants_html,!,bformatc(G).

with_color_span(C,G):- ansi_main,!,color_print(C,G).
with_color_span(underline,G):- !, cpwui0(style('text-decoration','underline'),G).
with_color_span(bold,G):- !, cpwui0(style('font-weight','bold'),G).
with_color_span(italic,G):- !, cpwui0(style('font-style','italic'),G).

%cpwui0(C,G):- \+ wants_html,!,color_print(C,G).
with_color_span(bg(C),G):- !, cpwui0(style('background-color',C),G).
with_color_span(hbg(C),G):- !, cpwui0([bg(C),style('filter','brightness(150%)')],G).
with_color_span(hfg(C),G):- !, cpwui0([C,style('brightness','200%')],G).
with_color_span(style(C),G):- !, format('<span style="~w">~@</span>',[C,bformatc_or_at(G)]).
with_color_span(style(N,V),G):- !, format('<span style="~w: ~w;">~@</span>',[N,V,bformatc_or_at(G)]).
with_color_span(C,G):- get_black(Black),C==Black,!, cpwui0(style('opacity: 0.5;'),G).

with_color_span(color(FG),G):- into_html_color(FG,C), !, format('<font color="~w" style="color: ~w">~@</font>',[C,C,bformatc_or_at(G)]),!.
with_color_span(fg(C),G):- !, with_color_span(color(C),G).
with_color_span(C,G):- is_color(C),with_color_span(color(C),G).
with_color_span(C,G):- integer(C),arc_acolor(C,CC),CC\==C,!,with_color_span(color(CC),G).
with_color_span(C,G):- format('<font style="~w">~@</font>',[C,bformatc_or_at(G)]),!.

%%with_color_span(fg(C),G):- !, format('<font color="~w" style="font-weight: bold;">~@</font>',[C,bformatc_or_at(G)]),!.
%cpwu((C),G):- !, format('<font color="~w">~@</font>',[C,bformatc(G)]).

bformatc_or_at(C):- wots_hs(S,bformatc(C)), ( (S=="";(fail,atom_contains(S,"><"))) -> write('@') ; write(S)).

is_html_color(A):- \+ atom(A),!,fail.
is_html_color(A):- atom_concat('#',Rest,A),!,atom_length(Rest,6).
is_html_color(teal).
is_html_color(C):- is_real_color(C),!.
/*
with_color_span(fg(C),G):- !, with_color_span(color(C),G).
with_color_span(color(C),G):- !, with_color_span(style('color',C),G)
% style="font-weight: bold;.
with_color_span(color(C),G):- format('<font color="~w">~@</font>',[C,bformatc(G)]),!.
with_color_span(C,G):- format('<span style="~w">~@</span>',[C,bformatc(G)]),!.
*/


:- meta_predicate(bformatc(+)).
bformatc(G):- var(G),!,bformatc(vaR(G)).
bformatc(G):- string(G),!,bformats(G).
bformatc(G):- is_list(G), notrace(catch(text_to_string(G,S),_,fail)),G\==S,!,bformatc(S).
bformatc(G):- is_writer_goal(G), wots_hs(S,call(G)),!, bformatc(S).
bformatc(G):- atom(G),!,bformats(G).
bformatc(G):- wots(S,write(G)), bformats(S).

bformats(S):- atom_contains(S,'<'),!,write(S).
bformats(S):- bformatc1(S).

bformatc1(S):- \+ wants_html,!,write(S).
bformatc1(S):- write(S),!.
bformatc1(S):- atom_codes(S,Cs), my_maplist(map_html_entities_mono,Cs,CsO),atomic_list_concat(CsO,W),!,bformatw(W).

grid_colors(GridI,WGrid):-
   must_det_ll((
  %maybe_grid_numbervars(GridI,Grid),
  GridI=Grid,
  grid_size(Grid,EH,EV),
  make_grid(EH,EV,WGrid),
  bg_sym_ui(BGC),
  forall(between(1,EV,V),
     forall(between(1,EH,H),
      ignore((((hv_cg_value(Grid,CG,H,V);CG=f(BGC))->
         (once(nb_set_chv(CG,H,V,WGrid)))))))))).

print_grid_ansi(SH,SV,EH,EV,GridII):- toplevel_pp(http),!, print_grid_html(SH,SV,EH,EV,GridII),!.
print_grid_ansi(SH,SV,EH,EV,GridII):- is_real_user_output,is_real_user_error,print_grid_ansi_real(SH,SV,EH,EV,GridII),!.
print_grid_ansi(SH,SV,EH,EV,GridII):- in_pp(bfly),!,print_grid_ansi_real(SH,SV,EH,EV,GridII),!.
print_grid_ansi(SH,SV,EH,EV,GridII):- with_toplevel_pp(http,print_grid_html(SH,SV,EH,EV,GridII)),!.

is_real_user_output:-  stream_property(X,alias(user_output)),stream_property(X,file_no(1)).
is_real_user_error:-  stream_property(X,alias(user_error)),stream_property(X,file_no(2)).

print_grid_ansi_real(SH,SV,EH,EV,GridII):- make_bg_visible(GridII,GridI),
 must_det_ll((
  nl_if_needed, 
  %maybe_grid_numbervars(GridI,Grid),
  GridI=Grid,
  ((plain_var(EH) ; plain_var(EV))->grid_size(Grid,EH,EV);true),
  Width is EH-SH, 
  (Width==0 -> DBW = 1 ; DBW is Width+1),
  once((dash_border_no_nl(DBW))),
  bg_sym_ui(BGC),
  forall(between(SV,EV,V),
   ((nl_if_needed,format('|'),
     forall(between(SH,EH,H),
      ignore((
       (((hv_cg_value(Grid,CG,H,V);hv_c_value(Grid,CG,H,V);/*grid_cpoint(Grid,CG-_,H,V);*/CG=BGC))->
        (once(print_gw1(CG);write('??')));write('?f'))))),
     write(' |')))),
  nl_if_needed,!,
  once((dash_uborder_no_nl(DBW),write(''))))), 
  nop((    
     (( \+ ground(GridI));sub_var(wbg,GridI);sub_var(bg,GridI);sub_var(fg,GridI);sub_var(fg,GridI)),
      grid_colors(GridI,CGrid),
      (nb_current(print_sbs,left)-> (nl_now,nl_now, write(left), write(=)) ; true),
     ppa(CGrid))).

 /*
         "#000000",  # 0: black
        "#0074D9",  # 1: blue
        "#FF2222",  # 2: red
        "#2ECC40",  # 3: green
        "#FFDC00",  # 4: yellow
        "#AAAAAA",  # 5: gray
        "#F012BE",  # 6: magenta
        "#FF8C00",  # 7: orange
        "#7FDBFF",  # 8: sky
        "#870C25",  # 9: brown
        "#444444",  # 10: dark grey, (for Transparency)
        "#888888",  # 11: light grey, (for Cutout)
 */
%print_grid(Grid):- is_grid(Grid),!, my_maplist(print_rows,Grid),nl_now.
%print_rows(List):- my_maplist(print_g,List),nl_now.
%block_colors([(black),(blue),(red),(green),(yellow),'#c0c0c0',(magenta),'#ff8c00',(cyan),'#8b4513']).
%block_colors([(black),(blue),(red),(green),(yellow),Silver,('#966cb8'),'#ff8c00',(cyan),'#8b4513']):- silver(Silver),!.
%block_colors([(black),(blue),(red),(green),(yellow),Silver,(magenta),'#ff8c00',(cyan),'#8b4513','#2a2a2a', 9379b4 '#3a5a3a']):- silver(Silver),!.
block_colors([('#4a2a2a'),(blue),(red),(green),(yellow),Silver,(magenta),'#ff8c00',(cyan), % '#104010'
                                                                                       '#8b4513','#707007','#3a5a3a','#f47c7c','#4a2a2a','#ffffff',FG]):- fg_cut(FG), silver(Silver),!.
named_colors([(Zero),(blue),(red),(green),(yellow),(silver),(purple),   (orange),(cyan), (brown),Black,  wbg,      wfg,      bg,      fg,     FG]):- 
  fg_cut(FG), grid_zero(Zero),z_or_b(Zero,Black).
named_colors([ (lack),(blue),(red),(green),(yellow),(Silver),(purple),(orange),(cyan),(brown)]):- silver(Silver).
named_colors([(lack),(blue),(red),(green),(yellow),(silver),(magenta),(orange),(cyan),(brown)]).
named_colors([(lack),(blue),(red),(green),(yellow),(grey),(pink),(orange),(teal),(maroon)]).

test_show_colors:- my_maplist(show_color,[0,1,2,3,4,5,6,7,8,9,zero,fg,wfg,bg,wbg,black,_,'#100010','#104010','#4a2a2a'],G),
  reverse(G,R),
  must_det_ll(print_grid([G,R,G])),nl.
show_color(X,N):- var(X),!,show_color('#101010',N).
show_color(X,N):- color_name(X,N),write(X),write(=),color_gl_int_g(X,Int),color_print(X,N),write('['),no_color_print(Int),write('] ').

no_color_print(X):- color_print([reset],X).
% '#1077f1'
fg_cut('#b399d4').
% silver(rgb(123,123,123)).
silver('#9a9a9a').
silver('#7b7b7b').
silver('#c0c0c0').

/*

0=black[t] 1=blue[u] 2=red[?] 3=green[?] 4=yellow[?] 5=silver[?] 6=purple[O] 7=orange[?] 8=cyan[?] 9=brown[?] fg=fg[O] fg=fg[g] bg= [U] wbg=wbg[?] black=black[t] #101010=#101010[#101010] #100010=#100010[#100010] #104010=#104010[#104010]
   _____________________________________
  | . l ? ? ? ? O ? ? ? O g     . #101010 #100010   |
  |   #100010 #101010 .     g O ? ? ? O ? ? ? ? l . |
  | . l ? ? ? ? O ? ? ? O g     . #101010 #100010   |
   -------------------------------------

true.


*/


unnegate_color(C,Neg):- number(C),C<0,C is -Neg,!.
unnegate_color(CI,C):- compound(CI),CI= '-'(C),!.

arc_acolor(C,A):- nonvar(C),make_bg_visible(C,BGV),C\==BGV,!,arc_acolor(BGV,A).
arc_acolor(C,[underline,Color]):- unnegate_color(C,Neg),arc_acolor(Neg,Color).
arc_acolor(C,fg(Color)):- integer(C),block_colors(L),length(L,UpTo),between(0,UpTo,C),nth0(C,L,Color),!.
arc_acolor(A,fg(Color)):- atom(A),color_int(A,C),integer(C),block_colors(L),length(L,UpTo),between(0,UpTo,C),nth0(C,L,Color),!.
arc_acolor(C,fg(Color)):- attvar(C),cant_be_color(C,E),!,arc_acolor(-E,Color).
arc_acolor(C,Color):- mv_ansi_color(C,AC), (compound(AC) -> Color = AC ; (atomic(AC) -> Color = fg(AC); (C\==AC -> arc_acolor(AC,Color)))).
arc_acolor(C,underline):- var(C),!. %,trace_or_throw(var(arc_acolor(C))).
arc_acolor(fg(C),fg(Color)):- !, arc_acolor(C,Color).
arc_acolor(bg(C),bg(Color)):- !, arc_acolor(C,Color).
arc_acolor(hfg(C),hfg(Color)):- !, arc_acolor(C,Color).
arc_acolor(hbg(C),hbg(Color)):- !, arc_acolor(C,Color).
arc_acolor(C,fg(Color)):- atom(C),atom_concat('#',Rest,C),!,atom_length(Rest,6),!,Color=C.
arc_acolor(L,LL):- is_list(L),!,my_maplist(arc_acolor,L,LL).
arc_acolor(C,Color):- nonvar(C), color_int(C,I)->C\==I,!,arc_acolor(I,Color).
arc_acolor(_,[bold,underline]).

ansi_color_bg(C,bg(C1)):- arc_acolor(C,C2),C2=fg(C1),nonvar(C1),!.
ansi_color_bg(C,C1):- arc_acolor(C,C1).

:- define_into_module(on_bg/2).
on_bg(C,G):- ansi_color_bg(C,C1),ansi_format_real(C1,'~@',[call(G)]).
:- define_into_module(on_bg/1).
:- export(on_bg/1).
:- system:import(on_bg/1).
on_bg(G):- \+ ansi_main,wants_html,!,call(G).
on_bg(G):- ansi_format_real(reset,'~@',[call(G)]).

:- define_into_module(ansi_format_arc/3).
ansi_format_arc(Ansi,Format,Args):- on_bg(ansi_format_real(Ansi,Format,Args)),!.

underline_print(W):- ansi_format_real([bold,underline],'~@',[W]),!.


:- module_transparent(ansi_format_real/2).
:- module_transparent(bold_print/1).
bold_print(W):- ansi_format_real(bold,'~@',[W]),!.

compound_var(C,N):- \+ plain_var(C), \+ attvar(C), is_ftVar(C),arg(1,C,N).


:- multifile(color_decls/0).
:- dynamic(color_decls/0).
multivalued_peek_color(C,V):- color_decls,mv_peek_color(C,V).
is_bg_sym_or_var_ui(C):- color_decls,is_bg_sym_or_var(C).
bg_sym_ui(Bg):- (color_decls -> bg_sym(Bg); Bg==bg).


c_dot(CDot):- number(CDot),!,c_dot_num(CDot).
c_dot(CDot):- \+ atomic(CDot),!,fail.
c_dot(CDot):- \+ atom(CDot), \+ string(CDot),!,fail.
c_dot(CDot):- name(CDot,[Num]),c_dot_num(Num),!.
c_dot_num(64). %c_dot_num(169).

color_gl_int(C,W):- \+ atomic(C),W=C.
color_gl_int(C,W):- atom(C),named_colors(L),nth0(W,L,C),!.
color_gl_int('#6666FF',C):- name(C,[169]).
color_gl_int('#201020','.').
color_gl_int(C,C):-!.

:- dynamic(now_reserved_for_color/2).
c2s(0,'.'). c2s(1,C):- name(C,[169]). 

c2s(2,C):- name(C,[174]). c2s(2,'r'). 

c2s(3,'G').  c2s(4,'Y'). c2s(5,'s'). c2s(6,'v'). c2s(7,'o').

c2s(8,C):- name(C,[189]). c2s(8,'C').

c2s(9,'B'). c2s(10,'f').  c2s(11,'q'). c2s(12,'.'). c2s(13,','). c2s(14,'*'). 
c2g(N,W):- c2s(N,W),!.
c2g(N,W):- C is 100+N,name(W,[C]), \+ c2s(_,W),!.
c2g(2,W):- c2g(21,W).
c2g(N,W):- NN is N *10, int2glyph(NN,W),!.
color_gl_int_g(C,W):- nonvar(C),now_reserved_for_color(W,C),!.
color_gl_int_g(C,W):- color_gl_int(C,N),number(N), c2g(N,W),!,asserta(now_reserved_for_color(W,C)).
color_gl_int_g(C,W):- color_gl_int(C,W),!.

%color_print(_,W):- write(W),!.
:- export(color_print/2).
:- system:import(color_print/2).
:- meta_predicate(color_print(+,+)).

% color_print(C,CDot):- c_dot(CDot),nonvar(C),!, color_gl_int_g(C,W),!,color_print(C,W).


%color_print(C,W):- '$current_typein_module'(M), muarc_mod(MM), MM\==M, !,'$set_typein_module'(MM), module(MM),color_print(C,W).
color_print(C,W):- plain_var(C),W=='?',!,on_bg(magenta,color_print(C,W)).
color_print(C,W):- ansi_main, !, with_tty_true(color_print_ansi(C,W)).
color_print(C,W):- wants_html,!,color_print_webui(C,W).
color_print(C,W):- color_print_ansi(C,W),!.

color_print_ansi(C,W):- color_decls,multivalued_peek_color(C,V),!,color_print(V,W).
%color_print(C,W):- C == black,!, color_print(white,W).

color_print_ansi(C,W):- compound(W),compound_name_arity(W,call,_),!,(wots_hs(S1,call(call,W))->color_print(C,S1);color_print(C,failed(W))).

color_print_ansi(C,W):- (cant_be_color(C,CbC);cant_be_color(W,CbC)),!,arc_acolor(-CbC,Ansi),ansi_format_arc(Ansi,'~w',['^']),!.

color_print_ansi(C,W):- is_bg_sym_or_var_ui(C),W=='_',!,on_bg(write_nbsp),!.
color_print_ansi(C,W):- is_bg_sym_or_var_ui(C),W=='_',color_print(C,'+').
color_print_ansi(_,' '):- write_nbsp,!.


color_print_ansi(C,W):- plain_var(C),integer(W),arc_acolor(W,CI),!,ansi_format_arc(CI,'~w',[W]),!.
color_print_ansi(C,W):- plain_var(C),!,on_bg(ansi_format_real(italic,'~w',[W])),!.

color_print_ansi(C,W):- is_bg_sym_or_var_ui(C),is_bg_sym_or_var_ui(W), !, write_nbsp.

color_print_ansi(C,W):- string(W),is_list(C),!,ansi_format_arc(C,'~w',W).
color_print_ansi(C,W):- is_bg_sym_or_var_ui(C),integer(W),W<10, arc_acolor(W,CI),!,ansi_format_arc(CI,'~w',[W]),!.
%color_print(C,W):- (is_ftVar(C);(bg_sym_ui(C))), !, on_bg(yellow,var_color_print(C,W)).
color_print_ansi(C,W):- is_bg_sym_or_var_ui(C),!,on_bg(cyan,ansi_format_arc([],'~w',[W])),!.
color_print_ansi(_-C,W):- is_color(C),!, color_print(C,W).
color_print_ansi(C-_,W):- is_color(C),!, color_print(C,W).
%color_print(C-_,W):- !, color_print(C,W).
%color_print(C,W):- atom(C),color_int(C,N),integer(N),!,color_print(N,W).
color_print_ansi(C,W):- arc_acolor(C,Color),on_bg(ansi_format_arc(Color,'~w',[W])),!.
color_print_ansi(C,W):- C==0,!,ansi_format_arc([fg('#444444')],'~w',[W]),!.

color_name(C,W):- plain_var(C),!,W=C.
color_name(C,W):- color_name0(C,W),!.
color_name(C-_,W):- color_name0(C,W),!.
color_name(_-C,W):- !,color_name(C,W),!.

color_name0(C,W):- atom(C),atom_number(C,I),!,color_name0(I,W).
color_name0('@'(null),_).
color_name0(C,W):- atom(C),display_length(C,L),L>1,!,W=C.
color_name0(C,W):- integer(C),C>=0,named_colors(L),nth0(C,L,W),!.
color_name0(C,W):- mv_color_name(C,V),C\==V,color_name0(V,W).
color_name0(C,W):- is_color(C),!,W=C.

color_int(C,C):- var(C),!.
color_int(C-_,W):-!,color_int(C,W).
color_int(C,W):- integer(C),!,W=C.
color_int(C,0):- grid_zero(C),!.
color_int(C,W):- atom(C),named_colors(L),nth0(W,L,C),!.
color_int(C,C).

%grid_zero(zero).
grid_zero(black).
z_or_b(zero,black):- may_use_zero.
z_or_b(black,zero).

may_use_zero:- false.

:- export(grid_color_code/2).
grid_color_code(C,C):- var(C),!.
grid_color_code(0,Zero):- grid_zero(Zero).
grid_color_code(C-W,CC-W):- color_code(C,CC),!.
grid_color_code(C,CC):- color_code(C,CC),!.
%grid_color_code(C,CC):- is_grid_color(black-_),!,color_code(C,CC).
%grid_color_code(C,CC):- color_int(C,CC).

:- export(color_code/2).
color_code(C,W):- color_name(C,W).


%print_g(H,V,_,LH,LV,HH,HV):- (H<LH;H>HH;V<LV;V>HV),!, write('  ').

special_dot(Code):- var_dot(Code);bg_dot(Code);fg_dot(Code);grid_dot(Code);cant_be_dot(Code).

resrv_dot(Code):-  code_type(Code,white);code_type(Code,punct);code_type(Code,quote); 
 member(Code,`?.\u00AF\u00AF\u00AF\u00AF\u00AF```);special_dot(Code).



var_dot(63).
/* code=63 ?  code=183 \u00AF code=176 \u00AF code=186 \u00AF 170 \u00AF   */
bg_dot(32).
/* 169	\u00AF 248	\u00AF 216	\u00AF  215 \u00AF  174	\u00AF   */
%fg_dot(C):- luser_getval(fg_dot,C),integer(C),!.
%fg_dot(_):- luser_getval(no_rdot,true),luser_setval(no_rdot,false)-> ibreak , fail.
fg_dot(C):- luser_getval(alt_grid_dot,C),C\==[],!.
fg_dot(64).
%fg_dot(174).
cant_be_dot(183).
grid_dot(C):- luser_getval(alt_grid_dot,C),C\==[],!.
grid_dot(169).

%print_g(H,V,C0,_,_,_,_):- cant_be_color(C0),cant_be_color(C0,C),!,  ansi_format_arc([bold,fg('#ff8c00')],'~@',[(write('c'),user:print_g1(H,V,C))]).
%print_g(H,V,C0,_,_,_,_):- plain_var(C0),print_g1(H,V,C-'?'),!.
%print_g(H,V,C,_,_,_,_):- write_nbsp, print_g1(P2,H,V,C),!.

object_glyph(G,Glyph):- var(G),!,into_obj(G,O),object_glyph(O,Glyph).
object_glyph([G|Rid],Glyph):- is_grid([G|Rid]),!,grid_dot(Dot),name(Glyph,[Dot]).
object_glyph(obj(L),Glyph):- is_list(L),member(giz(glyph(Glyph)),L),!.
object_glyph(obj(L),Glyph):- is_list(L),member(was_oid(OID),L),atom_chars(OID,[o,'_',Glyph|_]),!.
object_glyph(G,Glyph):- is_object(G),!,obj_iv(G,Iv), int2glyph(Iv,Glyph).
object_glyph(G,Glyph):- nobject_glyph(G,Glyph).

nobject_glyph(G,Glyph):- integer(G), between(0,9,G),atom_number(Glyph,G),!.
nobject_glyph(G,Glyph):- plain_var(G),!,plain_var_glyph(G,Glyph).
nobject_glyph(G,Glyph):- compound_var(G,N),!,nobject_glyph(N,Glyph).
nobject_glyph(A,Glyph):- atom(A),atom_chars(A,Chars),last(Chars,Glyph),!.
nobject_glyph(G,Glyph):- term_to_atom(G,A),nobject_glyph(A,Glyph).

int2glyph(GN2,Glyph):- int2glyph0(GN2,Glyph),!.%unwonk_ansi(Glyph).
int2glyph0(GN2,Glyph):- quietly(int2glyph1(GN2,Glyph)),!.
int2glyph0(GN,Glyph):- GN > 255, GN2 is GN div 2 + (GN rem 2), int2glyph1(GN2,Glyph),!.
int2glyph0(GN2,Glyph):- itrace,i_sym(GN2,GN),!,i_glyph(GN,Glyph),!.

int2glyph1(GN2,Glyph):- i_sym(GN2,GN),i_glyph(GN,Glyph),atom(Glyph),!.
int2glyph1(GN,Glyph):- GN > 255, GN2 is GN div 2 + (GN rem 2), int2glyph1(GN2,Glyph),!.


%user:portray(S):- (string(S);atom(S)),atom_codes(S,[27|_]),write('"'),write(S),write('"').

%print_gw1(C):- plain_var(C), write('  '),!.

print_gw1(N):- print_gw1(color_print_ele,N),!.
print_gw1(N):- print(N),!.

print_gw1(P2,N):-  
 wots_hs(S,(((get_bgc(BG),is_color(BG), once(( ( \+ is_black(BG))-> call(P2,BG,'.');write_nbsp);write(',')));write_nbsp),!,
  (print_g1(P2,N);write('.')))),!, gws(S).

gws(S):- write(S),!.
%gws(S):- display_length(S,L),(L=28->(write(L),atom_codes(S,Codes),arc_assert(ac(S)));write(S)).
%print_gw1(N):- compound(N),N = C-W,!,color_print(C,W),!.

%mregression_test:- mregression_test(print_grid).
mregression_test:- mregression_test(print_grid_html(_,_,_,_)).

mregression_test(P1):- G = [[1,2,3],[1,2,3],[1,2,3]], call(P1,G).
mregression_test(P1):- G = [[1,2,3],[1,_,3],[1,2,3]], call(P1,G).
mregression_test(P1):- G = [[A,2,3],[_,2,_],[A,2,3]], call(P1,G).
mregression_test(P1):- G = [['$VAR'(1),2,3],[_,2,'$VAR'(3)],[_,2,'$VAR'('Good')],['$VAR'(1),2,3]], call(P1,G).

mregression_test(P1):- call(P1,[[_,_-green]]).

mregression_test(P1):- call(P1,[[_17910,_17922-green,_17934-green,_17946-green,_17952,_17964-green,_17970,_17982-cyan,_17994-cyan,_18000,_18012-cyan,_18024-cyan,_18036-cyan,_18048-cyan,_18054,_18066-cyan,_18078-cyan,_18084,_18096-green,_18102,_18114-green,_18126-green,_18138-green,_18144],[_17712-green,_17718,7-green,_17736,5-green,_17754,3-cyan,_17772,5-cyan,_17790,_17796,_17802,_17808,_17814,_17820,5-cyan,_17838,3-cyan,_17856,5-green,_17874,7-green,_17892,_17904-green],[_17436-green,7-green,7-green,7-green,8-green,5-green,3-cyan,5-cyan,4-cyan,_17538,3-cyan,6-cyan,6-cyan,3-cyan,_17592,4-cyan,5-cyan,3-cyan,5-green,8-green,7-green,7-green,7-green,_17700-green],[_17220-green,_17226,7-green,_17244,6-green,4-green,_17274,_17280,_17286,2-cyan,_17304,8-cyan,8-cyan,_17334,2-cyan,_17352,_17358,_17364,4-green,6-green,_17394,7-green,_17412,_17424-green],[_16998,5-green,8-green,6-green,_17040,_17046,1-cyan,_17064,3-cyan,_17082,_17088,5-cyan,5-cyan,_17118,_17124,3-cyan,_17142,1-cyan,_17160,_17166,6-green,8-green,5-green,_17208],[_16764-green,_16770,5-green,4-green,_16800,
 1-green,2-cyan,_16830,3-cyan,4-cyan,2-cyan,_16872,_16878,2-cyan,4-cyan,3-cyan,_16920,2-cyan,1-green,_16950,4-green,5-green,_16980,_16992-green],[_16494,3-cyan,3-cyan,_16524,1-cyan,2-cyan,4-purple,6-purple,5-purple,6-purple,7-purple,7-purple,7-purple,7-purple,6-purple,5-purple,6-purple,4-purple,2-cyan,1-cyan,_16722,3-cyan,3-cyan,_16752],[_16272-cyan,_16278,5-cyan,_16296,_16302,_16308,6-purple,6-purple,_16338,7-purple,9-purple,10-purple,10-purple,9-purple,7-purple,_16416,6-purple,6-purple,_16446,_16452,_16458,5-cyan,_16476,_16488-cyan],[_16032-cyan,5-cyan,4-cyan,_16062,3-cyan,3-cyan,5-purple,_16104,_16110,6-purple,_16128,9-purple,9-purple,_16158,6-purple,_16176,_16182,5-purple,3-cyan,3-cyan,_16224,4-cyan,5-cyan,_16260-cyan],[_15786,_15792,_15798,2-cyan,_15816,4-cyan,6-purple,7-purple,6-purple,8-purple,9-purple,10-purple,10-purple,9-purple,8-purple,6-purple,7-purple,6-purple,4-cyan,_15990,2-cyan,_16008,_16014,_16020],[_15552-cyan,_15558,3-cyan,_15576,_15582,2-cyan,4-purple,5-purple,_15624,5-purple,7-purple,7-purple,8-purple,10-purple,9-purple,_15702,9-purple,7-purple,2-cyan,_15744,_15750,3-cyan,_15768,_15780-cyan],[_15300-cyan,_15306,6-cyan,8-cyan,5-cyan,_15348,4-blue,7-blue,7-blue,7-blue,4-blue,_15414,_15420,8-purple,10-purple,9-purple,10-purple,7-purple,_15486,5-cyan,8-cyan,6-cyan,_15528,_15540-cyan],[_15048-cyan,_15054,6-cyan,8-cyan,5-cyan,_15096,7-blue,11-blue,11-blue,11-blue,7-blue,_15162,_15168,8-purple,10-purple,9-purple,10-purple,7-purple,_15234,5-cyan,8-cyan,6-cyan,_15276,_15288-cyan],[_14802-cyan,_14808,3-cyan,_14826,_14832,2-cyan,7-blue,11-blue,11-blue,11-blue,7-blue,4-purple,8-purple,10-purple,9-purple,_14958,9-purple,7-purple,2-cyan,_15000,_15006,3-cyan,_15024,_15036-cyan],[_14556,_14562,_14568,2-cyan,_14586,4-cyan,7-blue,11-blue,11-blue,11-blue,7-blue,7-purple,10-purple,9-purple,8-purple,6-purple,7-purple,6-purple,4-cyan,_14760,2-cyan,_14778,_14784,_14790],[_14304-cyan,5-cyan,4-cyan,_14334,3-cyan,3-cyan,4-blue,7-blue,7-blue,7-blue,4-blue,8-purple,9-purple,_14448,6-purple,_14466,_14472,5-purple,3-cyan,3-cyan,_14514,4-cyan,5-cyan,_14550-cyan],[_14076-cyan,_14082,5-cyan,_14100,_14106,_14112,4-purple,5-purple,_14142,5-purple,8-purple,10-purple,10-purple,9-purple,7-purple,_14220,6-purple,6-purple,_14250,_14256,_14262,5-cyan,_14280,_14292-cyan],[_13806,3-cyan,3-cyan,_13836,1-cyan,2-cyan,4-purple,6-purple,5-purple,6-purple,7-purple,7-purple,7-purple,7-purple,6-purple,5-purple,6-purple,4-purple,2-cyan,1-cyan,_14034,3-cyan,3-cyan,_14064],[_13572-green,_13578,5-green,4-green,_13608,
 1-green,2-cyan,_13638,3-cyan,4-cyan,2-cyan,_13680,_13686,2-cyan,4-cyan,3-cyan,_13728,2-cyan,
 1-green,_13758,4-green,5-green,_13788,_13800-green],[_13350,5-green,8-green,6-green,_13392,_13398,-1-cyan,_13416,3-cyan,_13434,_13440,5-cyan,5-cyan,_13470,_13476,3-cyan,_13494,1-cyan,_13512,_13518,
 6-green,8-green,5-green,_13560],[_13140-green,_13146,7-green,_13164,6-green,4-green,_13194,_13200,_13206,2-cyan,_13224,8-cyan,8-cyan,_13254,2-cyan,_13272,_13278,_13284,4-green,6-green,_13314,7-green,_13332,_13344-green],[_12864-green,7-green,7-green,7-green,8-green,5-green,3-cyan,5-cyan,4-cyan,_12966,3-cyan,6-cyan,6-cyan,3-cyan,_13020,4-cyan,5-cyan,3-cyan,5-green,8-green,7-green,7-green,7-green,_13128-green],[_12660-green,_12666,7-green,_12684,5-green,_12702,3-cyan,_12720,5-cyan,_12738,_12744,_12750,_12756,_12762,_12768,5-cyan,_12786,3-cyan,_12804,5-green,_12822,7-green,_12840,_12852-green],[_12414,_12426-green,_12438-green,_12450-green,_12456,_12468-green,_12474,_12486-cyan,_12498-cyan,_12504,_12516-cyan,_12528-cyan,_12540-cyan,_12552-cyan,_12558,_12570-cyan,_12582-cyan,_12588,_12600-green,_12606,_12618-green,_12630-green,_12642-green,_12648]]).
%print_g1(C):- compound_var(C,N),underline_print(print_g1(N)),!.

print_g1(CG):- compound(CG),CG=( '' - Sym),nonvar(Sym),!,print_g1(Sym).
print_g1(CG):- \+ ansi_main, wants_html,with_toplevel_pp(ansi,( \+ wants_html,print_g1(CG))),!.
print_g1(CG):- \+ \+ print_g1(color_print_ele,CG),!.

print_g1(P2,CG):- arc_html,with_toplevel_pp(ansi,(\+ arc_html, print_g1(P2,CG))),!.
print_g1(P2,C):- P2==color_print_ele, compound(C),C=n(CbC),color_print(CbC,"^"),!.
print_g1(P2,C):- P2==color_print_ele, C==black,color_print_ele(black,'.').
print_g1(P2,C):- P2==color_print_ele, C=='$VAR'('_'),underline_print(color_print_ele(black,'.')).
print_g1(P2,C):- P2==color_print_ele, compound(C),C = '$VAR'(N),number(N),N=<24,underline_print(print(C)).
print_g1(P2,C):- P2==color_print_ele, compound(C),C = '$VAR'(N),number(N),int2glyph(N,S),underline_print(write(S)),!.
print_g1(P2,C):- P2==color_print_ele, compound(C), C='$VAR'(_),underline_print(print(C)),!.
print_g1(P2,C):- C == ((+) - wbg),!,call(P2,wbg,(+)).
print_g1(P2,C):- P2==color_print_ele, cant_be_color(C,CbC),arc_acolor(-CbC,Ansi),ansi_format_arc(Ansi,'~w',[' ']),!.
print_g1(P2,C):- multivalued_peek_color(C,V),C\==V,!,print_g1(P2,V).
print_g1(P2,C):- plain_var(C), write_nbsp,!, nop(( nobject_glyph(C,G),underline_print(print_g1(P2,G-G)))),!.
print_g1(_ ,C):- get_black(Black),C == Black,!, write_nbsp.
print_g1(_ ,C-CC):- get_black(Black), C == Black,CC == Black,!,write_nbsp,!.
print_g1(P2,N):- is_grid(N),call(P2,magenta,'G'),!.
print_g1(P2,C):- is_bg_color(C),get_bgc(BG),\+ attvar(C),!,call(P2,bg(BG),' '),!.
print_g1(P2,N-C):- plain_var(N),print_g1(P2,C).
print_g1(P2,C-N):- plain_var(N),print_g1(P2,C).
print_g1(P2,N-C):- integer(N),is_color(C),!,e_int2glyph(N,G),call(P2,C,G).
print_g1(P2,C-N):- integer(N),is_color(C),!,e_int2glyph(N,G),call(P2,C,G).
print_g1(P2,C):- compound_var(C,N),nobject_glyph(N,G),underline_print(print_g1(P2,G-G)),!.
print_g1(P2,N):- \+ compound(N), \+ is_colorish(N), print_g1(P2,N-N).
print_g1(P2,N):- into_color_glyph(N,C,Code),as_name(Code,S), call(P2,C,S),!.

                         

color_print_ele(C,G):- atom(G),atom_concat('o_',S,G),atom_chars(S,[AC|_]),atom_chars(GS,[AC]),!,color_print_ele(C,GS).
color_print_ele(C,G):- \+ ansi_main,wants_html,!,color_print_webui(C,G),!.
color_print_ele(C,G):- color_print(C,G),!.

plain_var_glyph(C,Name):- plain_var(C), get_var_name_or_fake(C,Fake),format(chars(Codes),'~w',[Fake]),
  include(lambda_rev(code_type(digit)),Codes,NewCodes),
  (NewCodes==[]-> last(Codes,Name) ; (atom_chars(Atom,NewCodes), atom_number(Atom,Int),i_glyph(Int,Name))).
%print_g1(C,S):- atom_number(S,_), cant_be_dot(DOT), name(N,[DOT]), color_print(C,N),!.
%print_g1(C,S):- color_print(C,S),!.

as_name(N,Glyph):- plain_var(N),!,plain_var_glyph(N,Glyph).
as_name(Code,S):- atom(Code), S=Code.
as_name(Code,S):- Code>0, integer(Code), name(S,[Code]).

into_color_glyph_ez(CTerm,Color,Code):- 
    ignore((sub_term(Color,CTerm),nonvar_or_ci(Color),is_color(Color))),
    ignore((sub_term(A,CTerm),atom(A), \+ is_color(A), i_glyph(A,Glyph))),
    ignore((sub_term(Nth,CTerm),integer(Nth),i_glyph(Nth,Glyph))),
    ignore((nonvar_or_ci(Glyph),name(Glyph,[Code|_]))).

%into_color_glyph(H,V,CTerm,Color,Code):- fail, get_grid_num_xyc(H,V,SColor,SNth),into_color_glyph(SColor+SNth+CTerm,Color,Code),nonvar_or_ci(Code).

color_fint('#6666FF','1').
color_fint('#201020',' ').
color_fint(C,N):- color_int(C,N),number(N),!.
%format(atom(N),'~w',[CN]).
into_color_number(C,C,N):- is_color(C),color_fint(C,CN),format(chars([N|_]),'~w',[CN]).
%into_color_number(C,C,N):- format(chars([_,_,N|_]),'~w',[C]).
into_color_number(C,C,' ').

into_color_glyph(CN,C,N):- fg_dot(color_number),!,into_color_number(CN,C,N).
into_color_glyph(C,wbg,VAR):- plain_var(C),var_dot(VAR),!.
into_color_glyph(N,C,DOT):- is_bg_color(N),get_bgc(C),bg_dot(DOT).
into_color_glyph(N,C,DOT):- is_spec_fg_color(N,C),fg_dot(DOT).
into_color_glyph(N,C,DOT):- is_fg_color(N),N=C,fg_dot(DOT).
into_color_glyph(N,C,DOT):- is_bg_color(N),C=N,bg_dot(DOT).
into_color_glyph(N,C,DOT):- cant_be_color(N,C),cant_be_dot(DOT).
into_color_glyph(Obj,C,G):- is_object(Obj),color(Obj,C),object_glyph(Obj,G),!.
into_color_glyph(N,C,Code):- compound(N),N=(C-G),is_color(C),Code=G.
into_color_glyph(N,C,Code):- compound(N),N=(G-C),is_color(C),Code=G.
into_color_glyph(CTerm,Color,Code):- compound(CTerm),into_color_glyph_ez(CTerm,Color,Code),nonvar_or_ci(Code),!.
into_color_glyph(N,C,Glyph):- var(N),C=N,sformat(chars(Codes),'~p',[N]),last(Codes,Glyph),!.
into_color_glyph(N,Black,BGD):- get_bg_label(BGL),BGL==N, get_bgc(Black), bg_dot(BGD),!.
into_color_glyph(0,Black,BGD):- get_bg_label(BGL),!, into_color_glyph(BGL,Black,BGD).
into_color_glyph(N,FGL,FGD):- get_fg_label(FGL),FGL==N, fg_dot(FGD),!.

i_glyph(N,Glyph):- notrace((i_glyph0(N,Glyph),atom(Glyph))),!.
i_glyph(_,Glyph):- bg_dot(Code), name_safe(Glyph,[Code]),!.
%i_glyph(N,Glyph):- i_glyph0(N,Glyph),!.%:- atrace,i_glyph0(N,Glyph),atom(Glyph),!.

name_safe(Glyph,Codes):- catch(name(Glyph,Codes),_,fail).

i_glyph0(N,C):-i_glyph1(N,Glyph),!,atom_chars(Glyph,[C|_]).
i_glyph1(NIL,'?'):- NIL==nil,!.
i_glyph1(N,Glyph):- bg_sym_ui(BG), BG==N, bg_dot(Code), name_safe(Glyph,[Code]),!.
i_glyph1(Code,Glyph):- integer(Code), Code> 255, name_safe(Glyph,[Code]),!.
i_glyph1(N,Glyph):- integer(N),quietly((i_sym(N,Code),name_safe(Glyph,[Code]))),!.
i_glyph1(N,Glyph):- number(N), N>10, integer(N),N3 is N div 3, i_glyph1(N3,Glyph),!.
i_glyph1(N,Glyph):- plain_var(N),format(chars(Codes),'~p',[N]),last(Codes,Glyph),!.
i_glyph1(N,Glyph):- atom(N),name_safe(N,[_111,95,Code|_])->name_safe(Glyph,[Code]),!.
i_glyph1(N,Glyph):- atom(N),atom_number(N,Num),i_glyph(Num,Glyph),!.
i_glyph1(N,Glyph):- atom(N),name_safe(N,[Code])->name_safe(Glyph,[Code]),!.
%i_glyph(N,Glyph):- atom(N),atom_chars(N,Chars),last(Chars,LGlyph),upcase_atom(LGlyph,Glyph).
                                                                            
i_sym(N2,Code):- integer(N2),!, N is N2+160, change_code(N,NN), iss:i_syms(Codes),nth0(NN,Codes,Code),!.
i_sym(N2,Code):- atom(N2),name_safe(N2,[C|_]),i_sym(C,Code),!.
i_sym(N,Code):- plain_var(N), Code = 63.
%change_code(N,M):- M is N * 100,!.
%change_code(N,M):- N>10, M is (N * 10 ),!.
change_code(N,M):- M is N.


unwonk_ansi(Y,YY):- string(Y),!,atomics_to_string(List,"\u0083",Y),atomics_to_string(List,YY).
unwonk_ansi(Y,YY):- is_list(Y),!,maplist(unwonk_ansi,Y,YY).
unwonk_ansi(Y, Y):- \+ callable(Y),!.
unwonk_ansi(Y,YY):- Y = oid_glyph_object(OID,Glyph,Obj), YY = oid_glyph_object(YOID,YGlyph,YObj),
  unwonk_atom(OID,Number,YOID),
  unwonk_ansi(Obj,YObj),  
  unwonk_atom(Glyph,Number,YGlyph),!,
  format(user_error,'~N% ~p.~n',[YY]).
   
unwonk_ansi(Y,YY):- compound(Y),!,compound_name_arguments(Y,F,A),maplist(unwonk_ansi,A,AA),compound_name_arguments(YY,F,AA),!.
unwonk_ansi(Y, Y):- \+ atom(Y),!.
unwonk_ansi(Y,YY):- unwonk_atom(Y,YY,_),!.
unwonk_ansi(Y,Y).


unwonk_atom(Y,YY,Number):- atom_codes(Y,[111,95,195,402|_Rest]),atomic_list_concat([o,Bad,ANumber|GID_L],'_',Y),
  atom_number(ANumber,Number),
  unbad_glyph(Bad,Number,Glyph),!,                            atomic_list_concat([o,Glyph,ANumber|GID_L],'_',YY).
unwonk_atom(Y,YY,Number):- atom_codes(Y,[195,402|_]), glyph_b_g(YY,Number,Y),!.
unwonk_atom(Y,YY,_):- atomics_to_string(List,"\u0083",Y),atomic_list_concat(List,YY).
 

unbad_glyph(Bad,Number,Glyph):- glyph_b_g(Glyph,N,Bad),!,ignore(Number=N).
unbad_glyph(Bad,Number,Glyph):- int2glyph(Number,Glyph),asserta(glyph_b_g(Glyph,Number,Bad)),!.

:- dynamic(glyph_b_g/3).


print_g1(P2,_,_, E):- print_g1(P2,E),!. 
%print_g1(_,_,C):- atrace, write(C).

code_not_bfly(Code):- between(170,inf,Code).

html_utf(utf8).

save_codes(Max):- 
 %stream_property(File,file_no(1)),
 html_utf(UTF8),
 locally(set_prolog_flag(encoding,UTF8),
  ((with_output_to(codes(CCC),
 ((  forall((
     between(160,Max,Code),     
     %code_type(Code,graph),  
  \+ code_type(Code,white),
  \+ between(688,1000,Code),
  \+ between(1350,4600,Code),
  \+ between(4650,5000,Code),
  \+ between(5850,11500,Code),
  \+ between(42560,42600,Code),
  \+ between(4602,4609,Code),
  % Code \== 4605 ,
  % % check_dot_spacing(Code),
  %(42600 > Code),
   is_single_char_ifiable_quiet(Code),
  \+ resrv_dot(Code)
   
   % ignore((0 is Code mod 50, format(File,'\n\n~d:',[Code]), put_code(File,Code))),
  ),put_code(Code))))),
  sort_safe(CCC,CCCO),
  assertz(iss:i_syms(CCCO))))).

is_single_char_ifiable_quiet(Code):- with_output_to(string(S),put(Code)),sformat(SS,'~q',[S]),!, display_length(SS,3),!.

is_single_char_ifiable(Code):- with_output_to(string(S),put(Code)),sformat(SS,'~q',[S]),!,is_single_char_ifiable(Code,SS).
is_single_char_ifiable(_,SS):- display_length(SS,3),!.
is_single_char_ifiable(C,SS):- writeln(user_error,C=SS),!,fail.

test_is_single_char_ifiable:-
 retract(iss:i_syms(CCC)), 
 include(is_single_char_ifiable,CCC,CCCO),
 assert(iss:i_syms(CCCO)).

is_html_ifiable(Code):- sformat(S,'~@',[as_html_encoded(put_code(Code))]), display_length(S,1),!.
is_html_ifiable(Code):- format('~@',[as_html_encoded(put_code(Code))]),!,fail.

save_codes:- save_codes(42600).

check_dot_spacing(CCC):- 
 html_utf(UTF8),
 locally(set_prolog_flag(encoding,UTF8),format(user_error,'~@', 
  [ignore((color_print(red, call(format('~n~w = |~s|',[CCC,[CCC,32,CCC,32,CCC,32,CCC,32,CCC,32]])))))])).

check_dot_spacing:- iss:i_syms(CCC),my_maplist(check_dot_spacing,CCC),!.

:- retractall(iss:i_syms(_)).
isc:- ignore(save_codes).
:- initialization(isc).

test_show_color_on_reload:- prolog_load_context(reloading,true)-> test_show_colors ; true.


/*
get_glyph(Point,Glyph):-  
  get_grid_num(Point,N),i_glyph(N,Glyph).
*/
:- include(kaggle_arc_footer).

:- initialization(test_show_color_on_reload,now).
%:- fixup_module_exports_now.
/*
12545 = |? ? ? ? ? |
12546 = |? ? ? ? ? |
12547 = |? ? ? ? ? |
12548 = |? ? ? ? ? |
2246 = |? ? ? ? ? |
12247 = |? ? ? ? ? |
12248 = |? ? ? ? ? |
12249 = |? ? ? ? ? |
12250 = |? ? ? ? ? |
12251 = |? ? ? ? ? |
12252 = |? ? ? ? ? |
12253 = |? ? ? ? ? |
12254 = |? ? ? ? ? |
12255 = |? ? ? ? ? |
12256 = |? ? ? ? ? |
12257 = |? ? ? ? ? |
12258 = |? ? ? ? ? |
12259 = |? ? ? ? ? |
12260 = |? ? ? ? ? |
12261 = |? ? ? ? ? |
12262 = |? ? ? ? ? |
12263 = |? ? ? ? ? |
12264 = |? ? ? ? ? |
12265 = |? ? ? ? ? |
12266 = |? ? ? ? ? |
12267 = |? ? ? ? ? |
12268 = |? ? ? ? ? |
12269 = |? ? ? ? ? |
12270 = |? ? ? ? ? |
12271 = |? ? ? ? ? |

*/


