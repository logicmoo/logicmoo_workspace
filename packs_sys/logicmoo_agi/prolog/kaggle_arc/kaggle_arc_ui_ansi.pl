/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/
:- if(current_module(trill)).
:- set_prolog_flag_until_eof(trill_term_expansion,false).
:- endif.

wno(G):-
 locally(b_setval(print_collapsed,10), G).

print_collapsed(Size,G):- 
 locally(b_setval(print_collapsed,Size), print_collapsed0(Size,G)).

print_collapsed0(Size,G):- Size<10, !, call(G). 
% print_collapsed(Size,G):-  call(G). 
print_collapsed0(Size,G):- Size>=10, !, wots(_S,G).
print_collapsed0(_,G):- wots(S,G),write(S).

tersify(I,O):- tracing,!,I=O.
%tersify(I,O):- term_variables(I,Vs), \+ ( member(V,Vs), attvar(V)),!,I=O.
tersify(I,O):- quietly((tersify2(I,M),tersify3(M,O))).

%srw_arc(I,O):- is_grid(I),!, wots(O,(write('"'),print_grid(I),write('"'))).
%srw_arc(I,O):- compound(I),!, wots(O,(write(ptt(I)))).
/*
srw_arc(I,O):- is_grid(I),!, wots(O,(write('"'),print_grid(I),write('"'))).
*/
srw_arc(I,O):- is_map(I),!, O='..vvmm..'.
srw_arc(I,O):- is_grid(I),!, O='..grid..'.
%srw_arc(gridFn(_),gridFn):-!.
%srw_arc(I,O):- is_points_list(I), length(I,N),N>10,!,O='..points..'(N),!.
%srw_arc(I,O):- is_list(I), length(I,N),N>10,!,O='..points..'(N),!.
srw_arc(I,O):- tersify(I,O),I\==O,!.

:- multifile(dumpst_hook:simple_rewrite/2).
:- dynamic(dumpst_hook:simple_rewrite/2).

%dumpst_hook:simple_rewrite(I,O):- compound(I), srw_arc(I,O), I\=@=O.


portray_terse:- true,!.

:- discontiguous arc_portray/2. 

arc_portray(Map,TF):- get_map_pairs(Map,Type,Pairs),!, arc_portray_pairs(Type,TF,Pairs). 

arc_portray_t(G, _):- is_map(G), !, write_map(G,'arc_portray_t').
arc_portray_t(G, _):- is_grid(G),  !, data_type(G,W),writeq(grid(W)).

arc_portray(G, _):- is_map(G),  !, write_map(G,'arc_portray').
arc_portray(G, TF):- TF == true, portray_terse, arc_portray_t(G, TF),!.
arc_portray(G, TF):- catch(arc_portray_nt(G, TF),_,fail),!.
%arc_portray(G, _TF):- writeq(G),!.

% Portray In Debugger

arc_portray_nt(G,false):- is_grid(G), print_grid(G),!.

arc_portray_nt(G0, _):- is_list(G0), length(G0,L),L>3, ptt(G0),!.
arc_portray_nt(G0, true):- is_group(G0), ptt(G0),!.
%arc_portray_nt(G0, false):- is_group(G0), ptt(G0),!.
arc_portray_nt(G0, false):- is_group(G0), into_list(G0,G), length(G,L),% L>1, !,
   dash_chars, 
   once(((why_grouped(_TestID,Why,WG),WG=@=G,fail);(Why = (size=L)))),!,
   print_grid(Why,G),nl,
   %underline_print(writeln(Why)),
   maplist(print_info,G),
   dash_chars.


arc_portray_nt(G,_False):- is_object(G), wots(S,ptt(G)), debug_as_grid(S,G).
  %object_grid(G,OG), 
  %neighbor_map(OG,NG), !,
  %print_grid(object_grid,NG),nl,
  %underline_print(debug_indiv(G)),
  
arc_portray_nt(G,false):- via_print_grid(G),!, grid_size(G,H,V),!,H>0,V>0, print_grid(H,V,G).

% Portray In tracer
arc_portray_nt(G,true):- is_object(G),underline_print((ptt(G))).
arc_portray_nt(G,true):- via_print_grid(G),write_nbsp,underline_print((ptt(G))),write_nbsp.
arc_portray_nt(G,true):- tersify(G,O),write_nbsp,writeq(O),write_nbsp.


arc_portray_pairs(Type,TF,Pairs):- 
  length(Pairs,N),
  writeln(arc_portray_pairs(Type,TF,len(N))),
  swap_kv(Pairs,VKPairs),
  keysort(VKPairs,SVKPairs),
  maplist(arg(2),SVKPairs,SVKPairs2),
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
 format('~N'),
 arc_portray_1_pair(Ps,K,Val,TF),
 format('~N').

arc_portray_1_pair(_Ps,call,Val,_TF):- !, call(Val).
arc_portray_1_pair(Ps,K,Val,TF):- 
 (via_print_grid(Val) -> print_grid(K,Val) 
   ;  (print(K),write('= '),once(arc_portray(Val,TF);print(Val)))),
 ignore(arc_portray_pair_optional(Ps,K,Val,TF)),!.

arc_portray_pair_optional(Ps,K,Val,TF):-
 once(( Val\==[], is_list(Val),maplist(is_object,Val),
  print_info(Val),
  Val \= [_], 
  compare_objects(Val,Diffs),  
  color_print(cyan,call(arc_portray_pair(Ps,diffs(K),Diffs,TF))))).


% arc_portray(G):- \+ \+ catch((wots(S,( tracing->arc_portray(G,true);arc_portray(G,false))),write(S),ttyflush),_,fail).
arc_portray(G):- is_vm(G), !, write('..VM..').
arc_portray(G):- \+ nb_current(arc_portray,t),is_print_collapsed,!, locally(nb_setval(arc_portray,t),arc_portray(G)).
arc_portray(G):- compound(G), \+ \+ catch(((tracing->arc_portray(G,true);arc_portray(G,false)),ttyflush),E,(format(user_error,"~N~q~n",[E]),fail)).

  

via_print_grid(G):- is_points_list(G),!,fail,grid_size(G,H,V),number(H),number(V),H>1,V>1.
via_print_grid(G):- is_grid(G).
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


tersify0(I,av(I,Others)):- attvar(I),copy_term(I,C,Attrs),C=I,terseA(I,Attrs,Others),!.
tersify0(I,I):- var(I),!.


%tersifyC(D):- is_map(D),!.
tersifyC(av(_,_)).
tersifyC(objFn(_,_)).
tersifyC(groupFn(_,_)).
tersifyC(objFn(_)).
tersifyC(groupFn(_)).

tersify1(av(_,Blue), -(Blue)):-!.
tersify1(I,O):- compound(I), tersifyC(I),!,I=O.
tersify1(gridFn(I),gridFn(I)):-!. % tersifyG(I,O).
%tersify1(gridFn(I),gridFn(O)):-tersifyG(I,O).
tersify1(Nil,[]):- Nil == [],!.
tersify1(I,gridFn(S)):- is_grid(I), into_gridnameA(I,O),!,sformat(S,'~w',[O]).
tersify1(I,gridFn(O)):- is_grid(I),tersifyG(I,O),!.
tersify1(I,groupFn(O,List)):- is_group(I), mapgroup(tersify1,I,List),mapgroup(obj_to_oid,I,OIDs),length(List,N), !,ignore((get_current_test(TestID),is_why_grouped(TestID,N,Why,OIDs),!,O=Why)).
tersify1(I,Q):- is_object(I),object_glyph_color(I,FC), o2g(I,O),!,wots(A,color_print(FC,call(format('"~w"',[O])))),
   amass(I,M),
   wots(S,call(write(objFn(A,M)))),atom_string(Q,S).
tersify1(I,O):- is_map(I), get_kov(objs,I,_),!, O='$VAR'('VM').
tersify1(I,O):- is_map(I), get_kov(pairs,I,_),!, O='$VAR'('Training').


tersifyG(I,O):- tersifyL(I,O),numbervars(O,1,_,[attvar(bind),singletons(false)]),!.
tersifyL(I,O):- is_list(I), maplist(tersifyL,I,O),!.
tersifyL(I,O):- tersify0(I,O),!.
tersifyL(I,O):- tersify1(I,O),!.
tersifyL(I,I).

tersify2(I,O):- compound(I),tersify1(I,O),!.
tersify2(I,O):- tersify0(I,O),!.
tersify2(I,O):- is_list(I), !, maplist(tersify2,I,O).
tersify2(I,O):- compound(I), !, compound_name_arguments(I,F,IA), maplist(tersify,IA,OA), compound_name_arguments(O,F,OA).
tersify2(I,I).

tersify3(I,O):- compound(I),tersify1(I,O),!.
tersify3(I,O):- tersify0(I,O),!.
tersify3([H|I],O):- is_list(I),  with_output_to(string(S),display(I)), ((atom_length(S,N), N>170) -> 
  (length(I,LL),tersify(H,HH),(('...'(HH,LL,'...'(N)))=O)); I=O),!.
tersify3(I,O):- is_list(I), !, maplist(tersify3,I,O).
tersify3(I,O):- compound(I), !, compound_name_arguments(I,F,IA), maplist(tersify,IA,OA), compound_name_arguments(O,F,OA).
tersify3(I,I).

write_map(G,Where):- is_vm(G), !, write('...VM_'),write(Where),write('...').
write_map(G,Where):- is_dict(G), !, write('...Dict_'),write(Where),write('...').
write_map(G,Where):- is_map(G), !, write('...Map_'),write(Where),write('...').
write_map(_G,Where):- write('...'),write(Where),write('...').

ptt(_):- is_print_collapsed,!.
ptt(G):- is_map(G), !, write_map(G,'ptt').
ptt(S):- term_is_ansi(S), !, write_keeping_ansi(S).
ptt(P):- \+ \+ ((tersify(P,Q),!,pt(Q))),!.
ptt(C,P):- \+ \+ ((tersify(P,Q),!,pt(C,Q))),!.

pt(Color,P):- quietlyd((wots(S,ptcol(P)),!,color_print(Color,S))).
ptcol(call(P)):- callable(P),!,call(P).
ptcol(P):- pt(P).
ptc(Color,Call):- pt(Color,call(Call)).

pt(_):- is_print_collapsed,!.
pt(_):- format('~N'), fail.
pt(P):- var(P),!,pt(var(P)).
pt(P):- atomic(P),atom_contains(P,'~'),!,format(P).
pt(G):- is_map(G), !, write_map(G,'pt').
pt(S):- term_is_ansi(S), !, write_keeping_ansi(S).
pt(P):- \+ \+ (( pt_guess_pretty(P,GP),ptw(GP))).
%pt(P):-!,writeq(P).
%ptw(P):- quietlyd(print_tree_nl(P)),!.
ptw(G):- is_map(G), !, write_map(G,'ptw').
ptw(S):- term_is_ansi(S), !, write_keeping_ansi(S).
ptw(P):- quietlyd(print_tree_nl(P)),!.
%ptw(P):- quietlyd(write_term(P,[blobs(portray),quoted(true),quote_non_ascii(false), portray_goal(print_ansi_tree),portray(true)])),!.

pt_guess_pretty(P,O):- \+ nb_current(in_pt_guess_pretty,t), locally(nb_setval(in_pt_guess_pretty,t),pt_guess_pretty_1(P,O)).
pt_guess_pretty(O,O).

pt_guess_pretty_1(P,O):- copy_term(P,O,_),
  ignore((sub_term(Body,O), compound(Body), Body=was_once(InSet,InVars),maplist(upcase_atom_var,InSet,InVars))),
  ignore(pretty1(O)),ignore(pretty_two(O)),ignore(pretty_three(O)),ignore(pretty_final(O)),!,
  ((term_singletons(O,SS),numbervars(SS,999999999999,_,[attvar(skip),singletons(true)]))).

:- dynamic(pretty_clauses:pp_hook/3).
:- multifile(pretty_clauses:pp_hook/3).
:- module_transparent(pretty_clauses:pp_hook/3).
pretty_clauses:pp_hook(_,Tab,S):- is_vm(S),!,prefix_spaces(Tab),!,write('..VM..').
pretty_clauses:pp_hook(_,Tab,S):- term_is_ansi(S), !,prefix_spaces(Tab), write_keeping_ansi(S).
pretty_clauses:pp_hook(_,_  ,G):- current_predicate(is_group/1),pp_hook_g(G).

lock_doing(Lock,G,Goal):- 
 (nb_current(Lock,Was);Was=[]), !, 
  \+ ((member(E,Was),E==G)),
  locally(nb_setval(Lock,[G|Was]),Goal).

pp_hook_g(G):- lock_doing(in_pp_hook_g,G,pp_hook_g1(G)).

pp_hook_g1(G):-  is_grid(G),!, fail,ptt(G), !.
pp_hook_g1(G):-  is_object(G),pt(G), !.
pp_hook_g1(G):-  is_group(G),ptt(G), !.
pp_hook_g1(G):-  is_map(G),write('map'),!.
%pp_hook_g(G):- compound(G),ptt(G),!.
%pp_hook_g(G):- ptt(G),!.
pp_hook_g1(G):-  is_grid(G), 
% \+ (sub_term(E,G),compound(E),E='$VAR'(_)), 
  catch((wots(S,print_grid(G)),strip_vspace(S,SS),ptc(orange,(format('"~w"',[SS])))),_,fail).

strip_vspace(S,Stripped):- string_concat(' ',SS,S),!,strip_vspace(SS,Stripped).
strip_vspace(S,Stripped):- string_concat(SS,' ',S),!,strip_vspace(SS,Stripped).
strip_vspace(S,Stripped):- string_concat('\n',SS,S),!,strip_vspace(SS,Stripped).
strip_vspace(S,Stripped):- string_concat(SS,'\n',S),!,strip_vspace(SS,Stripped).
strip_vspace(S,Stripped):- string_concat('\t',SS,S),!,strip_vspace(SS,Stripped).
strip_vspace(S,Stripped):- string_concat(SS,'\t',S),!,strip_vspace(SS,Stripped).
%strip_vspace(S,Stripped):- split_string(S, "", "\t\r\n", [Stripped]).
strip_vspace(S,S).

print_ansi_tree(S,_):- term_is_ansi(S), !, write_keeping_ansi(S).
print_ansi_tree(P,_):- catch(arc_portray(P),_,fail),!.
print_ansi_tree(P,_OL):- catch(print_tree_nl(P),_,fail),!.

wqs(G):- is_map(G), !, write_map(G,'wqs').
wqs(X):- is_grid(X), !, print_grid(X).
wqs(S):- term_is_ansi(S), !, write_keeping_ansi(S).
wqs(X):- is_object(X), !, show_shape(X).
wqs(X):- plain_var(X), !, wqs(plain_var(X)). wqs(nl):- !, nl. wqs(''):-!. wqs([]):-!.
%wqs([H1,H2|T]):- string(H1),string(H2),!, write(H1),write_nbsp, wqs([H2|T]).
%wqs([H1|T]):- string(H1),!, write(H1), wqs(T).
wqs([skip(_)|T]):- !,wqs(T).
%wqs([H|T]):- compound(H),!, writeq(H), wqs(T).
wqs([H|T]):- !, wqs(H),need_nl(H,T), wqs(T).
wqs(format(C,N)):- !, format(C,N).
wqs(writef(C,N)):- !, writef(C,N).
wqs(call(C)):- !, call(C).
wqs(pt(C)):- !, pt(C).
wqs(g(C)):- !, write_nbsp, bold_print(writeq(g(C))).
wqs(io(C)):- !, write_nbsp, bold_print(writeq(io(C))).
wqs(q(C)):- !, write_nbsp, writeq(C).
wqs(uc(C,W)):- !, write_nbsp, color_print(C,call(underline_print(format("\t~@",[wqs(W)])))).
wqs(cc(C,N)):- attvar(C), get_attrs(C,PC), !, wqs(ccc(PC,N)).
wqs(cc(C,N)):- var(C), sformat(PC,"~p",[C]), !, wqs(ccc(PC,N)).
wqs(cc(C,N)):- !, write(' cc('),color_print(C,C),write(','), writeq(N), write(')').
wqs(color_print(C,X)):- is_color(C), !, write_nbsp, color_print(C,X).
wqs(color_print(C,X)):- \+ plain_var(C), !, write_nbsp, color_print(C,X).
wqs(C):- is_color(C),!,wqs(color_print(C,C)).

wqs(S):- term_is_ansi(S), !, write_keeping_ansi(S).
wqs(X):- \+ compound(X),!, write_nbsp, write(X).
wqs(S):- term_contains_ansi(S), !,write_nbsp, write_keeping_ansi(S).
wqs(X):- write_nbsp, writeq(X).

write_nbsp:- arc_webui,!,write('&nbsp;').
write_nbsp:- write(' ').

is_breaker(P):- compound(P),functor(P,_,A), A>=3.
need_nl(_,_):- arc_webui,!.
need_nl(H,[P|_]):- \+ is_breaker(H),is_breaker(P),line_position(user_output,L1),L1>80,nl,write('\t\t').
need_nl(_,_):- line_position(user_output,L1),L1>160,nl,write('\t\t').
need_nl(_,_).
wqln(X):- wqnl(X).
wqnl(X):- is_list(X),!,g_out(wqs(X)).
wqnl(X):- format('~N~q~N',[X]).
dash_chars:- dash_chars(40),!.
dash_chars(H):- integer(H), dash_border(H).
dash_chars(S):- format('~N'),dash_chars(60,S),format('~N').
dash_chars(H,_):- H < 1,!.
dash_chars(H,C):-forall(between(0,H,_),write(C)).

dash_uborder_no_nl(1):-  line_position(current_output,0),!, write('¯¯¯ ').
dash_uborder_no_nl(1):-  line_position(current_output,W),write(W),!, write('¯¯¯ ').
dash_uborder_no_nl(1):- write('¯¯¯ ').
dash_uborder_no_nl(Width):- WidthM1 is Width-1, write(' ¯'),dash_chars(WidthM1,'¯¯'),!.
%dash_uborder_no_nl(Width):- WidthM1 is Width-1, write(' _'),dash_chars(WidthM1,'__').


dash_border_no_nl(1):-  line_position(current_output,0),!, write(' ___ ').
dash_border_no_nl(1):-  line_position(current_output,W),write(W),!, write('___ ').
dash_border_no_nl(1):- write(' ___ ').
dash_border_no_nl(Width):- WidthM1 is Width-1, write(' _'),dash_chars(WidthM1,'__').

dash_border(Width):- !, dash_border_no_nl(Width),nl,!.
dash_uborder(Width):- format('~N'), WidthM1 is Width-1, write(' ¯'),dash_chars(WidthM1,'¯¯'),nl.
%dash_uborder(Width):- format('~N'), WidthM1 is Width-1, write(' _'),dash_chars(WidthM1,'__'),nl.

functor_test_color(pass,green).
functor_test_color(fail,red).
functor_test_color(warn,yellow).

arcdbg(G):- is_map(G), !, write_map(G,'arcdbg').
arcdbg(G):- compound(G), compound_name_arity(G,F,_),functor_test_color(F,C),
  wots(S,print(G)),color_print(C,S),!,format('~N').
arcdbg(G):- wdmsg(G).


%user:portray(Grid):- ((\+ tracing, is_group(Grid),print_grid(Grid))).
%user:portray(Grid):- quietlyd((is_object(Grid),print_grid(Grid))).

banner_lines(Color):- format('~N'),
  color_print(Color,'--------------------------------------------------------------'),nl,
  color_print(Color,'=============================================================='),nl,
  color_print(Color,'--------------------------------------------------------------'),nl,
  color_print(Color,'=============================================================='),nl,
  color_print(Color,'--------------------------------------------------------------'),nl.

print_side_by_side(C1,C2):- print_side_by_side(C1,_LW,C2), !.

gridoid_size(G,30,30):- \+ compound(G),!.
gridoid_size(print_grid(H,V,_),H,V):- nonvar(H),nonvar(V),!.
gridoid_size(print_grid(H,V,_,_),H,V):- nonvar(H),nonvar(V),!.
gridoid_size(print_grid0(H,V,_),H,V):- nonvar(H),nonvar(V),!.
gridoid_size(print_grid0(H,V,_,_),H,V):- nonvar(H),nonvar(V),!.
gridoid_size(G,H,V):- compound_name_arity(G,print_grid,A),arg(A,G,GG),gridoid_size(GG,H,V).
gridoid_size(G,H,V):- is_gridoid(G),!,grid_size(G,H,V).

print_side_by_side(X,Y,Z):- g_out((nl,print_side_by_side0(X,Y,Z))).

print_side_by_side0([],_,[]):-!.
print_side_by_side0(C1-wqs(S1),LW,C2-wqs(S2)):- nonvar(S1),!,
  print_side_by_side0(C1,LW,C2),format('~N'),
  print_side_by_side0(wqs(S1),LW,wqs(S2)).
print_side_by_side0(C1-A,LW,C2-B):- nonvar(A),!,
  print_side_by_side0(C1,LW1,C2),format('~N'),
  print_side_by_side0(A,LW2,B),
  ignore(max_min(LW1,LW2,LW,_)).

print_side_by_side0(C1,LW,C2):- var(LW), gridoid_size(C1,H1,V1),gridoid_size(C2,H2,V2),!,    
    ((V2 > V1) -> LW is -(H2 * 2 + 12) ; LW is (H1 * 2 + 12)),!, print_side_by_side0(C1,LW,C2).
print_side_by_side0(C1,LW,C2):-  var(LW), LW=30,print_side_by_side0(C1,LW,C2),!.

print_side_by_side0(C1,W0,C2):- number(W0), W0 < 0, LW is -W0, !, print_side_by_side0(C2,LW,C1).

print_side_by_side0(C1,W0,C2):- number(W0), LW is floor(abs(W0)),
  locally(nb_setval(print_sbs,left),into_ss_string(C1,ss(W1,L1))),
  locally(nb_setval(print_sbs,right),into_ss_string(C2,ss(_,L2))),!,
  print_side_by_side_lists_1st(L1,W1,L2,LW).

is_side(RL):- nb_current(print_sbs,RL),RL\==[].

print_side_by_side_lists_1st([],_,[],_):-!.
print_side_by_side_lists_1st(L1,W1,L2,LW):-
  length(L1,N1),
  length(L2,N2),
  N2>N1,!,
  print_side_by_side_lists_1st(L2,W1,L1,LW).

print_side_by_side_lists_1st([E1,E2|L1],W1,L2,LW):- !,
  wots(S,(write(E2),write('\t '),dash_chars(W1,' ' ))),
  atom_length(S,Pre),
  print_side_by_side_lists(Pre,[E1,E2|L1],W1,L2,LW).

print_side_by_side_lists_1st([E2|L1],W1,L2,LW):- !,
  wots(S,(write(E2),write('\t '),dash_chars(W1,' ' ))),
  atom_length(S,Pre),
  print_side_by_side_lists(Pre,[E2|L1],W1,L2,LW).

print_side_by_side_lists(Pre,[E1|L1],W1,[E2|L2],W2):-!,
  write_padding(E1,W1,E2,W2), 
  print_side_by_side_lists(Pre,L1,W1,L2,W2).
  
print_side_by_side_lists(_Pre,[],W1,[],W2):- !, nop(write_padding([],W1,[],W2)),!.
  
print_side_by_side_lists(Pre,[E1|L1],W1,[],W2):- !,
  write_padding(E1,W1,[],W2),
  print_side_by_side_lists(Pre,L1,W1,[],W2).
  
print_side_by_side_lists(Pre,[],W1,[E2|L2],W2):-
  with_output_to(atom(S),dash_chars(Pre,' ')), write_padding(S,W1,E2,W2),
  print_side_by_side_lists(Pre,[],W1,L2,W2).

desc(A,B):- wots(S1,A),wots(S2,B),format('~N~n'),dash_chars,write(S1),format('~N'),write(S2),format('~N').

write_padding(E1,_W1,E2,LW):- %write_nbsp,
    W1 = LW,
   format('~N'),as_str(E1,S1), as_str(E2,S2), 
   write(S1), pre_s2(W1,S2), format('~N').

pre_s2(_,S2):- atom_contains(S2,'_'), write('    '),write(S2).
pre_s2(_,S2):- atom_contains(S2,'¯'), write('    '),write(S2).
pre_s2(_,S2):- atom_contains(S2,'|'), write('   '),write(S2).
pre_s2(W1,S2):- line_position(user_output,L1), Pad1 is W1 - L1, (dash_chars(Pad1, ' ')),write('  '),write(S2).

as_str(C,S):- plain_var(C),!,sformat(S,' var(~p)',[C]).
as_str([],""):-!.
as_str(S,A):- atom(S),!,atom_string(S,A).
as_str(call(C),S):- !, wots(S,C).
as_str(S,A):- \+ string(S), sformat(A,'~p',[S]),!.
as_str(S,S).

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
  wots(U1, print_grid(IH,IV,NameIn+fav(PairName),In)),
  wots(U2, print_grid(OH,OV,NameOut+fav(PairName),Out)),
  print_side_by_side(U1,LW,U2),!.

show_pair_grid(_,_,_,_,_, _,_,_,_):- is_print_collapsed,!.
show_pair_grid(TitleColor,IH,IV,OH,OV,NameIn,NameOut,PairName,In,Out):-
  toUpperC(NameIn,NameInU),toUpperC(NameOut,NameOutU),
 % ignore(IH=1),
  %LW is (IH * 2 + 12),
%  wots(U1, print_grid(IH,IV,In)),
%  wots(U2, print_grid(OH,OV,Out)),
  INFO = [grid_dim,amass,length,colors_count_size,colors],
%  print_side_by_side(U1,LW,U2),
  print_side_by_side(TitleColor,print_grid(IH,IV,In),NameInU,LW,print_grid(OH,OV,Out),NameOutU),
  print_side_by_side(
     call(describe_feature(In,[call(wqnl(NameInU+fav(PairName)))|INFO])),LW,
    call(describe_feature(Out,[call(wqnl(NameOutU+fav(PairName)))|INFO]))),!.

%print_side_by_side(TitleColor,G1,N1,G2,N2):- print_side_by_side(G1-wqs(N1),G2-wqs(N2)). 
print_side_by_side(TitleColor,G1,N1,LW,G2,N2):- 
   g_out((nl,
   print_side_by_side0(G1,LW,G2),
   data_type(G1,S1), data_type(G2,S2),
   print_side_by_side4d(TitleColor,S1," ~w   (~w)",N1,LW,S2," ~w  (~w)", N2))).

print_side_by_side4d(TitleColor,S1,F1,N1,W0,S2,F2,N2):- number(W0), W0 < 0, LW is -W0, !, print_side_by_side4d(TitleColor,S2,F2,N2,LW,S1,F1,N1).
print_side_by_side4d(TitleColor,S1,F1,N1,_LW,S2,F2,N2):- 
   format('~N',[]), write('\t'),format_u(TitleColor,F1,[N1,S1]),write('\t\t'),format_u(TitleColor,F2,[N2,S2]),write('\n'),!.

toUpperC(A,AU):- A==[],!,AU='  []  '.
toUpperC(A,AU):- string(A),!,AU=A.
toUpperC(A,A):-!.
toUpperC(A,AU):- atom(A),toPropercase(A,AU),!.
toUpperC(A,AU):- atomic(A),upcase_atom(A,AU),!.
toUpperC(A,AU):- is_list(A),maplist(toUpperC,A,AU),!.
toUpperC(I,O):- compound(I), !, compound_name_arguments(I,F,IA), maplist(toUpperC,IA,OA), compound_name_arguments(O,F,OA),!.
toUpperC(A,AU):- term_to_atom(A,AU).

show_pair_diff(IH,IV,OH,OV,NameIn,NameOut,PairName,In,Out):-
  toUpperC(NameIn,NameInU),toUpperC(NameOut,NameOutU),
  show_pair_grid(cyan,IH,IV,OH,OV,NameIn,NameOut,PairName,In,Out),
  ((is_group(In),is_group(Out))-> once(showdiff(In,Out));
    ignore((is_group(In),desc(wqnl(NameInU+fav(PairName)),locally(nb_setval(debug_as_grid,t), debug_indiv(In))))),
    ignore((is_group(Out),desc(wqnl(NameOutU+fav(PairName)), locally(nb_setval(debug_as_grid,t),debug_indiv(Out)))))),!.


uses_space(C):- code_type(C,print).

into_ss_string(C, ss(1,["var_into_ss_string"])):- var(C),!.
into_ss_string(C,_):- plain_var(C),!,throw(var_into_ss_string(C)).
%into_ss_string(A-B,ss(LenAB,ABL)):- into_ss_string(A,ss(LenA,LA)), into_ss_string(B,ss(LenB,LB)), append(LA,LB,ABL), max_min(LenA,LenB,LenAB,_).
into_ss_string(print_grid(G),SS):- into_ss_string(print_grid_ss(G),SS).
into_ss_string(print_grid(X,Y,G),SS):- into_ss_string(print_grid_ss(X,Y,G),SS).
into_ss_string(print_grid0(G),SS):- wots(S,print_grid_ss(G)),!,into_ss_string(S,SS).
into_ss_string(print_grid0(X,Y,G),SS):- wots(S,print_grid_ss(X,Y,G)),!,into_ss_string(S,SS).
into_ss_string(wqs(W),SS):- !,wots(SS,format_u(yellow,"\t~@",[wqs(W)])).
into_ss_string(ss(Len,L),ss(Len,L)):-!.
into_ss_string(uc(W),SS):- !,wots(SS,format_u(yellow,"\t~@",[wqs(W)])).
into_ss_string(uc(C,W),SS):- !,wots(SS,color_print(C,call(underline_print(format("\t~@",[wqs(W)]))))).
into_ss_string(call(C),SS):- wots(S,catch(C,E,true)), 
  (((nonvar_or_ci(E),notrace,break,rrtrace(C))->throw(E);true)), into_ss_string(S,SS).
into_ss_string(A+B,SS):-!,into_ss_string(A,ss(LenA,LA)), into_ss_string(B,ss(LenB,LB)), append(LA,LB,ABL), max_min(LenA,LenB,LenAB,_),
  ss(LenAB,ABL)=SS.
into_ss_string(A-B,SS):-!,into_ss_string(A,ss(LenA,LA)), into_ss_string(B,ss(LenB,LB)), append(LA,LB,ABL), max_min(LenA,LenB,LenAB,_),
  ss(LenAB,ABL)=SS.

into_ss_string(G,SS):- is_gridoid(G),!,wots(S,print_grid_ss(G)),!,into_ss_string(S,SS).
into_ss_string(G,SS):- is_grid(G),!,wots(S,print_grid_ss(G)),!,into_ss_string(S,SS).
into_ss_string(G,SS):- is_object(G),!,wots(S,print_grid_ss(G)),!,into_ss_string(S,SS).
into_ss_string(G,SS):- is_points_list(G),!,wots(S,print_grid_ss(G)),!,into_ss_string(S,SS).
into_ss_string(G,SS):- is_group(G),!,wots(S,print_grid_ss(G)),!,into_ss_string(S,SS).
into_ss_string(L,ss(Len,L)):- is_list(L), find_longest_len(L,Len),!.
into_ss_string(S,SS):- string(S), !, atomics_to_string(L,'\n',S),!,into_ss_string(L,SS).
into_ss_string(C,SS):-       wots(S,catch(C,E,true)), 
  (((nonvar_or_ci(E),notrace,break,rrtrace(C))->throw(E);true)), into_ss_string(S,SS).

find_longest_len(SL,L):- find_longest_len(SL,10,L),!.
find_longest_len([],L,L).
find_longest_len([S|SS],N,L):- print_length(S,N2),max_min(N,N2,NM,_),
  find_longest_len(SS,NM,L).

print_grid_ss(G):- print_grid_ss(_,_,G).

print_w_pad(Pad,S):- atomics_to_string(L,'\n',S)-> maplist(print_w_pad0(Pad),L).
print_w_pad0(Pad,S):- format('~N'),dash_chars(Pad,' '), write(S).

print_equals(_,N,V):- \+ compound(V),wqnl(N=V).
print_equals(Grid,N,Ps):- is_object(Ps),grid_size(Grid,H,V),print_grid(H,V,N,Ps),!.
%print_equals(Grid,N,PL):- is_group(PL), grid_size(Grid,H,V), locally(grid_nums(PL),print_list_of_points(N,H,V,[[]])).
print_equals(_,N,G):- print_equals(N,G).


print_equals(N,V):- \+ compound(V),wqnl(N=V).
print_equals(N,V):- is_grid(V),!,wqnl(N),print_grid(V).
print_equals(N,[G|L]):-
  is_grid(G),is_list(L),maplist(is_grid,L),!,
  length([G|L],Len), 
  grid_size(G,H,_),
  wqnl(N=len(Len)),  
  dash_chars(H,"-"),
  forall(member(E,[G|L]),(print_grid(E),dash_chars(H,"-"),nl)).
print_equals(N,V):- better_value(V,BV)-> BV\=@=V, !,print_equals(N,BV).
print_equals(N,[S|L]):- string(S),writeq(N),write('= '),write(S),maplist(commawrite,L),nl.
print_equals(Name,json(JSON)):-!, print_equals(Name,JSON).
print_equals(Name,trn=Y):- !, print_equals(Name,Y).
print_equals(Name,X->Y):- !, print_equals(in(Name),X), print_equals(out(Name),Y).
print_equals(colors,XY):-print_equals(cc,XY).
%print_equals(Name,cc(C,N)):-print_equals(Name,cc(C,N)).
print_equals(Name,X=Y):- !, print_equals(Name=X,Y).
%print_equals(Name,[H|L]):- !, maplist(print_equals(Name),[H|L]).
print_equals(Name,Val):- is_list(Val),forall(nth0(N,Val,E),print_equals(Name:N,E)).
print_equals(Name,Val):- pt(Name=Val).

commawrite(S):- write(','),write(S).



as_color(cc(Count,Num),List):- color_name(Num,Name),wots(List,color_print(Num,Name=Count)).
better_value(V,List):- is_list(V), maplist(as_color,V,List).
better_value([G|V],List):- 
  is_group([G|V]),
  maplist(points_to_grid,[G|V],List),
  [G|V] \=@= List.


fix_grid_pg(SIndvOut,_InOutL,_G,_PG):- is_grid(SIndvOut),!,fail.
fix_grid_pg(SIndvOut,PGP,G,GP):- compound(SIndvOut), SIndvOut=(GG,PGG),!,listify(PGG,PGL),listify(GG,G),!,my_append(PGL,PGP,GP).
fix_grid_pg(SIndvOut,PGP,SIndvOut,[PGP]):- \+ is_list(PGP),!.
fix_grid_pg(SIndvOut,PGP,[SIndvOut],PGP):- \+ is_list(SIndvOut),!.


with_glyph_index(Print,Goal):- Print==[],!,with_glyph_index([e],Goal).
with_glyph_index(Print,Goal):- listify(Print,PrintL),
  locally(b_setval(glyph_index,PrintL),Goal).

with_color_index(Print,Goal):- Print==[],!,with_color_index([e],Goal).
with_color_index(Print,Goal):- listify(Print,PrintL),
  locally(nb_setval(color_index,PrintL),Goal).
use_row_db :- fail.

is_print_collapsed:- \+ nb_current(arc_portray,t), luser_getval(print_collapsed,N),N\==[].

print_grid(_):- is_print_collapsed,!.
print_grid(Grid):- use_row_db, is_grid(Grid),!, grid_to_tid(Grid,TID),print_grid(TID).

print_grid(Grid):-  quietly(print_grid0(Grid)),!.

print_grid(Str,Grid):-  ignore((print_grid(_,_,Str,Grid))),!.
print_grid0(Grid):-  ignore(print_grid0(_,_,Grid)),!.

%print_grid0(Grid):- plain_var(Grid),!, throw(var_print_grid(Grid)).

format_u(TitleColor,Format,Args):- quietlyd( ignore((underline_print(color_print(TitleColor,call(format(Format,Args))))))).

print_grid(_,_,_,_):- is_print_collapsed,!.
print_grid(OH,OV,Name,Out):- 
 quietly((
  ignore((print_grid0(OH,OV,Out))),!,format('~N  '),
  ignore((data_type(Out,SS), toUpperC(Name,NameU),
  mesg_color(SS,TitleColor),
  format_u(TitleColor,"~w  (~w)",[NameU, SS]))))),!.
%print_grid(H,V,Grid):- use_row_db, grid_to_tid(Grid,TID),!,print_grid0(H,V,TID).

print_grid(_,_,_):- is_print_collapsed,!.
print_grid(H,V,Grid):- ignore(quietly(print_grid0(H,V,Grid))).

print_grid0(_,_,_):- is_print_collapsed,!.
print_grid0(H,V,G):- G==[],number(H),number(V),!,make_grid(H,V,GG),!,print_grid0(H,V,GG).
% print_grid0(_H,_V,G):- G==[],!,make_grid(H,V,GG),!,print_grid0(H,V,GG).

print_grid0(H,V,D):- is_map(D),ignore(H = D.h),ignore(V = D.v),
  vm_to_printable(D,R),D\==R,!,print_grid0(H,V,R).

print_grid0(H,V,Grid):- \+ callable(Grid),!,write('not grid: '),
  GG= nc_print_grid(H,V,Grid), pt(GG),!,nop(trace_or_throw(GG)).


print_grid0(H,V,SIndvOut):- compound(SIndvOut),SIndvOut=(G-GP), \+ is_nc_point(GP),!, 
  with_glyph_index(G,with_color_index(GP,print_grid0(H,V,G))),!.
print_grid0(H,V,Grid):- is_points_list(Grid), points_to_grid(H,V,Grid,PGrid),!,print_grid0(H,V,PGrid).
print_grid0(H,V,G):- is_empty_grid(G), %trace, arcST,
 wdmsg(is_empty_grid(H,V)),!,
 make_grid(H,V,Empty),
 print_grid0(H,V,Empty),!. 

print_grid0(H,V,Grid):- \+ is_gridoid(Grid), into_grid(Grid,G),!,print_grid0(H,V,G).
print_grid0(H,V,Grid):- print_grid0(1,1,H,V,Grid),!.

%print_grid(SH,SV,EH,EV,Grid):- nop(print_grid(SH,SV,EH,EV,Grid)),!.
print_grid(SH,SV,EH,EV,Grid):- quietlyd(print_grid0(SH,SV,EH,EV,Grid)),!.


print_grid0(_SH,_SV,_EH,_EV,Grid):- \+ is_printable_gridoid(Grid), !, writeln(\+ is_printable_gridoid(Grid)).

print_grid0(SH,SV,EH,EV,Grid):-  
  \+ \+ print_grid1(SH,SV,EH,EV,Grid),!,format('~N').


print_grid1(SH,SV,EH,EV,Grid):- is_object(Grid),
  object_grid(Grid,Points),!,print_grid1(SH,SV,EH,EV,Points).

print_grid1(SH,SV,EH,EV,Grid):-
 nl_if_not_side_by_side,
 %backtrace(10),
 (line_position(current_output,O);O=0),!,
 O1 is O+1,
 print_grid_pad(O1,SH,SV,EH,EV,Grid), 
 nl_if_not_side_by_side,format('~N').

nl_if_not_side_by_side:- ignore(( \+ in_side_by_side, format('~N'))).

in_side_by_side:- current_output(Out), \+ stream_property(Out,alias(user_output)).



print_grid_pad(O1,SH,SV,EH,EV,Grid):-
  into_color_name_always(Grid,GridI),
  wots(S,print_grid2(SH,SV,EH,EV,GridI)),
  print_w_pad(O1,S),!.

print_grid2(SH,SV,EH,EV,GridI):- arc_webui,!, print_grid_html(SH,SV,EH,EV,GridI),nl.
print_grid2(SH,SV,EH,EV,GridI):- ignore(print_grid_ansi(SH,SV,EH,EV,GridI)).
print_grid_ss(H,V,G):- must_det_ll(print_grid0(H,V,G)).

w_out(S):- toplevel_pp(bfly),!,correct_nbsp(S,SO),our_pengine_output(SO),!.
w_out(S):- is_webui,!,correct_nbsp(S,SO),our_pengine_output(SO),!.
w_out(S):- format('~N'),write(S).
%w_out(SO):- pengines:pengine_output('</pre>'),pengines:pengine_output(SO),pengines:pengine_output('<pre class="console">'),!.

arc_webui:- toplevel_pp(swish),!.
%arc_webui:- toplevel_pp(bfly),!.
arc_webui:- is_webui,!.

g_out(G):- is_side(_),!,call(G).
g_out(G):- \+ arc_webui,!,format('~N'),call(G),format('~N').
g_out(G):- nb_current(in_g_out,t),!,format('~N'),call(G),format('~N').
g_out(G):- locally(nb_setval(in_g_out,t), gg_out(G)).

gg_out(G):- \+ toplevel_pp(bfly),!,gg_out2(G).
gg_out(G):- bfly_html_goal(gg_out2(G)).

gg_out2(G):-
  wots(S0,call(G)),correct_nbsp(S0,S), !,
  sformat(SO,'<pre style="overflow-x: visible;"><font size="+0">~w</font></pre>',[S]),
  w_out(SO).

g_out(C,G):- wots(S0,g_out(G)),correct_nbsp(S0,S),
 mbfy(color_print_webui(C,S)).

mbfy(G):- \+ in_pp(bfly),!,call(G).
mbfy(G):- bfly_html_goal(G).

correct_nbsp(S0,S):- replace_in_string([" &nbsp;"="&nbsp;","&nbsp; "="&nbsp;"],S0,S).

ansi_format_real(Ansi,Format,Args):- \+ arc_webui,!,ansi_format(Ansi,Format,Args).
ansi_format_real(Ansi,Format,Args):- sformat(S,Format,Args),!,color_print_webui(Ansi,S).

color_print_webui(_,G):- G==' ',!,write_nbsp.
color_print_webui(C,G):- mv_peek_color(C,W),color_print_webui(W,G).
color_print_webui(C,G):- is_bg_sym_or_var(C),!,color_print_webui(wbg,G).
color_print_webui([],G):- !, write(G).
color_print_webui([C|CC],G):- !,wots(S,color_print_webui(C,G)),color_print_webui(CC,S).
color_print_webui(_C,G):- \+ arc_webui,!,write(G).

color_print_webui(underline,G):- !, color_print_webui(style('text-decoration','underline'),G).
color_print_webui(bold,G):- !, color_print_webui(style('font-weight','bold'),G).
color_print_webui(italic,G):- !, color_print_webui(style('font-style','italic'),G).

%color_print_webui(C,G):- \+ arc_webui,!,color_print(C,G).
color_print_webui(bg(C),G):- !, color_print_webui(style('background-color',C),G).
color_print_webui(hbg(C),G):- !, color_print_webui([bg(C),style('filter','brightness(150%)')],G).
color_print_webui(hfg(C),G):- !, color_print_webui([C,style('brightness','200%')],G).
color_print_webui(fg(C),G):- !, color_print_webui(C,G).
color_print_webui(style(C),G):- !, format('<font style="~w">~w</font>',[C,G]).
color_print_webui(style(N,V),G):- !, format('<font style="~w: ~w;">~w</font>',[N,V,G]).
color_print_webui(color(C),G):- !, format('<font color="~w">~w</font>',[C,G]).
color_print_webui(black,G):- color_print_webui(style('opacity: 0.5;'),G).
color_print_webui(C,G):- C==wbg,!,color_print_webui([black,black],G).
color_print_webui(C,G):- format('<font color="~w" style="font-weight: bold;">~w</font>',[C,G]),!.

print_grid_html(SH,SV,EH,EV,Grid):-
 g_out(must_det_ll((
  format('~N'), 
  ((plain_var(EH) ; plain_var(EV))->grid_size(Grid,EH,EV);true),
  Width is EH-SH, 
  (Width==0 -> DBW = 1 ; DBW is Width+1),
  once((dash_border_no_nl(DBW))),
  bg_sym(BGC),
  forall(between(SV,EV,V),
   ((format('~N|<font style="background-color: reset; line-height: .5; font-stretch: ultra-extended;">'),
     forall(between(SH,EH,H),
     ignore((((hv_cg_value(Grid,CG,H,V);/*grid_cpoint(Grid,CG-_,H,V);*/CG=BGC)->
        (once(print_gw1(CG))))))),write('&nbsp|</font>')))),
  %print_g(H,V,C,LoH,LoV,HiH,HiV)
  format('~N'),!,
  once((dash_uborder_no_nl(DBW)))))).

grid_colors(GridI,WGrid):-
   must_det_ll((
  %maybe_grid_numbervars(GridI,Grid),
  GridI=Grid,
  grid_size(Grid,EH,EV),
  make_grid(EH,EV,WGrid),
  bg_sym(BGC),
  forall(between(1,EV,V),
     forall(between(1,EH,H),
      ignore((((hv_cg_value(Grid,CG,H,V);CG=f(BGC))->
         (once(nb_set_chv(CG,H,V,WGrid)))))))))).
 

print_grid_ansi(SH,SV,EH,EV,GridI):-
 must_det_ll((
  format('~N'), 
  %maybe_grid_numbervars(GridI,Grid),
  GridI=Grid,
  ((plain_var(EH) ; plain_var(EV))->grid_size(Grid,EH,EV);true),
  Width is EH-SH, 
  (Width==0 -> DBW = 1 ; DBW is Width+1),
  once((dash_border_no_nl(DBW))),
  bg_sym(BGC),
  forall(between(SV,EV,V),
   ((format('~N|'),
     forall(between(SH,EH,H),
      ignore((
       (((hv_cg_value(Grid,CG,H,V);hv_c_value(Grid,CG,H,V);/*grid_cpoint(Grid,CG-_,H,V);*/CG=BGC))->
        (once(print_gw1(CG);write('??')));write('?f'))))),
     write(' |')))),
  format('~N'),!,
  once((dash_uborder_no_nl(DBW))))), 
  nop((    
     (( \+ ground(GridI));sub_var(wbg,GridI);sub_var(bg,GridI);sub_var(wfg,GridI);sub_var(fg,GridI)),
      grid_colors(GridI,CGrid),
      (nb_current(print_sbs,left)-> (nl,nl, write(left), write(=)) ; true),
     print_attvars(CGrid))).

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
%print_grid(Grid):- is_grid(Grid),!, maplist(print_rows,Grid),nl.
%print_rows(List):- maplist(print_g,List),nl.
%block_colors([(black),(blue),(red),(green),(yellow),'#c0c0c0',(magenta),'#ff8c00',(cyan),'#8b4513']).
%block_colors([(black),(blue),(red),(green),(yellow),Silver,('#966cb8'),'#ff8c00',(cyan),'#8b4513']):- silver(Silver),!.
block_colors([('#3a5a3a'),(blue),(red),(green),(yellow),Silver,(magenta),'#ff8c00',(cyan),'#8b4513','#2a2a2a','magenta','#444455']):- silver(Silver),!.
%block_colors([(black),(blue),(red),(green),(yellow),Silver,(magenta),'#ff8c00',(cyan),'#8b4513','#2a2a2a','#3a5a3a']):- silver(Silver),!.
named_colors([(black),(blue),(red),(green),(yellow),(silver),(purple),(orange),(cyan),(brown),wbg,fg,'#444455']).
named_colors([ (lack),(blue),(red),(green),(yellow),(silver),(purple),(orange),(cyan),(brown),bg,wfg]).
named_colors([(lack),(blue),(red),(green),(yellow),(silver),(magenta),(orange),(cyan),(brown)]).
named_colors([(lack),(blue),(red),(green),(yellow),(grey),(pink),(orange),(cyan),(maroon)]).

% silver(rgb(123,123,123)).
silver('#7b7b7b').
silver('#c0c0c0').
silver('#9a9a9a').

unnegate_color(C,Neg):- number(C),C<0,C is -Neg,!.
unnegate_color(CI,C):- compound(CI),CI= '-'(C),!.
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
arc_acolor(C,fg(Color)):- atom(C),atom_concat('#',_,C),!,Color=C.
arc_acolor(L,LL):- is_list(L),!,maplist(arc_acolor,L,LL).
arc_acolor(C,Color):- nonvar(C), color_int(C,I)->C\==I,!,arc_acolor(I,Color).
arc_acolor(_,[bold,underline]).

ansi_color_bg(C,bg(C1)):- arc_acolor(C,C2),C2=fg(C1),nonvar(C1),!.
ansi_color_bg(C,C1):- arc_acolor(C,C1).

:- define_into_module(on_bg/2).
on_bg(C,G):- ansi_color_bg(C,C1),ansi_format_real(C1,'~@',[call(G)]).
:- define_into_module(on_bg/1).
:- export(on_bg/1).
:- system:import(on_bg/1).
on_bg(G):- arc_webui,!,call(G).
on_bg(G):- ansi_format_real(reset,'~@',[call(G)]).

:- define_into_module(ansi_format_arc/3).
ansi_format_arc(Ansi,Format,Args):- on_bg(ansi_format_real(Ansi,Format,Args)),!.

underline_print(W):- ansi_format_real([bold,underline],'~@',[W]),!.


:- module_transparent(ansi_format_real/2).
:- module_transparent(bold_print/1).
bold_print(W):- ansi_format_real(bold,'~@',[W]),!.

compound_var(C,N):- \+ plain_var(C), \+ attvar(C), is_ftVar(C),arg(1,C,N).



%color_print(_,W):- write(W),!.
:- export(color_print/2).
:- system:import(color_print/2).

color_print(C,W):- mv_peek_color(C,V),!,color_print(V,W).
color_print(C,W):- '$current_typein_module'(M), muarc_mod(MM), MM\==M, !,'$set_typein_module'(MM), module(MM),color_print(C,W).
%color_print(C,W):- C == black,!, color_print(white,W).
color_print(C,W):- compound(W),compound_name_arity(W,call,_),!,(wots(S1,call(call,W))->color_print(C,S1);color_print(C,failed(W))).
color_print(C,W):- arc_webui,!,color_print_webui(C,W).

color_print(C,W):- is_bg_sym_or_var(C),W=='_',!,on_bg(write_nbsp),!.
color_print(C,W):- is_bg_sym_or_var(C),W=='_',color_print(C,'+').
color_print(_,' '):- write_nbsp,!.

color_print(C,W):- plain_var(C),integer(W),arc_acolor(W,CI),!,ansi_format_arc(CI,'~w',[W]),!.
color_print(C,W):- plain_var(C),W=='?',!,on_bg(magenta,color_print_webui(C,W)).
color_print(C,W):- plain_var(C),!,on_bg(ansi_format_real(italic,'~w',[W])),!.

color_print(C,W):- (cant_be_color(C,CbC);cant_be_color(W,CbC)),!,arc_acolor(-CbC,Ansi),ansi_format_arc(Ansi,'~w',['_']),!.
color_print(C,W):- is_bg_sym_or_var(C),is_bg_sym_or_var(W), !, write_nbsp.

color_print(C,W):- string(W),is_list(C),!,ansi_format_arc(C,'~w',W).
color_print(C,W):- is_bg_sym_or_var(C),integer(W),W<10, arc_acolor(W,CI),!,ansi_format_arc(CI,'~w',[W]),!.
%color_print(C,W):- (is_ftVar(C);(bg_sym(C))), !, on_bg(yellow,var_color_print(C,W)).
color_print(C,W):- is_bg_sym_or_var(C),!,on_bg(cyan,ansi_format_arc([],'~w',[W])),!.
color_print(_-C,W):- is_color(C),!, color_print(C,W).
color_print(C-_,W):- is_color(C),!, color_print(C,W).
%color_print(C-_,W):- !, color_print(C,W).
%color_print(C,W):- atom(C),color_int(C,N),integer(N),!,color_print(N,W).
color_print(C,W):- arc_acolor(C,Color),on_bg(ansi_format_arc(Color,'~w',[W])),!.
color_print(C,W):- C==0,!,ansi_format_arc([fg('#444444')],'~w',[W]),!.

color_name(C,W):- plain_var(C),!,W=C.
color_name(C,W):- color_name0(C,W),!.
color_name(C-_,W):- color_name0(C,W),!.
color_name(_-C,W):- !,color_name(C,W),!.

color_name0(C,W):- atom(C),atom_length(C,L),L>1,!,W=C.
color_name0(C,W):- integer(C),C>=0,named_colors(L),nth0(C,L,W),!.
color_name0(C,W):- mv_color_name(C,V),C\==V,color_name0(V,W).
color_name0(C,W):- is_color(C),!,W=C.

color_int(C,C):- var(C),!.
color_int(C-_,W):-!,color_int(C,W).
color_int(C,W):- integer(C),!,W=C.
color_int(C,W):- atom(C),named_colors(L),nth0(W,L,C),!.
color_int(C,C).


:- export(grid_color_code/2).
grid_color_code(C,C):- var(C).
grid_color_code(C-W,CC-W):- color_code(C,CC).
grid_color_code(C,CC):- color_code(C,CC).
%grid_color_code(C,CC):- is_grid_color(black-_),!,color_code(C,CC).
%grid_color_code(C,CC):- color_int(C,CC).

:- export(color_code/2).
color_code(C,W):- color_name(C,W).


%print_g(H,V,_,LH,LV,HH,HV):- (H<LH;H>HH;V<LV;V>HV),!, write('  ').

resrv_dot(Code):-  code_type(Code,white);code_type(Code,punct);code_type(Code,quote);var_dot(Code);bg_dot(Code);fg_dot(Code);
 member(Code,`?.¨«¬­°```).


var_dot(63).
/* code=63 ?  code=183 · code=176 ° code=186 º 170 ª */
bg_dot(32).
/* 169	© 248	ø 216	Ø  215 ×  174	® */
%fg_dot(C):- luser_getval(fg_dot,C),integer(C),!.
%fg_dot(_):- luser_getval(no_rdot,true),luser_setval(no_rdot,false)-> break , fail.
fg_dot(C):- luser_getval(alt_grid_dot,C),C\==[],!.
fg_dot(174).
cant_be_dot(183).
grid_dot(C):- luser_getval(alt_grid_dot,C),C\==[],!.
grid_dot(169).

%print_g(H,V,C0,_,_,_,_):- cant_be_color(C0),cant_be_color(C0,C),!,  ansi_format_arc([bold,fg('#ff8c00')],'~@',[(write('c'),user:print_g1(H,V,C))]).
%print_g(H,V,C0,_,_,_,_):- plain_var(C0),print_g1(H,V,C-'?'),!.

print_g(H,V,C,_,_,_,_):- write_nbsp, print_g1(H,V,C),!.

object_glyph(G,Glyph):- is_object(G),!,obj_iv(G,Iv), int2glyph(Iv,Glyph).
object_glyph(G,Glyph):- is_grid(G),!,grid_dot(Dot),name(Glyph,[Dot]).
object_glyph(G,Glyph):- nobject_glyph(G,Glyph).

nobject_glyph(G,Glyph):- integer(G), between(0,9,G),atom_number(Glyph,G),!.
nobject_glyph(G,Glyph):- plain_var(G),!,plain_var_glyph(G,Glyph).
nobject_glyph(G,Glyph):- compound_var(G,N),!,nobject_glyph(N,Glyph).
nobject_glyph(A,Glyph):- atom(A),atom_chars(A,Chars),last(Chars,Glyph),!.
nobject_glyph(G,Glyph):- term_to_atom(G,A),nobject_glyph(A,Glyph).


object_cglyph(G,CGlyph):- color(G,C),object_glyph(G,Glyph),wots(CGlyph,color_print(C,Glyph)).

int2glyph(GN2,Glyph):- quietly(int2glyph0(GN2,Glyph)),!.
int2glyph(GN,Glyph):- GN > 255, GN2 is GN div 2, int2glyph(GN2,Glyph).

int2glyph(GN2,Glyph):- trace,i_sym(GN2,GN),!,i_glyph(GN,Glyph),!.

int2glyph0(GN2,Glyph):- i_sym(GN2,GN),i_glyph(GN,Glyph),atom(Glyph),!.
int2glyph0(GN,Glyph):- GN > 255, GN2 is GN div 2, int2glyph0(GN2,Glyph).


%user:portray(S):- (string(S);atom(S)),atom_codes(S,[27|_]),write('"'),write(S),write('"').

%print_gw1(C):- plain_var(C), write('  '),!.

print_gw1(N):- 

 wots(S,(((get_bgc(BG),is_color(BG), once((BG\==black-> color_print(BG,'.');write_nbsp);write(',')));write_nbsp),!,
  (print_g1(N);write('?')))),!,
 gws(S).
gws(S):- write(S),!.
%gws(S):- atom_length(S,L),(L=28->(write(L),atom_codes(S,Codes),arc_assert(ac(S)));write(S)).
%print_gw1(N):- compound(N),N = C-W,!,color_print(C,W),!.

mregression_test:- G = [[1,2,3],[1,2,3],[1,2,3]], print_grid(G).
mregression_test:- G = [[1,2,3],[1,_,3],[1,2,3]], print_grid(G).
mregression_test:- G = [[A,2,3],[_,2,_],[A,2,3]], print_grid(G).
mregression_test:- G = [['$VAR'(1),2,3],[_,2,'$VAR'(3)],[_,2,'$VAR'('Good')],['$VAR'(1),2,3]], print_grid(G).

mregression_test:- print_grid([[_,_-green]]).

mregression_test:- print_grid([[_17910,_17922-green,_17934-green,_17946-green,_17952,_17964-green,_17970,_17982-cyan,_17994-cyan,_18000,_18012-cyan,_18024-cyan,_18036-cyan,_18048-cyan,_18054,_18066-cyan,_18078-cyan,_18084,_18096-green,_18102,_18114-green,_18126-green,_18138-green,_18144],[_17712-green,_17718,7-green,_17736,5-green,_17754,3-cyan,_17772,5-cyan,_17790,_17796,_17802,_17808,_17814,_17820,5-cyan,_17838,3-cyan,_17856,5-green,_17874,7-green,_17892,_17904-green],[_17436-green,7-green,7-green,7-green,8-green,5-green,3-cyan,5-cyan,4-cyan,_17538,3-cyan,6-cyan,6-cyan,3-cyan,_17592,4-cyan,5-cyan,3-cyan,5-green,8-green,7-green,7-green,7-green,_17700-green],[_17220-green,_17226,7-green,_17244,6-green,4-green,_17274,_17280,_17286,2-cyan,_17304,8-cyan,8-cyan,_17334,2-cyan,_17352,_17358,_17364,4-green,6-green,_17394,7-green,_17412,_17424-green],[_16998,5-green,8-green,6-green,_17040,_17046,1-cyan,_17064,3-cyan,_17082,_17088,5-cyan,5-cyan,_17118,_17124,3-cyan,_17142,1-cyan,_17160,_17166,6-green,8-green,5-green,_17208],[_16764-green,_16770,5-green,4-green,_16800,
 1-green,2-cyan,_16830,3-cyan,4-cyan,2-cyan,_16872,_16878,2-cyan,4-cyan,3-cyan,_16920,2-cyan,1-green,_16950,4-green,5-green,_16980,_16992-green],[_16494,3-cyan,3-cyan,_16524,1-cyan,2-cyan,4-purple,6-purple,5-purple,6-purple,7-purple,7-purple,7-purple,7-purple,6-purple,5-purple,6-purple,4-purple,2-cyan,1-cyan,_16722,3-cyan,3-cyan,_16752],[_16272-cyan,_16278,5-cyan,_16296,_16302,_16308,6-purple,6-purple,_16338,7-purple,9-purple,10-purple,10-purple,9-purple,7-purple,_16416,6-purple,6-purple,_16446,_16452,_16458,5-cyan,_16476,_16488-cyan],[_16032-cyan,5-cyan,4-cyan,_16062,3-cyan,3-cyan,5-purple,_16104,_16110,6-purple,_16128,9-purple,9-purple,_16158,6-purple,_16176,_16182,5-purple,3-cyan,3-cyan,_16224,4-cyan,5-cyan,_16260-cyan],[_15786,_15792,_15798,2-cyan,_15816,4-cyan,6-purple,7-purple,6-purple,8-purple,9-purple,10-purple,10-purple,9-purple,8-purple,6-purple,7-purple,6-purple,4-cyan,_15990,2-cyan,_16008,_16014,_16020],[_15552-cyan,_15558,3-cyan,_15576,_15582,2-cyan,4-purple,5-purple,_15624,5-purple,7-purple,7-purple,8-purple,10-purple,9-purple,_15702,9-purple,7-purple,2-cyan,_15744,_15750,3-cyan,_15768,_15780-cyan],[_15300-cyan,_15306,6-cyan,8-cyan,5-cyan,_15348,4-blue,7-blue,7-blue,7-blue,4-blue,_15414,_15420,8-purple,10-purple,9-purple,10-purple,7-purple,_15486,5-cyan,8-cyan,6-cyan,_15528,_15540-cyan],[_15048-cyan,_15054,6-cyan,8-cyan,5-cyan,_15096,7-blue,11-blue,11-blue,11-blue,7-blue,_15162,_15168,8-purple,10-purple,9-purple,10-purple,7-purple,_15234,5-cyan,8-cyan,6-cyan,_15276,_15288-cyan],[_14802-cyan,_14808,3-cyan,_14826,_14832,2-cyan,7-blue,11-blue,11-blue,11-blue,7-blue,4-purple,8-purple,10-purple,9-purple,_14958,9-purple,7-purple,2-cyan,_15000,_15006,3-cyan,_15024,_15036-cyan],[_14556,_14562,_14568,2-cyan,_14586,4-cyan,7-blue,11-blue,11-blue,11-blue,7-blue,7-purple,10-purple,9-purple,8-purple,6-purple,7-purple,6-purple,4-cyan,_14760,2-cyan,_14778,_14784,_14790],[_14304-cyan,5-cyan,4-cyan,_14334,3-cyan,3-cyan,4-blue,7-blue,7-blue,7-blue,4-blue,8-purple,9-purple,_14448,6-purple,_14466,_14472,5-purple,3-cyan,3-cyan,_14514,4-cyan,5-cyan,_14550-cyan],[_14076-cyan,_14082,5-cyan,_14100,_14106,_14112,4-purple,5-purple,_14142,5-purple,8-purple,10-purple,10-purple,9-purple,7-purple,_14220,6-purple,6-purple,_14250,_14256,_14262,5-cyan,_14280,_14292-cyan],[_13806,3-cyan,3-cyan,_13836,1-cyan,2-cyan,4-purple,6-purple,5-purple,6-purple,7-purple,7-purple,7-purple,7-purple,6-purple,5-purple,6-purple,4-purple,2-cyan,1-cyan,_14034,3-cyan,3-cyan,_14064],[_13572-green,_13578,5-green,4-green,_13608,
 1-green,2-cyan,_13638,3-cyan,4-cyan,2-cyan,_13680,_13686,2-cyan,4-cyan,3-cyan,_13728,2-cyan,
 1-green,_13758,4-green,5-green,_13788,_13800-green],[_13350,5-green,8-green,6-green,_13392,_13398,-1-cyan,_13416,3-cyan,_13434,_13440,5-cyan,5-cyan,_13470,_13476,3-cyan,_13494,1-cyan,_13512,_13518,
 6-green,8-green,5-green,_13560],[_13140-green,_13146,7-green,_13164,6-green,4-green,_13194,_13200,_13206,2-cyan,_13224,8-cyan,8-cyan,_13254,2-cyan,_13272,_13278,_13284,4-green,6-green,_13314,7-green,_13332,_13344-green],[_12864-green,7-green,7-green,7-green,8-green,5-green,3-cyan,5-cyan,4-cyan,_12966,3-cyan,6-cyan,6-cyan,3-cyan,_13020,4-cyan,5-cyan,3-cyan,5-green,8-green,7-green,7-green,7-green,_13128-green],[_12660-green,_12666,7-green,_12684,5-green,_12702,3-cyan,_12720,5-cyan,_12738,_12744,_12750,_12756,_12762,_12768,5-cyan,_12786,3-cyan,_12804,5-green,_12822,7-green,_12840,_12852-green],[_12414,_12426-green,_12438-green,_12450-green,_12456,_12468-green,_12474,_12486-cyan,_12498-cyan,_12504,_12516-cyan,_12528-cyan,_12540-cyan,_12552-cyan,_12558,_12570-cyan,_12582-cyan,_12588,_12600-green,_12606,_12618-green,_12630-green,_12642-green,_12648]]).
%print_g1(C):- compound_var(C,N),underline_print(print_g1(N)),!.


print_g1(C):- mv_peek_color(C,V),C\==V,!,print_g1(V).
print_g1(C):- plain_var(C), write_nbsp,!, nop(( nobject_glyph(C,G),underline_print(print_g1(G-G)))),!.
print_g1(C):- C == black,!, write_nbsp.
print_g1(C-CC):- C == black,CC == black,!,write_nbsp,!.
print_g1(N):- is_grid(N),color_print_ele(magenta,'G'),!.
print_g1(C):- is_bg_color(C),get_bgc(BG),\+ attvar(C),!,color_print_ele(bg(BG),' '),!.
print_g1(N-C):- plain_var(N),print_g1(C).
print_g1(C-N):- plain_var(N),print_g1(C).
print_g1(N-C):- integer(N),is_color(C),!,e_int2glyph(N,G),color_print_ele(C,G).
print_g1(C-N):- integer(N),is_color(C),!,e_int2glyph(N,G),color_print_ele(C,G).
print_g1(C):- compound_var(C,N),nobject_glyph(N,G),underline_print(print_g1(G-G)),!.
print_g1(N):- \+ compound(N), \+ is_colorish(N), print_g1(N-N).
print_g1(N):- into_color_glyph(N,C,Code),as_name(Code,S), color_print_ele(C,S),!.

color_print_ele(C,G):- arc_webui,!,color_print_webui(C,G),!.
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

into_color_glyph(C,wbg,VAR):- plain_var(C),var_dot(VAR),!.
into_color_glyph(N,C,DOT):- is_bg_color(N),get_bgc(C),bg_dot(DOT).
into_color_glyph(N,C,DOT):- is_spec_fg_color(N,C),fg_dot(DOT).
into_color_glyph(N,C,DOT):- is_fg_color(N),N=C,fg_dot(DOT).
into_color_glyph(N,C,DOT):- is_bg_color(N),C=N,bg_dot(DOT).
into_color_glyph(N,C,DOT):- cant_be_color(N,C),cant_be_dot(DOT).
into_color_glyph(N,C,Code):- compound(N),N=(C-G),is_color(C),Code=G.
into_color_glyph(N,C,Code):- compound(N),N=(G-C),is_color(C),Code=G.
into_color_glyph(CTerm,Color,Code):- compound(CTerm),into_color_glyph_ez(CTerm,Color,Code),nonvar_or_ci(Code),!.
into_color_glyph(N,C,Glyph):- var(N),C=N,sformat(chars(Codes),'~p',[N]),last(Codes,Glyph),!.
into_color_glyph(N,Black,BGD):- get_bg_label(BGL),BGL==N, get_bgc(Black), bg_dot(BGD),!.
into_color_glyph(0,Black,BGD):- get_bg_label(BGL),!, into_color_glyph(BGL,Black,BGD).
into_color_glyph(N,FGL,FGD):- get_fg_label(FGL),FGL==N, fg_dot(FGD),!.

i_glyph(N,Glyph):- notrace((i_glyph0(N,Glyph),atom(Glyph))),!.
i_glyph(N,Glyph):- trace,i_glyph0(N,Glyph),atom(Glyph),!.

i_glyph0(N,Glyph):- bg_sym(BG), BG==N, bg_dot(Code), name(Glyph,[Code]).
i_glyph0(Code,Glyph):- integer(Code), Code> 255, name(Glyph,[Code]).
i_glyph0(N,Glyph):- integer(N),quietly((i_sym(N,Code),name(Glyph,[Code]))).
i_glyph0(N,Glyph):- plain_var(N),format(chars(Codes),'~p',[N]),last(Codes,Glyph).
i_glyph0(N,Glyph):- atom(N),atom_length(N,1),Glyph=N.
i_glyph0(N,Glyph):- N>10, integer(N),N3 is N div 3, i_glyph0(N3,Glyph).
%i_glyph(N,Glyph):- atom(N),atom_chars(N,Chars),last(Chars,LGlyph),upcase_atom(LGlyph,Glyph).
                                                                            
i_sym(N2,Code):- integer(N2),!, N is N2, change_code(N,NN), i_syms(Codes),nth0(NN,Codes,Code),!.
i_sym(N2,Code):- atom(N2),name(N2,[C|_]),!,i_sym(C,Code).
i_sym(N,Code):- plain_var(N), Code = 63.
%change_code(N,M):- M is N * 100,!.
%change_code(N,M):- N>10, M is (N * 10 ),!.
change_code(N,N). % M is N+212.


print_g1(_,_, E):- print_g1(E),!. 
%print_g1(_,_,C):- trace, write(C).


save_codes(Max):- 
 %stream_property(File,file_no(1)),
 with_output_to(codes(CCC),
 ((forall((between(0,Max,Code),
     code_type(Code,graph),  
  \+ code_type(Code,white),
  \+ between(688,1000,Code),
  \+ between(1350,4600,Code),
  \+ between(4650,5000,Code),
  \+ between(5850,11500,Code),
  %(42600 > Code),
  
  \+ resrv_dot(Code)
   % ignore((0 is Code mod 50, format(File,'\n\n~d:',[Code]), put_code(File,Code))),
  ),put_code(Code))))),
  % format('~N~s~N',[CCC]),
  assertz(i_syms(CCC)).

save_codes:- save_codes(42600).

:- ignore(save_codes).

/*
get_glyph(Point,Glyph):-  
  get_grid_num(Point,N),i_glyph(N,Glyph).
*/

:- fixup_exports.
%:- fixup_module_exports_now.

