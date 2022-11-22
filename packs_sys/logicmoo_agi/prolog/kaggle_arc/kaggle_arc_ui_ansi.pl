:- encoding(octet).
/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/

:- include(kaggle_arc_header).

:- autoload(library(http/html_write),[html/3,print_html/1]).

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
tersify(I,O):- quietly((tersify2(I,M),tersify3(M,O))),!.

%srw_arc(I,O):- is_grid(I),!, wots(O,(write('"'),print_grid(I),write('"'))).
%srw_arc(I,O):- compound(I),!, wots(O,(write(ppt(I)))).
/*
srw_arc(I,O):- is_grid(I),!, wots(O,(write('"'),print_grid(I),write('"'))).
*/
srw_arc(I,O):- is_map(I),!, O='..vvmm..'.
srw_arc(I,O):- is_grid(I),!, O='..grid..'.
/*
srw_arc(List,O):- current_prolog_flag(dmsg_len,Three),
  is_list(List),length(List,L),L>Three,
   append([A,B,C],[F|_],List),F \='...'(_), !, 
  simplify_goal_printed([A,B,C,'....'(L>Three)],O).
*/
%srw_arc(gridFn(_),gridFn):-!.
%srw_arc(I,O):- is_points_list(I), length(I,N),N>10,!,O='..points..'(N),!.
%srw_arc(I,O):- is_list(I), length(I,N),N>10,!,O='..points..'(N),!.
srw_arc(I,O):- tersify(I,O),!,I\==O,!.

:- multifile(dumpst_hook:simple_rewrite/2).
:- dynamic(dumpst_hook:simple_rewrite/2).

dumpst_hook:simple_rewrite(I,O):- 
  \+ nb_current(arc_can_portray,nil),
  current_predicate(bfly_startup/0),
  current_predicate(is_group/1), 
  b_setval(arc_can_portray,nil),
  locally(b_setval(arc_can_portray,nil),once((compound(I), lock_doing(srw_arc,I,srw_arc(I,O))))), I\==O, I\=@=O, !, \+ I=O,
  b_setval(arc_can_portray,t).



portray_terse:- true,!.

:- discontiguous arc_portray/2. 

arc_portray(S,_):- term_is_ansi(S), !, write_keeping_ansi_mb(S).
arc_portray(Map,TF):- get_map_pairs(Map,Type,Pairs),!, arc_portray_pairs(Type,TF,Pairs). 

arc_portray_t(G, _):- is_map(G), !, write_map(G,'arc_portray_t').
arc_portray_t(G, _):- is_grid(G),  !, data_type(G,W),writeq(grid(W)).

arc_portray(G, _):- is_map(G),  !, write_map(G,'arc_portray').
arc_portray(G, TF):- TF == true, portray_terse, arc_portray_t(G, TF),!.
arc_portray(G, TF):- catch(arc_portray_nt(G, TF),_,fail),!.
%arc_portray(G, _TF):- writeq(G),!.

% Portray In Debugger

arc_portray_nt(G,false):- is_grid(G), print_grid(G),!.

arc_portray_nt(G0, _):- is_list(G0), length(G0,L),L>3, ppt(G0),!.
arc_portray_nt(G0, true):- is_group(G0), ppt(G0),!.
%arc_portray_nt(G0, false):- is_group(G0), ppt(G0),!.
arc_portray_nt(G0, false):- is_group(G0), into_list(G0,G), length(G,L),% L>1, !,
   dash_chars, 
   once(((why_grouped(_TestID,Why,WG),WG=@=G,fail);(Why = (size2D=L)))),!,
   print_grid(Why,G),nl,
   %underline_print(writeln(Why)),
   print_info_l(G),
   dash_chars.


arc_portray_nt(G,_False):- is_object(G), wots(S,ppt(G)), debug_as_grid(S,G).
  %object_grid(G,OG), 
  %neighbor_map(OG,NG), !,
  %print_grid(object_grid,NG),nl,
  %underline_print(debug_indiv(G)),
  
arc_portray_nt(G,false):- via_print_grid(G),!, grid_size(G,H,V),!,H>0,V>0, print_grid(H,V,G).

% Portray In tracer
arc_portray_nt(G,true):- is_object(G),underline_print((ppt(G))).
arc_portray_nt(G,true):- via_print_grid(G),write_nbsp,underline_print((ppt(G))),write_nbsp.
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
 nl_if_needed,
 arc_portray_1_pair(Ps,K,Val,TF),
 probably_nl.

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

  

via_print_grid(G):- is_points_list(G). %,!,fail,grid_size(G,H,V),number(H),number(V),H>1,V>1.
via_print_grid(G):- is_grid(G).
via_print_grid(G):- is_object(G).
via_print_grid(G):- is_group(G).
via_print_grid(G):- is_really_gridoid(G).

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
tersify1(I,Q):- is_object(I),object_glyph_one_color(I,FC), o2g(I,O),!,wots(A,color_print(FC,call(format('"~w"',[O])))),
   amass(I,M),
   wots(S,call(write(objFn(A,M)))),atom_string(Q,S).
tersify1(I,O):- is_map(I), get_kov(objs,I,_),!, O='$VAR'('VM').
tersify1(I,O):- is_map(I), get_kov(pairs,I,_),!, O='$VAR'('Training').


tersifyG(I,O):- tersifyL(I,O),numbervars(O,1,_,[attvar(bind),singletons(false)]),!.

tersifyL([H|I],O):- nonvar(H), \+ is_group(I),  with_output_to(string(S),display(I)), atom_length(S,N), N>170, 
  length(I,LL),tersify(H,HH),(('...'(HH,LL,'...'(N)))=O),!.
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

ppt(_):- is_print_collapsed,!.
ppt(G):- is_map(G), !, write_map(G,'ppt').
ppt(S):- term_is_ansi(S), !, write_keeping_ansi_mb(S).
ppt(P):- compound(P),wqs1(P),!.
ppt(P):- \+ \+ ((tersify(P,Q),!,pp(Q))),!.
ppt(C,P):- \+ \+ ((tersify(P,Q),!,pp(C,Q))),!.

pp(Color,P):- ignore((quietlyd((wots(S,ptcol(P)),!,color_print(Color,S))))).
ptcol(call(P)):- callable(P),!,call(P).
ptcol(P):- pp(P).

ptc(Color,Call):- pp(Color,call(Call)).

wqln(Term):- wqnl(Term).
wqnl(Term):- is_list(Term),!,g_out(wqs(Term)).
wqnl(Term):- nl_if_needed,format('~q',[Term]),probably_nl.

pp(_):- is_print_collapsed,!.
%pp(Term):- is_toplevel_printing(Term), !, nl_if_needed, pp_no_nl(Term),!,probably_nl.
pp(Term):- nl_if_needed, az_ansi(pp_no_nl(Term)),!,probably_nl.

%p_p_t_no_nl(Term):- is_toplevel_printing(Term), !, print_tree_no_nl(Term).
p_p_t_no_nl(Term):- az_ansi(print_tree_no_nl(Term)).

ppt_no_nl(P):- tersify(P,Q),!,pp_no_nl(Q).

is_toplevel_printing(_):- \+ is_string_output, line_position(current_output,N),  N<2, fail.

pp_no_nl(P):- var(P),!,pp(var_pt(P)),nop((dumpST,break)).
pp_no_nl(S):- term_is_ansi(S), !, write_keeping_ansi_mb(S).
pp_no_nl(P):- atom(P),atom_contains(P,'~'),!,format(P).
pp_no_nl(G):- is_map(G), !, write_map(G,'pp').
%pp_no_nl(S):- term_is_ansi(S), !, write_keeping_ansi_mb(S).
pp_no_nl(P):- \+ \+ (( pt_guess_pretty(P,GP),ptw(GP))).
%pp(P):-!,writeq(P).
%ptw(P):- quietlyd(p_p_t_nl(P)),!.
%ptw(_):- nl_if_needed,fail.
ptw(P):- var(P),!,ptw(var_ptw(P)),nop((dumpST,break)).
ptw(G):- is_map(G), !, write_map(G,'ptw').
ptw(S):- term_is_ansi(S), !, write_keeping_ansi_mb(S).
ptw(P):- p_p_t_no_nl(P),!.

%ptw(P):- quietlyd(write_term(P,[blobs(portray),quoted(true),quote_non_ascii(false), portray_goal(print_ansi_tree),portray(true)])),!.
print_ansi_tree(S,_):- term_is_ansi(S), !, write_keeping_ansi_mb(S).
print_ansi_tree(P,_):- catch(arc_portray(P),_,fail),!.
print_ansi_tree(P,_OL):- catch(p_p_t_no_nl(P),_,fail),!.

%p_p_t_nl(T):- az_ansi(print_tree_nl(T)).
%p_p_t(T):- az_ansi(print_tree(T)).

pt_guess_pretty(P,O):- \+ nb_current(in_pt_guess_pretty,t), locally(nb_setval(in_pt_guess_pretty,t),pt_guess_pretty_1(P,O)).
pt_guess_pretty(O,O).

upcase_atom_var_l(IntL,NameL):- upcase_atom_var(IntL,NameL).
upcase_atom_var_l(IntL,NameL):- is_list(IntL),!,maplist(upcase_atom_var_l,IntL,NameL).

pt_guess_pretty_1(P,O):- copy_term(P,O,_),
  ignore((sub_term(Body,O), compound(Body), Body=was_once(InSet,InVars),upcase_atom_var_l(InSet,InVars))),
  ignore(pretty1(O)),ignore(pretty_two(O)),ignore(pretty_three(O)),ignore(pretty_final(O)),!,
  ((term_singletons(O,SS),numbervars(SS,999999999999,_,[attvar(skip),singletons(true)]))).

:- dynamic(pretty_clauses:pp_hook/3).
:- multifile(pretty_clauses:pp_hook/3).
:- module_transparent(pretty_clauses:pp_hook/3).
pretty_clauses:pp_hook(_,Tab,S):- is_vm(S),!,prefix_spaces(Tab),!,write('..VM..').
pretty_clauses:pp_hook(_,Tab,S):- term_is_ansi(S), !,prefix_spaces(Tab), write_keeping_ansi_mb(S).
pretty_clauses:pp_hook(FS,_  ,G):- 
  current_predicate(is_group/1),
   locally(b_setval(pp_parent,FS),print_with_pad(pp_hook_g(G))),!.

pp_parent(PP):- nb_current(pp_parent,PP),!.
pp_parent([]):-!.

lock_doing(Lock,G,Goal):- 
 (nb_current(Lock,Was);Was=[]), !, 
  \+ ((member(E,Was),E==G)),
  locally(nb_setval(Lock,[G|Was]),Goal).

pp_hook_g(S):- term_contains_ansi(S), !, write_nbsp, pp_hook_g0(S).
pp_hook_g(S):- term_is_ansi(S), !, write_keeping_ansi_mb(S).
pp_hook_g(G):- \+ plain_var(G), \+ nb_current(arc_can_portray,nil),
  lock_doing(in_pp_hook_g,G,pp_hook_g0(G)).

pp_hook_g0(S):- term_is_ansi(S), !, write_nbsp, write_keeping_ansi_mb(S).
pp_hook_g0(_):- in_pp(bfly),!,fail.
pp_hook_g0(G):- wots(S,in_bfly(f,pp_hook_g1(G))),write(S).

mass_gt1(O1):- into_obj(O1,O2),mass(O2,M),!,M>1.

% Pretty printing

as_grid_string(O,SSS):- wots_vs(S,debug_as_grid(O)), sformat(SSS,'{  ~w}',[S]).
as_pre_string(O,SS):- wots(S,debug_as_grid(O)), strip_vspace(S,SS).

pp_hook_g1(O):-  plain_var(O), !, fail.
pp_hook_g1(O):-  attvar(O), !, is_colorish(O), data_type(O,DT), writeq('...'(DT)),!.
pp_hook_g1(S):- term_is_ansi(S), !, write_nbsp, write_keeping_ansi_mb(S).

pp_hook_g1(shape(O)):- !, is_points_list(O), as_grid_string(O,S), print(shape(S)),!.
pp_hook_g1(vals(O)):- !, writeq(vals(O)),!.
pp_hook_g1(localpoints(O)):- !, is_points_list(O), as_grid_string(O,S), print(localpoints(S)),!.
pp_hook_g1(C):- compound(C), compound_name_arguments(C,F,[O]),is_points_list(O), length(O,N),N>2, as_grid_string(O,S), compound_name_arguments(CO,F,[S]), print(CO),!.

pp_hook_g1(O):-  is_points_list(O),as_grid_string(O,S),print(S),!.
pp_hook_g1(O):-  is_real_color(O), color_print(O,call(writeq(O))),!.
pp_hook_g1(O):-  is_colorish(O), data_type(O,DT), writeq('...'(DT)),!.
pp_hook_g1(O):-  is_grid(O), 
% \+ (sub_term(E,O),compound(E),E='$VAR'(_)), 
  catch((wots(S,print_grid(O)),strip_vspace(S,SS),ptc(orange,(format('"  ~w  "',[SS])))),_,fail).

pp_hook_g1(_):-  \+ in_pp(ansi),!, fail.

pp_hook_g1(O):- atom(O), atom_contains(O,'o_'), pp_parent([LF|_]), \+ (LF==lf;LF==objFn), 
  resolve_reference(O,Var), O\==Var, \+ plain_var(Var),!, 
  write(' '), writeq(O), write(' /* '), debug_as_grid(Var), write(' */ ').

pp_hook_g1(O):-  is_object(O),pp_no_nl(O), !.
pp_hook_g1(O):-  is_group(O),pp_no_nl(O), !.

%pp_hook_g1(change_obj(N,O1,O2,Sames,Diffs)):-  showdiff_objects5(N,O1,O2,Sames,Diffs),!.

pp_hook_g1(O):-  is_map(O),data_type(O,DT), writeq('..map.'(DT)),!.
pp_hook_g1(O):-  is_really_gridoid(O),debug_as_grid(O), !.
%pp_hook_g1(O):-  O = change_obj( O1, O2, _Same, _Diff),  with_tagged('h5',collapsible_section(object,[O1, O2],pp(O))).
%pp_hook_g1(O):-  O = change_obj( O1, O2, _Same, _Diff), collapsible_section(object,showdiff_objects(O1,O2)),!.
%pp_hook_g1(O):-  O = change_obj( O1, O2, _Same, _Diff),  collapsible_section(object,[O1, O2],with_tagged('h5',pp(O))).
%pp_hook_g1(O):-  O = diff(A -> B), (is_really_gridoid(A);is_really_gridoid(B)),!, p_c_o('diff', [A, '-->', B]),!.
pp_hook_g1(O):-  O = showdiff( O1, O2), !, showdiff(O1, O2).
pp_hook_g1(O):- compound(O),wqs1(O),!.

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


print_nl(P):- format('~N~t'), wots(SS,pp_no_nl(P)), maybe_color(SS,P),nl_if_needed.

color_write(S):- term_is_ansi(S), !, write_keeping_ansi_mb(S).
color_write(P):- wots(SS,write(P)), maybe_color(SS,P).

write_keeping_ansi_mb(P):- is_maybe_bold(P,write_keeping_ansi(P)).

is_maybe_bold(P):- sformat(S,'~w',[P]),atom_contains(S,'stOF').

is_maybe_bold(P,G):- is_maybe_bold(P),!, underline_print(bold_print(G)).
is_maybe_bold(_P,G):- call(G).

pp_msg_color(P,C):- compound(P),pc_msg_color(P,C),!.
pp_msg_color(P,C):- must_det_ll(mesg_color(P,C)).
pc_msg_color(iz(P),C):- pp_msg_color(P,C).
pc_msg_color((_->P),C):- pp_msg_color(P,C).
pc_msg_color([P|_],C):- pp_msg_color(P,C).
pc_msg_color(diff(P),C):- pp_msg_color(P,C).

wots_vs(SSS,G):- wots(S,G),strip_vspace(S,SS), (atom_contains(SS,'\n') -> wots(SSS,(nl,write('   '),write(SS),nl));SSS=SS).


wqs_l(H):- \+ is_list(H),!, wqs(H).
wqs_l([]).
wqs_l([H|T]):- !, wqs(H), write_nbsp, wqs_l(T).


maybe_color(SS,_):- term_contains_ansi(SS),!, write_nbsp, write(SS).
maybe_color(SS,P):- term_contains_ansi(P),!,write_nbsp, write(SS).
maybe_color(SS,P):- pp_msg_color(P,C), ansicall(C,is_maybe_bold(P,write(SS))),!.

wqs(P):- wots(SS,wqs0(P)), maybe_color(SS,P).

 
wqs(C,P):- ansicall(C,wqs0(P)),!.

wqs0(X):- plain_var(X), !, wqs(plain_var(X)). 
wqs0(G):- compound(G), G = call(C),callable(C),!,call(C).
wqs0(G):- is_map(G), !, write_map(G,'wqs').
wqs0(X):- attvar(X), !, wqs(attvar(X)). 
wqs0(nl):- !, nl. wqs0(''):-!. wqs0([]):-!.
wqs0([H|T]):- is_list(T), string(H), !, wqs_l([H|T]).
wqs0([H|T]):- is_list(T), !, wqs(H),need_nl(H,T), wqs(T).
wqs0(S):- term_is_ansi(S), !, write_keeping_ansi_mb(S).
wqs0(X):- is_object(X), tersify1(X,Q), X\==Q,!, wqs(Q).
wqs0(X):- is_object(X), show_shape(X),!.
wqs0(X):- is_grid(X), !, print_grid(X).
wqs0(X):- string(X), atom_contains(X,'~'), catch((sformat(S,X,[]),color_write(S)),_,fail),!.
wqs0(X):- string(X), !, color_write(X).
%wqs([H1,H2|T]):- string(H1),string(H2),!, write(H1),write_nbsp, wqs([H2|T]).
%wqs([H1|T]):- string(H1),!, write(H1), wqs(T).
wqs0([skip(_)|T]):- !,wqs(T).
%wqs([H|T]):- compound(H),!, writeq(H), wqs(T).

wqs0(call(C)):- !, call(C).
wqs0(C):- is_color(C),!,wqs(color_print(C,C)).
wqs0(X):- \+ compound(X),!, write_nbsp, write(X).
wqs0(C):- compound(C),wqs1(C),!.
wqs0(C):- wqs2(C).
%wqs(S):- term_contains_ansi(S), !, write_nbsp, write_keeping_ansi_mb(S).

wqs2(S):- term_contains_ansi(S), !, write_nbsp, write_keeping_ansi_mb(S).
wqs2(X):- !, write_nbsp,writeq(X).
wqs2(X):- write_nbsp,write_term(X,[quoted(true)]).



as_arg_str(C,S):- wots_vs(S,print(C)).

arg_string(S):- string(S),!.
arg_string(S):- term_contains_ansi(S),!.

wqs1(C):- \+ compound(C),!,wqs0(C).
wqs1(format(C,N)):- catch((sformat(S,C,N),color_write(S)),_,fail),!.
wqs1(writef(C,N)):- !, writef(C,N).
wqs1(q(C)):-  \+ arg_string(C),wots(S,writeq(C)),color_write(S).
wqs1(g(C)):-  \+ arg_string(C), wots_vs(S,bold_print(wqs(C))),print(g(S)).
wqs1(b(C)):-  \+ arg_string(C), wots_vs(S,bold_print(wqs(C))),color_write(S).
wqs1(norm(C)):- writeq(norm(C)),!.

wqs1(pp(C)):- \+ arg_string(C), wots_vs(S,pp_no_nl(C)),write((S)).
wqs1(ppt(C)):- \+ arg_string(C), wots_vs(S,ppt_no_nl(C)),write((S)).
wqs1(vals(C)):- writeq(vals(C)),!.
%wqs1(colors(C)):- \+ arg_string(C), as_arg_str(C,S),wqs(colorsz(S)).
wqs1(io(C)):-  \+ arg_string(C),wots_vs(S,bold_print(wqs(C))),write(io(S)).

wqs1(uc(C,W)):- !, write_nbsp, color_print(C,call(underline_print(format("\t~@",[wqs(W)])))).
wqs1(cc(C,N)):- is_color(C),!,color_print(C,call(writeq(cc(C,N)))).
wqs1(-(C,N)):- is_color(C),!,color_print(C,call(writeq(C))), write('-'), wqs(N).
wqs1(cc(C,N)):- N\==0,attvar(C), get_attrs(C,PC), !, wqs(ccc(PC,N)).
wqs1(cc(C,N)):- N\==0,var(C), sformat(PC,"~p",[C]), !, wqs(ccc(PC,N)).
wqs1(cc(C,N)):- \+ arg_string(C), wots(S,color_print(C,C)), wqs(cc(S,N)).
wqs1(color_print(C,X)):- is_color(C), !, write_nbsp, color_print(C,X).
wqs1(color_print(C,X)):- \+ plain_var(C), !, write_nbsp, color_print(C,X).
wqs1(X):- compound(X), compound_name_arguments(X,_,[Arg]),is_really_gridoid(Arg),area_or_len(Arg,Area),Area<5,writeq(X),!.
% wqs1(C):- callable(C), is_wqs(C),wots_vs(S,catch(C,_,fail)),write((S)).
wqs1(X):- compound(X), compound_name_arguments(X,F,[Arg]),is_really_gridoid(Arg),wots_vs(VS,print_grid(Arg)),
  writeq(F),write('(`\n'),!,write(VS),write('`)').

is_really_gridoid(G):- is_gridoid(G),(is_list(G) -> ( \+ (member(E,G),non_gridoid_cell(E))); true).
non_gridoid_cell(C):- plain_var(C),!,fail.
non_gridoid_cell(C):- is_color(C),!.
non_gridoid_cell(ord(_,_)).
non_gridoid_cell(cc(_,_)).
non_gridoid_cell(_-Num):- number(Num).


%probably_nl :- arc_webui,!,write('<br/>').
nl_if_needed :- format('~N').
probably_nl :- format('~N').
%write_nbsp:- arc_webui,!,write('&nbsp;').
write_nbsp:- write(' ').

is_breaker(P):- compound(P),functor(P,_,A), A>=3.
need_nl(_,_):- arc_webui,!,write_nbsp.
%need_nl(_,_):- !,write_nbsp.
need_nl(H,[P|_]):- \+ is_breaker(H),is_breaker(P),line_position(user_output,L1),L1>80,nl,bformatc1('\t\t').
need_nl(_,_):- line_position(user_output,L1),L1>160,nl,bformatc1('\t\t').
need_nl(_,_).

dash_chars:- dash_chars(40),!.
dash_chars(H):- integer(H), dash_border(H).
dash_chars(S):- nl_if_needed,dash_chars(60,S),probably_nl.
dash_chars(H,_):- H < 1,!.
dash_chars(H,C):- forall(between(0,H,_),bformatc1(C)).

%dash_uborder_no_nl_1:-  line_position(current_output,0),!, bformatc1('\u00AF\u00AF\u00AF ').
%dash_uborder_no_nl_1:-  line_position(current_output,W),W==1,!, bformatc1('\u00AF\u00AF\u00AF ').
dash_uborder_no_nl_1:- bformatc1('\u00AF\u00AF\u00AF ').
dash_uborder_no_nl_1:- uborder(Short,Long),!, bformatc1(Short),bformatc1(Long),write_nbsp.
dash_uborder_no_nl(1):- !, dash_uborder_no_nl_1.
dash_uborder_no_nl(Width):- WidthM1 is Width-1, uborder(Short,Long),write(' '), write(Short),dash_chars(WidthM1,Long),!.
dash_uborder_no_nl(Width):- WidthM1 is Width-1, write_nbsp, bformat('\u00AF'),dash_chars(WidthM1,'\u00AF\u00AF'),!.
dash_uborder_no_nl(Width):- nl_if_needed, WidthM1 is Width-1, bformatc1(' \u00AF'),dash_chars(WidthM1,'\u00AF\u00AF').

dash_uborder(Width):- nl_if_needed,dash_uborder_no_nl(Width),nl.

uborder('-','--'):- stream_property(current_output,encoding(utf8)),!.
uborder('\u00AF','\u00AF\u00AF'):- !. %stream_property(current_output,encoding(text)).
%uborder('-','--').

dash_border_no_nl_1:-  line_position(current_output,0),!, bformatc1(' ___ ').
dash_border_no_nl_1:-  line_position(current_output,W),W==1,!, bformatc1('___ ').
dash_border_no_nl_1:- bformatc1(' ___ ').

%dash_border_no_nl(Width):- write(''),dash_chars(Width,'_'),write(' '),!.

dash_border_no_nl(Width):- nl_if_needed, WidthM1 is Width-1, bformatc1(' _'),dash_chars(WidthM1,'__').

dash_border(Width):- !, dash_border_no_nl(Width),nl,!.

functor_test_color(pass,green).
functor_test_color(fail,red).
functor_test_color(warn,yellow).

arcdbg(G):- is_map(G), !, write_map(G,'arcdbg').
arcdbg(G):- compound(G), compound_name_arity(G,F,_),functor_test_color(F,C),
  wots(S,print(G)),color_print(C,S),!,probably_nl.
arcdbg(G):- wdmsg(G).


%user:portray(Grid):- ((\+ tracing, is_group(Grid),print_grid(Grid))).
%user:portray(Grid):- quietlyd((is_object(Grid),print_grid(Grid))).

banner_lines(Color):- nl_if_needed,
  color_print(Color,'--------------------------------------------------------------'),nl,
  color_print(Color,'=============================================================='),nl,
  color_print(Color,'--------------------------------------------------------------'),nl,
  color_print(Color,'=============================================================='),nl,
  color_print(Color,'--------------------------------------------------------------'),nl,!.

print_ss(A):- ( \+ compound(A) ; \+ (sub_term(E,A), is_really_gridoid(E))),!, wdmsg(print_ss(A)),!.
print_ss(A):- grid_footer(A,G,W),writeln(print_ss(W)), print_ss(G,W),!.
print_ss(A):- must_det_ll(( format('~N'), into_ss_string(A,SS),!,
  SS = ss(L,Lst),
  writeln(print_ss(l(L))), 
  forall(member(S,Lst),writeln(S)),format('~N'))),!.

print_ss(G,W):- is_really_gridoid(G),!,must_det_ll(print_grid(W,G)).

print_side_by_side([]):-!.
%print_side_by_side([A,B,C,D|Rest]):- wots(AB,print_side_by_side(A,B)),wots(AC,print_side_by_side(C,D)),!,print_side_by_side(AB,AC),print_side_by_side(Rest).
%print_side_by_side([A,B,C|Rest]):- wots(AB,print_side_by_side(A,B)),wots(AC,print_grid(C)),print_side_by_side(AB,AC),!,print_side_by_side(Rest).
print_side_by_side(List):- member(Size,[15,10,8,6,4,3]), once((length(Left,Size), append(Left,Rest,List), reverse(Left,RLeft),
   sorted_by_vertical_size(RLeft), list_print_length(RLeft,Len))), Len < 200, !, print_side_by_side_three(Left),
 print_side_by_side(Rest).
print_side_by_side([A,B|Rest]):- print_side_by_side(A,B),format('~N'),!,print_side_by_side(Rest),!.
print_side_by_side([A|Rest]):- print_ss(A), print_side_by_side(Rest),!.

vertical_grid_size_with_key(Grid-N,V+H+N+F):- always_grid_footer(Grid,GG,F),grid_size(GG,H,V).


sorted_by_vertical_size(List):- sort_by_vertical_size(List,Sorted),!,List=@=Sorted.
sort_by_vertical_size(List,Sorted):- lists:number_list(List, 1, Numbered),
  predsort(sort_on(vertical_grid_size_with_key),Numbered,SortedKeys),maplist(arg(1),SortedKeys,Sorted),!.
grid_with_footer_string(C,CGS):- always_grid_footer(C,CG,CF),wots_vs(CGS,print_grid(CF,CG)).

print_side_by_side_three([]):-!.
print_side_by_side_three([B,C]):- !,
  grid_with_footer_string(B,BGS),
  grid_with_footer_string(C,CGS),
  print_side_by_side0(BGS,_,CGS).
%print_side_by_side_three([L|List]):- is_object(L), print_side_by_side([L|List]),!, % length(List,L),
%  nop((length(Left,5),append(Left,Rest,List),length(Rest,NN),NN>1,!, 
%  print_side_by_side_three([L|Left]),print_side_by_side_three(Rest))),!.
print_side_by_side_three([A|BC]):-  
  grid_with_footer_string(A,AGS),
  wots_vs(BCGS,print_side_by_side_three(BC)),
  print_side_by_side0(AGS,_,BCGS),!.
% should never get to the old code
print_side_by_side_three([A,B,C]):- !,
   always_grid_footer(A,AG,AF),
   wots(OS,(wots_vs(AC,print_grid(AF,AG)),write(AC),write('  '))),
   wots_vs(AB,print_side_by_side(B,C)),
   print_side_by_side(OS,AB),!.


g_nonvar(A,AA):- nonvar(A);nonvar(AA).

%print_side_by_side2(A,B):- (unsized_grid(A);unsized_grid(B)),!, print_ss(A),print_ss(B),!.
%print_side_by_side2(A,B):- g_smaller_than(A,B),!, print_ss(A),print_ss(B),!.
%print_side_by_side2(A,B):-  print_ss(A),print_ss(B),!.
always_grid_footer(A,GG,FF):- grid_footer(A,GG,GF),!,into_wqs_string(GF,FF).
always_grid_footer(A,A,"").

ensure_grid_footer(A,GGGF):- always_grid_footer(A,GG,GF),add_grid_label(GG,GF,GGGF).

is_wqs_f(A):- \+ atom(A),!,fail. is_wqs_f(wqs). is_wqs_f(print_grid). is_wqs_f(format). is_wqs_f(call). 
is_wqs_f(A):- is_wqs_ff(PP),atom_concat(PP,_,A). is_wqs_ff('print'). is_wqs_ff('pp'). is_wqs_ff('wq'). is_wqs_ff('write').
is_wqs(M):-compound(M),compound_name_arity(M,F,_),is_wqs_f(F),!.
into_wqs(M,WQS):- is_wqs(M),!,WQS=M.
into_wqs(M,wqs(C,ppt(M))):- pp_msg_color(M,C),!.

into_wqs_string(N1,NS1):- compound(N1), N1 = (A + B), !, into_wqs_string(A,AS),into_wqs_string(B,BS),sformat(NS1,'~w ~w',[AS,BS]).
into_wqs_string(N1,NS1):- string(N1),!,NS1=N1.
into_wqs_string(N1,NS1):- into_wqs(N1,WQS1),wots_vs(NS1,call(WQS1)).

add_grid_label(I,M,IM):- into_wqs_string(M,WQS),IM=..[-,I,WQS]. 

print_side_by_side(G1N1,G2N2):-
   always_grid_footer(G1N1,G1,N1),
   always_grid_footer(G2N2,G2,N2),
   pp_msg_color(N1,TitleColor),
   print_side_by_side6(TitleColor,G1,N1,_LW,G2,N2).

print_side_by_side(Info,G1N1,G2N2):- is_gridoid(G1N1),!,
  print_side_by_side_msg(Info,G1N1,G2N2).
print_side_by_side(TitleColor,G1N1,G2N2):- is_color(TitleColor),!,
  always_grid_footer(G1N1,G1,N1),
  always_grid_footer(G2N2,G2,N2),  
  print_side_by_side6(TitleColor,G1,N1,_LW,G2,N2).
print_side_by_side(X,Y,Z):- (var(Y);number(Y)),!, g_out((nl,print_side_by_side0(X,Y,Z))),!.
print_side_by_side(Info,G1N1,G2N2):- 
  print_side_by_side_msg(Info,G1N1,G2N2).

print_side_by_side_msg(Info,G1N1,G2N2):-
  always_grid_footer(G1N1,G1,N1),
  always_grid_footer(G2N2,G2,N2),
  pp_msg_color(Info,TitleColor),
  into_wqs_string(Info,String),
  print_side_by_side6(TitleColor,G1,String+N1,_LW,G2,N2).

print_side_by_side(C,G1,N1,G2,N2):- 
  print_side_by_side6(C,G1,N1,_LW,G2,N2).

print_side_by_side(TitleColor,G1,N1,LW,G2,N2):- 
  print_side_by_side6(TitleColor,G1,N1,LW,G2,N2).

:- meta_predicate(print_side_by_side6(+,+,+,?,+,+)).
print_side_by_side6(TitleColor,G1,N1,LW,G2,N2):-
   g_out((nl,
   print_side_by_side0(G1,LW,G2),
   data_type(G1,S1), data_type(G2,S2),
   into_wqs_string(N1,NS1),
   into_wqs_string(N2,NS2),
   print_side_by_side_footer(TitleColor,S1,NS1,LW,S2,NS2))).

% SWAP
print_side_by_side_footer(TitleColor,S1,N1,LW0,S2,N2):- number(LW0), LW0 < 0, LW is -LW0, !, 
   print_side_by_side_footer(TitleColor,S2,N2,LW,S1,N1).
print_side_by_side_footer(TitleColor,S1,N1,_LW,S2,N2):- 
   nl_if_needed, write('\t'),format_footer(TitleColor,N1,S1),write('\t\t'),format_footer(TitleColor,N2,S2),write('\n'),!.


unsized_grid(A):- is_really_gridoid(A),!,fail.
unsized_grid(A):- grid_footer(A,Grid,_Text), !, \+ is_really_gridoid(Grid),!.
unsized_grid(A):- \+ is_really_gridoid(A),!.

grid_footer(G,_,_):- \+ compound(G),!,fail.
grid_footer((GF=GG),GG,GF):-!.
grid_footer(Obj,GG,GF):- is_object(Obj), vis2D(Obj,H,V),localpoints(Obj,Ps),points_to_grid(H,V,Ps,GG), object_dglyphH(Obj,GF),!.
grid_footer(print_grid(GF,GG),GG,GF):-!.
grid_footer(print_grid(_,_,GF,GG),GG,GF):-!.
grid_footer((GG-GF),GG,GF):- is_grid(GG), !.
grid_footer((GF-GG),GG,GF):- is_grid(GG), !.
grid_footer((GG-GF),GG,GF):- is_really_gridoid(GG), !.
grid_footer((GF-GG),GG,GF):- is_really_gridoid(GG), !.
grid_footer((GG-wqs(GF)),GG,wqs(GF)):- nonvar(GF),!.
grid_footer((GF-GG),[['?']],GF):- GG==[], !.
grid_footer((GG-GF),[['?']],GF):- GG==[], !.
grid_footer((GF-GG),GG,GF):- \+ is_really_gridoid(GF), !.
grid_footer((GG-GF),GG,GF):- \+ is_really_gridoid(GF), !.
grid_footer((GG-GF),GG,GF).


g_smaller_than(A,B):- grid_footer(A,AA,_),!,g_smaller_than(AA,B).
g_smaller_than(B,A):- grid_footer(A,AA,_),!,g_smaller_than(B,AA).
g_smaller_than(A,B):- is_really_gridoid(A),is_really_gridoid(A),!, vis2D(A,_,AV),vis2D(B,_,BV), BV>AV.

gridoid_size(G,30,30):- \+ compound(G),!.
gridoid_size(print_grid(H,V,_),H,V):- nonvar(H),nonvar(V),!.
gridoid_size(print_grid(H,V,_,_),H,V):- nonvar(H),nonvar(V),!.
gridoid_size(print_grid0(H,V,_),H,V):- nonvar(H),nonvar(V),!.
gridoid_size(print_grid0(H,V,_,_),H,V):- nonvar(H),nonvar(V),!.
gridoid_size(G,H,V):- compound_name_arity(G,print_grid,A),arg(A,G,GG),gridoid_size(GG,H,V).
gridoid_size(G,H,V):- is_really_gridoid(G),!,grid_size(G,H,V).

print_side_by_side0([],_,[]):-!.
%print_side_by_side0(A,_,B):- (unsized_grid(A);unsized_grid(B)),!, writeln(unsized_grid), print_ss(A),print_ss(B),!.
% % % print_side_by_side0(A,_,B):- g_smaller_than(A,B),!, writeln(g_smaller_than), print_ss(A),print_ss(B),!.
/*
print_side_by_side0(C1-call(wqs(S1)),LW,C2-call(wqs(S2))):- nonvar(S1),!,
  print_side_by_side0(C1,LW,C2),nl_if_needed,
  print_side_by_side0(wqs(S1),LW,wqs(S2)).
print_side_by_side0(C1-A,LW,C2-B):- nonvar(A),!,
  print_side_by_side0(C1,LW1,C2),nl_if_needed,
  print_side_by_side0(A,LW2,B),
  ignore(max_min(LW1,LW2,LW,_)).
*/
print_side_by_side0(C1,LW,C2):- var(LW), fail, gridoid_size(C1,H1,V1),gridoid_size(C2,H2,V2),!,    
    ((V2 > V1) -> LW is -(H2 * 2 + 12) ; LW is (H1 * 2 + 12)),!, print_side_by_side0(C1,LW,C2).
print_side_by_side0(C1,LW,C2):-  var(LW), LW=30,print_side_by_side0(C1,LW,C2),!.

print_side_by_side0(C1,W0,C2):- number(W0), W0 < 0, LW is -W0, !, print_side_by_side0(C2,LW,C1).

print_side_by_side0(C1,W0,C2):- number(W0), LW is floor(abs(W0)),
  locally(nb_setval(print_sbs,left),into_ss_string(C1,ss(W1,L1))),
  locally(nb_setval(print_sbs,right),into_ss_string(C2,ss(_,L2))),!,
  with_style("font-size2D:100%",print_side_by_side_lists_1st(L1,W1,L2,LW)).

is_side(RL):- nb_current(print_sbs,RL),RL\==[].

trim_rs([],[]):-!.
trim_rs(S,SS):- member(WS, ["\s","\t","\n","\r"," "]), string_concat(Left,WS,S),
  trim_rs(Left,SS),!.
trim_rs(SS,SS).
  
extend_len(Need,S,New):- 
 must_det_ll((
  trim_rs(S,SS),
  atom_length(SS,Spaces),Makeup is Need -Spaces, 
  ( Makeup<1 -> New=SS ;  (make_spaces(Makeup,Pre), (SS==[] -> SSS='' ; SSS=SS), atomics_to_string([SSS,Pre,'\n'],New))))).
   
make_spaces(Spaces,S):-
 wots(S,(write_nbsp,forall(between(2,Spaces,_),write_nbsp),write_nbsp)),!.

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
  nth1(1,L1,E1), atom_length(E1,Spaces1),
  nth1(2,L1,E2), atom_length(E2,Spaces2),
  max_min(Spaces1,Spaces2,Spaces,_Min),
  make_spaces(Spaces,S),
  Needs is N2-N1, make_list(S,Needs,AppendL1), 
  append(L1,AppendL1,NL0),
  maplist(extend_len(Spaces),NL0,NL1))),!.

maybe_exend_len(L1,L2,L2L,L1):- length(L1,N1),length(L2,N2), N2 > N1, !,append(['Swapped'],L2,L2L).

maybe_exend_len(L1,L2,NL1,L2):-
  length(L1,N1),
  length(L2,N2),
  N2>N1, 
  Needs is ((N2-N1)+1),
  wots(S,write('\n')),
  make_list(S,Needs,AppendL1),
  append(L1,AppendL1,NL1),!.


maybe_exend_len(L1,L2,L2,L1):- length(L1,N1), length(L2,N2), N2 > N1, !.


print_side_by_side_lists_1st([],_,[],_):-!.

print_side_by_side_lists_1st(L1,W1,L2,LW):- maybe_exend_len(L1,L2,NL1,NL2),!, 
  print_side_by_side_lists_1st(NL1,W1,NL2,LW).

print_side_by_side_lists_1st([E1,E2|L1],W1,L2,LW):- !,
  wots(S,(write(E2),write('\t '),dash_chars(W1,' ' ))),
  atom_length(S,Pre),
  print_side_by_side_lists(Pre,[E1,E2|L1],W1,L2,LW).

 /*
print_side_by_side_lists_1st([E2|L1],W1,L2,LW):- !,
  wots(S,(write(E2),write('\t '),dash_chars(W1,' ' ))),
  atom_length(S,Pre),
  print_side_by_side_lists(Pre,[E2|L1],W1,L2,LW).
*/
spc_len(E2,SSS):- atom_length(E2,L),wots(SSS,forall(between(1,L,_),write(' '))).

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

desc(A,B):- wots(S1,A),wots(S2,B),nl_if_needed,format('~n'),dash_chars,write(S1),nl_if_needed,write(S2),probably_nl.

write_padding(E1,_W1,E2,LW):- %write_nbsp,
    W1 = LW,
   nl_if_needed,as_str(E1,S1), as_str(E2,S2), 
   write(S1), pre_s2(W1,S2), probably_nl.

pre_s2(_,S2):- atom_contains(S2,'_'), write('    '),write(S2).
pre_s2(_,S2):- atom_contains(S2,'\u00AF'), write('    '),write(S2).
pre_s2(_,S2):- atom_contains(S2,'|'), write('   '),write(S2).
pre_s2(W1,S2):- line_position(user_output,L1), Pad1 is W1 - L1, (dash_chars(Pad1, ' ')),write('  '),write(S2).

as_str(C,S):- plain_var(C),!,sformat(S,' var(~p)',[C]).
as_str([],""):-!.
as_str(S,A):- atom(S),!,atom_string(S,A).
as_str(call(C),S):- !, wots(S,C).
as_str(S,A):- \+ string(S), sformat(A,'~p',[S]),!.
as_str(S,S).

list_print_length(S,Sum):- maplist(print_length,S,LL),!,sumlist(LL,Sum).
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
  INFO = [grid_dim,amass,length,unique_color_count,colors],
%  print_side_by_side(U1,LW,U2),
  print_side_by_side(TitleColor,print_grid(IH,IV,In),NameInU,LW,print_grid(OH,OV,Out),NameOutU),
  print_side_by_side(
     call(describe_feature(In,[call(wqnl(NameInU+fav(PairName)))|INFO])),LW,
    call(describe_feature(Out,[call(wqnl(NameOutU+fav(PairName)))|INFO]))),!.


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
  locally(nb_setval(debug_as_grid,t),
   ((is_group(In),is_group(Out))-> once(showdiff(In,Out));
    ((ignore((is_group(In), desc(wqnl(NameInU+fav(PairName)), debug_indiv(In)))),
      ignore((is_group(Out),desc(wqnl(NameOutU+fav(PairName)), debug_indiv(Out)))))))),!.


uses_space(C):- code_type(C,print).

:- meta_predicate(into_ss_string(+,-)).
into_ss_string(C, X):- var(C),!, must_det_ll(X=ss(1,["var_into_ss_string"])).
into_ss_string(C,_):- plain_var(C),!,throw(var_into_ss_string(C)).
into_ss_string(print_grid(G),SS):- !,into_ss_grid(G,SS).
into_ss_string(print_grid0(G),SS):- !,into_ss_grid(G,SS).
into_ss_string(print_grid(X,Y,G),SS):- !, into_ss_grid(X,Y,G,SS).
into_ss_string(print_grid0(X,Y,G),SS):- !, into_ss_grid(X,Y,G,SS).
into_ss_string(wqs(W),SS):- !,into_ss_call(wqs(W),SS).
into_ss_string(wqs(C,W),SS):- !,into_ss_call(wqs(C,W),SS).
into_ss_string(ss(Len,L),ss(Len,L)):-!.
into_ss_string(uc(W),SS):- !, into_ss_string(uc(yellow,W),SS).
into_ss_string(uc(C,W),SS):- !,into_ss_call(color_print(C,call(underline_print(format("\t~@",[wqs(W)])))),SS).
into_ss_string(call(C),SS):- !,into_ss_call(C,SS),!.

into_ss_string(GG, SS):- is_grid(GG),!,into_ss_grid(GG,SS).
into_ss_string(GG, SS):- is_group(GG),!,into_ss_grid(GG,SS).
into_ss_string(GG, SS):- is_points_list(GG),!,into_ss_grid(GG,SS).
into_ss_string(GG, SS):- is_really_gridoid(GG),!,into_ss_grid(GG,SS).
into_ss_string(LL, SS):- is_list(LL), find_longest_len(LL,Len),!,SS=ss(Len,LL).
into_ss_string(Str,SS):- string(Str), !, atomics_to_string(L,'\n',Str),!,into_ss_string(L,SS).
into_ss_string(GG, SS):- is_object(GG),!,into_ss_grid(GG,SS).
into_ss_string(GG, SS):- is_point(GG),!,into_ss_grid([GG],SS).
into_ss_string(AB, SS):- grid_footer(AB,A,B),!,into_ss_concat(print_grid(A),wqs(B),SS).
into_ss_string(A+B,SS):- into_ss_concat(A,B,SS).
into_ss_string(A-B,SS):- into_ss_concat(A,B,SS).
into_ss_string(NCT,SS):- \+ callable(NCT), into_ss_call(wqs(NCT),SS).
into_ss_string(Goal,SS):-  \+ missing_arity(Goal,0), into_ss_call(Goal,SS).
into_ss_string(IntoG,SS):- into_grid(IntoG,GR),is_grid(GR),!,into_ss_grid(GR,SS).
into_ss_string(Goal,SS):- into_ss_call(Goal,SS).

%as_string(S,SS):- wots(SS,write(S)).
into_ss_grid(G,SS):- into_ss_call(print_grid(G),SS).
into_ss_grid(H,V,G,SS):- into_ss_call(print_grid0(H,V,G),SS).
into_ss_call(C,SS):- wots(Str,catch((C->true;write(failed(C))),E,true)), (nonvar(E)->must_not_error(C);true), into_ss_string(Str,SS).
into_ss_concat(A,B,ss(LenAB,ABL)):- !,into_ss_string(A,ss(LenA,LA)), into_ss_string(B,ss(LenB,LB)),
                                      append(LA,["",LB],ABL), max_min(LenA,LenB,LenAB,_).


find_longest_len(SL,L):- find_longest_len(SL,10,L),!.
find_longest_len([],L,L).
find_longest_len([S|SS],N,L):- print_length(S,N2),max_min(N,N2,NM,_),
  find_longest_len(SS,NM,L).

:- meta_predicate( print_with_pad(0)).
:- export( print_with_pad/1).
print_with_pad(Goal):- 
  (line_position(current_output,O);O=0),!, 
  O1 is O+1,
  wots(S,Goal),
  print_w_pad(O1,S).

print_w_pad(Pad,S):- atomics_to_string(L,'\n',S)-> maplist(print_w_pad0(Pad),L).
print_w_pad0(Pad,S):- nl_if_needed,dash_chars(Pad,' '), write(S).

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
print_equals(Name,Val):- pp(Name=Val).

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

print_grid(Grid):- make_bg_visible(Grid,GGrid),  quietly(print_grid0(_,_,GGrid)),!.

print_grid(Str,Grid):- Grid==[],!, pp(Str).
print_grid(Str,Grid):- Grid==[[]],!, pp(Str).
print_grid(Str,Grid):-  make_bg_visible(Grid,GGrid), ignore((print_grid(_,_,Str,GGrid))),!.
%print_grid(Str,G):- compound(G), maybe_make_bg_visible(G,GG),!,print_grid0(H,V,GG).
%print_grid0(Grid):-  ignore(print_grid0(_,_,Grid)),!.

%print_grid0(Grid):- plain_var(Grid),!, throw(var_print_grid(Grid)).

format_u(TitleColor,Format,Args):- quietlyd( ignore((underline_print(color_print(TitleColor,call(format(Format,Args))))))).

format_footer(TitleColor,W1,W2):- W2==string,!,format_u(TitleColor,'~w',[W1]).
format_footer(TitleColor,W1,W2):- format_u(TitleColor,'~w (~w)',[W1,W2]).

print_grid(_,_,_,_):- is_print_collapsed,!.
print_grid(OH,OV,Name,Grid):- 
 quietly(( make_bg_visible(Grid,Out),
  ignore((print_grid0(OH,OV,Out))),!,nl_if_needed,format('  '),
  ignore((data_type(Out,SS), toUpperC(Name,NameU),
  mesg_color(SS,TitleColor),
  format_footer(TitleColor,NameU,SS))))).

%print_grid(H,V,Grid):- use_row_db, grid_to_tid(Grid,TID),!,print_grid0(H,V,TID).

print_grid(_,_,_):- is_print_collapsed,!.
print_grid(H,V,Grid):-  make_bg_visible(Grid,GGrid), ignore(quietly(print_grid0(H,V,GGrid))).

print_grid0(_,_,_):- is_print_collapsed,!.
print_grid0(H,V,G):- G==[],number(H),number(V),!,make_grid(H,V,GG),!,print_grid0(H,V,GG).
% print_grid0(_H,_V,G):- G==[],!,make_grid(H,V,GG),!,print_grid0(H,V,GG).

print_grid0(H,V,D):- is_map(D),ignore(H = D.h),ignore(V = D.v),
  vm_to_printable(D,R),D\==R,!,print_grid0(H,V,R).

print_grid0(H,V,Grid):- \+ callable(Grid),!,write('not grid: '),
  GG= nc_print_grid(H,V,Grid), pp(GG),!,nop(trace_or_throw(GG)).


print_grid0(H,V,G):- compound(G), G=(GG-PP),is_grid(GG),!,print_grid(H,V,PP,GG).
print_grid0(H,V,SIndvOut):- compound(SIndvOut),SIndvOut=(G-GP), \+ is_nc_point(GP),!, 
  with_glyph_index(G,with_color_index(GP,print_grid0(H,V,G))),!.
print_grid0(H,V,Grid):- is_points_list(Grid), points_to_grid(H,V,Grid,PGrid),!,print_grid0(H,V,PGrid).
print_grid0(H,V,G):- is_empty_grid(G), %trace, arcST,
 wdmsg(is_empty_grid(H,V)),!,
 make_grid(H,V,Empty),
 print_grid0(H,V,Empty),!. 

print_grid0(H,V,Grid):- \+ is_really_gridoid(Grid), into_grid(Grid,G),!,print_grid0(H,V,G).
print_grid0(H,V,Grid):- print_grid0(1,1,H,V,Grid),!.

%print_grid(SH,SV,EH,EV,Grid):- nop(print_grid(SH,SV,EH,EV,Grid)),!.
print_grid(SH,SV,EH,EV,Grid):- quietlyd(print_grid0(SH,SV,EH,EV,Grid)),!.


print_grid0(_SH,_SV,_EH,_EV,Grid):- \+ is_printable_gridoid(Grid), !, writeln(\+ is_printable_gridoid(Grid)).

print_grid0(SH,SV,EH,EV,Grid):-  
  \+ \+ print_grid1(SH,SV,EH,EV,Grid),!,probably_nl.


print_grid1(SH,SV,EH,EV,Grid):- is_object(Grid),
  object_grid(Grid,Points),!,print_grid1(SH,SV,EH,EV,Points).

print_grid1(SH,SV,EH,EV,Grid):-
 nl_if_not_side_by_side,
 %backtrace(10),
 (line_position(current_output,O);O=0),!, O1 is O+1,
 print_grid_pad(O1,SH,SV,EH,EV,Grid), 
 nl_if_not_side_by_side,probably_nl.

nl_if_not_side_by_side:- ignore(( \+ in_side_by_side, probably_nl)).

in_side_by_side:- current_output(Out), \+ stream_property(Out,alias(user_output)).



print_grid_pad(O1,SH,SV,EH,EV,Grid):-
  into_color_name_always(Grid,GridI),
  wots(S,print_grid2(SH,SV,EH,EV,GridI)),
  print_w_pad(O1,S),!.

print_grid2(SH,SV,EH,EV,GridI):- in_pp(ansi),!,ignore(print_grid_ansi(SH,SV,EH,EV,GridI)).
print_grid2(SH,SV,EH,EV,GridI):- in_pp(bfly),!,az_ansi(ignore(print_grid_ansi(SH,SV,EH,EV,GridI))).
print_grid2(SH,SV,EH,EV,GridI):- \+ in_pp(bfly), \+ in_pp(ansi), arc_webui,!, print_grid_html(SH,SV,EH,EV,GridI),nl.
print_grid2(SH,SV,EH,EV,GridI):- !, bfly_html_pre(print_grid_ansi(SH,SV,EH,EV,GridI)).
print_grid2(SH,SV,EH,EV,GridI):- ignore(print_grid_ansi(SH,SV,EH,EV,GridI)).


w_out(S):- toplevel_pp(bfly),!,correct_nbsp(S,SO),our_pengine_output(SO),!.
w_out(S):- is_webui,!,correct_nbsp(S,SO),our_pengine_output(SO),!.
w_out(S):- nl_if_needed,write(S).
%w_out(SO):- pengines:pengine_output('</pre>'),pengines:pengine_output(SO),pengines:pengine_output('<pre class="console">'),!.

:- meta_predicate(g_out(0)).
:- export(g_out/1).
g_out(G):- is_side(_),!,call(G).
g_out(G):- \+ arc_webui,!,nl_if_needed,call(G),probably_nl.
g_out(G):- nb_current(in_g_out,t),!,nl_if_needed,call(G),probably_nl.
g_out(G):- locally(nb_setval(in_g_out,t), gg_out(G)).

gg_out(G):- \+ toplevel_pp(ansi),!,bfly_html_goal(G).
gg_out(G):- call(G).
%gg_out(G):- call(G).
%gg_out(G):- \+ toplevel_pp(bfly),!,gg_out2(G).
%gg_out(G):- bfly_html_goal(gg_out2(G)).

gg_out2(G):-
  wots(S0,call(G)),correct_nbsp(S0,S), !,
  sformat(SO,'<pre style="overflow-x: visible;"><font size2D="+0">~w</font></pre>',[S]),
  w_out(SO).

g_out_style(C,G):- wots(S0,g_out(G)),correct_nbsp(S0,S),
 mbfy(color_print(C,S)).

%mbfy(G):- in_pp(ansi),!,call(G).
%mbfy(G):- in_pp(http),!,call(G).
mbfy(G):- in_pp(bfly),!,bfly_html_goal(G).
mbfy(G):- !,call(G).

correct_nbsp(S,S):-!.
correct_nbsp(S0,S):- replace_in_string([" &nbsp;"="&nbsp;","&nbsp; "="&nbsp;"],S0,S).

%ansi_format_real(Ansi,Format,Args):- arc_webui,!,sformat(S,Format,Args),!,color_print_webui(Ansi,S).
%ansi_format_real(Ansi,Format,Args):- ansicall(Ansi,format(Format,Args)),!.
ansi_format_real(Ansi,Format,Args):- ansi_format(Ansi,Format,Args).


set_html_stream_encoding:- set_stream_encoding(utf8).

as_html_encoded(Goal):- with_enc(utf8,Goal).

with_enc(Enc,Goal):-
 stream_property(current_output,encoding(Was)),
 setup_call_cleanup(current_prolog_flag(encoding,EncWas),
 (( ignore(catch(set_prolog_flag(encoding,Enc),_,true)),
    current_prolog_flag(encoding,EncNew),
     locally(set_prolog_flag(encoding,EncNew),
 setup_call_cleanup(
       set_stream_encoding(Enc),
   Goal,
       set_stream_encoding(Was))))),
       set_prolog_flag(encoding,EncWas)).
      

set_stream_encoding(Text):- 
 %set_prolog_flag(encoding,Text),
 notrace((
 ignore(catch(set_stream(current_output,encoding(Text)),_,true)),
 ignore(catch(set_stream(user_output,encoding(Text)),_,true)),
 ignore(catch(set_stream(current_output,tty(true)),_,true)))),!.

/*
subst_entity(S,SS):- string(S),atom_chars(S,Codes),subst_entity1(Codes,CS),sformat(SS,'~s',[CS]).
subst_entity1([],[]):-!.
subst_entity1(['\\','u',A,B,C,D|Codes],[S|SS]):- into_entity(A,B,C,D,S), subst_entity1(Codes,SS).
subst_entity1([C|Codes],[S|SS]):- subst_entity1(Codes,SS).
into_entity(A,B,C,D,S):- atom_codes('\u0Aaa',C)
*/

color_print_webui(C,G):- mbfy(cpwui(C,G)).


cpwui(_,G):- G==' ',!,write_nbsp.
cpwui(C,G):- mv_peek_color(C,W),cpwui(W,G).
cpwui(C,G):- is_bg_sym_or_var(C),!,cpwui(wbg,G).

cpwui([],G):- !, bformatc(G).
cpwui([C|CC],G):- !,wots(S,cpwui(C,G)),cpwui(CC,S).
%cpwui(_C,G):- \+ arc_webui,!,bformatc(G).

cpwui(underline,G):- !, cpwui(style('text-decoration','underline'),G).
cpwui(bold,G):- !, cpwui(style('font-weight','bold'),G).
cpwui(italic,G):- !, cpwui(style('font-style','italic'),G).

%cpwui(C,G):- \+ arc_webui,!,color_print(C,G).
cpwui(bg(C),G):- !, cpwui(style('background-color',C),G).
cpwui(hbg(C),G):- !, cpwui([bg(C),style('filter','brightness(150%)')],G).
cpwui(hfg(C),G):- !, cpwui([C,style('brightness','200%')],G).
cpwui(style(C),G):- !, format('<font style="~w">~@</font>',[C,bformatc_or_at(G)]).
cpwui(style(N,V),G):- !, format('<font style="~w: ~w;">~@</font>',[N,V,bformatc_or_at(G)]).
cpwui(C,G):- get_black(Black),C==Black,!, cpwui(style('opacity: 0.5;'),G).
cpwui(C,G):- C==wbg,!,cpwui([black,black],G).

cpwui(fg(C),G):- !, format('<font color="~w" style="font-weight: bold;">~@</font>',[C,bformatc_or_at(G)]),!.
cpwui(color(C),G):- !, format('<font color="~w">~@</font>',[C,bformatc_or_at(G)]).
cpwui(C,G):- integer(C),arc_acolor(C,CC),CC\==C,!,cpwui(CC,G).
cpwui(C,G):- is_html_color(C),!, format('<font color="~w">~@</font>',[C,bformatc_or_at(G)]).
%cpwui((C),G):- !, format('<font color="~w">~@</font>',[C,bformatc(G)]).
cpwui(C,G):- format('<font style="~w">~@</font>',[C,bformatc_or_at(G)]),!.

bformatc_or_at(C):- wots(S,bformatc(C)), ( (S=="";(fail,atom_contains(S,"><"))) -> write('@') ; write(S)).

is_html_color(A):- \+ atom(A),!,fail.
is_html_color(teal).
is_html_color(C):- is_real_color(C),!.
is_html_color(A):- atom_concat('#',_,A),!.
/*
cpwui(fg(C),G):- !, cpwui(color(C),G).
cpwui(color(C),G):- !, cpwui(style('color',C),G)
% style="font-weight: bold;.
cpwui(color(C),G):- format('<font color="~w">~@</font>',[C,bformatc(G)]),!.
cpwui(C,G):- format('<span style="~w">~@</span>',[C,bformatc(G)]),!.
*/


bformatc(G):- var(G),!,bformatc(vaR(G)).
bformatc(call(G)):- !, wots(S,call(G)), bformatc(S).
bformatc(G):- string(G),!,bformats(G).
bformatc(G):- atom(G),!,bformats(G).
bformatc(G):- is_list(G), notrace(catch(text_to_string(G,S),_,fail)),G\==S,!,bformatc(S).
bformatc(G):- wots(S,write(G)), bformats(S).

bformats(S):- atom_contains(S,'<'),!,write(S).
bformats(S):- bformatc1(S).

bformatc1(S):- \+ arc_webui,!,write(S).
bformatc1(S):- write(S),!.
bformatc1(S):- atom_codes(S,Cs), maplist(map_html_entities_mono,Cs,CsO),atomic_list_concat(CsO,W),!,bformatw(W).

%map_html_entities_mono(I,O):- atom_codes(O,I),!.
map_html_entities_mono(I,O):- map_html_entities(I,O).

map_html_entities(Code,S):- Code>160, !, sformat(S, '&#~w;',[Code]).
map_html_entities(Code,S):- Code<33, !, sformat(S, '&#~w;',[Code]).
/*
map_html_entities(Code,S):- Code == 124,!,sformat(S, '&#~w;',[Code]).
map_html_entities(Code,S):- Code>255, !, sformat(S, '&#~w;',[Code]).
map_html_entities(62,'&gt;'). map_html_entities(60,'&lt;'). map_html_entities(38,'&amp;'). map_html_entities(32,'&nbsp;').
*/
map_html_entities(Code,S):- name(S,[Code]),!.

bformatw(G):- g_out(bformat(G)).

with_style(S,G):-
 (arc_webui -> 
   setup_call_cleanup(format('<span style="~w">',[S]),G,write('</span>')) 
    ; call(G)).

html_echo(G)--> [G].

mforeach(Generator, Rule) -->
    foreach(Generator, Rule, []).

:- use_module(library(dcg/high_order),[foreach // 3]).

print_grid_html:- arc_grid(Grid),print_grid_html(Grid).
print_grid_html(Grid):-print_grid_html(_SH,_SV,_EH,_EV,Grid),!.
print_grid_html(Name,Grid):-print_grid(_OH,_OV,Name,Grid),!.

print_grid_html(SH,SV,EH,EV,Grid):- print_grid_http(SH,SV,EH,EV,Grid),!.

print_grid_http(SH,SV,EH,EV,Grid):- bg_sym(BGC),
 arc_html_format(`<code>tbody td:nth-of-type(odd){ background:rgba(255,255,136,0.5); }</code>`),
(plain_var(EH) ->grid_size(Grid,EH,_) ; true),ignore(SH=1),
  (plain_var(EV) ->grid_size(Grid,_,EV) ; true),ignore(SV=1),
   output_html(table([ class([table, 'table-striped']), 
             style('width:auto; margin-left:2em') ],
           [ tr(th(colspan(EH), ['Table for ', 'This'])),
             \ mforeach(between(SV,EV,V),
                      html(tr([ \ mforeach((between(SH,EH,H),once(hv_cg_value(Grid,CG,H,V);CG=BGC), 
                         wots(Cell,(print_g1(cpwui,CG)))),
                                     html(td([class('mc-10'),style('text-align:center; width:11px;')], html_echo(Cell) ))) ])))
           ])),!.


print_grid_html_old(SH,SV,EH,EV,Grid):-
 % CSS = 'line-height: .5; font-stretch: ultra-extended;',
 %CSS = 'line-height: 1.2; font-stretch: ultra-extended; font-size2D:288px background-color: reset;',
 CSS = 'white-space: pre;',
 g_out(must_det_ll(( 
  nl_if_needed, 
  ((plain_var(EH) ; plain_var(EV))->grid_size(Grid,EH,EV);true),
  Width is EH-SH, 
  (Width==0 -> DBW = 1 ; DBW is Width+1),
  once((with_style(CSS,dash_border_no_nl(DBW)))),
  bg_sym(BGC),
  forall(between(SV,EV,V),
   with_style(CSS,((nl_if_needed,format('['),(format('<span style="~w">',[CSS])),
     forall(between(SH,EH,H),
     ignore((((hv_cg_value(Grid,CG,H,V);/*grid_cpoint(Grid,CG-_,H,V);*/CG=BGC)->
        (once(print_gw1(CG))))))),write('&nbsp;]'),(write('</span>')))))),
  %print_g(H,V,C,LoH,LoV,HiH,HiV)
  nl_if_needed,!,
  once(with_style(CSS,dash_uborder_no_nl(DBW)))))),!.

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
 

print_grid_ansi(SH,SV,EH,EV,GridII):- make_bg_visible(GridII,GridI),
 must_det_ll((
  nl_if_needed, 
  %maybe_grid_numbervars(GridI,Grid),
  GridI=Grid,
  ((plain_var(EH) ; plain_var(EV))->grid_size(Grid,EH,EV);true),
  Width is EH-SH, 
  (Width==0 -> DBW = 1 ; DBW is Width+1),
  once((dash_border_no_nl(DBW))),
  bg_sym(BGC),
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
     (( \+ ground(GridI));sub_var(wbg,GridI);sub_var(bg,GridI);sub_var(wfg,GridI);sub_var(fg,GridI)),
      grid_colors(GridI,CGrid),
      (nb_current(print_sbs,left)-> (nl,nl, write(left), write(=)) ; true),
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
%print_grid(Grid):- is_grid(Grid),!, maplist(print_rows,Grid),nl.
%print_rows(List):- maplist(print_g,List),nl.
%block_colors([(black),(blue),(red),(green),(yellow),'#c0c0c0',(magenta),'#ff8c00',(cyan),'#8b4513']).
%block_colors([(black),(blue),(red),(green),(yellow),Silver,('#966cb8'),'#ff8c00',(cyan),'#8b4513']):- silver(Silver),!.
block_colors([('#3a5a3a'),(blue),(red),(green),(yellow),Silver,(magenta),'#ff8c00',(cyan),'#8b4513','#2a2a2a','#f47c7c',FG]):- fg_cut(FG), silver(Silver),!.
%block_colors([(black),(blue),(red),(green),(yellow),Silver,(magenta),'#ff8c00',(cyan),'#8b4513','#2a2a2a', 9379b4 '#3a5a3a']):- silver(Silver),!.
named_colors([(black),(blue),(red),(green),(yellow),(silver),(purple),(orange),(cyan),(brown),wbg,fg,FG]):- fg_cut(FG).
named_colors([ (lack),(blue),(red),(green),(yellow),(Silver),(purple),(orange),(cyan),(brown),bg,wfg]):- silver(Silver).
named_colors([(lack),(blue),(red),(green),(yellow),(silver),(magenta),(orange),(cyan),(brown)]).
named_colors([(lack),(blue),(red),(green),(yellow),(grey),(pink),(orange),(teal),(maroon)]).

% '#1077f1'
fg_cut('#b399d4').
% silver(rgb(123,123,123)).
silver('#9a9a9a').
silver('#7b7b7b').
silver('#c0c0c0').


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

%color_print(C,W):- '$current_typein_module'(M), muarc_mod(MM), MM\==M, !,'$set_typein_module'(MM), module(MM),color_print(C,W).
color_print(C,W):- arc_webui,!,color_print_webui(C,W).
color_print(C,W):- mv_peek_color(C,V),!,color_print(V,W).
%color_print(C,W):- C == black,!, color_print(white,W).
color_print(C,W):- compound(W),compound_name_arity(W,call,_),!,(wots(S1,call(call,W))->color_print(C,S1);color_print(C,failed(W))).

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
grid_color_code(C,C):- var(C),!.
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
%fg_dot(_):- luser_getval(no_rdot,true),luser_setval(no_rdot,false)-> break , fail.
fg_dot(C):- luser_getval(alt_grid_dot,C),C\==[],!.
fg_dot(64).
%fg_dot(174).
cant_be_dot(183).
grid_dot(C):- luser_getval(alt_grid_dot,C),C\==[],!.
grid_dot(169).

%print_g(H,V,C0,_,_,_,_):- cant_be_color(C0),cant_be_color(C0,C),!,  ansi_format_arc([bold,fg('#ff8c00')],'~@',[(write('c'),user:print_g1(H,V,C))]).
%print_g(H,V,C0,_,_,_,_):- plain_var(C0),print_g1(H,V,C-'?'),!.
%print_g(H,V,C,_,_,_,_):- write_nbsp, print_g1(P2,H,V,C),!.

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

print_gw1(N):- print_gw1(color_print_ele,N),!.

print_gw1(P2,N):-  
 wots(S,(((get_bgc(BG),is_color(BG), once(( ( \+ is_black(BG))-> call(P2,BG,'.');write_nbsp);write(',')));write_nbsp),!,
  (print_g1(P2,N);write('?')))),!, gws(S).

gws(S):- write(S),!.
%gws(S):- atom_length(S,L),(L=28->(write(L),atom_codes(S,Codes),arc_assert(ac(S)));write(S)).
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

print_g1(CG):- print_g1(color_print_ele,CG).
print_g1(P2,C):- C == ((+) - wbg),!,call(P2,wbg,(+)).
print_g1(P2,C):- mv_peek_color(C,V),C\==V,!,print_g1(P2,V).
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
into_color_glyph(Obj,C,G):- is_object(Obj),color(Obj,C),object_glyph(Obj,G),!.
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
i_glyph0(N,Glyph):- atom(N),name(N,[_111,95,Code|_])->name(Glyph,[Code]),!.
i_glyph0(N,Glyph):- atom(N),name(N,[Code])->name(Glyph,[Code]),!.
i_glyph0(Code,Glyph):- integer(Code), Code> 255, name(Glyph,[Code]).
i_glyph0(N,Glyph):- integer(N),quietly((i_sym(N,Code),name(Glyph,[Code]))).
i_glyph0(N,Glyph):- plain_var(N),format(chars(Codes),'~p',[N]),last(Codes,Glyph).
i_glyph0(N,Glyph):- number(N), N>10, integer(N),N3 is N div 3, i_glyph0(N3,Glyph).
%i_glyph(N,Glyph):- atom(N),atom_chars(N,Chars),last(Chars,LGlyph),upcase_atom(LGlyph,Glyph).
                                                                            
i_sym(N2,Code):- integer(N2),!, N is N2+160, change_code(N,NN), iss:i_syms(Codes),nth0(NN,Codes,Code),!.
i_sym(N2,Code):- atom(N2),name(N2,[C|_]),!,i_sym(C,Code).
i_sym(N,Code):- plain_var(N), Code = 63.
%change_code(N,M):- M is N * 100,!.
%change_code(N,M):- N>10, M is (N * 10 ),!.
change_code(N,M):- M is N.


print_g1(P2,_,_, E):- print_g1(P2,E),!. 
%print_g1(_,_,C):- trace, write(C).

code_not_bfly(Code):- between(170,inf,Code).

save_codes(Max):- 
 %stream_property(File,file_no(1)),
 locally(set_prolog_flag(encoding,iso_latin_1),
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
  sort(CCC,CCCO),
  assertz(iss:i_syms(CCCO))))).

is_single_char_ifiable_quiet(Code):- with_output_to(string(S),put(Code)),sformat(SS,'~q',[S]),!, atom_length(SS,3),!.

is_single_char_ifiable(Code):- with_output_to(string(S),put(Code)),sformat(SS,'~q',[S]),!,is_single_char_ifiable(Code,SS).
is_single_char_ifiable(_,SS):- atom_length(SS,3),!.
is_single_char_ifiable(C,SS):- writeln(user_error,C=SS),!,fail.

test_is_single_char_ifiable:-
 retract(iss:i_syms(CCC)), 
 include(is_single_char_ifiable,CCC,CCCO),
 assert(iss:i_syms(CCCO)).

is_html_ifiable(Code):- sformat(S,'~@',[as_html_encoded(put_code(Code))]), atom_length(S,1),!.
is_html_ifiable(Code):- format('~@',[as_html_encoded(put_code(Code))]),!,fail.

save_codes:- save_codes(42600).

check_dot_spacing(CCC):- 
 locally(set_prolog_flag(encoding,utf8),format(user_error,'~@', 
  [ignore((color_print(red, call(format('~n~w = |~s|',[CCC,[CCC,32,CCC,32,CCC,32,CCC,32,CCC,32]])))))])).

check_dot_spacing:- iss:i_syms(CCC),maplist(check_dot_spacing,CCC),!.

:- retractall(iss:i_syms(_)).
:- ignore(save_codes).

/*
get_glyph(Point,Glyph):-  
  get_grid_num(Point,N),i_glyph(N,Glyph).
*/
:- include(kaggle_arc_footer).
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


