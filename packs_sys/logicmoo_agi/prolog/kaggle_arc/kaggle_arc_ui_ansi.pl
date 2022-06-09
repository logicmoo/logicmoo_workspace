/*
  this is part of (H)MUARC

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/
:- if(current_module(trill)).
:- set_prolog_flag_until_eof(trill_term_expansion,false).
:- endif.

print_collapsed(Size,G):-  !,
 locally(b_setval(print_collapsed,true), print_collapsed0(Size,G)).

print_collapsed0(Size,G):- Size<10, !, call(G). 
% print_collapsed(Size,G):-  call(G). 
print_collapsed0(Size,G):- Size>20, !, wots(_S,G).
print_collapsed0(_,G):- wots(S,G),write(S).

tersify(I,O):- quietlyd((tersify2(I,M),tersify3(M,O))).

terseA(_,[],[]):- !.
terseA(I,[A|L],[B|LL]):-terseA(I,A,B),terseA(I,L,LL).
terseA(I,dif(A,B),B):-A==I,!.
terseA(I,dif(B,A),B):-A==I,!.
terseA(_,put_attr(_,B,A),A):- B==ci,!.
terseA(_,put_attr(_,B,A),B=A):-!.
terseA(_,A,A):-!.




tersify0(I,av(I,Others)):- attvar(I),copy_term(I,C,Attrs),C=I,terseA(I,Attrs,Others),!.
tersify0(I,I):- var(I),!.


tersifyC(D):- is_dict(D),!.
tersifyC(av(_,_)).
tersifyC(objFn(_)).

tersify1(av(_,Blue), -(Blue)):-!.
tersify1(I,O):- compound(I), tersifyC(I),!,I=O.
tersify1(I,gridFn(O)):- is_grid(I),tersifyG(I,O),!.
tersify1(gridFn(I),gridFn(O)):-tersifyG(I,O).
tersify1(I,gridFn(S)):- is_grid(I), into_gridnameA(I,O),!,sformat(S,'~w',[O]).
tersify1(I,objFn(S)):- is_object(I), o2ansi(I,O),!,sformat(S,'~w',[O]).

tersifyG(I,O):- tersifyL(I,O),numbervars(O,1,_,[attvar(bind),singletons(false)]).
tersifyL(I,O):- is_list(I), maplist(tersifyL,I,O).
tersifyL(I,O):- tersify0(I,O).
tersifyL(I,O):- tersify1(I,O).
tersifyL(I,I).

tersify2(I,O):- compound(I),tersify1(I,O),!.
tersify2(I,O):- tersify0(I,O),!.
tersify2(I,O):- is_list(I), !, maplist(tersify2,I,O).
tersify2(I,O):- compound(I), !, compound_name_arguments(I,F,IA), maplist(tersify,IA,OA), compound_name_arguments(O,F,OA).
tersify2(I,I).

tersify3(I,O):- compound(I),tersify1(I,O),!.
tersify3(I,O):- tersify0(I,O),!.
tersify3([H|I],O):- is_list(I), !, with_output_to(string(S),display(I)),!, ((atom_length(S,N), N>170) -> 
  (length(I,LL),tersify(H,HH),tersify_len(HH,LL,s_l(N))=O); I=O).
tersify3(I,O):- compound(I), !, compound_name_arguments(I,F,IA), maplist(tersify,IA,OA), compound_name_arguments(O,F,OA).
tersify3(I,I).

ptt(P):- \+ \+ ((tersify(P,Q),!,pt(Q))).

pt(P):- var(P),!,pt(var(P)).
pt(_):- is_print_collapsed,!.
pt(P):- atomic(P),atom_contains(P,'~'),!,format(P).
pt(P):- format('~N'), quietlyd(print_tree_nl(P)),!.
pt(Color,P):- quietlyd((format('~N'), wots(S,pt(P)),!,color_print(Color,S))).


wqs(X):- plain_var(X), !, wqs(plain_var(X)). wqs(nl):- !, nl. wqs(''):-!. wqs([]):-!.
%wqs([H1,H2|T]):- string(H1),string(H2),!, write(H1),write(' '), wqs([H2|T]).
%wqs([H1|T]):- string(H1),!, write(H1), wqs(T).
wqs([skip(_)|T]):- !,wqs(T).
%wqs([H|T]):- compound(H),!, writeq(H), wqs(T).
wqs([H|T]):- !, wqs(H), wqs(T).
wqs(format(C,N)):- !, format(C,N).
wqs(writef(C,N)):- !, writef(C,N).
wqs(call(C)):- !, call(C).
wqs(pt(C)):- !, pt(C).
wqs(q(C)):- !, write(' '), writeq(C).
wqs(cc(C,N)):- !, write(' cc('),color_print(C,C),write(','), writeq(N), write(')').
wqs(color_print(C,X)):- !, write(' '), color_print(C,X).

wqs(X):- \+ compound(X),!, write(' '), write(X).
wqs(X):- write(' '), writeq(X).

wqln(X):- wqnl(X).
wqnl(X):- is_list(X),!,format('~N'),wqs(X),format('~N').
wqnl(X):- format('~N~q~N',[X]).
dash_char:- dash_char(40),!.
dash_char(H):- integer(H), dash_border(H).
dash_char(S):- format('~N'),dash_char(60,S),format('~N').
dash_char(H,_):- H < 1,!.
dash_char(H,C):-forall(between(0,H,_),write(C)).
dash_border_no_nl(Width):- WidthM1 is Width-1, write(' _'),dash_char(WidthM1,'__').
dash_uborder_no_nl(Width):- WidthM1 is Width-1, write(' ¯'),dash_char(WidthM1,'¯¯').
dash_uborder_no_nl(Width):- WidthM1 is Width-1, write(' _'),dash_char(WidthM1,'__').
dash_border(Width):- !, dash_border_no_nl(Width),nl,!.
dash_uborder(Width):- format('~N'), WidthM1 is Width-1, write(' ¯'),dash_char(WidthM1,'¯¯'),nl.
%dash_uborder(Width):- format('~N'), WidthM1 is Width-1, write(' _'),dash_char(WidthM1,'__'),nl.

functor_color(pass,green).
functor_color(fail,red).
functor_color(warn,yellow).

arcdbg(G):- compound(G), compound_name_arity(G,F,_),functor_color(F,C),wots(S,print(G)),color_print(C,S),!,format('~N').
arcdbg(G):- wdmsg(G).

user:portray(Grid):- setup_call_cleanup(arc_portray(Grid),true,true),!.


via_print_grid(G):- is_grid(G).
via_print_grid(G):- is_object(G).
via_print_grid(G):- maplist(is_object,G).
via_print_grid(G):- is_points_list(G),ground(G).
%via_print_grid(G):- is_gridoid(G).

arc_portray(G):- \+ \+ catch((( tracing,
    is_list(G),G\==[], tersify(G,T),write(T))),_,false).

arc_portray(G):- \+ \+ catch(((via_print_grid(G),
    tracing,via_print_grid(G),!,
    grid_size(G,H,V),!,H>0,V>0, 
     wots(S,print_grid(H,V,G)),write(S))),_,false).

arc_portray(G):- \+ \+ catch(((
   \+ tracing,via_print_grid(G),!,
    grid_size(G,H,V),!,H>0,V>0, 
     wots(S,print_grid(H,V,G)),write(S))),_,false).

%user:portray(Grid):- ((\+ tracing, is_group(Grid),print_grid(Grid))).
%user:portray(Grid):- quietlyd((is_object(Grid),print_grid(Grid))).

red_noise:- format('~N'),
  color_print(red,'--------------------------------------------------------------'),nl,
  color_print(red,'--------------------------------------------------------------'),nl,
  color_print(red,'--------------------------------------------------------------'),nl.

print_side_by_side(C1,C2):- is_gridoid(C1), is_gridoid(C2),grid_size(C1,H1,V1),grid_size(C2,H2,V2),    
    (V1>V2 -> print_side_by_side(C2,(H1 * 2 + 12),C1); print_side_by_side(C1,(H2 * 2 + 12),C2)),!.

print_side_by_side(C1,C2):- print_side_by_side(C1,20,C2),!.
print_side_by_side(C1,W0,C2):- LW is W0,
  into_ss_string(C1,ss(W1,L1)),
  into_ss_string(C2,ss(_,L2)),!,
  print_side_by_side_lists(L1,W1,L2,LW).
  
print_side_by_side_lists([E1|L1],W1,[E2|L2],W2):-!,
  write_padding(E1,W1,E2,W2), 
  print_side_by_side_lists(L1,W1,L2,W2).
  
print_side_by_side_lists([],W1,[],W2):- !, nop(write_padding([],W1,[],W2)),!.
  
print_side_by_side_lists([E1|L1],W1,[],W2):- !,
  write_padding(E1,W1,[],W2),
  print_side_by_side_lists(L1,W1,[],W2).
  
print_side_by_side_lists([],W1,[E2|L2],W2):-
  write_padding([],W1,E2,W2),
  print_side_by_side_lists([],W1,L2,W2).

desc(A,B):- wots(S1,A),wots(S2,B),format('~N~n'),dash_char,write(S1),format('~N'),write(S2),format('~N').

write_padding(E1,_W1,E2,LW):- %write(' '),
    W1 = LW,
   format('~N'),as_str(E1,S1), as_str(E2,S2), 
   write(S1), pre_s2(W1,S2), format('~N').

pre_s2(_,S2):- atom_contains(S2,'_'), write('    '),write(S2).
pre_s2(_,S2):- atom_contains(S2,'¯'), write('    '),write(S2).
pre_s2(_,S2):- atom_contains(S2,'|'), write('   '),write(S2).
pre_s2(W1,S2):- line_position(user_output,L1), Pad1 is W1 - L1, (dash_char(Pad1, ' ')),write('  '),write(S2).

as_str(Var,S):- plain_var(Var),!,sformat(S,' var(~p)',[Var]).
as_str([],""):-!.
as_str(S,A):- atom(S),!,atom_string(S,A).
as_str(call(C),S):- !, wots(S,C).
as_str(S,A):- \+ string(S), sformat(A,'~p',[S]),!.
as_str(S,S).

print_length(S,L):- as_str(S,A),atom_codes(A,C), include(uses_space,C,SS),length(SS,L).


append_term_safe(Type,PairName,NameIn):- append_term(Type,PairName,NameIn),!.
append_term_safe(Type,PairName,append_term(Type,PairName)).

show_pair_i(_,_,_,_, _,_,_,_):- is_print_collapsed,!.
show_pair_i(IH,IV,OH,OV,Type,PairName,In,Out):-
  show_pair_no_i(IH,IV,OH,OV,Type,PairName,In,Out),
  append_term_safe(Type,PairName+in,NameIn),
  append_term_safe(Type,PairName+out,NameOut),
  ignore(show_pair_I_info(NameIn,NameOut,In,Out)),!.

show_pair(_,_,_,_, _,_,_,_):- is_print_collapsed,!.

show_pair(IH,IV,OH,OV,Type,PairName,In,Out):-
  ignore(IH=1),
  LW is (IH * 2 + 12),
  append_term_safe(Type,PairName+in,NameIn),
  append_term_safe(Type,PairName+out,NameOut),
  wots(U1, (print_grid(IH,IV,In),writeln(NameIn))),
  wots(U2, (print_grid(OH,OV,Out),writeln(NameOut))),
  print_side_by_side(U1,LW,U2),!,
  INFO = [grid_dim,mass,colors_count_size,colors],
  print_side_by_side(
     describe_feature(In,[call(writeln('IN'))|INFO]),LW,
    describe_feature(Out,[call(writeln('OUT'))|INFO])),!.

show_pair_no_i(_,_,_,_, _,_,_,_):- is_print_collapsed,!.
show_pair_no_i(IH,IV,OH,OV,Type,PairName,In,Out):-
  ignore(IH=1),
  LW is (IH * 2 + 12),
  append_term_safe(Type,PairName+in,NameIn),
  append_term_safe(Type,PairName+out,NameOut),
  wots(U1, print_grid(IH,IV,NameIn,In)),
  wots(U2, print_grid(OH,OV,NameOut,Out)),
  print_side_by_side(U1,LW,U2),!.


show_pair_I_info(NameIn,NameOut,In,Out):- 
  ((is_group(In),is_group(Out))-> once(showdiff(In,Out));
    ignore((is_group(In),desc(wqnl(fav(NameIn)), debug_indiv(In)))),
    ignore((is_group(Out),desc(wqnl(fav(NameOut)), debug_indiv(Out))))),!.

uses_space(C):- code_type(C,print).

into_ss_string(Var,_):- plain_var(Var),!,throw(var_into_ss_string(Var)).
into_ss_string(G,SS):- is_grid(G),!,wots(S,print_grid(G)),!,into_ss_string(S,SS).
into_ss_string(G,SS):- is_object(G),!,wots(S,print_grid(G)),!,into_ss_string(S,SS).
into_ss_string(G,SS):- is_points_list(G),!,wots(S,print_grid(G)),!,into_ss_string(S,SS).
into_ss_string(G,SS):- is_group(G),!,wots(S,print_grid(G)),!,into_ss_string(S,SS).
into_ss_string(ss(Len,L),ss(Len,L)):-!.
into_ss_string(L,ss(Len,L)):- is_list(L), find_longest_len(L,Len),!.
into_ss_string(S,SS):- string(S), atomics_to_string(L,'\n',S),!,into_ss_string(L,SS).
into_ss_string(C,SS):- wots(S,catch(C,E,true)), 
  (((nonvar_or_ci(E),notrace,break,rtrace(C))->throw(E);true)), into_ss_string(S,SS).

find_longest_len(SL,L):- find_longest_len(SL,10,L),!.
find_longest_len([],L,L).
find_longest_len([S|SS],N,L):- print_length(S,N2),max_min(N,N2,NM,_),
  find_longest_len(SS,NM,L).

print_w_pad(Pad,S):- atomics_to_string(L,'\n',S)-> maplist(print_w_pad0(Pad),L).
print_w_pad0(Pad,S):- format('~N'),dash_char(Pad,' '), write(S).

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
  dash_char(H,"-"),
  forall(member(E,[G|L]),(print_grid(E),dash_char(H,"-"),nl)).
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
fix_grid_pg(SIndvOut,PGP,G,GP):- compound(SIndvOut), SIndvOut=(GG,PGG),!,listify(PGG,PGL),listify(GG,G),!,append(PGL,PGP,GP).
fix_grid_pg(SIndvOut,PGP,SIndvOut,[PGP]):- \+ is_list(PGP),!.
fix_grid_pg(SIndvOut,PGP,[SIndvOut],PGP):- \+ is_list(SIndvOut),!.


with_glyph_index(Print,Goal):- Print==[],!,with_glyph_index([e],Goal).
with_glyph_index(Print,Goal):- listify(Print,PrintL),
  locally(b_setval(glyph_index,PrintL),Goal).

with_color_index(Print,Goal):- Print==[],!,with_color_index([e],Goal).
with_color_index(Print,Goal):- listify(Print,PrintL),
  locally(b_setval(color_index,PrintL),Goal).
use_row_db :- fail.
is_print_collapsed:- nb_current(print_collapsed,true).

print_grid(_):- is_print_collapsed,!.
print_grid(Grid):- use_row_db, is_grid(Grid),!, grid_to_id(Grid,ID),print_grid(ID).

print_grid(Grid):-  quietlyd(print_grid0(_,_,Grid)),!.
%print_grid0(Grid):- plain_var(Grid),!, throw(var_print_grid(Grid)).

print_grid(OH,OV,Name,Out):- print_grid(OH,OV,Out),writeln(Name),!.
print_grid(H,V,Grid):- use_row_db, grid_to_id(Grid,ID),!,print_grid0(H,V,ID).
print_grid(H,V,Grid):- quietlyd(print_grid0(H,V,Grid)).

print_grid0(_,_,_):- is_print_collapsed,!.
print_grid0(H,V,G):- G==[],nonvar(H),make_grid(H,V,GG),!,print_grid0(H,V,GG).
print_grid0(H,V,Grid):- \+ callable(Grid),!,write('not grid: '),
  GG= nc_print_grid(H,V,Grid), pt(GG),!,trace_or_throw(GG).

print_grid0(H,V,SIndvOut):- compound(SIndvOut),SIndvOut=(G-GP), \+ is_nc_point(GP),!, with_glyph_index(G,with_color_index(GP,print_grid0(H,V,G))),!.
print_grid0(H,V,Grid):- is_points_list(Grid), points_to_grid(H,V,Grid,PGrid),!,print_grid0(H,V,PGrid).
print_grid0(H,V,G):- is_empty_grid(G), %trace, dumpST,
 wdmsg(is_empty_grid(H,V)),!,
 make_grid(H,V,Empty),
 print_grid0(H,V,Empty),!. 

print_grid0(H,V,Grid):- \+ is_gridoid(Grid), into_grid(Grid,G),!,print_grid0(H,V,G).
print_grid0(H,V,Grid):- print_grid0(1,1,H,V,Grid),!.

print_grid0(SH,SV,EH,EV,Grid):- print_grid0(SH,SV,SH,SV,EH,EV,EH,EV,Grid).
%print_grid(SH,SV,LoH,LoV,HiH,HiV,EH,EV,Grid):- nop(print_grid(SH,SV,LoH,LoV,HiH,HiV,EH,EV,Grid)),!.
print_grid(SH,SV,LoH,LoV,HiH,HiV,EH,EV,Grid):- print_grid0(true,SH,SV,LoH,LoV,HiH,HiV,EH,EV,Grid),!.

print_grid0(SH,SV,LoH,LoV,HiH,HiV,EH,EV,Grid):-
 line_position(current_output,O),
 O1 is O+1,
 quietlyd((wots(S, \+ \+ print_grid0(true,SH,SV,LoH,LoV,HiH,HiV,EH,EV,Grid)),
 print_w_pad(O1,S))),format('~N').

quietlyd(G):-call(G),!.
/*
print_grid0(Bordered,SH,SV,LoH,LoV,HiH,HiV,EH,EV,Grid):- 
  is_grid(Grid), Grid=[[AShape|_]|_], nonvar_or_ci(AShape),(AShape=A-Shape),plain_var(A),nonvar_or_ci(Shape),
   bagof(A-Shape,(sub_term(AShape,Grid),nonvar_or_ci(AShape),AShape= (A-Shape),plain_var(A)),Colors),list_to_set(Colors,AShapes),
   block_colors(BC), create_grid_key(BC,AShapes,KeyS),
   forall(member(Key,KeyS),wqnl(Key)),
   term_variables(Grid,Vars),
   create_grid_key2(BC,Vars,KeyS2),
   forall(member(Key,KeyS2),wqnl(Key)),
   print_grid1(Bordered,SH,SV,LoH,LoV,HiH,HiV,EH,EV,Grid),
   writeq(Grid).
*/

print_grid0(Bordered,SH,SV,LoH,LoV,HiH,HiV,EH,EV,Grid):- is_object(Grid),!,print_grid0(Bordered,SH,SV,LoH,LoV,HiH,HiV,EH,EV,[Grid]).
print_grid0(_Bordered,SH,SV,_LoH,_LoV,_HiH,_HiV,EH,EV,GridI):-
  write('\n'), 
  maybe_grid_numbervars(GridI,Grid),
  ((plain_var(EH) ; plain_var(EV))->grid_size(Grid,EH,EV);true),
  Width is EH-SH, 
  once((user:dash_border_no_nl(Width+1))),
  bg_sym(BGC),
  forall(between(SV,EV,V),
   ((format('~N|'),
     forall(between(SH,EH,H), 
     (hv_value_or(Grid,C,H,V,BGC)->
        (once(print_gw1(C))))),write(' |')))),
  %print_g(H,V,C,LoH,LoV,HiH,HiV)
  format('~N'),!,
  once((user:dash_uborder_no_nl(Width+1))).

%print_grid(Grid):- is_grid(Grid),!, maplist(print_rows,Grid),nl.
%print_rows(List):- maplist(print_g,List),nl.
%block_colors([(black),(blue),(red),(green),(yellow),'#c0c0c0',(magenta),'#ff8c00',(cyan),'#8b4513']).
%block_colors([(black),(blue),(red),(green),(yellow),Silver,('#966cb8'),'#ff8c00',(cyan),'#8b4513']):- silver(Silver),!.
block_colors([(black),(blue),(red),(green),(yellow),Silver,(magenta),'#ff8c00',(cyan),'#8b4513']):- silver(Silver),!.
named_colors([(black),(blue),(red),(green),(yellow),(silver),(purple),(orange),(cyan),(brown)]).
named_colors([(black),(blue),(red),(green),(yellow),(silver),(magenta),(orange),(cyan),(brown)]).
named_colors([(black),(blue),(red),(green),(yellow),(grey),(pink),(orange),(cyan),(maroon)]).

% silver(rgb(123,123,123)).
silver('#7b7b7b').
silver('#c0c0c0').
silver('#9a9a9a').

unnegate_color(C,Neg):- number(C),C<0,C is -Neg.
unnegate_color(-C,C):- !.
ansi_color(C,Color):- unnegate_color(C,Neg),ansi_color(Neg,Color).
ansi_color(C,Color):- attvar(C),get_attr(C,ci,fg(N)),ansi_color(N,Color),!.
ansi_color(C,fg(Color)):- attvar(C),cant_be_color(C,E),!,ansi_color(-E,Color).
ansi_color(C,fg(Color)):- attvar(C),get_attr(C,ci,bg),get_bgc(BG),!,ansi_color(BG,Color),!.
ansi_color(C,blink):- var(C),!,trace_or_throw(var(ansi_color(C))).
ansi_color(C,fg(Color)):- atom_concat('#',_,C),!,Color=C.
ansi_color(C,fg(Color)):- integer(C),block_colors(L),length(L,UpTo),between(0,UpTo,C),!,nth0(C,L,Color).
ansi_color(C,Color):- color_int(C,I)->C\==I,!,ansi_color(I,Color).
ansi_color(_,[bold,underline]).

on_bg(C,G):- ansi_format([bg(C)],'~@',[call(G)]).
on_bg(G):- get_bgc(C),on_bg(C,G).
ansi_format_arc(Ansi,Format,Args):- on_bg(ansi_format(Ansi,Format,Args)),!.

underline_print(W):- ansi_format([bold,underline],'~@',[W]),!.
bold_print(W):- ansi_format(bold,'~@',[W]),!.

compound_var(C):- \+ plain_var(C), \+ attvar(C), is_ftVar(C).

is_bg_sym_or_var(C):- attvar(C),get_attr(C,ci,fg(_)),!,fail.
is_bg_sym_or_var(C):- (attvar(C); bg_sym(C); C==' '; C==''; C=='bg'; C == 0),!.



%color_print(_,W):- write(W),!.
color_print(C,W):- is_bg_sym_or_var(C),W=='_',!,on_bg(black,write(' ')),!.
color_print(C,W):- is_bg_sym_or_var(C),W=='_',color_print(C,'+').

color_print(C,W):- plain_var(C),integer(W),ansi_color(W,CI),!,ansi_format_arc(CI,'~w',[W]),!.
color_print(C,W):- plain_var(C),W=='?',!,on_bg(magenta,write('?')),!.
color_print(C,W):- plain_var(C),!,on_bg(ansi_format(italic,'~w',[W])),!.

color_print(C,W):- (cant_be_color(C,CbC);cant_be_color(W,CbC)),!,ansi_color(-CbC,Ansi),ansi_format_arc(Ansi,'~w',['_']),!.
color_print(C,W):- is_bg_sym_or_var(C),is_bg_sym_or_var(W), !, write(' ').

color_print(C,W):- is_bg_sym_or_var(C),integer(W),W<10, ansi_color(W,CI),!,ansi_format_arc(CI,'~w',[W]),!.
%color_print(C,W):- (is_ftVar(C);(bg_sym(C))), !, on_bg(yellow,var_color_print(C,W)).
color_print(C,W):- is_bg_sym_or_var(C),!,on_bg(cyan,ansi_format_arc([],'~w',[W])),!.
color_print(C-_,W):- !, color_print(C,W).
color_print(C,W):- atom(C),color_int(C,N),integer(N),!,color_print(N,W).
color_print(C,W):- integer(C),ansi_color(C,Color),on_bg(red,ansi_format_arc([bold,Color],'~w',[W])),!.
color_print(C,W):- C==0,!,ansi_format_arc([fg('#444444')],'~w',[W]),!.

color_name(C,W):- var(C),!,W=C.
color_name(C-_,W):-!,color_name(C,W).
color_name(C,W):- atom(C),!,W=C.
color_name(C,W):- integer(C),named_colors(L),nth0(C,L,W),!.

color_int(C,C):- var(C),!.
color_int(C-_,W):-!,color_int(C,W).
color_int(C,W):- integer(C),!,W=C.
color_int(C,W):- atom(C),!,named_colors(L),nth0(W,L,C),!.
color_int(C,C).


grid_color_code(C,C):- var(C).
grid_color_code(C-W,CC-W):- color_code(C,CC).
grid_color_code(C,CC):- color_code(C,CC).
%grid_color_code(C,CC):- is_grid_color(black-_),!,color_code(C,CC).
%grid_color_code(C,CC):- color_int(C,CC).

color_code(C,W):- color_name(C,W).


%print_g(H,V,_,LH,LV,HH,HV):- (H<LH;H>HH;V<LV;V>HV),!, write('  ').

resrv_dot(Code):-  code_type(Code,white);code_type(Code,punct);code_type(Code,quote);var_dot(Code);bg_dot(Code);fg_dot(Code);
 member(Code,`?.¨«¬­°```).


var_dot(63).
/* code=63 ?  code=183 · code=176 ° code=186 º 170 ª */
bg_dot(32).
/* 169	© 248	ø 216	Ø  215 ×  174	® */
%fg_dot(C):- nb_current(fg_dot,C),integer(C),!.
fg_dot(174).
cant_be_dot(183).
grid_dot(169).

%print_g(H,V,C0,_,_,_,_):- cant_be_color(C0),cant_be_color(C0,C),!,  ansi_format_arc([bold,fg('#ff8c00')],'~@',[(write('c'),user:print_g1(H,V,C))]).
%print_g(H,V,C0,_,_,_,_):- plain_var(C0),print_g1(H,V,C-'?'),!.

print_g(H,V,C,_,_,_,_):- write(' '), print_g1(H,V,C),!.

object_glyph(G,Glyph):- is_grid(G),!,grid_dot(Dot),name(Glyph,[Dot]).
object_glyph(G,Glyph):- plain_var(G),!,var_dot(Dot),name(Glyph,[Dot]).
object_glyph(G,Glyph):- object_indv_id(G,_Tst,GN2), i_sym(GN2,GN),!,i_glyph(GN,Glyph).

object_cglyph(G,CGlyph):- color(G,C),object_glyph(G,Glyph),wots(CGlyph,color_print(C,Glyph)).

%user:portray(S):- (string(S);atom(S)),atom_codes(S,[27|_]),write('"'),write(S),write('"').


print_gw1(N):- write(' '),print_g1(N).

print_g1(N):- into_color_glyph(N,C,Code),as_name(Code,S), color_print(C,S),!.

print_g1(C):- plain_var(C), color_print(C,'.'),!.

print_g2(C,S):- atom_number(S,_), cant_be_dot(DOT), name(N,[DOT]), color_print(C,N),!.
print_g2(C,S):- color_print(C,S),!.

as_name(N,Glyph):- plain_var(N),!,format(chars(Codes),'~p',[N]),last(Codes,Glyph).
as_name(Code,S):- atom(Code), S=Code.
as_name(Code,S):- integer(Code), name(S,[Code]).

into_color_glyph_ez(CTerm,Color,Code):- 
    ignore((sub_term(Color,CTerm),nonvar_or_ci(Color),is_color(Color))),
    ignore((sub_term(A,CTerm),atom(A), \+ is_color(A), i_glyph(A,Glyph))),
    ignore((sub_term(Nth,CTerm),integer(Nth),i_glyph(Nth,Glyph))),
    ignore((nonvar_or_ci(Glyph),name(Glyph,[Code|_]))).

%into_color_glyph(H,V,CTerm,Color,Code):- fail, get_grid_num_xyc(H,V,SColor,SNth),into_color_glyph(SColor+SNth+CTerm,Color,Code),nonvar_or_ci(Code).

into_color_glyph(N,C,DOT):- is_bg_color(N),get_bgc(C),bg_dot(DOT).
into_color_glyph(N,C,DOT):- is_spec_color(N,C),fg_dot(DOT).
into_color_glyph(N,C,DOT):- is_fg_color(N),N=C,fg_dot(DOT).
into_color_glyph(N,C,DOT):- is_bg_color(N),C=N,bg_dot(DOT).
into_color_glyph(N,C,DOT):- cant_be_color(N,C),cant_be_dot(DOT).
into_color_glyph(N,C,Code):- compound(N),N=(C-G),is_color(C),Code=G.
into_color_glyph(N,C,Code):- compound(N),N=(G-C),is_color(C),Code=G.
into_color_glyph(CTerm,Color,Code):- compound(CTerm),into_color_glyph_ez(CTerm,Color,Code),nonvar_or_ci(Code),!.
into_color_glyph(N,C,Glyph):- var(N),C=N,sformat(chars(Codes),'~p',[N]),last(Codes,Glyph),!.
into_color_glyph(bg,Black,BGD):- get_bgc(Black), bg_dot(BGD),!.
into_color_glyph(C,C,VAR):- plain_var(C),var_dot(VAR),!.
into_color_glyph(0,Black,BGD):- !, into_color_glyph(bg,Black,BGD).
into_color_glyph(C,C,FGD):- fg_dot(FGD),!.


i_glyph(N,Glyph):- bg_sym(BG), BG==N, !, bg_dot(Code), name(Glyph,[Code]).
i_glyph(Code,Glyph):- integer(Code), Code> 255, !,name(Glyph,[Code]).
i_glyph(N,Glyph):- integer(N),i_sym(N,Code),name(Glyph,[Code]).
i_glyph(N,Glyph):- plain_var(N),!,format(chars(Codes),'~p',[N]),last(Codes,Glyph).
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
:- save_codes.

/*
get_glyph(Point,Glyph):-  
  get_grid_num(Point,N),i_glyph(N,Glyph).
*/

:- fixup_exports.

