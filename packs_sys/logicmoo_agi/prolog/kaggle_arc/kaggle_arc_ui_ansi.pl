/*
  this is part of (H)MUARC

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/


tersify(I,O):- is_list(I), with_output_to(string(S),display(I)),!, ((atom_length(S,N), N>70) -> len(N)=O; I=O).
tersify(I,O):- compound(I), !, compound_name_arguments(I,F,IA), maplist(tersify,IA,OA), compound_name_arguments(O,F,OA).
tersify(I,I).

ptt(P):- tersify(P,Q),!,pt(Q).
pt(P):- format('~N'),print_tree_nl(P),!.


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
dash_border(Width):- !, dash_border_no_nl(Width),nl,!.
dash_uborder(Width):- format('~N'), WidthM1 is Width-1, write(' ¯'),dash_char(WidthM1,'¯¯'),nl.

functor_color(pass,green).
functor_color(fail,red).
functor_color(warn,yellow).

arcdbg(G):- compound(G), compound_name_arity(G,F,_),functor_color(F,C),wots(S,print(G)),color_print(C,S),!,format('~N').
arcdbg(G):- wdmsg(G).

user:portray(Grid):- arc_portray(Grid),!.

arc_portray(Grid):- \+ \+ catch((
  % \+ tracing, 
  \+ is_object(Grid),  \+ is_group(Grid), % ground(Grid),
   (is_gridoid(Grid);(is_points_list(Grid),ground(Grid))),
   grid_size(Grid,H,V),!,H>0,V>0, wots(S,print_grid(H,V,Grid)),write(S)),_,false).
arc_portray(Grid):- \+ \+ catch(( fail,
  tracing, \+ is_object(Grid),  \+ is_group(Grid), ground(Grid),
   ((is_points_list(Grid),ground(Grid))),
   grid_size(Grid,H,V),!,H>0,V>0, wots(S,print_grid(H,V,Grid)),write(S)),_,false).
%user:portray(Grid):- ((\+ tracing, is_group(Grid),print_grid(Grid))).
%user:portray(Grid):- notrace((is_object(Grid),print_grid(Grid))).

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

show_pair(IH,IV,OH,OV,Type,PairName,In,Out):-
  LW is (IH * 2 + 12),
  NameIn =.. [Type,PairName+in],
  NameOut =.. [Type,PairName+out],
  wots(U1, print_Igrid(IH,IV,NameIn,In,[])),
  wots(U2, print_Igrid(OH,OV,NameOut,Out,[])),
  print_side_by_side(U1,LW,U2),!,
  INFO = [grid_dim,mass,colors_count_size,colors],
  print_side_by_side(
     describe_feature(In,[call(writeln('IN'))|INFO]),LW,
    describe_feature(Out,[call(writeln('OUT'))|INFO])),!,
  ignore(show_pair_I_info(NameIn,NameOut,In,Out)),!.

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
  (((nonvar_or_ci(E),notrace,trace,rtrace(C))->throw(E);true)), into_ss_string(S,SS).

find_longest_len(SL,L):- find_longest_len(SL,10,L),!.
find_longest_len([],L,L).
find_longest_len([S|SS],N,L):- print_length(S,N2),max_min(N,N2,NM,_),
  find_longest_len(SS,NM,L).

print_w_pad(Pad,S):- atomics_to_string(L,'\n',S)-> maplist(print_w_pad0(Pad),L).
print_w_pad0(Pad,S):- format('~N'),dash_char(Pad,' '), write(S).

print_equals(_,N,V):- \+ compound(V),wqnl(N=V).
print_equals(Grid,N,Ps):- is_object(Ps),grid_size(Grid,H,V),print_Igrid(H,V,N,Ps,[]),!.
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


print_Igrid(Name,SIndvOut,InOutL):-   
   grid_size(SIndvOut,H,V),
   print_Igrid(H,V,Name,SIndvOut,InOutL).

print_Igrid(H,V,Name,SIndvOut,InOutL):-   
   append(SIndvOut,InOutL,Print),
   print_grid(H,V,Print),format('~N'),
   write('  '),writeq(Name),writeln('  '),!.

print_grid(Grid):- quietly(print_grid0(_HH,_VV,Grid)).
%print_grid0(Grid):- plain_var(Grid),!, throw(var_print_grid(Grid)).

print_grid(H,V,Grid):- quietly(print_grid0(H,V,Grid)).

print_grid0(H,V,G):- is_empty_grid(G), %trace, dumpST,
 wdmsg(is_empty_grid(H,V)),!.
print_grid0(H,V,Grid):- \+ callable(Grid),!,write('not grid: '),
  GG= nc_print_grid(H,V,Grid),
  pt(GG),throw(GG).
print_grid0(H,V,Grid):- \+ is_gridoid(Grid), into_grid(Grid,G),!,print_grid0(H,V,G).
print_grid0(H,V,Grid):- print_grid(1,1,H,V,Grid).

print_grid(SH,SV,EH,EV,Grid):- print_grid(SH,SV,SH,SV,EH,EV,EH,EV,Grid).
%print_grid(SH,SV,LoH,LoV,HiH,HiV,EH,EV,Grid):- nop(print_grid(SH,SV,LoH,LoV,HiH,HiV,EH,EV,Grid)),!.

print_grid(SH,SV,LoH,LoV,HiH,HiV,EH,EV,Grid):-
 print_grid0(true,SH,SV,LoH,LoV,HiH,HiV,EH,EV,Grid),!.

print_grid(SH,SV,LoH,LoV,HiH,HiV,EH,EV,Grid):-
 line_position(current_output,O),
 O1 is O+1,
 quietly((wots(S, print_grid0(true,SH,SV,LoH,LoV,HiH,HiV,EH,EV,Grid)),
 print_w_pad(O1,S))).

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
print_grid0(Bordered,SH,SV,LoH,LoV,HiH,HiV,EH,EV,Grid):-
  \+ \+ print_grid1(Bordered,SH,SV,LoH,LoV,HiH,HiV,EH,EV,Grid).


print_grid1(_Bordered,SH,SV,LoH,LoV,HiH,HiV,EH,EV,GridI):-
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
        (once(print_g(H,V,C,LoH,LoV,HiH,HiV))))),write(' |')))),
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

silver('#7b7b7b').
silver('#c0c0c0').
silver('#9a9a9a').

ansi_color(C,Color):- attvar(C),get_attr(C,ci,fg(N)),ansi_color(N,Color),!.
ansi_color(C,Color):- attvar(C),get_attr(C,ci,bg),ansi_color(0,Color),!.
ansi_color(C,Color):- integer(C),block_colors(L),nth0(C,L,Color).
ansi_color(C,Color):- color_int(C,I),ansi_color(I,Color).

underline_print(W):- ansi_format([bold,underline],'~@',[W]),!.
bold_print(W):- ansi_format([bold],'~@',[W]),!.

compound_var(C):- \+ plain_var(C), \+ attvar(C), is_ftVar(C).
hi_color_print(CI,W):- ansi_format([hfg(CI)],'~w',[W]).
is_bg_sym_or_var(C):- (attvar(C); bg_sym(C); C==' '; C==''; C=='bg'; C == 0),!.

var_color_print(C,W):- bg_sym(BG),C==BG, W==BG, !, write(' ').
var_color_print(C,W):- bg_sym(BG),C==BG, plain_var(W),!, write(' ').
var_color_print(C,W):- compound_var(C), arg(1,C,C1),!, var_color_print(W,C1).
var_color_print(C,W):- compound_var(W), arg(1,W,W1),!, var_color_print(C,W1).
var_color_print(C,W):- C\==W,plain_var(W),C=W, var_color_print(C,W).

var_color_print(C,W):- C==' ',W==' ',!,write(' '),!.
var_color_print(C,W):- integer(W),ansi_color(C,CI),!, hi_color_print(CI,W),!.
var_color_print(C,W):- integer(W),i_glyph(W,WO),!,var_color_print(C,WO).
%var_color_print(C,W):- char_type(W,space),!,write(' '),!.
var_color_print(_,W):- char_type(W,space), ansi_format([],'~w',[W]),!.
var_color_print(_,W):- i_glyph(W,G),ansi_format([],'~w',[G]),!.

color_print(C,W):- (has_color_c(C,OC);has_color_c(W,OC)),!,ansi_format([fg(OC)],'~w',['_']),!.
color_print(C,W):- is_bg_sym_or_var(C),W=='_',!,ansi_format([],'~w',[' ']),!.
color_print(C,W):- is_bg_sym_or_var(C),W=='_',color_print(C,'+').

color_print(C,W):- plain_var(C),integer(W),ansi_color(W,CI),!,ansi_format([fg(CI)],'~w',[W]),!.
color_print(C,W):- plain_var(C),W=='?',!,ansi_format([],'~w',['?']),!.
color_print(C,W):- plain_var(C),!,ansi_format([italic],'~w',[W]),!.

color_print(C,W):- is_bg_sym_or_var(C),is_bg_sym_or_var(W), !, write(' ').

color_print(C,W):- is_bg_sym_or_var(C),integer(W),W<10, ansi_color(W,CI),!,ansi_format([fg(CI)],'~w',[W]),!.
color_print(C,W):- (is_ftVar(C);(bg_sym(C))), !, var_color_print(C,W).
color_print(C,W):- is_bg_sym_or_var(C),!,ansi_format([],'~w',[W]),!.
color_print(C-_,W):- !, color_print(C,W).
color_print(C,W):- atom(C),color_int(C,N),integer(N),!,color_print(N,W).
color_print(C,W):- integer(C),ansi_color(C,Color),ansi_format([bold,fg(Color)],'~w',[W]),!.
color_print(C,W):- C==0,!,ansi_format([fg('#444444')],'~w',[W]),!.

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

%print_g(H,V,C0,_,_,_,_):- has_color_c(C0),has_color_c(C0,C),!,  ansi_format([bold,fg('#ff8c00')],'~@',[(write('c'),user:print_g1(H,V,C))]).
%print_g(H,V,C0,_,_,_,_):- plain_var(C0),print_g1(H,V,C-'?'),!.

print_g(H,V,C,_,_,_,_):- write(' '), print_g1(H,V,C),!.

object_glyph(G,Glyph):- is_grid(G),!,grid_dot(Dot),name(Glyph,[Dot]).
object_glyph(G,Glyph):- plain_var(G),!,var_dot(Dot),name(Glyph,[Dot]).
object_glyph(G,Glyph):- object_indv_id(G,_Tst,GN2), i_sym(GN2,GN),!,i_glyph(GN,Glyph).

object_cglyph(G,CGlyph):- color(G,C),object_glyph(G,Glyph),wots(CGlyph,color_print(C,Glyph)).

%user:portray(S):- (string(S);atom(S)),atom_codes(S,[27|_]),write('"'),write(S),write('"').




%print_g1(E):- has_color_c(E,C),color_print(C,'+'),!.
%print_g1(C0):- sub_term(E,C0),has_color_c(E,C),color_print(C,'='),!.
print_g1(N):- plain_var(N),has_color_c(N,C),cant_be_dot(DOT),ansi_color(C,CI),ansi_format([fg(CI)],'~s',[[DOT]]),!.
print_g1(N):- plain_var(N),has_color_c(N,C),format(chars(Codes),'~p',[N]),last(Codes,S),print_g2(C,S),!.
print_g1(N):- into_color_glyph(N,C,Code),as_name(Code,S), color_print(C,S),!.
print_g1(C):- plain_var(C), color_print(C,'.'),!.

print_g2(C,S):- atom_number(S,_), cant_be_dot(DOT), name(N,[DOT]), color_print(C,N),!.
print_g2(C,S):- color_print(C,S),!.

as_name(Code,S):- plain_var(Code), i_glyph(Code,S).
as_name(Code,S):- integer(Code), name(S,[Code]).

into_color_glyph_ez(CTerm,Color,Code):- 
    ignore((sub_term(Color,CTerm),nonvar_or_ci(Color),is_color(Color))),
    ignore((sub_term(A,CTerm),atom(A), \+ is_color(A), i_glyph(A,Glyph))),
    ignore((sub_term(Nth,CTerm),integer(Nth),i_glyph(Nth,Glyph))),
    ignore((nonvar_or_ci(Glyph),name(Glyph,[Code|_]))).

%into_color_glyph(H,V,CTerm,Color,Code):- fail, get_grid_num_xyc(H,V,SColor,SNth),into_color_glyph(SColor+SNth+CTerm,Color,Code),nonvar_or_ci(Code).
into_color_glyph(N,C,Glyph):- plain_var(N),has_color_c(N,C),format(chars(Codes),'~p',N),last(Codes,Glyph),!.
into_color_glyph(N,C,Glyph):- has_color_c(N,C),format(chars(Codes),'~p',N),last(Codes,Glyph),!.

into_color_glyph(W,C,FGD):- has_color_c(W,C), bg_dot(FGD),!.
into_color_glyph(CTerm,Color,Code):- into_color_glyph_ez(CTerm,Color,Code),nonvar_or_ci(Code),!.
into_color_glyph(C,C,VAR):- plain_var(C),var_dot(VAR),!.
into_color_glyph(bg,black,BGD):- bg_dot(BGD),!.
into_color_glyph(0,0,BGD):- bg_dot(BGD),!.
into_color_glyph(C,C,FGD):- fg_dot(FGD),!.


i_glyph(N,Glyph):- bg_sym(BG), BG==N, !, bg_dot(Code), name(Glyph,[Code]).
i_glyph(Code,Glyph):- integer(Code), Code> 255, !,name(Glyph,[Code]).
i_glyph(N,Glyph):- integer(N),i_sym(N,Code),name(Glyph,[Code]).
i_glyph(N,Glyph):- plain_var(N),!,format(chars(Codes),'~p',N),last(Codes,Glyph).
i_glyph(N,Glyph):- atom(N),atom_chars(N,Chars),last(Chars,Glyph).

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
  assert(i_syms(CCC)).

save_codes:- save_codes(42600).
:- save_codes.

/*
get_glyph(Point,Glyph):-  
  get_grid_num(Point,N),i_glyph(N,Glyph).
*/


