pt(P):- format('~N'),print_tree_nl(P),!.
wqs(X):- var(X), !, wqs(var(X)). wqs(nl):- !, nl. wqs(''):-!. wqs([]):-!.
wqs([H1|T]):- string(H1),!, write(H1), wqs(T).
wqs([skip(_)|T]):- !,wqs(T).
wqs(format(C,N)):- !, format(C,N).
wqs(writef(C,N)):- !, writef(C,N).
wqs(call(C)):- !, call(C).
wqs(pt(C)):- !, pt(C).
wqs(q(C)):- !, writeq(C).
wqs(color_count(C,N)):- !, write('color_count('),color_print(C,C),write(','), writeq(N), write(') ').
wqs(color_print(C,X)):- !, color_print(C,X), write(' ').
wqs(X):- \+ compound(X),!, write(X), write(' ').
wqs([H|T]):- wqs(H), wqs(T).
wqs(X):- writeq(X), write(' ').
wqnl(X):- is_list(X),!,format('~N'),wqs(X),format('~N').
wqnl(X):- format('~N~q~N',[X]).
dash_char:- dash_char(40).
dash_char(H):- integer(H), dash_border(H).
dash_char(S):- format('~N'),dash_char(60,S),format('~N').
dash_char(H,_):- H < 1,!.
dash_char(H,C):-forall(between(0,H,_),write(C)).
dash_border_no_nl(Width):- WidthM1 is Width-1, write(' _'),dash_char(WidthM1,'__').
dash_border(Width):- !, dash_border_no_nl(Width),nl.
dash_uborder(Width):- format('~N'), WidthM1 is Width-1, write(' ¯'),dash_char(WidthM1,'¯¯'),nl.

functor_color(pass,green).
functor_color(fail,red).
functor_color(warn,yellow).

arcdbg(G):- compound(G), compound_name_arity(G,F,_),functor_color(F,C),wots(S,print(G)),color_print(C,S),!,format('~N').
arcdbg(G):- wdmsg(G).

user:portray(Grid):- \+ \+ catch((
  \+ tracing, \+ is_object(Grid),  \+ is_group(Grid), 
   (is_gridoid(Grid);is_points_list(Grid)),
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

as_str([],''):-!.
as_str(A,S):- atom_string(A,S).

write_padding(E1,_W1,E2,LW):- %write(' '),
    W1 = LW,
   format('~N'),as_str(E1,S1), as_str(E2,S2), 
   write(S1), write('\t'), line_position(user_output,L1), Pad1 is W1 - L1, dash_char(Pad1, ' '),
   write(S2), format('~N').

print_length(S,L):- atom_codes(S,C), include(uses_space,C,SS),length(SS,L).
uses_space(C):- code_type(C,print).

into_ss_string(Var,_):- var(Var),!,throw(var_into_ss_string(Var)).
into_ss_string(G,SS):- is_grid(G),!,wots(S,print_grid(G)),!,into_ss_string(S,SS).
into_ss_string(ss(Len,L),ss(Len,L)):-!.
into_ss_string(L,ss(Len,L)):- is_list(L), find_longest_len(L,Len),!.
into_ss_string(S,SS):- string(S), atomics_to_string(L,'\n',S),!,into_ss_string(L,SS).
into_ss_string(C,SS):- wots(S,C), into_ss_string(S,SS).

find_longest_len(SL,L):- find_longest_len(SL,10,L),!.
find_longest_len([],L,L).
find_longest_len([S|SS],N,L):- print_length(S,N2),max_min(N,N2,NM,_),
  find_longest_len(SS,NM,L).

print_w_pad(Pad,S):- atomics_to_string(L,'\n',S)-> maplist(print_w_pad0(Pad),L).
print_w_pad0(Pad,S):- format('~N'),dash_char(Pad,' '), write(S).

print_equals(_,N,V):- \+ compound(V),wqnl(N=V).
print_equals(Grid,N,Ps):- is_object(Ps),grid_size(Grid,H,V),print_points(N,H,V,Ps),!.
print_equals(Grid,N,PL):- is_group(PL), grid_size(Grid,H,V), 
  locally(grid_nums(PL),print_list_of_points(N,H,V,[[]])).
print_equals(_,N,G):- print_equals(N,G).


points_name(E,Name):- 
 props_of_points(E,Ns),
  ignore((get_grid_nums(Nums),nth0(N,Nums,EE),EE=@=E,i_sym(N,Code),format(atom(Name),' Individual #~w  Code: "~s" ~w',[N,[Code],Ns]))),!.
points_name(E,pop(Ns)):- props_of_points(E,Ns).

print_list_of_points(N,H,V,PL):- 
  length(PL,Len),
  wqnl(N=list_of_points(Len)),!, 
  ignore((nop(debug_indiv),  
    forall(nth1(I,PL,E),print_points(N:I,H,V,E)))).

% print_points(N,H,V,E):- nop(print_points(N,H,V,E)),!.
print_points(N,H,V,E):- 
  dash_char(H,"-"),nl,  
  points_name(E,Name),
  write(Name), write(' from '),writeln(N),
  points_range(E,LoH,LoV,HiH,HiV,H,V),length(E,Len),wqnl(len(Len)->offset_ranges(LoH,LoV,HiH,HiV,H,V)),
  
  print_grid(1,1,LoH,LoV,HiH,HiV,H,V,E),dash_char(H,"-"),!,
  nl.


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
print_equals(colors_count,XY):-print_equals(cc,XY).
print_equals(Name,color_count(C,N)):-print_equals(Name,cc(C,N)).
print_equals(Name,X=Y):- !, print_equals(Name=X,Y).
%print_equals(Name,[H|L]):- !, maplist(print_equals(Name),[H|L]).
print_equals(Name,Val):- is_list(Val),forall(nth0(N,Val,E),print_equals(Name:N,E)).
print_equals(Name,Val):- pt(Name=Val).

commawrite(S):- write(','),write(S).



as_color(color_count(Count,Num),List):- color_name(Num,Name),wots(List,color_print(Num,Name=Count)).
better_value(V,List):- is_list(V), maplist(as_color,V,List).
better_value([G|V],List):- 
  is_group([G|V]),
  maplist(points_to_grid,[G|V],List),
  [G|V] \=@= List.


print_igrid(Name,SIndvOut,InOutL):-
   write('  '),writeq(Name),writeln('  '),
   append(SIndvOut,InOutL,Print),
   grid_size(SIndvOut,H,V),
   print_grid(H,V,Print).

print_grid(Grid):- notrace(print_grid0(_HH,_VV,Grid)).
%print_grid0(Grid):- var(Grid),!, throw(var_print_grid(Grid)).

print_grid(H,V,Grid):- notrace(print_grid0(H,V,Grid)).

print_grid0(H,V,G):- is_empty_grid(G),!,wdmsg(is_empty_grid(H,V)).
print_grid0(H,V,Grid):- \+ callable(Grid),!,write('not grid: '),
  GG= nc_print_grid(H,V,Grid),
  pt(GG),throw(GG).
print_grid0(H,V,Grid):- \+ is_gridoid(Grid), into_grid(Grid,G),!,print_grid0(H,V,G).
print_grid0(H,V,Grid):- print_grid(1,1,H,V,Grid).

print_grid(SH,SV,EH,EV,Grid):- print_grid(SH,SV,SH,SV,EH,EV,EH,EV,Grid).
%print_grid(SH,SV,LoH,LoV,HiH,HiV,EH,EV,Grid):- nop(print_grid(SH,SV,LoH,LoV,HiH,HiV,EH,EV,Grid)),!.
print_grid(SH,SV,LoH,LoV,HiH,HiV,EH,EV,Grid):-
 line_position(current_output,O),
 O1 is O+1,
 notrace((wots(S, \+ \+ print_grid0(true,SH,SV,LoH,LoV,HiH,HiV,EH,EV,Grid)),
 print_w_pad(O1,S))).

/*
print_grid0(Bordered,SH,SV,LoH,LoV,HiH,HiV,EH,EV,Grid):- 
  is_grid(Grid), Grid=[[AShape|_]|_], nonvar(AShape),(AShape=A-Shape),var(A),nonvar(Shape),
   bagof(A-Shape,(sub_term(AShape,Grid),nonvar(AShape),AShape= (A-Shape),var(A)),Colors),list_to_set(Colors,AShapes),
   block_colors(BC), create_grid_key(BC,AShapes,KeyS),
   forall(member(Key,KeyS),wqnl(Key)),
   term_variables(Grid,Vars),
   create_grid_key2(BC,Vars,KeyS2),
   forall(member(Key,KeyS2),wqnl(Key)),
   print_grid1(Bordered,SH,SV,LoH,LoV,HiH,HiV,EH,EV,Grid),
   writeq(Grid).
*/
print_grid0(Bordered,SH,SV,LoH,LoV,HiH,HiV,EH,EV,Grid):-
  print_grid1(Bordered,SH,SV,LoH,LoV,HiH,HiV,EH,EV,Grid).


print_grid1(_Bordered,SH,SV,LoH,LoV,HiH,HiV,EH,EV,Grid):-
  write('\n '), 
  ((var(EH) ; var(EV))->grid_size(Grid,EH,EV);true),
  Width is EH-SH, 
  underline_print((user:dash_border_no_nl(Width))),
  nop(get_bgc(BG)),
  forall(between(SV,EV,V),
   ((format('~N'),
     forall(between(SH,EH,H), 
     (hv_value_or(Grid,C,H,V,BG)->
        (once(print_g(H,V,C,LoH,LoV,HiH,HiV))))),dash_char(3,' ')))),
  format('~N '),!,
  underline_print((user:dash_border_no_nl(Width))).


print_grid1(_Bordered,SH,SV,LoH,LoV,HiH,HiV,EH,EV,Grid):-
  write('\n '), 
  ((var(EH) ; var(EV))->grid_size(Grid,EH,EV);true),
  Width is EH-SH, underline_print((write('{|image||'),user:dash_border_no_nl(Width-4))),
  nop(get_bgc(BG)),
  forall(between(SV,EV,V),
   ((format('~N'),
     forall(between(SH,EH,H), 
     (hv_value_or(Grid,C,H,V,BG)->
        (once(print_g(H,V,C,LoH,LoV,HiH,HiV))))),dash_char(3,' ')))),
   format('~N'),!,
  underline_print((user:dash_border_no_nl(Width-1),write('|}'))).
%print_grid(Grid):- is_grid(Grid),!, maplist(print_rows,Grid),nl.
%print_rows(List):- maplist(print_g,List),nl.
%block_colors([(black),(blue),(red),(green),(yellow),'#c0c0c0',(magenta),'#ff8c00',(cyan),'#8b4513']).
%block_colors([(black),(blue),(red),(green),(yellow),Silver,('#966cb8'),'#ff8c00',(cyan),'#8b4513']):- silver(Silver),!.
block_colors([(black),(blue),(red),(green),(yellow),Silver,(magenta),'#ff8c00',(cyan),'#8b4513']):- silver(Silver),!.
named_colors([(black),(blue),(red),(green),(yellow),(silver),(purple),(orange),(cyan),(brown)]).
named_colors([(black),(blue),(red),(green),(yellow),(grey),(magenta),(orange),(cyan),(brown)]).
named_colors([(black),(blue),(red),(green),(yellow),(grey),(pink),(orange),(cyan),(maroon)]).

silver('#7b7b7b').
silver('#c0c0c0').
silver('#9a9a9a').

ansi_color(C,Color):- C\==0,block_colors(L),nth0(C,L,Color).

underline_print(W):- ansi_format([underline],'~@',[W]),!.

color_print(C,W):- var(C),integer(W),ansi_color(W,CI),!,ansi_format([underline,fg(CI)],'~w',[W]),!.
color_print(C,W):- var(C),!,ansi_format([underline],'~w',[W]),!.
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

is_grid_color(C):- var(C),!,fail.
% makes grid colors an integer.. 
%is_grid_color(C):- !,integer(C).
% we are using atoms currently
is_grid_color(C-_):- !, is_color(C).
is_grid_color(C):- is_color(C).

grid_color_code(C,C):- var(C).
grid_color_code(C-W,CC-W):- color_code(C,CC).
grid_color_code(C,CC):- color_code(C,CC).
%grid_color_code(C,CC):- is_grid_color(black-_),!,color_code(C,CC).
%grid_color_code(C,CC):- color_int(C,CC).

color_code(C,W):- color_name(C,W).
is_color_dat(C):- atomic(C),color_code(C,W),!,C==W.

%print_g(H,V,_,LH,LV,HH,HV):- (H<LH;H>HH;V<LV;V>HV),!, write('  ').

resrv_dot(Code):-  code_type(Code,white);code_type(Code,punct);code_type(Code,quote);var_dot(Code);bg_dot(Code);fg_dot(Code);
 member(Code,`?.¨«¬­°```).

var_dot(63).
/* code=63 ?  code=183 · code=176 ° code=186 º 170 ª */
bg_dot(183).
/* 169	© 248	ø 216	Ø  215 ×  174	® */
%fg_dot(C):- nb_current(fg_dot,C),integer(C),!.
fg_dot(174).

grid_dot(169).

print_g(H,V,C,_,_,_,_):- write(' '), print_g1(H,V,C),!.

into_color_glyph(CTerm,Color,Code):- 
    ignore((sub_term(Color,CTerm),nonvar(Color),is_color(Color))),
    ignore((sub_term(A,CTerm),atom(A), \+ is_color(A), i_glyph(A,Glyph))),
    ignore((sub_term(Nth,CTerm),integer(Nth),i_glyph(Nth,Glyph))),
    ignore((nonvar(Glyph),name(Glyph,[Code|_]))).

into_color_glyph(H,V,CTerm,Color,Code):- fail, get_grid_num_xyc(H,V,SColor,SNth),into_color_glyph(SColor+SNth+CTerm,Color,Code),nonvar(Code).
into_color_glyph(_,_,CTerm,Color,Code):- into_color_glyph(CTerm,Color,Code),nonvar(Code).
into_color_glyph(_,_,C,C,VAR):- var(C),var_dot(VAR),!.
into_color_glyph(_,_,0,0,BGD):- bg_dot(BGD),!.
into_color_glyph(_,_,C,C,FGD):- fg_dot(FGD),!.

print_g1(H,V,C0):- into_color_glyph(H,V,C0,C,Code),name(S,[Code]),!, color_print(C,S),!.
print_g1(_,_,C):- var(C), color_print(C,'?'),!.
%print_g1(_,_,C):- trace, write(C).

i_sym(N,Code):- var(N), Code = 63.
i_sym(N,Code):- change_code(N,NN), i_syms(Codes),nth0(NN,Codes,Code),!.
%change_code(N,M):- M is N * 100,!.
%change_code(N,M):- N>10, M is (N * 10 ),!.
change_code(N,N). % M is N+212.

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


get_glyph(Point,Glyph):-  
  get_grid_num(Point,N),i_glyph(N,Glyph).

i_glyph(N,Glyph):- atom(N),atom_codes(N,[Code|_]),name(Glyph,[Code]).
i_glyph(N,Glyph):- integer(N),i_sym(N,Code),name(Glyph,[Code]).

get_grid_num(C-Point,N):-
  hv_point(X,Y,Point),
  get_grid_num_xyc(X,Y,C,N).

get_grid_num(Point,N):-
  hv_point(X,Y,Point),
  get_grid_num_xyc(X,Y,_C,N).

get_grid_num_xyc(X,Y,C,N):- fail,
  hv_point(X,Y,Point),
  get_grid_nums(GridsN),
  nth0(N,GridsN,E),
  once((((member(C-PPoint,E);member(PPoint,E)),nonvar(PPoint),Point=PPoint);hv_value(E,C,X,Y))).

get_grid_nums(GridNums):-
  nb_current(test_name_w_type,NameType),
  (grid_nums(NameType,Grids1)->true;Grids1=[]),
  (grid_nums(Grids0)->true;Grids0=[]),
  append(Grids1,Grids0,GridNums).


:- thread_local(grid_nums/2).
:- thread_local(grid_nums/1).
set_grid_nums(Gs):- 
   nb_current(test_name_w_type,NameType),
   asserta(grid_nums(NameType,Gs)).

