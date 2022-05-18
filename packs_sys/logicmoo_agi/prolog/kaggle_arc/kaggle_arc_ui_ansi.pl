pt(P):- format('~N'),print_tree_nl(P),!.
wqs(X):- var(X), !, wqs(var(X)). wqs(nl):- !, nl. wqs(''):- !.
wqs(X):- \+ compound(X),!, write(X), write(' ').
wqs(fav(_)):- !.

wqs(color_count(C,N)):- !, write('color_count('),color_print(C,C),write(','), writeq(N), write(') ').
wqs(color_print(C,X)):- !, color_print(C,X), write(' ').
wqs(X):- writeq(X), write(' ').
wqnl(X):- is_list(X),!,format('~N'),maplist(wqs,X),format('~N').
wqnl(X):- format('~N~q~N',[X]).
dash_char:- dash_char(40).
dash_char(H):- integer(H), dash_border(H).
dash_char(S):- format('~N'),dash_char(60,S),format('~N').
dash_char(H,_):- H < 1,!.
dash_char(H,C):-forall(between(0,H,_),write(C)).
dash_border(Width):- format('~N'), WidthM1 is Width-1, write(' _'),dash_char(WidthM1,'__'),nl.
dash_uborder(Width):- !, dash_border(Width).
dash_uborder(Width):- format('~N'), WidthM1 is Width-1, write(' ¯'),dash_char(WidthM1,'¯¯'),nl.

functor_color(pass,green).
functor_color(fail,red).
functor_color(warn,yellow).

arcdbg(G):- compound(G), compound_name_arity(G,F,_),functor_color(F,C),wots(S,print(G)),color_print(C,S),!,format('~N').
arcdbg(G):- wdmsg(G).

red_noise:- format('~N'),
  color_print(red,'--------------------------------------------------------------'),nl,
  color_print(red,'--------------------------------------------------------------'),nl,
  color_print(red,'--------------------------------------------------------------'),nl.



nc_to_cint(C,C):- var(C),!.
nc_to_cint(C-_,I):- grid_color_code(C,I),!.
nc_to_cint(C,I):- grid_color_code(C,I).

debug_indiv:- test_config(nodebug_indiv),!,fail.
debug_indiv:- test_config(debug_indiv),!.
debug_indiv:- test_config(indiv(_)),!.

debug_indiv(Var):- var(Var),pt(debug_indiv(Var)),!.

debug_indiv(Grid):- is_grid(Grid),!,grid_size(Grid,H,V),
  dash_char(H),
  wqnl(debug_indiv_grid(H,V)),
  print_grid(Grid),
  dash_char(H),!.

debug_indiv(obj(A)):- \+ is_list(A),!, pt(debug_indiv(obj(A))).
debug_indiv(A):- is_point_obj(A,Color,Point),
  object_indv_id(A,Tst,Id), i_glyph(Id,Sym),
  hv_point(H,V,Point), i_glyph(Id,Sym),
  wqnl([' % Point: ', color_print(Color,Sym), dot, color(Color), fav(Tst), nth(Id), offset(H,V)]),!. 

debug_indiv(Obj):- Obj = obj(A), is_list(A),
  object_indv_id(Obj,_,Id),
  ignore(colors_count(Obj,[color_count(FC,_)|_])),
  maplist(remove_too_verbose,A,AA),
  flatten(AA,F),
  sort(F,AAA),
  i_glyph(Id,Sym),wqnl([' % Indiv: ',color_print(FC,Sym)|AAA]),!. 

debug_indiv(obj(A)):- is_list(A),!, 
  dash_char,  
  maplist(debug_indiv(obj(A)),A),
  dash_char,!.

debug_indiv(List):- is_list(List),!,length(List,Len), 
  dash_char,
  wqnl(list = Len),
  max_min(Len,40,_,Min),
  forall(between(1,Min,N),(N<40->(nth1(N,List,E),debug_indiv(E));wqnl(total = Len))),
  dash_char,!.

debug_indiv(Other):-
  dash_char,
  functor(Other,F,A),
  wqnl(other = F/A),
  pt(Other),
  dash_char,!.

remove_too_verbose(H,''):- too_verbose(H).
remove_too_verbose(object_shape(H),H).
remove_too_verbose(colors_count(H),H).
remove_too_verbose(object_indv_id(X,Y),[fav1(X),nth(Y)]).
remove_too_verbose(object_offset(X,Y),offset(X,Y)).
remove_too_verbose(object_size(X,Y),size(X,Y)).
remove_too_verbose(point_count(X),pixels(X)).
remove_too_verbose(H,H).
too_verbose(P):- compound(P),compound_name_arity(P,F,_),!,too_verbose(F).
too_verbose(globalpointlist).
too_verbose(localcolorlesspointlist).
too_verbose(localpointlist).
too_verbose(grid_size).

debug_indiv(Obj,P):- compound_name_arguments(P,F,A),debug_indiv(Obj,P,F,A).
debug_indiv(_,_,X,_):-too_verbose(X),!.
debug_indiv(Obj,_,F,[A]):- maplist(is_cpoint,A),!,
  object_size(Obj,H,V), wqnl(F), 
  object_offset(Obj,OH,OV),
  EH is OH+H-1,
  EV is OV+V-1,
  object_indv_id(Obj,_Tst,Id), %i_sym(Id,Sym),
  i_glyph(Id,Glyph),
  Pad1 is floor(H),  

  wots(S,
    (dash_char(Pad1,' '),write(Id=Glyph),
     print_grid(OH,OV,OH,OV,EH,EV,EH,EV,Obj))),

  nop(wots(S,
    (dash_char(Pad1,' '),write(Id=Glyph),
     print_grid(H,V,A)))),


  Pad is floor(20-V/2),
  max_min(Pad,OH,PadH,_),
  print_w_pad(PadH,S).


debug_indiv(_,P,_,_):- pt(P).




print_side_by_side(C1,LW,C2):- 
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
   write(S1), line_position(user_output,L1), Pad1 is W1 - L1, dash_char(Pad1, ' '),
   write(S2), format('~N').

print_length(S,L):- atom_codes(S,C), include(uses_space,C,SS),length(SS,L).
uses_space(C):- code_type(C,print).

into_ss_string(Var,_):- var(Var),!,throw(var_into_ss_string(Var)).
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
print_equals(Grid,N,PL):- is_objectlist(PL), grid_size(Grid,H,V), 
  locally(grid_nums(PL),print_list_of_points(N,H,V,[[]])).
print_equals(_,N,G):- print_equals(N,G).


points_name(E,Name):- 
 props_of_points(E,Ns),
  ignore((grid_nums(Nums),nth0(N,Nums,EE),EE=@=E,i_sym(N,Code),format(atom(Name),' Individual #~w  Code: "~s" ~w',[N,[Code],Ns]))),!.
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
print_equals(Name,X=Y):- !, print_equals(Name=X,Y).
%print_equals(Name,[H|L]):- !, maplist(print_equals(Name),[H|L]).
print_equals(Name,Val):- pt(Name=Val).

commawrite(S):- write(','),write(S).



as_color(color_count(Count,Num),List):- color_name(Num,Name),wots(List,color_print(Num,Name=Count)).
better_value(V,List):- is_list(V), maplist(as_color,V,List).
better_value([G|V],List):- 
  is_objectlist([G|V]),
  maplist(points_to_grid,[G|V],List),
  [G|V] \=@= List.


print_igrid(Name,SIndvOut,[In|Out]):- grid_size(In,H,V),
   %dash_char(H,' '),
   write('  '),writeq(Name),writeln('  '),
   append(SIndvOut,[In|Out],Print),
   print_grid(H,V,Print).

print_grid(Grid):- grid_size(Grid,HH,VV), print_grid(HH,VV,Grid).
print_grid(HH,VV,Grid):- print_grid(1,1,HH,VV,Grid).
print_grid(SH,SV,EH,EV,Grid):- print_grid(SH,SV,SH,SV,EH,EV,EH,EV,Grid).
%print_grid(SH,SV,LoH,LoV,HiH,HiV,EH,EV,Grid):- nop(print_grid(SH,SV,LoH,LoV,HiH,HiV,EH,EV,Grid)),!.
print_grid(SH,SV,LoH,LoV,HiH,HiV,EH,EV,Grid):- 
 wots(S,print_grid0(true,SH,SV,LoH,LoV,HiH,HiV,EH,EV,Grid)),
 print_w_pad(1,S).

print_grid0(Bordered,SH,SV,LoH,LoV,HiH,HiV,EH,EV,Grid):-
   IsBordered = (hv(1,1)\==hv(LoH,LoV);Bordered),
   Width is EH-SH,
  (ignore((IsBordered,dash_border(Width)))),
  nop(get_bgc(BG)),
  forall(between(SV,EV,V),
   ((format('~N'),
     forall(between(SH,EH,H), 
     (hv_value_or(Grid,C,H,V,BG)->
        (once(print_g(H,V,C,LoH,LoV,HiH,HiV))))),dash_char(10,' '),format('~N')))),
   format('~N'),!,
  (ignore((IsBordered,dash_uborder(Width)))),!.
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


color_print(C,W):- var(C),!,ansi_format([underline],'~w',[W]),!.
color_print(C-_,W):- !, color_print(C,W).
color_print(C,W):- atom(C),color_int(C,N),integer(N),!,color_print(N,W).
color_print(C,W):- integer(C),C\==0,block_colors(L),nth0(C,L,Color),ansi_format([bold,fg(Color)],'~w',[W]),!.
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
is_grid_color(C-_):- is_color(C).
%is_grid_color(C):- is_color(C).

grid_color_code(C,C):- var(C).
grid_color_code(C-W,CC-W):- color_code(C,CC).
grid_color_code(C,CC-[]):- color_code(C,CC).
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
%get_code_at(_,_,0,_):-!,fail.
i_code_at(_,_,C-N,NC,Code):- nonvar(N),!,=(N,Code),number(Code),NC=C.

i_code_at(H,V,C,NC,Code):- nonvar(C), get_grid_num_xyc(H,V,Was,N),nonvar(Was),C\==Was,=(N,Code),nonvar(Code),NC=Was,!.
i_code_at(H,V,C,NC,Code):- var(C), get_grid_num_xyc(H,V,Was,N),nonvar(Was),C\==Was,=(N,Code),nonvar(Code),NC=Was,!.

i_code_at(_,_,C,C,VAR):- var(C),var_dot(VAR),!.
i_code_at(_,_,0,0,BGD):- bg_dot(BGD),!.
i_code_at(_,_,C,C,FGD):- fg_dot(FGD),!.

print_g1(H,V,C0):- i_code_at(H,V,C0,C,Code),name(S,[Code]),!, color_print(C,S),!.
print_g1(_,_,C):- var(C), color_print(C,'?'),!.
%print_g1(_,_,C):- trace, write(C).

i_sym(N,Code):- var(N), Code = 63.
i_sym(N,Code):- change_code(N,NN), i_syms(Codes),nth0(NN,Codes,Code),!.
%change_code(N,M):- M is N * 100,!.
%change_code(N,M):- N>10, M is (N * 10 ),!.
change_code(N,N).

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

:- save_codes(42600).


get_glyph(Point,Glyph):-  
  get_grid_num(Point,N),
  i_sym(N,Code),name(Glyph,[Code]).

i_glyph(N,Glyph):- i_sym(N,Code),name(Glyph,[Code]).

get_grid_num(C-Point,N):-
  hv_point(X,Y,Point),
  get_grid_num_xyc(X,Y,C,N).

get_grid_num(Point,N):-
  hv_point(X,Y,Point),
  get_grid_num_xyc(X,Y,_C,N).

get_grid_num_xyc(X,Y,C,N):- fail,
  hv_point(X,Y,Point),
  nb_current(test_name_w_type,NameType),
  grid_nums(NameType,Grids),nth0(N,Grids,E),
  once(member(C-Point,E);member(Point,E);hv_value(E,C,X,Y)).

:- thread_local(grid_nums/2).
:- thread_local(grid_nums/1).
set_grid_nums(Gs):- 
   nb_current(test_name_w_type,NameType),
   asserta(grid_nums(NameType,Gs)).

