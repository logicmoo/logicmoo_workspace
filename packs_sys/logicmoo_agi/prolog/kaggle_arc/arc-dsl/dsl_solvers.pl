
read_dsl_test:-  make,     
      setup_call_cleanup(open('arc-dsl/solvers.py',read,In2,[]),read_python(In2),close(In2)),
      uast,!,
      setup_call_cleanup(open('arc-dsl/constants.py',read,In1,[]),read_python(In1),close(In1)),
      !.
read_michod_test:- read_sols,read_dsl.

uast:- uast_test.
uast_test:-setup_call_cleanup(open('arc-dsl/dsl.py.uast',read,In3,[]),read_uast_python(In3),close(In3)).

read_uast_python(In):- %json_read(In,AST),
 must_det_ll((
  notrace(((read_stream_to_codes(In,Codes),
  text_to_string(Codes,String),
  replace_in_string(['{'='[','}'=']','"'='\'',
    "'@type':" = "",
        '\n'=' ','\r'=' ','\t'=' ',
        '        '=' ','   '=' ','  '=' ', '  '=' ','  '=' ','  '=' ','  '=' ','  '=' ','  '=' ','  '=' ','  '=' ','  '=' ','  '=' ','  '=' ','  '=' ','  '=' ','  '=' ','  '=' ',
        ', }'=' }',', ]'=' ]'],String,SAST),!,
  %catch((atom_to_term(SAST,AST,Vs),maplist(call,Vs)),_,fail),
  atom_to_term(SAST,AST,Vs),maplist(call,Vs)))),
  do_uast_python(AST))).

remove_i('<>').
%remove_i(AB):- eq_swap(AB,BA),remove_ii(BA),!.
remove_i(AB):- remove_ii(AB),!.


eq_swap(AB,AB).
eq_swap(A:B,B:A):-!.

remove_ii('@pos':_).
remove_ii('@role':_).
remove_ii('else_stmts': []).
remove_ii(_ : []).
remove_ii(keywords:[]).

remove_ii(_ :['python:If.orelse']).
remove_ii(_ :['python:For.orelse']).

remove_ii('Format': '').

remove_ii('MapVariadic': false).
remove_ii('Receiver': false).
remove_ii('Variadic': false).
remove_ii(async:false).

remove_ii(is_async:0).

remove_ii('Name': '~').
remove_ii('Type': '~').
remove_ii(_: '~').

remove_ii(ctx:LoadSave):-load_store(LoadSave).

remove_ii('if' : '@token').
remove_ii('for' : '@token').
%remove_ii('uast:Block' : '@type').
remove_ii('@token':LCase):-  lowercase_word(LCase).
%remove_ii('@token',LCase):-  freeze(LCase,lowercase_word(LCase)).

lowercase_word(LCase):- atomic(LCase),downcase_atom(LCase,LCase), \+ upcase_atom(LCase,LCase).

%s_ast([List],[O]):- !, s_ast0(List,O),!, O\=@=List.
%s_ast(List,O):- !, s_ast0(List,O),!, O\=@=List.

%s_ast0([List],[O]):- s_ast0(List,O),!.
%s_ast(ListIn,O):- is_list(ListIn),!, ss_ast([S|List],O),is_list(List),permutation(List,Prem),maplist(eq_swap,Prem,I),ListIn=I,!.
%s_ast0(ListIn,O):- is_list(ListIn),!, ss_ast([S|List],O) ,permutation(List,Prem),eq_swap(S:'@type',W),ListIn=[W|Prem],!.  permutation(List,Prem),
s_ast(identifier_name(A),S):- atom_string(A,S),!.
s_ast(S,O):- ss_ast(S,O).
s_ast(('Name': 'None'),[]).
s_ast(['uast:Identifier',[]],[]).

s_ast(S,O):- basic_wrapper(F),M=..[F,O],S=M, \+ atom(O),!.
%s_ast0(Swap,O):- eq_swap(Swap,Term),ss_ast(Term,O),!.

%swap_colons(A:B,Swap):- freeze(Swap,eq_swap(A:B,Swap)),!.

tokenX(X:'@token',X):-X\=='@token'.
tokenX('@token':X,X):-X\=='@token'.

load_store('Load').
load_store('Store').

ss_ast('python:BoxedName'(boxed_value:'uast:Identifier'('Name':ToIndices),ctx:'Load'),ToIndices).
ss_ast('python:Assign'(targets:To,value:From),setvars(To,From)).
ss_ast('python:Call'(args:Args,func:Call),eval(Call,Args)).

ss_ast('uast:Identifier'('Name':X),X):-!.
ss_ast('python:Num'(TokenX),X):- tokenX(TokenX,X).
ss_ast('python:Add'(TokenX),X):- tokenX(TokenX,X).

ss_ast('python:Tuple'(ctx:LoadSave,elts:X),X):-load_store(LoadSave).
ss_ast('python:BoxedName'(boxed_value:X,ctx:LoadSave),X):-load_store(LoadSave).
%ss_ast(body_stmts:X,X).
%ss_ast('Statements':X,X).

basic_wrapper(boxed_name_value).
basic_wrapper(boxed_str_value).
basic_wrapper(index_value).
basic_wrapper(num_token).
basic_wrapper(tuple_elts).


not_excluded(X):- \+ remove_i(X),!.

simplify_ast(I,O):- var(I),!,O=I.
simplify_ast(I,O):- \+ compound(I),!,O=I.
simplify_ast(I,O):- s_ast(I,O),!.
%simplify_ast(M:I,M:O):- s_ast(I,O),!.
simplify_ast(M:I,M:O):- !, simplify_ast(I,O).
simplify_ast([I|J],[II|JJ]):- once(include(not_excluded,[I|J],[II|JJ])), [I|J]\=@=[II|JJ],!.
  %simplify_ast([II|JJ],IJO).
simplify_ast([I|J],[II|JJ]):- once(maplist(d_simplify_ast,[I|J],[II|JJ])), [I|J]\=@=[II|JJ],!.

simplify_ast([I,A1|J],IJ):- atom(I), reassemble_arumwents(I,[A1|J],IJ).

%simplify_ast([I|J],[II|JJ]):- once(maplist(d_simplify_ast,[I|J],[II|JJ])), [I|J]\=@=[II|JJ],!.
%simplify_ast(I,'<>'):- I\=='<>',remove_i(I),!.
/*simplify_ast([A,B,I|J],[II|JJ]):- s_ast([A,B,I],II),!,simplify_ast(J,JJ).
simplify_ast([A,I|J],[II|JJ]):- s_ast([A,I],II),!,simplify_ast(J,JJ).
simplify_ast([Rm|I],O):- remove_i(Rm),!,simplify_ast(I,O).
*/
simplify_ast([I|J],[I|J]):- !.
simplify_ast(I,O):- compound_name_arguments(I,F,II),maplist(d_simplify_ast,II,OO),!,
  reassemble_arumwents(F,OO,O).

reassemble_arumwents(F,OO,O):- %%compound_name_arguments(O,F,OO).
  make_kw(F,FKW),
  grab_kwords(OO,FOO),
  flatten(FOO,FOOF),
  predsort(sort_on(nth_kw_order(F)),FOOF,SortedFOO),
  maplist(arg(1),SortedFOO,SortedKW),
  maplist(arg(2),SortedFOO,SortedVals),
  flatten([FKW|SortedKW],KFOOF),
  list_to_set(KFOOF,SetKW),
  atomic_list_concat(SetKW,'_',SFOO),
  make_kw(SFOO,SFOOS),
  O=..[SFOOS|SortedVals].


make_kw(I,O):- is_list(I),!,maplist(make_kw,I,M),flatten(M,F),list_to_set(F,Set),atomic_list_concat(Set,'_',O).
make_kw(I,O):-atomic_list_concat([_,B],  ':',I),!,make_kw(B,O).
make_kw(I,O):-atomic_list_concat(['',B], '@',I),!,make_kw(B,O).
make_kw(I,O):-atomic_list_concat([_,B],  '.',I),!,make_kw(B,O).
make_kw(I,O):-atomic_list_concat([A,B|C],'_',I),!,list_to_set([A,B|C],Set),atomic_list_concat(Set,'_',O).
make_kw(I,O):-string_to_functor(I,O),!.

% grab_kwords(F,OO,FOO,Values):-

grab_kwords([(K:V)|FOO],[KW=V|OUT]):-!,
  make_kw(K,KW),
  grab_kwords(FOO,OUT),!.
grab_kwords([],[]).

nth_kw_order(F,Kw:_,Nth4):- !, nth_kw_order(F,Kw,Nth4).
nth_kw_order(F,Kw=_,Nth4):- !, nth_kw_order(F,Kw,Nth4).
nth_kw_order(F,Kw,Nth4):- kw_order(F,List),nth1(Nth1,List,E),E==Kw,!,Nth4 is 4 * Nth1.
nth_kw_order(_,_,10).

kw_order('python:Subscript',[value,slice]).
kw_order(_,[op,left,right]).
kw_order(_,[is,op,values,value]).
kw_order(_,[ops,left,comparators]).
kw_order(_,[func,args,ctx]).
kw_order(_,[while,for,target,iter,if,test,else,token,stmts,body,orelse]).
kw_order(_,[targets,value]).
kw_order(_,[return,value]).

d_simplify_ast(AST,ASTO):- once(simplify_ast(AST,AST1)),%writeln(AST1),
  AST1\=@=AST,!,d_simplify_ast(AST1,ASTO).
d_simplify_ast(AST,AST).

do_uast_python(AST):- 
 notrace((d_simplify_ast(AST,AST11),d_simplify_ast(AST11,AST1), writeq(AST1),nl,nl,print(AST1),nl,nl)),!,
  must_det_ll((compile_uast_python(AST1,OUT),print(OUT))).
  


compile_uast_python(AST,[]):- \+ compound(AST),!,nop(dmsg(compile_uast_python(AST,_))).
compile_uast_python(AST,OUT):- is_list(AST),!,maplist(compile_uast_python,AST,OUTE),flatten_code(OUTE,OUT),!.
compile_uast_python(alias_name_node(Name,AST),OUT):- !, do_alias_name_node(Name,AST,OUTE),flatten_code(OUTE,OUT),!.
compile_uast_python(AST,OUT):- compound_name_arguments(AST,_,ARGS),!,maplist(compile_uast_python,ARGS,OUTE),
   flatten_code(OUTE,OUT).

   
flatten_code(OUTE,OUTO):- flatten([OUTE],OUTL),maplist(conjuncts_to_list,OUTL,OUTL1),flatten(OUTL1,OUTO).

do_alias_name_node(Name,function_type_body(function_type_arguments_returns(IN,OUT),ASTNode),[Head:-Body]):- 
  pp(def(Name,function_type_body(function_type_arguments_returns(IN,OUT),ASTNode))),
  must_det_ll((
   atom_string(AName,Name),
     compile_to_head_args(IN,HEADARGS,Replace,Code1),
     subst_2L(Replace,HEADARGS,ASTNode+OUT,NewAST+NewOUT),
     compile_to_head_args(NewOUT,LASTHEADARG,Replace,Code2),
     append([AName|HEADARGS],[LASTHEADARG],HEADL),
   Head =.. HEADL,
   compile_using_return_arg(LASTHEADARG,NewAST,Code3),
   flatten_code([Code1,Code2,Code3],Body))).

compile_using_return_arg(_LASTHEADARG,NewAST,NewAST).

compile_to_head_args(List,Args,Replaces,Code):- is_list(List),!,
  maplist(compile_to_head_args,List,Args,Replaces,CodeE),flatten_code(CodeE,Code),!.
compile_to_head_args(argument_name(SName),'$VAR'(Name),SName,[true]):- atom_string(DName,SName),upcase_atom(DName,Name).
compile_to_head_args(argument_type(SName),'$VAR'(Name),OSName,[isType('$VAR'(Name),DName)]):-
   (var(OSName)->SName= OSName;true),
   atom_string(DName,SName),
   atom_string(ODName,OSName),upcase_atom(ODName,Name).
  /*

  vsplit(Grid,N,Tuple):-
    len(Grid,GridLen),
    H #= GridLen // N,
    subscript_value_slice(Grid,0,GridSlice0),
    len(GridSlice0,GridSlice0Len),
    W #= GridSlice0Len,
    Step1 is GridLen % N,
    into_integer(Step1 != 0, Offset),    
    for_range(I,N,      
       HS #= H * I + I * Offset,
       crop(Grid, [HS, 0], [H, W], G),
       create_tupple(T,Tuple)). 

    

              
                                            alias_name_node( "vsplit",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("grid"),
                                                      argument_name("n")],
                                                    [argument_type("Tuple")]),
                                                 block_statements(/*a1*/ [ expr_value(string_value(' split grid vertically ')),
                                                                           assign_targets_value( [ [ "h","w"]], [
                                                                             bin_op_left_right(floor_div_token(//),call_func_args("len",["grid"]),"n"),
                                                                             call_func_args("len",[subscript_value_slice("grid",0)])]),
                                                                           assign_targets_value( ["offset"],
                                                                             compare_ops_left_comparators( ops([not_eq_token('!=')]),
                                                                               bin_op_left_right(mod_token('%'),call_func_args("len",["grid"]),"n"),
                                                                               comparators([0]))),
                                                                           return_value(/*a1*/ call_func_args( "tuple", [
                                                                                                 generator_exp_elt_generators(
                                                                                                    call_func_args( "crop", [
                                                                                                      "grid",
                                                                                                      [ bin_op_left_right(add_token(+),bin_op_left_right(mult_token(*),"h","i"),bin_op_left_right(mult_token(*),"i","offset")),
                                                                                                        0],
                                                                                                      ["h","w"]]),
                                                                                                    [ comprehension_target_iter("i",call_func_args("range",["n"]))])]))])))]),

*/
:- dynamic(michod_solved/2).
:- dynamic(py_consts/2).

:- dynamic(michod_dsl/4).
print_sols:- make, T = michod_solved(TestID,Prog),
   %forall(T,(print(fun(TestID)),maplist(writeln,Prog),nl)).
   %forall(T,(michod_solved_cor(TestID,Prog,Clause),portray_clause(user_output,Clause))).
   forall(T,(michod_solved_cor(TestID,Prog,Clause),print(Clause))).
print_dsl:- T = michod_dsl(Fun,Params,_Cmt,Src),
   forall(T,(print(fun(Fun,Params)),maplist(writeln,Src),nl)).
read_python(In):- 
 repeat, 
  read_line_to_string(In,S), 
   (S==end_of_file -> ! ; once(process_line(In,S)),fail).

%py_substs(N,V):- between(0,60,X),atom_concat(x,X,N),V='$VAR'(X).
py_substs(N,V):- between(0,60,X),atom_concat(x,X,N),atom_concat('X',X,XX),V='$VAR'(XX).
py_substs('T',true).
py_substs('F',false).
py_substs(N,V):- py_consts(N,V).
py_substs(C,(X,Y)):- fff(C,X,Y).
py_substs('IntegerTuple','Point').
fff('DOWN',1, 0).
fff('RIGHT',0, 1).
fff('UP',-1, 0).
fff('LEFT',0, -1).
fff('ORIGIN',0, 0).
fff('UNITY',1, 1).
fff('NEG_UNITY',-1, -1).
fff('UP_RIGHT',-1, 1).
fff('DOWN_LEFT',1, -1).
fff('ZERO_BY_TWO',0, 2).
fff('TWO_BY_ZERO',2, 0).
fff('TWO_BY_TWO',2, 2).
fff('THREE_BY_THREE',3, 3).

%michod_solved_cor(TestID,Prog):- michod_solved(TestID,Prog).
michod_solved_cor(TestID,Prog,Clause):- 
  michod_solved(TestID,Prog),
  Head=..[solve,TestID,'I','O'],
  
  maplist(predifiy,Prog,Preds),
  findall(N-V,py_substs(N,V),Substs),
  subst_1L(['O'-'$VAR'('OUT'),'I'-'$VAR'('IN')|Substs],(Head:-Preds),Clause).

predifiy(X=Y,Preds):- Y=..[F|A],append(A,[X],O),
   get_typearray(F,AT),add_argType(O,AT,OO),Preds=..[f,F|OO].

get_typearray(F,[]):- var(F),!. 
get_typearray(F,AT):- michod_dsl(F,AT,_,_),!.
get_typearray(_F,[]).

add_argType([O|OO],[AT|ATT],[ATO|ATOO]):-
  add_1argType(O,AT,ATO),!,
  add_argType(OO,ATT,ATOO).
add_argType(A,_,A).

add_1argType(O,(A:'Boolean'),A=O):-!.
add_1argType(O,('returns':T),O:out(T)):-!.
add_1argType(O,(A:'Boolean,'),A=O):-!.
add_1argType(O,'Callable',O):- downcase_atom(O,O).
add_1argType(O,function:'Callable',O):- downcase_atom(O,O).
add_1argType(O,A:T,OT):- atom(A),atom(T),upcase_atom(A,UA),upcase_atom(T,UA),!,add_1argType(O,T,OT).
add_1argType(O,AT,O:AT):-!.
add_1argType(A,_,A).

read_solve(In,Prog):- read_line_to_string(In,Str),read_solve(In,Str,Prog).
read_solve(In,Str,[Var=Fun|Prog]):- str_between([s(Var),' = ',s(Fun)],Str),read_solve(In,Prog).
read_solve(_,Str,[]):- str_between(['return O',_],Str).

process_line(In,end_of_file):- close(In).
process_line(_, Str):- str_between(['import',_],Str),!.
process_line(_, Str):- str_between(['from',_],Str),!.
%process_line(_, Str):- str_between([s(N),'=',s(V)],Str),asserta_new(py_const(N,V)),print(N=V).
process_line(_, Str):- str_between([''],Str),!.
process_line(In,Str):- str_between(['def solve_',s(TestID),'(I)',_],Str),!,
  read_solve(In,Prog),
  assertz_new(michod_solved(TestID,Prog)).

process_line(In,Str):- str_between(['def ',s(Fun),'(',_],Str),
 read_params(In,Params),!,
 %read_cmt(In,Cmt),!, 
  read_until_blank(In,Src),
  writeln(Fun+Params=Src),
  assertz_new(michod_dsl(Fun,Params,_Cmt,Src)).

ltrim(Str,Rest):- string_concat('\t',Rest,Str). ltrim(Str,Rest):- string_concat(' ',Rest,Str). ltrim(Str,Str).
rtrim(Str,Rest):- string_concat(Rest,'\t',Str). rtrim(Str,Rest):- string_concat(Rest,' ',Str). rtrim(Str,Str).

trim(Str,StrO):- Str==end_of_file,!,StrO="".
trim(Str,StrO):- ltrim(Str,StrM),rtrim(StrM,StrO).

str_between([Const|Template],Str):- ground(Const),string_concat(Const,Rest,Str),!,str_between(Template,Rest).
str_between([Const|Template],Str):- ground(Const),trim(Str,Rest),Rest\==Str,!,str_between([Const|Template],Rest).
str_between([s(Var)],Str):- var(Var),!,read_funct(Str,Var),!.
str_between([Var],Str):- var(Var),Str=Var,!.

str_between([s(Var),Const|Template],Str):- var(Var),ground(Const),sub_string(Str,Left,_,_,Const),
  sub_string(Str,0,Left,_,SVar),read_funct(SVar,Var),!,string_concat(SVar,Rest,Str),str_between([Const|Template],Rest).
str_between([SVar,Const|Template],Str):- var(SVar),ground(Const),sub_string(Str,Left,_,_,Const),
  sub_string(Str,0,Left,_,SVar),string_concat(SVar,Rest,Str),str_between([Const|Template],Rest).
%str_between(Template,Str):- trim(Str,S),S\==Str,!,str_between(Template,Str).

%writeln(params:Str),
read_params(In,Prog):- read_line_to_string(In,Str), trim(Str,StrS),read_params(In,StrS,Prog).

read_params(_,Str,['returns':Type]):- str_between([') ->',s(Type),':',_],Str),!.
read_params(In,Str,[Name:Type|Prog]):- str_between([s(Name),':',s(Type),','],Str),!,read_params(In,Prog).
read_params(In,Str,[Name:Type|Prog]):- str_between([s(Name),':',s(Type)],Str),!,read_params(In,Prog).
read_cmt(In,Cmt):- read_line_to_string(In,Str), str_between(['"""',SType,'"""'],Str),trim(SType,Cmt).

read_until_blank(In,Src):- read_line_to_string(In,Str),trim(Str,T),
 (T=="" -> Src=[] ; (read_until_blank(In,More),Src=[Str|More])).

read_funct(SFun,Fun):- trim(SFun,SS),
 trim_comma(SS,S),catch((atom_to_term(S,Fun,Vs),maplist(call,Vs)),_,atom_string(Fun,S)).

trim_comma(SS,S):- atom_concat(S,',',SS),!.
trim_comma(S,S).




end_of_file.



