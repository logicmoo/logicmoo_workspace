
read_dsl:-  make,     
      open('arc-dsl/solvers.py',read,In2,[]),read_python(In2),!,
      uast,!,
      open('arc-dsl/constants.py',read,In1,[]),read_python(In1),
      !.
read_michod:- read_sols,read_dsl.

uast:- open('arc-dsl/dsl.py.uast',read,In3,[]),read_uast_python(In3),close(In3).
read_uast_python(In):- %json_read(In,AST),
  %read_term(In,AST,[]),
  read_stream_to_codes(In,Codes),
  text_to_string(Codes,String),
  replace_in_string(['{'='[','}'=']','"'='\'',
    "'@type':" = "",
        '\n'=' ','\r'=' ','\t'=' ','        '=' ','   '=' ','  '=' ',
          '  '=' ','  '=' ','  '=' ','  '=' ','  '=' ','  '=' ','  '=' ','  '=' ','  '=' ','  '=' ','  '=' ','  '=' ','  '=' ','  '=' ','  '=' ',
        ', }'=' }',', ]'=' ]'],String,SAST),!,
  %catch((atom_to_term(SAST,AST,Vs),maplist(call,Vs)),_,fail),
  atom_to_term(SAST,AST,Vs),maplist(call,Vs),
  do_uast_python(AST).

remove_i('<>').
%remove_i(AB):- eq_swap(AB,BA),remove_ii(BA),!.
remove_i(AB):- remove_ii(AB),!.

de_type('@type':X,X):-!. de_type(X:'@type',X):-!.

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
%s_ast([S|ListIn],O):- !, de_type(S,W), ss_ast([W|ListIn],O).
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

do_uast_python(AST):- d_simplify_ast(AST,AST11),d_simplify_ast(AST11,AST1), writeq(AST1),!,nl,nl,print(AST1),nl,nl,
  compile_uast_python(OUT,AST1),
  


compile_uast_python(C,[]):- \+ compound(C),nop(dmsg(compile_uast_python(C,_))).
compile_uast_python(C,OUT):- is_list(C),!,maplist(compile_uast_python,C,OUTE),flatten_code(OUTE,OUT)
compile_uast_python(alias_name_node(Name,AST),OUT):- !, do_alias_name_node(Name,AST,OUT).
compile_uast_python(AST,OUT):- compound_name_arguments(AST,_,ARGS),!,maplist(compile_uast_python,ARGS,OUTE),
   flatten_code(OUTE,OUT).

flatten_code(OUTE,OUT):- flatten(OUTE,OUTL),list_to_conjuct(OUTL,OUT).

do_alias_name_node(Name,function_type_body(In,OUT,ASTNode),[Head:-Body]):- 
  pp(def(Name,function_type_body(function_type_arguments_returns(IN,OUT),ASTNode)), atom_string(AName,Name),
     maplist(compile_to_head_args,IN,HEADARGS,Replace,Code1),
     subst_2L(Replace,HEADARGS,ASTNode+OUT,NewAST+NewOUT),
     compile_to_return_arg(NewOUT,LASTHEADARG,Code2),
     append([AName|HEADARGS],[LASTHEADARG],HEADL),
   Head =.. HEADL,
   compile_to_return_arg(LASTHEADARG,NewAST,Code3),
   flatten_code([Code1,Code2,Code3],Body).

compile_to_head_args(argument_name(SName),HEADARG,Replace,Code1):-
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


?- uast.


module_body([ function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "identity",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("x")],[argument_type("Any")]),
                                                 block_statements([expr_value(string_value(' identity function ')),return_value("x")])))]),
              function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "add",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("a"),
                                                      argument_name("b")],
                                                    [argument_type("Numerical")]),
                                                 block_statements( [ expr_value(string_value(' addition ')),
                                                                           if_test_body_orelse(
                                                                              bool_op_values( ['python:And'], [
                                                                                call_func_args("isinstance",["a","int"]),
                                                                                call_func_args("isinstance",["b","int"])]),
                                                                              body_stmts([return_value(bin_op_left_right(add_token(+),"a","b"))]),
                                                                              orelse_else_stmts( [ if_test_body_orelse(
                                                                                                            bool_op_values( ['python:And'], [
                                                                                                              call_func_args("isinstance",["a","tuple"]),
                                                                                                              call_func_args("isinstance",["b","tuple"])]),
                                                                                                            body_stmts( [ return_value( [ bin_op_left_right(add_token(+),subscript_value_slice("a",0),subscript_value_slice("b",0)),
                                                                                                                                                      bin_op_left_right(add_token(+),subscript_value_slice("a",1),subscript_value_slice("b",1))])])
                                                                                                         ,  orelse_else_stmts( [ if_test_body(
                                                                                                                                          bool_op_values( ['python:And'], [
                                                                                                                                            call_func_args("isinstance",["a","int"]),
                                                                                                                                            call_func_args("isinstance",["b","tuple"])]),
                                                                                                                                          body_stmts( [ return_value( [ bin_op_left_right(add_token(+),"a",subscript_value_slice("b",0)),
                                                                                                                                                                                    bin_op_left_right(add_token(+),"a",subscript_value_slice("b",1))])])
                                                                                                                                       )])
                                                                                                         )])
                                                                         ),
                                                                           return_value( [ bin_op_left_right(add_token(+),subscript_value_slice("a",0),"b"),
                                                                                                 bin_op_left_right(add_token(+),subscript_value_slice("a",1),"b")])
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "subtract",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("a"),
                                                      argument_name("b")],
                                                    [argument_type("Numerical")]),
                                                 block_statements( [ expr_value(string_value(' subtraction ')),
                                                                           if_test_body_orelse(
                                                                              bool_op_values( ['python:And'], [
                                                                                call_func_args("isinstance",["a","int"]),
                                                                                call_func_args("isinstance",["b","int"])]),
                                                                              body_stmts([return_value(bin_op_left_right(sub_token(-),"a","b"))]),
                                                                              orelse_else_stmts( [ if_test_body_orelse(
                                                                                                            bool_op_values( ['python:And'], [
                                                                                                              call_func_args("isinstance",["a","tuple"]),
                                                                                                              call_func_args("isinstance",["b","tuple"])]),
                                                                                                            body_stmts( [ return_value( [ bin_op_left_right(sub_token(-),subscript_value_slice("a",0),subscript_value_slice("b",0)),
                                                                                                                                                      bin_op_left_right(sub_token(-),subscript_value_slice("a",1),subscript_value_slice("b",1))])])
                                                                                                         ,  orelse_else_stmts( [ if_test_body(
                                                                                                                                          bool_op_values( ['python:And'], [
                                                                                                                                            call_func_args("isinstance",["a","int"]),
                                                                                                                                            call_func_args("isinstance",["b","tuple"])]),
                                                                                                                                          body_stmts( [ return_value( [ bin_op_left_right(sub_token(-),"a",subscript_value_slice("b",0)),
                                                                                                                                                                                    bin_op_left_right(sub_token(-),"a",subscript_value_slice("b",1))])])
                                                                                                                                       )])
                                                                                                         )])
                                                                         ),
                                                                           return_value( [ bin_op_left_right(sub_token(-),subscript_value_slice("a",0),"b"),
                                                                                                 bin_op_left_right(sub_token(-),subscript_value_slice("a",1),"b")])
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "multiply",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("a"),
                                                      argument_name("b")],
                                                    [argument_type("Numerical")]),
                                                 block_statements( [ expr_value(string_value(' multiplication ')),
                                                                           if_test_body_orelse(
                                                                              bool_op_values( ['python:And'], [
                                                                                call_func_args("isinstance",["a","int"]),
                                                                                call_func_args("isinstance",["b","int"])]),
                                                                              body_stmts([return_value(bin_op_left_right(mult_token(*),"a","b"))]),
                                                                              orelse_else_stmts( [ if_test_body_orelse(
                                                                                                            bool_op_values( ['python:And'], [
                                                                                                              call_func_args("isinstance",["a","tuple"]),
                                                                                                              call_func_args("isinstance",["b","tuple"])]),
                                                                                                            body_stmts( [ return_value( [ bin_op_left_right(mult_token(*),subscript_value_slice("a",0),subscript_value_slice("b",0)),
                                                                                                                                                      bin_op_left_right(mult_token(*),subscript_value_slice("a",1),subscript_value_slice("b",1))])])
                                                                                                         ,  orelse_else_stmts( [ if_test_body(
                                                                                                                                          bool_op_values( ['python:And'], [
                                                                                                                                            call_func_args("isinstance",["a","int"]),
                                                                                                                                            call_func_args("isinstance",["b","tuple"])]),
                                                                                                                                          body_stmts( [ return_value( [ bin_op_left_right(mult_token(*),"a",subscript_value_slice("b",0)),
                                                                                                                                                                                    bin_op_left_right(mult_token(*),"a",subscript_value_slice("b",1))])])
                                                                                                                                       )])
                                                                                                         )])
                                                                         ),
                                                                           return_value( [ bin_op_left_right(mult_token(*),subscript_value_slice("a",0),"b"),
                                                                                                 bin_op_left_right(mult_token(*),subscript_value_slice("a",1),"b")])
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "divide",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("a"),
                                                      argument_name("b")],
                                                    [argument_type("Numerical")]),
                                                 block_statements( [ expr_value(string_value(' floor division ')),
                                                                           if_test_body_orelse(
                                                                              bool_op_values( ['python:And'], [
                                                                                call_func_args("isinstance",["a","int"]),
                                                                                call_func_args("isinstance",["b","int"])]),
                                                                              body_stmts([return_value(bin_op_left_right(floor_div_token(//),"a","b"))]),
                                                                              orelse_else_stmts( [ if_test_body_orelse(
                                                                                                            bool_op_values( ['python:And'], [
                                                                                                              call_func_args("isinstance",["a","tuple"]),
                                                                                                              call_func_args("isinstance",["b","tuple"])]),
                                                                                                            body_stmts( [ return_value( [ bin_op_left_right(floor_div_token(//),subscript_value_slice("a",0),subscript_value_slice("b",0)),
                                                                                                                                                      bin_op_left_right(floor_div_token(//),subscript_value_slice("a",1),subscript_value_slice("b",1))])])
                                                                                                         ,  orelse_else_stmts( [ if_test_body(
                                                                                                                                          bool_op_values( ['python:And'], [
                                                                                                                                            call_func_args("isinstance",["a","int"]),
                                                                                                                                            call_func_args("isinstance",["b","tuple"])]),
                                                                                                                                          body_stmts( [ return_value( [ bin_op_left_right(floor_div_token(//),"a",subscript_value_slice("b",0)),
                                                                                                                                                                                    bin_op_left_right(floor_div_token(//),"a",subscript_value_slice("b",1))])])
                                                                                                                                       )])
                                                                                                         )])
                                                                         ),
                                                                           return_value( [ bin_op_left_right(floor_div_token(//),subscript_value_slice("a",0),"b"),
                                                                                                 bin_op_left_right(floor_div_token(//),subscript_value_slice("a",1),"b")])
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "invert",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("n")],[argument_type("Numerical")]),
                                                 block_statements( [ expr_value(string_value(' inversion with respect to addition ')),
                                                                           return_value( if_exp_test_body_orelse(
                                                                                                  call_func_args("isinstance",["n","int"]),
                                                                                                  unary_op_operand(us_ub_token(-),"n"),
                                                                                                  [ unary_op_operand(us_ub_token(-),subscript_value_slice("n",0)),
                                                                                                    unary_op_operand(us_ub_token(-),subscript_value_slice("n",1))]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "even",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("n")],[argument_type("Boolean")]),
                                                 block_statements( [ expr_value(string_value(' evenness ')),
                                                                           return_value( compare_ops_left_comparators( ops([eq_token(==)]),
                                                                                                 bin_op_left_right(mod_token('%'),"n",2),
                                                                                                 comparators([0])))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "double",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("n")],[argument_type("Numerical")]),
                                                 block_statements( [ expr_value(string_value(' scaling by two ')),
                                                                           return_value( if_exp_test_body_orelse(
                                                                                                  call_func_args("isinstance",["n","int"]),
                                                                                                  bin_op_left_right(mult_token(*),"n",2),
                                                                                                  [ bin_op_left_right(mult_token(*),subscript_value_slice("n",0),2),
                                                                                                    bin_op_left_right(mult_token(*),subscript_value_slice("n",1),2)]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "halve",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("n")],[argument_type("Numerical")]),
                                                 block_statements( [ expr_value(string_value(' scaling by one half ')),
                                                                           return_value( if_exp_test_body_orelse(
                                                                                                  call_func_args("isinstance",["n","int"]),
                                                                                                  bin_op_left_right(floor_div_token(//),"n",2),
                                                                                                  [ bin_op_left_right(floor_div_token(//),subscript_value_slice("n",0),2),
                                                                                                    bin_op_left_right(floor_div_token(//),subscript_value_slice("n",1),2)]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "flip",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("b")],[argument_type("Boolean")]),
                                                 block_statements( [ expr_value(string_value(' logical not ')),
                                                                           return_value(unary_op_operand(['python:Not'],"b"))])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "equality",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("a"),
                                                      argument_name("b")],
                                                    [argument_type("Boolean")]),
                                                 block_statements( [ expr_value(string_value(' equality ')),
                                                                           return_value(compare_ops_left_comparators(ops([eq_token(==)]),"a",comparators(["b"])))])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "contained",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("value"),
                                                      argument_name("container")],
                                                    [argument_type("Boolean")]),
                                                 block_statements( [ expr_value(string_value(' element of ')),
                                                                           return_value( compare_ops_left_comparators(ops([['python:In']]),"value",comparators(["container"])))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "combine",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("a"),
                                                      argument_name("b")],
                                                    [argument_type("Container")]),
                                                 block_statements( [ expr_value(string_value(' union ')),
                                                                           return_value( call_func_args(call_func_args("type",["a"]),[[starred_value("a"),starred_value("b")]]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "intersection",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("a"),
                                                      argument_name("b")],
                                                    [argument_type("FrozenSet")]),
                                                 block_statements( [ expr_value(string_value(' returns the intersection of two containers ')),
                                                                           return_value(bin_op_left_right(bit_and_token(&),"a","b"))])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "difference",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("a"),
                                                      argument_name("b")],
                                                    [argument_type("FrozenSet")]),
                                                 block_statements( [ expr_value(string_value(' set difference ')),
                                                                           return_value(bin_op_left_right(sub_token(-),"a","b"))])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "dedupe",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("tup")],[argument_type("Tuple")]),
                                                 block_statements( [ expr_value(string_value(' remove duplicates ')),
                                                                           return_value( call_func_args( "tuple", [
                                                                                                 generator_exp_elt_generators( "e", [
                                                                                                   comprehension_ifs_target_iter(
                                                                                                      [ compare_ops_left_comparators( ops([eq_token(==)]),
                                                                                                          call_func_args(qualified_identifier_identifiers(["tup",boxed_attribute_value("index")]),["e"]),
                                                                                                          comparators(["i"]))],
                                                                                                      ["i","e"],
                                                                                                      call_func_args("enumerate",["tup"]))])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "order",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("container"),
                                                      argument_name("compfunc")],
                                                    [argument_type("Tuple")]),
                                                 block_statements( [ expr_value(string_value(' order container by custom key ')),
                                                                           return_value( call_func_args( "tuple", [
                                                                                                 call_func_args_keywords("sorted",["container"],[keyword_value("compfunc")])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "repeat",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("item"),
                                                      argument_name("num")],
                                                    [argument_type("Tuple")]),
                                                 block_statements( [ expr_value(string_value(' repetition of item within vector ')),
                                                                           return_value( call_func_args( "tuple", [
                                                                                                 generator_exp_elt_generators("item",[comprehension_target_iter("i",call_func_args("range",["num"]))])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "greater",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("a"),
                                                      argument_name("b")],
                                                    [argument_type("Boolean")]),
                                                 block_statements( [ expr_value(string_value(' greater ')),
                                                                           return_value(compare_ops_left_comparators(ops([gt_token(>)]),"a",comparators(["b"])))])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "size",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("container")],[argument_type("Integer")]),
                                                 block_statements( [ expr_value(string_value(' cardinality ')),
                                                                           return_value(call_func_args("len",["container"]))])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "merge",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("containers")],[argument_type("Container")]),
                                                 block_statements( [ expr_value(string_value(' merging ')),
                                                                           return_value( call_func_args(
                                                                                                  call_func_args("type",["containers"]),
                                                                                                  [ generator_exp_elt_generators( "e", [
                                                                                                      comprehension_target_iter("c","containers"),
                                                                                                      comprehension_target_iter("e","c")])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "maximum",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("container")],[argument_type("Integer")]),
                                                 block_statements( [ expr_value(string_value(' maximum ')),
                                                                           return_value(call_func_args_keywords("max",["container"],[keyword_value(0)]))])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "minimum",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("container")],[argument_type("Integer")]),
                                                 block_statements( [ expr_value(string_value(' minimum ')),
                                                                           return_value(call_func_args_keywords("min",["container"],[keyword_value(0)]))])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "valmax",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("container"),
                                                      argument_name("compfunc")],
                                                    [argument_type("Integer")]),
                                                 block_statements( [ expr_value(string_value(' maximum by custom function ')),
                                                                           return_value( call_func_args( "compfunc", [
                                                                                                 call_func_args_keywords("max",["container"],[keyword_value("compfunc"),keyword_value(0)])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "valmin",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("container"),
                                                      argument_name("compfunc")],
                                                    [argument_type("Integer")]),
                                                 block_statements( [ expr_value(string_value(' minimum by custom function ')),
                                                                           return_value( call_func_args( "compfunc", [
                                                                                                 call_func_args_keywords("min",["container"],[keyword_value("compfunc"),keyword_value(0)])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "argmax",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("container"),
                                                      argument_name("compfunc")],
                                                    [argument_type("Any")]),
                                                 block_statements( [ expr_value(string_value(' largest item by custom order ')),
                                                                           return_value(call_func_args_keywords("max",["container"],[keyword_value("compfunc")]))])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "argmin",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("container"),
                                                      argument_name("compfunc")],
                                                    [argument_type("Any")]),
                                                 block_statements( [ expr_value(string_value(' smallest item by custom order ')),
                                                                           return_value(call_func_args_keywords("min",["container"],[keyword_value("compfunc")]))])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "mostcommon",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("container")],[argument_type("Any")]),
                                                 block_statements( [ expr_value(string_value(' most common item ')),
                                                                           return_value( call_func_args_keywords( "max",
                                                                                                 [ call_func_args("set",["container"])],
                                                                                                 [ keyword_value(qualified_identifier_identifiers(["container",boxed_attribute_value("count")]))]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "leastcommon",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("container")],[argument_type("Any")]),
                                                 block_statements( [ expr_value(string_value(' least common item ')),
                                                                           return_value( call_func_args_keywords( "min",
                                                                                                 [ call_func_args("set",["container"])],
                                                                                                 [ keyword_value(qualified_identifier_identifiers(["container",boxed_attribute_value("count")]))]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "initset",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("value")],[argument_type("FrozenSet")]),
                                                 block_statements( [ expr_value(string_value(' initialize container ')),
                                                                           return_value(call_func_args("frozenset",[set_elts(["value"])]))])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "both",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("a"),
                                                      argument_name("b")],
                                                    [argument_type("Boolean")]),
                                                 block_statements( [ expr_value(string_value(' logical and ')),
                                                                           return_value(bool_op_values(['python:And'],["a","b"]))])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "either",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("a"),
                                                      argument_name("b")],
                                                    [argument_type("Boolean")]),
                                                 block_statements( [ expr_value(string_value(' logical or ')),
                                                                           return_value(bool_op_values(['python:Or'],["a","b"]))])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "increment",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("x")],[argument_type("Numerical")]),
                                                 block_statements( [ expr_value(string_value(' incrementing ')),
                                                                           return_value( if_exp_test_body_orelse(
                                                                                                  call_func_args("isinstance",["x","int"]),
                                                                                                  bin_op_left_right(add_token(+),"x",1),
                                                                                                  [ bin_op_left_right(add_token(+),subscript_value_slice("x",0),1),
                                                                                                    bin_op_left_right(add_token(+),subscript_value_slice("x",1),1)]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "decrement",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("x")],[argument_type("Numerical")]),
                                                 block_statements( [ expr_value(string_value(' decrementing ')),
                                                                           return_value( if_exp_test_body_orelse(
                                                                                                  call_func_args("isinstance",["x","int"]),
                                                                                                  bin_op_left_right(sub_token(-),"x",1),
                                                                                                  [ bin_op_left_right(sub_token(-),subscript_value_slice("x",0),1),
                                                                                                    bin_op_left_right(sub_token(-),subscript_value_slice("x",1),1)]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "crement",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("x")],[argument_type("Numerical")]),
                                                 block_statements( [ expr_value(string_value(' incrementing positive and decrementing negative ')),
                                                                           if_test_body(
                                                                              call_func_args("isinstance",["x","int"]),
                                                                              body_stmts( [ return_value( if_exp_test_body_orelse(
                                                                                                                         compare_ops_left_comparators(ops([eq_token(==)]),"x",comparators([0])),
                                                                                                                         0,
                                                                                                                         if_exp_test_body_orelse(
                                                                                                                            compare_ops_left_comparators(ops([gt_token(>)]),"x",comparators([0])),
                                                                                                                            bin_op_left_right(add_token(+),"x",1),
                                                                                                                            bin_op_left_right(sub_token(-),"x",1))))])
                                                                         ),
                                                                           return_value( [ if_exp_test_body_orelse(
                                                                                                    compare_ops_left_comparators(ops([eq_token(==)]),subscript_value_slice("x",0),comparators([0])),
                                                                                                    0,
                                                                                                    if_exp_test_body_orelse(
                                                                                                       compare_ops_left_comparators(ops([gt_token(>)]),subscript_value_slice("x",0),comparators([0])),
                                                                                                       bin_op_left_right(add_token(+),subscript_value_slice("x",0),1),
                                                                                                       bin_op_left_right(sub_token(-),subscript_value_slice("x",0),1))),
                                                                                                 if_exp_test_body_orelse(
                                                                                                    compare_ops_left_comparators(ops([eq_token(==)]),subscript_value_slice("x",1),comparators([0])),
                                                                                                    0,
                                                                                                    if_exp_test_body_orelse(
                                                                                                       compare_ops_left_comparators(ops([gt_token(>)]),subscript_value_slice("x",1),comparators([0])),
                                                                                                       bin_op_left_right(add_token(+),subscript_value_slice("x",1),1),
                                                                                                       bin_op_left_right(sub_token(-),subscript_value_slice("x",1),1)))])
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "sign",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("x")],[argument_type("Numerical")]),
                                                 block_statements( [ expr_value(string_value(' sign ')),
                                                                           if_test_body(
                                                                              call_func_args("isinstance",["x","int"]),
                                                                              body_stmts( [ return_value( if_exp_test_body_orelse(
                                                                                                                         compare_ops_left_comparators(ops([eq_token(==)]),"x",comparators([0])),
                                                                                                                         0,
                                                                                                                         if_exp_test_body_orelse(
                                                                                                                            compare_ops_left_comparators(ops([gt_token(>)]),"x",comparators([0])), 1,unary_op_operand(us_ub_token(-),1))))])
                                                                         ),
                                                                           return_value( [ if_exp_test_body_orelse(
                                                                                                    compare_ops_left_comparators(ops([eq_token(==)]),subscript_value_slice("x",0),comparators([0])),
                                                                                                    0,
                                                                                                    if_exp_test_body_orelse(
                                                                                                       compare_ops_left_comparators(ops([gt_token(>)]),subscript_value_slice("x",0),comparators([0])), 1,unary_op_operand(us_ub_token(-),1))),
                                                                                                 if_exp_test_body_orelse(
                                                                                                    compare_ops_left_comparators(ops([eq_token(==)]),subscript_value_slice("x",1),comparators([0])),
                                                                                                    0,
                                                                                                    if_exp_test_body_orelse(
                                                                                                       compare_ops_left_comparators(ops([gt_token(>)]),subscript_value_slice("x",1),comparators([0])), 1,unary_op_operand(us_ub_token(-),1)))])
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "positive",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("x")],[argument_type("Boolean")]),
                                                 block_statements( [ expr_value(string_value(' positive ')),
                                                                           return_value(compare_ops_left_comparators(ops([gt_token(>)]),"x",comparators([0])))])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "toivec",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("i")],[argument_type("IntegerTuple")]),
                                                 block_statements( [ expr_value(string_value(' vector pointing vertically ')),
                                                                           return_value(["i",0])])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "tojvec",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("j")],[argument_type("IntegerTuple")]),
                                                 block_statements( [ expr_value(string_value(' vector pointing horizontally ')),
                                                                           return_value([0,"j"])])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "sfilter",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("container"),
                                                      argument_name("condition")],
                                                    [argument_type("Container")]),
                                                 block_statements( [ expr_value(string_value(' keep elements in container that satisfy condition ')),
                                                                           return_value( call_func_args(
                                                                                                  call_func_args("type",["container"]),
                                                                                                  [ generator_exp_elt_generators( "e", [
                                                                                                      comprehension_ifs_target_iter([call_func_args("condition",["e"])],"e","container")])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "mfilter",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("container"),
                                                      argument_name("function")],
                                                    [argument_type("FrozenSet")]),
                                                 block_statements( [ expr_value(string_value(' filter and merge ')),
                                                                           return_value(call_func_args("merge",[call_func_args("sfilter",["container","function"])]))])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "extract",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("container"),
                                                      argument_name("condition")],
                                                    [argument_type("Any")]),
                                                 block_statements( [ expr_value(string_value(' first element of container that satisfies condition ')),
                                                                           return_value( call_func_args( "next", [
                                                                                                 generator_exp_elt_generators( "e", [
                                                                                                   comprehension_ifs_target_iter([call_func_args("condition",["e"])],"e","container")])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "totuple",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("container")],[argument_type("Tuple")]),
                                                 block_statements( [ expr_value(string_value(' conversion to tuple ')),
                                                                           return_value(call_func_args("tuple",["container"]))])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "first",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("container")],[argument_type("Any")]),
                                                 block_statements( [ expr_value(string_value(' first item of container ')),
                                                                           return_value(call_func_args("next",[call_func_args("iter",["container"])]))])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "last",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("container")],[argument_type("Any")]),
                                                 block_statements( [ expr_value(string_value(' last item of container ')),
                                                                           return_value( subscript_value_slice(call_func_args("max",[call_func_args("enumerate",["container"])]),1))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "insert",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("value"),
                                                      argument_name("container")],
                                                    [argument_type("FrozenSet")]),
                                                 block_statements( [ expr_value(string_value(' insert item into container ')),
                                                                           return_value( call_func_args(
                                                                                                  qualified_identifier_identifiers(["container",boxed_attribute_value("union")]),
                                                                                                  [ call_func_args("frozenset",[set_elts(["value"])])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "remove",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("value"),
                                                      argument_name("container")],
                                                    [argument_type("Container")]),
                                                 block_statements( [ expr_value(string_value(' remove item from container ')),
                                                                           return_value( call_func_args(
                                                                                                  call_func_args("type",["container"]),
                                                                                                  [ generator_exp_elt_generators( "e", [
                                                                                                      comprehension_ifs_target_iter(
                                                                                                         [ compare_ops_left_comparators(ops([not_eq_token('!=')]),"e",comparators(["value"]))], "e","container")])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "other",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("container"),
                                                      argument_name("value")],
                                                    [argument_type("Any")]),
                                                 block_statements( [ expr_value(string_value(' other value in the container ')),
                                                                           return_value(call_func_args("first",[call_func_args("remove",["value","container"])]))])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "interval",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("start"), argument_name("stop"),argument_name("step")],
                                                    [argument_type("Tuple")]),
                                                 block_statements( [ expr_value(string_value(' range ')),
                                                                           return_value(call_func_args("tuple",[call_func_args("range",["start","stop","step"])]))])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "astuple",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("a"),
                                                      argument_name("b")],
                                                    [argument_type("IntegerTuple")]),
                                                 block_statements( [ expr_value(string_value(' constructs a tuple ')),
                                                                           return_value(["a","b"])])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "product",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("a"),
                                                      argument_name("b")],
                                                    [argument_type("FrozenSet")]),
                                                 block_statements( [ expr_value(string_value(' cartesian product ')),
                                                                           return_value( call_func_args( "frozenset", [
                                                                                                 generator_exp_elt_generators( ["i","j"], [
                                                                                                   comprehension_target_iter("j","b"),
                                                                                                   comprehension_target_iter("i","a")])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "pair",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("a"),
                                                      argument_name("b")],
                                                    [argument_type("TupleTuple")]),
                                                 block_statements( [ expr_value(string_value(' zipping of two tuples ')),
                                                                           return_value(call_func_args("tuple",[call_func_args("zip",["a","b"])]))])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "branch",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("condition"), argument_name("a"),argument_name("b")],
                                                    [argument_type("Any")]),
                                                 block_statements( [ expr_value(string_value(' if else branching ')),
                                                                           return_value(if_exp_test_body_orelse("condition","a","b"))])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "compose",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("outer"),
                                                      argument_name("inner")],
                                                    [argument_type("Callable")]),
                                                 block_statements( [ expr_value(string_value(' function composition ')),
                                                                           return_value( lambda_args_body( arguments_args([argument_name("x")]),
                                                                                                 body_stmts(call_func_args("outer",[call_func_args("inner",["x"])]))))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "chain",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("h"), argument_name("g"),argument_name("f")],
                                                    [argument_type("Callable")]),
                                                 block_statements( [ expr_value(string_value(' function composition with three functions ')),
                                                                           return_value( lambda_args_body( arguments_args([argument_name("x")]),
                                                                                                 body_stmts(call_func_args("h",[call_func_args("g",[call_func_args("f",["x"])])]))))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "matcher",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("function"),
                                                      argument_name("target")],
                                                    [argument_type("Callable")]),
                                                 block_statements( [ expr_value(string_value(' construction of equality function ')),
                                                                           return_value( lambda_args_body( arguments_args([argument_name("x")]),
                                                                                                 body_stmts( compare_ops_left_comparators( ops([eq_token(==)]),
                                                                                                                     call_func_args("function",["x"]),
                                                                                                                     comparators(["target"])))))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "rbind",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("function"),
                                                      argument_name("fixed")],
                                                    [argument_type("Callable")]),
                                                 block_statements( [ expr_value(string_value(' fix the rightmost argument ')),
                                                                           assign_targets_value( ["n"],
                                                                             qualified_identifier_identifiers( [ "function",
                                                                                                                       boxed_attribute_value("__code__"),
                                                                                                                       boxed_attribute_value("co_argcount")])),
                                                                           if_test_body_orelse(
                                                                              compare_ops_left_comparators(ops([eq_token(==)]),"n",comparators([2])),
                                                                              body_stmts( [ return_value( lambda_args_body( arguments_args([argument_name("x")]),
                                                                                                                        body_stmts(call_func_args("function",["x","fixed"]))))])
                                                                         ,    orelse_else_stmts( [ if_test_body_orelse(
                                                                                                            compare_ops_left_comparators(ops([eq_token(==)]),"n",comparators([3])),
                                                                                                            body_stmts( [ return_value( lambda_args_body(
                                                                                                                                                       arguments_args([argument_name("x"),argument_name("y")]),
                                                                                                                                                       body_stmts(call_func_args("function",["x","y","fixed"]))))])
                                                                                                         ,  orelse_else_stmts( [ return_value( lambda_args_body(
                                                                                                                                                              arguments_args([argument_name("x"),argument_name("y"),argument_name("z")]),
                                                                                                                                                              body_stmts(call_func_args("function",["x","y","z","fixed"]))))])
                                                                                                         )])
                                                                         )])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "lbind",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("function"),
                                                      argument_name("fixed")],
                                                    [argument_type("Callable")]),
                                                 block_statements( [ expr_value(string_value(' fix the leftmost argument ')),
                                                                           assign_targets_value( ["n"],
                                                                             qualified_identifier_identifiers( [ "function",
                                                                                                                       boxed_attribute_value("__code__"),
                                                                                                                       boxed_attribute_value("co_argcount")])),
                                                                           if_test_body_orelse(
                                                                              compare_ops_left_comparators(ops([eq_token(==)]),"n",comparators([2])),
                                                                              body_stmts( [ return_value( lambda_args_body( arguments_args([argument_name("y")]),
                                                                                                                        body_stmts(call_func_args("function",["fixed","y"]))))])
                                                                         ,    orelse_else_stmts( [ if_test_body_orelse(
                                                                                                            compare_ops_left_comparators(ops([eq_token(==)]),"n",comparators([3])),
                                                                                                            body_stmts( [ return_value( lambda_args_body(
                                                                                                                                                       arguments_args([argument_name("y"),argument_name("z")]),
                                                                                                                                                       body_stmts(call_func_args("function",["fixed","y","z"]))))])
                                                                                                         ,  orelse_else_stmts( [ return_value( lambda_args_body(
                                                                                                                                                              arguments_args([argument_name("y"),argument_name("z"),argument_name("a")]),
                                                                                                                                                              body_stmts(call_func_args("function",["fixed","y","z","a"]))))])
                                                                                                         )])
                                                                         )])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "power",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("function"),
                                                      argument_name("n")],
                                                    [argument_type("Callable")]),
                                                 block_statements( [ expr_value(string_value(' power of function ')),
                                                                           if_test_body(
                                                                              compare_ops_left_comparators(ops([eq_token(==)]),"n",comparators([1])),
                                                                              body_stmts([return_value("function")])),
                                                                           return_value( call_func_args( "compose", [
                                                                                                 "function",
                                                                                                 call_func_args("power",["function",bin_op_left_right(sub_token(-),"n",1)])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "fork",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("outer"), argument_name("a"),argument_name("b")],
                                                    [argument_type("Callable")]),
                                                 block_statements( [ expr_value(string_value(' creates a wrapper function ')),
                                                                           return_value( lambda_args_body( arguments_args([argument_name("x")]),
                                                                                                 body_stmts( call_func_args("outer",[call_func_args("a",["x"]),call_func_args("b",["x"])]))))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "apply",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("function"),
                                                      argument_name("container")],
                                                    [argument_type("Container")]),
                                                 block_statements( [ expr_value(string_value(' apply function to each item in container ')),
                                                                           return_value( call_func_args(
                                                                                                  call_func_args("type",["container"]),
                                                                                                  [ generator_exp_elt_generators(
                                                                                                       call_func_args("function",["e"]),
                                                                                                       [comprehension_target_iter("e","container")])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "rapply",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("functions"),
                                                      argument_name("value")],
                                                    [argument_type("Container")]),
                                                 block_statements( [ expr_value(string_value(' apply each function in container to value ')),
                                                                           return_value( call_func_args(
                                                                                                  call_func_args("type",["functions"]),
                                                                                                  [ generator_exp_elt_generators(
                                                                                                       call_func_args("function",["value"]),
                                                                                                       [comprehension_target_iter("function","functions")])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "mapply",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("function"),
                                                      argument_name("container")],
                                                    [argument_type("FrozenSet")]),
                                                 block_statements( [ expr_value(string_value(' apply and merge ')),
                                                                           return_value(call_func_args("merge",[call_func_args("apply",["function","container"])]))])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "papply",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("function"), argument_name("a"),argument_name("b")],
                                                    [argument_type("Tuple")]),
                                                 block_statements( [ expr_value(string_value(' apply function on two vectors ')),
                                                                           return_value( call_func_args( "tuple", [
                                                                                                 generator_exp_elt_generators(
                                                                                                    call_func_args("function",["i","j"]),
                                                                                                    [ comprehension_target_iter(["i","j"],call_func_args("zip",["a","b"]))])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "mpapply",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("function"), argument_name("a"),argument_name("b")],
                                                    [argument_type("Tuple")]),
                                                 block_statements( [ expr_value(string_value(' apply function on two vectors and merge ')),
                                                                           return_value(call_func_args("merge",[call_func_args("papply",["function","a","b"])]))])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "prapply",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("function"), argument_name("a"),argument_name("b")],
                                                    [argument_type("FrozenSet")]),
                                                 block_statements( [ expr_value(string_value(' apply function on cartesian product ')),
                                                                           return_value( call_func_args( "frozenset", [
                                                                                                 generator_exp_elt_generators(
                                                                                                    call_func_args("function",["i","j"]),
                                                                                                    [ comprehension_target_iter("j","b"),
                                                                                                      comprehension_target_iter("i","a")])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "mostcolor",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("element")],[argument_type("Integer")]),
                                                 block_statements( [ expr_value(string_value(' most common color ')),
                                                                           assign_targets_value( ["values"],
                                                                             if_exp_test_body_orelse(
                                                                                call_func_args("isinstance",["element","tuple"]),
                                                                                list_comp_elt_generators( "v", [
                                                                                  comprehension_target_iter("r","element"),
                                                                                  comprehension_target_iter("v","r")]),
                                                                                list_comp_elt_generators("v",[comprehension_target_iter(["v","_"],"element")]))),
                                                                           return_value( call_func_args_keywords( "max",
                                                                                                 [ call_func_args("set",["values"])],
                                                                                                 [ keyword_value(qualified_identifier_identifiers(["values",boxed_attribute_value("count")]))]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "leastcolor",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("element")],[argument_type("Integer")]),
                                                 block_statements( [ expr_value(string_value(' least common color ')),
                                                                           assign_targets_value( ["values"],
                                                                             if_exp_test_body_orelse(
                                                                                call_func_args("isinstance",["element","tuple"]),
                                                                                list_comp_elt_generators( "v", [
                                                                                  comprehension_target_iter("r","element"),
                                                                                  comprehension_target_iter("v","r")]),
                                                                                list_comp_elt_generators("v",[comprehension_target_iter(["v","_"],"element")]))),
                                                                           return_value( call_func_args_keywords( "min",
                                                                                                 [ call_func_args("set",["values"])],
                                                                                                 [ keyword_value(qualified_identifier_identifiers(["values",boxed_attribute_value("count")]))]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "height",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("piece")],[argument_type("Integer")]),
                                                 block_statements( [ expr_value(string_value(' height of grid or patch ')),
                                                                           if_test_body(
                                                                              compare_ops_left_comparators( ops([eq_token(==)]),
                                                                                call_func_args("len",["piece"]),
                                                                                comparators([0])),
                                                                              body_stmts([return_value(0)])),
                                                                           if_test_body(
                                                                              call_func_args("isinstance",["piece","tuple"]),
                                                                              body_stmts([return_value(call_func_args("len",["piece"]))])),
                                                                           return_value( bin_op_left_right( add_token(+),
                                                                                                 bin_op_left_right( sub_token(-),
                                                                                                   call_func_args("lowermost",["piece"]),
                                                                                                   call_func_args("uppermost",["piece"])),
                                                                                                 1))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "width",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("piece")],[argument_type("Integer")]),
                                                 block_statements( [ expr_value(string_value(' width of grid or patch ')),
                                                                           if_test_body(
                                                                              compare_ops_left_comparators( ops([eq_token(==)]),
                                                                                call_func_args("len",["piece"]),
                                                                                comparators([0])),
                                                                              body_stmts([return_value(0)])),
                                                                           if_test_body(
                                                                              call_func_args("isinstance",["piece","tuple"]),
                                                                              body_stmts([return_value(call_func_args("len",[subscript_value_slice("piece",0)]))])),
                                                                           return_value( bin_op_left_right( add_token(+),
                                                                                                 bin_op_left_right( sub_token(-),
                                                                                                   call_func_args("rightmost",["piece"]),
                                                                                                   call_func_args("leftmost",["piece"])),
                                                                                                 1))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "shape",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("piece")],[argument_type("IntegerTuple")]),
                                                 block_statements( [ expr_value(string_value(' height and width of grid or patch ')),
                                                                           return_value([call_func_args("height",["piece"]),call_func_args("width",["piece"])])])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "portrait",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("piece")],[argument_type("Boolean")]),
                                                 block_statements( [ expr_value(string_value(' whether height is greater than width ')),
                                                                           return_value( compare_ops_left_comparators( ops([gt_token(>)]),
                                                                                                 call_func_args("height",["piece"]),
                                                                                                 comparators([call_func_args("width",["piece"])])))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "colorcount",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("element"),
                                                      argument_name("value")],
                                                    [argument_type("Integer")]),
                                                 block_statements( [ expr_value(string_value(' number of cells with color ')),
                                                                           if_test_body(
                                                                              call_func_args("isinstance",["element","tuple"]),
                                                                              body_stmts( [ return_value( call_func_args( "sum", [
                                                                                                                        generator_exp_elt_generators(
                                                                                                                           call_func_args(
                                                                                                                              qualified_identifier_identifiers(["row",boxed_attribute_value("count")]),
                                                                                                                              ["value"]),
                                                                                                                           [comprehension_target_iter("row","element")])]))])
                                                                         ),
                                                                           return_value( call_func_args( "sum", [
                                                                                                 generator_exp_elt_generators(
                                                                                                    compare_ops_left_comparators(ops([eq_token(==)]),"v",comparators(["value"])),
                                                                                                    [ comprehension_target_iter(["v","_"],"element")])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "colorfilter",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("objs"),
                                                      argument_name("value")],
                                                    [argument_type("Objects")]),
                                                 block_statements( [ expr_value(string_value(' filter objects by color ')),
                                                                           return_value( call_func_args( "frozenset", [
                                                                                                 generator_exp_elt_generators( "obj", [
                                                                                                   comprehension_ifs_target_iter(
                                                                                                      [ compare_ops_left_comparators( ops([eq_token(==)]),
                                                                                                          subscript_value_slice(call_func_args("next",[call_func_args("iter",["obj"])]),0),
                                                                                                          comparators(["value"]))], "obj","objs")])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "sizefilter",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("container"),
                                                      argument_name("n")],
                                                    [argument_type("FrozenSet")]),
                                                 block_statements( [ expr_value(string_value(' filter items by size ')),
                                                                           return_value( call_func_args( "frozenset", [
                                                                                                 generator_exp_elt_generators( "item", [
                                                                                                   comprehension_ifs_target_iter(
                                                                                                      [ compare_ops_left_comparators( ops([eq_token(==)]),
                                                                                                          call_func_args("len",["item"]),
                                                                                                          comparators(["n"]))], "item","container")])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "asindices",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("grid")],[argument_type("Indices")]),
                                                 block_statements( [ expr_value(string_value(' indices of all grid cells ')),
                                                                           return_value( call_func_args( "frozenset", [
                                                                                                 generator_exp_elt_generators( ["i","j"], [
                                                                                                   comprehension_target_iter("i",call_func_args("range",[call_func_args("len",["grid"])])),
                                                                                                   comprehension_target_iter( "j",
                                                                                                     call_func_args("range",[call_func_args("len",[subscript_value_slice("grid",0)])]))])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "ofcolor",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("grid"),
                                                      argument_name("value")],
                                                    [argument_type("Indices")]),
                                                 block_statements( [ expr_value(string_value(' indices of all grid cells with value ')),
                                                                           return_value( call_func_args( "frozenset", [
                                                                                                 generator_exp_elt_generators( ["i","j"], [
                                                                                                   comprehension_target_iter(["i","r"],call_func_args("enumerate",["grid"])),
                                                                                                   comprehension_ifs_target_iter(
                                                                                                      [ compare_ops_left_comparators(ops([eq_token(==)]),"v",comparators(["value"]))],
                                                                                                      ["j","v"],
                                                                                                      call_func_args("enumerate",["r"]))])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "ulcorner",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("patch")],[argument_type("IntegerTuple")]),
                                                 block_statements( [ expr_value(string_value(' index of upper left corner ')),
                                                                           return_value( call_func_args( "tuple", [
                                                                                                 call_func_args( "map", [
                                                                                                   "min",
                                                                                                   call_func_args("zip",[starred_value(call_func_args("toindices",["patch"]))])])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "urcorner",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("patch")],[argument_type("IntegerTuple")]),
                                                 block_statements( [ expr_value(string_value(' index of upper right corner ')),
                                                                           return_value( call_func_args( "tuple", [
                                                                                                 call_func_args( "map", [
                                                                                                   lambda_args_body( arguments_args([argument_name("ix")]),
                                                                                                     body_stmts( call_func_args(
                                                                                                                          subscript_value_slice(
                                                                                                                             dict_keys_values([0,1],["min","max"]),
                                                                                                                             subscript_value_slice("ix",0)),
                                                                                                                          [subscript_value_slice("ix",1)]))),
                                                                                                   call_func_args( "enumerate", [
                                                                                                     call_func_args("zip",[starred_value(call_func_args("toindices",["patch"]))])])])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "llcorner",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("patch")],[argument_type("IntegerTuple")]),
                                                 block_statements( [ expr_value(string_value(' index of lower left corner ')),
                                                                           return_value( call_func_args( "tuple", [
                                                                                                 call_func_args( "map", [
                                                                                                   lambda_args_body( arguments_args([argument_name("ix")]),
                                                                                                     body_stmts( call_func_args(
                                                                                                                          subscript_value_slice(
                                                                                                                             dict_keys_values([0,1],["max","min"]),
                                                                                                                             subscript_value_slice("ix",0)),
                                                                                                                          [subscript_value_slice("ix",1)]))),
                                                                                                   call_func_args( "enumerate", [
                                                                                                     call_func_args("zip",[starred_value(call_func_args("toindices",["patch"]))])])])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "lrcorner",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("patch")],[argument_type("IntegerTuple")]),
                                                 block_statements( [ expr_value(string_value(' index of lower right corner ')),
                                                                           return_value( call_func_args( "tuple", [
                                                                                                 call_func_args( "map", [
                                                                                                   "max",
                                                                                                   call_func_args("zip",[starred_value(call_func_args("toindices",["patch"]))])])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "crop",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("grid"), argument_name("start"),argument_name("dims")],
                                                    [argument_type("Grid")]),
                                                 block_statements( [ expr_value(string_value(' subgrid specified by start and dimension ')),
                                                                           return_value( call_func_args( "tuple", [
                                                                                                 generator_exp_elt_generators(
                                                                                                    subscript_value_slice( "r",
                                                                                                      slice_lower_upper( subscript_value_slice("start",1),
                                                                                                        bin_op_left_right(add_token(+),subscript_value_slice("start",1),subscript_value_slice("dims",1)))),
                                                                                                    [ comprehension_target_iter( "r",
                                                                                                        subscript_value_slice( "grid",
                                                                                                          slice_lower_upper( subscript_value_slice("start",0),
                                                                                                            bin_op_left_right(add_token(+),subscript_value_slice("start",0),subscript_value_slice("dims",0)))))])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "toindices",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("patch")],[argument_type("Indices")]),
                                                 block_statements( [ expr_value(string_value(' indices of object cells ')),
                                                                           if_test_body(
                                                                              compare_ops_left_comparators( ops([eq_token(==)]),
                                                                                call_func_args("len",["patch"]),
                                                                                comparators([0])),
                                                                              body_stmts([return_value(call_func("frozenset"))])),
                                                                           if_test_body(
                                                                              call_func_args( "isinstance", [
                                                                                subscript_value_slice(call_func_args("next",[call_func_args("iter",["patch"])]),1),
                                                                                "tuple"]),
                                                                              body_stmts( [ return_value( call_func_args( "frozenset", [
                                                                                                                        generator_exp_elt_generators("index",[comprehension_target_iter(["value","index"],"patch")])]))])
                                                                         ),
                                                                           return_value("patch")])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "recolor",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("value"),
                                                      argument_name("patch")],
                                                    [argument_type("Object")]),
                                                 block_statements( [ expr_value(string_value(' recolor patch ')),
                                                                           return_value( call_func_args( "frozenset", [
                                                                                                 generator_exp_elt_generators( ["value","index"], [
                                                                                                   comprehension_target_iter("index",call_func_args("toindices",["patch"]))])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "shift",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("patch"),
                                                      argument_name("directions")],
                                                    [argument_type("Patch")]),
                                                 block_statements( [ expr_value(string_value(' shift patch ')),
                                                                           assign_targets_value([["di","dj"]],"directions"),
                                                                           if_test_body(
                                                                              call_func_args( "isinstance", [
                                                                                subscript_value_slice(call_func_args("next",[call_func_args("iter",["patch"])]),1),
                                                                                "tuple"]),
                                                                              body_stmts( [ return_value( call_func_args( "frozenset", [
                                                                                                                        generator_exp_elt_generators(
                                                                                                                           [ "value",
                                                                                                                             [ bin_op_left_right(add_token(+),"i","di"),
                                                                                                                               bin_op_left_right(add_token(+),"j","dj")]],
                                                                                                                           [ comprehension_target_iter(["value",["i","j"]],"patch")])]))])
                                                                         ),
                                                                           return_value( call_func_args( "frozenset", [
                                                                                                 generator_exp_elt_generators(
                                                                                                    [ bin_op_left_right(add_token(+),"i","di"),
                                                                                                      bin_op_left_right(add_token(+),"j","dj")],
                                                                                                    [ comprehension_target_iter(["i","j"],"patch")])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "normalize",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("patch")],[argument_type("Patch")]),
                                                 block_statements( [ expr_value(string_value(' moves upper left corner to origin ')),
                                                                           return_value( call_func_args( "shift", [
                                                                                                 "patch",
                                                                                                 [ unary_op_operand(us_ub_token(-),call_func_args("uppermost",["patch"])),
                                                                                                   unary_op_operand(us_ub_token(-),call_func_args("leftmost",["patch"]))]]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "dneighbors",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("loc")],[argument_type("Indices")]),
                                                 block_statements( [ expr_value(string_value(' directly adjacent indices ')),
                                                                           return_value( call_func_args( "frozenset", [
                                                                                                 set_elts( [ [ bin_op_left_right(sub_token(-),subscript_value_slice("loc",0),1),
                                                                                                                     subscript_value_slice("loc",1)],
                                                                                                                   [ bin_op_left_right(add_token(+),subscript_value_slice("loc",0),1),
                                                                                                                     subscript_value_slice("loc",1)],
                                                                                                                   [ subscript_value_slice("loc",0),
                                                                                                                     bin_op_left_right(sub_token(-),subscript_value_slice("loc",1),1)],
                                                                                                                   [ subscript_value_slice("loc",0),
                                                                                                                     bin_op_left_right(add_token(+),subscript_value_slice("loc",1),1)]])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "ineighbors",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("loc")],[argument_type("Indices")]),
                                                 block_statements( [ expr_value(string_value(' diagonally adjacent indices ')),
                                                                           return_value( call_func_args( "frozenset", [
                                                                                                 set_elts( [ [ bin_op_left_right(sub_token(-),subscript_value_slice("loc",0),1),
                                                                                                                     bin_op_left_right(sub_token(-),subscript_value_slice("loc",1),1)],
                                                                                                                   [ bin_op_left_right(sub_token(-),subscript_value_slice("loc",0),1),
                                                                                                                     bin_op_left_right(add_token(+),subscript_value_slice("loc",1),1)],
                                                                                                                   [ bin_op_left_right(add_token(+),subscript_value_slice("loc",0),1),
                                                                                                                     bin_op_left_right(sub_token(-),subscript_value_slice("loc",1),1)],
                                                                                                                   [ bin_op_left_right(add_token(+),subscript_value_slice("loc",0),1),
                                                                                                                     bin_op_left_right(add_token(+),subscript_value_slice("loc",1),1)]])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "neighbors",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("loc")],[argument_type("Indices")]),
                                                 block_statements( [ expr_value(string_value(' adjacent indices ')),
                                                                           return_value( bin_op_left_right( bit_or_token('|'),
                                                                                                 call_func_args("dneighbors",["loc"]),
                                                                                                 call_func_args("ineighbors",["loc"])))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "objects",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("grid"), argument_name("univalued"),argument_name("diagonal"),
                                                      argument_name("without_bg")],
                                                    [argument_type("Objects")]),
                                                 block_statements( [ expr_value(string_value(' objects occurring on the grid ')),
                                                                           assign_targets_value( ["bg"],
                                                                             if_exp_test_body_orelse( "without_bg",
                                                                               call_func_args("mostcolor",["grid"]),
                                                                               none_literal_value_token('None','None'))),
                                                                           assign_targets_value(["objs"],call_func("set")),
                                                                           assign_targets_value(["occupied"],call_func("set")),
                                                                           assign_targets_value( [ [ "h","w"]], [
                                                                             call_func_args("len",["grid"]),
                                                                             call_func_args("len",[subscript_value_slice("grid",0)])]),
                                                                           assign_targets_value(["unvisited"],call_func_args("asindices",["grid"])),
                                                                           assign_targets_value(["diagfun"],if_exp_test_body_orelse("diagonal","neighbors","dneighbors")),
                                                                           for_target_iter_body( "loc",
                                                                             "unvisited",
                                                                             body_stmts( [ if_test_body(
                                                                                                    compare_ops_left_comparators(ops([['python:In']]),"loc",comparators(["occupied"])),
                                                                                                    body_stmts([['python:Continue']])),
                                                                                                 assign_targets_value( ["val"],
                                                                                                   subscript_value_slice(subscript_value_slice("grid",subscript_value_slice("loc",0)),subscript_value_slice("loc",1))),
                                                                                                 if_test_body(
                                                                                                    compare_ops_left_comparators(ops([eq_token(==)]),"val",comparators(["bg"])),
                                                                                                    body_stmts([['python:Continue']])),
                                                                                                 assign_targets_value(["obj"],set_elts([["val","loc"]])),
                                                                                                 assign_targets_value(["cands"],set_elts(["loc"])),
                                                                                                 while_test_body(
                                                                                                    compare_ops_left_comparators( ops([gt_token(>)]),
                                                                                                      call_func_args("len",["cands"]),
                                                                                                      comparators([0])),
                                                                                                    body_stmts( [ assign_targets_value(["neighborhood"],call_func("set")),
                                                                                                                        for_target_iter_body( "cand",
                                                                                                                          "cands",
                                                                                                                          body_stmts( [ assign_targets_value( ["v"],
                                                                                                                                                subscript_value_slice(subscript_value_slice("grid",subscript_value_slice("cand",0)),subscript_value_slice("cand",1))),
                                                                                                                                              if_test_body(
                                                                                                                                                 if_exp_test_body_orelse( "univalued",
                                                                                                                                                   compare_ops_left_comparators(ops([eq_token(==)]),"val",comparators(["v"])),
                                                                                                                                                   compare_ops_left_comparators(ops([not_eq_token('!=')]),"v",comparators(["bg"]))),
                                                                                                                                                 body_stmts( [ expr_value( call_func_args(
                                                                                                                                                                                          qualified_identifier_identifiers(["obj",boxed_attribute_value("add")]),
                                                                                                                                                                                          [ [  "v"  ,"cand"]])),
                                                                                                                                                                     expr_value( call_func_args(
                                                                                                                                                                                          qualified_identifier_identifiers(["occupied",boxed_attribute_value("add")]),
                                                                                                                                                                                          ["cand"]))
                                                                                                                                                                   , aug_assign_op_target_value( bit_or_token('|'),
                                                                                                                                                                       "neighborhood",
                                                                                                                                                                       set_comp_elt_generators( ["i","j"], [
                                                                                                                                                                         comprehension_ifs_target_iter(
                                                                                                                                                                            [ bool_op_values( ['python:And'], [
                                                                                                                                                                                compare_ops_left_comparators(ops([lt_e_token(<=),lt_token(<)]),0,comparators(["i","h"])),
                                                                                                                                                                                compare_ops_left_comparators(
                                                                                                                                                                                   ops([lt_e_token(<=),lt_token(<)]), 0,comparators(["j","w"]))])],
                                                                                                                                                                            ["i","j"],
                                                                                                                                                                            call_func_args("diagfun",["cand"]))]))])
                                                                                                                                            )])
                                                                                                                      ),
                                                                                                                        assign_targets_value(["cands"],bin_op_left_right(sub_token(-),"neighborhood","occupied"))])
                                                                                               ),
                                                                                                 expr_value( call_func_args(
                                                                                                                      qualified_identifier_identifiers(["objs",boxed_attribute_value("add")]),
                                                                                                                      [ call_func_args("frozenset",["obj"])]))
                                                                                               ])
                                                                         ),
                                                                           return_value(call_func_args("frozenset",["objs"]))])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "partition",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("grid")],[argument_type("Objects")]),
                                                 block_statements( [ expr_value(string_value(' each cell with the same value part of the same object ')),
                                                                           return_value( call_func_args( "frozenset", [
                                                                                                 generator_exp_elt_generators(
                                                                                                    call_func_args( "frozenset", [
                                                                                                      generator_exp_elt_generators(
                                                                                                         [ "v",
                                                                                                           ["i","j"]],
                                                                                                         [ comprehension_target_iter(["i","r"],call_func_args("enumerate",["grid"])),
                                                                                                           comprehension_ifs_target_iter(
                                                                                                              [ compare_ops_left_comparators(ops([eq_token(==)]),"v",comparators(["value"]))],
                                                                                                              ["j","v"],
                                                                                                              call_func_args("enumerate",["r"]))])]),
                                                                                                    [ comprehension_target_iter("value",call_func_args("palette",["grid"]))])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "fgpartition",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("grid")],[argument_type("Objects")]),
                                                 block_statements( [ expr_value(string_value(' each cell with the same value part of the same object without background ')),
                                                                           return_value( call_func_args( "frozenset", [
                                                                                                 generator_exp_elt_generators(
                                                                                                    call_func_args( "frozenset", [
                                                                                                      generator_exp_elt_generators(
                                                                                                         [ "v",
                                                                                                           ["i","j"]],
                                                                                                         [ comprehension_target_iter(["i","r"],call_func_args("enumerate",["grid"])),
                                                                                                           comprehension_ifs_target_iter(
                                                                                                              [ compare_ops_left_comparators(ops([eq_token(==)]),"v",comparators(["value"]))],
                                                                                                              ["j","v"],
                                                                                                              call_func_args("enumerate",["r"]))])]),
                                                                                                    [ comprehension_target_iter( "value",
                                                                                                        bin_op_left_right( sub_token(-),
                                                                                                          call_func_args("palette",["grid"]),
                                                                                                          set_elts([call_func_args("mostcolor",["grid"])])))])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "uppermost",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("patch")],[argument_type("Integer")]),
                                                 block_statements( [ expr_value(string_value(' row index of uppermost occupied cell ')),
                                                                           return_value( call_func_args( "min", [
                                                                                                 generator_exp_elt_generators( "i", [
                                                                                                   comprehension_target_iter(["i","j"],call_func_args("toindices",["patch"]))])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "lowermost",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("patch")],[argument_type("Integer")]),
                                                 block_statements( [ expr_value(string_value(' row index of lowermost occupied cell ')),
                                                                           return_value( call_func_args( "max", [
                                                                                                 generator_exp_elt_generators( "i", [
                                                                                                   comprehension_target_iter(["i","j"],call_func_args("toindices",["patch"]))])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "leftmost",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("patch")],[argument_type("Integer")]),
                                                 block_statements( [ expr_value(string_value(' column index of leftmost occupied cell ')),
                                                                           return_value( call_func_args( "min", [
                                                                                                 generator_exp_elt_generators( "j", [
                                                                                                   comprehension_target_iter(["i","j"],call_func_args("toindices",["patch"]))])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "rightmost",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("patch")],[argument_type("Integer")]),
                                                 block_statements( [ expr_value(string_value(' column index of rightmost occupied cell ')),
                                                                           return_value( call_func_args( "max", [
                                                                                                 generator_exp_elt_generators( "j", [
                                                                                                   comprehension_target_iter(["i","j"],call_func_args("toindices",["patch"]))])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "square",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("piece")],[argument_type("Boolean")]),
                                                 block_statements( [ expr_value(string_value(' whether the piece forms a square ')),
                                                                           return_value( if_exp_test_body_orelse(
                                                                                                  call_func_args("isinstance",["piece","tuple"]),
                                                                                                  compare_ops_left_comparators( ops([eq_token(==)]),
                                                                                                    call_func_args("len",["piece"]),
                                                                                                    comparators([call_func_args("len",[subscript_value_slice("piece",0)])])),
                                                                                                  bool_op_values( ['python:And'], [
                                                                                                    compare_ops_left_comparators( ops([eq_token(==)]),
                                                                                                      bin_op_left_right(mult_token(*),call_func_args("height",["piece"]),call_func_args("width",["piece"])),
                                                                                                      comparators([call_func_args("len",["piece"])])),
                                                                                                    compare_ops_left_comparators( ops([eq_token(==)]),
                                                                                                      call_func_args("height",["piece"]),
                                                                                                      comparators([call_func_args("width",["piece"])]))])))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "vline",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("patch")],[argument_type("Boolean")]),
                                                 block_statements( [ expr_value(string_value(' whether the piece forms a vertical line ')),
                                                                           return_value( bool_op_values( ['python:And'], [
                                                                                                 compare_ops_left_comparators( ops([eq_token(==)]),
                                                                                                   call_func_args("height",["patch"]),
                                                                                                   comparators([call_func_args("len",["patch"])])),
                                                                                                 compare_ops_left_comparators( ops([eq_token(==)]),
                                                                                                   call_func_args("width",["patch"]),
                                                                                                   comparators([1]))]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "hline",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("patch")],[argument_type("Boolean")]),
                                                 block_statements( [ expr_value(string_value(' whether the piece forms a horizontal line ')),
                                                                           return_value( bool_op_values( ['python:And'], [
                                                                                                 compare_ops_left_comparators( ops([eq_token(==)]),
                                                                                                   call_func_args("width",["patch"]),
                                                                                                   comparators([call_func_args("len",["patch"])])),
                                                                                                 compare_ops_left_comparators( ops([eq_token(==)]),
                                                                                                   call_func_args("height",["patch"]),
                                                                                                   comparators([1]))]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "hmatching",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("a"),
                                                      argument_name("b")],
                                                    [argument_type("Boolean")]),
                                                 block_statements( [ expr_value(string_value(' whether there exists a row for which both patches have cells ')),
                                                                           return_value( compare_ops_left_comparators( ops([gt_token(>)]),
                                                                                                 call_func_args( "len", [
                                                                                                   bin_op_left_right( bit_and_token(&),
                                                                                                     call_func_args( "set", [
                                                                                                       generator_exp_elt_generators( "i", [
                                                                                                         comprehension_target_iter(["i","j"],call_func_args("toindices",["a"]))])]),
                                                                                                     call_func_args( "set", [
                                                                                                       generator_exp_elt_generators( "i", [
                                                                                                         comprehension_target_iter(["i","j"],call_func_args("toindices",["b"]))])]))]),
                                                                                                 comparators([0])))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "vmatching",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("a"),
                                                      argument_name("b")],
                                                    [argument_type("Boolean")]),
                                                 block_statements( [ expr_value(string_value(' whether there exists a column for which both patches have cells ')),
                                                                           return_value( compare_ops_left_comparators( ops([gt_token(>)]),
                                                                                                 call_func_args( "len", [
                                                                                                   bin_op_left_right( bit_and_token(&),
                                                                                                     call_func_args( "set", [
                                                                                                       generator_exp_elt_generators( "j", [
                                                                                                         comprehension_target_iter(["i","j"],call_func_args("toindices",["a"]))])]),
                                                                                                     call_func_args( "set", [
                                                                                                       generator_exp_elt_generators( "j", [
                                                                                                         comprehension_target_iter(["i","j"],call_func_args("toindices",["b"]))])]))]),
                                                                                                 comparators([0])))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "manhattan",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("a"),
                                                      argument_name("b")],
                                                    [argument_type("Integer")]),
                                                 block_statements( [ expr_value(string_value(' closest manhattan distance between two patches ')),
                                                                           return_value( call_func_args( "min", [
                                                                                                 generator_exp_elt_generators(
                                                                                                    bin_op_left_right( add_token(+),
                                                                                                      call_func_args("abs",[bin_op_left_right(sub_token(-),"ai","bi")]),
                                                                                                      call_func_args("abs",[bin_op_left_right(sub_token(-),"aj","bj")])),
                                                                                                    [ comprehension_target_iter(["ai","aj"],call_func_args("toindices",["a"])),
                                                                                                      comprehension_target_iter(["bi","bj"],call_func_args("toindices",["b"]))])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "adjacent",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("a"),
                                                      argument_name("b")],
                                                    [argument_type("Boolean")]),
                                                 block_statements( [ expr_value(string_value(' whether two patches are adjacent ')),
                                                                           return_value( compare_ops_left_comparators( ops([eq_token(==)]),
                                                                                                 call_func_args("manhattan",["a","b"]),
                                                                                                 comparators([1])))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "bordering",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("patch"),
                                                      argument_name("grid")],
                                                    [argument_type("Boolean")]),
                                                 block_statements( [ expr_value(string_value(' whether a patch is adjacent to a grid border ')),
                                                                           return_value( bool_op_values( ['python:Or'], [
                                                                                                 compare_ops_left_comparators( ops([eq_token(==)]),
                                                                                                   call_func_args("uppermost",["patch"]),
                                                                                                   comparators([0])),
                                                                                                 compare_ops_left_comparators( ops([eq_token(==)]),
                                                                                                   call_func_args("leftmost",["patch"]),
                                                                                                   comparators([0])),
                                                                                                 compare_ops_left_comparators( ops([eq_token(==)]),
                                                                                                   call_func_args("lowermost",["patch"]),
                                                                                                   comparators([bin_op_left_right(sub_token(-),call_func_args("len",["grid"]),1)])),
                                                                                                 compare_ops_left_comparators( ops([eq_token(==)]),
                                                                                                   call_func_args("rightmost",["patch"]),
                                                                                                   comparators( [ bin_op_left_right(sub_token(-),call_func_args("len",[subscript_value_slice("grid",0)]),1)])
                                                                                               )]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "centerofmass",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("patch")],[argument_type("IntegerTuple")]),
                                                 block_statements( [ expr_value(string_value(' center of mass ')),
                                                                           return_value( call_func_args( "tuple", [
                                                                                                 call_func_args( "map", [
                                                                                                   lambda_args_body( arguments_args([argument_name("x")]),
                                                                                                     body_stmts( bin_op_left_right(floor_div_token(//),call_func_args("sum",["x"]),call_func_args("len",["patch"])))),
                                                                                                   call_func_args("zip",[starred_value(call_func_args("toindices",["patch"]))])])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "palette",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("element")],[argument_type("IntegerSet")]),
                                                 block_statements( [ expr_value(string_value(' colors occurring in object or grid ')),
                                                                           if_test_body(
                                                                              call_func_args("isinstance",["element","tuple"]),
                                                                              body_stmts( [ return_value( call_func_args( "frozenset", [
                                                                                                                        set_comp_elt_generators( "v", [
                                                                                                                          comprehension_target_iter("r","element"),
                                                                                                                          comprehension_target_iter("v","r")])]))])
                                                                         ),
                                                                           return_value( call_func_args( "frozenset", [
                                                                                                 set_comp_elt_generators("v",[comprehension_target_iter(["v","_"],"element")])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "numcolors",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("element")],[argument_type("IntegerSet")]),
                                                 block_statements( [ expr_value(string_value(' number of colors occurring in object or grid ')),
                                                                           return_value(call_func_args("len",[call_func_args("palette",["element"])]))])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "color",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("obj")],[argument_type("Integer")]),
                                                 block_statements( [ expr_value(string_value(' color of object ')),
                                                                           return_value(subscript_value_slice(call_func_args("next",[call_func_args("iter",["obj"])]),0))])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "toobject",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("patch"),
                                                      argument_name("grid")],
                                                    [argument_type("Object")]),
                                                 block_statements( [ expr_value(string_value(' object from patch and grid ')),
                                                                           assign_targets_value( [ [ "h","w"]], [
                                                                             call_func_args("len",["grid"]),
                                                                             call_func_args("len",[subscript_value_slice("grid",0)])]),
                                                                           return_value( call_func_args( "frozenset", [
                                                                                                 generator_exp_elt_generators(
                                                                                                    [ subscript_value_slice(subscript_value_slice("grid","i"),"j"),
                                                                                                      ["i","j"]],
                                                                                                    [ comprehension_ifs_target_iter(
                                                                                                         [ bool_op_values( ['python:And'], [
                                                                                                             compare_ops_left_comparators(ops([lt_e_token(<=),lt_token(<)]),0,comparators(["i","h"])),
                                                                                                             compare_ops_left_comparators(
                                                                                                                ops([lt_e_token(<=),lt_token(<)]), 0,comparators(["j","w"]))])],
                                                                                                         ["i","j"],
                                                                                                         call_func_args("toindices",["patch"]))])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "asobject",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("grid")],[argument_type("Object")]),
                                                 block_statements( [ expr_value(string_value(' conversion of grid to object ')),
                                                                           return_value( call_func_args( "frozenset", [
                                                                                                 generator_exp_elt_generators(
                                                                                                    [ "v",
                                                                                                      ["i","j"]],
                                                                                                    [ comprehension_target_iter(["i","r"],call_func_args("enumerate",["grid"])),
                                                                                                      comprehension_target_iter(["j","v"],call_func_args("enumerate",["r"]))])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "rot90",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("grid")],[argument_type("Grid")]),
                                                 block_statements( [ expr_value(string_value(' quarter clockwise rotation ')),
                                                                           return_value( call_func_args( "tuple", [
                                                                                                 generator_exp_elt_generators( "row", [
                                                                                                   comprehension_target_iter( "row",
                                                                                                     call_func_args( "zip", [
                                                                                                       starred_value(subscript_value_slice("grid",slice_step(unary_op_operand(us_ub_token(-),1))))]))])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "rot180",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("grid")],[argument_type("Grid")]),
                                                 block_statements( [ expr_value(string_value(' half rotation ')),
                                                                           return_value( call_func_args( "tuple", [
                                                                                                 generator_exp_elt_generators(
                                                                                                    call_func_args("tuple",[subscript_value_slice("row",slice_step(unary_op_operand(us_ub_token(-),1)))]),
                                                                                                    [ comprehension_target_iter("row",subscript_value_slice("grid",slice_step(unary_op_operand(us_ub_token(-),1))))])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "rot270",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("grid")],[argument_type("Grid")]),
                                                 block_statements( [ expr_value(string_value(' quarter anticlockwise rotation ')),
                                                                           return_value( subscript_value_slice(
                                                                                                  call_func_args( "tuple", [
                                                                                                    generator_exp_elt_generators(
                                                                                                       call_func_args("tuple",[subscript_value_slice("row",slice_step(unary_op_operand(us_ub_token(-),1)))]),
                                                                                                       [ comprehension_target_iter( "row",
                                                                                                           call_func_args( "zip", [
                                                                                                             starred_value(subscript_value_slice("grid",slice_step(unary_op_operand(us_ub_token(-),1))))]))])]),
                                                                                                  slice_step(unary_op_operand(us_ub_token(-),1))))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "hmirror",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("piece")],[argument_type("Piece")]),
                                                 block_statements( [ expr_value(string_value(' mirroring along horizontal ')),
                                                                           if_test_body(
                                                                              call_func_args("isinstance",["piece","tuple"]),
                                                                              body_stmts([return_value(subscript_value_slice("piece",slice_step(unary_op_operand(us_ub_token(-),1))))])),
                                                                           assign_targets_value( ["d"],
                                                                             bin_op_left_right( add_token(+),
                                                                               subscript_value_slice(call_func_args("ulcorner",["piece"]),0),
                                                                               subscript_value_slice(call_func_args("lrcorner",["piece"]),0))),
                                                                           if_test_body(
                                                                              call_func_args( "isinstance", [
                                                                                subscript_value_slice(call_func_args("next",[call_func_args("iter",["piece"])]),1),
                                                                                "tuple"]),
                                                                              body_stmts( [ return_value( call_func_args( "frozenset", [
                                                                                                                        generator_exp_elt_generators(
                                                                                                                           [ "v",
                                                                                                                             [ bin_op_left_right(sub_token(-),"d","i"),
                                                                                                                               "j"]],
                                                                                                                           [ comprehension_target_iter(["v",["i","j"]],"piece")])]))])
                                                                         ),
                                                                           return_value( call_func_args( "frozenset", [
                                                                                                 generator_exp_elt_generators(
                                                                                                    [ bin_op_left_right(sub_token(-),"d","i"),
                                                                                                      "j"],
                                                                                                    [ comprehension_target_iter(["i","j"],"piece")])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "vmirror",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("piece")],[argument_type("Piece")]),
                                                 block_statements( [ expr_value(string_value(' mirroring along vertical ')),
                                                                           if_test_body(
                                                                              call_func_args("isinstance",["piece","tuple"]),
                                                                              body_stmts( [ return_value( call_func_args( "tuple", [
                                                                                                                        generator_exp_elt_generators(
                                                                                                                           subscript_value_slice("row",slice_step(unary_op_operand(us_ub_token(-),1))),
                                                                                                                           [comprehension_target_iter("row","piece")])]))])
                                                                         ),
                                                                           assign_targets_value( ["d"],
                                                                             bin_op_left_right( add_token(+),
                                                                               subscript_value_slice(call_func_args("ulcorner",["piece"]),1),
                                                                               subscript_value_slice(call_func_args("lrcorner",["piece"]),1))),
                                                                           if_test_body(
                                                                              call_func_args( "isinstance", [
                                                                                subscript_value_slice(call_func_args("next",[call_func_args("iter",["piece"])]),1),
                                                                                "tuple"]),
                                                                              body_stmts( [ return_value( call_func_args( "frozenset", [
                                                                                                                        generator_exp_elt_generators(
                                                                                                                           [ "v",
                                                                                                                             [ "i",
                                                                                                                               bin_op_left_right(sub_token(-),"d","j")]],
                                                                                                                           [ comprehension_target_iter(["v",["i","j"]],"piece")])]))])
                                                                         ),
                                                                           return_value( call_func_args( "frozenset", [
                                                                                                 generator_exp_elt_generators(
                                                                                                    [ "i",
                                                                                                      bin_op_left_right(sub_token(-),"d","j")],
                                                                                                    [ comprehension_target_iter(["i","j"],"piece")])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "dmirror",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("piece")],[argument_type("Piece")]),
                                                 block_statements( [ expr_value(string_value(' mirroring along diagonal ')),
                                                                           if_test_body(
                                                                              call_func_args("isinstance",["piece","tuple"]),
                                                                              body_stmts( [ return_value(call_func_args("tuple",[call_func_args("zip",[starred_value("piece")])]))])
                                                                         ),
                                                                           assign_targets_value([["a","b"]],call_func_args("ulcorner",["piece"])),
                                                                           if_test_body(
                                                                              call_func_args( "isinstance", [
                                                                                subscript_value_slice(call_func_args("next",[call_func_args("iter",["piece"])]),1),
                                                                                "tuple"]),
                                                                              body_stmts( [ return_value( call_func_args( "frozenset", [
                                                                                                                        generator_exp_elt_generators(
                                                                                                                           [ "v",
                                                                                                                             [ bin_op_left_right(add_token(+),bin_op_left_right(sub_token(-),"j","b"),"a"),
                                                                                                                               bin_op_left_right(add_token(+),bin_op_left_right(sub_token(-),"i","a"),"b")]],
                                                                                                                           [ comprehension_target_iter(["v",["i","j"]],"piece")])]))])
                                                                         ),
                                                                           return_value( call_func_args( "frozenset", [
                                                                                                 generator_exp_elt_generators(
                                                                                                    [ bin_op_left_right(add_token(+),bin_op_left_right(sub_token(-),"j","b"),"a"),
                                                                                                      bin_op_left_right(add_token(+),bin_op_left_right(sub_token(-),"i","a"),"b")],
                                                                                                    [ comprehension_target_iter(["i","j"],"piece")])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "cmirror",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("piece")],[argument_type("Piece")]),
                                                 block_statements( [ expr_value(string_value(' mirroring along counterdiagonal ')),
                                                                           if_test_body(
                                                                              call_func_args("isinstance",["piece","tuple"]),
                                                                              body_stmts( [ return_value( call_func_args( "tuple", [
                                                                                                                        call_func_args( "zip", [
                                                                                                                          starred_value( generator_exp_elt_generators(
                                                                                                                                                  subscript_value_slice("r",slice_step(unary_op_operand(us_ub_token(-),1))),
                                                                                                                                                  [ comprehension_target_iter("r",subscript_value_slice("piece",slice_step(unary_op_operand(us_ub_token(-),1))))]))])]))])
                                                                         ),
                                                                           return_value( call_func_args("vmirror",[call_func_args("dmirror",[call_func_args("vmirror",["piece"])])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "fill",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("grid"), argument_name("value"),argument_name("patch")],
                                                    [argument_type("Grid")]),
                                                 block_statements( [ expr_value(string_value(' fill value at indices ')),
                                                                           assign_targets_value( [ [ "h","w"]], [
                                                                             call_func_args("len",["grid"]),
                                                                             call_func_args("len",[subscript_value_slice("grid",0)])]),
                                                                           assign_targets_value( ["grid_filled"],
                                                                             call_func_args( "list", [
                                                                               generator_exp_elt_generators(call_func_args("list",["row"]),[comprehension_target_iter("row","grid")])])),
                                                                           for_target_iter_body( ["i","j"],
                                                                             call_func_args("toindices",["patch"]),
                                                                             body_stmts( [ if_test_body(
                                                                                                    bool_op_values( ['python:And'], [
                                                                                                      compare_ops_left_comparators(ops([lt_e_token(<=),lt_token(<)]),0,comparators(["i","h"])),
                                                                                                      compare_ops_left_comparators(
                                                                                                         ops([lt_e_token(<=),lt_token(<)]), 0,comparators(["j","w"]))]),
                                                                                                    body_stmts( [ assign_targets_value([subscript_value_slice(subscript_value_slice("grid_filled","i"),"j")],"value")])
                                                                                                 )])
                                                                         ),
                                                                           return_value( call_func_args( "tuple", [
                                                                                                 generator_exp_elt_generators(
                                                                                                    call_func_args("tuple",["row"]),
                                                                                                    [comprehension_target_iter("row","grid_filled")])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "paint",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("grid"),
                                                      argument_name("obj")],
                                                    [argument_type("Grid")]),
                                                 block_statements( [ expr_value(string_value(' paint object to grid ')),
                                                                           assign_targets_value( [ [ "h","w"]], [
                                                                             call_func_args("len",["grid"]),
                                                                             call_func_args("len",[subscript_value_slice("grid",0)])]),
                                                                           assign_targets_value( ["grid_painted"],
                                                                             call_func_args( "list", [
                                                                               generator_exp_elt_generators(call_func_args("list",["row"]),[comprehension_target_iter("row","grid")])])),
                                                                           for_target_iter_body(
                                                                              [ "value",
                                                                                ["i","j"]],
                                                                              "obj",
                                                                              body_stmts( [ if_test_body(
                                                                                                     bool_op_values( ['python:And'], [
                                                                                                       compare_ops_left_comparators(ops([lt_e_token(<=),lt_token(<)]),0,comparators(["i","h"])),
                                                                                                       compare_ops_left_comparators(
                                                                                                          ops([lt_e_token(<=),lt_token(<)]), 0,comparators(["j","w"]))]),
                                                                                                     body_stmts( [ assign_targets_value([subscript_value_slice(subscript_value_slice("grid_painted","i"),"j")],"value")])
                                                                                                  )])
                                                                         ),
                                                                           return_value( call_func_args( "tuple", [
                                                                                                 generator_exp_elt_generators(
                                                                                                    call_func_args("tuple",["row"]),
                                                                                                    [comprehension_target_iter("row","grid_painted")])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "underfill",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("grid"), argument_name("value"),argument_name("patch")],
                                                    [argument_type("Grid")]),
                                                 block_statements( [ expr_value(string_value(' fill value at indices that are background ')),
                                                                           assign_targets_value( [ [ "h","w"]], [
                                                                             call_func_args("len",["grid"]),
                                                                             call_func_args("len",[subscript_value_slice("grid",0)])]),
                                                                           assign_targets_value(["bg"],call_func_args("mostcolor",["grid"])),
                                                                           assign_targets_value( ["g"],
                                                                             call_func_args( "list", [
                                                                               generator_exp_elt_generators(call_func_args("list",["r"]),[comprehension_target_iter("r","grid")])])),
                                                                           for_target_iter_body( ["i","j"],
                                                                             call_func_args("toindices",["patch"]),
                                                                             body_stmts( [ if_test_body(
                                                                                                    bool_op_values( ['python:And'], [
                                                                                                      compare_ops_left_comparators(ops([lt_e_token(<=),lt_token(<)]),0,comparators(["i","h"])),
                                                                                                      compare_ops_left_comparators(
                                                                                                         ops([lt_e_token(<=),lt_token(<)]), 0,comparators(["j","w"]))]),
                                                                                                    body_stmts( [ if_test_body(
                                                                                                                           compare_ops_left_comparators( ops([eq_token(==)]),
                                                                                                                             subscript_value_slice(subscript_value_slice("g","i"),"j"),
                                                                                                                             comparators(["bg"])),
                                                                                                                           body_stmts( [ assign_targets_value([subscript_value_slice(subscript_value_slice("g","i"),"j")],"value")])
                                                                                                                        )])
                                                                                                 )])
                                                                         ),
                                                                           return_value( call_func_args( "tuple", [
                                                                                                 generator_exp_elt_generators(call_func_args("tuple",["r"]),[comprehension_target_iter("r","g")])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "underpaint",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("grid"),
                                                      argument_name("obj")],
                                                    [argument_type("Grid")]),
                                                 block_statements( [ expr_value(string_value(' paint object to grid where there is background ')),
                                                                           assign_targets_value( [ [ "h","w"]], [
                                                                             call_func_args("len",["grid"]),
                                                                             call_func_args("len",[subscript_value_slice("grid",0)])]),
                                                                           assign_targets_value(["bg"],call_func_args("mostcolor",["grid"])),
                                                                           assign_targets_value( ["g"],
                                                                             call_func_args( "list", [
                                                                               generator_exp_elt_generators(call_func_args("list",["r"]),[comprehension_target_iter("r","grid")])])),
                                                                           for_target_iter_body(
                                                                              [ "value",
                                                                                ["i","j"]],
                                                                              "obj",
                                                                              body_stmts( [ if_test_body(
                                                                                                     bool_op_values( ['python:And'], [
                                                                                                       compare_ops_left_comparators(ops([lt_e_token(<=),lt_token(<)]),0,comparators(["i","h"])),
                                                                                                       compare_ops_left_comparators(
                                                                                                          ops([lt_e_token(<=),lt_token(<)]), 0,comparators(["j","w"]))]),
                                                                                                     body_stmts( [ if_test_body(
                                                                                                                            compare_ops_left_comparators( ops([eq_token(==)]),
                                                                                                                              subscript_value_slice(subscript_value_slice("g","i"),"j"),
                                                                                                                              comparators(["bg"])),
                                                                                                                            body_stmts( [ assign_targets_value([subscript_value_slice(subscript_value_slice("g","i"),"j")],"value")])
                                                                                                                         )])
                                                                                                  )])
                                                                         ),
                                                                           return_value( call_func_args( "tuple", [
                                                                                                 generator_exp_elt_generators(call_func_args("tuple",["r"]),[comprehension_target_iter("r","g")])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "hupscale",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("grid"),
                                                      argument_name("factor")],
                                                    [argument_type("Grid")]),
                                                 block_statements( [ expr_value(string_value(' upscale grid horizontally ')),
                                                                           assign_targets_value(["g"],call_func("tuple")),
                                                                           for_target_iter_body( "row",
                                                                             "grid",
                                                                             body_stmts( [ assign_targets_value(["r"],call_func("tuple")),
                                                                                                 for_target_iter_body( "value",
                                                                                                   "row",
                                                                                                   body_stmts( [ assign_targets_value( ["r"],
                                                                                                                         bin_op_left_right( add_token(+),
                                                                                                                           "r",
                                                                                                                           call_func_args( "tuple", [
                                                                                                                             generator_exp_elt_generators( "value", [
                                                                                                                               comprehension_target_iter("num",call_func_args("range",["factor"]))])])))])
                                                                                               ),
                                                                                                 assign_targets_value(["g"],bin_op_left_right(add_token(+),"g",["r"]))])
                                                                         ),
                                                                           return_value("g")])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "vupscale",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("grid"),
                                                      argument_name("factor")],
                                                    [argument_type("Grid")]),
                                                 block_statements( [ expr_value(string_value(' upscale grid vertically ')),
                                                                           assign_targets_value(["g"],call_func("tuple")),
                                                                           for_target_iter_body( "row",
                                                                             "grid",
                                                                             body_stmts( [ assign_targets_value( ["g"],
                                                                                                   bin_op_left_right( add_token(+),
                                                                                                     "g",
                                                                                                     call_func_args( "tuple", [
                                                                                                       generator_exp_elt_generators( "row", [
                                                                                                         comprehension_target_iter("num",call_func_args("range",["factor"]))])])))])
                                                                         ),
                                                                           return_value("g")])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "upscale",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("element"),
                                                      argument_name("factor")],
                                                    [argument_type("Element")]),
                                                 block_statements( [ expr_value(string_value(' upscale object or grid ')),
                                                                           if_test_body_orelse(
                                                                              call_func_args("isinstance",["element","tuple"]),
                                                                              body_stmts( [ assign_targets_value(["g"],call_func("tuple")),
                                                                                                  for_target_iter_body( "row",
                                                                                                    "element",
                                                                                                    body_stmts( [ assign_targets_value(["upscaled_row"],call_func("tuple")),
                                                                                                                        for_target_iter_body( "value",
                                                                                                                          "row",
                                                                                                                          body_stmts( [ assign_targets_value( ["upscaled_row"],
                                                                                                                                                bin_op_left_right( add_token(+),
                                                                                                                                                  "upscaled_row",
                                                                                                                                                  call_func_args( "tuple", [
                                                                                                                                                    generator_exp_elt_generators( "value", [
                                                                                                                                                      comprehension_target_iter("num",call_func_args("range",["factor"]))])])))])
                                                                                                                      ),
                                                                                                                        assign_targets_value( ["g"],
                                                                                                                          bin_op_left_right( add_token(+),
                                                                                                                            "g",
                                                                                                                            call_func_args( "tuple", [
                                                                                                                              generator_exp_elt_generators( "upscaled_row", [
                                                                                                                                comprehension_target_iter("num",call_func_args("range",["factor"]))])])))])
                                                                                                ),
                                                                                                  return_value("g")])
                                                                         ,    orelse_else_stmts( [ if_test_body(
                                                                                                            compare_ops_left_comparators( ops([eq_token(==)]),
                                                                                                              call_func_args("len",["element"]),
                                                                                                              comparators([0])),
                                                                                                            body_stmts([return_value(call_func("frozenset"))])),
                                                                                                         assign_targets_value([["di_inv","dj_inv"]],call_func_args("ulcorner",["element"])),
                                                                                                         assign_targets_value( [ [ "di","dj"]], [
                                                                                                           unary_op_operand(us_ub_token(-),"di_inv"),
                                                                                                           unary_op_operand(us_ub_token(-),"dj_inv")]),
                                                                                                         assign_targets_value( ["normed_obj"],
                                                                                                           call_func_args("shift",["element",["di","dj"]])),
                                                                                                         assign_targets_value(["o"],call_func("set")),
                                                                                                         for_target_iter_body(
                                                                                                            [ "value",
                                                                                                              ["i","j"]],
                                                                                                            "normed_obj",
                                                                                                            body_stmts( [ for_target_iter_body( "io",
                                                                                                                                  call_func_args("range",["factor"]),
                                                                                                                                  body_stmts( [ for_target_iter_body( "jo",
                                                                                                                                                        call_func_args("range",["factor"]),
                                                                                                                                                        body_stmts( [ expr_value( call_func_args(
                                                                                                                                                                                                 qualified_identifier_identifiers(["o",boxed_attribute_value("add")]),
                                                                                                                                                                                                 [ [ "value",
                                                                                                                                                                                                     [ bin_op_left_right(add_token(+),bin_op_left_right(mult_token(*),"i","factor"),"io"),
                                                                                                                                                                                                       bin_op_left_right(add_token(+),bin_op_left_right(mult_token(*),"j","factor"),"jo")]]]))])
                                                                                                                                                      )])
                                                                                                                                )])
                                                                                                       ),
                                                                                                         return_value( call_func_args("shift",[call_func_args("frozenset",["o"]),["di_inv","dj_inv"]]))
                                                                                                       ])
                                                                         )])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "downscale",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("grid"),
                                                      argument_name("factor")],
                                                    [argument_type("Grid")]),
                                                 block_statements( [ expr_value(string_value(' downscale grid ')),
                                                                           assign_targets_value( [ [ "h","w"]], [
                                                                             call_func_args("len",["grid"]),
                                                                             call_func_args("len",[subscript_value_slice("grid",0)])]),
                                                                           assign_targets_value(["g"],call_func("tuple")),
                                                                           for_target_iter_body( "i",
                                                                             call_func_args("range",["h"]),
                                                                             body_stmts( [ assign_targets_value(["r"],call_func("tuple")),
                                                                                                 for_target_iter_body( "j",
                                                                                                   call_func_args("range",["w"]),
                                                                                                   body_stmts( [ if_test_body(
                                                                                                                          compare_ops_left_comparators( ops([eq_token(==)]),
                                                                                                                            bin_op_left_right(mod_token('%'),"j","factor"),
                                                                                                                            comparators([0])),
                                                                                                                          body_stmts( [ assign_targets_value( ["r"],
                                                                                                                                                bin_op_left_right(add_token(+),"r",[subscript_value_slice(subscript_value_slice("grid","i"),"j")]))])
                                                                                                                       )])
                                                                                               ),
                                                                                                 assign_targets_value(["g"],bin_op_left_right(add_token(+),"g",["r"]))])
                                                                         ),
                                                                           assign_targets_value(["h"],call_func_args("len",["g"])),
                                                                           assign_targets_value(["dsg"],call_func("tuple")),
                                                                           for_target_iter_body( "i",
                                                                             call_func_args("range",["h"]),
                                                                             body_stmts( [ if_test_body(
                                                                                                    compare_ops_left_comparators( ops([eq_token(==)]),
                                                                                                      bin_op_left_right(mod_token('%'),"i","factor"),
                                                                                                      comparators([0])),
                                                                                                    body_stmts( [ assign_targets_value(["dsg"],bin_op_left_right(add_token(+),"dsg",[subscript_value_slice("g","i")]))])
                                                                                                 )])
                                                                         ),
                                                                           return_value("dsg")])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "hconcat",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("a"),argument_name("b")],[argument_type("Grid")]),
                                                 block_statements( [ expr_value(string_value(' concatenate two grids horizontally ')),
                                                                           return_value( call_func_args( "tuple", [
                                                                                                 generator_exp_elt_generators(
                                                                                                    bin_op_left_right(add_token(+),"i","j"),
                                                                                                    [ comprehension_target_iter(["i","j"],call_func_args("zip",["a","b"]))])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "vconcat",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("a"),argument_name("b")],[argument_type("Grid")]),
                                                 block_statements( [ expr_value(string_value(' concatenate two grids vertically ')),
                                                                           return_value(bin_op_left_right(add_token(+),"a","b"))])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "subgrid",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("patch"),
                                                      argument_name("grid")],
                                                    [argument_type("Grid")]),
                                                 block_statements( [ expr_value(string_value(' smallest subgrid containing object ')),
                                                                           return_value( call_func_args( "crop", [
                                                                                                 "grid",
                                                                                                 call_func_args("ulcorner",["patch"]),
                                                                                                 call_func_args("shape",["patch"])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "hsplit",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("grid"),
                                                      argument_name("n")],
                                                    [argument_type("Tuple")]),
                                                 block_statements( [ expr_value(string_value(' split grid horizontally ')),
                                                                           assign_targets_value( [ [ "h","w"]], [
                                                                             call_func_args("len",["grid"]),
                                                                             bin_op_left_right(floor_div_token(//),call_func_args("len",[subscript_value_slice("grid",0)]),"n")]),
                                                                           assign_targets_value( ["offset"],
                                                                             compare_ops_left_comparators( ops([not_eq_token('!=')]),
                                                                               bin_op_left_right(mod_token('%'),call_func_args("len",[subscript_value_slice("grid",0)]),"n"),
                                                                               comparators([0]))),
                                                                           return_value( call_func_args( "tuple", [
                                                                                                 generator_exp_elt_generators(
                                                                                                    call_func_args( "crop", [
                                                                                                      "grid",
                                                                                                      [ 0,
                                                                                                        bin_op_left_right( add_token(+),
                                                                                                          bin_op_left_right(mult_token(*),"w","i"),
                                                                                                          bin_op_left_right(mult_token(*),"i","offset"))],
                                                                                                      ["h","w"]]),
                                                                                                    [ comprehension_target_iter("i",call_func_args("range",["n"]))])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "vsplit",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("grid"),
                                                      argument_name("n")],
                                                    [argument_type("Tuple")]),
                                                 block_statements( [ expr_value(string_value(' split grid vertically ')),
                                                                           assign_targets_value( [ [ "h","w"]], [
                                                                             bin_op_left_right(floor_div_token(//),call_func_args("len",["grid"]),"n"),
                                                                             call_func_args("len",[subscript_value_slice("grid",0)])]),
                                                                           assign_targets_value( ["offset"],
                                                                             compare_ops_left_comparators( ops([not_eq_token('!=')]),
                                                                               bin_op_left_right(mod_token('%'),call_func_args("len",["grid"]),"n"),
                                                                               comparators([0]))),
                                                                           return_value( call_func_args( "tuple", [
                                                                                                 generator_exp_elt_generators(
                                                                                                    call_func_args( "crop", [
                                                                                                      "grid",
                                                                                                      [ bin_op_left_right(add_token(+),bin_op_left_right(mult_token(*),"h","i"),bin_op_left_right(mult_token(*),"i","offset")),
                                                                                                        0],
                                                                                                      ["h","w"]]),
                                                                                                    [ comprehension_target_iter("i",call_func_args("range",["n"]))])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "cellwise",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("a"), argument_name("b"),argument_name("fallback")],
                                                    [argument_type("Grid")]),
                                                 block_statements( [ expr_value(string_value(' cellwise match of two grids ')),
                                                                           assign_targets_value( [ [ "h","w"]], [
                                                                             call_func_args("len",["a"]),
                                                                             call_func_args("len",[subscript_value_slice("a",0)])]),
                                                                           assign_targets_value(["resulting_grid"],call_func("tuple")),
                                                                           for_target_iter_body( "i",
                                                                             call_func_args("range",["h"]),
                                                                             body_stmts( [ assign_targets_value(["row"],call_func("tuple")),
                                                                                                 for_target_iter_body( "j",
                                                                                                   call_func_args("range",["w"]),
                                                                                                   body_stmts( [ assign_targets_value(["a_value"],subscript_value_slice(subscript_value_slice("a","i"),"j")),
                                                                                                                       assign_targets_value( ["value"],
                                                                                                                         if_exp_test_body_orelse(
                                                                                                                            compare_ops_left_comparators( ops([eq_token(==)]),
                                                                                                                              "a_value",
                                                                                                                              comparators([subscript_value_slice(subscript_value_slice("b","i"),"j")])), "a_value","fallback")),
                                                                                                                       assign_targets_value(["row"],bin_op_left_right(add_token(+),"row",["value"]))])
                                                                                               ),
                                                                                                 assign_targets_value(["resulting_grid"],bin_op_left_right(add_token(+),"resulting_grid",["row"]))])
                                                                         ),
                                                                           return_value("resulting_grid")])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "replace",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("grid"), argument_name("replacee"),argument_name("replacer")],
                                                    [argument_type("Grid")]),
                                                 block_statements( [ expr_value(string_value(' color substitution ')),
                                                                           return_value( call_func_args( "tuple", [
                                                                                                 generator_exp_elt_generators(
                                                                                                    call_func_args( "tuple", [
                                                                                                      generator_exp_elt_generators(
                                                                                                         if_exp_test_body_orelse(
                                                                                                            compare_ops_left_comparators(ops([eq_token(==)]),"v",comparators(["replacee"])), "replacer","v"),
                                                                                                         [comprehension_target_iter("v","r")])]),
                                                                                                    [comprehension_target_iter("r","grid")])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "switch",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("grid"), argument_name("a"),argument_name("b")],
                                                    [argument_type("Grid")]),
                                                 block_statements( [ expr_value(string_value(' color switching ')),
                                                                           return_value( call_func_args( "tuple", [
                                                                                                 generator_exp_elt_generators(
                                                                                                    call_func_args( "tuple", [
                                                                                                      generator_exp_elt_generators(
                                                                                                         if_exp_test_body_orelse(
                                                                                                            bool_op_values( ['python:And'], [
                                                                                                              compare_ops_left_comparators(ops([not_eq_token('!=')]),"v",comparators(["a"])),
                                                                                                              compare_ops_left_comparators(ops([not_eq_token('!=')]),"v",comparators(["b"]))]),
                                                                                                            "v",
                                                                                                            subscript_value_slice(dict_keys_values(["a","b"],["b","a"]),"v")),
                                                                                                         [comprehension_target_iter("v","r")])]),
                                                                                                    [comprehension_target_iter("r","grid")])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "center",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("patch")],[argument_type("IntegerTuple")]),
                                                 block_statements( [ expr_value(string_value(' center of the patch ')),
                                                                           return_value( [ bin_op_left_right( add_token(+),
                                                                                                   call_func_args("uppermost",["patch"]),
                                                                                                   bin_op_left_right(floor_div_token(//),call_func_args("height",["patch"]),2)),
                                                                                                 bin_op_left_right( add_token(+),
                                                                                                   call_func_args("leftmost",["patch"]),
                                                                                                   bin_op_left_right(floor_div_token(//),call_func_args("width",["patch"]),2))])
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "position",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("a"),
                                                      argument_name("b")],
                                                    [argument_type("IntegerTuple")]),
                                                 block_statements( [ expr_value(string_value(' relative position between two patches ')),
                                                                           assign_targets_value( [ [ "ia","ja"]],
                                                                             call_func_args("center",[call_func_args("toindices",["a"])])),
                                                                           assign_targets_value( [ [ "ib","jb"]],
                                                                             call_func_args("center",[call_func_args("toindices",["b"])])),
                                                                           if_test_body_orelse(
                                                                              compare_ops_left_comparators(ops([eq_token(==)]),"ia",comparators(["ib"])),
                                                                              body_stmts( [ return_value( [ 0,
                                                                                                                        if_exp_test_body_orelse(
                                                                                                                           compare_ops_left_comparators(ops([lt_token(<)]),"ja",comparators(["jb"])), 1,unary_op_operand(us_ub_token(-),1))])])
                                                                         ,    orelse_else_stmts( [ if_test_body_orelse(
                                                                                                            compare_ops_left_comparators(ops([eq_token(==)]),"ja",comparators(["jb"])),
                                                                                                            body_stmts( [ return_value( [ if_exp_test_body_orelse(
                                                                                                                                                         compare_ops_left_comparators(ops([lt_token(<)]),"ia",comparators(["ib"])), 1,unary_op_operand(us_ub_token(-),1)),
                                                                                                                                                      0])])
                                                                                                         ,  orelse_else_stmts( [ if_test_body_orelse(
                                                                                                                                          compare_ops_left_comparators(ops([lt_token(<)]),"ia",comparators(["ib"])),
                                                                                                                                          body_stmts( [ return_value( [ 1,
                                                                                                                                                                                    if_exp_test_body_orelse(
                                                                                                                                                                                       compare_ops_left_comparators(ops([lt_token(<)]),"ja",comparators(["jb"])), 1,unary_op_operand(us_ub_token(-),1))])])
                                                                                                                                       ,  orelse_else_stmts( [ if_test_body(
                                                                                                                                                                        compare_ops_left_comparators(ops([gt_token(>)]),"ia",comparators(["ib"])),
                                                                                                                                                                        body_stmts( [ return_value( [ unary_op_operand(us_ub_token(-),1),
                                                                                                                                                                                                                  if_exp_test_body_orelse(
                                                                                                                                                                                                                     compare_ops_left_comparators(ops([lt_token(<)]),"ja",comparators(["jb"])), 1,unary_op_operand(us_ub_token(-),1))])])
                                                                                                                                                                     )])
                                                                                                                                       )])
                                                                                                         )])
                                                                         )])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "index",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("grid"),
                                                      argument_name("loc")],
                                                    [argument_type("Integer")]),
                                                 block_statements( [ expr_value(string_value(' color at location ')),
                                                                           assign_targets_value([["i","j"]],"loc"),
                                                                           assign_targets_value( [ [ "h","w"]], [
                                                                             call_func_args("len",["grid"]),
                                                                             call_func_args("len",[subscript_value_slice("grid",0)])]),
                                                                           if_test_body(
                                                                              unary_op_operand( ['python:Not'],
                                                                                bool_op_values( ['python:And'], [
                                                                                  compare_ops_left_comparators(ops([lt_e_token(<=),lt_token(<)]),0,comparators(["i","h"])),
                                                                                  compare_ops_left_comparators(
                                                                                     ops([lt_e_token(<=),lt_token(<)]), 0,comparators(["j","w"]))])),
                                                                              body_stmts([return_value(none_literal_value_token('None','None'))])),
                                                                           return_value( subscript_value_slice(subscript_value_slice("grid",subscript_value_slice("loc",0)),subscript_value_slice("loc",1)))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "canvas",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("value"),
                                                      argument_name("dimensions")],
                                                    [argument_type("Grid")]),
                                                 block_statements( [ expr_value(string_value(' grid construction ')),
                                                                           return_value( call_func_args( "tuple", [
                                                                                                 generator_exp_elt_generators(
                                                                                                    call_func_args( "tuple", [
                                                                                                      generator_exp_elt_generators( "value", [
                                                                                                        comprehension_target_iter("j",call_func_args("range",[subscript_value_slice("dimensions",1)]))])]),
                                                                                                    [ comprehension_target_iter("i",call_func_args("range",[subscript_value_slice("dimensions",0)]))])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "corners",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("patch")],[argument_type("Indices")]),
                                                 block_statements( [ expr_value(string_value(' indices of corners ')),
                                                                           return_value( call_func_args( "frozenset", [
                                                                                                 set_elts( [ call_func_args("ulcorner",["patch"]),
                                                                                                                   call_func_args("urcorner",["patch"]),
                                                                                                                   call_func_args("llcorner",["patch"]),
                                                                                                                   call_func_args("lrcorner",["patch"])])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "connect",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("a"),
                                                      argument_name("b")],
                                                    [argument_type("Indices")]),
                                                 block_statements( [ expr_value(string_value(' line between two points ')),
                                                                           assign_targets_value([["ai","aj"]],"a"),
                                                                           assign_targets_value([["bi","bj"]],"b"),
                                                                           assign_targets_value(["si"],call_func_args("min",["ai","bi"])),
                                                                           assign_targets_value( ["ei"],
                                                                             bin_op_left_right(add_token(+),call_func_args("max",["ai","bi"]),1)),
                                                                           assign_targets_value(["sj"],call_func_args("min",["aj","bj"])),
                                                                           assign_targets_value( ["ej"],
                                                                             bin_op_left_right(add_token(+),call_func_args("max",["aj","bj"]),1)),
                                                                           if_test_body_orelse(
                                                                              compare_ops_left_comparators(ops([eq_token(==)]),"ai",comparators(["bi"])),
                                                                              body_stmts( [ return_value( call_func_args( "frozenset", [
                                                                                                                        generator_exp_elt_generators( ["ai","j"], [
                                                                                                                          comprehension_target_iter("j",call_func_args("range",["sj","ej"]))])]))])
                                                                         ,    orelse_else_stmts( [ if_test_body_orelse(
                                                                                                            compare_ops_left_comparators(ops([eq_token(==)]),"aj",comparators(["bj"])),
                                                                                                            body_stmts( [ return_value( call_func_args( "frozenset", [
                                                                                                                                                      generator_exp_elt_generators( ["i","aj"], [
                                                                                                                                                        comprehension_target_iter("i",call_func_args("range",["si","ei"]))])]))])
                                                                                                         ,  orelse_else_stmts( [ if_test_body_orelse(
                                                                                                                                          compare_ops_left_comparators( ops([eq_token(==)]),
                                                                                                                                            bin_op_left_right(sub_token(-),"bi","ai"),
                                                                                                                                            comparators([bin_op_left_right(sub_token(-),"bj","aj")])),
                                                                                                                                          body_stmts( [ return_value( call_func_args( "frozenset", [
                                                                                                                                                                                    generator_exp_elt_generators( ["i","j"], [
                                                                                                                                                                                      comprehension_target_iter( ["i","j"],
                                                                                                                                                                                        call_func_args( "zip", [
                                                                                                                                                                                          call_func_args("range",["si","ei"]),
                                                                                                                                                                                          call_func_args("range",["sj","ej"])]))])]))])
                                                                                                                                       ,  orelse_else_stmts( [ if_test_body(
                                                                                                                                                                        compare_ops_left_comparators( ops([eq_token(==)]),
                                                                                                                                                                          bin_op_left_right(sub_token(-),"bi","ai"),
                                                                                                                                                                          comparators([bin_op_left_right(sub_token(-),"aj","bj")])),
                                                                                                                                                                        body_stmts( [ return_value( call_func_args( "frozenset", [
                                                                                                                                                                                                                  generator_exp_elt_generators( ["i","j"], [
                                                                                                                                                                                                                    comprehension_target_iter( ["i","j"],
                                                                                                                                                                                                                      call_func_args( "zip", [
                                                                                                                                                                                                                        call_func_args("range",["si","ei"]),
                                                                                                                                                                                                                        call_func_args( "range", [
                                                                                                                                                                                                                          bin_op_left_right(sub_token(-),"ej",1),
                                                                                                                                                                                                                          bin_op_left_right(sub_token(-),"sj",1),
                                                                                                                                                                                                                          unary_op_operand(us_ub_token(-),1)])]))])]))])
                                                                                                                                                                     )])
                                                                                                                                       )])
                                                                                                         )])
                                                                         ),
                                                                           return_value(call_func("frozenset"))])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "cover",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("grid"),
                                                      argument_name("patch")],
                                                    [argument_type("Grid")]),
                                                 block_statements( [ expr_value(string_value(' remove object from grid ')),
                                                                           return_value( call_func_args( "fill", [
                                                                                                 "grid",
                                                                                                 call_func_args("mostcolor",["grid"]),
                                                                                                 call_func_args("toindices",["patch"])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "trim",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("grid")],[argument_type("Grid")]),
                                                 block_statements( [ expr_value(string_value(' trim border of grid ')),
                                                                           return_value( call_func_args( "tuple", [
                                                                                                 generator_exp_elt_generators(
                                                                                                    subscript_value_slice("r",slice_lower_upper(1,unary_op_operand(us_ub_token(-),1))),
                                                                                                    [ comprehension_target_iter("r",subscript_value_slice("grid",slice_lower_upper(1,unary_op_operand(us_ub_token(-),1))))])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "move",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("grid"), argument_name("obj"),argument_name("offset")],
                                                    [argument_type("Grid")]),
                                                 block_statements( [ expr_value(string_value(' move object on grid ')),
                                                                           return_value( call_func_args( "paint", [
                                                                                                 call_func_args("cover",["grid","obj"]),
                                                                                                 call_func_args("shift",["obj","offset"])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "tophalf",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("grid")],[argument_type("Grid")]),
                                                 block_statements( [ expr_value(string_value(' upper half of grid ')),
                                                                           return_value( subscript_value_slice( "grid",
                                                                                                 slice_upper(bin_op_left_right(floor_div_token(//),call_func_args("len",["grid"]),2))))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "bottomhalf",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("grid")],[argument_type("Grid")]),
                                                 block_statements( [ expr_value(string_value(' lower half of grid ')),
                                                                           return_value( subscript_value_slice( "grid",
                                                                                                 slice_lower( bin_op_left_right( add_token(+),
                                                                                                                      bin_op_left_right(floor_div_token(//),call_func_args("len",["grid"]),2),
                                                                                                                      bin_op_left_right(mod_token('%'),call_func_args("len",["grid"]),2)))))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "lefthalf",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("grid")],[argument_type("Grid")]),
                                                 block_statements( [ expr_value(string_value(' left half of grid ')),
                                                                           return_value( call_func_args("rot270",[call_func_args("tophalf",[call_func_args("rot90",["grid"])])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "righthalf",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("grid")],[argument_type("Grid")]),
                                                 block_statements( [ expr_value(string_value(' right half of grid ')),
                                                                           return_value( call_func_args("rot270",[call_func_args("bottomhalf",[call_func_args("rot90",["grid"])])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "vfrontier",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("location")],[argument_type("Indices")]),
                                                 block_statements( [ expr_value(string_value(' vertical frontier ')),
                                                                           return_value( call_func_args( "frozenset", [
                                                                                                 generator_exp_elt_generators(
                                                                                                    [ "i",
                                                                                                      subscript_value_slice("location",1)],
                                                                                                    [ comprehension_target_iter("i",call_func_args("range",[30]))])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "hfrontier",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("location")],[argument_type("Indices")]),
                                                 block_statements( [ expr_value(string_value(' horizontal frontier ')),
                                                                           return_value( call_func_args( "frozenset", [
                                                                                                 generator_exp_elt_generators(
                                                                                                    [ subscript_value_slice("location",0),
                                                                                                      "j"],
                                                                                                    [ comprehension_target_iter("j",call_func_args("range",[30]))])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "backdrop",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("patch")],[argument_type("Indices")]),
                                                 block_statements( [ expr_value(string_value(' indices in bounding box of patch ')),
                                                                           assign_targets_value(["indices"],call_func_args("toindices",["patch"])),
                                                                           assign_targets_value([["si","sj"]],call_func_args("ulcorner",["indices"])),
                                                                           assign_targets_value([["ei","ej"]],call_func_args("lrcorner",["patch"])),
                                                                           return_value( call_func_args( "frozenset", [
                                                                                                 generator_exp_elt_generators( ["i","j"], [
                                                                                                   comprehension_target_iter("i",call_func_args("range",["si",bin_op_left_right(add_token(+),"ei",1)])),
                                                                                                   comprehension_target_iter("j",call_func_args("range",["sj",bin_op_left_right(add_token(+),"ej",1)]))])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "delta",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("patch")],[argument_type("Indices")]),
                                                 block_statements( [ expr_value(string_value(' indices in bounding box but not part of patch ')),
                                                                           return_value( bin_op_left_right( sub_token(-),
                                                                                                 call_func_args("backdrop",["patch"]),
                                                                                                 call_func_args("toindices",["patch"])))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "gravitate",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("source"),
                                                      argument_name("destination")],
                                                    [argument_type("IntegerTuple")]),
                                                 block_statements( [ expr_value(string_value(' direction to move source until adjacent to destination ')),
                                                                           assign_targets_value([["si","sj"]],call_func_args("center",["source"])),
                                                                           assign_targets_value([["di","dj"]],call_func_args("center",["destination"])),
                                                                           assign_targets_value([["i","j"]],[0,0]),
                                                                           if_test_body_orelse(
                                                                              call_func_args("vmatching",["source","destination"]),
                                                                              body_stmts( [ assign_targets_value( ["i"],
                                                                                                    if_exp_test_body_orelse(
                                                                                                       compare_ops_left_comparators(ops([lt_token(<)]),"si",comparators(["di"])), 1,unary_op_operand(us_ub_token(-),1)))])
                                                                         ,    orelse_else_stmts( [ assign_targets_value( ["j"],
                                                                                                           if_exp_test_body_orelse(
                                                                                                              compare_ops_left_comparators(ops([lt_token(<)]),"sj",comparators(["dj"])), 1,unary_op_operand(us_ub_token(-),1)))])
                                                                         ),
                                                                           assign_targets_value([["gi","gj"]],["i","j"]),
                                                                           assign_targets_value(["c"],0),
                                                                           while_test_body(
                                                                              bool_op_values( ['python:And'], [
                                                                                unary_op_operand(['python:Not'],call_func_args("adjacent",["source","destination"])),
                                                                                compare_ops_left_comparators(ops([lt_token(<)]),"c",comparators([42]))]),
                                                                              body_stmts( [ aug_assign_op_target_value(add_token(+),"c",1),
                                                                                                  aug_assign_op_target_value(add_token(+),"gi","i"),
                                                                                                  aug_assign_op_target_value(add_token(+),"gj","j"),
                                                                                                  assign_targets_value(["source"],call_func_args("shift",["source",["i","j"]]))])
                                                                         ),
                                                                           return_value([bin_op_left_right(sub_token(-),"gi","i"),bin_op_left_right(sub_token(-),"gj","j")])])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "inbox",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("patch")],[argument_type("Indices")]),
                                                 block_statements( [ expr_value(string_value(' inbox for patch ')),
                                                                           assign_targets_value( [ [ "ai","aj"]], [
                                                                             bin_op_left_right(add_token(+),call_func_args("uppermost",["patch"]),1),
                                                                             bin_op_left_right(add_token(+),call_func_args("leftmost",["patch"]),1)]),
                                                                           assign_targets_value( [ [ "bi","bj"]], [
                                                                             bin_op_left_right(sub_token(-),call_func_args("lowermost",["patch"]),1),
                                                                             bin_op_left_right(sub_token(-),call_func_args("rightmost",["patch"]),1)]),
                                                                           assign_targets_value( [ [ "si","sj"]], [
                                                                             call_func_args("min",["ai","bi"]),
                                                                             call_func_args("min",["aj","bj"])]),
                                                                           assign_targets_value( [ [ "ei","ej"]], [
                                                                             call_func_args("max",["ai","bi"]),
                                                                             call_func_args("max",["aj","bj"])]),
                                                                           assign_targets_value( ["vlines"],
                                                                             bin_op_left_right( bit_or_token('|'),
                                                                               set_comp_elt_generators( ["i","sj"], [
                                                                                 comprehension_target_iter("i",call_func_args("range",["si",bin_op_left_right(add_token(+),"ei",1)]))]),
                                                                               set_comp_elt_generators( ["i","ej"], [
                                                                                 comprehension_target_iter("i",call_func_args("range",["si",bin_op_left_right(add_token(+),"ei",1)]))]))),
                                                                           assign_targets_value( ["hlines"],
                                                                             bin_op_left_right( bit_or_token('|'),
                                                                               set_comp_elt_generators( ["si","j"], [
                                                                                 comprehension_target_iter("j",call_func_args("range",["sj",bin_op_left_right(add_token(+),"ej",1)]))]),
                                                                               set_comp_elt_generators( ["ei","j"], [
                                                                                 comprehension_target_iter("j",call_func_args("range",["sj",bin_op_left_right(add_token(+),"ej",1)]))]))),
                                                                           return_value(call_func_args("frozenset",[bin_op_left_right(bit_or_token('|'),"vlines","hlines")]))])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "outbox",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("patch")],[argument_type("Indices")]),
                                                 block_statements( [ expr_value(string_value(' outbox for patch ')),
                                                                           assign_targets_value( [ [ "ai","aj"]], [
                                                                             bin_op_left_right(sub_token(-),call_func_args("uppermost",["patch"]),1),
                                                                             bin_op_left_right(sub_token(-),call_func_args("leftmost",["patch"]),1)]),
                                                                           assign_targets_value( [ [ "bi","bj"]], [
                                                                             bin_op_left_right(add_token(+),call_func_args("lowermost",["patch"]),1),
                                                                             bin_op_left_right(add_token(+),call_func_args("rightmost",["patch"]),1)]),
                                                                           assign_targets_value( [ [ "si","sj"]], [
                                                                             call_func_args("min",["ai","bi"]),
                                                                             call_func_args("min",["aj","bj"])]),
                                                                           assign_targets_value( [ [ "ei","ej"]], [
                                                                             call_func_args("max",["ai","bi"]),
                                                                             call_func_args("max",["aj","bj"])]),
                                                                           assign_targets_value( ["vlines"],
                                                                             bin_op_left_right( bit_or_token('|'),
                                                                               set_comp_elt_generators( ["i","sj"], [
                                                                                 comprehension_target_iter("i",call_func_args("range",["si",bin_op_left_right(add_token(+),"ei",1)]))]),
                                                                               set_comp_elt_generators( ["i","ej"], [
                                                                                 comprehension_target_iter("i",call_func_args("range",["si",bin_op_left_right(add_token(+),"ei",1)]))]))),
                                                                           assign_targets_value( ["hlines"],
                                                                             bin_op_left_right( bit_or_token('|'),
                                                                               set_comp_elt_generators( ["si","j"], [
                                                                                 comprehension_target_iter("j",call_func_args("range",["sj",bin_op_left_right(add_token(+),"ej",1)]))]),
                                                                               set_comp_elt_generators( ["ei","j"], [
                                                                                 comprehension_target_iter("j",call_func_args("range",["sj",bin_op_left_right(add_token(+),"ej",1)]))]))),
                                                                           return_value(call_func_args("frozenset",[bin_op_left_right(bit_or_token('|'),"vlines","hlines")]))])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "box",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("patch")],[argument_type("Indices")]),
                                                 block_statements( [ expr_value(string_value(' outline of patch ')),
                                                                           assign_targets_value([["ai","aj"]],call_func_args("ulcorner",["patch"])),
                                                                           assign_targets_value([["bi","bj"]],call_func_args("lrcorner",["patch"])),
                                                                           assign_targets_value( [ [ "si","sj"]], [
                                                                             call_func_args("min",["ai","bi"]),
                                                                             call_func_args("min",["aj","bj"])]),
                                                                           assign_targets_value( [ [ "ei","ej"]], [
                                                                             call_func_args("max",["ai","bi"]),
                                                                             call_func_args("max",["aj","bj"])]),
                                                                           assign_targets_value( ["vlines"],
                                                                             bin_op_left_right( bit_or_token('|'),
                                                                               set_comp_elt_generators( ["i","sj"], [
                                                                                 comprehension_target_iter("i",call_func_args("range",["si",bin_op_left_right(add_token(+),"ei",1)]))]),
                                                                               set_comp_elt_generators( ["i","ej"], [
                                                                                 comprehension_target_iter("i",call_func_args("range",["si",bin_op_left_right(add_token(+),"ei",1)]))]))),
                                                                           assign_targets_value( ["hlines"],
                                                                             bin_op_left_right( bit_or_token('|'),
                                                                               set_comp_elt_generators( ["si","j"], [
                                                                                 comprehension_target_iter("j",call_func_args("range",["sj",bin_op_left_right(add_token(+),"ej",1)]))]),
                                                                               set_comp_elt_generators( ["ei","j"], [
                                                                                 comprehension_target_iter("j",call_func_args("range",["sj",bin_op_left_right(add_token(+),"ej",1)]))]))),
                                                                           return_value(call_func_args("frozenset",[bin_op_left_right(bit_or_token('|'),"vlines","hlines")]))])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "shoot",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("start"),
                                                      argument_name("direction")],
                                                    [argument_type("Indices")]),
                                                 block_statements( [ expr_value(string_value(' line from starting point and direction ')),
                                                                           return_value( call_func_args( "connect", [
                                                                                                 "start",
                                                                                                 [ bin_op_left_right( add_token(+),
                                                                                                     subscript_value_slice("start",0),
                                                                                                     bin_op_left_right(mult_token(*),42,subscript_value_slice("direction",0))),
                                                                                                   bin_op_left_right( add_token(+),
                                                                                                     subscript_value_slice("start",1),
                                                                                                     bin_op_left_right(mult_token(*),42,subscript_value_slice("direction",1)))]]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "occurrences",
                                              function_type_body(
                                                 function_type_arguments_returns(
                                                    [ argument_name("grid"),
                                                      argument_name("obj")],
                                                    [argument_type("Indices")]),
                                                 block_statements( [ expr_value(string_value(' locations of occurrences of object in grid ')),
                                                                           assign_targets_value(["occs"],call_func("set")),
                                                                           assign_targets_value(["normed"],call_func_args("normalize",["obj"])),
                                                                           assign_targets_value( [ [ "h","w"]], [
                                                                             call_func_args("len",["grid"]),
                                                                             call_func_args("len",[subscript_value_slice("grid",0)])]),
                                                                           assign_targets_value([["oh","ow"]],call_func_args("shape",["obj"])),
                                                                           assign_targets_value( [ [ "h2","w2"]], [
                                                                             bin_op_left_right(add_token(+),bin_op_left_right(sub_token(-),"h","oh"),1),
                                                                             bin_op_left_right(add_token(+),bin_op_left_right(sub_token(-),"w","ow"),1)]),
                                                                           for_target_iter_body( "i",
                                                                             call_func_args("range",["h2"]),
                                                                             body_stmts( [ for_target_iter_body( "j",
                                                                                                   call_func_args("range",["w2"]),
                                                                                                   body_stmts( [ assign_targets_value(["occurs"],boxed_bool_literal_value(bool_value(true),'True')),
                                                                                                                       for_target_iter_body(
                                                                                                                          [ "v",
                                                                                                                            ["a","b"]],
                                                                                                                          call_func_args("shift",["normed",["i","j"]]),
                                                                                                                          body_stmts( [ if_test_body(
                                                                                                                                                 unary_op_operand( ['python:Not'],
                                                                                                                                                   bool_op_values( ['python:And'], [
                                                                                                                                                     compare_ops_left_comparators(ops([lt_e_token(<=),lt_token(<)]),0,comparators(["a","h"])),
                                                                                                                                                     compare_ops_left_comparators(
                                                                                                                                                        ops([lt_e_token(<=),lt_token(<)]), 0,comparators(["b","w"])),
                                                                                                                                                     compare_ops_left_comparators( ops([eq_token(==)]),
                                                                                                                                                       subscript_value_slice(subscript_value_slice("grid","a"),"b"),
                                                                                                                                                       comparators(["v"]))])),
                                                                                                                                                 body_stmts( [ assign_targets_value(["occurs"],boxed_bool_literal_value(bool_value(false),'False')),
                                                                                                                                                                     ['python:Break']])
                                                                                                                                              )])
                                                                                                                     ),
                                                                                                                       if_test_body( "occurs",
                                                                                                                         body_stmts( [ expr_value( call_func_args(
                                                                                                                                                                  qualified_identifier_identifiers(["occs",boxed_attribute_value("add")]),
                                                                                                                                                                  [ [ "i","j"]]))]))])
                                                                                                 )])
                                                                         ),
                                                                           return_value(call_func_args("frozenset",["occs"]))])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "frontiers",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("grid")],[argument_type("Objects")]),
                                                 block_statements( [ expr_value(string_value(' set of frontiers ')),
                                                                           assign_targets_value( [ [ "h","w"]], [
                                                                             call_func_args("len",["grid"]),
                                                                             call_func_args("len",[subscript_value_slice("grid",0)])]),
                                                                           assign_targets_value( ["row_indices"],
                                                                             call_func_args( "tuple", [
                                                                               generator_exp_elt_generators( "i", [
                                                                                 comprehension_ifs_target_iter(
                                                                                    [ compare_ops_left_comparators( ops([eq_token(==)]),
                                                                                        call_func_args("len",[call_func_args("set",["r"])]),
                                                                                        comparators([1]))],
                                                                                    ["i","r"],
                                                                                    call_func_args("enumerate",["grid"]))])])),
                                                                           assign_targets_value( ["column_indices"],
                                                                             call_func_args( "tuple", [
                                                                               generator_exp_elt_generators( "j", [
                                                                                 comprehension_ifs_target_iter(
                                                                                    [ compare_ops_left_comparators( ops([eq_token(==)]),
                                                                                        call_func_args("len",[call_func_args("set",["c"])]),
                                                                                        comparators([1]))],
                                                                                    ["j","c"],
                                                                                    call_func_args("enumerate",[call_func_args("dmirror",["grid"])]))])])),
                                                                           assign_targets_value( ["hfrontiers"],
                                                                             call_func_args( "frozenset", [
                                                                               set_comp_elt_generators(
                                                                                  call_func_args( "frozenset", [
                                                                                    set_comp_elt_generators(
                                                                                       [ subscript_value_slice(subscript_value_slice("grid","i"),"j"),
                                                                                         ["i","j"]],
                                                                                       [ comprehension_target_iter("j",call_func_args("range",["w"]))])]),
                                                                                  [comprehension_target_iter("i","row_indices")])])),
                                                                           assign_targets_value( ["vfrontiers"],
                                                                             call_func_args( "frozenset", [
                                                                               set_comp_elt_generators(
                                                                                  call_func_args( "frozenset", [
                                                                                    set_comp_elt_generators(
                                                                                       [ subscript_value_slice(subscript_value_slice("grid","i"),"j"),
                                                                                         ["i","j"]],
                                                                                       [ comprehension_target_iter("i",call_func_args("range",["h"]))])]),
                                                                                  [comprehension_target_iter("j","column_indices")])])),
                                                                           return_value(bin_op_left_right(bit_or_token('|'),"hfrontiers","vfrontiers"))])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "compress",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("grid")],[argument_type("Grid")]),
                                                 block_statements( [ expr_value(string_value(' removes frontiers from grid ')),
                                                                           assign_targets_value( ["ri"],
                                                                             call_func_args( "tuple", [
                                                                               generator_exp_elt_generators( "i", [
                                                                                 comprehension_ifs_target_iter(
                                                                                    [ compare_ops_left_comparators( ops([eq_token(==)]),
                                                                                        call_func_args("len",[call_func_args("set",["r"])]),
                                                                                        comparators([1]))],
                                                                                    ["i","r"],
                                                                                    call_func_args("enumerate",["grid"]))])])),
                                                                           assign_targets_value( ["ci"],
                                                                             call_func_args( "tuple", [
                                                                               generator_exp_elt_generators( "j", [
                                                                                 comprehension_ifs_target_iter(
                                                                                    [ compare_ops_left_comparators( ops([eq_token(==)]),
                                                                                        call_func_args("len",[call_func_args("set",["c"])]),
                                                                                        comparators([1]))],
                                                                                    ["j","c"],
                                                                                    call_func_args("enumerate",[call_func_args("dmirror",["grid"])]))])])),
                                                                           return_value( call_func_args( "tuple", [
                                                                                                 generator_exp_elt_generators(
                                                                                                    call_func_args( "tuple", [
                                                                                                      generator_exp_elt_generators( "v", [
                                                                                                        comprehension_ifs_target_iter(
                                                                                                           [ compare_ops_left_comparators(ops([['python:NotIn']]),"j",comparators(["ci"]))],
                                                                                                           ["j","v"],
                                                                                                           call_func_args("enumerate",["r"]))])]),
                                                                                                    [ comprehension_ifs_target_iter(
                                                                                                         [ compare_ops_left_comparators(ops([['python:NotIn']]),"i",comparators(["ri"]))],
                                                                                                         ["i","r"],
                                                                                                         call_func_args("enumerate",["grid"]))])]))
                                                                         ])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "hperiod",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("obj")],[argument_type("Integer")]),
                                                 block_statements( [ expr_value(string_value(' horizontal periodicity ')),
                                                                           assign_targets_value(["normalized"],call_func_args("normalize",["obj"])),
                                                                           assign_targets_value(["w"],call_func_args("width",["normalized"])),
                                                                           for_target_iter_body( "p",
                                                                             call_func_args("range",[1,"w"]),
                                                                             body_stmts( [ assign_targets_value( ["offsetted"],
                                                                                                   call_func_args("shift",["normalized",[0,unary_op_operand(us_ub_token(-),"p")]])),
                                                                                                 assign_targets_value( ["pruned"],
                                                                                                   call_func_args( "frozenset", [
                                                                                                     set_comp_elt_generators(
                                                                                                        [ "c",
                                                                                                          ["i","j"]],
                                                                                                        [ comprehension_ifs_target_iter(
                                                                                                             [ compare_ops_left_comparators(ops([gt_e_token(>=)]),"j",comparators([0]))],
                                                                                                             [ "c",
                                                                                                               ["i","j"]],
                                                                                                             "offsetted")])])),
                                                                                                 if_test_body(
                                                                                                    call_func_args(
                                                                                                       qualified_identifier_identifiers(["pruned",boxed_attribute_value("issubset")]),
                                                                                                       ["normalized"]),
                                                                                                    body_stmts([return_value("p")]))])
                                                                         ),
                                                                           return_value("w")])
                                              ))])
            , function_group_nodes( [ [ false : async, comments:[],decorators:[]],
                                            alias_name_node( "vperiod",
                                              function_type_body(
                                                 function_type_arguments_returns([argument_name("obj")],[argument_type("Integer")]),
                                                 block_statements( [ expr_value(string_value(' vertical periodicity ')),
                                                                           assign_targets_value(["normalized"],call_func_args("normalize",["obj"])),
                                                                           assign_targets_value(["h"],call_func_args("height",["normalized"])),
                                                                           for_target_iter_body( "p",
                                                                             call_func_args("range",[1,"h"]),
                                                                             body_stmts( [ assign_targets_value( ["offsetted"],
                                                                                                   call_func_args("shift",["normalized",[unary_op_operand(us_ub_token(-),"p"),0]])),
                                                                                                 assign_targets_value( ["pruned"],
                                                                                                   call_func_args( "frozenset", [
                                                                                                     set_comp_elt_generators(
                                                                                                        [ "c",
                                                                                                          ["i","j"]],
                                                                                                        [ comprehension_ifs_target_iter(
                                                                                                             [ compare_ops_left_comparators(ops([gt_e_token(>=)]),"i",comparators([0]))],
                                                                                                             [ "c",
                                                                                                               ["i","j"]],
                                                                                                             "offsetted")])])),
                                                                                                 if_test_body(
                                                                                                    call_func_args(
                                                                                                       qualified_identifier_identifiers(["pruned",boxed_attribute_value("issubset")]),
                                                                                                       ["normalized"]),
                                                                                                    body_stmts([return_value("p")]))])
                                                                         ),
                                                                           return_value("h")])
                                              ))])
            ])



