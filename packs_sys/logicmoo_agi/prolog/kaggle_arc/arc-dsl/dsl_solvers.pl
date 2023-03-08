
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
s_ast(comparators([V]),V).
s_ast(ops([V]),V).

s_ast(['uast:Identifier',[]],[]).

s_ast(S,O):- basic_wrapper(F),M=..[F,O],S=M, \+ atom(O),!.
%s_ast0(Swap,O):- eq_swap(Swap,Term),ss_ast(Term,O),!.

basic_wrapper(boxed_name_value).
basic_wrapper(boxed_str_value).
basic_wrapper(index_value).
basic_wrapper(num_token).

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

%basic_wrapper(tuple_elts).


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
 notrace((d_simplify_ast(AST,AST11),d_simplify_ast(AST11,AST1), nop((writeq(AST1),nl,nl,print(AST1))),nl,nl)),!,
  must_det_ll((compile_uast_python(AST1,_OUT))).

:- if(false).
see_training_pair(IN,OUT):-
%<backtracks to here>
    iprops = propFromGrid(IN); 
    oprops = propFromGrid(OUT); 
% <or maybe only to here>
     important, unimportant = search_for_the_important_and_unimportant_props(ioprop,oprops);
     matchesOut = make_output_grid(iprops,important, unimportant);
   % if (matchesOut==OUT) return(important, unimportant);
   fail.
pass_test(TEST):-
  iprops = propFromGrid(TEST); 
  make_output_grid(iprops,important, unimportant).
:- endif.

pp_w_dot(PP):-print_tree_with_final(PP,'.\n').
%pp_w_dot(PP):-pp(PP),writeln('.').

compile_uast_python(AST,[]):- \+ compound(AST),!,nop(dmsg(compile_uast_python(AST,_))).
compile_uast_python(AST,OUT):- is_list(AST),!,maplist(compile_uast_python,AST,OUTE),into_codeL(OUTE,OUT),!.
compile_uast_python(alias_name_node(Name,AST),OUT):- !, compile_pyfunction_to_kl_one(Name,AST,OUTE),into_codeL(OUTE,OUT),!,
  format('~N% Compiled KL-1 for ~w~n',[Name]),
  maplist(pp_w_dot,OUT),!.
compile_uast_python(AST,OUT):- compound_name_arguments(AST,_,ARGS),!,maplist(compile_uast_python,ARGS,OUTE),
   into_codeL(OUTE,OUT).

compile_pyfunction_to_kl_one(Name,function_type_body(function_type_arguments_returns(IN,OUT),ASTNode),[Head:-Body]):- 
  flag(py_gensym,_,1),
  in_cmt((writeln("% Universal AST Pass #0"),pp(red,def(Name,function_type_body(function_type_arguments_returns(IN,OUT),ASTNode))))),
  must_det_ll((
   compute_head(Name,HEADL,IN,OUT,ASTNode,NewAST,RET,Code12),
   into_callL(HEADL,Head),
   compile_block_return_arg_until_done(RET,NewAST,Code3),
   code_to_conjuncts([Code12,Code3],Body))).

code_to_conjuncts(BodyE,Body):- into_codeL(BodyE,BodyL), list_to_conjuncts(BodyL,Body),!.
into_codeL(OUTE,OUTO):- flatten([OUTE],OUTL),maplist(conjuncts_to_list,OUTL,OUTL1),flatten(OUTL1,OUTO).

into_callL([A|OUTE],OUTO):- atom(A), \+ (upcase_atom(A,UC),UC=A),!,OUTO=..[A|OUTE].
into_callL([A|OUTE],OUTO):- !,OUTO=..[call,A|OUTE].
into_callL(OUTE,OUTO):- OUTO=..[call,OUTE],!.

%string_to_var(Str,_):- any_to_atom(Str,Atom),downcase_atom(Atom,DAtom),Atom\==DAtom,!,fail.
string_to_var(Str,'$VAR'(UAtom)):- any_to_atom(Str,Atom),upcase_atom(Atom,UAtom),!.

compute_head(Name,HEADL,IN,OUT,ASTNode,NewAST,RET,Code12):-
     must_det_ll((atom_string(AName,Name),
     maplist(compile_parameter(head),HEADARGS,Replace,IN,Code1),
     subst_2L(Replace,HEADARGS,ASTNode+OUT,NewAST+NewOUT),
     maplist(compile_parameter(head,RET,_ReplaceOut),NewOUT,Code2),
     append([AName|HEADARGS],[RET],HEADL),
     into_codeL([Code1,Code2],Code12))).

compute_call(NeedsEval,HEADL,IN,OUT,ASTNode,NewAST,RET,Code123):-  needs_eval(NeedsEval),!,
  py_gensym('cc_',VarName),string_to_var(VarName,VAR),
  compute_call(VAR,HEADL,IN,OUT,ASTNode,NewAST,RET,Code12),
  into_codeL([assign_targets_value([VAR],NeedsEval),Code12],Code123).

compute_call(FuncNameStr,HEADL,IN,OUT,ASTNode,NewAST,RET,Code12):-
   must_det_ll((into_funct(FuncNameStr,AName),
     maplist(compile_parameter(body),HEADARGS,Replace,IN,Code1),
     subst_2L(Replace,HEADARGS,ASTNode+OUT,NewAST+NewOUT),
     maplist(compile_parameter(body,RET,_ReplaceOut),NewOUT,Code2),
     append([AName|HEADARGS],[RET],HEADL),
     into_codeL([Code1,Code2],Code12))).


compile_block_return_arg_until_done(RET,NewAST,Code):- 
  compile_block_return_arg(RET,NewAST,Code3),!,
  (NewAST\=@=Code3->compile_block_return_arg_until_done(RET,Code3,Code)
    ;(in_cmt((writeln("% Body Pass #1"),pp(yellow,NewAST))),do_gather_replace_vars(NewAST,Code))),!.

/*
do_gather_replace_vars(NewAST,Code):-
  sub_term(E,NewAST),compound(E),E=call(Str=WHAT),string(Str), str_var(WHAT) , !,
  subst_2L([E,Str],[true,WHAT],NewAST,CodeM),!,
  do_gather_replace_vars(CodeM,Code).
*/
do_gather_replace_vars(NewAST,Code):-
  sub_term(E,NewAST),compound(E),replace_str_var(E,Str,VAR), \+ (original_var(Str), \+ original_var(VAR)),
  subst001(NewAST,Str,VAR,CodeM),!,
  do_gather_replace_vars(CodeM,Code).
do_gather_replace_vars(Code,Code).


str_var(Str):- is_ftVar(Str),!.
str_var(Str):- string(Str),!.
str_var(Str):- number(Str),!.
str_var(Str):- atom(Str),!.

original_var('$VAR'(V)):-!, original_var(V).
original_var(V):- string(V), !, atom_string(A,V),!,original_var(A).
original_var(V):- number(V),!.
original_var(V):- \+ atom(V),!,fail.
original_var(V):- downcase_atom(V,DC),!, \+ arg_or_cc(DC).
arg_or_cc(DC):- atom_concat('arg_',_,DC).
arg_or_cc(DC):- atom_concat('cc_',_,DC).

replace_str_var(call(Str=VAR),call(Str=VAR),true):- Str=@=VAR,!.
replace_str_var(call(Str=VAR),Str,VAR):- is_ftVar(Str), is_ftVar(VAR).
replace_str_var(call(Str=VAR),Str,VAR):- string(Str), is_ftVar(VAR).
replace_str_var(call_func_args(_,List),Str,VAR):- member(Str,List),should_been_var(Str),string_to_var(Str,VAR).
replace_str_var(tuple_elts(List),Str,VAR):- member(Str,List),should_been_var(Str),string_to_var(Str,VAR).
replace_str_var(E,Str,VAR):- E=into_tuple(_,_,_),arg(_,E,Str),should_been_var(Str), string_to_var(Str,VAR).
replace_str_var(E,Str,VAR):- E=from_tuple(_,_,_),arg(_,E,Str),should_been_var(Str), string_to_var(Str,VAR).

/*
do_gather_replace_vars(NewAST,Code):-
  sub_term(E,NewAST),fail,string(E),!,
  string_t o_var(E,VAR), subst_2L([E],[VAR],NewAST,CodeM),!,
  do_gather_replace_vars(CodeM,Code).*/

should_been_var(Str):- string(Str), lowercase_word(Str).
tok_atom(ADDTOK,Atom):- atom(ADDTOK),!,Atom=ADDTOK.
tok_atom(ADDTOK,Atom):- string(ADDTOK),!,atom_string(Atom,ADDTOK).
tok_atom(ADDTOK,Atom):- \+ compound(ADDTOK),!,Atom=ADDTOK.
%1````````````````````````````````````````tok_atom(ADDTOK,Atom):- functor(ADDTOK,Atom,1),!.
tok_atom(ADDTOK,Atom):- arg(1,ADDTOK,Atom). %,atom_string(Atom,Str).


compile_block_return_arg(_RET,call_func_args("isinstance",[ELEMENT_01,STuple]),[
 call_func_args(isinstance,[ELEMENT_01,Tuple])]):- 
   string(STuple),atom_string(Tuple,STuple),!.

compile_block_return_arg(RET,AST,Code):- is_list(AST),!,
  maplist(compile_block_return_arg(RET),AST,ECode), into_codeL(ECode,Code).

compile_block_return_arg(RET,body_stmts(AST),BODY):-
  compile_block_return_arg(RET,AST,CODE),
  code_to_conjuncts(CODE,BODY).

compile_block_return_arg(RET,block_statements(AST),BODY):-!,
  compile_block_return_arg(RET,AST,CODE),
  code_to_conjuncts(CODE,BODY).


compile_block_return_arg(_RET,expr_value(string_value(Str)),comment(Str)).

compile_block_return_arg(RET,return_value(Stuff),Code):-
  compile_block_return_arg(RET,[assign_targets_value([RET],Stuff),exit_proc(RET)],Code).



compile_block_return_arg(_RET,bin_op_left_right(ADDTOK,L,R),call_func_args(Str,[L,R])):- 
  tok_atom(ADDTOK,Str),!.
compile_block_return_arg(_RET,unary_op_operand(ADDTOK,L),call_func_args(Str,[L])):- 
  tok_atom(ADDTOK,Str),!.

compile_block_return_arg(_RET,compare_ops_left_comparators(ops([]),_L,comparators([])),[]):-!.
compile_block_return_arg(RET,compare_ops_left_comparators(ops([E1|List1]),L,comparators([E2|List2])),Code):- 
  length(List1,Len),length(List2,Len),
  compile_block_return_arg(RET,compare_ops_left_comparators(E1,L,E2),Code1),
  compile_block_return_arg(RET,compare_ops_left_comparators(ops(List1),L,comparators(List2)),Code2),
  into_codeL([Code1,Code2],Code).
compile_block_return_arg(_RET,compare_ops_left_comparators(ADDTOK,L,R),call_func_args(Str,[L,R])):- \+ is_list(ADDTOK),
  tok_atom(ADDTOK,Str),!.
  

compile_block_return_arg(_RET,assign_targets_value([tuple_elts([])],tuple_elts([])),[]):-!.
compile_block_return_arg(RET,assign_targets_value([tuple_elts([E1|List1])],tuple_elts([E2|List2])),Code):-
  length(List1,Len),length(List2,Len),
  compile_block_return_arg(RET,assign_targets_value([E1],E2),Code1),
  compile_block_return_arg(RET,assign_targets_value([tuple_elts(List1)],tuple_elts(List2)),Code2),
  into_codeL([Code1,Code2],Code).

compile_block_return_arg(_RET,assign_targets_value([tuple_elts([Str1,Str2])],StrT),Code):-
  str_var(Str1),str_var(Str2),str_var(StrT),into_callL(from_tuple(StrT,Str1,Str2),Code),!.

compile_block_return_arg(_RET,assign_targets_value([StrT],tuple_elts([Str1,Str2])),Code):-
  str_var(Str1),str_var(Str2),str_var(StrT),into_callL(into_tuple(Str1,Str2,StrT),Code).

compile_block_return_arg(_RET,assign_targets_value([T1],call_func_args(Funct,OBJARGS)),Code):-
  compute_call(Funct,HEADL,OBJARGS,[T1],_Unsupplied,_Unused,_WillReturn,Code12),
   into_callL(HEADL,Head),
   into_codeL([Code12,Head],Code),!.

compile_block_return_arg(_RET,assign_targets_value([Str1],StrT),Code):-
  str_var(Str1),str_var(StrT),!,must_det_ll((into_callL('='(Str1,StrT),Code))).

compile_block_return_arg(RET,call(from_tuple(LOC_02,I,J)),CODE):-
  compile_block_return_arg(RET,from_tuple(LOC_02,I,J),CODE).
compile_block_return_arg(RET,call(to_tuple(LOC_02,I,J)),CODE):-
  compile_block_return_arg(RET,to_tuple(LOC_02,I,J),CODE).
  
compile_block_return_arg(_LASTHEADARG,NewAST,NewAST):- \+ compound(NewAST),!.
  
compile_block_return_arg(RET,AST,Code):- 
  compound_name_arguments(AST,F,ASTARGS),
  maplist(compile_block_return_arg(RET),ASTARGS,ECode),!,
  compound_name_arguments(Code,F,ECode).
  
needs_eval(Str):- \+ str_var(Str).

into_funct(Str,Atom):- string(Str),must_det_ll(atom_string(Atom,Str)),!.
into_funct(Var,Atom):- str_var(Var),!,must_det_ll( Atom=Var),!.


compile_parameter(Head,Args,Replaces,List,Code):- is_list(List),!,maplist(compile_parameter(Head),List,Args,Replaces,CodeE),into_codeL(CodeE,Code),!.
compile_parameter(_Head,VAR,Replaces,SName,[true]):- string(SName), string_to_var(SName,VAR),
  Replaces = SName,!.
compile_parameter(_Head,VAR,_Replaces,VAR,[true]):- str_var(VAR), \+ string(VAR),!.
compile_parameter(_Head,'$VAR'(Name),SName,argument_name(SName),[true]):- atom_string(DName,SName),
   py_gensym(DName,GenSym),
   upcase_atom(GenSym,Name).
compile_parameter(_Head,'$VAR'(Name),OSName,argument_type(SName),[willBeType('$VAR'(Name),DName)]):-
   (var(OSName)->SName= OSName;true),
   atom_string(DName,SName), atom_string(ODName,OSName), 
   py_gensym(ODName,GenSym), upcase_atom(GenSym,Name).

compile_parameter(_Head,'$VAR'(Name),SName,Invoke,[assign_targets_value(['$VAR'(Name)],Invoke)]):- 
  py_gensym('ARG_',Name),atom_string(Name,SName),!.

py_gensym(Atom,Sym):- atom_concat(_,'_',Atom), flag(py_gensym,X,X+1), atomic_list_concat([Atom,X],'0',Sym).
py_gensym(Atom,Sym):- flag(py_gensym,X,X+1), atomic_list_concat([Atom,X],'_0',Sym).

  /*

bool01_t(1,true).
bool01_t(0,false).

zeven_t(Z,Truth) :- Z mod 2 #= 0 #<==> B, bool01_t(B,Truth).

%zodd_t(Z,Truth) :- Z mod 2 #= 1 #<==> B, bool01_t(B,Truth).
zodd_t(Z,Truth)  :- Z mod 2 #=         B, bool01_t(B,Truth). % tweaked
?- tpartition(zeven_t,[1,2,3,4,5,6,7],Es,Os).
Es = [2,4,6], Os = [1,3,5,7].
?- tpartition(zodd_t ,[1,2,3,4,5,6,7],Os,Es). % argument order differs
Es = [2,4,6], Os = [1,3,5,7].


  vsplit(Grid,N,Tuple):-
    len(Grid,GridLen),
    H #= GridLen // N,
    subscript_value_slice(Grid,0,GridSlice0),
    len(GridSlice0,GridSlice0Len),
    W #= GridSlice0Len,
    Step1 #= GridLen % N,
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



