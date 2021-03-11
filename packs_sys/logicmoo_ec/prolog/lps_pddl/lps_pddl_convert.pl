% =========================================
% Goal/Plan translating
% =========================================
:- module(lps_pddl_convert,[%load_e/1, needs_proccess/3,process_ec/2,fix_time_args/3,fix_goal/3, 
  %brk_on_bind/1,assert_axiom_2/2,
   assert_1pddl_pddl/1,
   test_logicmoo_lps_pddl_reader/0,
   
   test_lps_pddl_ereader/0,
   test_logicmoo_pddl_reader_2/0,
   test_logicmoo_lps_pddl_reader/2,test_logicmoo_lps_pddl_reader/1]).
                     

:- use_module(library(logicmoo_common)).

/*export_transparent(P):-
  export(P),
  module_transparent(P).
*/
:- use_module(library(logicmoo_lps)).
:- use_module(library(ec_planner/ec_lps_convert)).
:- use_module(library(wam_cl/sreader)).
:- use_module(library(hyhtn_pddl/rsasak_pddl_parser)).
% system:pddl_current_domain(X):- wdmsg(pddl_current_domain(X)),fail.
%:- user:use_module(library('pddl_planner/pddl_planner_dmiles')).
%:- use_module(library(pddl_planner/pddl_reader)).

:- use_module(library(lps_corner)).

:- set_prolog_flag(lps_translation_only_HIDE,false).
:- set_prolog_flag(lps_translation_only,false).


assert_1pddl_pddl(Stuff):- 
  downcase_terms(Stuff,DCStuff1),
  with_kif_ok(to_untyped(DCStuff1,DCStuff)),
  print_tree_cmt('Translating',green,DCStuff),
  assert_pddl([],DCStuff).
   

include_e_lps_pddl_file_now(Type,MFile):- strip_module(MFile,M,File), include_e_lps_pddl_file_now(Type,M,File).
include_e_lps_pddl_file_now(Type,M,File):- absolute_file_name(File,AbsFile),File\==AbsFile,exists_file(AbsFile), !,include_e_lps_pddl_file_now(Type,M,AbsFile).

%include_e_lps_pddl_file_now(_Type,_Ctx,File):- 
%   with_lisp_translation(File,pprint_ecp(yellow)),!.
include_e_lps_pddl_file_now(_Type,_Ctx,File):- 
   with_lisp_translation(File,assert_1pddl_pddl),!.


load_e_lps_pddl_file(Type,File):- update_changed_files,  
  retractall(etmp:pddl_option(load(_), _)), include_e_lps_pddl_file(Type,File).
%load_e_lps_pddl_file(Type,File):- retractall(etmp:pddl_option(load(_), _)), include_e_lps_pddl_file(Type,File).


include_e_lps_pddl_file(Type,File):- is_list(File), !, maplist(include_e_lps_pddl_file(Type),File).
include_e_lps_pddl_file(Type,File):- wdmsg(include_e_lps_pddl_file(Type,File)),fail.
include_e_lps_pddl_file(Type,File):- needs_resolve_local_files(File,Resolved),!,include_e_lps_pddl_file(Type,Resolved).
include_e_lps_pddl_file(Type,File):- absolute_file_name(File,DB), exists_file(DB),!, 
  update_changed_files, was_s_l(File,1),  
  strip_module(_,M,_), prolog_statistics:time(M:include_e_lps_pddl_file_now(Type,File)),!.
include_e_lps_pddl_file(Type,File):- throw(with_abs_paths(include_e_lps_pddl_file(Type),File)).


test_logicmoo_lps_pddl_reader(File):- test_logicmoo_lps_pddl_reader(lps, File).
test_logicmoo_lps_pddl_reader(Proc1,File):- load_e_lps_pddl_file(Proc1,File).

solve_files_w_lps(DomainFile, ProblemFile):- 
  test_logicmoo_lps_pddl_reader(ProblemFile),!,
%  parseProblem(ProblemFile,PStuff),%break,
  %pprint_ecp(blue,PStuff),!, break,
  %parseDomain(DomainFile,Stuff), pprint_ecp(yellow,Stuff),!, % break,
  test_logicmoo_lps_pddl_reader(DomainFile),
  !.

test_logicmoo_lps_pddl_reader:-  
 test_logicmoo_lps_pddl_reader(pddl('orig_pddl_parser/test/blocks/domain-blocks.pddl')),
 test_logicmoo_lps_pddl_reader(pddl('../uw-yale-pddl/domains/transplan/domain.pddl')).

:- add_history((cls, test_logicmoo_lps_pddl_reader)).

test_logicmoo_lps_pddl_reader0:- 
  test_logicmoo_lps_pddl_reader(pddl('transplan/domain.pddl')),
  test_logicmoo_lps_pddl_reader(pddl('elearning/domain.pddl')),
  !.

test_logicmoo_lps_pddl_reader1:- 
   test_logicmoo_lps_pddl_reader(pddl('../uw-yale-pddl/strict-domains/mystery/*.pddl')),
   test_logicmoo_lps_pddl_reader(pddl('*/*.pddl')).

test_logicmoo_pddl_reader_2:- 
 test_logicmoo_lps_pddl_reader(pddl('benchmarks/*/*/*/*.pddl')).

:- ensure_loaded(library(logicmoo/util_structs)).
:- ensure_loaded(library(statistics)).
%:- ensure_loaded(library(logicmoo_util_bb_env)).

test_lps_pddl_ereader:- !,
   planner_solve_files(pddl('orig_pddl_parser/test/blocks/domain-blocks.pddl'), 
      pddl('orig_pddl_parser/test/blocks/blocks-03-0.pddl')),!.
    


compound_name_arguments_maybe_zero(F,F,[]):- \+ compound(F), !.
compound_name_arguments_maybe_zero(LpsM,F,ArgsO):- (compound(LpsM);atom(F)),!,compound_name_arguments(LpsM,F,ArgsO),!.
%compound_name_arguments_maybe_zero(LpsM,F,ArgsO):- dumpST,break.


already_lps_pddl(Form):- var(Form),!,throw(var_already_lps_pddl(Form)).
already_lps_pddl(:- _):-!.
already_lps_pddl(option(_,_)):-!.
already_lps_pddl(false(_)):-!.
already_lps_pddl(mpred_prop(_,_)):-!.
already_lps_pddl(sort(_)):-!.
already_lps_pddl(subsort(_,_)):-!.

into_pterm( Why,I,O):- must_or_rtrace_l((into_pterm_0( Why,I,M),our_sterm2pterm(M,O))).

atomic_or_var(Form):- ( \+ compound_gt(Form,0) ; Form='$VAR'(_); Form='$STRING'(_)),!.

get_svars(P,Vars):- findall(VAR, (sub_term(VAR,P),compound(VAR),VAR='$VAR'(_)), List), list_to_set(List,Vars).

into_pterm_0(_Why,Decl,Res):- atomic_or_var(Decl),!, Res = Decl. % Was Res = t(Decl)

into_pterm_0( Why,all(Decl,H),all(Decl,Res)):- into_pterm_0( Why,H,Res).
into_pterm_0( Why,exists(Decl,H),exists(Decl,Res)):- into_pterm_0( Why,H,Res).

into_pterm_0( Why,Compound,Res):- \+ is_list(Compound),!,compound_name_arguments_maybe_zero(Compound,F,Args),!,into_pterm_0( Why,[F|Args],Res).
into_pterm_0(_Why,[VAR|LIST],[VAR|LIST]):- (var(VAR);svar(VAR,_)),!.

into_pterm_0( Why,[(',')|X],Res):- into_pterm_0( Why,[and|X],Res).
into_pterm_0( Why,[and,X],Res):- !, into_pterm_0( Why,X,Res).
into_pterm_0( Why,[and,X|L],and(Res,LRes)):- into_pterm_0( Why,X,Res), into_pterm_0( Why,L,[and|LRes]).
into_pterm_0( Why,[or,X],Res):- !, into_pterm_0( Why,X,Res).
into_pterm_0( Why,[or,X|L],or(Res,LRes)):- into_pterm_0( Why,X,Res), into_pterm_0( Why,L,[or|LRes]).
into_pterm_0( Why,[not,X],not(Res)):- !, into_pterm_0( Why,X,Res).
into_pterm_0( Why,I,O):- our_sterm2pterm(Why, I,O),!.
% into_pterm_0( Why,[Compound],Res):- !, into_pterm_0( Why,Compound,Res).
%into_pterm_0( Why,[A|List],Res):- atom(A),is_list(List),!, Res =.. [A|List].
%into_pterm_0( Why,[A|List],Res):- compound(A),is_list(List),!,append_termlist(A,List,Res),!.
%into_pterm_0( Why,List,Res):- Res =..[t|List].
% into_pterm_0( Why,Decl,t(Decl)).

our_sterm2pterm(I,O):- our_sterm2pterm(domain,I,O).

our_sterm2pterm(_Ctx,VAR,VAR):-var(VAR),!.
our_sterm2pterm( Ctx,In,Out):-nonvar(Out),!,our_sterm2pterm( Ctx,In,OutM),must(Out=OutM).
our_sterm2pterm(_Ctx,'?'(Down),'?'(UP)):- svar_fixvarname(Down,UP),!.
our_sterm2pterm(_Ctx,KVList,T):- append(_,[_Item,'-',_Type],KVList),into_typed_params(KVList,T).
%our_sterm2pterm( Ctx,QDown,'?'(UP)):- \+ is_list(QDown),svar_fixvarname(QDown,UP),!.
our_sterm2pterm(domain,[S],S):-atom(S),!. % ,atom_concat(':',_,S),!.
our_sterm2pterm(_Ctx,[S],S):-atom(S),!. % ,atom_concat(':',_,S),!.
our_sterm2pterm(_Ctx,[Item,'-',Type],Item1):- atom(Type),Item1=typed(Type,Item).

our_sterm2pterm( Ctx,[S,Vars,SLIST],POUT):-atom(S),is_quantifier_type(S,SQ),into_typed_params(Vars,PVars),our_sterm2pterm( Ctx,SLIST,TERM),POUT=..[SQ,PVars,TERM].
our_sterm2pterm( Ctx,[S,Vars|SLIST],POUT):-atom(S),is_quantifier_type(S,SQ),into_typed_params(Vars,PVars),our_sterm2pterm_list( Ctx,SLIST,TERM),POUT=..[SQ,PVars,TERM].
our_sterm2pterm( Ctx,[S|SLIST],PTERM):-atom(S),atom_concat(':',_,S),
            our_sterm2pterm_list(Ctx,SLIST,PLIST),           
            PTERM=..[S,PLIST].
our_sterm2pterm( Ctx,[S|SLIST],PTERM):-atom(S), \+ svar(S,_),!,            
            our_sterm2pterm_list(Ctx,SLIST,PLIST),           
            PTERM=..[S|PLIST].
our_sterm2pterm( Ctx,SLIST,PLIST):- is_list(SLIST),!,our_sterm2pterm_list(Ctx,SLIST,PLIST).
our_sterm2pterm(_Ctx,VAR,VAR):-!.

our_sterm2pterm_list(_Ctx,[],[]).
our_sterm2pterm_list( Ctx,[Item,'-',Type|List],[H|T]):- atom(Type),H==typed(Type,Item),our_sterm2pterm_list(Ctx,List,T).
our_sterm2pterm_list( Ctx,[Item|List],[H|T]):- our_sterm2pterm( Ctx,Item,H),our_sterm2pterm_list(Ctx,List,T).

into_plus_minus(Conds,Pos,Neg):- 
   into_pterm( domain,Conds,PostC),
   into_enables(PostC,Pos),
   into_disables(PostC,Neg).
  


into_enables([and|Args],Enables):- !, maplist(into_enables,Args,EnablesL),append(EnablesL,Enables).
into_enables(Effect,Enables):- compound(Effect), compound_name_arguments_maybe_zero(Effect,and,Args),maplist(into_enables,Args,EnablesL),append(EnablesL,Enables).
into_enables(not(_),[]).
into_enables(E,[E]).

into_disables(not(E),Es):- into_enables(E,Es).
into_disables([not,E],Es):- into_enables(E,Es).
into_disables([and|Args],Enables):- !, maplist(into_disables,Args,EnablesL),append(EnablesL,Enables).
into_disables(Effect,Enables):- compound(Effect), compound_name_arguments_maybe_zero(Effect,and,Args),maplist(into_disables,Args,EnablesL),append(EnablesL,Enables).
into_disables(_,[]).




% assert_1pddl([constant|Ctx],C):- compound(C),

and_to_comma(Rule, true):- (Rule == [] ; Rule == and ; Rule == [and] ), !.
and_to_comma(Rule0,Rule):- atomic_or_var(Rule0),!, Rule0=Rule.
and_to_comma(Rule0,Rule):- into_pterm(and_to_comma,Rule0,Rule1),and_to_comma_0(Rule1,Rule).

and_to_comma_0(Rule0,Rule):- atomic_or_var(Rule0), !, Rule=Rule0.
and_to_comma_0(and(A,B),(AA,BB)):- !, and_to_comma_0(A,AA),and_to_comma_0(B,BB).
and_to_comma_0(and(A),AA):- !,and_to_comma_0(A,AA).
and_to_comma_0(ANDA,(AA,BB)):- compound(ANDA), ANDA=..[and,A|As],!,compound_name_arguments(B,and,As),and_to_comma_0(A,AA),and_to_comma_0(B,BB).
and_to_comma_0(A,AA):-
   % pddl_to_lps(top, A0, A),
   compound_name_arguments(A,F,As),
   maplist(and_to_comma_0,As,AAs),
   compound_name_arguments(AA,F,AAs).

is_pddl_amethod(action).
is_pddl_amethod(task).
is_pddl_amethod(method).
is_pddl_amethod('durative-action').

%assert_pddl(Ctx,_,include(F)):- include_e_lps_pddl_file_now(Type,Ctx:F).
%assert_pddl(Ctx,_,load(F)):- include_e_lps_pddl_file_now(Type,Ctx:F). 
%assert_pddl(Ctx,_,include(F)):- !, with_e_file(assert_pddl(Ctx),current_output, [pddl(F)]). 
%assert_pddl(Ctx,_,load(X)):- nop(assert_pddl(Ctx,include(X))),!.

%assert_pddl(Ctx,Form):- 
assert_pddl(Ctx,Form):- \+ compound_gt(Form,0),!,assert_1pddl(Ctx,Form).
assert_pddl(Ctx,t(Type,Inst)):- atom(Type), M=..[Type,Inst],!,assert_pddl(Ctx,M),!.
%assert_pddl(Ctx,Form):- already_lps_pddl(Form),!,assert_1pddl(Ctx,Form).
assert_pddl(Ctx,Form):- \+ is_list(Form),!,assert_1pddl(Ctx,Form).

assert_pddl(Ctx,Form):- 
  Form = [ define, Decl|Rest],
  into_pterm( define,Decl,Named),
  assert_pddl([Named|Ctx],Rest),!.

assert_pddl(Ctx,Form):- is_pddl_amethod(Method),
  Form = [ Method, Decl|Rest],
  into_pterm( named_method,Decl,Named),
  assert_pddl([Method|Ctx],[Named|Rest]),!.

assert_pddl(Ctx,[[KW,Data]|Rest]):-
  kw_directive(KW,NewType),
  kw_soon(Rest),
  assert_pddl([NewType|Ctx],Data),
  assert_pddl(Ctx,Rest),!.

assert_pddl(Ctx,[[KW|Data]|Rest]):- Data\==[],
  kw_directive(KW,NewType),
  kw_soon(Rest),
  assert_pddl([NewType|Ctx],Data),
  assert_pddl(Ctx,Rest),!.
/*
assert_pddl(Ctx,[KW,Data|Rest]):- Data\==[],
  kw_directive(KW,NewType),
  kw_soon(Rest),
  assert_pddl([NewType|Ctx],Data),
  assert_pddl(Ctx,Rest),!.
*/

assert_pddl([init|Ctx],Data):-  map_pddl_list(assert_pddl([s(initially)|Ctx]),Data).
assert_pddl(Ctx,Data):- \+ is_list(Data),!,assert_pddl(Ctx,[Data]).
assert_pddl([AtomS|Ctx],Data):- atom(AtomS),atom_concat(Atom,'s',AtomS),!, map_pddl_list(assert_pddl([Atom|Ctx]),Data).
assert_pddl([s(Pred)|Ctx],SData):- our_sterm2pterm(SData,Data), !,assert_1pddl([Pred|Ctx],Data).
assert_pddl([One,Ctx],SData):- atom(One),!, sterm22pterm(SData,Data),!,assert_1pddl([One,Ctx],Data).
assert_pddl(Ctx,[ KW, Decl|Rest]):-  un_kw_directive(KW,Atom), assert_pddl(Ctx,[ Atom, Decl|Rest]), !.
assert_pddl(Ctx,Form):- wdmsg(assert_pddl(Ctx,Form)),fail.
assert_pddl(Ctx,Form):- assert_1pddl(Ctx,Form), !.


assert_pddl_pairs(_,[]).
assert_pddl_pairs(Ctx,[[N,V]|Form]):- assert_1pddl_pair([N|Ctx],V),assert_pddl_pairs(Ctx,Form).
assert_pddl_pairs(Ctx,[[N|V]|Form]):- assert_1pddl_pair([N|Ctx],V),assert_pddl_pairs(Ctx,Form).
assert_pddl_pairs(Ctx,[N,V|Form]):- assert_1pddl_pair([N|Ctx],V),assert_pddl_pairs(Ctx,Form).

% assert_1pddl_pair(NameCtx,Value):- must_or_rtrace(assert_1pddl(NameCtx,Value)),!.
assert_1pddl_pair(NameCtx,Value):- assert_1pddl(NameCtx,Value),!.

downcase_terms(Data,DData):- atom(Data), \+ atom_contains(Data,"/"), downcase_atom(Data,DData),!.
downcase_terms(Data,DData):- atomic_or_var(Data), !, DData=Data.
downcase_terms(Data,DData):- compound_name_arguments(Data,F,ARGS), !, maplist(downcase_terms,[F|ARGS],[DF|DARGS]),!,
  compound_name_arguments(DData,DF,DARGS).

sterm22pterm(SData,Data):- SData=Data,!.
sterm22pterm(SData,Data):- our_sterm2pterm(SData,SSData),our_sterm2pterm(SSData,Data).

kw_soon(Rest):- 
  (Rest ==[] ; 
  (Rest = [KW2|_],kw_directive(KW2,_)); 
  (Rest = [[KW2|_]|_],kw_directive(KW2,_))).

kw_directive(KW,NewType):- atom(KW), atom_concat(':',Stuff,KW), downcase_atom(Stuff,NewType),!.

un_kw_directive(KW,NewType):- kw_directive(KW,NewType),!.
un_kw_directive(KW,NewType):- atom(KW), downcase_atom(KW,NewType),!, KW\==NewType.


pddl_type_of(X,Y):- into_typed(X,typed(Y,_)),!.
pddl_type_of(F,F).

pddl_value_of(X,Y):- into_typed(X,typed(_,Y)).
pddl_value_of(F,F).

as_isa_list([],[]).
as_isa_list([G|L],[isa(V,T)|LL]):- into_typed(G,typed(V,T)), as_isa_list(L,LL).
as_isa_list([_|L],LL):- as_isa_list(L,LL).


maybe_unlistify([C],C):- nonvar(C),!.
maybe_unlistify([C|AND],CAND):-and_to_comma([and,C|AND],CAND),!.
maybe_unlistify(C,C).

same_keys(X,Y):- must((unkw_s(X,X1),unkw_s(Y,Y1))), !,  Y1==X1.

into_kwu(N,KW):- freeze(KW,freeze(N,same_keys(KW,N))).
%select_within(N,V,Form,NForm):- nonvar(N), into_kwu(N,KW),!, select_within(KW,V,Form,NForm).
select_within(N,V,Form,NForm):- select([NN,V],Form,NForm),atom(NN),same_keys(NN,N),!.
select_within(N,V,Form,NForm):- select([NN|V],Form,NForm),atom(NN),same_keys(NN,N),!.
select_within(N,V,Form,NForm):- append(Left,[NN,V|Right],Form),atom(NN),same_keys(NN,N),append(Left,Right,NForm),!.
select_within(N,V,Form,NForm):- append(NForm,[NN|V],Form),atom(NN),same_keys(NN,N),!.

pddl_param_type(Why,_):-var(Why),!,fail.
pddl_param_type(':vars',typed).
pddl_param_type(':parameters',typed).
pddl_param_type(':domain-variables',typed).

pddl_param_type(Why,WhyO):-nonvar(Why),Why=WhyO.

maybe_convert(N,V0,V,Else):- member(Why,[N,Else,V0]),pddl_param_type(Why,typed),!,into_typed_params(V0,V),!.
%maybe_convert(N,V0,V,Else):- member(Why,[N,Else,V0]),pddl_param_type(Why,['and']),!,into_pterm(maybe_convert,V0,V),!.
maybe_convert(_,V,V,_):-!.

unkw_s(N,KW):- atom(N),atom_concat(':',M,N), !, unkw_s(M,KW).
unkw_s(N,KW):- downcase_atom(N,KW).

get_1pair_value( N,V,Form,NForm,Else):- select_within(N,V0,Form,NForm),maybe_convert(N,V0,V,Else),!.
get_1pair_value( _,V,Form, Form,Else):- V = Else,!.

get_pair_values([],Form,Form):-!.
get_pair_values(ndv(N,E,V),Form,FormOut):- !, get_1pair_value(N,V,Form, FormOut, E),!.
get_pair_values(prop(N,E,V),Form,FormOut):- !, get_1pair_value(N,V,Form, FormMID, E),!, (V==E -> FormOut=FormMID ; FormOut=[N,V|FormMID]).
get_pair_values([Op|Rest],Form,FormOut):-
  get_pair_values(Op,Form,FormM),!,
  get_pair_values(Rest,FormM,FormOut).

is_pddl_prop_holder(Term):- \+ atom(Term), !, fail.
is_pddl_prop_holder(length).

assert_1pddl(Lps):- assert_1pddl(lps_test_mod,Lps).

never:- set_prolog_flag(debugger_write_options,
  [quoted(true), max_depth(100), spacing(next_argument)]).
% assert_1pddl([_Ctx],Form):- Form ==[], 

assert_1pddl(Ctx,Form):- pprint_ecp_cmt(white,assert_1pddl(Ctx,Form)),fail.

assert_1pddl([Atom|Ctx], [V,Dash,Type|More]):- Dash=='-', !, 
  assert_1pddl([Atom|Ctx], typed(Type,V)),
  assert_1pddl([Atom|Ctx], More).

assert_1pddl([Length|_], []):-  is_pddl_prop_holder(Length),!.
assert_1pddl([Length|Ctx], [Domain|Props]):-  is_pddl_prop_holder(Length),!,
  assert_1pddl([Length|Ctx], Domain),
  assert_1pddl([Length|Ctx], Props).
	
assert_1pddl([AM|Ctx],[Name|Form]):- is_pddl_amethod(AM),
 must_or_rtrace_l((get_pair_values([
     ndv(':vars',[],Vars),
     ndv(':parameters',[],Params),
     prop(':expansion',[],Expansion),
     prop(':name',[],NameF),
     prop(':only-in-expansions',[],OiEs),
     prop(':duration',1,Dur),
     prop(':tasks',['and'],Tasks),     
     ndv(':precondition',['and'],Pre),
     ndv(':effect',['and'],Effect)]
                                   ,Form,ExtraInfo),
   our_sterm2pterm(Form,PForm),   
   get_svars(PForm,SVars),   
   maplist(pddl_value_of,Params,VParams),
   as_isa_list(Params,PreConds0),
   as_isa_list(Vars,PreConds1),
   append([[and],PreConds0,PreConds1],Types0),
   and_to_comma(Types0,Types),
   % must(into_plus_minus(Pre,PrePos,PreNeg)),
   %into_plus_minus(Effect,Enables,Disables),  
   compound_name_arguments_maybe_zero(Action,Name,VParams),
   compound_name_arguments_maybe_zero(ActionKey,Name,SVars),
   assert_lps_pl(action_type(Ctx,AM,Action,ActionKey)),
   debug_var('T1', T1),
   debug_var('T2', T2),
   Rule0 = if(initiates(Action, at(Effect, T2)), at((Pre, Types, T2>T1),T1)),
   and_to_comma(Rule0,Rule),
   assert_lps_pl(Rule),
   assert_pddl_pairs([ActionKey|Ctx],ExtraInfo))).

assert_1pddl([predicate|Ctx],[Name|Params]):- 
  must(atom(Name)),
  must_or_rtrace((
    into_typed_params(Params,RParams),
    maplist(pddl_type_of,RParams,TParams),!,
    compound_name_arguments_maybe_zero(Lps,Name,TParams),
  assert_1pddl([':predicate'|Ctx],Lps))).

assert_1pddl([domain(_)],[]):- !.

assert_1pddl([axiom|Ctx],Form):- 
 must_or_rtrace_l((
  get_1pair_value(':vars',Value1a,Form,Form0,[]),
  get_1pair_value(':context',Value2,Form0,Form1,['and']),
  get_1pair_value(':implies',Value3,Form1,LeftOver,'$error'))),  
 must_or_rtrace_l((
  into_typed_params(Value1a,Value1b),as_isa_list(Value1b,Value1),
  listify(Value2,Value2L),
  append(Value2L,Value1,Value12),
  and_to_comma(Value12,Precond),
  and_to_comma(Value3,PostCond),
  maplist(maybe_unlistify,[Ctx,Precond,PostCond],[UCtx,UPrecond,UPostCond]),
  assert_lps_pl(implication(UCtx,UPrecond,UPostCond)),
  assert_pddl_pairs([implication|Ctx],LeftOver))).

 
/*
assert_1pddl([KW,action(N,RParams)|Ctx],PreConds):- kw_directive(KW,Directive),
   maplist(pddl_value_of,RParams,VParams),
   assert_1pddl([action_types(N,TParams)|Ctx],[]),
   maplist(pddl_type_of,RParams,TParams),
   into_pterm( Why,PreConds,Conds),
   assert_1pddl([Directive,action(N,VParams)|Ctx],Conds),!.
*/

assert_1pddl(Ctx,Form):- Ctx=[Pred|Rest],atom(Pred),is_list(Rest),NewForm=..Ctx, append_term_pddl(NewForm,Form,Data),!,assert_lps_pl(Data).
assert_1pddl(Ctx,Form):- Ctx=[NewForm|Rest],is_list(Rest),append_termlist(NewForm,Rest,RData),append_term_pddl(RData,Form,Data),!,assert_lps_pl(Data).
assert_1pddl(_Ctx,Form):- assert_lps_pl(Form),!.

into_typed_params([], []):-!.
into_typed_params([Item, Dash, Type|List], [H|T]) :- Dash=='-',! , H=..[typed,Type,Item], into_typed_params(List, T).
into_typed_params([H|List], [HH|T]):- into_typed(H,HH),into_typed_params(List, T).

into_typed(G,H):- into_typed_name(G,M),maybe_into_named_var(M,H).

maybe_into_named_var(M,V):- with_kif_ok(svar(M,Name)),!,V='$VAR'(Name),!.
maybe_into_named_var(M,V):- M=V.

into_typed_name(H,          typed(any,H)) :- atomic_or_var(H),!.
into_typed_name(  isa(I,T), typed(T,I)).
into_typed_name(typed(T,I), typed(T,I)) .
into_typed_name(G,          typed(T,I)):- compound(G),compound_name_arguments(G,T,[I]),!.
into_typed_name(H,          typed(any,H1)):- our_sterm2pterm(H,H1). 

map_pddl_list(_Pred,[]).
map_pddl_list(Pred1,[Item,'-',Type|List]):- Item1=..[typed,Type,Item], !, 
  map_pddl_list(Pred1,[Item1|List]).
map_pddl_list(Pred1,[[Item,'-',Type]|List]):- Item1=..[typed,Type,Item], !, 
  map_pddl_list(Pred1,[Item1|List]).
map_pddl_list(Pred1,[Item1|List]):- call(Pred1,Item1),map_pddl_list(Pred1,List).

must_or_rtrace_l((A,B)):- !, must_or_rtrace_l(A), must_or_rtrace_l(B).
must_or_rtrace_l((C->A;B)):- !, (C-> must_or_rtrace_l(A);must_or_rtrace_l(B)).
must_or_rtrace_l(A):- must_or_rtrace(A).

append_term_pddl(X,Y,Z):- compound_gt(X,0),X=..[KW|ARGS],kw_directive(KW,NewType),X2=..[NewType|ARGS],!,append_term_pddl(X2,Y,Z).
append_term_pddl(X,Y,Z):- 
  append_term(X,Y,Z).

assert_lps_pl(Lps0):- 
  with_kif_ok(to_untyped(Lps0,Lps)),
  pprint_ecp_cmt(cyan,assert_lps_pl(Lps)),
  lps_xform(Lps,Prolog),!,
  ((Lps\==Prolog
   ->
  ( must_or_rtrace_l((print_lps_syntax(yellow,Lps),
    nop(pprint_ecp(yellow,Lps)),
    pprint_ecp_cmt(cyan,Prolog),
    pprint_ecp_cmt(white,"% ================================="))))
   ;
  assert_1pddl_pddl_try_harder(Lps))),!.

lps_xform(Lps,Prolog):- 
 Ctx = db,
 locally(current_prolog_flag(lps_translation_only_HIDE,true),
   locally(t_l:is_lps_program_module(Ctx),
    must_or_rtrace(lps_term_expander:lps_f_term_expansion_now(Ctx,Lps,Prolog)))),!.

:- use_module(library(lps_syntax)).



% [waiter,agent,food,time]
% HoldsAt(BeWaiter1(waiter),time) ->
% Initiates(Order(agent,waiter,food),
%           BeWaiter2(waiter),
%           time).


pddl_to_lps(_Top, X, X):- atomic_or_var(X),!.
pddl_to_lps(_Top, X, X):- functor(X,_,1), arg(1,X,Var), is_ftVar(Var),!.
pddl_to_lps(_Top,at(X,Y),loc_at(X,Y)).
pddl_to_lps(Top,Prop,O):- 
  Prop =.. [ThereExists,Vars,Term0], 
  is_quantifier_type(ThereExists,Exists),
  is_list(Vars), forall(member(E,Vars),ground(E)),
  QProp =.. [Exists,Vars,Term0],
  insert_vars(QProp, Vars, Term, _Has),
  pddl_to_lps(Top, Term,O),!.

pddl_to_lps(_Top,metreqs(X),X).
pddl_to_lps(_Top,'->'(at(F1,T1),initiates(E,F2,T2)),Becomes):- T1==T2,  
   Becomes = (F1->initiates(E,F2)).
pddl_to_lps(_Top,'->'(at(F1,T1),terminates(E,F2,T2)),Becomes):- T1==T2,  
  Becomes = (F1->terminates(E,F2)).
pddl_to_lps(_Top,'->'(holds_at(F1,T1),initiates(E,F2,T2)),Becomes):- T1==T2,  
   Becomes = (F1->initiates(E,F2)).
pddl_to_lps(_Top,'->'(holds_at(F1,T1),terminates(E,F2,T2)),Becomes):- T1==T2,  
  Becomes = (F1->terminates(E,F2)).

pddl_to_lps(Top,neg(X),Rest):- pddl_to_lps(Top,not(X),Rest).
pddl_to_lps(_Top,holds_at(Fluent, Time),initially(Fluent)):- Time==start, !.
pddl_to_lps(_Top,holds_at(Fluent, Time),initially(Fluent)):- Time==0, !.
%pddl_to_lps(_Top,holds_at(Fluent, Time),at(Fluent, Time)):- !.
pddl_to_lps([],happens_at(Event,Time),(observe Event at Time)):- !.
pddl_to_lps(_Top,happens(Event,Time),(Event at Time)):- !.
% observe(from(muestra_del_general('+86-555000001'),to(2,3)))
pddl_to_lps(  [],initiates_at(Event,Fluent,Time),initiates(Event,Fluent)):- is_ftVar(Time), !.
pddl_to_lps(  [],terminates_at(Event,Fluent,Time),terminates(Event,Fluent)):- is_ftVar(Time), !.

pddl_to_lps(_Top,initiates_at(Event,Fluent,Time),(Event initiates Fluent at Time)):- !.
pddl_to_lps(_Top,terminates_at(Event,Fluent,Time),(Event terminates Fluent at Time)):- !.

pddl_to_lps(_Top, not(exists(_,X)), not(X)):-!.
%pddl_to_lps(_Top, not(initially(X)),(initially not X)):-!.
%pddl_to_lps(_Top, not(holds_at(X,T)),holds_at(not(X),T)).

pddl_to_lps(_Top, holds_at(Fluent, From, To),holds(Fluent, From, To)):- !.



%pddl_to_lps(_Top,happensAt(Event,Time),at(observe(Event),Time)):- !.
pddl_to_lps(_Top,Form,LpsO):- Form=..[EFP,X], argtype_pred(EFP,_), protify(EFP,X,Lps),!,flatten([Lps],LpsO).
pddl_to_lps(_Top,X=Y,Lps):- callable(X),append_term_pddl(X,Y,Lps).
pddl_to_lps(Top,','(X1,X2),(Lps1,Lps2)):- pddl_to_lps(Top,X1,Lps1),pddl_to_lps(Top,X2,Lps2).
pddl_to_lps(Top,'<->'(X1,X2),[Lps1,Lps2]):- simply_atomic_or_conj(X1),simply_atomic_or_conj(X2), pddl_to_lps(Top,'->'(X1,X2),Lps1),pddl_to_lps(Top,'->'(X2,X1),Lps2).
pddl_to_lps(_Top,'->'(X1,X2),(X2 if X1)):- simply_atomic_or_conj(X1),simply_atomic_or_conj(X2),!.
pddl_to_lps(_Top,X1,X1):- simply_atomic(X1),!.
pddl_to_lps(_Top,X1,false(Lps)):- \+ (X1 = false(_)), into_false_conj(X1,Lps),Lps\=not(_),!.
pddl_to_lps(_Top,X,X):-!.

into_false_conj(X1,Lps):- \+ (X1 = false(_)), into_pnf_conj(X1,Lps) -> Lps\=not(_),simply_atomic_or_conj(Lps).
into_pnf_conj(X1,Lps):- pnf(X1,X2),nnf(X2,X3),conjuncts_to_list(X3,X3L),list_to_conjuncts(X3L,X4), Lps = X4.

removes_at(F,_):- sent_op_f(F),!,fail.
removes_at(F,F1):- atom_concat(F1,'_at',F),!.
%removes_at(F,F1):- F=F1.
remove_time_arg(_Time,Holds,Holds):- \+ compound_gt(Holds,0),!.
remove_time_arg(Time,Holds,HoldsMT):- \+ sub_var(Time,Holds),!,Holds=HoldsMT.
remove_time_arg(Time,not(Holds),not(HoldsMT)):-!, remove_time_arg(Time,Holds,HoldsMT).
remove_time_arg(Time,happens_at(Holds,T1),Holds):- T1==Time.
remove_time_arg(Time,holds_at(Holds,T1),Holds):- T1==Time.
remove_time_arg(Time,at(Holds,T1),Holds):- T1==Time.
remove_time_arg(_Time,Holds,Holds):- \+ compound_gt(Holds,1),!.
remove_time_arg(Time,Holds,HoldsMT):- Holds=..[F|Args],append(Left,[T1],Args),T1==Time,removes_at(F,F1),HoldsMT=..[F1|Left],!.
remove_time_arg(Time,Holds,HoldsMT):- Holds=..[F|Args],maplist(remove_time_arg(Time),Args,Left),HoldsMT=..[F|Left],!.

simply_atomic_or_conj(X1):- var(X1),!,fail.
simply_atomic_or_conj((X1,X2)):- !, simply_atomic_or_conj(X1),simply_atomic_or_conj(X2).
simply_atomic_or_conj(X1):- simply_atomic(X1).

simply_atomic(X1):- var(X1),!,fail.
simply_atomic(X1):- \+ compound_gt(X1,0),!.
simply_atomic(not(X1)):-!, simply_atomic(X1).
simply_atomic(at(X1,_)):-!, simply_atomic(X1).
simply_atomic((_;_)):- !, fail.
simply_atomic(X1):- compound_name_arguments_maybe_zero(X1,F,Args), simply_atomic_f(F), maplist(simply_atomic_arg,Args).
simply_atomic_f(F):- \+ sent_op_f(F).

sent_op_f(F):- upcase_atom(F,FU),FU=F.

simply_atomic_arg(A):- var(A);simply_atomic(A). 

assert_1pddl_pddl_try_harder_now((X2 if X1),(if X1 then X2)):- simply_atomic_or_conj(X1), simply_atomic_or_conj(X2).


assert_1pddl_pddl_try_harder1(Prolog):-  assert_1pddl_pddl_try_harder_now(Prolog,Again),
  lps_xform(lps_test_mod,Again,PrologAgain),Again\==PrologAgain,!, 
   print_lps_syntax(yellow,Again),
   pprint_ecp_cmt(cyan,PrologAgain),
   pprint_ecp_cmt(white,"% ================================="),
   !.
%assert_1pddl_pddl_try_harder(Prolog):- on_x_fail(assert_1pddl_pddl_try_harder1(Prolog)),!.
assert_1pddl_pddl_try_harder(Prolog):- pprint_ecp(red,Prolog),!.

argtype_pred(event,events).
argtype_pred(fluent,fluents).
argtype_pred(action,actions).
argtype_pred(predicate,predicates).
argtype_pred(Action,Actions):- arg_info(domain,Action,arginfo),atom_concat(Action,"s",Actions).

protify(both,Form,[Lps1,Lps2]):- protify(events,Form,Lps1),protify(action,Form,Lps2).
protify(Type,Form,Lps):- is_list(Form),!,maplist(protify(Type),Form,Lps).
protify(Type,Form,Lps):- argtype_pred(Type,LPSType), \+ callable(Form),!,Lps=..[LPSType,[Form]].
protify(Type,Form,Lps):- argtype_pred(Type,LPSType), \+ compound(Form),!,Lps=..[LPSType,[Form/0]].
protify(Type,F/A, Lps):- argtype_pred(Type,LPSType), integer(A),!,Lps=..[LPSType,[F/A]].
protify(Type,(X1,X2),[Lps1,Lps2]):- !, protify(Type,X1,Lps1),protify(Type,X2,Lps2).
%protify(Type,X,Lps):- cfunctor(X,F,A),Lps=(F/A).
protify(Event,X,LPS):- ((event) == Event), compound(X), arg(1,X,Agent),
  is_agent(Agent),
  !,protify(both,X,LPS).
protify(Type,X,[mpred_prop(X,Type),LPS]):- argtype_pred(Type,LPSType),protify(LPSType,X,LPS).
protify(LPSType,X,LPS):- cfunctor(X,F,A),cfunctor(_Lps,F,A),!,Pred=..[LPSType,[F/A]],LPS=[Pred].

is_agent(Agent):- \+ atom(Agent),!,fail.
is_agent(diver).
is_agent(agent).
is_agent(Agent):- call_u(subsort(Agent,agent)),!.

:- fixup_exports.


:- listing(test_lps_pddl_ereader).

%:- break.

