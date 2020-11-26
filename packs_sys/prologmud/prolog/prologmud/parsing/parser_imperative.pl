%:- if(( ( \+ ((current_prolog_flag(logicmoo_include,Call),Call))) )). 
/*
:- module(parser_imperative, [
                  
]).
*/
%:- endif.
/* * <module>  parser_imperative - Imperitive Sentence Parser (using DCG)
%
% Logicmoo Project PrologMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/
:- include(prologmud(mud_header)).
:- use_module(library(pfc)).

:- ensure_loaded(library(instant_prolog_docs)).

:- virtualize_source_file.

:-export((
    parse_agent_text_command/5,            
    parse_agent_text_command_0/5,            
    objects_match/3,
    match_object/2,
    object_string/2,
    save_fmt_a_0/2,
    save_fmt_a/2,
    % coerce/3,
    parseIsa//2,
    get_agent_text_command_0/4,
    phrase_parseForTypes_9//2,
    guess_nameStrings/2,
    parseForTypes//
    2)).

% :- register_module_type (utility).

:- if(false).
:- set_prolog_flag(gc,true).
:- trim_stacks.
:- garbage_collect_atoms.   
:- garbage_collect_clauses.
:- garbage_collect.
:- statistics.
%:- set_prolog_flag(%%gc,false).
:- endif.

some_term_to_atom(Term,Atom):- must(\+ is_list(Term)), term_to_atom(Term,Atom).

% =====================================================================================================================
% get_agent_text_command/4
% =====================================================================================================================
:-export(get_agent_text_command/4).
:-export(get_agent_text_command_0/4).

get_agent_text_command(Agent,VERBOrListIn,AgentR,CMD):-
   on_x_debug(loop_check(get_agent_text_command_0(Agent,VERBOrListIn,AgentR,CMD),fail)).

get_agent_text_command_0(Agent,ListIn,AgentR,CMD):- 
   (is_list(ListIn) -> UseList=ListIn ; UseList=[ListIn]),
       call_no_cuts(agent_text_command(Agent,UseList,AgentR,CMD)).


want_more_question(G):-call(G),!.

% ===========================================================
% PARSE command
% ===========================================================
:-ain((==>type_action_info(tHumanControlled,actParse(tCol,ftListFn(ftString)),"Development test to parse some Text for a human.  Usage: parse 'item' the blue backpack"))).

agent_command(_Gent,actParse(Type,StringM)):-
   want_more_question(parse_for(Type,StringM,_Term,_LeftOver)).

% ===========================================================
% CMDPARSE command
% ===========================================================
:-ain((==>type_action_info(tHumanControlled,actCmdparse(ftListFn(ftTerm)),"Development test to parse some Text for a human.  Usage: cmdparse take the blue backpack"))).

agent_command(_Gent,actCmdparse(StringM)):- !, want_more_question(parse_for(ftAction,StringM,Term,LeftOver)),fmt('==>'(parse_for(StringM) , [Term,LeftOver])).

% baseKB:mud_test("cmdparse test",...)
  

% ===========================================================
% parsetempl command
% ===========================================================
:-ain((==>type_action_info(tHumanControlled,actParsetempl(ftListFn(ftTerm)),"Development test to see what verb phrase heads are found. (uses get_vp_templates/4)  Usage: parsetempl who"))).


% :- use_module(library(func)).
% guess_nameStrings $ actParsetempl
% agent_text_command(Agent,[guess_nameStrings $ actParsetempl|List],Agent,actParsetempl(List)):- cwc.
agent_text_command(Agent,[Result|List],Agent,actParsetempl(List)):- guess_nameStrings( actParsetempl, Result).


agent_command(Agent,actParsetempl(StringM)):-
  to_word_list(StringM,[SVERB|ARGS]),
  get_vp_templates(Agent,SVERB,ARGS,TEMPLATES),fmt(templates=TEMPLATES),
  ignore((
     parse_for(ftAction,StringM,Goal,LeftOver),
     fmt([goal=Goal,lfto=LeftOver]))).

% ===========================================================
% parse_for/2-N
% ===========================================================
% should not be used really
% parse_for(Type,StringM):- parse_for(Type,StringM, _Term).

parse_for(Type,StringM, Term):-parse_for(Type,StringM, Term, Out),Out==[].

list_tail(_,[]).
list_tail(String,LeftOver):-ground(String),to_word_list(String,List),length(List,L),!,between(1,L,X),length(LeftOver,X).

:-export(parse_for/4).
parse_for(Type,StringM,Term,LeftOver):-
   to_word_list(StringM,String),  
   list_tail(String,LeftOver),
   HOW = phrase(parseIsa(Type,Term),String,LeftOver),
   fmt('parsing with ~q ~n.',[HOW]),
   (on_x_debug(HOW)*->
      fmt('Success! parse \'~q\' "~q" = ~q   (leftover=~q) . ~n',[Type,String,Term,LeftOver]);
      (fmt('No Success.~n',[]),!,fail)).

meets_desc_spec(T,_L):- some_term_to_atom(T,S0),string_to_atom(S0,A),atomic_list_concat_catch([_,_|_],'mudBareHandDa',A),!,fail.
meets_desc_spec(_,[]):-!.
meets_desc_spec(S,[DS|SL]):-!,meets_desc_spec(S,DS),meets_desc_spec(S,SL),!.
meets_desc_spec(S,From-To):-!, desc_len(S,Len),!, between(From,To,Len).
meets_desc_spec(_,_).

desc_len(S0,Region):- call(some_term_to_atom(S0,S)),
   atomic_list_concat_catch(Words,' ',S),length(Words,Ws),atomic_list_concat_catch(Sents,'.',S),length(Sents,Ss),Region is Ss+Ws,!.


:-export(objects_match_for_agent/3).
objects_match_for_agent(Agent,Text,ObjList):- 
   objects_match_for_agent(Agent,Text,
   [mudPossess(Agent,isThis),
    isSame(mudAtLoc),
    isSame(localityOfObject),tAgent,tItem,tRegion],ObjList).  
:-export(objects_match_for_agent/4).
objects_match_for_agent(Agent,Text,Match,ObjList):- trace,
 objects_for_agent(Agent,isOneOf([text_means(Agent,Text,isThis),
    isAnd([isOneOf(Match),match_object(Text,isThis)])]),ObjList).  


text_means(Agent,Text,Agent):- string_equal_ci(Text,"self"),!.
text_means(Agent,Text,Loc):- string_equal_ci(Text,"here"),where_atloc(Agent,Loc).
text_means(Agent,Text,Region):- string_equal_ci(Text,"region"),where_atloc(Agent,Loc),locationToRegion(Loc,Region).
text_means(_Agent,_Text,_Value):-fail.

relates(Agent,Relation,Obj):-loop_check(relates_ilc(Agent,Relation,Obj),fail).
relates_ilc(Agent,Relation,Obj):-text_means(Agent,Relation,Obj),!.
relates_ilc(_    ,Relation,Obj):- atom(Relation),tCol(Relation),!,isa(Obj,Relation).
relates_ilc(_    ,Relation,Obj):-contains_var(Relation,isThis),subst(Relation,isThis,Obj,Call),!,req1(Call).
relates_ilc(Agent,isSame(Relation),Obj):- !, relates(Agent,Relation,Value),relates(Obj,Relation,Value).
relates_ilc(Agent,Relation,Obj):- atom(Relation),!, prop(Agent,Relation,Obj).
relates_ilc(_    ,Relation,Obj):-contains_var(Relation,Obj),!,req1(Relation).
relates_ilc(Agent,Relation,Obj):-contains_var(Relation,Agent),append_term(Relation,Obj,Call),!,req1(Call).
relates_ilc(Agent,Relation,Obj):-objects_for_agent(Agent,Relation,MatchList),MatchList\=[],!,member(MatchList,Obj).


objects_for_agent(_Agent,isAnd([]),[]):-!.
objects_for_agent(_Agent,isMost([]),[]):-!.
objects_for_agent(_Agent,isOneOf([]),[]):-!.

objects_for_agent(Agent,isOneOf([Possible|Relations]),MatchList):-!,
   objects_for_agent(Agent,Possible,L1),
   objects_for_agent(Agent,Relations,L2),
   append(L1,L2,MatchListL),!,
   list_to_set(MatchListL,MatchList),!.

objects_for_agent(Agent,members(List,Relations),MatchList):-!, findall(Obj,(member(Obj,List),relates(Agent,Relations,Obj)),MatchListL),list_to_set(MatchListL,MatchList),!.

objects_for_agent(Agent,isAnd([Possible|Relations]),MatchList):-!,
   objects_for_agent(Agent,Possible,L1),
   objects_for_agent(Agent,members(L1,Relations),MatchListL),
   list_to_set(MatchListL,MatchList),!.
objects_for_agent(Agent,Relation,MatchList):- findall(Obj, relates(Agent,Relation,Obj), MatchListL),list_to_set(MatchListL,MatchList),!.


objects_match(Text,Possibles,MatchList):- findall(Obj,(member(Obj,Possibles),match_object(Text,Obj)), MatchList).

:-dynamic(object_string/2).
object_string(O,String):-nameString(O,String).
object_string(O,String):-mudKeyword(O,String).

nameString(O,S)==>mudKeyword(O,S).

%:- begin_tests(parser_imparative,[setup(foc_current_agent(_))]).

%:- end_tests(parser_imparative).
        
/*
object_string(O,String) :-  object_string(_,O,1-4,String),!.
object_string_0_5(O,String):-object_string(_,O,0-5,String),!.

:-export(object_string/4).
:-dynamic lmcache:object_string_fmt/3.
:-retractall(lmcache:object_string_fmt(_,_,_)).
object_string(_,O,DescSpecs,String):- lmcache:object_string_fmt(O,DescSpecs,String),!.
object_string(Agent,O,DescSpecs,String):- String = [O], 
   object_print_details(save_fmt(String),Agent,O,DescSpecs,[tCol,tItem,ftProlog,ftID,ftTerm,tAgent,tChannel]),
   asserta(lmcache:object_string_fmt(O,DescSpecs,String)),!.

save_fmt(OS,' ~w ',[A]):-!,save_fmt_e(OS,A),!.
save_fmt(OS,'~w',[A]):-!,save_fmt_e(OS,A),!.
save_fmt(OS,'~w',[A]):-!,save_fmt_e(OS,A),!.
save_fmt(OS,Fmt,[A|KW]):-sformat(Str,Fmt,[A|KW]),to_word_list(Str,WL),save_fmt_e(OS,WL),!.

save_fmt_e(_,E):-var(E),!.
save_fmt_e(_,[]):-!.
save_fmt_e(O,A):-atom(A),!,save_fmt_a(O,A),!.
save_fmt_e(O,[E|L]):-!,save_fmt_e(O,E),!,save_fmt_e(O,L),!.
save_fmt_e(O,isa(A)):-!,must(save_fmt_e(O,A)).
save_fmt_e(O,t(A,_)):-!,must(save_fmt_e(O,A)).
save_fmt_e(O,xti(A,_)):-!,must(save_fmt_e(O,A)).
save_fmt_e(_,E):-compound(E),!. % cycPred(_),predStub(_),cycPlus2(_),predStub(_),predicateConventionMt(_),arity(_),
%save_fmt_e(O,E):- string(E),!,must((to_word_list(E,WL),save_fmt_e(O,WL))),!.
save_fmt_e(O,E):- member_eq0(E,O) -> true ; (O=[_|CDR],nb_setarg(2,O,[E|CDR])).

member_eq0(X, [Y|Ys]) :- X==Y;member_eq0(X,Ys).

save_fmt_a(P,A):-loop_check(save_fmt_a_0(P,A),true),!.

save_fmt_a_0(_,E):-var(E),!.
save_fmt_a_0(O,xti(E,_)):-!,save_fmt_a(O,E).
save_fmt_a_0(O,t(E,_)):-!,save_fmt_a(O,E).
save_fmt_a_0(O,E):-compound(O),arg(1,O,E),!,save_fmt_a(O,E).
save_fmt_a_0(_,A):-atom(A),atom_length(A,L),L =< 1.
save_fmt_a_0(_,A):-vtSkippedPrintNames(A),!.
save_fmt_a_0(O,E):-to_case_breaks(E,List),must_maplist(save_fmt_a(O),List).


object_name_is_descriptive(O):- (isa(O,tCol);isa(O,tPred);t(functorDeclares,O);isa(O,ttValueType),isa(O,name_is_descriptive)).
*/

/*
:-export(object_print_details/5).


object_print_details(Print,Agent,O,DescSpecs,Skipped):- dumpST, break,atoms_of(O,OS),!,
   forall(member(M,OS),object_print_details0(Print,Agent,M,DescSpecs,Skipped)).

object_print_details0(Print,Agent,O,DescSpecs,Skipped):-
   member(O,Skipped) -> true ;
  (    
    ignore(forall(name_text(O,KW),ignore((meets_desc_spec(KW,DescSpecs)->call(Print,' ~w ',[KW]))))),
   (object_name_is_descriptive(O) -> true ; 
    (( 
       forall(is_asserted(descriptionHere(O,KW)),ignore((meets_desc_spec(KW,DescSpecs)->call(Print,' ~w ',[KW])))),
       forall(isa(O,S),ignore(( \+ (vtSkippedPrintNames(S)),object_print_details0(Print,Agent,S,DescSpecs,[O|Skipped])))))))).

%tCol(ttTypeType).
vtSkippedPrintNames(T):-var(T),!,fail.
vtSkippedPrintNames(T):-ttExpressionType(T).
%vtSkippedPrintNames(T):-isa(T,ttTypeType).
vtSkippedPrintNames(E):-member(E,[tObj,isThis,the,is,tSpatialThing,ttNotSpatialType,ttSpatialType,prologHybrid,t,prologPTTP,prologKIF,prologDynamic,tRelation,tPred,'',[]]).


must_make_object_string_list(_,Obj,WList):- object_string(Obj,WList),!.
must_make_object_string_list(P,Obj,WList):- lc_tcall(must_make_object_string_list_cached(P,Obj,WList)).
must_make_object_string_list_cached(P,Obj,WList):-
  must((object_string(P,Obj,0-5,String),nonvar(String),non_empty(String),string_ci(String,LString),convert_to_string_list(LString,WList))).
*/
same_ci(A,B):-quietly((must((non_empty(A),non_empty(B))),any_to_string(A,StringA),any_to_string(B,StringB),!,string_ci(StringA,StringB))),!.

match_object(S,Obj):-var(S),!,fail,freeze(S,match_object(S,Obj)).
match_object(S,Obj):-var(Obj),!,fail,freeze(Obj,match_object(S,Obj)).
match_object(S,Obj):-number(S),atom_number(A,S),!,match_object(A,Obj).
match_object(S,Obj):-same_ci(S,Obj),!.
match_object(S,Obj):-atomic(S),string_to_atom(S,ID),call_u(tKnownID(ID)),!,(var(Obj)->Obj=ID;same_ci(ID,Obj)).
match_object(S,Obj):-name_text(Obj,S),!.
match_object(S,Obj):-i_name(Obj,S),!.
match_object([S],Obj):-!,match_object(S,Obj).
match_object([S1|S],Obj):-match_object(S1,Obj),match_object(S,Obj),!.
% match_object(S,Obj):-atomic(S),string_to_atom(S,ID),call_u(tIndividual(ID)),!,(var(Obj)->Obj=ID;same_ci(ID,Obj)).
match_object(S,Obj):-to_case_breaks(Obj,List)->member(xti(Str,_),List),string_equal_ci(S,Str),!.
/*
match_object(S,Obj):-ground(S:Obj),match_object_exp(S,Obj),!.

match_object_exp(S,Obj):-sanity(ground(S:Obj)),must(((atoms_of(S,Atoms),!,Atoms\=[]))),match_object_0(Atoms,Obj).

match_object_0([S],Obj):-nonvar(S),match_object_1(S,Obj),!.
match_object_0(Atoms,Obj):-
   current_agent_or_var(P),quietly(must_make_object_string_list(P,Obj,WList)),!,
   forall(member(A,Atoms),(member(W,WList),quietly(string_equal_ci(A,W)))).

match_object_1(A,Obj):-same_ci(A,Obj),!.
match_object_1(A,Obj):-isa(Obj,Type),same_ci(A,Type),!.
*/
:-nodebug(logicmoo(parser)).
:-debug(logicmoo(parser)).

% dmsg_parserm(D):-dmsg(D),!.
dmsg_parserm(D):- dmsg_parserm('~N~q~n',[D]).
dmsg_parserm(F,A):-ignore((debugging_logicmoo(logicmoo(parser)),dmsg(F,A))).


% ===========================================================
% PARSER
% ===========================================================


must_atomics(A):-must(atomic(A)).

destringify_arg(A,A).

parse_agent_text_command(Agent,SVERB,Args,NewAgent,GOAL):- destringify_arg(SVERB,AVERB),SVERB \=@= AVERB,!,
   parse_agent_text_command(Agent,AVERB,Args,NewAgent,GOAL).
parse_agent_text_command(Agent,SVERB,Args,NewAgent,GOAL):- is_list(Args),maplist(destringify_arg,Args,AArgs),AArgs \=@= Args,!,
   parse_agent_text_command(Agent,SVERB,AArgs,NewAgent,GOAL).
parse_agent_text_command(Agent,SVERB,[],NewAgent,GOAL):-compound(SVERB),!,must((NewAgent=Agent,GOAL=SVERB)),!.
parse_agent_text_command(_Agent,SVERB,ARGS,_,_):-slow_sanity((must(atomic(SVERB)),maplist(must_atomics,ARGS))),fail.

parse_agent_text_command(Agent,SVERB,ARGS,NewAgent,GOAL):-
  dmsg(parse_agent_text_command(Agent,SVERB,ARGS,NewAgent,GOAL)),
  parse_agent_text_command_0(Agent,SVERB,ARGS,NewAgent,GOAL),
   dmsg_parserm(succeed_parse_agent_text_command_0(Agent,SVERB,ARGS,NewAgent,GOAL)),!.

parse_agent_text_command(Agent,VERB,[PT2|ARGS],NewAgent,GOAL):-
   atomic_list_concat_catch([VERB,PT2],'_',SVERB),
   parse_agent_text_command_0(Agent,SVERB,ARGS,NewAgent,GOAL),!,
   dmsg_parserm(special_succeed_parse_agent_text_command_0(Agent,SVERB,ARGS,NewAgent,GOAL)),!.

parse_agent_text_command(Agent,PROLOGTERM,[],Agent,actProlog(PROLOGTERM)):- nonvar(PROLOGTERM),predicate_property(PROLOGTERM,_),!.

parse_agent_text_command(Agent,SVERB,ARGS,NewAgent,GOAL):-
 dmsg(failed_parse_agent_text_command_0(Agent,SVERB,ARGS,NewAgent,GOAL)),
 debugging_logicmoo(logicmoo(parser)),
 % debug,visible(+all),leash(+all), 
 parse_agent_text_command_0(Agent,SVERB,ARGS,NewAgent,GOAL),!.

parse_agent_text_command(Agent,IVERB,ARGS,Agent,GOAL):- 
  ground(IVERB), string_to_atom(IVERB,VERB),GOAL=..[VERB|ARGS],!.

% try directly parsing first
parse_agent_text_command_0(Agent,SVERB,ARGS,NewAgent,GOAL):- 
   call_no_cuts(agent_text_command(Agent,[SVERB|ARGS],NewAgent,GOAL)),nonvar(NewAgent),nonvar(GOAL),!.   

% try indirectly parsing
parse_agent_text_command_0(Agent,SVERB,ARGS,NewAgent,GOAL):- 
   call_no_cuts(agent_text_command(Agent,[VERB|ARGS],NewAgent,GOAL)),ground(GOAL),nonvar(VERB),
   verb_matches(SVERB,VERB).

parse_agent_text_command_0(Agent,SVERB,ARGS,Agent,GOAL):-
   parse_agent_text_command_1(Agent,SVERB,ARGS,Agent,GOAL).

parse_agent_text_command_0(Agent,IVERB,ARGS,NewAgent,GOAL):-
   verb_alias_to_verb(IVERB,SVERB), IVERB\=SVERB,!,
   parse_agent_text_command(Agent,SVERB,ARGS,NewAgent,GOAL).

parse_agent_text_command_0(Agent,PROLOGTERM,[],Agent,actProlog(req1(PROLOGTERM))):- compound(PROLOGTERM),functor(PROLOGTERM,F,A),
  mpred_prop(_,F,A,_),!.
parse_agent_text_command_0(Agent,PROLOGTERM,[],Agent,actProlog(req1(PROLOGTERM))):- compound(PROLOGTERM),pfc_is_callable(PROLOGTERM),!.

:-export(parse_agent_text_command_1/5).
% parses a verb phrase and retuns one interpretation (action)
parse_agent_text_command_1(Agent,SVERB,ARGS,Agent,GOAL):-

   parse_vp_real(Agent,SVERB,ARGS,GOALANDLEFTOVERS),
   dmsg_parserm(parserm("GOALANDLEFTOVERS"=GOALANDLEFTOVERS)),
   GOALANDLEFTOVERS \= [],
   must(chooseBestGoal(GOALANDLEFTOVERS,GOAL)),
   dmsg_parserm(parserm("chooseBestGoal"=GOAL)).


text_actverb("i",actInventory).
text_actverb("l",actLook).
text_actverb("lo",actLook).
text_actverb("s",actMove(vSouth)).
text_actverb("go",actMove).
text_actverb("where is",actWhere).
% text_actverb(["where","is"],actWhereTest).

% remove nonstringed aliases
:-ain(((text_actverb(NonStr, Act), {\+ is_ftText(NonStr),convert_to_cycString(NonStr,EStr)}) ==> 
    text_actverb(EStr, Act),
   { ignore(call(call,retractall(( text_actverb(NonStr, Act)) ))) } )).
                     
ttTypeType(ttCoercable).

genls(ttStringType,ttCoercable).

nameString(O,S):-nonvar(O),nonvar(S),nameString(O,SU),same_ci(S,SU).

:- ain(((ttCoercable(StringType),argIsa(F,N,StringType),arity(F,A),
  {functor(P,F,A),P,arg(N,P,NonStr),\+ isa(NonStr,StringType),coerce(NonStr,StringType,EStr),replace_arg(P,N,EStr,Q)})
  ==> 
   ( \+ P,{coerce(NonStr,StringType,EStr)},Q))).

:- dmsg(call(listing(text_actverb/2))).


% :- sanity((clause_u(text_actverb(S,actWhere)),argIsa(text_actverb,1,C),isa(S,C))).

:- must((clause_u(text_actverb(S,actWhere))))->must(((argIsa(text_actverb,1,C);argQuotedIsa(text_actverb,1,C)),(isa(S,C);quotedIsa(S,C)))).

%:- listing(text_actverb/2).
%:- break.

% pos_word_formula('infinitive',Verb,Formula):- 'infinitive'(TheWord, Verb, _, _G183), 'verbSemTrans'(TheWord, 0, 'TransitiveNPCompFrame', Formula, _, _).

verb_alias_to_verb(IVERB,SVERB):- text_actverb(L,Look),verb_matches(L,IVERB),SVERB=Look,!.
verb_alias_to_verb(IVERB,SVERB):- coerce(IVERB,vtVerb,SVERB), IVERB \= SVERB.

subst_parser_vars(Agent,TYPEARGS,TYPEARGS_R):- subst(TYPEARGS,isSelfAgent,Agent,S1),where_atloc(Agent,Here),subst(S1,vHere,Here,TYPEARGS_R).

% verb_matches("go",VERB):-!,VERB=go.
verb_matches(SVERB,VERB):- samef(VERB,SVERB)*->true;name_text_matches(SVERB,VERB).

name_text_matches(SVERB,VERB):-name_text(SVERB,SSTR),name_text(VERB,STR),same_ci(SSTR,STR),!.


get_vp_templates(_Agent,SVERB,_ARGS,TEMPLATES):-
   findall([VERB|TYPEARGS],
    ((
      get_all_templates(TEMPL),
     %isa(Agent,What),
     %action_info(What,TEMPL,_),
     TEMPL=..[VERB|TYPEARGS],
     (verb_matches(SVERB,VERB)))),
     TEMPLATES_FA),
    % ( TEMPLATES_FA=[] -> (dmsg(noTemplates(Agent,SVERB,ARGS)),!,fail); true),
   predsort(mostIdiomatic,TEMPLATES_FA,TEMPLATES).
   
% parses a verb phrase and retuns multiple interps
parse_vp_real(Agent,SVERB,ARGS,Sorted):- locally_tl(infSkipFullExpand,parse_vp_real_no_arg_checking(Agent,SVERB,ARGS,Sorted)).
parse_vp_real_no_arg_checking(Agent,SVERB,ARGS,Sorted):-
   get_vp_templates(Agent,SVERB,ARGS,TEMPLATES),   
   dmsg_parserm(("TEMPLATES"= (orig([SVERB|ARGS]) = TEMPLATES))),
   TEMPLATES \= [],
   list_to_set(TEMPLATES,TEMPLATESSET),
   findall(LeftOver-GOAL,
     (( 
      member([VERB|TYPEARGS],TEMPLATESSET),
      once(((subst_parser_vars(Agent,TYPEARGS,TYPEARGS_R),
      subst_parser_vars(Agent,ARGS,ARGS_R)))),
      dmsg_parserm(("parseForTypes"=phrase_parseForTypes(TYPEARGS_R,ARGS_R,GOODARGS,LeftOver))),      
      phrase_parseForTypes(TYPEARGS_R,ARGS_R,GOODARGS,LeftOver),
      GOAL=..[VERB|GOODARGS])),
      GOALANDLEFTOVERS_FA),
   sort(GOALANDLEFTOVERS_FA,GOALANDLEFTOVERS),
   predsort(bestParse,GOALANDLEFTOVERS,Sorted),!.

chooseBestGoal([_LeftOver - GOAL],GOAL):-!.
chooseBestGoal(GOALANDLEFTOVERS,GOAL):-
   predsort(bestParse,GOALANDLEFTOVERS,Sorted),
   dmsg_parserm(("Sorted"=Sorted)),
   member(_LeftOver - GOAL,Sorted),!.

% mostIdiomatic(?Order, @Term1, @Term2)
mostIdiomatic(Order, Term1, Term2):-mostComplex(Order, Term1, Term2).
% mostComplex(?Order, @Term1, @Term2)
mostComplex(Order, Term1, Term2):-complexity_count(Term1,Complexity1),complexity_count(Term2,Complexity2),compare(Order,Complexity2,Complexity1),Order \== '=' ,!.
mostComplex(Order, Term1, Term2):-compare(Order,Term1,Term2).

complexity_count(S,-1):-var(S).
complexity_count(S,L):-string(S),!,string_length(S,L).
complexity_count(S,1):-atomic(S),!.
complexity_count([H|T],L):-!,complexity_count(H,HL),complexity_count(T,TL),L is HL+TL.
complexity_count(S,L):-functor(S,_,A),S=..[_|ARGS],!,complexity_count(ARGS,AL),L is A+AL.

% bestParse(?Order, @Term1, @Term2)
bestParse(Order,LeftOver1-GOAL1,LeftOver2-GOAL2):-
   length(LeftOver1,L1),length(LeftOver2,L2),
   functor_safe(GOAL1,_,A1),functor_safe(GOAL2,_,A2),
   must(once(bestParse(Order,LeftOver1-GOAL2,LeftOver1-GOAL2,L1,L2,A1,A2))).

:-style_check(-singleton).

bestParse(Order,LeftOver1-GOAL2,LeftOver1-GOAL2,L1,L2,A1,A2):-
   compare(Order,L1,L2), Order \== '='.
bestParse(Order,LeftOver1-GOAL2,LeftOver1-GOAL2,L1,L2,A1,A2):-
   compare(Order,-A1,-A2), Order \== '='.
bestParse(Order,LeftOver1-GOAL2,LeftOver1-GOAL2,L1,L2,A1,A2):-
   compare(Order,GOAL1,GOAL2).

:-style_check(+singleton).

:-  dynamic(name_text_compute_now/2).
:-multifile(name_text_compute_now/2).
:-   export(name_text_compute_now/2).
name_text_compute_now(Obj,Text):- name_text_cached(Obj,Text),!.
name_text_compute_now(Obj,Text):- atomic(Obj),
  guess_nameStrings(Obj,Text),!,
  maybe_ain_nameString(Obj,Text).

maybe_ain_nameString(Obj, Text):- name_text_cached(Obj,Text),!.
maybe_ain_nameString(_Obj,Text):- Text=="",!,fail.
maybe_ain_nameString(Obj, Text):- Obj=Text,!.
%maybe_ain_nameString(Obj,_Text):- string(Obj),!.
maybe_ain_nameString(Obj,Text):- ain(nameString(Obj,Text)).

:-multifile(name_text/2).
:-dynamic(name_text/2).
:-export(name_text/2).
name_text(I,O):- nonvar(O),!,name_text(I,M),string_equal_ci(M,O).
name_text(I,O):- nonvar(I),no_repeats(O,(name_text_compute_now(I,M),any_to_string(M,S), \+ empty_string(S), text_to_string(S,O))).


name_text_cached(Obj,Text):-clause_b(nameString(Obj,Text)).
name_text_cached(Obj,Text):-clause_b(mudKeyword(Obj,Text)).

:- ain('==>'(prologBuiltin(name_text_compute_now(ftTerm,ftString)))).

:- baseKB:import(logicmoo_util_strings:convert_to_cycString/2).

guess_mudDescription(O,S):-guess_mudDescription_0(O,OS),!,convert_to_cycString(OS,S).
guess_mudDescription_0([],_):-!,fail.
guess_mudDescription_0('',_):-!,fail.
guess_mudDescription_0("",_):-!,fail.
guess_mudDescription_0(Name,Text):-string(Name),!,Name=Text.
guess_mudDescription_0(Name,Text):-is_list(Name),!,
    maplist(as_atom,Name,RealName),atomic_list_concat(RealName,Obj),!,guess_mudDescription_0(Obj,Text).
guess_mudDescription_0(Name,Desc):- arity(Name,Int),integer(Int), \+ isa(Name,tCol),atomic(Name),make_summary([],Name,Desc).
guess_mudDescription_0(Name,Desc):- atomic(Name),!,atom(Name),to_case_breaks(Name,TextT),
   maplist(to_descriptive_name(Name),TextT,TextL),!,atomics_to_string(TextL,' ',Desc).

guess_nameStrings(O,S):- guess_nameStrings_0(O,OS),must(nonvar(OS)),!,convert_to_cycString(OS,S),!,OS\=="".
guess_nameStrings_0([],_):-!,fail.
guess_nameStrings_0('',_):-!,fail.
guess_nameStrings_0("",_):-!,fail.
guess_nameStrings_0(Name,Text):-string(Name),!,Name=Text.
guess_nameStrings_0(Name,Text):-is_list(Name),!,
    maplist(as_atom,Name,RealName),atomic_list_concat(RealName,Obj),!,guess_nameStrings_0(Obj,Text).
guess_nameStrings_0(Name,Text):-compound(Name),!, \+ is_ftVar(Name),Name=..[F,A|List],!,guess_nameStrings_0([F,A|List],Text).
guess_nameStrings_0(Name,Text):-atom(Name),to_case_breaks(Name,ListN),to_case_breaks_trimed(Name,ListN,Text).

to_case_breaks_trimed(Name,[xti(TextL,Class),xti(TextR,Class)|ListN],Text):-  
    maplist(to_descriptive_name(Name),[xti(TextL,Class),xti(TextR,Class)|ListN],Desc),
    atomics_to_string(Desc,' ',Text),!.

to_case_breaks_trimed(Name,[_|ListN],Text):- is_list(ListN),!,
    maplist(to_descriptive_name(Name),ListN,Desc),
    atomics_to_string(Desc,' ',Text).

type_descriptive_name(_,Str,Str):-string(Str),!.
type_descriptive_name(Name,xti(Atom,Class),Out):- !,Class\==digit,type_descriptive_name(Name,Atom,Out).
type_descriptive_name(tCol,t,'First-Order').
type_descriptive_name(tPred,Desc,Atom):-longer_sumry(Desc,Atom).
type_descriptive_name(tCol,tt,'Second-Order').
type_descriptive_name(tCol,vt,'Type-Class').
type_descriptive_name(tCol,'Col','Class').
type_descriptive_name(ftNonvar,'Fn','Function').
type_descriptive_name(tFunction,'a','Prototypical').
type_descriptive_name(tFunction,'i','Instance').
type_descriptive_name(ftAtom,'i','Instance').
type_descriptive_name(tCol,'Voprop','Verb-Object Properties').
type_descriptive_name(ftNonvar,Pefix,Desc):-
   type_prefix(Pefix,TypeName),guess_nameStrings(TypeName,Desc).


to_descriptive_name(For,Desc,Atom):- type_descriptive_name(Type,Desc,Atom),isa(For,Type),!.
to_descriptive_name(_For,Pefix,Desc):- call_u(type_prefix(Pefix,TypeName)), guess_nameStrings(TypeName,Desc).
to_descriptive_name(For,xti(Pefix,lower),Desc):-!,to_descriptive_name(For,Pefix,Desc).
to_descriptive_name(_For,Desc,Atom):- longer_sumry(Desc,Atom),!.
to_descriptive_name(_For,Desc,Atom):-any_to_atom(Desc,Atom),!.

:-dynamic(baseKB:ttKeyworded/1).

% :- mpred_trace_exec.
:-ain((tSet(ttKeyworded))).
:-ain((completelyAssertedCollection(ttKeyworded))).
:-ain((vtActionTemplate(AT)/(get_functor(AT,F))) ==> vtVerb(F)).

freeze_safe(A,G):- nonvar(A),freeze(A,G).

onSpawn((ttKeyworded(T),{freeze_safe(F,atomic(F))},isa(F,T),{ \+ call_u(nameString(F,_)),once(guess_nameStrings(F,Txt))}==>(nameString(F,Txt)))).
onSpawn((ttKeyworded(T),{freeze_safe(F,atomic(F))},isa(F,T),{ \+ call_u(mudDescription(F,_)),once(guess_mudDescription(F,Txt))}==>(mudDescription(F,Txt)))).
:-ain((ttKeyworded(vtVerb))).
%:-ain((ttKeyworded(tCol))).
% :-ain((ttKeyworded(tRelation))).
:- mpred_notrace_exec.

impl_coerce_hook(TextS,vtDirection,Dir):- !,
  member(Dir-Text,[vNorth-"n",vSouth-"s",vEast-"e",vWest-"w",vNE-"ne",vNW-"nw",vSE-"se",vSW-"sw",vUp-"u",vDown-"d"]),
  (name_text(Dir,TextS);string_equal_ci(TextS,Text)).

impl_coerce_hook(Text,Subclass,X):- 
   \+ (memberchk(Subclass,[tSpatialThing])),!,
   once((isa_asserted(X,Subclass),
   arg_to_var(ftText,Text,TextVar),
   req1(mudKeyword(X,TextVar)),   
   same_arg(ftText,TextVar,Text))). % dmsg(todo(impl_coerce_hook(Text,Subclass))),impliedSubClass(Subclass,tSpatialThing).


phrase_parseForTypes(TYPEARGS,ARGS,GOODARGS,LeftOver):- % length(TYPEARGS,N),length(GOODARGS,N),!,
  to_word_list(ARGS,ARGSL),!,phrase_parseForTypes_0(TYPEARGS,ARGSL,GOODARGS,LeftOver).

string_append(A,[B1,B2],C,ABC):-append(A,[B1,B2|C],ABC).
string_append(A,[B],C,ABC):-append(A,[B|C],ABC).


is_counted_for_parse(I):-t(tCountable,I), \+ (excluded_in_parse(I)),!.

excluded_in_parse(apathFn(_, _)).
excluded_in_parse(I):-tCol(I).
excluded_in_parse(I):-ttExpressionType(I).
excluded_in_parse(I):-mpred_prop(_,_,meta_argtypes(I)).
excluded_in_parse(apathFn(_ = _)).

instance_for_parse(I):-is_counted_for_parse(I).
insttype_for_parse(I):-findall(C,(instance_for_parse(I),isa_or_type(I,C)),List),list_to_set(List,Set),member(I,Set).

optional_strings_opt.

% optimization for isOptional strings
phrase_parseForTypes_0(TYPEARGS,ARGS,GOODARGS,LeftOver):- optional_strings_opt,
      (string_append(T1,[isOptionalStr(Str)],T2,TYPEARGS),
      (StrT =[_] /*;StrT=[_,_]*/),
      string_append(A1,StrT,A2,ARGS),
      string_equal_ci(Str,StrT)),!,
      show_call((phrase_parseForTypes_1(T1,A1,G1,[]),
         phrase_parseForTypes_9(T2,A2,G2,LeftOver),      
         string_append(G1,[Str],G2,GOODARGS))).
      
phrase_parseForTypes_0(TYPEARGS,ARGS,GOODARGS,LeftOver):-
   show_call(phrase_parseForTypes_1(TYPEARGS,ARGS,GOODARGS,LeftOver)).

phrase_parseForTypes_1(TYPEARGS,ARGS,GOODARGS,LeftOver):- catch(phrase_parseForTypes_9(TYPEARGS,ARGS,GOODARGS,LeftOver),_,fail),!.    
phrase_parseForTypes_1([isOptional(_, W)|TYPEARGS], [], [W|GOODARGS], LeftOver):- phrase_parseForTypes_1(TYPEARGS,[],GOODARGS,LeftOver).
phrase_parseForTypes_1([isOptionalStr(W)|TYPEARGS], [], [W|GOODARGS], LeftOver):- phrase_parseForTypes_1(TYPEARGS,[],GOODARGS,LeftOver).
phrase_parseForTypes_1(TYPEARGS,In,Out,[]):- length(TYPEARGS,L),between(1,4,L),length(In,L),must(Out=In),!,nop(fmt(fake_phrase_parseForTypes_l(foreach_isa(In,TYPEARGS)))),fail.
phrase_parseForTypes_1(TYPEARGS,ARGS,GOODARGS,LeftOver):- on_x_debug(phrase_parseForTypes_9(TYPEARGS,ARGS,GOODARGS,LeftOver)).    

phrase_parseForTypes_9(TYPEARGS,ARGS,GOODARGS,LeftOver):- (LeftOver=[];LeftOver=_ ), phrase(parseForTypes(TYPEARGS,GOODARGS),ARGS,LeftOver).

parseForTypes([], [], A, A):-!.
parseForTypes([TYPE|TYPES], [B|E], [C|C1], G) :- ground(TYPE:C),
        no_repeats_old(parseIsa_Call(TYPE, B, [C|C1], F)),
        parseForTypes(TYPES, E, F, G),!.
parseForTypes([isOptional(_, W)|TYPEARGS], [W|GOODARGS], A, A):- parseForTypes(TYPEARGS,GOODARGS,A,A).


parseIsa_Call(FT, BO, CIn, D):- once((ground(FT:CIn), list_tail(CIn,D), to_word_list(CIn,C))),
   parseIsa(FT, B, C, D),to_arg_value(B,BO).


% this parseIsa(T)-->parseIsa(T,_).
parseIsa(A, B, C) :- parseIsa(A, _, B, C).

is_parsable_type(T):-ttExpressionType(T).
is_parsable_type(T):-tCol(T).
is_parsable_type(ftAction).


%:- begin_tests(test_bad_verb).

baseKB:mud_test(test_bad_verb, [ true(
        \+ (phrase(parseIsa(vtVerb,ff),[ff],[]))
       )] ).


baseKB:mud_test(food_is_a_droppable, [ true(
       parse_agent_text_command(iExplorer1,actDrop,[food],_D2,_E2))]).


%:- end_tests(test_bad_verb).


query_trans_subft(FT,Sub):-subFormat(FT,Sub).
query_trans_subft(FT,Sub):-subFormat(FT,A),genls(A,Sub).
query_trans_subft(FT,Sub):-subFormat(FT,A),subFormat(A,B),subFormat(B,Sub).


parseFmt_vp1(Agent, do(NewAgent,Goal),[SVERB|ARGS],[]):- parse_agent_text_command(Agent,SVERB,ARGS,NewAgent,Goal),!.
parseFmt_vp2(Agent,GOAL,[SVERB|ARGS],UNPARSED):- parse_vp_real(Agent,SVERB,ARGS,TRANSLATIONS),!,member(UNPARSED-GOAL,TRANSLATIONS).

to_arg_value(Var,Var):-is_ftVar(Var),!.
to_arg_value(Val,What):- parserVars((Val;isParserVar(Val)),What,_),!. 
to_arg_value(vHere,Here):-must((current_agent(Who),where_atloc(Who,Here))).
to_arg_value(isSelfAgent,Who):-must((current_agent(Who))).
to_arg_value(isRandom(Type),Term):- nonvar(Type),!,must((to_arg_value(Type,TypeR),random_instance(TypeR,Term,true))).
to_arg_value(Call,TermO):-compound(Call),Call=..[call|CALLARGS],must((subst(CALLARGS,isThis,Term,CALLARGS2),maplist(to_arg_value,CALLARGS2,CALLARGS3),NewCall=..[call|CALLARGS3],must(req1(NewCall)),to_arg_value(Term,TermO))).
to_arg_value(Term,TermO):-must((map_term(to_arg_value,Term,TermO))).

map_term(Pred,Term,TermO):-var(Term),!,must(call(Pred,Term,TermO)).
map_term(Pred,Term,TermO):-is_list(Term),!,must(maplist(Pred,Term,TermO)).
map_term(Pred,Term,TermO):-compound(Term),Term=..TermL,!,must(maplist(Pred,TermL,TermOL)),TermO=..TermOL.
map_term(_,Term,Term):-!.

/*

some tests

 phrase_parseForTypes([isOptional(tAgent, isRandom(tAgent))], ['Crush'], A, B).

  phrase_parseForTypes([isOptional(tAgent, isRandom(tAgent))], ['Crush'], A, B).

parser_imperative:phrase_parseForTypes_9([isOptional(isAnd([obj, isNot(tRegion)]), 'NpcCol1000-Geordi684'), isOptionalStr("to"), isOptional(tRegion, isRandom(tRegion))], [food, 'Turbolift'], GOODARGS,[]).
parser_imperative:phrase_parseForTypes_9([isOptional(isAnd([obj, isNot(tRegion)]), 'NpcCol1000-Geordi684')], [food], GOODARGS,[]).
parser_imperative:phrase_parseForTypes_9([isOptional(tRegion, isRandom(tRegion))], ['Turbolift'], GOODARGS,[]).


*/
 % :- set_prolog_flag(subclause_expansion,false).

:- dynamic(parseIsa/4).

parseIsa(_T, _, [AT|_], _):- var(AT),!,fail.
parseIsa(FT, B, C, D):- var(FT),trace_or_throw(var_parseIsa(FT, B, C, D)).
parseIsa(Str,A,B,C) :-string(Str),!, parseIsa(exactStr(Str),A,B,C).

% this parseIsa(isNot(T),Term) --> dcgAnd(dcgNot(parseIsa(T)),theText(Term)).

:- call(call,assert((parseIsa(isNot(Type), Term, C, D) :- !, dcgAnd(dcgNot(parseIsa(Type)), theText(Term), C, D)))).

parseIsa(ftAction,Goal,Left,Right):-!,one_must(parseFmt_vp1(isSelfAgent,Goal,Left,Right),parseFmt_vp2(isSelfAgent,Goal,Left,Right)).

:- baseKB:ensure_loaded(library(multimodal_dcg)).

parseIsa(t(P,S,O),TermV) -->{!},parseIsa(call(t(P,S,O)),TermV).
parseIsa(call(Call),TermV) --> {!,subst(Call,isThis,TermV,NewCall)},theText(TermT), {req1(NewCall),match_object(TermT,TermV)}.
parseIsa(exactStr(Str),Str) --> {!},[Atom],{string_equal_ci(Atom,Str),!}.
parseIsa(isOptionalStr(Str),Str) --> { \+ (optional_strings_opt)},[Atom],{string_equal_ci(Atom,Str),!}.
parseIsa(isOptionalStr(_),isMissing) --> {!},[].
parseIsa(isOptionalStr(_Str),_) --> {!,fail}.
parseIsa(isOptional(_,Term),TermV) --> {to_arg_value(Term,TermV)}, [TermT], {samef(TermV,TermT)}.
parseIsa(isOptional(Type, _), TermV, C, D) :- nonvar(Type),parseIsa(Type, TermV, C, D).
parseIsa(isOptional(_Type,Default), DefaultV, D, D2):- !,D=D2,to_arg_value(Default,DefaultV).

%  parser_imperative:phrase_parseForTypes_9([isOptional(isAnd([obj, isNot(tRegion)]),'NpcCol1000-Geordi684'),isOptionalStr("to"),isOptional(tRegion, isRandom(tRegion))], [food], GOODARGS,[]).
%  parser_imperative:phrase_parseForTypes_9([isOptional(isAnd([obj, isNot(tRegion)]),'NpcCol1000-Geordi684'),isOptionalStr("to"),isOptional(tRegion, isRandom(tRegion))], [food,to,'Turbolift'], GOODARGS,[]).
%  parser_imperative:phrase_parseForTypes_9([tRegion], ['Turbolift'], GOODARGS,[]).

parseIsa(ftString,String)--> {!}, theString(String).
parseIsa(FT, B, [AT|C], D) :- nonvar(AT),member_ci(AT,["the","a","an"]),parseIsa(FT, B, C, D).

parseIsa(isOneOf(List),Term) --> {!,member(E,List)},parseIsa(E,Term).


parseIsa(ftListFn(Type),[Term|List]) --> parseIsa(Type,Term),parseIsa(ftListFn(Type),List).
parseIsa(ftListFn(_Type),[]) --> {!},[].

parseIsa(countBetween(_Type,_,High),[]) --> {High==0,!}, [].
parseIsa(countBetween(Type,Low,High),[Term|List]) --> parseIsa(Type,Term),{!,Low2 is Low -1,High2 is High -1 },
   parseIsa(countBetween(Type,Low2,High2),List).
parseIsa(countBetween(_Type,Low,_),[]) --> {!, Low < 1}, [].

% parseIsa(isAnd([L|List]),Term1) --> dcgAnd(parseIsa(L,Term1),parseIsa(isAnd(List),Term2)),{ignore(Term1==Term2),!}.
parseIsa(isAnd([L]),Term1) --> {!},parseIsa(L,Term1).
parseIsa(isAnd([L|List]),Term) --> {!},dcgAnd(parseIsa(L,Term),parseIsa(isAnd(List),Term)).

parseIsa(isMost(List),Term1) --> {!},parseIsaMost(List,Term1).


parseIsa(Type,Term)--> dcgAnd(dcgLenBetween(1,2),theText(String)),{coerce(String,Type,Term)}.

parseIsaMost(List,Term) --> parseIsa(isAnd(List),Term),{!}.
% parseIsaMost(A, B, C, D) :- parseIsa(isAnd(A), B, C, E), !, D=E.

coerce_as(B,A,C):-coerce(A,B,C).

coerce_hook(A,B,C):- (var(A);var(B)),!,fail,trace_or_throw(freeze(A,coerce_hook(A,B,C))).
% futureAssertion: THIS IS WHAT I THINKL THE CODE SHOULD LIKE
% coerce_hook(A,B,C):- to_arg_value(A,AStr),A\=@=AStr,!,coerce_hook(AStr,B,C).
coerce_hook(A,B,C):- to_arg_value(A,AStr),isa_or_type(AStr,B),AStr=C,!.
coerce_hook(A,B,C):- no_repeats(C,(coerce0(A,B,C0),to_arg_value(C0,C))),(show_failure(isa_or_type(C,B))->!;true).
% THIS SHOULD BEEN OK.. coerce_hook(AStr,B,C):- any_to_string(AStr,A), no_repeats(C,(coerce0(A,B,C0),to_arg_value(C0,C))),(show_failure(ereq(isa(C,B)))->!;true).

% Error conditions
coerce0(String,Type,Inst):- var(Type),!,trace_or_throw(var_specifiedItemType(String,Type,Inst)).

% higher order
coerce0(String,isNot(Type),Inst):-!, sanity(nonvar(Type)), \+ (coerce(String,Type,Inst)).
coerce0(String,isOneOf(Types),Inst):-!, member(Type,Types),coerce(String,Type,Inst),!.

% Strings
coerce0(String,ftString,String):- is_ftString2(String),!.
coerce0(Inst,ftString,String):- \+ is_ftText(Inst),!,must(name_text(Inst,String)).
coerce0(Any,ftString,String):- !, any_to_string(Any,String).


coerce0(IsList,ftListFn(How),Result):- is_list(IsList),!,maplist(coerce_as(How),IsList,Result).
coerce0(IsList,ftListFn(How),Result):- !,maplist(coerce_as(How),[IsList],Result).

coerce0([A],B,C):- string(A),coerce_hook(A,B,C),!.
coerce0(String,Type,Inst):- string(String),string_to_atom(String,Inst),isa_or_type(Inst,Type),!.


% Handles text
coerce0(String,ftText,String):- is_ftText(String),!.
coerce0(String,ftText,StringO):- !,coerce0(String,ftString,StringO).

% User dirrected
%coerce0(String,Type,Inst):- var(String),!,instances_of_type(Inst,Type),name_text(Inst,String).
coerce0(Inst,Type,Inst):- var(Inst),!,instances_of_type(Inst,Type).
% Stop string floundering
coerce0(Any,_,_):- empty_string(Any),!,fail.

% User overloaded
coerce0(Text,Type,Inst):- (no_repeats_old(call_no_cuts(impl_coerce_hook(Text,Type,Inst)))).
coerce0(Inst,Type,InstO):- instances_of_type(Inst,Type),!,InstO=Inst.




coerce0(isRandom(WhatNot),Type,Inst):- !, must((nonvar(WhatNot),to_arg_value(WhatNot,TypeR),random_instance(TypeR,Inst,isa(Inst,Type)))).
coerce0(String,Type,Inst):- Type==tCol, i_name('t',String,Inst),is_asserted(tCol(Inst)),!.
coerce0(String,Type,Inst):- ttExpressionType(Type),!,checkAnyType(clause(assert,actParse),String,Type,AAA),Inst=AAA.
%coerce0(String,Type,Longest) :- findall(Inst, (impl_coerce_hook(Inst,Type,Inst),string_equal_ci(Inst,String)), Possibles), sort_by_strlen(Possibles,[Longest|_]),!.
coerce0(String,C,Inst):- compound(C),!,loop_check(parseIsa(C,Inst,[String],[])).
coerce0(String,Type,Inst):- must(tCol(Type)),instances_of_type(Inst,Type),match_object(String,Inst).

% coerce0(A,Type,AA):- correctAnyType(change(_,_),A,Type,AA).



instances_of_type(Inst,Type):- no_repeats_old(instances_of_type_0(Inst,Type)).

available_instances_of_type(Agent,Obj,Type):- must(current_agent(Agent)), current_agent_or_var(Agent), isa(Obj,Type), mudDistance(Agent,Obj,D),D<6.

% test_with ?- coerce(s,vtDirection,O).
%TODO add back if usefull instances_of_type_0(Inst,Type):- \+ current_prolog_flag(unsafe_speedups , true) , instances_sortable(Type,HOW),!,get_sorted_instances(Inst,Type,HOW).
% should never need this but .. instances_of_type_0(Inst,Type):- genls(SubType,Type),isa(Inst,SubType).
instances_of_type_0(Inst,Type):- isa_or_type(Inst,Type).

instances_sortable(TYPE,HOW):-instances_sortable0(TYPE,HOW),!.
instances_sortable(TYPE,HOW):-genls(TYPE,SUPER),instances_sortable0(SUPER,HOW),!.
instances_sortable(_,distance_to_current_avatar(Agent)):-current_agent_or_var(Agent).

instances_sortable0(tWieldAble,distance_to_current_avatar(Agent)):-current_agent_or_var(Agent).
instances_sortable0(tWearAble,distance_to_current_avatar(Agent)):-current_agent_or_var(Agent).

distance_to_current_avatar(Agent,ORDEROUT,L,R):-mudDistance(Agent,L,L1),mudDistance(Agent,R,R1),compare(ORDER,L1,R1),!, (ORDER == '=' -> naming_order(ORDEROUT,L,R) ; ORDEROUT=ORDER).

% :- register_new_toString_hook(is_mud_object,name_text).

 % :- set_prolog_flag(subclause_expansion,true).

mudDistance(Agent,_Obj,(-1)):- var(Agent),!.
mudDistance(Agent,Obj,0):- mudWielding(Agent,Obj),!.
mudDistance(Agent,Obj,1):- wearsClothing(Agent,Obj),!.
mudDistance(Agent,Obj,2):- mudStowing(Agent,Obj),!.
mudDistance(Agent,Obj,3):- mudPossess(Agent,Obj),!.
mudDistance(Agent,Obj,N):- mudPossess(OtherAgent,Obj),mudDistance(OtherAgent,Obj,AD),!,
  (same_regions(Agent,OtherAgent)->OAD=5; OAD=20),!,N is AD + OAD.
mudDistance(Agent,Obj,5):- same_regions(Agent,Obj),!.
mudDistance(_Agent,_Obj,20).

naming_order(ORDER,L,R):-compare(ORDER,L,R).

get_sorted_instances(Inst,Type,HOW):-findall(Inst,isa(Inst,Type),List),sort(List,NoDupes),predsort(HOW,NoDupes,Sorted),!,member(Inst,Sorted).

% instances_of_type(Inst,Type):- atom(Type), Term =..[Type,Inst], on_x_log_cont(req1(Term)).
% longest_string(?Order, @Term1, @Term2)
/*
longest_string(Order,TStr1,TStr2):-
   text_to_string(TStr1,Str1),string_length(Str1,L1),
   text_to_string(TStr2,Str2),string_length(Str2,L2),
   compare(Order,L2-Str2,L1-Str1).
*/

:- include(prologmud(mud_footer)).
:- all_source_file_predicates_are_transparent.

end_of_file.

text_isa(I,T):-no_repeats_old(hook_text_isa(I,T)).

hook_text_isa(Text,Whatnot):- no_repeats_old(tCol(Whatnot)),isa(Inst,Whatnot), \+ (tCol(Inst)),once(name_text(Inst,Text)).
hook_text_isa(Text,txtVerb):- get_all_templates(A),nonvar(A),functor_safe(A,Inst,_),name_text(Inst,Text).


