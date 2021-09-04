/* *
 module
% Writer NPC_Interface  for supporting inforation based actions
%
%
% Douglas Miles
% Dec 13, 2035
%
%
*/
% :-swi_module(world_text,[]).
%      default_repl_obj_to_string/3,default_repl_writer/4,show_kb_via_pred/3,show_kb_preds/2,show_kb_preds/3,wasSuccess/2
:- include(prologmud(mud_header)).

% :-export(eng_fully_expand/2).

%:- expects_dialect(prolog).

when_command_show(Agent,ActionType):- 
  findall(Show,on_command_show(Agent,ActionType,Show),MORELOOK),
  (MORELOOK==[] -> true;
 (must(mudAtLoc(Agent,LOC)),show_kb_preds(Agent,LOC,MORELOOK))).
 

% ===========================================
% generatePhrase_local(+Term,-English).
% Generate english version of a message
% ===========================================
:-export(generatePhrase_local/2).

:- multifile lmconf:term_to_message_string/2.
:- dynamic lmconf:term_to_message_string/2.

lmconf:term_to_message_string(T,T):-var(T),!.
lmconf:term_to_message_string(T,T):-!.
lmconf:term_to_message_string(txtConcatFn(T),M):-on_x_debug(generatePhrase_local(T,M)),!.
lmconf:term_to_message_string(fmt(T),M):-on_x_debug(generatePhrase_local(T,M)),!.
lmconf:term_to_message_string(C,C):-compound(C),functor(C,F,_),is_leave_alone_msg(F),!.
lmconf:term_to_message_string((T),M):-on_x_fail(generatePhrase_local(T,M)),!.
lmconf:term_to_message_string(T,T):-!.

is_leave_alone_msg(exact_message).
is_leave_alone_msg(todo).
is_leave_alone_msg((error)).
is_leave_alone_msg(parserm).
% is_leave_alone_msg(F):- is_db_prop(F,_,_),!,fail.
is_leave_alone_msg(A):-on_x_fail((sub_atom(A,_,1,0,S),atom_number(S,_))),!.

prologBuiltin(term_anglify_args/6).
prologBuiltin(term_anglify_last/2).
:-export(term_anglify_args/6).
% :-export(term_anglify_last/2).

term_anglify_last(Head,English):-compound(Head),
   functor(Head,F,A),A>1,
   not(ends_with_icase(F,"Fn")),not(starts_with_icase(F,"SKF-")),
   atom_codes(F,[C|_]),code_type(C,lower),
   Head=..[F|ARGS],
   term_anglify_args(Head,F,A,ARGS,prologSingleValued,English).

:- discontiguous(baseKB:mudTermAnglify/2).
prologDynamic(baseKB:mudTermAnglify/2).
baseKB:mudTermAnglify(A,B):-mudTermAnglify0(A,B).

mudTermAnglify0(A,B):-local_term_anglify(A,B).

mudTermAnglify0(Head,EnglishO):- compound(Head), 
   Head=..[F|ARGS],mpred_prop(F,_,Info),
   member(Info,[prologSingleValued,predArgMulti(_)]),   
   term_anglify_args(Head,F,1,ARGS,Info,English),eng_fully_expand(English,EnglishO),!.

mudTermAnglify0(verbFn(isa),[is,a]):-!.
mudTermAnglify0(verbFn(F),[is|UL]):-not(string_lower(F,F)),unCamelCase(F,U),atomics_to_string(UL,"_",U).
mudTermAnglify0(verbFn(F),[is,F]):-atom_concat(_,'ing',F).
mudTermAnglify0(verbFn(F),[F,is]).
% term_anglify(ftCallable(Term),String):-term_to_atom(Term,Atom),any_to_string(Atom,String).
mudTermAnglify0(determinerString(Obj,Text),[posNP(Obj),is,uses,ftString(Text),as,a,determiner]).
mudTermAnglify0(nameString(Obj,Text),[posNP(Obj),is,refered,to,as,ftString(Text)]).
mudTermAnglify0(mudTermAnglify(Term,Text),[ftCallable(Term),is,converted,to,english,using,ftCallable(Text)]).



term_anglify_args(Head,F,A,ARGS,predArgMulti(Which),English):- !,replace_nth_arglist(ARGS,Which,NewVar,NEWARGS),!,
   NewHead=..[F|NEWARGS], findall(NewVar,req1(NewHead),ListNewVar),list_to_set_safe(ListNewVar,SetNewVar),NewVar=ftListFn(SetNewVar),
   term_anglify_args(Head,F,A,NewHead,prologSingleValued,English).


/*

term_expansion((term_anglify_args(_Head,F,A,ARGS0,prologSingleValued,English):- add_arg_parts_of_speech(F,1,ARGS0,ARGS),verb_after_arg(F,A,After),
   insert_into(ARGS,After,verbFn(F),NEWARGS),
   eng_fully_expand(NEWARGS,English),X),O).

*/
term_anglify_args(_Head,F,A,ARGS0,prologSingleValued,English):- add_arg_parts_of_speech(F,1,ARGS0,ARGS),verb_after_arg(F,A,After),
               insert_into(ARGS,After,verbFn(F),NEWARGS),
               eng_fully_expand(NEWARGS,English),!.

unCamelCase(S,String):-any_to_string(S,Str),S\=Str,!,unCamelCase(Str,String),!.
unCamelCase("",""):-!.
unCamelCase(S,String):-sub_string(S,0,1,_,Char),sub_string(S,1,_,0,Rest),unCamelCase(Rest,RestString),string_lower(Char,NewChar),
(Char\=NewChar->atomics_to_string(['_',NewChar,RestString],String);atomics_to_string([Char,RestString],String)),!.

term_anglify_np_last(Obj,T,String):- local_term_anglify_np_last(Obj,T,String).

generatePhrase_local(Term,String):- on_x_debug(( eng_fully_expand(Term,EnglishM),!,
          % fmt('FR0=~q~n',[eng_fully_expand(Term,EnglishM)]),
          eng_fully_expand(EnglishM,EnglishG),fix_grammar(EnglishG,English) , join_for_string(English,String))),!.

local_grammar_correction([are,is,"here"],[are,"here"]).
local_grammar_correction(["you",is],["you",are]).
local_grammar_correction(["you",Verb,is],[your,Verb,is]).

local_grammar_correction([at,"right"],["right"]).
local_grammar_correction([tRoom,are],[tRoom,is]).
local_grammar_correction([in,tRegion,"here"],[is,"here"]).
local_grammar_correction([X,X],[X]):-member(X,[is,are]).
 
get_grammar_correction(C1,C2):-
   local_grammar_correction(W1,W2),to_word_list(W1,C1),to_word_list(W2,C2).


fix_grammar(String,WordsO):-to_word_list(String,Ws),fix_grammar_0(Ws,Words),(Words\=Ws->fix_grammar_0(Words,WordsO);Words=WordsO),!.

fix_grammar_0([],[]).
fix_grammar_0(EnglishG,English):-
   get_grammar_correction(Before,After),
   append_ci(Before,Rest,EnglishG),
   append_ci(After,Rest,EnglishNew),
   fix_grammar_0(EnglishNew,English),!.
fix_grammar_0([Carry|EnglishG],[Carry|English]):-
   fix_grammar_0(EnglishG,English),!.

join_for_string(English,EnglishS):-on_x_fail(( flatten([English],EnglishF),list_to_atomics_list(EnglishF,EnglishA),atomics_to_string(EnglishA," ",EnglishS))),!.
join_for_string(English,English).

list_to_atomics_list(L,AL):-list_to_atomics_list0(L,AL),forall(member(E,AL),must(atomic(E))).

list_to_atomics_list0(Var,A):-var(Var),!,any_to_string(Var,A),!.
list_to_atomics_list0([E|EnglishF],[A|EnglishA]):-
   any_to_string(E,A),
   list_to_atomics_list0(EnglishF,EnglishA),!.
list_to_atomics_list0([],[]):-!.


eng_fully_expand(I,O):-loop_check(transitive(eng_fully_expand_ilc,I,O),I=O).
eng_fully_expand_ilc(I,O):-copy_term(I,C),flatten([C],FC),eng_fully_expand_0(FC,O).

eng_fully_expand_0(FC,O):-catch(eng_fully_expand_1(FC,O),E,(dtrace,dmsg(exact_message(error_m(E,eng_fully_expand_1(FC,O)))),fail)),!.
%eng_fully_expand_0(FC,O):-catch((trace,eng_fully_expand_1(FC,O)),_,fail).

eng_fully_expand_1(A,B):-loop_check(eng_fully_expand_1_ilc(A,B),A=B).

eng_fully_expand_1_ilc(Var,Var):-var(Var),!.
eng_fully_expand_1_ilc([],[]):-!.
% eng_fully_expand_1(StringIsError,_Out):-string(StringIsError),!,dtrace,fail.
eng_fully_expand_1_ilc([T|TT],FTAO):-local_term_anglify_first([T|TT],TA),flatten([TA],FTA),eng_fully_expand_1(FTA,FTAO),!.
eng_fully_expand_1_ilc([T|Term],Out):-!,
   eng_fully_expand_2(T,E),
   eng_fully_expand_1(Term,English),
   flatten_append(E,English,Out),!.

eng_fully_expand_1_ilc(T,E):-loop_check(eng_fully_expand_2(T,E),(T=E)),!.

eng_fully_expand_2(Var,Var):-var(Var),!.
eng_fully_expand_2([],[]):-!.
eng_fully_expand_2(Var,Var):-not(compound(Var)),!.
eng_fully_expand_2(T,FTAO):-local_term_anglify_first(T,TA),flatten([TA],FTA),eng_fully_expand_1(FTA,FTAO).
eng_fully_expand_2(StringIsOK,StringIsOK):-string(StringIsOK),!.
eng_fully_expand_2([T|Term],Out):-!,
   eng_fully_expand_2(T,E),
   eng_fully_expand_1(Term,English),
   flatten_append(E,English,Out),!.
eng_fully_expand_2(Pred,Pred):-!.
eng_fully_expand_2(Pred,Out):-
   safe_univ(Pred,[F|ARGS]),
   eng_fully_expand_1_l(ARGS,NEWARGS),
   safe_univ(Out,[F|NEWARGS]),!.

eng_fully_expand_1_l([],[]):-!.
eng_fully_expand_1_l([T|Term],[E|English]):-!,
   eng_fully_expand_1(T,E),
   eng_fully_expand_1_l(Term,English).


best_nl_phrase(List,Sorted):-predsort(best_nl_phrase,List,Sorted).

% longest_string(?Order, @Term1, @Term2)
best_nl_phrase(Order,TStr1,TStr2):-
   any_to_string(TStr1,Str1),string_length(Str1,L1),
   any_to_string(TStr2,Str2),string_length(Str2,L2),
   compare(Order,L1-Str1,L2-Str2).

is_phrase_type(posNP).

prologBuiltin(local_term_anglify/2).
prologBuiltin(local_term_anglify_first/2).
prologBuiltin(local_term_anglify_last/2).
% prologBuiltin(local_term_anglify_np/2).
prologBuiltin(enter_term_anglify/2).
prologBuiltin(term_anglify_np_last/2).

% ========================================
% enter_term_anglify(MASK)
% ========================================

enter_term_anglify(X,Y):-var(X),copy_term(X,Y),!.
enter_term_anglify(X,Y):-findall(X-Y-Body,clause( mudTermAnglify(X,Y),Body),List),member(X-Y-Body,List),call(Body),!.
enter_term_anglify(X,Y):-findall(X-Y-Body,clause( term_anglify_np(X,Y),Body),List),member(X-Y-Body,List),call(Body),!.
enter_term_anglify(X,Y):-findall(X-Y-Body,clause( term_anglify_last(X,Y),Body),List),member(X-Y-Body,List),call(Body),!.
enter_term_anglify(X,Y):-findall(X-Y-Body,clause( term_anglify_np_last(X,Y),Body),List),member(X-Y-Body,List),call(Body),!.
enter_term_anglify(X,X).


local_term_anglify_first(T,TA):-compound(T),loop_check(local_term_anglify(T,TA)),!.
% local_term_anglify_first(FmtObj,String):-compound(FmtObj),functor(FmtObj,Fmt,_),corece(FmtObj,Fmt,String),!.
local_term_anglify_first(T,TA):-must(enter_term_anglify(T,TA)),!.



% :-export(local_term_anglify/2).
local_term_anglify(Var,O):- var(Var),!,O=[ftCallable(Var)].
% local_term_anglify([Var],O):- var(Var),!,O=[ftCallable([Var])].

local_term_anglify(posNP(P),English):- local_term_anglify_np(P,English).
local_term_anglify(noun_phrase(P),English):- local_term_anglify_np(P,English).

local_term_anglify(actNotice(Who,What),[posNP(Who),notices,What]).
local_term_anglify(fN(Region,tRegion),[(String)]):- call_u(nameString(Region,String)),!.
local_term_anglify(fN(Region,tRegion),[nameString1(String)]):- holds_t(nameString,Region,String),!.


local_term_anglify([P|L],English):-!, local_term_anglify(P,PE),local_term_anglify(L,LE),!,flatten_append(PE,LE,English),!.

local_term_anglify(HOLDS,English):-HOLDS=..[H,Pred,A|MORE],atom(Pred),is_holds_true(H),HOLDS2=..[Pred,A|MORE],!,local_term_anglify(HOLDS2,English).
local_term_anglify(HOLDS,[A,verbFn(Pred)|MORE]):-HOLDS=..[H,Pred,A|MORE],is_holds_true(H),!.
local_term_anglify(HOLDS,English):-HOLDS=..[H,Pred,A|MORE],is_holds_false(H),atom(Pred),HOLDS2=..[Pred,A|MORE],!,local_term_anglify(not(HOLDS2),English).
local_term_anglify(HOLDS,[false,that,A,verbFn(Pred)|MORE]):-HOLDS=..[H,Pred,A|MORE],is_holds_false(H),!.
local_term_anglify(not(HOLDS),[false,that,English]):-!,local_term_anglify(HOLDS,English).
local_term_anglify(notFound(FNum,F,Type),[no,FNum,TypeC,'-s',for,FC]):-copy_term(notFound(F,Type),notFound(FC,TypeC)),ignore(TypeC=tCol),ignore(FC='whatever').
local_term_anglify(NPO,String):-NPO=..[NP,Obj],is_phrase_type(NP),!,enter_term_anglify(fN(Obj,NP),String).

local_term_anglify(fN(Obj,argIsaFn(_PathName,_NumTwo)),String):- enter_term_anglify(Obj,String),!.
local_term_anglify(cmdresult(Cmd,Whatnot),["the","command","result","of",Cmd,"is",Whatnot]):-!.
local_term_anglify(string(Obj),[String]):-on_x_fail(any_to_string(Obj,StringUQ)),atomics_to_string(['"',StringUQ,'"'],"",String).
% enter_term_anglify(ftCallable(Obj),string(String)):-on_x_fail(any_to_string(Obj,StringUQ)),atomics_to_string(['(',StringUQ,')'],"",String).
local_term_anglify(mudAtLoc(Obj,LOC),String):-eng_fully_expand( [fN(Obj,posNP),is,at,fN(LOC,posNP)],String).
local_term_anglify(mudDescription(Obj,Term),[fN(Obj,posNP),"description","contains",":",string(Term)]).
local_term_anglify(fN(Obj,X),String):- locationToRegion(Obj,Region), Obj \= Region, enter_term_anglify(fN(Region,X),String),!.
% should not have searched nouns yet
local_term_anglify(fN(Obj,T),String):- local_term_anglify_np(Obj,T,String),!.

local_term_anglify(done(Obj,Term),[fN(Obj,posNP),did,:,Term]).
local_term_anglify(failed(Obj,Term),[fN(Obj,posNP),didnt,:,Term]).
local_term_anglify(do(Obj,Term),[fN(Obj,posNP),begun,:,Term]).


% almost all else failed
local_term_anglify(fN(Obj,T),String):- anglify_noun_known(Obj,T,String),!.

% totally all else failed
% %enter_term_anglify(ftCallable(Obj),String):- any_to_string(Obj,StringUQ),!,atomics_to_string(['',StringUQ,''],"",String),!.
% %enter_term_anglify(Obj,Obj):-!.


term_anglify_np(Obj,Hint,String):-local_term_anglify_np(Obj,Hint,String).

local_term_anglify_np(Obj,String):-isa(Obj,Isa),local_term_anglify_np(Obj,Isa,String),!.
local_term_anglify_np(Obj,String):-local_term_anglify_np(Obj,ftTerm,String).

% specific noun searching
local_term_anglify_np(Obj,Hint,String):- anglify_noun_known(Obj,Hint,String),!.
local_term_anglify_np(Obj,vtDirection,Obj):- !.
local_term_anglify_np(string(Obj),string,Obj):- !.
local_term_anglify_np(Obj,string,Obj):- !.

local_term_anglify_np_last(Obj,Hint,String):- anglify_noun_known(Obj,Hint,String),!.
local_term_anglify_np_last(Obj,FT,String):- ttExpressionType(FT),correctFormatType(change(assert,_),Obj,FT,String),!.
local_term_anglify_np_last(Obj,Type,[prolog(Obj)]):-ttExpressionType(Type),!.
local_term_anglify_np_last(Obj,Type,["the",Type,ftCallable(Obj)]):-!.
local_term_anglify_np_last(apathFn(Region,Dir),_,["a",fN(Dir,vtDirection),"-ern","way","from",fN(Region,posNP)]):-!.
local_term_anglify_np_last(Obj,Type,[prolog(Obj),fN,Type]):-!.
local_term_anglify_np_last(Obj,_,["the",noun,with,token,Obj]):-!.

:-ain_expanded(==>prologBuiltin(anglify_noun_known,3)).

% anglify_noun_known(Self,_Hint,["you"]):- current_agent(Self),!.
anglify_noun_known(Obj,FT,String):- ttExpressionType(FT),correctFormatType(change(assert,_),Obj,FT,String),!.
anglify_noun_known(StringO,_Hint, [StringO]).
anglify_noun_known(Obj,_Hint,["right","here"]):- current_agent(Self),mudAtLoc(Self,Obj),!.
anglify_noun_known(Obj,_Hint,["here"]):- current_agent(Self),req1(localityOfObject(Self,Obj)),!.
anglify_noun_known(Obj,_Hint,StringO):- findall(String,holds_t(nameString,Obj,String),List),List\=[],sort_by_strlen(List,[StringO|_]),!.
%anglify_noun_known(Obj,_Hint,String):-
%nameString(X,Y,_,_)

%:- expects_dialect(prolog).



detWithSpace(WithSpace,String):-ddeterminer0(String),atom_concat(String,' ',WithSpace).
detWithSpace(WithSpace,String):-ddeterminer1(String),atom_concat(String,' ',WithSpace).

:-export(determinerRemoved/3).
determinerRemoved(S0,Det,S):- fail,detWithSpace(WithSpace,String),string_concat(WithSpace,S,S0),string_lower(String,Det).

:-export(query_description/1).
query_description(mudDescription(I,S)):-  is_asserted(mudDescription(I,S)).
query_description(t(mudDescription,I,S)):- is_asserted(mudDescription(I,S));is_asserted(mudKeyword(I,S)).


:-export(remove_description/1).
remove_description(mudDescription(I,S)):- dmsg(trace_or_throw(remove_description(mudDescription(I,S)))).

:-export(add_description/1).
add_description(mudDescription(I,S)):-add_description(I,S).

:-export(add_description/2).
add_description(A,S0):-ainz(mudDescription(A,S0)),fail.
add_description(A,S0):- atomic(S0),string_concat('actPunchingSomething ',S,S0),!,add_description(A,S).
% add_description(A,S0):-determinerRemoved(S0,String,S),!,add_description(A,S),ainz(determinerString(A,String)).
add_description(A,S0):-
   any_to_string(S0,S),
   atomic_list_concat(Words,' ',S),
   atomic_list_concat(Sents,'.',S),!,
   length(Words,Ws),
   must(add_description(A,S,S0,Ws,Sents,Words)),!.

% mudBareHandDamage: 10d10+75
add_description(A,S,_S0,Ws,_Sents,_Words):- Ws<3,  
   atomic_list_concat([K,V],': ',S),!,add_description_kv(A,K,V).

add_description(A,S,_S0,Ws,_Sents,_Words):- Ws<3,
   atomic_list_concat([K,V],'=',S),!,add_description_kv(A,K,V).

% "NOBACKSTAB","ACT_STAY_ZONE","MEMORY"
add_description(A,_S,_S0,1,_,[Word]):-add_description_word(A,Word),!.

%actPunchingSomething ..
add_description(A,S,S0,Ws,Sents,['actPunchingSomething',B|C]):-add_description(A,S,S0,Ws,Sents,[B|C]).
add_description(A,S,S0,Ws,Sents,[Det,B|C]):-ddeterminer(Det,L),add_description(A,S,S0,Ws,Sents,[B|C]),ainz(determinerString(A,L)).
add_description(A,S,S0,Ws,_Sents,_Words):-Ws>3,is_here_String(S),text_to_string(S0,String),!,ainz(descriptionHere(A,String)).
add_description(A,_S,S0,_Ws,_Sents,_Words):- any_to_string(S0,String),ainz(mudDescription(A,String)).

is_here_String(S):- atomic_list_concat_safe([_,is,_,"here",_],S).
is_here_String(S):- atomic_list_concat_safe([_,"here"],S).
is_here_String(S):- atomic_list_concat_safe([_,is,"here",_],S).


ddeterminer1("A").
ddeterminer1("An").
ddeterminer1("The").

ddeterminer0("a").
ddeterminer0("an").
ddeterminer0("the").

ddeterminer(L,L):-ddeterminer0(L).
ddeterminer(U,L):-string_lower(U,L),U\=L,!,ddeterminer0(L).

add_description_word(A,Word):- string_upper(Word,Word),string_lower(Word,Flag),string_to_atom(Flag,Atom),atom_concat(flagged_,Atom,FAtom),ainz((isa(A,FAtom))).
add_description_word(A,Word):- string_lower(Word,Word),ainz((mudKeyword(A,Word))).
add_description_word(A,Word):- string_lower(Word,Lower),ainz((mudKeyword(A,Lower))).


add_description_kv(A,K,V):- atom_concat('actPunchingSomething ',Key,K),!,add_description_kv(A,Key,V).
add_description_kv(A,K,V):- atom_concat('+',Key,K),!,add_description_kv(A,Key,V).
add_description_kv(A,K,V):-atom_to_value(V,Term),C=..[K,A,Term],show_load_call(ainz(C)).

