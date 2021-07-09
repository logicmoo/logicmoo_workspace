
:-module(user).

:- prolog_load_context(directory,ThisDir),
   absolute_file_name('..',Dir,[relative_from(ThisDir),file_type(directory)]),
   asserta(user:file_search_path(logicmoo,   Dir)).

user:file_search_path(semlib,     logicmoo('candc/src/prolog/lib')).
user:file_search_path(boxer,      logicmoo('candc/src/prolog/boxer')).
user:file_search_path(knowledge,  logicmoo('candc/src/prolog/boxer/knowledge')).
user:file_search_path(lex,        logicmoo('candc/src/prolog/boxer/lex')).


:- thread_local sem/5.
:- multifile sem/5.
:- discontiguous sem/5.

:- use_module(boxer(input)).
:- use_module(library(lists)).
:- use_module(semlib(drs2fol)).
:- use_module(semlib(drs2tacitus)).
:- use_module(semlib(errors)).
:- use_module(semlib(options)).
:- use_module(boxer(alphaConversionDRT)).
:- use_module(boxer(ccg2drs)).
:- use_module(knowledge(relations),[nn/3]).
:- use_module(boxer(xdrs2xml)).
:- use_module(boxer(drs2fdrs)).
:- use_module(boxer(printDrs)).
:- use_module(boxer(betaConversionDRT)).
:- use_module(boxer(tuples)).
:- use_module(boxer(output)).
:- use_module(boxer(resolveDRT)).
:- use_module(boxer(slashes)).
:- use_module(boxer(transform)).
:- use_module(knowledge(punctuation)).




aVar(V):-var(V),!.
aVar('$VAR'(_)).

:- ensure_loaded(boxer(boxer)).


:- multifile     sem/3, id/2.
:- discontiguous sem/3, id/2.
:- thread_local       sem/3, id/2.

:-setDefaultOptions(boxer).

/* ========================================================================
   Map POS tags
*/

mappos('NN',  '-n').
mappos('NNS', '-n').
mappos('NNP', '-n').
mappos('NNPS','-n').

mappos('VB',  '-v').
mappos('VBD', '-v').
mappos('VBG', '-v').
mappos('VBN', '-v').
mappos('VBP', '-v').
mappos('VBZ', '-v').
mappos('EX',  '-v').

mappos('JJ',  '-a').
mappos('JJR', '-a').
mappos('JJS', '-a').
mappos('SO',  '-a').

mappos('IN',  '-p').
mappos('POS', '-p').

mappos('RB',  '-r').
mappos('RBR', '-r').
mappos('RBS', '-r').
mappos('RP',  '-r').


e2lf(Sent):-
   e2lf(Sent,LF),
   fmt(lf=LF).

e2lf(English,LF):-not(is_list(English)),atomic(English),!,
   convert_members([replace_periods_string_list,to_list_of_sents],English,Sents),
   e2lf(Sents,LF),!. 
e2lf([],done([])).
e2lf([sent(WList)|MORE],and(LF,MORELF)):-!, e2lf(sent(WList),LF),e2lf(MORE,MORELF).
e2lf(sent(WList),'FakedLFParsedNowFn'(WList)):-!.
e2lf(English,LF):- !,e2lf(sent(English),LF).

:- e2lf("You find yourself standing by the door of Captain Picard's quarters.
He isn't very fond of visitors, but you decide to stay and have a look around . 
You can see several different ancient artifacts on tables and small pedestals, and a large wooden wardrobe is facing south .
A comfortable looking recliner with a matching footrest sits beside the door, along with a bright reading lamp and end table .
Two large windows offer a great view of space . 
A small partition at the northern part of the room contains Picard's sleeping area.").

% :-set_env('CANDC','/devel/logicmoo/src_modules/candc').

:- multifile w/8, ccg/2, id/2.
:- discontiguous w/8, ccg/2, id/2.
:- thread_local w/8, ccg/2, id/2.


set_memory_opts:-
 % setOption(boxer,'--loaded',do),
 % setOption(boxer,'--help',dont),
 % setOption(boxer,'--stdin',dont),
 % setOption(boxer,'--toploop',false),
 % setOption(boxer,'--version',dont),
   setDefaultOptions(boxer),
   setOption(boxer,'--box',false),setOption(boxer,'--ccg',false),setOption(boxer,'--copula',true),setOption(boxer,'--elimeq',false),
   setOption(boxer,'--format',prolog),setOption(boxer,'--instantiate',false),setOption(boxer,'--integrate',false),
   setOption(boxer,'--modal',false),setOption(boxer,'--nn',false),setOption(boxer,'--output',user_output),
   setOption(boxer,'--plural',false),setOption(boxer,'--presup',max),setOption(boxer,'--resolve',false),
   setOption(boxer,'--roles',proto),   setOption(boxer,'--theory',drt), % was drt
   setOption(boxer,'--semantics',drs),setOption(boxer,'--tense',false),
   setOption(boxer,'--tokid',local),setOption(boxer,'--warnings',false),setOption(boxer,'--x',false),
   setOption(boxer,'--input','/tmp/pl_boxer/logicmoo_33609_0.ccg'),
    setOption(boxer,'--loaded',do),
/*

   setOption(boxer,'--integrate',true),
   setOption(boxer,'--loaded',do),
   setOption(boxer,'--roles',verbnet),
   setOption(boxer,'--resolve',true),
   setOption(boxer,'--semantics',tacitus), % was drs
  */ 
   !.


% soap_server --models models --models models/boxer --models models/questions --server 127.0.0.1:9066 --candc-printer prolog
% soap_client --url http://127.0.0.1:9066 --input working/input.txt --output working/input.ccg
text2ccg(Text,Lines):-!,
   tmp_file('boxer/logicmoo',X),atom_concat(X,'.txt',Txt),atom_concat(X,'.ccg',Ccg),atom_concat(X,'.log',Log),
   open(Txt,write, TextStream),
   format(TextStream,'~w . ~n',[Text]),
   close(TextStream),
   CMDARGS= [ 'logicmoocanc',Txt,Ccg,Log],
   dmsg(CMDARGS),
          process_create(path('bash'), CMDARGS,
             [ cwd('/devel/logicmoo/src_modules/candc'),
             % will use pipes later stdout(pipe(Out)), stdin(pipe(In)),
             process(Process)]),
       process_wait(Process,_Status),
       dmsg(make_ccg_output_to(Ccg)),
   open(Ccg,read,Stream,[encoding(utf8)]),
   ignore_1_line(Stream),ignore_1_line(Stream),  
   read_ccg(Stream,[],LinesIn),
   close(Stream),!,
   must(clean_ccg(LinesIn,Lines)),!.

ignore_ccg_term(':-'(_)).
ignore_ccg_term(end_of_file).

clean_ccg_skips(w,7).
clean_ccg_skips(_,0).

ignore_1_line(Out):- read_line_to_string(Out, String),dmsg(ignore_1_line(String)).

%:-debug.

fix_apos(A,AA):- atom(A), atomic_list_concat_safe([X,'-APOS-',Y],A),ground(X:Y),!,must(atomic_list_concat_safe([X,'\'',Y],AA)),!.
fix_apos([],[]):-!.
fix_apos([A|B],[AA|BB]):-!,
   must(fix_apos(A,AA)),
   must(fix_apos(B,BB)),!.
fix_apos(A,A):-!.

ccg2words(CCG,Words):-extract_matches(t/_,CCG,Words).

%ccg2text(CCG,Text):-ccg2words(CCG,Words),extract_matches([t/_,vrcall(A,R,arg(1,A,R))],Words,Text).
ccg2text(CCG,Text):-ccg2words(CCG,Words),extract_matches(vrcall(A,R,(functor(A,t,_),arg(2,A,R))),Words,Text).

pattern_matches(A,B,C):-loop_check_clauses(pattern_matches(A,B,C),fail).
pattern_matches(not(P),M,R):-!,not(pattern_matches(P,M,R)).
pattern_matches(or(P1,P2),M,R):-!,(pattern_matches(P1,M,R);pattern_matches(P2,M,R)).
pattern_matches(and(P1,P2),M,R):-!,(pattern_matches(P1,M,R),pattern_matches(P2,M,R)).
pattern_matches(ignore(P1),M,R):-!,ignore(pattern_matches(P1,M,R)).
pattern_matches(call(P),M,M):-!,call(P,M).
pattern_matches(callr(P),M,R):-!,call(P,M,R).
pattern_matches(vcall(M,P),M,M):-!,call(P).
pattern_matches(vrcall(M,R,P),M,R):-!,call(P).
pattern_matches([A|B],M,R2):-!,pattern_matches(A,M,R1),!,pattern_matches(B,R1,R2).
pattern_matches(F/A,M,M):-!,compound(M),functor(M,F,A),!.
pattern_matches(P,M,M):-!,not(not(M=P)),!.

extract_matches(A,B,[]):-loop_check_clauses(extract_matches(A,B,_),true).
extract_matches(_Pattern,Var,[]):-var(Var),!.
extract_matches(Pattern,A,[R|Rest]):-copy_term(Pattern,Copy),pattern_matches(Copy,A,R),!,extract_matches0(Pattern,A,Rest),!.
extract_matches(Pattern,A,Rest):-extract_matches0(Pattern,A,Rest),!.

extract_matches0(_Pattern,Var,[]):-var(Var),!.
extract_matches0(_Pattern,[],[]):-!.
extract_matches0(Pattern,[A|B],AABB):-!,
   copy_term(Pattern,Copy),
   extract_matches(Copy,A,AA),!,
   extract_matches(Pattern,B,BB),!,
   append(AA,BB,AABB),!.
extract_matches0(Pattern,A,BB):-compound(A),!,A=..[_|AL],extract_matches(Pattern,AL,BB),!.
extract_matches0(_Pattern,_A,[]):- !.


clean_ccg([],[]):-!.
clean_ccg(skip(N,[A|B]),BB):-length(Len,N),append(Len,Rest,[A|B]),must(fix_apos(Len,LenB)),clean_ccg(Rest,RestCleaned),append(LenB,RestCleaned,BB),!.
clean_ccg([A|B],BB):-
   ignore_ccg_term(A),!,
   must(clean_ccg(B,BB)),!.
clean_ccg([A|B],[AA|BB]):-!,
   must(clean_ccg(A,AA)),
   must(clean_ccg(B,BB)),!.
clean_ccg(A,A):- number(A),!.
clean_ccg(A,AA):- compound(A),A=..[F|AL],clean_ccg_skips(F,N),!,clean_ccg(skip(N,AL),BB),AA=..[F|BB].
clean_ccg(A,A):- not(atom(A)),!.
clean_ccg(A,A):- !. % disable the 1.0.0 version 
clean_ccg(A,AA):- catch(must(atom2ccg(A,AA)),E,(dmsg(E),A=AA)).

:- use_module(semlib(drs2tacitus)).


% 'S[dcl]'
atom2ccg(A,A):-mappos(A,_),!.
atom2ccg(A,AA):-atom_chars(A,Chars),not((member(C,['\\','[','/','(']),member(C,Chars))),!,string_lower(A,S),atom_string(AA,S),!.
atom2ccg(A,XX:YY):-atomic_list_concat_safe([X,'[',Y,']'],A),atom2ccg(X,XX),atom2ccg(Y,YY).
atom2ccg(A,XX/YY):-atomic_list_concat_safe([X,'/',Y],A),atom2ccg(X,XX),atom2ccg(Y,YY).
atom2ccg(A,'\\'(XX,YY)):-atomic_list_concat_safe([X,'\\',Y],A),atom2ccg(X,XX),atom2ccg(Y,YY).
atom2ccg(A,XX):-atomic_list_concat_safe(['(',X,')'],A),atom2ccg(X,XX).
% atom2ccg(A,A).


output:boxer_assertz(_Stream,Sem,_Opts):-
   assert_sem(Sem).

get_current_drs(DRS):-current_drs(DRS),!.
get_current_drs(DRS):-
  gensym(drsNum,DRS),
  asserta(current_drs(DRS)),!.



unneeded_drsdata(DRS,Data):-redundant_drsdata(DRS,Data),!.
unneeded_drsdata(DRS,Data):-shown_drsdata(DRS,Data),!.
unneeded_drsdata(DRS,Data):-used_drsdata(DRS,Data),!.

:-thread_local current_drs/1.
:-thread_local shown_drsdata/2.
:-thread_local redundant_drsdata/2.
:-thread_local used_drsdata/2.

assert_sem(Sem):- output:assert(box_sem(Sem)). % ,dmsg(Sem).

text2drsWhatnot(Text):-  with_dmsg(skip_dmsg,text2ccg(Text,Out)),
   must(ccg2prolog(Out,Goal)),!,
   assert_drs(Goal).


lterm_to_lst(Props,PropsL):-safe_univ(Props,PropsL),!.

merge_term_lists([E1|L1],[E2|L2],[E1|L12]):-atom(E1),E1==E2,!,merge_term_lists(L1,L2,L12).
merge_term_lists(L1,L2,L12):-append(L1,L2,L12).

append_terms(Prep,Props,Target):-lterm_to_lst(Prep,PrepL),lterm_to_lst(Props,PropsL),merge_term_lists(PrepL,PropsL,TargetL),safe_univ(Target,TargetL),!.

assert_redundant(P):-get_current_drs(DRS), asserta_if_new(redundant_drsdata(DRS,P)).


pos2ColFn_pred(v,'Event').
pos2ColFn_pred(a,'Attribute').
pos2ColFn_pred(n,'Individual').
pos2ColFn_pred(r,'Pos2ColFn'('Adverb')).
pos2ColFn_pred(Pos,'Pos2ColFn'(Pos)).

assert_drs_holds([F,V,N]):-assert_drs_holds(F,V,N).
assert_drs_holds(HOLDS):- functor(HOLDS,F,2),HOLDS=..[F,V,N],!,assert_drs_holds(F,V,N).
assert_drs_holds(HOLDS):- assert_drsdata(HOLDS).
assert_drs_holds(P,V,N):- once(must(ground(assert_drs_holds(P,V,N)))),fail.
%assert_drs_holds(Of,Var,ObliqueObj):-nonvar(Of),assert_drs(rel(Of,Var,ObliqueObj)).
assert_drs_holds(P,V,'EachOfFn'(PropList)):- forall(member(R,PropList),assert_drs_holds(P,V,R)).
assert_drs_holds(P,V,[N]):- assert_drs_holds(P,V,N).
assert_drs_holds(P,[V],N):- assert_drs_holds(P,V,N).
assert_drs_holds([P],V,N):- assert_drs_holds(P,V,N).
assert_drs_holds(_,[],_).
assert_drs_holds(_,_,[]).
assert_drs_holds(tagV,_, 0).
assert_drs_holds(namex,_, 'O').
assert_drs_holds(tagV,A, B):- not((member('$VAR'(_),[A,B]))),!.
assert_drs_holds(P,V,N):-assert_drsdata(true(holds(P,V,N))).
% ------------------------------------------------------------------
% assert_drs/N
% ------------------------------------------------------------------
assert_drs(A):-dmsg(assert_drs(A)).

assert_drs(E):- not(ground(E)),numbervars(E),must(ground(E)),assert_drs(E).
assert_drs(E):- not(ground(E)),trace,safe_numbervars(E,EE),trace,!,must(ground(EE)),assert_drs(EE).

assert_drs(E):- loop_check_clauses(assert_drs(E),assert_drsdata(E)).
assert_drs(E):- aVar(E),!. % , assert_drsdata((erorr_var(E))).
assert_drs([]).
assert_drs(E):- not(compound(E)),!,trace,assert_drsdata((erorr_nc(E))).
assert_drs(true(E)):-assert_drs_holds(E).
assert_drs(later(_,_)):-!.
assert_drs(N:[tok:R|MORE]):-!, assert_drs( word(N:[tok:R|MORE])).

assert_drs(words(EL)):-!,forall(member(I,EL),must(assert_drs(word(I)))).
assert_drs(holds([S|Sent])):-atom(S),!,safe_univ(Assertion,[S|Sent]),!,assert_drs(Assertion).
assert_drs(holds([S|Sent])):- safe_univ(Assertion,[holds,S|Sent]),!,assert_drs(Assertion).
assert_drs(Marker:Sent):- !, compound(Sent),Sent=..[_,Var|Args],
   assert_redundant(Marker:Sent),
   assert_drs(tagV(Marker,'EachOfFn'([Var|Args]))),
   assert_drs(Sent).

assert_drs(pred(Var, Male, Pos, _N02)):- !, assert_drs(isaVarPos(Var,Male,Pos)).
%forall(member(E,[Var,string(Text),Nam1,Nam2]),assert_drs_holds(tagV(Marker,E))), assert_drs(isaVarPos(Var,u,Nam1)),assert_drs_holds(named(Var, Text)).
% assert_drs(named(Var, Text, Nam1, Nam2)):-  assert_drs_holds(isa(Var,Male)).
% [2006]:pred(S, 'visitors,', n, 0)
% [1010]:named(I, 'picard\'s', nam, nam)

assert_drs(prop(Var,Value)):- assert_drs_holds(isa(Var, 'Proposition')),expand_drs_args(Value,NV),assert_drs(NV).
assert_drs(Sent):- 
   Sent=..[F,Var|Args],
   member(F,[pred,named]),
   must((
   Args=[Col,Pos|_],
   assert_drs_holds(F,Var,Col),
   once(pos2ColFn_pred(Pos,POSCol)),
   assert_drs_holds(isa(Var,POSCol)),
   assert_drs_holds(incol(Var,Col)),
   assert_redundant(Sent))).

assert_drs(vars(EL)):-!,forall(member(I,EL),must(assert_drs(aevar(I)))).
assert_drs(aevar([]:'$VAR'(_))):-!. % assert_drs(true(all(I))),!.
assert_drs(aevar([Tag]:I)):-assert_drs(true(exst(I))),assert_drs(tagV(Tag,I)).

% rel(R, B, theme, 0),
% [1010]:prop(F,Var..] 
assert_drs(rel(Var, ObliqueObj, Of, _N02)):-assert_drs_holds(Of,Var,ObliqueObj).
assert_drs(word(Marker:PropList)):- nonvar(Marker), forall(member(N:V,PropList),assert_drs_holds(N,Marker,V)).
assert_drs(tagV(V,Thing)):-!,assert_drs_holds(tagV,V,Thing).


assert_drs(isaVarPos(Var,Male,Pos)):-
  assert_drs_holds(isa(Var,Male)), 
  once(pos2ColFn_pred(Pos,POSCol)),
  assert_drs_holds(isa(Var,POSCol)).


assert_drs(t(POS,Recliner,Rule,PropList,R2004)):-assert_drs(word(R2004:PropList)),assert_drs_holds(tagV,R2004,V),arg(1,Rule,V),isaVarPos(V,Recliner,POS),assert_drs(Rule).

assert_drs(E):- is_list(E),!,assert_drs_list(E).
assert_drs(lam(V,E)):-aVar(V),
 not(aVar(E)),assert_drs(E).
assert_drs(ba(E)):-!,assert_drs(E).
assert_drs(drs(Vars,Formulas)):-!, assert_drs(vars(Vars)),expand_drs_args(Formulas,EEL),assert_drsdata(EEL).
assert_drs(xdrs(Vars,Formulas)):-!,assert_drs(words(Vars)),expand_drs_args(Formulas,EEL),assert_drsdata(EEL).
assert_drs(der(_Number,Formulas)):-!,expand_drs_args(Formulas,EEL),assert_drsdata(EEL).
assert_drs(E):- functor(E,F,A),E=..[F|EL],assert_drs(F/A,E,EL),!.


:-style_check(-singleton).

assert_drs(FA,E,   EL):- member(FA,[ patient/_,theme/_,agent/2, exst/1, true/_, rel/3, all/_,  eq/2,  (=)/2, holds/_, isa/2 ]),assert_drsdata(E).
assert_drs(F/A,E,   EL):- nn(_,_,F),assert_drsdata(E).

assert_drs(FA,[]:Sent,EL):- trace,assert_drs(Sent).
assert_drs(FA,E,[_,EL]):-member(FA,[':'/2]),!,assert_drs(EL).
assert_drs(FA,E,   EL):-member(FA,[merge/2]), expand_drs_args(EL,EEL),EE=..[and|EEL],!,assert_drsdata(EE).
assert_drs(FA,E,[[E|L]]):-member(FA,[word/1]),!,assert_drs(tag(E,L)).
assert_drs(FA,E,   EL):-member(FA,[word/_]),assert_drsdata(E).
assert_drs(FA,E,   EL):-member(FA,[tuple/6]),assert_drs(later(FA,E)).
assert_drs(FA,E,   EL):-member(FA,[xdrs/2]),assert_drs_list(EL).
assert_drs(F/A,E,   EL):- assert_drsdata(E),!.
assert_drs(F/A,E,   ['$VAR'(_)|EL]):- not(member('$VAR'(_),EL)),!,expand_drs_args(EL,EEL),assert_drsdata(EEL).
assert_drs(F/A,E,   EL):- member('$VAR'(_),EL),!,assert_drsdata(E),!.
assert_drs(F/A,E,   EL):- expand_drs_args(EL,EEL),assert_drsdata(EEL).

% assert_drs(Target):- doall(extract_matches(example2,Target,_)).

assert_drs_list([Out|List]):-!,forall(member(E,[Out|List]),must(assert_drs(E))).

:-style_check(+singleton).


drs_filter(later(_,_)).

% ------------------------------------------------------------------
% assert_drsdata/1
% ------------------------------------------------------------------

% assert_drsdata(E):- aVar(E),!,assert_drsdata((erorr_var0(E))).
assert_drsdata(E):- not(compound(E)),!,assert_drsdata((erorr_nc0(E))).
assert_drsdata(E):- assert_drsdata_0(E),!.

assert_drsdata_0(true(E)):-!,assert_drsdata(E).
assert_drsdata_0(Filter):-drs_filter(Filter),!.
assert_drsdata_0(E):- get_current_drs(DRS), unneeded_drsdata(DRS,E),!.
assert_drsdata_0(E):- E=..[holds,P|ARGS],atom(P),EE=..[P|ARGS],!,assert_drsdata_0(EE).
assert_drsdata_0(E):-
   get_current_drs(DRS), 
   asserta(shown_drsdata(DRS,E)),
   assert_if_new(used_drsdata(DRS,E)),
   assert_if_new(used_drsdata(DRS,E)),

   functor(E,F,A),
   portray_clause(((pred(F,A):-E))),!.


ignore_drs_term([]:[]).
ignore_drs_term(lam(R10, R10)).

expand_drs_args(V,V):- (aVar(V);not(compound(V))),!.
expand_drs_args([],[]):-!.
expand_drs_args(skip(N,[A|B]),BB):-length(Len,N),append(Len,Rest,[A|B]),expand_drs_args(Rest,RestCleaned),append(Len,RestCleaned,BB),!.
expand_drs_args([A|B],BB):-
   ignore_drs_term(A),!,
   must(expand_drs_args(B,BB)),!.
expand_drs_args([A|B],AABB):-!,expand_drs_args_real(true,[A|B],AABB).

expand_drs_args(A,AA):- A=..[F|_AL],once(expand_drs_args_real(true,A,BB)), (functor(BB,F,_) -> AA=BB ; AA=..[F|BB]).

expand_drs_args(List,BB):-expand_drs_args(true,List,BB).

expand_drs_args_real(Call,List,BB):-
  gensym(drsNum,DRS),
  asserta(current_drs(DRS)),
 call_cleanup(
   ((Call,assert_drs_here(List)),findall(G,retract(used_drsdata(DRS,G)),BB)),
   retractall_drs(DRS)),!.


assert_drs_here(A):- not(compound(A)),!.
assert_drs_here('$VAR'(_)).
assert_drs_here(A):- is_list(A),!,forall(member(E,A),assert_drs(E)).
assert_drs_here(A):- assert_drs(A).



gather_assert_dbody(A,BB):-gather_assert_dbody(true,A,BB).



retractall_drs(DRS):-
  retractall(current_drs(DRS)),
  retractall(redundant_drsdata(DRS,_)),
  retractall(used_drsdata(DRS,_)),
  retractall(shown_drsdata(DRS,_)).


% ------------------------------------------------------------------
% ccg2prolog/N
% ------------------------------------------------------------------

ccg2prolog(Out,[id=Id|Goal]):-
 ignore(Id=local),
 with_output_to(string(_),
   ccg2stream(Id,Out)),
 findall(X,output:retract(box_sem(X)),Goal).

ccg2stream(IdTok,CCGList):-
   set_memory_opts,
   assert_ccg(CCGList),!,
   %  loadKnowledge,
     % this checkInputType,  should do
         retractall(input:inputtype(_)),
         assert(input:inputtype(ccg)),
         % vs sem(_,_,_,_,_)
 %  ignore((setOption(boxer,'--semantics',tacitus),ccg2stream2(IdTok,CCGList))),
 %  ignore((setOption(boxer,'--semantics',drs),ccg2stream2(IdTok,CCGList))),   
 %  ignore((setOption(boxer,'--semantics',der),ccg2stream2(IdTok,CCGList))),
   ignore((setOption(boxer,'--semantics',drg),ccg2stream2(IdTok,CCGList))).

ccg2stream2(IdTok,CCGList):-
    current_output(Stream), %  = user_output,
    %  version(Version),
    %  printHeader(Stream,Version,Command,Options),
    % format(Stream,'~n:- multifile     sem/3, id/2.',[]),
      setOption(boxer,'--integrate',true),
      setOption(boxer,'--loaded',do),
      initEval,
     % this  identifyIDs(List),
     setof(Id,X^member(ccg(Id,X),CCGList),List),sort(List,Sorted),
     %% buildList(List,1,Stream),      
      ignore(Index=1),
      contextParameters(IdTok,[],Context),      
      debugOnError(must(ccg2drs(Sorted,XDRS,Context))),
      my_outputSem(Stream,IdTok,Index,XDRS), !.
    %  printFooter(Stream),
       %!,
      %reportEval.


my_outputSem(Stream,Id,Index,XDRS0):-
%   eqDrs(XDRS0,XDRS1),
   XDRS0=XDRS1,
   printSem(Stream,Id,Index,XDRS1), !.
%   nl(Stream).



assert_ccg([]).
assert_ccg([Out|List]):-!,forall(member(E,[Out|List]),must(assert_ccg(E))).
assert_ccg(E):-assert_if_new(input:E),!. % ,ccg2words(E,Ws),ccg2text(E,Ts), dmsg(assert_ccg(E)),dmsg(assert_text(Ts)),dmsg(assert_words(Ws)).

read_ccg(Stream,TermsInOut,TermsInOut):-at_end_of_stream(Stream),!.
read_ccg(Stream,TermsIn,TermsOut):-
   catch((read_term(Stream,Term,[double_quotes(string)])),Error,(Term=was_read_error(Error))),
   append(TermsIn,[Term],TermsM),!,read_ccg(Stream,TermsM,TermsOut),!.

old_read_ccg(Stream,TermsIn,TermsOut):-
   catch((read_term(Stream,Term,[character_escapes(false),double_quotes(string),variable_names(_Vars)])),Error,(Term=was_read_error(Error))),
   append(TermsIn,[Term],TermsM),!,read_ccg(Stream,TermsM,TermsOut),!.




:- text2drsWhatnot("You find yourself standing by the door of Captain Picard's quarters.
He isn't very fond of visitors, but you decide to stay and have a look around . 
You can see several different ancient artifacts on tables and small pedestals, and a large wooden wardrobe is facing south .
A comfortable looking recliner with a matching footrest sits beside the door, along with a bright reading lamp and end table .
Two large windows offer a great view of space . 
A small partition at the northern part of the room contains Picard's sleeping area").

end_of_file.

:- text2drsWhatnot("You find yourself standing by the door of Captain Picard's quarters.
He isn't very fond of visitors, but you decide to stay and have a look around .").

% :- text2drsWhatnot("You find yourself standing by the door of Captain Picard's quarters.").
:- text2drsWhatnot("You're in the middle of Data's quarters.  Some easils and paintings have been left scattered around the southern part of the room, 
 while a huge computer screen showing a cross section of the Enterprise covers the entire northern wall.  In front of the screen is a large desk, 
    which is covered in computer controls.  You can't see a bed in this room, but you figure it's because Data doesn't sleep.").





:-atom2ccg('(S[to]\\NP)/(S[b]\\NP)',XX).
:-atom2ccg('(S[X]\\NP)\\(S[X]\\NP)',XX).

doc_only_out_eq([
ccg(1,
 rp('S[dcl]',
  ba('S[dcl]',
   lex('N','NP',
    fa('N',
     lf(1,1,'N/N'),
     fa('N',
      lf(1,2,'N/N'),
      lf(1,3,'N')))),
   fa('S[dcl]\\NP',
    lf(1,4,'(S[dcl]\\NP)/NP'),
    fa('NP[nb]', 
     lf(1,5,'NP[nb]/N'),
     lf(1,6,'N')))),
  lf(1,7,'.'))),

w(1, 1, 'Captain', 'Captain', 'NNP', 'I-NP', 'O', 'N/N'),
w(1, 2, 'Picard\'s', 'Picard\'s', 'NNP', 'I-NP', 'O', 'N/N'),
w(1, 3, 'puppy', 'puppy', 'NN', 'I-NP', 'O', 'N'),
w(1, 4, 'is', 'be', 'VBZ', 'I-VP', 'O', '(S[dcl]\\NP)/NP'),
w(1, 5, 'the', 'the', 'DT', 'I-NP', 'O', 'NP[nb]/N'),
w(1, 6, 'king', 'king', 'NN', 'I-NP', 'O', 'N'),
w(1, 7, '.', '.', '.', 'O', 'O', '.')
],

[
ccg(1,
 rp(s:dcl,
  ba(s:dcl,
   lx(np, n,
    fa(n,
     t(n/n, 'Captain', 'Captain', 'NNP', 'I-NP', 'O'),
     fa(n,
      t(n/n, 'Picard\'s', 'Picard\'s', 'NNP', 'I-NP', 'O'),
      t(n, 'puppy', 'puppy', 'NN', 'I-NP', 'O')))),
   fa(s:dcl\np,
    t((s:dcl\np)/np, 'is', 'be', 'VBZ', 'I-VP', 'O'),
    fa(np:nb,
     t(np:nb/n, 'the', 'the', 'DT', 'I-NP', 'O'),
     t(n, 'king', 'king', 'NN', 'I-NP', 'O')))),
  t(period, '.', '.', '.', 'O', 'O')))
]).

:-must(clean_ccg([ (:-multifile w/8,ccg/2,id/2), (:-discontiguous w/8,ccg/2,id/2), (:-dynamic w/8,ccg/2,id/2),ccg(1,ba('S[dcl]',lf(1,1,'NP'),fa('S[dcl]\\NP',fa('(S[dcl]\\NP)/(S[ng]\\NP)',lf(1,2,'((S[dcl]\\NP)/(S[ng]\\NP))/NP'),lf(1,3,'NP')),ba('S[ng]\\NP',lf(1,4,'S[ng]\\NP'),fa('(S[X]\\NP)\\(S[X]\\NP)',lf(1,5,'((S[X]\\NP)\\(S[X]\\NP))/NP'),ba('NP',fa('NP[nb]',lf(1,6,'NP[nb]/N'),lf(1,7,'N')),fa('NP\\NP',lf(1,8,'(NP\\NP)/NP'),lex('N','NP',fa('N',lf(1,9,'N/N'),fa('N',lf(1,10,'N/N'),lf(1,11,'N'))))))))))),w(1,1,'You',you,'PRP','I-NP','O','NP'),w(1,2,find,find,'VBP','I-VP','O','((S[dcl]\\NP)/(S[ng]\\NP))/NP'),w(1,3,yourself,yourself,'PRP','I-NP','O','NP'),w(1,4,standing,stand,'VBG','I-VP','O','S[ng]\\NP'),w(1,5,by,by,'IN','I-PP','O','((S[X]\\NP)\\(S[X]\\NP))/NP'),w(1,6,the,the,'DT','I-NP','O','NP[nb]/N'),w(1,7,door,door,'NN','I-NP','O','N'),w(1,8,of,of,'IN','I-PP','O','(NP\\NP)/NP'),w(1,9,'Captain','Captain','NNP','I-NP','O','N/N'),w(1,10,'Picard-APOS-s','Picard-APOS-s','NNP','I-NP','O','N/N'),w(1,11,'quarters.','quarters.','NN','I-NP','O','N'),ccg(2,rp('S[dcl]',ba('S[dcl]',ba('S[dcl]',lf(2,1,'NP'),fa('S[dcl]\\NP',lf(2,2,'(S[dcl]\\NP)/(S[adj]\\NP)'),fa('S[adj]\\NP',lf(2,3,'(S[adj]\\NP)/(S[adj]\\NP)'),fa('S[adj]\\NP',lf(2,4,'(S[adj]\\NP)/PP'),fa('PP',lf(2,5,'PP/NP'),lex('N','NP',lf(2,6,'N'))))))),conj(conj,'S[dcl]','S[dcl]\\S[dcl]',lf(2,7,conj),fa('S[dcl]',tr('S[X]/(S[X]\\NP)',lf(2,8,'NP')),fa('S[dcl]\\NP',lf(2,9,'(S[dcl]\\NP)/(S[to]\\NP)'),fa('S[to]\\NP',lf(2,10,'(S[to]\\NP)/(S[b]\\NP)'),ba('S[b]\\NP',lf(2,11,'S[b]\\NP'),conj(conj,'S[b]\\NP','(S[b]\\NP)\\(S[b]\\NP)',lf(2,12,conj),fa('S[b]\\NP',lf(2,13,'(S[b]\\NP)/NP'),fa('NP[nb]',lf(2,14,'NP[nb]/N'),ba('N',lf(2,15,'N'),lex('S[adj]\\NP','N\\N',lf(2,16,'S[adj]\\NP')))))))))))),lf(2,17,'.'))),w(2,1,'He',he,'PRP','I-NP','O','NP'),w(2,2,'isn-APOS-t','isn-APOS-t','VBD','I-VP','O','(S[dcl]\\NP)/(S[adj]\\NP)'),w(2,3,very,very,'RB','I-ADJP','O','(S[adj]\\NP)/(S[adj]\\NP)'),w(2,4,fond,fond,'JJ','I-ADJP','O','(S[adj]\\NP)/PP'),w(2,5,of,of,'IN','I-PP','O','PP/NP'),w(2,6,'visitors,','visitors,','NN','I-NP','O','N'),w(2,7,but,but,'CC','O','O',conj),w(2,8,you,you,'PRP','I-NP','O','NP'),w(2,9,decide,decide,'VBP','I-VP','O','(S[dcl]\\NP)/(S[to]\\NP)'),w(2,10,to,to,'TO','I-VP','O','(S[to]\\NP)/(S[b]\\NP)'),w(2,11,stay,stay,'VB','I-VP','O','S[b]\\NP'),w(2,12,and,and,'CC','I-VP','O',conj),w(2,13,have,have,'VB','I-VP','O','(S[b]\\NP)/NP'),w(2,14,a,a,'DT','I-NP','O','NP[nb]/N'),w(2,15,look,look,'NN','I-NP','O','N'),w(2,16,around,around,'RB','I-ADVP','O','S[adj]\\NP'),w(2,17,'.','.','.','O','O','.'),ccg(3,ba('S[dcl]',fa('S[dcl]',tr('S[X]/(S[X]\\NP)',lf(3,1,'NP')),fa('S[dcl]\\NP',lf(3,2,'(S[dcl]\\NP)/(S[b]\\NP)'),fa('S[b]\\NP',lf(3,3,'(S[b]\\NP)/NP'),ba('NP',lex('N','NP',fa('N',lf(3,4,'N/N'),fa('N',lf(3,5,'N/N'),fa('N',lf(3,6,'N/N'),lf(3,7,'N'))))),fa('NP\\NP',lf(3,8,'(NP\\NP)/NP'),ba('NP',lex('N','NP',lf(3,9,'N')),conj(conj,'NP','NP\\NP',lf(3,10,conj),lex('N','NP',fa('N',lf(3,11,'N/N'),lf(3,12,'N')))))))))),conj(conj,'S[dcl]','S[dcl]\\S[dcl]',lf(3,13,conj),rp('S[dcl]',ba('S[dcl]',fa('NP[nb]',lf(3,14,'NP[nb]/N'),fa('N',lf(3,15,'N/N'),fa('N',lf(3,16,'N/N'),lf(3,17,'N')))),fa('S[dcl]\\NP',lf(3,18,'(S[dcl]\\NP)/(S[ng]\\NP)'),fa('S[ng]\\NP',lf(3,19,'(S[ng]\\NP)/NP'),lex('N','NP',lf(3,20,'N'))))),lf(3,21,'.'))))),w(3,1,'You',you,'PRP','I-NP','O','NP'),w(3,2,can,can,'MD','I-VP','O','(S[dcl]\\NP)/(S[b]\\NP)'),w(3,3,see,see,'VB','I-VP','O','(S[b]\\NP)/NP'),w(3,4,several,several,'JJ','I-NP','O','N/N'),w(3,5,different,different,'JJ','I-NP','O','N/N'),w(3,6,ancient,ancient,'JJ','I-NP','O','N/N'),w(3,7,artifacts,artifact,'NNS','I-NP','O','N'),w(3,8,on,on,'IN','I-PP','O','(NP\\NP)/NP'),w(3,9,tables,table,'NNS','I-NP','O','N'),w(3,10,and,and,'CC','O','O',conj),w(3,11,small,small,'JJ','I-NP','O','N/N'),w(3,12,'pedestals,','pedestals,','NN','I-NP','O','N'),w(3,13,and,and,'CC','O','O',conj),w(3,14,a,a,'DT','I-NP','O','NP[nb]/N'),w(3,15,large,large,'JJ','I-NP','O','N/N'),w(3,16,wooden,wooden,'JJ','I-NP','O','N/N'),w(3,17,wardrobe,wardrobe,'NN','I-NP','O','N'),w(3,18,is,be,'VBZ','I-VP','O','(S[dcl]\\NP)/(S[ng]\\NP)'),w(3,19,facing,face,'VBG','I-VP','O','(S[ng]\\NP)/NP'),w(3,20,south,south,'RB','I-ADVP','O','N'),w(3,21,'.','.','.','O','O','.'),ccg(4,rp('S[dcl]',ba('S[dcl]',ba('NP',fa('NP[nb]',lf(4,1,'NP[nb]/N'),fa('N',lf(4,2,'N/N'),fa('N',lf(4,3,'N/N'),lf(4,4,'N')))),fa('NP\\NP',lf(4,5,'(NP\\NP)/NP'),fa('NP[nb]',lf(4,6,'NP[nb]/N'),fa('N',lf(4,7,'N/N'),lf(4,8,'N'))))),ba('S[dcl]\\NP',ba('S[dcl]\\NP',lf(4,9,'S[dcl]\\NP'),fa('(S[X]\\NP)\\(S[X]\\NP)',lf(4,10,'((S[X]\\NP)\\(S[X]\\NP))/NP'),fa('NP[nb]',lf(4,11,'NP[nb]/N'),lf(4,12,'N')))),fa('(S[X]\\NP)\\(S[X]\\NP)',lf(4,13,'((S[X]\\NP)\\(S[X]\\NP))/PP'),fa('PP',lf(4,14,'PP/NP'),fa('NP[nb]',lf(4,15,'NP[nb]/N'),fa('N',lf(4,16,'N/N'),fa('N',lf(4,17,'N/N'),ba('N',lf(4,18,'N'),conj(conj,'N','N\\N',lf(4,19,conj),fa('N',lf(4,20,'N/N'),lf(4,21,'N'))))))))))),lf(4,22,'.'))),w(4,1,'A',a,'DT','I-NP','O','NP[nb]/N'),w(4,2,comfortable,comfortable,'JJ','I-NP','O','N/N'),w(4,3,looking,look,'VBG','I-VP','O','N/N'),w(4,4,recliner,recliner,'NN','I-NP','O','N'),w(4,5,with,with,'IN','I-PP','O','(NP\\NP)/NP'),w(4,6,a,a,'DT','I-NP','O','NP[nb]/N'),w(4,7,matching,matching,'JJ','I-NP','O','N/N'),w(4,8,footrest,footrest,'NN','I-NP','O','N'),w(4,9,sits,sit,'VBZ','I-VP','O','S[dcl]\\NP'),w(4,10,beside,beside,'IN','I-PP','O','((S[X]\\NP)\\(S[X]\\NP))/NP'),w(4,11,the,the,'DT','I-NP','O','NP[nb]/N'),w(4,12,'door,','door,','NN','I-NP','O','N'),w(4,13,along,along,'IN','I-PP','O','((S[X]\\NP)\\(S[X]\\NP))/PP'),w(4,14,with,with,'IN','B-PP','O','PP/NP'),w(4,15,a,a,'DT','I-NP','O','NP[nb]/N'),w(4,16,bright,bright,'JJ','I-NP','O','N/N'),w(4,17,reading,reading,'NN','I-NP','O','N/N'),w(4,18,lamp,lamp,'NN','I-NP','O','N'),w(4,19,and,and,'CC','I-NP','O',conj),w(4,20,end,end,'NN','I-NP','O','N/N'),w(4,21,table,table,'NN','I-NP','O','N'),w(4,22,'.','.','.','O','O','.'),ccg(5,rp('S[dcl]',ba('S[dcl]',lex('N','NP',fa('N',lf(5,1,'N/N'),fa('N',lf(5,2,'N/N'),lf(5,3,'N')))),fa('S[dcl]\\NP',lf(5,4,'(S[dcl]\\NP)/NP'),ba('NP',fa('NP[nb]',lf(5,5,'NP[nb]/N'),fa('N',lf(5,6,'N/N'),lf(5,7,'N'))),fa('NP\\NP',lf(5,8,'(NP\\NP)/NP'),lex('N','NP',lf(5,9,'N')))))),lf(5,10,'.'))),w(5,1,'Two',two,'CD','I-NP','O','N/N'),w(5,2,large,large,'JJ','I-NP','O','N/N'),w(5,3,windows,window,'NNS','I-NP','O','N'),w(5,4,offer,offer,'VBP','I-VP','O','(S[dcl]\\NP)/NP'),w(5,5,a,a,'DT','I-NP','O','NP[nb]/N'),w(5,6,great,great,'JJ','I-NP','O','N/N'),w(5,7,view,view,'NN','I-NP','O','N'),w(5,8,of,of,'IN','I-PP','O','(NP\\NP)/NP'),w(5,9,space,space,'NN','I-NP','O','N'),w(5,10,'.','.','.','O','O','.'),ccg(6,rp('S[dcl]',ba('S[dcl]',ba('NP',fa('NP[nb]',lf(6,1,'NP[nb]/N'),fa('N',lf(6,2,'N/N'),lf(6,3,'N'))),fa('NP\\NP',lf(6,4,'(NP\\NP)/NP'),ba('NP',fa('NP[nb]',lf(6,5,'NP[nb]/N'),fa('N',lf(6,6,'N/N'),lf(6,7,'N'))),fa('NP\\NP',lf(6,8,'(NP\\NP)/NP'),fa('NP[nb]',lf(6,9,'NP[nb]/N'),lf(6,10,'N')))))),fa('S[dcl]\\NP',lf(6,11,'(S[dcl]\\NP)/NP'),appo('NP',lex('N','NP',lf(6,12,'N')),lex('N','NP',fa('N',lf(6,13,'N/N'),lf(6,14,'N')))))),lf(6,15,'.'))),w(6,1,'A',a,'DT','I-NP','O','NP[nb]/N'),w(6,2,small,small,'JJ','I-NP','O','N/N'),w(6,3,partition,partition,'NN','I-NP','O','N'),w(6,4,at,at,'IN','I-PP','O','(NP\\NP)/NP'),w(6,5,the,the,'DT','I-NP','O','NP[nb]/N'),w(6,6,northern,northern,'JJ','I-NP','O','N/N'),w(6,7,part,part,'NN','I-NP','O','N'),w(6,8,of,of,'IN','I-PP','O','(NP\\NP)/NP'),w(6,9,the,the,'DT','I-NP','O','NP[nb]/N'),w(6,10,room,room,'NN','I-NP','O','N'),w(6,11,contains,contain,'VBZ','I-VP','O','(S[dcl]\\NP)/NP'),w(6,12,'Picard-APOS-s','Picard-APOS-s','NNP','I-NP','O','N'),w(6,13,sleeping,sleep,'VBG','I-NP','O','N/N'),w(6,14,'area.','area.','NN','I-NP','O','N'),w(6,15,'.','.','.','O','O','.'),end_of_file],_)).


text2ccg(Text, Lines) :-
%       process_create(path('candc'), [ '--models','models/boxer','--input',Text,'--output',Ccg,'--log',Log,'--candc-printer','boxer','--integrate' ],
        process_create(path('candc'), [ '--models','models/boxer','--candc-printer','boxer','--integrate' ],
                       [ cwd('/devel/logicmoo/src_modules/candc'),
                         stdout(pipe(Out)),
                         stdin(pipe(In)),                    
                         process(Process)
                       ]),
        format(In,'~w~n',[Text]),
        put_code(In,10),
   flush_output(In),
      put_code(In,10),
   flush_output(In),
        close(In),
   read_line_terms(Out,Lines),
   process_wait(Process,_Status).

read_line_terms(Out,_):-!.
read_line_terms(Out,Lines):-
   ignore_1_line(Out),
   ignore_1_line(Out),
   ignore_1_line(Out),
   ignore_1_line(Out),
   ignore_1_line(Out),
   ignore_1_line(Out),
   read(Out, Lines),


% at_end_of_stream
read_lines(Out, Lines) :-
        read_line_to_codes(Out, Line1),
        read_lines(Line1, Out, Lines).

read_lines(end_of_file, _, []) :- !.
read_lines(Codes, Out, [Line|Lines]) :-
        atom_codes(Line, Codes),
        read_line_to_codes(Out, Line2),
        read_lines(Line2, Out, Lines).



/*
:- text2ccg("You find yourself standing by the door of Captain Picard's quarters.
He isn't very fond of visitors, but you decide to stay and have a look around . 
You can see several different ancient artifacts on tables and small pedestals, and a large wooden wardrobe is facing south .
A comfortable looking recliner with a matching footrest sits beside the door, along with a bright reading lamp and end table .
Two large windows offer a great view of space . 
A small partition at the northern part of the room contains Picard's sleeping area.").
*/

end_of_file.


File T:\devel\logicmoo\src_modules\candc\src\prolog\boxer\boxer.pl
  242 18:   Definition of start
  245 1:start:-
  253 1:start:- 
File T:\devel\logicmoo\src_modules\candc\src\prolog\boxer\knowledge\relations.pl
  4125 15:nn('housing','start','with').
  7335 14:nn('season','start','of').
  7876 5:nn('start-up','company','on').
File T:\devel\logicmoo\src_modules\candc\src\prolog\boxer\lex\verbnet.pl
  4427 9:verbnet(start, (s:_\np)/s:_, ['Theme','Agent'], [55,'.',1,-,1]).
  4442 9:verbnet(start, s:_\np, ['Theme'], [55,'.',1,-,1]).
  4457 9:verbnet(start, (s:_\np)/np, ['Theme','Agent'], [55,'.',1,-,1]).
  4472 9:verbnet(start, (s:_\np)/pp, ['Theme'], [55,'.',1,-,1]).
  4487 9:verbnet(start, ((s:_\np)/pp)/np, ['Theme','Agent'], [55,'.',1,-,1]).
  4502 9:verbnet(start, (s:_\np)/np, ['Theme','Instrument'], [55,'.',1,-,1]).
  4517 9:verbnet(start, (s:_\np)/ (s:to\np), ['Theme','Agent'], [55,'.',1]).
  23425 9:verbnet(start, (s:_\np)/pp, ['Experiencer'], [31,'.',3,-,1]).
  23603 9:verbnet(start, (s:_\np)/pp, ['Stimulus'], [31,'.',3]).
  23717 9:verbnet(start, s:_\np, ['Experiencer'], [31,'.',3]).
File T:\devel\logicmoo\src_modules\candc\src\prolog\boxer\lex\framenet.pl
  3100 11:fnpattern(start, 55010100, 'Activity_start', ['Agent': 'Agent', 'Theme': 'Activity', 'Time': 'Time']).
  3101 11:fnpattern(start, 55010100, 'Process_start', ['Theme': 'Event', 'Time': 'Time']).
File T:\devel\logicmoo\src_modules\candc\src\prolog\nutcracker\interfaceTPandMB.pl
  81 22:setsid or die "Can't start a new session: $!";
File T:\devel\logicmoo\src_modules\candc\src\prolog\nutcracker\nutcracker.pl
  958 18:   Definition of start
  961 1:start:-
  973 1:start:- 
File T:\devel\logicmoo\src_modules\candc\src\prolog\nutcracker\ntokkie.pl
  554 18:   Definition of start
  557 1:start:-
  564 1:start:- 
File T:\devel\logicmoo\src_modules\candc\src\prolog\nutcracker\tokkie.pl
  530 18:   Definition of start
  533 1:start:-
  540 1:start:- 
File T:\devel\logicmoo\src_modules\candc\src\prolog\nutcracker\para.pl
  133 18:   Definition of start
  136 1:start:-
  144 1:start:- halt.
  146 4::- start.
File T:\devel\logicmoo\src_modules\candc\src\data\italian\prolog\ccg.pl
  23 40:   Stack = [],      %%% stack empty at start
File T:\devel\logicmoo\src_modules\candc\src\data\vpe\transform.pl
  173 14:   Automatic start
File T:\devel\logicmoo\src_modules\candc\src\data\vpe\type.pl
  520 14:   Automatic start
File T:\devel\logicmoo\src_modules\candc\src\data\vpe\antecedent.pl
  408 14:   Automatic start
File T:\devel\logicmoo\src_modules\candc\src\data\vpe\annotate.pl
  517 14:   Automatic start
File T:\devel\logicmoo\src_modules\candc\src\data\vpe\eval.pl
  486 18:   Automatically start
