

:- module(transform_dump,[]).
:-include('trans_header.pl').

:- dynamic(assertion_holds/2),
   multifile(assertion_holds/2),
   dynamic(assertion_holds/3),
    multifile(assertion_holds/3),
   dynamic(assertion_holds/4),
    multifile(assertion_holds/4),
   dynamic(assertion_holds/5),
    multifile(assertion_holds/5),
   dynamic(assertion_holds/6),
    multifile(assertion_holds/6),
   dynamic(assertion_holds/7),
    multifile(assertion_holds/7),
    !.

:- style_check(+singleton).

%:-consult('quickkb.pl').

transform_file(File):-expand_file_name(File,Result),Result\=[File],!,forall(member(F0,Result),transform_file(F0)).
transform_file(File):-
   atom_concat(File,'.xfrm2',OFile),
   open(File,read,Desc),
   open(OFile,write,ODesc),
   fmt(ODesc,':-include(\'trans_header.pl\').~n',[]),
   repeat,
   catch(once(read_term(Desc,O,[variables(_Vars),variable_names(Names),double_quotes(string)])),E,(fmt('~n ;; READER FAILED ~q ',[E]),fail)),
   catch(once(must_write_assertions(ODesc,O,Names)),E2,(fmt('~n ;; SAVING ERRORED ~q on ~q ',[E2,O]),fail)),  
   O=end_of_file,!,
   close(ODesc),
   close(Desc).


% ?- transform_file2('../hl_holds_all_u.pl').
% ?- transform_file2('../src_data/pldata/el_holds.pl').
:-export(transform_file2/1).
transform_file2(File):-expand_file_name(File,Result),Result\=[File],!,forall(member(F0,Result),transform_file2(F0)).
transform_file2(File):-
   open(File,read,Desc),
   call_cleanup((
   repeat,
    catch(once(read_term(Desc,O,[variables(_Vars),variable_names(Names),double_quotes(string)])),E,(fmt('~n ;; READER FAILED ~q ',[E]),fail)),
    must_det(write_assertions2(O,Names)),
    O=end_of_file,!),
   close(Desc)),
   told,
   listing(tracking:el_holds_functor/2),
   listing(tracking:el_holds_pred/1),!,
   listing(tracking:tracked_thing/2),
   setof(A,tracking:tracked_thing(_,A),LIST),
   forall(member(A,LIST),show_flag(A)).

show_flag(A):-flag(A,V,V), format('% flag ~q ~w ~n',[A,V]).



:-dynamic tracking:el_holds_functor/2.
:-dynamic tracking:tracked_thing/2.

:-dynamic tracking:el_holds_pred/1.
:-dynamic last_assertion/1.

write_assertions2(vvar,Vars):-!, throw(write_assertions2(vvar,Vars)).
write_assertions2(end_of_file,_Vars):-!.
write_assertions2((:-(Call)),_Vars):- !, must_det(Call).
write_assertions2([],_Vars):-!. 

% write_assertions2(LA,Vars):- LA=..[_|PLIST],append(KEEP,[PROPS,_],PLIST),write_assertions2(LA,Vars,KEEP,PROPS),!.
write_assertions2(LA,Vars):- LA=..[F|PLIST],append(KEEP,[MT,PROPS],PLIST),write_assertions2(F,LA,Vars,KEEP,MT,PROPS),!.

write_assertions2(F,_LA,_Vars,KEEP,MT,PROPS):- atom_contains(F,'implies'),retractall(last_assertion(_)),asserta(last_assertion(KEEP)),
             append(POST,[PRE],KEEP),writeas([implies,PRE,POST],MT,PROPS),!.
write_assertions2(_,_LA,_Vars,KEEP,MT,PROPS):- retractall(last_assertion(_)),asserta(last_assertion(KEEP)),writeas(KEEP,MT,PROPS),!.

writeas(KEEP,PROPS):- ignore(member(amt(MT),PROPS)),writeas(KEEP,MT,PROPS).

writeas([K|KEEP],MT,PROPS):-not(member(wrap(upred),PROPS)),var(K),!,writeas2([K|KEEP],MT,[wrap(upred)|PROPS]).

writeas([rewrite,Post],MT,PROPS):-not(member(wrap(upred),PROPS)), !, must_det(writeas(Post,MT,PROPS)).
writeas([implies,Pre,Post],MT,PROPS):- 
   must_det(rewrite_ante(Pre,PreO,NewProps)),
   must_det(rewrite_consq(Post,PostO,NewProps2)),
   append(PostO,[PreO],KEEP),!,
   flatten([PROPS,NewProps,NewProps2],NPROPS),
   must_det(writeas(KEEP,MT,[wrap(implies)|NPROPS])).

writeas([isa,E,C],MT,PROPS):- !,writeas([C,E],MT,[wrap(isa)|PROPS]).
writeas([P,A],MT,PROPS):- var(A),!,writeas2([P,A],MT,[wrap(varg)|PROPS]).
writeas([P,A,B],MT,PROPS):- member(V,[A,B]),var(V),!,writeas2([P,A,B],MT,[wrap(varg)|PROPS]).
writeas([P,A,B,C],MT,PROPS):- var(C),!,writeas2([P,A,B,C],MT,[wrap(varg)|PROPS]).

writeas([P,A,B],MT,PROPS):- member('',[A,B]),!,writeas2([rewriteString,[P,A,B]],MT,PROPS).
writeas([P,A,B,C|L],MT,PROPS):- C=='',!,writeas2([rewriteString,[P,A,B,C|L]],MT,PROPS).

writeas(KEEP,MT,PROPS):-writeas2(KEEP,MT,PROPS).

writeas2(KEEP,MT,PROPS):-writeas_wrapped(el_holds,KEEP,MT,PROPS).

rewrite_consq(A,A,[]):-var(A),!.
rewrite_consq([],[simplyTrue],[]):-!.
rewrite_consq(A,A,[]):-is_list(A).
rewrite_consq(A,A,[wrap(plist)]).

rewrite_ante(A,A,[]):-var(A),!.
rewrite_ante([P|Post],holds_varplist(P,Post),[wrap(plist)]):-not(atom(P)),!.
rewrite_ante([],[simplyTrue],[]).
rewrite_ante(PostL,Post,[]):-is_list(PostL),!,Post=..PostL.
rewrite_ante([P|Post],holds_varplist(P,Post),[wrap(plist)]).

writeas_wrapped(El_holds,[isa,X,Y],MT,PROPS):- atom_concat(El_holds,'_unary',ISA),writeas_wrapped0(ISA,[Y,X],MT,PROPS).
writeas_wrapped(El_holds,KEEP,MT,PROPS):-writeas_wrapped0(El_holds,KEEP,MT,PROPS).

writeas_wrapped0(El_holds,KEEP,MT,PROPS):- member(wrap(_),PROPS),!, setof(W,member(wrap(W),PROPS),WRAPS),atomic_list_concat([El_holds|WRAPS],'_',NEWHOLDS),
   writeas_wrapped1(NEWHOLDS,KEEP,MT,PROPS),!.
writeas_wrapped0(El_holds,KEEP,MT,PROPS):- writeas_wrapped1(El_holds,KEEP,MT,PROPS),!.

writeas_wrapped1(El_holds,KEEP,MT,PROPS):- sort(PROPS,PROPSO),append(KEEP,[MT,PROPSO],PLIST),LA=..[El_holds|PLIST], write_nv(LA).

write_nv(LA):- once((functor(LA,F,A),retractall(tracking:el_holds_functor(F,A)),assertz_if_new(tracking:el_holds_functor(F,A)),retractall(tracking:el_holds_pred(F)),
               arg(1,LA,Arg1),\+ \+ add_count(F,Arg1),
                     assertz_if_new(tracking:el_holds_pred(F)))),numbervars(LA,50,_,[singletons(true),attvar(skip)]),format('~q.~n',[LA]),!.

add_count(F,Arg1):- var(Arg1),!,add_count0(F,var).
add_count(F,[Arg1,_]):- !, add_count(F,Arg1).
add_count(F,Arg1):- atom(Arg1),!,add_count0(F,Arg1).
add_count(F,Arg1):- compound(Arg1),functor(Arg1,F,_),!,add_count0(F,Arg1).
add_count(_F,_Arg1):- dtrace.

add_count0(F,Arg1):-must_det(atom(Arg1)),flag(Arg1,X,X+1),!,assertz_if_new(tracked_thing(F,Arg1)).


printall:- 
  forall(between(2,7,A),
   (length(List,A),
   Goal=..[call_assertion_holds|List],
   forall(Goal,transform_goal(List)))).


keep_p(assertion_holds_mworld0).
keep_p(assertion_holds).
keep_p(el_holds).

keep_escaped(vars).
keep_escaped(amt).
keep_escaped('=').
keep_escaped('UnicodeStringFn').
keep_escaped('URLFn').


:-export(writeall/0).
writeall:-doall((get_assertions(PLIST,PROPSIn),once((fix_props(PROPSIn,PROPS),append(PLIST,[PROPS,_],PLISTP),Call=..[assertion_props|PLISTP],writeq(Call),write((.)),nl)))).

:-export(dnf_to_pnf/2).
dnf_to_pnf([[],PNF],PNF):-!.
dnf_to_pnf(A,A):-trace.
% fix_props(PROPSIn,PROPSIn):-!.
fix_props(PROPSIn,PROPSOut):-sort(PROPSIn,PROPSOut).

:-export(fix_sentence/2).
fix_sentence(_,NV):-not(var(NV)),!,trace.
fix_sentence(VAR,VAR):-isvvvar(VAR),!.
fix_sentence(M:P,M:PP):-atom(M),!,fix_sentence(P,PP),!.
fix_sentence([],[]).
fix_sentence("",'').
fix_sentence(Proof,New):-atom(Proof),!,to_aword(Proof,New).
fix_sentence(S,A):-string(S),debugOnError(atomSplitEasy(S,L)),reduceWordList(L,A),!.
fix_sentence(Proof,New):-not(compound(Proof)),!,Proof=New.

% improper lists
fix_sentence([B|Proof],[B|Proof]):- not(is_list([B|Proof])),!.

% maybe proper lists 
fix_sentence([VAR|PLISTB],[VAR|PLISTA]):-isvvvar(VAR),!,fix_sentence(PLISTB,PLISTA),!.
fix_sentence([TheList|List],Words):-nonvar(TheList),reduceable_when_string(TheList),fix_sentence(List,Words),!.

% proper lists 
fix_sentence([A],W):- !, fix_sentence(A,W),!.
fix_sentence([implied|PLIST],PLISTO):-append(ALIST,[Last],PLIST),!,fix_sent_arglist([implies,Last,ALIST],PLISTO).
fix_sentence(B,A):- is_list(B), fix_sent_arglist(B,BB),  (B \= BB -> fix_sentence(BB,A) ; reduceWordList(BB,A)),!.
fix_sentence([B|L],[B|L]):-trace.

% compounds
fix_sentence(nart(NART),A):- !,fix_sentence(NART,A).
fix_sentence('NART'(NART),A):- !,fix_sentence(NART,A).
fix_sentence(string(B),AA):-!, stringToWords(B,C),reduceWordList(C,A),!,AA=A.
fix_sentence(TheList,Words):- functor(TheList,'TheList', _),TheList=..[_|List],fix_sentence(List,Words),!.
fix_sentence(B,A):- B=..[P|ARGS],fix_sent_arglist([P|ARGS],A),!.
fix_sentence(A,A):-!,trace.

fix_sent_arglist([B|Proof],OUT):- isvvvar(B),!,must_det(fix_arglist(Proof,New)),OUT=[B|New].
fix_sent_arglist([P|ARGS],[P|ARGS]):-keep_escaped(P),!.
fix_sent_arglist([P|ARGS],A):- keep_p(P),fix_arglist(ARGS,ARGSO),!,A=..[P|ARGSO],!.
fix_sent_arglist(B,A):-fix_arglist(B,A).




fix_arglist(X,X):-isvvvar(X).
fix_arglist(X,X):-not(compound(X)),!.
fix_arglist([B|Proof],OUT):- isvvvar(B),!,must_det(fix_arglist(Proof,New)),OUT=[B|New].
fix_arglist([B|Proof],OUT):- fix_sentence(B,A),!,must_det(fix_arglist(Proof,New)),OUT=[A|New].


isvvvar(Var):-var(Var),!.
isvvvar('$VAR'(_)).

reduceable_when_string('NLPattern-Exact').
reduceable_when_string('NLPatternList').
reduceable_when_string('TheList').

:-export(reduceWordList/2).
reduceWordList(Var,Var):-isvvvar(Var),!.
reduceWordList(string(S),Words):-!,reduceWordList(S,Words).
reduceWordList(L,A):-is_list(L),not((member(E,L),is_list(E))),once((stringToWords(L,W))),W\=L,reduceWordList(W,A),!.
reduceWordList([A],W):-reduceWordList(A,W),!.
reduceWordList(A,W):-to_aword(A,W),!.

:-export(stringToWords/2).
stringToWords(Var,Var):-var(Var).
stringToWords(Var,Var):-isvvvar(Var),!.
stringToWords([],[]).
stringToWords(string(S),Words):-!,stringToWords(S,Words).
stringToWords([S|Tring],NewString):- isvvvar(Tring),!,stringToWords(S,W),append(W,Tring,NewString),!.
stringToWords([S|Tring],[S|Tring]):- not(is_list([S|Tring])),!. 
stringToWords([S|Tring],NewString):- Tring=[_|_],is_list(Tring),!,stringToWords(S,W),stringToWords(Tring,Words),append(W,Words,NewString),!.
stringToWords([S|Tring],NewString):- Tring=[_|_],!,between(1,5,X),length(Tring,X),stringToWords(S,W),stringToWords(Tring,Words),append(W,Words,NewString),!.
stringToWords(B,A):-is_list(B),!,fix_arglist(B,A),!.
stringToWords(A,[W]):-to_aword(A,W),!.

to_string_words([],[]):-!.
to_string_words([Eng|Lish],[E|Atoms]):-
 to_aword(Eng,E),!,
 to_string_words(Lish,Atoms).

to_aword(Eng,Eng):-isvvvar(Eng),!.
to_aword(string(A),W):-!,reduceWordList(A,W).
to_aword([A],W):-!,to_aword(A,W).
to_aword("",'').
to_aword('""','').
to_aword([],'').
to_aword(Eng,W):-string(Eng),!,atomSplitEasy(Eng,WL),reduceWordList(WL,W),!.
to_aword(Eng,N):-atom(Eng),fix_atom(Eng,N),!.
to_aword(Eng,Eng).


perhaps_string(Left,N):-atomSplitEasy(Left,WL),reduceWordList(WL,N),!.

fix_atom(Eng,N):-atom_number(Eng,N),!.
fix_atom(Eng,Eng):-atom_concat('"htt',_,Eng),!.
fix_atom(Eng,Eng):-atom_concat('"<!',_,Eng),!.
fix_atom(Eng,Eng):-atom_concat('"<',_,Eng),!.
fix_atom(Eng,Eng):-atom_length(Eng,Len),Len>300,!.
fix_atom(Eng,N):-atom_concat('"',Right,Eng),atom_concat(Left,'"',Right),!,perhaps_string(Left,N).
fix_atom(Eng,N):-atom_concat('TTWord-',Right,Eng),atom_concat(Right,'-TheWord',N),!.
% fix_atom(Eng,Right):-atom_concat('#$',Right,Eng),atom_length(Right,Len),Len>0,!.
fix_atom(Eng,Eng).



transform_goal(List):-transform_goal(List,ListNew),!,write_goal(ListNew).
transform_goal(List,ListNew):- transform_assert(List,_MISSING_Vars,ListNew).
write_goal(Goal):- Goal=..[call_assertion_holds|_List],
  fmt('~q.~n',Goal).

printallW:- 
  see('mworld0.pl'),
  tell('mworld1.pl'),
  repeat,
  read(CL),
  CL=..List,
   Goal=..[assertion_holds_w|List],
  format('~q.~n',[Goal]),
  CL == end_of_file,
  seen,
  told.

end_of_file.

/*
end_of_file.


indexCyc(_).
dynamicCyc(Pred/Arity):-
      dynamic(Pred/Arity),
      A2 is Arity-2,
   dynamic(assertion_holds/A2),
   multifile(assertion_holds/A2),
      length(ListConsq,A2),
      append(ListConsq,[_Mt,_Vars],ListAnte),
      Head=..[assertion_holds,Pred|ListConsq],
      Body=..[Pred|ListAnte],
      ((clause(Head,Body)->true;
      assert((Head:-Body)))).

%['afterAdding','coExtensional',['SubLQuoteFn','ADD-TVA-CACHE-VALUE']
call_assertion_holds(P,A):- 'ASSERTION'(_TRUTH,_NNF,_MT,_,/*HL*/[P,A]).
call_assertion_holds(P,A,B):- 'ASSERTION'(_TRUTH,_NNF,_MT,_,/*HL*/[P,A,B]).
call_assertion_holds(P,A,B,C):- 'ASSERTION'(_TRUTH,_NNF,_MT,_,/*HL*/[P,A,B,C]).
call_assertion_holds(P,A,B,C,D):- 'ASSERTION'(_TRUTH,_NNF,_MT,_,/*HL*/[P,A,B,C,D]).
call_assertion_holds(P,A,B,C,D,E):- 'ASSERTION'(_TRUTH,_NNF,_MT,_,/*HL*/[P,A,B,C,D,E]).
call_assertion_holds(P,A,B,C,D,E,F):- 'ASSERTION'(_TRUTH,_NNF,_MT,_,/*HL*/[P,A,B,C,D,E,F]).

call_assertion_holds(P,A):-assertion_holds(P,A).
call_assertion_holds(P,A,B):-assertion_holds(P,A,B).
call_assertion_holds(P,A,B,C):-assertion_holds(P,A,B,C).
call_assertion_holds(P,A,B,C,D):-assertion_holds(P,A,B,C,D).
call_assertion_holds(P,A,B,C,D,E):-assertion_holds(P,A,B,C,D,E).
call_assertion_holds(P,A,B,C,D,E,F):-assertion_holds(P,A,B,C,D,E,F).



% ('mworld0.pldata') compiled into world 61.18 sec, 483,738 clauses
% :-dmsg(loading(kb0)).
:- ensure_loaded(('mworld0.pldata')).

%  ('tiny_kb.pldata') compiled into world 2.92 sec, 9,019 clauses
% :-dmsg(loading(kb1)).
:- ensure_loaded(('tiny_kb.pldata')).


% :-dmsg(loading(kb2)).
:- ensure_loaded(('hl_holds.pldata')).

:-tell(save_all).
:-listing.
:-told.


findall(call_assertion_holds(P,A,B,C,D,E,F)
end_of_file.

*/

%  mworld0.pldata compiled 33.49 sec, 485,079 clauses
%  tiny_kb.pldata compiled 1.64 sec, 9,019 clauses
%  hl_holds.pldata compiled 94.05 sec, 1,041,323 clauses


assertKif2(X,Y):-fmt('% ~w. ~n',[assertKif2(X,Y)]),!.

%assert_if_new(X):-catch(X,_,fail),!.
%assert_if_new(X):-assertz(X).

fmt(X,Y):-format(X,Y).
fmt(X,Y,Z):-format(X,Y,Z).

%:- ['constants7133.pl'].
%:- ['transform_dump.pl'].
%:-['fooo0'].
renameC(X,Y):-atom_concat('#$',L,X),string_to_atom(S,L),!,renameC1(Y,S).
renameC1(X,Y):-assertKif2('#$oldConstantName'(X,Y),_).
renameC1(X,Y):-assertKif2('#$mergedConstantName'(X,Y),_).

%not_renamed(X):-renamed(_,X),!,fail.
doRenamed(X,Y):-renamed(X,Y),not(renamed(_,X)).

:-dynamic(not_renamed/1).
:-dynamic(renamed/2).

renamedChk(X,Y):-not(var(Y)),!,fail.
renamedChk(X,Y):-not_renamed(X),!,fail.
renamedChk(X,Y):-renamed(X,Y),!.
/*renamedChk(Y,X):-
   atom_concat('#$',YY,Y),string_to_atom(YS,YY),
   renameC1(X,YS),not(twoFound([X,Y])),
   asserta(renamed(Y,X)),'fmt'('renamed ~w --> ~w. ~n',[Y,X]),!.
   */
renamedChk(Y,YO):-
   once((atom_concat('#$',YY,Y),string_to_atom(YS,YY),renameC1(YO,YS))),
   %renamedChk2(Y,YS),
   not(twoFound([YO,Y])),
   asserta(renamed(Y,YO)),'fmt'('renamed ~w --> ~w. ~n',[Y,YO]),!.
renamedChk(X,Y):-asserta(not_renamed(X)),!,fail.

renamedChk2(Y,YS):-
   renameC1(X,YS),twoFound([X,Y]),!,
   fmt('skip ~w <- ~w. ~n',[X,Y]),asserta(not_renamed(Y)),!,fail.




renameC1:-renameC1(X,Y),
   once(atom_concat('#$',Y,YY)),
   (twoFound([X,YY]) -> fmt('skip ~w <- ~w. ~n',[X,YY]) ; (asserta(renamed(YY,X)),fmt('renamed ~w --> ~w. ~n',[YY,X]))),
   fail.
renameC1.


twoFound([X,YY]):-assertKif2(O,_),sub_var(X,O),sub_var(YY,O),!.

tk:- transform_file('../src_data/tiny_kb.pldata'),transform_file('../src_data/*.pldata').

/*

*/
ggtrace:- visible(+all), leash(-exit),leash(-fail),leash(-call),leash(-redo).
ggtraceoff:- visible(-none).




rename_constants(O,O):- (var(O);O='$VAR'(_)),!.
rename_constants([],[]).
rename_constants([H|T],[HO|TO]):-!,rename_constants(H,HO),rename_constants(T,TO).
rename_constants(O,OO):-atom(O),must(transform_sent_atom(O,OO)),!.
rename_constants(O,OL):-compound(O),O=..LIST,rename_constants(LIST,OL).
rename_constants(O,O).

listify(O,OO):- must(rename_constants(O,OO)).

write_assertions(ODesc,Var,Vars):-var(Var),!, throw(write_assertions(ODesc,Var,Vars)).
write_assertions(ODesc,end_of_file,Vars):-!.
write_assertions(ODesc,(:-(Call)),Vars):- once((catch(Call,E,fmt('Error in directive ~q: ~q ',[Call,E]));fmt('Failed directive ~q. ~n',[Call]))),!.
write_assertions(ODesc,[],Vars):-!.
write_assertions(ODesc,O,Vars):-
   notrace((rename_vars(Vars,VarsO))),!,
   notrace((rename_constants(O,OO))),!,
   numbervars(VarsO,'$VAR',0,_),
   transform_assert(OO,VarsO,XFRM),!,
   fmt(ODesc,'~q.~n',[XFRM]),
   fmt(user_error,'SUCCEED: ~q.~n',[XFRM]),
   catch(flush_output(user_error),_,true).

rename_vars([],[]).
rename_vars([N=V|A],[NN=V|B]):-rename_var1(N,NN),!,rename_vars(A,B).
rename_var1(A,V):-atom_concat('__',AA,A),rename_var2(AA,BB),atom_concat('??',BB,V).
rename_var1(A,V):-atom_concat('_',AA,A),rename_var2(AA,BB),atom_concat('?',BB,V).
rename_var1(V,V):-!.
rename_var2(AA,BB):-concat_atom(L,'_',AA),concat_atom(L,'-',BB).

restring([],Var,[]):-!.
restring([R|HOLDS],Var,[RH|OLDS]):-!,
   restringArg(R,Var,RH),!,restring(HOLDS,Var,OLDS),!.

restringArg(In,Out):-restringArg00(In,Mid),restringArg11(Mid,Out),!.

restringArg00(RHOLDS,Var,string([RHOLDS])):-atom(RHOLDS),not(atom_concat('#$',_,RHOLDS)),!.
restringArg00([R|OLDS],Var,string([R|OLDS])):-atom(R),not(atom_concat('#$',_,R)),!.
restringArg00([R,H|OLDS],Var,string([R,H|OLDS])):-atom(H),not(atom_concat('#$',_,H)),!.
restringArg00(RHOLDS,Var,RHOLDS):-!.

restringArg11(string(Mid),Out):-restringArg11(Mid,Out).
restringArg11(([Mid]),Out):-restringArg11(Mid,Out).
restringArg11(Out,Out).

logicallySame(X,X).


transform_askable(Comp,Vars,Out):- Comp\=[_|_],compound(Comp),listify(Comp,LIST) ,!,transform_askable(LIST,Vars,Out).
transform_askable([P|In],Vars,Out):- isHolds(P), transform_askable(In,Vars,Out).
transform_askable([K|KK],Vars,KM):-is_list([K|KK]),atom(K),!,transform_list(K,0,[K|KK],Vars,KM).
transform_askable(K,Vars,KO):- transform_uthing(K,Vars,KO),!.
transform_askable(K,Vars,KO):- atom(K),transform_sent_atom(K,KM),transform_sent_atom_pt2(KM,KO).

transform_askable([[[],[SENT]],MT,[],SENT],                        Vars,ISTGAF):-transform_term_mt(SENT,Vars,MT,ISTGAF).
transform_askable([[[],[SENT]],MT,[],SENT,[not,SENT]],             Vars,ISTGAF):-transform_term_mt([not,SENT],Vars,MT,ISTGAF).
transform_askable([[[ANTE],[CONSEQ]],MT,CYCVARS,[implies,ANTE,CONSEQ]],  Vars,ISTGAF):-transform_term_mt([implies,ANTE,CONSEQ],Vars,MT,ISTGAF).

transform_askable([[[ANTE1,ANTE2,ANTE3],[CONSEQ]],MT,CYCVARS,[implies,[and,ANTE1,ANTE2,ANTE3],CONSEQ]],  Vars,ISTGAF):-
      EXPR = [implies,[and,ANTE1,ANTE2,ANTE3],CONSEQ],
      transform_term_mt(EXPER,Vars,MT,ISTGAF).

transform_askable([[ANDLIST,CONSEQL],MT,CYCVARS,EXPR],  Vars,ISTGAF):-
      logicallySame(EXPR , [implies,[and|ANDLIST],[and|CONSEQL]]),
      transform_term_mt(EXPR,Vars,MT,ISTGAF).

/*
transform_askable([[[[isa,A,B],[relationExistsAll,C,D,B]],[[C,['RelationExistsAllFn',A,C,D,B],A]]],'BaseKB',
 ["?TERM","?INDEP-COL","?PRED","?DEP-COL"],
 [implies,[and,[isa,A,B],[relationExistsAll,C,D,B]],[C,['RelationExistsAllFn',A,C,D,B],A]]]
*/

transform_askable(K,Vars,ask(Vars,K)):-trace.


must(P):-notrace((P)),!.
must(P):-trace,P,!.

isHolds(holds).
isHolds(asserted).

transform_sentCall([X|XO],hold([X|XO])).
transform_sentCall(XO,XO).

transform_assert((holds(pl_implied,PRED, REST, PRE)),Vars,OUT):-!,
   transform_assert(holds(implies,PRE,[PRED|REST]),Vars,OUT),!.

transform_assert(Comp,Vars,Out):- (( Comp\=[_|_],compound(Comp),listify(Comp,LIST))) ,!,transform_assert(LIST,Vars,Out).

transform_assert([Holds|HOLDS],Vars,H):-
   numbervars(HOLDS,'$VAR',0,_),
   transform_askable(HOLDS,Vars,H).

transform_assert([assertKif2,X,Y],Vars,assertKif3(XO,YO)):-
      numbervars(assertKif2(X,Y),'$VAR',0,_),
   transform_askable(X,Vars,XO),
   transform_askable(Y,Vars,YO),!.

transform_assert([assertKif3,X,Y],Vars,XOO):-
      numbervars(assertKif3(X,Y),'$VAR',0,_),
   transform_askable(X,Vars,XO),!,transform_sentCall(XO,XOO),!.

transform_assert([assertKif,[':KIF',SENT,':MICROTHEORY',MT|PROPS]],Vars,OUT):-
   numbervars(SENT,'$VAR',0,_),
   transform_askable(SENT,Vars,KIF),
   transform_uthing(MT,Vars,MTO),
   fixProps([':VARS',Vars,':MICROTHEORY',MTO|PROPS],FNEWPROPS),
   OUT = assertKif2(KIF,FNEWPROPS),!.

transform_assert(['termOfUnit', ['NART',[T|ERM]],[T|ERM]],Vars,':-'(declareNart(NEWGAF))):- transform_list(T,0,[T|ERM],Vars,NEWGAF),!.

transform_assert(['ASSERTION'|REST],Vars,NEWGAF):- !, transform_assert(REST,Vars,NEWGAF).

transform_assert([STR|REST],Vars,NEWGAFO):- transform_term_truth(GAF,STR,NEWGAF),!,
  must(( transform_gaf(REST,Vars,MT,STR,NEWGAFO) )),!.   

transform_assert([P|LIST],Vars,NEWGAF):- 
   transform_askable([P|LIST],Vars,NEWGAF),!.

fixProps([':TRUTH', ':TRUE'|NEWPROPS],NEWPROPSO):-!,fixProps(NEWPROPS,NEWPROPSO).
fixProps([_,[]|NEWPROPS],NEWPROPSO):-!,fixProps(NEWPROPS,NEWPROPSO).
fixProps([A,B|NEWPROPS],[A,B|NEWPROPSO]):-!,fixProps(NEWPROPS,NEWPROPSO).
fixProps(NEWPROPS,NEWPROPS):-!.


transform_term_truth(GAF,':TRUE-DEF',GAF):-!.
transform_term_truth(GAF,':TRUE-MON',proved(GAF)):-!.
transform_term_truth(GAF,':FALSE-DEF',naf(GAF)):-!.
transform_term_truth(GAF,':FALSE-MON',lognot(GAF)):-!.


noSVAR(svar(_,A),A).
noSVAR(A,A).

svar(A,B,A):-atom(B),atom_concat('?',_,B),!.
svar(_,A,A):-!.


transform_thing2(_,K):-not(var(K)),!,throw(not(var(K))).
transform_thing2(K,K):- (var(K);number(K)),!.
%%transform_thing2([K|KL],O):-atom(K),is_list([K|KL]),!,Comp=..[K|KL],!,transform_thing2(Comp,O).
transform_thing2('$VAR'(O), '$VAR'(O)):-!.
transform_thing2(K,S):- string(K),!,transform_string(K,S).
transform_thing2(['svar',A,B],AB):-!,svar(A,B,AB),!.
transform_thing2(['SubLQuoteFn',A],['SubLQuoteFn',AA]):-!,noSVAR(A,AA),!.
%//transform_thing2('string'([O]),Vars,OO):-!,restringArg(O,Vars,OO),!.
transform_thing2('string'(O),OO):-!,wordsList(O,OOO),!,(OOO=[OO];OOO=OO),!.
%// make too many strings
% transform_thing2([R|OLDS],string([R|OLDS])):-atom(R),not(atom_concat('#$',_,R)),!.
%//transform_thing2([R,H|OLDS],string([R,H|OLDS])):-atom(H),not(atom_concat('#$',_,H)),!.

transform_thing(Type,O,_Var,OO):- transform_thing2(O,OO),!.
transform_thing(Type,[nart,N],Vars,A):-!,transform_askable(isa(N,Type),Vars,A),!.
transform_thing(_,['BestNLPhraseOfStringFn',A],['BestNLPhraseOfStringFn',AA]):-!,restringArg(A,Vars,AA),!.
transform_thing(_,['BaseWordFormsEndingWithLetterSequenceFn',A],['BaseWordFormsEndingWithLetterSequenceFn',AA]):-!,restringArg(A,Vars,AA),!.
transform_thing(_,['BaseWordFormsStartingWithLetterSequenceFn',A],['BaseWordFormsStartingWithLetterSequenceFn',AA]):-!,restringArg(A,Vars,AA),!.
transform_thing(Type,[K|KK],Vars,['TheList'|KM]):-K=='TheList',!,transform_list(listOf(Type), 1, KK,Vars,KM),!.
transform_thing(Type,[K|KK],Vars,KM):-is_list([K|KK]),likeFun(K),!,transform_list(F,0,[K|KK],Vars,KM),!.
transform_thing(Type,[K|KK],Vars,[M|MM]):-transform_list(K,0,[K|KK],Vars,[M|MM]),!.

transform_uthing(K,Vars,KO):-transform_thing('Thing',K,Vars,KO),!.

argType(_,_,'Thing').


transform_sent_atom(K,A):-atom(K),var(A),doRenamed(K,A),!,fmt(user_error,'~w -> ~w. ~n',[K,A]),!.
%transform_sent_atom(K,A):-atom(K),var(A),renamedChk(K,A),!,fmt(user_error,'~w -> ~w. ~n',[K,A]),!.
transform_sent_atom(K,KO):-atom_concat('#$',KO,K),!.
transform_sent_atom(K,K):-atom_concat('#$',_,K),!.
transform_sent_atom(K,K):-atom_concat('ARG',_,K),!.
%transform_sent_atom(K,K):-atom_concat('NI',_,K),!.
transform_sent_atom(K,K):-atom_concat(':',_,K),!.
transform_sent_atom(K,K).

transform_sent_atom_pt2(A,B):-atom_concat('#$',B,A),!.
transform_sent_atom_pt2(A,A).

transform_string(AA,string(S4)):-string_to_atom(AA,A),
       concat_atom(List, ' ',A),
       wordsList(List,S1),wordsList(S1,S2),wordsList(S2,S3),wordsList(S3,S4).



transform_gaf(TERM,Vars,MT,STR,NEWGAF):-
	transform_askable(TERM,Vars,NEWTERM),!,
	transform_term_truth(NEWTERM,STR,GAF),!,
	transform_term_mt(GAF,Vars,MT,NEWGAF),!.


transform_term_mt(GAF,_Vars,MT,GAF):-var(MT),!.
transform_term_mt(GAF,_Vars,MT,GAF):-decontexted_mt(MT),!.
transform_term_mt(GAF,_Vars,MT,ist(MT,GAF)).

decontexted_mt(V):-var(V),!,fail.
decontexted_mt('BaseKB').
decontexted_mt('UniversalVocabularyMt').
decontexted_mt('BookkeepingMt').
decontexted_mt('EnglishParaphraseMt').
decontexted_mt(_).


wordsList([],[]).
wordsList([''|A],B):-!,wordsList(A,B).
wordsList(['.....'|A],['.'|B]):-!,wordsList(A,B).
wordsList(['....'|A],['.'|B]):-!,wordsList(A,B).
wordsList(['...'|A],['.'|B]):-!,wordsList(A,B).
wordsList(['..'|A],['.'|B]):-!,wordsList(A,B).
wordsList([string(S)|A],SB):-flatten([S],SS),wordsList(A,AA),append(SS,AA,SB).
wordsList([S|A],[S|B]):-not(atom(S)),!,wordsList(A,B).
wordsList([S|A],[S1,Punct|B]):-
   atom_length(S,L),L>1,
   member(Punct,['.',',','!','"','\n','?',')',']','}','n\'t','\'s','s\'']),atom_concat(S1,Punct,S),!,wordsList(A,B).
wordsList([S|A],[Punct,S1|B]):-
   atom_length(S,L),L>1,
   member(Punct,['"','{','[','(','\n']),atom_concat(Punct,S1,S),!,wordsList(A,B).
%wordsList([S|A],[S|B]):-sOK(S),!,wordsList(A,B).
%wordsList([S|A],ALL):-concat_atom([L1,L2],',',S),wordsList(A,B),append([L1,',',L2],B,ALL).
%wordsList([S|A],ALL):-concat_atom([L1,L2],',',S),wordsList(A,B),append(SS,B,ALL).
wordsList([S|A],[S|B]):-wordsList(A,B).

sOk(A):-atom_concat('?',_,A).
sOK(_):-fail.

transform_list(F,A,K,_Vars,K):-var(K),!.
transform_list(F,A,[K|KK],Vars,[M|MM]):- argType(F,A,Type), transform_thing(Type,K,Vars,M),A1 is A+1,!, transform_list(F,A1,KK,Vars,MM),!.
transform_list(F,A,[],Vars,[]).

likeFun(K):-var(K),!.
likeFun(K):-not(atom(K)),!.
likeFun(K):-atom_concat('#$',Trim,K),!,likeFun(Trim).
likeFun(K):-atom_concat(_,'FN',K),!.
likeFun(K):-atom_concat(_,'Unit',K),!.
likeFun(K):-atom_concat(_,'Fn',K),!.
likeFun(K):-atom_codes(K,[C|Odes]),is_lower(C),!,fail.
likeFun(K).

/*
':MICROTHEORY',MT,
                     ':DIRECTION',':FORWARD',':MONOTONICITY',':DEFAULT',
                     ':CREATOR','[]',':CREATION-DATE',20030416,
                     ':TRUTH',':TRUE',
                     ':ARGUMENT',':ASSERTED-TRUE-DEF'])).
*/
 % :-tk.



