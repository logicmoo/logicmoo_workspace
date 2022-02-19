/****************************************************************************/
% idioms(+Dictionay,+List,-Result)
%   implements non-overlapping idioms
%   idiomizes List into Result

:- module(nldata_dictionary_some01,[rememberDictionary2/3, idioms/3, rememberDictionary/0, translatePersonal/3, translatePersonalUTokens/2, rememberDictionary/3, explitVocab/2, to_codes/2, decontract/2, dictionary/3, dictionary_access/3, idioms2/3, idioms2/3, isa_speechpart/1, lexicicalRefWord/2, translatePersonalCodes3/2, lexicicalRefWords/2, translatePersonalCodes2/2, translatePersonalCodes/2, my_lexicalRef/4]).

:- dynamic((rememberDictionary2/3, idioms/3, rememberDictionary/0, translatePersonal/3, translatePersonalUTokens/2, rememberDictionary/3, explitVocab/2, to_codes/2, decontract/2, dictionary/3, dictionary_access/3, idioms2/3, idioms2/3, isa_speechpart/1, lexicicalRefWord/2, translatePersonalCodes3/2, lexicicalRefWords/2, translatePersonalCodes2/2, translatePersonalCodes/2, my_lexicalRef/4)).

:- use_module(library(logicmoo_plarkc)).

:- style_check(-singleton).
:- style_check(-discontiguous).
% :- style_check(-atom).
:- set_prolog_flag(double_quotes, string).
:- install_constant_renamer_until_eof.
:- set_prolog_flag(do_renames_sumo,never).

% ========================================================================================
% ========================================================================================


lexicicalRefWords([],[]).
lexicicalRefWords([W|L],[O|LL]):-
   lexicicalRefWord(W,O),!,
   lexicicalRefWords(L,LL).
   
lexicicalRefWord(W,lex(W,L)):-
   sformat(String,'~w',[W]),
   findall([POS,Pred,CycWord],my_lexicalRef(POS,Pred,CycWord,String),L).

% :- registerCycPred(((e2c_data:lexicalRef/4, e2c_data:isa/2))).


my_lexicalRef(POS,Pred,CycWord,String):-isa_speechpart(POS),assertion_holds('lexicalRef',POS,Pred,CycWord,String).

isa_speechpart(POS):-assertion_holds('isa',POS,'SpeechPart').
   

lexicicalRefWord(W,(W)):-!.

% ===================================================================
% ===================================================================

to_codes(A,O):-atom(A),!,atom_codes(A,O).
to_codes(S,O):-string(S),!,string_to_atom(S,A),to_codes(A,O).
to_codes(C,C).

translatePersonalUTokens(UWords,Output):-
	toLowercase(UWords,Words),idioms(doctor,Words,Output),!.

translatePersonal(In,OutputLower,OutputNormal):-
	to_codes(In,CodesNormal),
	toLowercase(CodesNormal,CodesLower),!,
        translatePersonalCodes(CodesLower,OutputLower),
        translatePersonalCodes(CodesNormal,OutputNormal),!.

translatePersonalCodes(Codes,Output):- swap_person(Codes,Output).
	

% ========================================================================================
% ========================================================================================
:-export(idioms/3).

idioms(D,F,R):- \+ is_list(F), into_text80(F,T80),!,idioms(D,T80,R).

idioms(doctor,F,R):-!,
      idioms2(slang,F,S),
      idioms2(contractions,S,C),
      idioms2(mud,C,M),
      idioms2(irc,M,I),!,
      idioms2(swap_person,I,P),
      idioms2(doctor ,P,R).

idioms(e2c,A,A):-!.
idioms(e2c,A,Z):-!,idioms2(contractions,A,Z),!.
idioms(e2c,A,Z):-!,
      idioms(contractions,A,D),      
      idioms(mud,D,H),
      idioms(irc,H,J),!,
      idioms(slang,J,Z),!.

idioms(D,F,R):-
      idioms2(D,F,R).


/*
idioms2(D,[Ignore|In],Out):-
	dictionary(D,ignore_word,Ignore),!,
	idioms2(D,In,Out).
	
idioms2(D,[F|In],[R|New]):-
      dictionary_access(D,F,R),!,
      idioms2(D,In,New).
*/

dig_into_dict([A,B,C],[_,_,_],[A,B,C]).
dig_into_dict([A,B],[_,_],[A,B]).
dig_into_dict([A],[_],[A]).
dig_into_dict([A],[_],A):- freeze(A, atom_or_string(A)).

idioms2(D,F,R):- \+ is_list(F), into_text80(F,T80),!,idioms2(D,T80,R).
idioms2(D,In,New):- D==contractions,idioms3(D,In,M),In\==M,!,idioms2(D,M,New).
idioms2(D,In,New):- idioms3(D,In,New).
idioms2(D,F,F).

idioms3(_,[],[]):-!.
idioms3(D,In,New):-
	((
  dig_into_dict(Match,From,Dig),
  dictionary_access(D,Dig,Replace),
  append(From,Right,In),
  %Match=From,
  (Match=From->true;maplist(nc_equal_s,Match,From)))),
  copy_term(Dig+Replace,Dig+Replace,Goals),maplist(call,Goals),
	idioms3(D,Right,NewAfter),
	flatten([Replace,NewAfter],New),!.

/*idioms3(D,[A|In],NewAfter):-
      idioms3(D,In,New),
      flatten([A,New],NewAfter).
*/
idioms3(D,[A|In],[A|New]):- idioms3(D,In,New).


:-export(swap_person/2).
swap_person(F,R):- idioms(doctor,F,R).

:- export(swap_person/1).
swap_person(P):- swap_person(P,X),writeq(P=X),nl.

:- export(swap_person/0).
swap_person:-  make, swap_persons.
swap_persons:- 
 swap_person([i,am,here]),
 swap_person([u,r,here]),
 swap_person([u,r,me]),
 swap_person([m,i,u,?]),
 swap_person('I cant?'),
 swap_person('Me myself and I.'),
 swap_person('my self and I.'),
 swap_person('I cannot?'),
 swap_person("I've and you've seen it"),
 swap_person(['I','cant',?]),
 swap_person(['I','can\'t',?]),
 swap_person(['I','can','t',?]),
 swap_person([i,can,'\'t']),
 swap_person([i,can,'\'','t']),
 swap_person('I am there?'),!.

%?-   swap_person('I am here',X),writeq(X).
%?-   swap_person('I am here',X),writeq(X).
nc_equal_s(A,B):-( ( \+ atom_or_string(A));( \+ atom_or_string(B))),!,A=B.
nc_equal_s(A,B):-upcase_atom(A,A1),upcase_atom(B,B1),B1=A1.
% nc_equal_s(A0,B0):- string_equal_ci(A0,B0),!.
%nc_equal_s(A0,B0):- as_nc_str(A0,AR),as_nc_str(B0,BR),!, AR = BR.

wappend_ci([], L, L).
wappend_ci([H|T], L, [HA|R]) :- freeze(H,freeze(HA,string_equal_ci(H,HA))),
    wappend_ci(T, L, R).


decontract([],_):-!,fail.
decontract([H|T],[H|T]):-!.
decontract(im,[i,am]):-!.
decontract(youre,[you,are]):-!.
decontract(H,[H]):-!.


listify_text(B0,B):- is_list(B0),!,B=B0.
listify_text(B0,B):- [B]=B0.

dictionary_access(D,B,A):-dictionary(D,B,A).
/*
dictionary_access(input,B,A):-dict(input,B,A).
dictionary_accessCyc(D,B,A):-
   assertion_holds('isa',Dict,'DictionaryPredicate'),
   atom_concat('dictionary_',D,Dict),
   Call =.. [Dict,DB,DA],Call,
   balanceBinding(DB=DA,B=A).
*/ 


explitVocab(so,satelites).
explitVocab(wonderful,interjects).
explitVocab(wonderfull,interjects).
explitVocab(great,interjects).
explitVocab(absolutely,interjects).
explitVocab(perfect,interjects).
explitVocab(no,interjects).
explitVocab(hi,interjects).
explitVocab(hello,interjects).
explitVocab(please,interjects).
explitVocab(thank,interjects).
explitVocab(thanks,interjects).
explitVocab(goodbye,interjects).
explitVocab(bye,interjects).
explitVocab(brb,interjects).
explitVocab(ttyl,interjects).
explitVocab(project,noun).
explitVocab(huh,interjects).

:-dynamic(dictionary/3).



dictionary(contractions,[cannot],[can,not]).
dictionary(contractions,[Can,'\'',T],[Can2,Not2]):- dict_c([Can,*,T],[Can2,Not2]).
dictionary(contractions,[CanT],[Can2,Not2]):- dict_c([Can,*,T],[Can2,Not2]),atomic_list_concat([Can,'\'',T],CanT).
dictionary(contractions,[Can,'\'',T],[Can,Not2]):- dict_c([I,*,T],[I,Not2]).
dictionary(contractions,[Cant],[Can2,Not2]):- dict_c([Can,*,T],[Can2,Not2]),atom_concat(Can,T,Cant).
dictionary(contractions,[AposT],['\'',T]):-dict_c([_,*,T],_),
  freeze(AposT,(atom_or_string(AposT),atom_concat('\'',T,AposT))).

dict_c([can,*,t],[can,not]).
dict_c([couldn,*,t],[could,not]).
dict_c([didn,*,t],[did,not]).
dict_c([don,*,t],[do,not]).
dict_c([i,*,ll],[i,will]).
dict_c([i,*,d],[i,would]).
dict_c([i,*,m],[i,am]).
dict_c([i,*,ve],[i,have]).
dict_c([isn,*,t],[is,not]).
dict_c([shal,*,t],[shall,not]).
dict_c([shan,*,t],[should,not]).
dict_c([shouldn,*,t],[should,not]).
dict_c([that,*,s],[that,is]).
dict_c([what,*,s],[what,is]).
dict_c([ain,*,t],[are,not]).
dict_c([won,*,t],[will,not]).
dict_c([you,*,re],[you,are]).
%dictionary(contractions,['Let',me,_],[yes]).
%dictionary(contractions,[let,me,_],[yes]).
%dictionary(contractions,['Lets',_],[yes]).
%dictionary(contractions,['lets',_],[yes]).

%dictionary(contractions,u,you).
%swap_person(them,'im',youre).



dictionary(swap_person,i,you).
dictionary(swap_person,me,you).
dictionary(swap_person,Me,You):- swap_person0(self,Me,You).
dictionary(swap_person,You,Me):- swap_person0(self,Me,You).

swap_person0(Who,[You,Are],[I,Am]):- swap_person1(Who,[Are,You],[Am,I]).
swap_person0(Who,Me,You):- swap_person1(Who,Me,You).

swap_person1(self,[you,are],[i,am]).
swap_person1(self,yourself,myself).
swap_person1(self,yours,mine).
swap_person1(self,your,my).
swap_person1(self,there,here).
swap_person1(self,mine,yours).
%swap_person(them,we,you).
%swap_person(them,them,they).
%swap_person(them,belong,belonging).
%swap_person(them,would,will).
%swap_person(them,were,was).
%swap_person(them,us,you).

%swap_person(them,his,my).
%swap_person(them,him,me).
%swap_person(them,he,i).
%swap_person(them,['he',is],[i,am]).

dictionary(slang,[precieved],[percieved]).
dictionary(slang,yur,your).
dictionary(slang,yuor,your).
dictionary(slang,wuv,love).
dictionary(slang,wut,what).
dictionary(slang,wazzup,hello).
dictionary(slang,wazup,hello).
dictionary(slang,waht,what).
dictionary(slang,teh,the).
dictionary(slang,m,am).
dictionary(slang,tard,retard).
dictionary(slang,sup,hello).
dictionary(slang,r,are).
dictionary(slang,c,see).
dictionary(slang,u,you).
dictionary(slang,neph,nephrael).
dictionary(slang,mum,mother).
dictionary(slang,mommy,mother).
dictionary(slang,momma,mother).
dictionary(slang,mom,mother).
dictionary(slang,mebbe,maybe).
dictionary(slang,mamma,mother).
dictionary(slang,luv,love).
dictionary(slang,lubbin,lubbing).
dictionary(slang,lub,love).
dictionary(slang,k,ok).
dictionary(slang,hijm,him).
dictionary(slang,hi,hello).
dictionary(slang,greetings,hello).
dictionary(slang,daddy,father).
dictionary(slang,dadda,father).
dictionary(slang,dad,father).
dictionary(slang,cuz,because).
dictionary(slang,cuase,because).
dictionary(slang,cause,because).
dictionary(slang,casue,because).
dictionary(slang,bith,bitch).
dictionary(slang,becasue,because).
dictionary(slang,[you,know],[yes]).
dictionary(slang,[wtf],[what,the,fuck]).
dictionary(slang,[whats],[what,is]).
dictionary(slang,[whats,up],[hello]).
dictionary(slang,[what,is,up],[hello]).
dictionary(slang,[wanna],[want,to]).
dictionary(slang,[the,bot],[me]).
dictionary(irc,ignore_word,'jllykifsh').
dictionary(irc,ignore_word,'jlly').
dictionary(irc,ignore_word,'jll').
dictionary(irc,ignore_word,'jellyfish').
dictionary(doctor,yup,yes).
dictionary(doctor,yep,yes).
dictionary(doctor,yeah,yes).
dictionary(doctor,would,will).
dictionary(doctor,when,when).
dictionary(doctor,whatever,insult).
dictionary(doctor,what,what).
dictionary(doctor,ok,ok).
dictionary(doctor,nobody,everyone).
dictionary(doctor,maybe,perhaps).
dictionary(doctor,machines,computer).
dictionary(doctor,machine,computer).
dictionary(doctor,how,how).
dictionary(doctor,francais,xfremd).
dictionary(doctor,fantasy,dreamt).
dictionary(doctor,fantasies,dreamt).
dictionary(doctor,everybody,everyone).
dictionary(doctor,espanol,xfremd).
dictionary(doctor,dreams,dream).
dictionary(doctor,dreamed,dreamt).

%dictionary(doctor,different,dit).
%dictionary(doctor,dit,same).
%dictionary(doctor,opposite,dit).
%dictionary(doctor,alike,dit).

dictionary(doctor,deutsch,xfremd).
dictionary(doctor,computers,computer).
dictionary(doctor,certainly,yes).
/*
*/
%swap_person(them,are,am).
%swap_person(them,am,are).
%dictionary(mud,X,Y):-catch(mud_dictionary(X,Y),_,fail).
%dictionary(irc,ignore_word,prolog).
%dictionary(doctor,X,Y):-dictionary(swap_person,X,Y).
% dictionary(slang,re,hello).

%dictionary(slang,X,Y):-isa_mem(X,is,Y),rnd(25).


  
rememberDictionary:-
      dictionary(X,Y,Z),
      rememberDictionary(X,Y,Z),
      fail.
rememberDictionary:-!.

rememberDictionary(X,Y,Z):-
      concat_atom(['dictionary_',X],PredHum),
      rememberDictionary2(PredHum,Y,Z),!.

rememberDictionary2(PredHum,Y,Z):-
      atomic(Y),!,rememberDictionary2(PredHum,[Y],[Z]).


rememberDictionary2(PredHum,Y,Z):-
   makeConstant(PredHum),
   cycAssert(isa(PredHum,'DictionaryPredicate'),'ElizaMt'),
   cycAssert(isa(PredHum,'BinaryPredicate'),'ElizaMt'),
   Cyc =.. [PredHum,string(Y),string(Z)],
   cycAssert(Cyc,'ElizaMt'),!.


