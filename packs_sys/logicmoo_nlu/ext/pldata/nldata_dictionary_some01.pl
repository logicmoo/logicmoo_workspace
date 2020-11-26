/****************************************************************************/
% idioms(+Dictionay,+List,-Result)
%   implements non-overlapping idioms
%   idiomizes List into Result

:- module(nldata_dictionary_some01,[rememberDictionary2/3, idioms/3, rememberDictionary/0, translatePersonal/3, translatePersonalUTokens/2, rememberDictionary/3, explitVocab/2, to_codes/2, decontract/2, dictionary/3, dictionary_access/3, idioms_each/3, idioms2/3, isa_speechpart/1, lexicicalRefWord/2, translatePersonalCodes3/2, lexicicalRefWords/2, translatePersonalCodes2/2, translatePersonalCodes/2, my_lexicalRef/4]).

:- dynamic((rememberDictionary2/3, idioms/3, rememberDictionary/0, translatePersonal/3, translatePersonalUTokens/2, rememberDictionary/3, explitVocab/2, to_codes/2, decontract/2, dictionary/3, dictionary_access/3, idioms_each/3, idioms2/3, isa_speechpart/1, lexicicalRefWord/2, translatePersonalCodes3/2, lexicicalRefWords/2, translatePersonalCodes2/2, translatePersonalCodes/2, my_lexicalRef/4)).

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

translatePersonalCodes(Codes,Output):-
   translatePersonalCodes2(Codes,M),
   translatePersonalCodes3(M,Output).

%translatePersonalCodes3(M,Output):-append(Output,['?'],M),!.
translatePersonalCodes3(M,M):-!.


translatePersonalCodes2(Codes,Output):-
	 atom_codes(Atom,Codes),
	atomSplit(Atom,Words),
	%writeSTDERR(atomSplit(Atom,Words)),
	idioms(doctor,Words,Output),!.
	

% ========================================================================================
% ========================================================================================
:-export(idioms/3).

idioms(doctor,F,R):-!,
      idioms_each(slang,F,S),
      idioms_each(contractions,S,C),
      idioms_each(mud,C,M),
      idioms_each(irc,M,I),!,
      idioms_each(swap_person,I,P),
      idioms_each(doctor ,P,R).

idioms(e2c,A,A):-!.
idioms(e2c,A,Z):-!,idioms_each(contractions,A,Z),!.
idioms(e2c,A,Z):-!,
      idioms_each(contractions,A,D),      
      idioms_each(mud,D,H),
      idioms_each(irc,H,J),!,
      idioms_each(slang,J,Z),!.

idioms(D,F,R):-
      idioms_each(D,F,R).

idioms_each(_,[],[]):-!.
idioms_each(D,F,R):-ignore(D=doctor),!,idioms2(D,F,R).

idioms2(_,[],[]):-!.

idioms2(D,[Ignore|In],Out):-
	dictionary(D,ignore_word,Ignore),!,
	idioms2(D,In,Out).
	
idioms2(D,[Ignore|In],Out):-
	dictionary(_,ignore_word,Ignore),!,
	idioms2(D,In,Out).
	
idioms2(D,[F|In],[R|New]):-
      dictionary_access(D,F,R),!,
      idioms2(D,In,New).

idioms2(D,In,New):-
	dictionary_access(D,This,Replace),
        stringToWords(This,ThisText),
        stringToWords(Replace,ReplaceText),
	append_ci(ThisText,After,In),!,
	idioms2(D,After,NewAfter),!,
	flatten([[ReplaceText,NewAfter]],New),!.


idioms2(D,[A|In],[A|New]):-
      idioms2(D,In,New).




decontract([],_):-!,fail.
decontract([H|T],[H|T]):-!.
decontract(im,[i,am]):-!.
decontract(youre,[you,are]):-!.
decontract(H,[H]):-!.


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


dictionary(contractions,['I','\'','m'],['I',am]).
dictionary(contractions,[can,'\'',t],[can,not]).
dictionary(contractions,[cannot],[can,not]).
dictionary(contractions,[cant],[can,not]).
dictionary(contractions,[couldn,'\'',t],[could,not]).
dictionary(contractions,[couldnt],[could,not]).
dictionary(contractions,[didn,'\'',t],[did,not]).
dictionary(contractions,[didnt],[did,not]).
dictionary(contractions,[don,'\'',t],[do,not]).
dictionary(contractions,[dont],[do,not]).
dictionary(contractions,[isn,'\'',t],[is,not]).
dictionary(contractions,[isnt],[is,not]).
dictionary(contractions,[shant],[should,not]).
dictionary(contractions,[shouldn,'\'',t],[should,not]).
dictionary(contractions,[shouldnt],[should,not]).
% dictionary(contractions,[will,not],[wont]).
dictionary(contractions,[won,'\'',t],[will,not]).
dictionary(contractions,[wont],[will,not]).
dictionary(contractions,youre,[you,are]).

%dictionary(contractions,['Let',me,_],[yes]).
%dictionary(contractions,[let,me,_],[yes]).
%dictionary(contractions,['Lets',_],[yes]).
%dictionary(contractions,['lets',_],[yes]).
dictionary(contractions,['thats'],['that','is']).



dictionary(slang,[precieved],[percieved]).

dictionary(swap_person,[you,are],[im]).
dictionary(swap_person,[i,am],[youre]).
dictionary(swap_person,[u,are],[im]).
dictionary(swap_person,['ill'],[you,will]).
dictionary(swap_person,['ive'],[you,have]).
dictionary(swap_person,your,my).
dictionary(swap_person,yours,mine).
dictionary(swap_person,there,here).
dictionary(swap_person,here,there).
dictionary(swap_person,were,was).
dictionary(swap_person,me,you).
dictionary(swap_person,myself,yourself).
dictionary(swap_person,yourself,myself).
dictionary(swap_person,'i',you).
dictionary(swap_person,'I',you).
dictionary(swap_person,'im',youre).
dictionary(swap_person,'youre',im).
dictionary(swap_person,am,are).
dictionary(swap_person,would,will).
dictionary(swap_person,i,you).
dictionary(swap_person,you,i).
dictionary(swap_person,u,i).
dictionary(swap_person,my,your).
dictionary(swap_person,mine,yours).
dictionary(swap_person,us,you).
dictionary(swap_person,we,you).
dictionary(swap_person,them,they).
dictionary(swap_person,belong,belonging).

dictionary(swap_person,he,i).
dictionary(swap_person,[hes],[i,am]).
dictionary(swap_person,['Hes'],[i,am]).
dictionary(swap_person,'He',i).

dictionary(swap_person,['he',is],[i,am]).
dictionary(swap_person,his,my).
dictionary(swap_person,him,me).

dictionary(slang,r,are).
dictionary(slang,[wanna],[want,to]).

dictionary(slang,sup,hello).
dictionary(slang,hijm,him).

dictionary(slang,yur,your).
dictionary(slang,yuor,your).
dictionary(slang,wazzup,hello).
dictionary(slang,wazup,hello).
dictionary(slang,greetings,hello).
dictionary(slang,[whats,up],[hello]).
dictionary(slang,[you,know],[yes]).
dictionary(slang,[whats],[what,is]).
dictionary(slang,mom,mother).
dictionary(slang,mum,mother).
dictionary(slang,mommy,mother).
dictionary(slang,momma,mother).
dictionary(slang,mamma,mother).
dictionary(slang,dad,father).
dictionary(slang,daddy,father).
dictionary(slang,dadda,father).

dictionary(slang,waht,what).
dictionary(slang,wut,what).
dictionary(slang,[you,know],[yes]).
dictionary(slang,teh,the).
dictionary(slang,k,ok).
% dictionary(slang,re,hello).
dictionary(slang,hi,hello).
dictionary(slang,cause,because).
dictionary(slang,casue,because).
dictionary(slang,cuase,because).
dictionary(slang,cuz,because).
dictionary(slang,luv,love).
dictionary(slang,wuv,love).
dictionary(slang,lub,love).
dictionary(slang,lubbin,lubbing).
dictionary(slang,becasue,because).
dictionary(slang,mebbe,maybe).


%dictionary(mud,X,Y):-catch(mud_dictionary(X,Y),_,fail).

dictionary(doctor,dreamed,dreamt).
dictionary(doctor,fantasy,dreamt).
dictionary(doctor,fantasies,dreamt).
dictionary(doctor,dreams,dream).
dictionary(doctor,how,how).
dictionary(doctor,what,what).
dictionary(doctor,when,when).
dictionary(doctor,alike,dit).
dictionary(doctor,different,dit).
dictionary(doctor,same,dit).
dictionary(doctor,opposite,dit).
dictionary(doctor,certainly,yes).
dictionary(doctor,ok,ok).
dictionary(doctor,yep,yes).
dictionary(doctor,yup,yes).
dictionary(doctor,yeah,yes).
dictionary(doctor,maybe,perhaps).
dictionary(doctor,deutsch,xfremd).
dictionary(doctor,francais,xfremd).
dictionary(doctor,espanol,xfremd).
dictionary(doctor,machine,computer).
dictionary(doctor,machines,computer).
dictionary(doctor,computers,computer).
dictionary(doctor,whatever,insult).
dictionary(doctor,everybody,everyone).
dictionary(doctor,nobody,everyone).
dictionary(doctor,would,will).

%dictionary(doctor,X,Y):-dictionary(swap_person,X,Y).

dictionary(swap_person,[you,are],[im]).
dictionary(swap_person,[i,am],[youre]).
dictionary(swap_person,[u,are],[im]).
dictionary(swap_person,['ill'],[you,will]).
dictionary(swap_person,['ive'],[you,have]).
dictionary(swap_person,your,my).
dictionary(swap_person,yours,mine).
dictionary(swap_person,there,here).
dictionary(swap_person,here,there).
dictionary(swap_person,were,was).
dictionary(swap_person,me,you).
dictionary(swap_person,myself,yourself).
dictionary(swap_person,yourself,myself).
dictionary(swap_person,'i',you).
dictionary(swap_person,'I',you).
dictionary(swap_person,'im',youre).
dictionary(swap_person,'youre',im).
dictionary(swap_person,am,are).
dictionary(swap_person,are,am).
dictionary(swap_person,would,will).
dictionary(swap_person,i,you).
dictionary(swap_person,you,i).
dictionary(swap_person,u,i).
dictionary(swap_person,my,your).
dictionary(swap_person,mine,yours).
dictionary(swap_person,us,you).
dictionary(swap_person,we,you).
dictionary(swap_person,them,they).
dictionary(swap_person,belong,belonging).

dictionary(swap_person,he,i).
dictionary(swap_person,[hes],[i,am]).
dictionary(swap_person,['Hes'],[i,am]).
dictionary(swap_person,'He',i).

dictionary(swap_person,['he',is],[i,am]).
dictionary(swap_person,his,my).
dictionary(swap_person,him,me).
%dictionary(irc,ignore_word,prolog).
dictionary(irc,ignore_word,'jllykifsh').
dictionary(irc,ignore_word,'jellyfish').
dictionary(irc,ignore_word,'jlly').
dictionary(irc,ignore_word,'jll').

dictionary(slang,r,are).
dictionary(slang,[wanna],[want,to]).

dictionary(slang,sup,hello).
dictionary(slang,hijm,him).

dictionary(slang,yur,your).
dictionary(slang,[the,bot],[me]).
dictionary(slang,bith,bitch).
dictionary(slang,yuor,your).
dictionary(slang,wazzup,hello).
dictionary(slang,wazup,hello).
dictionary(slang,greetings,hello).
dictionary(slang,[whats,up],[hello]).
dictionary(slang,[what,is,up],[hello]).
dictionary(slang,[whats],[what,is]).
dictionary(slang,mom,mother).
dictionary(slang,mum,mother).
dictionary(slang,mommy,mother).
dictionary(slang,momma,mother).
dictionary(slang,mamma,mother).
dictionary(slang,dad,father).
dictionary(slang,daddy,father).
dictionary(slang,dadda,father).

dictionary(slang,waht,what).
dictionary(slang,wut,what).
dictionary(slang,[wtf],[what,the,fuck]).
dictionary(slang,teh,the).
dictionary(slang,k,ok).
% dictionary(slang,re,hello).
dictionary(slang,hi,hello).
dictionary(slang,tard,retard).
dictionary(slang,neph,nephrael).
dictionary(slang,cause,because).
dictionary(slang,casue,because).
dictionary(slang,cuase,because).
dictionary(slang,cuz,because).
dictionary(slang,luv,love).
dictionary(slang,wuv,love).
dictionary(slang,lub,love).
dictionary(slang,lubbin,lubbing).
dictionary(slang,becasue,because).
dictionary(slang,mebbe,maybe).
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


