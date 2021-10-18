% ===================================================================
% File 'logicmoo_module_aiml_main.pl'
% Purpose: To load and test the AIML interpretor (sanity checks)
% Maintainers: Douglas Miles/Annie Ogborn/Kino Coursey
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'logicmoo_module_aiml_main.pl' 1.0.0
% Revision:  $Revision: 1.7 $
% Revised At:   $Date: 2002/07/11 21:57:28 $
% ===================================================================


%:- use_module(library(programk/cyc_pl/cyc),
%  [is_string/1,atom_to_number/2,balanceBinding/2,writeFmtFlushed/2,writeFmtFlushed/3,toCycApiExpression/3]).



% ===============================================================================================
% Split input into many words
% ===============================================================================================
toString_atom(Input,Atom):-toCycApiExpression(Input,Out,[]),atrace,string_to_atom(Atom,Out).

tokenizeInput(Input,Tokens):- notrace(is_string(Input)),toString_atom(Input,Atom),!,tokenizeInput(Atom,Tokens),!.
%%tokenizeInput(String,Tokens):-hotrace(tokenizeInput0(String,Tokens)),!.
tokenizeInput(String,Tokens):-hotrace(tokenizeInput0(String,Tokens)),Tokens\==[],ground(Tokens),!.
tokenizeInput(String,Tokens):-tokenizeInput0(String,Tokens),!.

tokenizeInput0(Input,Tokens):-var(Input),!,Input=Tokens.
tokenizeInput0([],[]):-!.
tokenizeInput0([C0,C1|Odes],Tokens):- integer(C0),integer(C1),name(Input,[C0,C1|Odes]),!,tokenizeInput0(Input,Tokens).
tokenizeInput0(Input,Tokens):- string(Input),string_to_atom(Input,Atom),!,tokenizeInput1(Atom,Tokens).
tokenizeInput0([A|B],Out):- tokenizeInput0(A,AA),tokenizeInput_l(B,BB),!,flatten([AA,BB],Out).
tokenizeInput0(Input,Tokens):- atom(Input),tokenizeInput1(Input,Tokens),!.
tokenizeInput0(Input,Tokens):- number(Input),atom_to_number(Tokens,Input),!.
tokenizeInput0(Input,Tokens):- notrace(is_string(Input)),toString_atom(Input,Atom),!,tokenizeInput0(Atom,Tokens).
tokenizeInput0(Compound,Out):-Compound=..[A|B],tokenizeInput_l(B,BB),!,Out=..[A|BB].
tokenizeInput0(A,A).


tokenizeInput_l(A,A):-atomic(A),!.
tokenizeInput_l([A|B],[AA|BB]):- tokenizeInput0(A,AA),tokenizeInput_l(B,BB),!.

tokenizeInput1(Input,Tokens):-  notrace(error_catch(((atom_to_term(Input,Tokens,Vars),ground(Vars))),_,fail)),!.
tokenizeInput1(Input,Tokens):-  (atom_contains(Input,'<');atom_contains(Input,'&')),
     error_catch((string_to_structure(Input,Tokens0),(Tokens0\==[Input]->tokenizeInput0(Tokens0,Tokens);Tokens0=Tokens)),_,fail),!.
tokenizeInput1(Input,Tokens):- atomWSplit(Input,Tokens0), (Tokens0\==[Input]->tokenizeInput0(Tokens0,Tokens);Tokens0=Tokens),!.
tokenizeInput1(Input,Input).


% ===============================================================================================
% Join input into many words
% ===============================================================================================
atomify(A,A):-var(A),!.
%%%atomify(A,A):-atomic(A),!.
atomify([A],A):-atom(A),!.
atomify(A,A):-atomic(A),!.
atomify(A,AA):-number(A),atom_to_number(AA,A),!.
atomify([A],AA):-atomify(A,AA),!.
atomify([A|List],Result):-joinAtoms([A|List],' ',Result).

joinAtoms([],_,'').
joinAtoms(List,Sep,Result):-atomify(Sep,SepA),Sep\==SepA,!,joinAtoms(List,SepA,Result).
joinAtoms(List,Sep,Result):-hotrace(prolog_must(joinAtoms0(List,Sep,Result))),!.

joinAtoms0([],_,'').
joinAtoms0([A],_,AA):-atomify(A,AA).
joinAtoms0([A|More],Sep,Result):-atomify(A,AA),A\==AA,!,joinAtoms0([AA|More],Sep,Result).
joinAtoms0([A|List],Sep,Result):-prolog_must(atomic(A)),joinAtoms1([A|List],Sep,Result).

joinAtoms1([],_,'').
joinAtoms1([A],_,AA):-atomify(A,AA).
%%%%joinAtoms1([A,'\b',B|List],Sep,Result):-!,joinAtoms0([B|List],Sep,ResultR),atom_concat(A,ResultR,Result).
joinAtoms1([A,'\b',B|List],Sep,Result):-!,atomify(B,BB),atom_concat(A,BB,C),!,joinAtoms0([C|List],Sep,Result).
joinAtoms1([A,B|List],'',Result):-!,atom_concat(A,B,C),!,joinAtoms1([C|List],'',Result).
joinAtoms1([A,B|List],Sep,Result):-atom_concat(A,Sep,C),joinAtoms1([B|List],Sep,ResultR),atom_concat(C,ResultR,Result).
joinAtoms1(List,Sep,Result):- debugOnError(atomic_list_concat_aiml(List,Sep,Result)),!.

% ===============================================================================================
% Split input into many sentences
% ===============================================================================================

splitSentences(In,Out):- notrace(splitSentences0(In,Out)). %%,flatten(Out,OutL),traceIf(member(xml,OutL)),!.
splitSentences0([],[]):-!.   
splitSentences0(SR1,[SR0|SRMORE]):-grabFirstSetence(SR1,SR0,LeftOver),!,splitSentences0(LeftOver,SRMORE),!.
splitSentences0(SR1,[SR1]):-!.

splitSentencesOn(Starters,Enders,In,Out):- hotrace(splitSentencesOn0(Starters,Enders,In,Out)).%%%,flatten(Out,OutL),traceIf(member(xml,OutL)),!.
splitSentencesOn0(_Starters,_Enders,[],[]):-!.   
splitSentencesOn0(Starters,Enders,SR1,[SR0|SRMORE]):-grabFirstSetenceOn(Starters,Enders,SR1,SR0,LeftOver),!,splitSentencesOn0(Starters,Enders,LeftOver,SRMORE),!.
splitSentencesOn0(_Starters,_Enders,SR1,[SR1]):-!.

grabFirstSetence(SR1,SRS,LeftOver):-LeftSide=[_|_],append(LeftSide,[EOS|LeftOver],SR1),sentenceBreakChar(EOS),validSentenceEnder([EOS|LeftOver]),append(LeftSide,[EOS],SR0),cleanSentence(SR0,SRS),!.
cleanSentence(SR0,SRSOutput):-prolog_must(leftTrim(SR0,sentenceEnderOrPunct,SRS)),!,rightTrim(SRS,sentenceEnderOrPunct_NoQuestion,SRSOut),trimWhitepaceOffEnds(SRSOut,SRSOutput).

trimWhitepaceOffEnds(SRSOut,SRSOutput):-leftTrim(SRSOut,isWhiteWord,SRS),rightTrim(SRS,isWhiteWord,SRSOutput).

validSentenceEnder(['.',xml|_]):-!,fail.
validSentenceEnder(['.','\b'|_]):-!,fail.
validSentenceEnder(_).

grabFirstSetenceOn(Starters,Enders,SR1,SRS,LeftOver):-LeftSide=[_|_],append(LeftSide,[EOS|LeftOver],SR1),
    ((member(EOS,Enders)->append(LeftSide,[EOS],SR0));
    (member(EOS,Starters)->append(LeftSide,[],SR0));
    fail),cleanSentence(SR0,SRS),!.

% ===============================================================================================
% Convert to Matchable
% ===============================================================================================

convertToMatchableCS(That,Words):-
      answerOutput(That,AA),!,
      deleteAll(AA,['.','!','?','\'','!','','\b',' ','\n',',','\r\n','\n\n'],Words),!.

convertToMatchable(That,LastSaid):-
      convertToMatchableCS(That,Words),!,
      ignorecase_literal(Words,LastSaid),!.



literal_atom_safe(A,B):-atom(A),literal_atom(A,B),!.
literal_atom(A,B):-downcase_atom(A,B),!.
is_literal(X):-atom(X),literal_atom(X,N),!,N=X.

ignorecase_literal(A,B):-literal_atom_safe(A,B),!.
ignorecase_literal(A,B):-number(A),atom_number(N,A),literal_atom_safe(N,B),!.
ignorecase_literal(A,B):-toLowercase(A,B),!.


% ===============================================================================================
% Create Talk Generation Paths
% ===============================================================================================
/*

generally we have these types of transformers


convert_ele (loading)
outputPaths (generate possible grounded outputs)
computeAnswer (really generate output at runtime)

?-  tell(allSaid),allSaid(That,Aiml),'format'('~q.   %%  ~q. ~n',[That,Aiml]),fail.


%% create transitions between <template> and <that> 
tell('allSaid1.pl'),allSaid(That),'format'('oneSaid(~q).~n',[That]),fail. told.
tell('allThat1.pl'),allThats(That),'format'('oneThat(~q).~n',[That]),fail. told.

*/
%%:-['temp/allThat1.pl'],['temp/allSaid1.pl'].

findThatMatch(Aiml1,Aiml2):-allThats(Sarg,Aiml1),allSaid(Path,Aiml2),sameBindingIC(Sarg,Path).

allThats(That):- findall(Sarg,(aimlCateArg(that,_Aiml,Arg),Arg\=(*),fromIndexableSArg(Arg,Sarg)),List),sort(List,Set),member(That,Set).
allThats(That,Aiml):- findall(Sarg=Aiml,(aimlCateArg(that,Aiml,Arg),Arg\=(*),fromIndexableSArg(Arg,Sarg)),List),sort(List,Set),member(That=Aiml,Set).

allSaid(That):- findall(Sarg,(aimlCateArg(template,_Aiml,Arg),outputPath(_Ctx,Arg,Sarg)),List),sort(List,Set),member(That,Set).
allSaid(That,Aiml):- findall(Sarg=Aiml,(aimlCateArg(template,Aiml,Arg),outputPath(_Ctx,Arg,Sarg)),List),sort(List,Set),member(That=Aiml,Set).


%%?- length(Text,T1),(T1<100->true;!),oneThat(Pattern),oneSaid(Text),starMatch(Pattern,Text,StarSets),'format'('~q.~n',[arMatch(Pattern,Text)]),fail.


%%:-dynamic(inGenOutput).
%%inGenOutput.·

outputPath(Ctx,Input,Path):-!, setup_call_cleanup(assert(inGenOutput),computeInnerTemplate(Ctx,1,Input,Path,_VotesO),retractall(inGenOutput)).


outputPath(Ctx,Input,Path):- 
   withAttributes(Ctx,[generateUnknownVars=true,generateTemplate=true],
    (computeInnerTemplate(Ctx,1,Input,Path,_VotesO))).


