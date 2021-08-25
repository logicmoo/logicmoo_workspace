module(sigma_reader,[readKIF/2,readKIF/1,readKIF_priv/1,readKIF_priv/2]).

% TODO make reader more robust

:-dynamic reading_in_comment/0.
:-dynamic reading_in_string/0.
:-dynamic read_in_atom/0.
:-dynamic prev_char/1.

readKIF(CHARS)  :-       !,
         readKIF(user_input,CHARS).


readKIF(Stream,[])  :-at_end_of_stream(Stream).     
readKIF(Stream,CHARS)  :-
		kifReadStatePopParens,!,
		retractAllProlog(reading_in_comment),
		retractAllProlog(reading_in_string),!,
		once(readKIFChars_p0(Stream,CHARS)),!.

readKIF_priv(Stream,[])  :-at_end_of_stream(Stream).     
readKIF_priv(Stream,CHARS)  :-  
		kifReadStatePopParens,!,
		unset_g(reading_in_comment),
		unset_g(reading_in_string),!, %trace,
		call_with_depth_limit(readKIFChars_p0_priv(Stream,CHARS),40000,_),!.

readKIFChars_p0_priv(Stream,[]):-at_end_of_stream(Stream),!.
readKIFChars_p0_priv(Stream,[Char|Chars]):- !,
        logOnFailure(peekKIFCharCode(Stream,C)),!,
	logOnFailure(term_to_atom(C,CS)),
	logOnFailure(kifUpdateReadState(CS)),!,
	(readKIFChars_next(C,Char,Stream,Chars)),!.
	
%peekKIFCharCode(Stream,10):-at_end_of_stream(Stream),!,dw('[at_end_of_stream]').
peekKIFCharCode(Stream,10):-peek_byte(Stream,13),!,skipKIFChar(Stream),dw('[ln]'),!.
peekKIFCharCode(Stream,10):-peek_byte(Stream,10),!,skipKIFChar(Stream),dw('[ln]'),!.
peekKIFCharCode(Stream,46):-peek_byte(Stream,46),!,skipKIFChar(Stream),dw('[dot]'),!.
peekKIFCharCode(Stream,32):-peek_byte(Stream,C),C < 32,!,skipKIFChar(Stream),dw('[ctl]'),!.
peekKIFCharCode(Stream,C):-peek_byte(Stream,38),!,skipKIFChar(Stream),dw('[skipping]'),peekKIFCharCode(Stream,C),!.
peekKIFCharCode(Stream,C):-peek_byte(Stream,46),!,skipKIFChar(Stream),dw('[skip-dot]'),peekKIFCharCode(Stream,C),!.
peekKIFCharCode(Stream,C):-peek_byte(Stream,37),!,skipKIFChar(Stream),dw('[skipping]'),peekKIFCharCode(Stream,C),!.
peekKIFCharCode(Stream,C):-peek_byte(Stream,C),skipKIFChar(Stream),!. %,put(C),!.
peekKIFCharCode(Stream,C):-peek_byte(Stream,C),flush,dw('[peekKIFCharCode]'),sleep(2),!,peekKIFCharCode(Stream,C),!.

readKIFChars_next(C,C,Stream,Chars):-if_g(reading_in_string),readKIFChars_p0_priv(Stream,Chars),!.
readKIFChars_next(10,10,Stream,[]):-if_g(reading_in_comment),!.
readKIFChars_next(13,10,Stream,[]):-if_g(reading_in_comment),!.
readKIFChars_next(C,C,Stream,Chars):-if_g(reading_in_comment),readKIFChars_p0_priv(Stream,Chars),!.
readKIFChars_next(41,41,Stream,[]):-flag('bracket_depth',X,X),(X=0),!.
readKIFChars_next(C,Char,Stream,Chars):-once(kifAsciiRemap(C,Char)),!,readKIFChars_p0_priv(Stream,Chars),!.
readKIFChars_next(C,Char,Stream,Chars):-dw(errror).

set_g(F):-!,flag(F,_,2),!.
unset_g(F):-!,flag(F,_,1),!.
if_g(F):-!,flag(F,X,X),not(X=1),!.

%end_char(41,41)  :-   sigma_console_bracket_depth(D),D<1,!. 

kifUpdateReadState('46'):-dw('[dotp]'),!.
kifUpdateReadState('32'):-!.
kifUpdateReadState(_):- if_g(reading_in_comment),dw('[;]'),!.
kifUpdateReadState('34'):-!,
		(if_g(reading_in_string) -> (dw('[strout]'),unset_g(reading_in_string));(set_g(reading_in_string),!,dw('[strin]'))),!.
kifUpdateReadState('46'):-if_g(reading_in_string),dw('='),!.
kifUpdateReadState(_):-if_g(reading_in_string),dw('='),!.
kifUpdateReadState('59'):- set_g(reading_in_comment),dw('[commentStart]'),!.

kifUpdateReadState('40'):-!,logOnFailure(flag('bracket_depth',N,N)),dw(n(N)),logOnFailure(flag('bracket_depth',N,N + 1)),logOnFailure((V is N +1)),logOnFailure(dw([brackin:V])),!.
kifUpdateReadState('41'):-!,flag('bracket_depth',N,N - 1),dw([brackout:N]),!.
kifUpdateReadState(_):-!. %dw('-'),!.

:-dynamic(bd/1).

%bdInc:-

dw(W):-flush_output,!. %write(W),flush. %,flush(user_error).

kifReadStatePopParens:-flag('bracket_depth',_,0),!,dw(newbd).

skipKIFChar(Stream):- at_end_of_stream(Stream),!.
skipKIFChar(Stream):- logOnFailure(get_char(Stream,_)),!.
/*
		stream_property(Stream,position('$stream_position'(PCharIndex, PLineNo, PLinePos))),
		NCharIndex is PCharIndex +1,
		seek(Stream,NCharIndex, bof, CharIndex),
		ignore(check_same(NCharIndex,CharIndex)).
*/

check_same(NCharIndex,CharIndex):-NCharIndex == CharIndex,!.
check_same(NCharIndex,CharIndex):-dw('!@#$%@#!@'),dw((NCharIndex,CharIndex)).


getSurfaceFromChars_d(Chars,WFFOut,VARSOut):- 
    retractAllProlog(var_counter(_)),retractAllProlog(numbered_var(_,_,_)),asserta(var_counter(0)), 
               (getKIFTokens(Chars,Tokens) -> true ; (sendNote(user,kifParser,'Syntax Error (or I need more work)',Chars),sleep(2),fail)),
               logOnFailure(clean_sexpression(Tokens,WFFClean)),
               logOnFailure(phrase(expr(WFF),WFFClean)),
               collect_temp_vars(VARS),
              !, ( 
                     (VARS=[],VARSOut=_,WFFOut=WFF)
               ;
                     (
                     unnumbervars(VARS,LIST),
                     kifVarNums(LIST,WFF,WFFOut,VARSOut2) ,
                     list_to_set(VARSOut2,VARSOut1),
                     open_list(VARSOut1,VARSOut)
                     ) 
               ).

            




readKIFChars_p0(Stream,[]):-at_end_of_stream(Stream),!.
readKIFChars_p0(Stream,[Char|Chars]):-
        get_code(Stream,C),!,
	kifReadStateChange(C),!,readKIFChars_p1(C,Char,Stream,Chars),!.
	
readKIFChars_p1(C,Char,Stream,[]):-isKIFTerminationStateChar(C,Char),!.
readKIFChars_p1(C,Char,Stream,Chars):-once(kifAsciiRemap(C,Char)),!,readKIFChars_p0(Stream,Chars),!.



isKIFTerminationStateChar(10,32)  :-reading_in_comment,!.
isKIFTerminationStateChar(13,32)  :-reading_in_comment,!.
isKIFTerminationStateChar(41,41)   :-  flag('bracket_depth',X,X),!,(X=0),!.
%isKIFTerminationStateChar(41,41)  :-   sigma_console_bracket_depth(D),D<1,!. 


kifReadStateChange(_):- reading_in_comment,!.
kifReadStateChange(34):-retract(reading_in_string),!.
kifReadStateChange(34):-assert(reading_in_string),!.
kifReadStateChange(_):-reading_in_string,!.
kifReadStateChange(59):- assert(reading_in_comment),!.

kifReadStateChange(40):-!,flag('bracket_depth',N,N + 1).
kifReadStateChange(41):-!,flag('bracket_depth',N,N - 1).
kifReadStateChange(_).

%kifAsciiRemap(X,Y):-(catch(kifAsciiRemap0(X,Y),_,fail)),!.

kifAsciiRemap(X,X).

kifAsciiRemap(N,32):-not(number(N)).
kifAsciiRemap(X,32):-X<32,!.
kifAsciiRemap(X,32):-X>128,!.
kifAsciiRemap(X,X):-!.


isCodesWhite([]).
isCodesWhite([T|W]):-member(T,[32,10,13]),isCodesWhite(W).




