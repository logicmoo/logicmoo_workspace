:- use_module(library(lists),[append/3, prefix/2, member/2]).

%%longNum([X,punkt,XX,punkt,XXX,punkt,XXXX,kolon,PORT],Svar)
longNum([X,punkt,XX,punkt,XXX,punkt,XXXX,kolon,Port],Svar):-
	%format("inne och kollar nummer~n",[]),
	(
	  number_phrase([X],Dig)
	;
	  chk100(X,Dig)
	;
	  chk100plus(X,Dig)
	),
	(
	  number_phrase([XX],Dig2)
	;
	  chk100(XX,Dig2)
		;
	  chk100plus(XX,Dig2)),
	(
	  number_phrase([XXX],Dig3)
	;
	  chk100(XXX,Dig3)
	;
	  chk100plus(XXX,Dig3)),
	(
	  number_phrase([XXXX],Dig4)
	;
	  chk100(XXXX,Dig4)
	;
	  chk100plus(XXXX,Dig4)
	),
	(
	  number_phrase([Port],DigPort)
	;
	  chk100(Port,DigPort)
	;
	  chk100plus(Port,DigPort)
	;
	  chk1000(Port,DigPort)
	),
	fixit([Dig,Dig2,Dig3,Dig4,DigPort],Svar).

%% utan några punkter  >>  lägg till kolon och port
longNum([X,XX,XXX,XXXX,Port],Svar):-
	%format("inne och kollar andras nummer~n",[]),
	(
	  number_phrase([X],Dig)
	;
	  chk100(X,Dig)
	;
	  chk100plus(X,Dig)
	),
	(
	  number_phrase([XX],Dig2)
	;
	  chk100(XX,Dig2)
		;
	  chk100plus(XX,Dig2)),
	(
	  number_phrase([XXX],Dig3)
	;
	  chk100(XXX,Dig3)
	;
	  chk100plus(XXX,Dig3)),
	(
	  number_phrase([XXXX],Dig4)
	;
	  chk100(XXXX,Dig4)
	;
	  chk100plus(XXXX,Dig4)
	),
	(
	  number_phrase([Port],DigPort)
	;
	  chk100(Port,DigPort)
	;
	  chk100plus(Port,DigPort)
	;
	  chk1000(Port,DigPort)
	),
	fixit([Dig,Dig2,Dig3,Dig4,DigPort],Svar).

fixit([D,D2,D3,D4,DigPort],Svar):-
	name(D,DL),
	name(D2,DL2),
	name(D3,DL3),
	name(D4,DL4),
	name(DigPort,DPL),
	pun(X),
	kolon(Kolon),
	append(DL,X,First),
	append(First,DL2,FirstD2),
	append(FirstD2,X,Second),
	append(Second,DL3,SecondD3),
	append(SecondD3,X,Third),
	append(Third,DL4,ThirdD3),
	append(ThirdD3,Kolon,Fourth),
	append(Fourth,DPL,Last),
	name(Svar,Last).

pun(X):-
	name(.,X).
kolon(X):-
	name(:,X).

first9([ett,två,tre,fyra,fem,sex,sju,åtta,nio]).

%% etthundratre
%% etthundratjugonio
chk100plus(BigAtom,DigAtom):-
	atomic(BigAtom),
 	name(BigAtom,BigAtomList),%%etthundratre
 	append(Small,Rest,BigAtomList),
 	name(S,Small),
	(
	  first9(First9),
 	  member(S,First9)
 	->
	  name(hundra,Hun),
	  prefix(Hun,Rest),
	  append(Hun,Some,Rest), %%hundra,nåt,hundranåt
	  name(Tens,Some),
	  (
	    member(Tens,First9)	%% tre
	  ->
	    !,
	    number_phrase([S],D),
	    number_phrase([noll], D0),
	    number_phrase([Tens],D1),
	    name(D,DL),
	    name(D0,D0L),
	    name(D1,D1L),
	    append(DL,D0L,Temp),
	    append(Temp,D1L,Svar),
	    name(DigAtom,Svar)
	  ;
	    (
	      Tens \= ''
	    ->
	      !,
	      number_phrase([S],D),
	      number_phrase([Tens],Dig), %%femton,15
	      name(D,DL),
	      name(Dig,DigL),
	      append(DL,DigL,Svar),
	      name(DigAtom,Svar)
	    ;
	      !,
	      number_phrase([S],D),
	      number_phrase([noll],D0),
	      name(D,DL),
	      name(D0,D0L),
	      append(DL,D0L,Temp),
	      append(Temp,D0L,Svar),
	      name(DigAtom,Svar)
	    )
	  )
	).

chk100(BigAtom,DigAtom):-
	%format("numer == ~w~n",[BigAtom]),
	atomic(BigAtom),
	name(BigAtom,BigAtomList), %%hundratre
 	append(Hundra,Rest,BigAtomList),
 	name(Hun,Hundra),
	Hun = hundra,
	first9(First9),
	name(Tens,Rest),
	(
	  member(Tens,First9) %% TTens = tre
	->
	  !,
	  number_phrase([ett],D),
	  number_phrase([noll], D0),
	  number_phrase([Tens],D1),
	  name(D,DL),
	  name(D0,D0L),
	  name(D1,D1L),
	  append(DL,D0L,Temp),
	  append(Temp,D1L,Svar),
	  name(DigAtom,Svar)
	;
	  (
	    Tens \= ''
	  ->
	    !, %% Tens = femton
	    number_phrase([ett],D),
	    number_phrase([Tens],D1),
	    name(D,DL),
	    name(D1,D1L),
	    append(DL,D1L,Svar),
	    name(DigAtom,Svar)
	  ;
	    !,
	    DigAtom = 100
	  )
	).

chk1000(BigAtom,DigAtom):-
	%% BigAtom == fyratusentvåhundratjugotre
	atomic(BigAtom),
	first9(FL),
	name(BigAtom,BigAtomList), %%BAL charlista
	append(First,ThousRest,BigAtomList),
	name(First9,First),
	member(First9,FL),
	(
	  First9 = ett
	->
	  name(usen,Tusen)
	;
	  name(tusen,Tusen)
	),
	append(Tusen,Hundreds,ThousRest),
	name(Hundra,Hundreds),
	(
	  Hundra = '',
	  !,
	  number_phrase([First9],F),
	  number_phrase([noll], N),
	  name(F,Fs),
	  name(N,Ns),
	  append(Fs,Ns,NNs),%40
	  append(NNs,Ns,NNNs),%400
	  append(NNNs,Ns,NNNNs),
	  name(DigAtom,NNNNs)
	;  
	  chk100plus(Hundra,AtomHundra),
	  !,
	  number_phrase([First9],F),
	  name(F,Fs),
	  name(AtomHundra,AH),
	  append(Fs,AH,Res),
	  name(DigAtom,Res)
	;
	  member(Hundra,FL), %== tre
	  !,
	  number_phrase([First9],F), % == 4
	  name(F,Fs),
	  number_phrase([Hundra],HA),
	  number_phrase([noll], N),
	  name(HA,HAL),
	  name(N,Ns),
	  append(Fs,Ns,NNs),%40
	  append(NNs,Ns,NNNs),%400
	  append(NNNs,HAL,Res),
	  name(DigAtom,Res)
	;
	  !,
	  number_phrase([First9],F), % == 4
	  name(F,Fs),
	  number_phrase([noll], N),
	  name(N,Ns),
	  number_phrase([Hundra],HA),
	  name(HA,HAL),
	  append(Fs,Ns,NNs),%40
	  append(NNs,HAL,Res),
	  name(DigAtom,Res)
	).


	
number_phrase([noll], 0).
number_phrase([ett], 1).
number_phrase([två], 2).
number_phrase([tre], 3).
number_phrase([fyra], 4).
number_phrase([fem], 5).
number_phrase([sex], 6).
number_phrase([sju], 7).
number_phrase([åtta], 8).
number_phrase([nio], 9).

number_phrase([tio], 10).
number_phrase([elva], 11).
number_phrase([tolv], 12).
number_phrase([tretton], 13).
number_phrase([fjorton], 14).
number_phrase([femton], 15).
number_phrase([sexton], 16).
number_phrase([sjutton], 17).
number_phrase([arton], 18).
number_phrase([nitton], 19).

number_phrase([tjugo], 20).
number_phrase([tjugoett], 21).
number_phrase([tjugotvå], 22).
number_phrase([tjugotre], 23).
number_phrase([tjugofyra], 24).
number_phrase([tjugofem], 25).
number_phrase([tjugosex], 26).
number_phrase([tjugosju], 27).
number_phrase([tjugoåtta], 28).
number_phrase([tjugonio], 29).

number_phrase([trettio], 30).
number_phrase([trettioett], 31).
number_phrase([trettiotvå], 32).
number_phrase([trettiotre], 33).
number_phrase([trettiofyra], 34).
number_phrase([trettiofem], 35).
number_phrase([trettiosex], 36).
number_phrase([trettiosju], 37).
number_phrase([trettioåtta], 38).
number_phrase([trettionio], 39).

number_phrase([fyrtio], 40).
number_phrase([fyrtioett], 41).
number_phrase([fyrtiotvå], 42).
number_phrase([fyrtiotre], 43).
number_phrase([fyrtiofyra], 44).
number_phrase([fyrtiofem], 45).
number_phrase([fyrtiosex], 46).
number_phrase([fyrtiosju], 47).
number_phrase([fyrtioåtta], 48).
number_phrase([fyrtionio], 49).

number_phrase([femtio], 50).
number_phrase([femtioett], 51).
number_phrase([femtiotvå], 52).
number_phrase([femtiotre], 53).
number_phrase([femtiofyra], 54).
number_phrase([femtiofem], 55).
number_phrase([femtiosex], 56).
number_phrase([femtiosju], 57).
number_phrase([femtioåtta], 58).
number_phrase([femtionio], 59).

number_phrase([sextio], 60).
number_phrase([sextioett], 61).
number_phrase([sextiotvå], 62).
number_phrase([sextiotre], 63).
number_phrase([sextiofyra], 64).
number_phrase([sextiofem], 65).
number_phrase([sextiosex], 66).
number_phrase([sextiosju], 67).
number_phrase([sextioåtta], 68).
number_phrase([sextionio], 69).

number_phrase([sjuttio], 70).
number_phrase([sjuttioett], 71).
number_phrase([sjuttiotvå], 72).
number_phrase([sjuttiotre], 73).
number_phrase([sjuttiofyra], 74).
number_phrase([sjuttiofem], 75).
number_phrase([sjuttiosex], 76).
number_phrase([sjuttiosju], 77).
number_phrase([sjuttioåtta], 78).
number_phrase([sjuttionio], 79).

number_phrase([åttio], 80).
number_phrase([åttioett], 81).
number_phrase([åttiotvå], 82).
number_phrase([åttiotre], 83).
number_phrase([åttiofyra], 84).
number_phrase([åttiofem], 85).
number_phrase([åttiosex], 86).
number_phrase([åttiosju], 87).
number_phrase([åttioåtta], 88).
number_phrase([åttionio], 89).

number_phrase([nittio], 90).
number_phrase([nittioett], 91).
number_phrase([nittiotvå], 92).
number_phrase([nittiotre], 93).
number_phrase([nittiofyra], 94).
number_phrase([nittiofem], 95).
number_phrase([nittiosex], 96).
number_phrase([nittiosju], 97).
number_phrase([nittioåtta], 98).
number_phrase([nittionio], 99).


%number_phrase([etthundra], 100).
%number_phrase([hundra], 100).
%%% number_phrase([etthundraett], 101).
%%% number_phrase([etthundratvå], 102).
%%% number_phrase([etthundratre], 103).
%%% number_phrase([etthundrafyra], 104).
%%% number_phrase([etthundrafem], 105).
%%% number_phrase([etthundrasex], 106).
%%% number_phrase([etthundrasju], 107).
%%% number_phrase([etthundraåtta], 108).
%%% number_phrase([etthundranio], 109).

%%% number_phrase([hundraett], 101).
%%% number_phrase([hundratvå], 102).
%%% number_phrase([hundratre], 103).
%%% number_phrase([hundrafyra], 104).
%%% number_phrase([hundrafem], 105).
%%% number_phrase([hundrasex], 106).
%%% number_phrase([hundrasju], 107).
%%% number_phrase([hundraåtta], 108).
%%% number_phrase([hundranio], 109).
