
:- module(ppDrs,[ppDrs/3]).

:- use_module(library(lists),[member/2,append/3,select/3,reverse/2]).
:- use_module(boxer(alphaConversionDRT),[alphaConvertDRS/2]).
:- use_module(semlib(options),[candc_option/2]).
:- use_module(knowledge(title),[title/1]).
:- use_module(boxer(noncomp),[noncomp/2]).


/*========================================================================
   Dynamic Predicates
========================================================================*/

:- dynamic counter/1.

counter(0).


/*========================================================================
   Postprocessing XDRSs
*/

ppDrs(X,_,X):- !.

ppDrs(xdrs(W1,P,E,A1),Context,xdrs(W2,P,E,A3)):-
   ppWords(W1,0,W2,W3),
   numbervars(A1,0,Counter),
   retractall(counter(_)),
   assert(counter(Counter)),
   ppDrs(A1,P,E,W3,Context,drs([],[])-_,A2),
   alphaConvertDRS(A2,A3).


/*========================================================================
   Postprocessing DRSs
*/

ppDrs(merge(A1,A2),P,E,W,C,CD1-CD3,merge(A3,A4)):- !,
   ppDrs(A1,P,E,W,C,CD1-CD2,A3), 
   ppDrs(A2,P,E,W,C,CD2-CD3,A4).

ppDrs(smerge(A1,A2),P,E,W,C,CD1-CD3,smerge(A3,A4)):- !,
   ppDrs(A1,P,E,W,C,CD1-CD2,A3), 
   ppDrs(A2,P,E,W,C,CD2-CD3,A4).

ppDrs(alfa(Type,A1,A2),P,E,W,C,CD1-CD3,alfa(Type,A3,A4)):- !,
   ppDrs(A1,P,E,W,C,CD1-CD2,A3),
   ppDrs(A2,P,E,W,C,CD2-CD3,A4).

ppDrs(B1,P,E,W,Context,CD1-CD2,B3):-
   checkDrs(B1,P,E,W,Context,CD1,B2),
   ppConds(B2,P,E,W,Context,CD1-CD2,B3).


/*========================================================================
   Postprocessing DRS-Conditions
*/

ppConds(drs([],[]),_,_,_,_,CD-CD,drs([],[])):- !.

ppConds(drs([D|D1],C1),P,E,W,Context,drs(G,H)-CD,drs([D|D2],C2)):- !,
   ppConds(drs(D1,C1),P,E,W,Context,drs([D|G],H)-CD,drs(D2,C2)).

ppConds(drs([],C1),P,E,W,Context,drs(G,H)-CD,drs(D3,[Cond|C3])):- 
   member(Cond,[_:card(_,_,_),_:pred(_,_,_,_),_:rel(_,_,_,_),_:named(_,_,_,_),_:timex(_,_),_:eq(_,_)]),
   select(Cond,C1,C2), !,
   ppConds(drs([],C2),P,E,W,Context,drs(G,[Cond|H])-CD,drs(D3,C3)).

ppConds(drs([],[I:imp(A1,A2)|Conds1]),P,E,W,C,CD1-CD3,drs(D2,[I:imp(A3,A4)|Conds2])):- !,
   ppDrs(A1,P,E,W,C,CD1-CD2,A3),
   ppDrs(A2,P,E,W,C,CD2-_,A4),
   ppConds(drs([],Conds1),P,E,W,C,CD1-CD3,drs(D2,Conds2)).

ppConds(drs([],[I:or(A1,A2)|Conds1]),P,E,W,C,CD1-CD3,drs(D2,[I:or(A3,A4)|Conds2])):- !,
   ppDrs(A1,P,E,W,C,CD1-_,A3),
   ppDrs(A2,P,E,W,C,CD1-_,A4),
   ppConds(drs([],Conds1),P,E,W,C,CD1-CD3,drs(D2,Conds2)).

ppConds(drs([],[I:duplex(Type,A1,Var,A2)|Conds1]),P,E,W,C,CD1-CD3,drs(D2,[I:duplex(Type,A3,Var,A4)|Conds2])):- !,
   ppDrs(A1,P,E,W,C,CD1-CD2,A3),
   ppDrs(A2,P,E,W,C,CD2-_,A4),
   ppConds(drs([],Conds1),P,E,W,C,CD1-CD3,drs(D2,Conds2)).

ppConds(drs([],[I:not(A1)|Conds1]),P,E,W,C,CD1-CD3,drs(D2,[I:not(A2)|Conds2])):- !,
   ppDrs(A1,P,E,W,C,CD1-_,A2),
   ppConds(drs([],Conds1),P,E,W,C,CD1-CD3,drs(D2,Conds2)).

ppConds(drs([],[I:nec(A1)|Conds1]),P,E,W,C,CD1-CD3,drs(D2,[I:nec(A2)|Conds2])):- !,
   ppDrs(A1,P,E,W,C,CD1-_,A2),
   ppConds(drs([],Conds1),P,E,W,C,CD1-CD3,drs(D2,Conds2)).

ppConds(drs([],[I:pos(A1)|Conds1]),P,E,W,C,CD1-CD3,drs(D2,[I:pos(A2)|Conds2])):- !,
   ppDrs(A1,P,E,W,C,CD1-_,A2),
   ppConds(drs([],Conds1),P,E,W,C,CD1-CD3,drs(D2,Conds2)).

ppConds(drs([],[I:prop(X,A1)|Conds1]),P,E,W,C,CD1-CD3,drs(D2,[I:prop(X,A2)|Conds2])):- !,
   ppDrs(A1,P,E,W,C,CD1-_,A2),
   ppConds(drs([],Conds1),P,E,W,C,CD1-CD3,drs(D2,Conds2)).


/*========================================================================
   Postprocessing DRS-Conditions
*/

checkDrs(B1,POS,NE,Words,Context,CDRS,B4):-
   rule(Check,Remove,Constraints,Add), 
   member(Check,B1,CDRS),
   remove(Remove,B1,B2),
   checkConstraints(Constraints,POS,NE,Words,Context), !,
   add(Add,B2,B3),
   checkDrs(B3,POS,NE,Words,Context,CDRS,B4).

checkDrs(B,_,_,_,_,_,B).


/*========================================================================
   Member check DRS-Conditions
========================================================================*/

member(drs([],[]),_,_):- !.

member(drs(D1,[C|C1]),drs(D2,C2),B):-
   member(C,C2),
   member(drs(D1,C1),drs(D2,C2),B).

member(drs(D1,[C|C1]),B,drs(D2,C2)):-
   member(C,C2),
   member(drs(D1,C1),B,drs(D2,C2)).

member(drs([D|D1],[]),drs(D2,C2),B):-
   member(D,D2),
   member(drs(D1,[]),drs(D2,C2),B).

member(drs([D|D1],[]),B,drs(D2,C2)):-
   member(D,D2),
   member(drs(D1,[]),B,drs(D2,C2)).


/*========================================================================
   Remove DRS-Conditions
========================================================================*/

remove(drs([],[]),B,B):- !.

remove(drs(D1,[C|C1]),drs(D2,C2),B):-
   select(C,C2,C3),
   remove(drs(D1,C1),drs(D2,C3),B).

remove(drs([D|D1],[]),drs(D2,C),B):-
   select(D,D2,D3),
   remove(drs(D1,[]),drs(D3,C),B).


/*========================================================================
   Add DRS-Conditions
========================================================================*/

add(drs([],[]),B,B).

add(drs([X|D],C1),drs(D1,C2),B):-
   add(drs(D,C1),drs([X|D1],C2),B).

add(drs([],[C|C1]),drs(D,C2),B):-
   add(drs([],C1),drs(D,[C|C2]),B).


/*========================================================================
   Check constraints
========================================================================*/

checkConstraints([],_,_,_,_).

checkConstraints([pos(A,POS)|L],P,N,W,Context):- !,
   member(B,POS),
   member(pos(A,B),P),
   checkConstraints(L,P,N,W,Context).

checkConstraints([ne(A,NE)|L],P,N,W,Context):- !,
   member(B,NE),
   member(ne(A,B),N),
   checkConstraints(L,P,N,W,Context).

checkConstraints([framerole(RIs,Role)|L],P,N,W,Context):- !,
   member(role(Role,R1,R2),Context),                   %%% find annotated framenet role
   member(RI,RIs), member(word(RI,_,R3,R4),W),         %%% relaxed match with words
   R3 >= R1, R4 =< R2, !,
   checkConstraints(L,P,N,W,Context).

checkConstraints([frametarget(EIs,Frame)|L],P,N,W,Context):- !,
   member(target(Frame,T1,T2),Context),                %%% find annotated Target
   member(EI,EIs), member(word(EI,_,T3,T4),W),         %%% relaxed match with words
   T3 >= T1, T4 =< T2, !,
   checkConstraints(L,P,N,W,Context).

checkConstraints([current_year(Year)|L],P,N,W,Context):- !,
   member(year:Year,Context), !,
   checkConstraints(L,P,N,W,Context).

checkConstraints([length_word(Index,Len)|L],P,N,W,Context):- !,
   member(word(Index,Word,_,_),W), !,
   atom_chars(Word,Chars), 
   length(Chars,Len),
   checkConstraints(L,P,N,W,Context).

checkConstraints([next_year(NextYear)|L],P,N,W,Context):- !,
   member(year:Year,Context), 
   nextYear(Year,NextYear),
   checkConstraints(L,P,N,W,Context).

checkConstraints([last_year(LastYear)|L],P,N,W,Context):- !,
   member(year:Year,Context), 
   lastYear(Year,LastYear),
   checkConstraints(L,P,N,W,Context).

checkConstraints([next_month(NextMonth,NextYear)|L],P,N,W,Context):- !,
   member(month:Month,Context),
   member(year:Year,Context),
   nextMonth(Month:Year,NextMonth:NextYear),
   checkConstraints(L,P,N,W,Context).

checkConstraints([last_month(LastMonth,LastYear)|L],P,N,W,Context):- !,
   member(month:Month,Context),
   member(year:Year,Context),
   lastMonth(Month:Year,LastMonth:LastYear),
   checkConstraints(L,P,N,W,Context).

checkConstraints([current_month(Month)|L],P,N,W,Context):- !,
   member(month:Month,Context), !,
   checkConstraints(L,P,N,W,Context).

checkConstraints([current_day(Day)|L],P,N,W,Context):- !,
   member(day:Day,Context), !,
   checkConstraints(L,P,N,W,Context).

checkConstraints([C|L],P,N,W,Context):- 
   call(ppDrs:C),
   checkConstraints(L,P,N,W,Context).


/* ------------------------------------------------------------------------------
   Next
------------------------------------------------------------------------------ */

nextYear(Year,NextYear):-
   atom_number(Year,Y),
   NextY is Y + 1,
   atom_number(NextYear,NextY).

nextMonth('01':Year, '02':Year):- !.
nextMonth('02':Year, '03':Year):- !.
nextMonth('03':Year, '04':Year):- !.
nextMonth('04':Year, '05':Year):- !.
nextMonth('05':Year, '06':Year):- !.
nextMonth('06':Year, '07':Year):- !.
nextMonth('07':Year, '08':Year):- !.
nextMonth('08':Year, '09':Year):- !.
nextMonth('09':Year, '10':Year):- !.
nextMonth('10':Year, '11':Year):- !.
nextMonth('11':Year, '12':Year):- !.
nextMonth('12':Year, '01':NextYear):- !, nextYear(Year,NextYear).


/* ------------------------------------------------------------------------------
   Last
------------------------------------------------------------------------------ */

lastYear(Year,LastYear):-
   atom_number(Year,Y),
   LastY is Y - 1,
   atom_number(LastYear,LastY).

lastMonth('01':Year, '12':LastYear):- !, lastYear(Year,LastYear).
lastMonth('02':Year, '01':Year):- !.
lastMonth('03':Year, '02':Year):- !.
lastMonth('04':Year, '03':Year):- !.
lastMonth('05':Year, '04':Year):- !.
lastMonth('06':Year, '05':Year):- !.
lastMonth('07':Year, '06':Year):- !.
lastMonth('08':Year, '07':Year):- !.
lastMonth('09':Year, '08':Year):- !.
lastMonth('10':Year, '09':Year):- !.
lastMonth('11':Year, '10':Year):- !.
lastMonth('12':Year, '11':Year):- !.


/* ------------------------------------------------------------------------------
   Dealing with Quotes [new]
------------------------------------------------------------------------------ */

rule(drs([],[]),
     drs([],[[I]:pred(X,quotation,n,2),[J]:pred(X,quotation,n,2)]),

     [ I < J ],

     drs([],[[I,J]:pred(X,quotation,n,2)])).


/* ------------------------------------------------------------------------------
   Dealing with Quotes [old, bit of a hack]
------------------------------------------------------------------------------ */

rule(drs([],[]),
     drs([],[I:pred(X,Sym,a,0)]),

     [ quoted(Sym) ],

     drs([],[I:named(X,Sym,quo,0)])).

rule(drs([],[]),
     drs([],[I:pred(X,Sym,n,0)]),

     [ quoted(Sym) ],

     drs([],[I:named(X,Sym,quo,0)])).


rule(drs([],[]),
     drs([],[I:named(X,Sym,nam,0)]),

     [ quoted(Sym) ],

     drs([],[I:named(X,Sym,quo,0)])).


/* ------------------------------------------------------------------------------
   Dealing with URLs and emails
------------------------------------------------------------------------------ */

rule(drs([],[]),
     drs([],[I:pred(X,Sym,_,Sense)]),

     [ url(Sym) ],

     drs([],[I:named(X,Sym,url,Sense)])).


rule(drs([],[]),
     drs([],[I:named(X,Sym,nam,Sense)]),

     [ url(Sym) ],

     drs([],[I:named(X,Sym,url,Sense)])).

rule(drs([],[]),
     drs([],[I:pred(X,Sym,_,Sense)]),

     [ email(Sym) ],

     drs([],[I:named(X,Sym,ema,Sense)])).


rule(drs([],[]),
     drs([],[I:named(X,Sym,nam,Sense)]),

     [ email(Sym) ],

     drs([],[I:named(X,Sym,ema,Sense)])).


/* ------------------------------------------------------------------------------
   Title, Names
------------------------------------------------------------------------------ */

rule(drs([],[]),
     drs([],[I:named(X,Title,nam,Sense1),J:named(X,Sym,Type,Sense2)]),

     [ title(Title),
       adjacent(I,J,_) ],

     drs([],[I:named(X,Title,ttl,Sense1),J:named(X,Sym,Type,Sense2)])).

rule(drs([],[I:named(X,_,ttl,_)]),
     drs([],[J:named(X,Sym,nam,Sense)]),

     [ adjacent(I,J,_) ],

     drs([],[J:named(X,Sym,per,Sense)])).

rule(drs([],[J:named(X,_,per,_)]),
     drs([],[I:named(X,Sym,nam,Sense)]),

     [ adjacent(I,J,_) ],

     drs([],[I:named(X,Sym,per,Sense)])).

rule(drs([],[I:named(X,_,per,_)]),
     drs([],[J:named(X,Sym,nam,Sense)]),

     [ adjacent(I,J,_) ],

     drs([],[J:named(X,Sym,per,Sense)])).


/* ------------------------------------------------------------------------------
   Ampersand in company names
------------------------------------------------------------------------------ */

rule(drs([],[]),
     drs([],[[I]:pred(X,'&',a,_)]),

     [ ne(I,['I-ORG','B-ORG','I-ORGANIZATION']) ],

     drs([],[[I]:named(X,'&',org,0)])).


/* ------------------------------------------------------------------------------
   Proper Names (refining sorts)

rule(drs([],[]),
     drs([],[[I]:named(X,Sym,nam,Sense)]),

     [ ne(I,['I-PER','I-PERSON']) ],

     drs([],[[I]:named(X,Sym,per,Sense)])).

rule(drs([],[]),
     drs([],[[I]:named(X,Sym,nam,Sense)]),

     [ ne(I,['I-ORG','B-ORG','I-ORGANIZATION']) ],

     drs([],[[I]:named(X,Sym,org,Sense)])).

rule(drs([],[]),
     drs([],[[I]:named(X,Sym,nam,Sense)]),

     [ ne(I,['I-LOC','B-LOC','I-LOCATION']) ],

     drs([],[[I]:named(X,Sym,loc,Sense)])).
------------------------------------------------------------------------------ */


/* ------------------------------------------------------------------------------
   Compound numeral expressions  (e.g. "4 billion")
------------------------------------------------------------------------------ */

rule(drs([],[]),
     drs([],[[I]:card(X,Num1,Type),[J]:card(X,1000000000,Type)]), 

     [ number(Num1), J is I + 1, 
       Num2 is integer(1000000000 * Num1) ],

     drs([],[[I,J]:card(X,Num2,Type)])).


/* ------------------------------------------------------------------------------
   Compound numeral expressions  (e.g. "12 million")

rule(drs([],[]),
     drs([],[[I]:card(X,Num1,Type),[J]:card(X,1000000,Type)]),

     [ number(Num1), J is I + 1, 
       Num2 is integer(1000000 * Num1) ],

     drs([],[[I,J]:card(X,Num2,Type)])).
------------------------------------------------------------------------------ */


/* ------------------------------------------------------------------------------
   Compound numeral expressions  (e.g. "7 thousand")


rule(drs([],[]),
     drs([],[[I]:card(X,Num1,Type),[J]:card(X,1000,Type)]),

     [ number(Num1), J is I + 1, 
       Num2 is integer(1000 * Num1) ],

     drs([],[[I,J]:card(X,Num2,Type)])).
------------------------------------------------------------------------------ */

/* ------------------------------------------------------------------------------
   Compound numeral expressions  (e.g. "four feet ten inches")
------------------------------------------------------------------------------ */

rule(drs([],[]),
     drs([],[[I]:card(X,Num1,Type),[J]:pred(X,foot,n,Sense),
             [K]:card(X,Num2,Type),[L]:pred(X,inch,n,_)]),

     [ number(Num1), number(Num2),
       J is I + 1, K is J + 1, L is K + 1,
       Num3 is (Num1 + (Num2/100)) ],

     drs([],[[I,K]:card(X,Num3,Type),[J,L]:pred(X,foot,n,Sense)])).


/* ------------------------------------------------------------------------------
   Date expressions (e.g. "March 2, 2004")
------------------------------------------------------------------------------ */

rule(drs([],[]),
     drs([],[[I]:pred(X,Month,a,_),[J]:card(X,Day,ge),[K]:card(X,Year,ge)]),
 
     [ month(Month,MID),
       J is I + 1,
       day(Day,DID),
       K is I + 3, 
       L is I + 2,
       number(Year), Year > 1000, Year < 3000,
       year(Year,YID) ],

     drs([],[[I,J,L,K]:timex(X,date([]:'+',[K]:YID,[I]:MID,[J]:DID))])).

rule(drs([],[]),
     drs([],[[I]:named(X,Month,nam,_),[J]:card(X,Day,ge),[K]:card(X,Year,ge)]),
 
     [ month(Month,MID),
       J is I + 1,
       day(Day,DID),
       K is I + 3, 
       L is I + 2,
       number(Year), Year > 1000, Year < 3000,
       year(Year,YID) ],

     drs([],[[I,J,L,K]:timex(X,date([]:'+',[K]:YID,[I]:MID,[J]:DID))])).

rule(drs([],[]),
     drs([],[[I]:named(X,Month,nam,_),[J]:pred(X,Day,_,_),[K]:card(X,Year,ge)]),
 
     [ month(Month,MID),
       J is I + 1, 
       day(Day,DID),
       K is I + 3,  
       L is I + 2,
       number(Year), Year > 1000, Year < 3000,
       year(Year,YID) ],

     drs([],[[I,J,L,K]:timex(X,date([]:'+',[K]:YID,[I]:MID,[J]:DID))])).

/* ------------------------------------------------------------------------------
   Clocktime expressions (e.g. "at 4 : 30 pm")
------------------------------------------------------------------------------ */

rule(drs([],[]),
     drs([],[[I]:card(X,Hour,ge),[I]:pred(X,thing,n,12),[J]:rel(X,Y,':',_),[K]:card(Y,Minutes,ge),[L]:pred(Y,AMPM,n,_)]),
 
     [ member(AMPM,[am,pm,'a.m.','p.m.']),
       hour(Hour,AMPM,HourID),
       minute(Minutes,MinID) ],

     drs([],[[I,J,K,L]:timex(X,time([I]:HourID,[K]:MinID,[]:'XX'))])).

rule(drs([],[]),
     drs([],[[I]:card(X,Hour,ge),[I]:pred(X,thing,n,12),[J]:rel(X,Y,':',_),[K]:card(Y,Minutes,ge)]),
 
     [ hour(Hour,pm,HourID),
       minute(Minutes,MinID) ],

     drs([],[[I,J,K]:timex(X,time([I]:HourID,[K]:MinID,[]:'XX'))])).

rule(drs([],[_:rel(_,X,at,_)]),
     drs([],[I:card(X,Hour,ge),I:pred(X,thing,n,12)]),
 
     [ hour(Hour,pm,HourID) ],

     drs([],[I:timex(X,time(I:HourID,[]:'XX',[]:'XX'))])).


/* ------------------------------------------------------------------------------
   Clocktime expressions (e.g. "2200 GMT")
------------------------------------------------------------------------------ */

rule(drs([],[]),
     drs([],[[I]:card(X,Time,ge),[J]:pred(X,gmt,n,_)]),

     [ gmt(Time,Hour,Minute),
       J is I + 1 ],

     drs([],[[I,J]:timex(X,time([]:Hour,[]:Minute,[]:'XX'))])).


/* ------------------------------------------------------------------------------
   Date expressions (e.g. "10th of March , 2004")
------------------------------------------------------------------------------ */

rule(drs([],[]),
     drs([],[[I]:pred(X,Day,_,_),[J]:rel(X,Y,of,_),[K]:named(Y,Month,nam,_),[M]:card(_,Year,ge)]),
 
     [ day(Day,DID),
       J is I + 1, 
       month(Month,MID),
       K is I + 2, 
       L is I + 3,
       M is I + 4,
       number(Year), Year > 1000, Year < 3000,
       year(Year,YID) ],

     drs([],[[I,J,K,L,M]:timex(X,date([]:'+',[L]:YID,[K]:MID,[I]:DID))])).

rule(drs([],[]),
     drs([],[[I]:pred(X,Day,_,_),[J]:rel(X,Y,of,_),[K]:pred(Y,Month,_,_),[M]:card(_,Year,ge)]),
 
     [ 
       day(Day,DID),
       J is I + 1,
       month(Month,MID),
       K is I + 2,
       L is I + 3,
       M is I + 4,
       number(Year), Year > 1000, Year < 3000,
       year(Year,YID) ],

     drs([],[[I,J,K,L,M]:timex(X,date([]:'+',[L]:YID,[K]:MID,[I]:DID))])).


/* ------------------------------------------------------------------------------
   Date expressions (e.g. "June 30", "28 July")
------------------------------------------------------------------------------ */

rule(drs([],[]),
     drs([],[[I]:named(X,Month,nam,_),[J]:card(X,Day,ge)]),

     [ month(Month,MID),
       J is I + 1, 
       day(Day,DID) ],
    
     drs([],[[I,J]:timex(X,date([]:'+',[]:'XXXX',[I]:MID,[J]:DID))])).

rule(drs([],[]),
     drs([],[[I]:pred(X,Month,_,_),[J]:card(X,Day,ge)]),

     [ month(Month,MID),
       J is I + 1, 
       day(Day,DID) ],
    
     drs([],[[I,J]:timex(X,date([]:'+',[]:'XXXX',[I]:MID,[J]:DID))])).

rule(drs([],[]),
     drs([],[[I]:card(X,Day,ge),[J]:named(X,Month,nam,_)]),

     [ month(Month,MID),
       J is I + 1,  
       day(Day,DID) ],
    
     drs([],[[I,J]:timex(X,date([]:'+',[]:'XXXX',[I]:MID,[J]:DID))])).

rule(drs([],[]),
     drs([],[[I]:card(X,Day,ge),[J]:pred(X,Month,_,_)]),

     [ month(Month,MID),
       J is I + 1, 
       day(Day,DID) ],
    
     drs([],[[I,J]:timex(X,date([]:'+',[]:'XXXX',[I]:MID,[J]:DID))])).


/* ------------------------------------------------------------------------------
   Date expressions (e.g. "June 1998")
------------------------------------------------------------------------------ */

rule(drs([],[]),
     drs([],[[I]:named(X,Month,nam,_),[J]:card(X,Year,ge)]),

     [ month(Month,MID),
       J is I + 1,
       number(Year), Year > 100, Year < 3000,
       year(Year,YID) ],

     drs([],[[I,J]:timex(X,date([]:'+',[J]:YID,[I]:MID,[]:'XX'))])).


/* ------------------------------------------------------------------------------
   Date expressions (e.g. "June")
------------------------------------------------------------------------------ */

rule(drs([],[]),
     drs([],[[I]:named(X,Month,nam,_)]),

     [ month(Month,MID) ],

     drs([],[[I]:timex(X,date([]:'+',[]:'XXXX',[I]:MID,[]:'XX'))])).

rule(drs([],[]),
     drs([],[[I]:pred(X,Month,n,_)]),

     [ month(Month,MID),
       pos(I,['NN','NNP']) ],

     drs([],[[I]:timex(X,date([]:'+',[]:'XXXX',[I]:MID,[]:'XX'))])).



/* ------------------------------------------------------------------------------
   Date expressions (years B.C.)
------------------------------------------------------------------------------ */

rule(drs([],[]),
     drs([],[[I]:card(X,Year,ge),[J]:named(X,BC,_,_)]),

     [ yearBC(BC),
       number(Year), 
       year(Year,YearFormatted),
       J is I + 1 ],

     drs([],[[I,J]:timex(X,date([J]:'-',[I]:YearFormatted,[]:'XX',[]:'XX'))])).

rule(drs([],[]),
     drs([],[[I]:card(X,Year,ge),[J]:pred(X,BC,n,_)]),

     [ yearBC(BC),
       number(Year), 
       year(Year,YearFormatted),
       J is I + 1 ],

     drs([],[[I,J]:timex(X,date([J]:'-',[I]:YearFormatted,[]:'XX',[]:'XX'))])).


/* ------------------------------------------------------------------------------
   Date expressions (years A.D.)
------------------------------------------------------------------------------ */

rule(drs([],[]),
     drs([],[[I]:card(X,Year,ge),[J]:named(X,AD,_,_)]),

     [ yearAD(AD),
       number(Year), 
       year(Year,YearFormatted),
       J is I + 1 ],

     drs([],[[I,J]:timex(X,date([J]:'+',[I]:YearFormatted,[]:'XX',[]:'XX'))])).

rule(drs([],[]),
     drs([],[[I]:card(X,Year,ge),[J]:pred(X,AD,n,_)]),

     [ yearAD(AD),
       number(Year), 
       year(Year,YearFormatted),
       J is I + 1 ],

     drs([],[[I,J]:timex(X,date([J]:'+',[I]:YearFormatted,[]:'XX',[]:'XX'))])).

rule(drs([],[]),
     drs([],[[I]:named(X,AD,_,_),[J]:card(X,Year,ge)]),

     [ yearAD(AD),
       number(Year), 
       year(Year,YearFormatted),
       J is I + 1 ],

     drs([],[[I,J]:timex(X,date([J]:'+',[I]:YearFormatted,[]:'XX',[]:'XX'))])).

rule(drs([],[]),
     drs([],[[I]:pred(X,AD,n,_),[J]:card(X,Year,ge)]),

     [ yearAD(AD),
       number(Year), 
       year(Year,YearFormatted),
       J is I + 1 ],

     drs([],[[I,J]:timex(X,date([J]:'+',[I]:YearFormatted,[]:'XX',[]:'XX'))])).


/* ------------------------------------------------------------------------------
   Date expressions (years)
------------------------------------------------------------------------------ */

rule(drs([],[]),
     drs([],[[I]:card(X,Year,ge)]),

     [ number(Year), 
       Year > 1000, Year < 2100,
       year(Year,YID),
       ne(I,['I-DAT','I-DATE']) ],

     drs([],[[I]:timex(X,date([]:'+',[I]:YID,[]:'XX',[]:'XX'))])).



/* ------------------------------------------------------------------------------
   Date expressions (e.g. "in/by/to/from 1992")
------------------------------------------------------------------------------ */

rule(drs([],[[J]:rel(_,X,_,_)]),
     drs([],[[I]:card(X,Year,ge)]),

     [ number(Year), 
       Year > 1000, Year < 2100,
       length_word(I,4),
       year(Year,YID),
       I is J + 1 ],

     drs([],[[I]:timex(X,date([]:'+',[I]:YID,[]:'XX',[]:'XX'))])).


/* ------------------------------------------------------------------------------
   Date expressions (decades)
------------------------------------------------------------------------------ */

rule(drs([],[_:rel(_,X,in,_)]),
     drs([],[[I]:pred(X,Decade,_,_)]),

     [ decade(Decade,D),
       ne(I,['I-DAT','I-DATE']), 
       pos(I,['NNS']) ],

     drs([],[[I]:timex(X,date([]:'+',[I]:D,[]:'XX',[]:'XX'))])).

rule(drs([],[]),
     drs([],[[I]:pred(X,Decade,_,_)]),

     [ decade(Decade,D) ],

     drs([],[[I]:timex(X,date([]:'+',[I]:D,[]:'XX',[]:'XX'))])).


/* ------------------------------------------------------------------------------
   Date expressions (e.g. 11th century)
------------------------------------------------------------------------------ */

rule(drs([],[]),
     drs([],[[I]:pred(X,Century,a,_),[J]:pred(X,century,n,_)]),

     [ J is I + 1, 
       century(Century,C) ],

     drs([],[[I,J]:timex(X,date([]:'+',[I,J]:C,[]:'XX',[]:'XX'))])).

rule(drs([],[]),
     drs([[J]:X],[[I]:pred(Y,Century,a,_),[J]:pred(X,century,n,_),[]:rel(X,Y,nn,_)]),

     [ J is I + 1, 
       century(Century,C) ],

     drs([],[[I,J]:timex(Y,date([]:'+',[I,J]:C,[]:'XX',[]:'XX'))])).


/* ------------------------------------------------------------------------------
   Date expressions (e.g. "the september 11 plot")
------------------------------------------------------------------------------ */

rule(drs([],[]),
     drs([],[[I]:named(X,Month,nam,_),[]:rel(X,Y,nn,Sense),[J]:card(Y,Day,ge)]),

     [ month(Month,MID),
       J is I + 1, 
       day(Day,DID) ],

     drs([],[[I,J]:timex(X,date([]:'+',[]:'XXXX',[I]:MID,[J]:DID)),[]:rel(X,Y,nn,Sense)])).


/* ------------------------------------------------------------------------------
   Date expressions (repair rules)
------------------------------------------------------------------------------ */

rule(drs([],[]),
     drs([_:Y],[[L]:timex(X,date([]:'+',[L]:Year,[]:_,[]:_)),[I,J]:timex(Y,date([]:'+',[]:_,[I]:Month,[J]:Day)),[L]:rel(E,X,rel,0),_:rel(E,Y,rel,0)]),

     [ J is I + 1,
       K is J + 1,
       L is K + 1 ],

     drs([],[[I,J,K,L]:timex(X,date([]:'+',[L]:Year,[I]:Month,[J]:Day)),[L]:rel(E,X,rel,0)])).


/* ------------------------------------------------------------------------------
   Indirect time expressions: today
------------------------------------------------------------------------------ */

rule(drs([],[]),
     drs([],[I:pred(X,today,n,_)]),

     [ current_year(Year),
       current_month(Month),
       current_day(Day) ],

     drs([],[I:timex(X,date([]:'+',[]:Year,[]:Month,[]:Day))])).


/* ------------------------------------------------------------------------------
   Indirect time expressions: next/last/this year/month
------------------------------------------------------------------------------ */

rule(drs([],[]),
     drs([],[[I]:pred(X,current,a,1),[J]:pred(X,year,n,_)]),

     [ current_year(Year) ],

     drs([],[[I,J]:timex(X,date([]:'+',[]:Year,[]:'XX',[]:'XX'))])).

rule(drs([],[]),
     drs([],[[I]:pred(X,next,a,_),[J]:pred(X,year,n,_)]),

     [ next_year(Year) ],

     drs([],[[I,J]:timex(X,date([]:'+',[]:Year,[]:'XX',[]:'XX'))])).

rule(drs([],[]),
     drs([],[[I]:pred(X,current,a,1),[J]:pred(X,month,n,_)]),

     [ current_month(Month), 
       current_year(Year) ],

     drs([],[[I,J]:timex(X,date([]:'+',[]:Year,[]:Month,[]:'XX'))])).

rule(drs([],[_:pred(E,event,n,_),_:rel(E,X,rel,0)]),
     drs([],[[I]:pred(E,next,a,_),[J]:pred(X,month,n,_)]),

     [ next_month(Month,Year) ],

     drs([],[[I,J]:timex(X,date([]:'+',[]:Year,[]:Month,[]:'XX'))])).

rule(drs([],[_:pred(E,event,n,_),_:rel(E,X,rel,0)]),
     drs([],[[I]:pred(E,last,a,_),[J]:pred(X,month,n,_)]),

     [ last_month(Month,Year) ],

     drs([],[[I,J]:timex(X,date([]:'+',[]:Year,[]:Month,[]:'XX'))])).

rule(drs([],[_:pred(E,event,n,_),_:rel(E,X,rel,0)]),
     drs([],[[I]:pred(E,last,a,_),[J]:pred(X,year,n,_)]),

     [ last_year(Year) ],

     drs([],[[I,J]:timex(X,date([]:'+',[]:Year,[]:'XX',[]:'XX'))])).

rule(drs([],[_:pred(E,event,n,_),_:rel(E,X,rel,0)]),
     drs([],[[I]:pred(E,next,a,_),[J]:pred(X,year,n,_)]),

     [ next_year(Year) ],

     drs([],[[I,J]:timex(X,date([]:'+',[]:Year,[]:'XX',[]:'XX'))])).


/* ------------------------------------------------------------------------------
   Date expressions (implicit year)
------------------------------------------------------------------------------ */

rule(drs([],[_:rel(E,X,_,_),_:pred(E,future,a,1)]),
     drs([],[I1:timex(X,date([]:'+',[]:'XXXX',I2:Month,Day))]),

     [ current_year(CurrentYear),
       current_month(CurrentMonth),
       \+ Month='XX',
       nextyofm(CurrentMonth,Month,CurrentYear,Year) ],

     drs([],[I1:timex(X,date([]:'+',[]:Year,I2:Month,Day))])).

rule(drs([],[_:rel(E,X,_,_),_:pred(E,past,a,1)]),
     drs([],[I:timex(X,date([]:'+',[]:'XXXX',MI:Month,Day))]),

     [ current_year(CurrentYear),
       current_month(CurrentMonth),
       current_day(CurrentDay),
       earlier_doy(doy(Month,Day),doy(CurrentMonth,CurrentDay)) ],

     drs([],[I:timex(X,date([]:'+',[]:CurrentYear,MI:Month,Day))])).

rule(drs([],[_:rel(E,X,_,_),_:pred(E,past,a,1)]),
     drs([],[I:timex(X,date([]:'+',[]:'XXXX',Month,Day))]),

     [ current_year(CurrentYear),
       last_year(CurrentYear,LastYear) ],

     drs([],[I:timex(X,date([]:'+',[]:LastYear,Month,Day))])).


/* ------------------------------------------------------------------------------
   NN-resolution (e.g. "document collection/grape variety")
------------------------------------------------------------------------------ */

rule(drs([],[[I]:pred(X,_,n,_),[J]:pred(Y,Collection,n,_)]),
     drs([],[[]:rel(X,Y,nn,_)]),

     [ member(Collection,[assortment,mixture,variety,potpourri,motley,
                          collection,aggregation,accumulation,assemblage,group]),
       J is I + 1 ],

     drs([],[[]:rel(Y,X,of,1)])).


/* ------------------------------------------------------------------------------
   Cardinals: just/exactly/at most CARD
------------------------------------------------------------------------------ */

rule(drs([],[]),
     drs([],[[I]:pred(X,just,a,_),[J]:card(X,C,ge)]),
    
     [ J is I + 1 ],

     drs([],[[I,J]:card(X,C,eq)])).

rule(drs([],[]),
     drs([],[[I]:pred(X,exactly,a,_),[J]:card(X,C,ge)]),
    
     [ J is I + 1 ],

     drs([],[[I,J]:card(X,C,eq)])).

rule(drs([],[]),
     drs([],[[I]:pred(X,at,_,_),[J]:prop(_,drs([],[[J]:pred(X,most,_,_)])),[K]:card(X,C,ge)]),

     [ J is I + 1,
       K is J + 1 ],

     drs([],[[I,J,K]:card(X,C,le)])).
      

/* ------------------------------------------------------------------------------
   Compound Names
------------------------------------------------------------------------------ */

rule(drs([],[]),
     drs([],[I:named(X,Sym1,Type,Sense),J:named(X,Sym2,nam,_)]),

     [ member(Type,[org,loc]),
       adjacent(I,J,K),
       concat_atom([Sym1,Sym2],'_',Sym) ],

     drs([],[K:named(X,Sym,Type,Sense)])).

rule(drs([],[]),
     drs([],[I:named(X,Sym1,nam,_),J:named(X,Sym2,Type,Sense)]),

     [ member(Type,[org,loc]),
       adjacent(I,J,K),
       concat_atom([Sym1,Sym2],'_',Sym) ],

     drs([],[K:named(X,Sym,Type,Sense)])).

rule(drs([],[]),
     drs([],[I:named(X,Sym1,Type,Sense),J:named(X,Sym2,Type,_)]),

     [ member(Type,[org,loc,nam]),
       adjacent(I,J,K),
       concat_atom([Sym1,Sym2],'_',Sym) ],

     drs([],[K:named(X,Sym,Type,Sense)])).


/* ------------------------------------------------------------------------------
   NN compounds: DATE + NAME / NOUN
------------------------------------------------------------------------------ */

rule(drs([],[J:named(X,_,_,_)]),
     drs([],[I:timex(X,Timex)]),

     [ adjacent(I,J,_),
       newref(New) ],

     drs([I:New],[I:timex(New,Timex),[]:rel(X,New,temp_rel,0)])).

rule(drs([],[J:pred(X,_,n,_)]),
     drs([],[I:timex(X,Timex)]),

     [ adjacent(I,J,_),
       newref(New) ],

     drs([I:New],[I:timex(New,Timex),[]:rel(X,New,temp_rel,0)])).

rule(drs([],[J:pred(X,_,a,_),K:pred(X,_,n,_)]),
     drs([],[I:timex(X,Timex)]),

     [ adjacent(I,J,_),
       adjacent(J,K,_),
       newref(New) ],

     drs([I:New],[I:timex(New,Timex),[]:rel(X,New,temp_rel,0)])).


/* ------------------------------------------------------------------------------
   NN compounds: PN + NOUN + NOUN
------------------------------------------------------------------------------ */

rule(drs([],[J:pred(X,Sym,n,_)]),
     drs([],[H:named(X,Sym0,Type,Sense),I:pred(X,Sym1,n,Sense1)]),

     [ adjacent(H,I,_),
       adjacent(I,J,_),  
       \+ member(Sym,[quantity,amount]),
       \+ member(Sym1,[quantity,amount]),
       newref(New1),
       newref(New2) ],

     drs([H:New1,I:New2],[H:named(New1,Sym0,Type,Sense),[]:rel(New1,X,nn,0),
                          I:pred(New2,Sym1,n,Sense1),[]:rel(New2,X,nn,0)])).


/* ------------------------------------------------------------------------------
   NN compounds: NOUN + PN + NOUN
------------------------------------------------------------------------------ */

rule(drs([],[J:pred(X,Sym,n,_)]),
     drs([],[H:pred(X,Sym0,n,Sense1),I:named(X,Sym1,Type,Sense)]),

     [ adjacent(H,I,_),
       adjacent(I,J,_),  
       \+ member(Sym,[quantity,amount]),
       \+ member(Sym0,[quantity,amount]),
       newref(New1),
       newref(New2) ],

     drs([H:New1,I:New2],[H:pred(New1,Sym0,n,Sense1),[]:rel(New1,X,nn,0),
                          I:named(New2,Sym1,Type,Sense),[]:rel(New2,X,nn,0)])).


/* ------------------------------------------------------------------------------
   NN compounds: CARD + NOUN + NOUN
------------------------------------------------------------------------------ */

% [4007]:pred(_G10737, percent, n, 1), [4006]:card(_G10737, 83.4, ge), [4008]:pred(_G10737, interest, n, 0)]

rule(drs([],[K:pred(X,Sym0,n,_)]),
     drs([],[I:card(X,Car,Type),J:pred(X,Sym,n,Sense)]),

     [ adjacent(I,J,_), adjacent(J,K,_),
       \+ member(Sym0,[quantity,amount]),
       \+ member(Sym,[quantity,amount]),
       newref(New) ],

     drs([J:New],[I:card(New,Car,Type),J:pred(New,Sym,n,Sense),[]:rel(New,X,nn,0)])
    ).



/* ------------------------------------------------------------------------------
   NN compounds: NOUN + NOUN

rule(drs([],[J:pred(X,Sym0,n,_)]),
     drs([],[I:pred(X,Sym,n,Sense)]),

     [ adjacent(I,J,_),
       \+ member(Sym0,[quantity,amount]),
       \+ member(Sym,[quantity,amount]),
       newref(New) ],

     drs([I:New],[I:pred(New,Sym,n,Sense),[]:rel(New,X,nn,0)])
    ).
------------------------------------------------------------------------------ */


/* ------------------------------------------------------------------------------
   NN compounds: NOUN + ADJ + NOUN
------------------------------------------------------------------------------ */

rule(drs([],[J:pred(X,_,a,_),K:pred(X,_,n,_)]),
     drs([],[I:pred(X,Sym,n,Sense)]),

     [ adjacent(I,J,_),
       \+ member(Sym,[quantity,amount]),
       adjacent(J,K,_),
       newref(New) ],

     drs([I:New],[I:pred(New,Sym,n,Sense),[]:rel(New,X,nn,0)])).


/* ------------------------------------------------------------------------------
   NN compounds: PN + PN + NOUN
------------------------------------------------------------------------------ */

rule(drs([],[J:pred(X,_,n,_)]),
     drs([],[H:named(X,Sym0,per,Sense1),I:named(X,Sym1,per,Sense2)]),

     [ adjacent(H,I,HI),
       adjacent(I,J,_),  
       newref(New) ],

     drs([HI:New],[H:named(New,Sym0,per,Sense1),I:named(New,Sym1,per,Sense2),[]:rel(New,X,nn,0)])).


/* ------------------------------------------------------------------------------
   NN compounds: NAME + NOUN
------------------------------------------------------------------------------ */

rule(drs([],[J:pred(X,_,n,_)]),
     drs([],[I:named(X,Sym1,Type,Sense1)]),

     [ adjacent(I,J,_),
       newref(New) ],

     drs([I:New],[I:named(New,Sym1,Type,Sense1),[]:rel(New,X,nn,0)])).


/* ------------------------------------------------------------------------------
   NN compounds: NOUN + NAME 
------------------------------------------------------------------------------ */

rule(drs([],[J:named(X,_,_,_)]),
     drs([],[I:pred(X,Sym,n,Sense)]),

     [ adjacent(I,J,_),
       \+ member(Sym,[quantity,amount]),
       newref(New) ],

     drs([I:New],[I:pred(New,Sym,n,Sense),[]:rel(New,X,nn,0)])).


/* ------------------------------------------------------------------------------
   Dimensional Adjectives
------------------------------------------------------------------------------ */

rule(drs([],[]),
     drs([],[[I]:pred(X,Adj,a,_)]),
    
     [ dimension(Adj,Dim,Sense),
       J is I - 1,
       pos(J,['WRB']) ],

     drs([],[[I]:pred(X,Dim,n,Sense)])).



/* ------------------------------------------------------------------------------
   Framenet targets
------------------------------------------------------------------------------ */

rule(drs([],[]),
     drs([],[I:pred(E,Sym,v,0)]),
    
     [ frametarget(I,Frame) ],

     drs([],[I:pred(E,Sym,v,1),[]:pred(E,Frame,a,99)])):-
   candc_candc_option('--framenet',training).

/*
rule(drs([],[]),
     drs([],[I:pred(E,Sym,n,0)]),
    
     [ frametarget(I,Frame) ],

     drs([],[I:pred(E,Sym,n,1),[]:pred(E,Frame,a,98)])).

rule(drs([],[]),
     drs([],[I:named(E,Sym,Type,0)]),
    
     [ frametarget(I,Frame) ],

     drs([],[I:named(E,Sym,Type,1),[]:pred(E,Frame,a,97)])).

rule(drs([],[]),
     drs([],[I:pred(E,Sym,a,0)]),
    
     [ frametarget(I,Frame) ],

     drs([],[I:pred(E,Sym,a,1),[]:pred(E,Frame,a,96)])).

rule(drs([],[]),
     drs([],[I:rel(E,X,Sym,0)]),
    
     [ frametarget(I,Frame) ],

     drs([],[I:rel(E,X,Sym,1),[]:pred(E,Frame,a,95)])).
*/


/* ------------------------------------------------------------------------------
   Framenet roles
------------------------------------------------------------------------------ */

% basic two-place relation
%
rule(drs([I2:X],[[]:pred(E,_Frame,a,99)]),
     drs([],[I1:rel(E,X,Rel,0)]),
    
     [ framerole(I2,Role) ],

     drs([],[I1:rel(E,X,Rel,1),[]:rel(E,X,Role,99)])):-
   candc_candc_option('--framenet',training).

% adverb
%
rule(drs([],[[]:pred(E,_Frame,a,99)]),
     drs([],[I1:pred(E,Sym,a,0)]),
    
     [ framerole(I1,Role) ],

     drs([],[I1:pred(E,Sym,a,1),[]:rel(E,E,Role,99)])):-
   candc_candc_option('--framenet',training).

% indirect (possessive)
%
rule(drs([I2:Y],[[]:pred(E,_Frame,a,99),[]:rel(E,X,_,99)]),
     drs([],[I1:rel(X,Y,of,0)]),
    
     [ framerole(I2,Role) ],

     drs([],[I1:rel(X,Y,of,1),[]:rel(E,Y,Role,98)])):-
   candc_candc_option('--framenet',training).


/*========================================================================
   New Refs
*/

newref(X):-
   counter(Old),
   X='$VAR'(Old),
   New is Old + 1,
   retractall(counter(_)),
   assert(counter(New)).


/*========================================================================
   Adjacent Indexes
*/

adjacent(A,B,C):-
   reverse(A,[I|_]),
   B=[J|_],
   J is I + 1,
   append(A,B,C).


/*========================================================================
   Months
*/

month('january','01').
month('jan','01').
month('jan.','01').
month('february','02').
month('february','02').
month('feb','02').
month('feb.','02').
month('march','03').
month('mar','03').
month('mar.','03').
month('april','04').
month('apr','04').
month('apr','04').
month('apr.','04').
month('may','05').
month('june','06').
month('july','07').
month('august','08').
month('aug','08').
month('aug.','08').
month('september','09').
month('sept','09').
month('sep','09').
month('sep.','09').
month('sept.','09').
month('october','10').
month('oct','10').
month('oct.','10').
month('november','11').
month('nov','11').
month('nov.','11').
month('december','12').
month('dec','12').
month('dec.','12').

earlier_day('01','02').
earlier_day('02','03').
earlier_day('03','04').
earlier_day('04','05').
earlier_day('05','06').
earlier_day('06','07').
earlier_day('07','08').
earlier_day('08','09').
earlier_day('09','10').
earlier_day('10','11').
earlier_day('11','12').
earlier_day('12','13').
earlier_day('13','14').
earlier_day('14','15').
earlier_day('15','16').
earlier_day('16','17').
earlier_day('17','18').
earlier_day('18','19').
earlier_day('19','20').
earlier_day('20','21').
earlier_day('21','22').
earlier_day('22','23').
earlier_day('23','24').
earlier_day('24','25').
earlier_day('25','26').
earlier_day('26','27').
earlier_day('27','28').
earlier_day('28','29').
earlier_day('29','30').
earlier_day('30','31').

earlier(A,B):- earlier_day(A,B), !.
earlier(A,B):- earlier_day(A,C), earlier(C,B).


earlier_doy(doy(M1,_),doy(M2,_)):-
   earlier(M1,M2), !.

earlier_doy(doy(M,D1),doy(M,D2)):-
   earlier(D1,D2), !.


nextyofm(X,X,Y,Y):- !.

nextyofm('12',M,Y1,Y3):- !,
   nextYear(Y1,Y2),
   nextyofm('01',M,Y2,Y3).

nextyofm(M1,M,Y1,Y2):-
   earlier_day(M1,M2),
   nextyofm(M2,M,Y1,Y2).
 


/*========================================================================
   Format Years
*/

yearBC(bc).
yearBC(bce).
yearBC('b.c.').
yearBC('b.c.e.').

yearAD(ad).
yearAD(ce).
yearAD('a.d.').
yearAD('c.e.').


year(Y,F):- 
   atom_chars(Y,Chars),
   ( Chars = [_], atom_chars(F,['0','0','0'|Chars]), !
   ; Chars = [_,_], atom_chars(F,['0','0'|Chars]), ! 
   ; Chars = [_,_,_], atom_chars(F,['0'|Chars]), !
   ; Chars = [_,_,_,_], atom_chars(F,Chars) ).


last_year(Year,Last):- 
   atom_codes(Year,[Y1,Y2,Y3,Y4]),
   Y1 > 47, Y1 < 58,
   Y2 > 47, Y2 < 58,
   Y3 > 47, Y3 < 58,
   Y4 > 47, Y4 < 58, !,
   number_codes(NumberYear,[Y1,Y2,Y3,Y4]),
   NumberLast is NumberYear - 1,
   number_codes(NumberLast,Codes),
   atom_codes(Last,Codes).

last_year(X,X).


/*========================================================================
   Centuries
*/

century('1st',     '00XX').
century('first',   '00XX').
century('2nd',     '01XX').
century('second',  '01XX').
century('3rd',     '02XX').
century('third',   '02XX').
century('4th','     03XX').
century('fourth',  '03XX').
century('5th',     '04XX').
century('fifth',   '04XX').
century('6th',     '05XX').
century('sixth',   '05XX').
century('7th',     '06XX').
century('seventh', '06XX').
century('8th',     '07XX').
century('eigth',   '07XX').
century('9th',     '08XX').
century('ninth',   '08XX').
century('10th',    '09XX').
century('tenth',   '09XX').
century('11th',    '10XX').
century('12th',    '11XX').
century('13th',    '12XX').
century('14th',    '13XX').
century('15th',    '14XX').
century('16th',    '15XX').
century('17th',    '16XX').
century('18th',    '17XX').
century('19th',    '18XX').
century('20th',    '19XX').
century('21th',    '20XX').


/*========================================================================
   Decades
========================================================================*/

decade(Date,Decade):-
   name(Date,[A,B,C,48]),
   name(Decade,[A,B,C,88]), !.

decade('1610s','161X').
decade('1620s','162X').
decade('1630s','163X').
decade('1640s','164X').
decade('1650s','165X').
decade('1660s','166X').
decade('1670s','167X').
decade('1680s','168X').
decade('1690s','169X').
decade('1700s','170X').
decade('1710s','171X').
decade('1720s','172X').
decade('1730s','173X').
decade('1740s','174X').
decade('1750s','175X').
decade('1760s','176X').
decade('1770s','177X').
decade('1780s','178X').
decade('1790s','179X').
decade('1800s','180X').
decade('1810s','181X').
decade('1820s','182X').
decade('1830s','183X').
decade('1840s','184X').
decade('1850s','185X').
decade('1860s','186X').
decade('1870s','187X').
decade('1880s','188X').
decade('1890s','189X').
decade('1900s','190X').
decade('1910s','191X').
decade('1920s','192X').
decade('1930s','193X').
decade('1940s','194X').
decade('1950s','195X').
decade('1960s','196X').
decade('1970s','197X').
decade('1980s','198X').
decade('1990s','199X').
decade('2000s','200X').


/*========================================================================
   Days
*/

day(1,'01').
day('1st','01').
day(2,'02').
day('2nd','02').
day(3,'03').
day('3rd','03').
day(4,'04').
day('4th','04').
day(5,'05').
day('5th','05').
day(6,'06').
day('6th','06').
day(7,'07').
day('7th','07').
day(8,'08').
day('8th','08').
day(9,'09').
day('9th','09').
day(10,'10').
day('10th','10').
day(11,'11').
day('11th','11').
day(12,'12').
day('12th','12').
day(13,'13').
day('13th','13').
day(14,'14').
day('14th','14').
day(15,'15').
day('15th','15').
day(16,'16').
day('16th','16').
day(17,'17').
day('17th','17').
day(18,'18').
day('18th','18').
day(19,'19').
day('19th','19').
day(20,'20').
day('20th','20').
day(21,'21').
day('21st','21').
day(22,'22').
day('22nd','22').
day(23,'23').
day('23rd','23').
day(24,'24').
day('24th','24').
day(25,'25').
day('25th','25').
day(26,'26').
day('26th','26').
day(27,'27').
day('27th','27').
day(28,'28').
day('28th','28').
day(29,'29').
day('29th','29').
day(30,'30').
day('30th','30').
day(31,'31').
day('31st','31').


/*========================================================================
   Hours
*/

hour(N,'a.m.',NN):- !, hour(N,am,NN).
hour(N,'p.m.',NN):- !, hour(N,pm,NN).

hour(1,am,'01'):-! .
hour(2,am,'02'):-!.
hour(3,am,'03'):-!.
hour(4,am,'04'):-!.
hour(5,am,'05'):-!.
hour(6,am,'06'):-!.
hour(7,am,'07'):-!.
hour(8,am,'08'):-!.
hour(9,am,'09'):-!.
hour(10,am,'10'):-!.
hour(11,am,'11'):-!.
hour(12,am,'12'):-!.

hour(1,pm,'13'):-! .
hour(2,pm,'14'):-!.
hour(3,pm,'15'):-!.
hour(4,pm,'16'):-!.
hour(5,pm,'17'):-!.
hour(6,pm,'18'):-!.
hour(7,pm,'19'):-!.
hour(8,pm,'20'):-!.
hour(9,pm,'21'):-!.
hour(10,pm,'22'):-!.
hour(11,pm,'23'):-!.
hour(12,pm,'00'):-!.

minute(Min,MinID):-
   atom_chars(Min,[M1,M2]),
   atom_chars(MinID,[M1,M2]).


gmt(GMT,Hour,Minutes):-
   atom_chars(GMT,[H1,H2,M1,M2]),
   atom_chars(Hour,[H1,H2]),
   atom_chars(Minutes,[M1,M2]).


/*========================================================================
   Dimensions
*/

dimension(big,size,1).
dimension(small,size,1). 
dimension(large,size,1).
dimension(measurement,size,1).
dimension(close,distance,1).
dimension(far,distance,1).
dimension(cold,temperature,1).
dimension(hot,temperature,1).
dimension(deep,depth,1).
dimension(fast,speed,1).
dimension(heavy,weight,1).
dimension(high,height,1).
dimension(tall,height,1).
dimension(late,timing,1).
dimension(long,length,1).
dimension(wide,width,1).
dimension(broad,breadth,1).
dimension(often,frequency,1).
dimension(old,age,1).
dimension(young,age,1).


/*========================================================================
   Quoted strings
*/

quoted(Pred):-
   atom(Pred), 
   atom_chars(Pred,[A,B,C,D|_]), 
   member(t(A,B,C,D),[t('l','q','o','-'),
                      t('L','Q','O','-'),
                      t('b','m','v','_'),
                      t('B','M','V','_')]), !.


/*========================================================================
   URL 
*/

url(Pred):-
   atom(Pred),
   atom_chars(Pred,Codes),  
   ( Codes = [w,w,w,_,_,_|_] ; Codes = [h,t,t,p,_,_,_,_|_] ), !.


/*========================================================================
   Email 
*/

email(Pred):-
   atom(Pred),
   atom_chars(Pred,Codes),  
   member('.',Codes),
   member('@',Codes), !.


/*========================================================================
   Postprocessing Words
*/

ppWords([],_,[],[]).

ppWords([word(I,W1)|L1],OldPos,[word(I,W2)|L2],[word(I,W2,OldPos,NewPos)|L3]):-
   atom(W1), 
   atom_chars(W1,C1),
   length(C1,Len),
   ( C1=['L','Q','O','-'|C3]; C1=['B','M','V','_'|C3] ), !,
   convertQuotes(C3,C2),      
   atom_chars(W2,C2),
   NewPos is OldPos + Len - 1,
   NextPos is NewPos + 2,
   ppWords(L1,NextPos,L2,L3).

ppWords([word(I,W)|L1],OldPos,[word(I,W)|L2],[word(I,W,OldPos,NewPos)|L3]):-
   atom_codes(W,C1), 
   removeSpecialChars(C1,C2),
   length(C2,Len),
   NewPos is OldPos + Len - 1,
   NextPos is NewPos + 2,
   ppWords(L1,NextPos,L2,L3).

ppWords([word(I,W,P1,P2)|L1],Pos,[word(I,W)|L2],[word(I,W,P1,P2)|L3]):-
   ppWords(L1,Pos,L2,L3).


removeSpecialChars([],[]).
removeSpecialChars([194|L1],L2):- !, removeSpecialChars(L1,L2).
removeSpecialChars([195|L1],L2):- !, removeSpecialChars(L1,L2).
removeSpecialChars([X|L1],[X|L2]):- !, removeSpecialChars(L1,L2).


/*========================================================================
   Postprocessing quoted strings
*/

convertQuotes([],[]).

convertQuotes(['-','R','Q','O'|L1],L2):- !,
   convertQuotes(L1,L2).

convertQuotes(['_','E','M','V'|L1],L2):- !,
   convertQuotes(L1,L2).

convertQuotes(['-'|L1],[' '|L2]):- !,
   convertQuotes(L1,L2).

convertQuotes(['_'|L1],[' '|L2]):- !,
   convertQuotes(L1,L2).

convertQuotes([X|L1],[X|L2]):-
   convertQuotes(L1,L2).
