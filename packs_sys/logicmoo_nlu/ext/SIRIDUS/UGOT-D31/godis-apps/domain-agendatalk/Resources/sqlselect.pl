/***********************************************************************

 
         name: sqlselect.pl
  description: Interacts with mysql (using mysql.pl)
               Select command
	       Delete command
	       Count command
       author: Peo Grimheden and Maria Håkansson
               modified by Rebecca Jonson

***************************************************************************/


:-module(sqlselect,[consultDB2/3, print_query/4, countLIKEDB/4, deleteDB/3]).

:- use_module(sqlinsert).
:- use_module(sqlselect).
:- use_module(library(system)).
:- use_module(library(charsio)).
:- use_module(library(lists)).
:- use_module(mysql,[mysql/3]).
:- use_module(library(terms)).

/*=====================================================================
     consultDB( +Beliefs, +Query, -Answer )
     -- Returns (if it succeeds) an Answer to a Query given
        background Beliefs and a Domain
=======================================================================*/

consultDB2(Whe, _^Ans, Ans):-
	pick_select(Ans, Sel),
	reverse(Whe,Ehw),
	pick_table1(Ehw,Fro),
	delete(Fro,[],From),
	print_query(Sel, From, Whe, Ans).

/*=====================================================================
     countLIKEDB( +Table, +Column, -Value,Answer )
     -- Returns (if it succeeds) a number of units having value like Value
     in column Column
=======================================================================*/
countLIKEDB(Table,Column,set(Values),Ans):-
	print_query_count(Table,Column,Values,Ans).

deleteDB(Table,set(Values),Ans):-
	print_query_delete(Table,Values,Ans).
/*==================================================
    pick_select(+Query, -DBColumn)
    -- Picks the right database column
==================================================*/

pick_select(X=_, Sel):-
	Sel= X.

/*==================================================
    Pick_table1(+BelievesList, -DBTable)
    -- Picks the right database table
===================================================*/

pick_table1([],[]).
pick_table1([X|Xs],[Y|Ys]):-
	pick_table(X,Y),
	(Y=[] ->
	pick_table1(Xs,Ys)
	;
	    !).
/*==================================================
    pick_table(+BelievesElement, -Task)
    -- Returns the task value
===================================================*/
	
	

pick_table([],[]).
pick_table(X=A,Y):-
	(X == task -> Y = A
	;
	    !).

/*===========================================================================
    print_query(+Select, +From, +Where, -Ans
    -- Constructs an SELECT SQL query, calls the mysql module and returns the answer
=============================================================================*/

print_query(Select, [From], Where,Ans):-
        SQLSel = "SELECT ",
	atom_to_chars(Select,Sel),
	Fro = " FROM ",
	atom_to_chars(From,From1),
	SQLWhe = " WHERE ",
	%priceWhereF(Where, From1, Wher), %Not necessary when DB is known
	controlBelievesList(Where,Whe),
	atom_chars(Whe,WheChar),
	append(SQLSel,Sel,SelList),
	append(SelList,Fro,FroList),
	append(FroList,From1,FroList1),
	append(FroList1,SQLWhe,WheList),
	append(WheList,WheChar,TempList),
	atom_chars(SQL,TempList),
	mysql(SQL,Temp,[]),
	getanswer(Temp,Ans,Select).
	%answer2atom(Temp,[TempAns]),
	%string2atom(Temp,[TempAns]),
	%subsumes(Ans,(Select=TempAns)).

print_query(Select, [From], Where,Select=empty).

getanswer([],Select=[empty],Select).
getanswer(Temp,Ans,Select):-
	answer2atom(Temp,[TA]),
	subsumes(Ans,(Select=TA)).
answer2atom([],[]).
%answer2atom(Temp,T):-
	%string2atom(Temp,T).
answer2atom(Temp,[T]):-
	string2atom(Temp,T).
	%answer2atom(TRest,TR).
/*===========================================================================
    print_query_count(+Select, +From, +Where, -Ans
    -- Constructs a SELECT COUNT(col) SQL query, calls the mysql module and returns the answer
=============================================================================*/

print_query_count(Table,Column,Values,Ans):-
        SQLSel = "SELECT COUNT",
	%atom_to_chars(Columns,Cols),
	Fro = " FROM ",
	atom_to_chars(Table,Table1),
	SQLWhe = " WHERE ",
	atom_to_chars(Column,Col),
	%priceWhereF(Where, From1, Wher), %Not necessary when DB is known
	controlBelievesList(Values,Whe),
	atom_chars(Whe,WheChar),
	append(SQLSel,[40|Col],ColList),
	append(ColList,[41],SelList),
	append(SelList,Fro,FroList),
	append(FroList,Table1,FroList1),
	append(FroList1,SQLWhe,WheList),
	append(WheList,WheChar,TempList),
	atom_chars(SQL,TempList),
	mysql(SQL,Temp,[]),
	string2atom(Temp,[Ans]).
	%subsumes(Ans,(Column=TempAns)).

/*===========================================================================
    print_query_delete(+Table, +Where, -Ans)
    -- Constructs a DELETE SQL query , calls the mysql module and returns the answer
=============================================================================*/

print_query_delete(Table, Where,Ans):-
        SQLDel = "DELETE FROM ",
	atom_to_chars(Table,Table1),
	SQLWhe = " WHERE ",
	controlBelievesList(Where,Whe),
	atom_chars(Whe,WheChar),
	append(SQLDel,Table1,DelList),
	append(DelList,SQLWhe,WheList),
	append(WheList,WheChar,TempList),
	atom_chars(SQL,TempList),
	mysql(SQL,Ans,[]).
	%string2atom(Temp,[Ans]).



/*===========================================================
    priceWhereF(+BelList, +DBTable, +WhereList)
    -- Returns list of 'WHERE specifications' for SQL-query
=============================================================*/

priceWhereF(A, From, B):-
	priceWhere(A, From, C),
	delete(C,[]=[],B).

/*===========================================================
    priceWhere(+BelList, +DBTable, +WhereList)
    -- Returns list of 'WHERE specifications' for SQL-query
    (polluted with empty lists)
=============================================================*/


priceWhere([], _, []).
priceWhere([X|Xs], From, [Y|Ys]):-
	checkRelevant(X, From, Y),
	priceWhere(Xs, From, Ys).

/*==================================================
    chekRelevant(+BelElem, +DBTable, +RelElem
    -- Returns believes element if relevant,
    otherwise it returns [] = []
===================================================*/

checkRelevant([], _, []).
checkRelevant(X=A, From, Y=B):-
	(inList(X, From) -> Y = X, B = A
	;
	    Y = [], B = []).


/*==================================================
    inList(+Col, +From)
    -- Succeeds if Col is column in relevant table
===================================================*/

inList(Col, From):-
	SQLShow = "show columns from ",
	append(SQLShow, From, SQL),
	atom_chars(Query,SQL),
	mysql(Query, CharAns),
	makeList(CharAns,List),
	atom_chars(Col,Char),
	%append([120],Char,ScoreChar),
	member(Char,List).

/*==================================================
    makeList(+ListOfLists, -Heads)
    -- Return a list of heads from a list of lists
===================================================*/

makeList([],[]).
makeList([X|Xs],[Y|Ys]):-
	getHead(X,Y),
	makeList(Xs,Ys).



/*==================================================
    getHead(+HeadTail, -Head)
===================================================*/

getHead([X|_],X).

/*==================================================
    controlBelievesList(+RelBelList, -PrepList)
    -- Takes a list of relevant believes and
    returns SQL prepared WHERE list
===================================================*/


controlBelievesList([],[]).
controlBelievesList(Xs,Ys):-
        atomList2stringList(Xs, Zs),
        megaList(Zs,Ws),
        atom_chars(Ys, Ws).


/*==================================================
    atomList2stringList(+AtomList, +StringList)
    -- Takes a list of atoms and returns a list of
    strings seperated by ' and '
===================================================*/
 
atomList2stringList([],[]).
atomList2stringList([A|As], [S|Ss]):-
        prepareAtom(A,C),
        atom_chars(C,X),
        (As == [] -> S = X
        ;
            append(X,[32,97,110,100,32],S)),
        atomList2stringList(As,Ss).
 
 
/*============================================================
    megaList(+ListofLists, -List)
    -- Returns the concatenation of all lists in list of list
==============================================================*/

megaList(Xs,Ys):- megaList(Xs,[],Ys).
 
megaList([],Ys,Ys).
megaList([X|Xs],Zs,Ys):-
        append(Zs, X, Qs),
        megaList(Xs, Qs, Ys).
 
/*==================================================
    prepareAtom(+WheEq, -PrepWheEq)
    -- takes a string like 'class=economy'
    and returns an atom like xclass="economy"
===================================================*/




prepareAtom(X=Y,Atom):-
        name(X,Chars1),
        name(Y,Chars2),
        append(Chars1,[61],TempList),
	append(TempList,[34|Chars2],Chars2Fnutt),
	append(Chars2Fnutt,[34],FinalList),
        atom_chars(Atom,FinalList).

%%X>>Y --> X LIKE "Y%"
prepareAtom(X>>Y,Atom):-
        name(X,Col),
        name(Y,Val),
        append(Col,[32,76,73,75,69,32],TempList),
	append(TempList,[34|Val],Chars2Fnutt),
	append(Chars2Fnutt,[37],Like),
	append(Like,[34],FinalList),
        atom_chars(Atom,FinalList).

%%X<<Y --> X LIKE "%Y"
prepareAtom(X>>Y,Atom):-
        name(X,Col),
        name(Y,Val),
        append(Col,[32,76,73,75,69,32],TempList),
	append(TempList,[34,37|Val],Like),
	append(Like,[34],FinalList),
        atom_chars(Atom,FinalList).

prepareAtom(X<Y,Atom):-
        name(X,Chars1),
        name(Y,Chars2),
        append(Chars1,[60],TempList),
	append(TempList,[34|Chars2],Chars2Fnutt),
	append(Chars2Fnutt,[34],FinalList),
        atom_chars(Atom,FinalList).

prepareAtom(X>Y,Atom):-
        name(X,Chars1),
        name(Y,Chars2),
        append(Chars1,[62],TempList),
	append(TempList,[34|Chars2],Chars2Fnutt),
	append(Chars2Fnutt,[34],FinalList),
        atom_chars(Atom,FinalList).

prepareAtom(X=<Y,Atom):-
        name(X,Chars1),
        name(Y,Chars2),
        append(Chars1,[60,61],TempList),
	append(TempList,[34|Chars2],Chars2Fnutt),
	append(Chars2Fnutt,[34],FinalList),
        atom_chars(Atom,FinalList).

prepareAtom(X>=Y,Atom):-
        name(X,Chars1),
        name(Y,Chars2),
        append(Chars1,[62,61],TempList),
	append(TempList,[34|Chars2],Chars2Fnutt),
	append(Chars2Fnutt,[34],FinalList),
        atom_chars(Atom,FinalList).

/*==================================================
    string2atom(+CharList, -AtomList)
    -- Takes a list of list of chars
    returns list of atoms
===================================================*/

string2atom([],[]).
string2atom([[L]|Ls], [A|As]):-
	member(13,L),
	delete(L,13,LNew),
        name(A,LNew),
        string2atom(Ls,As).
string2atom([[L]|Ls], [A|As]):-
	name(A,L),
        string2atom(Ls,As).
















