/***********************************************************************

 
         name: sqlinsert.pl
  description: Interacts with mysql (using mysql.pl)
               Constructs an Insert command
       author: Peo Grimheden and Maria Håkansson
               modified by Rebecca Jonson

***************************************************************************/


:- module(sqlinsert,[insertDB2/3]).
:- use_module(sqlselect).
:- use_module(library(system)).
:- use_module(library(charsio)).
:- use_module(library(lists)).
:- use_module(mysql).

/*=====================================================================
     insertDB( +Table, +Values, -Answer )
     -- Is supposed to return Answer if insertion succeeds
=======================================================================*/

insertDB2(To,set(Values),Answer):-
        splitIn2(Values,Columns,Vals),
        megaList(Columns, ConcColumns),
        megaList(Vals, ConcVals),
        print_insert(To, ConcColumns, ConcVals,AnswerC),
	%AnswerC = "Insertion completed.",
	atom_chars(Answer,AnswerC).

/*===========================================================================
    print_insert(+Table, +ListOfColums, +ListOfValues, -Answer
    -- Constructs an SQL query, calls the mysql module and returns the answer
=============================================================================*/

print_insert(To, ConcColumns, ConcVals, AnswerC):-
        INSERT = "INSERT INTO ",
        atom_to_chars(To, AsciiTo),
        VAL = " VALUES ",
        append(INSERT,AsciiTo,X),
        append(X,[40|ConcColumns],Y),
        append(Y,[41],Z),
        append(Z,VAL,W),
        append(W,[40|ConcVals],Q),
	append(Q,[41],V),
        atom_chars(SQL,V),
        mysql(SQL,AnswerC,[]).



/*============================================================
    splitIn2(+ValueList, -ColList, -ValList)
    -- Takes a list similar to believeslist and
    returnsseparated lists of colums and values
    with values surrounded with ""
=============================================================*/

splitIn2([],[],[]).
splitIn2([X=Y|List],[Col|Cols],[QuoteValQuote|Vals]):-
        name(X,Col),
        name(Y,Val),
        quotefier(Val,QuoteValQuote),
        splitIn2(List,Cols,Vals).

/*==================================================
    quotefier(+Val, -QuoteValQuote)
    -- provides a value with quotation marks
===================================================*/

quotefier(Val,QuoteValQuote):-
        append([34|Val],[34],QuoteValQuote).


/*============================================================
    megaList(+ListofLists, -List)
    -- Returns the concatenation of all lists in list of list
    values separated with comma
==============================================================*/

megaList(Xs,Ys):- megaList(Xs,[],Ys).
 
megaList([],Ys,Ys).
megaList([X|Xs],Zs,Ys):-
	(Xs == [] -> W = X
	;
	    append(X, [44], W)),    
	    append(Zs, W, Qs),   
	    megaList(Xs,Qs,Ys).










