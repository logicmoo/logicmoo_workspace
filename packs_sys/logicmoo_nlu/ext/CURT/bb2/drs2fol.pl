/*************************************************************************

    File: drs2fol.pl
    Copyright (C) 2004 Patrick Blackburn & Johan Bos

    This file is part of BB2, version 1.0 (June 2004).

    BB2 is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    BB2 is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with BB2; if not, write to the Free Software Foundation, Inc., 
    59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

*************************************************************************/

:- module(drs2fol,[drs2fol/2]).

:- use_module(comsemPredicates,[basicFormula/1]).

/*========================================================================
   Translate DRSs into FOL formulas 
========================================================================*/
drs2fol(drs([],[]),true):-!.

drs2fol(drs([],[Cond]),Formula):-
   cond2fol(Cond,Formula).

drs2fol(drs([],[Cond1,Cond2|Conds]),and(Formula1,Formula2)):-
   cond2fol(Cond1,Formula1),
   drs2fol(drs([],[Cond2|Conds]),Formula2).

drs2fol(drs([X|Referents],Conds),some(X,Formula)):-
   drs2fol(drs(Referents,Conds),Formula).


/*========================================================================
   Translate DRS-Conditions into FOL formulas 
========================================================================*/

cond2fol(bim(drs([X|L],Conds),Drs),all(X,Formula)):-
   drs2fol(drs(L,Conds),F1),
   drs2fol(Drs,F2),
   Formula=and(imp(F1,F2),imp(F2,F1)).

cond2fol(not(Drs),not(Formula)):-
   drs2fol(Drs,Formula).

cond2fol(or(Drs1,Drs2),or(Formula1,Formula2)):-
   drs2fol(Drs1,Formula1),
   drs2fol(Drs2,Formula2).

cond2fol(imp(drs([],Conds),Drs2),imp(Formula1,Formula2)):-
   drs2fol(drs([],Conds),Formula1),
   drs2fol(Drs2,Formula2).

cond2fol(imp(drs([X|Referents],Conds),Drs2),all(X,Formula)):-
   cond2fol(imp(drs(Referents,Conds),Drs2),Formula).

cond2fol(Basic,Formula):-
   basicFormula(Basic),
   Formula=Basic.

cond2fol(Basic:_,Formula):-
   basicFormula(Basic),
   Formula=Basic.


/*========================================================================
   Info
========================================================================*/

info:-
   format('~n> ------------------------------------------------------------------ <',[]),
   format('~n> drs2fol.pl, by Patrick Blackburn and Johan Bos                     <',[]),
   format('~n>                                                                    <',[]),
   format('~n> ?- drs2fol(DRS,FOL).    - translated a DRS into a FOL formula      <',[]),
   format('~n> ------------------------------------------------------------------ <',[]),
   format('~n~n',[]).


/*========================================================================
   Display info at start
========================================================================*/

:- info.

