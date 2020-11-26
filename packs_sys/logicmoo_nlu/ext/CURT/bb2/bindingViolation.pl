/*************************************************************************

    File: bindingViolation.pl
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

:- module(bindingViolation,[bindingViolationDrs/1]).

:- use_module(comsemPredicates,[compose/3]).


/*========================================================================
   DRS with no Binding Violation
========================================================================*/

bindingViolationDrs(drs(_,C)):-
   bindingViolationConds(C).

bindingViolationDrs(merge(B1,B2)):-
   (
      bindingViolationDrs(B1), !
   ;
      bindingViolationDrs(B2)
   ).

bindingViolationConds([not(B)|L]):-
   (
      bindingViolationDrs(B), !
   ;
      bindingViolationConds(L)
   ), !.

bindingViolationConds([imp(B1,B2)|L]):-
   (
      bindingViolationDrs(B1), !
   ;
      bindingViolationDrs(B2), !
   ;
      bindingViolationConds(L)
   ), !.


bindingViolationConds([or(B1,B2)|L]):-
   (
      bindingViolationDrs(B1), !
   ;
      bindingViolationDrs(B2), !
   ;
      bindingViolationConds(L)
   ), !.

bindingViolationConds([Basic|L]):-
   (
      Basic = Cond:[ref:no],
      compose(Cond,_,[X,Y]),
      X==Y, !
   ;
      Basic = Cond:[ref:yes],
      compose(Cond,_,[X,Y]),
      \+ X==Y, !
   ;
      bindingViolationConds(L)
   ).
   
