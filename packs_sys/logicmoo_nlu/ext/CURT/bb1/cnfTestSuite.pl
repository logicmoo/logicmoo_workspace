/*************************************************************************

    File: cnfTestSuite.pl
    Copyright (C) 2004 Patrick Blackburn & Johan Bos

    This file is part of BB1, version 1.2 (August 2005).

    BB1 is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    BB1 is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with BB1; if not, write to the Free Software Foundation, Inc., 
    59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

*************************************************************************/

:- module(cnfTestSuite,[formulaClause/2]).


/*========================================================================
   Formulas and known cnf forms for them.
========================================================================*/

formulaClause(imp(p,q),[[not(p),q]]).

formulaClause(imp(and(p,imp(q,r)),s),[[s,not(p),q],[s,not(p),not(r)]]).

formulaClause(or(not(or(not(p),q)),or(not(r),p)),[[p,not(r)],[not(q),not(r),p]]).

formulaClause(imp(imp(not(p),q),imp(not(r),s)),[[not(p),r,s],[not(q),r,s]]).
