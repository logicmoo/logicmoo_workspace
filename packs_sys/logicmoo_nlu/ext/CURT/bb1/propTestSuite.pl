/*************************************************************************

    File: propTestSuite.pl
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

:- module(propTestSuite,[formula/2]).


/*========================================================================
   Formulas
========================================================================*/

formula(imp(p, q), 'Not a theorem').

formula(imp(p, p), 'Theorem').

formula(imp(imp(imp(p,q),r),imp(imp(p,q),imp(p,r))), 'Theorem').

formula(imp(p,imp(q,p)), 'Theorem').

formula(imp(imp(p,q),imp(not(q),not(p))), 'Theorem').

formula(imp(imp(not(p),not(q)),imp(q,p)), 'Theorem').

formula(imp(not(not(p)),p), 'Theorem').

formula(imp(p,not(not(p))), 'Theorem').

formula(imp(not(not(not(not(p)))),p), 'Theorem').

formula(imp(p,not(not(not(not(p))))), 'Theorem').

formula(imp(not(or(p,q)),and(not(p),not(q))), 'Theorem').

formula(imp(and(not(p),not(q)),not(or(p,q))), 'Theorem').

formula(imp(not(and(p,q)),or(not(p),not(q))), 'Theorem').

formula(imp(and(not(p),not(q)),not(or(p,q))), 'Theorem').

formula(imp(or(r,and(p,q)),and(or(r,p),or(r,q))), 'Theorem').

formula(imp(and(or(r,p),or(r,q)),or(r,and(p,q))), 'Theorem').

formula(imp(and(r,or(p,q)),or(and(r,p),and(r,q))), 'Theorem').

formula(imp(or(and(r,p),and(r,q)),and(r,or(p,q))), 'Theorem').

formula(imp(and(imp(p,r),imp(q,s)),
            imp(and(p,q),and(r,s))), 'Theorem').

formula(imp(p,or(p,q)), 'Theorem').

formula(imp(p,or(q,p)), 'Theorem').

formula(imp(and(p,q),p), 'Theorem').

formula(imp(and(p,q),q), 'Theorem').

formula(imp(or(p,not(q)),or(p,q)), 'Not a theorem').

formula(imp(and(or(p,not(p)),or(p,not(q))),
            or(p,q)), 'Not a theorem').

formula(imp(and(and(or(p,not(p)),or(r,not(r))),or(p,not(q))),
            or(p,q)), 'Not a theorem').

formula(imp(or(p,and(not(p),q)),or(p,q)), 'Theorem').

formula(and(imp(or(p,and(not(p),q)),or(p,q)),
            imp(or(p,q),or(p,and(not(p),q)))), 'Theorem').


/*------------------------------------------------------------------------
     Pigeonehole principle with 3 pigeons and 2 pigeonholes. 
------------------------------------------------------------------------*/

formula(imp(and(and(or(p11,p12),or(p21,p22)),or(p31,p32)),
            or(or(or(or(or(and(p11,p21),and(p11,p31)),and(p21,p31)),
               and(p12,p22)),and(p12,p32)),and(p22,p32))), 'Theorem').


/*------------------------------------------------------------------------
     The following problem formula codes the pigeonehole principle
     with 4 pigeons and 3 pigeonholes. Neither the tableau nor
     resolution prover terminates on this example. For further
     discussion, see the last section of Chapter 4.
------------------------------------------------------------------------*/

formula(imp(and(and(and(or(or(p11,p12),p13),or(or(p21,p22),p23)),
            or(or(p31,p32),p33)),or(or(p41,p42),p43)),
               or(or(or(or(or(or(or(or(or(or(or(or(or(or(or(or(or(and(p11,p21),
                  and(p11,p31)),and(p11,p41)),and(p21,p31)),and(p21,p41)),
                      and(p31,p41)),and(p12,p22)),and(p12,p32)),and(p12,p42)),
                          and(p22,p32)),and(p22,p42)),and(p32,p42)),and(p13,p23)),
                              and(p13,p33)),and(p13,p43)),and(p23,p33)),
                                  and(p23,p43)),and(p33,p43))), 'Theorem').

formula(imp(and(or(or(or(p11,p12),p13),p14),
                and(or(or(or(p21,p22),p23),p24),
                    and(or(or(or(p31,p32),p33),p34),
                        and(or(or(or(p41,p42),p43),p44),
                            or(or(or(p51,p52),p53),p54))))),
            or(and(p11,p21),
               or(and(p11,p31),
                  or(and(p11,p41),
                     or(and(p11,p51),
                        or(and(p21,p31),
                           or(and(p21,p41),
                              or(and(p21,p51),
                                 or(and(p31,p41),
                                    or(and(p31,p41),
                                       or(and(p41,p51),
                                          or(and(p12,p22),
                                             or(and(p12,p32),
                                                or(and(p12,p42),
                                                   or(and(p12,p52),
                                                      or(and(p22,p32),
                                                         or(and(p22,p42),
                                                            or(and(p22,p52),
                                                               or(and(p32,p42),
                                                                  or(and(p32,p42),
                                                                     or(and(p42,p52),
                                                                        or(and(p13,p23),
                                                                           or(and(p13,p33),
                                                                              or(and(p13,p43),
                                                                                 or(and(p13,p53),
                                                                                    or(and(p23,p33),
                                                                                       or(and(p23,p43),
                                                                                          or(and(p23,p53),
                                                                                             or(and(p33,p43),
                                                                                                or(and(p33,p43),
                                                                                                   or(and(p43,p53),
                                                                                                      or(and(p14,p24),
                                                                                                         or(and(p14,p34),
                                                                                                            or(and(p14,p44),
                                                                                                               or(and(p14,p54),
                                                                                                                  or(and(p24,p34),
                                                                                                                     or(and(p24,p44),
                                                                                                                        or(and(p24,p54),
                                                                                                                           or(and(p34,p44),
                                                                                                                              or(and(p34,p44),
                                                                                                                                 or(and(p44,p54)))))))))))))))))))))))))))))))))))))))))), 'Theorem').
