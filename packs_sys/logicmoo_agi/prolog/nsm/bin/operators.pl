/*
    This file is part of NSM-DALIA, an extensible parser and generator
    for NSM grammars.
       
    Copyright (C) 2009 Francesco Zamblera.

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.

*/

    
    

:- op(700,xfx,<>).
:- op(1100,xfx,=>).
:- op(500,xfx,//).
:- op(850,fx,*).
:- op(850,fx,?).
:- op(870,fx,~).
% :- op(870,fx,<*).
% :- op(870,xf,*>).
:- op(800,xfx,===).
:- op(900,xfy,::).
:- op(700,fx,prolog_pred).
:- op(740,xfx,:).
:- op(740,xfx,:::).
:- op(700,xfx,/).
:- op(700,xfx,\).
:- op(700,xfx,<<).
:- op(800,xfx,cat).
:- op(500,xfx,isa).
:- op(550,xfy,--->).
:- op(730,fx,wfr).

/* TOKI PONA */
:- op(550,xfy,>>).
:- op(550,xfy,=>>).

/* DEPENDENCY */
:- op(730,xfy,dr).
:- op(730,xfy,dar).
:- op(720,xfx,==>).
:- op(700,fx,r_doc).
:- op(700,fx,r_a).
:- op(700,fx,r_t).
:- op(700,fx,r_c).

:- op(700,xfx,gtitle).
:- op(700,fx,gdate).
:- op(700,fx,gauthor).
:- op(700,fx,gversion).
:- op(700,fx,gackn).

:- op(700,xfx,gabstract).
:- op(700,xfx,glevels).
:- op(700,xfx,gprologue).
:- op(700,xfx,gtranscr).
:- op(700,xfx,gmorph).
:- op(700,xfx,gsynt).
:- op(700,xfx,gdep_intro).
:- op(700,xfx,gepilogue).


:- discontiguous(':::'/2).
:- discontiguous('::'/2).
:- discontiguous(m/3).
:- discontiguous(dr/2).
