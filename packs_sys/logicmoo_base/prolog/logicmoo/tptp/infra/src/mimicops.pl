/*
* Copyright (C) 2002, 2007 Christoph Wernhard
* 
* This program is free software; you can redistribute it and/or modify it
* under the terms of the GNU General Public License as published by the Free
* Software Foundation; either version 2 of the License, or (at your option)
* any later version.
* 
* This program is distributed in the hope that it will be useful, but WITHOUT
* ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
* FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
* more details.
* 
* You should have received a copy of the GNU General Public License along with
* this program; if not, see <http://www.gnu.org/licenses/>.
*/

:- module(mimicops, []).


:-(op(500, fx, :)).
:-(op(1200, xfx, +=)).

:-(op(0, xfx, ==)).
:-(op(0, fx, (dynamic))).
:-(op(0, xfx, is)).
:-(op(0, fx, ?)).
:-(op(0, yfx, ?)).
:-(op(0, xfy, (;))).
:-(op(0, fx, -)).
:-(op(0, yfx, -)).
:-(op(0, xfx, **)).
:-(op(0, xfy, (|))).
:-(op(0, fx, (:-))).
:-(op(0, xfx, (:-))).
:-(op(0, xfx, =..)).
:-(op(0, fx, (?-))).
:-(op(0, fx, \)).
:-(op(0, xfy, ^)).
:-(op(0, xfx, >)).
:-(op(0, xfx, >=)).
:-(op(0, fy, \+)).
:-(op(0, xfx, \=)).
:-(op(0, yfx, *)).
:-(op(0, yfx, <<)).
:-(op(0, fx, $)).
:-(op(0, yfx, xor)).
:-(op(0, yfx, /\)).
:-(op(0, xfx, (-->))).
:-(op(0, fx, @)).
:-(op(0, xfx, \==)).
:-(op(0, yfx, >>)).
:-(op(0, xfy, (->))).
:-(op(0, xfx, <)).
:-(op(0, xfx, =\=)).
:-(op(0, xfx, =:=)).
:-(op(0, xfx, =<)).
:-(op(0, xfx, :=)).
:-(op(0, xfy, (*->))).
:-(op(0, yfx, rem)).
:-(op(0, fx, (volatile))).
:-(op(0, fx, +)).
:-(op(0, yfx, +)).
:-(op(0, xfx, @>=)).
:-(op(0, xfx, @>)).
:-(op(0, yfx, mod)).
:-(op(0, xfx, =@=)).
:-(op(0, fx, (initialization))).
:-(op(0, fx, (discontiguous))).
:-(op(0, yfx, \/)).
:-(op(0, fx, (multifile))).
:-(op(0, yfx, //)).
:-(op(0, yfx, /)).
:-(op(0, xfx, @=<)).
:-(op(0, fx, (module_transparent))).
:-(op(0, xfx, @<)).
:-(op(0, xfx, (::=))).
:-(op(0, fx, (meta_predicate))).
:-(op(0, xfx, \=@=)).


