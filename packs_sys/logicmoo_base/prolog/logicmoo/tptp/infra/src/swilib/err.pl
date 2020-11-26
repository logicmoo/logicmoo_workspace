/*
* Copyright (C) 2002, 2003 Christoph Wernhard
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
* this program; if not, write to the Free Software Foundation, Inc., 59 Temple
* Place, Suite 330, Boston, MA 02111-1307 USA
*/

:- module(err, [msg/1, msg/2, err/1, err/2]).

msg(Format) :-
	msg(Format , []).
msg(Format, Args) :-
	\+ \+ ( numbervars(Args, 0, _),
	        format(user_error, Format, Args) ),
	nl(user_error),
	flush_output(user_error).

err(Format) :-
	err(Format, []).

err(Format, Args) :-
	throw(error(format(Format,Args), _Context)).







