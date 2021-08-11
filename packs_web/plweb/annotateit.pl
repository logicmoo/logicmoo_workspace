/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2013, VU University Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/


:- module(annotateit,
	  [ convert_annotations/0
	  ]).
:- use_module(library(persistency)).

/** <module> Convert old annotations

*/


		 /*******************************
		 *	       DATA		*
		 *******************************/

:- persistent
	annotation(object:any,			% Object attached to
		   annotation:atom,		% Text of the annotation
		   time:integer,		% When was it tagged
		   user:atom).			% User that added the tag


:- initialization
	db_attach('annotations.db',
		  [ sync(close)
		  ]).

%%	convert_annotations
%
%	Convert the old annotations to the new format.  Simply load this
%	file and run convert_annotations/0.

convert_annotations :-
	forall(annotation(Object, Text, Created, User),
	       ( atom_string(Text, Content),
		 uuid(PostId),
		 post:assert_post(PostId,
				  _{kind: annotation,
				    content: Content,
				    meta:_{id:PostId,
					   author:User,
					   object:Object,
					   time:_{created:Created}
					  }
				   }))).
