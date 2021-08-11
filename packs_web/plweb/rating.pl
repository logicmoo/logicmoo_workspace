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

:- module(rating,
	  [ rate//1
	  ]).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_dispatch), []).
:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).
:- use_module(library(option)).

:- html_resource(jq('jRating.jquery.js'),
		 [ requires([ jquery,
			      jq('jRating.jquery.css')
			    ])
		 ]).


/** <module> Provide a star-rating widget

@see http://www.myjqueryplugins.com/jquery-plugin/jrating
*/

rate(Options) -->
	{ option(class(Class), Options, jrating),
	  select_option(data_id(Id), Options, Options1, rating),
	  (   select_option(data_average(Avg), Options1, Options2)
	  ->  Extra = ['data-average'(Avg)]
	  ;   Extra = [],
	      Options2 = Options
	  )
	},
	html_requires(jq('jRating.jquery.js')),
	html(div([ class(Class), 'data-id'(Id)| Extra], [])),
	(   { option(post(Post), Options2) }
	->  html_post(Post, \script(Options2))
	;   script(Options2)
	).

script(Options) -->
	{ option(length(Length), Options, 5),
	  option(rate_max(RateMax), Options, 20),
	  option(step(Step), Options, false),
	  option(type(Type), Options, big),
	  option(class(Class), Options, jrating),
	  option(can_rate_again(CanRateAgain), Options, false),
	  http_absolute_location(jq('icons/stars.png'), BSP, []),
	  http_absolute_location(jq('icons/small.png'), SSP, [])
	},
	html(script(type('text/javascript'),
		    [ \[ '$(document).ready(function(){\n',
			 '$(".',Class,'").jRating(\n',
			 '   { bigStarsPath:"',BSP,'",\n',
			 '     smallStarsPath:"',SSP,'",\n',
			 '     step:',Step,',\n',
			 '     type:"',Type,'",\n',
			 '     length:',Length,',\n',
			 '     rateMax:',RateMax,',\n',
			 '     canRateAgain:',CanRateAgain,',\n'
		       ],
		      \set_disabled(Options),
		      \set_action(Options),
		      \set_field(Options),
		      \[ '   });\n',
			 '});\n'
		       ]
		    ])).

set_disabled(Options) -->
	{ option(disabled(true), Options) }, !,
	html(\[ '     isDisabled:true,\n'
	      ]).
set_disabled(_) --> [].

set_action(Options) -->
	{ option(on_rating(OnRating), Options) }, !,
	html(\[ '     phpPath:"',OnRating,'",\n'
	      ]).
set_action(_) --> [].

set_field(Options) -->
	{ option(set_field(Field), Options) }, !,
	html(\[ '     onSuccess: function(e,r)\n',
		'     { $(\'input[name=~w]\').val(r);\n'-[Field],
		'     }\n'
	      ]).
set_field(_) --> [].
