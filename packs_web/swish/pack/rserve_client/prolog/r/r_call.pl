/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2016-2019, VU University Amsterdam
			      CWI, Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(r_call,
	  [ (<-)/2,			% ?Var, +Expression
	    (<-)/1,			% +Expression
	    r_call/2,			% +Function, +Options

					% Internal predicates
	    r/4,			% Quasi quotation parser
	    r_execute/3,		% +Assignments, +Command, -Result
	    r_setup_graphics/2,		% +Rconn, +Format

	    op(900,  fx, <-),
	    op(900, xfx, <-),
	    op(400, yfx, $),
	    op(100, yf,  [])
	  ]).
:- use_module(r_serve).
:- use_module(r_grammar).
:- use_module(r_term).
:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(lists)).
:- use_module(library(debug)).
:- use_module(library(quasi_quotations)).
:- use_module(library(dcg/basics)).
:- use_module(library(settings)).
:- use_module(library(option)).

:- multifile
	r_init_session/1,		% +Session
	r_console/2,			% +Type, ?Term
	r_console_property/1,		% ?Property
	r_display_images/1.		% +Images

/** <module> R plugin for SWISH

This    module    make    R    available     to    SWISH    using    the
[Rserve](https://rforge.net/Rserve/) R package. The   module  r_serve.pl
implements a SWI-Prolog wrapper around the  Rserve C++ client to realise
the communication with the R server.

The      Prolog      view      at      R        is      inspired      by
[real](http://stoics.org.uk/~nicos/sware/real/) from Nicos Angelopoulos.

It consists of the following two predicates:

  - Var <- Expression
  Assign the result of evaluating the given R Expression to Var.  Var
  can be a Prolog variable or an R expression.
  - <- Expression
  Evaluate expression, discarding the result.  Possible console output
  is captured.

In addition, the _quasi quotation_ `r`   is defined. The quasi quotation
takes Prolog variables as arguments  and   an  R  expression as content.
Arguments (Prolog variable names) that  match   R  identifiers cause the
temporary of an R variable with that name bound to the translated Prolog
value. R quasi quotations can be used as   isolated goals, as well as as
right-hand arguments to <-/2 and <-/1.  The   example  below calls the R
plot() function on the given Prolog list.

  ```
  ?- numlist(1,10,Data),
     {|r(Data)||plot(Data)|}.
  ```

Images created by the R session are transferred   as SVG and sent to the
SWISH console using pengine_output/1.
*/

:- setting(rserve:socket, atom, '/home/rserve/socket',
	   "Unix domain socket for connecting to Rserve").
:- setting(rserve:host,	atom, '127.0.0.1',
	   "Host for connecting to Rserve").
:- setting(rserve:port,	integer, 6311,
	   "Port for connecting to Rserve").

%%	(Var <- Expression) is det.
%
%	Assign the result of evaluating the   given R Expression to Var.
%	Var can be a Prolog variable or an R expression.

Var <- Expression :-
	var(Var), !,
	(   var(Expression)
	->  instantiation_error(Expression)
	;   Expression = r_execute(Assignments, Command, Var)
	->  r_execute(Assignments, Command, Var)
	;   phrase(r_expression(Expression, Assignments), Command)
	->  r_execute(Assignments, Command, Var)
	;   domain_error(r_expression, Expression)
	).
Var <- Expression :-
	(   atom(Var),
	    r_primitive_data(Expression)
	->  r_assign($, Var, Expression)
	;   <-(Var<-Expression)
	).

r_primitive_data(Data) :-
	is_list(Data), !.
r_primitive_data(Data) :-
	compound(Data), !, fail.

%%	(<- Expression) is det.
%
%	Evaluate Expression, discarding  the   result.  Possible console
%	output is captured using the R function `capture.output`.

<- Term :-
	(   var(Term)
	->  instantiation_error(Term)
	;   Term = r_execute(Assignments, Command, _Var)
	->  r_capture_output(Assignments, Command)
	;   phrase(r_expression(Term, Assignments), Command)
	->  r_capture_output(Assignments, Command)
	;   domain_error(r_expression, Term)
	).

%%	r_capture(Assignments, Command)
%
%	Execute Command, presenting the R console output to the (Prolog)
%	user.

r_capture_output(Assignments, Command) :-
	to_string(Command, CommandS),
	r_assign($, 'Rserve.cmd', CommandS),
	r_execute(Assignments,
		  "capture.output(eval(parse(text=Rserve.cmd)))",
		  Output),
	emit_r_output(Output).

to_string(Command, CommandS) :-
	string(Command), !,
	CommandS = Command.
to_string(Command, CommandS) :-
	string_codes(CommandS, Command).

emit_r_output(Output) :-
	r_console(stdout, Output), !.
emit_r_output(Output) :-
	maplist(writeln, Output).

%!	r_call(+Fun, +Options)
%
%	Construct and possibly call an R function. Fun can be an atom or
%	a compound, eg  plot,  or   plot(width=3).  The  predicate  also
%	supports multiple output destinations.  Options processed:
%
%	  - call(Bool)
%	    If `false` (default `true`), do __not__ call the result.
%	  - fcall(-Term)
%	    Term is unified with the constructed call
%	  - rvar(Var)
%	    Variable for the output
%
%	@compat This is a partial implementation of the corresponding
%	[real](http://www.swi-prolog.org/pack/file_details/real/prolog/real.pl) predicate.

r_call(Func, Options) :-
	partition(eq_pair, Options, XArgs, Options1),
	extend(Func, XArgs, Call),
	option(fcall(Call), Options1, _),
	(   option(call(true), Options1, true)
	->  (   option(rvar(Var), Options1)
	    ->  Var <- Call
	    ;   <- Call
	    )
	;   true
	).

eq_pair(_=_).

extend(Compound, XArgs, Term) :-
	compound(Compound), !,
	compound_name_arguments(Compound, Func, Args0),
	append(Args0, XArgs, Args),
	compound_name_arguments(Term, Func, Args).
extend(Atom, XArgs, Term) :-
	compound_name_arguments(Term, Atom, XArgs).


%%	r_console(+Stream, ?Term)
%
%	Hook console interaction. Currently only used   for <-/1 to emit
%	the captured output. In this cases,  Stream is `stdout` and Term
%	is a list of strings, each representing   a  line of output. The
%	list can be empty. If the  hook fails, maplist(writeln, Term) is
%	called to write the output to `current_output`.

%%	r_execute(+Assignments, +Command, -Result) is det.
%
%	Execute the R command Command  after   binding  the variables in
%	Assignments and unify the result with Result.
%
%	@arg Assignments is a list of Name=Value, where Name must be a
%	valid R indentifier.
%	@arg Command is a string holding the R command to execute

r_execute(Assignments, Command, Result) :-
	r_setup_console($),
	setup_call_cleanup(
	    maplist(r_bind, Assignments),
	    r_eval_ex($, Command, Result),
	    r_unbind(Assignments)),
	r_send_images.

r_bind(RVar=Value) :-
	r_assign($, RVar, Value).

%%	r_unbind(+Bindings)
%
%	Remove the created bindings from the R environment

r_unbind([]) :- !.
r_unbind(Bindings) :-
	maplist(arg(1), Bindings, Vars),
	phrase(r_remove(Vars), Command),
	r_eval($, Command, _).

r_remove(Vars) -->
	"remove(", r_vars(Vars), ")".

r_vars([H|T]) -->
	atom(H),
	(   {T==[]}
	->  ""
	;   ",",
	    r_vars(T)
	).

%!	r_setup_console(+R)
%
%	Set the notion of R's console with   to  the width of the Prolog
%	console.       This       may        be         hooked        by
%	r_console_property(size(Rows,Cols) to deal with e.g., SWISH.

r_setup_console(R) :-
	(   r_console_property(size(_Rows, Cols))
	->  true
	;   tty_size(_Rows, Cols)
	), !,
	format(string(Command), 'options(width=~d)', Cols),
	r_eval(R, Command, _).
r_setup_console(_).


		 /*******************************
		 *	  QUASI QUOTATION	*
		 *******************************/

:- quasi_quotation_syntax(r).

%%	r(+Content, +Vars, +VarDict, -Goal) is det.
%
%	Parse {|r(Arg,...||R-code|} into a the   expression  below. This
%	expression may be passed to  <-/2  and   <-/1  as  well  as used
%	directly as a goal, calling r_execute/3.
%
%	    r_execute(Assignments, Command, Result)
%
%	@see https://cran.r-project.org/doc/manuals/r-release/R-lang.html#Parser
%	@tbd Verify more of the R syntax.

r(Content, Vars, Dict, r_execute(Assignments, Command, _Result)) :-
	include(qq_var(Vars), Dict, QQDict),
	phrase_from_quasi_quotation(
	    r(QQDict, Assignments, Parts),
	    Content),
	atomics_to_string(Parts, Command).

qq_var(Vars, _=Var) :-
	member(V, Vars),
	V == Var, !.

r(Dict, Assignments, [Pre|More]) -->
	here(Here0),
	r_tokens(_),
	r_token(identifier(Name)),
	here(Here1),
	{ memberchk(Name=Var, Dict), !,
	  Assignments = [Name=Var|AT],
	  diff_to_atom(Here0, Here1, Pre)
	},
	r(Dict, AT, More).
r(_, [], [Last]) -->
	string(Codes),
	\+ [_], !,
	{ atom_codes(Last, Codes) }.


%%	diff_to_atom(+Start, +End, -Atom)
%
%	True when Atom is an atom that represents the characters between
%	Start and End, where End must be in the tail of the list Start.

diff_to_atom(Start, End, Atom) :-
	diff_list(Start, End, List),
	atom_codes(Atom, List).

diff_list(Start, End, List) :-
	Start == End, !,
	List = [].
diff_list([H|Start], End, [H|List]) :-
	diff_list(Start, End, List).

here(Here, Here, Here).


		 /*******************************
		 *	       IMAGES		*
		 *******************************/

:- multifile rserve:r_open_hook/2.

%%	rserve:r_open_hook(+Name, -R)
%
%	Called for lazy creation to the   Rserve server. Connections are
%	per-thread. The destination depends on settings:
%
%	  $ Unix domain socket :
%	  If `rserve:socket` is defined and not empty, it is taken
%	  as the path to a Unix domain socket to connect to.
%	  $ TCP/IP socket :
%	  Else, if `rserve:port` and `rserve:host` is defined, we
%	  connect to the indicated host and port.
%
%	After  the  connection  is  established,   the  session  can  be
%	configured using the hook r_init_session/1.   The  default calls
%	r_setup_graphics/2 to setup graphics output to send SVG files.

rserve:r_open_hook($, R) :-
	nb_current('R', R), !.
rserve:r_open_hook($, R) :-
	setting(rserve:socket, Socket),
	Socket \== '',
	access_file(Socket, exist), !,
	debug(r(connect), 'Connecting to ~p ...', [Socket]),
	r_open(R,
	       [ host(Socket),
		 port(-1)
	       ]),
	r_setup(R).
rserve:r_open_hook($, R) :-
	setting(rserve:port, Port),
	setting(rserve:host, Host),
	debug(r(connect), 'Connecting to ~p ...', [Host:Port]),
	r_open(R,
	       [ host(Host),
		 port(Port)
	       ]),
	r_setup(R).

r_setup(R) :-
	thread_at_exit(r_close(R)),
	debug(r, 'Created ~p', [R]),
	call_init_session(R),
	nb_setval('R', R), !.

call_init_session(R) :-
	r_init_session(R), !.
call_init_session(R) :-
	r_setup_graphics(R, svg).

%%	r_init_session(+RConn) is semidet.
%
%	Multifile hook that is called after the Rserve server has handed
%	us a new connection. If this   hook fails, r_setup_graphics/2 is
%	called to setup capturing graphics as SVG files.

%%	r_setup_graphics(+Rconn, +Format) is det.
%
%	Setup graphics output  using  files.   Currently  only  supports
%	`Format = svg`.

r_setup_graphics(R, svg) :-
	r_eval(R, "mysvg <- function() {
                     svg(\"Rplot%03d.svg\")
		     par(mar=c(4,4,1,1))
                   }
	           options(device=mysvg)", X),
	debug(r, 'Devices: ~p', [X]),
	nb_setval('Rimage_base', 'Rplot'),
	nb_setval('Rimage_ext', 'svg').

%%	r_send_images is det.
%
%	Collect the images saved on the server and send them to SWISH
%	using pengine_output/1.

r_send_images :-
	r_images(Images), !,
	length(Images, Count),
	debug(r, 'Got ~d images~n', [Count]),
	r_send_images(Images).
r_send_images.

r_send_images(Images) :-
	r_display_images(Images), !.
r_send_images(Images) :-
	print_message(warning, r_images(Images)).

%%	r_display_images(+Images:list)
%
%	Hook to display images.
%
%	@arg Images is a list of images.  Each image is of the form
%	Format(String), where Format is the file extension.  Currently
%	only uses `svg`.  If not defined, print_message/2 is called
%	with the term r_images(Images).

%%	r_images(-Images:list) is semidet.
%
%	Collect saved image files from Rserve.  This assumes that
%
%	  1. The R connection is in the global variable =R=.  If
%	  there is no connection, there are no images.
%	  2. There are only images if there is a current device.
%	  This is closed using =|dev.off()|=.
%	  3. Images are called <base>%03d.<ext>, where <base> is
%	  in the global variable =Rimage_base= and <ext> in
%	  =Rimage_ext=.

r_images(List) :-
	nb_current('R', _),
	(   r_eval($, "names(dev.list())", Devices),
	    Devices = ["svg"|_]
	->  r_eval($, "dev.off()", _),
	    r_fetch_images(1, List)
	).

r_fetch_images(I, Images) :-
	nb_getval('Rimage_base', Base),
	nb_getval('Rimage_ext', Ext),
	format(string(Name), "~w~|~`0t~d~3+.~w", [Base,I,Ext]),
	debug(r, 'Trying ~p~n', [Name]),
	(   catch(r_read_file($, Name, Content), E, r_error_fail(E))
	->  debug(r, 'Got ~p~n', [Name]),
	    Image =.. [Ext,Content],
	    Images = [Image|Rest],
	    (   debugging(r(plot))
	    ->  save_plot(Name, Content)
	    ;	true
	    ),
	    I2 is I+1,
	    r_fetch_images(I2, Rest)
	;   Images = []
	).

r_error_fail(error(r_error(70),_)) :- !, fail.
r_error_fail(Error) :- print_message(warning, Error), fail.

save_plot(File, Data) :-
	setup_call_cleanup(
	    open(File, write, Out, [type(binary)]),
	    format(Out, '~s', [Data]),
	    close(Out)).


		 /*******************************
		 *	      MESSAGES		*
		 *******************************/

:- multifile
	prolog:message//1.

prolog:message(r_images(Images)) -->
	{ length(Images, Count) },
	[ 'Rserve sent ~d images files.'-[Count], nl ],
	[ 'Define r_call:r_display_images/1 to display them.'-[] ].
