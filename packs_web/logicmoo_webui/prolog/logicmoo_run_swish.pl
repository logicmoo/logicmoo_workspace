end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.

:- module(swish_with_localedit,
	  [
	  ]).

% % :- ensure_loaded(logicmoo_base).


:- volatile(swish_trace:installed/1).

:- kb_shared(mpred_online:semweb_startup/0).
% :- '@'(ensure_loaded(logicmoo(util/logicmoo_util_bugger)),user).
:- add_to_search_path(swish, '../pack/swish/').

:- dynamic user:file_search_path/2.
:- multifile user:file_search_path/2.


		 /*******************************
		 *	       PATHS		*
		 *******************************/



logicmoo_set_swish_path :-
	absolute_file_name(swish('swish.pl'), _,
			   [file_errors(fail), access(read)]), !.

logicmoo_set_swish_path :-
	filematch(pack(swish),Dir),
	asserta(user:file_search_path(swish, Dir)),!.




                 /*******************************
                 *   CREATE SWISH APPLICATION   *
                 *******************************/

:- kb_shared
	pengines:prepare_module/3.

:- logicmoo_set_swish_path.

% % % OFF :- system:use_module(swish(swish)).

% load rendering modules
:- swish:ensure_loaded(logicmoo(swish_lib/render/html),	[]).


:- retractall((http:location(swish, _, _))).
:- asserta(http:location(swish, root(swish), [priority(-100)])).

%%	swish_config:config(?Config, ?Value) is nondet.
%
%	All solutions of this predicate are  available in the JavaScript
%	object config.swish.config. Config must be an  atom that is also
%	a valid JavaScript identifier. Value  must   be  a value that is
%	valid for json_write_dict/2. Most configurations  are also saved
%	in the application preferences. These   are  marked [P]. Defined
%	config parameters:
%
%	  - show_beware
%	  [P] If `true`, show the *Beware* modal dialog on startup
%	  - tabled_results
%	  [P] If `true`, check the _table results_ checkbox by default.
%	  - application
%	  Name of the Pengine application.
%	  - csv_formats
%	  [P] CSV output formats offered. For example, ClioPatria
%	  defines this as [rdf,prolog]. The first element is default.
%	  - community_examples
%	  Allow marking saved programs as example.  If marked, the
%	  programs are added to the Examples menu.
%	  - public_access
%	  If lib/authenticate.pl is loaded and this flag is `true`,
%	  _all_ access to SWISH demands authentication.  If false,
%	  only running queries and saving files is restricted. Note
%	  that this flag has no effect if no authentication module is
%	  loaded.

assert_sv((A,B)):-!,assert_sv(A),assert_sv(B).
assert_sv(M:P):-!,functor(P,_,A),duplicate_term(P,R),setarg(A,R,_),ignore(retract(M:R)),asserta(M:P).
assert_sv(P):-!,functor(P,_,A),duplicate_term(P,R),setarg(A,R,_),ignore(retract(R)),asserta(P).

		 /*******************************
		 *	      CONFIG		*
		 *******************************/

:- abolish(swish_config:config,2).
:- was_dynamic(swish_config:config/2).
:- kb_shared
	swish_config:config/2,
	swish_config:source_alias/2.


:- assert_sv((
   swish_config:config(show_beware,        false),
   swish_config:config(tabled_results,     false),
   swish_config:config(application,        swish),
   swish_config:config(csv_formats,        [prolog]),
   swish_config:config(community_examples, true),
   swish_config:config(public_access,      true))).



%%	swish_config:source_alias(Alias, Options) is nondet.
%
%	Specify access for files below a given _alias_. Options define
%
%	  - access(Access)
%	  One of `read` or `both`.  Default is `read`.
%	  - if(Condition)
%	  Provide additional conditions.  Defined conditions are:
%	    - loaded
%	    Only provide access to the file if it is loaded.



% :- endif.



/*
:- meta_predicate swish_svgtree:'__aux_maplist/3_filtered_tree+2'(*,*,3,+).

% Hack: auto-loading this does not work.
:- [library(charsio)].
:- [charsio:library(memfile)].
:- debug(pengine(delay)).
% % % OFF :- system:use_module(library(plunit)).
% % % OFF :- system:use_module(library(pengines)).
% % % OFF :- system:use_module(pengine_sandbox:library(pengines)).
% % % OFF :- system:use_module(library(process)).
% % % OFF :- system:use_module(library(http/thread_httpd)).
% % % OFF :- system:use_module(library(http/http_files)).
% % % OFF :- system:use_module(library(http/http_dispatch)).
:- pengine_application(swish).
% % % OFF :- system:use_module(swish:library(pengines_io)).
pengines:prepare_module(Module, swish, _Options) :- pengines_io:pengine_bind_io_to_html(Module).
:- debug(http(request)).
*/

:- if(if_defined(must_install_bower)).


% % % OFF :- system:use_module(library(process)).
install_bower:- prolog_file_dir(('.'),LPWD),
   process_create(sudo,[bower,install,'--allow-root'],[cwd(LPWD),process(PID)]),
   process_wait(PID,_Status).

:- shell('sudo apt-get install npm nodejs-legacy').
:- shell('sudo npm install -g bower').

:- install_bower.
% :-shell('bower install').

/*
#### Download as zip

As installing node and bower is not a pleasure on all operating systems,
you can also download  the  dependencies  as   a  single  zip  file from
Unpack the zip file, maintaining the directory structure, from the swish
root directory to create the directory web/bower_components.
*/
:- shell('wget http://www.swi-prolog.org/download/swish/swish-bower-components.zip').
:- shell('unzip -o swish-bower-components.zip -d . ').
/*

### Get the latest SWI-Prolog

Install the latest  [SWI-Prolog](http://www.swi-prolog.org) _development
version_. As SWISH is very  much  in   flux  and  depends  on the recent
SWI-Prolog pengines and sandboxing libraries, it   is  quite common that
you            need            the             [nightly            build
(Windows)](http://www.swi-prolog.org/download/daily/bin/) or build   the
system    from    the     current      git     development    repository
[swipl-devel.git](https://github.com/SWI-Prolog/swipl-devel).

Nov 6, 2014: release 7.1.26 fully supports the current SWISH.


## Running SWISH

With a sufficiently recent Prolog installed, start the system by opening
`run.pl` either by running `swipl  run.pl`   (Unix)  or opening `run.pl`
from the Windows explorer.

Now direct your browser to http://localhost:3050/

If you want  to  know  what  the   latest  version  looks  like,  go  to
http://swish.swi-prolog.org/


### Running SWISH without sandbox limitations

By default, SWISH lets you only run _safe_  commands. If you want to use
SWISH for unrestricted development, load the authentication module:
*/

 ?- [lib/authenticate].

/*
Next, for first usage, you need  to   create  a user. The authentication
module defines swish_add_user/3, which updates or  creates a file called
`passwd`:
*/

  ?- swish_add_user(guru, 'top secret', []).

/*
If you now try to run a command in  SWISH, it will prompt for a user and
password. After authentication you can run any Prolog predicate.

**NOTE** Authentication uses plain HTTP   basic authentication. Only use
this on trusted networks and do not  use   a  password  that you use for
other sensitive services. If you want to  setup a public server this way
you are strongly adviced to use HTTPS.


## Design

Most of the application is realised  using client-side JavaScript, which
can be found  in  the  directory   `web/js`.  The  JavaScript  files use
[RequireJS](http://requirejs.org/)   for   dependency     tracking   and
[jQuery](http://jquery.com/) for structuring the   JavaScript  as jQuery
plugins. The accompanying CSS is in   `web/css`.  More details about the
organization of the JavaScript is in `web/js/README.md`

There are two overal pages. `web/swish.html`  provides a static page and
`lib/page.pl` provides a Prolog frontend to  generate the overal page or
parts thereof dynamically. The latter   facilitates smoothless embedding
in SWI-Prolog web applications.


## Development and debugging

No building is needed  to  run  the   system  from  sources.  For public
installations you probably want to create   the  minified JavaScript and
CSS files to reduce network traffic and startup time. You need some more
tools for that:
*/
:- shell('npm install -g jsdoc').
:- shell('npm install -g requirejs').
:- shell('npm install -g clean-css').

/*
You also need GNU make installed as   `make`  and SWI-Prolog as `swipl`.
With all that in  place,  the   following  command  creates the minified
versions:
*/

:- shell(make).

/*
The default main page (`/`)  is   generated  from `lib/page.pl`. It uses
minified    JavaScript    and    CSS      from     `web/js/swish-min.js`
`web/css/swish-min.css` when available. If the   minified  files are not
present,  the  server  automatically  includes   the  full  source.  The
generated files may be removed using

    make clean

Alternatively, use of the minified  files   can  be  disable from Prolog
using this command and reloading the page:
*/
%     ?- debug(nominified).


