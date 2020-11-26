#!/usr/local/bin/swipl 

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

:- module(logicmoo_cliop,
          [ start_cliop/0
          ]).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_path)).
:- use_module(library(www_browser)).
:- if(exists_source(library(uid))).
:- use_module(library(uid)).
:- endif.

/** <module>

  Isolated startup module for ClioPatria

*/

:- use_module(library(must_trace)).

:- multifile(owl2_model:datatype/1).
:- dynamic(owl2_model:datatype/1).

:- if(false).



:- multifile(owl2_model:datatype/2).
:- dynamic(owl2_model:datatype/2).



:- meta_predicate(from_http(0)).
from_http(G):- stream_property(Main_error,file_no(2)), with_output_to(Main_error, G).

:- ignore((stream_property(X,file_no(2)),
   stream_property(X,alias(Was)),
   set_stream(X,alias(main_error)),
   set_stream(X,alias(Was)))).

:- meta_predicate(from_http(0)).


reexport_from(ReExporter,From:P):-
    From:export(From:P),
    ReExporter:import(From:P),
    ReExporter:export(From:P).



:- if(exists_source(library(trill))).

:- system:use_module(library(trill)).

:- reexport_from(system,trill:end_bdd/1),
   reexport_from(system,trill:add_var/5),
   reexport_from(system,trill:and/4),
   reexport_from(system,trill:em/8),
   reexport_from(system,trill:randomize/1),
   reexport_from(system,trill:rand_seed/1),
   reexport_from(system,trill:or/4),
   reexport_from(system,trill:ret_prob/3),
   reexport_from(system,trill:init_test/2),
   reexport_from(system,trill:end/1),
   reexport_from(system,trill:end_test/1),
   reexport_from(system,trill:bdd_not/3),
   reexport_from(system,trill:zero/2),
   reexport_from(system,trill:one/2),
   reexport_from(system,trill:equality/4),
   reexport_from(system,trill:init_bdd/2),
   reexport_from(system,trill:init/3).

:- dynamic(user:db/1).
:- thread_local(user:db/1).

:- use_module(library(pita),[]).


% :- [pack(trill_on_swish/src/lib/authenticate)].
% :- use_module(library(r/r_sandbox)).



:- use_module(library(aleph),[]).
:- add_search_path_safe(swish, '/home/prologmud_server/lib/swipl/pack/swish').
:- use_module(library(cplint_r),[]).
:- use_module(library(mcintyre)).
:- use_module(library(slipcover)).
:- use_module(library(lemur),[]).
:- use_module(library(auc)).
:- use_module(library(matrix)).

:- use_module(library(clpr)).

:- endif.

:- multifile sandbox:safe_primitive/1.


sandbox:safe_primitive(nf_r:{_}).
:- dynamic http:location/3.
:- multifile http:location/3.
http:location(root, '/', [priority(1100)]).
http:location(swish, root('swish'), [priority(500)]).
% http:location(root, '/swish', []).


:- endif.

:- dmsg("CLIOP Load").

start_cliop :- 
   dmsg("CLIOP Start"),
   cp_server.


%:- initialization(start_cliop).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This file provides a skeleton startup file.  It can be localized by running

    % ./configure                       (Unix)
    % Double-clicking win-config.exe    (Windows)

After  that,  the  system  may  be  customized  by  copying  or  linking
customization  files  from  config-available    to  config-enabled.  See
config-enabled/README.txt for details.

To run the system, do one of the following:

    * Running for development
      Run ./run.pl (Unix) or open run.pl by double clicking it (Windows)

    * Running as Unix daemon (service)
      See daemon.pl
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

% Setup search path for cliopatria. We add  both a relative and absolute
% path. The absolute path allow us to  start in any directory, while the
% relative one ensures that the system remains working when installed on
% a device that may be mounted on a different location.

add_relative_search_path(Alias, Rel):- 
    absolute_file_name(Rel, Real,[file_type(directory),access(read),file_errors(fail)]),
    add_relative_search_path0(Alias, Real),!.

add_relative_search_path0(Alias, Abs) :-
        is_absolute_file_name(Abs), 
        exists_directory(Abs),
        assertz(user:file_search_path(Alias, Abs)),fail.
add_relative_search_path0(Alias, Abs) :-
        is_absolute_file_name(Abs), 
        prolog_load_context(file, Here),
        relative_file_name(Abs, Here, Rel),
        assertz(user:file_search_path(Alias, Rel)),fail.
add_relative_search_path0(Alias, Rel) :-
        assertz(user:file_search_path(Alias, Rel)).


% Make loading files silent. Comment if you want verbose loading.

:- current_prolog_flag(verbose, Verbose),
   asserta(saved_verbose(Verbose)),
   set_prolog_flag(verbose, silent).

:- add_relative_search_path(cliopatria, pack('ClioPatria')).
% :- add_relative_search_path(cpacks, 'cpack').


% :- use_module(library(cplint_r)).


                 /*******************************
                 *            LOAD CODE         *
                 *******************************/

% Use the ClioPatria help system.  May   be  commented to disable online
% help on the source-code.



:- use_module(cliopatria('applications/help/load')).


% Load ClioPatria itself.  Better keep this line.

:- use_module(cliopatria(cliopatria)).

:- listing(user:file_search_path/2).

% Get back normal verbosity of the toplevel.

:- (   retract(saved_verbose(Verbose))
   ->  set_prolog_flag(verbose, Verbose)
   ;   true
   ).



:- if(exists_source(rdfql(sparql_csv_result))).
:- use_module(rdfql(sparql_csv_result)).
:- endif.

cliop_restore :- app_argv('--nocliop'),!.
cliop_restore :- must(start_cliop).
                     
:- initialization(cliop_restore).
:- initialization(cliop_restore,now).
:- initialization(cliop_restore,restore).


/*
:- ['./cpack/trill_on_swish/lib/trill_on_swish/storage_trill_on_swish'].
:- ['./cpack/trill_on_swish/lib/trill_on_swish/gitty_trill_on_swish'].
*/


