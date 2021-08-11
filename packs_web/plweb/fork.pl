:- doc_collect(true).
:- load_files([ library(pldoc/doc_library),
		library(debug),
		library(settings),
		library(option),
		http_fork,
		plweb,
		wiki_edit
	      ],
	      [ silent(true)
	      ]).

:- doc_load_library.

:- debug(http(_)).

run :-
	run([]).

run(Options) :-
	load_settings('plweb.conf'),
	setting(http:port, Port),
	setting(http:workers, Workers),
	merge_options(Options,
		      [ port(Port),
			workers(Workers)
		      ], HTTPOptions),
	option(port(Port), HTTPOptions),
	thread_create(forked_server(Port,
				    [ HTTPOptions
				    ]),
		      _,
		      [ alias(http_server_monitor),
			detached(true)
		      ]).

