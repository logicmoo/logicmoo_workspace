:- module(upload, [run/0]).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_header)).
:- use_module(library(http/http_multipart_plugin)).
:- use_module(library(http/http_client)).
:- use_module(library(http/html_write)).
:- use_module(library(option)).

:- http_handler(root(.),	upload_form, []).
:- http_handler(root(upload),	upload,      []).

run :-
	http_server(http_dispatch, [port(8080)]).

upload_form(_Request) :-
	reply_html_page(
	    title('Upload a file'),
	    [ h1('Upload a file'),
	      form([ method('POST'),
		     action(location_by_id(upload)),
		     enctype('multipart/form-data')
		   ],
		   table([],
			 [ tr([td(input([type(file), name(file)]))]),
			   tr([td(align(right),
				  input([type(submit), value('Upload!')]))])
			 ]))
	    ]).

upload(Request) :-
	multipart_post_request(Request), !,
	http_read_data(Request, Parts,
		       [ on_filename(save_file)
		       ]),
	memberchk(file=file(FileName, Saved), Parts),
	format('Content-type: text/plain~n~n'),
	format('Saved your file "~w" into "~w"~n', [FileName, Saved]).
upload(_Request) :-
	throw(http_reply(bad_request(bad_file_upload))).

multipart_post_request(Request) :-
	memberchk(method(post), Request),
	memberchk(content_type(ContentType), Request),
	http_parse_header_value(
	    content_type, ContentType,
	    media(multipart/'form-data', _)).

:- public save_file/3.

save_file(In, file(FileName, File), Options) :-
	option(filename(FileName), Options),
	setup_call_cleanup(
	    tmp_file_stream(octet, File, Out),
	    copy_stream_data(In, Out),
	    close(Out)).

:- multifile prolog:message//1.

prolog:message(bad_file_upload) -->
	[ 'A file upload must be submitted as multipart/form-data using', nl,
	  'name=file and providing a file-name'
	].
