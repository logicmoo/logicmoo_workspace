# How do I upload a file, preserving the file-name?

*NOTE* This example requires SWI-Prolog 7.2 or later.

The way to preserve the user-entered file-name is to use a form with
=enctype= =|multipart/form-data|= and =input= element of type =file=.
This causes a browser to formulate a MIME encoded POST request that
contains the filename.

First, let us import the required HTTP libraries and create the server.
Note that we need library(http/http_multipart_plugin) to make this demo
work.  This library acts as a plugin for library(http/http_client) for
parsing `multipart/form-data` documents.

  ==
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
  ==

Next step, we create the form by defining the implementation for
upload_form/1:

  ==
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
  ==

Next step, we must define upload/1. Unfortunately, we cannot use the
simple http_parameters/2 to obtain the data because this interface only
provides the name and value of parameters. Instead, we must use
http_read_data/3. First we validate the request to be a POST holding the
proper content using multipart_post_request/1. If this fails, we throw
the error http_reply(bad_request(Culprit)), where the Culprit is a
Prolog term describing the problem. This is matched to a clean
message in the next section.

Next, we define the hook save_file/3 which is called to process parts
that provide a `filename` attribute.  The first argument is a stream
containing the data.  The stream is binary, but may be switched using
the encoding(Enc) option of set_stream/2.  Its task is to process the
data in the stream and return a term that represents this _part_ of
the multipart message.  In the example we save the data in a (temporary)
file and return a term file(UserFileName, SavedFileName).

After the http_read_data/3 call, we select the part that holds the file
and print a document holding the details.

  ==
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
  ==

Finally, we must translate =bad_file_upload= into a clean message.  We
do this using the DCG rule prolog:message//1:

==
:- multifile prolog:message//1.

prolog:message(bad_file_upload) -->
	[ 'A file upload must be submitted as multipart/form-data using', nl,
	  'name=file and providing a file-name'
	].
==

@see  You can download the complete code from demo_upload_file.pl.
