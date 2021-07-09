%#!/usr/bin/swipl 

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

:- module(logicmoo_pldoc,[]).

:- multifile user:file_search_path/2.
:- dynamic   user:file_search_path/2.

:- include(library(pldoc/hooks)).

:- if(exists_source(library(pldoc))).
% Must be loaded before doc_process
:- user:use_module(library(pldoc), []).
:- user:use_module(library(pldoc/doc_process)).
:- endif.

/*
:- user:use_module(library(pldoc/doc_access)).
:- user:use_module(library(pldoc/doc_pack)).

:- user:use_module(library(doc_http)).
:- reexport(library(pldoc/doc_html)).
:- user:use_module(library(pldoc/doc_wiki)).
:- user:use_module(library(pldoc/doc_search)).
:- user:use_module(library(pldoc/doc_util)).
:- user:use_module(library(pldoc/doc_library)).
%:- doc_load_library.

*/

end_of_file.




/*

:- user:use_module(library(http/thread_httpd)).
:- user:use_module(library(http/http_error)).
:- user:use_module(library(http/http_client)).

% http_reply_from_files is here
:- user:use_module(library(http/http_files)).
% http_404 is in here
%:- user:use_module(library(http/http_dispatch)).

%:- user:use_module(library(http/http_dispatch)).
%:- user:use_module(library(http/html_write),except([op(_,_,_)])).
%:- user:use_module(library(http/html_head)).
:- user:use_module(library(http/http_parameters)).
:- user:use_module(library(http/http_server_files)).
:- user:use_module(library(http/http_wrapper)).


:- multifile(http_session:urandom_handle/1).
:- dynamic(http_session:urandom_handle/1).
:- volatile(http_session:urandom_handle/1).
:- user:use_module(library(http/http_session)).
*/

:- multifile(http_session:urandom_handle/1).
:- dynamic(http_session:urandom_handle/1).
:- volatile(http_session:urandom_handle/1).

:- multifile(http_log:log_stream/2).
:- dynamic(http_log:log_stream/2).
:- volatile(http_log:log_stream/2).

