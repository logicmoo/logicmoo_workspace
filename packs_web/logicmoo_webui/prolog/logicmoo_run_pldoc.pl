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

:- if(( ( \+ ((current_prolog_flag(logicmoo_include,Call),Call))) )). 
:- endif.
:- module(logicmoo_run_pldoc,[]).

% % % OFF :- system:use_module(library(settings)).

:- kb_shared http:location/3.
:- was_dynamic http:location/3.

% % % OFF :- system:use_module(library(memfile)).
%:- ensure_loaded(logicmoo_base).% WAS OFF  :- system:use_module(server).

/*
swish_highlight:insert_memory_file(X,Y,Z):-dmsg(error(swish_highlight:insert_memory_file(X,Y,Z))).
swish_highlight:delete_memory_file(X,Y,Z):-dmsg(error(swish_highlight:delete_memory_file(X,Y,Z))).
swish_highlight:memory_file_line_position(X,Y,Z,A):-dmsg(error(swish_highlight:memory_file_line_position(X,Y,Z,A))).
swish_highlight:memory_file_substring(X,Y,Z,A,B):-dmsg(error(swish_highlight:memory_file_substring(X,Y,Z,A,B))).
swish_highlight:memory_file_to_string(X,Y):- memory_file_to_codes(X,C),string_codes(Y,C). %  dmsg(error(swish_highlight:memory_file_to_string(X,Y))).
*/

% % % OFF :- system:use_module(library(pldoc)).
% % % OFF :- system:use_module(library(http/thread_httpd)).
% % % OFF :- system:use_module(library(http/http_parameters)).
% % % OFF :- system:use_module(swi(library/http/html_write)).
% % % OFF :- system:use_module(library(http/mimetype)).
% % % OFF :- system:use_module(library(dcg/basics)).
% % % OFF :- system:use_module(library(http/http_dispatch)).
% % % OFF :- system:use_module(library(http/http_hook)).
% % % OFF :- system:use_module(library(http/http_path)).
% % % OFF :- system:use_module(library(http/http_wrapper)).
% % % OFF :- system:use_module(library(uri)).
% % % OFF :- system:use_module(library(debug)).
% % % OFF :- system:use_module(library(lists)).
% % % OFF :- system:use_module(library(url)).
% % % OFF :- system:use_module(library(socket)).
% % % OFF :- system:use_module(library(option)).
% % % OFF :- system:use_module(library(error)).
% % % OFF :- system:use_module(library(www_browser)).

% % % OFF :- system:use_module(pldoc(doc_process)).
% % % OFF :- system:use_module(pldoc(doc_htmlsrc)).
% % % OFF :- system:use_module(pldoc(doc_html)).
% % % OFF :- system:use_module(pldoc(doc_index)).
% % % OFF :- system:use_module(pldoc(doc_search)).
% % % OFF :- system:use_module(pldoc(doc_man)).
% % % OFF :- system:use_module(pldoc(doc_wiki)).
% % % OFF :- system:use_module(pldoc(doc_util)).
% % % OFF :- system:use_module(pldoc(doc_access)).
% % % OFF :- system:use_module(pldoc(doc_pack)).

% % % OFF :- system:use_module(library(doc_http)).
:- abolish(pldoc_http:src_skin,5).



pldoc_http:src_skin(Request, _Show, FormatComments, header, Out) :-
  pldoc_http:((
     member(request_uri(ReqURI), Request),!,
	prolog_xref:negate(FormatComments, AltFormatComments),
	replace_parameters(ReqURI, [show(raw)], RawLink),
        replace_parameters(ReqURI, [], EditLink0),
         logicmoo_util_strings:atom_subst(EditLink0,'help/source/doc','swish/filesystem/',EditLink),
	replace_parameters(ReqURI, [format_comments(AltFormatComments)], CmtLink),
	phrase(html(div(class(src_formats),
			[ 'View source with ',
			  a(href(CmtLink), \alt_view(AltFormatComments)),
                        ' or as ',
                        a(href(RawLink), raw),
                        ' or EDIT ',
                        a(href(EditLink), edit)
			])), Tokens),
	html_write:print_html(Out, Tokens))).

% called through source_to_html/3.
:- public(pldoc_http:src_skin/5).

:- if(if_defined(ultra_verbose)).
:- prolog_listing:listing(pldoc_http:src_skin/5).
:- endif.

edit_file_href(_Options,File0, HREF) :-
 pldoc_index:((  is_absolute_file_name(File0),
	insert_alias(File0, File),
	ensure_slash_start(File, SlashFile),
	http_location([path(SlashFile)], Escaped),
	http_location_by_id(pldoc_doc, DocRoot),
        atom_concat(DocRoot, Escaped, HREFDOC))),
        logicmoo_util_strings:atom_subst(HREFDOC,'help/source/doc','swish/filesystem/',HREF),!.
edit_file_href(_Options,HREF, HREF).

doc_file_href(_Options,File0, HREF) :-
 pldoc_index:(( is_absolute_file_name(File0),
	insert_alias(File0, File),
	ensure_slash_start(File, SlashFile),
	http_location([path(SlashFile)], Escaped),
	http_location_by_id(pldoc_doc, DocRoot),
        atom_concat(DocRoot, Escaped, HREF))).

doc_file_href(_Options,HREF, HREF).


%%	source_button(+File, +Options)// is det.
%
%	Add show-source button.
:- abolish(pldoc_html:source_button,4).
:- public(pldoc_html:source_button//2).
pldoc_html:source_button(_File, Options) -->
	{ pldoc_html:option(files(_Map), Options) }, !.	% generating files
pldoc_html:source_button(File, _Options) -->
	{show_call(why,(doc_file_href(Options, File, HREF0),
         edit_file_href(Options, File, EDIT_HREF0)))},

	html_write:html([
         a(href(HREF0+[show(src)]),
	       img([ class(action),
		     alt('Show source cOdE'),
		     title('Show source CODE'),
		     src(location_by_id(pldoc_resource)+'source.png')
		   ])),
         a(href(EDIT_HREF0+[]),
	       img([ class(action),
		     alt('Edit source'),
		     title('Edit source'),
		     src(location_by_id(pldoc_resource)+'edit.png')
		   ]))]).


doug_debug(O):-format(user_error,'~nDOUG_DEBUG: ~q.~n',[O]),!.


testml([]):-!. testml([M|L]):-!,testml(M),testml(L).
testml(M):-atomic(M),!,format('~w',[M]).
testml(nl(E)):-!,ignore((between(0,E,_),nl,fail)).
testml(ML):-phrase(ML,C,[]),testml(C).

/*
:- asserta((http:location(pldoc, root('pldoc'), []))),
   asserta((http:location(pldoc_resource, root('pldoc'), []) :- pldoc_http:http_location_by_id(pldoc_resource, root('pldoc')))),
   asserta((http:location(pldoc_resource, R, []) :- pldoc_http:http_location_by_id(pldoc_resource, R))).
*/


hup(_Signal) :-
        thread_send_message(main, stop).


:- catch(on_signal(hup, _, hup),error(domain_error(signal, hup), context(system:'$on_signal'/4, _)),dmsg(warn(not_installing_HUP_handler))).

% % % OFF :- system:use_module(library(http/thread_httpd)).
% % % OFF :- system:use_module(library(http/http_dispatch)).
% :- if_startup_script((http_server(http_dispatch, [ port(3050), workers(16) ]), debug(http_request(_)),debug(cm(_)),debug(swish(_)),debug(storage))).


:- was_export(do_semweb_startup/0).
do_semweb_startup:-
   predicate_property(mpred_online:semweb_startup,number_of_clauses(N1)),
   forall(clause(mpred_online:semweb_startup,Body,Ref),must(do_ref_job(Body,Ref))),
   predicate_property(mpred_online:semweb_startup,number_of_clauses(N2)),
   ((N2\=N1) -> do_semweb_startup ; true).

% [Optionaly] register swish server (remote file editing)
% TODO :- with_no_mpred_expansions(if_file_exists(ensure_loaded('../pack/swish/lm_xref_run_swish'))).

% [Optionaly] register/run Cliopatria sparql server (remote RDF browsing)
% TODO mpred_online:semweb_startup:-ensure_loaded('run_clio').

% [Optionaly] register/run KnowRob robot services (we use it for the ontology mainly)
% TODO mpred_online:semweb_startup :- with_no_mpred_expansions(if_file_exists(ensure_loaded('../pack/MUD_KnowRob/knowrob_addons/knowrob_mud/prolog/init.pl'))).

% [Optionaly] register/run MILO robot services (we use it for the ontology mainly)
% TODO mpred_online:semweb_startup :- register_ros_package(milo).

% [Optionaly] register/run EulerSharp robot services (we use it for the ontology mainly)
% TODO mpred_online:semweb_startup :- register_ros_package(euler).

% :- ensure_loaded(logicmoo(dbase/mpred_i_pldoc)).
% :- do_semweb_startup.



% [Optionaly] remove debug noises
% mpred_online:semweb_startup:- nodebug_logicmoo(http(_)).
% mpred_online:semweb_startup:- nodebug_logicmoo(_).

:- kb_shared(pre_file_search_path/2).

% user:pre_file_search_path(_,_):-!,fail.

:- kb_shared
	sandbox:safe_primitive/1,		% Goal
	sandbox:safe_meta_predicate/1,		% Name/Arity
	sandbox:safe_meta/2,			% Goal, Calls
	sandbox:safe_global_variable/1,		% Name
	sandbox:safe_directive/1.		% Module:Goal

:- kb_shared(prolog:sandbox_allowed_clause/1).

prolog:sandbox_allowed_clause(Clause):-nonvar(Clause).


/*
normal_verify_predefined_safe_declarations :-
        \+ ( clause(safe_primitive(A), _, C),
             \+ ( catch(verify_safe_declaration(A), B, true),
                  (   nonvar(B)
                  ->  clause_property(C, file(D)),
                      clause_property(C, line_count(E)),
                      print_message(error,
                                    bad_safe_declaration(A,
                                                         D,
                                                         E))
                  ;   true
                  )
                )
           ).
*/


:- abolish(sandbox:safe_primitive,1).

% must sneak around pengines security! (we make it dynamic .. but if it loads before we do we have to kill it)
:- abolish(sandbox:verify_predefined_safe_declarations,0).
:- kb_shared(sandbox:verify_predefined_safe_declarations).
:- asserta(sandbox:verify_predefined_safe_declarations).
:- asserta((sandbox:safe_primitive(X):-nonvar(X))),!.
:- asserta((sandbox:safe_primitive(P):-var(P),!,current_predicate(F/A),functor(P,F,A))).
:- asserta((sandbox:safe_primitive(M:P):-var(P),!,current_predicate(M:F/A),functor(P,F,A))).
sandbox:safe_meta_predicate(V):-nonvar(V).
sandbox:safe_meta(V,O):-nonvar(V),nonvar(O).
sandbox:safe_global_variable(V):-nonvar(V).
sandbox:safe_directive(V):-nonvar(V).

:- gripe_time(40,ensure_loaded(logicmoo(xlisting/xlisting_web))),if_defined(xlisting_web:ensure_webserver).


end_of_file.




%

:- multifile '$si$':'$was_imported_kb_content$'/2.
:- dynamic '$si$':'$was_imported_kb_content$'/2.
:- discontiguous('$si$':'$was_imported_kb_content$'/2).

:- multifile baseKB:startup_option/2. 
:- dynamic baseKB:startup_option/2. 

:- ensure_loaded(system:library(logicmoo_utils_all)).

baseKB:startup_option(datalog,sanity). %  Run datalog sanity tests while starting
baseKB:startup_option(clif,sanity). %  Run datalog sanity tests while starting




/*
:- dynamic user:file_search_path/2.
:- multifile user:file_search_path/2.
:- user:prolog_load_context(directory,Dir),
   %Dir = (DirThis/planner),
   DirFor = logicmoo,
   (( \+ user:file_search_path(DirFor,Dir)) ->asserta(user:file_search_path(DirFor,Dir));true),
   absolute_file_name('../../../',Y,[relative_to(Dir),file_type(directory)]),
   (( \+ user:file_search_path(pack,Y)) ->asserta(user:file_search_path(pack,Y));true).
:- user:attach_packs.
:- initialization(user:attach_packs).

% [Required] Load the Logicmoo Library Utils
% baseKB:mpred_is_impl_file(error,logicmoo(logicmoo_utils)).

:- user:file_search_path(logicmoo,_)-> true; (user:prolog_load_context(directory,Dir),asserta_if_new(user:file_search_path(logicmoo,Dir))).

:- was_dynamic(baseKB:isa_pred_now_locked/0).
*/

% :- include(mpred/'mpred_header.pi').



/*
:- meta_predicate call_mpred_body(*,0).
:- meta_predicate decl_mpred_hybrid_ilc_0(*,*,0,*).
*/
:- meta_predicate t(*,?,?,?,?,?,?,?).
:- meta_predicate t(*,?,?,?,?,?,?).
:- meta_predicate t(*,?,?,?,?,?).
:- meta_predicate t(*,?,?,?).
:- meta_predicate t(*,?,?,?,?).
:- meta_predicate t(*,?,?).


% ========================================
% defaultAssertMt/1
% ========================================

% TODO uncomment the next line without breaking it all!
% baseKB:use_cyc_database.

:- asserta(baseKB:pfcManageHybrids).






% ================================================
% Debugging settings
% ================================================

:- was_export(is_stable/0).

is_stable:-fail.

:- if(current_prolog_flag(optimise,true)).
is_recompile.
:- else.
is_recompile:-fail.
:- endif.

fast_mud.
xperimental:-fail.
xperimental_big_data:-fail.

simple_code :- fail.
save_in_mpred_t:-true.
not_simple_code :- \+ simple_code.
type_error_checking:-false.
% slow_sanity(A):-nop(A).
:- meta_predicate xtreme_debug(0).

xtreme_debug(_):- skipWrapper,!.
xtreme_debug(P):- is_release,!,nop(P).
xtreme_debug(P):- not_is_release, sanity(P).
xtreme_debug(_).


:- meta_predicate sanity(0).
sanity(P):- quietly((\+ is_recompile,is_release,!,nop(P))).
sanity(P):- on_x_debug(quietly(P)),!.
sanity(P):- dmsg('$ERROR_incomplete_SANITY'(P)),!,ignore(ftrace(P)).

:- meta_predicate(when_debugging(+,0)).
when_debugging(What,Call):- debugging_logicmoo(What),!,Call.
when_debugging(_,_).

% :- asserta(tlbugger:no_colors).
% :- asserta(tlbugger:show_must_go_on).

:- set_prolog_flag(double_quotes, atom).
:- set_prolog_flag(double_quotes, string).

% ================================================
% DBASE_T System
% ================================================
:- gripe_time(40,ensure_loaded(logicmoo(xlisting/xlisting_web))).
% user:term_expansion((:-module(Name,List)), :-maplist(export,List)):- atom(Name),atom_concat(mpred_,_,Name).
% user:term_expansion((% % % OFF :- system:use_module(Name)), :-true):- atom(Name),atom_concat(mpred_,_,Name).




:- asserta(t_l:disable_px).

% user:goal_expansion(ISA,G) :- compound(ISA),t_l:is_calling,use_was_isa(ISA,I,C),to_isa_form(I,C,OUT),G=no_repeats(OUT).
:- meta_predicate(mpred_expander0(?,?,?,?)).
:- meta_predicate(lmbase_record_transactions_maybe(?,?)).
:- meta_predicate(mpred_file_expansion(?,?)).


% :- read_source_files.
% logicmoo_html_needs_debug.
:- if((baseKB:startup_option(www,sanity),if_defined(logicmoo_html_needs_debug))).
:- write(ready),nl,flush_output.
:- prolog.
:- endif.
:-  call(with_mfa_of( (dynamic_safe)),user,user,boxlog_to_compile(_D,_E,_F),boxlog_to_compile/3).
:- retractall(t_l:disable_px).

.

:- list_undefined.
