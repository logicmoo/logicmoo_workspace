
% ===================================================
fixundef_call(G):- format(string(Out),'% Need   ~q.~n',[:- G]), wdmsg(Out), (ground(G)->call(G) ; true).

module_of_pred(F/A,M,File):-
 with_no_retry_undefined(( current_module(M),
  current_predicate(M:F/A), functor(P,F,A), \+  /*ex*/predicate_property(M:P,imported_from(_)),!,
  ignore((source_file(M:P,File)->true;module_property(M,file(File)))))).

fixundef([]):-!.                                                       
fixundef([H|T]):- fixundef(H),fixundef(T).
fixundef(((M1:F2/A2) - Refs)):- !, %  clause_property(Ref,module(M1)), %  clause_property(Ref,predicate(P1)),
   P2 = F2/A2,fixundef(undef(M1:P2,Refs)).

fixundef(Info):-    
   Info = undef(M1:P2,_Refs),                        
   (module_of_pred(P2,M2,File2) ->
     maplist(fixundef_call,[M2:export(M2:P2),M1:import(M2:P2),M1:autoload(File2,[P2])]);
     (format(string(Out),'% ~q~n',[undef(M1:P2)]),dmsg(Out),assert_if_new(fixundef_later(Info)))).

fixundef_later:- with_no_retry_undefined((forall(retract(fixundef_later(M1P2)),fixundef(M1P2)),check:list_undefined)).

:- dynamic message_hook/3.
:- multifile message_hook/3.
:- module_transparent message_hook/3.

user:message_hook(check(undefined_procedures,List),_Type,_Warn):- fail, 
   rtrace,
   once(fixundef(List)),
   fail.
% ===================================================

:- include(('mpred_at_box.pl')).
:- include(('mpred_justify.pl')).
:- include(('mpred_core.pl')).
%:- include(('mpred_gvars.pl')).
:- include(('mpred_expansion.pl')).
:- include(('mpred_loader.pl')).                                                        
:- include(('mpred_database.pl')).
:- include(('mpred_listing.pl')).
%:- include(('mpred_prolog_file.pl')).
:- include(('mpred_terms.pl')).

%:- fixup_exports.

% premodule(pldoc_man 0.00 sec, 2 clauses
% premodule(gvlib).
% premodule(json).
% premodule(thread).
% premodule(license).
% premodule(process).
% premodule(prolog_metainference).
% premodule(record).
% premodule(dif).
% premodule(mimetype).
% premodule(prolog_config).
% premodule(terms).
% premodule(file_scope).
% premodule(pldoc_http).
% premodule(prolog_autoload).
% premodule($file_scope).
% premodule(prolog_debug).
% premodule(crypto).
% premodule(prolog_operator).
% premodule(quasi_quotations).
% premodule(ssl).
% premodule(dumpst).
% premodule($dcg).
% premodule($predopts).
% premodule(prolog_clause).
% premodule(zip).
% premodule(doc_util).
% premodule(varnumbers).
% premodule(t_l).
% premodule(pldoc_colours).
% premodule(sort).
% premodule(porter_stem).
% premodule(pldoc_wiki).
% premodule(dmsg).
% premodule(lmcache).
% premodule(ucatch).
% premodule(toplevel_variables).
% premodule(pldoc_index).
% premodule(crypto_hash).
% premodule($qlf).
% premodule(prolog_manual_index).
% premodule(pldoc_htmlsrc).
% premodule(settings).
% premodule(http_host).
% premodule(lazy_lists).
% premodule(ordsets).
% premodule(random).
% premodule(each_call_cleanup).
% premodule(logicmoo_util_terms).
% premodule(ansi_term).
% premodule(pce_dispatch).
% premodule($apply).
% premodule(writef).
% premodule(dictoo_lib).
% premodule(error).
% premodule(term_html).
% premodule(thread_pool).
% premodule(pldoc_modes).
% premodule(http_path).
% premodule(pce_compatibility_layer).
% premodule(apply_macros).
% premodule(dicts).
% premodule(date).
% premodule(http_exception).
% premodule(qsave).
% premodule(arithmetic).
% premodule(check).
% premodule(html_quasi_quotations).
% premodule(clause_attvars).
% premodule(pure_input).
% premodule(logicmoo_util_filestreams).
% premodule(html_head).
% premodule(swi_system_utilities).
% premodule(logicmoo_util_strings).
% premodule(http_dispatch).
% premodule(pldoc).
% premodule(pairs).
% premodule(prolog_listing).
% premodule($dwim).
% premodule($var_info).
% premodule(rtrace).
% premodule(util_varnames).
% premodule($syspreds).
% premodule(socket).
% premodule($gc).
% premodule(doc_access).
% premodule(thread_util).
% premodule(prolog_system_predicate_options).
% premodule(charsio).
% premodule(occurs).
% premodule(script_files).
% premodule(first).
% premodule(logicmoo_util_filesystem).
% premodule(xlisting_console).
% premodule(http_hook).
% premodule(prolog_edit).
% premodule(portray_vars).
% premodule(prolog_colour).
% premodule(yall).
% premodule($rc).
% premodule($toplevel).
% premodule(quintus).
% premodule(prolog_code).
% premodule(pce_messages).
% premodule(http_parameters).
% premodule(prolog_pack).
% premodule(apply).
% premodule($iri).
% premodule(memory_file).
% premodule(http_header).
% premodule($engines).
% premodule(http_stream).
% premodule(lists).
% premodule($attvar).
% premodule(archive).
% premodule(html_write).
% premodule(nb_set).
% premodule(edinburgh).
% premodule(mime_pack).
% premodule(codesio).
% premodule(shlib).
% premodule(pldoc_html).
% premodule(broadcast).
% premodule(logicmoo_common).
% premodule(backward_compatibility).
% premodule(pce).
% premodule(link_xpce).
% premodule(bugger).
% premodule(utf8).
% premodule(frames).
% premodule(base32).
% premodule($expand).
% premodule(clpfd).
% premodule(call_from).
% premodule($autoload).
% premodule(when).
% premodule(xpath).
% premodule(locally_each).
% premodule(http_client).
% premodule(aggregate).
% premodule(wfs).
% premodule(pldoc_register).
% premodule(rbtrees).
% premodule(sgml_write).
% premodule(uri).
% premodule(iostream).
% premodule($bags).
% premodule(swi_option).
% premodule(dcg_basics).
% premodule(http_multipart_plugin).
% premodule(sgml).
% premodule(subclause_expansion).
% premodule(git).
% premodule(prolog_breakpoints).
% premodule(must_sanity).
% premodule(no_repeats).
% premodule($tabling).
% premodule(start_emacs).
% premodule(url).
% premodule(http_open).
% premodule(nb_rbtrees).
% premodule(attvar_serializer).
% premodule($history).
% premodule(prolog_dialect).
% premodule(www_browser).
% premodule(solution_sequences).
% premodule(thread_httpd).
% premodule(lockable_vars).
% premodule(shell).
% premodule(prolog_source).
% premodule(time).
% premodule(hook_database).
% premodule(assoc).
% premodule(pldoc_search).
% premodule($pack).
% premodule(jquery).
% premodule(httpd_wrapper).
% premodule(make).
% premodule(gui_tracer).
% premodule(read_util).
% premodule($messages).
% premodule(predicate_options).
% premodule(http_server_files).
% premodule(pldoc_pack).
% premodule(ugraphs).
% premodule(prolog_statistics).
% premodule(prolog_stack).
% premodule(prolog_format).
% premodule(loop_check).
% premodule(pldoc_files).
% premodule(virtualize_source).
% premodule($dicts).
% premodule(doc_words).
% premodule(editline).
% premodule(atom).
% premodule(prolog_history).
% premodule(jpl).
% premodule(pldoc_process).
% premodule(files_ex).
% premodule(prolog_pretty_print).
% premodule(prolog_xref).
% premodule(prolog_codewalk).
% premodule(gensym).
% premodule(ctypes).
% premodule(oset).
% premodule(base64).
% premodule(modules).
% premodule(predicate_inheritance).

