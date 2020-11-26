/* <module> xlisting_web
% Provides /logicmoo runtime preds browsing
%
%
% Logicmoo Project PrologMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/
% :-module(xlisting_web,[ensure_sigma/0,search4term/0]).
%:- if(( ( \+ ((current_prolog_flag(logicmoo_include,Call),Call))) )). 
:- module(xlisting_web,
          [ action_menu_applied/3,
            %action_menu_item/2,
            add_form_script/0,
            register_logicmoo_browser/0,
            as_ftVars/1,
            call_for_terms/1,
            classify_alpha_tail/1,
            classify_name/2,
            classify_other_tail/1,
            current_form_var/1,
            current_line_position/1,
            current_line_position/2,
            cvt_param_to_term/2,
            cvt_param_to_term/3,
            do_guitracer/0,
            edit1term/0,
            output_telnet_console/1,
            edit1term/1,
            ensure_sigma/1,
            ensure_sigma/0,
            find_cl_ref/2,
            find_ref/2,
            fmtimg/2,
            'functor spec'/4,
            functor_to_color/2,
            functor_to_color/4,
            
            get_http_current_request/1,
            get_http_session/1,
            get_nv_session/3,
            get_param_req/2,
            get_param_sess/2,
            get_param_sess/3,
            get_request_vars/1,
            handler_logicmoo_cyclone/1,
            head_functor_sort/3,
            must_run/1,
            human_language/1,
            i2tml_hbr/3,
            if_html/2,
            output_html/1,
            write_html/1,
            show_map_legend/0,
            indent_nbsp/1,
            indent_nbsp/2,
            indent_nl/0,
            is_cgi_stream/0,
            is_context/2,
            is_goog_bot/0,
            'list clauses'/4,
            'list magic'/2,
            'list magic'/3,
            'list magic'/4,
            logic_lang_name/2,
            make_page_pretext_obj/1,
            make_quotable/2,
            make_session/1,
            maybe_paren/5,
            maybe_space/2,
            member_open/2,
            merge_key_vals/3,
            name_the_var/5,
            nl_same_pos/0,
            numberlist_at/2,
            object_sub_page/4,
            %param_default_value/2,
            param_matches/2,
            parameter_names/2,
            %partOfSpeech/2,
            portable_display/1,
            portable_listing/0,
            portable_listing/1,
            portable_print/1,
            portable_write/1,
            portable_writeq/1,
            pp_i2tml/1,
            pp_i2tml_now/1,
            pp_i2tml_save_seen/1,
            pp_i2tml_saved_done/1,
            pp_i2tml_v/1,
            pp_item_html/2,
            pp_item_html_if_in_range/2,
            pp_item_html_now/2,
            pp_now/0,
            print_request/1,
            prover_name/2,
            
            reply_object_sub_page/1,
            reset_assertion_display/0,
            return_to_pos/1,
            rok_portray_clause/1,
            save_in_session/1,
            save_in_session/2,
            save_in_session/3,
            save_request_in_session/1,
            search4term/0,
            search_filter_name_comment/3,
            section_close/1,
            section_open/1,
            sensical_nonvar/1,
            session_checkbox/3,
            session_checked/1,
            set_line_pos/1,
            set_line_pos/2,
            show_clause_ref/1,
            show_clause_ref_now/1,
            show_edit_term/3,
               show_http_session/0,
            show_iframe/1,
            show_iframe/3,
            show_pcall_footer/0,
            show_search_filters/1,
            show_search_filtersTop/1,
            term_to_pretty_string/2,
            this_listing/1,
            test_tmw/0,
            tovl/3,
            url_decode/2,
            url_decode_term/2,
            url_encode/2,
            url_encode_term/3,
            with_search_filters/1,
            with_search_filters0/1,
            write_VAR/4,
            write_args/5,
            write_as_url_encoded/2,
            write_atom/4,
            write_atom_link/1,
            write_atom_link/2,
            write_atom_link/3,
            write_begin_html/3,
            write_end_html/0,
            write_oper/5,
            write_out/5,
            write_oout/7,
            write_tail/2,
            write_term_to_atom_one/2,
            write_variable/1,
          
          xlisting_web_file/0
            /*
            http:location/3,
            http_dispatch:handler/4,
            http_log:log_stream/2,
            http_session:session_data/2,
            http_session:urandom_handle/1,
            system:'$init_goal'/3,
            user:file_search_path/2
            */
          ]).


:- set_module(class(library)).
/*
% % % OFF :- system:use_module(library(hook_database)).
% % % OFF :- system:use_module(library(logicmoo/no_repeats)).
% % % OFF :- system:use_module(library(logicmoo/each_call)).
% % % OFF :- system:use_module(library(logicmoo/locally_redo)).
% % % OFF :- system:use_module(library(logicmoo/virtualize_source)).% WAS OFF  :- system:use_module(library(no_repeats)).
*/
% % % OFF :- system:use_module(library(logicmoo/attvar_serializer)).

:- dynamic user:library_directory/1.
:- multifile user:library_directory/1.
hide_xpce_library_directory:- 
  user:library_directory(X),
  atom(X),
  atom_concat(_,'xpce/prolog/lib/',X),!,
  retract((user:library_directory(X))),
  assert((user:library_directory(X):- \+ current_prolog_flag(hide_xpce_library_directory,true))).
hide_xpce_library_directory.

:- hide_xpce_library_directory.
:- set_prolog_flag(hide_xpce_library_directory,true).

%:- ensure_loaded(library(logicmoo_swilib)).
% % % OFF :- system:use_module(library(http/thread_httpd)).
% % % OFF :- system:use_module(thread_httpd:library(http/http_dispatch)).
% % % OFF :- system:use_module(swi(library/http/html_write)).
% % % OFF :- system:use_module(swi(library/http/html_head)).
% % % OFF :- system:use_module(library(http/http_dispatch)).
% % % OFF :- system:use_module(library(http/http_path)).
% % % OFF :- system:use_module(library(http/http_log)).
% % % OFF :- system:use_module(library(http/http_client)).
% % % OFF :- system:use_module(library(http/http_server_files)).
% % % OFF :- system:use_module(library(http/http_parameters)).


% % % OFF :- system:use_module(library(predicate_streams)).
% % % OFF :- system:use_module(library(logicmoo/with_no_x)).
% % % OFF :- system:use_module(library(logicmoo/each_call)).
% % % OFF :- system:use_module(library(logicmoo/butterfly)).


:- thread_local(t_l:no_cycstrings/0).
:- asserta(t_l:no_cycstrings).

/*
:- include(library('pfc2.0'/'mpred_header.pi')).
:-
 op(1199,fx,('==>')), 
 op(1190,xfx,('::::')),
 op(1180,xfx,('==>')),
 op(1170,xfx,'<==>'),  
 op(1160,xfx,('<-')),
 op(1150,xfx,'=>'),
 op(1140,xfx,'<='),
 op(1130,xfx,'<=>'), 
 op(600,yfx,'&'), 
 op(600,yfx,'v'),
 op(350,xfx,'xor'),
 op(300,fx,'~'),
 op(300,fx,'-').

:- 
 user:((
 op(1199,fx,('==>')), 
 op(1190,xfx,('::::')),
 op(1180,xfx,('==>')),
 op(1170,xfx,'<==>'),  
 op(1160,xfx,('<-')),
 op(1150,xfx,'=>'),
 op(1140,xfx,'<='),
 op(1130,xfx,'<=>'), 
 op(600,yfx,'&'), 
 op(600,yfx,'v'),
 op(350,xfx,'xor'),
 op(300,fx,'~'),
 op(300,fx,'-'))).
*/
%:- endif.

:- thread_local(t_l:print_mode/1).

:- if(exists_source(cliopatria('applications/help/load'))).
% % % OFF :- system:use_module(cliopatria('applications/help/load')).
% Load ClioPatria itself.  Better keep this line.
% % % OFF :- system:use_module(cliopatria(cliopatria)).
:- else.
cp_menu:cp_menu(X,X).
cp_menu:cp_menu.
:- endif.

:- kb_global(baseKB:param_default_value/2).
:- kb_global(baseKB:mtExact/1).

:- meta_predicate 
        edit1term(*),
        handler_logicmoo_cyclone(+),
        must_run(*),
        output_html(//),
        if_html(?, 0),
        return_to_pos(0),
        show_edit_term(0, ?, ?),
        show_edit_term0(0, ?, ?),
        show_edit_term1(0, ?, ?),
        with_search_filters(0),
        with_search_filters0(0).
:- (multifile http:location/3, http_dispatch:handler/4, http_log:log_stream/2, http_session:session_data/2, http_session:urandom_handle/1, baseKB:shared_hide_data/1, system:'$init_goal'/3, user:file_search_path/2).
:- (module_transparent edit1term/1, must_run/1, if_html/2, return_to_pos/1, show_edit_term/3, show_edit_term0/3, show_edit_term1/3, with_search_filters/1).
:- (volatile http_log:log_stream/2, http_session:session_data/2, http_session:urandom_handle/1).
:- export((current_form_var0/1, get_http_session0/1,  is_context0/1, make_quotable_0/2, pp_i2tml_0/1, pp_i2tml_1/1, sanity_test_000/0, show_edit_term0/3, show_edit_term1/3, show_select1/2, show_select2/3)).
:- multifile((lmcache:last_item_offered/1, http:location/3, http_dispatch:handler/4, http_session:session_data/2, http_session:urandom_handle/1,
   foobar/1, lmcache:last_http_request/1, lmcache:last_item_offered/1, system:'$init_goal'/3, user:file_search_path/2)).


:- thread_initialization(nb_setval(pldoc_options,[ prefer(manual) ])).

:- meta_predicate must_run(0).
:- meta_predicate must_run(0).
:- meta_predicate with_search_filters(0).
:- meta_predicate return_to_pos(0).
:- meta_predicate show_edit_term1(0,*,*).
:- meta_predicate show_edit_term0(0,*,*).
:- meta_predicate show_edit_term(0,*,*).
:- meta_predicate edit1term(0).

:- meta_predicate www_main_error_to_out(0).

www_main_error_to_out(G):- with_main_error_to_output(G).

%% ensure_sigma( ?ARG1) is det.
%
% Ensure Webserver.
%
ensure_sigma(Port) :- format(atom(A),'httpd@~w_1',[Port]),thread_property(_,alias(A)),!.
ensure_sigma(Port) :- on_x_debug(catch((http_server(http_dispatch,[ port(Port), workers(16) ])),E,wdmsg(E))).



%% ensure_sigma is det.
%
% Ensure Webserver.
%
ensure_sigma:- ensure_sigma(3020).

:- multifile(http_session:session_data/2).
:- volatile(http_session:session_data/2).

:- multifile(system:'$loading_file'/3).
:- volatile(system:'$loading_file'/3).

:- multifile(http_session:urandom_handle/1).
:- volatile(http_session:urandom_handle/1).

:- multifile(http_log:log_stream/2).
:- volatile(http_log:log_stream/2).



:- if( \+ exists_source(library(logicmoo_utils_all))).
:- dynamic user:file_search_path/2.
:- multifile user:file_search_path/2.
:- prolog_load_context(directory,Dir),
   DirFor = mpred_online,
   (( \+ user:file_search_path(DirFor,Dir)) ->asserta(user:file_search_path(DirFor,Dir));true),
   absolute_file_name('../../',Y,[relative_to(Dir),file_type(directory)]),
   (( \+ user:file_search_path(pack,Y)) ->asserta(user:file_search_path(pack,Y));true).
:- initialization(attach_packs,now).
% [Required] Load the Logicmoo Library Utils
:- endif.
 




% :- portray_text(false).  % or Enable portray of strings


:- thread_local(t_l:omit_full_stop).

% :- thread_property(_,alias('http@3020'))->true; http_server(http_dispatch, [port(3020)]).

register_logicmoo_browser:- 
  http_handler('/logicmoo/', handler_logicmoo_cyclone, [prefix]), % chunked
  http_handler('/logicmoo_nc/', handler_logicmoo_cyclone, [prefix,chunked]),
  http_handler('/swish/logicmoo/', handler_logicmoo_cyclone, [prefix]), % chunked
  http_handler('/swish/logicmoo_nc/', handler_logicmoo_cyclone, [prefix,chunked]),
  doc_collect(true).


%% location( ?ARG1, ?ARG2, ?ARG3) is det.
%
% Hook To [http:location/3] For Module Mpred_www.
% Location.
%
:- assert_if_new(http:location(pixmapx, root(pixmapx), [])).



%% user:file_search_path( ?ARG1, ?ARG2) is det.
%
% Hook To [user:file_search_path/2] For Module Mpred_www.
% File Search Path.
%
:- prolog_load_context(directory,Here),atom_concat(Here,'/pixmapx',NewDir),asserta((user:file_search_path(pixmapx,NewDir))).
% user:file_search_path(pixmapx, logicmoo('mpred_online/pixmapx')).

:- during_boot(http_handler(pixmapx(.), http_server_files:serve_files_in_directory(pixmapx), [prefix])).

:- meta_predicate
	handler_logicmoo_cyclone(+).

:- must(prolog_load_context(module,xlisting_web)).
in_xlisting_web1.
:- must( \+ pfc_lib:is_pfc_file0).
:- ensure_loaded('xlisting_web.pfc').
:- must( \+ is_pfc_file).

in_xlisting_web2.
:- xlisting_web:listing(in_xlisting_web2).

%% print_request( :TermARG1) is det.
%
% Print Request.
%
print_request([]).
print_request([H|T]) :-
        H =.. [Name, Value],
        bformat(user_error,'<tr><td>~w<td>~w~n', [Name, Value]),
        print_request(T).


:- xlisting_web:listing(print_request/1).




%% make_quotable_0( ?ARG1, ?ARG2) is det.
%
% make quotable  Primary Helper.
%
make_quotable_0(SUnq0,SObj):-
  any_to_string(SUnq0,SUnq),
  atom_subst(SUnq,'\\','\\\\',SObj0),atom_subst(SObj0,'\n','\\n',SObj1),atom_subst(SObj1,'"','\\\"',SObj).



%% make_quotable( ?ARG1, ?ARG2) is det.
%
% Make Quotable.
%
make_quotable(String,SObj):-string(String),format(string(SUnq),'~s',[String]),make_quotable_0(SUnq,SObj),!.
make_quotable(String,SObj):-atomic(String),format(string(SUnq),'~w',[String]),make_quotable_0(SUnq,SObj),!.
make_quotable(String,SObj):-format(string(SUnq),'~q',[String]),make_quotable_0(SUnq,SObj),!.

% 
% <link rel="SHORTCUT ICON" href="/pixmapx/mini-logo.gif"><meta name="ROBOTS" content="NOINDEX, NOFOLLOW">

% :- set_yes_debug.

:- export(save_in_session/1).



%% save_in_session( :TermARG1) is det.
%
% Save In Session.
%
save_in_session(NV):- \+ compound(NV),!.
save_in_session(NV):-is_list(NV),!,must_maplist(save_in_session,NV),!.
save_in_session(search([X=Y|R])):-nonvar(Y),is_list([X=Y|R]),once(save_in_session([X=Y|R])),!.
save_in_session(NV):-NV=..[N,V],!,must_run(save_in_session(N,V)),!.
save_in_session(N=V):- must_run(save_in_session(N,V)),!.
save_in_session(NV):- dmsg(not_save_in_session(NV)),!.

:- export(save_in_session/2).



%% save_in_session( ?ARG1, ?ARG2) is det.
%
% Save In Session.
%
save_in_session(Unsaved,_):- member(Unsaved,[session_data,request_uri,search,pool,path,input,session]),!.
save_in_session(_,V):- sub_term(Sub,V),nonvar(Sub),is_stream(Sub),!.
save_in_session(N,V):- get_http_session(S), save_in_session(S, N,V),!.

% save_in_session(S,N,V):- \+ param_default_value(N,_),!.



%% save_in_session( ?ARG1, ?ARG2, ?ARG3) is det.
%
% Save In Session.
%
save_in_session(S,N,V):- atom(N), NV=..[N,V],functor(NVR,N,1),
   retractall(http_session:session_data(S,NVR)),
   asserta(http_session:session_data(S,NV)),!.
save_in_session(S,N,V):- dmsg(not_save_in_session(S,N,V)),!.





%% show_http_session is det.
%
% Show Http Session.
%
show_http_session:-must_run(get_http_session(S)),listing(http_session:session_data(S,_NV)).
  




%% make_session( ?ARG1) is det.
%
% Make Session.
%
make_session(S):- catch((is_cgi_stream->http_session:http_open_session(S,[renew(false)]);true),_,true).



:- export(get_http_session/1).
%% get_http_session( ?ARG1) is det.
%
% Get Http Session.
%
get_http_session(S):- catch(get_http_session0(S),_,fail),nonvar(S),!, make_session(S).
get_http_session(main).

% on_x_log_fail(G):- catch(G,E,(dmsg(E:G),fail)).


:- export(get_http_session0/1).
%% get_http_session0( ?ARG1) is det.
%
% Get Http Session Primary Helper.
%
get_http_session0(S):- on_x_log_fail((http_session:http_in_session(S))),!.
get_http_session0(S):- on_x_log_fail((get_http_current_request(R),member(session(S),R))),!.
get_http_session0(S):- on_x_log_fail((get_http_current_request(R),member(cookie([swipl_session=S]),R))),!.
get_http_session0(S):- is_cgi_stream,catch(((http_session:http_open_session(S,[renew(false)]))),_,true),!.




%% is_cgi_stream is det.
%
% If Is A Cgi Stream.
%
is_cgi_stream:-current_output(X),http_stream:is_cgi_stream(X).




%% reset_assertion_display is det.
%
% Reset Assertion Display.
%
reset_assertion_display:-
   flag(matched_assertions,_,0),
   flag(show_asserions_offered,_,0),
   retractall(shown_subtype(_)),
   retractall(xlw:shown_clause(_)).




%% get_param_sess( ?ARG1, ?ARG2) is det.
%
% Get Param Sess.
%
get_param_sess(N,V):- must_run(param_default_value(N,D);D=''),!,get_param_sess(N,V,D),!.

:- dynamic(lmcache:last_http_request/1).
:- volatile(lmcache:last_http_request/1).
:- dynamic(lmcache:last_item_offered/1).
:- volatile(lmcache:last_item_offered/1).

%% lmcache:last_item_offered( ?ARG1) is det.
%
% Last Item Offered.
%
:- asserta(lmcache:last_item_offered(unknown)).





%% get_http_current_request( ?ARG1) is det.
%
% Get Http Current Request.
%
get_http_current_request(B):- httpd_wrapper:http_current_request(B), !,ignore((retractall(lmcache:last_http_request(_)),asserta(lmcache:last_http_request(B)))).
get_http_current_request(B):- lmcache:last_http_request(B),!.
get_http_current_request([]).




%% get_param_sess( ?ARG1, ?ARG2, ?ARG3) is det.
%
% Get Param Sess.
%
get_param_sess(N,V,D):- nonvar(V),!,get_param_sess(N,VV,D),!,param_matches(V,VV).
get_param_sess(L,V,D):-get_nv_session(L,V,D).




%% get_param_req( ?ARG1, ?ARG2) is det.
%
% Get Param Req.
%
get_param_req(L,V):- (is_list(L)-> member(N,L) ; N=L),
     CALL2 =.. [N,V,[optional(true),default(Foo)]],
  get_http_current_request(B),
   http_parameters:http_parameters(B,[CALL2])->
       V \== Foo,!.

% get_param_sess(L,V,V):- (is_list(L)-> member(N,L) ; N=L), save_in_session(N=V),!.




%% get_nv_session( ?ARG1, ?ARG2, ?ARG3) is det.
%
% Get Nv Session.
%
get_nv_session(L,V,_):- (is_list(L)-> member(N,L) ; N=L),
     CALL2 =.. [N,V], (get_http_session(F),http_session:session_data(F, CALL2)),!.
get_nv_session(_,V,V):-!.





%% save_request_in_session( ?ARG1) is det.
%
% Save Request In Session.
%
save_request_in_session(Request):- 
        (member(method(post), Request) -> (http_read_data(Request, Data, []),save_in_session(Data));true),
        save_in_session(Request).
        % http_session:http_session_id(F),forall(http_session:session_data(F,D),wdmsg(D)).



:- dynamic(lmcache:current_ioet/4).
:- volatile(lmcache:current_ioet/4).

:- create_prolog_flag(retry_undefined,default,[type(term),keep(true)]).

%% handler_logicmoo_cyclone( +Request) is det.
%
% Handler Logicmoo Cyclone.
%
handler_logicmoo_cyclone(_):- quietly(is_goog_bot),!,
  quietly((format('Content-type: text/html~n~n',[]),
  bformat('<!DOCTYPE html><html><head></head><body><pre></pre></body></html>~n~n',[]),
  flush_output_safe)),!.
handler_logicmoo_cyclone(Request):- quietly(is_goog_bot),!,
  quietly((format('Content-type: text/html~n~n',[]),
  bformat('<!DOCTYPE html><html><head></head><body><pre>~q</pre></body></html>~n~n',[Request]),
  flush_output_safe)),!.

handler_logicmoo_cyclone(Request):-
 wdmsg(handler_logicmoo_cyclone(Request)),
 ignore((
 %nodebugx
 ((
  ignore(get_http_session(_)), 
  locally(set_prolog_flag(retry_undefined, none),
    % with_no_x
    (( 
     must_run((
      current_input(In),current_output(Out),
       (stream_property(Err,file_no(2));current_error_stream(Err)),
   thread_self(ID),!,
   asserta(lmcache:current_ioet(In,Out,Err,ID)),
%    format('Content-type: text/html~n~n',[]),
   html_write:html_current_option(content_type(D)),
   format('Content-type: ~w~n~n', [D]),
   bformat('<!DOCTYPE html>',[]),flush_output_safe,
    must_run(save_request_in_session(Request)),
    % member(request_uri(URI),Request),
     member(path(PATH),Request),
    directory_file_path(_,FCALL,PATH),
   once(get_param_req(webproc,Call);(current_predicate(FCALL/0),Call=FCALL);get_param_sess(webproc,Call,edit1term)),
   must_run(Call)))))))))),!.
   


:- asserta(cp_menu:menu_item(500=places/handler_logicmoo_cyclone,	'LogicMOO')).
:- asserta(cp_menu:menu_item(500=swish/handler_logicmoo_cyclone,	'LogicMOO')).


%% write_begin_html( ?ARG1, ?ARG2, ?ARG3) is det.
%
% Write Begin HTML.
%
write_begin_html(B,BASE,URI):-  
  must_run((
      % sformat(BASE,'~w~@',[B,get_request_vars('_n_~w_v0_~w_vZ')]),
      BASE = B,
      bformat('<html><head><style type="text/css">
   element.style {
    position: relative;
    min-height: 100%;
    top: 0px;
}
html, body {
    font-family: Verdana,sans-serif;
    font-size: 10px;
    line-height: 1.5;
}
body {
    margin: 1;
}
        input[type="checkbox"] {width:10px; height:10px; }</style>',
        []),            
      must_run((get_http_current_request(Request))),
      must_run(member(request_uri(URI),Request)->true;URI=''),
      % ((URI\==''->bformat('<meta http-equiv="refresh" content="300;~w">',[URI]);true)),
      % must_run((BASE\='' -> bformat('<base href="~w" target="_parent"/>',[BASE]);true)),
      ignore(URI=''),
      ignore(BASE=''),
     bformat('<script src="http://ajax.googleapis.com/ajax/libs/jquery/1.7.1/jquery.min.js"></script>',[]),
     html_head:output_html(html_requires(plain)),     
     bformat('<title>~w for ~w</title>
      <meta http-equiv="X-Frame-Options" content="ALLOWAll">
      <link rel="stylesheet" type="text/css" href="/css/cliopatria.css">
      <link rel="stylesheet" type="text/css" href="/css/menu.css">
      <script type="text/javascript" src="/js/jquery-2.1.3.min.js"></script>
      <script type="text/javascript" src="/js/cliopatria.js"></script>
      <link rel="stylesheet" type="text/css" href="/www/yui/2.7.0/build/autocomplete/assets/skins/sam/autocomplete.css">
      <script type="text/javascript" src="/www/yui/2.7.0/build/utilities/utilities.js"></script>
      <script type="text/javascript" src="/www/yui/2.7.0/build/datasource/datasource.js"></script>
      <script type="text/javascript" src="/www/yui/2.7.0/build/autocomplete/autocomplete.js"></script></head>',
   [BASE,URI]),
     bformat('<body class="yui-skin-sam">',[]),flush_output_safe)),!,
  with_output_to(string(SMenu),output_html(cp_menu:cp_menu)),
  output_html(div([id('cp-menu'), class(menu)], SMenu)).     

   
test_rok:- test_rok(test_rok).

/*
dasm:print_clause_plain(Term) :-
        current_prolog_flag(color_term, Prolog_flag_Ret),
        make_pretty(Term, Make_pretty_Ret),
        setup_call_cleanup(set_prolog_flag(color_term, false),
                           ( nl,
                             lcolormsg1(Make_pretty_Ret)
                           ),
                           set_prolog_flag(color_term, Prolog_flag_Ret)).
*/

test_rok(W) :- handler_logicmoo_cyclone([path_info(search4term), protocol(http), peer(ip(127, 0, 0, 1)), 
  In = user_input,
  Out = user_put,
  format(atom(S4T),'/logicmoo/search4term?find=~w',[W]),
  pool(client('httpd@3020', http_dispatch, In, Out)),
    input(In), method(get), request_uri(S4T),
     path('/logicmoo/search4term'), search([find=W]), 
     http_version(1-1), host('127.0.0.1'), port(3020), cache_control('max-age=0'), 
     upgrade_insecure_requests('1'), user_agent('Mozilla/5.0 (Windows NT 6.1; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/67.0.3393.4 Safari/537.36'),
     accept([media(text/html, [], 1.0, []), media(application/'xhtml+xml', [], 1.0, []), 
     media(image/webp, [], 1.0, []), media(image/apng, [], 1.0, []), media(application/xml, [], 0.9, []), 
     media(_9672/_9674, [], 0.8, [])]), accept_encoding('gzip, deflate'), accept_language('en-US,en;q=0.9'), 
     cookie(['PHPSESSID'=u265i7e611jval7odhrs316n07, '_ga'='GA1.2.854971883.1519291037', 
     session='eyJjc3JmX3Rva2VuIjoiMGU3MzE1ZWUxMjVkZTNlZDNlZDg3ZDgyNWQ5ZmZiNjMxNjE4ODdjZiJ9.DYDY5A.so4fbyaXlbCXtzExefb_aYRjJ6g', 
     io='DjFUY0jh0SbK64uLAAAM', lo_session_in='1', '_jsuid'='984133034', 
     '__lotl'='http%3A%2F%2Flogicmoo.org%2Fdocs%2FA%2520Fuzzy%2520Belief-Desire-Intention%2520Model%2520for%2520Agent-Based%2520Image%2520Analysis%2520_%2520IntechOpen.html', 
     euCookie='1', swipl_session='cc4e-bdf6-b3ff-9ffc.gitlab']), x_forwarded_for('10.0.0.122'), x_forwarded_host('logicmoo.org'),
      x_forwarded_server('127.0.1.1'), connection('Keep-Alive')]),!.


%% write_end_html is det.
%
% Write End HTML.
%
write_end_html:- flush_output_safe,bformat('</body></html>~n~n',[]),flush_output_safe,!.

% logicmoo_html_needs_debug.




%% add_form_script is det.
%
% Add Form Script.
%
add_form_script:-
format("<script type=\"text/javascript\">
$('form').submit(function() {
  $(this).find('input[type=checkbox]').each(function (i, el) {
    if(!el.checked) {
      var hidden_el = $(el).clone();
      hidden_el[0].checked = true;
      hidden_el[0].value = '0';
      hidden_el[0].type = 'hidden';
      hidden_el.insertAfter($(el));
    }    
  })
 // alert($(this));
});

var handled = false;

function callback(e) {
    var e = window.e || e;

    var targ = e.target;
    if (targ.tagName !== 'A')
        return;
    if(!handled) {     
      handled = true;
     // alert('hi ' +  targ.target);
      if (targ.target !== '') {
       return;
      }
      e.preventDefault();
      e.stopPropagation();
      $('form').action = targ.href;
      document.getElementById('find').value = targ.innerText;
     // alert('hi ' +  targ.innerText);
      $('form').submit();
    } else {
      handled = false;           
    }
}

if (document.addEventListener)
    document.addEventListener('click', callback, false);
else
    document.attachEvent('onclick', callback);
</script>"
).





%% show_pcall_footer is det.
%
% Show Pcall Footer.
%
show_pcall_footer:- bformat('<hr><a href="http://prologmoo.com">LogicMOO/PrologMUD</a>',[]),!.




%% sensical_nonvar( ?ARG1) is det.
%
% Sensical Nonvar.
%
sensical_nonvar(O):-nonvar(O), O \= (_ - _).




%% cvt_param_to_term( ?ARG1, ?ARG2, ?ARG3) is det.
%
% Cvt Param Converted To Term.
%
cvt_param_to_term(In,Obj,Vs):-atom(In),on_x_fail(atom_to_term(In,Obj,Vs)),sensical_nonvar(Obj),!.
cvt_param_to_term(In,Obj,Vs):-string(In),on_x_fail(atom_to_term(In,Obj,Vs)),sensical_nonvar(Obj),!.



%% cvt_param_to_term( ?ARG1, ?ARG2) is det.
%
% Cvt Param Converted To Term.
%
cvt_param_to_term('~w',""):-!.
cvt_param_to_term(In,Obj):-cvt_param_to_term(In,Obj,_Vs),!.
cvt_param_to_term(Obj,Obj).



% :- (thread_property(ID,status(running)),ID=reloader30) -> true; thread_create(((repeat,sleep(30),mmake,fail)),_,[alias(reloader30),detached(true)]).
% ===================================================
% Pretty Print Formula
% ===================================================


%% write_atom_link( ?ARG1) is det.
%
% Write Atom Link.
%
:- export(write_atom_link/1).
write_atom_link(A):-must_run(write_atom_link(A,A)).



%% write_atom_link( ?ARG1, ?ARG2) is det.
%
% Write Atom Link.
%
:- export(write_atom_link/2).
write_atom_link(L,N):-must_run((write_atom_link(atom(W),L,N),bformat('~w',[W]))),!.

% pred_href(Name/Arity, Module, HREF) :-



%% write_atom_link( ?ARG1, ?ARG2, ?ARG3) is det.
%
% Write Atom Link.
%
:- export(write_atom_link/3).
write_atom_link(W,A/_,N):-atom(A),!,write_atom_link(W,A,N).
write_atom_link(W,C,N):-compound(C),get_functor(C,F,A),!,write_atom_link(W,F/A,N).
%write_atom_link(W,_,N):- thread_self_main,!,write_term_to_atom_one(W,N),!.
write_atom_link(W,_,N):- must_run(nonvar(W)),\+ is_html_mode,write_term_to_atom_one(W,N),!.
write_atom_link(W,A,N):- sanity(nonvar(W)),
 catch((format(atom(AQ),'~q',[A]),url_encode(AQ,URL),
   format(W,'<a href="?find=~w">~w</a>',[URL,N])),_,write_term_to_atom_one(W,N)).




%% write_term_to_atom_one( :TermARG1, ?ARG2) is det.
%
% Write Term Converted To Atom One.
%
write_term_to_atom_one(atom(A),Term):-format(atom(A),'~q',[Term]).

/*


%   File   : WRITE.PL
%   Author : Richard A. O'Keefe.
%   Updated: 22 October 1984
%   Purpose: Portable definition of write/1 and friends.

:- public
	portable_display/1,
	portable_listing/0,
	portable_listing/1,
	portable_print/1,
	portable_write/1,
	portable_writeq/1,
	rok_portray_clause/1.

:- meta_predicate
	classify_name(+, -),
	classify_alpha_tail(+),
	classify_other_tail(+),
	'functor spec'(+, -, -, -),
	'list clauses'(+, +, +, +),
	'list magic'(+, +),
	'list magic'(+, +, +),
	'list magic'(+, +, +, +),
	maybe_paren(+, +, +, +, -),
	maybe_space(+, +),
	rok_portray_clause(+),
	put_string(+),
	put_string(+, +),
	write_args(+, +, +, +, +),
	write_atom(+, +, +, -),
	write_oper(+, +, +, +, -),
	write_out(+, +, +, +, -),
	write_out(+, +, +, +, +, +, -),
	write_tail(+, +),
	write_VAR(+, +, +, -),
	write_variable(?).
*/
     

/*  WARNING!
    This file was written to assist portability and to help people
    get a decent set of output routines off the ground fast.  It is
    not particularly efficient.  Information about atom names and
    properties should be precomputed and fetched as directly as
    possible, and strings should not be created as lists!

    The four output routines differ in the following respects:
    [a] display doesn't use operator information or handle {X} or
	[H|T] specially.  The others do.
    [b] print calls portray/1 to give the user a chance to do
	something different.  The others don't.
    [c] writeq puts quotes around atoms that can't be read back.
	The others don't.
    Since they have such a lot in common, we just pass around a
    single Style argument to say what to do.

    In a Prolog which supports strings;
	write(<string>) should just write the text of the string, this so
	that write("Beware bandersnatch") can be used.  The other output
	commands should quote the string.

    listing(Preds) is supposed to write the predicates out so that they
    can be read back in exactly as they are now, provided the operator
    declarations haven't changed.  So it has to use writeq.  $VAR(X)
    will write the atom X without quotes, this so that you can write
    out a clause in a readable way by binding each input variable to
    its name.
*/





%% portable_display( ?ARG1) is det.
%
% Portable Display.
%
portable_display(Term) :-
	write_out(Term, display, 1200, punct, _).





%% portable_print( ?ARG1) is det.
%
% Portable Print.
%
portable_print(Term) :-
	write_out(Term, print, 1200, punct, _).





%% portable_write( ?ARG1) is det.
%
% Portable Write.
%
portable_write(Term) :-
	write_out(Term, write, 1200, punct, _).





%% portable_writeq( ?ARG1) is det.
%
% Portable Writeq.
%
portable_writeq(Term) :-
       write_out(Term, writeq, 1200, punct, _).



%   maybe_paren(P, Prio, Char, Ci, Co)
%   writes a parenthesis if the context demands it.




%% maybe_paren( ?ARG1, ?ARG2, ?ARG3, ?ARG4, ?ARG5) is det.
%
% Maybe Paren.
%
maybe_paren(P, Prio, Char, _, punct) :-
	P > Prio,
	!,
	put(Char).
maybe_paren(_, _, _, C, C).



%   maybe_space(LeftContext, TypeOfToken)
%   generates spaces as needed to ensure that two successive
%   tokens won't run into each other.




%% maybe_space( ?ARG1, ?ARG2) is det.
%
% Maybe Space.
%
maybe_space(punct, _) :- !.
maybe_space(X, X) :- !,
	put(32).
maybe_space(quote, alpha) :- !,
	put(32).
maybe_space(_, _).




%   write_variable(V)
%   is system dependent.  This just uses whatever Prolog supplies.




%% write_variable( ?ARG1) is det.
%
% Write Variable.
%
write_variable(V) :-
	write(V).




portray_or_print(Term):- catch(user:portray(Term),_,fail),!.
portray_or_print(Term):- catch(print(Term),_,fail),!.

%%   write_out(Term, Style, Priority, Ci, Co)
%   writes out a Term in a given Style (display,write,writeq,print)
%   in a context of priority Priority (that is, operators with
%   greater priority have to be quoted), where the last token to be
%   written was of type Ci, and reports that the last token it wrote
%   was of type Co.


write_out(Term, Style, Prio, Ci, Co):-
 write_oout(Term, Style, Prio, Ci, Co).

%% write_oout(Term, Style, Prio, Ci, Co) is det.
%
% Write Out.
%
write_oout(Term, _, _, Ci, alpha) :-
	var(Term),
	!,
	maybe_space(Ci, alpha),
	write_variable(Term).
write_oout('$VAR'(N), Style, _, Ci, Co) :- !,
	write_VAR(N, Style, Ci, Co).
write_oout(N, _, _, Ci, alpha) :-
	integer(N),
	(   N < 0, maybe_space(Ci, other)
	;   maybe_space(Ci, alpha)
	),  !,
	name(N, String),
	put_string(String).
write_oout(TermS, Style, Prio, Ci, Co) :- string(TermS),
        term_to_atom(TermS,Term),!,write_oout(Term, Style, Prio, Ci, Co).
write_oout(Term, print, _,  _, alpha) :-
	% DMILES HSOULD BE portray/1
        loop_check(portray_or_print(Term),writeq(Term)),
        % print(Term),
	!.
write_oout(Atom, Style, Prio, _, punct) :-
	atom(Atom),
	current_op(P, _, Atom),
	P > Prio,
	!,
	put(40),
	(   Style = writeq, write_atom(Atom, Style, punct, _)
	;   (name(Atom, String), put_string(String))
	),  !,
	put(41).
write_oout(Atom, Style, _, Ci, Co) :-
	atom(Atom),
	!,
	write_atom(Atom, Style, Ci, Co).
write_oout(Term, display, _, Ci, punct) :- !,
	functor(Term, Fsymbol, Arity),
	write_atom(Fsymbol, display, Ci, _),
	write_args(0, Arity, Term, 40, display).
write_oout({Term}, Style, _, _, punct) :- !,
	put(123),
	write_oout(Term, Style, 1200, punct, _),
	put(125).
write_oout([Head|Tail], Style, _, _, punct) :- !,
	put(91),
	write_oout(Head, Style, 999, punct, _),
	write_tail(Tail, Style).
write_oout((A,B), Style, Prio, Ci, Co) :- !,
	%  This clause stops writeq quoting commas.
	maybe_paren(1000, Prio, 40, Ci, C1),
	write_oout(A, Style, 999, C1, _),
	put(44),
	write_oout(B, Style, 1000, punct, C2),
	maybe_paren(1000, Prio, 41, C2, Co).
write_oout(Term, Style, Prio, Ci, Co) :-
	functor(Term, F, N),
	write_oout(N, F, Term, Style, Prio, Ci, Co).



%% write_oout( ?ARG1, ?ARG2, ?ARG3, ?ARG4, ?ARG5, ?ARG6, ?ARG7) is det.
%
% Write Out.
%
write_oout(1, F, Term, Style, Prio, Ci, Co) :-
	(   current_op(O, fx, F), P is O-1
	;   current_op(O, fy, F), P = O
	),  !,
	maybe_paren(O, Prio, 40, Ci, C1),
	write_atom(F, Style, C1, C2),
	arg(1, Term, A),
	write_oout(A, Style, P, C2, C3),
	maybe_paren(O, Prio, 41, C3, Co).
write_oout(1, F, Term, Style, Prio, Ci, Co) :-
	(   current_op(O, xf, F), P is O-1
	;   current_op(O, yf, F), P = O
	),  !,
	maybe_paren(O, Prio, 40, Ci, C1),
	arg(1, Term, A),
	write_oout(A, Style, P, C1, C2),
	write_atom(F, Style, C2, C3),
	maybe_paren(O, Prio, 41, C3, Co).
write_oout(2, F, Term, Style, Prio, Ci, Co) :-
	(   current_op(O, xfy, F), P is O-1, Q = O
	;   current_op(O, xfx, F), P is O-1, Q = P
	;   current_op(O, yfx, F), Q is O-1, P = O
	),  !,
	maybe_paren(O, Prio, 40, Ci, C1),
	arg(1, Term, A),
	write_oout(A, Style, P, C1, C2),
	write_oper(F, O, Style, C2, C3),
	arg(2, Term, B),
	write_oout(B, Style, Q, C3, C4),
	maybe_paren(O, Prio, 41, C4, Co).
write_oout(N, F, Term, Style, _Prio, Ci, punct) :-
	write_atom(F, Style, Ci, _),
	write_args(0, N, Term, 40, Style).





%% write_oper( ?ARG1, ?ARG2, ?ARG3, ?ARG4, ?ARG5) is det.
%
% Write Oper.
%
write_oper(Op, Prio, Style, Ci, Co) :-
	Prio < 700, !,
	write_atom(Op, Style, Ci, Co).
write_oper(Op, _, Style, _Ci, punct) :-
	put(32),
	write_atom(Op, Style, punct, _),
	put(32).




%% write_VAR( ?ARG1, ?ARG2, ?ARG3, ?ARG4) is det.
%
% Write Var.
%
write_VAR(A, _Style, _Ci, _Co) :- atom(A), !,write(A).
write_VAR(N, writeq, _Ci, alpha):- writeq('$VAR'(N)),!.
write_VAR(X, Style, Ci, punct) :-
	write_atom('$VAR', Style, Ci, _),
	write_args(0, 1, '$VAR'(X), 40, Style).





%% write_atom( ?ARG1, ?ARG2, ?ARG3, ?ARG4) is det.
%
% Write Atom.
%
write_atom(('!'), _, _, punct) :- !,
	put(33).
write_atom((';'), _, _, punct) :- !,
	put(59).
write_atom([], _, _, punct) :- !,
	put(91), put(93).
write_atom({}, _, _, punct) :- !,
	put(123), put(125).
write_atom(A, write, _Ci, _Co):- !,write(A),!.
write_atom(A, _Style, _Ci, _Co):- write_atom_link(A,A),!.
write_atom(Atom, Style, Ci, Co) :-
	name(Atom, String),
	(   classify_name(String, Co),
	    maybe_space(Ci, Co),
	    put_string(String)
	;   Style = writeq, Co = quote,
	    maybe_space(Ci, Co),
	    (put(39), put_string(String, 39),put(39))
	;   Co = alpha,
	    put_string(String)
	),  !.



%txt_to_codes(Text,Codes):- text_to_string(Text,Str),name(Str,Codes).

%% put_string( ?ARG1) is det.
%
%   writes a list of character codes.
%
put_string(B):- txt_to_codes(B,C),put_string0(C).



%% put_string0( :TermARG1) is det.
%
% Put String Primary Helper.
%
put_string0([]).
put_string0([H|T]) :-
	put(H),
	put_string0(T).


%%   put_string(S, Q)
%   writes a quoted list of character codes, where the first
%   quote has already been written.  Instances of Q in S are doubled.
put_string(A,B):- is_html_mode,!,
  with_output_to(atom(S),put_string2(A,B)),
  url_iri(URL,S),bformat('<a href="?find=~w">~w</a>',[URL,S]).
put_string(A,B):- put_string2(A,B).


put_string2(A,B):- txt_to_codes(A,C),to_ascii_code(B,BC),put_string0(C,BC).
to_ascii_code(B,BC):- (number(B)->BC=B;name(B,[BC|_])).

% :-rtrace.



%% put_string0( :TermARG1, ?ARG2) is det.
%
% Put String Primary Helper.
%
put_string0([], _) :- !. % put(Q).
put_string0([Q|T], Q) :- !,
	put(Q), put(Q),
	put_string0(T, Q).
put_string0([H|T], Q) :-
	put(H),
	put_string0(T, Q).


%%   classify_name(String, Co)
%   says whether a String is an alphabetic identifier starting
%   with a lower case letter (Co=alpha) or a string of symbol characters
%   like ++/=? (Co=other).  If it is neither of these, it fails.  That
%   means that the name needs quoting.  The special atoms ! ; [] {} are
%   handled directly in write_atom.  In a basic Prolog system with no
%   way of changing the character classes this information can be
%   calculated when an atom is created, andf just looked up.  This has to
%   be as fast as you can make it.
classify_name([H|T], alpha) :-
	H >= 97, H =< 122,
	!,
	classify_alpha_tail(T).
classify_name([H|T], other) :-
	memberchk(H, "#$&=-~^\`@+*:<>./?"),
	classify_other_tail(T).




%% classify_alpha_tail( :TermARG1) is det.
%
% Classify Alpha Tail.
%
classify_alpha_tail([]).
classify_alpha_tail([H|T]) :-
	(  H >= 97, H =< 122
	;  H >= 65, H =< 90
	;  H >= 48, H =< 57
	;  H =:= 95
	), !,
	classify_alpha_tail(T).




%% classify_other_tail( :TermARG1) is det.
%
% Classify Other Tail.
%
classify_other_tail([]).
classify_other_tail([H|T]) :-
	memberchk(H, "#$&=-~^\`@+*:<>./?"),
	classify_other_tail(T).



%   write_args(DoneSoFar, Arity, Term, Separator, Style)
%   writes the remaining arguments of a Term with Arity arguments
%   all told in Style, given that DoneSoFar have already been written.
%   Separator is 0'( initially and later 0', .




%% write_args( ?ARG1, ?ARG2, ?ARG3, ?ARG4, ?ARG5) is det.
%
% Write Arguments.
%
write_args(N, N, _, _, _) :- !,
	put(41).
write_args(I, N, Term, C, Style) :-
	put(C),
	J is I+1,
	arg(J, Term, A),
	write_oout(A, Style, 999, punct, _),
	write_args(J, N, Term, 44, Style).



%   write_tail(Tail, Style)
%   writes the tail of a list of a given style.




%% write_tail( :TermARG1, ?ARG2) is det.
%
% Write Tail.
%
write_tail(Var, _) :-			%  |var]
	var(Var),
	!,
	put(124),
	write_variable(Var),
	put(93).
write_tail([], _) :- !,			%  ]
	put(93).
write_tail([Head|Tail], Style) :- !,	%  ,Head tail
	put(44),
	write_oout(Head, Style, 999, punct, _),
        
	write_tail(Tail, Style).
write_tail(Other, Style) :-		%  |junk]
	put(124),
	write_oout(Other, Style, 999, punct, _),
	put(93).


/*  The listing/0 and listing/1 commands are based on the Dec-10
    commands, but the bformat they generate is based on the "pp" command.
    The idea of rok_portray_clause/1 came from PDP-11 Prolog.

    BUG: the arguments of goals are not separated by comma-space but by
    just comma.  This should be fixed, but I haven't the time right not.
    Run the output through COMMA.EM if you really care.

    An irritating fact is that we can't guess reliably which clauses
    were grammar rules, so we can't print them out in grammar rule form.

    We need a proper pretty-printer that takes the line width into
    acount, but it really isn't all that feasible in Dec-10 Prolog.
    Perhaps we could use some ideas from NIL?
*/




%% portable_listing is det.
%
% Portable Listing.
%
portable_listing :-
	current_predicate(_, M:Pred),
        \+ predicate_property(M:Pred,imported_from(_)),
        predicate_property(M:Pred,number_of_clauses(_)),        
	nl,
	forall(clause(M:Pred, Body),rok_portray_clause((M:Pred:-Body))),
        fail.
portable_listing.


%   listing(PredSpecs)

%   Takes a predicate specifier F/N, a partial specifier F, or a
%   list of such things, and lists each current_predicate Pred
%   matching one of these specifications.




%% portable_listing( :TermARG1) is det.
%
% Portable Listing.
%
portable_listing(V) :-
	var(V), !.       % ignore variables
portable_listing([]) :- !.
portable_listing([X|Rest]) :- !,
	portable_listing(X),
	portable_listing(Rest).
portable_listing(X) :-
	'functor spec'(X, Name, Low, High),
	current_predicate(Name, Pred),
	functor(Pred, _, N),
	N >= Low, N =< High,
	nl, 
	clause(Pred, Body),
	rok_portray_clause((Pred:-Body)),
	fail.
portable_listing(_).




%% functor spec( ?ARG1, ?ARG2, :GoalARG3, :PRED255ARG4) is det.
%
% Functor Spec.
%
'functor spec'(Name/Low-High, Name, Low, High) :- !.
'functor spec'(Name/Arity, Name, Arity, Arity) :- !.
'functor spec'(Name, Name, 0, 255).




%% rok_portray_clause( :TermARG1) is det.
%
% Rok Portray Clause.
%

rok_portray_clause(Var):- var(Var),writeq(Var).

rok_portray_clause(I):- catch(make_pretty(I,O),_,I=O),block_format(rok_portray_clause1(O)).

rok_portray_clause1( :-(Command)) :- 
	(   Command = public(Body), Key = (public)
	;   Command = mode(Body),   Key = (mode)
	;   Command = type(Body),   Key = (type)
	;   Command = pred(Body),   Key = (pred)
	;   Command = Body,	    Key = ''
	),  !,
	nl,
	% nu mbervars(Body, 0, _),
	\+ \+ 'list clauses'(Body, Key, 2, 8),!.
rok_portray_clause1(M:(Pred:-Body)) :- !,
     must_run((
	% nu mbervars(Pred+Body, 0, _),
	\+ \+ portable_writeq(M:Pred),
	\+ \+ 'list clauses'(Body, 0, 2, 8))), !.
rok_portray_clause1((Pred:-Body)) :- !,
     must_run((
	% nu mbervars(Pred+Body, 0, _),
	\+ \+ portable_writeq(Pred),
	\+ \+ 'list clauses'(Body, 0, 2, 8))), !.
rok_portray_clause1(M:(Pred)) :- 
	call(call,rok_portray_clause1((M:Pred:-true))).
rok_portray_clause1((Pred)) :- !,
	call(call,rok_portray_clause1((Pred:-true))).


%% list clauses( :TermARG1, ?ARG2, ?ARG3, ?ARG4) is det.
%
% List Clauses.
%
'list clauses'((A,B), L, R, D) :- !,
	'list clauses'(A, L, 1, D), !,
	'list clauses'(B, 1, R, D).
'list clauses'(true, _L, 2, _D) :- !,
	put(0'.
        ), nl.
        
'list clauses'((A;B), L, R, D) :- !,
	'list magic'(fail, L, D),
	'list magic'((A;B), 0, 2, D),
	'list magic'(R, '.
'
).

'list clauses'((A->B), L, R, D) :- !,
	'list clauses'(A, L, 5, D), !,
	'list clauses'(B, 5, R, D).
'list clauses'(Goal, L, R, D) :-
	'list magic'(Goal, L, D),
	portable_writeq(Goal),
	'list magic'(R, '.
'
).




%% list magic( ?ARG1, :PRED5ARG2, ?ARG3) is det.
%
% List Magic.
%
'list magic'(!,    0, _D) :- !,
	write(' :- ').
'list magic'(!,    1, _D) :- !,
	write(',  ').
'list magic'(_Goal, 0, D) :- !,
	write(' :- '),
	nl, tab(D).
'list magic'(_Goal, 1, D) :- !,
	put(0',
        ),
	nl, tab(D).
'list magic'(_Goal, 3, _D) :- !,
	write('(   ').
'list magic'(_Goal, 4, _D) :- !,
	write(';   ').
'list magic'(_Goal, 5, D) :- !,
	write(' ->'),
	nl, tab(D).
'list magic'(_Goal, Key, D) :-
	atom(Key),
	write(':- '), write(Key),
	nl, tab(D).





%% list magic( ?ARG1, ?ARG2) is det.
%
% List Magic.
%
'list magic'(2, C) :- !, write(C).
'list magic'(_, _).





%% list magic( ?ARG1, ?ARG2, ?ARG3, ?ARG4) is det.
%
% List Magic.
%
'list magic'((A;B), L, R, D) :- !,
	'list magic'(A, L, 1, D), !,
	'list magic'(B, 1, R, D).
'list magic'(Conj,  L, R, D) :-
	E is D+8,
	M is L+3,
	'list clauses'(Conj, M, 1, E),
	nl, tab(D),
	'list magic'(R, ')').


/*	Test code for rok_portray_clause.
	If it works, test_portray_clause(File) should write out the
	contents of File in a more or less readable fashion.

test_portray_clause(File) :-
	see(File),
	repeat,
	    read(Clause, Vars),
	    (   Clause = end_of_file
	    ;   test_bind(Vars), rok_portray_clause(Clause), fail
	    ),
	!,
	seen.

test_bind([]) :- !.
test_bind([X='$VAR'(X)|L]) :-
	test_bind(L).
:- public test_portray_clause/1.
*/









% '$messages':baseKB:my_portray(X):-fail,loop_check(baseKB:my_portray(X)).
% user:portray(X):-loop_check(baseKB:my_portray(X)).
/*
:- dynamic user:portray/1.
:- multifile user:portray/1.
:- discontiguous my_portray/1. 
:- export(baseKB:my_portray/1).
baseKB:my_portray(A) :- var(A),!,fail,writeq(A).
baseKB:my_portray(A) :-
        atom(A),
        sub_atom(A, 0, _, _, 'http://'), !,
        (   style(B)
        ->  true
        ;   B=prefix:id
        ),
        portray_url(B, A).
baseKB:my_portray(A) :-
        atom(A),
        atom_concat('__file://', B, A),
        sub_atom(B, D, _, C, #),
        sub_atom(B, _, C, 0, G),
        sub_atom(B, 0, D, _, E),
        file_base_name(E, F),
        bformat('__~w#~w', [F, G]).
baseKB:my_portray(A) :- atom(A),!,baseKB:write_atom_link(A,A).
baseKB:my_portray(A) :- \+compound(A),fail.
%baseKB:my_portray(P):- must_run((return_to_pos(rok_portray_clause(P)),!)).
*/




is_html_mode:- \+ get_print_mode(text).


%% sanity_test_000 is det.
%
% Optional Sanity Checking test  Primary Helper Primary Helper Primary Helper.
%
sanity_test_000:- find_and_call((rok_portray_clause((
pkif :-

        [ implies,

          [ isa(F, tPred),
            isa(A, ftInt),
            poss(KB, pos([arity(F, A)])),
            poss(KB, arity(F, A))
          ],
          =>,

          [ all([F]),

            [ implies,
              [isa(F, tPred), ex([A]), isa(A, ftInt), poss(KB, arity(F, A))],
              =>,
              [ex([A]), [isa(A, ftInt), arity(F, A)]]
            ]
          ]
        ])))),nl,nl,nl.



x123:- locally_tl(print_mode(html),xlisting_inner(i2tml_hbr,end_of_file,[])).


%% param_matches( ?ARG1, ?ARG2) is det.
%
% Param Matches.
%
param_matches(A,B):-A=B,!.
param_matches(VV,V):-atomic(VV),atomic(V),string_to_atom(VV,VVA),string_to_atom(V,VA),downcase_atom(VVA,VD),downcase_atom(VA,VD).
param_matches(A,B):-A=B,!.




%% show_select2( ?ARG1, ?ARG2, ?ARG3) is det.
%
% Show Select Extended Helper.
%
show_select2(Name,Pred,Options):- block_format(show_select22(Name,Pred,Options)).
show_select22(Name,Pred,Options):-  
    Call=..[Pred,ID,Value],
    must_run(param_default_value(Name,D); param_default_value(Pred,D)),!,
    get_param_sess(Name,UValue,D),
    format('<select name="~w">',[Name]),
    forall(no_repeats(Call),
       (((member(atom_subst(Item,ItemName),Options) -> (any_to_string(Value,ValueS),atom_subst(ValueS,Item,ItemName,NValue)); NValue=Value),
        (((param_matches(UValue,ID);param_matches(UValue,NValue)) -> format('<option value="~w" selected="yes">~w</option>',[ID,NValue]);
                   format('<option value="~w">~w</option>',[ID,Value])))))),
    format('</select>',[]),!.



%% show_select1( ?ARG1, ?ARG2) is det.
%
% Show Select Secondary Helper.
%
show_select1(Name,Pred):- block_format(show_select11(Name,Pred)).
show_select11(Name,Pred):-
 Call=..[Pred,Value],
 ( param_default_value(Name,D); param_default_value(Pred,D)),!,
 format('<select name="~w">',[Name]),
 forall(Call,
    (get_param_sess(Name,Value,D)->format('<option value="~w" selected="yes">~w</option>',[Value,Value]);
                format('<option value="~w">~w</option>',[Value,Value]))),
 format('</select>',[]),!.





%% as_ftVars( :TermARG1) is det.
%
% Converted To Format Type Variables.
%
as_ftVars(N='$VAR'(N)):-atomic(N),!.
as_ftVars(_N=_V).
as_ftVars(_).

% :- ensure_loaded(library(logicmoo/util/logicmoo_util_varnames)).

% :- use_listing_vars.



%% search4term is det.
%
% Search4term.
%
search4term:- must_run((
  maybe_scan_for_varnames,
  get_param_sess(term,Term,"tHumanHead"),
  get_param_sess(find,SObj,Term),
  cvt_param_to_term(SObj,Obj),
  call_for_terms(make_page_pretext_obj(Obj)))),!.




%% edit1term is det.
%
% Edit1term.
%

edit1term:- get_param_req(xref,'Overlap'),!,search4term.

edit1term:-  
  get_param_req('ASK','ASK'),!,
  www_main_error_to_out(
   must_run((
   get_param_sess(term,String,""),
   cvt_param_to_term(String,Term,VNs),
   save_in_session(find,Term),
   % call_for_terms
   edit1term(forall(Term,pp_item_html('Answer',':-'(VNs,Term))))))),!.
  
edit1term:- 
  get_param_req('TELL','TELL'),!,
  www_main_error_to_out(
   must_run((
   get_param_sess(term,String,""),
   cvt_param_to_term(String,Term,VNs),
   save_in_session(find,Term),
   maplist(as_ftVars,VNs),
   call_for_terms(forall(ain(Term),pp_item_html('Assert',':-'(VNs,Term))))))),!.
  
edit1term:- 
  get_param_req('RETRACT','RETRACT'),!,
  www_main_error_to_out(
   must_run((
   get_param_sess(term,String,""),
   cvt_param_to_term(String,Term,VNs),
   save_in_session(find,Term),
   maplist(as_ftVars,VNs),
   call_for_terms(forall(mpred_withdraw(Term),pp_item_html('Retract',':-'(VNs,Term))))))),!.
  
edit1term:- 
 must_run((
             reset_assertion_display,
             get_param_sess(term,String,""),get_param_sess(find,Word,""),term_to_pretty_string(Word,SWord),
                save_in_session(find,Word),
   show_edit_term(true,String,SWord))),!,
 show_iframe(search4term,find,SWord).




%% edit1term( :GoalARG1) is det.
%
% Edit1term.
%
edit1term(Call):-
 must_run((
             reset_assertion_display,
             get_param_sess(term,String,""),get_param_sess(find,Word,""),term_to_pretty_string(Word,SWord),save_in_session(find,Word),
   show_edit_term(Call,String,SWord))),!.





%% show_edit_term( :GoalARG1, ?ARG2, ?ARG3) is det.
%
% Show Edit Term.
%
show_edit_term(Call,String,_SWord):- cvt_param_to_term(String,T),compound(T),T=(H:-_),!,show_edit_term0(Call,String,H).
show_edit_term(Call,String,SWord):- show_edit_term0(Call,String,SWord),!.




%% show_edit_term0( :GoalARG1, ?ARG2, ?ARG3) is det.
%
% Show Edit Term Primary Helper.
%
show_edit_term0(Call,String,SWord):-atomic(SWord),cvt_param_to_term(SWord,T),nonvar(T),!,show_edit_term1(Call,String,T).
show_edit_term0(Call,String,SWord):-show_edit_term1(Call,String,SWord).


ensure_guitracer_x:-!.
ensure_guitracer_x:- break,
 absolute_file_name(swi(xpce/prolog/lib),X), assert_if_new(user:library_directory(X)), 
 user:use_module(library(pce_prolog_xref)),
 user:use_module(library(emacs_extend)),
 user:use_module(library(trace/gui)),
 user:use_module(library(pce)),
 user:use_module(library(gui_tracer)),
 reload_library_index.


%% do_guitracer is det.
%
% Do Guitracer.
%
do_guitracer:- ensure_guitracer_x, guitracer,dtrace.

output_telnet_console(Port):- HttpPort is Port +100,
  sformat(HTML,'<iframe id="port~w" src="http://logicmoo.org:~w/" height="600" width="100%">loading...</iframe>',[HttpPort,HttpPort]),
  write_html(HTML).
output_telnet_console2(Port):- HttpPort is Port +100,
  sformat(HTML,'<iframe id="port~w" src="http://logicmoo.org:~w/" height="80%" width="100%">loading...</iframe>',[HttpPort,HttpPort]),
  write_html(HTML).


output_html(Var):- var(Var),!,term_to_atom(Var,Atom),output_html(pre([Atom])).
%output_html(html(HTML)):- !,output_html(HTML). %output_html(HTML):- atomic(HTML),!,write_html(HTML). %output_html(HTML):- is_list(HTML),send_tokens(HTML).
output_html(HTML):- phrase(html(HTML), Tokens,[]),!,send_tokens(Tokens).

remove_if_last(Tokens,TokensRight,TokensLeft):-append(TokensLeft,TokensRight,Tokens),!.
remove_if_last(TokensRightLeft,_,TokensRightLeft).

send_tokens(['<',html,'>'|Tokens]):-!,remove_if_last(Tokens,['</',html,'>'],TokensLeft),send_tokens_1(TokensLeft).
send_tokens(Tokens):- send_tokens_1(Tokens).
send_tokens_1([nl(1)|Tokens]):-!,remove_if_last(Tokens,[nl(1)],TokensLeft),send_tokens(TokensLeft).
send_tokens_1(Tokens):- with_output_to(string(HTMLString), html_write:print_html(Tokens)),write_html(HTMLString).

%write_html(HTMLString):- ((pengines:pengine_self(_) -> pengines:pengine_output(HTMLString) ;write(HTMLString))),!.
write_html(HTMLString):- (nb_current('$in_swish',t) -> pengines:pengine_output(HTMLString) ; bformat(HTMLString)).

%write_html(HTML):- phrase(html(HTML), Tokens), html_write:print_html(Out, Tokens))).
% output_html(html([div([id('cp-menu'), class(menu)], cp_skin: cp_logo_and_menu)]))
show_map_legend :- write_html(
'<table border=0 cellpadding=5 bgcolor="#000000"><tr><td>
<pre><div style="background-color:#000000;float:left"><code><font size=2 face="Courier New, FixedSys, Lucida Console, Courier New, Courier"><font color="#0">
</font><font color="#C0C0C0">The map key is:

        </font><font color="#FF00FF">#</font><font color="#C0C0C0">  - You                         --- - North/south wall
        </font><font color="#FF0000">*</font><font color="#C0C0C0">  - Other players                |  - East/west wall
        </font><font color="#FFFF00">!</font><font color="#C0C0C0">  - Mobiles                      +  - Door (closed)
        </font><font color="#00FFFF">!</font><font color="#C0C0C0">  - Pet/other charmed mob        </font><font color="#0000FF">+</font><font color="#C0C0C0">  - Door (locked)
        </font><font color="#FF0000">!</font><font color="#C0C0C0">  - Angry mob (with Sense        &gt;  - Up exit
             Anger cast)                  </font><font color="#808000">&gt;</font><font color="#C0C0C0">  - Up exit (closed)
        </font><font color="#00FF00">!</font><font color="#C0C0C0">  - Unkillable Mob               &lt;  - Down exit
        </font><font color="#00FF00">$</font><font color="#C0C0C0">  - Shopkeeper                   </font><font color="#808000">&lt;</font><font color="#C0C0C0">  - Down exit (closed)
       </font><font color="#00FFFF">[</font><font color="#FFFFFF">?</font><font color="#00FFFF">]</font><font color="#C0C0C0"> - Area exit                    </font><font color="#800000">#</font><font color="#C0C0C0">  - PK-flagged room             
       </font><font color="#00FF00">[</font><font color="#FFFFFF">?</font><font color="#00FF00">]</font><font color="#C0C0C0"> - Clan public hall exit        </font><font color="#FF0000">D</font><font color="#C0C0C0">  - Donation room

Other characters on the map represent the terrain of the local area. Some 
of the major terrains are:

        [</font><font color="#FF00FF"> </font><font color="#C0C0C0">]   Inside             .</font><font color="#FF00FF"> </font><font color="#C0C0C0">.   City
        </font><font color="#008000">,</font><font color="#FF00FF"> </font><font color="#008000">`</font><font color="#C0C0C0">   Field              </font><font color="#00FF00">;</font><font color="#FF00FF"> </font><font color="#00FF00">;</font><font color="#C0C0C0">   Hills
        </font><font color="#808000">/</font><font color="#FF00FF"> </font><font color="#808000">\\</font><font color="#C0C0C0">   Mountain           </font><font color="#0000FF">~</font><font color="#FF00FF"> </font><font color="#0000FF">~</font><font color="#C0C0C0">   Water
        </font><font color="#0000FF">~</font><font color="#FF00FF"> </font><font color="#0000FF">~</font><font color="#C0C0C0">   Waternoswim        </font><font color="#008080">.</font><font color="#FF00FF"> </font><font color="#008080">.</font><font color="#C0C0C0">   Air
        </font><font color="#808000">~</font><font color="#FF00FF"> </font><font color="#808000">~</font><font color="#C0C0C0">   Desert             </font><font color="#FFFF00">%</font><font color="#FF00FF"> </font><font color="#FFFF00">%</font><font color="#C0C0C0">   Quicksand
        </font><font color="#000080">~</font><font color="#FF00FF"> </font><font color="#000080">~</font><font color="#C0C0C0">   Underwater         </font><font color="#00FFFF">~</font><font color="#FF00FF"> </font><font color="#00FFFF">~</font><font color="#C0C0C0">   Ice
        </font><font color="#0000FF">.</font><font color="#FF00FF"> </font><font color="#0000FF">.</font><font color="#C0C0C0">   Underground        -</font><font color="#FF00FF"> </font><font color="#C0C0C0">-   East/West road
        . .   North/South road   </font><font color="#00FFFF">~ ~</font><font color="#C0C0C0">   River
        </font><font color="#FF0000">/</font><font color="#FF00FF"> </font><font color="#FF0000">\\</font><font color="#C0C0C0">   Volcano            </font><font color="#000080">%</font><font color="#FF00FF"> </font><font color="#000080">%</font><font color="#C0C0C0">   Cave
        # #   Dungeon            </font><font color="#008000">( *</font><font color="#C0C0C0">   Forest

Other terrain types not listed here are for aesthetic purposes only, such
as </font><font color="#008080">[ ]</font><font color="#C0C0C0"> for temples, </font><font color="#FFFF00">* *</font><font color="#C0C0C0"> for shops, etc.
</font></font></code></div></pre></td></tr></table>'),!.


%% show_edit_term1( :GoalARG1, ?ARG2, ?ARG3) is det.
%
% Show Edit Term Secondary Helper.
%
show_edit_term1(Call,String,'=>'(P,Q)):-!,show_edit_term1(Call,String,(P;Q;'=>'(P,Q))),!.
show_edit_term1(Call,String,SWord):- 
 write_begin_html('edit1term',_BASE,URL),!,
   bformat('<br/><p>
<table width="1111" cellspacing="0" cellpadding="0" height="121" id="table4">
 <!-- MSTableType="nolayout" -->
	<form action="edit1term">
      <!-- MSTableType="nolayout" -->
		<tr>
          <td align="left" valign="top" width="36" rowspan="2"><img src="/pixmapx/sigmaSymbol-gray.gif"></td>
          <td></td>
          <td align="left" valign="top" width="711" rowspan="2">
          <img src="/pixmapx/logoText-gray.gif">&nbsp;&nbsp;Prover:&nbsp; ~@
                   <table cellspacing="0" cellpadding="0" id="table5" width="658" height="97">
      <!-- MSTableType="nolayout" -->
	<tr>
          <td align="right"><b>Fml:</b></td>
          <td align="left" valign="top" colspan="2">
              <textarea style="white-space: pre; overflow: auto; font-size: 7pt; font-weight: bold; font-family: Verdana, Arial, Helvetica, sans-serif;border: 1px solid black;"
               wrap="off" rows="10" cols="70" name="term">~w</textarea>
          </td>
          <td align="left" valign="top" height="68">~@
             <br><b>Microthory</b><br>~@<br/><input type="submit" value="ASK" name="ASK"><input type="submit" value="TELL" name="TELL"><input type="submit" value="RETRACT" name="RETRACT">
             <br><b>Formal Language</b><br>~@</td>
      </tr>
        <tr><td><img src="/pixmapx/1pixel.gif" height="3"></td>
      		<td></td>
			<td></td>
			<td height="3"></td>
            </tr>
            <tr>
                  <td align="right" width="99"><b>Search:&nbsp;</b></td>
                  <td align="left" valign="top" width="276"><input type="text" size="27" name="find" value="~w">&nbsp;<input type="submit" value="Overlap" name="xref">&nbsp;</td>
                  <td align="left" valign="top" width="144">~@&nbsp;<input type="submit" value="NatLg" name="ShowEnglish"></td>
                  <td align="left" valign="top" height="26" width="139">~@</td>
             </tr>
            </table>
          </td>
          <td valign="bottom" width="9" rowspan="2"></td>
          <td height="121" rowspan="2" width="163">
          <span class="navlinks">
          <b>[&nbsp;<a href="/">Home</a>&nbsp;|&nbsp;              
          <a href="~w&Graph=true">Grap2h</a>]</b></span><p>
          <b>Response&nbsp;Language&nbsp;<br></b>~@<p>
                        <input type="checkbox" name="sExprs" value="1" checked>S-Exprs&nbsp;
                        <input type="checkbox" name="webDebug" value="1" checked>Debugging
                        </td>
          <td height="121" rowspan="2" width="188"></td>
      </tr>
		<tr>
			<td width="4">&nbsp;</td>
		</tr>
  </form></table><hr>'
  ,[show_select2(prover,prover_name,[]),
    String,
    action_menu_applied('action_above',"Item",""),
    show_select2('context',is_context,[]),
    show_select2(flang,logic_lang_name,[]),
    SWord,
    %show_select2('POS',partOfSpeech,[]),
    show_select1('humanLang',human_language),
    URL,
    show_select2(olang,logic_lang_name,[])]),!,   
    bformat('<pre>',[]),
    on_x_debug(Call),!,
    bformat('</pre>',[]),
   write_end_html,!.




%% show_iframe( ?ARG1, ?ARG2, ?ARG3) is det.
%
% Show Iframe.
%
show_iframe(URL,Name,Value):- bformat('<iframe width="100%" height="800" frameborder="0" scrolling="yes" marginheight="0" marginwidth="0" allowtransparency=true id="main" name="main" style="width:100%;height:800" src="~w?~w= ~w"></iframe>',[URL,Name,Value]).



%% show_iframe( ?ARG1) is det.
%
% Show Iframe.
%
show_iframe(URL):- bformat('<iframe width="100%" height="800" frameborder="0" scrolling="yes" marginheight="0" marginwidth="0" allowtransparency=true id="main" name="main" style="width:100%;height:800" src="search4term?find= ~w"></iframe>',[URL]).
  



%% show_search_filtersTop( ?ARG1) is det.
%
% Show Search Filters Top.
%
show_search_filtersTop(BR):- write(BR).




%% show_search_filters( ?ARG1) is det.
%
% Show Search Filters.
%
show_search_filters(BR):- 
   forall(no_repeats(N=C,search_filter_name_comment(N,C,_)),session_checkbox(N,C,BR)).




%% parameter_names( ?ARG1, ?ARG2) is det.
%
% Parameter Names.
%
parameter_names(List,N):-is_list(List),!,member(E,List),parameter_names(E,N).
parameter_names(V,_):- var(V),!,fail.
parameter_names(N=_,N):-!,atom(N).
parameter_names(C,N):-compound(C),functor(C,N,1).




%% current_form_var( ?ARG1) is det.
%
% Current Form Variable.
%
current_form_var(N):-no_repeats((current_form_var0(N))),atom(N),\+ arg(_,v(peer,idle,ip,session),N).



%% current_form_var0( ?ARG1) is det.
%
% Current Form Variable Primary Helper.
%
current_form_var0(N):- param_default_value(N,_).
%current_form_var0(N):- get_http_current_request(B),member(search(Parameters),B),parameter_names(Parameters,N).
%current_form_var0(N):- http_current_session(_, Parameters),parameter_names(Parameters,N).




%% is_goog_bot is det.
%
% If Is A Goog Bot.
%
is_goog_bot:- get_http_current_request(B),member(user_agent(UA),B),!,atom_contains(UA,'Googlebot').
 

%% pp_now is det.
%
% Pretty Print Now.
%
pp_now.




%% this_listing( :TermARG1) is det.
%
% This Listing.
%
this_listing(M:F/A):-functor(H,F,A),predicate_property(M:H,number_of_causes(_)),!, forall(clause(M:H,Body),pp_i2tml((M:H :- Body))).
this_listing(M:F/A):-functor(H,F,A),predicate_property(H,number_of_causes(_)),!, forall(clause(H,Body),pp_i2tml((M:H :- Body))).
this_listing(M:F/A):-listing(M:F/A),!.
this_listing(MFA):-listing(MFA).

:- thread_local(sortme_buffer/2).


% i2tml_save(Obj,H):- \+ is_list(H),cyc:pterm_to_sterm(H,S),H\=@=S,!,i2tml_save(Obj,S).




%% pp_i2tml_saved_done( ?ARG1) is det.
%
% Pretty Print I2tml Saved Done.
%
pp_i2tml_saved_done(_Obj):-pp_now,!,flush_output_safe.
pp_i2tml_saved_done(Obj):-
  findall(H,retract(sortme_buffer(Obj,H)),List),predsort(head_functor_sort,List,Set),
  forall(member(S,Set),pp_i2tml(S)),!.




%% find_cl_ref( :TermARG1, ?ARG2) is det.
%
% Find Clause Ref.
%
find_cl_ref(_,none):- t_l:tl_hide_data(hideClauseInfo),!.
find_cl_ref(clause(_,_,Ref),Ref):-!.
find_cl_ref(clause(H,B),Ref):- clause(H,B,Ref),!.
find_cl_ref((H:-B),Ref):-!, clause(H,B,Ref),clause(HH,BB,Ref),H=@=HH,B=@=BB,!.
find_cl_ref(H,Ref):- clause(H,true,Ref),clause(HH,true,Ref),H=@=HH,!.




%% find_ref( :TermARG1, ?ARG2) is det.
%
% Find Ref.
%
find_ref(_,none):- t_l:tl_hide_data(hideClauseInfo),!.
find_ref(H,Ref):- find_cl_ref(H,Ref),!.
find_ref(This,Ref):- call(call,'$si$':'$was_imported_kb_content$'(A,CALL)),
   arg(1,CALL,This),clause('$si$':'$was_imported_kb_content$'(A,CALL),true,Ref),!.
find_ref(M:This,Ref):- atom(M),!,find_ref(This,Ref).




%% head_functor_sort( ?ARG1, ?ARG2, ?ARG3) is det.
%
% Head Functor Sort.
%
head_functor_sort(Result,H1,H2):- (var(H1);var(H2)),compare(Result,H1,H2),!.
head_functor_sort(Result,H1,H2):- once((get_functor(H1,F1,A1),get_functor(H2,F2,A2))),F1==F2,A1>0,A2>0,arg(1,H1,E1),arg(1,H2,E2),compare(Result,E1,E2),Result \== (=),!.
head_functor_sort(Result,H1,H2):- once((get_functor(H1,F1,_),get_functor(H2,F2,_))),F1\==F2,compare(Result,F1,F2),Result \== (=),!.
head_functor_sort(Result,H1,H2):-compare(Result,H1,H2),!.




%% i2tml_hbr( ?ARG1, ?ARG2, ?ARG3) is det.
%
% I2tml Hbr.
%
i2tml_hbr(H,B,Ref):- nonvar(Ref),!,pp_i2tml_save_seen(clause(H,B,Ref)).
i2tml_hbr(H,B,_):- B==true,!, pp_i2tml_save_seen(H).
i2tml_hbr(H,B,_):- pp_i2tml_save_seen((H:-B)).




%% pp_i2tml_save_seen( ?ARG1) is det.
%
% Pretty Print I2tml Save Seen.
%
pp_i2tml_save_seen(HB):- pp_now, !,must_run(pp_i2tml(HB)),!.
pp_i2tml_save_seen(HB):- assertz_if_new(sortme_buffer(_Obj,HB)),!.


:- thread_local(t_l:pp_i2tml_hook/1).

:- thread_local(t_l:tl_hide_data/1).
   
:- thread_local(shown_subtype/1).
:- thread_local(xlw:shown_clause/1).
:- meta_predicate if_html(*,0).






%% section_open( ?ARG1) is det.
%
% Section Open.
%
section_open(Type):-  once(shown_subtype(Type)->true;((is_html_mode->bformat('~n</pre><hr>~w<hr><pre>~n<font face="verdana,arial,sans-serif">',[Type]);(draw_line,format('% ~w~n~n',[Type]))),asserta(shown_subtype(Type)))),!.



%% section_close( ?ARG1) is det.
%
% Section Close.
%
section_close(Type):- shown_subtype(Type)->(retractall(shown_subtype(Type)),(is_html_mode->bformat('</font>\n</pre><hr/><pre>',[]);draw_line));true.

:- export((action_menu_applied/3,
            %xaction_menu_item/2,
            add_form_script/0,
            register_logicmoo_browser/0,
            as_ftVars/1,
            call_for_terms/1,
            classify_alpha_tail/1,
            classify_name/2,
            classify_other_tail/1,
            current_form_var/1,
            current_line_position/1,
            current_line_position/2,
            cvt_param_to_term/2,
            cvt_param_to_term/3,
            do_guitracer/0,
            edit1term/0,
            edit1term/1,
            ensure_sigma/1,
            %get_print_mode/1,               
            ensure_sigma/0,
            find_cl_ref/2,
            find_ref/2,
            fmtimg/2,
            'functor spec'/4,
            functor_to_color/2,
            functor_to_color/4,
            
            get_http_current_request/1,
            get_http_session/1,
            get_nv_session/3,
            get_param_req/2,
            get_param_sess/2,
            get_param_sess/3,
            get_request_vars/1,
            handler_logicmoo_cyclone/1,
            head_functor_sort/3,
            must_run/1,
            human_language/1,
            i2tml_hbr/3,
            if_html/2,
            indent_nbsp/1,
            indent_nbsp/2,
            indent_nl/0,
            is_cgi_stream/0,
            is_context/2,
            is_goog_bot/0,
            'list clauses'/4,
            'list magic'/2,
            'list magic'/3,
            'list magic'/4,
            logic_lang_name/2,
            make_page_pretext_obj/1,
            make_quotable/2,
            make_session/1,
            maybe_paren/5,
            maybe_space/2,
            member_open/2,
            merge_key_vals/3,
            name_the_var/5,
            nl_same_pos/0,
            numberlist_at/2,
            object_sub_page/4,
            % param_default_value/2,
            param_matches/2,
            parameter_names/2,
            %partOfSpeech/2,
            portable_display/1,
            portable_listing/0,
            portable_listing/1,
            portable_print/1,
            portable_write/1,
            portable_writeq/1,
            pp_i2tml/1,
            pp_i2tml_now/1,
            pp_i2tml_save_seen/1,
            pp_i2tml_saved_done/1,
            pp_i2tml_v/1,
            pp_item_html/2,
            pp_item_html_if_in_range/2,
            pp_item_html_now/2,
            pp_now/0,
            print_request/1,
            prover_name/2,
            put_string/1,
            put_string/2,
            reply_object_sub_page/1,
            reset_assertion_display/0,
            return_to_pos/1,
            rok_portray_clause/1,
            save_in_session/1,
            save_in_session/2,
            save_in_session/3,
            save_request_in_session/1,
            search4term/0,
            search_filter_name_comment/3,
            section_close/1,
            section_open/1,
            sensical_nonvar/1,
            session_checkbox/3,
            session_checked/1,
            set_line_pos/1,
            set_line_pos/2,
            show_clause_ref/1,
            show_clause_ref_now/1,
            show_edit_term/3,
               show_http_session/0,
            show_iframe/1,
            show_iframe/3,
            show_pcall_footer/0,
            show_search_filters/1,
            show_search_filtersTop/1,
            term_to_pretty_string/2,
            this_listing/1,
            test_tmw/0,
            tovl/3,
            url_decode/2,
            url_decode_term/2,
            url_encode/2,
            url_encode_term/3,
            with_search_filters/1,
            with_search_filters0/1,
            write_VAR/4,
            write_args/5,
            write_as_url_encoded/2,
            write_atom/4,
            write_atom_link/1,
            write_atom_link/2,
            write_atom_link/3,
            write_begin_html/3,
            write_end_html/0,
            write_oper/5,
            write_out/5,
            write_oout/7,
            write_tail/2,
            write_term_to_atom_one/2,
            write_variable/1,
          
          xlisting_web_file/0)).


%% pp_item_html( ?ARG1, ?ARG2) is det.
%
% Pretty Print Item HTML.
%
pp_item_html(_Type,H):-var(H),!.
pp_item_html(Type,done):-!,section_close(Type),!.
pp_item_html(_,H):- xlw:shown_clause(H),!.
pp_item_html(_,P):- is_hidden_pred(P),!.
pp_item_html(Type,H):- \+ is_html_mode, pp_item_html_now(Type,H),!.
pp_item_html(Type,H):- ignore((flag(matched_assertions,X,X),between(0,5000,X),pp_item_html_now(Type,H))).

is_hidden_pred(M:P):-!, (is_listing_hidden(M); is_hidden_pred(P)).
is_hidden_pred(P):- (is_listing_hidden(P); (compound(P),functor(P,F,A),(is_listing_hidden((F/A));is_listing_hidden((F))))),!.





%% pp_item_html_now( ?ARG1, ?ARG2) is det.
%
% Pretty Print Item HTML Now.
%
pp_item_html_now(Type,H):-    
   flag(matched_assertions,X,X+1),!,
   pp_item_html_if_in_range(Type,H),!,
   assert(xlw:shown_clause(H)),!.





%% pp_item_html_if_in_range( ?ARG1, ?ARG2) is det.
%
% Pretty Print Item HTML If In Range.
%
pp_item_html_if_in_range(Type,H):- section_open(Type),!,pp_i2tml(H),!.

:- thread_local(t_l:last_show_clause_ref/1).
:- thread_local(t_l:current_clause_ref/1).





%% show_clause_ref( ?ARG1) is det.
%
% Show Clause Ref.
%
show_clause_ref(Ref):- Ref == none,!.
show_clause_ref(Ref):- t_l:last_show_clause_ref(Ref),!.
show_clause_ref(Ref):- retractall(t_l:last_show_clause_ref(_)),asserta(t_l:last_show_clause_ref(Ref)),on_x_debug(show_clause_ref_now(Ref)),!.




%% show_clause_ref_now( :GoalARG1) is det.
%
% Show Clause Ref Now.
%
show_clause_ref_now(_Ref):- is_listing_hidden(hideClauseRef),!.
show_clause_ref_now(V):-var(V),!.
show_clause_ref_now(0):-!.
show_clause_ref_now(none):-!.
show_clause_ref_now(Ref):- is_listing_hidden(showFilenames), \+ clause_property(Ref,predicate(_)),format('~N~p~N',[clref(Ref)]),!.
% write_html(div(class(src_formats),a(href(EditLink), edit)])).
show_clause_ref_now(Ref):- is_listing_hidden(showFilenames),clause_property(Ref,file(File)),ignore(clause_property(Ref,line_count(Line))),
  ignore(clause_property(Ref,module(Module))),
    bformat('<a href="/swish/filesystem/~w#L~w">@file:~w:~w</a>(~w)~N',[File,Line,File,Line,Module]),
    fail. 
show_clause_ref_now(Ref):- clause_property(Ref,erased),
  ignore(clause_property(Ref,module(Module))),
    bformat('erased(~w) (~w)~N',[Ref,Module]),!.




%% pp_i2tml( :TermARG1) is det.
%
% Pretty Print I2tml.
%
pp_i2tml(Done):-Done==done,!.
pp_i2tml(T):-var(T),!,format('~w~n',[T]),!.
pp_i2tml(T):-string(T),!,format('"~w"~n',[T]).
pp_i2tml(clause(H,B,Ref)):- !, locally_tl(current_clause_ref(Ref),pp_i2tml_v((H:-B))).
pp_i2tml(HB):- find_ref(HB,Ref),!, must_run(locally_tl(current_clause_ref(Ref),pp_i2tml_v((HB)))).
pp_i2tml(HB):- locally_tl(current_clause_ref(none),must_run(pp_i2tml_v((HB)))).




%% numberlist_at( ?ARG1, :TermARG2) is det.
%
% Numberlist When.
%
numberlist_at(_,[]).
numberlist_at(_,[N|More]):- number(N),!,N2 is N+1,numberlist_at(N2,More),!.
numberlist_at(Was,[N|More]):-var(N),  N is Was+1, N2 is N+1,  numberlist_at(N2,More),!.
numberlist_at(Was,[_|More]):- N2 is Was+2, numberlist_at(N2,More),!.




%get_clause_vars_for_print_here(HB,HB2):- catch(get_clause_vars_for_print(HB,HB2),_,fail),!.
get_clause_vars_for_print_here(HB,HB2):- make_pretty(HB,HB2),!.

%% pp_i2tml_v( ?ARG1) is det.
%
% Pretty Print I2tml V.
%
pp_i2tml_v(HB):- ignore(catch(( \+ \+ ((get_clause_vars_for_print_here(HB,HB2),pp_i2tml_0(HB2)))),_,true)),!.




%% pp_i2tml_0( :TermARG1) is det.
%
% Pretty Print i2tml  Primary Helper.
%
pp_i2tml_0(Var):-var(Var),!.
pp_i2tml_0(USER:HB):-USER==user,!,pp_i2tml_0(HB),!.
pp_i2tml_0((H :- B)):-B==true,!,pp_i2tml_0((H)),!.
pp_i2tml_0(((USER:H) :- B)):-USER==user,!,pp_i2tml_0((H:-B)),!.
pp_i2tml_0((H:-B)):-B==true, !, pp_i2tml_0(H).

pp_i2tml_0(P):- is_listing_hidden(P),!.
pp_i2tml_0(was_chain_rule(H)):- pp_i2tml_0(H).
pp_i2tml_0(M:(H)):-M==user, pp_i2tml_0(H).
pp_i2tml_0(is_edited_clause(H,B,A)):- pp_i2tml_0(proplst([(clause)=H,before=B,after=A])).
pp_i2tml_0(is_disabled_clause(H)):- pp_i2tml_0((disabled)=H).


% pp_i2tml_0(FET):-fully_expand(change(assert,html_gen),FET,NEWFET),FET\=@=NEWFET,!,pp_i2tml_0(NEWFET).
pp_i2tml_0(spft(_MZ,P,F,T)):- !, pp_i2tml_0(spft(P,F,T)).

pp_i2tml_0(spft(P,F,T)):-!,
   locally_tl(current_why_source(T),pp_i2tml_0(spft(P,F,T))).

pp_i2tml_0(spft(P,U,U)):- nonvar(U),!, pp_i2tml_1(P:-asserted_by(U)).
pp_i2tml_0(spft(P,F,T)):- atom(F),atom(T),!, pp_i2tml_1(P:-asserted_in(F:T)).
pp_i2tml_0(spft(P,F,T)):- atom(T),!,  pp_i2tml_1(((P):-  T:'t-deduced',F)). 
pp_i2tml_0(spft(P,F,T)):- atom(F),!,  pp_i2tml_1(((P):-  F:'f-deduced',T)). 
pp_i2tml_0(spft(P,F,T)):- !, pp_i2tml_1((P:- ( 'deduced-from'=F,  (rule_why = T)))).
pp_i2tml_0(nt(_,Trigger,Test,Body)) :- !, pp_i2tml_1(proplst(['n-trigger'=Trigger , bformat=Test  ,  (body = (Body))])).
pp_i2tml_0(pt(_,Trigger,Body)):-      pp_i2tml_1(proplst(['p-trigger'=Trigger , ( body = Body)])).
pp_i2tml_0(bt(_,Trigger,Body)):-      pp_i2tml_1(proplst(['b-trigger'=Trigger ,  ( body = Body)])).

pp_i2tml_0(proplst([N=V|Val])):- is_list(Val),!, pp_i2tml_1(N:-([clause=V|Val])).
pp_i2tml_0(proplst(Val)):-!, pp_i2tml_1(:-(proplst(Val))).


pp_i2tml_0(M:H):- M==user,!,pp_i2tml_1(H).
pp_i2tml_0((M:H:-B)):- M==user,!,pp_i2tml_1((H:-B)).
pp_i2tml_0(HB):-pp_i2tml_1(HB).




%% if_html( ?ARG1, :GoalARG2) is det.
%
% If HTML.
%
if_html(F,A):-is_html_mode,!,bformat(F,[A]).
if_html(_,A):-A.



%% pp_i2tml_1( ?ARG1) is det.
%
% Pretty Print i2tml  Secondary Helper.
%
pp_i2tml_1(H):- 
 once(((lmcache:last_item_offered(Was);Was=foobar),get_functor(Was,F1,_A1),get_functor(H,F2,_A2),
   retractall(lmcache:last_item_offered(Was)),asserta(lmcache:last_item_offered(H)),
    ((F1 \== F2 -> if_html('~N~@<hr/>',true);true)))),flush_output_safe,fail.

pp_i2tml_1(_H):- t_l:current_clause_ref(Ref),
    if_html('<font size="1">~@</font>',show_clause_ref(Ref)),fail.

pp_i2tml_1(H):- is_html_mode, 
  term_to_pretty_string(H,ALT)->
    term_to_pretty_string(ALT,URL)->
   functor_to_color(H,FC)->fmtimg(FC,ALT)->
    bformat('<input type="checkbox" name="assertion[]" value="~w">',[URL]),fail.

pp_i2tml_1(H):- \+ \+ must_run(pp_i2tml_now(H)).




%% pp_i2tml_now( ?ARG1) is det.
%
% Pretty Print I2tml Now.
%
pp_i2tml_now(C):- t_l:pp_i2tml_hook(C),!.
pp_i2tml_now(C):- if_html('<font size="3">~@</font>~N',if_defined(rok_portray_clause(C),portray_clause(C))).



%% functor_to_color( ?ARG1, ?ARG2) is det.
%
% Functor Converted To Color.
%
functor_to_color(wid(_,_,G),C):-!,functor_to_color(G,C).
functor_to_color(G,C):-compound(G),functor(G,F,A),functor_to_color(G,F,A,C).
functor_to_color(_G,green):-!.





%% functor_to_color( ?ARG1, ?ARG2, ?ARG3, ?ARG4) is det.
%
% Functor Converted To Color.
%
functor_to_color(_G,isa,_,bug_btn_s).

functor_to_color(_G,genls,1,'plus-green').
functor_to_color(_G,arity,_,'white').
functor_to_color(_G,argIsa,_,'white').
functor_to_color(_G,argGenls,_,'white').

functor_to_color(_,_,1,yellow).

functor_to_color(G:-_,_,_,C):-nonvar(G),!,functor_to_color(G,C).



functor_to_color(_,(<==>),_,'plus-purple').
functor_to_color(_,(<-),_,purple).
functor_to_color(_,(<=),_,'cyc-right-triangle-violet').
functor_to_color(_,(==>),_,'cyc-right-triangle-violet').
functor_to_color(_,(:-),_,red_diam).


functor_to_color(_,-,_,red).
functor_to_color(_,not,_,red).
functor_to_color(_,~,_,red).
functor_to_color(_,~,_,red).

functor_to_color(_,(if),_,cy_menu).
functor_to_color(_,(iff),_,cyan).
functor_to_color(_,(all),_,cyan).
functor_to_color(_,(exists),_,blue).

functor_to_color(_,(mudEquals),_,pink).
functor_to_color(_,(skolem),_,pink).
functor_to_color(_,(wid),_,green_yellow).

functor_to_color(G,_,_,'lightgrey'):-predicate_property(G,foreign).
functor_to_color(G,_,_,'cyc-logo-3-t'):-predicate_property(G,built_in).





%% session_checked( ?ARG1) is det.
%
% Session Checked.
%
session_checked(Name):- get_param_sess(Name,V),V\=='0',V\==0,V\=="0".




%% session_checkbox( ?ARG1, ?ARG2, ?ARG3) is det.
%
% Session Checkbox.
%
session_checkbox(Name,Caption,BR):-
 (session_checked(Name)-> CHECKED='CHECKED';CHECKED=''),
 bformat('<font size="-3"><input type="checkbox" name="~w" value="1" ~w />~w</font>~w',[Name,CHECKED,Caption,BR]).
 % bformat('<font size="-3"><label><input type="checkbox" name="~w" value="1" ~w/>~w</label></font>~w',[Name,CHECKED,Caption,BR]).




%% action_menu_applied( ?ARG1, ?ARG2, ?ARG3) is det.
%
% Action Menu Applied.
%
action_menu_applied(MenuName,ItemName,Where):-
  block_format(( bformat('<label>',[]),show_select2(MenuName,xaction_menu_item,[atom_subst('$item',ItemName)]),
      bformat('&nbsp;~w&nbsp;&nbsp;<input type="submit" value="Now" name="Apply">',[Where]),
      bformat('</label>',[]))).

%% is_context( ?ARG1, ?ARG2) is det.
%
% If Is A Context.
%
is_context(MT,MT):-no_repeats(is_context0(MT)).



%% is_context0( ?ARG1) is det.
%
% If Is A Context Primary Helper.
%
is_context0(MT):- if_defined(exactlyAssertedEL_first(isa, MT, 'tMicrotheory',_,_),fail).
is_context0(MT):- if_defined(isa(MT,'tMicrotheory'),fail).
is_context0('BaseKB').                           






%% get_request_vars( ?ARG1) is det.
%
% Get Request Variables.
%
get_request_vars(Format):- ignore(Exclude=[term,find,session_data,webproc,user_agent,referer,session,request_uri,accept]),
   findall(N=V,(current_form_var(N),\+ member(N,Exclude),once(get_param_sess(N,V))),NVs),
   forall(member(N=V,NVs),format(Format,[N,V])).


%% must_run( :GoalARG1) is det.
%
% Hmust (list Version).
%
must_run(List):-  is_list(List),!,must_maplist(must_run,List),!.
must_run((G1,G2)):- !,must_run(G1),!,must_run(G2),!.
must_run([G1|G2]):- !,must_run(G1),!,must_run(G2),!.
must_run(Goal):- flush_output_safe,
   (Goal
    -> flush_output_safe ; wdmsg(assertion_failed(fail, Goal))).




%% call_for_terms( ?ARG1) is det.
%
% Call For Terms.
%
call_for_terms(Call):- 
   must_run((
      get_param_sess(term,Term,"tHumanHead"),
      get_param_sess(find,SObj,Term),
      cvt_param_to_term(SObj,Obj),
        write_begin_html('search4term',Base,_),
        show_search_form(Obj,Base),
        bformat('<pre>',[]),        
        locally_tl(print_mode(html),with_search_filters(catch(ignore(Call),E,dmsg(E)))),
        bformat('</pre>',[]),
        show_pcall_footer,
        write_end_html)),!.

:- thread_local(t_l:tl_hide_data/1).

show_search_form(Obj,Base):-
   block_format((
        format('<form action="search4term" target="_self"><font size="-3">Apply: ~@',[action_menu_applied('action_below','Checked or Clicked',"&nbsp;below&nbsp;")]),
        format('&nbsp;&nbsp;&nbsp;find: <input id="find" type="text" name="find" value="~q"> Base = ~w</font> <a href="edit1term" target="_top">edit1term</a><br/>~@ <hr/></form>~n~@',
            [Obj,Base,show_search_filters('&nbsp;&nbsp;'),add_form_script]))),  !.


%% with_search_filters( :GoalARG1) is det.
%
% Using Search Filters.
%


with_search_filters(C):-
  retractall(t_l:tl_hide_data(_)),
  with_search_filters0(C),!.

with_search_filters0(C):-
   search_filter_name_comment(FILTER,_,_),
   session_checked(FILTER), 
   \+ t_l:tl_hide_data(FILTER),!,
    locally_tl(tl_hide_data(FILTER),with_search_filters0(C)).
with_search_filters0(C):-call(C).





%% make_page_pretext_obj( ?ARG1) is det.
%
% Make Page Pretext Obj.
%

% make_page_pretext_obj(Obj):- atom(Obj),atom_to_term(Obj,Term,Bindings),nonvar(Term),Term\=@=Obj,!,must_run(make_page_pretext_obj(Term)).

make_page_pretext_obj(Obj):- 
 must_run((
  % catch(mmake,_,true),
  % forall(no_repeats(M:F/A,(f_to_mfa(Pred/A,M,F,A))),ignore(logOnFailure((this_listing(M:F/A),flush_output_safe)))),
  % forall(no_repeats(M:F/A,(f_to_mfa(Pred/A,M,F,A))),ignore(logOnFailure((reply_object_sub_page(M:F/A),flush_output_safe)))),
  % ignore((fail,catch(mpred_listing(Pred),_,true))),
  quietly(call_with_time_limit(300,ignore(catch(xlisting_inner(i2tml_hbr,Obj,[]),E,wdmsg(E))))),
  pp_i2tml_saved_done(Obj))),!.

make_page_pretext_obj(Obj):- writeq(make_page_pretext_obj(Obj)),!.



% :- prolog_xref:assert_default_options(register_called(all)).




%% reply_object_sub_page( ?ARG1) is det.
%
% Reply Object Sub Page.
%
reply_object_sub_page(Obj) :- phrase(object_sub_page(Obj, []), HTML), html_write:print_html(HTML),!.


%%  object_sub_page(+ Obj, + Options)// is det.
%
% -->.
%
object_sub_page(Obj, Options) -->
	{ pldoc_process:doc_comment(Obj, File:_Line, _Summary, _Comment)
	}, !,
	(   { \+ ( pldoc_process:doc_comment(Obj, File2:_, _, _),
		   File2 \== File )
	    }
	->  html([ \object_synopsis(Obj, []),
		   \objects([Obj], Options)
		 ])
	;   html([
		   \objects([Obj], [synopsis(true)|Options])
		 ])
	).













%% return_to_pos( :GoalARG1) is det.
%
% Return Converted To Pos.
%
return_to_pos(Call):- current_line_position(LP),Call,!, must_run(set_line_pos(LP)).



%% nl_same_pos is det.
%
% Nl Same Pos.
%
nl_same_pos:-return_to_pos(nl).






%% set_line_pos( ?ARG1) is det.
%
% Set Line Pos.
%
set_line_pos(LP):-current_output(Out),set_line_pos(Out,LP).



%% set_line_pos( ?ARG1, ?ARG2) is det.
%
% Set Line Pos.
%
set_line_pos(_,_):-!.
set_line_pos(Out,LP):- 
  current_line_position(Out,CLP), 
  (CLP==LP->! ;((CLP>LP->nl(Out);put_code(Out,32)),!,set_line_pos(Out,LP))).




%% current_line_position( ?ARG1) is det.
%
% Current Line Position.
%
current_line_position(LP):-current_output(Out),current_line_position(Out,LP).

:- kb_shared(baseKB:wid/3).


%% current_line_position( ?ARG1, ?ARG2) is det.
%
% Current Line Position.
%
current_line_position(Out,LP):-stream_property(Out,position( Y)),stream_position_data(line_position,Y,LP),!.



%% test_tmw is det.
%
% Tmw.
%
test_tmw:- locally_tl(print_mode(html),
 (rok_portray_clause(a(LP)),
  rok_portray_clause((a(LP):-b([1,2,3,4]))),
  nl,nl,call_u(wid(_,_,KIF)),
  KIF='=>'(_,_),nl,nl,print(KIF),listing(print_request/1))),!.
test_tmw2:- locally_tl(print_mode(html),(print((a(_LP):-b([1,2,3,4]))),nl,nl,wid(_,_,KIF),KIF='=>'(_,_),nl,nl,print(KIF),listing(print_request/1))),!.



% II = 56+TTT, ((show_call(why,(url_encode(II,EE),var_property(TTT,name(NNN)),url_decode(EE,OO))))),writeq(OO).




%% url_encode( ?ARG1, ?ARG2) is det.
%
% Url Encode.
%
url_encode(B,A):- \+ atom(B),!,term_variables(B,Vars),url_encode_term(B,Vars,O),O=A.
url_encode(B,A):- atom_concat('\n',BT,B),!,url_encode(BT,A).
url_encode(B,A):- atom_concat(BT,'\n',B),!,url_encode(BT,A).
url_encode(B,A):- atom_concat(' ',BT,B),!,url_encode(BT,A).
url_encode(B,A):- atom_concat(BT,' ',B),!,url_encode(BT,A).
url_encode(B,A):- url_iri(A,B).





%% url_encode_term( ?ARG1, ?ARG2, ?ARG3) is det.
%
% Url Encode Term.
%
url_encode_term(B,[],O):- !, term_to_atom('#$'(B:[]),BB),!,url_iri(O,BB).
url_encode_term(InTerm,_VsIn,URL):- fail, with_output_to(atom(IRI),portray_clause('#$'((InTerm:_)))),
  url_iri(URL,IRI),nb_linkval(URL,InTerm),!.

url_encode_term(InTerm,VsIn,URL):-
  get_varname_list(Prev),
  name_the_var(40,Prev,VsIn,_NewVs,Added),
  % (NewVs\==Prev ->  show_call(why,put_variable_names(NewVs)) ; true),
  with_output_to(atom(IRI),write_term('#$'(InTerm:Added),[quoted(true),variable_names(Added),quoted,priority(9)])),
  url_iri(URL,IRI),!.




%% member_open( ?ARG1, :TermARG2) is det.
%
% Member Open.
%
member_open(C, [B|A]) :-  (nonvar(B),B=C) ; (nonvar(A),member_open(C, A)).




%% name_the_var( ?ARG1, ?ARG2, :TermARG3, :TermARG4, :TermARG5) is det.
%
% Name The Variable.
%
name_the_var(_Num,Vs,[],Vs,[]).

name_the_var(Num,Vs,[VIn|More],VsOut,[N=V|Added]):- member_open(N=V,Vs),VIn==V,!,name_the_var(Num,Vs,More,VsOut,Added).
% name_the_var(Num,Vs,[VIn|More],VsOut,[N=VIn|Added]):- \+ is_list(Vs), append(Vs,[N=VIn],NewVs),!, name_the_var(Num,NewVs,More,VsOut,Added).
name_the_var(Num,Vs,[VIn|More],[N=VIn|VsOut],[N=VIn|Added]):- Num2 is Num +1, NV = '$VAR'(Num),
  with_output_to(atom(N),write_term(NV,[portrayed(true),quoted,priority(9)])),
  name_the_var(Num2,Vs,More,VsOut,Added).



%  II = 56+TTT, rtrace((url_encode(II,EE),url_decode(EE,OO))),writeq(OO),OO=II.



% url_decode(B,A):- \+ atom(B),!,term_to_atom(B,BB),!,url_encode(BB,O),!,A=O.



%% url_decode( ?ARG1, ?ARG2) is det.
%
% Url Decode.
%
url_decode(B,A):- \+ atom(B),A=B.
url_decode(A,B):- atom_concat('#%24%28',_,A) , url_decode_term(A,T),!,T=B.
url_decode(A,B):- url_iri(A,C),!,B=C.




%% url_decode_term( ?ARG1, ?ARG2) is det.
%
% Url Decode Term.
%
url_decode_term(A,T):- nb_current(A,T),nb_delete(A),!.
url_decode_term(A,T):- url_iri(A,B),
    read_term_from_atom(B,'#$'(T:Vs2),[variable_names(Vs3)]),
    ignore(Vs2=Vs3),!, ignore(Vs2=[]),!.

url_decode_term(A,T):-
    url_iri(A,B),
    read_term_from_atom(B,'#$'(T:Vs2),[variable_names(Vs3)]),
    ignore(Vs2=[]),ignore(Vs2=Vs3),
    merge_key_vals(B,Vs2,Merge),
    get_varname_list(Env),
    merge_key_vals(Env,Merge,New),
    put_variable_names(New),!.






%% tovl( :TermARG1, :TermARG2, :TermARG3) is det.
%
% Tovl.
%
tovl([],[],[]).
tovl([K|KL],[V|VL],[K=V|KVL]) :- tovl(KL, VL, KVL).




%% merge_key_vals( :TermARG1, ?ARG2, ?ARG3) is det.
%
% Merge Key Vals.
%
merge_key_vals(Prev,Pairs,NewSave):-var(Prev),!,NewSave=Pairs.
merge_key_vals([],Pairs,NewSave):-!,NewSave=Pairs.
merge_key_vals([K=V1|Prev],Pairs,NewSave):-
   member_open(K=V2,Pairs),
   V1==V2, merge_key_vals(Prev,Pairs,NewSave).
merge_key_vals([K1=V1|Prev],Pairs,NewSave):-
   member_open(K2=V2,Pairs),
   K1==K2, V1=V2, merge_key_vals(Prev,Pairs,NewSave).
merge_key_vals([K1=V1|Prev],Pairs,NewSave):-
   merge_key_vals(Prev,[K1=V1|Pairs],NewSave).





% x(Z+B)

%   b_setval(URL,InTerm).




%% write_as_url_encoded( ?ARG1, ?ARG2) is det.
%
% Write Converted To Url Encoded.
%
write_as_url_encoded(_Arg, D):- url_encode(D,U),!,writeq(U).
:- format_predicate('u',write_as_url_encoded(_Arg,_Time)).




%% term_to_pretty_string( ?ARG1, ?ARG2) is det.
%
% Term Converted To Pretty String.
%
term_to_pretty_string(H,HS):- \+ compound(H),!,with_output_to(string(HS),writeq(H)).
term_to_pretty_string(H,HS):-
   % igno re(source_variables(X))->ignore(X=[])->
   % numb ervars(HC,0,_)->
  with_output_to(string(HS),portray_clause(H)).




%% fmtimg( ?ARG1, ?ARG2) is det.
%
% Fmtimg.
%
fmtimg(N,Alt):- is_html_mode,!,
 make_quotable(Alt,AltQ),
 url_encode(Alt,AltS),
 bformat('~N<a href="?webproc=edit1term&term=~w" target="_parent"><img src="/pixmapx/~w.gif" alt="~w" title="~w"><a>',[AltS,N,AltQ,AltQ]).
fmtimg(_,_).





%% indent_nbsp( ?ARG1) is det.
%
% Indent Nbsp.
%
indent_nbsp(X):-is_html_mode,forall(between(0,X,_),bformat('&nbsp;')),!.
indent_nbsp(X):-forall(between(0,X,_),format('~t',[])),!.




%% indent_nl is det.
%
% Indent Nl.
%
indent_nl:- fresh_line, flag(indent,X,X), indent_nbsp(X).





%% indent_nbsp( :PRED1ARG1, ?ARG2) is det.
%
% Indent Nbsp.
%
indent_nbsp(0,''):-!.
indent_nbsp(1,'\n         '):-!.
indent_nbsp(X,Chars):-XX is X -1,!, indent_nbsp(XX,OutP),!,sformat(Chars,'~w   ',[OutP]),!.






%% shared_hide_data( :PRED4ARG1) is det.
%
% Hook To [logicmoo_util_term_listing:shared_hide_data/1] For Module Mpred_www.
% Shared Hide Data.
%

:- xlisting_web:import(xlisting:is_listing_hidden/1).

shared_hide_data_sp(Var):- is_ftVar(Var),!,fail.
shared_hide_data_sp(_:F/A):- !,shared_hide_data_sp(F/A).
shared_hide_data_sp('$si$':'$was_imported_kb_content$'/2):- !,is_listing_hidden(hideMeta).
shared_hide_data_sp(spft/3):- !,is_listing_hidden(hideTriggers).
shared_hide_data_sp(nt/3):- !,is_listing_hidden(hideTriggers).
shared_hide_data_sp(pt/2):- !, is_listing_hidden(hideTriggers).
shared_hide_data_sp(bt/2):- !, is_listing_hidden(hideTriggers).
shared_hide_data_sp((_:-
 cwc,
        second_order(_,G19865),
        (   _G19865 = (G19867,!,G19871) ->
                call(G19867),  !,
                call(G19871)
        ;   CALL
        ))):- CALL=@=call(G19865).


shared_hide_data_sp(saved_request/_):- !.
shared_hide_data_sp(session_data/_):- !.
shared_hide_data_sp(mpred_prop/3):- !,is_listing_hidden(hideMeta).
shared_hide_data_sp(last_item_offered/1):- !,is_listing_hidden(hideMeta).
shared_hide_data_sp(P0):- strip_module(P0,_,P), compound(P),functor(P,F,A),F\== (/) , !,shared_hide_data_sp(F/A).
shared_hide_data_sp((Pred)) :-  fail, rok_portray_clause((Pred:-true)).


:- multifile baseKB:shared_hide_data/1.
:- kb_global(baseKB:shared_hide_data/1).
baseKB:shared_hide_data(MFA):- cwc,nonvar(MFA), shared_hide_data_sp(MFA).

%:- mpred_trace_exec.

/*use_baseKB(M,I) :-
  M:import(pfccore:pfcDefault/2),
  I:import(pfccore:pfcDefault/2),
 % pfc_umt:abox_pred_list(PREDS)-> must_maplist(kb_shared_local(M,I),PREDS),
 forall(no_repeats(pfc_umt:pfcDatabaseTerm_DYN(F/A)),show_call(kb_shared_local(M,I,F/A))).
:- use_baseKB(xlisting_web).
*/


%:- nb_setval(defaultAssertMt,xlisting_web).



xlisting_web_file.

          

%:- mpred_notrace_exec.

%:- nb_setval(defaultAssertMt,[]).

% :- ensure_sigma(6767).
:- must( \+ pfc_lib:is_pfc_file0).
:- ensure_loaded('xlisting_web.pfc').
:- must( \+ is_pfc_file).

:- fixup_exports.

%:- noguitracer.
% WANT 

:- set_prolog_flag(hide_xpce_library_directory,false).
:- retract(t_l:no_cycstrings).

:- during_net_boot(register_logicmoo_browser).
:- set_fileAssertMt(baseKB).



