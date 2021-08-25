% ===================================================================
% COMPLIER OPTIONS
% ===================================================================

:- style_check(-singleton).
:- style_check(-discontiguous).
:- was_style_check(-atom).
:- was_style_check(-string).

% ===================================================================
% DYNAMIC PREDICATES
% ===================================================================

:-was_indexed(sigmaCache(1)).
:-was_indexed(sigmaCache(1,1)).
:-was_indexed(sigmaCache(1,1,1)).
:-was_indexed(sigmaCache(1,1,1,1)).
:-was_indexed(sigmaCache(1,1,1,1,0)).
:-was_indexed(sigmaCache(1,0,1,1,1,0)).

:-dynamic(sigmaCache/1).
:-dynamic(sigmaCache/2).
:-dynamic(sigmaCache/3).
:-dynamic(sigmaCache/4).
:-dynamic(sigmaCache/5).
:-dynamic(sigmaCache/6).
:-dynamic(sigmaCache/9).


:- op(1200,fx,::-).         /* operator for integrity constraints */

%:- op(1200,xfx,<--).
%:- op(1150,fx,[(tabled),(prolog),(default)]).
:- op(900,xfx,<-).
:- op(400,fy,'~').


% User Agent
:-dynamic(always_hide_callback/1).
:-dynamic(always_show_callback/1).
:-dynamic('$SigmaOption'/3).
:-dynamic(client_socket/1).
:-dynamic(callback_pred/1).
:-dynamic(kb_make_status_start/1). 
:-dynamic(saved_note/4).
:-dynamic(act_mem/3).
:-dynamic(server_environment/1).

% Database
:-dynamic(tq_attempted_query/0).      
:-dynamic(title/1).      

% TQ System
:-dynamic(tq_missed_one_answer/0).      
:-dynamic(tq_least_one_answer/0).      
:-dynamic(t_answer_found/1).

:-dynamic(predicates_declared_inline_HL/2).
:-multifile(predicates_declared_inline_HL/2).

:-dynamic('surface-word'/2).
:-dynamic('surface-macro'/2).
:-dynamic('browser-only'/1).
:-dynamic('not-implemented'/1).
:-dynamic('surface-atom'/1).
:-dynamic('surface-single-arity'/1).
:-dynamic('surface-multiple-arity'/1).
:-dynamic('surface-instance'/2).
:-dynamic('surface-subclass'/2).
:-dynamic('surface-class'/1).
:-dynamic('surface-quantifier'/1).
:-multifile('surface-word'/2).
:-multifile('surface-macro'/2).
:-multifile('browser-only'/1).
:-multifile('not-implemented'/1).
:-multifile('surface-atom'/1).
:-multifile('surface-single-arity'/1).
:-multifile('surface-multiple-arity'/1).
:-multifile('surface-instance'/2).
:-multifile('surface-subclass'/2).
:-multifile('surface-class'/1).
:-multifile('surface-quantifier'/1).

%:-set_prolog_flag(unknown,fail).
 
% ===================================================================
% OPERATION PREDICATES
% ===================================================================
% Defaults
:-dynamic(getDefaultKB/1).
:-dynamic(get_default_query_context/1).
:-dynamic(get_default_assertion_context/1).
:-dynamic(version_tag/1).

:-dynamic(answer_found/1).
:-dynamic(sigma_K_scenario/2).
:-dynamic(telling_prolog/0).  % If set asserts clauses into prolog database
:-dynamic(telling_file/0).   % If set write assertions to file
:-dynamic(disp_debug/5).         %PREDICATE RESOLUTON
:-dynamic(contexts/2).            %CONTEXT STATES
:-dynamic(setting_table/2).
:-dynamic(tabling/1).
:-dynamic(tabled_t/1).
:-dynamic(tabled_f/1).
:-dynamic(answer_yes/0).
:-dynamic(already_asked/2).
:-dynamic(save_all/2).
:-dynamic(sigma_K_scenario/6).         %We keep a cache of forall consultations
:-dynamic(consultation_mode_on/0).
:-dynamic(resource_cache/2).
:-dynamic(debuggerize/0).

:-dynamic( le_ele/1).

% ===================================================================
% OPERATOR PRECEDANCE
% ===================================================================

% ===================================================================
% OPERATOR PRECEDANCE
% ===================================================================
:- op(500,xfx,#).
%:- op(500,xfy,:).
:- op(1000,xfy,'=>').
:- op(500,xfx,#).
:- op(500,xfx,'#').
%:- op(500,xfx,'@').

:- op(400,fy,not).    % negation
:- op(500,xfy,and).   % conjunction
:- op(600,xfy,or).   % disjunction
%:- op(500,xfy,:).
:- op(0,xfx, 'equal' ).
:- op(900,xfx,'<=').
:- op(900,xfx,'if').
:- op(1000,xfy,'=>').
:- op(400,fy,known).  % Found in Model
:- op(400,fy,possible).  % Not In Model
:- op(400,fy,next).  % Next time
:- op(400,fy,after).  % Next time
:- op(400,fy,then).  % Next time
:- op(650,xfy,=>).  % implication
:- op(700,xfy,<=>). % equivalence
:- op(400,fy,always).  % Necessity, Always
:- op(400,fy,possible).  % Possibly, Eventually
:- op(400,fy,necessary).  % Necessity



