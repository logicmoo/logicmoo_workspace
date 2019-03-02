/* 
LPS web server plumbing. Written by Miguel Calejo.

Copyright (c) 2017-2019, Renting Point - Serviços de Informática, Lda., Portugal
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

1. Redistributions of source code must retain the above copyright
notice, this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright
notice, this list of conditions and the following disclaimer in the
documentation and/or other materials provided with the distribution.

3. Neither the name of the copyright holder nor the names of its
contributors may be used to endorse or promote products derived from
this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

*/

:- module(lps_server_UI, [ check_user_server_usage/0, lps_user_is_super/0, check_powerful_user/1, term_rendering//3]).

:- use_module(library(http/html_write)).
:- use_module(library(http/term_html)).
:- use_module(library(http/js_write)).
:- use_module(library(http/http_json)).
:- use_module('../../swish/lib/render').

:- multifile sandbox:safe_primitive/1.

:- register_renderer(lps_server_UI, "Access to a LPS server execution").

:- discontiguous(lps_server_UI:term_rendering/5).

term_rendering(lpsServer(LPS_ID), _Vars, _Options) -->
	html(div(['data-render'('As LPS Server')],[
		p('See this LPS server\'s status, state and inject events from:'),
		\serverLinks([LPS_ID])
		])).

/* Failed hack:
% cf. swipl-devel/library/sandbox.pl:
:- asserta((  sandbox:verify_safe_declaration(Goal) :- nonvar(Goal), Goal = threadutil:threads)). % hack to allow the following:
:- use_module(library(threadutil),[threads/0]).
sandbox:safe_primitive(threadutil:threads).
tt:- threadutil:threads. % ...BUT STILL No permission to call sandboxed threadutil:threads ???
*/

% Relax authentication requirement, e.g. for development servers... This MUST be commented out in production!
% On a dev machine, better inject this fact on starting the local dev server:
% swipl -l user_module_file.pl -l ../../swish/server.pl -g "assert(lps_server_UI:allow_anonymous_powerful_ops)" -g server:server
:- dynamic allow_anonymous_powerful_ops/0.
% allow_anonymous_powerful_ops.


% call this before every potentially dangerous operation:
check_powerful_user(serve_ethereum) :- (allow_anonymous_powerful_ops -> true ; lps_user(User), User\=unknown_user ), !.
check_powerful_user(_Op) :- (allow_anonymous_powerful_ops -> true ; lps_user_is_super), !.
check_powerful_user(Op) :- throw(unsufficient_lps_user_privilege_for(Op)).

% Extend this predicate to give some users all powers
:- multifile lps_server_UI:super_user/1. % Make sure to include quotes in the user ids

lps_user_is_super :- lps_user(User), super_user(User).

any_call(G) :- check_powerful_user(sudo), G.

sandbox:safe_primitive(lps_server_UI:any_call(G)) :- nonvar(G).

user:sudo(G) :- any_call(G).

:- multifile prolog:message//1.
prolog:message(unsufficient_lps_user_privilege_for(Op)) --> {lps_user(unknown_user), ! },
	['To perform ~w you must login first with your Google account. Please use the login button on top right.'-[Op]].
prolog:message(unsufficient_lps_user_privilege_for(Op)) --> {lps_user(U), ! },
	['You must be logged in using a Google account with privileges for ~w (and your current account ~w does not). Please use the login button on top right.'-[Op,U]].

% user:lps_postmortem_filename(-FilePath)
% Determines filename for the post mortem ("test") file on the server, based on program source code
% and current user: current_dir/postmortems/userID/MD5hash.lpst; creates directories if necessary
user:lps_postmortem_filename(FilePath) :-
    	interpreter:lps_user(U), concat_atom(['postmortems/',U,'/'],UD),
	(exists_directory(UD)->true;make_directory(UD)),
	interpreter:get_lps_program_hash(Hash),
	concat_atom([UD,Hash,'.lpst'],FilePath).

% Launch the current window's progrwhynam in background; BEWARE, this requires upcoming authentication, cycles budget, etc.!!!!
user:serve(lpsServer(ThreadID)) :-
	check_user_server_usage,
	interpreter:go(_, [background(ThreadID),/*silent,*/swish,dc]). % current logging is unbound, uncomment this in production

check_user_server_usage :-
	lps_user(User),
	findall(InvSleep,(background_execution(User,_TID,_RT,_MaxRT,_ET,MinCT,_,running),MinCT\==0,InvSleep is 1/MinCT),L),
	sum_list(L, Sum),
	(Sum > 10000 -> % e.g. more than 10 servers with 1 mS cycle sleep time each:
		print_message(error,'Can not create server. This user needs to increase minCycleTime'), fail
		; true).

user:servers :-
	format(atom(Title),'~a  ~w  ~w  ~w  ~w  ~w  ~w ~w~n',['User','Thread','BeganRT','MaxRT','MaxCycles','CycleSleep', 'Final State', 'Status']),
	writeln(Title),
	background_execution(User,TID,RT,MaxRT,ET,MinCT,FinalState,Status),
	format(atom(Line),'~a  ~w  ~w  ~w  ~w  ~w ~w ~w~n',[User,TID,RT,MaxRT,ET,MinCT,FinalState,Status]),
	writeln(Line), fail.
user:servers.

user:servers(lpsServers(IDs)) :- lps_user(User),
	findall(TID,background_execution(User,TID,_RT,_MaxRT,_ET,_MinCT,_FS,running),IDs).

term_rendering(lpsServers(IDs), _Vars, _Options) -->
	html(div(['data-render'('As LPS Servers list')],[
		p('Your running LPS servers (use ?- servers. to see dead ones too):'),
		\serverLinks(IDs)
		])).

serverLinks([]) --> [].
serverLinks([ID|IDs]) -->
	{format(atom(MURL),"/lps_server/manager/~w",[ID]), format(atom(MLabel),"Manage ~w",[ID])},
	{(
		background_execution(ID,_,_,_,_,_FinalState,running) -> 
			format(atom(DURL),"/lps_server/d_sample/~w",[ID]), DLabel="Display", Displayer=span([" | ",a([href=DURL],DLabel)])
			; Displayer=[]
	)},
	html(p([span(a([href=MURL],MLabel))|Displayer])), 
	serverLinks(IDs).

background_execution(TID,RT,MaxRT,ET,MinCT,FS,Status) :- lps_user(User),
	background_execution(User,TID,RT,MaxRT,ET,MinCT,FS,Status).

background_execution(User,TID,RT,MaxRT,ET,MinCT,FS,Status) :-
	interpreter:background_execution(User,TID,RT,MaxRT,ET,MinCT,FS), catch((thread_property(TID,Prop), Prop=status(Status)),_,Status='???').

:- multifile sandbox:safe_primitive/1.
% somehow removing this causes "Arguments are not sufficiently instantiated" in thread_property(...) above???
sandbox:safe_primitive(thread_property(_,_)).

user:kill_all_servers :- kill_all_servers.

kill_all_servers :- % TODO: restrict to administrators!
	check_powerful_user(kill_all_servers),
	interpreter:background_execution(_User,TID,_RT,_MaxRT,_ET,_MinCT,_FS), catch(thread_property(TID,status(Status)),_,Status='???'),
	Status == running,
	writeln(killing-TID),
	catch(interpreter:inject_events(TID,[lps_terminate],_), Ex, writeln(failed_to_kill-Ex)),
	fail.
kill_all_servers.

sandbox:safe_primitive(lps_server_UI:kill_all_servers).

:- http_handler('/lps_server/manager/', lps_serve_manager, [prefix]). % .../lps_server/manager/lps1
lps_serve_manager(Request) :-
	lps_user(User,Email),
	member(path_info(LPS_ID),Request),
	background_execution(LPS_ID,RealTimeBeginning,MaxRealT,MaxCycles, MinCycleTime,FinalState,Status), % user is checked here
	get_time(Now), Elapsed is Now - RealTimeBeginning,
	header_style(Style),
	format(atom(LogURL),"/lps_server/log/~w",[LPS_ID]),
	(Status==running ->
		interpreter:get_rtb_fluent_event_templates(LPS_ID,Cycle,Beginning,Fluents,Events), % TODO: nicer failure reporting, please
		format_time(atom(RTB),'%a, %d %b %Y %T %Z',Beginning),
		(select(lps_terminate,Events,UserEvents) -> true; UserEvents=Events),
		format(atom(SaveURL),"/lps_server/events/~w?events=[]&fluents=[lps_saved_state(_,_,_,_,_,_,_,_)]&after=true",[LPS_ID]),
		format(atom(KillURL),"/lps_server/events/~w?events=[lps_terminate]",[LPS_ID]),
		format(atom(SaveFinishURL),"/lps_server/events/~w?events=[lps_terminate]&fluents=[lps_saved_state(_,_,_,_,_,_,_,_)]&after=true",[LPS_ID]),
		reply_html_page(title([LPS_ID,' manager']), [ % overkill, too many styles AND messes with simple scrolling: swish_page : \swish_css,
			h2([Style],['LPS manager for ',LPS_ID]),
			p("Hello ~w"-[User/Email]),
			p([ "Status: ", b("~w"-[Status]), " Cycle: ~w"-[Cycle], " ", a([href=LogURL,target='_blank'],"See execution log") ]),
			p("~1f seconds elapsed after ~s."-[Elapsed,RTB]),
			p("maxRealTime ~w seconds, maxCycles ~w, cycle sleep ~w"-[MaxRealT,MaxCycles,MinCycleTime]),
			h3('Fluents'), p('Please click to sample their state:'), ul(\fluentLinks(Fluents,LPS_ID)),
			h3('Events'), \eventsForm(LPS_ID,UserEvents),
			h3('Commands'),
			p(a([href=SaveURL],'See snapshot of execution state')),
			p(a([href=SaveFinishURL],'Save execution state and Kill')),
			p(a([href=KillURL],'Kill'))
		])
		;
		reply_html_page(title([LPS_ID,' manager']), [ % swish_page: \swish_css,
			h2([Style],['LPS manager for ',LPS_ID]),
			p(["Status: ~w"-[Status], ' (FINISHED) ', a([href=LogURL,target='_blank'],"See execution log")]),
			p("~1f seconds elapsed."-[Elapsed]),
			p("maxRealTime ~w seconds, maxCycles ~w, cycle sleep ~w"-[MaxRealT,MaxCycles,MinCycleTime]),
			h2('Final state:'), p("~w"-[FinalState])
		])
	).

header_style(style='padding: 7px; background-color: #317BB8; color: #FFFFFF; font-family:"Arial"').

:- http_handler('/lps_server/log/', lps_serve_log, [prefix]). % .../lps_server/log/lps1
lps_serve_log(Request) :-
	member(path_info(LPS_ID),Request),
	% consider tail of log... ? member(search(Query),Request),
	background_execution(LPS_ID,_,_,_,_,_FinalState,Status), % user is checked here
	(Status==running -> Refresher = [meta([http-equiv=refresh,content=10])] ; Refresher = []),
	interpreter:server_log_filename(LPS_ID,File),
	read_file_to_html(File,HTML),
	header_style(Style),
	swish_page:reply_html_page([
		title([LPS_ID,' log']),
		 % auto-refresh page
		% TODO: scrolldown is not happening on manual log reload
		% courtesy of https://stackoverflow.com/questions/11715646/scroll-automatically-to-the-bottom-of-the-page :
		script("window.onload = function scrollit(){window.scrollTo(0, document.body.scrollHeight || document.documentElement.scrollHeight);}")
		|Refresher
		], [h2([Style],['Execution log for ',LPS_ID,':'])|HTML] ).

read_file_to_html(File,HTML) :-
	open(File,read,S), read_file_to_html_(S,HTML), close(S).

read_file_to_html_(S,HTML) :-
	read_line_to_string(S, Line),
	( Line==end_of_file -> HTML=[]
	;
		(	(Line = "" -> HTML=[br([])|MoreHTML] ; HTML=[Line,br([])|MoreHTML] ),
			read_file_to_html_(S,MoreHTML)
		)
	).


fluentLinks([],_) --> [].
fluentLinks([Fluent|Fluents],LPS_ID) -->
	{ functor(Fluent,F,N), format(atom(URL),"/lps_server/fluents/~w?fluents=[~w]",[LPS_ID,Fluent]) },
	html(li( a([href=URL,target='_blamk'], "~w"-[F/N]) )),
	fluentLinks(Fluents,LPS_ID).

eventsForm(LPS_ID,Events) --> html(form([
	'Events to send:',
	input([type=text,name=events,size=80,value="~w"-[Events]]),
	input([type=button,value='Send',onclick="window.open('/lps_server/events/~w?events='+this.form.events.value+'');"-[LPS_ID]])
	])).

:- http_handler('/lps_server/fluents/', lps_server_UI:lps_serve_fluents, [prefix]). % .../lps_server/fluents/lps1?fluents=[available(_)]
lps_serve_fluents(Request) :-
	% http_log('lps_serve:~w', [Request]),
	member(path_info(LPS_ID),Request),
	% http_log('lps_serve:~w~n', [LPS_ID]),
	member(search(Query),Request),
	background_execution(ServerUser,LPS_ID,_RT,_MaxRT,_ET,_MinCT,_FS,_Status),
	prepare_fluent_templates(Query,ServerUser,Fluents),
	(Fluents = [_|_] -> true ; throw(no_fluents_requested)),
	interpreter:get_fluents(LPS_ID,Fluents,Cycle,Values), % TODO: nicer failure reporting, please
	fluent_values_to_table_list(Fluents,Values,Tables),
	header_style(Style),
	reply_html_page(title([LPS_ID,' fluents at ',Cycle]), [
		h2([Style], ['LPS fluents for ',LPS_ID,' at ',Cycle, ':']),\fluentTables(Tables)
		] ).

prepare_fluent_templates(Query,ServerUser,Fluents) :-
	((member((fluents)=FluentsAtom,Query)) ->
		read_term_from_atom(FluentsAtom, Fluents_, [])
		; Fluents_ = [] ),
	(is_list(Fluents_) -> Fluents=Fluents_ ; Fluents=[Fluents_]),
	lps_user(U),
	((member(lps_saved_state(_,_,_,_,_,_,_,_),Fluents), U\==ServerUser) ->
		throw(saving_state_requires_server_creator_user)
		; true).

fluent_values_to_table_list(Fluents,Values,Tables) :-
	findall(Functor/Arity-Tuples,
		(member(Template,Fluents), functor(Template,Functor,Arity), findall(Template,member(Template,Values),Tuples)),
		Tables).

% :- use_module('../../swish/lib/page',[swish_css//0]).  Use \swish_css to insert SWISH CSSs

fluentTables([]) --> [].
fluentTables([Header-Tuples|Tables]) --> fluentTable(Header,Tuples), html(br([])), fluentTables(Tables).

fluentTable(Header,Tuples) --> html(table([border=1],[ \fluent_header(Header), \fluent_rows(Tuples)])).

fluent_header(HeaderF/HeaderA) --> html(tr(th([colspan=HeaderA], "~w"-[HeaderF/HeaderA]))).

fluent_rows([]) --> [].
fluent_rows([Fluent|Fluents]) --> {fluent_spans_row(Fluent), !, functor(Fluent,_,N)},
	html(tr(td([colspan=N], "~k."-[Fluent]))), % use write_canonical, to ensure term readability
	fluent_rows(Fluents).
fluent_rows([Fluent|Fluents]) --> {Fluent=..[_Functor]}, html(tr( \fluent_cells([true]) )), fluent_rows(Fluents).
fluent_rows([Fluent|Fluents]) --> {Fluent=..[_|Cells]}, html(tr( \fluent_cells(Cells) )), fluent_rows(Fluents).

fluent_cells([]) --> [].
fluent_cells([Cell|Cells]) --> html(td("~w"-[Cell])), fluent_cells(Cells).

fluent_spans_row(lps_saved_state(_,_,_,_,_,_,_,_)).

:- http_handler('/lps_server/events/', lps_server_UI:lps_serve_events_fluents, [prefix]).
% .../lps_server/events/lps1?events=[pickup(kant,fork1)]
% http://localhost:3050/lps_server/events/lps1?events=[pickup(kant,fork1)]&fluents=[available(_)]&after=true
% http://localhost:3050/lps_server/events/lps1?events=[lps_terminate]&fluents=[lps_saved_state(_,_,_,_,_,_,_,_)]&after=true  suspends and shows saved state
% Inject events into server and sample and return fluents; if after=true, sampling occurs  the beginning of the next LPS cycle
% (after the events are accepted, 1+ event injection cycle), otherwise at current cycle (prior to the events changing state)
lps_serve_events_fluents(Request) :-
	member(path_info(LPS_ID),Request),
	% http_log('lps_serve:~w~n', [LPS_ID]),
	member(search(Query),Request),
	background_execution(ServerUser,LPS_ID,_RT,_MaxRT,_ET,_MinCT,_FS,_Status),
	prepare_events(Query,ServerUser,Events),
	prepare_fluent_templates(Query,ServerUser,Fluents),
	(member(after=true,Query) -> After=true, Fluents=[_|_] ; After=false),
	once(( Fluents=[_|_] ; Events=[_|_])),

	interpreter:inject_events_fetch_fluents(LPS_ID,Events,After,Fluents,Result),
	(Result = failed(Message) -> Status=409 ; Status = 200),
	(Result = ok(Cycle,Values) ->
		format(atom(Message),'Events injected at cycle ~w~n', [Cycle]),
		fluent_values_to_table_list(Fluents,Values,Tables)
		; Result = failed(Message), Tables=[]),
	format('Status: ~d~n',[Status]),
	header_style(Style),
	reply_html_page( title('LPS at your service'), [
		h2([Style],'Events result:'),p(Message),h2([Style],'Fluent values:'),\fluentTables(Tables)
		] ).

prepare_events(Query,ServerUser,Events) :-
	(member((events)=EventsAtom,Query) ->
		read_term_from_atom(EventsAtom, Events_, []) %, http_log('Events:~w~n', [Events_])
		; Events_ = [] ),
	(is_list(Events_) -> Events=Events_ ; Events=[Events_]),
	AloneEvents = [lps_terminate],
	( (member(E,Events),member(E,AloneEvents), Events=[_,_|_])
		-> throw(that_event_must_be_alone_in_request)
	; true),
	lps_user(User),
	( (member(lps_terminate,Events), ServerUser\=User) %should possibly return Status=403:
		-> throw('LPS servers can be killed only by their creator user')
		; true).

% respond to a request for a sample of the current fluents and events covered by d/2 (2d display) declarations
:- http_handler('/lps_server/d_sample/', lps_server_UI:display_sample, [prefix]).  % .../lps_server/d_sample/lps1
display_sample(Request) :- 
	member(path_info(LPS_ID),Request),
	background_execution(LPS_ID,_,_,_,_,_FinalState,Status), % user is checked here
	( Status==running ->
		% Obtain list of t(Literal,T1-T2,Group) and use collect_display_specs to build a single cycle
		% fake cycle transitions, likely keeping a last_cycle_sample
		% likely in visualization.P
		% use variant of inject_events_fetch_fluents with access to d/2 etc., using query_thread:-)
		% no point using cycle_hook, different thread anyway
		
		%Events=[], Fluents = [balance(_,_)],
		%interpreter:inject_events_fetch_fluents(LPS_ID,Events,false/*sample before applying events*/,Fluents,Result),
		interpreter:query_thread(LPS_ID, lps_server_UI:get_fluents_events_actions(X,interpreter:d(X,_),_Terms), Cycle, Result),
		term_string(Result,ResultS),
		Sample = _{cycle:Cycle, ops:[],test:ResultS},
		reply_json_dict(Sample)
		; reply_json_dict(_{error:"Not running"})).

% get_fluents_events_actions(+Template,+Condition,-Terms) 
% Each term will be t(Literal,Type), where Type is action or event or Functor/Arity (of fluent)
% E.g. call with get_fluents_events_actions(lps13,X,interpreter:d(X,_),Cycle,Terms)
% TODO: 'timeless' missing!!! and fetch properties, we need those fetched in the user prograns's thread!
get_fluents_events_actions(X,Cond,Terms) :-
	findall(t(X,fluent),(
		interpreter:user_fluent(X), Cond, interpreter:query(holds(X,_)) 
		), Fluents),
	findall(t(X,Type),( 
		interpreter:happens(X,T1,T2), T2=:=T1+1, Cond, (interpreter:action_(X)->Type=action;Type=event)
		), EventsActions),
	append(Fluents,EventsActions,Terms).

/*
predsort(variant_compare,List,Sorted)
variant_compare(O,A,B):- variant(A,B)->O=(=);compare(O,A,B).
ord_subtract(LastSample,Current,Delta),...
% this actually can get more verbose then simply dumping the relevant fluents...
% ultimately a more incremental structure could be used, e.g. a state trie, plus diffs encoded as paths
% see also ipcompareTerms in interprolog.P for a simple approach
%	state_diff(Diffs), format("State DIFF: ~w~n",[Diffs]),
state_diff(Diffs) :-
	findall(- D,(state(D), \+ next_state(D)), Deleted),
	findall(+ I,(next_state(I), \+ state(I)), Inserted),
	append(Deleted,Inserted,Diffs).
*/

