/* 
LPS Explanator, by  Miguel Calejo.

Copyright (c) 201-2021, Miguel Calejo, Portugal
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

:- module(explanator,[expl_pretty/2,w_pretty/1,m_pretty/1,w/3,m/3,w/4,m/4,get_state/2,expl_tree/3]).
% assumes the LPS program has just executed with make_test, dc

:- if(\+ current_prolog_flag(dialect, swi)).
:- writeln("LPS requires SWI-Prolog"), throw(swi_prolog_required).
:- endif.


:- if(current_module(swish)).
			
% Running the explanator on Swish without re-running the whole LPS program 
% requires loading the "post mortem trace" (used by timelines and test suite) to be stored
% in a server file, named after the program source (e.g. get_lps_program_hash) and current user (lps_user);
% explanator and future UI predicates can then access it without having to execute godc(..) and friends.
% TODO: Such files will need garbage collection (possibly as soon as the current session (?) ends)

action(X) :- lps_program_module(M), catch(M:action(X),_,fail).
real_time_beginning(X) :- lps_program_module(M), M:real_time_beginning(X).
d_pre(X) :- lps_program_module(M), catch(M:d_pre(X),_,fail).
event(X) :- lps_program_module(M), catch(M:event(X),_,fail).
fluent(X) :- lps_program_module(M), catch(M:fluent(X),_,fail).
initial_state(X) :- lps_program_module(M), catch(M:initial_state(X),_,fail).
initiated(X,Y,Z) :- lps_program_module(M), catch(M:initiated(X,Y,Z),_,fail).
l_events(X,Y) :- lps_program_module(M), catch(M:l_events(X,Y),_,fail).
l_int(X,Y) :- lps_program_module(M), catch(M:l_int(X,Y),_,fail).
l_timeless(X,Y) :- lps_program_module(M), catch(M:l_timeless(X,Y),_,fail).
maxTime(X) :- lps_program_module(M), catch(M:maxTime(X),_,fail). % LPS execution/simulation cycles
maxRealTime(X) :- lps_program_module(M), catch(M:maxRealTime(X),_,fail). % real time (seconds, float)
minCycleTime(X) :- lps_program_module(M), catch(M:minCycleTime(X),_,fail). % real time (seconds, float); AVOID in SWISH!
observe(X,Y) :- lps_program_module(M), catch(M:observe(X,Y),_,fail).
option(X) :- lps_program_module(M), catch(M:option(X),_,fail).
reactive_rule(X,Y) :- lps_program_module(M), catch(M:reactive_rule(X,Y),_,fail).
reactive_rule(X,Y,Z) :- lps_program_module(M), catch(M:reactive_rule(X,Y,Z),_,fail).
terminated(X,Y,Z) :- lps_program_module(M), catch(M:terminated(X,Y,Z),_,fail).
updated(A,B,C,D) :- lps_program_module(M), catch(M:updated(A,B,C,D),_,fail).
lps_test_result_item(X,Y,Z) :- lps_program_module(M), catch(M:lps_test_result_item(X,Y,Z),_,fail).
lps_test_action_ancestor(X,Y,Z) :- lps_program_module(M), catch(M:lps_test_action_ancestor(X,Y,Z),_,fail).
events(X) :- lps_program_module(M), catch(M:events(X),_,fail).
fluents(X) :- lps_program_module(M), catch(M:fluents(X),_,fail).
actions(X) :- lps_program_module(M), catch(M:actions(X),_,fail).
unserializable(X) :- lps_program_module(M), catch(M:unserializable(X),_,fail).

lps_program_module(M) :- interpreter:lps_program_module(M).

% Explanation renderer
:- use_module(library(http/html_write)).
:- use_module(library(http/term_html)).

:- if(true). % exists_source(swish(lib/render))
:- user:ensure_loaded(('../swish/user_module_base')).
%:- use_module(/packs_web/swish/lib/render.plswish(lib/render)).
:- use_module('/opt/logicmoo_workspace/packs_web/swish/lib/render').
:- else.
:- use_module('../../swish/lib/render').
:- endif.

:- register_renderer(explanator, "Indented LPS explanation").
:- discontiguous(term_rendering/5).
term_rendering(lpsExplanation(HTML), _Vars, _Options) --> 
	html(div(['data-render'('As LPS Explanation')],[ div([],HTML) ])).

explanationNarrative([]) --> {!}.
explanationNarrative(Leaf) --> {atomic(Leaf), !}, html(li(Leaf)).
explanationNarrative([Node|Children]) --> {!}, explanationNarrative(Node), explanationNarrative(Children). 
explanationNarrative(Tree) --> {Tree=..[Node|Children]}, html(li([span(Node),ul( \explanationNarrative(Children) )])).
	
:- else.
% Now support for vanilla SWI Prolog
:- use_module('../engine/db.P',[real_time_beginning/1,
	action/1, d_pre/1 ,event/1,
	fluent/1,(initiated)/3, l_events/2, l_int/2, %  happens/3 redefined here
	l_timeless/2, (observe)/2, reactive_rule/2, reactive_rule/3, % state/1 redefined here
	(terminated)/3, updated/4, maxTime/1, maxRealTime/1, minCycleTime/1,
	option/1, 
	initial_state/1,
	lps_test_result_item/3, lps_test_action_ancestor/3,
	(events)/1, (fluents)/1, (actions)/1, (unserializable)/1]).
:- endif.

:- use_module('../engine/interpreter.P',[ callprolog/1, replace_term/4, check_lps_program_swish_module/0, check_load_postmortem/0,
	simulatedRealTimeBeginning/1, simulatedRealTimePerCycle/1]).
:- use_module('../utils/psyntax.P',[syntax2p/4,syntax2p_literal/7]).

:- thread_local(micro_state/2). % used to simulate serialized actions
micro_assert(F,T) :- asserta(micro_state(F,T)).
micro_retractall(F,T) :- retractall(micro_state(F,T)).
micro_clean(T) :- retractall(micro_state(_F,T)).

:- thread_local(reconstructed_fluent/2).

% Access the execution post mortem
state(F,T) :- micro_state(F,T), !.
state(F,T) :- reconstructed_fluent(F,T).
state(F,T) :- lps_test_result_item((fluents),T,F). % holds/2 for extensional fluents

happens(E,T1,T2) :- lps_test_result_item((events),T2,E), T1 is T2-1 ; lps_test_result_item(composites,T2,happens(E,T1,T2)).

% Setup LPS engine to do something with the window program source 
% (typically not executing it, because the user has already executed it)
% Load test (postmortem) data from our server cache
init_lps_swish :- 
	check_lps_program_swish_module, 
	catch(check_load_postmortem, must_execute_program_first, (print_message(informational,'Executing program first..'-[]),godc(_))),
	% reconstruct system fluents not part of the stored state:
	retractall(reconstructed_fluent(_,_)),
	((simulatedRealTimeBeginning(SB), parse_time(SB, SBNow), setof(T,F^state(F,T),Cycles)) -> 
		simulatedRealTimePerCycle(SCT),
		(member(Cycle,Cycles), RT is Cycle*SCT+SBNow, assert(reconstructed_fluent(real_time(RT),Cycle)), fail ; true)
		; true).

% for testing:
get_state(S,T) :- init_lps_swish, state(S,T).

:- thread_local(lazy/0). % basically an argument to w/4 etc., hidden for convenience
% set_lazy_mode(UselazyMode) In lazy mode the explanator returns less explanations, but many of these
% will be explanator calls: w(G,...), i(G,...), m(G,...). Default is false.
set_lazy_mode(true) :- retractall(lazy), assert(lazy).
set_lazy_mode(false) :- retractall(lazy).

% expandE(SubExplanationGoal,SubExpl,Expl)
expandE(G,_,G) :- lazy, !.
expandE(G,E,E) :- G.

/* OLD implementation:
expl_tree(Type,X,lpsExplanation(Tree)) :- 
	init_lps_swish, Node =..[Type,X,[],_,_], 
	generate_explanation_tree_relation(Node),
	retractall(explanation_tree_used(_)),
	explanation_tree(Node,Tree).
*/

% NEW implementation:
expl_tree(Type,X,lpsExplanation(HTML)) :- 
	init_lps_swish, Node =..[Type,X,[],_,_], 
	generate_explanation_tree_relation(Node),
	retractall(explanation_tree_used(_)),
	explanationTreeHTML(Node,HTML).

% explanationTreeHTML(+Node,-HTMLlist)
explanationTreeHTML([C1|Cn],CH) :- !, 
	explanationTreeHTML(C1,C1_), explanationTreeHTML(Cn,Cn_), append(C1_,Cn_,CH).
explanationTreeHTML([],[]) :- !.
explanationTreeHTML(Node,[]) :-
	node_type(Node,Type,X,_), 
	%TODO: explanation_tree_used is keeping useless term arguments
	explanation_tree_used(Node_), node_type(Node_,Type,X_,_), variant(X,X_),
	!.
explanationTreeHTML(Node,Tree) :- 
	%???? Used=..[Type,X], assert(explanation_tree_used(Used)),
	node_type(Node,Type,X,RelaxedNode),
	once( explanation_tree_relation(RelaxedNode,Label,Children) ), % pick the first, TODO: should pick the smallest
	assert(explanation_tree_used(Node)),
	% cf. colours in lps_corner/swish/web/lps/lps.css
	(X=happens(_,_,_) -> Style="color: #E19735;";
		X=holds(_,_) -> Style="color: #1A1A1A; background: #D7DCF5;";
		Type=clause -> Style="font-style: italic;";
		Style=""),
	Tree = [li(span([style=Style,title="~w"-[X]], "~a: ~w"-[Type,Label])) | UL],
	explanationTreeHTML(Children,CH),
	(CH==[]->UL=[];UL=[ul(CH)]).

% node_type(+Node,-Type,-Term,-RelaxedNode)
node_type(Node,Type,X,RelaxedNode) :- Node=..[Type,X,_,_,_], member(Type,[w,i,m]), !,RelaxedNode=..[Type,X,_,_,_].
node_type(calledBy-X,caller,X,calledBy-X) :- !.
node_type(X,clause,X,X).

node_type(Node,Type) :- node_type(Node,Type,_,_).

:- thread_local(explanation_tree_used/1). % Node; detect repeated subtrees
explanation_tree(Node,_Atom) :- explanation_tree_used(Node_), variant(Node,Node_), !,
	fail. % we really don't care to see repeated nodes
	% once((explanation_tree_relation(Node_,Label,_), variant(Node,Node_))), 
	% format(atom(Atom),'REPEATED: ~w',[Label]).
explanation_tree(Node,Tree) :- explanation_tree_relation(Node_,Label,Children), variant(Node,Node_), !,
	assert(explanation_tree_used(Node)),
	(Children=[] -> Tree=Label ; explanation_trees(Children,Trees), Tree=..[Label|Trees]).

explanation_trees([C1|Cn],Trees) :- !, 
	(explanation_tree(C1,T1) -> Trees=[T1|Tn] ; Trees=Tn), 
	explanation_trees(Cn,Tn).
explanation_trees([],[]).

% explanation_tree_relation(?Node,-Label,-Children)   
% Node will be either:
% - Type(+X,[],_,_), where Type=w/i/m
% - a LPS clause instance C, in internal syntax form
% - a calledBy-C term, meaning that LPS clause instance C "called" or selected the action or macro action being explained
:- thread_local(explanation_tree_relation/3).

% Build explanation relation
generate_explanation_tree_relation(Node) :- 
	retractall(explanation_tree_relation(_,_,_)), 
	set_lazy_mode(true),
	generate_explanation_tree_relation_(Node).

generate_explanation_tree_relation_(Repeated) :- 
	node_type(Repeated,Type,X,_),
	explanation_tree_relation(Node,_Label,_Children), node_type(Node,Type,XX,_),
	variant(XX,X), !.
generate_explanation_tree_relation_(Node) :- 
	tree_node(Node,Label,Children), 
	expand_children_lists(Children,Children_),
	assert(explanation_tree_relation(Node,Label,Children_)), 
	generate_explanation_trees_(Children_).

generate_explanation_trees_([C1|Cn]) :- !, generate_explanation_tree_relation_(C1), generate_explanation_trees_(Cn).
generate_explanation_trees_([]).

expand_children_lists([C1|Cn],Expanded) :- C1=..[Type,L,Ancestors,_,_], is_list(L), !,
	findall(C,(member(X,L), C=..[Type,X,Ancestors,_,_]), C1_),
	expand_children_lists(Cn,En),
	append(C1_,En,Expanded).
expand_children_lists([C1|Cn],[C1|En]) :- !, expand_children_lists(Cn,En).
expand_children_lists([],[]).





% tree_node(+Node,-Label,-Children)
tree_node(Node,Label,Children) :-  Node =..[Type,X,_Anc,Expl,_I], member(Type,[w,i,m]), !,
	pretty_explanation(X,PX), 
	%format(atom(Label),'~w: ~w',[Type,PX]),
	format(atom(Label),'~w',[PX]),
	findall(Expl,Node,Children_), 
	% avoid trivial goal conjunctions: TODO: missing treatment negated fluent goals?? perhaps not
	findall(Child,( member(C,Children_), (is_list(C)->member(Child,C);C=Child) ),Children). 
tree_node(calledBy-C,Label,[]) :- !, pretty_explanation(C,PC), format(atom(Label),'~w',[PC]). % format(atom(Label),'Caller: ~w',[PC]).
tree_node(C,Label,[]) :- !, pretty_explanation(C,PC), format(atom(Label),'~w',[PC]).

% Top level predicate:
expl_pretty(Type,X) :- 
	init_lps_swish, % setup LPS engine access to the program for SWISH; does nothing otherwise
	(Type==w -> write('Why '), G = w(X,Expl,I) ; write('Why not '), G = m(X,Expl,I)), 
	syntax2p_literal(NicerX,[],lps2p,_Interval,_ExplicitTime,_NiceColouring,X),
	write(NicerX), writeln('?'), nl,
	setof(I/Expl,G,Explanations), remove_variants(Explanations,Explanations_),
	member(_/E,Explanations_), 
	(E=calledBy-E_ -> write('Called by ') ; E=E_),
	%writeln(xxx-E_),
	pretty_explanation(E_,PE), writeln(PE),
	fail.
expl_pretty(_,_) :- nl.

w_pretty(X) :- expl_pretty(w,X).
m_pretty(X) :- expl_pretty(m,X).

w(G,E,I) :- init_lps_swish,w(G,[],E,I).
m(G,E,I) :- init_lps_swish,m(G,[],E,I).


% remove_variants(List,ListWithoutDuplicates)  ... where "no duplicate term" means "no variant terms"
remove_variants([X|L],NL) :- member(XX,L), variant(X,XX), !, remove_variants(L,NL).
remove_variants([X|L],[X|NL]) :- remove_variants(L,NL).
remove_variants([],[]).

pretty_explanation(E,PE) :- syntax2p(PE,[],lps2p,E), PE\=E, !.
pretty_explanation(E,PE) :- syntax2p_literal(PE,[],lps2p,[_,_], _ETL, _, E), PE\=E, !.
pretty_explanation(E,E).


% w(+Goal,+Ancestors,-Explanation,-Interval) Why is (ground) Fluent true. Or why is the answer "wrong". 
% For background on "wrong", "inadmissible" etc. see https://www.researchgate.net/publication/269696225_A_Framework_for_Declarative_Prolog_Debugging
% One answer for each immediate explanation. Interval is the minimal time interval [T1,T2] including all explanation nodes (unbound on lazy mode)
% This ASSUMES all fluent literals to be ground. When an explanation is found, others of similar origin are discarded
% Ancestors is a list w(X),i(Y),m(Z), only for basic actions, to detect loops due to preconditions involving multiple actions, e.g. 
% happens(switch(dad,kitchen,off),1,2) and happens(goto(dad,_9260),1,2) in examples/CLOUT_workshop/badlight.pl
% i(+G,+Expl): same as w, but restricted to the computation supporting the G call
/* Testing:
golps('/Users/mc/git/logicalcontracts/examples/CLOUT_workshop/RockPaperScissors.pl',[dc,make_test]).
setof(Expl,explanator:w(happens(send(miguel,1000),3,4),Expl),Explanations), member(E,Explanations), nl, write(E), fail.
w_pretty(happens(send(miguel,1000),3,4)).
*/
% w(G,_,_,_) :- writeln(w(G)), fail.
% fluents:
% too strong for meta predicates: w(X,_,_Ex,_I) :- \+ ground(X), !, throw(w_explanator_requires_ground(X)).
w(X,Anc,_,_) :- member(w(XX),Anc), variant(X,XX), !, fail. % repeated explanation
w(holds(findall(_X,G,_L),_T),Anc,Ex,I) :- !, 
	( findall(Expl-I,w(G,Anc,Expl,I),[E1|En]) -> member(Ex-I,[E1|En]) ; m(G,Anc,Ex,I)).
w(holds(not(FS),_T),A,Expl,I) :- is_list(FS), !, m(FS,A,Expl,I).
w(holds(not(F),T),A,Expl,I) :- !, m(holds(F,T),A,Expl,I).
w(holds(F,T),_,initial_state(IS),[1,1]) :- initial_state(IS), 
	member(F,IS), Prev is T-1, forall( between(1,Prev,TT), state(F,TT) ), % not terminated
	!.
% extensional fluent value, not declared initially:
w(holds(F,T),A,Expl,I) :- state(F,T), between(1,T,TT), Previous is T-TT, \+ state(F,Previous), Time is Previous+1,
	!, 
	% the present state originated in transition Previous,Previous+1; let's get the ocurred actions;
	% aligned with updateNextStateFluents/1 in interpreter.P:
    	(unserializable(UActions)->true;UActions=[]),
    	findall( happens(Ev, Previous, Time), (happens(Ev, Previous, Time), \+ \+ member(Ev,UActions)), UAs),
    	findall( happens(Ev, Previous, Time), (happens(Ev, Previous, Time), \+ member(Ev,UActions)), SAs),	
    	functor(F,Functor,Arity), 
    	% We start by the unserializable actions; 
    	( (member(Ev,UAs), initiated(Ev,F,Cond), expl_evaluate(Cond)) -> % happy with the first explanation
    		(Expl=initiated(Ev,F,Cond), Ev=happens(_,T1,T2), I=[T1,T2]; expandE(i(Ev,A,SubExpl,I),SubExpl,Expl); w(Cond,A,Expl,I)) 
		; % none found, let's consider other post conditions:
		(
			functor(OldF,Functor,Arity), 
			updated(Ev,OldF,Old-New,Cond), replace_term(OldF,Old,New,F), 
			member(Ev,UAs), state(OldF,Previous), expl_evaluate(Cond)
			) -> (Expl=updated(Ev,OldF,Old-New,Cond), Ev=happens(_,T1,T2),I=[T1,T2]; 
					expandE(i(Ev,A,SubExpl,I),SubExpl,Expl); w(Cond,A,Expl,I))
		; 
		% now let's consider serializable actions:
		% simulate (micro)state changes for relevant events, collecting all events/conditions, 
		% and at the end we'll clean all micro_state(_,Previous) changes
		findall(PostC/holds(TFl,Previous)/Ev/Cond, (member(Ev,SAs), (
			PostC=terminated(Ev, TFl, Cond), PostC, functor(TFl,Functor,Arity), expl_evaluate(Cond), state(TFl,Previous), micro_retractall(TFl,Previous);
    			PostC=initiated(Ev, Fl, Cond), PostC, functor(Fl,Functor,Arity), expl_evaluate(Cond), \+ state(Fl,Previous), micro_assert(Fl,Previous) ;
    			PostC=updated(Ev_,TFl,Old-New,Cond), PostC, functor(TFl,Functor,Arity), replace_term(TFl,Old,New,IFl), Ev=Ev_, state(TFl,Previous), 
    				expl_evaluate(Cond), micro_retractall(TFl,Previous), micro_assert(IFl,Previous)
    				)), Explanations),	
		(state(F,Previous) -> 
			micro_clean(Previous) 
			; /* this condition seems normal to me now... throw(weird_serializations-holds(F,T)-state(F,Previous))*/ true),
    		member(PostC/holds(TFl,Previous)/Ev/Cond, Explanations), 
    		(Expl=PostC, Ev=happens(_,T1,T2),I=[T1,T2] ; 
    				expandE(i(Ev,A,SubExpl,I),SubExpl,Expl) ; w(Cond,A,Expl,I) /* this should fail for intermediate micro states */; 
    				nonvar(TFl), expandE(w(holds(TFl,Previous),A,SubExpl,I),SubExpl,Expl))
    	).
% intensional fluents:
w(Fl,A,Expl,I) :- Fl=holds(_,T), !, l_int(Fl,Body), expl_evaluate(Body), !, 
	(Expl=l_int(Fl,Body), I=[T,T] ; Body\==[], expandE(w(Body,A,SubExpl,I),SubExpl,Expl)).
% events
w(Ev,A,Expl,I) :- Ev=happens(_,_,_), Ev, !, i(Ev,[w(Ev)|A],Expl,I). % basic action
w(Ev,A,Expl,I) :- Ev=happens(_,T1,T2), l_events(Ev,Body), % composite event
	expl_evaluate(Body), 
	!, 
	(Expl=l_events(Ev,Body), I=[T1,T2] ; Body\==[], expandE(w(Body,A,SubExpl,I),SubExpl,Expl)).
w([G|Gn],A,Expl,I) :- !, (w(G,A,Expl,I) ; Gn\==[], w(Gn,A,Expl,I)).
w(G,_A,timeless(G),[1,_] /*no specific interval*/) :- \+ is_list(G), \+ uninteresting_timeless(G).


% i(+G,-Expl) Explain "inadmissible" event G, e.g. why was G called during execution.
%  event seen as selected action:
% i(X,_,_,_):- writeln(i(X)), fail.
i(happens(E,_T1,T2),_A,observe(Events,T2),[T1,T2]) :- 
	observe(Events,T2), member(E,Events), !, T1 is T2-1.
i(Ev,Anc,Expl,I) :-  Ev=happens(_,T1,T2), 
	% find a macroaction caling action Ev:
	l_events(MacroA,Body), MacroA=happens(Ancestor,AT1,AT2), append(PartialC,[Ev|_],Body), 
	lps_test_action_ancestor(Ancestor,AT1,AT2), AT1=<T1, (nonvar(AT2)->T2=<AT2/*optimization*/;true), % get closest ancestor
	expl_evaluate(PartialC), expl_evaluate([Ev]), % got bindings from closest ancestor, and they're compatible
	!,
	(Expl=calledBy-l_events(MacroA,Body), I=[T1,T2] ; w(PartialC,Anc,Expl,I); expandE(i(MacroA,Anc,SubExpl,I),SubExpl,Expl); explain_preconditions(Ev,Anc,1,Expl,I)).
i(Ev,Anc,Expl,I) :- Ev=happens(_,T1,T2), !, 
	% find it in a rule consequent:
	reactive_rule(A,C), append(PartialC,[Ev|_],C), expl_evaluate(A), expl_evaluate(PartialC), !,
	(Expl=calledBy-reactive_rule(A,C),I=[T1,T2] ; PartialC\==[], expandE(w(PartialC,Anc,SubExpl,I),SubExpl,Expl) ; A\==[], expandE(w(A,Anc,SubExpl,I),SubExpl,Expl) ; 
		explain_preconditions(Ev,Anc,1,Expl,I)).

% explain_preconditions(+Action,+Ancestors,+Tmin,-Expl,-Interval)  explains why the action didn't occur earlier
% Action is a basic ground action, TMin delimits the relevant interval
% TODO: We could use a better Tmin (in the above calls to this predicate it is always 1...):
%  the last time of the antecedent triggering this goal; may be an extra argument, 
%  or rediscover it (for l_events; for reactive_rule just look at its A).
% this is part of the explanation for why an action did not happen earlier
% explain_violated_preconditions(A,Tmin,E,I) :- writeln(explain_violated_preconditions(A,Tmin,E,I)), fail. % for debugging
% Why didn't the action happen earlier?
explain_preconditions(happens(A,T1,_),Anc,Tmin,Expl,[T1_,T2_]) :- 
	d_pre(PreCond), select(happens(A,T1_,T2_),PreCond,Cond),
	between(Tmin,T1,T), T1_ is T1-T, T2_ is T1_+1, expl_evaluate(Cond),
	!, % consider only the most recent precondition violation
	(Expl=d_pre1(PreCond) ; w(Cond,Anc,Expl,_) ).
% No earlier precondition violations, so let's consider also why this action did not violate preconditions
explain_preconditions(happens(A,T1,T2),Anc,_Tmin,Expl,I) :- 
	d_pre(PreCond), select(happens(A,T1,T2),PreCond,Cond),
	\+ expl_evaluate(Cond), m(Cond,Anc,Expl,I).

% m(G,Ancestors,Expl,I)  Why is Go false? Or: "why is G failing?"
% floundering possible... hence the use of catch(expl_evaluate(..),..)
% m(X,_Anc,_,_) :- writeln(m(X)), fail.
% fluents:
% findall always succeeds!
m(holds(not(FS),_T),Anc,Expl,I) :- is_list(FS), !, w(FS,Anc,Expl,I).
m(holds(not(F),T),Anc,Expl,I) :- !, w(holds(F,T),Anc,Expl,I).
m(Fl,Anc,Expl,I) :- Fl=holds(F,T), functor(F,Functor,Arity), functor(Fl_,Functor,Arity), \+ l_int(Fl_,_), !, 
	% extensional fluent missing; 
	% let's assume micro states can NOT be explained!
	% some initiated or updated failed... or terminated succeeded...
	% for each T-1, T-2, ...:  TODO: probably need to prune this, too many!
	between(0,T,TT), Tx is T-TT, Previous is Tx-1, Ev=happens(_A,Previous,Tx), 
	( (terminated(Ev, F, Cond), Ev, catch(expl_evaluate(Cond),_,fail)) -> 
		(Expl = terminated(Ev, F, Cond), I=[Previous,Tx] ; w(Cond,Anc,Expl,I) ; expandE(i(Ev,Anc,SubExpl,I),SubExpl,Expl)) 
		;
		% our missing fluent may have been terminated by an update; need to bind vars used in cond first:
		(Ev, updated(Ev,TFl,Old-New,Cond), replace_term(TFl,Old,New,_), F=TFl, catch(expl_evaluate(Cond),_,fail) ) ->
			(Expl = updated(Ev,F,Old-New,Cond), I=[Previous,Tx] ; w(Cond,Anc,Expl,I) ; expandE(i(Ev,Anc,SubExpl,I),SubExpl,Expl))
			;
			(initiated(Ev, F, Cond), \+ expl_evaluate([Ev|Cond]), m([Ev|Cond],Anc,Expl,I))
	).
% (extensional) fluent present in initial state, perhaps we forgot this tuple:
m(holds(F,_),_Anc,initial_state(IS),[1,1]) :- functor(F,Functor,Arity), functor(FF,Functor,Arity),
	once((initial_state(IS), member(FF,IS), \+ member(F,IS))). 
% intensional fluents:
m(Fl,Anc,Expl,I) :- Fl=holds(F,T), !, 
	(\+l_int(Fl,_) -> Expl=no_intensional_rules_for(F), I=[T,T] ; l_int(Fl,Body), Body\==[], expandE(m(Body,Anc,SubExpl,I),SubExpl,Expl)).
% events/actions:
m(Ev,Anc,Expl,I) :- Ev=happens(E,T1,T2), functor(E,Functor,Arity), functor(E_,Functor,Arity), \+ l_events(happens(E_,_,_),_), !, 
	% Tons of possible reasons for a basic action to be absent... let's content ourselves with a violated precondition.
	% Other possibilities to consider...: 
	% 	if the same action A happened before or after, consider i(A); 
	%	failed subgoals in rule consequent;
	%	failed rule antecedents.
	% The following might be refactored with explain_violated_preconditions:
	d_pre(PreCond), select(happens(E,T1,T2),PreCond,Cond),
	catch(expl_evaluate(Cond),_,fail),
	!, 
	(Expl=d_pre(PreCond), I=[T1,T2] ; w(Cond,Anc,Expl,I) ).
m(Ev,Anc,Expl,I) :- Ev=happens(E,T1,T2), !, % composite event
	(\+l_events(Ev,_) -> Expl=no_macroaction_rules_for(E), I=[T1,T2] ; l_events(Ev,Body), Body\==[], expandE(m(Body,Anc,SubExpl,I),SubExpl,Expl)).
m([G|Gn],Anc,Expl,I) :- !, (\+ catch(expl_evaluate(G),_,fail) -> 
		m(G,Anc,Expl,I) ; 
		Gn\==[], expl_evaluate(G), (w(G,Anc,Expl,I); \+ expl_evaluate(Gn), m(Gn,Anc,Expl,I))
		). 
m(G,_Anc,timeless(G),[1,_] /*no specific interval*/) :- \+ is_list(G), \+ uninteresting_timeless(G). 
		
% Mostly redundant with evaluate/query in interpreter.P, but no time now to refactor modules to parametrize these
expl_evaluate([]).
expl_evaluate([P|Rest]) :-expl_query(P), expl_evaluate(Rest).

expl_query(holds(not(P),T)) :- !, \+ expl_query(holds(P,T)).
expl_query(holds(P,T)) :- expl_query_evaluate(P,T). % extensional
expl_query(holds(P, Now)) :- !, l_int(holds(P, Now), B), expl_evaluate(B).
expl_query(happens(P,T1,T2)) :- happens(P,T1,T2).
expl_query(happens(P,T1,T2)) :- !, l_events(happens(P,T1,T2),Body), expl_evaluate(Body).
expl_query(P) :-
    	(\+ l_timeless(P, _B) -> callprolog(P) ; l_timeless(P, B), expl_evaluate(B)).

expl_query_evaluate(findall(X,G,L),T) :- !, 
	(nonvar(T)->true; last_time(Last), 
	between(1,Last,T)), findall(X, expl_evaluate(G) ,L).
% TODO: we'll need to add "volatile states" to keep these in our postmortem schema... 
% plus postmortem files will no longer be reusable for the same source
expl_query_evaluate(real_time(_),_T) :- \+ simulatedRealTimeBeginning(_), !, throw(no_real_time_support_yet). 
expl_query_evaluate(P,T) :- state(P,T).

:- thread_local(last_time_cache/1).
last_time(T) :- last_time_cache(T), !.
last_time(T) :- state(_,T), \+ (state(_,Later), Later>T), !, assert(last_time_cache(T)).

% Declarations to unclutter the explanation from trivia:
uninteresting_timeless(_G).

% Ex: psyntax:golps('/Users/mc/git/lps_corner/examples/CLOUT_workshop/RockPaperScissorsBase.pl',[dc,make_test]), why(happens(pay(bob,2000),2,3)).
user:why(X) :- expl_pretty(w,X).
user:why(X,Tree) :- expl_tree(w,X,Tree). 
user:whynot(X) :- expl_pretty(m,X).
user:whynot(X,Tree) :- expl_tree(m,X,Tree). 

/* 
% for debugging:
user:whylist(X) :- 
	init_lps_swish, generate_explanation_tree_relation(w(X,[],_,_)), 
	writeln('Explanation relation:'), 
	explanation_tree_relation(Node,_Label,Children), writeln(Node/Children), fail.
user:whylist(_X) :- writeln('End of explanation.').
sandbox:safe_primitive(user:whylist(_X)).
countTree(T,1) :- (atomic(T);var(T)), !.
countTree(T,N) :- T=..[_|Children], countTrees(Children,NC), N is NC+1.
countTrees([C1|Cn],N) :- !, countTree(C1,N1), countTrees(Cn,Nn), N is Nn+N1.
countTrees([],0).
*/

:- if(current_module(swish)).
sandbox:safe_primitive(explanator:expl_tree(_,_,_)).
sandbox:safe_primitive(explanator:expl_pretty(_,_)).
sandbox:safe_primitive(explanator:w(_,_,_)).
sandbox:safe_primitive(explanator:m(_,_,_)).
sandbox:safe_primitive(explanator:w(_,_,_,_)).
sandbox:safe_primitive(explanator:m(_,_,_,_)).
sandbox:safe_primitive(explanator:get_state(_,_)).
:-endif.

