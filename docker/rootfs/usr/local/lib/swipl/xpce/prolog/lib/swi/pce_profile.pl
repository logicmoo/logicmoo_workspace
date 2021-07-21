/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org/packages/xpce/
    Copyright (c)  2003-2019, University of Amsterdam
                              VU University Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(pce_profile,
          [ pce_show_profile/0
          ]).
:- use_module(library(pce)).
:- use_module(library(lists)).
:- use_module(library(persistent_frame)).
:- use_module(library(toolbar)).
:- use_module(library(pce_report)).
:- use_module(library(tabular)).
:- use_module(library(prolog_predicate)).

:- require([ auto_call/1,
	     reset_profiler/0,
	     is_dict/1,
	     profile_data/1,
	     www_open_url/1,
	     pi_head/2,
	     predicate_label/2,
	     predicate_sort_key/2,
	     get_chain/3,
	     send_list/3
	   ]).

/** <module> GUI frontend for the profiler

This module hooks into profile/1 and  provides   a  graphical UI for the
profiler output.
*/

%!  pce_show_profile is det.
%
%   Show already collected profile using a graphical browser.

pce_show_profile :-
    profile_data(Data),
    in_pce_thread(show_profile(Data)).

show_profile(Data) :-
    send(new(F, prof_frame), open),
    send(F, wait),
    send(F, load_profile, Data).


                 /*******************************
                 *             FRAME            *
                 *******************************/

:- pce_begin_class(prof_frame, persistent_frame,
                   "Show Prolog profile data").

variable(samples,          int,  get, "Total # samples").
variable(ticks,            int,  get, "Total # ticks").
variable(accounting_ticks, int,  get, "# ticks while accounting").
variable(time,             real, get, "Total time").
variable(nodes,            int,  get, "Nodes created").
variable(time_view,        {percentage,seconds} := percentage,
                                 get, "How time is displayed").

class_variable(auto_reset, bool, @on, "Reset profiler after collecting").

initialise(F) :->
    send_super(F, initialise, 'SWI-Prolog profiler'),
    send(F, append, new(TD, tool_dialog(F))),
    send(new(B, prof_browser), left, new(prof_details)),
    send(B, below, TD),
    send(new(report_dialog), below, B),
    send(F, fill_dialog, TD).

fill_dialog(F, TD:tool_dialog) :->
    send(TD, append, new(File, popup(file))),
    send(TD, append, new(Sort, popup(sort))),
    send(TD, append, new(Time, popup(time))),
    send(TD, append, new(Help, popup(help))),
    send_list(File, append,
              [ menu_item(statistics,
                          message(F, show_statistics)),
                gap,
                menu_item(exit,
                          message(F, destroy))
              ]),
    forall(sort_by(Label, Field, Order),
           send(Sort, append,
                menu_item(Label, message(F, sort_by, Field, Order)))),
    get(F?class, instance_variable, time_view, TV),
    get(TV, type, Type),
    get_chain(Type, value_set, Values),
    forall(member(TimeView, Values),
           send(Time, append,
                menu_item(TimeView, message(F, time_view, TimeView)))),
    send_list(Help, append,
              [ menu_item(about,
                          message(F, about)),
                menu_item(help,
                          message(F, help))
              ]).


load_profile(F, ProfData0:[prolog]) :->
    "Load stored profile from the Prolog database"::
    (   is_dict(ProfData0)
    ->  ProfData = ProfData0
    ;   profile_data(ProfData)
    ),
    Summary = ProfData.summary,
    send(F, slot, samples, Summary.samples),
    send(F, slot, ticks, Summary.ticks),
    send(F, slot, accounting_ticks, Summary.accounting),
    send(F, slot, time, Summary.time),
    send(F, slot, nodes, Summary.nodes),
    get(F, member, prof_browser, B),
    send(F, report, progress, 'Loading profile data ...'),
    send(B, load_profile, ProfData.nodes),
    send(F, report, done),
    send(F, show_statistics),
    (   get(F, auto_reset, @on)
    ->  reset_profiler
    ;   true
    ).


show_statistics(F) :->
    "Show basic statistics on profile"::
    get(F, samples, Samples),
    get(F, ticks, Ticks),
    get(F, accounting_ticks, Account),
    get(F, time, Time),
    get(F, slot, nodes, Nodes),
    get(F, member, prof_browser, B),
    get(B?dict?members, size, Predicates),
    (   Ticks == 0
    ->  Distortion = 0.0
    ;   Distortion is 100.0*(Account/Ticks)
    ),
    send(F, report, inform,
         '%d samples in %.2f sec; %d predicates; \c
              %d nodes in call-graph; distortion %.0f%%',
         Samples, Time, Predicates, Nodes, Distortion).


details(F, From:prolog) :->
    "Show details on node or predicate"::
    get(F, member, prof_details, W),
    (   is_dict(From)
    ->  send(W, node, From)
    ;   get(F, member, prof_browser, B),
        get(B?dict, find,
            message(@arg1, has_predicate, prolog(From)),
            DI)
    ->  get(DI, data, Node),
        send(W, node, Node)
    ).

sort_by(F, SortBy:name, Order:[{normal,reverse}]) :->
    "Define the key for sorting the flat profile"::
    get(F, member, prof_browser, B),
    send(B, sort_by, SortBy, Order).

time_view(F, TV:name) :->
    send(F, slot, time_view, TV),
    get(F, member, prof_browser, B),
    get(F, member, prof_details, W),
    send(B, update_labels),
    send(W, refresh).

render_time(F, Ticks:int, Rendered:any) :<-
    "Render a time constant"::
    get(F, time_view, View),
    (   View == percentage
    ->  get(F, ticks, Total),
        get(F, accounting_ticks, Accounting),
        (   Total-Accounting =:= 0
        ->  Rendered = '0.0%'
        ;   Percentage is 100.0 * (Ticks/(Total-Accounting)),
            new(Rendered, string('%.1f%%', Percentage))
        )
    ;   View == seconds
    ->  get(F, ticks, Total),
        (   Total == 0
        ->  Rendered = '0.0 s.'
        ;   get(F, time, TotalTime),
            Time is TotalTime*(Ticks/float(Total)),
            new(Rendered, string('%.2f s.', Time))
        )
    ).

about(_F) :->
    send(@display, inform,
         'SWI-Prolog execution profile viewer\n\c
             By Jan Wielemaker').

help(_F) :->
    send(@display, confirm,
         'No online help yet\n\c
              The profiler is described in the SWI-Prolog Reference Manual\n\c
              available from www.swi-prolog.org\n\n\c
              Press OK to open the manual in your browser'),
    www_open_url('http://www.swi.psy.uva.nl/projects/SWI-Prolog/Manual/profile.html').

:- pce_end_class(prof_frame).


                 /*******************************
                 *     FLAT PROFILE BROWSER     *
                 *******************************/

:- pce_begin_class(prof_browser, browser,
                   "Show flat profile in browser").

class_variable(size, size, size(40,20)).

variable(sort_by,  name := ticks, get, "How the items are sorted").

initialise(B) :->
    send_super(B, initialise),
    send(B, update_label),
    send(B, select_message, message(@arg1, details)).

resize(B) :->
    get(B?visible, width, W),
    send(B, tab_stops, vector(W-80)),
    send_super(B, resize).

load_profile(B, Nodes:prolog) :->
    "Load stored profile from the Prolog database"::
    get(B, frame, Frame),
    get(B, sort_by, SortBy),
    forall(member(Node, Nodes),
           send(B, append, prof_dict_item(Node, SortBy, Frame))),
    send(B, sort).

update_label(B) :->
    get(B, sort_by, Sort),
    sort_by(Human, Sort, _How),
    send(B, label, Human?label_name).

sort_by(B, SortBy:name, Order:[{normal,reverse}]) :->
    "Define key on which to sort"::
    send(B, slot, sort_by, SortBy),
    send(B, update_label),
    send(B, sort, Order),
    send(B, update_labels).

sort(B, Order:[{normal,reverse}]) :->
    get(B, sort_by, Sort),
    (   Order == @default
    ->  sort_by(_, Sort, TheOrder)
    ;   TheOrder = Order
    ),
    send_super(B, sort, ?(@arg1, compare, @arg2, Sort, TheOrder)).

update_labels(B) :->
    "Update labels of predicates"::
    get(B, sort_by, SortBy),
    get(B, frame, F),
    send(B?dict, for_all, message(@arg1, update_label, SortBy, F)).

:- pce_end_class(prof_browser).

:- pce_begin_class(prof_dict_item, dict_item,
                   "Show entry of Prolog flat profile").

variable(data,         prolog, get, "Predicate data").

initialise(DI, Node:prolog, SortBy:name, F:prof_frame) :->
    "Create from predicate head"::
    send(DI, slot, data, Node),
    pce_predicate_label(Node.predicate, Key),
    send_super(DI, initialise, Key),
    send(DI, update_label, SortBy, F).

value(DI, Name:name, Value:prolog) :<-
    "Get associated value"::
    get(DI, data, Data),
    value(Name, Data, Value).

has_predicate(DI, Test:prolog) :->
    get(DI, data, Data),
    same_pred(Test, Data.predicate).

same_pred(X, X) :- !.
same_pred(QP1, QP2) :-
    unqualify(QP1, P1),
    unqualify(QP2, P2),
    same_pred_(P1, P2).

unqualify(user:X, X) :- !.
unqualify(X, X).

same_pred_(X, X) :- !.
same_pred_(Head, Name/Arity) :-
    pi_head(Name/Arity, Head).
same_pred_(Head, user:Name/Arity) :-
    pi_head(Name/Arity, Head).

compare(DI, DI2:prof_dict_item,
        SortBy:name, Order:{normal,reverse},
        Result:name) :<-
    "Compare two predicate items on given key"::
    get(DI, value, SortBy, K1),
    get(DI2, value, SortBy, K2),
    (   Order == normal
    ->  get(K1, compare, K2, Result)
    ;   get(K2, compare, K1, Result)
    ).

update_label(DI, SortBy:name, F:prof_frame) :->
    "Update label considering sort key and frame"::
    get(DI, key, Key),
    (   SortBy == name
    ->  send(DI, update_label, ticks_self, F)
    ;   get(DI, value, SortBy, Value),
        (   time_key(SortBy)
        ->  get(F, render_time, Value, Rendered)
        ;   Rendered = Value
        ),
        send(DI, label, string('%s\t%s', Key, Rendered))
    ).

time_key(ticks).
time_key(ticks_self).
time_key(ticks_children).

details(DI) :->
    "Show details"::
    get(DI, data, Data),
    send(DI?dict?browser?frame, details, Data).

:- pce_end_class(prof_dict_item).


                 /*******************************
                 *         DETAIL WINDOW        *
                 *******************************/

:- pce_begin_class(prof_details, window,
                   "Table showing profile details").

variable(tabular, tabular, get, "Displayed table").
variable(node,    prolog,  get, "Currently shown node").

initialise(W) :->
    send_super(W, initialise),
    send(W, pen, 0),
    send(W, label, 'Details'),
    send(W, background, colour(grey80)),
    send(W, scrollbars, vertical),
    send(W, display, new(T, tabular)),
    send(T, rules, all),
    send(T, cell_spacing, -1),
    send(W, slot, tabular, T).

resize(W) :->
    send_super(W, resize),
    get(W?visible, width, Width),
    send(W?tabular, table_width, Width-3).

title(W) :->
    "Show title-rows"::
    get(W, tabular, T),
    BG = (background := khaki1),
    send(T, append, 'Time',   bold, center, colspan := 2, BG),
    send(T, append, 'Port',   bold, center, colspan := 4, BG),
    send(T, append, 'Predicate', bold, center,
         valign := center, BG,
         rowspan := 2),
    send(T, next_row),
    send(T, append, 'Self',   bold, center, BG),
    send(T, append, 'Children',   bold, center, BG),
    send(T, append, 'Call',   bold, center, BG),
    send(T, append, 'Redo',   bold, center, BG),
    send(T, append, 'Exit',   bold, center, BG),
    send(T, append, 'Fail',   bold, center, BG),
    send(T, next_row).

cluster_title(W, Cycle:int) :->
    get(W, tabular, T),
    send(T, append, string('Cluster <%d>', Cycle),
         bold, center, colspan := 7,
         background := navyblue, colour := yellow),
    send(T, next_row).

refresh(W) :->
    "Refresh to accomodate visualisation change"::
    (   get(W, node, Data),
        Data \== @nil
    ->  send(W, node, Data)
    ;   true
    ).

node(W, Data:prolog) :->
    "Visualise a node"::
    send(W, slot, node, Data),
    send(W?tabular, clear),
    send(W, scroll_to, point(0,0)),
    send(W, title),
    clusters(Data.callers, CallersCycles),
    clusters(Data.callees, CalleesCycles),
    (   CallersCycles = [_]
    ->  show_clusters(CallersCycles, CalleesCycles, Data, 0, W)
    ;   show_clusters(CallersCycles, CalleesCycles, Data, 1, W)
    ).

show_clusters([], [], _, _, _) :- !.
show_clusters([P|PT], [C|CT], Data, Cycle, W) :-
    show_cluster(P, C, Data, Cycle, W),
    Next is Cycle+1,
    show_clusters(PT, CT, Data, Next, W).
show_clusters([P|PT], [], Data, Cycle, W) :-
    show_cluster(P, [], Data, Cycle, W),
    Next is Cycle+1,
    show_clusters(PT, [], Data, Next, W).
show_clusters([], [C|CT], Data, Cycle, W) :-
    show_cluster([], C, Data, Cycle, W),
    Next is Cycle+1,
    show_clusters([], CT, Data, Next, W).


show_cluster(Callers, Callees, Data, Cycle, W) :-
    (   Cycle == 0
    ->  true
    ;   send(W, cluster_title, Cycle)
    ),
    sort_relatives(Callers, Callers1),
    show_relatives(Callers1, parent, W),
    ticks(Callers1, Self, Children, Call, Redo, Exit),
    send(W, show_predicate, Data, Self, Children, Call, Redo, Exit),
    sort_relatives(Callees, Callees1),
    reverse(Callees1, Callees2),
    show_relatives(Callees2, child, W).

ticks(Callers, Self, Children, Call, Redo, Exit) :-
    ticks(Callers, 0, Self, 0, Children, 0, Call, 0, Redo, 0, Exit).

ticks([], Self, Self, Sibl, Sibl, Call, Call, Redo, Redo, Exit, Exit).
ticks([H|T],
      Self0, Self, Sibl0, Sibl, Call0, Call, Redo0, Redo, Exit0, Exit) :-
    arg(1, H, '<recursive>'),
    !,
    ticks(T, Self0, Self, Sibl0, Sibl, Call0, Call, Redo0, Redo, Exit0, Exit).
ticks([H|T], Self0, Self, Sibl0, Sibl, Call0, Call, Redo0, Redo, Exit0, Exit) :-
    arg(3, H, ThisSelf),
    arg(4, H, ThisSibings),
    arg(5, H, ThisCall),
    arg(6, H, ThisRedo),
    arg(7, H, ThisExit),
    Self1 is ThisSelf + Self0,
    Sibl1 is ThisSibings + Sibl0,
    Call1 is ThisCall + Call0,
    Redo1 is ThisRedo + Redo0,
    Exit1 is ThisExit + Exit0,
    ticks(T, Self1, Self, Sibl1, Sibl, Call1, Call, Redo1, Redo, Exit1, Exit).


%       clusters(+Relatives, -Cycles)
%
%       Organise the relatives by cluster.

clusters(Relatives, Cycles) :-
    clusters(Relatives, 0, Cycles).

clusters([], _, []).
clusters(R, C, [H|T]) :-
    cluster(R, C, H, T0),
    C2 is C + 1,
    clusters(T0, C2, T).

cluster([], _, [], []).
cluster([H|T0], C, [H|TC], R) :-
    arg(2, H, C),
    !,
    cluster(T0, C, TC, R).
cluster([H|T0], C, TC, [H|T]) :-
    cluster(T0, C, TC, T).

%       sort_relatives(+Relatives, -Sorted)
%
%       Sort relatives in ascending number of calls.

sort_relatives(List, Sorted) :-
    key_with_calls(List, Keyed),
    keysort(Keyed, KeySorted),
    unkey(KeySorted, Sorted).

key_with_calls([], []).
key_with_calls([H|T0], [0-H|T]) :-      % get recursive on top
    arg(1, H, '<recursive>'),
    !,
    key_with_calls(T0, T).
key_with_calls([H|T0], [K-H|T]) :-
    arg(4, H, Calls),
    arg(5, H, Redos),
    K is Calls+Redos,
    key_with_calls(T0, T).

unkey([], []).
unkey([_-H|T0], [H|T]) :-
    unkey(T0, T).

%       show_relatives(+Relatives, +Rolw, +Window)
%
%       Show list of relatives as table-rows.

show_relatives([], _, _) :- !.
show_relatives([H|T], Role, W) :-
    send(W, show_relative, H, Role),
    show_relatives(T, Role, W).

show_predicate(W, Data:prolog,
               Ticks:int, ChildTicks:int,
               Call:int, Redo:int, Exit:int) :->
    "Show the predicate we have details on"::
    Pred = Data.predicate,
    get(W, frame, Frame),
    get(Frame, render_time, Ticks, Self),
    get(Frame, render_time, ChildTicks, Children),
    get(W, tabular, T),
    BG = (background := khaki1),
    Fail is Call+Redo-Exit,
    send(T, append, Self, halign := right, BG),
    send(T, append, Children, halign := right, BG),
    send(T, append, Call, halign := right, BG),
    send(T, append, Redo, halign := right, BG),
    send(T, append, Exit, halign := right, BG),
    send(T, append, Fail, halign := right, BG),
    (   object(Pred)
    ->  new(Txt, prof_node_text(Pred, self))
    ;   new(Txt, prof_predicate_text(Pred, self))
    ),
    send(T, append, Txt, BG),
    send(W, label, string('Details -- %s', Txt?string)),
    send(T, next_row).

show_relative(W, Caller:prolog, Role:name) :->
    Caller = node(Pred, _Cluster, Ticks, ChildTicks, Calls, Redos, Exits),
    get(W, tabular, T),
    get(W, frame, Frame),
    (   Pred == '<recursive>'
    ->  send(T, append, new(graphical), colspan := 2),
        send(T, append, Calls, halign := right),
        send(T, append, new(graphical), colspan := 3),
        send(T, append, Pred, italic)
    ;   get(Frame, render_time, Ticks, Self),
        get(Frame, render_time, ChildTicks, Children),
        send(T, append, Self, halign := right),
        send(T, append, Children, halign := right),
        Fails is Calls+Redos-Exits,
        send(T, append, Calls, halign := right),
        send(T, append, Redos, halign := right),
        send(T, append, Exits, halign := right),
        send(T, append, Fails, halign := right),
        (   Pred == '<spontaneous>'
        ->  send(T, append, Pred, italic)
        ;   object(Pred)
        ->  send(T, append, prof_node_text(Pred, Role))
        ;   send(T, append, prof_predicate_text(Pred, Role))
        )
    ),
    send(T, next_row).


:- pce_end_class(prof_details).


:- pce_begin_class(prof_node_text, text,
                   "Show executable object").

variable(context,   any,                 get, "Represented executable").
variable(role,      {parent,self,child}, get, "Represented role").

initialise(T, Context:any, Role:{parent,self,child}, Cycle:[int]) :->
    send(T, slot, context, Context),
    send(T, slot, role, Role),
    get(T, label, Label),
    (   (   Cycle == 0
        ;   Cycle == @default
        )
    ->  TheLabel = Label
    ;   N is Cycle+1,               % people like counting from 1
        TheLabel = string('%s <%d>', Label, N)
    ),
    send_super(T, initialise, TheLabel),
    send(T, colour, blue),
    send(T, underline, @on),
    (   Role == self
    ->  send(T, font, bold)
    ;   true
    ).


label(T, Label:char_array) :<-
    get(T?context, print_name, Label).


:- free(@prof_node_text_recogniser).
:- pce_global(@prof_node_text_recogniser,
              make_prof_node_text_recogniser).

make_prof_node_text_recogniser(G) :-
    Text = @arg1,
    Pred = @arg1?context,
    new(P, popup),
    send_list(P, append,
              [ menu_item(details,
                          message(Text, details),
                          condition := Text?role \== self),
                menu_item(edit,
                          message(Pred, edit),
                          condition := Pred?source),
                menu_item(documentation,
                          message(Pred, help),
                          condition := message(Text, has_help))
              ]),
    new(C, click_gesture(left, '', single,
                         message(@receiver, details))),
    new(G, handler_group(C, popup_gesture(P))).


event(T, Ev:event) :->
    (   send_super(T, event, Ev)
    ->  true
    ;   send(@prof_node_text_recogniser, event, Ev)
    ).

has_help(T) :->
    get(T, context, Ctx),
    (   send(Ctx, instance_of, method) % hack
    ->  auto_call(manpce)
    ;   true
    ),
    send(Ctx, has_send_method, has_help),
    send(Ctx, has_help).

details(T) :->
    "Show details of clicked predicate"::
    get(T, context, Context),
    send(T?frame, details, Context).

:- pce_end_class(prof_node_text).


:- pce_begin_class(prof_predicate_text, prof_node_text,
                   "Show a predicate").

initialise(T, Pred:prolog, Role:{parent,self,child}, Cycle:[int]) :->
    send_super(T, initialise, prolog_predicate(Pred), Role, Cycle).

details(T) :->
    "Show details of clicked predicate"::
    get(T?context, pi, @on, Head),
    send(T?frame, details, Head).

:- pce_end_class(prof_predicate_text).


                 /*******************************
                 *              UTIL            *
                 *******************************/

value(name, Data, Name) :-
    !,
    predicate_sort_key(Data.predicate, Name).
value(label, Data, Label) :-
    !,
    pce_predicate_label(Data.predicate, Label).
value(ticks, Data, Ticks) :-
    !,
    Ticks is Data.ticks_self + Data.ticks_siblings.
value(Name, Data, Value) :-
    Value = Data.Name.

sort_by(cumulative_profile_by_time,          ticks,          reverse).
sort_by(flat_profile_by_time_self,           ticks_self,     reverse).
sort_by(cumulative_profile_by_time_children, ticks_siblings, reverse).
sort_by(flat_profile_by_number_of_calls,     call,           reverse).
sort_by(flat_profile_by_number_of_redos,     redo,           reverse).
sort_by(flat_profile_by_name,                name,           normal).


%!  pce_predicate_label(+PI, -Label)
%
%   Label is the human-readable identification   for Head. Calls the
%   hook user:prolog_predicate_name/2.

pce_predicate_label(Obj, Label) :-
    object(Obj),
    !,
    get(Obj, print_name, Label).
pce_predicate_label(PI, Label) :-
    predicate_label(PI, Label).
