/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2013-2018, VU University Amsterdam
			      CWI, Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(mail_notify,
	  [ notify/2,			% +Object, +Term
	    msg_user//1			% +UUID
	  ]).
:- use_module(library(smtp)).
:- use_module(library(debug)).
:- use_module(library(error)).
:- use_module(library(http/http_host)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(pldoc/doc_html), [object_href/2]).
:- use_module(object_support).
:- use_module(openid).

:- multifile
	event_subject//1,			% +Event
	event_message//1.			% +Event

/** <module> Send notications by E-mail

This module sends E-mail notifications  to   _watchers_  for events that
take place on watched objects.  The   messages  themselves are generated
similar to print_message/2 using the grammars

  - mail_notify:event_subject//1
    Define the subject of the message
  - mail_notify:event_message//1
    Define the body of the message

@tbd	Eventually, this should also be used to provide an RSS feed from
	the side.
*/

%%	notify(+Object, +Term)
%
%	Notify watching users by mail of  the event on Object, described
%	by Term.

notify(Object, Term) :-
	server(_),			% check cache from calling thread
	notification_user(User),
	catch(thread_send_message(
		  mail_notifier,
		  notification(Object, User, Term)),
	      error(existence_error(message_queue, _),_),
	      start_notifier(Object, User, Term)).

%%	notification_user(-User) is det.
%
%	Find the origin of the event, which   is  either the UUID of the
%	logged on user that triggered the event,  the peer IP address of
%	this user or `'<not from http>'`.

notification_user(User) :-
	site_user_logged_in(User), !.
notification_user(Peer) :-
	http_current_request(Request),
	http_peer(Request, Peer), !.
notification_user('<not from http>').

%%	start_notifier(+Object, +User, +Term) isa det.
%
%	Start the notification thread of this is not already running.

start_notifier(Object, User, Term) :-
	thread_create(mail_notifier, _,
		      [ alias(mail_notifier),
			detached(true)
		      ]),
	thread_send_message(
	    mail_notifier,
	    notification(Object, User, Term)).

mail_notifier :-
	set_output(user_output),
	repeat,
	thread_get_message(Msg),
	catch(handle_message(Msg), E,
	      print_message(error, E)),
	fail.

handle_message(notification(Object, User, Term)) :- !,
	do_notify(Object, User, Term).
handle_message(Message) :-
	domain_error(notification, Message).

do_notify(Object, EventUser, Term) :-
	(   watcher(Object, Watcher),
	    (	site_user_property(Watcher, email(Email))
	    ->	User = Watcher
	    ;	site_user_property(User, email(Watcher))
	    ->	Email = Watcher
	    ;	Email = Watcher,
		User = unknown
	    ),
	    catch(notify(User, Email, Object, EventUser, Term),
		  E,
		  print_message(error, E)),
	    fail
	;   true
	).

notify(User, Email, Object, EventUser, Term) :-
	phrase(make_subject(Object, Term), SubjectList),
	phrase(make_message(User, Object, EventUser, Term), Message),
	with_output_to(atom(Subject),
		       send_message(SubjectList, current_output)),
	debug(notify, 'Sending mail to ~w about ~w', [Email, Object]),
	smtp_send_mail(Email,
		       send_message(Message),
		       [ subject(Subject),
			 from('noreply@swi-prolog.org')
		       ]), !.

%%	send_message(+Parts, +Output) is det.
%
%	Write  message  fragments  to  Output.    This   is  similar  to
%	print_message/2.

send_message([], _) :- !.
send_message([H|T], Out) :- !,
	send_one(H, Out),
	send_message(T, Out).

send_one(Fmt-Args, Out) :- !,
	format(Out, Fmt, Args).
send_one(nl, Out) :- !,
	format(Out, '~n', []).
send_one(X, _Out) :- !,
	domain_error(mail_message_fragment, X).

%%	make_subject(+Object, +Event)//
%
%	Generate the fragments that describe the   subject  for Event on
%	Object.

make_subject(Object, Event) -->
	{ object_label(Object, Label) },
	[ '[SWIPL] ~w: '-[Label] ],
	(   event_subject(Event)
	->  []
	;   ['<unknown event>'-[]]
	).

%%	make_message(+UUID, +Object, +User, +Event)//
%
%	Generate the fragments that describe the  message body for Event
%	on Object.

make_message(UUID, Object, User, Event) -->
	opening(UUID),
	on_object(Object),
	by_user(User),
	[nl],
	(   event_message(Event)
	->  []
	;   ['Unknown notication event: ~q'-[Event] ]
	),
	closing(UUID, Object).

opening(UUID) -->
	{ site_user_property(UUID, name(Name)) }, !,
	[ 'Dear ~w,'-[Name], nl, nl ].
opening(_) -->
	[ 'Hi'-[], nl, nl ].

on_object(Object) -->
	{ object_label(Object, Label),
	  object_href(Object, HREF),
	  server(Server)
	},
	[ 'This is a change notification for ~w'-[Label], nl,
	  'URL: ~w~w'-[Server, HREF], nl
	].

by_user(UUID) -->
	[ 'Event generated by '-[] ],
	msg_user(UUID), !,
	[nl].
by_user(_) -->
	[].

closing(UUID, _Object) -->
	{ site_user_property(UUID, _) }, !,
	[ nl, nl,
	  'You received this message because you have indicated to '-[], nl,
	  'watch this page on the SWI-Prolog website.'-[], nl,
	  'User details: '-[]
	],
	msg_user(UUID).
closing(_, _) --> [].


%%	server(-Server) is det.
%
%	Provide a URL for the server. Note  that the mail server runs in
%	a different thread and thus  the   HTTP  thread should call this
%	before launching the mail thread.

:- dynamic
	server_cache/1.

server(Server) :-
	server_cache(Server), !.
server(Server) :-
	ignore(http_current_request(Request)),
	http_current_host(Request, Host, Port, [global(true)]),
	(   Port == 80
	->  format(atom(Server), 'http://~w', [Host])
	;   Port == 443
	->  format(atom(Server), 'https://~w', [Host])
	;   format(atom(Server), 'http://~w:~w', [Host, Port])
	).


%%	msg_user(+UUID)// is det.
%
%	Plain-text reference to a user with hyperlink.

msg_user(UUID) -->
	{ site_user_property(UUID, name(Name)),
	  http_link_to_id(view_profile, [user(UUID)], HREF),
	  server(Server)
	},
	[ '~w <~w~w>'-[Name, Server, HREF] ].


		 /*******************************
		 *	    WATCH LIST		*
		 *******************************/

%%	watcher(+Object, -MailOrUser) is nondet.
%
%	True when Object is being watched by MailOrUser. Note that users
%	are described by their UUID,  and   thus  never  conflict with a
%	valid mail address.
%
%	@tbd:	Allow users to set watches

watcher(_, 'jan@swi-prolog.org').
