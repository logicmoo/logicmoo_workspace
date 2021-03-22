% From E: 
% 
% ':-'(call_pel_directive(translate(unskipped,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Book.lps.pl'))).
:- call_pel_directive(translate(unskipped,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Book.lps.pl')).
:-include(library('ec_planner/ec_test_incl')).
:-expects_dialect(lps).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',183).
% From E: 
% 
% ':-'(call_pel_directive(translate(begining,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Book.lps.pl'))).
:- call_pel_directive(translate(begining,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Book.lps.pl')).
% Sun, 21 Mar 2021 23:28:06 GMT File: <stream>(0x555567223300)%;
%; Copyright (c) 2005 IBM Corporation and others.
%; All rights reserved. This program and the accompanying materials
%; are made available under the terms of the Common Public License v1.0
%; which accompanies this distribution, and is available at
%; http://www.eclipse.org/legal/cpl-v10.html
%;
%; Contributors:
%; IBM - Initial implementation
%;
%; Book: book (a sort of device)
%;

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Book.e',14).
% sort page: integer
% From E: 
% 
% subsort(page,integer).
subsort(page, integer).
%; agent opens book to page.

% event BookOpenTo(agent,book,page)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Book.e',16).
% From E: 
% 
% event(bookOpenTo(agent,book,page)).
events([bookOpenTo/3]).
mpred_prop(bookOpenTo(agent, book, page), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Book.e',16).
actions([bookOpenTo/3]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Book.e',19).
%; agent closes book.

% event BookClose(agent,book)
% From E: 
% 
% event(bookClose(agent,book)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Book.e',19).
events([bookClose/2]).
mpred_prop(bookClose(agent, book), action).
actions([bookClose/2]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Book.e',22).
%; book is open to page.

% fluent BookIsOpenTo(book,page)
% From E: 
% 
% fluent(bookIsOpenTo(book,page)).
mpred_prop(bookIsOpenTo(book, page), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Book.e',22).
fluents([bookIsOpenTo/2]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Book.e',25).
% fluent BookClosed(book)
% From E: 
% 
% fluent(bookClosed(book)).
mpred_prop(bookClosed(book), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Book.e',25).
fluents([bookClosed/1]).

% noninertial BookClosed
% From E: 
% 
% ':-'(call_pel_directive(noninertial(bookClosed))).
:- call_pel_directive(noninertial(bookClosed)).
%; agent turns page of book to page.

% event BookTurnPageTo(agent,book,page)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Book.e',28).
% From E: 
% 
% event(bookTurnPageTo(agent,book,page)).
events([bookTurnPageTo/3]).
mpred_prop(bookTurnPageTo(agent, book, page), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Book.e',28).
actions([bookTurnPageTo/3]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Book.e',31).
% [book,page1,page2,time]
% HoldsAt(BookIsOpenTo(book,page1),time) &
% HoldsAt(BookIsOpenTo(book,page2),time) ->
% page1=page2.
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          bookIsOpenTo(Book,Page1), 
%          Time), 
%       holds(
%          bookIsOpenTo(Book,Page2), 
%          Time)), 
%    Page1=Page2).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Book.e',31).
 if(not(equals(Page1, Page2)),
       (not(bookIsOpenTo(Book, Page1));not(bookIsOpenTo(Book, Page2)))).


% [book,time]
% HoldsAt(BookClosed(book),time) <->
% ! {page} HoldsAt(BookIsOpenTo(book,page),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Book.e',37).
% From E: 
% 
% <->(
%    holds(
%       bookClosed(Book), 
%       Time), 
%    not(thereExists(Page, 
%           holds(
%              bookIsOpenTo(Book,Page), 
%              Time)))).
 %   [Time].
if(thereExists(Page, bookIsOpenTo(Book, Page)), not(bookClosed(Book))),
if(not(bookClosed(Book)), thereExists(Page, bookIsOpenTo(Book, Page))).


%; A precondition axiom states that
%; for an agent to open a book to a page,
%; the agent must be awake,
%; the book must be closed, and
%; the agent must be holding the book.
% [agent,book,page,time]
% Happens(BookOpenTo(agent,book,page),time) ->
% HoldsAt(Awake(agent),time) &
% HoldsAt(BookClosed(book),time) &
% HoldsAt(Holding(agent,book),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Book.e',45).
% From E: 
% 
% '->'(
%    happens(
%       bookOpenTo(Agent,Book,Page), 
%       Time), 
%    ','(
%       holds(
%          awake(Agent), 
%          Time), 
%       ','(
%          holds(
%             bookClosed(Book), 
%             Time), 
%          holds(
%             holding(Agent,Book), 
%             Time)))).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Book.e',45).
 if((not(awake(Agent));not(bookClosed(Book));not(holding(Agent, Book))),
      not(bookOpenTo(Agent, Book, Page))).


%; An effect axiom states that
%; if an agent opens a book to a page,
%; the book will be open to the page:
% [agent,book,page,time]
% Initiates(BookOpenTo(agent,book,page),BookIsOpenTo(book,page),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Book.e',54).
% From E: 
% 
% initiates_at(
%    bookOpenTo(Agent,Book,Page), 
%    bookIsOpenTo(Book,Page), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Book.e',54).
initiates(bookOpenTo(Agent,Book,Page),
	  bookIsOpenTo(Book,Page)).


%; A precondition axiom states that
%; for an agent to close a book,
%; the agent must be awake,
%; the book must not already be closed, and
%; the agent must be holding the book.
% [agent,book,time]
% Happens(BookClose(agent,book),time) ->
% HoldsAt(Awake(agent),time) &
% !HoldsAt(BookClosed(book),time) &
% HoldsAt(Holding(agent,book),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Book.e',62).
% From E: 
% 
% '->'(
%    happens(
%       bookClose(Agent,Book), 
%       Time), 
%    ','(
%       holds(
%          awake(Agent), 
%          Time), 
%       ','(
%          holds(
%             not(bookClosed(Book)), 
%             Time), 
%          holds(
%             holding(Agent,Book), 
%             Time)))).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Book.e',62).
 if((not(awake(Agent));bookClosed(Book);not(holding(Agent, Book))),
      not(bookClose(Agent, Book))).


%; An effect axiom states that
%; if an agent closes a book,
%; the book will no longer be open:
% [agent,book,page,time]
% Terminates(BookClose(agent,book),BookIsOpenTo(book,page),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Book.e',71).
% From E: 
% 
% terminates_at(
%    bookClose(Agent,Book), 
%    bookIsOpenTo(Book,Page), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Book.e',71).
terminates(bookClose(Agent,Book),
	   bookIsOpenTo(Book,Page)).


% [agent,book,page,time]
% Happens(BookTurnPageTo(agent,book,page),time) ->
% HoldsAt(Awake(agent),time) &
% ({page1} page1 != page & HoldsAt(BookIsOpenTo(book,page1),time)) &
% HoldsAt(Holding(agent,book),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Book.e',75).
% From E: 
% 
% '->'(
%    happens(
%       bookTurnPageTo(Agent,Book,Page), 
%       Time), 
%    ','(
%       holds(
%          awake(Agent), 
%          Time), 
%       ','(
%          thereExists(Page1, 
%             ','(
%                Page1\=Page, 
%                holds(
%                   bookIsOpenTo(Book,Page1), 
%                   Time))), 
%          holds(
%             holding(Agent,Book), 
%             Time)))).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Book.e',75).
if((not(awake(Agent));not(thereExists(Page1,  ({dif(Page1, Page)}, bookIsOpenTo(Book, Page1))));not(holding(Agent, Book))), not(bookTurnPageTo(Agent, Book, Page))).


% [agent,book,page,time]
% Initiates(BookTurnPageTo(agent,book,page),BookIsOpenTo(book,page),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Book.e',81).
% From E: 
% 
% initiates_at(
%    bookTurnPageTo(Agent,Book,Page), 
%    bookIsOpenTo(Book,Page), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Book.e',81).
initiates(bookTurnPageTo(Agent,Book,Page),
	  bookIsOpenTo(Book,Page)).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Book.e',83).
% [agent,book,page1,page2,time]
% HoldsAt(BookIsOpenTo(book,page1),time) &
% page1 != page2 ->
% Terminates(BookTurnPageTo(agent,book,page2),BookIsOpenTo(book,page1),time).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          bookIsOpenTo(Book,Page1), 
%          Time), 
%       Page1\=Page2), 
%    terminates_at(
%       bookTurnPageTo(Agent,Book,Page2), 
%       bookIsOpenTo(Book,Page1), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Book.e',83).
 if(not(terminates(bookTurnPageTo(Agent, Book, Page2),
                     at(bookIsOpenTo(Book, Page1), Time))),
       (not(holds(bookIsOpenTo(Book, Page1), Time));not({dif(Page1, Page2)}))).


%; End of file.
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Book.e',87).
% From E: 
% 
% ':-'(call_pel_directive(translate(ending,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Book.lps.pl'))).
:- call_pel_directive(translate(ending,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Book.lps.pl')).
