;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; Book: book (a sort of device)
;

sort page: integer

; agent opens book to page.
event BookOpenTo(agent,book,page)

; agent closes book.
event BookClose(agent,book)

; book is open to page.
fluent BookIsOpenTo(book,page)

fluent BookClosed(book)
noninertial BookClosed

; agent turns page of book to page.
event BookTurnPageTo(agent,book,page)

[book,page1,page2,time]
HoldsAt(BookIsOpenTo(book,page1),time) &
HoldsAt(BookIsOpenTo(book,page2),time) ->
page1=page2.

[book,time]
HoldsAt(BookClosed(book),time) <->
! {page} HoldsAt(BookIsOpenTo(book,page),time).

; A precondition axiom states that
; for an agent to open a book to a page,
; the agent must be awake,
; the book must be closed, and
; the agent must be holding the book.
[agent,book,page,time]
Happens(BookOpenTo(agent,book,page),time) ->
HoldsAt(Awake(agent),time) &
HoldsAt(BookClosed(book),time) &
HoldsAt(Holding(agent,book),time).

; An effect axiom states that
; if an agent opens a book to a page,
; the book will be open to the page:
[agent,book,page,time]
Initiates(BookOpenTo(agent,book,page),BookIsOpenTo(book,page),time).

; A precondition axiom states that
; for an agent to close a book,
; the agent must be awake,
; the book must not already be closed, and
; the agent must be holding the book.
[agent,book,time]
Happens(BookClose(agent,book),time) ->
HoldsAt(Awake(agent),time) &
!HoldsAt(BookClosed(book),time) &
HoldsAt(Holding(agent,book),time).

; An effect axiom states that
; if an agent closes a book,
; the book will no longer be open:
[agent,book,page,time]
Terminates(BookClose(agent,book),BookIsOpenTo(book,page),time).

[agent,book,page,time]
Happens(BookTurnPageTo(agent,book,page),time) ->
HoldsAt(Awake(agent),time) &
({page1} page1 != page & HoldsAt(BookIsOpenTo(book,page1),time)) &
HoldsAt(Holding(agent,book),time).

[agent,book,page,time]
Initiates(BookTurnPageTo(agent,book,page),BookIsOpenTo(book,page),time).

[agent,book,page1,page2,time]
HoldsAt(BookIsOpenTo(book,page1),time) &
page1 != page2 ->
Terminates(BookTurnPageTo(agent,book,page2),BookIsOpenTo(book,page1),time).

; End of file.
