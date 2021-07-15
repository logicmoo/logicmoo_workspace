"======================================================================
|
|   Lisp interpreter written in Smalltalk
|
|
 ======================================================================"


"======================================================================
|
| Written by Aoki Atsushi and Nishihara Satoshi.
| Modified by Paolo Bonzini (removed GUI and compiler for subset of Smalltalk).
|
| This file is part of GNU Smalltalk.
|
| GNU Smalltalk is free software; you can redistribute it and/or modify it
| under the terms of the GNU General Public License as published by the Free
| Software Foundation; either version 2, or (at your option) any later version.
|
| GNU Smalltalk is distributed in the hope that it will be useful, but WITHOUT
| ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
| FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
| details.
|
| You should have received a copy of the GNU General Public License along with
| GNU Smalltalk; see the file COPYING.  If not, write to the Free Software
| Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
|
 ======================================================================"

SequenceableCollection subclass:  #LispList
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Examples-Lisp'!

LispList subclass:  #LispCons
	instanceVariableNames: 'head tail '
	classVariableNames: 'VerticalLevel HorizontalLevel '
	poolDictionaries: ''
	category: 'Examples-Lisp'!

LispList subclass:  #LispNil
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Examples-Lisp'!


!LispList class methodsFor: 'copyright'!

copyright
    ^'Copyright (C) 1995-1998 AOKI Atsushi, All Rights Reserved.'!

system
    ^'Goodies'!

version
    ^'003'! !

!LispList class methodsFor: 'instance creation'!

cell
    ^self subclassResponsibility!

head: headObject
    ^self subclassResponsibility!

head: headObject tail: tailObject
    ^self subclassResponsibility!

list: anArray
    "LispCons list: #(1 2 3 4)"

    | size list |
    size := anArray size.
    list := self null.
    size
	to: 1
	by: -1
	do: [:i | list := self head: (anArray at: i)
			tail: list].
    ^list!

new: anInteger
    "LispCons new: 5"

    | newList |
    newList := self null.
    anInteger timesRepeat: [newList := self head: self null tail: newList].
    ^newList!

null
    ^self subclassResponsibility!

with: anObject
    "LispCons with: 1"

    ^self head: anObject!

with: firstObject with: secondObject
    "LispCons with: 1 with: 2"

    ^self head: firstObject tail: (self with: secondObject)!

with: firstObject with: secondObject with: thirdObject
    "LispCons with: 1 with: 2 with: 3"

    ^self head: firstObject tail: (self with: secondObject with: thirdObject)!

with: firstObject with: secondObject with: thirdObject with: fourthObject
    "LispCons with: 1 with: 2 with: 3 with: 4"

    ^self head: firstObject tail: (self
	    with: secondObject
	    with: thirdObject
	    with: fourthObject)! !

!LispList methodsFor: 'accessing'!

at: indexInteger put: anObject
    ^self subscriptOutOfBoundsError: indexInteger!

size
    | tally |
    tally := 0.
    self do: [:each | tally := tally + 1].
    ^tally! !

!LispList methodsFor: 'private'!

subscriptOutOfBoundsError: index
    ^self error: 'subscript out of bounds: ' , index printString! !

!LispList methodsFor: 'testing'!

isCons
    ^self null not!

null
    ^false! !



!LispCons class methodsFor: 'class initialization'!

initialize
    "LispCons initialize."

    HorizontalLevel := VerticalLevel := nil! !

!LispCons class methodsFor: 'copyright'!

copyright
    ^'Copyright (C) 1995-1998 AOKI Atsushi, All Rights Reserved.'!

system
    ^'Goodies'!

version
    ^'003'! !

!LispCons class methodsFor: 'examples'!

example1
    "LispCons example1."

    | list |
    list := LispCons list: #(1 2 3 4 5 6 7 8 9 10 ).
    Transcript nl; show: list printString.
    ^list!

example2
    "LispCons example2."

    | null list |
    null := LispCons null.
    list := LispCons list: #(1 2 ).
    list := LispCons head: list tail: null.
    list := LispCons head: list tail: null.
    Transcript nl; show: list printString.
    ^list!

example3
    "LispCons example3."

    | x y z |
    x := LispCons list: #(1 2 3 ).
    y := LispCons list: #(4 5 6 ).
    z := LispCons list: #(1 2 3 4 5 6 ).
    Transcript nl; show: '(setq x ''(1 2 3)) => ' , x printString.
    Transcript nl; show: '(setq y ''(4 5 6)) => ' , y printString.
    Transcript nl; show: '(setq z ''(1 2 3 4 5 6)) => ' , z printString.
    Transcript nl; show: '(append x y) => ' , (x append: y) printString.
    Transcript nl; show: '(length z) => ' , z length printString.
    Transcript nl; show: '(member 3 z) => ' , (z member: 3) printString.
    Transcript nl; show: '(nth 4 z) => ' , (z nth: 4) printString.
    ^z!

example4
    "LispCons example4."

    | list |
    list := LispCons list: #(1 2 ).
    list := LispCons head: list tail: (LispCons list: #(3 4 )).
    list := LispCons head: list tail: (LispCons list: #(5 6 )).
    Transcript nl; show: list saveString.
    ^list!

example5
    "LispCons example5."

    | list |
    list := LispCons loadFrom: '
	    (PetriNet Aoki
		(Place p1 p2 p3 p4 p5)
		(Transition t1 t2 t3 t4 t5)
		(InputFunction
		    (t1 p1 p2 p3 p4 p5)
		    (t2 . p4)
		    (t3 . p5))
		(OutputFunction
		    (t1 p1 p2 p3 p4 p5)
		    (t2 . p4)
		    (t3 . p5))
		(Marking {#(1 2 3 4 5)})))'.
    Transcript nl; show: list saveString.
    ^list!

example6
    "LispCons example6."

    | list |
    list := LispCons loadFrom: '(aaa bbb ccc)'.
    Transcript nl; show: list saveString.
    ^list!

example7
    "LispCons example7."

    | list |
    list := LispCons loadFrom: ' `(`(1 2 `3) . `4 ) '.
    Transcript nl; show: list saveString.
    ^list! !

!LispCons class methodsFor: 'instance creation'!

cell
    ^super new head: self null tail: self null!

head: headObject
    ^super new head: headObject tail: self null!

head: headObject tail: tailObject
    ^super new head: headObject tail: tailObject!

list: anArray
    | size list |
    size := anArray size.
    list := self null.
    size
	to: 1
	by: -1
	do: [:i | list := self head: (anArray at: i)
			tail: list].
    ^list!

loadFrom: aStream
    "by nishis, 1998/04/19 07:51"

    | list |
    list := LispParser parse: aStream.
    ^list!

new
    ^self cell!

null
    ^LispNil null! !
