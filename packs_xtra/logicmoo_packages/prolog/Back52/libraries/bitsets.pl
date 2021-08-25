/******************************************************************************
    This file is being distributed, by written permission of Quintus 
    Corporation, for use with the BACK system only.  This file may not
    be used with Prolog implementations other than Quintus Prolog except
    (a) as part of the BACK system, or (b) with the written permission
    of Quintus Corporation.  To obtain such written permission, please
    contact:

	Quintus Corporation
	2100 Geng Road
	Palo Alto,
	California  94303
	USA
	415-813-3800
	marketing@quintus.com
******************************************************************************/

%   Package: bitsets
%   Author : Richard A. O'Keefe
%   Updated: 10/30/91
%   Updated: 1/30/90 with fixes by F. Pereira
%   Defines: Operations on sets of integers (bitsets)

%   Copyright (C) 1989, Quintus Computer Systems, Inc.  All rights reserved.

:- module(bitsets, [
	is_bitset/1,
	bitset_ordset/2,
	bitset_to_list/2,
	list_to_bitset/2,
	bit_add_element/3,
    	bit_card/2,
	bit_del_element/3,
	bit_disjoint/2,
	bit_intersect/2,
	bit_intersection/2,
	bit_intersection/3,
	bit_member/2,
	bit_memberchk/2,
	bit_seteq/2,
	bit_subtract/3,
	bit_symdiff/3,
	bit_union/2,
	bit_union/3
   ]).
:- mode
	is_bitset(?),
	bit_add_element(+, +, ?),
	    bit_add_element_1(+, +, ?),
		bit_add_element_1(+, +, ?, +,+,+,+,+),
	    bit_add_element_2(+, +, ?, +),
		bit_add_element_2(+, +, ?, +, +),
    	bit_card(+, ?),
	    bit_card_1(+, +, +, +, ?),
    	    	bit_card_2(+, +, +, +, ?),
	bit_del_element(+, +, ?),
	    bit_del_element_1(+, +, ?),
		bit_del_element_1(+, +, ?, +,+,+,+,+),
	bit_disjoint(+, +),
	    bit_disjoint_few(+, +),
	    bit_disjoint_many(+, +,+,+,+,+),
	bit_intersect(+, +),
	bit_intersection(+, ?),
	    bit_intersection_1(+, +, ?),
	bit_intersection(+, +, ?),
	    bit_intersection_few(+, +, ?),
	    bit_intersection_many(+, +,+,+,+,+, ?),
		pack_bit_set(+, +, +, +, +, ?),
	bit_memberchk(+, +),
	    bit_memberchk_1(+, +),
	bit_member(?, +),
	    bit_member_1(+, -),
	bit_seteq(?, ?),
	bit_subset(+, +),
	    bit_subset_few(+, +),
	bit_subtract(+, +, ?),
	    bit_subtract_few(+, +, ?),
	    bit_subtract_many(+, +,+,+,+,+, ?),
	bit_symdiff(+, +, ?),
	    bit_symdiff_few(+, +, ?),
	    bit_symdiff_many(+, +,+,+,+,+, ?),
	bit_union(+, ?),
	    bit_union_1(+, +, ?),
	bit_union(+, +, ?),
	    bit_union_few(+, +, ?),
	    bit_union_many(+, +,+,+,+,+, ?),
	list_to_bitset(+, ?),
	    list_to_bitset(+, +, +, +, +, ?),
	    partition_bits(+, +, -, -, -, -, -),
		partition_bits(+, +, -, -, -, -, -, +, +).

sccs_id('"@(#)91/10/30 bitsets.pl	65.1"').

/*  This package was inspired by Mats Carlsson's file "isets.pl",
    which he posted to the net on 13-Dec-88, and was written shortly after.
    Carlsson's representation is
	[]		represents {}
	$		represents {0}
	$(A,B,C,D)	represents {4i+0 | i .in. A} U {4i+1 | i .in. B}
				 U {4i+2 | i .in. C} U {4i+3 | i .in. D}
    His representation can represent sets of non-negative integers.  With
    no disrespect intended, I wanted to see whether it was possible to
    produce a representation which was a wee bit closer to bit sets.  As
    Carlsson's representation is essentially identical to the Warren &
    Pereira 1+4 tree representation for arrays, the obvious thing to try
    was another variation on 1+4 trees.  The representation I chose is

	few(M)		represents {i | (1<<i)/\Mask =\= 0}
	many(M,A,B,C,D)	represents {i | (1<<i)/\Mask =\= 0}
				U {4i+29 | i .in. A} U {4i+30 | i .in. B}
				U {4i+31 | i .in. C} U {4i+32 | i .in. D}

    The magic number 29 is the number of bits we can cheaply fit into an
    integer.

    All of the operations in library(ordsets) except ord_setproduct/3 have
    analogues in this file.
*/


%   is_bitset(+Set)
%   is true when Set is a ground bitset representation.

is_bitset(few(M)) :- !,		% also catches variables
	integer(M).		% also rejects variables
is_bitset(many(M,A,B,C,D)) :-
	integer(M),
	is_bitset(A),
	is_bitset(B),
	is_bitset(C),
	is_bitset(D).



%   bit_add_element(+Set1, +BitNo, ?Set2)
%   is the equivalent of add_element/3 for bit sets, that is, it is true
%   when Set2 represents Set1 U {BitNo}.

bit_add_element(Set1, BitNo, Set2) :-
	integer(BitNo),
	BitNo >= 0,
	bit_add_element_1(Set1, BitNo, Set2).


%.  bit_add_element_1(+Set1, +BitNo, ?Set2)
%   adds BitNo to Set1 giving Set2, when it is known that BitNo is valid.

bit_add_element_1(few(M), BitNo, Set2) :-
	(   BitNo < 29 ->
	    M1 is (1 << BitNo) \/ M,
	    Set2 = few(M1)
	;   bit_add_element_2(M, BitNo, Set2, few(0))
	).
bit_add_element_1(many(M,A,B,C,D), BitNo, Set2) :-
	(   BitNo < 29 ->
	    M1 is (1 << BitNo) \/ M,
	    Set2 = many(M1,A,B,C,D)
	;   BitNo1 is (BitNo-29) >> 2,
	    Case is (BitNo-29) /\ 3,
	    bit_add_element_1(Case, BitNo1, Set2, M, A, B, C, D)
	).


bit_add_element_1(0, BitNo, many(M,A1,B,C,D), M, A, B, C, D) :-
	bit_add_element_1(A, BitNo, A1).
bit_add_element_1(1, BitNo, many(M,A,B1,C,D), M, A, B, C, D) :-
	bit_add_element_1(B, BitNo, B1).
bit_add_element_1(2, BitNo, many(M,A,B,C1,D), M, A, B, C, D) :-
	bit_add_element_1(C, BitNo, C1).
bit_add_element_1(3, BitNo, many(M,A,B,C,D1), M, A, B, C, D) :-
	bit_add_element_1(D, BitNo, D1).

bit_add_element_2(Mask0, BitNo, Set, Few0) :-
	(   BitNo < 29 ->
	    Mask is (1 << BitNo) \/ Mask0,
	    Set = few(Mask)
	;   BitNo1 is (BitNo-29) >> 2,
	    Case is (BitNo-29) /\ 3,
	    bit_add_element_2(Case, BitNo1, Set, Few0, Mask0)
	).


bit_add_element_2(0, BitNo, many(M,A,F,F,F), F, M) :-
	bit_add_element_2(0, BitNo, A, F).
bit_add_element_2(1, BitNo, many(M,F,B,F,F), F, M) :-
	bit_add_element_2(0, BitNo, B, F).
bit_add_element_2(2, BitNo, many(M,F,F,C,F), F, M) :-
	bit_add_element_2(0, BitNo, C, F).
bit_add_element_2(3, BitNo, many(M,F,F,F,D), F, M) :-
	bit_add_element_2(0, BitNo, D, F).



%   bit_del_element(+Set1, +BitNo, ?Set2)
%   is the equivalent of del_element/3 for bitsets.  That is, it is true
%   when BitNo is a non-negative integer and Set2 is Set1\{BitNo}.

bit_del_element(Set1, BitNo, Set2) :-
	integer(BitNo),
	BitNo >= 0,
	bit_del_element_1(Set1, BitNo, Set2).


bit_del_element_1(few(M), BitNo, few(M1)) :-
	( BitNo < 29 -> M1 is \(1 << BitNo) /\ M ; M1 = M ).
bit_del_element_1(many(M,A,B,C,D), BitNo, Set2) :-
	(   BitNo < 29 ->
	    M1 is \(1 << BitNo) /\ M,
	    pack_bit_set(A, B, C, D, M1, Set2)
	;   BitNo1 is (BitNo-29) >> 2,
	    Case is (BitNo-29) /\ 3,
	    bit_del_element_1(Case, BitNo1, Set2, M, A, B, C, D)
	).


bit_del_element_1(0, BitNo, Set, M, A, B, C, D) :-
	bit_del_element(A, BitNo, A1),
	pack_bit_set(A1, B, C, D, M, Set).
bit_del_element_1(1, BitNo, Set, M, A, B, C, D) :-
	bit_del_element(B, BitNo, B1),
	pack_bit_set(A, B1, C, D, M, Set).
bit_del_element_1(2, BitNo, Set, M, A, B, C, D) :-
	bit_del_element(C, BitNo, C1),
	pack_bit_set(A, B, C1, D, M, Set).
bit_del_element_1(3, BitNo, Set, M, A, B, C, D) :-
	bit_del_element(D, BitNo, D1),
	pack_bit_set(A, B, C, D1, M, Set).



%   bit_disjoint(+Set1, +Set2)
%   is true when bitsets Set1 and Set2 have no elemenet in common.

bit_disjoint(few(M1), S2) :-
	bit_disjoint_few(S2, M1).
bit_disjoint(many(M1,A1,B1,C1,D1), S2) :-
	bit_disjoint_many(S2, M1,A1,B1,C1,D1).


bit_disjoint_few(few(M2), M1) :-
	M1 /\ M2 =:= 0.
bit_disjoint_few(many(M2,_,_,_,_), M1) :-
	M1 /\ M2 =:= 0.


bit_disjoint_many(few(M2), M1,_,_,_,_) :-
	M1 /\ M2 =:= 0.
bit_disjoint_many(many(M2,A2,B2,C2,D2), M1,A1,B1,C1,D1) :-
	M1 /\ M2 =:= 0,
	bit_disjoint(A1, A2),
	bit_disjoint(B1, B2),
	bit_disjoint(C1, C2),
	bit_disjoint(D1, D2).



%   bit_intersect(+Set1, +Set2)
%   is true when bitsets Set1, Set2 have at least one element in common.

bit_intersect(Set1, Set2) :-
	\+ bit_disjoint(Set1, Set2).



%   bit_intersection(+Set1, +Set2, ?Intersection)
%   is true when Intersection is the intersection of bitsets Set1 and Set2.

bit_intersection(few(M1), S2, Intersection) :-
	bit_intersection_few(S2, M1, Intersection).
bit_intersection(many(M1,A1,B1,C1,D1), S2, Intersection) :-
	bit_intersection_many(S2, M1, A1, B1, C1, D1, Intersection).


bit_intersection_few(few(M2), M1, few(M3)) :-
	M3 is M1 /\ M2.
bit_intersection_few(many(M2,_,_,_,_), M1, few(M3)) :-
	M3 is M1 /\ M2.


bit_intersection_many(few(M2), M1,_,_,_,_, few(M3)) :-
	M3 is M1 /\ M2.
bit_intersection_many(many(M2,A2,B2,C2,D2), M1,A1,B1,C1,D1, Intersection) :-
	M3 is M1 /\ M2,
	bit_intersection(A1, A2, A3),
	bit_intersection(B1, B2, B3),
	bit_intersection(C1, C2, C3),
	bit_intersection(D1, D2, D3),
	pack_bit_set(A3, B3, C3, D3, M3, Intersection).


pack_bit_set(few(0), few(0), few(0), few(0), Mask, BitSet) :- !,
	BitSet = few(Mask).
pack_bit_set(A, B, C, D, M, many(M,A,B,C,D)).



%   bit_intersection(ListOfBitSets, Intersection)
%   is true when ListOfBitSets is a list of bitsets, and Intersection is
%   their intersection.  As usual, ListOfSets must be a non-empty proper
%   list; the intersection of the empty list of sets is the universe!

bit_intersection([BitSet|BitSets], Intersection) :-
	bit_intersection_1(BitSets, BitSet, Intersection).

bit_intersection_1([], Intersection, Intersection).
bit_intersection_1([BitSet|BitSets], Intersection0, Intersection) :-
	bit_intersection(BitSet, Intersection0, Intersection1),
	bit_intersection_1(BitSets, Intersection1, Intersection).


%   bit_seteq(+Set1, +Set2)
%   is true when Set1 and Set2 represent the same bitset.

bit_seteq(BitSet, BitSet).



%   bit_subset(+Set1, +Set2)
%   is true when every element of the bitset Set1 appears in the bitset Set2.

bit_subset(few(M), Set2) :-
	bit_subset_few(Set2, M).
bit_subset(many(M1,A1,B1,C1,D1), many(M2,A2,B2,C2,D2)) :-
	\(M2)/\M1 =:= 0,
	bit_subset(A1, A2),
	bit_subset(B1, B2),
	bit_subset(C1, C2),
	bit_subset(D1, D2).


bit_subset_few(few(M2), M1) :-
	\(M2)/\M1 =:= 0.
bit_subset_many(many(M2,_,_,_,_), M1) :-
	\(M2)/\M1 =:= 0.



%   bit_subtract(+Set1, +Set2, ?Difference)
%   is true when Difference represents Set1\Set2, all being bitsets.

bit_subtract(few(M1), S2, Difference) :-
	bit_subtract_few(S2, M1, Difference).
bit_subtract(many(M1,A1,B1,C1,D1), S2, Difference) :-
	bit_subtract_many(S2, M1, A1, B1, C1, D1, Difference).


bit_subtract_few(few(M2), M1, few(M3)) :-
	M3 is \(M2) /\ M1.
bit_subtract_few(many(M2,_,_,_,_), M1, few(M3)) :-
	M3 is \(M2) /\ M1.


bit_subtract_many(few(M2), M1,A1,B1,C1,D1, many(M3,A1,B1,C1,D1)) :-
	M3 is \(M2) /\ M1.
bit_subtract_many(many(M2,A2,B2,C2,D2), M1,A1,B1,C1,D1, Difference) :-
	M3 is \(M2) /\ M1,
	bit_subtract(A1, A2, A3),
	bit_subtract(B1, B2, B3),
	bit_subtract(C1, C2, C3),
	bit_subtract(D1, D2, D3),
	pack_bit_set(A3, B3, C3, D3, M3, Difference).



%   bit_symdiff(+Set1, +Set2, ?Difference)
%   is true when Difference is the symmetric difference of Set1 and
%   Set2, all three of them bitsets.

bit_symdiff(few(M1), S2, Difference) :-
	bit_symdiff_few(S2, M1, Difference).
bit_symdiff(many(M1,A1,B1,C1,D1), S2, Difference) :-
	bit_symdiff_many(S2, M1, A1, B1, C1, D1, Difference).


bit_symdiff_few(few(M2), M1, few(M3)) :-
	M3 is (\(M2) /\ M1) \/ (\(M1) /\ M2).
bit_symdiff_few(many(M2,_,_,_,_), M1, few(M3)) :-
	M3 is (\(M2) /\ M1) \/ (\(M1) /\ M2).


bit_symdiff_many(few(M2), M1,A1,B1,C1,D1, many(M3,A1,B1,C1,D1)) :-
	M3 is (\(M2) /\ M1) \/ (\(M1) /\ M2).
bit_symdiff_many(many(M2,A2,B2,C2,D2), M1,A1,B1,C1,D1, Difference) :-
	M3 is (\(M2) /\ M1) \/ (\(M1) /\ M2),
	bit_symdiff(A1, A2, A3),
	bit_symdiff(B1, B2, B3),
	bit_symdiff(C1, C2, C3),
	bit_symdiff(D1, D2, D3),
	pack_bit_set(A3, B3, C3, D3, M3, Difference).



%   bit_union(+Set1, +Set2, ?Union)
%   is true when Union is the union of bitsets Set1 and Set2.

bit_union(few(M1), S2, Union) :-
	bit_union_few(S2, M1, Union).
bit_union(many(M1,A1,B1,C1,D1), S2, Union) :-
	bit_union_many(S2, M1, A1, B1, C1, D1, Union).


bit_union_few(few(M2), M1, few(M3)) :-
	M3 is M1 \/ M2.
bit_union_few(many(M2,A2,B2,C2,D2), M1, many(M3,A2,B2,C2,D2)) :-
	M3 is M1 \/ M2.


bit_union_many(few(M2), M1,A1,B1,C1,D1, many(M3,A1,B1,C1,D1)) :-
	M3 is M1 \/ M2.
bit_union_many(many(M2,A2,B2,C2,D2), M1,A1,B1,C1,D1, many(M3,A3,B3,C3,D3)) :-
	M3 is M1 \/ M2,
	bit_union(A1, A2, A3),
	bit_union(B1, B2, B3),
	bit_union(C1, C2, C3),
	bit_union(D1, D2, D3).



%   bit_union(ListOfBitSets, Union)
%   is true when ListOfBitSets is a list of bitsets, and Union is
%   their union.  We could use the logarithmetic method for this,
%   but I shall wait until I've timed both alternatives.

bit_union(BitSets, Union) :-
	bit_union_1(BitSets, few(0), Union).

bit_union_1([], Union, Union).
bit_union_1([BitSet|BitSets], Union0, Union) :-
	bit_union(BitSet, Union0, Union1),
	bit_union_1(BitSets, Union1, Union).


%   list_to_bitset(+Numbers, ?BitSet)
%   is true when Numbers is a ground list of non-negative integers,
%   BitSet is a bitset, and the two represent the same set of integers.
%   It may be used to compute or to check BitSet, but cannot be used to
%   compute Numbers.

list_to_bitset(Numbers, BitSet) :-
	partition_bits(Numbers, 0, M, As, Bs, Cs, Ds),
	list_to_bitset(As, Bs, Cs, Ds, M, BitSet).


list_to_bitset([], [], [], [], M, BitSet) :- !,
	BitSet = few(M).
list_to_bitset(As, Bs, Cs, Ds, M, many(M,A,B,C,D)) :-
	list_to_bitset(As, A),
	list_to_bitset(Bs, B),
	list_to_bitset(Cs, C),
	list_to_bitset(Ds, D).


partition_bits([], M, M, [], [], [], []).
partition_bits([Number|Numbers], M0, M, As, Bs, Cs, Ds) :-
	(   Number < 29 ->
	    Number >= 0,
	    M1 is (1 << Number) \/ M0,
	    partition_bits(Numbers, M1, M, As, Bs, Cs, Ds)
	;   Case is (Number-29)/\3,
	    Num is  (Number-29) >> 2,
	    partition_bits(Case, M0, M, As, Bs, Cs, Ds, Num, Numbers)
	).


partition_bits(0, M0, M, [N|As], Bs, Cs, Ds, N, Numbers) :-
	partition_bits(Numbers, M0, M, As, Bs, Cs, Ds).
partition_bits(1, M0, M, As, [N|Bs], Cs, Ds, N, Numbers) :-
	partition_bits(Numbers, M0, M, As, Bs, Cs, Ds).
partition_bits(2, M0, M, As, Bs, [N|Cs], Ds, N, Numbers) :-
	partition_bits(Numbers, M0, M, As, Bs, Cs, Ds).
partition_bits(3, M0, M, As, Bs, Cs, [N|Ds], N, Numbers) :-
	partition_bits(Numbers, M0, M, As, Bs, Cs, Ds).



%   bitset_to_list(+BitSet, ?Numbers)
%   converts a bitset to a list of integers.  The order of the numbers in
%   the list is NOT standard order (not currently, anyway).

bitset_to_list(BitSet, Numbers) :-
	bitset_to_list(BitSet, 0, 0, Numbers, []).

%.  bitset_to_list(+BitSet, +Shift, +Offset, ?Numbers0, Numbers)
%   computes Numbers = [(i << Shift)+Offset | i .in. BitSet]

bitset_to_list(few(M), Shift, Offset) -->
	bits_to_list(M, Shift, Offset).
bitset_to_list(many(M,A,B,C,D), Shift, Offset) -->
	bits_to_list(M, Shift, Offset),
	{ Shift1 is Shift+2 },
	{ Offset_A is 29<<Shift+Offset }, bitset_to_list(A, Shift1, Offset_A),
	{ Offset_B is 30<<Shift+Offset }, bitset_to_list(B, Shift1, Offset_B),
	{ Offset_C is 31<<Shift+Offset }, bitset_to_list(C, Shift1, Offset_C),
	{ Offset_D is 32<<Shift+Offset }, bitset_to_list(D, Shift1, Offset_D).

bits_to_list(M, Shift, Offset) -->
	(   { M =:= 0 } -> =
	;   { Y is \(M-1) /\ M, M1 is M-Y, mask_bit(Y, N) },
	    { Bit is N<<Shift + Offset },
	    [ Bit ],
	    bits_to_list(M1, Shift, Offset)
	).



%   bitset_ordset(?BitSet, ?OrdSet)
%   converts between bitset representation and ordset representation
%   for sets of non-negative integers.  This is rather slower than
%   bitset_to_list/2, so should be used only when you need an ordset
%   or when you want a bidirectional predicate.

bitset_ordset(BitSet, OrdSet) :-
	(   is_bitset(BitSet) ->
	    bitset_to_list(BitSet, Raw),
	    sort(Raw, OrdSet)
	;   list_to_bitset(OrdSet, BitSet)
	).



%   bit_memberchk(+BitNo, +BitSet)
%   is true when the specified integer BitNo is an element of the given
%   BitSet.  It can only be used to check BitNo, not to generate it.

bit_memberchk(BitNo, BitSet) :-
	integer(BitNo),
	BitNo >= 0,
	bit_memberchk_1(BitSet, BitNo).


bit_memberchk_1(few(M), BitNo) :-
	BitNo < 29,
	(M >> BitNo) /\ 1 =\= 0.
bit_memberchk_1(many(M,A,B,C,D), BitNo) :-
	(   BitNo < 29 ->
	    (M >> BitNo) /\ 1 =\= 0
	;   BitNo1 is BitNo-29,
	    BitNo2 is BitNo1 >> 2,
	    Case is BitNo1 /\ 3,
	    (	Case =:= 0 -> bit_memberchk_1(A, BitNo2)
	    ;	Case =:= 1 -> bit_memberchk_1(B, BitNo2)
	    ;	Case =:= 2 -> bit_memberchk_1(C, BitNo2)
	    ;	Case =:= 3 -> bit_memberchk_1(D, BitNo2)
	    )
	).



%   bit_member(?BitNo, +BitSet)
%   is true when BitNo is a non-negative integer occurring in BitSet.
%   It may be used to check whether a given BitNo occurs in BitSet,
%   or to enumerate all the BitNo elements of BitSet.

bit_member(BitNo, BitSet) :-
	(   var(BitNo) ->
	    bit_member_1(BitSet, BitNo)
	;   integer(BitNo), BitNo >= 0,
	    bit_memberchk_1(BitSet, BitNo)
	).


bit_member_1(few(M), BitNo) :-
	bit_mask(BitNo, OneBitMask),
	M /\ OneBitMask =\= 0.
bit_member_1(many(M,A,B,C,D), BitNo) :-
	bit_mask(BitNo, OneBitMask),
	M /\ OneBitMask =\= 0
    ;	bit_member_1(A, N),
	BitNo is N << 2 + 29
    ;	bit_member_1(B, N),
	BitNo is N << 2 + 30
    ;	bit_member_1(C, N),
	BitNo is N << 2 + 31
    ;	bit_member_1(D, N),
	BitNo is N << 2 + 32.


%.  bit_card(+BitSet, -Card)
%   is true when Card =  cardinality of BitSet

bit_card(BitSet, Card) :-
	bit_card_1(BitSet, 0, 0, 0, Card).

bit_card_1(few(M), Shift, Offset, Card0, Card) :-
	bit_card_2(M, Shift, Offset, Card0, Card).
bit_card_1(many(M,A,B,C,D), Shift, Offset, Card0, Card) :- 
	bit_card_2(M, Shift, Offset, Card0, Card1),
	Shift1 is Shift+2,
	Offset_A is 29<<Shift+Offset,
    	bit_card_1(A, Shift1, Offset_A, Card1, Card2),
	Offset_B is 30<<Shift+Offset,
    	bit_card_1(B, Shift1, Offset_B, Card2, Card3),
	Offset_C is 31<<Shift+Offset,
    	bit_card_1(C, Shift1, Offset_C, Card3, Card4),
	Offset_D is 32<<Shift+Offset,
    	bit_card_1(D, Shift1, Offset_D, Card4, Card).

bit_card_2(M, Shift, Offset, Card0, Card) :-
    	(   M =:= 0  -> Card = Card0
	;   Y is \(M-1) /\ M, M1 is M-Y,
	    Card1 is Card0 + 1,
	    bit_card_2(M1, Shift, Offset, Card1, Card)
	).


bit_mask( 0, 2'00000000000000000000000000000001).
bit_mask( 1, 2'00000000000000000000000000000010).
bit_mask( 2, 2'00000000000000000000000000000100).
bit_mask( 3, 2'00000000000000000000000000001000).
bit_mask( 4, 2'00000000000000000000000000010000).
bit_mask( 5, 2'00000000000000000000000000100000).
bit_mask( 6, 2'00000000000000000000000001000000).
bit_mask( 7, 2'00000000000000000000000010000000).
bit_mask( 8, 2'00000000000000000000000100000000).
bit_mask( 9, 2'00000000000000000000001000000000).
bit_mask(10, 2'00000000000000000000010000000000).
bit_mask(11, 2'00000000000000000000100000000000).
bit_mask(12, 2'00000000000000000001000000000000).
bit_mask(13, 2'00000000000000000010000000000000).
bit_mask(14, 2'00000000000000000100000000000000).
bit_mask(15, 2'00000000000000001000000000000000).
bit_mask(16, 2'00000000000000010000000000000000).
bit_mask(17, 2'00000000000000100000000000000000).
bit_mask(18, 2'00000000000001000000000000000000).
bit_mask(19, 2'00000000000010000000000000000000).
bit_mask(20, 2'00000000000100000000000000000000).
bit_mask(21, 2'00000000001000000000000000000000).
bit_mask(22, 2'00000000010000000000000000000000).
bit_mask(23, 2'00000000100000000000000000000000).
bit_mask(24, 2'00000001000000000000000000000000).
bit_mask(25, 2'00000010000000000000000000000000).
bit_mask(26, 2'00000100000000000000000000000000).
bit_mask(27, 2'00001000000000000000000000000000).
bit_mask(28, 2'00010000000000000000000000000000).


mask_bit(2'00000000000000000000000000000001,  0).
mask_bit(2'00000000000000000000000000000010,  1).
mask_bit(2'00000000000000000000000000000100,  2).
mask_bit(2'00000000000000000000000000001000,  3).
mask_bit(2'00000000000000000000000000010000,  4).
mask_bit(2'00000000000000000000000000100000,  5).
mask_bit(2'00000000000000000000000001000000,  6).
mask_bit(2'00000000000000000000000010000000,  7).
mask_bit(2'00000000000000000000000100000000,  8).
mask_bit(2'00000000000000000000001000000000,  9).
mask_bit(2'00000000000000000000010000000000, 10).
mask_bit(2'00000000000000000000100000000000, 11).
mask_bit(2'00000000000000000001000000000000, 12).
mask_bit(2'00000000000000000010000000000000, 13).
mask_bit(2'00000000000000000100000000000000, 14).
mask_bit(2'00000000000000001000000000000000, 15).
mask_bit(2'00000000000000010000000000000000, 16).
mask_bit(2'00000000000000100000000000000000, 17).
mask_bit(2'00000000000001000000000000000000, 18).
mask_bit(2'00000000000010000000000000000000, 19).
mask_bit(2'00000000000100000000000000000000, 20).
mask_bit(2'00000000001000000000000000000000, 21).
mask_bit(2'00000000010000000000000000000000, 22).
mask_bit(2'00000000100000000000000000000000, 23).
mask_bit(2'00000001000000000000000000000000, 24).
mask_bit(2'00000010000000000000000000000000, 25).
mask_bit(2'00000100000000000000000000000000, 26).
mask_bit(2'00001000000000000000000000000000, 27).
mask_bit(2'00010000000000000000000000000000, 28).


end_of_file.

test :-
	random(0, 7'777777777, N),
	bit_add_element(few(0), N, Set1),
	list_to_bitset([N], Set2),
	(   Set1 \== Set2 ->
	    write(bit_add_element(few(0), N, Set1)), write(' but '),
	    write(list_to_bitset([N], Set2)), nl
	;   \+ bit_memberchk(N, Set1) ->
	    write(bit_add_element(few(0), N, Set1)), write(' but '),
	    write(\+ bit_memberchk(N, Set1)), nl
	;   bit_member(X, Set1), X \== N ->
	    write(bit_add_element(few(0), N, Set1)), write(' but '),
	    write(bit_member(X, Set1)), nl
	;   bit_del_element(Set1, N, Set3), Set3 \== few(0) ->
	    write(bit_add_element(few(0), N, Set1)), write(' but '),
	    write(bit_del_element(Set1, N, Set3)), nl
	).

