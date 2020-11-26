A TOUGH NUT FOR PROOF

PROCEDURES

John McCarthy

Computer Science Department

Stanford University

Stanford, CA 94305

jmc@cs.stanford.edu

http://www-formal.stanford.edu/jmc/

1999 Jan 2, 5:30 p.m.

STANFORD ARTIFICIAL INTELLIGENCE PROJECT

Memo No. 16, July 17, 1964

Abstract

It is well known to be impossible to tile with dominoes a checker-

board with two opposite corners deleted. This fact is readily stated in

the ﬁrst order predicate calculus, but the usual proof which involves

a parity and counting argument does not readily translate into pred-

icate calculus. We conjecture that this problem will be very diﬃcult

for programmed proof procedures.

The research reported here was supported in part by the Advanced

Research Project Agency of the Oﬃce of the Secretary of Defense (SD-

183). 1

It is impossible to cover the mutilated checkerboard shown in the ﬁgure

with dominoes like the one in the ﬁgure. Namely, a domino covers a square

of each color, but there are 30 black squares and 32 white squares to be

covered.

11999: Life was simpler when AI was supported directly out of the Oﬃce of the Secre-

tary of Defense.

This old impossibility statement is readily formulated as a sentence of

the predicate calculus, but I don’t see how the parity and counting argument

can be translated into a guide to the method of semantic tableaus1, into a

resolvent argument2, or into a standard proof. Therefore, I oﬀer the problem

of proving the following sentences inconsistent as a challenge to the program-

mers of proof procedures and to the optimists who believe that by formulating

number theory in predicate calculus and by devising eﬃcient general proof

procedures for predicate calculus, signiﬁcant mathematical theorems can be

proved.

We number the rows and columns from 1 to 8 and we introduce predicates

S(x,y), L(x,y), E(x,y), G1(x, y), G2(x, y), G3(x, y), G4(x, y), and G5(x, y)

with the following intended interpretations: S(x,y) means y = x + 1

L(x,y) means x < y

E(x,y) means x = y

G1(x, y) means the square (x,y) and the square (x+1,y) are covered by a

domino.

domino.

domino.

domino.

G2(x, y) means the square (x,y) and the square (x,y+1) are covered by a

G3(x, y) means the square (x,y) and the square (x-1,y) are covered by a

G4(x, y) means the square (x,y) and the square (x,y-1) are covered by a

G5(x, y) means the square (x,y) is not covered. We shall axiomatize only

as much of the properties of the numbers from 1 to 8 as we shall need.

1. S(1,2) ∧ S(2,3) ∧ S(3,4) ∧ S(4,5) ∧ S(5,6) ∧ S(6,7) ∧ S(7,8)

2. S(x,y) ⊃ L(x,y)

3. L(x,y) ∧ Lyz ⊃ Lxz ∧ ¬ S(x,z)

4. L(x,y) ⊃ ¬ E(x,y)

5. E(x,x)

These axioms insure that all eight numbers are diﬀerent and determine

the values of S(x,y), L(x,y), and E(x,y) for x, y = 1, · · ·, 8.

6. G1(x, y) ∨ G2(x, y) ∨ G3(x, y) ∨ G4(x, y) ∨ G5(x, y)

7. G1(x, y) ⊃ ¬(G2(x, y) ∨ G3(x, y) ∨ G4(x, y) ∨ G5(x, y))

8. G2(x, y) ⊃ ¬(G3(x, y) ∨ G4(x, y) ∨ G5(x, y))

9. G3(x, y) ⊃ ¬(G4(x, y) ∨ G5(x, y))

10. G4(x, y) ⊃ ¬G5(x, y)

11. G5(1, 1) ∧ G5(8, 8)

These axioms insure that every square (x,y) satisﬁes exactly one

i

G(x,y)

12. G5(x, y) ⊃ (E(1, x) ∧ E(1, y)) ∨ (E(8, x) ∧ E(8, y)) These axioms insure

that the uncovered squares are precisely (1,1) and (8,8).

13. S(x1, x2) ⊃ G(x1, y) ≡ G3(x2, y)

14. S(y1, y2) ⊃ G2(x, y1) ≡ G4(x, y2) These axioms state the conditions

that a pair of adjacent squares be covered by a domino.

15. ¬G3(1, y) ∧ ¬G1(8, y) ∧ ¬G2(x.8) ∧ ¬G4(x, 1)

These axioms state that the dominoes don’t stick out over the edge of

the board.

Suppose we had a model of these 15 sentences (in Robinson’s clausal

formalism, there would be 31 clauses). There would have to be eight indi-

viduals 1(cid:48), . . . , 8(cid:48) satisfying the relations asserted for 1, . . . , 8 in the axioms.

They would have to be distinct since axioms 1,2, and 3 allow us to prove

L(x, y) whenever this is so and axioms 4 and 5 then allow us to show that

L(x, y) holds only for distinct x and y.

We then label the squares of a checkboard and place a domino on each

square (x,y) that satisﬁes G1(x, y) or G2(x, y) sticking to the right or up as

the case may be. Axioms 13 and 14 insure that the dominoes don’t overlap,

axioms 6-12 insure that all squares but the corner squares are covered and

axiom 15 insures that no dominoes stick out over the edge.

Since there is no such covering the sentences have no model and are

inconsistent.

In a formalism that allows functions and equality we have a briefer in-

consistent set of sentences involving

the successor of x

s(x)

g(x, y) has value of 1 to 5 according to whether G1(x, y) or · · · or G5(x, y)

The sentences are

1. s(s(s(s(s(s(s(s(8)))))))) = 8

2. ¬s(s(s(s(x)))) = x The sentences insure the existence of 8 distinct

individuals using a cyclic successor function.

3. g(x,y) = 5 ≡ x = 8 ∧ y = 8 ∨ x = 1 ∧ y = 1 Insures that exactly the

corner squares (1,1) and (8,8) are uncovered.

4. g(x,y) = 1 ≡ g(s(x),y) = 3

5. g(x,y) = 2 ≡ g(x,s(y)) = 4 Each domino covers two adjacent squares

6. g(1, y) (cid:54)= 3 ∧ g(8, y) (cid:54)= 1 ∧ g(x, 1) (cid:54)= 4 ∧ g(x, 8) (cid:54)= 2

Dominoes don’t stick out

7. 1 = s(8) ∧ 2 = s(1) ∧ 3 = s(2) ∧ 4 = s(3) ∧ 5 = s(4)

8. g(x,y) = 1 ∨ g(x,y) = 2 ∨ g(x,y) = 3 ∨ g(x,y) = 4 ∨ g(x,y) = 5

Identiﬁes the numbers used and ties down the values of g.

/@steam.stanford.edu:/u/jmc/f91/toughnut.tex: begun Sat Jan 2 13:51:51 1999, latexed January 2, 1999 at 5:30 p.m.

