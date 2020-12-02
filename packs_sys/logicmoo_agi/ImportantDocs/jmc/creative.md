CREATIVE SOLUTIONS TO PROBLEMS

John McCarthy

Computer Science Department

Stanford University

Stanford, CA 94305

jmc@cs.stanford.edu

http://www-formal.stanford.edu/jmc/

March 29, 1999

Abstract

The idea is to chip a piece out of the problem of creativity by

deﬁning a creative solution to a problem relative to the functions and

predicates used in posing the problem. The simpliﬁcation comes from

not talking about the creativity of the problem solver but only about

the creativity of the solution.

Deﬁnition (informal): A solution to a problem is creative if it in-

volves concepts not present in statement of the problem and the gen-

eral knowledge surrounding it. Don’t identify creativity with diﬃculty

although they are usually correlated.

Example: The mutilated checkerboard problem.

We also consider how to express concisely the idea of a solution.

Whether the expression is adequate is relative to the knowledge and

ability of the person or program to which the idea is expressed.

Introduction

Making genuinely creative programs is likely to remain a distant goal

for AI until someone comes up with a suitable new idea. Therefore,

it is worthwhile to chip pieces oﬀ the creativity problem and work on

them separately. It seems that it is possible to study the notion of a

creative solution to a problem apart from studying how a person or

machine ﬁnds the solution.

Deﬁnition (informal): A solution to a problem is creative if

it involves concepts not present in statement of the problem and the

general knowledge surrounding it.

As a small step toward programs that ﬁnd creative solutions, we

consider how to express the idea of a solution concisely. The adequacy

of an idea of a solution is relative to the background of the person or

program that will complete the solution. Conciseness isolates the idea

from the background.

2 The mutilated checkerboard

Our ﬁrst Drosophila for studying creative solutions is the mutilated

checkerboard problem.

Two diagonally opposite corner squares are removed from a checker-

board. Is it possible to cover the remaining squares with dominoes? A

domino is a 1 × 2 rectangle, i.e. can cover two rectilinearly adjacent

squares.

The standard proof that this is impossible notes that a domino

covers one black square and one white square, and therefore any cov-

ering by dominoes covers equal numbers of squares of the two colors.

However, the mutilated checkerboard has 32 squares of one color and

30 squares of the other color.

We regard this proof as creative, because it involves an element

not present in the formulation of the problem—namely the colors of

the squares.

Here’s a concise expression of the idea.

A domino covers two squares of opposite color.

For some people this will be enough. Others may require the ad-

ditional sentence

The two squares that have been removed are of the same

color.

One could argue that the colors are present implicitly when a

checkerboard is mentioned, and perhaps the problem would be purer

if it referred to an 8 × 8 array. However, one’s initial reaction to the

problem is to consider there being two colors as irrelevant, just as it is

irrelevant what the two colors are—some boards have black and white

squares, some have red and black, and some have green and oﬀ-white

(perhaps thought to be more relaxing for the players).

(McCarthy 1964) presented the problem as a tough nut for ﬁrst

order theorem provers, because a straightforward formalization of the

problem provides no way of making the argument in the language of

the formalization. Creativity was not mentioned in that 1964 memo,

but I evidently mentioned it in lectures, because the problem gave

rise to an informal competition aimed at ﬁnding the most non-creative

solution to the problem.

The ﬁrst “non-creative” solution was proposed by Shmuel Wino-

grad of IBM. He claimed it was non-creative, because it didn’t involve

coloring.

Assume a covering. The number of dominoes projecting from the

top row to the second row is odd. Likewise the number from the sec-

ond to the third is odd, etc. Therefore, the total number of vertical

dominoes is the sum of seven odd numbers and hence odd. Likewise

the the number of horizontal dominoes is odd. Odd + odd is even so

the total is even, but the total is 31. There is an apparent mathemat-

ical induction here in the “etc.”. We will see later that the idea itself

does not include the induction.

Surely Winograd’s proof is creative, but we can ask whether there

is one creative idea in it or several. My guess is that there was one cre-

ative idea, and the rest was straightforward for a good mathematician

like Winograd.

The second was by Marvin Minsky of M.I.T.

Start with the 2-diagonal next to an excluded corner square. 2

dominoes must project from it to the adjacent 3-diagonal. Subtract-

ing, one domino projects from the 3-diagonal to the 4-diagonal. 3

project from the 4-diagonal to the 5-diagonal, 2 project from the 5-

diagonal to the 6-diagonal, 4 from the 6-diagonal to the 7-diagonal and

ﬁnally 3 project from the 7 diagonal to the 8-diagonal. This covers

only 3 of the 8 squares in the 8-diagonal. Coming from the opposite

excluded corner also only covers 3 squares of the 8-diagonal, leaving 2

uncovered squares. Minsky’s proof gets high points for non-creativity,

because it is speciﬁc to the 8 by 8 board.

The third method is by Dimitri Stefanuk of Moscow, Russia. He

suggested 62 proofs—or 17, taking into account symmetries.

Choose an arbitrary square and mark it 1. Mark its rectilinear

neighbors 2, their unmarked neighbors 3, continuing until every unex-

cluded square is marked. Then proceed as in Minsky’s proof, count-

ing the number of dominoes projecting from the 1-squares to the 2-

squares, etc. We get a proof if there are not enough dominoes pro-

jecting from the n − 1 squares to the n-squares. Every attempt at

a Stefanuk proof succeeds. Stefanuk proofs are just as uncreative a

Minsky proofs.

Remark: Counting colors shows that Stefanuk proofs and Minsky

proofs always work on any even board. However, this argument is at

a meta-level to the Minsky and Stefanuk proofs and therefore can’t

claim to be non-creative.

In fact, the Winograd, Minsky and Stefanuk proofs are all creative,

and we will try to identify the creativity involved and give a concise

expression of the ideas.

The most recent proofs, by separate computer programs written

by Mark Stickel ((Uribe and Stickel 1994)) and Dan Pehoushek (per-

sonal communication) apply propositional satisfaction algorithms to

a propositional expression in 256 variables. There is a variable assert-

ing for each square and each of the four direction whether a domino

projects from that square in that direction.1

The propositional satisfaction proofs are genuinely non-creative,

and but because they come from computer programs. Writing the

propositional calculus formulas asserting that a domino projects in

exactly one direction from each square, that there is a domino project

back from a square into which a domino projects and that no domino

projects oﬀ the board or into a forbidden square is taken as straight-

forward, i.e. not involving creativity.

(Subramanian 1993) includes an interactive proof using the Boyer-

Moore prover NQTHM. Allen Newell also discussed the problem.

3 Pinning down the ideas, English ﬁrst

Actually solving a problem involves both a creative part and a routine

part. It is sometimes possible to separate the creative part from the

routine part. Consider the Minsky solution. We can tell a person,

Starting with the diagonal next to an excluded square, com-

pute how many dominoes project from each diagonal to the

next diagonal.

1Stickel’s program solved it in 80.3 seconds in 1983 and running on a Pentium 200

solved it recently in 5.3 seconds.

This proved to be adequate for a sample of one veterinarian. This

is a kind of program, but it doesn’t need a termination condition, be-

cause the termination is apparent to the problem solver. It is harder

to tell whether it is close to the form in which the solution was dis-

covered, although it looks close.

When we consider expressing the creative idea to a computer pro-

gram, it would be most convenient to tell it a fact or to tell it an

executable program. I know of no-one who has tried to make com-

puter programs that can accept a fragment of a program as above as

the non-routine part of the solution to the problem.

Here’s a try at expressing concisely the idea of the Winograd so-

lution. It seems to be longer and to involve more than one idea.

Starting with the top row, compute the parity of the num-

ber of dominoes projecting down out of each row. Consider

the parity of the sum. Repeat going horizontally starting

with the left column. Compute the parity of the total num-

ber of dominoes compared to the sum of the two parities.

According to a 1999 personal communication, the idea can be ex-

pressed more concisely. Winograd got the idea as a result of seeing a

movie about the Socratic method in mathematics education. A stu-

dent said something like, “An odd number of dominoes project down

from the top row and an odd number down from the second row to the

third . . . ”. At this point the teacher interrupted the student and led

him by the Socratic method to the standard solution. Winograd won-

dered whether the student’s original idea could be pushed through,

and this led to the solution.

Looking at it this way, the creative idea seems to be

Note that an odd number of dominoes project from the top

row to the second and continue from there.

Presumably not everyone would be able to push the argument

through, because at some point you have to notice that although you

get no contradiction from determining that there are an odd number

of vertical dominoes, you also get an odd number of horizontal domi-

noes and an even number of dominoes altogether. Thus the notion of

creative solution seems to depend on the ability available to push an

idea through.

Winograd idea can also be pushed through when two squares of

the same color are removed any where on the board. If one starts at

the top and computes the successive parities, there is a jog in parity

when an excluded square is encountered. Taking into account the jog

one gets an answer as to the parity of the number of vertical dominoes.

For some pairs of excluded squares, the parity turns out to be even.

However, the parity turns out, it will be the same going horizontally.

The Winograd idea can then be expressed more abstractly.

Successively compute the parity of the number of vertical

dominoes projecting from each row to the next.

The Stefanuk solution seems to involve two ideas. The ﬁrst is to

label the squares starting from an arbitary square. The second is like

the Minsky solution, namely

Starting with n = 1, compute how many dominoes project

from the set of squares labelled n to the squares labelled

n + 1.

In this section I have striven for concise expressions of the creative

idea. The reason is to establish that there is one idea in each of the

cases. Obviously it will be easier to make computers come up with

creative solutions if the idea of the solution is just one thing—whatever

kind of thing that may be.

4 Elementary ﬁrst order formulations

The impossibility statement is readily formulated as a sentence of

the predicate calculus, but I don’t see how the parity and counting

argument can be translated into a guide to the method of semantic

tableaus, into a resolution argument, or into a standard proof.

The word “elementary” is used in the sense that the quantiﬁers

range over numbers and not over sets.

The formulas are here for completeness. If you don’t like reading

them, the rest of this section may be skipped.

We number the rows and columns from 1 to 8 and we intro-

duce predicates S(x, y), L(x, y), E(x, y), G1(x, y), G2(x, y), G3(x, y),

G4(x, y), and G5(x, y) with the following intended interpretations:

S(x, y) means y = x + 1.

L(x, y) means x < y.

E(x, y) means x = y.

G1(x, y) means the square (x, y) and the square (x + 1, y) are cov-

G2(x, y) means the square (x,y) and the square (x,y+1) are covered

G3(x, y) means the square (x,y) and the square (x-1,y) are covered

G4(x, y) means the square (x,y) and the square (x,y-1) are covered

ered by a domino.

by a domino.

by a domino.

by a domino.

G5(x, y) means the square (x,y) is not covered. We shall axiomatize

only as much of the properties of the numbers from 1 to 8 as we shall

need.

1. S(1, 2) ∧ S(2, 3) ∧ S(3, 4) ∧ S(4, 5) ∧ S(5, 6) ∧ S(6, 7) ∧ S(7, 8)

2. S(x, y) → L(x, y).

3. L(x, y) ∧ L(y, z) → L(x, z) ∧ ¬S(x, z).

4. L(x, y) → ¬E(x, y).

5. E(x, x).

These axioms insure that all eight numbers are diﬀerent and

determine the values of S(x, y), L(x, y), and E(x, y) for x, y =

1, . . . , 8.

6. G1(x, y) ∨ G2(x, y) ∨ G3(x, y) ∨ G4(x, y) ∨ G5(x, y)

7. G1(x, y) → ¬(G2(x, y) ∨ G3(x, y) ∨ G4(x, y) ∨ G5(x, y))

8. G2(x, y) → ¬(G3(x, y) ∨ G4(x, y) ∨ G5(x, y))

9. G3(x, y) → ¬(G4(x, y) ∨ G5(x, y))

10. G4(x, y) → ¬G5(x, y)

These axioms insure that every square (x,y) satisﬁes exactly one

Gi(x, y).

11. G5(x, y) ≡ x = 1 ∧ y = 1 ∨ x = 8 ∧ y = 8.

12. G5(x, y) → (E(1, x) ∧ E(1, y)) ∨ (E(8, x) ∧ E(8, y)) These axioms

insure that the uncovered squares are precisely (1,1) and (8,8).

13. S(x1, x2) → G(x1, y) ≡ G3(x2, y)

14. S(y1, y2) → G2(x, y1) ≡ G4(x, y2) These axioms state the condi-

tions that a pair of adjacent squares be covered by a domino.

15. ¬G3(1, y) ∧ ¬G1(8, y) ∧ ¬G2(x.8) ∧ ¬G4(x, 1)

These axioms state that the dominoes don’t stick out over the edge

of the board.

Suppose we had a model of these 15 sentences (in Robinson’s

clausal formalism, there would be 31 clauses). There would have to be

eight individuals 1(cid:48), . . . , 8(cid:48) satisfying the relations asserted for 1, . . . , 8

in the axioms. They would have to be distinct since axioms 1,2, and 3

allow us to prove L(x, y) whenever this is so and axioms 4 and 5 then

allow us to show that L(x, y) holds only for distinct x and y.

We then label the squares of a checkboard and place a domino

on each square (x,y) that satisﬁes G1(x, y) or G2(x, y) sticking to the

right or up as the case may be. Axioms 13 and 14 insure that the

dominoes don’t overlap, axioms 6-12 insure that all squares but the

corner squares are covered and axiom 15 insures that no dominoes

stick out over the edge.

Since there is no such covering the sentences have no model and

are inconsistent.

In a formalism that allows functions and equality we have a briefer

inconsistent set of sentences involving

the successor of x

s(x)

g(x, y) has value of 1 to 5 according to whether G1(x, y) or . . . or

G5(x, y)

The sentences are

1. s(s(s(s(s(s(s(s(8)))))))) = 8.

2. ¬s(s(s(s(x)))) = x The sentences insure the existence of 8 dis-

tinct individuals using a cyclic successor function.

3. g(x, y) = 5 ≡ x = 8 ∧ y = 8 ∨ x = 1 ∧ y = 1 Insures that exactly

the corner squares (1,1) and (8,8) are uncovered.

4. g(x, y) = 1 ≡ g(s(x), y) = 3

5. g(x, y) = 2 ≡ g(x, s(y)) = 4 Each domino covers two adjacent

squares

6. g(1, y) (cid:54)= 3 ∧ g(8, y) (cid:54)= 1 ∧ g(x, 1) (cid:54)= 4 ∧ g(x, 8) (cid:54)= 2

Dominoes don’t stick out

7. 1 = s(8) ∧ 2 = s(1) ∧ 3 = s(2) ∧ 4 = s(3) ∧ 5 = s(4).

8. g(x, y) = 1 ∨ g(x, y) = 2 ∨ g(x, y) = 3 ∨ g(x, y) = 4 ∨ g(x, y) = 5.

This identiﬁes the numbers used and ties down the values of g(x, y).

Not only is this language incapable of expressing the colors of the

squares. It is also incapable of expressing the counts of the numbers of

squares with a given property, the latter being required for expressing

the Minsky, Winograd and Stefanuk proofs.

5 Expressing the creative ideas in set

theory

In this section we try to identify the creative aspects of the proofs

with speciﬁc formulas of logic. We use Zermelo-Fraenkel set theory,

formalized in ﬁrst order logic, because its power allows formulas that

are closer to expressing the human ideas behind the proofs.

The traditional proof and the proofs by Winograd, Minsky and

Stefanuk can all be based on the following sentences of set theory

axiomatized in ﬁrst order logic.

5.1 Deﬁnitions common to the proofs

We have the deﬁnitions

Board = Z8 × Z8,

mutilated-board = Board − {(0, 0), (7, 7)},

domino-on-board(x) ≡ (x ⊂ Board) ∧ card(x) = 2

∧(∀x1 x2)(x1 (cid:54)= x2 ∧ x1 ∈ x ∧ x2 ∈ x

→ adjacent(x1, x2))

and

adjacent(x1, x2) ≡ |c(x1, 1) − c(x2, 1)| = 1

∧c(x1, 2) = c(x2, 2)

∨|c(x1, 2) − c(x2, 2)| = 1 ∧ c(x1, 1) = c(x2, 1).

If we are willing to be slightly tricky, we can write more compactly

adjacent(x1, x2) ≡

|c(x1, 1) − c(x2, 1)| + |c(x1, 2) − c(x2, 2)| = 1,

but then the proof might be more diﬃcult for a computer program.

(1)

(2)

(3)

(4)

(5)

Next we have.

Theorem:

partial-covering(z)

≡ (∀x)(x ∈ z → domino-on-board(x))

∧(∀x y)(x ∈ z ∧ y ∈ z → x = y ∨ x ∩ y = {})

(6)

¬(∃z)(partial-covering(z) ∧ (cid:91) z = mutilated-board)

(7)

Up to this point, there is no creativity. Someone might argue

that the decision to use set theory is creative, but I’m striving for a

technical notion of creative solution, and one has to start with some

background language. Taking set theory as background is more likely

to lead to technical results than trying to use English.

The proofs are as follows:

5.2 Standard proof

We deﬁne

x ∈ Board → color(x) = rem(c(x, 1) + c(x, 2), 2).

(8)

Making this deﬁnition is a creative step, but it doesn’t give the

solution by itself.

domino-on-board(x) →

(∃u v)(u ∈ x ∧ v ∈ x ∧ color(u) = 0

∧color(v) = 1).

partial-covering(z) →

card({u ∈ (cid:83) z|color(u) = 0})

= card({u ∈ (cid:83) z|color(u) = 1}).

This is the key creative step.

card({u ∈ mutilated-board|color(u) = 0})

(cid:54)= card({u ∈ mutilated-board|color(u) = 1}),

and ﬁnally

Q.E.D.

¬(∃z)(partial-covering(z)

∧mutilated-board = (cid:83) z)

(9)

(10)

(11)

(12)

and

than two.

This last step is presumably routine.

(McCarthy 1996) argued that the above proof, or some equally

concise expression of its ideas, should be accepted by any mathemat-

ical proof checker that mathematicians would actually use. Unfortu-

nately, no present proof checker comes close.

The following two deﬁnitions are used in the proofs by Winograd,

Minsky and Stefanuk.

(∀x A B)(domino-on-board(x)

∧A ⊂ Board ∧ B ⊂ Board →

(overlaps(x, A, B) ≡

x ∩ A (cid:54)= {} ∧ x ∩ B (cid:54)= {})

(∀A B)(A ⊂ Board ∧ B ⊂ Board

→ (size-overlap(A, B) =

card({x|overlaps(x, A, B)}))).

(13)

(14)

I suppose they count as creative, but maybe as one creation rather

In a future article, we hope to put the Winograd and Stefanuk

proofs in a logical form that isolates the creative part from the routine

part.

Acknowledgements

I am indebted to Tom Costello for both scientiﬁc and TeXnical help.

The work was supported by the Air Force Oﬃce of Scientiﬁc Re-

search New World Vistas program and by the Defense Advanced Re-

search Projects Agency High Performance Knowledge Bases program.

References

McCarthy, J. 1964. a tough nut for theorem provers2. Stanford

AI Memo 16—now on the web.

McCarthy, J. 1996. the mutilated checkerboard in set the-

ory3. presented at a 1996 conference in Warsaw.

2http://www-formal.stanford.edu/jmc/nut.html

3http://www-formal.stanford.edu/jmc/checkerboard.html

Subramanian, S. 1993. A Mechanized Framework for Specifying

Problem Domains and Verifying Plans. PhD thesis, University of

Texas, Austin.

Uribe, T., and M. Stickel. 1994. Ordered binary decision diagrams

and the davis-putnam procedure. In J. P. Drummond (Ed.), Lec-

ture Notes on Computer Science, First international conference on

constraints in computational logics, Vol. 845. Springer, September.

/@steam.stanford.edu:/u/jmc/w99/creative.tex: begun Sat Mar 6 20:57:30 1999, latexed March 29, 1999

at 3:18 p.m.

