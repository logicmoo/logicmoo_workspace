The Mutilated Checkerboard in Set Theory

John McCarthy

Computer Science Department

Stanford University

jmc@cs.stanford.edu

http://www-formal.stanford.edu/jmc/

April 27, 2001

An 8 by 8 checkerboard with two diagonally opposite squares removed cannot

be covered by dominoes each of which covers two rectilinearly adjacent squares.

We present a set theory description of the proposition and an informal proof

that the covering is impossible. While no present system that I know of will

accept either the formal description or the proof, I claim that both should be

admitted in any heavy duty set theory.1

We have the deﬁnitions

Board = Z8 × Z8,

mutilated-board = Board − {(0, 0), (7, 7)},

domino-on-board(x) ≡ (x ⊂ Board) ∧ card(x) = 2

∧(∀x1 x2)(x = {x1, x2} → adjacent(x1, x2))

and

adjacent(x1, x2) ≡ |c(x1, 1) − c(x2, 1)| = 1

∧c(x1, 2) = c(x2, 2)

∨|c(x1, 2) − c(x2, 2)| = 1 ∧ c(x1, 1) = c(x2, 1).

If we are willing to be slightly tricky, we can write more compactly

(1)

(2)

(3)

(4)

adjacent(x1, x2) ≡ |c(x1, 1) − c(x2, 1)| + |c(x1, 2) − c(x2, 2)| = 1,

(5)

but then the proof might not be so obvious to the program.

Next we have.

in Mizar is 400 lines.

1The Mizar proof checker accepts the deﬁnitions essentially as they are, but the ﬁrst proof

partial-covering(z)

≡ (∀x)(x ∈ z → domino-on-board(x))

∧(∀x y)(x ∈ z ∧ y ∈ z → x = y ∨ x ∩ y = {})

¬(∃z)(partial-covering(z) ∧ (cid:91) z = mutilated-board)

x ∈ Board → color(x) = rem(c(x, 1) + c(x, 2), 2)

domino-on-board(x) →

(∃u v)(u ∈ x ∧ v ∈ x ∧ color(u) = 0 ∧ color(v) = 1),

partial-covering(z) →

card({u ∈ (cid:83) z|color(u) = 0})

= card({u ∈ (cid:83) z|color(u) = 1}),

card({u ∈ mutilated-board|color(u) = 0})

(cid:54)= card({u ∈ mutilated-board|color(u) = 1}),

(6)

(7)

(8)

(9)

(10)

(11)

¬(∃z)(partial-covering(z) ∧ mutilated-board = (cid:91) z)

(12)

Theorem:

Proof:

We deﬁne

and ﬁnally

Q.E.D.

