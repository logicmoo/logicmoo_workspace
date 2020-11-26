Parameterizing Models of Propositional

Calculus Formulas

John McCarthy

Computer Science Department

Stanford University

jmc@cs.stanford.edu

http://www-formal.stanford.edu/jmc/

December 24, 2003

Abstract

It is often inadequate that a theory be consistent, i.e. have models.

It should have enough models. We discuss parameterizing the set of

models in the special case of propositional satisﬁability.

A propositional formula π in variables p1, . . . , pn is called satisﬁable if it

has a model, i.e. a tuple of truth values for p1, . . . , pn that makes π true.

Many programs exist for deciding this.

However, we can ask for more than just whether a formula has models;

we can ask about its set of models. One way to get a grip on the set of a

formula’s models is to parametrize it, i.e. to express the variables p1, . . . , pn of

the formula π as propositional expressions in other variables r1, . . . , rk, such

that arbitrary values of r1, . . . , rk give rise to exactly the values of p1, . . . , pn

satisfying π.

Here are some examples.

p2 = true.

• π = p1p2. this is a trivial case and the formulas are p1 = true and

• π = p1 ≡ p2. We have p1 = r1 and p2 = r1.

• π = p1 ∨ p2. We have p1 = r1 and p2 = ¯r1 ∨ ¯r2.

• π = true. We have p1 = r1 and p2 = r2.

• π = false. No model.

The number of parameters required depends on the number N of models

of π. Indeed it is ceiling(log2(N )), the least it could possibly be. Here’s how

to construct a parametrization.

Let π be written in disjunctive normal form. Choose enough variables

r1, . . . , rk, so that each conjunction is assigned one or more of the 2k con-

junctions of the ri and ¯ri. The assignment can be arbitrary provided each

conjunction of the pis gets a unique conjunction of the rjs. For each pi, we

write a conditional expression

if case 1 then tv 1

else if case 2 then tv 2

...

else if case 2k then tv 2k ,

(1)

where the cases are the r-conjunctions and the tv ’s are the truth values

that pi assumes in that case. The conditional expressions can be converted

to propositional expressions if desired, since all the consequents are truth

values.

Example:

[We’ll use juxtaposition for conjunction to keep the formulas compact.]

Let

π = (p1p2p3) ∨ (¯p1p2 ¯p3) ∨ (p1 ¯p2p3).

(2)

We need only 2 r variables. These give 4 cases. We can assign the cases

to the three terms of (2) arbitrarily, so let’s assign tt and tf to the ﬁrst

conjunction of (2) and ft and ﬀ to the two remaining conjunctions. We then

have

p1 = if r1r2 then t else if r1¯r2 then t else if ¯r1r2 then f else if ¯r1¯r2 then tp2 = if r1r2 then t else if r1¯r2 then t else if ¯r1r2 then t else if ¯r1¯r2 then fp3 = if r1r2 then t else if r1¯r2 then t else if ¯r1r2 then f else if ¯r1¯r2 then tRemarks:

1. Presumably the above systematic procedure usually doesn’t give the

optimal parametrization in terms of length of formula, although it is

optimal in the number of r-variables.

2. The parametrization is easy, because we have assumed that the the-

ory is represented by an expression π in disjunctive normal form. If

putting the theory in disjunctive normal form leads to an excessively

long expression, the parametrization may be more diﬃcult.

3. There is no apparent way of turning a parameterization of the models

of π into a parameterization of the models of ¬π.

4. Parameterizing models of modal formulas may oﬀer diﬃculties, because

there will often be an inﬁnity of models of even rather simple formulas.

This will depend on the modal logic.

5. Extending the idea to predicate calculus is likely to be reasonable only

under some restrictions. Thus parameterizing the models of the axioms

for a group is the problem of group classiﬁcation with its hundred year

history. However, Abelian groups are nicely parameterized.

6. Perhaps monadic predicate calculus will be a good domain.

7. Parameterizing the models of nonmonotonic theories may present in-

teresting problems even in the propositional case. 2001 August.

8. There may be straightforward ways of going from a parameterization

of a theory to parameterizations of some kinds of elaborations of the

theory. This may help with the problem of establishing consistency of

the elaborated theory. Thus the mere fact of consistency of a theory

may not help in establishing the consistency of an elaborated theory,

but a parameterization of the theory may lead to consistency of the

elaborated theory via its parameterization. - 2001 August.

