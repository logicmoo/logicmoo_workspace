A BASIS FOR A MATHEMATICAL

THEORY OF COMPUTATION∗

JOHN McCARTHY

1961–1963

- [This 1963 paper was included in Computer Programming and Formal Sys-

- tems, edited by P. Braﬀort and D. Hirshberg and published by North-Holland.

- An earlier version was published in 1961 in the Proceedings of the Western

- Joint Computer Conference.]

Introduction

- Computation is sure to become one of the most important of the sciences.

- This is because it is the science of how machines can be made to carry out

- intellectual processes. We know that any intellectual process that can be car-

- ried out mechanically can be performed by a general purpose digital computer.

- Moreover, the limitations on what we have been able to make computers do

- so far clearly come far more from our weakness as programmers than from the

- intrinsic limitations of the machines. We hope that these limitations can be

- greatly reduced by developing a mathematical science of computation.

- There are three established directions of mathematical research relevant to

- a science of computation. The ﬁrst and oldest of these is numerical analysis.

- Unfortunately, its subject matter is too narrow to be of much help in forming

- a general theory, and it has only recently begun to be aﬀected by the existence

- of automatic computation.

- ∗This paper is a corrected version of the paper of the same title given at the Western

- Joint Computer Conference, May 1961. A tenth section discussing the relations between

- mathematical logic and computation has bean added.

- The second relevant direction of research is the theory of computability

- as a branch of recursive function theory. The results of the basic work in

- this theory, including the existence of universal machines and the existence

- of unsolvable problems, have established a framework in which any theory of

- computation must ﬁt. Unfortunately, the general trend of research in this

- ﬁeld has been to establish more and better unsolvability theorems, and there

- has been very little attention paid to positive results and none to establishing

- the properties of the kinds of algorithms that are actually used. Perhaps for

- this reason the formalisms for describing algorithms are too cumbersome to

- be used to describe actual algorithms.

- The third direction of mathematical research is the theory of ﬁnite au-

- tomata. Results which use the ﬁniteness of the number of states tend not to

- be very useful in dealing with present computers which have so many states

- that it is impossible for them to go through a substantial fraction of them in

- a reasonable time.

- The present paper is an attempt to create a basis for a mathematical theory

- of computation. Before mentioning what is in the paper, we shall discuss

- brieﬂy what practical results can be hoped for from a suitable mathematical

- theory. This paper contains direct contributions towards only a few of the

- goals to be mentioned, but we list additional goals in order to encourage a

- gold rush.

- 1. To develop a universal programming language. We believe that this

- goal has been written oﬀ prematurely by a number of people. Our opinion of

- the present situation is that ALGOL is on the right track but mainly lacks the

- ability to describe diﬀerent kinds of data, that COBOL is a step up a blind

- alley on account of its orientation towards English which is not well suited

- to the formal description of procedures, and that UNCOL is an exercise in

- group wishful thinking. The formalism for describing computations in this

- paper is not presented as a candidate for a universal programming language

- because it lacks a number of features, mainly syntactic, which are necessary

- for convenient use.

- 2. To deﬁne a theory of the equivalence of computation processes. With

- such a theory we can deﬁne equivalence preserving transformations. Such

- transformations can be used to take an algorithm from a form in which it is

- easily seen to give the right answers to an equivalent form guaranteed to give

- the same answers but which has other advantages such as speed, economy of

- storage, or the incorporation of auxiliary processes.

- 3. To represent algorithms by symbolic expressions in such a way that sig-

- niﬁcant changes in the behavior represented by the algorithms are represented

- by simple changes in the symbolic expressions. Programs that are supposed

- to learn from experience change their behavior by changing the contents of

- the registers that represent the modiﬁable aspects of their behavior. From

- a certain point of view, having a convenient representation of one’s behavior

- available for modiﬁcation is what is meant by consciousness.

- 4. To represent computers as well as computations in a formalism that

- permits a treatment of the relation between a computation and the computer

- that carries out the computation.

- 5. To give a quantitative theory of computation. There might be a quan-

- titative measure of the size of a computation analogous to Shannon’s measure

- of information. The present paper contains no information about this.

- The present paper is divided into two sections. The ﬁrst contains several

- descriptive formalisms with a few examples of their use, and the second con-

- tains what little theory we have that enables us to prove the equivalence of

- computations expressed in these formalisms. The formalisms treated are the

- following:

- 1. A way of describing the functions that are computable in terms of given

- base functions, using conditional expressions and recursive function deﬁnitions.

- This formalism diﬀers from those of recursive function theory in that it is not

- based on the integers, strings of symbols, or any other ﬁxed domain.

- 2. Computable functionals, i.e. functions with functions as arguments.

- 3. Non-computable functions. By adjoining quantiﬁers to the computable

- function formalism, we obtain a wider class of functions which are not a priori

- computable. However, such functions can often be shown to be equivalent to

- computable functions. In fact, the mathematics of computation may have, as

- one of its major aspects, rules which permit us to transform functions from a

- non-computable form into a computable form.

- 4. Ambiguous functions. Functions whose values are incompletely speci-

- ﬁed may be useful in proving facts about functions where certain details are

- irrelevant to the statement being proved.

- 5. A way of deﬁning new data spaces in terms of given base spaces and

- of deﬁning functions on the new spaces in terms of functions on the base

- spaces. Lack of such a formalism is one of the main weaknesses of ALGOL but

- the business data processing languages such as FLOWMATIC and COBOL

- have made a start in this direction, even though this start is hampered by

- concessions to what the authors presume are the prejudices of business men.

- The second part of the paper contains a few mathematical results about

- the properties of the formalisms introduced in the ﬁrst part. Speciﬁcally, we

- describe the following:

- 1. The formal properties of conditional expressions.

- 2. A method called recursion induction for proving the equivalence of

- recursively deﬁned functions.

- 3. Some relations between the formalisms introduced in this paper and

- other formalisms current in recursive function theory and in programming.

- We hope that the reader will not be angry about the contrast between the

- great expectations of a mathematical theory of computation and the meager

- results presented in this paper.

- 2 Formalisms For Describing Computable Func-

tions and Related Entities

- In this part we describe a number of new formalisms for expressing computable

- functions and related entities. The most important section is 1, the subject

- matter of which is fairly well understood. The other sections give formalisms

- which we hope will be useful in constructing computable functions and in prov-

- ing theorems about them.

- 2.1 Functions Computable in Terms of Given Base Func-

tions

- Suppose we are given a base collection F of functions (including predicates)

- having certain domains and ranges. In the case of the non-negative integers, we

- may have the successor function and the predicate of equality, and in the case

- of the S-expressions discussed in reference 7, we have the ﬁve basic operations.

- Our object is to deﬁne a class of functions C {F} which we shall call the class

- of f unctions computable in terms of F.

- Before developing C {F} formally, we wish to give an example, and in order

- to give the example, we ﬁrst need the concept of conditional expression. In

- our notation a conditional expression has the form

(p1 → e1, p2 → e2, . . . , pn → en)

- which corresponds to the ALGOL 60 reference language (12) expression

if p1 then e1 else if p2 then e2 . . . else if pn then en.

- Here p1, . . . , pn are propositional expressions taking the values T or F

- standing for truth and falsity respectively.

- The value of (p1 → e1, p2 → e2, . . . , pn → en) is the value of the e corre-

- sponding to the ﬁrst p that has value T. Thus

(4 < 3 → 7, 2 > 3 → 8, 2 < 3 → 9, 4 < 5 → 7) = 9.

- Some examples of the conditional expressions for well known functions are

|x| = (x < 0 → −x, x ≥ 0 → x)

δij = (i = j → 1, i (cid:54)= j → 0)

- and the triangular function whose graph is given in ﬁgure 1 is represented by

- the conditional expression

tri(x) = (x ≤ −1 → 0, x ≤ 0 → x + 1, x ≤ 1 → 1 − x, x > 1 → 0).

y

(0,1)

(-1,0)

Fig. 1

(1,0)

x

- Now we are ready to use conditional expressions to deﬁne functions recur-

- sively. For example, we have

n! = (n = 0 → 1, n (cid:54)= 0 → n · (n − 1)!)

- Let us evaluate 2! according to this deﬁnition. We have

2! = (2 = 0 → 1, 2 (cid:54)= 0 → 2 · (2 − 1)!)

= 2 · 1!

= 2 · (1 = 0 → 1, 1 (cid:54)= 0 → 1 · (1 − 1)!)

= 2 · 1 · 0!

= 2 · 1 · (0 = 0 → 1, 0 (cid:54)= 0 → 0 · (0 − 1)!)

= 2 · 1 · 1

= 2.

- The reader who has followed these simple examples is ready for the con-

- struction of C{F} which is a straightforward generalization of the above to-

- gether with a tying up of a few loose ends.

- Some notation. Let F be a collection (ﬁnite in the examples we shall give)

- of functions whose domains and ranges are certain sets. C{F} will be a class

- of functions involving the same sets which we shall call computable in terms

- of F.

- Suppose f is a function of n variables, and suppose that if we write y =

- f (xi, ..., xn), each xi takes values in the set Ui and y takes its value in the set

- V . It is customary to describe this situation by writing

f : U1 × U2 × . . . × Un → V.

- The set U1 × · · · × Un of n-tuples (x1, . . . , xn) is called the domain of f ,

- and the set V is called the range of f .

- Forms and functions. In order to make properly the deﬁnitions that follow,

- we will distinguish between functions and expressions involving free variables.

- Following Church [1] the latter are called f orms. Single letters such as f, g, h,

- etc. or sequences of letters such as sin are used to denote functions. Expres-

- sions such as f (x, y), f (g(x), y), x2 + y are called forms. In particular we may

- refer to the function f deﬁned by f (x, y) = x2 + y. Our deﬁnitions will be

- written as though all forms involving functions were written f (, ..., ) although

- we will use expressions like x + y with inﬁxes like + in examples.

- Composition. Now we shall describe the ways in which new functions are

- deﬁned from old. The ﬁrst way may be called (generalized) composition and

- involves the use of forms. We shall use the letters x, y, ... (sometimes with

- subscripts) for variables and will suppose that there is a notation for constants

- that does not make expressions ambiguous. (Thus, the decimal notation is

- allowed for constants when we are dealing with integers.)

- The class of forms is deﬁned recursively as follows:

- (i) A variable x with an associated space U is a form, and with this form

- we also associate U . A constant in a space U is a form and we also associate

- U with this form.

- (ii) If e1, ..., en are forms associated with the spaces U1, ..., Un respectively,

- then f (e1, ..., en) is a form associated with the space V . Thus the form

- f (g(x, y), x) may be built from the forms g(x, y) and x and the function f .

- If all the variables occurring in a form e are among x1, ..., xn, we can de-

- ﬁne a function h by writing h(x1, ..., xn) = e. We shall assume that the reader

- knows how to compute the values of a function deﬁned in this way. If f1, ..., fm

- are all the functions occurring in e we shall say that the function h is deﬁned by

- composition from f1, ..., fm. The class of functions deﬁnable from given func-

- tions using only composition is narrower than the class of function computable

- in terms of these functions.

- Partial functions. In the theory of computation it is necessary to deal with

- partial functions which are not deﬁned for all n-tuples in their domains. Thus

- we have the partial function minus, deﬁned by minus(x, y) = x − y, which

- is deﬁned on those pairs (x, y) of positive integers for which x is greater than

- y. A function which is deﬁned for all n-tuples in its domain is called a total

- function. We admit the limiting case of a partial function which is not deﬁned

- for any n-tuples.

- The n-tuples for which a function described by composition is deﬁned is

- determined in an obvious way from the sets of n-tuples for which the func-

- tions entering the composition are deﬁned. If all the functions occurring in a

- composition are total functions, the new function is also a total function, but

- the other processes for deﬁning functions are not so kind to totality. When

- the word “function” is used from here on, we shall mean partial function.

- Having to introduce partial functions is a nuisance, but an unavoidable

- one. The rules for deﬁning computable functions sometimes give computation

- processes that never terminate, and when the computation process fails to

- terminate, the result is undeﬁned. It is well known that there is no eﬀective

- general way of deciding whether a process will terminate.

- Predicates and propositional forms. The space Π of truth values whose only

- elements are T (for truth) and F (for falsity) has a special role in our theory.

- A function whose range is Π is called a predicate. Examples of predicates on

- the integers are prime deﬁned by

- and less deﬁned by

prime(x) = (cid:40) T if x is prime

F otherwise

less(x, y) = (cid:40) T if x < y

F otherwise

eqU (x, y) = (cid:40) T if x = y

F otherwise

- We shall, of course, write x < y instead of less(x, y). For any space U there

- is a predicate eqU of two arguments deﬁned by

- We shall write x = y instead of eqU (x, y), but some of the remarks about

- functions might not hold if we tried to consider equality a single predicate

- deﬁned on all spaces at once.

- A form with values in Π such as x < y, x = y, or prime(x) is called a

- propositional form.

- Propositional forms constructed directly from predicates such as prime(x)

- or x < y may be called simple. Compound propositional forms can be con-

- structed from the simple ones by means of the propositional connectives ∧, ∨,

- and ∼. We shall assume that the reader is familiar with the use of these

- connectives.

- Conditional forms or conditional expressions. Conditional forms require

- a little more careful treatment than was given above in connection with the

- example. The value of the conditional form

(p1 → e1, ..., pn → en)

- is the value of the e corresponding to the ﬁrst p that has value T; if all p’s

- have value F, then the value of the conditional form is not deﬁned. This rule

- is complete provided all the p’s and e’s have deﬁned values, but we need to

- make provision for the possibility that some of the p’s or e’s are undeﬁned.

- The rule is as follows:

- If an undeﬁned p occurs before a true p or if all p’s are false or if the e

- corresponding to the ﬁrst true p is undeﬁned, then the form is undeﬁned. Oth-

- erwise, the value of the form is the value of the e corresponding to the ﬁrst

- true p.

- We shall illustrate this deﬁnition by additional examples:

(2 < 1 → 1, 2 > 1 → 3) = 3

(1 < 2 → 4, 1 < 2 → 3) = 4

(2 < 1 → 1, 3 < 1 → 3) is undeﬁned

(0/0 < 1 → 1, 1 < 2 → 3) is undeﬁned

(1 < 2 → 0/0, 1 < 2 → 1) is undeﬁned

(1 < 2 → 2, 1 < 3 → 0/0) = 2

- The truth value T can be used to simplify certain conditional forms. Thus,

- instead of

|x| = (x < 0 → −x, x ≥ 0 → x),

- we shall write

- forms as follows:

|x| = (x < 0 → −x, T → x).

- The propositional connectives can be expressed in terms of conditional

- Considerations of truth tables show that these formulae give the same

- results as the usual deﬁnitions. However, in order to treat partial functions

- we must consider the possibility that p or q may be undeﬁned.

p ∧ q = (p → q, T → F)

p ∨ q = (p → T, T → q)

∼ p = (p → F, T → T)

p ⊃ q = (p → q, T → T)

- Suppose that p is false and q is undeﬁned; then according to the conditional

- form deﬁnition p ∧ q is false and q ∧ p is undeﬁned. This unsymmetry in the

- propositional connectives turns out to be appropriate in the theory of compu-

- tation since if a calculation of p gives F as a result q need not be computed

- to evaluate p ∧ q, but if the calculation of p does not terminate, we never get

- around to computing q.

- It is natural to ask if a function condn of 2n variables can be deﬁned so

- that

(p1 → e1, ..., pn → en) = condn(p, ..., pn, e1, ..., en).

- This is not possible unless we extend our notion of function because normally

- one requires all the arguments of a function to be given before the function is

- computed. However, as we shall shortly see, it is important that a conditional

- form be considered deﬁned when, for example, p1 is true and e1 is deﬁned and

- all the other p’s and e’s are undeﬁned. The required extension of the concept

- of function would have the property that functions of several variables could

- no longer be identiﬁed with one-variable functions deﬁned on product spaces.

- We shall not pursue this possibility further here.

- We now want to extend our notion of forms to include conditional forms.

- Suppose p1, ..., pn are forms associated with the space of truth values and

- e1, ..., en are forms each of which is associated with the space V . Suppose

- further that each variable xi occurring in p1, ..., pn and e1, ..., en is associated

- with the space U . Then (p1 → e1, ..., pn → en) is a form associated with V .

- We believe that conditional forms will eventually come to be generally used

- in mathematics whenever functions are deﬁned by considering cases. Their

- introduction is the same kind of innovation as vector notation. Nothing can

- be proved with them that could not also be proved without them. However,

- their formal properties, which will be discussed later, will reduce many case-

- analysis verbal arguments to calculation.

- Deﬁnition of functions by recursion. The deﬁnition

n! = (n = 0 → 1, T → n · (n − 1)!)

- is an example of deﬁnition by recursion. Consider the computation of 0!

0! = (0 = 0 → 1, T → 0 · (0 − 1)!) = 1.

- We now see that it is important to provide that the conditional form be deﬁned

- even if a term beyond the one that gives the value is undeﬁned. In this case

- (0 - 1)! is undeﬁned.

- Note also that if we consider a wider domain than the non-negative integers,

- n! as deﬁned above becomes a partial function, since unless n is a non-negative

- integer, the recursion process does not terminate.

- In general, we can either deﬁne single functions by recursion or deﬁne sev-

- eral functions together by simultaneous recursion, the former being a particular

- case of the latter.

- To deﬁne simultaneously functions f1, ..., fk, we write equations

f1(x1, ..., xn) = e1

...

fk(x1, ..., xn) = ek

- The expressions e1, ..., ek must contain only known functions and the functions

- f1, ..., fk. Suppose that the ranges of the functions are to be V1, ..., Vk respec-

- tively; then we further require that the expressions e1, ..., ek be associated with

- these spaces respectively, given that within e1, ..., ek the f ’s are taken as having

- the corresponding V ’s as ranges. This is a consistency condition.

- fi(xi, ..., xk) is to be evaluated for given values of the x’s as follows.

- 1. If ei is a conditional form then the p’s are to be evaluated in the pre-

- scribed order stopping when a true p and the corresponding e have been eval-

- uated.

- 2. If ei has the form g(e∗

1, ..., e∗

m), then e∗

1, ..., e∗

m are to be evaluated and

- then the function g applied.

- deﬁning equation.

- cording to the same rules.

- assigned values.

- 3.

If any expression fi(e∗

1, ..., e∗

n) occurs it is to be evaluated from the

- 4. Any subexpressions of ei that have to be evaluated are evaluated ac-

- 5. Variables occurring as subexpressions are evaluated by giving them the

- There is no guarantee that the evaluation process will terminate in any

- given case. If for particular arguments the process does not terminate, then

- the function is undeﬁned for these arguments. If the function fi occurs in the

- expression ei, then the possibility of termination depends on the presence of

- conditional expressions in the ei’s.

- The class of functions C{F} computable in terms of the given base func-

- tions F is deﬁned to consist of the functions which can be deﬁned by repeated

- applications of the above recursive deﬁnition process.

- 2.2 Recursive Functions of the Integers

- In Reference 7 we develop the recursive functions of a class of symbolic expres-

- sions in terms of the conditional expression and recursive function formalism.

- As an example of the use of recursive function deﬁnitions, we shall give

- recursive deﬁnitions of a number of functions over the integers. We do this for

- three reasons: to help the reader familiarize himself with recursive deﬁnition,

- to show how much simpler in practice our methods of recursive deﬁnition are

- than either Turing machines or Kleene’s formalism, and to prove that any par-

- tial recursive function (Kleene) on the non-negative integers is in C{F} where

- F contains only the successor function and the predicate equality.

- Let I be the set of non-negative integers {0,1,2,...} and denote the successor

- of an integer n by n(cid:48) and denote the equality of integers n1 and n2 by n1 = n2.

- If we deﬁne functions succ and eq by

succ(n) = n(cid:48)

eq(n1, n2) = (cid:40) T if n1 = n2

F if n1 (cid:54)= n2

- then we write F = {succ, eq}. We are interested in C{F}. Clearly all functions

- in C{F} will have either integers or truth values as values.

- First we deﬁne the predecessor function pred(not deﬁned for n = 0) by

pred(n) = pred2(n, 0)

pred2(n, m) = (m(cid:48) = n → m, T → pred2(n, m(cid:48))).

- We shall denote pred(n) by n−.

- Now we deﬁne the sum

m + n = (n = 0 → m, T → m(cid:48) + n−),

- the product

- the diﬀerence

mn = (n = 0 → 0, T → m + mn−),

m − n = (n = 0 → m, T → m− − n−)

- which is deﬁned only for m ≥ n. The inequality predicate m ≤ n is deﬁned by

m ≤ n = (m = 0) ∨ (∼ (n = 0) ∧ (m− ≤ n−)).

- The strict inequality m < n is deﬁned by

m < n = (m ≤ n)∧ ∼ (m = n).

- The integer valued quotient m/n is deﬁned by

m/n = (m < n → 0, T → ((m − n)/n)(cid:48)).

- The remainder on dividing m by n is deﬁned by

rem(m/n) = (m < n → m, T → rem((m − n)/n)),

- and the divisibility of a number n by a number m,

m|n = (n = 0) ∨ ((n ≥ m) ∧ (m|(n − m))).

- The primeness of a number is deﬁned by

prime(n) = (n (cid:54)= 0) ∧ (n (cid:54)= 1) ∧ prime2(n, 2)

- where

prime2(m, n) = (m = n) ∨ (∼ (m|n) ∧ prime2(n, m(cid:48))).

- The Euclidean algorithm deﬁnes the greatest common divisor, and we write

- gcd(m, n) = (m > n → gcd(n, m), rem(n/m) = 0 → m, T → gcd(rem(n/m), m))

- and we can deﬁne Euler’s ϕ-function by

ϕ(n) = ϕ2(n, n)

- where

ϕ2(n, m) = (m = 1 → 1, gcd(n, m) = 1 → ϕ2(n, m−)(cid:48), T → ϕ2(n, m−)).

- ϕ(n) is the number of numbers less than n and relatively prime to n.

- The above shows that our form of recursion is a convenient way of deﬁn-

- ing arithmetical functions. We shall see how some of the properties of the

- arithmetical functions can conveniently be derived in this formalism in a later

- section.

- 2.3 Computable Functionals

- The formalism previously described enables us to deﬁne functions that have

- functions as arguments. For example,

- can be regarded as a function of the numbers m and n and the sequence {ai}.

- If we regard the sequence as a function f we can write the recursive deﬁnition

sum(m, n, f ) = (m > n → 0, T → f (m) + sum(m + 1, n, f ))

- or in terms of the conventional notation

n

(cid:88)i=m

f (i) = (m > n → 0, T → f (m) +

f (i)).

n

(cid:88)i=m+1

- Functions with functions as arguments are called functionals.

- Another example is the functional least(p) which gives the least integer n

- such that p(n) for a predicate p. We have

least(p) = least2(p, 0)

- where

least2(p, n) = (p(n) → n, T → least2(p, n + 1)).

- In order to use functionals it is convenient to have a notation for naming

- functions. We use Church’s [1] lambda notation. Suppose we have a func-

- tion f deﬁned by an equation f (x1, ..., xn) = e where e is some expression

ai

n

(cid:88)i=m

- in x1, ..., xn. The name of this function is λ((x1, ..., xn), e). For example, the

- name of the function f deﬁned by f (x, y) = x2 + y is λ((x, y), x2 + y).

- Thus we have

λ((x, y), x2 + y)(3, 4) = 13,

λ((y, x), x2 + y)(3, 4) = 19.

- The variables occurring in a λ deﬁnition are dummy or bound variables and can

- be replaced by others without changing the function provided the replacement

- is done consistently. For example, the expressions

- but

- and

λ((x, y), x2 + y),

λ((u, v), u2 + v),

λ((y, x), y2 + x)

- all represent the same function.

- In the notation (cid:80)n

- integer n for which n2 > 50 is represented by

i=1 i2 is represented by sum(1, n, λ((i), i2)) and the least

least(λ((n), n2 > 50)).

- When the functions with which we are dealing are deﬁned recursively, a

- diﬃculty arises. For example, consider f actorial deﬁned by

f actorial(n) = (n = 0 → 1, T → n · f actorial(n − 1)).

- The expression

λ((n), (n = 0 → 1, T → n · f actorial(n − 1)))

- cannot serve as a name for this function because it is not clear that the oc-

- currence of “factorial” in the expression refers to the function deﬁned by the

- expression as a whole. Therefore, for recursive functions we adopt an addi-

- tional convention, Namely,

- stands for the function f deﬁned by the equation

label(f, λ((x1, ..., xn), e))

f (x1, ..., xn) = e

- where any occurrences of the function letter f within e stand for the function

- being deﬁned. The letter f is a dummy variable. The factorial function then

- has the name

label(f actorial, λ((n), (n = 0 → 1, T → n · f actorial(n − 1)))),

- and since f actorial and n are dummy variables the expression

label(g, λ((r), (r = 0 → 1, T → r · g(r − 1))))

- represents the same function.

- If we start with a base domain for our variables, it is possible to consider

- a hierarchy of functionals. At level 1 we have functions whose arguments are

- in the base domain. At level 2 we have functionals taking functions of level

- 1 as arguments. At level 3 are functionals taking functionals of level 2 as

- arguments, etc. Actually functionals of several variables can be of mixed type.

- However, this hierarchy does not exhaust the possibilities, and if we allow

- functions which can take themselves as arguments we can eliminate the use of

- label in naming recursive functions. Suppose that we have a function f deﬁned

- by

f (x) = E(x, f )

- where E(x, f ) is some expression in x and the function variable f . This function

- can be named

label(f, λ((x), E(x, f ))).

- However, suppose we deﬁne a function g by

- or

g(x, ϕ) = E(x, λ((x), ϕ(x, ϕ)))

g = λ((x, ϕ), E(x, λ((x), ϕ(x, ϕ)))).

- We then have

- since g(x, g) satisﬁes the equation

f (x) = g(x, g)

- Now we can write f as

g(x, g) = E(x, λ((x), g(x, g))).

- f = λ((x), λ((y, ϕ), E(y, λ((u), ϕ(u, ϕ))))(x, λ((y, ϕ), E(y, λ((u), ϕ(u, ϕ)))))).

- This eliminates label at what seems to be an excessive cost. Namely, the

- expression gets quite complicated and we must admit functionals capable of

- taking themselves as arguments. These escape our orderly hierarchy of func-

- tionals.

- 2.4 Non-Computable Functions and Functionals

- It might be supposed that in a mathematical theory of computation one need

- only consider computable functions. However, mathematical physics is car-

- ried out in terms of real valued functions which we not computable but only

- approximable by computable functions.

- We shall consider several successive extensions of the class C{F}. First we

- adjoin the universal quantiﬁer ∀ to the operations used to deﬁne new functions.

- Suppose e is a form in a variable x and other variables associated with the

- space Π of truth values. Then

- is a new form in the remaining variables also associated with Π. ∀((x), e) has

- the value T for given values of the remaining variables if for all values of x, e

- has the value T. ∀((x), e) has the value F if for at least one value of x, e has

- the value F. In the remaining case, i.e. for some values of x, e has the value T

- and for all others e is undeﬁned, ∀((x), e) is undeﬁned.

- If we allow the use of the universal quantiﬁer to form new propositional

- forms for use in conditional forms, we get a class of functions Ha{F} which

- may well be called the class of functions hyper-arithmetic over F since in

∀((x), e)

- the case where F = {successor, equality} on the integers, Ha{F} consists of

- Kleene’s hyper-arithmetic functions.

- Our next step is to allow the description operator ı. ı((x), p(x)) stands for

- the unique x such that p(x) is true. Unless there is such an x and it is unique,

- ı((x), p(x)) is undeﬁned. In the case of the integers ı((x), p(x)) can be deﬁned

- in terms of the universal quantiﬁer using conditional expressions, but this does

- not seem to be the case in domains which are not eﬀectively enumerable, and

- one may not wish to do so in domains where enumeration is unnatural.

- The next step is to allow quantiﬁcation over functions. This gets us to

- Kleene’s [5] analytic hierarchy and presumably allows the functions used in

- analysis. Two facts are worth noting. First ∀((f ), ϕ(f )) refers to all functions

- on the domain and not just the computable ones. If we restrict quantiﬁca-

- tion to computable functions, we get diﬀerent results. Secondly, if we allow

- functions which can take themselves as arguments, it is diﬃcult to assign a

- meaning to the quantiﬁcation. In fact, we are apparently confronted with the

- paradoxes of naive set theory.

- 2.5 Ambiguous Functions

- Ambiguous functions are not really functions. For each prescription of values

- to the arguments the ambiguous function has a collection of possible values.

- An example of an ambiguous function is less(n) deﬁned for all positive integer

- values of n. Every non-negative integer less than n is a possible value of

- less(n). First we deﬁne a basic ambiguity operator amb(x, y) whose possible

- values are x and y when both are deﬁned: otherwise, whichever is deﬁned.

- Now we can deﬁne less(n) by

less(n) = amb(n − 1, less(n − 1)).

- less(n) has the property that if we deﬁne

ult(n) = (n = 0 → 0, T → ult(less(n)))

- then

∀((n), ult(n) = 0) = T .

- There are a number of important kinds of mathematical arguments whose

- convenient formalization may involve ambiguous functions. In order to give

- an example, we need two deﬁnitions.

- If f and g are two ambiguous functions, we shall say that f is a descendant

- of g if for each x every possible value of f (x) is also a possible value of g(x).

- Secondly, we shall say that a property of ambiguous functions is hereditary

- if whenever it is possessed by a function g it is also possessed by all descendants

- of g. The property that iteration of an integer valued function eventually gives

- 0 is hereditary, and the function less has this property. So, therefore, do all

- its descendants. Therefore any integer-function g satisfying g(0) = 0 and

- n > 0 ⊃ g(n) < n has the property that g ∗(n) = (n = 0 → 0, T → g∗(g(n)))

- is identically 0 since g is a descendant of less. Thus any function, however

- complicated, which always reduces a number will if iterated suﬃciently always

- give 0.

- This example is one of our reasons for hoping that ambiguous functions

- will turn out to be useful.

- With just the operation amb deﬁned above adjoined to those used to gen-

- erate C{F}, we can extend F to the class C ∗{F} which may be called the

- computably ambiguous functions. A wider class of ambiguous functions is

- formed using the operator Am(x, π(x)) whose values are all x’s satisfying π(x).

- 2.6 Recursive Deﬁnitions of Sets

- In the previous sections on recursive deﬁnition of functions the domains and

- ranges of the basic functions were prescribed and the deﬁned functions had

- the same domains and ranges.

- In this section we shall consider the deﬁnition of new sets and the basic

- functions on them. First we shall consider some operations whereby new sets

- can be deﬁned.

- 1. The Cartesian product A × B of two sets A and B is the set of all

- ordered pairs (a · b) with a (cid:15) A and b (cid:15) B.

If A and B are ﬁnite sets and

- n(A) and n(B) denote the numbers of members of A and B respectively then

- n(A × B) = n(A) · n(B).

- Associated with the pair of sets (A, B) are two canonical mappings:

πA,B : A × B → A deﬁned by πA,B ((a · b)) = a

(cid:37)A,B : A × B → B deﬁned by (cid:37)A,B ((a · b)) = b.

- The word “canonical” refers to the fact that πA,B and (cid:37)A,B are deﬁned by the

- sets A and B and do not depend on knowing anything about the members of

- A and B.

- The next canonical function γ is a function of two variables γA,B : A, B →

- A × B deﬁned by

γA,B (a, b) = (a · b).

- For some purposes functions of two variables, x from A and y from B, can be

- identiﬁed with functions of one variable deﬁned on A × B.

- 2. The direct union A ⊕ B of the sets A and B is the union of two non-

- intersecting sets one of which is in 1-1 correspondence with A and the other

- with B. If A and B are ﬁnite, then n(A ⊕ B) = n(A) + n(B) even if A and

- B intersect. The elements of A ⊕ B may be written as elements of A or B

- subscripted with the set from which they come, i.e. aA or bB.

- The canonical mappings associated with the direct union A ⊕ B are

- iA,B : A → A ⊕ B deﬁned by iA,B (a) = aA,

- jA,B : B → A ⊕ B deﬁned by jA,B (b) = bB,

- pA,B : A ⊕ B → Π deﬁned by pA,B (x) = T if and only if x comes from A,

- qA,B : A ⊕ B → Π deﬁned by qA,B (x) = T if and only if x comes from B.

- There are two canonical partial functions rA,B and sA,B. rA,B : A ⊕ B → A

- is deﬁned only for elements coming from A and satisﬁes rA,B(iA,B (a)) = a.

- Similarly, sA,B : A ⊕ B → B satisﬁes sA,B (jA,B (b)) = b.

- 3. The power set AB is the set of all mappings f : B → A. The canonical

- mapping αA,B : AB × B → A is deﬁned by αA,B (f, b) = f (b).

- Canonical mappings. We will not regard the sets A × (B × C) and (A ×

- B) × C as the same, but there is a canonical 1-1 mapping between them,

gA,B,C : (A × B) × C → A × (B × C)

- deﬁned by

- We shall write

- gA,B,C(u) = γA,B×C(πA,B(πA×B,C(u)), γB,C((cid:37)A,B(πA×B,C(u)), (cid:37)A×B,C(u))).

(A × B) × C (cid:39) A × (B × C)

- to express the fact that these sets are canonically isomorphic.

- Other canonical isomorphisms are

tA,B : A × B → B × A deﬁned by t(u) = γB,A((cid:37)A,B(u), πA,B(u))

1.

2. d1 : A × (B ⊕ C) → A × B ⊕ A × C

3. a2 : (A ⊕ B) ⊕ C → A ⊕ (B ⊕ C)

4. d2 : AC × BC → (A × B)C

5. d3 : AB × AC → AB⊕C

6. s1 : (AB)C → AB×C

- We shall denote the null set (containing no elements) by 0 and the set consist-

- ing of the integers from 1 to n by n. We have

A ⊕ 0 (cid:39) A

A × 0 (cid:39) 0

A × 1 (cid:39) A

A × 2 (cid:39) A ⊕ A(n terms, associate to left by convention)

A0 (cid:39) 1

A1 (cid:39) A

An (cid:39) A × ... × A(n terms, associate to left by convention)

(by convention)

- Suppose we write the recursive equation

S = {Λ} ⊕ A × S.

- We can interpret this as deﬁning the set of sequences of elements of A as

- follows:

- 1.

Interpret Λ as denoting the null sequence. Then the null sequence

- (strictly an image of it) is an element of S.

- 2. Since a pair consisting of an element of A and an element of S is an

- element of S, a pair (a, Λ) is an element of S. So, then, are

(a1 · (a2 · Λ)) and (a1 · (a2 · (a3 · Λ))) etc.

- Thus S consists of all sequences of elements of A including the null sequence.

- Suppose we substitute {Λ}⊕A×S for S in the right side of S = {Λ}⊕A×S.

- We get

S = {Λ} ⊕ A × ({Λ} ⊕ A × S).

- If we again substitute for S and expand by the distributive law expressed in

- equation (2) above we get

S = {Λ} ⊕ A × {Λ} ⊕ A × A × {Λ} ⊕ ...

- which, if we now denote the set {Λ} by 1, becomes

S = 1 ⊕ A ⊕ A2 ⊕ A3 ⊕ ...

- which is another way of writing the set of sequences. We shall denote the set

- of sequences of elements of A by seq(A).

- We can also derive this relation by writing S = 1 ⊕ A × S and solving

- formally for S, getting S = 1/(1 − A) which we expand in geometric series to

- get S = 1 ⊕ A ⊕ A2 ⊕ ... just as before.

- Another useful recursive construction is

S = A ⊕ S × S.

- Its elements have the forms a or (a1 · a2) or ((a1 · a2) · a3) or (a1 · (a2 · a3)) etc.

- Thus we have the set of S-expressions on the alphabet A which we may denote

- by sexp(A). This set is the subject matter of Reference 7, and the following

- paragraph refers to this paper.

- When sets are formed by this kind of recursive deﬁnition, the canonical

- mappings associated with the direct sum and Cartesian product operations

- have signiﬁcance. Consider, for example, sexp(A).

- We can deﬁne the basic operations of Lisp, i.e. atom, eq, car, cdr and cons

- by the equations

atom(x) = pA,S×S(x)

eq(x, y) = (iA,S×S(x) = iA,S×S(y))

- assuming that equality is deﬁned on the space A.

car(x) = nS,S(sA,S×S(x))

cdr(x) = (cid:37)S,S(sA,S×S(x))

cons(x, y) = jA,S×S(γS,S(x, y))

- Deﬁnition of the set of integers. Let 0 denote the null set as before. We

- can deﬁne the set of integers I by

- Its elements are then

I = {0} ⊕ {0} × I.

0, (0 · 0), (0 · (0 · 0)), etc.

- which we shall denote by 0,1,2,3 etc. The successor and predecessor functions

- are then deﬁnable in terms of the canonical operations of the deﬁning equation.

- We have

succ(n) = γ(0, n)

pred(n) = (cid:37)(s(n)).

- 3 Properties of Computable Functions

- The ﬁrst part of this paper was solely concerned with presenting descriptive

- formalisms. In this part we shall establish a few of the properties of the enti-

- ties we previously introduced. The most important section is section 8 which

- deals with recursion induction.

- 3.1 Formal Properties of Conditional Forms

- The theory of conditional expressions corresponds to analysis by cases in math-

- ematics and is only a mild generalization of propositional calculus.

- We start by considering expressions called generalized Boolean forms (gbf)

- 1. Variables are divided into propositional variables p, q, r, etc. and general

- formed as follows:

- variables x, y, z, etc.

- 2. We shall write (p → x, y) for (p → x, T → y). (p → x, y) is called an

- elementary conditional form (ecf) of which p, x, and y are called the premiss,

- conclusion and the alternative, respectively.1

- 3. A variable is a gbf, and if it is a propositional variable it is called a

- propositional form (pf).

- α and β are pfs, so is (π → α, β).

- 4. If π is a pf and α and β are gbfs, then (π → α, β) is a gbf. If, in addition,

- The value of a gbf α for given values (T, F or undeﬁned) of the propositional

- variables will be T or F in case α is a pf or a general variable otherwise. This

- value is determined for a gbf (π → α, β) according to the table

value(π)

T

F

undeﬁned undeﬁned

value((π → α, β))

value(α)

value(β)

- We shall say that two gbfs are strongly equivalent if they have the same value

- for all values of the propositional variables in them including the case of unde-

- ﬁned propositional variables. They are weakly equivalent if they have the same

- values for all values of the propositional variables when these are restricted to

- F and T.

- The equivalence of gbfs can be tested by a method of truth tables identical

- to that of propositional calculus. The table for ((p → q, r) → a, b) and (p →

- (q → a, b), (r → a, b)) is given on the foregoing page.

- 11996: I adopted Alonzo Church’s spelling for the premise of an argument. It was not

- adopted generally, and I later gave up.

p q r

( p → q, r)

( (p → q, r) → a, b)

(q → a, b)

( r → a, b)

( p → (q → a, b),(r → a, b))T T T

T T F

T T u

T F T

T F F

T F u

T u T

T u F

T u u

F T T

F T F

F T u

F F T

F F F

F F u

F u T

F u F

F u u

u T T

u T F

u T u

u F T

u F F

u F u

u u T

u u F

u u u

T

T

T

F

F

F

u

u

u

T

F

u

T

F

u

T

F

u

u

u

u

u

u

u

u

u

u

a

a

a

b

b

b

u

u

u

a

a

a

b

b

b

u

u

u

a

a

a

b

b

b

u

u

u

a

a

u

a

b

u

a

b

u

a

b

u

a

b

u

a

b

u

a

b

u

a

a

u

a

b

u

a

a

a

b

b

b

u

u

u

a

b

u

a

b

u

a

b

u

u

u

u

u

u

u

u

u

u

a

a

a

b

b

b

u

u

u

a

b

u

a

b

u

a

b

u

u

u

u

u

u

u

u

u

u

- According to the table, ((p → q, r) → a, b) and (p → (q → a, b), (r → a, b))

- are strongly equivalent.

- Consider the table,

- For weak equivalence the u case can be left out of the table.

- p q

( q → a, b)

( q → c, d)

( p → a, c)

( p → b, d)

(p → (q → a, b),

(q → c, d))

( q → (p → a, c),(p → b, d))c

d

c

d

a

b

a

b

- T T

- T F

- F T

- F F

- which proves that (p → (q → a, b), (q → c, d)) and (q → (p → a, c), (p → b, d))

- are weakly equivalent. They are also strongly equivalent. We shall write ≡s

- and ≡w for the relations of strong and weak equivalence.

b

b

d

d

a

b

c

d

a

a

c

c

a

b

c

d

- There are two rules whereby an equivalence can be used to generate other

- equivalences.

- 1.

If α ≡ β and α1 ≡ B1 is the result of substituting any gbf for any

- variable in α ≡ β, then α1 ≡ β1. This is called the rule of substitution.

- 2. If α ≡ β and α is subexpression of γ and δ is the result of replacing an

- occurrence of α in γ by an occurrence of β, then γ ≡ δ. This is called the rule

- of replacement.

- These rules are applicable to either strong or weak equivalence and in fact

- to much more general situations.

- Weak equivalence corresponds more closely to equivalence of truth func-

- tions in propositional calculus than does strong equivalence.

- Consider the equations

1)

2)

3)

4)

5)

6)

7)

8)

(p → a, a) ≡w a

(T → a, b) ≡s a

(F → a, b) ≡s b

(p → T, F) ≡s p

(p → (p → a, b), c) ≡s (p → a, c)

(p → a, (p → b, c) ≡s (p → a, c)

((p → q, r) → a, b) ≡s (p → (q → a, b), (r → a, b))

((p → (q → a, b), (q → c, d)) ≡s (q → (p → a, c), (p → b, d))

- All are strong equivalence except the ﬁrst, and all can be proved by truth

- tables.

- These eight equations can be used as axioms to transform any gbf into any

- weakly equivalent one using substitution and replacement.

- In fact, they can be used to transform any gbf into a canonical form. This

- canonical form is the following. Let p1, ..., pn be the variables of the gbf a

- taken in an arbitrary order. Then a can be transformed into the form

- where each ai has the form

(p1 → a0, a1)

ai = (p2 → ai0, ai1)

- and in general for each k = 1, ..., n − 1

ai1...ik = (pk+1 → ai1...ik 0, ai1...ik 1)

- and each ai1...in is a truth value or a general variable.

- For example, the canonical form of

- with the variables taken in the order r, q, p is

((p → q, r) → a, b)

(r → (q → (p → a, a), (p → b, a)), (q → (p → a, b), (p → b, b))).

- In this canonical form, the 2n cases of the truth or falsity of p1, ..., pn are

- explicitly exhibited.

- An expression may be transformed into canonical form as follows:

- 1) Axiom 7 is used repeatedly until in every subexpression the π in (π →

- α, β) consists of a single propositional variable.

- 2) The variable p1 is moved to the front by repeated application of axiom

- 8. There are three cases: (q → (p1 → a, b), p1 → c, d)) to which axiom 8 is

- directly applicable; (q → a, (p1 → c, d)) where axiom 8 becomes applicable

- after axiom 1 is used to make it (q → (p1 → a, a), (p1 → c, b)); the case

- (q → (p1 → a, b), c) which is handled in a manner similar to that of case 2.

- Once the main expression has the form (p1 → α, β) we move any p1’s which

- occur in α and β to the front and eliminate them using axioms 5 and 6. We

- then bring p2 to the front of α and β using axiom 1 if necessary to guarantee

- at least one occurrence of p2 in each of α and β. The process is continued until

- the canonical form is achieved.

- There is also a canonical form for strong equivalence. Any gbf a is strongly

- equivalent to one of the form (p1 → α, β), where α and β do not contain p1 and

- are themselves in canonical form. However, the variable p1 may not be chosen

- arbitrarily but must be an inevitable propositional variable of the original gbf

- and can be chosen to be any inevitable variable. An inevitable variable of a

- gbf (π → α, β) is deﬁned to be either the ﬁrst propositional variable or else an

- inevitable variable of both α and β. Thus p and q are the inevitable variables

- of

(p → (r → (q → a, b), (q → c, d)), (q → e, f )).

- A gbf a may be put in strong canonical form as follows:

- 1) Use axiom 7 to get all premisses as propositional variables.

- 2) Choose any inevitable variable, say p1, and put a in the form (p1 → α, β)

- by using axiom 8.

- 3) The next step is to eliminate occurrences of p1 in α and β. This can

- be done by the general rule that in any ecf occurrences of the premiss in the

- conclusion can be replaced by T and occurrences in the alternative by F. How-

- ever, if we wish to use substitution and replacement on formulas we need the

- additional axioms

- (9) (p → (q → a, b), c) ≡s (p → (q → (p → a, a), (p → b, b)), c)

- and

- (10) (p → a, (q → b, c)) ≡s (p → a, (q → (p → b, b), (p → c, c))).

- Suppose there is an occurrence of p1 in the conclusion; we want to replace it by

- T. To do this, we use axioms 9 and 10 to move in a p1 until the objectionable

- p1 occurs as the inner p1 of one of the forms

p1 → (p1 → a, b), c)

- or

- forth.

p1 → a, (p1 → b, c)).

- In either case, the objectionable p1 can be removed by axiom 5 or 6, and the

- p1’s that were moved in can be moved out again.

- Thus we have (p1 → α, β) with p1 missing from α and β.

- 4) Inevitable variables are then brought to the front of α and β and so

- Two gbfs are equivalent (weakly or strongly) if and only if they have the

- same (weak or strong) canonical form. One way this is easy to prove; if two gbfs

- have the same canonical form they can be transformed into each other via the

- canonical form. Suppose two gbfs have diﬀerent weak canonical forms when

- the variables are taken in the same order. Then values can be chosen for the

- p’s giving diﬀerent values for the form proving non-equivalence. In the strong

- case, suppose that two gbfs do not have the same inevitable propositional

- variables. Let p be inevitable in a but not in b. Then if the other variables are

- assigned suitable values b will be deﬁned with p undeﬁned. However, a will be

- undeﬁned since p is inevitable in a which proves non-equivalence. Therefore,

- strongly equivalent gbfs have the same inevitable variables, so let one of them

- be put in front of both gbfs. The process is then repeated in the conclusion

- and alternative etc.

- The general conditional form

- can be regarded as having the form

(p1 → e1, ..., pn → en)

(p1 → e1, (p2 → e2, ..., (pn → en, u), ...))

- where u is a special undeﬁned variable and their properties can be derived

- from those of gbf’s.

- The relation of functions to conditional forms is given by the distributive

- law

- f (x1, ..., xi−1, (p1 → e1, ..., pn → en), xi+1, ..., xk) =

(p1 → f (x1, ..., xi−1, e1, xi+1, ..., xk), ..., pn → f (x1, ..., xi−1, en, xi+1, ..., xk)).

- The rule of replacement can be extended in the case of conditional ex-

- pressions. Suppose α is an occurrence of a subexpression of an expression β.

- We deﬁne a certain propositional expression π called the premiss of α in β as

- follows:

- 1) The premiss of α in α is T

- 2) The premiss of α in f (x1, ..., xi, ..., xn) where α is part of xi is the premiss

- of α in xi.

- 3) If α occurs in e1 and the premiss of α in ei is π, then the premiss of α

- in (p1 → e1, ..., pi → ei, ..., pn → en) is (∼ p1 ∧ ...∧ ∼ pi−1) ∧ pi ∧ π.

- 4) If α occurs in pi and the premiss of α in pi is π, then the premiss of α

- in (p1 → e1, ..., pi → ei, ..., pn → en) is ∼ p1 ∧ ...∧ ∼ pi−1 ∧ π.

- The extension of the rule of replacement is that an occurrence of α in β

- may be replaced by α(cid:48) if (π → α) ≡s (π → α(cid:48)) where π is the premiss of α in

- β. Thus in a subcase one needs only prove equivalence under the premiss of

- the subcase.

- 3.2 Recursion Induction

- Suppose a function f is deﬁned recursively by

- (1)

f (x1, ..., xn) = E {x1, ..., xn, f }

- where E is an expression that in general contains f . Suppose that A is the set

- of n-tuples (x1, ..., xn) for which f is deﬁned. Now let g and h be two other

- functions with the same domain as f and which are deﬁned for all n-tuples

- in A. Suppose further that g and h satisfy the equation which deﬁned f . We

- assert that

g(x1, ..., xn) = h(x1, ..., xn)

- for all (x1, ..., xn) in A. This is so, simply because equation (1) uniquely

- determines the value that any function satisfying it has for arguments in A

- which in turn follows from the fact that (1) can be used to compute f (x1, ..., xn)

- for (x1, ..., xn) in A.

- We shall call this method of proving two functions equivalent by the name

- of recursion induction.

- We shall develop some of the properties of the elementary functions of

- integers in order to illustrate proof by recursion induction. We recall the

- deﬁnitions

m + n = (n = 0 → m, T → m(cid:48) + n−)

mn = (n = 0 → 0, T → m + mn−)

T h. 1. m + 0 = m

P roof m + 0 = (0 = 0 → m, T → m(cid:48) + 0−)

= m.

- Only the deﬁnition of addition and the properties of conditional expressions

- were used in this proof.

- T h. 2. (m + n)(cid:48) = m(cid:48) + n

- P roof Deﬁne f (m, n) = (n = 0 → m(cid:48), T → f (m, n−)). It is easily seen that

- f (m, n) converges for all m and n and hence is completely deﬁned by the above

- equation and is computable from it. Now

(m + n)(cid:48) = (n = 0 → m, T → (m(cid:48) + n−)(cid:48))

= (n = 0 → m(cid:48), T → (m(cid:48) + n−)(cid:48)), while

m(cid:48) + n = (n = 0 → m(cid:48), T → (m(cid:48))(cid:48) + n−).

- It is easily seen that the functions g and h deﬁned by the equations g(m, n) =

- (m + n)(cid:48) and h(m, n) = m(cid:48) + n both satisfy the equation f . For example, it

- is clear that g(m(cid:48), n) = (m(cid:48) + n−)(cid:48) and h(m(cid:48), n−) = (m(cid:48))(cid:48) + n−. Therefore, by

- the principle of recursion induction h and g are equivalent functions on the

- domain of where f is deﬁned, but this is the set of all pairs of integers.

- The fact that the above deﬁned f (m, n) converges for all m and n is a case

- of the more general fact that all functions deﬁned by equations of the form

f (n, x, ..., z) = (n = 0 → g(x, ..., z), T → h(n, x, ..., z,

f (n−, r(x, ..., z), ..., t(x, ..., z)),

f (n−, u(x, ..., z), ..., w(x, ..., z)), etc.))

- converge. We are not yet able to discuss formal proofs of convergence.

- In presenting further proofs we shall be more terse.

- T h. 3. (m + n) + p = (m + p) + n.

- Proof Let f (m, n, p) = (p = 0 → m + n, T → f (m(cid:48), n, p−)). Again f converges

- for all m, n, p. We have

(m + n) + p = (p = 0 → m + n, T → (m + n)(cid:48) + p−)

= (p = 0 → m + n, T → (m(cid:48) + n) + p−) using Th. 2.

(m + p) + n = (p = 0 → m, T → m(cid:48) + p−) + n

= (p = 0 → m + n, T → (m(cid:48) + p−) + n).

- Each of these forms satisﬁes the equation for f (m, n, p).

- Setting m = 0 in Theorem 3 gives

(0 + n) + p = (0 + p) + n

- so that if we had 0 + m = m we would have commutativity of addition.

- In fact, we cannot prove 0 + m = m without making some assumptions

- that take into account that we are dealing with the integers. For suppose our

- space consisted of the vertices of the binary tree in ﬁgure 2, where

(cid:83)

(cid:19)

(cid:83)

(cid:19)

(cid:83)

b

(cid:28)(cid:28)

(cid:64)(cid:64)

(cid:28)

(cid:28)

(cid:83)

(cid:83)

(cid:28)

(cid:83)

(cid:83)

(cid:83)

(cid:83)

(cid:83)

(cid:92)

(cid:0)(cid:0)

(cid:92)

(cid:92)

a

(cid:64)

(cid:19)

(cid:19)

(cid:64)

(cid:19)

(cid:19)

(cid:92)(cid:92)

(cid:19)

(cid:19)

(cid:19)

(cid:19)

(cid:19)

(cid:19)

(cid:19)

(cid:83)

(cid:83)

(cid:83)

(cid:19)

(cid:19)

(cid:19)(cid:19)

(cid:83)(cid:83)

Fig. 2

- m(cid:48) is the vertex just above and to the left, and m− is the vertex just below,

- and 0 is the bottom of the tree. m + n can be deﬁned as above and of course

- satisﬁes Theorems 1, 2, and 3 but does not satisfy 0 + m = m. For example,

- in the diagram 0 + a = b although a + 0 = a.

- We shall make the following assumptions:

- 1. m(cid:48) (cid:54)= 0

- 2. (m(cid:48))− = m

- 3. (m (cid:54)= 0) ⊃ ((m−)(cid:48) = m)

- which embody all of Peano’s axioms except the induction axiom.

- T h. 4. 0 + n = n.

Proof Let f (n) = (n = 0 → 0, T → f (n−)(cid:48))

0 + n = (n = 0 → 0, T → 0(cid:48) + n−)

= (n = 0 → 0, T → (0 + n−)(cid:48))

n = (n = 0 → n, T → n)

= (n = 0 → 0, T → (n−)(cid:48))

axiom 3

- T h. 5. m + n = n + m.

- Proof By 3 and 4 as remarked above.

- T h. 6. (m + n) + p = m + (n + p)

Proof (m + n) + p = (m + p) + n Th. 3.

= (p + m) + n Th. 5.

= (p + n) + m Th. 3.

= m + (n + p) Th. 5. twice.

- T h. 7. m · 0 = 0.

Proof m · 0 = (0 = 0 + 0, T → m + n · 0−)

= 0

- T h. 8. 0 · n = 0.

- Proof Letf (n) = (n = 0 → 0, T → f (n−))

0 · n = (n = 0 → 0, T → 0 + 0 · n) = (n = 0 → 0, T → 0 · n)

0 = (n = 0 → 0, T → 0)

- T h. 9. mn(cid:48) = m + mn.

Proof mn(cid:48) = (n(cid:48) = 0 → 0, T → m + m · (n(cid:48))−)

= m + mn

axioms 1 and 2.

- T h. 10. m(n + p) = mn + mp.

Proof Letf (m, n, p) = (p = 0 → mn, T → f (m, n(cid:48), p−))

m(n + p) = m(p = 0 → n, T → n(cid:48) + p−))

= (p = 0 → mn, T → m(n(cid:48) + p−))

mn + mp = mn + (p = 0 → 0, T → m + mp−)

= (p = 0 → mn + 0, T → mn + (m + mp−))

= (p = 0 → mn, T → (mn + m) + mp−)

= (p = 0 → mn, T → mn(cid:48) + mp−)

- Now we shall give some examples of the application of recursion induction

- to proving theorems about functions of symbolic expressions. The rest of these

- proofs depend on an acquaintance with the Lisp formalism.

- We start with the basic identities.

car[cons[x; y]] = x

cdr[cons[x; y]] = y

∼ atom[x] ⊃ cons[car[x]; cdr[x] = x

atom[cons[x; y]] = F

null[x] = eq[x; NIL]

- Let us deﬁne the concatenation x∗y of two lists x and y by the formula

x∗y = [null[x] → y; T → cons[car[x]; cdr[x]∗y]]

- Our ﬁrst objective is to show that concatenation is associative.

- T h. 11. [x∗y]∗z = x∗[y∗z].

- Proof

- We shall show that [x∗y]∗z and x∗[y∗z] satisfy the functional equation

f [x; y; z] = [null[x] → y∗z; T → cons[car[x]; f [cdr[x]; y; z]]]

- First we establish an auxiliary result:

- cons[a; u]∗v = [null[cons[a; u]] → v; T → cons[car[cons[a; u]]; cdr[cons[a; u]]∗v]] = cons[a; u∗v]

- Now we write

[x∗y]∗z = [null[x] → y; T → cons[car[x]; cdr[x]∗y]]∗z

= [null[x] → y∗z; T → cons[car[x]; cdr[x]∗y]∗z]

= [null[x] → y∗z; T → cons[car[x]; cdr[x]∗y]∗z]]

- and

x∗[y∗z] = [null[x] → y∗z; T → cons[car[x]; cdr[x]∗[y∗z]]].

- From these results it is obvious that both [x∗y]∗z and [x∗[y∗z] satisfy the

- functional equation.

T h. 12. NIL∗x = x

x∗NIL = x.

= x

Proof NIL∗x = [null[NIL] → x; T → cons[car[NIL]; cdr[NIL]∗x]]

x∗NIL = [null[x] → NIL; T → cons[car[x]; cdr[x]∗NIL]].

- Let f [x] = [null[x] → NIL; T → cons[car[x]; f [cdr[x]]]]. x∗NIL satisﬁes this

- equation. We can also write for any list x

x = [null[x] → x; T → x]

= [null[x] → NIL; T → cons[car[x]; cdr[x]]]

- which also satisﬁes the equation.

- Next we consider the function reverse[x] deﬁned by

reverse[x] = [null[x] → NIL; T → reverse[cdr[x]]∗cons[car[x]; NIL].

- It is not diﬃcult to prove by recursion induction that

- and

reverse[x∗y] = reverse[y]∗reverse[x]

reverse[reverse[x]] = x.

- Many other elementary results in the elementary theory of numbers and

- in the elementary theory of symbolic expressions are provable in the same

- straightforward way as the above. In number theory one gets as far as the

- theorem that if a prime p divides ab, then it divides either a or b. However, to

- formulate the unique factorization theorem requires a notation for dealing with

- sets of integers. Wilson’s theorem, a moderately deep result, can be expressed

- in this formalism but apparently cannot be proved by recursion induction.

- One of the most immediate problems in extending this theory is to develop

- better techniques for proving that a recursively deﬁned function converges. We

- hope to ﬁnd some based on ambiguous functions. However, Godel’s theorem

- disallows any hope that a complete set of such rules can be formed.

- The relevance to a theory of computation of this excursion into number

- theory is that the theory illustrates in a simple form mathematical prob-

- lems involved in developing rules for proving the equivalence of algorithms.

- Recursion induction, which was discovered by considering number theoretic

- problems, turns out to be applicable without change to functions of symbolic

- expressions.

- 4 Relation to Other Formalisms

- 4.1 Recursive function theory

- Our characterization of C{F} as the set of functions computable in terms of

- the base functions in F cannot be independently veriﬁed in general since there

- is no other concept with which it can be compared. However, it is not hard

- to show that all partial recursive functions in the sense of Church and Kleene

- are in C{succ, eg}. In order to prove this we shall use the deﬁnition of partial

- recursive functions given by Davis [3]. If we modify deﬁnition 1.1 of page 41

- of Davis [3] to omit reference to oracles we have the following: A function is

- partial recursive if it can be obtained by a ﬁnite number of applications of

- composition and minimalization beginning with the functions on the following

- list:

1) x(cid:48)

2) Uin(x1, ..., xn) = xi, 1 ≤ i ≤ n

3) x + y

4) x − y = (x − y > 0 → x − y, T → 0)

5) xy

- All the above functions are in C{succ, eq}. Any C{F} is closed under

- composition so all that remains is to show that C{succ, eq} is closed under

- the minimalization operation. This operation is deﬁned as follows: The op-

- eration of minimalization associates with each total function f (y, x1, ..., xn)

- the function h(x1, ..., xn) whose value for given x1, ..., xn is the least y for

- whichf (y, x1, ..., xn) = 0, and which is undeﬁned if no such y exists. We have

- to show that if f is in C{succ, eq} so is h. But h may be deﬁned by

h(x1, ..., xn) = h2(0, x1, ..., xn)

- where

h2(y, x1, ..., xn) = (f (, y, x1, ..., xn) = 0 → y, T → h2(y(cid:48), x1, ..., xn)).

- The converse statement that all functions in C{succ, eq} are partial recur-

- sive is presumably also true but not quite so easy to prove.

- It is our opinion that the recursive function formalism based on condi-

- tional expressions presented in this paper is better than the formalisms which

- have heretofore been used in recursive function theory both for practical and

- theoretical purposes. First of all, particular functions in which one may be in-

- terested are more easily written down and the resulting expressions are briefer

- and more understandable. This has been observed in the cases we have looked

- at, and there seems to be a fundamental reason why this is so. This is that both

- the original Church-Kleene formalism and the formalism using the minimaliza-

- tion operation use integer calculations to control the ﬂow of the calculations.

- That this can be done is noteworthy, but controlling the ﬂow in this way is

- less natural than using conditional expressions which control the ﬂow directly.

- A similar objection applies to basing the theory of computation on Turing

- machines. Turing machines are not conceptually diﬀerent from the automatic

- computers in general use, but they are very poor in their control structure.

- Any programmer who has also had to write down Turing machines to com-

- pute functions will observe that one has to invent a few artiﬁces and that

- constructing Turing machines is like programming. Of course, most of the

- theory of computability deals with questions which are not concerned with

- the particular ways computations are represented. It is suﬃcient that com-

- putable functions be represented somehow by symbolic expressions, e.g. num-

- bers, and that functions computable in terms of given functions be somehow

- represented by expressions computable in terms of the expressions represent-

- ing the original functions. However, a practical theory of computation must

- be applicable to particular algorithms. The same objection applies to basing a

- theory of computation on Markov’s [9] normal algorithms as applies to basing

- it on properties of the integers; namely ﬂow of control is described awkwardly.

- The ﬁrst attempt to give a formalism for describing computations that

- allows computations with entities from arbitrary spaces was made by A. P.

- Ershov [4]. However, his formalism uses computations with the symbolic ex-

- pressions representing program steps, and this seems to be an unnecessary

- complication.

- We now discuss the relation between our formalism and computer pro-

- gramming languages. The formalism has been used as the basis for the Lisp

- programming system for computing with symbolic expressions and has turned

- out to be quite practical for this kind of calculation. A particular advantage

- has been that it is easy to write recursive functions that transform programs,

- and this makes compilers and other program generators easy to write.

- The relation between recursive functions and the description of ﬂow con-

- trol by ﬂow charts is described in Reference 7. An ALGOL program can be

- described by a recursive function provided we lump all the variables into a

- single state vector having all the variables as components. If the number of

- components is large and most of the operations performed involve only a few

- of them, it is necessary to have separate names for the components. This

- means that a programming language should include both recursive function

- deﬁnitions and ALGOL-like statements. However, a theory of computation

- certainly must have techniques for proving algorithms equivalent, and so far

- it has seemed easier to develop proof techniques like recursion induction for

- recursive functions than for ALGOL-like programs.

- 4.2 On the Relations between Computation and Math-

ematical Logic

- In what follows computation and mathematical logic will each be taken in a

- wide sense. The subject of computation is essentially that of artiﬁcial intel-

- ligence since the development of computation is in the direction of making

- machines carry out ever more complex and sophisticated processes, i.e.

to

- behave as intelligently as possible. Mathematical logic is concerned with for-

- mal languages, with the representation of information of various mathematical

- and non-mathematical kinds in formal systems, with relations of logical de-

- pendence, and with the process of deduction.

- In discussions of relations between logic and computation there has been

- a tendency to make confused statements, e.g. to say that aspect A of logic is

- identical with aspect B of computation, when actually there is a relation but

- not an identity. We shall try to be precise.

- There is no single relationship between logic and computation which dom-

- inates the others. Here is a list of some of the more important relationships.

- 1. Morphological parallels

- The formal command languages in which procedures are described, e.g.

- ALGOL; the formal languages of mathematical logic, e.g. ﬁrst order predicate

- calculus; and natural languages to some extent: all may be described morpho-

- logically (i.e., one can describe what a Grammatical sentence is) using similar

- syntactical terms. In my opinion, the importance of this relationship has been

- exaggerated, because as soon as one goes into what the sentences mean the

- parallelism disappears.

- 2. Equivalent classes of problems

- Certain classes of problems about computations are equivalent to certain

- classes of problems about formal systems. For example, let

- E1 be the class of Turing machines with initial tapes,

- E2 be the class of formulas of the ﬁrst order predicate calculus,

- E3 be the class of general recursive functions,

- E4 be the class of formulas in a universal Post canonical system,

- E5 ¯be a class of each element which is a Lisp S-function f together with a

- suitable set of arguments a1, ..., ak,

- E6 be a program for a stored program digital computer.

- About E1 we ask: Will the machine ever stop?

- About E2 we ask: Is the formula valid?

- About E3 we ask: Is f (0) deﬁned?

- About E4 we ask: Is the formula a theorem?

- About E5 we ask: Is f [a1; ...; ak] deﬁned?

- About E6 we ask: Will the program ever stop?

- For any pair (Ei,Ej) we can deﬁne a computable map that takes any one

- of the problems about elements of Ei into a corresponding problem about an

- element of Ei and which is such that the problems have the same answer.

- Thus, for any Turing machine and initial tape we can ﬁnd a corresponding

- formula of the ﬁrst order predicate calculus such that the Turing machine will

- eventually stop if and only if the formula is valid.

- In the case of E6 if we want strict equivalence the computer must be pro-

- vided with an inﬁnite memory of some kind. Practically, any present computer

- has so many states, e.g. 23615

, that we cannot reason from ﬁniteness that a

- computation will terminate or repeat before the solar system comes to an

- end and one is forced to consider problems concerning actual computers by

- methods appropriate to machines with an inﬁnite number of states.

- These results owe much of their importance to the fact that each of the

- problem classes is unsolvable in the sense that for each class there is no ma-

- chine which will solve all the problems in the class. This result can most

- easily be proved for certain classes (traditionally Turing machine), and then

- the equivalence permits its extension to other classes. These results have been

- generalized in various ways. There is the world of Post, Myhill, and others, on

- creative sets and the work of Kleene on hierarchies of unsolvability. Some of

- this world is of potential interest for computation even though the generation

- of new unsolvable classes of problems does not in itself seem to be of great

- interest for computation.

- 3. Proof procedures and proof checking procedures

- The next relation stems from the fact that computers can be used to carry

- out the algorithms that are being devised to generate proofs of sentences in

- various formal systems. These formal systems may have any subject matter of

- interest in mathematics, in science, or concerning the relation of an intelligent

- computer program to its environment. The formal system on which the most

- work has been done is the ﬁrst order predicate calculus which is particularly

- important for several reasons. First, many subjects of interest can be axiom-

- atized within this calculus. Second, it is complete, i.e. every valid formula

- has a proof. Third, although it seems unlikely that the general methods for

- the ﬁrst order predicate calculus will be able to produce proofs of signiﬁcant

- results in the part of arithmetic axiomatizable in this calculus (or in any other

- important domain of mathematics), the development of these general meth-

- ods will provide a measure of what must be left to subject-matter-dependent

- heuristics. It should be understood by the reader that the ﬁrst order predicate

- calculus is undecidable; hence there is no possibility of a program that will

- decide whether a formula is valid. All that can be done is to construct pro-

- grams that will decide some cases and will eventually prove any valid formula

- but which will run on indeﬁnitely in the case of certain invalid formulas.

- Proof-checking by computer may be as important as proof generation. It

- is part of the deﬁnition of formal system that proofs be machine checkable.

- In my forthcoming paper [9], I explore the possibilities and applications of

- machine checked proofs. Because a machine can be asked to do much more

- work in checking a proof than can a human, proofs can be made much easier

- to write in such systems. In particular, proofs can contain a request for the

- machine to explore a tree of possibilities for a conventional proof. The potential

- applications for computer-checked proofs are very large. For example, instead

- of trying out computer programs on test cases until they are debugged, one

- should prove that they have the desired properties.

- Incidentally, it is desirable in this work to use a mildly more general concept

- of formal system. Namely, a formal system consists of a computable predicate

- of the symbolic expressions statement and Proof. We say that Proof is a proof

- of statement provided

check[statement; proof ]

check[statement; proof ]

- has the value T.

- The usefulness of computer checked proofs depends both on the develop-

- ment of types of formal systems in which proofs are easy to write and on the

- formalization of interesting subject domains. It should be remembered that

- the formal systems so far developed by logicians have heretofore quite prop-

- erly had as their objective that it should be convenient to prove metatheorems

- about the systems rather than that it be convenient to prove theorems in the

- systems.

- 4. Use of formal systems by computer programs

- When one instructs a computer to perform a task one uses a sequence of

- imperative sentences. On the other hand, when one instructs a human being to

- perform a task one uses mainly declarative sentences describing the situation

- in which he is to act. A single imperative sentence is then frequently suﬃcient.

- The ability to instruct a person in this way depends on his possession of

- common-sense which we shall deﬁne as the fact that we can count on his

- having available any suﬃciently immediate consequence of what we tell him

- and what we can presume he already knows. In my paper [10] I proposed a

- computer program called the Advice Taker that would have these capabilities

- and discussed its advantages. The main problem in realizing the Advice Taker

- has been devising suitable formal languages covering the subject matter about

- which we want the program to think.

- This experience and others has led me to the conclusion that mathematical

- linguists are making a serious mistake in their almost exclusive concentration

- on the syntax and, even more specially, the grammar of natural languages.

- It is even more important to develop a mathematical understanding and a

- formalization of the kinds of information conveyed in natural language.

- 5 Conclusion: Mathematical Theory of Com-

putation

- In the earlier sections of this paper I have tried to lay a basis for a theory of

- how computations are built up from elementary operations and also of how

- data spaces are built up. The formalism diﬀers from those heretofore used

- in the theory of computability in its emphasis on cases of proving statements

- within the system rather than metatheorems about it. This seems to be a very

- fruitful ﬁeld for further work by logicians.

- It is reasonable to hope that the relationship between computation and

- mathematical logic will be as fruitful in the next century as that between

- analysis and physics in the last. The development of this relationship demands

- a concern for both applications and for mathematical elegance.

- 6 REFERENCES

- [1] CHURCH, A., The Calculi of Lambda-Conversion, Annals of Mathematics

- Studies, no. 6, Princeton, 1941, Princeton University Press.

- [2] –, Introduction to Mathematical Logic, Princeton, 1952, Princeton University

- Press.

- [3] DAVIS, M., Computability and Unsolvability, New York, 1958, McGraw-Hill.

- [4] ERSHOV, A. P., On Operator Algorithms (Russian), Doklady Akademii

- Nauk, vol 122, no. 6, pp. 967-970.

- [5] KLEENE, S.C., Recursive Predicates and Quantiﬁers, Transactions of the

- American Mathematical Society, vol. 53, 1953, p. 41.

- [6] MCCARTHY, J., letter to the editor, Communications of the Association

- for Computing Machinery, vol. 2, August, 1959, p. 2.

- [7] –, Recursive Functions of Symbolic Expressions and Their Computation by

- Machine, Part I, Communications Of the ACM, vol. 3, April, 1960, pp. 184-195.

- [8] –, The LISP Programmer’s Manual, M.I.T. Computation Center, 1960.

- [9] –, Computer Programs for Checking Mathematical Proofs, to be published in

- the Proceedings of the American Mathematical Society’s Symposium

- on Recursive Function Theory, held in New York, April, 1961.

- [10] –, Programs With Common Sense, Proceedings of the Teddington

Conference on the Mechanization of Thought Processes,

H. M. Stationery Oﬃce, 1960.

- [11] MARKOV, A.A., Theory of Algorithms (Russian), Moscow, 1954,

USSR Academy of Sciences, Steklov Mathematical Institute.

- [12] NAUR, P., et al., Report on the Algorithmic Language ALGOL 60,

Communications of the ACM, vol. 3, May 1960.

- [13] TURING, A.M., On Computable Numbers with an Application to the

Entscheidungs Problem, Proceedings of the London Mathematical Society,

ser. 2, vol. 43, 1937, p. 230; correction, ibid, vol. 43, 1937, p. 544.

- [14] YANOV, Y.I., The Logical Schemes of Algorithms, from Problems of

Cybernetics I, translated from the Russian by Nadler, Griﬃths, Kiss,

and Muir, New York, 1960, Pergamon Press Ltd., pp. 82-140.

