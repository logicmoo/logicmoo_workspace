- Recursive Functions of Symbolic Expressions

- and Their Computation by Machine, Part I

- John McCarthy, Massachusetts Institute of Technology, Cambridge, Mass. ∗

April 1960

- 1 Introduction

- A programming system called LISP (for LISt Processor) has been developed

- for the IBM 704 computer by the Artiﬁcial Intelligence group at M.I.T. The

- system was designed to facilitate experiments with a proposed system called

- the Advice Taker, whereby a machine could be instructed to handle declarative

- as well as imperative sentences and could exhibit “common sense” in carrying

- out its instructions. The original proposal [1] for the Advice Taker was made

- in November 1958. The main requirement was a programming system for

- manipulating expressions representing formalized declarative and imperative

- sentences so that the Advice Taker system could make deductions.

- In the course of its development the LISP system went through several

- stages of simpliﬁcation and eventually came to be based on a scheme for rep-

- resenting the partial recursive functions of a certain class of symbolic expres-

- sions. This representation is independent of the IBM 704 computer, or of any

- other electronic computer, and it now seems expedient to expound the system

- by starting with the class of expressions called S-expressions and the functions

- called S-functions.

- ∗Putting this paper in LATEXpartly supported by ARPA (ONR) grant N00014-94-1-0775

- to Stanford University where John McCarthy has been since 1962. Copied with minor nota-

- tional changes from CACM, April 1960. If you want the exact typography, look there. Cur-

- rent address, John McCarthy, Computer Science Department, Stanford, CA 94305, (email:

- jmc@cs.stanford.edu), (URL: http://www-formal.stanford.edu/jmc/ )

![140446618146064](images/140446618146064)

- In this article, we ﬁrst describe a formalism for deﬁning functions recur-

- sively. We believe this formalism has advantages both as a programming

- language and as a vehicle for developing a theory of computation. Next, we

- describe S-expressions and S-functions, give some examples, and then describe

- the universal S-function apply which plays the theoretical role of a universal

- Turing machine and the practical role of an interpreter. Then we describe the

- representation of S-expressions in the memory of the IBM 704 by list structures

- similar to those used by Newell, Shaw and Simon [2], and the representation

- of S-functions by program. Then we mention the main features of the LISP

- programming system for the IBM 704. Next comes another way of describ-

- ing computations with symbolic expressions, and ﬁnally we give a recursive

- function interpretation of ﬂow charts.

- We hope to describe some of the symbolic computations for which LISP

- has been used in another paper, and also to give elsewhere some applications

- of our recursive function formalism to mathematical logic and to the problem

- of mechanical theorem proving.

- 2 Functions and Function Deﬁnitions

- We shall need a number of mathematical ideas and notations concerning func-

- tions in general. Most of the ideas are well known, but the notion of conditional

- expression is believed to be new1, and the use of conditional expressions per-

- mits functions to be deﬁned recursively in a new and convenient way.

- a. Partial Functions. A partial function is a function that is deﬁned only

- on part of its domain. Partial functions necessarily arise when functions are

- deﬁned by computations because for some values of the arguments the com-

- putation deﬁning the value of the function may not terminate. However, some

- of our elementary functions will be deﬁned as partial functions.

- b. Propositional Expressions and Predicates. A propositional expression is

- an expression whose possible values are T (for truth) and F (for falsity). We

- shall assume that the reader is familiar with the propositional connectives ∧

- (“and”), ∨ (“or”), and ¬ (“not”). Typical propositional expressions are:

- 1reference Kleene

![140446616929872](images/140446616929872)

x < y

(x < y) ∧ (b = c)

x is prime

- A predicate is a function whose range consists of the truth values T and F.

- c. Conditional Expressions. The dependence of truth values on the values

- of quantities of other kinds is expressed in mathematics by predicates, and the

- dependence of truth values on other truth values by logical connectives. How-

- ever, the notations for expressing symbolically the dependence of quantities of

- other kinds on truth values is inadequate, so that English words and phrases

- are generally used for expressing these dependences in texts that describe other

- dependences symbolically. For example, the function |x| is usually deﬁned in

- words. Conditional expressions are a device for expressing the dependence of

- quantities on propositional quantities. A conditional expression has the form

(p1 → e1, · · · , pn → en)

- where the p’s are propositional expressions and the e’s are expressions of any

- kind. It may be read, “If p1 then e1 otherwise if p2 then e2, · · · , otherwise if

- pn then en,” or “p1 yields e1, · · · , pn yields en.” 2

- We now give the rules for determining whether the value of

(p1 → e1, · · · , pn → en)

- is deﬁned, and if so what its value is. Examine the p’s from left to right. If

- a p whose value is T is encountered before any p whose value is undeﬁned is

- encountered then the value of the conditional expression is the value of the

- corresponding e (if this is deﬁned). If any undeﬁned p is encountered before

- 2I sent a proposal for conditional expressions to a CACM forum on what should be

- included in Algol 60. Because the item was short, the editor demoted it to a letter to the

- editor, for which CACM subsequently apologized. The notation given here was rejected for

- Algol 60, because it had been decided that no new mathematical notation should be allowed

- in Algol 60, and everything new had to be English. The if . . . then . . . else that Algol 60

- adopted was suggested by John Backus.

![140446615734288](images/140446615734288)

- a true p, or if all p’s are false, or if the e corresponding to the ﬁrst true p is

- undeﬁned, then the value of the conditional expression is undeﬁned. We now

- give examples.

(1 < 2 → 4, 1 > 2 → 3) = 4

(2 < 1 → 4, 2 > 1 → 3, 2 > 1 → 2) = 3

(2 < 1 → 4, T → 3) = 3

(2 < 1 →

, T → 3) = 3

(2 < 1 → 3, T →

) is undeﬁned

(2 < 1 → 3, 4 < 1 → 4) is undeﬁned

|x| = (x < 0 → −x, T → x)

δij = (i = j → 1, T → 0)

sgn(x) = (x < 0 → −1, x = 0 → 0, T → 1)

- Some of the simplest applications of conditional expressions are in giving

- such deﬁnitions as

- d. Recursive Function Deﬁnitions. By using conditional expressions we

- can, without circularity, deﬁne functions by formulas in which the deﬁned

- function occurs. For example, we write

n! = (n = 0 → 1, T → n · (n − 1)!)

- When we use this formula to evaluate 0! we get the answer 1; because of the

- way in which the value of a conditional expression was deﬁned, the meaningless

![140446615237136](images/140446615237136)

![140446615239312](images/140446615239312)

- expression 0 · (0 - 1)! does not arise. The evaluation of 2! according to this

- deﬁnition proceeds as follows:

2! = (2 = 0 → 1, T → 2 · (2 − 1)!)

= 2 · 1!

= 2 · (1 = 0 → 1T → ·(1 − 1)!)

= 2 · 1 · 0!

= 2 · 1 · (0 = 0 → 1, T → 0 · (0 − 1)!)

= 2 · 1 · 1

= 2

- We now give two other applications of recursive function deﬁnitions. The

- greatest common divisor, gcd(m,n), of two positive integers m and n is com-

- puted by means of the Euclidean algorithm. This algorithm is expressed by

- the recursive function deﬁnition:

- gcd(m, n) = (m > n → gcd(n, m), rem(n, m) = 0 → m, T → gcd(rem(n, m), m))

- where rem(n, m) denotes the remainder left when n is divided by m.

- The Newtonian algorithm for obtaining an approximate square root of a

- number a, starting with an initial approximation x and requiring that an

- acceptable approximation y satisfy |y2 − a| < (cid:15), may be written as

- sqrt(a, x, (cid:15))

= (|x2 − a| < (cid:15) → x,T → sqrt (a, 1

2(x + a

x), (cid:15)))

- The simultaneous recursive deﬁnition of several functions is also possible,

- and we shall use such deﬁnitions if they are required.

- There is no guarantee that the computation determined by a recursive

- deﬁnition will ever terminate and, for example, an attempt to compute n!

- from our deﬁnition will only succeed if n is a non-negative integer.

If the

- computation does not terminate, the function must be regarded as undeﬁned

- for the given arguments.

- The propositional connectives themselves can be deﬁned by conditional

- expressions. We write

![140446614542352](images/140446614542352)

![140446614543632](images/140446614543632)

p ∧ q = (p → q, T → F )

p ∨ q = (p → T, T → q)

¬p = (p → F, T → T )

p ⊃ q = (p → q, T → T )

- It is readily seen that the right-hand sides of the equations have the correct

- truth tables. If we consider situations in which p or q may be undeﬁned, the

- connectives ∧ and ∨ are seen to be noncommutative. For example if p is false

- and q is undeﬁned, we see that according to the deﬁnitions given above p ∧ q

- is false, but q ∧ p is undeﬁned. For our applications this noncommutativity is

- desirable, since p ∧ q is computed by ﬁrst computing p, and if p is false q is not

- computed. If the computation for p does not terminate, we never get around

- to computing q. We shall use propositional connectives in this sense hereafter.

- e. Functions and Forms. It is usual in mathematics—outside of mathe-

- matical logic—to use the word “function” imprecisely and to apply it to forms

- such as y2 + x. Because we shall later compute with expressions for functions,

- we need a distinction between functions and forms and a notation for express-

- ing this distinction. This distinction and a notation for describing it, from

- which we deviate trivially, is given by Church [3].

- Let f be an expression that stands for a function of two integer variables.

- It should make sense to write f (3, 4) and the value of this expression should be

- determined. The expression y2 + x does not meet this requirement; y2 + x(3, 4)

- is not a conventional notation, and if we attempted to deﬁne it we would be

- uncertain whether its value would turn out to be 13 or 19. Church calls an

- expression like y2 + x, a form. A form can be converted into a function if we

- can determine the correspondence between the variables occurring in the form

- and the ordered list of arguments of the desired function. This is accomplished

- by Church’s λ-notation.

- If E is a form in variables x1, · · · , xn, then λ((x1, · · · , xn), E) will be taken

- to be the function of n variables whose value is determined by substituting

- the arguments for the variables x1, · · · , xn in that order in E and evaluating

- the resulting expression. For example, λ((x, y), y2 + x) is a function of two

- variables, and λ((x, y), y2 + x)(3, 4) = 19.

- The variables occurring in the list of variables of a λ-expression are dummy

- or bound, like variables of integration in a deﬁnite integral. That is, we may

- change the names of the bound variables in a function expression without

- changing the value of the expression, provided that we make the same change

- for each occurrence of the variable and do not make two variables the same

- that previously were diﬀerent. Thus λ((x, y), y2 + x), λ((u, v), v2 + u) and

- λ((y, x), x2 + y) denote the same function.

- We shall frequently use expressions in which some of the variables are

- bound by λ’s and others are not. Such an expression may be regarded as

- deﬁning a function with parameters. The unbound variables are called free

- variables.

- An adequate notation that distinguishes functions from forms allows an

- unambiguous treatment of functions of functions. It would involve too much

- of a digression to give examples here, but we shall use functions with functions

- as arguments later in this report.

- Diﬃculties arise in combining functions described by λ-expressions, or by

- any other notation involving variables, because diﬀerent bound variables may

- be represented by the same symbol. This is called collision of bound variables.

- There is a notation involving operators that are called combinators for com-

- bining functions without the use of variables. Unfortunately, the combinatory

- expressions for interesting combinations of functions tend to be lengthy and

- unreadable.

- f. Expressions for Recursive Functions. The λ-notation is inadequate for

- naming functions deﬁned recursively. For example, using λ’s, we can convert

- the deﬁnition

sqrt(a, x, (cid:15)) = (|x2 − a| < (cid:15) → x, T → sqrt(a,

(x +

), (cid:15)))

- into

sqrt = λ((a, x, (cid:15)), (|x2 − a| < (cid:15) → x, T → sqrt(a,

(x +

), (cid:15)))),

- but the right-hand side cannot serve as an expression for the function be-

- cause there would be nothing to indicate that the reference to sqrt within the

- expression stood for the expression as a whole.

- In order to be able to write expressions for recursive functions, we intro-

- duce another notation.

label(a, E) denotes the expression E, provided that

- occurrences of a within E are to be interpreted as referring to the expression

a

x

a

x

![140446611825232](images/140446611825232)

![140446611826192](images/140446611826192)

![140446611859728](images/140446611859728)

![140446611860688](images/140446611860688)

- as a whole. Thus we can write

- label(sqrt, λ((a, x, (cid:15)), (|x2 − a| < (cid:15) → x, T → sqrt(a, 1

2(x + a

x ), (cid:15)))))

- as a name for our sqrt function.

- The symbol a in label (a, E) is also bound, that is, it may be altered

It behaves

- systematically without changing the meaning of the expression.

- diﬀerently from a variable bound by a λ, however.

- 3 Recursive Functions of Symbolic Expressions

- We shall ﬁrst deﬁne a class of symbolic expressions in terms of ordered pairs

- and lists. Then we shall deﬁne ﬁve elementary functions and predicates, and

- build from them by composition, conditional expressions, and recursive def-

- initions an extensive class of functions of which we shall give a number of

- examples. We shall then show how these functions themselves can be ex-

- pressed as symbolic expressions, and we shall deﬁne a universal function apply

- that allows us to compute from the expression for a given function its value

- for given arguments. Finally, we shall deﬁne some functions with functions as

- arguments and give some useful examples.

- a. A Class of Symbolic Expressions. We shall now deﬁne the S-expressions

- (S stands for symbolic). They are formed by using the special characters

- and an inﬁnite set of distinguishable atomic symbols. For atomic symbols,

- we shall use strings of capital Latin letters and digits with single imbedded

·

)

(

![140446611282576](images/140446611282576)

![140446611283536](images/140446611283536)

- blanks.3 Examples of atomic symbols are

A

ABA

AP P LE P IE N U M BER 3

- There is a twofold reason for departing from the usual mathematical prac-

- tice of using single letters for atomic symbols. First, computer programs fre-

- quently require hundreds of distinguishable symbols that must be formed from

- the 47 characters that are printable by the IBM 704 computer. Second, it is

- convenient to allow English words and phrases to stand for atomic entities for

- mnemonic reasons. The symbols are atomic in the sense that any substructure

- they may have as sequences of characters is ignored. We assume only that dif-

- ferent symbols can be distinguished. S-expressions are then deﬁned as follows:

- 1. Atomic symbols are S-expressions.

- 2. If e1 and e2 are S-expressions, so is (e1 · e2).

- Examples of S-expressions are

AB

(A · B)

((AB · C) · D)

- An S-expression is then simply an ordered pair, the terms of which may be

- atomic symbols or simpler S-expressions. We can can represent a list of arbi-

- trary length in terms of S-expressions as follows. The list

- is represented by the S-expression

(m1, m2, · · · , mn)

(m1 · (m2 · (· · · (mn · N IL) · · ·)))

- Here N IL is an atomic symbol used to terminate lists. Since many of the

- symbolic expressions with which we deal are conveniently expressed as lists,

- we shall introduce a list notation to abbreviate certain S-expressions. We have

- 31995 remark: Imbedded blanks could be allowed within symbols, because lists were then

- written with commas between elements.

![140446610015312](images/140446610015312)

- l. (m) stands for (m ·N IL).

- 2. (m1, · · · , mn) stands for (m1 · (· · · (mn · N IL) · · ·)).

- 3. (m1, · · · , mn · x) stands for (m1 · (· · · (mn · x) · · ·)).

- Subexpressions can be similarly abbreviated. Some examples of these ab-

- breviations are

- ((AB, C), D) for ((AB · (C · N IL)) · (D · N IL))

- ((A, B), C, D · E) for ((A · (B · N IL)) · (C · (D · E)))

- Since we regard the expressions with commas as abbreviations for those

- not involving commas, we shall refer to them all as S-expressions.

- b. Functions of S-expressions and the Expressions That Represent Them.

- We now deﬁne a class of functions of S-expressions. The expressions represent-

- ing these functions are written in a conventional functional notation. However,

- in order to clearly distinguish the expressions representing functions from S-

- expressions, we shall use sequences of lower-case letters for function names

- and variables ranging over the set of S-expressions. We also use brackets and

- semicolons, instead of parentheses and commas, for denoting the application

- of functions to their arguments. Thus we write

car[x]

car[cons[(A · B); x]]

- In these M-expressions (meta-expressions) any S-expression that occur stand

- for themselves.

- c. The Elementary S-functions and Predicates. We introduce the following

- 1. atom. atom[x] has the value of T or F according to whether x is an

- functions and predicates:

- atomic symbol. Thus

- atom [X] = T

- atom [(X · A)] = F

- 2. eq. eq [x;y] is deﬁned if and only if both x and y are atomic. eq [x; y]

- = T if x and y are the same symbol, and eq [x; y] = F otherwise. Thus

- eq [X; X] = T

- eq [X; A] = F

- eq [X; (X · A)] is undeﬁned.

- Thus car [X] is undeﬁned.

- car [(X · A)] = X

- car [((X · A) · Y )] = (X · A)

- 3. car. car[x] is deﬁned if and only if x is not atomic. car [(e1 · e2)] = e1.

- 4.

cdr.

cdr [x] is also deﬁned when x is not atomic. We have cdr

- [(e1 · e2)] = e2. Thus cdr [X] is undeﬁned.

- cdr [(X · A)] = A cdr [((X · A) · Y )] = Y

- 5. cons. cons [x; y] is deﬁned for any x and y. We have cons [e1; e2] =

- (e1 · e2). Thus

- cons [X; A] = (X A)

- cons [(X · A); Y ] = ((X · A)Y )

- car, cdr, and cons are easily seen to satisfy the relations

- car [cons [x; y]] = x

- cdr [cons [x; y]] = y

- cons [car [x]; cdr [x]] = x, provided that x is not atomic.

- The names “car” and “cons” will come to have mnemonic signiﬁcance only

- when we discuss the representation of the system in the computer. Composi-

- tions of car and cdr give the subexpressions of a given expression in a given

- position. Compositions of cons form expressions of a given structure out of

- parts. The class of functions which can be formed in this way is quite limited

- and not very interesting.

- d. Recursive S-functions. We get a much larger class of functions (in fact,

- all computable functions) when we allow ourselves to form new functions of

- S-expressions by conditional expressions and recursive deﬁnition. We now give

- some examples of functions that are deﬁnable in this way.

- 1. ﬀ[x]. The value of ﬀ[x] is the ﬁrst atomic symbol of the S-expression x

- with the parentheses ignored. Thus

- We have

ﬀ[((A · B) · C)] = A

ﬀ[x] = [atom[x] → x; T → ﬀ[car[x]]]

- We now trace in detail the steps in the evaluation of

- ﬀ [((A · B) · C)]:

- ﬀ [((A · B) · C)]

= [atom[((A · B) · C)] → ((A · B) · C); T → ﬀ[car[((A · B)C·)]]]

= [F → ((A · B) · C); T → ﬀ[car[((A · B) · C)]]]

= [T → ﬀ[car[((A · B) · C)]]]

= ﬀ[car[((A · B) · C)]]

= ﬀ[(A · B)]

= [atom[(A · B)] → (A · B); T → ﬀ[car[(A · B)]]]

= [F → (A · B); T → ﬀ[car[(A · B)]]]

= [T → ﬀ[car[(A · B)]]]

= ﬀ[car[(A · B)]]

= ﬀ[A]

= [atom[A] → A; T → ﬀ[car[A]]]

= [T → A; T → ﬀ[car[A]]]

= A

- 2.

subst [x; y; z]. This function gives the result of substituting the S-

- expression x for all occurrences of the atomic symbol y in the S-expression z.

- It is deﬁned by

- subst [x; y; z] = [atom [z] → [eq [z; y] → x; T → z];

- T → cons [subst [x; y; car [z]]; subst [x; y; cdr [z]]]]

- As an example, we have

subst[(X · A); B; ((A · B) · C)] = ((A · (X · A)) · C)

- 3. equal [x; y]. This is a predicate that has the value T if x and y are the

- same S-expression, and has the value F otherwise. We have

- equal [x; y] = [atom [x] ∧ atom [y] ∧ eq [x; y]]

- ∨[¬ atom [x] ∧¬ atom [y] ∧ equal [car [x]; car [y]]

∧ equal [cdr [x]; cdr [y]]]

- It is convenient to see how the elementary functions look in the abbreviated

- list notation. The reader will easily verify that

- (i) car[(m1, m2, · · · , mn)] = m1

- (ii) cdr[(ms, m2, · · · , mn)] = (m2, · · · , mn)

- (iii) cdr[(m)] = N IL

- (iv) cons[m1; (m2, · · · , mn)] = (m1, m2, · · · , mn)

- (v) cons[m; N IL] = (m)

- We deﬁne

- null[x] = atom[x] ∧ eq[x; N IL]

- This predicate is useful in dealing with lists.

- Compositions of car and cdr arise so frequently that many expressions can

- be written more concisely if we abbreviate

- cadr[x] for car[cdr[x]],

- caddr[x] for car[cdr[cdr[x]]], etc.

- Another useful abbreviation is to write list [e1; e2; · · · ; en]

- for cons[e1; cons[e2; · · · ; cons[en; N IL] · · ·]].

- This function gives the list, (e1, · · · , en), as a function of its elements.

- The following functions are useful when S-expressions are regarded as lists.

- append [x; y] = [null[x] → y; T → cons [car [x]; append [cdr [x]; y]]]

- 1. append [x;y].

- An example is

- append [(A, B); (C, D, E)] = (A, B, C, D, E)

- 2. among [x;y]. This predicate is true if the S-expression x occurs among

- the elements of the list y. We have

among[x; y] = ¬null[y] ∧ [equal[x; car[y]] ∨ among[x; cdr[y]]]

- 3. pair [x;y]. This function gives the list of pairs of corresponding elements

- of the lists x and y. We have

- pair[x; y] = [null[x]∧null[y] → N IL; ¬atom[x]∧¬atom[y] → cons[list[car[x]; car[y]]; pair[cdr[x]; cdr[y]]]

- An example is

pair[(A, B, C); (X, (Y, Z), U)] = ((A, X), (B, (Y, Z)), (C, U)).

- 4. assoc [x;y]. If y is a list of the form ((u1, v1), · · · , (un, vn)) and x is one

- of the u’s, then assoc [x; y] is the corresponding v. We have

assoc[x; y] = eq[caar[y]; x] → cadar[y]; T → assoc[x; cdr[y]]]

- An example is

assoc[X; ((W, (A, B)), (X, (C, D)), (Y, (E, F )))] = (C, D).

- 5. sublis[x; y]. Here x is assumed to have the form of a list of pairs

- ((u1, v1), · · · , (un, vn)), where the u’s are atomic, and y may be any S-expression.

- The value of sublis[x; y] is the result of substituting each v for the correspond-

- ing u in y. In order to deﬁne sublis, we ﬁrst deﬁne an auxiliary function. We

- have

- sub2[x; z] = [null[x] → z; eq[caar[x]; z] → cadar[x]; T → sub2[cdr[x]; z]]

- sublis[x; y] = [atom[y] → sub2[x; y]; T → cons[sublis[x; car[y]]; sublis[x; cdr[y]]]

- and

- We have

- sublis [((X, (A, B)), (Y, (B, C))); (A, X · Y)] = (A, (A, B), B, C)

- e. Representation of S-Functions by S-Expressions. S-functions have been

- described by M-expressions. We now give a rule for translating M-expressions

- into S-expressions, in order to be able to use S-functions for making certain

- computations with S-functions and for answering certain questions about S-

- functions.

- The translation is determined by the following rules in rich we denote the

- translation of an M-expression E by E*.

- 1. If E is an S-expression E* is (QUOTE, E).

- 2. Variables and function names that were represented by strings of lower-

- case letters are translated to the corresponding strings of the corresponding

- uppercase letters. Thus car* is CAR, and subst* is SUBST.

- 3. A form f [e1; · · · ; en] is translated to (f ∗, e∗

1 · · · , e∗

n). Thus cons [car [x];

- cdr [x]]∗ is (CONS, (CAR, X), (CDR, X)).

- 4. {[p1 → e1; · · · ; pn → en]}∗ is (COND, (p∗

1, e∗

1), · · · , (p∗

n · e∗

n)).

- 5. {λ[[x1; · · · ; xn]; E]}∗ is (LAMBDA, (x∗

- 6. {label[a; E]}∗ is (LABEL, a∗, E ∗).

- With these conventions the substitution function whose M-expression is

- label [subst; λ [[x; y; z]; [atom [z] → [eq [y; z] → x; T → z]; T → cons [subst

- [x; y; car [z]]; subst [x; y; cdr [z]]]]]] has the S-expression

1, · · · , x∗

n), E ∗).

- (LABEL, SUBST, (LAMBDA, (X, Y, Z), (COND ((ATOM, Z), (COND,

- (EQ, Y, Z), X), ((QUOTE, T), Z))), ((QUOTE, T), (CONS, (SUBST, X, Y,

- (CAR Z)), (SUBST, X, Y, (CDR, Z)))))))

- This notation is writable and somewhat readable. It can be made easier

If more

- to read and write at the cost of making its structure less regular.

- characters were available on the computer, it could be improved considerably.4

- f. The Universal S-Function apply. There is an S-function apply with the

- property that if f is an S-expression for an S-function f 0 and args is a list of

- arguments of the form (arg1, · · · , argn), where arg1, · · · , argn are arbitrary S-

- expressions, then apply[f ; args] and f 0[arg1; · · · ; argn] are deﬁned for the same

- values of arg1, · · · , argn, and are equal when deﬁned. For example,

λ[[x; y]; cons[car[x]; y]][(A, B); (C, D)]

- = apply[(LAM BDA, (X, Y ), (CON S, (CAR, X), Y )); ((A, B), (C, D))] = (A, C, D)

- The S-function apply is deﬁned by

apply[f ; args] = eval[cons[f ; appq[args]]; N IL],

- appq[m] = [null[m] → N IL; T → cons[list[QU OT E; car[m]]; appq[cdr[m]]]]

- where

- and

- eval[e; a] = [

- 41995: More characters were made available on SAIL and later on the Lisp machines.

- Alas, the world went back to inferior character sets again—though not as far back as when

- this paper was written in early 1959.

![140446604526544](images/140446604526544)

- atom [e] → assoc [e; a];

- atom [car [e]] → [

- eq [car [e]; QUOTE] → cadr [e];

- eq [car [e]; ATOM] → atom [eval [cadr [e]; a]];

- eq [car [e]; EQ] → [eval [cadr [e]; a] = eval [caddr [e]; a]];

- eq [car [e]; COND] → evcon [cdr [e]; a];

- eq [car [e]; CAR] → car [eval [cadr [e]; a]];

- eq [car [e]; CDR] → cdr [eval [cadr [e]; a]];

- eq [car [e]; CONS] → cons [eval [cadr [e]; a]; eval [caddr [e];

- a]]; T → eval [cons [assoc [car [e]; a];

- evlis [cdr [e]; a]]; a]];

- eq [caar [e]; LABEL] → eval [cons [caddar [e]; cdr [e]];

- cons [list [cadar [e]; car [e]; a]];

- eq [caar [e]; LAMBDA] → eval [caddar [e];

- append [pair [cadar [e]; evlis [cdr [e]; a]; a]]]

- and

- and

evcon[c; a] = [eval[caar[c]; a] → eval[cadar[c]; a]; T → evcon[cdr[c]; a]]

- evlis[m; a] = [null[m] → N IL; T → cons[eval[car[m]; a]; evlis[cdr[m]; a]]]

- We now explain a number of points about these deﬁnitions. 5

- 1. apply itself forms an expression representing the value of the function

- applied to the arguments, and puts the work of evaluating this expression onto

- a function eval. It uses appq to put quotes around each of the arguments, so

- that eval will regard them as standing for themselves.

- 2. eval[e; a] has two arguments, an expression e to be evaluated, and a list

- of pairs a. The ﬁrst item of each pair is an atomic symbol, and the second is

- the expression for which the symbol stands.

- 3. If the expression to be evaluated is atomic, eval evaluates whatever is

- paired with it ﬁrst on the list a.

- 4. If e is not atomic but car[e] is atomic, then the expression has one of the

- forms (QU OT E, e) or (AT OM, e) or (EQ, e1, e2) or (CON D, (p1, e1), · · · , (pn, en)),

- or (CAR, e) or (CDR, e) or (CON S, e1, e2) or (f, e1, · · · , en) where f is an

- atomic symbol.

- In the case (QU OT E, e) the expression e, itself, is taken. In the case of

- (AT OM, e) or (CAR, e) or (CDR, e) the expression e is evaluated and the

- appropriate function taken. In the case of (EQ, e1, e2) or (CON S, e1, e2) two

- expressions have to be evaluated. In the case of (CON D, (p1, e1), · · · (pn, en))

- the p’s have to be evaluated in order until a true p is found, and then the

- corresponding e must be evaluated. This is accomplished by evcon. Finally, in

- the case of (f, e1, · · · , en) we evaluate the expression that results from replacing

- f in this expression by whatever it is paired with in the list a.

- 5. The evaluation of ((LABEL, f, E), e1, · · · , en) is accomplished by eval-

- uating (E , e1, · · · , en) with the pairing (f, (LABEL, f, E)) put on the front of

- the previous list a of pairs.

- 6. Finally, the evaluation of ((LAM BDA, (x1, · · · , xn), E), e1, · · · en) is ac-

- complished by evaluating E with the list of pairs ((x1, e1), · · · , ((xn, en)) put

- on the front of the previous list a.

- The list a could be eliminated, and LAMBDA and LABEL expressions

- evaluated by substituting the arguments for the variables in the expressions

- E. Unfortunately, diﬃculties involving collisions of bound variables arise, but

- they are avoided by using the list a.

- 51995: This version isn’t quite right. A comparison of this and other versions of eval

- including what was actually implemented (and debugged) is given in “The Inﬂuence of the

- Designer on the Design” by Herbert Stoyan and included in Artiﬁcial Intelligence and Math-

- ematical Theory of Computation: Papers in Honor of John McCarthy, Vladimir Lifschitz

- (ed.), Academic Press, 1991

![140446602680464](images/140446602680464)

- Calculating the values of functions by using apply is an activity better

- suited to electronic computers than to people. As an illustration, however, we

- now give some of the steps for calculating

- apply [(LABEL, FF, (LAMBDA, (X), (COND, (ATOM, X), X), ((QUOTE,

- T),(FF, (CAR, X))))));((A· B))] = A

- The ﬁrst argument is the S-expression that represents the function ﬀ deﬁned

- in section 3d. We shall abbreviate it by using the letter φ. We have

- apply [φ; ( (A·B) )]

= eval [((LABEL, FF, ψ), (QUOTE, (A·B))); NIL]

where ψ is the part of φ beginning (LAMBDA

= eval[((LAMBDA, (X), ω), (QUOTE, (A·B)));((FF, φ))]

where ω is the part of ψ beginning (COND

= eval [(COND, (π1, (cid:15)1), (π2, (cid:15)2)); ((X, (QUOTE, (A·B) ) ), (FF, φ) )]

Denoting ((X, (QUOTE, (A·B))), (FF, φ)) by a, we obtain

= atom [eval [assoc [X; ((X, (QUOTE, (A·B))), (FF,φ))];a]]

= atom [eval [(QUOTE, (A·B)); a]]

= evcon [((π1, (cid:15)1), (π2, (cid:15)2)); a]

This involves eval [π1; a]

= eval [( ATOM, X); a]

= atom [eval [X; a]]

= atom [(A·B)],

= F

Our main calulation continues with

- apply [φ; ((A·B))]

= evcon [((π2, (cid:15)2, )); a],

- which involves eval [π2; a] = eval [(QUOTE, T); a] = T

- Our main calculation again continues with

- apply [φ; ((A·B))]

- = eval [(cid:15)2; a]

- = eval [(FF, (CAR, X));a]

- = eval [Cons [φ; evlis [((CAR, X)); a]]; a]

- Evaluating evlis [((CAR, X)); a] involves

- eval [(CAR, X); a]

- = car [eval [X; a]]

- and so evlis [((CAR, X)); a] then becomes

list [list [QUOTE; A]] = ((QUOTE, A)),

- and our main quantity becomes

- = eval [(φ, (QUOTE, A)); a]

- The subsequent steps are made as in the beginning of the calculation. The

- LABEL and LAMBDA cause new pairs to be added to a, which gives a new

- list of pairs a1. The π1 term of the conditional eval [(ATOM, X); a1] has the

- = car [(A·B)], where we took steps from the earlier computation of atom [eval [X; a]] = A,

- value T because X is paired with (QUOTE, A) ﬁrst in a1, rather than with

- (QUOTE, (A·B)) as in a.

- Therefore we end up with eval [X; a1] from the evcon, and this is just A.

- g. Functions with Functions as Arguments. There are a number of useful

- functions some of whose arguments are functions. They are especially useful

- in deﬁning other functions. One such function is maplist[x; f ] with an S-

- expression argument x and an argument f that is a function from S-expressions

- to S-expressions. We deﬁne

maplist[x; f ] = [null[x] → N IL; T → cons[f [x]; maplist[cdr[x]; f ]]]

- The usefulness of maplist is illustrated by formulas for the partial derivative

- with respect to x of expressions involving sums and products of x and other

- variables. The S-expressions that we shall diﬀerentiate are formed as follows.

- 1. An atomic symbol is an allowed expression.

- 2. If e1, e2, · · · , en are allowed expressions, ( PLUS, e1, · · · , en) and (TIMES,

- e1, · · · , en) are also, and represent the sum and product, respectively, of e1, · · · , en.

- This is, essentially, the Polish notation for functions, except that the in-

- clusion of parentheses and commas allows functions of variable numbers of

- arguments. An example of an allowed expression is (TIMES, X, (PLUS, X,

- A), Y), the conventional algebraic notation for which is X(X + A)Y.

- Our diﬀerentiation formula, which gives the derivative of y with respect to

- x, is

- diﬀ [y; x] = [atom [y] → [eq [y; x] → ONE; T → ZERO]; eq [car [Y]; PLUS]

- → cons [PLUS; maplist [cdr [y]; λ[[z]; diﬀ [car [z]; x]]]]; eq[car [y]; TIMES] →

- cons[PLUS; maplist[cdr[y]; λ[[z]; cons [TIMES; maplist[cdr [y]; λ[[w]; ¬ eq [z;

- w] → car [w]; T → diﬀ [car [[w]; x]]]]]]]

- The derivative of the expression (TIMES, X, (PLUS, X, A), Y), as com-

- puted by this formula, is

- (PLUS, (TIMES, ONE, (PLUS, X, A), Y), (TIMES, X, (PLUS, ONE,

- ZERO), Y), (TIMES, X, (PLUS, X, A), ZERO))

- Besides maplist, another useful function with functional arguments is search,

- which is deﬁned as

- search[x; p; f ; u] = [null[x] → u; p[x] → f [x]; T → search[cdr[x]; p; f ; u]

- The function search is used to search a list for an element that has the property

- p, and if such an element is found, f of that element is taken. If there is no

- such element, the function u of no arguments is computed.

- 4 The LISP Programming System

- The LISP programming system is a system for using the IBM 704 computer to

- compute with symbolic information in the form of S-expressions. It has been

- or will be used for the following purposes:

- l. Writing a compiler to compile LISP programs into machine language.

- 2. Writing a program to check proofs in a class of formal logical systems.

- 3. Writing programs for formal diﬀerentiation and integration.

- 4. Writing programs to realize various algorithms for generating proofs in

- 5. Making certain engineering calculations whose results are formulas

- predicate calculus.

- rather than numbers.

- 6. Programming the Advice Taker system.

- The basis of the system is a way of writing computer programs to evaluate

- S-functions. This will be described in the following sections.

- In addition to the facilities for describing S-functions, there are facilities

- for using S-functions in programs written as sequences of statements along the

- lines of FORTRAN (4) or ALGOL (5). These features will not be described

- in this article.

- a. Representation of S-Expressions by List Structure. A list structure is a

- collection of computer words arranged as in ﬁgure 1a or 1b. Each word of the

- list structure is represented by one of the subdivided rectangles in the ﬁgure.

- The lef t box of a rectangle represents the address ﬁeld of the word and the

- right box represents the decrement ﬁeld. An arrow from a box to another

- rectangle means that the ﬁeld corresponding to the box contains the location

- of the word corresponding to the other rectangle.

![140446599076368](images/140446599076368)

![140446599110224](images/140446599110224)

![140446598603664](images/140446598603664)

![140446598757904](images/140446598757904)

![140446598721424](images/140446598721424)

![140446598754576](images/140446598754576)

![140446598757520](images/140446598757520)

-

-

-

-

-

-

-

-

-

-

- -

-

-

- -

- --

- -

-

-

-

-

-

-

Fig. 1

- It is permitted for a substructure to occur in more than one place in a list

- structure, as in ﬁgure 1b, but it is not permitted for a structure to have cycles,

- as in ﬁgure 1c. An atomic symbol is represented in the computer by a list

- structure of special form called the association list of the symbol. The address

- ﬁeld of the ﬁrst word contains a special constant which enables the program to

- tell that this word represents an atomic symbol. We shall describe association

- lists in section 4b.

- An S-expression x that is not atomic is represented by a word, the address

- and decrement parts of which contain the locations of the subexpressions car[x]

- and cdr[x], respectively.

to denote the

- locations of the association list of these symbols, then the S-expression ((A ·

- B) · (C · (E · F ))) is represented by the list structure a of ﬁgure 2. Turning

- to the list form of S-expressions, we see that the S-expression (A, (B, C), D),

- which is an abbreviation for (A · ((B · (C · N IL)) · (D · N IL))), is represented

- by the list structure of ﬁgure 2b.

If we use the symbols A, B, etc.

![140446599076624](images/140446599076624)

![140446599076880](images/140446599076880)

![140446599077392](images/140446599077392)

![140446599110480](images/140446599110480)

![140446598602896](images/140446598602896)

![140446598603408](images/140446598603408)

![140446598603920](images/140446598603920)

![140446598604176](images/140446598604176)

![140446598604688](images/140446598604688)

![140446599077136](images/140446599077136)

![140446598603152](images/140446598603152)

![140446598604432](images/140446598604432)

![140446598758224](images/140446598758224)

![140446598783120](images/140446598783120)

![140446598604944](images/140446598604944)

![140446598637520](images/140446598637520)

![140446599075088](images/140446599075088)

![140446599107664](images/140446599107664)

![140446598754960](images/140446598754960)

![140446598755280](images/140446598755280)

![140446598757136](images/140446598757136)

![140446598783376](images/140446598783376)

![140446598783632](images/140446598783632)

![140446598785104](images/140446598785104)

![140446598605200](images/140446598605200)

![140446598605456](images/140446598605456)

![140446598605968](images/140446598605968)

![140446598637776](images/140446598637776)

![140446598638032](images/140446598638032)

![140446598638544](images/140446598638544)

![140446598605712](images/140446598605712)

![140446598638288](images/140446598638288)

![140446599075344](images/140446599075344)

![140446599075600](images/140446599075600)

![140446599076112](images/140446599076112)

![140446599107920](images/140446599107920)

![140446599108176](images/140446599108176)

![140446599108688](images/140446599108688)

![140446599075856](images/140446599075856)

![140446599108432](images/140446599108432)

![140446598755600](images/140446598755600)

![140446598755920](images/140446598755920)

![140446598783952](images/140446598783952)

![140446598784272](images/140446598784272)

![140446598606224](images/140446598606224)

![140446598636240](images/140446598636240)

![140446599077648](images/140446599077648)

![140446599108944](images/140446599108944)

![140446598756176](images/140446598756176)

![140446598756432](images/140446598756432)

![140446598756752](images/140446598756752)

![140446598784528](images/140446598784528)

![140446598784784](images/140446598784784)

![140446598785488](images/140446598785488)

![140446598606480](images/140446598606480)

![140446598606736](images/140446598606736)

![140446598635984](images/140446598635984)

![140446598636496](images/140446598636496)

![140446598636752](images/140446598636752)

![140446598637264](images/140446598637264)

![140446598635728](images/140446598635728)

![140446598637008](images/140446598637008)

![140446599106640](images/140446599106640)

![140446599106896](images/140446599106896)

![140446599107408](images/140446599107408)

![140446599109200](images/140446599109200)

![140446599109456](images/140446599109456)

![140446599109968](images/140446599109968)

![140446599107152](images/140446599107152)

![140446599109712](images/140446599109712)

![140446598846864](images/140446598846864)

![140446598638800](images/140446598638800)

![140446598698576](images/140446598698576)

![140446598699856](images/140446598699856)

![140446598717584](images/140446598717584)

![140446598718864](images/140446598718864)

![140446598720144](images/140446598720144)

![140446598785872](images/140446598785872)

![140446598847120](images/140446598847120)

![140446598786192](images/140446598786192)

![140446598786512](images/140446598786512)

![140446598786896](images/140446598786896)

![140446598847376](images/140446598847376)

![140446598846608](images/140446598846608)

![140446598844880](images/140446598844880)

![140446598845200](images/140446598845200)

![140446598845904](images/140446598845904)

![140446598846288](images/140446598846288)

![140446598639056](images/140446598639056)

![140446598639312](images/140446598639312)

![140446598668560](images/140446598668560)

![140446598698832](images/140446598698832)

![140446598699088](images/140446598699088)

![140446598699600](images/140446598699600)

![140446598700112](images/140446598700112)

![140446598700368](images/140446598700368)

![140446598700880](images/140446598700880)

![140446598845520](images/140446598845520)

![140446598639568](images/140446598639568)

![140446598699344](images/140446598699344)

![140446598700624](images/140446598700624)

![140446598717840](images/140446598717840)

![140446598718096](images/140446598718096)

![140446598718608](images/140446598718608)

![140446598719120](images/140446598719120)

![140446598719376](images/140446598719376)

![140446598719888](images/140446598719888)

![140446598720400](images/140446598720400)

![140446598720656](images/140446598720656)

![140446598721168](images/140446598721168)

![140446598718352](images/140446598718352)

![140446598719632](images/140446598719632)

![140446598720912](images/140446598720912)

![140446598821264](images/140446598821264)

![140446598821584](images/140446598821584)

![140446598820112](images/140446598820112)

![140446598820432](images/140446598820432)

![140446598668816](images/140446598668816)

![140446598697296](images/140446598697296)

![140446598820688](images/140446598820688)

![140446598820944](images/140446598820944)

![140446598844496](images/140446598844496)

![140446598821840](images/140446598821840)

![140446598822096](images/140446598822096)

![140446598669072](images/140446598669072)

![140446598669328](images/140446598669328)

![140446598669840](images/140446598669840)

![140446598697552](images/140446598697552)

![140446598697808](images/140446598697808)

![140446598698320](images/140446598698320)

![140446598669584](images/140446598669584)

![140446598698064](images/140446598698064)

![140446598822416](images/140446598822416)

![140446598822736](images/140446598822736)

![140446598670096](images/140446598670096)

![140446598671376](images/140446598671376)

![140446598822992](images/140446598822992)

![140446598823248](images/140446598823248)

![140446598823568](images/140446598823568)

![140446598670352](images/140446598670352)

![140446598670608](images/140446598670608)

![140446598671120](images/140446598671120)

![140446598671632](images/140446598671632)

![140446598671888](images/140446598671888)

![140446598697040](images/140446598697040)

![140446598670864](images/140446598670864)

![140446598672144](images/140446598672144)

![140446598064656](images/140446598064656)

![140446598065936](images/140446598065936)

![140446597563472](images/140446597563472)

![140446598030800](images/140446598030800)

![140446597621136](images/140446597621136)

![140446597621456](images/140446597621456)

![140446597621776](images/140446597621776)

![140446597622160](images/140446597622160)

![140446597587856](images/140446597587856)

![140446598064912](images/140446598064912)

![140446598065168](images/140446598065168)

![140446598065680](images/140446598065680)

![140446597562448](images/140446597562448)

![140446597562704](images/140446597562704)

![140446597563216](images/140446597563216)

![140446597563728](images/140446597563728)

![140446597563984](images/140446597563984)

![140446597564496](images/140446597564496)

![140446597588112](images/140446597588112)

![140446597589840](images/140446597589840)

![140446598065424](images/140446598065424)

![140446597562960](images/140446597562960)

![140446597564240](images/140446597564240)

- -

- -

A B

-

C

-

E F

(a)

-

A

-

-

D

-

B

-

C

(b)

Figure 2

- When a list structure is regarded as representing a list, we see that each term

- of the list occupies the address part of a word, the decrement part of which

- points to the word containing the next term, while the last word has NIL in

- its decrement.

- An expression that has a given subexpression occurring more than once

- can be represented in more than one way. Whether the list structure for

- the subexpression is or is not repeated depends upon the history of the pro-

- gram. Whether or not a subexpression is repeated will make no diﬀerence

- in the results of a program as they appear outside the machine, although it

- will aﬀect the time and storage requirements. For example, the S-expression

- ((A·B)·(A·B)) can be represented by either the list structure of ﬁgure 3a or

- 3b.

- -

-

-

A B

A B

(a)

-

--

A B

(b)

Figure 3

- The prohibition against circular list structures is essentially a prohibition

![140446598031056](images/140446598031056)

![140446598031312](images/140446598031312)

![140446598031824](images/140446598031824)

![140446598033360](images/140446598033360)

![140446598031568](images/140446598031568)

![140446597590160](images/140446597590160)

![140446597590416](images/140446597590416)

![140446597590736](images/140446597590736)

![140446597588432](images/140446597588432)

![140446597588752](images/140446597588752)

![140446598062352](images/140446598062352)

![140446598062608](images/140446598062608)

![140446598063120](images/140446598063120)

![140446597622544](images/140446597622544)

![140446597622864](images/140446597622864)

![140446598062864](images/140446598062864)

![140446597619792](images/140446597619792)

![140446597620048](images/140446597620048)

![140446597564752](images/140446597564752)

![140446597566032](images/140446597566032)

![140446598063376](images/140446598063376)

![140446597623120](images/140446597623120)

![140446597620304](images/140446597620304)

![140446597623376](images/140446597623376)

![140446597623696](images/140446597623696)

![140446597620560](images/140446597620560)

![140446597620816](images/140446597620816)

![140446597565008](images/140446597565008)

![140446597565264](images/140446597565264)

![140446597565776](images/140446597565776)

![140446597566288](images/140446597566288)

![140446597587088](images/140446597587088)

![140446597587600](images/140446597587600)

![140446597565520](images/140446597565520)

![140446597587344](images/140446597587344)

![140446598032080](images/140446598032080)

![140446598063632](images/140446598063632)

![140446598063888](images/140446598063888)

![140446598064400](images/140446598064400)

![140446598064144](images/140446598064144)

![140446597589008](images/140446597589008)

![140446597589264](images/140446597589264)

![140446597589520](images/140446597589520)

![140446598032336](images/140446598032336)

![140446598032592](images/140446598032592)

![140446598033104](images/140446598033104)

![140446598032848](images/140446598032848)

![140446597464272](images/140446597464272)

![140446597493136](images/140446597493136)

![140446597531408](images/140446597531408)

![140446597531728](images/140446597531728)

![140446597495696](images/140446597495696)

![140446597495952](images/140446597495952)

![140446597464912](images/140446597464912)

![140446597465168](images/140446597465168)

![140446597465680](images/140446597465680)

![140446597493392](images/140446597493392)

![140446597493648](images/140446597493648)

![140446597494160](images/140446597494160)

![140446597465424](images/140446597465424)

![140446597493904](images/140446597493904)

![140446597496272](images/140446597496272)

![140446597530256](images/140446597530256)

![140446597532048](images/140446597532048)

![140446597496592](images/140446597496592)

![140446597530576](images/140446597530576)

![140446597532368](images/140446597532368)

![140446597533200](images/140446597533200)

![140446597533520](images/140446597533520)

![140446597465936](images/140446597465936)

![140446597467216](images/140446597467216)

![140446597494416](images/140446597494416)

![140446597529680](images/140446597529680)

![140446597530832](images/140446597530832)

![140446597030032](images/140446597030032)

![140446597529936](images/140446597529936)

![140446597531088](images/140446597531088)

![140446597030288](images/140446597030288)

![140446597532624](images/140446597532624)

![140446597532880](images/140446597532880)

![140446597466192](images/140446597466192)

![140446597466448](images/140446597466448)

![140446597466960](images/140446597466960)

![140446597467472](images/140446597467472)

![140446597467728](images/140446597467728)

![140446597492880](images/140446597492880)

![140446597466704](images/140446597466704)

![140446597467984](images/140446597467984)

![140446597494672](images/140446597494672)

![140446597494928](images/140446597494928)

![140446597495440](images/140446597495440)

![140446597495184](images/140446597495184)

- against an expression being a subexpression of itself. Such an expression could

- not exist on paper in a world with our topology. Circular list structures would

- have some advantages in the machine, for example, for representing recursive

- functions, but diﬃculties in printing them, and in certain other operations,

- make it seem advisable not to use them for the present.

- The advantages of list structures for the storage of symbolic expressions

- are:

- 1. The size and even the number of expressions with which the program

- will have to deal cannot be predicted in advance. Therefore, it is diﬃcult to

- arrange blocks of storage of ﬁxed length to contain them.

- 2. Registers can be put back on the free-storage list when they are no longer

- needed. Even one register returned to the list is of value, but if expressions

- are stored linearly, it is diﬃcult to make use of blocks of registers of odd sizes

- that may become available.

- 3. An expression that occurs as a subexpression of several expressions need

- be represented in storage only once.

- b. Association Lists6 . In the LISP programming system we put more in

- the association list of a symbol than is required by the mathematical system

- described in the previous sections. In fact, any information that we desire to

- associate with the symbol may be put on the association list. This information

- may include: the print name, that is, the string of letters and digits which

- represents the symbol outside the machine; a numerical value if the symbol

- represents a number; another S-expression if the symbol, in some way, serves

- as a name for it; or the location of a routine if the symbol represents a function

- for which there is a machine-language subroutine. All this implies that in the

- machine system there are more primitive entities than have been described in

- the sections on the mathematical system.

- For the present, we shall only describe how print names are represented

- on association lists so that in reading or printing the program can establish

- a correspondence between information on punched cards, magnetic tape or

- printed page and the list structure inside the machine. The association list of

- the symbol DIFFERENTIATE has a segment of the form shown in ﬁgure 4.

- Here pname is a symbol that indicates that the structure for the print name

- of the symbol whose association list this is hanging from the next word on

- the association list.

In the second row of the ﬁgure we have a list of three

- words. The address part of each of these words points to a Word containing

- 61995: These were later called property lists.

![140446595759952](images/140446595759952)

- six 6-bit characters. The last word is ﬁlled out with a 6-bit combination that

- does not represent a character printable by the computer. (Recall that the

- IBM 7O4 has a 36-bit word and that printable characters are each represented

- by 6 bits.) The presence of the words with character information means that

- the association lists do not themselves represent S-expressions, and that only

- some of the functions for dealing with S-expressions make sense within an

- association list.

-

pname

- ...

-

-

....

-

-

-

-

-

-

DIFFER

ENTIAT

E ??????

Figure 4

- c. Free-Storage List. At any given time only a part of the memory reserved

- for list structures will actually be in use for storing S-expressions. The remain-

- ing registers (in our system the number, initially, is approximately 15,000) are

- arranged in a single list called the free-storage list. A certain register, FREE,

- in the program contains the location of the ﬁrst register in this list. When

- a word is required to form some additional list structure, the ﬁrst word on

- the free-storage list is taken and the number in register FREE is changed to

- become the location of the second word on the free-storage list. No provision

- need be made for the user to program the return of registers to the free-storage

- list.

- This return takes place automatically, approximately as follows (it is nec-

- essary to give a simpliﬁed description of this process in this report): There is

- a ﬁxed set of base registers in the program which contains the locations of list

- structures that are accessible to the program. Of course, because list struc-

- tures branch, an arbitrary number of registers may be involved. Each register

- that is accessible to the program is accessible because it can be reached from

- one or more of the base registers by a chain of car and cdr operations. When

![140446595190736](images/140446595190736)

![140446595253776](images/140446595253776)

![140446595255376](images/140446595255376)

![140446595255760](images/140446595255760)

![140446595255056](images/140446595255056)

![140446595221456](images/140446595221456)

![140446595221712](images/140446595221712)

![140446595222224](images/140446595222224)

![140446595254032](images/140446595254032)

![140446595254288](images/140446595254288)

![140446595254800](images/140446595254800)

![140446595221968](images/140446595221968)

![140446595254544](images/140446595254544)

![140446595256144](images/140446595256144)

![140446595285200](images/140446595285200)

![140446595222480](images/140446595222480)

![140446595252496](images/140446595252496)

![140446595286800](images/140446595286800)

![140446595285456](images/140446595285456)

![140446595285712](images/140446595285712)

![140446595286032](images/140446595286032)

![140446595286416](images/140446595286416)

![140446595222736](images/140446595222736)

![140446595222992](images/140446595222992)

![140446595223504](images/140446595223504)

![140446595252752](images/140446595252752)

![140446595253008](images/140446595253008)

![140446595253520](images/140446595253520)

![140446595287120](images/140446595287120)

![140446595287376](images/140446595287376)

![140446595287888](images/140446595287888)

![140446595223248](images/140446595223248)

![140446595253264](images/140446595253264)

![140446595287632](images/140446595287632)

![140446595312848](images/140446595312848)

![140446595342736](images/140446595342736)

![140446595313168](images/140446595313168)

![140446595343056](images/140446595343056)

![140446595311760](images/140446595311760)

![140446595312016](images/140446595312016)

![140446595288144](images/140446595288144)

![140446595309712](images/140446595309712)

![140446595310736](images/140446595310736)

![140446595312272](images/140446595312272)

![140446595313424](images/140446595313424)

![140446595312528](images/140446595312528)

![140446595342416](images/140446595342416)

![140446595343312](images/140446595343312)

![140446595343568](images/140446595343568)

![140446595288400](images/140446595288400)

![140446595288656](images/140446595288656)

![140446595309968](images/140446595309968)

![140446595310224](images/140446595310224)

![140446595310992](images/140446595310992)

![140446595311248](images/140446595311248)

![140446595288912](images/140446595288912)

![140446595310480](images/140446595310480)

![140446595311504](images/140446595311504)

- the contents of a base register are changed, it may happen that the register

- to which the base register formerly pointed cannot be reached by a car − cdr

- chain from any base register. Such a register may be considered abandoned

- by the program because its contents can no longer be found by any possible

- program; hence its contents are no longer of interest, and so we would like to

- have it back on the free-storage list. This comes about in the following way.

- Nothing happens until the program runs out of free storage. When a free

- register is wanted, and there is none left on the free-storage list, a reclamation7

- cycle starts.

- First, the program ﬁnds all registers accessible from the base registers and

- makes their signs negative. This is accomplished by starting from each of the

- base registers and changing the sign of every register that can be reached from

- it by a car − cdr chain. If the program encounters a register in this process

- which already has a negative sign, it assumes that this register has already

- been reached.

- After all of the accessible registers have had their signs changed, the pro-

- gram goes through the area of memory reserved for the storage of list structures

- and puts all the registers whose signs were not changed in the previous step

- back on the free-storage list, and makes the signs of the accessible registers

- positive again.

- This process, because it is entirely automatic, is more convenient for the

- programmer than a system in which he has to keep track of and erase un-

- wanted lists. Its eﬃciency depends upon not coming close to exhausting the

- available memory with accessible lists. This is because the reclamation process

- requires several seconds to execute, and therefore must result in the addition

- of at least several thousand registers to the free-storage list if the program is

- not to spend most of its time in reclamation.

- d. Elementary S-Functions in the Computer. We shall now describe the

- computer representations of atom, = , car, cdr, and cons. An S-expression

- is communicated to the program that represents a function as the location of

- the word representing it, and the programs give S-expression answers in the

- same form.

- atom. As stated above, a word representing an atomic symbol has a special

- 7We already called this process “garbage collection”, but I guess I chickened out of using

- it in the paper—or else the Research Laboratory of Electronics grammar ladies wouldn’t let

- me.

![140446593518032](images/140446593518032)

- constant in its address part: atom is programmed as an open subroutine that

- tests this part. Unless the M-expression atom[e] occurs as a condition in a

- conditional expression, the symbol T or F is generated as the result of the

- test. In case of a conditional expression, a conditional transfer is used and the

- symbol T or F is not generated.

- eq. The program for eq[e; f ] involves testing for the numerical equality of

- the locations of the words. This works because each atomic symbol has only

- one association list. As with atom, the result is either a conditional transfer

- or one of the symbols T or F .

- car. Computing car[x] involves getting the contents of the address part of

- register x. This is essentially accomplished by the single instruction CLA 0, i,

- where the argument is in index register, and the result appears in the address

- part of the accumulator.

(We take the view that the places from which a

- function takes its arguments and into which it puts its results are prescribed

- in the deﬁnition of the function, and it is the responsibility of the programmer

- or the compiler to insert the required datamoving instructions to get the results

- of one calculation in position for the next.) (“car” is a mnemonic for “contents

- of the address part of register.”)

- cdr. cdr is handled in the same way as car, except that the result appears

- in the decrement part of the accumulator (“cdr” stands for “contents of the

- decrement part of register.”)

- cons. The value of cons[x; y] must be the location of a register that has x

- and y in its address and decrement parts, respectively. There may not be such

- a register in the computer and, even if there were, it would be time-consuming

- to ﬁnd it. Actually, what we do is to take the ﬁrst available register from the

- free-storage list, put x and y in the address and decrement parts, respectively,

- and make the value of the function the location of the register taken. (“cons”

- is an abbreviation for “construct.”)

- It is the subroutine for cons that initiates the reclamation when the free-

- storage list is exhausted. In the version of the system that is used at present

- cons is represented by a closed subroutine. In the compiled version, cons is

- open.

- e. Representation of S-Functions by Programs. The compilation of func-

- tions that are compositions of car, cdr, and cons, either by hand or by a

- compiler program, is straightforward. Conditional expressions give no trouble

- except that they must be so compiled that only the p’s and e’s that are re-

- quired are computed. However, problems arise in the compilation of recursive

- functions.

- In general (we shall discuss an exception), the routine for a recursive func-

- tion uses itself as a subroutine. For example, the program for subst[x; y; z] uses

- itself as a subroutine to evaluate the result of substituting into the subexpres-

- sions car[z] and cdr[z]. While subst[x; y; cdr[z]] is being evaluated, the result

- of the previous evaluation of subst[x; y; car[z]] must be saved in a temporary

- storage register. However, subst may need the same register for evaluating

- subst[x; y; cdr[z]]. This possible conﬂict is resolved by the SAVE and UN-

- SAVE routines that use the public push-down list 8. The SAVE routine is

- entered at the beginning of the routine for the recursive function with a re-

- quest to save a given set of consecutive registers. A block of registers called

- the public push-down list is reserved for this purpose. The SAVE routine has

- an index that tells it how many registers in the push-down list are already

- in use.

It moves the contents of the registers which are to be saved to the

- ﬁrst unused registers in the push-down list, advances the index of the list, and

- returns to the program from which control came. This program may then

- freely use these registers for temporary storage. Before the routine exits it

- uses UNSAVE, which restores the contents of the temporary registers from

- the push-down list and moves back the index of this list. The result of these

- conventions is described, in programming terminology, by saying that the re-

- cursive subroutine is transparent to the temporary storage registers.

- f. Status of the LISP Programming System (February 1960). A variant of

- the function apply described in section 5f has been translated into a program

- APPLY for the IBM 704. Since this routine can compute values of S-functions

- given their descriptions as S-expressions and their arguments, it serves as an

- interpreter for the LISP programming language which describes computation

- processes in this way.

- The program APPLY has been imbedded in the LISP programming system

- which has the following features:

- 1. The programmer may deﬁne any number of S-functions by S-expressions.

- these functions may refer to each other or to certain S-functions represented

- by machine language program.

- 2. The values of deﬁned functions may be computed.

- 3. S-expressions may be read and printed (directly or via magnetic tape).

- 81995: now called a stack

![140446590626832](images/140446590626832)

- 4. Some error diagnostic and selective tracing facilities are included.

- 5. The programmer may have selected S-functions compiled into machine

- language programs put into the core memory. Values of compiled functions

- are computed about 60 times as fast as they would if interpreted. Compilation

- is fast enough so that it is not necessary to punch compiled program for future

- use.

- 6. A “program feature” allows programs containing assignment and go to

- statements in the style of ALGOL.

- 7. Computation with ﬂoating point numbers is possible in the system, but

- this is ineﬃcient.

- 8. A programmer’s manual is being prepared. The LISP programming

- system is appropriate for computations where the data can conveniently be

- represented as symbolic expressions allowing expressions of the same kind as

- subexpressions. A version of the system for the IBM 709 is being prepared.

- 5 Another Formalism for Functions of Sym-

bolic Expressions

- There are a number of ways of deﬁning functions of symbolic expressions which

- are quite similar to the system we have adopted. Each of them involves three

- basic functions, conditional expressions, and recursive function deﬁnitions, but

- the class of expressions corresponding to S-expressions is diﬀerent, and so are

- the precise deﬁnitions of the functions. We shall describe one of these variants

- called linear LISP.

- The L-expressions are deﬁned as follows:

- 1. A ﬁnite list of characters is admitted.

- 2. Any string of admitted characters in an L-expression. This includes the

- null string denoted by Λ.

- There are three functions of strings:

- 1. f irst[x] is the ﬁrst character of the string x.

- f irst[Λ] is undeﬁned. For example: f irst[ABC] = A

- 2. rest[x] is the string of characters which remains when the ﬁrst character

- of the string is deleted.

- rest[Λ] is undeﬁned. For example: rest[ABC] = BC.

- 3. combine[x; y] is the string formed by preﬁxing the character x to the

- string y. For example: combine[A; BC] = ABC

- There are three predicates on strings:

- 1. char[x], x is a single character.

- 2. null[x], x is the null string.

- 3. x = y, deﬁned for x and y characters.

- The advantage of linear LISP is that no characters are given special roles,

- as are parentheses, dots, and commas in LISP. This permits computations

- with all expressions that can be written linearly. The disadvantage of linear

- LISP is that the extraction of subexpressions is a fairly involved, rather than

- an elementary, operation. It is not hard to write, in linear LISP, functions that

- correspond to the basic functions of LISP, so that, mathematically, linear LISP

- includes LISP. This turns out to be the most convenient way of programming,

- in linear LISP, the more complicated manipulations. However, if the functions

- are to be represented by computer routines, LISP is essentially faster.

- 6 Flowcharts and Recursion

- Since both the usual form of computer program and recursive function deﬁ-

- nitions are universal computationally, it is interesting to display the relation

- between them. The translation of recursive symbolic functions into computer

- programs was the subject of the rest of this report. In this section we show

- how to go the other way, at least in principle.

- The state of the machine at any time during a computation is given by the

- values of a number of variables. Let these variables be combined into a vector

- ξ. Consider a program block with one entrance and one exit. It deﬁnes and is

- essentially deﬁned by a certain function f that takes one machine conﬁguration

- into another, that is, f has the form ξ 0 = f (ξ). Let us call f the associated

- function of the program block. Now let a number of such blocks be combined

- into a program by decision elements π that decide after each block is completed

- which block will be entered next. Nevertheless, let the whole program still have

- one entrance and one exit.

![140446588335568](images/140446588335568)

![140446588336592](images/140446588336592)

![140446587921808](images/140446587921808)

![140446587898256](images/140446587898256)

![140446587921488](images/140446587921488)

![140446588359184](images/140446588359184)

![140446588337360](images/140446588337360)

![140446588337872](images/140446588337872)

![140446588358864](images/140446588358864)

![140446587864912](images/140446587864912)

![140446587865936](images/140446587865936)

![140446587865168](images/140446587865168)

![140446587865424](images/140446587865424)

![140446587865680](images/140446587865680)

f1

f2

?

(cid:27)

-?

(cid:7)

(cid:4)

π1

(cid:5)

(cid:6)

XXXz(cid:8)(cid:8)(cid:8)(cid:25)

(cid:17)

HHHj

(cid:17)(cid:17)+

S

(cid:4)

(cid:7)

π2

(cid:5)

(cid:6)

?

(cid:27)

f3

T

?

(cid:7)

(cid:4)

π3

(cid:5)

(cid:6)

?

f4

?

?

Figure 5

- We give as an example the ﬂowcart of ﬁgure 5. Let us describe the function

- r[ξ] that gives the transformation of the vector ξ between entrance and exit

- of the whole block. We shall deﬁne it in conjunction with the functions s(ξ),

- and t[ξ], which give the transformations that ξ undergoes between the points

- S and T, respectively, and the exit. We have

r[ξ] = [π11[ξ] → S[f1[ξ]]; T → S[f2[ξ]]]

S[ξ] = [π21[ξ] → r[ξ]; T → t[f3[ξ]]]

t[ξ] = [π3I[ξ] → f4[ξ]; π32[ξ] → r[ξ]; T → t[f3[ξ]]]

- Given a ﬂowchart with a single entrance and a single exit, it is easy to

- write down the recursive function that gives the transformation of the state

- vector from entrance to exit in terms of the corresponding functions for the

- computation blocks and the predicates of the branch. In general, we proceed

- as follows.

- In ﬁgure 6, let β be an n-way branch point, and let f1, · · · , fn be the

- computations leading to branch points β1, β2, · · · , βn. Let φ be the function

![140446587866192](images/140446587866192)

![140446587866448](images/140446587866448)

![140446587866704](images/140446587866704)

![140446588360720](images/140446588360720)

![140446588359504](images/140446588359504)

![140446588359952](images/140446588359952)

![140446588360400](images/140446588360400)

![140446587921232](images/140446587921232)

![140446587920784](images/140446587920784)

![140446587866960](images/140446587866960)

![140446587863888](images/140446587863888)

![140446587897360](images/140446587897360)

![140446587864144](images/140446587864144)

![140446587864400](images/140446587864400)

![140446587864656](images/140446587864656)

![140446587896016](images/140446587896016)

![140446588362256](images/140446588362256)

![140446587897104](images/140446587897104)

![140446587896784](images/140446587896784)

![140446588361040](images/140446588361040)

![140446588361488](images/140446588361488)

![140446588361936](images/140446588361936)

![140446587898000](images/140446587898000)

![140446587897680](images/140446587897680)

![140446587896400](images/140446587896400)

![140446588362512](images/140446588362512)

![140446587863120](images/140446587863120)

![140446587863376](images/140446587863376)

![140446587863632](images/140446587863632)

![140446588335824](images/140446588335824)

![140446588336080](images/140446588336080)

![140446587922192](images/140446587922192)

![140446588336336](images/140446588336336)

![140446588336912](images/140446588336912)

- that transforms ξ between β and the exit of the chart, and let φ1, · · · , φn be

- the corresponding functions for β1, · · · , βn. We then write

φ[ξ] = [p1[ξ] → φ1[f1[ξ]]; · · · ; pn[ξ] → φn[ξ]]]

β

(cid:0)

@@

φ

(cid:0)(cid:0)

@

(cid:10)

(cid:10)

@@(cid:0)

@

(cid:0)(cid:0)

A

A

....

?

.....

f2

A

AAU

fn

C

CCW

(cid:11)

(cid:10)

φ2

?

(cid:11)

(cid:10)

β2

(cid:8)

.....

(cid:9)

φn

(cid:8)

(cid:9)

βn

(cid:10)

(cid:10)(cid:10)(cid:29)

f1

(cid:2)

(cid:2)(cid:2)(cid:13)

φ1

(cid:8)

(cid:9)

- (cid:11)

- (cid:10)

- β1

- 7 Acknowledgments

- The inadequacy of the λ-notation for naming recursive functions was noticed

- by N. Rochester, and he discovered an alternative to the solution involving

- label which has been used here. The form of subroutine for cons which per-

- mits its composition with other functions was invented, in connection with

- another programming system, by C. Gerberick and H. L. Gelernter, of IBM

- Corporation. The LlSP programming system was developed by a group in-

- cluding R. Brayton, D. Edwards, P. Fox, L. Hodes, D. Luckham, K. Maling,

- J. McCarthy, D. Park, S. Russell.

- The group was supported by the M.I.T. Computation Center, and by the

- M.I.T. Research Laboratory of Electronics (which is supported in part by the

- the U.S. Army (Signal Corps), the U.S. Air Force (Oﬃce of Scientiﬁc Research,

- Air Research and Development Command), and the U.S. Navy (Oﬃce of Naval

- Research)). The author also wishes to acknowledge the personal ﬁnancial sup-

Figure 6

![140446587122384](images/140446587122384)

![140446587123536](images/140446587123536)

![140446587124560](images/140446587124560)

![140446587184080](images/140446587184080)

![140446587122768](images/140446587122768)

![140446587123024](images/140446587123024)

![140446587123792](images/140446587123792)

![140446587124048](images/140446587124048)

![140446587124816](images/140446587124816)

![140446587125072](images/140446587125072)

![140446587123280](images/140446587123280)

![140446587124304](images/140446587124304)

![140446587125328](images/140446587125328)

![140446587183312](images/140446587183312)

![140446587185616](images/140446587185616)

![140446587155600](images/140446587155600)

![140446587157136](images/140446587157136)

![140446587157456](images/140446587157456)

![140446587157904](images/140446587157904)

![140446587125648](images/140446587125648)

![140446587154832](images/140446587154832)

![140446587155920](images/140446587155920)

![140446587156368](images/140446587156368)

![140446587158352](images/140446587158352)

![140446587155280](images/140446587155280)

![140446587156816](images/140446587156816)

- port of the Alfred P. Sloan Foundation.

REFERENCES

- 1. J. McCARTHY, Programs with common sense, Paper presented at the

- Symposium on the Mechanization of Thought Processes, National Physical

- Laboratory, Teddington, England, Nov. 24-27, 1958. (Published in Proceed-

- ings of the Symposium by H. M. Stationery Oﬃce).

- 2. A. NEWELL AND J. C. SHAW, Programming the logic theory machine,

- Proc. Western Joint Computer Conference, Feb. 1957.

- 3. A. CHURCH, The Calculi of Lambda-Conversion (Princeton University

- Press, Princeton, N. J., 1941).

- York, Oct. 15, 1956.

- 4. FORTRAN Programmer’s Reference Manual, IBM Corporation, New

- 5. A. J. PERLIS AND K. SAMELS0N, International algebraic language,

- Preliminary Report, Comm. Assoc. Comp. Mach., Dec. 1958.

