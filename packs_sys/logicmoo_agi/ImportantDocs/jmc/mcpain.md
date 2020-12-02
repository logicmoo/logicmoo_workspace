CORRECTNESS OF A COMPILER FOR

ARITHMETIC EXPRESSIONS∗

JOHN McCARTHY and JAMES PAINTER

Introduction

- This paper contains a proof of the correctness of a simple compiling algorithm

- for compiling arithmetic expressions into machine language.

- The deﬁnition of correctness, the formalism used to express the description

- of source language, object language and compiler, and the methods of proof are

- all intended to serve as prototypes for the more complicated task of proving the

- correctness of usable compilers. The ultimate goal, as outlined in references

- [1], [2], [3] and [4] is to make it possible to use a computer to check proofs that

- compilers are correct.

- The concepts of abstract syntax, state vector, the use of an interpreter

- for deﬁning the semantics of a programming language, and the deﬁnition of

- correctness of a compiler are all the same as in [3]. The present paper, however,

- is the ﬁrst in which the correctness of a compiler is proved.

- The expressions dealt with in this paper are formed from constants and

- variables. The only operation allowed is a binary + although no change in

- method would be required to include any other binary operations. An example

- of an expression that can be compiled is

(x + 3) + (x + (y + 2))

- ∗This is a reprint with minor changes of ”Correctness of a Compiler for Arithmetic Ex-

- pressions” by John McCarthy and James Painter which was published in MATHEMATICAL

- ASPECTS OF COMPUTER SCIENCE 1, which was Volume 19 of Proceedings of Symposia

- in Applied Mathematics and published by the American Mathematical Society in 1967

- although, because we use abstract syntax, no commitment to a particular

- notation is made.

- The computer language into which these expressions are compiled is a

- single address computer with an accumulator, called ac, and four instructions:

- li (load immediate), load, sto (store) and add. Note that there are no jump

- instructions. Needless to say, this is a severe restriction on the generality of

- our results which we shall overcome in future work.

- The compiler produces code that computes the value of the expression

- being compiled and leaves this value in the accumulator. The above expression

- is compiled into code which in assembly language might look as follows:

load

sto

li

add

sto

load

sto

load

sto

li

add

add

add

x

t

t

t

x

t + 1

y

t + 2

t + 2

t + 1

t

- Again because we are using abstract syntax there is no commitment to a

- precise form for the object code.

- 2 The source language

- The abstract analytic syntax of the source expressions is given by the table:

associated functions

predicate

isconst(e)

isvar(e)

issum(e)

s1(e) s2(e)

- which asserts that the expressions comprise constants, variables and binary

- sums, that the predicates isconst, isvar, and issum enable one to classify

- each expression and that each sum e has summands s1(e) and s2(e).

- The semantics is given by the formula

- (2.1)

value(e, ξ) = if isconst(e) then val(e) else if isvar(e) then c(e, ξ)

else if issum(e) then value(s1(e), ξ) + value(s2(e), ξ)

- where val(e) gives the numerical value of an expression e representing a con-

- stant, c(e, ξ) gives the value of the variable e in the state vector ξ and + is some

- binary operation. (It is natural to regard + as an operation that resembles

- addition of real numbers, but our results do not depend on this).

- For our present purposes we do not have to give a synthetic syntax for

- the source language expressions since both the interpreter and the compiler

- use only the analytic syntax. However, we shall need the following induction

- principle for expressions:

- Suppose Φ is a predicate applicable to expressions, and suppose that for

- all expressions e we have

isconst(e) ⊃ Φ(e)

isvar(e) ⊃ Φ(e)

issum(e) ⊃ Φ(s1(e)) ∧ Φ(s2(e)) ⊃ Φ(e).

and

and

- Then we may conclude that Φ(e) is true for all expressions e.

- 3 The object language.

- We must give both the analytic and synthetic syntaxes for the object language

- because the interpreter deﬁning its semantics uses the analytic syntax and the

- compiler uses the synthetic syntax. We may write the analytic and synthetic

- syntaxes for instructions in the following table.

- operation

- li α

- load x

- sto x

- add x

predicate

isli(s)

isload(s)

issto(s)

isadd(s)

analytic operation

synthetic operation

mkli(α)

mkload(x)

mksto(x)

mkadd(x)

- A program is a list of instructions and null(p) asserts that p is the null list.

- If the program p is not null then ﬁrst(p) gives the ﬁrst instruction and rest(p)

arg(s)

adr(s)

adr(s)

adr(s)

- gives the list of remaining instructions. We shall use the operation p1 ∗ p2 to

- denote the program obtained by appending p2 onto the end of p1. Since we

- have only one level of list we can identify a single instruction with a program

- that has just one instruction.

- The synthetic and analytic syntaxes of instructions are related by the fol-

- lowing.

isli(mkli(α))

α = arg(mkli(α))

isli(s) ⊃ s = mkli(arg(s))

null(rest(mkli(α)))

isli(s) ⊃ f irst(s) = s

isload(mkload(x))

x = adr(mkload(x))

isload(x) ⊃ x = mkload(adr(x))

null(rest(mkload(x)))

isload(s) ⊃ f irst(s) = s

issto(mksto(x))

x = adr(mksto(x))

issto(x) ⊃ x = mksto(adr(x))

null(rest(mksto(x)))

issto(s) ⊃ f irst(s) = s

isadd(mkadd(x))

x = adr(mkadd(x))

isadd(x) ⊃ x = mkadd(adr(x))

null(rest(mkadd(x)))

isadd(x) ⊃ f irst(s) = s

(3.1)

(3.2)

(3.3)

(3.4)

(3.5)

(3.6)

(3.7)

¬ null(p) ⊃ p = f irst(p) ∗ rest(p),

¬null(p1) ∧ null(rest(p1)) ⊃ p1 = f irst(p1 ∗ p2)

null(p1 ∗ p2) ≡ null(p1) ∧ null(p2).

- The ∗ operation is associative.

(The somewhat awkward form of these

- relations comes from having a general concatenation operation rather than

- just an operation that preﬁxes a single instruction onto a program.)

- A state vector for a machine gives, for each register in the machine, its

- contents. We include the accumulator denoted by ac as a register. There are

- two functions of state vectors as introduced in [3], namely

- 1. c(x, η) denotes the value of the contents of register x in machine state

- η.

- 2. a(x, α, η) denotes the state vector that is obtained from the state vec-

- tor η by changing the contents of register x to α leaving the other registers

- unaﬀected.

- These functions satisfy the following relations:

- (3.8)

c(x, a(y, α, η)) = if x = y then α else c(x, η),

- (3.9) a(x, α, a(y, β, η)) = if x = y then a(x, α, η) else a(y, β, a(x, α, η)),

- (3.10)

a(x, c(x, η), η) = η.

- Now we can deﬁne the semantics of the object language by

step(s, η) = if isli(s)then a(ac, arg(s), η)

(3.11)

else if isload(s) then a(ac, c(adr(s), η), η)

else if issto(s) thena(adr(s), c(ac, η), η)

elseif isadd(s) then a(ac, c(adr(s), η) + c(ac, η), η)

- which gives the state vector that results from executing an instruction and

- (3.12)

- outcome(p, η)

= if null(p) then η else outcome(rest(p), step(ﬁrst(p), η))

- which gives the state vector that results from executing the program p with

- state vector η.

- The following lemma is easily proved.

- (3.13)

outcome(p1 ∗ p2, η) = outcome(p2, outcome(p1, η))

- 4 The compiler

- We shall assume that there is a map giving for each variable in the expression

- a location in the main memory of the machine loc(ν, map) gives this location

- and we shall assume

- (4.1)

c(loc(ν, map), η) = c(ν, ξ)

- as a relation between the state vector η before the compiled program starts to

- act and the state vector ξ of the source program.

- Now we can write the compiler. It is

compile(e, t) = if isconst(e) then mkli(val(e))

- (4.2)

else if isvar(e) then mkload(loc(e,map))

else if issum(e) then compile(s1(e), t) ∗ mksto(t) ∗ compile(s2, t + 1) ∗ mkadd(t)- Here t is the number of a register such that all variables are stored in

- registers numbered less than t, so that registers t and above are available for

- temporary storage.

- Before we can state our deﬁnition of correctness of the compiler, we need

- a notion of partial equality for state vectors

ζ1 =A ζ2,

- where ζ1 and ζ2 are state vectors and A is a set of variables means that cor-

- responding components of ζ1 and ζ2 are equal except possibly for values, of

- variables in A. Symbolically, x /∈ A ⊃ c(x, ζ1) = c(x, ζ2). Partial equality

- satisﬁes the following relations:

- (4.3)

ζ1 = ζ2 is equivalent to ζ1 ={} ζ2, where {} denotes the empty set ,

- (4.4)

if A ⊂ B and ζ1 =A ζ2 then ζ1 =B ζ2.

- (4.5)

if ζ1 =A ζ2 then a(x, α, ζ1) =A−{x} a(x, α, ζ2).

- (4.6)

if x ∈ A then a(x, α, ζ) =A ζ,

- (4.7)

if ζ1 =A ζ2 and ζ2 =B ζ3 then ζ1 =A∪B ζ3.

- In our case we need a specialization of this notation and will use

ζ1 =t ζ2 to denote ζ1 ={x|x≥t} ζ2

ζ1 =ac ζ2 to denote ζ1 ={ac} ζ2

- and

- and

ζ1 =t,ac ζ2 to denote ζ1 ={x|x=ac∨x≥t} ζ2.

- The correctness of the compiler is stated in

- THEOREM 1. If η and ξ are machine and source language state vectors

- respectively such that

- (4.8)

c(loc(v, η) = c(v, ξ),

then

outcome(compile(e, t), η) =t a(ac,value(e, ξ), η).

- It states that the result of running the compiled program is to put the

- value of the expression compiled into the accumulator. No registers except the

- accumulator and those with addresses ≥ t are aﬀected.

- 5 Proof of Theorem 1.

- The proof is accomplished by an induction on the expression e being compiled.

- We prove it ﬁrst for constants, then for variables, and then for sums on the

- induction hypothesis that it is true for the summands. Thus there are three

- cases.

- I.

isconst(e). We have

- outcome(compile(e, t), η) = outcome(mkli(val(e)), η)

= step(mkli(val(e)), η)

= a(ac, arg(mkli(val(e))), η)

= a(ac, val(e), η)

= a(ac, value(e, ξ), η)

=t a(ac, value(e, ξ), η).

Justiﬁcation

4.2

3.12, 3.1

3.1, 3.11

3.1

2.1

4.3, 4.4

- II.

isvar(e). We have

- outcome(compile(e, t), η)

= outcome(mkload(loc(e, map)), η)

= a(ac, c(adr(mkload(loc(e))), η), η)

= a(ac, c(loc(e, map), η, η)

= a(ac, c(e, ξ), η)

= a(ac, value(e, ξ), η)

=t a(ac, value(e, ξ), η).

4.2

3.12, 3.2, 3.113.2

4.1

2.1

4.3, 4.4

- III.

issum(e). In this case, we ﬁrst write

outcome(compile(e, t), η)

= outcome(compile(s1(e), t) ∗ mksto(t)

∗compile(s2(e), t + 1) ∗ mkadd(t), η)

by 4.2

= outcome(mkadd(t), outcome(compile(s2(e), t + 1),

outcome(mksto(t), outcome(compile(s1(e), t), η))))

by 3.13

- using the relation between concatenating programs and composing the func-

- tions they represent. Now we introduce some notation. Let

ν = value(e, ξ),

ν1 = value(s1(e), ξ),

ν2 = value(s2(e), ξ),

- so that ν = ν1 + ν2. Further let

ζ1 = outcome(compile(s1(e), t), η),

ζ2 = outcome(mksto(t), ζ1),

ζ3 = outcome(compile(s2(e), t + 1), ζ2),

ζ4 = outcome(mkadd(t), ζ3)

- so that ζ4 = outcome(compile(e, t), η, and we want to prove that

ζ4 =t a(ac, ν, η).

- We have

ζ1 = outcome(compile(s1(e), t), η)

=t a(ac, ν1, η)

Induction Hypothesis

- and

- Now

- and

- Next

c(ac, ζ1) = ν1.

3.8

ζ2 = outcome(mksto(t), ζ1)

= a(t, c(ac, ζ1), ζ1)

= a(t, ν1), ζ1)

=t+1 a(t, ν1, a(ac, ν1, η))

=t+1,ac a(t, ν1, η)

3.12, 3.3, 3.11

Substitution

4.5

4.5, 3.9

c(t, ζ2) = ν1

3.8

ζ3 = outcome(compile(s2(e), t + 1), ζ2)

= t+1a(ac, ν2, ζ2).

- Here we again use the induction hypothesis that s2(e) is compiled correctly.

- In order to apply it, we need c(loc(ν,map),ζ2) = c(ν, ξ) for each variable ν

- which is proved as follows:

c(loc(ν, map), ζ2) = c(loc(ν), map)a(t, ν1, η)) since loc(ν, map) < t

= c(loc(ν, map), η) for the same reason

= c(ν, ξ) by the hypothesis of the theorem.

ζ3 =t+1 a(ac, ν2, a(t, ν1, η))

by 3.9

- Now we can continue with

- Finally,

ζ4 = outcome(mkadd (t), ζ3)

= a(ac, c(t, ζ3) + c(ac, ζ3), ζ3)

= a(ac, ν, ζ3)

=t+1 a(ac, ν, a(ac, ν2, a(t, ν1, η)))

=t+1 a(ac, ν, a(t, ν1, η))

=t a(ac, ν, η).

- This concludes the proof.

Deﬁnition of ν, substitution

3.12, 3.4, 3.114.53.93.9, 4.6, 4.7- 6 Remarks

- The problem of the relations between source language and object language

- arithmetic is dealt with here by assuming that the + signs in formulas (2.1) and

- (3.11) which deﬁne the semantics of the source and object languages represent

- the same operation. Theorem 1 does not depend on any properties of this

- operation, not even commutativity or associativity.

- The proof is entirely straightforward once the necessary machinery has

- been created. Additional operations such as subtraction, multiplication and

- division could be added without essential change in the proof.

- For example, to put multiplication into the system the following changes

- would be required. 1. Add isprod(e), and p1(e), and p2(e) to the abstract

- syntax of the source language.

- 2. Add a term

- to Equation (2.1).

- 3. Add

if isprod(e) then value(p1(e), ζ)× value(p2(e), ζ)

isprod(e) ∧ Φ(p1(e)) ∧ Φ(p2(e)) ⊃ Φ(e)

- to the hypotheses of the source language induction principle.

- 4. Add an instruction mul x and the three syntactical functions ismul(s)

- adr(r), mkmul(x) to the abstract syntax of the object language together with

- the necessary relations among them.

- 5. Add to the deﬁnition (3.11) of step a term

else if ismul(s) then a(ac, c(adr(s), η) × x(ac, η), η).

- 6. Add to the compiler a term

- if isprod(e)thencompile(p1(e), t) ∗ mksto(t) ∗ compile(p2(e), t + 1) ∗ mkmul(t).

- 7. Add to the proof a case isprod(e) which parallels the case issum(e)

- exactly.

- The following other extensions are contemplated. 1. Variable length sums.

- 2. Sequences of assignment statements.

- 3. Conditional expressions.

- 4. go to statements in the source language.

- In order to make these extensions, a complete revision of the formalism will

- be required.

- 7 References

- 1. J. McCarthy, Computer programs for checking mathematical proofs, Proc.

- Sympos. Pure Math. Vol. 5, Amer. Math. Soc., Providence, R. I., 1962, pp.

- 219-227.

- 2. ———–, ”A basis for a mathematical theory of computation” in Com-

- puter programming and formal systems, edited by P. Braﬀort and D. Hersh-

- berg, North-Holland, Amsterdam, 1963.

- 3. ———–, Towards a mathematical theory of computation, Proc. Internat.

- Congr. on Information Processing, 1962.

- 4. ———–, A formal description of a subset of Algol, Proc. Conf. on

- Formal Language Description Languages, Vienna, 1964.

