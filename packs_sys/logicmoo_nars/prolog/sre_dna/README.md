
DCTG-GP - a genetic programming system using definite clause translation grammars

Brian Ross
Brock University
bross@cosc.brocku.ca

October 2001. All rights reserved.


HOW TO USE DCTG-GP.

The following files pertain to the user's experiment:

a) parameters_P.pro: this has a variety of GP system parameter settings. Most are documented in this file, although not all the parameters are functional at this time. 

b) fitness_func_P(File): This is set in parameters_P.pro. The file File is the Prolog file with the user's fitness function code.

c) dctg_file_P(File): This is also set in parameters_P.pro. This is the DCTG grammar file with the user's grammar.

Once these files are coded, then using Sicstus Prolog 3.7 Windows console, do the following:

?-    compile('c:/folder/compile').    % replace folder with the location of the system
?-    gp.

If all works as planned, the system should write a "stats" file for every run with evolution statistics, as well as a "soln" file with the dump of the solution's DCTG structure.


EXAMPLE EXPERIMENT

The archive includes an example experiment that evolves stochastic regular expressions. This example is intended to show a working example of a DCTG grammar with semantics. This is not a very efficient or effective genetic programming experiment, however! It crawls on a Pentium II/400.

The files used are: parameters_P.pro, reg_gram_1.pro, and sre3.pro. 

Good luck!

#### Charles University in Prague

#### Faculty of Mathematics and Physics

### MASTER THESIS

### Adam Nohejl

### Grammar-based genetic programming

#### Department of Software and Computer Science Education

#### Supervisor of the master thesis: RNDr. František Mráz, CSc.

#### Study programme: Computer science

#### Specialisation: Theoretical computer science

#### Prague 2011



I would like to thank to František Mráz, who supervised my work on this
thesis, for his guidance and helpful suggestions. I am also grateful to Dr Man
Leung Wong for providing his implementation of the logic grammar based
framework LOGENPRO.



I declare that I carried out this master thesis independently, and only with the
cited sources, literature and other professional sources.

I understand that my work relates to the rights and obligations under the Act
No. 121 / 2000 Coll., the Copyright Act, as amended, in particular the fact that
the Charles University in Prague has the right to conclude a license agreement
on the use of this work as a school work pursuant to Section 60 paragraph 1 of
the Copyright Act.

Prague, 25 July 2011 Adam Nohejl



Title: Grammar-based genetic programming

Author: Adam Nohejl

Department: Department of Software and Computer Science Education

Supervisor of the master thesis: RNDr. František Mráz, CSc.

Abstract: Tree-based genetic programming (GP) has several known shortcomings: dif-
ficult adaptability to specific programming languages and environments, the problem
of closure and multiple types, and the problem of declarative representation of know-
ledge. Most of the methods that try to solve these problems are based on formal
grammars. The precise effect of their distinctive features is often difficult to analyse
and a good comparison of performance in specific problems is missing. This thesis
reviews three grammar-based methods: context-free grammar genetic programming
(CFG-GP), including its variant GPHH recently applied to exam timetabling, gram-
matical evolution (GE), and LOGENPRO, it discusses how they solve the problems
encountered by GP, and compares them in a series of experiments in six applications
using success rates and derivation tree characteristics. The thesis demonstrates that
neither GE nor LOGENPRO provide a substantial advantage over CFG-GP in any of
the experiments, and analyses the differences between the effects of operators used
in CFG-GP and GE. It also presents results from a highly efficient implementation of
CFG-GP and GE.

Keywords: genetic programming, formal grammar, evolutionary algorithms,
grammatical evolution.

Název práce: Genetické programování založené na gramatikách

Autor: Adam Nohejl

Katedra: Kabinet software a výuky informatiky

Vedoucí diplomové práce: RNDr. František Mráz, CSc.

Abstrakt: Genetické programování (GP) založené na stromech má nˇekolik známých
nedostatk ̊u: složité pˇrizp ̊usobení specifickým programovacím jazyk ̊um a prostˇredím,
problém uzávˇeru a více typ ̊u a problém deklarativní reprezentace vˇedomostí. Vˇetšina
metod, které se snaží tyto problémy vyˇrešit, je založena na formálních gramatikách.
Pˇresné d ̊usledky vlastností, které je odlišují, je tˇežké analyzovat a dobré srovnání
výsledk ̊u v konkrétních problémech chybí. Tato práce zkoumá tˇri metody založené
na gramatikách: genetické programování s bezkontextovými gramatikami (CFG-GP),
vˇcetnˇe jeho varianty GPHH nedávno aplikované na rozvrhování zkoušek, gramatickou
evoluci (GE) a LOGENPRO, pojednává o tom, jak ˇreší problémy GP, a porovnává je v sé-
rii experiment ̊u v šesti aplikacích podle ˇcetností úspˇechu a charakteristik derivaˇcních
strom ̊u. Práce ukazuje, že GE ani LOGENPRO neposkytují podstatnou výhodu v žád-
ném z experiment ̊u a analyzuje rozdíly v úˇcincích operátor ̊u používaných v CFG-GP
a GE. Jsou také prezentovány výsledky velmi efektivní implementace metod CFG-GP
a GE.

Klíˇcová slova: genetické programování, formální gramatika, evoluˇcní algoritmy,
gramatická evoluce.



## Contents


- Introduction
- 1 Grammar-Based Genetic Programming Methods
   - 1 1 Formal grammars and programming
   - 1 2 Genetic programming
   - 1 3 Grammatically biased ILP
   - 1 4 CFG-GP, language bias, and search bias
   - 1 5 Logic grammar based genetic programming: LOGENPRO
   - 1 6 Grammatical evolution
   - 1 7 Common features and shortcomings
   - 1 8 Implications for performance
- 2 Existing Applications
   - 2 1 Simple symbolic regression
   - 2 2 Artificial ant trail
   - 2 3 Symbolic regression with multiple types
   - 2 4 Boolean symbolic regression
   - 2 5 Hyper-heuristics
   - 2 6 Other applications
- 3 Experiments with Grammar-Based Methods
   - 3 1 Observed characteristics
   - 3 2 Setup
   - 3 3 Statistics
   - 3 4 Simple symbolic regression
      - 3 4 1 Experimental setups
      - 3 4 2 Results from setups
      - 3 4 3 Experimental setups
      - 3 4 4 Results from setups
      - 3 4 5 Conclusion
   - 3 5 Santa Fe ant trail
      - 3 5 1 Experimental setups
      - 3 5 2 Results from setups
      - 3 5 3 Experimental setups
      - 3 5 4 Results from setups
      - 3 5 5 Conclusion
   - 3 6 Dot product symbolic regression
      - 3 6 1 Experimental setups
      - 3 6 2 Results
      - 3 6 3 Conclusion
   - 3 7 Symbolic regression with ADFs
      - 3 7 1 Experimental setups
      - 3 7 2 Results
      - 3 7 3 Conclusion
   - 3 8 Boolean parity functions with ADFs
      - 3 8 1 Experimental setups
      - 3 8 2 Results
      - 3 8 3 Conclusion
   - 3 9 Exam timetabling hyper-heuristics
      - 3 9 1 Experimental setup
      - 3 9 2 Results
      - 3 9 3 Computational time
      - 3 9 4 Conclusion
   - 3 10 Conclusion
- 4 Implementation Notes
   - 4 1 Implementation of CFG-GP
   - 4 2 Accompanying files
- Conclusion
- Bibliography
- List of Abbreviations


## Introduction

This thesis explores and analyses methods of genetic programming based on
formal grammars. Genetic programming(GP) is a metaheuristic for deriving
problem solutions in the form of programs based on evolutionary principles,
and thus belonging to the family ofevolutionary algorithms. Traditionally, ge-
netic programming was tied to the Lisp programming language, taking advant-
age of its simplicity and straightforward correspondence between a program
and its parse tree. Several other strains of genetic programming were devised
over time: linear GPsystems departed from the original premise of thetree-
based GPthat programs are trees, and variants of tree-based GP usually either
extended the original genetic programming with new features (such as auto-
matically defined functions, which effectively add co-evolution of subroutines)
or placed restrictions on permitted tree forms (such as strongly-typed genetic
programming).
Variousgrammar-based GPmethods, which have also emerged, either can
be put in the same category with tree-based GP or linear GP, or often more
appropriately, can be thought of as being in between them. For a computer
scientist, a formal grammar is the natural link between program as a tree and
program as a linear string. This is always the primary role of grammars in
genetic programming: they specify the language of candidate solutions. The
grammar-based GP methods, however, can be employed in several ways:
( 1 ) Constraining tree-based GP: The grammar is used to restrict the search
space, and consequently also to redefine search operators under which the
restricted search space is algebraically closed.
( 2 ) Introduction of bias into tree-based GP: The grammar is used as a vehicle
for bias toward certain solutions. The bias may also be adjusted over the course
of the algorithm’s execution.
( 3 ) Replacement of the traditional tree-based GP mechanisms: The gram-
mar may serve both of the above purposes, but more importantly it is an integral
part of the algorithm that provides mapping between two representations of
candidate solutions.
From the short descriptions we can already glimpse that different grammar-
based GP methods have different aims. In case ( 1 ) the grammar is used to
remedy a shortcoming of GP: the so-calledclosureproblem, but most grammar-
based methods also raise problems and questions of their own concerning the
encoding of individuals, and the design of operators. In spite of the different
motivations behind the methods, there are also significant areas of overlap


between them. The goal of this thesis will therefore be to

- describe the problems arising from integration of grammars and genetic
    programming,
- compare the approaches of several existing methods,
- compare appropriateness and performance of the methods on benchmark
    problems.

The text is organised in the following chapters:

- Chapter 1 introduces the techniques of traditional tree-based GP, the
    necessary concepts from formal language theory, and several grammar-
    based GP methods. Common features and issues are pointed out.
- Chapter 2 presents applications that we will use for comparison and
    benchmarking.
- Chapter 3 describes several experiments with grammar-based methods
    in the presented applications, and analyses the results.
- Chapter 4 provides information about the implementation used for the
    experiments, which is available on the accompanying medium and on-
    line^1.
- The closing chapter concludes the thesis and suggests possibilities for
    further research.

(^1) [http://nohejl.name/age/](http://nohejl.name/age/)


## Chapter 1

## Grammar-Based Genetic

## Programming Methods

In this chapter we will introduce several methods for genetic programming
based on formal grammars, assuming basic knowledge about evolutionary
algorithms, particularly genetic programming andgenetic algorithms(GA), and
formal grammars. If you are not familiar with evolutionary algorithms, the
textbooks by Goldberg ( 1989 ) (on genetic algorithms) or Poli et al. ( 2008 ) (on
genetic programming in a broad sense) provide a good overview. Alternatively,
you can find a short summary of the commonest techniques in my bachelor
thesis (Nohejl, 2009 ).
We will begin with an informal review of the basic concepts and methods
that preceded grammar-based genetic programming:

- in Section 1. 1 , we will describe the grammars and notations for them
    commonly used in grammar-based methods,
- in Section 1. 2 , we will review the plain tree-based genetic programming
    and one of its developments highlighting the points that will later interest
    us,
- in Section 1. 3 , we will outline how grammars were used to encode bias
    in inductive logic programming.

Then, we will describe the following grammar-based GP methods:

- in Section 1. 4 , context-free grammar genetic programming,
- in Section 1. 5 , LOGENPRO, a genetic programming system based on logic
    grammars,
- in Section 1. 6 , grammatical evolution.

To complete our tour we will discuss the common features and shortcomings of
the methods in Section 1. 7 and examine the implications for their performance
in applications in Section 1. 8.


### 1 1 Formal grammars and programming

Context-free grammars(CFGs) are used to express syntax of most currently used
programming languages because they provide a reasonable trade-offbetween
expressiveness (relative size of the class of expressible languages) and efficiency,
which is especially important for syntactic analysis (parsing) of programs.
This also makes them a natural choice for augmenting genetic programming
with grammars. Most of the methods that we are going to discuss are based on
CFGs, while the remaining use grammars that extend CFG in some way. We will
therefore begin with a precise definition and the corresponding terminology
and notation:

**Definition.** A context-free grammar, orCFG, is formed by four components
(adapted from Hopcroft et al., 2000 , ch. 5 ):

```
1. There is a finite set of symbols that form the strings of the language being
defined. We call this alphabet theterminals, orterminal symbols.
```
2. There is a finite set ofnonterminals, ornonterminal symbols.^1 Each nonter-
minal represents a language; i.e., a set of strings.
3. One of the nonterminals represents the language being defined; it is called
thestart symbol. Other nonterminals represent auxiliary classes of strings
that are used to help define the language of the start symbol.
4. There is a finite set ofproductionsorrulesthat represents the recursive
definition of a language. Each production consists of:
(a) A nonterminal that is being (partially) defined by the production.
This nonterminal is often called theheadof the production.
(b) The production symbol→.
(c) A string of zero or more terminals and nonterminals. This string,
called thebodyof the production, represents one way to form strings
in the language of the nonterminal of the head. In so doing, we leave
terminals unchanged and substitute for each nonterminal of the body
any string that is known to be in the language of that nonterminal.
When applying the rule, we say that it is used torewriteits head to its
body, or that using the rule a string containing the bodyderivesfrom the string
containing the head. The terminology that we have introduced can be used
for other kinds of grammars as well. It is the restriction that production heads
consist of a single nonterminal that gives context-free grammars their name by
ruling out context dependence.
Formally, a CFG is usually represented as the ordered quadrupleG =
(N,T,P,S), whereNis the set of nonterminals,T the set of terminals,Pthe
set of productions, andSthe start symbol.

(^1) Hopcroft et al. ( 2000 ) prefer the namevariables, which is reserved for its more common
use in this text.


**Notation.** We will use a number of conventions when working with CFGs and
grammars in general:

```
1. Letters, digits and symbols in upright or non-proportional type represent
terminals. Examples: x, 4,×,%.
```
```
2. Identifiers consisting of lower-case letters in italic type represent nonter-
minals. Examples:s,var,expr.
```
```
3. Uppercase letters in italic type are used as meta-variables (symbols that
stand for an unspecified nonterminal, or less often terminal). Examples:
A,B,X.
```
```
4. Lower-case Greek letters stand for strings consisting of terminals and
nonterminals, λdenotes an empty string, and period is used to make
concatenation explicit. Examples:α,β,ξ=ξ.λ.
```
Note particularly the representation of nonterminals, which is contrary to the
usual convention of the formal language theory, but allows us to use more
expressive identifiers.
We will also use theBackus-Naur form(BNF) as a notation for context-free
grammars. Examples of a CFG describing simple arithmetic expressions and
a corresponding BNF notation are provided in Listing 1. 1 and Listing 1. 2. See
Naur ( 1963 ) for a formal definition.

**Definition.** LetG=(N,T,P,S) be a context-free grammar. Thederivation trees,
orparse trees, forGare rooted, ordered trees that satisfy the following conditions
(adapted from Hopcroft et al., 2000 , ch. 5 ):

```
1. Each internal node is labelled by a nonterminal inN.
```
```
2. Each leaf is labelled by either a nonterminal, a terminal, orλ. However,
if a leaf is labelledλ, then it must be the only child of its parent.
```
```
3. If an internal node is labelledA, and its children are labelledX 1 ,X 2 ,...,Xk
respectively, from the left, thenA→X 1 X 2 ···Xkis a production in P.
```
Theyieldof a derivation tree is the concatenation of its leaves in the order
they appear in the tree. Thedepthof a node in a tree is the length of the path
from root to that node counted as the number of edges. Theheightof a tree
is the largest depth of a node in the tree. Thus root node has depth 0, and
the minimum height of a tree whose yield consists of terminals andλis 1.
These definitions are in line with the standard textbook terminology (Hopcroft
et al., 2000 , ch. 5 ; Cormen et al., 2001 , sec. B. 5. 2 ), and can be easily extended
to derivation trees for other types of grammars, and in case of depth and
height to any rooted trees. (See Figure 1. 1 for an example.) There is no formal
difference between a derivation tree and a parse tree: the former emphasises
the generative aspect, the latter emphasises the aspect of syntactic analysis.


```
expr→(expr op expr) expr→prim
op →+ op →×
prim→x prim→ 1. 0
```
**Listing** 1**.** 1 **:** Production rules for a CFG whose set of nonterminals is{expr,op,prim}, its
set of terminals is{(,),+,×,x, 1. 0 }, and its start nonterminal isexpr.

```
<expr> ::= ( <expr> <op> <expr> ) | <prim>
<op> ::= + | *
<prim> ::= x | 1.
```
**Listing** 1**.** 2 **:** A BNF version of the previous example. Note that|denotes alternatives,
and that the sets of terminals and nonterminals are implied, as is the start
nonterminal following the convention of putting its productions first.

```
Let’s state informally several basic facts about context-free grammars:
```
**Fact** 1**.** The same language can be described by multiple context-free grammars.Con-
sider the ruless→p,s→q,s→1,p→s+s,q→s×s, the ruless→s+s,
s→s×s,s→1, and the ruless→s+s,s→s×s,s→s+s×s,s→1.

**Fact** 2**.** A CFG may be ambiguous. Consider the ruless→s+s,s→s×s, and
s→1, and a string 1+ 1 ×1. It is impossible to tell if it was derived by first
using the first rule or the second rule. Thus, for an ambiguous CFG, different
derivation trees can yield the same string.

**Fact** 3**.** A CFG cannot express all what is usually considered part of a programming
language syntax. Notably, the use of declared variables is context-dependent.
Consider a language with a “let var = expr in expr” construct: by syntactic
analysis (or generation) according to any given context-free grammar, it is
impossible to ensure that eachvarnonterminal occurring in the derivation of
the secondexprnonterminal is rewritten to an identifier declared in an enclosing
letconstruct.

**Fact** 4**.** For a given CFG the set of strings yielded by parse trees such that ( 1 ) they are
rooted in the start symbol, and ( 2 ) they yield a terminal string, is the language defined
by the CFG.A corresponding derivation of a terminal string from the language
can be constructed from any given derivation tree and vice versa.

Definite clause grammars(DCGs) are a formalism tied to the Prolog pro-
gramming language and related to its early application to natural language
processing. Rather than being another type of a formal grammar, they are a spe-
cific notation for grammars with semantics derived from logic programming.
In the context of logic programming languages such as Prolog or Mercury, they
are often used to create complex parsers.
DCGs can easily capture long distance dependencies, and can be used as a
natural notation for context-free grammars, as well as for the more expressive
attribute grammars (as shown by Sterling and Shapiro, 1994 , ch. 19 ), which are


```
expr --> ['('],expr,op,expr,[')']. expr --> prim.
op --> ['+']. op --> ['*'].
prim --> ['x']. expr --> ['1.0'].
```
**Listing** 1**.** 3 **:** A DCG version of the previous example. Note that commas denote con-
catenation and square brackets enclose terminals, which can be strings, as
in the example, or any terms.

```
s --> rep(N,a), rep(N,b), rep(N,c).
rep(end,_) --> [].
rep(s(N),X) --> [X], rep(X,N).
```
**Listing** 1**.** 4 **:** A DCG grammar foranbncn, a language that cannot be described by a
context-free grammar. Note the Prolog variablesXandN, the number of
repetitions expressed using the term structures(s(···s(end)···)).

```
expr(V) --> ['let'],X,['='],expr(V),['in'],expr([X|V]),{var(X)}.
expr(V) --> X, {member(X,V)}.
```
**Listing** 1**.** 5 **:** A fragment of a DCG grammar for checking variable declaration in a “let
var =expr in expr” construct. Note the Prolog variablesV,Xand the
external predicatesvar/1,member/2in curly braces.

commonly employed for syntactic analysis in compilers rather than plain CFGs.
The looser termlogic grammarsmay refer to DCGs or a derived formalism of
equal or restricted expressive power (see Section 1. 5 , Section 1. 3 ).

In the simplest case a DCG expresses a context-free grammar (Listing 1. 3 ).
As in CFGs, a head of a production consists of a single item, and its body is
a string of items. The items, however, may be arbitrary terms with Prolog
variables (Listing 1. 4 ), and further extending the computational power, a list
of arbitrary Prolog goals may be added to each rule (Listing 1. 5 ). As shown in
Listing 1. 4 and Listing 1. 5 , a DCG can describe context-dependency such as a
restriction to use only declared variables in a programming language syntax.

A precise definition and more complex examples can be found inThe Art of
Prologby Sterling and Shapiro ( 1994 , ch. 19 , 24 ), and Sperberg-McQueen ( 2004 )
provides a good practical overview online. The semantics should intuitively
be clear to readers familiar with logic programming: the notation is translated
into a Prolog program for a top-down left-to-right parser resulting in a clause
for each production with any goals in square brackets being added to the body
of the clause. When the parser is executed, the terms and variables used in the
grammar are subject to unification. Finally, let’s state an obvious fact:

**Fact** 5**.** The DCG formalism has enough power to express unrestricted grammars.
Prolog is Turing-complete (Sterling and Shapiro, 1994 , sec. 17. 2 ), and any Prolog
goals can be added to the grammar rules.


### 1 2 Genetic programming

In genetic programming, as pioneered by John Koza and described in his book
Genetic Programming( 1992 ), candidate solutions are programs represented by
trees. In Koza’s programming language of choice, such representation does not
involve any extra costs: when evolving Lisp programs in a Lisp-based system,
the program, the corresponding tree, and the data that represents it coincide
(see Figure 1. 1 for comparison between a GP tree and a derivation tree). In GP
terminology, inner nodes of a tree are drawn from a set offunctions, while its
leaves are drawn from a set ofterminals^2. Except for quantitative parameters
and a fitness function, the two sets are the only input of a genetic programming
algorithm, and thus they determine its search space.

##### *

```
m expt
```
```
c 2
```
```
E
```
```
(* E E )
```
```
m
```
```
c
```
```
(expt E E )
```
```
2
```
```
GP functions: *, expt (binary)
GP terminals: 2 , c, m
```
```
CFG productions:
E → (* E E ) E → (expt E E )
E → 2 E → c E → m
```
```
GP tree: Derivation tree:
```
```
depth 0
```
```
depth 1
```
```
depth 2
```
```
depth 0
```
```
depth 1
```
```
depth 2
```
```
depth 3
```
```
height: 2
```
```
height: 3
```
**Figure** 1**.** 1 **:** An individual tree in GP and the corresponding derivation tree (parse tree)
for a CFG, both representing the Lisp expression(* m (expt c 2)). In
the context of tree-based GP, the formal distinction is often neglected and
individual trees are called parse trees (Koza, 1992 ). The CFG with a single
nonterminal expresses the closure property of GP functions and terminals.

For the search to be effective, the sets are required to possess theclosure
property, quoting Koza ( 1992 , sec. 6. 1. 1 ): “each of the functions in the function
set [must] be able to accept, as its arguments, any value and data type that may
possibly be returned by any function in the function set and any value and
data type that may possibly be assumed by any terminal in the terminal set.”
Without this property, invalid programs would often arise from initialisation
(by randomly generating trees), and then repeatedly from crossover (by swap-
ping arbitrary subtrees) and mutation (by replacing an arbitrary subtree with
a randomly generated tree).

(^2) Note the difference in terminology between the terminology of GP and that of formal
grammars. When necessary to prevent ambiguity, I will refer to GP terminals and GP functions.


The other required property stated by Koza issufficiency: if the functions and
terminals are not sufficient to represent a solution, no solution can be found.
This is a general problem of all machine learning algorithms: analogously,
inputs and outputs of a neural network need to be assigned before training the
network. A more subtle point, however, is worth noting: not all sufficient sets
of terminals and nonterminals result in equally efficient searches.
Satisfying the closure property was not intended only as a workaround
for the issue of syntactic invalidity but also for runtime errors (for instance
division by zero). It proved to be an effective solution in some cases, but it
is not feasible for problems that demand extensive use of several mutually
incompatible types of values (such as scalars, vectors and matrices of different
dimensions). The obvious way to solve this is to restrict the genetic operators
ad hoc, as shown by Koza ( 1992 , ch. 19 ).
Koza ( 1992 ) stressed that genetic programming is, like GA (Goldberg, 1989 ),
a weak method: the search algorithm is problem-independent. He also emphas-
ised the positive consequences, universality and ability to “rapidly [search] an
unknown search space”, over the nontrivial issues of adapting such a method
to a specific problem, which we anticipated in the discussion of closure and
sufficiency. These issues motivated the development of the more advanced
genetic programming techniques that we are going to discuss.
One such early technique, a more general way of restricting the search
space than Koza’s ad hoc modification of operators, was devised by David
Montana ( 1994 ). In hisstrongly-typed genetic programming(STGP), all functions
and terminals have types, which are used to automatically constrain how trees
can be composed and modified. As noted by Montana, although this is a
cleaner method, it is in the end equivalent with Koza’s solution.
What Montana thought of as a “big new contribution” was the introduction
of generics, which allows you to specify for instance a general function for
matrix multiplication, which takes anm×nmatrix and ann×pmatrix and
returns anm×pmatrix, wherem,n, andpare arbitrary integers. The useful-
ness of generics beyond specific problems with multi-dimensional structures
hinges on the assumption that “the key to creating more complex programs is
the ability to create intermediate building blocks capable of reuse in multiple
contexts,” and that generics induce an appropriate level of generality. Whether
STGP with generics really results in more reusable building blocks within a
single complex problem remains to be confirmed^3 but the idea of restricting
the search space of the GP algorithm declaratively by formal rules has since
become widespread.
Another important technique that extends in tree-based GP areautomat-

(^3) Arguably, one of the more complex problems to which genetic programming has been
applied was evolving a strategy for a virtual soccer team in the RoboCup competition. The
algorithm ran for months on a 40 -node DEC Alpha cluster to evolve a “team” that won its
first two games against hand-coded opponents and received the RoboCup Scientific Challenge
Award (Luke et al., 1997 ; Luke, 1998 ). While the authors did use STGP, they employed its basic
form without generics.


```
root
```
```
ADF1 ADF n programmain
```
# ...

```
function-defining branches value-returning branch
```
```
ARG1, ..., ARG mi for ADF i
```
```
problem-dependent, and
ADF1, ..., ADF n
GP functions: problem-dependent
```
```
GP terminals: problem-dependent
```
**Figure** 1**.** 2 **:** ADFs in tree-based GP: the partially constrained structure of individuals’
trees fornADFs, where theith ADF hasmiparameters.

ically defined functions(ADFs). ADFs, as originally adopted by Koza ( 1992 ,
ch. 20 ), provide a simple mechanism for co-evolution of subroutines. Since
then a number of more sophisticated variants have been proposed, but we will
describe the original version. The idea is inspired by the practise of human
programmers: in the same way as man-made programs are usually composed
of the main function and additional reusable functions, the individual trees in
GP may be composed of avalue-returning branchand severalfunction-defining
branches. The number and order of these branches is fixed, as is the number of
formal parameters of the ADF defined by each function-defining branch. Based
on these numerical parameters, new GP functionsADF1,... ,ADFn, which refer
to the ordered function-defining branches, and GP terminalsARG1,... ,ARGm,
which refer to their formal parameters, are added to the problem-dependent
sets of GP functions and GP terminals. The new terminals may be used only
in function-defining branches, and the new functions may be used only in the
value-returning branch.

While the inspiration by procedural programming is obvious, ADFs are
not meant to make the resulting programs more human-readable. Instead
they are a way to adapt genetic programming to problems in which symmetry
or repetition are expected, thus a way to embed a special kind of problem-
dependent knowledge in the general GP algorithm. In order to use ADFs, the
standard initialisation procedure and genetic operators need to be modified to
preserve these constraints common to all individuals (Figure 1. 2 ).

Thus ADFs place further constraints on the search space, while the tradi-
tional genetic programming does not provide a general mechanism for satisfy-
ing such additional constraints.


### 1 3 Grammatically biased ILP

In parallel with the beginnings Koza’s genetic programming and its first vari-
ants such as STGP, grammars already started to be used in inductive logic
programming(ILP), another branch of machine learning research^4 that emerged
at the time. As we will later see, this is where the grammar-based GP methods
drew inspiration.
ILP constructs a hypothesis in the form of a logic program, more precisely
a Prolog program, from a set of positive examples, from a set of negative ex-
amples, and frombackground knowledge, also a Prolog program. The article
by Muggleton ( 1994 ) provides a concise description of the basic techniques of
ILP and the theory behind it. It also acknowledges the importance of problem-
dependent knowledge to restrict the search space: “in order to ensure efficiency,
it is usually found necessary to employ extra-logical constraints within ILP
systems”. Two categories of such constraints are discussed: “statistical con-
firmation” via aconfirmation function, which “fits a graded preference surface
to the hypothesis space”, andlanguage bias, which “reduce[s] the size of the
hypothesis space”.
From the point of view of evolutionary algorithms, the confirmation func-
tion is simply a type of fitness function. We will focus our attention the other
kind of constraint, language bias: particularly interesting is a “generalised”
approach that “provides a general purpose ‘declarative bias’” (as described by
Muggleton, 1994 ). (In this context, “bias” is used to encompass both restric-
tion and preference.) This approach devised by Cohen ( 1992 , 1994 ) consists of
translating the background knowledge into a grammar in a way that guides
the formation of a hypothesis. Cohen noted that various existing ILP methods,
each using a different algorithm, were well-suited for different problems, and
felt that the search bias embedded in the algorithms should instead be “com-
piled into” the background knowledge. To achieve this he used “antecedent
description grammars”, which are a special case of definite clause grammars
that retains the use of arbitrary terms and unification among these terms (as
shown in the Listing 1. 4 ) but does not allow adding external goals to the pro-
ductions.^5 Such grammars describe antecedents (bodies) of Prolog clauses of a
hypothesis.
To search with a weak bias, the grammar could allow various combinations
of problem-specific predicates to occur in a clause body, while a strong bias
could prescribe a mostly fixed body and restrict the variation only to its part.
Unification is used both to ensure that variables from the head of a clause

(^4) Koza ( 1992 ) originally considered GP a machine learning parading, similarly, Muggleton
( 1994 ), who conceived the original ILP, considered it a machine learning framework. Genetic
programming can, however, be applied to search and optimisation problems likely to be
considered out of the traditional scope of machine learning.
(^5) A notationA→αwhereP, wherePis a Prolog goal, superficially similar to the curly-
bracketed goals in DCGs, is introduced in the 1994 article. It is, however, clarified that the goal
Pis evaluated with regards toAandα“by a macro-expansion process when the grammar is
read in”.


are used in its body and to impose further constraints: for instance that two
predicates should share a variable, or that variables in some predicate should
be of the same type. In addition to the “hard constraint” defined by a grammar,
Cohen ( 1992 ) uses what he calls “preference bias”: some production rules are
marked “preferred” and some “deferred”. Only if the system does not succeed
using the preferred productions, it resorts to the deferred ones.
In his articles, Cohen ( 1992 , 1994 ) shows how to emulate different strategies,
including that of a well-known ILP system FOIL, and how to improve on FOIL’s
performance by adding various kinds of background knowledge using only
antecedent description grammars: “The contribution of this paper is to describe
asingletechnique which can make use ofallof these types of background
knowledge—as well as other types of information about the target concept—in
a uniform way.” As we will show in the next section, the concept of declarative
bias and some of these mechanisms can be transposed to genetic programming.
In Section 1. 5 we will describe a more recent system integrating ILP and GP,
which shares even more details with Cohen’s methods.

### 1 4 CFG-GP, language bias, and search bias

The first notable use of formal grammars to control the search algorithm of
genetic programming probably came from Peter Whigham as both another, in
a way more general, solution to the typing problem recognised by Montana,
and a means of introducing more bias into genetic programming (Whigham,
1995 , 1996 ). In the 1995 article Whigham noted that a context-free grammar can
be used in similar ways as types to restrict the structure of candidate solutions,
in his terminology, to introduce language bias. The proposed method, called
context-free grammar genetic programming(CFG-GP), is based on a straightfor-
ward redefinition of the elements of tree-based GP to respect a given context-
free grammar. The individuals still have the form of trees, but instead of
representing Lisp expressions, the trees are derived according to an arbitrary
CFG, and genetic operators are altered to preserve this representation.
We have already mentioned a coarser form of language bias, which is created
by the sets of terminals and functions in original GP, but a CFG allows to embed
more problem-dependent knowledge and also to easily use the programming
language most appropriate for a given problem.
Whigham ( 1995 ) also proposes the following mechanism for learning bias:

- Let each production rule in the grammar have an integer weight (called
    “fitness” by Whigham, although it’s a different concept from individual
    fitness) and let the weights initially be equal to 1.
- In each generation: Find the fittest individual (choosing one of the least
    high among equally fit); choose one of the deepest nonterminalsBin its
    derivation tree; letαbe the string of terminals into which it is rewritten;
    ifBis a singleton, then letAbe the parent of the highest node in the chain


```
of singletons that ends inB, else letA=B. Create a new production rule
A→ α; ifA→ αis already in the grammar, increase its weight by 1,
otherwise addA→αwith weight 1 to the grammar.
```
- When applying mutation or “replacement” (creating new individuals):
    select production rules for a given nonterminal with probability propor-
    tional to their weights.

The additional operator called replacement simply replaces a fixed part of the
population with individuals created in the same way as when creating the
initial population. The learnt bias thus affects the mutation and “replacement”
operators in each generation, and can also be used in a subsequent run of the
algorithm.
Later, Whigham ( 1996 ) emphasised the distinction between three kinds of
bias: selection bias, language bias, and search bias. In his terminology selection
bias is the compound effect of a selection scheme and the fitness function,
language bias consists of the restriction imposed by language (grammar), and
search bias consists of the factors that control search (crossover and mutation).
Seen from this perspective, the bias learning in Whigham’s original article
compiled search bias into language bias. In contrast to this, the 1996 article
presents a mechanism to control search bias separately:

- “Selective” versions of the mutation and crossover operators are intro-
    duced. A selective operator may be applied only to a subtree rooted in
    a nonterminal from a particular set. Several instances of such a selective
    operator may be used, each with a different probability. These probab-
    ilities are considered the principal means of search bias. Note that the
    probabilities do not govern frequencies of particular nonterminals in the
    population, but frequencies of operator application to them.
- Production rules in the grammar may still be assigned weights (in this
    article called “merit weighting”), but the weights are only constant, and
    no new production rules are added to the grammar. The weights apply
    to initialisation and mutation as already described, and are presumably
    part of search bias.

Whigham’s approach has shown that grammars, in this case CFGs, can be
used in GP in a straightforward manner to constrain the search space. As noted
by Whigham ( 1995 ) without going into detailed comparison, it is a “different
approach to the closure problem [than STGP].” A context-free grammar can be
used both to express constructs of a wide range of languages and to emulate a
type system with a small finite set of types by having one nonterminal for each.
STGP with its generics (Montana, 1994 ) is more powerful in this regard, but
such power comes with a performance trade-off(Poli et al., 2008 , sec. 6. 2. 4 ).
Additionally, Whigham has used grammars a vehicle for a finer control of
bias. In the two articles, Whigham ( 1995 , 1996 ) proposed two different ways of
working with bias: in the former using means analogous to those that Cohen


used in ILP (see Section 1. 3 : knowledge is being compiled into grammar, some
rules may be preferred to others), but added a simple learning mechanism;
in the latter he abandoned learning and tried to keep search bias separated
from language bias, while still taking advantage of using a grammar. Neither
of these approaches would be possible if the language bias wasn’t specified
declaratively.

### 1 5 Logic grammar based genetic programming: LOGENPRO

Wong and Leung ( 1995 ) presented a genetic programming system based on
logic grammars, called LOGENPRO (LOgic grammar based GENetic PRO-
gramming system), and later ( 2000 ) the same authors published a book on this
system. LOGENPRO is presented as “a framework [... ] that can combine GP
and ILP to induce knowledge from databases” (Wong and Leung, 2000 ). The
system is based on the same core algorithm as tree-based GP or CFG-GP: iter-
ated application of fitness-based selection and genetic operators, but instead of
context-free grammars, LOGENPRO employs logic grammars “described in a
notation similar to that of definite clause grammars”.
The only difference other than in notation between DCGs and these logic
grammars seems to be that the “logical goals” that can be added to rules in
LOGENPRO are not strictly limited to logic goals as used in Prolog: they are
in fact procedures defined in Lisp, the language in which the framework itself
is implemented. (See Listing 3. 3 on page 47 for an example of a LOGENPRO
grammar.) The framework emulates the mechanisms of logic programming to
interpret the grammar, but it does not feature a complete or cleanly separated
logic programming environment.^6
LOGENPRO does not use any mechanisms or algorithms specific to ILP
but Wong and Leung ( 2000 ) demonstrate that with a suitable grammar, it
can be used to learn logic programs and it achieves results competitive with
earlier ILP systems not based on GP and grammars. What differentiates it
from CFG-GP is the more powerful formalism for grammars, which is closer
to the one used by Cohen in ILP. As we have noted in Section 1. 1 , definite
clause grammars can be used to describe the context-dependent constructs
often found in programming languages. The representation of individuals in
LOGENPRO is still conceptually the same as in CFG-GP (a derivation tree,
although with structured nodes, as they can contain terms and goals), but
operators need to be much more complex to respect the grammar.
While DCGs are essentially Turing-complete (see Fact 5 , page 7 ), LO-
GENPRO does not evaluate logical goals except when generating new trees:

(^6) I obtained LOGENPRO source code from the authors via personal communication. Al-
though Wong and Leung ( 1995 , 2000 ) present the “logic grammars” used in LOGENPRO as
different from DCGs, no difference is evident from their description, and no details about the
implementation are given.


the subtrees that contain logic goals cannot be changed by the operators (Wong
and Leung, 2000 , sec. 5. 3 ), so they behave as atomic and immutable compon-
ents throughout the evolution (compare with the approach used by Cohen,
1994 , see footnote 5 , page 11 ).
On one hand, even the remaining power of DCGs, which lies in the use
of variables and unification, still make operators, particularly crossover, quite
complex: according to Wong and Leung ( 2000 , sec. 5. 3 ), the worst-case time
complexity of crossover isO(m·n·logm), wheremandnare the sizes of the
two parental trees, slightly higher thanO(mn) in Koza’s GP with ADFs and
Montana’s STGP. On the other hand, the same power allows it to emulate
the effect of both of these methods (Wong and Leung, 2000 ). The scheme of
individuals that use ADFs (as shown in Figure 1. 2 ) can be easily embedded
in a grammar (see Section 3. 7 , also demonstrated by Wong and Leung, 2000 ).
While the authors do not explain how exactly LOGENPRO can emulate STGP
including generics, a finite set of types can be emulated even using a CFG (as
we have remarked in Section 1. 4 ).
LOGENPRO focuses on sophisticated constraints on the search space that
can be described declaratively using a DCG, and performs a search using
elaborate operators that preserve these constraints. It does not provide any
special mechanisms for learning bias, or any additional parameters for the
search.

### 1 6 Grammatical evolution

Grammatical evolution(GE) (Ryan et al., 1998 ; O’Neill and Ryan, 2003 ) is a recent
method for grammar-based GP that, unlike LOGENPRO or CFG-GP, signi-
ficantly changes the paradigm of traditional tree-based GP by introduces the
notion ofgenotype-phenotype mapping. In a parallel to the biological process of
gene expression, each individual has a variable-length lineargenotypeconsist-
ing of integer codons, to which the genetic operators such as mutation and
crossover are applied. In order to evaluate the individual’s fitness, the geno-
type is mapped tophenotype, a program in the language specified by the given
context-free grammar. Trees are not used for individual representation but
implicitly as a temporary structure used in the course of mapping.
The mapping used in GE is so simple that we can describe it in full details
(adapted from Nohejl, 2009 ):

```
In analogy to the DNA helix and nucleobase triplets, the string
is often calledchromosome, and the values it consists of are called
codons. Codons consist of a fixed number of bits. The mapping to
phenotype, proceeds by deriving a string as outlined below in the
pseudocode for Derivation-Using-Codons. The procedure accepts
the following parameters:
```

- G, a context-free grammar in BNF. Note that BNF ensures
that there is a non-empty ordered list of rewriting rules for each
nonterminal.
- S, a nonterminal to start deriving from. The start nonterminal
ofGshould be passed in the initial call.
- C, a string of codons to be mapped. Note that it is passed
by reference and the recursive calls will sequentially consume its
codons using the procedure Pop.

```
Derivation-Using-Codons(G,S,C)
1 P←array of rules for the nonterminalSinGindexed from 0
2 n←length[P]
ch A←symbols ofσsequentially
11 do if Ais terminal
12 then τ←τ.A
13 else τ←τ.Derivation-Using-Codons(G,A,C)
14 return τ
```
Out of the context of fitness evaluation, the individuals are simple binary
strings to which standard GA (Goldberg, 1989 ) operators for one-point cros-
sover and mutation are applied (the only differences are that the chromosomes
are variable-length and the crossover is applied on codon boundary, not at ar-
bitrary position), the population can also be initialised by generating random
binary strings. Thus the operators for GE can be implemented very efficiently,
and the performance penalty of performing the mapping is also low.
It would appear that this efficiency comes at the cost of a high proportion
of invalid individuals (see line 7 of the pseudocode). This issue is addressed
bywrappingthe chromosome (interpreting it in a circular fashion) if needed,
which can greatly reduce the number of invalid individuals (O’Neill and Ryan,
2003 , sec. 6. 2 ). Still, the operators clearly may have a different effect than
the traditional GP operators, which tend to preserve most of the individuals’
structure and are designed to have a predictable effect. In GE, on the one hand,
minor changes in genotype can translate into massive changes in phenotype;
on the other hand, some parts of genotype (and thus any changes to them)
may not have any effect on the phenotype. O’Neill and Ryan ( 2003 ) justify
these issues as parallels to genetic phenomena (such asgenetic code degeneracy
in the case of unused genetic information), and show that in some cases they
can improve performance.


While a formal analysis of the general effect of these simple operators on
phenotype, and thus the overall search performance, is lacking, the effect can
be measured and compared statistically in specific applications. This method
is used by O’Neill and Ryan ( 2003 ) to compare different variants of operators,
and we will use it to compare performance with CFG-GP in Chapter 3.
Compared with CFG-GP, and especially with LOGENPRO, grammatical
evolution has an extremely simple implementation that consists of highly effi-
cient elements. We could call these elements (operators, simple random initial-
isation) grammar-agnostic: they do not depend on the grammar at all, and the
search bias that they create cannot be adjusted with regards to the grammar.
The constraints of the grammar are ensured by the genotype-phenotype map-
ping at the expense of preservation of phenotypic structure that the traditional
GP, as well as CFG-GP or LOGENPRO, strive for.

### 1 7 Common features and shortcomings

Figure 1. 3 provides an overview of the grammar-based GP methods that we
have presented along with traditional GP and grammatically biased ILP. The
most important trait of all grammar-based methods is that they provide a gen-
eral framework for declaratively describing the search space that the traditional
GP or ILP lacks. One aspect that differentiates them is the power of this de-
clarative description. The relatively weak context-free grammars can describe
the basic structure of common programming languages or their subsets (except
context-dependent constructs), and emulate simple type systems, and ADFs.
The logic grammars (antecedent description grammars used in Cohen’s ILP or
DCG-like logic grammars in LOGENPRO) can capture context-dependency us-
ing logic variables and unification, but their ability to use arbitrary logic goals
is of limited use in genetic programming (as exemplified by genetic operators
in LOGENPRO, Section 1. 5 ).
The expressive power of grammars entails a performance trade-off for
structure-preserving operators. Grammatical evolution seems to avoid this
issue by using grammar-agnostic, possibly destructive operators similar to
those traditionally used in GA.
Apart from the hard constraints of the search space, the grammar may
serve as a vehicle for further bias (preference). This direction was explored by
Whigham ( 1995 , 1996 ) through selective operators and learning of production
weights. It may be less obvious that the grammar itself creates a bias by
interaction of its form (equal grammars may differ in form, see Fact 1 , page 6 )
and the operators, or the genotype-phenotype mapping in case of GE. The
grammar-based methods seem to be designed as if using a particular form of
the grammar was an obvious way to embed problem-dependent knowledge,
but with their growing complexity (logic grammars in LOGENPRO, interaction
of operators and the mapping in GE) this is not the case. (The grammar for the
artificial ant problem in Section 3. 5 , and the grammars for timetabling heuristics


in Section 3. 9 will later be discussed in this context.) The otherwise relatively
simple CFG-GP provides a mechanism (Whigham, 1995 ) to adjust the form of
the grammar over the course of running the algorithm, but the more recent
methods (GE and LOGENPRO) do not address this issue.

### 1 8 Implications for performance

LOGENPRO seems to be very different from the two other methods by using a
more powerful formalism for grammars. This could be a double-edged sword
if it was actually used in some application. We will attempt to replicate two
experiments done by Wong and Leung ( 2000 ) with LOGENPRO (in Section 3. 6
and Section 3. 7 ). We will show that the experiments do not actually require a
logic grammar, and that the results are comparable with those of CFG-GP.
Grammatical evolution ensures constraints given by the grammar by its
genotype-phenotype mapping but its operators can be applied to any part of
its genotype regardless of the grammar and the phenotype. This makes any
grammatical constraints work in an essentially different way in GE than in
CFG-GP or LOGENPRO. We can expect this to cause GE to produce different
shapes of trees than the other two methods, which work with derivation trees
(considered to represent the phenotype for GE). This in turn will result in a
different search space. We will attempt to analyse this effect and link it to dif-
ferences in performance using experiments with several different applications
in Chapter 3.
Techniques for reuse of building blocks are crucial for performance in ap-
plications that use such blocks. As we will show, it is easy to carry over the
techniques used by Koza ( 1992 ) for ADFs to any of the grammar-based meth-
ods. In fact, it is far easier to specify the ADFs using a grammar than to add
the necessary ad hoc constraints to tree-based GP operators. The same gram-
mar will, however, have a different effect in GE than in the other two methods
because operators are applied regardless of the grammar. We will see how this
impacts performance in experiments in Section 3. 7 and Section 3. 8.


**Tree-based GP**

**Cohen's ILP**

**CFG-GP**

**LOGENPRO**

**GE**

```
Representation
```
```
tree (LISP S-expressions)
```
```
logic program
```
```
tree (derivation tree)
```
```
tree (derivation tree)
```
```
variable-length integer string (mapped to derivation tree)
```
```
Language specifi
```
```
cation
```
```
set of terminals, set of functions with closure property
```
```
antecedent description grammar (DCG without arbitrary goals)
```
```
context-free grammar
```
```
logic grammars with arbitrary goals (analogous to DCG)
```
```
context-free grammar
```
```
Search guided by
```
```
fitness function
```
```
positive and negative examples (confi
```
```
rmation function)
```
```
fitness function
```
```
fitness function
```
```
fitness function
```
```
Search type
```
```
evolutionary algorithm with GP operators
```
```
FOIL-based ILP algorithm
```
```
evolutionary algorithm with GP-like operators
```
```
evolutionary algorithm with GP-like operators
```
```
evolutionary algorithm with GE-like operators and genotype-phenotype mapping
```
```
Additional search bias
```
```
operator probabilities, other EA parameters
```
-

```
operator probabilities, other EA parameters
```
```
operator probabilities, other EA parameters
```
```
operator probabilities, other EA parameters, codon size, initial/maximum chromosome lengths, wrapping
```
```
Additional search bias using grammar
```
```
N/A
```
```
preferred/deferred productions
```
```
nonterminal sets for selective operators; weighted productions
```
-
-

```
Additional features
```
-

```
macros applied when reading the grammar
```
```
possibility of bias learning
```
```
application to machine learning, data mining,
```
```
genetic code degeneracy
```
```
Figure
```
```
1.
```
```
3 :
```
```
Comparison of grammar-based GP methods along with traditional GP and grammatically biased ILP.
```


## Chapter 2

## Existing Applications

Genetic programming is a very general method that has been applied to a vari-
ety of problem domains from circuit design to the arts (Poli et al., 2008 , ch. 12 ).
While methods that are more efficient and have gained more widespread indus-
trial use exist in most of these fields, the strength of GP lies in its applicability to
problems in which the precise form of the solution is unknown and analytical
methods cannot be employed or would be too resource-intensive.
As we have explained in the previous chapter, the addition of grammars
to genetic programming serves primarily as a means of adapting the general
method to a particular problem or an implementation language: a grammar can
be used both to embed problem-dependent knowledge such as variable types
and structural restrictions, and to specify a subset of a programming language
(or some other formal language) that we want to use for implementation.
Our goal is to compare the results both among different methods and with
previously published results. Thus we will focus on relatively simple applica-
tions suitable for comparing the three main methods that we have presented:
CFG-GP, GE, and in two instances also LOGENPRO. All applications presen-
ted in this chapter have been described in existing literature in conjunction
with one of the methods or with traditional GP, usually in order to highlight
advantages of these methods. One application, the use of grammar-based GP
methods for hyper-heuristic (Section 2. 5 ), stands out a little: as we will see,
this area is relatively open-ended and demanding on implementation but also
capable of producing heuristics competitive with those designed by humans.
Before we proceed to evaluating the methods in the next chapter, we will
describe the chosen applications.

### 2 1 Simple symbolic regression

Regressionis concerned with finding a function that best fits known data. The
problem has most commonly been reducedparametric regression: finding para-
meters for a function whose form is specified in advance (e.g., annth-degree
polynomial). Assuming that the chosen form is adequate, such methods may
be very efficient. In situations in which the adequate form is not known, ge-


netic programming can be used to solve the more general problem assymbolic
regression: search for a function represented in symbolic form, as an expression
using some predefined operations.
A simple problem of real-valued symbolic regression was used as one of
the introductory examples by Koza ( 1992 ) and continues to be used both as
a basic benchmark for newer GP techniques and in more advanced research
applications (Poli et al., 2008 , sec. 12. 2 ). When used as a benchmark, as opposed
to a real-world application, the target values are precomputed using the known
target function, which may be a polynomial. The point is that GP is able to find
the expression representing the polynomial in the space of expressions that
also involve operations unnecessary for the target function such as division or
logarithm.
Fitness of candidate solutions is usually measured as a sum of absolute or
squared errors at the given points to which various scaling methods can be
applied.
Except for specifying a custom language instead of Lisp, there is little ad-
vantage in using grammar-based methods over using tree-based GP for simple
instances of the problem. But these simple instances can serve as a test bed
for unusual operators, such as those employed in GE, which do not behave
analogously to those used in tree-based GP.
More intricate cases of symbolic regression will be presented in Section 2. 3
and Section 2. 4.

### 2 2 Artificial ant trail

The artificial ant trail is another classic introductory GP problem. The goal is to
navigate an artificial ant so that it finds all food lying on a toroidal floor within
a time limit. The pieces of food are placed on a rectangular grid to form a trail
with increasingly demanding gaps and turns. The ant can use the actions

- Left: turn left by 90 °without moving,
- Right: turn right by 90 °without moving,
- Move: advance one square in the direction the ant is facing, eating any food
    on that square,
combined using conditional branching on Food-Ahead: test the square the ant
is facing for food. All actions take one unit of time, the test is instant.
    The problem was originally designed to test evolution of finite-state ma-
chines using GA and various trails have appeared in subsequent versions of
the experiment. The most common one, also used by Koza ( 1992 ) to demon-
strate the competence of GP in solving this problem, is the so-called Santa Fe
ant trail. When using GP, the solution has a form of a short program, which
is executed in a loop until time runs out. The syntactic restrictions placed on
the program play an important role that we will discuss when evaluating the
experiments. Fitness of candidate solution is measured as the number of food
pieces eaten within the time limit.


```
Empty squares Food Ant
```
**Figure** 2**.** 1 **:** The Santa Fe ant trail. 89 pieces of ant food on a 32 by 32 toroidal grid. Ant
starts in the north-east corner facing east

### 2 3 Symbolic regression with multiple types

Symbolic regression can feature values of multiple types: for instance when the
independent variables are vectors and the dependent variable is a scalar. It is
then desirable to restrict the search space to expressions with vector variables
that yield a scalar by applying correct operations to both scalar and vector
values. Such search space does not, of course, possess the closure property
required by traditional GP. The need to overcome this problem motivated
strongly-typed genetic programming (discussed in Section 1. 2 ), but it can be
shown that grammars can describe such simple type systems very efficiently.
Wong and Leung ( 2000 ) use two such problems to demonstrate that their
framework for logic-grammar based genetic programming LOGENPRO can
outperform GP when type constraints are involved and that it can also be used
to emulate ADFs (briefly introduced in Section 1. 2 ):

```
1. finding a real-valued function of two three-component vector variables
~x,~ythat computes the dot product~x·~y(the standard Euclidean inner
product, also called scalar product),
```
```
2. finding a real-valued function of three three-component vector variables
~x,~y,~zthat yields the scalar~x·~y+~y·~z, where·is the dot product, using
ADFs.
```

While Wong and Leung ( 2000 ) compare the performance of LOGENPRO
with a GP setup that does not use any syntactic constraints, I would suggest
that the point should not be that GP cannot use ad hoc syntactic constraints to
the same effect but that doing so using a grammar is more general but also far
easier and less error-prone: note that even in this seemingly simple example
there are three “natural” types of unary and binary operations (scalar-to-scalar,
vector-to-vector, vector-to-scalar) and perhaps scalar multiplication.
We will discuss the approach used by Wong and Leung ( 2000 ) and demon-
strate that other grammar-based methods can be used with similar results. We
will also analyse the role of ADFs in the second variant of the problem, which
Wong and Leung ( 2000 ) did not do properly.

### 2 4 Boolean symbolic regression

The spaces ofk-ary Boolean functions are relatively small (2^2 k), as are the suffi-
cient sets of basic functions from which they can be composed (e.g. the set of
common logical connectives{and,or,not}), yet the search space of their symbolic
representations is vast. Although symbolic regression of Boolean functions is
of no practical interest, these properties make it suitable for evaluating genetic
programming systematically. Koza ( 1992 ) has demonstrated that the difficulty
of finding different ternary Boolean functions by blind random search among
expressions of certain maximum size varies considerably and that GP is stat-
istically more successful than blind random search in finding those that are
particularly hard to find. (More details in Koza, 1992 , ch. 9 .)
His experiments have also shown that the hardest to find among ternary
functions are the parity functions (the even and odd parity function are true if
and only if the number of true arguments is even and odd, respectively). Con-
sequently, the parity functions are relatively hard to find even in the classes of
higher-arity Boolean functions. At the same time they can be easily composed
from simple building blocks: the exclusive-or functions, and a negation in case
of even parity. This in turn makes Boolean symbolic regression of the parity
functions suitable for testing the impact of ADFs on performance.
We will use the even-parity Boolean regression problem to test the perform-
ance of techniques for the grammar-based methods that emulate ADFs. Results
will be compared to those presented by Koza ( 1992 ) for tree-based GP with and
without ADFs.

### 2 5 Hyper-heuristics

In contrast to the previously presented applications,hyper-heuristicsis actually
a whole field covering very diverse problems. A hyper-heuristic search engine
does not search for problem solutions but rather for heuristics for solving a
problem. Thus if the class of problems of interest was Boolean satisfiability,


heuristics for a SAT solver would be sought rather than truth-value assign-
ments.
Different approaches can be used to find heuristics, but if some heuristics
have already been developed and can be combined in more complex ways then
just choosing a sequence, grammar-based GP can be used to great advantage.
The grammar allows us both to use the language of our problem-solving frame-
work directly and to embed our knowledge about the existing heuristics that
are to be used as building blocks.
Bader-El-Den et al. ( 2009 ) have recently successfully applied a grammar-
based GP hyper-heuristic framework to timetabling. The method they used
was essentially CFG-GP. We will try to replicate their results with our im-
plementation of CFG-GP, and compare them to the results obtained using
grammatical evolution in Section 3. 9.

### 2 6 Other applications

While we will not experiment with other problems, several other areas of
application are worth at least mentioning:
The authors of LOGENPRO have developed their system specifically to
target data mining problems. They see their logic grammar based framework
(covered in Section 1. 5 ) as a combination of two important approaches to data
mining: ILP and GP (Section 1. 2 and Section 1. 3 ). In addition to artificial
problems such as the dot product problem (see Section 2. 3 above), they apply
LOGENPRO to two benchmark data mining problems (credit card screening
using decision trees and the chess endgame problem using logic programming),
and to data mining from “real-life medical databases”. LOGENPRO provides
results competent with other learning systems in the first benchmark and out-
performs ILP systems FOIL, BEAM-FOIL, and mFOIL significantly at most
noise levels in the latter benchmark. It is interesting to note that in none of the
applications presented by Wong and Leung ( 2000 ) the specific feature of their
system, the power of logic grammars, is used. In particular, their applications
use only simple logic goals that could be replaced by short enumerations, and
none of the grammars feature context-dependence via unification. Conceiv-
ably, any of their results could be replicated using grammar-based methods
other than LOGENPRO, in the same way as we will show on the example of
the dot product problem in Section 3. 6. Thus, grammar-based methods can
also be used as a viable data mining framework.
Grammatical evolution has recently been used in dynamic applications,
where the grammar itself is described by a (meta-)grammar and co-evolves
along with the individual, allowing for further adaptability (Dempsey et al.,
2009 ).
Natural Computing Research & Applications Group at University College
Dublin (UCD NCRA), where most of the current work on grammatical evolu-
tion is being done, has also used GE to evolve a logo for the group interactively


(O’Neill and Brabazon, 2008 ). Their web site^1 showcases other unconventional
and creative applications including evolution of elevator music. Results in such
applications cannot of course be quantitatively measured but they demonstrate
the variety of objects and processes that can be described using a grammar and
consequently evolved using grammar-based GP methods.

(^1) [http://ncra.ucd.ie/](http://ncra.ucd.ie/)


## Chapter 3

## Experiments with Grammar-Based

## Methods

In this chapter we will perform several experiments with instances of the
problems chosen in Chapter 2.
When evaluating CFG-GP and GE, we will use the AGE (Algorithms for
Grammar-based Evolution, originally Algorithms for Grammatical Evolution)
framework, which is a competent and well-documented implementation of GE
and common evolutionary algorithm elements, as I have shown in my bachelor
thesis (Nohejl, 2009 ). For the purpose of these experiments AGE has been ex-
tended with CFG-GP algorithm elements implemented according to Whigham
( 1995 ). This will allow us to test CFG-GP and GE in almost identical setups. See
Chapter 4 for more information about AGE and its implementation of CFG-GP.
The software is available on the accompanying medium and online^1.
The performance of LOGENPRO will be evaluated using the implementa-
tion that Dr Man Leung Wong, a co-author of the method (Wong and Leung,
1995 , 2000 ), has kindly provided to me.

### 3 1 Observed characteristics

As a measure of success I adopt the cumulative success rate over a number of
runs, as usual when evaluating evolutionary algorithms. The success in each
experiment is defined by theSuccess predicateentry in its table (see Section 3. 2
below). For the purpose of these experiments, the algorithms are left running
until the maximum number of generations is reached even when the success
predicate has been satisfied.
To further facilitate comparison, we will use several characteristics of de-
rivation trees applicable to both methods: tree height (defined in Section 1. 1 ),
number of choice nodes, and bushiness (both defined below). In the following
definitions, letG=(N,T,P,S) be a context-free grammar andρa derivation
tree for the grammarG.

(^1) [http://nohejl.name/age/](http://nohejl.name/age/)


**Definition.** We will say that a particular node ofρis achoice nodeif it is labelled
with a nonterminal for whichPcontains more than one production. (Thus
a choice of production had to be made at this node.) Internal choice nodeis a
choice node that has at least one choice node among its descendants.

When using GE, the number of choice nodes coincides with the number of
codons used during the mapping process. Additionally, the number of choice
nodes reflects the amount of information contained in a given derivation tree
more accurately than the total number of nodes.

**Definition.** Letnbe the number of all choice nodes inρ, andkthe number of
internal choice nodes inρ. The ratiokn+ 1 expresses thebushinessofρ.

Bushiness is simply the ratio of the total number of choice nodes and the
number of their “parents”. Note that in any derivation tree that contains at
least one choice node, there is a non-empty group of topmost choice nodes (the
group has no choice node ancestor): the additive constant 1 in the formula acts
as a virtual parent of these choice nodes. The bushiness is thus analogous to the
average branching factor, except that it is defined only based on choice nodes.
AGE can report tree characteristics for both GE and CFG-GP. LOGENPRO
offers only basic data about fitness and success.

### 3 2 Setup

Both AGE, and LOGENPRO allow to configure parameters such as selection
method, operator probabilities, or maximum tree heights. We will present these
parameters for each experiment in a table derived from the “tableau” format
used by Koza ( 1992 ), and in a modified form by O’Neill and Ryan ( 2003 ). In
addition to the entries used by either of them, I also specify the details of
the algorithm in the entriesAlgorithm,Selection,Initialisation,Operators, and
Parameters(for other method-specific parameters).
In order to make comparisons statistically relevant, we will use data from at
least 100 runs. In the case of AGE, these are always runs from one sequence with
random number generator (RNG) seed value 42; in the case of LOGENPRO
these are runs with RNG seeds from the sequence 1, 2 , 3 ,...,n. (LOGENPRO
normally seeds the generator with the current time. I have modified its source
code to use a fixed number to seed the generator.)

### 3 3 Statistics

We will use several statistics: averageusing arithmetic mean, variance, and
coefficient of variance(CV), which is a relative measure of dispersion defined
as the ratio of the standard deviation to the absolute value of the mean. The
statistics will be computed from a population of individuals from a particular
generation. When evaluating results from multiple runs of the same setup


we will average these statistics (using arithmetic mean) over the performed
runs. If a population contains invalid individuals, they are excluded from the
statistics.
When I describe a difference between two sets of numerical results (or rates
of success in two sets) assignificant, it means that the statistical hypothesis that
the two sets are samples of a statistical population with the same mean (or that
the probabilities of success in the two sets are the same), has to be rejected on
the level of statistical significance of 5 %. Accordingly, whenever a difference
is said to beinsignificant, the same hypothesis cannot be rejected on the level
of 5 %. Unless stated otherwise, any differences I point out are significant. I
use Student’st-test for testing equivalence of means, and a simple test of equal
proportions for testing the probabilities of success.^2

### 3 4 Simple symbolic regression

We will compare the performance of CFG-GP and GE in a simple instance of
the symbolic regression problem (Section 2. 1 ).

#### 3 5 1 Experimental setups

The first setup of GE is deliberately based on the parameters used in the sym-
bolic regression example supplied with GEVA^3. I have used the same setup
(except for maximum tree height, see explanation below) in my bachelor thesis
(Nohejl, 2009 ) to show that the codon size higher than 8 bits does not provide
substantial advantage, and that AGE achieves a success rate of 706 and 701
out of 1000 runs (with 31 and 8 bits per codon, respectively), which was sig-
nificantly better than the rate achieved by GEVA. Thus we will also use 8-bit
codons in the current experiment.
When possible, we will use the same settings for CFG-GP: elite size, fitness
measure, selection scheme, etc., and also the crossover rate will be the same:
even though each method has a different crossover operator, the crossover
rate of 0.9 is a standard value of crossover probability in GP, which is used
systematically by both Whigham ( 1995 , 1996 ) and O’Neill and Ryan ( 2003 ).
The mutation rate for CFG-GP will be, somewhat arbitrarily, set to 0.1.
The higher nominal value is meant to reflect that the mutation rate in CFG-
GP is equivalent to its effective per-individual mutation rate, whereas the
effective per-individual rate in GE is much higher then the 0.02 per-codon
rate, and is dependent on the number of used codons and the distribution of
nonterminals in each individual. Therefore, no fixed per-individual mutation

(^2) Both are computed using theR stats package(functionst.testandprop.test), which is part
of the open-sourceR Project for Statistical Computingavailable athttp://www.r-project.org/.
(^3) GEVA is “an open source implementation of Grammatical Evolution [... ], which provides
a search engine framework in addition to a simple GUI and the genotype-phenotype mapper of
GE.” (O’Neill et al., 2008 ). It is being developed at Natural Computing Research & Applications
Group (NCRA) at University College Dublin:http://ncra.ucd.ie/Site/GEVA.html.


rate corresponds to a given per-codon rate used in GE. Whigham ( 1995 , 1996 )
has used mutation rates of 0.15 and 0, respectively, so 0.1 is a conservative
choice.
Because CFG-GP uses a maximum tree height for its operators, while GE
usually ensures reasonable tree size only implicitly, we will use 20 as maximum
tree height for both methods, a limit high enough to have only marginal impact
on the results of the GE setup.
The only parameter left is the maximum height of derivation trees created
by the CFG-GP initialisation method. We would like to operate on individuals
of roughly the same size as in the GE setup, so that both methods are evalu-
ated on search spaces of comparable size. I have experimentally determined
that maximum height 5 results in derivation trees of height closest to those
generated by the GE setup (as shown in Figure 3. 1 ).
The setup for both methods is detailed in Table 3. 1.

```
Objective: Find a function of one variablexrepresented by an
expression to fit a given set of the target function values at
specified points. The target function isx^4 +x^3 +x^2 +x.
Terminal operands: x, 1.0.
Terminal operators: +,−,·(all binary).
Grammar: See Listing 3. 1.
Fitness cases: 20 fixed points− 1 .0,− 0 .9,... , 1.9.
Raw fitness: The sum of squared errors taken over the 20 fitness cases.
Scaled fitness: Same as raw fitness.
Algorithm: Simple with elite size: 10, generations: 101, population:
100.
Selection: Tournament, size: 3.
GE initialisation: Random codon string initialisation, length: 200.
CFG-GP initialisation: “Grow” method (Whigham, 1995 ), maximum height: 5.
GE operators: Fixed-length one-point crossover, probability: 0.9.
Codon-level mutation, probability: 0.02.
CFG-GP operators: Crossover (Whigham, 1995 ), probability: 0.9.
Mutation (Whigham, 1995 ), probability: 0.1.
Common parameters: Maximum tree height: 20.
GE parameters: Maximum wraps: 3. Codon size: 8.
Success predicate: Raw fitness lower than 0.00001 (to allow for floating-point
round-offerror.)
```
**Table** 3**.** 1 **:** Simple symbolic regression, parameters for setups 1 ,CFG-GP ( 1 )andGE
( 1 ).

```
<expr> ::= ( <expr> <op> <expr> ) | <var>
<op> ::= + | - | *
<var> ::= x | 1.0
```
```
Listing 3. 1 : Simple grammar for symbolic regression in Lua.
```

#### 3 5 2 Results from setups

The final success rate of 999 out of 1000 runs achieved by CFG-GP has been
substantially higher than the 733 out of 1000 runs achieved by GE, moreover
CFG-GP was more successful in all generations and converged very fast (see
Figure 3. 1 ). While derivation tree heights suggest that both methods were
searching a similar space, a closer look at the tree characteristics (Figure 3. 3 )
reveals differences:
( 1 ) During the first five generations, GE has a very high CV of both tree
height and bushiness compared to CFG-GP, and consequently also a high vari-
ance of the number of choice nodes. This can be explained by the initialisation
method, and does not seem to have an important lasting effect.
( 2 ) After ( 1 ), CFG-GP has a significantly larger CV of tree height, and neither
method continuously dominates in average height.
( 3 ) CFG-GP produces significantly bushier trees throughout the run. After
( 1 ), neither method continuously dominates in CV of bushiness.
( 4 ) The compound effect of ( 2 ) and ( 3 ) is a higher variance of the number
of choice nodes in trees produced by CFG-GP. We can interpret this variance
(which is an absolute measure as opposed to CV) in conjunction with the
average as the extent of the search space. Note that this is observable even
if we do not take into account the portion of CFG-GP results after CFG-GP
reaches almost full success rate.
To examine the possibility that grammatical evolution produces less diverse
candidate solutions because of inappropriate operator parameters, let’s have
a look at fitness statistics in Figure 3. 2 : GE soon reaches higher coefficient of
variance than CFG-GP, and the CV keeps growing while average fitness stays
nearly constant in GE. Note that the difference is not due to invalid individuals,
which are excluded from the statistics.
This shows that the parameters of the operators result in enough phenotypic
diversity but fail to produce a favourable distribution of tree characteristics.
Additionally, even if a higher mutation rate had the effect of producing better
tree characteristics, it would effectively turn GE into a blind search: both its
standard operators can be very disruptive (their effect is not localised to a
subtree: see the “ripple” crossover discussed in O’Neill and Ryan, 2003 , and
the codon and bit-level mutation in my bachelor thesis, Nohejl, 2009 ). The
success rate reached in this experiment depends on the high elitism, which
compensates for the disruptive effect of operators.
We can conclude that CFG-GP has searched a larger search space (bushier
trees of more varied height) and has achieved substantially better results than
GE. (Note that the larger search space is not necessarily a superset of the
smaller one as GE may explore less bushy trees better). This may not be a
single direct cause of the higher success rate but being able to find a solution
more reliably while searching through more diverse candidate solutions is
certainly beneficial.


```
Generations
```
```
Cumulative success rate in 1000 runs
```
```
0 20 40 60 80 100
```
```
0
```
```
200
```
```
400
```
```
600
```
```
800
```
```
1000
```
```
GE (1)
CFG−GP (1)
```
**Figure** 3**.** 1 **:** Cumulative frequency of success of GE and CFG in symbolic regression of
the polynomialx^4 +x^3 +x^2 +x, setups 1.

```
Generations
```
```
Fitness
```
```
0 20 40 60 80 100
```
```
0
```
```
5
```
```
10
```
```
15
```
(^20) GE (1) aver. fitness
CFG−GP (1) aver. fitness
GE (1) best fitness
CFG−GP (1) best fitness
Generations
Coeff. of var. of fitness
0 20 40 60 80 100
0.0
0.5
1.0
1.5
2.0
GE (1) fitness CV
CFG−GP (1) fitness CV
**Figure** 3**.** 2 **:** Fitness in GE and CFG-GP in symbolic regression of the polynomialx^4 +
x^3 +x^2 +x, setups 1. The plots show averages, taken over 1000 runs, of
the following population statistics: averages, minima (best values), and
coefficients of variance of fitness.


```
Generations
```
```
Average tree height
```
```
0 20 40 60 80 100
```
```
2
```
```
4
```
```
6
```
```
8
```
```
10
```
```
12
```
```
GE (1) height
CFG−GP (1) height
```
```
Generations
```
```
Coeff. of var. of tree height
```
```
0 20 40 60 80 100
```
```
0.0
```
```
0.2
```
```
0.4
```
```
0.6
```
```
0.8
```
```
1.0 GE (1) height CV
CFG−GP (1) height CV
```
```
Generations
```
```
Average tree bushiness
```
```
0 20 40 60 80 100
```
```
1.0
```
```
1.2
```
```
1.4
```
```
1.6
```
```
1.8
```
```
2.0
```
```
GE (1) bushiness
CFG−GP (1) bushiness
```
```
Generations
```
```
Coeff. of var. of tree bushiness
```
```
0 20 40 60 80 100
```
```
0.000
```
```
0.005
```
```
0.010
```
```
0.015
```
```
0.020 GE (1) bushiness CV
CFG−GP (1) bushiness CV
```
```
Generations
```
```
Average of # choice nodes
```
```
0 20 40 60 80 100
```
```
0
```
```
20
```
```
40
```
```
60
```
```
80
```
```
100
```
```
GE (1) # choice nodes
CFG−GP (1) # choice nodes
```
```
Generations
```
```
Variance of # choice nodes
```
```
0 20 40 60 80 100
```
```
0
```
```
200
```
```
400
```
```
600
```
```
800
```
(^1000) GE (1) # ch. nodes var.
CFG−GP (1) # ch. nodes var.
**Figure** 3**.** 3 **:** Tree characteristics of GE and CFG-GP in symbolic regression of the poly-
nomialx^4 +x^3 +x^2 +x, setups 1. The plots show averages, taken over
1000 runs, of the following population statistics: averages and coefficients
of variance of tree height and tree bushiness, average and variance of the
number of choice nodes.


#### 3 5 3 Experimental setups

We have used grammatical evolution with fixed chromosome length in the pre-
vious setup. GE was, however, originally conceived to be used with variable-
length chromosomes (O’Neill and Ryan, 2003 ). We have also remarked that
the initial population created by the random codon string initialisation has an
extremely varied tree characteristics, which persist over several generations.
This time, we will use variable chromosome length in conjunction with two
different initialisation methods to find out how they affect the success rate and
tree shape diversity. The first method is random codon string initialisation as
in setups 1 but now with variable string length, and the second method is the
“grow” method analogous to the one normally used in CFG-GP only with ran-
dom genetic degeneracy added to each codon as usual in GE. Thus, using the
“grow” method with maximum height 5, GE will now start with statistically
equivalent phenotypes (trees and strings) to those CFG-GP started with in the
first setup.
As before, we will use the same values of the remaining parameters for
both methods when possible. As we have already evaluated CFG-GP with
maximum initialisation height 5, we will now use two different maximum
heights, 2 and 7, to assess the sensitivity of CFG-GP to characteristics of the
initial population. (Derivation trees of height 2 are the shortest possible and
can represent only the two strings1.0andx.) Settings used for both methods
are listed in Table 3. 2.

```
GE initialisation: GE ( 2 a): Random codon string initialisation, length:
100–150.
GE ( 2 b): “Grow” method (unique trees, random
codon-level degeneracy), maximum height: 5.
GE operators: Variable-length one-point crossover, probability: 0.9.
Mutation as before.
CFG-GP initialisation: CFG-GP ( 2 a)and( 2 b): “Grow” method (Whigham, 1995 ),
maximum height: 2 and 7, respectively.
CFG-GP operators: Crossover and mutation as before.
```
**Table** 3**.** 2 **:** Simple symbolic regression, parameters for setups 2 ,CFG-GP ( 2 a, 2 b), and
GE ( 2 a, 2 b), where different from setups 1 (Table 3. 1 ).

#### 3 5 4 Results from setups

Both GE settings were an improvement over setups 1. The use of variable-
length chromosomes alone inGE ( 2 a)resulted in a modest increase to 807
successful runs, while the use of the CFG-GP-like initialisation inGE ( 2 b)in
a marked increase to 923 successful runs. Along with this improvement the
tree-aware initialisation eliminated the extreme variation of tree characteristics
at the start of the run. Another remarkable difference between the results of the
two initialisation methods can be seen in the fitness statistics (Figure 3. 5 ). The


use of the random string initialisation in conjunction with variable-length chro-
mosomes resulted in frequent occurrence of highly unfit individuals through-
out the run (the plot had to be cropped). While such individuals could have
in principle occurred using any settings of both methods (they just represent
relatively complex arithmetic expressions with output very far from the tar-
get function), the occurred only in GE with variable-length chromosomes and
random-string initialisation (a combination used by O’Neill and Ryan, 2003 ,
although they acknowledge the positive effect of tree-based initialisation).
CFG-GP with both initialisation parameters was still better than GE. With
maximum initialisation height 2 it reached full success rate already in genera-
tion 58, and with maximum initialisation height 7 it succeeded in 992 runs out
of 1000. The success rates for heights 2, 5, and 7 are significantly different, yet
all three have led to successful solution quite consistently compared to GE.
We can make almost identical observations as in setups 1 about tree char-
acteristics of the two methods: regardless of the initialisation method, GE
results in bushier trees of more varied height and is able to expand the size
of its search space over that of GE, at the same time CV of bushiness is sim-
ilar for the two methods, and achieved tree heights can be too. This shows a
consistent difference between GE and CFG-GP not caused by initialisation or
use of variable-length vs. fixed-length chromosomes in GE, that is a difference
resulting from the nature of operators of the two methods. The CFG-GP oper-
ators have searched the solution space of this simple problem more effectively
than those of GE; additionally, CFG-GP has been relatively insensitive to the
characteristics of the initial population.

```
Generations
```
```
Cumulative success rate in 1000 runs
```
```
0 20 40 60 80 100
```
```
0
```
```
200
```
```
400
```
```
600
```
```
800
```
```
1000
```
```
GE (2a)
GE (2b)
CFG−GP (2a)
CFG−GP (2b)
```
**Figure** 3**.** 4 **:** Cumulative frequency of success of GE and CFG-GP in symbolic regression
of the polynomialx^4 +x^3 +x^2 +x, setups 2.


```
Generations
```
```
Fitness
```
```
0 20 40 60 80 100
```
```
0
```
```
10
```
```
20
```
```
30
```
```
40
```
(^50) GE (2a) aver. fitness
GE (2b) aver. fitness
CFG−GP (2a) aver. fitness
CFG−GP (2b) aver. fitness
Generations
Coeff. of var. of fitness
0 20 40 60 80 100
0.0
0.5
1.0
1.5
2.0
2.5
3.0
GE (2a) fitness CV
GE (2b) fitness CV
CFG−GP (2a) fitness CV
CFG−GP (2b) fitness CV
**Figure** 3**.** 5 **:** Fitness in GE and CFG-GP in symbolic regression of the polynomialx^4 +
x^3 +x^2 +x, setups 2. The plots show averages, taken over 1000 runs, of
the following population statistics: averages, and coefficients of variance
of fitness. The fitness plot was cropped (inclusion of outliers would make
it difficult to discern the important differences)

#### 3 4 5 Conclusion

The presented problem of simple symbolic regression should indeed be simple
to solve even with the relatively small population and number of generations:
the grammar is relatively constrained and thus embeds significant amount of
knowledge about the solution. The experiments have shown that CFG-GP
can search the small space very effectively, achieving nearly full success rate
regardless of selection method and initial population characteristics, whereas
the success rate of grammatical evolution has been conspicuously low in com-
parison with CFG-GP despite improvements brought by a more appropriate
selection method and the use of variable-length chromosomes.
Further analysis of characteristics of derivation trees during the run have
shown that GE explores the same search space less effectively: its populations
are less diverse than those evolved by CFG-GP. This is the result of different
representation and operators employed by the two methods, and it is likely
the cause of the disappointing performance of GE in this simple benchmark
problem.


```
Generations
```
```
Average tree height
```
```
0 20 40 60 80 100
```
```
2
```
```
4
```
```
6
```
```
8
```
```
10
```
```
12
```
```
GE (2a) height
GE (2b) height
CFG−GP (2a) height
CFG−GP (2b) height
```
```
Generations
```
```
Coeff. of var. of tree height
```
```
0 20 40 60 80 100
```
```
0.0
```
```
0.2
```
```
0.4
```
```
0.6
```
```
0.8
```
```
1.0 GE (2a) height CV
GE (2b) height CV
CFG−GP (2a) height CV
CFG−GP (2b) height CV
```
```
Generations
```
```
Average tree bushiness
```
```
0 20 40 60 80 100
```
```
1.0
```
```
1.2
```
```
1.4
```
```
1.6
```
```
1.8
```
```
2.0
```
```
GE (2a) bushiness
GE (2b) bushiness
CFG−GP (2a) bushiness
CFG−GP (2b) bushiness
```
```
Generations
```
```
Coeff. of var. of tree bushiness
```
```
0 20 40 60 80 100
```
```
0.000
```
```
0.005
```
```
0.010
```
```
0.015
```
```
0.020 GE (2a) bushiness CV
GE (2b) bushiness CV
CFG−GP (2a) bush. CV
CFG−GP (2b) bush. CV
```
```
Generations
```
```
Average of # choice nodes
```
```
0 20 40 60 80 100
```
```
0
```
```
20
```
```
40
```
```
60
```
```
80
```
```
100
```
```
GE (2a) # choice nodes
GE (2b) # choice nodes
CFG−GP (2a) # choice nodes
CFG−GP (2b) # choice nodes
```
```
Generations
```
```
Variance of # choice nodes
```
```
0 20 40 60 80 100
```
```
0
```
```
500
```
```
1000
```
(^1500) GE (2a) # ch. nodes var.
GE (2b) # ch. nodes var.
CFG−GP (2a) # ch. nodes var.
CFG−GP (2b) # ch. nodes var.
**Figure** 3**.** 6 **:** Tree characteristics of GE and CFG-GP in symbolic regression of the poly-
nomialx^4 +x^3 +x^2 +x, setups 2. The plots show averages, taken over
1000 runs, of the following population statistics: averages and coefficients
of variance of tree height and tree bushiness, average and variance of the
number of choice nodes.


### 3 5 Santa Fe ant trail

In this experiment with the Santa Fe ant trail (described in Section 2. 2 ) we
will again start with a setup based upon an example GE application as imple-
mented and configured in GEVA. Although the success of GE will initially be
poor, it should be noted that this is not due to the implementation (as I have
demonstrated in my bachelor thesis, Nohejl, 2009 , GEVA achieves similarly
poor results: in fact, AGE achieved exactly the same level of success when
using the same initialisation method). All setups will use a relatively small
population (100) and number of generations (101), so it is very likely that no
parameters for GE and CFG-GP would result in full success. This way we will
be able to compare population characteristics more clearly. In the symbolic
regression experiment, CFG-GP converged so fast, that only a relatively small
portion of the statistics concerned what happens until the solution is found.

#### 3. 5. 1 Experimental setups 1

We will use the standard Santa Fe ant trail map in conjunction with a 600-tick
time limit for each individual and the grammar in Listing 3. 2 to replicate the
configuration used in GEVA. Our goal is to compare different methods, not
different grammars, but it is worth noting that the specification of grammar
in this application offers room for subtle but important choices. The terminals
simply represent the ant’s actions and senses but we can decide how to combine
them: for instance the grammar that we use does not allow nested conditional
branching. We can see this as a bit of knowledge about the problem that has
been embedded in the grammar: we (correctly) assume that nested conditional
branching is not necessary for the solution. Additionally, the way the<line>
and<op>nonterminals are chained in<code>and<opcode>productions will
result in relatively high and sparse trees (compare to Listing 3. 1 for symbolic re-
gression, which can produce binary branching recursively). Both these features
of the grammar result in a smaller search space and a faster search.

```
<prog> ::= <code>
<code> ::= <line> | <code> <line>
<line> ::= <condition> | <op>
<condition> ::= if foodAhead(h)==1 then <opcode> else <opcode> end
<op> ::= left(h) | right(h) | move(h)
<opcode> ::= <op> | <opcode> <op>
```
**Listing** 3**.** 2 **:** A Lua equivalent to the Groovy grammar used for Santa Fe ant trail
application included with GEVA.

In Section 3. 4 we have suggested that different operators in CFG-GP and GE
result in different spaces of tree shapes to be searched, but different initialisation
techniques may have also played a role. This time, we will use the same two
initialisation techniques with both methods. ForGE ( 1 a)andCFG-GP ( 1 a), we


will use the “sensible” initialisation as proposed by O’Neill and Ryan ( 2003 )
for GE. The “sensible” initialisation, being a variation on the ramped half-
and-half initialisation (Koza, 1992 ), is a tree-based method that can be easily
transposed from GE to CFG-GP. It is also used in the Santa Fe application in
GEVA, although not implemented correctly (Nohejl, 2009 , sec. 8. 3. 2 ). ForGE
( 1 b)andCFG-GP ( 1 b)we will again use the same initialisation for both methods:
“grow” initialisation as used by Whigham ( 1995 ) for CFG-GP. We will use the
same maximum height value as for “sensible” initialisation, but the resulting
average heights and distributions of tree shapes will be different.

Other parameters are listed in Table 3. 3. Again, we use the same parameters
whenever possible. In the case of mutation rate, which is measured differently
for each method, we use a 0.01 per-codon rate for GE as in the setup used by
GEVA and a 0.1 per-individual rate for CFG-GP as before (see Section 3. 4. 1 for
details about the choice). We will try to assess the impact of mutation rates in
the discussion of results.

```
Objective: Find a program for navigating the ant so that it finds all 89
pieces of food on the Santa Fe trail within 600 time units.
Terminal operands: None.
Terminal operators: Left, Right, Move, Food-Ahead.
Grammar: See Listing 3. 2.
Fitness cases: One fitness case.
Raw fitness: Number of pieces of food left on the grid after 600 time
units of running the ant’s program in a loop.
Scaled fitness: Same as raw fitness.
Algorithm: Simple with elite size: 10, generations: 101, population:
100.
Selection: Tournament, size: 3.
GE initialisation: GE ( 1 a): Ramped (“sensible”), maximum height: 6.
GE ( 1 b): “Grow” method (Whigham, 1995 ), maximum
height: 6.
CFG-GP initialisation: CFG-GP ( 1 a): Ramped (“sensible”), maximum height: 6.
CFG-GP ( 1 b): “Grow” method (Whigham, 1995 ),
maximum height: 6.
GE operators: Variable-length one-point crossover, probability: 0.9.
Codon-level mutation, probability: 0.01.
CFG-GP operators: Crossover (Whigham, 1995 ), probability: 0.9.
Mutation (Whigham, 1995 ), probability: 0.1.
Common parameters: Maximum tree height: 20.
GE parameters: Maximum wraps: 3. Codon size: 8.
Success predicate: Raw fitness equals 0 (all food eaten).
```
```
Table 3. 3 : Santa Fe ant trail, parameters for setups 1 ,GE ( 1 a, 1 b),CFG-GP ( 1 a, 1 b).
```

#### 3. 5. 2 Results from setups 1

Both methods were relatively unsuccessful. CFG-GP with either initialisation
technique achieved a substantially higher success rate (191 and 206 out of 1000
runs were successful) than GE (9 and 24 runs out of 1000), see Figure 3. 7. The
differences caused by initialisation are small relatively to differences between
GE and CFG-GP. The same pattern can be observed in tree characteristics
except for bushiness where all differences are too small to draw a conclusion.
Regardless of the initialisation method, the GE setups and likewise the CFG-GP
setups converge to similar characteristics.
As in the previous setups for symbolic regression, in the long run CFG-
GP with either initialisation method results in trees that are higher and more
varied in height and number of choice nodes. The difference in success of the
two methods is, however, much larger than before. Could this be attributed
to a disparity in mutation rates? The coefficient of variance of fitness in the
symbolic regression setups was either similar for both methods or higher for
GE, now it is lower for GE (see Figure 3. 8 ), which would suggest a lower
effective mutation rate given no disparity in other parameters.

```
Generations
```
```
Cumulative success rate in 1000 runs
```
```
0 20 40 60 80 100
```
```
0
```
```
50
```
```
100
```
```
150
```
```
200
```
```
250 GE (1a)
GE (1b)
CFG−GP (1a)
CFG−GP (1b)
```
**Figure** 3**.** 7 **:** Cumulative frequency of success of GE and CFG-GP in Santa Fe ant trail,
setups 1. (Scale from 0 to 250 out of 1000.)

We have observed the same kind of differences in success rates and tree
characteristics between GE and CFG-GP as before but now independently of
initialisation. Comparison of fitness statistics suggests, however, that part of
the difference in success may be attributed to different effective mutation rates.
Additionally, the less sophisticated “grow” method was more successful with
both GE and CFG-GP. We will investigate the effect of initialisation using the
second set of experimental setups.


```
Generations
```
```
Fitness
```
```
0 20 40 60 80 100
```
```
0
```
```
20
```
```
40
```
```
60
```
```
80
```
(^100) GE (1a) aver. fitness
GE (1b) aver. fitness
CFG−GP (1a) aver. fitness
CFG−GP (1b) aver. fitness
Generations
Coeff. of var. of fitness
0 20 40 60 80 100
0.0
0.1
0.2
0.3
0.4
0.5
0.6
GE (1a) fitness CV
GE (1b) fitness CV
CFG−GP (1a) fitness CV
CFG−GP (1b) fitness CV
**Figure** 3**.** 8 **:** Fitness in GE and CFG-GP in Santa Fe ant trail, setups 1. The plots show
averages, taken over 1000 runs, of the following population statistics: av-
erages, and coefficients of variance of fitness.


```
Generations
```
```
Average tree height
```
```
0 20 40 60 80 100
```
```
4
```
```
6
```
```
8
```
```
10
```
```
12
```
```
GE (1a) height
GE (1b) height
CFG−GP (1a) height
CFG−GP (1b) height
```
```
Generations
```
```
Coeff. of var. of tree height
```
```
0 20 40 60 80 100
```
```
0.0
```
```
0.1
```
```
0.2
```
```
0.3
```
```
0.4 GE (1a) height CV
GE (1b) height CV
CFG−GP (1a) height CV
CFG−GP (1b) height CV
```
```
Generations
```
```
Average tree bushiness
```
```
0 20 40 60 80 100
```
```
1.0
```
```
1.1
```
```
1.2
```
```
1.3
```
```
1.4
```
```
1.5
```
```
1.6
```
```
GE (1a) bushiness
GE (1b) bushiness
CFG−GP (1a) bushiness
CFG−GP (1b) bushiness
```
```
Generations
```
```
Coeff. of var. of tree bushiness
```
```
0 20 40 60 80 100
```
```
0.000
```
```
0.005
```
```
0.010
```
```
0.015
```
```
0.020 GE (1a) bushiness CV
GE (1b) bushiness CV
CFG−GP (1a) bush. CV
CFG−GP (1b) bush. CV
```
```
Generations
```
```
Average of # choice nodes
```
```
0 20 40 60 80 100
```
```
0
```
```
10
```
```
20
```
```
30
```
```
40
```
```
GE (1a) # choice nodes
GE (1b) # choice nodes
CFG−GP (1a) # choice nodes
CFG−GP (1b) # choice nodes
```
```
Generations
```
```
Variance of # choice nodes
```
```
0 20 40 60 80 100
```
```
0
```
```
20
```
```
40
```
```
60
```
```
GE (1a) # ch. nodes var.
GE (1b) # ch. nodes var.
CFG−GP (1a) # ch. nodes var.
CFG−GP (1b) # ch. nodes var.
```
**Figure** 3**.** 9 **:** Tree characteristics of GE and CFG-GP in Santa Fe ant trail, setups 1. The
plots show averages, taken over 1000 runs, of the following population
statistics: averages and coefficients of variance of tree height and tree
bushiness, average and variance of the number of choice nodes.


#### 3. 5. 3 Experimental setups 2

In the second round we will use GE with bit-level, instead of codon-level,
mutation rate 0.01 with the aim to increase the effective mutation rate. We
expect this to cause GE to have comparable (or higher) CV of fitness than
CFG-GP. We will also use a different initialisation method then in setups 1 :
the ramped half-and-half method modelled exactly after that introduced by
Koza, 1992 , which differs from the “sensible” initialisation (O’Neill and Ryan,
2003 ) most importantly by ensuring generation of unique trees at the expense
of raising their height above the designated maximum (in the same way as the
“grow” method used by Whigham, 1995 ). We suppose that this caused the
relative success of the simpler “grow” method in setups 1. If this is correct, the
ramped half-and-half initialisation should fare at least as well as the “grow”
initialisation.

```
The differences from the previous setups are listed in Table 3. 4.
```
```
GE initialisation: Ramped half-and half (unique trees), maximum height: 6.
GE operators: Crossover as before. Bit-level mutation, probability: 0.01.
CFG-GP initialisation: Ramped half-and half (unique trees), maximum height: 6.
CFG-GP operators: Crossover and mutation as before.
```
**Table** 3**.** 4 **:** Santa Fe ant trail, parameters for setups 2 ,CFG-GP ( 2 ), andGE ( 2 ), where
different from setups 1 (Table 3. 3 ).

#### 3. 5. 4 Results from setups 2

The final success rates of CFG-GP and GE are 710 and 251 (see Figure 3. 10 ), both
several times higher than in the previous setups. The ramped half-and half
initialisation has created trees of height only slightly lower than the “grow”
initialisation but higher than the “sensible” initialisation. This shows that gen-
eration of unique trees, and the automatic increase of height it causes if needed,
are vital parts of the ramped half-and-half initialisation. Without uniqueness it
has perform worse than a simple “grow” method with uniqueness, while with
uniqueness it performs substantially better.

The gap in success between CFG-GP and GE is also smaller than in the pre-
vious setups, and as we can see in Figure 3. 11 , the higher effective mutation rate
has raised the CV of fitness for GE slightly above that of CFG-GP throughout
the run.

Thus, we have evaluated GE and CFG-GP with similar effective mutation
rates and the same initialisation with best results so far for both methods in the
Santa Fe ant trail. The statistics of tree shapes of both methods are still related
to each other in the same way as before and GE is still less successful.


```
Generations
```
```
Cumulative success rate in 1000 runs
```
```
0 20 40 60 80 100
```
```
0
```
```
200
```
```
400
```
```
600
```
```
800
```
```
1000 GE (2)
CFG−GP (2)
```
**Figure** 3**.** 10 **:** Cumulative frequency of success of GE and CFG-GP in Santa Fe ant trail,
setups 2.

#### 3 5 5 Conclusion

The Santa Fe ant trail problem differed from the simple symbolic regression.
Foremost, it was more difficult to solve given the relatively small population
size and number of generations. Also, the grammar we used was more re-
strictive in the sense that it excluded some constructs that traditional genetic
programming would allow in this problem (nested if-then-else conditions) and
that it did not allow recursive binary branching. Without recursive binary
branching, trees are less bushy, and consequently there are not as many unique
trees of a given height.
We have used the same parameters based on the setup used in GEVA
for both GE and CFG-GP, including the same initialisation method (“sensible
initialisation” proposed for GE by O’Neill and Ryan, 2003 ). We have also
evaluated both methods with the “grow” initialisation (designed for use with
CFG-GP by Whigham, 1995 ), and the ramped half-and-half initialisation (ori-
ginally designed for tree-based GP by Koza, 1992 ). We have adjusted mutation
rate for GE to reach similar effect in both methods (the mutation operators are
qualitatively different, so we could do this only based on the CV of fitness).
In all cases the methods displayed the same differences in tree characteristics
we have already observed in simple symbolic regression (Section 3. 4 ), which
prevailed in the long run over the effect of initialisation. At the same time,
we could see that both methods benefit from the same changes in initialisa-
tion. Koza’s ramped half-and-half initialisation had the best results of the three
tree-based techniques, partially thanks to the generation of unique individuals
which automatically raises tree height as necessary.


```
Generations
```
```
Fitness
```
```
0 20 40 60 80 100
```
```
0
```
```
20
```
```
40
```
```
60
```
```
80
```
(^100) GE (2) aver. fitness
CFG−GP (2) aver. fitness
GE (2) best fitness
CFG−GP (2) best fitness
Generations
Coeff. of var. of fitness
0 20 40 60 80 100
0.0
0.2
0.4
0.6
0.8
1.0
GE (2) fitness CV
CFG−GP (2) fitness CV
**Figure** 3**.** 11 **:** Fitness in GE and CFG-GP in Santa Fe ant trail, setups 2. The plots show
averages, taken over 1000 runs, of the following population statistics:
averages, minima (best values), and coefficients of variance of fitness.


```
Generations
```
```
Average tree height
```
```
0 20 40 60 80 100
```
```
4
```
```
6
```
```
8
```
```
10
```
```
12
```
```
GE (2) height
CFG−GP (2) height
```
```
Generations
```
```
Coeff. of var. of tree height
```
```
0 20 40 60 80 100
```
```
0.0
```
```
0.1
```
```
0.2
```
```
0.3
```
```
0.4 GE (2) height CV
CFG−GP (2) height CV
```
```
Generations
```
```
Average tree bushiness
```
```
0 20 40 60 80 100
```
```
1.0
```
```
1.1
```
```
1.2
```
```
1.3
```
```
1.4
```
```
1.5
```
```
1.6
```
```
GE (2) bushiness
CFG−GP (2) bushiness
```
```
Generations
```
```
Coeff. of var. of tree bushiness
```
```
0 20 40 60 80 100
```
```
0.000
```
```
0.005
```
```
0.010
```
```
0.015
```
```
0.020 GE (2) bushiness CV
CFG−GP (2) bush. CV
```
```
Generations
```
```
Average of # choice nodes
```
```
0 20 40 60 80 100
```
```
0
```
```
10
```
```
20
```
```
30
```
```
40
```
```
GE (2) # choice nodes
CFG−GP (2) # choice nodes
```
```
Generations
```
```
Variance of # choice nodes
```
```
0 20 40 60 80 100
```
```
0
```
```
20
```
```
40
```
```
60
```
```
80
```
```
100
```
```
GE (2) # ch. nodes var.
CFG−GP (2) # ch. nodes var.
```
**Figure** 3**.** 12 **:** Tree characteristics of GE and CFG-GP in Santa Fe ant trail, setups 2. The
plots show averages, taken over 1000 runs, of the following population
statistics: averages and coefficients of variance of tree height and tree
bushiness, average and variance of the number of choice nodes.


### 3 6 Dot product symbolic regression

In the previous experiments we have compared GE and CFG-GP as imple-
mented in AGE, and we started with setups based on those used for GE in
GEVA. In this section we will compare three methods: GE, CFG-GP, and LO-
GENPRO. The dot product problem and the initial setup we will use has been
described by Wong and Leung ( 2000 , sec. 6. 1. 2 ), who use it to demonstrate that
LOGENPRO can express type constraints, thus outperforming basic tree-based
GP, and generate ephemeral constants using logic goals (see Section 2. 3 ).

```
start -> s-expr(number).
s-expr([list,number,?n]) -> [ (mapcar(function ], op2, [ ) ],
s-expr([list,number,?n]),
s-expr([list,number,?n]), [ ) ].
s-expr([list,number,?n]) -> [ (mapcar(function ], op1, [ ) ],
s-expr([list,number,?n]), [ ) ].
s-expr([list,number,?n]) -> term([list,number,?n]).
s-expr(number) -> term(number).
s-expr(number) -> [ (apply(function ], op2, [ ) ],
s-expr([list,number,?n]),[ ) ].
s-expr(number) -> [ ( ], op2, s-expr(number),
s-expr(number), [ ) ].
s-expr(number) -> [ ( ], opl, s-expr(number), [ ) ].
op2 -> [ + ].
op2 -> [ - ].
op2 -> [ * ].
op2 -> [ % ].
op1 -> [ protected-log ].
term([list,number,n]) -> [ X ].
term([list,number,n]) -> [ Y ].
term(number) -> { random(-10, 10, ?a) }, [ ?a ].
```
**Listing** 3**.** 3 **:** Logic grammar (LOGENPRO) for dot product symbolic regression in Lisp
(from Wong and Leung, 2000 ). Unlike in a DCG,->is the production sym-
bol, logic variables are denoted by leading question marks, and terminals
(in square brackets) do not need additional quotes.

To this end, Wong and Leung ( 2000 ) use an elaborate logic grammar (List-
ing 3. 3 ) in a formalism based on DCGs. The terminals that contain calls to
( 1 )mapcarand ( 2 )applyare segments of Lisp code that apply the scalar op-
erators to vectors ( 1 ) by components and ( 2 ) to components of a single vector,
respectively. This is not something inherent to logic grammars, it is a conveni-
ent way to express different types of unary and binary operators in Lisp, which
LOGENPRO uses to evaluate individuals.
What is interesting about the grammar is how it expresses type constraints.
Two types are used: scalars and vectors of a given number of components. The


grammar expresses the types with arguments to functorsterm, ands-expr:
numberfor scalars and[list,number,?n] or [list,number,n]for vectors.
Seemingly, this takes advantage of the formalism of logic grammars andncould
express the number of vector components, but upon further inspection, we see
that all occurrences of the variablen(denoted?n) are unified with each other
in every derivation, and in the end, via the two penultimate productions, also
with the atomn. The compound term[list,number,n]does not have any in-
herent meaning, so all occurrences of[list,number,?n]and[list,number,n]
could be replaced with an arbitrary atom (sayvector) without any change to
the semantics of the grammar. Therefore all rules without logic goals in the
grammar could also be represented in a context-free grammar.
The last rule of the grammar contains a logic goalrandom(-10, 10, ?a),
which unifies the variableawith a random floating-point number between
-10and-10. LOGENPRO evaluates logic goals only once: when generating
the trees in initialisation or mutation, so this results in the same behaviour as
the ephemeral random constants as used Koza ( 1992 ). This is the only case
of actual use of the computational power of logic grammars demonstrated by
Wong and Leung ( 2000 ). As the efficient evolution of ephemeral constants in
GP is a nontrivial problem on its own (see Dempsey et al., 2009 , ch. 5 – 6 ) and
the solution to the dot product problem does not actually require constants,
we will use the simple and obvious technique for CFG-GP and GE: there will
be fixed nonterminals representing several arbitrarily chosen numbers from
the target range. We will also evaluate LOGENPRO’s performance with this
simpler approach using a modified version of the original grammar (outlined
inListing 3. 4 ).

```
The grammar is obtained from Listing 3. 3 by replacing the production
term(number) -> { random(-10, 10, ?a) }, [ ?a ].
with the following 21 productions:
term(number) -> [ -10 ].
term(number) -> [ -9 ].
..
.
term(number) -> [ 9 ].
term(number) -> [ 10 ].
```
**Listing** 3**.** 4 **:** Alternative logic grammar (LOGENPRO) for dot product symbolic regres-
sion in Lisp. Instead of generating random numbers via logic goals, it
includes several arbitrary values as terminals.

This application is interesting chiefly as a demonstration of how grammars
can efficiently describe type constraints. Once the constraints are enforced, it
is a relatively easy problem: the tree height of the shortest solution is 5 and
generating populations of 100 individuals using the “grow” method without
uniqueness with maximum height 5 finds a solution in 241 cases out of 1000
(experiment performed in the same way as all experiments in the chapter).


#### 3 6 1 Experimental setups

In the setup for LOGENPRO we will use the two presented grammars: the one
used by Wong and Leung ( 2000 ), and the modified version for the sake of better
comparison with CFG-GP and GE. Other parameters were chosen according
to the description of the basic LOGENPRO algorithm and the original dot
product experiment by Wong and Leung ( 2000 , sec. 5. 5 , 6. 1. 2 ). The book,
however, omits selection method (LOGENPRO uses either roulette-wheel or
tournament selection), operator probabilities, and maximum tree height (for
initialisation and during the run). I have chosen these parameters except the
mutation rate according to the configuration files for the dot product problem
provided to me along with the LOGENPRO source code by Dr Man Leung
Wong. The mutation rate was set to 0.05 instead of 0 because:

- Wong and Leung ( 2000 ) present LOGENPRO as a method that uses two
operators, crossover and mutation, evaluating a special case of the method
would not be as valuable,
- LOGENPRO with mutation performed slightly better in this problem.
Additionally, I have redefined theprotected-logfunction so that it returns
1 when undefined as specified by Wong and Leung ( 2000 ). (The source code
supplied by Dr Wong defined the values for arguments less than 1 as 0.)
GE and CFG-GP were set up in the same way when possible. Both meth-
ods used the “grow” method without uniqueness, like LOGENPRO did, and
the same grammar (Listing 3. 5 ), which emulates the alternative grammar for
LOGENPRO. For GE we have lowered the nominal mutation rate (similarly to
the previous experiments). Table 3. 5 contains the full parameters of the setups.

```
<start> ::= return <num>
<num> ::= <op2>v(<vec>)
| <op2>(<num>,<num>)
| <op1>(<num>)
| <num-term>
<vec> ::= v<op2>(<vec>,<vec>)
| v<op1>(<vec>)
| <vec-term>
<op2> ::= add | sub | mul | div
<op1> ::= log
<vec-term> ::= x | y
<num-term> ::= -10|-9|-8|-7|-6|-5|-4|-3|-2|-1
| 0|1|2|3|4|5|6|7|8|9|10
```
**Listing** 3**.** 5 **:** Context-free grammar in BNF for dot product symbolic regression in Lua.
Vector-to-scalar operator names are suffixed byv. Vector operator names
and prefixed byv. Scalar operators are named without suffix or prefix:
add,sub,mul,div,log.


```
Objective: Find a real-valued function of two three-component
vector variables~x,~ythat computes the dot product~x·~y.
Terminal operands: ~x,~y, ephemeral scalar constants between−10 and 10.
Terminal operators: Addition, subtraction, multiplication, and division in
three variants: scalar, vector (by components),
vector-to-scalar (applied to components of a single
vector); binary logarithm: scalar and vector (by
components). Logarithm and division are protected
(returning 1 when undefined).
Grammar: See Listing 3. 3 (original LOGENPRO grammar, results
marked with *), Listing 3. 4 (modified LOGENPRO
grammar), Listing 3. 5 (grammar for GE and CFG-GP).
Fitness cases: 10 vectors with components from{ 0 , 1 , 2 , 3 }randomly
generated for each run.
Raw fitness: The sum of absolute errors taken over the 10 fitness cases.
Scaled fitness: Same as raw fitness.
Algorithm: Simple, generations: 100, population: 100.
Selection: Tournament, size: 7.
LOGENPRO init.: “Grow” method without uniqueness (Wong and Leung,
2000 ), maximum height: 6.
CFG-GP initialisation: “Grow” method without uniqueness, maximum height:
6.
GE initialisation: “Grow” method without uniqueness, maximum height:
6.
LOGENPRO operators: Crossover (Wong and Leung, 2000 ), probability: 0.9.
Mutation (Wong and Leung, 2000 ), probability: 0.05.
Crossover and mutation are mutually exclusive.
CFG-GP operators: Crossover (Whigham, 1995 ), probability: 0.9.
Mutation (Whigham, 1995 ), probability: 0.05.
GE operators: Fixed-length one-point crossover, probability: 0.9.
Bit-level mutation, probability: 0.001.
Common parameters: Maximum tree height: 9.
GE parameters: Maximum wraps: 3. Codon size: 8.
Success predicate: Raw fitness lower than 0.00001 (to allow for
floating-point round-offerror.)
```
**Table** 3**.** 5 **:** Dot product symbolic regression, parameters for LOGENPRO, CFG-GP, and
GE. Note: Although the LOGENPRO implementation measures tree height
by nodes instead of edges, the table follows the established convention (see
page 5 ).


#### 3 6 2 Results

The success rates of the four setups (Figure 3. 13 ) are similar, often without
a significant difference, only during the first thirteen generations. Thereafter,
LOGENPRO with the original grammar is significantly less successful then
any of the other three setups, LOGENPRO with the modified grammar is not
significantly different from CFG-GP in generations 28 through 59, and CFG-GP
is not significantly different from GE in generations 60 through 70, 75, 76, and
then from 82 on. LOGENPRO with the original grammar, LOGENPRO with
the modified grammar, CFG-GP, and GE have respectively succeeded in 53, 62,
68, and 68 of the 100 runs.

```
Generations
```
```
Cumulative success rate in 100 runs
```
```
0 20 40 60 80 100
```
```
0
```
```
20
```
```
40
```
```
60
```
```
80
```
```
100
```
```
LOGENPRO*
LOGENPRO
CFG−GP
GE
```
**Figure** 3**.** 13 **:** Cumulative frequency of success of LOGENPRO, CFG-GP and GE in
symbolic regression of the dot product~x·~y.

The most pronounced was the difference caused by the change of grammar
in LOGENPRO, which can be attributed to the difference in search space size,
given that ephemeral constants are of no use in this problem. As we could
expect based on our discussion of the grammar used by LOGENPRO, there is
no practical difference between the performance of LOGENPRO with a logic
grammar and that of CFG-GP with a context-free grammar.
The 0.53 success rate (53 out of 100) obtained from LOGENPRO with the
original grammar is lower than the 0.8 rate reported by Wong and Leung ( 2000 ,
sec. 6. 1. 2 , approximate reading of a plot). Regardless of the number of trials
performed (not mentioned by Wong and Leung, 2000 ), such a difference would
be statistically significant. I have not been able to replicate this success rate by
varying operator probabilities, maximum tree heights or selection parameters,
with either grammar. That said, even with the more modest success rate their
comparison with tree-based GP would still hold (success rate of GP with the


same population size was reported below 0.1).
As LOGENPRO does not provide any statistics about the generated trees,
we can compare these only for CFG-GP and GE, see Figure 3. 14. In contrast
with the previous experiments, in which CFG-GP performed better, GE has
now reached exactly the same level of success, and in a large part of the
run, there was no significant difference in success between the two. Despite
these similar rates of success, we can still observe the differences in the tree
characteristics: CFG-GP has soon reached both higher average values and CV
of height and bushiness and maintained them throughout the run. As we have
noted in the beginning of Section 3. 6 , correct solutions occur relatively densely
among random trees of height 5, so the lower tree height close to 5 maintained
by the operators of grammatical evolution (see Figure 3. 14 ) are a very likely
explanation for the improved success of GE.

#### 3 6 3 Conclusion

We have shown that the use of logic grammars to describe types demonstrated
by Wong and Leung ( 2000 ) does not require any of their distinctive features
(unification and incorporation of logic goals): the same can be done using a
context-free grammar in either CFG-GP or GE. Both methods have performed
on a par with LOGENPRO (with a slightly higher success rate) using an equi-
valent context-free grammar and other parameters.
One could conceive of a type system that could be properly enforced using a
logic grammar but not a context-free grammar. It is not obvious, however, that
LOGENPRO would perform well if such a type system was necessary. It would
depend on how efficiently the genetic operators would be able to fulfil their
roles in the evolutionary algorithm under the more complex type constraints.


```
Generations
```
```
Average tree height
```
```
0 20 40 60 80 100
```
```
4.0
```
```
4.5
```
```
5.0
```
```
5.5
```
```
6.0
```
```
6.5
```
```
7.0
```
```
CFG−GP height
GE height
```
```
Generations
```
```
Coeff. of var. of tree height
```
```
0 20 40 60 80 100
```
```
0.0
```
```
0.1
```
```
0.2
```
```
0.3
```
```
0.4 CFG−GP height CV
GE height CV
```
```
Generations
```
```
Average tree bushiness
```
```
0 20 40 60 80 100
```
```
1.0
```
```
1.2
```
```
1.4
```
```
1.6
```
```
1.8
```
```
CFG−GP bushiness
GE bushiness
```
```
Generations
```
```
Coeff. of var. of tree bushiness
```
```
0 20 40 60 80 100
```
```
0.000
```
```
0.005
```
```
0.010
```
```
0.015
```
```
0.020 CFG−GP bush. CV
GE bushiness CV
```
```
Generations
```
```
Average of # choice nodes
```
```
0 20 40 60 80 100
```
```
0
```
```
5
```
```
10
```
```
15
```
```
20
```
```
CFG−GP # choice nodes
GE # choice nodes
```
```
Generations
```
```
Variance of # choice nodes
```
```
0 20 40 60 80 100
```
```
0
```
```
5
```
```
10
```
```
15
```
```
20
```
```
25
```
(^30) CFG−GP # ch. nodes var.
GE # ch. nodes var.
**Figure** 3**.** 14 **:** Tree characteristics of CFG-GP and GE in symbolic regression of the dot
product. The plots show averages, taken over 100 runs, of the following
population statistics: averages and coefficients of variance of tree height
and tree bushiness, average and variance of the number of choice nodes.


### 3 7 Symbolic regression with ADFs

In this section, we will try to replicate another experiment presented by Wong
and Leung ( 2000 ) using LOGENPRO, and compare the results with the other
two grammar-based methods. It is again an instance of symbolic regression
with multiple types but the target function is more complex. It has three
vector variables~x,~y,~z, and can be expressed as~x·~y+~y·~z, where·is the
dot product. To find an equivalent expression in a random search using the
“grow” initialisation method with the grammar that we are going to use is an
order of magnitude more difficult than it was to find the dot product. Wong
and Leung ( 2000 ) designed the experiment to show that LOGENPRO can use
ADFs similarly as traditional genetic programming (see Section 2. 3 ).
To emulate ADFs, the grammar employed by Wong and Leung ( 2000 ) (List-
ing 3. 6 ) uses two sets of nonterminals: those for the main program (s-expr(···),
term(···)) and those for the body of the ADF (s-expr2(···),term2(···)). This
way the terminalsX,Y,Z,ADF0, andarg0,arg1can be restricted to occur only
in the main subtree and the ADF subtree, respectively, and only subtrees of the
same kind can be combined via crossover, which is equivalent to how ADFs
are handled in tree-based GP. The grammar does not depend on any specific
features of logic grammars (see explanation for a similar grammar in the previ-
ous experiment), so an equivalent CFG can be constructed (Listing 3. 7 ) for use
with GE and CFG-GP.

#### 3 7 1 Experimental setups

As in Section 3. 6 , the setup of LOGENPRO replicates the one used by Wong
and Leung ( 2000 ), and the missing parameters are chosen based on the config-
uration file obtained from Dr Wong. For the same reasons as before I also use
the 0.05 mutation rate. Table 3. 6 contains the full details of parameters for LO-
GENPRO, CFG-GP, and GE. The parameters are unchanged from the previous
experiment without ADFs except for the number of generations lowered to 50
and a smaller number of terminals. Note that while the problem is obviously
more difficult than the previous one, the search space has been restricted by
removing several terminals from the grammar.

#### 3 7 2 Results

All three methods have produced similar results in terms of success (see Fig-
ure 3. 15 ): 51, 45, and 56 runs were successful out of 100 in the case of LO-
GENPRO, CFG-GP, and GE, respectively. There were significant differences in
success between all of them in all generations of the second half of the run. In
this experiment, grammatical evolution achieved the best performance of the
three methods.
As in the previous experiment without ADFs, the success rate I have been
able to achieve with LOGENPRO was significantly lower than that reported


```
start -> [ (progn (defun ADF0(arg0 arg1) ],
s-expr2(number), [ ) ],
s-expr(number), [ ) ].
s-expr([list,number,?n]) -> [ (mapcar(function ], op2, [ ) ],
s-expr([list,number,?n]),
s-expr([list,number,?n]), [ ) ].
s-expr([list,number,?n]) -> term([list,number,?n]).
s-expr(number) -> [ (apply (function ], op2, [ ) ],
s-expr2([list,number,?n]), [ ) ].
s-expr(number) -> [ ( ], op2, s-expr(number),
s-expr(number), [ ) ].
s-expr(number) -> [ (ADF0 ],
s-expr([list,number,?n]),
s-expr([list,number,?n]), [ ) ].
term([list,number,n]) -> [ X ].
term([list,number,n]) -> [ Y ].
term([list,number,n]) -> [ Z ].
s-expr2([list,number,?n]) -> [ (mapcar(function ], op2, [ ) ],
s-expr2([list,number,?n]),
s-expr2([list,number,?n]), [ ) ].
s-expr2([list,number,?n]) -> term2([list,number,?n]).
s-expr2(number) -> [ (apply(function ], op2, [ ) ],
s-expr2([list,number,?n]), [ ) ].
s-expr2(number) -> [ ( ], op2, s-expr2(number),
s-expr2(number), [ ) ].
term2([list,number,n]) -> [ arg0 ].
term2([list,number,n]) -> [ arg1 ].
op2 -> [ + ].
op2 -> [ - ].
op2 -> [ * ].
```
**Listing** 3**.** 6 **:** Logic grammar (LOGENPRO) for symbolic regression of the expression
~x·~y+~y·~zin Lisp (from Wong and Leung, 2000 ).

by Wong and Leung ( 2000 , sec. 6. 1. 3 ). A rate higher than 0.65 can be read from
their plot but even the lower one we have reached would be high enough to
outperform GP without any constraints, for which they report a success rate
lower than 0.1.
We can observe the same relationship between tree characteristics of GE
and CFG-GP as in the previous experiment.
Wong and Leung ( 2000 ) do neither offer any details about the solutions
found with their setup, nor do they compare the results with a LOGENPRO
setup without ADFs. It is however rather conspicuous that the target expres-
sion~x·~y+~y·~zcan be rewritten more succinctly as~y·(~x+~z), where the dot
product appears only once. One would not expect a strong selection pressure


```
<start> ::= function adf0(arg0,arg1) return <num'> end
return <num>
<num> ::= <op2>v(<vec>)
| <op2>(<num>,<num>)
| adf0(<vec>,<vec>)
<vec> ::= v<op2>(<vec>,<vec>)
| <vec-term>
<num'> ::= <op2>v(<vec'>)
| <op2>(<num'>,<num'>)
<vec'> ::= v<op2>(<vec'>,<vec'>)
| <vec'-term>
<vec-term> ::= x | y | z
<vec'-term> ::= arg0 | arg1
<op2> ::= add | sub | mul
```
**Listing** 3**.** 7 **:** Context-free grammar in BNF for symbolic regression of the expression
~x·~y+~y·~zin Lua. Vector-to-scalar operator names are suffixed byv. Vector
operator names and prefixed byv. Scalar operators are named without
suffix or prefix:add,sub,mul.

to evolve a building block that needs to be used a single time. Indeed, out of
the 51 solutions found by the successful runs of LOGENPRO I have performed,
24 use ADFs. 21 out of the 24 ADFs perform just addition on the components
of one of its arguments. Two ADFs out of the remaining three do compute the
dot product, which is then used in an expression equal to~y·(~x+~z), and the
last remaining ADF computes a more complex expressionarg0~ ·(arg1~ −arg0~ ),
which is then used in a similar way. The experiments with GE and CFG-GP
had the same outcome: ADFs are not used as intended in any of the 100 runs.
This suggest that the experiment devised by Wong and Leung ( 2000 ) is
not suitable for demonstrating the ability to use ADFs. I have performed it
again with CFG-GP and GE but with ADF-related structures removed from the
grammar (see Figure 3. 17 ). The removal of ADFs has improved performance
significantly for CFG-GP (from 45 to 55 successful runs out of 100), and insigni-
ficantly for GE (from 56 to 57 successful runs). This reflects that GE operators
are grammar-agnostic as a result of the genotype-phenotype distinction, and
so crossover can occur between a function-defining subtree and a main subtree
regardless of the grammar. Without ADFs, CFG-GP and GE achieved similar
performance: there is a significant difference in generations 1 through 16 and
then only in the last 7 generations.

#### 3 7 3 Conclusion

We have discovered that the problem specified by Wong and Leung ( 2000 )
is not suitable for evaluating performance grammar-based GP methods with
automatically defined functions as it can be solved more simply and efficiently


```
Generations
```
```
Cumulative success rate in 100 runs
```
```
0 10 20 30 40 50
```
```
0
```
```
20
```
```
40
```
```
60
```
```
80
```
```
100
```
```
LOGENPRO
CFG−GP
GE
```
**Figure** 3**.** 15 **:** Cumulative frequency of success of LOGENPRO, CFG-GP, and GE in
symbolic regression of the expression~x·~y+~y·~z.

without them. Nevertheless we have made three valuable observations. First,
GE and CFG-GP have performed on a par with LOGENPRO. Secondly, as in
the previous experiments, CFG-GP has in the long run produced more varied
tree shapes, as characterised by bushiness and height. Thirdly, GE has suffered
less from the unnecessary addition of ADF-related structures to the grammar
because, in contrast with CFG-GP and LOGENPRO, the syntactical constraints
do not constrain the operators, which act only on the genotype. In order to
enforce similar behaviour in grammatical evolution, we would have to use
separately mapped chromosomes for each function-defining branch and for
each value-returning branch and allow crossover only between chromosomes
of the same type.


```
Objective: Find a real-valued function of three three-component
vector variables~x,~y,~zthat yields~x·~y+~y·~z, where·is the
dot product.
Terminal operands: ~x,~y,~z. Within ADF:arg0~ ,arg1~.
Terminal operators: Addition, subtraction, and multiplication in three
variants: scalar, vector (by components), vector-to-scalar
(applied to components of a single vector). Out of ADF:
the vector-to-scalar functionADF0of two arguments.
Grammar: See Listing 3. 6 (LOGENPRO), Listing 3. 7 (GE and
CFG-GP).
Fitness cases: 10 vectors with components from{ 0 , 1 , 2 , 3 }randomly
generated for each run.
Raw fitness: The sum of absolute errors taken over the 10 fitness cases.
Scaled fitness: Same as raw fitness.
Algorithm: Simple, generations: 50, population: 100.
Selection: Tournament, size: 7.
LOGENPRO init.: “Grow” method without uniqueness (Wong and Leung,
2000 ), maximum height: 6.
CFG-GP initialisation: “Grow” method without uniqueness, maximum height:
6.
GE initialisation: “Grow” method without uniqueness, maximum height:
6.
LOGENPRO operators: Crossover (Wong and Leung, 2000 ), probability: 0.9.
Mutation (Wong and Leung, 2000 ), probability: 0.05.
Crossover and mutation are mutually exclusive.
CFG-GP operators: Crossover (Whigham, 1995 ), probability: 0.9.
Mutation (Whigham, 1995 ), probability: 0.05.
GE operators: Fixed-length one-point crossover, probability: 0.9.
Bit-level mutation, probability: 0.001.
Common parameters: Maximum tree height: 9.
GE parameters: Maximum wraps: 3. Codon size: 8.
Success predicate: Raw fitness lower than 0.00001 (to allow for
floating-point round-offerror.)
```
**Table** 3**.** 6 **:** Symbolic regression of the expression~x·~y+~y·~z, parameters for LOGENPRO,
CFG-GP, and GE. Note: Although the LOGENPRO implementation soft-
ware tree height by nodes instead of edges, the table follows the established
convention (see page 5 ).


```
Generations
```
```
Average tree height
```
```
0 10 20 30 40 50
```
```
5.0
```
```
5.5
```
```
6.0
```
```
6.5
```
```
7.0
```
```
7.5
```
```
8.0
```
```
CFG−GP height
GE height
```
```
Generations
```
```
Coeff. of var. of tree height
```
```
0 10 20 30 40 50
```
```
0.00
```
```
0.10
```
```
0.20
```
```
0.30 CFG−GP height CV
GE height CV
```
```
Generations
```
```
Average tree bushiness
```
```
0 10 20 30 40 50
```
```
1.70
```
```
1.75
```
```
1.80
```
```
1.85
```
```
1.90
```
```
CFG−GP bushiness
GE bushiness
```
```
Generations
```
```
Coeff. of var. of tree bushiness
```
```
0 10 20 30 40 50
```
```
0.000
```
```
0.002
```
```
0.004
```
```
0.006 CFG−GP bush. CV
GE bushiness CV
```
```
Generations
```
```
Average of # choice nodes
```
```
0 10 20 30 40 50
```
```
20
```
```
25
```
```
30
```
```
35
```
```
40
```
```
45
```
```
50
```
```
CFG−GP # choice nodes
GE # choice nodes
```
```
Generations
```
```
Variance of # choice nodes
```
```
0 10 20 30 40 50
```
```
0
```
```
50
```
```
100
```
```
150
```
(^200) CFG−GP # ch. nodes var.
GE # ch. nodes var.
**Figure** 3**.** 16 **:** Tree characteristics of CFG-GP and GE in symbolic regression of the ex-
pression~x·~y+~y·~z. The plots show averages, taken over 100 runs, of
the following population statistics: averages and coefficients of variance
of tree height and tree bushiness, average and variance of the number of
choice nodes.


```
Generations
```
```
Cumulative success rate in 100 runs
```
```
0 10 20 30 40 50
```
```
0
```
```
20
```
```
40
```
```
60
```
```
80
```
```
100
```
```
CFG−GP with ADFs
GE with ADFs
CFG−GP without ADFs
GE without ADFs
```
**Figure** 3**.** 17 **:** Cumulative frequency of success of CFG-GP and GE without ADFs in
symbolic regression of the expression~x·~y+~y·~z.


### 3 8 Boolean parity functions with ADFs

In the previous experiment we have shown that the symbolic regression prob-
lem used by Wong and Leung ( 2000 ) to demonstrate the use of ADFs in LO-
GENPRO is not fit for the purpose. Additionally, the problem involves multiple
types, so a grammar-based approach with a suitable grammar would always
have an advantage over tree-based GP without type constraints, regardless
of how it handled ADFs. Specifying ADFs using a context-free grammar in
CFG-GP (or LOGENPRO) will have the same effect on operators as the con-
straints Koza ( 1992 ) has used with ADFs in tree-based GP. The operators used
in grammatical evolution, however, operate on the genotype rather than on the
grammatically constrained phenotype. In the previous experiment, we have
observed that this is helpful, if the ADF-related structures are unnecessary, but
we have yet to find out how well GE can perform when ADFs are actually
needed.
To be able to compare performance of GE both with CFG-GP and tree-based
GP we will use the problem of symbolic regression of the Boolean parity func-
tions (see Section 2. 4 ). We will essentially try to replicate the results achieved
by Koza ( 1992 ) with GP in a series of experiments with these functions.

#### 3 8 1 Experimental setups

We will perform a series of the following experiments:

- symbolic regression of the even-3-parity function without ADFs,
- symbolic regression of the even-4-parity function with and without ADFs,
- symbolic regression of the even-5-parity function with and without ADFs,
- symbolic regression of the even-6-parity function with ADFs.

There will ben−2 function-defining branches for ann-ary parity function
with ADFs. The two ADFs for the even-4-parity function will have 3 and
4 arguments. The three ADFs for the even-5-parity function will have 2, 3 and
4 arguments. The four ADFs for the even-6-parity function will have 2, 3, 4,
and 5 arguments.

We will use the same parameters for CFG-GP as Koza ( 1992 , sec. 20. 1 ) did for
GP. In order to make the setup as close as possible to the original setup for GP,
we will use grammars that mimic the behaviour of tree-based GP by having
one nonterminal for each GP node type, and a GP-like crossover operator,
which operates on inner tree nodes with 90% probability. Koza did not use
a mutation operator, but this variant of crossover effectively emulates a point
mutation with 10% probability. Listing 3. 8 and Listing 3. 9 provide examples of
the grammars we will use, the other grammars follow the same pattern. For
GE, we will use its usual operators with conservatively chosen rates, the other
parameters remain unchanged. The full setup for both methods is specified in
Table 3. 7.


```
Objective: Find an expression for the even parity function of 3, 4, 5,
and 6 arguments using ADFs as described in the text.
Terminal operands: A subset ofd0,d1,... ,d5(variables),a0,a1,... ,d4(ADF
parameters) based on arity and whether ADFs are used.
Terminal operators: Boolean connectivesand,or,nand,nor; a subset ofadf0,
adf1,adf2based on arity and whether ADFs are used.
Grammar: See Listing 3. 8 and Listing 3. 9 for examples.
Fitness cases: The set of all possible arguments.
Raw fitness: The number of fitness cases for which the value returned
by the expression is different from the correct value of the
even parity function.
Scaled fitness: Koza’s adjusted fitness, and greedy over-selection with
the parameters used by Koza ( 1992 ).
Algorithm: Simple, generations: 51, population: 4000.
Selection: Roulette-wheel.
CFG-GP initialisation: Ramped half-and half (unique trees), maximum height:
8 /7 (with/without ADFs).
GE initialisation: Ramped half-and half (unique trees), maximum height:
8 /7 (with/without ADFs).
CFG-GP operators: Only GP-like crossover (Koza, 1992 ), probability: 0.9.
GE operators: Fixed-length one-point crossover, probability: 0. 9
Bit-level mutation, probability: 0.002.
Common parameters: Maximum tree height: 18/19 (with/without ADFs).
GE parameters: Maximum wraps: 3. Codon size: 8.
Success predicate: Raw fitness equal to 0.
```
**Table** 3**.** 7 **:** Symbolic regression of the Boolean parity function, CFG-GP, and GE. Note:
the maximum initialisation tree height and the overall maximum tree height
correspond to GP tree heights 6 and 17, respectively, used by Koza ( 1992 ).

#### 3 8 2 Results

Figure 3. 18 shows plots of cumulative frequency of success for all experiments.
In Table 3. 8 we compare the final success rates with those reported for tree-based
GP by Koza ( 1992 ). Koza performed tens of runs in all experiments except for
even-5-parity with ADFs (7 runs) and even-6-parity with ADFs (unreported).
Both CFG-GP and GE perform surprisingly well without ADFs compared to
the results reported by Koza ( 1992 ) for GP: for instance in the even-4-parity,
CFG-GP has more than twice the success rate of GP, and even the even-5-parity
without ADFs intimidates neither CFG-GP nor GE.
We are, however, more interested in the performance with ADFs. Context-
free grammar genetic programming performs only slightly worse than tree-
based genetic programming in the even-4-parity problem (96% vs. 99% suc-
cess). For even-5-parity, Koza ( 1992 ) reports results from mere 7 runs, so I will
avoid drawing any conclusion from the comparison: incidentally the first 7
runs I have performed with CFG-GP were also successful.) CFG-GP performs
relatively well even in the even-6-parity problem (Koza does not report any


```
<start> ::= return <expr>
<expr> ::= (<expr> and <expr>) | (<expr> or <expr>)
| not (<expr> and <expr>) | not (<expr> or <expr>)
| d0 | d1 | d2
```
**Listing** 3**.** 8 **:** Context-free grammar in BNF for symbolic regression of a ternary Boolean
function in Lua. The connectives nand and nor are expressed using the
three basic connectives not, or, and.

```
<start> ::= function adf0(a0,a1) return <expr0> end
function adf1(a0,a1,a2) return <expr1> end
return <expr>
<expr> ::= (<expr> and <expr>) | (<expr> or <expr>)
| not (<expr> and <expr>) | not (<expr> or <expr>)
| adf0(<expr>,<expr>) | adf1(<expr>,<expr>,<expr>)
| d0 | d1 | d2 | d3
<expr0> ::= (<expr0> and <expr0>) | (<expr0> or <expr0>)
| not (<expr0> and <expr0>)
| not (<expr0> or <expr0>) | a0 | a1
<expr1> ::= (<expr1> and <expr1>) | (<expr1> or <expr1>)
| not (<expr1> and <expr1>)
| not (<expr1> or <expr1>) | a0 | a1 | a2
```
**Listing** 3**.** 9 **:** Context-free grammar in BNF for symbolic regression of a quaternary
Boolean function using ADFs in Lua.

precise results). Overall, the results of CFG-GP with ADFs are similar to those
of tree-based GP as far as we can judge from the results provided by Koza
( 1992 ).

Grammatical evolution performs substantially worse with ADFs than CFG-
GP. While it reached a significantly lower success rate also without ADFs, the
difference between CFG-GP and GE was not that marked. Clearly, ADF still
improve the performance of GE in this problem, but not as effectively as in the
case of CFG-GP and tree-based GP. This is likely a result of the different effect
grammar-defined ADFs have on the operators of CFG-GP and GE, as we have
already noted.

Figure 3. 19 shows plots of tree characteristics in the even-5-parity with
ADFs experiment. This time, GE produces trees of higher and more varied tree
bushiness and also more varied height. This is not very surprising because we
have set up CFG-GP to use the GP-like crossover operator and no mutation,
which is normally responsible for generation of new subtrees. Nevertheless,
CFG-GP creates much larger trees thanks to a steadily growing tree height,
while the operators of GE do not allow higher and larger trees to evolve. (This
also applies to tree characteristics produced by the two methods in the other
experiments. The plots are omitted for brevity.)


```
Method GP CFG-GP GE
ADFs %! %! %!
even-3-parity 100 % – 100 % – 93 % –
even-4-parity 45 % 99 % 93 % 96 % 25 % 77 %
even-5-parity †0 % ‡7 of 7 7 % 72 % 8 % 37 %
even-6-parity – *> 0 – 44 % – 22 %
†: no success in 20 runs.‡: 100% success but only in 7 runs.
*: a solution can be found within 20 runs.
```
**Table** 3**.** 8 **:** Comparison of success rates of tree-based GP, CFG-GP, and GE in the even
parity problems.

#### 3 8 3 Conclusion

In the series of experiments with symbolic regression of the Boolean even par-
ity functions using ADFs, CFG-GP has achieved success rates comparable with
those reported by Koza ( 1992 ) for tree-based GP. The success rates of GE were
substantially lower, but nevertheless ADFs improved its performance. We can
conclude that the grammar-based ADFs we have used are also suitable for
grammatical evolution, but if we know beforehand that ADFs will be needed
for some problem, tree-based GP or CFG-GP will likely provide a superior
performance. This reveals what might be a general problem with GE: if indi-
viduals consist of subtrees with terminals of mutually incompatible types, we
can expect the crossover operator to perform poorly in GE.


```
Generations / even−3−parity
```
```
Cumulative success rate in 100 runs
0 10 20 30 40 50
```
```
0
```
```
20
```
```
40
```
```
60
```
```
80
```
```
100
```
```
CFG−GP
GE
```
```
Generations / even−4−parity
```
```
Cumulative success rate in 100 runs
0 10 20 30 40 50
```
```
0
```
```
20
```
```
40
```
```
60
```
```
80
```
```
100
```
```
CFG−GP
GE
CFG−GP with ADFs
GE with ADFs
```
```
Generations / even−5−parity
```
```
Cumulative success rate in 100 runs
0 10 20 30 40 50
```
```
0
```
```
20
```
```
40
```
```
60
```
```
80
```
(^100) CFG−GP
GE
CFG−GP with ADFs
GE with ADFs
Generations / even−6−parity
Cumulative success rate in 100 runs
0 10 20 30 40 50
0
20
40
60
80
(^100) CFG−GP with ADFs
GE with ADFs
**Figure** 3**.** 18 **:** Cumulative frequency of success of CFG-GP and GE with and without
ADFs in symbolic regression of the Boolean even parity functions.


```
Generations
```
```
Average tree height
```
```
0 10 20 30 40 50
```
```
6
```
```
8
```
```
10
```
```
12
```
```
14
```
```
CFG−GP height
GE height
```
```
Generations
```
```
Coeff. of var. of tree height
```
```
0 10 20 30 40 50
```
```
0.0
```
```
0.2
```
```
0.4
```
```
0.6
```
```
0.8
```
```
1.0 CFG−GP height CV
GE height CV
```
```
Generations
```
```
Average tree bushiness
```
```
0 10 20 30 40 50
```
```
2.0
```
```
2.1
```
```
2.2
```
```
2.3
```
```
2.4
```
```
2.5
```
```
CFG−GP bushiness
GE bushiness
```
```
Generations
```
```
Coeff. of var. of tree bushiness
```
```
0 10 20 30 40 50
```
```
0.0000
```
```
0.0010
```
```
0.0020 CFG−GP bushiness CV
GE bushiness CV
```
```
Generations
```
```
Average of # choice nodes
```
```
0 10 20 30 40 50
```
```
0
```
```
100
```
```
300
```
```
500
```
```
CFG−GP # choice nodes
GE # choice nodes
```
```
Generations
```
```
Variance of # choice nodes
```
```
0 10 20 30 40 50
```
```
0
```
```
10000
```
```
30000
```
```
50000 CFG−GP # ch. nodes var.
GE # ch. nodes var.
```
**Figure** 3**.** 19 **:** Tree characteristics of CFG-GP and GE with ADFs in symbolic regression
of the Boolean even-5-parity function. The plots show averages, taken
over 100 runs, of the following population statistics: averages and coeffi-
cients of variance of tree height and tree bushiness, average and variance
of the number of choice nodes.


### 3 9 Exam timetabling hyper-heuristics

Exam timetabling is a real-world problem of scheduling exams for university
courses under given constraints. The constraints may vary according to institu-
tion but the most usual hard constraint is that no student can attend two exams
at the same time, and the most usual soft constrain is that exams scheduled
very close to each other for some students should be avoided. Most literat-
ure uses problems that can be scheduled relatively easily to satisfy the hard
constraint, and the focus is on optimising the soft one. Various methods have
been applied to exam timetabling and a widely used set of problem data has
been established by Carter et al. ( 1996 ). The problems are based on real data
from thirteen universities and are often referred to as the University of Toronto
benchmark data.
Recently, Bader-El-Den et al. ( 2009 ) have applied a grammar-based GP
hyper-heuristic approach (see Section 2. 5 ) to timetabling and have evaluated
the method on the above mentioned data sets. As follows from using a hyper-
heuristic approach, they compose several simple hand-crafted heuristics using
conditional branching, to create a new heuristic. The grammar-based GP part
of their method (called GPHH, Grammar-based Genetic Programming Hyper-
Heuristic) claims to be “a new, hybrid way of using grammars in GP” different
from both Koza’s GP and Whigham’s CFG-GP: the method operates according
to the grammar directly on GP trees instead of derivation trees in order to
increase performance. Other than this difference, which could be considered
an implementation detail, the method based on the “grow” initialisation and
the two familiar operators seems practically identical to CFG-GP.
Bader-El-Den et al. ( 2009 ) have evaluated their method on ten problems
from the Toronto data set and their results were comparable with several exist-
ing algorithms. In the previous sections, we have applied GE and CFG-GP to
several artificial problems that are either commonly used in the realm of genetic
programming or highlight the use of grammar for ensuring type constraints.
These artificial problems were also well-suited for comparing the characterist-
ics of GE and CFG-GP. Exam timetabling is an opportunity to evaluate the
performance of grammar-based GP methods on a real-world problem to which
many methods other than GP have been applied. Another interesting differ-
ence from the previous experiments is that there are no known target solutions,
instead there are many correct solutions that differ in how well they satisfy the
soft constraints.
We will try to replicate the published results with CFG-GP, compare them
with a similar setup of grammatical evolution, and we will also briefly discuss
the performance in terms of computational time.
Let’s start by stating the exam timetabling problem formally using the same
notation as Bader-El-Den et al. ( 2009 ): LetEbe the set of exams,Sthe set of
students,Pthe set of time slots,R:S→P(E) a registration function (Pdenotes a
power set). The quadruple consisting ofE,S,P, andRis a timetabling problem.
A solution to the timetabling problem (E,S,P,R) is a functionO:E→P. The


solution is incorrect if∃s∈S,i,j∈E:i,j&{i,j}⊆R(s) &P(i)=P(j) (a student
is to attend two exams at the same time), and the solution is incomplete ifOis
a partial function.

The soft constraint can be expressed quantitatively for correct solutions as
a penaltyp:

```
p=
```
##### 1

##### S

##### N∑− 1

```
i= 1
```
##### ∑N

```
j=i+ 1
```
```
w(|pi−pj|)·ai j ( 1 )
```
whereSis the total number of students,Nis the total number of exams,ai jis
the number of students attending both examsiandj,piis the time slot to which
examiis assigned, andw(d) is defined as 2^5 −difd≤5 and as 0 otherwise. This
formula proposed by Carter et al. ( 1996 ) has since been used in the literature
to compare the quality of solutions.

```
We will use a fitness valuefbased on the penalty formula:
```
```
f=
```
##### (M∑− 1

```
i= 1
```
##### ∑M

```
j=i+ 1
```
```
w(|pi−pj|)·ai j
```
##### )

```
+α·(N−M) ( 2 )
```
whereMis the number of assigned exams. This is only a minor modification
of the fitness function used by Bader-El-Den et al. ( 2009 ): we leave out theS^1
factor as it does not serve any purpose when computing fitness. The difference
from the penalty function is that it allows for incomplete solutions. Bader-
El-Den et al. ( 2009 ) do not mention any value for theαconstant, but I have
found out that any value large enough to disqualify incomplete solutions in
tournaments with complete solutions works well. If we were to use a fitness-
proportionate selection, however, it might not be the case.

Algorithms for timetabling that use heuristics usually proceed by repeatedly
using the heuristic to select an exam and then its slot. It may happen that no
slot is available for a given exam (the partial assignment would be incorrect), in
which case the algorithm may stop, continue constructing an incomplete solu-
tion, or continue constructing an incorrect solution. This first phase may then
be followed by a repair or improvement phase. Bader-El-Den et al. ( 2009 ) make
a distinction between “constructive” algorithms without a repair or improve-
ment phase and “improvement” algorithms, and they compare their results
primarily with other constructive algorithms. They do not describe precisely
the algorithm they use, but it is conceivably the one that continues to construct
an incomplete solution even if some exams cannot be scheduled, as outlined
in this pseudocode:


Time-Tabling(E,S,P,R)

et-Up(E,S,P,R)
while E,∅
do e ←Exam-Heuristic(T,E)
s ←Slot-Heuristic(T,e)
if s,not-found
then TT-Assign(T,e,s)

ssigment(T)
```
We will therefore use this algorithm skeleton to evaluate our candidate
solutions. The procedures TT-Set-Upand TT-Assignimplement operations
with data structures that serve for computations performed by the heuristics
and maintain the incomplete assignment, which can be retrieved using TT-
Get-Assigment. The evaluated hyper-heuristic is performed by calls to Exam-
Heuristicand Slot-Heuristic. We will compose the hyper-heuristics from the
same components as Bader-El-Den et al. ( 2009 ):

- branching based on current assignment size: V-Small, Small, Mid, Large;
- probabilistic random branching: Pr- 20 , Pr- 40 , Pr- 50 , Pr- 70 , Pr- 90 ;
- the list of all unprocessed exams (Ein the pseudocode): All-Exams;
- the list of all time slots: All-Slots,
- hand-crafted heuristics for selecting exams that act as list filters and can
    thus be pipelined:
       **-** Max-Conflict(largest degree), selects exams with most conflicts,
       **-** Least-Slot(saturation degree), selects exams with the least number
          of available slots,
       **-** Max-Students(largest enrolment), selects exams with the largest
          number of registered students;
- analogously working hand-crafted time slot heuristics:
    **-** Least-Cost, selects slots that increases the penalty the least,
    **-** Least-Busy, selects slots with the least number of assigned exams,
    **-** Most-Busy, selects slots with the largest number of assigned exams,
    **-** Least-Blocking, selects slots to which the least other conflicting
       unscheduled exams could be assigned;
- procedures for selecting a single exam or a slot from a list (Random-Exam,
    First-Exam,Random-Slot, First-Slot).

The exam selection heuristics are inspired by the graph-colouring problem
(hence their alternative names) and were already proposed by Carter et al.


( 1996 ). The time selection heuristics seem to have been introduced by Bader-
El-Den et al. ( 2009 ) who give commonsense explanations for them. The basic
idea is that even if some of the heuristics would not be useful alone, they may
become useful in conjunction with each other or at particular stages of the
timetable construction (using assignment size conditions).
The basic heuristics and other components can be combined according to
the grammar shown Listing 3. 10. Note particularly that conditional branching
can occur anywhere, and so the pipelines of heuristics may form branches of
arbitrarily shaped trees.

```
<assign> ::= while <exam>~=0 do <slot> end
<exam> ::= randomExam(<eList>) | firstExam(<eList>)
<eList> ::= maxConflict(<eList>)
| leastSlot(<eList>)
| maxStudents(<eList>)
| allExams(t)
| (<cond> and <eList> or <eList>)
<slot> ::= randomSlot(<sList>) | firstSlot(<sList>)
<sList> ::= leastCost(<sList>)
| leastBusy(<sList>)
| mostBusy(<sList>)
| leastBlocking(<sList>)
| allSlots(t)
| (<cond> and <sList> or <sList>)
<cond> ::= <prob> | <size>
<size> ::= vSmall(t) | small(t) | mid(t) | large(t)
<prob> ::= pr20(t) | pr40(t) | pr50(t) | pr70(t) | pr90(t)
```
**Listing** 3**.** 10 **:** Context-free grammar in BNF for a timetabling hyper-heuristic in Lua.
The(··· and ··· or ···)construct is a Lua idiom for a functional if-then-
else condition with short-circuit (lazy) evaluation. The exam heuristic
functions are implemented so that they return 0 when there are no exams
left, the slot heuristic functions perform an exam-slot assignment if a slot
is found. All data structures and the currently selected exam and slot
are kept in thetdata structure. This is a Lua equivalent of the grammar
presented by Bader-El-Den et al. ( 2009 ).

#### 3 9 1 Experimental setup

In addition to the grammar equivalent to the one used by Bader-El-Den et al.
( 2009 ), we will also report results with an alternative grammar without random
selection of exams and slots and conditional branching for slots.
For CFG-GP, we will use the parameters specified by Bader-El-Den et al.
( 2009 ). The values of maximum initialisation height and maximum height are
missing in their article as well as the already discussedαparameter of the fitness


function. As we do not have any beforehand knowledge of suitable tree height
parameters, we will out of cautiousness use a low maximum initialisation tree
height, letting the initialisation procedure raise it as needed for generation of
unique individuals, and a high maximum tree height. There are only a few
minor differences from the setup used by Bader-El-Den et al. ( 2009 ):

- We do not use mutually exclusive operators.
- We will evaluate individuals that use random selection or probabilistic condi-
    tions three times with different random number generator states and assign
    them the mean fitness value of the three runs. We will also report only this
    mean value in the results. Bader-El-Den et al. ( 2009 ) ran “the best performing
    individuals [... ] for an extra 2 times”. It is not clear if this concerns only
    the best individuals in a generation and whether the mean or the minimum
    value is used.
- We will perform only runs with 50 generations and 50 individuals in the pop-
    ulation. In addition to this, Bader-El-Den et al. ( 2009 ) performed runs with
    “larger populations, with sizes ranging between 500 and 1000 individuals”,
    but they do not give precise value for each experiment.
We will, as usual, use a lower nominal mutation rate for GE to reach an effective
mutation rate similar to that of CFG-GP. The full list of parameters is provided
in Table 3. 9 .)


Objective: Find an exam and time slot selection heuristic for
Time-Tabling.
Terminal operands: All-Exams, All-Slots,
V-Small, Small, Mid, Large,
Pr- 20 , Pr- 40 , Pr- 50 , Pr- 70 , Pr- 90.
Terminal operators: Random-Exam*, First-Exam, Max-Conflict, Least-Slot,
Max-Students, Exam-If-Then-Else,
Random-Slot*, First-Slot, Least-Cost, Least-Busy,
Most-Busy, Least-Blocking, Slot-If-Then-Else*.
Alternatively, without starred items.
Grammar: See Listing 3. 10. Alternatively, without productions
containing starred terminals.
Fitness cases: One of the standard data setscar91,car92,ear83,hec92,
kfu93,lse91,sta83,tre92,uta92,yor83(Carter et al.,
1996 ). Candidates that use random selection or
probabilistic conditions are evaluated three times with
different random number generator states, and the mean
fitness value is computed. The RNG state for the first
evaluation is fixed for each run.
Raw fitness: Formula ( 2 ) whereα=100000.
Scaled fitness: Same as raw fitness.
Algorithm: Simple, generations: 50, population: 50.
Selection: Tournament, size: 5.
CFG-GP initialisation: “Grow” method (Whigham, 1995 ), maximum height: 4.
GE initialisation: “Grow” method (Whigham, 1995 ), maximum height: 4.
GE operators: Fixed-length one-point crossover, probability: 0.8.
Bit-level mutation, probability: 0.01.
CFG-GP operators: Crossover (Whigham, 1995 ), probability: 0.8.
Mutation (Whigham, 1995 ), probability: 0.1.
Common parameters: Maximum tree height: 20.
GE parameters: Maximum wraps: 3. Codon size: 8.
Success predicate: None.

```
Table 3. 9 : Exam timetabling parameters, CFG-GP and GE.
```

#### 3 9 2 Results

In Table 3. 10 we report the best results from ten runs for each method, grammar,
and data set, as Bader-El-Den et al. ( 2009 ) did, along with the results reported
by them. We use the same ten data sets.^4

```
car91 car92 ear83 hec92 kfu93 lse91 sta83 tre92 uta92 yor83
GPHH ‡ 5. 12 4. 46 ‡ 37. 10 ‡ 11. 78 ‡ 14. 72 ‡ 11. 11 ‡ 158. 70 † 8. 62 3. 47 † 40. 56
CFG-GP † 5. 11 ‡ 4 .44 38.78 12.67 15.21 11.93 159. 44 8. 78 3. 49 40. 73
GE 5. 15 ‡ 4 .44 39.95 12.20 14.86 12.04 159. 37 ‡ 8. 63 3. 51 ‡ 40. 60
CFG-GP* 5. 13 † 4 .37 37.29 12.15 14.74 11.75 160. 22 ‡ 8. 63 ‡ 3. 44 40. 94
GE* 5. 22 4 .51 39.68 12.27 15.23 11.96 160. 22 8. 79 ‡ 3. 44 41. 47
Best other Fuzzy Fuzzy Car. Car. Car. Car. Tabu Fuzzy TM Fuzzy
5. 20 4. 52 † 36. 4 † 10. 8 † 14. 0 † 10. 5 † 158. 19 8. 67 † 3. 04 40. 66
*: alternative grammar. Best other: best other constructive heuristic, as reported by
Bader-El-Den et al. ( 2009 ).†,‡: best and second best results. Car.: Carter et al. ( 1996 ).
Fuzzy: Asmuni et al. ( 2005 ). Tabu, TM (Tabu-Multi-stage): Burke et al. ( 2007 ).
```
**Table** 3**.** 10 **:** Penalties achieved by time tabling heuristics. Mean values are reported
for stochastic heuristics evolved with CFG-GP and GE.

CFG-GP with the original grammar has in two cases (car91,car92) outper-
formed GPHH, which is the more interesting as these are two of the four cases
in which Bader-El-Den et al. ( 2009 ) reported that GPHH already outperformed
the best other constructive method.

GE with the original grammar has performed similarly to CFG-GP (four
times better, five times worse). With thecar92 data set it has performed
equally with CFG-GP, also beating both GPHH and the best other constructive
method. Withcar92andtre92it has still performed better than the best other
method, but worse than GPHH.

Because the grammar used by Bader-El-Den et al. ( 2009 ) is quite rich and the
authors do not discuss their choices in much detail, I have experimented with
removing various features. Most of the changes, such as permitting conditional
branching only at the top level, or reducing the number of conditions, resulted
in inferior performance. Removal of random selection of exams and slots
and conditional branching for slots did, however, improve CFG-GP and GE
results for seven and four data sets, respectively. CFG-GP with the alternative
grammar outperformed all other methods incar92. Both CFG-GP and GE with
the alternative grammar have outperformed GPHH. What is interesting about
these results is that we have managed to remove a large part of randomness
from the heuristics without impacting the quality of solutions.

(^4) Bader-El-Den et al. ( 2009 ) refer to one of the Toronto data sets asuta93. Although they
give parameters forute92, it must beuta92, judging from the achieved penalty.


#### 3 9 3 Computational time

Bader-El-Den et al. ( 2009 ) report approximate run times “on an ordinary PC”,
and their article was published in 2009. I performed my experiments on a
consumer laptop manufactured in late 2008 (2.4 GHz Intel Core 2 Duo Apple
MacBook with 2 GB of RAM), so the times should be comparable. According
to Bader-El-Den et al. ( 2009 ), a run of GPHH took from 10 minutes to about 4
hours depending on the problem. With the same grammar and evaluating all
individuals with random elements three times, all runs took me from less than
a second to about 80 seconds. Even if they by mistake reported times for ten
runs instead of one run, the running times of CFG-GP and GE using AGE are
still an order of magnitude lower. Let’s suppose that they meant 10 runs and
compare the total running times for all experiments (10 runs for each data set)
in Table 3. 11.

```
GPHH CFG-GP GE CFG-GP* GE*
CPU time ≥12 hours 27 min 27 s 28 min 44 s 11 min 31 s 14 min 4 s
*: alternative grammar. Total CPU time (for both processor cores) is reported.
```
```
Table 3. 11 : Penalties achieved by time tabling heuristics.
```
Of course, the run times are only a matter of implementation, but the large
performance gap is worth noting because the authors of GPHH regard their
system as an improvement over CFG-GP specifically because it is more efficient
by not being implemented using derivation trees. The alternative grammar
has brought further substantial improvements because fewer individuals used
random elements and thus fewer evaluations were necessary.

#### 3 9 4 Conclusion

In this section we have compared CFG-GP and GE in terms of both quality
of results and computational time with GPHH, an existing grammar-based
GP systems with published results that are comparable to (and in some cases
better than) those of other constructive methods for timetabling. In the case
of CFG-GP, we could have expected to achieve similar results because GPHH
is essentially based on the same method. In the case of GE, however, the
results are one of the scarce examples of GE being compared with previously
published results of methods that have performed well in some application.
It is a pleasant discovery that GE has performed on a par with CFG-GP in
evolving timetabling heuristics. I have not been able to find such comparison
for any other problem field in the literature.
On two data sets CFG-GP has even slightly outperformed GPHH achieving
the best results among the known constructive heuristics. GE has managed to
do so on one data set. We have also shown that comparable results could be
achieved in a substantially shorter run time using an efficient implementation
and by adjusting the grammar. The shorter run times would be of critical


importance if the grammar-based GP methods were to be used in a more
complex setup to evolve improvement heuristics for timetabling and compete
with other improvement methods as Bader-El-Den et al. ( 2009 ) suggest for
further research.

### 3 10 Conclusion

We have compared performance of GE and CFG-GP in a variety of application
and discovered links between the differences in the produced tree shapes and
the relative success of the two methods. The two methods performed simil-
arly well a few times but in several applications the performance of GE was
substantially worse.
In Section 3. 4 we have found out that the more successful CFG-GP pro-
duces higher trees of more varied shapes, thus searching a larger space. In
Section 3. 5 we have made the same observation and verified it with different
initialisation techniques. In the long run, both methods tended to converge
to tree characteristics typical for them regardless of tree characteristics of the
initial populations. In the two experiments done in Section 3. 6 and Section 3. 7 ,
which compared performance with LOGENPRO, all three methods performed
similarly. Nonetheless, we could observe the same patterns in development
of tree characteristics for GE and CFG-GP. We have explained why the good
results of GE were likely caused by its tendency to preserve low tree height
throughout its run, the same property that would often have the opposite effect.
In Section 3. 8 , we have shown that both GE and CFG-GP can use ADFs suc-
cessfully but that the technique adapted from tree-based GP is not as effective
for GE. As we have not used the usual mutation operators, the patterns of tree
characteristics were different than in the earlier experiments but CFG-GP still
produced higher and larger trees than GE, and was more successful even in
the ADF-less versions of the experiments. GE has, however, found solutions of
similar quality to those of CFG-GP in the real-world application to timetabling
in Section 3. 9.
We have thus shown a consistent difference between the tree shapes pro-
duced by GE and CFG-GP. GE tends to produce shorter trees, usually also less
bushy and of less varied height and bushiness. The search of this smaller search
space tends to be explorative thanks to the disruptive operators used in GE,
and GE can provide slightly but significantly better results in some cases. In
other cases, though, this seems to hamper performance. Additionally, the fact
that the operators used in GE are grammar-agnostic clearly worsens perform-
ance with ADFs, and it would likely have the same effect with any grammar
specifying subtrees with terminals of mutually incompatible types. We have
not found GE to provide a substantial advantage in any of the problems.
In Section 3. 9 we have also compared computational times needed by GE,
CFG-GP, and another implementation of a CFG-GP-like algorithm to evolve
timetabling heuristics. The efficient implementation of CFG-GP and GE I have


developed was at least an order of magnitude faster than the one used by
Bader-El-Den et al. ( 2009 ). CFG-GP has performed slightly better than GE in
terms of CPU time. This is a consequence of what individuals evolved during
the run, and it demonstrates that the simpler operators of GE do not imply
shorter overall run times.


## Chapter 4

## Implementation Notes

All experiments with GE and CFG-GP in Chapter 3 were done using an open-
source framework called AGE (Algorithms for Grammar-based Evolution). I
have created the initial version of the framework as an implementation of GE
and general evolutionary algorithm elements as part of my bachelor thesis with
the following goals:

- a clean, comprehensive implementation of standard algorithms,
- modularity,
- adequate documentation,
- versatile output,
- reproducible results,
- acceptable performance.
    Thanks to the modular approach it was easy to extend the framework with
implementation of CFG-GP and several new features. Although the framework
aimed only at an “acceptable performance” in terms of computational time,
it outperformed GEVA (an implementation of GE maintained at NCRA at
University College Dublin) by an order of magnitude in benchmarks done as
part of my bachelor thesis (Nohejl, 2009 ). The comparison of the CFG-GP
implementation, which is now part of AGE, with published results from the
CFG-GP-based GPHH framework (Bader-El-Den et al., 2009 ) was similarly
favourable (Section 3. 9. 3 ).
    In this chapter, we will describe the implementation of CFG-GP used in
AGE (in Section 4. 1 ), provide information about the accompanying files and
about how to use them to replicate the results presented in the previous chapter
(Section 4. 2 ). User and developer documentation of the framework and tools,
written as part of my bachelor thesis, is available inside the software package.

### 4 1 Implementation of CFG-GP

The original version of AGE already featured an efficient implementation of
grammatical evolution. This implementation does not store derivation trees, in-
stead phenotype strings are created directly from the codon strings (genotype),
and when initialising individuals, genotype is generated directly. Thus deriva-


tion trees are used only implicitly without having to unnecessarily manipulate
tree data structures. Both genotype and phenotype is stored in continuous
arrays, and the implementation tries to prevent unnecessary allocations and
deallocations of storage.
CFG-GP as described by (Whigham, 1995 ) represents individuals as trees, so
that they can be manipulated by the tree-based operators. Tree data structures
are traditionally stored as separately allocated nodes that contain pointers to
their children. Such an approach is natural in most programming languages
and offers the most flexibility when manipulating trees. When both the nodes
and their trees are relatively small and the most frequent operations are sequen-
tial reads and writes of whole trees or large subtrees, a different representation
is more efficient. Let’s call it aserialised tree.
Nodes of a serialised tree are stored in depth-first left-to-right order, which
is the same as the leftmost derivation order in the case of a derivation tree.
This way nodes of each subtree are stored in a continuous sequence. Each
node must contain (implicitly or explicitly) the number of its children in order
to express structure of the tree, pointers to children can then be omitted and
relations between nodes can be determined solely from their relative positions
in the sequence. Size of each subtree of a serialised tree is optionally stored
in the node where the subtree is rooted. These sizes are not strictly necessary
to manipulate a serialised trees but they make it possible to replace a subtree
without having to retrieve numbers of children from all its nodes. An example
of a serialised tree is shown in Figure 4. 1.

```
A
```
```
B F
```
```
E
```
```
C D G
```
```
H
A B C D E F G H
8 4 1 2 1 2 1 1
```
**Figure** 4**.** 1 **:** A tree in the usual representation and its corresponding serialised tree with
subtree sizes in each node. Subtree boundaries are outlined.

One approach to representing CFG-GP derivation trees is therefore to rep-
resent them as serialised trees. Let’s consider what information would we
need to have about the nodes in order to implement the CFG-GP operators.
If we know which production is used in each nonterminal node, we can also
tell the number of its children and their labels (terminal and nonterminal sym-
bols). Mutation and crossover sites are always located on nonterminal nodes
(Whigham, 1995 ), thus terminal nodes do not need to be stored and we only
need to store information about which productions were used at nonterminal
nodes. Let’s number the productions for each nonterminal from zero, and call
these numberschoices. It may happen that some nonterminals have only one


production, offering a single choice (effectively no choice), in that case the node
does not need to be stored in the serialised representation.^1
A minimal representation of a derivation tree is therefore a serialised tree
of its choice nodes. Our definition of choice nodes (see page 28 ) coincides with
nonterminal nodes with more than one choice, and as we have noted, these
are precisely the nodes encoded by the codons used in grammatical evolution.
The order in which the nodes are stored is also the same. Thus a serialised
choice-node tree is a special case of a GE chromosome that represents the same
derivation. This correspondence is illustrated by Figure 4. 2.

```
CFG productions:
S → E (single choice)
E → ( E * E )(0) E → ( E ^ E ) (1) E → T (2)
T → 2 (0) T → c (1) T → m (2) T → 42 (3)
```
```
E
```
```
( E * E )
```
```
T
```
```
T
```
##### ( E ^ E )

```
T
```
```
Derivation tree
S
```
```
Choices in leftmost
derivation order
```
```
(2) (1)
```
```
(2) (2)
```
```
0 2 2 1 2 1 2 0
```
```
GE genotype examples
```
```
(0)
```
```
c 2
```
```
m
```
```
(2)
```
```
(1) (0)
```
```
EETEETET
0 2 2 1 2 1 2 0
wrapping:
0 2 2 1 2 1 2
codon-level degeneracy:
0 2 6 1 2 9 2 4
9 8 6 7 2 1 2 0
chromosome-level deg.:
0 2 2 1 2 1 2 0 4 2
0 2 2 1 2 1 2 0 3 1 4 1 5 9
```
```
Yield: (m * (c ^ 2))
```
**Figure** 4**.** 2 **:** Three representations of the derivation of the string(m * (c ^ 2))accord-
ing to a set of CFG productions: ( 1 ) derivation tree: nonterminal nodes,
at which crossover or mutation can occur in CFG-GP, are underlined; ( 2 )
choices in leftmost derivation order: basis for genotype in GE, and for the
implementation of CFG-GP in AGE; ( 3 ) GE genotype: codon values may
correspond to choices exactly, codons may be reused in a circular fashion
(wrapping), codons are interpreted modulo number of choices (codon-
level degeneracy), unused codons may be appended (chromosome-level
degeneracy).

The implementation of CFG-GP in AGE is based on serialised trees of choice
nodes. Individuals represented this way can share evaluation procedures, in-

(^1) If we were to follow (Whigham, 1995 ) literally, we would have to store all nonterminal
nodes as crossover or mutation can occur on any one of them. Crossover or mutation on a
node with single choice is, however, equivalent to crossover or mutation on its parent node
(except for a single-choice root node). Additionally, single-choice nodes can be considered a
degenerate cases, which bias operators towards their parents.


cluding the genotype-phenotype mapping, with GE. The tree-based initialisa-
tion procedures used for GE need only one modification: removal of genetic
code degeneracy (see Figure 4. 2 for comparison). Other general evolution-
ary algorithm elements such as selection methods can be shared regardless of
representation. Only the mutation and crossover operators need to be imple-
mented separately.
For the purposes of these operators, AGE generates other node information
such as the already mentioned subtree size. Because such data can be generated
quickly, it is relatively small, and its reuse is limited, it is actually more efficient
to re-generate it every time it is needed. Along with this data, statistics about
tree characteristics that we have used in Chapter 3 can be generated using
common procedures for both GE and CFG-GP.
The main advantages of this implementation is that CFG-GP trees are stored
in compact and easily manipulated data structures (arrays of choice numbers
and temporarily generated arrays of other node data). Note that in contrast to
traditional Lisp-based GP the derivation trees to which operators are applied
cannot share the representation with the parse trees used when evaluating indi-
viduals because arbitrary grammars are allowed. We cannot avoid generating
the full string representation of each individual (the phenotype in GE) which
is then parsed in order to be evaluated. Therefore the minimal representation
is advantageous.

### 4 2 Accompanying files

This thesis is accompanied by the following files on the enclosed medium:

- Adam-Nohejl-2011-Master-Thesis.pdf, a PDF version of this thesis,
- AGE-1.1.tar.gz, the AGE source package with documentation,
- Experiments.tar.gz, results from LOGENPRO and several scripts:
    **-** AGE-run.sh, a shell script to run experiments for this thesis,
    **-** AGE-stats.sh, a helper R script forstats.sh,
    **-** exstats.sh, a helper shell script forstats.sh,
    **-** plot.r, an R script to draw plots used in this thesis,
    **-** runner.sh, a helper shell script forAGE-run.sh,
    **-** stats.sh, a shell script to compute statistics for the plots.

The included version of the AGE framework has been extended mainly
by the CFG-GP algorithm elements as described above. The implementations
of all experimental problems used in this thesis are also part of the default
build. The software is portable: it can be built on a POSIX-compliant system
(tested on Mac OS X, NetBSD and Linux) with a decent standards-compliant


C/C++compiler (tested with GCC and Clang) and it does not have any external
dependencies (details in the included documentation). The whole package is in
the archive namedAGE-1.1.tar.gzand has a customary UNIX source package
structure (you can start by reading theREADMEandINSTALLtext files).
To replicate the experiments done in Chapter 3 , follow these steps:

```
1. Build and install AGE from the source package and make sure it is in your
search path (yourPATHvariable must include the directory where you
have installed theAGEexecutable). Suggested commands for installation
into your home directory:
```
```
% tar xzf path /AGE-1.1.tar.gz
% cd AGE-1.1; make INSTALL_PREFIX=~ install
% cd ..; export PATH=~/bin:"$PATH"
```
```
wherepathis path to the directory containing the fileAGE-1.1.tar.gz
(mount point of the accompanying disc).
```
```
2. Expand theExperiments.tar.gzarchive. The resultingExperiments
directory will contain the results obtained from LOGENPRO that we
have presented in Chapter 3. Suggested command:
```
```
% tar xzf path /Experiments.tar.gz
```
```
wherepathis path to the directory containing the fileExperiments.tar.gz.
```
```
3. Change your working directory to the newly created Experiments direct-
ory. Suggested command:
```
```
% cd Experiments
```
```
4. Run theAGE-run.shscript. Running all the experiments will likely take
a few hours to finish. Alternatively, you can open the file in a text editor
and extract only commands for the experiments you want to run or adjust
options. Suggested command:
```
```
% ./AGE-run.sh
```
```
Output from the experiments is in a documented format and results from
each run can be displayed using a XSLT style sheet (consult the AGE
documentation for details).
```
```
5. Optionally, to compute the necessary statistics and render the plots used
in the thesis, run thestats.shscript and then theplot.rscript. (The
R statistical package must be installed.) Suggested commands:
```
```
% ./stats.sh; ./plot.r
```

The generated plots will be saved in PDF files with the following name
patterns:

- plot-ad*.pdf: Symbolic regression with ADFs (Section 3. 7 ),
- plot-at*.pdf: Santa Fe ant trail (Section 3. 5 ),
- plot-bp*.pdf: Boolean parity functions with ADFs (Section 3. 8 ),
- plot-dp*.pdf: Dot product symbolic regression (Section 3. 6 ),
- plot-sr*.pdf: Simple symbolic regression (Section 3. 4 ).


## Conclusion

We have discussed the problems encountered by traditional genetic program-
ming that can be solved using grammars. We have reviewed three different
grammar-based GP methods, described their distinctive features, and tested
them in a number of benchmark application, including the real-world applic-
ation of finding heuristics for timetabling. We have implemented two of the
methods, grammatical evolution and context-free grammar genetic program-
ming, in a common framework, which allowed us to compare their perform-
ance in identical setups. In addition to comparing performance, we have fo-
cused on a statistical analysis of tree characteristics produced by each method,
which has provided insight into differences between GE and CFG-GP. As most
of the research in this field focuses either on novel applications or compares
results only with traditional genetic programming, we have been able to shed
more light on actual performance and comparative advantages of the grammar-
based methods.
In Chapter 1 we have introduced the basic concepts of genetic program-
ming and formal grammars, and we have pointed to the following problems
encountered by GP that grammars can solve: adaptability to different pro-
gramming languages and environments, the problem of closure and multiple
types, and the problem of declarative representation of knowledge. We have
presented three different approaches to applying grammars to GP: context-free
grammar genetic programming, LOGENPRO, and grammatical evolution. We
have described their distinctive features, and what they imply for performance
of these methods. Based on this survey, we have focused on the role of the
power of logic grammars in LOGENPRO, on the unclear effect of operators in
GE, and the problem of bias (whether wanted or not) encoded in a grammar.
In Chapter 2 we have described several problem classes and areas of applic-
ation, most of them admittedly artificial and of little practical value but useful
for comparing the performance of grammar-based GP methods. We have also
briefly presented hyper-heuristics, a field that offers possibilities of real-world
application to grammar-based genetic programming.
In Chapter 3 we have tested GE and CFG-GP in six different applications.
In two of them we have also compared the results with LOGENPRO, and
found no particular advantage in using logic grammars in the applications that
Wong and Leung ( 2000 ) used to demonstrate the abilities of LOGENPRO. In
one application we have compared the performance of CFG-GP and GE with
tree-based GP (Koza, 1992 , results reported by), demonstrated that ADFs can


be carried over to the grammar-based methods, particularly to CFG-GP, and
explained the inferior performance of GE. In the last application we have com-
pared performance both in terms of quality and speed with recently published
results of a grammar-based hyper-heuristic framework (GPHH, Bader-El-Den
et al., 2009 ) for timetabling. The comparison was very favourable: using CFG-
GP we have outperformed the published results in two problem instances, in
which GPHH was already reported to be the best constructive heuristic to date.
Additionally, our implementation is at least an order of magnitude faster. I
believe that such a large improvement in speed could open new possibilities
for practical applications of grammar-based GP methods.
Perhaps even more important output from the six experiments is that GE
does not provide a substantial advantage in any of them. In several experiments
it performs substantially worse, and we have been able to link this perform-
ance to the tree characteristics resulting from its operators, and the fact that
grammatical constraints in GE are enforced only by the genotype-phenotype
mapping, not by the operators. Additionally, we have demonstrated that the
simpler operators of GE do not transform in shorter overall run times. I see
this as the largest contribution of my thesis, as the effect of operators used in
GE is hard to analyse, and I have not been able to find such comparisons with
other grammar-based GP methods in the literature.
The thesis has fulfilled its goal of thoroughly comparing the grammar-based
GP methods, has showed that CFG-GP, the simplest of the three compared
methods, provides comparable or even better results than the other two, and
has demonstrated that CFG-GP can be implemented very efficiently. We have
several times touched the problem of embedding knowledge (bias) in gram-
mars. The choice of grammar obviously has a great effect on performance of
any grammar-based GP algorithm, and if at least part of the problem could be
automated by co-evolving grammars or transforming them into more suitable
forms, it would provide great advantage, especially when CFG-GP is applied to
more complex problems. This seems to be the most promising area for further
research.


## Bibliography

Hishammuddin Asmuni, Edmund Burke, Jonathan Garibaldi, and Barry Mc-
Collum. Fuzzy multiple heuristic orderings for examination timetabling. In
Edmund Burke and Michael Trick, editors,Practice and Theory of Automated
Timetabling V, volume 3616 ofLecture Notes in Computer Science, pages 334 –
353. Springer Berlin/Heidelberg, 2005.

Mohamed Bahy Bader-El-Den, Riccardo Poli, and Shaheen Fatima. Evolving
timetabling heuristics using a grammar-based genetic programming hyper-
heuristic framework.Memetic Computing, 1 ( 3 ): 205 – 219 , 2009.

Edmund K Burke, Barry Mccollum, Amnon Meisels, Sanja Petrovic, and Rong
Qu. A graph-based hyper-heuristic for educational timetabling problems.
European Journal of Operational Research, 176 : 177 – 192 , 2007.

Michael W. Carter, Gilbert Laporte, and Sau Yan Lee. Examination timetabling:
Algorithmic strategies and applications. J Oper Res Soc, 47 ( 3 ): 373 – 383 , 03
1996. URLhttp://dx.doi.org/10.1057/jors.1996.37.

William W. Cohen. Compiling prior knowledge into an explicit bias. InProceed-
ings of the Ninth International Conference on Machine Learning, pages 102 – 110.
Morgan Kaufmann, 1992.

William W. Cohen. Grammatically biased learning: Learning logic programs
using an explicit antecedent description language.Artif. Intell., 68 ( 2 ): 303 – 366 ,
1994.

T. H. Cormen, C. E. Leiserson, R. L. Rivest, and C. Stein. Introduction to Al-
gorithms. MIT Press, second edition, 2001.

Ian Dempsey, Michael O’Neill, and Anthony Brabazon. Foundations in Gram-
matical Evolution for Dynamic Environments, volume 194 ofStudies in Com-
putational Intelligence. Springer, 2009. URL [http://www.springer.com/](http://www.springer.com/)
engineering/book/978-3-642-00313-4.

David E. Goldberg. Genetic Algorithms in Search, Optimization, and Machine
Learning. Addison-Wesley Professional, 1989. ISBN 0201157675.

John E. Hopcroft, Rajeev Motwani, and Jeffrey D. Ullman. Introduction to
Automata Theory, Languages, and Computation. Addison Wesley, 2 edition,
2000. ISBN 0201441241.


John R. Koza.Genetic programming: On the programming of computers by natural
selection. MIT Press, 1992. ISBN 0 - 262 - 11170 - 5.

Sean Luke. Genetic programming produced competitive soccer softbot teams
for RoboCup 97. InGenetic Programming 1998 : Proceedings of the Third Annual
Conference, pages 214 – 222. Morgan Kaufmann, 1998.

Sean Luke, Charles Hohn, Jonathan Farris, Gary Jackson, and James Hendler.
Co-evolving soccer softbot team coordination with genetic programming. In
Hiroaki Kitano, editor,RoboCup- 97 : Robot Soccer World Cup I, pages 398 – 411.
Springer-Verlag, 1997.

David J. Montana. Strongly typed genetic programming.Evolutionary Compu-
tation, 3 : 199 – 230 , 1994.

Stephen Muggleton. Inductive logic programming: derivations, successes and
shortcomings.SIGART Bull., 5 : 5 – 11 , January 1994. ISSN 0163 - 5719.

Peter Naur. Revised report on the algorithmic language Algol 60 .CACM, 6 ( 1 ): 1 –
17 , 1963. URLhttp://www.cc.gatech.edu/data_files/classes/cs6390/
readings/algol.pdf.

Adam Nohejl. Grammatical Evolution. Bachelor thesis, available athttp://
nohejl.name/age/documentation/, 2009.

Michael O’Neill and Anthony Brabazon. Evolving a logo design using linden-
mayer systems, postscript and grammatical evolution. In Jun Wang, ed-
itor, 2008 IEEE World Congress on Computational Intelligence, pages 3788 – 3794 ,
Hong Kong, 1 - 6 June 2008. IEEE Computational Intelligence Society, IEEE
Press.

Michael O’Neill and Conor Ryan.Grammatical Evolution: Evolutionary Automatic
Programming in an Arbitrary Language. Springer, 1 st edition, 2003.

Michael O’Neill, Erik Hemberg, Conor Gilligan, Eliott Bartley, James McDer-
mott, and Anthony Brabazon. GEVA – Grammatical Evolution in Java (v 1. 0 ).
Technical report, UCD School of Computer Science and Informatics, 2008.
URLhttp://www.csi.ucd.ie/files/ucd-csi-2008-09.pdf.

Riccardo Poli, William B. Langdon, and Nicholas Freitag McPhee. A field
guide to genetic programming. Published viahttp://lulu.com and freely
available athttp://www.gp-field-guide.org.uk, 2008. URLhttp://www.
gp-field-guide.org.uk. (With contributions by J. R. Koza).

Conor Ryan, J. J. Collins, and Michael O’Neill. Grammatical evolution:
Evolving programs for an arbitrary language. In Wolfgang Banzhaf, Ric-
cardo Poli, Marc Schoenauer, and Terence C. Fogarty, editors,Proceedings of
the First European Workshop on Genetic Programming, volume 1391 ofLNCS,
pages 83 – 95 , Paris, 14 - 15 April 1998. Springer-Verlag. ISBN 3 - 540 - 64360 - 5.


C. M. Sperberg-McQueen.A brief introduction to definite clause grammars and defin-
ite clause translation grammars. A working paper prepared for the W 3 C XML
Schema Working Group, 2004. URLhttp://cmsmcq.com/2004/lgintro.
html.

Leon Sterling and Ehud Shapiro. The Art of Prolog: Advanced Programming
Techniques. MIT Press, 2 nd edition, 1994. ISBN 978 - 0 - 262 - 69163 - 5.

Peter Whigham. Inductive bias and genetic programming. InFirst Interna-
tional Conference on Genetic Algorithms in Engineering Systems: Innovations and
Applications, GALESIA, pages 461 – 466 , 1995.

Peter Whigham. Search bias, language bias and genetic programming. In
Genetic Programming 1996 : Proceedings of the First Annual Conference, pages
230 – 237. MIT Press, 1996.

Man Leung Wong and Kwong Sak Leung. Applying logic grammars to induce
sub-functions in genetic programming. In 1995 IEEE Conference on Evolution-
ary Computation, volume 2 , pages 737 – 740 , Perth, Australia, 29 November - 1
December 1995. IEEE Press.

Man Leung Wong and Kwong Sak Leung. Data Mining Using Grammar-Based
Genetic Programming and Applications. Kluwer Academic Publishers, Norwell,
MA, USA, 2000. ISBN 079237746 X.



## List of Abbreviations

**ADF** automatically defined function. 9 , 10 , 15 , 17 , 18 , 23 , 24 , 54 – 58 , 61 – 64 , 75 ,
83

**BNF** Backus-Naur form. 5

**CFG** context-free grammar. 4 – 7 , 12 , 13 , 15 , 54

**CFG-GP** context-free grammar genetic programming. 12 , 14 , 15 , 17 , 18 , 21 , 25 ,
27 – 31 , 34 – 36 , 38 – 40 , 43 , 44 , 47 – 52 , 54 – 58 , 61 – 64 , 67 , 70 – 80 , 83 , 84

**CV** coefficient of variance. 28 , 31 , 35 , 43 , 44 , 52

**DCG** definite clause grammar. 6 , 7 , 11 , 14 , 15 , 17 , 47

**GA** genetic algorithms. 3 , 9 , 16 , 17 , 22

**GE** grammatical evolution. 15 – 18 , 21 , 22 , 25 , 27 – 31 , 34 – 36 , 38 – 40 , 43 , 44 , 47 – 52 ,
54 – 58 , 61 – 64 , 67 , 71 – 77 , 79 , 80 , 83 , 84

**GP** genetic programming. 1 , 8 – 12 , 14 – 17 , 21 – 25 , 48 , 51 , 55 , 61 – 64 , 67 , 80 , 83

**ILP** inductive logic programming. 11 , 12 , 14 , 17 , 25

**STGP** strongly-typed genetic programming. 9 , 11 , 13 , 15




The Evolution of Stochastic Regular Motifs for Protein Sequences 1

# The Evolution of Stochastic Regular Mo-

# tifs for Protein Sequences

```
Brian J. ROSS
Brock University, Dept. of Computer Science
St. Catharines, Ontario, Canada L2S 3A
bross@cosc.brocku.ca
```
## Abstract Stochastic regular motifs are evolved for protein sequences

```
using genetic programming. The motif language, SRE-DNA, is a stochas-
tic regular expression language suitable for denoting biosequences. Three
restricted versions of SRE-DNA are used as target languages for evolved
motifs. The genetic programming experiments are implemented in DCTG-
GP, which is a genetic programming system that uses logic�based at-
tribute grammars to define the target language for evolved programs.
Earlier preliminary work tested SRE-DNA�s viablility as a representation
language for aligned protein sequences. This work establishes that SRE-
DNA is also suitable for evolving motifs for unaligned sets of sequences.*^1
```
```
Keywords Protein Motifs, Stochastic Regular Expressions, Grammat-
ical Genetic Programming, Evolutionary Computation.
```
## � 1 Introduction

A motif is a representation modeling some shared characteristic of a
family of proteins.5)There are a number of ways in which motifs can be used.
The most common use of a motif, as considered in this paper, is to act as a device
for characterizing the sequence pattern common to a particular protein family,
and therefore distinguishes them from unrelated sequences. In other words, a
motif is a signature for a protein family. Once an effective motif is established
for a set of protein sequences, it can be used to query a database of proteins
in order to extract the sequences belonging to the family. This permits the
discovery of structural similarities between new sequences added to the database

* (^1) New Generation Computing, vol.20, n.2, Feb. 2002, pp. 187-213.


2 Brian J. ROSS

and others whose functionalities have been established earlier. Hence, motifs are
means for determining potential protein functionalities for new sequences, which
is obviously an important practical tool for biologists.
An active research area is automated motif discovery. Given the grow-
ing rate at which biosequences are being cultivated and added to databases, the
manual construction of useful motifs becomes increasingly impractical, given the
sheer volume of data to be analyzed. Furthermore, manual design of motifs is
error prone, since a human being may not recognize the subtleties that deter-
mine protein family membership. The use of machine learning techniques for
automatic motif synthesis has been studied by many researchers.3, 6)Given the
technical complexities of predicting protein functionality from raw biosequences,
there does not yet exist a machine learning technology which can determine the
most biologically pertinent motif for a given family of sequences. In the mean-
time, however, different machine learning algorithms and sequence representa-
tions can serve as tools that geneticists can use to study new sequence patterns,
and therefore aid them in deriving the most biologically sound representation
for new families of interest.
This paper describes research in the automatic synthesis of biosequence
motifs using evolutionary computation. A contribution of this work is the in-
troduction of a new motif language, stochastic regular expressions (SRE-DNA).
SRE-DNA has characteristics of other, more established motif representations.
Its regular expression bases is similar to commonly used regular expression motif
languages, such as that used by the PROSITE protein database.12)On the other
hand, its probabilistic nature is similar to stochastic biosequence representations
such as HMM�s.22)Another goal of this paper is to show that SRE-DNA motifs
are readily synthesizable by genetic programming. SRE-DNA�s stochastic mod-
eling of biosequences can be exploited directly by the evolution process, both
during fitness evaluation, and during the evolutionary search itself. It is not a
goal of this paper to suggest that this approach to motif discovery is necessarily
superior to other established techniques, but rather, to illustrate that the au-
tomatic discovery of stochastic motifs is feasible using SRE-DNA and genetic
programming.
Section 2 reviews the problem of motif identification. The SRE-DNA
motif language is outlined in Section 3. Genetic programming is reviewed in Sec-
tion 4. Section 5 overviews the design of the experiments. Results are presented
in Section 6, and evaluated in Section 7. Comparisons to related work are given


The Evolution of Stochastic Regular Motifs for Protein Sequences 3

in Section 8. Conclusions and future directions conclude the paper in Section 9.

## � 2 Biosequence Identification

### 2.1 Motif Representations

Genetic similarities due to common evolutionary origins amongst differ-
ent species can often be identified by the similar protein functionalities observed
at the bio-molecular level. The precise functionality of a transcribed protein
is fundamentally determined by its 3D structure. Although of primary impor-
tance, such 3D structures are difficult and impractical to ascertain directly from
biosequences themselves, and thus remains a critically important open problem
in bioinformatics. Consequently, more rudimentary characterizations are used
for protein classification and prediction. Motifs that model protein families via
the shared similarities in their biosequence composition are widely used. Even
though such motifs are crude, indirect denotations of therealfactor of impor-
tance (3D structure), often they are currently the most practical means for char-
acterizing protein families, due to their parsimony, efficiency of interpretation,
and amenability to automatic acquisition from raw sequences.
Many factors are pertinent to biosequence representation, which dif-
ferent motif representations may incorporate in varying degrees.5, 8)Of central
importance is the ability to representconserved regionsof amino acid sequences.
These are the sequences common within a protein family, where differences are
usually minor, and inserts and deletes are rare. Typically the conserved region
is surrounded by similar sequential patterns unique to an identified protein, but
which vary in composition, size, and location according to the particular species
of interest. Naturally, when larger sequence lengths in the vicinity are consid-
ered, wider variability to motif patterns are introduced; a point will be reached
when the length is too large to be representationally beneficial. Consensus pat-
terns can vary with respect to their location within supersequences. They can
also repeat within sequences, and possibly overlap. What can greatly complicate
the use of motifs is the phenomena of convergent evolution, in which two sepa-
rate evolutionary paths have evolved completely different sequences, but which
have similar (converged) protein functionalities. In other words, the 3D struc-
ture of these different sequences yield similar functionalities. Such cases prohibit
straight-forward use of motifs, and they will not be considered further in this
paper.


4 Brian J. ROSS

Different motif languages have different linguistic strengths and weak-
nesses with respect to biosequence representation. The simplest motif is a con-
sensus sequence, which defines the exact sequence of amino acid codons common
to a family of proteins. Since such common sequences may be too small to be
practical, longer consensus patterns are often used. Pattern representations
permit variability, which reflect species diversity due to evolution. Consensus
patterns may be symbolic, for example, regular languages2, 6, 7)and higher-level
grammars.27, 28)Symbolic representations are called such because the structure
of the motif is directly mappable to the symbolic representation within the mo-
tif expression. This lends symbolic representations such as regular expressions
their greatest advantage: the user-level interface to a family of sequences is an
algebraic abstraction of the consensus pattern. Such motifs are akin to query
languages used in conventional databases. A disadvantage of symbolic represen-
tations is that more complex protein families result in correspondingly complex
symbolic expressions. Numeric and probabilistic representations are also com-
monly used in motif representations.8, 22, 16) Their main advantages are their
inherent ability to account for structural variation, and their natural encoding
of �scores� with which to heuristically judge the relative similarity of candidate
sequences to a family profile. An instance of a motif language sharing both sym-
bolic and numeric features is the stochastic context-free grammar of Sakakibara
et al.,26)as well as the stochastic regular expression language used in this paper.
The comparative linguistic power of different motif representations can
be better understood when formal language theory is considered.14)An impor-
tant insight obtained from such a viewpoint is that motif languages can be
formally categorized with respect to their expressive power, according to their
position in the Chomsky hierarchy. For example, all regular motif languages are
expressively equivalent; regular expressions are no more or less powerful than
regular automata such as HMM�s. Furthermore, context-free grammar motifs
are representationally more powerful than regular language motifs. Formal lan-
guage theory also lends insight into the complexity of recognizing sequences with
respect to motif representations. For example, strings can be recognized by reg-
ular languages in polynomial time. This means that efficient database access is
possible for regular motif languages; such advantages will be lost if higher-level
denotations such as context-sensitive grammars are used. Finally, a language-
theoretic view of motif representation can indicate if the automatic induction of
motifs is feasible.


The Evolution of Stochastic Regular Motifs for Protein Sequences 5

Not withstanding the advantages of characterizing motifs as formal lan-
guages, this purely formal perspective can be somewhat myopic. At a funda-
mental level, biosequences are indeed programs: they are used as instructions
during protein translation. However, this does not imply that a family of biose-
quences is most naturally or effectively denoted by a particular formal language
in the Chomsky hierarchy. Although symbolic or numeric representations can
be useful classification and prediction tools, they are not designed as models
of the pertinent structural effects that determine protein functionality. For ex-
ample, a regular expression motif may adequately segregate a family of protein
sequences from non-family sequences. However, this sequence-level classifica-
tion does not account for the deeper structural principles defining the family.
A more intelligent motif representation would use 3D stuctural information and
other domain-specific knowledge. Such a motif representation would likely be
too complex for user-specified protein classification and database access, at least
compared to regular motifs. Despite shortcomings, there are often benefits to
simplicity.

### 2.2 Regular Expression Motifs

Regular expressions are widely used as symbolic motif languages, and
are used in protein databases such as PROSITE and its variants.12)Their popu-
larity as motif languages arises from their simplicity, which makes them straight-
forward to learn; their expressive adequacy for representing relatively small se-
quences; and their computational tractability with respect to interpretation and
automatic acquisition. A hierarchy of regular expression motif languages has
been proposed.6)This hierarchy identifies the various degrees of expressiveness
with which nondeterministic choice of codons and gap expressions can be ar-
ticulated. For example, the most deterministic category (class A) is one that
denotes simple sequences of required codons (�t-c-t-t-g-a�). Class B extends
class A with a wildcard �x� character, which substitutes for any codon (�t-c-
x-x-g-a�). Further classes introduce additional devices. At the most expressive
extreme, class I languages are those which additionally permit nondeterministic
skip expressions, indeterminate gaps, and alternate choices (masks):

```
d-t-x(2,4)-v-*-a-x-[nq]-g
```
Here, �x(2,4)� denotes a skip of length 2 to 4, �*� is an indeterminate gap, and
�[nq]� denotes a choice of either n or q.


6 Brian J. ROSS

Note that formal language theory states thatallregular languages, and
hence all the motif classes discussed above, are equivalent with respect to the
languages denotable. However, the introduction of new language constructs en-
ables motif expressions to be more convenient and parsimonious. Higher classes
of regular expressions can characterize particular protein patterns more con-
cisely than more rudimentary classes. Hence the concept of expressiveness as
used in the classification scheme in6)is one of user-oriented convenience, rather
than a language-theoretic one. Nevertheless, this classification scheme can have
ramifications on the tractability of automatic expression synthesis performed via
machine learning algorithms.

### 2.3 Automatic Motif Acquisition

Much work has been done on machine learning techniques for biose-
quences identification.6, 3)The survey in6)lists 26 different algorithms, published
between the years 1983 and 1996. Most work has focussed on regular language
representations, since they are efficient to learn, and both adequate and prac-
tical for the relatively low complexity of sequences being analyzed. Motif dis-
covery is an instance of a classical machine learning problem: formal language
induction.17) A successful motif should accurately identify a sequence belong-
ing to a given family, while at the same time, reject sequences that are not
members. This implies the existence of two sets of examples � a positive set
consisting of protein sequences, and negative examples which are disjoint from
the positive set. Motif discovery algorithms that use this criteria areclassifi-
cationalgorithms, as opposed toconservationalgorithms which only use a set
of positive example sequences.6)Successful classification requires a balance be-
tween recognizing member sequences and rejecting non-member sequences in the
training set, while at the same time being general enough to recognize and reject
sequences not seen in the original training sets.
There are other dimensions by which motif learning algorithms can be
classified. Thesolution spaceof an algorithm is the type of representation used
to denote hypotheses, and may include regular consensus patterns, weight matri-
ces, Bayesian networks, or Hidden Markov Models (HMM). The alphabet used
by a representation is also pertinent, as are the generalizations used by the motif
language, for example, wildcard characters that denote sequence gaps. Learning
algorithms are also distinguishable by such factors as whether sequences must
be prealigned or not, whether they guarantee solutions, their computational


The Evolution of Stochastic Regular Motifs for Protein Sequences 7

efficiency, and their overall effectiveness in biological applications. The effec-
tiveness of a learning algorithm is difficult to define, since different algorithms
exhibit particular advantages and disadvantages in their natural domains. Con-
sequently, the various approaches to motif learning have not yet been empirically
compared with one another.

### 2.4 Motifs and Genetic Programming

Genetic programming (GP) is a machine learning paradigm, and it is re-
viewed in Section 4. GP has been successfully applied towards various problems
in biosequence analysis.19, 10, 11, 20)Most of these applications involve the evolu-
tion of programs which identify various properties of biosequences, for example,
intracellular or extracellular portions of sequences.
There are a couple of examples of the use of GP to evolve biosequence
motifs. Hu uses GP to evolve motifs for unaligned example sequences, using a
regular language equivalent to that used by PROSITE.15)Expressions are evolved
from sets of unaligned example sequences. Hu�s evolution algorithm uses a local
optimization step, in which expression terms denoting gaps are refined. A num-
ber of protein families were studied, and the evolved solutions were often very
similar to the source PROSITE motifs used to extract the example sequences.
Koza, Bennett, Andre and Keane use GP with ADF�s (automatically
defined functions) to evolve a motif for a few unaligned sequence examples.21)
An ADF is a type of module or subroutine. The regular motif language used is
simpler than the full PROSITE language as used by Hu. The protein families
studied contain sequence repetitions, which makes the use of ADF�s advanta-
geous, since the ADF�s modularize the repeated structures. Koza did not specify
any motif parameterization requirements (eg. window size), other than overall
expression depth limits. The motif evolved for one case was found to be more
accurate than the accepted one on file for that protein family.

## � 3 Stochastic Regular Expressions

### 3.1 SRE

Stochastic Regular Expressions (SRE) is a probabilistic regular expres-
sion language, in which regular expressions14)are embellished with probability
fields.23)A similar language was previously proposed by Garg, Kumar and Mar-
cus, who prove a number of mathematical properties of the language.9)


8 Brian J. ROSS

Unrestricted SRE has the following form. LetErange over SRE,arange
over atomic actions,nrange over integers (n=1), andprange over probabilities
(0< p <1). SRE syntax is:

```
E ::= a | E:E | E*p |E+p | E 1 (n 1 ) +...+Ek(nk)
```
The terms denote atomic actions, concatenation, iteration (Kleene closure or
* iteration, and + iteration), and choice. The + iteration operator,E+p, is
equivalent toE:E*p.
The semantics of the language are briefly outlined as follows. With
choice, each termEi(ni) is chosen with a probability equivalent toni/Sj(nj).
For example,a(1) +b(2) +c(3) means that the terms are selected with probabil-
ities of 1/6, 1/3 and 1/2 respectively. With the Kleene closure termE*p, each
iteration ofEoccurs with a probabilityp, and the termination of iteration has
a probability 1-p. Probabilities between terms propagate in an intuitive way.
For example, with concatenation, the probability ofE:F is the probability of
Emultiplied by the probability ofF. Therefore, the probability ofE+pis the
same asE:E*p, which is the probability ofEmultiplied by the probability of
the Kleene iteration.
The overall effect of this probability scheme is that an SRE expression
defines a sound, well-formed model of probability: each expression defines a
probability function. The sum of all the probabilities for alls?L(E) is 1.23)
Furthermore, each strings?L(E) has an associated probability, while any
s6?L(E) has a probability of 0. This property is important for this research,
since biosequences will be associated with probabilities when given to particular
SRE-DNA motifs. A task of motif synthesis will therefore be to evolve motifs
that yield high probabilities for candidate strings.
An example SRE expression is:
(a:b*^0.^7 )(2) +c*^0.^1 (3)

It recognizes stringcwithPr= 0.054 (the term withccan be chosen with
Pr= 3/(2 + 3) = 0.6; then that term iterates once withPr= 0.1; finally the
iteration terminates withPr= 1- 0 .1 = 0.9, giving an overall probability of
0. 6 � 0. 1 � 0 .9 = 0.054). The stringbbis not recognized; its probability is 0.
An SRE interpreter is implemented and available for GP fitness func-
tions. Like the case with conventional regular expressions,14)string recognition
for SRE expressions is of polynomial time complexity. To test whether a string
sis a member of an SRE expressionE, the interpreter attempts to consumes


The Evolution of Stochastic Regular Motifs for Protein Sequences 9

withE. If successful, a probabilityp >0 is produced. Unsuccessful matches
will result in probabilities of 0. The SRE-DNA interpreter only succeeds if an
entire SRE-DNA expression is successfully interpreted. For example, inE 1 :E 2 ,
ifE 1 consumes part of a string, butE 2 does not, then the interpretation fails
and yields a probability of 0.

### 3.2 SRE-DNA

The protein sequences studied here are fairly restricted in form. This is
evident if one examines the source PROSITE motifs used to access them from
the database. This implies that unrestricted SRE may be too descriptively rich
for the motifs required here. This would not normally pose a problem, since
a linguistically richer language may enjoy benefits over weaker ones. Unfor-
tunately, unrestricted SRE does indeed pose problems during interpretation of
expressions, because of the nature of the iteration and choice operators. Al-
though regular expression and SRE expression interpretation is of polynomial
complexity, expression interpretation is of combinatorial complexity with respect
to expression size. For example, the expression ((a)*)*can generate the string
a...aof lengthka total of 2kdifferent ways, due to the combinatorial number
of ways the two nested iteration operators can interact.
Early work on this project found that inefficient SRE expressions with
nested iteration and choice were commonly constructed, and their slow interpre-
tation made processing impossible.25)As a consequence, a restricted version of
SRE, SRE-DNA, is more practical. The details of the grammatical constraints
in SRE-DNA are described in Section 5.2, where three variations of SRE-DNA
are introduced. All the variants ignore the choice operator, and restrict iteration
so that terms with iteration are guaranteed to be constructive.
SRE-DNA also introduces mask terms, which are sets of amino acid
codons. A mask within an SRE-DNA expression means that any of the listed
codons is permissible at that portion of the sequence. For example, the mask in
the expression

```
a: [b,c,d] :e
```
denotes a choice ofb,c, ord, each with an equal probability of 1/3. Hence the
stringsabe,ace, andadeare valid strings of the expression, and each have a
probability of 1/3. SRE�s choice (+) operator is more general than a mask, and
can denote the same probabilistic languages. For example, SRE would denoted
the above as:


10 Brian J. ROSS

```
a: (b(1) +c(1) +d(1)) :e
```
Choice expressions also permit the modeling of more informative codon proba-
bility distributions that might exist in real familes. For example,

```
a: (b(120) +c(12) +d(3)) :e
```
says that codonbhas a probability of 120/135 or 88.9%. This expressiveness is
not available in a mask set, which treats all codons with the same probability.
Despite the weaker stochastic expressiveness of masks compared to choice
expressions, masks are more concise, and consequently lend efficiency to evolu-
tion during motif synthesis. Earlier work found that the choice operator is
detrimental to motif evolution for the proteins studied here, because it promotes
GP intron material (expression bloat).25)Another reason to adopt masks is that
PROSITE motifs use them, and so it might be possible to evolve SRE expres-
sions similar to the non-probabilistic regular motifs used by PROSITE. If a more
precise denotation of codon distributions is required, choice expressions should
be investigated further.
Core regions of conserved motifs are ungapped, in the sense that in-
sertions or deletions can change the activity dramatically. Nevertheless, as a
linguistic convenience, the use ofskip(gap) expressions is convenient in the
context of regular motif languages such as SRE-DNA and PROSITE. This is
because the purpose of such regular motif expressions is to concisely denote the
common amino acids within a protein sequence. Since there will areas of pat-
tern variability within family sequences, they are best denoted by gaps. Ideally,
the resulting codons resident in a motif will be the commonly shared ones in
the family. The alternative is to represent all possible codons at these positions
explicitly with masks or choice expressions, as is done in21). This would result
in large motifs whose compactness and utility is lost.
Skip expressions are implemented via the codex, which is a wildcard
that replaces any codon in a sequence. SRE-DNA will combined this with iter-
ation operators (for example, �c*.^10 �), which permits variable-length gaps to be
represented (details in Section 5.2).

## � 4 Genetic Programming

Genetic programming4, 18, 21)is a method of automatic programming in
which programs are evolved using a genetic algorithm.13). Both GP and GA are
characterized by their use of the following (see Fig. 1): (i) an initial popula-


The Evolution of Stochastic Regular Motifs for Protein Sequences 11

1. Initialize: Generate initial randomized population.
2. Evolution:
    GenCount := 0
    Loop whileGenCount<maximum generations
       andfitness of best individual not considered a solution{
          Loop untilNew population size = max. population size{
             Select a genetic operation probabilistically:
             ?Crossover:
                Select two individuals based on fitness.
                Perform crossover.
             ?Mutation:
                Select one individual based on fitness.
                Perform mutation.
             Add offspring to new population.
          }
GenCount := GenCount+
    }
3. Output:Printbest solution obtained.

```
Fig. 1 Genetic Algorithm
```
tion of random individuals, which in the case of GP are randomly�constructed
programs; (ii) a finite number of generations, each of which results in a new
or replenished population of individuals; (iii) a problem�dependent fitness func-
tion, which takes an individual and gives it a numeric score indicative of that
individual�s ability to solve a problem at hand; (iv) a fitness�proportional se-
lection scheme, in which programs are selected for reproduction in proportion
to their fitness; (v) reproduction operations, usually the crossover and mutation
operations, which take selected programs and generate offspring for the next
generation.
The essential difference between GP and GA is the denotation of in-
dividuals in the population. A pure GA uses genotypes that are fixed�length
bit strings, and which must be decoded into a phenotype for the problem be-
ing solved. A GP uses a variable�length tree data structure genotype, which
is directly interpretable as a computer program by some interpreter. The use
of programming code as genotype is a powerful and practical representation for


12 Brian J. ROSS

solving a wide variety of problems.
The two main reproduction operators used in GP are crossover and
mutation. Crossover permits the genetic combination of program code from
programs into their offspring, and hence acts as the means for inheritance of
desirable traits during evolution. Crossover takes two selected programs, finds
a randomcrossover pointin each program�s internal representation (normally a
parse tree), and swaps the subtrees at those crossover points. Mutation finds a
random node in a selected program, and the subtree at that node is replaced with
a new, randomly generated tree. This is the means by which new genetic traits
can be introduced into the population during evolution. Although crossover and
mutation preserve the grammatical integrity of programs, the user must ensure
closure� that the resulting programs are always executable. So long as closure
is maintained, all programs derivable by the GP system will be executable by
the fitness function, and hence their fitness will be derivable.
The GP implementation used in this paper is DCTG-GP. DCTG-GP
is a grammatical genetic programming system.24)It uses logical grammars for
defining the target language for evolved programs. The logic grammar formalism
used is definite clause translation grammars (DCTG).1)A DCTG is a logical
version of a context-free attribute grammar, which allows the complete syntax
and semantics of a language to be defined in a unified framework. DCTG-
GP is useful for these experiments because of the ease with which variations of
motif languages can be designed and implemented (see Section 5.2). The use of
grammars within GP also helps enhance search efficiency, by pruning the search
space into more sensible structures.

## � 5 Experiment

### 5.1 Protein Sequences

As discussed in Section 2, motif classification algorithms require the dis-
covery of an expression that recognizes positive protein sequence members, while
rejecting negative sequences. The positive examples used here comprise a set
of unaligned protein sequences. To generate this set, aligned protein sequences
were initially obtained from the SWISS-PROT and TrEMBL sequence databases
for the protein families listed in Table 1.12)*^2. For example, on August 9 2000,
seaching the expression �snake toxin� resulted in 176 instances in the SWISS-

* (^2) Accessed between July and November 2000, at [http://expasy.cbr.nrc.ca/sprot/](http://expasy.cbr.nrc.ca/sprot/)


The Evolution of Stochastic Regular Motifs for Protein Sequences 13

```
Table 1 Protein families and related parameters
Protein (accession #) lab m p w s1 s
```
```
Amino acid oxidase (PS00677) AAO 5 1e-12 19 8 -
Scorpion toxin (PS001138) ScT 5 1e-12 24 20 -
Zinc finger, C2H2 type (PS00028) ZF1 10 1e-13 25 29 678
Zinc finger, C3HC4 type (PS00518) ZF2 8 1e-13 10 21 168
Snake toxin (PS00272) SnT 5 1e-12 22 18 127
Kazal inhibitor (PS00282) KI 5 1e-12 24 24 125
```
PROT database, and an additional 98 examples from TrEMBL. Each example
in the database is a contribution by biologists, and contains a protein pattern
from various species and genome locations. Note that many sequences are dupli-
cated in the databases. In addition, the entire family of snake toxin patterns has
a reference PROSITE motif expression. This pattern is useful for comparison
with SRE-DNA motifs, as there are enough similarities in the PROSITE regular
expression language and SRE-DNA that similar patterns can often be detected
between them in many experiments.
The following is done to create a set of unaligned sequences from the
above aligned data. First, duplicate sequences are removed. Then the aligned
sequences are padded evenly on each side with randomly generated sequences of
amino acid codons. The length of unaligned sequences for all experiments is 150
codons. When possible, a subset of these unaligned sequences is used as training
data, and the remainder are testing data. Otherwise, in cases when there are
few examples, the entire set of sequences is used for training. Using artificial
sequences such as these has advantages and disadvantages. A possible disadvan-
tage is that, unlike real unaligned data, the synthesized unaligned data�s random
padding is unlikely to share subsequences outside of the functional domain of
the protein in question. This suggests that the unaligned sequences we are using
may be easier to process than what might be seen in a more realistic production
environment. On the other hand, an advantage in using them is that distract-
ing noise is being avoided. This permits the experiments to focus on the basic
problem of SRE-DNA motif synthesis, without becoming encumbered by noisy
instances of shared artifacts that arise in real unaligned sequences. In the future,
the problem of evolving SRE-DNA motifs in a noisy, more realistic environment


14 Brian J. ROSS

should be addressed.
Negative examples are randomly-generated amino acid sequences, each
having a length approximately the size of the original aligned PROSITE data
(thewindow size). Again, this is not as realistic as is possible. A more chal-
lenging negative set would consist of a variety of real sequences, many of which
might be closely related to the family belonging to the positive training set. For
the purposes of this research, however, a randomly-generated negative set is ade-
quate for ensuring that evolved motif expressions are sufficiently discriminating.
Table 1 summarizes characteristics of the example protein sequences
used for the GP experiments. The amino acid oxidase and scorpion toxins were
chosen because the PROSITE source motifs were of intermediate complexity.
The zinc fingers, snake toxin, and kazal inhibitors were chosen in order to com-
pare the results to that of Hu.15)The protein names and PROSITE accession
numbers are in column 1. Column 2 (lab) contains a shorthand label used in
subsequent figures. Themcolumn gives the maximum mask size permitted in
SRE-DNA expressions for that protein family. It is based on the maximum mask
size used in the PROSITE motif for that family, and is typically a little larger
than used by PROSITE in order to give motif evolution some extra freedom.
The minimum probability value (p) is the minimum computed probability re-
quired by an SRE-DNA expression to continue processing, before terminating
expression interpretation. Thewvalue is the size of the window used for that
protein set, and is configured to be large enough to cover the largest aligned
sequence for the family as defined by its source PROSITE motif. Finally,s1and
s2are the sizes of the disjoint training and testing sets respectively.

### 5.2 SRE-DNA Definition

DCTG-GP was used to derive 3 restricted variations of SRE-DNA.
Grammars for these variants are shown in Figure 2. All the grammars offer
various levels of grammatical constraints on full SRE, while at the same time
use important SRE-DNA operations such as probabilistic skip and/or iteration.
The grammars also encode general characteristics similar to what are found in
established PROSITE motifs for protein families. In particular, the use of alter-
nating skip and mask expressions will be favoured in evolved solutions.
One motivation for using constrained SRE-DNA is the fact that unre-
stricted SRE-DNA will generally result in inefficient expressions. For example,
previous work25)established that SRE-DNA�s choice operator is distinctly un-


The Evolution of Stochastic Regular Motifs for Protein Sequences 15

```
G 1 expr ::= guard |guard:expr |expr+p
guard ::= mask |mask:skip
skip::= x+p
```
```
G 2 expr ::= guard |guard:expr
guard ::= mask |mask:skip
skip::= x*p |x+p
```
```
G 3 expr ::= gexpr |gexpr*p |gexpr+p
gexpr ::= guard |guard:expr |expr:guard
guard ::= mask |mask:skip
skip::= x*p |x+p
Fig. 2 SRE-DNA Variations
```
desirable for denoting the types of sequences being analyzed here. The choice
operator promotes intron material in expressions, which is program code which
does not contribute meaningfully to computations. It was found that this op-
erator could be removed without any loss in expressiveness, at least with the
sequences studied here (its value as a nondeterministic operator is replaced by
mask terms). Likewise, totally unrestricted iteration tends to result in intron
material, as well as very inefficient expressions. Restricting iteration is therefore
practical.
Grammar 1 permits nested + iteration and + skip. Iteration is restricted
by the use of guard terms, which forces iteration to be applied to suffixes of
concatenated expressions. In other words, guards create a bias towards prefix
consumption, which promotes efficient sequence interpretation. Grammar 1 also
permits directly nested iteration. Nesting should not be common, however,
because nested iterative expressions usually have low probabilities, and hence
low fitness values. They therefore become extinct during evolution.
Grammar 2 is the only grammar without the iteration operator, and
is the language most similar to PROSITE�s motif language. Expressions in
grammar 2 will take the form of alternating masks and skips. Both * and + skip
are used.
Grammar 3 is the least-constrained grammar. Besides using both + and
* iteration, it permits a more symmetric distribution of iteration expressions on
either side of expressions (unlike grammar 1�s bias towards iteration on the right-


16 Brian J. ROSS

side of expressions). Like grammar 2, both * and + skip are used. However,
unlike grammar 1, nested iteration is not allowed. This is done via the use of
thegexprnonterminal.
The entire syntax and semantics of all the languages denoted in Figure 2
are implemented together in DCTG-GP. The semantic rules of the DCTG permit
various interpretation and analytical processing to be programmed within the
grammatical definitions. An advantage of this is that many grammatical devices
are concisely denoted with a DCTG. For example, the actual implementation
of grammar 3 does not use the distinct nonterminalsexprandgexpr. Rather,
the semantics forexprare used to directly test whether expressions are iterative
or not, in order to ensure that iteration is not nested. Using these semantic
devices greatly reduces the complexity of the grammar, which in turn promotes
efficiency of the GP implementation.
Note that, although not encountered in the sequences studied here, SRE-
DNA�s iteration operators permit the denotation of protein repeats (eg. Hunt-
ington�s polyCAG pattern).

### 5.3 Fitness Evaluation

Practically speaking, regular motif languages such as PROSITE�s and
SRE-DNA are most suitable for relatively small sequences. Large sequences
require impractically large expressions, which in SRE-DNA�s case, yield very
small probabilities. The aligned segment of sequences shared by a family is but
a fraction of the overall unaligned sequence. Therefore, to make processing more
efficient, the GP system requires that the user supply a window size parameter,
which defines the length of the longest window or aligned sequence length of
interest. In addition, the SRE-DNA interpreter onlys succeeds in evaluating an
expression if the entire SRE-DNA expression is used. For example, inE:F(where
Fdoes not recognize an empty string), if an input string is fully consumed byE,
then interpretation of the entire expression fails becauseFhas not been used.
Taken together with the window length, the result is that successful SRE-DNA
expressions are ones that are used in their entirety in recognizing a complete
window from the unaligned sequence.
A candidate motif must recognize members of the protein family of in-
terest, while at the same time reject non-member sequences. Likewise, a fitness
measure must balance the acceptance and rejection characteristics of expressions.
Consider the formula,


The Evolution of Stochastic Regular Motifs for Protein Sequences 17

```
Fitness=N+NegFit-PosFit
```
NegTotandPosFitare the negative and positive training scores respectively, and
Nis the number of positive (and negative) training sequences. A hypothetical
solution can yield a totalPosFitscore ofN(Npositive examples multiplied by
a score of 1 each, which is illustrated below). TheNegFitscore will be 0 for a
correct solution. Therefore, the value ofFitnessin the above forula yields 0 for
a perfect solution. This is impossible to obtain in practice, because the values
represented by thePosTotterm are usually small, due to the way probability
scores are used within them.
Positive scoring is performed on each positive example sequence. A
sequence is processed by extracting every consecutive window from it, and com-
puting a fitnessFiton each window. If a sequenceeihas length|ei|, and the
window size isw(wherew <|ei|), then there are|ei|-w+ 1 windows to process.
For example, the sequenceabcdefhas 4 windows of length 3:abc,bcd,cde, and
def. The positive fitness formula is then:

```
PosFit=
```
#### ?

```
ei?Pos
```
```
maximum(Fit(winj(ei)))
```
whereeiis a positive example sequence being processed, andwinjis one of its
jwindows. Here, for each positive sequenceei, each windowwinjis extracted
from it, and a positive score is obtained. The maximum of all the window scores
for a sequence is used for that sequence. Finally, all these maximal scores for all
the positive sequences in setPosare summed together, giving an overall positive
score.
Positive fitness evaluation of a window incorporates two measurements:
the probability of recognizing the window, and the proportional amount of the
window recognized in terms of its length. Consider the formula,

```
Fit(win) =^12
```
#### (

```
Pr(smax) +|smax|e||
```
#### )

Here,smaxis the longest prefix recognized from some windowwin, and|smax|
is its length. The termPr(smax) is the probability of recognizingsmax. The
second term measures the proportion of the window recognized. The fitness
pressure introduced by this formula is to favour expressions that recognize entire
windows with high probabilities. In early generations, the window-length term
dominates the score, which forces fitness to favour expressions that recognize
large portions of windows. The probability term still comes into consideration,


18 Brian J. ROSS

however, especially when the population converges to expressions that recognize
windows from a significant number of sequences. At that time, the probability
fitness measure favours expressions that yield high probabilities.
Negative fitness scoring is calculated as:
NegTot=maximum(Fit(ni))*N

whereni?Neg(negative examples). The highest obtained fitness value for any
recognized negative example suffix is used for the score. A discriminating expres-
sion will not normally recognize negative examples, however, and soFit(ni) = 0
for mostni.

### 5.4 Genetic Programming Parameters

```
Table 2 GP Parameters
```
```
Parameter Value
```
```
GA type generational
Maximum generations 100
Maximum runs/experiment 6
Functions SRE-DNA variants
Terminals amino acid codons, probabilities
Population size (initial) 2000
Population size (culled) 1000
Unique population yes
Max. depth initial popn. 12
Max. depth offspring 24
Tournament size 7
Elite migration size 10
Retries for reproduction 3
Prob. crossover 0.
Prob. mutation 0.
Prob. internal crossover 0.
Prob. terminal mutation 0.
Prob. SRE crossover 0.
Prob. SRE mutation 0.
SRE mutation range 0.
```

The Evolution of Stochastic Regular Motifs for Protein Sequences 19

Table 2 lists genetic programming parameters used for the experiments.
Generational evolution is performed, in which distinct populations are evolved
during each generation. Each population always consists of syntactically unique
expressions. The initial population size is 2000 individuals, which is gener-
ated using Koza�s ramped half-and-half tree generation scheme.18)Half the trees
generated aregrow trees, in which a terminal or nonterminal can be randomly
selected as the root of each subtree, while the remaining half arefull trees, in
which nonterminals are always selected so long as the tree depth limit is not ex-
ceeded. During tree generation, the tree depths are staggered (orramped) from
depths 2 through 12. The result is a population of random expressions having
a fair distribution of varied tree shapes. This initial population is culled by se-
lecting the 1000 most fit individuals. This oversampling of the initial population
helps discard the weak expressions which commonly arise during random tree
generation.
Nonterminals and terminals are determined by the SRE-DNA grammar
variant used (Section 5.2). All the grammars use amino acid codons in mask
terms, as well as numeric fields for probabilities. Reproduction operations on
these fields work in a number of ways. Crossover and mutation are selected
with probabilities of 90% and 10% respectively. When one of these reproductive
operations is selected, either a conventional GP version or a hybrid SRE-DNA
version may be applied. For example, the table entry indicating the probability
of SRE crossover means that, when crossover is to be performed, there is a 25%
chance that SRE-DNA crossover is used. Conventional grammatical crossover
selects a subtree having the same nonterminal or terminal type of the grammar
in each parent, and swaps these subtrees, yielding the offspring. SRE-DNA
crossover works on masks: two mask fields are selected in the parents, and the
two offspring created are identical to each respective parent, but with the selected
masks a merge of the elements from the parent masks.
Conventional grammatical mutation takes a subtree, and replaces it with
a randomly generated subtree of the same nonterminal or terminal type. SRE-
DNA mutation can involve a number of special operations: (i) a numeric field is
perturbed�10% of its original value; (ii) a mask has a random element inserted
into it; (iii) a mask has a random element removed from it; and (iv) a mask
element is randomly replaced.

### 5.5 Solution Refinement


20 Brian J. ROSS

```
For each maskmaskiin expressionexpr:
For each elementaj?maski:
```
- Removeajfrommaski, and call new expressionexpr'.
- If fitnessexpr'>fitnessexpr
    thenexpr?expr'.
       Fig. 3 Mask refinement

Often, an evolved solution motif can be further improved after the run
has terminated. Its overall performance as measured by its probability score on
recognized sequences can be increased by refining its mask terms. Many experi-
ments result in solution expressions that have unnecessarily large masks, which
yield smaller probabilities than optimally-sized ones. Although mask mutation
might occassionally delete extraneous mask codons, this occurs too infrequently
during evolution to prevent these bloated masks from appearing in solutions.
Mask refinement is performed on solution expressions by the procedure
in Figure 3. The procedure performs hill-climbing transformations on an expres-
sion, by deleting the mask elements whose removal improves overall fitness. The
algorithm is a greedy one, and it is not guaranteed to find the optimal refinement
for an expression. In other words, depending on the SRE-DNA grammar used,
some combinations of mask refinements might yield higher probabilities than
others. The algorithm used is adequate for the majority of expressions evolved
in this paper.

## � 6 Results

Table 3 summarizes the training and testing performance results for the
protein families in Table 1. Giis the grammar used from Figure 2. The best
measure of a motif�s performance is to compute the average probability obtained
when recognizing positive example sequences. We call this value theprobability
score, and denote it bypr. Then, �avgpr� is the average probability scores for
the best motifs for all six runs. Similarly, �bestpr� is the probability score of
the overall best motif. Testing was performed on the best evolved motif for every
grammar (the motif used to obtain �bestpr�). �%tp� is the percentage of true
positives obtained by the best motif on the testing set. �pr� is the probability
score obtained for the recognized testing sequences. This score is normalized for
the true positives recognized. The percentage of false negatives (positive test
cases yielding a probability of 0.0) is in column �%fn�, and is merely 100 - %tp.


The Evolution of Stochastic Regular Motifs for Protein Sequences 21

```
Table 3 Solution statistics for example proteins
Training Testing (best)
Family Gi avgpr bestpr %tp pr %fn %fp
AAO G 1 1.846e-4 8.565e-4
G 2 5.306e-5 1.914e-4 - - - -
G 3 5.414e-5 2.143e-4
ScT G 1 1.312e-7 5.170e-7
G 2 2.349e-8 5.537e-8 - - - -
G 3 2.451e-5 1.469e-4
ZF1 G 1 1.113e-11 2.898e-11 67.4 3.562e-11 32.6 0.0
G 2 7.330e-11 4.128e-10 81.3 4.645e-10 18.7 0.0
G 3 6.027e-11 3.374e-10 89.1 4.772e-10 10.9 0.0
ZF2 G 1 9.614e-3 2.019e-2 100.0 1.959e-2 0.0 0.0
G 2 1.278e-2 1.919e-2 100.0 1.953e-2 0.0 0.0
G 3 1.415e-2 2.542e-2 100.0 2.410e-2 0.0 0.0
SnT G 1 1.202e-7 2.398e-7 80.3 1.975e-7 19.7 0.0
G 2 5.153e-8 1.633e-7 82.7 1.311e-8 17.3 0.0
G 3 9.786e-8 1.857e-7 64.6 2.967e-7 35.4 0.0
KI G 1 2.964e-9 5.461e-9 80.0 5.416e-9 20.0 0.0
G 2 1.340e-8 3.772e-8 68.8 3.337e-8 31.2 0.0
G 3 2.354e-8 4.546e-8 70.4 5.424e-8 29.6 0.0
```
Finally, false positives (erroneously identifying a negative example as a member
sequence) is in column �%fp�. The actual testing results for the Kazal inhibitor
(KI) experiments using all three grammars can be viewed on the web*^3.
Table 4 presents some additional testing results on the best solutions
tested in Table 3. False negative (FN) and false positive (FP) sequences were
obtained from the PROSITE database for four of the protein families. This data
represents sequences which either were missed by the original PROSITE motif
(FN), or were erroneously classified as belonging to the protein family (FP).
Not all the families have FN and FP available (the AAO and ScT proteins had
none, and are omitted). In the #FN column in Table 4, the first number repre-
sents the number of false negative sequences identified by the SRE-DNA motif.
The number in parentheses is the total number of FN sequences extracted from
PROSITE for that protein. In terms of matching false negative sequences, higher

* (^3) [http://www.cosc.brocku.ca/~bross/research/kazal.html](http://www.cosc.brocku.ca/~bross/research/kazal.html)


22 Brian J. ROSS

```
Table 4 Additional testing results
Family Gi #FN (tot) pr #FP (tot) pr
ZF1 G 1 2 (3) 7.764e-12 12 (12) 3.882e-11
G 2 1 1.925e-10 6 1.724e-10
G 3 2 2.163e-11 7 1.829e-10
ZF2 G 1 4 (12) 2.398e-2 5 (5) 1.918e-2
G 2 4 2.398e-2 5 1.918e-2
G 3 4 1.942e-2 5 1.831e-2
SnT G 1 2 (10) 6.662e-8 (0) -
G 2 2 3.707e-8 -
G 3 0 0.0 -
KI G 1 0 (1) 0.0 0 (2) 0.0
G 2 0 0.0 1 8.569e-9
G 3 0 0.0 1 4.371e-9
```
numbers in the FN column are preferred, as they indicate that the SRE-DNA
motif recognized candidate proteins that were missed by the original PROSITE
regular motif expression. Theprvalue is the normalized average probability of
recognizing the FN sequences. The next two columns are similar to the FN ones,
except that false positive sequences are tested. Here, lower numbers in the FP
column are preferred, since they mean that SRE-DNA motifs were more discrim-
inatory than the PROSITE motifs, correctly classifying non-member sequences
that PROSITE erroneously permitted.
Tables 5 and 6 list the best solutions for the different protein families
and SRE-DNA variations, along with the source PROSITE motif used to obtain
the example sequences. Note that the PROSITE motif language differs from
SRE-DNA (see Section 2.3).

## � 7 Discussion

Firstly, the quality of results is influenced by the level of precision of
the experimental parameters. For example, due to time constraints, only 6 runs
were undertaken per experiment, which is inadequate for statistically meaningful
conclusions to be drawn. Additionally, more precise SRE-DNA interpretation
will occur when lower minimal probabilities are used by the interpreter (see
Table 1). For example, early experiments suggested that the zinc finger C2H2
(ZF1) family required a smaller minimum probability in order to get meaningful


The Evolution of Stochastic Regular Motifs for Protein Sequences 23

Table 5 Best evolved motifs for protein families. The motif beside family label is the source
PROSITE motif.
AAO [ilmv](2) :h: [ahn] :y:g:x: [ags](2) :x:g:x(5) :g:x:a
G 1 h:x+.^1 :y:g:x+.^17 : [gs] :x+.^17 :g:x+.^1 : [iqt] :x+.^17 : [ghs] :x+.^13
:g:x+.^1 :a:x+.^19
G 2 h:x+.^1 :y:x*.^11 :g:x+.^19 : [gs] :x+.^16 :g:x+.^19 : [hst]
:x+.^19 :g:x+.^16 :a:x+.^19
G 3 h:x+.^1 :y:g:x+.^19 : [gs] :x+.^19 :g:x+.^19 : [hst] :x+.^19
:g:x+.^17 :a:x+.^16
ScT c:x(3) :c:x(6,9) : [ags] :k:c: [imqt] :x(3) :c:x:c
G 1 c:x+.^19 : ((([acg] :x+.^12 : [cg] :x+.^19 :k:x+.^12 : [ct] :x+.^11 :c
:x+.^19 )+.^19 )+.^19 )+.^19
G 2 c:x+.^19 : [gkps] :x+.^19 : [acgk] :x+.^19 : [agn] :x+.^19 :c:x+.^19 : [gn]
:x+.^19 :c:x+.^1 :c:x+.^19
G 3 [ck] :x+.^17 :c:x+.^18 :k:x+.^19 : (c:x*.^15 :k:x+.^19 :a:x+.^18 :m
:x+.^15 :f:x+.^1 :k:c:x+.^1 )*.^19 : [gn] :x+.^18 :k:c:x+.^18
ZF1 c:x(2,4) :c:x(3) : [cfilmvwy] :x(8) :h:x(3,5) :h
G 1 [acfkr] :x+.^19 : [acfsv] :x+.^19 : [eklr] :x+.^19 : [fklnst]
:x+.^19 : [adklrt] :x+.^19 : [hlnrsv] :x+.^19 : [kr] :x+.^19 : [hl]
:x+.^19 : [filnrtvw] :x+.^19
G 2 c:x+.^19 :c:x+.^19 : [kqr] :x+.^18 : [afklr] :x+.^19 : [lqrs] :x+.^19 :h
:x+.^18 : [hklrt] :x+.^19 : [acdhkt] :x+.^19
G 3 c:x+.^19 :c:x+.^19 : [hkr] :x+.^19 : [afkqrsty] :x+.^19 : [hlnstv] :x+.^19
: [ahklnrst] :x+.^19 : [hkr] :x+.^19 : [hr] :x+.^19

results. Using a low probability like this enhances precision, but at the expense
of computation time. If even smaller probability limits were used, both the
training and testing results may be improved, due to the increased likelihood of
recognizing some examples.
Other parameters, such as mask size, iteration ranges, window size, and
training set size, also affect results. Preliminary runs suggested that larger
masks, as used in the zinc finger cases, usually result in lower quality solutions.
This implies that small masks are more naturally suitable for SRE-DNA motifs,
in comparison to PROSITE�s motifs. Similarly, large iteration ranges greater
than 0.25 often resulted in the evolution of motifs that would tend to skip large
portions of relevant aligned subsequences. The value used in these experiments


24 Brian J. ROSS

```
Table 6 Best evolved motifs for protein families (cont.).
ZF2 c:x:h:x: [filmvy] :c:x(2) :c: [ailmvy]
G 1 , 2 c:x+.^1 :h:x+.^19 :c:x+.^19 :c:x+.^1
G 3 c:x+.^1 :h:x+.^19 : (f:x*.^11 )*.^19 :c:x+.^19 :c:x+.^1
SnT g:c:x(1,3) :c:p:x(8,10) :c:c:x(2) : [denp]
G 1 g:c:x+.^19 :c:x+.^19 : [kps] :x+.^19 : [klv] :x+.^19 :c:c:x+.^19
: [dt] :x+.^19
G 2 g:x*.^11 :c:x+.^19 :c:x+.^19 : [gks] :x+.^19 : [dklv] :x+.^19 :c:c:x+.^19
: [dt] :x+.^18
G 3 ((((g:c:x+.^19 )+.^1 :c)+.^12 :p:x+.^18 : [gkns] :x+.^19 : [dgls] :x+.^19
: [iklt] :x+.^19 )+.^1 :c:x+.^17 )+.^1 : [dt] :x+.^19
KI c:x(7) :c:x(6) :y:x(3) :c:x(2,3) :c
G 1 [cv] :x+.^19 : [elrvy] :x+.^19 : ([cps] :x+.^19 : [cgs] :x+.^19 : [cty] :x+.^19
:n:x+.^11 : [ct] :x+.^19 : [ct] :x+.^17 )+.^1
G 2 [ct] :x+.^19 : [celr] :x+.^19 : [elpqr] :x+.^19 : [cgk] :x+.^19 : [dgk] :x+.^19
: [fty] :x+.^18 : [dnr] :x+.^18 :c:x+.^19 :c:x+.^18
G 3 c:x+.^19 : [eir] :x+.^19 : [epr] :x+.^19 :c:x+.^19 : [dgks] :x+.^19
:y:x+.^19 : [cn] :x+.^19 :c:x+.^19
```
(0.19) resulted in the most interesting motifs.
With respect to the results in Table 3, the results obtained range from
acceptable (those with testing percentages less than 70%) to excellent. With
respect to the training results, the best SRE-DNA motif for each family was
produced twice by grammar 1, once by grammar 2, and three times by gram-
mar 3. Often, the difference in probabilities between best motifs by different
grammars was many orders of magnitude. For example, consider the scorpion
toxin experiment (ScT), in which the best grammar 3 solution had an average
probability over 280 times larger than the next best grammar 1 motif, and over
2600 times larger than the grammar 2 motif. Unfortunately, neither the ScT nor
AAO families had enough examples with which to perform testing.
The ZF1 runs were hindered by the small probabilities inherent with the
motifs. This seems to be a product of the large mask size (10) combined with
the large window size (25). It was observed that many recognized probabilities
for examples were in the 1e-12 range. Others may have been smaller than 1e-13,
which was beyond the probability limit for those runs, and thus would have been
terminated.


The Evolution of Stochastic Regular Motifs for Protein Sequences 25

In the ZF2 and KI cases, the observedprfor testing roughly correlate
with the bestprfor training. The ZF2 motifs were simple, and runs often
converged to the identical motif.
The snake toxin testing forG 3 �s motif suggests that this particular motif
may be overtrained. Although it recognized all of the training cases, it could
only recognize 64% of the testing set.
The results in Table 4 indicate that SRE-DNA often performed better
than the PROSITE equivalents with respect to false negative and false positive
sequences. However, this must be considered in balance with the testing results
in Table 3.
Cyclic subsequences are not common in the protein sequences studied
here. This might imply that the iteration operator is detrimental for GP, and
that grammarG 2 would be the most effective variation of SRE-DNA. The results
in Table 3, however, suggest otherwise. Iteration can be argued to be beneficial
for evolution, given the number of best solutions found with grammarsG 1 and
G 3 (the grammars with iteration operators). One hypothesis for this is that
iteration aids evolution by allowing richer intermediate expressions to evolve,
because iteration permits the recognition of greater numbers of positive exam-
ple subsequences. This occurs because an SRE-DNA expression with iteration
recognizes more strings than one without it (ignoring the effects of skip terms,
which are controlled by the use of negative examples). Upon inspecting the
motifs in Tables 5 and 6, however, it is clear that iteration is not an important
factor with respect to the construction of final motifs. In 7 of the 12 experiments
using grammars with iteration (G 1 andG 3 ), the iteration operator did not arise
in the best motifs. In the remaining 5 motifs, iteration is incidental, and takes
the form of intron material to protect useful terms. An obvious example of this
is theG 1 solution for ScT in Table 5. Iteration could be removed from these
expressions, resulting in motifs with higher probability performance.
Considering the structure of SRE-DNA expressions in Tables 5 and 6, it
is clear that majority of evolved solutions are much more complex than the source
PROSITE motifs. One reason for this complexity is the fact that expression size
was not accounted for in the fitness function. Expression parsimony could be
enhanced by incorporating a score for expression complexity, or reducing the
GP parameter limiting tree sizes. Furthermore, evolved GP programs are often
more complex than manually programmed ones.


26 Brian J. ROSS

## � 8 Related work

### 8.1 Other representations: PROSITE, HMMs

SRE-DNA is similar to the regular expressions used by PROSITE and
related databases.6, 12) SRE-DNA is essentially a PROSITE-like language en-
hanced with a probability model. Not surprisingly, many evolved motifs in Fig-
ures 5 and 6 are similar to the PROSITE source motifs. Some of the SRE-DNA
motifs, however, vary substantially from their PROSITE equivalents. This is a
combined result of the methodology used during their acquisition with GP, such
as the fitness evaluation strategy and other GP parameters, and nuances in the
SRE-DNA grammar used.
SRE-DNA�s main expressive advantage over PROSITE�s language is its
probabilistic model. A SRE-DNA motif has associated with it a probability dis-
tribution, and each member sequence has a corresponding probability computed
with it. Non-member sequences have a zero probability. With PROSITE�s lan-
guage, there is no associated probability or scoring scheme; sequences are either
members or not members of the motif expression in question. Although the over-
all structure of the regular expression underlying an SRE-DNA motif is often
just as deterministic as with a PROSITE equivalent, the probability distribution
helps to numerically justify the strength of membership of sequences within a
family. For example, sequences that use longer gaps will have lower probabilities
than those with smaller gaps.
SRE-DNA is more expressive at denoting variable-length terms and gaps
than PROSITE motifs. In PROSITE, a numeric range such as �X(2, 4)� indi-
cates a variable gap between 2 and 4 codons. In SRE-DNA (as used in this
paper), a variable-length gap is expressed as a term like �x+.^10 �. Probabilistic
iteration can permit gaps of any potential length, but at a probabilistic cost,
since long gaps caused by highly-iterated terms have corresponding lower prob-
abilities. A motif that requires a large gap should use an iteration with a higher
iterative probability. SRE-DNA�s use of probabilistic choice also enhances its ex-
pressiveness over PROSITE�s motifs. Although choice was not used here, other
protein families may benefit by it.
A comparison of SRE-DNA with Hidden Markov Models (HMM�s) is
also useful.22)A HMM is a stochastic finite automata. In the HMM model of
Kroghet al., a target sequence of lengthkhas associated with it an HMM
graph (automata) of approximately 3knodes and 9karcs. Nodes are either


The Evolution of Stochastic Regular Motifs for Protein Sequences 27

match states, insert states, or delete states. Match states arise when sequences
match codes of the sequence, delete states cause codes to be skipped, and insert
states introduce codes. Every transition between these states has associated
with it a probability. Codons themselves have specific probabilities when they
are seen when moving between states. All these probabilities can be determined
automatically via various training algorithms. Given a candidate sequence, the
codons invoke translations through the HMM, until the sequence terminates,
resulting in a probability score for that sequence.
HMM�s have shown good performance in protein classification, and have
performed well in comparison to PROSITE�s regular motifs.22)HMM�s are not
intended as user-level interfaces to protein databases, unlike SRE-DNA and
PROSITE motifs. HMM�s are much less rigid than regular pattern motifs, as
their architecture is designed to permit variations of sequence patterns within a
set window size. In comparison, PROSITE and SRE-DNA motifs specify fairly
rigid pattern matching criteria (although SRE-DNA motifs are somewhat more
relaxed due to probabilistic iteration). The fact that HMM�s are more forgiving
with sequence variations than regular pattern motifs is a mixed blessing. On one
hand, an HMM is able to represent a large assortment of structurally dissimilar
sequences, by fitting a probability distribution over them. The HMM archi-
tecture is fixed with respect to the maximum sequence size permitted. Should
the sequences be very dissimilar, however, the resulting information content en-
coded in the HMM�s probability distribution will become increasingly negligible.
PROSITE patterns have a similar ability to permit structure variation through
the use of variable gaps and codon choices. It treats sequence identification as
a boolean operation, and there is no probability associated with membership.
Although SRE-DNA expressions can denote dissimilar sequences with the choice
operator, the resulting motif will grow in size and complexity.
Reconciling the expressive differences between HMM�s, SRE-DNA, and
PROSITE expressions is not straight-forward. There is a 1-to-1 mapping be-
tween regular expressions and finite automata: each can be automatically com-
piled to the other.14) Similarly, there is a mapping between stochastic regular
expressions and stochastic finite automata. This implies that the languages de-
noted by PROSITE, SRE-DNA and HMM motifs are theoretically equivalent.
However, even though these models are inter-translatable, it does not imply
that given sets of protein sequences are always more conveniently denoted in
one model over another. It must be realized that all these motif models are


28 Brian J. ROSS

simple structural characterizations of the real factor of importance � the 3D
topology of the protein.

### 8.2 Motifs and Genetic Programming

The first successful application of genetic programming towards evolv-
ing PROSITE-style motifs for unaligned biosequences is by Hu.15) There are
similarities and differences between Hu�s GP methodology and ours. Similar
to our experiments, Hu pre-specifies the maximum window length, maximum
gap length, and gap flexibility (ranges for variable gaps of the form �X(i, j)�).
Whereas we use amino acid codons, Hu uses a combination of nucleotide and
amino acid codons, as well as predefined codon subset codes. The use of pre-
defined subsets is interesting, as it uses domain knowledge to establish useful
combinations of possible codons for mask terms. Hu seeds the initial popula-
tion with substrings from the example set. Although our initial population was
randomly generated, in hindsight, seeding might prove advantageous.
One major difference between Hu�s approach and ours is his extensive
use of greedy local optimization to refine expressions during evolution. Firstly,
each pattern sub-term is replaced by a wildcard; if the fitness improves, the
wildcard is retained. Then all the legal values for each variable gap are iterated
through, to find the combination giving the highest fitness. Although Hu does
not specify the performance cost of this optimization procedure, it certainly must
be significant. SRE-DNA evolution circumvented the need for gap placement
and variable gap refinement, as the skip fields were automatically refined during
evolution. We applied greedy optimization to mask terms, but only on the best
solution at the end of a run.

```
Table 7 Comparison of PROSITE, Hu, and SRE-DNA motifs for snake toxin (SnT)
PROSITE: g:c:x(1,3) :c:p:x(8,10) :c:c:x(2) : [denp]
Hu #1: g:c:x(1,3) :c:p
Hu #2: c:c:x(1,2) : [denp]
SRE-DNAG 2 : g:x*.^11 :c:x+.^19 :c:x+.^19 : [gks] :x+.^19 : [dklv] :x+.^19
:c:c:x+.^19 : [dt] :x+.^18
```
A qualitative comparison of Hu�s results with ours is difficult, because of
the difference in motif languages, as well as Hu�s lack of any numerical analyses
of his results (eg. training and testing measurements). Hu investigated the same
ZF1, ZF2, SnT, and KI protein families that we did. Hu�s resulting motifs for


The Evolution of Stochastic Regular Motifs for Protein Sequences 29

ZF1 and KI are impressive, as they are nearly identical to the PROSITE motif
expressions. In other results, Hu�s motifs were simpler than the PROSITE ones.
For example, Table 7 compares the PROSITE, Hu, and SRE-DNAG 2 motifs
for SnT. The Hu motifs are clearly too short, as they cover sequences between
4 and 7 codons long, which is far smaller than the maximum window size of 22
codons used in the PROSITE motif. In theG 2 motif, the first 4 terms of the
PROSITE motif (up to �p�) are nearly identically covered, as is the �c:c� term
later in the sequence. TheG 2 motif needs to break up the long �x(8,10)� gap
with a series of iterations and masks, because the iteration limit of 0.19 prevents
a single skip expression from being applied too often (a skip of 0.19 iterated 10
times gives a probability of 5e-8).
The other GP work evolving regular expression motifs for unaligned
sequences is by Koza, Bennett, Andre and Keane.21) The 2 protein families
studied are the D-E-A-D box and manganese superoxide dismutase families.
The target language is a subset of the PROSITE motif language, in which gap
expressions (fixed or variable) are not used. They also use GP with ADF�s, which
enable the modularization of common subexpressions in evolved motifs. This is
advantageous for the protein families studied, which contain repetitive elements.
The two protein families investigated have very small window lengths, of length
8 and 9 respectively. This is considerably smaller than the lengths used in this
paper, which ranged up to 25 codons. They reported good results with their
work, as one of their evolved motifs improved upon an established PROSITE
source motif, due to its replacement of a PROSITE gap with an explicit choice
of amino acid codons.
Unlike ours or Hu�s work, Kozaet al. did not need to supply preset
window limits. Although fewer user-specified parameters implies increased au-
tomation, which is always an ideal in machine learning, a preset window size
was used here out of necessity: an indeterminately large window results in long
interpretation times, especially when iteration is used. Given the complexity of
SRE-DNA and the lengths of the sequences studied here, it is unlikely that an
unspecified window would have been feasible.

## � 9 Conclusion

This is a first attempt at evolving SRE-DNA motifs for unaligned se-
quences, and the results are promising. The expressive characteristics of SRE-
DNA motifs are highly dependent upon language restrictions used, such as gram-


30 Brian J. ROSS

matical restrictions, mask sizes, and iteration ranges. Some of these constraints
may be more suitable to particular protein families than others. Further reseach
is required to understand in more depth the effect of language constraints on
motifs, and which constraints are most practically applicable to the majority of
protein families.
One weakness in this paper�s application of SRE-DNA is that motifs
do not ensure fixed-size sequences for conserved regions. Conserved regions
rarely vary in size, since extra or missing amino acids will cause major changes
to functionality. GP usually evolves appropriate iteration values that produce
good results for the training set. These terms are not fixed in length, however,
if iterative gaps are embedded in them. An interesting direction to take in the
future is to use an enhanced SRE-DNA grammar that explicitly accounts for
conserved regions. For example, the grammar could generate motifs of the form,

```
< var. region > < conserved region > < var. region >
```
The grammar rules for the variable regions would use iterative probabilities as
is done currently, while the conserved region would forgo the use of all iteration
operators.
SRE-DNA is a new motif language for protein sequences. As with
PROSITE motifs, much of the characterisation of a family of proteins is inherent
in the structure of the SRE-DNA motif. Additionally, like HMM�s, an SRE-DNA
motif defines a probability distribution over the member sequences. In this two-
level approach to sequence representation, the more discriminating linguistic
level is the regular expression, since it implicitly defines which sequences belong
to the modeled family. The probability fields refine this representation by as-
cribing a probabiity distribution to member sequences. Like PROSITE�s motif
language, SRE-DNA motifs tend to grow in size in accordance with the com-
plexity and size of the sequences being characterized. GP tends to create larger
expressions as well, which can often be simplified afterwards. SRE-DNA�s motif
size contrasts to HMM motifs, whose structure is fixed for the maximum size of
sequences being represented. Unlike HMM�s, however, SRE-DNA is a relatively
user-friendly motif language, and can be used for interactive database access.
An empirical comparison of SRE-DNA and other representation models
would be interesting. Since these different motif representations can vary sub-
stantially, however, a direct comparison of them may not be too fruitful. It is
worth realizing that the biological functionality of proteins is entirely dependent
upon the 3D structure of the protein molecules. Motif representations such as


The Evolution of Stochastic Regular Motifs for Protein Sequences 31

SRE-DNA, HMM�s, regular expressions, and context-free languages, are crude
means for modeling characteristic features of proteins. Nevertheless, they are
widely used, because they are relatively concise, computationally efficient, and
amenable to automatic acquisition.
There are other directions for future investigations. Similar to the mask
refinement performed on the final solutions, additional refinement of probability
fields can be undertaken, which would improve the probability performance of
motifs. Future work will investigate using a fuller variant of SRE-DNA to model
sequences with repetitive elements and weighted choice. More sophisticated GP
approaches, for example, the use of multiple populations, may result in better
evolution performance. Finally, more runs should be undertaken to obtain more
meaningful statistical results. A practical limitation to this is the speed of runs,
which can be very slow when large masks and low probability limits are used.
Porting DCTG-GP to a faster, compiled language such as C would be useful in
this regard.
Our fitness scheme uses a strictly structural perspective of motif perfor-
mance: if a regular language can recognize the positive sequences in a family,
and reject sequences outside of it, then it is considered correct. This formal view
of motif performance is practical and adequate for many problems, and therefore
is adopted in most machine learning applications. On the other hand, such an
approach is inherently limited. For example, it cannot account for converged
functionality for distantly related proteins. Such real-world phenomena require
additional information (domain-specific knowledge about protein structure) be-
yond what can be modeled by first-order structural principles themselves.
A comparison of different motif learning techniques is worth undertaking
in the future. SRE-DNA motifs may be synthesizable by other machine learning
algorithms, many of which may yield superior performance over GP.

Acknowledgement: The author thanks two anonymous referees for their helpful
comments. This work is supported though NSERC Operating Grant 138467-
1998.

## References

```
1) H. Abramson and V. Dahl.Logic grammars. Springer-Verlag, 1989.
2) S. Arikawa, S. Miyano, A. Shinohara, S. Kuhara, Y. Mukouchi, and T. Shino-
hara. A Machine Discovery from Amino Acid Sequences by Decision Trees over
Regular Patterns. New Generation Computing, 11:361�375, 1993.
```

32 Brian J. ROSS

```
3) P. Baldi and S. Brunak.Bioinformatics: the Machine Learning Approach. MIT
Press, 1998.
4) W. Banzhaf, P. Nordin, R.E. Keller, and F.D. Francone.Genetic Programming:
An Introduction. Morgan Kaufmann, 1998.
5) P. Bork and E.V. Koonin. Protein sequence motifs.Current Opinion in Struc-
tural Biology, 6:366�376, 1996.
6) A. Brazma, I. Jonassen, I. Eidhammer, and D. Gilbert. Approaches to the
Automatic Discovery of Patterns in Biosequences. Journal of Computational
Biology, 5(2):279�305, 1998.
7) A. Brazma, I. Jonassen, J. Vilo, and E. Ukkonen. Pattern Discovery in Biose-
quences. pages 255�270. Springer Verlag, 1998. LNAI 1433.
8) P. Bucher and A. Bairoch. A Generalized Profile Syntax for Biomolecular
Sequence Motifs and its Function in Automatic Sequence Interpretation. In
R.Altmanet al, editor,Proceedings 2nd International Conference on Intelligent
Systems for Molecular Biology, pages 53�61. AAAI Press, 1994.
9) V.K. Garg, R. Kumar, and S.I Marcus. Probabilistic Language Formalism for
Stochastic Discrete Event Systems. IEEE Trans. Automatic Control, 44:280�
293, February 1999.
10) S. Handley. Automated Learning of a Detector for the Cores ofa-Helices in
Protein Sequences Via Genetic Programming. InProceedings 1994 IEEE World
Congress on Computational Intelligence, volume 1, pages 474�479. IEEE Press,
1994.
11) S. Handley. Classifying Nucleic Acid Sub-Sequences as Introns or Exons Using
Genetic Programming. InProceedings 3rd International Conference on Intel-
ligent Systems for Molecular Biology (ISMB-95), pages 162�169. AAAI Press,
1995.
12) K. Hofmann, P. Bucher, L. Falquet, and A. Bairoch. The PROSITE database,
its status in 1999. Nucleic Acids Research, 27(1):215�219, 1999.
13) J.H. Holland. Adaptation in Natural and Artificial Systems. MIT Press, 1992.
14) J.E. Hopcroft and J.D. Ullman. Introduction to Automata Theory, Languages,
and Computation. Addison Wesley, 1979.
15) Y.-J. Hu. Biopattern Discovery by Genetic Programming. In J.R. Kozaet al,
editor,Proceedings Genetic Programming 1998, pages 152�157. Morgan Kauf-
mann, 1998.
16) K. Karplus, K. Sjolander, C. Barrett, M. Cline, D. Haussler, R. Hughey,
L. Holm, and C. Sander. Predicting protein structure using hidden Markov
models. Proteins: Structure, Function, and Genetics, pages 134�139, 1997.
supplement 1.
17) M.J. Kearns and U.V. Vazirani. An Introduction to Computational Learning
Theory. MIT Press, 1994.
18) J.R. Koza.Genetic Programming: On the Programming of Computers by Means
of Natural Selection. MIT Press, 1992.
19) J.R. Koza. Automated Discovery of Detectors and Iteration-Performing Calcu-
lations to Recognize Patterns in Protein Sequences Using Genetic Programming.
InProceedings of the Conference on Computer Vision and Pattern Recognition,
pages 684�689. IEEE Press, 1994.
```

The Evolution of Stochastic Regular Motifs for Protein Sequences 33

```
20) J.R. Koza, F.H. Bennett, and D. Andre. Classifying Proteins as Extracellular
Using Programmatic Motifs and Genetic Programming. InProceedings 1998
IEEE World Congress on Computational Intelligence, pages 212�217. IEEE
Press, 1998.
21) J.R. Koza, F.H. Bennett, D. Andre, and M.A. Keane. Genetic Programming
III: Darwinian Invention and Problem Solving. Morgan Kaufmann, 1999.
22) A. Krogh, M. Brown, I.S. Mian, K. Sjolander, and D. Haussler. Hidden Markov
Models in Computational Biology. Journal of Molecular Biology, 235:1501�
1531, 1994.
23) B.J. Ross. Probabilistic Pattern Matching and the Evolution of Stochastic
Regular Expressions. International Journal of Applied Intelligence, 13(3):285�
300, November/December 2000.
24) B.J. Ross. Logic-based Genetic Programming with Definite Clause Translation
Grammars. New Generation Computing, 2001. In press.
25) B.J. Ross. The Evaluation of a Stochastic Regular Motif Language for Pro-
tein Sequences. In L. Spectoret al., editor,Proceedings of the Genetic and
Evolutionary Computation Conference (GECCO-2001), pages 120�128. Morgan
Kaufmann, 2001.
26) Y. Sakakibara, M. Brown, R. Hughey, I.S. Mian, K. Sjolander, R.C. Underwood,
and D. Haussler. Stochastic Context-Free Grammars for tRNA Modeling. Nu-
cleic Acids Research, 22(23):5112�5120, 1994.
27) D.B. Searls. The Computational Linguistics of Biological Sequences. In
L. Hunter, editor,Artificial Intelligence and Molecular Biology, pages 47�120.
AAAI Press, 1993.
28) D.B. Searls. String Variable Grammar: a Logic Grammar Formalism for the
Biological Language of DNA. Journal of Logic Programming, 24(1,2), 1995.
```


