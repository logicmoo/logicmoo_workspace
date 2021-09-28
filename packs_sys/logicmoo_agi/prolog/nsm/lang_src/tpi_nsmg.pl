:- include('../bin/operators.pl').

(eng:e) gtitle "Tok Pisin NSM Grammar".

gauthor "Francesco Zamblera".
gdate "2009".
gversion "0.8".
gackn "Carol Priestley".

(eng:e) 
gabstract 
"This grammar is an attempt to a computational treatment \
of the Tok Pisin Natural Semantic Metalanguage. It is still \
a preliminary version, and feedback is needed to improve the
automatically generated Tok Pisin.".

(   eng:e)
gprologue
"This Tok Pisin NSM computational grammar is the second module \
to have been written for NSM-DALIA after the English one. It is \
based largely on Carol Priestley's work on Tok Pisin NSM. I \
here thank Dr. Priestley for having kindly sent me her unpublished \
material on Tok Pisin semantic primes. Any inadequacy in the resulting
grammar is of course my full responsibility.

This grammar exemplifies a literal programming approach to grammar \
writing, with source code and comments interspersed. The \"print grammar\" \
command of NSM-DALIA collects the comments, with a formatted version of the \
source code, and generates a printable booklet. This file has been generated
from \"tpi@_nsmg.pl\", the source code of the Tok Pisin grammar. 
I have only added the seventh section (\"Bibliography\"), but the next 
version of NSM-DALIA will do this automatically. 
You can compare the source with the generated rtf to see how it works.

This grammar also shows the flaws and limits of the current format: 
the final output of a sentence-processing session must be a NSM-PROLOG \
formula, but, at the intermediate stages, the sentence representation \
can be everything we want, provided it has the form ct(Category,Structure). \

This lets in whatever hack we want (you will see many in this grammar), and 
renders grammars difficult to write and read, and theoretically dubious for 
their unrestrictedness. As you will see comparing this booklet with 
the source code, writing a NSM-PROLOG grammar is still too 
much an exercise in PROLOG programming.

I am currently developing a new grammar format which will be much more \
restricted and far easier to read and write. 
NSM-DALIA version 0.9 (the one currently released) can already
automatically detect which version of the grammar is used, and 
adapt the resulting NSM-PROLOG formula, so old grammars will
not have to be rewritten in the new format.".



synt_grammar_type(dependency).
morph_grammar_type(dependency).

(eng:e)
glevels 
"Dependencies are parted into _levels_, and the parser tries to apply 
lower-level dependencies first. Consider, for example, the following
string:

\"do something good\" 

We probably want the parser to recognise first the dependency between 
\"something\" and \"good\", and only then the one between \"something 
good\" and \"do\". We will obtain this by assigning the \"verb-object\" 
dependency to a higher level.

I have tentatively recognised the following levels:
".

morph_threshold(9).

dep_threshold(1,100). % NP
dep_threshold(2,150). % PP e NP con genitivo
dep_threshold(3,300). % VP
dep_threshold(4,370). % CLAUSE
dep_threshold(5,400). % COMPLEX SENTENCES

max_dep_threshold(5).


/* TAGMEMIC
morphology.

v(Type) === [h:vr(Type)/[v:V]] <> [v:V].

v(t) === [h:v(i)/[v:V],vt:vt/make] <> [v:make(V)].

n === [h:n/[n:N]] <> [n:N].

d === [h:d/[d:D]] <> [d:D].

syntax.
*/




(eng:e) gtranscr
"Tok Pisin NSM grammar needs no special transcription system, so
no transcription table is provided. You can see an example of 
transcription table in the English module.".




(eng:e) gmorph
"The morphology section in an NSM-PROLOG grammar consists of three \
parts: a series of _allomorph construction rules_, a statement of _permissible morpheme sequences_, and that subset of dependency rules which refers to dependencies between _morphemes_.

As Tok Pisin needs no allomorph construction rules, \
the first subsection is empty. As for the second subsection, \
there is a default rule (1. @*), which simply states that any morpheme can \
constitute a word of its own, and then a rule which permits _reduplication_ and
_-im_ morpheme. NSM grammar needs these word forms to distinguish between \
allolexes of THINK and SAY (_ting_, _tingting_, _tingim_ and, 
respectively, _tok_, _toktok_ and _tokim_).

The _morpheme sequence specification_ number 2 reads as follows: \" a
word can be consist of a verb stem, optionally followed by (a copy of)
itself, optionally followed by the *trans* suffix (_-im_)\" ".






(eng:e) gdep_intro 
"The bulk of an NSM-PROLOG grammar consists of 
*dependency rules*. The source code of these rules is rendered into tables
by the grammar formatter. Each rule is displayed as follows:

1. A header, which shows the rule number, followed by the formula _A+B=C_
if the rule is directional, or _A+B=C // B+A=C_, if the dependants can
show up in both linear orders;

2. then, a three-column table shows the structures of, respectively, 
_A_, _B_ and _C_.

3. If variables in the rules are bound by _conditions_ (paradigms),
these conditions are shown just below the tables.

4. Then, if the author has added some textual comment to a rule, it is 
displayed.

5. The author can provide two example lists for each rule: an
_analysis_ and a _translation_ list. Examples in
each list are processed, and the results are tabulated after textual
comment.

Now all the rules of the Tok Pisin grammar follow, first the 
morphological dependencies, then, in the next section, the syntactic ones.
".


/*
DEPENDENCIES
*/



% -im
3 dr r_doc "Suffix _-im_ added to verb stems acts often as a \
transitivizer, modifying their argument list. The trans/4 paradigm \
relates the verb V with its equivalent _V-im_ (you can find the code 
in the _p ::: trans_ section of the grammar). The NSM exponents \
_tok_ and _ting_ have allolexes _tokim_ and _tingim_ in \
transitive syntactic configuration (see Priestley 1999a, p. 9, 1999b, p. 2-4)".
3 dr ct(v(T),p(V,ARGLIST)) + ct(trans,trans) ==>
     ct(v(T),p(V1,ARGLIST1)) //
     [trans(V,V1,ARGLIST,ARGLIST1)].
3 dr r_a ["ting", "tingim", "tok", "tokim"].

% trans(V,V1,ARGLIST,ARGLIST1)
p ::: trans(think,think,[e:e],[e:e,o:e]).
p ::: trans(want,want,[e:e],[e:e,o:e]).
p ::: trans(say,say,[a:e],[a:e,t:e]).



% reduplication
7 dr r_doc "The NSM exponents _tok_ and _ting_ show a reduplicated \
allomorph (_toktok_ and _tingting_) in specific syntactic \
configurations (see Priestley 1999a, p. 9, 199b, p. 2-4).

The rule works by recognising to identical instances of a verb 
following each other.

This Tok Pisin NSM grammar needs no more morphological rules. From now on, 
all the listed dependencies are syntactical.".
7 dr ct(v(T),p(V,ARGLIST)) + ct(v(T),p(V,ARGLIST))  ==>
     ct(v(T),p(V1,ARGLIST1)) //
     [redupl(V,V1,ARGLIST,ARGLIST1)]. 
7 dr r_a ["tok", "toktok", "ting", "tingting"].

% redupl(V,V1,ARGLIST,ARGLIST1).
p ::: redupl(think,think,[e:e],[e:e,o:e,t:e]).
p ::: redupl(say,say,[a:e],[a:e,o:e,d:e,t:e]).



% compound nouns
11 dr r_doc "This rule recognises _compound nouns_ as *haus mani* 
\"bank\".".
11 dr ct(n(n),sp(e,e,e,[],N1)) + ct(n(n),sp(e,e,e,[],N2)) 
				   ==> ct(n(n),sp(e,e,e,[],N3)) 
				   // [compound(N1,N2,N3)].
11 dr r_a ["haus mani", "pikinini meri"].
p ::: compound(somewhere(house),something(money),somewhere(bank)).
p ::: compound(part,something(water),somewhere(river)).
p ::: compound(someone(child),someone(woman),someone(girl)).





% compound adverbs
13 dr r_doc "Rules 13 to 15 form compound lexical items: *longtaim 
liklik* _for some time_, *long wanem* _because_, and the conjunction 
*olsem na* _as_".
13 dr ct(adv(dur),long) + ct(a(a),small) ==> ct(adv(dur),some).
13 dr r_a ["longtaim liklik"].

14 dr ct(p,long) + ct(n(q),what) ==> ct(conj,because).
14 dr r_a ["long wanem"].

15 dr ct(p,like) + ct(conj,and) ==> ct(conj,like).
15 dr r_a ["olsem na"].



% BIN
16 dr r_doc "This rule recognises *bin* + _verb_ as a construction 
marking anterior time.".
16 dr ct(tma,bin) + ct(v(T),p(V,ARGS)) ==> ct(v(T), p(pf(V),ARGS)).
16 dr r_a ["bin wokim", "bin go", "bin stap", "bin harim", "bin lukim"].




% NEGATION
19 dr r_doc "Negation rule: *no* + _verb_ -->  _negative verb_.".
19 dr ct(neg,not) + ct(v(T),p(V,A)) ==> ct(v(T),p(neg(V),A)).
19 dr r_a ["no wokim", "no stap", "no bin wokim", "no bin lukim"].




% i + VERB
20 dr ct(i,i) + ct(v(T),p(V,[Role:e|Args])) ==>
      ct(v(T),p(V,[Role:3|Args])).
20 dr r_doc
"When the subject is a third person nominal (that is, \
when it is different from _I_ or _you_), the predicate is \
preceded by the agreement particle *i*".
20 dr r_a ["i stap", "stap", "i wokim", "wokim", "i bin wokim", "i bin lukim",
	  "i no stap", "i no bin wokim", "i no bin harim"].





% go + come --> move
22 dr r_doc "The prime MOVE is expressed by a compound of *go* and *kam*. 
(Priestley 1999b pag. 7).

This rule has a higher level number than the \"i+verb\" dependency 
in order for it to recognise combination as *i go i kam*. The 
following rule (23) recognises the negative version of MOVE.". 
22 dr ct(v(T),p(go,A)) + ct(v(T),p(come,A)) ==> ct(v(T),p(move,A)).
22 dr r_a ["go kam", "i go i kam"].
22 dr r_t ["mi go kam", "em i go i kam", "dispela samting i go i kam"].


23 dr ct(v(T),p(neg(go),A)) + 
      ct(v(T),p(neg(come),A)) ==> 
      ct(v(T),p(neg(move),A)).
23 dr r_a ["em i no go i no kam"].




% V i stap
25 dr r_doc "This rule recognises _verb_ + *i stap* as a construction marking 
imperfective (progressive) aspect. The form of the B part of the rule 
depends on the fact that *i stap* has already been recognised 
by previous rules as the third-person form of the verb *stap* LIVE.".
25 dr ct(v(T),p(V,A)) + ct(v(v), p(live, [a:3, c:e])) ==> ct(v(T),p(i(V),A)).
25 dr r_t ["em i lukim mi i stap", "dispela man i wokim samting i stap"].




% VERY
30 dr r_doc "Rules 30 to 32 recognise the combinatory properties 
of *tumas* and *tru* (both VERY). Both Priestley 2008 p. 278 and 
Priestley 1999b, p. 15, give *tumas* as equivalent of _very_; I 
tentatively include *tru* for the expressin *bipo tru* _a long time before_".
30 dr ct(adv(time(tr)),time(e,P,R)) 
    + ct(adv,very(_)) ==> 
    ct(adv(time(tr)),time(much,P,R)).

31 dr ct(a(T),A) + ct(adv,very(_)) ==> ct(a(T),very(A)).
31 dr r_a ["gutpela tumas", "bikpela tumas"].

32 dr ct(adv(T),A) + ct(adv,very(_)) ==> ct(adv(T),very(A)).





% compound preposition(antap long, insait long etc.)
35 dr r_doc "Spatial adverbs as *antap* _above_, *insait* _inside_, \
become spatial preposition when followed by the all-purpose \
preposition *long*".
35 dr ct(adv(l(rel)),ADV) + ct(p,long) ==> ct(p,ADV).
35 dr r_a ["antap long", "klostu long", "insait long", "ananit long"].




% bipo bipo
36 dr r_doc "Tentative rule for reduplicated time adverbials lie *bipo bipo*.".
36 dr ct(adv(time(tr)), time(A,B,C)) + 
      ct(adv(time(tr)), time(A,B,C)) ==>
      ct(adv(time(tr)), time(emph,A,B,C)).



% say so (tok olsem: "...")
50 dr "*tok* + *olsem* followed by quoted material, equivalent 
to English NSM _say like this: ..._.".
50 dr ct(v(v),p(say,A)) + ct(p,like) ==> ct(v(v),p(say_so,A)).
50 dr r_a ["em i tok olsem"].




% ATTR
71 dr r_doc
"This grammar partitions adjectives into two subclasses, according \
to whether they precede (class *a(a)*) or follow (class *a(p)*) the head noun. Most adjectives precede. Examples from Priestley 1999a,  p. 8".
71 dr ct(a(a),A) + ct(n(T),sp(e,e,e,Attr,N)) ==> ct(n(T),sp(e,e,e,[A|Attr],N)).
71 dr r_a ["gutpela samting", "bikpela samting", "liklik samting"].

72 dr r_doc "Dependency rule for adjectives which follow the head noun.".
72 dr ct(n(T),sp(e,e,e,Attr,N)) + ct(a(p),A) ==> ct(n(T),sp(e,e,e,[A|Attr],N)).
72 dr r_a ["samting nogut", "ples nogut"].




%ALT
78 dr r_doc "Rule for the prime *narapela* OTHER.".
78 dr ct(alt,ALT) + ct(n(T),sp(e,e,e,Attr,N)) ==> ct(n(T),sp(_,ALT,e,Attr,N)).
78 dr r_a ["narapela samting", "dispela narapela man", "narapela mamneri"].
78 dr r_t ["em i stap long narapela ples", "mi bin wokim narapela samting"].




%NUM (other than "one")
80 dr r_doc "Dependency rule for numerals other than *wanpela* _one_
 in a substantive phrase. For *wanpela*, cf. rule 82".
80 dr ct(num,NUM) + ct(n(_),sp(e,ALT,e,A,N)) ==> ct(n(n),sp(e,ALT,num(NUM),A,N)).
80 dr r_a ["wanpela man", "tupela man", "planti man", "olgeta man"].




%NUM ("one")
82 dr r_doc "As *wanpela* represents both numeral ONE and substative SOMEONE, 
it is given this special rule. The two primes are easily distinguished 
syntactically: when *wanpela* means SOMEONE, it is the head of the 
substantive phrase, while, in its numeral meaning, it is followed 
by a substantive-head.".
82 dr ct(n(d),sp(e,e,e,[],someone(person))) +
      ct(n(n),sp(e,ALT,e,A,N)) 
      ==> ct(n(n),sp(e,ALT,num(one),A,N)).
82 dr r_a ["wanpela pikinini", "wanpela man", "wanpela ples"].





% ol
84 dr r_doc "This rule probably belongs to an extention of NSM rather than 
to NSM proper. *ol* is the plural marker in a substantive phrase. 
As English NSM often uses expressions like HE IS DOING THINGS, 
SUCH THINGS, I tentatively add a rule for *ol* to match English plural. 
Of course, if the English expressions are elliptical (the prime SOME remains 
unexpressed), then we can do without *ol* in Tok Pisin NSM.".
84 dr ct(q(1),plur) + 
      ct(n(n),sp(e,Alt,e,[],H)) ==>
      ct(n(n),sp(e,Alt,num(plur),[],H)).
84 dr r_t ["dispela samting i gutpela", "dispela ol samting i gutpela"].




%DET
85 dr r_doc "Determiner in a substantive phrase. This rule exemplifies the 
important built-in condition lexical(Category,Meaning), which checks 
if there is such a lexical entry before applying the rule. 

One of the main cause of errors in writing rules for the generator is 
_left recursion_. If you look at the source code for rule 85, you see 
that it could be matched vacuously _ad infinitum_, as the output of the 
rule in generation (B part of A+B) still matches the input (C), 
because nothing prevents the empty predicate *e* to be matched 
against the variable *D*. 

Therefore, A and B parts of a dependency rule *must always be
such that they do not match part C any more*.

If a rule must be written that does not meet this restriction, a check must 
be made to avoid sending the generator into an infinite loop.

In generation, this rule produces a lexical item (the determiner), and 
a \"less instantiated\" substantive phrase, which has been stripped 
of the determiner. The condition that will prevent further vacuous 
application of the rule is simply that _there be such lexical item_ as
m(d,PhoneticForm,Meaning). As there is no such entry as 
m(d,\"...\",e), the rule is stopped.".
85 dr ct(d,D) + 
   ct(n(_),sp(e,ALT,NUM,ATTR,N)) ==> 
   ct(n(n),sp(D,ALT,NUM,ATTR,N)) //
  [lexical(d,D)].
85 dr r_a ["dispela samting", "dispela man", "dispela wanpela"].



108 dr r_doc "This rule recognises *laik* + *long* as the construction 
for non-equi WANT in Tok Pisin.".
108 dr ct(v(v),p(want,[e:e])) + ct(p,long) ==>
       ct(v(v),p(want,[e:e,o:p])). 
108 dr r_a ["laik", "laik long"].
108 dr r_t ["mi laik wokim samting", "mi laik long yu wokim samting"].




% bai + verb
109 dr r_doc "The future-irrealis marker *bai*. Its syntax is more 
unrestricted thant that of the marker *bin*: while *bin* immediately 
precedes the verb, *bai* can precede the verb or stay at the beginning of 
the sentence.".
109 dr ct(tma,bai) + ct(v(T),p(V,ARGS)) ==> ct(v(T), p(fut(V),ARGS)).
109 dr r_a ["bai go", "bai wokim"].
109 dr r_t ["samting i kamap", "samting i bin kamap", 
	    "bai samting i kamap", "samting bai i kamap"].



% PP
110 dr r_doc "Preposition + Substantive Phrase ---> 
Prepositional phrase. This rule belongs to the second level, so it will 
be tried (in parsing) after all the substantive-phrase-building rules 
have been attempted.".
110 dr ct(p,P) + ct(n(_T),sp(A,B,C,D,E)) ==> ct(pp(P),sp(A,B,C,D,E)).
110 dr r_a ["long dispela manmeri", "wantaim yu", "bilong mi"].



111 dr r_doc "Substantive use of *dispela* THIS. When the parser is trying to 
apply this rule, all first-level rules have been tried. If *dispela* had 
been part of a substantive phrase, this would already have been recognised. 
Therefore, a preposition can combine with a \"stranded\" THIS to form 
a prepositional phrase. Rule 112 is a particular case -- the prime 
LIKE THIS, formed, as in English, by *olsem* + *dispela*
(Cf. Priestley 2008, pp. 282, 285, 291)".
111 dr ct(p,P) + ct(d,this) ==> ct(pp(P),sp(this,e,e,[],something(fact))).
111 dr r_a ["wantaim dispela", "long dispela", "bilong dispela"].


% olsem dispela : like this
112 dr ct(p,like) + ct(d,this) ==> ct(pp(like),this).
112 dr r_a ["olsem dispela"].




% NP + PP
120 dr r_doc "Prepositional phrases used as modifiers of substantive 
phrases. Cf. Presteley 1999a p.8".
120 dr ct(n(T),sp(DET1,ALT1,NUM1,A1,H1)) 
     + ct(pp(REL),SP) ==>
       ct(n(T),sp(DET1,ALT1,NUM1,[rel(REL,SP)|A1],H1)) //
       [np_adj(REL),
        not_pn(T)].
120 dr r_a ["em i samting bilong mi", "manmeri bilong dispela ples"].

p ::: np_adj(gen).
p ::: np_adj(above).
p ::: np_adj(below).
p ::: np_adj(inside).
p ::: np_adj(like).

p ::: not_pn(d).
p ::: not_pn(n).





% kain + N
121 dr r_doc "Dependency rule for prime KIND OF.".
121 dr ct(n(n),sp(DET1,ALT1,NUM1,A1,kind)) 
     + ct(n(_),sp(e,e,e,[],H)) ==>
       ct(n(n),sp(DET1,ALT1,NUM1,[rel(kind,e)|A1],H)).
121 dr r_a ["kain samting", "kai ples", "narapela kain samting"].



123 dr r_doc "Rules 123 and 124: TASOL after a substantive phrase is interpreted as 
a further determiner (to be reviewed).".
123 dr ct(n(T),sp(D,A,B,C,H)) + ct(adv,only) ==> ct(n(T),sp(only(D),A,B,C,H)).
123 dr r_a ["dispela samting tasol"].

124 dr ct(pp(T),sp(D,A,B,C,H)) + ct(adv,only) ==> ct(pp(T),sp(only(D),A,B,C,H)).
124 dr r_a ["bilong dispela samting tasol"].





% VP Verb + Object
 
204 dr r_doc "After substantive phrases and prepositional phrases have been 
recognised, the verb phrase is then attempted. The first rule of the third 
level links a verb with a following substantive phrase in a _verb-object_  
dependency. Translation examples from Priestley 1999a p. 8.

The object must be the _first_ substantive phrase after the verb, hence the 
condition that all other slots in the argument list be empty at the point an 
object is recognised.".
204 dr ct(v(T),p(V,[R:S,o:e|A])) 
     + ct(n(_),sp(AA,B,C,D,E))
     ==> ct(v(T),p(V,[R:S,o:sp(AA,B,C,D,E)|A])) //
     [all_empty(A)].
204 dr r_a ["lukim mi", "harim yu", "wokim gutpela samting"].
204 dr r_t ["mi wokim gutpela samting", 
	    "mi laikim samting",
	    "mi tingim samting",
	    "em i lukim mi"
	    ].

p ::: all_empty([]).
p ::: all_empty([_:e]).
p ::: all_empty([_:e,_:e]).
p ::: all_empty([_:e,_:e,_:e]).
p ::: all_empty([_:e,_:e,_:e,_:e]).
p ::: all_empty([_:e,_:e,_:e,_:e,_:e]).




203 dr r_doc "Object of *tokim* SAY. If the object is animate (if a \"someone\"), 
it is interpreted as the _dative_ argument (SAY SOMETHING *TO SOMEONE*); if 
the object is _something_ or _words_, it is the complement (SAY *SOMETHING*).

This rule recognises the first case; rule 204 the second.".
203 dr ct(v(v),p(say,[a:S,t:e])) 
     + ct(n(_),sp(AA,B,C,D,E))
     ==> ct(v(v),p(say,[a:S,o:e,d:sp(AA,B,C,D,E),t:e])) //
     [is_animate(E)].

204 dr ct(v(v),p(say,[a:S,t:e])) 
     + ct(n(_),sp(AA,B,C,D,E))
     ==> ct(v(v),p(say,[a:S,o:sp(AA,B,C,D,E),d:e,t:e])) //
     [not(is_animate(E))].
204 dr r_t ["em i bin tokim dispela samting", "em i bin tokim dispela manmeri"].

p ::: is_animate(d(_)).
p ::: is_animate(people).
p ::: is_animate(someone(_)).




207 dr r_doc "MANY (*planti*) used alone as object (cf. Priestley 1999a p. 11-12). 
To be reviewed.". 
207 dr ct(v(T),p(V,[R:S,o:e|A])) 
     + ct(num,many)
     ==> ct(v(T),p(V,[R:S,o:sp(e,e,num(many),[],something(thing))|A])).
207 dr r_t ["mi save planti long yu"].




208 dr r_doc "\"Standalone\" THIS used as object. It is surely a flaw 
of this kind of representation (SN-PROLOG) that so many separate rules for 
substantive and pronominal objects must be written.".
208 dr ct(v(T),p(V,[R:S,o:e|A])) 
     + ct(d,this)
     ==> ct(v(T),p(V,[R:S,o:sp(this,e,e,[],something(fact))|A])).
208 dr r_t ["mi save dispela", "em i bin wokim dispela", "mi lukim dispela"].




209 dr r_doc "This rule and the following recognise the expression *stap gut* 
(be well) which is used in the translation of the prime combination 
FEEL SOMETHING GOOD (cf. Priestley 199a, p. 13-16). The final expression 
(*bel bilong me i stap gut* = I feel something good) is recognised by 
a idiom/2 predicate, which is a tentative add-on to the NSM-PROLOG engine. 
You can see how it looks in the source code.".

209 dr ct(v(v), p(live, [a:S, c:e])) + ct(r(a),good) ==>
       ct(v(stat),p(be_good,[e:S])).
209 dr ct(v(v), p(live, [a:S, c:e])) + ct(r(a),very(good)) ==>
       ct(v(stat),p(be_very_good,[e:S])).
209 dr r_a ["mi stap gut"].
209 dr r_t ["bel bilong mi i stap gut"].







% VP verb + complement
210 dr r_doc "Rules 210 to 215 link the verb to its prepositional complements. 
This is also an area for further improvements in the NSM-PROLOG notation.".
210 dr ct(v(T),p(V,[Subj,R:e|A])) 
     + ct(pp(R1),sp(AA,B,C,D,E)) ==>
     ct(v(T),p(V,[Subj,R:sp(AA,B,C,D,E)|A])) //
     [macro_role(R1,E,R)].


212 dr ct(v(T),p(V,[S,O,R3,R:e|A])) 
     + ct(pp(R1),sp(AA,B,C,D,E)) ==>
     ct(v(T),p(V,[S,O,R3,R:sp(AA,B,C,D,E)|A])) //
     [macro_role(R1,E,R)].
212 dr r_t ["em i wokim gutpela samting wantaim mi"].


213 dr ct(v(T),p(V,[S,O,R:e|A])) 
     + ct(pp(R1),sp(AA,B,C,D,E)) ==>
     ct(v(T),p(V,[S,O,R:sp(AA,B,C,D,E)|A])) //
     [macro_role(R1,E,R)].
213 dr r_t ["em i wokim gutpela samting long mi"].

214 dr ct(v(T),p(V,[S,O,R3,R4,R:e|A])) 
     + ct(pp(R1),sp(AA,B,C,D,E)) ==>
     ct(v(T),p(V,[S,O,R3,R4,R:sp(AA,B,C,D,E)|A])) //
     [macro_role(R1,E,R)].
214 dr r_t ["em i wokim samting wantaim dispela samting"].


215 dr ct(v(T),p(V,[S,O,R3,R4,R5,R:e|A])) 
     + ct(pp(R1),sp(AA,B,C,D,E)) ==>
     ct(v(T),p(V,[S,O,R3,R4,R5,R:sp(AA,B,C,D,E)|A])) //
     [macro_role(R1,E,R)].





250 dr r_doc "Rules 250 to 264  are another area for future improvements. 
These 
rules recognise subject-predicate dependencies when 
the predicate is a substantive, and adjective and an adverb, and link 
the inflectional particle *i* and sentence negator *no* to these predicates.".

250 dr ct(i,i) + ct(neg,not) ==> ct(i,not).
250 dr r_a ["i no"].

251 dr ct(i,I) 
     + ct(n(_),N) ==>  ct(v(stat),p(BE_OR_NOT_BE,[j:3,j:N])) //
     [i_no(I,BE_OR_NOT_BE)].

p ::: i_no(i,be).
p ::: i_no(i,neg(be)).

251 dr r_a ["i no gutpela samting"].
251 dr r_t ["dispela samting i no gutpela samting"].


252 dr ct(i,i) 
     + ct(a(_),A) ==>  
     ct(v(stat),p(A,[o:3|Add])) // [good_for(A,Add)].
252 dr r_t ["dispela samting i gutpela"].

253 dr ct(i,not) 
     + ct(a(_),A) ==>  
     ct(v(stat),p(neg(A),[o:3|Add])) // [good_for(A,Add)].
252 dr r_t ["dispela samting i no gutpela"].


254 dr ct(i,i) 
     + ct(pp(P),N) ==>  
     ct(v(v),p(be,[j:3,l:loc(P,N)]))
     // [locative(P)].
254 dr r_a ["i antap long dispela narapela samting"].
254 dr r_t ["dispela samting i antap long dispela narapela samting"].

255 dr ct(i,i) 
     + ct(pp(like),N) ==>  
     ct(v(v),p(like,[j:3,o:N])).
255 dr r_a ["i olsem dispela narapela samting"].
255 dr r_t ["dispela samting i olsem dispela narapela samting"].


256 dr ct(i,not) 
     + ct(pp(P),N) ==>  
     ct(v(v),p(neg(be),[j:3,l:loc(P,N)]))
          // [locative(P)].
256 dr r_a ["i no ananit long dispela narapela samting"].

257 dr ct(i,not) 
     + ct(pp(like),N) ==>  
     ct(v(v),p(neg(like),[j:3,o:N])).
257 dr r_a ["i no olsem dispela narapela samting"].

258 dr ct(neg,not) 
     + ct(n(_),N) ==>  ct(v(stat),p(neg(be),[j:e,j:N])).
258 dr r_t ["mi no samting"].

259 dr ct(neg,not) 
     + ct(a(_),A) ==>  
     ct(v(stat),p(neg(A),[o:e|Add])) // [good_for(A,Add)].
259 dr r_t ["mi no bikpela"].


260 dr ct(neg,not) 
     + ct(pp(P),N) ==>  
     ct(v(v),p(neg(be),[j:e,l:loc(P,N)])).
260 dr r_t ["mi no antap long yu"].



% i + NP
261 dr r_doc "*i* + _NP_ (COPULA).".
261 dr ct(i,i) + ct(n(_),N) ==> 
       ct(v(stat),p(be,[j:3,j:N])).
261 dr r_a ["em i bikpela man"]. 

% i + AP
262 dr r_doc "*i* + _A_ (COPULA). Ex. _Priestley_ p.8.".
262 dr ct(i,i) + ct(a(_),A) ==> 
       ct(v(stat),p(A,[o:3|Add])) // [good_for(A,Add)].
262 dr r_a ["i gutpela", "i bikpela", "i nogut"].


p ::: good_for(good,[b:e]).
p ::: good_for(bad,[b:e]).
p ::: good_for(_,[]).





% gat + obj
309 dr r_doc "If *gat* is not preceded by *i*, it is surely an 
instance of HAVE with local subject (I or YOU). So, before the rule that 
recognises the subject-verb dependency, we can transform *gat* + 
substantive phrase into a verb-object structure. Cf. *i gat* + substantive 
phrase instead.". 
309 dr ct(v(v), p(exist, [j:e])) + ct(n(_),SP) ==>
       ct(v(v),p(have, [j:e,o:SP])).
309 dr r_a ["gat samting", "gat planti samting", "i gat samting"].




% SUBJECT + VERB
310 dr r_doc  "_Subject-verb_ dependency.

Conditions check sobject agreement (a third person subject triggers the particle *i* 
before the verb) and negation. Thr third condition (to be reviewed) tries to 
distinguish between the meanings LIVE and EXIST, which are both *stap*. in Tok 
Pisin.

Examples from Presteley 1999a, p.5".
310 dr ct(n(T),SP) +
       ct(v(VT),p(V,[R:Subj|Args])) ==>
       ct(s,s(NEG,TMA,e,e,e,p(V3,[R1:SP1|Args1]),e,e)) //
       [subj_agr(T,VT,Subj,SP,SP1),
        tma(V,TMA,V1),
        neg(V1,V2,NEG),
        exist(V2,V3,R,R1,Args,Args1,SP1)].
310 dr r_a ["mi ting", "mi save", "yu laik", "dispela manmeri i lukim"]. 
310 dr r_t ["mi ting", "wanpela i save", "yu harim", 
	    "dispela samting i go i kam", "mi bin wokim gutpela samting",
	   "em i stap"].

p ::: exist(live,exist,a,j,[c:e],[],sp(this,_,_,_,_)).
p ::: exist(V,V,R,R,A,A,_).

p ::: neg(i(neg(V)),i(V),not).
p ::: neg(p(neg(V)),p(V),not).
p ::: neg(neg(V),V,not).
p ::: neg(V,V,e).




% i gat + Subj
311 dr r_doc "Rules 311 and 312 recognise the existential structure 
*i gat* + substantive phrase.".
311 dr ct(v(v),p(exist,[j:3])) + ct(n(_),sp(Det,Alt,Num,Att,H)) ==>
       ct(s,s(e,e,e,e,e,p(exist,[j:sp(Det,Alt,Num,Att,H)]),e,e)) //
       [indet_sp(Det)].
311 dr r_t ["i gat planti samting long dispela ples", "i gat sampela manmeri"].


312 dr ct(v(v),p(neg(exist),[j:3])) + ct(n(_),sp(Det,Alt,Num,Att,H)) ==>
       ct(s,s(not,e,e,e,e,p(exist,[j:sp(Det,Alt,Num,Att,H)]),e,e)) //
       [indet_sp(Det)].
312 dr r_t ["i no gat samting"].





% have
313 dr r_doc "If the *i gat* structure recognised in rule 312 is preceded by a 
substantive phrase, then it is an instance of the HAVE prime.".
313 dr ct(n(T),SP1) +
       ct(s,s(NEG,e,e,e,e,p(exist,[j:OBJ]),e,e)) ==>
       ct(s,s(NEG,e,e,e,e,p(have,[j:SP2,o:OBJ]),e,e)) //
       [subj_agr(_,_,_,SP1,SP2)].
313 dr r_t ["em i gat planti samting"].

p ::: indet_sp(e).
p ::: indet_sp(ref(unkn)).





315 dr r_doc "Subject of adjectival predicate.".
315 dr ct(n(p),SP) +
       ct(a(_),A) ==>
       ct(s,s(e,e,e,e,e,p(A,[o:SP]),e,e)).
315 dr r_t ["yu gutpela", "mi liklik"].

316 dr r_doc "Subject of prepositional (locative) predicate.".
316 dr ct(n(p),SP) +
       ct(pp(R),SP1) ==>
       ct(s,s(e,e,e,e,e,p(be,[j:SP,l:loc(P,SP1)]),e,e)) // [locative(P)].

317 dr r_doc "Subject of prepositional (non locative) predicate.".
317 dr ct(n(p),SP) +
       ct(pp(like),SP1) ==>
       ct(s,s(e,e,e,e,e,p(like,[j:SP,o:SP1]),e,e)).
317 dr r_t ["em i olsem yu", "mi olsem dispela man"].

319 dr r_doc "Subject of nominal predicate.".
319 dr ct(n(p),SP) +
       ct(n(_),SP1) ==>
       ct(s,s(e,e,e,e,e,p(be,[j:SP,j:SP1]),e,e)).
319 dr r_t ["yu gutpela wanpela", "mi wanpela"].






% LOC ADJUNCTS
320 dr r_doc "Locative adjuncts (adverbials).".
320 dr ct(s,s(NEG,Tense,CAN,TIMES,DUR,p(V1,A),e,MAN)) +
       ct(adv(l(_)),ADV) ==> 
       ct(s,s(NEG,Tense,CAN,TIMES,DUR,p(V1,A),loc(ADV),MAN)).
320 dr r_t ["em i wokim samting hia"].



321 dr r_doc "Locative adjuncts (prepositional phrases).".
321 dr ct(s,s(NEG,Tense,CAN,TIMES,DUR,p(V1,A),e,MAN)) +
       ct(pp(LOC),sp(AA,BB,CC,DD,somewhere(X))) ==> 
       ct(s,s(NEG,Tense,CAN,TIMES,DUR,p(V1,A),loc(LOC1,sp(AA,BB,CC,DD,somewhere(X))),MAN)) // [loc_p(LOC,LOC1)].
321 dr r_t ["em i wokim samting i stap long dispela ples"].

p ::: loc_p(above,above).
p ::: loc_p(long,l).
p ::: loc_p(below,below).
p ::: loc_p(far,far).
p ::: loc_p(near,near).
p ::: loc_p(inside,inside).
p ::: loc_p(front,front).
p ::: loc_p(back,back).
p ::: loc_p(side,side).


322 dr r_doc "Local adjuncts when the head noun of the adjunct is not 
a local noun. As a prepositional phrase introduce by _long_ can be a causal as 
well as a local adjunct, we check that a local preposition be present.

This treatmen of adjuncts is provisional; if there are only two NSM 
constructons of the prime BECAUSE (1. BECAUSE+THIS _because of this_ and 2. 
Sentence1 BECAUSE Sentence2), then such \"causal adjuncts\" are not needed.".
322 dr ct(s,s(NEG,Tense,CAN,TIMES,DUR,p(V1,A),e,MAN)) +
       ct(pp(LOC),sp(AA,BB,CC,DD,something(X))) ==> 
       ct(s,s(NEG,Tense,CAN,TIMES,DUR,p(V1,A),loc(LOC1,sp(AA,BB,CC,DD,something(X))),MAN)) // [strict_loc_p(LOC,LOC1)].
321 dr r_t ["em i wokim samting ananit long dispela ples"].

p ::: strict_loc_p(above,above).
p ::: strict_loc_p(below,below).
p ::: strict_loc_p(far,far).
p ::: strict_loc_p(near,near).
p ::: strict_loc_p(inside,inside).
p ::: strict_loc_p(front,front).
p ::: strict_loc_p(back,back).
p ::: strict_loc_p(side,side).




% LOC ARGUMENTS (I STAP)
323 dr r_doc "Locative prepositional phrases are _arguments_, and not 
adjuncts, with the prime BE(loc). This rule recogfnises the absolute 
(adverbial) use of the local prepositions; rule 324 links a locatve 
preposition with its NP complement to the locative slot of the valence 
list of the prime BE(loc).".
323 dr ct(s,s(NEG,TENSE,e,e,e,p(exist,[j:SP]),e,e)) + ct(adv(l(_)), ADV)
       ==>
       ct(s, s(NEG,TENSE,e,e,e,p(be, [j:SP,l:ADV]), e, e)).
323 dr r_t ["em i stap antap", "em i stap insait"].


324 dr ct(s,s(NEG,TENSE,e,e,e,p(exist,[j:SP]),e,e)) +
       ct(pp(LOC),SP1)
       ==>
       ct(s, s(NEG,TENSE,e,e,e,p(be, [j:SP,l:loc(LOC,SP1)]), e, e)) //
       [locative(LOC)].
323 dr r_t ["em i stap antap long dispela samting",
	   "dispela samting i stap ananit long dispela narapela samting"].

p ::: locative(near).
p ::: locative(far).
p ::: locative(above).
p ::: locative(below).
p ::: locative(front).
p ::: locative(back).
p ::: locative(side).
p ::: locative(inside).




330 dr r_doc "Causal adjunct (he did this because of something).".
330 dr ct(s,S) + ct(pp(long),sp(this,e,e,[],something(fact))) ==>
       ct(s,because(S,sp(this,e,e,[],something(fact)))).
330 dr r_t ["em i wokim samting long dispela"].




331 dr r_doc "Causal adjunct (he did this just because of something). This 
rule is probably external to the NSM core.".
331 dr ct(s,S) + ct(pp(long),sp(only(this),e,e,[],something(fact))) ==>
       ct(s,because(S,sp(only(this),e,e,[],something(fact)))).
330 dr r_t ["em i wokim samting long dispela tasol"].    
       





% DUR ADJUNCTS
332 dr r_doc "FOR-SOME-TIME adjuncts (duration). NSM proposed prime is  
FOR SOME TIME, which can be modified to yield FOR A LONG/SHORT TIME (cf. 
Goddard 2008b, p. 66-68). This rule recognises \"for a long time\", the 
following, \"for a short time/for some time\".".
332 dr ct(s,s(NEG,Tense,CAN,TIMES,e,p(V1,A),LOC,MAN)) +
       ct(adv(dur),ADV) ==> 
       ct(s,s(NEG,Tense,CAN,TIMES,dur(ADV),p(V1,A),LOC,MAN)).
332 dr r_t ["em i wokim samting longtaim",
	    "em i wokim samting liklik taim"].


333 dr ct(s,s(NEG,Tense,CAN,TIMES,e,p(V1,A),LOC,MAN)) +
       ct(n(n), sp(e, e, e, [small], time(time)))
       ==> 
       ct(s,s(NEG,Tense,CAN,TIMES,dur(short),p(V1,A),LOC,MAN)).
333 dr r_t ["em i wokim dispela samting liklik taim"].





% FREQ ADJUNCTS
335 dr r_doc "N-TIMES (repetition) adjunct.".
335 dr ct(s,s(NEG,e,CAN,e,DUR,PRED,LOC,MAN)) +
       ct(n(n), sp(e, e, num(NUM), [], time(time))) ==>
       ct(s,s(NEG,e,CAN,freq(NUM),DUR,PRED,LOC,MAN)).
335 dr r_t ["em i wokim samting tupela taim"].





% MANNER
337 dr r_doc "Manner adjuncts, which in NSM depend on the prime LIKE.".
337 dr ct(s,s(NEG,e,CAN,DUR,DUR,PRED,LOC,e)) +
       ct(pp(like),LIKE) ==>
       ct(s,s(NEG,e,CAN,DUR,DUR,PRED,LOC,manner(LIKE))).
337 dr r_t ["em i wokim dispela samting olsem mi"].






% TIME ADJUNCTS
% non emphatic (=English tenses)
338 dr r_doc "Time adjuncts is surely another area of further improvement  
for NSM-PROLOG. In the current English grammar, I have tentatively taken tense 
inflection to stand for temporal primes such as BEFORE+NOW (past tense) 
and AFTER+NOW (future). In Tok Pisin NSM, future tense finds a tentative  
equivalent in future-irrealis particle *bai*; as for past tense, I have 
used here the adverb *bipo* _before_. 

The adverb *bihain* _after_ can mean not only AFTER+NOW 
(that is, _in the future_), but also \"after that\" (as in: he did this 
thing. AFTER THAT, he did this other thing. In a Swahili NSM, this second 
sentence would need a particular tense, the _narrative_ or _consecutive_:
alifanya kitu hicho. (baado) *akafanya* kitu kingine). 

This means that  in an Englis NSM sentence as _after that, he did 
something_ there are *two* time adverbials (_after that_ and _before now_, 
which surfaces as past tense).

In my first paper about NSM-PROLOG, I have distinguished three kinds 
of time modifier, by giving three different slots in the sentence 
representation formula:

1. _Frequency_ (ONE+TIME _once_, TWO+TIMES _twice_, MANY+TIMES);

2. _Duration_ (FOR SOME TIME + LONG/SHORT);

3. _Time Location_.

I think that the _time location_ slot will have to be split into an 
_absolute time location_ (NOW, BEFORE/AFTER + NOW) and a _relative 
time location_ (AT/BEFORE/AFTER + THIS TIME, where THIS refers to a 
previously mentioned situation).

This will require some changes in the current English grammar, which will be 
done for the next release.

I am not yet sure as to how to render all this effective in Tok Pisin; 
feedback is welcome in this like in any other area.".						    						    
338 dr ct(adv(time(_)),time(E,P,e)) -
       ct(s,s(NEG,e,CAN,TIMES,DUR,PRED,LOC,MAN)) ==>
       ct(s,s(NEG,time(E,P,e),CAN,TIMES,DUR,PRED,LOC,MAN)).
338 dr r_t ["bipo em i wokim dispela"].

339 dr ct(pp(long), sp(this, e, e, [], time(time))) -
       ct(s,s(NEG,e,CAN,TIMES,DUR,PRED,LOC,MAN)) ==>
       ct(s,s(NEG,time(e,at,this),CAN,TIMES,DUR,PRED,LOC,MAN)).
339 dr r_a ["long dispela taim em i wokim dispela"].

340 dr ct(n(n), sp(e, e, num(all), [], time(time))) -
       ct(s,s(NEG,e,CAN,TIMES,DUR,PRED,LOC,MAN)) ==>
       ct(s,s(NEG,time(e,at,all),CAN,TIMES,DUR,PRED,LOC,MAN)).
340 dr r_a ["mi wokim dispela olgeta taim"].

341 dr ct(adv(time(_)),time(e,now,e)) -
       ct(s,s(NEG,e,CAN,TIMES,DUR,PRED,LOC,MAN)) ==>
       ct(s,s(NEG,time(e,at,now),CAN,TIMES,DUR,PRED,LOC,MAN)).
341 dr r_a ["nau mi wokim dispela"].




% bai in testa alla frase
342 dr r_doc "Rule for *bai* (future marker) in sentence-initial 
position.".
342 dr ct(tma,bai) + ct(s,s(NEG,e,CAN,TIMES,DUR,p(V1,A),e,MAN)) ==>
       ct(s,s(NEG,time(e,after,e),CAN,TIMES,DUR,p(V1,A),e,MAN)).
342 dr r_t ["bai mi wokim samting"].




% emphatic (= English explicit adverbials)
343 dr r_doc "This rule and the following will have to be reviewed when 
a better representation of time location adverbials will be available.".
343 dr ct(adv(time(_)),time(emph,E,P,e)) -
       ct(s,s(NEG,e,CAN,TIMES,DUR,PRED,LOC,MAN)) ==>
       ct(s,s(NEG,time(emph,E,P,_),CAN,TIMES,DUR,PRED,LOC,MAN)).

344 dr ct(adv(time(_)),time(emph,e,now,e)) -
       ct(s,s(NEG,e,CAN,TIMES,DUR,PRED,LOC,MAN)) ==>
       ct(s,s(NEG,time(emph,e,at,now),CAN,TIMES,DUR,PRED,LOC,MAN)).






% PROPOSITIONAL COMPLEMENTS

350 dr r_doc "*ting* THINK and *save* KNOW + proposition.".
350 dr ct(s, s(e, e, e, e, e, p(V, [e:SP]), e, e))
     + ct(s,S) ==>
    ct(s, s(e, e, e, e, e, p(V, [e:SP,prop:S]), e, e)) //
    [prop_compl(V)]. 
350 dr r_t ["mi ting yu gutpela",
	   "em i save mi bin wokim samting"].


351 dr r_doc "In the lexical representation of the verbs which can take 
a proposition as complement (*save*, *tok*, *ting*), I have put no 
object slot, to prevent the parser from analysing structures like  
_I know you are someone good_ as [_I know you_] where _you_ is 
attached as an object of _know_.

In spoken language there are probably hints to the human parser (due to 
intonation and/or stress) which alert the hearer to wait further.

The NSM-PROLOG parser does not treat _know_ and _think_ like normal 
transitive verbs, so it does not expect an object. 

Therefore, the transitive version of _know_ and _think_ (KNOW SOMETHING, 
THINK SOMETHING) need a separate rule, which is to be higher than the rule 
which recognises the propositional frame THINK/KNOW (THAT) + S.

					 
This happens in English because the complementizer _that is optional, and in 
Tok Pisin, because this language has no complementizer in this construction.
".
351 dr ct(s, s(e, e, e, e, e, p(know, [e:SP]), e, e))
     + ct(n(_),O) ==>
     ct(s, s(e, e, e, e, e, p(know, [e:SP,o:O]), e, e)).
351 dr r_t ["em i save planti samting", "mi save dispela samting"].




352 dr r_doc "*save* + *plenti* = KNOW MANY THINGS".
352 dr ct(s, s(e, e, e, e, e, p(know, [e:SP]), e, e))
     + ct(num,many) ==>
     ct(s, s(e, e, e, e, e, 
	     p(know, [e:SP,
		      o:sp(e,e,num(many),[],something(thing))]), 
	     e, e)).
352 dr r_t ["em i save plenti"].




353 dr r_doc "*save* + *long N* = KNOW (SOMETHING) ABOUT N.".
353 dr ct(s, s(e, e, e, e, e, p(know, [e:SP]), e, e))
     + ct(pp(long),T) ==>
     ct(s, s(e, e, e, e, e, p(know, [e:SP,o:sp(e,e,e,[],something(thing)),o:e,t:T]), e, e)).
353 dr r_a ["mi save long yu"].


354 dr r_doc "\"Standalone THIS\" as object of KNOW.".
354 dr ct(s, s(e, e, e, e, e, p(know, [e:SP]), e, e))
     + ct(d,this) ==>
     ct(s, s(e, e, e, e, e, p(know, [e:SP,o:sp(this,e,e,[],something(fact))]), e, e)). 
354 dr r_t ["mi save dispela"].

355 dr r_doc "KNOW SOMETHING ABOUT".
355 dr ct(s, s(e, e, e, e, e, p(know, [e:SP,o:O]), e, e))
     + ct(pp(long),T) ==>
     ct(s, s(e, e, e, e, e, p(know, [e:SP,o:O,t:T]), e, e)).
355 dr r_a ["mi no save plenti samting long dispela"].


357 dr r_doc "_Equi_ want. The prime WANT is *laik* in Tok Pisin (cf. 
Priestley 1999a, pp. 12-13).".
357 dr ct(s,s(NEG,Tense,CAN,TIMES,DUR,p(want,[e:S]),LOC,MAN)) +
       ct(v(_),p(V,[R:e|A])) ==>
       ct(s,s(NEG,Tense,CAN,TIMES,DUR,p(want,[e:S,o:[p(V,[R:e|A])]]),LOC,MAN)).
357 dr r_t ["mi laik wokim samting"].


358 dr r_doc "_Non-equi_ want is *laik long* + Sentence.".
358 dr ct(s,s(NEG1,TMA,CAN,e,e,p(want,[e:S1,o:p]),e,e)) +
       ct(s,s(NEG2,e,e,TIMES,DUR,p(V,[R:S2|A]),LOC,MAN)) ==>
       ct(s,s(NEG1,TMA,CAN,TIMES,DUR,p(want,[e:S1,o:[p(V1,[R:S2|A])]]),LOC,MAN))
       // [want_not(NEG2,V,V1)].  
358 dr r_t ["mi laik long yu wokim samting"].




371 dr r_doc "The last level in Tok Pisin grammars is that of complex 
sentences. Rule 372 takes care of recognising subordinate 
clauses (clauses preceded by a conjunction); before of this, however, a 
stranded *taim* before a clause will be interpreted as the 
conjunction _when_.".
371 dr ct(n(n),sp(e,e,e,[],time(time))) + ct(s,S) ==>
       ct(s(when),S).
371 dr r_a ["taim yu wokim samting"]. 


372 dr ct(conj,C) + ct(s,S) ==> ct(s(C),S).
372 dr r_a ["mi stap","sapos mi stap","yu go kam","long wanem yu go kam"].


373 dr r_doc "IF S1 S2.".
373 dr ct(s(if),S1) + ct(s,S2) ==> ct(s,if(S1,S2)).
373 dr r_t ["sapos yu wokim dispela samting bel bilong mi i stap gut"].


374 dr r_doc "S1 as S2.".
374 dr ct(s,S2) + ct(s(like),S1)  ==> ct(s,like(S1,S2)).
374 dr r_t ["bel bilong mi i stap gut olsem na bel bilong yu i stap gut"].

375 dr r_doc "WHEN S1, S2".
375 dr ct(s(when),S1)  + ct(s,S2) ==> ct(s,when(S1,S2)).
375 dr r_t ["taim yu wokim dispela samting bel bilong mi i stap gut"].


377 dr r_doc "S1 BECAUSE S2".
377 dr ct(s,S2)+ ct(s(because),S1)  ==> ct(s,because(S1,S2)).
377 dr r_t ["mi wokim samting long wanem yu laik long mi wokim samting"].


380 dr r_doc "S + because of this".
380 dr ct(pp(long), sp(this, e, e, [], something(fact))) -
    ct(s,S) ==>
    ct(s,because(S,this)).
380 dr r_t ["mi wokim samting long dispela"].



(eng:e)
gepilogue
"This grammar of Tok Pisin NSM is still very tentative.

First of all, feedback from Tok Pisin experts will presumably lead to 
corrections (the generated Tok Pisin could be simply wrong in some cases, 
constructions could have to be added, etc.). 

Secondly, a better NSM-PROLOG representation of time adverbials will lead 
to changes in both the English and Tok Pisin grammars.

When this will be done, a sort of \"NSM-specification\" should be prepared 
using English, Tok Pisin and other modules which will have been prepared in 
the meantime. NSM texts written for automatic treatment should of course 
stick to the NSM standard so defined, and new grammars should be able 
to parse and generate those sentences in the specification.

Now that a Tok Pisin module is available, NSM-PROLOG can translate between 
two (language-particular) NSMs. I am confident that this project can have 
both theoretical and practical significance:

@* from the side of theory, \"reducing\" a language-particular NSM to 
computational treatment forces the grammar writer to make all assumptions 
explicit. 

@* As for the practical significance, every text written in a 
language-particular NSM  will be immediately available in every other 
language-particular NSM for which a grammar is available. This is, 
in essence, Father Roberto Busa's dream (cf. Busa 2006), if we see NSM as 
a radical form of \"controlled language\".

Now, I think that NSM can greatly help endangered (or already dead) language 
revitalization programs. If a language-particular NSM is the smallest 
usable subset of that language, a first step in a language revitalization 
program could consist in teaching NSM first. Even the most time-lacking and 
grammar-intimidated person could think he/she can afford learning some 
sixty-odd 
words and their structural patterns. Ther result would be rewarding, because 
NSM can be used to express cultural scripts, word definitions and perhaps 
other culturally significant aspects of one's own culture, if these two were 
not enough. 

Let's imagine, for example, an American First Nation whose language is gone 
or almost gone, but documented. By extracting this language's NSM from 
the documentation, and writing a PROLOG grammar, all NSM texts stored in 
electronic format could immediately be made available in that language. Such 
texts could include, for example, an \"NSM primer\", a ground-level course 
(I am preparing one, for the matter), and various anthologies of cultural 
scripts, or some biblical paraphrase. 

But. most important, that First Nation could engage in detecting their 
own cultural scripts. They could put them down in English, and have 
them translated into their language NSM. This language could then become 
_culturally functional_, in that it would have a sphere of use -- transmitting 
the traditional culture of the group. If learning a \"whole\" language can 
be seen as a daunting, intimidating task for many people, learning an NSM 
surely shouldn't. If for someone the language revitalization enterprise 
ended there, it would already be something.

But, of course, if NSM is a first step, then one could ask what should be 
the second. Is there a \"next subset\", larger than NSM but nonetheless 
restricted? Does this subset (if it is there at all) coincide with what 
Father busa has in mind with his \"controlled languages\"?". 



idiom(
 s(A, B, C, D, E, p(be_good, [e:sp(e, e, e, [rel(gen,SP)], insides)]), F, G),
 s(A, B, C, D, E, p(feel, [e:SP,o:sp(e,e,e,[good],something(thing)),g:e]),F,G)
 ).
idiom(
 s(A, B, C, D, E, p(be_very_good, [e:sp(e, e, e, [rel(gen,SP)], insides)]), F, G),
 s(A, B, C, D, E, p(feel, [e:SP,o:sp(e,e,e,[very(good)],something(thing)),g:e]),F,G)
 ).
idiom(
      s(A,B,C,D,E, p(bad, [o:sp(e, e, e, [rel(gen, SP)], insides)]), F,G),
      s(A, B, C, D, E, p(feel, [e:SP,o:sp(e,e,e,[bad],something(thing)),g:e]),F,G)).
idiom(
      s(A,B,C,D,E, p(make, [a:SUBJ,o:sp(A1,B1,C1,D1,words)]), F,G),
      s(A,B,C,D,E,p(say, [a:SUBJ,o:sp(A1,B1,C1,D1,words)]),F,G)).
      

p ::: want_not(not,V,not(V)).
p ::: want_not(e,V,V).

p ::: prop_compl(think).
p ::: prop_compl(know).
p ::: prop_compl(say_so).


% p ::: tma(p(V),e,V).
p ::: tma(fut(V),time(e,after,e),V).
p ::: tma(neg(fut(V)),time(e,after,e),neg(V)).
p ::: tma(V,e,V).

p ::: subj_agr(p,_,3,sp(e,e,e,[],d(s3(nom))),sp(this,e,e,[],someone(person))).
p ::: subj_agr(p,_,e,sp(A,B,C,D,d(E)),sp(A,B,C,D,d(E))).
p ::: subj_agr(n,_,3,SP,SP).
p ::: subj_agr(d,_,3,SP,SP).
p ::: subj_agr(wh,_,3,SP,SP).

p ::: third_person(someone(_)).
p ::: third_person(something(_)).
p ::: third_person(somewhere(_)).
p ::: third_person(time(_)).

p ::: macro_role(long,something(fact),cause). %LOCATION
p ::: macro_role(long,something(_),l). %LOCATION
p ::: macro_role(long,something(_),t). %ARGUMENT
p ::: macro_role(long,somewhere(_),l). %LOCATION
p ::: macro_role(long,somewhere(_),t). %ARGUMENT
p ::: macro_role(long,someone(_),d). %DAT
p ::: macro_role(long,someone(_),t). %ARGUMENT
p ::: macro_role(long,someone(_),b). %BEN
p ::: macro_role(with,someone(_),c). %COMITATIVE
p ::: macro_role(long,d(me),d). %DAT
p ::: macro_role(long,d(me),t). %ARGUMENT
p ::: macro_role(long,d(me),b). %BEN
p ::: macro_role(with,d(me),c). %COMITATIVE
p ::: macro_role(long,d(you),d). %DAT
p ::: macro_role(long,d(you),t). %ARGUMENT
p ::: macro_role(long,d(you),b). %BEN
p ::: macro_role(with,d(you),c). %COMITATIVE
p ::: macro_role(with,something(_),i). %INSTRUMENT








ms ::: [v(T),?v(T),?trans].
ms ::: [_].

/* MACROS */

ct(n(d),N) ::: sp(e,e,e,[],N).
ct(n(n),N) ::: sp(e,e,e,[],N).
ct(n(wh),N) ::: sp(e,e,e,[],N).
ct(n(p),N) ::: sp(e,e,e,[],d(N)).


ct(v(v),happen) ::: p(happen,[o:e,d:e]).
ct(v(v),do) ::: p(do,[a:e,o:e,d:e,c:e,i:e,b:e]).
ct(v(v),go) ::: p(go,[a:e]).
ct(v(v),come) ::: p(come,[a:e]).

ct(v(v),die) ::: p(die,[o:e]).
ct(v(v),live) ::: p(live,[a:e,c:e]).

ct(v(v),feel) ::: p(feel,[e:e,o:e,g:e]).
ct(v(v),know) ::: p(know,[e:e]).
ct(v(v),think) :::  p(think,[e:e]).
ct(v(v),say) ::: p(say,[a:e]).
ct(v(v),hear):::  p(hear,[a:e,o:e,t:e]).
ct(v(v),see) ::: p(see,[a:e,o:e]).
ct(v(v),want) :::  p(want,[e:e]).
ct(v(v),touch) :::  p(touch,[a:e,o:e]).

ct(v(v),make) :::  p(make,[a:e,o:e]).

ct(v(v),exist) ::: p(exist,[j:e]).


ct(v(v),do_to(X)) ::: p(X,[a:e,o:e]).


u :: m(v(v),"pilim",feel).
u :: m(n(n),"pipel",people).
u :: m(conj,"bekos",because).


m(trans,"im",trans).

m(i,"i",i).
m(tma,"bin",bin).
m(tma,"bai",bai).


m(q(1),"ol",plur).

m(n(p),"mi",me).
m(n(p),"yu",you).

m(n(p),"em",s3(nom)).
m(n(p),"en",s3(acc)).

m(n(d),"wanpela",someone(person)).

m(n(wh),"husat",someone(wh)).

m(n(n),"manmeri",people).
m(n(n),"man",someone(person)).
m(n(n),"samting",something(thing)).
m(n(n),"taim",time(time)).
m(n(n),"ples",somewhere(place)).

% m(n(n),"way",manner(manner)).
m(n(n),"bodi",body).
m(n(n),"kain",kind).
m(n(n),"hap",part).

% m(num,"wanpela",one).
m(num,"tupela",two).
m(num,"sampela",some).
m(num,"planti",many).
% m(num,"muC",many(1)).
m(num,"olgeta",all).

m(alt,"moa",more).


m(a(a),"gutpela",good).
m(r(a),"gut",good).
m(a(p),"nogut",bad).
m(a(a),"bikpela",big).
m(a(a),"liklik",small).

m(d,"dispela",this).

m(alt,"wankain",same).
m(alt,"narapela",other).

m(adv,"tumas",very(0)).
m(adv,"tru",very(1)).

m(p,"long",long).
m(p,"olsem",like).
m(p,"bilong",gen).
m(p,"wantaim",with).

m(v(v),"gat",exist).
% anche: there is, ma there is ha sempre i: i gat
% m(v(aux),"will",fut).

m(v(aux),"inap",can).

m(v(v),"wokim",do).
m(v(v),"kamap",happen).
m(v(v),"go",go).
m(v(v),"kam",come).
m(v(v),"pas",touch).

m(v(v),"stap",live).
m(v(v),"sindaun",live).
m(v(v),"dai",die).
m(v(v),"stap",exist).

m(a,"tru",true).
m(v(v),"tok",say).
m(n(n),"tok",words).
m(v(v),"harim",hear).
m(v(v),"lukim",see).
m(v(v),"ting",think).

m(v(v),"save",know).
m(v(v),"laik",want).

m(n(n),"ples",place).

m(adv(l(i)),"we",where).

m(adv(l(rel)),"insait",inside).
m(adv(l(rel)),"antap",above).
m(adv(l(i)),"daunbilo",below).
m(adv(l(rel)),"ananit",below).
m(adv(l(rel)),"klostu",near).
m(adv(l(rel)),"longwe",far).

m(adv(l(i)),"hia",here).
% m(adv(l(i)),"",there).
% m(n(n),"front",front).
m(n(n),"sait",side). % SUFFISSO

m(adv(time(i)),"nau",time(e,now,e)).
m(adv(time(tr)),"bipo",time(e,before,e)).
m(adv(time(tr)),"bihain",time(e,after,e)).

m(adv(dur),"longtaim",long).
% LIKLIK TAIM: short time, LONGTAIM LIKLIK: for some time

m(neg,"no",not).
% m(conj,"long dispela",because).
% m(conj,"long wanem",because).
m(conj,"sapos",if).
% m(conj,"taim",if).
m(adv(s),"ating",maybe).

m(adv(sent),"ating",maybe).

m(n(n),"Bikpela",someone(god)).
m(n(n),"pisin",something(bird)).


m(n(n),"bel",insides).


m(n(q),"wanem",what).


m(adv,"tasol",only).


m(n(n),"wara",something(water)).


m(v(v),"bagarapim",do_to(destroy)).
m(v(v),"mekim",make).


m(n(n),"pikinini",someone(child)).
m(n(n),"kaikai",something(food)).
m(n(n),"diwai",something(tree)).
m(n(n),"haus",somewhere(house)).
m(n(n),"mani",something(money)).
m(n(n),"taim",time(time)).
m(n(n),"meri",someone(woman)).


m(n(n),"dok",something(dog)).


m(conj,"na",and).

auto_text(conditions,"Conditions").

