%	TALK Grammar
%
%	From the book "Prolog and Natural-Language Analysis"
%	by Fernando Pereira and Stuart Shieber


%	TEST AS FOLLOWS:
%
%	PARSING
%
%		String: bertrand wrote principia
%
%	GENERATION
%
%		Start Symbol :  s(_,nogap)
%		Max Tree Depth:  5	
%		Mode: Random		

 

/*=====================================================
                       Operators
=====================================================*/

:- op(500,xfy,&). 
:- op(510,xfy,=>). 
:- op(100,fx,Õ).


/*=====================================================
                        Grammar

Nonterminal names:

        q        Question
        sinv     INVerted Sentence
        s        noninverted Sentence
        np       Noun Phrase
        vp       Verb Phrase
        iv       Intransitive Verb
        tv       Transitive Verb
        aux      AUXiliary verb
        rov      subject-Object Raising Verb
        optrel   OPTional RELative clause
        relpron  RELative PRONoun
        whpron   WH PRONoun
        det      DETerminer
        n        Noun
        pn       Proper Noun

Typical order of and values for arguments:

   1. verb form:

      (main verbs)  finite, nonfinite, etc.
      (auxiliaries and raising verbs)  Form1-Form2 
          where Form1 is form of embedded VP
                Form2 is form of verb itself)

   2. FOL logical form

   3. gap information:  

      nogap or gap(Nonterm, Var)
          where Nonterm is nonterminal for gap
                    Var is the LF variable that
                           the filler will bind
=====================================================*/

%%%                    Questions

q(S =>  `answer(X)) --> 
   whpron, vp(finite, X^S, nogap).
q(S => `answer(X)) --> 
   whpron, sinv(S, gap(np, X)).
q(S => `answer(yes)) --> 
   sinv(S, nogap).
q(S => `answer(yes)) -->
   cop, 
   np((X^S0)^S, nogap), 
   np((X^true)^exists(X,S0&true), nogap).

%%%              Declarative Sentences

s(S, GapInfo) --> 
   np(VP^S, nogap), 
   vp(finite, VP, GapInfo).

%%%               Inverted Sentences

sinv(S, GapInfo) --> 
   aux(finite/Form, VP1^VP2), 
   np(VP2^S, nogap), 
   vp(Form, VP1, GapInfo).

%%%                  Noun Phrases

np(NP, nogap) --> 
   det(N2^NP), n(N1), optrel(N1^N2).
np(NP, nogap) --> pn(NP).
np((X^S)^S, gap(np, X)) --> [].

%%%                  Verb Phrases

vp(Form, X^S, GapInfo) -->
   tv(Form, X^VP), 
   np(VP^S, GapInfo).
vp(Form, VP, nogap) --> 
   iv(Form, VP).
vp(Form1, VP2, GapInfo) -->
   aux(Form1/Form2, VP1^VP2), 
   vp(Form2, VP1, GapInfo).


vp(Form1, VP2, GapInfo) -->
   rov(Form1/Form2, NP^VP1^VP2), 
   np(NP, GapInfo), 
   vp(Form2, VP1, nogap).
vp(Form2, VP2, GapInfo) -->
   rov(Form1/Form2, NP^VP1^VP2), 
   np(NP, nogap), 
   vp(Form1, VP1, GapInfo).


%% This rule is responsible for creating a cyclic term when parsing 
%% sentences like "bertrand is bertrand" that will bomb the tree 
%% drawing routine. If you want to use this rule, you must not try 
%% to print the trees corresponding to sentences like that.

vp(finite, X^S, GapInfo) -->
  cop,
  np((X^P)^exists(X,S&P), GapInfo).


%%%                 Relative Clauses

optrel((X^S1)^(X^(S1&S2))) -->
   relpron, vp(finite,X^S2, nogap).
optrel((X^S1)^(X^(S1&S2))) -->
   relpron, s(S2, gap(np, X)).
optrel(N^N) --> [].



/*=====================================================
                      Dictionary
=====================================================*/

/*-----------------------------------------------------
                     Preterminals
-----------------------------------------------------*/

det(LF) --> [D], {det(D, LF)}.
n(LF)   --> [N], {n(N, LF)}.
pn((E^S)^S) --> [PN], {pn(PN, E)}.

aux(Form, LF) --> [Aux], {aux(Aux, Form, LF)}.
relpron --> [RP], {relpron(RP)}.
whpron --> [WH], {whpron(WH)}.

% Verb entry arguments:
%   1. nonfinite form of the verb
%   2. third person singular present tense form of the verb
%   3. past tense form of the verb
%   4. past participle form of the verb
%   5. pres participle form of the verb
%   6. logical form of the verb

iv(nonfinite,       LF) --> [IV], {iv(IV, _, _, _, _, LF)}.
iv(finite,          LF) --> [IV], {iv(_, IV, _, _, _, LF)}.
iv(finite,          LF) --> [IV], {iv(_, _, IV, _, _, LF)}.
iv(past_participle, LF) --> [IV], {iv(_, _, _, IV, _, LF)}.
iv(pres_participle, LF) --> [IV], {iv(_, _, _, _, IV, LF)}.

tv(nonfinite,       LF) --> [TV], {tv(TV, _, _, _, _, LF)}.
tv(finite,          LF) --> [TV], {tv(_, TV, _, _, _, LF)}.
tv(finite,          LF) --> [TV], {tv(_, _, TV, _, _, LF)}.
tv(past_participle, LF) --> [TV], {tv(_, _, _, TV, _, LF)}.
tv(pres_participle, LF) --> [TV], {tv(_, _, _, _, TV, LF)}.

rov(nonfinite      /Requires, LF) 
           --> [ROV], {rov(ROV, _, _, _, _, LF, Requires)}.
rov(finite         /Requires, LF)
           --> [ROV], {rov(_, ROV, _, _, _, LF, Requires)}.
rov(finite         /Requires, LF)
           --> [ROV], {rov(_, _, ROV, _, _, LF, Requires)}.
rov(past_participle/Requires, LF)
           --> [ROV], {rov(_, _, _, ROV, _, LF, Requires)}.
rov(pres_participle/Requires, LF)
           --> [ROV], {rov(_, _, _, _, ROV, LF, Requires)}.


cop --> [is].


/*-----------------------------------------------------
                     Lexical Items
-----------------------------------------------------*/

relpron( that ).
relpron( who  ).
relpron( whom ).

whpron( who  ).
whpron( whom ).
whpron( what ).

det( every, (X^S1)^(X^S2)^   all(X,S1=>S2) ).
det( a,     (X^S1)^(X^S2)^exists(X,S1&S2)  ).
det( some,  (X^S1)^(X^S2)^exists(X,S1&S2)  ).

n( author,     X^`author(X)     ).
n( book,       X^`book(X)       ).
n( professor,  X^`professor(X)  ).
n( program,    X^`program(X)    ).
n( programmer, X^`programmer(X) ).
n( student,    X^`student(X)    ).
pn( begriffsschrift, begriffsschrift ).
pn( bertrand,        bertrand        ).
pn( bill,            bill            ).
pn( gottlob,         gottlob         ).
pn( lunar,           lunar           ).
pn( principia,       principia       ).
pn( shrdlu,          shrdlu          ).
pn( terry,           terry           ).

iv( halt,      halts,      halted,    
    halted,    halting,    X^`halt(X)         ).

tv( write,     writes,     wrote,
    written,   writing,    X^Y^`writes(X,Y)   ).
tv( meet,      meets,      met,
    met,       meeting,    X^Y^`meets(X,Y)    ).
tv( concern,   concerns,   concerned, 
    concerned, concerning, X^Y^`concerns(X,Y) ).
tv( run,       runs,       ran,       
    run,       running,    X^Y^`runs(X,Y)     ).

rov( want,     wants,      wanted,    
     wanted,    wanting,
     % semantics is partial execution of
     % NP ^ VP ^ Y ^ NP( X^want(Y,X,VP(X)) )
     ((X^`want(Y,X,Comp))^S) ^ (X^Comp) ^ Y ^ S,
     % form of VP required:
     infinitival).

aux( to,   infinitival/nonfinite, VP^ VP       ).
aux( does, finite/nonfinite,      VP^ VP       ).
aux( did,  finite/nonfinite,      VP^ VP       ).
