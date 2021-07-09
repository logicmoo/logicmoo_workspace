:-module(parser_jpaine,[trans/3]).


/*  GB.PL  */


:- dynamic(lexadjective/1).
:- dynamic(lexconj/2).
:- dynamic(lexdet/3).
:- dynamic(lexmodal/1).
:- dynamic(lexnoun/2).
:- dynamic(lexprep/1).
:- dynamic(lexverb/4).

/*
This is the parser that I use with PLANNING_BUG.PL. It was sent
by Cameron Shelley. It defines a top-level predicate
    trans( Sentence+, Meaning-, Type- )
which parses Sentence and translates it into a type and a meaning.

Shelley wrote the parser. I wrote the parse-tree to meaning translator.
For more documentation on the parser, see right at the end of this
file.                 

Sentences are represented as lists of tokens (atoms).

Meanings are represented in a simple "logical form language". I shan't
define it formally, since in its present state, it's just a hack
which I wrote to get something going. Amongst other things, it
doesn't interface properly with the lexicon, and imperatives aren't
done correctly. Here are some examples:
    ?- trans( [eat,the,food], M, T ).
    M = the(_1, food(_1)) ,
        eat(me, _1)
    T = command

    ?- trans( [the,bug,eats,the,food], M, T ).
    M = the(_1, bug(_1)) ,
        the(_2, food(_2)) ,
        eats(_1, _2)
    T = assertion

    ?- trans( [the,big,bug,smashes,the,small,rock], M, T ).
    M = the(_1, (bug(_1) , big(_1))) ,
        the(_2, (rock(_2) , small(_2))) ,
        smashes(_1, _2)
    T = assertion

    ?- trans( [the,big,bug,smashes,the,small,rock,by,the,quicksand],M,T).
    M = the(_1, (bug(_1) , big(_1))) ,
        the(_2, (rock(_2) , small(_2) ,
        the(_3, quicksand(_3)) , by(_2, _3))) ,
        smashes(_1, _2)
    T = assertion
*/

/*
The parser
----------
*/


%%%%%%%%%
%       "Gibberish" -- A GB'ish parser!
%
%   A first attempt at a Government-Binding (GB) type
%   parser.  This parser is intended as an introductory
%   'toy' parser for NLU courses, so complexity will be
%   kept to a minimum while still being general enough
%   to be easily extended.  All constraints have been
%   been implemented as explicit prolog goals for
%   perspicuity.  Many optimizations are possible!  The
%   gapping mechanism actually performs a transformation
%   of the sentence into a 'normal' form and so has been
%   made general enough to move arbitrary structures
%   through the parse tree.
%
%   Author: Cameron Shelley
%   Address: cpshelley@violet.waterloo.edu
%        University of Waterloo
%
%   Comments are welcome!
%
%   This software is released into the public domain on the
%   condition that the author is cited as such, and all
%   modifications remain in the public domain; and this
%   condition is imposed on all subsequent users.
%
%   Modification History:
%   ---------------------
%   Jan 17/91 - creation
%   Feb 4/91  - fixed modal(nil) matching bug in "modal-2".
%   Feb 4/91  - added general conjunction rule "conj".
%           (idea from Steve Green -- thanks Steve!)
%
%%%%%%%%%

parse :- read_sentence(Sentence),
     sentence(Struc,Sentence,[]),
     print_struct(Struc).

print_struct(S):- fmt(S).

system:trans( Sentence, Goal, Type ) :-
     sentence( Struc, Sentence, [] ),
     print_struct(struc:-Struc),
     trans_sentence( Struc, Goal, Type ).


%%%%%%%%%
%   'sentence' will parse the basic np,vp structure
%   at the top level.  Different sentence types will
%   be added (ie. questions).
%
%   sentence(
%       Struc  : return structure from sentence call
%       )
%
%%%%%%%%%

%   normal sentence
%
sentence(Struc) -->
    noun_phrase(Np,Pers,Nnum,nogap,nogap),
    {Nnum = Vnum},
    verb_phrase(Vp,Pers,Vnum,nogap),
    {Struc = s(Np,Vp)}.

%   question with modal transformed to initial position
%
sentence(Struc) -->
    modal(M,_,_,_,_,nogap,_),
    noun_phrase(Np,Pers,Nnum,nogap,nogap),
    {Nnum = Vnum},
    verb_phrase(Vp,Pers,Vnum,M),
    {Struc = q(Np,Vp)}.

%   imperative
%
sentence(Struc) -->
    verb_phrase(Vp,_Pers,_Vnum,_M),
    {Struc = c(Vp)}.


%%%%%%%%%
%   'noun_phrase' will parse the various types of np's and
%   should subcategorize between np's and sbars at some point.
%   Also, proper nouns and pronouns can be treated as special
%   np's in this system.
%
%   noun_phrase(
%       Struc : return structure,
%       Pers  : np 'person' = first | second | third,
%       Nnum  : np 'number' = sg | pl,
%       Gap   : transformed np (if any),
%       Gapout: output gap if Gap not resolved
%   )
%
%%%%%%%%%

noun_phrase(Struc,_Pers,_Nnum,Gap,Gapout) -->
    [],
    {Gap =.. [np|_]},
    {Gapout = nogap},
    {Struc = Gap}.

noun_phrase(Struc,Pers,Num,Gap,Gapout) -->
    determiner(Det,Dnum),
    {Nnum = Dnum},
    noun_bar(Nbar,Nnum),
    {Pers = third},
    {Gapout = Gap},
    {InStruc = np(Det,Nbar)},
    conj(Struc,InStruc,Nnum,Num,np).

%
%   determiner is the noun phrase specifier
%
%   determiner(
%       Struc : return structure,
%       Dnum  : det 'number' = sg | pl
%   )
%
%   No determiner is considered to pluralize the np, ie:
%   "cats go" but not *"cat goes".  The default could be
%   changed to "all" or "some" if desired.
%

determiner(Struc,Dnum) --> [Word], {lexdet(Word,_Det,Dnum)}, {Struc = det(Word)}.
determiner(Struc,Dnum) --> [], {Dnum = pl}, {Struc = det(nil)}.

%
%   noun_bar is here just a noun with arguments.  A treatment
%   of adjectives should be added.
%
%   noun_bar(
%       Struc : return structure,
%       Nnum  : noun 'number' parsed = sg | pl
%       )
%

noun_bar(Struc,Nnum) -->
    adjectives( As ),
    noun(N,Nnum),
    noun_args(Nmod),
    {Struc = nbar(As,N,Nmod)}.


%   adjectives(
%       Struc : return structure
%       )
%
adjectives(Struc) -->
    [], {Struc = adjectives(nil)}.

adjectives(Struc) -->
    [Word], {lexadjective(Word)},
    adjectives(Rest),
    {Struc = adjectives(Word,Rest)}.


%
%   mass nouns should be considered as noun_bars in this system!
%

noun(Struc,Nnum) --> [Word], {Nnum = sg}, {lexnoun(Word,_)},
    {Struc = noun(Word)}.
noun(Struc,Nnum) --> [Word], {Nnum = pl}, {lexnoun(_,Word)},
    {Struc = noun(Word)}.

%
%   noun_args here allows only pp's or nil's.  Handling of
%   embedded sentences can be added as suggested.
%
%   noun_args(
%       Struc : return structure
%       )
%

noun_args(Struc) -->
    prep_phrase(Pp),
    {Struc = n_args(Pp)}.

%noun_args(Struc) -->
%   sentence_bar(Sb),
%   {Struc = n_args(Sb)}.

noun_args(Struc) -->
    [],
    {Struc = n_args(nil)}.

%%%%%%%%%
%   'verb_phrase' will parse off the predicate of a sentence.
%   Auxiliaries could be added as suggested.  Sensitivity to
%   tense would also be handy.
%
%   verb_phrase(
%       Struc : return structure,
%       Mpers : 'person' of subject input to modal,
%       Mnum  : 'number' of subject input to modal,
%       Mgap  : gap (if any) input to modal
%   )
%
%   'Xpers' and 'Xnum' represent constraints passed to the
%   verb phrase which may be altered by the components and
%   passed to the next component as 'Ypers' or 'Ynum', ie.
%   Mpers ==> Vpers.
%
%%%%%%%%%

verb_phrase(Struc,Mpers,Mnum,Mgap) -->
    modal(M,Mpers,Vpers,Mnum,Vbnum,Mgap,Vbgap),
    verb_bar(Vb,Vpers,Vbnum,Vbgap),
    {InStruc = vp(M,Vb)},
    conj(Struc,InStruc,Mpers,Mnum,vp).

%
%   'verb_bar' parses a verb followed by arguments, if any.
%   Auxiliaries can be handled as specifiers before the actual
%   verb is read.  Subcategorization (Scat) could also be made
%   more detailed.
%
%   verb_bar(
%       Struc : return structure,
%       Pers  : 'person' of subject (check for agreement),
%       Vnum  : 'number' of subject (check for agreement again),
%       Pgap  : transformed np from predicate (if any)
%   )
%
%   {Gapout = nogap} ensures that the parse doesn't end with
%   an unresolved structure being gapped.
%

verb_bar(Struc,Pers,Vnum,Pgap) -->
    verb(V,Pers,Vnum,Scat),
    predicate(P,Pgap,Gapout,Scat),
    {Gapout = nogap},
    {Struc = vbar(V,P)}.

%
%   'modal' accepts the specifier of a vp.  It should be
%   expanded to help compute the mood and tense of the
%   sentence.
%
%   modal(
%       Struc : return structure,
%       Mpers : 'person' of subject np,
%       Vpers : 'person' resulting from 'modal' ("nil" if found),
%       Mnum  : 'number' of subject np,
%       Vbnum : 'number' resulting from 'modal' ("inf" if found),
%       Mgap  : transformed modal (if any),
%       Vbgap : gap resulting from 'modal' (unchanged if found)
%   )
%

modal(Struc,_Mpers,Vpers,_Mnum,Vbnum,Mgap,Vbgap) --> [Word],
    {lexmodal(Word)}, {Vbgap = Mgap}, {Vbnum = inf},
    {Vpers = nil}, {Struc = modal(Word)}.
modal(Struc,_Mpers,Vpers,_Mnum,Vbnum,Mgap,Vbgap) --> [],
    {Mgap =.. [modal|[X]]}, {X \== nil}, {Vbgap = nogap}, {Vbnum = inf},
    {Vpers = nil}, {Struc = Mgap}.
modal(Struc,Mpers,Vpers,Mnum,Vbnum,Mgap,Vbgap) --> [],
    {Mgap = nogap}, {Vbgap = nogap}, {Vbnum = Mnum},
    {Vpers = Mpers}, {Struc = modal(nil)}.

%
%   'verb' parses the verb from the input if it is found in
%   the lexicon.  "lexverb" could contain more info on the
%   verb.
%
%   verb(
%       Struc : return structure,
%       Pers  : 'person' of the subject (for agreement check),
%       Vnum  : 'number' of the subject (for agreement check again!),
%       Scat  : SubCATegory of the verb =
%               dt (ditransitive : two objects) |
%               tv (transitive : one object) |
%               iv (intransitive : no objects)
%   )
%

verb(Struc,Pers,Vnum,Scat) --> [Word],
    {Pers \== third; Vnum = pl}, {lexverb(Scat,Word,_,_)},
    {Struc = verb(Word)}.
verb(Struc,Pers,Vnum,Scat) --> [Word],
    {Pers = third}, {Vnum = sg}, {lexverb(Scat,_,Word,_)},
    {Struc = verb(Word)}.
verb(Struc,_Pers,Vnum,Scat) --> [Word],
    {Vnum \== inf}, {lexverb(Scat,_,_,Word)},
    {Struc = verb(Word)}.

%
%   'predicate' parses the subcategorized dt, tv, or iv arguments
%   of the verb.
%
%   predicate(
%       Struc : return structure,
%       Pgap  : transformed np gap (if any),
%       Gapout: output any unresolved gap,
%       Scat  : SubCATegory to be returned
%   )
%

predicate(Struc,Pgap,Gapout,Scat) -->
    {Scat = dt},
    noun_phrase(Np1,_,_,nogap,_),
    noun_phrase(Np2,_,_,Pgap,Gapout),
    {Struc = pred(Np1,Np2)}.

predicate(Struc,Pgap,Gapout,Scat) -->
    {Scat = tv},
    noun_phrase(Np,_,_,Pgap,Gapout),
    {Struc = pred(Np)}.

predicate(Struc,Pgap,Gapout,Scat) -->
    {Scat = iv},
    [],
    {Gapout = Pgap},
    {Struc = pred(nil)}.

%%%%%%%%%
%   'prep_phrase' does the obvious.  Gapping could be introduced
%   to handle transformed pp's (but I doubt it :).
%
%   prep_phrase(
%       Struc : return structure
%   )
%
%%%%%%%%%

prep_phrase(Struc) -->
    preposition(P),
    noun_phrase(Np,_,_,nogap,_),
    {InStruc = pp(P,Np)},
    conj(Struc,InStruc,_,_,pp).

preposition(Struc) --> [Word], {lexprep(Word)}, {Struc = prep(Word)}.


%%%%%%%%%
%   'conj' will parse off a conjuction followed by a constituent
%   of category 'Cat'.  The result will be the right sister of
%   the previously parsed structure passed in.
%
%   conj(
%       OutStruc : result structure from conj,
%       InStruc  : previous structure parsed,
%       Arg1     : first constraint on constituent,
%       Arg2     : second constraint on constituent,
%       Cat      : category of new structure to be parsed
%   )
%
%   By McCawley's usage (McCawley 1988, Vol 1 & 2), constituents
%   should only be conjoined to others of the same category; ie.
%   np "and" np, vp "or" vp, etc.  If no conjunction is found
%   (conj-2,3), then the result structure is unchanged.
%
%%%%%%%%%

conj(OutStruc,InStruc,Arg1,Arg2,Cat) -->
    conjunction(C,Num),
    construct(Constr,Cat,Arg1,Arg2,Num),
    {OutStruc =.. [Cat,InStruc,C,Constr]}.

conj(Struc,Struc,_,_,vp) --> [].
conj(Struc,Struc,Arg,Arg,_) --> [].

conjunction(conj(Word),Num) --> [Word], {lexconj(Word,Num)}.

%
%   the meaning of the last three args for 'construct' depend
%   on which constituent is being parsed.  For np, the number
%   of the conjoined np is the 'number' of the first conjunction.
%   This is just a convenient heuristic.  For vp, the person
%   and number must still agree across conjunction.  For pp,
%   no such constraints are necessary.
%

construct(Struct,np,_,Num,Num) -->
    noun_phrase(Struct,_,_,nogap,_).
construct(Struct,vp,Pers,Vnum,_) -->
    verb_phrase(Struct,Pers,Vnum,nogap).
construct(Struct,pp,_,_,_) -->
    prep_phrase(Struct).


/*
Reading sentences
-----------------

Not used by the planning bug, but useful when testing.
*/


%%%%%%%%%
%   'read_sentence' provides the ability to get input
%   in a natural fashion by typing in words separated
%   by spaces and terminated with a period.  Adapted
%   from _Prolog and Natural Language Analysis_ by
%   Pereira and Schieber.
%
%%%%%%%%%

read_sentence(Input) :- get0(Char), read_sentence(Char,Input).
read_sentence(Char,[]) :- period(Char),!.
read_sentence(Char,Input) :- space(Char),!,get0(Char1),
    read_sentence(Char1,Input).
read_sentence(Char,[Word|Words]) :- read_word(Char,Chars,Next),
    name(Word,Chars),
    read_sentence(Next,Words).

read_word(C,[],C) :- space(C),!.
read_word(C,[],C) :- period(C),!.
read_word(Char,[Char|Chars],Last) :- get0(Next), read_word(Next,Chars,Last).

space(32).
period(46).


/*
Translating parse-trees to meanings
-----------------------------------
*/


/*  trans_sentence( Tree+, Meaning-, Type- ):
        Translates a parse tree into a meaning-representation
        and a type: one of 'assertion', 'command', 'question'.
*/
trans_sentence( s(NP,VP), Assertion, assertion ) :-
    !,
    trans_noun_phrase( NP, NPGoals, NPVar ),
    trans_verb_phrase( VP, Modal, Verb, ArgGoals, ArgVars ),
    combine_noun_verb( NPVar, Modal, Verb, ArgVars, VerbGoals ),
    Assertion = (NPGoals, ArgGoals, VerbGoals).

trans_sentence( q(NP,VP), Question, question ) :-
    trans_noun_phrase( NP, NPGoals, NPVar ),
    trans_verb_phrase( VP, Modal, Verb, ArgGoals, ArgVars ),
    combine_noun_verb( NPVar, Modal, Verb, ArgVars, VerbGoals ),
    conjoin( NPGoals, ArgGoals, VerbGoals, Question ).

trans_sentence( c(VP), Command, command ) :-
    trans_verb_phrase( VP, Modal, Verb, ArgGoals, ArgVars ),
    combine_noun_verb( me, Modal, Verb, ArgVars, VerbGoals ),
    Command = (ArgGoals, VerbGoals).


/*  combine_noun_verb( NPVar+, Modal+, Verb+, Vars+, VerbGoal- ):
        NPVar is the variable representing the subject of Verb.
        Vars are the variables representing its object(s).
        combine_noun_verb returns in VerbGoal a goal which
        represents the relation described by the verb, acting on
        NPVar and Vars.       
*/
combine_noun_verb( NPVar, _Modal, Verb, [], VerbGoals ) :-
    !,
    VerbGoals =.. [ Verb, NPVar ].

combine_noun_verb( NPVar, _Modal, Verb, [ArgVar1], VerbGoals ) :-
    !,
    VerbGoals =.. [ Verb, NPVar, ArgVar1 ].

combine_noun_verb( NPVar, _Modal, Verb, [ArgVar1,ArgVar2], VerbGoals ) :-
    VerbGoals =.. [ Verb, NPVar, ArgVar1, ArgVar2 ].


/*  trans_noun_phrase( Tree+, Goal-, Var- ):
        Translates Tree, representing a noun phrase, into a goal. Some
        of the predicates in the goal will take Var as their argument.
        Goal should be regarded as existentially quantifying Var thus:
            (there exists Var) such that Goal
        This represents the meaning of the phrase in that Var is the
        object denoted by it, and Goal the conditions that object must
        satisfy.
*/
:- discontiguous(trans_noun_phrase/3).

trans_noun_phrase( np(det(nil),NBar), NPGoals, NPVar ) :-
    !,
    trans_noun_bar( NBar, NPGoals, NPVar ).

trans_noun_phrase( np(det(the),NBar), the(NPVar,NPGoals), NPVar ) :-
    !,
    trans_noun_bar( NBar, NPGoals, NPVar ).

trans_noun_phrase( np(det(a),NBar), NPGoals, NPVar ) :-
    trans_noun_bar( NBar, NPGoals, NPVar ).


/*  trans_noun_bar( Tree+, Goal-, Var- ):
        Translates Tree, representing a noun-bar structure phrase, into
        a goal. Goal and Var represent an object, as above for
        trans_noun_phrase.
*/
trans_noun_bar( nbar(Adjectives,noun(Noun),Mod), NPGoals, NPVar ) :-
    trans_adjectives( Adjectives, NPVar, AdjGoals ),
    NounGoal =.. [ Noun, NPVar ],
    trans_noun_args( Mod, NPVar, ModGoals ),
    conjoin( NounGoal, AdjGoals, ModGoals, NPGoals ).


/*  trans_adjectives( Tree+, Goal-, Var- ):
        Translates Tree, representing a sequence of adjectives, into a
        goal. Goal and Var represent the meaning of these adjectives,
        in that any object Var which is describable by them must satisfy
        the conditions in Goal.
*/
trans_adjectives( adjectives(nil), _, true ) :- !.

trans_adjectives( adjectives(Adj,Rest), NPVar, AdjGoals ) :-
    trans_adjectives( Rest, NPVar, RestGoals ),
    AdjGoal =.. [ Adj, NPVar ],
    conjoin( AdjGoal, RestGoals, AdjGoals ).


/*  trans_noun_args( Tree+, Goal-, Var- ):
        The "arguments" of a noun are the following prepositional
        phrases, if any. trans_noun_args translates a
        prepositional-phrase-tree into a goal and variable representing
        a condition on the noun, as for trans_adjectives.
*/
trans_noun_args( n_args(nil), _, true ) :- !.

trans_noun_args( n_args(PP), NPVar, ModGoals  ) :-
    trans_prep_phrase( PP, Prep, ArgGoals, ArgVar ),
    PrepGoal =.. [ Prep, NPVar, ArgVar ],
    conjoin( ArgGoals, PrepGoal, ModGoals ).


/*  trans_verb_phrase( Tree+, Modal-, Verb-, Goal-, Vars- ):
        Translates Tree into a Verb, Goal, and Vars as for
        trans_verb_bar, and isolates the modal verb into Modal.
*/
trans_verb_phrase( vp(Modal,VerbBar), Modal, Verb, ArgGoals, ArgVars ) :-
    trans_verb_bar( VerbBar, Verb, ArgGoals, ArgVars ).


/*  trans_verb_bar( Tree+, Verb-, Goal-, Vars- ):
        Tree represents a verb-bar phrase. trans_verb_bar translates the
        verb's "argument" (the predicate: see below) into Goal and Vars,
        and isolates the verb into Verb.
*/
trans_verb_bar( vbar(verb(Verb),P), Verb, ArgGoals, ArgVars ) :-
    trans_predicate( P, ArgGoals, ArgVars ).


/*  trans_predicate( Tree+, Goal-, Vars- ):
        Tree represents a predicate, in the grammatical sense: the
        object(s) of a verb. These are noun phrases. trans_predicate
        translates each of them and returns a conjunction of the goals
        defining each phrase, and a list of variables representing the
        things referred to.
*/
:- discontiguous (trans_predicate/3).
trans_predicate( pred(NP1,NP2), (NP1Goals,NP2Goals), [NP1Var,NP2Var] ) :-
    !,
    trans_noun_phrase( NP1, NP1Goals, NP1Var ),
    trans_noun_phrase( NP2, NP2Goals, NP2Var ).

trans_predicate( pred(NP), NPGoals, [NPVar] ) :-
    !,
    trans_noun_phrase( NP, NPGoals, NPVar ).

trans_predicate( pred(nil), true, [] ) :- !.


/*  trans_prep_phrase( Tree+, Prep-, Goal-, Var- ):
        Tree is the parse-tree for a prepositional phrase.
        trans_prep_phrase translates its noun-phrase part into Goal and
        Var, and extracts the preposition into Prep.
*/
trans_prep_phrase( pp(prep(Prep),NP), Prep, NPGoals, NPVar ) :-
    trans_noun_phrase( NP, NPGoals, NPVar ).


/*
Joining goals together
----------------------
*/


/*  conjoin( G1+, G2+, G- ):
        G is the goal representing (G1 and G2). conjoin optimises
        out redundant trues.
*/
conjoin( true, G, G ) :- !.
conjoin( G, true, G ) :- !.
conjoin( G1, G2, (G1,G2) ) :- !.


/*  conjoin( G1+, G2+, G3, G- ):
        G is the goal representing (G1 and G2 and G3).
*/
conjoin( G1, G2, G3, G1G2G3 ) :-
    conjoin( G2, G3, G2G3 ),
    conjoin( G1, G2G3, G1G2G3 ).


/*  conjoin( G1+, G2+, G3+, G4+, G- ):
        G is the goal representing (G1 and G2 and G3 and G4).
*/
conjoin( G1, G2, G3, G4, G1G2G3G4 ) :-
    conjoin( G2, G3, G4, G2G3G4 ),
    conjoin( G1, G2G3G4, G1G2G3G4 ).


/*
Notes
-----

This is a documentation file, also written by Cameron, about how
the parser works, and extensions to it.

    Introduction to Gibberish
    -------------------------

The Gibberish parser has a limited but flexible parsing ability.
The code contains comments suggesting how Gibberish can be
modified to handle the sentences in file test2.  As a preliminary,
the student is advised to look at the simpler sentences in test1
and note how Gibberish handles them and why they are correct or
incorrect.  Comparing those sentences with the ones in test2 may
suggest what coverage this parser lacks, and what structures will
need to be added.  The following are a few hints about those
structures and how Gibberish can be expanded to suit:

    1)  the 'gapping' mechanism ("gap" is a term for a missing
        sentence constituent) has been implemented in full
        generality, so that any structure can be passed
        through as a gap.  In fact, gapping of noun phrases (NP's)
        has already been accomodated in the verb predicate,
        but it has not been utilized.  You *may* also wish
        to rewrite the prepositional phrase (PP) rule so that
        it can also accept gaps (hint)!

    2)  For an example of how the gapping works, look at the
        second "sentence" rule -- where the sentence is a
        question in which the modal has been 'moved' out of
        'normal' position to the front of the input.  The
        modal (if one is found, of course) is parsed unconditionally
        and its parsed structure is placed in the gap slot of
        the verb phrase (VP).  Notice in the VP rule that that
        structure is passed directly to the modal rule.  The
        second modal rule checks (since no modal has been
        found on the input) to see if the gap contains a modal
        passed in from the previous parsing.  The check is
        found in the Prolog goal "{Mgap =.. [modal|_]}".  If
        a modal is found, the output gap (Vbgap) is set to
        nogap (ie. the gap has been consumed) and the other
        variables are set to the appropriate state.  Now look
        at the first NP rule...

    3)  A sentence can only be judged correct if the gap has
        been consumed during the parse so that nothing is left
        over at the end of the sentence unresolved.  This is
        handled by the Prolog goal "{Gapout = nogap}" in the
        verb_bar rule.  If this equality fails, the parser 
        must backtrack.

    4)  This suggests that a profitable method of finding more
        general structures for parsing is to re-write sentences
        into a 'normal' form, ie.

        Does he know you stole his car.

        becomes:

        He does know you stole his car.

        A further possibility is:

        What do you want.

        becomes:

        You do want what.

    5)  In light of this, it is obvious that Gibberish's VP
        rules have to be expanded to include 'auxiliaries'
        and 'operators'.  'Wh-constituents' can also be worked
        into a similar scheme.  Of the roughly ten different
        'verb' tenses, Gibberish handles only three (hint)!

    6)  Another structure lacking in Gibberish is the sentence_bar
        (Sbar).  An example would be "for her to argue", or
        "having pleasure at the office".  The parser should be
        expanded to accomodate these as possible subjects and
        objects of verbs (and could be sub-categorized for).
        One possibility, in "noun_args", has been commented out.

    7)  The conjunction handling includes a simple mechanism
        to make conjoined structures agree much like simple ones.
        More structures than np, vp, and pp can be conjoined.
        In particular, the test sentence:

        a cat or dog will go and see the stamp.

        doesn't parse because the vp-level 'conj' expects both
        "will go" and "see" to be third & sg.  Conjunction at
        the vbar level will be needed to handle such a sentence.
        The nbar rule could use a similar arrangement.

    8)  Other areas of note: imperatives, adjectives, adverbs
        (and the phrases headed by both),  mood, voice, the
        verb "be",  the copula "be", pronouns, idioms, etc.
*/

/*  GB_LEX.PL  */


/*
This is the lexicon used with GB.PL, for PLANNING_BUG.PL. It doesn't
handle the parts of 'be' properly, and (of course) this form of
representation is no use for highly inflected languages.
*/


/*  lexdet( Word+, Syn+, Number+ ):
*/
lexdet(the,the,_).
lexdet(a,a,sg).
lexdet(an,a,sg).


/*  lexadjective( Word+ ):
*/
lexadjective(big).
lexadjective(small).


/*  lexnoun( WordSing+, WordPlur+ ):
*/
lexnoun(boulder,boulders).
lexnoun(bug,bugs).
lexnoun(door,doors).
lexnoun(food,food).
lexnoun(hammer,hammers).
lexnoun(key,keys).
lexnoun(quicksand,quicksands).
lexnoun(rock,rocks).


/*  lexmodal( Word+ ):
*/
lexmodal(can).
lexmodal(could).
lexmodal(do).
lexmodal(does).
lexmodal(shall).
lexmodal(should).
lexmodal(will).
lexmodal(would).


/*  lexverb( Trans-, Pres, Pres3, Past ):
*/
lexverb(tv,drop,drops,dropped).
lexverb(tv,eat,eats,ate).
lexverb(dt,give,gives,gave).
lexverb(iv,go,goes,went).
lexverb(tv,grab,grabs,grabbed).
lexverb(tv,have,has,had).
lexverb(tv,zzzz,is,was).
lexverb(tv,open,opens,opened).
lexverb(tv,see,sees,saw).
lexverb(tv,smash,smashes,smashed).
lexverb(tv,use,uses,used).
lexverb(tv,want,wants,wanted).


/*  lexprep( Word+ ):
*/
lexprep(by).
lexprep(for).
lexprep(from).
lexprep(in).
lexprep(near).
lexprep(of).
lexprep(on).
lexprep(to).
lexprep(with).


/*  lexconj( Word+, Number ):
*/
lexconj(and,pl).
lexconj(or,sg).


