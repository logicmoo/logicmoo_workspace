/*  Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2017, VU University Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(wordnet,
	  [ wn_s/6,			% basic Wordnet relations
	    wn_g/2,
	    wn_hyp/2,
	    wn_ins/2,
	    wn_ent/2,
	    wn_sim/2,
	    wn_mm/2,
	    wn_ms/2,
	    wn_mp/2,
	    wn_der/4,
	    wn_cls/5,
	    wn_cs/2,
	    wn_vgp/4,
	    wn_at/2,
	    wn_ant/4,
	    wn_sa/4,
	    wn_sk/3,
	    wn_syntax/3,
	    wn_ppl/4,
	    wn_per/4,
	    wn_fr/3,

	    wn_cat/3,			% +SynSet, -SyntacticCategory, -Offset
	    ss_type/2,			% +Code, -Type

	    load_wordnet/0		% force loading everything
	  ]).

/** <module> Wordnet lexical and semantic database

This module discloses the Wordnet  Prolog   files  is  a more SWI-Prolog
friendly manner. It exploits SWI-Prolog   demand-loading  and SWI-Prolog
Quick Load Files to load `just-in-time' and as quickly as possible.

The system creates Quick Load Files for  each wordnet file needed if the
.qlf file doesn't exist and  the   wordnet  directory  is writeable. For
shared installations it is adviced to   run  load_wordnet/0 as user with
sufficient privileges to create the Quick Load Files.

This library defines a portray/1 rule to explain synset ids.

Some more remarks:

 - SynSet identifiers are large numbers. Such numbers require
   significant more space on the stacks but not in clauses and
   therefore it is not considered worthwhile to strip the
   type info represented in the most significant digit.

 - On wordnet 2.0, the syntactic category deduced from the
   synset id is consistent with the 4th argument of s/6, though
   both adjective and adjective_satellite are represented as
   3XXXXXXXX

@author Originally by Jan Wielemaker. Partly documented by Samer
Abdallah. Current comments copied from prologdb.5WN.html file from the
sources.
@see Wordnet is a lexical database for the English language.
See http://www.cogsci.princeton.edu/~wn/
*/


		 /*******************************
		 *          FIND WORDNET	*
		 *******************************/

:- multifile user:file_search_path/2.

user:file_search_path(wndb, WNDB) :-
    (   getenv('WNDB', WNDB)
    ->  true
    ;   current_prolog_flag(windows, true)
    ->  WNDB = 'C:\\Program Files\\WordNet\\3.0'
    ;   WNDB = '/usr/local/WordNet-3.0'
    ).

haswndb :-
    absolute_file_name(wndb(wn_s), _,
                       [ file_type(prolog),
                         access(read),
                         file_errors(fail)
                       ]).
checkwndb :-
    haswndb,
    !.
checkwndb :-
    print_message(error, wordnet(nodb)).

:- initialization
    checkwndb.


%!  wn_op(PredSpec) is nondet.
%
%   Definition of wordnet operator types.

wn_op(ant(synset_id, w_num, synset_id, w_num)).
wn_op(at(synset_id, synset_id)).
wn_op(cls(synset_id, w_num, synset_id, wn_num, class_type)).
wn_op(cs(synset_id, synset_id)).
wn_op(der(synset_id, w_num, synset_id, wn_num)).
wn_op(ent(synset_id, synset_id)).
wn_op(fr(synset_id, w_num, f_num)).
wn_op(g(synset_id, '(gloss)')).
wn_op(hyp(synset_id, synset_id)).
wn_op(ins(synset_id, synset_id)).
wn_op(mm(synset_id, synset_id)).
wn_op(mp(synset_id, synset_id)).
wn_op(ms(synset_id, synset_id)).
wn_op(per(synset_id, w_num, synset_id, w_num)).
wn_op(ppl(synset_id, w_num, synset_id, w_num)).
wn_op(s(synset_id, w_num, 'word', ss_type, sense_number, tag_count)).
wn_op(sa(synset_id, w_num, synset_id, w_num)).
wn_op(sim(synset_id, synset_id)).
wn_op(sk(synset_id, w_num, sense_key)).
wn_op(syntax(synset_id, w_num, syntax)).
wn_op(vgp(synset_id, w_num, synset_id, w_num)).

:- if(current_prolog_flag(xref, true)).

% this  declaration  ensures  that  the  (ClioPatria)  cross  referencer
% considers this predicates defined. We should not really define them as
% handling these predicates is based on autoloading.

:- dynamic
    ant/4,
    at/2,
    cls/5,
    cs/2,
    der/4,
    ent/2,
    fr/3,
    g/2,
    hyp/2,
    ins/2,
    mm/2,
    ms/2,
    per/4,
    ppl/4,
    s/6,
    sa/4,
    sim/2,
    sk/3,
    syntax/3,
    vgp/4.

:- endif.

:- export((
    ant/4,
    at/2,
    cls/5,
    cs/2,
    der/4,
    ent/2,
    fr/3,
    g/2,
    hyp/2,
    ins/2,
    mm/2,
    ms/2,
    per/4,
    ppl/4,
    s/6,
    sa/4,
    sim/2,
    sk/3,
    syntax/3,
    vgp/4)).
:- multifile((
    ant/4,
    at/2,
    cls/5,
    cs/2,
    der/4,
    ent/2,
    fr/3,
    g/2,
    hyp/2,
    ins/2,
    mm/2,
    ms/2,
    per/4,
    ppl/4,
    s/6,
    sa/4,
    sim/2,
    sk/3,
    syntax/3,
    vgp/4)).

:- dynamic((
    ant/4,
    at/2,
    cls/5,
    cs/2,
    der/4,
    ent/2,
    fr/3,
    g/2,
    hyp/2,
    ins/2,
    mm/2,
    ms/2,
    per/4,
    ppl/4,
    s/6,
    sa/4,
    sim/2,
    sk/3,
    syntax/3,
    vgp/4)).
:- public
    ant/4,
    at/2,
    cls/5,
    cs/2,
    der/4,
    ent/2,
    fr/3,
    g/2,
    hyp/2,
    ins/2,
    mm/2,
    ms/2,
    per/4,
    ppl/4,
    s/6,
    sa/4,
    sim/2,
    sk/3,
    syntax/3,
    vgp/4.

		 /*******************************
		 *    WORDNET BASIC RELATIONS   *
		 *******************************/

%!  wn_ant(?Antonym1, ?Wnum1, ?Antonym2, ?WNum2) is nondet.
%
%   The ant operator specifies antonymous  word   s.  This  is a lexical
%   relation  that  holds  for  all    syntactic  categories.  For  each
%   antonymous pair, both relations are listed (ie. each synset_id,w_num
%   pair is both a source and target word.)

wn_ant(Antonym1, Wnum1, Antonym2, WNum2) :- ant(Antonym1, Wnum1, Antonym2, WNum2).

%!  wn_at(?Noun, ?Adjective) is nondet.
%
%   The at operator defines the  attribute   relation  between  noun and
%   adjective synset pairs in which the  adjective   is  a  value of the
%   noun. For each pair, both relations   are listed (ie. each synset_id
%   is both a source and target).

wn_at(Noun, Adjective) :- at(Noun, Adjective).

%!  wn_cls(?SynSet, ?W1, ?Class, ?W2, ?ClassType) is nondet.
%
%   The cls operator specifies that the first synset has been classified
%   as a member of the class represented by the second synset. Either of
%   the w_num's can be 0, reflecting that the pointer is semantic in the
%   original WordNet database.

wn_cls(SynSet, W1, Class, W2, ClassType) :-
    cls(SynSet, W1, Class, W2, ClassType).

%!  wn_cs(?SynSet, ?Causes) is nondet.
%
%   First kind of event is caused by second.
%
%   The cs operator specifies that the second   synset is a cause of the
%   first synset. This relation only holds for verbs.

wn_cs(SynSet, Causes) :-
    cs(SynSet, Causes).

%!  wn_der(?SynSet1, ?W1, ?SynSet2, ?W2) is nondet.
%
%   The der operator specifies that  there   exists  a reflexive lexical
%   morphosemantic relation between the first   and  second synset terms
%   representing derivational morphology.

wn_der(SynSet1, W1, SynSet2, W2) :-
    der(SynSet1, W1, SynSet2, W2).

%!  wn_ent(?SynSet, ?Entailment) is nondet.
%
%   The ent operator specifies that the   second synset is an entailment
%   of first synset. This relation only holds for verbs.

wn_ent(SynSet, Entailment) :-
    ent(SynSet, Entailment).

%!  wn_fr(?Synset, ?Wnum, ?Fnum) is nondet.
%
%   fr operator specifies a generic sentence frame  for one or all words
%   in a synset. The operator is defined only for verbs.

wn_fr(Synset, Wnum, Fnum) :-
    fr(Synset, Wnum, Fnum).

%!  wn_g(?SynSet, ?Gloss) is nondet.
%
%   The g operator specifies the gloss for a synset.

wn_g(SynSet, Gloss) :-
    g(SynSet, Gloss).

%!  wn_hyp(?Hyponym, ?HyperNym) is nondet.
%
%   The hyp operator specifies that the second   synset is a hypernym of
%   the first synset. This  relation  holds   for  nouns  and verbs. The
%   reflexive operator, hyponym, implies that  the   first  synset  is a
%   hyponym of the second synset.

wn_hyp(Hyponym, HyperNym) :-
    hyp(Hyponym, HyperNym).

%!  wn_ins(?A,?B) is nondet.
%
%   The ins operator specifies that the first   synset is an instance of
%   the second synset. This relation  holds   for  nouns.  The reflexive
%   operator,  has_instance,  implies  that  the  second  synset  is  an
%   instance of the first synset.

wn_ins(A,B) :- ins(A,B).

%!  wn_mm(?SynSet, ?MemberMeronym) is nondet.
%
%   The mm operator specifies that the second synset is a member meronym
%   of the first  synset.  This  relation   only  holds  for  nouns. The
%   reflexive operator, member holonym, can be implied.

wn_mm(SynSet, MemberMeronym) :-
    mm(SynSet, MemberMeronym).

%!  wn_mp(?SynSet, ?PartMeronym) is nondet.
%
%   The mp opeQrator specifies that the second synset is a part meronym
%   of the first synset. This relation only holds for nouns. The
%   reflexive operator, part holonym, can be implied.

wn_mp(SynSet, PartMeronym) :-
    ms(SynSet, PartMeronym).

%!  wn_ms(?SynSet, ?SubstanceMeronym) is nondet.
%
%   The ms operator specifies that  the   second  synset  is a substance
%   meronym of the first synset. This relation only holds for nouns. The
%   reflexive operator, substance holonym, can be implied.

wn_ms(SynSet, SubstanceMeronym) :-
    ms(SynSet, SubstanceMeronym).

%!  wn_per(?Synset1, ?WNum1, ?Synset2, ?WNum2) is nondet.
%
%   The per operator specifies two  different   relations  based  on the
%   parts of speech involved. If  the  first   word  is  in an adjective
%   synset, that word pertains to either   the  noun or adjective second
%   word. If the first word is in an adverb synset, that word is derived
%   from the adjective second word.

wn_per(Synset1, WNum1, Synset2, WNum2) :-
    per(Synset1, WNum1, Synset2, WNum2).

%!  wn_ppl(?Synset1, ?WNum1, ?Synset2, ?WNum2) is nondet.
%
%   ppl operator specifies that the adjective first word is a participle
%   of the verb second word. The reflexive operator can be implied.

wn_ppl(Synset1, WNum1, Synset2, WNum2) :-
    ppl(Synset1, WNum1, Synset2, WNum2).

%!  wn_s(?SynSet, ?WNum, ?Word, ?SynSetType, ?Sense, ?Tag) is nondet.
%
%   A s operator is present for every word sense in WordNet. In wn_s.pl,
%   w_num specifies the word number for word in the synset.

wn_s(SynSet, WNum, Word, SynSetType, Sense, Tag) :-
    s(SynSet, WNum, Word, SynSetType, Sense, Tag).

%!  wn_sa(?Synset1, ?WNum1, ?Synset2, ?WNum2) is nondet.
%
%   The sa operator specifies  that   additional  information  about the
%   first word can be obtained by seeing  the second word. This operator
%   is only defined for verbs  and   adjectives.  There  is no reflexive
%   relation (ie. it cannot be inferred  that the additional information
%   about the second word can be obtained from the first word).

wn_sa(Synset1, WNum1, Synset2, WNum2) :-
    sa(Synset1, WNum1, Synset2, WNum2).

%!  wn_sim(?SynSet, ?Similar) is nondet.
%
%   The sim operator specifies that  the   second  synset  is similar in
%   meaning to the first synset. This means  that the second synset is a
%   satellite the first synset, which is the cluster head. This relation
%   only holds for adjective synsets contained in adjective clusters.

wn_sim(SynSet, Similar) :-
    sim(SynSet, Similar).

%!  wn_sk(?A,?B,?C) is nondet.
%
%   A sk operator is present for every word sense in WordNet. This gives
%   the WordNet sense key for each word sense.

wn_sk(A,B,C) :-
    sk(A,B,C).

%!  wn_syntax(?A,?B,?C) is nondet.
%
%   The syntax operator specifies the syntactic  marker for a given word
%   sense if one is specified.

wn_syntax(A,B,C) :-
    syntax(A,B,C).

%!  wn_vgp(?Verb, ?W1, ?Similar, ?W2) is nondet.
%
%   vgp operator specifies verb synsets that  are similar in meaning and
%   should be grouped together when displayed   in response to a grouped
%   synset search.

wn_vgp(Verb, W1, Similar, W2) :-
    vgp(Verb, W1, Similar, W2).


		 /*******************************
		 *	   CODE MAPPINGS	*
		 *******************************/

%!	wn_cat(+SynSet, -SyntacticCategory, -Offset) is det.
%
%	Break the synset id into its   syntactic  category and offset as
%	defined in the manpage prologdb.5

wn_cat(SynSet, Category, Small) :-
	Small is SynSet mod 100000000,
	CatNum is SynSet // 100000000,
	wn_cat(CatNum, Category).

wn_cat(1, noun).
wn_cat(2, verb).
wn_cat(3, adjective).
wn_cat(4, adverb).

%!	ss_type(+Code, -Type) is det.
%!	ss_type(-Code, -Type) is nondet.
%
%	Mapping between readable syntactic category and code.

ss_type(n, noun).
ss_type(v, verb).
ss_type(a, adjective).
ss_type(s, adjective_satellite).
ss_type(r, adverb).


%!	load_wordnet is det.
%
%	Load all of wordnet.  This must be used to create all .QLF
%	files or before creating a stand-alone saved state

load_wordnet :-
	(   wn_op(O),
	    functor(O, Name, _),
	    load_op(Name),
	    fail
	;   true
	).

load_op(Name) :-
	atom_concat('wn_', Name, File),
        load_files(wndb(File),
                   [ qcompile(auto)
                   ]).


		 /*******************************
		 *     JUST IN TIME LOADING	*
		 *******************************/

:- multifile user:exception/3.

user:exception(undefined_predicate, wordnet:Name/Arity, retry) :-
	functor(Op, Name, Arity),
	wordnet:((call(call,wn_op(Op)),
                  call(call,load_op(Name)))).


		 /*******************************
		 *            MESSAGES		*
		 *******************************/

:- multifile prolog:message//1.

prolog:message(wordnet(nodb)) -->
    [ 'Cannot find WordNet data files.  Please set the environment'-[], nl,
      'variable WNDB to point at the directory holding the WordNet files'-[]
    ].

/*
:- include('WNprolog-3.0/prolog/wn_ant.pl').
:- include('WNprolog-3.0/prolog/wn_at.pl').
:- include('WNprolog-3.0/prolog/wn_cls.pl').
:- include('WNprolog-3.0/prolog/wn_cs.pl').
:- include('WNprolog-3.0/prolog/wn_der.pl').
:- include('WNprolog-3.0/prolog/wn_ent.pl').
:- include('WNprolog-3.0/prolog/wn_fr.pl').
:- include('WNprolog-3.0/prolog/wn_g.pl').
:- include('WNprolog-3.0/prolog/wn_hyp.pl').
:- include('WNprolog-3.0/prolog/wn_ins.pl').
:- include('WNprolog-3.0/prolog/wn_mm.pl').
:- include('WNprolog-3.0/prolog/wn_mp.pl').
:- include('WNprolog-3.0/prolog/wn_ms.pl').
:- include('WNprolog-3.0/prolog/wn_per.pl').
:- include('WNprolog-3.0/prolog/wn_ppl.pl').
:- include('WNprolog-3.0/prolog/wn_s.pl').
:- include('WNprolog-3.0/prolog/wn_sa.pl').
:- include('WNprolog-3.0/prolog/wn_sim.pl').
:- include('WNprolog-3.0/prolog/wn_sk.pl').
:- include('WNprolog-3.0/prolog/wn_syntax.pl').
:- include('WNprolog-3.0/prolog/wn_vgp.pl').
*/

end_of_file.



vgp(201346978,0,201345109,0).
vgp(201345109,0,201346978,0).
sim(302096382,302096213).
sim(302096213,302096382).
sa(201348174,1,201348452,1).
sa(201348174,1,201347678,7).
sa(201345109,2,201587062,4).
sa(201345109,2,201347678,5).
s(302096382,5,'unsecured',s,1,0).
s(302096382,4,'unlocked',s,1,2).
s(302096382,3,'unlatched',s,1,0).
s(302096382,2,'unbolted',s,1,0).
s(302096382,1,'unbarred',s,1,0).
s(201348705,1,'unlock',v,1,4).
s(201348174,1,'lock',v,1,8).
s(201346978,2,'shut',v,2,2).
s(201346978,1,'close',v,2,20).
s(201346003,2,'open up',v,1,2).
s(201346003,1,'open',v,1,66).
s(201345109,2,'shut',v,1,10).
s(201345109,1,'close',v,1,32).
s(103683606,1,'locker',n,2,0).
s(103682487,1,'lock',n,1,6).
mp(104497005,103682487).
mp(103682487,103661340).
mp(103682487,103427296).
mp(103682487,103233905).
mp(103682487,103221720).
mp(103614782,103682487).
mp(102865931,103682487).
hyp(201603885,201346003).
hyp(201593614,201346003).
hyp(201593254,201346003).
hyp(201423793,201346003).
hyp(201354006,201345109).
hyp(201353873,201346003).
hyp(201348987,201346003).
hyp(201348838,201348174).
hyp(201348705,201346003).
hyp(201348174,201340439).
hyp(201346978,200146138).
hyp(201346693,201346003).
hyp(201346548,201346003).
hyp(201346430,201346003).
hyp(201345769,201345109).
hyp(201345589,201345109).
hyp(201343079,201346003).
hyp(201342012,201348174).
hyp(201243148,201345109).
hyp(201242996,201345109).
hyp(201242832,201345109).
hyp(201220528,201345109).
hyp(200355670,201345109).
hyp(104136800,103682487).
hyp(103874599,103682487).
hyp(103683606,103323703).
hyp(103682487,103323703).
hyp(103659950,103682487).
hyp(103645011,103682487).
hyp(103223162,103682487).
hyp(103156767,103682487).
hyp(103075370,103682487).
g(302096382,'not firmly fastened or secured; "an unbarred door"; "went through the unlatched gate into the street"; "an unlocked room"').
g(201348705,'open the lock of; "unlock the door"').
g(201348174,'fasten with a lock; "lock the bike to the fence"').
g(201346978,'become closed; "The windows closed with a loud bang"').
g(201346003,'cause to open or to become open; "Mary opened the car door"').
g(201345109,'move so that an opening or passage is obstructed; make shut; "Close the door"; "shut the window"').
g(103683606,'a fastener that locks or closes').
g(103682487,'a fastener fitted to a door or drawer to keep it firmly closed').
fr(201348705,0,8).
fr(201348705,0,21).
fr(201348705,0,11).
fr(201348174,0,8).
fr(201348174,0,21).
fr(201348174,0,20).
fr(201348174,0,11).
fr(201346978,0,1).
fr(201346003,0,8).
fr(201346003,0,11).
fr(201345109,0,8).
fr(201345109,0,11).
der(201348174,1,103683606,1).
der(201348174,1,103682487,1).
der(201346978,2,104211528,1).
der(201346978,1,100344040,2).
der(201346003,1,110737431,2).
der(201346003,1,103848348,1).
der(201346003,1,100383390,1).
der(201346003,1,100338641,1).
der(201345109,2,104211528,1).
der(201345109,2,104211356,1).
der(201345109,2,100344040,1).
der(201345109,1,101074694,2).
der(201345109,1,100344040,2).
der(110737431,2,201346003,1).
der(104211528,1,201346978,2).
der(104211528,1,201345109,2).
der(104211356,1,201345109,2).
der(103848348,1,201346003,1).
der(103683606,1,201348174,1).
der(103682487,1,201348174,1).
der(101074694,2,201345109,1).
der(100383390,1,201346003,1).
der(100344040,2,201346978,1).
der(100344040,2,201345109,1).
der(100344040,1,201345109,2).
der(100338641,1,201346003,1).
cs(201346003,201346804).
cs(201345109,201346978).
ant(201348705,1,201348174,1).
ant(201348174,1,201348705,1).
ant(201348174,1,200219963,1).
ant(201346978,1,201346804,1).
ant(201346804,1,201346978,1).
ant(201346003,1,201345109,1).
ant(201345109,1,201346003,1).
ant(200219963,1,201348174,1).


vgp(202767760,0,202764765,0).
vgp(202764765,0,202767760,0).
vgp(200002573,0,200001740,0).
vgp(200002325,0,200001740,0).
vgp(200001740,0,200002573,0).
vgp(200001740,0,200002325,0).
syntax(302725548,1,ip).
syntax(302687822,1,ip).
syntax(300020410,4,p).
syntax(300020103,1,a).
syntax(300019731,2,p).
syntax(300014358,2,ip).
sk(302095936,1,'fastened%3:00:00::').
sim(302598110,302597951).
sim(302597951,302598110).
sim(302096604,302096213).
sim(302096382,302096213).
sim(302096213,302096604).
sim(302096213,302096382).
sim(302096083,302095936).
sim(302095936,302096083).
sim(300003553,300003356).
sim(300003356,300003829).
sim(300003356,300003700).
sim(300003356,300003553).
sa(302588099,0,302502163,0).
sa(302588099,0,302037272,0).
sa(200005815,1,200006238,3).
sa(200005815,1,200006238,2).
sa(200001740,1,200005041,3).
sa(200001740,1,200004227,3).
s(400516492,1,'wrongfully',r,1,0).
s(400516401,1,'wafer-thin',r,1,0).
s(302096382,5,'unsecured',s,1,0).
s(302096382,4,'unlocked',s,1,2).
s(302096382,3,'unlatched',s,1,0).
s(302096382,2,'unbolted',s,1,0).
s(302096382,1,'unbarred',s,1,0).
s(302096213,1,'unfastened',a,1,1).
s(302095936,1,'fastened',a,1,1).
s(302087178,2,'opened',s,3,0).
s(302086879,1,'unopened',s,1,0).
s(301654377,2,'opened',a,1,2).
s(301653135,1,'opened',s,2,1).
s(201511289,1,'unlock',v,2,1).
s(201348705,1,'unlock',v,1,4).
s(200219963,1,'unlock',v,3,1).
s(100002137,2,'abstract entity',n,1,0).
s(100002137,1,'abstraction',n,6,0).
s(100001930,1,'physical entity',n,1,0).
s(100001740,1,'entity',n,1,11).
ppl(303155306,1,200538571,1).
ppl(303155193,1,200538571,1).
ppl(303147643,1,201589497,1).
ppl(303147543,1,201624897,1).
ppl(303147408,1,201153486,2).
ppl(303147281,1,201153486,2).
per(400516492,1,301371009,1).
per(400516401,1,302415294,1).
per(302598981,1,105556943,1).
per(302598768,1,102667379,1).
per(302598608,2,114549070,1).
per(302598608,1,114549070,1).
ms(115108087,115108745).
ms(115098161,104277669).
ms(102828427,102827606).
ms(102751782,104410190).
ms(102158494,107586485).
ms(101896844,103266749).
mp(115300051,115212739).
mp(115299225,115203229).
mp(100071700,100482298).
mp(100042541,113448334).
mp(100027167,100028651).
mp(100006484,100004475).
mm(114726124,114666012).
mm(114725941,114666012).
mm(100390198,106351202).
mm(100017222,111529603).
mm(100015388,101313093).
mm(100007846,107942152).
ins(115300051,101246697).
ins(115298283,107314078).
ins(100237869,100235435).
ins(100208141,100207761).
ins(100060817,100058743).
ins(100060548,100058743).
hyp(202772310,202762468).
hyp(202772202,202770717).
hyp(201511289,201511380).
hyp(100002684,100001930).
hyp(100002452,100001930).
hyp(100002137,100001740).
hyp(100001930,100001740).
g(400516492,'in an unjust or unfair manner; "the employee claimed that she was wrongfully dismissed"; "people who were wrongfully imprisoned should be released"').
g(400516401,'very thin; "it was cut wafer-thin"').
g(302096382,'not firmly fastened or secured; "an unbarred door"; "went through the unlatched gate into the street"; "an unlocked room"').
g(302096213,'not closed or secured; "the car door was unfastened"; "unfastened seatbelts"').
g(302095936,'firmly closed or secured; "found the gate fastened"; "a fastened seatbelt"').
g(201511289,'set free or release').
g(100002452,'a separate and self-contained entity').
g(100002137,'a general concept formed by extracting common features from specific examples').
g(100001930,'an entity that has physical existence').
g(100001740,'that which is perceived or known or inferred to have its own distinct existence (living or nonliving)').
fr(202772310,0,8).
fr(202772310,0,11).
fr(201511289,0,8).
fr(201511289,0,11).
fr(200002573,0,2).
fr(200002325,0,2).
fr(200001740,0,8).
fr(200001740,0,2).
ent(202770535,202769900).
ent(202770170,202756558).
ent(200015713,200014742).
ent(200004819,200004227).
ent(200001740,200005041).
ent(200001740,200004227).
der(400428572,2,301822563,1).
der(100003553,2,201462005,4).
der(100003553,1,300784215,3).
der(100002684,1,200532607,5).
der(100002137,1,200692329,1).
cs(202763283,202763740).
cs(202762468,200377002).
cs(200025654,200026385).
cs(200025203,200026153).
cs(200020449,200020259).
cs(200019273,200014742).
cls(400513248,0,107020895,0,t).
cls(400492745,0,107073447,0,u).
cls(100036580,0,107105475,0,u).
cls(100029114,0,106090869,0,t).
cls(100017222,0,106066555,0,t).
cls(100006484,0,106037666,0,t).
at(302588099,104806804).
at(302584981,104806804).
at(100844254,301201422).
at(100844254,301201100).
at(100033615,302297166).
at(100033615,302295998).
ant(400512597,1,400512503,1).
ant(400512503,1,400512597,1).
ant(302096213,1,302095936,1).
ant(302095936,1,302096213,1).
ant(100047550,1,100047356,1).
ant(100047356,1,100047550,1).
ant(100021939,1,100019128,1).
ant(100019128,1,100021939,1).

