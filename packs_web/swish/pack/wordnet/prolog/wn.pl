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
	wn_op(Op),
	load_op(Name).


		 /*******************************
		 *            MESSAGES		*
		 *******************************/

:- multifile prolog:message//1.

prolog:message(wordnet(nodb)) -->
    [ 'Cannot find WordNet data files.  Please set the environment'-[], nl,
      'variable WNDB to point at the directory holding the WordNet files'-[]
    ].
