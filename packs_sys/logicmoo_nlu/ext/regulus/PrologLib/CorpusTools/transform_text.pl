
:- ensure_loaded('$REGULUS/PrologLib/compatibility').

%---------------------------------------------------------------

:- module(transform_text,
	  [init_transform_text/0,
	   get_transform/2,
	   transform_sent/3,
	   transform_sent/4,
	   apply_transform/3,
	   
	   transform_text_in_file/3,
	   transform_text_in_file/4,
	   
	   test_transform_text/1
	   ]
      ).

%---------------------------------------------------------------

:- use_module(tokenize_sents).
:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).

%---------------------------------------------------------------

get_transform(TransformId, Transform) :-
	transform(TransformId, Transform),
	!.
get_transform(TransformId, _Transform) :-
	format('~N*** Error: unknown transform: ~w~n', [TransformId]),
	fail.

transform('2pl_to_2sing_minimal',
	  [vous/tu,
	   (From:transform_2p_verb_indicative(From, To))/To]).

transform('vous_te',
	  [W:not_prep(W),
	   vous/te,
	   Verb:non_2p_verb(Verb),
	   or([votre/ton, MascSingNoun:noun(MascSingNoun, masculine, singular)],
	      [votre/ta, FemSingNoun:noun(FemSingNoun, feminine, singular)],
	      [vos/tes, PlurNoun:noun(PlurNoun, _Gender, plural)],
	      []
	     )
	   ]
	  ).

transform('2pl_to_2sing',
	  or([or(il, 'Il'),                                             %il
	      opt(or(ne, 'n\'')),                                       %optional ne
	      est,                                                      %est
	      constrained_sequence:(X^not_verb_ce_etc(X)),              %sequence of non-verb words
	      W:non_verb_taking_subjunctive(W),                         %word like 'important' signalling subjunctive after following 'que'
	      constrained_sequence:(X^not_verb_ce_etc(X)),              %sequence of non-verb words
	      que,                                                      %que
	      constrained_sequence:(X^not_verb_ce_etc(X)),              %sequence of non-verb words
	      or('Vous', vous)/tu,                                      %vous (transformed)
	      opt(or(ne, 'n\'')),                                       %optional ne
	      opt(or(vous/te, (C:clitic_pronoun(C))/C)),                %optional clitic (reflexive transformed)
	      opt((C1:clitic_pronoun(C1))/C1),                          %optional 2nd clitic 
	      (From:transform_2p_verb_subjunctive(From, To))/To,        %2pl verb (taken as subjunctive if possible + transformed)
	      or(V:verb(V), []),                                        %optional verb
	      or([vous, '-', même]/['toi-même', ' '], [])               %optional vous-même -> toi-même
	     ],

	     [W:verb_taking_subjunctive(W),                             %word like 'veut' signalling subjunctive after following 'que'
	      constrained_sequence:(X^not_verb_ce_etc(X)),              %sequence of non-verb words
	      que,                                                      %que
	      constrained_sequence:(X^not_verb_ce_etc(X)),              %sequence of non-verb words
	      or('Vous', vous)/tu,                                      %vous (transformed)
	      opt(or(ne, 'n\'')),                                       %optional ne
	      opt(or(vous/te, (C:clitic_pronoun(C))/C)),                %optional clitic (reflexive transformed)
	      opt((C1:clitic_pronoun(C1))/C1),                          %optional 2nd clitic 
	      (From:transform_2p_verb_subjunctive(From, To))/To,        %2pl verb (taken as subjunctive if possible + transformed)
	      or(V:verb(V), []),                                        %optional verb
	      or([vous, '-', même]/['toi-même', ' '], [])               %optional vous-même -> toi-même
	     ],
	     
	     [or('Vous', vous)/tu,                                      %vous (transformed)
	      opt(or(ne, 'n\'')),                                       %optional ne
	      opt(or(vous/te, (C:clitic_pronoun(C))/C)),                %optional clitic (reflexive transformed)
	      opt((C1:clitic_pronoun(C1))/C1),                          %optional 2nd clitic 
	      (From:transform_2p_verb_indicative(From, To))/To,         %2pl verb (taken as indicative if possible + transformed)
	      or(V:verb(V), []),                                        %optional verb
	      or([vous, '-', même]/['toi-même', ' '], [])               %optional vous-même -> toi-même
	     ]
	    )
	 ).

transform('2sing_to_2pl',
	  or([or(il, 'Il'),                                             %il
	      opt(or(ne, 'n\'')),                                       %optional ne
	      est,                                                      %est
	      constrained_sequence:(X^not_verb_ce_etc(X)),              %sequence of non-verb words
	      W:non_verb_taking_subjunctive(W),                         %word like 'important' signalling subjunctive after following 'que'
	      constrained_sequence:(X^not_verb_ce_etc(X)),              %sequence of non-verb words
	      que,                                                      %que
	      constrained_sequence:(X^not_verb_ce_etc(X)),              %sequence of non-verb words
	      or('Tu', tu)/vous,                                        %tu (transformed)
	      opt(or(ne, 'n\'')),                                       %optional ne
	      opt(or(te/vous, 't\''/[vous, ' '], (C:clitic_pronoun(C))/C)),    %optional clitic (reflexive transformed)
	      opt((C1:clitic_pronoun(C1))/C1),                          %optional 2nd clitic 
	      (From:transform_2s_verb_subjunctive(From, To))/To,        %2s verb (taken as subjunctive if possible + transformed)
	      or(V:verb(V), []),                                        %optional verb
	      or([toi, '-', même]/['vous-même', ' '], [])               %optional toi-même -> vous-même
	     ],

	     [W:verb_taking_subjunctive(W),                             %word like 'veut' signalling subjunctive after following 'que'
	      constrained_sequence:(X^not_verb_ce_etc(X)),              %sequence of non-verb words
	      que,                                                      %que
	      constrained_sequence:(X^not_verb_ce_etc(X)),              %sequence of non-verb words
	      or('Tu', tu)/vous,                                        %tu (transformed)
	      opt(or(ne, 'n\'')),                                       %optional ne
	      opt(or(te/vous, 't\''/[vous, ' '], (C:clitic_pronoun(C))/C)),    %optional clitic (reflexive transformed)
	      opt((C1:clitic_pronoun(C1))/C1),                          %optional 2nd clitic 
	      (From:transform_2s_verb_subjunctive(From, To))/To,        %2pl verb (taken as subjunctive if possible + transformed)
	      or(V:verb(V), []),                                        %optional verb
	      or([toi, '-', même]/['vous-même', ' '], [])               %optional toi-même -> vous-même
	     ],
	     
	     [or('Tu'/'Vous', tu/vous),                                 %tu (transformed)
	      opt(or(ne, 'n\'')),                                       %optional ne
	      opt(or(te/vous, 't\''/[vous, ' '], (C:clitic_pronoun(C))/C)),    %optional clitic (reflexive transformed)
	      opt((C1:clitic_pronoun(C1))/C1),                          %optional 2nd clitic 
	      (From:transform_2s_verb_indicative(From, To))/To,         %2pl verb (taken as indicative if possible + transformed)
	      or(V:verb(V), []),                                        %optional verb
	      or([toi, '-', même]/['vous-même', ' '], [])               %optional toi-même -> vous-même
	     ],

	     [(From:transform_2s_verb_indicative(From, To))/To,         %2pl verb (taken as indicative if possible + transformed),
	      opt('-'),                                                 %hyphen (optional, since people are sloppy)
	      (tu/vous)                                                 %tu (transformed)
	     ],

	     [(or(te, 't\'')/[vous, ' '])                               %te/t' (transformed)
	     ]
	    )
	 ).

transform('2sing_to_2pl_minimal',
	  or('Tu'/'Vous',
	     tu/vous,
	     te/vous,
	     't\''/[vous, ' '],
	     toi/vous,
	     ton/votre,
	     ta/votre,
	     tes/vos,
	     (From:transform_2s_verb_indicative(From, To))/To
	    )
	 ).

transform('inverted_to_est_ce_que_minimal',
	  [Verb:indicative_verb(Verb), '-', vous] / ['est-ce que', ' ', vous, ' ', Verb, ' ']
	 ).

transform('inverted_to_est_ce_que_1',
	  [Verb:indicative_verb(Verb), '-', Pron:subj_pron(Pron)] / ['est-ce que', ' ', Pron, ' ', Verb, ' ']
	 ).


transform('inverted_to_est_ce_que',
	  or(([Clitic:clitic_pronoun(Clitic), Verb:indicative_verb(Verb), '-', Pron:subj_pron(Pron)] / ['est-ce que', ' ', Pron, ' ', l(Clitic), ' ', Verb, ' ']),
	     ([or(y, 'Y'), a, '-', t, '-', il] / ['est-ce qu\'il y a ']),
	     ([a, '-', t, '-', Pron:subj_pron(Pron)] / ['est-ce que', ' ', Pron, ' ', a, ' ']),
	     ([or(ne, 'n\'', 'Ne', 'N\''), Verb:indicative_verb(Verb), '-', Pron:subj_pron(Pron)] / ['est-ce que', ' ', Pron, ' ', ne, ' ', Verb, ' ']),
	     ([Before:not_mid_np_word(Before), Art:article(Art), W1, Verb:indicative_verb(Verb), '-', Pron:subj_pron_3p(Pron)] / [Before, ' ', 'est-ce que', ' ', l(Art), ' ', W1, ' ', Verb, ' ']),
	     ([Before:not_mid_np_word(Before), Art:article(Art), W1, W2, Verb:indicative_verb(Verb), '-', Pron:subj_pron_3p(Pron)] / [Before, ' ', 'est-ce que', ' ', l(Art), ' ', W1, ' ', W2, ' ', Verb, ' ']),
	     ([Verb:indicative_verb(Verb), '-', Pron:subj_pron(Pron)] / ['est-ce que', ' ', Pron, ' ', l(Verb), ' ']),
	     ([Verb:indicative_verb(Verb), '-', Pron:subj_pron(Pron)] / ['est-ce que', ' ', Pron, ' ', l(Verb), ' '])
	    )
	 ).

transform('add_hyphen_to_est_ce_que',
	  or((['c\'', est, ce, que] / ['c\'est ce que', ' ']),
	     (['C\'', est, ce, que] / ['C\'est ce que', ' ']),	
	     ([est, ce, que] / ['est-ce que', ' ']),
	     (['Est', ce, que] / ['Est-ce que', ' ']),
	     ([est, ce, qu] / ['est-ce qu']),
	     (['Est', ce, qu] / ['Est-ce qu'])
	    )
	 ).

transform('verb_plus_verb_pas',
	  [(Verb:verb(Verb)),
	   plus/pas,
	   (W:not_adjective_adverb_or_particle(W))]
	 ).

%---------------------------------------------------------------

test_transform_text(0) :-
	%Sent = 'Je suis désolée, Monsieur Hänsch et Monsieur Cox, je n\'avais pas vu que vous demandiez la parole.',
	Sent = 'vous demandiez la parole.',
	TransformId = '2pl_to_2sing_minimal',
	test_transform_text_example(Sent, TransformId).

test_transform_text(1) :-
	%Sent = 'Je suis désolée, Monsieur Hänsch et Monsieur Cox, je n\'avais pas vu que vous demandiez la parole.',
	%Sent = 'Monsieur le Président, intervenir en dernier lieu me donne le privilège, Monsieur le Commissaire, de vous dire que cette Assemblée est majoritairement favorable à votre initiative et a montré qu\'elle avait une totale confiance ­ tout à fait justifiée, selon moi ­ dans la manière dont vous gouvernez cette bateau.',
	Sent = 'vous gouvernez.',
	%Sent = 'Vous demandiez la parole.',
	TransformId = '2pl_to_2sing',
	test_transform_text_example(Sent, TransformId).

test_transform_text(2) :-
	Sent = 'Je suis désolée, Monsieur Hänsch et Monsieur Cox, je n\'avais pas vu que vous demandiez la parole.',
	%Sent = 'Vous m\'avez demandé la parole.',
	TransformId = '2pl_to_2sing',
	test_transform_text_example(Sent, TransformId).

test_transform_text('2a') :-
	%Sent = 'Je suis désolée, Monsieur Hänsch, je n\'avais pas vu que tu demandais la parole.',
	Sent = 'tu demandais la parole.',
	TransformId = '2sing_to_2pl',
	test_transform_text_example(Sent, TransformId).

test_transform_text('2a1') :-
	%Sent = 'Je suis désolée, Monsieur Hänsch, je n\'avais pas vu que tu demandais la parole.',
	Sent = 'tu demandais la parole.',
	TransformId = '2sing_to_2pl',
	test_transform_text_example1(Sent, TransformId).

test_transform_text('2b') :-
	Sent = 'Tu m\'as demandé la parole.',
	TransformId = '2sing_to_2pl',
	test_transform_text_example(Sent, TransformId).

test_transform_text(3) :-
	Sent = 'avez-vous demandé la parole.',
	TransformId = 'inverted_to_est_ce_que_minimal',
	test_transform_text_example(Sent, TransformId).

test_transform_text(4) :-
	Sent = 'avons-nous demandé la parole.',
	TransformId = 'inverted_to_est_ce_que',
	test_transform_text_example(Sent, TransformId).

test_transform_text(5) :-
	Sent = 'Ces grands entreprises avaient-elles besoin de l\'aide des États pour survivre ?',
	TransformId = 'inverted_to_est_ce_que',
	test_transform_text_example(Sent, TransformId).

test_transform_text(6) :-
	Sent = 'Il faut que vous l\'accueilliez',
	TransformId = '2pl_to_2sing',
	test_transform_text_example(Sent, TransformId).

test_transform_text(7) :-
	Sent = 'Il est bon que vous l\'accueilliez',
	TransformId = '2pl_to_2sing',
	test_transform_text_example(Sent, TransformId).

test_transform_text(8) :-
	Sent = 'Il n\'est pas bon que vous l\'accueilliez',
	TransformId = '2pl_to_2sing',
	test_transform_text_example(Sent, TransformId).

test_transform_text(9) :-
	Sent = 'vous vous en souviendrez',
	TransformId = '2pl_to_2sing',
	test_transform_text_example(Sent, TransformId).

test_transform_text(10) :-
	Sent = 'vous avez dit vous-même',
	TransformId = '2pl_to_2sing',
	test_transform_text_example(Sent, TransformId).

test_transform_text(11) :-
	Sent = 'Je vous ai dit',
	TransformId = 'vous_te',
	test_transform_text_example(Sent, TransformId).

test_transform_text(12) :-
	Sent = 'vous avez dit',
	TransformId = 'vous_te',
	test_transform_text_example(Sent, TransformId).

test_transform_text(13) :-
	Sent = 'Plusieurs d\'entre vous ont mentionné des points',
	TransformId = 'vous_te',
	test_transform_text_example(Sent, TransformId).

test_transform_text(14) :-
	Sent = 'Je vous demande votre approbation',
	TransformId = 'vous_te',
	test_transform_text_example(Sent, TransformId).

test_transform_text(15) :-
	Sent = 'Est ce que cette offre se valide uniquement lors de l\'installation ?',
	TransformId = 'add_hyphen_to_est_ce_que',
	test_transform_text_example(Sent, TransformId).

test_transform_text(16) :-
	Sent = 'Qu\'est ce que je dois faire ?',
	TransformId = 'add_hyphen_to_est_ce_que',
	test_transform_text_example(Sent, TransformId).

test_transform_text(17) :-
	Sent = 'C\'est ce que j\'allais quand même vérifier car je me rappelle plus.',
	TransformId = 'add_hyphen_to_est_ce_que',
	test_transform_text_example(Sent, TransformId).

test_transform_text(18) :-
	Sent = 'Si tu n\'as plus l\'installeur de NIS2009 , il te faudra certainement contacter le support Norton et leur faire une demande de mise à jour pour la version 2010.',
	TransformId = 'verb_plus_verb_pas',
	test_transform_text_example(Sent, TransformId).

test_transform_text(ez_small) :-
	%TransformId = '2pl_to_2sing_minimal',
	TransformId = '2pl_to_2sing',

	init_transform_text,
	transform_text_in_file('$ACCEPT/MT/Europarl/Generated/europarl_ez_filtered_small.txt',
			       TransformId,
			       '$ACCEPT/MT/Europarl/Generated/europarl_ez_transformed_small.txt').

test_transform_text(ez_full) :-
	%TransformId = '2pl_to_2sing_minimal',
	TransformId = '2pl_to_2sing',

	init_transform_text,
	transform_text_in_file('$ACCEPT/MT/Europarl/Generated/europarl_ez_filtered_fr.txt',
			       TransformId,
			       '$ACCEPT/MT/Europarl/Generated/europarl_ez_transformed_fr.txt').

test_transform_text(questions_small) :-
	TransformId = 'inverted_to_est_ce_que',

	init_transform_text,
	transform_text_in_file('$ACCEPT/MT/Europarl/Generated/europarl_questions_filtered_small_fr.txt',
			       TransformId,
			       '$ACCEPT/MT/Europarl/Generated/europarl_questions_transformed_small_fr.txt').

test_transform_text(questions_full) :-
	TransformId = 'inverted_to_est_ce_que',

	init_transform_text,
	transform_text_in_file('$ACCEPT/MT/Europarl/Generated/europarl_questions_filtered_fr.txt',
			       TransformId,
			       '$ACCEPT/MT/Europarl/Generated/europarl_questions_transformed_fr.txt').

test_transform_text(vous_te_full) :-
	TransformId = 'vous_te',

	init_transform_text,
	transform_text_in_file('$ACCEPT/MT/Europarl/Generated/europarl_vous_fr.txt',
			       TransformId,
			       '$ACCEPT/MT/Europarl/Generated/europarl_vous_transformed_fr.txt').

test_transform_text(fix_est_ce_que) :-
	TransformId = 'add_hyphen_to_est_ce_que',

	%init_transform_text,
	transform_text_in_file('$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/est_ce_que_200.txt',
			       TransformId,
			       keep,
			       '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/est_ce_que_200_cleaned.txt').

test_transform_text(tu_200_to_pl) :-
	TransformId = '2sing_to_2pl',

	init_transform_text,
	transform_text_in_file('$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/tu_200.txt',
			       TransformId,
			       keep,
			       '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/tu_200_vous_version.txt').

test_transform_text(tu_200_v2_to_pl) :-
	TransformId = '2sing_to_2pl',

	init_transform_text,
	transform_text_in_file('$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/tu_200_v2.txt',
			       TransformId,
			       keep,
			       '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/tu_200_vous_version_v2.txt').

test_transform_text(tu_200_v2_to_pl_minimal) :-
	TransformId = '2sing_to_2pl_minimal',

	init_transform_text,
	transform_text_in_file('$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/tu_200_v2.txt',
			       TransformId,
			       keep,
			       '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/tu_200_minimal_vous_version_v2.txt').

test_transform_text(devtest_b_to_pl) :-
	TransformId = '2sing_to_2pl',

	init_transform_text,
	transform_text_in_file('$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/devtest_b.fr',
			       TransformId,
			       keep,
			       '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/devtest_b_vous.fr').
			       
%---------------------------------------------------------------

test_transform_text_example(Sent, TransformId) :-
	init_transform_text,
	test_transform_text_example1(Sent, TransformId).

test_transform_text_example1(Sent, TransformId) :-
	format('~NIn   : ~w~n', [Sent]),

	atom_codes(Sent, SentStr),
	
	tokenize_sent(SentStr, TokenizedSent),
	format('~NTok  : ~w~n', [TokenizedSent]),

	transform_sent(TokenizedSent, TransformId, TokenizedSent1, TrivialP),
	format('~NTok1 : ~w~n', [TokenizedSent1]),
		       
	tokenized_sent_to_atom(TokenizedSent1, Sent1),
	format('~NTrans: ~w~n', [Sent1]),
	format('~N~w~n', [TrivialP]),
	!.

%---------------------------------------------------------------

init_transform_text :-
	format('~N--- Loading prepositions~n', []),
	compile('$ACCEPT/MT/Europarl/Mmorph/prepositions.pl'),
	load_multiwords('$ACCEPT/MT/Europarl/Mmorph/multiword_prepositions.pl'),

	format('~N--- Loading adverbs~n', []),
	compile('$ACCEPT/MT/Europarl/Mmorph/advs.pl'),

	format('~N--- Loading adjectives~n', []),
	compile('$ACCEPT/MT/Europarl/Mmorph/adjs.pl'),
	
	format('~N--- Loading nouns~n', []),
	compile('$ACCEPT/MT/Europarl/Mmorph/nouns.pl'),
	
	format('~N--- Loading verbs~n', []),
	compile('$ACCEPT/MT/Europarl/Mmorph/verbs_with_mood.pl'),

	format('~N--- Loading words marking subjunctive~n', []),
	compile('$ACCEPT/MT/Europarl/Mmorph/verbs_taking_subjunctive.pl'),
	
	format('~N--- Loading verb transforms~n', []),
	compile('$ACCEPT/MT/Europarl/Mmorph/verb_transforms.pl'),
	compile('$ACCEPT/MT/Europarl/Mmorph/verb_transforms_2sing_to_2pl.pl').	

%---------------------------------------------------------------

transform_text_in_file(InFile, TransformId, OutFile) :-
	transform_text_in_file(InFile, TransformId, discard, OutFile).

transform_text_in_file(InFile, TransformId, KeepOrDiscard, OutFile) :-
	(   member(KeepOrDiscard, [keep, discard]) ->
	    true
	;
	    otherwise ->
	    format('~N*** Error: third arg ~w to transform_text_in_file/4 must be "keep" or "discard"~n',
		   [KeepOrDiscard]),
	    fail
	),
	absolute_file_name(InFile, AbsInFile),
	absolute_file_name(OutFile, AbsOutFile),
	
	read_unicode_file_to_atom_list(AbsInFile, InList),
	length(InList, NIn),
	format('~N--- Read file (~d lines) ~w~n', [NIn, AbsInFile]),
	
	transform_sents_in_list(InList, TransformId, KeepOrDiscard, OutList, 0, 0-NOut),

	write_out_transformed_list_to_file(OutList, AbsOutFile, NIn, NOut),
	!.


write_out_transformed_list_to_file(List, File, NIn, NOut) :-
	open(File, write, S, [encoding('UTF-8'), encoding_signature(true)]),
	write_out_transformed_list_to_stream(List, S),
	close(S),
	PC is (100.0 * NOut) / NIn,
	format('~N--- Written file (~d lines = ~1f%) ~w~n', [NOut, PC, File]),
	!.

write_out_transformed_list_to_stream([], _S).
write_out_transformed_list_to_stream([F | R], S) :-
	format(S, '~N~w~n', [F]),
	!,
	write_out_transformed_list_to_stream(R, S).

transform_sents_in_list([], _Transform, _KeepOrDiscard, [], _I, CIn-CIn).
transform_sents_in_list([F | R], Transform, KeepOrDiscard, Out, I, CIn-COut) :-
	tokenize_transform_and_untokenize_sent(F, Transform, F1, TrivialP),
	(   TrivialP = non_trivial ->
	    Out = [F1 | R1],
	    CNext is CIn + 1,
	    format('+', [])
	;
	    otherwise ->
	    (   KeepOrDiscard = discard ->
		Out = ['*** NO TRANSFORM - DISCARD ***' | R1]
	    ;
		otherwise ->
		Out = [F | R1]
	    ),
	    CNext = CIn,
	    format('-', [])
	),
	I1 is I + 1,
	(   0 is I1 mod 100 ->
	    format(' (~d)~n', [I1])
	;
	    otherwise ->
	    true
	),
	flush_output(user),
	!,
	transform_sents_in_list(R, Transform, KeepOrDiscard, R1, I1, CNext-COut).

%---------------------------------------------------------------

tokenize_transform_and_untokenize_sent(In, TransformId, Out, TrivialP) :-
	tokenize_sent(In, TokenizedIn),
	transform_sent(TokenizedIn, TransformId, TokenizedOut, TrivialP),
	tokenized_sent_to_atom(TokenizedOut, Out),
	!.

%---------------------------------------------------------------

transform_sent(In, TransformId, Out) :-
	transform_sent(In, TransformId, Out, _TrivialP),
	!.

transform_sent(In, TransformId, Out, TrivialP) :-
	transform_sent1(In, TransformId, Out, trivial-TrivialP),
	!.
transform_sent(In, _TransformId, In, trivial).

transform_sent1([], _TransformId, [], TrivIn-TrivOut) :-
	equalize_trivs(TrivIn, TrivOut).
transform_sent1(In, TransformId, Out, _TrivIn-TrivOut) :-
	get_transform(TransformId, Transform),
	apply_transform(Transform, In-InNext, Out-OutNext),
	!,
	transform_sent1(InNext, TransformId, OutNext, non_trivial-TrivOut).
transform_sent1([F | InNext], TransformId, [F | OutNext], TrivIn-TrivOut) :-
	!,
	transform_sent1(InNext, TransformId, OutNext, TrivIn-TrivOut).

equalize_trivs(TrivIn, TrivOut) :-
	TrivIn = TrivOut.

% Skip spaces
apply_transform(Transform, [' ' | InNext]-InRest, [' ' | OutNext]-OutRest) :-
	!,
	apply_transform(Transform, InNext-InRest, OutNext-OutRest).

% Null
apply_transform([], In-In, Out-Out).

% Sequence
apply_transform([F | R], In-InRest, Out-OutRest) :-
	apply_transform(F, In-InNext, Out-OutNext),
	apply_transform(R, InNext-InRest, OutNext-OutRest).

% Disjunction
apply_transform(Disjunction, In-InRest, Out-OutRest) :-
	compound(Disjunction),
	functor(Disjunction, or, _N),
	Disjunction =.. [or | Disjuncts],
	apply_transform_disjunctive(Disjuncts, In-InRest, Out-OutRest).

% Optional
apply_transform(opt(Body), In-InRest, Out-OutRest) :-
	apply_transform(Body, In-InRest, Out-OutRest),
	!.
apply_transform(opt(_Body), In-In, Out-Out).

% Substitution
apply_transform(From/To, In-InRest, Out-OutRest) :-
	match_pattern(From, In-InRest),
	add_output(To, OutRest, Out),
	!.

% Verb transform
%apply_transform(transform_2pl_verb, [PlurVerb | InRest]-InRest, [SingVerb | OutRest]-OutRest) :-
%	transform_2p_verb(PlurVerb, SingVerb),
%	!.

% Plain match
apply_transform(Atom, In-InRest, Out-OutRest) :-
	atomic(Atom),
	!,
	apply_transform(Atom/Atom, In-InRest, Out-OutRest).
apply_transform(Var:Call, In-InRest, Out-OutRest) :-
	( var(Var) ; atomic(Var) ),
	!,
	apply_transform((Var:Call)/Var, In-InRest, Out-OutRest).

% Try null first, because we want the sequence to be as short as possible.
apply_transform(constrained_sequence:(_X^_Call), In-In, Out-Out).
apply_transform(constrained_sequence:(X^Call), [X1 | InNext]-InRest, [X1 | OutNext]-OutRest) :-
	copy_term(X^Call, X1^Call1),
	call(Call1),
	!,
	apply_transform(constrained_sequence:(X^Call), InNext-InRest, OutNext-OutRest).

apply_transform_disjunctive([F | _R], In-InRest, Out-OutRest) :-
	apply_transform(F, In-InRest, Out-OutRest),
	!.
apply_transform_disjunctive([_F | R], In-InRest, Out-OutRest) :-
	apply_transform_disjunctive(R, In-InRest, Out-OutRest),
	!.

%---------------------------------------------------------------

% Skip spaces
match_pattern(Pattern, [' ' | Next]-Out) :-
	!,
	match_pattern(Pattern, Next-Out).
match_pattern(Word, [Word | Out]-Out) :-
	!.
match_pattern(Word:Call, [Word | Out]-Out) :-
	call(Call),
	!.
match_pattern([F | R], In-Out) :-
	match_pattern(F, In-Next),
	match_pattern(R, Next-Out),
	!.
match_pattern([], In-In) :-
	!.
match_pattern(Disjunction, In-Out) :-
	compound(Disjunction),
	functor(Disjunction, or, _N),
	Disjunction =.. [or | Disjuncts],
	match_pattern_disjunctive(Disjuncts, In-Out),
	!.

match_pattern_disjunctive([F | R], In-Out) :-
	(   match_pattern(F, In-Out) ->
	    true
	;
	    match_pattern_disjunctive(R, In-Out)
	).

add_output(To, OutRest, Out) :-
	atomic(To),
	Out = [To | OutRest],
	!.
add_output(List, OutRest, Out) :-
	is_list(List),
	append(List, OutRest, Out),
	!.
add_output(Other, OutRest, Out) :-
	format('~N*** Error: bad call: ~w~n', [add_output(Other, OutRest, Out)]),
	fail.

%---------------------------------------------------------------

% verb_2_pl_to_verb_2_sing(accueilliez, accueilles, subjunctive, present).
% verb_2_pl_to_verb_2_sing(accueilliez, accueillais, indicative, imperfect).

% Interpret verb as indicative if possible.
transform_2p_verb_indicative(FirstVerb0, SecondVerb) :-
	lowercase_atom(FirstVerb0, FirstVerb),
	verb_2_pl_to_verb_2_sing(FirstVerb, SecondVerb, Mood, _Tense),
	indicative_or_similar(Mood),
	!.
transform_2p_verb_indicative(FirstVerb0, SecondVerb) :-
	lowercase_atom(FirstVerb0, FirstVerb),
	verb_2_pl_to_verb_2_sing(FirstVerb, SecondVerb, _Mood, _Tense),
	!.

% Interpret verb as subjunctive if possible.
transform_2p_verb_subjunctive(FirstVerb0, SecondVerb) :-
	lowercase_atom(FirstVerb0, FirstVerb),
	verb_2_pl_to_verb_2_sing(FirstVerb, SecondVerb, subjunctive, _Tense),
	!.
transform_2p_verb_subjunctive(FirstVerb0, SecondVerb) :-
	lowercase_atom(FirstVerb0, FirstVerb),
	verb_2_pl_to_verb_2_sing(FirstVerb, SecondVerb, _Mood, _Tense),
	!.

% Interpret verb as indicative if possible.
transform_2s_verb_indicative(FirstVerb0, SecondVerb) :-
	lowercase_atom(FirstVerb0, FirstVerb),
	verb_2_sing_to_verb_2_pl(FirstVerb, SecondVerb, Mood, _Tense),
	indicative_or_similar(Mood),
	!.
transform_2s_verb_indicative(FirstVerb0, SecondVerb) :-
	lowercase_atom(FirstVerb0, FirstVerb),
	verb_2_sing_to_verb_2_pl(FirstVerb, SecondVerb, _Mood, _Tense),
	!.

% Interpret verb as subjunctive if possible.
transform_2s_verb_subjunctive(FirstVerb0, SecondVerb) :-
	lowercase_atom(FirstVerb0, FirstVerb),
	verb_2_sing_to_verb_2_pl(FirstVerb, SecondVerb, subjunctive, _Tense),
	!.
transform_2s_verb_subjunctive(FirstVerb0, SecondVerb) :-
	lowercase_atom(FirstVerb0, FirstVerb),
	verb_2_sing_to_verb_2_pl(FirstVerb, SecondVerb, _Mood, _Tense),
	!.

indicative_or_similar(indicative).
indicative_or_similar(conditional).

% Words that signal a subjunctive after a following 'que'
word_signalling_subjunctive(W) :-
	verb_taking_subjunctive(W),
	!.
word_signalling_subjunctive(W) :-
	non_verb_taking_subjunctive(W),
	!.

not_verb_ce_etc(W) :-
	\+ member(W, [ce, que]),
	\+ verb(W, _Mood, _Person, _Number).

not_verb(W) :-
	\+ verb(W, _Mood, _Person, _Number).

not_indicative_verb(W) :-
	\+ indicative_verb(W).

indicative_verb(W) :-
	verb(W, indicative, _Person, _Number).

verb(W) :-
	verb(W, _Mood, _Person, _Number).

past_participle(W) :-
	verb(W, participle, _Person, Number),
	member(Number, [singular, plural]).

non_2p_verb(W) :-
	verb(W, Mood, Person, Number),
	non_2p(Mood, Person, Number).

non_2p(Mood, Person, Number) :-
	member(Mood, [indicative, subjunctive, conditional]),
	(   Person \== '2' ->
	    true
	;
	    Number \== plural ->
	    true
	),
	!.

non_2s_verb(W) :-
	verb(W, Mood, Person, Number),
	non_2p(Mood, Person, Number).

non_2s(Mood, Person, Number) :-
	member(Mood, [indicative, subjunctive, conditional]),
	(   Person \== '2' ->
	    true
	;
	    Number \== singular ->
	    true
	),
	!.

not_prep(Word) :-
	\+ prep(Word).

not_adjective_adverb_or_particle(Word) :-
	\+ adj(Word, _Gender, _Number),
	\+ adv(Word),
	\+ particle(Word).

%---------------------------------------------------------------

subj_pron(vous).
subj_pron(nous).
subj_pron(il).
subj_pron(elle).
subj_pron(ils).
subj_pron(elles).

subj_pron_3p(il).
subj_pron_3p(elle).
subj_pron_3p(ils).
subj_pron_3p(elles).

clitic_pronoun('en').
clitic_pronoun('l\'').
clitic_pronoun('la').
clitic_pronoun('le').
clitic_pronoun('les').
clitic_pronoun('m\'').
clitic_pronoun('me').
clitic_pronoun('nous').
clitic_pronoun('s\'').
clitic_pronoun('se').
clitic_pronoun('t\'').
clitic_pronoun('te').
clitic_pronoun('vous').
clitic_pronoun('y').

clitic_pronoun('En').
clitic_pronoun('L\'').
clitic_pronoun('La').
clitic_pronoun('Le').
clitic_pronoun('Les').
clitic_pronoun('M\'').
clitic_pronoun('Me').
clitic_pronoun('Nous').
clitic_pronoun('S\'').
clitic_pronoun('Se').
clitic_pronoun('T\'').
clitic_pronoun('Te').
clitic_pronoun('Vous').
clitic_pronoun('Y').

article(le).
article(la).
article(les).
article('l\'').
article(ce).
article(ces).
article(cet).
article(cette).

article('Le').
article('La').
article('Les').
article('L\'').
article('Ce').
article('Ces').
article('Cet').
article('Cette').

not_mid_np_word(Word) :-
	\+ mid_np_word(Word).

mid_np_word(et).
mid_np_word(de).
mid_np_word(à).

particle(de).
particle(d).
particle('d\'').

