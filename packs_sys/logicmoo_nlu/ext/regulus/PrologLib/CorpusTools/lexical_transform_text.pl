
:- ensure_loaded('$REGULUS/PrologLib/compatibility').

%---------------------------------------------------------------

:- module(lexical_transform_text,
	  [lexical_transform_text_in_file/3,
	   lexical_transform_text_in_file/4,

	   test_lexical_transform/1,
	   test_lexical_transform_text/1
	   ]
      ).

%---------------------------------------------------------------

:- use_module('$REGULUS/PrologLib/CorpusTools/orthography_process_text').
:- use_module('$REGULUS/PrologLib/CorpusTools/transform_text').
:- use_module('$REGULUS/PrologLib/CorpusTools/tokenize_sents').

:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).

%---------------------------------------------------------------

test_lexical_transform_text(all_clitic_bigrams) :-
	init_transform_text,
	init_orthography_process_text('$REGULUS/PrologLib/CorpusTools/french_orthography.pl'),
				      
	test_lexical_transform_text(clitic_bigram_tu_vous),
	test_lexical_transform_text(clitic_bigram_remove),
	test_lexical_transform_text(clitic_bigram_ça),
	test_lexical_transform_text(clitic_bigram_ceci),
	test_lexical_transform_text(clitic_bigram_direct_object),
	test_lexical_transform_text(clitic_bigram_indirect_object).

test_lexical_transform_text(all_plus_pas) :-
	init_transform_text,
	init_orthography_process_text('$REGULUS/PrologLib/CorpusTools/french_orthography.pl'),

	test_lexical_transform_text(plus_pas).

test_lexical_transform_text(all_plus_pas2) :-
	init_transform_text,
	init_orthography_process_text('$REGULUS/PrologLib/CorpusTools/french_orthography.pl'),

	test_lexical_transform_text(plus_pas2).

test_lexical_transform_text(clitic_bigram_tu_vous) :-
	lexical_transform_text_in_file('$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/bigrams_clitics_dev.pl',
				       ['2sing_to_2pl'],
				       trivial,
				       '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/bigrams_clitics_dev_tu_vous.pl').
test_lexical_transform_text(clitic_bigram_remove) :-
	lexical_transform_text_in_file('$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/bigrams_clitics_dev.pl',
				       ['2sing_to_2pl'],
				       remove,
				       '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/bigrams_clitics_dev_remove.pl').
test_lexical_transform_text(clitic_bigram_ça) :-
	lexical_transform_text_in_file('$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/bigrams_clitics_dev.pl',
				       ['2sing_to_2pl'],
				       ça,
				       '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/bigrams_clitics_dev_ça.pl').
test_lexical_transform_text(clitic_bigram_ceci) :-
	lexical_transform_text_in_file('$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/bigrams_clitics_dev.pl',
				       ['2sing_to_2pl'],
				       ceci,
				       '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/bigrams_clitics_dev_ceci.pl').
test_lexical_transform_text(clitic_bigram_direct_object) :-
	lexical_transform_text_in_file('$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/bigrams_clitics_dev.pl',
				       ['2sing_to_2pl'],
				       direct_object,
				       '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/bigrams_clitics_dev_direct_object.pl').
test_lexical_transform_text(clitic_bigram_indirect_object) :-
	lexical_transform_text_in_file('$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/bigrams_clitics_dev.pl',
				       ['2sing_to_2pl'],
				       indirect_object,
				       '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/bigrams_clitics_dev_indirect_object.pl').

test_lexical_transform_text(plus_pas) :-
	lexical_transform_text_in_file('$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/bigrams_plus_or_quoi_dev.pl',
				       [],
				       verb_plus_to_verb_pas,
				       ['2sing_to_2pl'],
				       '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/bigrams_plus_or_quoi_dev_pas.pl').

test_lexical_transform_text(plus_pas2) :-
	lexical_transform_text_in_file('$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/bigrams_plus_or_quoi_dev2.pl',
				       [],
				       verb_plus_to_verb_pas,
				       ['2sing_to_2pl'],
				       '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/bigrams_plus_or_quoi_dev_pas2.pl').
				       
%---------------------------------------------------------------

test_lexical_transform(en_remove) :-
	%In = "Pour un ami, le 24/11 j\'en ai acheté un à 39,90 euros pour un ami.",
	In = "j\'en ai acheté un à 39,90 euros pour un ami.",
	Transform = remove,
	NGram = [en, ai],
	test_transform_text_example(In, Transform, NGram).
test_lexical_transform(en_ça) :-
	%In = "Pour un ami, le 24/11 j\'en ai acheté un à 39,90 euros pour un ami.",
	In = "j\'en ai acheté un à 39,90 euros pour un ami.",
	Transform = ça,
	NGram = [en, ai],
	test_transform_text_example(In, Transform, NGram).
test_lexical_transform(le_ça) :-
	%In = "Pour un ami, le 24/11 j\'en ai acheté un à 39,90 euros pour un ami.",
	In = "ce fichier est trop précis pour ne pas le retrouver s\'il existe.",
	Transform = ça,
	NGram = [le, retrouver],
	test_transform_text_example(In, Transform, NGram).

test_lexical_transform(en_ça_with_pretransform) :-
	In = "Si oui, tu t'en sers encore ?",
	Pretransforms = ['2sing_to_2pl'],
	Transform = ça,
	NGram = [en, sers],
	test_transform_text_example(In, Pretransforms, Transform, NGram).
			       
%---------------------------------------------------------------

test_transform_text_example(Sent, Transform, NGram) :-
	test_transform_text_example(Sent, [], Transform, NGram).

test_transform_text_example(Sent, PreTransforms, Transform, NGram) :-
	format('~NIn   : ~s~n', [Sent]),
	
	tokenize_sent(Sent, TokenizedSent),
	format('~NTok  : ~w~n', [TokenizedSent]),

	apply_transform_list(TokenizedSent, PreTransforms, TokenizedSentNext),
	format('~NPre  : ~w~n', [TokenizedSentNext]),

	lexically_transform_sent(TokenizedSentNext, Transform, NGram, TokenizedSentOut, TrivialP),
	format('~NLex  : ~w~n', [TokenizedSentOut]),
		       
	tokenized_sent_to_atom(TokenizedSentOut, Sent1),
	format('~NTrans: ~w~n', [Sent1]),
	format('~N~w~n', [TrivialP]),

	orthography_process_string(Sent1, Sent2),
	format('~NOrth: ~w~n', [Sent2]),
	!.

%---------------------------------------------------------------

lexical_transform_text_in_file(InFile, TransformId, OutFile) :-
	lexical_transform_text_in_file(InFile, [], TransformId, OutFile).

lexical_transform_text_in_file(InFile, PreTransforms, TransformId, OutFile) :-
	lexical_transform_text_in_file(InFile, PreTransforms, TransformId, [], OutFile).

lexical_transform_text_in_file(InFile, PreTransforms, TransformId, PostTransforms, OutFile) :-
	absolute_file_name(InFile, AbsInFile),
	absolute_file_name(OutFile, AbsOutFile),

	safe_prolog_file_to_list(AbsInFile, InList, 'UTF-8'),
	length(InList, NIn),
	format('~N~n--- Read file (~d lines) ~w~n', [NIn, AbsInFile]),
	
	transform_sents_in_list(InList, PreTransforms, TransformId, PostTransforms, OutList, 0, 0-NOut),

	write_out_transformed_list_to_file(OutList, AbsOutFile, NOut),
	!.


write_out_transformed_list_to_file(List, File, NOut) :-
	open(File, write, S, [encoding('UTF-8'), encoding_signature(true)]),
	write_out_transformed_list_to_stream(List, S),
	close(S),
	format('~N--- Written file (~d transformed lines) ~w~n', [NOut, File]),
	!.

write_out_transformed_list_to_stream([], _S).
write_out_transformed_list_to_stream([F | R], S) :-
	format(S, '~N~q.~n', [F]),
	!,
	write_out_transformed_list_to_stream(R, S).

transform_sents_in_list([], _PreTransforms, _Transform, _PostTransforms, [], _I, CIn-CIn).
transform_sents_in_list([F | R], PreTransforms, Transform, PostTransforms, [F1 | R1], I, CIn-COut) :-
	F = ngram_example(NGram, Multiplicity, Sent),
	tokenize_transform_and_untokenize_sent(Sent, PreTransforms, Transform, PostTransforms, NGram, Sent1, TrivialP),
	(   TrivialP = non_trivial ->
	    F1 = ngram_example(NGram, Multiplicity, Sent1),
	    CNext is CIn + 1,
	    format('+', [])
	;
	    otherwise ->
	    F1 = ngram_example(NGram, '*** NOT USED ***', Sent),
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
	transform_sents_in_list(R, PreTransforms, Transform, PostTransforms, R1, I1, CNext-COut).

%---------------------------------------------------------------

tokenize_transform_and_untokenize_sent(InAtom, PreTransforms, Transform, PostTransforms, NGram, Out, TrivialP) :-
	atom_codes(InAtom, InStr),
	tokenize_sent(InStr, TokenizedIn),
	apply_transform_list(TokenizedIn, PreTransforms, TokenizedNext),
	lexically_transform_sent(TokenizedNext, Transform, NGram, TokenizedTransformed, TrivialP),
	apply_transform_list(TokenizedTransformed, PostTransforms, TokenizedPostTransformed),
	tokenized_sent_to_atom(TokenizedPostTransformed, AtomicTransformed),
	orthography_process_string(AtomicTransformed, Out),
	!.
tokenize_transform_and_untokenize_sent(InAtom, PreTransforms, Transform, PostTransforms, NGram, Out, TrivialP) :-
	format('~N*** Error: bad call: ~w~n',
	       [tokenize_transform_and_untokenize_sent(InAtom, PreTransforms, Transform, PostTransforms, NGram, Out, TrivialP)]),
	fail.

apply_transform_list(In, [], In).
apply_transform_list(In, [Transform | Transforms], Out) :-
	transform_sent(In, Transform, Next),
	!,
	apply_transform_list(Next, Transforms, Out).


%---------------------------------------------------------------

% We want the 'trivial' transforms to be marked as 'non_trivial', since we want to keep them.
lexically_transform_sent(In, trivial, _NGram, Out, TrivialP) :-
	Out = In,
	TrivialP = non_trivial,
	!.
lexically_transform_sent(In, TransformType, NGram, Out, TrivialP) :-
	lexical_transform_for_ngram(TransformType, NGram, Transform),
	lexically_transform_sent1(In, Transform, Out, trivial-TrivialP),
	!.
lexically_transform_sent(In, _Transform, _NGram, In, trivial).

%---------------------------------------------------------------

% J'y arrive -> j'arrive
% J'en rêve -> je rêve
% Je me demande -> Je demande

lexical_transform_for_ngram(remove, [Clitic, Verb], [Clitic, Verb]/[Verb, ' ']) :-
	member(Clitic, [y, en, me]).

% J'y arrive -> j'arrive à ça
% J'en rêve -> je rêve de ça
% Je le/la veux -> je veux ça 
%lexical_transform_for_ngram(ça, [y, Verb], [y, Verb]/[Verb, ' ', à, ' ', ça, ' ']) :-
%	!.
%lexical_transform_for_ngram(ça, [en, Verb], [en, Verb]/[Verb, ' ', de, ' ', ça, ' ']) :-
%	!.
lexical_transform_for_ngram(ça, [LeLa, Verb],
			    or([LeLa, Verb, (Participle:(transform_text:past_participle(Participle)))]/[Verb, ' ', Participle, ça, ' '],
			       [LeLa, Verb]/[Verb, ' ', ça, ' ']
			      )
			   ) :-
	member(LeLa, [le, la, 'l\'']),
	!.

% J'y arrive -> j'arrive à ceci
% J'en rêve -> je rêve de ceci
% Je le/la veux -> je veux ceci
% Je les veux -> je veux ceux-ci
%lexical_transform_for_ngram(ceci, [y, Verb], [y, Verb]/[Verb, ' ', à, ' ', ceci, ' ']) :-
%	!.
%lexical_transform_for_ngram(ceci, [en, Verb], [en, Verb]/[Verb, ' ', de, ' ', ceci, ' ']) :-
%	!.
lexical_transform_for_ngram(ceci, [LeLa, Verb],
			    or([LeLa, Verb, (Participle:(transform_text:past_participle(Participle)))]/[Verb, ' ', Participle, ça, ' '],
			       [LeLa, Verb]/[Verb, ' ', ceci, ' ']
			      )
			   ) :-
	member(LeLa, [le, la, 'l\'']),
	!.
lexical_transform_for_ngram(ceci, [les, Verb], [les, Verb]/[Verb, ' ', 'ceux-ci', ' ']) :-
	!.

% Je les veux -> Je veux eux 
% Je vous veux -> Je veux vous
% Je me demande -> Je demande moi-même
% Il se demande -> Il demande soi-même
lexical_transform_for_ngram(direct_object, [les, Verb], [les, Verb]/[Verb, ' ', eux, ' ']) :-
	!.
lexical_transform_for_ngram(direct_object, [TeVous, Verb], or([te, Verb], [vous, Verb])/[Verb, ' ', vous, ' ']) :-
	member(TeVous, [te, vous]),
	!.
lexical_transform_for_ngram(direct_object, [me, Verb], [me, Verb]/[Verb, ' ', 'moi-même', ' ']) :-
	!.
lexical_transform_for_ngram(direct_object, [se, Verb], [se, Verb]/[Verb, ' ', 'soi-même', ' ']) :-
	!.

% Je vous veux -> Je veux à vous
% Je me demande -> Je demande à moi-même
% Il se demande -> Il demande à soi-même
lexical_transform_for_ngram(indirect_object, [TeVous, Verb], or([te, Verb], [vous, Verb])/[Verb, ' ', à, ' ', vous, ' ']) :-
	member(TeVous, [te, vous]),
	!.
lexical_transform_for_ngram(indirect_object, [me, Verb], [me, Verb]/[Verb, ' ', à, ' ', 'moi-même', ' ']) :-
	!.
lexical_transform_for_ngram(indirect_object, [se, Verb], [se, Verb]/[Verb, ' ', à, ' ', 'soi-même', ' ']) :-
	!.
lexical_transform_for_ngram(indirect_object, [lui, Verb], [lui, Verb]/[Verb, ' ', à, ' ', 'lui', ' ']) :-
	!.

% SO FAR NOT HANDLED

% J'y arrive -> J'arrive là 

% Je les veux -> Je veux eux 

% Je te/vous veux -> Je vous veux

% Block 'plus ou moins'
% Block 'plus en plus'
% plus aucun -> aucun
% plus rien -> rien
% otherwise 'V plus' -> 'V pas' unless followed by adjective etc.

lexical_transform_for_ngram(verb_plus_to_verb_pas, [Verb, plus],
			    or([Verb, plus, ou, moins],
			       [Verb, plus, en, plus],
			       [Verb, [plus, rien]/[rien, ' ']],
			       [Verb, [plus, aucun]/[aucun, ' ']],
			       [Verb, plus/pas, (W:not_adjective_adverb_or_particle(W))]
			      )
			    ) :-
	transform_text:verb(Verb),
	!.
			   
%---------------------------------------------------------------

% apply_transform/3 is defined in transform_text.pl

lexically_transform_sent1([], _Transform, [], TrivIn-TrivIn) :-
	!.
lexically_transform_sent1(In, Transform, Out, _TrivIn-non_trivial) :-
	apply_transform(Transform, In-InRest, Out-OutRest),
	InRest = OutRest,
	!.
lexically_transform_sent1([F | InNext], TransformId, [F | OutNext], TrivIn-TrivOut) :-
	!,
	lexically_transform_sent1(InNext, TransformId, OutNext, TrivIn-TrivOut).

