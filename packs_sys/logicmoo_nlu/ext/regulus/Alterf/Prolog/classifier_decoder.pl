
%--------------------------------------------------------------------------------------------

:- module(classifier_decoder,
	  [decode_words_and_nl_value/4,
	   decode_words_and_nl_value/5,
	   present_decoder_pairs_used/2,
	   decoder_pairs_to_sorted_semantic_atoms/2]
      ).

%--------------------------------------------------------------------------------------------

:- use_module('$REGULUS/Alterf/Prolog/extract_feats').
:- use_module('$REGULUS/Alterf/Prolog/make_discriminants').
:- use_module('$REGULUS/Alterf/Prolog/generic_numbers').

:- use_module('$REGULUS/Alterf/Prolog/classifier_utilities').

:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).
:- use_module(library(assoc)).

%:- use_module(runtime(discriminants)).
%:- use_module(library(target_model)).

%--------------------------------------------------------------------------------------------

/*

decode_words_and_nl_value(+WordsAndNLValAlist, +FeatureExtractionSpecAlist, -SelectedAtoms, -PairsUsed, -AllPairs)

Top-level call to decoder. 

1. WordsAndNLValAlist

List of one or more items of the form Key-[Words, NLVal], where 

- Key is a key matching one of those in FeatureExtractionSpecAlist

- Words is a list of surface words (Prolog atoms)

- NLVal is a logical form

2. FeatureExtractionSpecAlist 

List in the format described in classifier_trainer.pl

3. SelectedAtoms

List of zero or more semantic atoms produced by decoding process.

4. PairsUsed

List of zero or more terms of the form

Score-[Feat, Atom, G, B]

meaning that Atom was selected due to the presence of a discriminant
associating it with Feat, based on G good and B bad examples and with
score Score.

5. AllPairs

All relevant pairs found, whether used or not.

--------------------------------------------------------------------------------------------

present_decoder_pairs_used(+Stream, +PairsUsed).

Print formatted representation of PairsUsed to Stream.

PairsUsed is list of zero or more terms in format described above under decode_words_and_nl_value/4

--------------------------------------------------------------------------------------------

decoder_pairs_to_sorted_semantic_atoms(+PairsUsed, -SemanticAtoms)

PairsUsed is list of zero or more terms in format described above under decode_words_and_nl_value/4

SemanticAtoms is list of corresponding semantic atoms.

*/

%--------------------------------------------------------------------------------------------

decode_words_and_nl_value(WordsAndNLValAlist, FeatureExtractionSpecAlist,
			  SelectedAtoms, PairsUsed) :-
  decode_words_and_nl_value(WordsAndNLValAlist, FeatureExtractionSpecAlist,
			    SelectedAtoms, PairsUsed, _).

decode_words_and_nl_value(WordsAndNLValAlist, FeatureExtractionSpecAlist, SelectedAtoms, PairsUsed, AllPairs) :-
	check_alists_are_compatible([WordsAndNLValAlist, FeatureExtractionSpecAlist]),

	findall(
	Score-[Feat, Atom, G, B], 
	feat_atom_g_b_and_score_for_words_and_nlval_alist(WordsAndNLValAlist, FeatureExtractionSpecAlist, Feat, Atom, G, B, Score),
	Pairs),
	keysort(Pairs, SortedPairs),
	reverse(SortedPairs, AllPairs),
	decode_from_sorted_discriminants(AllPairs, SelectedAtoms, PairsUsed),
	!.
decode_words_and_nl_value(WordsAndNLValAlist, FeatureExtractionSpecAlist, SelectedAtoms, PairsUsed, AllPairs) :-
	format('~N*** Error: bad call: ~w~n', [decode_words_and_nl_value(WordsAndNLValAlist, FeatureExtractionSpecAlist, SelectedAtoms, PairsUsed, AllPairs)]),
	fail.

%--------------------------------------------------------------------------------------------

present_decoder_pairs_used(_S, []).
present_decoder_pairs_used(S, [F | R]) :-
	present_pair(S, F),
	!,
	present_decoder_pairs_used(S, R).

present_pair(S, Score-[Feat, Atom, G, B]) :-
	format(S, '~N% Score: ~2f; Atom: ~w; Feat: ~w (~d-~d)~n', [Score, Atom, Feat, G, B]).

%--------------------------------------------------------------------------------------------

% We prefer the pairs in order smallest first, since we're planning to throw away small ones first.
decoder_pairs_to_sorted_semantic_atoms(DecoderPairs, SortedSemanticAtoms) :-
	keysort(DecoderPairs, ReversedDecoderPairs),
	decoder_pairs_to_semantic_atoms(ReversedDecoderPairs, SortedSemanticAtoms),
	!.
decoder_pairs_to_sorted_semantic_atoms(DecoderPairs, SortedSemanticAtoms) :-
	format('~N*** Error: bad call: ~w~n', [decoder_pairs_to_sorted_semantic_atoms(DecoderPairs, SortedSemanticAtoms)]),
	fail.
	
decoder_pairs_to_semantic_atoms([], []) :-
	!.
decoder_pairs_to_semantic_atoms([F | R], [F1 | R1]) :-
	decoder_pair_to_semantic_atom(F, F1),
	decoder_pairs_to_semantic_atoms(R, R1),
	!.
decoder_pairs_to_semantic_atoms(Pairs, Atoms) :-
	format('~N*** Error: bad call: ~w~n', [decoder_pairs_to_semantic_atoms(Pairs, Atoms)]),
	fail.

decoder_pair_to_semantic_atom(_Score-[_Feat, Atom, _G, _B], Atom).

%--------------------------------------------------------------------------------------------

decode_from_sorted_discriminants(SortedPairs, SortedSelectedAtoms, PairsUsed) :-
	sorted_pairs_to_target_atom_assoc(SortedPairs, TargetAtomsAssoc, NTargetAtoms),
	decode_from_sorted_discriminants1(SortedPairs, TargetAtomsAssoc, NTargetAtoms, PairsUsed-[]),
	%target_atoms_assoc_to_selected_atoms(TargetAtomsAssoc, SelectedAtoms).
	decoder_pairs_to_semantic_atoms(PairsUsed, SelectedAtoms),
	sort(SelectedAtoms, SortedSelectedAtoms),
	!.
decode_from_sorted_discriminants(SortedPairs, SelectedAtoms, PairsUsed) :-
	format('~N*** Error: bad call: ~w~n', [decode_from_sorted_discriminants(SortedPairs, SelectedAtoms, PairsUsed)]),
	fail.

%--------------------------------------------------------------------------------------------

decode_from_sorted_discriminants1([], _TargetAtomsAssoc, _NUnresolvedAtoms, PairsUsedIn-PairsUsedIn) :-
	!.
decode_from_sorted_discriminants1(_SortedPairs, _TargetAtomsAssoc, NUnresolvedAtoms, PairsUsedIn-PairsUsedIn) :-
	NUnresolvedAtoms = 0,
	!.
decode_from_sorted_discriminants1([F | R], TargetAtomsAssoc, NUnresolvedAtomsIn, PairsUsedIn-PairsUsedOut) :-
	NUnresolvedAtomsIn > 0,
	use_single_discriminant(F, TargetAtomsAssoc, NUnresolvedAtomsIn, NUnresolvedAtomsNext, PairsUsedIn-PairsUsedNext),
	!,
	decode_from_sorted_discriminants1(R, TargetAtomsAssoc, NUnresolvedAtomsNext, PairsUsedNext-PairsUsedOut).

%--------------------------------------------------------------------------------------------

use_single_discriminant(Pair, TargetAtomsAssoc, NUnresolvedIn, NUnresolvedNext, PairsUsedIn-PairsUsedOut) :-
	Pair = _Score-[_Feat, Atom, _G, _B],
	make_numbers_in_feat_generic(Atom, Atom1, _Substitution),
	get_assoc(Atom1, TargetAtomsAssoc, AtomStatus),
	use_single_discriminant1(AtomStatus, Atom1, TargetAtomsAssoc, NUnresolvedIn, NUnresolvedNext, Pair, PairsUsedIn-PairsUsedOut),
	!.
use_single_discriminant(Pair, TargetAtomsAssoc, NUnresolvedIn, NUnresolvedNext, PairsUsedIn-PairsUsedOut) :-
	format('~N*** Error: bad call: ~w~n', [use_single_discriminant(Pair, TargetAtomsAssoc, NUnresolvedIn, NUnresolvedNext, PairsUsedIn-PairsUsedOut)]),
	fail.

% If AtomStatus is not a variable, then it's already been resolved - nothing to do.
use_single_discriminant1(AtomStatus, _Atom, _TargetAtomsAssoc, NUnresolvedIn, NUnresolvedIn, _Pair, PairsUsedIn-PairsUsedIn) :-
	nonvar(AtomStatus),
	!.
use_single_discriminant1(AtomStatus, Atom1, TargetAtomsAssoc, NUnresolvedIn, NUnresolvedOut, Pair, PairsUsedIn-PairsUsedOut) :-
	AtomStatus = y,
	!,
	PairsUsedIn = [Pair | PairsUsedOut],
	NUnresolvedNext is NUnresolvedIn - 1,
	propagate_positive_atom(Atom1, TargetAtomsAssoc, NUnresolvedNext, NUnresolvedOut).

%--------------------------------------------------------------------------------------------

propagate_positive_atom(Atom1, TargetAtomsAssoc, NUnresolvedIn, NUnresolvedOut) :-
	user:target_atom_excludes(Atom1, ExcludedAtoms),
	propagate_positive_atom1(ExcludedAtoms, TargetAtomsAssoc, NUnresolvedIn, NUnresolvedOut),
	!.
propagate_positive_atom(_Atom1, _TargetAtomsAssoc, NUnresolvedIn, NUnresolvedIn).

propagate_positive_atom1([], _TargetAtomsAssoc, NUnresolvedIn, NUnresolvedIn) :-
	!.
propagate_positive_atom1(_ExcludedAtoms, _TargetAtomsAssoc, 0, 0) :-
	!.
propagate_positive_atom1([ExcludedAtom | R], TargetAtomsAssoc, NUnresolvedIn, NUnresolvedOut) :-
	NUnresolvedIn > 0,
	(   ( get_assoc(ExcludedAtom, TargetAtomsAssoc, AtomStatus), var(AtomStatus) ) ->
	    AtomStatus = n,
	    NUnresolvedNext is NUnresolvedIn - 1 ;
	    NUnresolvedNext is NUnresolvedIn
	),
	!,
	propagate_positive_atom1(R, TargetAtomsAssoc, NUnresolvedNext, NUnresolvedOut).

%--------------------------------------------------------------------------------------------

sorted_pairs_to_target_atom_assoc(SortedPairs, TargetAtomsAssoc, NTargetAtoms) :-
	findall(Atom, possibly_generic_atom_in_pairs(Atom, SortedPairs), TargetAtoms0),
	remove_duplicates(TargetAtoms0, TargetAtoms),
	length(TargetAtoms, NTargetAtoms),
	atom_list_to_target_atom_assoc(TargetAtoms, TargetAtomsAssoc).

possibly_generic_atom_in_pairs(Atom1, Pairs) :-
	member(_Score-[_Feat, Atom, _G, _B], Pairs),
	make_numbers_in_feat_generic(Atom, Atom1, _Substitution).

atom_list_to_target_atom_assoc(TargetAtoms, TargetAtomsAssoc) :-
	atom_list_to_atom_var_keyval_list(TargetAtoms, TargetAtomsKV),
	list_to_assoc(TargetAtomsKV, TargetAtomsAssoc).

atom_list_to_atom_var_keyval_list([], []).
atom_list_to_atom_var_keyval_list([F | R], [F-_Var | R1]) :-
	atom_list_to_atom_var_keyval_list(R, R1).

%--------------------------------------------------------------------------------------------

target_atoms_assoc_to_selected_atoms(TargetAtomsAssoc, SelectedAtoms) :-
	assoc_to_list(TargetAtomsAssoc, TargetAtomsKV),
	target_atoms_kv_to_selected_atoms(TargetAtomsKV, SelectedAtoms).

target_atoms_kv_to_selected_atoms([], []).
target_atoms_kv_to_selected_atoms([_Atom-Val | R], R1) :-
	Val == n,
	!,
	target_atoms_kv_to_selected_atoms(R, R1).
target_atoms_kv_to_selected_atoms([Atom-Val | R], [Atom | R1]) :-
	Val == y,
	!,
	target_atoms_kv_to_selected_atoms(R, R1).
target_atoms_kv_to_selected_atoms(TargetAtomsKV, SelectedAtoms) :-
	format('~N*** Error: bad call: ~q~n', [target_atoms_kv_to_selected_atoms(TargetAtomsKV, SelectedAtoms)]),
	fail.

%--------------------------------------------------------------------------------------------

feat_atom_g_b_and_score_for_words_and_nlval_alist(WordsAndNLValAlist, FeatureExtractionSpecAlist, Feat, Atom, G, B, Score) :-
	member(Key-[Words, NLVal], WordsAndNLValAlist),
	member(Key-FeatureExtractionSpec, FeatureExtractionSpecAlist),
	feat_atom_g_b_and_score_for_words_and_nlval(Words, NLVal, FeatureExtractionSpec, Feat, Atom, G, B, Score).

%--------------------------------------------------------------------------------------------

feat_atom_g_b_and_score_for_words_and_nlval(Words, NLVal, FeatureExtractionSpec, Feat, Atom, G, B, Score) :-
	feat_for_words_and_nlval(Feat0, Words, NLVal, FeatureExtractionSpec),
	make_numbers_in_feat_generic(Feat0, Feat, Substitution),
	user:d(Feat, Atom0, G, B, Score),
	(   Substitution = SpecificAtom/GeneralAtom ->
	    %Atom = SpecificAtom ;
	    substitute_in_term(Atom0, GeneralAtom/SpecificAtom, Atom) ;
	    Atom = Atom0
	).

feat_for_words_and_nlval(Feat, Words, _NLVal, FeatureExtractionSpec) :-
	feat_for_surface_words(Feat, FeatureExtractionSpec, Words).
feat_for_words_and_nlval(Feat, _Words, NLVal, FeatureExtractionSpec) :-
	feat_for_nl_value(Feat, FeatureExtractionSpec, NLVal).

%--------------------------------------------------------------------------------------------

substitute_in_term(V, _Subst, V) :-
	var(V),
	!.
substitute_in_term(Atom, From/To, Atom1) :-
	atomic(Atom),
	!,
	(   Atom = From ->
	    Atom1 = To ;
	    Atom1 = Atom
	).
substitute_in_term(Term, From/To, Term1) :-
	functor(Term, F, N),
	functor(Term1, F, N),
	substitute_in_term_args(N, Term, From/To, Term1).

substitute_in_term_args(0, _Term, _From/_To, _Term1).
substitute_in_term_args(I, Term, From/To, Term1) :-
	I > 0,
	arg(I, Term, Arg),
	arg(I, Term1, Arg1),
	substitute_in_term(Arg, From/To, Arg1),
	I1 is I - 1,
	!,
	substitute_in_term_args(I1, Term, From/To, Term1).
