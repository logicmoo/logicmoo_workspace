
/*

train_classifier(+AnnotatedWavfiles, +RecParamsAlist, +FeatureExtractionSpecAlist, +DiscriminantSpec, +Discriminants, +RecOrNoRec)

Example call:

train_classifier(
	corpora(annotated_wavfiles_training_in_domain), 
	[1-[package=ebllm_gemini(recogniser_sem), grammar='.MAIN', 'rec.Pruning'=1200], 
         2-[package=slm(package), grammar='.TOP', 'rec.Pruning'=1200]], 
	[1-[hand_coded_patterns, sem_triples, lf_postproc_pred=riacs_postproc_lf], 
	 2-[class_unigrams, class_bigrams, class_trigrams]],
	[],
	alterf_generated_files('discriminants_slm.pl'),
	rec([1-alterf_generated_files('batchrec_results_train1.pl'), 2-alterf_generated_files('batchrec_results_train2.pl')])
    ).

Top-level call to build discriminants file. The arguments are as follows:

1. AnnotatedWavfiles. File of training data. A line is of the form

<WavfileOrDummyWavfile> | <SemanticAtoms> | <Transcription>

- <WavfileOrDummyWavfile> is either the absolute pathname of a wavfile (necessary if we want to do training using real speech data), or
else an arbitrary ID (permissible if we are going to train in text mode). 

- <SemanticAtoms> is a space-separated list of semantic atoms. These can be either simple unquoted Prolog atoms, or else 
one of the following special atoms:

    a) Positive integers, e.g. 1, 5, 42
    b) Numbers with a decimal point separator, e.g. 1.1, 2.5, 3.10. <N>.<M> is interpreted NOT as a standard decimal
       number, but as a pair of the form <<N>, <M>>.
    c) Times, expressed in the notation N:NN or NN:NN, e.g. 3:20, 05:15, 10:24, 8:03
    d) Ranges of numbers, expressed in the form <Num1>-<Num2>, e.g. 3-15, 3.1-10.2, 5.1-20

- <Transcription>. A space-separated list of atoms representing the text form of the training utterance.

Examples of possible lines from training file:

C:\home\speech\Corpora\wavfiles\checklist\se_091102\utt07.wav | load water | load water procedure
C:\home\speech\Corpora\wavfiles\checklist3\2003617_1254\utt64.wav | go_to caution step 5 | go to the caution for step five
C:\home\speech\Corpora\wavfiles\checklist3\2003617_1254\utt43.wav | stop_alarm 01:21 | cancel the alarm for oh one twenty one
C:\home\speech\Corpora\wavfiles\checklist3\2003617_1254\utt13.wav | read line 4 | read substep four
dummy_file174 | correction 6.5 | no i meant six point five

2. RecParamsAlist. List containing one or more elements of the form <Id>-<RecParams>, where <Id> is an arbitrary Prolog
atom and <RecParams> is a list of elements of the form <Key>=<Value>. The RecParamsAlist specifies how recognition is
to be carried out for each recogniser package used. Each package needs to specify at least the following two 
parameters:

    a) package. E.g. package=ebllm_gemini(recogniser_sem) says to use the compiled Nuance package ebllm_gemini(recogniser_sem)
    b) grammar. Top-level grammar to use in the specified package.

Other keys are interpreted as extra parameters to pass to the Nuance batchrec process, if training is carried out
in speech mode. They are ignored if training is carried out in text mode.

3. FeatureExtractionSpecAlist. List containing one or more elements of the form <Id>-<FeatExtractionSpec>, where the <Id>s 
are the same as the ones used for the RecParamsAlist, and the <FeatExtractionSpec>s are lists specifying the features
that are to be extracted for each recogniser. The following types of features are currently supported:

    - unigrams.            Single words from surface string.
    - bigrams.             Pairs of words from surface string.
    - trigrams.            Triples of words from surface string.
    - class_unigrams.      Single words from surface string, backed off using a tagging grammar defined by the predicate user:tagging_grammar/3.
                           An example of a tagging grammar appears in home/speech/rialist/checklist/Alterf/Prolog/checklist_tagging_grammar.pl
    - class_bigrams.       Like class_unigrams, but pairs.
    - class_trigrams.      Like class_unigrams, but triples.
    - sem_triples.         Semantic triples. These are closely modelled on the semantic triples used in the SRI CLE project, and are extracted
                           from logical forms.
    - hand_coded_patterns. Matches of subforms of the logical form to patterns defined by the predicate user:alterf_pattern/3. 
                           Examples of patterns appears in home/speech/rialist/checklist/Alterf/Prolog/checklist_alterf_patterns.pl

If you are using sem_triples or hand_coded_patterns, you may need to define a post processing predicate, which is applied to
the LF before further processing is carried out. The syntax is lf_postproc_pred=<PostProcessingPred>. For the current Checklist
system, we need lf_postproc_pred=riacs_postproc_lf.   

4. DiscriminantSpec. List containing zero or more elements of the form <Key>=<Value>, specifying how the discriminants are to
be calculated. The following keys and values are supported. Many of these are probably now only of historical interest,
and represent unsuccessful attempts to tune the algorithm. The definitions are in make_discriminants.pl.

- bayes_version.             Possible values: [naive, normalised]. 
                             'naive' -> discriminant score = log2(P(Atom | Feat)), 
                             'normalised' -> log2(P(Atom | Feat)) - log2(P(Atom)
                             'Normalised' seemed to give better results for small amounts of training data, but didn't hold up.

- use_proportion_of_data     Real number between 0 and 1. What proportion of the data to use, rest is discarded.

- rec_ok_weight.             Positive integer. If greater than 1, count each correctly recognised utterance as though it had
                             occurred N times rather than just once. Didn't turn out useful in the end.

- confidence_threshold_ignore. Positive number between 0 and 100. Discard utterances tagged "ignore" if they are under threshold.
                             Current default is 45, but right now we don't train on the ignored utterances so this is irrevelevant.

- confidence_threshold_non_ignore. Positive number between 0 and 100. Discard utterances not tagged "ignore" if they are under threshold.
                             Current default is 45. Training on low-confidence utterances seems to be a bad idea, so this is useful.

- discriminant_score_threshold. Real. Discard discriminants with values less than given threshold. This makes the discriminant table much
                             smaller, so it's useful. Default threshold is 0.5.

- discriminant_n_good_examples_threshold. Integer. Discard discriminants based on insufficiently many positive examples. One positive example
                             by default.

- mle_formula.               Possible values: [standard, carter]. Use the normal MLE formula or the compicated formula from the
                             appendix to the SLT book. 'carter' may be better, but so far not significantly so. 'normal' is default.

- hand_coded_rule_bonus.     Real. Can add a bonus to hand-coded rule scores if we want to prioritise them. Zero by default -
                             higher values haven't given better results.

- assumed_minimum_n_good_examples_for_rule. We can assume we have more positive examples for a rule-based feature if we want.
                             Minimum of 10 by default.

5. Discriminants. 

Write out discriminants to this file. The format is 

d(<Feat>, <Atom>, <NGood>, <NBad>, <Score>).

So for example

d(class_trigram([the,alarm,for]),stop_alarm,2,0,1.0).

means that there were two good examples and zero bad examples associating the class trigram feature for the trigram [the,alarm,for]
and the semantic atom stop_alarm, giving a score of 1.0.

Similarly,

d(sem_triple([bag,adj,small]),show,18,7,0.75).

means that there were 18 good examples and 7 bad examples associating the sem triple feature for the triple [bag,adj,small]
and the semantic atom show, giving a score of 0.75.

You can find an example of a discriminants file in home/speech/rialist/checklist/Alterf/GeneratedFiles/discriminants.pl.

6. RecOrNoRec

This can have three possible types of value:

- text. Carry out training in text mode, using only the transcriptions.

- rec(<List>). <List> is a list containing one or more elements of the form <Id>-<File>, where the <Id>s 
are the same as the ones used for the RecParamsAlist, and the recognition results get written to the
<Files>s for each recogniser.

- saved_rec(<List>). <List> is a list in the same format as for rec(<List>). Training doesn't carry
out recognition, but just uses the results saved in the designated files.

*/

%--------------------------------------------------------------------------------------------

:- module(classifier_trainer,
	  [train_classifier/6]
      ). 

%--------------------------------------------------------------------------------------------

:- use_module('$REGULUS/Alterf/Prolog/parse_annotated_wavfiles').
:- use_module('$REGULUS/Alterf/Prolog/batchrec').
:- use_module('$REGULUS/Alterf/Prolog/extract_feats').
:- use_module('$REGULUS/Alterf/Prolog/make_discriminants').

:- use_module('$REGULUS/Alterf/Prolog/classifier_utilities').

:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(system)).
:- use_module(library(lists)).
%:- use_module(library(ordsets)).

%--------------------------------------------------------------------------------------------

/*

Top-level is very simple:

- Do any front-end processing required on the training data (real batchrec, text batchrec, or retrieve saved results).

- Extract features from batchrec/text-batchrec results using routines in extract_feats.pl

- Turn feat vectors into discriminants using routines in make_discriminants.pl

*/

train_classifier(AnnotatedWavfiles0, RecParamsAlist, FeatureExtractionSpecAlist, DiscriminantSpec, DiscriminantScores, RecOrNoRec) :-
	check_alists_are_compatible([RecParamsAlist, FeatureExtractionSpecAlist]),
	
	make_classifier_tmp_files(TmpFeatVectors, TmpWavfiles, TmpTranscriptions, TmpSents, TmpAnnotatedWavfiles),

	parse_annotated_wavfiles(AnnotatedWavfiles0, TmpWavfiles, TmpTranscriptions, TmpSents, TmpAnnotatedWavfiles),
	    
	(   RecOrNoRec = rec(RecResultsAlist) ->
	    check_alists_are_compatible([RecParamsAlist, RecResultsAlist]),
	    do_batchrec_multiple(TmpWavfiles, TmpTranscriptions, RecParamsAlist, RecResultsAlist) ;

	    RecOrNoRec = text ->
	    do_text_batchrec_multiple(TmpTranscriptions, RecParamsAlist, RecResultsAlist) ;

	    RecOrNoRec = saved_rec(RecResultsAlist) ->
	    check_alists_are_compatible([RecParamsAlist, RecResultsAlist]),
	    check_saved_rec_results_alist(RecResultsAlist) ;

	    format('~N*** Error: bad value "~w" for last arg to train_classifier/6. Must be "rec(<RecResultAlist>)", "text" or "saved_rec(<RecResultAlist>)".~n', [RecOrNoRec]),
	    fail
	),

	rec_results_to_feature_vectors(RecResultsAlist, FeatureExtractionSpecAlist, TmpFeatVectors),
	feature_vectors_to_discriminant_scores(TmpAnnotatedWavfiles, TmpFeatVectors, DiscriminantSpec, DiscriminantScores).

%--------------------------------------------------------------------------------------------

make_classifier_tmp_files(TmpFeatVectors, TmpWavfiles, TmpTranscriptions, TmpSents, TmpAnnotatedWavfiles) :-
	make_tmp_file(feat_vectors, TmpFeatVectors),
	make_tmp_file(wavfiles, TmpWavfiles),
	make_tmp_file(transcriptions, TmpTranscriptions),
	make_tmp_file('sents.pl', TmpSents),
	make_tmp_file('annotated_wavfiles.pl', TmpAnnotatedWavfiles).

%--------------------------------------------------------------------------------------------

 
