
get_verbs:
	grep 'Verb' ${ACCEPT}/MT/Europarl/Mmorph/toto.txt > ${ACCEPT}/MT/Europarl/Mmorph/verbs.txt
	wc ${ACCEPT}/MT/Europarl/Mmorph/verbs.txt
	sicstus -l run_parse_verbs.pl

get_propositions:
	grep 'Prep' ${ACCEPT}/MT/Europarl/Mmorph/toto.txt > ${ACCEPT}/MT/Europarl/Mmorph/prepositions.txt
	wc ${ACCEPT}/MT/Europarl/Mmorph/prepositions.txt
	sicstus -l run_parse_prepositions.pl

get_nouns:
	grep 'Noun' ${ACCEPT}/MT/Europarl/Mmorph/toto.txt > ${ACCEPT}/MT/Europarl/Mmorph/nouns.txt
	wc ${ACCEPT}/MT/Europarl/Mmorph/nouns.txt
	sicstus -l run_parse_nouns.pl

# ---------------------------------------------------------

all:
	${MAKE} all_ez
	${MAKE} all_te
	${MAKE} all_questions

# ---------------------------------------------------------

all_ez:
	${MAKE} extract_ez
	${MAKE} transform_ez
	${MAKE} orthography_ez
	${MAKE} filter_ez

main_ez:
	${MAKE} transform_ez
	${MAKE} orthography_ez
	${MAKE} filter_ez

# Get the subset of the bicorpus where the French has 'ez ' and none of the common distractors, e.g. 'chez'
extract_ez:
	sicstus -l run_extract_ez.pl

# Transform 2 pl to 2 sing where possible
transform_ez:
	sicstus -l run_transform_ez.pl

# Tidy up orthography in the French
orthography_ez:
	sicstus -l run_orthography_ez.pl

# Discard lines in the bicorpus that weren't transformed
filter_ez:
	sicstus -l run_filter_ez.pl

# ---------------------------------------------------------

all_te:
	${MAKE} extract_te
	${MAKE} transform_te
	${MAKE} orthography_te
	${MAKE} filter_te

main_te:
	${MAKE} transform_te
	${MAKE} orthography_te
	${MAKE} filter_te

# Get the subset of the bicorpus where the French has 'vous'
extract_te:
	sicstus -l run_extract_te.pl

# Transform 2 pl to 2 sing where possible
transform_te:
	sicstus -l run_transform_te.pl

# Tidy up orthography in the French
orthography_te:
	sicstus -l run_orthography_te.pl

# Discard lines in the bicorpus that weren't transformed
filter_te:
	sicstus -l run_filter_te.pl

# ---------------------------------------------------------

all_questions:
	${MAKE} extract_questions
	${MAKE} transform_questions
	${MAKE} orthography_questions
	${MAKE} filter_questions

main_questions:
	${MAKE} transform_questions
	${MAKE} orthography_questions
	${MAKE} filter_questions

# Get the subset of the bicorpus where the French contains a question mark and a string of the form "-<Pronoun>".
extract_questions:
	sicstus -l run_extract_questions.pl

# Transform 2 pl to 2 sing where possible
transform_questions:
	sicstus -l run_transform_questions.pl

# Tidy up orthography in the French
orthography_questions:
	sicstus -l run_orthography_questions.pl

# Discard lines in the bicorpus that weren't transformed
filter_questions:
	sicstus -l run_filter_questions.pl

