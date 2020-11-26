
multi_ling:
	${MAKE} specialised_nuance_german_multi_ling
	${MAKE} recogniser_german_multi_ling

specialised_nuance_german_multi_ling:
	sicstus -l build_nuance_files_german_multi_ling.pl

recogniser_german_multi_ling:
	nuance-compile ../Generated/robots_specialised_recogniser German -auto_pron -dont_flatten 	
