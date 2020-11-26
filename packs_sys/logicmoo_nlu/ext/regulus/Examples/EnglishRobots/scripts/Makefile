
multi_ling:
	${MAKE} specialised_nuance_english_multi_ling
	${MAKE} recogniser_english_multi_ling

specialised_nuance_english_multi_ling:
	sicstus -l build_nuance_files_english_multi_ling.pl

recogniser_english_multi_ling:
	nuance-compile ../Generated/robots_specialised_recogniser English.America -auto_pron -dont_flatten 	
