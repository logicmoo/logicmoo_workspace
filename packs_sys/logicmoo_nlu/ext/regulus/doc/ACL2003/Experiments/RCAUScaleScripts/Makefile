
default:
	sicstus -l build_nuance_files.pl
	../../../../scripts/run_license.bat &
	sleep 5
	nuance-compile ../GeneratedFiles/psa_rcau_1_recogniser English.America -auto_pron -dont_flatten 
	nuance-compile ../GeneratedFiles/psa_rcau_2_recogniser English.America -auto_pron -dont_flatten 
	nuance-compile ../GeneratedFiles/psa_rcau_5_recogniser English.America -auto_pron -dont_flatten 
	nuance-compile ../GeneratedFiles/psa_rcau_10_recogniser English.America -auto_pron -dont_flatten 
	nuance-compile ../GeneratedFiles/psa_rcau_25_recogniser English.America -auto_pron -dont_flatten 
	nuance-compile ../GeneratedFiles/psa_rcau_50_recogniser English.America -auto_pron -dont_flatten 
	nuance-compile ../GeneratedFiles/psa_rcau_100_recogniser English.America -auto_pron -dont_flatten 

clean:
	rm *~


