
default:
	sicstus -l build_nuance_files_250.pl
	sicstus -l build_nuance_files_500.pl
	sicstus -l build_nuance_files_1000.pl
	sicstus -l build_nuance_files_2500.pl
	sicstus -l build_nuance_files_5000.pl
	../../../../scripts/run_license.bat &
	sleep 5
	nuance-compile ../GeneratedFiles/psa_250_recogniser English.America -auto_pron -dont_flatten 
	nuance-compile ../GeneratedFiles/psa_500_recogniser English.America -auto_pron -dont_flatten 
	nuance-compile ../GeneratedFiles/psa_1000_recogniser English.America -auto_pron -dont_flatten 
	nuance-compile ../GeneratedFiles/psa_2500_recogniser English.America -auto_pron -dont_flatten 
	nuance-compile ../GeneratedFiles/psa_5000_recogniser English.America -auto_pron -dont_flatten 

clean:
	rm *~


