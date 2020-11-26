
default:
	sicstus -l build_nuance_files_orig.pl
	sicstus -l build_nuance_files_house.pl
	sicstus -l build_nuance_files_travel_deals.pl
	sicstus -l build_nuance_files_med.pl
	sicstus -l build_nuance_files_checklist.pl
	sicstus -l build_nuance_files_mobile_agents.pl
	../../../../scripts/run_license.bat &
	sleep 5
	nuance-compile ../GeneratedFiles/psa_original_recogniser English.America -auto_pron -dont_flatten 
	nuance-compile ../GeneratedFiles/psa_house_recogniser English.America -auto_pron -dont_flatten 
	nuance-compile ../GeneratedFiles/psa_travel_deals_recogniser English.America -auto_pron -dont_flatten 
	nuance-compile ../GeneratedFiles/psa_med_recogniser English.America -auto_pron -dont_flatten 
	nuance-compile ../GeneratedFiles/psa_checklist_no_syn_recogniser English.America -auto_pron -dont_flatten 
	nuance-compile ../GeneratedFiles/psa_mobile_agents_no_syn_recogniser English.America -auto_pron -dont_flatten 
	nuance-compile ../GeneratedFiles/psa_checklist_recogniser English.America -auto_pron -dont_flatten 
	nuance-compile ../GeneratedFiles/psa_mobile_agents_recogniser English.America -auto_pron -dont_flatten 

clean:
	rm *~


