
default:
	sicstus -l build_nuance_files.pl
	# You need to make sure you have a Nuance license in $REGULUS/scripts/run_license.bat
	../../../scripts/run_license.bat &
	sleep 5
	nuance-compile ../GeneratedFiles/psa_specialised_recogniser English.America -auto_pron -dont_flatten 

clean:
	rm -R ../GeneratedFiles/*


