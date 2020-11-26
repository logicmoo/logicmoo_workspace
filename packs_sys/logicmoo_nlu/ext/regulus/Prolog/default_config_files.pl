% Example file listing config files.
% Replace this with your own config files and copy to $REGULUS/Prolog/config_files.pl

file("Toy1", '$REGULUS/Examples/Toy1/scripts/toy1.cfg').
file("Toy1 Fre", '$REGULUS/Examples/Toy1/scripts/french.cfg').
file("Toy1 EngFre", '$REGULUS/Examples/Toy1/scripts/toy1_slt_batch.cfg', "LOAD", "LOAD_TRANSLATE").
file("Toy1 Specialised", '$REGULUS/Examples/Toy1Specialised/scripts/toy1_specialised.cfg').
file("MedSLT EngFre Headache", '$MED_SLT2/EngFre/Prolog/med_new_interlingua.cfg', "EBL_LOAD", "LOAD_TRANSLATE").
file("MedSLT FreEng Headache", '$MED_SLT2/FreEng/Prolog/med.cfg').
file("MedSLT EngJap Headache", '$MED_SLT2/EngJap/Prolog/med_new_interlingua.cfg', "EBL_LOAD", "LOAD_TRANSLATE").
file("MedSLT EngAra Headache", '$MED_SLT2/EngAra/Prolog/med.cfg', "EBL_LOAD", "LOAD_TRANSLATE").
file("MedSLT EngSpa Sore Throat",
     '$MED_SLT2/EngSpa/Prolog/med_sore_throat_merged.cfg',
     "EBL_LOAD", "LOAD_TRANSLATE", "TRANSLATE",
     "BIDIRECTIONAL_ON",
     "A: EBL_LOAD", "A: LOAD_TRANSLATE", "A: TRANSLATE", "A: ANSWER_ELLIPSIS_ON").
file("MedSLT EngSpa Sore Throat Restricted",
     '$MED_SLT2/EngSpa/Prolog/med_sore_throat_restricted_merged.cfg',
     "EBL_LOAD", "LOAD_TRANSLATE", "TRANSLATE",
     "BIDIRECTIONAL_ON",
     "A: EBL_LOAD", "A: LOAD_TRANSLATE", "A: TRANSLATE", "A: ANSWER_ELLIPSIS_ON").
file("MedSLT SpaEng Sore Throat",
     '$MED_SLT2/SpaEng/Prolog/med.cfg').
file("MedSLT Spa Recognition Q",
     '$MED_SLT2/Spa/scripts/merged_spa_recognition.cfg',
     "EBL_ANALYSIS").
file("MedSLT Spa Recognition A",
     '$MED_SLT2/Spa/scripts/merged_spa_recognition_answers.cfg',
     "EBL_ANALYSIS").
file("MedSLT Spa Generation Q",
     '$MED_SLT2/Spa/scripts/merged_spa_generation.cfg',
     "EBL_ANALYSIS").
file("MedSLT Spa Generation A",
     '$MED_SLT2/Spa/scripts/merged_spa_generation_answers.cfg',
     "EBL_ANALYSIS").
