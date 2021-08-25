/* PD624 code file: CH5-3.PL
More powerful frame and rule-based medical diagnosis system
from Section 5.3 of PD624 'Knowledge Engineering'
*/
/* patient database */
patient subclass_of person with
	symptoms: [],
	history: [],
	signs: [].

adam instance_of patient with
	symptoms: [fever, systemic_upset,purulent_sputum],
	history: [previous_lung_disease],
 signs : [finger_clubbing,halitosis].

bob instance_of patient with
	symptoms: [sneezing, runny_nose],
	history: [],
	signs: [red_itchy_watery_eyes].

carol instance_of patient with
	symptoms: [cough, sneezing, fever, dry_throat],
	history: [],
	signs: [hoarse_voice].

/* donna and earl are undiagnosable in this db... you'll have to augment it to
   work out what they have! */

donna instance_of patient with
	symptoms: [cough, sneezing, fever, shivering],
	history: [],
	signs: [].

earl instance_of patient with
	symptoms: [],
	history: [post_operative, ageing],
	signs: [].

/* diseases */
respiratory_tract_disease subclass_of disease with
	indicators: [productive_cough, dry_cough, breathlessness, sneezing,
 runny_nose, fever],
	physical_signs: [],
	typical_histories_or_contexts: [],
	discriminators: [].

upper_rtd subclass_of respiratory_tract_disease with
	indicators: [fever, dry_cough, sneezing, runny_nose],
	physical_signs: [],
	typical_histories_or_contexts: [],
	discriminators: [].

lower_rtd subclass_of respiratory_tract_disease with
	indicators: [productive_cough, breathlessness, fever],
	physical_signs: [],
	typical_histories_or_contexts: [],
	discriminators: [].

pneumonia subclass_of lower_rtd with
	indicators: [productive_cough, fever, systemic_upset,
             purulent_sputum, mucoid_sputum],
	physical_signs: [],
	typical_histories_or_contexts: [],
	discriminators: [].

bacterial_pneumonia subclass_of pneumonia with
	indicators: [purulent_sputum],
	physical_signs: [],
	typical_histories_or_contexts: [],

	discriminators: [].
non_bacterial_pneumonia subclass_of pneumonia with
indicators: [mucoid_sputum],
physical_signs: [],
typical_histories_or_contexts: [],
discriminators: [].

staphylococcal_pneumonia instance_of bacterial_pneumonia with
	indicators: [fever, purulent_sputum, malaise ],
	physical_signs: [crepitations ],
	typical_histories_or_contexts: [previous_respiratory_disease],
	discriminators: [gram_positive_cocci].

bronchiectasis instance_of lower_rtd with
	indicators: [fever, systemic_upset, purulent_sputum],
	physical_signs: [finger_clubbing, halitosis, breathlessness, cyanosis],
	typical_histories_or_contexts: [previous_lung_disease],
	discriminators: ['bronchiectasis seen on bronchogram'].

lung_abscess  instance_of lower_rtd with
	indicators: [fever, systemic_upset, purulent_sputum],
	physical_signs: [consolidation, halitosis, finger_clubbing],
	typical_histories_or_contexts: [unresolving_pneumonia],
	discriminators: [pulmonary_cavitation_with_fluid_levels].

hay_fever instance_of upper_rtd with
	indicators: [sneezing, runny_nose],
	physical_signs: [red_itchy_watery_eyes],
	typical_histories_or_contexts: [],
	discriminators: [positive_reaction_to_allergens].

laryngitis instance_of upper_rtd with
	indicators: [fever, dry_cough, malaise],
	physical_signs: [hoarse_voice],
	typical_histories_or_contexts: [],
	discriminators: [inflamed_larynx ].

common_cold instance_of upper_rtd with
	indicators: [runny_nose, sneezing, fever],
	physical_signs: [headache],
	typical_histories_or_contexts: [],
	discriminators: [negative_reaction_to_allergens].

influenza instance_of upper_rtd with
	indicators: [fever, dry_cough, malaise],
	physical_signs: [discomfort],
	typical_histories_or_contexts: [],
	discriminators: [sore_throat_and_persistent_dry_cough].
/* control rules */
rule init forward
	if
		start
	then
		remove start &
		query the name of patient receives_answer X &
		add goal(hypothesize).  /* see next comment for
explanation of 'goals' */

rule switch_strategies_1 forward
	if
		goal(hypothesize)
	then
		remove goal(hypothesize) &
		add goal(refine).

rule switch_strategies_2 forward
	if
		goal(refine)
	then
		remove goal(refine) &
		add goal(discriminate).

rule switch_strategies_3 forward
	if
		goal(discriminate)
	then
		remove goal(discriminate) &
		add goal(choose_winner).

rule switch_strategies_4 forward
	if
		goal(choose_winner)
	then
		remove goal(choose_winner) &
		halt.               /* <=== This is where the whole thing ends  */

/* forward chaining diagnosis rules */
rule ordinary_diagnosis forward
	if
		goal(hypothesize) &	/* in 'hypothesis' mode? */
		the name of patient is N &	/* retrieve name */
		the symptoms of N is Symp &	/* now find ANY symptom Symp */
		D subclass_of disease &	/* and any category of disease...  */
		-- possible(D) &	/* that we haven't suggested already...  */
		the indicators of D is Symp 	/* which might be indicated by Symp */
	then
		add possible(D).	/* place it in working memory */

rule refinement_to_subclass forward
	if
		goal(refine) &
		possible(DiseaseClass) &	/* given this candidate */
		Subclass subclass_of DiseaseClass & 	/* find a subclass of it...  */
		-- possible(Subclass) &	/* which we haven't dealt with yet */
  deduce allowable(Subclass)
	then
  announce ['just refined down to subclass ',Subclass] &
		add possible(Subclass).	/* if so, add to set of 'possibles' */

rule refinement_to_instance forward  /* as above, but only for instances */
	if
		goal(refine) &
		possible(DiseaseClass) &	/* given this candidate */
		Disease instance_of DiseaseClass & 	/* find an instance of it...  */
		-- possible(Disease) &	/* which we haven't dealt with yet */
		deduce passes_phys_sign_test(Disease)	/* see if it passes further tests */
	then
  announce ['just passed physical sign for disease instance ',Disease] &
		add possible_instance(Disease).	/* if so, add to set of 'possibles' */

rule eliminate forward
	if
		goal(discriminate) &
		possible_instance(X) &
		deduce passes_discriminating_test(X)
	then
  announce ['a highly likely candidate after discriminating test is ',X] &
		add likely(X).

rule simple_selection forward  /* specifity prefers above rule to this one */
	if
		goal(choose_winner) &
		likely(X)
	then
		remove likely(X) &
		announce ['A likely diagnosis is that the patient has ', X].


/* ---------------- backward chaining 'filters'---------------------- */

rule allowable_1 backward
 if
   the name of patient is N &
   the indicators of Disease is Ind &
   the symptoms of N is Ind     /* one symptom in common with indicators */
 then
   allowable(Disease).


rule necessary_sign_test backward
	if
		the physical_signs of Disease is Sign &	/* get any sign */
		the name of patient is N &
		the signs of N is Sign /* see if patient has got it */
	then
		passes_phys_sign_test(Disease).

rule discriminatory_diagnosis backward
	if
		the discriminators of X is D &
		query ['Upon further investigation, is there solid evidence of ', D]
        receives_answer yes
	then
		passes_discriminating_test(X).


/* PD624 code file: CH6.PL
   master 'explain' file, as described in Chapter 6 of
   PD624: Knowledge Engineering
   The format for explanations is
   <pattern> explained_by <text-list>
   Where <pattern> is any valid Prolog term, typically a string or a
list, which is normally passed in to the operator 'query' in a
MIKE rule.  <text-list> is a list of atoms or quoted strings
*/
'What is the name of the patient' explained_by
  ['In order to carry out a diagnosis, we first need',nl,
  'to determine the name of the patient',
   nl,'in question',nl].
the name of patient explained_by
   ['In order to carry out a diagnosis, we first need',nl,
   'to determine the name of the patient in question',nl].
'does the patient show positive reaction to allergens' explained_by
  ['Answering this question will help MIKE determine whether the',nl,
  'patient is suffering from hay fever (seasonal allergic rhinitis)',
   nl,'or if the symptoms are instead indicative of a common cold',nl].
'does the patient show signs of erythematous larynx' explained_by
      ['Answering this question will help MIKE determine whether the',nl,
      'patient is suffering from laryngitis',nl].
goal(hypothesize) explained_by
        ['Given a particular set of symptoms, MIKE is trying to',
        nl,'determine which are the possible "candidates"', nl].
goal(discriminate) explained_by
  ['Attempting to trim down the set of possible "candidates"',nl].
goal(choose_winner) explained_by
  ['Determine which of the possible candidates is/are the winner(s)',nl].
possible(laryngitis) explained_by
  ['the patient has fever, dry cough, and malaise',nl,
  'and therefore laryngitis is a possible ailment',nl].
possible(influenza) explained_by
  ['the patient has fever, a dry cough, and malaise',nl,
  'and therefore influenza is a possible ailment',nl].
possible(common_cold) explained_by
  ['the patient is sneezing, and has a runny nose and fever',
  nl,'therefore a possible explanation is a common cold',nl].
possible(hay_fever_1) explained_by
['the patient is sneezing and has a blocked nose',nl,
'therefore hay fever is a possibility',nl].
possible(hay_fever_2) explained_by
['the patient is sneezing and has a runny nose',nl,
'therefore hay fever is a possible ailment',nl].
likely(X) explained_by
['Using the known symptoms of the patient ',X,
nl,'was identified as a possible source of the complaint',
nl,'After this more descriminating tests were applied.',nl,
X,' passed these further criteria, and thus is deemed',nl,
'a likely cause of the patient''s condition',nl].
passes_discriminating_test(influenza) explained_by
['The answer helps indicate whether it is likely that the',nl,
'patient has influenza',nl].
'does the patient show positive reactions to allergens'
explained_by ['The answer to this question is important for
determining whether', nl,'the patient is suffering from a common
cold',nl].
passes_discriminating_test(laryngitis) explained_by ['The
answer to this questions is important for determing',nl, 'whether
the patient is suffering from laryngitis',nl].

['Upon further investigation, is there solid evidence of ', D]
      explained_by
['Having worked our way down through the hierarchy of disease classes and',
nl,
'subclasses all the way down to individual instances, we are at last in a',
nl,
'position to make a discrimination among all of the final contenders',
nl,
'by performing a critical test.  This particular test, namely evidence of',
nl,
D,nl,
'will help us home in on a final choice'].


