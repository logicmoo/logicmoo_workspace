% %% this domain is under development, and a long ways from being able to work

% %% for examples of working behaviors, see:

% %% https://github.com/TeamSPoon/logicmoo_nlu/blob/master/prolog/marty_white/adv_axiom.pl
% %% https://github.com/TeamSPoon/logicmoo_nlu/blob/master/prolog/marty_white/adv_implies.pl

% %% ideas:

% %% can have it post constraints, like "do_not_leave_the_house", and
% %% then the web UI shows these.  when they ask to go shopping, it %%
% %% brings up a troubleshooter, since they're not supposed to leave
% %% the house

% %% special predicates:

% %% ensure_state, ensure_not, random, choose_one, ask_user, must, possible(k(...)), start/begin

% %% isa, are, 

macro_expand((A->B), (A->B;true)).

% cond/1 means we are expecting to test a conditional in cases a b/k/h are not used

trigger(daily(Person)) ==>>
   not(h(has_illness,Person,covid19)) -> 
   act(self_check_for_covid19_symptoms(Person)).

act(self_check_for_covid19_symptoms(Person)) ==>
  (   cond(self_check_for_covid19_symptoms_is_positive(Person)) ->
      (
       declare(h(needs_to_be_tested,Person,covid19)),
       act(get_tested_for_covid19(Person)),
       begin_state(patient_under_investigation(Person))
      )).

/*
daily(Person) ==>>
  not(h(has_illness,Person,covid19)),
  true.
*/

act(get_tested_for_covid19(SymptomaticPerson))  ==>>
  h(needs_to_be_tested,SymptomaticPerson,covid19),
  (   cond(tests_positive_for_covid19(SymptomaticPerson)) ->
      (
       declare(h(has_illness,SymptomaticPerson,covid19)),
       undeclare(h(needs_to_be_tested,SymptomaticPerson,covid19)),
       begin_state(patient_with_symptomatic_laboratory_confirmed_covid_19(SymptomaticPerson))
      ) ;
      (
       undeclare(h(needs_to_be_tested,SymptomaticPerson,covid19))
      )).

trigger(daily(Person)) ==>>
  true_code(has_household(Person,Household)),
  act(sanitize_enviornment(Household)).

trigger(frequently(Person)) ==>>
  act(perform_hand_hygiene(Person)).


act(perform_hand_hygiene(Person)) ==>>
  act(wash_hands(Person)).

act(sanitize_enviornment(Household)) ==>>
  act(wipe_down_all_surfaces_in_the_household_with_virus_killing_hospital_grade_santizing_wipes(Household)),
  act(clean_all__high_touch__surfaces(Household)),
  act(clean_any_surfaces_that_may_have_blood__stool__or_body_fluids_on_them(Household)).

clean_all__high_touch__surfaces(Household) ==>>
  household_has_members(Household,HouseholdMembers),
  choose_one(Person,HouseholdMembers),
  {member(SurfaceType,[counter,tabletop,doorknob,bathroom_fixtures,toilet,phone,keyboard,tablet,bedsideTable])},
  true_state(in(Surface,Household)),
  true_code(isa(Surface,SurfaceType)),
  sanitize_surface(Person,Surface).

sanitize_surface(Person,Surface) ==>>
  ensure_state(wearing_gloves(Person,_Gloves)),
  true_state(in(Surface,Room)),
  true_code(isa(Room,room)),
  ensure_state(well_ventilated(Room)),
  true_state(holding(Person,HouseholdCleaningSprayOrWipe)),
  true_code( isa(HouseholdCleaningSprayOrWipe,cleaningSpray) ; isa(HouseholdCleaningSprayOrWipe,cleaningWipe)),
  act(read_label(Person,labelFn(HouseholdCleaningSprayOrWipe))),
  act(use_as_directed_on(Person,HouseholdCleaningSprayOrWipe,Surface)).

act(wake_up) ==>>
  ignore(true_state(infectious_disease_outbreak) -> act(grab_tissues)).

act(self_check_for_covid19_symptoms_is_positive(Person)) ==>>
  act(check_temperature_fahrenheit(Person,Temperature)),
  sources([s(d('https://www.cdc.gov/coronavirus/2019-ncov/downloads/COVID-19_CAREKit_ENG.pdf'),s([56]))]),
  (   Temperature > 100.4 ->
      (
       declare(h(has_illness,Person,fever)),
       declare(h(is_symptomatic,Person,covid19))
      )).

self_check_for_covid19_symptoms_is_positive(Person) ==>>
	not(forall(isa(Symptom,covid19Symptom),not(has_symptom(Person,Symptom)))).

check_temperature_fahrenheit(Person,Temperature) ==>>
	sources([s(d('https://www.cdc.gov/coronavirus/2019-ncov/downloads/COVID-19_CAREKit_ENG.pdf'),s([56]))]),
	ensure_not('Have you exercised in the last 30 minutes?'),
	ensure_not('Have you taken medication that can lower your temperature in the last 6 hours?'),
        are([acetaminophen,paracetamol,aspirin],medicationThatCanLowerYourTemperature).

ensure_not(Question) ==>>
	ask_user(Person,Question,Result),
	Result = yes.


patient_with_symptomatic_laboratory_confirmed_covid_19(SymptomaticPerson) ==>>
  patient_either_under_investigation_or_with_symptomatic_laboratory_confirmed_covid_19(SymptomaticPerson).

patient_under_investigation(SymptomaticPerson) ==>>
  patient_either_under_investigation_or_with_symptomatic_laboratory_confirmed_covid_19(SymptomaticPerson).

patient_either_under_investigation_or_with_symptomatic_laboratory_confirmed_covid_19(SymptomaticPerson) ==>>
  do_not_handle_pets_or_other_animals_while_sick(SymptomaticPerson),
  close_contacts_should_monitor_their_health(SymptomaticPerson),
  avoid_other_people(SymptomaticPerson),
  must(do_not_touch_eyes_comma_nose_or_mouth_with_unwashed_hands(SymptomaticPerson)),
  avoid_sharing_household_items_with_the_patient(Person,SymptomaticPerson),
  has_household(SymptomaticPerson,Household),
  household_has_members(Household,HouseholdMembers),
  subtract(HouseholdMembers,SymptomaticPerson,HealthyHouseholdMembers),
  must(use_separate_room_and_bathroom(Household,HealthyHouseholdMembers,SymptomaticPerson)).

has_bathrooms(Household,Bathrooms) ==>>
  findall(Bathroom,(isa(Bathroom,bathroom),k(in,Bathroom,Household)),Bathrooms).

has_multiple_bathrooms(Household) ==>>
  has_bathrooms(Household,Bathrooms),
  length(Bathrooms,N),
  N > 1.

use_separate_room_and_bathroom(HealthyHouseholdMembers,SymptomaticPerson) ==>>
  has_household(SymptomaticPerson,Household),
  has_multiple_bathrooms(Household),
  has_bathrooms(Household,Bathrooms),
  choose_one(BathroomForSick,Bathrooms),
  choose_one(BathroomForHealthy,Bathrooms),
  must(designate_bathroom_for(HealthyHouseholdMembers,BathroomForHealthy)),
  must(designate_bathroom_for([SymptomaticPerson],BathroomForSick)).
  
avoid_sharing_household_items_with_the_patient(Person,SymptomaticPerson) ==>>
  household_has_members(Household,HouseholdMembers),
  member(Person,HouseholdMembers),
  member(SymptomaticPerson,HouseholdMembers),
  isa(Item,householdItem),
  in(Item,Household),
  must(not(share(Person,SymptomaticPerson,Item))).
  
cover_exposed_wounds_with_bandaids(Person) ==>>
  true.


avoid_other_people(Person) ==>>
  must(restrict_all_unnecessary_long_distance_travel(Person)),
  must(do_not_travel_unless_for_medical_care(Person)),
  must(prohibit_visitors(Person)).

close_contacts_should_monitor_their_health(Person) ==>>
  true.

visit_person(Person,SymptomaticPerson) ==>>
  ensure_necessary,
  (   possible(k(has_illness,SymptomaticPerson,covid19)) ->
      wear_facemask(Person,Facemask0),
      wear_facemask(SymptomaticPerson,Facemask1)).

wash_laundry_thoroughly ==>>
  %immediately 
  wear_gloves(Person,Gloves),
  handle_soiled_items(Person,SoiledItems),

% %% "wear disposable gloves while handling soiled items and . clean
% %% your hands (with soap and water or an
% %% alcohol-based hand sanitizer) immediately after removing your gloves".


handle_soiled_items(Person,SoiledItems) ==>>
  ensure_state(wearing(Person,DisposableGloves)),
  start(keep_soiled_items_away_from_your_body(Person,SoiledItems)),
  forall(member(SoiledItem,SoiledItems),
         (   disposable(SoiledItem) ->
       dispose_of_properly(Person,SoiledItem))).

remove_and_wash_clothes_or_bedding_that_have_blood__stool__or_body_fluids_on_them(Person,ClothesOrBedding) ==>>
  remove_properly(ClothesOrBedding),
  read_and_follow_directions_on_labels_of_laundry_or_clothing_items_and_detergent(Person,ClothesOrBedding),
  using_a_normal_laundry_detergent_according_to_washing,
  machine_instructions_and_dry_thoroughly_using_the_warmest_temperatures_recommended_on_the_clothing_label.  

remove_gloves(Person) ==>>
  wearing(Person,Gloves),
  isa(Gloves,gloves),
  remove_properly(Gloves),
  must(dispose_of_properly(Person,Gloves)),
  must(not(wear_gloves(Person,Gloves))),
  true
  .

remove_facemask(Person) ==>>
  wearing(Person,Facemask),
  isa(Facemask,facemask),
  remove_properly(Person,Facemask),
  must(dispose_of_properly(Person,Facemask)),
  must(not(wear_facemask(Person,Facemask))),  
  true
  .

wear_facemask(Person,Facemask) ==>>
  isa(Person,person),
  isa(Facemask,facemask),
  true.
  



%%% https://www.cdc.gov/coronavirus/2019-ncov/specific-groups/high-risk-complications.html#Have-supplies-on-hand

%%% Be sure you have over-the-counter medicines and medical supplies
%%% (tissues, etc.) to treat fever and other symptoms. Most people will
%%% be able to recover from COVID-19 at home.

%%% Have enough household items and groceries on hand so that you will be prepared to stay at home for a period of time.

%%% To the extent possible, avoid touching high-touch surfaces in public places – elevator buttons, door handles, handrails, handshaking with people, etc. Use a tissue or your sleeve to cover your hand or finger if you must touch something.

%%% Wash your hands after touching surfaces in public places.

%%% Avoid touching your face, nose, eyes, etc.


transfer_possession_of_items(Person0,Person1,Items) ==>>
  true.



%%% dishes, drinking glasses, cups, eating utensils, towels, or bedding

leave_sickroom(Person) ==>>
  empty_hands(Person), %%% Avoid sharing personal household items
  wear_facemask(Person,Facemask).

return_to_sickroom(Person) ==>>
  remove_facemask(Person).

go_to_doctor(Person,Doctor) ==>>
  call_ahead_before_visiting_your_doctor(Person,Doctor).

discontinuing_home_isolation(Person) ==>>
  true.

%%% The following symptoms may appear 2-14 days after exposure.*
hasSymptoms(coronavirus,[fever,cough,shortnessOfBreath]).

%%% Have been in close contact with a person known to have COVID-19 or
%%% have recently traveled from an area with widespread or ongoing
%%% community spread of COVID-19 . Call ahead before you go to a
%%% doctor’s office or emergency room. Tell them about your recent
%%% travel and your symptoms.

%%% Fact 3 Someone who has completed quarantine or has been released
%%% from isolation does not pose a risk of infection to other people.


when_being_being_passed_by_community_spread_in_your_country ==>>
  restrict_all_unnecessary_travel.

restrict_all_unnecessary_travel ==>>
  do_not_go_to_the_gym.

if_there_is_a_confirmed_case_in_your_hometown ==>>
  stay_indoors_as_much_as_possible,
  if_you_go_outside_stay_9_feet_or_more_from_passers_by,
  shop_at_times_of_day_where_there_are_not_a_lot_of_people,
  have_home_gym.

if_there_is_an_outbreak_in_your_hometown ==>>
  do_not_leave_house_at_all ;
  leave_the_city.

leave_the_city ==>>
  get_tested_for_coronavirus.

get_tested_for_coronavirus ==>>
  (   outbreak_in_of(YourHometown,coronavirus) -> true ; true).

get_home_test_kits ==>>
  true.

if_we_have_to_go_to_a_place_where_there_is_a_known_case ==>>
  true.

if_our_loved_ones_insist_on_going_to_a_place_where_there_is_a_known_case ==>>
  (   if_loved_one_lives_with_you ->
      institute_home_lockdown ;
      true).

institute_home_lockdown ==>>
  do_not_leave_rooms,
  stay_9_feet_apart.

what_if_someone_in_the_house_becomes_sick ==>>
  true.

what_if_we_have_to_go_out_of_the_house ==>>
  true.


(go_shopping(Person)) ==>>
  shop_at_times_of_day_where_there_are_not_a_lot_of_people,
  

(buy_groceries_at(Person,AldiStore)) ==>>
  storeIsPartOfChain(AldiStore,aldi),
        ensure_state(possesses(Person,Quarter)),
        isa(Quarter,quarter),
        ensure_state(possesses(Person,Bags)),
        isa(Bags,shoppingBags),
  go_to(Person,AldiStore).

clean_regularly ==>>
  true.

good_hygiene ==>>
  true.

hold_practice_trials ==>>
  someone_sick_in_the_house.
  

prepare_for_prolonged_outbreak ==>>
  true.

what_to_do_if_you_are_sick_with_coronavirus_disease_2019_covid_19 ==>>
  stay_home_except_to_get_medical_care,
  separate_yourself_from_other_people_and_animals_in_your_home.

separate_yourself_from_other_people_and_animals_in_your_home ==>>
  'as much as possible, you should stay in a specific room and away from other people in your home. Also, you should use a separate bathroom, if available'.


go_to_the_bathroom ==>>
  use_the_toilet,
  wash_hands.

eating ==>>
  wash_hands,
  eat.
