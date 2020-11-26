
declare_a_sick_day ==>>
  declare_an_off_off_day,
  prevent_going_to_gym,
  treat_sickness.

treat_sickness ==>>
     treat_cold ; treat_flu.

treat_cold ==>>
  has_cold,
  drink_emergen_c_packet,
  schedule_meal_chicken_soup,
  schedule_nap.

treat_flu ==>>
  true.

must_cough_or_sneeze ==>>
  covert_cough_or_sneeze_with_with_a_tissue,
  wash_hands.

%%% source: 30 seconds: ?
%%% source: 70 percent: Amanda
wash_hands(Person) ==>>
  has_household(Person,Household),
  (   if_hands_are_visibly_dirty(Person) ->
      wash_hands_with_soap_and_water_for_at_least_30_seconds(Person) ;
      (  if_soap_and_water_are_readily_available(Household) ->
    wash_hands_with_soap_and_water_for_at_least_30_seconds(Person) ;
    wash_hands_with_a_hand_sanitizer_that_is_at_least_70_percent_alcohol(Person))).

wash_hands_with_a_hand_sanitizer_that_is_at_least_70_percent_alcohol(Person) ==>>
  wash_hands_with_hand_sanitzer_covering_all_surfaces_of_your_hands_and_rubbing_them_together_until_they_feel_dry(Person).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
has_symptoms(flu,[fever, feeling_feverish_andor_chills, cough, sore_throat, runny_or_stuffy_nose, muscle_or_body_aches, headaches, fatigue__tiredness]).
has_symptoms(flu,[vomiting_and_diarrhea]).

%%% Flu Symptoms
has_symptoms(fever,[headache_warm_forehead_chills_aching_muscles_general_feeling_of_weakness_sore_eyes_loss_of_appetite_dehydration_swollen_lymph_nodes]).
has_symptoms(fever,[greater_irritability_than_usual, lethargy, flushed_skin, paleness, difficulty_swallowing, refusal_to_eat__drink__or_breastfeed]).
has_symptoms(fever,[excessive_sleepiness_confusion_convulsions_severe_pain_in_other_parts_of_the_body_unusual_vaginal_discharge_pain_during_urination_skin_rash_vomiting_diarrhea]).
 
