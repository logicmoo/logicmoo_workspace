
rest_dur(N) ==>> {N<0},!.
rest_dur(N) ==>> {N>0, N2 is N-1}, rest_one_second,rest_dur(N2).

rest_one_second ==>> add_todo( $self, wait($self)).
  

do_exercises ==>>
  exercise_upper_body,
  skip_a_day,
  exercise_lower_body,
  skip_a_day,
  exercise_upper_body,
  skip_a_day,
  exercise_lower_body
  .

exercise_upper_body ==>>
  pre_exercise_routine,
  stretch_with_a_roller,
  do_some_brisk_walking_for_1_to_5_mins,
  do_three_compound_exercises_for_upper_body,
  post_exercise_routine
  .

pre_exercise_routine ==>>
  have_decent_meal_of_nutritious_food,
  ensure_not_hungry,
  have_bottle_of_water_or_sugar_free_gatorade
  .

post_exercise_routine ==>>
  wait_between_30_mins_and_two_hours,
  ensure_eat_20_grams_of_protein_and_ensure_40_grams_of_carbohydrates,
  ensure_7_to_9_hours_of_sleep
  .

wait_between_30_mins_and_two_hours ==>> 
  rest_dur(1800);
  rest_dur(3600);
  rest_dur(5400);
  rest_dur(7200).
  

have_decent_meal_of_nutritious_food ==>>
  ensure_eat_20_grams_of_protein_and_ensure_40_grams_of_carbohydrates
  .

ensure_eat_20_grams_of_protein_and_ensure_40_grams_of_carbohydrates ==>>
  eat_bowl_of_oatmeal_and_a_couple_of_scrambled_eggs,
  eat_ham_sandwich,
  eat_gatorade_bar
  .

do_three_compound_exercises_for_upper_body ==>>
  shoulder_press,
  shoulder_row,
  chest_press
  .

shoulder_press ==>>
  do_3_to_5_sets_of_8_to_12_reps
  .

shoulder_row ==>>
  do_3_to_5_sets_of_8_to_12_reps
  .

chest_press ==>>
  do_3_to_5_sets_of_8_to_12_reps
  .

do_3_to_5_sets_of_8_to_12_reps ==>>
  set_of_8_to_12,
  rest_30_to_120_seconds,
  set_of_8_to_12,
  rest_30_to_120_seconds,
  set_of_8_to_12,
  rest_30_to_120_seconds,
  optional_set_of_8_to_12,
  rest_30_to_120_seconds,
  optional_set_of_8_to_12
  .

set_of_8_to_12 ==>>
  do_rep,
  do_rep,
  do_rep,
  do_rep,
  do_rep,
  do_rep,
  do_rep,
  do_rep,
  optional_do_rep,
  optional_do_rep,
  optional_do_rep,
  optional_do_rep
  .
  
rest_30_to_120_seconds ==>>
  rest_dur(30);
  rest_dur(60);
  rest_dur(90);
  rest_dur(120).

exercise_lower_body ==>>
  pre_exercise_routine,
  stretch_with_a_roller,
  do_some_brisk_walking_for_1_to_5_mins,
  do_three_compound_exercises_for_lower_body
  .

do_three_compound_exercises_for_lower_body ==>>
  dead_lifts,
  squats,
  lunges
  .

dead_lifts ==>>
  do_4_to_6_sets_of_4_to_6_reps
  .

squats ==>>
  do_3_to_5_sets_of_8_to_12_reps
  .

lunges ==>>
  do_3_to_5_sets_of_8_to_12_reps
  .

do_4_to_6_sets_of_4_to_6_reps ==>>
  set_of_4_to_6_reps,
  rest_120_seconds,
  set_of_4_to_6_reps,
  rest_120_seconds,
  set_of_4_to_6_reps,
  rest_120_seconds,
  set_of_4_to_6_reps,
  rest_120_seconds,
  optional_set_of_4_to_6_reps,
  rest_120_seconds,
  optional_set_of_4_to_6_reps
  .

rest_120_seconds ==>> rest_dur(120).

set_of_4_to_6_reps ==>>
  do_rep,
  do_rep,
  do_rep,
  do_rep,
  optional_do_rep,
  optional_do_rep
  .
