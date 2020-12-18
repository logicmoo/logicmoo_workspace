

person_leave_house(Person) ==>>
  isa(Person,person),
  (   has_epidemic(Illness,Epidemic) ->
      (
       (   sick_with_illness(Person,Illness) ;
     exhibiting_any_symptoms_of_illness(Person,Illness)) ->
       (   abort(leave_house(Person)),
     add_todo( Person,stay_home(Person))) ;
       true
      )).

has_symptom(Illness,Symptom) ==>>
  has_symptoms(Illness,Symptoms),
  member(Symptom,Symptoms).

has_symptoms(covid19,[fever,cough,difficulty(breathing)]).

exhibiting_any_symptoms_of_illness(Person,Illness) ==>>
  has_symptom(Illness,Symptom),
  exhibiting_symptom_of_illness(Person,Symptom).
  
sick_with_illness(Person,Illness) ==>>
  true.

party_leave_house(Party) ==>>
  isa(Party,group),
  forall(member(Person,Party),person_leave_house(Person)),
  someone_turn_down_the_heat(Party),
  one_or_more_people_bring_payment_options(Party).

someone_turn_down_the_heat(Party) ==>>
  member(Person,Party),
  turn_down_heat(Person,House).

one_or_more_people_bring_payment_options(Party) ==>>
  (   member(Person,Party),
      hasAccessToBankAccount(Person,Account),
      isa(Account,bankAccount)),
  (   
      check_payment_option_balances(Person,Account,Balance),
      bring_payment_options(Person,Account)).

start_drive_car(Person,Car) ==>>
  check_tire_pressure(Car),
  get_in_car(Person,Car),
  ensure_have_enough_fuel_remaining(Car),
  put_on_seat_belt(Person,Car),
  (   is_nighttime -> turn_on_lights(Car) ; true).
