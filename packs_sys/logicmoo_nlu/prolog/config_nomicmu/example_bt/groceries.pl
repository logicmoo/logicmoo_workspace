

buy_groceries(Person) ==>>
  ensure_you_are_replete(Person),
  resides_at(Person,House),
  located_at(KitchenTable,House),
  isa(KitchenTable,kitchenTable),
  ensure_kitchen_table_is_cleared(Person,Kitchentable),
  (   
      buy_groceries_at(Person,nearestFn(aldi)) ;
      buy_groceries_at(Person,nearestFn(walmart))
  ).

buy_groceries_at(Person,AldiStore) ==>>
  storeIsPartOfChain(AldiStore,aldi),
        ensure(possesses(Person,Quarter)),
        isa(Quarter,quarter),
        ensure(possesses(Person,Bags)),
        isa(Bags,shoppingBags),
  go_to(Person,AldiStore).

go_to_farmers_market(Party) ==>>
  member(Person,Party),
  call_farmers_market_to_be_sure_double_up_bucks_are_active(Person),!.

return_home_from_walmart(Party) ==>>
  member(Person,Party),
  carry_from_to(Person,Car,House),
  foreach(member(Person,Party),
    wash_hands(Person)),
  change_5_gallon_jug(Person).


buy_groceries ==>>
  ensure_you_are_replete,
  ensure_kitchen_table_is_cleared,
  (
   buy_groceries_at_aldi ;
   buy_groceries_at_walmart
  ).

buy_groceries_at_aldi ==>>
  ensure_have_quarter,
  ensure_have_aldi_bags,
  go_to_aldi.
