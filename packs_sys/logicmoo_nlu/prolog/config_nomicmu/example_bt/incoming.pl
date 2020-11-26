%%% incoming

go_upstairs ==>>
  % WHILE go_upstairs IF hands_are_free, THEN bring_up_dishes
  (   hands_are_free -> bring_up_dishes).

go_downstairs ==>>
  (   hands_are_free -> bring_down_towels).
