
%:- consult( '../aleph_dlsettings.pl' ).

:- declare_concept( train ),
	declare_concept( car ),
	declare_concept( closed_car ),
	declare_concept( open_car ),
	declare_concept( triangle_load ),
	declare_concept( square_load ).


:- add_to_concept( train,      [ (t1,0.88), (t2,0.9), (t3,0.8), (t4,1.0), (t5, 0.95) ] ).
:- add_to_concept( car,        [ (c11, 0.95), (c12, 0.95), (c13, 0.95), (c14, 0.95), (c21, 0.95), (c22, 0.95), (c23, 0.95), (c31, 0.95), (c32, 0.95), (c51, 0.95),   (c41, 0.95), (c42, 0.95), (c52, 0.95)] ).
:- add_to_concept( closed_car, [ (c11, 0.7), (c12, 0.9), (c14, 0.94), (c21, 0.95), (c22, 0.95), (c31, 0.9), (c32, 0.95), (c51, 0.94) ] ).
:- add_to_concept( open_car,   [ (c13, 0.95), (c23, 0.9), (c41, 0.95), (c42, 0.88), (c52, 0.91) ] ).
:- add_to_concept( triangle_load, [ (l121,0.87), (l131,0.9), (l141, 0.8), (l211, 0.5), (l411, 0.79), (l511,0.87), (l521,0.87) ] ).
:- add_to_concept( square_load,   [ (l111,0.87), (l142,0.87), (l221,0.9), (l211, 0.5), (l231,0.87), (l311,0.87), (l321,0.87), (l421,0.87), (l422,0.87), (l522,0.87) ] ).


:-      declare_relation( has_car,   train, car),
	declare_relation( has_load,  car, thing).

:-      add_to_relation( has_car,  t1,  [(c11,0.9), (c12,0.9), (c13,0.9), (c14,0.9) ]),
	add_to_relation( has_load, c11, [(l111, 0.88)] ),
	add_to_relation( has_load, c12, [(l121, 0.88)] ),
	add_to_relation( has_load, c13, [(l131, 0.88)] ),
	add_to_relation( has_load, c14, [(l141, 0.88), (l142, 0.9)] ).

:-      add_to_relation( has_car,  t2,  [(c21,0.8), (c22,1.0), (c23,1.0) ] ),
	add_to_relation( has_load, c21, [(l211, 0.5)] ),
	add_to_relation( has_load, c22, [(l221, 1.0)] ),
	add_to_relation( has_load, c23, [(l231, 0.88)] ).

:-      add_to_relation( has_car,  t3,  [(c31,0.9), (c32, 0.9)] ),
	add_to_relation( has_load, c31, [(l311, 0.88)] ),
	add_to_relation( has_load, c32, [(l321, 0.88)] ).

:-      add_to_relation( has_car,  t4,  [(c41,0.9),(c42,0.88)] ),
	add_to_relation( has_load, c41, [(l411,1.0)] ),
	add_to_relation( has_load, c42, [(l421,1.0),(l422,1.0)] ).

:-      add_to_relation( has_car,  t5,  [(c51,0.9),(c52,0.88)] ),
	add_to_relation( has_load, c51, [(l511,1.0)] ),
	add_to_relation( has_load, c52, [(l521,1.0),(l522,1.0)] ).

