
transfer_rule([[action, switch], [onoff, on]],
	      [[action, allumer]]).

transfer_rule([[action, switch], [onoff, off]],
	      [[action, éteindre]]).

transfer_lexicon([utterance_type, command], [utterance_type, command]).
transfer_lexicon([utterance_type, query], [utterance_type, query]).
transfer_lexicon([state, be], [state, être]).
transfer_lexicon([action, dim], [action, baisser]).
transfer_lexicon([device, light], [device, lampe]).
transfer_lexicon([device, fan], [device, ventilateur]).
transfer_lexicon([location, kitchen], [location, cuisine]).
transfer_lexicon([location, living_room], [location, salon]).
transfer_lexicon([onoff, on], [onoff, allumé]).
transfer_lexicon([onoff, off], [onoff, éteint]).



