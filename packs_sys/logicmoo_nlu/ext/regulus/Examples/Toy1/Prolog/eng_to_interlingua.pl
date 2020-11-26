
transfer_rule([[action, switch], [onoff, on]],
	      [[action, switch_on]]).

transfer_rule([[action, switch], [onoff, off]],
	      [[action, switch_off]]).

transfer_lexicon([utterance_type, command], [utterance_type, command]).
transfer_lexicon([utterance_type, query], [utterance_type, query]).
transfer_lexicon([state, be], [state, be]).
transfer_lexicon([action, dim], [action, dim]).
transfer_lexicon([device, light], [device, light]).
transfer_lexicon([device, fan], [device, fan]).
transfer_lexicon([location, kitchen], [location, kitchen]).
transfer_lexicon([location, living_room], [location, living_room]).
transfer_lexicon([onoff, on], [onoff, on]).
transfer_lexicon([onoff, off], [onoff, off]).
