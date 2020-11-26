
transfer_rule([[action, switch], [prep, on]],
	      [[action, switch_on]]).

transfer_rule([[action, switch], [prep, off]],
	      [[action, switch_off]]).

transfer_rule([[tense, imperative]], []).
transfer_rule([[tense, present]], []).
transfer_rule([[spec, the_sing]], []).
transfer_rule([[pronoun, you]], []).
transfer_rule([[prep, in_loc]], []).
transfer_rule([[voice, active]], []).

transfer_lexicon([utterance_type, imp], [utterance_type, command]).
transfer_lexicon([utterance_type, ynq], [utterance_type, query]).
transfer_lexicon([verb, be], [state, be]).
transfer_lexicon([action, dim], [action, dim]).
transfer_lexicon([device, light], [device, light]).
transfer_lexicon([device, fan], [device, fan]).
transfer_lexicon([location, kitchen], [location, kitchen]).
transfer_lexicon([location, living_room], [location, living_room]).
transfer_lexicon([prep, on], [onoff, on]).
transfer_lexicon([prep, off], [onoff, off]).
transfer_lexicon([adj, on], [onoff, on]).
transfer_lexicon([adj, off], [onoff, off]).
