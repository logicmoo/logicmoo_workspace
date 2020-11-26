
transfer_lexicon([utterance_type, command], [utterance_type, command]).
transfer_lexicon([utterance_type, query], [utterance_type, query]).
transfer_lexicon([state, be], [state, vara]).

transfer_lexicon([action, dim], [action, vrid_ner]).

macro(light_dependent_rule(From, LightVersion, NonLightVersion),
      (  transfer_rule(From, LightVersion) :- context([device, light]) )
     ).
macro(light_dependent_rule(From, LightVersion, NonLightVersion),
      (  transfer_rule(From, NonLightVersion) :- \+ context([device, light]) )
     ).

@light_dependent_rule([[action, switch_on]], [[action, tända]], [[action, sätta_på]]).
@light_dependent_rule([[action, switch_off]], [[action, släck]], [[action, stänga_av]]).
@light_dependent_rule([[onoff, on]], [[onoff, tänd]], [[onoff, på]]).
@light_dependent_rule([[onoff, off]], [[onoff, släckt]], [[onoff, av]]).

transfer_lexicon([device, light], [device, lampa]).
transfer_lexicon([device, fan], [device, fläkt]).
transfer_lexicon([location, kitchen], [location, kök]).
transfer_lexicon([location, living_room], [location, vardagsrum]).


