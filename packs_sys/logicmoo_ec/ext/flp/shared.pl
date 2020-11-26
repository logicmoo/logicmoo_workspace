:- use_module(library(regex)).

duration(Duration)              --> oneOrMore(token,Tokens),
	{
	 atomic_list_concat(Tokens,'',TmpTimeUnits),
	 atomic_list_concat([Date,Time],'_',TmpTimeUnits),
	 atomic_list_concat([AY,AM,AD],'-',Date),
	 atomic_list_concat([AH,AMi,AS],':',Time),
	 atom_number(AY,Y),atom_number(AM,M),atom_number(AD,D),atom_number(AH,H),atom_number(AMi,Mi),atom_number(AS,S),
	 Duration = [Y-M-D,H:Mi:S]
	}.

dateTimeZone(DateTimeZone)      --> oneOrMore(token,Tokens),
	%% {atomic_list_concat(Tokens,'',DateTimeZone)}.
	{
	 Tokens = [TimeZoneSpec,:,DateTimeSpec],
	 atomic_list_concat([_,TimeZone],'=',TimeZoneSpec),
	 atomic_list_concat([Date,Time],'T',DateTimeSpec),
	 regex_atom("^([0-9]+)([0-9][0-9])([0-9][0-9])$",[],Date,[AD,AM,AY]),
	 regex_atom("^([0-9][0-9])([0-9][0-9])([0-9][0-9])$",[],Time,[AS,AMi,AH]),
	 atom_codes(BY,AY),atom_codes(BM,AM),atom_codes(BD,AD),atom_codes(BH,AH),atom_codes(BMi,AMi),atom_codes(BS,AS),	
	 atom_number(BY,Y),atom_number(BM,M),atom_number(BD,D),atom_number(BH,H),atom_number(BMi,Mi),atom_number(BS,S),
	 DateTimeZone = [[Y-M-D,H:Mi:S],TimeZone]
	}.
