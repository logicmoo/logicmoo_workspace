
% Disprefer PP containing a postmodified gap (not even clear if this is correct!)
parse_preference_score(tree_includes_structure=(pp<[p<_, np<[np<null, post_mods<_]]), -1).

% Disprefer NPs consisting of a bare D
parse_preference_score(tree_includes_structure=(np<[d<_]), -1).

% Disprefer attaching relatives to verbs
parse_preference_score(lf_includes_structure=form(_, [_, [lambda, _, _]]), -1).
parse_preference_score(lf_includes_structure=form(_, [_, _, [lambda, _, _]]), -1).
parse_preference_score(lf_includes_structure=form(_, [_, _, _, [lambda, _, _]]), -1).

% Disprefer NPs with two date modifiers
parse_preference_score(lf_includes_structure=[[_,date(_,_,_)], [_,date(_,_,_)] | _], -1).

% Disprefer bare year modifiers
parse_preference_score(lf_includes_structure=[date, date(_, unspecified, unspecified)], -1).

% Disprefer names used as possessives, since they occur as components in some names
parse_preference_score(lf_includes_structure=[possessive, term(name,_,_)], -1).

% Disprefer 'time/day' with mod 
parse_preference_score(lf_includes_structure=term(_, time, [_|_]), -1).

% Disprefer 'what' with mod 
parse_preference_score(lf_includes_structure=term(what, _, [_|_]), -1).
