
% Disprefer PP containing a postmodified gap (not even clear if this is correct!)
parse_preference_score(tree_includes_structure=(pp<[p<_, np<[np<null, post_mods<_]]), -1).

% Disprefer NPs consisting of a bare D
parse_preference_score(tree_includes_structure=(np<[d<_]), -1).
parse_preference_score(lf_includes_structure=term(1, null, []), -1).
parse_preference_score(lf_includes_structure=term(2, null, []), -1).
parse_preference_score(lf_includes_structure=term(3, null, []), -1).
parse_preference_score(lf_includes_structure=term(4, null, []), -1).
parse_preference_score(lf_includes_structure=term(5, null, []), -1).

% Disprefer relatives in general
parse_preference_score(lf_includes_structure=[lambda, _, _], -1).

% Disprefer attaching relatives to verbs
parse_preference_score(lf_includes_structure=form(_, [_, [lambda, _, _]]), -1).
parse_preference_score(lf_includes_structure=form(_, [_, _, [lambda, _, _]]), -1).
parse_preference_score(lf_includes_structure=form(_, [_, _, _, [lambda, _, _]]), -1).

% Prefer adding mods to 'put', 'place' etc
parse_preference_score(lf_includes_structure=[[put, _Subj, _Obj]], -1).
parse_preference_score(lf_includes_structure=[[place, _Subj, _Obj]], -1).
parse_preference_score(lf_includes_structure=[[set, _Subj, _Obj]], -1).
parse_preference_score(lf_includes_structure=[[put, _Subj, _Obj], _Mod1], 0).
parse_preference_score(lf_includes_structure=[[place, _Subj, _Obj], _Mod1], 0).
parse_preference_score(lf_includes_structure=[[set, _Subj, _Obj], _Mod1], 0).
parse_preference_score(lf_includes_structure=[[put, _Subj, _Obj], _Mod1, _Mod2], 1).
parse_preference_score(lf_includes_structure=[[place, _Subj, _Obj], _Mod1, _Mod2], 1).
parse_preference_score(lf_includes_structure=[[set, _Subj, _Obj], _Mod1, _Mod2], 1).
parse_preference_score(lf_includes_structure=[[put, _Subj, _Obj], _Mod1, _Mod2, _Mod3], 2).
parse_preference_score(lf_includes_structure=[[place, _Subj, _Obj], _Mod1, _Mod2, _Mod3], 2).
parse_preference_score(lf_includes_structure=[[set, _Subj, _Obj], _Mod1, _Mod2, _Mod3], 2).

% Disprefer 'what' with mod 
parse_preference_score(lf_includes_structure=term(what, _, [_|_]), -1).
