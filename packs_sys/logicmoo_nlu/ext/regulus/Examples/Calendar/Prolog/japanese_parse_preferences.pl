
% Disprefer a topic reading of "wa" if a subject one is available
parse_preference_score(lf_includes_structure=[topic, _], -1).
parse_preference_score(lf_includes_structure=[subject, _], 1).

% Prefer to attach as much as possible to the subordinate clause
parse_preference_score(lf_includes_structure=[clause, form(_, [_A])], 1).
parse_preference_score(lf_includes_structure=[clause, form(_, [_A, _B])], 2).
parse_preference_score(lf_includes_structure=[clause, form(_, [_A, _B, _C])], 3).
parse_preference_score(lf_includes_structure=[clause, form(_, [_A, _B, _C, _D])], 4).
parse_preference_score(lf_includes_structure=[clause, form(_, [_A, _B, _C, _D, _E])], 5).
parse_preference_score(lf_includes_structure=[clause, form(_, [_A, _B, _C, _D, _E, _F])], 6).

% By default, prefer readings with an NN compound
parse_preference_score(lf_includes_structure=[nn, _], 1).

% By default, prefer readings with continuous_present
parse_preference_score(lf_includes_structure=[tense, continuous_present], 1).

% By default, prefer "oshieu" with just an object and no modifiers
parse_preference_score(lf_includes_structure=[[oshieu], [object, _]], 1).

% By default, prefer "desu" with two subjects
parse_preference_score(lf_includes_structure=[[desu], [subject, _], [subject, _]], 1).

% By default, disprefer readings with "naru" - use lexical entry of form "X naru" instead
parse_preference_score(lf_includes_structure=[path_proc, naru], -1).
