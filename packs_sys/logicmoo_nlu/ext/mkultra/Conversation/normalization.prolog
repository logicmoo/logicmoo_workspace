normalize_dialog_act(Act, Normalized) :-
   da_normal_form(Act, Normalized) ->
      true
      ;
      (Normalized=Act).

% Indirect request - "can you hand me that screwdriver?"
da_normal_form(question(Speaker, Addressee, can(X), present, simple),
	       command(Speaker, Addressee, X)).
