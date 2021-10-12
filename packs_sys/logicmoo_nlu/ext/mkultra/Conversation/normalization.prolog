normalize_dialog_act(Act, Normalized) :-
   dialog_act_normal_form(Act, Normalized) -> true ; (Normalized=Act).