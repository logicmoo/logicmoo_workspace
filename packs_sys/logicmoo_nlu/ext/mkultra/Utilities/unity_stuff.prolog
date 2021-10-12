%%%
%%% Code for talking to other parts of the C# code
%%%

%%
%% Sound interface
%%

emit_grain(Name, Duration) :-
   $this \= $me,
   $this.'EmitGrain'(Name, Duration),
   !.
emit_grain(_,_).

