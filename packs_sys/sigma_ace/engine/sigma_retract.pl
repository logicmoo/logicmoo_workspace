%:-include('sigma_header.pl').

% =====================================================================================
%  retract(Retract_chars,Ctx,TN,KB,User)
% =====================================================================================

bp_retract(Retraction_Chars,KB,Ctx,TN,L,User):-
      (((idGen(TN),TrackingAtom=(TN)) ; TrackingAtom=TN)),!,
      setSigmaOption(OptionList),
      write_response_begin,!, %%trace,
      tell_retract_parse_chars(Retraction_Chars,Formula,Vars),
      %%ignore(once(retract(Retraction_Chars,_Ctx,TrackingAtom,KB,User))),
      retract_odbc(Formula,Ctx,TrackingAtom,KB,Vars,User),
      write_response_end.


     /*
retract(Retract_chars,Cxt):-!,
         retract(Retract_chars,Ctx,TN,KB,'Author').
         
retract(Retract_chars,Ctx,TN):- 
         retract(Retract_chars,Ctx,TN,KB,'Author').

retract(Retract_chars,Ctx,TN,KB,User):-!,   
            once(tell_retract_parse_chars(Retract_chars,Pterm,Vars)),
            sigma_invoke(retract,forall,surface,Pterm,Ctx,TN,KB,Vars,User).
       */


