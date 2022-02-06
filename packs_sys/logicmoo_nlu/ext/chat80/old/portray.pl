:- op(100,fx,?).

portray(#(PP,Pred,Trace,Adv)) :-
   portray_bit(pp,PP,S0,S1),
   portray_bit(pred,Pred,S1,S2),
   portray_bit(trace,Trace,S2,S3),
   portray_bit(adv,Adv,S3,[]),
   write(S0).

portray_bit(Bit,Value,[?Bit|Bits],Bits) :- var(Value), !.
portray_bit(Bit,1,[Bit|Bits],Bits).
portray_bit(Bit,0,Bits,Bits).
