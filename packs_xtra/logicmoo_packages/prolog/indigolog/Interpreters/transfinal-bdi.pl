%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% FILE    : Interpreters/transfinal-bdi.pl
%
%       IndiGolog TRANS & FINAL Implementation for BDI constructs.
%
%  AUTHOR : Yves Lesperance (July 2010) 
%  EMAIL  : 
%  WWW    : www.cs.toronto.edu/cogrobo
%  TYPE   : system independent code
%  TESTED : SWI Prolog 5.0.10 http://www.swi-prolog.org
%
%           For more information on Golog and some of its variants, see:
%               http://www.cs.toronto.edu/~cogrobo/
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file provides:
%
% -- trans(P,H,P2,H2)    : configuration (P,H) can perform a single step
%                          to configuration (P2,H2)
% -- final(P,H)          : configuration (P,H) is terminating
%
%  The following special features are also provided:
% 
%
%
%  The following is required for this file:
%
% FROM SYSTEM CODE DEPENDING ON WHERE IT IS USED
% -- report_message(T, M) : report message M of type T
%
% FROM TEMPORAL PROJECTOR:
% -- isTrue(+C, +H) 
%           Conditio C is true at history H
% -- calc_arg(+A, -A2, +H) 
%           calculate the arguments of action A at history H
% -- domain(-V, +D)       
% -- rdomain(-V, +D)       
%           object V is an element of domain D (random)
% -- getdomain(+D, -L) 
%           L is the list of elements in domain D
% -- sensed(+A, ?V, ?H) 
%           action A got sensing result V w.r.t. history H
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


trans(achieve(Goal),H,E1,H1) :- 
	rule(Goal,Guard,Body), trans(if(Guard,Body,?(false)), H, E1, H1).

final(achieve(Goal),H) :- 
	rule(Goal,Guard,Body), final(if(Guard,Body,?(false)), H).

	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EOF: Interpreters/transfinal-bdi.pl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%