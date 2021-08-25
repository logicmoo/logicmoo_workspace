%%----------------------------------------------------------------------------
%%  BACK 5.0        Operator Definitions
%%----------------------------------------------------------------------------

%%----------------------------------------------------------------------------
% Operators of the BACK V Syntax. The weights are relative to the weight of
% operator ':' (fills), which is a quintus built-in with weight 600.
%%----------------------------------------------------------------------------


:- op(900, fx,  forget).
:- op(900, fx,  redescribe).
:- op(800, xf,  noibox).
:- op(700, xfy, ?<).
:- op(700, xfy, ?:).
:- op(700, xfy, :=).
:- op(700, xfy, :<).
:- op(700, xfy, ::).
:- op(700, xfy, =>).
:- op(700, xfy, *=).
:- op(650, xfy, type).
:- op(640, fx,  describe).          
:- op(640, fx,  describe_fully). 
:- op(640, fx,  defined_as).       
:- op(640, fx,  introduced_as).       
:- op(640, fx,  self).       
:- op(640, fx,  msc).       
:- op(635, xfy, for).              
:- op(630, fx,  getall).            
:- op(630, xfy, union).
:- op(625, xfy, '.').
:- op(625, xfy, comp).
:- op(625, xfy, intersection).
:- op(620, xfy, or).
:- op(610, xfy, without).
:- op(610, xfy, and).
:- op(575, xfy, '..').  % -okp-
:- op(606, xfy, '*').
