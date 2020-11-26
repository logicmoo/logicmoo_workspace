/*************************************************************************

         name: comsemOperators.pl
      version: May 25, 1999
  description: Operator definitions
      authors: Patrick Blackburn & Johan Bos
 
*************************************************************************/

:-module(comsemOperators,[]).
/*========================================================================
   Operator Definitions
========================================================================*/

:- op(950,yfx,user:(@)).         % application
:- op(900,yfx,user:('<>')).      % bin impl
:- op(900,yfx,user:(>)).         % implication
:- op(850,yfx,user:(v)).         % disjunction
:- op(800,yfx,user:(&)).         % conjunction
:- op(750, fy,user:(~)).         % negation
