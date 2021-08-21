/** <module> a_nani_household
% This file contains the definitions for the room in a household
% To create a new world, simply change the room definitions as
% described below (or in manual)
%

use this file with...

:- declare_load_dbase('a_nani_household.pfc.pl').

*/

:- style_check(-singleton).
:- style_check(-discontiguous).

:- op(600,fx,onSpawn).

:- expects_dialect(pfc).


