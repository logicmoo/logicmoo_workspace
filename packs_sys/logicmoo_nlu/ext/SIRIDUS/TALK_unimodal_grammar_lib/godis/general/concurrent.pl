/*************************************************************************

         name: concurrent.pol
      version: August 30, 2000
  description: Concurrency settings
       author: Alexander Berman
 
*************************************************************************/


/*-------------------------------------------------------------------------------------------

agent_protocol( +Protocol )

Specify the choice of intra-TrindiKit agent communication protocol. Protocol is either oaa
(Open Agent Architecture) or ae (Agent Environment).

-------------------------------------------------------------------------------------------*/

agent_protocol( ae ).


/*-------------------------------------------------------------------------------------------

Appearance settings for agent windows

columns( +NumberOfWindowColumns )          - Number of window columns on screen
window_dimension_pixels( +Width, +Height ) - Width and height of each window (in pixels)
window_dimension_chars( +Width, +Height )  - Width and height of each window (in characters)

-------------------------------------------------------------------------------------------*/
/*
% Large screen
columns( 3 ).
window_dimension_pixels( 412, 290 ).
window_dimension_chars( 65, 20 ).
*/
%/*
% 1024x768
columns( 3 ).
window_dimension_pixels( 345, 250 ).
window_dimension_chars( 54, 17 ).
%*/

/*
% 800x600
columns( 2 ).
window_dimension_pixels( 400, 160 ).
window_dimension_chars( 63, 10 ).
*/


/*-------------------------------------------------------------------------------------------

display_type( ?Agent, +Type )

Specify whether a certain agent should run in a separate window
(Type=window) or in the background (Type=background). Agent
is either tis, console, spy, print, resource or control.

-------------------------------------------------------------------------------------------*/

display_type( _, window ).

/*
display_type( input, window ) :- !.
display_type( console, window ) :- !.
display_type( output, window ) :- !.
display_type( _, background ).
*/


/*-------------------------------------------------------------------------------------------

resource_mode( ?Resource, +Mode )

Specify whether a certain resource should be loaded into the TIS (Mode=tis), loaded into each
module that uses it (Mode=modules) or run as an agent of its own (Mode=standalone).

-------------------------------------------------------------------------------------------*/

resource_mode( _, tis ).


/*-------------------------------------------------------------------------------------------

lang( Agent, Language )

Specify the programming language of an agent. Language is either sicstus (SICStus Prolog)
or java.

-------------------------------------------------------------------------------------------*/

%DH: use sicstus instead as default
%lang( _, sicstus('/usr/lic/sicstus3p8/bin/sicstus') ).
lang( _, sicstus('sicstus') ).


/*-------------------------------------------------------------------------------------------

show_com( Agent, YesOrNo )

Specify whether communication to and from agents should be output. Agent
is either tis, console, spy, print, resource, control or starter.

-------------------------------------------------------------------------------------------*/

%show_com( spy, no ) :- !.
%show_com( print, no ) :- !.
%show_com( _, yes ) :- !.

show_com( tis, no ).
show_com( console, no ).
show_com( spy, no ).
show_com( control, no ).
show_com( print, no ).
show_com( starter, no ).
show_com( resource, no ).
