/*

Alterf patterns for surface strings

*/

% "*number*"
alterf_surface_pattern([number(N)], N, 'switch on two lights').

% switch
alterf_surface_pattern([switch], switch, 'switch on the light').

% is
alterf_surface_pattern([is/are], is, 'is the light switched on').

% dim
alterf_surface_pattern([dim], dim, 'dim the light').

% light
alterf_surface_pattern([light/lights], light, 'switch on the light').

% fan
alterf_surface_pattern([fan/fans], fan, 'switch on the light').

% kitchen
alterf_surface_pattern([kitchen], kitchen, 'switch on the light in the kitchen').

% living_room
alterf_surface_pattern([living, room], living_room, 'switch on the light in the living room').

% on
alterf_surface_pattern([on], on, 'is the light switched on').

% off
alterf_surface_pattern([off], off, 'is the light switched off').
