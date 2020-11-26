/*

Alterf patterns for LFs

*/

% "*number*"
alterf_pattern([spec, N], N, 'switch on two lights') :- number(N).

% switch
alterf_pattern([action, switch], switch, 'switch on the light').

% is
alterf_pattern([utterance_type,ynq], is, 'is the light switched on').

% dim
alterf_pattern([action, dim], dim, 'dim the light').

% light
alterf_pattern([device, light], light, 'switch on the light').

% fan
alterf_pattern([device, fan], fan, 'switch on the light').

% kitchen
alterf_pattern([location, kitchen], kitchen, 'switch on the light in the kitchen').

% living_room
alterf_pattern([location, living_room], living_room, 'switch on the light in the living room').

% on
alterf_pattern([adj, on], on, 'is the light switched on').
alterf_pattern([prep, on], on, 'switch on the light').

% off
alterf_pattern([adj, off], off, 'is the light switched off').
alterf_pattern([prep, off], off, 'switch off the light').
