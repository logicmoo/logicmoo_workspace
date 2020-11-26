/*************************************************************************

  name: start-medical-text.pl
  description: starter
 
*************************************************************************/

% Make sure you have set the GODIS and TRINDIKIT environment variables
:-ensure_loaded('$GODIS/prolog/godis/general/search_paths').
% Load starter routines; don't change
:- use_module( library(starter) ).

%%% APPLY CHANGES BELOW %%%

% Select appropriate godis library/libraries
:- assertz(user:library_directory(godis('godis-aod'))).


% Set the application home path
:- assertz(user:file_search_path(home,'c:/CVS/godis/godis-apps/domain-medical')).
:- assertz(user:library_directory(home(''))).

% add any subdirectories to the application home directory below
:- assertz(user:library_directory(home('Database'))).

% GODIS spec filename here
:- init( 'godis-medical-text' ).

