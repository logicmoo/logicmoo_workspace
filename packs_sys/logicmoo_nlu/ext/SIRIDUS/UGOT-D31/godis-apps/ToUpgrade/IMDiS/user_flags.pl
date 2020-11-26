/*************************************************************************

         name: user_flags.pl 
      version: Nov 26, 1999
  description: Flags for InDiTeS 
       author: Staffan Larsson
 
*************************************************************************/

:- multifile flagValue/2, flagInfo/3.
:- dynamic flagValue/2, flagInfo/3.


/*========================================================================
   User flags: default values.
========================================================================*/

flagValue( output_prompt, '$S> ' ).
flagValue( input_prompt, '$U> ' ).
flagValue( language, english ).
flagValue( domain, homecentre ).


/*========================================================================
   Flags: declarations.
========================================================================*/

flagInfo( output_prompt, ['$S> ','$U> '], 'The output prompt' ).
flagInfo( input_prompt, ['$S> ','$U> '], 'The input prompt' ).
flagInfo( language, [ english], 'Language' ).
flagInfo( domain, [ homecentre ], 'Domain' ).


