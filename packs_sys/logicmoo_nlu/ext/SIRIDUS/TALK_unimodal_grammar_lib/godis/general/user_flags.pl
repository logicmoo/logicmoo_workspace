/*************************************************************************

         name: user_flags.pl 
      version: Nov 26, 1999
  description: Flags for GoDiS
       author: Johan Bos, Peter Bohlin, Staffan Larsson
 
*************************************************************************/

:- multifile flagValue/2, flagInfo/3.
:- dynamic flagValue/2, flagInfo/3.


/*========================================================================
   User flags: default values.
========================================================================*/

flagValue( output_prompt, '$S> ' ).
flagValue( input_prompt, '$U> ' ).
flagValue( language, english ).
flagValue( domain, travel ).
flagValue( visualize_devices, no ).

/*========================================================================
   Flags: declarations.
========================================================================*/

flagInfo( output_prompt, ['$S> ','$U> '], 'The output prompt' ).
flagInfo( input_prompt, ['$S> ','$U> '], 'The input prompt' ).
flagInfo( language, [ english, svenska], 'Language' ).
flagInfo( domain, [ travel, vcr, telephone, telvcr, telvcrlogin, medical , tvgodis,agenda,player,X], 'Domain' ).
flagInfo( visualize_devices, [ yes, no ], 'Whether to simulate VCR, telephone and house' ).

