/*************************************************************************

         name: infoamount.pl 
      version: 8 December, 2005
  description: Type definition for type infoamount
       author: Peter Bohlin, Staffan Larsson, Johan Bos, Stina Ericsson
 
*************************************************************************/

:- multifile is_type/1, of_type/2, empty_object/2, operation/4, relation/2, function/3, selector/3.
:- discontiguous is_type/1, of_type/2, empty_object/2, operation/4, relation/2, function/3, selector/3.


:- use_module( library(lists),
	       [ member/2, select/3, append/3, remove_duplicates/2  ] ).



%% A type for specifying the degree to which a message is to be, or was,
%% realised in some input or output modality
is_type( infoamount ).

of_type( no, infoamount ).     % no material at all
of_type( min, infoamount ).    % minimal = only focus
of_type( interm, infoamount ). % intermediate = focus and partial ground
of_type( max, infoamount ).    % maximal = focus and full ground
of_type( ground, infoamount ). % only ground
of_type( compl, infoamount ).  % complementary = whatever is not realised
                               %   in other modality/ies,
                               %   e.g. if output modality 1 is set to
                               %        'interm', and output modality 2 to
                               %        'compl', then output modality 2 will
                               %        realise any ground material not
                               %        realised by modality 1
of_type( indet, infoamount ).  % indeterminate (unknown) value
