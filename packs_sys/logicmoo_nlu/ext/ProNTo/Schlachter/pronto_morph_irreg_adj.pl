/****************************************************
% File Name: pronto_morph_irreg_adj.pl
% Author: Jason Schlachter (ai@uga.edu)(www.arches.uga.edu/~ai)

% Released: May 8th, 2003
% Artificial Intelligence Center (www.ai.uga.edu)
% ***see pronto_morph.pdf for documentation
%
% This file is to be used as part of the morphological analyzer
% that is a component of ProNTo (Prolog Natural Language
% Toolkit), made at the Artificial Intelligence Center of
% the University of Georgia (www.ai.uga.edu).
%
% Special thanks:
% 1)To the wordnet project for their list words that are exceptions to the spelling rules.
% 2)To Dr. Covington for his guidence on the project.
******************************************************/

:-multifile( irregular_form/3 ).

% irregular_form(+Atom,-Tail,-List)
%  Interprets an irregular form (as an open list)


irregular_form( bluer,X,[blue,-er| X ]).
irregular_form( cagier,X,[cagey,-er| X ]).
irregular_form( cagiest,X,[cagey,-est| X ]).
irregular_form( dicier,X,[dicey,-er| X ]).
irregular_form( diciest,X,[dicey,-est| X ]).
irregular_form( dopier,X,[dopey,-er| X ]).
irregular_form( dopiest,X,[dopey,-est| X ]).
irregular_form( eerier,X,[eerie,-er| X ]).
irregular_form( freer,X,[free,-er| X ]).
irregular_form( gooier,X,[gooey,-er| X ]).
irregular_form( gooiest,X,[gooey,-est| X ]).
irregular_form( homier,X,[homey,-er| X ]).
irregular_form( homiest,X,[homey,-est| X ]).
irregular_form( truer,X,[true,-er| X ]).
