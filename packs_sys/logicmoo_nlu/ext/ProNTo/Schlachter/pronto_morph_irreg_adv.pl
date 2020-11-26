/****************************************************
% File Name: pronto_morph_irreg_adv.pl
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

irregular_form( best,X,[well,-est| X ]).
irregular_form( better,X,[well,-er| X ]).
irregular_form( deeper,X,[deeply,-er| X ]).
irregular_form( farther,X,[far,-er| X ]).
irregular_form( further,X,[far,-er| X ]).
