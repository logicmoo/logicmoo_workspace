% orthography.pl

% Ordered list of defaults, strongest first.

letter_class('V', "aeiouéy").
letter_class('C', "bcdfghjklmnpqrstvwxz").

orthography_rewrite(" de les ", " des ").
orthography_rewrite(" de le ", " du ").
orthography_rewrite(" à le ", " au ").
orthography_rewrite(" à les ", " aux ").
orthography_rewrite(" de un ", " d'un ").
orthography_rewrite(" de une ", " d'une ").

orthography_rewrite(" ton V1", " ta V1").

orthography_rewrite(" je V1", " j'V1").
orthography_rewrite(" se V1", " s'V1").
orthography_rewrite(" te V1", " t'V1").
orthography_rewrite(" me V1", " m'V1").
orthography_rewrite(" ne V1", " n'V1").
orthography_rewrite(" que V1", " qu'V1").

orthography_rewrite(" Je V1", " J'V1").
orthography_rewrite(" Se V1", " S'V1").
orthography_rewrite(" Te V1", " T'V1").
orthography_rewrite(" Me V1", " M'V1").
orthography_rewrite(" Ne V1", " N'V1").
orthography_rewrite(" Que V1", " Qu'V1").

orthography_rewrite(" j'C1", " je C1").
orthography_rewrite(" s'C1", " se C1").
orthography_rewrite(" t'C1", " te C1").
orthography_rewrite(" m'C1", " me C1").
%orthography_rewrite(" n'C1", " ne C1").
orthography_rewrite(" qu'C1", " que C1").

orthography_rewrite(" J'C1", " Je C1").
orthography_rewrite(" S'C1", " Se C1").
orthography_rewrite(" T'C1", " Te C1").
orthography_rewrite(" M'C1", " Me C1").
%orthography_rewrite(" N'C1", " Ne C1").
orthography_rewrite(" Qu'C1", " Que C1").

		
