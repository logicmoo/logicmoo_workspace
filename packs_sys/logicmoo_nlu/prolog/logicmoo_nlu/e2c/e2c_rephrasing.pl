
idiomatic_replace([why,did,np(X),Y],[what,situation,caused,np(X),to,Y]).
% why did X ... 
% % -> what situation caused X to ...

txt_rewrite([IT,'''','s'],[ITS]):- atom(IT),atom_concat(IT,'s',ITS).
txt_rewrite([wont],[will,not]).

do_txt_rewrites([],[]):-!.
do_txt_rewrites([WordA|WordsI],WordsO):- downcase_atom(WordA,WordD), txt_rewrite([WordD|Left],Into), append(Left,Mid,WordsI),!,
   append(Into,Mid,WordsM),
   do_txt_rewrites(WordsM,WordsO).
do_txt_rewrites([Word|Words],[Word|WordsO]):- !, do_txt_rewrites(Words,WordsO).

