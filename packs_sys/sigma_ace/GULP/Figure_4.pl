% Same as first GULP example (Figure_3), but written in a much more PATR-like style,
% treating the unifications as separate operations.

s(Sfeatures) --> np(NPfeatures), vp(VPfeatures),
	{ Sfeatures = sem<-> (pred<->X .. arg1<->Y .. arg2<->Z),
	  NPfeatures = sem<->Y .. case<->nom,
	  VPfeatures = sem<-> (pred<->X .. arg2<->Z) }.

vp(VPfeatures) --> v(Vfeatures), np(NPfeatures),
	{ VPfeatures = sem<-> (pred<->X1 .. arg2<->Y1),
	  Vfeatures = sem<->X1,
	  NPfeatures = sem<->Y1 }.

v(Features) --> [sees], { Features = sem<->'SEES' }.

np(Features) --> [max], { Features = sem<->'MAX' }.

np(Features) --> [bill], { Features = sem<->'BILL'}.

np(Features) --> [me], { Features = sem<->'ME' .. case<->acc }.

% Procedure to parse a sentence and display its features

try(Sentence) :- writeln([Sentence]), phrase(s(Features),Sentence),display_feature_structure(Features).

% Example Sentences

test1 :- try([max,sees,bill]).

test2 :- try([max,sees,me]).

test3 :- try([me,sees,max]). /* should fail */

			

