% A grammar in DCG notation, with GULP feature structures.

s(sem<-> (pred<->X .. arg1<->Y .. arg2<->Z)) -->
	np(sem<->Y .. case<->nom),
	vp(sem<-> (pred<->X .. arg2<->Z)).

vp(sem<-> (pred<->X1 .. arg2<->Y1)) -->
	v(sem<->X1),
	np(sem<->Y1).

v(sem<->'SEES') --> [sees].

np(sem<->'MAX') --> [max].

np(sem<->'BILL') --> [bill].

np(sem<->'ME' .. case<->acc) --> [me].

% Procedure to parse a sentence and display its features

try(Sentence) :- writeln([Sentence]), phrase(s(Features),Sentence),display_feature_structure(Features).

% Example Sentences

test1 :- try([max,sees,bill]).

test2 :- try([max,sees,me]).

test3 :- try([me,sees,max]). /* should fail */

			

