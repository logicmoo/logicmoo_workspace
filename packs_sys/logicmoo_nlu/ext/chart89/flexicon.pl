% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%   Example code from the book "Natural Language Processing in Prolog"  %
%                      published by Addison Wesley                      %
%        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%
%
% flexicon.pl [Chapter  4] Example lexicon - makes extensive use of features
%

:- dynamic((--->)/2).
:- multifile((--->)/2).


:- op(1200,xfx,--->).
%:- op(30,xfx,':').
%

np(person:3,number:singular,sex:_,case:_)  ---> [kim].
np(person:3,number:singular,sex:_,case:_)  ---> [sandy].
np(person:3,number:singular,sex:_,case:_)  ---> [lee].
np(person:3,number:singular,sex:male,case:_)  ---> [john].
np(person:3,number:singular,sex:female,case:_)  ---> [mary].
np(person:3,number:singular,sex:male,case:nominative)  ---> [he].
np(person:3,number:singular,sex:female,case:nominative)  ---> [she].
np(person:3,number:singular,sex:male,case:accusative)  ---> [him].
np(person:3,number:singular,sex:female,case:accusative)  ---> [her].
np(person:3,number:singular,sex:none,case:_)  ---> [it].
np(person:3,number:plural,sex:_,case:nominative)  ---> [they].
np(person:3,number:plural,sex:_,case:accusative)  ---> [them].
np(person:2,number:_,sex:_,case:_)  ---> [you].
np(person:1,number:singular,sex:_,case:nominative)  ---> [i].
np(person:1,number:singular,sex:_,case:accusative)  ---> [me].
np(person:1,number:plural,sex:_,case:nominative)  ---> [we].
np(person:1,number:plural,sex:_,case:accusative)  ---> [us].
%
det(number:singular)  ---> [a].
det(number:singular)  ---> [this].
det(number:plural)  ---> [these].
det(number:_)  ---> [the].
det(number:_)  ---> [her].
%
nn(number:singular,sex:_)  ---> [duck].
nn(number:singular,sex:none)  ---> [saw].
nn(number:singular,sex:_)  ---> [child].
nn(number:singular,sex:male)  ---> [man].
nn(number:singular,sex:female)  ---> [woman].
nn(number:plural,sex:_)  ---> [ducks].
nn(number:plural,sex:none)  ---> [saws].
nn(number:plural,sex:_)  ---> [children].
nn(number:plural,sex:male)  ---> [men].
nn(number:plural,sex:female)  ---> [women].
nn(number:plural,sex:none)  ---> [scissors].
nn(number:_,sex:_)  ---> [sheep].
nn(number:_,sex:_)  ---> [fish].
%
bv(person:_,number:_,verb_form:infinitive)  ---> [be].
bv(person:1,number:singular,verb_form:tensed)  ---> [am].
bv(person:2,number:singular,verb_form:tensed)  ---> [are].
bv(person:3,number:singular,verb_form:tensed)  ---> [is].
bv(person:_,number:plural,verb_form:tensed)  ---> [are].
bv(person:1,number:singular,verb_form:tensed)  ---> [was].
bv(person:2,number:singular,verb_form:tensed)  ---> [were].
bv(person:3,number:singular,verb_form:tensed)  ---> [was].
bv(person:_,number:plural,verb_form:tensed)  ---> [were].
iv(person:_,number:_,verb_form:infinitive)  ---> [duck].
iv(person:1,number:singular,verb_form:tensed)  ---> [duck].
iv(person:2,number:singular,verb_form:tensed)  ---> [duck].
iv(person:3,number:singular,verb_form:tensed)  ---> [ducks].
iv(person:_,number:plural,verb_form:tensed)  ---> [duck].
iv(person:_,number:_,verb_form:tensed)  ---> [ducked].
iv(person:_,number:_,verb_form:infinitive)  ---> [die].
iv(person:1,number:singular,verb_form:tensed)  ---> [die].
iv(person:2,number:singular,verb_form:tensed)  ---> [die].
iv(person:3,number:singular,verb_form:tensed)  ---> [dies].
iv(person:_,number:plural,verb_form:tensed)  ---> [die].
iv(person:_,number:_,verb_form:tensed)  ---> [died].
tv(person:_,number:_,verb_form:infinitive)  ---> [eat].
tv(person:1,number:singular,verb_form:tensed)  ---> [eat].
tv(person:2,number:singular,verb_form:tensed)  ---> [eat].
tv(person:3,number:singular,verb_form:tensed)  ---> [eats].
tv(person:_,number:plural,verb_form:tensed)  ---> [eat].
tv(person:_,number:_,verb_form:tensed)  ---> [ate].
tv(person:_,number:_,verb_form:infinitive)  ---> [see].
tv(person:1,number:singular,verb_form:tensed)  ---> [see].
tv(person:2,number:singular,verb_form:tensed)  ---> [see].
tv(person:3,number:singular,verb_form:tensed)  ---> [sees].
tv(person:_,number:plural,verb_form:tensed)  ---> [see].
tv(person:_,number:_,verb_form:tensed)  ---> [saw].
xv(person:_,number:_,verb_form:infinitive)  ---> [see].
xv(person:1,number:singular,verb_form:tensed)  ---> [see].
xv(person:2,number:singular,verb_form:tensed)  ---> [see].
xv(person:3,number:singular,verb_form:tensed)  ---> [sees].
xv(person:_,number:plural,verb_form:tensed)  ---> [see].
xv(person:_,number:_,verb_form:tensed)  ---> [saw].
%
adj(number:_,sex:_)  ---> [stupid].
adj(number:plural,sex:_)  ---> [numerous].
adj(number:_,sex:female)  ---> [pregnant].
%
wh(sex:male)  ---> [who].
wh(sex:female)  ---> [who].
wh(sex:_)  ---> [that].
%
