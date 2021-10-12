pdebug(WordList) :-
   repeat,
   nonterminal(N),
   parse(N, WordList, []),
   writeln(N),
   fail.

nonterminal(np(_LF, _Case, _Agreement, nogap)).
nonterminal(aux_vp(_VP, _Polarity, _Agreement, _Tense, _Aspect)).
nonterminal(s(_LF, _Mood, _Polarity, _Tense, _Aspect)).