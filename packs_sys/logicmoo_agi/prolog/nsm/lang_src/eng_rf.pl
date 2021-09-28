ff(p(Pred,[A,B,C,D,E],ArgList),
   [ predicate:
     [ head : Pred,
       prog : A,
       perfect : B,
       tpast : C,
       future : D,
       neg: E,
       arguments : ArgList_fo]],
  [format_arglist(eng:_,ArgList,ArgList_fo)
  ]).

auto_text(version,"Version").
auto_text(acknowledgements,"Acknowledgements to").
auto_text(abstract,"Abstract").
auto_text(level,"Level:").
auto_text(levels,"Level Declaration").
auto_text(ends_at,"Ends at:").
auto_text(transcr,"Transcription Tables").
auto_text(morphology,"Morphology").
auto_text(ph,"Allomorph Construction Rules").
auto_text(mseq,"Allowed Morpheme Sequence Rules").
auto_text(gdep,"Dependencies").
auto_text(syntax,"Syntax").
auto_text(table,"Table").
auto_text(conditions,"Conditions").
auto_text(paradigms,"Paradigms").
auto_text(prologue,"Prologue").
auto_text(epilogue,"Epilogue").
auto_text(examples,"Examples:").

