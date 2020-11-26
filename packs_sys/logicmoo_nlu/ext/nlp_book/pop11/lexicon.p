;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;; %   Example code from the book "Natural Language Processing in POP-11"  %
;;; %                      published by Addison Wesley                      %
;;; %        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;;
;;; lexicon.p [Chapter  7] Example lexical entries

lib pop_patr;

[    [mor form1 stem]   = [mor root],
     [mor form1 suffix] = NULL,
     [mor form2 stem]   = [mor root],
     [mor form2 suffix] = NULL,
     [mor form3 stem]   = [mor root],
     [mor form3 suffix] = s,
     [mor form4 stem]   = [mor root],
     [mor form4 suffix] = ed,
     [mor form5 stem]   = [mor root],
     [mor form5 suffix] = ed,
     [mor form6 stem]   = [mor root],
     [mor form6 suffix] = ed,
     [mor form7 stem]   = [mor root],
     [mor form7 suffix] = ing
] -> patr_macro("mor_regV");

  [[syn cat] = V,
   [syn arg0 cat] = np,
   [syn arg0 case] = nom] -> patr_macro("syn_iV");

  [syn_iV,
   [syn arg1 cat] = np,
   [syn arg1 case] = acc] -> patr_macro("syn_tV");

vars lexicon;
newproperty([], 100, false, true) -> lexicon;

  [[mor root] = love,
   [sem] = love2a,
   mor_regV,
   syn_tV] -> lexicon("love");

vars example_wfc;
[ [word mor form] = [lexeme mor form3],
  [word syn] = [lexeme syn],
  [word syn cat] = V,
  [word syn arg0 per] = 3,
  [word syn arg0 num] = sing,
  [word syn tense] = pres,
  [word sem] = [lexeme sem]
] -> example_wfc;

define lookup_lex(lex);
   vars entry structure subst;
   lexicon(lex) -> entry;
   unless entry then
      mishap('no lexical entry for lexeme',[^lex])
   endunless;
   newvar() -> structure;
   apply_condition(entry,structure,empty_subst) -> subst;
   if subst then
      apply_subst(subst,structure)
   else
      mishap('Inconsistent lexical entry',[^lex])
   endif
enddefine;

define apply_wfc(wfc,word,lex);
   vars str subst;
   [[word ^word] [lexeme ^lex] [& ^(newvar())]] -> str;
   apply_condition(wfc,str,empty_subst) -> subst;
   if subst then
      simplify_features(subst,word)
   else
      false
   endif
enddefine;
