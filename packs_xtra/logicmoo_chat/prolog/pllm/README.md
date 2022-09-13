
```prolog

root@gitlab:/opt/logicmoo_workspace/packs_xtra/logicmoo_chat# swipl
Welcome to SWI-Prolog (threaded, 64 bits, version 8.5.0-26-gce6ffabac-DIRTY)
SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software.
Please run ?- license. for legal details.

For online help and background, visit https://www.swi-prolog.org
For built-in help, use ?- help(Topic). or ?- apropos(Word).

?- use_module(library(logicmoo_pllm)).
%~ /root/.local/share/swi-prolog/pack/logicmoo_utils/prolog/logicmoo_common.pl:33
%~ init_phase(after_load)
true.

2 ?- compile_corpus.
%~ reading corpus...
% 138,245,537 inferences, 18.918 CPU in 18.921 seconds (100% CPU, 7307492 Lips)
%~ /opt/logicmoo_workspace/packs_xtra/logicmoo_chat/self_dialogue_corpus/train_from.txt:396886
%~ Trained on 372720 lines
%~ Overlap 803,179
%~ Nodes 2,841,658
%~ compute corpus stats...
% 45,649,005 inferences, 6.838 CPU in 6.839 seconds (100% CPU, 6675596 Lips)
true.

3 ?- autoc([len(55)]).
[other_conversant(0),have,you,seen,thor,?,i,cant,help,but,feel,more,of,an,attachment,
  to,the,first,one,left,off,
  other_conversant(102),youre,correct,but,i,cant,help,but,feel,more,of,an,attachment,to,the,orginial,,
  ,so,the,story,line,pretty,much,picked,up,where,the,first,one,left,off,.,i,do]
true .

4 ?- autoc([len(23),love,len(23)]).
[other_conversant(0),have,you,seen,thor,?,i,cant,help,but,feel,more,of,an,attachment,to,the,first,one,left,now,,,i,love,them,other_conversant(2402),they,are,great,for,the,part,when,the,mom,and,his,daughter,had,that,moment,in,the,basement,brought,tears,to]
true .

5 ?- autoc([len(23),dumb,len(23)]).
[other_conversant(0),have,you,seen,thor,?,i,cant,help,but,feel,more,of,an,attachment,to,the,first,one,left,now,,,and,dumb,to,compare,them,theres,nobody,really,better,?,other_conversant(156604),yeah,,,hes,awesome,to,live,forever,,,what,a,thought,right,!,?]
true .


```
