

```
(base) root@mail:/opt/logicmoo_workspace/packs_sys/pfc/t/pfc_1_8# swipl -l pfc.pl
% init_why(before_boot,after(/.../(pfc_1_8,'pfc.pl'))).
Welcome to SWI-Prolog (threaded, 64 bits, version 8.3.19-2-gc2ef0f4e8-DIRTY)
SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software.
Please run ?- license. for legal details.

For online help and background, visit https://www.swi-prolog.org
For built-in help, use ?- help(Topic). or ?- apropos(Word).

?- make.
Warning: The predicates below are not defined. If these are defined
Warning: at runtime using assert/1, use :- dynamic Name/Arity.
Warning:
Warning: else/0, which is referenced by
Warning:        /opt/logicmoo_workspace/packs_sys/pfc/t/pfc_1_8/pfccore.pl:118:8: 1-st clause of pfcEnqueue/2
Warning: fcompile/1, which is referenced by
Warning:        /opt/logicmoo_workspace/packs_sys/pfc/t/pfc_1_8/pfc.pl:19:27: 1-st clause of pfcFcompile/0
Warning: nth/3, which is referenced by
Warning:        /opt/logicmoo_workspace/packs_sys/pfc/t/pfc_1_8/pfcwhy.pl:94:2: 1-st clause of pfcSelectJustificationNode/3
Warning:        /opt/logicmoo_workspace/packs_sys/pfc/t/pfc_1_8/pfcwhy.pl:96:2: 1-st clause of pfcSelectJustificationNode/3
Warning: pfcAddSupport/3, which is referenced by
Warning:        /opt/logicmoo_workspace/packs_sys/pfc/t/pfc_1_8/pfcsupport.pl:53:2: 1-st clause of pfc_make_supports/1
Warning: pfcBtPtCombine/2, which is referenced by
Warning:        /opt/logicmoo_workspace/packs_sys/pfc/t/pfc_1_8/pfccore.pl:241:2: 3-th clause of pfcAddTrigger/2
Warning: pfcCurrentDb/1, which is referenced by
Warning:        /opt/logicmoo_workspace/packs_sys/pfc/t/pfc_1_8/pfccore.pl:97:2: 1-st clause of pfcAddDbToHead/2
Warning: pfcGetTrigger/1, which is referenced by
Warning:        /opt/logicmoo_workspace/packs_sys/pfc/t/pfc_1_8/pfcdebug.pl:74:16: 1-st clause of pfcPrintTriggers/0
Warning:        /opt/logicmoo_workspace/packs_sys/pfc/t/pfc_1_8/pfcdebug.pl:77:18: 1-st clause of pfcPrintTriggers/0
Warning:        /opt/logicmoo_workspace/packs_sys/pfc/t/pfc_1_8/pfcdebug.pl:80:16: 1-st clause of pfcPrintTriggers/0
Warning: support1/3, which is referenced by
Warning:        /opt/logicmoo_workspace/packs_sys/pfc/t/pfc_1_8/pfcsupport.pl:14:24: 1-st clause of pfcGetSupport/2
Warning:        /opt/logicmoo_workspace/packs_sys/pfc/t/pfc_1_8/pfcsupport.pl:17:24: 1-st clause of pfcGetSupport/2
Warning:        /opt/logicmoo_workspace/packs_sys/pfc/t/pfc_1_8/pfcsupport.pl:50:2: 1-st clause of pfc_support_relation/1
Warning: support2/3, which is referenced by
Warning:        /opt/logicmoo_workspace/packs_sys/pfc/t/pfc_1_8/pfcsupport.pl:15:24: 1-st clause of pfcGetSupport/2
Warning: support3/3, which is referenced by
Warning:        /opt/logicmoo_workspace/packs_sys/pfc/t/pfc_1_8/pfccore.pl:576:2: 1-st clause of fcnt/2
Warning:        /opt/logicmoo_workspace/packs_sys/pfc/t/pfc_1_8/pfcsupport.pl:16:24: 1-st clause of pfcGetSupport/2
Warning: whymemory/2, which is referenced by
Warning:        /opt/logicmoo_workspace/packs_sys/pfc/t/pfc_1_8/pfcwhy.pl:11:2: 1-st clause of pfcWhy/0
Warning:        /opt/logicmoo_workspace/packs_sys/pfc/t/pfc_1_8/pfcwhy.pl:17:2: 1-st clause of pfcWhy/1
```
