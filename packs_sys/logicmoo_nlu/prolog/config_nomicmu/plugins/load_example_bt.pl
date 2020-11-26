

:- multifile(covid19_domain/3).
:- expand_file_name('/home/andrewdo/lib/swipl/pack/logicmoo_nlu/prolog/config_nomicmu/example_bt/*.pl',Expand),
   forall(member(File,Expand),
    locally(b_setval('$bt_context',covid19_domain),load_bt_file(File))).
                            
