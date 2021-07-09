
# cls ; find -name "*_*.pl" \( -name "parser_*" -or -name "logicmoo_*" \) -printf "| Emulation/Support of %f | ([[%f >>https://logicmoo.org/gitlab/logicmoo/logicmoo_workspace/-/tree/master/%P]]) | 100%% | 0%% | 0%% |\n" | sed -e "s/\.pl //g" -e "s/of logicmoo_/of /g" -e "s/of parser_/for Parser /g"  | grep -v example

cls ; find -name "*_*.pl" \( -name "parser_*" -or -name "logicmoo_*" \) -printf '| Emulation/Support of %f | ([[%f >>https://logicmoo.org/gitlab/logicmoo/logicmoo_workspace/-/tree/master/%P]])  |(%% style="background-color: green" %%) 100%% |(%% style="background-color: red" %%)   0%% |(%% style="background-color: red" %%)  0%% |     (%% style="background-color: orange" %%)  0/?? | \n' | sed -e "s/\.pl //g" -e "s/of logicmoo_/of /g" -e "s/of parser_/for Parser /g"  | grep -v example

