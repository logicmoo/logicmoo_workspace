port="2763"

cd ..
echo "
    ['webservice/acerules_server'],
    qsave_program('webservice/acerules-server.exe', [goal(acerules_server($port)), stand_alone(false), toplevel(halt)]).
  " | swipl -O
