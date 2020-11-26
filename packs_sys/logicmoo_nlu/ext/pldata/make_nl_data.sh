
echo "QCompiling clex_nldata..."
swipl -g "time(qcompile(clex_nldata))" -g halt
echo "QCompiling tt0_00022_cycl..."
swipl -g "time(qcompile(tt0_00022_cycl))" -g halt
echo "Be patient, this next one will take 60-120 seconds..."
echo "QCompiling ac_xnl_7166..."
swipl -g "time(qcompile(ac_xnl_7166))" -g halt
echo "Success!"

