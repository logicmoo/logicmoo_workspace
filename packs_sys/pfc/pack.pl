name(pfc).
title('Pfc -- a package for forward chaining in Prolog').
version('2.0.3').
download('https://github.com/logicmoo/pfc/releases/*.zip').
author( 'Douglas R. Miles', 'logicmoo@gmail.com' ).
packager( 'logicmoo/LogicMoo', 'https://github.com/logicmoo/' ).
home('https://github.com/logicmoo/pfc').
%requires(s_expression).
requires(logicmoo_utils).
requires(dictoo).
autoload(false).

