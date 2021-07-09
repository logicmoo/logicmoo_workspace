% Enter your password.
% Connected to PrologMUD, running eggdrop v1.6.21
%      ____                __
%     / __/___ _ ___ _ ___/ /____ ___   ___
%    / _/ / _ `// _ `// _  // __// _ \ / _ \
%   /___/ \_, / \_, / \_,_//_/   \___// .__/
%        /___/ /___/                 /_/
% Hey swipl!  My name is PrologMUD and I am running eggdrop v1.6.21, on Linux 5.0.0-38-generic.
% Local time is now 13:56
% You are an owner of this bot. Only +n users can see this! For more info,
% see .help set motd. Please edit the motd file in your bot's 'text'
% Use .help for basic help.
% Use .help <command> for help on a specific command.
% Use .help all to get a full command list.
% Use .help *somestring* to list any help texts containing "somestring".
% Have fun.
% Commands start with '.' (like '.quit' or '.help')
% Everything else goes out to the party line.
% get2react([chon,"swipl","10"]).
% You have no messages.
% *** swipl joined the party line.
% Echo turned off.
% Set your console to ##prolog: - (none).
% That channel already exists!
% That channel already exists!
% That channel already exists!
% That channel already exists!
% Msg to nickserv: identify swipl
% Set your console to #logicmoo: - (none).
% /pack/lps_corner/examples/binaryChop2.pl:2
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(examples, 'binaryChop2.pl'), strip=lps, ctx=lps, sm= /.../(examples, 'binaryChop2.pl'), lps= /.../(examples, 'binaryChop2.pl'), using= /.../(examples, 'binaryChop2.pl')].
% continue_lps_dialect.
% ops.

% LPS:  fluents((left(_18944),right(_18944),searching(_19000))).
% Into: fluents([left(_18944),right(_18944),searching(_19000)]).

% LPS:  initially((left(0),right(9),searching(60))).
% Into: initial_state([left(0),right(9),searching(60)]).

% LPS:  actions(sample(_20552)).
% Into: actions([sample(_20552)]).

% LPS:  events(do_sample/0).
% Into: events([do_sample]).

% LPS:  if(at(found(_38926),_38948),(at(left(_38926),_38948),at(right(_38926),_38948))).
% Into: l_int(holds(found(_38926),_38948),[holds(left(_38926),_38948),holds(right(_38926),_38948)]).

% LPS:  if(do_sample,(left(_40974),right(_41014),_41156 is (_41014+_40974)div 2,sample(_41156))).
% Into: l_events(happens(do_sample,_42410,_42416),[holds(left(_40974),_42410),holds(right(_41014),_42410),_41156 is (_41014+_40974)div 2,happens(sample(_41156),_42410,_42416)]).

% LPS:  then(if(not(found(_43290))),do_sample).
% Into: reactive_rule([holds(not(found(_43290)),_44386)],[happens(do_sample,_44466,_44472)]).

% LPS:  if(initiates(sample(_44390),left(_44430)),(searching(_44502),a(_44390,_44558),_44558<_44502,_44430 is _44390+1)).
% Into: initiated(happens(sample(_44390),_46052,_46058),left(_44430),[holds(searching(_44502),_46052),a(_44390,_44558),_44558<_44502,_44430 is _44390+1]).

% LPS:  if(terminates(sample(_45962),left(_46002)),(searching(_46074),a(_45962,_46130),_46130<_46074)).
% Into: terminated(happens(sample(_45962),_47442,_47448),left(_46002),[holds(searching(_46074),_47442),a(_45962,_46130),_46130<_46074]).

% LPS:  if(initiates(sample(_47360),right(_47360)),(searching(_47472),a(_47360,_47528),_47528>=_47472)).
% Into: initiated(happens(sample(_47360),_48840,_48846),right(_47360),[holds(searching(_47472),_48840),a(_47360,_47528),_47528>=_47472]).

% LPS:  if(terminates(sample(_48758),right(_48798)),(searching(_48870),a(_48758,_48926),_48926>=_48870)).
% Into: terminated(happens(sample(_48758),_50238,_50244),right(_48798),[holds(searching(_48870),_50238),a(_48758,_48926),_48926>=_48870]).
% /pack/lps_corner/examples/binaryChop2.pl:29
% pop_lps_dialect('$BLOB'("<stream>(0x562ef33c0700)"),  (/.../(examples, 'binaryChop2.pl')-> /.../(examples, 'binaryChop2.pl'))).
% ops.
% :-listing('/pack/lps_corner/examples/binaryChop2.pl':_58286).


initiated(happens(sample(A), B, _), left(C), [holds(searching(D), B), a(A, E), E<D, C is A+1]).
initiated(happens(sample(A), B, _), right(A), [holds(searching(C), B), a(A, D), D>=C]).

fluents([left(A), right(A), searching(_)]).

l_int(holds(found(A), B), [holds(left(A), B), holds(right(A), B)]).

a(0, 10).
a(1, 12).
a(2, 20).
a(3, 25).
a(4, 30).
a(5, 31).
a(6, 35).
a(7, 60).
a(8, 65).
a(9, 500).

reactive_rule([holds(not(found(_)), _)], [happens(do_sample, _, _)]).

initial_state([left(0), right(9), searching(60)]).

l_events(happens(do_sample, A, B), [holds(left(C), A), holds(right(D), A), E is (D+C)div 2, happens(sample(E), A, B)]).

terminated(happens(sample(A), B, _), left(_), [holds(searching(C), B), a(A, D), D<C]).
terminated(happens(sample(A), B, _), right(_), [holds(searching(C), B), a(A, D), D>=C]).

:- dynamic actions/1.
:- multifile actions/1.

actions([sample(_)]).

events([do_sample]).
% dB(/.../(examples, 'binaryChop2.pl'), lps_visualization(_62278{groups:[_60894{content:"left(A)", id:"left/1", order:3, subgroupStack:"false"}, _60972{content:"right(A)", id:"right/1", order:3, subgroupStack:"false"}, _61050{content:"searching(A)", id:"searching/1", order:3, subgroupStack:"false"}, _61116{content:"Actions", id:"action", order:4}], items:[_61238{content:"0", end:2, group:"left/1", id:0, start:1, subgroup:"0", title:"Fluent left(0) initiated at 1<br/>and terminated at transition to 2"}, _61364{content:"5", end:4, group:"left/1", id:1, start:2, subgroup:"5", title:"Fluent left(5) initiated at 2<br/>and terminated at transition to 4"}, _61490{content:"7", end:21, group:"left/1", id:2, start:4, subgroup:"7", title:"Fluent left(7) initiated at 4<br/>and terminated at transition to 21"}, _61616{content:"7", end:21, group:"right/1", id:3, start:3, subgroup:"7", title:"Fluent right(7) initiated at 3<br/>and terminated at transition to 21"}, _61742{content:"9", end:3, group:"right/1", id:4, start:1, subgroup:"9", title:"Fluent right(9) initiated at 1<br/>and terminated at transition to 3"}, _61868{content:"60", end:21, group:"searching/1", id:5, start:1, subgroup:"60", title:"Fluent searching(60) initiated at 1<br/>and terminated at transition to 21"}, _61994{content:"sample(4)", group:"action", id:6, start:2, style:"color:green", title:"happens(sample(4),1,2)", type:"point"}, _62120{content:"sample(7)", group:"action", id:7, start:3, style:"color:green", title:"happens(sample(7),2,3)", type:"point"}, _62246{content:"sample(6)", group:"action", id:8, start:4, style:"color:green", title:"happens(sample(6),3,4)", type:"point"}]}, [])).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'salomon.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'salomon.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/salomon.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'salomon.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'salomon.pl'), lps= /.../(lps_user_examples, 'salomon.pl'), using= /.../(lps_user_examples, 'salomon.pl')].
% continue_lps_dialect.
% ops.

% LPS:  events((disputa_entre(_19928,_19930),poner_a_prueba(_19928,_19930),propone_salida_drastica(_20026),tomar_decision)).
% Into: events([disputa_entre(_19928,_19930),poner_a_prueba(_19928,_19930),propone_salida_drastica(_20026),tomar_decision]).

% LPS:  actions((propone_dividir_nino(_21290),dice(_21290,_21346),declara(_21290,_21402),dicta(_21290,_21458))).
% Into: actions([propone_dividir_nino(_21290),dice(_21290,_21346),declara(_21290,_21402),dicta(_21290,_21458)]).

% LPS:  observe(from(disputa_entre(a,b),to(1,2))).
% Into: observe([disputa_entre(a,b)],2).

% LPS:  then(if(from(disputa_entre(_23912,_23914),to(_23950,_23952))),from(poner_a_prueba(_23912,_23914),to(_23952,_24128))).
% Into: reactive_rule([happens(disputa_entre(_23912,_23914),_23950,_23952)],[happens(poner_a_prueba(_23912,_23914),_23952,_24128)]).

% LPS:  if(from(poner_a_prueba(_25390,_25392),to(_25428,_25430)),from(propone_salida_drastica(salomon),to(_25428,_25430))).
% Into: l_events(happens(poner_a_prueba(_25390,_25392),_25428,_25430),[happens(propone_salida_drastica(salomon),_25428,_25430)]).

% LPS:  if(from(propone_salida_drastica(_26730),to(_26766,_26768)),from(propone_dividir_nino(_26730),to(_26766,_26768))).
% Into: l_events(happens(propone_salida_drastica(_26730),_26766,_26768),[happens(propone_dividir_nino(_26730),_26766,_26768)]).

% LPS:  then(if((from(propone_dividir_nino(salomon),to(_28154,_28156)),mujer(_28254),soy_su_madre(_28254))),from(dice(_28254,'No lo mate! Déselo a Ella'),to(_28156,_28476))).
% Into: reactive_rule([happens(propone_dividir_nino(salomon),_28154,_28156),mujer(_28254),soy_su_madre(_28254)],[happens(dice(_28254,'No lo mate! Déselo a Ella'),_28156,_28476)]).

% LPS:  then(if((from(propone_dividir_nino(salomon),to(_30698,_30700)),mujer(_30798),not(soy_su_madre(_30798)))),from(dice(_30798,'Sí, mátelo'),to(_30700,_31044))).
% Into: reactive_rule([happens(propone_dividir_nino(salomon),_30698,_30700),mujer(_30798),not(soy_su_madre(_30798))],[happens(dice(_30798,'Sí, mátelo'),_30700,_31044)]).

% LPS:  then(if((from(propone_dividir_nino(_33742),to(_33778,_33780)),from(dice(_33892,'No lo mate! Déselo a Ella'),to(_33930,_33932)),from(dice(_34044,'Sí, mátelo'),to(_34082,_34084)))),(from(declara(_33742,la_verdadera_madre_es(_33892)),to(_34346,_34348)),from(dicta(_33742,entreguen_nino_a(_33892)),to(_34348,_34524)))).
% Into: reactive_rule([happens(propone_dividir_nino(_33742),_33778,_33780),happens(dice(_33892,'No lo mate! Déselo a Ella'),_33930,_33932),happens(dice(_34044,'Sí, mátelo'),_34082,_34084)],[happens(declara(_33742,la_verdadera_madre_es(_33892)),_34346,_34348),happens(dicta(_33742,entreguen_nino_a(_33892)),_34348,_34524)]).
% /pack/logicmoo_ec/test/lps_user_examples/salomon.pl:68
% pop_lps_dialect('$BLOB'("<stream>(0x562ef33c3e00)"),  (/.../(lps_user_examples, 'salomon.pl')-> /.../(lps_user_examples, 'salomon.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/salomon.pl':_47468).


reactive_rule([happens(disputa_entre(A, B), _, C)], [happens(poner_a_prueba(A, B), C, _)]).
reactive_rule([happens(propone_dividir_nino(salomon), _, A), mujer(B), soy_su_madre(B)], [happens(dice(B, 'No lo mate! Déselo a Ella'), A, _)]).
reactive_rule([happens(propone_dividir_nino(salomon), _, A), mujer(B), not(soy_su_madre(B))], [happens(dice(B, 'Sí, mátelo'), A, _)]).
reactive_rule([happens(propone_dividir_nino(A), _, _), happens(dice(B, 'No lo mate! Déselo a Ella'), _, _), happens(dice(_, 'Sí, mátelo'), _, _)], [happens(declara(A, la_verdadera_madre_es(B)), _, C), happens(dicta(A, entreguen_nino_a(B)), C, _)]).

soy_su_madre(b).

l_events(happens(poner_a_prueba(_, _), A, B), [happens(propone_salida_drastica(salomon), A, B)]).
l_events(happens(propone_salida_drastica(A), B, C), [happens(propone_dividir_nino(A), B, C)]).

maxtime(10).

:- dynamic actions/1.
:- multifile actions/1.

actions([propone_dividir_nino(A), dice(A, _), declara(A, _), dicta(A, _)]).

events([disputa_entre(A, B), poner_a_prueba(A, B), propone_salida_drastica(_), tomar_decision]).

observe([disputa_entre(a, b)], 2).

mujer(a).
mujer(b).
% dB(/.../(lps_user_examples, 'salomon.pl'), lps_visualization(_74304{groups:[_73458{content:"Events", id:"event", order:1}, _73520{content:"Actions", id:"action", order:4}], items:[_73642{content:"disputa_entre(a,b)", group:"event", id:0, start:2, style:"color:#E19735", title:"happens(disputa_entre(a,b),1,2)", type:"point"}, _73768{content:"propone_dividir_nino(salomon)", group:"action", id:1, start:3, style:"color:green", title:"happens(propone_dividir_nino(salomon),2,3)", type:"point"}, _73894{content:"dice(a,Sí, mátelo)", group:"action", id:2, start:4, style:"color:green", title:"happens(dice(a,Sí, mátelo),3,4)", type:"point"}, _74020{content:"dice(b,No lo mate! Déselo a Ella)", group:"action", id:3, start:4, style:"color:green", title:"happens(dice(b,No lo mate! Déselo a Ella),3,4)", type:"point"}, _74146{content:"declara(salomon,la_verdadera_madre_es(b))", group:"action", id:4, start:5, style:"color:green", title:"happens(declara(salomon,la_verdadera_madre_es(b)),4,5)", type:"point"}, _74272{content:"dicta(salomon,entreguen_nino_a(b))", group:"action", id:5, start:6, style:"color:green", title:"happens(dicta(salomon,entreguen_nino_a(b)),5,6)", type:"point"}]}, [])).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'Samuel Contreras.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'Samuel Contreras.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'Samuel Contreras.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'Samuel Contreras.pl'), lps= /.../(lps_user_examples, 'Samuel Contreras.pl'), using= /.../(lps_user_examples, 'Samuel Contreras.pl')].
% continue_lps_dialect.
% ops.

% LPS:  fluents(engaña/2).
% Into: fluents([engaña(_55932,_55934)]).

% LPS:  actions((ayuda/2,encuentra/1)).
% Into: actions([ayuda(_57134,_57136),encuentra(_57146)]).

% LPS:  events((necesita/2,escapa/1)).
% Into: events([necesita(_58294,_58296),escapa(_58306)]).

% LPS:  initially(engaña(bruja,niño)).
% Into: initial_state([engaña(bruja,niño)]).

% LPS:  observe(from(necesita(bruja,objeto),to(1,2))).
% Into: observe([necesita(bruja,objeto)],2).

% LPS:  if(initiates(encuentra(_60436),engaña(_60490,_60492)),engaña(_60492,_60490)).
% Into: initiated(happens(encuentra(_60436),_61754,_61760),engaña(_60490,_60492),[holds(engaña(_60492,_60490),_61754)]).

% LPS:  then(if(necesita(bruja,objeto)),ayuda(niño,bruja)).
% Into: reactive_rule([happens(necesita(bruja,objeto),_63140,_63146)],[happens(ayuda(niño,bruja),_63172,_63178)]).

% LPS:  then(if(ayuda(niño,bruja)),encuentra(objeto)).
% Into: reactive_rule([happens(ayuda(niño,bruja),_64514,_64520)],[happens(encuentra(objeto),_64678,_64684)]).

% LPS:  if(escapa(niño),engaña(niño,bruja)).
% Into: l_events(happens(escapa(niño),_65806,_65806),[holds(engaña(niño,bruja),_65806)]).

% LPS:  observe(from(necesita(bruja,objeto),to(4,5))).
% Into: observe([necesita(bruja,objeto)],5).

% LPS:  then(if(necesita(bruja,objeto)),ayuda(niño,bruja)).
% Into: reactive_rule([happens(necesita(bruja,objeto),_68356,_68362)],[happens(ayuda(niño,bruja),_68388,_68394)]).

% LPS:  then(if(ayuda(niño,bruja)),encuentra(objeto)).
% Into: reactive_rule([happens(ayuda(niño,bruja),_69482,_69488)],[happens(encuentra(objeto),_69514,_69520)]).

% LPS:  then(if((encuentra(objeto),engaña(niño,bruja))),escapa(niño)).
% Into: reactive_rule([happens(encuentra(objeto),_70690,_70696),holds(engaña(niño,bruja),_70696)],[happens(escapa(niño),_70788,_70794)]).
% /usr/lib/swipl/library/option.pl:0
% tryed(tmp:% module_dialect_lps('$BLOB'("<stream>(0x562ef33a9a00)"), _64924, /.../(lps_user_examples, 'Samuel Contreras.pl'), _64928)).
:- thread_local % module_dialect_lps/4.
:- dynamic % module_dialect_lps/4.
:- volatile % module_dialect_lps/4.

% module_dialect_lps('$BLOB'("<stream>(0x562ef32ab000)"), '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl', '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl', [op(0, fy, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':not), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':then), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':if), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':if), op(0, xfy, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':else), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':terminates), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':initiates), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':updates), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':observe), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':false), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':initially), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':fluents), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':events), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':prolog_events), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':actions), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':unserializable), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':update), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':initiate), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':terminate), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':in), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':at), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':during), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':from), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':to), op(0, xfy, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl': ::), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl': <-), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl': <-), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl': <=)]).

% /usr/lib/swipl/library/option.pl:37
% tryed(tmp:% module_dialect_lps('$BLOB'("<stream>(0x562ef33a9a00)"), _72670, /.../(lps_user_examples, 'Samuel Contreras.pl'), _72674)).
:- thread_local % module_dialect_lps/4.
:- dynamic % module_dialect_lps/4.
:- volatile % module_dialect_lps/4.

% module_dialect_lps('$BLOB'("<stream>(0x562ef32ab000)"), '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl', '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl', [op(0, fy, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':not), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':then), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':if), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':if), op(0, xfy, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':else), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':terminates), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':initiates), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':updates), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':observe), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':false), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':initially), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':fluents), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':events), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':prolog_events), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':actions), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':unserializable), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':update), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':initiate), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':terminate), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':in), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':at), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':during), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':from), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':to), op(0, xfy, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl': ::), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl': <-), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl': <-), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl': <=)]).

% /usr/lib/swipl/library/option.pl:0
% tryed(tmp:% module_dialect_lps('$BLOB'("<stream>(0x562ef33a9400)"), _22632, /.../(lps_user_examples, 'Samuel Contreras.pl'), _22636)).
:- thread_local % module_dialect_lps/4.
:- dynamic % module_dialect_lps/4.
:- volatile % module_dialect_lps/4.

% module_dialect_lps('$BLOB'("<stream>(0x562ef32ab000)"), '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl', '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl', [op(0, fy, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':not), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':then), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':if), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':if), op(0, xfy, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':else), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':terminates), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':initiates), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':updates), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':observe), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':false), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':initially), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':fluents), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':events), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':prolog_events), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':actions), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':unserializable), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':update), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':initiate), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':terminate), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':in), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':at), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':during), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':from), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':to), op(0, xfy, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl': ::), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl': <-), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl': <-), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl': <=)]).

% /usr/lib/swipl/library/option.pl:37
% tryed(tmp:% module_dialect_lps('$BLOB'("<stream>(0x562ef33a9400)"), _30036, /.../(lps_user_examples, 'Samuel Contreras.pl'), _30040)).
:- thread_local % module_dialect_lps/4.
:- dynamic % module_dialect_lps/4.
:- volatile % module_dialect_lps/4.

% module_dialect_lps('$BLOB'("<stream>(0x562ef32ab000)"), '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl', '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl', [op(0, fy, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':not), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':then), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':if), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':if), op(0, xfy, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':else), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':terminates), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':initiates), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':updates), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':observe), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':false), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':initially), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':fluents), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':events), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':prolog_events), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':actions), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':unserializable), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':update), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':initiate), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':terminate), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':in), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':at), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':during), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':from), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':to), op(0, xfy, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl': ::), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl': <-), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl': <-), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl': <=)]).

% /usr/lib/swipl/library/process.pl:0
% tryed(tmp:% module_dialect_lps('$BLOB'("<stream>(0x562ef33a8e00)"), _37396, /.../(lps_user_examples, 'Samuel Contreras.pl'), _37400)).
:- thread_local % module_dialect_lps/4.
:- dynamic % module_dialect_lps/4.
:- volatile % module_dialect_lps/4.

% module_dialect_lps('$BLOB'("<stream>(0x562ef32ab000)"), '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl', '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl', [op(0, fy, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':not), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':then), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':if), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':if), op(0, xfy, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':else), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':terminates), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':initiates), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':updates), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':observe), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':false), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':initially), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':fluents), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':events), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':prolog_events), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':actions), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':unserializable), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':update), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':initiate), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':terminate), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':in), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':at), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':during), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':from), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':to), op(0, xfy, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl': ::), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl': <-), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl': <-), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl': <=)]).

% /usr/lib/swipl/library/process.pl:37
% tryed(tmp:% module_dialect_lps('$BLOB'("<stream>(0x562ef33a8e00)"), _45334, /.../(lps_user_examples, 'Samuel Contreras.pl'), _45338)).
:- thread_local % module_dialect_lps/4.
:- dynamic % module_dialect_lps/4.
:- volatile % module_dialect_lps/4.

% module_dialect_lps('$BLOB'("<stream>(0x562ef32ab000)"), '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl', '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl', [op(0, fy, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':not), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':then), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':if), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':if), op(0, xfy, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':else), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':terminates), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':initiates), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':updates), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':observe), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':false), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':initially), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':fluents), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':events), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':prolog_events), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':actions), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':unserializable), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':update), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':initiate), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':terminate), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':in), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':at), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':during), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':from), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':to), op(0, xfy, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl': ::), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl': <-), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl': <-), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl': <=)]).

% /usr/lib/swipl/library/error.pl:0
% tryed(tmp:% module_dialect_lps('$BLOB'("<stream>(0x562ef33a8800)"), _52816, /.../(lps_user_examples, 'Samuel Contreras.pl'), _52820)).
:- thread_local % module_dialect_lps/4.
:- dynamic % module_dialect_lps/4.
:- volatile % module_dialect_lps/4.

% module_dialect_lps('$BLOB'("<stream>(0x562ef32ab000)"), '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl', '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl', [op(0, fy, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':not), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':then), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':if), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':if), op(0, xfy, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':else), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':terminates), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':initiates), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':updates), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':observe), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':false), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':initially), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':fluents), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':events), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':prolog_events), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':actions), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':unserializable), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':update), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':initiate), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':terminate), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':in), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':at), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':during), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':from), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':to), op(0, xfy, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl': ::), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl': <-), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl': <-), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl': <=)]).

% /usr/lib/swipl/library/error.pl:36
% tryed(tmp:% module_dialect_lps('$BLOB'("<stream>(0x562ef33a8800)"), _60872, /.../(lps_user_examples, 'Samuel Contreras.pl'), _60876)).
:- thread_local % module_dialect_lps/4.
:- dynamic % module_dialect_lps/4.
:- volatile % module_dialect_lps/4.

% module_dialect_lps('$BLOB'("<stream>(0x562ef32ab000)"), '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl', '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl', [op(0, fy, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':not), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':then), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':if), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':if), op(0, xfy, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':else), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':terminates), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':initiates), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':updates), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':observe), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':false), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':initially), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':fluents), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':events), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':prolog_events), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':actions), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':unserializable), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':update), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':initiate), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':terminate), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':in), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':at), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':during), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':from), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':to), op(0, xfy, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl': ::), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl': <-), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl': <-), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl': <=)]).

% /usr/lib/swipl/library/apply.pl:0
% tryed(tmp:% module_dialect_lps('$BLOB'("<stream>(0x562ef33a8300)"), _68076, /.../(lps_user_examples, 'Samuel Contreras.pl'), _68080)).
:- thread_local % module_dialect_lps/4.
:- dynamic % module_dialect_lps/4.
:- volatile % module_dialect_lps/4.

% module_dialect_lps('$BLOB'("<stream>(0x562ef32ab000)"), '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl', '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl', [op(0, fy, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':not), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':then), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':if), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':if), op(0, xfy, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':else), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':terminates), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':initiates), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':updates), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':observe), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':false), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':initially), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':fluents), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':events), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':prolog_events), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':actions), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':unserializable), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':update), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':initiate), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':terminate), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':in), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':at), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':during), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':from), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':to), op(0, xfy, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl': ::), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl': <-), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl': <-), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl': <=)]).

% /usr/lib/swipl/library/apply.pl:36
% tryed(tmp:% module_dialect_lps('$BLOB'("<stream>(0x562ef33a8300)"), _76690, /.../(lps_user_examples, 'Samuel Contreras.pl'), _76694)).
:- thread_local % module_dialect_lps/4.
:- dynamic % module_dialect_lps/4.
:- volatile % module_dialect_lps/4.

% module_dialect_lps('$BLOB'("<stream>(0x562ef32ab000)"), '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl', '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl', [op(0, fy, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':not), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':then), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':if), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':if), op(0, xfy, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':else), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':terminates), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':initiates), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':updates), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':observe), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':false), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':initially), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':fluents), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':events), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':prolog_events), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':actions), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':unserializable), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':update), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':initiate), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':terminate), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':in), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':at), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':during), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':from), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':to), op(0, xfy, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl': ::), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl': <-), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl': <-), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl': <=)]).

% /usr/lib/swipl/library/option.pl:0
% tryed(tmp:% module_dialect_lps('$BLOB'("<stream>(0x562ef33a7d00)"), _83960, /.../(lps_user_examples, 'Samuel Contreras.pl'), _83964)).
:- thread_local % module_dialect_lps/4.
:- dynamic % module_dialect_lps/4.
:- volatile % module_dialect_lps/4.

% module_dialect_lps('$BLOB'("<stream>(0x562ef32ab000)"), '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl', '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl', [op(0, fy, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':not), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':then), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':if), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':if), op(0, xfy, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':else), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':terminates), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':initiates), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':updates), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':observe), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':false), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':initially), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':fluents), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':events), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':prolog_events), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':actions), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':unserializable), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':update), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':initiate), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':terminate), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':in), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':at), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':during), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':from), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':to), op(0, xfy, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl': ::), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl': <-), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl': <-), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl': <=)]).

% /usr/lib/swipl/library/option.pl:37
% tryed(tmp:% module_dialect_lps('$BLOB'("<stream>(0x562ef33a7d00)"), _91364, /.../(lps_user_examples, 'Samuel Contreras.pl'), _91368)).
:- thread_local % module_dialect_lps/4.
:- dynamic % module_dialect_lps/4.
:- volatile % module_dialect_lps/4.

% module_dialect_lps('$BLOB'("<stream>(0x562ef32ab000)"), '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl', '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl', [op(0, fy, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':not), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':then), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':if), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':if), op(0, xfy, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':else), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':terminates), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':initiates), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':updates), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':observe), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':false), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':initially), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':fluents), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':events), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':prolog_events), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':actions), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':unserializable), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':update), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':initiate), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':terminate), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':in), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':at), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':during), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':from), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':to), op(0, xfy, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl': ::), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl': <-), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl': <-), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl': <=)]).

% /usr/lib/swipl/library/readutil.pl:0
% tryed(tmp:% module_dialect_lps('$BLOB'("<stream>(0x562ef33a7600)"), _21206, /.../(lps_user_examples, 'Samuel Contreras.pl'), _21210)).
:- thread_local % module_dialect_lps/4.
:- dynamic % module_dialect_lps/4.
:- volatile % module_dialect_lps/4.

% module_dialect_lps('$BLOB'("<stream>(0x562ef32ab000)"), '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl', '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl', [op(0, fy, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':not), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':then), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':if), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':if), op(0, xfy, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':else), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':terminates), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':initiates), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':updates), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':observe), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':false), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':initially), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':fluents), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':events), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':prolog_events), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':actions), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':unserializable), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':update), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':initiate), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':terminate), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':in), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':at), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':during), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':from), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':to), op(0, xfy, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl': ::), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl': <-), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl': <-), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl': <=)]).

% /usr/lib/swipl/library/readutil.pl:37
% tryed(tmp:% module_dialect_lps('$BLOB'("<stream>(0x562ef33a7600)"), _28732, /.../(lps_user_examples, 'Samuel Contreras.pl'), _28736)).
:- thread_local % module_dialect_lps/4.
:- dynamic % module_dialect_lps/4.
:- volatile % module_dialect_lps/4.

% module_dialect_lps('$BLOB'("<stream>(0x562ef32ab000)"), '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl', '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl', [op(0, fy, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':not), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':then), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':if), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':if), op(0, xfy, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':else), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':terminates), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':initiates), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':updates), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':observe), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':false), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':initially), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':fluents), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':events), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':prolog_events), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':actions), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':unserializable), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':update), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':initiate), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':terminate), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':in), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':at), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':during), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':from), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':to), op(0, xfy, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl': ::), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl': <-), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl': <-), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl': <=)]).

% /usr/lib/swipl/library/readutil.pl:0
% tryed(tmp:% module_dialect_lps('$BLOB'("<stream>(0x562ef33a7000)"), _36084, /.../(lps_user_examples, 'Samuel Contreras.pl'), _36088)).
:- thread_local % module_dialect_lps/4.
:- dynamic % module_dialect_lps/4.
:- volatile % module_dialect_lps/4.

% module_dialect_lps('$BLOB'("<stream>(0x562ef32ab000)"), '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl', '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl', [op(0, fy, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':not), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':then), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':if), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':if), op(0, xfy, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':else), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':terminates), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':initiates), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':updates), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':observe), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':false), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':initially), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':fluents), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':events), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':prolog_events), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':actions), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':unserializable), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':update), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':initiate), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':terminate), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':in), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':at), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':during), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':from), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':to), op(0, xfy, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl': ::), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl': <-), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl': <-), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl': <=)]).

% /usr/lib/swipl/library/readutil.pl:37
% tryed(tmp:% module_dialect_lps('$BLOB'("<stream>(0x562ef33a7000)"), _43610, /.../(lps_user_examples, 'Samuel Contreras.pl'), _43614)).
:- thread_local % module_dialect_lps/4.
:- dynamic % module_dialect_lps/4.
:- volatile % module_dialect_lps/4.

% module_dialect_lps('$BLOB'("<stream>(0x562ef32ab000)"), '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl', '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl', [op(0, fy, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':not), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':then), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':if), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':if), op(0, xfy, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':else), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':terminates), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':initiates), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':updates), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':observe), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':false), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':initially), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':fluents), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':events), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':prolog_events), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':actions), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':unserializable), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':update), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':initiate), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':terminate), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':in), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':at), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':during), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':from), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':to), op(0, xfy, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl': ::), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl': <-), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl': <-), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl': <=)]).

% /usr/lib/swipl/library/process.pl:0
% tryed(tmp:% module_dialect_lps('$BLOB'("<stream>(0x562ef33a6700)"), _50712, /.../(lps_user_examples, 'Samuel Contreras.pl'), _50716)).
:- thread_local % module_dialect_lps/4.
:- dynamic % module_dialect_lps/4.
:- volatile % module_dialect_lps/4.

% module_dialect_lps('$BLOB'("<stream>(0x562ef32ab000)"), '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl', '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl', [op(0, fy, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':not), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':then), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':if), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':if), op(0, xfy, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':else), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':terminates), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':initiates), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':updates), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':observe), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':false), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':initially), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':fluents), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':events), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':prolog_events), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':actions), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':unserializable), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':update), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':initiate), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':terminate), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':in), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':at), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':during), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':from), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':to), op(0, xfy, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl': ::), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl': <-), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl': <-), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl': <=)]).

% /usr/lib/swipl/library/process.pl:37
% tryed(tmp:% module_dialect_lps('$BLOB'("<stream>(0x562ef33a6700)"), _58650, /.../(lps_user_examples, 'Samuel Contreras.pl'), _58654)).
:- thread_local % module_dialect_lps/4.
:- dynamic % module_dialect_lps/4.
:- volatile % module_dialect_lps/4.

% module_dialect_lps('$BLOB'("<stream>(0x562ef32ab000)"), '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl', '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl', [op(0, fy, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':not), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':then), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':if), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':if), op(0, xfy, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':else), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':terminates), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':initiates), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':updates), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':observe), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':false), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':initially), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':fluents), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':events), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':prolog_events), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':actions), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':unserializable), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':update), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':initiate), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':terminate), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':in), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':at), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':during), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':from), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':to), op(0, xfy, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl': ::), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl': <-), op(0, fx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl': <-), op(0, xfx, '/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl': <=)]).

LPS version 21ef6da75fc8874032a83b114d06961df3effedc
Using dc:

 Simulation time is up. Unsolved goals:

** -1 cycles took 0.000410 seconds **
% /pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl:32
% pop_lps_dialect('$BLOB'("<stream>(0x562ef32ab000)"),  (/.../(lps_user_examples, 'Samuel Contreras.pl')-> /.../(lps_user_examples, 'Samuel Contreras.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/Samuel Contreras.pl':_73798).


:- dynamic used/1.

used(0).
used(0).

:- dynamic state/1.

state(engaña(bruja, niño)).
state(engaña(niño, bruja)).
state(real_time(1601499407.5697262)).
state(lps_user(unknown_user)).
state(lps_user(unknown_user, unknown_email)).
state(engaña(bruja, niño)).
state(real_time(1601499407.6215806)).
state(lps_user(unknown_user)).
state(lps_user(unknown_user, unknown_email)).

:- dynamic tried/3.


maxTime(8).

:- dynamic happens/3.


fluents([engaña(_, _)]).

:- dynamic lps_test_result/3.

lps_test_result(fluents, 0, 1).
lps_test_result(events, 1, 0).
lps_test_result(fluents, 1, 1).
lps_test_result(events, 2, 1).
lps_test_result(fluents, 2, 1).
lps_test_result(events, 3, 1).
lps_test_result(fluents, 3, 1).
lps_test_result(events, 4, 1).
lps_test_result(fluents, 4, 2).
lps_test_result(events, 5, 1).
lps_test_result(fluents, 5, 2).
lps_test_result(events, 6, 1).
lps_test_result(fluents, 6, 2).
lps_test_result(events, 7, 1).
lps_test_result(fluents, 7, 2).
lps_test_result(events, 8, 0).
lps_test_result(fluents, 8, 2).

l_events(happens(escapa(niño), A, A), [holds(engaña(niño, bruja), A)]).

:- dynamic actions/1.
:- multifile actions/1.

actions([ayuda(_, _), encuentra(_)]).

initiated(happens(encuentra(_), A, _), engaña(B, C), [holds(engaña(C, B), A)]).

:- dynamic current_goal/1.

current_goal(0).
current_goal(0).

:- dynamic lps_test_result_item/3.

lps_test_result_item(fluents, 0, engaña(bruja, niño)).
lps_test_result_item(fluents, 1, engaña(bruja, niño)).
lps_test_result_item(events, 2, necesita(bruja, objeto)).
lps_test_result_item(fluents, 2, engaña(bruja, niño)).
lps_test_result_item(events, 3, ayuda(niño, bruja)).
lps_test_result_item(fluents, 3, engaña(bruja, niño)).
lps_test_result_item(events, 4, encuentra(objeto)).
lps_test_result_item(fluents, 4, engaña(bruja, niño)).
lps_test_result_item(fluents, 4, engaña(niño, bruja)).
lps_test_result_item(events, 5, necesita(bruja, objeto)).
lps_test_result_item(fluents, 5, engaña(bruja, niño)).
lps_test_result_item(fluents, 5, engaña(niño, bruja)).
lps_test_result_item(events, 6, ayuda(niño, bruja)).
lps_test_result_item(fluents, 6, engaña(bruja, niño)).
lps_test_result_item(fluents, 6, engaña(niño, bruja)).
lps_test_result_item(events, 7, encuentra(objeto)).
lps_test_result_item(fluents, 7, engaña(bruja, niño)).
lps_test_result_item(fluents, 7, engaña(niño, bruja)).
lps_test_result_item(fluents, 8, engaña(bruja, niño)).
lps_test_result_item(fluents, 8, engaña(niño, bruja)).

:- dynamic next_state/1.


initial_state([engaña(bruja, niño)]).

:- dynamic lps_updating_current_state/0.


:- dynamic real_time_beginning/1.

real_time_beginning(1601499407.4535487).
real_time_beginning(1601499407.621502).

:- dynamic option/1.

option(swish).
option(dc).

reactive_rule([happens(necesita(bruja, objeto), _, _)], [happens(ayuda(niño, bruja), _, _)]).
reactive_rule([happens(ayuda(niño, bruja), _, _)], [happens(encuentra(objeto), _, _)]).
reactive_rule([happens(necesita(bruja, objeto), _, _)], [happens(ayuda(niño, bruja), _, _)]).
reactive_rule([happens(ayuda(niño, bruja), _, _)], [happens(encuentra(objeto), _, _)]).
reactive_rule([happens(encuentra(objeto), _, A), holds(engaña(niño, bruja), A)], [happens(escapa(niño), _, _)]).

:- dynamic depth/1.

depth(0).
depth(0).

:- dynamic current_time/1.

current_time(0).
current_time(10).

events([necesita(_, _), escapa(_)]).

observe([necesita(bruja, objeto)], 2).
observe([necesita(bruja, objeto)], 5).
% dB(/.../(lps_user_examples, 'Samuel Contreras.pl'), lps_visualization(_53206{groups:[_52030{content:"Events", id:"event", order:1}, _52104{content:"engaña(A,B)", id:"engaña/2", order:3, subgroupStack:"false"}, _52170{content:"Actions", id:"action", order:4}], items:[_52292{content:"bruja,niño", end:9, group:"engaña/2", id:0, start:1, subgroup:"bruja", title:"Fluent engaña(bruja,niño) initiated at 1<br/>and terminated at transition to 9"}, _52418{content:"niño,bruja", end:9, group:"engaña/2", id:1, start:4, subgroup:"niño", title:"Fluent engaña(niño,bruja) initiated at 4<br/>and terminated at transition to 9"}, _52544{content:"necesita(bruja,objeto)", group:"event", id:2, start:2, style:"color:#E19735", title:"happens(necesita(bruja,objeto),1,2)", type:"point"}, _52670{content:"ayuda(niño,bruja)", group:"action", id:3, start:3, style:"color:green", title:"happens(ayuda(niño,bruja),2,3)", type:"point"}, _52796{content:"encuentra(objeto)", group:"action", id:4, start:4, style:"color:green", title:"happens(encuentra(objeto),3,4)", type:"point"}, _52922{content:"necesita(bruja,objeto)", group:"event", id:5, start:5, style:"color:#E19735", title:"happens(necesita(bruja,objeto),4,5)", type:"point"}, _53048{content:"ayuda(niño,bruja)", group:"action", id:6, start:6, style:"color:green", title:"happens(ayuda(niño,bruja),5,6)", type:"point"}, _53174{content:"encuentra(objeto)", group:"action", id:7, start:7, style:"color:green", title:"happens(encuentra(objeto),6,7)", type:"point"}]}, [])).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'sdsd.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'sdsd.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/sdsd.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'sdsd.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'sdsd.pl'), lps= /.../(lps_user_examples, 'sdsd.pl'), using= /.../(lps_user_examples, 'sdsd.pl')].
% continue_lps_dialect.
% ops.

% LPS:  fluents(engaña(_19262,_19264)).
% Into: fluents([engaña(_19262,_19264)]).

% LPS:  actions((ayuda/2,encuentra/1,obtiene/1)).
% Into: actions([ayuda(_21672,_21674),encuentra(_21684),obtiene(_21694)]).

% LPS:  events((necesita/2,escapa/1)).
% Into: events([necesita(_22790,_22792),escapa(_22802)]).

% LPS:  initially(engaña(bruja,niño)).
% Into: initial_state([engaña(bruja,niño)]).

% LPS:  observe(from(necesita(bruja,objeto),to(1,2))).
% Into: observe([necesita(bruja,objeto)],2).

% LPS:  if(initiates(ayuda(_24946,_24948),engaña(_24946,_24948)),obtiene(objeto)).
% Into: initiated(happens(ayuda(_24946,_24948),_26240,_26246),engaña(_24946,_24948),[happens(obtiene(objeto),_26240,_26246)]).

% LPS:  if(escapa(niño),ayuda(niño,bruja)).
% Into: l_events(happens(escapa(niño),_27556,_27562),[happens(ayuda(niño,bruja),_27556,_27562)]).

% LPS:  then(if(from(necesita(bruja,objeto),to(_27634,_27636))),from(escapa(niño),to(_27636,_27796))).
% Into: reactive_rule([happens(necesita(bruja,objeto),_27634,_27636)],[happens(escapa(niño),_27636,_27796)]).
LPS version 21ef6da75fc8874032a83b114d06961df3effedc
Using dc:

 Simulation time is up. Unsolved goals:

** -1 cycles took 0.000451 seconds **
% /pack/logicmoo_ec/test/lps_user_examples/sdsd.pl:29
% pop_lps_dialect('$BLOB'("<stream>(0x562ef32abb00)"),  (/.../(lps_user_examples, 'sdsd.pl')-> /.../(lps_user_examples, 'sdsd.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/sdsd.pl':_73000).


:- dynamic used/1.

used(0).
used(0).

:- dynamic state/1.

state(engaña(bruja, niño)).
state(real_time(1601499407.8738532)).
state(lps_user(unknown_user)).
state(lps_user(unknown_user, unknown_email)).
state(engaña(bruja, niño)).
state(real_time(1601499407.8823154)).
state(lps_user(unknown_user)).
state(lps_user(unknown_user, unknown_email)).

:- dynamic tried/3.


maxTime(8).

:- dynamic happens/3.


fluents([engaña(_, _)]).

:- dynamic lps_test_result/3.

lps_test_result(fluents, 0, 1).
lps_test_result(events, 1, 0).
lps_test_result(fluents, 1, 1).
lps_test_result(events, 2, 1).
lps_test_result(fluents, 2, 1).
lps_test_result(events, 3, 1).
lps_test_result(fluents, 3, 1).
lps_test_result(events, 4, 0).
lps_test_result(fluents, 4, 1).
lps_test_result(events, 5, 0).
lps_test_result(fluents, 5, 1).
lps_test_result(events, 6, 0).
lps_test_result(fluents, 6, 1).
lps_test_result(events, 7, 0).
lps_test_result(fluents, 7, 1).
lps_test_result(events, 8, 0).
lps_test_result(fluents, 8, 1).

l_events(happens(escapa(niño), A, B), [happens(ayuda(niño, bruja), A, B)]).

:- dynamic lps_test_action_ancestor/3.

lps_test_action_ancestor(escapa(niño), 2, 3).

:- dynamic actions/1.
:- multifile actions/1.

actions([ayuda(_, _), encuentra(_), obtiene(_)]).

initiated(happens(ayuda(A, B), C, D), engaña(A, B), [happens(obtiene(objeto), C, D)]).

:- dynamic current_goal/1.

current_goal(0).
current_goal(0).

:- dynamic lps_test_result_item/3.

lps_test_result_item(fluents, 0, engaña(bruja, niño)).
lps_test_result_item(fluents, 1, engaña(bruja, niño)).
lps_test_result_item(events, 2, necesita(bruja, objeto)).
lps_test_result_item(fluents, 2, engaña(bruja, niño)).
lps_test_result_item(events, 3, ayuda(niño, bruja)).
lps_test_result_item(fluents, 3, engaña(bruja, niño)).
lps_test_result_item(fluents, 4, engaña(bruja, niño)).
lps_test_result_item(fluents, 5, engaña(bruja, niño)).
lps_test_result_item(fluents, 6, engaña(bruja, niño)).
lps_test_result_item(fluents, 7, engaña(bruja, niño)).
lps_test_result_item(fluents, 8, engaña(bruja, niño)).

:- dynamic next_state/1.


initial_state([engaña(bruja, niño)]).

:- dynamic lps_updating_current_state/0.


:- dynamic real_time_beginning/1.

real_time_beginning(1601499407.6736755).
real_time_beginning(1601499407.8822443).

:- dynamic option/1.

option(swish).
option(dc).

reactive_rule([happens(necesita(bruja, objeto), _, A)], [happens(escapa(niño), A, _)]).

:- dynamic depth/1.

depth(0).
depth(0).

:- dynamic current_time/1.

current_time(0).
current_time(10).

events([necesita(_, _), escapa(_)]).

observe([necesita(bruja, objeto)], 2).
% dB(/.../(lps_user_examples, 'sdsd.pl'), lps_visualization(_39800{groups:[_39254{content:"Events", id:"event", order:1}, _39328{content:"engaña(A,B)", id:"engaña/2", order:3, subgroupStack:"false"}, _39394{content:"Actions", id:"action", order:4}], items:[_39516{content:"bruja,niño", end:9, group:"engaña/2", id:0, start:1, subgroup:"bruja", title:"Fluent engaña(bruja,niño) initiated at 1<br/>and terminated at transition to 9"}, _39642{content:"necesita(bruja,objeto)", group:"event", id:1, start:2, style:"color:#E19735", title:"happens(necesita(bruja,objeto),1,2)", type:"point"}, _39768{content:"ayuda(niño,bruja)", group:"action", id:2, start:3, style:"color:green", title:"happens(ayuda(niño,bruja),2,3)", type:"point"}]}, [])).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'search.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'search.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/search.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'search.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'search.pl'), lps= /.../(lps_user_examples, 'search.pl'), using= /.../(lps_user_examples, 'search.pl')].
% continue_lps_dialect.
% ops.

% LPS:  events((g/2,gx/1,o)).
% Into: events([g(_20526,_20528),gx(_20538),o]).

% LPS:  actions(s/1).
% Into: actions([s(_21512)]).

% LPS:  observe(from(o,to(1,2))).
% Into: observe([o],2).

% LPS:  if(g(_22620,_22622),(_22620\==_22622,c(_22620,_22742),g(_22620,_22742),g(_22742,_22622))).
% Into: l_events(happens(g(_22620,_22622),_24040,_24046),[_22620\==_22622,c(_22620,_22742),happens(g(_22620,_22742),_24040,_24812),happens(g(_22742,_22622),_24812,_24046)]).

% LPS:  if(g(_24826,_24826),s(_24826)).
% Into: l_events(happens(g(_24826,_24826),_25896,_25902),[happens(s(_24826),_25896,_25902)]).

% LPS:  if(from(gx(_25914),to(_25950,_25952)),(c(_26064,_25914),from(gx(_26064),to(_25950,_25952)),s(_25914))).
% Into: l_events(happens(gx(_25914),_25950,_25952),[c(_26064,_25914),happens(gx(_26064),_25950,_25952),happens(s(_25914),_27822,_27508)]).

% LPS:  if(from(gx(_27752),to(_27788,_27790)),from(s(_27752),to(_27788,_27790))).
% Into: l_events(happens(gx(_27752),_27788,_27790),[happens(s(_27752),_27788,_27790)]).

% LPS:  then(if(from(o,to(_34480,_34482))),from(gx(5),to(_34482,_34642))).
% Into: reactive_rule([happens(o,_34480,_34482)],[happens(gx(5),_34482,_34642)]).
% /pack/logicmoo_ec/test/lps_user_examples/search.pl:28
% pop_lps_dialect('$BLOB'("<stream>(0x562ef33a8000)"),  (/.../(lps_user_examples, 'search.pl')-> /.../(lps_user_examples, 'search.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/search.pl':_43462).


reactive_rule([happens(o, _, A)], [happens(gx(5), A, _)]).

l_events(happens(g(A, B), C, D), [A\==B, c(A, E), happens(g(A, E), C, F), happens(g(E, B), F, D)]).
l_events(happens(g(A, A), B, C), [happens(s(A), B, C)]).
l_events(happens(gx(A), B, C), [c(D, A), happens(gx(D), B, C), happens(s(A), _, _)]).
l_events(happens(gx(A), B, C), [happens(s(A), B, C)]).

:- dynamic actions/1.
:- multifile actions/1.

actions([s(_)]).

events([g(_, _), gx(_), o]).

observe([o], 2).

maxTime(10).

c(1, 2).
c(2, 3).
c(3, 4).
c(4, 5).
% dB(/.../(lps_user_examples, 'search.pl'), lps_visualization(_70572{groups:[_69726{content:"Events", id:"event", order:1}, _69788{content:"Actions", id:"action", order:4}], items:[_69910{content:"o", group:"event", id:0, start:2, style:"color:#E19735", title:"happens(o,1,2)", type:"point"}, _70036{content:"s(1)", group:"action", id:1, start:3, style:"color:green", title:"happens(s(1),2,3)", type:"point"}, _70162{content:"s(2)", group:"action", id:2, start:3, style:"color:green", title:"happens(s(2),2,3)", type:"point"}, _70288{content:"s(3)", group:"action", id:3, start:3, style:"color:green", title:"happens(s(3),2,3)", type:"point"}, _70414{content:"s(4)", group:"action", id:4, start:3, style:"color:green", title:"happens(s(4),2,3)", type:"point"}, _70540{content:"s(5)", group:"action", id:5, start:3, style:"color:green", title:"happens(s(5),2,3)", type:"point"}]}, [])).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'sel-driving cars.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'sel-driving cars.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/sel-driving cars.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'sel-driving cars.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'sel-driving cars.pl'), lps= /.../(lps_user_examples, 'sel-driving cars.pl'), using= /.../(lps_user_examples, 'sel-driving cars.pl')].
% continue_lps_dialect.
% ops.

% LPS:  fluents(location(_19264,_19266,_19268)).
% Into: fluents([location(_19264,_19266,_19268)]).

% LPS:  events(destination(_20348,_20350)).
% Into: events([destination(_20348,_20350)]).

% LPS:  actions((step(_21402),turn(_21402,_21458))).
% Into: actions([step(_21402),turn(_21402,_21458)]).

% LPS:  if(updates(step(_22544),in(to(_22580,_22582),location(_22544,_22580,_22680))),next(_22580,_22680,_22582)).
% Into: updated(happens(step(_22544),_24064,_24070),location(_22544,_22580,_22680),_22580-_22582,[next(_22580,_22680,_22582)]).

% LPS:  updates(turn(_32264,_32266),in(to(_32302,_32266),location(_32264,_32400,_32302))).
% Into: updated(happens(turn(_32264,_32266),_33654,_33660),location(_32264,_32400,_32302),_32302-_32266,[]).

% LPS:  initially((location(mycar,2-1,northward),location(yourcar,9-9,westward))).
% Into: initial_state([location(mycar,2-1,northward),location(yourcar,9-9,westward)]).

% LPS:  observe(from(destination(mycar,9-9),to(2,3))).
% Into: observe([destination(mycar,9-9)],3).

% LPS:  observe(from(destination(yourcar,2-1),to(3,4))).
% Into: observe([destination(yourcar,2-1)],4).

% LPS:  then(if((to(destination(_50646,_50648),_50670),at(location(_50646,_50766,_50768),_50670),directions(_50766,_50886,_50648))),from(drive(_50646,_50886,_50648),to(_50670,_51086))).
% Into: reactive_rule([happens(destination(_50646,_50648),_52318,_50670),holds(location(_50646,_50766,_50768),_50670),directions(_50766,_50886,_50648)],[happens(drive(_50646,_50886,_50648),_50670,_51086)]).

% LPS:  if(from(drive(_53782,_53784,_53786),to(_53822,_53822)),at(location(_53782,_53786,_53954),_53822)).
% Into: l_events(happens(drive(_53782,_53784,_53786),_53822,_53822),[holds(location(_53782,_53786,_53954),_53822)]).

% LPS:  if(from(drive(_55514,[_55454-_55456|_55488],_55518),to(_55554,_55556)),(location(_55514,_55684,_55454),on(_55684,_55456),next(_55684,_55454,_55814),on(_55814,_55456),from(step(_55514),to(_55554,_55948)),from(drive(_55514,[_55454-_55456|_55488],_55518),to(_55948,_55556)))).
% Into: l_events(happens(drive(_55514,[_55454-_55456|_55488],_55518),_55554,_55556),[holds(location(_55514,_55684,_55454),_57772),on(_55684,_55456),next(_55684,_55454,_55814),on(_55814,_55456),happens(step(_55514),_55554,_55948),happens(drive(_55514,[_55454-_55456|_55488],_55518),_55948,_55556)]).

% LPS:  if(from(drive(_58674,[_58546-_58548,_58614-_58616|_58648],_58678),to(_58714,_58716)),(location(_58674,_58844,_58546),on(_58844,_58548),on(_58844,_58616),from(turn(_58674,_58614),to(_58714,_59052)),from(drive(_58674,[_58614-_58616|_58648],_58678),to(_59052,_58716)))).
% Into: l_events(happens(drive(_58674,[_58546-_58548,_58614-_58616|_58648],_58678),_58714,_58716),[holds(location(_58674,_58844,_58546),_60856),on(_58844,_58548),on(_58844,_58616),happens(turn(_58674,_58614),_58714,_59052),happens(drive(_58674,[_58614-_58616|_58648],_58678),_59052,_58716)]).
% /pack/logicmoo_ec/test/lps_user_examples/sel-driving cars.pl:77
% pop_lps_dialect('$BLOB'("<stream>(0x562ef33a6d00)"),  (/.../(lps_user_examples, 'sel-driving cars.pl')-> /.../(lps_user_examples, 'sel-driving cars.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/sel-driving cars.pl':_30514).


d(location(mycar, X-Y, Heading), [type:circle, center:[XX, YY], radius:5, fillColor:blue]) :-
    XX is X*10,
    YY is Y*10.
d(location(yourcar, X-Y, Heading), [type:circle, center:[XX, YY], radius:5, fillColor:red]) :-
    XX is X*10,
    YY is Y*10.
d(timeless, [type:rectangle, fillColor:yellow, from:[A, B], to:[_, _]]) :-
    1=<C,
    C=<10,
    1=<D,
    D=<10,
    not(on(C-D, _)),
    A is C*10,
    B is D*10,
    _ is A+10,
    _ is B+10.

d(timeless, [type:rectangle, fillColor:yellow, from:[20, 20], to:[30, 30]], sendToBack).

fluents([location(_, _, _)]).

next(X-Y1, northward, X-Y2) :-
    Y2 is Y1+1.
next(X-Y1, southward, X-Y2) :-
    Y2 is Y1-1.
next(X1-Y, eastward, X2-Y) :-
    X2 is X1+1.
next(X1-Y, westward, X2-Y) :-
    X2 is X1-1.

directions(2-1, [northward-westStreet, eastward-northStreet], 9-9).
directions(9-9, [westward-northStreet, southward-westStreet], 2-1).

reactive_rule([happens(destination(A, B), _, C), holds(location(A, D, _), C), directions(D, E, B)], [happens(drive(A, E, B), C, _)]).

initial_state([location(mycar, 2-1, northward), location(yourcar, 9-9, westward)]).

l_events(happens(drive(A, _, B), C, C), [holds(location(A, B, _), C)]).
l_events(happens(drive(A, [B-C|D], E), F, G), [holds(location(A, H, B), _), on(H, C), next(H, B, I), on(I, C), happens(step(A), F, J), happens(drive(A, [B-C|D], E), J, G)]).
l_events(happens(drive(A, [B-C, D-E|F], G), H, I), [holds(location(A, J, B), _), on(J, C), on(J, E), happens(turn(A, D), H, K), happens(drive(A, [D-E|F], G), K, I)]).

:- dynamic actions/1.
:- multifile actions/1.

actions([step(A), turn(A, _)]).

on(X-5, mainStreet) :-
    3=<X,
    X=<8.
on(X-9, northStreet) :-
    2=<X,
    X=<9.
on(6-Y, highStreet) :-
    1=<Y,
    Y=<9.
on(2-Y, westStreet) :-
    1=<Y,
    Y=<9.
on(7-Y, eastStreet) :-
    1=<Y,
    Y=<9.

events([destination(_, _)]).

updated(happens(step(A), _, _), location(A, B, C), B-D, [next(B, C, D)]).
updated(happens(turn(A, B), _, _), location(A, _, C), C-B, []).

maxTime(20).

observe([destination(mycar, 9-9)], 3).
observe([destination(yourcar, 2-1)], 4).
% get2react([notc,"NickServ","NickServ@services.","*","You are already logged in as \002\PrologMUD\002\.","PrologMUD"]).
% irc_receive("NickServ","NickServ@services.","*","PrologMUD",ctcp("NOTICE","You are already logged in as \002\PrologMUD\002\.")).
% unused(ircEventNow("PrologMUD","nickserv",ctcp("NOTICE","You are already logged in as \002\PrologMUD\002\."))).
% dB(/.../(lps_user_examples, 'sel-driving cars.pl'), lps_visualization(_189458{groups:[_180722{content:"Events", id:"event", order:1}, _180796{content:"location(A,B,C)", id:"location/3", order:3, subgroupStack:"false"}, _180862{content:"Actions", id:"action", order:4}], items:[_180984{content:"mycar,2-1,northward", end:4, group:"location/3", id:0, start:1, subgroup:"mycar", title:"Fluent location(mycar,2-1,northward) initiated at 1<br/>and terminated at transition to 4"}, _181110{content:"mycar,2-2,northward", end:5, group:"location/3", id:1, start:4, subgroup:"mycar", title:"Fluent location(mycar,2-2,northward) initiated at 4<br/>and terminated at transition to 5"}, _181236{content:"mycar,2-3,northward", end:6, group:"location/3", id:2, start:5, subgroup:"mycar", title:"Fluent location(mycar,2-3,northward) initiated at 5<br/>and terminated at transition to 6"}, _181362{content:"mycar,2-4,northward", end:7, group:"location/3", id:3, start:6, subgroup:"mycar", title:"Fluent location(mycar,2-4,northward) initiated at 6<br/>and terminated at transition to 7"}, _181488{content:"mycar,2-5,northward", end:8, group:"location/3", id:4, start:7, subgroup:"mycar", title:"Fluent location(mycar,2-5,northward) initiated at 7<br/>and terminated at transition to 8"}, _181614{content:"mycar,2-6,northward", end:9, group:"location/3", id:5, start:8, subgroup:"mycar", title:"Fluent location(mycar,2-6,northward) initiated at 8<br/>and terminated at transition to 9"}, _181740{content:"mycar,2-7,northward", end:10, group:"location/3", id:6, start:9, subgroup:"mycar", title:"Fluent location(mycar,2-7,northward) initiated at 9<br/>and terminated at transition to 10"}, _181866{content:"mycar,2-8,northward", end:11, group:"location/3", id:7, start:10, subgroup:"mycar", title:"Fluent location(mycar,2-8,northward) initiated at 10<br/>and terminated at transition to 11"}, _181992{content:"mycar,2-9,eastward", end:13, group:"location/3", id:8, start:12, subgroup:"mycar", title:"Fluent location(mycar,2-9,eastward) initiated at 12<br/>and terminated at transition to 13"}, _182118{content:"mycar,2-9,northward", end:12, group:"location/3", id:9, start:11, subgroup:"mycar", title:"Fluent location(mycar,2-9,northward) initiated at 11<br/>and terminated at transition to 12"}, _182244{content:"mycar,3-9,eastward", end:14, group:"location/3", id:10, start:13, subgroup:"mycar", title:"Fluent location(mycar,3-9,eastward) initiated at 13<br/>and terminated at transition to 14"}, _182370{content:"mycar,4-9,eastward", end:15, group:"location/3", id:11, start:14, subgroup:"mycar", title:"Fluent location(mycar,4-9,eastward) initiated at 14<br/>and terminated at transition to 15"}, _182496{content:"mycar,5-9,eastward", end:16, group:"location/3", id:12, start:15, subgroup:"mycar", title:"Fluent location(mycar,5-9,eastward) initiated at 15<br/>and terminated at transition to 16"}, _182622{content:"mycar,6-9,eastward", end:17, group:"location/3", id:13, start:16, subgroup:"mycar", title:"Fluent location(mycar,6-9,eastward) initiated at 16<br/>and terminated at transition to 17"}, _182748{content:"mycar,7-9,eastward", end:18, group:"location/3", id:14, start:17, subgroup:"mycar", title:"Fluent location(mycar,7-9,eastward) initiated at 17<br/>and terminated at transition to 18"}, _182874{content:"mycar,8-9,eastward", end:19, group:"location/3", id:15, start:18, subgroup:"mycar", title:"Fluent location(mycar,8-9,eastward) initiated at 18<br/>and terminated at transition to 19"}, _183000{content:"mycar,9-9,eastward", end:21, group:"location/3", id:16, start:19, subgroup:"mycar", title:"Fluent location(mycar,9-9,eastward) initiated at 19<br/>and terminated at transition to 21"}, _183126{content:"yourcar,2-1,southward", end:21, group:"location/3", id:17, start:20, subgroup:"yourcar", title:"Fluent location(yourcar,2-1,southward) initiated at 20<br/>and terminated at transition to 21"}, _183252{content:"yourcar,2-2,southward", end:20, group:"location/3", id:18, start:19, subgroup:"yourcar", title:"Fluent location(yourcar,2-2,southward) initiated at 19<br/>and terminated at transition to 20"}, _183378{content:"yourcar,2-3,southward", end:19, group:"location/3", id:19, start:18, subgroup:"yourcar", title:"Fluent location(yourcar,2-3,southward) initiated at 18<br/>and terminated at transition to 19"}, _183504{content:"yourcar,2-4,southward", end:18, group:"location/3", id:20, start:17, subgroup:"yourcar", title:"Fluent location(yourcar,2-4,southward) initiated at 17<br/>and terminated at transition to 18"}, _183630{content:"yourcar,2-5,southward", end:17, group:"location/3", id:21, start:16, subgroup:"yourcar", title:"Fluent location(yourcar,2-5,southward) initiated at 16<br/>and terminated at transition to 17"}, _183756{content:"yourcar,2-6,southward", end:16, group:"location/3", id:22, start:15, subgroup:"yourcar", title:"Fluent location(yourcar,2-6,southward) initiated at 15<br/>and terminated at transition to 16"}, _183882{content:"yourcar,2-7,southward", end:15, group:"location/3", id:23, start:14, subgroup:"yourcar", title:"Fluent location(yourcar,2-7,southward) initiated at 14<br/>and terminated at transition to 15"}, _184008{content:"yourcar,2-8,southward", end:14, group:"location/3", id:24, start:13, subgroup:"yourcar", title:"Fluent location(yourcar,2-8,southward) initiated at 13<br/>and terminated at transition to 14"}, _184134{content:"yourcar,2-9,southward", end:13, group:"location/3", id:25, start:12, subgroup:"yourcar", title:"Fluent location(yourcar,2-9,southward) initiated at 12<br/>and terminated at transition to 13"}, _184260{content:"yourcar,2-9,westward", end:12, group:"location/3", id:26, start:11, subgroup:"yourcar", title:"Fluent location(yourcar,2-9,westward) initiated at 11<br/>and terminated at transition to 12"}, _184386{content:"yourcar,3-9,westward", end:11, group:"location/3", id:27, start:10, subgroup:"yourcar", title:"Fluent location(yourcar,3-9,westward) initiated at 10<br/>and terminated at transition to 11"}, _184512{content:"yourcar,4-9,westward", end:10, group:"location/3", id:28, start:9, subgroup:"yourcar", title:"Fluent location(yourcar,4-9,westward) initiated at 9<br/>and terminated at transition to 10"}, _184638{content:"yourcar,5-9,westward", end:9, group:"location/3", id:29, start:8, subgroup:"yourcar", title:"Fluent location(yourcar,5-9,westward) initiated at 8<br/>and terminated at transition to 9"}, _184764{content:"yourcar,6-9,westward", end:8, group:"location/3", id:30, start:7, subgroup:"yourcar", title:"Fluent location(yourcar,6-9,westward) initiated at 7<br/>and terminated at transition to 8"}, _184890{content:"yourcar,7-9,westward", end:7, group:"location/3", id:31, start:6, subgroup:"yourcar", title:"Fluent location(yourcar,7-9,westward) initiated at 6<br/>and terminated at transition to 7"}, _185016{content:"yourcar,8-9,westward", end:6, group:"location/3", id:32, start:5, subgroup:"yourcar", title:"Fluent location(yourcar,8-9,westward) initiated at 5<br/>and terminated at transition to 6"}, _185142{content:"yourcar,9-9,westward", end:5, group:"location/3", id:33, start:1, subgroup:"yourcar", title:"Fluent location(yourcar,9-9,westward) initiated at 1<br/>and terminated at transition to 5"}, _185268{content:"destination(mycar,9-9)", group:"event", id:34, start:3, style:"color:#E19735", title:"happens(destination(mycar,9-9),2,3)", type:"point"}, _185394{content:"destination(yourcar,2-1)", group:"event", id:35, start:4, style:"color:#E19735", title:"happens(destination(yourcar,2-1),3,4)", type:"point"}, _185520{content:"step(mycar)", group:"action", id:36, start:4, style:"color:green", title:"happens(step(mycar),3,4)", type:"point"}, _185646{content:"step(mycar)", group:"action", id:37, start:5, style:"color:green", title:"happens(step(mycar),4,5)", type:"point"}, _185772{content:"step(yourcar)", group:"action", id:38, start:5, style:"color:green", title:"happens(step(yourcar),4,5)", type:"point"}, _185898{content:"step(yourcar)", group:"action", id:39, start:6, style:"color:green", title:"happens(step(yourcar),5,6)", type:"point"}, _186024{content:"step(mycar)", group:"action", id:40, start:6, style:"color:green", title:"happens(step(mycar),5,6)", type:"point"}, _186150{content:"step(mycar)", group:"action", id:41, start:7, style:"color:green", title:"happens(step(mycar),6,7)", type:"point"}, _186276{content:"step(yourcar)", group:"action", id:42, start:7, style:"color:green", title:"happens(step(yourcar),6,7)", type:"point"}, _186402{content:"step(yourcar)", group:"action", id:43, start:8, style:"color:green", title:"happens(step(yourcar),7,8)", type:"point"}, _186528{content:"step(mycar)", group:"action", id:44, start:8, style:"color:green", title:"happens(step(mycar),7,8)", type:"point"}, _186654{content:"step(mycar)", group:"action", id:45, start:9, style:"color:green", title:"happens(step(mycar),8,9)", type:"point"}, _186780{content:"step(yourcar)", group:"action", id:46, start:9, style:"color:green", title:"happens(step(yourcar),8,9)", type:"point"}, _186906{content:"step(yourcar)", group:"action", id:47, start:10, style:"color:green", title:"happens(step(yourcar),9,10)", type:"point"}, _187032{content:"step(mycar)", group:"action", id:48, start:10, style:"color:green", title:"happens(step(mycar),9,10)", type:"point"}, _187158{content:"step(mycar)", group:"action", id:49, start:11, style:"color:green", title:"happens(step(mycar),10,11)", type:"point"}, _187284{content:"step(yourcar)", group:"action", id:50, start:11, style:"color:green", title:"happens(step(yourcar),10,11)", type:"point"}, _187410{content:"turn(yourcar,southward)", group:"action", id:51, start:12, style:"color:green", title:"happens(turn(yourcar,southward),11,12)", type:"point"}, _187536{content:"turn(mycar,eastward)", group:"action", id:52, start:12, style:"color:green", title:"happens(turn(mycar,eastward),11,12)", type:"point"}, _187662{content:"step(mycar)", group:"action", id:53, start:13, style:"color:green", title:"happens(step(mycar),12,13)", type:"point"}, _187788{content:"step(yourcar)", group:"action", id:54, start:13, style:"color:green", title:"happens(step(yourcar),12,13)", type:"point"}, _187914{content:"step(yourcar)", group:"action", id:55, start:14, style:"color:green", title:"happens(step(yourcar),13,14)", type:"point"}, _188040{content:"step(mycar)", group:"action", id:56, start:14, style:"color:green", title:"happens(step(mycar),13,14)", type:"point"}, _188166{content:"step(mycar)", group:"action", id:57, start:15, style:"color:green", title:"happens(step(mycar),14,15)", type:"point"}, _188292{content:"step(yourcar)", group:"action", id:58, start:15, style:"color:green", title:"happens(step(yourcar),14,15)", type:"point"}, _188418{content:"step(yourcar)", group:"action", id:59, start:16, style:"color:green", title:"happens(step(yourcar),15,16)", type:"point"}, _188544{content:"step(mycar)", group:"action", id:60, start:16, style:"color:green", title:"happens(step(mycar),15,16)", type:"point"}, _188670{content:"step(mycar)", group:"action", id:61, start:17, style:"color:green", title:"happens(step(mycar),16,17)", type:"point"}, _188796{content:"step(yourcar)", group:"action", id:62, start:17, style:"color:green", title:"happens(step(yourcar),16,17)", type:"point"}, _188922{content:"step(yourcar)", group:"action", id:63, start:18, style:"color:green", title:"happens(step(yourcar),17,18)", type:"point"}, _189048{content:"step(mycar)", group:"action", id:64, start:18, style:"color:green", title:"happens(step(mycar),17,18)", type:"point"}, _189174{content:"step(mycar)", group:"action", id:65, start:19, style:"color:green", title:"happens(step(mycar),18,19)", type:"point"}, _189300{content:"step(yourcar)", group:"action", id:66, start:19, style:"color:green", title:"happens(step(yourcar),18,19)", type:"point"}, _189426{content:"step(yourcar)", group:"action", id:67, start:20, style:"color:green", title:"happens(step(yourcar),19,20)", type:"point"}]}, _247852{cycles:[[_242414{create:_242390{center:[20, 10], fillColor:"blue", id:"location(mycar,2-1,northward)", radius:5, type:"circle"}}, _242544{create:_242520{center:[90, 90], fillColor:"red", id:"location(yourcar,9-9,westward)", radius:5, type:"circle"}}], [], [], [_242692{create:_242668{center:[20, 20], fillColor:"blue", id:"location(mycar,2-2,northward)", radius:5, type:"circle"}}, _242722{kill:"location(mycar,2-1,northward)"}], [_242858{create:_242834{center:[20, 30], fillColor:"blue", id:"location(mycar,2-3,northward)", radius:5, type:"circle"}}, _242988{create:_242964{center:[80, 90], fillColor:"red", id:"location(yourcar,8-9,westward)", radius:5, type:"circle"}}, _243018{kill:"location(mycar,2-2,northward)"}, _243048{kill:"location(yourcar,9-9,westward)"}], [_243184{create:_243160{center:[20, 40], fillColor:"blue", id:"location(mycar,2-4,northward)", radius:5, type:"circle"}}, _243314{create:_243290{center:[70, 90], fillColor:"red", id:"location(yourcar,7-9,westward)", radius:5, type:"circle"}}, _243344{kill:"location(mycar,2-3,northward)"}, _243374{kill:"location(yourcar,8-9,westward)"}], [_243510{create:_243486{center:[20, 50], fillColor:"blue", id:"location(mycar,2-5,northward)", radius:5, type:"circle"}}, _243640{create:_243616{center:[60, 90], fillColor:"red", id:"location(yourcar,6-9,westward)", radius:5, type:"circle"}}, _243670{kill:"location(mycar,2-4,northward)"}, _243700{kill:"location(yourcar,7-9,westward)"}], [_243836{create:_243812{center:[20, 60], fillColor:"blue", id:"location(mycar,2-6,northward)", radius:5, type:"circle"}}, _243966{create:_243942{center:[50, 90], fillColor:"red", id:"location(yourcar,5-9,westward)", radius:5, type:"circle"}}, _243996{kill:"location(mycar,2-5,northward)"}, _244026{kill:"location(yourcar,6-9,westward)"}], [_244162{create:_244138{center:[20, 70], fillColor:"blue", id:"location(mycar,2-7,northward)", radius:5, type:"circle"}}, _244292{create:_244268{center:[40, 90], fillColor:"red", id:"location(yourcar,4-9,westward)", radius:5, type:"circle"}}, _244322{kill:"location(mycar,2-6,northward)"}, _244352{kill:"location(yourcar,5-9,westward)"}], [_244488{create:_244464{center:[20, 80], fillColor:"blue", id:"location(mycar,2-8,northward)", radius:5, type:"circle"}}, _244618{create:_244594{center:[30, 90], fillColor:"red", id:"location(yourcar,3-9,westward)", radius:5, type:"circle"}}, _244648{kill:"location(mycar,2-7,northward)"}, _244678{kill:"location(yourcar,4-9,westward)"}], [_244814{create:_244790{center:[20, 90], fillColor:"blue", id:"location(mycar,2-9,northward)", radius:5, type:"circle"}}, _244944{create:_244920{center:[20, 90], fillColor:"red", id:"location(yourcar,2-9,westward)", radius:5, type:"circle"}}, _244974{kill:"location(mycar,2-8,northward)"}, _245004{kill:"location(yourcar,3-9,westward)"}], [_245140{create:_245116{center:[20, 90], fillColor:"blue", id:"location(mycar,2-9,eastward)", radius:5, type:"circle"}}, _245270{create:_245246{center:[20, 90], fillColor:"red", id:"location(yourcar,2-9,southward)", radius:5, type:"circle"}}, _245300{kill:"location(mycar,2-9,northward)"}, _245330{kill:"location(yourcar,2-9,westward)"}], [_245466{create:_245442{center:[30, 90], fillColor:"blue", id:"location(mycar,3-9,eastward)", radius:5, type:"circle"}}, _245596{create:_245572{center:[20, 80], fillColor:"red", id:"location(yourcar,2-8,southward)", radius:5, type:"circle"}}, _245626{kill:"location(mycar,2-9,eastward)"}, _245656{kill:"location(yourcar,2-9,southward)"}], [_245792{create:_245768{center:[40, 90], fillColor:"blue", id:"location(mycar,4-9,eastward)", radius:5, type:"circle"}}, _245922{create:_245898{center:[20, 70], fillColor:"red", id:"location(yourcar,2-7,southward)", radius:5, type:"circle"}}, _245952{kill:"location(mycar,3-9,eastward)"}, _245982{kill:"location(yourcar,2-8,southward)"}], [_246118{create:_246094{center:[50, 90], fillColor:"blue", id:"location(mycar,5-9,eastward)", radius:5, type:"circle"}}, _246248{create:_246224{center:[20, 60], fillColor:"red", id:"location(yourcar,2-6,southward)", radius:5, type:"circle"}}, _246278{kill:"location(mycar,4-9,eastward)"}, _246308{kill:"location(yourcar,2-7,southward)"}], [_246444{create:_246420{center:[60, 90], fillColor:"blue", id:"location(mycar,6-9,eastward)", radius:5, type:"circle"}}, _246574{create:_246550{center:[20, 50], fillColor:"red", id:"location(yourcar,2-5,southward)", radius:5, type:"circle"}}, _246604{kill:"location(mycar,5-9,eastward)"}, _246634{kill:"location(yourcar,2-6,southward)"}], [_246770{create:_246746{center:[70, 90], fillColor:"blue", id:"location(mycar,7-9,eastward)", radius:5, type:"circle"}}, _246900{create:_246876{center:[20, 40], fillColor:"red", id:"location(yourcar,2-4,southward)", radius:5, type:"circle"}}, _246930{kill:"location(mycar,6-9,eastward)"}, _246960{kill:"location(yourcar,2-5,southward)"}], [_247096{create:_247072{center:[80, 90], fillColor:"blue", id:"location(mycar,8-9,eastward)", radius:5, type:"circle"}}, _247226{create:_247202{center:[20, 30], fillColor:"red", id:"location(yourcar,2-3,southward)", radius:5, type:"circle"}}, _247256{kill:"location(mycar,7-9,eastward)"}, _247286{kill:"location(yourcar,2-4,southward)"}], [_247422{create:_247398{center:[90, 90], fillColor:"blue", id:"location(mycar,9-9,eastward)", radius:5, type:"circle"}}, _247552{create:_247528{center:[20, 20], fillColor:"red", id:"location(yourcar,2-2,southward)", radius:5, type:"circle"}}, _247582{kill:"location(mycar,8-9,eastward)"}, _247612{kill:"location(yourcar,2-3,southward)"}], [_247748{create:_247724{center:[20, 10], fillColor:"red", id:"location(yourcar,2-1,southward)", radius:5, type:"circle"}}, _247778{kill:"location(yourcar,2-2,southward)"}], [_247814{kill:"location(mycar,9-9,eastward)"}, _247844{kill:"location(yourcar,2-1,southward)"}]]})).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'self-drivers.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'self-drivers.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/self-drivers.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'self-drivers.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'self-drivers.pl'), lps= /.../(lps_user_examples, 'self-drivers.pl'), using= /.../(lps_user_examples, 'self-drivers.pl')].
% continue_lps_dialect.
% ops.

% LPS:  fluents((location(_81626,_81628,_81630),collisionWarning(_81698,_81700,_81628),rightOfWay(_81698,_81700,_81628),collisionPossible(_81698,_81700))).
% Into: fluents([location(_81626,_81628,_81630),collisionWarning(_81698,_81700,_81628),rightOfWay(_81698,_81700,_81628),collisionPossible(_81698,_81700)]).

% LPS:  events((destination(_83070,_83072),remove(_83070))).
% Into: events([destination(_83070,_83072),remove(_83070)]).

% LPS:  actions((step(_84234,_84236,_84238),turn(_84234,_84294))).
% Into: actions([step(_84234,_84236,_84238),turn(_84234,_84294)]).

% LPS:  initially((location(mycar,2-1,northward),location(yourcar,6-1,northward),location(othercar,10-5,westward),location(troubleMaker,6-2,northward),location(brokencar,2-7,noward))).
% Into: initial_state([location(mycar,2-1,northward),location(yourcar,6-1,northward),location(othercar,10-5,westward),location(troubleMaker,6-2,northward),location(brokencar,2-7,noward)]).

% LPS:  observe(from(destination(mycar,9-9),to(2,3))).
% Into: observe([destination(mycar,9-9)],3).

% LPS:  observe(from(destination(troubleMaker,9-9),to(2,3))).
% Into: observe([destination(troubleMaker,9-9)],3).

% LPS:  observe(from(destination(yourcar,9-9),to(2,3))).
% Into: observe([destination(yourcar,9-9)],3).

% LPS:  observe(from(destination(othercar,6-1),to(1,2))).
% Into: observe([destination(othercar,6-1)],2).

% LPS:  observe(from(remove(brokencar),to(15,16))).
% Into: observe([remove(brokencar)],16).

% LPS:  terminates(remove(_93240),location(_93240,_93310,_93312)).
% Into: terminated(happens(remove(_93240),_94428,_94434),location(_93240,_93310,_93312),[]).

% LPS:  then(if((to(destination(_94446,_94448),_94470),at(location(_94446,_94566,_94568),_94470))),(directions(_94566,_94742,_94448),from(drive(_94446,_94742,_94448),_94470))).
% Into: reactive_rule([happens(destination(_94446,_94448),_96052,_94470),holds(location(_94446,_94566,_94568),_94470)],[directions(_94566,_94742,_94448),happens(drive(_94446,_94742,_94448),_94470,_96624)]).

% LPS:  if(from(drive(_19082,[_19034-_19036],_19086),to(_19102,_19104)),(at(location(_19082,_19212,_19034),_19102),_19212\=_19086,next(_19212,_19034,_19348),on(_19348,_19036),from(step(_19082,_19212,_19348),to(_19462,_19464)),from(drive(_19082,[_19034-_19036],_19086),to(_19464,_19104)))).
% Into: l_events(happens(drive(_19082,[_19034-_19036],_19086),_19102,_19104),[holds(location(_19082,_19212,_19034),_19102),_19212\=_19086,next(_19212,_19034,_19348),on(_19348,_19036),happens(step(_19082,_19212,_19348),_19462,_19464),happens(drive(_19082,[_19034-_19036],_19086),_19464,_19104)]).

% LPS:  if(from(drive(_21974,[_21846-_21848,_21914-_21916|_21948],_21978),to(_22014,_22016)),(location(_21974,_22144,_21846),next(_22144,_21846,_22218),not(on(_22218,_21916)),from(step(_21974,_22144,_22218),to(_22414,_22416)),from(drive(_21974,[_21846-_21848,_21914-_21916|_21948],_21978),to(_22416,_22016)))).
% Into: l_events(happens(drive(_21974,[_21846-_21848,_21914-_21916|_21948],_21978),_22014,_22016),[holds(location(_21974,_22144,_21846),_24332),next(_22144,_21846,_22218),not(on(_22218,_21916)),happens(step(_21974,_22144,_22218),_22414,_22416),happens(drive(_21974,[_21846-_21848,_21914-_21916|_21948],_21978),_22416,_22016)]).

% LPS:  if(from(drive(_25372,[_25244-_25246,_25312-_25314|_25346],_25376),to(_25412,_25414)),(location(_25372,_25542,_25244),next(_25542,_25244,_25616),on(_25616,_25314),from(step(_25372,_25542,_25616),to(_25780,_25782)),from(turn(_25372,_25312),to(_25780,_25782)),from(drive(_25372,[_25312-_25314|_25346],_25376),to(_25782,_25414)))).
% Into: l_events(happens(drive(_25372,[_25244-_25246,_25312-_25314|_25346],_25376),_25412,_25414),[holds(location(_25372,_25542,_25244),_27822),next(_25542,_25244,_25616),on(_25616,_25314),happens(step(_25372,_25542,_25616),_25780,_25782),happens(turn(_25372,_25312),_25780,_25782),happens(drive(_25372,[_25312-_25314|_25346],_25376),_25782,_25414)]).

% LPS:  updates(step(_28408,_28410,_28412),in(to(_28410,_28412),location(_28408,_28410,_28548))).
% Into: updated(happens(step(_28408,_28410,_28412),_29796,_29802),location(_28408,_28410,_28548),_28410-_28412,[]).

% LPS:  updates(turn(_29694,_29696),in(to(_29732,_29696),location(_29694,_29830,_29732))).
% Into: updated(happens(turn(_29694,_29696),_31076,_31082),location(_29694,_29830,_29732),_29732-_29696,[]).

% LPS:  false((step(_37744,_37746,_37748),collisionPossible(_37744,_37804))).
% Into: d_pre([happens(step(_37744,_37746,_37748),_38898,_38904),holds(collisionPossible(_37744,_37804),_38898)]).

% LPS:  if(at(collisionPossible(_38932,_38934),_38956),(at(location(_38932,_39052,_39054),_38956),next(_39052,_39054,_39174),at(location(_38934,_39174,_39246),_38956),not(opposite(_39054,_39246)))).
% Into: l_int(holds(collisionPossible(_38932,_38934),_38956),[holds(location(_38932,_39052,_39054),_38956),next(_39052,_39054,_39174),holds(location(_38934,_39174,_39246),_38956),not(opposite(_39054,_39246))]).

% LPS:  false((step(_45810,_45812,_45814),collisionWarning(_45810,_45884,_45814),not(rightOfWay(_45810,_45884,_45814)))).
% Into: d_pre([happens(step(_45810,_45812,_45814),_47136,_47142),holds(collisionWarning(_45810,_45884,_45814),_47136),holds(not(rightOfWay(_45810,_45884,_45814)),_47136)]).

% LPS:  if(at(collisionWarning(_47184,_47186,_47188),_47210),(at(location(_47184,_47306,_47308),_47210),at(location(_47186,_47426,_47428),_47210),next(_47306,_47308,_47188),next(_47426,_47428,_47188),clash(_47308,_47428))).
% Into: l_int(holds(collisionWarning(_47184,_47186,_47188),_47210),[holds(location(_47184,_47306,_47308),_47210),holds(location(_47186,_47426,_47428),_47210),next(_47306,_47308,_47188),next(_47426,_47428,_47188),clash(_47308,_47428)]).

% LPS:  if(rightOfWay(_53868,_53870,_53872),(priorityTjunction(_53872,_53928),location(_53868,_53998,_54000),on(_53998,_53928))).
% Into: l_int(holds(rightOfWay(_53868,_53870,_53872),_55226),[priorityTjunction(_53872,_53928),holds(location(_53868,_53998,_54000),_55226),on(_53998,_53928)]).

% LPS:  if(rightOfWay(_55272,_55274,_55276),(crossRoads(_55276),at(location(_55272,_55386,_55388),_55410),at(location(_55274,_55506,_55508),_55410),rightOf(_55388,_55508))).
% Into: l_int(holds(rightOfWay(_55272,_55274,_55276),_56850),[crossRoads(_55276),holds(location(_55272,_55386,_55388),_55410),holds(location(_55274,_55506,_55508),_55410),rightOf(_55388,_55508)]).
% /pack/logicmoo_ec/test/lps_user_examples/self-drivers.pl:237
% pop_lps_dialect('$BLOB'("<stream>(0x562ef31d3700)"),  (/.../(lps_user_examples, 'self-drivers.pl')-> /.../(lps_user_examples, 'self-drivers.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/self-drivers.pl':_41872).


clash(Heading1, Heading2) :-
    horizontal(Heading1),
    vertical(Heading2).
clash(Heading1, Heading2) :-
    vertical(Heading1),
    horizontal(Heading2).

d_pre([happens(step(A, _, _), B, _), holds(collisionPossible(A, _), B)]).
d_pre([happens(step(A, _, B), C, _), holds(collisionWarning(A, D, B), C), holds(not(rightOfWay(A, D, B)), C)]).

directions(Start, [Heading-Street], Finish) :-
    on(Start, Street),
    on(Finish, Street),
    orientation(Start, Finish, Heading).
directions(Start, NewRoute, Finish) :-
    on(Finish, Street2),
    route(_, Route, _),
    append(FirstPart,
           [Heading2-Street2|_],
           Route),
    on(Start, Street1),
    append(_, [Heading1-Street1|Link], FirstPart),
    append([Heading1-Street1|Link],
           [Heading2-Street2],
           NewRoute).

crossRoads(6-5).
crossRoads(8-5).

updated(happens(step(A, B, C), _, _), location(A, B, _), B-C, []).
updated(happens(turn(A, B), _, _), location(A, _, C), C-B, []).

maxTime(30).

opposite(northward, southward).
opposite(southward, northward).
opposite(eastward, westward).
opposite(westward, eastward).

fluents([location(_, A, _), collisionWarning(B, C, A), rightOfWay(B, C, A), collisionPossible(B, C)]).

next(X-Y1, northward, X-Y2) :-
    Y2 is Y1+1.
next(X-Y1, southward, X-Y2) :-
    Y2 is Y1-1.
next(X1-Y, eastward, X2-Y) :-
    X2 is X1+1.
next(X1-Y, westward, X2-Y) :-
    X2 is X1-1.

vertical(H) :-
    (   H=northward
    ;   H=southward
    ).

l_events(happens(drive(A, [B-C], D), E, F), [holds(location(A, G, B), E), G\=D, next(G, B, H), on(H, C), happens(step(A, G, H), _, I), happens(drive(A, [B-C], D), I, F)]).
l_events(happens(drive(A, [B-C, D-E|F], G), _, H), [holds(location(A, I, B), _), next(I, B, J), not(on(J, E)), happens(step(A, I, J), _, K), happens(drive(A, [B-C, D-E|F], G), K, H)]).
l_events(happens(drive(A, [B-_, C-D|E], F), _, G), [holds(location(A, H, B), _), next(H, B, I), on(I, D), happens(step(A, H, I), J, K), happens(turn(A, C), J, K), happens(drive(A, [C-D|E], F), K, G)]).

:- dynamic actions/1.
:- multifile actions/1.

actions([step(A, _, _), turn(A, _)]).

rightOf(westward, northward).
rightOf(southward, westward).
rightOf(eastward, southward).
rightOf(northward, eastward).

horizontal(H) :-
    (   H=eastward
    ;   H=westward
    ).

d(location(mycar, X-Y, Heading), [type:ellipse, point:[XX, YY], size:[Xcar, Ycar], fillColor:blue]) :-
    position(X,
             Y,
             Heading,
             XX,
             YY,
             Xcar,
             Ycar).
d(location(yourcar, X-Y, Heading), [type:ellipse, point:[XX, YY], size:[Xcar, Ycar], fillColor:red]) :-
    position(X,
             Y,
             Heading,
             XX,
             YY,
             Xcar,
             Ycar).
d(location(othercar, X-Y, Heading), [type:ellipse, point:[XX, YY], size:[Xcar, Ycar], fillColor:green]) :-
    position(X,
             Y,
             Heading,
             XX,
             YY,
             Xcar,
             Ycar).
d(location(troubleMaker, X-Y, Heading), [type:ellipse, point:[XX, YY], size:[Xcar, Ycar], fillColor:maroon]) :-
    position(X,
             Y,
             Heading,
             XX,
             YY,
             Xcar,
             Ycar).
d(location(brokencar, X-Y, Heading), [type:circle, center:[XX, YY], radius:10, fillColor:red]) :-
    XX is X*20+10,
    YY is Y*20+10.
d(timeless, A) :-
    findall([ type:rectangle,
              from:[B, C],
              to:[D, E],
              fillColor:yellow
            ],
            ( place(F-G),
              not(on(F-G, _)),
              B is F*20,
              C is G*20,
              D is B+20,
              E is C+20
            ),
            A).

initial_state([location(mycar, 2-1, northward), location(yourcar, 6-1, northward), location(othercar, 10-5, westward), location(troubleMaker, 6-2, northward), location(brokencar, 2-7, noward)]).

route(2-9, [eastward-northStreet], 9-9).
route(2-1, [northward-westStreet, eastward-northStreet], 9-9).
route(2-1, [northward-westStreet, eastward-mainStreet], 9-6).
route(6-1, [northward-highStreet, eastward-northStreet], 9-9).
route(9-9, [westward-northStreet, southward-westStreet], 2-1).
route(10-5, [westward-mainStreet, southward-highStreet], 6-1).

on(X-5, mainStreet) :-
    2=<X,
    X=<10.
on(X-9, northStreet) :-
    2=<X,
    X=<9.
on(6-Y, highStreet) :-
    1=<Y,
    Y=<9.
on(2-Y, westStreet) :-
    1=<Y,
    Y=<9.
on(8-Y, eastStreet) :-
    1=<Y,
    Y=<9.

place(X-Y) :-
    member(X, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]),
    member(Y, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]).

priorityTjunction(2-5, westStreet).
priorityTjunction(6-9, northStreet).
priorityTjunction(8-9, northStreet).

orientation(X-Y1, X-Y2, northward) :-
    Y1<Y2.
orientation(X-Y1, X-Y2, southward) :-
    Y1>Y2.
orientation(X1-Y, X2-Y, eastward) :-
    X1<X2.
orientation(X1-Y, X2-Y, westward) :-
    X1>X2.

terminated(happens(remove(A), _, _), location(A, _, _), []).

l_int(holds(collisionPossible(A, B), C), [holds(location(A, D, E), C), next(D, E, F), holds(location(B, F, G), C), not(opposite(E, G))]).
l_int(holds(collisionWarning(A, B, C), D), [holds(location(A, E, F), D), holds(location(B, G, H), D), next(E, F, C), next(G, H, C), clash(F, H)]).
l_int(holds(rightOfWay(A, _, B), C), [priorityTjunction(B, D), holds(location(A, E, _), C), on(E, D)]).
l_int(holds(rightOfWay(A, B, C), _), [crossRoads(C), holds(location(A, _, D), E), holds(location(B, _, F), E), rightOf(D, F)]).

reactive_rule([happens(destination(A, B), _, C), holds(location(A, D, _), C)], [directions(D, E, B), happens(drive(A, E, B), C, _)]).

position(X, Y, northward, XX, YY, 3, 7) :-
    XX is X*20,
    YY is Y*20.
position(X, Y, southward, XX, YY, 3, 7) :-
    XX is X*20+15,
    YY is Y*20.
position(X, Y, westward, XX, YY, 7, 3) :-
    XX is X*20+15,
    YY is Y*20.
position(X, Y, eastward, XX, YY, 7, 3) :-
    XX is X*20,
    YY is Y*20+15.

events([destination(A, _), remove(A)]).

observe([destination(mycar, 9-9)], 3).
observe([destination(troubleMaker, 9-9)], 3).
observe([destination(yourcar, 9-9)], 3).
observe([destination(othercar, 6-1)], 2).
observe([remove(brokencar)], 16).
% dB(/.../(lps_user_examples, 'self-drivers.pl'), lps_visualization(_280106{groups:[_267842{content:"Events", id:"event", order:1}, _267916{content:"location(A,B,C)", id:"location/3", order:3, subgroupStack:"false"}, _267982{content:"Actions", id:"action", order:4}], items:[_268104{content:"brokencar,2-7,noward", end:16, group:"location/3", id:0, start:1, subgroup:"brokencar", title:"Fluent location(brokencar,2-7,noward) initiated at 1<br/>and terminated at transition to 16"}, _268230{content:"mycar,2-1,northward", end:4, group:"location/3", id:1, start:1, subgroup:"mycar", title:"Fluent location(mycar,2-1,northward) initiated at 1<br/>and terminated at transition to 4"}, _268356{content:"mycar,2-2,northward", end:5, group:"location/3", id:2, start:4, subgroup:"mycar", title:"Fluent location(mycar,2-2,northward) initiated at 4<br/>and terminated at transition to 5"}, _268482{content:"mycar,2-3,northward", end:6, group:"location/3", id:3, start:5, subgroup:"mycar", title:"Fluent location(mycar,2-3,northward) initiated at 5<br/>and terminated at transition to 6"}, _268608{content:"mycar,2-4,northward", end:7, group:"location/3", id:4, start:6, subgroup:"mycar", title:"Fluent location(mycar,2-4,northward) initiated at 6<br/>and terminated at transition to 7"}, _268734{content:"mycar,2-5,northward", end:8, group:"location/3", id:5, start:7, subgroup:"mycar", title:"Fluent location(mycar,2-5,northward) initiated at 7<br/>and terminated at transition to 8"}, _268860{content:"mycar,2-6,northward", end:17, group:"location/3", id:6, start:8, subgroup:"mycar", title:"Fluent location(mycar,2-6,northward) initiated at 8<br/>and terminated at transition to 17"}, _268986{content:"mycar,2-7,northward", end:18, group:"location/3", id:7, start:17, subgroup:"mycar", title:"Fluent location(mycar,2-7,northward) initiated at 17<br/>and terminated at transition to 18"}, _269112{content:"mycar,2-8,northward", end:19, group:"location/3", id:8, start:18, subgroup:"mycar", title:"Fluent location(mycar,2-8,northward) initiated at 18<br/>and terminated at transition to 19"}, _269238{content:"mycar,2-9,eastward", end:20, group:"location/3", id:9, start:19, subgroup:"mycar", title:"Fluent location(mycar,2-9,eastward) initiated at 19<br/>and terminated at transition to 20"}, _269364{content:"mycar,3-9,eastward", end:21, group:"location/3", id:10, start:20, subgroup:"mycar", title:"Fluent location(mycar,3-9,eastward) initiated at 20<br/>and terminated at transition to 21"}, _269490{content:"mycar,4-9,eastward", end:22, group:"location/3", id:11, start:21, subgroup:"mycar", title:"Fluent location(mycar,4-9,eastward) initiated at 21<br/>and terminated at transition to 22"}, _269616{content:"mycar,5-9,eastward", end:23, group:"location/3", id:12, start:22, subgroup:"mycar", title:"Fluent location(mycar,5-9,eastward) initiated at 22<br/>and terminated at transition to 23"}, _269742{content:"mycar,6-9,eastward", end:24, group:"location/3", id:13, start:23, subgroup:"mycar", title:"Fluent location(mycar,6-9,eastward) initiated at 23<br/>and terminated at transition to 24"}, _269868{content:"mycar,7-9,eastward", end:31, group:"location/3", id:14, start:24, subgroup:"mycar", title:"Fluent location(mycar,7-9,eastward) initiated at 24<br/>and terminated at transition to 31"}, _269994{content:"othercar,6-1,southward", end:31, group:"location/3", id:15, start:10, subgroup:"othercar", title:"Fluent location(othercar,6-1,southward) initiated at 10<br/>and terminated at transition to 31"}, _270120{content:"othercar,6-2,southward", end:10, group:"location/3", id:16, start:9, subgroup:"othercar", title:"Fluent location(othercar,6-2,southward) initiated at 9<br/>and terminated at transition to 10"}, _270246{content:"othercar,6-3,southward", end:9, group:"location/3", id:17, start:8, subgroup:"othercar", title:"Fluent location(othercar,6-3,southward) initiated at 8<br/>and terminated at transition to 9"}, _270372{content:"othercar,6-4,southward", end:8, group:"location/3", id:18, start:7, subgroup:"othercar", title:"Fluent location(othercar,6-4,southward) initiated at 7<br/>and terminated at transition to 8"}, _270498{content:"othercar,6-5,southward", end:7, group:"location/3", id:19, start:6, subgroup:"othercar", title:"Fluent location(othercar,6-5,southward) initiated at 6<br/>and terminated at transition to 7"}, _270624{content:"othercar,7-5,westward", end:6, group:"location/3", id:20, start:5, subgroup:"othercar", title:"Fluent location(othercar,7-5,westward) initiated at 5<br/>and terminated at transition to 6"}, _270750{content:"othercar,8-5,westward", end:5, group:"location/3", id:21, start:4, subgroup:"othercar", title:"Fluent location(othercar,8-5,westward) initiated at 4<br/>and terminated at transition to 5"}, _270876{content:"othercar,9-5,westward", end:4, group:"location/3", id:22, start:3, subgroup:"othercar", title:"Fluent location(othercar,9-5,westward) initiated at 3<br/>and terminated at transition to 4"}, _271002{content:"othercar,10-5,westward", end:3, group:"location/3", id:23, start:1, subgroup:"othercar", title:"Fluent location(othercar,10-5,westward) initiated at 1<br/>and terminated at transition to 3"}, _271128{content:"troubleMaker,6-2,northward", end:4, group:"location/3", id:24, start:1, subgroup:"troubleMaker", title:"Fluent location(troubleMaker,6-2,northward) initiated at 1<br/>and terminated at transition to 4"}, _271254{content:"troubleMaker,6-3,northward", end:5, group:"location/3", id:25, start:4, subgroup:"troubleMaker", title:"Fluent location(troubleMaker,6-3,northward) initiated at 4<br/>and terminated at transition to 5"}, _271380{content:"troubleMaker,6-4,northward", end:7, group:"location/3", id:26, start:5, subgroup:"troubleMaker", title:"Fluent location(troubleMaker,6-4,northward) initiated at 5<br/>and terminated at transition to 7"}, _271506{content:"troubleMaker,6-5,northward", end:8, group:"location/3", id:27, start:7, subgroup:"troubleMaker", title:"Fluent location(troubleMaker,6-5,northward) initiated at 7<br/>and terminated at transition to 8"}, _271632{content:"troubleMaker,6-6,northward", end:9, group:"location/3", id:28, start:8, subgroup:"troubleMaker", title:"Fluent location(troubleMaker,6-6,northward) initiated at 8<br/>and terminated at transition to 9"}, _271758{content:"troubleMaker,6-7,northward", end:10, group:"location/3", id:29, start:9, subgroup:"troubleMaker", title:"Fluent location(troubleMaker,6-7,northward) initiated at 9<br/>and terminated at transition to 10"}, _271884{content:"troubleMaker,6-8,northward", end:11, group:"location/3", id:30, start:10, subgroup:"troubleMaker", title:"Fluent location(troubleMaker,6-8,northward) initiated at 10<br/>and terminated at transition to 11"}, _272010{content:"troubleMaker,6-9,eastward", end:12, group:"location/3", id:31, start:11, subgroup:"troubleMaker", title:"Fluent location(troubleMaker,6-9,eastward) initiated at 11<br/>and terminated at transition to 12"}, _272136{content:"troubleMaker,7-9,eastward", end:13, group:"location/3", id:32, start:12, subgroup:"troubleMaker", title:"Fluent location(troubleMaker,7-9,eastward) initiated at 12<br/>and terminated at transition to 13"}, _272262{content:"troubleMaker,8-9,eastward", end:14, group:"location/3", id:33, start:13, subgroup:"troubleMaker", title:"Fluent location(troubleMaker,8-9,eastward) initiated at 13<br/>and terminated at transition to 14"}, _272388{content:"troubleMaker,9-9,eastward", end:31, group:"location/3", id:34, start:14, subgroup:"troubleMaker", title:"Fluent location(troubleMaker,9-9,eastward) initiated at 14<br/>and terminated at transition to 31"}, _272514{content:"yourcar,6-1,northward", end:5, group:"location/3", id:35, start:1, subgroup:"yourcar", title:"Fluent location(yourcar,6-1,northward) initiated at 1<br/>and terminated at transition to 5"}, _272640{content:"yourcar,6-2,northward", end:6, group:"location/3", id:36, start:5, subgroup:"yourcar", title:"Fluent location(yourcar,6-2,northward) initiated at 5<br/>and terminated at transition to 6"}, _272766{content:"yourcar,6-3,northward", end:8, group:"location/3", id:37, start:6, subgroup:"yourcar", title:"Fluent location(yourcar,6-3,northward) initiated at 6<br/>and terminated at transition to 8"}, _272892{content:"yourcar,6-4,northward", end:9, group:"location/3", id:38, start:8, subgroup:"yourcar", title:"Fluent location(yourcar,6-4,northward) initiated at 8<br/>and terminated at transition to 9"}, _273018{content:"yourcar,6-5,northward", end:10, group:"location/3", id:39, start:9, subgroup:"yourcar", title:"Fluent location(yourcar,6-5,northward) initiated at 9<br/>and terminated at transition to 10"}, _273144{content:"yourcar,6-6,northward", end:11, group:"location/3", id:40, start:10, subgroup:"yourcar", title:"Fluent location(yourcar,6-6,northward) initiated at 10<br/>and terminated at transition to 11"}, _273270{content:"yourcar,6-7,northward", end:12, group:"location/3", id:41, start:11, subgroup:"yourcar", title:"Fluent location(yourcar,6-7,northward) initiated at 11<br/>and terminated at transition to 12"}, _273396{content:"yourcar,6-8,northward", end:13, group:"location/3", id:42, start:12, subgroup:"yourcar", title:"Fluent location(yourcar,6-8,northward) initiated at 12<br/>and terminated at transition to 13"}, _273522{content:"yourcar,6-9,eastward", end:14, group:"location/3", id:43, start:13, subgroup:"yourcar", title:"Fluent location(yourcar,6-9,eastward) initiated at 13<br/>and terminated at transition to 14"}, _273648{content:"yourcar,7-9,eastward", end:15, group:"location/3", id:44, start:14, subgroup:"yourcar", title:"Fluent location(yourcar,7-9,eastward) initiated at 14<br/>and terminated at transition to 15"}, _273774{content:"yourcar,8-9,eastward", end:31, group:"location/3", id:45, start:15, subgroup:"yourcar", title:"Fluent location(yourcar,8-9,eastward) initiated at 15<br/>and terminated at transition to 31"}, _273900{content:"destination(othercar,6-1)", group:"event", id:46, start:2, style:"color:#E19735", title:"happens(destination(othercar,6-1),1,2)", type:"point"}, _274026{content:"destination(mycar,9-9)", group:"event", id:47, start:3, style:"color:#E19735", title:"happens(destination(mycar,9-9),2,3)", type:"point"}, _274152{content:"destination(troubleMaker,9-9)", group:"event", id:48, start:3, style:"color:#E19735", title:"happens(destination(troubleMaker,9-9),2,3)", type:"point"}, _274278{content:"destination(yourcar,9-9)", group:"event", id:49, start:3, style:"color:#E19735", title:"happens(destination(yourcar,9-9),2,3)", type:"point"}, _274404{content:"step(othercar,10-5,9-5)", group:"action", id:50, start:3, style:"color:green", title:"happens(step(othercar,10-5,9-5),2,3)", type:"point"}, _274530{content:"step(othercar,9-5,8-5)", group:"action", id:51, start:4, style:"color:green", title:"happens(step(othercar,9-5,8-5),3,4)", type:"point"}, _274656{content:"step(troubleMaker,6-2,6-3)", group:"action", id:52, start:4, style:"color:green", title:"happens(step(troubleMaker,6-2,6-3),3,4)", type:"point"}, _274782{content:"step(mycar,2-1,2-2)", group:"action", id:53, start:4, style:"color:green", title:"happens(step(mycar,2-1,2-2),3,4)", type:"point"}, _274908{content:"step(mycar,2-2,2-3)", group:"action", id:54, start:5, style:"color:green", title:"happens(step(mycar,2-2,2-3),4,5)", type:"point"}, _275034{content:"step(troubleMaker,6-3,6-4)", group:"action", id:55, start:5, style:"color:green", title:"happens(step(troubleMaker,6-3,6-4),4,5)", type:"point"}, _275160{content:"step(yourcar,6-1,6-2)", group:"action", id:56, start:5, style:"color:green", title:"happens(step(yourcar,6-1,6-2),4,5)", type:"point"}, _275286{content:"step(othercar,8-5,7-5)", group:"action", id:57, start:5, style:"color:green", title:"happens(step(othercar,8-5,7-5),4,5)", type:"point"}, _275412{content:"step(othercar,7-5,6-5)", group:"action", id:58, start:6, style:"color:green", title:"happens(step(othercar,7-5,6-5),5,6)", type:"point"}, _275538{content:"turn(othercar,southward)", group:"action", id:59, start:6, style:"color:green", title:"happens(turn(othercar,southward),5,6)", type:"point"}, _275664{content:"step(yourcar,6-2,6-3)", group:"action", id:60, start:6, style:"color:green", title:"happens(step(yourcar,6-2,6-3),5,6)", type:"point"}, _275790{content:"step(mycar,2-3,2-4)", group:"action", id:61, start:6, style:"color:green", title:"happens(step(mycar,2-3,2-4),5,6)", type:"point"}, _275916{content:"step(mycar,2-4,2-5)", group:"action", id:62, start:7, style:"color:green", title:"happens(step(mycar,2-4,2-5),6,7)", type:"point"}, _276042{content:"step(troubleMaker,6-4,6-5)", group:"action", id:63, start:7, style:"color:green", title:"happens(step(troubleMaker,6-4,6-5),6,7)", type:"point"}, _276168{content:"step(othercar,6-5,6-4)", group:"action", id:64, start:7, style:"color:green", title:"happens(step(othercar,6-5,6-4),6,7)", type:"point"}, _276294{content:"step(othercar,6-4,6-3)", group:"action", id:65, start:8, style:"color:green", title:"happens(step(othercar,6-4,6-3),7,8)", type:"point"}, _276420{content:"step(yourcar,6-3,6-4)", group:"action", id:66, start:8, style:"color:green", title:"happens(step(yourcar,6-3,6-4),7,8)", type:"point"}, _276546{content:"step(troubleMaker,6-5,6-6)", group:"action", id:67, start:8, style:"color:green", title:"happens(step(troubleMaker,6-5,6-6),7,8)", type:"point"}, _276672{content:"step(mycar,2-5,2-6)", group:"action", id:68, start:8, style:"color:green", title:"happens(step(mycar,2-5,2-6),7,8)", type:"point"}, _276798{content:"step(troubleMaker,6-6,6-7)", group:"action", id:69, start:9, style:"color:green", title:"happens(step(troubleMaker,6-6,6-7),8,9)", type:"point"}, _276924{content:"step(yourcar,6-4,6-5)", group:"action", id:70, start:9, style:"color:green", title:"happens(step(yourcar,6-4,6-5),8,9)", type:"point"}, _277050{content:"step(othercar,6-3,6-2)", group:"action", id:71, start:9, style:"color:green", title:"happens(step(othercar,6-3,6-2),8,9)", type:"point"}, _277176{content:"step(othercar,6-2,6-1)", group:"action", id:72, start:10, style:"color:green", title:"happens(step(othercar,6-2,6-1),9,10)", type:"point"}, _277302{content:"step(yourcar,6-5,6-6)", group:"action", id:73, start:10, style:"color:green", title:"happens(step(yourcar,6-5,6-6),9,10)", type:"point"}, _277428{content:"step(troubleMaker,6-7,6-8)", group:"action", id:74, start:10, style:"color:green", title:"happens(step(troubleMaker,6-7,6-8),9,10)", type:"point"}, _277554{content:"step(troubleMaker,6-8,6-9)", group:"action", id:75, start:11, style:"color:green", title:"happens(step(troubleMaker,6-8,6-9),10,11)", type:"point"}, _277680{content:"turn(troubleMaker,eastward)", group:"action", id:76, start:11, style:"color:green", title:"happens(turn(troubleMaker,eastward),10,11)", type:"point"}, _277806{content:"step(yourcar,6-6,6-7)", group:"action", id:77, start:11, style:"color:green", title:"happens(step(yourcar,6-6,6-7),10,11)", type:"point"}, _277932{content:"step(yourcar,6-7,6-8)", group:"action", id:78, start:12, style:"color:green", title:"happens(step(yourcar,6-7,6-8),11,12)", type:"point"}, _278058{content:"step(troubleMaker,6-9,7-9)", group:"action", id:79, start:12, style:"color:green", title:"happens(step(troubleMaker,6-9,7-9),11,12)", type:"point"}, _278184{content:"step(troubleMaker,7-9,8-9)", group:"action", id:80, start:13, style:"color:green", title:"happens(step(troubleMaker,7-9,8-9),12,13)", type:"point"}, _278310{content:"step(yourcar,6-8,6-9)", group:"action", id:81, start:13, style:"color:green", title:"happens(step(yourcar,6-8,6-9),12,13)", type:"point"}, _278436{content:"turn(yourcar,eastward)", group:"action", id:82, start:13, style:"color:green", title:"happens(turn(yourcar,eastward),12,13)", type:"point"}, _278562{content:"step(yourcar,6-9,7-9)", group:"action", id:83, start:14, style:"color:green", title:"happens(step(yourcar,6-9,7-9),13,14)", type:"point"}, _278688{content:"step(troubleMaker,8-9,9-9)", group:"action", id:84, start:14, style:"color:green", title:"happens(step(troubleMaker,8-9,9-9),13,14)", type:"point"}, _278814{content:"step(yourcar,7-9,8-9)", group:"action", id:85, start:15, style:"color:green", title:"happens(step(yourcar,7-9,8-9),14,15)", type:"point"}, _278940{content:"remove(brokencar)", group:"event", id:86, start:16, style:"color:#E19735", title:"happens(remove(brokencar),15,16)", type:"point"}, _279066{content:"step(mycar,2-6,2-7)", group:"action", id:87, start:17, style:"color:green", title:"happens(step(mycar,2-6,2-7),16,17)", type:"point"}, _279192{content:"step(mycar,2-7,2-8)", group:"action", id:88, start:18, style:"color:green", title:"happens(step(mycar,2-7,2-8),17,18)", type:"point"}, _279318{content:"step(mycar,2-8,2-9)", group:"action", id:89, start:19, style:"color:green", title:"happens(step(mycar,2-8,2-9),18,19)", type:"point"}, _279444{content:"turn(mycar,eastward)", group:"action", id:90, start:19, style:"color:green", title:"happens(turn(mycar,eastward),18,19)", type:"point"}, _279570{content:"step(mycar,2-9,3-9)", group:"action", id:91, start:20, style:"color:green", title:"happens(step(mycar,2-9,3-9),19,20)", type:"point"}, _279696{content:"step(mycar,3-9,4-9)", group:"action", id:92, start:21, style:"color:green", title:"happens(step(mycar,3-9,4-9),20,21)", type:"point"}, _279822{content:"step(mycar,4-9,5-9)", group:"action", id:93, start:22, style:"color:green", title:"happens(step(mycar,4-9,5-9),21,22)", type:"point"}, _279948{content:"step(mycar,5-9,6-9)", group:"action", id:94, start:23, style:"color:green", title:"happens(step(mycar,5-9,6-9),22,23)", type:"point"}, _280074{content:"step(mycar,6-9,7-9)", group:"action", id:95, start:24, style:"color:green", title:"happens(step(mycar,6-9,7-9),23,24)", type:"point"}]}, _422170{cycles:[[_414046{create:[_406824{fillColor:"yellow", from:[20, 20], id:"timeless", to:[40, 40], type:"rectangle"}, _406942{fillColor:"yellow", from:[20, 40], id:"timeless", to:[40, 60], type:"rectangle"}, _407060{fillColor:"yellow", from:[20, 60], id:"timeless", to:[40, 80], type:"rectangle"}, _407178{fillColor:"yellow", from:[20, 80], id:"timeless", to:[40, 100], type:"rectangle"}, _407296{fillColor:"yellow", from:[20, 100], id:"timeless", to:[40, 120], type:"rectangle"}, _407414{fillColor:"yellow", from:[20, 120], id:"timeless", to:[40, 140], type:"rectangle"}, _407532{fillColor:"yellow", from:[20, 140], id:"timeless", to:[40, 160], type:"rectangle"}, _407650{fillColor:"yellow", from:[20, 160], id:"timeless", to:[40, 180], type:"rectangle"}, _407768{fillColor:"yellow", from:[20, 180], id:"timeless", to:[40, 200], type:"rectangle"}, _407886{fillColor:"yellow", from:[20, 200], id:"timeless", to:[40, 220], type:"rectangle"}, _408004{fillColor:"yellow", from:[40, 200], id:"timeless", to:[60, 220], type:"rectangle"}, _408122{fillColor:"yellow", from:[60, 20], id:"timeless", to:[80, 40], type:"rectangle"}, _408240{fillColor:"yellow", from:[60, 40], id:"timeless", to:[80, 60], type:"rectangle"}, _408358{fillColor:"yellow", from:[60, 60], id:"timeless", to:[80, 80], type:"rectangle"}, _408476{fillColor:"yellow", from:[60, 80], id:"timeless", to:[80, 100], type:"rectangle"}, _408594{fillColor:"yellow", from:[60, 120], id:"timeless", to:[80, 140], type:"rectangle"}, _408712{fillColor:"yellow", from:[60, 140], id:"timeless", to:[80, 160], type:"rectangle"}, _408830{fillColor:"yellow", from:[60, 160], id:"timeless", to:[80, 180], type:"rectangle"}, _408948{fillColor:"yellow", from:[60, 200], id:"timeless", to:[80, 220], type:"rectangle"}, _409066{fillColor:"yellow", from:[80, 20], id:"timeless", to:[100, 40], type:"rectangle"}, _409184{fillColor:"yellow", from:[80, 40], id:"timeless", to:[100, 60], type:"rectangle"}, _409302{fillColor:"yellow", from:[80, 60], id:"timeless", to:[100, 80], type:"rectangle"}, _409420{fillColor:"yellow", from:[80, 80], id:"timeless", to:[100, 100], type:"rectangle"}, _409538{fillColor:"yellow", from:[80, 120], id:"timeless", to:[100, 140], type:"rectangle"}, _409656{fillColor:"yellow", from:[80, 140], id:"timeless", to:[100, 160], type:"rectangle"}, _409774{fillColor:"yellow", from:[80, 160], id:"timeless", to:[100, 180], type:"rectangle"}, _409892{fillColor:"yellow", from:[80, 200], id:"timeless", to:[100, 220], type:"rectangle"}, _410010{fillColor:"yellow", from:[100, 20], id:"timeless", to:[120, 40], type:"rectangle"}, _410128{fillColor:"yellow", from:[100, 40], id:"timeless", to:[120, 60], type:"rectangle"}, _410246{fillColor:"yellow", from:[100, 60], id:"timeless", to:[120, 80], type:"rectangle"}, _410364{fillColor:"yellow", from:[100, 80], id:"timeless", to:[120, 100], type:"rectangle"}, _410482{fillColor:"yellow", from:[100, 120], id:"timeless", to:[120, 140], type:"rectangle"}, _410600{fillColor:"yellow", from:[100, 140], id:"timeless", to:[120, 160], type:"rectangle"}, _410718{fillColor:"yellow", from:[100, 160], id:"timeless", to:[120, 180], type:"rectangle"}, _410836{fillColor:"yellow", from:[100, 200], id:"timeless", to:[120, 220], type:"rectangle"}, _410954{fillColor:"yellow", from:[120, 200], id:"timeless", to:[140, 220], type:"rectangle"}, _411072{fillColor:"yellow", from:[140, 20], id:"timeless", to:[160, 40], type:"rectangle"}, _411190{fillColor:"yellow", from:[140, 40], id:"timeless", to:[160, 60], type:"rectangle"}, _411308{fillColor:"yellow", from:[140, 60], id:"timeless", to:[160, 80], type:"rectangle"}, _411426{fillColor:"yellow", from:[140, 80], id:"timeless", to:[160, 100], type:"rectangle"}, _411544{fillColor:"yellow", from:[140, 120], id:"timeless", to:[160, 140], type:"rectangle"}, _411662{fillColor:"yellow", from:[140, 140], id:"timeless", to:[160, 160], type:"rectangle"}, _411780{fillColor:"yellow", from:[140, 160], id:"timeless", to:[160, 180], type:"rectangle"}, _411898{fillColor:"yellow", from:[140, 200], id:"timeless", to:[160, 220], type:"rectangle"}, _412016{fillColor:"yellow", from:[160, 200], id:"timeless", to:[180, 220], type:"rectangle"}, _412134{fillColor:"yellow", from:[180, 20], id:"timeless", to:[200, 40], type:"rectangle"}, _412252{fillColor:"yellow", from:[180, 40], id:"timeless", to:[200, 60], type:"rectangle"}, _412370{fillColor:"yellow", from:[180, 60], id:"timeless", to:[200, 80], type:"rectangle"}, _412488{fillColor:"yellow", from:[180, 80], id:"timeless", to:[200, 100], type:"rectangle"}, _412606{fillColor:"yellow", from:[180, 120], id:"timeless", to:[200, 140], type:"rectangle"}, _412724{fillColor:"yellow", from:[180, 140], id:"timeless", to:[200, 160], type:"rectangle"}, _412842{fillColor:"yellow", from:[180, 160], id:"timeless", to:[200, 180], type:"rectangle"}, _412960{fillColor:"yellow", from:[180, 200], id:"timeless", to:[200, 220], type:"rectangle"}, _413078{fillColor:"yellow", from:[200, 20], id:"timeless", to:[220, 40], type:"rectangle"}, _413196{fillColor:"yellow", from:[200, 40], id:"timeless", to:[220, 60], type:"rectangle"}, _413314{fillColor:"yellow", from:[200, 60], id:"timeless", to:[220, 80], type:"rectangle"}, _413432{fillColor:"yellow", from:[200, 80], id:"timeless", to:[220, 100], type:"rectangle"}, _413550{fillColor:"yellow", from:[200, 120], id:"timeless", to:[220, 140], type:"rectangle"}, _413668{fillColor:"yellow", from:[200, 140], id:"timeless", to:[220, 160], type:"rectangle"}, _413786{fillColor:"yellow", from:[200, 160], id:"timeless", to:[220, 180], type:"rectangle"}, _413904{fillColor:"yellow", from:[200, 180], id:"timeless", to:[220, 200], type:"rectangle"}, _414022{fillColor:"yellow", from:[200, 200], id:"timeless", to:[220, 220], type:"rectangle"}]}], [_414182{create:_414158{center:[50, 150], fillColor:"red", id:"location(brokencar,2-7,noward)", radius:10, type:"circle"}}, _414324{create:_414300{fillColor:"blue", id:"location(mycar,2-1,northward)", point:[40, 20], size:[3, 7], type:"ellipse"}}, _414466{create:_414442{fillColor:"green", id:"location(othercar,10-5,westward)", point:[215, 100], size:[7, 3], type:"ellipse"}}, _414608{create:_414584{fillColor:"maroon", id:"location(troubleMaker,6-2,northward)", point:[120, 40], size:[3, 7], type:"ellipse"}}, _414750{create:_414726{fillColor:"red", id:"location(yourcar,6-1,northward)", point:[120, 20], size:[3, 7], type:"ellipse"}}], [], [_414904{create:_414880{fillColor:"green", id:"location(othercar,9-5,westward)", point:[195, 100], size:[7, 3], type:"ellipse"}}, _414934{kill:"location(othercar,10-5,westward)"}], [_415082{create:_415058{fillColor:"blue", id:"location(mycar,2-2,northward)", point:[40, 40], size:[3, 7], type:"ellipse"}}, _415224{create:_415200{fillColor:"green", id:"location(othercar,8-5,westward)", point:[175, 100], size:[7, 3], type:"ellipse"}}, _415366{create:_415342{fillColor:"maroon", id:"location(troubleMaker,6-3,northward)", point:[120, 60], size:[3, 7], type:"ellipse"}}, _415396{kill:"location(mycar,2-1,northward)"}, _415426{kill:"location(othercar,9-5,westward)"}, _415456{kill:"location(troubleMaker,6-2,northward)"}], [_415604{create:_415580{fillColor:"blue", id:"location(mycar,2-3,northward)", point:[40, 60], size:[3, 7], type:"ellipse"}}, _415746{create:_415722{fillColor:"green", id:"location(othercar,7-5,westward)", point:[155, 100], size:[7, 3], type:"ellipse"}}, _415888{create:_415864{fillColor:"maroon", id:"location(troubleMaker,6-4,northward)", point:[120, 80], size:[3, 7], type:"ellipse"}}, _416030{create:_416006{fillColor:"red", id:"location(yourcar,6-2,northward)", point:[120, 40], size:[3, 7], type:"ellipse"}}, _416060{kill:"location(mycar,2-2,northward)"}, _416090{kill:"location(othercar,8-5,westward)"}, _416120{kill:"location(troubleMaker,6-3,northward)"}, _416150{kill:"location(yourcar,6-1,northward)"}], [_416298{create:_416274{fillColor:"blue", id:"location(mycar,2-4,northward)", point:[40, 80], size:[3, 7], type:"ellipse"}}, _416440{create:_416416{fillColor:"green", id:"location(othercar,6-5,southward)", point:[135, 100], size:[3, 7], type:"ellipse"}}, _416582{create:_416558{fillColor:"red", id:"location(yourcar,6-3,northward)", point:[120, 60], size:[3, 7], type:"ellipse"}}, _416612{kill:"location(mycar,2-3,northward)"}, _416642{kill:"location(othercar,7-5,westward)"}, _416672{kill:"location(yourcar,6-2,northward)"}], [_416820{create:_416796{fillColor:"blue", id:"location(mycar,2-5,northward)", point:[40, 100], size:[3, 7], type:"ellipse"}}, _416962{create:_416938{fillColor:"green", id:"location(othercar,6-4,southward)", point:[135, 80], size:[3, 7], type:"ellipse"}}, _417104{create:_417080{fillColor:"maroon", id:"location(troubleMaker,6-5,northward)", point:[120, 100], size:[3, 7], type:"ellipse"}}, _417134{kill:"location(mycar,2-4,northward)"}, _417164{kill:"location(othercar,6-5,southward)"}, _417194{kill:"location(troubleMaker,6-4,northward)"}], [_417342{create:_417318{fillColor:"blue", id:"location(mycar,2-6,northward)", point:[40, 120], size:[3, 7], type:"ellipse"}}, _417484{create:_417460{fillColor:"green", id:"location(othercar,6-3,southward)", point:[135, 60], size:[3, 7], type:"ellipse"}}, _417626{create:_417602{fillColor:"maroon", id:"location(troubleMaker,6-6,northward)", point:[120, 120], size:[3, 7], type:"ellipse"}}, _417768{create:_417744{fillColor:"red", id:"location(yourcar,6-4,northward)", point:[120, 80], size:[3, 7], type:"ellipse"}}, _417798{kill:"location(mycar,2-5,northward)"}, _417828{kill:"location(othercar,6-4,southward)"}, _417858{kill:"location(troubleMaker,6-5,northward)"}, _417888{kill:"location(yourcar,6-3,northward)"}], [_418036{create:_418012{fillColor:"green", id:"location(othercar,6-2,southward)", point:[135, 40], size:[3, 7], type:"ellipse"}}, _418178{create:_418154{fillColor:"maroon", id:"location(troubleMaker,6-7,northward)", point:[120, 140], size:[3, 7], type:"ellipse"}}, _418320{create:_418296{fillColor:"red", id:"location(yourcar,6-5,northward)", point:[120, 100], size:[3, 7], type:"ellipse"}}, _418350{kill:"location(othercar,6-3,southward)"}, _418380{kill:"location(troubleMaker,6-6,northward)"}, _418410{kill:"location(yourcar,6-4,northward)"}], [_418558{create:_418534{fillColor:"green", id:"location(othercar,6-1,southward)", point:[135, 20], size:[3, 7], type:"ellipse"}}, _418700{create:_418676{fillColor:"maroon", id:"location(troubleMaker,6-8,northward)", point:[120, 160], size:[3, 7], type:"ellipse"}}, _418842{create:_418818{fillColor:"red", id:"location(yourcar,6-6,northward)", point:[120, 120], size:[3, 7], type:"ellipse"}}, _418872{kill:"location(othercar,6-2,southward)"}, _418902{kill:"location(troubleMaker,6-7,northward)"}, _418932{kill:"location(yourcar,6-5,northward)"}], [_419080{create:_419056{fillColor:"maroon", id:"location(troubleMaker,6-9,eastward)", point:[120, 195], size:[7, 3], type:"ellipse"}}, _419222{create:_419198{fillColor:"red", id:"location(yourcar,6-7,northward)", point:[120, 140], size:[3, 7], type:"ellipse"}}, _419252{kill:"location(troubleMaker,6-8,northward)"}, _419282{kill:"location(yourcar,6-6,northward)"}], [_419430{create:_419406{fillColor:"maroon", id:"location(troubleMaker,7-9,eastward)", point:[140, 195], size:[7, 3], type:"ellipse"}}, _419572{create:_419548{fillColor:"red", id:"location(yourcar,6-8,northward)", point:[120, 160], size:[3, 7], type:"ellipse"}}, _419602{kill:"location(troubleMaker,6-9,eastward)"}, _419632{kill:"location(yourcar,6-7,northward)"}], [_419780{create:_419756{fillColor:"maroon", id:"location(troubleMaker,8-9,eastward)", point:[160, 195], size:[7, 3], type:"ellipse"}}, _419922{create:_419898{fillColor:"red", id:"location(yourcar,6-9,eastward)", point:[120, 195], size:[7, 3], type:"ellipse"}}, _419952{kill:"location(troubleMaker,7-9,eastward)"}, _419982{kill:"location(yourcar,6-8,northward)"}], [_420130{create:_420106{fillColor:"maroon", id:"location(troubleMaker,9-9,eastward)", point:[180, 195], size:[7, 3], type:"ellipse"}}, _420272{create:_420248{fillColor:"red", id:"location(yourcar,7-9,eastward)", point:[140, 195], size:[7, 3], type:"ellipse"}}, _420302{kill:"location(troubleMaker,8-9,eastward)"}, _420332{kill:"location(yourcar,6-9,eastward)"}], [_420480{create:_420456{fillColor:"red", id:"location(yourcar,8-9,eastward)", point:[160, 195], size:[7, 3], type:"ellipse"}}, _420510{kill:"location(yourcar,7-9,eastward)"}], [_420546{kill:"location(brokencar,2-7,noward)"}], [_420694{create:_420670{fillColor:"blue", id:"location(mycar,2-7,northward)", point:[40, 140], size:[3, 7], type:"ellipse"}}, _420724{kill:"location(mycar,2-6,northward)"}], [_420872{create:_420848{fillColor:"blue", id:"location(mycar,2-8,northward)", point:[40, 160], size:[3, 7], type:"ellipse"}}, _420902{kill:"location(mycar,2-7,northward)"}], [_421050{create:_421026{fillColor:"blue", id:"location(mycar,2-9,eastward)", point:[40, 195], size:[7, 3], type:"ellipse"}}, _421080{kill:"location(mycar,2-8,northward)"}], [_421228{create:_421204{fillColor:"blue", id:"location(mycar,3-9,eastward)", point:[60, 195], size:[7, 3], type:"ellipse"}}, _421258{kill:"location(mycar,2-9,eastward)"}], [_421406{create:_421382{fillColor:"blue", id:"location(mycar,4-9,eastward)", point:[80, 195], size:[7, 3], type:"ellipse"}}, _421436{kill:"location(mycar,3-9,eastward)"}], [_421584{create:_421560{fillColor:"blue", id:"location(mycar,5-9,eastward)", point:[100, 195], size:[7, 3], type:"ellipse"}}, _421614{kill:"location(mycar,4-9,eastward)"}], [_421762{create:_421738{fillColor:"blue", id:"location(mycar,6-9,eastward)", point:[120, 195], size:[7, 3], type:"ellipse"}}, _421792{kill:"location(mycar,5-9,eastward)"}], [_421940{create:_421916{fillColor:"blue", id:"location(mycar,7-9,eastward)", point:[140, 195], size:[7, 3], type:"ellipse"}}, _421970{kill:"location(mycar,6-9,eastward)"}], [], [], [], [], [], [], [_422042{kill:"location(mycar,7-9,eastward)"}, _422072{kill:"location(othercar,6-1,southward)"}, _422102{kill:"location(troubleMaker,9-9,eastward)"}, _422132{kill:"location(yourcar,8-9,eastward)"}, _422162{kill:"timeless"}]]})).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'self-driving cars.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'self-driving cars.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/self-driving cars.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'self-driving cars.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'self-driving cars.pl'), lps= /.../(lps_user_examples, 'self-driving cars.pl'), using= /.../(lps_user_examples, 'self-driving cars.pl')].
% continue_lps_dialect.
% ops.

% LPS:  fluents(location(_107232,_107234,_107236)).
% Into: fluents([location(_107232,_107234,_107236)]).

% LPS:  events(destination(_108316,_108318)).
% Into: events([destination(_108316,_108318)]).

% LPS:  actions((step(_109370),turn(_109370,_109426))).
% Into: actions([step(_109370),turn(_109370,_109426)]).

% LPS:  if(updates(step(_110512),in(to(_110548,_110550),location(_110512,_110548,_110648))),next(_110548,_110648,_110550)).
% Into: updated(happens(step(_110512),_112024,_112030),location(_110512,_110548,_110648),_110548-_110550,[next(_110548,_110648,_110550)]).

% LPS:  updates(turn(_120224,_120226),in(to(_120262,_120226),location(_120224,_120360,_120262))).
% Into: updated(happens(turn(_120224,_120226),_121606,_121612),location(_120224,_120360,_120262),_120262-_120226,[]).

% LPS:  initially((location(mycar,2-1,northward),location(yourcar,9-9,westward))).
% Into: initial_state([location(mycar,2-1,northward),location(yourcar,9-9,westward)]).

% LPS:  observe(from(destination(mycar,9-9),to(2,3))).
% Into: observe([destination(mycar,9-9)],3).

% LPS:  observe(from(destination(yourcar,2-1),to(3,4))).
% Into: observe([destination(yourcar,2-1)],4).

% LPS:  then(if((to(destination(_138670,_138672),_138694),at(location(_138670,_138790,_138792),_138694),directions(_138790,_138910,_138672))),from(drive(_138670,_138910,_138672),to(_138694,_139110))).
% Into: reactive_rule([happens(destination(_138670,_138672),_140342,_138694),holds(location(_138670,_138790,_138792),_138694),directions(_138790,_138910,_138672)],[happens(drive(_138670,_138910,_138672),_138694,_139110)]).

% LPS:  if(from(drive(_141806,_141808,_141810),to(_141846,_141846)),at(location(_141806,_141810,_141978),_141846)).
% Into: l_events(happens(drive(_141806,_141808,_141810),_141846,_141846),[holds(location(_141806,_141810,_141978),_141846)]).

% LPS:  if(from(drive(_143538,[_143478-_143480|_143512],_143542),to(_143578,_143580)),(location(_143538,_143708,_143478),on(_143708,_143480),next(_143708,_143478,_143838),on(_143838,_143480),from(step(_143538),to(_143578,_143972)),from(drive(_143538,[_143478-_143480|_143512],_143542),to(_143972,_143580)))).
% Into: l_events(happens(drive(_143538,[_143478-_143480|_143512],_143542),_143578,_143580),[holds(location(_143538,_143708,_143478),_145796),on(_143708,_143480),next(_143708,_143478,_143838),on(_143838,_143480),happens(step(_143538),_143578,_143972),happens(drive(_143538,[_143478-_143480|_143512],_143542),_143972,_143580)]).

% LPS:  if(from(drive(_146694,[_146566-_146568,_146634-_146636|_146668],_146698),to(_146734,_146736)),(location(_146694,_146864,_146566),on(_146864,_146568),on(_146864,_146636),from(turn(_146694,_146634),to(_146734,_147072)),from(drive(_146694,[_146634-_146636|_146668],_146698),to(_147072,_146736)))).
% Into: l_events(happens(drive(_146694,[_146566-_146568,_146634-_146636|_146668],_146698),_146734,_146736),[holds(location(_146694,_146864,_146566),_148876),on(_146864,_146568),on(_146864,_146636),happens(turn(_146694,_146634),_146734,_147072),happens(drive(_146694,[_146634-_146636|_146668],_146698),_147072,_146736)]).
% /pack/logicmoo_ec/test/lps_user_examples/self-driving cars.pl:83
% pop_lps_dialect('$BLOB'("<stream>(0x562ef3ca3900)"),  (/.../(lps_user_examples, 'self-driving cars.pl')-> /.../(lps_user_examples, 'self-driving cars.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/self-driving cars.pl':_166774).


d(location(mycar, X-Y, Heading), [type:circle, center:[XX, YY], radius:5, fillColor:blue]) :-
    XX is X*10+5,
    YY is Y*10+5.
d(location(yourcar, X-Y, Heading), [type:circle, center:[XX, YY], radius:5, fillColor:red]) :-
    XX is X*10+5,
    YY is Y*10+5.
d(timeless, A) :-
    findall([ type:rectangle,
              from:[B, C],
              to:[D, E],
              fillColor:yellow
            ],
            ( place(F-G),
              not(on(F-G, _)),
              B is F*10,
              C is G*10,
              D is B+10,
              E is C+10
            ),
            A).

fluents([location(_, _, _)]).

next(X-Y1, northward, X-Y2) :-
    Y2 is Y1+1.
next(X-Y1, southward, X-Y2) :-
    Y2 is Y1-1.
next(X1-Y, eastward, X2-Y) :-
    X2 is X1+1.
next(X1-Y, westward, X2-Y) :-
    X2 is X1-1.

directions(2-1, [northward-westStreet, eastward-northStreet], 9-9).
directions(9-9, [westward-northStreet, southward-westStreet], 2-1).

reactive_rule([happens(destination(A, B), _, C), holds(location(A, D, _), C), directions(D, E, B)], [happens(drive(A, E, B), C, _)]).

initial_state([location(mycar, 2-1, northward), location(yourcar, 9-9, westward)]).

l_events(happens(drive(A, _, B), C, C), [holds(location(A, B, _), C)]).
l_events(happens(drive(A, [B-C|D], E), F, G), [holds(location(A, H, B), _), on(H, C), next(H, B, I), on(I, C), happens(step(A), F, J), happens(drive(A, [B-C|D], E), J, G)]).
l_events(happens(drive(A, [B-C, D-E|F], G), H, I), [holds(location(A, J, B), _), on(J, C), on(J, E), happens(turn(A, D), H, K), happens(drive(A, [D-E|F], G), K, I)]).

:- dynamic actions/1.
:- multifile actions/1.

actions([step(A), turn(A, _)]).

on(X-5, mainStreet) :-
    3=<X,
    X=<10.
on(X-9, northStreet) :-
    2=<X,
    X=<9.
on(6-Y, highStreet) :-
    1=<Y,
    Y=<9.
on(2-Y, westStreet) :-
    1=<Y,
    Y=<9.
on(8-Y, eastStreet) :-
    1=<Y,
    Y=<9.

events([destination(_, _)]).

updated(happens(step(A), _, _), location(A, B, C), B-D, [next(B, C, D)]).
updated(happens(turn(A, B), _, _), location(A, _, C), C-B, []).

maxTime(20).

observe([destination(mycar, 9-9)], 3).
observe([destination(yourcar, 2-1)], 4).

place(X-Y) :-
    member(X, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]),
    member(Y, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]).
% dB(/.../(lps_user_examples, 'self-driving cars.pl'), lps_visualization(_209334{groups:[_200598{content:"Events", id:"event", order:1}, _200672{content:"location(A,B,C)", id:"location/3", order:3, subgroupStack:"false"}, _200738{content:"Actions", id:"action", order:4}], items:[_200860{content:"mycar,2-1,northward", end:4, group:"location/3", id:0, start:1, subgroup:"mycar", title:"Fluent location(mycar,2-1,northward) initiated at 1<br/>and terminated at transition to 4"}, _200986{content:"mycar,2-2,northward", end:5, group:"location/3", id:1, start:4, subgroup:"mycar", title:"Fluent location(mycar,2-2,northward) initiated at 4<br/>and terminated at transition to 5"}, _201112{content:"mycar,2-3,northward", end:6, group:"location/3", id:2, start:5, subgroup:"mycar", title:"Fluent location(mycar,2-3,northward) initiated at 5<br/>and terminated at transition to 6"}, _201238{content:"mycar,2-4,northward", end:7, group:"location/3", id:3, start:6, subgroup:"mycar", title:"Fluent location(mycar,2-4,northward) initiated at 6<br/>and terminated at transition to 7"}, _201364{content:"mycar,2-5,northward", end:8, group:"location/3", id:4, start:7, subgroup:"mycar", title:"Fluent location(mycar,2-5,northward) initiated at 7<br/>and terminated at transition to 8"}, _201490{content:"mycar,2-6,northward", end:9, group:"location/3", id:5, start:8, subgroup:"mycar", title:"Fluent location(mycar,2-6,northward) initiated at 8<br/>and terminated at transition to 9"}, _201616{content:"mycar,2-7,northward", end:10, group:"location/3", id:6, start:9, subgroup:"mycar", title:"Fluent location(mycar,2-7,northward) initiated at 9<br/>and terminated at transition to 10"}, _201742{content:"mycar,2-8,northward", end:11, group:"location/3", id:7, start:10, subgroup:"mycar", title:"Fluent location(mycar,2-8,northward) initiated at 10<br/>and terminated at transition to 11"}, _201868{content:"mycar,2-9,eastward", end:13, group:"location/3", id:8, start:12, subgroup:"mycar", title:"Fluent location(mycar,2-9,eastward) initiated at 12<br/>and terminated at transition to 13"}, _201994{content:"mycar,2-9,northward", end:12, group:"location/3", id:9, start:11, subgroup:"mycar", title:"Fluent location(mycar,2-9,northward) initiated at 11<br/>and terminated at transition to 12"}, _202120{content:"mycar,3-9,eastward", end:14, group:"location/3", id:10, start:13, subgroup:"mycar", title:"Fluent location(mycar,3-9,eastward) initiated at 13<br/>and terminated at transition to 14"}, _202246{content:"mycar,4-9,eastward", end:15, group:"location/3", id:11, start:14, subgroup:"mycar", title:"Fluent location(mycar,4-9,eastward) initiated at 14<br/>and terminated at transition to 15"}, _202372{content:"mycar,5-9,eastward", end:16, group:"location/3", id:12, start:15, subgroup:"mycar", title:"Fluent location(mycar,5-9,eastward) initiated at 15<br/>and terminated at transition to 16"}, _202498{content:"mycar,6-9,eastward", end:17, group:"location/3", id:13, start:16, subgroup:"mycar", title:"Fluent location(mycar,6-9,eastward) initiated at 16<br/>and terminated at transition to 17"}, _202624{content:"mycar,7-9,eastward", end:18, group:"location/3", id:14, start:17, subgroup:"mycar", title:"Fluent location(mycar,7-9,eastward) initiated at 17<br/>and terminated at transition to 18"}, _202750{content:"mycar,8-9,eastward", end:19, group:"location/3", id:15, start:18, subgroup:"mycar", title:"Fluent location(mycar,8-9,eastward) initiated at 18<br/>and terminated at transition to 19"}, _202876{content:"mycar,9-9,eastward", end:21, group:"location/3", id:16, start:19, subgroup:"mycar", title:"Fluent location(mycar,9-9,eastward) initiated at 19<br/>and terminated at transition to 21"}, _203002{content:"yourcar,2-1,southward", end:21, group:"location/3", id:17, start:20, subgroup:"yourcar", title:"Fluent location(yourcar,2-1,southward) initiated at 20<br/>and terminated at transition to 21"}, _203128{content:"yourcar,2-2,southward", end:20, group:"location/3", id:18, start:19, subgroup:"yourcar", title:"Fluent location(yourcar,2-2,southward) initiated at 19<br/>and terminated at transition to 20"}, _203254{content:"yourcar,2-3,southward", end:19, group:"location/3", id:19, start:18, subgroup:"yourcar", title:"Fluent location(yourcar,2-3,southward) initiated at 18<br/>and terminated at transition to 19"}, _203380{content:"yourcar,2-4,southward", end:18, group:"location/3", id:20, start:17, subgroup:"yourcar", title:"Fluent location(yourcar,2-4,southward) initiated at 17<br/>and terminated at transition to 18"}, _203506{content:"yourcar,2-5,southward", end:17, group:"location/3", id:21, start:16, subgroup:"yourcar", title:"Fluent location(yourcar,2-5,southward) initiated at 16<br/>and terminated at transition to 17"}, _203632{content:"yourcar,2-6,southward", end:16, group:"location/3", id:22, start:15, subgroup:"yourcar", title:"Fluent location(yourcar,2-6,southward) initiated at 15<br/>and terminated at transition to 16"}, _203758{content:"yourcar,2-7,southward", end:15, group:"location/3", id:23, start:14, subgroup:"yourcar", title:"Fluent location(yourcar,2-7,southward) initiated at 14<br/>and terminated at transition to 15"}, _203884{content:"yourcar,2-8,southward", end:14, group:"location/3", id:24, start:13, subgroup:"yourcar", title:"Fluent location(yourcar,2-8,southward) initiated at 13<br/>and terminated at transition to 14"}, _204010{content:"yourcar,2-9,southward", end:13, group:"location/3", id:25, start:12, subgroup:"yourcar", title:"Fluent location(yourcar,2-9,southward) initiated at 12<br/>and terminated at transition to 13"}, _204136{content:"yourcar,2-9,westward", end:12, group:"location/3", id:26, start:11, subgroup:"yourcar", title:"Fluent location(yourcar,2-9,westward) initiated at 11<br/>and terminated at transition to 12"}, _204262{content:"yourcar,3-9,westward", end:11, group:"location/3", id:27, start:10, subgroup:"yourcar", title:"Fluent location(yourcar,3-9,westward) initiated at 10<br/>and terminated at transition to 11"}, _204388{content:"yourcar,4-9,westward", end:10, group:"location/3", id:28, start:9, subgroup:"yourcar", title:"Fluent location(yourcar,4-9,westward) initiated at 9<br/>and terminated at transition to 10"}, _204514{content:"yourcar,5-9,westward", end:9, group:"location/3", id:29, start:8, subgroup:"yourcar", title:"Fluent location(yourcar,5-9,westward) initiated at 8<br/>and terminated at transition to 9"}, _204640{content:"yourcar,6-9,westward", end:8, group:"location/3", id:30, start:7, subgroup:"yourcar", title:"Fluent location(yourcar,6-9,westward) initiated at 7<br/>and terminated at transition to 8"}, _204766{content:"yourcar,7-9,westward", end:7, group:"location/3", id:31, start:6, subgroup:"yourcar", title:"Fluent location(yourcar,7-9,westward) initiated at 6<br/>and terminated at transition to 7"}, _204892{content:"yourcar,8-9,westward", end:6, group:"location/3", id:32, start:5, subgroup:"yourcar", title:"Fluent location(yourcar,8-9,westward) initiated at 5<br/>and terminated at transition to 6"}, _205018{content:"yourcar,9-9,westward", end:5, group:"location/3", id:33, start:1, subgroup:"yourcar", title:"Fluent location(yourcar,9-9,westward) initiated at 1<br/>and terminated at transition to 5"}, _205144{content:"destination(mycar,9-9)", group:"event", id:34, start:3, style:"color:#E19735", title:"happens(destination(mycar,9-9),2,3)", type:"point"}, _205270{content:"destination(yourcar,2-1)", group:"event", id:35, start:4, style:"color:#E19735", title:"happens(destination(yourcar,2-1),3,4)", type:"point"}, _205396{content:"step(mycar)", group:"action", id:36, start:4, style:"color:green", title:"happens(step(mycar),3,4)", type:"point"}, _205522{content:"step(mycar)", group:"action", id:37, start:5, style:"color:green", title:"happens(step(mycar),4,5)", type:"point"}, _205648{content:"step(yourcar)", group:"action", id:38, start:5, style:"color:green", title:"happens(step(yourcar),4,5)", type:"point"}, _205774{content:"step(yourcar)", group:"action", id:39, start:6, style:"color:green", title:"happens(step(yourcar),5,6)", type:"point"}, _205900{content:"step(mycar)", group:"action", id:40, start:6, style:"color:green", title:"happens(step(mycar),5,6)", type:"point"}, _206026{content:"step(mycar)", group:"action", id:41, start:7, style:"color:green", title:"happens(step(mycar),6,7)", type:"point"}, _206152{content:"step(yourcar)", group:"action", id:42, start:7, style:"color:green", title:"happens(step(yourcar),6,7)", type:"point"}, _206278{content:"step(yourcar)", group:"action", id:43, start:8, style:"color:green", title:"happens(step(yourcar),7,8)", type:"point"}, _206404{content:"step(mycar)", group:"action", id:44, start:8, style:"color:green", title:"happens(step(mycar),7,8)", type:"point"}, _206530{content:"step(mycar)", group:"action", id:45, start:9, style:"color:green", title:"happens(step(mycar),8,9)", type:"point"}, _206656{content:"step(yourcar)", group:"action", id:46, start:9, style:"color:green", title:"happens(step(yourcar),8,9)", type:"point"}, _206782{content:"step(yourcar)", group:"action", id:47, start:10, style:"color:green", title:"happens(step(yourcar),9,10)", type:"point"}, _206908{content:"step(mycar)", group:"action", id:48, start:10, style:"color:green", title:"happens(step(mycar),9,10)", type:"point"}, _207034{content:"step(mycar)", group:"action", id:49, start:11, style:"color:green", title:"happens(step(mycar),10,11)", type:"point"}, _207160{content:"step(yourcar)", group:"action", id:50, start:11, style:"color:green", title:"happens(step(yourcar),10,11)", type:"point"}, _207286{content:"turn(yourcar,southward)", group:"action", id:51, start:12, style:"color:green", title:"happens(turn(yourcar,southward),11,12)", type:"point"}, _207412{content:"turn(mycar,eastward)", group:"action", id:52, start:12, style:"color:green", title:"happens(turn(mycar,eastward),11,12)", type:"point"}, _207538{content:"step(mycar)", group:"action", id:53, start:13, style:"color:green", title:"happens(step(mycar),12,13)", type:"point"}, _207664{content:"step(yourcar)", group:"action", id:54, start:13, style:"color:green", title:"happens(step(yourcar),12,13)", type:"point"}, _207790{content:"step(yourcar)", group:"action", id:55, start:14, style:"color:green", title:"happens(step(yourcar),13,14)", type:"point"}, _207916{content:"step(mycar)", group:"action", id:56, start:14, style:"color:green", title:"happens(step(mycar),13,14)", type:"point"}, _208042{content:"step(mycar)", group:"action", id:57, start:15, style:"color:green", title:"happens(step(mycar),14,15)", type:"point"}, _208168{content:"step(yourcar)", group:"action", id:58, start:15, style:"color:green", title:"happens(step(yourcar),14,15)", type:"point"}, _208294{content:"step(yourcar)", group:"action", id:59, start:16, style:"color:green", title:"happens(step(yourcar),15,16)", type:"point"}, _208420{content:"step(mycar)", group:"action", id:60, start:16, style:"color:green", title:"happens(step(mycar),15,16)", type:"point"}, _208546{content:"step(mycar)", group:"action", id:61, start:17, style:"color:green", title:"happens(step(mycar),16,17)", type:"point"}, _208672{content:"step(yourcar)", group:"action", id:62, start:17, style:"color:green", title:"happens(step(yourcar),16,17)", type:"point"}, _208798{content:"step(yourcar)", group:"action", id:63, start:18, style:"color:green", title:"happens(step(yourcar),17,18)", type:"point"}, _208924{content:"step(mycar)", group:"action", id:64, start:18, style:"color:green", title:"happens(step(mycar),17,18)", type:"point"}, _209050{content:"step(mycar)", group:"action", id:65, start:19, style:"color:green", title:"happens(step(mycar),18,19)", type:"point"}, _209176{content:"step(yourcar)", group:"action", id:66, start:19, style:"color:green", title:"happens(step(yourcar),18,19)", type:"point"}, _209302{content:"step(yourcar)", group:"action", id:67, start:20, style:"color:green", title:"happens(step(yourcar),19,20)", type:"point"}]}, _326008{cycles:[[_320404{create:[_313182{fillColor:"yellow", from:[10, 10], id:"timeless", to:[20, 20], type:"rectangle"}, _313300{fillColor:"yellow", from:[10, 20], id:"timeless", to:[20, 30], type:"rectangle"}, _313418{fillColor:"yellow", from:[10, 30], id:"timeless", to:[20, 40], type:"rectangle"}, _313536{fillColor:"yellow", from:[10, 40], id:"timeless", to:[20, 50], type:"rectangle"}, _313654{fillColor:"yellow", from:[10, 50], id:"timeless", to:[20, 60], type:"rectangle"}, _313772{fillColor:"yellow", from:[10, 60], id:"timeless", to:[20, 70], type:"rectangle"}, _313890{fillColor:"yellow", from:[10, 70], id:"timeless", to:[20, 80], type:"rectangle"}, _314008{fillColor:"yellow", from:[10, 80], id:"timeless", to:[20, 90], type:"rectangle"}, _314126{fillColor:"yellow", from:[10, 90], id:"timeless", to:[20, 100], type:"rectangle"}, _314244{fillColor:"yellow", from:[10, 100], id:"timeless", to:[20, 110], type:"rectangle"}, _314362{fillColor:"yellow", from:[20, 100], id:"timeless", to:[30, 110], type:"rectangle"}, _314480{fillColor:"yellow", from:[30, 10], id:"timeless", to:[40, 20], type:"rectangle"}, _314598{fillColor:"yellow", from:[30, 20], id:"timeless", to:[40, 30], type:"rectangle"}, _314716{fillColor:"yellow", from:[30, 30], id:"timeless", to:[40, 40], type:"rectangle"}, _314834{fillColor:"yellow", from:[30, 40], id:"timeless", to:[40, 50], type:"rectangle"}, _314952{fillColor:"yellow", from:[30, 60], id:"timeless", to:[40, 70], type:"rectangle"}, _315070{fillColor:"yellow", from:[30, 70], id:"timeless", to:[40, 80], type:"rectangle"}, _315188{fillColor:"yellow", from:[30, 80], id:"timeless", to:[40, 90], type:"rectangle"}, _315306{fillColor:"yellow", from:[30, 100], id:"timeless", to:[40, 110], type:"rectangle"}, _315424{fillColor:"yellow", from:[40, 10], id:"timeless", to:[50, 20], type:"rectangle"}, _315542{fillColor:"yellow", from:[40, 20], id:"timeless", to:[50, 30], type:"rectangle"}, _315660{fillColor:"yellow", from:[40, 30], id:"timeless", to:[50, 40], type:"rectangle"}, _315778{fillColor:"yellow", from:[40, 40], id:"timeless", to:[50, 50], type:"rectangle"}, _315896{fillColor:"yellow", from:[40, 60], id:"timeless", to:[50, 70], type:"rectangle"}, _316014{fillColor:"yellow", from:[40, 70], id:"timeless", to:[50, 80], type:"rectangle"}, _316132{fillColor:"yellow", from:[40, 80], id:"timeless", to:[50, 90], type:"rectangle"}, _316250{fillColor:"yellow", from:[40, 100], id:"timeless", to:[50, 110], type:"rectangle"}, _316368{fillColor:"yellow", from:[50, 10], id:"timeless", to:[60, 20], type:"rectangle"}, _316486{fillColor:"yellow", from:[50, 20], id:"timeless", to:[60, 30], type:"rectangle"}, _316604{fillColor:"yellow", from:[50, 30], id:"timeless", to:[60, 40], type:"rectangle"}, _316722{fillColor:"yellow", from:[50, 40], id:"timeless", to:[60, 50], type:"rectangle"}, _316840{fillColor:"yellow", from:[50, 60], id:"timeless", to:[60, 70], type:"rectangle"}, _316958{fillColor:"yellow", from:[50, 70], id:"timeless", to:[60, 80], type:"rectangle"}, _317076{fillColor:"yellow", from:[50, 80], id:"timeless", to:[60, 90], type:"rectangle"}, _317194{fillColor:"yellow", from:[50, 100], id:"timeless", to:[60, 110], type:"rectangle"}, _317312{fillColor:"yellow", from:[60, 100], id:"timeless", to:[70, 110], type:"rectangle"}, _317430{fillColor:"yellow", from:[70, 10], id:"timeless", to:[80, 20], type:"rectangle"}, _317548{fillColor:"yellow", from:[70, 20], id:"timeless", to:[80, 30], type:"rectangle"}, _317666{fillColor:"yellow", from:[70, 30], id:"timeless", to:[80, 40], type:"rectangle"}, _317784{fillColor:"yellow", from:[70, 40], id:"timeless", to:[80, 50], type:"rectangle"}, _317902{fillColor:"yellow", from:[70, 60], id:"timeless", to:[80, 70], type:"rectangle"}, _318020{fillColor:"yellow", from:[70, 70], id:"timeless", to:[80, 80], type:"rectangle"}, _318138{fillColor:"yellow", from:[70, 80], id:"timeless", to:[80, 90], type:"rectangle"}, _318256{fillColor:"yellow", from:[70, 100], id:"timeless", to:[80, 110], type:"rectangle"}, _318374{fillColor:"yellow", from:[80, 100], id:"timeless", to:[90, 110], type:"rectangle"}, _318492{fillColor:"yellow", from:[90, 10], id:"timeless", to:[100, 20], type:"rectangle"}, _318610{fillColor:"yellow", from:[90, 20], id:"timeless", to:[100, 30], type:"rectangle"}, _318728{fillColor:"yellow", from:[90, 30], id:"timeless", to:[100, 40], type:"rectangle"}, _318846{fillColor:"yellow", from:[90, 40], id:"timeless", to:[100, 50], type:"rectangle"}, _318964{fillColor:"yellow", from:[90, 60], id:"timeless", to:[100, 70], type:"rectangle"}, _319082{fillColor:"yellow", from:[90, 70], id:"timeless", to:[100, 80], type:"rectangle"}, _319200{fillColor:"yellow", from:[90, 80], id:"timeless", to:[100, 90], type:"rectangle"}, _319318{fillColor:"yellow", from:[90, 100], id:"timeless", to:[100, 110], type:"rectangle"}, _319436{fillColor:"yellow", from:[100, 10], id:"timeless", to:[110, 20], type:"rectangle"}, _319554{fillColor:"yellow", from:[100, 20], id:"timeless", to:[110, 30], type:"rectangle"}, _319672{fillColor:"yellow", from:[100, 30], id:"timeless", to:[110, 40], type:"rectangle"}, _319790{fillColor:"yellow", from:[100, 40], id:"timeless", to:[110, 50], type:"rectangle"}, _319908{fillColor:"yellow", from:[100, 60], id:"timeless", to:[110, 70], type:"rectangle"}, _320026{fillColor:"yellow", from:[100, 70], id:"timeless", to:[110, 80], type:"rectangle"}, _320144{fillColor:"yellow", from:[100, 80], id:"timeless", to:[110, 90], type:"rectangle"}, _320262{fillColor:"yellow", from:[100, 90], id:"timeless", to:[110, 100], type:"rectangle"}, _320380{fillColor:"yellow", from:[100, 100], id:"timeless", to:[110, 110], type:"rectangle"}]}], [_320540{create:_320516{center:[25, 15], fillColor:"blue", id:"location(mycar,2-1,northward)", radius:5, type:"circle"}}, _320670{create:_320646{center:[95, 95], fillColor:"red", id:"location(yourcar,9-9,westward)", radius:5, type:"circle"}}], [], [], [_320818{create:_320794{center:[25, 25], fillColor:"blue", id:"location(mycar,2-2,northward)", radius:5, type:"circle"}}, _320848{kill:"location(mycar,2-1,northward)"}], [_320984{create:_320960{center:[25, 35], fillColor:"blue", id:"location(mycar,2-3,northward)", radius:5, type:"circle"}}, _321114{create:_321090{center:[85, 95], fillColor:"red", id:"location(yourcar,8-9,westward)", radius:5, type:"circle"}}, _321144{kill:"location(mycar,2-2,northward)"}, _321174{kill:"location(yourcar,9-9,westward)"}], [_321310{create:_321286{center:[25, 45], fillColor:"blue", id:"location(mycar,2-4,northward)", radius:5, type:"circle"}}, _321440{create:_321416{center:[75, 95], fillColor:"red", id:"location(yourcar,7-9,westward)", radius:5, type:"circle"}}, _321470{kill:"location(mycar,2-3,northward)"}, _321500{kill:"location(yourcar,8-9,westward)"}], [_321636{create:_321612{center:[25, 55], fillColor:"blue", id:"location(mycar,2-5,northward)", radius:5, type:"circle"}}, _321766{create:_321742{center:[65, 95], fillColor:"red", id:"location(yourcar,6-9,westward)", radius:5, type:"circle"}}, _321796{kill:"location(mycar,2-4,northward)"}, _321826{kill:"location(yourcar,7-9,westward)"}], [_321962{create:_321938{center:[25, 65], fillColor:"blue", id:"location(mycar,2-6,northward)", radius:5, type:"circle"}}, _322092{create:_322068{center:[55, 95], fillColor:"red", id:"location(yourcar,5-9,westward)", radius:5, type:"circle"}}, _322122{kill:"location(mycar,2-5,northward)"}, _322152{kill:"location(yourcar,6-9,westward)"}], [_322288{create:_322264{center:[25, 75], fillColor:"blue", id:"location(mycar,2-7,northward)", radius:5, type:"circle"}}, _322418{create:_322394{center:[45, 95], fillColor:"red", id:"location(yourcar,4-9,westward)", radius:5, type:"circle"}}, _322448{kill:"location(mycar,2-6,northward)"}, _322478{kill:"location(yourcar,5-9,westward)"}], [_322614{create:_322590{center:[25, 85], fillColor:"blue", id:"location(mycar,2-8,northward)", radius:5, type:"circle"}}, _322744{create:_322720{center:[35, 95], fillColor:"red", id:"location(yourcar,3-9,westward)", radius:5, type:"circle"}}, _322774{kill:"location(mycar,2-7,northward)"}, _322804{kill:"location(yourcar,4-9,westward)"}], [_322940{create:_322916{center:[25, 95], fillColor:"blue", id:"location(mycar,2-9,northward)", radius:5, type:"circle"}}, _323070{create:_323046{center:[25, 95], fillColor:"red", id:"location(yourcar,2-9,westward)", radius:5, type:"circle"}}, _323100{kill:"location(mycar,2-8,northward)"}, _323130{kill:"location(yourcar,3-9,westward)"}], [_323266{create:_323242{center:[25, 95], fillColor:"blue", id:"location(mycar,2-9,eastward)", radius:5, type:"circle"}}, _323396{create:_323372{center:[25, 95], fillColor:"red", id:"location(yourcar,2-9,southward)", radius:5, type:"circle"}}, _323426{kill:"location(mycar,2-9,northward)"}, _323456{kill:"location(yourcar,2-9,westward)"}], [_323592{create:_323568{center:[35, 95], fillColor:"blue", id:"location(mycar,3-9,eastward)", radius:5, type:"circle"}}, _323722{create:_323698{center:[25, 85], fillColor:"red", id:"location(yourcar,2-8,southward)", radius:5, type:"circle"}}, _323752{kill:"location(mycar,2-9,eastward)"}, _323782{kill:"location(yourcar,2-9,southward)"}], [_323918{create:_323894{center:[45, 95], fillColor:"blue", id:"location(mycar,4-9,eastward)", radius:5, type:"circle"}}, _324048{create:_324024{center:[25, 75], fillColor:"red", id:"location(yourcar,2-7,southward)", radius:5, type:"circle"}}, _324078{kill:"location(mycar,3-9,eastward)"}, _324108{kill:"location(yourcar,2-8,southward)"}], [_324244{create:_324220{center:[55, 95], fillColor:"blue", id:"location(mycar,5-9,eastward)", radius:5, type:"circle"}}, _324374{create:_324350{center:[25, 65], fillColor:"red", id:"location(yourcar,2-6,southward)", radius:5, type:"circle"}}, _324404{kill:"location(mycar,4-9,eastward)"}, _324434{kill:"location(yourcar,2-7,southward)"}], [_324570{create:_324546{center:[65, 95], fillColor:"blue", id:"location(mycar,6-9,eastward)", radius:5, type:"circle"}}, _324700{create:_324676{center:[25, 55], fillColor:"red", id:"location(yourcar,2-5,southward)", radius:5, type:"circle"}}, _324730{kill:"location(mycar,5-9,eastward)"}, _324760{kill:"location(yourcar,2-6,southward)"}], [_324896{create:_324872{center:[75, 95], fillColor:"blue", id:"location(mycar,7-9,eastward)", radius:5, type:"circle"}}, _325026{create:_325002{center:[25, 45], fillColor:"red", id:"location(yourcar,2-4,southward)", radius:5, type:"circle"}}, _325056{kill:"location(mycar,6-9,eastward)"}, _325086{kill:"location(yourcar,2-5,southward)"}], [_325222{create:_325198{center:[85, 95], fillColor:"blue", id:"location(mycar,8-9,eastward)", radius:5, type:"circle"}}, _325352{create:_325328{center:[25, 35], fillColor:"red", id:"location(yourcar,2-3,southward)", radius:5, type:"circle"}}, _325382{kill:"location(mycar,7-9,eastward)"}, _325412{kill:"location(yourcar,2-4,southward)"}], [_325548{create:_325524{center:[95, 95], fillColor:"blue", id:"location(mycar,9-9,eastward)", radius:5, type:"circle"}}, _325678{create:_325654{center:[25, 25], fillColor:"red", id:"location(yourcar,2-2,southward)", radius:5, type:"circle"}}, _325708{kill:"location(mycar,8-9,eastward)"}, _325738{kill:"location(yourcar,2-3,southward)"}], [_325874{create:_325850{center:[25, 15], fillColor:"red", id:"location(yourcar,2-1,southward)", radius:5, type:"circle"}}, _325904{kill:"location(yourcar,2-2,southward)"}], [_325940{kill:"location(mycar,9-9,eastward)"}, _325970{kill:"location(yourcar,2-1,southward)"}, _326000{kill:"timeless"}]]})).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'semantic_graphs.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'semantic_graphs.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/semantic_graphs.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'semantic_graphs.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'semantic_graphs.pl'), lps= /.../(lps_user_examples, 'semantic_graphs.pl'), using= /.../(lps_user_examples, 'semantic_graphs.pl')].
% continue_lps_dialect.
% ops.
% /pack/logicmoo_ec/test/lps_user_examples/semantic_graphs.pl:17
% pop_lps_dialect('$BLOB'("<stream>(0x562ef33a9400)"),  (/.../(lps_user_examples, 'semantic_graphs.pl')-> /.../(lps_user_examples, 'semantic_graphs.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/semantic_graphs.pl':_104690).


lf_graph(LogicalForm, dot(digraph(Edges))) :-
    findall(edge((From->To), [label=Relation]),
            ( member(P, LogicalForm),
              P=..[Relation, From_, To_],
              term_string(From_, From),
              term_string(To_, To)
            ),
            Edges).

:- dynamic'swish renderer'/2.

'swish renderer'(graphviz, []).

:- dynamic actions/1.
:- multifile actions/1.

% dB(/.../(lps_user_examples, 'semantic_graphs.pl'), lps_visualization(_44390{groups:[], items:[]}, [])).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'Simpified contaminated doesn\'t work.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'Simpified contaminated doesn\'t work.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/Simpified contaminated doesn't work.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'Simpified contaminated doesn\'t work.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'Simpified contaminated doesn\'t work.pl'), lps= /.../(lps_user_examples, 'Simpified contaminated doesn\'t work.pl'), using= /.../(lps_user_examples, 'Simpified contaminated doesn\'t work.pl')].
% continue_lps_dialect.
% ops.

% LPS:  fluents((contaminated/3,infected/3,met/3,tested/3)).
% Into: fluents([contaminated(_21328,_21330,_21332),infected(_21342,_21344,_21346),met(_21356,_21358,_21360),tested(_21370,_21372,_21374)]).

% LPS:  events((test(_21226,_21228,_21230),meets(_21226,_21300,_21230))).
% Into: events([test(_21226,_21228,_21230),meets(_21226,_21300,_21230)]).

% LPS:  actions(notify/3).
% Into: actions([notify(_23482,_23484,_23486)]).

% LPS:  initiates(meets(_23498,_23500,_23502),met(_23498,_23500,_23502)).
% Into: initiated(happens(meets(_23498,_23500,_23502),_24722,_24728),met(_23498,_23500,_23502),[]).

% LPS:  initiates(meets(_24678,_24680,_24682),met(_24680,_24678,_24682)).
% Into: initiated(happens(meets(_24678,_24680,_24682),_25902,_25908),met(_24680,_24678,_24682),[]).

% LPS:  initiates(test(_25858,_25860,_25862),tested(_25858,_25860,_25862)).
% Into: initiated(happens(test(_25858,_25860,_25862),_27082,_27088),tested(_25858,_25860,_25862),[]).

% LPS:  if(contaminated(_27082,_27084,_27086),(tested(_27082,positive,_27158),two_week_after(_27158,_27086),five_days_before(_27084,_27158))).
% Into: l_int(holds(contaminated(_27082,_27084,_27086),_28428),[holds(tested(_27082,positive,_27158),_28428),two_week_after(_27158,_27086),five_days_before(_27084,_27158)]).

% LPS:  if(contaminated(_29640,_29642,_29644),(met(_29640,_29714,_29642),contaminated(_29714,_29786,_29788),within(_29786,_29642,_29788),five_days_after(_29642,_29916),two_week_after(_29642,_29644))).
% Into: l_int(holds(contaminated(_29640,_29642,_29644),_31258),[holds(met(_29640,_29714,_29642),_31258),holds(contaminated(_29714,_29786,_29788),_31258),within(_29786,_29642,_29788),five_days_after(_29642,_29916),two_week_after(_29642,_29644)]).

% LPS:  observe(from(meets(gertrude,alice,date(2020,3,15,0,0,0,0,'UTC',-)),to(2,3))).
% Into: observe([meets(gertrude,alice,date(2020,3,15,0,0,0,0,'UTC',-))],3).

% LPS:  observe(from(test(alice,positive,date(2020,3,14,0,0,0,0,'UTC',-)),to(4,5))).
% Into: observe([test(alice,positive,date(2020,3,14,0,0,0,0,'UTC',-))],5).
% /pack/logicmoo_ec/test/lps_user_examples/Simpified contaminated doesn't work.pl:109
% pop_lps_dialect('$BLOB'("<stream>(0x562ef3ca4f00)"),  (/.../(lps_user_examples, 'Simpified contaminated doesn\'t work.pl')-> /.../(lps_user_examples, 'Simpified contaminated doesn\'t work.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/Simpified contaminated doesn\'t work.pl':_59124).


initiated(happens(meets(A, B, C), _, _), met(A, B, C), []).
initiated(happens(meets(A, B, C), _, _), met(B, A, C), []).
initiated(happens(test(A, B, C), _, _), tested(A, B, C), []).

fluents([contaminated(_, _, _), infected(_, _, _), met(_, _, _), tested(_, _, _)]).

l_int(holds(contaminated(A, B, C), D), [holds(tested(A, positive, E), D), two_week_after(E, C), five_days_before(B, E)]).
l_int(holds(contaminated(A, B, C), D), [holds(met(A, E, B), D), holds(contaminated(E, F, G), D), within(F, B, G), five_days_after(B, _), two_week_after(B, C)]).

two_week_after(date(Y, M, D, H, Mn, S, Off, TZ, DST), Date2) :-
    nonvar(D),
    NewD is D+15,
    date_time_stamp(date(Y,
                         M,
                         NewD,
                         H,
                         Mn,
                         S,
                         Off,
                         TZ,
                         DST),
                    Stamp),
    stamp_date_time(Stamp, Date2, 0).
two_week_after(Date2, date(Y, M, D, H, Mn, S, Off, TZ, DST)) :-
    nonvar(D),
    NewD is D+ -15,
    date_time_stamp(date(Y,
                         M,
                         NewD,
                         H,
                         Mn,
                         S,
                         Off,
                         TZ,
                         DST),
                    Stamp),
    stamp_date_time(Stamp, Date2, 0).

within(Date1, T, Date2) :-
    nonvar(Date1),
    nonvar(Date2),
    nonvar(T),
    date_time_stamp(Date1, Stamp1),
    date_time_stamp(Date2, Stamp2),
    date_time_stamp(T, Stamp3),
    Stamp1=<Stamp3,
    Stamp3=<Stamp2.

maxtime(20).

five_days_before(date(Y, M, D, H, Mn, S, Off, TZ, DST), Date2) :-
    nonvar(D),
    NewD is D+5,
    date_time_stamp(date(Y,
                         M,
                         NewD,
                         H,
                         Mn,
                         S,
                         Off,
                         TZ,
                         DST),
                    Stamp),
    stamp_date_time(Stamp, Date2, 0).
five_days_before(Date2, date(Y, M, D, H, Mn, S, Off, TZ, DST)) :-
    nonvar(D),
    NewD is D+ -5,
    date_time_stamp(date(Y,
                         M,
                         NewD,
                         H,
                         Mn,
                         S,
                         Off,
                         TZ,
                         DST),
                    Stamp),
    stamp_date_time(Stamp, Date2, 0).

:- dynamic actions/1.
:- multifile actions/1.

actions([notify(_, _, _)]).

five_days_after(date(Y, M, D, H, Mn, S, Off, TZ, DST), Date2) :-
    nonvar(D),
    NewD is D+5,
    date_time_stamp(date(Y,
                         M,
                         NewD,
                         H,
                         Mn,
                         S,
                         Off,
                         TZ,
                         DST),
                    Stamp),
    stamp_date_time(Stamp, Date2, 0).
five_days_after(Date2, date(Y, M, D, H, Mn, S, Off, TZ, DST)) :-
    nonvar(D),
    NewD is D+ -5,
    date_time_stamp(date(Y,
                         M,
                         NewD,
                         H,
                         Mn,
                         S,
                         Off,
                         TZ,
                         DST),
                    Stamp),
    stamp_date_time(Stamp, Date2, 0).

events([test(A, _, B), meets(A, _, B)]).

observe([meets(gertrude, alice, date(2020, 3, 15, 0, 0, 0, 0, 'UTC', -))], 3).
observe([test(alice, positive, date(2020, 3, 14, 0, 0, 0, 0, 'UTC', -))], 5).
% dB(/.../(lps_user_examples, 'Simpified contaminated doesn\'t work.pl'), lps_visualization(_71492{groups:[_70678{content:"Events", id:"event", order:1}, _70752{content:"met(A,B,C)", id:"met/3", order:3, subgroupStack:"false"}, _70830{content:"tested(A,B,C)", id:"tested/3", order:3, subgroupStack:"false"}], items:[_70956{content:"alice,gertrude,date(2020,3,15,0,0,0,0,UTC,-)", end:21, group:"met/3", id:0, start:3, subgroup:"alice", title:"Fluent met(alice,gertrude,date(2020,3,15,0,0,0,0,UTC,-)) initiated at 3<br/>and terminated at transition to 21"}, _71082{content:"gertrude,alice,date(2020,3,15,0,0,0,0,UTC,-)", end:21, group:"met/3", id:1, start:3, subgroup:"gertrude", title:"Fluent met(gertrude,alice,date(2020,3,15,0,0,0,0,UTC,-)) initiated at 3<br/>and terminated at transition to 21"}, _71208{content:"alice,positive,date(2020,3,14,0,0,0,0,UTC,-)", end:21, group:"tested/3", id:2, start:5, subgroup:"alice", title:"Fluent tested(alice,positive,date(2020,3,14,0,0,0,0,UTC,-)) initiated at 5<br/>and terminated at transition to 21"}, _71334{content:"meets(gertrude,alice,date(2020,3,15,0,0,0,0,UTC,-))", group:"event", id:3, start:3, style:"color:#E19735", title:"happens(meets(gertrude,alice,date(2020,3,15,0,0,0,0,UTC,-)),2,3)", type:"point"}, _71460{content:"test(alice,positive,date(2020,3,14,0,0,0,0,UTC,-))", group:"event", id:4, start:5, style:"color:#E19735", title:"happens(test(alice,positive,date(2020,3,14,0,0,0,0,UTC,-)),4,5)", type:"point"}]}, [])).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'simple conveyor.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'simple conveyor.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/simple conveyor.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'simple conveyor.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'simple conveyor.pl'), lps= /.../(lps_user_examples, 'simple conveyor.pl'), using= /.../(lps_user_examples, 'simple conveyor.pl')].
% continue_lps_dialect.
% ops.

% LPS:  fluents((pouring(_55194),contents(_55194,_55250),location(_55304,_55306),moving(_55346))).
% Into: fluents([pouring(_55194),contents(_55194,_55250),location(_55304,_55306),moving(_55346)]).

% LPS:  initially((contents(bottle,0),contents(container,0),contents(tank1,100),contents(tank2,100),contents(heaven,1000),location(bottle,160),location(tank1,160),location(tank2,240),location(container,400))).
% Into: initial_state([contents(bottle,0),contents(container,0),contents(tank1,100),contents(tank2,100),contents(heaven,1000),location(bottle,160),location(tank1,160),location(tank2,240),location(container,400)]).

% LPS:  actions((openValve(_58406),closeValve(_58406),pourChunk(_58500,_58502),startConveyor(_58542),stopConveyor,turnConveyor/1)).
% Into: actions([openValve(_58406),closeValve(_58406),pourChunk(_58500,_58502),startConveyor(_58542),stopConveyor,turnConveyor(_60084)]).

% LPS:  false((closeValve(_59980),not(pouring(_59980)))).
% Into: d_pre([happens(closeValve(_59980),_61108,_61114),holds(not(pouring(_59980)),_61108)]).

% LPS:  if(from(makeLocation(bottle,_61720),to(_61756,_61756)),(at(location(bottle,_61872),_61756),at(location(_61720,_61872),_61756))).
% Into: l_events(happens(makeLocation(bottle,_61720),_61756,_61756),[holds(location(bottle,_61872),_61756),holds(location(_61720,_61872),_61756)]).

% LPS:  if(from(makeLocation(bottle,_63776),to(_63812,_63814)),(at(location(bottle,_63928),_63812),at(location(_63776,_64032),_63812),_64164 is _64032-_63928,stopPlace(_64164,_64032,_64264),from(startConveyor(_64164),_63812),at(location(bottle,_64264),_64430),at(moving(_64164),_64430),from(stopConveyor,to(_64430,_63814)))).
% Into: l_events(happens(makeLocation(bottle,_63776),_63812,_63814),[holds(location(bottle,_63928),_63812),holds(location(_63776,_64032),_63812),_64164 is _64032-_63928,stopPlace(_64164,_64032,_64264),happens(startConveyor(_64164),_63812,_66910),holds(location(bottle,_64264),_64430),holds(moving(_64164),_64430),happens(stopConveyor,_64430,_63814)]).

% LPS:  initiates(startConveyor(_70524),moving(_70524)).
% Into: initiated(happens(startConveyor(_70524),_71672,_71678),moving(_70524),[]).

% LPS:  terminates(stopConveyor,moving(_71624)).
% Into: terminated(happens(stopConveyor,_72712,_72718),moving(_71624),[]).

% LPS:  then(if((at(moving(_72660),_72682),_72660>0)),from(turnConveyor(clockwise),_72682)).
% Into: reactive_rule([holds(moving(_72660),_72682),_72660>0],[happens(turnConveyor(clockwise),_72682,_74514)]).

% LPS:  then(if((at(moving(_74560),_74582),_74560<0)),from(turnConveyor(counterClockwise),_74582)).
% Into: reactive_rule([holds(moving(_74560),_74582),_74560<0],[happens(turnConveyor(counterClockwise),_74582,_76410)]).

% LPS:  if(updates(turnConveyor(counterClockwise),in(to(_18990,_18992),location(bottle,_18990))),(conveyorSpeed(_19146),_18992 is _18990-_19146)).
% Into: updated(happens(turnConveyor(counterClockwise),_19724,_19730),location(bottle,_18990),_18990-_18992,[conveyorSpeed(_19146),_18992 is _18990-_19146]).

% LPS:  if(updates(turnConveyor(clockwise),in(to(_20206,_20208),location(bottle,_20206))),(conveyorSpeed(_20394),_20208 is _20206+_20394)).
% Into: updated(happens(turnConveyor(clockwise),_21776,_21782),location(bottle,_20206),_20206-_20208,[conveyorSpeed(_20394),_20208 is _20206+_20394]).

% LPS:  then(if((at(contents(bottle,0),_23710),at(location(tank1,_23792),_23710),at(location(bottle,_23792),_23710),at(not(pouring(_23992)),_23710))),(from(pour(tank1,bottle,50),to(_23710,_24294)),from(makeLocation(bottle,tank2),to(_24294,_24446)),from(pour(tank2,bottle,50),to(_24446,_24614)),from(makeLocation(bottle,container),to(_24614,_24766)),from(pour(bottle,container,100),to(_24766,_24934)),from(makeLocation(bottle,tank1),_24934))).
% Into: reactive_rule([holds(contents(bottle,0),_23710),holds(location(tank1,_23792),_23710),holds(location(bottle,_23792),_23710),holds(not(pouring(_23992)),_23710)],[happens(pour(tank1,bottle,50),_23710,_24294),happens(makeLocation(bottle,tank2),_24294,_24446),happens(pour(tank2,bottle,50),_24446,_24614),happens(makeLocation(bottle,container),_24614,_24766),happens(pour(bottle,container,100),_24766,_24934),happens(makeLocation(bottle,tank1),_24934,_27030)]).

% LPS:  if(from(pour(_26636,_26638,_26640),to(_26676,_26678)),(at(contents(_26638,_26792),_26676),at(contents(_26636,_26896),_26676),_26896>=_26640,_27092 is _26640+_26792,valveRate(_27160),_27244 is _27092-_27160,from(openValve(_26636),to(_26676,_27350)),at(contents(_26638,_27244),_27486),from(closeValve(_26636),to(_27486,_26678)))).
% Into: l_events(happens(pour(_26636,_26638,_26640),_26676,_26678),[holds(contents(_26638,_26792),_26676),holds(contents(_26636,_26896),_26676),_26896>=_26640,_27092 is _26640+_26792,valveRate(_27160),_27244 is _27092-_27160,happens(openValve(_26636),_26676,_27350),holds(contents(_26638,_27244),_27486),happens(closeValve(_26636),_27486,_26678)]).

% LPS:  initiates(openValve(_30496),pouring(_30496)).
% Into: initiated(happens(openValve(_30496),_31644,_31650),pouring(_30496),[]).

% LPS:  terminates(closeValve(_31580),pouring(_31580)).
% Into: terminated(happens(closeValve(_31580),_32728,_32734),pouring(_31580),[]).

% LPS:  then(if((at(pouring(_32672),_32694),at(location(_32672,_32776),_32694),at(location(_32878,_32776),_32694),_32878\=_32672)),from(pourChunk(_32672,_32878),to(_32694,_33206))).
% Into: reactive_rule([holds(pouring(_32672),_32694),holds(location(_32672,_32776),_32694),holds(location(_32878,_32776),_32694),_32878\=_32672],[happens(pourChunk(_32672,_32878),_32694,_33206)]).

% LPS:  if(updates(pourChunk(_35230,_35232),in(to(_35268,_35270),contents(_35232,_35268))),(valveRate(_35456),_35270 is _35268+_35456)).
% Into: updated(happens(pourChunk(_35230,_35232),_36854,_36860),contents(_35232,_35268),_35268-_35270,[valveRate(_35456),_35270 is _35268+_35456]).

% LPS:  if(updates(pourChunk(_37310,_37312),in(to(_37348,_37350),contents(_37310,_37348))),(valveRate(_37536),_37350 is _37348-_37536)).
% Into: updated(happens(pourChunk(_37310,_37312),_38934,_38940),contents(_37310,_37348),_37348-_37350,[valveRate(_37536),_37350 is _37348-_37536]).

% LPS:  if(at(locatedContents(_40878,_40880,_40882),_40904),(at(location(_40878,_40880),_40904),at(contents(_40878,_40882),_40904))).
% Into: l_int(holds(locatedContents(_40878,_40880,_40882),_40904),[holds(location(_40878,_40880),_40904),holds(contents(_40878,_40882),_40904)]).
% /pack/logicmoo_ec/test/lps_user_examples/simple conveyor.pl:180
% pop_lps_dialect('$BLOB'("<stream>(0x562ef33a9200)"),  (/.../(lps_user_examples, 'simple conveyor.pl')-> /.../(lps_user_examples, 'simple conveyor.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/simple conveyor.pl':_61668).


initiated(happens(startConveyor(A), _, _), moving(A), []).
initiated(happens(openValve(A), _, _), pouring(A), []).

d_pre([happens(closeValve(A), B, _), holds(not(pouring(A)), B)]).

d(locatedContents(bottle, Pos, Level), [type:rectangle, fillColor:yellow, from:[X1, 60], to:[X2, Height]]) :-
    Height is 60+Level/4,
    X1 is Pos-10,
    X2 is Pos+10.
d(locatedContents(tank1, Pos, Level), [type:rectangle, fillColor:yellow, from:[130, 120], to:[190, Height]]) :-
    Height is 120+Level/4.
d(locatedContents(tank2, Pos, Level), [type:rectangle, fillColor:yellow, from:[210, 120], to:[270, Height]]) :-
    Height is 120+Level/4.
d(location(bottle, Pos), [type:rectangle, from:[X1, 60], to:[X2, 100], strokeColor:blue]) :-
    X1 is Pos-10,
    X2 is Pos+10.
d(timeless, [[type:line, strokeWidth:2, strokeColor:black, from:[100, 60], to:[400, 60]], [type:circle, strokeWidth:2, strokeColor:black, center:[100, 40], radius:20], [type:circle, strokeWidth:2, strokeColor:black, center:[400, 40], radius:20], [type:rectangle, fillColor:white, from:[130, 120], to:[190, 150], strokeColor:blue], [type:rectangle, fillColor:white, from:[210, 120], to:[270, 150], strokeColor:blue], [type:line, strokeWidth:2, strokeColor:black, from:[100, 20], to:[400, 20]]]).

fluents([pouring(A), contents(A, _), location(_, _), moving(_)]).

l_int(holds(locatedContents(A, B, C), D), [holds(location(A, B), D), holds(contents(A, C), D)]).

terminated(happens(stopConveyor, _, _), moving(_), []).
terminated(happens(closeValve(A), _, _), pouring(A), []).

stopPlace(Vector, Place2, Stop) :-
    conveyorSpeed(S),
    Vector>0,
    Stop is Place2-S.
stopPlace(Vector, Place2, Stop) :-
    conveyorSpeed(S),
    Vector<0,
    Stop is Place2+S.

reactive_rule([holds(moving(A), B), A>0], [happens(turnConveyor(clockwise), B, _)]).
reactive_rule([holds(moving(A), B), A<0], [happens(turnConveyor(counterClockwise), B, _)]).
reactive_rule([holds(contents(bottle, 0), A), holds(location(tank1, B), A), holds(location(bottle, B), A), holds(not(pouring(_)), A)], [happens(pour(tank1, bottle, 50), A, C), happens(makeLocation(bottle, tank2), C, D), happens(pour(tank2, bottle, 50), D, E), happens(makeLocation(bottle, container), E, F), happens(pour(bottle, container, 100), F, G), happens(makeLocation(bottle, tank1), G, _)]).
reactive_rule([holds(pouring(A), B), holds(location(A, C), B), holds(location(D, C), B), D\=A], [happens(pourChunk(A, D), B, _)]).

initial_state([contents(bottle, 0), contents(container, 0), contents(tank1, 100), contents(tank2, 100), contents(heaven, 1000), location(bottle, 160), location(tank1, 160), location(tank2, 240), location(container, 400)]).

l_events(happens(makeLocation(bottle, A), B, B), [holds(location(bottle, C), B), holds(location(A, C), B)]).
l_events(happens(makeLocation(bottle, A), B, C), [holds(location(bottle, D), B), holds(location(A, E), B), F is E-D, stopPlace(F, E, G), happens(startConveyor(F), B, _), holds(location(bottle, G), H), holds(moving(F), H), happens(stopConveyor, H, C)]).
l_events(happens(pour(A, B, C), D, E), [holds(contents(B, F), D), holds(contents(A, G), D), G>=C, H is C+F, valveRate(I), J is H-I, happens(openValve(A), D, _), holds(contents(B, J), K), happens(closeValve(A), K, E)]).

valveRate(10).

:- dynamic actions/1.
:- multifile actions/1.

actions([openValve(A), closeValve(A), pourChunk(_, _), startConveyor(_), stopConveyor, turnConveyor(_)]).

updated(happens(turnConveyor(counterClockwise), _, _), location(bottle, A), A-B, [conveyorSpeed(C), B is A-C]).
updated(happens(turnConveyor(clockwise), _, _), location(bottle, A), A-B, [conveyorSpeed(C), B is A+C]).
updated(happens(pourChunk(_, A), _, _), contents(A, B), B-C, [valveRate(D), C is B+D]).
updated(happens(pourChunk(A, _), _, _), contents(A, B), B-C, [valveRate(D), C is B-D]).

maxTime(160).

conveyorSpeed(10).
PROGRAM FAILED
% dB(/.../(lps_user_examples, 'simple conveyor.pl'), lps_visualization(_68598{groups:[_54912{content:"contents(A,B)", id:"contents/2", order:3, subgroupStack:"false"}, _54938{content:"location(A,B)", id:"location/2", order:3, subgroupStack:"false"}, _54964{content:"moving(A)", id:"moving/1", order:3, subgroupStack:"false"}, _54990{content:"pouring(A)", id:"pouring/1", order:3, subgroupStack:"false"}, _55016{content:"Actions", id:"action", order:4}], items:[_55038{content:"-240", end:75, group:"moving/1", id:0, start:51, subgroup:"-240", title:"Fluent moving(-240) initiated at 51<br/>and terminated at transition to 75"}, _55076{content:"-240", end:149, group:"moving/1", id:1, start:125, subgroup:"-240", title:"Fluent moving(-240) initiated at 125<br/>and terminated at transition to 149"}, _55114{content:"80", end:16, group:"moving/1", id:2, start:8, subgroup:"80", title:"Fluent moving(80) initiated at 8<br/>and terminated at transition to 16"}, ...(_135804)]}, _134822{cycles:[[_117200{create:[_116526{from:[100, 60], id:"timeless", strokeColor:"black", strokeWidth:2, to:[400, 60], type:"line"}, _116648{center:[100, 40], id:"timeless", radius:20, strokeColor:"black", strokeWidth:2, type:"circle"}, _116770{center:[400, 40], id:"timeless", radius:20, strokeColor:"black", strokeWidth:2, type:"circle"}, _116904{fillColor:"white", from:[130, 120], id:"timeless", strokeColor:"blue", to:[190, 150], type:"rectangle"}, _117038{fillColor:"white", from:[210, 120], id:"timeless", strokeColor:"blue", to:[270, 150], type:"rectangle"}, _117172{from:[100, 20], id:"timeless", strokeColor:"black", strokeWidth:2, to:[400, 20], type:"line"}]}], [_117348{create:_117324{from:[150, 60], id:"location(bottle,160)", strokeColor:"blue", to:[170, 100], type:"rectangle"}}], [], ...(_137852)]})).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'simplified checkout.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'simplified checkout.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/simplified checkout.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'simplified checkout.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'simplified checkout.pl'), lps= /.../(lps_user_examples, 'simplified checkout.pl'), using= /.../(lps_user_examples, 'simplified checkout.pl')].
% continue_lps_dialect.
% ops.

% LPS:  fluents(approved/1).
% Into: fluents([approved(_177492)]).

% LPS:  actions((enter/1,approve/1)).
% Into: actions([enter(_179684),approve(_179694)]).

% LPS:  initiates(approve(_179630),approved(_179630)).
% Into: initiated(happens(approve(_179630),_180786,_180792),approved(_179630),[]).

% LPS:  fluents(card/1).
% Into: fluents([card(_181778)]).

% LPS:  initially((card(1),card(2))).
% Into: initial_state([card(1),card(2)]).

% LPS:  then(if(true),(at(card(_183070),_183092),from(enter(_183070),_183092),at(approved(_183070),_183092+2))).
% Into: reactive_rule([],[holds(card(_183070),_183092),happens(enter(_183070),_183092,_185032),holds(approved(_183070),_183092+2)]).

% LPS:  false((enter(_185610),enter(_185650),_185610\=_185650)).
% Into: d_pre([happens(enter(_185610),_186822,_186828),happens(enter(_185650),_186822,_186828),_185610\=_185650]).

% LPS:  then(if(to(enter(2),_187376)),from(approve(2),_187376)).
% Into: reactive_rule([happens(enter(2),_188570,_187376)],[happens(approve(2),_187376,_188660)]).
% /pack/logicmoo_ec/test/lps_user_examples/simplified checkout.pl:35
% pop_lps_dialect('$BLOB'("<stream>(0x562ef4162c00)"),  (/.../(lps_user_examples, 'simplified checkout.pl')-> /.../(lps_user_examples, 'simplified checkout.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/simplified checkout.pl':_196230).


initiated(happens(approve(A), _, _), approved(A), []).

d_pre([happens(enter(A), B, C), happens(enter(D), B, C), A\=D]).

fluents([approved(_)]).
fluents([card(_)]).

reactive_rule([], [holds(card(A), B), happens(enter(A), B, _), holds(approved(A), B+2)]).
reactive_rule([happens(enter(2), _, A)], [happens(approve(2), A, _)]).

initial_state([card(1), card(2)]).

:- dynamic actions/1.
:- multifile actions/1.

actions([enter(_), approve(_)]).

maxTime(5).
% dB(/.../(lps_user_examples, 'simplified checkout.pl'), lps_visualization(_277266{groups:[_276416{content:"card(A)", id:"card/1", order:3, subgroupStack:"false"}, _276482{content:"Actions", id:"action", order:4}], items:[_276604{content:"1", end:6, group:"card/1", id:0, start:1, subgroup:"1", title:"Fluent card(1) initiated at 1<br/>and terminated at transition to 6"}, _276730{content:"2", end:6, group:"card/1", id:1, start:1, subgroup:"2", title:"Fluent card(2) initiated at 1<br/>and terminated at transition to 6"}, _276856{content:"enter(1)", group:"action", id:2, start:2, style:"color:green", title:"happens(enter(1),1,2)", type:"point"}, _276982{content:"enter(1)", group:"action", id:3, start:3, style:"color:green", title:"happens(enter(1),2,3)", type:"point"}, _277108{content:"enter(1)", group:"action", id:4, start:4, style:"color:green", title:"happens(enter(1),3,4)", type:"point"}, _277234{content:"enter(1)", group:"action", id:5, start:5, style:"color:green", title:"happens(enter(1),4,5)", type:"point"}]}, [])).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'Solomon and the two women.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'Solomon and the two women.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/Solomon and the two women.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'Solomon and the two women.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'Solomon and the two women.pl'), lps= /.../(lps_user_examples, 'Solomon and the two women.pl'), using= /.../(lps_user_examples, 'Solomon and the two women.pl')].
% continue_lps_dialect.
% ops.

% LPS:  events((dispute_between(a,b),put_to_test(a,b),propose_drastic_output(agent),make_decision)).
% Into: events([dispute_between(a,b),put_to_test(a,b),propose_drastic_output(agent),make_decision]).

% LPS:  actions((propose_cut_baby(agent),says(agent,message),declares(agent,verdict),decide(agent,judgment))).
% Into: actions([propose_cut_baby(agent),says(agent,message),declares(agent,verdict),decide(agent,judgment)]).

% LPS:  observe(from(dispute_between(a,b),to(1,2))).
% Into: observe([dispute_between(a,b)],2).

% LPS:  then(if(from(dispute_between(_59676,_59678),to(_59714,_59716))),from(put_a_test(_59676,_59678),to(_59716,_59892))).
% Into: reactive_rule([happens(dispute_between(_59676,_59678),_59714,_59716)],[happens(put_a_test(_59676,_59678),_59716,_59892)]).

% LPS:  if(from(put_a_test(_61156,_61158),to(_61194,_61196)),from(proposes_nasty_idea(salomon),to(_61194,_61196))).
% Into: l_events(happens(put_a_test(_61156,_61158),_61194,_61196),[happens(proposes_nasty_idea(salomon),_61194,_61196)]).

% LPS:  if(from(proposes_nasty_idea(_62496),to(_62532,_62534)),from(propose_cut_baby(_62496),to(_62532,_62534))).
% Into: l_events(happens(proposes_nasty_idea(_62496),_62532,_62534),[happens(propose_cut_baby(_62496),_62532,_62534)]).

% LPS:  then(if((from(propose_cut_baby(salomon),to(_63922,_63924)),woman(_64022),i_am_your_mother(_64022))),from(says(_64022,'Dont kill him! Give it to Her'),to(_63924,_64244))).
% Into: reactive_rule([happens(propose_cut_baby(salomon),_63922,_63924),woman(_64022),i_am_your_mother(_64022)],[happens(says(_64022,'Dont kill him! Give it to Her'),_63924,_64244)]).

% LPS:  then(if((from(propose_cut_baby(salomon),to(_66468,_66470)),woman(_66568),not(i_am_your_mother(_66568)))),from(says(_66568,'Yes, kill him'),to(_66470,_66814))).
% Into: reactive_rule([happens(propose_cut_baby(salomon),_66468,_66470),woman(_66568),not(i_am_your_mother(_66568))],[happens(says(_66568,'Yes, kill him'),_66470,_66814)]).

% LPS:  then(if((from(propose_cut_baby(_69510),to(_69546,_69548)),from(says(_69660,'Dont kill him! Give it to Her'),to(_69698,_69700)),from(says(_69812,'Yes, kill him'),to(_69850,_69852)))),(from(declare(_69510,the_true_mother_is(_69660)),to(_70114,_70116)),from(decide(_69510,give_baby_to(_69660)),to(_70116,_70292)))).
% Into: reactive_rule([happens(propose_cut_baby(_69510),_69546,_69548),happens(says(_69660,'Dont kill him! Give it to Her'),_69698,_69700),happens(says(_69812,'Yes, kill him'),_69850,_69852)],[happens(declare(_69510,the_true_mother_is(_69660)),_70114,_70116),happens(decide(_69510,give_baby_to(_69660)),_70116,_70292)]).
% /pack/logicmoo_ec/test/lps_user_examples/Solomon and the two women.pl:81
% pop_lps_dialect('$BLOB'("<stream>(0x562ef33a9000)"),  (/.../(lps_user_examples, 'Solomon and the two women.pl')-> /.../(lps_user_examples, 'Solomon and the two women.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/Solomon and the two women.pl':_83296).


woman(a).
woman(b).

reactive_rule([happens(dispute_between(A, B), _, C)], [happens(put_a_test(A, B), C, _)]).
reactive_rule([happens(propose_cut_baby(salomon), _, A), woman(B), i_am_your_mother(B)], [happens(says(B, 'Dont kill him! Give it to Her'), A, _)]).
reactive_rule([happens(propose_cut_baby(salomon), _, A), woman(B), not(i_am_your_mother(B))], [happens(says(B, 'Yes, kill him'), A, _)]).
reactive_rule([happens(propose_cut_baby(A), _, _), happens(says(B, 'Dont kill him! Give it to Her'), _, _), happens(says(_, 'Yes, kill him'), _, _)], [happens(declare(A, the_true_mother_is(B)), _, C), happens(decide(A, give_baby_to(B)), C, _)]).

l_events(happens(put_a_test(_, _), A, B), [happens(proposes_nasty_idea(salomon), A, B)]).
l_events(happens(proposes_nasty_idea(A), B, C), [happens(propose_cut_baby(A), B, C)]).

maxtime(10).

i_am_your_mother(b).

:- dynamic actions/1.
:- multifile actions/1.

actions([propose_cut_baby(agent), says(agent, message), declares(agent, verdict), decide(agent, judgment)]).

events([dispute_between(a, b), put_to_test(a, b), propose_drastic_output(agent), make_decision]).

observe([dispute_between(a, b)], 2).
PROGRAM FAILED
% dB(/.../(lps_user_examples, 'Solomon and the two women.pl'), lps_visualization(_44068{groups:[_43914{content:"Events", id:"event", order:1}], items:[_44036{content:"dispute_between(a,b)", group:"event", id:0, start:2, style:"color:#E19735", title:"happens(dispute_between(a,b),1,2)", type:"point"}]}, [])).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'SOS.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'SOS.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/SOS.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'SOS.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'SOS.pl'), lps= /.../(lps_user_examples, 'SOS.pl'), using= /.../(lps_user_examples, 'SOS.pl')].
% continue_lps_dialect.
% ops.

% LPS:  fluents((lightOn,lightOff)).
% Into: fluents([lightOn,lightOff]).

% LPS:  events(switch).
% Into: events([switch]).

% LPS:  actions(switch).
% Into: actions([switch]).

% LPS:  if(lightOff,not(lightOn)).
% Into: l_int(holds(lightOff,_23262),[holds(not(lightOn),_23262)]).

% LPS:  if(from(sos,to(_23726,_23728)),(at(lightOff,_23726),from(makeOn,to(_23726,_23904)),from(makeOn,to(_23904,_24016)),from(makeOn,to(_24016,_23728)))).
% Into: l_events(happens(sos,_23726,_23728),[holds(lightOff,_23726),happens(makeOn,_23726,_23904),happens(makeOn,_23904,_24016),happens(makeOn,_24016,_23728)]).

% LPS:  if(from(makeOn,to(_25856,_25858)),(from(switch,to(_25856,_25970)),from(switch,to(_25970+1,_25858)))).
% Into: l_events(happens(makeOn,_25856,_25858),[happens(switch,_25856,_25970),happens(switch,_25970+1,_25858)]).

% LPS:  then(if(true),sos).
% Into: reactive_rule([],[happens(sos,_28388,_28394)]).

% LPS:  if(initiates(switch,lightOn),lightOff).
% Into: initiated(happens(switch,_29554,_29560),lightOn,[holds(lightOff,_29554)]).

% LPS:  if(terminates(switch,lightOn),lightOn).
% Into: terminated(happens(switch,_30624,_30630),lightOn,[holds(lightOn,_30624)]).
% /pack/logicmoo_ec/test/lps_user_examples/SOS.pl:38
% pop_lps_dialect('$BLOB'("<stream>(0x562ef3ca5a00)"),  (/.../(lps_user_examples, 'SOS.pl')-> /.../(lps_user_examples, 'SOS.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/SOS.pl':_38180).


initiated(happens(switch, A, _), lightOn, [holds(lightOff, A)]).

fluents([lightOn, lightOff]).

l_int(holds(lightOff, A), [holds(not(lightOn), A)]).

reactive_rule([], [happens(sos, _, _)]).

terminated(happens(switch, A, _), lightOn, [holds(lightOn, A)]).

l_events(happens(sos, A, B), [holds(lightOff, A), happens(makeOn, A, C), happens(makeOn, C, D), happens(makeOn, D, B)]).
l_events(happens(makeOn, A, B), [happens(switch, A, C), happens(switch, C+1, B)]).

:- dynamic actions/1.
:- multifile actions/1.

actions([switch]).

events([switch]).

maxTime(25).
% dB(/.../(lps_user_examples, 'SOS.pl'), lps_visualization(_65570{groups:[_64390{content:"lightOn", id:"lightOn/0", order:3, subgroupStack:"false"}, _64456{content:"Actions", id:"action", order:4}], items:[_64566{content:"lightOn", end:4, group:"lightOn/0", id:0, start:2, title:"Fluent lightOn initiated at 2<br/>and terminated at transition to 4"}, _64676{content:"lightOn", end:7, group:"lightOn/0", id:1, start:5, title:"Fluent lightOn initiated at 5<br/>and terminated at transition to 7"}, _64786{content:"lightOn", end:10, group:"lightOn/0", id:2, start:8, title:"Fluent lightOn initiated at 8<br/>and terminated at transition to 10"}, _64908{content:"switch", group:"action", id:3, start:2, style:"color:green", title:"happens(switch,1,2)", type:"point"}, _65034{content:"switch", group:"action", id:4, start:4, style:"color:green", title:"happens(switch,3,4)", type:"point"}, _65160{content:"switch", group:"action", id:5, start:5, style:"color:green", title:"happens(switch,4,5)", type:"point"}, _65286{content:"switch", group:"action", id:6, start:7, style:"color:green", title:"happens(switch,6,7)", type:"point"}, _65412{content:"switch", group:"action", id:7, start:8, style:"color:green", title:"happens(switch,7,8)", type:"point"}, _65538{content:"switch", group:"action", id:8, start:10, style:"color:green", title:"happens(switch,9,10)", type:"point"}]}, [])).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, '123 contrato.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, '123 contrato.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/123 contrato.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, '123 contrato.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, '123 contrato.pl'), lps= /.../(lps_user_examples, '123 contrato.pl'), using= /.../(lps_user_examples, '123 contrato.pl')].
% continue_lps_dialect.
% ops.

% LPS:  actions(pagar(comprador,cantidad,dueño)).
% Into: actions([pagar(comprador,cantidad,dueño)]).

% LPS:  events(causa(personal)).
% Into: events([causa(personal)]).

% LPS:  fluents((validar(comprador),por_pagar(cantidad))).
% Into: fluents([validar(comprador),por_pagar(cantidad)]).

% LPS:  at(opcompra(comprador,cantidad,dueño),_64238).
% Into: l_int(holds(opcompra(comprador,cantidad,dueño),_64238),[]).

% LPS:  then(if((causa(personal),to(validar(comprador),_65532))),cantidad is acordado).
% Into: reactive_rule([happens(causa(personal),_66740,_66782),happens(validar(comprador),_66804,_65532)],[cantidad is acordado]).

% LPS:  then(if((causa(personal),to(validar(comprador),_67190))),from(pagar(dueño,cantidad,comprador),to(_67190,_67382))).
% Into: reactive_rule([happens(causa(personal),_68528,_68570),happens(validar(comprador),_68592,_67190)],[happens(pagar(dueño,cantidad,comprador),_67190,_67382)]).
ERROR: /pack/logicmoo_ec/test/lps_user_examples/123 contrato.pl:25:15: Syntax error: Operator expected
% /pack/logicmoo_ec/test/lps_user_examples/123 contrato.pl:29
% pop_lps_dialect('$BLOB'("<stream>(0x562ef4161a00)"),  (/.../(lps_user_examples, '123 contrato.pl')-> /.../(lps_user_examples, '123 contrato.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/123 contrato.pl':_77804).


total(_).

fluents([validar(comprador), por_pagar(cantidad)]).

l_int(holds(opcompra(comprador, cantidad, dueño), _), []).

reactive_rule([happens(causa(personal), _, _), happens(validar(comprador), _, _)], [cantidad is acordado]).
reactive_rule([happens(causa(personal), _, _), happens(validar(comprador), _, A)], [happens(pagar(dueño, cantidad, comprador), A, _)]).

cantidad(_).

:- dynamic actions/1.
:- multifile actions/1.

actions([pagar(comprador, cantidad, dueño)]).

events([causa(personal)]).

maxTime(10).

acordado(_).

if(nCuenta is total-cantidad).
% dB(/.../(lps_user_examples, '123 contrato.pl'), lps_visualization(_70132{groups:[], items:[]}, [])).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'Acompaña.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'Acompaña.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/Acompaña.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'Acompaña.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'Acompaña.pl'), lps= /.../(lps_user_examples, 'Acompaña.pl'), using= /.../(lps_user_examples, 'Acompaña.pl')].
% continue_lps_dialect.
% ops.
% /pack/logicmoo_ec/test/lps_user_examples/Acompaña.pl:26
% pop_lps_dialect('$BLOB'("<stream>(0x562ef31d3300)"),  (/.../(lps_user_examples, 'Acompaña.pl')-> /.../(lps_user_examples, 'Acompaña.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/Acompaña.pl':_56128).


acompaña(X) :-
    plato(X).

:- dynamic actions/1.
:- multifile actions/1.


plato(Arroz).

acompaña(Hijo, Padre).
acompaña(Padre, Hijo).
acompaña(Madre, Hijo).
acompaña(Hijo, Madre).
acompaña(Hombre, Mujer).
acompaña(Mujer, Hombre).
acompaña(Respecto, Rezar).
acompaña(Rezar, Respecto).
acompaña(Arepa, Desayuno).
acompaña(Desayuno, arepa).
acompaña(Almuerzo, Arroz).
acompaña(Arroz, Almuerzo).
acompaña(Persona, mascota).
acompaña(mascota, Persona).
acompaña(amigo, feliz).
acompaña(Feliz, amigo).
% dB(/.../(lps_user_examples, 'Acompaña.pl'), lps_visualization(_37388{groups:[], items:[]}, [])).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'acompa.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'acompa.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/acompa.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'acompa.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'acompa.pl'), lps= /.../(lps_user_examples, 'acompa.pl'), using= /.../(lps_user_examples, 'acompa.pl')].
% continue_lps_dialect.
% ops.
% /pack/logicmoo_ec/test/lps_user_examples/acompa.pl:32
% pop_lps_dialect('$BLOB'("<stream>(0x562ef4163d00)"),  (/.../(lps_user_examples, 'acompa.pl')-> /.../(lps_user_examples, 'acompa.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/acompa.pl':_50570).


ama(romeo, julieta).
ama(julieta, romeo).
ama(bolivar, colombia).
ama(manuela, bolivar).

rcombina(X, Y) :-
    combina(Y, X).

viaja_junto(fulano, sutano).

:- dynamic actions/1.
:- multifile actions/1.


viaja_junto(X, Y, _) :-
    viaja_junto(X, Y).
viaja_junto(X, Y, C) :-
    not(member((X, Y), C)),
    viaja_junto(Y, X, [(X, Y)|C]).

combina(cafe, chocolate).
combina(salsa, pasta).

acompaña(X, Y) :-
    ama(X, Y).
acompaña(X, Y) :-
    combina(X, Y).
acompaña(X, Y) :-
    rcombina(X, Y).
acompaña(X, Y) :-
    viaja_junto(X, Y, []).
% dB(/.../(lps_user_examples, 'acompa.pl'), lps_visualization(_33264{groups:[], items:[]}, [])).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'ama.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'ama.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/ama.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'ama.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'ama.pl'), lps= /.../(lps_user_examples, 'ama.pl'), using= /.../(lps_user_examples, 'ama.pl')].
% continue_lps_dialect.
% ops.
% /pack/logicmoo_ec/test/lps_user_examples/ama.pl:27
% pop_lps_dialect('$BLOB'("<stream>(0x562ef33a9300)"),  (/.../(lps_user_examples, 'ama.pl')-> /.../(lps_user_examples, 'ama.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/ama.pl':_46112).


persona(rosa).

ama(romeo, julieta).
ama(julieta, romeo).
ama(bolivar, colombia).
ama(manuela, bolivar).

humano(socrates).

:- dynamic actions/1.
:- multifile actions/1.


acompañante(X) :-
    persona(X).

mortal(X) :-
    humano(X).

acompaña(rosa, amanda).
acompaña(amanda, gretta).
acompaña(gretta, cesar).
acompaña(cesar, rosa).
% dB(/.../(lps_user_examples, 'ama.pl'), lps_visualization(_28376{groups:[], items:[]}, [])).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'antecedents_and_observations.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'antecedents_and_observations.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/antecedents_and_observations.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'antecedents_and_observations.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'antecedents_and_observations.pl'), lps= /.../(lps_user_examples, 'antecedents_and_observations.pl'), using= /.../(lps_user_examples, 'antecedents_and_observations.pl')].
% continue_lps_dialect.
% ops.

% LPS:  actions((a1,r1,a2,a3)).
% Into: actions([a1,r1,a2,a3]).

% LPS:  events(e2).
% Into: events([e2]).

% LPS:  fluents(f1).
% Into: fluents([f1]).

% LPS:  observe(from(a1,to(1,2))).
% Into: observe([a1],2).

% LPS:  observe(from(a2,to(2,3))).
% Into: observe([a2],3).

% LPS:  observe(from(e2,to(3,6))).
% Into: observe([e2],6).

% LPS:  observe(from(a3,to(8,9))).
% Into: observe([a3],9).

% LPS:  then(if((from(a1,to(_22180,_22182)),from(e1,to(_22182,_22294)))),from(r1,to(_22294,_22462))).
% Into: reactive_rule([happens(a1,_22180,_22182),happens(e1,_22182,_22294)],[happens(r1,_22294,_22462)]).

% LPS:  if(from(e1,to(_23650,_23652)),(from(a2,to(_23650,_23764)),from(e2,to(_23874,_23652)))).
% Into: l_events(happens(e1,_23650,_23652),[happens(a2,_23650,_23764),happens(e2,_23874,_23652)]).

% LPS:  if(from(e2,to(_25092,_25094)),from(a3,to(_25204,_25206))).
% Into: l_events(happens(e2,_25092,_25094),[happens(a3,_25204,_25206)]).

% LPS:  initiates(e1,f1).
% Into: initiated(happens(e1,_27426,_27432),f1,[]).
% /pack/logicmoo_ec/test/lps_user_examples/antecedents_and_observations.pl:20
% pop_lps_dialect('$BLOB'("<stream>(0x562ef3ca3c00)"),  (/.../(lps_user_examples, 'antecedents_and_observations.pl')-> /.../(lps_user_examples, 'antecedents_and_observations.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/antecedents_and_observations.pl':_35002).


initiated(happens(e1, _, _), f1, []).

fluents([f1]).

reactive_rule([happens(a1, _, A), happens(e1, A, B)], [happens(r1, B, _)]).

l_events(happens(e1, A, B), [happens(a2, A, _), happens(e2, _, B)]).
l_events(happens(e2, _, _), [happens(a3, _, _)]).

:- dynamic actions/1.
:- multifile actions/1.

actions([a1, r1, a2, a3]).

events([e2]).

observe([a1], 2).
observe([a2], 3).
observe([e2], 6).
observe([a3], 9).

maxTime(10).
% dB(/.../(lps_user_examples, 'antecedents_and_observations.pl'), lps_visualization(_51546{groups:[_50638{content:"Events", id:"event", order:1}, _50712{content:"f1", id:"f1/0", order:3, subgroupStack:"false"}, _50778{content:"Actions", id:"action", order:4}], items:[_50888{content:"f1", end:11, group:"f1/0", id:0, start:9, title:"Fluent f1 initiated at 9<br/>and terminated at transition to 11"}, _51010{content:"a1", group:"event", id:1, start:2, style:"color:#E19735", title:"happens(a1,1,2)", type:"point"}, _51136{content:"a2", group:"event", id:2, start:3, style:"color:#E19735", title:"happens(a2,2,3)", type:"point"}, _51262{content:"e2", group:"event", id:3, start:6, style:"color:#E19735", title:"happens(e2,5,6)", type:"point"}, _51388{content:"a3", group:"event", id:4, start:9, style:"color:#E19735", title:"happens(a3,8,9)", type:"point"}, _51514{content:"r1", group:"action", id:5, start:10, style:"color:green", title:"happens(r1,9,10)", type:"point"}]}, [])).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'arrival_correct.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'arrival_correct.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/arrival_correct.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'arrival_correct.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'arrival_correct.pl'), lps= /.../(lps_user_examples, 'arrival_correct.pl'), using= /.../(lps_user_examples, 'arrival_correct.pl')].
% continue_lps_dialect.
% ops.

% LPS:  actions((attack,rob,call_the(_19898),repeat_wife_message(_19938))).
% Into: actions([attack,rob,call_the(_19898),repeat_wife_message(_19938)]).

% LPS:  fluents((is_a_weapon,is_a_gift,has_phone)).
% Into: fluents([is_a_weapon,is_a_gift,has_phone]).

% LPS:  events((persuade,prepare,general_shows(_22244),call_general(_22244),general_remembers_wife(_22324))).
% Into: events([persuade,prepare,general_shows(_22244),call_general(_22244),general_remembers_wife(_22324)]).

% LPS:  initially(is_a_weapon).
% Into: initial_state([is_a_weapon]).

% LPS:  false((is_a_gift,attack)).
% Into: d_pre([holds(is_a_gift,_25556),happens(attack,_25556,_25562)]).

% LPS:  false((has_phone,rob)).
% Into: d_pre([holds(has_phone,_26994),happens(rob,_26994,_27000)]).

% LPS:  then(if(is_a_weapon),(prepare,attack)).
% Into: reactive_rule([holds(is_a_weapon,_28502)],[happens(prepare,_28660,_28702),happens(attack,_28702,_28666)]).

% LPS:  then(if(is_a_weapon),persuade).
% Into: reactive_rule([holds(is_a_weapon,_29984)],[happens(persuade,_30142,_30148)]).

% LPS:  then(if(is_a_weapon),rob).
% Into: reactive_rule([holds(is_a_weapon,_31246)],[happens(rob,_31404,_31410)]).

% LPS:  if(from(prepare,to(_31630,_31632)),(at(true,_31630),_31632 is _31630+6)).
% Into: l_events(happens(prepare,_31630,_31632),[holds(true,_31630),_31632 is _31630+6]).

% LPS:  if(persuade,(general_shows(_33670),general_remembers_wife(_33710),call_general(_33670),repeat_wife_message(_33710))).
% Into: l_events(happens(persuade,_34950,_34956),[happens(general_shows(_33670),_34950,_35024),happens(general_remembers_wife(_33710),_35024,_35090),happens(call_general(_33670),_35090,_35156),happens(repeat_wife_message(_33710),_35156,_34956)]).

% LPS:  if(call_general(_34968),call_the(_34968)).
% Into: l_events(happens(call_general(_34968),_36034,_36040),[happens(call_the(_34968),_36034,_36040)]).

% LPS:  terminates(persuade,is_a_weapon).
% Into: terminated(happens(persuade,_37118,_37124),is_a_weapon,[]).

% LPS:  initiates(persuade,is_a_gift).
% Into: initiated(happens(persuade,_38134,_38140),is_a_gift,[]).

% LPS:  initiates(rob,has_phone).
% Into: initiated(happens(rob,_39158,_39164),has_phone,[]).

% LPS:  observe(from(general_shows('+86-555000001'),to(2,3))).
% Into: observe([general_shows('+86-555000001')],3).

% LPS:  observe(from(general_remembers_wife(##########),to(3,4))).
% Into: observe([general_remembers_wife(##########)],4).
% /pack/logicmoo_ec/test/lps_user_examples/arrival_correct.pl:46
% pop_lps_dialect('$BLOB'("<stream>(0x562ef4161000)"),  (/.../(lps_user_examples, 'arrival_correct.pl')-> /.../(lps_user_examples, 'arrival_correct.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/arrival_correct.pl':_49012).


initiated(happens(persuade, _, _), is_a_gift, []).
initiated(happens(rob, _, _), has_phone, []).

d_pre([holds(is_a_gift, A), happens(attack, A, _)]).
d_pre([holds(has_phone, A), happens(rob, A, _)]).

fluents([is_a_weapon, is_a_gift, has_phone]).

reactive_rule([holds(is_a_weapon, _)], [happens(prepare, _, A), happens(attack, A, _)]).
reactive_rule([holds(is_a_weapon, _)], [happens(persuade, _, _)]).
reactive_rule([holds(is_a_weapon, _)], [happens(rob, _, _)]).

terminated(happens(persuade, _, _), is_a_weapon, []).

initial_state([is_a_weapon]).

l_events(happens(prepare, A, B), [holds(true, A), B is A+6]).
l_events(happens(persuade, A, B), [happens(general_shows(C), A, D), happens(general_remembers_wife(E), D, F), happens(call_general(C), F, G), happens(repeat_wife_message(E), G, B)]).
l_events(happens(call_general(A), B, C), [happens(call_the(A), B, C)]).

:- dynamic actions/1.
:- multifile actions/1.

actions([attack, rob, call_the(_), repeat_wife_message(_)]).

events([persuade, prepare, general_shows(A), call_general(A), general_remembers_wife(_)]).

observe([general_shows('+86-555000001')], 3).
observe([general_remembers_wife(##########)], 4).

maxTime(15).
% dB(/.../(lps_user_examples, 'arrival_correct.pl'), lps_visualization(_65518{groups:[_64234{content:"Events", id:"event", order:1}, _64308{content:"has_phone", id:"has_phone/0", order:3, subgroupStack:"false"}, _64386{content:"is_a_gift", id:"is_a_gift/0", order:3, subgroupStack:"false"}, _64464{content:"is_a_weapon", id:"is_a_weapon/0", order:3, subgroupStack:"false"}, _64530{content:"Actions", id:"action", order:4}], items:[_64640{content:"has_phone", end:16, group:"has_phone/0", id:0, start:2, title:"Fluent has_phone initiated at 2<br/>and terminated at transition to 16"}, _64750{content:"is_a_gift", end:16, group:"is_a_gift/0", id:1, start:6, title:"Fluent is_a_gift initiated at 6<br/>and terminated at transition to 16"}, _64860{content:"is_a_weapon", end:6, group:"is_a_weapon/0", id:2, start:1, title:"Fluent is_a_weapon initiated at 1<br/>and terminated at transition to 6"}, _64982{content:"rob", group:"action", id:3, start:2, style:"color:green", title:"happens(rob,1,2)", type:"point"}, _65108{content:"general_shows(+86-555000001)", group:"event", id:4, start:3, style:"color:#E19735", title:"happens(general_shows(+86-555000001),2,3)", type:"point"}, _65234{content:"general_remembers_wife(##########)", group:"event", id:5, start:4, style:"color:#E19735", title:"happens(general_remembers_wife(##########),3,4)", type:"point"}, _65360{content:"call_the(+86-555000001)", group:"action", id:6, start:5, style:"color:green", title:"happens(call_the(+86-555000001),4,5)", type:"point"}, _65486{content:"repeat_wife_message(##########)", group:"action", id:7, start:6, style:"color:green", title:"happens(repeat_wife_message(##########),5,6)", type:"point"}]}, [])).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'arrival_español(1).pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'arrival_español(1).pl')).
% /pack/logicmoo_ec/test/lps_user_examples/arrival_español(1).pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'arrival_español(1).pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'arrival_español(1).pl'), lps= /.../(lps_user_examples, 'arrival_español(1).pl'), using= /.../(lps_user_examples, 'arrival_español(1).pl')].
% continue_lps_dialect.
% ops.

% LPS:  actions((atacar,robar,llamar_al(_19956),repita_mensaje_esposa(_19996))).
% Into: actions([atacar,robar,llamar_al(_19956),repita_mensaje_esposa(_19996)]).

% LPS:  fluents((es_un_arma,es_un_regalo)).
% Into: fluents([es_un_arma,es_un_regalo]).

% LPS:  events((prepare,persuade,muestra_del_general(_22436),llamar_general(_22436),general_recuerda_esposa(_22516))).
% Into: events([prepare,persuade,muestra_del_general(_22436),llamar_general(_22436),general_recuerda_esposa(_22516)]).

% LPS:  initially(es_un_arma).
% Into: initial_state([es_un_arma]).

% LPS:  observe(from(muestra_del_general('+86-555000001'),to(2,3))).
% Into: observe([muestra_del_general('+86-555000001')],3).

% LPS:  observe(from(general_recuerda_esposa(##########),to(3,4))).
% Into: observe([general_recuerda_esposa(##########)],4).

% LPS:  false((es_un_regalo,atacar)).
% Into: d_pre([holds(es_un_regalo,_28204),happens(atacar,_28204,_28210)]).

% LPS:  then(if(es_un_arma),(prepare,atacar)).
% Into: reactive_rule([holds(es_un_arma,_29790)],[happens(prepare,_29948,_29990),happens(atacar,_29990,_29954)]).

% LPS:  then(if(es_un_arma),persuade).
% Into: reactive_rule([holds(es_un_arma,_31272)],[happens(persuade,_31430,_31436)]).

% LPS:  if(from(prepare,to(_31560,_31562)),(at(true,_31560),_31562 is _31560+6)).
% Into: l_events(happens(prepare,_31560,_31562),[holds(true,_31560),_31562 is _31560+6]).

% LPS:  if(persuade,(muestra_del_general(_33592),general_recuerda_esposa(_33632),llamar_general(_33592),repita_mensaje_esposa(_33632))).
% Into: l_events(happens(persuade,_34872,_34878),[happens(muestra_del_general(_33592),_34872,_34946),happens(general_recuerda_esposa(_33632),_34946,_35012),happens(llamar_general(_33592),_35012,_35078),happens(repita_mensaje_esposa(_33632),_35078,_34878)]).

% LPS:  if(llamar_general(_34890),(robar,llamar_al(_34890))).
% Into: l_events(happens(llamar_general(_34890),_36010,_36016),[happens(robar,_36010,_36090),happens(llamar_al(_34890),_36090,_36016)]).

% LPS:  terminates(persuade,es_un_arma).
% Into: terminated(happens(persuade,_37194,_37200),es_un_arma,[]).

% LPS:  initiates(persuade,es_un_regalo).
% Into: initiated(happens(persuade,_38210,_38216),es_un_regalo,[]).
% /pack/logicmoo_ec/test/lps_user_examples/arrival_español(1).pl:61
% pop_lps_dialect('$BLOB'("<stream>(0x562ef4161900)"),  (/.../(lps_user_examples, 'arrival_español(1).pl')-> /.../(lps_user_examples, 'arrival_español(1).pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/arrival_español(1).pl':_45810).


initiated(happens(persuade, _, _), es_un_regalo, []).

d_pre([holds(es_un_regalo, A), happens(atacar, A, _)]).

fluents([es_un_arma, es_un_regalo]).

reactive_rule([holds(es_un_arma, _)], [happens(prepare, _, A), happens(atacar, A, _)]).
reactive_rule([holds(es_un_arma, _)], [happens(persuade, _, _)]).

terminated(happens(persuade, _, _), es_un_arma, []).

initial_state([es_un_arma]).

l_events(happens(prepare, A, B), [holds(true, A), B is A+6]).
l_events(happens(persuade, A, B), [happens(muestra_del_general(C), A, D), happens(general_recuerda_esposa(E), D, F), happens(llamar_general(C), F, G), happens(repita_mensaje_esposa(E), G, B)]).
l_events(happens(llamar_general(A), B, C), [happens(robar, B, D), happens(llamar_al(A), D, C)]).

:- dynamic actions/1.
:- multifile actions/1.

actions([atacar, robar, llamar_al(_), repita_mensaje_esposa(_)]).

events([prepare, persuade, muestra_del_general(A), llamar_general(A), general_recuerda_esposa(_)]).

observe([muestra_del_general('+86-555000001')], 3).
observe([general_recuerda_esposa(##########)], 4).

maxTime(10).
% dB(/.../(lps_user_examples, 'arrival_español(1).pl'), lps_visualization(_85454{groups:[_84358{content:"Events", id:"event", order:1}, _84432{content:"es_un_arma", id:"es_un_arma/0", order:3, subgroupStack:"false"}, _84510{content:"es_un_regalo", id:"es_un_regalo/0", order:3, subgroupStack:"false"}, _84576{content:"Actions", id:"action", order:4}], items:[_84686{content:"es_un_arma", end:7, group:"es_un_arma/0", id:0, start:1, title:"Fluent es_un_arma initiated at 1<br/>and terminated at transition to 7"}, _84796{content:"es_un_regalo", end:11, group:"es_un_regalo/0", id:1, start:7, title:"Fluent es_un_regalo initiated at 7<br/>and terminated at transition to 11"}, _84918{content:"muestra_del_general(+86-555000001)", group:"event", id:2, start:3, style:"color:#E19735", title:"happens(muestra_del_general(+86-555000001),2,3)", type:"point"}, _85044{content:"general_recuerda_esposa(##########)", group:"event", id:3, start:4, style:"color:#E19735", title:"happens(general_recuerda_esposa(##########),3,4)", type:"point"}, _85170{content:"robar", group:"action", id:4, start:5, style:"color:green", title:"happens(robar,4,5)", type:"point"}, _85296{content:"llamar_al(+86-555000001)", group:"action", id:5, start:6, style:"color:green", title:"happens(llamar_al(+86-555000001),5,6)", type:"point"}, _85422{content:"repita_mensaje_esposa(##########)", group:"action", id:6, start:7, style:"color:green", title:"happens(repita_mensaje_esposa(##########),6,7)", type:"point"}]}, [])).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'arrival_español.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'arrival_español.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/arrival_español.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'arrival_español.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'arrival_español.pl'), lps= /.../(lps_user_examples, 'arrival_español.pl'), using= /.../(lps_user_examples, 'arrival_español.pl')].
% continue_lps_dialect.
% ops.

% LPS:  actions((atacar,robar,llamar_al(_56062),repita_mensaje_esposa(_56102))).
% Into: actions([atacar,robar,llamar_al(_56062),repita_mensaje_esposa(_56102)]).

% LPS:  fluents((es_un_arma,es_un_regalo)).
% Into: fluents([es_un_arma,es_un_regalo]).

% LPS:  events((prepare,persuade,muestra_del_general(_58542),llamar_general(_58542),general_recuerda_esposa(_58622))).
% Into: events([prepare,persuade,muestra_del_general(_58542),llamar_general(_58542),general_recuerda_esposa(_58622)]).

% LPS:  initially(es_un_arma).
% Into: initial_state([es_un_arma]).

% LPS:  observe(from(muestra_del_general('+86-555000001'),to(2,3))).
% Into: observe([muestra_del_general('+86-555000001')],3).

% LPS:  observe(from(general_recuerda_esposa(##########),to(3,4))).
% Into: observe([general_recuerda_esposa(##########)],4).

% LPS:  false((es_un_regalo,atacar)).
% Into: d_pre([holds(es_un_regalo,_64310),happens(atacar,_64310,_64316)]).

% LPS:  then(if(es_un_arma),(prepare,atacar)).
% Into: reactive_rule([holds(es_un_arma,_65896)],[happens(prepare,_66054,_66096),happens(atacar,_66096,_66060)]).

% LPS:  then(if(es_un_arma),persuade).
% Into: reactive_rule([holds(es_un_arma,_67378)],[happens(persuade,_67536,_67542)]).

% LPS:  if(from(prepare,to(_67666,_67668)),(at(true,_67666),_67668 is _67666+6)).
% Into: l_events(happens(prepare,_67666,_67668),[holds(true,_67666),_67668 is _67666+6]).

% LPS:  if(persuade,(muestra_del_general(_69698),general_recuerda_esposa(_69738),llamar_general(_69698),repita_mensaje_esposa(_69738))).
% Into: l_events(happens(persuade,_70978,_70984),[happens(muestra_del_general(_69698),_70978,_71052),happens(general_recuerda_esposa(_69738),_71052,_71118),happens(llamar_general(_69698),_71118,_71184),happens(repita_mensaje_esposa(_69738),_71184,_70984)]).

% LPS:  if(llamar_general(_70996),(robar,llamar_al(_70996))).
% Into: l_events(happens(llamar_general(_70996),_72116,_72122),[happens(robar,_72116,_72196),happens(llamar_al(_70996),_72196,_72122)]).

% LPS:  terminates(persuade,es_un_arma).
% Into: terminated(happens(persuade,_73300,_73306),es_un_arma,[]).

% LPS:  initiates(persuade,es_un_regalo).
% Into: initiated(happens(persuade,_74316,_74322),es_un_regalo,[]).
% /pack/logicmoo_ec/test/lps_user_examples/arrival_español.pl:61
% pop_lps_dialect('$BLOB'("<stream>(0x562ef33a8d00)"),  (/.../(lps_user_examples, 'arrival_español.pl')-> /.../(lps_user_examples, 'arrival_español.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/arrival_español.pl':_81910).


initiated(happens(persuade, _, _), es_un_regalo, []).

d_pre([holds(es_un_regalo, A), happens(atacar, A, _)]).

fluents([es_un_arma, es_un_regalo]).

reactive_rule([holds(es_un_arma, _)], [happens(prepare, _, A), happens(atacar, A, _)]).
reactive_rule([holds(es_un_arma, _)], [happens(persuade, _, _)]).

terminated(happens(persuade, _, _), es_un_arma, []).

initial_state([es_un_arma]).

l_events(happens(prepare, A, B), [holds(true, A), B is A+6]).
l_events(happens(persuade, A, B), [happens(muestra_del_general(C), A, D), happens(general_recuerda_esposa(E), D, F), happens(llamar_general(C), F, G), happens(repita_mensaje_esposa(E), G, B)]).
l_events(happens(llamar_general(A), B, C), [happens(robar, B, D), happens(llamar_al(A), D, C)]).

:- dynamic actions/1.
:- multifile actions/1.

actions([atacar, robar, llamar_al(_), repita_mensaje_esposa(_)]).

events([prepare, persuade, muestra_del_general(A), llamar_general(A), general_recuerda_esposa(_)]).

observe([muestra_del_general('+86-555000001')], 3).
observe([general_recuerda_esposa(##########)], 4).

maxTime(10).
% dB(/.../(lps_user_examples, 'arrival_español.pl'), lps_visualization(_55514{groups:[_54418{content:"Events", id:"event", order:1}, _54492{content:"es_un_arma", id:"es_un_arma/0", order:3, subgroupStack:"false"}, _54570{content:"es_un_regalo", id:"es_un_regalo/0", order:3, subgroupStack:"false"}, _54636{content:"Actions", id:"action", order:4}], items:[_54746{content:"es_un_arma", end:7, group:"es_un_arma/0", id:0, start:1, title:"Fluent es_un_arma initiated at 1<br/>and terminated at transition to 7"}, _54856{content:"es_un_regalo", end:11, group:"es_un_regalo/0", id:1, start:7, title:"Fluent es_un_regalo initiated at 7<br/>and terminated at transition to 11"}, _54978{content:"muestra_del_general(+86-555000001)", group:"event", id:2, start:3, style:"color:#E19735", title:"happens(muestra_del_general(+86-555000001),2,3)", type:"point"}, _55104{content:"general_recuerda_esposa(##########)", group:"event", id:3, start:4, style:"color:#E19735", title:"happens(general_recuerda_esposa(##########),3,4)", type:"point"}, _55230{content:"robar", group:"action", id:4, start:5, style:"color:green", title:"happens(robar,4,5)", type:"point"}, _55356{content:"llamar_al(+86-555000001)", group:"action", id:5, start:6, style:"color:green", title:"happens(llamar_al(+86-555000001),5,6)", type:"point"}, _55482{content:"repita_mensaje_esposa(##########)", group:"action", id:6, start:7, style:"color:green", title:"happens(repita_mensaje_esposa(##########),6,7)", type:"point"}]}, [])).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'arrival.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'arrival.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/arrival.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'arrival.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'arrival.pl'), lps= /.../(lps_user_examples, 'arrival.pl'), using= /.../(lps_user_examples, 'arrival.pl')].
% continue_lps_dialect.
% ops.

% LPS:  actions((attack,rob,call_the(_19880),repeat_wife_message(_19920))).
% Into: actions([attack,rob,call_the(_19880),repeat_wife_message(_19920)]).

% LPS:  fluents((is_a_weapon,is_a_gift)).
% Into: fluents([is_a_weapon,is_a_gift]).

% LPS:  events((persuade,prepare,general_shows(_22172),call_general(_22172),general_remembers_wife(_22252))).
% Into: events([persuade,prepare,general_shows(_22172),call_general(_22172),general_remembers_wife(_22252)]).

% LPS:  initially(is_a_weapon).
% Into: initial_state([is_a_weapon]).

% LPS:  false((is_a_gift,attack)).
% Into: d_pre([holds(is_a_gift,_25484),happens(attack,_25484,_25490)]).

% LPS:  then(if(is_a_weapon),(prepare,attack)).
% Into: reactive_rule([holds(is_a_weapon,_26992)],[happens(prepare,_27150,_27192),happens(attack,_27192,_27156)]).

% LPS:  then(if(is_a_weapon),persuade).
% Into: reactive_rule([holds(is_a_weapon,_28474)],[happens(persuade,_28632,_28638)]).

% LPS:  if(from(prepare,to(_28700,_28702)),(at(true,_28700),_28702 is _28700+6)).
% Into: l_events(happens(prepare,_28700,_28702),[holds(true,_28700),_28702 is _28700+6]).

% LPS:  if(persuade,(general_shows(_30732),general_remembers_wife(_30772),call_general(_30732),repeat_wife_message(_30772))).
% Into: l_events(happens(persuade,_32012,_32018),[happens(general_shows(_30732),_32012,_32086),happens(general_remembers_wife(_30772),_32086,_32152),happens(call_general(_30732),_32152,_32218),happens(repeat_wife_message(_30772),_32218,_32018)]).

% LPS:  if(call_general(_32030),(rob,call_the(_32030))).
% Into: l_events(happens(call_general(_32030),_33150,_33156),[happens(rob,_33150,_33230),happens(call_the(_32030),_33230,_33156)]).

% LPS:  terminates(persuade,is_a_weapon).
% Into: terminated(happens(persuade,_34234,_34240),is_a_weapon,[]).

% LPS:  initiates(persuade,is_a_gift).
% Into: initiated(happens(persuade,_35250,_35256),is_a_gift,[]).

% LPS:  observe(from(general_shows('+86-555000001'),to(2,3))).
% Into: observe([general_shows('+86-555000001')],3).

% LPS:  observe(from(general_remembers_wife(##########),to(3,4))).
% Into: observe([general_remembers_wife(##########)],4).
% /pack/logicmoo_ec/test/lps_user_examples/arrival.pl:41
% pop_lps_dialect('$BLOB'("<stream>(0x562ef4163d00)"),  (/.../(lps_user_examples, 'arrival.pl')-> /.../(lps_user_examples, 'arrival.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/arrival.pl':_45100).


initiated(happens(persuade, _, _), is_a_gift, []).

d_pre([holds(is_a_gift, A), happens(attack, A, _)]).

fluents([is_a_weapon, is_a_gift]).

reactive_rule([holds(is_a_weapon, _)], [happens(prepare, _, A), happens(attack, A, _)]).
reactive_rule([holds(is_a_weapon, _)], [happens(persuade, _, _)]).

terminated(happens(persuade, _, _), is_a_weapon, []).

initial_state([is_a_weapon]).

l_events(happens(prepare, A, B), [holds(true, A), B is A+6]).
l_events(happens(persuade, A, B), [happens(general_shows(C), A, D), happens(general_remembers_wife(E), D, F), happens(call_general(C), F, G), happens(repeat_wife_message(E), G, B)]).
l_events(happens(call_general(A), B, C), [happens(rob, B, D), happens(call_the(A), D, C)]).

:- dynamic actions/1.
:- multifile actions/1.

actions([attack, rob, call_the(_), repeat_wife_message(_)]).

events([persuade, prepare, general_shows(A), call_general(A), general_remembers_wife(_)]).

observe([general_shows('+86-555000001')], 3).
observe([general_remembers_wife(##########)], 4).

maxTime(10).
% dB(/.../(lps_user_examples, 'arrival.pl'), lps_visualization(_90168{groups:[_89072{content:"Events", id:"event", order:1}, _89146{content:"is_a_gift", id:"is_a_gift/0", order:3, subgroupStack:"false"}, _89224{content:"is_a_weapon", id:"is_a_weapon/0", order:3, subgroupStack:"false"}, _89290{content:"Actions", id:"action", order:4}], items:[_89400{content:"is_a_gift", end:11, group:"is_a_gift/0", id:0, start:7, title:"Fluent is_a_gift initiated at 7<br/>and terminated at transition to 11"}, _89510{content:"is_a_weapon", end:7, group:"is_a_weapon/0", id:1, start:1, title:"Fluent is_a_weapon initiated at 1<br/>and terminated at transition to 7"}, _89632{content:"general_shows(+86-555000001)", group:"event", id:2, start:3, style:"color:#E19735", title:"happens(general_shows(+86-555000001),2,3)", type:"point"}, _89758{content:"general_remembers_wife(##########)", group:"event", id:3, start:4, style:"color:#E19735", title:"happens(general_remembers_wife(##########),3,4)", type:"point"}, _89884{content:"rob", group:"action", id:4, start:5, style:"color:green", title:"happens(rob,4,5)", type:"point"}, _90010{content:"call_the(+86-555000001)", group:"action", id:5, start:6, style:"color:green", title:"happens(call_the(+86-555000001),5,6)", type:"point"}, _90136{content:"repeat_wife_message(##########)", group:"action", id:6, start:7, style:"color:green", title:"happens(repeat_wife_message(##########),6,7)", type:"point"}]}, [])).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'badBlocks.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'badBlocks.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/badBlocks.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'badBlocks.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'badBlocks.pl'), lps= /.../(lps_user_examples, 'badBlocks.pl'), using= /.../(lps_user_examples, 'badBlocks.pl')].
% continue_lps_dialect.
% ops.

% LPS:  fluents(location(_55870,_55872)).
% Into: fluents([location(_55870,_55872)]).

% LPS:  actions(move(_56914,_56916)).
% Into: actions([move(_56914,_56916)]).

% LPS:  initially((location(b,floor),location(c,b),location(a,floor))).
% Into: initial_state([location(b,floor),location(c,b),location(a,floor)]).

% LPS:  observe(from(move(d,a),to(3,4))).
% Into: observe([move(d,a)],4).

% LPS:  then(if(true),from(make_tower([b,a,floor]),to(_60518,_60520))).
% Into: reactive_rule([],[happens(make_tower([b,a,floor]),_60518,_60520)]).

% LPS:  if(at(clear(_61708),_61730),(_61708\=floor,at(not(location(_61882,_61708)),_61730))).
% Into: l_int(holds(clear(_61708),_61730),[_61708\=floor,holds(not(location(_61882,_61708)),_61730)]).

% LPS:  at(clear(floor),_63874).
% Into: l_int(holds(clear(floor),_63874),[]).

% LPS:  if(from(make_tower([_65102,floor]),to(_65164,_65166)),from(make_on(_65102,floor),to(_65164,_65166))).
% Into: l_events(happens(make_tower([_65102,floor]),_65164,_65166),[happens(make_on(_65102,floor),_65164,_65166)]).

% LPS:  if(from(make_tower([_66502,_66522|_66524]),to(_66572,_66574)),(_66522\=floor,from(make_tower([_66522|_66524]),to(_66572,_66808)),from(make_on(_66502,_66522),to(_66808,_66574)))).
% Into: l_events(happens(make_tower([_66502,_66522|_66524]),_66572,_66574),[_66522\=floor,happens(make_tower([_66522|_66524]),_66572,_66808),happens(make_on(_66502,_66522),_66808,_66574)]).

% LPS:  if(from(make_on(_68354,_68356),to(_68392,_68394)),(at(not(location(_68354,_68356)),_68392),from(make_clear(_68354),to(_68664,_68666)),from(make_clear(_68356),to(_68394,_68802)),from(move(_68354,_68356),to(_68952,_68954)))).
% Into: l_events(happens(make_on(_68354,_68356),_68392,_68394),[holds(not(location(_68354,_68356)),_68392),happens(make_clear(_68354),_68664,_68666),happens(make_clear(_68356),_68394,_68802),happens(move(_68354,_68356),_68952,_68954)]).

% LPS:  if(from(make_on(_70356,_70358),to(_70394,_70394)),at(location(_70356,_70358),_70394)).
% Into: l_events(happens(make_on(_70356,_70358),_70394,_70394),[holds(location(_70356,_70358),_70394)]).

% LPS:  if(from(make_clear(_71648),to(_71684,_71684)),at(clear(_71648),_71684)).
% Into: l_events(happens(make_clear(_71648),_71684,_71684),[holds(clear(_71648),_71684)]).

% LPS:  if(from(make_clear(_72906),to(_72942,_72944)),(at(location(_73056,_72906),_72942),from(make_on(_73056,floor),to(_72942,_72944)))).
% Into: l_events(happens(make_clear(_72906),_72942,_72944),[holds(location(_73056,_72906),_72942),happens(make_on(_73056,floor),_72942,_72944)]).

% LPS:  initiates(move(_74428,_74430),location(_74428,_74430)).
% Into: initiated(happens(move(_74428,_74430),_75614,_75620),location(_74428,_74430),[]).

% LPS:  terminates(move(_75560,_75562),location(_75560,_75618)).
% Into: terminated(happens(move(_75560,_75562),_76746,_76752),location(_75560,_75618),[]).

% LPS:  false((move(_76700,_76702),not(clear(_76700)))).
% Into: d_pre([happens(move(_76700,_76702),_77844,_77850),holds(not(clear(_76700)),_77844)]).

% LPS:  false((move(_77886,_77888),not(clear(_77888)))).
% Into: d_pre([happens(move(_77886,_77888),_79030,_79036),holds(not(clear(_77888)),_79030)]).
% /pack/logicmoo_ec/test/lps_user_examples/badBlocks.pl:61
% pop_lps_dialect('$BLOB'("<stream>(0x562ef32aa300)"),  (/.../(lps_user_examples, 'badBlocks.pl')-> /.../(lps_user_examples, 'badBlocks.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/badBlocks.pl':_86646).


initiated(happens(move(A, B), _, _), location(A, B), []).

d_pre([happens(move(A, _), B, _), holds(not(clear(A)), B)]).
d_pre([happens(move(_, A), B, _), holds(not(clear(A)), B)]).

fluents([location(_, _)]).

l_int(holds(clear(A), B), [A\=floor, holds(not(location(_, A)), B)]).
l_int(holds(clear(floor), _), []).

reactive_rule([], [happens(make_tower([b, a, floor]), _, _)]).

terminated(happens(move(A, _), _, _), location(A, _), []).

initial_state([location(b, floor), location(c, b), location(a, floor)]).

l_events(happens(make_tower([A, floor]), B, C), [happens(make_on(A, floor), B, C)]).
l_events(happens(make_tower([A, B|C]), D, E), [B\=floor, happens(make_tower([B|C]), D, F), happens(make_on(A, B), F, E)]).
l_events(happens(make_on(A, B), C, D), [holds(not(location(A, B)), C), happens(make_clear(A), _, _), happens(make_clear(B), D, _), happens(move(A, B), _, _)]).
l_events(happens(make_on(A, B), C, C), [holds(location(A, B), C)]).
l_events(happens(make_clear(A), B, B), [holds(clear(A), B)]).
l_events(happens(make_clear(A), B, C), [holds(location(D, A), B), happens(make_on(D, floor), B, C)]).

:- dynamic actions/1.
:- multifile actions/1.

actions([move(_, _)]).

observe([move(d, a)], 4).

maxTime(10).
Warning: Rejected observations [move(d,a)] attempting to satisfy false preconditions [happens(move(d,a),3,4),holds(not(clear(a)),3)]
% dB(/.../(lps_user_examples, 'badBlocks.pl'), lps_visualization(_98908{groups:[_97932{content:"location(A,B)", id:"location/2", order:3, subgroupStack:"false"}, _97998{content:"Actions", id:"action", order:4}], items:[_98120{content:"a,floor", end:11, group:"location/2", id:0, start:1, subgroup:"a", title:"Fluent location(a,floor) initiated at 1<br/>and terminated at transition to 11"}, _98246{content:"b,a", end:11, group:"location/2", id:1, start:3, subgroup:"b", title:"Fluent location(b,a) initiated at 3<br/>and terminated at transition to 11"}, _98372{content:"b,floor", end:3, group:"location/2", id:2, start:1, subgroup:"b", title:"Fluent location(b,floor) initiated at 1<br/>and terminated at transition to 3"}, _98498{content:"c,b", end:2, group:"location/2", id:3, start:1, subgroup:"c", title:"Fluent location(c,b) initiated at 1<br/>and terminated at transition to 2"}, _98624{content:"c,floor", end:11, group:"location/2", id:4, start:2, subgroup:"c", title:"Fluent location(c,floor) initiated at 2<br/>and terminated at transition to 11"}, _98750{content:"move(c,floor)", group:"action", id:5, start:2, style:"color:green", title:"happens(move(c,floor),1,2)", type:"point"}, _98876{content:"move(b,a)", group:"action", id:6, start:3, style:"color:green", title:"happens(move(b,a),2,3)", type:"point"}]}, [])).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'bad conveyor.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'bad conveyor.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/bad conveyor.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'bad conveyor.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'bad conveyor.pl'), lps= /.../(lps_user_examples, 'bad conveyor.pl'), using= /.../(lps_user_examples, 'bad conveyor.pl')].
% continue_lps_dialect.
% ops.

% LPS:  fluents((pouring(_19836),contents(_19836,_19892),empty/1,location(_20010,_20012))).
% Into: fluents([pouring(_19836),contents(_19836,_19892),empty(_21324),location(_20010,_20012)]).

% LPS:  initially((contents(bottle,0),contents(container,0),contents(tank1,100),contents(tank2,100),location(bottle,160),location(tank1,160),location(tank2,240),location(container,360))).
% Into: initial_state([contents(bottle,0),contents(container,0),contents(tank1,100),contents(tank2,100),location(bottle,160),location(tank1,160),location(tank2,240),location(container,360)]).

% LPS:  actions((openValve(_22962),closeValve(_22962),pourChunk(_23056,_23058),turnConveyor/1)).
% Into: actions([openValve(_22962),closeValve(_22962),pourChunk(_23056,_23058),turnConveyor(_24426)]).

% LPS:  false((pourChunk(_24370,_24372),turnConveyor(_24412))).
% Into: d_pre([happens(pourChunk(_24370,_24372),_25454,_25460),happens(turnConveyor(_24412),_25454,_25460)]).

% LPS:  false((pourChunk(_25540,_25542),location(_25540,_25598),location(_25542,_25654),_25598\=_25654)).
% Into: d_pre([happens(pourChunk(_25540,_25542),_26898,_26904),holds(location(_25540,_25598),_26898),holds(location(_25542,_25654),_26898),_25598\=_25654]).

% LPS:  if(from(makeLocation(bottle,_27744),_27766),(at(location(bottle,_27848),_27766),at(location(_27744,_27952),_27766),_28084 is _27952-_27848,from(moveConveyor(_28084),_27766))).
% Into: l_events(happens(makeLocation(bottle,_27744),_27766,_29444),[holds(location(bottle,_27848),_27766),holds(location(_27744,_27952),_27766),_28084 is _27952-_27848,happens(moveConveyor(_28084),_27766,_29534)]).

% LPS:  from(moveConveyor(0),to(_30470,_30470)).
% Into: l_events(happens(moveConveyor(0),_30470,_30470),[]).

% LPS:  if(from(moveConveyor(_31544),to(_31580,_31582)),(_31544>0,from(turnConveyor(clockwise),to(_31580,_31782)),_31924 is _31544-10,from(moveConveyor(_31924),to(_31782,_31582)))).
% Into: l_events(happens(moveConveyor(_31544),_31580,_31582),[_31544>0,happens(turnConveyor(clockwise),_31580,_31782),_31924 is _31544-10,happens(moveConveyor(_31924),_31782,_31582)]).

% LPS:  if(moveConveyor(_33992),(_33992<0,turnConveyor(counterClockwise),_34180 is _33992+10,moveConveyor(_34180))).
% Into: l_events(happens(moveConveyor(_33992),_35422,_35428),[_33992<0,happens(turnConveyor(counterClockwise),_35422,_35704),_34180 is _33992+10,happens(moveConveyor(_34180),_35704,_35428)]).

% LPS:  if(updates(turnConveyor(counterClockwise),in(to(_36012,_36014),location(bottle,_36012))),_36014 is _36012-10).
% Into: updated(happens(turnConveyor(counterClockwise),_37488,_37494),location(bottle,_36012),_36012-_36014,[_36014 is _36012-10]).

% LPS:  if(updates(turnConveyor(clockwise),in(to(_37766,_37768),location(bottle,_37766))),_37768 is _37766+10).
% Into: updated(happens(turnConveyor(clockwise),_39242,_39248),location(bottle,_37766),_37766-_37768,[_37768 is _37766+10]).

% LPS:  if(empty(_39484),contents(_39484,0)).
% Into: l_int(holds(empty(_39484),_40568),[holds(contents(_39484,0),_40568)]).

% LPS:  then(if((at(empty(bottle),_40616),at(location(bottle,160),_40616))),(from(pour(tank1,bottle,50),to(_40616,_40912)),from(makeLocation(bottle,tank2),to(_40912,_41064)),from(pour(tank2,bottle,50),to(_41064,_41232)),from(makeLocation(bottle,container),to(_41232,_41384)),from(pour(bottle,container,100),to(_41384,_41552)),from(makeLocation(bottle,tank1),_41552))).
% Into: reactive_rule([holds(empty(bottle),_40616),holds(location(bottle,160),_40616)],[happens(pour(tank1,bottle,50),_40616,_40912),happens(makeLocation(bottle,tank2),_40912,_41064),happens(pour(tank2,bottle,50),_41064,_41232),happens(makeLocation(bottle,container),_41232,_41384),happens(pour(bottle,container,100),_41384,_41552),happens(makeLocation(bottle,tank1),_41552,_43344)]).

% LPS:  if(from(pour(_43168,_43170,_43172),to(_43208,_43210)),(at(contents(_43170,_43324),_43208),_43456 is _43172+_43324,valveRate(_43524),_43608 is _43456-_43524,from(openValve(_43168),to(_43208,_43714)),at(contents(_43170,_43608),_43850),from(closeValve(_43168),to(_43850,_43210)))).
% Into: l_events(happens(pour(_43168,_43170,_43172),_43208,_43210),[holds(contents(_43170,_43324),_43208),_43456 is _43172+_43324,valveRate(_43524),_43608 is _43456-_43524,happens(openValve(_43168),_43208,_43714),holds(contents(_43170,_43608),_43850),happens(closeValve(_43168),_43850,_43210)]).

% LPS:  initiates(openValve(_45500),pouring(_45500)).
% Into: initiated(happens(openValve(_45500),_46648,_46654),pouring(_45500),[]).

% LPS:  terminates(closeValve(_46584),pouring(_46584)).
% Into: terminated(happens(closeValve(_46584),_47732,_47738),pouring(_46584),[]).

% LPS:  then(if((at(pouring(_47676),_47698),at(location(_47676,_47780),_47698),at(location(_47882,_47780),_47698),_47882\=_47676)),from(pourChunk(_47676,_47882),to(_47698,_48210))).
% Into: reactive_rule([holds(pouring(_47676),_47698),holds(location(_47676,_47780),_47698),holds(location(_47882,_47780),_47698),_47882\=_47676],[happens(pourChunk(_47676,_47882),_47698,_48210)]).

% LPS:  if(updates(pourChunk(_49446,_49448),in(to(_49484,_49486),contents(_49448,_49484))),(valveRate(_49672),_49486 is _49484+_49672)).
% Into: updated(happens(pourChunk(_49446,_49448),_51070,_51076),contents(_49448,_49484),_49484-_49486,[valveRate(_49672),_49486 is _49484+_49672]).

% LPS:  if(updates(pourChunk(_50954,_50956),in(to(_50992,_50994),contents(_50954,_50992))),(valveRate(_51180),_50994 is _50992-_51180)).
% Into: updated(happens(pourChunk(_50954,_50956),_52578,_52584),contents(_50954,_50992),_50992-_50994,[valveRate(_51180),_50994 is _50992-_51180]).

% LPS:  if(at(locatedContents(_53568,_53570,_53572),_53594),(at(location(_53568,_53570),_53594),at(contents(_53568,_53572),_53594))).
% Into: l_int(holds(locatedContents(_53568,_53570,_53572),_53594),[holds(location(_53568,_53570),_53594),holds(contents(_53568,_53572),_53594)]).
% /pack/logicmoo_ec/test/lps_user_examples/bad conveyor.pl:129
% pop_lps_dialect('$BLOB'("<stream>(0x562ef4161900)"),  (/.../(lps_user_examples, 'bad conveyor.pl')-> /.../(lps_user_examples, 'bad conveyor.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/bad conveyor.pl':_70820).


initiated(happens(openValve(A), _, _), pouring(A), []).

d_pre([happens(pourChunk(_, _), A, B), happens(turnConveyor(_), A, B)]).
d_pre([happens(pourChunk(A, B), C, _), holds(location(A, D), C), holds(location(B, E), C), D\=E]).

d(locatedContents(bottle, Pos, Level), [type:rectangle, fillColor:yellow, from:[X1, 60], to:[X2, Height]]) :-
    Height is 60+Level/4,
    X1 is Pos-10,
    X2 is Pos+10.
d(location(bottle, Pos), [type:rectangle, from:[X1, 60], to:[X2, 100], strokeColor:blue]) :-
    X1 is Pos-10,
    X2 is Pos+10.
d(timeless, [[type:line, strokeWidth:2, strokeColor:black, from:[100, 60], to:[400, 60]], [type:circle, strokeWidth:2, strokeColor:black, center:[100, 40], radius:20], [type:circle, strokeWidth:2, strokeColor:black, center:[400, 40], radius:20], [type:rectangle, fillColor:white, from:[130, 120], to:[190, 150], strokeColor:blue], [type:rectangle, fillColor:white, from:[210, 120], to:[270, 150], strokeColor:blue], [type:line, strokeWidth:2, strokeColor:black, from:[100, 20], to:[400, 20]]]).

fluents([pouring(A), contents(A, _), empty(_), location(_, _)]).

l_int(holds(empty(A), B), [holds(contents(A, 0), B)]).
l_int(holds(locatedContents(A, B, C), D), [holds(location(A, B), D), holds(contents(A, C), D)]).

reactive_rule([holds(empty(bottle), A), holds(location(bottle, 160), A)], [happens(pour(tank1, bottle, 50), A, B), happens(makeLocation(bottle, tank2), B, C), happens(pour(tank2, bottle, 50), C, D), happens(makeLocation(bottle, container), D, E), happens(pour(bottle, container, 100), E, F), happens(makeLocation(bottle, tank1), F, _)]).
reactive_rule([holds(pouring(A), B), holds(location(A, C), B), holds(location(D, C), B), D\=A], [happens(pourChunk(A, D), B, _)]).

terminated(happens(closeValve(A), _, _), pouring(A), []).

valveRate(10).

initial_state([contents(bottle, 0), contents(container, 0), contents(tank1, 100), contents(tank2, 100), location(bottle, 160), location(tank1, 160), location(tank2, 240), location(container, 360)]).

l_events(happens(makeLocation(bottle, A), B, _), [holds(location(bottle, C), B), holds(location(A, D), B), E is D-C, happens(moveConveyor(E), B, _)]).
l_events(happens(moveConveyor(0), A, A), []).
l_events(happens(moveConveyor(A), B, C), [A>0, happens(turnConveyor(clockwise), B, D), E is A-10, happens(moveConveyor(E), D, C)]).
l_events(happens(moveConveyor(A), B, C), [A<0, happens(turnConveyor(counterClockwise), B, D), E is A+10, happens(moveConveyor(E), D, C)]).
l_events(happens(pour(A, B, C), D, E), [holds(contents(B, F), D), G is C+F, valveRate(H), I is G-H, happens(openValve(A), D, _), holds(contents(B, I), J), happens(closeValve(A), J, E)]).

:- dynamic actions/1.
:- multifile actions/1.

actions([openValve(A), closeValve(A), pourChunk(_, _), turnConveyor(_)]).

updated(happens(turnConveyor(counterClockwise), _, _), location(bottle, A), A-B, [B is A-10]).
updated(happens(turnConveyor(clockwise), _, _), location(bottle, A), A-B, [B is A+10]).
updated(happens(pourChunk(_, A), _, _), contents(A, B), B-C, [valveRate(D), C is B+D]).
updated(happens(pourChunk(A, _), _, _), contents(A, B), B-C, [valveRate(D), C is B-D]).

maxTime(90).
ERROR: LPS: execution timeout(resolveAndUpdate)
PROGRAM FAILED
% dB(/.../(lps_user_examples, 'bad conveyor.pl'), lps_visualization(_50470{groups:[_41598{content:"contents(A,B)", id:"contents/2", order:3, subgroupStack:"false"}, _41624{content:"location(A,B)", id:"location/2", order:3, subgroupStack:"false"}, _41650{content:"pouring(A)", id:"pouring/1", order:3, subgroupStack:"false"}, _41676{content:"Actions", id:"action", order:4}], items:[_41698{content:"bottle", end:44, group:"pouring/1", id:0, start:34, subgroup:"bottle", title:"Fluent pouring(bottle) initiated at 34<br/>and terminated at transition to 44"}, _41736{content:"bottle", end:46, group:"pouring/1", id:1, start:45, subgroup:"bottle", title:"Fluent pouring(bottle) initiated at 45<br/>and terminated at transition to 46"}, _41774{content:"bottle", end:48, group:"pouring/1", id:2, start:47, subgroup:"bottle", title:"Fluent pouring(bottle) initiated at 47<br/>and terminated at transition to 48"}, ...(_83892)]}, _82910{cycles:[[_76854{create:[_76180{from:[100, 60], id:"timeless", strokeColor:"black", strokeWidth:2, to:[400, 60], type:"line"}, _76302{center:[100, 40], id:"timeless", radius:20, strokeColor:"black", strokeWidth:2, type:"circle"}, _76424{center:[400, 40], id:"timeless", radius:20, strokeColor:"black", strokeWidth:2, type:"circle"}, _76558{fillColor:"white", from:[130, 120], id:"timeless", strokeColor:"blue", to:[190, 150], type:"rectangle"}, _76692{fillColor:"white", from:[210, 120], id:"timeless", strokeColor:"blue", to:[270, 150], type:"rectangle"}, _76826{from:[100, 20], id:"timeless", strokeColor:"black", strokeWidth:2, to:[400, 20], type:"line"}]}], [_77002{create:_76978{from:[150, 60], id:"location(bottle,160)", strokeColor:"blue", to:[170, 100], type:"rectangle"}}], [], [], [], [], [], [], [_77186{create:_77162{from:[160, 60], id:"location(bottle,170)", strokeColor:"blue", to:[180, 100], type:"rectangle"}}, _77216{kill:"location(bottle,160)"}], [_77364{create:_77340{from:[170, 60], id:"location(bottle,180)", strokeColor:"blue", to:[190, 100], type:"rectangle"}}, _77394{kill:"location(bottle,170)"}], [_77542{create:_77518{from:[180, 60], id:"location(bottle,190)", strokeColor:"blue", to:[200, 100], type:"rectangle"}}, _77572{kill:"location(bottle,180)"}], [_77720{create:_77696{from:[190, 60], id:"location(bottle,200)", strokeColor:"blue", to:[210, 100], type:"rectangle"}}, _77750{kill:"location(bottle,190)"}], [_77898{create:_77874{from:[200, 60], id:"location(bottle,210)", strokeColor:"blue", to:[220, 100], type:"rectangle"}}, _77928{kill:"location(bottle,200)"}], [_78076{create:_78052{from:[210, 60], id:"location(bottle,220)", strokeColor:"blue", to:[230, 100], type:"rectangle"}}, _78106{kill:"location(bottle,210)"}], [_78254{create:_78230{from:[220, 60], id:"location(bottle,230)", strokeColor:"blue", to:[240, 100], type:"rectangle"}}, _78284{kill:"location(bottle,220)"}], [_78432{create:_78408{from:[230, 60], id:"location(bottle,240)", strokeColor:"blue", to:[250, 100], type:"rectangle"}}, _78462{kill:"location(bottle,230)"}], [], [], [], [], [], [], [_78646{create:_78622{from:[240, 60], id:"location(bottle,250)", strokeColor:"blue", to:[260, 100], type:"rectangle"}}, _78676{kill:"location(bottle,240)"}], [_78824{create:_78800{from:[250, 60], id:"location(bottle,260)", strokeColor:"blue", to:[270, 100], type:"rectangle"}}, _78854{kill:"location(bottle,250)"}], [_79002{create:_78978{from:[260, 60], id:"location(bottle,270)", strokeColor:"blue", to:[280, 100], type:"rectangle"}}, _79032{kill:"location(bottle,260)"}], [_79180{create:_79156{from:[270, 60], id:"location(bottle,280)", strokeColor:"blue", to:[290, 100], type:"rectangle"}}, _79210{kill:"location(bottle,270)"}], [_79358{create:_79334{from:[280, 60], id:"location(bottle,290)", strokeColor:"blue", to:[300, 100], type:"rectangle"}}, _79388{kill:"location(bottle,280)"}], [_79536{create:_79512{from:[290, 60], id:"location(bottle,300)", strokeColor:"blue", to:[310, 100], type:"rectangle"}}, _79566{kill:"location(bottle,290)"}], [_79714{create:_79690{from:[300, 60], id:"location(bottle,310)", strokeColor:"blue", to:[320, 100], type:"rectangle"}}, _79744{kill:"location(bottle,300)"}], [_79892{create:_79868{from:[310, 60], id:"location(bottle,320)", strokeColor:"blue", to:[330, 100], type:"rectangle"}}, _79922{kill:"location(bottle,310)"}], [_80070{create:_80046{from:[320, 60], id:"location(bottle,330)", strokeColor:"blue", to:[340, 100], type:"rectangle"}}, _80100{kill:"location(bottle,320)"}], [_80248{create:_80224{from:[330, 60], id:"location(bottle,340)", strokeColor:"blue", to:[350, 100], type:"rectangle"}}, _80278{kill:"location(bottle,330)"}], [_80426{create:_80402{from:[340, 60], id:"location(bottle,350)", strokeColor:"blue", to:[360, 100], type:"rectangle"}}, _80456{kill:"location(bottle,340)"}], [_80604{create:_80580{from:[350, 60], id:"location(bottle,360)", strokeColor:"blue", to:[370, 100], type:"rectangle"}}, _80634{kill:"location(bottle,350)"}], [], [], [], [], [], [], [], [], [], [], [], [_80848{create:_80824{from:[340, 60], id:"location(bottle,350)", strokeColor:"blue", to:[360, 100], type:"rectangle"}}, _80878{kill:"location(bottle,360)"}], [_81026{create:_81002{from:[330, 60], id:"location(bottle,340)", strokeColor:"blue", to:[350, 100], type:"rectangle"}}, _81056{kill:"location(bottle,350)"}], [_81204{create:_81180{from:[320, 60], id:"location(bottle,330)", strokeColor:"blue", to:[340, 100], type:"rectangle"}}, _81234{kill:"location(bottle,340)"}], [_81382{create:_81358{from:[310, 60], id:"location(bottle,320)", strokeColor:"blue", to:[330, 100], type:"rectangle"}}, _81412{kill:"location(bottle,330)"}], [_81560{create:_81536{from:[300, 60], id:"location(bottle,310)", strokeColor:"blue", to:[320, 100], type:"rectangle"}}, _81590{kill:"location(bottle,320)"}], [_81738{create:_81714{from:[290, 60], id:"location(bottle,300)", strokeColor:"blue", to:[310, 100], type:"rectangle"}}, _81768{kill:"location(bottle,310)"}], [_81916{create:_81892{from:[280, 60], id:"location(bottle,290)", strokeColor:"blue", to:[300, 100], type:"rectangle"}}, _81946{kill:"location(bottle,300)"}], [_82094{create:_82070{from:[270, 60], id:"location(bottle,280)", strokeColor:"blue", to:[290, 100], type:"rectangle"}}, _82124{kill:"location(bottle,290)"}], [_82272{create:_82248{from:[260, 60], id:"location(bottle,270)", strokeColor:"blue", to:[280, 100], type:"rectangle"}}, _82302{kill:"location(bottle,280)"}], [_82450{create:_82426{from:[250, 60], id:"location(bottle,260)", strokeColor:"blue", to:[270, 100], type:"rectangle"}}, _82480{kill:"location(bottle,270)"}], [_82628{create:_82604{from:[240, 60], id:"location(bottle,250)", strokeColor:"blue", to:[260, 100], type:"rectangle"}}, _82658{kill:"location(bottle,260)"}], [_82806{create:_82782{from:[230, 60], id:"location(bottle,240)", strokeColor:"blue", to:[250, 100], type:"rectangle"}}, _82836{kill:"location(bottle,250)"}], [_82872{kill:"location(bottle,240)"}, _82902{kill:"timeless"}]]})).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'Ballot.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'Ballot.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/Ballot.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'Ballot.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'Ballot.pl'), lps= /.../(lps_user_examples, 'Ballot.pl'), using= /.../(lps_user_examples, 'Ballot.pl')].
% continue_lps_dialect.
% ops.

% LPS:  events((ballot(_147606,_147608),giveRightToVote(_147606,_147664),delegate(_147718,_147720),vote(_147664,_147776))).
% Into: events([ballot(_147606,_147608),giveRightToVote(_147606,_147664),delegate(_147718,_147720),vote(_147664,_147776)]).

% LPS:  fluents((chairman(_149008),voter(_149062,_149064),voted(_149062,_149120),delegateOf(_149062,_149176),voteCount(_149120,_149232))).
% Into: fluents([chairman(_149008),voter(_149062,_149064),voted(_149062,_149120),delegateOf(_149062,_149176),voteCount(_149120,_149232)]).

% LPS:  observe(from(ballot(chair,[trump,clinton]),to(1,2))).
% Into: observe([ballot(chair,[trump,clinton])],2).

% LPS:  observe((giveRightToVote(chair,miguel),giveRightToVote(chair,fariba),giveRightToVote(chair,bob),from(giveRightToVote(chair,jacinto),to(3,4)))).
% Into: observe([giveRightToVote(chair,miguel),giveRightToVote(chair,fariba),giveRightToVote(chair,bob),giveRightToVote(chair,jacinto)],4).

% LPS:  observe(from(delegate(bob,miguel),to(4,5))).
% Into: observe([delegate(bob,miguel)],5).

% LPS:  observe(from(vote(miguel,clinton),to(5,6))).
% Into: observe([vote(miguel,clinton)],6).

% LPS:  observe(from(delegate(jacinto,bob),to(6,7))).
% Into: observe([delegate(jacinto,bob)],7).

% LPS:  observe(from(delegate(fariba,miguel),to(7,8))).
% Into: observe([delegate(fariba,miguel)],8).

% LPS:  if(initiates(ballot(_157782,_157784),voteCount(_157838,0)),member(_157838,_157784)).
% Into: initiated(happens(ballot(_157782,_157784),_159098,_159104),voteCount(_157838,0),[member(_157838,_157784)]).

% LPS:  initiates(ballot(_159494,_159496),voter(_159494,1)).
% Into: initiated(happens(ballot(_159494,_159496),_160680,_160686),voter(_159494,1),[]).

% LPS:  initiates(ballot(_160626,_160628),chairman(_160626)).
% Into: initiated(happens(ballot(_160626,_160628),_161792,_161798),chairman(_160626),[]).

% LPS:  false((ballot(_161780,_161782),voteCount(_161836,_161838))).
% Into: d_pre([happens(ballot(_161780,_161782),_162882,_162888),holds(voteCount(_161836,_161838),_162882)]).

% LPS:  if(initiates(giveRightToVote(_163182,_163184),voter(_163184,1)),(chairman(_163182),not(voter(_163184,_163376)))).
% Into: initiated(happens(giveRightToVote(_163182,_163184),_164604,_164610),voter(_163184,1),[holds(chairman(_163182),_164604),holds(not(voter(_163184,_163376)),_164604)]).

% LPS:  updates(delegate(_165064,_165066),in(to(_165102,0),voter(_165064,_165102))).
% Into: updated(happens(delegate(_165064,_165066),_166414,_166420),voter(_165064,_165102),_165102-0,[]).

% LPS:  if(updates(delegate(_166316,_166318),in(to(_166354,_166356),voter(_166436,_166354))),(delegateOf(_166318,_166436),voter(_166316,_166614),_166356 is _166614+_166354)).
% Into: updated(happens(delegate(_166316,_166318),_168082,_168088),voter(_166436,_166354),_166354-_166356,[holds(delegateOf(_166318,_166436),_168082),holds(voter(_166316,_166614),_168082),_166356 is _166614+_166354]).

% LPS:  if(updates(delegate(_168936,_168938),in(to(_168974,_168976),voteCount(_169056,_168974))),(delegateOf(_168938,_169178),voted(_169178,_169056),voter(_168936,_169290),_168976 is _168974+_169290)).
% Into: updated(happens(delegate(_168936,_168938),_170814,_170820),voteCount(_169056,_168974),_168974-_168976,[holds(delegateOf(_168938,_169178),_170814),holds(voted(_169178,_169056),_170814),holds(voter(_168936,_169290),_170814),_168976 is _168974+_169290]).

% LPS:  initiates(delegate(_171822,_171824),voted(_171822,delegated(_171824))).
% Into: initiated(happens(delegate(_171822,_171824),_173036,_173042),voted(_171822,delegated(_171824)),[]).

% LPS:  if(delegateOf(_173016,_173018),(voted(_173016,delegated(_173092)),delegateOf(_173092,_173018))).
% Into: l_int(holds(delegateOf(_173016,_173018),_174256),[holds(voted(_173016,delegated(_173092)),_174256),holds(delegateOf(_173092,_173018),_174256)]).

% LPS:  if(delegateOf(_174868,_174868),not(voted(_174868,delegated(_174952)))).
% Into: l_int(holds(delegateOf(_174868,_174868),_176020),[holds(not(voted(_174868,delegated(_174952))),_176020)]).

% LPS:  false((delegate(_176494,_176496),delegate(_176550,_176496),_176494\=_176550)).
% Into: d_pre([happens(delegate(_176494,_176496),_177740,_177746),happens(delegate(_176550,_176496),_177740,_177746),_176494\=_176550]).

% LPS:  false((delegate(_19610,_19612),voted(_19610,_19654))).
% Into: d_pre([happens(delegate(_19610,_19612),_19940,_19946),holds(voted(_19610,_19654),_19940)]).

% LPS:  false((delegate(_20248,_20250),_20248==_20250)).
% Into: d_pre([happens(delegate(_20248,_20250),_21382,_21388),_20248==_20250]).

% LPS:  false((delegate(_21690,_21692),delegate(_21690,_21748),_21692\=_21748)).
% Into: d_pre([happens(delegate(_21690,_21692),_22936,_22942),happens(delegate(_21690,_21748),_22936,_22942),_21692\=_21748]).

% LPS:  false((delegate(_23344,_23346),delegateOf(_23346,_23344))).
% Into: d_pre([happens(delegate(_23344,_23346),_24470,_24476),holds(delegateOf(_23346,_23344),_24470)]).

% LPS:  initiates(vote(_24770,_24772),voted(_24770,_24772)).
% Into: initiated(happens(vote(_24770,_24772),_25956,_25962),voted(_24770,_24772),[]).

% LPS:  if(updates(vote(_25970,_25972),in(to(_26008,_26010),voteCount(_25972,_26008))),(voter(_25970,_26212),_26010 is _26008+_26212)).
% Into: updated(happens(vote(_25970,_25972),_27624,_27630),voteCount(_25972,_26008),_26008-_26010,[holds(voter(_25970,_26212),_27624),_26010 is _26008+_26212]).

% LPS:  false((vote(_28096,_28098),vote(_28152,_28098),_28096\=_28152)).
% Into: d_pre([happens(vote(_28096,_28098),_29342,_29348),happens(vote(_28152,_28098),_29342,_29348),_28096\=_28152]).

% LPS:  false((vote(_29716,_29718),voted(_29716,_29774))).
% Into: d_pre([happens(vote(_29716,_29718),_30830,_30836),holds(voted(_29716,_29774),_30830)]).

% LPS:  false((vote(_31138,_31140),vote(_31138,_31196),_31140\=_31196)).
% Into: d_pre([happens(vote(_31138,_31140),_32384,_32390),happens(vote(_31138,_31196),_32384,_32390),_31140\=_31196]).

% LPS:  if(at(winningProposal(_32750,_32752),_32774),(at(findall(_32752-_32750,voteCount(_32750,_32752),_32960),_32774),sort(_32960,_33064),append(_33206,[_32752-_32750],_33064))).
% Into: l_int(holds(winningProposal(_32750,_32752),_32774),[holds(findall(_32752-_32750,[holds(voteCount(_32750,_32752),_32774)],_32960),_32774),sort(_32960,_33064),append(_33206,[_32752-_32750],_33064)]).

% LPS:  then(if(winningProposal(_35530,4)),from(lps_terminate,_35594)).
% Into: reactive_rule([holds(winningProposal(_35530,4),_36668)],[happens(lps_terminate,_35594,_37062)]).
% /pack/logicmoo_ec/test/lps_user_examples/Ballot.pl:77
% pop_lps_dialect('$BLOB'("<stream>(0x562ef4163d00)"),  (/.../(lps_user_examples, 'Ballot.pl')-> /.../(lps_user_examples, 'Ballot.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/Ballot.pl':_44738).


initiated(happens(ballot(_, A), _, _), voteCount(B, 0), [member(B, A)]).
initiated(happens(ballot(A, _), _, _), voter(A, 1), []).
initiated(happens(ballot(A, _), _, _), chairman(A), []).
initiated(happens(giveRightToVote(A, B), C, _), voter(B, 1), [holds(chairman(A), C), holds(not(voter(B, _)), C)]).
initiated(happens(delegate(A, B), _, _), voted(A, delegated(B)), []).
initiated(happens(vote(A, B), _, _), voted(A, B), []).

d_pre([happens(ballot(_, _), A, _), holds(voteCount(_, _), A)]).
d_pre([happens(delegate(A, B), C, D), happens(delegate(E, B), C, D), A\=E]).
d_pre([happens(delegate(A, _), B, _), holds(voted(A, _), B)]).
d_pre([happens(delegate(A, B), _, _), A==B]).
d_pre([happens(delegate(A, B), C, D), happens(delegate(A, E), C, D), B\=E]).
d_pre([happens(delegate(A, B), C, _), holds(delegateOf(B, A), C)]).
d_pre([happens(vote(A, B), C, D), happens(vote(E, B), C, D), A\=E]).
d_pre([happens(vote(A, _), B, _), holds(voted(A, _), B)]).
d_pre([happens(vote(A, B), C, D), happens(vote(A, E), C, D), B\=E]).

fluents([chairman(_), voter(A, _), voted(A, B), delegateOf(A, _), voteCount(B, _)]).

l_int(holds(delegateOf(A, B), C), [holds(voted(A, delegated(D)), C), holds(delegateOf(D, B), C)]).
l_int(holds(delegateOf(A, A), B), [holds(not(voted(A, delegated(_))), B)]).
l_int(holds(winningProposal(A, B), C), [holds(findall(B-A, [holds(voteCount(A, B), C)], D), C), sort(D, E), append(_, [B-A], E)]).

reactive_rule([holds(winningProposal(_, 4), _)], [happens(lps_terminate, _, _)]).

:- dynamic actions/1.
:- multifile actions/1.


events([ballot(A, _), giveRightToVote(A, B), delegate(_, _), vote(B, _)]).

observe([ballot(chair, [trump, clinton])], 2).
observe([giveRightToVote(chair, miguel), giveRightToVote(chair, fariba), giveRightToVote(chair, bob), giveRightToVote(chair, jacinto)], 4).
observe([delegate(bob, miguel)], 5).
observe([vote(miguel, clinton)], 6).
observe([delegate(jacinto, bob)], 7).
observe([delegate(fariba, miguel)], 8).

maxTime(15).

updated(happens(delegate(A, _), _, _), voter(A, B), B-0, []).
updated(happens(delegate(A, B), C, _), voter(D, E), E-F, [holds(delegateOf(B, D), C), holds(voter(A, G), C), F is G+E]).
updated(happens(delegate(A, B), C, _), voteCount(D, E), E-F, [holds(delegateOf(B, G), C), holds(voted(G, D), C), holds(voter(A, H), C), F is E+H]).
updated(happens(vote(A, B), C, _), voteCount(B, D), D-E, [holds(voter(A, F), C), E is D+F]).
% dB(/.../(lps_user_examples, 'Ballot.pl'), lps_visualization(_91362{groups:[_87054{content:"Events", id:"event", order:1}, _87128{content:"chairman(A)", id:"chairman/1", order:3, subgroupStack:"false"}, _87206{content:"voteCount(A,B)", id:"voteCount/2", order:3, subgroupStack:"false"}, _87284{content:"voted(A,B)", id:"voted/2", order:3, subgroupStack:"false"}, _87362{content:"voter(A,B)", id:"voter/2", order:3, subgroupStack:"false"}, _87428{content:"Actions", id:"action", order:4}], items:[_87550{content:"chair", end:9, group:"chairman/1", id:0, start:2, subgroup:"chair", title:"Fluent chairman(chair) initiated at 2<br/>and terminated at transition to 9"}, _87676{content:"clinton,0", end:6, group:"voteCount/2", id:1, start:2, subgroup:"clinton", title:"Fluent voteCount(clinton,0) initiated at 2<br/>and terminated at transition to 6"}, _87802{content:"clinton,2", end:7, group:"voteCount/2", id:2, start:6, subgroup:"clinton", title:"Fluent voteCount(clinton,2) initiated at 6<br/>and terminated at transition to 7"}, _87928{content:"clinton,3", end:8, group:"voteCount/2", id:3, start:7, subgroup:"clinton", title:"Fluent voteCount(clinton,3) initiated at 7<br/>and terminated at transition to 8"}, _88054{content:"clinton,4", end:9, group:"voteCount/2", id:4, start:8, subgroup:"clinton", title:"Fluent voteCount(clinton,4) initiated at 8<br/>and terminated at transition to 9"}, _88180{content:"trump,0", end:9, group:"voteCount/2", id:5, start:2, subgroup:"trump", title:"Fluent voteCount(trump,0) initiated at 2<br/>and terminated at transition to 9"}, _88306{content:"bob,delegated(miguel)", end:9, group:"voted/2", id:6, start:5, subgroup:"bob", title:"Fluent voted(bob,delegated(miguel)) initiated at 5<br/>and terminated at transition to 9"}, _88432{content:"fariba,delegated(miguel)", end:9, group:"voted/2", id:7, start:8, subgroup:"fariba", title:"Fluent voted(fariba,delegated(miguel)) initiated at 8<br/>and terminated at transition to 9"}, _88558{content:"jacinto,delegated(bob)", end:9, group:"voted/2", id:8, start:7, subgroup:"jacinto", title:"Fluent voted(jacinto,delegated(bob)) initiated at 7<br/>and terminated at transition to 9"}, _88684{content:"miguel,clinton", end:9, group:"voted/2", id:9, start:6, subgroup:"miguel", title:"Fluent voted(miguel,clinton) initiated at 6<br/>and terminated at transition to 9"}, _88810{content:"bob,0", end:9, group:"voter/2", id:10, start:5, subgroup:"bob", title:"Fluent voter(bob,0) initiated at 5<br/>and terminated at transition to 9"}, _88936{content:"bob,1", end:5, group:"voter/2", id:11, start:4, subgroup:"bob", title:"Fluent voter(bob,1) initiated at 4<br/>and terminated at transition to 5"}, _89062{content:"chair,1", end:9, group:"voter/2", id:12, start:2, subgroup:"chair", title:"Fluent voter(chair,1) initiated at 2<br/>and terminated at transition to 9"}, _89188{content:"fariba,0", end:9, group:"voter/2", id:13, start:8, subgroup:"fariba", title:"Fluent voter(fariba,0) initiated at 8<br/>and terminated at transition to 9"}, _89314{content:"fariba,1", end:8, group:"voter/2", id:14, start:4, subgroup:"fariba", title:"Fluent voter(fariba,1) initiated at 4<br/>and terminated at transition to 8"}, _89440{content:"jacinto,0", end:9, group:"voter/2", id:15, start:7, subgroup:"jacinto", title:"Fluent voter(jacinto,0) initiated at 7<br/>and terminated at transition to 9"}, _89566{content:"jacinto,1", end:7, group:"voter/2", id:16, start:4, subgroup:"jacinto", title:"Fluent voter(jacinto,1) initiated at 4<br/>and terminated at transition to 7"}, _89692{content:"miguel,1", end:5, group:"voter/2", id:17, start:4, subgroup:"miguel", title:"Fluent voter(miguel,1) initiated at 4<br/>and terminated at transition to 5"}, _89818{content:"miguel,2", end:7, group:"voter/2", id:18, start:5, subgroup:"miguel", title:"Fluent voter(miguel,2) initiated at 5<br/>and terminated at transition to 7"}, _89944{content:"miguel,3", end:8, group:"voter/2", id:19, start:7, subgroup:"miguel", title:"Fluent voter(miguel,3) initiated at 7<br/>and terminated at transition to 8"}, _90070{content:"miguel,4", end:9, group:"voter/2", id:20, start:8, subgroup:"miguel", title:"Fluent voter(miguel,4) initiated at 8<br/>and terminated at transition to 9"}, _90196{content:"ballot(chair,[trump,clinton])", group:"event", id:21, start:2, style:"color:#E19735", title:"happens(ballot(chair,[trump,clinton]),1,2)", type:"point"}, _90322{content:"giveRightToVote(chair,miguel)", group:"event", id:22, start:4, style:"color:#E19735", title:"happens(giveRightToVote(chair,miguel),3,4)", type:"point"}, _90448{content:"giveRightToVote(chair,fariba)", group:"event", id:23, start:4, style:"color:#E19735", title:"happens(giveRightToVote(chair,fariba),3,4)", type:"point"}, _90574{content:"giveRightToVote(chair,bob)", group:"event", id:24, start:4, style:"color:#E19735", title:"happens(giveRightToVote(chair,bob),3,4)", type:"point"}, _90700{content:"giveRightToVote(chair,jacinto)", group:"event", id:25, start:4, style:"color:#E19735", title:"happens(giveRightToVote(chair,jacinto),3,4)", type:"point"}, _90826{content:"delegate(bob,miguel)", group:"event", id:26, start:5, style:"color:#E19735", title:"happens(delegate(bob,miguel),4,5)", type:"point"}, _90952{content:"vote(miguel,clinton)", group:"event", id:27, start:6, style:"color:#E19735", title:"happens(vote(miguel,clinton),5,6)", type:"point"}, _91078{content:"delegate(jacinto,bob)", group:"event", id:28, start:7, style:"color:#E19735", title:"happens(delegate(jacinto,bob),6,7)", type:"point"}, _91204{content:"delegate(fariba,miguel)", group:"event", id:29, start:8, style:"color:#E19735", title:"happens(delegate(fariba,miguel),7,8)", type:"point"}, _91330{content:"lps_terminate(unknown)", group:"action", id:30, start:9, style:"color:green", title:"happens(lps_terminate(unknown),8,9)", type:"point"}]}, [])).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'bank.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'bank.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/bank.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'bank.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'bank.pl'), lps= /.../(lps_user_examples, 'bank.pl'), using= /.../(lps_user_examples, 'bank.pl')].
% continue_lps_dialect.
% ops.

% LPS:  actions(transfer(_62346,_62348,_62350)).
% Into: actions([transfer(_62346,_62348,_62350)]).

% LPS:  fluents(balance(_63430,_63432)).
% Into: fluents([balance(_63430,_63432)]).

% LPS:  initially((balance(bob,0),balance(fariba,100))).
% Into: initial_state([balance(bob,0),balance(fariba,100)]).

% LPS:  observe(from(transfer(fariba,bob,10),to(1,2))).
% Into: observe([transfer(fariba,bob,10)],2).

% LPS:  then(if((transfer(fariba,bob,_66830),balance(bob,_66886),_66886>=10)),transfer(bob,fariba,10)).
% Into: reactive_rule([happens(transfer(fariba,bob,_66830),_68198,_68204),holds(balance(bob,_66886),_68204),_66886>=10],[happens(transfer(bob,fariba,10),_68778,_68784)]).

% LPS:  then(if((transfer(bob,fariba,_68866),balance(fariba,_68922),_68922>=20)),transfer(fariba,bob,20)).
% Into: reactive_rule([happens(transfer(bob,fariba,_68866),_70234,_70240),holds(balance(fariba,_68922),_70240),_68922>=20],[happens(transfer(fariba,bob,20),_70814,_70820)]).

% LPS:  if(updates(transfer(_70890,_70892,_70894),in(to(_70930,_70932),balance(_70892,_70930))),_70932 is _70930+_70894).
% Into: updated(happens(transfer(_70890,_70892,_70894),_72450,_72456),balance(_70892,_70930),_70930-_70932,[_70932 is _70930+_70894]).

% LPS:  if(updates(transfer(_72860,_72862,_72864),in(to(_72900,_72902),balance(_72860,_72900))),_72902 is _72900-_72864).
% Into: updated(happens(transfer(_72860,_72862,_72864),_74420,_74426),balance(_72860,_72900),_72900-_72902,[_72902 is _72900-_72864]).

% LPS:  false((transfer(_74838,_74840,_74842),balance(_74838,_74898),_74898-_74842<0)).
% Into: d_pre([happens(transfer(_74838,_74840,_74842),_76154,_76160),holds(balance(_74838,_74898),_76154),_74898-_74842<0]).
% /pack/logicmoo_ec/test/lps_user_examples/bank.pl:50
% pop_lps_dialect('$BLOB'("<stream>(0x562ef4163600)"),  (/.../(lps_user_examples, 'bank.pl')-> /.../(lps_user_examples, 'bank.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/bank.pl':_84656).


d_pre([happens(transfer(A, _, B), C, _), holds(balance(A, D), C), D-B<0]).

fluents([balance(_, _)]).

reactive_rule([happens(transfer(fariba, bob, _), _, A), holds(balance(bob, B), A), B>=10], [happens(transfer(bob, fariba, 10), _, _)]).
reactive_rule([happens(transfer(bob, fariba, _), _, A), holds(balance(fariba, B), A), B>=20], [happens(transfer(fariba, bob, 20), _, _)]).

initial_state([balance(bob, 0), balance(fariba, 100)]).

:- dynamic actions/1.
:- multifile actions/1.

actions([transfer(_, _, _)]).

observe([transfer(fariba, bob, 10)], 2).

maxTime(10).

updated(happens(transfer(_, A, B), _, _), balance(A, C), C-D, [D is C+B]).
updated(happens(transfer(A, _, B), _, _), balance(A, C), C-D, [D is C-B]).
% dB(/.../(lps_user_examples, 'bank.pl'), lps_visualization(_92516{groups:[_88694{content:"Events", id:"event", order:1}, _88768{content:"balance(A,B)", id:"balance/2", order:3, subgroupStack:"false"}, _88834{content:"Actions", id:"action", order:4}], items:[_88956{content:"bob,0", end:2, group:"balance/2", id:0, start:1, subgroup:"bob", title:"Fluent balance(bob,0) initiated at 1<br/>and terminated at transition to 2"}, _89082{content:"bob,0", end:4, group:"balance/2", id:1, start:3, subgroup:"bob", title:"Fluent balance(bob,0) initiated at 3<br/>and terminated at transition to 4"}, _89208{content:"bob,10", end:3, group:"balance/2", id:2, start:2, subgroup:"bob", title:"Fluent balance(bob,10) initiated at 2<br/>and terminated at transition to 3"}, _89334{content:"bob,10", end:6, group:"balance/2", id:3, start:5, subgroup:"bob", title:"Fluent balance(bob,10) initiated at 5<br/>and terminated at transition to 6"}, _89460{content:"bob,20", end:5, group:"balance/2", id:4, start:4, subgroup:"bob", title:"Fluent balance(bob,20) initiated at 4<br/>and terminated at transition to 5"}, _89586{content:"bob,20", end:8, group:"balance/2", id:5, start:7, subgroup:"bob", title:"Fluent balance(bob,20) initiated at 7<br/>and terminated at transition to 8"}, _89712{content:"bob,30", end:7, group:"balance/2", id:6, start:6, subgroup:"bob", title:"Fluent balance(bob,30) initiated at 6<br/>and terminated at transition to 7"}, _89838{content:"bob,30", end:10, group:"balance/2", id:7, start:9, subgroup:"bob", title:"Fluent balance(bob,30) initiated at 9<br/>and terminated at transition to 10"}, _89964{content:"bob,40", end:9, group:"balance/2", id:8, start:8, subgroup:"bob", title:"Fluent balance(bob,40) initiated at 8<br/>and terminated at transition to 9"}, _90090{content:"bob,50", end:11, group:"balance/2", id:9, start:10, subgroup:"bob", title:"Fluent balance(bob,50) initiated at 10<br/>and terminated at transition to 11"}, _90216{content:"fariba,50", end:11, group:"balance/2", id:10, start:10, subgroup:"fariba", title:"Fluent balance(fariba,50) initiated at 10<br/>and terminated at transition to 11"}, _90342{content:"fariba,60", end:9, group:"balance/2", id:11, start:8, subgroup:"fariba", title:"Fluent balance(fariba,60) initiated at 8<br/>and terminated at transition to 9"}, _90468{content:"fariba,70", end:7, group:"balance/2", id:12, start:6, subgroup:"fariba", title:"Fluent balance(fariba,70) initiated at 6<br/>and terminated at transition to 7"}, _90594{content:"fariba,70", end:10, group:"balance/2", id:13, start:9, subgroup:"fariba", title:"Fluent balance(fariba,70) initiated at 9<br/>and terminated at transition to 10"}, _90720{content:"fariba,80", end:5, group:"balance/2", id:14, start:4, subgroup:"fariba", title:"Fluent balance(fariba,80) initiated at 4<br/>and terminated at transition to 5"}, _90846{content:"fariba,80", end:8, group:"balance/2", id:15, start:7, subgroup:"fariba", title:"Fluent balance(fariba,80) initiated at 7<br/>and terminated at transition to 8"}, _90972{content:"fariba,90", end:3, group:"balance/2", id:16, start:2, subgroup:"fariba", title:"Fluent balance(fariba,90) initiated at 2<br/>and terminated at transition to 3"}, _91098{content:"fariba,90", end:6, group:"balance/2", id:17, start:5, subgroup:"fariba", title:"Fluent balance(fariba,90) initiated at 5<br/>and terminated at transition to 6"}, _91224{content:"fariba,100", end:2, group:"balance/2", id:18, start:1, subgroup:"fariba", title:"Fluent balance(fariba,100) initiated at 1<br/>and terminated at transition to 2"}, _91350{content:"fariba,100", end:4, group:"balance/2", id:19, start:3, subgroup:"fariba", title:"Fluent balance(fariba,100) initiated at 3<br/>and terminated at transition to 4"}, _91476{content:"transfer(fariba,bob,10)", group:"event", id:20, start:2, style:"color:#E19735", title:"happens(transfer(fariba,bob,10),1,2)", type:"point"}, _91602{content:"transfer(bob,fariba,10)", group:"action", id:21, start:3, style:"color:green", title:"happens(transfer(bob,fariba,10),2,3)", type:"point"}, _91728{content:"transfer(fariba,bob,20)", group:"action", id:22, start:4, style:"color:green", title:"happens(transfer(fariba,bob,20),3,4)", type:"point"}, _91854{content:"transfer(bob,fariba,10)", group:"action", id:23, start:5, style:"color:green", title:"happens(transfer(bob,fariba,10),4,5)", type:"point"}, _91980{content:"transfer(fariba,bob,20)", group:"action", id:24, start:6, style:"color:green", title:"happens(transfer(fariba,bob,20),5,6)", type:"point"}, _92106{content:"transfer(bob,fariba,10)", group:"action", id:25, start:7, style:"color:green", title:"happens(transfer(bob,fariba,10),6,7)", type:"point"}, _92232{content:"transfer(fariba,bob,20)", group:"action", id:26, start:8, style:"color:green", title:"happens(transfer(fariba,bob,20),7,8)", type:"point"}, _92358{content:"transfer(bob,fariba,10)", group:"action", id:27, start:9, style:"color:green", title:"happens(transfer(bob,fariba,10),8,9)", type:"point"}, _92484{content:"transfer(fariba,bob,20)", group:"action", id:28, start:10, style:"color:green", title:"happens(transfer(fariba,bob,20),9,10)", type:"point"}]}, [])).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'bankTransfer2D.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'bankTransfer2D.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/bankTransfer2D.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'bankTransfer2D.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'bankTransfer2D.pl'), lps= /.../(lps_user_examples, 'bankTransfer2D.pl'), using= /.../(lps_user_examples, 'bankTransfer2D.pl')].
% continue_lps_dialect.
% ops.

% LPS:  actions(transfer(_61330,_61332,_61334)).
% Into: actions([transfer(_61330,_61332,_61334)]).

% LPS:  fluents(balance(_62414,_62416)).
% Into: fluents([balance(_62414,_62416)]).

% LPS:  initially((balance(bob,0),balance(fariba,100))).
% Into: initial_state([balance(bob,0),balance(fariba,100)]).

% LPS:  observe(from(transfer(fariba,bob,10),to(1,2))).
% Into: observe([transfer(fariba,bob,10)],2).

% LPS:  then(if((from(transfer(fariba,bob,_65814),to(_65850,_65852)),at(balance(bob,_65966),_65852),_65966>=10)),from(transfer(bob,fariba,10),to(_65852,_66276))).
% Into: reactive_rule([happens(transfer(fariba,bob,_65814),_65850,_65852),holds(balance(bob,_65966),_65852),_65966>=10],[happens(transfer(bob,fariba,10),_65852,_66276)]).

% LPS:  then(if((from(transfer(bob,fariba,_68924),to(_68960,_68962)),at(balance(fariba,_69076),_68962),_69076>=20)),from(transfer(fariba,bob,20),to(_68962,_69386))).
% Into: reactive_rule([happens(transfer(bob,fariba,_68924),_68960,_68962),holds(balance(fariba,_69076),_68962),_69076>=20],[happens(transfer(fariba,bob,20),_68962,_69386)]).

% LPS:  if(updates(transfer(_72022,_72024,_72026),in(to(_72062,_72064),balance(_72024,_72062))),_72064 is _72062+_72026).
% Into: updated(happens(transfer(_72022,_72024,_72026),_73582,_73588),balance(_72024,_72062),_72062-_72064,[_72064 is _72062+_72026]).

% LPS:  if(updates(transfer(_73992,_73994,_73996),in(to(_74032,_74034),balance(_73992,_74032))),_74034 is _74032-_73996).
% Into: updated(happens(transfer(_73992,_73994,_73996),_75552,_75558),balance(_73992,_74032),_74032-_74034,[_74034 is _74032-_73996]).

% LPS:  false((transfer(_75970,_75972,_75974),balance(_75970,_76030),_76030<_75974)).
% Into: d_pre([happens(transfer(_75970,_75972,_75974),_77232,_77238),holds(balance(_75970,_76030),_77232),_76030<_75974]).

% LPS:  false((transfer(_77894,_77896,_77898),transfer(_77894,_77968,_77970),_77896\=_77968)).
% Into: d_pre([happens(transfer(_77894,_77896,_77898),_79186,_79192),happens(transfer(_77894,_77968,_77970),_79186,_79192),_77896\=_77968]).

% LPS:  false((transfer(_79710,_79712,_79714),transfer(_79782,_79712,_79786),_79710\=_79782)).
% Into: d_pre([happens(transfer(_79710,_79712,_79714),_81002,_81008),happens(transfer(_79782,_79712,_79786),_81002,_81008),_79710\=_79782]).
% /pack/logicmoo_ec/test/lps_user_examples/bankTransfer2D.pl:45
% pop_lps_dialect('$BLOB'("<stream>(0x562ef4160e00)"),  (/.../(lps_user_examples, 'bankTransfer2D.pl')-> /.../(lps_user_examples, 'bankTransfer2D.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/bankTransfer2D.pl':_30044).


d_pre([happens(transfer(A, _, B), C, _), holds(balance(A, D), C), D<B]).
d_pre([happens(transfer(A, B, _), C, D), happens(transfer(A, E, _), C, D), B\=E]).
d_pre([happens(transfer(A, B, _), C, D), happens(transfer(E, B, _), C, D), A\=E]).

d(balance(Person, V), [from:[X, 0], to:[RightX, V], label:Person:V, type:rectangle, fontSize:13, fillColor:'#85bb65']) :-
    (   Person=bob,
        X=50
    ;   Person=fariba,
        X=200
    ),
    RightX is X+70.
d(transfer(From, To, Amount), [type:arrow, label:Amount, from:[FX, 20], to:[TX, 20]]) :-
    (   From=bob,
        FX=120,
        TX=200
    ;   From=fariba,
        FX=200,
        TX=120
    ).
d(timeless, [[type:star, center:[250, 150], points:9, radius1:20, radius2:25, fillColor:yellow, sendToBack], [type:rectangle, from:[0, 0], to:[320, 200], sendToBack, fillColor:[0, 0.746, 1]], [type:ellipse, shadowOffset:5, shadowColor:darkGray, point:[50, 150], size:[110, 40], fillColor:white], [type:ellipse, point:[20, 130], size:[90, 30], fillColor:white]]).

fluents([balance(_, _)]).

reactive_rule([happens(transfer(fariba, bob, _), _, A), holds(balance(bob, B), A), B>=10], [happens(transfer(bob, fariba, 10), A, _)]).
reactive_rule([happens(transfer(bob, fariba, _), _, A), holds(balance(fariba, B), A), B>=20], [happens(transfer(fariba, bob, 20), A, _)]).

initial_state([balance(bob, 0), balance(fariba, 100)]).

:- dynamic actions/1.
:- multifile actions/1.

actions([transfer(_, _, _)]).

observe([transfer(fariba, bob, 10)], 2).

maxTime(10).

updated(happens(transfer(_, A, B), _, _), balance(A, C), C-D, [D is C+B]).
updated(happens(transfer(A, _, B), _, _), balance(A, C), C-D, [D is C-B]).
% dB(/.../(lps_user_examples, 'bankTransfer2D.pl'), lps_visualization(_124530{groups:[_120708{content:"Events", id:"event", order:1}, _120782{content:"balance(A,B)", id:"balance/2", order:3, subgroupStack:"false"}, _120848{content:"Actions", id:"action", order:4}], items:[_120970{content:"bob,0", end:2, group:"balance/2", id:0, start:1, subgroup:"bob", title:"Fluent balance(bob,0) initiated at 1<br/>and terminated at transition to 2"}, _121096{content:"bob,0", end:4, group:"balance/2", id:1, start:3, subgroup:"bob", title:"Fluent balance(bob,0) initiated at 3<br/>and terminated at transition to 4"}, _121222{content:"bob,10", end:3, group:"balance/2", id:2, start:2, subgroup:"bob", title:"Fluent balance(bob,10) initiated at 2<br/>and terminated at transition to 3"}, _121348{content:"bob,10", end:6, group:"balance/2", id:3, start:5, subgroup:"bob", title:"Fluent balance(bob,10) initiated at 5<br/>and terminated at transition to 6"}, _121474{content:"bob,20", end:5, group:"balance/2", id:4, start:4, subgroup:"bob", title:"Fluent balance(bob,20) initiated at 4<br/>and terminated at transition to 5"}, _121600{content:"bob,20", end:8, group:"balance/2", id:5, start:7, subgroup:"bob", title:"Fluent balance(bob,20) initiated at 7<br/>and terminated at transition to 8"}, _121726{content:"bob,30", end:7, group:"balance/2", id:6, start:6, subgroup:"bob", title:"Fluent balance(bob,30) initiated at 6<br/>and terminated at transition to 7"}, _121852{content:"bob,30", end:10, group:"balance/2", id:7, start:9, subgroup:"bob", title:"Fluent balance(bob,30) initiated at 9<br/>and terminated at transition to 10"}, _121978{content:"bob,40", end:9, group:"balance/2", id:8, start:8, subgroup:"bob", title:"Fluent balance(bob,40) initiated at 8<br/>and terminated at transition to 9"}, _122104{content:"bob,50", end:11, group:"balance/2", id:9, start:10, subgroup:"bob", title:"Fluent balance(bob,50) initiated at 10<br/>and terminated at transition to 11"}, _122230{content:"fariba,50", end:11, group:"balance/2", id:10, start:10, subgroup:"fariba", title:"Fluent balance(fariba,50) initiated at 10<br/>and terminated at transition to 11"}, _122356{content:"fariba,60", end:9, group:"balance/2", id:11, start:8, subgroup:"fariba", title:"Fluent balance(fariba,60) initiated at 8<br/>and terminated at transition to 9"}, _122482{content:"fariba,70", end:7, group:"balance/2", id:12, start:6, subgroup:"fariba", title:"Fluent balance(fariba,70) initiated at 6<br/>and terminated at transition to 7"}, _122608{content:"fariba,70", end:10, group:"balance/2", id:13, start:9, subgroup:"fariba", title:"Fluent balance(fariba,70) initiated at 9<br/>and terminated at transition to 10"}, _122734{content:"fariba,80", end:5, group:"balance/2", id:14, start:4, subgroup:"fariba", title:"Fluent balance(fariba,80) initiated at 4<br/>and terminated at transition to 5"}, _122860{content:"fariba,80", end:8, group:"balance/2", id:15, start:7, subgroup:"fariba", title:"Fluent balance(fariba,80) initiated at 7<br/>and terminated at transition to 8"}, _122986{content:"fariba,90", end:3, group:"balance/2", id:16, start:2, subgroup:"fariba", title:"Fluent balance(fariba,90) initiated at 2<br/>and terminated at transition to 3"}, _123112{content:"fariba,90", end:6, group:"balance/2", id:17, start:5, subgroup:"fariba", title:"Fluent balance(fariba,90) initiated at 5<br/>and terminated at transition to 6"}, _123238{content:"fariba,100", end:2, group:"balance/2", id:18, start:1, subgroup:"fariba", title:"Fluent balance(fariba,100) initiated at 1<br/>and terminated at transition to 2"}, _123364{content:"fariba,100", end:4, group:"balance/2", id:19, start:3, subgroup:"fariba", title:"Fluent balance(fariba,100) initiated at 3<br/>and terminated at transition to 4"}, _123490{content:"transfer(fariba,bob,10)", group:"event", id:20, start:2, style:"color:#E19735", title:"happens(transfer(fariba,bob,10),1,2)", type:"point"}, _123616{content:"transfer(bob,fariba,10)", group:"action", id:21, start:3, style:"color:green", title:"happens(transfer(bob,fariba,10),2,3)", type:"point"}, _123742{content:"transfer(fariba,bob,20)", group:"action", id:22, start:4, style:"color:green", title:"happens(transfer(fariba,bob,20),3,4)", type:"point"}, _123868{content:"transfer(bob,fariba,10)", group:"action", id:23, start:5, style:"color:green", title:"happens(transfer(bob,fariba,10),4,5)", type:"point"}, _123994{content:"transfer(fariba,bob,20)", group:"action", id:24, start:6, style:"color:green", title:"happens(transfer(fariba,bob,20),5,6)", type:"point"}, _124120{content:"transfer(bob,fariba,10)", group:"action", id:25, start:7, style:"color:green", title:"happens(transfer(bob,fariba,10),6,7)", type:"point"}, _124246{content:"transfer(fariba,bob,20)", group:"action", id:26, start:8, style:"color:green", title:"happens(transfer(fariba,bob,20),7,8)", type:"point"}, _124372{content:"transfer(bob,fariba,10)", group:"action", id:27, start:9, style:"color:green", title:"happens(transfer(bob,fariba,10),8,9)", type:"point"}, _124498{content:"transfer(fariba,bob,20)", group:"action", id:28, start:10, style:"color:green", title:"happens(transfer(fariba,bob,20),9,10)", type:"point"}]}, _182436{cycles:[[_176560{create:[_176104{center:[250, 150], fillColor:"yellow", id:"timeless", points:9, radius1:20, radius2:25, sendToBack:"true", type:"star"}, _176264{fillColor:[0, 0.746, 1], from:[0, 0], id:"timeless", sendToBack:"true", to:[320, 200], type:"rectangle"}, _176410{fillColor:"white", id:"timeless", point:[50, 150], shadowColor:"darkGray", shadowOffset:5, size:[110, 40], type:"ellipse"}, _176536{fillColor:"white", id:"timeless", point:[20, 130], size:[90, 30], type:"ellipse"}]}], [_176740{create:_176708{fillColor:"#85bb65", fontSize:13, from:[50, 0], id:"balance(bob,0)", label:"bob:0", to:[120, 0], type:"rectangle"}}, _176914{create:_176882{fillColor:"#85bb65", fontSize:13, from:[200, 0], id:"balance(fariba,100)", label:"fariba:100", to:[270, 100], type:"rectangle"}}, _177072{create:_177044{event:"true", from:[200, 20], id:"transfer(fariba,bob,10)", label:10, to:[120, 20], type:"arrow"}}], [_177252{create:_177220{fillColor:"#85bb65", fontSize:13, from:[50, 0], id:"balance(bob,10)", label:"bob:10", to:[120, 10], type:"rectangle"}}, _177426{create:_177394{fillColor:"#85bb65", fontSize:13, from:[200, 0], id:"balance(fariba,90)", label:"fariba:90", to:[270, 90], type:"rectangle"}}, _177584{create:_177556{event:"true", from:[120, 20], id:"transfer(bob,fariba,10)", label:10, to:[200, 20], type:"arrow"}}, _177614{kill:"balance(bob,0)"}, _177644{kill:"balance(fariba,100)"}, _177674{kill:"transfer(fariba,bob,10)"}], [_177854{create:_177822{fillColor:"#85bb65", fontSize:13, from:[50, 0], id:"balance(bob,0)", label:"bob:0", to:[120, 0], type:"rectangle"}}, _178028{create:_177996{fillColor:"#85bb65", fontSize:13, from:[200, 0], id:"balance(fariba,100)", label:"fariba:100", to:[270, 100], type:"rectangle"}}, _178186{create:_178158{event:"true", from:[200, 20], id:"transfer(fariba,bob,20)", label:20, to:[120, 20], type:"arrow"}}, _178216{kill:"balance(bob,10)"}, _178246{kill:"balance(fariba,90)"}, _178276{kill:"transfer(bob,fariba,10)"}], [_178456{create:_178424{fillColor:"#85bb65", fontSize:13, from:[50, 0], id:"balance(bob,20)", label:"bob:20", to:[120, 20], type:"rectangle"}}, _178630{create:_178598{fillColor:"#85bb65", fontSize:13, from:[200, 0], id:"balance(fariba,80)", label:"fariba:80", to:[270, 80], type:"rectangle"}}, _178788{create:_178760{event:"true", from:[120, 20], id:"transfer(bob,fariba,10)", label:10, to:[200, 20], type:"arrow"}}, _178818{kill:"balance(bob,0)"}, _178848{kill:"balance(fariba,100)"}, _178878{kill:"transfer(fariba,bob,20)"}], [_179058{create:_179026{fillColor:"#85bb65", fontSize:13, from:[50, 0], id:"balance(bob,10)", label:"bob:10", to:[120, 10], type:"rectangle"}}, _179232{create:_179200{fillColor:"#85bb65", fontSize:13, from:[200, 0], id:"balance(fariba,90)", label:"fariba:90", to:[270, 90], type:"rectangle"}}, _179390{create:_179362{event:"true", from:[200, 20], id:"transfer(fariba,bob,20)", label:20, to:[120, 20], type:"arrow"}}, _179420{kill:"balance(bob,20)"}, _179450{kill:"balance(fariba,80)"}, _179480{kill:"transfer(bob,fariba,10)"}], [_179660{create:_179628{fillColor:"#85bb65", fontSize:13, from:[50, 0], id:"balance(bob,30)", label:"bob:30", to:[120, 30], type:"rectangle"}}, _179834{create:_179802{fillColor:"#85bb65", fontSize:13, from:[200, 0], id:"balance(fariba,70)", label:"fariba:70", to:[270, 70], type:"rectangle"}}, _179992{create:_179964{event:"true", from:[120, 20], id:"transfer(bob,fariba,10)", label:10, to:[200, 20], type:"arrow"}}, _180022{kill:"balance(bob,10)"}, _180052{kill:"balance(fariba,90)"}, _180082{kill:"transfer(fariba,bob,20)"}], [_180262{create:_180230{fillColor:"#85bb65", fontSize:13, from:[50, 0], id:"balance(bob,20)", label:"bob:20", to:[120, 20], type:"rectangle"}}, _180436{create:_180404{fillColor:"#85bb65", fontSize:13, from:[200, 0], id:"balance(fariba,80)", label:"fariba:80", to:[270, 80], type:"rectangle"}}, _180594{create:_180566{event:"true", from:[200, 20], id:"transfer(fariba,bob,20)", label:20, to:[120, 20], type:"arrow"}}, _180624{kill:"balance(bob,30)"}, _180654{kill:"balance(fariba,70)"}, _180684{kill:"transfer(bob,fariba,10)"}], [_180864{create:_180832{fillColor:"#85bb65", fontSize:13, from:[50, 0], id:"balance(bob,40)", label:"bob:40", to:[120, 40], type:"rectangle"}}, _181038{create:_181006{fillColor:"#85bb65", fontSize:13, from:[200, 0], id:"balance(fariba,60)", label:"fariba:60", to:[270, 60], type:"rectangle"}}, _181196{create:_181168{event:"true", from:[120, 20], id:"transfer(bob,fariba,10)", label:10, to:[200, 20], type:"arrow"}}, _181226{kill:"balance(bob,20)"}, _181256{kill:"balance(fariba,80)"}, _181286{kill:"transfer(fariba,bob,20)"}], [_181466{create:_181434{fillColor:"#85bb65", fontSize:13, from:[50, 0], id:"balance(bob,30)", label:"bob:30", to:[120, 30], type:"rectangle"}}, _181640{create:_181608{fillColor:"#85bb65", fontSize:13, from:[200, 0], id:"balance(fariba,70)", label:"fariba:70", to:[270, 70], type:"rectangle"}}, _181798{create:_181770{event:"true", from:[200, 20], id:"transfer(fariba,bob,20)", label:20, to:[120, 20], type:"arrow"}}, _181828{kill:"balance(bob,40)"}, _181858{kill:"balance(fariba,60)"}, _181888{kill:"transfer(bob,fariba,10)"}], [_182068{create:_182036{fillColor:"#85bb65", fontSize:13, from:[50, 0], id:"balance(bob,50)", label:"bob:50", to:[120, 50], type:"rectangle"}}, _182242{create:_182210{fillColor:"#85bb65", fontSize:13, from:[200, 0], id:"balance(fariba,50)", label:"fariba:50", to:[270, 50], type:"rectangle"}}, _182272{kill:"balance(bob,30)"}, _182302{kill:"balance(fariba,70)"}, _182332{kill:"transfer(fariba,bob,20)"}], [_182368{kill:"balance(bob,50)"}, _182398{kill:"balance(fariba,50)"}, _182428{kill:"timeless"}]]})).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'Basic If Then Else.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'Basic If Then Else.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/Basic If Then Else.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'Basic If Then Else.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'Basic If Then Else.pl'), lps= /.../(lps_user_examples, 'Basic If Then Else.pl'), using= /.../(lps_user_examples, 'Basic If Then Else.pl')].
% continue_lps_dialect.
% ops.

% LPS:  fluents((locked(_80174),trash(_80214),bin(_80254))).
% Into: fluents([locked(_80174),trash(_80214),bin(_80254)]).

% LPS:  actions((dispose(_81378,_81380),keep(_81420))).
% Into: actions([dispose(_81378,_81380),keep(_81420)]).

% LPS:  initially(bin(bucket)).
% Into: initial_state([bin(bucket)]).

% LPS:  then(if(true),then(if(at(bin(_83574),3)),else(dispose(garbage,_83574),keep(garbage)))).
% Into: reactive_rule([],[([holds(bin(_83574),3)]->[happens(dispose(garbage,_83574),_85192,_84896)];[happens(keep(garbage),_85358,_84896)])]).

% LPS:  then(if(true),(from(keep(uhuh),5),terminate(bin(bucket)),then(if(bin(_85804)),else(dispose(garbage,_85804),keep(garbage))))).
% Into: reactive_rule([],[happens(keep(uhuh),5,_87202),happens(terminate(bin(bucket)),_87284,_87330),([holds(bin(_85804),_87330)]->[happens(dispose(garbage,_85804),_87330,_87166)];[happens(keep(garbage),_87888,_87166)])]).
% /pack/logicmoo_ec/test/lps_user_examples/Basic If Then Else.pl:18
% pop_lps_dialect('$BLOB'("<stream>(0x562ef20f0500)"),  (/.../(lps_user_examples, 'Basic If Then Else.pl')-> /.../(lps_user_examples, 'Basic If Then Else.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/Basic If Then Else.pl':_95558).


fluents([locked(_), trash(_), bin(_)]).

reactive_rule([], [([holds(bin(A), 3)]->[happens(dispose(garbage, A), _, B)];[happens(keep(garbage), _, B)])]).
reactive_rule([], [happens(keep(uhuh), 5, _), happens(terminate(bin(bucket)), _, A),  ([holds(bin(B), A)]->[happens(dispose(garbage, B), A, C)];[happens(keep(garbage), _, C)])]).

initial_state([bin(bucket)]).

:- dynamic actions/1.
:- multifile actions/1.

actions([dispose(_, _), keep(_)]).

maxTime(10).
% dB(/.../(lps_user_examples, 'Basic If Then Else.pl'), lps_visualization(_70072{groups:[_69348{content:"bin(A)", id:"bin/1", order:3, subgroupStack:"false"}, _69414{content:"Actions", id:"action", order:4}], items:[_69536{content:"bucket", end:6, group:"bin/1", id:0, start:1, subgroup:"bucket", title:"Fluent bin(bucket) initiated at 1<br/>and terminated at transition to 6"}, _69662{content:"dispose(garbage,bucket)", group:"action", id:1, start:4, style:"color:green", title:"happens(dispose(garbage,bucket),3,4)", type:"point"}, _69788{content:"keep(uhuh)", group:"action", id:2, start:6, style:"color:green", title:"happens(keep(uhuh),5,6)", type:"point"}, _69914{content:"terminate(bin(bucket))", group:"action", id:3, start:6, style:"color:green", title:"happens(terminate(bin(bucket)),5,6)", type:"point"}, _70040{content:"keep(garbage)", group:"action", id:4, start:7, style:"color:green", title:"happens(keep(garbage),6,7)", type:"point"}]}, [])).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'blocks_world_interference.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'blocks_world_interference.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/blocks_world_interference.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'blocks_world_interference.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'blocks_world_interference.pl'), lps= /.../(lps_user_examples, 'blocks_world_interference.pl'), using= /.../(lps_user_examples, 'blocks_world_interference.pl')].
% continue_lps_dialect.
% ops.

% LPS:  fluents(location(_55410,_55412)).
% Into: fluents([location(_55410,_55412)]).

% LPS:  actions(move(_56454,_56456)).
% Into: actions([move(_56454,_56456)]).

% LPS:  initially((location(b,floor),location(c,b),location(a,floor))).
% Into: initial_state([location(b,floor),location(c,b),location(a,floor)]).

% LPS:  observe(from(move(c,a),to(2,3))).
% Into: observe([move(c,a)],3).

% LPS:  then(if(true),from(make_tower([b,a,floor]),to(_60022,_60024))).
% Into: reactive_rule([],[happens(make_tower([b,a,floor]),_60022,_60024)]).

% LPS:  if(at(clear(_61168),_61190),(_61168\=floor,at(not(location(_61342,_61168)),_61190))).
% Into: l_int(holds(clear(_61168),_61190),[_61168\=floor,holds(not(location(_61342,_61168)),_61190)]).

% LPS:  at(clear(floor),_63326).
% Into: l_int(holds(clear(floor),_63326),[]).

% LPS:  if(from(make_tower([_64554,floor]),to(_64616,_64618)),from(make_on(_64554,floor),to(_64616,_64618))).
% Into: l_events(happens(make_tower([_64554,floor]),_64616,_64618),[happens(make_on(_64554,floor),_64616,_64618)]).

% LPS:  if(from(make_tower([_65954,_65974|_65976]),to(_66024,_66026)),(_65974\=floor,from(make_tower([_65974|_65976]),to(_66024,_66260)),from(make_on(_65954,_65974),to(_66260,_66026)))).
% Into: l_events(happens(make_tower([_65954,_65974|_65976]),_66024,_66026),[_65974\=floor,happens(make_tower([_65974|_65976]),_66024,_66260),happens(make_on(_65954,_65974),_66260,_66026)]).

% LPS:  if(from(make_on(_67730,_67732),to(_67768,_67770)),(at(not(location(_67730,_67732)),_67768),from(make_clear(_67732),to(_67768,_68042)),from(make_clear(_67730),to(_68042,_68178)),from(move(_67730,_67732),to(_68178,_67770)))).
% Into: l_events(happens(make_on(_67730,_67732),_67768,_67770),[holds(not(location(_67730,_67732)),_67768),happens(make_clear(_67732),_67768,_68042),happens(make_clear(_67730),_68042,_68178),happens(move(_67730,_67732),_68178,_67770)]).

% LPS:  if(from(make_on(_69696,_69698),to(_69734,_69734)),at(location(_69696,_69698),_69734)).
% Into: l_events(happens(make_on(_69696,_69698),_69734,_69734),[holds(location(_69696,_69698),_69734)]).

% LPS:  if(from(make_clear(_70988),to(_71024,_71024)),at(clear(_70988),_71024)).
% Into: l_events(happens(make_clear(_70988),_71024,_71024),[holds(clear(_70988),_71024)]).

% LPS:  if(from(make_clear(_72246),to(_72282,_72284)),(at(location(_72396,_72246),_72282),from(make_on(_72396,floor),to(_72282,_72284)))).
% Into: l_events(happens(make_clear(_72246),_72282,_72284),[holds(location(_72396,_72246),_72282),happens(make_on(_72396,floor),_72282,_72284)]).

% LPS:  initiates(move(_73768,_73770),location(_73768,_73770)).
% Into: initiated(happens(move(_73768,_73770),_74954,_74960),location(_73768,_73770),[]).

% LPS:  terminates(move(_74900,_74902),location(_74900,_74958)).
% Into: terminated(happens(move(_74900,_74902),_76086,_76092),location(_74900,_74958),[]).
% /pack/logicmoo_ec/test/lps_user_examples/blocks_world_interference.pl:45
% pop_lps_dialect('$BLOB'("<stream>(0x562ef3ca2900)"),  (/.../(lps_user_examples, 'blocks_world_interference.pl')-> /.../(lps_user_examples, 'blocks_world_interference.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/blocks_world_interference.pl':_83638).


initiated(happens(move(A, B), _, _), location(A, B), []).

fluents([location(_, _)]).

l_int(holds(clear(A), B), [A\=floor, holds(not(location(_, A)), B)]).
l_int(holds(clear(floor), _), []).

reactive_rule([], [happens(make_tower([b, a, floor]), _, _)]).

terminated(happens(move(A, _), _, _), location(A, _), []).

initial_state([location(b, floor), location(c, b), location(a, floor)]).

l_events(happens(make_tower([A, floor]), B, C), [happens(make_on(A, floor), B, C)]).
l_events(happens(make_tower([A, B|C]), D, E), [B\=floor, happens(make_tower([B|C]), D, F), happens(make_on(A, B), F, E)]).
l_events(happens(make_on(A, B), C, D), [holds(not(location(A, B)), C), happens(make_clear(B), C, E), happens(make_clear(A), E, F), happens(move(A, B), F, D)]).
l_events(happens(make_on(A, B), C, C), [holds(location(A, B), C)]).
l_events(happens(make_clear(A), B, B), [holds(clear(A), B)]).
l_events(happens(make_clear(A), B, C), [holds(location(D, A), B), happens(make_on(D, floor), B, C)]).

:- dynamic actions/1.
:- multifile actions/1.

actions([move(_, _)]).

observe([move(c, a)], 3).

maxTime(10).
% dB(/.../(lps_user_examples, 'blocks_world_interference.pl'), lps_visualization(_78056{groups:[_76754{content:"Events", id:"event", order:1}, _76828{content:"location(A,B)", id:"location/2", order:3, subgroupStack:"false"}, _76894{content:"Actions", id:"action", order:4}], items:[_77016{content:"a,floor", end:11, group:"location/2", id:0, start:1, subgroup:"a", title:"Fluent location(a,floor) initiated at 1<br/>and terminated at transition to 11"}, _77142{content:"b,a", end:11, group:"location/2", id:1, start:3, subgroup:"b", title:"Fluent location(b,a) initiated at 3<br/>and terminated at transition to 11"}, _77268{content:"b,floor", end:3, group:"location/2", id:2, start:1, subgroup:"b", title:"Fluent location(b,floor) initiated at 1<br/>and terminated at transition to 3"}, _77394{content:"c,a", end:11, group:"location/2", id:3, start:3, subgroup:"c", title:"Fluent location(c,a) initiated at 3<br/>and terminated at transition to 11"}, _77520{content:"c,b", end:2, group:"location/2", id:4, start:1, subgroup:"c", title:"Fluent location(c,b) initiated at 1<br/>and terminated at transition to 2"}, _77646{content:"c,floor", end:3, group:"location/2", id:5, start:2, subgroup:"c", title:"Fluent location(c,floor) initiated at 2<br/>and terminated at transition to 3"}, _77772{content:"move(c,floor)", group:"action", id:6, start:2, style:"color:green", title:"happens(move(c,floor),1,2)", type:"point"}, _77898{content:"move(c,a)", group:"event", id:7, start:3, style:"color:#E19735", title:"happens(move(c,a),2,3)", type:"point"}, _78024{content:"move(b,a)", group:"action", id:8, start:3, style:"color:green", title:"happens(move(b,a),2,3)", type:"point"}]}, [])).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'brujanino.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'brujanino.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/brujanino.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'brujanino.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'brujanino.pl'), lps= /.../(lps_user_examples, 'brujanino.pl'), using= /.../(lps_user_examples, 'brujanino.pl')].
% continue_lps_dialect.
% ops.

% LPS:  fluents(engaña/2).
% Into: fluents([engaña(_57220,_57222)]).

% LPS:  actions((ayuda/2,encuentra/1)).
% Into: actions([ayuda(_58422,_58424),encuentra(_58434)]).

% LPS:  events((necesita/2,escapa/1)).
% Into: events([necesita(_59582,_59584),escapa(_59594)]).

% LPS:  initially(engaña(bruja,niño)).
% Into: initial_state([engaña(bruja,niño)]).

% LPS:  observe(from(necesita(bruja,objeto),to(1,2))).
% Into: observe([necesita(bruja,objeto)],2).

% LPS:  if(initiates(encuentra(_61724),engaña(_61778,_61780)),engaña(_61780,_61778)).
% Into: initiated(happens(encuentra(_61724),_63034,_63040),engaña(_61778,_61780),[holds(engaña(_61780,_61778),_63034)]).

% LPS:  then(if(necesita(bruja,objeto)),ayuda(niño,bruja)).
% Into: reactive_rule([happens(necesita(bruja,objeto),_64420,_64426)],[happens(ayuda(niño,bruja),_64452,_64458)]).

% LPS:  then(if(ayuda(niño,bruja)),encuentra(objeto)).
% Into: reactive_rule([happens(ayuda(niño,bruja),_65546,_65552)],[happens(encuentra(objeto),_65578,_65584)]).

% LPS:  if(escapa(niño),engaña(niño,bruja)).
% Into: l_events(happens(escapa(niño),_66622,_66622),[holds(engaña(niño,bruja),_66622)]).

% LPS:  observe(from(necesita(bruja,objeto),to(4,5))).
% Into: observe([necesita(bruja,objeto)],5).

% LPS:  then(if(necesita(bruja,objeto)),ayuda(niño,bruja)).
% Into: reactive_rule([happens(necesita(bruja,objeto),_69172,_69178)],[happens(ayuda(niño,bruja),_69204,_69210)]).

% LPS:  then(if(ayuda(niño,bruja)),encuentra(objeto)).
% Into: reactive_rule([happens(ayuda(niño,bruja),_70298,_70304)],[happens(encuentra(objeto),_70330,_70336)]).

% LPS:  then(if((encuentra(objeto),engaña(niño,bruja))),escapa(niño)).
% Into: reactive_rule([happens(encuentra(objeto),_71506,_71512),holds(engaña(niño,bruja),_71512)],[happens(escapa(niño),_71604,_71610)]).
% /pack/logicmoo_ec/test/lps_user_examples/brujanino.pl:29
% pop_lps_dialect('$BLOB'("<stream>(0x562ef4162800)"),  (/.../(lps_user_examples, 'brujanino.pl')-> /.../(lps_user_examples, 'brujanino.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/brujanino.pl':_79106).


initiated(happens(encuentra(_), A, _), engaña(B, C), [holds(engaña(C, B), A)]).

fluents([engaña(_, _)]).

reactive_rule([happens(necesita(bruja, objeto), _, _)], [happens(ayuda(niño, bruja), _, _)]).
reactive_rule([happens(ayuda(niño, bruja), _, _)], [happens(encuentra(objeto), _, _)]).
reactive_rule([happens(necesita(bruja, objeto), _, _)], [happens(ayuda(niño, bruja), _, _)]).
reactive_rule([happens(ayuda(niño, bruja), _, _)], [happens(encuentra(objeto), _, _)]).
reactive_rule([happens(encuentra(objeto), _, A), holds(engaña(niño, bruja), A)], [happens(escapa(niño), _, _)]).

initial_state([engaña(bruja, niño)]).

l_events(happens(escapa(niño), A, A), [holds(engaña(niño, bruja), A)]).

:- dynamic actions/1.
:- multifile actions/1.

actions([ayuda(_, _), encuentra(_)]).

events([necesita(_, _), escapa(_)]).

observe([necesita(bruja, objeto)], 2).
observe([necesita(bruja, objeto)], 5).

maxTime(8).
% dB(/.../(lps_user_examples, 'brujanino.pl'), lps_visualization(_66724{groups:[_65548{content:"Events", id:"event", order:1}, _65622{content:"engaña(A,B)", id:"engaña/2", order:3, subgroupStack:"false"}, _65688{content:"Actions", id:"action", order:4}], items:[_65810{content:"bruja,niño", end:9, group:"engaña/2", id:0, start:1, subgroup:"bruja", title:"Fluent engaña(bruja,niño) initiated at 1<br/>and terminated at transition to 9"}, _65936{content:"niño,bruja", end:9, group:"engaña/2", id:1, start:4, subgroup:"niño", title:"Fluent engaña(niño,bruja) initiated at 4<br/>and terminated at transition to 9"}, _66062{content:"necesita(bruja,objeto)", group:"event", id:2, start:2, style:"color:#E19735", title:"happens(necesita(bruja,objeto),1,2)", type:"point"}, _66188{content:"ayuda(niño,bruja)", group:"action", id:3, start:3, style:"color:green", title:"happens(ayuda(niño,bruja),2,3)", type:"point"}, _66314{content:"encuentra(objeto)", group:"action", id:4, start:4, style:"color:green", title:"happens(encuentra(objeto),3,4)", type:"point"}, _66440{content:"necesita(bruja,objeto)", group:"event", id:5, start:5, style:"color:#E19735", title:"happens(necesita(bruja,objeto),4,5)", type:"point"}, _66566{content:"ayuda(niño,bruja)", group:"action", id:6, start:6, style:"color:green", title:"happens(ayuda(niño,bruja),5,6)", type:"point"}, _66692{content:"encuentra(objeto)", group:"action", id:7, start:7, style:"color:green", title:"happens(encuentra(objeto),6,7)", type:"point"}]}, [])).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'buggymartianrobot.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'buggymartianrobot.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/buggymartianrobot.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'buggymartianrobot.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'buggymartianrobot.pl'), lps= /.../(lps_user_examples, 'buggymartianrobot.pl'), using= /.../(lps_user_examples, 'buggymartianrobot.pl')].
% continue_lps_dialect.
% ops.

% LPS:  fluents((at_pos/2,free/2,visited/2,obstacle/2,life/2,lookingtowards/2)).
% Into: fluents([at_pos(_21600,_21602),free(_21612,_21614),visited(_21624,_21626),obstacle(_21636,_21638),life(_21648,_21650),lookingtowards(_21660,_21662)]).

% LPS:  actions((step(_21400,_21402),turn_right,report)).
% Into: actions([step(_21400,_21402),turn_right,report]).

% LPS:  initially((at_pos(0,0),life(2,1),free(1,0),free(2,0),obstacle(3,0),obstacle(2,-1),obstacle(2,1),lookingtowards(1,0))).
% Into: initial_state([at_pos(0,0),life(2,1),free(1,0),free(2,0),obstacle(3,0),obstacle(2,-1),obstacle(2,1),lookingtowards(1,0)]).

% LPS:  terminates(step(_24312,_24314),lookingtowards(_24312,_24314)).
% Into: terminated(happens(step(_24312,_24314),_25506,_25512),lookingtowards(_24312,_24314),[]).

% LPS:  if(initiates(step(_25444,_25446),lookingtowards(_25444,_25502)),(at_pos(_25444,_25590),lookingtowards(_25444,_25446),_25730 is _25446-_25590,abs(_25730)>0,_25502 is _25446+_25730)).
% Into: initiated(happens(step(_25444,_25446),_27344,_27350),lookingtowards(_25444,_25502),[holds(at_pos(_25444,_25590),_27344),holds(lookingtowards(_25444,_25446),_27344),_25730 is _25446-_25590,abs(_25730)>0,_25502 is _25446+_25730]).

% LPS:  if(initiates(step(_28832,_28834),lookingtowards(_28888,_28834)),(at_pos(_28976,_28834),lookingtowards(_28832,_28834),_29118 is _28832-_28976,abs(_29118)>0,_28888 is _28832+_29118)).
% Into: initiated(happens(step(_28832,_28834),_30732,_30738),lookingtowards(_28888,_28834),[holds(at_pos(_28976,_28834),_30732),holds(lookingtowards(_28832,_28834),_30732),_29118 is _28832-_28976,abs(_29118)>0,_28888 is _28832+_29118]).

% LPS:  initiates(step(_32220,_32222),at_pos(_32220,_32222)).
% Into: initiated(happens(step(_32220,_32222),_33406,_33412),at_pos(_32220,_32222),[]).

% LPS:  if(terminates(step(_33352,_33354),at_pos(_33408,_33410)),at_pos(_33408,_33410)).
% Into: terminated(happens(step(_33352,_33354),_34680,_34686),at_pos(_33408,_33410),[holds(at_pos(_33408,_33410),_34680)]).

% LPS:  terminates(step(_34972,_34974),free(_34972,_34974)).
% Into: terminated(happens(step(_34972,_34974),_36158,_36164),free(_34972,_34974),[]).

% LPS:  if(initiates(step(_36104,_36106),visited(_36160,_36162)),at_pos(_36160,_36162)).
% Into: initiated(happens(step(_36104,_36106),_37432,_37438),visited(_36160,_36162),[holds(at_pos(_36160,_36162),_37432)]).

% LPS:  then(if((at(lookingtowards(_37700,_37702),_37724),at(free(_37700,_37702),_37724),at(not(visited(_37700,_37702)),_37724))),from(step(_37700,_37702),to(_37724,_38164))).
% Into: reactive_rule([holds(lookingtowards(_37700,_37702),_37724),holds(free(_37700,_37702),_37724),holds(not(visited(_37700,_37702)),_37724)],[happens(step(_37700,_37702),_37724,_38164)]).
% /pack/logicmoo_ec/test/lps_user_examples/buggymartianrobot.pl:41
% pop_lps_dialect('$BLOB'("<stream>(0x562ef3ca3100)"),  (/.../(lps_user_examples, 'buggymartianrobot.pl')-> /.../(lps_user_examples, 'buggymartianrobot.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/buggymartianrobot.pl':_48448).


initiated(happens(step(A, B), C, _), lookingtowards(A, D), [holds(at_pos(A, E), C), holds(lookingtowards(A, B), C), F is B-E, abs(F)>0, D is B+F]).
initiated(happens(step(A, B), C, _), lookingtowards(D, B), [holds(at_pos(E, B), C), holds(lookingtowards(A, B), C), F is A-E, abs(F)>0, D is A+F]).
initiated(happens(step(A, B), _, _), at_pos(A, B), []).
initiated(happens(step(_, _), A, _), visited(B, C), [holds(at_pos(B, C), A)]).

fluents([at_pos(_, _), free(_, _), visited(_, _), obstacle(_, _), life(_, _), lookingtowards(_, _)]).

terminated(happens(step(A, B), _, _), lookingtowards(A, B), []).
terminated(happens(step(_, _), A, _), at_pos(B, C), [holds(at_pos(B, C), A)]).
terminated(happens(step(A, B), _, _), free(A, B), []).

reactive_rule([holds(lookingtowards(A, B), C), holds(free(A, B), C), holds(not(visited(A, B)), C)], [happens(step(A, B), C, _)]).

initial_state([at_pos(0, 0), life(2, 1), free(1, 0), free(2, 0), obstacle(3, 0), obstacle(2, -1), obstacle(2, 1), lookingtowards(1, 0)]).

:- dynamic actions/1.
:- multifile actions/1.

actions([step(_, _), turn_right, report]).

maxTime(5).
% dB(/.../(lps_user_examples, 'buggymartianrobot.pl'), lps_visualization(_93554{groups:[_91054{content:"at_pos(A,B)", id:"at_pos/2", order:3, subgroupStack:"false"}, _91132{content:"free(A,B)", id:"free/2", order:3, subgroupStack:"false"}, _91210{content:"life(A,B)", id:"life/2", order:3, subgroupStack:"false"}, _91288{content:"lookingtowards(A,B)", id:"lookingtowards/2", order:3, subgroupStack:"false"}, _91366{content:"obstacle(A,B)", id:"obstacle/2", order:3, subgroupStack:"false"}, _91444{content:"visited(A,B)", id:"visited/2", order:3, subgroupStack:"false"}, _91510{content:"Actions", id:"action", order:4}], items:[_91632{content:"0,0", end:2, group:"at_pos/2", id:0, start:1, subgroup:"0", title:"Fluent at_pos(0,0) initiated at 1<br/>and terminated at transition to 2"}, _91758{content:"1,0", end:3, group:"at_pos/2", id:1, start:2, subgroup:"1", title:"Fluent at_pos(1,0) initiated at 2<br/>and terminated at transition to 3"}, _91884{content:"2,0", end:6, group:"at_pos/2", id:2, start:3, subgroup:"2", title:"Fluent at_pos(2,0) initiated at 3<br/>and terminated at transition to 6"}, _92010{content:"1,0", end:2, group:"free/2", id:3, start:1, subgroup:"1", title:"Fluent free(1,0) initiated at 1<br/>and terminated at transition to 2"}, _92136{content:"2,0", end:3, group:"free/2", id:4, start:1, subgroup:"2", title:"Fluent free(2,0) initiated at 1<br/>and terminated at transition to 3"}, _92262{content:"2,1", end:6, group:"life/2", id:5, start:1, subgroup:"2", title:"Fluent life(2,1) initiated at 1<br/>and terminated at transition to 6"}, _92388{content:"1,0", end:2, group:"lookingtowards/2", id:6, start:1, subgroup:"1", title:"Fluent lookingtowards(1,0) initiated at 1<br/>and terminated at transition to 2"}, _92514{content:"2,0", end:3, group:"lookingtowards/2", id:7, start:2, subgroup:"2", title:"Fluent lookingtowards(2,0) initiated at 2<br/>and terminated at transition to 3"}, _92640{content:"3,0", end:6, group:"lookingtowards/2", id:8, start:3, subgroup:"3", title:"Fluent lookingtowards(3,0) initiated at 3<br/>and terminated at transition to 6"}, _92766{content:"2,-1", end:6, group:"obstacle/2", id:9, start:1, subgroup:"2", title:"Fluent obstacle(2,-1) initiated at 1<br/>and terminated at transition to 6"}, _92892{content:"2,1", end:6, group:"obstacle/2", id:10, start:1, subgroup:"2", title:"Fluent obstacle(2,1) initiated at 1<br/>and terminated at transition to 6"}, _93018{content:"3,0", end:6, group:"obstacle/2", id:11, start:1, subgroup:"3", title:"Fluent obstacle(3,0) initiated at 1<br/>and terminated at transition to 6"}, _93144{content:"0,0", end:6, group:"visited/2", id:12, start:2, subgroup:"0", title:"Fluent visited(0,0) initiated at 2<br/>and terminated at transition to 6"}, _93270{content:"1,0", end:6, group:"visited/2", id:13, start:3, subgroup:"1", title:"Fluent visited(1,0) initiated at 3<br/>and terminated at transition to 6"}, _93396{content:"step(1,0)", group:"action", id:14, start:2, style:"color:green", title:"happens(step(1,0),1,2)", type:"point"}, _93522{content:"step(2,0)", group:"action", id:15, start:3, style:"color:green", title:"happens(step(2,0),2,3)", type:"point"}]}, [])).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'buggymartianrobot,pl.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'buggymartianrobot,pl.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/buggymartianrobot,pl.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'buggymartianrobot,pl.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'buggymartianrobot,pl.pl'), lps= /.../(lps_user_examples, 'buggymartianrobot,pl.pl'), using= /.../(lps_user_examples, 'buggymartianrobot,pl.pl')].
% continue_lps_dialect.
% ops.

% LPS:  fluents((at_pos/2,free/2,visited/2,obstacle/2,life/2,lookingtowards/2)).
% Into: fluents([at_pos(_60298,_60300),free(_60310,_60312),visited(_60322,_60324),obstacle(_60334,_60336),life(_60346,_60348),lookingtowards(_60358,_60360)]).

% LPS:  actions((step(_60098,_60100),turn_right,report)).
% Into: actions([step(_60098,_60100),turn_right,report]).

% LPS:  initially((at_pos(0,0),life(2,1),free(1,0),free(2,0),obstacle(3,0),obstacle(2,-1),obstacle(2,1),lookingtowards(1,0))).
% Into: initial_state([at_pos(0,0),life(2,1),free(1,0),free(2,0),obstacle(3,0),obstacle(2,-1),obstacle(2,1),lookingtowards(1,0)]).

% LPS:  terminates(step(_63010,_63012),lookingtowards(_63010,_63012)).
% Into: terminated(happens(step(_63010,_63012),_64196,_64202),lookingtowards(_63010,_63012),[]).

% LPS:  if(initiates(step(_64142,_64144),lookingtowards(_64142,_64200)),(at_pos(_64142,_64288),lookingtowards(_64142,_64144),_64428 is _64144-_64288,abs(_64428)>0,_64200 is _64144+_64428)).
% Into: initiated(happens(step(_64142,_64144),_66042,_66048),lookingtowards(_64142,_64200),[holds(at_pos(_64142,_64288),_66042),holds(lookingtowards(_64142,_64144),_66042),_64428 is _64144-_64288,abs(_64428)>0,_64200 is _64144+_64428]).

% LPS:  if(initiates(step(_67530,_67532),lookingtowards(_67586,_67532)),(at_pos(_67674,_67532),lookingtowards(_67530,_67532),_67816 is _67530-_67674,abs(_67816)>0,_67586 is _67530+_67816)).
% Into: initiated(happens(step(_67530,_67532),_69430,_69436),lookingtowards(_67586,_67532),[holds(at_pos(_67674,_67532),_69430),holds(lookingtowards(_67530,_67532),_69430),_67816 is _67530-_67674,abs(_67816)>0,_67586 is _67530+_67816]).

% LPS:  initiates(step(_70918,_70920),at_pos(_70918,_70920)).
% Into: initiated(happens(step(_70918,_70920),_72104,_72110),at_pos(_70918,_70920),[]).

% LPS:  if(terminates(step(_72050,_72052),at_pos(_72106,_72108)),at_pos(_72106,_72108)).
% Into: terminated(happens(step(_72050,_72052),_73378,_73384),at_pos(_72106,_72108),[holds(at_pos(_72106,_72108),_73378)]).

% LPS:  terminates(step(_73670,_73672),free(_73670,_73672)).
% Into: terminated(happens(step(_73670,_73672),_74856,_74862),free(_73670,_73672),[]).

% LPS:  if(initiates(step(_74802,_74804),visited(_74858,_74860)),at_pos(_74858,_74860)).
% Into: initiated(happens(step(_74802,_74804),_76130,_76136),visited(_74858,_74860),[holds(at_pos(_74858,_74860),_76130)]).

% LPS:  then(if((lookingtowards(_76398,_76400),free(_76398,_76400),not(visited(_76398,_76400)))),step(_76398,_76400)).
% Into: reactive_rule([holds(lookingtowards(_76398,_76400),_77768),holds(free(_76398,_76400),_77768),holds(not(visited(_76398,_76400)),_77768)],[happens(step(_76398,_76400),_78616,_78622)]).
% /pack/logicmoo_ec/test/lps_user_examples/buggymartianrobot,pl.pl:36
% pop_lps_dialect('$BLOB'("<stream>(0x562ef4160300)"),  (/.../(lps_user_examples, 'buggymartianrobot,pl.pl')-> /.../(lps_user_examples, 'buggymartianrobot,pl.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/buggymartianrobot,pl.pl':_86266).


initiated(happens(step(A, B), C, _), lookingtowards(A, D), [holds(at_pos(A, E), C), holds(lookingtowards(A, B), C), F is B-E, abs(F)>0, D is B+F]).
initiated(happens(step(A, B), C, _), lookingtowards(D, B), [holds(at_pos(E, B), C), holds(lookingtowards(A, B), C), F is A-E, abs(F)>0, D is A+F]).
initiated(happens(step(A, B), _, _), at_pos(A, B), []).
initiated(happens(step(_, _), A, _), visited(B, C), [holds(at_pos(B, C), A)]).

fluents([at_pos(_, _), free(_, _), visited(_, _), obstacle(_, _), life(_, _), lookingtowards(_, _)]).

terminated(happens(step(A, B), _, _), lookingtowards(A, B), []).
terminated(happens(step(_, _), A, _), at_pos(B, C), [holds(at_pos(B, C), A)]).
terminated(happens(step(A, B), _, _), free(A, B), []).

reactive_rule([holds(lookingtowards(A, B), C), holds(free(A, B), C), holds(not(visited(A, B)), C)], [happens(step(A, B), _, _)]).

initial_state([at_pos(0, 0), life(2, 1), free(1, 0), free(2, 0), obstacle(3, 0), obstacle(2, -1), obstacle(2, 1), lookingtowards(1, 0)]).

:- dynamic actions/1.
:- multifile actions/1.

actions([step(_, _), turn_right, report]).

maxTime(5).
% dB(/.../(lps_user_examples, 'buggymartianrobot,pl.pl'), lps_visualization(_62876{groups:[_60376{content:"at_pos(A,B)", id:"at_pos/2", order:3, subgroupStack:"false"}, _60454{content:"free(A,B)", id:"free/2", order:3, subgroupStack:"false"}, _60532{content:"life(A,B)", id:"life/2", order:3, subgroupStack:"false"}, _60610{content:"lookingtowards(A,B)", id:"lookingtowards/2", order:3, subgroupStack:"false"}, _60688{content:"obstacle(A,B)", id:"obstacle/2", order:3, subgroupStack:"false"}, _60766{content:"visited(A,B)", id:"visited/2", order:3, subgroupStack:"false"}, _60832{content:"Actions", id:"action", order:4}], items:[_60954{content:"0,0", end:2, group:"at_pos/2", id:0, start:1, subgroup:"0", title:"Fluent at_pos(0,0) initiated at 1<br/>and terminated at transition to 2"}, _61080{content:"1,0", end:3, group:"at_pos/2", id:1, start:2, subgroup:"1", title:"Fluent at_pos(1,0) initiated at 2<br/>and terminated at transition to 3"}, _61206{content:"2,0", end:6, group:"at_pos/2", id:2, start:3, subgroup:"2", title:"Fluent at_pos(2,0) initiated at 3<br/>and terminated at transition to 6"}, _61332{content:"1,0", end:2, group:"free/2", id:3, start:1, subgroup:"1", title:"Fluent free(1,0) initiated at 1<br/>and terminated at transition to 2"}, _61458{content:"2,0", end:3, group:"free/2", id:4, start:1, subgroup:"2", title:"Fluent free(2,0) initiated at 1<br/>and terminated at transition to 3"}, _61584{content:"2,1", end:6, group:"life/2", id:5, start:1, subgroup:"2", title:"Fluent life(2,1) initiated at 1<br/>and terminated at transition to 6"}, _61710{content:"1,0", end:2, group:"lookingtowards/2", id:6, start:1, subgroup:"1", title:"Fluent lookingtowards(1,0) initiated at 1<br/>and terminated at transition to 2"}, _61836{content:"2,0", end:3, group:"lookingtowards/2", id:7, start:2, subgroup:"2", title:"Fluent lookingtowards(2,0) initiated at 2<br/>and terminated at transition to 3"}, _61962{content:"3,0", end:6, group:"lookingtowards/2", id:8, start:3, subgroup:"3", title:"Fluent lookingtowards(3,0) initiated at 3<br/>and terminated at transition to 6"}, _62088{content:"2,-1", end:6, group:"obstacle/2", id:9, start:1, subgroup:"2", title:"Fluent obstacle(2,-1) initiated at 1<br/>and terminated at transition to 6"}, _62214{content:"2,1", end:6, group:"obstacle/2", id:10, start:1, subgroup:"2", title:"Fluent obstacle(2,1) initiated at 1<br/>and terminated at transition to 6"}, _62340{content:"3,0", end:6, group:"obstacle/2", id:11, start:1, subgroup:"3", title:"Fluent obstacle(3,0) initiated at 1<br/>and terminated at transition to 6"}, _62466{content:"0,0", end:6, group:"visited/2", id:12, start:2, subgroup:"0", title:"Fluent visited(0,0) initiated at 2<br/>and terminated at transition to 6"}, _62592{content:"1,0", end:6, group:"visited/2", id:13, start:3, subgroup:"1", title:"Fluent visited(1,0) initiated at 3<br/>and terminated at transition to 6"}, _62718{content:"step(1,0)", group:"action", id:14, start:2, style:"color:green", title:"happens(step(1,0),1,2)", type:"point"}, _62844{content:"step(2,0)", group:"action", id:15, start:3, style:"color:green", title:"happens(step(2,0),2,3)", type:"point"}]}, [])).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'busqueda.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'busqueda.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/busqueda.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'busqueda.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'busqueda.pl'), lps= /.../(lps_user_examples, 'busqueda.pl'), using= /.../(lps_user_examples, 'busqueda.pl')].
% continue_lps_dialect.
% ops.

% LPS:  events((a,a1,a3)).
% Into: events([a,a1,a3]).

% LPS:  actions((a11,a31)).
% Into: actions([a11,a31]).

% LPS:  then(if(true),a).
% Into: reactive_rule([],[happens(a,_21970,_21976)]).

% LPS:  if(a,a2).
% Into: l_events(happens(a,_22990,_22996),[happens(a2,_22990,_22996)]).

% LPS:  if(a,a3).
% Into: l_events(happens(a,_24006,_24012),[happens(a3,_24006,_24012)]).

% LPS:  if(a1,a11).
% Into: l_events(happens(a1,_25022,_25028),[happens(a11,_25022,_25028)]).

% LPS:  if(a3,a31).
% Into: l_events(happens(a3,_26038,_26044),[happens(a31,_26038,_26044)]).
% /pack/logicmoo_ec/test/lps_user_examples/busqueda.pl:17
% pop_lps_dialect('$BLOB'("<stream>(0x562ef4161200)"),  (/.../(lps_user_examples, 'busqueda.pl')-> /.../(lps_user_examples, 'busqueda.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/busqueda.pl':_36354).


a22.

reactive_rule([], [happens(a, _, _)]).

l_events(happens(a, A, B), [happens(a2, A, B)]).
l_events(happens(a, A, B), [happens(a3, A, B)]).
l_events(happens(a1, A, B), [happens(a11, A, B)]).
l_events(happens(a3, A, B), [happens(a31, A, B)]).

a2 :-
    a22,
    writeln(hello).

:- dynamic actions/1.
:- multifile actions/1.

actions([a11, a31]).

events([a, a1, a3]).
hello

% dB(/.../(lps_user_examples, 'busqueda.pl'), lps_visualization(_58436{groups:[_58282{content:"Actions", id:"action", order:4}], items:[_58404{content:"a2", group:"action", id:0, start:2, style:"color:green", title:"happens(a2,1,2)", type:"point"}]}, [])).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'carta.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'carta.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/carta.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'carta.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'carta.pl'), lps= /.../(lps_user_examples, 'carta.pl'), using= /.../(lps_user_examples, 'carta.pl')].
% continue_lps_dialect.
% ops.

% LPS:  fluents((alerta1,alerta2,alerta3)).
% Into: fluents([alerta1,alerta2,alerta3]).

% LPS:  actions(carta).
% Into: actions([carta]).

% LPS:  events((giroMensaje1,giroMensaje2,giroMensaje3,giroMensaje4)).
% Into: events([giroMensaje1,giroMensaje2,giroMensaje3,giroMensaje4]).

% LPS:  observe(from(alerta1,to(1,2))).
% Into: observe([alerta1],2).

% LPS:  then(if(at(alerta1,_24144)),from(giroMensaje1,to(_24144,_24248))).
% Into: reactive_rule([holds(alerta1,_24144)],[happens(giroMensaje1,_24144,_24248)]).

% LPS:  observe(from(alerta2,to(3,4))).
% Into: observe([alerta2],4).

% LPS:  then(if(at(alerta2,_26922)),from(giroMensaje2,to(_26922,_27026))).
% Into: reactive_rule([holds(alerta2,_26922)],[happens(giroMensaje2,_26922,_27026)]).

% LPS:  observe(from(alerta2,to(5,6))).
% Into: observe([alerta2],6).

% LPS:  then(if(at(alerta3,_29700)),from(giroMensaje3,to(_29700,_29804))).
% Into: reactive_rule([holds(alerta3,_29700)],[happens(giroMensaje3,_29700,_29804)]).

% LPS:  observe(from(carta,to(3,5))).
% Into: observe([carta],5).

% LPS:  if(from(giroMensaje4,to(_32484,_32486)),from(carta,to(_32484,_32486))).
% Into: l_events(happens(giroMensaje4,_32484,_32486),[happens(carta,_32484,_32486)]).
% /pack/logicmoo_ec/test/lps_user_examples/carta.pl:29
% pop_lps_dialect('$BLOB'("<stream>(0x562ef3ca4f00)"),  (/.../(lps_user_examples, 'carta.pl')-> /.../(lps_user_examples, 'carta.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/carta.pl':_41318).


fluents([alerta1, alerta2, alerta3]).

reactive_rule([holds(alerta1, A)], [happens(giroMensaje1, A, _)]).
reactive_rule([holds(alerta2, A)], [happens(giroMensaje2, A, _)]).
reactive_rule([holds(alerta3, A)], [happens(giroMensaje3, A, _)]).

l_events(happens(giroMensaje4, A, B), [happens(carta, A, B)]).

:- dynamic actions/1.
:- multifile actions/1.

actions([carta]).

events([giroMensaje1, giroMensaje2, giroMensaje3, giroMensaje4]).

observe([alerta1], 2).
observe([alerta2], 4).
observe([alerta2], 6).
observe([carta], 5).

maxTime(20).
% dB(/.../(lps_user_examples, 'carta.pl'), lps_visualization(_72730{groups:[_72198{content:"Events", id:"event", order:1}], items:[_72320{content:"alerta1", group:"event", id:0, start:2, style:"color:#E19735", title:"happens(alerta1,1,2)", type:"point"}, _72446{content:"alerta2", group:"event", id:1, start:4, style:"color:#E19735", title:"happens(alerta2,3,4)", type:"point"}, _72572{content:"carta", group:"event", id:2, start:5, style:"color:#E19735", title:"happens(carta,4,5)", type:"point"}, _72698{content:"alerta2", group:"event", id:3, start:6, style:"color:#E19735", title:"happens(alerta2,5,6)", type:"point"}]}, [])).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'CCAcompanaB2018.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'CCAcompanaB2018.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/CCAcompanaB2018.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'CCAcompanaB2018.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'CCAcompanaB2018.pl'), lps= /.../(lps_user_examples, 'CCAcompanaB2018.pl'), using= /.../(lps_user_examples, 'CCAcompanaB2018.pl')].
% continue_lps_dialect.
% ops.
% /pack/logicmoo_ec/test/lps_user_examples/CCAcompanaB2018.pl:19
% pop_lps_dialect('$BLOB'("<stream>(0x562ef4161300)"),  (/.../(lps_user_examples, 'CCAcompanaB2018.pl')-> /.../(lps_user_examples, 'CCAcompanaB2018.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/CCAcompanaB2018.pl':_77868).


persona(claudio).
persona(vanessa).
persona(jorge).
persona(jacinto).
persona(aquiles).

:- dynamic actions/1.
:- multifile actions/1.


acompaña(jorge, claudio).
acompaña(claudio, jorge).
acompaña(vanessa, jorge).
acompaña(jorge, vanessa).
acompaña(X, Y) :-
    acompaña(Z, X),
    acompaña(Z, Y),
    not(X=Y),
    !.
% dB(/.../(lps_user_examples, 'CCAcompanaB2018.pl'), lps_visualization(_30614{groups:[], items:[]}, [])).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'checkout.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'checkout.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/checkout.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'checkout.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'checkout.pl'), lps= /.../(lps_user_examples, 'checkout.pl'), using= /.../(lps_user_examples, 'checkout.pl')].
% continue_lps_dialect.
% ops.

% LPS:  fluents((entered_at(_66334,_66336),authorised(_66334,_66392))).
% Into: fluents([entered_at(_66334,_66336),authorised(_66334,_66392)]).

% LPS:  actions((enter_card(_67500),authorise(_67500,_67556))).
% Into: actions([enter_card(_67500),authorise(_67500,_67556)]).

% LPS:  initiates(authorise(_19544,_19546),authorised(_19544,_19546)).
% Into: initiated(happens(authorise(_19544,_19546),_19956,_19962),authorised(_19544,_19546),[]).

% LPS:  initiates(from(enter_card(_19880),to(_19916,_19918)),entered_at(_19880,_19918)).
% Into: initiated(happens(enter_card(_19880),_19916,_19918),entered_at(_19880,_19918),[]).

% LPS:  if(from(checkout(_24382),to(_24418,_24420)),(have_card(_24518),from(enter_card(_24518),to(_24418,_24596)),at(authorised(_24518,_24382),_24420),_24420=<_24596+3)).
% Into: l_events(happens(checkout(_24382),_24418,_24420),[have_card(_24518),happens(enter_card(_24518),_24418,_24596),holds(authorised(_24518,_24382),_24420),_24420=<_24596+3]).

% LPS:  then(if(true),checkout(my_new_laptop)).
% Into: reactive_rule([],[happens(checkout(my_new_laptop),_28370,_28376)]).

% LPS:  false((enter_card(_28370),enter_card(_28410),_28370\=_28410)).
% Into: d_pre([happens(enter_card(_28370),_29582,_29588),happens(enter_card(_28410),_29582,_29588),_28370\=_28410]).

% LPS:  false((to(enter_card(_29938),_29960),entered_at(_30040,_30042),_30042<_29960,_29960=<_30042+3)).
% Into: d_pre([happens(enter_card(_29938),_31408,_29960),holds(entered_at(_30040,_30042),_31524),_30042<_29960,_29960=<_30042+3]).
% /pack/logicmoo_ec/test/lps_user_examples/checkout.pl:33
% pop_lps_dialect('$BLOB'("<stream>(0x562ef4161500)"),  (/.../(lps_user_examples, 'checkout.pl')-> /.../(lps_user_examples, 'checkout.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/checkout.pl':_39640).


initiated(happens(authorise(A, B), _, _), authorised(A, B), []).
initiated(happens(enter_card(A), _, B), entered_at(A, B), []).

d_pre([happens(enter_card(A), B, C), happens(enter_card(D), B, C), A\=D]).
d_pre([happens(enter_card(_), _, A), holds(entered_at(_, B), _), B<A, A=<B+3]).

fluents([entered_at(A, _), authorised(A, _)]).

have_card(1).
have_card(2).

reactive_rule([], [happens(checkout(my_new_laptop), _, _)]).

l_events(happens(checkout(A), B, C), [have_card(D), happens(enter_card(D), B, E), holds(authorised(D, A), C), C=<E+3]).

:- dynamic actions/1.
:- multifile actions/1.

actions([enter_card(A), authorise(A, _)]).

maxTime(12).
% dB(/.../(lps_user_examples, 'checkout.pl'), lps_visualization(_63892{groups:[_63294{content:"entered_at(A,B)", id:"entered_at/2", order:3, subgroupStack:"false"}, _63360{content:"Actions", id:"action", order:4}], items:[_63482{content:"1,2", end:13, group:"entered_at/2", id:0, start:2, subgroup:"1", title:"Fluent entered_at(1,2) initiated at 2<br/>and terminated at transition to 13"}, _63608{content:"2,6", end:13, group:"entered_at/2", id:1, start:6, subgroup:"2", title:"Fluent entered_at(2,6) initiated at 6<br/>and terminated at transition to 13"}, _63734{content:"enter_card(1)", group:"action", id:2, start:2, style:"color:green", title:"happens(enter_card(1),1,2)", type:"point"}, _63860{content:"enter_card(2)", group:"action", id:3, start:6, style:"color:green", title:"happens(enter_card(2),5,6)", type:"point"}]}, [])).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'checkout with more_actions.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'checkout with more_actions.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/checkout with more_actions.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'checkout with more_actions.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'checkout with more_actions.pl'), lps= /.../(lps_user_examples, 'checkout with more_actions.pl'), using= /.../(lps_user_examples, 'checkout with more_actions.pl')].
% continue_lps_dialect.
% ops.

% LPS:  fluents((entered_at(_32210,_32212),authorised(_32210,_32268))).
% Into: fluents([entered_at(_32210,_32212),authorised(_32210,_32268)]).

% LPS:  actions((enter_card(_33376),authorise(_33376,_33432))).
% Into: actions([enter_card(_33376),authorise(_33376,_33432)]).

% LPS:  initiates(authorise(_34532,_34534),authorised(_34532,_34534)).
% Into: initiated(happens(authorise(_34532,_34534),_35718,_35724),authorised(_34532,_34534),[]).

% LPS:  initiates(from(enter_card(_35650),to(_35686,_35688)),entered_at(_35650,_35688)).
% Into: initiated(happens(enter_card(_35650),_35686,_35688),entered_at(_35650,_35688),[]).

% LPS:  then(if(to(enter_card(2),_40150)),from(authorise(2,_40256),_40150+1)).
% Into: reactive_rule([happens(enter_card(2),_41428,_40150)],[happens(authorise(2,_40256),_40150+1,_41518)]).

% LPS:  if(from(checkout(_41476),to(_41512,_41514)),(have_card(_41612),from(enter_card(_41612),to(_41512,_41690)),at(authorised(_41612,_41476),_41514),_41514=<_41690+3)).
% Into: l_events(happens(checkout(_41476),_41512,_41514),[have_card(_41612),happens(enter_card(_41612),_41512,_41690),holds(authorised(_41612,_41476),_41514),_41514=<_41690+3]).

% LPS:  then(if(true),checkout(my_new_laptop)).
% Into: reactive_rule([],[happens(checkout(my_new_laptop),_45452,_45458)]).

% LPS:  false((enter_card(_45452),enter_card(_45492),_45452\=_45492)).
% Into: d_pre([happens(enter_card(_45452),_46664,_46670),happens(enter_card(_45492),_46664,_46670),_45452\=_45492]).

% LPS:  false((to(enter_card(_47020),_47042),entered_at(_47122,_47124),_47124<_47042,_47042=<_47124+3)).
% Into: d_pre([happens(enter_card(_47020),_48490,_47042),holds(entered_at(_47122,_47124),_48606),_47124<_47042,_47042=<_47124+3]).

% LPS:  false((enter_card(_49098),entered_at(_49098,_49154))).
% Into: d_pre([happens(enter_card(_49098),_50208,_50214),holds(entered_at(_49098,_49154),_50208)]).
% /pack/logicmoo_ec/test/lps_user_examples/checkout with more_actions.pl:36
% pop_lps_dialect('$BLOB'("<stream>(0x562ef3ca4800)"),  (/.../(lps_user_examples, 'checkout with more_actions.pl')-> /.../(lps_user_examples, 'checkout with more_actions.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/checkout with more_actions.pl':_57896).


initiated(happens(authorise(A, B), _, _), authorised(A, B), []).
initiated(happens(enter_card(A), _, B), entered_at(A, B), []).

d_pre([happens(enter_card(A), B, C), happens(enter_card(D), B, C), A\=D]).
d_pre([happens(enter_card(_), _, A), holds(entered_at(_, B), _), B<A, A=<B+3]).
d_pre([happens(enter_card(A), B, _), holds(entered_at(A, _), B)]).

fluents([entered_at(A, _), authorised(A, _)]).

have_card(1).
have_card(2).

reactive_rule([happens(enter_card(2), _, A)], [happens(authorise(2, _), A+1, _)]).
reactive_rule([], [happens(checkout(my_new_laptop), _, _)]).

l_events(happens(checkout(A), B, C), [have_card(D), happens(enter_card(D), B, E), holds(authorised(D, A), C), C=<E+3]).

:- dynamic actions/1.
:- multifile actions/1.

actions([enter_card(A), authorise(A, _)]).

maxTime(12).
% dB(/.../(lps_user_examples, 'checkout with more_actions.pl'), lps_visualization(_87278{groups:[_86350{content:"authorised(A,B)", id:"authorised/2", order:3, subgroupStack:"false"}, _86428{content:"entered_at(A,B)", id:"entered_at/2", order:3, subgroupStack:"false"}, _86494{content:"Actions", id:"action", order:4}], items:[_86616{content:"2,A", end:13, group:"authorised/2", id:0, start:8, subgroup:"2", title:"Fluent authorised(2,A) initiated at 8<br/>and terminated at transition to 13"}, _86742{content:"1,2", end:13, group:"entered_at/2", id:1, start:2, subgroup:"1", title:"Fluent entered_at(1,2) initiated at 2<br/>and terminated at transition to 13"}, _86868{content:"2,6", end:13, group:"entered_at/2", id:2, start:6, subgroup:"2", title:"Fluent entered_at(2,6) initiated at 6<br/>and terminated at transition to 13"}, _86994{content:"enter_card(1)", group:"action", id:3, start:2, style:"color:green", title:"happens(enter_card(1),1,2)", type:"point"}, _87120{content:"enter_card(2)", group:"action", id:4, start:6, style:"color:green", title:"happens(enter_card(2),5,6)", type:"point"}, _87246{content:"authorise(2,A)", group:"action", id:5, start:8, style:"color:green", title:"happens(authorise(2,A),7,8)", type:"point"}]}, [])).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'completeness.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'completeness.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/completeness.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'completeness.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'completeness.pl'), lps= /.../(lps_user_examples, 'completeness.pl'), using= /.../(lps_user_examples, 'completeness.pl')].
% continue_lps_dialect.
% ops.

% LPS:  events((e1,e2,e3,a1)).
% Into: events([e1,e2,e3,a1]).

% LPS:  actions(a1).
% Into: actions([a1]).

% LPS:  fluents(p).
% Into: fluents([p]).

% LPS:  then(if(from(e1,_57732)),(from(a1,_57820),_57820>_57732)).
% Into: reactive_rule([happens(e1,_57732,_59020)],[happens(a1,_57820,_59130),_57820>_57732]).

% LPS:  then(if(from(e2,_60000)),at(p,_60000+3)).
% Into: reactive_rule([happens(e2,_60000,_61222)],[holds(p,_60000+3)]).

% LPS:  initiates(a1,p).
% Into: initiated(happens(a1,_62794,_62800),p,[]).

% LPS:  terminates(e3,p).
% Into: terminated(happens(e3,_63810,_63816),p,[]).

% LPS:  observe(from(e1,1)).
% Into: observe([e1],2).

% LPS:  observe(from(e3,3)).
% Into: observe([e3],4).

% LPS:  observe(from(e2,4)).
% Into: observe([e2],5).
% /pack/logicmoo_ec/test/lps_user_examples/completeness.pl:15
% pop_lps_dialect('$BLOB'("<stream>(0x562ef4160900)"),  (/.../(lps_user_examples, 'completeness.pl')-> /.../(lps_user_examples, 'completeness.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/completeness.pl':_74518).


initiated(happens(a1, _, _), p, []).

fluents([p]).

reactive_rule([happens(e1, A, _)], [happens(a1, B, _), B>A]).
reactive_rule([happens(e2, A, _)], [holds(p, A+3)]).

terminated(happens(e3, _, _), p, []).

:- dynamic actions/1.
:- multifile actions/1.

actions([a1]).

events([e1, e2, e3, a1]).

observe([e1], 2).
observe([e3], 4).
observe([e2], 5).
PROGRAM FAILED
% dB(/.../(lps_user_examples, 'completeness.pl'), lps_visualization(_36108{groups:[_35326{content:"Events", id:"event", order:1}, _35400{content:"p", id:"p/0", order:3, subgroupStack:"false"}, _35466{content:"Actions", id:"action", order:4}], items:[_35576{content:"p", end:4, group:"p/0", id:0, start:3, title:"Fluent p initiated at 3<br/>and terminated at transition to 4"}, _35698{content:"e1", group:"event", id:1, start:2, style:"color:#E19735", title:"happens(e1,1,2)", type:"point"}, _35824{content:"a1", group:"action", id:2, start:3, style:"color:green", title:"happens(a1,2,3)", type:"point"}, _35950{content:"e3", group:"event", id:3, start:4, style:"color:#E19735", title:"happens(e3,3,4)", type:"point"}, _36076{content:"e2", group:"event", id:4, start:5, style:"color:#E19735", title:"happens(e2,4,5)", type:"point"}]}, [])).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'complex loan.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'complex loan.pl')).
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/complex loan.pl':_52822).

% dB(/.../(lps_user_examples, 'complex loan.pl'), lps_visualization(_67528{groups:[], items:[]}, [])).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'consolidated loan.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'consolidated loan.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/consolidated loan.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'consolidated loan.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'consolidated loan.pl'), lps= /.../(lps_user_examples, 'consolidated loan.pl'), using= /.../(lps_user_examples, 'consolidated loan.pl')].
% continue_lps_dialect.
% ops.
% /pack/logicmoo_ec/test/lps_user_examples/consolidated loan.pl:3
% pop_lps_dialect('$BLOB'("<stream>(0x562ef3ca2500)"),  (/.../(lps_user_examples, 'consolidated loan.pl')-> /.../(lps_user_examples, 'consolidated loan.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/consolidated loan.pl':_109892).


:- dynamic actions/1.
:- multifile actions/1.

% dB(/.../(lps_user_examples, 'consolidated loan.pl'), lps_visualization(_31014{groups:[], items:[]}, [])).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'contagion2.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'contagion2.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/contagion2.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'contagion2.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'contagion2.pl'), lps= /.../(lps_user_examples, 'contagion2.pl'), using= /.../(lps_user_examples, 'contagion2.pl')].
% continue_lps_dialect.
% ops.
% /pack/logicmoo_ec/test/lps_user_examples/contagion2.pl:3
% pop_lps_dialect('$BLOB'("<stream>(0x562ef4162500)"),  (/.../(lps_user_examples, 'contagion2.pl')-> /.../(lps_user_examples, 'contagion2.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/contagion2.pl':_73344).


:- dynamic actions/1.
:- multifile actions/1.

% dB(/.../(lps_user_examples, 'contagion2.pl'), lps_visualization(_31008{groups:[], items:[]}, [])).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'contagionlps.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'contagionlps.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/contagionlps.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'contagionlps.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'contagionlps.pl'), lps= /.../(lps_user_examples, 'contagionlps.pl'), using= /.../(lps_user_examples, 'contagionlps.pl')].
% continue_lps_dialect.
% ops.

% LPS:  fluents((contaminated/3,infected/3,met/3)).
% Into: fluents([contaminated(_20026,_20028,_20030),infected(_20040,_20042,_20044),met(_20054,_20056,_20058)]).

% LPS:  events((tested(_19966,_19968,_19970),meets(_19966,_20040,_19970))).
% Into: events([tested(_19966,_19968,_19970),meets(_19966,_20040,_19970)]).

% LPS:  if(initiates(tested(_21186,positive,_21190),contaminated(_21186,_21260,_21262)),(two_week_after(_21190,_21262),five_days_before(_21260,_21190))).
% Into: initiated(happens(tested(_21186,positive,_21190),_22640,_22646),contaminated(_21186,_21260,_21262),[two_week_after(_21190,_21262),five_days_before(_21260,_21190)]).

% LPS:  if(initiates(meets(_23392,_23394,_23396),contaminated(_23394,_23466,_23468)),(at(contamination(_23392,[],_23644,(_23608,_23610)),_23668),within(_23608,_23396,_23610),five_days_after(_23608,_23466),two_week_after(_23608,_23468))).
% Into: initiated(happens(meets(_23392,_23394,_23396),_25254,_25260),contaminated(_23394,_23466,_23468),[holds(contamination(_23392,[],_23644,(_23608,_23610)),_23668),within(_23608,_23396,_23610),five_days_after(_23608,_23466),two_week_after(_23608,_23468)]).

% LPS:  initiates(meets(_26622,_26624,_26626),met(_26622,_26624,_26626)).
% Into: initiated(happens(meets(_26622,_26624,_26626),_27846,_27852),met(_26622,_26624,_26626),[]).

% LPS:  initiates(meets(_27802,_27804,_27806),met(_27804,_27802,_27806)).
% Into: initiated(happens(meets(_27802,_27804,_27806),_29026,_29032),met(_27804,_27802,_27806),[]).

% LPS:  if(contamination(_29054,_29056,_29056,(_29022,_29024)),contaminated(_29054,_29022,_29024)).
% Into: l_int(holds(contamination(_29054,_29056,_29056,(_29022,_29024)),_30210),[holds(contaminated(_29054,_29022,_29024),_30210)]).

% LPS:  if(contamination(_30778,_30780,_30782,(_30746,_30748)),(met(_30778,_30854,_30856),not(member(_30854,_30780)),contamination(_30854,[_30854|_30780],_30782,(_31078,_31080)),within(_31078,_30856,_31080),five_days_after(_30856,_30746),two_week_after(_30856,_30748))).
% Into: l_int(holds(contamination(_30778,_30780,_30782,(_30746,_30748)),_32680),[holds(met(_30778,_30854,_30856),_32680),not(member(_30854,_30780)),holds(contamination(_30854,[_30854|_30780],_30782,(_31078,_31080)),_32680),within(_31078,_30856,_31080),five_days_after(_30856,_30746),two_week_after(_30856,_30748)]).

% LPS:  observe(from(meets(gertrude,alice,date(2020,3,15,0,0,0,0,'UTC',-)),to(2,3))).
% Into: observe([meets(gertrude,alice,date(2020,3,15,0,0,0,0,'UTC',-))],3).

% LPS:  observe(from(tested(alice,positive,date(2020,3,14,0,0,0,0,'UTC',-)),to(4,5))).
% Into: observe([tested(alice,positive,date(2020,3,14,0,0,0,0,'UTC',-))],5).
% /pack/logicmoo_ec/test/lps_user_examples/contagionlps.pl:107
% pop_lps_dialect('$BLOB'("<stream>(0x562ef4161000)"),  (/.../(lps_user_examples, 'contagionlps.pl')-> /.../(lps_user_examples, 'contagionlps.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/contagionlps.pl':_60844).


initiated(happens(tested(A, positive, B), _, _), contaminated(A, C, D), [two_week_after(B, D), five_days_before(C, B)]).
initiated(happens(meets(A, B, C), _, _), contaminated(B, D, E), [holds(contamination(A, [], _,  (F, G)), _), within(F, C, G), five_days_after(F, D), two_week_after(F, E)]).
initiated(happens(meets(A, B, C), _, _), met(A, B, C), []).
initiated(happens(meets(A, B, C), _, _), met(B, A, C), []).

fluents([contaminated(_, _, _), infected(_, _, _), met(_, _, _)]).

l_int(holds(contamination(A, B, B,  (C, D)), E), [holds(contaminated(A, C, D), E)]).
l_int(holds(contamination(A, B, C,  (D, E)), F), [holds(met(A, G, H), F), not(member(G, B)), holds(contamination(G, [G|B], C,  (I, J)), F), within(I, H, J), five_days_after(H, D), two_week_after(H, E)]).

two_week_after(date(Y, M, D, H, Mn, S, Off, TZ, DST), Date2) :-
    nonvar(D),
    NewD is D+15,
    date_time_stamp(date(Y,
                         M,
                         NewD,
                         H,
                         Mn,
                         S,
                         Off,
                         TZ,
                         DST),
                    Stamp),
    stamp_date_time(Stamp, Date2, 0).
two_week_after(Date2, date(Y, M, D, H, Mn, S, Off, TZ, DST)) :-
    nonvar(D),
    NewD is D+ -15,
    date_time_stamp(date(Y,
                         M,
                         NewD,
                         H,
                         Mn,
                         S,
                         Off,
                         TZ,
                         DST),
                    Stamp),
    stamp_date_time(Stamp, Date2, 0).

within(Date1, T, Date2) :-
    nonvar(Date1),
    nonvar(Date2),
    nonvar(T),
    date_time_stamp(Date1, Stamp1),
    date_time_stamp(Date2, Stamp2),
    date_time_stamp(T, Stamp3),
    Stamp1=<Stamp3,
    Stamp3=<Stamp2.

maxtime(20).

five_days_before(date(Y, M, D, H, Mn, S, Off, TZ, DST), Date2) :-
    nonvar(D),
    NewD is D+5,
    date_time_stamp(date(Y,
                         M,
                         NewD,
                         H,
                         Mn,
                         S,
                         Off,
                         TZ,
                         DST),
                    Stamp),
    stamp_date_time(Stamp, Date2, 0).
five_days_before(Date2, date(Y, M, D, H, Mn, S, Off, TZ, DST)) :-
    nonvar(D),
    NewD is D+ -5,
    date_time_stamp(date(Y,
                         M,
                         NewD,
                         H,
                         Mn,
                         S,
                         Off,
                         TZ,
                         DST),
                    Stamp),
    stamp_date_time(Stamp, Date2, 0).

:- dynamic actions/1.
:- multifile actions/1.


five_days_after(date(Y, M, D, H, Mn, S, Off, TZ, DST), Date2) :-
    nonvar(D),
    NewD is D+5,
    date_time_stamp(date(Y,
                         M,
                         NewD,
                         H,
                         Mn,
                         S,
                         Off,
                         TZ,
                         DST),
                    Stamp),
    stamp_date_time(Stamp, Date2, 0).
five_days_after(Date2, date(Y, M, D, H, Mn, S, Off, TZ, DST)) :-
    nonvar(D),
    NewD is D+ -5,
    date_time_stamp(date(Y,
                         M,
                         NewD,
                         H,
                         Mn,
                         S,
                         Off,
                         TZ,
                         DST),
                    Stamp),
    stamp_date_time(Stamp, Date2, 0).

events([tested(A, _, B), meets(A, _, B)]).

observe([meets(gertrude, alice, date(2020, 3, 15, 0, 0, 0, 0, 'UTC', -))], 3).
observe([tested(alice, positive, date(2020, 3, 14, 0, 0, 0, 0, 'UTC', -))], 5).
% dB(/.../(lps_user_examples, 'contagionlps.pl'), lps_visualization(_70552{groups:[_69738{content:"Events", id:"event", order:1}, _69812{content:"contaminated(A,B,C)", id:"contaminated/3", order:3, subgroupStack:"false"}, _69890{content:"met(A,B,C)", id:"met/3", order:3, subgroupStack:"false"}], items:[_70016{content:"alice,date(2020,3,9,0,0,0.0,0,-,-),date(2020,3,29,0,0,0.0,0,-,-)", end:21, group:"contaminated/3", id:0, start:5, subgroup:"alice", title:"Fluent contaminated(alice,date(2020,3,9,0,0,0.0,0,-,-),date(2020,3,29,0,0,0.0,0,-,-)) initiated at 5<br/>and terminated at transition to 21"}, _70142{content:"alice,gertrude,date(2020,3,15,0,0,0,0,UTC,-)", end:21, group:"met/3", id:1, start:3, subgroup:"alice", title:"Fluent met(alice,gertrude,date(2020,3,15,0,0,0,0,UTC,-)) initiated at 3<br/>and terminated at transition to 21"}, _70268{content:"gertrude,alice,date(2020,3,15,0,0,0,0,UTC,-)", end:21, group:"met/3", id:2, start:3, subgroup:"gertrude", title:"Fluent met(gertrude,alice,date(2020,3,15,0,0,0,0,UTC,-)) initiated at 3<br/>and terminated at transition to 21"}, _70394{content:"meets(gertrude,alice,date(2020,3,15,0,0,0,0,UTC,-))", group:"event", id:3, start:3, style:"color:#E19735", title:"happens(meets(gertrude,alice,date(2020,3,15,0,0,0,0,UTC,-)),2,3)", type:"point"}, _70520{content:"tested(alice,positive,date(2020,3,14,0,0,0,0,UTC,-))", group:"event", id:4, start:5, style:"color:#E19735", title:"happens(tested(alice,positive,date(2020,3,14,0,0,0,0,UTC,-)),4,5)", type:"point"}]}, [])).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'contagionnet.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'contagionnet.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/contagionnet.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'contagionnet.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'contagionnet.pl'), lps= /.../(lps_user_examples, 'contagionnet.pl'), using= /.../(lps_user_examples, 'contagionnet.pl')].
% continue_lps_dialect.
% ops.
% /pack/logicmoo_ec/test/lps_user_examples/contagionnet.pl:85
% pop_lps_dialect('$BLOB'("<stream>(0x562ef5a59a00)"),  (/.../(lps_user_examples, 'contagionnet.pl')-> /.../(lps_user_examples, 'contagionnet.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/contagionnet.pl':_42318).


two_week_after(date(Y, M, D, H, Mn, S, Off, TZ, DST), Date2) :-
    nonvar(D),
    NewD is D+15,
    date_time_stamp(date(Y,
                         M,
                         NewD,
                         H,
                         Mn,
                         S,
                         Off,
                         TZ,
                         DST),
                    Stamp),
    stamp_date_time(Stamp, Date2, 0).
two_week_after(Date2, date(Y, M, D, H, Mn, S, Off, TZ, DST)) :-
    nonvar(D),
    NewD is D+ -15,
    date_time_stamp(date(Y,
                         M,
                         NewD,
                         H,
                         Mn,
                         S,
                         Off,
                         TZ,
                         DST),
                    Stamp),
    stamp_date_time(Stamp, Date2, 0).

within(Date1, T, Date2) :-
    nonvar(Date1),
    nonvar(Date2),
    nonvar(T),
    date_time_stamp(Date1, Stamp1),
    date_time_stamp(Date2, Stamp2),
    date_time_stamp(T, Stamp3),
    Stamp1=<Stamp3,
    Stamp3=<Stamp2.

five_days_before(date(Y, M, D, H, Mn, S, Off, TZ, DST), Date2) :-
    nonvar(D),
    NewD is D+5,
    date_time_stamp(date(Y,
                         M,
                         NewD,
                         H,
                         Mn,
                         S,
                         Off,
                         TZ,
                         DST),
                    Stamp),
    stamp_date_time(Stamp, Date2, 0).
five_days_before(Date2, date(Y, M, D, H, Mn, S, Off, TZ, DST)) :-
    nonvar(D),
    NewD is D+ -5,
    date_time_stamp(date(Y,
                         M,
                         NewD,
                         H,
                         Mn,
                         S,
                         Off,
                         TZ,
                         DST),
                    Stamp),
    stamp_date_time(Stamp, Date2, 0).

:- dynamic actions/1.
:- multifile actions/1.


met(A, B, T) :-
    (   observe(meets(A, B), T)
    ;   observe(meets(B, A), T)
    ).

observe(meets(alice, bob), date(2020, 3, 1, 0, 0, 0, 0, 'UTC', -)).
observe(meets(bob, charlie), date(2020, 3, 6, 0, 0, 0, 0, 'UTC', -)).
observe(meets(bob, delilah), date(2020, 3, 6, 0, 0, 0, 0, 'UTC', -)).
observe(meets(delilah, edgar), date(2020, 3, 12, 0, 0, 0, 0, 'UTC', -)).
observe(meets(delilah, fiona), date(2020, 3, 12, 0, 0, 0, 0, 'UTC', -)).
observe(meets(delilah, gertrude), date(2020, 3, 12, 0, 0, 0, 0, 'UTC', -)).
observe(meets(iona, edgar), date(2020, 3, 12, 0, 0, 0, 0, 'UTC', -)).
observe(meets(iona, fiona), date(2020, 3, 12, 0, 0, 0, 0, 'UTC', -)).
observe(meets(iona, gertrude), date(2020, 3, 12, 0, 0, 0, 0, 'UTC', -)).
observe(meets(edgar, hannah), date(2020, 3, 14, 0, 0, 0, 0, 'UTC', -)).
observe(meets(fiona, hannah), date(2020, 3, 14, 0, 0, 0, 0, 'UTC', -)).
observe(meets(gertrude, hannah), date(2020, 3, 14, 0, 0, 0, 0, 'UTC', -)).
observe(meets(edgar, iona), date(2020, 3, 14, 0, 0, 0, 0, 'UTC', -)).
observe(meets(fiona, iona), date(2020, 3, 14, 0, 0, 0, 0, 'UTC', -)).
observe(meets(gertrude, iona), date(2020, 3, 14, 0, 0, 0, 0, 'UTC', -)).
observe(tested(alice, positive), date(2020, 3, 15, 0, 0, 0, 0, 'UTC', -)).

contaminated(A, Path, Path,  (T1, T2)) :-
    observe(tested(A, positive), Date),
    five_days_before(T1, Date),
    two_week_after(Date, T2).
contaminated(A, Path, FPath,  (Tm, T2)) :-
    met(A, B, Tm),
    not(member(B, Path)),
    contaminated(B,
                 [B|Path],
                 FPath,
                 (TB1, TB2)),
    within(TB1, Tm, TB2),
    two_week_after(Tm, T2).
% dB(/.../(lps_user_examples, 'contagionnet.pl'), lps_visualization(_53084{groups:[], items:[]}, [])).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'contagion.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'contagion.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/contagion.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'contagion.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'contagion.pl'), lps= /.../(lps_user_examples, 'contagion.pl'), using= /.../(lps_user_examples, 'contagion.pl')].
% continue_lps_dialect.
% ops.
% /pack/logicmoo_ec/test/lps_user_examples/contagion.pl:3
% pop_lps_dialect('$BLOB'("<stream>(0x562ef5a57300)"),  (/.../(lps_user_examples, 'contagion.pl')-> /.../(lps_user_examples, 'contagion.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/contagion.pl':_95400).


:- dynamic actions/1.
:- multifile actions/1.

% dB(/.../(lps_user_examples, 'contagion.pl'), lps_visualization(_31114{groups:[], items:[]}, [])).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'contrato116.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'contrato116.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/contrato116.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'contrato116.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'contrato116.pl'), lps= /.../(lps_user_examples, 'contrato116.pl'), using= /.../(lps_user_examples, 'contrato116.pl')].
% continue_lps_dialect.
% ops.

% LPS:  actions((dragar(_19502,_19504,_19506),botar(_19502,_19504,_19562),pagar(_19504),paralizar(_19618),modificar(_19646))).
% Into: actions([dragar(_19502,_19504,_19506),botar(_19502,_19504,_19562),pagar(_19504),paralizar(_19618),modificar(_19646)]).

% LPS:  events(validar(_20210,_20212,_20214)).
% Into: events([validar(_20210,_20212,_20214)]).

% LPS:  fluents((obra_paralizada,especificacion(_21406),cuenta_camion(_21446),por_pagar(_21486),validado(_21446,_21556,_21558))).
% Into: fluents([obra_paralizada,especificacion(_21406),cuenta_camion(_21446),por_pagar(_21486),validado(_21446,_21556,_21558)]).

% LPS:  if(from(excavar(_22832,_22834,_22836),to(_22872,_22874)),(at(especificacion(_22972),_22872),at(cuenta_camion(_22832),_22872),conforme_a(_22972,_22834,_22836),capacidad(_22832,_22834),from(dragar(_22832,_22834,_22836),to(_22872,_22874)))).
% Into: l_events(happens(excavar(_22832,_22834,_22836),_22872,_22874),[holds(especificacion(_22972),_22872),holds(cuenta_camion(_22832),_22872),conforme_a(_22972,_22834,_22836),capacidad(_22832,_22834),happens(dragar(_22832,_22834,_22836),_22872,_22874)]).

% LPS:  then(if((at(not(obra_paralizada),_30204),at(not(fecha_tope(_30204)),_30204))),from(excavar(_30466,_30468,_30470),to(_30204,_30508))).
% Into: reactive_rule([holds(not(obra_paralizada),_30204),holds(not(fecha_tope(_30204)),_30204)],[happens(excavar(_30466,_30468,_30470),_30204,_30508)]).

% LPS:  then(if(from(dragar(_31740,_31742,aqui),to(_31780,_31782))),from(botar(_31740,_31742,alli),to(_31782,_31974))).
% Into: reactive_rule([happens(dragar(_31740,_31742,aqui),_31780,_31782)],[happens(botar(_31740,_31742,alli),_31782,_31974)]).

% LPS:  if(updates(dragar(_33188,_33190,_33192),in(to(_33188,_33230),cuenta_camion(_33188))),_33230 is _33188+1).
% Into: updated(happens(dragar(_33188,_33190,_33192),_34716,_34722),cuenta_camion(_33188),_33188-_33230,[_33230 is _33188+1]).

% LPS:  if(updates(botar(_34986,_34988,_34990),in(to(_35026,_35028),por_pagar(_35026))),(validado(_34986,_34988,_34990),_35028 is _35026+_34988)).
% Into: updated(happens(botar(_34986,_34988,_34990),_36644,_36650),por_pagar(_35026),_35026-_35028,[holds(validado(_34986,_34988,_34990),_36644),_35028 is _35026+_34988]).

% LPS:  if(initiates(validar(_36978,_36980,_36982),validado(_36978,_36980,_36982)),cuenta_camion(_36978)).
% Into: initiated(happens(validar(_36978,_36980,_36982),_38302,_38308),validado(_36978,_36980,_36982),[holds(cuenta_camion(_36978),_38302)]).

% LPS:  initially((especificacion(vacia),cuenta_camion(1),por_pagar(0))).
% Into: initial_state([especificacion(vacia),cuenta_camion(1),por_pagar(0)]).

% LPS:  observe(from(validar(_39438,10,alli),to(1,2))).
% Into: observe([validar(_39438,10,alli)],2).

% LPS:  observe(from(validar(_40608,10,alli),to(2,3))).
% Into: observe([validar(_40608,10,alli)],3).

% LPS:  observe(from(validar(_41778,10,alli),to(3,4))).
% Into: observe([validar(_41778,10,alli)],4).

% LPS:  observe(from(validar(_42948,10,alli),to(4,5))).
% Into: observe([validar(_42948,10,alli)],5).

% LPS:  observe(from(validar(_44118,10,alli),to(6,7))).
% Into: observe([validar(_44118,10,alli)],7).

% LPS:  observe(from(validar(_45288,10,alli),to(7,8))).
% Into: observe([validar(_45288,10,alli)],8).
% /pack/logicmoo_ec/test/lps_user_examples/contrato116.pl:59
% pop_lps_dialect('$BLOB'("<stream>(0x562ef3ca4f00)"),  (/.../(lps_user_examples, 'contrato116.pl')-> /.../(lps_user_examples, 'contrato116.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/contrato116.pl':_54024).


initiated(happens(validar(A, B, C), D, _), validado(A, B, C), [holds(cuenta_camion(A), D)]).

fluents([obra_paralizada, especificacion(_), cuenta_camion(A), por_pagar(_), validado(A, _, _)]).

capacidad(_, 10).

reactive_rule([holds(not(obra_paralizada), A), holds(not(fecha_tope(A)), A)], [happens(excavar(_, _, _), A, _)]).
reactive_rule([happens(dragar(A, B, aqui), _, C)], [happens(botar(A, B, alli), C, _)]).

initial_state([especificacion(vacia), cuenta_camion(1), por_pagar(0)]).

l_events(happens(excavar(A, B, C), D, E), [holds(especificacion(F), D), holds(cuenta_camion(A), D), conforme_a(F, B, C), capacidad(A, B), happens(dragar(A, B, C), D, E)]).

:- dynamic actions/1.
:- multifile actions/1.

actions([dragar(A, B, _), botar(A, B, _), pagar(B), paralizar(_), modificar(_)]).

conforme_a(_, _, _).

events([validar(_, _, _)]).

fecha_tope(8).

updated(happens(dragar(A, _, _), _, _), cuenta_camion(A), A-B, [B is A+1]).
updated(happens(botar(A, B, C), D, _), por_pagar(E), E-F, [holds(validado(A, B, C), D), F is E+B]).

maxTime(10).

observe([validar(_, 10, alli)], 2).
observe([validar(_, 10, alli)], 3).
observe([validar(_, 10, alli)], 4).
observe([validar(_, 10, alli)], 5).
observe([validar(_, 10, alli)], 7).
observe([validar(_, 10, alli)], 8).
% dB(/.../(lps_user_examples, 'contrato116.pl'), lps_visualization(_157238{groups:[_151292{content:"Events", id:"event", order:1}, _151366{content:"cuenta_camion(A)", id:"cuenta_camion/1", order:3, subgroupStack:"false"}, _151444{content:"especificacion(A)", id:"especificacion/1", order:3, subgroupStack:"false"}, _151522{content:"por_pagar(A)", id:"por_pagar/1", order:3, subgroupStack:"false"}, _151600{content:"validado(A,B,C)", id:"validado/3", order:3, subgroupStack:"false"}, _151666{content:"Actions", id:"action", order:4}], items:[_151788{content:"1", end:2, group:"cuenta_camion/1", id:0, start:1, subgroup:"1", title:"Fluent cuenta_camion(1) initiated at 1<br/>and terminated at transition to 2"}, _151914{content:"2", end:3, group:"cuenta_camion/1", id:1, start:2, subgroup:"2", title:"Fluent cuenta_camion(2) initiated at 2<br/>and terminated at transition to 3"}, _152040{content:"3", end:4, group:"cuenta_camion/1", id:2, start:3, subgroup:"3", title:"Fluent cuenta_camion(3) initiated at 3<br/>and terminated at transition to 4"}, _152166{content:"4", end:5, group:"cuenta_camion/1", id:3, start:4, subgroup:"4", title:"Fluent cuenta_camion(4) initiated at 4<br/>and terminated at transition to 5"}, _152292{content:"5", end:6, group:"cuenta_camion/1", id:4, start:5, subgroup:"5", title:"Fluent cuenta_camion(5) initiated at 5<br/>and terminated at transition to 6"}, _152418{content:"6", end:7, group:"cuenta_camion/1", id:5, start:6, subgroup:"6", title:"Fluent cuenta_camion(6) initiated at 6<br/>and terminated at transition to 7"}, _152544{content:"7", end:8, group:"cuenta_camion/1", id:6, start:7, subgroup:"7", title:"Fluent cuenta_camion(7) initiated at 7<br/>and terminated at transition to 8"}, _152670{content:"8", end:10, group:"cuenta_camion/1", id:7, start:8, subgroup:"8", title:"Fluent cuenta_camion(8) initiated at 8<br/>and terminated at transition to 10"}, _152796{content:"9", end:11, group:"cuenta_camion/1", id:8, start:10, subgroup:"9", title:"Fluent cuenta_camion(9) initiated at 10<br/>and terminated at transition to 11"}, _152922{content:"vacia", end:11, group:"especificacion/1", id:9, start:1, subgroup:"vacia", title:"Fluent especificacion(vacia) initiated at 1<br/>and terminated at transition to 11"}, _153048{content:"0", end:3, group:"por_pagar/1", id:10, start:1, subgroup:"0", title:"Fluent por_pagar(0) initiated at 1<br/>and terminated at transition to 3"}, _153174{content:"10", end:4, group:"por_pagar/1", id:11, start:3, subgroup:"10", title:"Fluent por_pagar(10) initiated at 3<br/>and terminated at transition to 4"}, _153300{content:"20", end:5, group:"por_pagar/1", id:12, start:4, subgroup:"20", title:"Fluent por_pagar(20) initiated at 4<br/>and terminated at transition to 5"}, _153426{content:"30", end:6, group:"por_pagar/1", id:13, start:5, subgroup:"30", title:"Fluent por_pagar(30) initiated at 5<br/>and terminated at transition to 6"}, _153552{content:"40", end:8, group:"por_pagar/1", id:14, start:6, subgroup:"40", title:"Fluent por_pagar(40) initiated at 6<br/>and terminated at transition to 8"}, _153678{content:"50", end:9, group:"por_pagar/1", id:15, start:8, subgroup:"50", title:"Fluent por_pagar(50) initiated at 8<br/>and terminated at transition to 9"}, _153804{content:"60", end:11, group:"por_pagar/1", id:16, start:9, subgroup:"60", title:"Fluent por_pagar(60) initiated at 9<br/>and terminated at transition to 11"}, _153930{content:"1,10,alli", end:11, group:"validado/3", id:17, start:2, subgroup:"1", title:"Fluent validado(1,10,alli) initiated at 2<br/>and terminated at transition to 11"}, _154056{content:"2,10,alli", end:11, group:"validado/3", id:18, start:3, subgroup:"2", title:"Fluent validado(2,10,alli) initiated at 3<br/>and terminated at transition to 11"}, _154182{content:"3,10,alli", end:11, group:"validado/3", id:19, start:4, subgroup:"3", title:"Fluent validado(3,10,alli) initiated at 4<br/>and terminated at transition to 11"}, _154308{content:"4,10,alli", end:11, group:"validado/3", id:20, start:5, subgroup:"4", title:"Fluent validado(4,10,alli) initiated at 5<br/>and terminated at transition to 11"}, _154434{content:"6,10,alli", end:11, group:"validado/3", id:21, start:7, subgroup:"6", title:"Fluent validado(6,10,alli) initiated at 7<br/>and terminated at transition to 11"}, _154560{content:"7,10,alli", end:11, group:"validado/3", id:22, start:8, subgroup:"7", title:"Fluent validado(7,10,alli) initiated at 8<br/>and terminated at transition to 11"}, _154686{content:"validar(A,10,alli)", group:"event", id:23, start:2, style:"color:#E19735", title:"happens(validar(A,10,alli),1,2)", type:"point"}, _154812{content:"dragar(1,10,B)", group:"action", id:24, start:2, style:"color:green", title:"happens(dragar(1,10,B),1,2)", type:"point"}, _154938{content:"validar(A,10,alli)", group:"event", id:25, start:3, style:"color:#E19735", title:"happens(validar(A,10,alli),2,3)", type:"point"}, _155064{content:"dragar(2,10,B)", group:"action", id:26, start:3, style:"color:green", title:"happens(dragar(2,10,B),2,3)", type:"point"}, _155190{content:"botar(1,10,alli)", group:"action", id:27, start:3, style:"color:green", title:"happens(botar(1,10,alli),2,3)", type:"point"}, _155316{content:"validar(A,10,alli)", group:"event", id:28, start:4, style:"color:#E19735", title:"happens(validar(A,10,alli),3,4)", type:"point"}, _155442{content:"botar(2,10,alli)", group:"action", id:29, start:4, style:"color:green", title:"happens(botar(2,10,alli),3,4)", type:"point"}, _155568{content:"dragar(3,10,B)", group:"action", id:30, start:4, style:"color:green", title:"happens(dragar(3,10,B),3,4)", type:"point"}, _155694{content:"validar(A,10,alli)", group:"event", id:31, start:5, style:"color:#E19735", title:"happens(validar(A,10,alli),4,5)", type:"point"}, _155820{content:"dragar(4,10,B)", group:"action", id:32, start:5, style:"color:green", title:"happens(dragar(4,10,B),4,5)", type:"point"}, _155946{content:"botar(3,10,alli)", group:"action", id:33, start:5, style:"color:green", title:"happens(botar(3,10,alli),4,5)", type:"point"}, _156072{content:"botar(4,10,alli)", group:"action", id:34, start:6, style:"color:green", title:"happens(botar(4,10,alli),5,6)", type:"point"}, _156198{content:"dragar(5,10,A)", group:"action", id:35, start:6, style:"color:green", title:"happens(dragar(5,10,A),5,6)", type:"point"}, _156324{content:"validar(A,10,alli)", group:"event", id:36, start:7, style:"color:#E19735", title:"happens(validar(A,10,alli),6,7)", type:"point"}, _156450{content:"dragar(6,10,B)", group:"action", id:37, start:7, style:"color:green", title:"happens(dragar(6,10,B),6,7)", type:"point"}, _156576{content:"botar(5,10,alli)", group:"action", id:38, start:7, style:"color:green", title:"happens(botar(5,10,alli),6,7)", type:"point"}, _156702{content:"validar(A,10,alli)", group:"event", id:39, start:8, style:"color:#E19735", title:"happens(validar(A,10,alli),7,8)", type:"point"}, _156828{content:"botar(6,10,alli)", group:"action", id:40, start:8, style:"color:green", title:"happens(botar(6,10,alli),7,8)", type:"point"}, _156954{content:"dragar(7,10,B)", group:"action", id:41, start:8, style:"color:green", title:"happens(dragar(7,10,B),7,8)", type:"point"}, _157080{content:"botar(7,10,alli)", group:"action", id:42, start:9, style:"color:green", title:"happens(botar(7,10,alli),8,9)", type:"point"}, _157206{content:"dragar(8,10,A)", group:"action", id:43, start:10, style:"color:green", title:"happens(dragar(8,10,A),9,10)", type:"point"}]}, [])).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'conveyor belt(1).pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'conveyor belt(1).pl')).
% /pack/logicmoo_ec/test/lps_user_examples/conveyor belt(1).pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'conveyor belt(1).pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'conveyor belt(1).pl'), lps= /.../(lps_user_examples, 'conveyor belt(1).pl'), using= /.../(lps_user_examples, 'conveyor belt(1).pl')].
% continue_lps_dialect.
% ops.

% LPS:  fluents((normalOperation,started,contents/2,empty/1,location/2)).
% Into: fluents([normalOperation,started,contents(_66506,_66508),empty(_66518),location(_66528,_66530)]).

% LPS:  initially((normalOperation,contents(bottle,0),contents(container,0),contents(tank1,100),contents(tank2,100),location(bottle,2),location(tank1,2),location(tank2,5),location(container,7))).
% Into: initial_state([normalOperation,contents(bottle,0),contents(container,0),contents(tank1,100),contents(tank2,100),location(bottle,2),location(tank1,2),location(tank2,5),location(container,7)]).

% LPS:  actions((pour/3,turnConveyor/1,waitCycle)).
% Into: actions([pour(_69440,_69442,_69444),turnConveyor(_69454),waitCycle]).

% LPS:  from(wait(0),to(_69398,_69398)).
% Into: l_events(happens(wait(0),_69398,_69398),[]).

% LPS:  if(from(wait(_70472),to(_70508,_70510)),(_70472>0,from(waitCycle,to(_70508,_70686)),_70828 is _70472-1,from(wait(_70828),to(_70686,_70510)))).
% Into: l_events(happens(wait(_70472),_70508,_70510),[_70472>0,happens(waitCycle,_70508,_70686),_70828 is _70472-1,happens(wait(_70828),_70686,_70510)]).

% LPS:  false((pour(_72968,_72970,_72972),turnConveyor(_73012))).
% Into: d_pre([happens(pour(_72968,_72970,_72972),_74056,_74062),happens(turnConveyor(_73012),_74056,_74062)]).

% LPS:  false((pour(_74154,_74156,_74158),location(_74154,_74214),location(_74156,_74270),_74214\=_74270)).
% Into: d_pre([happens(pour(_74154,_74156,_74158),_75516,_75522),holds(location(_74154,_74214),_75516),holds(location(_74156,_74270),_75516),_74214\=_74270]).

% LPS:  if(from(makeLocation(bottle,_75942),_75964),(at(location(bottle,_76046),_75964),at(location(_75942,_76150),_75964),_76282 is _76150-_76046,from(moveConveyor(_76282),_75964))).
% Into: l_events(happens(makeLocation(bottle,_75942),_75964,_77642),[holds(location(bottle,_76046),_75964),holds(location(_75942,_76150),_75964),_76282 is _76150-_76046,happens(moveConveyor(_76282),_75964,_77732)]).

% LPS:  from(moveConveyor(0),to(_78248,_78248)).
% Into: l_events(happens(moveConveyor(0),_78248,_78248),[]).

% LPS:  if(from(moveConveyor(_79322),to(_79358,_79360)),(_79322>0,from(turnConveyor(clockwise),to(_79358,_79560)),_79702 is _79322-1,from(moveConveyor(_79702),to(_79560,_79360)))).
% Into: l_events(happens(moveConveyor(_79322),_79358,_79360),[_79322>0,happens(turnConveyor(clockwise),_79358,_79560),_79702 is _79322-1,happens(moveConveyor(_79702),_79560,_79360)]).

% LPS:  if(moveConveyor(_81770),(_81770<0,turnConveyor(counterClockwise),_81958 is _81770+1,moveConveyor(_81958))).
% Into: l_events(happens(moveConveyor(_81770),_83200,_83206),[_81770<0,happens(turnConveyor(counterClockwise),_83200,_83482),_81958 is _81770+1,happens(moveConveyor(_81958),_83482,_83206)]).

% LPS:  if(updates(turnConveyor(counterClockwise),in(to(_83790,_83792),location(bottle,_83790))),_83792 is _83790-1).
% Into: updated(happens(turnConveyor(counterClockwise),_85266,_85272),location(bottle,_83790),_83790-_83792,[_83792 is _83790-1]).

% LPS:  if(updates(turnConveyor(clockwise),in(to(_85544,_85546),location(bottle,_85544))),_85546 is _85544+1).
% Into: updated(happens(turnConveyor(clockwise),_87020,_87026),location(bottle,_85544),_85544-_85546,[_85546 is _85544+1]).

% LPS:  if(empty(_87262),contents(_87262,0)).
% Into: l_int(holds(empty(_87262),_88346),[holds(contents(_87262,0),_88346)]).

% LPS:  if(updates(pour(_88392,_88394,_88396),in(to(_88432,_88434),contents(_88394,_88432))),_88434 is _88432+_88396).
% Into: updated(happens(pour(_88392,_88394,_88396),_89952,_89958),contents(_88394,_88432),_88432-_88434,[_88434 is _88432+_88396]).

% LPS:  if(updates(pour(_89848,_89850,_89852),in(to(_89888,_89890),contents(_89848,_89888))),_89890 is _89888-_89852).
% Into: updated(happens(pour(_89848,_89850,_89852),_91408,_91414),contents(_89848,_89888),_89888-_89890,[_89890 is _89888-_89852]).

% LPS:  then(if((at(normalOperation,_91342),at(empty(bottle),_91342),at(not(started),_91342))),(initiate(from(started,_91342)),from(makeLocation(bottle,tank1),to(_91342,_91830)),from(pour(tank1,bottle,5),to(_91830,_91998)),from(wait(3),to(_91998,_92134)),from(makeLocation(bottle,tank2),to(_92134,_92286)),from(pour(tank2,bottle,5),to(_92286,_92454)),from(wait(3),to(_92454,_92590)),from(makeLocation(bottle,container),to(_92590,_92742)),from(pour(bottle,container,10),to(_92742,_92910)),from(wait(3),to(_92910,_93046)),from(makeLocation(bottle,tank1),to(_93046,_93198)),terminate(from(started,_93198)))).
% Into: reactive_rule([holds(normalOperation,_91342),holds(empty(bottle),_91342),holds(not(started),_91342)],[happens(initiate(started),_91342,_95518),happens(makeLocation(bottle,tank1),_91342,_91830),happens(pour(tank1,bottle,5),_91830,_91998),happens(wait(3),_91998,_92134),happens(makeLocation(bottle,tank2),_92134,_92286),happens(pour(tank2,bottle,5),_92286,_92454),happens(wait(3),_92454,_92590),happens(makeLocation(bottle,container),_92590,_92742),happens(pour(bottle,container,10),_92742,_92910),happens(wait(3),_92910,_93046),happens(makeLocation(bottle,tank1),_93046,_93198),happens(terminate(started),_93198,_95482)]).
% /pack/logicmoo_ec/test/lps_user_examples/conveyor belt(1).pl:107
% pop_lps_dialect('$BLOB'("<stream>(0x562ef4162e00)"),  (/.../(lps_user_examples, 'conveyor belt(1).pl')-> /.../(lps_user_examples, 'conveyor belt(1).pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/conveyor belt(1).pl':_30064).


d_pre([happens(pour(_, _, _), A, B), happens(turnConveyor(_), A, B)]).
d_pre([happens(pour(A, B, _), C, _), holds(location(A, D), C), holds(location(B, E), C), D\=E]).

d(location(bottle, Pos), [type:rectangle, fillColor:yellow, from:[X1, 60], to:[X2, 100], strokeColor:blue]) :-
    X1 is 100+Pos*30,
    X2 is 110+Pos*30.
d(timeless, [[type:line, strokeWidth:2, strokeColor:black, from:[100, 60], to:[400, 60]], [type:circle, strokeWidth:2, strokeColor:black, center:[100, 40], radius:20], [type:circle, strokeWidth:2, strokeColor:black, center:[400, 40], radius:20], [type:rectangle, fillColor:white, from:[130, 120], to:[190, 150], strokeColor:blue], [type:rectangle, fillColor:white, from:[210, 120], to:[270, 150], strokeColor:blue], [type:line, strokeWidth:2, strokeColor:black, from:[100, 20], to:[400, 20]]]).

fluents([normalOperation, started, contents(_, _), empty(_), location(_, _)]).

l_int(holds(empty(A), B), [holds(contents(A, 0), B)]).

reactive_rule([holds(normalOperation, A), holds(empty(bottle), A), holds(not(started), A)], [happens(initiate(started), A, _), happens(makeLocation(bottle, tank1), A, B), happens(pour(tank1, bottle, 5), B, C), happens(wait(3), C, D), happens(makeLocation(bottle, tank2), D, E), happens(pour(tank2, bottle, 5), E, F), happens(wait(3), F, G), happens(makeLocation(bottle, container), G, H), happens(pour(bottle, container, 10), H, I), happens(wait(3), I, J), happens(makeLocation(bottle, tank1), J, K), happens(terminate(started), K, _)]).

initial_state([normalOperation, contents(bottle, 0), contents(container, 0), contents(tank1, 100), contents(tank2, 100), location(bottle, 2), location(tank1, 2), location(tank2, 5), location(container, 7)]).

l_events(happens(wait(0), A, A), []).
l_events(happens(wait(A), B, C), [A>0, happens(waitCycle, B, D), E is A-1, happens(wait(E), D, C)]).
l_events(happens(makeLocation(bottle, A), B, _), [holds(location(bottle, C), B), holds(location(A, D), B), E is D-C, happens(moveConveyor(E), B, _)]).
l_events(happens(moveConveyor(0), A, A), []).
l_events(happens(moveConveyor(A), B, C), [A>0, happens(turnConveyor(clockwise), B, D), E is A-1, happens(moveConveyor(E), D, C)]).
l_events(happens(moveConveyor(A), B, C), [A<0, happens(turnConveyor(counterClockwise), B, D), E is A+1, happens(moveConveyor(E), D, C)]).

:- dynamic actions/1.
:- multifile actions/1.

actions([pour(_, _, _), turnConveyor(_), waitCycle]).

updated(happens(turnConveyor(counterClockwise), _, _), location(bottle, A), A-B, [B is A-1]).
updated(happens(turnConveyor(clockwise), _, _), location(bottle, A), A-B, [B is A+1]).
updated(happens(pour(_, A, B), _, _), contents(A, C), C-D, [D is C+B]).
updated(happens(pour(A, _, B), _, _), contents(A, C), C-D, [D is C-B]).

maxTime(30).
% dB(/.../(lps_user_examples, 'conveyor belt(1).pl'), lps_visualization(_256476{groups:[_240304{content:"contents(A,B)", id:"contents/2", order:3, subgroupStack:"false"}, _240382{content:"location(A,B)", id:"location/2", order:3, subgroupStack:"false"}, _240460{content:"normalOperation", id:"normalOperation/0", order:3, subgroupStack:"false"}, _240538{content:"started", id:"started/0", order:3, subgroupStack:"false"}, _240604{content:"Actions", id:"action", order:4}], items:[_240714{content:"normalOperation", end:31, group:"normalOperation/0", id:0, start:1, title:"Fluent normalOperation initiated at 1<br/>and terminated at transition to 31"}, _240824{content:"started", end:31, group:"started/0", id:1, start:2, title:"Fluent started initiated at 2<br/>and terminated at transition to 31"}, _240946{content:"bottle,0", end:2, group:"contents/2", id:2, start:1, subgroup:"bottle", title:"Fluent contents(bottle,0) initiated at 1<br/>and terminated at transition to 2"}, ...(_268358)]}, _267376{cycles:[[_266964{create:[_266290{from:[100, 60], id:"timeless", strokeColor:"black", strokeWidth:2, to:[400, 60], type:"line"}, _266412{center:[100, 40], id:"timeless", radius:20, strokeColor:"black", strokeWidth:2, type:"circle"}, _266534{center:[400, 40], id:"timeless", radius:20, strokeColor:"black", strokeWidth:2, type:"circle"}, _266668{fillColor:"white", from:[130, 120], id:"timeless", strokeColor:"blue", to:[190, 150], type:"rectangle"}, _266802{fillColor:"white", from:[210, 120], id:"timeless", strokeColor:"blue", to:[270, 150], type:"rectangle"}, _266936{from:[100, 20], id:"timeless", strokeColor:"black", strokeWidth:2, to:[400, 20], type:"line"}]}], [_267128{create:_267100{fillColor:"yellow", from:[160, 60], id:"location(bottle,2)", strokeColor:"blue", to:[170, 100], type:"rectangle"}}], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [_267338{kill:"location(bottle,2)"}, _267368{kill:"timeless"}]]})).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'ConveyorBelt2.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'ConveyorBelt2.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/ConveyorBelt2.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'ConveyorBelt2.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'ConveyorBelt2.pl'), lps= /.../(lps_user_examples, 'ConveyorBelt2.pl'), using= /.../(lps_user_examples, 'ConveyorBelt2.pl')].
% continue_lps_dialect.
% ops.

% LPS:  fluents((contents(_68840,_68842),empty/1,location(_68960,_68962))).
% Into: fluents([contents(_68840,_68842),empty(_70196),location(_68960,_68962)]).

% LPS:  initially((normalOperation,contents(bottle,0),contents(container,0),contents(tank1,200),contents(tank2,200),location(bottle,0),location(tank1,25),location(tank2,65),location(container,100))).
% Into: initial_state([normalOperation,contents(bottle,0),contents(container,0),contents(tank1,200),contents(tank2,200),location(bottle,0),location(tank1,25),location(tank2,65),location(container,100)]).

% LPS:  actions((pourChunk(_71938,_71940),turnConveyor(_71980))).
% Into: actions([pourChunk(_71938,_71940),turnConveyor(_71980)]).

% LPS:  if(from(makeLocation(bottle,_73150),to(_73186,_73188)),(at(location(bottle,_73302),_73186),at(location(_73150,_73406),_73186),_73538 is _73406-_73302,from(moveBottle(_73538,_73406),to(_73186,_73188)))).
% Into: l_events(happens(makeLocation(bottle,_73150),_73186,_73188),[holds(location(bottle,_73302),_73186),holds(location(_73150,_73406),_73186),_73538 is _73406-_73302,happens(moveBottle(_73538,_73406),_73186,_73188)]).

% LPS:  if(from(moveBottle(_75992,_75994),to(_76030,_76030)),(_75992>=0,at(location(bottle,_76210),_76030),_76210>=_75994)).
% Into: l_events(happens(moveBottle(_75992,_75994),_76030,_76030),[_75992>=0,holds(location(bottle,_76210),_76030),_76210>=_75994]).

% LPS:  if(from(moveBottle(_78136,_78138),to(_78174,_78174)),(_78136<0,at(location(bottle,_78354),_78174),_78354=<_78138)).
% Into: l_events(happens(moveBottle(_78136,_78138),_78174,_78174),[_78136<0,holds(location(bottle,_78354),_78174),_78354=<_78138]).

% LPS:  if(from(moveBottle(_80280,_80282),to(_80318,_80320)),(_80280>0,at(location(bottle,_80498),_80318),_80498<_80282,from(turnConveyor(clockwise),to(_80318,_80688)),from(moveBottle(_80280,_80282),to(_80688,_80320)))).
% Into: l_events(happens(moveBottle(_80280,_80282),_80318,_80320),[_80280>0,holds(location(bottle,_80498),_80318),_80498<_80282,happens(turnConveyor(clockwise),_80318,_80688),happens(moveBottle(_80280,_80282),_80688,_80320)]).

% LPS:  if(from(moveBottle(_82878,_82880),to(_82916,_82918)),(_82878<0,at(location(bottle,_83096),_82916),_83096>_82880,from(turnConveyor(counterClockwise),to(_82916,_83286)),from(moveBottle(_82878,_82880),to(_83286,_82918)))).
% Into: l_events(happens(moveBottle(_82878,_82880),_82916,_82918),[_82878<0,holds(location(bottle,_83096),_82916),_83096>_82880,happens(turnConveyor(counterClockwise),_82916,_83286),happens(moveBottle(_82878,_82880),_83286,_82918)]).

% LPS:  if(updates(turnConveyor(counterClockwise),in(to(_85498,_85500),location(bottle,_85498))),(conveyorSpeed(_85686),_85500 is _85498-_85686)).
% Into: updated(happens(turnConveyor(counterClockwise),_87068,_87074),location(bottle,_85498),_85498-_85500,[conveyorSpeed(_85686),_85500 is _85498-_85686]).

% LPS:  if(updates(turnConveyor(clockwise),in(to(_87546,_87548),location(bottle,_87546))),(conveyorSpeed(_87734),_87548 is _87546+_87734)).
% Into: updated(happens(turnConveyor(clockwise),_89116,_89122),location(bottle,_87546),_87546-_87548,[conveyorSpeed(_87734),_87548 is _87546+_87734]).

% LPS:  if(empty(_90922),contents(_90922,0)).
% Into: l_int(holds(empty(_90922),_92006),[holds(contents(_90922,0),_92006)]).

% LPS:  if(updates(pourChunk(_92038,_92040),in(to(_92076,_92078),contents(_92040,_92076))),(valveRate(_92264),_92078 is _92076+_92264)).
% Into: updated(happens(pourChunk(_92038,_92040),_93662,_93668),contents(_92040,_92076),_92076-_92078,[valveRate(_92264),_92078 is _92076+_92264]).

% LPS:  if(updates(pourChunk(_93546,_93548),in(to(_93584,_93586),contents(_93546,_93584))),(valveRate(_93772),_93586 is _93584-_93772)).
% Into: updated(happens(pourChunk(_93546,_93548),_95170,_95176),contents(_93546,_93584),_93584-_93586,[valveRate(_93772),_93586 is _93584-_93772]).

% LPS:  if(from(pour(_96104,_96106,_96108),to(_96144,_96146)),(at(contents(_96104,_96260),_96144),_96392 is _96260-_96108,from(pourUntil(_96104,_96106,_96392),to(_96144,_96146)))).
% Into: l_events(happens(pour(_96104,_96106,_96108),_96144,_96146),[holds(contents(_96104,_96260),_96144),_96392 is _96260-_96108,happens(pourUntil(_96104,_96106,_96392),_96144,_96146)]).

% LPS:  if(from(pourUntil(_97864,_97866,_97868),to(_97904,_97904)),(at(contents(_97864,_98020),_97904),_98020=<_97868)).
% Into: l_events(happens(pourUntil(_97864,_97866,_97868),_97904,_97904),[holds(contents(_97864,_98020),_97904),_98020=<_97868]).

% LPS:  if(from(pourUntil(_99308,_99310,_99312),to(_99348,_99350)),(at(contents(_99308,_99464),_99348),_99464>_99312,from(pourChunk(_99308,_99310),to(_99348,_99670)),from(pourUntil(_99308,_99310,_99312),to(_99670,_99350)))).
% Into: l_events(happens(pourUntil(_99308,_99310,_99312),_99348,_99350),[holds(contents(_99308,_99464),_99348),_99464>_99312,happens(pourChunk(_99308,_99310),_99348,_99670),happens(pourUntil(_99308,_99310,_99312),_99670,_99350)]).

% LPS:  if(from(dump(_101208,_101210),to(_101246,_101248)),(at(contents(_101208,_101362),_101246),from(pour(_101208,_101210,_101362),to(_101246,_101248)))).
% Into: l_events(happens(dump(_101208,_101210),_101246,_101248),[holds(contents(_101208,_101362),_101246),happens(pour(_101208,_101210,_101362),_101246,_101248)]).

% LPS:  then(if((at(empty(bottle),_102780),at(location(bottle,0),_102780))),(from(makeLocation(bottle,tank1),to(_102780,_103060)),from(pour(tank1,bottle,50),to(_103060,_103228)),from(makeLocation(bottle,tank2),to(_103228,_103380)),from(pour(tank2,bottle,50),to(_103380,_103548)),from(makeLocation(bottle,container),to(_103548,_103700)),from(dump(bottle,container),to(_103700,_103852)),from(makeLocation(bottle,tank1),_103852))).
% Into: reactive_rule([holds(empty(bottle),_102780),holds(location(bottle,0),_102780)],[happens(makeLocation(bottle,tank1),_102780,_103060),happens(pour(tank1,bottle,50),_103060,_103228),happens(makeLocation(bottle,tank2),_103228,_103380),happens(pour(tank2,bottle,50),_103380,_103548),happens(makeLocation(bottle,container),_103548,_103700),happens(dump(bottle,container),_103700,_103852),happens(makeLocation(bottle,tank1),_103852,_105710)]).

% LPS:  if(at(locatedContents(_105588,_105590,_105592),_105614),(at(location(_105588,_105590),_105614),at(contents(_105588,_105592),_105614))).
% Into: l_int(holds(locatedContents(_105588,_105590,_105592),_105614),[holds(location(_105588,_105590),_105614),holds(contents(_105588,_105592),_105614)]).
% /pack/logicmoo_ec/test/lps_user_examples/ConveyorBelt2.pl:101
% pop_lps_dialect('$BLOB'("<stream>(0x562ef4163d00)"),  (/.../(lps_user_examples, 'ConveyorBelt2.pl')-> /.../(lps_user_examples, 'ConveyorBelt2.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/ConveyorBelt2.pl':_31056).


d(locatedContents(bottle, Location, Level), [Props, [type:rectangle, fillColor:blue, from:[X1, Y1], to:[X2, Y]]]) :-
    d_(location(bottle, Location), Props),
    member(from:[X1, Y1], Props),
    member(to:[X2, Y2], Props),
    Yrange is Y2-Y1,
    Y is round(Level/100*Yrange+Y1).
d(timeless, [[type:rectangle, from:[0, 0], to:[450, 300]], [type:line, strokeWidth:2, strokeColor:black, from:[Start, 100], to:[End, 100]], [type:circle, strokeWidth:2, strokeColor:black, center:[Start, 80], radius:20], [type:circle, strokeWidth:2, strokeColor:black, center:[End, 80], radius:20], [type:ellipse, fillColor:white, from:[Tank1Left, 200], to:[Tank1Right, 300], strokeColor:blue], [type:ellipse, fillColor:white, from:[Tank2Left, 200], to:[Tank2Right, 300], strokeColor:blue], [type:line, strokeWidth:2, strokeColor:black, from:[Start, 60], to:[End, 60]]]) :-
    locationToPixels(0, Start),
    locationToPixels(100, End),
    locationToPixels(25, Tank1Left),
    Tank1Right is Tank1Left+30,
    locationToPixels(65, Tank2Left),
    Tank2Right is Tank2Left+30.

locationToPixels(L, P) :-
    P is 25+L*4.

fluents([contents(_, _), empty(_), location(_, _)]).

l_int(holds(empty(A), B), [holds(contents(A, 0), B)]).
l_int(holds(locatedContents(A, B, C), D), [holds(location(A, B), D), holds(contents(A, C), D)]).

valveRate(10).

reactive_rule([holds(empty(bottle), A), holds(location(bottle, 0), A)], [happens(makeLocation(bottle, tank1), A, B), happens(pour(tank1, bottle, 50), B, C), happens(makeLocation(bottle, tank2), C, D), happens(pour(tank2, bottle, 50), D, E), happens(makeLocation(bottle, container), E, F), happens(dump(bottle, container), F, G), happens(makeLocation(bottle, tank1), G, _)]).

initial_state([normalOperation, contents(bottle, 0), contents(container, 0), contents(tank1, 200), contents(tank2, 200), location(bottle, 0), location(tank1, 25), location(tank2, 65), location(container, 100)]).

l_events(happens(makeLocation(bottle, A), B, C), [holds(location(bottle, D), B), holds(location(A, E), B), F is E-D, happens(moveBottle(F, E), B, C)]).
l_events(happens(moveBottle(A, B), C, C), [A>=0, holds(location(bottle, D), C), D>=B]).
l_events(happens(moveBottle(A, B), C, C), [A<0, holds(location(bottle, D), C), D=<B]).
l_events(happens(moveBottle(A, B), C, D), [A>0, holds(location(bottle, E), C), E<B, happens(turnConveyor(clockwise), C, F), happens(moveBottle(A, B), F, D)]).
l_events(happens(moveBottle(A, B), C, D), [A<0, holds(location(bottle, E), C), E>B, happens(turnConveyor(counterClockwise), C, F), happens(moveBottle(A, B), F, D)]).
l_events(happens(pour(A, B, C), D, E), [holds(contents(A, F), D), G is F-C, happens(pourUntil(A, B, G), D, E)]).
l_events(happens(pourUntil(A, _, B), C, C), [holds(contents(A, D), C), D=<B]).
l_events(happens(pourUntil(A, B, C), D, E), [holds(contents(A, F), D), F>C, happens(pourChunk(A, B), D, G), happens(pourUntil(A, B, C), G, E)]).
l_events(happens(dump(A, B), C, D), [holds(contents(A, E), C), happens(pour(A, B, E), C, D)]).

d_(location(bottle, Pos), [type:rectangle, fillColor:yellow, from:[X1, 100], to:[X2, 150], strokeColor:blue]) :-
    locationToPixels(Pos, X1),
    X2 is X1+10.

:- dynamic actions/1.
:- multifile actions/1.

actions([pourChunk(_, _), turnConveyor(_)]).

updated(happens(turnConveyor(counterClockwise), _, _), location(bottle, A), A-B, [conveyorSpeed(C), B is A-C]).
updated(happens(turnConveyor(clockwise), _, _), location(bottle, A), A-B, [conveyorSpeed(C), B is A+C]).
updated(happens(pourChunk(_, A), _, _), contents(A, B), B-C, [valveRate(D), C is B+D]).
updated(happens(pourChunk(A, _), _, _), contents(A, B), B-C, [valveRate(D), C is B-D]).

maxTime(60).

conveyorSpeed(5).
% dB(/.../(lps_user_examples, 'ConveyorBelt2.pl'), lps_visualization(_353378{groups:[_335630{content:"contents(A,B)", id:"contents/2", order:3, subgroupStack:"false"}, _335708{content:"location(A,B)", id:"location/2", order:3, subgroupStack:"false"}, _335786{content:"normalOperation", id:"normalOperation/0", order:3, subgroupStack:"false"}, _335852{content:"Actions", id:"action", order:4}], items:[_335962{content:"normalOperation", end:61, group:"normalOperation/0", id:0, start:1, title:"Fluent normalOperation initiated at 1<br/>and terminated at transition to 61"}, _336084{content:"bottle,0", end:7, group:"contents/2", id:1, start:1, subgroup:"bottle", title:"Fluent contents(bottle,0) initiated at 1<br/>and terminated at transition to 7"}, _336210{content:"bottle,0", end:61, group:"contents/2", id:2, start:41, subgroup:"bottle", title:"Fluent contents(bottle,0) initiated at 41<br/>and terminated at transition to 61"}, ...(_365964)]}, _364982{cycles:[[_364578{create:[_363778{from:[0, 0], id:"timeless", to:[450, 300], type:"rectangle"}, _363904{from:[25, 100], id:"timeless", strokeColor:"black", strokeWidth:2, to:[425, 100], type:"line"}, _364026{center:[25, 80], id:"timeless", radius:20, strokeColor:"black", strokeWidth:2, type:"circle"}, _364148{center:[425, 80], id:"timeless", radius:20, strokeColor:"black", strokeWidth:2, type:"circle"}, _364282{fillColor:"white", from:[125, 200], id:"timeless", strokeColor:"blue", to:[155, 300], type:"ellipse"}, _364416{fillColor:"white", from:[285, 200], id:"timeless", strokeColor:"blue", to:[315, 300], type:"ellipse"}, _364550{from:[25, 60], id:"timeless", strokeColor:"black", strokeWidth:2, to:[425, 60], type:"line"}]}], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [_364974{kill:"timeless"}]]})).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'conveyor belt.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'conveyor belt.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/conveyor belt.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'conveyor belt.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'conveyor belt.pl'), lps= /.../(lps_user_examples, 'conveyor belt.pl'), using= /.../(lps_user_examples, 'conveyor belt.pl')].
% continue_lps_dialect.
% ops.

% LPS:  fluents((normalOperation,started,contents/2,empty/1,location/2)).
% Into: fluents([normalOperation,started,contents(_72008,_72010),empty(_72020),location(_72030,_72032)]).

% LPS:  initially((normalOperation,contents(bottle,0),contents(container,0),contents(tank1,100),contents(tank2,100),location(bottle,2),location(tank1,2),location(tank2,5),location(container,7))).
% Into: initial_state([normalOperation,contents(bottle,0),contents(container,0),contents(tank1,100),contents(tank2,100),location(bottle,2),location(tank1,2),location(tank2,5),location(container,7)]).

% LPS:  actions((pour/3,turnConveyor/1,waitCycle)).
% Into: actions([pour(_74942,_74944,_74946),turnConveyor(_74956),waitCycle]).

% LPS:  from(wait(0),to(_74900,_74900)).
% Into: l_events(happens(wait(0),_74900,_74900),[]).

% LPS:  if(from(wait(_75974),to(_76010,_76012)),(_75974>0,from(waitCycle,to(_76010,_76188)),_76330 is _75974-1,from(wait(_76330),to(_76188,_76012)))).
% Into: l_events(happens(wait(_75974),_76010,_76012),[_75974>0,happens(waitCycle,_76010,_76188),_76330 is _75974-1,happens(wait(_76330),_76188,_76012)]).

% LPS:  false((pour(_78462,_78464,_78466),turnConveyor(_78506))).
% Into: d_pre([happens(pour(_78462,_78464,_78466),_79550,_79556),happens(turnConveyor(_78506),_79550,_79556)]).

% LPS:  false((pour(_79648,_79650,_79652),location(_79648,_79708),location(_79650,_79764),_79708\=_79764)).
% Into: d_pre([happens(pour(_79648,_79650,_79652),_81010,_81016),holds(location(_79648,_79708),_81010),holds(location(_79650,_79764),_81010),_79708\=_79764]).

% LPS:  if(from(makeLocation(bottle,_81436),_81458),(at(location(bottle,_81540),_81458),at(location(_81436,_81644),_81458),_81776 is _81644-_81540,from(moveConveyor(_81776),_81458))).
% Into: l_events(happens(makeLocation(bottle,_81436),_81458,_83136),[holds(location(bottle,_81540),_81458),holds(location(_81436,_81644),_81458),_81776 is _81644-_81540,happens(moveConveyor(_81776),_81458,_83226)]).

% LPS:  from(moveConveyor(0),to(_83742,_83742)).
% Into: l_events(happens(moveConveyor(0),_83742,_83742),[]).

% LPS:  if(from(moveConveyor(_84816),to(_84852,_84854)),(_84816>0,from(turnConveyor(clockwise),to(_84852,_85054)),_85196 is _84816-1,from(moveConveyor(_85196),to(_85054,_84854)))).
% Into: l_events(happens(moveConveyor(_84816),_84852,_84854),[_84816>0,happens(turnConveyor(clockwise),_84852,_85054),_85196 is _84816-1,happens(moveConveyor(_85196),_85054,_84854)]).

% LPS:  if(moveConveyor(_87264),(_87264<0,turnConveyor(counterClockwise),_87452 is _87264+1,moveConveyor(_87452))).
% Into: l_events(happens(moveConveyor(_87264),_88694,_88700),[_87264<0,happens(turnConveyor(counterClockwise),_88694,_88976),_87452 is _87264+1,happens(moveConveyor(_87452),_88976,_88700)]).

% LPS:  if(updates(turnConveyor(counterClockwise),in(to(_89284,_89286),location(bottle,_89284))),_89286 is _89284-1).
% Into: updated(happens(turnConveyor(counterClockwise),_90760,_90766),location(bottle,_89284),_89284-_89286,[_89286 is _89284-1]).

% LPS:  if(updates(turnConveyor(clockwise),in(to(_91038,_91040),location(bottle,_91038))),_91040 is _91038+1).
% Into: updated(happens(turnConveyor(clockwise),_92514,_92520),location(bottle,_91038),_91038-_91040,[_91040 is _91038+1]).

% LPS:  if(empty(_92756),contents(_92756,0)).
% Into: l_int(holds(empty(_92756),_93840),[holds(contents(_92756,0),_93840)]).

% LPS:  if(updates(pour(_93886,_93888,_93890),in(to(_93926,_93928),contents(_93888,_93926))),_93928 is _93926+_93890).
% Into: updated(happens(pour(_93886,_93888,_93890),_95446,_95452),contents(_93888,_93926),_93926-_93928,[_93928 is _93926+_93890]).

% LPS:  if(updates(pour(_95342,_95344,_95346),in(to(_95382,_95384),contents(_95342,_95382))),_95384 is _95382-_95346).
% Into: updated(happens(pour(_95342,_95344,_95346),_96902,_96908),contents(_95342,_95382),_95382-_95384,[_95384 is _95382-_95346]).

% LPS:  then(if((at(normalOperation,_96836),at(empty(bottle),_96836),at(not(started),_96836))),(initiate(from(started,_96836)),from(makeLocation(bottle,tank1),to(_96836,_97324)),from(pour(tank1,bottle,5),to(_97324,_97492)),from(wait(3),to(_97492,_97628)),from(makeLocation(bottle,tank2),to(_97628,_97780)),from(pour(tank2,bottle,5),to(_97780,_97948)),from(wait(3),to(_97948,_98084)),from(makeLocation(bottle,container),to(_98084,_98236)),from(pour(bottle,container,10),to(_98236,_98404)),from(wait(3),to(_98404,_98540)),from(makeLocation(bottle,tank1),to(_98540,_98692)),terminate(from(started,_98692)))).
% Into: reactive_rule([holds(normalOperation,_96836),holds(empty(bottle),_96836),holds(not(started),_96836)],[happens(initiate(started),_96836,_101012),happens(makeLocation(bottle,tank1),_96836,_97324),happens(pour(tank1,bottle,5),_97324,_97492),happens(wait(3),_97492,_97628),happens(makeLocation(bottle,tank2),_97628,_97780),happens(pour(tank2,bottle,5),_97780,_97948),happens(wait(3),_97948,_98084),happens(makeLocation(bottle,container),_98084,_98236),happens(pour(bottle,container,10),_98236,_98404),happens(wait(3),_98404,_98540),happens(makeLocation(bottle,tank1),_98540,_98692),happens(terminate(started),_98692,_100976)]).
% /pack/logicmoo_ec/test/lps_user_examples/conveyor belt.pl:107
% pop_lps_dialect('$BLOB'("<stream>(0x562ef3ca5b00)"),  (/.../(lps_user_examples, 'conveyor belt.pl')-> /.../(lps_user_examples, 'conveyor belt.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/conveyor belt.pl':_114640).


d_pre([happens(pour(_, _, _), A, B), happens(turnConveyor(_), A, B)]).
d_pre([happens(pour(A, B, _), C, _), holds(location(A, D), C), holds(location(B, E), C), D\=E]).

d(location(bottle, Pos), [type:rectangle, fillColor:yellow, from:[X1, 60], to:[X2, 100], strokeColor:blue]) :-
    X1 is 100+Pos*30,
    X2 is 110+Pos*30.
d(timeless, [[type:line, strokeWidth:2, strokeColor:black, from:[100, 60], to:[400, 60]], [type:circle, strokeWidth:2, strokeColor:black, center:[100, 40], radius:20], [type:circle, strokeWidth:2, strokeColor:black, center:[400, 40], radius:20], [type:rectangle, fillColor:white, from:[130, 120], to:[190, 150], strokeColor:blue], [type:rectangle, fillColor:white, from:[210, 120], to:[270, 150], strokeColor:blue], [type:line, strokeWidth:2, strokeColor:black, from:[100, 20], to:[400, 20]]]).

fluents([normalOperation, started, contents(_, _), empty(_), location(_, _)]).

l_int(holds(empty(A), B), [holds(contents(A, 0), B)]).

reactive_rule([holds(normalOperation, A), holds(empty(bottle), A), holds(not(started), A)], [happens(initiate(started), A, _), happens(makeLocation(bottle, tank1), A, B), happens(pour(tank1, bottle, 5), B, C), happens(wait(3), C, D), happens(makeLocation(bottle, tank2), D, E), happens(pour(tank2, bottle, 5), E, F), happens(wait(3), F, G), happens(makeLocation(bottle, container), G, H), happens(pour(bottle, container, 10), H, I), happens(wait(3), I, J), happens(makeLocation(bottle, tank1), J, K), happens(terminate(started), K, _)]).

initial_state([normalOperation, contents(bottle, 0), contents(container, 0), contents(tank1, 100), contents(tank2, 100), location(bottle, 2), location(tank1, 2), location(tank2, 5), location(container, 7)]).

l_events(happens(wait(0), A, A), []).
l_events(happens(wait(A), B, C), [A>0, happens(waitCycle, B, D), E is A-1, happens(wait(E), D, C)]).
l_events(happens(makeLocation(bottle, A), B, _), [holds(location(bottle, C), B), holds(location(A, D), B), E is D-C, happens(moveConveyor(E), B, _)]).
l_events(happens(moveConveyor(0), A, A), []).
l_events(happens(moveConveyor(A), B, C), [A>0, happens(turnConveyor(clockwise), B, D), E is A-1, happens(moveConveyor(E), D, C)]).
l_events(happens(moveConveyor(A), B, C), [A<0, happens(turnConveyor(counterClockwise), B, D), E is A+1, happens(moveConveyor(E), D, C)]).

:- dynamic actions/1.
:- multifile actions/1.

actions([pour(_, _, _), turnConveyor(_), waitCycle]).

updated(happens(turnConveyor(counterClockwise), _, _), location(bottle, A), A-B, [B is A-1]).
updated(happens(turnConveyor(clockwise), _, _), location(bottle, A), A-B, [B is A+1]).
updated(happens(pour(_, A, B), _, _), contents(A, C), C-D, [D is C+B]).
updated(happens(pour(A, _, B), _, _), contents(A, C), C-D, [D is C-B]).

maxTime(30).
% dB(/.../(lps_user_examples, 'conveyor belt.pl'), lps_visualization(_256464{groups:[_240292{content:"contents(A,B)", id:"contents/2", order:3, subgroupStack:"false"}, _240370{content:"location(A,B)", id:"location/2", order:3, subgroupStack:"false"}, _240448{content:"normalOperation", id:"normalOperation/0", order:3, subgroupStack:"false"}, _240526{content:"started", id:"started/0", order:3, subgroupStack:"false"}, _240592{content:"Actions", id:"action", order:4}], items:[_240702{content:"normalOperation", end:31, group:"normalOperation/0", id:0, start:1, title:"Fluent normalOperation initiated at 1<br/>and terminated at transition to 31"}, _240812{content:"started", end:31, group:"started/0", id:1, start:2, title:"Fluent started initiated at 2<br/>and terminated at transition to 31"}, _240934{content:"bottle,0", end:2, group:"contents/2", id:2, start:1, subgroup:"bottle", title:"Fluent contents(bottle,0) initiated at 1<br/>and terminated at transition to 2"}, ...(_268346)]}, _267364{cycles:[[_266952{create:[_266278{from:[100, 60], id:"timeless", strokeColor:"black", strokeWidth:2, to:[400, 60], type:"line"}, _266400{center:[100, 40], id:"timeless", radius:20, strokeColor:"black", strokeWidth:2, type:"circle"}, _266522{center:[400, 40], id:"timeless", radius:20, strokeColor:"black", strokeWidth:2, type:"circle"}, _266656{fillColor:"white", from:[130, 120], id:"timeless", strokeColor:"blue", to:[190, 150], type:"rectangle"}, _266790{fillColor:"white", from:[210, 120], id:"timeless", strokeColor:"blue", to:[270, 150], type:"rectangle"}, _266924{from:[100, 20], id:"timeless", strokeColor:"black", strokeWidth:2, to:[400, 20], type:"line"}]}], [_267116{create:_267088{fillColor:"yellow", from:[160, 60], id:"location(bottle,2)", strokeColor:"blue", to:[170, 100], type:"rectangle"}}], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [_267326{kill:"location(bottle,2)"}, _267356{kill:"timeless"}]]})).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'cooking.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'cooking.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/cooking.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'cooking.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'cooking.pl'), lps= /.../(lps_user_examples, 'cooking.pl'), using= /.../(lps_user_examples, 'cooking.pl')].
% continue_lps_dialect.
% ops.

% LPS:  fluents(location(_67890,_67892)).
% Into: fluents([location(_67890,_67892)]).

% LPS:  actions(setLocation(_69034,_69036)).
% Into: actions([setLocation(_69034,_69036)]).

% LPS:  if(updates(setLocation(_70094,_70096),in(to(_70132,_70096),location(_70094,_70132))),(_70096=point(_70350,_70352),number(_70350),number(_70352))).
% Into: updated(happens(setLocation(_70094,_70096),_71812,_71818),location(_70094,_70132),_70132-_70096,[_70096=point(_70350,_70352),number(_70350),number(_70352)]).

% LPS:  fluents(working(_72934,_72936)).
% Into: fluents([working(_72934,_72936)]).

% LPS:  actions((start(_74024),stop(_74064))).
% Into: actions([start(_74024),stop(_74064)]).

% LPS:  updates(start(_75148),in(to(_75184,true),working(_75148,_75184))).
% Into: updated(happens(start(_75148),_76488,_76494),working(_75148,_75184),_75184-true,[]).

% LPS:  updates(stop(_76370),in(to(_76416,false),working(_76370,_76416))).
% Into: updated(happens(stop(_76370),_77720,_77726),working(_76370,_76416),_76416-false,[]).

% LPS:  if(from(clone(_77646,_77648),to(_77684,_77686)),(_77646=..[_77800,_77820],objectName(_77894),_77648=..[_77800,_77894],findall(_78312,(holds(_78102,_77684),_78102=..[_78160,_77646|_78182],_78312=..[_78160,_77648|_78182]),_78426),from(initiateAll(_78426),to(_77684,_77686)))).
% Into: l_events(happens(clone(_77646,_77648),_77684,_77686),[_77646=..[_77800,_77820],objectName(_77894),_77648=..[_77800,_77894],holds(findall(_78312,[holds(_78102,_77684),_78102=..[_78160,_77646|_78182],_78312=..[_78160,_77648|_78182]],_78426),_77684),happens(initiateAll(_78426),_77684,_77686)]).

% LPS:  if(from(initiateAll([_82406|_82408]),to(_82456,_82458)),(initiate(from(_82406,_82456)),from(initiateAll(_82408),to(_82456,_82458)))).
% Into: l_events(happens(initiateAll([_82406|_82408]),_82456,_82458),[happens(initiate(_82406),_82456,_84020),happens(initiateAll(_82408),_82456,_82458)]).

% LPS:  if(from(initiateAll([]),to(_83942,_83944)),_83944=:=_83942+1).
% Into: l_events(happens(initiateAll([]),_83942,_83944),[_83944=:=_83942+1]).

% LPS:  if(from(clone(_85582,_85584,[_85568|_85570]),to(_85622,_85624)),(_85582>0,from(clone(_85584,_85568),to(_85622,_85624)),_85982 is _85582-1,from(clone(_85982,_85584,_85570),to(_85622,_85624)))).
% Into: l_events(happens(clone(_85582,_85584,[_85568|_85570]),_85622,_85624),[_85582>0,happens(clone(_85584,_85568),_85622,_85624),_85982 is _85582-1,happens(clone(_85982,_85584,_85570),_85622,_85624)]).

% LPS:  clone(0,_88166,[]).
% Into: l_events(happens(clone(0,_88166,[]),_89132,_89138),[]).

% LPS:  fluents((transports(_89296,_89298),conveyorEndpoint(_89352,_89354),conveyorSpeed(_89352,_89410))).
% Into: fluents([transports(_89296,_89298),conveyorEndpoint(_89352,_89354),conveyorSpeed(_89352,_89410)]).

% LPS:  actions(setConveyorSpeed(_90634,_90636)).
% Into: actions([setConveyorSpeed(_90634,_90636)]).

% LPS:  updates(setConveyorSpeed(_91694,_91696),in(to(_91732,_91696),conveyorSpeed(_91694,_91732))).
% Into: updated(happens(setConveyorSpeed(_91694,_91696),_93052,_93058),conveyorSpeed(_91694,_91732),_91732-_91696,[]).

% LPS:  if(from(placeOnConveyor(_92960,_92962,_92964),to(_93000,_93002)),(at(not(transports(_92962,_92960)),_93000),location(_92962,_93252),conveyorEndpoint(_92962,_93308),interpolate(_93252,_93308,_92964,_93396),update(in(to(_93440,_93396),from(location(_92960,_93440),to(_93000,_93002)))),initiate(from(transports(_92962,_92960),to(_93000,_93002))))).
% Into: l_events(happens(placeOnConveyor(_92960,_92962,_92964),_93000,_93002),[holds(not(transports(_92962,_92960)),_93000),holds(location(_92962,_93252),_95512),holds(conveyorEndpoint(_92962,_93308),_95576),interpolate(_93252,_93308,_92964,_93396),happens(update(_93440-_93396,location(_92960,_93440)),_93000,_93002),happens(initiate(transports(_92962,_92960)),_93000,_93002)]).

% LPS:  if(from(createConveyor(_95996,_95998,_96000,_96002),to(_96038,_96040)),(_96002=conveyor(_95996),objectName(_95996),initiate(from(location(_96002,_95998),to(_96038,_96040))),initiate(from(conveyorEndpoint(_96002,_96000),to(_96038,_96040))),initiate(from(conveyorSpeed(_96002,0),to(_96038,_96040))),initiate(from(working(_96002,false),to(_96038,_96040))))).
% Into: l_events(happens(createConveyor(_95996,_95998,_96000,_96002),_96038,_96040),[_96002=conveyor(_95996),objectName(_95996),happens(initiate(location(_96002,_95998)),_96038,_96040),happens(initiate(conveyorEndpoint(_96002,_96000)),_96038,_96040),happens(initiate(conveyorSpeed(_96002,0)),_96038,_96040),happens(initiate(working(_96002,false)),_96038,_96040)]).

% LPS:  if(at(conveyor(_98962,_98964,_98966,_98968,_98970),_98992),(at(conveyorEndpoint(_98962,_98966),_98992),at(location(_98962,_98964),_98992),at(working(_98962,_98968),_98992),at(conveyorSpeed(_98962,_98970),_98992))).
% Into: l_int(holds(conveyor(_98962,_98964,_98966,_98968,_98970),_98992),[holds(conveyorEndpoint(_98962,_98966),_98992),holds(location(_98962,_98964),_98992),holds(working(_98962,_98968),_98992),holds(conveyorSpeed(_98962,_98970),_98992)]).

% LPS:  then(if((at(conveyorSpeed(_105920,_105922),_105944),at(working(_105920,true),_105944),at(transports(_105920,_106130),_105944),at(conveyorEndpoint(_105920,_106234),_105944),at(location(_106130,_106338),_105944),at(location(_105920,_106442),_105944),notReachingConveyorEnds(_105922,_106442,_106234,_106338))),(newPosition(_106338,_106442,_106234,_105922,_106898),update(in(to(_106338,_106898),from(location(_106130,_106338),_105944))))).
% Into: reactive_rule([holds(conveyorSpeed(_105920,_105922),_105944),holds(working(_105920,true),_105944),holds(transports(_105920,_106130),_105944),holds(conveyorEndpoint(_105920,_106234),_105944),holds(location(_106130,_106338),_105944),holds(location(_105920,_106442),_105944),notReachingConveyorEnds(_105922,_106442,_106234,_106338)],[newPosition(_106338,_106442,_106234,_105922,_106898),happens(update(_106338-_106898,location(_106130,_106338)),_105944,_109198)]).

% LPS:  fluents(container(_112530,_112532)).
% Into: fluents([container(_112530,_112532)]).

% LPS:  if(from(createContainer(_113604,_113606,_113608),to(_113644,_113646)),(_113608=container(_113604),objectName(_113604),number(_113606),initiate(from(container(_113608,_113606),to(_113644,_113646))),initiate(from(location(_113608,point(0,0)),to(_113644,_113646))))).
% Into: l_events(happens(createContainer(_113604,_113606,_113608),_113644,_113646),[_113608=container(_113604),objectName(_113604),number(_113606),happens(initiate(container(_113608,_113606)),_113644,_113646),happens(initiate(location(_113608,point(0,0))),_113644,_113646)]).

% LPS:  if(at(container(_115664,_115666,_115668),_115690),(at(container(_115664,_115666),_115690),at(location(_115664,_115668),_115690))).
% Into: l_int(holds(container(_115664,_115666,_115668),_115690),[holds(container(_115664,_115666),_115690),holds(location(_115664,_115668),_115690)]).

% LPS:  fluents((heater(_20970,_20972,_20974,_20976),heatable(_20970,_20976),initialTemperature(_21100,_21102,_21104))).
% Into: fluents([heater(_20970,_20972,_20974,_20976),heatable(_20970,_20976),initialTemperature(_21100,_21102,_21104)]).

% LPS:  if(from(createHeater(_22358,_22360,_22362,_22364,_22366),to(_22402,_22404)),(_22366=heater(_22358),objectName(_22358),initiate(from(heater(_22366,_22360,_22362,_22364),to(_22402,_22404))),initiate(from(working(_22366,false),to(_22402,_22404))))).
% Into: l_events(happens(createHeater(_22358,_22360,_22362,_22364,_22366),_22402,_22404),[_22366=heater(_22358),objectName(_22358),happens(initiate(heater(_22366,_22360,_22362,_22364)),_22402,_22404),happens(initiate(working(_22366,false)),_22402,_22404)]).

% LPS:  then(if((at(heater(_24348,_24350,_24352,_24354),_24376),at(working(_24348,true),_24376),at(heatable(_24560,_24562),_24376),at(location(_24560,_24666),_24376),\+inside(_24666,_24350,_24352),at(location(_24560,_24874),_24376+1),inside(_24874,_24350,_24352))),initiate(from(initialTemperature(_24348,_24560,_24562),_24376+1))).
% Into: reactive_rule([holds(heater(_24348,_24350,_24352,_24354),_24376),holds(working(_24348,true),_24376),holds(heatable(_24560,_24562),_24376),holds(location(_24560,_24666),_24376),\+inside(_24666,_24350,_24352),holds(location(_24560,_24874),_24376+1),inside(_24874,_24350,_24352)],[happens(initiate(initialTemperature(_24348,_24560,_24562)),_24376+1,_27448)]).

% LPS:  then(if((at(heater(_26794,_26796,_26798,_26800),_26822),at(working(_26794,true),_26822),at(heatable(_27006,_27008),_26822),at(location(_27006,_27112),_26822),inside(_27112,_26796,_26798))),update(in(to(_27428,_26800),from(heatable(_27006,_27428),_26822)))).
% Into: reactive_rule([holds(heater(_26794,_26796,_26798,_26800),_26822),holds(working(_26794,true),_26822),holds(heatable(_27006,_27008),_26822),holds(location(_27006,_27112),_26822),inside(_27112,_26796,_26798)],[happens(update(_27428-_26800,heatable(_27006,_27428)),_26822,_29364)]).

% LPS:  then(if((at(heater(_28902,_28904,_28906,_28908),_28930),at(working(_28902,true),_28930),at(heatable(_29114,_29116),_28930),at(location(_29114,_29220),_28930),inside(_29220,_28904,_28906),at(location(_29114,_29396),_28930+1),\+inside(_29396,_28904,_28906),initialTemperature(_28902,_29114,_29668))),(terminate(from(initialTemperature(_28902,_29114,_29668),_28930+1)),update(in(to(_29116,_29668),from(heatable(_29114,_29116),_28930+1))))).
% Into: reactive_rule([holds(heater(_28902,_28904,_28906,_28908),_28930),holds(working(_28902,true),_28930),holds(heatable(_29114,_29116),_28930),holds(location(_29114,_29220),_28930),inside(_29220,_28904,_28906),holds(location(_29114,_29396),_28930+1),\+inside(_29396,_28904,_28906),holds(initialTemperature(_28902,_29114,_29668),_31770)],[happens(terminate(initialTemperature(_28902,_29114,_29668)),_28930+1,_32564),happens(update(_29116-_29668,heatable(_29114,_29116)),_28930+1,_32528)]).

% LPS:  fluents(cookable(_33888,_33890,_33892)).
% Into: fluents([cookable(_33888,_33890,_33892)]).

% LPS:  if(from(createCookable(_34978,_34980,_34982),to(_35018,_35020)),(_34982=cookable(_34978),objectName(_34978),initiate(from(location(_34982,point(0,0)),to(_35018,_35020))),initiate(from(heatable(_34982,_34980),to(_35018,_35020))),initiate(from(cookable(_34982,_34980,0),to(_35018,_35020))))).
% Into: l_events(happens(createCookable(_34978,_34980,_34982),_35018,_35020),[_34982=cookable(_34978),objectName(_34978),happens(initiate(location(_34982,point(0,0))),_35018,_35020),happens(initiate(heatable(_34982,_34980)),_35018,_35020),happens(initiate(cookable(_34982,_34980,0)),_35018,_35020)]).

% LPS:  then(if((at(cookable(_37194,_37196,_37198),_37220),at(heatable(_37194,_37302),_37220),_37302>_37196)),(_37698 is _37198+(_37302-_37196)*0.01,update(in(to(_37198,_37698),cookable(_37194,_37196,_37198))))).
% Into: reactive_rule([holds(cookable(_37194,_37196,_37198),_37220),holds(heatable(_37194,_37302),_37220),_37302>_37196],[_37698 is _37198+(_37302-_37196)*0.01,happens(update(_37198-_37698,cookable(_37194,_37196,_37198)),_39424,_39430)]).

% LPS:  if(at(cookable(_39230,_39232,_39234,_39236),_39258),(at(cookable(_39230,_39232,_39234),_39258),at(location(_39230,_39236),_39258))).
% Into: l_int(holds(cookable(_39230,_39232,_39234,_39236),_39258),[holds(cookable(_39230,_39232,_39234),_39258),holds(location(_39230,_39236),_39258)]).

% LPS:  fluents((pump(_42692,_42694,_42696),pumpFlow(_42692,_42752))).
% Into: fluents([pump(_42692,_42694,_42696),pumpFlow(_42692,_42752)]).

% LPS:  if(from(createPump(_43948,_43950,_43952,_43954),to(_43990,_43992)),(_43950\=_43952,_43950=container(_44170),_43952=container(_44258),_43954=pump(_43948),objectName(_43948),initiate(from(pump(_43954,_43950,_43952),to(_43990,_43992))),initiate(from(pumpFlow(_43954,0),to(_43990,_43992))),initiate(from(working(_43954,false),to(_43990,_43992))))).
% Into: l_events(happens(createPump(_43948,_43950,_43952,_43954),_43990,_43992),[_43950\=_43952,_43950=container(_44170),_43952=container(_44258),_43954=pump(_43948),objectName(_43948),happens(initiate(pump(_43954,_43950,_43952)),_43990,_43992),happens(initiate(pumpFlow(_43954,0)),_43990,_43992),happens(initiate(working(_43954,false)),_43990,_43992)]).

% LPS:  actions((setPumpFlow(_46500,_46502),switchPumpOutputTo(_46500,_46558),switchPumpInputTo(_46500,_46614),pumpIt(_46500,_46670))).
% Into: actions([setPumpFlow(_46500,_46502),switchPumpOutputTo(_46500,_46558),switchPumpInputTo(_46500,_46614),pumpIt(_46500,_46670)]).

% LPS:  if(updates(setPumpFlow(_47896,_47898),in(to(_47934,_47898),pumpFlow(_47896,_47934))),number(_47898)).
% Into: updated(happens(setPumpFlow(_47896,_47898),_49354,_49360),pumpFlow(_47896,_47934),_47934-_47898,[number(_47898)]).

% LPS:  if(updates(switchPumpOutputTo(_49230,_49232),in(to(_49268,_49232),pump(_49230,_49366,_49268))),_49232=container(_49488)).
% Into: updated(happens(switchPumpOutputTo(_49230,_49232),_50774,_50780),pump(_49230,_49366,_49268),_49268-_49232,[_49232=container(_49488)]).

% LPS:  if(updates(switchPumpInputTo(_50648,_50650),in(to(_50686,_50650),pump(_50648,_50686,_50786))),_50650=container(_50906)).
% Into: updated(happens(switchPumpInputTo(_50648,_50650),_52180,_52186),pump(_50648,_50686,_50786),_50686-_50650,[_50650=container(_50906)]).

% LPS:  then(if((at(pump(_52124,_52126,_52128),_52150),at(working(_52124,true),_52150),at(container(_52126,_52336),_52150))),(at(pumpFlow(_52124,_52528),_52150),_52336-_52528>=0,from(pumpIt(_52126,-_52528),_52150),from(pumpIt(_52128,_52528),_52150))).
% Into: reactive_rule([holds(pump(_52124,_52126,_52128),_52150),holds(working(_52124,true),_52150),holds(container(_52126,_52336),_52150)],[holds(pumpFlow(_52124,_52528),_52150),_52336-_52528>=0,happens(pumpIt(_52126,-_52528),_52150,_54792),happens(pumpIt(_52128,_52528),_52150,_54570)]).

% LPS:  if(updates(pumpIt(_54326,_54328),in(to(_54364,_54366),container(_54326,_54364))),_54366 is _54364+_54328).
% Into: updated(happens(pumpIt(_54326,_54328),_55868,_55874),container(_54326,_54364),_54364-_54366,[_54366 is _54364+_54328]).

% LPS:  fluents(dropper(_55878,_55880,_55882,_55884,_55886)).
% Into: fluents([dropper(_55878,_55880,_55882,_55884,_55886)]).

% LPS:  if(from(createDropper(_57028,_57030,_57032,_57034,_57036),to(_57072,_57074)),(_57036=dropper(_57028),objectName(_57028),number(_57030),_57034=conveyor(_57356),at(location(_57034,point(_57476,_57478)),_57072),_57616 is _57478+25,initiate(from(working(_57036,false),to(_57072,_57074))),initiate(from(location(_57036,point(_57476,_57616)),to(_57072,_57074))),initiate(from(dropper(_57036,_57030,_57032,_57034,_57072),to(_57072,_57074))))).
% Into: l_events(happens(createDropper(_57028,_57030,_57032,_57034,_57036),_57072,_57074),[_57036=dropper(_57028),objectName(_57028),number(_57030),_57034=conveyor(_57356),holds(location(_57034,point(_57476,_57478)),_57072),_57616 is _57478+25,happens(initiate(working(_57036,false)),_57072,_57074),happens(initiate(location(_57036,point(_57476,_57616))),_57072,_57074),happens(initiate(dropper(_57036,_57030,_57032,_57034,_57072)),_57072,_57074)]).

% LPS:  actions(setDroppingSpeed(_59914,_59916)).
% Into: actions([setDroppingSpeed(_59914,_59916)]).

% LPS:  if(updates(setDroppingSpeed(_60974,_60976),in(to(_61012,_60976),dropper(_60974,_61012,_61140,_61142,_61144))),number(_60976)).
% Into: updated(happens(setDroppingSpeed(_60974,_60976),_62492,_62498),dropper(_60974,_61012,_61140,_61142,_61144),_61012-_60976,[number(_60976)]).

% LPS:  then(if((at(dropper(_62412,_62414,_62416,_62418,_62420),_62442),at(working(_62412,true),_62442),_62414*(_62442-_62420)>=1)),(update(in(to(_62874,_62442),from(dropper(_62412,_62414,_62416,_62418,_62874),to(_62442,_62442+1)))),_63372 is round(_62414*(_62442-_62420)),from(clone(_63372,_62416,_63472),to(_62442,_62442+1)),from(drop(_63472,_62418),to(_62442+1,_62442+2)))).
% Into: reactive_rule([holds(dropper(_62412,_62414,_62416,_62418,_62420),_62442),holds(working(_62412,true),_62442),_62414*(_62442-_62420)>=1],[happens(update(_62874-_62442,dropper(_62412,_62414,_62416,_62418,_62874)),_62442,_62442+1),_63372 is round(_62414*(_62442-_62420)),happens(clone(_63372,_62416,_63472),_62442,_62442+1),happens(drop(_63472,_62418),_62442+1,_62442+2)]).

% LPS:  if(from(drop([_65318|_65320],_65348),to(_65384,_65386)),(from(placeOnConveyor(_65318,_65348,0),to(_65384,_65386)),from(drop(_65320,_65348),to(_65384,_65386)))).
% Into: l_events(happens(drop([_65318|_65320],_65348),_65384,_65386),[happens(placeOnConveyor(_65318,_65348,0),_65384,_65386),happens(drop(_65320,_65348),_65384,_65386)]).

% LPS:  drop([],_66964).
% Into: l_events(happens(drop([],_66964),_67926,_67932),[]).

% LPS:  if(at(dropper(_68018,_68020,_68022),_68044),(at(dropper(_68018,_68020,_68170,_68172,_68174),_68044),at(location(_68018,_68022),_68044))).
% Into: l_int(holds(dropper(_68018,_68020,_68022),_68044),[holds(dropper(_68018,_68020,_68170,_68172,_68174),_68044),holds(location(_68018,_68022),_68044)]).

% LPS:  then(if(at(true,1)),(from(createContainer(hotOil,50,_19546),to(_19562,_19564)),from(createContainer(usedOil,5,_19674),to(_19562,_19564)),from(createContainer(newOil,30,_19802),to(_19562,_19564)),from(setLocation(_19546,point(300,200)),to(_19564,_19970)),from(setLocation(_19674,point(200,0)),to(_19564,_19970)),from(setLocation(_19802,point(200,80)),to(_19564,_19970)),from(createPump(outward,_19546,_19674,_20394),to(_19564,_19970)),from(createPump(inward,_19674,_19546,_20536),to(_19564,_19970)),from(setPumpFlow(_20394,1),_19970),from(setPumpFlow(_20536,1),_19970),from(start(_20394),_19970),from(start(_20536),to(_19970,_20874)),from(createConveyor(feeding,point(550,140),point(50,140),_21070),to(_20874,_21088)),from(setConveyorSpeed(_21070,10),to(_21088,_21202)),from(start(_21070),to(_21088,_21202)),from(createHeater(_21504,point(100,75),point(500,290),150,_21512),to(_21202,_21530)),from(start(_21512),_21530),from(createCookable(shrimp,4,_21704),to(_21530,_21722)),from(setLocation(_21704,point(-20,-20)),_21722),from(createDropper(_21972,0.25,_21704,_21070,_21980),to(_21722,_21998)),from(start(_21980),_21998))).
% Into: reactive_rule([holds(true,1)],[happens(createContainer(hotOil,50,_19546),_19562,_19564),happens(createContainer(usedOil,5,_19674),_19562,_19564),happens(createContainer(newOil,30,_19802),_19562,_19564),happens(setLocation(_19546,point(300,200)),_19564,_19970),happens(setLocation(_19674,point(200,0)),_19564,_19970),happens(setLocation(_19802,point(200,80)),_19564,_19970),happens(createPump(outward,_19546,_19674,_20394),_19564,_19970),happens(createPump(inward,_19674,_19546,_20536),_19564,_19970),happens(setPumpFlow(_20394,1),_19970,_24816),happens(setPumpFlow(_20536,1),_19970,_24936),happens(start(_20394),_19970,_25056),happens(start(_20536),_19970,_20874),happens(createConveyor(feeding,point(550,140),point(50,140),_21070),_20874,_21088),happens(setConveyorSpeed(_21070,10),_21088,_21202),happens(start(_21070),_21088,_21202),happens(createHeater(_21504,point(100,75),point(500,290),150,_21512),_21202,_21530),happens(start(_21512),_21530,_25780),happens(createCookable(shrimp,4,_21704),_21530,_21722),happens(setLocation(_21704,point(-20,-20)),_21722,_26020),happens(createDropper(_21972,0.25,_21704,_21070,_21980),_21722,_21998),happens(start(_21980),_21998,_23804)]).

% LPS:  if(at(kitchenSummary(_24880,_24882,_24884,_24886),_24908),(findall(_25040,cookable(_25036,_25038,_25040),_25062),length(_25062,_24880),_24880>0,sum_list(_25062,_25238),_24882 is _25238/_24880,min_list(_25062,_24884),max_list(_25062,_24886))).
% Into: l_int(holds(kitchenSummary(_24880,_24882,_24884,_24886),_24908),[holds(findall(_25040,[holds(cookable(_25036,_25038,_25040),_24908)],_25062),_24908),length(_25062,_24880),_24880>0,sum_list(_25062,_25238),_24882 is _25238/_24880,min_list(_25062,_24884),max_list(_25062,_24886)]).
% /pack/logicmoo_ec/test/lps_user_examples/cooking.pl:315
% pop_lps_dialect('$BLOB'("<stream>(0x562ef32aac00)"),  (/.../(lps_user_examples, 'cooking.pl')-> /.../(lps_user_examples, 'cooking.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/cooking.pl':_52454).


distance(point(X1, Y1), point(X2, Y2), D) :-
    D is sqrt((X2-X1)*(X2-X1)+(Y2-Y1)*(Y2-Y1)).

updated(happens(setLocation(A, B), _, _), location(A, C), C-B, [B=point(D, E), number(D), number(E)]).
updated(happens(start(A), _, _), working(A, B), B-true, []).
updated(happens(stop(A), _, _), working(A, B), B-false, []).
updated(happens(setConveyorSpeed(A, B), _, _), conveyorSpeed(A, C), C-B, []).
updated(happens(setPumpFlow(A, B), _, _), pumpFlow(A, C), C-B, [number(B)]).
updated(happens(switchPumpOutputTo(A, B), _, _), pump(A, _, C), C-B, [B=container(_)]).
updated(happens(switchPumpInputTo(A, B), _, _), pump(A, C, _), C-B, [B=container(_)]).
updated(happens(pumpIt(A, B), _, _), container(A, C), C-D, [D is C+B]).
updated(happens(setDroppingSpeed(A, B), _, _), dropper(A, C, _, _, _), C-B, [number(B)]).

maxTime(100).

fluents([location(_, _)]).
fluents([working(_, _)]).
fluents([transports(_, _), conveyorEndpoint(A, _), conveyorSpeed(A, _)]).
fluents([container(_, _)]).
fluents([heater(A, _, _, B), heatable(A, B), initialTemperature(_, _, _)]).
fluents([cookable(_, _, _)]).
fluents([pump(A, _, _), pumpFlow(A, _)]).
fluents([dropper(_, _, _, _, _)]).

interpolate(point(SX, Y), point(EX, Y), D, point(X, Y)) :-
    !,
    (   EX>=SX
    ->  X is SX+D
    ;   X is SX-D
    ).
interpolate(point(X, SY), point(X, EY), D, point(X, Y)) :-
    !,
    (   EY>=SY
    ->  Y is SY+D
    ;   Y is SY-D
    ).
interpolate(point(SX, SY), point(EX, EY), D, point(X, Y)) :-
    M is (EY-SY)/(EX-SX),
    DX is D/sqrt(1+M*M),
    DY is DX*M,
    X is round(SX+DX),
    Y is round(SY+DY).

l_events(happens(clone(A, B), C, D), [A=..[E, _], objectName(F), B=..[E, F], holds(findall(G, [holds(H, C), H=..[I, A|J], G=..[I, B|J]], K), C), happens(initiateAll(K), C, D)]).
l_events(happens(initiateAll([A|B]), C, D), [happens(initiate(A), C, _), happens(initiateAll(B), C, D)]).
l_events(happens(initiateAll([]), A, B), [B=:=A+1]).
l_events(happens(clone(A, B, [C|D]), E, F), [A>0, happens(clone(B, C), E, F), G is A-1, happens(clone(G, B, D), E, F)]).
l_events(happens(clone(0, _, []), _, _), []).
l_events(happens(placeOnConveyor(A, B, C), D, E), [holds(not(transports(B, A)), D), holds(location(B, F), _), holds(conveyorEndpoint(B, G), _), interpolate(F, G, C, H), happens(update(I-H, location(A, I)), D, E), happens(initiate(transports(B, A)), D, E)]).
l_events(happens(createConveyor(A, B, C, D), E, F), [D=conveyor(A), objectName(A), happens(initiate(location(D, B)), E, F), happens(initiate(conveyorEndpoint(D, C)), E, F), happens(initiate(conveyorSpeed(D, 0)), E, F), happens(initiate(working(D, false)), E, F)]).
l_events(happens(createContainer(A, B, C), D, E), [C=container(A), objectName(A), number(B), happens(initiate(container(C, B)), D, E), happens(initiate(location(C, point(0, 0))), D, E)]).
l_events(happens(createHeater(A, B, C, D, E), F, G), [E=heater(A), objectName(A), happens(initiate(heater(E, B, C, D)), F, G), happens(initiate(working(E, false)), F, G)]).
l_events(happens(createCookable(A, B, C), D, E), [C=cookable(A), objectName(A), happens(initiate(location(C, point(0, 0))), D, E), happens(initiate(heatable(C, B)), D, E), happens(initiate(cookable(C, B, 0)), D, E)]).
l_events(happens(createPump(A, B, C, D), E, F), [B\=C, B=container(_), C=container(_), D=pump(A), objectName(A), happens(initiate(pump(D, B, C)), E, F), happens(initiate(pumpFlow(D, 0)), E, F), happens(initiate(working(D, false)), E, F)]).
l_events(happens(createDropper(A, B, C, D, E), F, G), [E=dropper(A), objectName(A), number(B), D=conveyor(_), holds(location(D, point(H, I)), F), J is I+25, happens(initiate(working(E, false)), F, G), happens(initiate(location(E, point(H, J))), F, G), happens(initiate(dropper(E, B, C, D, F)), F, G)]).
l_events(happens(drop([A|B], C), D, E), [happens(placeOnConveyor(A, C, 0), D, E), happens(drop(B, C), D, E)]).
l_events(happens(drop([], _), _, _), []).

inside(point(X, Y), point(BLX, BLY), point(TRX, TRY)) :-
    X>BLX,
    X<TRX,
    Y>BLY,
    Y<TRY.

:- dynamic actions/1.
:- multifile actions/1.

actions([setLocation(_, _)]).
actions([start(_), stop(_)]).
actions([setConveyorSpeed(_, _)]).
actions([setPumpFlow(A, _), switchPumpOutputTo(A, _), switchPumpInputTo(A, _), pumpIt(A, _)]).
actions([setDroppingSpeed(_, _)]).

newPosition(Current, _Start, End, Delta, NewPoint) :-
    Delta>=0,
    interpolate(Current, End, Delta, NewPoint).
newPosition(Current, Start, _End, Delta, NewPoint) :-
    Delta<0,
    interpolate(Current, Start, Delta, NewPoint).

d(conveyor(C, point(SX, SY_), point(EX, EY_), _Working, Speed), [type:Type, arrow:Speed, headLength:10, strokeWidth:2, strokeColor:black, from:[RSX, RSY], to:[REX, REY]|Label]) :-
    SY is SY_+ -5,
    EY is EY_+ -5,
    format(string(Sp), "~w px/cycle", [Speed]),
    (   Speed=0
    ->  Type=line,
        RSX=SX,
        RSY=SY,
        REX=EX,
        REY=EY,
        label=[]
    ;   Speed>0
    ->  Type=arrow,
        RSX=SX,
        RSY=SY,
        REX=EX,
        REY=EY,
        Label=[label:Sp]
    ;   Type=arrow,
        RSX=EX,
        RSY=EY,
        REX=SX,
        REY=SY,
        Label=[label:Sp]
    ).
d(container(C, Level, point(X, Y)), [from:[X, Y], to:[RightX, RightY], label:Name:Level, type:rectangle, fontSize:13, fillColor:'#85bb65']) :-
    RightX is X+10,
    RightY is Y+Level,
    C=..[_, Name].
d(heater(ID, point(BLX, BLY), point(TRX, TRY), Temp), [type:rectangle, label:TS, from:[BLX, BLY], to:[TRX, TRY], strokeColor:red]) :-
    format(string(TS), "~wo", [Temp]).
d(cookable(C, _, D, point(X, Y)), [type:circle, center:[X, Y], radius:5, label:Ds, fontSize:13, fillColor:red]) :-
    format(string(Ds), "~2f", [D]).
d(dropper(dropper(Name), Speed, point(X, Y)), [[type:line, from:[X, Y], to:[TLX, TY], strokeColor:black], [type:line, from:[X, Y], to:[TRX, TY], strokeColor:black]]) :-
    TLX is X-10,
    TY is Y+15,
    TRX is X+10.
d(kitchenSummary(Count, Avg, Min, Max), [from:[400, 300], to:[600, 350], label:S, type:rectangle, fillColor:salmon]) :-
    format(string(S),
           "~w items, ~ndoneness ~2f (~2f-~2f)",
           [Count, Avg, Min, Max]).
d(timeless, [[type:rectangle, from:[0, 0], to:[600, 350], strokeColor:green]]).

objectName(Name) :-
    (   nonvar(Name)
    ->  true
    ;   gensym(object, Name)
    ).

notReachingConveyorEnds(0, _, _, _).
notReachingConveyorEnds(Speed, _, End, Current) :-
    Speed>0,
    distance(Current, End, Delta),
    Delta>=abs(Speed).
notReachingConveyorEnds(Speed, Start, _End, Current) :-
    Speed<0,
    distance(Start, Current, Delta),
    Delta>=abs(Speed).

l_int(holds(conveyor(A, B, C, D, E), F), [holds(conveyorEndpoint(A, C), F), holds(location(A, B), F), holds(working(A, D), F), holds(conveyorSpeed(A, E), F)]).
l_int(holds(container(A, B, C), D), [holds(container(A, B), D), holds(location(A, C), D)]).
l_int(holds(cookable(A, B, C, D), E), [holds(cookable(A, B, C), E), holds(location(A, D), E)]).
l_int(holds(dropper(A, B, C), D), [holds(dropper(A, B, _, _, _), D), holds(location(A, C), D)]).
l_int(holds(kitchenSummary(A, B, C, D), E), [holds(findall(F, [holds(cookable(_, _, F), E)], G), E), length(G, A), A>0, sum_list(G, H), B is H/A, min_list(G, C), max_list(G, D)]).

reactive_rule([holds(conveyorSpeed(A, B), C), holds(working(A, true), C), holds(transports(A, D), C), holds(conveyorEndpoint(A, E), C), holds(location(D, F), C), holds(location(A, G), C), notReachingConveyorEnds(B, G, E, F)], [newPosition(F, G, E, B, H), happens(update(F-H, location(D, F)), C, _)]).
reactive_rule([holds(heater(A, B, C, _), D), holds(working(A, true), D), holds(heatable(E, F), D), holds(location(E, G), D), \+inside(G, B, C), holds(location(E, H), D+1), inside(H, B, C)], [happens(initiate(initialTemperature(A, E, F)), D+1, _)]).
reactive_rule([holds(heater(A, B, C, D), E), holds(working(A, true), E), holds(heatable(F, _), E), holds(location(F, G), E), inside(G, B, C)], [happens(update(H-D, heatable(F, H)), E, _)]).
reactive_rule([holds(heater(A, B, C, _), D), holds(working(A, true), D), holds(heatable(E, F), D), holds(location(E, G), D), inside(G, B, C), holds(location(E, H), D+1), \+inside(H, B, C), holds(initialTemperature(A, E, I), _)], [happens(terminate(initialTemperature(A, E, I)), D+1, _), happens(update(F-I, heatable(E, F)), D+1, _)]).
reactive_rule([holds(cookable(A, B, C), D), holds(heatable(A, E), D), E>B], [F is C+(E-B)*0.01, happens(update(C-F, cookable(A, B, C)), _, _)]).
reactive_rule([holds(pump(A, B, C), D), holds(working(A, true), D), holds(container(B, E), D)], [holds(pumpFlow(A, F), D), E-F>=0, happens(pumpIt(B, -F), D, _), happens(pumpIt(C, F), D, _)]).
reactive_rule([holds(dropper(A, B, C, D, E), F), holds(working(A, true), F), B*(F-E)>=1], [happens(update(G-F, dropper(A, B, C, D, G)), F, F+1), H is round(B*(F-E)), happens(clone(H, C, I), F, F+1), happens(drop(I, D), F+1, F+2)]).
reactive_rule([holds(true, 1)], [happens(createContainer(hotOil, 50, A), B, C), happens(createContainer(usedOil, 5, D), B, C), happens(createContainer(newOil, 30, E), B, C), happens(setLocation(A, point(300, 200)), C, F), happens(setLocation(D, point(200, 0)), C, F), happens(setLocation(E, point(200, 80)), C, F), happens(createPump(outward, A, D, G), C, F), happens(createPump(inward, D, A, H), C, F), happens(setPumpFlow(G, 1), F, _), happens(setPumpFlow(H, 1), F, _), happens(start(G), F, _), happens(start(H), F, I), happens(createConveyor(feeding, point(550, 140), point(50, 140), J), I, K), happens(setConveyorSpeed(J, 10), K, L), happens(start(J), K, L), happens(createHeater(_, point(100, 75), point(500, 290), 150, M), L, N), happens(start(M), N, _), happens(createCookable(shrimp, 4, O), N, P), happens(setLocation(O, point(-20, -20)), P, _), happens(createDropper(_, 0.25, O, J, Q), P, R), happens(start(Q), R, _)]).
% dB(/.../(lps_user_examples, 'cooking.pl'), lps_visualization(_1247824{groups:[_968256{content:"container(A,B)", id:"container/2", order:3, subgroupStack:"false"}, _968282{content:"conveyorEndpoint(A,B)", id:"conveyorEndpoint/2", order:3, subgroupStack:"false"}, _968308{content:"conveyorSpeed(A,B)", id:"conveyorSpeed/2", order:3, subgroupStack:"false"}, _968334{content:"cookable(A,B,C)", id:"cookable/3", order:3, subgroupStack:"false"}, _968360{content:"dropper(A,B,C,D,E)", id:"dropper/5", order:3, subgroupStack:"false"}, _968386{content:"heatable(A,B)", id:"heatable/2", order:3, subgroupStack:"false"}, _968412{content:"heater(A,B,C,D)", id:"heater/4", order:3, subgroupStack:"false"}, _968438{content:"initialTemperature(A,B,C)", id:"initialTemperature/3", order:3, subgroupStack:"false"}, _968464{content:"location(A,B)", id:"location/2", order:3, subgroupStack:"false"}, _968490{content:"pump(A,B,C)", id:"pump/3", order:3, subgroupStack:"false"}, _968516{content:"pumpFlow(A,B)", id:"pumpFlow/2", order:3, subgroupStack:"false"}, _968542{content:"transports(A,B)", id:"transports/2", order:3, subgroupStack:"false"}, _968568{content:"working(A,B)", id:"working/2", order:3, subgroupStack:"false"}, _968594{content:"Actions", id:"action", order:4}], items:[_968628{content:"container(hotOil),50", end:101, group:"container/2", id:0, start:2, subgroup:"container(hotOil)", title:"Fluent container(container(hotOil),50) initiated at 2<br/>and terminated at transition to 101"}, _968666{content:"container(newOil),30", end:101, group:"container/2", id:1, start:2, subgroup:"container(newOil)", title:"Fluent container(container(newOil),30) initiated at 2<br/>and terminated at transition to 101"}, _968704{content:"container(usedOil),5", end:101, group:"container/2", id:2, start:2, subgroup:"container(usedOil)", title:"Fluent container(container(usedOil),5) initiated at 2<br/>and terminated at transition to 101"}, ...(_1258922)]}, _1257940{cycles:[[_1257108{create:[_1257084{from:[0, 0], id:"timeless", strokeColor:"green", to:[600, 350], type:"rectangle"}]}], [], [], ...(_1262716)]})).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'deliveryDelay.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'deliveryDelay.pl')).
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/deliveryDelay.pl':_1279298).

% dB(/.../(lps_user_examples, 'deliveryDelay.pl'), lps_visualization(_1392242{groups:[], items:[]}, [])).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'delivery.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'delivery.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/delivery.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'delivery.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'delivery.pl'), lps= /.../(lps_user_examples, 'delivery.pl'), using= /.../(lps_user_examples, 'delivery.pl')].
% continue_lps_dialect.
% ops.

% LPS:  fluents((day/1,penalty/2,delivered/1,force_majeure/1,terminated/1)).
% Into: fluents([day(_1428926),penalty(_1428936,_1428938),delivered(_1428948),force_majeure(_1428958),terminated(_1428968)]).

% LPS:  events((end_of_day/1,deliver/1)).
% Into: events([end_of_day(_1429960),deliver(_1429970)]).

% LPS:  initiates(deliver(_1429906),delivered(_1429906)).
% Into: initiated(happens(deliver(_1429906),_1431054,_1431060),delivered(_1429906),[]).

% LPS:  if(updates(end_of_day(_1431050),in(to(_1431086,_1431088),penalty(_1431168,_1431086))),(latest_delivery(_1431168,_1431290),not(delivered(_1431168)),day(_1431402),_1431402@>_1431290,days_difference(_1431290,_1431536,_1431402),not(force_majeure(_1431586)),not(terminated(_1431168)),total_value(_1431168,_1431738),penalty_percentage(_1431168,_1431794),percentage_cap(_1431168,_1431850),_1431088 is _1431794*_1431738*_1431536,_1432094 is _1431850*_1431738,_1431088=<_1432094)).
% Into: updated(happens(end_of_day(_1431050),_1434044,_1434050),penalty(_1431168,_1431086),_1431086-_1431088,[latest_delivery(_1431168,_1431290),holds(not(delivered(_1431168)),_1434044),holds(day(_1431402),_1434044),_1431402@>_1431290,days_difference(_1431290,_1431536,_1431402),holds(not(force_majeure(_1431586)),_1434044),holds(not(terminated(_1431168)),_1434044),total_value(_1431168,_1431738),penalty_percentage(_1431168,_1431794),percentage_cap(_1431168,_1431850),_1431088 is _1431794*_1431738*_1431536,_1432094 is _1431850*_1431738,_1431088=<_1432094]).

% LPS:  if(at(entitled(terminate(_1438268,_1438270)),_1438296),(buyer(_1438270,_1438268),latest_delivery(_1438270,_1438434),at(not(delivered(_1438270)),_1438296),at(day(_1438586),_1438296),_1438586@>_1438434)).
% Into: l_int(holds(entitled(terminate(_1438268,_1438270)),_1438296),[buyer(_1438270,_1438268),latest_delivery(_1438270,_1438434),holds(not(delivered(_1438270)),_1438296),holds(day(_1438586),_1438296),_1438586@>_1438434]).

% LPS:  initially(penalty(mydelivery,0)).
% Into: initial_state([penalty(mydelivery,0)]).

% LPS:  observe(from(deliver(mydelivery),to(11,12))).
% Into: observe([deliver(mydelivery)],12).

% LPS:  initially(day(2018/3/31)).
% Into: initial_state([day(2018/3/31)]).

% LPS:  observe(from(end_of_day(2018/3/31),to(2,3))).
% Into: observe([end_of_day(2018/3/31)],3).

% LPS:  observe(from(end_of_day(2018/4/1),to(4,5))).
% Into: observe([end_of_day(2018/4/1)],5).

% LPS:  observe(from(end_of_day(2018/4/2),to(6,7))).
% Into: observe([end_of_day(2018/4/2)],7).

% LPS:  observe(from(end_of_day(2018/4/3),to(8,9))).
% Into: observe([end_of_day(2018/4/3)],9).

% LPS:  observe(from(end_of_day(2018/4/4),to(10,11))).
% Into: observe([end_of_day(2018/4/4)],11).

% LPS:  observe(from(end_of_day(2018/4/5),to(12,13))).
% Into: observe([end_of_day(2018/4/5)],13).

% LPS:  observe(from(end_of_day(2018/4/6),to(14,15))).
% Into: observe([end_of_day(2018/4/6)],15).

% LPS:  if(updates(end_of_day(_1463460),in(to(_1463496,_1463498),day(_1463496))),next_day(_1463496,_1463498)).
% Into: updated(happens(end_of_day(_1463460),_1464890,_1464896),day(_1463496),_1463496-_1463498,[next_day(_1463496,_1463498)]).
% /pack/logicmoo_ec/test/lps_user_examples/delivery.pl:95
% pop_lps_dialect('$BLOB'("<stream>(0x562ef76d7b00)"),  (/.../(lps_user_examples, 'delivery.pl')-> /.../(lps_user_examples, 'delivery.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/delivery.pl':_1478026).


initiated(happens(deliver(A), _, _), delivered(A), []).

percentage_cap(mydelivery, 0.5).

latest_delivery(mydelivery, 2018/4/1).

total_value(mydelivery, 100).

fluents([day(_), penalty(_, _), delivered(_), force_majeure(_), terminated(_)]).

l_int(holds(entitled(terminate(A, B)), C), [buyer(B, A), latest_delivery(B, D), holds(not(delivered(B)), C), holds(day(E), C), E@>D]).

next_day(2018/3/31, 2018/4/1).
next_day(Year/4/Day1, Year/4/Day2) :-
    Day1<31,
    Day2 is Day1+1.

seller(mydelivery, bob).

buyer(mydelivery, alex).

initial_state([penalty(mydelivery, 0)]).
initial_state([day(2018/3/31)]).

penalty_percentage(mydelivery, 0.2).

days_difference(Year/Month/Day1, Difference, Year/Month/Day2) :-
    Difference is Day2-Day1.

:- dynamic actions/1.
:- multifile actions/1.


events([end_of_day(_), deliver(_)]).

updated(happens(end_of_day(_), A, _), penalty(B, C), C-D, [latest_delivery(B, E), holds(not(delivered(B)), A), holds(day(F), A), F@>E, days_difference(E, G, F), holds(not(force_majeure(_)), A), holds(not(terminated(B)), A), total_value(B, H), penalty_percentage(B, I), percentage_cap(B, J), D is I*H*G, K is J*H, D=<K]).
updated(happens(end_of_day(_), _, _), day(A), A-B, [next_day(A, B)]).

observe([deliver(mydelivery)], 12).
observe([end_of_day(2018/3/31)], 3).
observe([end_of_day(2018/4/1)], 5).
observe([end_of_day(2018/4/2)], 7).
observe([end_of_day(2018/4/3)], 9).
observe([end_of_day(2018/4/4)], 11).
observe([end_of_day(2018/4/5)], 13).
observe([end_of_day(2018/4/6)], 15).

equipment(mydelivery, logicforproblemsolving).
% dB(/.../(lps_user_examples, 'delivery.pl'), lps_visualization(_1623562{groups:[_1620780{content:"Events", id:"event", order:1}, _1620854{content:"day(A)", id:"day/1", order:3, subgroupStack:"false"}, _1620932{content:"delivered(A)", id:"delivered/1", order:3, subgroupStack:"false"}, _1621010{content:"penalty(A,B)", id:"penalty/2", order:3, subgroupStack:"false"}], items:[_1621136{content:"2018/3/31", end:3, group:"day/1", id:0, start:1, subgroup:"2018/3/31", title:"Fluent day(2018/3/31) initiated at 1<br/>and terminated at transition to 3"}, _1621262{content:"2018/4/1", end:5, group:"day/1", id:1, start:3, subgroup:"2018/4/1", title:"Fluent day(2018/4/1) initiated at 3<br/>and terminated at transition to 5"}, _1621388{content:"2018/4/2", end:7, group:"day/1", id:2, start:5, subgroup:"2018/4/2", title:"Fluent day(2018/4/2) initiated at 5<br/>and terminated at transition to 7"}, _1621514{content:"2018/4/3", end:9, group:"day/1", id:3, start:7, subgroup:"2018/4/3", title:"Fluent day(2018/4/3) initiated at 7<br/>and terminated at transition to 9"}, _1621640{content:"2018/4/4", end:11, group:"day/1", id:4, start:9, subgroup:"2018/4/4", title:"Fluent day(2018/4/4) initiated at 9<br/>and terminated at transition to 11"}, _1621766{content:"2018/4/5", end:13, group:"day/1", id:5, start:11, subgroup:"2018/4/5", title:"Fluent day(2018/4/5) initiated at 11<br/>and terminated at transition to 13"}, _1621892{content:"2018/4/6", end:15, group:"day/1", id:6, start:13, subgroup:"2018/4/6", title:"Fluent day(2018/4/6) initiated at 13<br/>and terminated at transition to 15"}, _1622018{content:"2018/4/7", end:21, group:"day/1", id:7, start:15, subgroup:"2018/4/7", title:"Fluent day(2018/4/7) initiated at 15<br/>and terminated at transition to 21"}, _1622144{content:"mydelivery", end:21, group:"delivered/1", id:8, start:12, subgroup:"mydelivery", title:"Fluent delivered(mydelivery) initiated at 12<br/>and terminated at transition to 21"}, _1622270{content:"mydelivery,0", end:7, group:"penalty/2", id:9, start:1, subgroup:"mydelivery", title:"Fluent penalty(mydelivery,0) initiated at 1<br/>and terminated at transition to 7"}, _1622396{content:"mydelivery,20.0", end:9, group:"penalty/2", id:10, start:7, subgroup:"mydelivery", title:"Fluent penalty(mydelivery,20.0) initiated at 7<br/>and terminated at transition to 9"}, _1622522{content:"mydelivery,40.0", end:21, group:"penalty/2", id:11, start:9, subgroup:"mydelivery", title:"Fluent penalty(mydelivery,40.0) initiated at 9<br/>and terminated at transition to 21"}, _1622648{content:"end_of_day(2018/3/31)", group:"event", id:12, start:3, style:"color:#E19735", title:"happens(end_of_day(2018/3/31),2,3)", type:"point"}, _1622774{content:"end_of_day(2018/4/1)", group:"event", id:13, start:5, style:"color:#E19735", title:"happens(end_of_day(2018/4/1),4,5)", type:"point"}, _1622900{content:"end_of_day(2018/4/2)", group:"event", id:14, start:7, style:"color:#E19735", title:"happens(end_of_day(2018/4/2),6,7)", type:"point"}, _1623026{content:"end_of_day(2018/4/3)", group:"event", id:15, start:9, style:"color:#E19735", title:"happens(end_of_day(2018/4/3),8,9)", type:"point"}, _1623152{content:"end_of_day(2018/4/4)", group:"event", id:16, start:11, style:"color:#E19735", title:"happens(end_of_day(2018/4/4),10,11)", type:"point"}, _1623278{content:"deliver(mydelivery)", group:"event", id:17, start:12, style:"color:#E19735", title:"happens(deliver(mydelivery),11,12)", type:"point"}, _1623404{content:"end_of_day(2018/4/5)", group:"event", id:18, start:13, style:"color:#E19735", title:"happens(end_of_day(2018/4/5),12,13)", type:"point"}, _1623530{content:"end_of_day(2018/4/6)", group:"event", id:19, start:15, style:"color:#E19735", title:"happens(end_of_day(2018/4/6),14,15)", type:"point"}]}, [])).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'emergencia.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'emergencia.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/emergencia.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'emergencia.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'emergencia.pl'), lps= /.../(lps_user_examples, 'emergencia.pl'), using= /.../(lps_user_examples, 'emergencia.pl')].
% continue_lps_dialect.
% ops.

% LPS:  fluents(emergencia).
% Into: fluents([emergencia]).

% LPS:  actions(presione_el_botón).
% Into: actions([presione_el_botón]).

% LPS:  events(alerte_al_conductor).
% Into: events([alerte_al_conductor]).

% LPS:  observe(from(emergencia,to(1,2))).
% Into: observe([emergencia],2).

% LPS:  then(if(at(emergencia,_1672600)),from(alerte_al_conductor,to(_1672600,_1672704))).
% Into: reactive_rule([holds(emergencia,_1672600)],[happens(alerte_al_conductor,_1672600,_1672704)]).

% LPS:  if(from(alerte_al_conductor,to(_1674278,_1674280)),from(presione_el_botón,to(_1674278,_1674280))).
% Into: l_events(happens(alerte_al_conductor,_1674278,_1674280),[happens(presione_el_botón,_1674278,_1674280)]).
% /pack/logicmoo_ec/test/lps_user_examples/emergencia.pl:20
% pop_lps_dialect('$BLOB'("<stream>(0x562ef70be500)"),  (/.../(lps_user_examples, 'emergencia.pl')-> /.../(lps_user_examples, 'emergencia.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/emergencia.pl':_1683160).


fluents([emergencia]).

reactive_rule([holds(emergencia, A)], [happens(alerte_al_conductor, A, _)]).

l_events(happens(alerte_al_conductor, A, B), [happens(presione_el_botón, A, B)]).

:- dynamic actions/1.
:- multifile actions/1.

actions([presione_el_botón]).

events([alerte_al_conductor]).

observe([emergencia], 2).

maxTime(5).
% dB(/.../(lps_user_examples, 'emergencia.pl'), lps_visualization(_1720846{groups:[_1720692{content:"Events", id:"event", order:1}], items:[_1720814{content:"emergencia", group:"event", id:0, start:2, style:"color:#E19735", title:"happens(emergencia,1,2)", type:"point"}]}, [])).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'emergenciasimple.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'emergenciasimple.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/emergenciasimple.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'emergenciasimple.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'emergenciasimple.pl'), lps= /.../(lps_user_examples, 'emergenciasimple.pl'), using= /.../(lps_user_examples, 'emergenciasimple.pl')].
% continue_lps_dialect.
% ops.

% LPS:  actions(presione_el_botón).
% Into: actions([presione_el_botón]).

% LPS:  events((emergencia,alerte_al_conductor)).
% Into: events([emergencia,alerte_al_conductor]).

% LPS:  observe(from(emergencia,to(1,2))).
% Into: observe([emergencia],2).

% LPS:  then(if(from(emergencia,to(_1760562,_1760564))),from(alerte_al_conductor,to(_1760564,_1760700))).
% Into: reactive_rule([happens(emergencia,_1760562,_1760564)],[happens(alerte_al_conductor,_1760564,_1760700)]).

% LPS:  if(from(alerte_al_conductor,to(_1761858,_1761860)),from(presione_el_botón,to(_1761858,_1761860))).
% Into: l_events(happens(alerte_al_conductor,_1761858,_1761860),[happens(presione_el_botón,_1761858,_1761860)]).
% /pack/logicmoo_ec/test/lps_user_examples/emergenciasimple.pl:19
% pop_lps_dialect('$BLOB'("<stream>(0x562ef70bfc00)"),  (/.../(lps_user_examples, 'emergenciasimple.pl')-> /.../(lps_user_examples, 'emergenciasimple.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/emergenciasimple.pl':_1770746).


reactive_rule([happens(emergencia, _, A)], [happens(alerte_al_conductor, A, _)]).

l_events(happens(alerte_al_conductor, A, B), [happens(presione_el_botón, A, B)]).

:- dynamic actions/1.
:- multifile actions/1.

actions([presione_el_botón]).

events([emergencia, alerte_al_conductor]).

observe([emergencia], 2).

maxTime(5).

% dB(/.../(lps_user_examples, 'emergenciasimple.pl'), lps_visualization(_1815414{groups:[_1815072{content:"Events", id:"event", order:1}, _1815134{content:"Actions", id:"action", order:4}], items:[_1815256{content:"emergencia", group:"event", id:0, start:2, style:"color:#E19735", title:"happens(emergencia,1,2)", type:"point"}, _1815382{content:"presione_el_botón", group:"action", id:1, start:3, style:"color:green", title:"happens(presione_el_botón,2,3)", type:"point"}]}, [])).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'emergency.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'emergency.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/emergency.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'emergency.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'emergency.pl'), lps= /.../(lps_user_examples, 'emergency.pl'), using= /.../(lps_user_examples, 'emergency.pl')].
% continue_lps_dialect.
% ops.

% LPS:  fluents(emergency).
% Into: fluents([emergency]).

% LPS:  actions(press(_1853490)).
% Into: actions([press(_1853490)]).

% LPS:  events(alert/1).
% Into: events([alert(_1855572)]).

% LPS:  initially(emergency).
% Into: initial_state([emergency]).

% LPS:  then(if(at(emergency,_1856564)),from(alert(driver),to(_1856564,_1856692))).
% Into: reactive_rule([holds(emergency,_1856564)],[happens(alert(driver),_1856564,_1856692)]).

% LPS:  if(from(alert(driver),to(_1858294,_1858296)),from(press(buttom),to(_1858294,_1858296))).
% Into: l_events(happens(alert(driver),_1858294,_1858296),[happens(press(buttom),_1858294,_1858296)]).

% LPS:  terminates(alert(driver),emergency).
% Into: terminated(happens(alert(driver),_1860668,_1860674),emergency,[]).
% /pack/logicmoo_ec/test/lps_user_examples/emergency.pl:21
% pop_lps_dialect('$BLOB'("<stream>(0x562ef70c1300)"),  (/.../(lps_user_examples, 'emergency.pl')-> /.../(lps_user_examples, 'emergency.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/emergency.pl':_1868246).


fluents([emergency]).

terminated(happens(alert(driver), _, _), emergency, []).

reactive_rule([holds(emergency, A)], [happens(alert(driver), A, _)]).

initial_state([emergency]).

l_events(happens(alert(driver), A, B), [happens(press(buttom), A, B)]).

:- dynamic actions/1.
:- multifile actions/1.

actions([press(_)]).

events([alert(_)]).

maxTime(5).
% dB(/.../(lps_user_examples, 'emergency.pl'), lps_visualization(_1922360{groups:[_1921904{content:"emergency", id:"emergency/0", order:3, subgroupStack:"false"}, _1921970{content:"Actions", id:"action", order:4}], items:[_1922080{content:"emergency", end:2, group:"emergency/0", id:0, start:1, title:"Fluent emergency initiated at 1<br/>and terminated at transition to 2"}, _1922202{content:"press(buttom)", group:"action", id:1, start:2, style:"color:green", title:"happens(press(buttom),1,2)", type:"point"}, _1922328{content:"press(buttom)", group:"action", id:2, start:3, style:"color:green", title:"happens(press(buttom),2,3)", type:"point"}]}, [])).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'end of daay.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'end of daay.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/end of daay.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'end of daay.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'end of daay.pl'), lps= /.../(lps_user_examples, 'end of daay.pl'), using= /.../(lps_user_examples, 'end of daay.pl')].
% continue_lps_dialect.
% ops.
% /pack/logicmoo_ec/test/lps_user_examples/end of daay.pl:3
% pop_lps_dialect('$BLOB'("<stream>(0x562ef76d8e00)"),  (/.../(lps_user_examples, 'end of daay.pl')-> /.../(lps_user_examples, 'end of daay.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/end of daay.pl':_1966472).


:- dynamic actions/1.
:- multifile actions/1.

% dB(/.../(lps_user_examples, 'end of daay.pl'), lps_visualization(_2077176{groups:[], items:[]}, [])).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'end of day.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'end of day.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/end of day.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'end of day.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'end of day.pl'), lps= /.../(lps_user_examples, 'end of day.pl'), using= /.../(lps_user_examples, 'end of day.pl')].
% continue_lps_dialect.
% ops.
% /pack/logicmoo_ec/test/lps_user_examples/end of day.pl:3
% pop_lps_dialect('$BLOB'("<stream>(0x562ef6884e00)"),  (/.../(lps_user_examples, 'end of day.pl')-> /.../(lps_user_examples, 'end of day.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/end of day.pl':_2119616).


:- dynamic actions/1.
:- multifile actions/1.

% dB(/.../(lps_user_examples, 'end of day.pl'), lps_visualization(_2230320{groups:[], items:[]}, [])).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'eventgraph.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'eventgraph.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/eventgraph.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'eventgraph.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'eventgraph.pl'), lps= /.../(lps_user_examples, 'eventgraph.pl'), using= /.../(lps_user_examples, 'eventgraph.pl')].
% continue_lps_dialect.
% ops.
% /pack/logicmoo_ec/test/lps_user_examples/eventgraph.pl:92
% pop_lps_dialect('$BLOB'("<stream>(0x562ef33a8d00)"),  (/.../(lps_user_examples, 'eventgraph.pl')-> /.../(lps_user_examples, 'eventgraph.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/eventgraph.pl':_2294948).


alledges(L) :-
    findall(X,
            ( base(B),
              member(Event, B),
              event2edge(Event, X)
            ),
            L).

event2edge(event(A, Rel, B), subgraph(RelID, [node([style=filled]), edge([label=Rel]),  (A->B)])) :-
    gensym(Rel, RelID),
    not(A=B).

copyfromto(_C, _N, _M, [], []).
copyfromto(A, B, C, [D|E], [D|F]) :-
    B=<A,
    <=(A, C),
    !,
    G is A+1,
    copyfromto(G, B, C, E, F).
copyfromto(C, N, M, [_H|R1], R2) :-
    CC is C+1,
    copyfromto(CC, N, M, R1, R2).

:- dynamic'swish renderer'/2.

'swish renderer'(graphviz, []).

base([event('CD8', bind, 'CD4'), event('CD4', regulate, 'SARS-CoV-2'), event('CD8', regulate, 'SARS-CoV-2'), event('CD8', inhibit, 'SARS-CoV-2'), event('CD4', inhibit, 'SARS-CoV-2'), event('CD4', regulate, 'JAK3'), event('CD8', regulate, 'JAK3'), event('CD8', inhibit, 'JAK3'), event('CD4', bind, 'JAK3'), event('CD4', inhibit, 'JAK3'), event('CD4', bind, 'CD8'), event('CD8', bind, 'SARS-CoV-2'), event('CD8', bind, 'JAK3'), event('CD4', associate, 'JAK3'), event('JAK3', regulate, 'JAK3'), event('JAK3', bind, 'JAK3'), event('JAK3', inhibit, 'JAK3'), event('JAK3', associate, 'STAT'), event('STAT', regulate, 'SARS-CoV-2'), event('STAT', inhibit, 'SARS-CoV-2'), event('STAT', associate, 'MHC'), event('MHC', bind, 'SARS-CoV-2'), event('MHC', inhibit, 'SARS-CoV-2'), event('MHC', regulate, 'SARS-CoV-2'), event('MHC', associate, importin), event(importin, inhibit, 'SARS-CoV-2'), event(importin, regulate, 'SARS-CoV-2'), event(importin, bind, 'SARS-CoV-2'), event('JAK3', associate, 'MHC'), event('MHC', associate, 'STAT'), event(importin, associate, 'STAT'), event('JAK3', associate, importin), event(importin, associate, 'MHC'), event('CD4', associate, 'STAT'), event('STAT', regulate, 'JAK3'), event('STAT', bind, 'JAK3'), event('STAT', inhibit, 'JAK3'), event('STAT', associate, 'JAK3'), event('MHC', bind, 'JAK3'), event('MHC', regulate, 'JAK3'), event('MHC', inhibit, 'JAK3'), event('MHC', associate, 'JAK3'), event(importin, regulate, 'JAK3'), event(importin, inhibit, 'JAK3'), event(importin, bind, 'JAK3'), event('CD4', associate, 'MHC'), event('CD8', associate, 'JAK3'), event('CD8', associate, 'STAT'), event('CD8', associate, 'MHC')]).

:- dynamic actions/1.
:- multifile actions/1.


draw_graph(N, M, digraph(F)) :-
    alledges(L),
    copyfromto(1, N, M, L, F).

test1(SG) :-
    event2edge(event('CD4', increase, 'RUNX1'), SG).
% dB(/.../(lps_user_examples, 'eventgraph.pl'), lps_visualization(_2413030{groups:[], items:[]}, [])).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'eYStcpBj.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'eYStcpBj.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/eYStcpBj.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'eYStcpBj.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'eYStcpBj.pl'), lps= /.../(lps_user_examples, 'eYStcpBj.pl'), using= /.../(lps_user_examples, 'eYStcpBj.pl')].
% continue_lps_dialect.
% ops.

% LPS:  fluents((leon_atrapado,liberar_leon,salvar_leon)).
% Into: fluents([leon_atrapado,liberar_leon,salvar_leon]).

% LPS:  actions((precavido,utilizar_dientes,correr)).
% Into: actions([precavido,utilizar_dientes,correr]).

% LPS:  events((liberar,cortar_amarre,escapar_trampa)).
% Into: events([liberar,cortar_amarre,escapar_trampa]).

% LPS:  initially(leon_atrapado).
% Into: initial_state([leon_atrapado]).

% LPS:  then(if(at(leon_atrapado,_2453160)),from(liberar,to(_2453160,_2453264))).
% Into: reactive_rule([holds(leon_atrapado,_2453160)],[happens(liberar,_2453160,_2453264)]).

% LPS:  if(from(liberar,to(_2454838,_2454840)),from(precavido,to(_2454838,_2454840))).
% Into: l_events(happens(liberar,_2454838,_2454840),[happens(precavido,_2454838,_2454840)]).

% LPS:  terminates(liberar,leon_atrapado).
% Into: terminated(happens(liberar,_2457148,_2457154),leon_atrapado,[]).

% LPS:  initially(liberar_leon).
% Into: initial_state([liberar_leon]).

% LPS:  then(if(at(liberar_leon,_2458102)),from(cortar_amarre,to(_2458102,_2458206))).
% Into: reactive_rule([holds(liberar_leon,_2458102)],[happens(cortar_amarre,_2458102,_2458206)]).

% LPS:  if(from(cortar_amarre,to(_2459346,_2459348)),from(utilizar_dientes,to(_2459346,_2459348))).
% Into: l_events(happens(cortar_amarre,_2459346,_2459348),[happens(utilizar_dientes,_2459346,_2459348)]).

% LPS:  terminates(cortar_amarre,liberar_leon).
% Into: terminated(happens(cortar_amarre,_2461656,_2461662),liberar_leon,[]).

% LPS:  initially(salvar_leon).
% Into: initial_state([salvar_leon]).

% LPS:  then(if(at(salvar_leon,_2462610)),from(escapar_trampa,to(_2462610,_2462714))).
% Into: reactive_rule([holds(salvar_leon,_2462610)],[happens(escapar_trampa,_2462610,_2462714)]).

% LPS:  if(from(escapar_trampa,to(_2463854,_2463856)),from(correr,to(_2463854,_2463856))).
% Into: l_events(happens(escapar_trampa,_2463854,_2463856),[happens(correr,_2463854,_2463856)]).

% LPS:  terminates(escapar_trampa,salvar_leon).
% Into: terminated(happens(escapar_trampa,_2466164,_2466170),salvar_leon,[]).
% /pack/logicmoo_ec/test/lps_user_examples/eYStcpBj.pl:34
% pop_lps_dialect('$BLOB'("<stream>(0x562ef31d3a00)"),  (/.../(lps_user_examples, 'eYStcpBj.pl')-> /.../(lps_user_examples, 'eYStcpBj.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/eYStcpBj.pl':_2473710).


fluents([leon_atrapado, liberar_leon, salvar_leon]).

terminated(happens(liberar, _, _), leon_atrapado, []).
terminated(happens(cortar_amarre, _, _), liberar_leon, []).
terminated(happens(escapar_trampa, _, _), salvar_leon, []).

reactive_rule([holds(leon_atrapado, A)], [happens(liberar, A, _)]).
reactive_rule([holds(liberar_leon, A)], [happens(cortar_amarre, A, _)]).
reactive_rule([holds(salvar_leon, A)], [happens(escapar_trampa, A, _)]).

initial_state([leon_atrapado]).
initial_state([liberar_leon]).
initial_state([salvar_leon]).

l_events(happens(liberar, A, B), [happens(precavido, A, B)]).
l_events(happens(cortar_amarre, A, B), [happens(utilizar_dientes, A, B)]).
l_events(happens(escapar_trampa, A, B), [happens(correr, A, B)]).

:- dynamic actions/1.
:- multifile actions/1.

actions([precavido, utilizar_dientes, correr]).

events([liberar, cortar_amarre, escapar_trampa]).

maxTime(10).
% dB(/.../(lps_user_examples, 'eYStcpBj.pl'), lps_visualization(_2595392{groups:[_2594056{content:"leon_atrapado", id:"leon_atrapado/0", order:3, subgroupStack:"false"}, _2594134{content:"liberar_leon", id:"liberar_leon/0", order:3, subgroupStack:"false"}, _2594212{content:"salvar_leon", id:"salvar_leon/0", order:3, subgroupStack:"false"}, _2594278{content:"Actions", id:"action", order:4}], items:[_2594388{content:"leon_atrapado", end:2, group:"leon_atrapado/0", id:0, start:1, title:"Fluent leon_atrapado initiated at 1<br/>and terminated at transition to 2"}, _2594498{content:"liberar_leon", end:2, group:"liberar_leon/0", id:1, start:1, title:"Fluent liberar_leon initiated at 1<br/>and terminated at transition to 2"}, _2594608{content:"salvar_leon", end:2, group:"salvar_leon/0", id:2, start:1, title:"Fluent salvar_leon initiated at 1<br/>and terminated at transition to 2"}, _2594730{content:"correr", group:"action", id:3, start:2, style:"color:green", title:"happens(correr,1,2)", type:"point"}, _2594856{content:"utilizar_dientes", group:"action", id:4, start:2, style:"color:green", title:"happens(utilizar_dientes,1,2)", type:"point"}, _2594982{content:"precavido", group:"action", id:5, start:2, style:"color:green", title:"happens(precavido,1,2)", type:"point"}, _2595108{content:"precavido", group:"action", id:6, start:3, style:"color:green", title:"happens(precavido,2,3)", type:"point"}, _2595234{content:"utilizar_dientes", group:"action", id:7, start:3, style:"color:green", title:"happens(utilizar_dientes,2,3)", type:"point"}, _2595360{content:"correr", group:"action", id:8, start:3, style:"color:green", title:"happens(correr,2,3)", type:"point"}]}, [])).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'farmer, wolf, goat and cabbage.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'farmer, wolf, goat and cabbage.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/farmer, wolf, goat and cabbage.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'farmer, wolf, goat and cabbage.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'farmer, wolf, goat and cabbage.pl'), lps= /.../(lps_user_examples, 'farmer, wolf, goat and cabbage.pl'), using= /.../(lps_user_examples, 'farmer, wolf, goat and cabbage.pl')].
% continue_lps_dialect.
% ops.

% LPS:  actions(transport(_2636060,_2636062,_2636064)).
% Into: actions([transport(_2636060,_2636062,_2636064)]).

% LPS:  fluents(loc(_2637120,_2637122)).
% Into: fluents([loc(_2637120,_2637122)]).

% LPS:  initially((loc(wolf,south),loc(goat,south),loc(cabbage,south),loc(farmer,south))).
% Into: initial_state([loc(wolf,south),loc(goat,south),loc(cabbage,south),loc(farmer,south)]).

% LPS:  then(if((loc(_2639592,south),_2639592\=farmer)),from(makeLoc(_2639592,north),to(_2639806,_2639808))).
% Into: reactive_rule([holds(loc(_2639592,south),_2640962),_2639592\=farmer],[happens(makeLoc(_2639592,north),_2639806,_2639808)]).

% LPS:  if(from(makeLoc(_2641968,north),to(_2642006,_2642008)),(_2641968\=farmer,from(makeLoc(farmer,south),to(_2642006,_2642224)),at(loc(_2641968,south),_2642224),from(transport(_2641968,south,north),to(_2642224,_2642008)))).
% Into: l_events(happens(makeLoc(_2641968,north),_2642006,_2642008),[_2641968\=farmer,happens(makeLoc(farmer,south),_2642006,_2642224),holds(loc(_2641968,south),_2642224),happens(transport(_2641968,south,north),_2642224,_2642008)]).

% LPS:  if(from(makeLoc(farmer,_2644676),to(_2644712,_2644712)),at(loc(farmer,_2644676),_2644712)).
% Into: l_events(happens(makeLoc(farmer,_2644676),_2644712,_2644712),[holds(loc(farmer,_2644676),_2644712)]).

% LPS:  if(from(makeLoc(farmer,_2646004),to(_2646040,_2646042)),(at(loc(farmer,_2646156),_2646040),_2646004\=_2646156,at(loc(_2646322,_2646156),_2646040),_2646322\=farmer,from(transport(_2646322,_2646156,_2646004),to(_2646040,_2646042)))).
% Into: l_events(happens(makeLoc(farmer,_2646004),_2646040,_2646042),[holds(loc(farmer,_2646156),_2646040),_2646004\=_2646156,holds(loc(_2646322,_2646156),_2646040),_2646322\=farmer,happens(transport(_2646322,_2646156,_2646004),_2646040,_2646042)]).

% LPS:  if(from(makeLoc(farmer,_2648724),to(_2648760,_2648762)),(at(loc(farmer,_2648876),_2648760),_2648724\=_2648876,from(transport(farmer,_2648876,_2648724),to(_2648760,_2648762)))).
% Into: l_events(happens(makeLoc(farmer,_2648724),_2648760,_2648762),[holds(loc(farmer,_2648876),_2648760),_2648724\=_2648876,happens(transport(farmer,_2648876,_2648724),_2648760,_2648762)]).

% LPS:  updates(transport(_2650828,_2650830,_2650832),in(to(_2650830,_2650832),loc(_2650828,_2650830))).
% Into: updated(happens(transport(_2650828,_2650830,_2650832),_2652184,_2652190),loc(_2650828,_2650830),_2650830-_2650832,[]).

% LPS:  updates(transport(_2652098,_2652100,_2652102),in(to(_2652100,_2652102),loc(farmer,_2652100))).
% Into: updated(happens(transport(_2652098,_2652100,_2652102),_2653454,_2653460),loc(farmer,_2652100),_2652100-_2652102,[]).

% LPS:  false((transport(_2653376,_2653378,_2653380),transport(_2653448,_2653378,_2653380),_2653376\=_2653448)).
% Into: d_pre([happens(transport(_2653376,_2653378,_2653380),_2654656,_2654662),happens(transport(_2653448,_2653378,_2653380),_2654656,_2654662),_2653376\=_2653448]).

% LPS:  false((loc(goat,_2655070),loc(wolf,_2655070),not(loc(farmer,_2655070)))).
% Into: d_pre([holds(loc(goat,_2655070),_2656318),holds(loc(wolf,_2655070),_2656318),holds(not(loc(farmer,_2655070)),_2656318)]).

% LPS:  false((loc(goat,_2656362),loc(cabbage,_2656362),not(loc(farmer,_2656362)))).
% Into: d_pre([holds(loc(goat,_2656362),_2657610),holds(loc(cabbage,_2656362),_2657610),holds(not(loc(farmer,_2656362)),_2657610)]).
% /pack/logicmoo_ec/test/lps_user_examples/farmer, wolf, goat and cabbage.pl:73
% pop_lps_dialect('$BLOB'("<stream>(0x562ef73e5500)"),  (/.../(lps_user_examples, 'farmer, wolf, goat and cabbage.pl')-> /.../(lps_user_examples, 'farmer, wolf, goat and cabbage.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/farmer, wolf, goat and cabbage.pl':_2665300).


d_pre([happens(transport(A, B, C), D, E), happens(transport(F, B, C), D, E), A\=F]).
d_pre([holds(loc(goat, A), B), holds(loc(wolf, A), B), holds(not(loc(farmer, A)), B)]).
d_pre([holds(loc(goat, A), B), holds(loc(cabbage, A), B), holds(not(loc(farmer, A)), B)]).

fluents([loc(_, _)]).

reactive_rule([holds(loc(A, south), _), A\=farmer], [happens(makeLoc(A, north), _, _)]).

initial_state([loc(wolf, south), loc(goat, south), loc(cabbage, south), loc(farmer, south)]).

l_events(happens(makeLoc(A, north), B, C), [A\=farmer, happens(makeLoc(farmer, south), B, D), holds(loc(A, south), D), happens(transport(A, south, north), D, C)]).
l_events(happens(makeLoc(farmer, A), B, B), [holds(loc(farmer, A), B)]).
l_events(happens(makeLoc(farmer, A), B, C), [holds(loc(farmer, D), B), A\=D, holds(loc(E, D), B), E\=farmer, happens(transport(E, D, A), B, C)]).
l_events(happens(makeLoc(farmer, A), B, C), [holds(loc(farmer, D), B), A\=D, happens(transport(farmer, D, A), B, C)]).

:- dynamic actions/1.
:- multifile actions/1.

actions([transport(_, _, _)]).

updated(happens(transport(A, B, C), _, _), loc(A, B), B-C, []).
updated(happens(transport(_, A, B), _, _), loc(farmer, A), A-B, []).

maxTime(10).
ERROR: LPS: execution timeout(resolveAndUpdate)
PROGRAM FAILED
% dB(/.../(lps_user_examples, 'farmer, wolf, goat and cabbage.pl'), lps_visualization(_73390{groups:[_72854{content:"loc(A,B)", id:"loc/2", order:3, subgroupStack:"false"}], items:[_72980{content:"cabbage,south", end:2, group:"loc/2", id:0, start:1, subgroup:"cabbage", title:"Fluent loc(cabbage,south) initiated at 1<br/>and terminated at transition to 2"}, _73106{content:"farmer,south", end:2, group:"loc/2", id:1, start:1, subgroup:"farmer", title:"Fluent loc(farmer,south) initiated at 1<br/>and terminated at transition to 2"}, _73232{content:"goat,south", end:2, group:"loc/2", id:2, start:1, subgroup:"goat", title:"Fluent loc(goat,south) initiated at 1<br/>and terminated at transition to 2"}, _73358{content:"wolf,south", end:2, group:"loc/2", id:3, start:1, subgroup:"wolf", title:"Fluent loc(wolf,south) initiated at 1<br/>and terminated at transition to 2"}]}, [])).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'faultymartianrobot.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'faultymartianrobot.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/faultymartianrobot.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'faultymartianrobot.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'faultymartianrobot.pl'), lps= /.../(lps_user_examples, 'faultymartianrobot.pl'), using= /.../(lps_user_examples, 'faultymartianrobot.pl')].
% continue_lps_dialect.
% ops.

% LPS:  fluents((at_pos/2,free/2,visited/2,obstacle/2,life/2,lookingtowards/2)).
% Into: fluents([at_pos(_21426,_21428),free(_21438,_21440),visited(_21450,_21452),obstacle(_21462,_21464),life(_21474,_21476),lookingtowards(_21486,_21488)]).

% LPS:  actions((step(_21226,_21228),turn_right,report)).
% Into: actions([step(_21226,_21228),turn_right,report]).

% LPS:  initially((at_pos(0,0),life(2,1),free(1,0),free(2,0),obstacle(3,0),obstacle(2,-1),obstacle(2,1),lookingtowards(1,0))).
% Into: initial_state([at_pos(0,0),life(2,1),free(1,0),free(2,0),obstacle(3,0),obstacle(2,-1),obstacle(2,1),lookingtowards(1,0)]).

% LPS:  terminates(step(_24138,_24140),lookingtowards(_24138,_24140)).
% Into: terminated(happens(step(_24138,_24140),_25324,_25330),lookingtowards(_24138,_24140),[]).

% LPS:  if(initiates(step(_25270,_25272),lookingtowards(_25270,_25328)),(at_pos(_25270,_25416),lookingtowards(_25270,_25272),_25556 is _25272-_25416,abs(_25556)>0,_25328 is _25272+_25556)).
% Into: initiated(happens(step(_25270,_25272),_27170,_27176),lookingtowards(_25270,_25328),[holds(at_pos(_25270,_25416),_27170),holds(lookingtowards(_25270,_25272),_27170),_25556 is _25272-_25416,abs(_25556)>0,_25328 is _25272+_25556]).

% LPS:  if(initiates(step(_28658,_28660),lookingtowards(_28714,_28660)),(at_pos(_28802,_28660),lookingtowards(_28658,_28660),_28944 is _28658-_28802,abs(_28944)>0,_28714 is _28658+_28944)).
% Into: initiated(happens(step(_28658,_28660),_30558,_30564),lookingtowards(_28714,_28660),[holds(at_pos(_28802,_28660),_30558),holds(lookingtowards(_28658,_28660),_30558),_28944 is _28658-_28802,abs(_28944)>0,_28714 is _28658+_28944]).

% LPS:  initiates(step(_32046,_32048),at_pos(_32046,_32048)).
% Into: initiated(happens(step(_32046,_32048),_33232,_33238),at_pos(_32046,_32048),[]).

% LPS:  if(terminates(step(_33178,_33180),at_pos(_33234,_33236)),at_pos(_33234,_33236)).
% Into: terminated(happens(step(_33178,_33180),_34506,_34512),at_pos(_33234,_33236),[holds(at_pos(_33234,_33236),_34506)]).

% LPS:  terminates(step(_34798,_34800),free(_34798,_34800)).
% Into: terminated(happens(step(_34798,_34800),_35984,_35990),free(_34798,_34800),[]).

% LPS:  if(initiates(step(_35930,_35932),visited(_35986,_35988)),at_pos(_35986,_35988)).
% Into: initiated(happens(step(_35930,_35932),_37258,_37264),visited(_35986,_35988),[holds(at_pos(_35986,_35988),_37258)]).

% LPS:  if(initiates(step(_37518,_37520),free(_37574,_37576)),at_pos(_37574,_37576)).
% Into: initiated(happens(step(_37518,_37520),_38846,_38852),free(_37574,_37576),[holds(at_pos(_37574,_37576),_38846)]).

% LPS:  terminates(turn_right,lookingtowards(_39122,_39124)).
% Into: terminated(happens(turn_right,_40248,_40254),lookingtowards(_39122,_39124),[]).

% LPS:  if(initiates(turn_right,lookingtowards(_40208,_40210)),(at_pos(_40208,_40298),lookingtowards(_40352,_40298),_40352 is _40208+1,_40210 is _40298-1)).
% Into: initiated(happens(turn_right,_41892,_41898),lookingtowards(_40208,_40210),[holds(at_pos(_40208,_40298),_41892),holds(lookingtowards(_40352,_40298),_41892),_40352 is _40208+1,_40210 is _40298-1]).

% LPS:  if(initiates(turn_right,lookingtowards(_43048,_43050)),(at_pos(_43136,_43050),lookingtowards(_43136,_43194),_43048 is _43136-1,_43194 is _43050-1)).
% Into: initiated(happens(turn_right,_44732,_44738),lookingtowards(_43048,_43050),[holds(at_pos(_43136,_43050),_44732),holds(lookingtowards(_43136,_43194),_44732),_43048 is _43136-1,_43194 is _43050-1]).

% LPS:  if(initiates(turn_right,lookingtowards(_45888,_45890)),(at_pos(_45888,_45978),lookingtowards(_46032,_45978),_46032 is _45888-1,_45890 is _45978+1)).
% Into: initiated(happens(turn_right,_47572,_47578),lookingtowards(_45888,_45890),[holds(at_pos(_45888,_45978),_47572),holds(lookingtowards(_46032,_45978),_47572),_46032 is _45888-1,_45890 is _45978+1]).

% LPS:  if(initiates(turn_right,lookingtowards(_48728,_48730)),(at_pos(_48816,_48730),lookingtowards(_48816,_48874),_48728 is _48816+1,_48874 is _48730+1)).
% Into: initiated(happens(turn_right,_50412,_50418),lookingtowards(_48728,_48730),[holds(at_pos(_48816,_48730),_50412),holds(lookingtowards(_48816,_48874),_50412),_48728 is _48816+1,_48874 is _48730+1]).

% LPS:  false((visited(_51630,_51632),step(_51630,_51632))).
% Into: d_pre([holds(visited(_51630,_51632),_52756),happens(step(_51630,_51632),_52756,_52762)]).

% LPS:  false((life(_53030,_53032),lookingtowards(_53030,_53032),turn_right)).
% Into: d_pre([holds(life(_53030,_53032),_54210),holds(lookingtowards(_53030,_53032),_54210),happens(turn_right,_54210,_54216)]).

% LPS:  then(if((lookingtowards(_54726,_54728),free(_54726,_54728))),step(_54726,_54728)).
% Into: reactive_rule([holds(lookingtowards(_54726,_54728),_55968),holds(free(_54726,_54728),_55968)],[happens(step(_54726,_54728),_56562,_56568)]).

% LPS:  then(if((lookingtowards(_56634,_56636),free(_56634,_56636),visited(_56634,_56636))),turn_right).
% Into: reactive_rule([holds(lookingtowards(_56634,_56636),_57930),holds(free(_56634,_56636),_57930),holds(visited(_56634,_56636),_57930)],[happens(turn_right,_58728,_58734)]).

% LPS:  then(if((lookingtowards(_58832,_58834),obstacle(_58832,_58834))),turn_right).
% Into: reactive_rule([holds(lookingtowards(_58832,_58834),_60028),holds(obstacle(_58832,_58834),_60028)],[happens(turn_right,_60622,_60628)]).

% LPS:  then(if((lookingtowards(_60694,_60696),obstacle(_60694,_60696),life(_60694,_60696))),report).
% Into: reactive_rule([holds(lookingtowards(_60694,_60696),_61990),holds(obstacle(_60694,_60696),_61990),holds(life(_60694,_60696),_61990)],[happens(report,_62788,_62794)]).
% /pack/logicmoo_ec/test/lps_user_examples/faultymartianrobot.pl:73
% pop_lps_dialect('$BLOB'("<stream>(0x562ef73e3500)"),  (/.../(lps_user_examples, 'faultymartianrobot.pl')-> /.../(lps_user_examples, 'faultymartianrobot.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/faultymartianrobot.pl':_70686).


initiated(happens(step(A, B), C, _), lookingtowards(A, D), [holds(at_pos(A, E), C), holds(lookingtowards(A, B), C), F is B-E, abs(F)>0, D is B+F]).
initiated(happens(step(A, B), C, _), lookingtowards(D, B), [holds(at_pos(E, B), C), holds(lookingtowards(A, B), C), F is A-E, abs(F)>0, D is A+F]).
initiated(happens(step(A, B), _, _), at_pos(A, B), []).
initiated(happens(step(_, _), A, _), visited(B, C), [holds(at_pos(B, C), A)]).
initiated(happens(step(_, _), A, _), free(B, C), [holds(at_pos(B, C), A)]).
initiated(happens(turn_right, A, _), lookingtowards(B, C), [holds(at_pos(B, D), A), holds(lookingtowards(E, D), A), E is B+1, C is D-1]).
initiated(happens(turn_right, A, _), lookingtowards(B, C), [holds(at_pos(D, C), A), holds(lookingtowards(D, E), A), B is D-1, E is C-1]).
initiated(happens(turn_right, A, _), lookingtowards(B, C), [holds(at_pos(B, D), A), holds(lookingtowards(E, D), A), E is B-1, C is D+1]).
initiated(happens(turn_right, A, _), lookingtowards(B, C), [holds(at_pos(D, C), A), holds(lookingtowards(D, E), A), B is D+1, E is C+1]).

d_pre([holds(visited(A, B), C), happens(step(A, B), C, _)]).
d_pre([holds(life(A, B), C), holds(lookingtowards(A, B), C), happens(turn_right, C, _)]).

fluents([at_pos(_, _), free(_, _), visited(_, _), obstacle(_, _), life(_, _), lookingtowards(_, _)]).

terminated(happens(step(A, B), _, _), lookingtowards(A, B), []).
terminated(happens(step(_, _), A, _), at_pos(B, C), [holds(at_pos(B, C), A)]).
terminated(happens(step(A, B), _, _), free(A, B), []).
terminated(happens(turn_right, _, _), lookingtowards(_, _), []).

reactive_rule([holds(lookingtowards(A, B), C), holds(free(A, B), C)], [happens(step(A, B), _, _)]).
reactive_rule([holds(lookingtowards(A, B), C), holds(free(A, B), C), holds(visited(A, B), C)], [happens(turn_right, _, _)]).
reactive_rule([holds(lookingtowards(A, B), C), holds(obstacle(A, B), C)], [happens(turn_right, _, _)]).
reactive_rule([holds(lookingtowards(A, B), C), holds(obstacle(A, B), C), holds(life(A, B), C)], [happens(report, _, _)]).

initial_state([at_pos(0, 0), life(2, 1), free(1, 0), free(2, 0), obstacle(3, 0), obstacle(2, -1), obstacle(2, 1), lookingtowards(1, 0)]).

:- dynamic actions/1.
:- multifile actions/1.

actions([step(_, _), turn_right, report]).

maxTime(10).
% dB(/.../(lps_user_examples, 'faultymartianrobot.pl'), lps_visualization(_106208{groups:[_102196{content:"at_pos(A,B)", id:"at_pos/2", order:3, subgroupStack:"false"}, _102274{content:"free(A,B)", id:"free/2", order:3, subgroupStack:"false"}, _102352{content:"life(A,B)", id:"life/2", order:3, subgroupStack:"false"}, _102430{content:"lookingtowards(A,B)", id:"lookingtowards/2", order:3, subgroupStack:"false"}, _102508{content:"obstacle(A,B)", id:"obstacle/2", order:3, subgroupStack:"false"}, _102586{content:"visited(A,B)", id:"visited/2", order:3, subgroupStack:"false"}, _102652{content:"Actions", id:"action", order:4}], items:[_102774{content:"0,0", end:2, group:"at_pos/2", id:0, start:1, subgroup:"0", title:"Fluent at_pos(0,0) initiated at 1<br/>and terminated at transition to 2"}, _102900{content:"1,0", end:3, group:"at_pos/2", id:1, start:2, subgroup:"1", title:"Fluent at_pos(1,0) initiated at 2<br/>and terminated at transition to 3"}, _103026{content:"2,0", end:11, group:"at_pos/2", id:2, start:3, subgroup:"2", title:"Fluent at_pos(2,0) initiated at 3<br/>and terminated at transition to 11"}, _103152{content:"0,0", end:11, group:"free/2", id:3, start:2, subgroup:"0", title:"Fluent free(0,0) initiated at 2<br/>and terminated at transition to 11"}, _103278{content:"1,0", end:2, group:"free/2", id:4, start:1, subgroup:"1", title:"Fluent free(1,0) initiated at 1<br/>and terminated at transition to 2"}, _103404{content:"1,0", end:11, group:"free/2", id:5, start:3, subgroup:"1", title:"Fluent free(1,0) initiated at 3<br/>and terminated at transition to 11"}, _103530{content:"2,0", end:3, group:"free/2", id:6, start:1, subgroup:"2", title:"Fluent free(2,0) initiated at 1<br/>and terminated at transition to 3"}, _103656{content:"2,1", end:11, group:"life/2", id:7, start:1, subgroup:"2", title:"Fluent life(2,1) initiated at 1<br/>and terminated at transition to 11"}, _103782{content:"1,0", end:2, group:"lookingtowards/2", id:8, start:1, subgroup:"1", title:"Fluent lookingtowards(1,0) initiated at 1<br/>and terminated at transition to 2"}, _103908{content:"1,0", end:6, group:"lookingtowards/2", id:9, start:5, subgroup:"1", title:"Fluent lookingtowards(1,0) initiated at 5<br/>and terminated at transition to 6"}, _104034{content:"2,-1", end:5, group:"lookingtowards/2", id:10, start:4, subgroup:"2", title:"Fluent lookingtowards(2,-1) initiated at 4<br/>and terminated at transition to 5"}, _104160{content:"2,0", end:3, group:"lookingtowards/2", id:11, start:2, subgroup:"2", title:"Fluent lookingtowards(2,0) initiated at 2<br/>and terminated at transition to 3"}, _104286{content:"2,1", end:11, group:"lookingtowards/2", id:12, start:6, subgroup:"2", title:"Fluent lookingtowards(2,1) initiated at 6<br/>and terminated at transition to 11"}, _104412{content:"3,0", end:4, group:"lookingtowards/2", id:13, start:3, subgroup:"3", title:"Fluent lookingtowards(3,0) initiated at 3<br/>and terminated at transition to 4"}, _104538{content:"2,-1", end:11, group:"obstacle/2", id:14, start:1, subgroup:"2", title:"Fluent obstacle(2,-1) initiated at 1<br/>and terminated at transition to 11"}, _104664{content:"2,1", end:11, group:"obstacle/2", id:15, start:1, subgroup:"2", title:"Fluent obstacle(2,1) initiated at 1<br/>and terminated at transition to 11"}, _104790{content:"3,0", end:11, group:"obstacle/2", id:16, start:1, subgroup:"3", title:"Fluent obstacle(3,0) initiated at 1<br/>and terminated at transition to 11"}, _104916{content:"0,0", end:11, group:"visited/2", id:17, start:2, subgroup:"0", title:"Fluent visited(0,0) initiated at 2<br/>and terminated at transition to 11"}, _105042{content:"1,0", end:11, group:"visited/2", id:18, start:3, subgroup:"1", title:"Fluent visited(1,0) initiated at 3<br/>and terminated at transition to 11"}, _105168{content:"step(1,0)", group:"action", id:19, start:2, style:"color:green", title:"happens(step(1,0),1,2)", type:"point"}, _105294{content:"step(2,0)", group:"action", id:20, start:3, style:"color:green", title:"happens(step(2,0),2,3)", type:"point"}, _105420{content:"turn_right", group:"action", id:21, start:4, style:"color:green", title:"happens(turn_right,3,4)", type:"point"}, _105546{content:"turn_right", group:"action", id:22, start:5, style:"color:green", title:"happens(turn_right,4,5)", type:"point"}, _105672{content:"turn_right", group:"action", id:23, start:6, style:"color:green", title:"happens(turn_right,5,6)", type:"point"}, _105798{content:"report", group:"action", id:24, start:7, style:"color:green", title:"happens(report,6,7)", type:"point"}, _105924{content:"report", group:"action", id:25, start:8, style:"color:green", title:"happens(report,7,8)", type:"point"}, _106050{content:"report", group:"action", id:26, start:9, style:"color:green", title:"happens(report,8,9)", type:"point"}, _106176{content:"report", group:"action", id:27, start:10, style:"color:green", title:"happens(report,9,10)", type:"point"}]}, [])).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'faultypurereacting.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'faultypurereacting.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/faultypurereacting.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'faultypurereacting.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'faultypurereacting.pl'), lps= /.../(lps_user_examples, 'faultypurereacting.pl'), using= /.../(lps_user_examples, 'faultypurereacting.pl')].
% continue_lps_dialect.
% ops.

% LPS:  events(alguien_me_ataca).
% Into: events([alguien_me_ataca]).

% LPS:  actions((responde_igual,consigue_ayuda,trata_de_escapar)).
% Into: actions([responde_igual,consigue_ayuda,trata_de_escapar]).

% LPS:  observe(from(alguien_me_ataca,to(1,2))).
% Into: observe([alguien_me_ataca],2).

% LPS:  then(if(from(alguien_me_ataca,to(_64324,_64326))),from(responde_igual,to(_64326,_64462))).
% Into: reactive_rule([happens(alguien_me_ataca,_64324,_64326)],[happens(responde_igual,_64326,_64462)]).

% LPS:  then(if(from(alguien_me_ataca,to(_65668,_65670))),from(consigue_ayuda,to(_65670,_65806))).
% Into: reactive_rule([happens(alguien_me_ataca,_65668,_65670)],[happens(consigue_ayuda,_65670,_65806)]).

% LPS:  then(if(from(alguien_me_ataca,to(_67012,_67014))),from(trata_de_escapar,to(_67014,_67150))).
% Into: reactive_rule([happens(alguien_me_ataca,_67012,_67014)],[happens(trata_de_escapar,_67014,_67150)]).

% LPS:  if(from(responde_igual,to(_68308,_68310)),from(consigue_ayuda,to(_68308,_68310))).
% Into: l_events(happens(responde_igual,_68308,_68310),[happens(consigue_ayuda,_68308,_68310)]).

% LPS:  if(from(trata_de_escapar,to(_69564,_69566)),from(consigue_ayuda,to(_69564,_69566))).
% Into: l_events(happens(trata_de_escapar,_69564,_69566),[happens(consigue_ayuda,_69564,_69566)]).
% /pack/logicmoo_ec/test/lps_user_examples/faultypurereacting.pl:27
% pop_lps_dialect('$BLOB'("<stream>(0x562ef6885800)"),  (/.../(lps_user_examples, 'faultypurereacting.pl')-> /.../(lps_user_examples, 'faultypurereacting.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/faultypurereacting.pl':_78460).


reactive_rule([happens(alguien_me_ataca, _, A)], [happens(responde_igual, A, _)]).
reactive_rule([happens(alguien_me_ataca, _, A)], [happens(consigue_ayuda, A, _)]).
reactive_rule([happens(alguien_me_ataca, _, A)], [happens(trata_de_escapar, A, _)]).

l_events(happens(responde_igual, A, B), [happens(consigue_ayuda, A, B)]).
l_events(happens(trata_de_escapar, A, B), [happens(consigue_ayuda, A, B)]).

:- dynamic actions/1.
:- multifile actions/1.

actions([responde_igual, consigue_ayuda, trata_de_escapar]).

events([alguien_me_ataca]).

observe([alguien_me_ataca], 2).

maxTime(10).
% dB(/.../(lps_user_examples, 'faultypurereacting.pl'), lps_visualization(_49590{groups:[_49248{content:"Events", id:"event", order:1}, _49310{content:"Actions", id:"action", order:4}], items:[_49432{content:"alguien_me_ataca", group:"event", id:0, start:2, style:"color:#E19735", title:"happens(alguien_me_ataca,1,2)", type:"point"}, _49558{content:"consigue_ayuda", group:"action", id:1, start:3, style:"color:green", title:"happens(consigue_ayuda,2,3)", type:"point"}]}, [])).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'foxonegoal.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'foxonegoal.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/foxonegoal.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'foxonegoal.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'foxonegoal.pl'), lps= /.../(lps_user_examples, 'foxonegoal.pl'), using= /.../(lps_user_examples, 'foxonegoal.pl')].
% continue_lps_dialect.
% ops.

% LPS:  fluents((has(_19622,_19624),near(_19622,_19624))).
% Into: fluents([has(_19622,_19624),near(_19622,_19624)]).

% LPS:  actions((pray/2,takes/2,eats/2,gets/2)).
% Into: actions([pray(_22282,_22284),takes(_22294,_22296),eats(_22306,_22308),gets(_22318,_22320)]).

% LPS:  events((hunger/1,sings/1,finds/2)).
% Into: events([hunger(_23508),sings(_23518),finds(_23528,_23530)]).

% LPS:  initially(has(crow,cheese)).
% Into: initial_state([has(crow,cheese)]).

% LPS:  observe(from(hunger(me),to(1,2))).
% Into: observe([hunger(me)],2).

% LPS:  if(initiates(takes(_25604,_25606),has(_25604,_25606)),near(_25604,_25606)).
% Into: initiated(happens(takes(_25604,_25606),_26916,_26922),has(_25604,_25606),[holds(near(_25604,_25606),_26916)]).

% LPS:  if(terminates(takes(_27176,_27178),has(_27232,_27178)),_27232\==_27176).
% Into: terminated(happens(takes(_27176,_27178),_28500,_28506),has(_27232,_27178),[_27232\==_27176]).

% LPS:  if(initiates(sings(crow),near(me,cheese)),has(crow,cheese)).
% Into: initiated(happens(sings(crow),_30156,_30162),near(me,cheese),[holds(has(crow,cheese),_30156)]).

% LPS:  if(sings(crow),pray(me,crow)).
% Into: l_events(happens(sings(crow),_31474,_31480),[happens(pray(me,crow),_31474,_31480)]).

% LPS:  if(gets(me,cheese),(has(crow,cheese),sings(crow))).
% Into: l_events(happens(gets(me,cheese),_32904,_32910),[holds(has(crow,cheese),_32904),happens(sings(crow),_32904,_32910)]).

% LPS:  if(finds(_32936,_32938),(gets(_32936,_32938),takes(_32936,_32938),eats(_32936,_32938))).
% Into: l_events(happens(finds(_32936,_32938),_34236,_34242),[happens(gets(_32936,_32938),_34236,_34316),happens(takes(_32936,_32938),_34316,_34458),happens(eats(_32936,_32938),_34458,_34242)]).

% LPS:  then(if(from(hunger(me),to(_34298,_34300))),from(finds(me,cheese),to(_34300,_34476))).
% Into: reactive_rule([happens(hunger(me),_34298,_34300)],[happens(finds(me,cheese),_34300,_34476)]).
% /pack/logicmoo_ec/test/lps_user_examples/foxonegoal.pl:30
% pop_lps_dialect('$BLOB'("<stream>(0x562ef6c54000)"),  (/.../(lps_user_examples, 'foxonegoal.pl')-> /.../(lps_user_examples, 'foxonegoal.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/foxonegoal.pl':_43272).


initiated(happens(takes(A, B), C, _), has(A, B), [holds(near(A, B), C)]).
initiated(happens(sings(crow), A, _), near(me, cheese), [holds(has(crow, cheese), A)]).

fluents([has(A, B), near(A, B)]).

terminated(happens(takes(A, B), _, _), has(C, B), [C\==A]).

reactive_rule([happens(hunger(me), _, A)], [happens(finds(me, cheese), A, _)]).

initial_state([has(crow, cheese)]).

l_events(happens(sings(crow), A, B), [happens(pray(me, crow), A, B)]).
l_events(happens(gets(me, cheese), A, B), [holds(has(crow, cheese), A), happens(sings(crow), A, B)]).
l_events(happens(finds(A, B), C, D), [happens(gets(A, B), C, E), happens(takes(A, B), E, F), happens(eats(A, B), F, D)]).

:- dynamic actions/1.
:- multifile actions/1.

actions([pray(_, _), takes(_, _), eats(_, _), gets(_, _)]).

events([hunger(_), sings(_), finds(_, _)]).

observe([hunger(me)], 2).

maxTime(6).
% dB(/.../(lps_user_examples, 'foxonegoal.pl'), lps_visualization(_81774{groups:[_80646{content:"Events", id:"event", order:1}, _80720{content:"has(A,B)", id:"has/2", order:3, subgroupStack:"false"}, _80798{content:"near(A,B)", id:"near/2", order:3, subgroupStack:"false"}, _80864{content:"Actions", id:"action", order:4}], items:[_80986{content:"crow,cheese", end:4, group:"has/2", id:0, start:1, subgroup:"crow", title:"Fluent has(crow,cheese) initiated at 1<br/>and terminated at transition to 4"}, _81112{content:"me,cheese", end:7, group:"has/2", id:1, start:4, subgroup:"me", title:"Fluent has(me,cheese) initiated at 4<br/>and terminated at transition to 7"}, _81238{content:"me,cheese", end:7, group:"near/2", id:2, start:3, subgroup:"me", title:"Fluent near(me,cheese) initiated at 3<br/>and terminated at transition to 7"}, _81364{content:"hunger(me)", group:"event", id:3, start:2, style:"color:#E19735", title:"happens(hunger(me),1,2)", type:"point"}, _81490{content:"pray(me,crow)", group:"action", id:4, start:3, style:"color:green", title:"happens(pray(me,crow),2,3)", type:"point"}, _81616{content:"takes(me,cheese)", group:"action", id:5, start:4, style:"color:green", title:"happens(takes(me,cheese),3,4)", type:"point"}, _81742{content:"eats(me,cheese)", group:"action", id:6, start:5, style:"color:green", title:"happens(eats(me,cheese),4,5)", type:"point"}]}, [])).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'GDPR compliance - article 46.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'GDPR compliance - article 46.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/GDPR compliance - article 46.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'GDPR compliance - article 46.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'GDPR compliance - article 46.pl'), lps= /.../(lps_user_examples, 'GDPR compliance - article 46.pl'), using= /.../(lps_user_examples, 'GDPR compliance - article 46.pl')].
% continue_lps_dialect.
% ops.
ERROR: /pack/logicmoo_ec/test/lps_user_examples/GDPR compliance - article 46.pl:3:
ERROR:    dynamic/1: Type error: `predicate_indicator' expected, found `located(_57606,thirdCountry)' (a compound)
ERROR: /pack/logicmoo_ec/test/lps_user_examples/GDPR compliance - article 46.pl:3:
ERROR:    dynamic/1: Type error: `predicate_indicator' expected, found `decisionPursuantToArticle_45_3(_58440)' (a compound)
ERROR: /pack/logicmoo_ec/test/lps_user_examples/GDPR compliance - article 46.pl:3:
ERROR:    dynamic/1: Type error: `predicate_indicator' expected, found `legallyBindingEnforceableInstrument(_59272)' (a compound)
ERROR: /pack/logicmoo_ec/test/lps_user_examples/GDPR compliance - article 46.pl:3:
ERROR:    dynamic/1: Type error: `predicate_indicator' expected, found `bindingCorporateRules(_60104)' (a compound)
ERROR: /pack/logicmoo_ec/test/lps_user_examples/GDPR compliance - article 46.pl:3:
ERROR:    dynamic/1: Type error: `predicate_indicator' expected, found `standardDataProtectionClauses(_60936)' (a compound)
ERROR: /pack/logicmoo_ec/test/lps_user_examples/GDPR compliance - article 46.pl:3:
ERROR:    dynamic/1: Type error: `predicate_indicator' expected, found `approvedCodeOfConduct(_61768)' (a compound)
ERROR: /pack/logicmoo_ec/test/lps_user_examples/GDPR compliance - article 46.pl:3:
ERROR:    dynamic/1: Type error: `predicate_indicator' expected, found `approvedCertificateMechanism(_62600)' (a compound)
% /pack/logicmoo_ec/test/lps_user_examples/GDPR compliance - article 46.pl:39
% pop_lps_dialect('$BLOB'("<stream>(0x562ef76d7400)"),  (/.../(lps_user_examples, 'GDPR compliance - article 46.pl')-> /.../(lps_user_examples, 'GDPR compliance - article 46.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/GDPR compliance - article 46.pl':_27418).


partOf(event001, process001).

located(companyA_US, thirdCountry).

legalRemediesForDataSubjects(process001).

enforceableDataSubjectRights(process001).

:- dynamic actions/1.
:- multifile actions/1.


appropriateSafeguards(Process) :-
    (   legallyBindingEnforceableInstrument(Process)
    ;   bindingCorporateRules(Process)
    ;   standardDataProtectionClauses(Process)
    ;   approvedCodeOfConduct(Process)
    ;   approvedCertificateMechanism(Process)
    ).

isa(event001, transfer(companyA_Ireland, data, companyA_US)).
isa(companyA_Ireland, controller).
isa(data, personalData).

permitted(A) :-
    isa(A, transfer(B, C, D)),
    not(decisionPursuantToArticle_45_3(A)),
    (   isa(B, controller)
    ;   isa(B, processor)
    ),
    isa(C, personalData),
    (   located(D, thirdCountry)
    ;   isa(D, internationalOrganisation)
    ),
    partOf(A, E),
    appropriateSafeguards(E),
    enforceableDataSubjectRights(E),
    legalRemediesForDataSubjects(E).
% dB(/.../(lps_user_examples, 'GDPR compliance - article 46.pl'), lps_visualization(_65828{groups:[], items:[]}, [])).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'Giomar Aquiles Salazar Molina.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'Giomar Aquiles Salazar Molina.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/Giomar Aquiles Salazar Molina.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'Giomar Aquiles Salazar Molina.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'Giomar Aquiles Salazar Molina.pl'), lps= /.../(lps_user_examples, 'Giomar Aquiles Salazar Molina.pl'), using= /.../(lps_user_examples, 'Giomar Aquiles Salazar Molina.pl')].
% continue_lps_dialect.
% ops.

% LPS:  fluents((alerta1,alerta2,alerta3)).
% Into: fluents([alerta1,alerta2,alerta3]).

% LPS:  actions(carta).
% Into: actions([carta]).

% LPS:  events((giroMensaje1,giroMensaje2,giroMensaje3,giroMensaje4)).
% Into: events([giroMensaje1,giroMensaje2,giroMensaje3,giroMensaje4]).

% LPS:  observe(from(alerta1,to(1,2))).
% Into: observe([alerta1],2).

% LPS:  then(if(at(alerta1,_36410)),from(giroMensaje1,to(_36410,_36514))).
% Into: reactive_rule([holds(alerta1,_36410)],[happens(giroMensaje1,_36410,_36514)]).

% LPS:  observe(from(alerta2,to(3,4))).
% Into: observe([alerta2],4).

% LPS:  then(if(at(alerta2,_39188)),from(giroMensaje2,to(_39188,_39292))).
% Into: reactive_rule([holds(alerta2,_39188)],[happens(giroMensaje2,_39188,_39292)]).

% LPS:  observe(from(alerta2,to(5,6))).
% Into: observe([alerta2],6).

% LPS:  then(if(at(alerta3,_41966)),from(giroMensaje3,to(_41966,_42070))).
% Into: reactive_rule([holds(alerta3,_41966)],[happens(giroMensaje3,_41966,_42070)]).

% LPS:  observe(from(carta,to(3,5))).
% Into: observe([carta],5).

% LPS:  if(from(giroMensaje4,to(_44750,_44752)),from(carta,to(_44750,_44752))).
% Into: l_events(happens(giroMensaje4,_44750,_44752),[happens(carta,_44750,_44752)]).
% /pack/logicmoo_ec/test/lps_user_examples/Giomar Aquiles Salazar Molina.pl:30
% pop_lps_dialect('$BLOB'("<stream>(0x562ef76d9700)"),  (/.../(lps_user_examples, 'Giomar Aquiles Salazar Molina.pl')-> /.../(lps_user_examples, 'Giomar Aquiles Salazar Molina.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/Giomar Aquiles Salazar Molina.pl':_53620).


fluents([alerta1, alerta2, alerta3]).

reactive_rule([holds(alerta1, A)], [happens(giroMensaje1, A, _)]).
reactive_rule([holds(alerta2, A)], [happens(giroMensaje2, A, _)]).
reactive_rule([holds(alerta3, A)], [happens(giroMensaje3, A, _)]).

l_events(happens(giroMensaje4, A, B), [happens(carta, A, B)]).

:- dynamic actions/1.
:- multifile actions/1.

actions([carta]).

events([giroMensaje1, giroMensaje2, giroMensaje3, giroMensaje4]).

observe([alerta1], 2).
observe([alerta2], 4).
observe([alerta2], 6).
observe([carta], 5).

maxTime(20).
% dB(/.../(lps_user_examples, 'Giomar Aquiles Salazar Molina.pl'), lps_visualization(_59158{groups:[_58626{content:"Events", id:"event", order:1}], items:[_58748{content:"alerta1", group:"event", id:0, start:2, style:"color:#E19735", title:"happens(alerta1,1,2)", type:"point"}, _58874{content:"alerta2", group:"event", id:1, start:4, style:"color:#E19735", title:"happens(alerta2,3,4)", type:"point"}, _59000{content:"carta", group:"event", id:2, start:5, style:"color:#E19735", title:"happens(carta,4,5)", type:"point"}, _59126{content:"alerta2", group:"event", id:3, start:6, style:"color:#E19735", title:"happens(alerta2,5,6)", type:"point"}]}, [])).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'goat.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'goat.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/goat.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'goat.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'goat.pl'), lps= /.../(lps_user_examples, 'goat.pl'), using= /.../(lps_user_examples, 'goat.pl')].
% continue_lps_dialect.
% ops.

% LPS:  actions((row(_19604,_19606),transport(_19674,_19676,_19678))).
% Into: actions([row(_19604,_19606),transport(_19674,_19676,_19678)]).

% LPS:  fluents(loc(_20778,_20780)).
% Into: fluents([loc(_20778,_20780)]).

% LPS:  initially((loc(wolf,south),loc(goat,south),loc(cabbage,south),loc(farmer,south))).
% Into: initial_state([loc(wolf,south),loc(goat,south),loc(cabbage,south),loc(farmer,south)]).

% LPS:  then(if((loc(_23272,south),_23272\=farmer)),from(makeLoc(_23272,north),to(_23486,_23488))).
% Into: reactive_rule([holds(loc(_23272,south),_24642),_23272\=farmer],[happens(makeLoc(_23272,north),_23486,_23488)]).

% LPS:  if(from(makeLoc(_25436,_25438),to(_25474,_25476)),(_25436\=farmer,at(loc(_25436,_25654),_25474),_25438\=_25654,from(makeLoc(farmer,_25654),to(_25474,_25860)),from(row(_25654,_25438),to(_25860,_25476)),from(transport(_25436,_25654,_25438),to(_25860,_25476)))).
% Into: l_events(happens(makeLoc(_25436,_25438),_25474,_25476),[_25436\=farmer,holds(loc(_25436,_25654),_25474),_25438\=_25654,happens(makeLoc(farmer,_25654),_25474,_25860),happens(row(_25654,_25438),_25860,_25476),happens(transport(_25436,_25654,_25438),_25860,_25476)]).

% LPS:  if(from(makeLoc(farmer,_28802),to(_28838,_28838)),at(loc(farmer,_28802),_28838)).
% Into: l_events(happens(makeLoc(farmer,_28802),_28838,_28838),[holds(loc(farmer,_28802),_28838)]).

% LPS:  if(from(makeLoc(farmer,_30130),to(_30166,_30168)),(at(loc(farmer,_30282),_30166),_30130\=_30282,from(row(_30282,_30130),to(_30166,_30168)))).
% Into: l_events(happens(makeLoc(farmer,_30130),_30166,_30168),[holds(loc(farmer,_30282),_30166),_30130\=_30282,happens(row(_30282,_30130),_30166,_30168)]).

% LPS:  updates(transport(_32314,_32316,_32318),in(to(_32316,_32318),loc(_32314,_32316))).
% Into: updated(happens(transport(_32314,_32316,_32318),_33670,_33676),loc(_32314,_32316),_32316-_32318,[]).

% LPS:  updates(row(_33570,_33572),in(to(_33570,_33572),loc(farmer,_33570))).
% Into: updated(happens(row(_33570,_33572),_34908,_34914),loc(farmer,_33570),_33570-_33572,[]).

% LPS:  false((transport(_34832,_34834,_34836),transport(_34904,_34834,_34836),_34832\=_34904)).
% Into: d_pre([happens(transport(_34832,_34834,_34836),_36112,_36118),happens(transport(_34904,_34834,_34836),_36112,_36118),_34832\=_34904]).

% LPS:  false((at(loc(goat,_36522),_36544),at(loc(wolf,_36522),_36544),at(not(loc(farmer,_36522)),_36544))).
% Into: d_pre([holds(loc(goat,_36522),_36544),holds(loc(wolf,_36522),_36544),holds(not(loc(farmer,_36522)),_36544)]).

% LPS:  false((at(loc(goat,_38020),_38042),at(loc(cabbage,_38020),_38042),at(not(loc(farmer,_38020)),_38042))).
% Into: d_pre([holds(loc(goat,_38020),_38042),holds(loc(cabbage,_38020),_38042),holds(not(loc(farmer,_38020)),_38042)]).
% /pack/logicmoo_ec/test/lps_user_examples/goat.pl:66
% pop_lps_dialect('$BLOB'("<stream>(0x562ef73e4800)"),  (/.../(lps_user_examples, 'goat.pl')-> /.../(lps_user_examples, 'goat.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/goat.pl':_47122).


d_pre([happens(transport(A, B, C), D, E), happens(transport(F, B, C), D, E), A\=F]).
d_pre([holds(loc(goat, A), B), holds(loc(wolf, A), B), holds(not(loc(farmer, A)), B)]).
d_pre([holds(loc(goat, A), B), holds(loc(cabbage, A), B), holds(not(loc(farmer, A)), B)]).

fluents([loc(_, _)]).

reactive_rule([holds(loc(A, south), _), A\=farmer], [happens(makeLoc(A, north), _, _)]).

initial_state([loc(wolf, south), loc(goat, south), loc(cabbage, south), loc(farmer, south)]).

l_events(happens(makeLoc(A, B), C, D), [A\=farmer, holds(loc(A, E), C), B\=E, happens(makeLoc(farmer, E), C, F), happens(row(E, B), F, D), happens(transport(A, E, B), F, D)]).
l_events(happens(makeLoc(farmer, A), B, B), [holds(loc(farmer, A), B)]).
l_events(happens(makeLoc(farmer, A), B, C), [holds(loc(farmer, D), B), A\=D, happens(row(D, A), B, C)]).

:- dynamic actions/1.
:- multifile actions/1.

actions([row(_, _), transport(_, _, _)]).

updated(happens(transport(A, B, C), _, _), loc(A, B), B-C, []).
updated(happens(row(A, B), _, _), loc(farmer, A), A-B, []).

maxTime(10).
PROGRAM FAILED
% dB(/.../(lps_user_examples, 'goat.pl'), lps_visualization(_84594{groups:[_83240{content:"loc(A,B)", id:"loc/2", order:3, subgroupStack:"false"}, _83306{content:"Actions", id:"action", order:4}], items:[_83428{content:"cabbage,south", end:4, group:"loc/2", id:0, start:1, subgroup:"cabbage", title:"Fluent loc(cabbage,south) initiated at 1<br/>and terminated at transition to 4"}, _83554{content:"farmer,north", end:3, group:"loc/2", id:1, start:2, subgroup:"farmer", title:"Fluent loc(farmer,north) initiated at 2<br/>and terminated at transition to 3"}, _83680{content:"farmer,south", end:2, group:"loc/2", id:2, start:1, subgroup:"farmer", title:"Fluent loc(farmer,south) initiated at 1<br/>and terminated at transition to 2"}, _83806{content:"farmer,south", end:4, group:"loc/2", id:3, start:3, subgroup:"farmer", title:"Fluent loc(farmer,south) initiated at 3<br/>and terminated at transition to 4"}, _83932{content:"goat,north", end:4, group:"loc/2", id:4, start:2, subgroup:"goat", title:"Fluent loc(goat,north) initiated at 2<br/>and terminated at transition to 4"}, _84058{content:"goat,south", end:2, group:"loc/2", id:5, start:1, subgroup:"goat", title:"Fluent loc(goat,south) initiated at 1<br/>and terminated at transition to 2"}, _84184{content:"wolf,south", end:4, group:"loc/2", id:6, start:1, subgroup:"wolf", title:"Fluent loc(wolf,south) initiated at 1<br/>and terminated at transition to 4"}, _84310{content:"row(south,north)", group:"action", id:7, start:2, style:"color:green", title:"happens(row(south,north),1,2)", type:"point"}, _84436{content:"transport(goat,south,north)", group:"action", id:8, start:2, style:"color:green", title:"happens(transport(goat,south,north),1,2)", type:"point"}, _84562{content:"row(north,south)", group:"action", id:9, start:3, style:"color:green", title:"happens(row(north,south),2,3)", type:"point"}]}, [])).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'hogwarts.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'hogwarts.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/hogwarts.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'hogwarts.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'hogwarts.pl'), lps= /.../(lps_user_examples, 'hogwarts.pl'), using= /.../(lps_user_examples, 'hogwarts.pl')].
% continue_lps_dialect.
% ops.

% LPS:  fluents((varita_obedece(_56158),vivo(_56158))).
% Into: fluents([varita_obedece(_56158),vivo(_56158)]).

% LPS:  actions((desarmo/2,cortar_cabeza/2,destruye_varita/1)).
% Into: actions([desarmo(_58634,_58636),cortar_cabeza(_58646,_58648),destruye_varita(_58658)]).

% LPS:  events((batalla/1,derroto/2)).
% Into: events([batalla(_59752),derroto(_59762,_59764)]).

% LPS:  initially((varita_obedece(dumbledore),vivo(voldemort),vivo(nagini))).
% Into: initial_state([varita_obedece(dumbledore),vivo(voldemort),vivo(nagini)]).

% LPS:  observe(from(batalla(hogwarts),to(4,5))).
% Into: observe([batalla(hogwarts)],5).

% LPS:  if(derroto(_62036,_62038),(varita_obedece(_62036),cortar_cabeza(neville,nagini))).
% Into: l_events(happens(derroto(_62036,_62038),_63218,_63224),[holds(varita_obedece(_62036),_63218),happens(cortar_cabeza(neville,nagini),_63218,_63224)]).

% LPS:  terminates(derroto(_63692,_63694),vivo(_63694)).
% Into: terminated(happens(derroto(_63692,_63694),_64858,_64864),vivo(_63694),[]).

% LPS:  initiates(desarmo(_64806,_64808),varita_obedece(_64806)).
% Into: initiated(happens(desarmo(_64806,_64808),_65980,_65986),varita_obedece(_64806),[]).

% LPS:  terminates(desarmo(_65920,_65922),varita_obedece(_65962)).
% Into: terminated(happens(desarmo(_65920,_65922),_67098,_67104),varita_obedece(_65962),[]).

% LPS:  terminates(destruye_varita(_67032),varita_obedece(_67032)).
% Into: terminated(happens(destruye_varita(_67032),_68188,_68194),varita_obedece(_67032),[]).

% LPS:  terminates(cortar_cabeza(_68130,_68132),vivo(_68132)).
% Into: terminated(happens(cortar_cabeza(_68130,_68132),_69304,_69310),vivo(_68132),[]).

% LPS:  then(if(at(varita_obedece(dumbledore),_69260)),from(desarmo(malfoy,dumbledore),to(_69402,_69404))).
% Into: reactive_rule([holds(varita_obedece(dumbledore),_69260)],[happens(desarmo(malfoy,dumbledore),_69402,_69404)]).

% LPS:  then(if(at(varita_obedece(malfoy),_70584)),from(desarmo(harry,malfoy),to(_70726,_70728))).
% Into: reactive_rule([holds(varita_obedece(malfoy),_70584)],[happens(desarmo(harry,malfoy),_70726,_70728)]).

% LPS:  then(if(at(not(vivo(voldemort)),_71940)),from(destruye_varita(harry),to(_72066,_72068))).
% Into: reactive_rule([holds(not(vivo(voldemort)),_71940)],[happens(destruye_varita(harry),_72066,_72068)]).

% LPS:  then(if(from(batalla(hogwarts),to(_73264,_73266))),from(derroto(harry,voldemort),to(_73266,_73442))).
% Into: reactive_rule([happens(batalla(hogwarts),_73264,_73266)],[happens(derroto(harry,voldemort),_73266,_73442)]).
% /pack/logicmoo_ec/test/lps_user_examples/hogwarts.pl:34
% pop_lps_dialect('$BLOB'("<stream>(0x562ef32ad200)"),  (/.../(lps_user_examples, 'hogwarts.pl')-> /.../(lps_user_examples, 'hogwarts.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/hogwarts.pl':_82194).


initiated(happens(desarmo(A, _), _, _), varita_obedece(A), []).

fluents([varita_obedece(A), vivo(A)]).

terminated(happens(derroto(_, A), _, _), vivo(A), []).
terminated(happens(desarmo(_, _), _, _), varita_obedece(_), []).
terminated(happens(destruye_varita(A), _, _), varita_obedece(A), []).
terminated(happens(cortar_cabeza(_, A), _, _), vivo(A), []).

reactive_rule([holds(varita_obedece(dumbledore), _)], [happens(desarmo(malfoy, dumbledore), _, _)]).
reactive_rule([holds(varita_obedece(malfoy), _)], [happens(desarmo(harry, malfoy), _, _)]).
reactive_rule([holds(not(vivo(voldemort)), _)], [happens(destruye_varita(harry), _, _)]).
reactive_rule([happens(batalla(hogwarts), _, A)], [happens(derroto(harry, voldemort), A, _)]).

initial_state([varita_obedece(dumbledore), vivo(voldemort), vivo(nagini)]).

l_events(happens(derroto(A, _), B, C), [holds(varita_obedece(A), B), happens(cortar_cabeza(neville, nagini), B, C)]).

:- dynamic actions/1.
:- multifile actions/1.

actions([desarmo(_, _), cortar_cabeza(_, _), destruye_varita(_)]).

events([batalla(_), derroto(_, _)]).

observe([batalla(hogwarts)], 5).

maxTime(10).
% dB(/.../(lps_user_examples, 'hogwarts.pl'), lps_visualization(_73850{groups:[_72092{content:"Events", id:"event", order:1}, _72166{content:"varita_obedece(A)", id:"varita_obedece/1", order:3, subgroupStack:"false"}, _72244{content:"vivo(A)", id:"vivo/1", order:3, subgroupStack:"false"}, _72310{content:"Actions", id:"action", order:4}], items:[_72432{content:"dumbledore", end:2, group:"varita_obedece/1", id:0, start:1, subgroup:"dumbledore", title:"Fluent varita_obedece(dumbledore) initiated at 1<br/>and terminated at transition to 2"}, _72558{content:"harry", end:8, group:"varita_obedece/1", id:1, start:3, subgroup:"harry", title:"Fluent varita_obedece(harry) initiated at 3<br/>and terminated at transition to 8"}, _72684{content:"malfoy", end:3, group:"varita_obedece/1", id:2, start:2, subgroup:"malfoy", title:"Fluent varita_obedece(malfoy) initiated at 2<br/>and terminated at transition to 3"}, _72810{content:"nagini", end:6, group:"vivo/1", id:3, start:1, subgroup:"nagini", title:"Fluent vivo(nagini) initiated at 1<br/>and terminated at transition to 6"}, _72936{content:"voldemort", end:6, group:"vivo/1", id:4, start:1, subgroup:"voldemort", title:"Fluent vivo(voldemort) initiated at 1<br/>and terminated at transition to 6"}, _73062{content:"desarmo(malfoy,dumbledore)", group:"action", id:5, start:2, style:"color:green", title:"happens(desarmo(malfoy,dumbledore),1,2)", type:"point"}, _73188{content:"desarmo(harry,malfoy)", group:"action", id:6, start:3, style:"color:green", title:"happens(desarmo(harry,malfoy),2,3)", type:"point"}, _73314{content:"batalla(hogwarts)", group:"event", id:7, start:5, style:"color:#E19735", title:"happens(batalla(hogwarts),4,5)", type:"point"}, _73440{content:"cortar_cabeza(neville,nagini)", group:"action", id:8, start:6, style:"color:green", title:"happens(cortar_cabeza(neville,nagini),5,6)", type:"point"}, _73566{content:"destruye_varita(harry)", group:"action", id:9, start:8, style:"color:green", title:"happens(destruye_varita(harry),7,8)", type:"point"}, _73692{content:"destruye_varita(harry)", group:"action", id:10, start:9, style:"color:green", title:"happens(destruye_varita(harry),8,9)", type:"point"}, _73818{content:"destruye_varita(harry)", group:"action", id:11, start:10, style:"color:green", title:"happens(destruye_varita(harry),9,10)", type:"point"}]}, [])).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'initiating more actions.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'initiating more actions.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/initiating more actions.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'initiating more actions.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'initiating more actions.pl'), lps= /.../(lps_user_examples, 'initiating more actions.pl'), using= /.../(lps_user_examples, 'initiating more actions.pl')].
% continue_lps_dialect.
% ops.

% LPS:  actions(initiating(_57220)).
% Into: actions([initiating(_57220)]).

% LPS:  fluents(gotit(_58246)).
% Into: fluents([gotit(_58246)]).

% LPS:  initiates(initiating(_59264),gotit(_59264)).
% Into: initiated(happens(initiating(_59264),_60420,_60426),gotit(_59264),[]).

% LPS:  then(if(true),(initiating(1),initiating(2),update(in(to(_60480,13),from(gotit(_60480),3))))).
% Into: reactive_rule([],[happens(initiating(1),_61810,_61852),happens(initiating(2),_61874,_61920),happens(update(_60480-13,gotit(_60480)),3,_61816)]).

% LPS:  then(if(initiating(2)),terminate(from(gotit(1),_61950))).
% Into: reactive_rule([happens(initiating(2),_63042,_63048)],[happens(terminate(gotit(1)),_61950,_63080)]).
% /pack/logicmoo_ec/test/lps_user_examples/initiating more actions.pl:22
% pop_lps_dialect('$BLOB'("<stream>(0x562ef33a9e00)"),  (/.../(lps_user_examples, 'initiating more actions.pl')-> /.../(lps_user_examples, 'initiating more actions.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/initiating more actions.pl':_70742).


initiated(happens(initiating(A), _, _), gotit(A), []).

fluents([gotit(_)]).

reactive_rule([], [happens(initiating(1), _, _), happens(initiating(2), _, _), happens(update(A-13, gotit(A)), 3, _)]).
reactive_rule([happens(initiating(2), _, _)], [happens(terminate(gotit(1)), _, _)]).

:- dynamic actions/1.
:- multifile actions/1.

actions([initiating(_)]).

maxTime(5).
% dB(/.../(lps_user_examples, 'initiating more actions.pl'), lps_visualization(_37578{groups:[_36602{content:"gotit(A)", id:"gotit/1", order:3, subgroupStack:"false"}, _36668{content:"Actions", id:"action", order:4}], items:[_36790{content:"1", end:3, group:"gotit/1", id:0, start:2, subgroup:"1", title:"Fluent gotit(1) initiated at 2<br/>and terminated at transition to 3"}, _36916{content:"2", end:4, group:"gotit/1", id:1, start:2, subgroup:"2", title:"Fluent gotit(2) initiated at 2<br/>and terminated at transition to 4"}, _37042{content:"13", end:6, group:"gotit/1", id:2, start:4, subgroup:"13", title:"Fluent gotit(13) initiated at 4<br/>and terminated at transition to 6"}, _37168{content:"initiating(1)", group:"action", id:3, start:2, style:"color:green", title:"happens(initiating(1),1,2)", type:"point"}, _37294{content:"initiating(2)", group:"action", id:4, start:2, style:"color:green", title:"happens(initiating(2),1,2)", type:"point"}, _37420{content:"terminate(gotit(1))", group:"action", id:5, start:3, style:"color:green", title:"happens(terminate(gotit(1)),2,3)", type:"point"}, _37546{content:"update(A-13,gotit(A))", group:"action", id:6, start:4, style:"color:green", title:"happens(update(A-13,gotit(A)),3,4)", type:"point"}]}, [])).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'insurance.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'insurance.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/insurance.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'insurance.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'insurance.pl'), lps= /.../(lps_user_examples, 'insurance.pl'), using= /.../(lps_user_examples, 'insurance.pl')].
% continue_lps_dialect.
% ops.
% /pack/logicmoo_ec/test/lps_user_examples/insurance.pl:18
% pop_lps_dialect('$BLOB'("<stream>(0x562ef4162e00)"),  (/.../(lps_user_examples, 'insurance.pl')-> /.../(lps_user_examples, 'insurance.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/insurance.pl':_83446).


:- dynamic actions/1.
:- multifile actions/1.

% dB(/.../(lps_user_examples, 'insurance.pl'), lps_visualization(_69536{groups:[], items:[]}, [])).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'Jeinfferson Bernal.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'Jeinfferson Bernal.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/Jeinfferson Bernal.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'Jeinfferson Bernal.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'Jeinfferson Bernal.pl'), lps= /.../(lps_user_examples, 'Jeinfferson Bernal.pl'), using= /.../(lps_user_examples, 'Jeinfferson Bernal.pl')].
% continue_lps_dialect.
% ops.

% LPS:  actions((atacar,robar,llamar_al(_19662),repita_mensaje_esposa(_19702))).
% Into: actions([atacar,robar,llamar_al(_19662),repita_mensaje_esposa(_19702)]).

% LPS:  fluents((es_un_arma,es_un_regalo)).
% Into: fluents([es_un_arma,es_un_regalo]).

% LPS:  events((prepare,persuade,muestra_del_general(_22142),llamar_general(_22142),general_recuerda_esposa(_22222))).
% Into: events([prepare,persuade,muestra_del_general(_22142),llamar_general(_22142),general_recuerda_esposa(_22222)]).

% LPS:  initially(es_un_arma).
% Into: initial_state([es_un_arma]).

% LPS:  observe(from(muestra_del_general('+86-555000001'),to(2,3))).
% Into: observe([muestra_del_general('+86-555000001')],3).

% LPS:  observe(from(general_recuerda_esposa(##########),to(3,4))).
% Into: observe([general_recuerda_esposa(##########)],4).

% LPS:  false((es_un_regalo,atacar)).
% Into: d_pre([holds(es_un_regalo,_27910),happens(atacar,_27910,_27916)]).

% LPS:  then(if(es_un_arma),(prepare,atacar)).
% Into: reactive_rule([holds(es_un_arma,_29496)],[happens(prepare,_29654,_29696),happens(atacar,_29696,_29660)]).

% LPS:  then(if(es_un_arma),persuade).
% Into: reactive_rule([holds(es_un_arma,_30978)],[happens(persuade,_31136,_31142)]).

% LPS:  if(from(prepare,to(_31266,_31268)),(at(true,_31266),_31268 is _31266+6)).
% Into: l_events(happens(prepare,_31266,_31268),[holds(true,_31266),_31268 is _31266+6]).

% LPS:  if(persuade,(muestra_del_general(_33298),general_recuerda_esposa(_33338),llamar_general(_33298),repita_mensaje_esposa(_33338))).
% Into: l_events(happens(persuade,_34578,_34584),[happens(muestra_del_general(_33298),_34578,_34652),happens(general_recuerda_esposa(_33338),_34652,_34718),happens(llamar_general(_33298),_34718,_34784),happens(repita_mensaje_esposa(_33338),_34784,_34584)]).

% LPS:  if(llamar_general(_34596),(robar,llamar_al(_34596))).
% Into: l_events(happens(llamar_general(_34596),_35716,_35722),[happens(robar,_35716,_35796),happens(llamar_al(_34596),_35796,_35722)]).

% LPS:  terminates(persuade,es_un_arma).
% Into: terminated(happens(persuade,_36900,_36906),es_un_arma,[]).

% LPS:  initiates(persuade,es_un_regalo).
% Into: initiated(happens(persuade,_37916,_37922),es_un_regalo,[]).
% /pack/logicmoo_ec/test/lps_user_examples/Jeinfferson Bernal.pl:61
% pop_lps_dialect('$BLOB'("<stream>(0x562ef4161700)"),  (/.../(lps_user_examples, 'Jeinfferson Bernal.pl')-> /.../(lps_user_examples, 'Jeinfferson Bernal.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/Jeinfferson Bernal.pl':_45516).


initiated(happens(persuade, _, _), es_un_regalo, []).

d_pre([holds(es_un_regalo, A), happens(atacar, A, _)]).

fluents([es_un_arma, es_un_regalo]).

reactive_rule([holds(es_un_arma, _)], [happens(prepare, _, A), happens(atacar, A, _)]).
reactive_rule([holds(es_un_arma, _)], [happens(persuade, _, _)]).

terminated(happens(persuade, _, _), es_un_arma, []).

initial_state([es_un_arma]).

l_events(happens(prepare, A, B), [holds(true, A), B is A+6]).
l_events(happens(persuade, A, B), [happens(muestra_del_general(C), A, D), happens(general_recuerda_esposa(E), D, F), happens(llamar_general(C), F, G), happens(repita_mensaje_esposa(E), G, B)]).
l_events(happens(llamar_general(A), B, C), [happens(robar, B, D), happens(llamar_al(A), D, C)]).

:- dynamic actions/1.
:- multifile actions/1.

actions([atacar, robar, llamar_al(_), repita_mensaje_esposa(_)]).

events([prepare, persuade, muestra_del_general(A), llamar_general(A), general_recuerda_esposa(_)]).

observe([muestra_del_general('+86-555000001')], 3).
observe([general_recuerda_esposa(##########)], 4).

maxTime(10).
% dB(/.../(lps_user_examples, 'Jeinfferson Bernal.pl'), lps_visualization(_60436{groups:[_59340{content:"Events", id:"event", order:1}, _59414{content:"es_un_arma", id:"es_un_arma/0", order:3, subgroupStack:"false"}, _59492{content:"es_un_regalo", id:"es_un_regalo/0", order:3, subgroupStack:"false"}, _59558{content:"Actions", id:"action", order:4}], items:[_59668{content:"es_un_arma", end:7, group:"es_un_arma/0", id:0, start:1, title:"Fluent es_un_arma initiated at 1<br/>and terminated at transition to 7"}, _59778{content:"es_un_regalo", end:11, group:"es_un_regalo/0", id:1, start:7, title:"Fluent es_un_regalo initiated at 7<br/>and terminated at transition to 11"}, _59900{content:"muestra_del_general(+86-555000001)", group:"event", id:2, start:3, style:"color:#E19735", title:"happens(muestra_del_general(+86-555000001),2,3)", type:"point"}, _60026{content:"general_recuerda_esposa(##########)", group:"event", id:3, start:4, style:"color:#E19735", title:"happens(general_recuerda_esposa(##########),3,4)", type:"point"}, _60152{content:"robar", group:"action", id:4, start:5, style:"color:green", title:"happens(robar,4,5)", type:"point"}, _60278{content:"llamar_al(+86-555000001)", group:"action", id:5, start:6, style:"color:green", title:"happens(llamar_al(+86-555000001),5,6)", type:"point"}, _60404{content:"repita_mensaje_esposa(##########)", group:"action", id:6, start:7, style:"color:green", title:"happens(repita_mensaje_esposa(##########),6,7)", type:"point"}]}, [])).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'JindoshRiddle.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'JindoshRiddle.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/JindoshRiddle.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'JindoshRiddle.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'JindoshRiddle.pl'), lps= /.../(lps_user_examples, 'JindoshRiddle.pl'), using= /.../(lps_user_examples, 'JindoshRiddle.pl')].
% continue_lps_dialect.
% ops.
% /pack/logicmoo_ec/test/lps_user_examples/JindoshRiddle.pl:71
% pop_lps_dialect('$BLOB'("<stream>(0x562ef73e2400)"),  (/.../(lps_user_examples, 'JindoshRiddle.pl')-> /.../(lps_user_examples, 'JindoshRiddle.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/JindoshRiddle.pl':_44734).


seating(Seats) :-
    length(Seats, 5),
    member(s(winslow, _, _, _, purple), Seats),
    left_of(s(_, _, _, whiskey, red),
            s(_, _, _, _, green),
            Seats),
    member(s(_, _, dabokva, _, white), Seats),
    next(s(_, snuff, _, _, _),
         s(_, _, dabokva, _, _),
         Seats),
    Seats=[s(marcola, _, _, _, _), s(_, _, _, _, blue), s(_, _, _, beer, _), _, _],
    member(s(contee, medal, _, _, _), Seats),
    member(s(_, ring, fraeport, _, _), Seats),
    next(s(_, _, dunwall, _, _),
         s(_, _, _, absinthe, _),
         Seats),
    next(s(_, snuff, _, _, _),
         s(_, _, dabokva, _, _),
         Seats),
    member(s(finch, _, _, rum, _), Seats),
    member(s(_, _, karnaca, wine, _), Seats),
    next(s(_, pendant, _, _, _),
         s(_, _, dunwall, _, _),
         Seats),
    member(s(natsiou, _, baleton, _, _), Seats),
    member(s(_, diamond, _, _, _), Seats).

next(A, B, Ls) :-
    left_of(A, B, Ls).
next(A, B, Ls) :-
    right_of(A, B, Ls).

right_of(A, B, Ls) :-
    append(_, [B, A|_], Ls).

:- dynamic'swish renderer'/2.

'swish renderer'(table, [header(s('Owner', 'Heirloom', 'Town', 'Drink', 'Color'))]).

:- dynamic actions/1.
:- multifile actions/1.


left_of(A, B, Ls) :-
    append(_, [A, B|_], Ls).
% dB(/.../(lps_user_examples, 'JindoshRiddle.pl'), lps_visualization(_42858{groups:[], items:[]}, [])).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'JohnDTractorInsuranceContract.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'JohnDTractorInsuranceContract.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/JohnDTractorInsuranceContract.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'JohnDTractorInsuranceContract.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'JohnDTractorInsuranceContract.pl'), lps= /.../(lps_user_examples, 'JohnDTractorInsuranceContract.pl'), using= /.../(lps_user_examples, 'JohnDTractorInsuranceContract.pl')].
% continue_lps_dialect.
% ops.
% /pack/logicmoo_ec/test/lps_user_examples/JohnDTractorInsuranceContract.pl:3
% pop_lps_dialect('$BLOB'("<stream>(0x562ef73e4b00)"),  (/.../(lps_user_examples, 'JohnDTractorInsuranceContract.pl')-> /.../(lps_user_examples, 'JohnDTractorInsuranceContract.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/JohnDTractorInsuranceContract.pl':_85540).


:- dynamic actions/1.
:- multifile actions/1.

% dB(/.../(lps_user_examples, 'JohnDTractorInsuranceContract.pl'), lps_visualization(_27022{groups:[], items:[]}, [])).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'Joker_s_dilemma.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'Joker_s_dilemma.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/Joker_s_dilemma.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'Joker_s_dilemma.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'Joker_s_dilemma.pl'), lps= /.../(lps_user_examples, 'Joker_s_dilemma.pl'), using= /.../(lps_user_examples, 'Joker_s_dilemma.pl')].
% continue_lps_dialect.
% ops.

% LPS:  fluents((peligro_fisico(_62992),peligro_sancion_social(_63032))).
% Into: fluents([peligro_fisico(_62992),peligro_sancion_social(_63032)]).

% LPS:  events((peligro_de_muerte(_64100),alejarse_del_peligro(_64100),causar_muerte(_64100,_64196),posible_sancion_social(_64250,_64100),evitar_sancion(_64250,_64100),gana(_64348))).
% Into: events([peligro_de_muerte(_64100),alejarse_del_peligro(_64100),causar_muerte(_64100,_64196),posible_sancion_social(_64250,_64100),evitar_sancion(_64250,_64100),gana(_64348)]).

% LPS:  actions((usar_detonador(_65638),guardar_detonador(_65638))).
% Into: actions([usar_detonador(_65638),guardar_detonador(_65638)]).

% LPS:  then(if(from(peligro_de_muerte(_66870),to(_66906,_66908))),from(alejarse_del_peligro(_66870),to(_67066,_67068))).
% Into: reactive_rule([happens(peligro_de_muerte(_66870),_66906,_66908)],[happens(alejarse_del_peligro(_66870),_67066,_67068)]).

% LPS:  if(from(alejarse_del_peligro(_19316),to(_19332,_19334)),from(usar_detonador(_19316),to(_19432,_19434))).
% Into: l_events(happens(alejarse_del_peligro(_19316),_19332,_19334),[happens(usar_detonador(_19316),_19432,_19434)]).

% LPS:  if(from(causar_muerte(_19884,_19886),to(_19922,_19924)),from(usar_detonador(_19884),to(_20058,_20060))).
% Into: l_events(happens(causar_muerte(_19884,_19886),_19922,_19924),[happens(usar_detonador(_19884),_20058,_20060)]).

% LPS:  if(posible_sancion_social(_21262,_21264),causar_muerte(_21264,_21320)).
% Into: l_events(happens(posible_sancion_social(_21262,_21264),_22374,_22380),[happens(causar_muerte(_21264,_21320),_22374,_22380)]).

% LPS:  then(if(from(posible_sancion_social(_22414,_22416),to(_22452,_22454))),from(evitar_sancion(_22414,_22416),to(_22628,_22630))).
% Into: reactive_rule([happens(posible_sancion_social(_22414,_22416),_22452,_22454)],[happens(evitar_sancion(_22414,_22416),_22628,_22630)]).

% LPS:  if(evitar_sancion(_23838,_23840),from(guardar_detonador(_23840),to(_23916,_23918))).
% Into: l_events(happens(evitar_sancion(_23838,_23840),_23916,_23918),[happens(guardar_detonador(_23840),_23916,_23918)]).

% LPS:  if(gana(batman),(evitar_sancion(_25166,ciudadanos),evitar_sancion(_25166,delincuentes))).
% Into: l_events(happens(gana(batman),_26296,_26302),[happens(evitar_sancion(_25166,ciudadanos),_26296,_26376),happens(evitar_sancion(_25166,delincuentes),_26376,_26302)]).

% LPS:  if(gana(joker),alejarse_del_peligro(ciudadanos)).
% Into: l_events(happens(gana(joker),_27368,_27374),[happens(alejarse_del_peligro(ciudadanos),_27368,_27374)]).

% LPS:  if(gana(joker),alejarse_del_peligro(delincuentes)).
% Into: l_events(happens(gana(joker),_28440,_28446),[happens(alejarse_del_peligro(delincuentes),_28440,_28446)]).

% LPS:  initially((peligro_fisico(pasajeros),peligro_sancion_social(pasajeros))).
% Into: initial_state([peligro_fisico(pasajeros),peligro_sancion_social(pasajeros)]).

% LPS:  observe((from(peligro_de_muerte(ciudadanos),to(1,2)),from(peligro_de_muerte(delincuentes),to(1,2)))).
% Into: observe([peligro_de_muerte(ciudadanos),peligro_de_muerte(delincuentes)],2).

% LPS:  terminates(guardar_detonador(_30890),peligro_sancion_social(_30930)).
% Into: terminated(happens(guardar_detonador(_30890),_32026,_32032),peligro_sancion_social(_30930),[]).

% LPS:  terminates(usar_detonador(_31962),peligro_fisico(_32002)).
% Into: terminated(happens(usar_detonador(_31962),_33098,_33104),peligro_fisico(_32002),[]).
% /pack/logicmoo_ec/test/lps_user_examples/Joker_s_dilemma.pl:48
% pop_lps_dialect('$BLOB'("<stream>(0x562ef73e5c00)"),  (/.../(lps_user_examples, 'Joker_s_dilemma.pl')-> /.../(lps_user_examples, 'Joker_s_dilemma.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/Joker_s_dilemma.pl':_40678).


fluents([peligro_fisico(_), peligro_sancion_social(_)]).

terminated(happens(guardar_detonador(_), _, _), peligro_sancion_social(_), []).
terminated(happens(usar_detonador(_), _, _), peligro_fisico(_), []).

reactive_rule([happens(peligro_de_muerte(A), _, _)], [happens(alejarse_del_peligro(A), _, _)]).
reactive_rule([happens(posible_sancion_social(A, B), _, _)], [happens(evitar_sancion(A, B), _, _)]).

initial_state([peligro_fisico(pasajeros), peligro_sancion_social(pasajeros)]).

l_events(happens(alejarse_del_peligro(A), _, _), [happens(usar_detonador(A), _, _)]).
l_events(happens(causar_muerte(A, _), _, _), [happens(usar_detonador(A), _, _)]).
l_events(happens(posible_sancion_social(_, A), B, C), [happens(causar_muerte(A, _), B, C)]).
l_events(happens(evitar_sancion(_, A), B, C), [happens(guardar_detonador(A), B, C)]).
l_events(happens(gana(batman), A, B), [happens(evitar_sancion(C, ciudadanos), A, D), happens(evitar_sancion(C, delincuentes), D, B)]).
l_events(happens(gana(joker), A, B), [happens(alejarse_del_peligro(ciudadanos), A, B)]).
l_events(happens(gana(joker), A, B), [happens(alejarse_del_peligro(delincuentes), A, B)]).

:- dynamic actions/1.
:- multifile actions/1.

actions([usar_detonador(A), guardar_detonador(A)]).

events([peligro_de_muerte(A), alejarse_del_peligro(A), causar_muerte(A, _), posible_sancion_social(B, A), evitar_sancion(B, A), gana(_)]).

observe([peligro_de_muerte(ciudadanos), peligro_de_muerte(delincuentes)], 2).

maxTime(15).
% dB(/.../(lps_user_examples, 'Joker_s_dilemma.pl'), lps_visualization(_58410{groups:[_57156{content:"Events", id:"event", order:1}, _57230{content:"peligro_fisico(A)", id:"peligro_fisico/1", order:3, subgroupStack:"false"}, _57308{content:"peligro_sancion_social(A)", id:"peligro_sancion_social/1", order:3, subgroupStack:"false"}, _57374{content:"Actions", id:"action", order:4}], items:[_57496{content:"pasajeros", end:3, group:"peligro_fisico/1", id:0, start:1, subgroup:"pasajeros", title:"Fluent peligro_fisico(pasajeros) initiated at 1<br/>and terminated at transition to 3"}, _57622{content:"pasajeros", end:4, group:"peligro_sancion_social/1", id:1, start:1, subgroup:"pasajeros", title:"Fluent peligro_sancion_social(pasajeros) initiated at 1<br/>and terminated at transition to 4"}, _57748{content:"peligro_de_muerte(ciudadanos)", group:"event", id:2, start:2, style:"color:#E19735", title:"happens(peligro_de_muerte(ciudadanos),1,2)", type:"point"}, _57874{content:"peligro_de_muerte(delincuentes)", group:"event", id:3, start:2, style:"color:#E19735", title:"happens(peligro_de_muerte(delincuentes),1,2)", type:"point"}, _58000{content:"usar_detonador(delincuentes)", group:"action", id:4, start:3, style:"color:green", title:"happens(usar_detonador(delincuentes),2,3)", type:"point"}, _58126{content:"usar_detonador(ciudadanos)", group:"action", id:5, start:3, style:"color:green", title:"happens(usar_detonador(ciudadanos),2,3)", type:"point"}, _58252{content:"guardar_detonador(ciudadanos)", group:"action", id:6, start:4, style:"color:green", title:"happens(guardar_detonador(ciudadanos),3,4)", type:"point"}, _58378{content:"guardar_detonador(delincuentes)", group:"action", id:7, start:4, style:"color:green", title:"happens(guardar_detonador(delincuentes),3,4)", type:"point"}]}, [])).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'leonraton01.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'leonraton01.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/leonraton01.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'leonraton01.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'leonraton01.pl'), lps= /.../(lps_user_examples, 'leonraton01.pl'), using= /.../(lps_user_examples, 'leonraton01.pl')].
% continue_lps_dialect.
% ops.

% LPS:  fluents((león_atrapado,quiero_liberar_al_león,salvar_al_león)).
% Into: fluents([león_atrapado,quiero_liberar_al_león,salvar_al_león]).

% LPS:  actions((precavido,utilizar_afilados_dientes,correr)).
% Into: actions([precavido,utilizar_afilados_dientes,correr]).

% LPS:  events((liberar,cortar_cuerdas,escapar_al_dejar_trampa)).
% Into: events([liberar,cortar_cuerdas,escapar_al_dejar_trampa]).

% LPS:  initially(león_atrapado).
% Into: initial_state([león_atrapado]).

% LPS:  then(if(at(león_atrapado,_23856)),from(liberar,to(_23856,_23960))).
% Into: reactive_rule([holds(león_atrapado,_23856)],[happens(liberar,_23856,_23960)]).

% LPS:  if(from(liberar,to(_25534,_25536)),from(precavido,to(_25534,_25536))).
% Into: l_events(happens(liberar,_25534,_25536),[happens(precavido,_25534,_25536)]).

% LPS:  terminates(liberar,león_atrapado).
% Into: terminated(happens(liberar,_27844,_27850),león_atrapado,[]).

% LPS:  initially(quiero_liberar_al_león).
% Into: initial_state([quiero_liberar_al_león]).

% LPS:  then(if(at(quiero_liberar_al_león,_28798)),from(cortar_cuerdas,to(_28798,_28902))).
% Into: reactive_rule([holds(quiero_liberar_al_león,_28798)],[happens(cortar_cuerdas,_28798,_28902)]).

% LPS:  if(from(cortar_cuerdas,to(_30042,_30044)),from(utilizar_afilados_dientes,to(_30042,_30044))).
% Into: l_events(happens(cortar_cuerdas,_30042,_30044),[happens(utilizar_afilados_dientes,_30042,_30044)]).

% LPS:  terminates(cortar_cuerdas,quiero_liberar_al_león).
% Into: terminated(happens(cortar_cuerdas,_32352,_32358),quiero_liberar_al_león,[]).

% LPS:  initially(salvar_al_león).
% Into: initial_state([salvar_al_león]).

% LPS:  then(if(at(salvar_al_león,_33306)),from(escapar_al_dejar_trampa,to(_33306,_33410))).
% Into: reactive_rule([holds(salvar_al_león,_33306)],[happens(escapar_al_dejar_trampa,_33306,_33410)]).

% LPS:  if(from(escapar_al_dejar_trampa,to(_34550,_34552)),from(correr,to(_34550,_34552))).
% Into: l_events(happens(escapar_al_dejar_trampa,_34550,_34552),[happens(correr,_34550,_34552)]).

% LPS:  terminates(escapar_al_dejar_trampa,salvar_al_león).
% Into: terminated(happens(escapar_al_dejar_trampa,_36860,_36866),salvar_al_león,[]).
% /pack/logicmoo_ec/test/lps_user_examples/leonraton01.pl:25
% pop_lps_dialect('$BLOB'("<stream>(0x562ef76d6c00)"),  (/.../(lps_user_examples, 'leonraton01.pl')-> /.../(lps_user_examples, 'leonraton01.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/leonraton01.pl':_44412).


fluents([león_atrapado, quiero_liberar_al_león, salvar_al_león]).

terminated(happens(liberar, _, _), león_atrapado, []).
terminated(happens(cortar_cuerdas, _, _), quiero_liberar_al_león, []).
terminated(happens(escapar_al_dejar_trampa, _, _), salvar_al_león, []).

reactive_rule([holds(león_atrapado, A)], [happens(liberar, A, _)]).
reactive_rule([holds(quiero_liberar_al_león, A)], [happens(cortar_cuerdas, A, _)]).
reactive_rule([holds(salvar_al_león, A)], [happens(escapar_al_dejar_trampa, A, _)]).

initial_state([león_atrapado]).
initial_state([quiero_liberar_al_león]).
initial_state([salvar_al_león]).

l_events(happens(liberar, A, B), [happens(precavido, A, B)]).
l_events(happens(cortar_cuerdas, A, B), [happens(utilizar_afilados_dientes, A, B)]).
l_events(happens(escapar_al_dejar_trampa, A, B), [happens(correr, A, B)]).

:- dynamic actions/1.
:- multifile actions/1.

actions([precavido, utilizar_afilados_dientes, correr]).

events([liberar, cortar_cuerdas, escapar_al_dejar_trampa]).

maxTime(18).
% dB(/.../(lps_user_examples, 'leonraton01.pl'), lps_visualization(_57872{groups:[_56536{content:"león_atrapado", id:"león_atrapado/0", order:3, subgroupStack:"false"}, _56614{content:"quiero_liberar_al_león", id:"quiero_liberar_al_león/0", order:3, subgroupStack:"false"}, _56692{content:"salvar_al_león", id:"salvar_al_león/0", order:3, subgroupStack:"false"}, _56758{content:"Actions", id:"action", order:4}], items:[_56868{content:"león_atrapado", end:2, group:"león_atrapado/0", id:0, start:1, title:"Fluent león_atrapado initiated at 1<br/>and terminated at transition to 2"}, _56978{content:"quiero_liberar_al_león", end:2, group:"quiero_liberar_al_león/0", id:1, start:1, title:"Fluent quiero_liberar_al_león initiated at 1<br/>and terminated at transition to 2"}, _57088{content:"salvar_al_león", end:2, group:"salvar_al_león/0", id:2, start:1, title:"Fluent salvar_al_león initiated at 1<br/>and terminated at transition to 2"}, _57210{content:"correr", group:"action", id:3, start:2, style:"color:green", title:"happens(correr,1,2)", type:"point"}, _57336{content:"utilizar_afilados_dientes", group:"action", id:4, start:2, style:"color:green", title:"happens(utilizar_afilados_dientes,1,2)", type:"point"}, _57462{content:"precavido", group:"action", id:5, start:2, style:"color:green", title:"happens(precavido,1,2)", type:"point"}, _57588{content:"precavido", group:"action", id:6, start:3, style:"color:green", title:"happens(precavido,2,3)", type:"point"}, _57714{content:"utilizar_afilados_dientes", group:"action", id:7, start:3, style:"color:green", title:"happens(utilizar_afilados_dientes,2,3)", type:"point"}, _57840{content:"correr", group:"action", id:8, start:3, style:"color:green", title:"happens(correr,2,3)", type:"point"}]}, [])).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'leonraton02.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'leonraton02.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/leonraton02.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'leonraton02.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'leonraton02.pl'), lps= /.../(lps_user_examples, 'leonraton02.pl'), using= /.../(lps_user_examples, 'leonraton02.pl')].
% continue_lps_dialect.
% ops.

% LPS:  fluents((esta(_19550,_19552),cerca(_19550,_19552))).
% Into: fluents([esta(_19550,_19552),cerca(_19550,_19552)]).

% LPS:  actions((precavido/2,puede/2,salvar/2,liberarlo/2)).
% Into: actions([precavido(_22210,_22212),puede(_22222,_22224),salvar(_22234,_22236),liberarlo(_22246,_22248)]).

% LPS:  events((cortar_cuerdas/1,libero/1,liberar/2)).
% Into: events([cortar_cuerdas(_23436),libero(_23446),liberar(_23456,_23458)]).

% LPS:  initially(esta(león,amarre)).
% Into: initial_state([esta(león,amarre)]).

% LPS:  observe(from(cortar_cuerdas(yo),to(1,2))).
% Into: observe([cortar_cuerdas(yo)],2).

% LPS:  if(initiates(puede(_25532,_25534),esta(_25532,_25534)),cerca(_25532,_25534)).
% Into: initiated(happens(puede(_25532,_25534),_26844,_26850),esta(_25532,_25534),[holds(cerca(_25532,_25534),_26844)]).

% LPS:  if(terminates(puede(_27104,_27106),esta(_27160,_27106)),_27160\==_27104).
% Into: terminated(happens(puede(_27104,_27106),_28428,_28434),esta(_27160,_27106),[_27160\==_27104]).

% LPS:  if(initiates(libero(león),cerca(yo,amarre)),esta(león,amarre)).
% Into: initiated(happens(libero(león),_30084,_30090),cerca(yo,amarre),[holds(esta(león,amarre),_30084)]).

% LPS:  if(libero(león),precavido(yo,león)).
% Into: l_events(happens(libero(león),_31402,_31408),[happens(precavido(yo,león),_31402,_31408)]).

% LPS:  if(liberarlo(yo,amarre),(esta(león,amarre),libero(león))).
% Into: l_events(happens(liberarlo(yo,amarre),_32832,_32838),[holds(esta(león,amarre),_32832),happens(libero(león),_32832,_32838)]).

% LPS:  if(liberar(_32864,_32866),(liberarlo(_32864,_32866),puede(_32864,_32866),salvar(_32864,_32866))).
% Into: l_events(happens(liberar(_32864,_32866),_34164,_34170),[happens(liberarlo(_32864,_32866),_34164,_34244),happens(puede(_32864,_32866),_34244,_34386),happens(salvar(_32864,_32866),_34386,_34170)]).

% LPS:  then(if(from(cortar_cuerdas(yo),to(_34226,_34228))),from(liberar(yo,amarre),to(_34228,_34404))).
% Into: reactive_rule([happens(cortar_cuerdas(yo),_34226,_34228)],[happens(liberar(yo,amarre),_34228,_34404)]).
% /pack/logicmoo_ec/test/lps_user_examples/leonraton02.pl:20
% pop_lps_dialect('$BLOB'("<stream>(0x562ef6c57200)"),  (/.../(lps_user_examples, 'leonraton02.pl')-> /.../(lps_user_examples, 'leonraton02.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/leonraton02.pl':_43196).


initiated(happens(puede(A, B), C, _), esta(A, B), [holds(cerca(A, B), C)]).
initiated(happens(libero(león), A, _), cerca(yo, amarre), [holds(esta(león, amarre), A)]).

fluents([esta(A, B), cerca(A, B)]).

terminated(happens(puede(A, B), _, _), esta(C, B), [C\==A]).

reactive_rule([happens(cortar_cuerdas(yo), _, A)], [happens(liberar(yo, amarre), A, _)]).

initial_state([esta(león, amarre)]).

l_events(happens(libero(león), A, B), [happens(precavido(yo, león), A, B)]).
l_events(happens(liberarlo(yo, amarre), A, B), [holds(esta(león, amarre), A), happens(libero(león), A, B)]).
l_events(happens(liberar(A, B), C, D), [happens(liberarlo(A, B), C, E), happens(puede(A, B), E, F), happens(salvar(A, B), F, D)]).

:- dynamic actions/1.
:- multifile actions/1.

actions([precavido(_, _), puede(_, _), salvar(_, _), liberarlo(_, _)]).

events([cortar_cuerdas(_), libero(_), liberar(_, _)]).

observe([cortar_cuerdas(yo)], 2).

maxTime(6).
% dB(/.../(lps_user_examples, 'leonraton02.pl'), lps_visualization(_39260{groups:[_38132{content:"Events", id:"event", order:1}, _38206{content:"cerca(A,B)", id:"cerca/2", order:3, subgroupStack:"false"}, _38284{content:"esta(A,B)", id:"esta/2", order:3, subgroupStack:"false"}, _38350{content:"Actions", id:"action", order:4}], items:[_38472{content:"yo,amarre", end:7, group:"cerca/2", id:0, start:3, subgroup:"yo", title:"Fluent cerca(yo,amarre) initiated at 3<br/>and terminated at transition to 7"}, _38598{content:"león,amarre", end:4, group:"esta/2", id:1, start:1, subgroup:"león", title:"Fluent esta(león,amarre) initiated at 1<br/>and terminated at transition to 4"}, _38724{content:"yo,amarre", end:7, group:"esta/2", id:2, start:4, subgroup:"yo", title:"Fluent esta(yo,amarre) initiated at 4<br/>and terminated at transition to 7"}, _38850{content:"cortar_cuerdas(yo)", group:"event", id:3, start:2, style:"color:#E19735", title:"happens(cortar_cuerdas(yo),1,2)", type:"point"}, _38976{content:"precavido(yo,león)", group:"action", id:4, start:3, style:"color:green", title:"happens(precavido(yo,león),2,3)", type:"point"}, _39102{content:"puede(yo,amarre)", group:"action", id:5, start:4, style:"color:green", title:"happens(puede(yo,amarre),3,4)", type:"point"}, _39228{content:"salvar(yo,amarre)", group:"action", id:6, start:5, style:"color:green", title:"happens(salvar(yo,amarre),4,5)", type:"point"}]}, [])).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'Leon_raton.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'Leon_raton.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/Leon_raton.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'Leon_raton.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'Leon_raton.pl'), lps= /.../(lps_user_examples, 'Leon_raton.pl'), using= /.../(lps_user_examples, 'Leon_raton.pl')].
% continue_lps_dialect.
% ops.

% LPS:  fluents((leon_atrapado,liberar_leon,salvar_leon)).
% Into: fluents([leon_atrapado,liberar_leon,salvar_leon]).

% LPS:  actions((precavido,utilizar_dientes,correr)).
% Into: actions([precavido,utilizar_dientes,correr]).

% LPS:  events((liberar,cortar_amarre,escapar_trampa)).
% Into: events([liberar,cortar_amarre,escapar_trampa]).

% LPS:  initially(leon_atrapado).
% Into: initial_state([leon_atrapado]).

% LPS:  then(if(at(leon_atrapado,_23844)),from(liberar,to(_23844,_23948))).
% Into: reactive_rule([holds(leon_atrapado,_23844)],[happens(liberar,_23844,_23948)]).

% LPS:  if(from(liberar,to(_25522,_25524)),from(precavido,to(_25522,_25524))).
% Into: l_events(happens(liberar,_25522,_25524),[happens(precavido,_25522,_25524)]).

% LPS:  terminates(liberar,leon_atrapado).
% Into: terminated(happens(liberar,_27832,_27838),leon_atrapado,[]).

% LPS:  initiates(liberar,liberar_leon).
% Into: initiated(happens(liberar,_28848,_28854),liberar_leon,[]).

% LPS:  then(if(at(liberar_leon,_28804)),from(cortar_amarre,to(_28804,_28908))).
% Into: reactive_rule([holds(liberar_leon,_28804)],[happens(cortar_amarre,_28804,_28908)]).

% LPS:  if(from(cortar_amarre,to(_30048,_30050)),from(utilizar_dientes,to(_30048,_30050))).
% Into: l_events(happens(cortar_amarre,_30048,_30050),[happens(utilizar_dientes,_30048,_30050)]).

% LPS:  terminates(cortar_amarre,liberar_leon).
% Into: terminated(happens(cortar_amarre,_32358,_32364),liberar_leon,[]).

% LPS:  initiates(cortar_amarre,salvar_leon).
% Into: initiated(happens(cortar_amarre,_33374,_33380),salvar_leon,[]).

% LPS:  then(if(at(salvar_leon,_33330)),from(escapar_trampa,to(_33330,_33434))).
% Into: reactive_rule([holds(salvar_leon,_33330)],[happens(escapar_trampa,_33330,_33434)]).

% LPS:  if(from(escapar_trampa,to(_34574,_34576)),from(correr,to(_34574,_34576))).
% Into: l_events(happens(escapar_trampa,_34574,_34576),[happens(correr,_34574,_34576)]).

% LPS:  terminates(escapar_trampa,salvar_leon).
% Into: terminated(happens(escapar_trampa,_36884,_36890),salvar_leon,[]).
% /pack/logicmoo_ec/test/lps_user_examples/Leon_raton.pl:34
% pop_lps_dialect('$BLOB'("<stream>(0x562ef70bf800)"),  (/.../(lps_user_examples, 'Leon_raton.pl')-> /.../(lps_user_examples, 'Leon_raton.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/Leon_raton.pl':_44436).


initiated(happens(liberar, _, _), liberar_leon, []).
initiated(happens(cortar_amarre, _, _), salvar_leon, []).

fluents([leon_atrapado, liberar_leon, salvar_leon]).

terminated(happens(liberar, _, _), leon_atrapado, []).
terminated(happens(cortar_amarre, _, _), liberar_leon, []).
terminated(happens(escapar_trampa, _, _), salvar_leon, []).

reactive_rule([holds(leon_atrapado, A)], [happens(liberar, A, _)]).
reactive_rule([holds(liberar_leon, A)], [happens(cortar_amarre, A, _)]).
reactive_rule([holds(salvar_leon, A)], [happens(escapar_trampa, A, _)]).

initial_state([leon_atrapado]).

l_events(happens(liberar, A, B), [happens(precavido, A, B)]).
l_events(happens(cortar_amarre, A, B), [happens(utilizar_dientes, A, B)]).
l_events(happens(escapar_trampa, A, B), [happens(correr, A, B)]).

:- dynamic actions/1.
:- multifile actions/1.

actions([precavido, utilizar_dientes, correr]).

events([liberar, cortar_amarre, escapar_trampa]).

maxTime(10).
% dB(/.../(lps_user_examples, 'Leon_raton.pl'), lps_visualization(_47122{groups:[_45786{content:"leon_atrapado", id:"leon_atrapado/0", order:3, subgroupStack:"false"}, _45864{content:"liberar_leon", id:"liberar_leon/0", order:3, subgroupStack:"false"}, _45942{content:"salvar_leon", id:"salvar_leon/0", order:3, subgroupStack:"false"}, _46008{content:"Actions", id:"action", order:4}], items:[_46118{content:"leon_atrapado", end:2, group:"leon_atrapado/0", id:0, start:1, title:"Fluent leon_atrapado initiated at 1<br/>and terminated at transition to 2"}, _46228{content:"liberar_leon", end:4, group:"liberar_leon/0", id:1, start:2, title:"Fluent liberar_leon initiated at 2<br/>and terminated at transition to 4"}, _46338{content:"salvar_leon", end:6, group:"salvar_leon/0", id:2, start:4, title:"Fluent salvar_leon initiated at 4<br/>and terminated at transition to 6"}, _46460{content:"precavido", group:"action", id:3, start:2, style:"color:green", title:"happens(precavido,1,2)", type:"point"}, _46586{content:"precavido", group:"action", id:4, start:3, style:"color:green", title:"happens(precavido,2,3)", type:"point"}, _46712{content:"utilizar_dientes", group:"action", id:5, start:4, style:"color:green", title:"happens(utilizar_dientes,3,4)", type:"point"}, _46838{content:"utilizar_dientes", group:"action", id:6, start:5, style:"color:green", title:"happens(utilizar_dientes,4,5)", type:"point"}, _46964{content:"correr", group:"action", id:7, start:6, style:"color:green", title:"happens(correr,5,6)", type:"point"}, _47090{content:"correr", group:"action", id:8, start:7, style:"color:green", title:"happens(correr,6,7)", type:"point"}]}, [])).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'Life.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'Life.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/Life.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'Life.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'Life.pl'), lps= /.../(lps_user_examples, 'Life.pl'), using= /.../(lps_user_examples, 'Life.pl')].
% continue_lps_dialect.
% ops.

% LPS:  fluents(_19520-_19522).
% Into: fluents([_19520-_19522]).

% LPS:  actions((die(_20600),live(_20640))).
% Into: actions([die(_20600),live(_20640)]).

% LPS:  initially((2-3,3-3,4-3)).
% Into: initial_state([2-3,3-3,4-3]).

% LPS:  then(if((at(_23064-_23066,_23114),at(aliveNeighbors(_23064-_23066,_23244),_23114),_23244<2)),die(_23064-_23066)).
% Into: reactive_rule([holds(_23064-_23066,_23114),holds(aliveNeighbors(_23064-_23066,_23244),_23114),_23244<2],[happens(die(_23064-_23066),_26372,_26378)]).

% LPS:  then(if((at(_26594-_26596,_26644),at(aliveNeighbors(_26594-_26596,_26774),_26644),_26774>3)),die(_26594-_26596)).
% Into: reactive_rule([holds(_26594-_26596,_26644),holds(aliveNeighbors(_26594-_26596,_26774),_26644),_26774>3],[happens(die(_26594-_26596),_29894,_29900)]).

% LPS:  then(if((cell(_30134,_30136),at(aliveNeighbors(_30134-_30136,3),_30262))),live(_30134-_30136)).
% Into: reactive_rule([cell(_30134,_30136),holds(aliveNeighbors(_30134-_30136,3),_30262)],[happens(live(_30134-_30136),_32762,_32768)]).

% LPS:  if(at(aliveNeighbors(_38172-_38174,_38220),_38242),(adjacent(_38172-_38174,_38372),at(countLivingNeighbors(_38372,_38220),_38242))).
% Into: l_int(holds(aliveNeighbors(_38172-_38174,_38220),_38242),[adjacent(_38172-_38174,_38372),holds(countLivingNeighbors(_38372,_38220),_38242)]).

% LPS:  at(countLivingNeighbors([],0),_40454).
% Into: l_int(holds(countLivingNeighbors([],0),_40454),[]).

% LPS:  if(at(countLivingNeighbors([_41700-_41702|_41734],_41762),_41784),(at(_41700-_41702,_41784),at(countLivingNeighbors(_41734,_41978),_41784),_41762 is _41978+1)).
% Into: l_int(holds(countLivingNeighbors([_41700-_41702|_41734],_41762),_41784),[holds(_41700-_41702,_41784),holds(countLivingNeighbors(_41734,_41978),_41784),_41762 is _41978+1]).

% LPS:  if(at(countLivingNeighbors([_44352-_44354|_44386],_44414),_44436),(at(not(_44352-_44354),_44436),at(countLivingNeighbors(_44386,_44414),_44436))).
% Into: l_int(holds(countLivingNeighbors([_44352-_44354|_44386],_44414),_44436),[holds(not(_44352-_44354),_44436),holds(countLivingNeighbors(_44386,_44414),_44436)]).

% LPS:  initiates(live(_46596-_46598),_46596-_46598).
% Into: initiated(happens(live(_46596-_46598),_47830,_47836),_46596-_46598,[]).

% LPS:  terminates(die(_47772-_47774),_47772-_47774).
% Into: terminated(happens(die(_47772-_47774),_49006,_49012),_47772-_47774,[]).
% /pack/logicmoo_ec/test/lps_user_examples/Life.pl:64
% pop_lps_dialect('$BLOB'("<stream>(0x562ef76d8600)"),  (/.../(lps_user_examples, 'Life.pl')-> /.../(lps_user_examples, 'Life.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/Life.pl':_65572).


initiated(happens(live(A-B), _, _), A-B, []).

d(X-Y, [center:[XX, YY], radius:5, type:circle, fillColor:green]) :-
    XX is X*10,
    YY is Y*10.
d(live(X-Y), [type:star, center:[XX, YY], points:7, radius1:4, radius2:7, fillColor:red]) :-
    XX is X*10,
    YY is Y*10.
d(die(X-Y), [type:star, center:[XX, YY], points:7, radius1:4, radius2:7, fillColor:black]) :-
    XX is X*10,
    YY is Y*10.
d(timeless, [[type:raster, position:[50, 120], scale:0.08, source:'https://upload.wikimedia.org/wikipedia/commons/0/04/John_H_Conway_2005_%28cropped%29.jpg'], [type:text, point:[0, 5], content:'Conway\'s Game of Life']]).

fluents([_-_]).

l_int(holds(aliveNeighbors(A-B, C), D), [adjacent(A-B, E), holds(countLivingNeighbors(E, C), D)]).
l_int(holds(countLivingNeighbors([], 0), _), []).
l_int(holds(countLivingNeighbors([A-B|C], D), E), [holds(A-B, E), holds(countLivingNeighbors(C, F), E), D is F+1]).
l_int(holds(countLivingNeighbors([A-B|C], D), E), [holds(not(A-B), E), holds(countLivingNeighbors(C, D), E)]).

reactive_rule([holds(A-B, C), holds(aliveNeighbors(A-B, D), C), D<2], [happens(die(A-B), _, _)]).
reactive_rule([holds(A-B, C), holds(aliveNeighbors(A-B, D), C), D>3], [happens(die(A-B), _, _)]).
reactive_rule([cell(A, B), holds(aliveNeighbors(A-B, 3), _)], [happens(live(A-B), _, _)]).

cell(X, Y) :-
    Range=[1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
    member(X, Range),
    member(Y, Range).

initial_state([2-3, 3-3, 4-3]).

terminated(happens(die(A-B), _, _), A-B, []).

:- dynamic actions/1.
:- multifile actions/1.

actions([die(_), live(_)]).

adjacent(A-B, C) :-
    findall(D-E,
            ( member(F, [-1, 0, 1]),
              member(G, [-1, 0, 1]),
              not(( F=0,
                    G=0
                  )),
              D is A+F,
              E is B+G
            ),
            C).

maxTime(8).
% dB(/.../(lps_user_examples, 'Life.pl'), lps_visualization(_324786{groups:[_319022{content:"A-B", id:"(-)/2", order:3, subgroupStack:"false"}, _319088{content:"Actions", id:"action", order:4}], items:[_319210{content:"2,3", end:2, group:"(-)/2", id:0, start:1, subgroup:"2", title:"Fluent 2-3 initiated at 1<br/>and terminated at transition to 2"}, _319336{content:"2,3", end:4, group:"(-)/2", id:1, start:3, subgroup:"2", title:"Fluent 2-3 initiated at 3<br/>and terminated at transition to 4"}, _319462{content:"2,3", end:6, group:"(-)/2", id:2, start:5, subgroup:"2", title:"Fluent 2-3 initiated at 5<br/>and terminated at transition to 6"}, _319588{content:"2,3", end:8, group:"(-)/2", id:3, start:7, subgroup:"2", title:"Fluent 2-3 initiated at 7<br/>and terminated at transition to 8"}, _319714{content:"3,2", end:3, group:"(-)/2", id:4, start:2, subgroup:"3", title:"Fluent 3-2 initiated at 2<br/>and terminated at transition to 3"}, _319840{content:"3,2", end:5, group:"(-)/2", id:5, start:4, subgroup:"3", title:"Fluent 3-2 initiated at 4<br/>and terminated at transition to 5"}, _319966{content:"3,2", end:7, group:"(-)/2", id:6, start:6, subgroup:"3", title:"Fluent 3-2 initiated at 6<br/>and terminated at transition to 7"}, _320092{content:"3,2", end:9, group:"(-)/2", id:7, start:8, subgroup:"3", title:"Fluent 3-2 initiated at 8<br/>and terminated at transition to 9"}, _320218{content:"3,3", end:9, group:"(-)/2", id:8, start:1, subgroup:"3", title:"Fluent 3-3 initiated at 1<br/>and terminated at transition to 9"}, _320344{content:"3,4", end:3, group:"(-)/2", id:9, start:2, subgroup:"3", title:"Fluent 3-4 initiated at 2<br/>and terminated at transition to 3"}, _320470{content:"3,4", end:5, group:"(-)/2", id:10, start:4, subgroup:"3", title:"Fluent 3-4 initiated at 4<br/>and terminated at transition to 5"}, _320596{content:"3,4", end:7, group:"(-)/2", id:11, start:6, subgroup:"3", title:"Fluent 3-4 initiated at 6<br/>and terminated at transition to 7"}, _320722{content:"3,4", end:9, group:"(-)/2", id:12, start:8, subgroup:"3", title:"Fluent 3-4 initiated at 8<br/>and terminated at transition to 9"}, _320848{content:"4,3", end:2, group:"(-)/2", id:13, start:1, subgroup:"4", title:"Fluent 4-3 initiated at 1<br/>and terminated at transition to 2"}, _320974{content:"4,3", end:4, group:"(-)/2", id:14, start:3, subgroup:"4", title:"Fluent 4-3 initiated at 3<br/>and terminated at transition to 4"}, _321100{content:"4,3", end:6, group:"(-)/2", id:15, start:5, subgroup:"4", title:"Fluent 4-3 initiated at 5<br/>and terminated at transition to 6"}, _321226{content:"4,3", end:8, group:"(-)/2", id:16, start:7, subgroup:"4", title:"Fluent 4-3 initiated at 7<br/>and terminated at transition to 8"}, _321352{content:"live(3-4)", group:"action", id:17, start:2, style:"color:green", title:"happens(live(3-4),1,2)", type:"point"}, _321478{content:"live(3-2)", group:"action", id:18, start:2, style:"color:green", title:"happens(live(3-2),1,2)", type:"point"}, _321604{content:"die(4-3)", group:"action", id:19, start:2, style:"color:green", title:"happens(die(4-3),1,2)", type:"point"}, _321730{content:"die(2-3)", group:"action", id:20, start:2, style:"color:green", title:"happens(die(2-3),1,2)", type:"point"}, _321856{content:"die(3-2)", group:"action", id:21, start:3, style:"color:green", title:"happens(die(3-2),2,3)", type:"point"}, _321982{content:"die(3-4)", group:"action", id:22, start:3, style:"color:green", title:"happens(die(3-4),2,3)", type:"point"}, _322108{content:"live(2-3)", group:"action", id:23, start:3, style:"color:green", title:"happens(live(2-3),2,3)", type:"point"}, _322234{content:"live(4-3)", group:"action", id:24, start:3, style:"color:green", title:"happens(live(4-3),2,3)", type:"point"}, _322360{content:"live(3-4)", group:"action", id:25, start:4, style:"color:green", title:"happens(live(3-4),3,4)", type:"point"}, _322486{content:"live(3-2)", group:"action", id:26, start:4, style:"color:green", title:"happens(live(3-2),3,4)", type:"point"}, _322612{content:"die(4-3)", group:"action", id:27, start:4, style:"color:green", title:"happens(die(4-3),3,4)", type:"point"}, _322738{content:"die(2-3)", group:"action", id:28, start:4, style:"color:green", title:"happens(die(2-3),3,4)", type:"point"}, _322864{content:"die(3-2)", group:"action", id:29, start:5, style:"color:green", title:"happens(die(3-2),4,5)", type:"point"}, _322990{content:"die(3-4)", group:"action", id:30, start:5, style:"color:green", title:"happens(die(3-4),4,5)", type:"point"}, _323116{content:"live(2-3)", group:"action", id:31, start:5, style:"color:green", title:"happens(live(2-3),4,5)", type:"point"}, _323242{content:"live(4-3)", group:"action", id:32, start:5, style:"color:green", title:"happens(live(4-3),4,5)", type:"point"}, _323368{content:"live(3-4)", group:"action", id:33, start:6, style:"color:green", title:"happens(live(3-4),5,6)", type:"point"}, _323494{content:"live(3-2)", group:"action", id:34, start:6, style:"color:green", title:"happens(live(3-2),5,6)", type:"point"}, _323620{content:"die(4-3)", group:"action", id:35, start:6, style:"color:green", title:"happens(die(4-3),5,6)", type:"point"}, _323746{content:"die(2-3)", group:"action", id:36, start:6, style:"color:green", title:"happens(die(2-3),5,6)", type:"point"}, _323872{content:"die(3-2)", group:"action", id:37, start:7, style:"color:green", title:"happens(die(3-2),6,7)", type:"point"}, _323998{content:"die(3-4)", group:"action", id:38, start:7, style:"color:green", title:"happens(die(3-4),6,7)", type:"point"}, _324124{content:"live(2-3)", group:"action", id:39, start:7, style:"color:green", title:"happens(live(2-3),6,7)", type:"point"}, _324250{content:"live(4-3)", group:"action", id:40, start:7, style:"color:green", title:"happens(live(4-3),6,7)", type:"point"}, _324376{content:"live(3-4)", group:"action", id:41, start:8, style:"color:green", title:"happens(live(3-4),7,8)", type:"point"}, _324502{content:"live(3-2)", group:"action", id:42, start:8, style:"color:green", title:"happens(live(3-2),7,8)", type:"point"}, _324628{content:"die(4-3)", group:"action", id:43, start:8, style:"color:green", title:"happens(die(4-3),7,8)", type:"point"}, _324754{content:"die(2-3)", group:"action", id:44, start:8, style:"color:green", title:"happens(die(2-3),7,8)", type:"point"}]}, _396472{cycles:[[_387836{create:[_387722{id:"timeless", position:[50, 120], scale:0.08, source:"https://upload.wikimedia.org/wikipedia/commons/0/04/John_H_Conway_2005_%28cropped%29.jpg", type:"raster"}, _387816{content:"Conway's Game of Life", id:"timeless", point:[0, 5], type:"text"}]}], [_387972{create:_387948{center:[20, 30], fillColor:"green", id:"2-3", radius:5, type:"circle"}}, _388102{create:_388078{center:[30, 30], fillColor:"green", id:"3-3", radius:5, type:"circle"}}, _388232{create:_388208{center:[40, 30], fillColor:"green", id:"4-3", radius:5, type:"circle"}}, _388410{create:_388374{center:[20, 30], event:"true", fillColor:"black", id:"die(2-3)", points:7, radius1:4, radius2:7, type:"star"}}, _388588{create:_388552{center:[40, 30], event:"true", fillColor:"black", id:"die(4-3)", points:7, radius1:4, radius2:7, type:"star"}}, _388766{create:_388730{center:[30, 20], event:"true", fillColor:"red", id:"live(3-2)", points:7, radius1:4, radius2:7, type:"star"}}, _388944{create:_388908{center:[30, 40], event:"true", fillColor:"red", id:"live(3-4)", points:7, radius1:4, radius2:7, type:"star"}}], [_389080{create:_389056{center:[30, 20], fillColor:"green", id:"3-2", radius:5, type:"circle"}}, _389210{create:_389186{center:[30, 40], fillColor:"green", id:"3-4", radius:5, type:"circle"}}, _389388{create:_389352{center:[30, 20], event:"true", fillColor:"black", id:"die(3-2)", points:7, radius1:4, radius2:7, type:"star"}}, _389566{create:_389530{center:[30, 40], event:"true", fillColor:"black", id:"die(3-4)", points:7, radius1:4, radius2:7, type:"star"}}, _389744{create:_389708{center:[20, 30], event:"true", fillColor:"red", id:"live(2-3)", points:7, radius1:4, radius2:7, type:"star"}}, _389922{create:_389886{center:[40, 30], event:"true", fillColor:"red", id:"live(4-3)", points:7, radius1:4, radius2:7, type:"star"}}, _389952{kill:"2-3"}, _389982{kill:"4-3"}, _390012{kill:"die(2-3)"}, _390042{kill:"die(4-3)"}, _390072{kill:"live(3-2)"}, _390102{kill:"live(3-4)"}], [_390238{create:_390214{center:[20, 30], fillColor:"green", id:"2-3", radius:5, type:"circle"}}, _390368{create:_390344{center:[40, 30], fillColor:"green", id:"4-3", radius:5, type:"circle"}}, _390546{create:_390510{center:[20, 30], event:"true", fillColor:"black", id:"die(2-3)", points:7, radius1:4, radius2:7, type:"star"}}, _390724{create:_390688{center:[40, 30], event:"true", fillColor:"black", id:"die(4-3)", points:7, radius1:4, radius2:7, type:"star"}}, _390902{create:_390866{center:[30, 20], event:"true", fillColor:"red", id:"live(3-2)", points:7, radius1:4, radius2:7, type:"star"}}, _391080{create:_391044{center:[30, 40], event:"true", fillColor:"red", id:"live(3-4)", points:7, radius1:4, radius2:7, type:"star"}}, _391110{kill:"3-2"}, _391140{kill:"3-4"}, _391170{kill:"die(3-2)"}, _391200{kill:"die(3-4)"}, _391230{kill:"live(2-3)"}, _391260{kill:"live(4-3)"}], [_391396{create:_391372{center:[30, 20], fillColor:"green", id:"3-2", radius:5, type:"circle"}}, _391526{create:_391502{center:[30, 40], fillColor:"green", id:"3-4", radius:5, type:"circle"}}, _391704{create:_391668{center:[30, 20], event:"true", fillColor:"black", id:"die(3-2)", points:7, radius1:4, radius2:7, type:"star"}}, _391882{create:_391846{center:[30, 40], event:"true", fillColor:"black", id:"die(3-4)", points:7, radius1:4, radius2:7, type:"star"}}, _392060{create:_392024{center:[20, 30], event:"true", fillColor:"red", id:"live(2-3)", points:7, radius1:4, radius2:7, type:"star"}}, _392238{create:_392202{center:[40, 30], event:"true", fillColor:"red", id:"live(4-3)", points:7, radius1:4, radius2:7, type:"star"}}, _392268{kill:"2-3"}, _392298{kill:"4-3"}, _392328{kill:"die(2-3)"}, _392358{kill:"die(4-3)"}, _392388{kill:"live(3-2)"}, _392418{kill:"live(3-4)"}], [_392554{create:_392530{center:[20, 30], fillColor:"green", id:"2-3", radius:5, type:"circle"}}, _392684{create:_392660{center:[40, 30], fillColor:"green", id:"4-3", radius:5, type:"circle"}}, _392862{create:_392826{center:[20, 30], event:"true", fillColor:"black", id:"die(2-3)", points:7, radius1:4, radius2:7, type:"star"}}, _393040{create:_393004{center:[40, 30], event:"true", fillColor:"black", id:"die(4-3)", points:7, radius1:4, radius2:7, type:"star"}}, _393218{create:_393182{center:[30, 20], event:"true", fillColor:"red", id:"live(3-2)", points:7, radius1:4, radius2:7, type:"star"}}, _393396{create:_393360{center:[30, 40], event:"true", fillColor:"red", id:"live(3-4)", points:7, radius1:4, radius2:7, type:"star"}}, _393426{kill:"3-2"}, _393456{kill:"3-4"}, _393486{kill:"die(3-2)"}, _393516{kill:"die(3-4)"}, _393546{kill:"live(2-3)"}, _393576{kill:"live(4-3)"}], [_393712{create:_393688{center:[30, 20], fillColor:"green", id:"3-2", radius:5, type:"circle"}}, _393842{create:_393818{center:[30, 40], fillColor:"green", id:"3-4", radius:5, type:"circle"}}, _394020{create:_393984{center:[30, 20], event:"true", fillColor:"black", id:"die(3-2)", points:7, radius1:4, radius2:7, type:"star"}}, _394198{create:_394162{center:[30, 40], event:"true", fillColor:"black", id:"die(3-4)", points:7, radius1:4, radius2:7, type:"star"}}, _394376{create:_394340{center:[20, 30], event:"true", fillColor:"red", id:"live(2-3)", points:7, radius1:4, radius2:7, type:"star"}}, _394554{create:_394518{center:[40, 30], event:"true", fillColor:"red", id:"live(4-3)", points:7, radius1:4, radius2:7, type:"star"}}, _394584{kill:"2-3"}, _394614{kill:"4-3"}, _394644{kill:"die(2-3)"}, _394674{kill:"die(4-3)"}, _394704{kill:"live(3-2)"}, _394734{kill:"live(3-4)"}], [_394870{create:_394846{center:[20, 30], fillColor:"green", id:"2-3", radius:5, type:"circle"}}, _395000{create:_394976{center:[40, 30], fillColor:"green", id:"4-3", radius:5, type:"circle"}}, _395178{create:_395142{center:[20, 30], event:"true", fillColor:"black", id:"die(2-3)", points:7, radius1:4, radius2:7, type:"star"}}, _395356{create:_395320{center:[40, 30], event:"true", fillColor:"black", id:"die(4-3)", points:7, radius1:4, radius2:7, type:"star"}}, _395534{create:_395498{center:[30, 20], event:"true", fillColor:"red", id:"live(3-2)", points:7, radius1:4, radius2:7, type:"star"}}, _395712{create:_395676{center:[30, 40], event:"true", fillColor:"red", id:"live(3-4)", points:7, radius1:4, radius2:7, type:"star"}}, _395742{kill:"3-2"}, _395772{kill:"3-4"}, _395802{kill:"die(3-2)"}, _395832{kill:"die(3-4)"}, _395862{kill:"live(2-3)"}, _395892{kill:"live(4-3)"}], [_396028{create:_396004{center:[30, 20], fillColor:"green", id:"3-2", radius:5, type:"circle"}}, _396158{create:_396134{center:[30, 40], fillColor:"green", id:"3-4", radius:5, type:"circle"}}, _396188{kill:"2-3"}, _396218{kill:"4-3"}, _396248{kill:"die(2-3)"}, _396278{kill:"die(4-3)"}, _396308{kill:"live(3-2)"}, _396338{kill:"live(3-4)"}], [_396374{kill:"3-2"}, _396404{kill:"3-3"}, _396434{kill:"3-4"}, _396464{kill:"timeless"}]]})).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'LP.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'LP.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/LP.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'LP.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'LP.pl'), lps= /.../(lps_user_examples, 'LP.pl'), using= /.../(lps_user_examples, 'LP.pl')].
% continue_lps_dialect.
% ops.
% /pack/logicmoo_ec/test/lps_user_examples/LP.pl:8
% pop_lps_dialect('$BLOB'("<stream>(0x562ef70c0000)"),  (/.../(lps_user_examples, 'LP.pl')-> /.../(lps_user_examples, 'LP.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/LP.pl':_89500).


:- dynamic actions/1.
:- multifile actions/1.


insere_ordenado(N, [], [N]).
insere_ordenado(N, [P|R], [N, P|R]) :-
    N<P.
insere_ordenado(N, [P|R], [P|Temp]) :-
    N>=P,
    insere_ordenado(N, R, Temp).
% dB(/.../(lps_user_examples, 'LP.pl'), lps_visualization(_39366{groups:[], items:[]}, [])).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'martianrobotl.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'martianrobotl.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/martianrobotl.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'martianrobotl.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'martianrobotl.pl'), lps= /.../(lps_user_examples, 'martianrobotl.pl'), using= /.../(lps_user_examples, 'martianrobotl.pl')].
% continue_lps_dialect.
% ops.

% LPS:  fluents((en/2,libre/2,estuve/2,obstaculo/2,vida/2,mirada/2)).
% Into: fluents([en(_21318,_21320),libre(_21330,_21332),estuve(_21342,_21344),obstaculo(_21354,_21356),vida(_21366,_21368),mirada(_21378,_21380)]).

% LPS:  actions((ir(_21118,_21120),derecha,reporte)).
% Into: actions([ir(_21118,_21120),derecha,reporte]).

% LPS:  initially((en(0,0),vida(2,1),libre(1,0),libre(2,0),obstaculo(3,0),obstaculo(2,-1),obstaculo(2,1),mirada(1,0))).
% Into: initial_state([en(0,0),vida(2,1),libre(1,0),libre(2,0),obstaculo(3,0),obstaculo(2,-1),obstaculo(2,1),mirada(1,0)]).

% LPS:  terminates(ir(_24030,_24032),mirada(_24030,_24032)).
% Into: terminated(happens(ir(_24030,_24032),_25224,_25230),mirada(_24030,_24032),[]).

% LPS:  if(initiates(ir(_25162,_25164),mirada(_25162,_25220)),(en(_25162,_25308),mirada(_25162,_25164),_25448 is _25164-_25308,abs(_25448)>0,_25220 is _25164+_25448)).
% Into: initiated(happens(ir(_25162,_25164),_27062,_27068),mirada(_25162,_25220),[holds(en(_25162,_25308),_27062),holds(mirada(_25162,_25164),_27062),_25448 is _25164-_25308,abs(_25448)>0,_25220 is _25164+_25448]).

% LPS:  if(initiates(ir(_28550,_28552),mirada(_28606,_28552)),(en(_28694,_28552),mirada(_28550,_28552),_28836 is _28550-_28694,abs(_28836)>0,_28606 is _28550+_28836)).
% Into: initiated(happens(ir(_28550,_28552),_30450,_30456),mirada(_28606,_28552),[holds(en(_28694,_28552),_30450),holds(mirada(_28550,_28552),_30450),_28836 is _28550-_28694,abs(_28836)>0,_28606 is _28550+_28836]).

% LPS:  initiates(ir(_31938,_31940),en(_31938,_31940)).
% Into: initiated(happens(ir(_31938,_31940),_33124,_33130),en(_31938,_31940),[]).

% LPS:  terminates(ir(_33070,_33072),en(_33126,_33128)).
% Into: terminated(happens(ir(_33070,_33072),_34280,_34286),en(_33126,_33128),[]).

% LPS:  terminates(ir(_34262,_34264),libre(_34262,_34264)).
% Into: terminated(happens(ir(_34262,_34264),_35448,_35454),libre(_34262,_34264),[]).

% LPS:  if(initiates(ir(_35394,_35396),estuve(_35450,_35452)),en(_35450,_35452)).
% Into: initiated(happens(ir(_35394,_35396),_36722,_36728),estuve(_35450,_35452),[holds(en(_35450,_35452),_36722)]).

% LPS:  if(initiates(ir(_36982,_36984),libre(_37038,_37040)),en(_37038,_37040)).
% Into: initiated(happens(ir(_36982,_36984),_38310,_38316),libre(_37038,_37040),[holds(en(_37038,_37040),_38310)]).

% LPS:  terminates(derecha,mirada(_38586,_38588)).
% Into: terminated(happens(derecha,_39712,_39718),mirada(_38586,_38588),[]).

% LPS:  if(initiates(derecha,mirada(_39672,_39674)),(en(_39672,_39762),mirada(_39816,_39762),_39816 is _39672+1,_39674 is _39762-1)).
% Into: initiated(happens(derecha,_41356,_41362),mirada(_39672,_39674),[holds(en(_39672,_39762),_41356),holds(mirada(_39816,_39762),_41356),_39816 is _39672+1,_39674 is _39762-1]).

% LPS:  if(initiates(derecha,mirada(_42512,_42514)),(en(_42600,_42514),mirada(_42600,_42658),_42512 is _42600-1,_42658 is _42514-1)).
% Into: initiated(happens(derecha,_44196,_44202),mirada(_42512,_42514),[holds(en(_42600,_42514),_44196),holds(mirada(_42600,_42658),_44196),_42512 is _42600-1,_42658 is _42514-1]).

% LPS:  if(initiates(derecha,mirada(_45352,_45354)),(en(_45352,_45442),mirada(_45496,_45442),_45496 is _45352-1,_45354 is _45442+1)).
% Into: initiated(happens(derecha,_47036,_47042),mirada(_45352,_45354),[holds(en(_45352,_45442),_47036),holds(mirada(_45496,_45442),_47036),_45496 is _45352-1,_45354 is _45442+1]).

% LPS:  if(initiates(derecha,mirada(_48192,_48194)),(en(_48280,_48194),mirada(_48280,_48338),_48192 is _48280+1,_48338 is _48194+1)).
% Into: initiated(happens(derecha,_49876,_49882),mirada(_48192,_48194),[holds(en(_48280,_48194),_49876),holds(mirada(_48280,_48338),_49876),_48192 is _48280+1,_48338 is _48194+1]).

% LPS:  then(if((mirada(_51168,_51170),libre(_51168,_51170),not(estuve(_51168,_51170)))),ir(_51168,_51170)).
% Into: reactive_rule([holds(mirada(_51168,_51170),_52538),holds(libre(_51168,_51170),_52538),holds(not(estuve(_51168,_51170)),_52538)],[happens(ir(_51168,_51170),_53386,_53392)]).

% LPS:  then(if((mirada(_53512,_53514),libre(_53512,_53514),estuve(_53512,_53514))),derecha).
% Into: reactive_rule([holds(mirada(_53512,_53514),_54808),holds(libre(_53512,_53514),_54808),holds(estuve(_53512,_53514),_54808)],[happens(derecha,_55606,_55612)]).

% LPS:  then(if((mirada(_55780,_55782),obstaculo(_55780,_55782),not(vida(_55780,_55782)))),derecha).
% Into: reactive_rule([holds(mirada(_55780,_55782),_57104),holds(obstaculo(_55780,_55782),_57104),holds(not(vida(_55780,_55782)),_57104)],[happens(derecha,_57952,_57958)]).

% LPS:  then(if((mirada(_58112,_58114),obstaculo(_58112,_58114),vida(_58112,_58114))),(reporte,derecha)).
% Into: reactive_rule([holds(mirada(_58112,_58114),_59462),holds(obstaculo(_58112,_58114),_59462),holds(vida(_58112,_58114),_59462)],[happens(reporte,_60260,_60302),happens(derecha,_60302,_60266)]).
% /pack/logicmoo_ec/test/lps_user_examples/martianrobotl.pl:84
% pop_lps_dialect('$BLOB'("<stream>(0x562ef73e5800)"),  (/.../(lps_user_examples, 'martianrobotl.pl')-> /.../(lps_user_examples, 'martianrobotl.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/martianrobotl.pl':_68146).


initiated(happens(ir(A, B), C, _), mirada(A, D), [holds(en(A, E), C), holds(mirada(A, B), C), F is B-E, abs(F)>0, D is B+F]).
initiated(happens(ir(A, B), C, _), mirada(D, B), [holds(en(E, B), C), holds(mirada(A, B), C), F is A-E, abs(F)>0, D is A+F]).
initiated(happens(ir(A, B), _, _), en(A, B), []).
initiated(happens(ir(_, _), A, _), estuve(B, C), [holds(en(B, C), A)]).
initiated(happens(ir(_, _), A, _), libre(B, C), [holds(en(B, C), A)]).
initiated(happens(derecha, A, _), mirada(B, C), [holds(en(B, D), A), holds(mirada(E, D), A), E is B+1, C is D-1]).
initiated(happens(derecha, A, _), mirada(B, C), [holds(en(D, C), A), holds(mirada(D, E), A), B is D-1, E is C-1]).
initiated(happens(derecha, A, _), mirada(B, C), [holds(en(B, D), A), holds(mirada(E, D), A), E is B-1, C is D+1]).
initiated(happens(derecha, A, _), mirada(B, C), [holds(en(D, C), A), holds(mirada(D, E), A), B is D+1, E is C+1]).

fluents([en(_, _), libre(_, _), estuve(_, _), obstaculo(_, _), vida(_, _), mirada(_, _)]).

terminated(happens(ir(A, B), _, _), mirada(A, B), []).
terminated(happens(ir(_, _), _, _), en(_, _), []).
terminated(happens(ir(A, B), _, _), libre(A, B), []).
terminated(happens(derecha, _, _), mirada(_, _), []).

reactive_rule([holds(mirada(A, B), C), holds(libre(A, B), C), holds(not(estuve(A, B)), C)], [happens(ir(A, B), _, _)]).
reactive_rule([holds(mirada(A, B), C), holds(libre(A, B), C), holds(estuve(A, B), C)], [happens(derecha, _, _)]).
reactive_rule([holds(mirada(A, B), C), holds(obstaculo(A, B), C), holds(not(vida(A, B)), C)], [happens(derecha, _, _)]).
reactive_rule([holds(mirada(A, B), C), holds(obstaculo(A, B), C), holds(vida(A, B), C)], [happens(reporte, _, D), happens(derecha, D, _)]).

initial_state([en(0, 0), vida(2, 1), libre(1, 0), libre(2, 0), obstaculo(3, 0), obstaculo(2, -1), obstaculo(2, 1), mirada(1, 0)]).

:- dynamic actions/1.
:- multifile actions/1.

actions([ir(_, _), derecha, reporte]).

maxTime(10).
% dB(/.../(lps_user_examples, 'martianrobotl.pl'), lps_visualization(_90764{groups:[_86248{content:"en(A,B)", id:"en/2", order:3, subgroupStack:"false"}, _86326{content:"estuve(A,B)", id:"estuve/2", order:3, subgroupStack:"false"}, _86404{content:"libre(A,B)", id:"libre/2", order:3, subgroupStack:"false"}, _86482{content:"mirada(A,B)", id:"mirada/2", order:3, subgroupStack:"false"}, _86560{content:"obstaculo(A,B)", id:"obstaculo/2", order:3, subgroupStack:"false"}, _86638{content:"vida(A,B)", id:"vida/2", order:3, subgroupStack:"false"}, _86704{content:"Actions", id:"action", order:4}], items:[_86826{content:"0,0", end:2, group:"en/2", id:0, start:1, subgroup:"0", title:"Fluent en(0,0) initiated at 1<br/>and terminated at transition to 2"}, _86952{content:"1,0", end:3, group:"en/2", id:1, start:2, subgroup:"1", title:"Fluent en(1,0) initiated at 2<br/>and terminated at transition to 3"}, _87078{content:"2,0", end:11, group:"en/2", id:2, start:3, subgroup:"2", title:"Fluent en(2,0) initiated at 3<br/>and terminated at transition to 11"}, _87204{content:"0,0", end:11, group:"estuve/2", id:3, start:2, subgroup:"0", title:"Fluent estuve(0,0) initiated at 2<br/>and terminated at transition to 11"}, _87330{content:"1,0", end:11, group:"estuve/2", id:4, start:3, subgroup:"1", title:"Fluent estuve(1,0) initiated at 3<br/>and terminated at transition to 11"}, _87456{content:"0,0", end:11, group:"libre/2", id:5, start:2, subgroup:"0", title:"Fluent libre(0,0) initiated at 2<br/>and terminated at transition to 11"}, _87582{content:"1,0", end:2, group:"libre/2", id:6, start:1, subgroup:"1", title:"Fluent libre(1,0) initiated at 1<br/>and terminated at transition to 2"}, _87708{content:"1,0", end:11, group:"libre/2", id:7, start:3, subgroup:"1", title:"Fluent libre(1,0) initiated at 3<br/>and terminated at transition to 11"}, _87834{content:"2,0", end:3, group:"libre/2", id:8, start:1, subgroup:"2", title:"Fluent libre(2,0) initiated at 1<br/>and terminated at transition to 3"}, _87960{content:"1,0", end:2, group:"mirada/2", id:9, start:1, subgroup:"1", title:"Fluent mirada(1,0) initiated at 1<br/>and terminated at transition to 2"}, _88086{content:"1,0", end:6, group:"mirada/2", id:10, start:5, subgroup:"1", title:"Fluent mirada(1,0) initiated at 5<br/>and terminated at transition to 6"}, _88212{content:"1,0", end:11, group:"mirada/2", id:11, start:10, subgroup:"1", title:"Fluent mirada(1,0) initiated at 10<br/>and terminated at transition to 11"}, _88338{content:"2,-1", end:5, group:"mirada/2", id:12, start:4, subgroup:"2", title:"Fluent mirada(2,-1) initiated at 4<br/>and terminated at transition to 5"}, _88464{content:"2,-1", end:10, group:"mirada/2", id:13, start:9, subgroup:"2", title:"Fluent mirada(2,-1) initiated at 9<br/>and terminated at transition to 10"}, _88590{content:"2,0", end:3, group:"mirada/2", id:14, start:2, subgroup:"2", title:"Fluent mirada(2,0) initiated at 2<br/>and terminated at transition to 3"}, _88716{content:"2,1", end:8, group:"mirada/2", id:15, start:6, subgroup:"2", title:"Fluent mirada(2,1) initiated at 6<br/>and terminated at transition to 8"}, _88842{content:"3,0", end:4, group:"mirada/2", id:16, start:3, subgroup:"3", title:"Fluent mirada(3,0) initiated at 3<br/>and terminated at transition to 4"}, _88968{content:"3,0", end:9, group:"mirada/2", id:17, start:8, subgroup:"3", title:"Fluent mirada(3,0) initiated at 8<br/>and terminated at transition to 9"}, _89094{content:"2,-1", end:11, group:"obstaculo/2", id:18, start:1, subgroup:"2", title:"Fluent obstaculo(2,-1) initiated at 1<br/>and terminated at transition to 11"}, _89220{content:"2,1", end:11, group:"obstaculo/2", id:19, start:1, subgroup:"2", title:"Fluent obstaculo(2,1) initiated at 1<br/>and terminated at transition to 11"}, _89346{content:"3,0", end:11, group:"obstaculo/2", id:20, start:1, subgroup:"3", title:"Fluent obstaculo(3,0) initiated at 1<br/>and terminated at transition to 11"}, _89472{content:"2,1", end:11, group:"vida/2", id:21, start:1, subgroup:"2", title:"Fluent vida(2,1) initiated at 1<br/>and terminated at transition to 11"}, _89598{content:"ir(1,0)", group:"action", id:22, start:2, style:"color:green", title:"happens(ir(1,0),1,2)", type:"point"}, _89724{content:"ir(2,0)", group:"action", id:23, start:3, style:"color:green", title:"happens(ir(2,0),2,3)", type:"point"}, _89850{content:"derecha", group:"action", id:24, start:4, style:"color:green", title:"happens(derecha,3,4)", type:"point"}, _89976{content:"derecha", group:"action", id:25, start:5, style:"color:green", title:"happens(derecha,4,5)", type:"point"}, _90102{content:"derecha", group:"action", id:26, start:6, style:"color:green", title:"happens(derecha,5,6)", type:"point"}, _90228{content:"reporte", group:"action", id:27, start:7, style:"color:green", title:"happens(reporte,6,7)", type:"point"}, _90354{content:"derecha", group:"action", id:28, start:8, style:"color:green", title:"happens(derecha,7,8)", type:"point"}, _90480{content:"reporte", group:"action", id:29, start:8, style:"color:green", title:"happens(reporte,7,8)", type:"point"}, _90606{content:"derecha", group:"action", id:30, start:9, style:"color:green", title:"happens(derecha,8,9)", type:"point"}, _90732{content:"derecha", group:"action", id:31, start:10, style:"color:green", title:"happens(derecha,9,10)", type:"point"}]}, [])).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'martianrobot.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'martianrobot.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/martianrobot.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'martianrobot.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'martianrobot.pl'), lps= /.../(lps_user_examples, 'martianrobot.pl'), using= /.../(lps_user_examples, 'martianrobot.pl')].
% continue_lps_dialect.
% ops.

% LPS:  fluents((at_pos/2,free/2,visited/2,obstacle/2,life/2,lookingtowards/2)).
% Into: fluents([at_pos(_63616,_63618),free(_63628,_63630),visited(_63640,_63642),obstacle(_63652,_63654),life(_63664,_63666),lookingtowards(_63676,_63678)]).

% LPS:  actions((step(_63416,_63418),turn_right,report)).
% Into: actions([step(_63416,_63418),turn_right,report]).

% LPS:  initially((at_pos(0,0),life(2,1),free(1,0),free(2,0),obstacle(3,0),obstacle(2,-1),obstacle(2,1),lookingtowards(1,0))).
% Into: initial_state([at_pos(0,0),life(2,1),free(1,0),free(2,0),obstacle(3,0),obstacle(2,-1),obstacle(2,1),lookingtowards(1,0)]).

% LPS:  terminates(step(_66328,_66330),lookingtowards(_66328,_66330)).
% Into: terminated(happens(step(_66328,_66330),_67514,_67520),lookingtowards(_66328,_66330),[]).

% LPS:  if(initiates(step(_67460,_67462),lookingtowards(_67460,_67518)),(at_pos(_67460,_67606),lookingtowards(_67460,_67462),_67746 is _67462-_67606,abs(_67746)>0,_67518 is _67462+_67746)).
% Into: initiated(happens(step(_67460,_67462),_69360,_69366),lookingtowards(_67460,_67518),[holds(at_pos(_67460,_67606),_69360),holds(lookingtowards(_67460,_67462),_69360),_67746 is _67462-_67606,abs(_67746)>0,_67518 is _67462+_67746]).

% LPS:  if(initiates(step(_70848,_70850),lookingtowards(_70904,_70850)),(at_pos(_70992,_70850),lookingtowards(_70848,_70850),_71134 is _70848-_70992,abs(_71134)>0,_70904 is _70848+_71134)).
% Into: initiated(happens(step(_70848,_70850),_72748,_72754),lookingtowards(_70904,_70850),[holds(at_pos(_70992,_70850),_72748),holds(lookingtowards(_70848,_70850),_72748),_71134 is _70848-_70992,abs(_71134)>0,_70904 is _70848+_71134]).

% LPS:  initiates(step(_74236,_74238),at_pos(_74236,_74238)).
% Into: initiated(happens(step(_74236,_74238),_75422,_75428),at_pos(_74236,_74238),[]).

% LPS:  if(terminates(step(_75368,_75370),at_pos(_75424,_75426)),at_pos(_75424,_75426)).
% Into: terminated(happens(step(_75368,_75370),_76696,_76702),at_pos(_75424,_75426),[holds(at_pos(_75424,_75426),_76696)]).

% LPS:  terminates(step(_76988,_76990),free(_76988,_76990)).
% Into: terminated(happens(step(_76988,_76990),_78174,_78180),free(_76988,_76990),[]).

% LPS:  if(initiates(step(_78120,_78122),visited(_78176,_78178)),at_pos(_78176,_78178)).
% Into: initiated(happens(step(_78120,_78122),_79448,_79454),visited(_78176,_78178),[holds(at_pos(_78176,_78178),_79448)]).

% LPS:  if(initiates(step(_79708,_79710),free(_79764,_79766)),at_pos(_79764,_79766)).
% Into: initiated(happens(step(_79708,_79710),_81036,_81042),free(_79764,_79766),[holds(at_pos(_79764,_79766),_81036)]).

% LPS:  terminates(turn_right,lookingtowards(_81312,_81314)).
% Into: terminated(happens(turn_right,_82430,_82436),lookingtowards(_81312,_81314),[]).

% LPS:  if(initiates(turn_right,lookingtowards(_82398,_82400)),(at_pos(_82398,_82488),lookingtowards(_82542,_82488),_82542 is _82398+1,_82400 is _82488-1)).
% Into: initiated(happens(turn_right,_84082,_84088),lookingtowards(_82398,_82400),[holds(at_pos(_82398,_82488),_84082),holds(lookingtowards(_82542,_82488),_84082),_82542 is _82398+1,_82400 is _82488-1]).

% LPS:  if(initiates(turn_right,lookingtowards(_85238,_85240)),(at_pos(_85326,_85240),lookingtowards(_85326,_85384),_85238 is _85326-1,_85384 is _85240-1)).
% Into: initiated(happens(turn_right,_86922,_86928),lookingtowards(_85238,_85240),[holds(at_pos(_85326,_85240),_86922),holds(lookingtowards(_85326,_85384),_86922),_85238 is _85326-1,_85384 is _85240-1]).

% LPS:  if(initiates(turn_right,lookingtowards(_88078,_88080)),(at_pos(_88078,_88168),lookingtowards(_88222,_88168),_88222 is _88078-1,_88080 is _88168+1)).
% Into: initiated(happens(turn_right,_89762,_89768),lookingtowards(_88078,_88080),[holds(at_pos(_88078,_88168),_89762),holds(lookingtowards(_88222,_88168),_89762),_88222 is _88078-1,_88080 is _88168+1]).

% LPS:  if(initiates(turn_right,lookingtowards(_19292,_19294)),(at_pos(_19364,_19294),lookingtowards(_19364,_19408),_19292 is _19364+1,_19408 is _19294+1)).
% Into: initiated(happens(turn_right,_20104,_20110),lookingtowards(_19292,_19294),[holds(at_pos(_19364,_19294),_20104),holds(lookingtowards(_19364,_19408),_20104),_19292 is _19364+1,_19408 is _19294+1]).

% LPS:  false((at_pos(_21288,_21290),step(_21288,_21290))).
% Into: d_pre([holds(at_pos(_21288,_21290),_22414),happens(step(_21288,_21290),_22414,_22420)]).

% LPS:  then(if((at(lookingtowards(_22736,_22738),_22760),at(free(_22736,_22738),_22760),at(not(visited(_22736,_22738)),_22760))),from(step(_22736,_22738),to(_22760,_23200))).
% Into: reactive_rule([holds(lookingtowards(_22736,_22738),_22760),holds(free(_22736,_22738),_22760),holds(not(visited(_22736,_22738)),_22760)],[happens(step(_22736,_22738),_22760,_23200)]).

% LPS:  then(if((at(lookingtowards(_25814,_25816),_25838),at(free(_25814,_25816),_25838),at(visited(_25814,_25816),_25838))),from(turn_right,to(_25838,_26214))).
% Into: reactive_rule([holds(lookingtowards(_25814,_25816),_25838),holds(free(_25814,_25816),_25838),holds(visited(_25814,_25816),_25838)],[happens(turn_right,_25838,_26214)]).

% LPS:  then(if((at(lookingtowards(_28852,_28854),_28876),at(obstacle(_28852,_28854),_28876))),turn_right).
% Into: reactive_rule([holds(lookingtowards(_28852,_28854),_28876),holds(obstacle(_28852,_28854),_28876)],[happens(turn_right,_31088,_31094)]).

% LPS:  then(if((at(lookingtowards(_31106,_31108),_31130),at(obstacle(_31106,_31108),_31130),at(life(_31106,_31108),_31130))),report).
% Into: reactive_rule([holds(lookingtowards(_31106,_31108),_31130),holds(obstacle(_31106,_31108),_31130),holds(life(_31106,_31108),_31130)],[happens(report,_33972,_33978)]).
% /pack/logicmoo_ec/test/lps_user_examples/martianrobot.pl:72
% pop_lps_dialect('$BLOB'("<stream>(0x562ef73e2400)"),  (/.../(lps_user_examples, 'martianrobot.pl')-> /.../(lps_user_examples, 'martianrobot.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/martianrobot.pl':_41858).


initiated(happens(step(A, B), C, _), lookingtowards(A, D), [holds(at_pos(A, E), C), holds(lookingtowards(A, B), C), F is B-E, abs(F)>0, D is B+F]).
initiated(happens(step(A, B), C, _), lookingtowards(D, B), [holds(at_pos(E, B), C), holds(lookingtowards(A, B), C), F is A-E, abs(F)>0, D is A+F]).
initiated(happens(step(A, B), _, _), at_pos(A, B), []).
initiated(happens(step(_, _), A, _), visited(B, C), [holds(at_pos(B, C), A)]).
initiated(happens(step(_, _), A, _), free(B, C), [holds(at_pos(B, C), A)]).
initiated(happens(turn_right, A, _), lookingtowards(B, C), [holds(at_pos(B, D), A), holds(lookingtowards(E, D), A), E is B+1, C is D-1]).
initiated(happens(turn_right, A, _), lookingtowards(B, C), [holds(at_pos(D, C), A), holds(lookingtowards(D, E), A), B is D-1, E is C-1]).
initiated(happens(turn_right, A, _), lookingtowards(B, C), [holds(at_pos(B, D), A), holds(lookingtowards(E, D), A), E is B-1, C is D+1]).
initiated(happens(turn_right, A, _), lookingtowards(B, C), [holds(at_pos(D, C), A), holds(lookingtowards(D, E), A), B is D+1, E is C+1]).

d_pre([holds(at_pos(A, B), C), happens(step(A, B), C, _)]).

fluents([at_pos(_, _), free(_, _), visited(_, _), obstacle(_, _), life(_, _), lookingtowards(_, _)]).

terminated(happens(step(A, B), _, _), lookingtowards(A, B), []).
terminated(happens(step(_, _), A, _), at_pos(B, C), [holds(at_pos(B, C), A)]).
terminated(happens(step(A, B), _, _), free(A, B), []).
terminated(happens(turn_right, _, _), lookingtowards(_, _), []).

reactive_rule([holds(lookingtowards(A, B), C), holds(free(A, B), C), holds(not(visited(A, B)), C)], [happens(step(A, B), C, _)]).
reactive_rule([holds(lookingtowards(A, B), C), holds(free(A, B), C), holds(visited(A, B), C)], [happens(turn_right, C, _)]).
reactive_rule([holds(lookingtowards(A, B), C), holds(obstacle(A, B), C)], [happens(turn_right, _, _)]).
reactive_rule([holds(lookingtowards(A, B), C), holds(obstacle(A, B), C), holds(life(A, B), C)], [happens(report, _, _)]).

initial_state([at_pos(0, 0), life(2, 1), free(1, 0), free(2, 0), obstacle(3, 0), obstacle(2, -1), obstacle(2, 1), lookingtowards(1, 0)]).

:- dynamic actions/1.
:- multifile actions/1.

actions([step(_, _), turn_right, report]).

maxTime(15).
% dB(/.../(lps_user_examples, 'martianrobot.pl'), lps_visualization(_106876{groups:[_102234{content:"at_pos(A,B)", id:"at_pos/2", order:3, subgroupStack:"false"}, _102312{content:"free(A,B)", id:"free/2", order:3, subgroupStack:"false"}, _102390{content:"life(A,B)", id:"life/2", order:3, subgroupStack:"false"}, _102468{content:"lookingtowards(A,B)", id:"lookingtowards/2", order:3, subgroupStack:"false"}, _102546{content:"obstacle(A,B)", id:"obstacle/2", order:3, subgroupStack:"false"}, _102624{content:"visited(A,B)", id:"visited/2", order:3, subgroupStack:"false"}, _102690{content:"Actions", id:"action", order:4}], items:[_102812{content:"0,0", end:2, group:"at_pos/2", id:0, start:1, subgroup:"0", title:"Fluent at_pos(0,0) initiated at 1<br/>and terminated at transition to 2"}, _102938{content:"1,0", end:3, group:"at_pos/2", id:1, start:2, subgroup:"1", title:"Fluent at_pos(1,0) initiated at 2<br/>and terminated at transition to 3"}, _103064{content:"2,0", end:16, group:"at_pos/2", id:2, start:3, subgroup:"2", title:"Fluent at_pos(2,0) initiated at 3<br/>and terminated at transition to 16"}, _103190{content:"0,0", end:16, group:"free/2", id:3, start:2, subgroup:"0", title:"Fluent free(0,0) initiated at 2<br/>and terminated at transition to 16"}, _103316{content:"1,0", end:2, group:"free/2", id:4, start:1, subgroup:"1", title:"Fluent free(1,0) initiated at 1<br/>and terminated at transition to 2"}, _103442{content:"1,0", end:16, group:"free/2", id:5, start:3, subgroup:"1", title:"Fluent free(1,0) initiated at 3<br/>and terminated at transition to 16"}, _103568{content:"2,0", end:3, group:"free/2", id:6, start:1, subgroup:"2", title:"Fluent free(2,0) initiated at 1<br/>and terminated at transition to 3"}, _103694{content:"2,1", end:16, group:"life/2", id:7, start:1, subgroup:"2", title:"Fluent life(2,1) initiated at 1<br/>and terminated at transition to 16"}, _103820{content:"1,0", end:2, group:"lookingtowards/2", id:8, start:1, subgroup:"1", title:"Fluent lookingtowards(1,0) initiated at 1<br/>and terminated at transition to 2"}, _103946{content:"1,0", end:6, group:"lookingtowards/2", id:9, start:5, subgroup:"1", title:"Fluent lookingtowards(1,0) initiated at 5<br/>and terminated at transition to 6"}, _104072{content:"2,-1", end:5, group:"lookingtowards/2", id:10, start:4, subgroup:"2", title:"Fluent lookingtowards(2,-1) initiated at 4<br/>and terminated at transition to 5"}, _104198{content:"2,0", end:3, group:"lookingtowards/2", id:11, start:2, subgroup:"2", title:"Fluent lookingtowards(2,0) initiated at 2<br/>and terminated at transition to 3"}, _104324{content:"2,1", end:16, group:"lookingtowards/2", id:12, start:6, subgroup:"2", title:"Fluent lookingtowards(2,1) initiated at 6<br/>and terminated at transition to 16"}, _104450{content:"3,0", end:4, group:"lookingtowards/2", id:13, start:3, subgroup:"3", title:"Fluent lookingtowards(3,0) initiated at 3<br/>and terminated at transition to 4"}, _104576{content:"2,-1", end:16, group:"obstacle/2", id:14, start:1, subgroup:"2", title:"Fluent obstacle(2,-1) initiated at 1<br/>and terminated at transition to 16"}, _104702{content:"2,1", end:16, group:"obstacle/2", id:15, start:1, subgroup:"2", title:"Fluent obstacle(2,1) initiated at 1<br/>and terminated at transition to 16"}, _104828{content:"3,0", end:16, group:"obstacle/2", id:16, start:1, subgroup:"3", title:"Fluent obstacle(3,0) initiated at 1<br/>and terminated at transition to 16"}, _104954{content:"0,0", end:16, group:"visited/2", id:17, start:2, subgroup:"0", title:"Fluent visited(0,0) initiated at 2<br/>and terminated at transition to 16"}, _105080{content:"1,0", end:16, group:"visited/2", id:18, start:3, subgroup:"1", title:"Fluent visited(1,0) initiated at 3<br/>and terminated at transition to 16"}, _105206{content:"step(1,0)", group:"action", id:19, start:2, style:"color:green", title:"happens(step(1,0),1,2)", type:"point"}, _105332{content:"step(2,0)", group:"action", id:20, start:3, style:"color:green", title:"happens(step(2,0),2,3)", type:"point"}, _105458{content:"turn_right", group:"action", id:21, start:4, style:"color:green", title:"happens(turn_right,3,4)", type:"point"}, _105584{content:"turn_right", group:"action", id:22, start:5, style:"color:green", title:"happens(turn_right,4,5)", type:"point"}, _105710{content:"turn_right", group:"action", id:23, start:6, style:"color:green", title:"happens(turn_right,5,6)", type:"point"}, _105836{content:"report", group:"action", id:24, start:7, style:"color:green", title:"happens(report,6,7)", type:"point"}, _105962{content:"report", group:"action", id:25, start:8, style:"color:green", title:"happens(report,7,8)", type:"point"}, _106088{content:"report", group:"action", id:26, start:9, style:"color:green", title:"happens(report,8,9)", type:"point"}, _106214{content:"report", group:"action", id:27, start:10, style:"color:green", title:"happens(report,9,10)", type:"point"}, _106340{content:"report", group:"action", id:28, start:11, style:"color:green", title:"happens(report,10,11)", type:"point"}, _106466{content:"report", group:"action", id:29, start:12, style:"color:green", title:"happens(report,11,12)", type:"point"}, _106592{content:"report", group:"action", id:30, start:13, style:"color:green", title:"happens(report,12,13)", type:"point"}, _106718{content:"report", group:"action", id:31, start:14, style:"color:green", title:"happens(report,13,14)", type:"point"}, _106844{content:"report", group:"action", id:32, start:15, style:"color:green", title:"happens(report,14,15)", type:"point"}]}, [])).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'martianrobot,pl.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'martianrobot,pl.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/martianrobot,pl.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'martianrobot,pl.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'martianrobot,pl.pl'), lps= /.../(lps_user_examples, 'martianrobot,pl.pl'), using= /.../(lps_user_examples, 'martianrobot,pl.pl')].
% continue_lps_dialect.
% ops.

% LPS:  fluents((en/2,libre/2,estuve/2,obstaculo/2,vida/2,mirada/2)).
% Into: fluents([en(_63776,_63778),libre(_63788,_63790),estuve(_63800,_63802),obstaculo(_63812,_63814),vida(_63824,_63826),mirada(_63836,_63838)]).

% LPS:  actions((ir(_63576,_63578),derecha,izquierda,reporte)).
% Into: actions([ir(_63576,_63578),derecha,izquierda,reporte]).

% LPS:  initially((en(0,0),vida(2,1),libre(1,0),libre(2,0),obstaculo(3,0),obstaculo(2,-1),obstaculo(2,1),mirada(1,0))).
% Into: initial_state([en(0,0),vida(2,1),libre(1,0),libre(2,0),obstaculo(3,0),obstaculo(2,-1),obstaculo(2,1),mirada(1,0)]).

% LPS:  then(if((mirada(_66658,_66660),libre(_66658,_66660),not(estuve(_66658,_66660)))),ir(_66658,_66660)).
% Into: reactive_rule([holds(mirada(_66658,_66660),_68028),holds(libre(_66658,_66660),_68028),holds(not(estuve(_66658,_66660)),_68028)],[happens(ir(_66658,_66660),_68876,_68882)]).

% LPS:  then(if((mirada(_69002,_69004),libre(_69002,_69004),estuve(_69002,_69004))),derecha).
% Into: reactive_rule([holds(mirada(_69002,_69004),_70298),holds(libre(_69002,_69004),_70298),holds(estuve(_69002,_69004),_70298)],[happens(derecha,_71096,_71102)]).

% LPS:  then(if((mirada(_71270,_71272),obstaculo(_71270,_71272),not(vida(_71270,_71272)))),derecha).
% Into: reactive_rule([holds(mirada(_71270,_71272),_72594),holds(obstaculo(_71270,_71272),_72594),holds(not(vida(_71270,_71272)),_72594)],[happens(derecha,_73442,_73448)]).

% LPS:  then(if((mirada(_73602,_73604),obstaculo(_73602,_73604),vida(_73602,_73604))),(reporte,derecha)).
% Into: reactive_rule([holds(mirada(_73602,_73604),_74952),holds(obstaculo(_73602,_73604),_74952),holds(vida(_73602,_73604),_74952)],[happens(reporte,_75750,_75792),happens(derecha,_75792,_75756)]).
% /pack/logicmoo_ec/test/lps_user_examples/martianrobot,pl.pl:50
% pop_lps_dialect('$BLOB'("<stream>(0x562ef2ab8f00)"),  (/.../(lps_user_examples, 'martianrobot,pl.pl')-> /.../(lps_user_examples, 'martianrobot,pl.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/martianrobot,pl.pl':_83642).


fluents([en(_, _), libre(_, _), estuve(_, _), obstaculo(_, _), vida(_, _), mirada(_, _)]).

reactive_rule([holds(mirada(A, B), C), holds(libre(A, B), C), holds(not(estuve(A, B)), C)], [happens(ir(A, B), _, _)]).
reactive_rule([holds(mirada(A, B), C), holds(libre(A, B), C), holds(estuve(A, B), C)], [happens(derecha, _, _)]).
reactive_rule([holds(mirada(A, B), C), holds(obstaculo(A, B), C), holds(not(vida(A, B)), C)], [happens(derecha, _, _)]).
reactive_rule([holds(mirada(A, B), C), holds(obstaculo(A, B), C), holds(vida(A, B), C)], [happens(reporte, _, D), happens(derecha, D, _)]).

initial_state([en(0, 0), vida(2, 1), libre(1, 0), libre(2, 0), obstaculo(3, 0), obstaculo(2, -1), obstaculo(2, 1), mirada(1, 0)]).

:- dynamic actions/1.
:- multifile actions/1.

actions([ir(_, _), derecha, izquierda, reporte]).

maxTime(10).
% dB(/.../(lps_user_examples, 'martianrobot,pl.pl'), lps_visualization(_68586{groups:[_66038{content:"en(A,B)", id:"en/2", order:3, subgroupStack:"false"}, _66116{content:"libre(A,B)", id:"libre/2", order:3, subgroupStack:"false"}, _66194{content:"mirada(A,B)", id:"mirada/2", order:3, subgroupStack:"false"}, _66272{content:"obstaculo(A,B)", id:"obstaculo/2", order:3, subgroupStack:"false"}, _66350{content:"vida(A,B)", id:"vida/2", order:3, subgroupStack:"false"}, _66416{content:"Actions", id:"action", order:4}], items:[_66538{content:"0,0", end:11, group:"en/2", id:0, start:1, subgroup:"0", title:"Fluent en(0,0) initiated at 1<br/>and terminated at transition to 11"}, _66664{content:"1,0", end:11, group:"libre/2", id:1, start:1, subgroup:"1", title:"Fluent libre(1,0) initiated at 1<br/>and terminated at transition to 11"}, _66790{content:"2,0", end:11, group:"libre/2", id:2, start:1, subgroup:"2", title:"Fluent libre(2,0) initiated at 1<br/>and terminated at transition to 11"}, _66916{content:"1,0", end:11, group:"mirada/2", id:3, start:1, subgroup:"1", title:"Fluent mirada(1,0) initiated at 1<br/>and terminated at transition to 11"}, _67042{content:"2,-1", end:11, group:"obstaculo/2", id:4, start:1, subgroup:"2", title:"Fluent obstaculo(2,-1) initiated at 1<br/>and terminated at transition to 11"}, _67168{content:"2,1", end:11, group:"obstaculo/2", id:5, start:1, subgroup:"2", title:"Fluent obstaculo(2,1) initiated at 1<br/>and terminated at transition to 11"}, _67294{content:"3,0", end:11, group:"obstaculo/2", id:6, start:1, subgroup:"3", title:"Fluent obstaculo(3,0) initiated at 1<br/>and terminated at transition to 11"}, _67420{content:"2,1", end:11, group:"vida/2", id:7, start:1, subgroup:"2", title:"Fluent vida(2,1) initiated at 1<br/>and terminated at transition to 11"}, _67546{content:"ir(1,0)", group:"action", id:8, start:2, style:"color:green", title:"happens(ir(1,0),1,2)", type:"point"}, _67672{content:"ir(1,0)", group:"action", id:9, start:3, style:"color:green", title:"happens(ir(1,0),2,3)", type:"point"}, _67798{content:"ir(1,0)", group:"action", id:10, start:4, style:"color:green", title:"happens(ir(1,0),3,4)", type:"point"}, _67924{content:"ir(1,0)", group:"action", id:11, start:5, style:"color:green", title:"happens(ir(1,0),4,5)", type:"point"}, _68050{content:"ir(1,0)", group:"action", id:12, start:6, style:"color:green", title:"happens(ir(1,0),5,6)", type:"point"}, _68176{content:"ir(1,0)", group:"action", id:13, start:7, style:"color:green", title:"happens(ir(1,0),6,7)", type:"point"}, _68302{content:"ir(1,0)", group:"action", id:14, start:8, style:"color:green", title:"happens(ir(1,0),7,8)", type:"point"}, _68428{content:"ir(1,0)", group:"action", id:15, start:9, style:"color:green", title:"happens(ir(1,0),8,9)", type:"point"}, _68554{content:"ir(1,0)", group:"action", id:16, start:10, style:"color:green", title:"happens(ir(1,0),9,10)", type:"point"}]}, [])).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'minimalGoat.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'minimalGoat.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/minimalGoat.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'minimalGoat.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'minimalGoat.pl'), lps= /.../(lps_user_examples, 'minimalGoat.pl'), using= /.../(lps_user_examples, 'minimalGoat.pl')].
% continue_lps_dialect.
% ops.

% LPS:  actions((row(_57034,_57036),transport(_57104,_57034,_57036))).
% Into: actions([row(_57034,_57036),transport(_57104,_57034,_57036)]).

% LPS:  fluents(loc(_58232,_58234)).
% Into: fluents([loc(_58232,_58234)]).

% LPS:  initially((loc(wolf,south),loc(goat,south),loc(cabbage,south),loc(farmer,south))).
% Into: initial_state([loc(wolf,south),loc(goat,south),loc(cabbage,south),loc(farmer,south)]).

% LPS:  updates(transport(_60626,_60628,_60630),in(to(_60628,_60630),loc(_60626,_60628))).
% Into: updated(happens(transport(_60626,_60628,_60630),_61982,_61988),loc(_60626,_60628),_60628-_60630,[]).

% LPS:  updates(row(_61882,_61884),in(to(_61882,_61884),loc(farmer,_61882))).
% Into: updated(happens(row(_61882,_61884),_63220,_63226),loc(farmer,_61882),_61882-_61884,[]).

% LPS:  false((at(loc(wolf,_63132),_63154),at(loc(goat,_63132),_63154),at(not(loc(farmer,_63132)),_63154),happens(_63488,_63490,_63154))).
% Into: d_pre([holds(loc(wolf,_63132),_63154),holds(loc(goat,_63132),_63154),holds(not(loc(farmer,_63132)),_63154),happens(_63488,_63490,_63154)]).

% LPS:  false((at(loc(cabbage,_65464),_65486),at(loc(goat,_65464),_65486),at(not(loc(farmer,_65464)),_65486),happens(_65820,_65822,_65486))).
% Into: d_pre([holds(loc(cabbage,_65464),_65486),holds(loc(goat,_65464),_65486),holds(not(loc(farmer,_65464)),_65486),happens(_65820,_65822,_65486)]).

% LPS:  false((row(_67794,_67796),row(_67796,_67794))).
% Into: d_pre([happens(row(_67794,_67796),_68920,_68926),happens(row(_67796,_67794),_68920,_68926)]).

% LPS:  false((transport(_68976,_68978,_68980),transport(_69048,_68978,_68980),_68976\=_69048)).
% Into: d_pre([happens(transport(_68976,_68978,_68980),_70256,_70262),happens(transport(_69048,_68978,_68980),_70256,_70262),_68976\=_69048]).

% LPS:  if(from(move,to(_74008,_74010)),(at(loc(_74122,_74124),_74008),_74122\=farmer,opposite(_74124,_74292),from(transport(_74122,_74124,_74292),to(_74008,_74010)),at(loc(farmer,_74124),_74008),from(row(_74124,_74292),to(_74008,_74010)))).
% Into: l_events(happens(move,_74008,_74010),[holds(loc(_74122,_74124),_74008),_74122\=farmer,opposite(_74124,_74292),happens(transport(_74122,_74124,_74292),_74008,_74010),holds(loc(farmer,_74124),_74008),happens(row(_74124,_74292),_74008,_74010)]).

% LPS:  if(move,(at(loc(farmer,_77494),_77516),opposite(_77494,_77598),from(row(_77494,_77598),_77516))).
% Into: l_events(happens(move,_77516,_78856),[holds(loc(farmer,_77494),_77516),opposite(_77494,_77598),happens(row(_77494,_77598),_77516,_78856)]).

% LPS:  from(moves,to(_79260,_79260)).
% Into: l_events(happens(moves,_79260,_79260),[]).

% LPS:  if(moves,(to(move,_80344),from(moves,_80344))).
% Into: l_events(happens(moves,_81502,_81508),[happens(move,_81502,_80344),happens(moves,_80344,_81508)]).

% LPS:  then(if(at(not(loc(_81550,north)),_81598)),(from(moves,to(_81598,_81702)),at(not(loc(_81822,south)),_81702))).
% Into: reactive_rule([holds(not(loc(_81550,north)),_81598)],[happens(moves,_81598,_81702),holds(not(loc(_81822,south)),_81702)]).
% /pack/logicmoo_ec/test/lps_user_examples/minimalGoat.pl:40
% pop_lps_dialect('$BLOB'("<stream>(0x562ef73e2900)"),  (/.../(lps_user_examples, 'minimalGoat.pl')-> /.../(lps_user_examples, 'minimalGoat.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/minimalGoat.pl':_90668).


d_pre([holds(loc(wolf, A), B), holds(loc(goat, A), B), holds(not(loc(farmer, A)), B), happens(_, _, B)]).
d_pre([holds(loc(cabbage, A), B), holds(loc(goat, A), B), holds(not(loc(farmer, A)), B), happens(_, _, B)]).
d_pre([happens(row(A, B), C, D), happens(row(B, A), C, D)]).
d_pre([happens(transport(A, B, C), D, E), happens(transport(F, B, C), D, E), A\=F]).

opposite(north, south).
opposite(south, north).

fluents([loc(_, _)]).

reactive_rule([holds(not(loc(_, north)), A)], [happens(moves, A, B), holds(not(loc(_, south)), B)]).

initial_state([loc(wolf, south), loc(goat, south), loc(cabbage, south), loc(farmer, south)]).

l_events(happens(move, A, B), [holds(loc(C, D), A), C\=farmer, opposite(D, E), happens(transport(C, D, E), A, B), holds(loc(farmer, D), A), happens(row(D, E), A, B)]).
l_events(happens(move, A, B), [holds(loc(farmer, C), A), opposite(C, D), happens(row(C, D), A, B)]).
l_events(happens(moves, A, A), []).
l_events(happens(moves, A, B), [happens(move, A, C), happens(moves, C, B)]).

:- dynamic actions/1.
:- multifile actions/1.

actions([row(A, B), transport(_, A, B)]).

updated(happens(transport(A, B, C), _, _), loc(A, B), B-C, []).
updated(happens(row(A, B), _, _), loc(farmer, A), A-B, []).
ERROR: LPS: execution timeout(resolveAndUpdate)
PROGRAM FAILED
% dB(/.../(lps_user_examples, 'minimalGoat.pl'), lps_visualization(_100714{groups:[_97092{content:"loc(A,B)", id:"loc/2", order:3, subgroupStack:"false"}, _97158{content:"Actions", id:"action", order:4}], items:[_97280{content:"cabbage,south", end:8, group:"loc/2", id:0, start:1, subgroup:"cabbage", title:"Fluent loc(cabbage,south) initiated at 1<br/>and terminated at transition to 8"}, _97406{content:"farmer,north", end:3, group:"loc/2", id:1, start:2, subgroup:"farmer", title:"Fluent loc(farmer,north) initiated at 2<br/>and terminated at transition to 3"}, _97532{content:"farmer,north", end:5, group:"loc/2", id:2, start:4, subgroup:"farmer", title:"Fluent loc(farmer,north) initiated at 4<br/>and terminated at transition to 5"}, _97658{content:"farmer,north", end:7, group:"loc/2", id:3, start:6, subgroup:"farmer", title:"Fluent loc(farmer,north) initiated at 6<br/>and terminated at transition to 7"}, _97784{content:"farmer,south", end:2, group:"loc/2", id:4, start:1, subgroup:"farmer", title:"Fluent loc(farmer,south) initiated at 1<br/>and terminated at transition to 2"}, _97910{content:"farmer,south", end:4, group:"loc/2", id:5, start:3, subgroup:"farmer", title:"Fluent loc(farmer,south) initiated at 3<br/>and terminated at transition to 4"}, _98036{content:"farmer,south", end:6, group:"loc/2", id:6, start:5, subgroup:"farmer", title:"Fluent loc(farmer,south) initiated at 5<br/>and terminated at transition to 6"}, _98162{content:"farmer,south", end:8, group:"loc/2", id:7, start:7, subgroup:"farmer", title:"Fluent loc(farmer,south) initiated at 7<br/>and terminated at transition to 8"}, _98288{content:"goat,north", end:3, group:"loc/2", id:8, start:2, subgroup:"goat", title:"Fluent loc(goat,north) initiated at 2<br/>and terminated at transition to 3"}, _98414{content:"goat,north", end:5, group:"loc/2", id:9, start:4, subgroup:"goat", title:"Fluent loc(goat,north) initiated at 4<br/>and terminated at transition to 5"}, _98540{content:"goat,north", end:7, group:"loc/2", id:10, start:6, subgroup:"goat", title:"Fluent loc(goat,north) initiated at 6<br/>and terminated at transition to 7"}, _98666{content:"goat,south", end:2, group:"loc/2", id:11, start:1, subgroup:"goat", title:"Fluent loc(goat,south) initiated at 1<br/>and terminated at transition to 2"}, _98792{content:"goat,south", end:4, group:"loc/2", id:12, start:3, subgroup:"goat", title:"Fluent loc(goat,south) initiated at 3<br/>and terminated at transition to 4"}, _98918{content:"goat,south", end:6, group:"loc/2", id:13, start:5, subgroup:"goat", title:"Fluent loc(goat,south) initiated at 5<br/>and terminated at transition to 6"}, _99044{content:"goat,south", end:8, group:"loc/2", id:14, start:7, subgroup:"goat", title:"Fluent loc(goat,south) initiated at 7<br/>and terminated at transition to 8"}, _99170{content:"wolf,south", end:8, group:"loc/2", id:15, start:1, subgroup:"wolf", title:"Fluent loc(wolf,south) initiated at 1<br/>and terminated at transition to 8"}, _99296{content:"transport(goat,south,north)", group:"action", id:16, start:2, style:"color:green", title:"happens(transport(goat,south,north),1,2)", type:"point"}, _99422{content:"row(south,north)", group:"action", id:17, start:2, style:"color:green", title:"happens(row(south,north),1,2)", type:"point"}, _99548{content:"transport(goat,north,south)", group:"action", id:18, start:3, style:"color:green", title:"happens(transport(goat,north,south),2,3)", type:"point"}, _99674{content:"row(north,south)", group:"action", id:19, start:3, style:"color:green", title:"happens(row(north,south),2,3)", type:"point"}, _99800{content:"transport(goat,south,north)", group:"action", id:20, start:4, style:"color:green", title:"happens(transport(goat,south,north),3,4)", type:"point"}, _99926{content:"row(south,north)", group:"action", id:21, start:4, style:"color:green", title:"happens(row(south,north),3,4)", type:"point"}, _100052{content:"transport(goat,north,south)", group:"action", id:22, start:5, style:"color:green", title:"happens(transport(goat,north,south),4,5)", type:"point"}, _100178{content:"row(north,south)", group:"action", id:23, start:5, style:"color:green", title:"happens(row(north,south),4,5)", type:"point"}, _100304{content:"transport(goat,south,north)", group:"action", id:24, start:6, style:"color:green", title:"happens(transport(goat,south,north),5,6)", type:"point"}, _100430{content:"row(south,north)", group:"action", id:25, start:6, style:"color:green", title:"happens(row(south,north),5,6)", type:"point"}, _100556{content:"transport(goat,north,south)", group:"action", id:26, start:7, style:"color:green", title:"happens(transport(goat,north,south),6,7)", type:"point"}, _100682{content:"row(north,south)", group:"action", id:27, start:7, style:"color:green", title:"happens(row(north,south),6,7)", type:"point"}]}, [])).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'MyLife.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'MyLife.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/MyLife.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'MyLife.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'MyLife.pl'), lps= /.../(lps_user_examples, 'MyLife.pl'), using= /.../(lps_user_examples, 'MyLife.pl')].
% continue_lps_dialect.
% ops.

% LPS:  fluents(_19484-_19486).
% Into: fluents([_19484-_19486]).

% LPS:  actions((die(_20564),live(_20604))).
% Into: actions([die(_20564),live(_20604)]).

% LPS:  initially((2-3,3-3,4-3)).
% Into: initial_state([2-3,3-3,4-3]).

% LPS:  then(if((at(_23028-_23030,_23078),at(aliveNeighbors(_23028-_23030,_23208),_23078),_23208<2)),die(_23028-_23030)).
% Into: reactive_rule([holds(_23028-_23030,_23078),holds(aliveNeighbors(_23028-_23030,_23208),_23078),_23208<2],[happens(die(_23028-_23030),_26328,_26334)]).

% LPS:  then(if((at(_26380-_26382,_26430),at(aliveNeighbors(_26380-_26382,_26560),_26430),_26560>3)),die(_26380-_26382)).
% Into: reactive_rule([holds(_26380-_26382,_26430),holds(aliveNeighbors(_26380-_26382,_26560),_26430),_26560>3],[happens(die(_26380-_26382),_29680,_29686)]).

% LPS:  then(if((cell(_29750,_29752),at(aliveNeighbors(_29750-_29752,3),_29878))),live(_29750-_29752)).
% Into: reactive_rule([cell(_29750,_29752),holds(aliveNeighbors(_29750-_29752,3),_29878)],[happens(live(_29750-_29752),_32374,_32380)]).

% LPS:  if(at(aliveNeighbors(_37562-_37564,_37610),_37632),(adjacent(_37562-_37564,_37762),at(countLivingNeighbors(_37762,_37610),_37632))).
% Into: l_int(holds(aliveNeighbors(_37562-_37564,_37610),_37632),[adjacent(_37562-_37564,_37762),holds(countLivingNeighbors(_37762,_37610),_37632)]).

% LPS:  at(countLivingNeighbors([],0),_39840).
% Into: l_int(holds(countLivingNeighbors([],0),_39840),[]).

% LPS:  if(at(countLivingNeighbors([_41086-_41088|_41120],_41148),_41170),(at(_41086-_41088,_41170),at(countLivingNeighbors(_41120,_41364),_41170),_41148 is _41364+1)).
% Into: l_int(holds(countLivingNeighbors([_41086-_41088|_41120],_41148),_41170),[holds(_41086-_41088,_41170),holds(countLivingNeighbors(_41120,_41364),_41170),_41148 is _41364+1]).

% LPS:  if(at(countLivingNeighbors([_43738-_43740|_43772],_43800),_43822),(at(not(_43738-_43740),_43822),at(countLivingNeighbors(_43772,_43800),_43822))).
% Into: l_int(holds(countLivingNeighbors([_43738-_43740|_43772],_43800),_43822),[holds(not(_43738-_43740),_43822),holds(countLivingNeighbors(_43772,_43800),_43822)]).

% LPS:  initiates(live(_45982-_45984),_45982-_45984).
% Into: initiated(happens(live(_45982-_45984),_47208,_47214),_45982-_45984,[]).

% LPS:  terminates(die(_47158-_47160),_47158-_47160).
% Into: terminated(happens(die(_47158-_47160),_48384,_48390),_47158-_47160,[]).
% /pack/logicmoo_ec/test/lps_user_examples/MyLife.pl:64
% pop_lps_dialect('$BLOB'("<stream>(0x562ef70bf100)"),  (/.../(lps_user_examples, 'MyLife.pl')-> /.../(lps_user_examples, 'MyLife.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/MyLife.pl':_64964).


initiated(happens(live(A-B), _, _), A-B, []).

d(X-Y, [center:[XX, YY], radius:5, type:circle, fillColor:green]) :-
    XX is X*10,
    YY is Y*10.
d(live(X-Y), [type:star, center:[XX, YY], points:7, radius1:4, radius2:7, fillColor:red]) :-
    XX is X*10,
    YY is Y*10.
d(die(X-Y), [type:star, center:[XX, YY], points:7, radius1:4, radius2:7, fillColor:black]) :-
    XX is X*10,
    YY is Y*10.
d(timeless, [[type:raster, position:[50, 120], scale:0.08, source:'https://upload.wikimedia.org/wikipedia/commons/0/04/John_H_Conway_2005_%28cropped%29.jpg'], [type:text, point:[0, 5], content:'Conway\'s Game of Life']]).

fluents([_-_]).

l_int(holds(aliveNeighbors(A-B, C), D), [adjacent(A-B, E), holds(countLivingNeighbors(E, C), D)]).
l_int(holds(countLivingNeighbors([], 0), _), []).
l_int(holds(countLivingNeighbors([A-B|C], D), E), [holds(A-B, E), holds(countLivingNeighbors(C, F), E), D is F+1]).
l_int(holds(countLivingNeighbors([A-B|C], D), E), [holds(not(A-B), E), holds(countLivingNeighbors(C, D), E)]).

reactive_rule([holds(A-B, C), holds(aliveNeighbors(A-B, D), C), D<2], [happens(die(A-B), _, _)]).
reactive_rule([holds(A-B, C), holds(aliveNeighbors(A-B, D), C), D>3], [happens(die(A-B), _, _)]).
reactive_rule([cell(A, B), holds(aliveNeighbors(A-B, 3), _)], [happens(live(A-B), _, _)]).

cell(X, Y) :-
    Range=[1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
    member(X, Range),
    member(Y, Range).

initial_state([2-3, 3-3, 4-3]).

terminated(happens(die(A-B), _, _), A-B, []).

:- dynamic actions/1.
:- multifile actions/1.

actions([die(_), live(_)]).

adjacent(A-B, C) :-
    findall(D-E,
            ( member(F, [-1, 0, 1]),
              member(G, [-1, 0, 1]),
              not(( F=0,
                    G=0
                  )),
              D is A+F,
              E is B+G
            ),
            C).

maxTime(8).
% dB(/.../(lps_user_examples, 'MyLife.pl'), lps_visualization(_324750{groups:[_318986{content:"A-B", id:"(-)/2", order:3, subgroupStack:"false"}, _319052{content:"Actions", id:"action", order:4}], items:[_319174{content:"2,3", end:2, group:"(-)/2", id:0, start:1, subgroup:"2", title:"Fluent 2-3 initiated at 1<br/>and terminated at transition to 2"}, _319300{content:"2,3", end:4, group:"(-)/2", id:1, start:3, subgroup:"2", title:"Fluent 2-3 initiated at 3<br/>and terminated at transition to 4"}, _319426{content:"2,3", end:6, group:"(-)/2", id:2, start:5, subgroup:"2", title:"Fluent 2-3 initiated at 5<br/>and terminated at transition to 6"}, _319552{content:"2,3", end:8, group:"(-)/2", id:3, start:7, subgroup:"2", title:"Fluent 2-3 initiated at 7<br/>and terminated at transition to 8"}, _319678{content:"3,2", end:3, group:"(-)/2", id:4, start:2, subgroup:"3", title:"Fluent 3-2 initiated at 2<br/>and terminated at transition to 3"}, _319804{content:"3,2", end:5, group:"(-)/2", id:5, start:4, subgroup:"3", title:"Fluent 3-2 initiated at 4<br/>and terminated at transition to 5"}, _319930{content:"3,2", end:7, group:"(-)/2", id:6, start:6, subgroup:"3", title:"Fluent 3-2 initiated at 6<br/>and terminated at transition to 7"}, _320056{content:"3,2", end:9, group:"(-)/2", id:7, start:8, subgroup:"3", title:"Fluent 3-2 initiated at 8<br/>and terminated at transition to 9"}, _320182{content:"3,3", end:9, group:"(-)/2", id:8, start:1, subgroup:"3", title:"Fluent 3-3 initiated at 1<br/>and terminated at transition to 9"}, _320308{content:"3,4", end:3, group:"(-)/2", id:9, start:2, subgroup:"3", title:"Fluent 3-4 initiated at 2<br/>and terminated at transition to 3"}, _320434{content:"3,4", end:5, group:"(-)/2", id:10, start:4, subgroup:"3", title:"Fluent 3-4 initiated at 4<br/>and terminated at transition to 5"}, _320560{content:"3,4", end:7, group:"(-)/2", id:11, start:6, subgroup:"3", title:"Fluent 3-4 initiated at 6<br/>and terminated at transition to 7"}, _320686{content:"3,4", end:9, group:"(-)/2", id:12, start:8, subgroup:"3", title:"Fluent 3-4 initiated at 8<br/>and terminated at transition to 9"}, _320812{content:"4,3", end:2, group:"(-)/2", id:13, start:1, subgroup:"4", title:"Fluent 4-3 initiated at 1<br/>and terminated at transition to 2"}, _320938{content:"4,3", end:4, group:"(-)/2", id:14, start:3, subgroup:"4", title:"Fluent 4-3 initiated at 3<br/>and terminated at transition to 4"}, _321064{content:"4,3", end:6, group:"(-)/2", id:15, start:5, subgroup:"4", title:"Fluent 4-3 initiated at 5<br/>and terminated at transition to 6"}, _321190{content:"4,3", end:8, group:"(-)/2", id:16, start:7, subgroup:"4", title:"Fluent 4-3 initiated at 7<br/>and terminated at transition to 8"}, _321316{content:"live(3-4)", group:"action", id:17, start:2, style:"color:green", title:"happens(live(3-4),1,2)", type:"point"}, _321442{content:"live(3-2)", group:"action", id:18, start:2, style:"color:green", title:"happens(live(3-2),1,2)", type:"point"}, _321568{content:"die(4-3)", group:"action", id:19, start:2, style:"color:green", title:"happens(die(4-3),1,2)", type:"point"}, _321694{content:"die(2-3)", group:"action", id:20, start:2, style:"color:green", title:"happens(die(2-3),1,2)", type:"point"}, _321820{content:"die(3-2)", group:"action", id:21, start:3, style:"color:green", title:"happens(die(3-2),2,3)", type:"point"}, _321946{content:"die(3-4)", group:"action", id:22, start:3, style:"color:green", title:"happens(die(3-4),2,3)", type:"point"}, _322072{content:"live(2-3)", group:"action", id:23, start:3, style:"color:green", title:"happens(live(2-3),2,3)", type:"point"}, _322198{content:"live(4-3)", group:"action", id:24, start:3, style:"color:green", title:"happens(live(4-3),2,3)", type:"point"}, _322324{content:"live(3-4)", group:"action", id:25, start:4, style:"color:green", title:"happens(live(3-4),3,4)", type:"point"}, _322450{content:"live(3-2)", group:"action", id:26, start:4, style:"color:green", title:"happens(live(3-2),3,4)", type:"point"}, _322576{content:"die(4-3)", group:"action", id:27, start:4, style:"color:green", title:"happens(die(4-3),3,4)", type:"point"}, _322702{content:"die(2-3)", group:"action", id:28, start:4, style:"color:green", title:"happens(die(2-3),3,4)", type:"point"}, _322828{content:"die(3-2)", group:"action", id:29, start:5, style:"color:green", title:"happens(die(3-2),4,5)", type:"point"}, _322954{content:"die(3-4)", group:"action", id:30, start:5, style:"color:green", title:"happens(die(3-4),4,5)", type:"point"}, _323080{content:"live(2-3)", group:"action", id:31, start:5, style:"color:green", title:"happens(live(2-3),4,5)", type:"point"}, _323206{content:"live(4-3)", group:"action", id:32, start:5, style:"color:green", title:"happens(live(4-3),4,5)", type:"point"}, _323332{content:"live(3-4)", group:"action", id:33, start:6, style:"color:green", title:"happens(live(3-4),5,6)", type:"point"}, _323458{content:"live(3-2)", group:"action", id:34, start:6, style:"color:green", title:"happens(live(3-2),5,6)", type:"point"}, _323584{content:"die(4-3)", group:"action", id:35, start:6, style:"color:green", title:"happens(die(4-3),5,6)", type:"point"}, _323710{content:"die(2-3)", group:"action", id:36, start:6, style:"color:green", title:"happens(die(2-3),5,6)", type:"point"}, _323836{content:"die(3-2)", group:"action", id:37, start:7, style:"color:green", title:"happens(die(3-2),6,7)", type:"point"}, _323962{content:"die(3-4)", group:"action", id:38, start:7, style:"color:green", title:"happens(die(3-4),6,7)", type:"point"}, _324088{content:"live(2-3)", group:"action", id:39, start:7, style:"color:green", title:"happens(live(2-3),6,7)", type:"point"}, _324214{content:"live(4-3)", group:"action", id:40, start:7, style:"color:green", title:"happens(live(4-3),6,7)", type:"point"}, _324340{content:"live(3-4)", group:"action", id:41, start:8, style:"color:green", title:"happens(live(3-4),7,8)", type:"point"}, _324466{content:"live(3-2)", group:"action", id:42, start:8, style:"color:green", title:"happens(live(3-2),7,8)", type:"point"}, _324592{content:"die(4-3)", group:"action", id:43, start:8, style:"color:green", title:"happens(die(4-3),7,8)", type:"point"}, _324718{content:"die(2-3)", group:"action", id:44, start:8, style:"color:green", title:"happens(die(2-3),7,8)", type:"point"}]}, _396436{cycles:[[_387800{create:[_387686{id:"timeless", position:[50, 120], scale:0.08, source:"https://upload.wikimedia.org/wikipedia/commons/0/04/John_H_Conway_2005_%28cropped%29.jpg", type:"raster"}, _387780{content:"Conway's Game of Life", id:"timeless", point:[0, 5], type:"text"}]}], [_387936{create:_387912{center:[20, 30], fillColor:"green", id:"2-3", radius:5, type:"circle"}}, _388066{create:_388042{center:[30, 30], fillColor:"green", id:"3-3", radius:5, type:"circle"}}, _388196{create:_388172{center:[40, 30], fillColor:"green", id:"4-3", radius:5, type:"circle"}}, _388374{create:_388338{center:[20, 30], event:"true", fillColor:"black", id:"die(2-3)", points:7, radius1:4, radius2:7, type:"star"}}, _388552{create:_388516{center:[40, 30], event:"true", fillColor:"black", id:"die(4-3)", points:7, radius1:4, radius2:7, type:"star"}}, _388730{create:_388694{center:[30, 20], event:"true", fillColor:"red", id:"live(3-2)", points:7, radius1:4, radius2:7, type:"star"}}, _388908{create:_388872{center:[30, 40], event:"true", fillColor:"red", id:"live(3-4)", points:7, radius1:4, radius2:7, type:"star"}}], [_389044{create:_389020{center:[30, 20], fillColor:"green", id:"3-2", radius:5, type:"circle"}}, _389174{create:_389150{center:[30, 40], fillColor:"green", id:"3-4", radius:5, type:"circle"}}, _389352{create:_389316{center:[30, 20], event:"true", fillColor:"black", id:"die(3-2)", points:7, radius1:4, radius2:7, type:"star"}}, _389530{create:_389494{center:[30, 40], event:"true", fillColor:"black", id:"die(3-4)", points:7, radius1:4, radius2:7, type:"star"}}, _389708{create:_389672{center:[20, 30], event:"true", fillColor:"red", id:"live(2-3)", points:7, radius1:4, radius2:7, type:"star"}}, _389886{create:_389850{center:[40, 30], event:"true", fillColor:"red", id:"live(4-3)", points:7, radius1:4, radius2:7, type:"star"}}, _389916{kill:"2-3"}, _389946{kill:"4-3"}, _389976{kill:"die(2-3)"}, _390006{kill:"die(4-3)"}, _390036{kill:"live(3-2)"}, _390066{kill:"live(3-4)"}], [_390202{create:_390178{center:[20, 30], fillColor:"green", id:"2-3", radius:5, type:"circle"}}, _390332{create:_390308{center:[40, 30], fillColor:"green", id:"4-3", radius:5, type:"circle"}}, _390510{create:_390474{center:[20, 30], event:"true", fillColor:"black", id:"die(2-3)", points:7, radius1:4, radius2:7, type:"star"}}, _390688{create:_390652{center:[40, 30], event:"true", fillColor:"black", id:"die(4-3)", points:7, radius1:4, radius2:7, type:"star"}}, _390866{create:_390830{center:[30, 20], event:"true", fillColor:"red", id:"live(3-2)", points:7, radius1:4, radius2:7, type:"star"}}, _391044{create:_391008{center:[30, 40], event:"true", fillColor:"red", id:"live(3-4)", points:7, radius1:4, radius2:7, type:"star"}}, _391074{kill:"3-2"}, _391104{kill:"3-4"}, _391134{kill:"die(3-2)"}, _391164{kill:"die(3-4)"}, _391194{kill:"live(2-3)"}, _391224{kill:"live(4-3)"}], [_391360{create:_391336{center:[30, 20], fillColor:"green", id:"3-2", radius:5, type:"circle"}}, _391490{create:_391466{center:[30, 40], fillColor:"green", id:"3-4", radius:5, type:"circle"}}, _391668{create:_391632{center:[30, 20], event:"true", fillColor:"black", id:"die(3-2)", points:7, radius1:4, radius2:7, type:"star"}}, _391846{create:_391810{center:[30, 40], event:"true", fillColor:"black", id:"die(3-4)", points:7, radius1:4, radius2:7, type:"star"}}, _392024{create:_391988{center:[20, 30], event:"true", fillColor:"red", id:"live(2-3)", points:7, radius1:4, radius2:7, type:"star"}}, _392202{create:_392166{center:[40, 30], event:"true", fillColor:"red", id:"live(4-3)", points:7, radius1:4, radius2:7, type:"star"}}, _392232{kill:"2-3"}, _392262{kill:"4-3"}, _392292{kill:"die(2-3)"}, _392322{kill:"die(4-3)"}, _392352{kill:"live(3-2)"}, _392382{kill:"live(3-4)"}], [_392518{create:_392494{center:[20, 30], fillColor:"green", id:"2-3", radius:5, type:"circle"}}, _392648{create:_392624{center:[40, 30], fillColor:"green", id:"4-3", radius:5, type:"circle"}}, _392826{create:_392790{center:[20, 30], event:"true", fillColor:"black", id:"die(2-3)", points:7, radius1:4, radius2:7, type:"star"}}, _393004{create:_392968{center:[40, 30], event:"true", fillColor:"black", id:"die(4-3)", points:7, radius1:4, radius2:7, type:"star"}}, _393182{create:_393146{center:[30, 20], event:"true", fillColor:"red", id:"live(3-2)", points:7, radius1:4, radius2:7, type:"star"}}, _393360{create:_393324{center:[30, 40], event:"true", fillColor:"red", id:"live(3-4)", points:7, radius1:4, radius2:7, type:"star"}}, _393390{kill:"3-2"}, _393420{kill:"3-4"}, _393450{kill:"die(3-2)"}, _393480{kill:"die(3-4)"}, _393510{kill:"live(2-3)"}, _393540{kill:"live(4-3)"}], [_393676{create:_393652{center:[30, 20], fillColor:"green", id:"3-2", radius:5, type:"circle"}}, _393806{create:_393782{center:[30, 40], fillColor:"green", id:"3-4", radius:5, type:"circle"}}, _393984{create:_393948{center:[30, 20], event:"true", fillColor:"black", id:"die(3-2)", points:7, radius1:4, radius2:7, type:"star"}}, _394162{create:_394126{center:[30, 40], event:"true", fillColor:"black", id:"die(3-4)", points:7, radius1:4, radius2:7, type:"star"}}, _394340{create:_394304{center:[20, 30], event:"true", fillColor:"red", id:"live(2-3)", points:7, radius1:4, radius2:7, type:"star"}}, _394518{create:_394482{center:[40, 30], event:"true", fillColor:"red", id:"live(4-3)", points:7, radius1:4, radius2:7, type:"star"}}, _394548{kill:"2-3"}, _394578{kill:"4-3"}, _394608{kill:"die(2-3)"}, _394638{kill:"die(4-3)"}, _394668{kill:"live(3-2)"}, _394698{kill:"live(3-4)"}], [_394834{create:_394810{center:[20, 30], fillColor:"green", id:"2-3", radius:5, type:"circle"}}, _394964{create:_394940{center:[40, 30], fillColor:"green", id:"4-3", radius:5, type:"circle"}}, _395142{create:_395106{center:[20, 30], event:"true", fillColor:"black", id:"die(2-3)", points:7, radius1:4, radius2:7, type:"star"}}, _395320{create:_395284{center:[40, 30], event:"true", fillColor:"black", id:"die(4-3)", points:7, radius1:4, radius2:7, type:"star"}}, _395498{create:_395462{center:[30, 20], event:"true", fillColor:"red", id:"live(3-2)", points:7, radius1:4, radius2:7, type:"star"}}, _395676{create:_395640{center:[30, 40], event:"true", fillColor:"red", id:"live(3-4)", points:7, radius1:4, radius2:7, type:"star"}}, _395706{kill:"3-2"}, _395736{kill:"3-4"}, _395766{kill:"die(3-2)"}, _395796{kill:"die(3-4)"}, _395826{kill:"live(2-3)"}, _395856{kill:"live(4-3)"}], [_395992{create:_395968{center:[30, 20], fillColor:"green", id:"3-2", radius:5, type:"circle"}}, _396122{create:_396098{center:[30, 40], fillColor:"green", id:"3-4", radius:5, type:"circle"}}, _396152{kill:"2-3"}, _396182{kill:"4-3"}, _396212{kill:"die(2-3)"}, _396242{kill:"die(4-3)"}, _396272{kill:"live(3-2)"}, _396302{kill:"live(3-4)"}], [_396338{kill:"3-2"}, _396368{kill:"3-3"}, _396398{kill:"3-4"}, _396428{kill:"timeless"}]]})).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'myLogGroker.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'myLogGroker.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/myLogGroker.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'myLogGroker.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'myLogGroker.pl'), lps= /.../(lps_user_examples, 'myLogGroker.pl'), using= /.../(lps_user_examples, 'myLogGroker.pl')].
% continue_lps_dialect.
% ops.
% /pack/logicmoo_ec/test/lps_user_examples/myLogGroker.pl:388
% pop_lps_dialect('$BLOB'("<stream>(0x562ef70c0000)"),  (/.../(lps_user_examples, 'myLogGroker.pl')-> /.../(lps_user_examples, 'myLogGroker.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/myLogGroker.pl':_38150).


callbackTimes(Session, QueryName, Updating, JavaCall, Parsing, Deletion, Insertion) :-
    log(Session, NI, CPUi, framesWereInserted(Updating)),
    Nd is NI+ -1,
    log(Session, Nd, CPUd, framesWereDeleted(_)),
    Np is NI+ -2,
    log(Session, Np, CPUp, framesWereParsed),
    Nj is NI+ -3,
    log(Session, Nj, CPUj, javaMessageReturned),
    Nc is Nj+ -1,
    log(Session, Nc, CPUc, Call),
    Call='%getMetaQLQueryResults'(_CONTEXT, _Module, _HandlerClass, QueryName, _Params, _Status),
    JavaCall is CPUj-CPUc,
    Insertion is CPUi-CPUd,
    Parsing is CPUp-CPUj,
    Deletion is CPUd-CPUp.

log('2020-06-03T11:16:25', 1, 63.756, '%getMetaQLQueryResults'('8b6a4bbd-6bf7-45b3-a543-a2ca32c42114', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/FoodServices', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6008329)).
log('2020-06-03T11:16:25', 2, 63.758, javaMessageReturned).
log('2020-06-03T11:16:25', 3, 63.76, '%getMetaQLQueryResults'('8b6a4bbd-6bf7-45b3-a543-a2ca32c42114', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/Environmental', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6008387)).
log('2020-06-03T11:16:25', 4, 63.761, javaMessageReturned).
log('2020-06-03T11:16:25', 5, 63.763, '%getMetaQLQueryResults'('8b6a4bbd-6bf7-45b3-a543-a2ca32c42114', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/RealEstate', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6008445)).
log('2020-06-03T11:16:25', 6, 63.764, javaMessageReturned).
log('2020-06-03T11:16:25', 7, 63.765, '%getMetaQLQueryResults'('8b6a4bbd-6bf7-45b3-a543-a2ca32c42114', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/Utilities', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6008503)).
log('2020-06-03T11:16:25', 8, 63.767, javaMessageReturned).
log('2020-06-03T11:16:25', 9, 63.768, '%getMetaQLQueryResults'('8b6a4bbd-6bf7-45b3-a543-a2ca32c42114', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/WholesaleTrade', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6008561)).
log('2020-06-03T11:16:25', 10, 63.77, javaMessageReturned).
log('2020-06-03T11:16:25', 11, 63.772, '%getMetaQLQueryResults'('8b6a4bbd-6bf7-45b3-a543-a2ca32c42114', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/PoliticalOrganizations', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6008619)).
log('2020-06-03T11:16:25', 12, 63.774, javaMessageReturned).
log('2020-06-03T11:16:25', 13, 63.775, '%getMetaQLQueryResults'('8b6a4bbd-6bf7-45b3-a543-a2ca32c42114', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/Industry', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6008677)).
log('2020-06-03T11:16:25', 14, 63.777, javaMessageReturned).
log('2020-06-03T11:16:25', 15, 63.778, '%getMetaQLQueryResults'('8b6a4bbd-6bf7-45b3-a543-a2ca32c42114', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/FinancialServices', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6008735)).
log('2020-06-03T11:16:25', 16, 63.788, javaMessageReturned).
log('2020-06-03T11:16:25', 17, 63.813, framesWereParsed).
log('2020-06-03T11:16:25', 18, 63.998, framesWereDeleted(14)).
log('2020-06-03T11:16:25', 19, 66.68, framesWereInserted('\\true')).
log('2020-06-03T11:16:25', 20, 66.954, ruleExiting).
log('2020-06-03T11:16:25', 21, 68.497, '%getMetaQLQueryResults'('8b6a4bbd-6bf7-45b3-a543-a2ca32c42114', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/Agriculture', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6181758)).
log('2020-06-03T11:16:25', 22, 68.499, javaMessageReturned).
log('2020-06-03T11:16:25', 23, 68.5, '%getMetaQLQueryResults'('8b6a4bbd-6bf7-45b3-a543-a2ca32c42114', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/Construction', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6181816)).
log('2020-06-03T11:16:25', 24, 68.502, javaMessageReturned).
log('2020-06-03T11:16:25', 25, 68.504, '%getMetaQLQueryResults'('8b6a4bbd-6bf7-45b3-a543-a2ca32c42114', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/ArtsMediaEntertainment', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6181874)).
log('2020-06-03T11:16:25', 26, 68.506, javaMessageReturned).
log('2020-06-03T11:16:25', 27, 68.507, '%getMetaQLQueryResults'('8b6a4bbd-6bf7-45b3-a543-a2ca32c42114', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/SocialServices', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6181932)).
log('2020-06-03T11:16:25', 28, 68.509, javaMessageReturned).
log('2020-06-03T11:16:25', 29, 68.511, '%getMetaQLQueryResults'('8b6a4bbd-6bf7-45b3-a543-a2ca32c42114', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/Education', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6181990)).
log('2020-06-03T11:16:25', 30, 68.514, javaMessageReturned).
log('2020-06-03T11:16:25', 31, 68.515, '%getMetaQLQueryResults'('8b6a4bbd-6bf7-45b3-a543-a2ca32c42114', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/Health', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6182048)).
log('2020-06-03T11:16:25', 32, 68.517, javaMessageReturned).
log('2020-06-03T11:16:25', 33, 68.518, '%getMetaQLQueryResults'('8b6a4bbd-6bf7-45b3-a543-a2ca32c42114', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/Hospitality', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6182106)).
log('2020-06-03T11:16:25', 34, 68.52, javaMessageReturned).
log('2020-06-03T11:16:25', 35, 68.521, '%getMetaQLQueryResults'('8b6a4bbd-6bf7-45b3-a543-a2ca32c42114', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/PublicAdministration', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6182164)).
log('2020-06-03T11:16:25', 36, 68.523, javaMessageReturned).
log('2020-06-03T11:16:25', 37, 68.524, '%getMetaQLQueryResults'('8b6a4bbd-6bf7-45b3-a543-a2ca32c42114', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/ReligiousServices', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6182222)).
log('2020-06-03T11:16:25', 38, 68.527, javaMessageReturned).
log('2020-06-03T11:16:25', 39, 68.528, '%getMetaQLQueryResults'('8b6a4bbd-6bf7-45b3-a543-a2ca32c42114', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/EnergyandMining', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6182280)).
log('2020-06-03T11:16:25', 40, 68.53, javaMessageReturned).
log('2020-06-03T11:16:25', 41, 68.531, '%getMetaQLQueryResults'('8b6a4bbd-6bf7-45b3-a543-a2ca32c42114', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/RetailTrade', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6182338)).
log('2020-06-03T11:16:25', 42, 68.534, javaMessageReturned).
log('2020-06-03T11:16:25', 43, 68.535, '%getMetaQLQueryResults'('8b6a4bbd-6bf7-45b3-a543-a2ca32c42114', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/Manufacturing', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6182396)).
log('2020-06-03T11:16:25', 44, 68.537, javaMessageReturned).
log('2020-06-03T11:16:25', 45, 68.539, '%getMetaQLQueryResults'('8b6a4bbd-6bf7-45b3-a543-a2ca32c42114', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/TransportationandWarehousing', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6182454)).
log('2020-06-03T11:16:25', 46, 68.541, javaMessageReturned).
log('2020-06-03T11:16:25', 47, 68.542, '%getMetaQLQueryResults'('8b6a4bbd-6bf7-45b3-a543-a2ca32c42114', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/ProfessionalServices', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6182512)).
log('2020-06-03T11:16:25', 48, 68.544, javaMessageReturned).
log('2020-06-03T11:16:25', 49, 68.545, '%getMetaQLQueryResults'('8b6a4bbd-6bf7-45b3-a543-a2ca32c42114', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/Technology', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6182570)).
log('2020-06-03T11:16:25', 50, 68.547, javaMessageReturned).
log('2020-06-03T11:16:25', 51, 68.548, '%getMetaQLQueryResults'('8b6a4bbd-6bf7-45b3-a543-a2ca32c42114', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/OtherServices', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6182628)).
log('2020-06-03T11:16:25', 52, 68.55, javaMessageReturned).
log('2020-06-03T11:16:25', 53, 71.197, '%getMetaQLQueryResults'('c4c55000-48d3-491a-93ba-7b7595eb4232', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/FoodServices', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h8078859)).
log('2020-06-03T11:16:25', 54, 71.199, javaMessageReturned).
log('2020-06-03T11:16:25', 55, 71.203, '%getMetaQLQueryResults'('c4c55000-48d3-491a-93ba-7b7595eb4232', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/Environmental', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h8078917)).
log('2020-06-03T11:16:25', 56, 71.205, javaMessageReturned).
log('2020-06-03T11:16:25', 57, 71.207, '%getMetaQLQueryResults'('c4c55000-48d3-491a-93ba-7b7595eb4232', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/RealEstate', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h8078975)).
log('2020-06-03T11:16:25', 58, 71.209, javaMessageReturned).
log('2020-06-03T11:16:25', 59, 71.21, '%getMetaQLQueryResults'('c4c55000-48d3-491a-93ba-7b7595eb4232', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/Utilities', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h8079033)).
log('2020-06-03T11:16:25', 60, 71.212, javaMessageReturned).
log('2020-06-03T11:16:25', 61, 71.214, '%getMetaQLQueryResults'('c4c55000-48d3-491a-93ba-7b7595eb4232', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/WholesaleTrade', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h8079091)).
log('2020-06-03T11:16:25', 62, 71.216, javaMessageReturned).
log('2020-06-03T11:16:25', 63, 71.218, '%getMetaQLQueryResults'('c4c55000-48d3-491a-93ba-7b7595eb4232', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/PoliticalOrganizations', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h8079149)).
log('2020-06-03T11:16:25', 64, 71.22, javaMessageReturned).
log('2020-06-03T11:16:25', 65, 71.222, '%getMetaQLQueryResults'('c4c55000-48d3-491a-93ba-7b7595eb4232', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/Industry', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h8079207)).
log('2020-06-03T11:16:25', 66, 71.224, javaMessageReturned).
log('2020-06-03T11:16:25', 67, 71.225, '%getMetaQLQueryResults'('c4c55000-48d3-491a-93ba-7b7595eb4232', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/FinancialServices', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h8079265)).
log('2020-06-03T11:16:25', 68, 71.235, javaMessageReturned).
log('2020-06-03T11:16:25', 69, 71.257, framesWereParsed).
log('2020-06-03T11:16:25', 70, 71.274, framesWereDeleted(14)).
log('2020-06-03T11:16:25', 71, 74.388, framesWereInserted('\\true')).
log('2020-06-03T11:16:25', 72, 74.491, ruleExiting).
log('2020-06-03T11:16:25', 73, 76.033, '%getMetaQLQueryResults'('c4c55000-48d3-491a-93ba-7b7595eb4232', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/Agriculture', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h8251761)).
log('2020-06-03T11:16:25', 74, 76.035, javaMessageReturned).
log('2020-06-03T11:16:25', 75, 76.036, '%getMetaQLQueryResults'('c4c55000-48d3-491a-93ba-7b7595eb4232', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/Construction', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h8251819)).
log('2020-06-03T11:16:25', 76, 76.038, javaMessageReturned).
log('2020-06-03T11:16:25', 77, 76.04, '%getMetaQLQueryResults'('c4c55000-48d3-491a-93ba-7b7595eb4232', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/ArtsMediaEntertainment', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h8251877)).
log('2020-06-03T11:16:25', 78, 76.041, javaMessageReturned).
log('2020-06-03T11:16:25', 79, 76.043, '%getMetaQLQueryResults'('c4c55000-48d3-491a-93ba-7b7595eb4232', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/SocialServices', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h8251935)).
log('2020-06-03T11:16:25', 80, 76.045, javaMessageReturned).
log('2020-06-03T11:16:25', 81, 76.046, '%getMetaQLQueryResults'('c4c55000-48d3-491a-93ba-7b7595eb4232', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/Education', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h8251993)).
log('2020-06-03T11:16:25', 82, 76.048, javaMessageReturned).
log('2020-06-03T11:16:25', 83, 76.05, '%getMetaQLQueryResults'('c4c55000-48d3-491a-93ba-7b7595eb4232', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/Health', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h8252051)).
log('2020-06-03T11:16:25', 84, 76.052, javaMessageReturned).
log('2020-06-03T11:16:25', 85, 76.054, '%getMetaQLQueryResults'('c4c55000-48d3-491a-93ba-7b7595eb4232', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/Hospitality', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h8252109)).
log('2020-06-03T11:16:25', 86, 76.055, javaMessageReturned).
log('2020-06-03T11:16:25', 87, 76.057, '%getMetaQLQueryResults'('c4c55000-48d3-491a-93ba-7b7595eb4232', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/PublicAdministration', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h8252167)).
log('2020-06-03T11:16:25', 88, 76.059, javaMessageReturned).
log('2020-06-03T11:16:25', 89, 76.061, '%getMetaQLQueryResults'('c4c55000-48d3-491a-93ba-7b7595eb4232', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/ReligiousServices', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h8252225)).
log('2020-06-03T11:16:25', 90, 76.063, javaMessageReturned).
log('2020-06-03T11:16:25', 91, 76.065, '%getMetaQLQueryResults'('c4c55000-48d3-491a-93ba-7b7595eb4232', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/EnergyandMining', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h8252283)).
log('2020-06-03T11:16:25', 92, 76.067, javaMessageReturned).
log('2020-06-03T11:16:25', 93, 76.068, '%getMetaQLQueryResults'('c4c55000-48d3-491a-93ba-7b7595eb4232', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/RetailTrade', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h8252341)).
log('2020-06-03T11:16:25', 94, 76.07, javaMessageReturned).
log('2020-06-03T11:16:25', 95, 76.072, '%getMetaQLQueryResults'('c4c55000-48d3-491a-93ba-7b7595eb4232', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/Manufacturing', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h8252399)).
log('2020-06-03T11:16:25', 96, 76.074, javaMessageReturned).
log('2020-06-03T11:16:25', 97, 76.075, '%getMetaQLQueryResults'('c4c55000-48d3-491a-93ba-7b7595eb4232', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/TransportationandWarehousing', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h8252457)).
log('2020-06-03T11:16:25', 98, 76.077, javaMessageReturned).
log('2020-06-03T11:16:25', 99, 76.078, '%getMetaQLQueryResults'('c4c55000-48d3-491a-93ba-7b7595eb4232', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/ProfessionalServices', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h8252515)).
log('2020-06-03T11:16:25', 100, 76.08, javaMessageReturned).
log('2020-06-03T11:16:25', 101, 76.082, '%getMetaQLQueryResults'('c4c55000-48d3-491a-93ba-7b7595eb4232', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/Technology', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h8252573)).
log('2020-06-03T11:16:25', 102, 76.084, javaMessageReturned).
log('2020-06-03T11:16:25', 103, 76.085, '%getMetaQLQueryResults'('c4c55000-48d3-491a-93ba-7b7595eb4232', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/OtherServices', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h8252631)).
log('2020-06-03T11:16:25', 104, 76.087, javaMessageReturned).
log('2020-06-03T11:16:25', 105, 78.477, '%getMetaQLQueryResults'('ecb6d2da-2c72-48b7-ad9f-666b7c086f7b', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/FoodServices', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h8081685)).
log('2020-06-03T11:16:25', 106, 78.48, javaMessageReturned).
log('2020-06-03T11:16:25', 107, 78.483, '%getMetaQLQueryResults'('ecb6d2da-2c72-48b7-ad9f-666b7c086f7b', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/Environmental', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h8081743)).
log('2020-06-03T11:16:25', 108, 78.485, javaMessageReturned).
log('2020-06-03T11:16:25', 109, 78.487, '%getMetaQLQueryResults'('ecb6d2da-2c72-48b7-ad9f-666b7c086f7b', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/RealEstate', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h8081801)).
log('2020-06-03T11:16:25', 110, 78.49, javaMessageReturned).
log('2020-06-03T11:16:25', 111, 78.491, '%getMetaQLQueryResults'('ecb6d2da-2c72-48b7-ad9f-666b7c086f7b', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/Utilities', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h8081859)).
log('2020-06-03T11:16:25', 112, 78.493, javaMessageReturned).
log('2020-06-03T11:16:25', 113, 78.494, '%getMetaQLQueryResults'('ecb6d2da-2c72-48b7-ad9f-666b7c086f7b', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/WholesaleTrade', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h8081917)).
log('2020-06-03T11:16:25', 114, 78.497, javaMessageReturned).
log('2020-06-03T11:16:25', 115, 78.499, '%getMetaQLQueryResults'('ecb6d2da-2c72-48b7-ad9f-666b7c086f7b', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/PoliticalOrganizations', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h8081975)).
log('2020-06-03T11:16:25', 116, 78.501, javaMessageReturned).
log('2020-06-03T11:16:25', 117, 78.504, '%getMetaQLQueryResults'('ecb6d2da-2c72-48b7-ad9f-666b7c086f7b', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/Industry', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h8082033)).
log('2020-06-03T11:16:25', 118, 78.506, javaMessageReturned).
log('2020-06-03T11:16:25', 119, 78.509, '%getMetaQLQueryResults'('ecb6d2da-2c72-48b7-ad9f-666b7c086f7b', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/FinancialServices', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h8082091)).
log('2020-06-03T11:16:25', 120, 78.522, javaMessageReturned).
log('2020-06-03T11:16:25', 121, 78.547, framesWereParsed).
log('2020-06-03T11:16:25', 122, 78.568, framesWereDeleted(14)).
log('2020-06-03T11:16:25', 123, 81.691, framesWereInserted('\\true')).
log('2020-06-03T11:16:25', 124, 81.791, ruleExiting).
log('2020-06-03T11:16:25', 125, 83.303, '%getMetaQLQueryResults'('ecb6d2da-2c72-48b7-ad9f-666b7c086f7b', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/Agriculture', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h8254587)).
log('2020-06-03T11:16:25', 126, 83.306, javaMessageReturned).
log('2020-06-03T11:16:25', 127, 83.308, '%getMetaQLQueryResults'('ecb6d2da-2c72-48b7-ad9f-666b7c086f7b', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/Construction', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h8254645)).
log('2020-06-03T11:16:25', 128, 83.309, javaMessageReturned).
log('2020-06-03T11:16:25', 129, 83.311, '%getMetaQLQueryResults'('ecb6d2da-2c72-48b7-ad9f-666b7c086f7b', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/ArtsMediaEntertainment', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h8254703)).
log('2020-06-03T11:16:25', 130, 83.313, javaMessageReturned).
log('2020-06-03T11:16:25', 131, 83.314, '%getMetaQLQueryResults'('ecb6d2da-2c72-48b7-ad9f-666b7c086f7b', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/SocialServices', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h8254761)).
log('2020-06-03T11:16:25', 132, 83.316, javaMessageReturned).
log('2020-06-03T11:16:25', 133, 83.318, '%getMetaQLQueryResults'('ecb6d2da-2c72-48b7-ad9f-666b7c086f7b', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/Education', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h8254819)).
log('2020-06-03T11:16:25', 134, 83.32, javaMessageReturned).
log('2020-06-03T11:16:25', 135, 83.322, '%getMetaQLQueryResults'('ecb6d2da-2c72-48b7-ad9f-666b7c086f7b', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/Health', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h8254877)).
log('2020-06-03T11:16:25', 136, 83.324, javaMessageReturned).
log('2020-06-03T11:16:25', 137, 83.326, '%getMetaQLQueryResults'('ecb6d2da-2c72-48b7-ad9f-666b7c086f7b', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/Hospitality', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h8254935)).
log('2020-06-03T11:16:25', 138, 83.328, javaMessageReturned).
log('2020-06-03T11:16:25', 139, 83.33, '%getMetaQLQueryResults'('ecb6d2da-2c72-48b7-ad9f-666b7c086f7b', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/PublicAdministration', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h8254993)).
log('2020-06-03T11:16:25', 140, 83.332, javaMessageReturned).
log('2020-06-03T11:16:25', 141, 83.334, '%getMetaQLQueryResults'('ecb6d2da-2c72-48b7-ad9f-666b7c086f7b', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/ReligiousServices', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h8255051)).
log('2020-06-03T11:16:25', 142, 83.336, javaMessageReturned).
log('2020-06-03T11:16:25', 143, 83.338, '%getMetaQLQueryResults'('ecb6d2da-2c72-48b7-ad9f-666b7c086f7b', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/EnergyandMining', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h8255109)).
log('2020-06-03T11:16:25', 144, 83.34, javaMessageReturned).
log('2020-06-03T11:16:25', 145, 83.341, '%getMetaQLQueryResults'('ecb6d2da-2c72-48b7-ad9f-666b7c086f7b', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/RetailTrade', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h8255167)).
log('2020-06-03T11:16:25', 146, 83.343, javaMessageReturned).
log('2020-06-03T11:16:25', 147, 83.346, '%getMetaQLQueryResults'('ecb6d2da-2c72-48b7-ad9f-666b7c086f7b', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/Manufacturing', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h8255225)).
log('2020-06-03T11:16:25', 148, 83.349, javaMessageReturned).
log('2020-06-03T11:16:25', 149, 83.351, '%getMetaQLQueryResults'('ecb6d2da-2c72-48b7-ad9f-666b7c086f7b', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/TransportationandWarehousing', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h8255283)).
log('2020-06-03T11:16:25', 150, 83.353, javaMessageReturned).
log('2020-06-03T11:16:25', 151, 83.354, '%getMetaQLQueryResults'('ecb6d2da-2c72-48b7-ad9f-666b7c086f7b', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/ProfessionalServices', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h8255341)).
log('2020-06-03T11:16:25', 152, 83.356, javaMessageReturned).
log('2020-06-03T11:16:25', 153, 83.358, '%getMetaQLQueryResults'('ecb6d2da-2c72-48b7-ad9f-666b7c086f7b', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/Technology', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h8255399)).
log('2020-06-03T11:16:25', 154, 83.36, javaMessageReturned).
log('2020-06-03T11:16:25', 155, 83.361, '%getMetaQLQueryResults'('ecb6d2da-2c72-48b7-ad9f-666b7c086f7b', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/OtherServices', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h8255457)).
log('2020-06-03T11:16:25', 156, 83.364, javaMessageReturned).
log('2020-06-03T11:16:25', 157, 85.766, '%getMetaQLQueryResults'('dbcf3d74-d506-4887-ad7c-5e546b547001', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/FoodServices', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h8081685)).
log('2020-06-03T11:16:25', 158, 85.768, javaMessageReturned).
log('2020-06-03T11:16:25', 159, 85.772, '%getMetaQLQueryResults'('dbcf3d74-d506-4887-ad7c-5e546b547001', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/Environmental', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h8081743)).
log('2020-06-03T11:16:25', 160, 85.773, javaMessageReturned).
log('2020-06-03T11:16:25', 161, 85.775, '%getMetaQLQueryResults'('dbcf3d74-d506-4887-ad7c-5e546b547001', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/RealEstate', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h8081801)).
log('2020-06-03T11:16:25', 162, 85.777, javaMessageReturned).
log('2020-06-03T11:16:25', 163, 85.779, '%getMetaQLQueryResults'('dbcf3d74-d506-4887-ad7c-5e546b547001', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/Utilities', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h8081859)).
log('2020-06-03T11:16:25', 164, 85.781, javaMessageReturned).
log('2020-06-03T11:16:25', 165, 85.784, '%getMetaQLQueryResults'('dbcf3d74-d506-4887-ad7c-5e546b547001', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/WholesaleTrade', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h8081917)).
log('2020-06-03T11:16:25', 166, 85.786, javaMessageReturned).
log('2020-06-03T11:16:25', 167, 85.788, '%getMetaQLQueryResults'('dbcf3d74-d506-4887-ad7c-5e546b547001', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/PoliticalOrganizations', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h8081975)).
log('2020-06-03T11:16:25', 168, 85.79, javaMessageReturned).
log('2020-06-03T11:16:25', 169, 85.791, '%getMetaQLQueryResults'('dbcf3d74-d506-4887-ad7c-5e546b547001', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/Industry', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h8082033)).
log('2020-06-03T11:16:25', 170, 85.793, javaMessageReturned).
log('2020-06-03T11:16:25', 171, 85.794, '%getMetaQLQueryResults'('dbcf3d74-d506-4887-ad7c-5e546b547001', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/FinancialServices', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h8082091)).
log('2020-06-03T11:16:25', 172, 85.805, javaMessageReturned).
log('2020-06-03T11:16:25', 173, 85.828, framesWereParsed).
log('2020-06-03T11:16:25', 174, 85.845, framesWereDeleted(14)).
log('2020-06-03T11:16:25', 175, 88.951, framesWereInserted('\\true')).
log('2020-06-03T11:16:25', 176, 89.051, ruleExiting).
log('2020-06-03T11:16:25', 177, 90.587, '%getMetaQLQueryResults'('dbcf3d74-d506-4887-ad7c-5e546b547001', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/Agriculture', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h8254587)).
log('2020-06-03T11:16:25', 178, 90.589, javaMessageReturned).
log('2020-06-03T11:16:25', 179, 90.591, '%getMetaQLQueryResults'('dbcf3d74-d506-4887-ad7c-5e546b547001', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/Construction', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h8254645)).
log('2020-06-03T11:16:25', 180, 90.593, javaMessageReturned).
log('2020-06-03T11:16:25', 181, 90.594, '%getMetaQLQueryResults'('dbcf3d74-d506-4887-ad7c-5e546b547001', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/ArtsMediaEntertainment', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h8254703)).
log('2020-06-03T11:16:25', 182, 90.596, javaMessageReturned).
log('2020-06-03T11:16:25', 183, 90.598, '%getMetaQLQueryResults'('dbcf3d74-d506-4887-ad7c-5e546b547001', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/SocialServices', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h8254761)).
log('2020-06-03T11:16:25', 184, 90.599, javaMessageReturned).
log('2020-06-03T11:16:25', 185, 90.601, '%getMetaQLQueryResults'('dbcf3d74-d506-4887-ad7c-5e546b547001', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/Education', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h8254819)).
log('2020-06-03T11:16:25', 186, 90.602, javaMessageReturned).
log('2020-06-03T11:16:25', 187, 90.604, '%getMetaQLQueryResults'('dbcf3d74-d506-4887-ad7c-5e546b547001', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/Health', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h8254877)).
log('2020-06-03T11:16:25', 188, 90.606, javaMessageReturned).
log('2020-06-03T11:16:25', 189, 90.607, '%getMetaQLQueryResults'('dbcf3d74-d506-4887-ad7c-5e546b547001', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/Hospitality', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h8254935)).
log('2020-06-03T11:16:25', 190, 90.609, javaMessageReturned).
log('2020-06-03T11:16:25', 191, 90.611, '%getMetaQLQueryResults'('dbcf3d74-d506-4887-ad7c-5e546b547001', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/PublicAdministration', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h8254993)).
log('2020-06-03T11:16:25', 192, 90.612, javaMessageReturned).
log('2020-06-03T11:16:25', 193, 90.614, '%getMetaQLQueryResults'('dbcf3d74-d506-4887-ad7c-5e546b547001', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/ReligiousServices', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h8255051)).
log('2020-06-03T11:16:25', 194, 90.616, javaMessageReturned).
log('2020-06-03T11:16:25', 195, 90.617, '%getMetaQLQueryResults'('dbcf3d74-d506-4887-ad7c-5e546b547001', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/EnergyandMining', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h8255109)).
log('2020-06-03T11:16:25', 196, 90.619, javaMessageReturned).
log('2020-06-03T11:16:25', 197, 90.62, '%getMetaQLQueryResults'('dbcf3d74-d506-4887-ad7c-5e546b547001', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/RetailTrade', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h8255167)).
log('2020-06-03T11:16:25', 198, 90.622, javaMessageReturned).
log('2020-06-03T11:16:25', 199, 90.623, '%getMetaQLQueryResults'('dbcf3d74-d506-4887-ad7c-5e546b547001', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/Manufacturing', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h8255225)).
log('2020-06-03T11:16:25', 200, 90.625, javaMessageReturned).
log('2020-06-03T11:16:25', 201, 90.627, '%getMetaQLQueryResults'('dbcf3d74-d506-4887-ad7c-5e546b547001', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/TransportationandWarehousing', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h8255283)).
log('2020-06-03T11:16:25', 202, 90.628, javaMessageReturned).
log('2020-06-03T11:16:25', 203, 90.63, '%getMetaQLQueryResults'('dbcf3d74-d506-4887-ad7c-5e546b547001', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/ProfessionalServices', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h8255341)).
log('2020-06-03T11:16:25', 204, 90.631, javaMessageReturned).
log('2020-06-03T11:16:25', 205, 90.633, '%getMetaQLQueryResults'('dbcf3d74-d506-4887-ad7c-5e546b547001', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/Technology', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h8255399)).
log('2020-06-03T11:16:25', 206, 90.635, javaMessageReturned).
log('2020-06-03T11:16:25', 207, 90.637, '%getMetaQLQueryResults'('dbcf3d74-d506-4887-ad7c-5e546b547001', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/OtherServices', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h8255457)).
log('2020-06-03T11:16:25', 208, 90.639, javaMessageReturned).
log('2020-06-03T11:16:25', 209, 94.715, '%getMetaQLQueryResults'('4aad9803-3e2c-4f1a-ac18-239fbcd1c51a', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/FoodServices', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6016048)).
log('2020-06-03T11:16:25', 210, 94.717, javaMessageReturned).
log('2020-06-03T11:16:25', 211, 94.721, '%getMetaQLQueryResults'('4aad9803-3e2c-4f1a-ac18-239fbcd1c51a', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/Environmental', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6016106)).
log('2020-06-03T11:16:25', 212, 94.723, javaMessageReturned).
log('2020-06-03T11:16:25', 213, 94.724, '%getMetaQLQueryResults'('4aad9803-3e2c-4f1a-ac18-239fbcd1c51a', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/RealEstate', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6016164)).
log('2020-06-03T11:16:25', 214, 94.726, javaMessageReturned).
log('2020-06-03T11:16:25', 215, 94.728, '%getMetaQLQueryResults'('4aad9803-3e2c-4f1a-ac18-239fbcd1c51a', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/Utilities', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6016222)).
log('2020-06-03T11:16:25', 216, 94.729, javaMessageReturned).
log('2020-06-03T11:16:25', 217, 94.731, '%getMetaQLQueryResults'('4aad9803-3e2c-4f1a-ac18-239fbcd1c51a', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/WholesaleTrade', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6016280)).
log('2020-06-03T11:16:25', 218, 94.733, javaMessageReturned).
log('2020-06-03T11:16:25', 219, 94.734, '%getMetaQLQueryResults'('4aad9803-3e2c-4f1a-ac18-239fbcd1c51a', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/PoliticalOrganizations', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6016338)).
log('2020-06-03T11:16:25', 220, 94.736, javaMessageReturned).
log('2020-06-03T11:16:25', 221, 94.738, '%getMetaQLQueryResults'('4aad9803-3e2c-4f1a-ac18-239fbcd1c51a', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/Industry', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6016396)).
log('2020-06-03T11:16:25', 222, 94.74, javaMessageReturned).
log('2020-06-03T11:16:25', 223, 94.742, '%getMetaQLQueryResults'('4aad9803-3e2c-4f1a-ac18-239fbcd1c51a', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/FinancialServices', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6016454)).
log('2020-06-03T11:16:25', 224, 94.753, javaMessageReturned).
log('2020-06-03T11:16:25', 225, 94.78, framesWereParsed).
log('2020-06-03T11:16:25', 226, 95.038, framesWereDeleted(14)).
log('2020-06-03T11:16:25', 227, 97.335, framesWereInserted('\\true')).
log('2020-06-03T11:16:25', 228, 97.635, ruleExiting).
log('2020-06-03T11:16:25', 229, 129.109, '%getMetaQLQueryResults'('4aad9803-3e2c-4f1a-ac18-239fbcd1c51a', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/Agriculture', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6189066)).
log('2020-06-03T11:16:25', 230, 129.111, javaMessageReturned).
log('2020-06-03T11:16:25', 231, 129.114, '%getMetaQLQueryResults'('4aad9803-3e2c-4f1a-ac18-239fbcd1c51a', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/Construction', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6189124)).
log('2020-06-03T11:16:25', 232, 129.116, javaMessageReturned).
log('2020-06-03T11:16:25', 233, 129.118, '%getMetaQLQueryResults'('4aad9803-3e2c-4f1a-ac18-239fbcd1c51a', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/ArtsMediaEntertainment', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6189182)).
log('2020-06-03T11:16:25', 234, 129.12, javaMessageReturned).
log('2020-06-03T11:16:25', 235, 129.122, '%getMetaQLQueryResults'('4aad9803-3e2c-4f1a-ac18-239fbcd1c51a', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/SocialServices', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6189240)).
log('2020-06-03T11:16:25', 236, 129.124, javaMessageReturned).
log('2020-06-03T11:16:25', 237, 129.126, '%getMetaQLQueryResults'('4aad9803-3e2c-4f1a-ac18-239fbcd1c51a', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/Education', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6189298)).
log('2020-06-03T11:16:25', 238, 129.128, javaMessageReturned).
log('2020-06-03T11:16:25', 239, 129.13, '%getMetaQLQueryResults'('4aad9803-3e2c-4f1a-ac18-239fbcd1c51a', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/Health', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6189356)).
log('2020-06-03T11:16:25', 240, 129.132, javaMessageReturned).
log('2020-06-03T11:16:25', 241, 129.134, '%getMetaQLQueryResults'('4aad9803-3e2c-4f1a-ac18-239fbcd1c51a', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/Hospitality', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6189414)).
log('2020-06-03T11:16:25', 242, 129.136, javaMessageReturned).
log('2020-06-03T11:16:25', 243, 129.138, '%getMetaQLQueryResults'('4aad9803-3e2c-4f1a-ac18-239fbcd1c51a', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/PublicAdministration', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6189472)).
log('2020-06-03T11:16:25', 244, 129.14, javaMessageReturned).
log('2020-06-03T11:16:25', 245, 129.142, '%getMetaQLQueryResults'('4aad9803-3e2c-4f1a-ac18-239fbcd1c51a', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/ReligiousServices', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6189530)).
log('2020-06-03T11:16:25', 246, 129.144, javaMessageReturned).
log('2020-06-03T11:16:25', 247, 129.146, '%getMetaQLQueryResults'('4aad9803-3e2c-4f1a-ac18-239fbcd1c51a', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/EnergyandMining', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6189588)).
log('2020-06-03T11:16:25', 248, 129.148, javaMessageReturned).
log('2020-06-03T11:16:25', 249, 129.15, '%getMetaQLQueryResults'('4aad9803-3e2c-4f1a-ac18-239fbcd1c51a', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/RetailTrade', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6189646)).
log('2020-06-03T11:16:25', 250, 129.152, javaMessageReturned).
log('2020-06-03T11:16:25', 251, 129.154, '%getMetaQLQueryResults'('4aad9803-3e2c-4f1a-ac18-239fbcd1c51a', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/Manufacturing', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6189704)).
log('2020-06-03T11:16:25', 252, 129.156, javaMessageReturned).
log('2020-06-03T11:16:25', 253, 129.158, '%getMetaQLQueryResults'('4aad9803-3e2c-4f1a-ac18-239fbcd1c51a', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/TransportationandWarehousing', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6189762)).
log('2020-06-03T11:16:25', 254, 129.16, javaMessageReturned).
log('2020-06-03T11:16:25', 255, 129.163, '%getMetaQLQueryResults'('4aad9803-3e2c-4f1a-ac18-239fbcd1c51a', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/ProfessionalServices', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6189820)).
log('2020-06-03T11:16:25', 256, 129.165, javaMessageReturned).
log('2020-06-03T11:16:25', 257, 129.166, '%getMetaQLQueryResults'('4aad9803-3e2c-4f1a-ac18-239fbcd1c51a', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/Technology', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6189878)).
log('2020-06-03T11:16:25', 258, 129.169, javaMessageReturned).
log('2020-06-03T11:16:25', 259, 129.17, '%getMetaQLQueryResults'('4aad9803-3e2c-4f1a-ac18-239fbcd1c51a', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/OtherServices', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6189936)).
log('2020-06-03T11:16:25', 260, 129.173, javaMessageReturned).
log('2020-06-03T11:16:25', 261, 134.322, '%getMetaQLQueryResults'('388113aa-50f3-4d1a-9852-87d67dcd2743', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/FoodServices', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6016048)).
log('2020-06-03T11:16:25', 262, 134.324, javaMessageReturned).
log('2020-06-03T11:16:25', 263, 134.328, '%getMetaQLQueryResults'('388113aa-50f3-4d1a-9852-87d67dcd2743', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/Environmental', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6016106)).
log('2020-06-03T11:16:25', 264, 134.33, javaMessageReturned).
log('2020-06-03T11:16:25', 265, 134.332, '%getMetaQLQueryResults'('388113aa-50f3-4d1a-9852-87d67dcd2743', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/RealEstate', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6016164)).
log('2020-06-03T11:16:25', 266, 134.334, javaMessageReturned).
log('2020-06-03T11:16:25', 267, 134.335, '%getMetaQLQueryResults'('388113aa-50f3-4d1a-9852-87d67dcd2743', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/Utilities', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6016222)).
log('2020-06-03T11:16:25', 268, 134.337, javaMessageReturned).
log('2020-06-03T11:16:25', 269, 134.338, '%getMetaQLQueryResults'('388113aa-50f3-4d1a-9852-87d67dcd2743', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/WholesaleTrade', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6016280)).
log('2020-06-03T11:16:25', 270, 134.34, javaMessageReturned).
log('2020-06-03T11:16:25', 271, 134.343, '%getMetaQLQueryResults'('388113aa-50f3-4d1a-9852-87d67dcd2743', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/PoliticalOrganizations', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6016338)).
log('2020-06-03T11:16:25', 272, 134.345, javaMessageReturned).
log('2020-06-03T11:16:25', 273, 134.347, '%getMetaQLQueryResults'('388113aa-50f3-4d1a-9852-87d67dcd2743', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/Industry', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6016396)).
log('2020-06-03T11:16:25', 274, 134.35, javaMessageReturned).
log('2020-06-03T11:16:25', 275, 134.352, '%getMetaQLQueryResults'('388113aa-50f3-4d1a-9852-87d67dcd2743', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/FinancialServices', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6016454)).
log('2020-06-03T11:16:25', 276, 134.363, javaMessageReturned).
log('2020-06-03T11:16:25', 277, 134.39, framesWereParsed).
log('2020-06-03T11:16:25', 278, 134.653, framesWereDeleted(14)).
log('2020-06-03T11:16:25', 279, 137.358, framesWereInserted('\\true')).
log('2020-06-03T11:16:25', 280, 137.719, ruleExiting).
log('2020-06-03T11:16:25', 281, 165.209, '%getMetaQLQueryResults'('388113aa-50f3-4d1a-9852-87d67dcd2743', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/Agriculture', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6189066)).
log('2020-06-03T11:16:25', 282, 165.211, javaMessageReturned).
log('2020-06-03T11:16:25', 283, 165.213, '%getMetaQLQueryResults'('388113aa-50f3-4d1a-9852-87d67dcd2743', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/Construction', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6189124)).
log('2020-06-03T11:16:25', 284, 165.215, javaMessageReturned).
log('2020-06-03T11:16:25', 285, 165.217, '%getMetaQLQueryResults'('388113aa-50f3-4d1a-9852-87d67dcd2743', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/ArtsMediaEntertainment', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6189182)).
log('2020-06-03T11:16:25', 286, 165.218, javaMessageReturned).
log('2020-06-03T11:16:25', 287, 165.22, '%getMetaQLQueryResults'('388113aa-50f3-4d1a-9852-87d67dcd2743', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/SocialServices', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6189240)).
log('2020-06-03T11:16:25', 288, 165.222, javaMessageReturned).
log('2020-06-03T11:16:25', 289, 165.223, '%getMetaQLQueryResults'('388113aa-50f3-4d1a-9852-87d67dcd2743', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/Education', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6189298)).
log('2020-06-03T11:16:25', 290, 165.226, javaMessageReturned).
log('2020-06-03T11:16:25', 291, 165.227, '%getMetaQLQueryResults'('388113aa-50f3-4d1a-9852-87d67dcd2743', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/Health', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6189356)).
log('2020-06-03T11:16:25', 292, 165.23, javaMessageReturned).
log('2020-06-03T11:16:25', 293, 165.231, '%getMetaQLQueryResults'('388113aa-50f3-4d1a-9852-87d67dcd2743', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/Hospitality', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6189414)).
log('2020-06-03T11:16:25', 294, 165.233, javaMessageReturned).
log('2020-06-03T11:16:25', 295, 165.235, '%getMetaQLQueryResults'('388113aa-50f3-4d1a-9852-87d67dcd2743', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/PublicAdministration', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6189472)).
log('2020-06-03T11:16:25', 296, 165.237, javaMessageReturned).
log('2020-06-03T11:16:25', 297, 165.238, '%getMetaQLQueryResults'('388113aa-50f3-4d1a-9852-87d67dcd2743', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/ReligiousServices', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6189530)).
log('2020-06-03T11:16:25', 298, 165.24, javaMessageReturned).
log('2020-06-03T11:16:25', 299, 165.242, '%getMetaQLQueryResults'('388113aa-50f3-4d1a-9852-87d67dcd2743', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/EnergyandMining', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6189588)).
log('2020-06-03T11:16:25', 300, 165.244, javaMessageReturned).
log('2020-06-03T11:16:25', 301, 165.247, '%getMetaQLQueryResults'('388113aa-50f3-4d1a-9852-87d67dcd2743', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/RetailTrade', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6189646)).
log('2020-06-03T11:16:25', 302, 165.249, javaMessageReturned).
log('2020-06-03T11:16:25', 303, 165.251, '%getMetaQLQueryResults'('388113aa-50f3-4d1a-9852-87d67dcd2743', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/Manufacturing', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6189704)).
log('2020-06-03T11:16:25', 304, 165.253, javaMessageReturned).
log('2020-06-03T11:16:25', 305, 165.255, '%getMetaQLQueryResults'('388113aa-50f3-4d1a-9852-87d67dcd2743', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/TransportationandWarehousing', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6189762)).
log('2020-06-03T11:16:25', 306, 165.257, javaMessageReturned).
log('2020-06-03T11:16:25', 307, 165.26, '%getMetaQLQueryResults'('388113aa-50f3-4d1a-9852-87d67dcd2743', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/ProfessionalServices', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6189820)).
log('2020-06-03T11:16:25', 308, 165.262, javaMessageReturned).
log('2020-06-03T11:16:25', 309, 165.264, '%getMetaQLQueryResults'('388113aa-50f3-4d1a-9852-87d67dcd2743', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/Technology', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6189878)).
log('2020-06-03T11:16:25', 310, 165.266, javaMessageReturned).
log('2020-06-03T11:16:25', 311, 165.268, '%getMetaQLQueryResults'('388113aa-50f3-4d1a-9852-87d67dcd2743', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/OtherServices', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6189936)).
log('2020-06-03T11:16:25', 312, 165.271, javaMessageReturned).
log('2020-06-03T11:16:25', 313, 170.984, '%getMetaQLQueryResults'('c591c090-9bbc-47cd-afa5-f3ea3625309c', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/FoodServices', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6016048)).
log('2020-06-03T11:16:25', 314, 170.986, javaMessageReturned).
log('2020-06-03T11:16:25', 315, 170.99, '%getMetaQLQueryResults'('c591c090-9bbc-47cd-afa5-f3ea3625309c', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/Environmental', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6016106)).
log('2020-06-03T11:16:25', 316, 170.992, javaMessageReturned).
log('2020-06-03T11:16:25', 317, 170.994, '%getMetaQLQueryResults'('c591c090-9bbc-47cd-afa5-f3ea3625309c', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/RealEstate', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6016164)).
log('2020-06-03T11:16:25', 318, 170.996, javaMessageReturned).
log('2020-06-03T11:16:25', 319, 170.999, '%getMetaQLQueryResults'('c591c090-9bbc-47cd-afa5-f3ea3625309c', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/Utilities', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6016222)).
log('2020-06-03T11:16:25', 320, 171.002, javaMessageReturned).
log('2020-06-03T11:16:25', 321, 171.004, '%getMetaQLQueryResults'('c591c090-9bbc-47cd-afa5-f3ea3625309c', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/WholesaleTrade', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6016280)).
log('2020-06-03T11:16:25', 322, 171.006, javaMessageReturned).
log('2020-06-03T11:16:25', 323, 171.008, '%getMetaQLQueryResults'('c591c090-9bbc-47cd-afa5-f3ea3625309c', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/PoliticalOrganizations', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6016338)).
log('2020-06-03T11:16:25', 324, 171.01, javaMessageReturned).
log('2020-06-03T11:16:25', 325, 171.012, '%getMetaQLQueryResults'('c591c090-9bbc-47cd-afa5-f3ea3625309c', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/Industry', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6016396)).
log('2020-06-03T11:16:25', 326, 171.014, javaMessageReturned).
log('2020-06-03T11:16:25', 327, 171.016, '%getMetaQLQueryResults'('c591c090-9bbc-47cd-afa5-f3ea3625309c', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/FinancialServices', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6016454)).
log('2020-06-03T11:16:25', 328, 171.028, javaMessageReturned).
log('2020-06-03T11:16:25', 329, 171.054, framesWereParsed).
log('2020-06-03T11:16:25', 330, 171.314, framesWereDeleted(14)).
log('2020-06-03T11:16:25', 331, 174.258, framesWereInserted('\\true')).
log('2020-06-03T11:16:25', 332, 174.621, ruleExiting).
log('2020-06-03T11:16:25', 333, 203.682, '%getMetaQLQueryResults'('c591c090-9bbc-47cd-afa5-f3ea3625309c', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/Agriculture', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6189066)).
log('2020-06-03T11:16:25', 334, 203.684, javaMessageReturned).
log('2020-06-03T11:16:25', 335, 203.686, '%getMetaQLQueryResults'('c591c090-9bbc-47cd-afa5-f3ea3625309c', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/Construction', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6189124)).
log('2020-06-03T11:16:25', 336, 203.688, javaMessageReturned).
log('2020-06-03T11:16:25', 337, 203.689, '%getMetaQLQueryResults'('c591c090-9bbc-47cd-afa5-f3ea3625309c', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/ArtsMediaEntertainment', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6189182)).
log('2020-06-03T11:16:25', 338, 203.691, javaMessageReturned).
log('2020-06-03T11:16:25', 339, 203.694, '%getMetaQLQueryResults'('c591c090-9bbc-47cd-afa5-f3ea3625309c', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/SocialServices', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6189240)).
log('2020-06-03T11:16:25', 340, 203.696, javaMessageReturned).
log('2020-06-03T11:16:25', 341, 203.697, '%getMetaQLQueryResults'('c591c090-9bbc-47cd-afa5-f3ea3625309c', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/Education', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6189298)).
log('2020-06-03T11:16:25', 342, 203.699, javaMessageReturned).
log('2020-06-03T11:16:25', 343, 203.701, '%getMetaQLQueryResults'('c591c090-9bbc-47cd-afa5-f3ea3625309c', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/Health', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6189356)).
log('2020-06-03T11:16:25', 344, 203.703, javaMessageReturned).
log('2020-06-03T11:16:25', 345, 203.704, '%getMetaQLQueryResults'('c591c090-9bbc-47cd-afa5-f3ea3625309c', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/Hospitality', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6189414)).
log('2020-06-03T11:16:25', 346, 203.706, javaMessageReturned).
log('2020-06-03T11:16:25', 347, 203.708, '%getMetaQLQueryResults'('c591c090-9bbc-47cd-afa5-f3ea3625309c', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/PublicAdministration', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6189472)).
log('2020-06-03T11:16:25', 348, 203.71, javaMessageReturned).
log('2020-06-03T11:16:25', 349, 203.712, '%getMetaQLQueryResults'('c591c090-9bbc-47cd-afa5-f3ea3625309c', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/ReligiousServices', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6189530)).
log('2020-06-03T11:16:25', 350, 203.713, javaMessageReturned).
log('2020-06-03T11:16:25', 351, 203.715, '%getMetaQLQueryResults'('c591c090-9bbc-47cd-afa5-f3ea3625309c', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/EnergyandMining', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6189588)).
log('2020-06-03T11:16:25', 352, 203.717, javaMessageReturned).
log('2020-06-03T11:16:25', 353, 203.719, '%getMetaQLQueryResults'('c591c090-9bbc-47cd-afa5-f3ea3625309c', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/RetailTrade', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6189646)).
log('2020-06-03T11:16:25', 354, 203.721, javaMessageReturned).
log('2020-06-03T11:16:25', 355, 203.723, '%getMetaQLQueryResults'('c591c090-9bbc-47cd-afa5-f3ea3625309c', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/Manufacturing', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6189704)).
log('2020-06-03T11:16:25', 356, 203.725, javaMessageReturned).
log('2020-06-03T11:16:25', 357, 203.726, '%getMetaQLQueryResults'('c591c090-9bbc-47cd-afa5-f3ea3625309c', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/TransportationandWarehousing', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6189762)).
log('2020-06-03T11:16:25', 358, 203.729, javaMessageReturned).
log('2020-06-03T11:16:25', 359, 203.73, '%getMetaQLQueryResults'('c591c090-9bbc-47cd-afa5-f3ea3625309c', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/ProfessionalServices', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6189820)).
log('2020-06-03T11:16:25', 360, 203.732, javaMessageReturned).
log('2020-06-03T11:16:25', 361, 203.735, '%getMetaQLQueryResults'('c591c090-9bbc-47cd-afa5-f3ea3625309c', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/Technology', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6189878)).
log('2020-06-03T11:16:25', 362, 203.737, javaMessageReturned).
log('2020-06-03T11:16:25', 363, 203.739, '%getMetaQLQueryResults'('c591c090-9bbc-47cd-afa5-f3ea3625309c', application_mod, 'ai.haley.mind.inference.HaleyMindInferenceCallback', harborappetitegraphquery, [segments/['harbor-directory-data'], 'InsuranceProvider'/'http://vital.ai/haley.ai/harbor-directory/HarborInsuranceProvider/insuranceprovider-100212', 'BusinessCategory'/'http://vital.ai/harbor-ai/HarborBusinessCategory/OtherServices', 'ProductCategory'/'http://vital.ai/harbor-ai/HarborProductCategory/BusinessOwnerPolicy', 'USState'/'http://vital.ai/ontology/harbor-ai#US_State_NewYork'], _h6189936)).
log('2020-06-03T11:16:25', 364, 203.741, javaMessageReturned).

:- dynamic actions/1.
:- multifile actions/1.


printTimes :-
    format("Session\tQueryName\tUpdating\tJavaCall\tParsing\tDeletion\tInsertion~n"),
    callbackTimes(Session,
                  QueryName,
                  Updating,
                  JavaCall,
                  Parsing,
                  Deletion,
                  Insertion),
    format("~w\t~w\t~w\t~w\t~w\t~w\t~w~n",
           [ Session,
             QueryName,
             Updating,
             JavaCall,
             Parsing,
             Deletion,
             Insertion
           ]),
    fail.
printTimes.
% dB(/.../(lps_user_examples, 'myLogGroker.pl'), lps_visualization(_47412{groups:[], items:[]}, [])).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'need insulin.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'need insulin.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/need insulin.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'need insulin.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'need insulin.pl'), lps= /.../(lps_user_examples, 'need insulin.pl'), using= /.../(lps_user_examples, 'need insulin.pl')].
% continue_lps_dialect.
% ops.

% LPS:  fluents((has/2,need/2,diabetic/1,paid/3)).
% Into: fluents([has(_19900,_19902),need(_19912,_19914),diabetic(_19924),paid(_19934,_19936,_19938)]).

% LPS:  events((highBloodSugar/1,obtain/2)).
% Into: events([highBloodSugar(_20976),obtain(_20986,_20988)]).

% LPS:  actions((takeFrom/3,use/2,pay/3)).
% Into: actions([takeFrom(_22286,_22288,_22290),use(_22300,_22302),pay(_22312,_22314,_22316)]).

% LPS:  initially((has(dave,insulin),has(carla,insulin),diabetic(dave))).
% Into: initial_state([has(dave,insulin),has(carla,insulin),diabetic(dave)]).

% LPS:  observe(from(highBloodSugar(bob),to(1,2))).
% Into: observe([highBloodSugar(bob)],2).

% LPS:  if(updates(takeFrom(_24670,_24672,_24674),in(to(_24672,_24670),has(_24672,_24674))),has(_24672,_24674)).
% Into: updated(happens(takeFrom(_24670,_24672,_24674),_26152,_26158),has(_24672,_24674),_24672-_24670,[holds(has(_24672,_24674),_26152)]).

% LPS:  initiates(pay(_26426,_26428,_26430),paid(_26426,_26428,_26430)).
% Into: initiated(happens(pay(_26426,_26428,_26430),_27658,_27664),paid(_26426,_26428,_26430),[]).

% LPS:  then(if(highBloodSugar(_27630)),(obtain(_27630,insulin),use(_27630,insulin))).
% Into: reactive_rule([happens(highBloodSugar(_27630),_28856,_28862)],[happens(obtain(_27630,insulin),_28888,_28930),happens(use(_27630,insulin),_28930,_28894)]).

% LPS:  if(from(obtain(_29226,_29228),to(_29264,_29264)),at(has(_29226,_29228),_29264)).
% Into: l_events(happens(obtain(_29226,_29228),_29264,_29264),[holds(has(_29226,_29228),_29264)]).

% LPS:  if(from(obtain(_30826,_30828),to(_30864,_30866)),(at(has(_30978,_30828),_30864),from(takeFrom(_30826,_30978,_30828),to(_30864,_30866)))).
% Into: l_events(happens(obtain(_30826,_30828),_30864,_30866),[holds(has(_30978,_30828),_30864),happens(takeFrom(_30826,_30978,_30828),_30864,_30866)]).

% LPS:  false((takeFrom(_32556,_32558,_32560),has(_32556,_32560))).
% Into: d_pre([happens(takeFrom(_32556,_32558,_32560),_33698,_33704),holds(has(_32556,_32560),_33698)]).

% LPS:  false((takeFrom(_33754,_33756,_33758),takeFrom(_33754,_33828,_33758),_33756\=_33828)).
% Into: d_pre([happens(takeFrom(_33754,_33756,_33758),_35034,_35040),happens(takeFrom(_33754,_33828,_33758),_35034,_35040),_33756\=_33828]).
% /pack/logicmoo_ec/test/lps_user_examples/need insulin.pl:64
% pop_lps_dialect('$BLOB'("<stream>(0x562ef8559400)"),  (/.../(lps_user_examples, 'need insulin.pl')-> /.../(lps_user_examples, 'need insulin.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/need insulin.pl':_42984).


initiated(happens(pay(A, B, C), _, _), paid(A, B, C), []).

d_pre([happens(takeFrom(A, _, B), C, _), holds(has(A, B), C)]).
d_pre([happens(takeFrom(A, B, C), D, E), happens(takeFrom(A, F, C), D, E), B\=F]).

fluents([has(_, _), need(_, _), diabetic(_), paid(_, _, _)]).

reactive_rule([happens(highBloodSugar(A), _, _)], [happens(obtain(A, insulin), _, B), happens(use(A, insulin), B, _)]).

initial_state([has(dave, insulin), has(carla, insulin), diabetic(dave)]).

l_events(happens(obtain(A, B), C, C), [holds(has(A, B), C)]).
l_events(happens(obtain(A, B), C, D), [holds(has(E, B), C), happens(takeFrom(A, E, B), C, D)]).

:- dynamic actions/1.
:- multifile actions/1.

actions([takeFrom(_, _, _), use(_, _), pay(_, _, _)]).

events([highBloodSugar(_), obtain(_, _)]).

observe([highBloodSugar(bob)], 2).

updated(happens(takeFrom(A, B, C), D, _), has(B, C), B-A, [holds(has(B, C), D)]).
% dB(/.../(lps_user_examples, 'need insulin.pl'), lps_visualization(_68078{groups:[_66950{content:"Events", id:"event", order:1}, _67024{content:"diabetic(A)", id:"diabetic/1", order:3, subgroupStack:"false"}, _67102{content:"has(A,B)", id:"has/2", order:3, subgroupStack:"false"}, _67168{content:"Actions", id:"action", order:4}], items:[_67290{content:"dave", end:21, group:"diabetic/1", id:0, start:1, subgroup:"dave", title:"Fluent diabetic(dave) initiated at 1<br/>and terminated at transition to 21"}, _67416{content:"bob,insulin", end:21, group:"has/2", id:1, start:3, subgroup:"bob", title:"Fluent has(bob,insulin) initiated at 3<br/>and terminated at transition to 21"}, _67542{content:"carla,insulin", end:21, group:"has/2", id:2, start:1, subgroup:"carla", title:"Fluent has(carla,insulin) initiated at 1<br/>and terminated at transition to 21"}, _67668{content:"dave,insulin", end:3, group:"has/2", id:3, start:1, subgroup:"dave", title:"Fluent has(dave,insulin) initiated at 1<br/>and terminated at transition to 3"}, _67794{content:"highBloodSugar(bob)", group:"event", id:4, start:2, style:"color:#E19735", title:"happens(highBloodSugar(bob),1,2)", type:"point"}, _67920{content:"takeFrom(bob,dave,insulin)", group:"action", id:5, start:3, style:"color:green", title:"happens(takeFrom(bob,dave,insulin),2,3)", type:"point"}, _68046{content:"use(bob,insulin)", group:"action", id:6, start:4, style:"color:green", title:"happens(use(bob,insulin),3,4)", type:"point"}]}, [])).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'new ballot.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'new ballot.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/new ballot.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'new ballot.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'new ballot.pl'), lps= /.../(lps_user_examples, 'new ballot.pl'), using= /.../(lps_user_examples, 'new ballot.pl')].
% continue_lps_dialect.
% ops.

% LPS:  events((delegate(_55678,_55680),vote(_55734,_55736))).
% Into: events([delegate(_55678,_55680),vote(_55734,_55736)]).

% LPS:  fluents((voter(_56904,_56906),voted(_56904,_56962),delegateOf(_56904,_57018),voteCount(_56962,_57074))).
% Into: fluents([voter(_56904,_56906),voted(_56904,_56962),delegateOf(_56904,_57018),voteCount(_56962,_57074)]).

% LPS:  initially((voter(bob,1),voter(fariba,1),voter(jacinto,1),voter(miguel,1))).
% Into: initial_state([voter(bob,1),voter(fariba,1),voter(jacinto,1),voter(miguel,1)]).

% LPS:  initially((voteCount(trump,0),voteCount(clinton,0))).
% Into: initial_state([voteCount(trump,0),voteCount(clinton,0)]).

% LPS:  observe(from(delegate(bob,miguel),to(4,5))).
% Into: observe([delegate(bob,miguel)],5).

% LPS:  observe(from(vote(miguel,clinton),to(5,6))).
% Into: observe([vote(miguel,clinton)],6).

% LPS:  observe(from(vote(jacinto,clinton),to(5,6))).
% Into: observe([vote(jacinto,clinton)],6).

% LPS:  observe(from(delegate(fariba,miguel),to(4,5))).
% Into: observe([delegate(fariba,miguel)],5).

% LPS:  updates(delegate(_65564,_65566),in(to(_65602,0),voter(_65564,_65602))).
% Into: updated(happens(delegate(_65564,_65566),_66914,_66920),voter(_65564,_65602),_65602-0,[]).

% LPS:  if(updates(delegate(_66816,_66818),in(to(_66854,_66856),voter(_66936,_66854))),(delegateOf(_66818,_66936),voter(_66816,_67114),_66856 is _67114+_66854)).
% Into: updated(happens(delegate(_66816,_66818),_68582,_68588),voter(_66936,_66854),_66854-_66856,[holds(delegateOf(_66818,_66936),_68582),holds(voter(_66816,_67114),_68582),_66856 is _67114+_66854]).

% LPS:  if(updates(delegate(_69436,_69438),in(to(_69474,_69476),voteCount(_69556,_69474))),(delegateOf(_69438,_69678),voted(_69678,_69556),voter(_69436,_69790),_69476 is _69474+_69790)).
% Into: updated(happens(delegate(_69436,_69438),_71314,_71320),voteCount(_69556,_69474),_69474-_69476,[holds(delegateOf(_69438,_69678),_71314),holds(voted(_69678,_69556),_71314),holds(voter(_69436,_69790),_71314),_69476 is _69474+_69790]).

% LPS:  initiates(delegate(_72322,_72324),voted(_72322,delegated(_72324))).
% Into: initiated(happens(delegate(_72322,_72324),_73536,_73542),voted(_72322,delegated(_72324)),[]).

% LPS:  if(delegateOf(_73554,_73556),(voted(_73554,delegated(_73630)),delegateOf(_73630,_73556))).
% Into: l_int(holds(delegateOf(_73554,_73556),_74794),[holds(voted(_73554,delegated(_73630)),_74794),holds(delegateOf(_73630,_73556),_74794)]).

% LPS:  if(delegateOf(_75406,_75406),not(voted(_75406,delegated(_75490)))).
% Into: l_int(holds(delegateOf(_75406,_75406),_76558),[holds(not(voted(_75406,delegated(_75490))),_76558)]).

% LPS:  false((delegate(_76972,_76974),voted(_76972,_77030))).
% Into: d_pre([happens(delegate(_76972,_76974),_78086,_78092),holds(voted(_76972,_77030),_78086)]).

% LPS:  false((delegate(_19250,_19252),_19250==_19252)).
% Into: d_pre([happens(delegate(_19250,_19252),_19592,_19598),_19250==_19252]).

% LPS:  false((delegate(_19900,_19902),delegate(_19900,_19958),_19902\=_19958)).
% Into: d_pre([happens(delegate(_19900,_19902),_21146,_21152),happens(delegate(_19900,_19958),_21146,_21152),_19902\=_19958]).

% LPS:  false((delegate(_21554,_21556),delegateOf(_21556,_21554))).
% Into: d_pre([happens(delegate(_21554,_21556),_22680,_22686),holds(delegateOf(_21556,_21554),_22680)]).

% LPS:  initiates(vote(_22980,_22982),voted(_22980,_22982)).
% Into: initiated(happens(vote(_22980,_22982),_24166,_24172),voted(_22980,_22982),[]).

% LPS:  if(updates(vote(_24112,_24114),in(to(_24150,_24152),voteCount(_24114,_24150))),(voter(_24112,_24354),_24152 is _24150+_24354)).
% Into: updated(happens(vote(_24112,_24114),_25766,_25772),voteCount(_24114,_24150),_24150-_24152,[holds(voter(_24112,_24354),_25766),_24152 is _24150+_24354]).

% LPS:  false((vote(_26238,_26240),voted(_26238,_26296))).
% Into: d_pre([happens(vote(_26238,_26240),_27352,_27358),holds(voted(_26238,_26296),_27352)]).

% LPS:  false((vote(_27660,_27662),vote(_27660,_27718),_27662\=_27718)).
% Into: d_pre([happens(vote(_27660,_27662),_28906,_28912),happens(vote(_27660,_27718),_28906,_28912),_27662\=_27718]).
% /pack/logicmoo_ec/test/lps_user_examples/new ballot.pl:74
% pop_lps_dialect('$BLOB'("<stream>(0x562ef86f0b00)"),  (/.../(lps_user_examples, 'new ballot.pl')-> /.../(lps_user_examples, 'new ballot.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/new ballot.pl':_36974).


initiated(happens(delegate(A, B), _, _), voted(A, delegated(B)), []).
initiated(happens(vote(A, B), _, _), voted(A, B), []).

d_pre([happens(delegate(A, _), B, _), holds(voted(A, _), B)]).
d_pre([happens(delegate(A, B), _, _), A==B]).
d_pre([happens(delegate(A, B), C, D), happens(delegate(A, E), C, D), B\=E]).
d_pre([happens(delegate(A, B), C, _), holds(delegateOf(B, A), C)]).
d_pre([happens(vote(A, _), B, _), holds(voted(A, _), B)]).
d_pre([happens(vote(A, B), C, D), happens(vote(A, E), C, D), B\=E]).

fluents([voter(A, _), voted(A, B), delegateOf(A, _), voteCount(B, _)]).

l_int(holds(delegateOf(A, B), C), [holds(voted(A, delegated(D)), C), holds(delegateOf(D, B), C)]).
l_int(holds(delegateOf(A, A), B), [holds(not(voted(A, delegated(_))), B)]).

initial_state([voter(bob, 1), voter(fariba, 1), voter(jacinto, 1), voter(miguel, 1)]).
initial_state([voteCount(trump, 0), voteCount(clinton, 0)]).

:- dynamic actions/1.
:- multifile actions/1.


events([delegate(_, _), vote(_, _)]).

observe([delegate(bob, miguel)], 5).
observe([vote(miguel, clinton)], 6).
observe([vote(jacinto, clinton)], 6).
observe([delegate(fariba, miguel)], 5).

maxTime(15).

updated(happens(delegate(A, _), _, _), voter(A, B), B-0, []).
updated(happens(delegate(A, B), C, _), voter(D, E), E-F, [holds(delegateOf(B, D), C), holds(voter(A, G), C), F is G+E]).
updated(happens(delegate(A, B), C, _), voteCount(D, E), E-F, [holds(delegateOf(B, G), C), holds(voted(G, D), C), holds(voter(A, H), C), F is E+H]).
updated(happens(vote(A, B), C, _), voteCount(B, D), D-E, [holds(voter(A, F), C), E is D+F]).
Warning: Rejected observations [delegate(bob,miguel),delegate(fariba,miguel)] attempting to satisfy false preconditions [happens(delegate(bob,miguel),4,5),happens(delegate(fariba,miguel),4,5),bob\=fariba]
Warning: Rejected observations [vote(miguel,clinton),vote(jacinto,clinton)] attempting to satisfy false preconditions [happens(vote(miguel,clinton),5,6),happens(vote(jacinto,clinton),5,6),miguel\=jacinto]
% dB(/.../(lps_user_examples, 'new ballot.pl'), lps_visualization(_81092{groups:[_80226{content:"voteCount(A,B)", id:"voteCount/2", order:3, subgroupStack:"false"}, _80304{content:"voter(A,B)", id:"voter/2", order:3, subgroupStack:"false"}], items:[_80430{content:"clinton,0", end:16, group:"voteCount/2", id:0, start:1, subgroup:"clinton", title:"Fluent voteCount(clinton,0) initiated at 1<br/>and terminated at transition to 16"}, _80556{content:"trump,0", end:16, group:"voteCount/2", id:1, start:1, subgroup:"trump", title:"Fluent voteCount(trump,0) initiated at 1<br/>and terminated at transition to 16"}, _80682{content:"bob,1", end:16, group:"voter/2", id:2, start:1, subgroup:"bob", title:"Fluent voter(bob,1) initiated at 1<br/>and terminated at transition to 16"}, _80808{content:"fariba,1", end:16, group:"voter/2", id:3, start:1, subgroup:"fariba", title:"Fluent voter(fariba,1) initiated at 1<br/>and terminated at transition to 16"}, _80934{content:"jacinto,1", end:16, group:"voter/2", id:4, start:1, subgroup:"jacinto", title:"Fluent voter(jacinto,1) initiated at 1<br/>and terminated at transition to 16"}, _81060{content:"miguel,1", end:16, group:"voter/2", id:5, start:1, subgroup:"miguel", title:"Fluent voter(miguel,1) initiated at 1<br/>and terminated at transition to 16"}]}, [])).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'new goat.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'new goat.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/new goat.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'new goat.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'new goat.pl'), lps= /.../(lps_user_examples, 'new goat.pl'), using= /.../(lps_user_examples, 'new goat.pl')].
% continue_lps_dialect.
% ops.

% LPS:  actions(transport(_55362,_55364,_55366)).
% Into: actions([transport(_55362,_55364,_55366)]).

% LPS:  fluents(loc(_56422,_56424)).
% Into: fluents([loc(_56422,_56424)]).

% LPS:  initially((loc(wolf,south),loc(goat,south),loc(cabbage,south),loc(farmer,south))).
% Into: initial_state([loc(wolf,south),loc(goat,south),loc(cabbage,south),loc(farmer,south)]).

% LPS:  then(if((loc(_58894,south),_58894\=farmer)),from(makeLoc(_58894,north),to(_59108,_59110))).
% Into: reactive_rule([holds(loc(_58894,south),_60264),_58894\=farmer],[happens(makeLoc(_58894,north),_59108,_59110)]).

% LPS:  if(from(makeLoc(_61270,north),to(_61308,_61310)),(_61270\=farmer,from(makeLoc(farmer,south),to(_61308,_61526)),at(loc(_61270,south),_61526),from(transport(_61270,south,north),to(_61526,_61310)))).
% Into: l_events(happens(makeLoc(_61270,north),_61308,_61310),[_61270\=farmer,happens(makeLoc(farmer,south),_61308,_61526),holds(loc(_61270,south),_61526),happens(transport(_61270,south,north),_61526,_61310)]).

% LPS:  if(from(makeLoc(farmer,_63978),to(_64014,_64014)),at(loc(farmer,_63978),_64014)).
% Into: l_events(happens(makeLoc(farmer,_63978),_64014,_64014),[holds(loc(farmer,_63978),_64014)]).

% LPS:  if(from(makeLoc(farmer,_65306),to(_65342,_65344)),(at(loc(farmer,_65458),_65342),_65306\=_65458,at(loc(_65624,_65458),_65342),_65624\=farmer,from(transport(_65624,_65458,_65306),to(_65342,_65344)))).
% Into: l_events(happens(makeLoc(farmer,_65306),_65342,_65344),[holds(loc(farmer,_65458),_65342),_65306\=_65458,holds(loc(_65624,_65458),_65342),_65624\=farmer,happens(transport(_65624,_65458,_65306),_65342,_65344)]).

% LPS:  if(from(makeLoc(farmer,_68026),to(_68062,_68064)),(at(loc(farmer,_68178),_68062),_68026\=_68178,from(transport(farmer,_68178,_68026),to(_68062,_68064)))).
% Into: l_events(happens(makeLoc(farmer,_68026),_68062,_68064),[holds(loc(farmer,_68178),_68062),_68026\=_68178,happens(transport(farmer,_68178,_68026),_68062,_68064)]).

% LPS:  updates(transport(_70130,_70132,_70134),in(to(_70132,_70134),loc(_70130,_70132))).
% Into: updated(happens(transport(_70130,_70132,_70134),_71486,_71492),loc(_70130,_70132),_70132-_70134,[]).

% LPS:  updates(transport(_71400,_71402,_71404),in(to(_71402,_71404),loc(farmer,_71402))).
% Into: updated(happens(transport(_71400,_71402,_71404),_72756,_72762),loc(farmer,_71402),_71402-_71404,[]).

% LPS:  false((transport(_72678,_72680,_72682),transport(_72750,_72680,_72682),_72678\=_72750)).
% Into: d_pre([happens(transport(_72678,_72680,_72682),_73958,_73964),happens(transport(_72750,_72680,_72682),_73958,_73964),_72678\=_72750]).

% LPS:  false((loc(goat,_74372),loc(wolf,_74372),not(loc(farmer,_74372)))).
% Into: d_pre([holds(loc(goat,_74372),_75620),holds(loc(wolf,_74372),_75620),holds(not(loc(farmer,_74372)),_75620)]).

% LPS:  false((loc(goat,_75664),loc(cabbage,_75664),not(loc(farmer,_75664)))).
% Into: d_pre([holds(loc(goat,_75664),_76912),holds(loc(cabbage,_75664),_76912),holds(not(loc(farmer,_75664)),_76912)]).
% /pack/logicmoo_ec/test/lps_user_examples/new goat.pl:73
% pop_lps_dialect('$BLOB'("<stream>(0x562ef6d1d400)"),  (/.../(lps_user_examples, 'new goat.pl')-> /.../(lps_user_examples, 'new goat.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/new goat.pl':_84566).


d_pre([happens(transport(A, B, C), D, E), happens(transport(F, B, C), D, E), A\=F]).
d_pre([holds(loc(goat, A), B), holds(loc(wolf, A), B), holds(not(loc(farmer, A)), B)]).
d_pre([holds(loc(goat, A), B), holds(loc(cabbage, A), B), holds(not(loc(farmer, A)), B)]).

fluents([loc(_, _)]).

reactive_rule([holds(loc(A, south), _), A\=farmer], [happens(makeLoc(A, north), _, _)]).

initial_state([loc(wolf, south), loc(goat, south), loc(cabbage, south), loc(farmer, south)]).

l_events(happens(makeLoc(A, north), B, C), [A\=farmer, happens(makeLoc(farmer, south), B, D), holds(loc(A, south), D), happens(transport(A, south, north), D, C)]).
l_events(happens(makeLoc(farmer, A), B, B), [holds(loc(farmer, A), B)]).
l_events(happens(makeLoc(farmer, A), B, C), [holds(loc(farmer, D), B), A\=D, holds(loc(E, D), B), E\=farmer, happens(transport(E, D, A), B, C)]).
l_events(happens(makeLoc(farmer, A), B, C), [holds(loc(farmer, D), B), A\=D, happens(transport(farmer, D, A), B, C)]).

:- dynamic actions/1.
:- multifile actions/1.

actions([transport(_, _, _)]).

updated(happens(transport(A, B, C), _, _), loc(A, B), B-C, []).
updated(happens(transport(_, A, B), _, _), loc(farmer, A), A-B, []).

maxTime(10).
% dB(/.../(lps_user_examples, 'new goat.pl'), lps_visualization(_78646{groups:[_75654{content:"loc(A,B)", id:"loc/2", order:3, subgroupStack:"false"}, _75720{content:"Actions", id:"action", order:4}], items:[_75842{content:"cabbage,north", end:11, group:"loc/2", id:0, start:6, subgroup:"cabbage", title:"Fluent loc(cabbage,north) initiated at 6<br/>and terminated at transition to 11"}, _75968{content:"cabbage,south", end:6, group:"loc/2", id:1, start:1, subgroup:"cabbage", title:"Fluent loc(cabbage,south) initiated at 1<br/>and terminated at transition to 6"}, _76094{content:"farmer,north", end:3, group:"loc/2", id:2, start:2, subgroup:"farmer", title:"Fluent loc(farmer,north) initiated at 2<br/>and terminated at transition to 3"}, _76220{content:"farmer,north", end:5, group:"loc/2", id:3, start:4, subgroup:"farmer", title:"Fluent loc(farmer,north) initiated at 4<br/>and terminated at transition to 5"}, _76346{content:"farmer,north", end:7, group:"loc/2", id:4, start:6, subgroup:"farmer", title:"Fluent loc(farmer,north) initiated at 6<br/>and terminated at transition to 7"}, _76472{content:"farmer,north", end:11, group:"loc/2", id:5, start:8, subgroup:"farmer", title:"Fluent loc(farmer,north) initiated at 8<br/>and terminated at transition to 11"}, _76598{content:"farmer,south", end:2, group:"loc/2", id:6, start:1, subgroup:"farmer", title:"Fluent loc(farmer,south) initiated at 1<br/>and terminated at transition to 2"}, _76724{content:"farmer,south", end:4, group:"loc/2", id:7, start:3, subgroup:"farmer", title:"Fluent loc(farmer,south) initiated at 3<br/>and terminated at transition to 4"}, _76850{content:"farmer,south", end:6, group:"loc/2", id:8, start:5, subgroup:"farmer", title:"Fluent loc(farmer,south) initiated at 5<br/>and terminated at transition to 6"}, _76976{content:"farmer,south", end:8, group:"loc/2", id:9, start:7, subgroup:"farmer", title:"Fluent loc(farmer,south) initiated at 7<br/>and terminated at transition to 8"}, _77102{content:"goat,north", end:5, group:"loc/2", id:10, start:2, subgroup:"goat", title:"Fluent loc(goat,north) initiated at 2<br/>and terminated at transition to 5"}, _77228{content:"goat,north", end:11, group:"loc/2", id:11, start:8, subgroup:"goat", title:"Fluent loc(goat,north) initiated at 8<br/>and terminated at transition to 11"}, _77354{content:"goat,south", end:2, group:"loc/2", id:12, start:1, subgroup:"goat", title:"Fluent loc(goat,south) initiated at 1<br/>and terminated at transition to 2"}, _77480{content:"goat,south", end:8, group:"loc/2", id:13, start:5, subgroup:"goat", title:"Fluent loc(goat,south) initiated at 5<br/>and terminated at transition to 8"}, _77606{content:"wolf,north", end:11, group:"loc/2", id:14, start:4, subgroup:"wolf", title:"Fluent loc(wolf,north) initiated at 4<br/>and terminated at transition to 11"}, _77732{content:"wolf,south", end:4, group:"loc/2", id:15, start:1, subgroup:"wolf", title:"Fluent loc(wolf,south) initiated at 1<br/>and terminated at transition to 4"}, _77858{content:"transport(goat,south,north)", group:"action", id:16, start:2, style:"color:green", title:"happens(transport(goat,south,north),1,2)", type:"point"}, _77984{content:"transport(farmer,north,south)", group:"action", id:17, start:3, style:"color:green", title:"happens(transport(farmer,north,south),2,3)", type:"point"}, _78110{content:"transport(wolf,south,north)", group:"action", id:18, start:4, style:"color:green", title:"happens(transport(wolf,south,north),3,4)", type:"point"}, _78236{content:"transport(goat,north,south)", group:"action", id:19, start:5, style:"color:green", title:"happens(transport(goat,north,south),4,5)", type:"point"}, _78362{content:"transport(cabbage,south,north)", group:"action", id:20, start:6, style:"color:green", title:"happens(transport(cabbage,south,north),5,6)", type:"point"}, _78488{content:"transport(farmer,north,south)", group:"action", id:21, start:7, style:"color:green", title:"happens(transport(farmer,north,south),6,7)", type:"point"}, _78614{content:"transport(goat,south,north)", group:"action", id:22, start:8, style:"color:green", title:"happens(transport(goat,south,north),7,8)", type:"point"}]}, [])).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'New Tezos.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'New Tezos.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/New Tezos.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'New Tezos.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'New Tezos.pl'), lps= /.../(lps_user_examples, 'New Tezos.pl'), using= /.../(lps_user_examples, 'New Tezos.pl')].
% continue_lps_dialect.
% ops.

% LPS:  events((play(_59468,_59470,_59472),addfunds(_59526,_59528),observeRandom(_59470))).
% Into: events([play(_59468,_59470,_59472),addfunds(_59526,_59528),observeRandom(_59470)]).

% LPS:  fluents((game(_60772,_60774,_60776),balance(_60816))).
% Into: fluents([game(_60772,_60774,_60776),balance(_60816)]).

% LPS:  actions((failwith(_61950,_61952),transfer(_62006,_62008),send(_62062,_62064))).
% Into: actions([failwith(_61950,_61952),transfer(_62006,_62008),send(_62062,_62064)]).

% LPS:  if(updates(transfer(_63324,_63326),in(to(_63362,_63364),balance(_63362))),_63364 is _63362-_63326).
% Into: updated(happens(transfer(_63324,_63326),_64842,_64848),balance(_63362),_63362-_63364,[_63364 is _63362-_63326]).

% LPS:  if(updates(addfunds(_65282,_65284),in(to(_65320,_65322),balance(_65320))),_65322 is _65320+_65284).
% Into: updated(happens(addfunds(_65282,_65284),_66804,_66810),balance(_65320),_65320-_65322,[_65322 is _65320+_65284]).

% LPS:  initially(balance(100)).
% Into: initial_state([balance(100)]).

% LPS:  observe(from(play(miguel,52,90),to(1,2))).
% Into: observe([play(miguel,52,90)],2).

% LPS:  observe(from(observeRandom(51),to(2,3))).
% Into: observe([observeRandom(51)],3).

% LPS:  if(at(playFails(_72248,_72250,_72252,'number must be <= 100'),_72276),at(_72250>100,_72276)).
% Into: l_int(holds(playFails(_72248,_72250,_72252,'number must be <= 100'),_72276),[holds(_72250>100,_72276)]).

% LPS:  if(at(playFails(_74304,_74306,_74308,'I do not have enough money for this bet'),_74332),(at(balance(_74398),_74332),_74308>_74398)).
% Into: l_int(holds(playFails(_74304,_74306,_74308,'I do not have enough money for this bet'),_74332),[holds(balance(_74398),_74332),holds(_74308>_74398,_76132)]).

% LPS:  if(at(playFails(_76436,_76438,_76440,'Game already started with g????'),_76464),at(game(_76558,_76560,_76562),_76464)).
% Into: l_int(holds(playFails(_76436,_76438,_76440,'Game already started with g????'),_76464),[holds(game(_76558,_76560,_76562),_76464)]).

% LPS:  then(if((from(play(_78224,_78226,_78228),to(_78264,_78266)),at(playFails(_78224,_78226,_78228,_78412),_78264))),from(failwith(_78224,_78412),_78266)).
% Into: reactive_rule([happens(play(_78224,_78226,_78228),_78264,_78266),holds(playFails(_78224,_78226,_78228,_78412),_78264)],[happens(failwith(_78224,_78412),_78266,_80346)]).

% LPS:  if(initiates(play(_80392,_80394,_80396),game(_80392,_80394,_80396)),not(playFails(_80392,_80394,_80396,_80596))).
% Into: initiated(happens(play(_80392,_80394,_80396),_81818,_81824),game(_80392,_80394,_80396),[holds(not(playFails(_80392,_80394,_80396,_80596)),_81818)]).

% LPS:  if(updates(play(_82124,_82126,_82128),in(to(_82164,_82166),balance(_82164))),(not(playFails(_82124,_82126,_82128,_82392)),_82166 is _82164+_82128)).
% Into: updated(happens(play(_82124,_82126,_82128),_83848,_83854),balance(_82164),_82164-_82166,[holds(not(playFails(_82124,_82126,_82128,_82392)),_83848),_82166 is _82164+_82128]).

% LPS:  if(at(oracleFails(_19238,'No game already started'),_19252),at(not(game(_19328,_19330,_19332)),_19252)).
% Into: l_int(holds(oracleFails(_19238,'No game already started'),_19252),[holds(not(game(_19328,_19330,_19332)),_19252)]).

% LPS:  if(at(oracleFails(_19712,'Random numbers can not be generated'),_19736),(oracle(_19802),_19712\=_19802)).
% Into: l_int(holds(oracleFails(_19712,'Random numbers can not be generated'),_19736),[oracle(_19802),_19712\=_19802]).

% LPS:  then(if((to(send(_21686,_21688),_21710),at(oracleFails(_21686,_21792),_21710),game(_21908,_21910,_21912))),from(failwith(_21908,_21792),_21710)).
% Into: reactive_rule([happens(send(_21686,_21688),_23268,_21710),holds(oracleFails(_21686,_21792),_21710),holds(game(_21908,_21910,_21912),_23274)],[happens(failwith(_21908,_21792),_21710,_24290)]).

% LPS:  then(if((to(send(_24432,_24434),_24456),at(not(oracleFails(_24432,_24546)),_24456),at(game(_24686,_24688,_24690),_24456),at(balance(_24778),_24456),_24688>_24434)),terminate(from(game(_24686,_24688,_24690),_24456))).
% Into: reactive_rule([happens(send(_24432,_24434),_26402,_24456),holds(not(oracleFails(_24432,_24546)),_24456),holds(game(_24686,_24688,_24690),_24456),holds(balance(_24778),_24456),holds(_24688>_24434,_26408)],[happens(terminate(game(_24686,_24688,_24690)),_24456,_28362)]).

% LPS:  then(if((to(send(_28504,_28506),_28528),at(not(oracleFails(_28504,_28618)),_28528),at(game(_28758,_28760,_28762),_28528),at(balance(_28850),_28528),_28760=<_28506,_29152 is _28762+_28762*_28760/100)),(from(transfer(_28758,_29152),_29442),terminate(from(game(_28758,_28760,_28762),_28528)))).
% Into: reactive_rule([happens(send(_28504,_28506),_30932,_28528),holds(not(oracleFails(_28504,_28618)),_28528),holds(game(_28758,_28760,_28762),_28528),holds(balance(_28850),_28528),_28760=<_28506,_29152 is _28762+_28762*_28760/100],[happens(transfer(_28758,_29152),_29442,_33354),happens(terminate(game(_28758,_28760,_28762)),_28528,_33318)]).

% LPS:  then(if((to(play(_33514,_33516,_33518),_33540),at(game(_33514,_33516,_33518),_33540),from(observeRandom(_33726),to(_33540,_33764)))),from(send(myaddress,_33726),_33764)).
% Into: reactive_rule([happens(play(_33514,_33516,_33518),_35178,_33540),holds(game(_33514,_33516,_33518),_33540),happens(observeRandom(_33726),_33540,_33764)],[happens(send(myaddress,_33726),_33764,_35870)]).
% /pack/logicmoo_ec/test/lps_user_examples/New Tezos.pl:103
% pop_lps_dialect('$BLOB'("<stream>(0x562ef85cbd00)"),  (/.../(lps_user_examples, 'New Tezos.pl')-> /.../(lps_user_examples, 'New Tezos.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/New Tezos.pl':_43408).


initiated(happens(play(A, B, C), D, _), game(A, B, C), [holds(not(playFails(A, B, C, _)), D)]).

fluents([game(_, _, _), balance(_)]).

l_int(holds(playFails(_, A, _, 'number must be <= 100'), B), [holds(A>100, B)]).
l_int(holds(playFails(_, _, A, 'I do not have enough money for this bet'), B), [holds(balance(C), B), holds(A>C, _)]).
l_int(holds(playFails(_, _, _, 'Game already started with g????'), A), [holds(game(_, _, _), A)]).
l_int(holds(oracleFails(_, 'No game already started'), A), [holds(not(game(_, _, _)), A)]).
l_int(holds(oracleFails(A, 'Random numbers can not be generated'), _), [oracle(B), A\=B]).

reactive_rule([happens(play(A, B, C), D, E), holds(playFails(A, B, C, F), D)], [happens(failwith(A, F), E, _)]).
reactive_rule([happens(send(A, _), _, B), holds(oracleFails(A, C), B), holds(game(D, _, _), _)], [happens(failwith(D, C), B, _)]).
reactive_rule([happens(send(A, B), _, C), holds(not(oracleFails(A, _)), C), holds(game(D, E, F), C), holds(balance(_), C), holds(E>B, _)], [happens(terminate(game(D, E, F)), C, _)]).
reactive_rule([happens(send(A, B), _, C), holds(not(oracleFails(A, _)), C), holds(game(D, E, F), C), holds(balance(_), C), E=<B, G is F+F*E/100], [happens(transfer(D, G), _, _), happens(terminate(game(D, E, F)), C, _)]).
reactive_rule([happens(play(A, B, C), _, D), holds(game(A, B, C), D), happens(observeRandom(E), D, F)], [happens(send(myaddress, E), F, _)]).

initial_state([balance(100)]).

:- dynamic actions/1.
:- multifile actions/1.

actions([failwith(_, _), transfer(_, _), send(_, _)]).

events([play(_, A, _), addfunds(_, _), observeRandom(A)]).

updated(happens(transfer(_, A), _, _), balance(B), B-C, [C is B-A]).
updated(happens(addfunds(_, A), _, _), balance(B), B-C, [C is B+A]).
updated(happens(play(A, B, C), D, _), balance(E), E-F, [holds(not(playFails(A, B, C, _)), D), F is E+C]).

maxTime(10).

oracle(myaddress).

observe([play(miguel, 52, 90)], 2).
observe([observeRandom(51)], 3).
PROGRAM FAILED
% dB(/.../(lps_user_examples, 'New Tezos.pl'), lps_visualization(_70196{groups:[_69068{content:"Events", id:"event", order:1}, _69142{content:"balance(A)", id:"balance/1", order:3, subgroupStack:"false"}, _69220{content:"game(A,B,C)", id:"game/3", order:3, subgroupStack:"false"}, _69286{content:"Actions", id:"action", order:4}], items:[_69408{content:"100", end:2, group:"balance/1", id:0, start:1, subgroup:"100", title:"Fluent balance(100) initiated at 1<br/>and terminated at transition to 2"}, _69534{content:"190", end:6, group:"balance/1", id:1, start:2, subgroup:"190", title:"Fluent balance(190) initiated at 2<br/>and terminated at transition to 6"}, _69660{content:"miguel,52,90", end:5, group:"game/3", id:2, start:2, subgroup:"miguel", title:"Fluent game(miguel,52,90) initiated at 2<br/>and terminated at transition to 5"}, _69786{content:"play(miguel,52,90)", group:"event", id:3, start:2, style:"color:#E19735", title:"happens(play(miguel,52,90),1,2)", type:"point"}, _69912{content:"observeRandom(51)", group:"event", id:4, start:3, style:"color:#E19735", title:"happens(observeRandom(51),2,3)", type:"point"}, _70038{content:"send(myaddress,51)", group:"action", id:5, start:4, style:"color:green", title:"happens(send(myaddress,51),3,4)", type:"point"}, _70164{content:"terminate(game(miguel,52,90))", group:"action", id:6, start:5, style:"color:green", title:"happens(terminate(game(miguel,52,90)),4,5)", type:"point"}]}, [])).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'NormalOutcomeOfJohnDTractorInsuranceContract.pl .pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'NormalOutcomeOfJohnDTractorInsuranceContract.pl .pl')).
% /pack/logicmoo_ec/test/lps_user_examples/NormalOutcomeOfJohnDTractorInsuranceContract.pl .pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'NormalOutcomeOfJohnDTractorInsuranceContract.pl .pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'NormalOutcomeOfJohnDTractorInsuranceContract.pl .pl'), lps= /.../(lps_user_examples, 'NormalOutcomeOfJohnDTractorInsuranceContract.pl .pl'), using= /.../(lps_user_examples, 'NormalOutcomeOfJohnDTractorInsuranceContract.pl .pl')].
% continue_lps_dialect.
% ops.
% /pack/logicmoo_ec/test/lps_user_examples/NormalOutcomeOfJohnDTractorInsuranceContract.pl .pl:3
% pop_lps_dialect('$BLOB'("<stream>(0x562ef89a0700)"),  (/.../(lps_user_examples, 'NormalOutcomeOfJohnDTractorInsuranceContract.pl .pl')-> /.../(lps_user_examples, 'NormalOutcomeOfJohnDTractorInsuranceContract.pl .pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/NormalOutcomeOfJohnDTractorInsuranceContract.pl .pl':_62582).


:- dynamic actions/1.
:- multifile actions/1.

% dB(/.../(lps_user_examples, 'NormalOutcomeOfJohnDTractorInsuranceContract.pl .pl'), lps_visualization(_44312{groups:[], items:[]}, [])).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'P6 Claudio Condor.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'P6 Claudio Condor.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/P6 Claudio Condor.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'P6 Claudio Condor.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'P6 Claudio Condor.pl'), lps= /.../(lps_user_examples, 'P6 Claudio Condor.pl'), using= /.../(lps_user_examples, 'P6 Claudio Condor.pl')].
% continue_lps_dialect.
% ops.

% LPS:  fluents((varita_obedece(_19446),vivo(_19446))).
% Into: fluents([varita_obedece(_19446),vivo(_19446)]).

% LPS:  actions((desarmo/2,cortar_cabeza/2,destruye_varita/1)).
% Into: actions([desarmo(_21922,_21924),cortar_cabeza(_21934,_21936),destruye_varita(_21946)]).

% LPS:  events((batalla/1,derroto/2)).
% Into: events([batalla(_23040),derroto(_23050,_23052)]).

% LPS:  initially((varita_obedece(dumbledore),vivo(voldemort),vivo(nagini))).
% Into: initial_state([varita_obedece(dumbledore),vivo(voldemort),vivo(nagini)]).

% LPS:  observe(from(batalla(hogwarts),to(4,5))).
% Into: observe([batalla(hogwarts)],5).

% LPS:  if(derroto(_25324,_25326),(varita_obedece(_25324),cortar_cabeza(neville,nagini))).
% Into: l_events(happens(derroto(_25324,_25326),_26506,_26512),[holds(varita_obedece(_25324),_26506),happens(cortar_cabeza(neville,nagini),_26506,_26512)]).

% LPS:  terminates(derroto(_26808,_26810),vivo(_26810)).
% Into: terminated(happens(derroto(_26808,_26810),_27974,_27980),vivo(_26810),[]).

% LPS:  initiates(desarmo(_27922,_27924),varita_obedece(_27922)).
% Into: initiated(happens(desarmo(_27922,_27924),_29088,_29094),varita_obedece(_27922),[]).

% LPS:  terminates(desarmo(_29036,_29038),varita_obedece(_29078)).
% Into: terminated(happens(desarmo(_29036,_29038),_30214,_30220),varita_obedece(_29078),[]).

% LPS:  terminates(destruye_varita(_30148),varita_obedece(_30148)).
% Into: terminated(happens(destruye_varita(_30148),_31296,_31302),varita_obedece(_30148),[]).

% LPS:  terminates(cortar_cabeza(_31246,_31248),vivo(_31248)).
% Into: terminated(happens(cortar_cabeza(_31246,_31248),_32412,_32418),vivo(_31248),[]).

% LPS:  then(if(at(varita_obedece(dumbledore),_32376)),from(desarmo(malfoy,dumbledore),to(_32518,_32520))).
% Into: reactive_rule([holds(varita_obedece(dumbledore),_32376)],[happens(desarmo(malfoy,dumbledore),_32518,_32520)]).

% LPS:  then(if(at(varita_obedece(malfoy),_33700)),from(desarmo(harry,malfoy),to(_33842,_33844))).
% Into: reactive_rule([holds(varita_obedece(malfoy),_33700)],[happens(desarmo(harry,malfoy),_33842,_33844)]).

% LPS:  then(if(at(not(vivo(voldemort)),_35056)),from(destruye_varita(harry),to(_35182,_35184))).
% Into: reactive_rule([holds(not(vivo(voldemort)),_35056)],[happens(destruye_varita(harry),_35182,_35184)]).

% LPS:  then(if(from(batalla(hogwarts),to(_36380,_36382))),from(derroto(harry,voldemort),to(_36382,_36558))).
% Into: reactive_rule([happens(batalla(hogwarts),_36380,_36382)],[happens(derroto(harry,voldemort),_36382,_36558)]).
% /pack/logicmoo_ec/test/lps_user_examples/P6 Claudio Condor.pl:37
% pop_lps_dialect('$BLOB'("<stream>(0x562ef89a1700)"),  (/.../(lps_user_examples, 'P6 Claudio Condor.pl')-> /.../(lps_user_examples, 'P6 Claudio Condor.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/P6 Claudio Condor.pl':_45322).


initiated(happens(desarmo(A, _), _, _), varita_obedece(A), []).

fluents([varita_obedece(A), vivo(A)]).

terminated(happens(derroto(_, A), _, _), vivo(A), []).
terminated(happens(desarmo(_, _), _, _), varita_obedece(_), []).
terminated(happens(destruye_varita(A), _, _), varita_obedece(A), []).
terminated(happens(cortar_cabeza(_, A), _, _), vivo(A), []).

reactive_rule([holds(varita_obedece(dumbledore), _)], [happens(desarmo(malfoy, dumbledore), _, _)]).
reactive_rule([holds(varita_obedece(malfoy), _)], [happens(desarmo(harry, malfoy), _, _)]).
reactive_rule([holds(not(vivo(voldemort)), _)], [happens(destruye_varita(harry), _, _)]).
reactive_rule([happens(batalla(hogwarts), _, A)], [happens(derroto(harry, voldemort), A, _)]).

initial_state([varita_obedece(dumbledore), vivo(voldemort), vivo(nagini)]).

l_events(happens(derroto(A, _), B, C), [holds(varita_obedece(A), B), happens(cortar_cabeza(neville, nagini), B, C)]).

:- dynamic actions/1.
:- multifile actions/1.

actions([desarmo(_, _), cortar_cabeza(_, _), destruye_varita(_)]).

events([batalla(_), derroto(_, _)]).

observe([batalla(hogwarts)], 5).

maxTime(8).
% dB(/.../(lps_user_examples, 'P6 Claudio Condor.pl'), lps_visualization(_93982{groups:[_92476{content:"Events", id:"event", order:1}, _92550{content:"varita_obedece(A)", id:"varita_obedece/1", order:3, subgroupStack:"false"}, _92628{content:"vivo(A)", id:"vivo/1", order:3, subgroupStack:"false"}, _92694{content:"Actions", id:"action", order:4}], items:[_92816{content:"dumbledore", end:2, group:"varita_obedece/1", id:0, start:1, subgroup:"dumbledore", title:"Fluent varita_obedece(dumbledore) initiated at 1<br/>and terminated at transition to 2"}, _92942{content:"harry", end:8, group:"varita_obedece/1", id:1, start:3, subgroup:"harry", title:"Fluent varita_obedece(harry) initiated at 3<br/>and terminated at transition to 8"}, _93068{content:"malfoy", end:3, group:"varita_obedece/1", id:2, start:2, subgroup:"malfoy", title:"Fluent varita_obedece(malfoy) initiated at 2<br/>and terminated at transition to 3"}, _93194{content:"nagini", end:6, group:"vivo/1", id:3, start:1, subgroup:"nagini", title:"Fluent vivo(nagini) initiated at 1<br/>and terminated at transition to 6"}, _93320{content:"voldemort", end:6, group:"vivo/1", id:4, start:1, subgroup:"voldemort", title:"Fluent vivo(voldemort) initiated at 1<br/>and terminated at transition to 6"}, _93446{content:"desarmo(malfoy,dumbledore)", group:"action", id:5, start:2, style:"color:green", title:"happens(desarmo(malfoy,dumbledore),1,2)", type:"point"}, _93572{content:"desarmo(harry,malfoy)", group:"action", id:6, start:3, style:"color:green", title:"happens(desarmo(harry,malfoy),2,3)", type:"point"}, _93698{content:"batalla(hogwarts)", group:"event", id:7, start:5, style:"color:#E19735", title:"happens(batalla(hogwarts),4,5)", type:"point"}, _93824{content:"cortar_cabeza(neville,nagini)", group:"action", id:8, start:6, style:"color:green", title:"happens(cortar_cabeza(neville,nagini),5,6)", type:"point"}, _93950{content:"destruye_varita(harry)", group:"action", id:9, start:8, style:"color:green", title:"happens(destruye_varita(harry),7,8)", type:"point"}]}, [])).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'planningfox.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'planningfox.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/planningfox.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'planningfox.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'planningfox.pl'), lps= /.../(lps_user_examples, 'planningfox.pl'), using= /.../(lps_user_examples, 'planningfox.pl')].
% continue_lps_dialect.
% ops.

% LPS:  fluents((has(_56330,_56332),near(_56330,_56332))).
% Into: fluents([has(_56330,_56332),near(_56330,_56332)]).

% LPS:  actions((praise/2,take/2,eat/2)).
% Into: actions([praise(_58840,_58842),take(_58852,_58854),eat(_58864,_58866)]).

% LPS:  events((hunger/1,sing/1,gets/2,reach/1)).
% Into: events([hunger(_60258),sing(_60268),gets(_60278,_60280),reach(_60290)]).

% LPS:  initially(has(crow,cheese)).
% Into: initial_state([has(crow,cheese)]).

% LPS:  observe(from(hunger(me),to(1,2))).
% Into: observe([hunger(me)],2).

% LPS:  if(initiates(take(_62312,_62314),has(_62312,_62314)),near(_62312,_62314)).
% Into: initiated(happens(take(_62312,_62314),_63624,_63630),has(_62312,_62314),[holds(near(_62312,_62314),_63624)]).

% LPS:  if(terminates(take(_63884,_63886),has(_63940,_63886)),_63940\==_63884).
% Into: terminated(happens(take(_63884,_63886),_65208,_65214),has(_63940,_63886),[_63940\==_63884]).

% LPS:  initiates(sing(crow),near(me,cheese)).
% Into: initiated(happens(sing(crow),_66746,_66752),near(me,cheese),[]).

% LPS:  if(sing(crow),praise(me,crow)).
% Into: l_events(happens(sing(crow),_67752,_67758),[happens(praise(me,crow),_67752,_67758)]).

% LPS:  if(reach(has(me,cheese)),(reach(near(me,cheese)),take(me,cheese))).
% Into: l_events(happens(reach(has(me,cheese)),_69348,_69354),[happens(reach(near(me,cheese)),_69348,_69428),happens(take(me,cheese),_69428,_69354)]).

% LPS:  if(reach(near(me,cheese)),sing(crow)).
% Into: l_events(happens(reach(near(me,cheese)),_70466,_70472),[happens(sing(crow),_70466,_70472)]).

% LPS:  then(if(from(hunger(me),to(_70528,_70530))),(from(reach(has(me,cheese)),to(_70530,_70730)),from(eat(me,cheese),to(_70730,_70882)))).
% Into: reactive_rule([happens(hunger(me),_70528,_70530)],[happens(reach(has(me,cheese)),_70530,_70730),happens(eat(me,cheese),_70730,_70882)]).
% /pack/logicmoo_ec/test/lps_user_examples/planningfox.pl:37
% pop_lps_dialect('$BLOB'("<stream>(0x562ef897f300)"),  (/.../(lps_user_examples, 'planningfox.pl')-> /.../(lps_user_examples, 'planningfox.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/planningfox.pl':_79746).


initiated(happens(take(A, B), C, _), has(A, B), [holds(near(A, B), C)]).
initiated(happens(sing(crow), _, _), near(me, cheese), []).

fluents([has(A, B), near(A, B)]).

terminated(happens(take(A, B), _, _), has(C, B), [C\==A]).

reactive_rule([happens(hunger(me), _, A)], [happens(reach(has(me, cheese)), A, B), happens(eat(me, cheese), B, _)]).

initial_state([has(crow, cheese)]).

l_events(happens(sing(crow), A, B), [happens(praise(me, crow), A, B)]).
l_events(happens(reach(has(me, cheese)), A, B), [happens(reach(near(me, cheese)), A, C), happens(take(me, cheese), C, B)]).
l_events(happens(reach(near(me, cheese)), A, B), [happens(sing(crow), A, B)]).

:- dynamic actions/1.
:- multifile actions/1.

actions([praise(_, _), take(_, _), eat(_, _)]).

events([hunger(_), sing(_), gets(_, _), reach(_)]).

observe([hunger(me)], 2).

maxTime(6).
% dB(/.../(lps_user_examples, 'planningfox.pl'), lps_visualization(_73630{groups:[_72502{content:"Events", id:"event", order:1}, _72576{content:"has(A,B)", id:"has/2", order:3, subgroupStack:"false"}, _72654{content:"near(A,B)", id:"near/2", order:3, subgroupStack:"false"}, _72720{content:"Actions", id:"action", order:4}], items:[_72842{content:"crow,cheese", end:4, group:"has/2", id:0, start:1, subgroup:"crow", title:"Fluent has(crow,cheese) initiated at 1<br/>and terminated at transition to 4"}, _72968{content:"me,cheese", end:7, group:"has/2", id:1, start:4, subgroup:"me", title:"Fluent has(me,cheese) initiated at 4<br/>and terminated at transition to 7"}, _73094{content:"me,cheese", end:7, group:"near/2", id:2, start:3, subgroup:"me", title:"Fluent near(me,cheese) initiated at 3<br/>and terminated at transition to 7"}, _73220{content:"hunger(me)", group:"event", id:3, start:2, style:"color:#E19735", title:"happens(hunger(me),1,2)", type:"point"}, _73346{content:"praise(me,crow)", group:"action", id:4, start:3, style:"color:green", title:"happens(praise(me,crow),2,3)", type:"point"}, _73472{content:"take(me,cheese)", group:"action", id:5, start:4, style:"color:green", title:"happens(take(me,cheese),3,4)", type:"point"}, _73598{content:"eat(me,cheese)", group:"action", id:6, start:5, style:"color:green", title:"happens(eat(me,cheese),4,5)", type:"point"}]}, [])).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'pobrearico.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'pobrearico.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/pobrearico.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'pobrearico.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'pobrearico.pl'), lps= /.../(lps_user_examples, 'pobrearico.pl'), using= /.../(lps_user_examples, 'pobrearico.pl')].
% continue_lps_dialect.
% ops.

% LPS:  fluents((salud/1,ahorros/1,suerte/1,salario/1,pobreza/1,empleo/1,emprendimiento/1,riqueza/1)).
% Into: fluents([salud(_57552),ahorros(_57562),suerte(_57572),salario(_57582),pobreza(_57592),empleo(_57602),emprendimiento(_57612),riqueza(_57622)]).

% LPS:  events((ir/1,miseria,hambre,enfermedad,posible/1)).
% Into: events([ir(_58694),miseria,hambre,enfermedad,posible(_58722)]).

% LPS:  actions(paso_a/1).
% Into: actions([paso_a(_59632)]).

% LPS:  initially((pobreza(yo),salud(yo),suerte(yo))).
% Into: initial_state([pobreza(yo),salud(yo),suerte(yo)]).

% LPS:  observe(from(miseria,to(1,2))).
% Into: observe([miseria],2).

% LPS:  if(ir([_61972]),(posible(_61972),paso_a(_61972))).
% Into: l_events(happens(ir([_61972]),_63132,_63138),[happens(posible(_61972),_63132,_63212),happens(paso_a(_61972),_63212,_63138)]).

% LPS:  if(ir([_63474|_63476]),(posible(_63474),paso_a(_63474),ir(_63476))).
% Into: l_events(happens(ir([_63474|_63476]),_64736,_64742),[happens(posible(_63474),_64736,_64816),happens(paso_a(_63474),_64816,_64882),happens(ir(_63476),_64882,_64742)]).

% LPS:  if(ahorros(yo),salario(yo)).
% Into: l_int(holds(ahorros(yo),_74544),[holds(salario(yo),_74544)]).

% LPS:  if(terminates(paso_a(pobreza),empleo(yo)),empleo(yo)).
% Into: terminated(happens(paso_a(pobreza),_75806,_75812),empleo(yo),[holds(empleo(yo),_75806)]).

% LPS:  if(terminates(paso_a(pobreza),emprendimiento(yo)),emprendimiento(yo)).
% Into: terminated(happens(paso_a(pobreza),_76952,_76958),emprendimiento(yo),[holds(emprendimiento(yo),_76952)]).

% LPS:  if(terminates(paso_a(empleo),pobreza(yo)),pobreza(yo)).
% Into: terminated(happens(paso_a(empleo),_78106,_78112),pobreza(yo),[holds(pobreza(yo),_78106)]).

% LPS:  if(terminates(paso_a(emprendimiento),pobreza(yo)),pobreza(yo)).
% Into: terminated(happens(paso_a(emprendimiento),_19644,_19650),pobreza(yo),[holds(pobreza(yo),_19644)]).

% LPS:  if(initiates(paso_a(empleo),empleo(yo)),salud(yo)).
% Into: initiated(happens(paso_a(empleo),_20798,_20804),empleo(yo),[holds(salud(yo),_20798)]).

% LPS:  if(initiates(paso_a(emprendimiento),emprendimiento(yo)),(salud(yo),suerte(yo))).
% Into: initiated(happens(paso_a(emprendimiento),_22034,_22040),emprendimiento(yo),[holds(salud(yo),_22034),holds(suerte(yo),_22034)]).

% LPS:  if(posible(pobreza),pobreza(yo)).
% Into: l_events(happens(posible(pobreza),_23006,_23006),[holds(pobreza(yo),_23006)]).

% LPS:  if(posible(empleo),salud(yo)).
% Into: l_events(happens(posible(empleo),_24078,_24078),[holds(salud(yo),_24078)]).

% LPS:  if(posible(emprendimiento),(salud(yo),suerte(yo))).
% Into: l_events(happens(posible(emprendimiento),_25232,_25232),[holds(salud(yo),_25232),holds(suerte(yo),_25232)]).

% LPS:  if(posible(riqueza),ahorros(yo)).
% Into: l_events(happens(posible(riqueza),_26304,_26304),[holds(ahorros(yo),_26304)]).

% LPS:  if(posible(riqueza),suerte(yo)).
% Into: l_events(happens(posible(riqueza),_27376,_27376),[holds(suerte(yo),_27376)]).

% LPS:  if(initiates(paso_a(empleo),salario(yo)),salud(yo)).
% Into: initiated(happens(paso_a(empleo),_28630,_28636),salario(yo),[holds(salud(yo),_28630)]).

% LPS:  if(initiates(paso_a(emprendimiento),ahorros(yo)),(salud(yo),suerte(yo))).
% Into: initiated(happens(paso_a(emprendimiento),_29866,_29872),ahorros(yo),[holds(salud(yo),_29866),holds(suerte(yo),_29866)]).

% LPS:  if(initiates(paso_a(riqueza),riqueza(yo)),ahorros(yo)).
% Into: initiated(happens(paso_a(riqueza),_31020,_31026),riqueza(yo),[holds(ahorros(yo),_31020)]).

% LPS:  if(initiates(paso_a(riqueza),riqueza(yo)),suerte(yo)).
% Into: initiated(happens(paso_a(riqueza),_32174,_32180),riqueza(yo),[holds(suerte(yo),_32174)]).

% LPS:  terminates(enfermedad,suerte(yo)).
% Into: terminated(happens(enfermedad,_33232,_33238),suerte(yo),[]).

% LPS:  then(if(from(miseria,to(_33192,_33194))),(plan(pobreza,riqueza,_33348),from(ir(_33348),to(_33194,_33426)))).
% Into: reactive_rule([happens(miseria,_33192,_33194)],[plan(pobreza,riqueza,_33348),happens(ir(_33348),_33194,_33426)]).

% LPS:  then(if(from(hambre,to(_34654,_34656))),(plan(pobreza,riqueza,_34810),from(ir(_34810),to(_34656,_34888)))).
% Into: reactive_rule([happens(hambre,_34654,_34656)],[plan(pobreza,riqueza,_34810),happens(ir(_34810),_34656,_34888)]).
% /pack/logicmoo_ec/test/lps_user_examples/pobrearico.pl:66
% pop_lps_dialect('$BLOB'("<stream>(0x562ef98c3000)"),  (/.../(lps_user_examples, 'pobrearico.pl')-> /.../(lps_user_examples, 'pobrearico.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/pobrearico.pl':_43742).


conecta(pobreza, empleo).
conecta(pobreza, emprendimiento).
conecta(empleo, riqueza).
conecta(emprendimiento, riqueza).

initiated(happens(paso_a(empleo), A, _), empleo(yo), [holds(salud(yo), A)]).
initiated(happens(paso_a(emprendimiento), A, _), emprendimiento(yo), [holds(salud(yo), A), holds(suerte(yo), A)]).
initiated(happens(paso_a(empleo), A, _), salario(yo), [holds(salud(yo), A)]).
initiated(happens(paso_a(emprendimiento), A, _), ahorros(yo), [holds(salud(yo), A), holds(suerte(yo), A)]).
initiated(happens(paso_a(riqueza), A, _), riqueza(yo), [holds(ahorros(yo), A)]).
initiated(happens(paso_a(riqueza), A, _), riqueza(yo), [holds(suerte(yo), A)]).

fluents([salud(_), ahorros(_), suerte(_), salario(_), pobreza(_), empleo(_), emprendimiento(_), riqueza(_)]).

l_int(holds(ahorros(yo), A), [holds(salario(yo), A)]).

terminated(happens(paso_a(pobreza), A, _), empleo(yo), [holds(empleo(yo), A)]).
terminated(happens(paso_a(pobreza), A, _), emprendimiento(yo), [holds(emprendimiento(yo), A)]).
terminated(happens(paso_a(empleo), A, _), pobreza(yo), [holds(pobreza(yo), A)]).
terminated(happens(paso_a(emprendimiento), A, _), pobreza(yo), [holds(pobreza(yo), A)]).
terminated(happens(enfermedad, _, _), suerte(yo), []).

reactive_rule([happens(miseria, _, A)], [plan(pobreza, riqueza, B), happens(ir(B), A, _)]).
reactive_rule([happens(hambre, _, A)], [plan(pobreza, riqueza, B), happens(ir(B), A, _)]).

initial_state([pobreza(yo), salud(yo), suerte(yo)]).

l_events(happens(ir([A]), B, C), [happens(posible(A), B, D), happens(paso_a(A), D, C)]).
l_events(happens(ir([A|B]), C, D), [happens(posible(A), C, E), happens(paso_a(A), E, F), happens(ir(B), F, D)]).
l_events(happens(posible(pobreza), A, A), [holds(pobreza(yo), A)]).
l_events(happens(posible(empleo), A, A), [holds(salud(yo), A)]).
l_events(happens(posible(emprendimiento), A, A), [holds(salud(yo), A), holds(suerte(yo), A)]).
l_events(happens(posible(riqueza), A, A), [holds(ahorros(yo), A)]).
l_events(happens(posible(riqueza), A, A), [holds(suerte(yo), A)]).

plan(A, B, [B]) :-
    conecta(A, B).
plan(A, B, [A, C|R]) :-
    conecta(A, C),
    not(C=B),
    plan(C, B, R).

:- dynamic actions/1.
:- multifile actions/1.

actions([paso_a(_)]).

events([ir(_), miseria, hambre, enfermedad, posible(_)]).

observe([miseria], 2).

maxTime(10).
% dB(/.../(lps_user_examples, 'pobrearico.pl'), lps_visualization(_102526{groups:[_100174{content:"Events", id:"event", order:1}, _100248{content:"ahorros(A)", id:"ahorros/1", order:3, subgroupStack:"false"}, _100326{content:"empleo(A)", id:"empleo/1", order:3, subgroupStack:"false"}, _100404{content:"emprendimiento(A)", id:"emprendimiento/1", order:3, subgroupStack:"false"}, _100482{content:"pobreza(A)", id:"pobreza/1", order:3, subgroupStack:"false"}, _100560{content:"riqueza(A)", id:"riqueza/1", order:3, subgroupStack:"false"}, _100638{content:"salario(A)", id:"salario/1", order:3, subgroupStack:"false"}, _100716{content:"salud(A)", id:"salud/1", order:3, subgroupStack:"false"}, _100794{content:"suerte(A)", id:"suerte/1", order:3, subgroupStack:"false"}, _100860{content:"Actions", id:"action", order:4}], items:[_100982{content:"yo", end:11, group:"ahorros/1", id:0, start:4, subgroup:"yo", title:"Fluent ahorros(yo) initiated at 4<br/>and terminated at transition to 11"}, _101108{content:"yo", end:11, group:"empleo/1", id:1, start:4, subgroup:"yo", title:"Fluent empleo(yo) initiated at 4<br/>and terminated at transition to 11"}, _101234{content:"yo", end:11, group:"emprendimiento/1", id:2, start:4, subgroup:"yo", title:"Fluent emprendimiento(yo) initiated at 4<br/>and terminated at transition to 11"}, _101360{content:"yo", end:4, group:"pobreza/1", id:3, start:1, subgroup:"yo", title:"Fluent pobreza(yo) initiated at 1<br/>and terminated at transition to 4"}, _101486{content:"yo", end:11, group:"riqueza/1", id:4, start:5, subgroup:"yo", title:"Fluent riqueza(yo) initiated at 5<br/>and terminated at transition to 11"}, _101612{content:"yo", end:11, group:"salario/1", id:5, start:4, subgroup:"yo", title:"Fluent salario(yo) initiated at 4<br/>and terminated at transition to 11"}, _101738{content:"yo", end:11, group:"salud/1", id:6, start:1, subgroup:"yo", title:"Fluent salud(yo) initiated at 1<br/>and terminated at transition to 11"}, _101864{content:"yo", end:11, group:"suerte/1", id:7, start:1, subgroup:"yo", title:"Fluent suerte(yo) initiated at 1<br/>and terminated at transition to 11"}, _101990{content:"miseria", group:"event", id:8, start:2, style:"color:#E19735", title:"happens(miseria,1,2)", type:"point"}, _102116{content:"paso_a(pobreza)", group:"action", id:9, start:3, style:"color:green", title:"happens(paso_a(pobreza),2,3)", type:"point"}, _102242{content:"paso_a(emprendimiento)", group:"action", id:10, start:4, style:"color:green", title:"happens(paso_a(emprendimiento),3,4)", type:"point"}, _102368{content:"paso_a(empleo)", group:"action", id:11, start:4, style:"color:green", title:"happens(paso_a(empleo),3,4)", type:"point"}, _102494{content:"paso_a(riqueza)", group:"action", id:12, start:5, style:"color:green", title:"happens(paso_a(riqueza),4,5)", type:"point"}]}, [])).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'potencia.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'potencia.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/potencia.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'potencia.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'potencia.pl'), lps= /.../(lps_user_examples, 'potencia.pl'), using= /.../(lps_user_examples, 'potencia.pl')].
% continue_lps_dialect.
% ops.
% /pack/logicmoo_ec/test/lps_user_examples/potencia.pl:3
% pop_lps_dialect('$BLOB'("<stream>(0x562ef899e200)"),  (/.../(lps_user_examples, 'potencia.pl')-> /.../(lps_user_examples, 'potencia.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/potencia.pl':_64296).


:- dynamic actions/1.
:- multifile actions/1.

% dB(/.../(lps_user_examples, 'potencia.pl'), lps_visualization(_41292{groups:[], items:[]}, [])).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'PrologContagion.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'PrologContagion.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/PrologContagion.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'PrologContagion.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'PrologContagion.pl'), lps= /.../(lps_user_examples, 'PrologContagion.pl'), using= /.../(lps_user_examples, 'PrologContagion.pl')].
% continue_lps_dialect.
% ops.

% LPS:  tested(15,alice,positive).
% Into: l_events(happens(tested(15,alice,positive),_26368,_26374),[]).
% /pack/logicmoo_ec/test/lps_user_examples/PrologContagion.pl:29
% pop_lps_dialect('$BLOB'("<stream>(0x562ef899f300)"),  (/.../(lps_user_examples, 'PrologContagion.pl')-> /.../(lps_user_examples, 'PrologContagion.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/PrologContagion.pl':_42874).


:- dynamic infected/2.


met(1, [alice, bob]).
met(6, [bob, charlie, delilah]).
met(12, [delilah, edgar, fiona, gertrude, iona]).
met(14, [edgar, fiona, gertrude, hannah, iona]).

l_events(happens(tested(15, alice, positive), _, _), []).

propagate :-
    retractall(infected(_, _)),
    tested(When, P, positive),
    infectionInterval(When, Begin, _),
    assert(infected(Begin, P)),
    fail.
propagate :-
    met(When, Persons),
    select(Sick, Persons, Others),
    (   infected(Begin, Sick)
    ->  true
    ),
    Begin=<When,
    member(Victim, Others),
    \+ infected(_, Victim),
    assert(infected(When, Victim)),
    fail.
propagate :-
    \+ ( infected(When, P),
         \+ writeln(When/P)
       ).

:- dynamic actions/1.
:- multifile actions/1.


infectionInterval(Test, Begin, End) :-
    Begin is Test-5,
    End is Test+10.
% dB(/.../(lps_user_examples, 'PrologContagion.pl'), lps_visualization(_38324{groups:[], items:[]}, [])).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'purereacting.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'purereacting.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/purereacting.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'purereacting.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'purereacting.pl'), lps= /.../(lps_user_examples, 'purereacting.pl'), using= /.../(lps_user_examples, 'purereacting.pl')].
% continue_lps_dialect.
% ops.

% LPS:  events(alguien_me_ataca).
% Into: events([alguien_me_ataca]).

% LPS:  actions((responde_igual,consigue_ayuda,trata_de_escapar)).
% Into: actions([responde_igual,consigue_ayuda,trata_de_escapar]).

% LPS:  observe(from(alguien_me_ataca,to(1,2))).
% Into: observe([alguien_me_ataca],2).

% LPS:  then(if(from(alguien_me_ataca,to(_22678,_22680))),from(responde_igual,to(_22680,_22816))).
% Into: reactive_rule([happens(alguien_me_ataca,_22678,_22680)],[happens(responde_igual,_22680,_22816)]).

% LPS:  then(if(from(alguien_me_ataca,to(_24022,_24024))),from(consigue_ayuda,to(_24024,_24160))).
% Into: reactive_rule([happens(alguien_me_ataca,_24022,_24024)],[happens(consigue_ayuda,_24024,_24160)]).

% LPS:  then(if(from(alguien_me_ataca,to(_25366,_25368))),from(trata_de_escapar,to(_25368,_25504))).
% Into: reactive_rule([happens(alguien_me_ataca,_25366,_25368)],[happens(trata_de_escapar,_25368,_25504)]).
% /pack/logicmoo_ec/test/lps_user_examples/purereacting.pl:23
% pop_lps_dialect('$BLOB'("<stream>(0x562ef897e600)"),  (/.../(lps_user_examples, 'purereacting.pl')-> /.../(lps_user_examples, 'purereacting.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/purereacting.pl':_34290).


reactive_rule([happens(alguien_me_ataca, _, A)], [happens(responde_igual, A, _)]).
reactive_rule([happens(alguien_me_ataca, _, A)], [happens(consigue_ayuda, A, _)]).
reactive_rule([happens(alguien_me_ataca, _, A)], [happens(trata_de_escapar, A, _)]).

:- dynamic actions/1.
:- multifile actions/1.

actions([responde_igual, consigue_ayuda, trata_de_escapar]).

events([alguien_me_ataca]).

observe([alguien_me_ataca], 2).

maxTime(10).
% dB(/.../(lps_user_examples, 'purereacting.pl'), lps_visualization(_67564{groups:[_66970{content:"Events", id:"event", order:1}, _67032{content:"Actions", id:"action", order:4}], items:[_67154{content:"alguien_me_ataca", group:"event", id:0, start:2, style:"color:#E19735", title:"happens(alguien_me_ataca,1,2)", type:"point"}, _67280{content:"responde_igual", group:"action", id:1, start:3, style:"color:green", title:"happens(responde_igual,2,3)", type:"point"}, _67406{content:"consigue_ayuda", group:"action", id:2, start:3, style:"color:green", title:"happens(consigue_ayuda,2,3)", type:"point"}, _67532{content:"trata_de_escapar", group:"action", id:3, start:3, style:"color:green", title:"happens(trata_de_escapar,2,3)", type:"point"}]}, [])).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'PxkqiaFK.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'PxkqiaFK.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/PxkqiaFK.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'PxkqiaFK.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'PxkqiaFK.pl'), lps= /.../(lps_user_examples, 'PxkqiaFK.pl'), using= /.../(lps_user_examples, 'PxkqiaFK.pl')].
% continue_lps_dialect.
% ops.

% LPS:  fluents(fire).
% Into: fluents([fire]).

% LPS:  actions((escape,eliminate)).
% Into: actions([escape,eliminate]).

% LPS:  events(deal_with_fire).
% Into: events([deal_with_fire]).

% LPS:  initially(fire).
% Into: initial_state([fire]).

% LPS:  then(if(at(fire,_58540)),from(deal_with_fire,to(_58540,_58644))).
% Into: reactive_rule([holds(fire,_58540)],[happens(deal_with_fire,_58540,_58644)]).

% LPS:  if(from(deal_with_fire,to(_60218,_60220)),from(escape,to(_60218,_60220))).
% Into: l_events(happens(deal_with_fire,_60218,_60220),[happens(escape,_60218,_60220)]).

% LPS:  if(from(deal_with_fire,to(_61474,_61476)),from(eliminate,to(_61474,_61476))).
% Into: l_events(happens(deal_with_fire,_61474,_61476),[happens(eliminate,_61474,_61476)]).

% LPS:  terminates(eliminate,fire).
% Into: terminated(happens(eliminate,_63784,_63790),fire,[]).
% /pack/logicmoo_ec/test/lps_user_examples/PxkqiaFK.pl:25
% pop_lps_dialect('$BLOB'("<stream>(0x562ef86eec00)"),  (/.../(lps_user_examples, 'PxkqiaFK.pl')-> /.../(lps_user_examples, 'PxkqiaFK.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/PxkqiaFK.pl':_71366).


fluents([fire]).

terminated(happens(eliminate, _, _), fire, []).

reactive_rule([holds(fire, A)], [happens(deal_with_fire, A, _)]).

initial_state([fire]).

l_events(happens(deal_with_fire, A, B), [happens(escape, A, B)]).
l_events(happens(deal_with_fire, A, B), [happens(eliminate, A, B)]).

:- dynamic actions/1.
:- multifile actions/1.

actions([escape, eliminate]).

events([deal_with_fire]).

maxTime(5).
% dB(/.../(lps_user_examples, 'PxkqiaFK.pl'), lps_visualization(_61742{groups:[_61034{content:"fire", id:"fire/0", order:3, subgroupStack:"false"}, _61100{content:"Actions", id:"action", order:4}], items:[_61210{content:"fire", end:6, group:"fire/0", id:0, start:1, title:"Fluent fire initiated at 1<br/>and terminated at transition to 6"}, _61332{content:"escape", group:"action", id:1, start:2, style:"color:green", title:"happens(escape,1,2)", type:"point"}, _61458{content:"escape", group:"action", id:2, start:3, style:"color:green", title:"happens(escape,2,3)", type:"point"}, _61584{content:"escape", group:"action", id:3, start:4, style:"color:green", title:"happens(escape,3,4)", type:"point"}, _61710{content:"escape", group:"action", id:4, start:5, style:"color:green", title:"happens(escape,4,5)", type:"point"}]}, [])).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'reactivesolutions.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'reactivesolutions.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/reactivesolutions.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'reactivesolutions.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'reactivesolutions.pl'), lps= /.../(lps_user_examples, 'reactivesolutions.pl'), using= /.../(lps_user_examples, 'reactivesolutions.pl')].
% continue_lps_dialect.
% ops.

% LPS:  events(b/1).
% Into: events([b(_19450)]).

% LPS:  then(if(true),b(_19486)).
% Into: reactive_rule([],[happens(b(_19486),_20538,_20544)]).

% LPS:  if(b(_20530),c(_20530)).
% Into: l_events(happens(b(_20530),_21596,_21596),[c(_20530)]).
% /pack/logicmoo_ec/test/lps_user_examples/reactivesolutions.pl:11
% pop_lps_dialect('$BLOB'("<stream>(0x562ef86f1d00)"),  (/.../(lps_user_examples, 'reactivesolutions.pl')-> /.../(lps_user_examples, 'reactivesolutions.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/reactivesolutions.pl':_33824).


c(1) :-
    writeln(1).
c(2) :-
    writeln(2).
c(3) :-
    writeln(3).

reactive_rule([], [happens(b(_), _, _)]).

l_events(happens(b(A), B, B), [c(A)]).

:- dynamic actions/1.
:- multifile actions/1.


events([b(_)]).
1
2
3
% dB(/.../(lps_user_examples, 'reactivesolutions.pl'), lps_visualization(_32272{groups:[], items:[]}, [])).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'realt_time_cycle(1).pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'realt_time_cycle(1).pl')).
% /pack/logicmoo_ec/test/lps_user_examples/realt_time_cycle(1).pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'realt_time_cycle(1).pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'realt_time_cycle(1).pl'), lps= /.../(lps_user_examples, 'realt_time_cycle(1).pl'), using= /.../(lps_user_examples, 'realt_time_cycle(1).pl')].
% continue_lps_dialect.
% ops.

% LPS:  observe(at(remove(me,items),2019/6/17)).
% Into: observe([remove(me,items)],2019/6/17).
% /pack/logicmoo_ec/test/lps_user_examples/realt_time_cycle(1).pl:11
% pop_lps_dialect('$BLOB'("<stream>(0x562ef86ee500)"),  (/.../(lps_user_examples, 'realt_time_cycle(1).pl')-> /.../(lps_user_examples, 'realt_time_cycle(1).pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/realt_time_cycle(1).pl':_29406).


simulatedRealTimePerCycle(T) :-
    T is 86400/3.

simulatedRealTimeBeginning('2019-06-15').

:- dynamic actions/1.
:- multifile actions/1.


observe([remove(me, items)], 2019/6/17).

maxTime(30).
% dB(/.../(lps_user_examples, 'realt_time_cycle(1).pl'), lps_visualization(_40754{groups:[_40548{content:"Events", id:"event", order:1}], items:[_40670{content:"remove(me,items)", group:"event", id:0, start:9, style:"color:#E19735", title:"happens(remove(me,items),8,9)", type:"point"}], simulatedRealTime:_40742{begin:1560556800.0, cycle:28800}}, [])).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'realt_time_cycle.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'realt_time_cycle.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/realt_time_cycle.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'realt_time_cycle.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'realt_time_cycle.pl'), lps= /.../(lps_user_examples, 'realt_time_cycle.pl'), using= /.../(lps_user_examples, 'realt_time_cycle.pl')].
% continue_lps_dialect.
% ops.

% LPS:  observe(at(remove(me,items),2019/6/17)).
% Into: observe([remove(me,items)],2019/6/17).
% /pack/logicmoo_ec/test/lps_user_examples/realt_time_cycle.pl:11
% pop_lps_dialect('$BLOB'("<stream>(0x562ef85cbe00)"),  (/.../(lps_user_examples, 'realt_time_cycle.pl')-> /.../(lps_user_examples, 'realt_time_cycle.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/realt_time_cycle.pl':_30458).


simulatedRealTimePerCycle(T) :-
    T is 86400/3.

simulatedRealTimeBeginning('2019-06-15').

:- dynamic actions/1.
:- multifile actions/1.


observe([remove(me, items)], 2019/6/17).

maxTime(30).
% dB(/.../(lps_user_examples, 'realt_time_cycle.pl'), lps_visualization(_42864{groups:[_42658{content:"Events", id:"event", order:1}], items:[_42780{content:"remove(me,items)", group:"event", id:0, start:9, style:"color:#E19735", title:"happens(remove(me,items),8,9)", type:"point"}], simulatedRealTime:_42852{begin:1560556800.0, cycle:28800}}, [])).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'salomon.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'salomon.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/salomon.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'salomon.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'salomon.pl'), lps= /.../(lps_user_examples, 'salomon.pl'), using= /.../(lps_user_examples, 'salomon.pl')].
% continue_lps_dialect.
% ops.

% LPS:  events((disputa_entre(_19778,_19780),poner_a_prueba(_19778,_19780),propone_salida_drastica(_19876),tomar_decision)).
% Into: events([disputa_entre(_19778,_19780),poner_a_prueba(_19778,_19780),propone_salida_drastica(_19876),tomar_decision]).

% LPS:  actions((propone_dividir_nino(_21140),dice(_21140,_21196),declara(_21140,_21252),dicta(_21140,_21308))).
% Into: actions([propone_dividir_nino(_21140),dice(_21140,_21196),declara(_21140,_21252),dicta(_21140,_21308)]).

% LPS:  observe(from(disputa_entre(a,b),to(1,2))).
% Into: observe([disputa_entre(a,b)],2).

% LPS:  then(if(from(disputa_entre(_23762,_23764),to(_23800,_23802))),from(poner_a_prueba(_23762,_23764),to(_23802,_23978))).
% Into: reactive_rule([happens(disputa_entre(_23762,_23764),_23800,_23802)],[happens(poner_a_prueba(_23762,_23764),_23802,_23978)]).

% LPS:  if(from(poner_a_prueba(_25240,_25242),to(_25278,_25280)),from(propone_salida_drastica(salomon),to(_25278,_25280))).
% Into: l_events(happens(poner_a_prueba(_25240,_25242),_25278,_25280),[happens(propone_salida_drastica(salomon),_25278,_25280)]).

% LPS:  if(from(propone_salida_drastica(_26580),to(_26616,_26618)),from(propone_dividir_nino(_26580),to(_26616,_26618))).
% Into: l_events(happens(propone_salida_drastica(_26580),_26616,_26618),[happens(propone_dividir_nino(_26580),_26616,_26618)]).

% LPS:  then(if((from(propone_dividir_nino(salomon),to(_28004,_28006)),mujer(_28104),soy_su_madre(_28104))),from(dice(_28104,'No lo mate! Déselo a Ella'),to(_28006,_28326))).
% Into: reactive_rule([happens(propone_dividir_nino(salomon),_28004,_28006),mujer(_28104),soy_su_madre(_28104)],[happens(dice(_28104,'No lo mate! Déselo a Ella'),_28006,_28326)]).

% LPS:  then(if((from(propone_dividir_nino(salomon),to(_30544,_30546)),mujer(_30644),not(soy_su_madre(_30644)))),from(dice(_30644,'Sí, mátelo'),to(_30546,_30890))).
% Into: reactive_rule([happens(propone_dividir_nino(salomon),_30544,_30546),mujer(_30644),not(soy_su_madre(_30644))],[happens(dice(_30644,'Sí, mátelo'),_30546,_30890)]).

% LPS:  then(if((from(propone_dividir_nino(_33588),to(_33624,_33626)),from(dice(_33738,'No lo mate! Déselo a Ella'),to(_33776,_33778)),from(dice(_33890,'Sí, mátelo'),to(_33928,_33930)))),(from(declara(_33588,la_verdadera_madre_es(_33738)),to(_34192,_34194)),from(dicta(_33588,entreguen_nino_a(_33738)),to(_34194,_34370)))).
% Into: reactive_rule([happens(propone_dividir_nino(_33588),_33624,_33626),happens(dice(_33738,'No lo mate! Déselo a Ella'),_33776,_33778),happens(dice(_33890,'Sí, mátelo'),_33928,_33930)],[happens(declara(_33588,la_verdadera_madre_es(_33738)),_34192,_34194),happens(dicta(_33588,entreguen_nino_a(_33738)),_34194,_34370)]).
% /pack/logicmoo_ec/test/lps_user_examples/salomon.pl:68
% pop_lps_dialect('$BLOB'("<stream>(0x562ef6d1c200)"),  (/.../(lps_user_examples, 'salomon.pl')-> /.../(lps_user_examples, 'salomon.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/salomon.pl':_47314).


:- dynamic used/1.

used(0).

:- dynamic state/1.

state(real_time(1601499407.4318533)).
state(lps_user(unknown_user)).
state(lps_user(unknown_user, unknown_email)).

soy_su_madre(b).

:- dynamic tried/3.


:- dynamic happens/3.


:- dynamic lps_test_result/3.

lps_test_result(fluents, 0, 0).
lps_test_result(events, 1, 0).
lps_test_result(fluents, 1, 0).
lps_test_result(events, 2, 1).
lps_test_result(fluents, 2, 0).
lps_test_result(events, 3, 1).
lps_test_result(fluents, 3, 0).
lps_test_result(events, 4, 2).
lps_test_result(fluents, 4, 0).
lps_test_result(events, 5, 1).
lps_test_result(fluents, 5, 0).
lps_test_result(events, 6, 1).
lps_test_result(fluents, 6, 0).
lps_test_result(events, 7, 0).
lps_test_result(fluents, 7, 0).
lps_test_result(events, 8, 0).
lps_test_result(fluents, 8, 0).
lps_test_result(events, 9, 0).
lps_test_result(fluents, 9, 0).
lps_test_result(events, 10, 0).
lps_test_result(fluents, 10, 0).
lps_test_result(events, 11, 0).
lps_test_result(fluents, 11, 0).
lps_test_result(events, 12, 0).
lps_test_result(fluents, 12, 0).
lps_test_result(events, 13, 0).
lps_test_result(fluents, 13, 0).
lps_test_result(events, 14, 0).
lps_test_result(fluents, 14, 0).
lps_test_result(events, 15, 0).
lps_test_result(fluents, 15, 0).
lps_test_result(events, 16, 0).
lps_test_result(fluents, 16, 0).
lps_test_result(events, 17, 0).
lps_test_result(fluents, 17, 0).
lps_test_result(events, 18, 0).
lps_test_result(fluents, 18, 0).
lps_test_result(events, 19, 0).
lps_test_result(fluents, 19, 0).
lps_test_result(events, 20, 0).
lps_test_result(fluents, 20, 0).

l_events(happens(poner_a_prueba(_, _), A, B), [happens(propone_salida_drastica(salomon), A, B)]).
l_events(happens(propone_salida_drastica(A), B, C), [happens(propone_dividir_nino(A), B, C)]).

maxtime(10).

:- dynamic lps_test_action_ancestor/3.

lps_test_action_ancestor(poner_a_prueba(a, b), 2, 3).
lps_test_action_ancestor(propone_salida_drastica(salomon), 2, 3).

:- dynamic actions/1.
:- multifile actions/1.

actions([propone_dividir_nino(A), dice(A, _), declara(A, _), dicta(A, _)]).

mujer(a).
mujer(b).

:- dynamic current_goal/1.

current_goal(0).

:- dynamic lps_test_result_item/3.

lps_test_result_item(events, 2, disputa_entre(a, b)).
lps_test_result_item(events, 3, propone_dividir_nino(salomon)).
lps_test_result_item(events, 4, dice(a, 'Sí, mátelo')).
lps_test_result_item(events, 4, dice(b, 'No lo mate! Déselo a Ella')).
lps_test_result_item(events, 5, declara(salomon, la_verdadera_madre_es(b))).
lps_test_result_item(events, 6, dicta(salomon, entreguen_nino_a(b))).

:- dynamic next_state/1.


:- dynamic lps_updating_current_state/0.


:- dynamic real_time_beginning/1.

real_time_beginning(1601499407.205363).

:- dynamic option/1.

option(make_test).
option(swish).
option(dc).
option(silent).

reactive_rule([happens(disputa_entre(A, B), _, C)], [happens(poner_a_prueba(A, B), C, _)]).
reactive_rule([happens(propone_dividir_nino(salomon), _, A), mujer(B), soy_su_madre(B)], [happens(dice(B, 'No lo mate! Déselo a Ella'), A, _)]).
reactive_rule([happens(propone_dividir_nino(salomon), _, A), mujer(B), not(soy_su_madre(B))], [happens(dice(B, 'Sí, mátelo'), A, _)]).
reactive_rule([happens(propone_dividir_nino(A), _, _), happens(dice(B, 'No lo mate! Déselo a Ella'), _, _), happens(dice(_, 'Sí, mátelo'), _, _)], [happens(declara(A, la_verdadera_madre_es(B)), _, C), happens(dicta(A, entreguen_nino_a(B)), C, _)]).

:- dynamic depth/1.

depth(0).

:- dynamic current_time/1.

current_time(21).

events([disputa_entre(A, B), poner_a_prueba(A, B), propone_salida_drastica(_), tomar_decision]).

observe([disputa_entre(a, b)], 2).
% with_abs_paths(run_lps_corner_file1, /.../(lps_user_examples, 'salomon.pl')).
% run_lps_corner_file1(/.../(lps_user_examples, 'salomon.pl')).
% /pack/logicmoo_ec/test/lps_user_examples/salomon.pl:1
% push_lps_dialect.
% ops.
% [ti=user, load= /.../(lps_user_examples, 'salomon.pl'), strip=lps, ctx=lps, sm= /.../(lps_user_examples, 'salomon.pl'), lps= /.../(lps_user_examples, 'salomon.pl'), using= /.../(lps_user_examples, 'salomon.pl')].
% continue_lps_dialect.
% ops.

% LPS:  events((disputa_entre(_19810,_19812),poner_a_prueba(_19810,_19812),propone_salida_drastica(_19908),tomar_decision)).
% Into: events([disputa_entre(_19810,_19812),poner_a_prueba(_19810,_19812),propone_salida_drastica(_19908),tomar_decision]).

% LPS:  actions((propone_dividir_nino(_21172),dice(_21172,_21228),declara(_21172,_21284),dicta(_21172,_21340))).
% Into: actions([propone_dividir_nino(_21172),dice(_21172,_21228),declara(_21172,_21284),dicta(_21172,_21340)]).

% LPS:  observe(from(disputa_entre(a,b),to(1,2))).
% Into: observe([disputa_entre(a,b)],2).

% LPS:  then(if(from(disputa_entre(_23794,_23796),to(_23832,_23834))),from(poner_a_prueba(_23794,_23796),to(_23834,_24010))).
% Into: reactive_rule([happens(disputa_entre(_23794,_23796),_23832,_23834)],[happens(poner_a_prueba(_23794,_23796),_23834,_24010)]).

% LPS:  if(from(poner_a_prueba(_25272,_25274),to(_25310,_25312)),from(propone_salida_drastica(salomon),to(_25310,_25312))).
% Into: l_events(happens(poner_a_prueba(_25272,_25274),_25310,_25312),[happens(propone_salida_drastica(salomon),_25310,_25312)]).

% LPS:  if(from(propone_salida_drastica(_26612),to(_26648,_26650)),from(propone_dividir_nino(_26612),to(_26648,_26650))).
% Into: l_events(happens(propone_salida_drastica(_26612),_26648,_26650),[happens(propone_dividir_nino(_26612),_26648,_26650)]).

% LPS:  then(if((from(propone_dividir_nino(salomon),to(_28036,_28038)),mujer(_28136),soy_su_madre(_28136))),from(dice(_28136,'No lo mate! Déselo a Ella'),to(_28038,_28358))).
% Into: reactive_rule([happens(propone_dividir_nino(salomon),_28036,_28038),mujer(_28136),soy_su_madre(_28136)],[happens(dice(_28136,'No lo mate! Déselo a Ella'),_28038,_28358)]).

% LPS:  then(if((from(propone_dividir_nino(salomon),to(_30576,_30578)),mujer(_30676),not(soy_su_madre(_30676)))),from(dice(_30676,'Sí, mátelo'),to(_30578,_30922))).
% Into: reactive_rule([happens(propone_dividir_nino(salomon),_30576,_30578),mujer(_30676),not(soy_su_madre(_30676))],[happens(dice(_30676,'Sí, mátelo'),_30578,_30922)]).

% LPS:  then(if((from(propone_dividir_nino(_33620),to(_33656,_33658)),from(dice(_33770,'No lo mate! Déselo a Ella'),to(_33808,_33810)),from(dice(_33922,'Sí, mátelo'),to(_33960,_33962)))),(from(declara(_33620,la_verdadera_madre_es(_33770)),to(_34224,_34226)),from(dicta(_33620,entreguen_nino_a(_33770)),to(_34226,_34402)))).
% Into: reactive_rule([happens(propone_dividir_nino(_33620),_33656,_33658),happens(dice(_33770,'No lo mate! Déselo a Ella'),_33808,_33810),happens(dice(_33922,'Sí, mátelo'),_33960,_33962)],[happens(declara(_33620,la_verdadera_madre_es(_33770)),_34224,_34226),happens(dicta(_33620,entreguen_nino_a(_33770)),_34226,_34402)]).
% /pack/logicmoo_ec/test/lps_user_examples/salomon.pl:68
% pop_lps_dialect('$BLOB'("<stream>(0x562ef6d1dd00)"),  (/.../(lps_user_examples, 'salomon.pl')-> /.../(lps_user_examples, 'salomon.pl'))).
% ops.
% :-listing('/pack/logicmoo_ec/test/lps_user_examples/salomon.pl':_47346).


:- dynamic used/1.

used(0).
used(0).

:- dynamic state/1.

state(real_time(1601499407.4318533)).
state(lps_user(unknown_user)).
state(lps_user(unknown_user, unknown_email)).
state(real_time(1601499787.071311)).
state(lps_user(unknown_user)).
state(lps_user(unknown_user, unknown_email)).

soy_su_madre(b).

:- dynamic tried/3.


:- dynamic happens/3.


:- dynamic lps_test_result/3.

lps_test_result(fluents, 0, 0).
lps_test_result(events, 1, 0).
lps_test_result(fluents, 1, 0).
lps_test_result(events, 2, 1).
lps_test_result(fluents, 2, 0).
lps_test_result(events, 3, 1).
lps_test_result(fluents, 3, 0).
lps_test_result(events, 4, 2).
lps_test_result(fluents, 4, 0).
lps_test_result(events, 5, 1).
lps_test_result(fluents, 5, 0).
lps_test_result(events, 6, 1).
lps_test_result(fluents, 6, 0).
lps_test_result(events, 7, 0).
lps_test_result(fluents, 7, 0).
lps_test_result(events, 8, 0).
lps_test_result(fluents, 8, 0).
lps_test_result(events, 9, 0).
lps_test_result(fluents, 9, 0).
lps_test_result(events, 10, 0).
lps_test_result(fluents, 10, 0).
lps_test_result(events, 11, 0).
lps_test_result(fluents, 11, 0).
lps_test_result(events, 12, 0).
lps_test_result(fluents, 12, 0).
lps_test_result(events, 13, 0).
lps_test_result(fluents, 13, 0).
lps_test_result(events, 14, 0).
lps_test_result(fluents, 14, 0).
lps_test_result(events, 15, 0).
lps_test_result(fluents, 15, 0).
lps_test_result(events, 16, 0).
lps_test_result(fluents, 16, 0).
lps_test_result(events, 17, 0).
lps_test_result(fluents, 17, 0).
lps_test_result(events, 18, 0).
lps_test_result(fluents, 18, 0).
lps_test_result(events, 19, 0).
lps_test_result(fluents, 19, 0).
lps_test_result(events, 20, 0).
lps_test_result(fluents, 20, 0).
lps_test_result(fluents, 0, 0).

l_events(happens(poner_a_prueba(_, _), A, B), [happens(propone_salida_drastica(salomon), A, B)]).
l_events(happens(propone_salida_drastica(A), B, C), [happens(propone_dividir_nino(A), B, C)]).

maxtime(10).

:- dynamic lps_test_action_ancestor/3.

lps_test_action_ancestor(poner_a_prueba(a, b), 2, 3).
lps_test_action_ancestor(propone_salida_drastica(salomon), 2, 3).

:- dynamic actions/1.
:- multifile actions/1.

actions([propone_dividir_nino(A), dice(A, _), declara(A, _), dicta(A, _)]).

mujer(a).
mujer(b).

:- dynamic current_goal/1.

current_goal(0).
current_goal(0).

:- dynamic lps_test_result_item/3.

lps_test_result_item(events, 2, disputa_entre(a, b)).
lps_test_result_item(events, 3, propone_dividir_nino(salomon)).
lps_test_result_item(events, 4, dice(a, 'Sí, mátelo')).
lps_test_result_item(events, 4, dice(b, 'No lo mate! Déselo a Ella')).
lps_test_result_item(events, 5, declara(salomon, la_verdadera_madre_es(b))).
lps_test_result_item(events, 6, dicta(salomon, entreguen_nino_a(b))).

:- dynamic next_state/1.


:- dynamic lps_updating_current_state/0.


:- dynamic real_time_beginning/1.

real_time_beginning(1601499407.205363).
real_time_beginning(1601499787.0712419).

:- dynamic option/1.

option(make_test).
option(swish).
option(dc).
option(silent).

reactive_rule([happens(disputa_entre(A, B), _, C)], [happens(poner_a_prueba(A, B), C, _)]).
reactive_rule([happens(propone_dividir_nino(salomon), _, A), mujer(B), soy_su_madre(B)], [happens(dice(B, 'No lo mate! Déselo a Ella'), A, _)]).
reactive_rule([happens(propone_dividir_nino(salomon), _, A), mujer(B), not(soy_su_madre(B))], [happens(dice(B, 'Sí, mátelo'), A, _)]).
reactive_rule([happens(propone_dividir_nino(A), _, _), happens(dice(B, 'No lo mate! Déselo a Ella'), _, _), happens(dice(_, 'Sí, mátelo'), _, _)], [happens(declara(A, la_verdadera_madre_es(B)), _, C), happens(dicta(A, entreguen_nino_a(B)), C, _)]).

:- dynamic depth/1.

depth(0).
depth(0).

:- dynamic current_time/1.

current_time(0).
current_time(22).

events([disputa_entre(A, B), poner_a_prueba(A, B), propone_salida_drastica(_), tomar_decision]).

observe([disputa_entre(a, b)], 2).
/*
^  Call: (109) [logicmoo_lps] forall(((name_to_files('../test/lps_user_examples/salomon.pl', _18772)*->member(_18776, _18772);'../test/lps_user_examples/salomon.pl'=_18776), spec_to_files(_18776, _18790)), with_abs_paths(run_lps_corner_file1, _18790))
^  Unify: (109) [$apply] forall(logicmoo_lps:((name_to_files('../test/lps_user_examples/salomon.pl', _18772)*->member(_18776, _18772);'../test/lps_user_examples/salomon.pl'=_18776), spec_to_files(_18776, _18790)), logicmoo_lps:with_abs_paths(run_lps_corner_file1, _18790))
^  Call: (111) [logicmoo_lps] logicmoo_startup:name_to_files('../test/lps_user_examples/salomon.pl', _18772)
^  Unify: (111) [logicmoo_lps] logicmoo_startup:name_to_files('../test/lps_user_examples/salomon.pl', _18772)
^  Call: (112) [logicmoo_lps] logicmoo_startup:name_to_files('../test/lps_user_examples/salomon.pl', _18772, true)
^  Unify: (112) [logicmoo_lps] logicmoo_startup:name_to_files('../test/lps_user_examples/salomon.pl', _18772, true)
^  Call: (113) [logicmoo_lps] logicmoo_startup:name_to_files_('../test/lps_user_examples/salomon.pl', _18772, true)
*/
