%   File   : color.pl
%   Author : Neng-Fa ZHOU
%   Date   : 1992
%   Purpose: color Gardner's map with CLP(FD)

top:-
    functor(Vect,vars,110),
    Vect=..[_|Vars],
    Vars in 0..3,
    build_vars(110,Vect),
    labeling(Vars).
%    write(Vars),nl.

go :-
    statistics(runtime,[Start|_]),
    top,
    statistics(runtime,[End|_]),
    T is End-Start,
    write('%execution time ='), write(T), write(' milliseconds'),nl.

build_vars(N,Vect):-
    N=<0,!.
build_vars(N,Vect):-
    neighbors(N,Neibs),
    arg(N,Vect,Var),
    constrain_color(Var,Neibs,Vect),
    N1 is N-1,
    build_vars(N1,Vect).

constrain_color(Var,[],Vect).
constrain_color(Var,[N|Neibs],Vect):-
    arg(N,Vect,Var1),
    Var#\=Var1,
    constrain_color(Var,Neibs,Vect).

neighbors(1,[]).
neighbors(2,[1]).
neighbors(3,[1,2]).
neighbors(4,[1,3]).
neighbors(5,[1,4]).
neighbors(6,[1,5]).
neighbors(7,[1,6]).
neighbors(8,[1,7]).
neighbors(9,[1,8]).
neighbors(10,[1,9]).
neighbors(11,[1,10]).
neighbors(12,[2,3]).
neighbors(13,[3,4,12]).
neighbors(14,[4,5,13]).
neighbors(15,[5,6,14]).
neighbors(16,[6,7,15]).
neighbors(17,[7,8,16]).
neighbors(18,[8,9,17]).
neighbors(19,[9,10,18]).
neighbors(20,[1,10,11,19]).
neighbors(21,[12,13]).
neighbors(22,[13,14,21]).
neighbors(23,[14,15,22]).
neighbors(24,[15,16,23]).
neighbors(25,[16,17,24]).
neighbors(26,[17,18,25]).
neighbors(27,[18,19,26]).
neighbors(28,[19,20,27]).
neighbors(29,[21,22]).
neighbors(30,[22,23,29]).
neighbors(31,[23,24,30]).
neighbors(32,[24,25,31]).
neighbors(33,[25,26,32]).
neighbors(34,[26,27,33]).
neighbors(35,[27,28,34]).
neighbors(36,[29,30]).
neighbors(37,[30,31,36]).
neighbors(38,[31,32,37]).
neighbors(39,[32,33,38]).
neighbors(40,[33,34,39]).
neighbors(41,[34,35,40]).
neighbors(42,[36,37]).
neighbors(43,[37,38,42]).
neighbors(44,[38,39,43]).
neighbors(45,[39,40,44]).
neighbors(46,[40,41,45]).
neighbors(47,[42,43]).
neighbors(48,[43,44,47]).
neighbors(49,[44,45,48]).
neighbors(50,[45,46,49]).
neighbors(51,[47,48]).
neighbors(52,[48,49,51]).
neighbors(53,[49,50,52]).
neighbors(54,[51,52]).
neighbors(55,[52,53,54]).
neighbors(56,[54,55]).
neighbors(57,[2,12]).
neighbors(58,[12,21,57]).
neighbors(59,[21,29,58]).
neighbors(60,[29,36,59]).
neighbors(61,[36,42,60]).
neighbors(62,[42,47,61]).
neighbors(63,[47,51,62]).
neighbors(64,[51,54,63]).
neighbors(65,[54,56,64]).
neighbors(66,[55,56,65]).
neighbors(67,[53,55,66]).
neighbors(68,[50,53,67]).
neighbors(69,[46,50,68]).
neighbors(70,[41,46,69]).
neighbors(71,[35,41,70]).
neighbors(72,[28,35,71]).
neighbors(73,[1,20,28,72]).
neighbors(74,[64,65,66,67]).
neighbors(75,[63,64,74]).
neighbors(76,[67,68,74,75]).
neighbors(77,[62,63,75]).
neighbors(78,[75,76,77]).
neighbors(79,[68,69,76,78]).
neighbors(80,[61,62,77]).
neighbors(81,[77,78,80]).
neighbors(82,[78,79,81]).
neighbors(83,[69,70,79,82]).
neighbors(84,[60,61,80]).
neighbors(85,[80,81,84]).
neighbors(86,[81,82,85]).
neighbors(87,[82,83,86]).
neighbors(88,[70,71,83,87]).
neighbors(89,[59,60,84]).
neighbors(90,[84,85,89]).
neighbors(91,[85,86,90]).
neighbors(92,[86,87,91]).
neighbors(93,[87,88,92]).
neighbors(94,[71,72,88,93]).
neighbors(95,[58,59,89]).
neighbors(96,[89,90,95]).
neighbors(97,[90,91,96]).
neighbors(98,[91,92,97]).
neighbors(99,[92,93,98]).
neighbors(100,[93,94,99]).
neighbors(101,[72,73,94,100]).
neighbors(102,[2,57,58,95]).
neighbors(103,[2,95,96,102]).
neighbors(104,[2,96,97,103]).
neighbors(105,[2,97,98,104]).
neighbors(106,[2,98,99,105]).
neighbors(107,[99,100,106]).
neighbors(108,[100,101,107]).
neighbors(109,[1,73,101,108]).
neighbors(110,[1,2,106,107,108,109]).
