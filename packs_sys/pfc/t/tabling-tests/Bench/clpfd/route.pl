%------------------------------------------------------------
%   File   : route.pl
%   Author : Neng-Fa ZHOU
%   Date   : 1996-1997
%   Purpose: A multi-layer channel router in CLP(FD)
%            written originally for B-Prolog (2.1)
%            should be easily ported to Eclipse and SICStus
/*********************************************************************
Problem Description:
VLSI layout design consists of two phases: the first phase, called
placement, determines the positions of the modules on the VLSI chip,
and the second phase, called routing, connects the modules with
wiring. Channel routing is a kind of routing where the routing area is
restricted to a rectangular channel. A channel consists of two
parallel rows with terminals on them. A connection requirement, called
a net, specifies the terminals that must be interconnected through a
routing path. A channel routing problem is to find routing paths for a
given set of nets in a given channel such that no paths overlap each
other.

Use:
   Vars in L..U    : declaration of domain variables
   Var notin L..U  : equal to (Var #<L; Var #> U)
   indomain(X)     : enumerate X
   X #\= Y         : inequality constraint
   freeze(X,G)     : delay G until X is instantiated

Acknowledgement:
Part of the work was supported by AITEC.

References:
Simonis, H., Channel Routing Seen as a Constraint Problem, Tech. Rep., 
TR-LP-51, ECRC, Munich, July, 1990. 

Huitoze, S.L. and Dechier D., Channel Routing with clp(FD), Proc. of 
2nd PACT, 1996.

Zhou, N.F., A Logic Programming Approach to Channel Routing, Proc. 
12th ICLP, Kanazawa, MIT Press, 159-173, 1995.

Zhou, N.F., Channel Routing with Constraint Logic Programming and Delay,
Proc. 9th IEA-AIE, Fukuoka, Gordon and Breach Pub. 383-388, 1996.
*********************************************************************/
go1:- % one horizontal layer channel 
    N=72, L=1, T=28, C=174, 
    main(N,L,T,C).

go2:- % two horizontal layer channel 
    N=72, L=2, T=10, C=174, 
    main(N,L,T,C).

go3:- % three horizontal layer channel 
    N=72, L=3, T=7, C=174, 
    main(N,L,T,C).

go4:- % four horizontal layer channel 
    N=72, L=4, T=5, C=174, 
    main(N,L,T,C).

main(N,L,T,C):-
    statistics(runtime,[Start|_]),
    route(N,L,T,C,Start,Vector),
    statistics(runtime,[End|_]),
    write(output),nl,
    output(N,L,T,C,Vector),
    Time is End-Start,
    write('%execution time go1='), write(Time), write(' milliseconds'),nl,
    told.
    
route(N,L,T,C,Start,Vector):-
    functor(Vector,nets,N),  % N<256 in some systems
    build_net_vector(1,N,Vector), 
    functor(G,graphs,N),      % generate constraint graphs Gv and Gh
    build_constraint_graphs(N,Vector,G),
    compute_depth(1,N,Vector,G),
    Vector=..[_|Nets],
    order_nets(Nets,OrdNets,G),
    extract_domain_vars(OrdNets,Vars),
    TotalTracks is L*T-1,
    Vars in 0..TotalTracks,
    generate_constraints(1,N,Vector,G,L,T), % generate constraints from Gv and Gh
    labeling_ff(Vars).

/*****************************************************************
Each net is represented as a term:

    net(N0,Head,Tail,Terminals,Track,Depth)

where N0 is the number of the net, Head is the left-most terminal 
Number, Tail is the right-most terminal Number, Terminals is a set 
of terminals in the net, and Track is a domain variable.
*****************************************************************/
build_net_vector(N0,N,Vector):-
    N0>N,!.
build_net_vector(N0,N,Vector):-
    net(N0,Terminals),
    net_head(Terminals,Head),
    net_tail(Terminals,Tail),
    Var=net(N0,Head,Tail,Terminals,Track,Depth),
    arg(N0,Vector,Var),
    N1 is N0+1,
    build_net_vector(N1,N,Vector).

compute_depth(N0,N,Vector,G):-
    N0>N,!.
compute_depth(N0,N,Vector,G):-
    arg(N0,G,Node),
    compute_depth_node(Node,Vector,G,_),
    N1 is N0+1,
    compute_depth(N1,N,Vector,G).

compute_depth_node(node(N,Tag,As,Bs,Hs),Vector,G,Dep):-
    arg(N,Vector,Net),
    depth_field(Net,Depth),
    nonvar(Depth),!,Dep=Depth.
compute_depth_node(node(N,Tag,[],Bs,Hs),Vector,G,Dep):-!,
    arg(N,Vector,Net),
    depth_field(Net,0),
    Dep=0.
compute_depth_node(node(N,Tag,As,Bs,Hs),Vector,G,Dep):-
    compute_depth_nodes(As,0,Depth,Vector,G),
    Depth1 is Depth+1,
    arg(N,Vector,Net),
    depth_field(Net,Depth1),
    Dep=Depth1.

compute_depth_nodes([],Depth0,Depth,Vector,G):-
    Depth=Depth0.
compute_depth_nodes([Node|As],Depth0,Depth,Vector,G):-
    compute_depth_node(Node,Vector,G,NodeDepth),
    (NodeDepth>Depth0->Depth1=NodeDepth; Depth1=Depth0),
    compute_depth_nodes(As,Depth1,Depth,Vector,G).

/*****************************************************************
Order nets based on the constraint graphs
*****************************************************************/
order_nets([],OrdNets,G):-!,
    OrdNets=[].
order_nets(Nets,OrdNets,G):-
    choose_net(Nets,Net,RestNets,G),
    remove_net(Net,G),
    OrdNets=[Net|OrdNets1],
    order_nets(RestNets,OrdNets1,G).

choose_net([Net|Nets],BestNet,Rest,G):-
    evaluate_net(Net,EValue,G),
    choose_net(Nets,Net,EValue,BestNet,Rest,G).

choose_net([],Net,EValue,BestNet,Rest,G):-
    BestNet=Net,
    Rest=[].
choose_net([Net|Nets],Net1,EValue1,BestNet,Rest,G):-
    evaluate_net(Net,EValue,G),
    compare_net(Net1,EValue1,Net,EValue,GoodNet,GoodEValue,BadNet),
    Rest=[BadNet|Rest1],
    choose_net(Nets,GoodNet,GoodEValue,BestNet,Rest1,G).

compare_net(Net1,EValue1,Net,EValue,GoodNet,GoodEValue,BadNet):-
    eval_ge(EValue1,EValue),!,
    GoodNet=Net1,
    GoodEValue=EValue1,
    BadNet=Net.
compare_net(Net1,EValue1,Net,EValue,GoodNet,GoodEValue,BadNet):-
    GoodNet=Net,
    GoodEValue=EValue,
    BadNet=Net1.

eval_ge([],[]):-!.
eval_ge([X|Xs],[Y|Ys]):-
    X>Y,!.
eval_ge([X|Xs],[Y|Ys]):-
    X=:=Y,!,
    eval_ge(Xs,Ys).

% 1: select first open nets, i.e., nets at the bottom of Gv
% 2: select first those nets lie deep in Gv
% 3: select first those nets with the greatest degree in Gv 
% 4: select first those nets with the greatest degree in Gh
evaluate_net(Net,EValue,G):-
    arg(1,Net,N),
    is_open(N,Open,G),
    depth_field(Net,Depth),
    gv_degree(N,G,Degree1),
    gh_degree(N,G,Degree2),
    EValue=[Open,Depth,Degree1,Degree2].

is_open(N,Open,G):-
    arg(N,G,node(_,Tag,As,Bs,Hs)),
    empty_nodes(Bs),!,
    Open=1.
is_open(N,Open,G):-
    Open=0.

empty_nodes([]).
empty_nodes([node(N,Tag,_,_,_)|Nodes]):-
    var(Tag),!,fail.
empty_nodes([Node|Nodes]):-
    empty_nodes(Nodes).

gv_degree(N,G,Degree):-
    arg(N,G,node(_,_,As,Bs,Hs)),
    degree(Bs,0,Degree).

gh_degree(N,G,Degree):-
    arg(N,G,node(_,_,As,Bs,Hs)),
    degree(Hs,0,Degree).

degree([],N0,N):-N=N0.
degree([node(_,Tag,_,_,_)|Nodes],N0,N):-
    var(Tag),!,
    N1 is N0+1,
    degree(Nodes,N1,N).
degree([Node|Nodes],N0,N):-
    degree(Nodes,N0,N).

/********************************************************************
The constraint graphs Gv and Gh are represented as a functor whose I'th
argument has the form:

    node(I,Tag,As,Bs,Hs)

where As is the list of nets lying directly above I and Bs is the list
of nets lying directly below I in Gv, Hs is a list of nets connected to
I in Gh, and Tag is used to indicate whether the net has been removed 
from the graphs.
*********************************************************************/
build_constraint_graphs(N,Vector,G):-
    build_constraint_graphs(1,N,Vector,G).

build_constraint_graphs(N0,N,Vector,G):-
    N0>N,!.
build_constraint_graphs(N0,N,Vector,G):-
    arg(N0,Vector,Net0),
    Node=node(N0,Tag,As,Bs,Hs),
    compute_As_Bs_Hs(N0,Net0,1,N,Vector,G,[],As,[],Bs,[],Hs),
    arg(N0,G,Node),
    N1 is N0+1,
    build_constraint_graphs(N1,N,Vector,G).

compute_As_Bs_Hs(N,Net,From,To,Vector,G,As0,As,Bs0,Bs,Hs0,Hs):-
    From>To,!,
    As=As0, Bs=Bs0, Hs=Hs0.
compute_As_Bs_Hs(N,Net,From,To,Vector,G,As0,As,Bs0,Bs,Hs0,Hs):-
    From=:=N,!,
    From1 is From+1,
    compute_As_Bs_Hs(N,Net,From1,To,Vector,G,As0,As,Bs0,Bs,Hs0,Hs).
compute_As_Bs_Hs(N,Net,From,To,Vector,G,As0,As,Bs0,Bs,Hs0,Hs):-
    arg(From,Vector,Net0),
    arg(From,G,Node),
    (above(Net0,Net)->As1=[Node|As0];As1=As0),
    (above(Net,Net0)->Bs1=[Node|Bs0];Bs1=Bs0),
    (h_overlap(Net,Net0)->Hs1=[Node|Hs0];Hs1=Hs0),
    From1 is From+1,
    compute_As_Bs_Hs(N,Net,From1,To,Vector,G,As1,As,Bs1,Bs,Hs1,Hs).

%%%%%%%%%%%%%%%%%%%%%%% generate constraints %%%%%%%%%%%%%%%%%%%%%%% 
generate_constraints(N0,N,Vector,G,MaxL,MaxT):-
    N0>N,!.
generate_constraints(N0,N,Vector,G,MaxL,MaxT):-
    arg(N0,G,node(_,_,As,Bs,Hs)),
    arg(N0,Vector,Net0),
    track_field(Net0,Track0),
    (MaxL=:=1->
     global_vertical_constraints(Track0,1,Bs,Vector)
      ;vertical_constraints(Track0,Bs,Vector,MaxT)),
    horizontal_constraints(Track0,Hs,Vector),
    N1 is N0+1,
    generate_constraints(N1,N,Vector,G,MaxL,MaxT).

% if X>Y and Y>Z then X>Z+1
global_vertical_constraints(Track0,I,[],Vector).
global_vertical_constraints(Track0,I,[Node|Nodes],Vector):-
    global_vertical_constraint(Track0,I,Node,Vector),
    global_vertical_constraints(Track0,I,Nodes,Vector).

global_vertical_constraint(Track0,I,node(N,_,As,Bs,Hs),Vector):-
    arg(N,Vector,Net),
    track_field(Net,Track),
    Track0#>=Track+I,
    I1 is I+1,
    global_vertical_constraints(Track0,I1,Bs,Vector).

vertical_constraints(Track0,[],Vector,MaxT).
vertical_constraints(Track0,[node(N,_,_,_,_)|Ns],Vector,MaxT):-
    arg(N,Vector,Net),
    track_field(Net,Track),
    above_constraints(Track0,Track,MaxT),
    vertical_constraints(Track0,Ns,Vector,MaxT).

horizontal_constraints(Track0,[],Vector).
horizontal_constraints(Track0,[node(N,_,_,_,_)|Ns],Vector):-
    arg(N,Vector,Net),
    track_field(Net,Track),
    Track0#\=Track,
    horizontal_constraints(Track0,Ns,Vector).


above_constraints(T1,T0,MaxT):-
    freeze(T1,above_constraints_a(T1,T0,MaxT)), % T1 is instantiated  
    freeze(T0,above_constraints_b(T1,T0,MaxT)).

above_constraints_a(T1,T0,MaxT):-
    Upper is (T1//MaxT)*MaxT+MaxT-1,
    T0 notin T1..Upper.

above_constraints_b(T1,T0,MaxT):-
    Low is (T0//MaxT)*MaxT,
    T1 notin Low..T0.

%%%%%%%%%%%%%%%%% Other library routines %%%%%%%%%%%%%%%%%%%%%%%
h_overlap(Net0,Net):-
    head_field(Net,H),
    tail_field(Net,T),
    head_field(Net0,H0),
    tail_field(Net0,T0),
    h_overlap(H0,T0,H,T).

h_overlap(H0,T0,H,T):-
    connected(H0,T0,H,T),!,fail.
h_overlap(H0,T0,H,T):-
    arg(1,H0,I0),
    arg(1,T0,J0),
    arg(1,H,I),
    arg(1,T,J),
    (I>=I0,I=<J0; I0>=I,I0=<J).

% if two nets are connected, then they are actually on net.
connected(t(I),_,H,t(I)):-!.
connected(b(I),_,H,b(I)):-!.
connected(_,t(I),t(I),_):-!.
connected(_,b(I),b(I),_):-!.

above(Var1,Var2):-
    terms_field(Var1,Terms1),
    terms_field(Var2,Terms2),
    above2(Terms1,Terms2).

above2([t(K)|_],[b(K)|_]):-!.
above2(Ts1,Ts2):-
    [T1|Ts3]=Ts1,
    [T2|Ts4]=Ts2,
    arg(1,T1,C1),
    arg(1,T2,C2),
    (C1<C2 -> above2(Ts3,Ts2); above2(Ts1,Ts4)).

extract_domain_vars([],Vars):-Vars=[].
extract_domain_vars([Net|Nets],Vars):-
    track_field(Net,Track),
    Vars=[Track|Vars1],
    extract_domain_vars(Nets,Vars1).
    
net_head([H|_],H1):-H1=H.

net_tail([Tail],Tail1):-!,Tail1=Tail.
net_tail([_|Tail],Tail1):-
    net_tail(Tail,Tail1).

head_field(Var,Head):-
    arg(2,Var,Head).

tail_field(Var,Tail):-
    arg(3,Var,Tail).

terms_field(Var,Terms):-
    arg(4,Var,Terms).

track_field(Var,Track):-
    arg(5,Var,Track).

depth_field(Var,Depth):-
    arg(6,Var,Depth).

remove_net(Net,G):-
    arg(1,Net,N),
    arg(N,G,Node),
    Node=node(_,1,_,_,_).

%%%%%%%%%%%%%%%%%%%%%%% output %%%%%%%%%%%%%%%%%%%%%%% 
output(N,L,T,C,Vector):-
    name(N,S1),
    name(L,S2),
    name(sol,S0),
    append(S0,[0'_|S1],[0'_|S2],S012),
    append(S012,".tex",S),
    name(OutFile,S),
    write('solution stored in '), write(OutFile),nl,
    tell(OutFile),
    functor(Vector1,nets,N),
    decode_layer_track(N,Vector,Vector1,L,T),
    output(L,T,C,Vector1),
%    output_data(1,N,Vector1),
    told.

output_data(N0,N,Vector):-
    N0>N,!.
output_data(N0,N,Vector):-
    arg(N0,Vector,Net),
    write(Net),write('.'),nl,
    N1 is N0+1,
    output_data(N1,N,Vector).


append(L1,L2,L3,L):-
    append(L1,L2,L12),
    append(L12,L3,L).

decode_layer_track(N,Vector,Vector1,MaxLayer,MaxTrack):-
    N=:=0,!.
decode_layer_track(N,Vector,Vector1,MaxLayer,MaxTrack):-
    arg(N,Vector,Var),
    Var=net(N0,Head,Tail,Terminals,TrackValue,Depth),
    NewVar=net(N0,Head,Tail,Terminals,NewLayer,NewTrack),
    NewLayer is TrackValue//MaxTrack+1,
    NewTrack is TrackValue-(TrackValue//MaxTrack)*MaxTrack+1,
    arg(N,Vector1,NewVar),
    N1 is N-1,
    decode_layer_track(N1,Vector,Vector1,MaxLayer,MaxTrack).

output(MaxLayer,MaxTrack,MaxTerm,Dvars):-
    X is MaxTerm+1,
    Y is MaxLayer*(MaxTrack+3)+1,
    write_line(['\documentstyle[11pt,epsf]{article}']),
    write_line(['\topmargin=-0.5cm']),
    write_line(['\oddsidemargin=-1.5cm']),
    write_line(['\textheight=23cm \textwidth=20cm']),
    write_line(['\begin{document}']),
    write_line(['\begin{figure}[hbt]']),
    write_line(['\setlength{\unitlength}{1.0mm}']),
    write_line(['\begin','{',picture,'}','(',X,',',Y,')','(',0,',',0,')']),
    output_rows(MaxLayer,MaxTrack,MaxTerm),
    functor(Dvars,F,N),
    output_nets(MaxTrack,Dvars,1,N),
    write_line(['\end{picture}']),
    write_line(['\end{figure}']),
    write_line(['\end{document}']).

output_rows(Layer,MaxTrack,MaxTerm):-
    Layer=<0,!.
output_rows(Layer,MaxTrack,MaxTerm):-
    Top is Layer*(MaxTrack+3),
    Bottom is (Layer-1)*(MaxTrack+3)+2,
    Len is MaxTerm+1,
    write_line(['\put','(',0,',',Top,')','{\line(1,0){',Len,'}}']),
    write_line(['\multiput(1,',Top,')(1,0){',MaxTerm,'}{\circle*{.2}}']),
    write_line(['\put','(',0,',',Bottom,')','{\line(1,0){',Len,'}}']),
    write_line(['\multiput(1,',Bottom,')(1,0){',MaxTerm,'}{\circle*{.2}}']),
    Layer1 is Layer-1,
    output_rows(Layer1,MaxTrack,MaxTerm).

output_nets(MaxTrack,Dvars,N0,N):-
    N0>N,!.
output_nets(MaxTrack,Dvars,N0,N):-
    arg(N0,Dvars,Dvar),
    Dvar=net(N0,Head,Tail,Terms,Layer,Track),
    Y is (Layer-1)*(MaxTrack+3)+2+Track,
    arg(1,Head,LeftT),
    arg(1,Tail,RightT),
    Length is RightT-LeftT,
    write_line(['\put(',LeftT,',',Y,'){\line(1,0){',Length,'}}']),
    Top is Layer*(MaxTrack+3),
    Bottom is (Layer-1)*(MaxTrack+3)+2,
    Top_len is MaxTrack-Track+1,
    output_terminals(Top_len,Track,Y,Terms),
    N1 is N0+1,
    output_nets(MaxTrack,Dvars,N1,N).

output_terminals(Top_len,Bottom_len,Y,[]):-!.
output_terminals(Top_len,Bottom_len,Y,[t(X)|Terminals]):-
    (X=:=0->true;
    write_line(['\put(',X,',',Y,'){\line(0,1){',Top_len,'}}']),
    write_line(['\put(',X,',',Y,'){\circle*{0.2}}'])),
    output_terminals(Top_len,Bottom_len,Y,Terminals).
output_terminals(Top_len,Bottom_len,Y,[b(X)|Terminals]):-
    (X=:=0->true;
     write_line(['\put(',X,',',Y,'){\line(0,-1){',Bottom_len,'}}']),
     write_line(['\put(',X,',',Y,'){\circle*{0.2}}'])),
    output_terminals(Top_len,Bottom_len,Y,Terminals).

write_line([]):-
    nl.
write_line([X|L]):-
    write(X),
    write_line(L).

%%%%%%%%%%%%%% Nets in Deutsch's difficult problem %%%%%%%%%%%%%%%%%%%%%
net(1,N):-N=[t(5),t(28)].
net(2,N):-N=[t(39),t(67)].
net(3,N):-N=[t(74),t(117)].
net(4,N):-N=[b(145),t(151)].
net(5,N):-N=[t(161),t(163)].
net(6,N):-N=[b(62),t(77)].
net(7,N):-N=[t(78),t(82)].
net(8,N):-N=[b(90),t(110),b(118),t(123)].
net(9,N):-N=[t(139),t(141),t(144),b(151),t(174)].
net(10,N):-N=[t(106),t(130),t(132),b(161),t(168)].
net(11,N):-N=[t(70),t(98),t(100)].
net(12,N):-N=[t(109),t(131),t(135),b(141),t(153),t(155),t(171)].
net(13,N):-N=[t(24),b(37),t(53),b(55),t(60),t(92),b(110)].
net(14,N):-N=[b(117),t(166)].
net(15,N):-N=[t(12),t(19)].
net(16,N):-N=[t(22),b(39),t(51),t(58),t(94),b(97),b(106),b(108),b(135),b(144),b(155),b(166)].
net(17,N):-N=[t(6),t(13),b(22),t(30),t(34),t(36),t(40)].
net(18,N):-N=[b(78),t(147),t(149)].
net(19,N):-N=[t(159),b(165)].
net(20,N):-N=[t(0),t(21),b(40),t(48),t(50),t(57),t(95)].
net(21,N):-N=[b(98),t(119)].
net(22,N):-N=[t(120),t(154),t(156)].
net(23,N):-N=[t(2),b(13)].
net(24,N):-N=[t(20),b(57),t(68),t(76),t(111),b(119),t(122)].
net(25,N):-N=[t(128),b(149),b(160),t(167)].
net(26,N):-N=[b(2),b(5),t(11),t(14),t(46),t(49)].
net(27,N):-N=[t(66),b(70)].
net(28,N):-N=[b(95),t(105),b(113),t(124),b(128)].
net(29,N):-N=[t(138),t(140)].
net(30,N):-N=[t(7),b(14)].
net(31,N):-N=[b(7),b(11),t(15),t(16),b(19)].
net(32,N):-N=[t(23),b(24)].
net(33,N):-N=[b(66),b(68),t(83),b(92),t(99),t(101),b(102)].
net(34,N):-N=[t(3),b(16),b(21),b(32),b(58),t(69),t(75),b(77),t(112),b(120),t(121)].
net(35,N):-N=[b(124),t(129)].
net(36,N):-N=[t(134),b(140),b(150),t(162),t(164),t(173)].
net(37,N):-N=[t(73),b(75)].
net(38,N):-N=[t(87),b(94),b(101),t(114),t(116)].
net(39,N):-N=[t(136),b(154)].
net(40,N):-N=[t(44),b(60),t(65),b(73),t(79),t(104),b(112),t(125),b(129)].
net(41,N):-N=[b(79),t(93)].
net(42,N):-N=[b(114),t(133)].
net(43,N):-N=[b(134),t(158)].
net(44,N):-N=[b(65),b(74)].
net(45,N):-N=[t(84),t(86),b(93),t(146),t(148)].
net(46,N):-N=[t(25),b(36),t(54),t(61),t(91),b(99),b(104),b(133),b(142),b(146),b(153),b(164)].
net(47,N):-N=[t(52),b(54)].
net(48,N):-N=[t(1),b(50),b(52)].
net(49,N):-N=[b(1),t(8),t(29),t(41),b(44),b(46),t(63)].
net(50,N):-N=[t(33),t(35)].
net(51,N):-N=[t(38),t(45),b(61),t(71),b(86)].
net(52,N):-N=[t(127),t(143),b(159)].
net(53,N):-N=[t(10),t(27),b(29),t(43)].
net(54,N):-N=[t(47),b(67),b(71),t(81),b(82),b(84),t(89),b(91)].
net(55,N):-N=[b(127),t(172)].
net(56,N):-N=[b(6),b(10),t(18)].
net(57,N):-N=[t(31),b(38)].
net(58,N):-N=[b(41),t(59)].
net(59,N):-N=[b(63),b(69),t(72),b(87)].
net(60,N):-N=[t(88),b(89)].
net(61,N):-N=[t(96),b(105)].
net(62,N):-N=[t(4),b(15),b(20),b(31),b(59),t(64),b(72),t(80),t(103),b(111),t(126),b(130)].
net(63,N):-N=[b(138),b(168),t(170)].
net(64,N):-N=[b(4),t(9),t(42),b(49),b(51),t(56)].
net(65,N):-N=[b(64),t(85),b(88)].
net(66,N):-N=[b(158),t(169)].
net(67,N):-N=[b(3),b(9),b(12),t(17)].
net(68,N):-N=[b(23),b(43),b(45)].
net(69,N):-N=[b(56),b(81),b(83)].
net(70,N):-N=[b(96),b(107),b(109),b(136),b(143),b(156),b(167)].
net(71,N):-N=[b(8),b(17)].
net(72,N):-N=[b(18),b(26),b(28)].
