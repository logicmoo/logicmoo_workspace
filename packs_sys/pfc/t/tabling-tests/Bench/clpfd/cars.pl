/*-------------------------------------------------------------------------*/
/* Benchmark (Finite Domain)            INRIA Rocquencourt - ChLoE Project */
/*                                                                         */
/* Name           : cars.pl                                                */
/* Title          : car sequencing problem                                 */
/* Original Source: Dincbas, Simonis and Van Hentenryck                    */
/* Adapted by     : Daniel Diaz - INRIA France                             */
/* Date           : September 1992                                         */
/*                                                                         */
/* Car sequencing problem with 10 cars                                     */
/* Solution:                                                               */
/*   [1,2,6,3,5,4,4,5,3,6]                                                 */
/*   [1,3,6,2,5,4,3,5,4,6]                                                 */
/*   [1,3,6,2,6,4,5,3,4,5]                                                 */
/*   [5,4,3,5,4,6,2,6,3,1]                                                 */
/*   [6,3,5,4,4,5,3,6,2,1]                                                 */
/*   [6,4,5,3,4,5,2,6,3,1]                                                 */
/*                                                                         */
/*-------------------------------------------------------------------------*/


go:-
    statistics(runtime,_),
    top,
    statistics(runtime,[_,Y]),
    write('time : '), write(Y), nl.

top:-
    cars(L),
%    write(L),nl,
    fail.
top.

cars(X):-
	X=[X1,X2,X3,X4,X5,X6,X7,X8,X9,X10],

	Y=[O11,O12,O13,O14,O15,
	O21,O22,O23,O24,O25,
	O31,O32,O33,O34,O35,
	O41,O42,O43,O44,O45,
	O51,O52,O53,O54,O55,
	O61,O62,O63,O64,O65,
	O71,O72,O73,O74,O75,
	O81,O82,O83,O84,O85,
	O91,O92,O93,O94,O95,
	O101,O102,O103,O104,O105],

	L1=[1,0,0,0,1,1],
	L2=[0,0,1,1,0,1],
	L3=[1,0,0,0,1,0],
	L4=[1,1,0,1,0,0],
	L5=[0,0,1,0,0,0],

	domain(Y,0,1),
	domain(X,1,6),

	atmost(1,X,1),
	atmost(1,X,2),
	atmost(2,X,3),
	atmost(2,X,4),
	atmost(2,X,5),
	atmost(2,X,6),

%            card(1,X,1),
%            card(1,X,2),
%            card(2,X,3),
%            card(2,X,4),
%            card(2,X,5),
%            card(2,X,6),            

	element(X1,L1,O11),
	element(X1,L2,O12),
	element(X1,L3,O13),
	element(X1,L4,O14),
	element(X1,L5,O15),

	element(X2,L1,O21),
	element(X2,L2,O22),
	element(X2,L3,O23),
	element(X2,L4,O24),
	element(X2,L5,O25),

	element(X3,L1,O31),
	element(X3,L2,O32),
	element(X3,L3,O33),
	element(X3,L4,O34),
	element(X3,L5,O35),

	element(X4,L1,O41),
	element(X4,L2,O42),
	element(X4,L3,O43),
	element(X4,L4,O44),
	element(X4,L5,O45),

	element(X5,L1,O51),
	element(X5,L2,O52),
	element(X5,L3,O53),
	element(X5,L4,O54),
	element(X5,L5,O55),

	element(X6,L1,O61),
	element(X6,L2,O62),
	element(X6,L3,O63),
	element(X6,L4,O64),
	element(X6,L5,O65),

	element(X7,L1,O71),
	element(X7,L2,O72),
	element(X7,L3,O73),
	element(X7,L4,O74),
	element(X7,L5,O75),

	element(X8,L1,O81),
	element(X8,L2,O82),
	element(X8,L3,O83),
	element(X8,L4,O84),
	element(X8,L5,O85),

	element(X9,L1,O91),
	element(X9,L2,O92),
	element(X9,L3,O93),
	element(X9,L4,O94),
	element(X9,L5,O95),

	element(X10,L1,O101),
	element(X10,L2,O102),
	element(X10,L3,O103),
	element(X10,L4,O104),
	element(X10,L5,O105),

	1 #>= O11+O21,
	1 #>= O21+O31,
	1 #>= O31+O41,
	1 #>= O41+O51,
	1 #>= O51+O61,
	1 #>= O61+O71,
	1 #>= O71+O81,
	1 #>= O81+O91,
	1 #>= O91+O101,
	2 #>= O12+O22+O32,
	2 #>= O22+O32+O42,
	2 #>= O32+O42+O52,
	2 #>= O42+O52+O62,
	2 #>= O52+O62+O72,
	2 #>= O62+O72+O82,
	2 #>= O72+O82+O92,
	2 #>= O82+O92+O102,
	1 #>= O13+O23+O33,
	1 #>= O23+O33+O43,
	1 #>= O33+O43+O53,
	1 #>= O43+O53+O63,
	1 #>= O53+O63+O73,
	1 #>= O63+O73+O83,
	1 #>= O73+O83+O93,
	1 #>= O83+O93+O103,
	2 #>= O14+O24+O34+O44+O54,
	2 #>= O24+O34+O44+O54+O64,
	2 #>= O34+O44+O54+O64+O74,
	2 #>= O44+O54+O64+O74+O84,
	2 #>= O54+O64+O74+O84+O94,
	2 #>= O64+O74+O84+O94+O104,
	1 #>= O15+O25+O35+O45+O55,
	1 #>= O25+O35+O45+O55+O65,
	1 #>= O35+O45+O55+O65+O75,
	1 #>= O45+O55+O65+O75+O85,
	1 #>= O55+O65+O75+O85+O95,
	1 #>= O65+O75+O85+O95+O105,



% redundant constraints


	O11+O21+O31+O41+O51+O61+O71+O81 #>= 4,
	O11+O21+O31+O41+O51+O61         #>= 3,
	O11+O21+O31+O41                 #>= 2,
	O11+O21                         #>= 1,

	O12+O22+O32+O42+O52+O62+O72     #>= 4,
	O12+O22+O32+O42                 #>= 2,
	O12                             #>= 0,

	O13+O23+O33+O43+O53+O63+O73     #>= 2,
	O13+O23+O33+O43                 #>= 1,
	O13                             #>= 0,

	O14+O24+O34+O44+O54             #>= 2,

	O15+O25+O35+O45+O55             #>= 1,

    labeling(X).

    

