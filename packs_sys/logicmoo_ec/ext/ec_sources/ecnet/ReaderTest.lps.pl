% From E: 
% 
% ':-'(call_pel_directive(translate(unskipped,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReaderTest.lps.pl'))).
:- call_pel_directive(translate(unskipped,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReaderTest.lps.pl')).
:-include(library('ec_planner/ec_test_incl')).
:-expects_dialect(lps).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rain.e',41).
% From E: 
% 
% ':-'(call_pel_directive(translate(begining,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReaderTest.lps.pl'))).
:- call_pel_directive(translate(begining,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReaderTest.lps.pl')).
% Fri, 26 Mar 2021 01:06:04 GMT File: <stream>(0x55556759a500)% [physobj1,physobj2]
% !(physobj1=Pen1 & physobj2=Desk1) &
% !(physobj1=Paper1 & physobj2=Desk1) ->
% !HoldsAt(On(physobj1, physobj2),0).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReaderTest.e',1).
% From E: 
% 
% '->'(
%    ','(
%       not(','(
%              Physobj1=pen1, 
%              Physobj2=desk1)), 
%       not(','(
%              Physobj1=paper1, 
%              Physobj2=desk1))), 
%    holds(
%       not(on(Physobj1,Physobj2)), 0)).
on(Physobj1, Physobj2)at 0 if equals(Physobj1, pen1), equals(Physobj2, desk1);equals(Physobj1, paper1), equals(Physobj2, desk1).
 %  l_int(holds(on(Physobj1, Physobj2), 0), [(equals(Physobj1, pen1), equals(Physobj2, desk1);equals(Physobj1, paper1), equals(Physobj2, desk1))]).
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReaderTest.e',7).
% [agent,book,page,time]
% Happens(BookTurnPageTo(agent,book,page),time) ->
% HoldsAt(Awake(agent),time) &
% ({page1} page1 != page & HoldsAt(BookIsOpenTo(book,page1),time)) &
% HoldsAt(Holding(agent,book),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReaderTest.e',7).
% From E: 
% 
% '->'(
%    happens(
%       bookTurnPageTo(Agent,Book,Page), 
%       Time), 
%    ','(
%       holds(
%          awake(Agent), 
%          Time), 
%       ','(
%          thereExists(Page1, 
%             ','(
%                Page1\=Page, 
%                holds(
%                   bookIsOpenTo(Book,Page1), 
%                   Time))), 
%          holds(
%             holding(Agent,Book), 
%             Time)))).
(   awake(Agent)at Time,
    thereExists(Page1,
                 ({dif(Page1, Page)}, bookIsOpenTo(Book, Page1)at Time)),
    holding(Agent, Book)at Time
;   not happens(bookTurnPageTo(Agent, Book, Page), Time)
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReaderTest.e',7).

 /*  (   at(awake(Agent), Time),
         thereExists(Page1,
                      ({dif(Page1, Page)}, at(bookIsOpenTo(Book, Page1), Time))),
         at(holding(Agent, Book), Time)
     ;   not(happens(bookTurnPageTo(Agent, Book, Page), Time))
     ).
 */
 %  % =================================.


% [book,time]
% HoldsAt(BookClosed(book),time) ->
% ! {page} HoldsAt(BookIsOpenTo1(book,page),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReaderTest.e',14).
% From E: 
% 
% '->'(
%    holds(
%       bookClosed(Book), 
%       Time), 
%    not(thereExists(Page, 
%           holds(
%              bookIsOpenTo1(Book,Page), 
%              Time)))).
thereExists(Page, bookIsOpenTo1(Book, Page)at Time)if not bookClosed(Book)at Time.

 /*  if(thereExists(Page,
     	       at(bookIsOpenTo1(Book,Page),Time)),
        at(not(bookClosed(Book)),Time)).
 */
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReaderTest.e',18).
% [book,time]
% HoldsAt(BookClosed(book),time) ->
% {page}%  ! HoldsAt(BookIsOpenTo2(book,page),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReaderTest.e',20).
% From E: 
% 
% exists(Page, 
%    '->'(
%       holds(
%          bookClosed(Book), 
%          Time), 
%       holds(
%          not(bookIsOpenTo2(Book,Page)), 
%          Time))).
exists(Page,  (not bookIsOpenTo2(Book, Page)at Time;not bookClosed(Book)at Time)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReaderTest.e',20).

 /*  exists(Page,
       (   at(not(bookIsOpenTo2(Book, Page)), Time)
        ;   at(not(bookClosed(Book)), Time)
        )).
 */
 %  % =================================.
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReaderTest.e',20).
% From E: 
% 
% ':-'(call_pel_directive(translate(ending,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReaderTest.lps.pl'))).
:- call_pel_directive(translate(ending,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReaderTest.lps.pl')).
