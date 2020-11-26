:- module(nlkb7166,[
   % assertion_lifting_consequent/2,assertion_lifting/2,assertion_mt/2,
   acnl/3,acnl/4,acnl/5,acnl/6,
   acnl/7,acnl/8,acnl/9,acnl/10,
   acnl/11,acnl/12,acnl/13]).


:- public((acnl/3,acnl/4,acnl/5,acnl/6,acnl/7,acnl/8,acnl/9,acnl/10,acnl/11,acnl/12,acnl/13)).
:- export((acnl/3,acnl/4,acnl/5,acnl/6,acnl/7,acnl/8,acnl/9,acnl/10,acnl/11,acnl/12,acnl/13)).
:- multifile((acnl/3,acnl/4,acnl/5,acnl/6,acnl/7,acnl/8,acnl/9,acnl/10,acnl/11,acnl/12,acnl/13)).
:- dynamic((acnl/3,acnl/4,acnl/5,acnl/6,acnl/7,acnl/8,acnl/9,acnl/10,acnl/11,acnl/12,acnl/13)).
:- discontiguous((acnl/3,acnl/4,acnl/5,acnl/6,acnl/7,acnl/8,acnl/9,acnl/10,acnl/11,acnl/12,acnl/13)).

/*
:- public((assertion_lifting_consequent/2,assertion_lifting/2,assertion_mt/2)).
:- export((assertion_lifting_consequent/2,assertion_lifting/2,assertion_mt/2)).
:- multifile((assertion_lifting_consequent/2,assertion_lifting/2,assertion_mt/2)).
:- dynamic((assertion_lifting_consequent/2,assertion_lifting/2,assertion_mt/2)).
:- discontiguous((assertion_lifting_consequent/2,assertion_lifting/2,assertion_mt/2)).
*/

:- set_prolog_flag(double_quotes,string).
:- style_check(-singleton).

:-dynamic(acnl/3).
:-dynamic(acnl/4).
:-dynamic(acnl/5).
:-dynamic(acnl/6).
:-dynamic(acnl/7).
:-dynamic(acnl/8).
:-dynamic(acnl/9).
:-dynamic(acnl/10).
:-dynamic(acnl/11).
:-dynamic(acnl/12).
:-dynamic(acnl/13).
:-dynamic(acnl_implies/3).
:-dynamic(acnl_implies/4).
:-dynamic(acnl_implies/5).
:-dynamic(acnl_implies/6).
:-dynamic(acnl_implies/7).
:-dynamic(acnl_implies/8).
:-dynamic(acnl_implies/9).
:-dynamic(acnl_implies/10).
:-dynamic(acnl_not/2).
:-dynamic(acnl_not/3).
:-dynamic(acnl_not/4).
:-dynamic(acnl_not/5).
:-dynamic(acnl_not/6).
:-dynamic(acnl_not/7).

:- if(exists_source('ac_xnl/ac_xnl_7166_part_000000.nldata')).

:- style_check(-discontiguous).
:- style_check(-singleton).

:- include('ac_xnl/ac_xnl_7166_part_000000.nldata').
:- include('ac_xnl/ac_xnl_7166_part_000100.nldata').
:- include('ac_xnl/ac_xnl_7166_part_000200.nldata').
:- include('ac_xnl/ac_xnl_7166_part_000300.nldata').
:- include('ac_xnl/ac_xnl_7166_part_000400.nldata').
:- include('ac_xnl/ac_xnl_7166_part_000500.nldata').
:- include('ac_xnl/ac_xnl_7166_part_000600.nldata').
:- include('ac_xnl/ac_xnl_7166_part_000700.nldata').
:- include('ac_xnl/ac_xnl_7166_part_000800.nldata').
:- include('ac_xnl/ac_xnl_7166_part_000900.nldata').
:- include('ac_xnl/ac_xnl_7166_part_001000.nldata').
:- include('ac_xnl/ac_xnl_7166_part_001100.nldata').
:- include('ac_xnl/ac_xnl_7166_part_001200.nldata').
:- include('ac_xnl/ac_xnl_7166_part_001300.nldata').
:- include('ac_xnl/ac_xnl_7166_part_001400.nldata').
:- include('ac_xnl/ac_xnl_7166_part_001500.nldata').
:- include('ac_xnl/ac_xnl_7166_part_001600.nldata').
:- include('ac_xnl/ac_xnl_7166_part_001700.nldata').
:- include('ac_xnl/ac_xnl_7166_part_001800.nldata').
:- include('ac_xnl/ac_xnl_7166_part_001900.nldata').
:- include('ac_xnl/ac_xnl_7166_part_002000.nldata').
:- include('ac_xnl/ac_xnl_7166_part_002100.nldata').
:- include('ac_xnl/ac_xnl_7166_part_002200.nldata').
:- include('ac_xnl/ac_xnl_7166_part_002300.nldata').
:- include('ac_xnl/ac_xnl_7166_part_002400.nldata').
:- include('ac_xnl/ac_xnl_7166_part_002500.nldata').
:- include('ac_xnl/ac_xnl_7166_part_002600.nldata').
:- include('ac_xnl/ac_xnl_7166_part_002700.nldata').
:- include('ac_xnl/ac_xnl_7166_part_002800.nldata').
:- include('ac_xnl/ac_xnl_7166_part_002900.nldata').
:- include('ac_xnl/ac_xnl_7166_part_003000.nldata').
:- include('ac_xnl/ac_xnl_7166_part_003100.nldata').
:- include('ac_xnl/ac_xnl_7166_part_003200.nldata').
:- include('ac_xnl/ac_xnl_7166_part_003300.nldata').
:- include('ac_xnl/ac_xnl_7166_part_003400.nldata').
:- include('ac_xnl/ac_xnl_7166_part_003500.nldata').
:- include('ac_xnl/ac_xnl_7166_part_003600.nldata').
:- include('ac_xnl/ac_xnl_7166_part_003700.nldata').
:- include('ac_xnl/ac_xnl_7166_part_003800.nldata').
:- include('ac_xnl/ac_xnl_7166_part_003900.nldata').
:- include('ac_xnl/ac_xnl_7166_part_004000.nldata').
:- include('ac_xnl/ac_xnl_7166_part_004100.nldata').
:- include('ac_xnl/ac_xnl_7166_part_004200.nldata').
:- include('ac_xnl/ac_xnl_7166_part_004300.nldata').
:- include('ac_xnl/ac_xnl_7166_part_004400.nldata').
:- include('ac_xnl/ac_xnl_7166_part_004500.nldata').
:- include('ac_xnl/ac_xnl_7166_part_004600.nldata').
:- include('ac_xnl/ac_xnl_7166_part_004700.nldata').
:- include('ac_xnl/ac_xnl_7166_part_004800.nldata').
:- include('ac_xnl/ac_xnl_7166_part_004900.nldata').
:- include('ac_xnl/ac_xnl_7166_part_005000.nldata').
:- include('ac_xnl/ac_xnl_7166_part_005100.nldata').
:- include('ac_xnl/ac_xnl_7166_part_005200.nldata').
:- include('ac_xnl/ac_xnl_7166_part_005300.nldata').
:- include('ac_xnl/ac_xnl_7166_part_005400.nldata').
:- include('ac_xnl/ac_xnl_7166_part_005500.nldata').
:- include('ac_xnl/ac_xnl_7166_part_005600.nldata').
:- include('ac_xnl/ac_xnl_7166_part_005700.nldata').
:- include('ac_xnl/ac_xnl_7166_part_005800.nldata').
:- include('ac_xnl/ac_xnl_7166_part_005900.nldata').
:- include('ac_xnl/ac_xnl_7166_part_006000.nldata').
:- include('ac_xnl/ac_xnl_7166_part_006100.nldata').
:- include('ac_xnl/ac_xnl_7166_part_006200.nldata').
:- include('ac_xnl/ac_xnl_7166_part_006300.nldata').
:- include('ac_xnl/ac_xnl_7166_part_006400.nldata').
:- include('ac_xnl/ac_xnl_7166_part_006500.nldata').
:- include('ac_xnl/ac_xnl_7166_part_006600.nldata').
:- include('ac_xnl/ac_xnl_7166_part_006700.nldata').
:- include('ac_xnl/ac_xnl_7166_part_006800.nldata').
:- include('ac_xnl/ac_xnl_7166_part_006900.nldata').
:- include('ac_xnl/ac_xnl_7166_part_007000.nldata').
:- include('ac_xnl/ac_xnl_7166_part_007100.nldata').
:- include('ac_xnl/ac_xnl_7166_part_007200.nldata').
:- include('ac_xnl/ac_xnl_7166_part_007300.nldata').
:- include('ac_xnl/ac_xnl_7166_part_007400.nldata').
:- include('ac_xnl/ac_xnl_7166_part_007500.nldata').
:- include('ac_xnl/ac_xnl_7166_part_007600.nldata').
:- include('ac_xnl/ac_xnl_7166_part_007700.nldata').
:- include('ac_xnl/ac_xnl_7166_part_007800.nldata').
:- include('ac_xnl/ac_xnl_7166_part_007900.nldata').
:- include('ac_xnl/ac_xnl_7166_part_008000.nldata').
:- include('ac_xnl/ac_xnl_7166_part_008100.nldata').
:- include('ac_xnl/ac_xnl_7166_part_008200.nldata').
:- include('ac_xnl/ac_xnl_7166_part_008300.nldata').
:- include('ac_xnl/ac_xnl_7166_part_008400.nldata').
:- include('ac_xnl/ac_xnl_7166_part_008500.nldata').
:- include('ac_xnl/ac_xnl_7166_part_008600.nldata').
:- include('ac_xnl/ac_xnl_7166_part_008700.nldata').
:- include('ac_xnl/ac_xnl_7166_part_008800.nldata').
:- include('ac_xnl/ac_xnl_7166_part_008900.nldata').
:- include('ac_xnl/ac_xnl_7166_part_009000.nldata').
:- include('ac_xnl/ac_xnl_7166_part_009100.nldata').
:- include('ac_xnl/ac_xnl_7166_part_009200.nldata').
:- include('ac_xnl/ac_xnl_7166_part_009300.nldata').
:- include('ac_xnl/ac_xnl_7166_part_009400.nldata').
:- include('ac_xnl/ac_xnl_7166_part_009500.nldata').
:- include('ac_xnl/ac_xnl_7166_part_009600.nldata').
:- include('ac_xnl/ac_xnl_7166_part_009700.nldata').
:- include('ac_xnl/ac_xnl_7166_part_009800.nldata').
:- include('ac_xnl/ac_xnl_7166_part_009900.nldata').
:- include('ac_xnl/ac_xnl_7166_part_010000.nldata').
:- include('ac_xnl/ac_xnl_7166_part_010100.nldata').
:- include('ac_xnl/ac_xnl_7166_part_010200.nldata').
:- include('ac_xnl/ac_xnl_7166_part_010300.nldata').
:- include('ac_xnl/ac_xnl_7166_part_010400.nldata').
:- include('ac_xnl/ac_xnl_7166_part_010500.nldata').
:- include('ac_xnl/ac_xnl_7166_part_010600.nldata').
:- include('ac_xnl/ac_xnl_7166_part_010700.nldata').
:- include('ac_xnl/ac_xnl_7166_part_010800.nldata').
:- include('ac_xnl/ac_xnl_7166_part_010900.nldata').
:- include('ac_xnl/ac_xnl_7166_part_011000.nldata').
:- include('ac_xnl/ac_xnl_7166_part_011100.nldata').
:- include('ac_xnl/ac_xnl_7166_part_011200.nldata').
:- include('ac_xnl/ac_xnl_7166_part_011300.nldata').
:- include('ac_xnl/ac_xnl_7166_part_011400.nldata').
:- include('ac_xnl/ac_xnl_7166_part_011500.nldata').
:- include('ac_xnl/ac_xnl_7166_part_011600.nldata').
:- include('ac_xnl/ac_xnl_7166_part_011700.nldata').
:- include('ac_xnl/ac_xnl_7166_part_011800.nldata').
:- include('ac_xnl/ac_xnl_7166_part_011900.nldata').
:- include('ac_xnl/ac_xnl_7166_part_012000.nldata').
:- include('ac_xnl/ac_xnl_7166_part_012100.nldata').
:- include('ac_xnl/ac_xnl_7166_part_012200.nldata').
:- include('ac_xnl/ac_xnl_7166_part_012300.nldata').
:- include('ac_xnl/ac_xnl_7166_part_012400.nldata').
:- include('ac_xnl/ac_xnl_7166_part_012500.nldata').
:- include('ac_xnl/ac_xnl_7166_part_012600.nldata').
:- include('ac_xnl/ac_xnl_7166_part_012700.nldata').
:- include('ac_xnl/ac_xnl_7166_part_012800.nldata').
:- include('ac_xnl/ac_xnl_7166_part_012900.nldata').
:- include('ac_xnl/ac_xnl_7166_part_013000.nldata').
:- include('ac_xnl/ac_xnl_7166_part_013100.nldata').
:- include('ac_xnl/ac_xnl_7166_part_013200.nldata').
:- include('ac_xnl/ac_xnl_7166_part_013300.nldata').
:- include('ac_xnl/ac_xnl_7166_part_013400.nldata').
:- include('ac_xnl/ac_xnl_7166_part_013500.nldata').
:- include('ac_xnl/ac_xnl_7166_part_013600.nldata').
:- include('ac_xnl/ac_xnl_7166_part_013700.nldata').
:- include('ac_xnl/ac_xnl_7166_part_013800.nldata').
:- include('ac_xnl/ac_xnl_7166_part_013900.nldata').
:- include('ac_xnl/ac_xnl_7166_part_014000.nldata').
:- include('ac_xnl/ac_xnl_7166_part_014100.nldata').
:- include('ac_xnl/ac_xnl_7166_part_014200.nldata').
:- include('ac_xnl/ac_xnl_7166_part_014300.nldata').
:- include('ac_xnl/ac_xnl_7166_part_014400.nldata').
:- include('ac_xnl/ac_xnl_7166_part_014500.nldata').
:- include('ac_xnl/ac_xnl_7166_part_014600.nldata').
:- include('ac_xnl/ac_xnl_7166_part_014700.nldata').
:- include('ac_xnl/ac_xnl_7166_part_014800.nldata').
:- include('ac_xnl/ac_xnl_7166_part_014900.nldata').
:- include('ac_xnl/ac_xnl_7166_part_015000.nldata').
:- include('ac_xnl/ac_xnl_7166_part_015100.nldata').
:- include('ac_xnl/ac_xnl_7166_part_015200.nldata').
:- include('ac_xnl/ac_xnl_7166_part_015300.nldata').
:- include('ac_xnl/ac_xnl_7166_part_015400.nldata').
:- include('ac_xnl/ac_xnl_7166_part_015500.nldata').
:- include('ac_xnl/ac_xnl_7166_part_015600.nldata').
:- include('ac_xnl/ac_xnl_7166_part_015700.nldata').
:- include('ac_xnl/ac_xnl_7166_part_015800.nldata').
:- include('ac_xnl/ac_xnl_7166_part_015900.nldata').
:- include('ac_xnl/ac_xnl_7166_part_016000.nldata').
:- include('ac_xnl/ac_xnl_7166_part_016100.nldata').
:- include('ac_xnl/ac_xnl_7166_part_016200.nldata').
:- include('ac_xnl/ac_xnl_7166_part_016300.nldata').
:- include('ac_xnl/ac_xnl_7166_part_016400.nldata').
:- include('ac_xnl/ac_xnl_7166_part_016500.nldata').
:- include('ac_xnl/ac_xnl_7166_part_016600.nldata').
:- include('ac_xnl/ac_xnl_7166_part_016700.nldata').
:- include('ac_xnl/ac_xnl_7166_part_016800.nldata').
:- include('ac_xnl/ac_xnl_7166_part_016900.nldata').
:- include('ac_xnl/ac_xnl_7166_part_017000.nldata').
:- include('ac_xnl/ac_xnl_7166_part_017100.nldata').
:- include('ac_xnl/ac_xnl_7166_part_017200.nldata').
:- include('ac_xnl/ac_xnl_7166_part_017300.nldata').
:- include('ac_xnl/ac_xnl_7166_part_017400.nldata').
:- include('ac_xnl/ac_xnl_7166_part_017500.nldata').
:- include('ac_xnl/ac_xnl_7166_part_017600.nldata').
:- include('ac_xnl/ac_xnl_7166_part_017700.nldata').
:- include('ac_xnl/ac_xnl_7166_part_017800.nldata').
:- include('ac_xnl/ac_xnl_7166_part_017900.nldata').
:- include('ac_xnl/ac_xnl_7166_part_018000.nldata').
:- include('ac_xnl/ac_xnl_7166_part_018100.nldata').
:- include('ac_xnl/ac_xnl_7166_part_018200.nldata').
:- include('ac_xnl/ac_xnl_7166_part_018300.nldata').
:- include('ac_xnl/ac_xnl_7166_part_018400.nldata').
:- include('ac_xnl/ac_xnl_7166_part_018500.nldata').
:- include('ac_xnl/ac_xnl_7166_part_018600.nldata').
:- include('ac_xnl/ac_xnl_7166_part_018700.nldata').
:- include('ac_xnl/ac_xnl_7166_part_018800.nldata').
:- include('ac_xnl/ac_xnl_7166_part_018900.nldata').
:- include('ac_xnl/ac_xnl_7166_part_019000.nldata').
:- include('ac_xnl/ac_xnl_7166_part_019100.nldata').
:- include('ac_xnl/ac_xnl_7166_part_019200.nldata').
:- include('ac_xnl/ac_xnl_7166_part_019300.nldata').
:- include('ac_xnl/ac_xnl_7166_part_019400.nldata').
:- include('ac_xnl/ac_xnl_7166_part_019500.nldata').
:- include('ac_xnl/ac_xnl_7166_part_019600.nldata').
:- include('ac_xnl/ac_xnl_7166_part_019700.nldata').
:- include('ac_xnl/ac_xnl_7166_part_019800.nldata').
:- include('ac_xnl/ac_xnl_7166_part_019900.nldata').
:- include('ac_xnl/ac_xnl_7166_part_020000.nldata').
:- include('ac_xnl/ac_xnl_7166_part_020100.nldata').
:- include('ac_xnl/ac_xnl_7166_part_020200.nldata').
:- include('ac_xnl/ac_xnl_7166_part_020300.nldata').
:- include('ac_xnl/ac_xnl_7166_part_020400.nldata').
:- include('ac_xnl/ac_xnl_7166_part_020500.nldata').
:- include('ac_xnl/ac_xnl_7166_part_020600.nldata').
:- include('ac_xnl/ac_xnl_7166_part_020700.nldata').
:- include('ac_xnl/ac_xnl_7166_part_020800.nldata').
:- include('ac_xnl/ac_xnl_7166_part_020900.nldata').
:- include('ac_xnl/ac_xnl_7166_part_021000.nldata').
:- include('ac_xnl/ac_xnl_7166_part_021100.nldata').
:- include('ac_xnl/ac_xnl_7166_part_021200.nldata').
:- include('ac_xnl/ac_xnl_7166_part_021300.nldata').
:- include('ac_xnl/ac_xnl_7166_part_021400.nldata').
:- include('ac_xnl/ac_xnl_7166_part_021500.nldata').
:- include('ac_xnl/ac_xnl_7166_part_021600.nldata').
:- include('ac_xnl/ac_xnl_7166_part_021700.nldata').
:- include('ac_xnl/ac_xnl_7166_part_021800.nldata').
:- include('ac_xnl/ac_xnl_7166_part_021900.nldata').
:- include('ac_xnl/ac_xnl_7166_part_022000.nldata').
:- include('ac_xnl/ac_xnl_7166_part_022100.nldata').
:- include('ac_xnl/ac_xnl_7166_part_022200.nldata').
:- include('ac_xnl/ac_xnl_7166_part_022300.nldata').
:- include('ac_xnl/ac_xnl_7166_part_022400.nldata').
:- include('ac_xnl/ac_xnl_7166_part_022500.nldata').
:- include('ac_xnl/ac_xnl_7166_part_022600.nldata').
:- include('ac_xnl/ac_xnl_7166_part_022700.nldata').
:- include('ac_xnl/ac_xnl_7166_part_022800.nldata').
:- include('ac_xnl/ac_xnl_7166_part_022900.nldata').
:- include('ac_xnl/ac_xnl_7166_part_023000.nldata').
:- include('ac_xnl/ac_xnl_7166_part_023100.nldata').
:- include('ac_xnl/ac_xnl_7166_part_023200.nldata').
:- include('ac_xnl/ac_xnl_7166_part_023300.nldata').
:- include('ac_xnl/ac_xnl_7166_part_023400.nldata').
:- include('ac_xnl/ac_xnl_7166_part_023500.nldata').
:- include('ac_xnl/ac_xnl_7166_part_023600.nldata').
:- include('ac_xnl/ac_xnl_7166_part_023700.nldata').
:- include('ac_xnl/ac_xnl_7166_part_023800.nldata').
:- include('ac_xnl/ac_xnl_7166_part_023900.nldata').
:- include('ac_xnl/ac_xnl_7166_part_024000.nldata').
:- include('ac_xnl/ac_xnl_7166_part_024100.nldata').
:- include('ac_xnl/ac_xnl_7166_part_024200.nldata').
:- include('ac_xnl/ac_xnl_7166_part_024300.nldata').
:- include('ac_xnl/ac_xnl_7166_part_024400.nldata').
:- include('ac_xnl/ac_xnl_7166_part_024500.nldata').
:- include('ac_xnl/ac_xnl_7166_part_024600.nldata').
:- include('ac_xnl/ac_xnl_7166_part_024700.nldata').
:- include('ac_xnl/ac_xnl_7166_part_024800.nldata').
:- include('ac_xnl/ac_xnl_7166_part_024900.nldata').
:- include('ac_xnl/ac_xnl_7166_part_025000.nldata').
:- include('ac_xnl/ac_xnl_7166_part_025100.nldata').
:- include('ac_xnl/ac_xnl_7166_part_025200.nldata').
:- include('ac_xnl/ac_xnl_7166_part_025300.nldata').
:- include('ac_xnl/ac_xnl_7166_part_025400.nldata').
:- include('ac_xnl/ac_xnl_7166_part_025500.nldata').
:- include('ac_xnl/ac_xnl_7166_part_025600.nldata').
:- include('ac_xnl/ac_xnl_7166_part_025700.nldata').
:- include('ac_xnl/ac_xnl_7166_part_025800.nldata').
:- include('ac_xnl/ac_xnl_7166_part_025900.nldata').
:- include('ac_xnl/ac_xnl_7166_part_026000.nldata').
:- include('ac_xnl/ac_xnl_7166_part_026100.nldata').
:- include('ac_xnl/ac_xnl_7166_part_026200.nldata').
:- include('ac_xnl/ac_xnl_7166_part_026300.nldata').
:- include('ac_xnl/ac_xnl_7166_part_026400.nldata').
:- include('ac_xnl/ac_xnl_7166_part_026500.nldata').
:- include('ac_xnl/ac_xnl_7166_part_026600.nldata').
:- include('ac_xnl/ac_xnl_7166_part_026700.nldata').
:- include('ac_xnl/ac_xnl_7166_part_026800.nldata').

:- endif.

/*
split -l 8000 -d -a 4 --additional-suffix=00.nldata sorted ac_xnl/ac_xnl_7166_part_

   acnl/3
   acnl/4
   acnl/5
   acnl/6
   acnl/7
   acnl/8
   acnl/9
   % acnl/10
   acnl/11

*/




