(*
    Tests for: Daniel`ARC`ARCHollowCount
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCHollowCount]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ARCHollowCount[{{1, 1, 1}, {1, -1, 1}, {1, 1, 1}}]
    ,
    1
    ,
    TestID -> "ARCHollowCount-20220903-OQZU9E"
]

Test[
    Daniel`ARC`ARCHollowCount[{{1, 1, 1, 1, 1}, {1, -1, 1, -1, 1}, {1, 1, 1, 1, 1}}]
    ,
    2
    ,
    TestID -> "ARCHollowCount-20220903-0I4WSR"
]

Test[
    Daniel`ARC`ARCHollowCount[{{1, 1, 1}, {1, 1, 1}, {1, 1, 1}}]
    ,
    0
    ,
    TestID -> "ARCHollowCount-20220903-TXMF2T"
]