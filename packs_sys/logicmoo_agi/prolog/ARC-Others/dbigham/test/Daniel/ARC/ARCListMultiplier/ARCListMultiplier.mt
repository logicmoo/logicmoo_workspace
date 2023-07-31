(*
    Tests for: Daniel`ARC`ARCListMultiplier
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCListMultiplier]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ARCListMultiplier[{1, 2, 3}, {2, 4, 6}]
    ,
    2
    ,
    TestID -> "ARCListMultiplier-20220903-1ZMUK5"
]

Test[
    Daniel`ARC`ARCListMultiplier[{1, 2, 3}, {2, 4, 9}]
    ,
    Missing["None"]
    ,
    TestID -> "ARCListMultiplier-20220903-A9RE8R"
]