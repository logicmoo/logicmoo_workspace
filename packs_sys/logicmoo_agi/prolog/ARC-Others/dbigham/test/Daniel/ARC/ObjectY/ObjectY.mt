(*
    Tests for: Daniel`ARC`ObjectY
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ObjectY]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ObjectY[<|"Y" -> 1|>]
    ,
    1
    ,
    TestID -> "ObjectY-20221112-L471L2"
]

Test[
    Daniel`ARC`ObjectY[<|"Position" -> {1, 2}|>]
    ,
    1
    ,
    TestID -> "ObjectY-20221112-ZHKL18"
]

Test[
    Daniel`ARC`ObjectY[<|"Position" -> <|"Y" -> 3|>|>]
    ,
    3
    ,
    TestID -> "ObjectY-20221112-IPIH3E"
]

Test[
    Daniel`ARC`ObjectY[<||>]
    ,
    Missing["NetSpecified", "Y"]
    ,
    TestID -> "ObjectY-20221112-2Q53PZ"
]