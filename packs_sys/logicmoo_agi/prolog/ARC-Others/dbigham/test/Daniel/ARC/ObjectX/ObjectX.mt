(*
    Tests for: Daniel`ARC`ObjectX
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ObjectX]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ObjectX[<|"X" -> 1|>]
    ,
    1
    ,
    TestID -> "ObjectX-20221112-KNZ63O"
]

Test[
    Daniel`ARC`ObjectX[<|"Position" -> {1, 2}|>]
    ,
    2
    ,
    TestID -> "ObjectX-20221112-F6EOAI"
]

Test[
    Daniel`ARC`ObjectX[<|"Position" -> <|"X" -> 3|>|>]
    ,
    3
    ,
    TestID -> "ObjectX-20221112-WTRV76"
]

Test[
    Daniel`ARC`ObjectX[<||>]
    ,
    Missing["NetSpecified", "X"]
    ,
    TestID -> "ObjectX-20221112-9T6B8S"
]