(*
    Tests for: Daniel`ARC`ARCFindLinearModel
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCFindLinearModel]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ARCFindLinearModel[
        {3, 5, 7},
        <|"a" -> {1, 2, 3}, "b" -> {9, 9, 9}, "c" -> {"a", "b", "c"}|>
    ]
    ,
    <|"Property" -> "a", "Slope" -> 2, "Offset" -> 1|>
    ,
    TestID -> "ARCFindLinearModel-20221008-8M7BWY"
]

Test[
    Daniel`ARC`ARCFindLinearModel[
        {3, 5, 7},
        <|"a" -> {1, 1, 1}, "b" -> {9, 9, 9}, "c" -> {"a", "b", "c"}|>
    ]
    ,
    None
    ,
    TestID -> "ARCFindLinearModel-20221008-8BXXF0"
]