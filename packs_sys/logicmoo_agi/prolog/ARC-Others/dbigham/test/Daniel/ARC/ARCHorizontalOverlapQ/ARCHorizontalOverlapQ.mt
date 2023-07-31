(*
    Tests for: Daniel`ARC`ARCHorizontalOverlapQ
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCHorizontalOverlapQ]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ARCHorizontalOverlapQ[
        <|"X" -> 1, "Y" -> 1, "Width" -> 2, "Height" -> 1|>,
        <|"X" -> 2, "Y" -> 3, "Width" -> 2, "Height" -> 1|>
    ]
    ,
    True
    ,
    TestID -> "ARCHorizontalOverlapQ-20220804-Z3O7JY"
]

Test[
    Daniel`ARC`ARCHorizontalOverlapQ[
        <|"X" -> 1, "Y" -> 1, "Width" -> 2, "Height" -> 1|>,
        <|"X" -> 4, "Y" -> 3, "Width" -> 2, "Height" -> 1|>
    ]
    ,
    False
    ,
    TestID -> "ARCHorizontalOverlapQ-20220804-8YCCKA"
]

Test[
    Daniel`ARC`ARCHorizontalOverlapQ[
        <|"X" -> 1, "Y" -> 1, "Width" -> 2, "Height" -> 1|>,
        <|"X" -> -1, "Y" -> 3, "Width" -> 2, "Height" -> 1|>
    ]
    ,
    False
    ,
    TestID -> "ARCHorizontalOverlapQ-20220804-YW7J25"
]