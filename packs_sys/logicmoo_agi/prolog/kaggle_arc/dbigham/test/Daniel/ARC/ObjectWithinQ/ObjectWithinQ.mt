(*
    Tests for: Daniel`ARC`ObjectWithinQ
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ObjectWithinQ]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ObjectWithinQ[
        <|"Position" -> {2, 2}, "Width" -> 1, "Height" -> 1|>,
        <|"Position" -> {1, 1}, "Width" -> 3, "Height" -> 3|>
    ]
    ,
    True
    ,
    TestID -> "ObjectWithinQ-20220725-N0G0A0"
]

Test[
    Daniel`ARC`ObjectWithinQ[
        <|"Position" -> {1, 1}, "Width" -> 1, "Height" -> 1|>,
        <|"Position" -> {1, 1}, "Width" -> 1, "Height" -> 1|>
    ]
    ,
    True
    ,
    TestID -> "ObjectWithinQ-20220725-P9LNB0"
]

Test[
    Daniel`ARC`ObjectWithinQ[
        <|"Position" -> {1, 1}, "Width" -> 3, "Height" -> 3|>,
        <|"Position" -> {2, 2}, "Width" -> 1, "Height" -> 1|>
    ]
    ,
    False
    ,
    TestID -> "ObjectWithinQ-20220725-ROZE64"
]