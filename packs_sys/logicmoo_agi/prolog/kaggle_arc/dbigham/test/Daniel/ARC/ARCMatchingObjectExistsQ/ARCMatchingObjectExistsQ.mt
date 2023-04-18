(*
    Tests for: Daniel`ARC`ARCMatchingObjectExistsQ
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCMatchingObjectExistsQ]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ARCMatchingObjectExistsQ[
        {
            <|"UUID" -> 1, "Image" -> {{1, 0}, {0, 1}}, "Position" -> {1, 1}|>,
            <|"UUID" -> 2, "Image" -> {{0, 1}, {1, 0}}, "Position" -> {3, 3}|>
        },
        <|"Image" -> {{1, 0}, {0, 1}}, "Position" -> {1, 1}|>
    ]
    ,
    True
    ,
    TestID -> "ARCMatchingObjectExistsQ-20220724-UWORCI"
]

Test[
    Daniel`ARC`ARCMatchingObjectExistsQ[
        {<|"UUID" -> 2, "Image" -> {{0, 1}, {1, 0}}, "Position" -> {3, 3}|>},
        <|"Image" -> {{1, 0}, {0, 1}}, "Position" -> {1, 1}|>
    ]
    ,
    False
    ,
    TestID -> "ARCMatchingObjectExistsQ-20220724-ADNDAM"
]