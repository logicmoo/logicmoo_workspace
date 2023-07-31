(*
    Tests for: Daniel`ARC`ARCBlockingDirection
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCBlockingDirection]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ARCBlockingDirection[
        <|"Y" -> 1, "X" -> 1, "Width" -> 1, "Height" -> 2|>,
        <|"Y" -> 2, "X" -> 3, "Width" -> 1, "Height" -> 2|>
    ]
    ,
    {0, 1}
    ,
    TestID -> "ARCBlockingDirection-20220804-Y6T2JD"
]

Test[
    Daniel`ARC`ARCBlockingDirection[
        <|"Y" -> 1, "X" -> 1, "Width" -> 1, "Height" -> 2|>,
        <|"Y" -> 3, "X" -> 3, "Width" -> 1, "Height" -> 2|>
    ]
    ,
    None
    ,
    TestID -> "ARCBlockingDirection-20220804-PNB6KZ"
]