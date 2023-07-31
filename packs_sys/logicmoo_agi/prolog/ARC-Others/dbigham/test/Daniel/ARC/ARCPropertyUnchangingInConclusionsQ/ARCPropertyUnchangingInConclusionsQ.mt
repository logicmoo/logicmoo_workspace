(*
    Tests for: Daniel`ARC`ARCPropertyUnchangingInConclusionsQ
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCPropertyUnchangingInConclusionsQ]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ARCPropertyUnchangingInConclusionsQ[
        {
            <|"Input" -> <|"X" -> 1|>, "Output" -> <|"X" -> 1|>|>,
            <|"Input" -> <|"X" -> 2|>, "Output" -> <|"X" -> 2|>|>
        },
        "X"
    ]
    ,
    True
    ,
    TestID -> "ARCPropertyUnchangingInConclusionsQ-20221023-OLXO9M"
]

Test[
    Daniel`ARC`ARCPropertyUnchangingInConclusionsQ[
        {
            <|"Input" -> <|"X" -> 1|>, "Output" -> <|"X" -> 1|>|>,
            <|"Input" -> <|"X" -> 2|>, "Output" -> <|"X" -> 3|>|>
        },
        "X"
    ]
    ,
    False
    ,
    TestID -> "ARCPropertyUnchangingInConclusionsQ-20221023-D42RMZ"
]