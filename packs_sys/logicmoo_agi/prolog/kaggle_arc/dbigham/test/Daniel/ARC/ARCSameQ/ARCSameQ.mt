(*
    Tests for: Daniel`ARC`ARCSameQ
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCSameQ]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ARCSameQ[
        <|"Image" -> 1, "Position" -> 2, "AnotherKey" -> 1|>,
        <|"Image" -> 1, "Position" -> 2, "AnotherKey" -> 2|>
    ]
    ,
    True
    ,
    TestID -> "ARCSameQ-20220719-VC3YQN"
]

Test[
    Daniel`ARC`ARCSameQ[
        <|"Image" -> 1, "Position" -> 2, "AnotherKey" -> 1|>,
        <|"Image" -> 1, "Position" -> 3, "AnotherKey" -> 2|>
    ]
    ,
    False
    ,
    TestID -> "ARCSameQ-20220719-R06K57"
]