(*
    Tests for: Daniel`ARC`ARCCombineConclusions
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCCombineConclusions]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ARCCombineConclusions[
        {
            <|
                "b" -> 1,
                "Examples" -> {1, 2},
                "ExampleCount" -> 1,
                "UseCount" -> 1,
                "InputObjects" -> {"a", "b"}
            |>,
            <|
                "b" -> 1,
                "Examples" -> {2},
                "ExampleCount" -> 2,
                "UseCount" -> 3,
                "InputObjects" -> {"c"}
            |>,
            <|"b" -> 1|>
        }
    ]
    ,
    <|
        "b" -> 1,
        "Examples" -> {1, 2},
        "ExampleCount" -> 3,
        "UseCount" -> 4,
        "InputObjects" -> {"a", "b", "c"}
    |>
    ,
    TestID -> "ARCCombineConclusions-20220819-DPWYVN"
]

Test[
    Daniel`ARC`ARCCombineConclusions[{<|"b" -> 1|>, <|"b" -> 1|>}]
    ,
    <|"b" -> 1|>
    ,
    TestID -> "ARCCombineConclusions-20220819-0I7PMB"
]