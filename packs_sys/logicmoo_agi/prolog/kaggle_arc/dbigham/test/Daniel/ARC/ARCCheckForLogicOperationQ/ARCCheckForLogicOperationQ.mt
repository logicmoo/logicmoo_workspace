(*
    Tests for: Daniel`ARC`ARCCheckForLogicOperationQ
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCCheckForLogicOperationQ]
    
    Author: danielb
*)

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`ARCCheckForLogicOperationQ[
                Daniel`ARC`ARCParseInputAndOutputScenes[
                    Daniel`ARC`ARCParseFile["0520fde7"]["Train"]
                ]
            ]
        ]
    ]
    ,
    <|
        "Result" -> True,
        "InputCells" -> {
            <|"Y" -> 1, "X" -> 1, "Width" -> 3, "Height" -> 3|>,
            <|"Y" -> 1, "X" -> 5, "Width" -> 3, "Height" -> 3|>
        },
        "OutputColor" -> 2
    |>
    ,
    TestID -> "ARCCheckForLogicOperationQ-20220925-AVG2DO"
]