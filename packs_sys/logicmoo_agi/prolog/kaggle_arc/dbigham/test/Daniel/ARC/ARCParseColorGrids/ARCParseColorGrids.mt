(*
    Tests for: Daniel`ARC`ARCParseColorGrids
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCParseColorGrids]
    
    Author: danielb
*)

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`ARCParseColorGrids[
                Daniel`ARC`ARCParseInputAndOutputScenes[
                    Daniel`ARC`ARCParseFile["94f9d214"]["Train"]
                ]
            ]
        ]
    ]
    ,
    <|
        "Result" -> True,
        "Type" -> "ColorGrid",
        "RowCount" -> 2,
        "ColumnCount" -> 1,
        "Cells" -> {
            {<|"Y" -> 1, "X" -> 1, "Width" -> 4, "Height" -> 4|>},
            {<|"Y" -> 5, "X" -> 1, "Width" -> 4, "Height" -> 4|>}
        }
    |>
    ,
    TestID -> "ARCParseColorGrids-20220926-O79FXJ"
]

Test[
    Daniel`ARC`ARCParseColorGrids[
        Daniel`ARC`ARCParseInputAndOutputScenes[
            Daniel`ARC`ARCParseFile["99B1BC43"]["Train"]
        ]
    ]
    ,
    <|"Result" -> False|>
    ,
    TestID -> "ARCParseColorGrids-20220926-CD754J"
]