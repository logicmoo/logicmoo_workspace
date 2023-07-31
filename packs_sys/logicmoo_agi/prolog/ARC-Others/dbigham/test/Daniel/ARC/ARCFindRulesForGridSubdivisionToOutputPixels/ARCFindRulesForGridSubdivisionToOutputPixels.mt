(*
    Tests for: Daniel`ARC`ARCFindRulesForGridSubdivisionToOutputPixels
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCFindRulesForGridSubdivisionToOutputPixels]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ARCSimplifyRules[
        Daniel`ARC`ARCFindRulesForGridSubdivisionToOutputPixels[
            Daniel`ARC`ARCParseInputAndOutputScenes[
                Daniel`ARC`ARCParseFile["6773b310"]["Train"]
            ]
        ]
    ]
    ,
    <|
        "Subdivision" -> <|"Input" -> "Grid", "Output" -> "Pixels"|>,
        "Rules" -> <|
            "SceneAsSingleObject" -> True,
            "Width" -> Inactive[
                Daniel`ARC`ObjectValue["InputScene", "Width"]*0.3333333333333333
            ],
            "Height" -> Inactive[
                Daniel`ARC`ObjectValue["InputScene", "Height"]*0.3333333333333333
            ],
            "Rules" -> {
                <|"FilledArea" -> 1|> -> <|"Image" -> Daniel`ARC`ARCScene[{{-1}}]|>,
                <|"FilledArea" -> 2|> -> <|"Image" -> Daniel`ARC`ARCScene[{{1}}]|>
            }
        |>
    |>
    ,
    TestID -> "ARCFindRulesForGridSubdivisionToOutputPixels-20220911-4BUGVQ"
]