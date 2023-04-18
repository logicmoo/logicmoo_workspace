(*
    Tests for: Daniel`ARC`ARCRemoveEmptySpaceQ
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCRemoveEmptySpaceQ]
    
    Author: danielb
*)

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`ARCRemoveEmptySpaceQ[
                <|
                    "FormMultiColorCompositeObjects" -> False,
                    "Rules" -> {
                        <||> -> <|
                            "Shape" -> <|"Name" -> "Pixel"|>,
                            "X" -> Daniel`ARC`ObjectValue["InputObject", "X.InverseRank"],
                            "Y" -> Daniel`ARC`ObjectValue["InputObject", "Y.InverseRank"]
                        |>
                    }
                |>,
                With[
                    {parsedFile = Daniel`ARC`ARCParseFile["746b3537"]["Train"]},
                    Map[
                        Function[
                            {example},
                            Utility`ReturnIfFailure[
                                Daniel`ARC`ARCParseInputAndOutputScenes[
                                    example["Input"],
                                    example["Output"],
                                    1,
                                    "FormMultiColorCompositeObjects" -> False
                                ]
                            ]
                        ],
                        parsedFile
                    ]
                ]
            ]
        ]
    ]
    ,
    True
    ,
    TestID -> "ARCRemoveEmptySpaceQ-20220828-Z7MG2I"
]