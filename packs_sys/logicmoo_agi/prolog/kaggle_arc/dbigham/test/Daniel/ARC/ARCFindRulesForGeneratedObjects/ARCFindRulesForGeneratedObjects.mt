(*
    Tests for: Daniel`ARC`ARCFindRulesForGeneratedObjects
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCFindRulesForGeneratedObjects]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ARCSimplifyRules[
        Module[
            {example},
            examples = Utility`PrintIfFailure[
                Daniel`ARC`ARCParseInputAndOutputScenes[
                    Daniel`ARC`ARCParseFile["e5790162"]["Train"],
                    "FormMultiColorCompositeObjects" -> False
                ]
            ];
            Daniel`ARC`ARCFindRulesForGeneratedObjects[
                Map[
                    Function[
                        {example},
                        Append[
                            SelectFirst[
                                example[["Output", "Objects"]],
                                #1["Color"] === 3 & 
                            ],
                            "Scene" -> Replace[
                                example["Output", "Scene"],
                                0 -> Daniel`ARC`Private`$nonImageColor,
                                {3}
                            ]
                        ]
                    ],
                    examples
                ],
                examples
            ]
        ]
    ]
    ,
    {
        <|"ColorAhead" -> Missing["KeyAbsent", "ColorAhead"]|> -> <|
            "OutgoingDirection" -> {0, 1}
        |>,
        <|"ColorAhead" -> -1|> -> <|"TurnDegrees" -> 0|>,
        <|"ColorAhead" -> 6|> -> <|"TurnDegrees" -> 90|>,
        <|"ColorAhead" -> -2|> -> <|"Stop" -> True|>,
        <|"ColorAhead" -> 8|> -> <|"TurnDegrees" -> -90|>
    }
    ,
    TestID -> "ARCFindRulesForGeneratedObjects-20220922-J2ZQTL"
]

Test[
    Daniel`ARC`ARCSimplifyRules[
        Module[
            {example},
            examples = Utility`PrintIfFailure[
                Daniel`ARC`ARCParseInputAndOutputScenes[
                    Daniel`ARC`ARCParseFile["d9f24cd1"]["Train"],
                    "FormMultiColorCompositeObjects" -> False,
                    "CheckForGridsAndDividers" -> False
                ]
            ];
            Daniel`ARC`ARCFindRulesForGeneratedObjects[
                Flatten[
                    Map[
                        Function[
                            {example},
                            Map[
                                Function[
                                    {object},
                                    Join[
                                        object,
                                        <|
                                            "Scene" -> Replace[
                                                example["Output", "Scene"],
                                                0 -> Daniel`ARC`Private`$nonImageColor,
                                                {3}
                                            ],
                                            "Input" -> <|
                                                "Position" -> {10, object["X"]},
                                                "Width" -> 1,
                                                "Height" -> 1
                                            |>
                                        |>
                                    ]
                                ],
                                Select[
                                    example[["Output", "Objects"]],
                                    #1["Color"] === 2 & 
                                ]
                            ]
                        ],
                        examples
                    ]
                ],
                examples
            ]
        ]
    ]
    ,
    {
        <|"ColorAhead" -> Missing["KeyAbsent", "ColorAhead"]|> -> <|
            "OutgoingDirection" -> {-1, 0}
        |>,
        <|"ColorAhead" -> -1|> -> <|"OutgoingDirection" -> {-1, 0}|>,
        <|"ColorAhead" -> -2|> -> <|"Stop" -> True|>,
        <|"ColorAhead" -> 5|> -> <|"OutgoingDirection" -> {0, 1}, "TurnDegrees" -> 90|>
    }
    ,
    TestID -> "ARCFindRulesForGeneratedObjects-20220925-PCRLFU"
]