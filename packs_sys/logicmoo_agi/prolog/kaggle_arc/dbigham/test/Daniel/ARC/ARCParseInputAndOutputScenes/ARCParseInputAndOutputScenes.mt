(*
    Tests for: Daniel`ARC`ARCParseInputAndOutputScenes
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCParseInputAndOutputScenes]
    
    Author: danielb
*)

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Length[
                ERPTesting`NormalizeOutput[
                    With[
                        {parsedFile = Daniel`ARC`ARCParseFile["0ca9ddb6"]},
                        Daniel`ARC`ARCParseInputAndOutputScenes[
                            parsedFile["Train", 1, "Input"],
                            parsedFile["Train", 1, "Output"],
                            1
                        ]
                    ]["Output", "Objects"]
                ]
            ]
        ]
    ]
    ,
    2
    ,
    TestID -> "ARCParseInputAndOutputScenes-20220724-1VOP6G"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`SimplifyObjects[
                ERPTesting`NormalizeOutput[
                    With[
                        {parsedFile = Daniel`ARC`ARCParseFile["25d8a9c8"]},
                        Daniel`ARC`ARCParseInputAndOutputScenes[
                            parsedFile["Train", 3, "Input"],
                            parsedFile["Train", 3, "Output"],
                            1,
                            "FormMultiColorCompositeObjects" -> False
                        ]
                    ]["Output"]
                ]
            ]
        ]
    ]
    ,
    <|
        "Background" -> 0,
        "Width" -> 3,
        "Height" -> 3,
        "ObjectCount" -> 2,
        "Objects" -> {
            <|"Image" -> Daniel`ARC`ARCScene[{{5, 5, 5}}], "Position" -> {2, 1}|>,
            <|"Image" -> Daniel`ARC`ARCScene[{{5, 5, 5}}], "Position" -> {3, 1}|>
        },
        "Scene" -> Daniel`ARC`ARCScene[{{0, 0, 0}, {5, 5, 5}, {5, 5, 5}}]
    |>
    ,
    TestID -> "ARCParseInputAndOutputScenes-20220819-FFEEU6"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`SimplifyObjects[
                ERPTesting`NormalizeOutput[
                    With[
                        {parsedFile = Daniel`ARC`ARCParseFile["25d8a9c8"]},
                        Daniel`ARC`ARCParseInputAndOutputScenes[
                            parsedFile["Train", 3, "Input"],
                            parsedFile["Train", 3, "Output"],
                            1
                        ]
                    ]["Output"][[
                        "Objects",
                        All,
                        "Components"
                    ]]
                ]
            ]
        ]
    ]
    ,
    {Missing["KeyAbsent", "Components"]}
    ,
    TestID -> "ARCParseInputAndOutputScenes-20220819-9BBT2Q"
]