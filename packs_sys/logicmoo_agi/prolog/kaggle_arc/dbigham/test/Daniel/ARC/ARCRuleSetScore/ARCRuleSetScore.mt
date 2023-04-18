(*
    Tests for: Daniel`ARC`ARCRuleSetScore
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCRuleSetScore]
    
    Author: danielb
*)

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCRuleSetScore[
            {
                <|"Color" -> 1|> -> <|"X" -> 1, "Examples" -> {1, 2, 3}|>,
                <|"Color" -> 2|> -> <|"X" -> 2|>
            }
        ]
    ]
    ,
    -0.3786104442449456
    ,
    TestID -> "ARCRuleSetScore-20220825-YKR23C"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCRuleSetScore[
            {<|"X" -> 1|> -> <|"Color" -> 1|>, <|"X" -> 2|> -> <|"Color" -> 2|>}
        ]
    ]
    ,
    -1.0191866507758092
    ,
    TestID -> "ARCRuleSetScore-20220826-6EL1X5"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCRuleSetScore[
            {
                <|"Shape" -> <|"Name" -> "Square", "Filled" -> True|>|> -> <|
                    "Image" -> {{5, 5, 5}}
                |>,
                <|"Shape" -> Except[<|"Name" -> "Square", "Filled" -> True|>]|> -> <|
                    "Shape" -> <|
                        "Name" -> "Line",
                        "Angle" -> Inactive[Times][
                            Daniel`ARC`ObjectValue[
                                <|"Colors" -> {}, "Context" -> "Component"|>,
                                "Y2"
                            ],
                            45
                        ]
                    |>,
                    "Color" -> 5
                |>
            }
        ]
    ]
    ,
    -3.225789657650295
    ,
    TestID -> "ARCRuleSetScore-20221005-WC2JWK"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCRuleSetScore[
            {
                <|"X" -> 1|> -> <|"Color" -> 1, "ExampleCount" -> 1|>,
                <|"X" -> 2|> -> <|"Color" -> 2, "ExampleCount" -> 2|>
            }
        ]
    ]
    ,
    -1.6340447318308224
    ,
    TestID -> "ARCRuleSetScore-20221005-0TF1Z0"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCRuleSetScore[
            {<|"X" -> 1|> -> <|"Color" -> 1, "ExampleCount" -> 1, "UseCount" -> 1|>}
        ]
    ]
    ,
    -11.582971888569093
    ,
    TestID -> "ARCRuleSetScore-20221005-23M5PG"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCRuleSetScore[
            {<|"X" -> 1|> -> <|"Color" -> 1, "ExampleCount" -> 1, "UseCount" -> 2|>}
        ]
    ]
    ,
    -9.120624825711085
    ,
    TestID -> "ARCRuleSetScore-20221005-DWLP64"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCRuleSetScore[
            {
                {
                    <|
                        "SceneAsSingleObject" -> True,
                        "Rules" -> {
                            <||> -> <|
                                "Same" -> True,
                                "Examples" -> {1, 2, 3},
                                "ExampleCount" -> 3,
                                "UseCount" -> 3,
                                "InputObjects" -> {
                                    "49b747d8-4bc2-4ba7-bc28-f48b31c8fff8",
                                    "c52e5327-9c44-444f-90bc-cbf75ced50f5",
                                    "98f7c073-bde5-4a08-ab9a-e235ef4d3756"
                                }
                            |>
                        }
                    |>,
                    <|
                        "SceneAsSingleObject" -> True,
                        "Rules" -> {
                            <||> -> <|
                                "Transform" -> <|"Type" -> "Flip", "Direction" -> "Horizontal"|>,
                                "Examples" -> {1, 2, 3},
                                "ExampleCount" -> 3,
                                "UseCount" -> 3,
                                "InputObjects" -> {
                                    "49b747d8-4bc2-4ba7-bc28-f48b31c8fff8",
                                    "c52e5327-9c44-444f-90bc-cbf75ced50f5",
                                    "98f7c073-bde5-4a08-ab9a-e235ef4d3756"
                                }
                            |>
                        }
                    |>
                },
                {
                    <|
                        "SceneAsSingleObject" -> True,
                        "Rules" -> {
                            <||> -> <|
                                "Transform" -> <|"Type" -> "Flip", "Direction" -> "Vertical"|>
                            |>
                        }
                    |>,
                    <|
                        "SceneAsSingleObject" -> True,
                        "Rules" -> {
                            <||> -> <|
                                "Transform" -> <|"Type" -> "Rotation", "Angle" -> 180|>
                            |>
                        }
                    |>
                }
            }
        ]
    ]
    ,
    1.557493763283929
    ,
    TestID -> "ARCRuleSetScore-20221005-UT0GIB"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCRuleSetScore[
            {
                <||> -> <|
                    "Transform" -> <|
                        "Type" -> "ColorMapping",
                        "Mapping" -> {
                            {
                                Inactive[Plus][
                                    Inactive[Times][
                                        Daniel`ARC`ObjectValue["Object", "YInverse"],
                                        1.5
                                    ],
                                    0.5
                                ],
                                Inactive[Plus][
                                    Inactive[Times][
                                        Daniel`ARC`ObjectValue["Object", "YInverse"],
                                        -1.5
                                    ],
                                    12.5
                                ]
                            },
                            {
                                Inactive[Plus][
                                    Inactive[Times][
                                        Daniel`ARC`ObjectValue["Object", "YInverse"],
                                        -1.5
                                    ],
                                    12.5
                                ],
                                Inactive[Plus][
                                    Inactive[Times][
                                        Daniel`ARC`ObjectValue["Object", "YInverse"],
                                        1.5
                                    ],
                                    0.5
                                ]
                            }
                        }
                    |>
                |>
            }
        ]
    ]
    ,
    -11.122234487727725
    ,
    TestID -> "ARCRuleSetScore-20221020-UCKXYG"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCRuleSetScore[
            {
                {
                    <|
                        "FormMultiColorCompositeObjects" -> False,
                        "Rules" -> {
                            <||> -> <|"Same" -> True|>,
                            <|
                                "Transform" -> <|
                                    "Type" -> "AddObjects",
                                    "Objects" -> {
                                        <|
                                            "Shapes" -> {
                                                <|"Name" -> "Rectangle", "Filled" -> True|>
                                            },
                                            "Color" -> 4,
                                            "XInverse" -> 3,
                                            "Y" -> 2,
                                            "X2" -> Inactive[Plus][
                                                Daniel`ARC`ObjectValue["Object", "Color"],
                                                -1
                                            ],
                                            "Y2Inverse" -> 1,
                                            "ZOrder" -> 0
                                        |>
                                    },
                                    "Condition" -> <|
                                        Daniel`ARC`ObjectValue["Object", "X2Inverse"] -> 1
                                    |>
                                |>
                            |>
                        }
                    |>
                },
                {
                    <|
                        "Rules" -> {
                            <|"XInverse" -> 5|> -> <|
                                "Transform" -> <|
                                    "Type" -> "AddComponents",
                                    "Components" -> {
                                        <|
                                            "Image" -> Daniel`ARC`ARCScene[
                                                {{4, 4}, {4, 4}, {4, 4}, {4, 4}, {4, 4}}
                                            ],
                                            "Position" -> <|
                                                "RelativePosition" -> <|
                                                    "Y" -> -5,
                                                    "XInverse" -> -2
                                                |>
                                            |>
                                        |>
                                    }
                                |>
                            |>,
                            <|"XInverse" -> Except[5]|> -> <|"Same" -> True|>
                        }
                    |>
                }
            }
        ]
    ]
    ,
    -4.9528235023424925
    ,
    TestID -> "ARCRuleSetScore-20221021-A2RRQP"
]

Test[
    Daniel`ARC`ARCRuleSetScore[
        {
            <||> -> <|"Same" -> True|>,
            <|
                "Transform" -> <|
                    "Type" -> "AddObjects",
                    "Objects" -> {
                        <|
                            "Shapes" -> {<|"Name" -> "Rectangle", "Filled" -> True|>},
                            "Color" -> 4,
                            "X" -> Daniel`ARC`ObjectValue[<|"X.Rank" -> 1|>, "X"],
                            "Y" -> Inactive[Plus][
                                Inactive[Times][
                                    Daniel`ARC`ObjectValue[<|"Width.Rank" -> 1|>, "Y"],
                                    0.5
                                ],
                                1.5
                            ],
                            "X2" -> Daniel`ARC`ObjectValue[<|"Width.Rank" -> 2|>, "X2"],
                            "Height" -> Inactive[Plus][
                                Daniel`ARC`ObjectValue["InputScene", "YMiddle"],
                                -2
                            ],
                            "ZOrder" -> 0
                        |>
                    }
                |>
            |>
        }
    ]
    ,
    -4.602240929586872
    ,
    TestID -> "ARCRuleSetScore-20221021-D7H8R6"
]