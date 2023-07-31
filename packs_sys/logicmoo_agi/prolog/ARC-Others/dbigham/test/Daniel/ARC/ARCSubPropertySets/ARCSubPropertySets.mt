(*
    Tests for: Daniel`ARC`ARCSubPropertySets
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCSubPropertySets]
    
    Author: danielb
*)

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCSubPropertySets[Daniel`ARC`Private`$transformTypes[Automatic]]
    ]
    ,
    {
        {
            "Image" -> <||>,
            "Position" -> <|
                "SubProperties" -> {
                    "RelativePosition" -> <|
                        "SubProperties" -> {"Y" | "YInverse", "X" | "XInverse"}
                    |>,
                    "Y" -> <|"ObjectGet" -> (#1["Y"] & )|>,
                    "X" -> <|"ObjectGet" -> (#1["X"] & )|>
                },
                "MinimalPropertySets" -> {{"RelativePosition"}, {"Y", "X"}}
            |>,
            "ZOrder" -> <||>
        },
        {
            Alternatives[
                "Shape" -> <|
                    "ObjectGet" -> (#1["Shape"] & ),
                    "MinimalPropertySets" -> {{"Name", "Angle", "Fill" | Missing["Fill"]}}
                |>,
                "MonochromeImage" -> <||>,
                "Shapes" -> <|
                    "ClassList" -> True,
                    "Condition" -> Hold[
                        Not[
                            Daniel`ARC`ARCConclusionsSoFarMatchQ[
                                #1,
                                "Shape",
                                Alternatives[KeyValuePattern["Fill" -> _]]
                            ]
                        ]
                    ]
                |>
            ],
            Alternatives[
                "Color" -> <|"ObjectGet" -> (#1["Color"] & )|>,
                Missing["Color"] -> <|
                    "ObjectGet" -> (#1["Color"] & ),
                    "Condition" -> Hold[
                        Daniel`ARC`ARCConclusionsSoFarMatchQ[
                            #1,
                            "Shape",
                            Alternatives[
                                KeyValuePattern[
                                    {
                                        "Border" -> KeyValuePattern["Color" -> _],
                                        "Interior" -> KeyValuePattern["Color" -> _]
                                    }
                                ],
                                KeyValuePattern["Fill" -> _]
                            ]
                        ]
                    ]
                |>
            ],
            ("X" -> <|"ObjectGet" -> (#1["X"] & )|>) | ("XInverse" -> <||>),
            ("Y" -> <|"ObjectGet" -> (#1["Y"] & )|>) | ("YInverse" -> <||>),
            Alternatives[
                "Width" -> <|"ObjectGet" -> (#1["Width"] & )|>,
                "X2" -> <|
                    "AllowUnspecifiedIfUnchanged" -> Function[
                        {
                            Daniel`ARC`Private`conclusionSoFar,
                            Daniel`ARC`Private`conclusionsBeingGeneralized
                        },
                        And[
                            MissingQ[Daniel`ARC`Private`conclusionSoFar["X"]],
                            Daniel`ARC`ARCPropertyUnchangingInConclusionsQ[
                                Daniel`ARC`Private`conclusionsBeingGeneralized,
                                "Width"
                            ]
                        ]
                    ]
                |>,
                "X2Inverse" -> <|
                    "AllowUnspecifiedIfUnchanged" -> Function[
                        {
                            Daniel`ARC`Private`conclusionSoFar,
                            Daniel`ARC`Private`conclusionsBeingGeneralized
                        },
                        And[
                            MissingQ[Daniel`ARC`Private`conclusionSoFar["X"]],
                            Daniel`ARC`ARCPropertyUnchangingInConclusionsQ[
                                Daniel`ARC`Private`conclusionsBeingGeneralized,
                                "Width"
                            ]
                        ]
                    ]
                |>
            ],
            Alternatives[
                "Height" -> <|"ObjectGet" -> (#1["Height"] & )|>,
                "Y2" -> <|
                    "AllowUnspecifiedIfUnchanged" -> Function[
                        {
                            Daniel`ARC`Private`conclusionSoFar,
                            Daniel`ARC`Private`conclusionsBeingGeneralized
                        },
                        And[
                            MissingQ[Daniel`ARC`Private`conclusionSoFar["Y"]],
                            Daniel`ARC`ARCPropertyUnchangingInConclusionsQ[
                                Daniel`ARC`Private`conclusionsBeingGeneralized,
                                "Height"
                            ]
                        ]
                    ]
                |>,
                "Y2Inverse" -> <|
                    "AllowUnspecifiedIfUnchanged" -> Function[
                        {
                            Daniel`ARC`Private`conclusionSoFar,
                            Daniel`ARC`Private`conclusionsBeingGeneralized
                        },
                        And[
                            MissingQ[Daniel`ARC`Private`conclusionSoFar["Y"]],
                            Daniel`ARC`ARCPropertyUnchangingInConclusionsQ[
                                Daniel`ARC`Private`conclusionsBeingGeneralized,
                                "Height"
                            ]
                        ]
                    ]
                |>
            ],
            "ZOrder" -> <||>
        }
    }
    ,
    TestID -> "ARCSubPropertySets-20220824-T9149Q"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCSubPropertySets[
            <|"SubProperties" -> {"Y" | Missing["Y"], "X" | Missing["Y"]}|>
        ]
    ]
    ,
    {{("Y" -> <||>) | (Missing["Y"] -> <||>), ("X" -> <||>) | (Missing["Y"] -> <||>)}}
    ,
    TestID -> "ARCSubPropertySets-20220826-XZS2EB"
]

Test[
    Daniel`ARC`ARCSubPropertySets[
        <|
            "MinimalPropertySets" -> {
                {"Image", "Y", "X"},
                {"Shape", "Width", "Height", "Color", "Y", "X"}
            }
        |>
    ]
    ,
    {
        {"Image" -> <||>, "Y" -> <||>, "X" -> <||>},
        {
            "Shape" -> <||>,
            "Width" -> <||>,
            "Height" -> <||>,
            "Color" -> <||>,
            "Y" -> <||>,
            "X" -> <||>
        }
    }
    ,
    TestID -> "ARCSubPropertySets-20220827-4W9THQ"
]

Test[
    Daniel`ARC`ARCSubPropertySets[
        <|
            "MinimalPropertySets" -> {
                {"Image", "Y", "X"},
                {"Shape", "Width", "Height", "Color", "Y", "X"}
            },
            "SubProperties" -> {
                "Image" -> <||>,
                "Y" -> <||>,
                "X" -> <||>,
                "Shape" -> <||>,
                "Color" -> <||>,
                "Width" -> <||>,
                "Height" -> <||>
            }
        |>
    ]
    ,
    {
        {"Image" -> <||>, "Y" -> <||>, "X" -> <||>},
        {
            "Shape" -> <||>,
            "Width" -> <||>,
            "Height" -> <||>,
            "Color" -> <||>,
            "Y" -> <||>,
            "X" -> <||>
        }
    }
    ,
    TestID -> "ARCSubPropertySets-20220827-CAJVYZ"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCSubPropertySets[
            Utility`Gett[
                Daniel`ARC`Private`$transformTypes["AddComponents", "SubProperties"],
                "Components"
            ]
        ]
    ]
    ,
    {
        {
            "Image" -> <||>,
            "Position" -> <|
                "SubProperties" -> {
                    "RelativePosition" -> <|
                        "SubProperties" -> {"Y" | "YInverse", "X" | "XInverse"}
                    |>,
                    "Y" -> <|"ObjectGet" -> (#1["Y"] & )|>,
                    "X" -> <|"ObjectGet" -> (#1["X"] & )|>
                },
                "MinimalPropertySets" -> {{"RelativePosition"}, {"Y", "X"}}
            |>
        },
        {
            Alternatives[
                "Shape" -> <|
                    "ObjectGet" -> (#1["Shape"] & ),
                    "MinimalPropertySets" -> {{"Name", "Angle", "Fill" | Missing["Fill"]}}
                |>,
                "MonochromeImage" -> <||>,
                "Shapes" -> <|
                    "ClassList" -> True,
                    "Condition" -> Hold[
                        Not[
                            Daniel`ARC`ARCConclusionsSoFarMatchQ[
                                #1,
                                "Shape",
                                Alternatives[KeyValuePattern["Fill" -> _]]
                            ]
                        ]
                    ]
                |>
            ],
            Alternatives[
                "Color" -> <|"ObjectGet" -> (#1["Color"] & )|>,
                Missing["Color"] -> <|
                    "ObjectGet" -> (#1["Color"] & ),
                    "Condition" -> Hold[
                        Daniel`ARC`ARCConclusionsSoFarMatchQ[
                            #1,
                            "Shape",
                            Alternatives[
                                KeyValuePattern[
                                    {
                                        "Border" -> KeyValuePattern["Color" -> _],
                                        "Interior" -> KeyValuePattern["Color" -> _]
                                    }
                                ],
                                KeyValuePattern["Fill" -> _]
                            ]
                        ]
                    ]
                |>
            ],
            "Position" -> <|
                "SubProperties" -> {
                    "RelativePosition" -> <|
                        "SubProperties" -> {"Y" | "YInverse", "X" | "XInverse"}
                    |>,
                    "Y" -> <|"ObjectGet" -> (#1["Y"] & )|>,
                    "X" -> <|"ObjectGet" -> (#1["X"] & )|>
                },
                "MinimalPropertySets" -> {{"RelativePosition"}, {"Y", "X"}}
            |>,
            "Width" -> <|"ObjectGet" -> (#1["Width"] & )|>,
            "Height" -> <|"ObjectGet" -> (#1["Height"] & )|>
        },
        {
            "Outward" -> <||>,
            "Shape" -> <|
                "ObjectGet" -> (#1["Shape"] & ),
                "MinimalPropertySets" -> {{"Name", "Angle", "Fill" | Missing["Fill"]}}
            |>,
            "Direction" -> <||>,
            "Color" -> <|"ObjectGet" -> (#1["Color"] & )|>,
            Alternatives[
                "Y" -> <|
                    "ObjectGet" -> (#1["Y"] & ),
                    "Condition" -> Hold[MatchQ[#1["Direction"], {0, -1} | {0, 1}]]
                |>,
                "X" -> <|
                    "ObjectGet" -> (#1["X"] & ),
                    "Condition" -> Hold[MatchQ[#1["Direction"], {-1, 0} | {1, 0}]]
                |>
            ]
        }
    }
    ,
    TestID -> "ARCSubPropertySets-20220827-YX24O2"
]