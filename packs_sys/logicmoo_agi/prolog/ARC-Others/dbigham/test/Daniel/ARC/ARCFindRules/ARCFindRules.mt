(*
    Tests for: Daniel`ARC`ARCFindRules
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCFindRules]
    
    Author: danielb
*)

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`ARCSimplifyRules[
                Utility`BlockUUID[
                    Daniel`ARC`ARCFindRules[
                        Daniel`ARC`ARCParseFile[file = "0ca9ddb6"]["Train"]
                    ]
                ]
            ]
        ]
    ]
    ,
    {
        <|"Colors" -> {1}|> -> <|
            "Transform" -> <|
                "Type" -> "AddComponents",
                "Components" -> {
                    <|
                        "Image" -> Daniel`ARC`ARCScene[{{-1, 7, -1}, {7, -1, 7}, {-1, 7, -1}}],
                        "Position" -> <|"RelativePosition" -> <|"Y" -> -1, "X" -> -1|>|>
                    |>
                }
            |>
        |>,
        <|"Colors" -> {2}|> -> <|
            "Transform" -> <|
                "Type" -> "AddComponents",
                "Components" -> {
                    <|
                        "Image" -> Daniel`ARC`ARCScene[{{4}}],
                        "Position" -> <|"RelativePosition" -> <|"Y" -> -1, "X" -> -1|>|>
                    |>,
                    <|
                        "Image" -> Daniel`ARC`ARCScene[{{4}}],
                        "Position" -> <|"RelativePosition" -> <|"Y" -> -1, "X" -> 1|>|>
                    |>,
                    <|
                        "Image" -> Daniel`ARC`ARCScene[{{4}}],
                        "Position" -> <|"RelativePosition" -> <|"Y" -> 1, "X" -> -1|>|>
                    |>,
                    <|
                        "Image" -> Daniel`ARC`ARCScene[{{4}}],
                        "Position" -> <|"RelativePosition" -> <|"Y" -> 1, "X" -> 1|>|>
                    |>
                }
            |>
        |>,
        <|"Colors" -> Except[{1} | {2}]|> -> <|"Same" -> True|>
    }
    ,
    TestID -> "ARCFindRules-20220719-XYQH41"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`ARCSimplifyRules[
                Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "3c9b0459"]["Train"]]
            ]
        ]
    ]
    ,
    {<||> -> <|"Transform" -> <|"Type" -> "Rotation", "Angle" -> 180|>|>}
    ,
    TestID -> "ARCFindRules-20220722-MAF6JZ"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`ARCSimplifyRules[
                Utility`BlockUUID[
                    Daniel`ARC`ARCFindRules[
                        Daniel`ARC`ARCParseFile[file = "321b1fc6"]["Train"]
                    ]
                ]
            ]
        ]
    ]
    ,
    {
        <|"Colors" -> {8}|> -> <|
            "Image" -> Daniel`ARC`ObjectValue[<|"Colors" -> Except[{8}]|>, "Image"]
        |>,
        <|"Colors" -> Except[{8}]|> -> <|"Transform" -> <|"Type" -> "Delete"|>|>
    }
    ,
    TestID -> "ARCFindRules-20220725-AR54Q9"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`ARCSimplifyRules[
                Utility`BlockUUID[
                    Daniel`ARC`ARCFindRules[
                        Daniel`ARC`ARCParseFile[file = "05f2a901"]["Train"]
                    ]
                ]
            ]
        ]
    ]
    ,
    {
        <|"Area.Rank" -> 1|> -> <|
            "Transform" -> <|
                "Type" -> "Move",
                "BlockedBy" -> Daniel`ARC`Object[<|"Colors" -> {8}|>]
            |>
        |>,
        <|"Area.Rank" -> 2|> -> <|"Same" -> True|>
    }
    ,
    TestID -> "ARCFindRules-20220804-KVNY6K"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`ARCSimplifyRules[
                Utility`BlockUUID[
                    Daniel`ARC`ARCFindRules[
                        Daniel`ARC`ARCParseFile[file = "08ed6ac7"]["Train"]
                    ]
                ]
            ]
        ]
    ]
    ,
    {<||> -> <|"Color" -> Daniel`ARC`ObjectValue["InputObject", "Height.Rank"]|>}
    ,
    TestID -> "ARCFindRules-20220804-KVISKF"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`ARCSimplifyRules[
                Utility`BlockUUID[
                    Daniel`ARC`ARCFindRules[
                        Daniel`ARC`ARCParseFile[file = "2wfys5w64-relative-right-side"][
                            "Train"
                        ]
                    ]
                ]
            ]
        ]
    ]
    ,
    {
        <|"Shapes" -> <|"Name" -> "Square"|>|> -> <|
            "Transform" -> <|
                "Type" -> "AddComponents",
                "Components" -> {
                    <|
                        "Image" -> Daniel`ARC`ARCScene[{{2}}],
                        "Position" -> <|"RelativePosition" -> <|"Y" -> -1, "XInverse" -> 0|>|>
                    |>
                }
            |>
        |>,
        <|"Shapes" -> Except[<|"Name" -> "Square"|>]|> -> <|"Same" -> True|>
    }
    ,
    TestID -> "ARCFindRules-20220807-UIY7RU"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`ARCSimplifyRules[
                Utility`BlockUUID[
                    Daniel`ARC`ARCFindRules[
                        Daniel`ARC`ARCParseFile[file = "ifmyulnv8-shape"]["Train"]
                    ]
                ]
            ]
        ]
    ]
    ,
    {
        <|"Shape" -> <|"Name" -> "Pixel"|>|> -> <|"Transform" -> <|"Type" -> "Delete"|>|>,
        <|"Shape" -> <|"Name" -> "Square", "Filled" -> False|>|> -> <|
            "Transform" -> <|
                "Type" -> "AddComponents",
                "Components" -> {
                    <|
                        "Shape" -> <|"Name" -> "Square", "Filled" -> True|>,
                        "Color" -> Daniel`ARC`ObjectValue[<|"Colors" -> Except[{5}]|>, "Color"],
                        "Position" -> <|"RelativePosition" -> <|"Y" -> 1, "X" -> 1|>|>,
                        "Width" -> Inactive[Plus][
                            Daniel`ARC`ObjectValue["InputObject", "Width"],
                            -2
                        ],
                        "Height" -> Inactive[Plus][
                            Daniel`ARC`ObjectValue["InputObject", "Height"],
                            -2
                        ]
                    |>
                }
            |>
        |>
    }
    ,
    TestID -> "ARCFindRules-20220809-GFDCR1"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`ARCSimplifyRules[
                Utility`BlockUUID[
                    Daniel`ARC`ARCFindRules[
                        Daniel`ARC`ARCParseFile[file = "253bf280"]["Train"]
                    ]
                ]
            ]
        ]
    ]
    ,
    <|
        "Rules" -> {
            <|"Angle" -> 0|> -> <|
                "Transform" -> <|
                    "Type" -> "AddComponents",
                    "Components" -> {
                        <|
                            "Shape" -> <|"Name" -> "Line", "Angle" -> 0|>,
                            "Color" -> 3,
                            "Position" -> <|"RelativePosition" -> <|"Y" -> 0, "X" -> 1|>|>,
                            "Width" -> Inactive[Plus][
                                Daniel`ARC`ObjectValue["InputObject", "Width"],
                                -2
                            ],
                            "Height" -> 1
                        |>
                    }
                |>
            |>,
            <|"Angle" -> 90|> -> <|
                "Transform" -> <|
                    "Type" -> "AddComponents",
                    "Components" -> {
                        <|
                            "Shape" -> <|"Name" -> "Line", "Angle" -> 90|>,
                            "Color" -> 3,
                            "Position" -> <|"RelativePosition" -> <|"Y" -> 1, "X" -> 0|>|>,
                            "Width" -> 1,
                            "Height" -> Inactive[Plus][
                                Daniel`ARC`ObjectValue["InputObject", "Height"],
                                -2
                            ]
                        |>
                    }
                |>
            |>,
            <|"Angle" -> Missing[]|> -> <|"Same" -> True|>
        },
        "Groups" -> {
            <|
                "Height" -> 1,
                "Components" -> {
                    Repeated[
                        <|
                            "Image" -> Daniel`ARC`ARCScene[{{8}}],
                            "ZOrder" -> 0,
                            "Y" -> "Same"
                        |>,
                        {2}
                    ]
                },
                "Color" -> 8,
                "ZOrder" -> 0,
                "FilledArea" -> 2,
                "SurfacePixelCount" -> 2,
                "HollowCount" -> 0
            |>,
            <|
                "Width" -> 1,
                "Components" -> {
                    Repeated[
                        <|
                            "Image" -> Daniel`ARC`ARCScene[{{8}}],
                            "ZOrder" -> 0,
                            "X" -> "Same"
                        |>,
                        {2}
                    ]
                },
                "Color" -> 8,
                "ZOrder" -> 0,
                "FilledArea" -> 2,
                "SurfacePixelCount" -> 2,
                "HollowCount" -> 0
            |>
        }
    |>
    ,
    TestID -> "ARCFindRules-20220812-24SCQ1"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`ARCSimplifyRules[
                Utility`BlockUUID[
                    Daniel`ARC`ARCFindRules[
                        Daniel`ARC`ARCParseFile[file = "3ac3eb23"]["Train"]
                    ]
                ]
            ]
        ]
    ]
    ,
    {
        <||> -> <|
            "Shape" -> Daniel`ARC`ARCScene[
                {
                    {-1, 10, -1},
                    {10, -1, 10},
                    {-1, 10, -1},
                    {10, -1, 10},
                    {-1, 10, -1},
                    {10, -1, 10}
                }
            ],
            "X" -> Inactive[Plus][Daniel`ARC`ObjectValue["InputObject", "X"], -1]
        |>
    }
    ,
    TestID -> "ARCFindRules-20220817-R66XW8"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`ARCSimplifyRules[
                Utility`BlockUUID[
                    Daniel`ARC`ARCFindRules[
                        Daniel`ARC`ARCParseFile[file = "referenceable-components"]["Train"]
                    ]
                ]
            ]
        ]
    ]
    ,
    {
        <|"Colors" -> {1}|> -> <|"Same" -> True|>,
        <|"Colors" -> {1, 5}|> -> <|
            "Transform" -> <|
                "Type" -> "AddComponents",
                "Components" -> {
                    <|
                        "Image" -> Daniel`ARC`ARCScene[{{2}}],
                        "Position" -> <|
                            "RelativePosition" -> <|
                                "Y" -> Daniel`ARC`ObjectValue[
                                    <|"Colors" -> {1}, "Context" -> "Component"|>,
                                    "YRelative"
                                ],
                                "X" -> Inactive[Plus][
                                    Daniel`ARC`ObjectValue[
                                        <|"Colors" -> {1}, "Context" -> "Component"|>,
                                        "XRelative"
                                    ],
                                    1
                                ]
                            |>
                        |>
                    |>
                }
            |>
        |>
    }
    ,
    TestID -> "ARCFindRules-20220817-V92ONB"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`ARCSimplifyRules[
                Utility`BlockUUID[
                    Daniel`ARC`ARCFindRules[
                        Daniel`ARC`ARCParseFile[file = "5521c0d9"]["Train"]
                    ]
                ]
            ]
        ]
    ]
    ,
    {
        <||> -> <|
            "Transform" -> <|
                "Type" -> "Move",
                "Offset" -> <|
                    "Y" -> Inactive[Plus][Daniel`ARC`ObjectValue["InputObject", "Y"], -16]
                |>
            |>
        |>
    }
    ,
    TestID -> "ARCFindRules-20220819-2LNLJY"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`ARCSimplifyRules[
                Utility`BlockUUID[
                    Daniel`ARC`ARCFindRules[
                        Daniel`ARC`ARCParseFile[file = "6c434453"]["Train"]
                    ]
                ]
            ]
        ]
    ]
    ,
    {
        <|"HollowCount" -> 0|> -> <|"Same" -> True|>,
        <|"HollowCount" -> 1|> -> <|
            "Image" -> Daniel`ARC`ARCScene[{{-1, 2, -1}, {2, 2, 2}, {-1, 2, -1}}]
        |>
    }
    ,
    TestID -> "ARCFindRules-20220819-F8C648"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`ARCSimplifyRules[
                Utility`BlockUUID[
                    Daniel`ARC`ARCFindRules[
                        Daniel`ARC`ARCParseFile[file = "6e82a1ae"]["Train"]
                    ]
                ]
            ]
        ]
    ]
    ,
    {<||> -> <|"Color" -> Daniel`ARC`ObjectValue["InputObject", "FilledArea.Rank"]|>}
    ,
    TestID -> "ARCFindRules-20220819-N9WT39"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`ARCSimplifyRules[
                Utility`BlockUUID[
                    Daniel`ARC`ARCFindRules[
                        Daniel`ARC`ARCParseFile[file = "aabf363d"]["Train"]
                    ]
                ]
            ]
        ]
    ]
    ,
    {
        <|"Shape" -> <|"Name" -> "Pixel"|>|> -> <|"Transform" -> <|"Type" -> "Delete"|>|>,
        <|"Shape" -> Except[<|"Name" -> "Pixel"|>]|> -> <|
            "Color" -> Daniel`ARC`ObjectValue[
                <|"MonochromeImage" -> Daniel`ARC`ARCScene[{{10}}]|>,
                "Color"
            ]
        |>
    }
    ,
    TestID -> "ARCFindRules-20220819-W87PMH"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`ARCSimplifyRules[
                Utility`BlockUUID[
                    Daniel`ARC`ARCFindRules[
                        Daniel`ARC`ARCParseFile[file = "25d8a9c8"]["Train"]
                    ]
                ]
            ]
        ]
    ]
    ,
    <|
        "FormMultiColorCompositeObjects" -> False,
        "Rules" -> {
            <|"Width" -> 3|> -> <|"Color" -> 5|>,
            <|"Width" -> Except[3]|> -> <|"Transform" -> <|"Type" -> "Delete"|>|>
        }
    |>
    ,
    TestID -> "ARCFindRules-20220819-P3KT28"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`ARCSimplifyRules[
                Utility`BlockUUID[
                    Daniel`ARC`ARCFindRules[
                        Daniel`ARC`ARCParseFile[file = "b1948b0a"]["Train"]
                    ]
                ]
            ]
        ]
    ]
    ,
    <|
        "FormMultiColorCompositeObjects" -> False,
        "Rules" -> {
            <|"Colors" -> {6}|> -> <|"Color" -> 2|>,
            <|"Colors" -> {7}|> -> <|"Same" -> True|>
        }
    |>
    ,
    TestID -> "ARCFindRules-20220819-LOF0LU"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`ARCSimplifyRules[
                Utility`BlockUUID[
                    Daniel`ARC`ARCFindRules[
                        Daniel`ARC`ARCParseFile[file = "d511f180"]["Train"]
                    ]
                ]
            ]
        ]
    ]
    ,
    <|
        "FormMultiColorCompositeObjects" -> False,
        "Rules" -> {
            <|"Colors" -> {1} | {2} | {3} | {4} | {6} | {7} | {9}|> -> <|"Same" -> True|>,
            <|"Colors" -> {5}|> -> <|"Color" -> 8|>,
            <|"Colors" -> {8}|> -> <|"Color" -> 5|>
        }
    |>
    ,
    TestID -> "ARCFindRules-20220819-M8D92N"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`ARCSimplifyRules[
                Utility`BlockUUID[
                    Daniel`ARC`ARCFindRules[
                        Daniel`ARC`ARCParseFile[file = "31aa019c"]["Train"]
                    ]
                ]
            ]
        ]
    ]
    ,
    <|
        "FormMultiColorCompositeObjects" -> False,
        "Rules" -> {
            <|"ColorUseCount" -> 1|> -> <|"Same" -> True|>,
            <|"ColorUseCount" -> Except[1]|> -> <|"Transform" -> <|"Type" -> "Delete"|>|>,
            <|
                "Transform" -> <|
                    "Type" -> "AddObjects",
                    "Objects" -> {
                        <|
                            "Image" -> Daniel`ARC`ARCScene[{{2, 2, 2}, {2, -1, 2}, {2, 2, 2}}],
                            "Y" -> Inactive[Plus][
                                Daniel`ARC`ObjectValue[<|"ColorUseCount" -> 1|>, "Y"],
                                -1
                            ],
                            "X" -> Inactive[Plus][
                                Daniel`ARC`ObjectValue[<|"ColorUseCount" -> 1|>, "X"],
                                -1
                            ]
                        |>
                    }
                |>
            |>
        }
    |>
    ,
    TestID -> "ARCFindRules-20220820-5QD3K6"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`ARCSimplifyRules[
                Utility`BlockUUID[
                    Daniel`ARC`ARCFindRules[
                        Daniel`ARC`ARCParseFile[file = "25d487eb"]["Train"]
                    ]
                ]
            ]
        ]
    ]
    ,
    {
        <||> -> <|
            "RotationNormalization" -> True,
            "Transform" -> <|
                "Type" -> "AddComponents",
                "Components" -> {
                    <|
                        "Outward" -> True,
                        "Shape" -> <|"Name" -> "Line", "Angle" -> 90|>,
                        "Direction" -> {-1, 0},
                        "Color" -> Daniel`ARC`ObjectValue[
                            <|
                                "MonochromeImage" -> Daniel`ARC`ARCScene[{{10}}],
                                "Context" -> "Component"
                            |>,
                            "Color"
                        ],
                        "X" -> Daniel`ARC`ObjectValue[
                            <|
                                "MonochromeImage" -> Daniel`ARC`ARCScene[{{10}}],
                                "Context" -> "Component"
                            |>,
                            "X"
                        ]
                    |>
                }
            |>
        |>
    }
    ,
    TestID -> "ARCFindRules-20220821-YXTCPM"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`ARCSimplifyRules[
                Utility`BlockUUID[
                    Daniel`ARC`ARCFindRules[
                        Daniel`ARC`ARCParseFile[file = "0962bcdd"]["Train"]
                    ]
                ]
            ]
        ]
    ]
    ,
    <|
        "FormMultiColorCompositeObjects" -> False,
        "Rules" -> {
            <|"Area.Rank" -> 1|> -> <|
                "Shape" -> Daniel`ARC`ARCScene[
                    {
                        {-1, -1, 10, -1, -1},
                        {-1, -1, 10, -1, -1},
                        {10, 10, -1, 10, 10},
                        {-1, -1, 10, -1, -1},
                        {-1, -1, 10, -1, -1}
                    }
                ],
                "X" -> Inactive[Plus][Daniel`ARC`ObjectValue["InputObject", "X"], -1],
                "Y" -> Inactive[Plus][Daniel`ARC`ObjectValue["InputObject", "Y"], -1]
            |>,
            <|"Area.Rank" -> 2|> -> <|
                "Shape" -> Daniel`ARC`ARCScene[
                    {
                        {10, -1, -1, -1, 10},
                        {-1, 10, -1, 10, -1},
                        {-1, -1, 10, -1, -1},
                        {-1, 10, -1, 10, -1},
                        {10, -1, -1, -1, 10}
                    }
                ],
                "X" -> Inactive[Plus][Daniel`ARC`ObjectValue["InputObject", "X"], -2],
                "Y" -> Inactive[Plus][Daniel`ARC`ObjectValue["InputObject", "Y"], -2]
            |>
        }
    |>
    ,
    TestID -> "ARCFindRules-20220822-FVFZUH"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`ARCSimplifyRules[
                Utility`BlockUUID[
                    Daniel`ARC`ARCFindRules[
                        Daniel`ARC`ARCParseFile[file = "0d3d703e"]["Train"]
                    ]
                ]
            ]
        ]
    ]
    ,
    {
        <|"Colors" -> {1}|> -> <|"Color" -> 5|>,
        <|"Colors" -> {2}|> -> <|"Color" -> 6|>,
        <|"Colors" -> {3}|> -> <|"Color" -> 4|>,
        <|"Colors" -> {4}|> -> <|"Color" -> 3|>,
        <|"Colors" -> {5}|> -> <|"Color" -> 1|>,
        <|"Colors" -> {6}|> -> <|"Color" -> 2|>,
        <|"Colors" -> {8}|> -> <|"Color" -> 9|>,
        <|"Colors" -> {9}|> -> <|"Color" -> 8|>
    }
    ,
    TestID -> "ARCFindRules-20220822-U3G40I"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`ARCSimplifyRules[
                Utility`BlockUUID[
                    Daniel`ARC`ARCFindRules[
                        Daniel`ARC`ARCParseFile[file = "1bfc4729"]["Train"]
                    ]
                ]
            ]
        ]
    ]
    ,
    <|
        "FormMultiColorCompositeObjects" -> False,
        "Rules" -> {
            <|"Y.Rank" -> 1|> -> <|
                "Shape" -> Daniel`ARC`ARCScene[
                    {
                        {10, -1, -1, -1, -1, -1, -1, -1, -1, 10},
                        {10, -1, -1, -1, -1, -1, -1, -1, -1, 10},
                        {10, 10, 10, 10, 10, 10, 10, 10, 10, 10},
                        {10, -1, -1, -1, -1, -1, -1, -1, -1, 10},
                        {10, 10, 10, 10, 10, 10, 10, 10, 10, 10}
                    }
                ],
                "X" -> 1,
                "Y" -> 6
            |>,
            <|"Y.Rank" -> 2|> -> <|
                "Shape" -> Daniel`ARC`ARCScene[
                    {
                        {10, 10, 10, 10, 10, 10, 10, 10, 10, 10},
                        {10, -1, -1, -1, -1, -1, -1, -1, -1, 10},
                        {10, 10, 10, 10, 10, 10, 10, 10, 10, 10},
                        {10, -1, -1, -1, -1, -1, -1, -1, -1, 10},
                        {10, -1, -1, -1, -1, -1, -1, -1, -1, 10}
                    }
                ],
                "X" -> 1,
                "Y" -> 1
            |>
        }
    |>
    ,
    TestID -> "ARCFindRules-20220822-45GA4N"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`ARCSimplifyRules[
                Utility`BlockUUID[
                    Daniel`ARC`ARCFindRules[
                        Daniel`ARC`ARCParseFile[file = "178fcbfb-easier"]["Train"]
                    ]
                ]
            ]
        ]
    ]
    ,
    {<||> -> <|"Shape" -> <|"Name" -> "Line", "Angle" -> 0|>, "X" -> 1, "X2Inverse" -> 1|>}
    ,
    TestID -> "ARCFindRules-20220826-CKGJI0"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`ARCSimplifyRules[
                Utility`BlockUUID[
                    Daniel`ARC`ARCFindRules[
                        Daniel`ARC`ARCParseFile[file = "4347f46a"]["Train"]
                    ]
                ]
            ]
        ]
    ]
    ,
    {<||> -> <|"Shape" -> <|"Name" -> "Rectangle", "Filled" -> False|>|>}
    ,
    TestID -> "ARCFindRules-20220826-6ENYJE"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`ARCSimplifyRules[
                Utility`BlockUUID[
                    Daniel`ARC`ARCFindRules[
                        Daniel`ARC`ARCParseFile[file = "fc754716"]["Train"]
                    ]
                ]
            ]
        ]
    ]
    ,
    {
        <||> -> <|
            "Shape" -> <|"Name" -> "Rectangle", "Filled" -> False|>,
            "X" -> 1,
            "Y" -> 1,
            "X2Inverse" -> 1,
            "Y2Inverse" -> 1
        |>
    }
    ,
    TestID -> "ARCFindRules-20220826-SXWYIL"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`ARCSimplifyRules[
                Utility`BlockUUID[
                    Daniel`ARC`ARCFindRules[
                        Daniel`ARC`ARCParseFile[file = "178fcbfb"]["Train"]
                    ]
                ]
            ]
        ]
    ]
    ,
    {
        <|"Colors" -> {1} | {3}|> -> <|
            "Shape" -> <|"Name" -> "Line", "Angle" -> 0|>,
            "X" -> 1,
            "X2Inverse" -> 1
        |>,
        <|"Colors" -> {2}|> -> <|
            "Shape" -> <|"Name" -> "Line", "Angle" -> 90|>,
            "Y" -> 1,
            "Y2Inverse" -> 1,
            "ZOrder" -> 1
        |>
    }
    ,
    TestID -> "ARCFindRules-20220827-P7K0UM"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`ARCSimplifyRules[
                Utility`BlockUUID[
                    Daniel`ARC`ARCFindRules[
                        Daniel`ARC`ARCParseFile[file = "1f876c06"]["Train"]
                    ]
                ]
            ]
        ]
    ]
    ,
    <|
        "FormMultiColorCompositeObjects" -> False,
        "Rules" -> {
            <||> -> <|
                "Shape" -> <|
                    "Name" -> "Line",
                    "Angle" -> Daniel`ARC`ObjectValue["InputObject", "Angle"]
                |>
            |>
        },
        "Groups" -> {
            <|
                "Components" -> {
                    Repeated[
                        <|
                            "Shape" -> <|"Name" -> "Pixel"|>,
                            "ZOrder" -> 0,
                            "Image" -> "Same"
                        |>,
                        {2}
                    ]
                },
                "ZOrder" -> 0,
                "AspectRatio" -> 1,
                "FilledArea" -> 2,
                "SurfacePixelCount" -> 2,
                "VerticalLineSymmetry" -> False,
                "HorizontalLineSymmetry" -> False,
                "VerticalAndHorizontalLineSymmetry" -> False,
                "HollowCount" -> 0
            |>
        }
    |>
    ,
    TestID -> "ARCFindRules-20220827-E1WZVT"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`ARCSimplifyRules[
                Utility`BlockUUID[
                    Daniel`ARC`ARCFindRules[
                        Daniel`ARC`ARCParseFile[file = "56ff96f3"]["Train"]
                    ]
                ]
            ]
        ]
    ]
    ,
    <|
        "Rules" -> {<||> -> <|"Shape" -> <|"Name" -> "Rectangle", "Filled" -> True|>|>},
        "Groups" -> {
            <|
                "Components" -> {
                    Repeated[
                        <|
                            "Shape" -> <|"Name" -> "Pixel"|>,
                            "ZOrder" -> 0,
                            "Image" -> "Same"
                        |>,
                        {2}
                    ]
                },
                "ZOrder" -> 0,
                "FilledArea" -> 2,
                "SurfacePixelCount" -> 2,
                "VerticalLineSymmetry" -> False,
                "HorizontalLineSymmetry" -> False,
                "VerticalAndHorizontalLineSymmetry" -> False,
                "HollowCount" -> 0
            |>
        }
    |>
    ,
    TestID -> "ARCFindRules-20220827-IC01P3"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`ARCSimplifyRules[
                Utility`BlockUUID[
                    Daniel`ARC`ARCFindRules[
                        Daniel`ARC`ARCParseFile[file = "22eb0ac0"]["Train"]
                    ]
                ]
            ]
        ]
    ]
    ,
    <|
        "Rules" -> {
            <||> -> <|
                "Shapes" -> {
                    <|"Name" -> "Line", "Angle" -> 0|>,
                    <|"Name" -> "Rectangle", "Filled" -> True|>
                }
            |>
        },
        "Groups" -> {
            <|
                "X" -> 1,
                "X2" -> 10,
                "Width" -> 10,
                "Height" -> 1,
                "Components" -> {
                    Repeated[
                        <|
                            "Shape" -> <|"Name" -> "Pixel"|>,
                            "ZOrder" -> 0,
                            "Image" -> "Same",
                            "Y" -> "Same"
                        |>,
                        {2}
                    ]
                },
                "ZOrder" -> 0,
                "FilledArea" -> 2,
                "SurfacePixelCount" -> 2,
                "VerticalLineSymmetry" -> True,
                "HollowCount" -> 0
            |>
        }
    |>
    ,
    TestID -> "ARCFindRules-20220828-SUS7QU"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`ARCSimplifyRules[
                Utility`BlockUUID[
                    Daniel`ARC`ARCFindRules[
                        Daniel`ARC`ARCParseFile[file = "746b3537"]["Train"]
                    ]
                ]
            ]
        ]
    ]
    ,
    <|
        "FormMultiColorCompositeObjects" -> False,
        "RemoveEmptySpace" -> True,
        "Rules" -> {
            <||> -> <|
                "Shape" -> <|"Name" -> "Pixel"|>,
                "X" -> Daniel`ARC`ObjectValue["InputObject", "X.InverseRank"],
                "Y" -> Daniel`ARC`ObjectValue["InputObject", "Y.InverseRank"]
            |>
        }
    |>
    ,
    TestID -> "ARCFindRules-20220828-5OGS6D"
]

Test[
    Daniel`ARC`ARCSimplifyRules[
        Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "4be741c5"]["Train"]]
    ]
    ,
    <|
        "FormMultiColorCompositeObjects" -> False,
        "RemoveEmptySpace" -> True,
        "Rules" -> {
            <||> -> <|
                "Shape" -> <|"Name" -> "Pixel"|>,
                "X" -> Daniel`ARC`ObjectValue["InputObject", "X.InverseRank"],
                "Y" -> Daniel`ARC`ObjectValue["InputObject", "Y.InverseRank"]
            |>
        }
    |>
    ,
    TestID -> "ARCFindRules-20220828-MUUZ8N"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`ARCSimplifyRules[
                Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "90c28cc7"]["Train"]]
            ]
        ]
    ]
    ,
    <|
        "FormMultiColorCompositeObjects" -> False,
        "RemoveEmptySpace" -> True,
        "Rules" -> {
            <||> -> <|
                "Shapes" -> {
                    <|"Name" -> "Line", "Angle" -> 0|>,
                    <|"Name" -> "Rectangle", "Filled" -> True|>
                },
                "X" -> Daniel`ARC`ObjectValue["InputObject", "X.InverseRank"],
                "Y" -> Daniel`ARC`ObjectValue["InputObject", "Y.InverseRank"],
                "X2" -> Daniel`ARC`ObjectValue["InputObject", "X2.InverseRank"],
                "Height" -> 1
            |>
        }
    |>
    ,
    TestID -> "ARCFindRules-20220828-L8TAAG"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`ARCSimplifyRules[
                Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "a87f7484"]["Train"]]
            ]
        ]
    ]
    ,
    <|
        "FormMultiColorCompositeObjects" -> False,
        "RemoveEmptySpace" -> True,
        "Rules" -> {
            <|"Position" -> Except[{1, 1}]|> -> <|"Transform" -> <|"Type" -> "Delete"|>|>,
            <|"Position" -> {1, 1}|> -> <|
                "Image" -> Daniel`ARC`ObjectValue[<|"ShapeUseCount" -> 1|>, "Image"]
            |>
        }
    |>
    ,
    TestID -> "ARCFindRules-20220828-EY9YMC"
]

Test[
    Daniel`ARC`ARCSimplifyRules[
        Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "e9afcf9a"]["Train"]]
    ]
    ,
    <|
        "FormMultiColorCompositeObjects" -> False,
        "Rules" -> {
            <|"Position" -> {1, 1}|> -> <|
                "Shape" -> Daniel`ARC`ARCScene[
                    {{10, -1, 10, -1, 10, -1}, {-1, 10, -1, 10, -1, 10}}
                ]
            |>,
            <|"Position" -> {2, 1}|> -> <|
                "Shape" -> Daniel`ARC`ARCScene[
                    {{-1, 10, -1, 10, -1, 10}, {10, -1, 10, -1, 10, -1}}
                ],
                "Y" -> 1
            |>
        }
    |>
    ,
    TestID -> "ARCFindRules-20220828-AQHB4Z"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`ARCSimplifyRules[
                Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "f8ff0b80"]["Train"]]
            ]
        ]
    ]
    ,
    <|
        "RemoveEmptySpace" -> True,
        "Rules" -> {
            <||> -> <|
                "Shape" -> <|"Name" -> "Pixel"|>,
                "X" -> 1,
                "Y" -> Daniel`ARC`ObjectValue["InputObject", "Area.Rank"]
            |>
        }
    |>
    ,
    TestID -> "ARCFindRules-20220828-365IJ8"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`ARCSimplifyRules[
                Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "6F8CD79B"]["Train"]]
            ]
        ]
    ]
    ,
    {
        <|
            "Transform" -> <|
                "Type" -> "AddObjects",
                "Objects" -> {
                    <|
                        "Shape" -> <|"Name" -> "Rectangle", "Filled" -> False|>,
                        "Color" -> 8,
                        "X" -> 1,
                        "Y" -> 1,
                        "X2Inverse" -> 1,
                        "Y2Inverse" -> 1,
                        "ZOrder" -> 0
                    |>
                }
            |>
        |>
    }
    ,
    TestID -> "ARCFindRules-20220828-RTMMEN"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`ARCSimplifyRules[
                Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "72CA375D"]["Train"]]
            ]
        ]
    ]
    ,
    <|
        "RemoveEmptySpace" -> True,
        "Rules" -> {
            <|"VerticalLineSymmetry" -> True|> -> <|
                "Transform" -> <|"Type" -> "Move", "Position" -> <|"Y" -> 1, "X" -> 1|>|>
            |>,
            <|"VerticalLineSymmetry" -> False|> -> <|"Transform" -> <|"Type" -> "Delete"|>|>
        }
    |>
    ,
    TestID -> "ARCFindRules-20220828-2HEGQB"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`ARCSimplifyRules[
                Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "A79310A0"]["Train"]]
            ]
        ]
    ]
    ,
    {
        <||> -> <|
            "Color" -> 2,
            "Y" -> Inactive[Plus][Daniel`ARC`ObjectValue["InputObject", "Y"], 1]
        |>
    }
    ,
    TestID -> "ARCFindRules-20220828-KZK6G6"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`ARCSimplifyRules[
                Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "40853293"]["Train"]]
            ]
        ]
    ]
    ,
    <|
        "FormMultiColorCompositeObjects" -> False,
        "Rules" -> {
            <|"Angle" -> 0|> -> <|"Shape" -> <|"Name" -> "Line", "Angle" -> 0|>, "ZOrder" -> 1|>,
            <|"Angle" -> 90|> -> <|"Shape" -> <|"Name" -> "Line", "Angle" -> 90|>|>
        },
        "Groups" -> {
            <|
                "Width" -> 1,
                "Components" -> {
                    Repeated[
                        <|
                            "Shape" -> <|"Name" -> "Pixel"|>,
                            "ZOrder" -> 0,
                            "Image" -> "Same",
                            "X" -> "Same"
                        |>,
                        {2}
                    ]
                },
                "ZOrder" -> 0,
                "FilledArea" -> 2,
                "SurfacePixelCount" -> 2,
                "HollowCount" -> 0
            |>,
            <|
                "Height" -> 1,
                "Components" -> {
                    Repeated[
                        <|
                            "Shape" -> <|"Name" -> "Pixel"|>,
                            "ZOrder" -> 0,
                            "Image" -> "Same",
                            "Y" -> "Same"
                        |>,
                        {2}
                    ]
                },
                "ZOrder" -> 0,
                "FilledArea" -> 2,
                "SurfacePixelCount" -> 2,
                "HollowCount" -> 0
            |>
        }
    |>
    ,
    TestID -> "ARCFindRules-20220829-INX45J"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`ARCSimplifyRules[
                Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "1A2E2828"]["Train"]]
            ]
        ]
    ]
    ,
    <|
        "FormMultiColorCompositeObjects" -> False,
        "RemoveEmptySpace" -> True,
        "Rules" -> {
            <||> -> <|"Transform" -> <|"Type" -> "Delete"|>|>,
            <|
                "Transform" -> <|
                    "Type" -> "AddObjects",
                    "Objects" -> {
                        <|
                            "Shape" -> <|"Name" -> "Pixel"|>,
                            "Color" -> Daniel`ARC`ObjectValue[<|"ZOrder" -> 0|>, "Color"],
                            "X" -> 1,
                            "Y" -> 1,
                            "X2" -> 1,
                            "Y2" -> 1,
                            "ZOrder" -> 0
                        |>
                    }
                |>
            |>
        }
    |>
    ,
    TestID -> "ARCFindRules-20220829-P274J4"
]

Test[
    Daniel`ARC`ARCSimplifyRules[
        Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "95990924"]["Train"]]
    ]
    ,
    {
        <||> -> <|
            "Transform" -> <|
                "Type" -> "AddComponents",
                "Components" -> {
                    <|
                        "Image" -> Daniel`ARC`ARCScene[{{1}}],
                        "Position" -> <|"RelativePosition" -> <|"Y" -> -1, "X" -> -1|>|>
                    |>,
                    <|
                        "Image" -> Daniel`ARC`ARCScene[{{2}}],
                        "Position" -> <|"RelativePosition" -> <|"Y" -> -1, "X" -> 2|>|>
                    |>,
                    <|
                        "Image" -> Daniel`ARC`ARCScene[{{3}}],
                        "Position" -> <|"RelativePosition" -> <|"Y" -> 2, "X" -> -1|>|>
                    |>,
                    <|
                        "Image" -> Daniel`ARC`ARCScene[{{4}}],
                        "Position" -> <|"RelativePosition" -> <|"Y" -> 2, "X" -> 2|>|>
                    |>
                }
            |>
        |>
    }
    ,
    TestID -> "ARCFindRules-20220830-W6FS72"
]

Test[
    Daniel`ARC`ARCSimplifyRules[
        Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "BE94B721"]["Train"]]
    ]
    ,
    <|
        "RemoveEmptySpace" -> True,
        "Rules" -> {
            <|"FilledArea.Rank" -> 1|> -> <|
                "Transform" -> <|"Type" -> "Move", "Position" -> <|"Y" -> 1, "X" -> 1|>|>
            |>,
            <|"FilledArea.Rank" -> Except[1]|> -> <|"Transform" -> <|"Type" -> "Delete"|>|>
        }
    |>
    ,
    TestID -> "ARCFindRules-20220902-YWOP1G"
]

Test[
    Daniel`ARC`ARCSimplifyRules[
        Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "4258a5f9"]["Train"]]
    ]
    ,
    {
        <||> -> <|
            "Transform" -> <|
                "Type" -> "AddComponents",
                "Components" -> {
                    <|
                        "Image" -> Daniel`ARC`ARCScene[{{1, 1, 1}, {1, -1, 1}, {1, 1, 1}}],
                        "Position" -> <|"RelativePosition" -> <|"Y" -> -1, "X" -> -1|>|>
                    |>
                }
            |>
        |>
    }
    ,
    TestID -> "ARCFindRules-20220902-776QLJ"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`ARCSimplifyRules[
                Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "913fb3ed"]["Train"]]
            ]
        ]
    ]
    ,
    {
        <|"Image" -> Daniel`ARC`ARCScene[{{2}}]|> -> <|
            "Transform" -> <|
                "Type" -> "AddComponents",
                "Components" -> {
                    <|
                        "Image" -> Daniel`ARC`ARCScene[{{1, 1, 1}, {1, -1, 1}, {1, 1, 1}}],
                        "Position" -> <|"RelativePosition" -> <|"Y" -> -1, "X" -> -1|>|>
                    |>
                }
            |>
        |>,
        <|"Image" -> Daniel`ARC`ARCScene[{{3}}]|> -> <|
            "Transform" -> <|
                "Type" -> "AddComponents",
                "Components" -> {
                    <|
                        "Image" -> Daniel`ARC`ARCScene[{{6, 6, 6}, {6, -1, 6}, {6, 6, 6}}],
                        "Position" -> <|"RelativePosition" -> <|"Y" -> -1, "X" -> -1|>|>
                    |>
                }
            |>
        |>,
        <|"Image" -> Daniel`ARC`ARCScene[{{8}}]|> -> <|
            "Transform" -> <|
                "Type" -> "AddComponents",
                "Components" -> {
                    <|
                        "Image" -> Daniel`ARC`ARCScene[{{4, 4, 4}, {4, -1, 4}, {4, 4, 4}}],
                        "Position" -> <|"RelativePosition" -> <|"Y" -> -1, "X" -> -1|>|>
                    |>
                }
            |>
        |>
    }
    ,
    TestID -> "ARCFindRules-20220902-ATD7NW"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`ARCSimplifyRules[
                Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "a61ba2ce"]["Train"]]
            ]
        ]
    ]
    ,
    <|
        "RemoveEmptySpace" -> True,
        "Rules" -> {
            <|"MonochromeImage" -> Daniel`ARC`ARCScene[{{-1, 10}, {10, 10}}]|> -> <|
                "Transform" -> <|"Type" -> "Move", "Position" -> <|"Y" -> 3, "X" -> 3|>|>
            |>,
            <|"MonochromeImage" -> Daniel`ARC`ARCScene[{{10, -1}, {10, 10}}]|> -> <|
                "Transform" -> <|"Type" -> "Move", "Position" -> <|"Y" -> 3, "X" -> 1|>|>
            |>,
            <|"MonochromeImage" -> Daniel`ARC`ARCScene[{{10, 10}, {-1, 10}}]|> -> <|
                "Transform" -> <|"Type" -> "Move", "Position" -> <|"Y" -> 1, "X" -> 3|>|>
            |>,
            <|"MonochromeImage" -> Daniel`ARC`ARCScene[{{10, 10}, {10, -1}}]|> -> <|
                "Transform" -> <|"Type" -> "Move", "Position" -> <|"Y" -> 1, "X" -> 1|>|>
            |>
        }
    |>
    ,
    TestID -> "ARCFindRules-20220902-II1FMH"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`ARCSimplifyRules[
                Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "810b9b61"]["Train"]]
            ]
        ]
    ]
    ,
    {
        <|"HollowCount" -> 0|> -> <|"Same" -> True|>,
        <|"HollowCount" -> 1|> -> <|"Color" -> 3|>
    }
    ,
    TestID -> "ARCFindRules-20220902-LN1NO5"
]

Test[
    Daniel`ARC`ARCSimplifyRules[
        Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "ed36ccf7"]["Train"]]
    ]
    ,
    <|
        "SceneAsSingleObject" -> True,
        "Rules" -> {<||> -> <|"Transform" -> <|"Type" -> "Rotation", "Angle" -> 270|>|>}
    |>
    ,
    TestID -> "ARCFindRules-20220902-DZQDJD"
]

Test[
    Daniel`ARC`ARCSimplifyRules[
        Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "a740d043"]["Train"]]
    ]
    ,
    <|
        "SceneAsSingleObject" -> True,
        "RemoveEmptySpace" -> True,
        "Background" -> 0,
        "Rules" -> {<||> -> <|"Transform" -> <|"Type" -> "RemoveEmptySpace"|>|>}
    |>
    ,
    TestID -> "ARCFindRules-20220903-MEJ1SK"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`ARCSimplifyRules[
                Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "b9b7f026"]["Train"]]
            ]
        ]
    ]
    ,
    <|
        "RemoveEmptySpace" -> True,
        "Rules" -> {
            <||> -> <|"Transform" -> <|"Type" -> "Delete"|>|>,
            <|
                "Transform" -> <|
                    "Type" -> "AddObjects",
                    "Objects" -> {
                        <|
                            "Shape" -> <|"Name" -> "Pixel"|>,
                            "Color" -> Daniel`ARC`ObjectValue[<|"HollowCount" -> 1|>, "Color"],
                            "X" -> 1,
                            "Y" -> 1,
                            "X2" -> 1,
                            "Y2" -> 1,
                            "ZOrder" -> 0
                        |>
                    }
                |>
            |>
        }
    |>
    ,
    TestID -> "ARCFindRules-20220903-BKEW67"
]

Test[
    Daniel`ARC`ARCSimplifyRules[
        Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "c59eb873"]["Train"]]
    ]
    ,
    <|
        "Width" -> Inactive[Daniel`ARC`ObjectValue["InputScene", "Width"]*2],
        "Height" -> Inactive[Daniel`ARC`ObjectValue["InputScene", "Height"]*2],
        "Rules" -> {<||> -> <|"Transform" -> <|"Type" -> "Scaled", "Factor" -> 2.|>|>}
    |>
    ,
    TestID -> "ARCFindRules-20220903-NF9PTM"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`ARCSimplifyRules[
                Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "9172f3a0"]["Train"]]
            ]
        ]
    ]
    ,
    <|
        "Width" -> Inactive[Daniel`ARC`ObjectValue["InputScene", "Width"]*3],
        "Height" -> Inactive[Daniel`ARC`ObjectValue["InputScene", "Height"]*3],
        "Rules" -> {<||> -> <|"Transform" -> <|"Type" -> "Scaled", "Factor" -> 3.|>|>}
    |>
    ,
    TestID -> "ARCFindRules-20220903-7LU3CS"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`ARCSimplifyRules[
                Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "d4469b4b"]["Train"]]
            ]
        ]
    ]
    ,
    <|
        "SceneAsSingleObject" -> True,
        "RemoveEmptySpace" -> True,
        "Rules" -> {
            <|"Colors" -> {1}|> -> <|
                "Image" -> Daniel`ARC`ARCScene[{{-1, 5, -1}, {5, 5, 5}, {-1, 5, -1}}]
            |>,
            <|"Colors" -> {2}|> -> <|
                "Image" -> Daniel`ARC`ARCScene[{{5, 5, 5}, {-1, 5, -1}, {-1, 5, -1}}]
            |>,
            <|"Colors" -> {3}|> -> <|
                "Image" -> Daniel`ARC`ARCScene[{{-1, -1, 5}, {-1, -1, 5}, {5, 5, 5}}]
            |>
        }
    |>
    ,
    TestID -> "ARCFindRules-20220903-I4JTV7"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`ARCSimplifyRules[
                Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "d631b094"]["Train"]]
            ]
        ]
    ]
    ,
    <|
        "RemoveEmptySpace" -> True,
        "Rules" -> {
            <||> -> <|
                "Shapes" -> {
                    <|"Name" -> "Line", "Angle" -> 0|>,
                    <|"Name" -> "Rectangle", "Filled" -> True|>
                },
                "X" -> 1,
                "Y" -> 1,
                "X2" -> Daniel`ARC`ObjectValue["InputScene", "FilledArea"],
                "Y2" -> 1
            |>
        }
    |>
    ,
    TestID -> "ARCFindRules-20220903-Q5SIX5"
]

Test[
    Daniel`ARC`ARCSimplifyRules[
        Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "5614dbcf"]["Train"]]
    ]
    ,
    <|
        "Denoise" -> True,
        "SceneAsSingleObject" -> True,
        "Width" -> Inactive[Daniel`ARC`ObjectValue["InputScene", "Width"]*0.3333333333333333],
        "Height" -> Inactive[Daniel`ARC`ObjectValue["InputScene", "Height"]*0.3333333333333333],
        "Rules" -> {
            <||> -> <|"Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.3333333333333333|>|>
        }
    |>
    ,
    TestID -> "ARCFindRules-20220904-JVUY94"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`ARCSimplifyRules[
                Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "694f12f3"]["Train"]]
            ]
        ]
    ]
    ,
    {
        <|"Area.Rank" -> 1|> -> <|
            "Shape" -> <|
                "Name" -> "Rectangle",
                "Filled" -> True,
                "Interior" -> <|"Color" -> 2|>,
                "Border" -> <|"Color" -> 4|>
            |>
        |>,
        <|"Area.Rank" -> 2|> -> <|
            "Shape" -> <|
                "Name" -> "Rectangle",
                "Filled" -> True,
                "Interior" -> <|"Color" -> 1|>,
                "Border" -> <|"Color" -> 4|>
            |>
        |>
    }
    ,
    TestID -> "ARCFindRules-20220904-WFSK8X"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`ARCSimplifyRules[
                Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "50cb2852"]["Train"]]
            ]
        ]
    ]
    ,
    {
        <|"Colors" -> {1}|> -> <|
            "Shape" -> <|
                "Name" -> "Rectangle",
                "Filled" -> True,
                "Interior" -> <|"Color" -> 8|>,
                "Border" -> <|"Color" -> 1|>
            |>
        |>,
        <|"Colors" -> {2}|> -> <|
            "Shape" -> <|
                "Name" -> "Rectangle",
                "Filled" -> True,
                "Interior" -> <|"Color" -> 8|>,
                "Border" -> <|"Color" -> 2|>
            |>
        |>,
        <|"Colors" -> {3}|> -> <|
            "Shape" -> <|
                "Name" -> "Rectangle",
                "Filled" -> True,
                "Interior" -> <|"Color" -> 8|>,
                "Border" -> <|"Color" -> 3|>
            |>
        |>
    }
    ,
    TestID -> "ARCFindRules-20220905-34UMX6"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`ARCSimplifyRules[
                Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "b230c067"]["Train"]]
            ]
        ]
    ]
    ,
    {<|"Area.Rank" -> 1|> -> <|"Color" -> 1|>, <|"Area.Rank" -> 2|> -> <|"Color" -> 2|>}
    ,
    TestID -> "ARCFindRules-20220905-Q65LO6"
]

Test[
    Daniel`ARC`ARCSimplifyRules[
        Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "bb43febb"]["Train"]]
    ]
    ,
    {
        <||> -> <|
            "Shape" -> <|
                "Name" -> "Rectangle",
                "Filled" -> True,
                "Interior" -> <|"Color" -> 2|>,
                "Border" -> <|"Color" -> 5|>
            |>
        |>
    }
    ,
    TestID -> "ARCFindRules-20220905-G4PUG3"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`ARCSimplifyRules[
                Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "bdad9b1f"]["Train"]]
            ]
        ]
    ]
    ,
    <|
        "FormMultiColorCompositeObjects" -> False,
        "Rules" -> {
            <|"Colors" -> {2}|> -> <|
                "Shape" -> <|"Name" -> "Line", "Angle" -> 0|>,
                "X" -> 1,
                "X2Inverse" -> 1,
                "ZOrder" -> 1
            |>,
            <|"Colors" -> {8}|> -> <|
                "Shape" -> <|"Name" -> "Line", "Angle" -> 90|>,
                "Y2Inverse" -> 1,
                "ZOrder" -> 1
            |>,
            <|
                "Transform" -> <|
                    "Type" -> "AddObjects",
                    "Objects" -> {
                        <|
                            "Image" -> Daniel`ARC`ARCScene[{{4}}],
                            "Y" -> Daniel`ARC`ObjectValue[<|"Colors" -> {2}|>, "Y"],
                            "X" -> Daniel`ARC`ObjectValue[<|"Colors" -> {8}|>, "X"]
                        |>
                    }
                |>
            |>
        }
    |>
    ,
    TestID -> "ARCFindRules-20220905-LLXOPN"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`ARCSimplifyRules[
                Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "middle"]["Train"]]
            ]
        ]
    ]
    ,
    <|
        "FormMultiColorCompositeObjects" -> False,
        "Rules" -> {
            <||> -> <|"Same" -> True|>,
            <|
                "Transform" -> <|
                    "Type" -> "AddObjects",
                    "Objects" -> {
                        <|
                            "Image" -> Daniel`ARC`ARCScene[{{2}}],
                            "Y" -> Daniel`ARC`ObjectValue[<|"Colors" -> {5}|>, "YMiddle"],
                            "X" -> Daniel`ARC`ObjectValue[<|"Colors" -> {5}|>, "XMiddle"]
                        |>
                    }
                |>
            |>
        }
    |>
    ,
    TestID -> "ARCFindRules-20220906-Z44D4P"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`ARCSimplifyRules[
                Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "794b24be"]["Train"]]
            ]
        ]
    ]
    ,
    <|
        "SceneAsSingleObject" -> True,
        "Rules" -> {
            <|"FilledArea" -> 1|> -> <|
                "Image" -> Daniel`ARC`ARCScene[{{2, -1, -1}, {-1, -1, -1}, {-1, -1, -1}}]
            |>,
            <|"FilledArea" -> 2|> -> <|
                "Image" -> Daniel`ARC`ARCScene[{{2, 2, -1}, {-1, -1, -1}, {-1, -1, -1}}]
            |>,
            <|"FilledArea" -> 3|> -> <|
                "Image" -> Daniel`ARC`ARCScene[{{2, 2, 2}, {-1, -1, -1}, {-1, -1, -1}}]
            |>,
            <|"FilledArea" -> 4|> -> <|
                "Image" -> Daniel`ARC`ARCScene[{{2, 2, 2}, {-1, 2, -1}, {-1, -1, -1}}]
            |>
        }
    |>
    ,
    TestID -> "ARCFindRules-20220906-UA8P8Q"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`ARCSimplifyRules[
                Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "85c4e7cd"]["Train"]]
            ]
        ]
    ]
    ,
    <|
        "FormMultiColorCompositeObjects" -> False,
        "Rules" -> {
            <|"Area.InverseRank" -> 1|> -> <|
                "Color" -> Daniel`ARC`ObjectValue[<|"Position" -> {1, 1}|>, "Color"]
            |>,
            <|"Area.InverseRank" -> 2|> -> <|
                "Color" -> Daniel`ARC`ObjectValue[<|"Position" -> {2, 2}|>, "Color"]
            |>,
            <|"Area.InverseRank" -> 3|> -> <|
                "Color" -> Daniel`ARC`ObjectValue[<|"Position" -> {3, 3}|>, "Color"]
            |>,
            <|"Area.InverseRank" -> 5|> -> <|
                "Color" -> Inactive[Plus][Daniel`ARC`ObjectValue["InputScene", "Width"], -7]
            |>,
            <|"Area.Rank" -> 1|> -> <|
                "Color" -> Daniel`ARC`ObjectValue[
                    <|"MonochromeImage" -> Daniel`ARC`ARCScene[{{10, 10}, {10, 10}}]|>,
                    "Color"
                ]
            |>,
            <|"Area.Rank" -> 2|> -> <|
                "Color" -> Daniel`ARC`ObjectValue[
                    <|
                        "MonochromeImage" -> Daniel`ARC`ARCScene[
                            {
                                {10, 10, 10, 10},
                                {10, -1, -1, 10},
                                {10, -1, -1, 10},
                                {10, 10, 10, 10}
                            }
                        ]
                    |>,
                    "Color"
                ]
            |>,
            <|"Area.Rank" -> 3|> -> <|
                "Color" -> Daniel`ARC`ObjectValue[
                    <|
                        "MonochromeImage" -> Daniel`ARC`ARCScene[
                            {
                                {10, 10, 10, 10, 10, 10},
                                {10, -1, -1, -1, -1, 10},
                                {10, -1, -1, -1, -1, 10},
                                {10, -1, -1, -1, -1, 10},
                                {10, -1, -1, -1, -1, 10},
                                {10, 10, 10, 10, 10, 10}
                            }
                        ]
                    |>,
                    "Color"
                ]
            |>
        }
    |>
    ,
    TestID -> "ARCFindRules-20220906-89DI3K"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`ARCSimplifyRules[
                Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "ba97ae07"]["Train"]]
            ]
        ]
    ]
    ,
    <|
        "FormMultiColorCompositeObjects" -> False,
        "Rules" -> {
            <|"ZOrder" -> 0|> -> <|"ZOrder" -> 1|>,
            <|"ZOrder" -> 1|> -> <|"ZOrder" -> 0|>
        }
    |>
    ,
    TestID -> "ARCFindRules-20220906-5XC305"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`ARCSimplifyRules[
                Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "ff28f65a"]["Train"]]
            ]
        ]
    ]
    ,
    <|
        "SceneAsSingleObject" -> True,
        "Width" -> 3,
        "Height" -> 3,
        "Rules" -> {
            <|"FilledArea" -> 4|> -> <|
                "Image" -> Daniel`ARC`ARCScene[{{1, -1, -1}, {-1, -1, -1}, {-1, -1, -1}}]
            |>,
            <|"FilledArea" -> 8|> -> <|
                "Image" -> Daniel`ARC`ARCScene[{{1, -1, 1}, {-1, -1, -1}, {-1, -1, -1}}]
            |>,
            <|"FilledArea" -> 12|> -> <|
                "Image" -> Daniel`ARC`ARCScene[{{1, -1, 1}, {-1, 1, -1}, {-1, -1, -1}}]
            |>,
            <|"FilledArea" -> 16|> -> <|
                "Image" -> Daniel`ARC`ARCScene[{{1, -1, 1}, {-1, 1, -1}, {1, -1, -1}}]
            |>,
            <|"FilledArea" -> 20|> -> <|
                "Image" -> Daniel`ARC`ARCScene[{{1, -1, 1}, {-1, 1, -1}, {1, -1, 1}}]
            |>
        }
    |>
    ,
    TestID -> "ARCFindRules-20220906-I8KEGT"
]

Test[
    Daniel`ARC`ARCSimplifyRules[
        Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "8be77c9e"]["Train"]]
    ]
    ,
    <|
        "SceneAsSingleObject" -> True,
        "Height" -> Inactive[Daniel`ARC`ObjectValue["InputScene", "Height"]*2],
        "Rules" -> {
            {<|"SceneAsSingleObject" -> True, "Rules" -> {<||> -> <|"Same" -> True|>}|>},
            {
                <|
                    "SceneAsSingleObject" -> True,
                    "Rules" -> {
                        <||> -> <|
                            "Transform" -> <|"Type" -> "Flip", "Direction" -> "Vertical"|>
                        |>
                    }
                |>
            }
        }
    |>
    ,
    TestID -> "ARCFindRules-20220907-4K1SKO"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`ARCSimplifyRules[
                Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "67a3c6ac"]["Train"]]
            ]
        ]
    ]
    ,
    {<||> -> <|"Transform" -> <|"Type" -> "Flip", "Direction" -> "Horizontal"|>|>}
    ,
    TestID -> "ARCFindRules-20220907-9K7CJL"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`ARCSimplifyRules[
                Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "68b16354"]["Train"]]
            ]
        ]
    ]
    ,
    {<||> -> <|"Transform" -> <|"Type" -> "Flip", "Direction" -> "Vertical"|>|>}
    ,
    TestID -> "ARCFindRules-20220907-J2QT3W"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`ARCSimplifyRules[
                Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "a8c38be5"]["Train"]]
            ]
        ]
    ]
    ,
    <|
        "FormMultiColorCompositeObjects" -> False,
        "RemoveEmptySpace" -> True,
        "Background" -> 5,
        "Rules" -> {
            <|"Shape" -> <|"Name" -> "L"|>|> -> <|
                "Transform" -> <|"Type" -> "Move", "Position" -> <|"Y" -> 8, "X" -> 1|>|>
            |>,
            <|
                "Shape" -> <|
                    "Name" -> "L",
                    "Transform" -> <|"Type" -> "Rotation", "Angle" -> 90|>
                |>
            |> -> <|
                "Transform" -> <|"Type" -> "Move", "Position" -> <|"Y" -> 1, "X" -> 1|>|>
            |>,
            <|
                "Shape" -> <|
                    "Name" -> "L",
                    "Transform" -> <|"Type" -> "Rotation", "Angle" -> 180|>
                |>
            |> -> <|
                "Transform" -> <|"Type" -> "Move", "Position" -> <|"Y" -> 1, "X" -> 8|>|>
            |>,
            <|
                "Shape" -> <|
                    "Name" -> "L",
                    "Transform" -> <|"Type" -> "Rotation", "Angle" -> 270|>
                |>
            |> -> <|
                "Transform" -> <|"Type" -> "Move", "Position" -> <|"Y" -> 8, "X" -> 8|>|>
            |>,
            <|"Shape" -> <|"Name" -> "Triangle"|>|> -> <|
                "Transform" -> <|"Type" -> "Move", "Position" -> <|"Y" -> 8, "X" -> 4|>|>
            |>,
            <|
                "Shape" -> <|
                    "Name" -> "Triangle",
                    "Transform" -> <|"Type" -> "Rotation", "Angle" -> 90|>
                |>
            |> -> <|
                "Transform" -> <|"Type" -> "Move", "Position" -> <|"Y" -> 4, "X" -> 1|>|>
            |>,
            <|
                "Shape" -> <|
                    "Name" -> "Triangle",
                    "Transform" -> <|"Type" -> "Rotation", "Angle" -> 180|>
                |>
            |> -> <|
                "Transform" -> <|"Type" -> "Move", "Position" -> <|"Y" -> 1, "X" -> 4|>|>
            |>,
            <|
                "Shape" -> <|
                    "Name" -> "Triangle",
                    "Transform" -> <|"Type" -> "Rotation", "Angle" -> 270|>
                |>
            |> -> <|
                "Transform" -> <|"Type" -> "Move", "Position" -> <|"Y" -> 4, "X" -> 8|>|>
            |>,
            <|
                "Shape" -> Alternatives[
                    <|"Name" -> "Square", "Filled" -> True|>,
                    Daniel`ARC`ARCScene[{{10, -1, 10}, {10, 10, 10}}],
                    Daniel`ARC`ARCScene[{{10, 10, 10}, {10, -1, 10}}],
                    Daniel`ARC`ARCScene[{{10, 10}, {-1, 10}, {10, 10}}],
                    Daniel`ARC`ARCScene[{{10, 10}, {10, -1}, {10, 10}}],
                    Daniel`ARC`ARCScene[{{-1, -1, 10}, {-1, 10, 10}, {10, 10, 10}}],
                    Daniel`ARC`ARCScene[{{10, -1, -1}, {10, 10, -1}, {10, 10, 10}}],
                    Daniel`ARC`ARCScene[{{10, 10, 10}, {-1, 10, 10}, {-1, -1, 10}}],
                    Daniel`ARC`ARCScene[{{10, 10, 10}, {10, 10, -1}, {10, -1, -1}}]
                ]
            |> -> <|
                "Transform" -> <|"Type" -> "Delete"|>
            |>
        }
    |>
    ,
    TestID -> "ARCFindRules-20220907-7UAUD5"
]

Test[
    Daniel`ARC`ARCSimplifyRules[
        Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "62c24649"]["Train"]]
    ]
    ,
    <|
        "SceneAsSingleObject" -> True,
        "Width" -> Inactive[Daniel`ARC`ObjectValue["InputScene", "Width"]*2],
        "Height" -> Inactive[Daniel`ARC`ObjectValue["InputScene", "Height"]*2],
        "Rules" -> {
            {
                <|"SceneAsSingleObject" -> True, "Rules" -> {<||> -> <|"Same" -> True|>}|>,
                <|
                    "SceneAsSingleObject" -> True,
                    "Rules" -> {
                        <||> -> <|
                            "Transform" -> <|"Type" -> "Flip", "Direction" -> "Horizontal"|>
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
                        <||> -> <|"Transform" -> <|"Type" -> "Rotation", "Angle" -> 180|>|>
                    }
                |>
            }
        }
    |>
    ,
    TestID -> "ARCFindRules-20220907-E1JST8"
]

Test[
    Daniel`ARC`ARCSimplifyRules[
        Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "67e8384a"]["Train"]]
    ]
    ,
    <|
        "SceneAsSingleObject" -> True,
        "Width" -> Inactive[Daniel`ARC`ObjectValue["InputScene", "Width"]*2],
        "Height" -> Inactive[Daniel`ARC`ObjectValue["InputScene", "Height"]*2],
        "Rules" -> {
            {
                <|"SceneAsSingleObject" -> True, "Rules" -> {<||> -> <|"Same" -> True|>}|>,
                <|
                    "SceneAsSingleObject" -> True,
                    "Rules" -> {
                        <||> -> <|
                            "Transform" -> <|"Type" -> "Flip", "Direction" -> "Horizontal"|>
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
                        <||> -> <|"Transform" -> <|"Type" -> "Rotation", "Angle" -> 180|>|>
                    }
                |>
            }
        }
    |>
    ,
    TestID -> "ARCFindRules-20220907-7NJSQX"
]

Test[
    Daniel`ARC`ARCSimplifyRules[
        Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "6d0aefbc"]["Train"]]
    ]
    ,
    <|
        "SceneAsSingleObject" -> True,
        "Width" -> Inactive[Daniel`ARC`ObjectValue["InputScene", "Width"]*2],
        "Rules" -> {
            {
                <|"SceneAsSingleObject" -> True, "Rules" -> {<||> -> <|"Same" -> True|>}|>,
                <|
                    "SceneAsSingleObject" -> True,
                    "Rules" -> {
                        <||> -> <|
                            "Transform" -> <|"Type" -> "Flip", "Direction" -> "Horizontal"|>
                        |>
                    }
                |>
            }
        }
    |>
    ,
    TestID -> "ARCFindRules-20220907-0OWD6Y"
]

Test[
    Daniel`ARC`ARCSimplifyRules[
        Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "46442a0e"]["Train"]]
    ]
    ,
    <|
        "SceneAsSingleObject" -> True,
        "Width" -> Inactive[Daniel`ARC`ObjectValue["InputScene", "Width"]*2],
        "Height" -> Inactive[Daniel`ARC`ObjectValue["InputScene", "Height"]*2],
        "Rules" -> {
            {
                <|"SceneAsSingleObject" -> True, "Rules" -> {<||> -> <|"Same" -> True|>}|>,
                <|
                    "SceneAsSingleObject" -> True,
                    "Rules" -> {
                        <||> -> <|"Transform" -> <|"Type" -> "Rotation", "Angle" -> 90|>|>
                    }
                |>
            },
            {
                <|
                    "SceneAsSingleObject" -> True,
                    "Rules" -> {
                        <||> -> <|"Transform" -> <|"Type" -> "Rotation", "Angle" -> 270|>|>
                    }
                |>,
                <|
                    "SceneAsSingleObject" -> True,
                    "Rules" -> {
                        <||> -> <|"Transform" -> <|"Type" -> "Rotation", "Angle" -> 180|>|>
                    }
                |>
            }
        }
    |>
    ,
    TestID -> "ARCFindRules-20220908-R8EBWK"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`ARCSimplifyRules[
                Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "2dee498d"]["Train"]]
            ]
        ]
    ]
    ,
    <|
        "SubdivideInput" -> {1, 3},
        "RemoveEmptySpace" -> True,
        "Rules" -> {
            <|"Position" -> Except[{1, 1}]|> -> <|"Transform" -> <|"Type" -> "Delete"|>|>,
            <|"Position" -> {1, 1}|> -> <||>
        }
    |>
    ,
    TestID -> "ARCFindRules-20220909-EK29OK"
]

Test[
    Daniel`ARC`ARCSimplifyRules[
        Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "3af2c5a8"]["Train"]]
    ]
    ,
    <|
        "SceneAsSingleObject" -> True,
        "Width" -> Inactive[Daniel`ARC`ObjectValue["InputScene", "Width"]*2],
        "Height" -> Inactive[Daniel`ARC`ObjectValue["InputScene", "Height"]*2],
        "Rules" -> {
            {
                <|"SceneAsSingleObject" -> True, "Rules" -> {<||> -> <|"Same" -> True|>}|>,
                <|
                    "SceneAsSingleObject" -> True,
                    "Rules" -> {
                        <||> -> <|
                            "Transform" -> <|"Type" -> "Flip", "Direction" -> "Horizontal"|>
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
                        <||> -> <|"Transform" -> <|"Type" -> "Rotation", "Angle" -> 180|>|>
                    }
                |>
            }
        }
    |>
    ,
    TestID -> "ARCFindRules-20220909-SKD0NH"
]

Test[
    Daniel`ARC`ARCSimplifyRules[
        Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "4c4377d9"]["Train"]]
    ]
    ,
    <|
        "SceneAsSingleObject" -> True,
        "Height" -> Inactive[Daniel`ARC`ObjectValue["InputScene", "Height"]*2],
        "Rules" -> {
            {
                <|
                    "SceneAsSingleObject" -> True,
                    "Rules" -> {
                        <||> -> <|
                            "Transform" -> <|"Type" -> "Flip", "Direction" -> "Vertical"|>
                        |>
                    }
                |>
            },
            {<|"SceneAsSingleObject" -> True, "Rules" -> {<||> -> <|"Same" -> True|>}|>}
        }
    |>
    ,
    TestID -> "ARCFindRules-20220909-4IKKMN"
]

Test[
    Daniel`ARC`ARCSimplifyRules[
        Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "8d5021e8"]["Train"]]
    ]
    ,
    <|
        "SceneAsSingleObject" -> True,
        "Width" -> Inactive[Daniel`ARC`ObjectValue["InputScene", "Width"]*2],
        "Height" -> Inactive[Daniel`ARC`ObjectValue["InputScene", "Height"]*3],
        "Rules" -> {
            {
                <|
                    "SceneAsSingleObject" -> True,
                    "Rules" -> {
                        <||> -> <|"Transform" -> <|"Type" -> "Rotation", "Angle" -> 180|>|>
                    }
                |>,
                <|
                    "SceneAsSingleObject" -> True,
                    "Rules" -> {
                        <||> -> <|
                            "Transform" -> <|"Type" -> "Flip", "Direction" -> "Vertical"|>
                        |>
                    }
                |>
            },
            {
                <|
                    "SceneAsSingleObject" -> True,
                    "Rules" -> {
                        <||> -> <|
                            "Transform" -> <|"Type" -> "Flip", "Direction" -> "Horizontal"|>
                        |>
                    }
                |>,
                <|"SceneAsSingleObject" -> True, "Rules" -> {<||> -> <|"Same" -> True|>}|>
            },
            {
                <|
                    "SceneAsSingleObject" -> True,
                    "Rules" -> {
                        <||> -> <|"Transform" -> <|"Type" -> "Rotation", "Angle" -> 180|>|>
                    }
                |>,
                <|
                    "SceneAsSingleObject" -> True,
                    "Rules" -> {
                        <||> -> <|
                            "Transform" -> <|"Type" -> "Flip", "Direction" -> "Vertical"|>
                        |>
                    }
                |>
            }
        }
    |>
    ,
    TestID -> "ARCFindRules-20220909-EY6WY5"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`ARCSimplifyRules[
                Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "a416b8f3"]["Train"]]
            ]
        ]
    ]
    ,
    <|
        "Width" -> Inactive[Times][Daniel`ARC`ObjectValue["InputScene", "Width"], 2],
        "Height" -> Daniel`ARC`ObjectValue["InputScene", "Height"],
        "Rules" -> <|
            "Type" -> "PatternFill",
            "StartY" -> 1,
            "StartX" -> 1,
            "TrajectoryY" -> 0,
            "TrajectoryX" -> Daniel`ARC`ObjectValue["InputScene", "Width"]
        |>
    |>
    ,
    TestID -> "ARCFindRules-20220909-RONLH4"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`ARCSimplifyRules[
                Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "d0f5fe59"]["Train"]]
            ]
        ]
    ]
    ,
    <|
        "RemoveEmptySpace" -> True,
        "Rules" -> {
            <||> -> <|"Transform" -> <|"Type" -> "Delete"|>|>,
            <|
                "Transform" -> <|
                    "Type" -> "AddObjects",
                    "Objects" -> {
                        <|
                            "Shape" -> <|"Name" -> "Line", "Angle" -> 135|>,
                            "Color" -> 8,
                            "X" -> 1,
                            "Y" -> 1,
                            "X2" -> Daniel`ARC`ObjectValue["InputScene", "ObjectCount"],
                            "Y2" -> Daniel`ARC`ObjectValue["InputScene", "ObjectCount"],
                            "ZOrder" -> 0
                        |>
                    }
                |>
            |>
        }
    |>
    ,
    TestID -> "ARCFindRules-20220909-GY54SK"
]

Test[
    Daniel`ARC`ARCSimplifyRules[
        Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "beb8660c"]["Train"]]
    ]
    ,
    <|
        "FormMultiColorCompositeObjects" -> False,
        "Rules" -> {
            <||> -> <|
                "X" -> Daniel`ARC`ObjectValue["InputObject", "Width.Rank"],
                "YInverse" -> Daniel`ARC`ObjectValue["InputObject", "Width.Rank"]
            |>
        }
    |>
    ,
    TestID -> "ARCFindRules-20220910-Q4IHSD"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`ARCSimplifyRules[
                Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "272f95fa"]["Train"]]
            ]
        ]
    ]
    ,
    <|
        "Subdivision" -> "Grid",
        "Rules" -> {
            {
                <|"SceneAsSingleObject" -> True, "Rules" -> {<||> -> <|"Same" -> True|>}|>,
                <|
                    "Rules" -> {
                        <|
                            "Transform" -> <|
                                "Type" -> "AddObjects",
                                "Objects" -> {
                                    <|
                                        "Shape" -> <|"Name" -> "Rectangle", "Filled" -> True|>,
                                        "Color" -> 2,
                                        "X" -> 1,
                                        "Y" -> 1,
                                        "X2Inverse" -> 1,
                                        "Y2Inverse" -> 1,
                                        "ZOrder" -> 0
                                    |>
                                }
                            |>
                        |>
                    }
                |>,
                <|"SceneAsSingleObject" -> True, "Rules" -> {<||> -> <|"Same" -> True|>}|>
            },
            {
                <|
                    "Rules" -> {
                        <|
                            "Transform" -> <|
                                "Type" -> "AddObjects",
                                "Objects" -> {
                                    <|
                                        "Shape" -> <|"Name" -> "Square", "Filled" -> True|>,
                                        "Color" -> 4,
                                        "X" -> 1,
                                        "Y" -> 1,
                                        "X2Inverse" -> 1,
                                        "Y2Inverse" -> 1,
                                        "ZOrder" -> 0
                                    |>
                                }
                            |>
                        |>
                    }
                |>,
                <|
                    "Rules" -> {
                        <|
                            "Transform" -> <|
                                "Type" -> "AddObjects",
                                "Objects" -> {
                                    <|
                                        "Shape" -> <|"Name" -> "Rectangle", "Filled" -> True|>,
                                        "Color" -> 6,
                                        "X" -> 1,
                                        "Y" -> 1,
                                        "X2Inverse" -> 1,
                                        "Y2Inverse" -> 1,
                                        "ZOrder" -> 0
                                    |>
                                }
                            |>
                        |>
                    }
                |>,
                <|
                    "Rules" -> {
                        <|
                            "Transform" -> <|
                                "Type" -> "AddObjects",
                                "Objects" -> {
                                    <|
                                        "Shape" -> <|"Name" -> "Rectangle", "Filled" -> True|>,
                                        "Color" -> 3,
                                        "X" -> 1,
                                        "Y" -> 1,
                                        "X2Inverse" -> 1,
                                        "Y2Inverse" -> 1,
                                        "ZOrder" -> 0
                                    |>
                                }
                            |>
                        |>
                    }
                |>
            },
            {
                <|"SceneAsSingleObject" -> True, "Rules" -> {<||> -> <|"Same" -> True|>}|>,
                <|
                    "Rules" -> {
                        <|
                            "Transform" -> <|
                                "Type" -> "AddObjects",
                                "Objects" -> {
                                    <|
                                        "Shape" -> <|"Name" -> "Rectangle", "Filled" -> True|>,
                                        "Color" -> 1,
                                        "X" -> 1,
                                        "Y" -> 1,
                                        "X2Inverse" -> 1,
                                        "Y2Inverse" -> 1,
                                        "ZOrder" -> 0
                                    |>
                                }
                            |>
                        |>
                    }
                |>,
                <|"SceneAsSingleObject" -> True, "Rules" -> {<||> -> <|"Same" -> True|>}|>
            }
        }
    |>
    ,
    TestID -> "ARCFindRules-20220910-PQFY4J"
]

Test[
    Daniel`ARC`ARCSimplifyRules[
        Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "6773b310"]["Train"]]
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
    TestID -> "ARCFindRules-20220911-0EVEKZ"
]

Test[
    Daniel`ARC`ARCSimplifyRules[
        Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "8e5a5113"]["Train"]]
    ]
    ,
    {
        <||> -> <|"Same" -> True|>,
        <|
            "Transform" -> <|
                "Type" -> "AddObjects",
                "Objects" -> {
                    <|
                        "Image" -> Inactive[Daniel`ARC`Transform][
                            Daniel`ARC`ObjectValue[<|"Colors" -> Except[{5}]|>, "Image"],
                            <|"Type" -> "Rotation", "Angle" -> 90|>
                        ],
                        "Y" -> 1,
                        "X" -> 5
                    |>,
                    <|
                        "Image" -> Inactive[Daniel`ARC`Transform][
                            Daniel`ARC`ObjectValue[<|"Colors" -> Except[{5}]|>, "Image"],
                            <|"Type" -> "Rotation", "Angle" -> 180|>
                        ],
                        "Y" -> 1,
                        "X" -> 9
                    |>
                }
            |>
        |>
    }
    ,
    TestID -> "ARCFindRules-20220912-V40529"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`ARCSimplifyRules[
                Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "b91ae062"]["Train"]]
            ]
        ]
    ]
    ,
    <|
        "SceneAsSingleObject" -> True,
        "Rules" -> {
            <||> -> <|
                "Transform" -> <|
                    "Type" -> "Scaled",
                    "Factor" -> Daniel`ARC`ObjectValue["InputScene", "ColorCount"]
                |>
            |>
        }
    |>
    ,
    TestID -> "ARCFindRules-20220912-06HYGG"
]

Test[
    Daniel`ARC`ARCSimplifyRules[
        Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "74dd1130"]["Train"]]
    ]
    ,
    <|
        "SceneAsSingleObject" -> True,
        "Rules" -> {
            <||> -> <|
                "Transform" -> {
                    <|"Type" -> "Rotation", "Angle" -> 270|>,
                    <|"Type" -> "Flip", "Direction" -> "Vertical"|>
                }
            |>
        }
    |>
    ,
    TestID -> "ARCFindRules-20220913-WE3712"
]

Test[
    Daniel`ARC`ARCSimplifyRules[
        Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "7468f01a"]["Train"]]
    ]
    ,
    <|
        "RemoveEmptySpace" -> True,
        "Rules" -> {
            <||> -> <|
                "Image" -> Inactive[Daniel`ARC`Transform][
                    Daniel`ARC`ObjectValue["InputObject", "Image"],
                    <|"Type" -> "Flip", "Direction" -> "Horizontal"|>
                ],
                "Position" -> {1, 1}
            |>
        }
    |>
    ,
    TestID -> "ARCFindRules-20220913-BGBOXR"
]

Test[
    Daniel`ARC`ARCSimplifyRules[
        Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "5117e062"]["Train"]]
    ]
    ,
    <|
        "RemoveEmptySpace" -> True,
        "Rules" -> {
            <||> -> <|"Transform" -> <|"Type" -> "Delete"|>|>,
            <|
                "Transform" -> <|
                    "Type" -> "AddObjects",
                    "Objects" -> {
                        <|
                            "Shape" -> Daniel`ARC`ObjectValue[
                                <|"ColorCount" -> 2|>,
                                "MonochromeImage"
                            ],
                            "Color" -> Daniel`ARC`ObjectValue[
                                <|"ColorCount" -> 2|>,
                                "MostUsedColor"
                            ],
                            "X" -> 1,
                            "Y" -> 1,
                            "X2" -> 3,
                            "Y2" -> 3,
                            "ZOrder" -> 0
                        |>
                    }
                |>
            |>
        }
    |>
    ,
    TestID -> "ARCFindRules-20220913-KU8N1T"
]

Test[
    Daniel`ARC`ARCSimplifyRules[
        Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "ac0a08a4"]["Train"]]
    ]
    ,
    <|
        "SceneAsSingleObject" -> True,
        "Rules" -> {
            <||> -> <|
                "Transform" -> <|
                    "Type" -> "Scaled",
                    "Factor" -> Daniel`ARC`ObjectValue["InputScene", "ColorCount"]
                |>
            |>
        }
    |>
    ,
    TestID -> "ARCFindRules-20220917-QQ6EKK"
]

Test[
    Daniel`ARC`ARCSimplifyRules[
        Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "67385a82"]["Train"]]
    ]
    ,
    <|
        "FollowDiagonals" -> False,
        "Rules" -> {
            <|"Shape" -> <|"Name" -> "Pixel"|>|> -> <|"Same" -> True|>,
            <|"Shape" -> Except[<|"Name" -> "Pixel"|>]|> -> <|"Color" -> 8|>
        }
    |>
    ,
    TestID -> "ARCFindRules-20220917-JP5P6M"
]

Test[
    Daniel`ARC`ARCSimplifyRules[
        Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "28bf18c6"]["Train"]]
    ]
    ,
    <|
        "RemoveEmptySpace" -> True,
        "Rules" -> {
            <||> -> <|"Transform" -> <|"Type" -> "Delete"|>|>,
            <|
                "Transform" -> <|
                    "Type" -> "AddObjects",
                    "Objects" -> {
                        <|
                            "Image" -> Daniel`ARC`ObjectValue[<|"Width" -> 3|>, "Image"],
                            "Y" -> 1,
                            "X" -> 1
                        |>,
                        <|
                            "Image" -> Daniel`ARC`ObjectValue[<|"Width" -> 3|>, "Image"],
                            "Y" -> 1,
                            "X" -> 4
                        |>
                    }
                |>
            |>
        }
    |>
    ,
    TestID -> "ARCFindRules-20220917-JF7L0Z"
]

Test[
    Daniel`ARC`ARCSimplifyRules[
        Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "496994bd"]["Train"]]
    ]
    ,
    <|
        "CheckForGridsAndDividers" -> False,
        "Rules" -> {
            <||> -> <|"Same" -> True|>,
            <|
                "Transform" -> <|
                    "Type" -> "AddObjects",
                    "Objects" -> {
                        <|
                            "Image" -> Inactive[Daniel`ARC`Transform][
                                Daniel`ARC`ObjectValue[<|"Position" -> {1, 1}|>, "Image"],
                                <|"Type" -> "Flip", "Direction" -> "Vertical"|>
                            ],
                            "Y" -> Daniel`ARC`ObjectValue[<|"Position" -> {1, 1}|>, "Y2Inverse"],
                            "X" -> 1
                        |>
                    }
                |>
            |>
        }
    |>
    ,
    TestID -> "ARCFindRules-20220917-CYFM4Y"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`ARCSimplifyRules[
                Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "d2abd087"]["Train"]]
            ]
        ]
    ]
    ,
    {
        <|"FilledArea" -> 6|> -> <|"Color" -> 2|>,
        <|"FilledArea" -> Except[6]|> -> <|"Color" -> 1|>
    }
    ,
    TestID -> "ARCFindRules-20220918-YCSX3K"
]

Test[
    Daniel`ARC`ARCSimplifyRules[
        Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "e5790162"]["Train"]]
    ]
    ,
    <|
        "FormMultiColorCompositeObjects" -> False,
        "Rules" -> {
            <|"Colors" -> {6} | {8}|> -> <|"Same" -> True|>,
            <|"Colors" -> {3}|> -> <|
                "Transform" -> <|
                    "Type" -> "GenerateObject",
                    "StartPosition" -> <|"RelativePosition" -> <|"Y" -> 0, "X" -> 0|>|>,
                    "Color" -> 3,
                    "Rules" -> {
                        <|"ColorAhead" -> Missing[]|> -> <|"OutgoingDirection" -> {0, 1}|>,
                        <|"ColorAhead" -> -1|> -> <|"TurnDegrees" -> 0|>,
                        <|"ColorAhead" -> 6|> -> <|"TurnDegrees" -> 90|>,
                        <|"ColorAhead" -> -2|> -> <|"Stop" -> True|>,
                        <|"ColorAhead" -> 8|> -> <|"TurnDegrees" -> -90|>
                    }
                |>
            |>
        }
    |>
    ,
    TestID -> "ARCFindRules-20220922-NYXCK1"
]

Test[
    Daniel`ARC`ARCSimplifyRules[
        Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "5582e5ca"]["Train"]]
    ]
    ,
    {
        <||> -> <|
            "Shape" -> <|"Name" -> "Square", "Filled" -> True|>,
            "Color" -> Daniel`ARC`ObjectValue["InputScene", "MostUsedColor"]
        |>
    }
    ,
    TestID -> "ARCFindRules-20220924-AGG2N7"
]

Test[
    Daniel`ARC`ARCSimplifyRules[
        Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "0bb8dee"]["Train"]]
    ]
    ,
    <|
        "RemoveEmptySpace" -> True,
        "Rules" -> {
            <|"GridPosition" -> {1, 1}|> -> <|
                "Transform" -> <|"Type" -> "Move", "Position" -> <|"Y" -> 1, "X" -> 1|>|>
            |>,
            <|"GridPosition" -> {1, 2}|> -> <|
                "Transform" -> <|"Type" -> "Move", "Position" -> <|"Y" -> 1, "X" -> 4|>|>
            |>,
            <|"GridPosition" -> {2, 1}|> -> <|
                "Transform" -> <|"Type" -> "Move", "Position" -> <|"Y" -> 4, "X" -> 1|>|>
            |>,
            <|"GridPosition" -> {2, 2}|> -> <|
                "Transform" -> <|"Type" -> "Move", "Position" -> <|"Y" -> 4, "X" -> 4|>|>
            |>,
            <|"GridPosition" -> Missing[]|> -> <|"Transform" -> <|"Type" -> "Delete"|>|>
        }
    |>
    ,
    TestID -> "ARCFindRules-20220924-ERTT9B"
]

Test[
    Daniel`ARC`ARCSimplifyRules[
        Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "27a77e38"]["Train"]]
    ]
    ,
    {
        <||> -> <|"Same" -> True|>,
        <|
            "Transform" -> <|
                "Type" -> "AddObjects",
                "Objects" -> {
                    <|
                        "Shape" -> <|"Name" -> "Pixel"|>,
                        "Color" -> Daniel`ARC`ObjectValue["InputScene", "SecondMostUsedColor"],
                        "X" -> Daniel`ARC`ObjectValue["InputScene", "YMiddle"],
                        "YInverse" -> 1,
                        "Width" -> 1,
                        "Y2Inverse" -> 1,
                        "ZOrder" -> 0
                    |>
                }
            |>
        |>
    }
    ,
    TestID -> "ARCFindRules-20220924-YHDBXZ"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`ARCSimplifyRules[
                Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "d9f24cd1"]["Train"]]
            ]
        ]
    ]
    ,
    <|
        "FormMultiColorCompositeObjects" -> False,
        "CheckForGridsAndDividers" -> False,
        "Rules" -> {
            <|"Image" -> Daniel`ARC`ARCScene[{{2}}]|> -> <|
                "Transform" -> <|
                    "Type" -> "GenerateObject",
                    "StartPosition" -> <|"RelativePosition" -> <|"Y" -> 0, "X" -> 0|>|>,
                    "Color" -> 2,
                    "Rules" -> {
                        <|"ColorAhead" -> Missing[]|> -> <|"OutgoingDirection" -> {-1, 0}|>,
                        <|"ColorAhead" -> -1|> -> <|"OutgoingDirection" -> {-1, 0}|>,
                        <|"ColorAhead" -> -2|> -> <|"Stop" -> True|>,
                        <|"ColorAhead" -> 5|> -> <|
                            "OutgoingDirection" -> {0, 1},
                            "TurnDegrees" -> 90
                        |>
                    }
                |>
            |>,
            <|"Image" -> Daniel`ARC`ARCScene[{{5}}]|> -> <|"Same" -> True|>
        }
    |>
    ,
    TestID -> "ARCFindRules-20220925-550EAJ"
]

Test[
    Daniel`ARC`ARCSimplifyRules[
        Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "0520fde7"]["Train"]]
    ]
    ,
    <|
        "Subdivision" -> <|"Input" -> "Grid", "Output" -> None|>,
        "Rules" -> <|
            "Type" -> "ValueMap",
            "Binarize" -> True,
            "OutputColor" -> 2,
            "Mapping" -> <|{1, 0} -> 0, {0, 0} -> 0, {0, 1} -> 0, {1, 1} -> 1|>
        |>
    |>
    ,
    TestID -> "ARCFindRules-20220925-KQT0JX"
]

Test[
    Daniel`ARC`ARCSimplifyRules[
        Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "1b2d62fb"]["Train"]]
    ]
    ,
    <|
        "Subdivision" -> <|"Input" -> "Grid", "Output" -> None|>,
        "Rules" -> <|
            "Type" -> "ValueMap",
            "Binarize" -> True,
            "OutputColor" -> 8,
            "Mapping" -> <|{0, 1} -> 0, {1, 1} -> 0, {0, 0} -> 1, {1, 0} -> 0|>
        |>
    |>
    ,
    TestID -> "ARCFindRules-20220926-UK5L9O"
]

Test[
    Daniel`ARC`ARCSimplifyRules[
        Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "99b1bc43"]["Train"]]
    ]
    ,
    <|
        "Subdivision" -> <|"Input" -> "Grid", "Output" -> None|>,
        "Rules" -> <|
            "Type" -> "ValueMap",
            "Binarize" -> True,
            "OutputColor" -> 3,
            "Mapping" -> <|{0, 0} -> 0, {1, 0} -> 1, {1, 1} -> 0, {0, 1} -> 1|>
        |>
    |>
    ,
    TestID -> "ARCFindRules-20220926-HJD005"
]

Test[
    Daniel`ARC`ARCSimplifyRules[
        Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "6430c8c4"]["Train"]]
    ]
    ,
    <|
        "Subdivision" -> <|"Input" -> "Grid", "Output" -> None|>,
        "Rules" -> <|
            "Type" -> "ValueMap",
            "Binarize" -> True,
            "OutputColor" -> 3,
            "Mapping" -> <|{1, 0} -> 0, {0, 1} -> 0, {0, 0} -> 1, {1, 1} -> 0|>
        |>
    |>
    ,
    TestID -> "ARCFindRules-20220926-HLLW9Q"
]

Test[
    Daniel`ARC`ARCSimplifyRules[
        Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "3428a4f5"]["Train"]]
    ]
    ,
    <|
        "Subdivision" -> <|"Input" -> "Grid", "Output" -> None|>,
        "Rules" -> <|
            "Type" -> "ValueMap",
            "Binarize" -> True,
            "OutputColor" -> 3,
            "Mapping" -> <|{0, 1} -> 1, {0, 0} -> 0, {1, 1} -> 0, {1, 0} -> 1|>
        |>
    |>
    ,
    TestID -> "ARCFindRules-20220926-HDLZWN"
]

Test[
    Daniel`ARC`ARCSimplifyRules[
        Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "94f9d214"]["Train"]]
    ]
    ,
    <|
        "Subdivision" -> <|
            "Input" -> <|"Type" -> "ColorGrid", "RowCount" -> 2, "ColumnCount" -> 1|>,
            "Output" -> None
        |>,
        "Rules" -> <|
            "Type" -> "ValueMap",
            "Binarize" -> True,
            "OutputColor" -> 2,
            "Mapping" -> <|{0, 0} -> 1, {1, 0} -> 0, {0, 1} -> 0, {1, 1} -> 0|>
        |>
    |>
    ,
    TestID -> "ARCFindRules-20220926-C3P684"
]

Test[
    Daniel`ARC`ARCSimplifyRules[
        Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "f25ffba3"]["Train"]]
    ]
    ,
    {
        <||> -> <|"Same" -> True|>,
        <|
            "Transform" -> <|
                "Type" -> "AddObjects",
                "Objects" -> {
                    <|
                        "Image" -> Inactive[Daniel`ARC`Transform][
                            Daniel`ARC`ObjectValue["Object", "Image"],
                            <|"Type" -> "Flip", "Direction" -> "Vertical"|>
                        ],
                        "Y" -> 1,
                        "X" -> 1
                    |>
                }
            |>
        |>
    }
    ,
    TestID -> "ARCFindRules-20220928-OSLLMJ"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`ARCSimplifyRules[
                Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "9ecd008a"]["Train"]]
            ]
        ]
    ]
    ,
    <|"Type" -> "Imputation", "Pattern" -> "Symmetry", "Output" -> "ImputedRectangle"|>
    ,
    TestID -> "ARCFindRules-20220928-EMUDDF"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`ARCSimplifyRules[
                Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "FF805C23"]["Train"]]
            ]
        ]
    ]
    ,
    <|"Type" -> "Imputation", "Pattern" -> "Symmetry", "Output" -> "ImputedRectangle"|>
    ,
    TestID -> "ARCFindRules-20220928-7WECHR"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`ARCSimplifyRules[
                Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "DC0A314F"]["Train"]]
            ]
        ]
    ]
    ,
    <|"Type" -> "Imputation", "Pattern" -> "Symmetry", "Output" -> "ImputedRectangle"|>
    ,
    TestID -> "ARCFindRules-20220928-SSJHVU"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`ARCSimplifyRules[
                Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "67B4A34D"]["Train"]]
            ]
        ]
    ]
    ,
    <|"Type" -> "Imputation", "Pattern" -> "Symmetry", "Output" -> "ImputedRectangle"|>
    ,
    TestID -> "ARCFindRules-20220928-RSY06C"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`ARCSimplifyRules[
                Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "29ec7d0e"]["Train"]]
            ]
        ]
    ]
    ,
    <|"Type" -> "Imputation", "Pattern" -> "GridFill", "Output" -> "RepairedScene"|>
    ,
    TestID -> "ARCFindRules-20220929-V973SP"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`ARCSimplifyRules[
                Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "0dfd9992"]["Train"]]
            ]
        ]
    ]
    ,
    <|"Type" -> "Imputation", "Pattern" -> "GridFill", "Output" -> "RepairedScene"|>
    ,
    TestID -> "ARCFindRules-20220929-ES65R9"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`ARCSimplifyRules[
                Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "484b58aa"]["Train"]]
            ]
        ]
    ]
    ,
    <|"Type" -> "Imputation", "Pattern" -> "GridFill", "Output" -> "RepairedScene"|>
    ,
    TestID -> "ARCFindRules-20220929-35JPOP"
]

Test[
    Daniel`ARC`ARCSimplifyRules[
        Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "bda2d7a6"]["Train"]]
    ]
    ,
    <|
        "FormMultiColorCompositeObjects" -> False,
        "Background" -> None,
        "Rules" -> {
            <|"ColorUseCount.Rank" -> 1|> -> <|
                "Color" -> Daniel`ARC`ObjectValue[<|"Position" -> {3, 3}|>, "Color"]
            |>,
            <|"ColorUseCount.Rank" -> 2|> -> <|
                "Color" -> Daniel`ARC`ObjectValue[<|"Position" -> {1, 1}|>, "Color"]
            |>,
            <|"ColorUseCount.Rank" -> 3|> -> <|
                "Color" -> Daniel`ARC`ObjectValue[<|"Position" -> {2, 2}|>, "Color"]
            |>
        }
    |>
    ,
    TestID -> "ARCFindRules-20221001-2DSTBL"
]

Test[
    Daniel`ARC`ARCSimplifyRules[
        Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "d5d6de2d"]["Train"]]
    ]
    ,
    {
        <|"HollowCount" -> 0|> -> <|"Transform" -> <|"Type" -> "Delete"|>|>,
        <|"HollowCount" -> 1|> -> <|
            "Shapes" -> {<|"Name" -> "Rectangle", "Filled" -> True|>},
            "Color" -> 3,
            "X" -> Inactive[Plus][Daniel`ARC`ObjectValue["InputObject", "X"], 1],
            "Y" -> Inactive[Plus][Daniel`ARC`ObjectValue["InputObject", "Y"], 1],
            "X2" -> Inactive[Plus][Daniel`ARC`ObjectValue["InputObject", "X2"], -1],
            "Y2" -> Inactive[Plus][Daniel`ARC`ObjectValue["InputObject", "Y2"], -1]
        |>
    }
    ,
    TestID -> "ARCFindRules-20221001-EZXGLV"
]

Test[
    Daniel`ARC`ARCSimplifyRules[
        Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "3bd67248"]["Train"]]
    ]
    ,
    {
        <||> -> <|
            "Transform" -> <|
                "Type" -> "AddComponents",
                "Components" -> {
                    <|
                        "Shape" -> <|"Name" -> "Line", "Angle" -> 45|>,
                        "Color" -> 2,
                        "Position" -> <|"RelativePosition" -> <|"Y" -> 0, "X" -> 1|>|>,
                        "Width" -> Inactive[Plus][
                            Daniel`ARC`ObjectValue["InputScene", "Width"],
                            -1
                        ],
                        "Height" -> Inactive[Plus][
                            Daniel`ARC`ObjectValue["InputObject", "Height"],
                            -1
                        ]
                    |>,
                    <|
                        "Shape" -> <|"Name" -> "Line", "Angle" -> 0|>,
                        "Color" -> 4,
                        "Position" -> <|"RelativePosition" -> <|"YInverse" -> 0, "X" -> 1|>|>,
                        "Width" -> Inactive[Plus][
                            Daniel`ARC`ObjectValue["InputScene", "Width"],
                            -1
                        ],
                        "Height" -> 1
                    |>
                }
            |>
        |>
    }
    ,
    TestID -> "ARCFindRules-20221001-27LKXS"
]

Test[
    Daniel`ARC`ARCSimplifyRules[
        Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "3631a71a"]["Train"]]
    ]
    ,
    <|"Type" -> "Imputation", "Pattern" -> "Symmetry", "Output" -> "RepairedScene"|>
    ,
    TestID -> "ARCFindRules-20221002-SQVKWZ"
]

Test[
    Daniel`ARC`ARCSimplifyRules[
        Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "4938f0c2-easy"]["Train"]]
    ]
    ,
    {
        <||> -> <|
            "Transform" -> <|
                "Type" -> "AddComponents",
                "Components" -> {
                    <|
                        "Image" -> Inactive[Daniel`ARC`Transform][
                            Daniel`ARC`ObjectValue[
                                <|"Colors" -> {2}, "Context" -> "Component"|>,
                                "Image"
                            ],
                            <|"Type" -> "Flip", "Direction" -> "Horizontal"|>
                        ],
                        "Position" -> <|"RelativePosition" -> <|"Y" -> 0, "XInverse" -> 1|>|>
                    |>,
                    <|
                        "Image" -> Inactive[Daniel`ARC`Transform][
                            Daniel`ARC`ObjectValue[
                                <|"Colors" -> {2}, "Context" -> "Component"|>,
                                "Image"
                            ],
                            <|"Type" -> "Flip", "Direction" -> "Vertical"|>
                        ],
                        "Position" -> <|"RelativePosition" -> <|"YInverse" -> 1, "X" -> 0|>|>
                    |>,
                    <|
                        "Image" -> Inactive[Daniel`ARC`Transform][
                            Daniel`ARC`ObjectValue[
                                <|"Colors" -> {2}, "Context" -> "Component"|>,
                                "Image"
                            ],
                            <|"Type" -> "Rotation", "Angle" -> 180|>
                        ],
                        "Position" -> <|
                            "RelativePosition" -> <|"YInverse" -> 1, "XInverse" -> 1|>
                        |>
                    |>
                }
            |>
        |>
    }
    ,
    TestID -> "ARCFindRules-20221002-UBDTIY"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`ARCSimplifyRules[
                Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "d13f3404"]["Train"]]
            ]
        ]
    ]
    ,
    <|
        "Width" -> Inactive[Plus][Daniel`ARC`ObjectValue["InputScene", "Width"], 3],
        "Height" -> Inactive[Plus][Daniel`ARC`ObjectValue["InputScene", "Width"], 3],
        "Rules" -> <|
            "Type" -> "PatternFill",
            "StartY" -> 1,
            "StartX" -> 1,
            "TrajectoryY" -> 1,
            "TrajectoryX" -> 1
        |>
    |>
    ,
    TestID -> "ARCFindRules-20221003-KLSKL1"
]

Test[
    Daniel`ARC`ARCSimplifyRules[
        Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "feca6190"]["Train"]]
    ]
    ,
    <|
        "Width" -> Inactive[Times][Daniel`ARC`ObjectValue["InputScene", "ColorCount"], 5],
        "Height" -> Inactive[Times][Daniel`ARC`ObjectValue["InputScene", "ColorCount"], 5],
        "Rules" -> <|
            "Type" -> "PatternFill",
            "StartY" -> Inactive[Times][Daniel`ARC`ObjectValue["InputScene", "ColorCount"], 5],
            "StartX" -> 1,
            "TrajectoryY" -> -1,
            "TrajectoryX" -> 1
        |>
    |>
    ,
    TestID -> "ARCFindRules-20221004-Z4I7W9"
]

Test[
    Daniel`ARC`ARCSimplifyRules[
        Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "4938f0c2"]["Train"]]
    ]
    ,
    {
        <|"VerticalLineSymmetry" -> True|> -> <|"Same" -> True|>,
        <|"VerticalLineSymmetry" -> False|> -> <|
            "Transform" -> <|
                "Type" -> "AddComponents",
                "Components" -> {
                    <|
                        "Image" -> Inactive[Daniel`ARC`Transform][
                            Daniel`ARC`ObjectValue[
                                <|"Colors" -> {2}, "Context" -> "Component"|>,
                                "Image"
                            ],
                            <|"Type" -> "Flip", "Direction" -> "Horizontal"|>
                        ],
                        "Position" -> <|"RelativePosition" -> <|"Y" -> 0, "XInverse" -> 1|>|>
                    |>,
                    <|
                        "Image" -> Inactive[Daniel`ARC`Transform][
                            Daniel`ARC`ObjectValue[
                                <|"Colors" -> {2}, "Context" -> "Component"|>,
                                "Image"
                            ],
                            <|"Type" -> "Flip", "Direction" -> "Vertical"|>
                        ],
                        "Position" -> <|"RelativePosition" -> <|"YInverse" -> 1, "X" -> 0|>|>
                    |>,
                    <|
                        "Image" -> Inactive[Daniel`ARC`Transform][
                            Daniel`ARC`ObjectValue[
                                <|"Colors" -> {2}, "Context" -> "Component"|>,
                                "Image"
                            ],
                            <|"Type" -> "Rotation", "Angle" -> 180|>
                        ],
                        "Position" -> <|
                            "RelativePosition" -> <|"YInverse" -> 1, "XInverse" -> 1|>
                        |>
                    |>
                }
            |>
        |>
    }
    ,
    TestID -> "ARCFindRules-20221004-FWFWZT"
]

Test[
    Daniel`ARC`ARCSimplifyRules[
        Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "8eb1be9a"]["Train"]]
    ]
    ,
    <|
        "Width" -> Daniel`ARC`ObjectValue["InputScene", "Width"],
        "Height" -> Daniel`ARC`ObjectValue["InputScene", "Height"],
        "Rules" -> <|
            "Type" -> "PatternFill",
            "StartY" -> 1,
            "StartX" -> 1,
            "TrajectoryY" -> 3,
            "TrajectoryX" -> 0,
            "Bidirectional" -> True
        |>
    |>
    ,
    TestID -> "ARCFindRules-20221007-2JC2L9"
]

Test[
    Daniel`ARC`ARCSimplifyRules[
        Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "eb281b96"]["Train"]]
    ]
    ,
    <|
        "Width" -> Daniel`ARC`ObjectValue["InputScene", "Width"],
        "Height" -> Inactive[Plus][
            Inactive[Times][Daniel`ARC`ObjectValue["InputScene", "Height"], 4],
            -3
        ],
        "Rules" -> <|
            "Type" -> "PatternFill",
            "List" -> {
                <|
                    "StartY" -> 1,
                    "StartX" -> 1,
                    "TrajectoryY" -> Inactive[Plus][
                        Inactive[Times][Daniel`ARC`ObjectValue["InputScene", "Height"], 2],
                        -2
                    ],
                    "TrajectoryX" -> 0
                |>,
                <|
                    "StartY" -> Inactive[Plus][
                        Inactive[Times][Daniel`ARC`ObjectValue["InputScene", "Height"], 3],
                        -2
                    ],
                    "StartX" -> 1,
                    "TrajectoryY" -> Inactive[Plus][
                        Inactive[Times][Daniel`ARC`ObjectValue["InputScene", "Height"], -2],
                        2
                    ],
                    "TrajectoryX" -> 0,
                    "Transform" -> <|"Type" -> "Flip", "Direction" -> "Vertical"|>
                |>
            }
        |>
    |>
    ,
    TestID -> "ARCFindRules-20221008-TI6M0X"
]

Test[
    Daniel`ARC`ARCSimplifyRules[
        Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "91413438"]["Train"]]
    ]
    ,
    <|
        "Width" -> Inactive[Plus][
            Inactive[Times][Daniel`ARC`ObjectValue["InputScene", "FilledArea"], -3],
            27
        ],
        "Height" -> Inactive[Plus][
            Inactive[Times][Daniel`ARC`ObjectValue["InputScene", "FilledArea"], -3],
            27
        ],
        "Rules" -> <|
            "Type" -> "PatternFill",
            "StartY" -> 1,
            "StartX" -> 1,
            "TrajectoryY" -> 0,
            "TrajectoryX" -> 3,
            "Wrapped" -> True,
            "Count" -> Daniel`ARC`ObjectValue["InputScene", "FilledArea"]
        |>
    |>
    ,
    TestID -> "ARCFindRules-20221008-K3GJ6I"
]

Test[
    Daniel`ARC`ARCSimplifyRules[
        Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "a5f85a15"]["Train"]]
    ]
    ,
    {
        <||> -> <|
            "Shape" -> <|
                "Name" -> "Line",
                "Angle" -> 135,
                "Fill" -> {Daniel`ARC`ObjectValue[<|"X" -> 1|>, "Color"], 4}
            |>
        |>
    }
    ,
    TestID -> "ARCFindRules-20221011-N2JMIN"
]

Test[
    Daniel`ARC`ARCSimplifyRules[
        Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "6d75e8bb"]["Train"]]
    ]
    ,
    {<||> -> <|"Transform" -> <|"Type" -> "ColorMapping", "Mapping" -> {{-1, 2}}|>|>}
    ,
    TestID -> "ARCFindRules-20221014-RVMNV8"
]

Test[
    Daniel`ARC`ARCSimplifyRules[
        Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "63613498"]["Train"]]
    ]
    ,
    <|
        "FormMultiColorCompositeObjects" -> False,
        "Rules" -> {
            <|
                "MonochromeImage" -> Daniel`ARC`ObjectValue[
                    <|"Within" -> Daniel`ARC`Object[<|"Colors" -> {5}|>]|>,
                    "MonochromeImage"
                ],
                "Except" -> <|"Within" -> Daniel`ARC`Object[<|"Colors" -> {5}|>]|>
            |> -> <|
                "Color" -> 5
            |>
        }
    |>
    ,
    TestID -> "ARCFindRules-20221017-QK38XC"
]

Test[
    Daniel`ARC`ARCSimplifyRules[
        Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "29c11459"]["Train"]]
    ]
    ,
    <|
        "FormMultiColorCompositeObjects" -> False,
        "FollowDiagonals" -> False,
        "Rules" -> {
            <||> -> <|
                "Transform" -> <|
                    "Type" -> "MapComponents",
                    "Rules" -> {
                        <|"X.Rank" -> 2|> -> <|
                            "Shape" -> <|"Name" -> "Line", "Angle" -> 0|>,
                            "X2" -> 5
                        |>,
                        <|"X.Rank" -> 1|> -> <|
                            "Shape" -> <|"Name" -> "Line", "Angle" -> 0|>,
                            "X" -> 7
                        |>,
                        <|
                            "Transform" -> <|
                                "Type" -> "AddObjects",
                                "Objects" -> {
                                    <|
                                        "Image" -> Daniel`ARC`ARCScene[{{5}}],
                                        "Y" -> Daniel`ARC`ObjectValue["Parent", "Y"],
                                        "X" -> 6
                                    |>
                                }
                            |>
                        |>
                    }
                |>
            |>
        },
        "Groups" -> {
            <|
                "X" -> 1,
                "X2" -> 11,
                "Width" -> 11,
                "Height" -> 1,
                "Components" -> {
                    Repeated[
                        <|"Shape" -> <|"Name" -> "Pixel"|>, "Y" -> "Same"|>,
                        {2}
                    ]
                },
                "ZOrder" -> 0,
                "XMiddle" -> 6,
                "FilledArea" -> 2,
                "SurfacePixelCount" -> 2,
                "VerticalLineSymmetry" -> False,
                "HollowCount" -> 0
            |>
        }
    |>
    ,
    TestID -> "ARCFindRules-20221019-4FY74X"
]

Test[
    Daniel`ARC`ARCSimplifyRules[
        Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "af902bf9"]["Train"]]
    ]
    ,
    <|
        "FormMultiColorCompositeObjects" -> False,
        "FollowDiagonals" -> False,
        "Rules" -> {
            <||> -> <|
                "Transform" -> <|
                    "Type" -> "AddComponents",
                    "Components" -> {
                        <|
                            "Shape" -> <|"Name" -> "Rectangle", "Filled" -> True|>,
                            "Color" -> 2,
                            "Position" -> <|"RelativePosition" -> <|"Y" -> 1, "X" -> 1|>|>,
                            "Width" -> Inactive[Plus][
                                Daniel`ARC`ObjectValue["InputObject", "Width"],
                                -2
                            ],
                            "Height" -> Inactive[Plus][
                                Daniel`ARC`ObjectValue["InputObject", "Height"],
                                -2
                            ]
                        |>
                    }
                |>
            |>
        },
        "Groups" -> {
            <|
                "Components" -> {
                    Repeated[<|"Image" -> Daniel`ARC`ARCScene[{{4}}]|>, {4}]
                },
                "Color" -> 4,
                "ZOrder" -> 0,
                "FilledArea" -> 4,
                "SurfacePixelCount" -> 4,
                "VerticalLineSymmetry" -> True,
                "HorizontalLineSymmetry" -> True,
                "VerticalAndHorizontalLineSymmetry" -> True,
                "HollowCount" -> 0
            |>
        }
    |>
    ,
    TestID -> "ARCFindRules-20221022-68Y8Y3"
]

Test[
    Daniel`ARC`ARCSimplifyRules[
        Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "b6afb2da"]["Train"]]
    ]
    ,
    <|
        "FollowDiagonals" -> False,
        "Rules" -> {
            <||> -> <|
                "Transform" -> <|
                    "Type" -> "MapComponents",
                    "Rules" -> {
                        <||> -> <|"Transform" -> <|"Type" -> "Delete"|>|>,
                        <|
                            "Transform" -> <|
                                "Type" -> "AddObjects",
                                "Objects" -> {
                                    <|
                                        "Shape" -> <|"Name" -> "Pixel"|>,
                                        "Color" -> 1,
                                        "X" -> Daniel`ARC`ObjectValue["Parent", "X"],
                                        "Y" -> Daniel`ARC`ObjectValue["Parent", "Y"],
                                        "Width" -> 1,
                                        "Height" -> 1,
                                        "ZOrder" -> 0
                                    |>,
                                    <|
                                        "Shape" -> <|"Name" -> "Line", "Angle" -> 0|>,
                                        "Color" -> 4,
                                        "X" -> Inactive[Plus][
                                            Daniel`ARC`ObjectValue["Parent", "X"],
                                            1
                                        ],
                                        "Y" -> Daniel`ARC`ObjectValue["Parent", "Y"],
                                        "X2" -> Inactive[Plus][
                                            Daniel`ARC`ObjectValue["Parent", "X2"],
                                            -1
                                        ],
                                        "Height" -> 1,
                                        "ZOrder" -> 0
                                    |>,
                                    <|
                                        "Shape" -> <|"Name" -> "Pixel"|>,
                                        "Color" -> 1,
                                        "X" -> Daniel`ARC`ObjectValue["Parent", "X2"],
                                        "Y" -> Daniel`ARC`ObjectValue["Parent", "Y"],
                                        "Width" -> 1,
                                        "Height" -> 1,
                                        "ZOrder" -> 0
                                    |>,
                                    <|
                                        "Shape" -> <|"Name" -> "Line", "Angle" -> 90|>,
                                        "Color" -> 4,
                                        "X" -> Daniel`ARC`ObjectValue["Parent", "X"],
                                        "Y" -> Inactive[Plus][
                                            Daniel`ARC`ObjectValue["Parent", "Y"],
                                            1
                                        ],
                                        "Width" -> 1,
                                        "Y2" -> Inactive[Plus][
                                            Daniel`ARC`ObjectValue["Parent", "Y2"],
                                            -1
                                        ],
                                        "ZOrder" -> 0
                                    |>,
                                    <|
                                        "Shape" -> <|"Name" -> "Rectangle", "Filled" -> True|>,
                                        "Color" -> 2,
                                        "X" -> Inactive[Plus][
                                            Daniel`ARC`ObjectValue["Parent", "X"],
                                            1
                                        ],
                                        "Y" -> Inactive[Plus][
                                            Daniel`ARC`ObjectValue["Parent", "Y"],
                                            1
                                        ],
                                        "X2" -> Inactive[Plus][
                                            Daniel`ARC`ObjectValue["Parent", "X2"],
                                            -1
                                        ],
                                        "Y2" -> Inactive[Plus][
                                            Daniel`ARC`ObjectValue["Parent", "Y2"],
                                            -1
                                        ],
                                        "ZOrder" -> 0
                                    |>,
                                    <|
                                        "Shape" -> <|"Name" -> "Line", "Angle" -> 90|>,
                                        "Color" -> 4,
                                        "X" -> Daniel`ARC`ObjectValue["Parent", "X2"],
                                        "Y" -> Inactive[Plus][
                                            Daniel`ARC`ObjectValue["Parent", "Y"],
                                            1
                                        ],
                                        "Width" -> 1,
                                        "Y2" -> Inactive[Plus][
                                            Daniel`ARC`ObjectValue["Parent", "Y2"],
                                            -1
                                        ],
                                        "ZOrder" -> 0
                                    |>,
                                    <|
                                        "Shape" -> <|"Name" -> "Pixel"|>,
                                        "Color" -> 1,
                                        "X" -> Daniel`ARC`ObjectValue["Parent", "X"],
                                        "Y" -> Daniel`ARC`ObjectValue["Parent", "Y2"],
                                        "Width" -> 1,
                                        "Height" -> 1,
                                        "ZOrder" -> 0
                                    |>,
                                    <|
                                        "Shape" -> <|"Name" -> "Line", "Angle" -> 0|>,
                                        "Color" -> 4,
                                        "X" -> Inactive[Plus][
                                            Daniel`ARC`ObjectValue["Parent", "X"],
                                            1
                                        ],
                                        "Y" -> Daniel`ARC`ObjectValue["Parent", "Y2"],
                                        "X2" -> Inactive[Plus][
                                            Daniel`ARC`ObjectValue["Parent", "X2"],
                                            -1
                                        ],
                                        "Height" -> 1,
                                        "ZOrder" -> 0
                                    |>,
                                    <|
                                        "Shape" -> <|"Name" -> "Pixel"|>,
                                        "Color" -> 1,
                                        "X" -> Daniel`ARC`ObjectValue["Parent", "X2"],
                                        "Y" -> Daniel`ARC`ObjectValue["Parent", "Y2"],
                                        "Width" -> 1,
                                        "Height" -> 1,
                                        "ZOrder" -> 0
                                    |>
                                }
                            |>
                        |>
                    }
                |>
            |>
        }
    |>
    ,
    TestID -> "ARCFindRules-20221022-MLA34O"
]

Test[
    Daniel`ARC`ARCSimplifyRules[
        Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "963e52fc"]["Train"]]
    ]
    ,
    <|
        "Width" -> Inactive[Daniel`ARC`ObjectValue["InputScene", "Width"]*2],
        "Rules" -> {<||> -> <|"X2Inverse" -> 1|>}
    |>
    ,
    TestID -> "ARCFindRules-20221023-HWMCYT"
]

Test[
    Daniel`ARC`ARCSimplifyRules[
        Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "d364b489"]["Train"]]
    ]
    ,
    <|
        "FormMultiColorCompositeObjects" -> False,
        "FollowDiagonals" -> False,
        "Rules" -> {
            <||> -> <|
                "Transform" -> <|
                    "Type" -> "AddComponents",
                    "Components" -> {
                        <|
                            "Image" -> Daniel`ARC`ARCScene[{{2}}],
                            "Position" -> <|"RelativePosition" -> <|"Y" -> -1, "X" -> 0|>|>
                        |>,
                        <|
                            "Image" -> Daniel`ARC`ARCScene[{{7}}],
                            "Position" -> <|"RelativePosition" -> <|"Y" -> 0, "X" -> -1|>|>
                        |>,
                        <|
                            "Image" -> Daniel`ARC`ARCScene[{{6}}],
                            "Position" -> <|"RelativePosition" -> <|"Y" -> 0, "X" -> 1|>|>
                        |>,
                        <|
                            "Image" -> Daniel`ARC`ARCScene[{{8}}],
                            "Position" -> <|"RelativePosition" -> <|"Y" -> 1, "X" -> 0|>|>
                        |>
                    }
                |>
            |>
        }
    |>
    ,
    TestID -> "ARCFindRules-20221023-M4PSAD"
]

Test[
    Daniel`ARC`ARCSimplifyRules[
        Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "1e0a9b12"]["Train"]]
    ]
    ,
    <|
        "FormMultiColorCompositeObjects" -> False,
        "Rules" -> {
            <||> -> <|
                "Shapes" -> {<|"Name" -> "Line", "Angle" -> 90|>},
                "YInverse" -> Daniel`ARC`ObjectValue["InputObject", "FilledArea"],
                "Y2Inverse" -> 1
            |>
        },
        "Groups" -> {
            <|
                "Width" -> 1,
                "Components" -> {
                    Repeated[
                        <|
                            "Shapes" -> {<|"Name" -> "Line", "Angle" -> 90|>},
                            "ZOrder" -> 0,
                            "Width" -> 1,
                            "FilledProportion" -> 1.,
                            "HorizontalLineSymmetry" -> True,
                            "Width.Rank" -> 1,
                            "Width.InverseRank" -> 1,
                            "FilledProportion.Rank" -> 1,
                            "FilledProportion.InverseRank" -> 1,
                            "Color" -> "Same",
                            "ColorUseCount" -> "Same",
                            "ColorUseCount.InverseRank" -> "Same",
                            "ColorUseCount.Rank" -> "Same",
                            "X" -> "Same",
                            "X2" -> "Same",
                            "XMiddle" -> "Same"
                        |>,
                        {2}
                    ]
                },
                "ZOrder" -> 0,
                "PrimarySizeDimension" -> "Y",
                "HollowCount" -> 0
            |>
        }
    |>
    ,
    TestID -> "ARCFindRules-20221029-8JXO9P"
]

Test[
    Daniel`ARC`ARCSimplifyRules[
        Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "7e0986d6"]["Train"]]
    ]
    ,
    <|"Denoise" -> True, "Rules" -> {<||> -> <|"Same" -> True|>}|>
    ,
    TestID -> "ARCFindRules-20221105-2VTFKM"
]

Test[
    Daniel`ARC`ARCSimplifyRules[
        Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "56dc2b01"]["Train"]]
    ]
    ,
    <|
        "RotationNormalization" -> <|
            Daniel`ARC`Object[<|"Colors" -> {3}|>] -> <|"X.InverseRank" -> 1|>,
            Daniel`ARC`Object[<|"Colors" -> {2}|>] -> <|"X.InverseRank" -> 2|>
        |>,
        "FormMultiColorCompositeObjects" -> False,
        "CheckForGridsAndDividers" -> False,
        "Rules" -> {
            <|"Area.Rank" -> 1|> -> <|
                "Transform" -> <|
                    "Type" -> "Move",
                    "BlockedBy" -> Daniel`ARC`Object[<|"Colors" -> {2}, "Context" -> "Output"|>]
                |>
            |>,
            <|"Area.Rank" -> 2|> -> <|"Same" -> True|>,
            <|
                "Transform" -> <|
                    "Type" -> "AddObjects",
                    "Objects" -> {
                        <|
                            "Shape" -> <|"Name" -> "Line", "Angle" -> 90|>,
                            "Color" -> 8,
                            "X" -> Inactive[Plus][
                                Daniel`ARC`ObjectValue[
                                    <|"Colors" -> {3}, "Context" -> "Output"|>,
                                    "X"
                                ],
                                -1
                            ],
                            "Y" -> 1,
                            "Width" -> 1,
                            "Y2Inverse" -> 1,
                            "ZOrder" -> 0
                        |>
                    }
                |>
            |>
        }
    |>
    ,
    TestID -> "ARCFindRules-20221107-7J9NW9"
]

Test[
    Daniel`ARC`ARCSimplifyRules[
        Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "56dc2b01"]["Train"]]
    ]
    ,
    <|
        "RotationNormalization" -> <|
            Daniel`ARC`Object[<|"Colors" -> {3}|>] -> <|"X.InverseRank" -> 1|>,
            Daniel`ARC`Object[<|"Colors" -> {2}|>] -> <|"X.InverseRank" -> 2|>
        |>,
        "FormMultiColorCompositeObjects" -> False,
        "CheckForGridsAndDividers" -> False,
        "Rules" -> {
            <|"Area.Rank" -> 1|> -> <|
                "Transform" -> <|
                    "Type" -> "Move",
                    "BlockedBy" -> Daniel`ARC`Object[<|"Colors" -> {2}, "Context" -> "Output"|>]
                |>
            |>,
            <|"Area.Rank" -> 2|> -> <|"Same" -> True|>,
            <|
                "Transform" -> <|
                    "Type" -> "AddObjects",
                    "Objects" -> {
                        <|
                            "Shape" -> <|"Name" -> "Line", "Angle" -> 90|>,
                            "Color" -> 8,
                            "X" -> Inactive[Plus][
                                Daniel`ARC`ObjectValue[
                                    <|"Colors" -> {3}, "Context" -> "Output"|>,
                                    "X"
                                ],
                                -1
                            ],
                            "Y" -> 1,
                            "Width" -> 1,
                            "Y2Inverse" -> 1,
                            "ZOrder" -> 0
                        |>
                    }
                |>
            |>
        }
    |>
    ,
    TestID -> "ARCFindRules-20221107-S4HZP4"
]

Test[
    Daniel`ARC`ARCSimplifyRules[
        Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "d6ad076f"]["Train"]]
    ]
    ,
    <|
        "RotationNormalization" -> <|"ObjectsAngle" -> 90|>,
        "FormMultiColorCompositeObjects" -> False,
        "FollowDiagonals" -> False,
        "Rules" -> {
            <||> -> <|"Same" -> True|>,
            <|
                "Transform" -> <|
                    "Type" -> "AddObjects",
                    "Objects" -> {
                        <|
                            "Shape" -> <|"Name" -> "Rectangle", "Filled" -> True|>,
                            "Color" -> 8,
                            "X" -> Inactive[Plus][
                                Daniel`ARC`ObjectValue[<|"Area.Rank" -> 2|>, "X"],
                                1
                            ],
                            "Y" -> Inactive[Plus][
                                Daniel`ARC`ObjectValue[<|"Y.Rank" -> 2|>, "Y2"],
                                1
                            ],
                            "X2" -> Inactive[Plus][
                                Daniel`ARC`ObjectValue[<|"Area.Rank" -> 2|>, "X2"],
                                -1
                            ],
                            "Y2Inverse" -> Daniel`ARC`ObjectValue[<|"Y.Rank" -> 2|>, "Y2"],
                            "ZOrder" -> 0
                        |>
                    }
                |>
            |>
        }
    |>
    ,
    TestID -> "ARCFindRules-20221109-DCV8DK"
]

Test[
    Daniel`ARC`ARCSimplifyRules[
        Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "f8a8fe49"]["Train"]]
    ]
    ,
    <|
        "RotationNormalization" -> <|"ObjectsAngle" -> 90|>,
        "FormMultiColorCompositeObjects" -> False,
        "Rules" -> {
            <|"Area.Rank" -> 1|> -> <|"Same" -> True|>,
            <|"Area.Rank" -> 2|> -> <|
                "Image" -> Inactive[Daniel`ARC`Transform][
                    Daniel`ARC`ObjectValue[<|"Area.Rank" -> 2|>, "Image"],
                    <|"Type" -> "Flip", "Direction" -> "Vertical"|>
                ],
                "Position" -> {
                    Daniel`ARC`ObjectValue[<|"Area.Rank" -> 2|>, "X.Rank"],
                    Daniel`ARC`ObjectValue[<|"Area.Rank" -> 2|>, "X"]
                }
            |>,
            <|"Area.Rank" -> 3|> -> <|
                "Y" -> Inactive[Plus][Daniel`ARC`ObjectValue["InputObject", "Y"], 4]
            |>
        }
    |>
    ,
    TestID -> "ARCFindRules-20221109-OAK00V"
]

Test[
    Daniel`ARC`ARCSimplifyRules[
        Daniel`ARC`ARCFindRules[Daniel`ARC`ARCParseFile[file = "5168d44c"]["Train"]]
    ]
    ,
    <|
        "RotationNormalization" -> <|"ObjectsAngle" -> 0, "FavoredRotationAngle" -> -90|>,
        "FormMultiColorCompositeObjects" -> False,
        "FollowDiagonals" -> False,
        "Rules" -> {
            <|"Area.Rank" -> 1|> -> <|
                "X" -> Inactive[Plus][Daniel`ARC`ObjectValue["InputObject", "X"], 2]
            |>
        }
    |>
    ,
    TestID -> "ARCFindRules-20221110-C47ACU"
]