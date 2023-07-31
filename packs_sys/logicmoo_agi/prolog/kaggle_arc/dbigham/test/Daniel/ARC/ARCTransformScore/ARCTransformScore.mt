(*
    Tests for: Daniel`ARC`ARCTransformScore
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCTransformScore]
    
    Author: danielb
*)

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCTransformScore[
            <|
                "Type" -> "Move",
                "Position" -> <|
                    "Y" -> Daniel`ARC`ObjectValue[<|"Colors" -> {4}|>, "Y"],
                    "X" -> 0
                |>
            |>
        ]
    ]
    ,
    -0.7804722738154957
    ,
    TestID -> "ARCTransformScore-20220804-R5DIE5"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCTransformScore[
            <|
                "Type" -> "Move",
                "Offset" -> <|
                    "Y" -> Daniel`ARC`ObjectValue[<|"Colors" -> {4}|>, "X"],
                    "X" -> 0
                |>
            |>
        ]
    ]
    ,
    -1.2604722738154956
    ,
    TestID -> "ARCTransformScore-20220804-P26PZ7"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCTransformScore[
            <|
                "Type" -> "Move",
                "Offset" -> <|
                    "Y" -> Daniel`ARC`ObjectValue[<|"Colors" -> {4}|>, "YRank"],
                    "X" -> 0
                |>
            |>
        ]
    ]
    ,
    -1.9004722738154958
    ,
    TestID -> "ARCTransformScore-20220804-XKHR3Z"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCTransformScore[
            <|"Type" -> "Move", "Offset" -> <|"Y" -> 1, "X" -> 0|>|>
        ]
    ]
    ,
    -0.26
    ,
    TestID -> "ARCTransformScore-20220805-0X48JF"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCTransformScore[
            <|
                "Type" -> "Move",
                "Offset" -> <|
                    "Y" -> Inactive[Plus][Daniel`ARC`ObjectValue[<|"Colors" -> {8}|>, "Y"], -5],
                    "X" -> Inactive[Plus][Daniel`ARC`ObjectValue[<|"Colors" -> {8}|>, "X"], -4]
                |>
            |>
        ]
    ]
    ,
    -2.6059445476309913
    ,
    TestID -> "ARCTransformScore-20220818-PBN4K1"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCTransformScore[
            <|"Type" -> "Move", "BlockedBy" -> Daniel`ARC`Object[<|"Colors" -> {8}|>]|>
        ]
    ]
    ,
    -0.48
    ,
    TestID -> "ARCTransformScore-20220818-8BHZGI"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCTransformScore[
            <|
                "Color" -> Inactive[Plus][
                    Daniel`ARC`ObjectValue[<|"X" -> 2|>, "Height.Rank"],
                    -1
                ]
            |>
        ]
    ]
    ,
    -2.6585871324805668
    ,
    TestID -> "ARCTransformScore-20220825-4RBNEY"
]

Test[
    Daniel`ARC`ARCTransformScore[<|"Color" -> 4|>]
    ,
    -0.1
    ,
    TestID -> "ARCTransformScore-20220825-CWTDTY"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCTransformScore[
            <|
                "X" -> Inactive[Plus][
                    Daniel`ARC`ObjectValue[
                        <|"Colors" -> {1}, "Context" -> "Component"|>,
                        "XRelative"
                    ],
                    1
                ]
            |>
        ]
    ]
    ,
    -0.8506481449381562
    ,
    TestID -> "ARCTransformScore-20220905-TREZ4Z"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCTransformScore[
            <|
                "X" -> Inactive[Plus][
                    Daniel`ARC`ObjectValue[
                        <|"XMiddle" -> 3, "Context" -> "Component"|>,
                        "Width"
                    ],
                    1
                ]
            |>
        ]
    ]
    ,
    -1.8841984052872978
    ,
    TestID -> "ARCTransformScore-20220905-3095OU"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCTransformScore[
            <|"Width" -> Daniel`ARC`ObjectValue["InputScene", "ObjectCount"]|>
        ]
    ]
    ,
    -1.2
    ,
    TestID -> "ARCTransformScore-20220909-WAIQU4"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCTransformScore["X2" -> 6]
    ]
    ,
    -0.07
    ,
    TestID -> "ARCTransformScore-20220910-SLVDL3"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCTransformScore["X2Inverse" -> 1]
    ]
    ,
    -0.020000000000000004
    ,
    TestID -> "ARCTransformScore-20220910-WIS6O9"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCTransformScore[
            <|"Color" -> Daniel`ARC`ClassValue[<|"Colors" -> Except[{5}]|>, "Color"]|>
        ]
    ]
    ,
    -0.6602738113403046
    ,
    TestID -> "ARCTransformScore-20220917-WMF8CB"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCTransformScore[
            <|"Color" -> Daniel`ARC`ClassValue[<|"XMiddle.Rank" -> 1|>, "Color"]|>
        ]
    ]
    ,
    -1.0040151617906417
    ,
    TestID -> "ARCTransformScore-20220917-9MWWQA"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCTransformScore[
            <|
                "Color" -> Daniel`ARC`ClassValue[
                    <|"FilledProportion.InverseRank" -> 2|>,
                    "Color"
                ]
            |>
        ]
    ]
    ,
    -1.3598692513674213
    ,
    TestID -> "ARCTransformScore-20220917-PW5Q7G"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCTransformScore[
            <|
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
        ]
    ]
    ,
    -3.085433065567418
    ,
    TestID -> "ARCTransformScore-20221005-5GARZ7"
]

Test[
    Daniel`ARC`ARCTransformScore[
        <|"Y" -> Inactive[Plus][Daniel`ARC`ObjectValue[<|"Y.Rank" -> 2|>, "Y2"], 1]|>
    ]
    ,
    -0.7879521278602413
    ,
    TestID -> "ARCTransformScore-20221109-2U7DOJ"
]

Test[
    Daniel`ARC`ARCTransformScore[
        <|"Y" -> Inactive[Plus][Daniel`ARC`ObjectValue[<|"Y.Rank" -> 2|>, "Y"], 3]|>
    ]
    ,
    -1.1079521278602413
    ,
    TestID -> "ARCTransformScore-20221109-O7NJE6"
]

Test[
    Daniel`ARC`ARCTransformScore[
        <|"X2" -> Inactive[Plus][Daniel`ARC`ObjectValue[<|"Area.Rank" -> 2|>, "X2"], -1]|>
    ]
    ,
    -1.112979589711327
    ,
    TestID -> "ARCTransformScore-20221109-QOOSVQ"
]

Test[
    Daniel`ARC`ARCTransformScore[
        <|
            "Width" -> Inactive[Plus][Daniel`ARC`ObjectValue[<|"Area.Rank" -> 2|>, "Width"], -2]
        |>
    ]
    ,
    -1.2079795897113272
    ,
    TestID -> "ARCTransformScore-20221109-ARNAM4"
]

Test[
    Daniel`ARC`ARCTransformScore[
        <|"X2" -> Inactive[Times][Daniel`ARC`ObjectValue[<|"Area.Rank" -> 2|>, "X"], 2]|>
    ]
    ,
    -1.2529795897113272
    ,
    TestID -> "ARCTransformScore-20221109-6NC6YM"
]