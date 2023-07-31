(*
    Tests for: Daniel`ARC`ARCChooseBestTransform
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCChooseBestTransform]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ARCChooseBestTransform[
        {
            <|
                "Transform" -> <|
                    "Type" -> "Move",
                    "Offset" -> <|
                        "Y" -> Daniel`ARC`ObjectValue[<|"Colors" -> {4}|>, "YRank"],
                        "X" -> 0
                    |>
                |>
            |>,
            <|
                "Transform" -> <|
                    "Type" -> "Move",
                    "Position" -> <|"Y" -> Daniel`ARC`ObjectValue[<|"Colors" -> {1}|>, "Y"]|>
                |>
            |>
        }
    ]
    ,
    <|
        "Transform" -> <|
            "Type" -> "Move",
            "Position" -> <|"Y" -> Daniel`ARC`ObjectValue[<|"Colors" -> {1}|>, "Y"]|>
        |>
    |>
    ,
    TestID -> "ARCChooseBestTransform-20220804-V1W2ZT"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCChooseBestTransform[
            {
                <|
                    "Transform" -> <|
                        "Type" -> "Move",
                        "Position" -> <|
                            "Y" -> Daniel`ARC`ObjectValue[<|"Colors" -> {4}|>, "Y"]
                        |>
                    |>
                |>,
                <|
                    "Transform" -> <|
                        "Type" -> "Move",
                        "Offset" -> <|
                            "Y" -> Daniel`ARC`ObjectValue[<|"Colors" -> {1}|>, "YRank"],
                            "X" -> 0
                        |>
                    |>
                |>
            }
        ]
    ]
    ,
    <|
        "Transform" -> <|
            "Type" -> "Move",
            "Position" -> <|"Y" -> Daniel`ARC`ObjectValue[<|"Colors" -> {4}|>, "Y"]|>
        |>
    |>
    ,
    TestID -> "ARCChooseBestTransform-20220805-WL0Y67"
]

Test[
    Daniel`ARC`ARCChooseBestTransform[
        {
            "X" -> Inactive[Plus][Daniel`ARC`ObjectValue[<|"Y2InverseRank" -> 2|>, "Width"], -1],
            "XInverse" -> 0
        }
    ]
    ,
    "XInverse" -> 0
    ,
    TestID -> "ARCChooseBestTransform-20220818-K8OYIN"
]

Test[
    Daniel`ARC`ARCChooseBestTransform[{"X" -> 0, "XInverse" -> 0}]
    ,
    "X" -> 0
    ,
    TestID -> "ARCChooseBestTransform-20220818-247NKD"
]

Test[
    Daniel`ARC`ARCChooseBestTransform[
        {
            <|
                "Y" -> Inactive[Plus][Daniel`ARC`ObjectValue["InputObject", "Y"], -1],
                "X" -> Inactive[Plus][Daniel`ARC`ObjectValue["InputObject", "X"], -1]
            |>,
            <|"RelativePosition" -> {-2, -2}|>
        }
    ]
    ,
    <|"RelativePosition" -> {-2, -2}|>
    ,
    TestID -> "ARCChooseBestTransform-20220826-6O5JRI"
]

Test[
    Daniel`ARC`ARCChooseBestTransform[
        {
            <|"Y" -> Inactive[Plus][Daniel`ARC`ObjectValue["InputObject", "X2"], -4]|>,
            <|"Y" -> Daniel`ARC`ObjectValue[<|"Colors" -> {}|>, "Y"]|>
        }
    ]
    ,
    <|"Y" -> Daniel`ARC`ObjectValue[<|"Colors" -> {}|>, "Y"]|>
    ,
    TestID -> "ARCChooseBestTransform-20220827-R3ZI5X"
]

Test[
    Daniel`ARC`ARCChooseBestTransform[
        {
            <|
                "X" -> Inactive[Plus][
                    Daniel`ARC`ObjectValue[
                        <|"Colors" -> {1}, "Context" -> "Component"|>,
                        "XRelative"
                    ],
                    1
                ]
            |>,
            <|"X" -> Daniel`ARC`ObjectValue[<|"X2.InverseRank" -> 2|>, "Width"]|>
        }
    ]
    ,
    <|
        "X" -> Inactive[Plus][
            Daniel`ARC`ObjectValue[
                <|"Colors" -> {1}, "Context" -> "Component"|>,
                "XRelative"
            ],
            1
        ]
    |>
    ,
    TestID -> "ARCChooseBestTransform-20220827-APSTJD"
]

Test[
    Daniel`ARC`ARCChooseBestTransform[
        {
            <|"Width" -> Daniel`ARC`ObjectValue["InputScene", "ObjectCount"]|>,
            <|"Width" -> Daniel`ARC`ObjectValue[<|"Y.InverseRank" -> 1|>, "Y.Rank"]|>
        }
    ]
    ,
    <|"Width" -> Daniel`ARC`ObjectValue["InputScene", "ObjectCount"]|>
    ,
    TestID -> "ARCChooseBestTransform-20220909-TYUQI3"
]

Test[
    Daniel`ARC`ARCChooseBestTransform[{"Width" -> 6, "X2" -> 6, "X2Inverse" -> 1}]
    ,
    "X2Inverse" -> 1
    ,
    TestID -> "ARCChooseBestTransform-20220910-Q2YR41"
]

Test[
    Daniel`ARC`ARCChooseBestTransform[
        {
            <|"Color" -> Daniel`ARC`ClassValue[<|"Colors" -> Except[{5}]|>, "Color"]|>,
            <|"Color" -> Daniel`ARC`ClassValue[<|"XMiddle.Rank" -> 1|>, "Color"]|>,
            <|
                "Color" -> Daniel`ARC`ClassValue[
                    <|"FilledProportion.InverseRank" -> 2|>,
                    "Color"
                ]
            |>
        }
    ]
    ,
    <|"Color" -> Daniel`ARC`ClassValue[<|"Colors" -> Except[{5}]|>, "Color"]|>
    ,
    TestID -> "ARCChooseBestTransform-20220917-5877CF"
]

Test[
    Daniel`ARC`ARCChooseBestTransform[
        {
            "Shape" -> <|
                "Name" -> "Line",
                "Angle" -> 0,
                "Fill" -> {Daniel`ARC`ObjectValue["InputObject", "Color"], 5}
            |>,
            "Shapes" -> {<|"Name" -> "Rectangle", "Filled" -> True|>}
        }
    ]
    ,
    "Shape" -> <|
        "Name" -> "Line",
        "Angle" -> 0,
        "Fill" -> {Daniel`ARC`ObjectValue["InputObject", "Color"], 5}
    |>
    ,
    TestID -> "ARCChooseBestTransform-20221011-ABQ0YS"
]