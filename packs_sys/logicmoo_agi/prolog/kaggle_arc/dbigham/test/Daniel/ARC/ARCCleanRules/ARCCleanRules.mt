(*
    Tests for: Daniel`ARC`ARCCleanRules
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCCleanRules]
    
    Author: danielb
*)

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCCleanRules[
            {
                <|"Colors" -> {2}|> -> <|
                    "Transform" -> <|
                        "Type" -> "AddComponents",
                        "Components" -> {
                            <|
                                "Image" -> Daniel`ARC`ARCScene[{{4}}],
                                "Position" -> <|
                                    "RelativePosition" -> <|
                                        "Y" -> -1,
                                        "X" -> -1,
                                        "YInverse" -> -1,
                                        "XInverse" -> -1
                                    |>
                                |>
                            |>,
                            <|
                                "Image" -> Daniel`ARC`ARCScene[{{4}}],
                                "Position" -> <|
                                    "RelativePosition" -> <|
                                        "Y" -> -1,
                                        "X" -> 1,
                                        "YInverse" -> -1,
                                        "XInverse" -> 1
                                    |>
                                |>
                            |>,
                            <|
                                "Image" -> Daniel`ARC`ARCScene[{{4}}],
                                "Position" -> <|
                                    "RelativePosition" -> <|
                                        "Y" -> 1,
                                        "X" -> -1,
                                        "YInverse" -> 1,
                                        "XInverse" -> -1
                                    |>
                                |>
                            |>,
                            <|
                                "Image" -> Daniel`ARC`ARCScene[{{4}}],
                                "Position" -> <|
                                    "RelativePosition" -> <|
                                        "Y" -> 1,
                                        "X" -> 1,
                                        "YInverse" -> 1,
                                        "XInverse" -> 1
                                    |>
                                |>
                            |>
                        }
                    |>
                |>,
                <|"Colors" -> {1}|> -> <|
                    "Transform" -> <|
                        "Type" -> "AddComponents",
                        "Components" -> {
                            <|
                                "Image" -> Daniel`ARC`ARCScene[
                                    {{-1, 7, -1}, {7, -1, 7}, {-1, 7, -1}}
                                ],
                                "Position" -> <|
                                    "RelativePosition" -> <|
                                        "Y" -> -1,
                                        "X" -> -1,
                                        "YInverse" -> -1,
                                        "XInverse" -> -1
                                    |>
                                |>
                            |>
                        }
                    |>
                |>,
                <|"Colors" -> Except[{2} | {1}]|> -> <|"Same" -> True|>
            },
            {}
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
        <|"Colors" -> Except[{2} | {1}]|> -> <|"Same" -> True|>
    }
    ,
    TestID -> "ARCCleanRules-20220808-YZXCS1"
]

Test[
    Daniel`ARC`ARCCleanRules[
        {
            <|"FilledArea.Rank" -> 1|> -> <|
                "Transform" -> <|"Type" -> "Move", "Position" -> <|"Y" -> 1, "X" -> 1|>|>
            |>,
            <|"FilledArea.Rank" -> 3 | 2|> -> <|"Transform" -> <|"Type" -> "Delete"|>|>
        },
        {<|"FilledArea.Rank" -> 1|>, <|"FilledArea.Rank" -> 2|>, <|"FilledArea.Rank" -> 3|>}
    ]
    ,
    {
        <|"FilledArea.Rank" -> 1|> -> <|
            "Transform" -> <|"Type" -> "Move", "Position" -> <|"Y" -> 1, "X" -> 1|>|>
        |>,
        <|"FilledArea.Rank" -> Except[1]|> -> <|"Transform" -> <|"Type" -> "Delete"|>|>
    }
    ,
    TestID -> "ARCCleanRules-20220902-RPQOPE"
]

Test[
    Daniel`ARC`ARCCleanRules[
        <|
            "Rules" -> {
                <|"FilledArea.Rank" -> 1|> -> <|
                    "Transform" -> <|"Type" -> "Move", "Position" -> <|"Y" -> 1, "X" -> 1|>|>
                |>,
                <|"FilledArea.Rank" -> 3 | 2|> -> <|"Transform" -> <|"Type" -> "Delete"|>|>
            }
        |>,
        {<|"FilledArea.Rank" -> 1|>, <|"FilledArea.Rank" -> 2|>, <|"FilledArea.Rank" -> 3|>}
    ]
    ,
    <|
        "Rules" -> {
            <|"FilledArea.Rank" -> 1|> -> <|
                "Transform" -> <|"Type" -> "Move", "Position" -> <|"Y" -> 1, "X" -> 1|>|>
            |>,
            <|"FilledArea.Rank" -> Except[1]|> -> <|"Transform" -> <|"Type" -> "Delete"|>|>
        }
    |>
    ,
    TestID -> "ARCCleanRules-20220905-JKLAZ4"
]

Test[
    Daniel`ARC`ARCCleanRules[
        <|
            "Rules" -> {
                <||> -> <|
                    "Shape" -> Daniel`ARC`ARCScene[{{1}}],
                    "X2" -> Inactive[Plus][Daniel`ARC`ObjectValue["InputObject", "X2"], 1],
                    "Width" -> 3,
                    "Height" -> 6
                |>
            }
        |>,
        {}
    ]
    ,
    <|
        "Rules" -> {
            <||> -> <|
                "Shape" -> Daniel`ARC`ARCScene[{{1}}],
                "X2" -> Inactive[Plus][Daniel`ARC`ObjectValue["InputObject", "X2"], 1]
            |>
        }
    |>
    ,
    TestID -> "ARCCleanRules-20221112-XA42PG"
]