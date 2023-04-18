(*
    Tests for: Daniel`ARC`ARCTestRules
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCTestRules]
    
    Author: danielb
*)

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Utility`BlockUUID[
                ERPTesting`NormalizeOutput["Key" -> "TestIndex"][
                    ERPTesting`NormalizeOutput["Key" -> "AbsoluteTimeUsed"][
                        ERPTesting`NormalizeOutput["Key" -> "CPUTimeUsed"][
                            ERPTesting`NormalizeOutput["Key" -> "MemoryUsed"][
                                With[
                                    {
                                        examples = Daniel`ARC`ARCParseFile[file = "0ca9ddb6"][
                                            "Train"
                                        ]
                                    },
                                    Daniel`ARC`ARCTestRules[
                                        examples[[1]],
                                        Utility`ReturnIfFailure[
                                            Daniel`ARC`ARCFindRules[examples]
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
    ]
    ,
    TestResultObject[
        <|
            "TestClass" -> None,
            "TestIndex" -> _Integer,
            "TestID" -> None,
            "Outcome" -> "Success",
            "Input" -> HoldForm[
                Daniel`ARC`ARCApplyRules[
                    <|
                        "Input" -> Daniel`ARC`ARCScene[
                            {
                                {0, 0, 0, 0, 0, 0, 0, 0, 0},
                                {0, 0, 0, 0, 0, 0, 0, 0, 0},
                                {0, 0, 0, 0, 0, 0, 0, 0, 0},
                                {0, 0, 2, 0, 0, 0, 0, 0, 0},
                                {0, 0, 0, 0, 0, 0, 0, 0, 0},
                                {0, 0, 0, 0, 0, 0, 0, 0, 0},
                                {0, 0, 0, 0, 0, 0, 1, 0, 0},
                                {0, 0, 0, 0, 0, 0, 0, 0, 0},
                                {0, 0, 0, 0, 0, 0, 0, 0, 0}
                            }
                        ],
                        "Output" -> Daniel`ARC`ARCScene[
                            {
                                {0, 0, 0, 0, 0, 0, 0, 0, 0},
                                {0, 0, 0, 0, 0, 0, 0, 0, 0},
                                {0, 4, 0, 4, 0, 0, 0, 0, 0},
                                {0, 0, 2, 0, 0, 0, 0, 0, 0},
                                {0, 4, 0, 4, 0, 0, 0, 0, 0},
                                {0, 0, 0, 0, 0, 0, 7, 0, 0},
                                {0, 0, 0, 0, 0, 7, 1, 7, 0},
                                {0, 0, 0, 0, 0, 0, 7, 0, 0},
                                {0, 0, 0, 0, 0, 0, 0, 0, 0}
                            }
                        ]
                    |>["Input"],
                    <|
                        "Rules" -> {
                            <|"Colors" -> {1}|> -> <|
                                "Transform" -> <|
                                    "Type" -> "AddComponents",
                                    "Components" -> {
                                        <|
                                            "Image" -> Daniel`ARC`ARCScene[
                                                {{-1, 7, -1}, {7, -1, 7}, {-1, 7, -1}}
                                            ],
                                            "Position" -> <|
                                                "RelativePosition" -> <|"Y" -> -1, "X" -> -1|>
                                            |>
                                        |>
                                    }
                                |>,
                                "Examples" -> {1, 2, 3},
                                "ExampleCount" -> 3,
                                "UseCount" -> 4,
                                "InputObjects" -> {
                                    "e7a71aa3-1a87-4e68-a1ce-009fa20742es",
                                    "e7a71aa3-1a87-4e68-a1ce-009fa20742fs",
                                    "e7a71aa3-1a87-4e68-a1ce-009fa20742fr",
                                    "e7a71aa3-1a87-4e68-a1ce-009fa20742hz"
                                }
                            |>,
                            <|"Colors" -> {2}|> -> <|
                                "Transform" -> <|
                                    "Type" -> "AddComponents",
                                    "Components" -> {
                                        <|
                                            "Image" -> Daniel`ARC`ARCScene[{{4}}],
                                            "Position" -> <|
                                                "RelativePosition" -> <|"Y" -> -1, "X" -> -1|>
                                            |>
                                        |>,
                                        <|
                                            "Image" -> Daniel`ARC`ARCScene[{{4}}],
                                            "Position" -> <|
                                                "RelativePosition" -> <|"Y" -> -1, "X" -> 1|>
                                            |>
                                        |>,
                                        <|
                                            "Image" -> Daniel`ARC`ARCScene[{{4}}],
                                            "Position" -> <|
                                                "RelativePosition" -> <|"Y" -> 1, "X" -> -1|>
                                            |>
                                        |>,
                                        <|
                                            "Image" -> Daniel`ARC`ARCScene[{{4}}],
                                            "Position" -> <|
                                                "RelativePosition" -> <|"Y" -> 1, "X" -> 1|>
                                            |>
                                        |>
                                    }
                                |>,
                                "Examples" -> {1, 2, 3},
                                "ExampleCount" -> 3,
                                "UseCount" -> 4,
                                "InputObjects" -> {
                                    "e7a71aa3-1a87-4e68-a1ce-009fa20742et",
                                    "e7a71aa3-1a87-4e68-a1ce-009fa20742ft",
                                    "e7a71aa3-1a87-4e68-a1ce-009fa20742fu",
                                    "e7a71aa3-1a87-4e68-a1ce-009fa20742i0"
                                }
                            |>,
                            <|"Colors" -> Except[{1} | {2}]|> -> <|
                                "Same" -> True,
                                "Examples" -> {2, 3},
                                "ExampleCount" -> 2,
                                "UseCount" -> 2,
                                "InputObjects" -> {
                                    "e7a71aa3-1a87-4e68-a1ce-009fa20742fv",
                                    "e7a71aa3-1a87-4e68-a1ce-009fa20742i1"
                                }
                            |>
                        }
                    |>
                ]
            ],
            "ExpectedOutput" -> HoldForm[
                Daniel`ARC`ARCScene[
                    {
                        {0, 0, 0, 0, 0, 0, 0, 0, 0},
                        {0, 0, 0, 0, 0, 0, 0, 0, 0},
                        {0, 4, 0, 4, 0, 0, 0, 0, 0},
                        {0, 0, 2, 0, 0, 0, 0, 0, 0},
                        {0, 4, 0, 4, 0, 0, 0, 0, 0},
                        {0, 0, 0, 0, 0, 0, 7, 0, 0},
                        {0, 0, 0, 0, 0, 7, 1, 7, 0},
                        {0, 0, 0, 0, 0, 0, 7, 0, 0},
                        {0, 0, 0, 0, 0, 0, 0, 0, 0}
                    }
                ]
            ],
            "ActualOutput" -> HoldForm[
                Daniel`ARC`ARCScene[
                    {
                        {0, 0, 0, 0, 0, 0, 0, 0, 0},
                        {0, 0, 0, 0, 0, 0, 0, 0, 0},
                        {0, 4, 0, 4, 0, 0, 0, 0, 0},
                        {0, 0, 2, 0, 0, 0, 0, 0, 0},
                        {0, 4, 0, 4, 0, 0, 0, 0, 0},
                        {0, 0, 0, 0, 0, 0, 7, 0, 0},
                        {0, 0, 0, 0, 0, 7, 1, 7, 0},
                        {0, 0, 0, 0, 0, 0, 7, 0, 0},
                        {0, 0, 0, 0, 0, 0, 0, 0, 0}
                    }
                ]
            ],
            "ExpectedMessages" -> {},
            "ActualMessages" -> {},
            "AbsoluteTimeUsed" -> _Quantity,
            "CPUTimeUsed" -> _Quantity,
            "MemoryUsed" -> _Quantity
        |>
    ]
    ,
    TestID -> "ARCTestRules-20220722-0KB564"
]