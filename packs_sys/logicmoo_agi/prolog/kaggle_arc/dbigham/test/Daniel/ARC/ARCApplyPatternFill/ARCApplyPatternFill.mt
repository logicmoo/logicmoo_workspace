(*
    Tests for: Daniel`ARC`ARCApplyPatternFill
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCApplyPatternFill]
    
    Author: danielb
*)

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCScene[
            Daniel`ARC`ARCApplyPatternFill[
                ConstantArray[Daniel`ARC`Private`$nonImageColor, {6, 6}],
                <|
                    "Type" -> "PatternFill",
                    "Pattern" -> Daniel`ARC`ARCScene[{{6, 1, -1}, {3, -1, -1}, {-1, -1, -1}}],
                    "StartY" -> 1,
                    "StartX" -> 1,
                    "TrajectoryY" -> 1,
                    "TrajectoryX" -> 1
                |>
            ]
        ]
    ]
    ,
    Daniel`ARC`ARCScene[
        {
            {6, 1, -1, -1, -1, -1},
            {3, 6, 1, -1, -1, -1},
            {-1, 3, 6, 1, -1, -1},
            {-1, -1, 3, 6, 1, -1},
            {-1, -1, -1, 3, 6, 1},
            {-1, -1, -1, -1, 3, 6}
        }
    ]
    ,
    TestID -> "ARCApplyPatternFill-20221003-EFN638"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCScene[
            Daniel`ARC`ARCApplyPatternFill[
                ConstantArray[Daniel`ARC`Private`$nonImageColor, {10, 10}],
                <|
                    "Type" -> "PatternFill",
                    "Pattern" -> Daniel`ARC`ARCScene[{{1, -1, 7, -1, -1}}],
                    "StartY" -> 10,
                    "StartX" -> 1,
                    "TrajectoryY" -> -1,
                    "TrajectoryX" -> 1
                |>
            ]
        ]
    ]
    ,
    Daniel`ARC`ARCScene[
        {
            {-1, -1, -1, -1, -1, -1, -1, -1, -1, 1},
            {-1, -1, -1, -1, -1, -1, -1, -1, 1, -1},
            {-1, -1, -1, -1, -1, -1, -1, 1, -1, 7},
            {-1, -1, -1, -1, -1, -1, 1, -1, 7, -1},
            {-1, -1, -1, -1, -1, 1, -1, 7, -1, -1},
            {-1, -1, -1, -1, 1, -1, 7, -1, -1, -1},
            {-1, -1, -1, 1, -1, 7, -1, -1, -1, -1},
            {-1, -1, 1, -1, 7, -1, -1, -1, -1, -1},
            {-1, 1, -1, 7, -1, -1, -1, -1, -1, -1},
            {1, -1, 7, -1, -1, -1, -1, -1, -1, -1}
        }
    ]
    ,
    TestID -> "ARCApplyPatternFill-20221003-CBDEOP"
]

Test[
    Daniel`ARC`ARCScene[
        Daniel`ARC`ARCApplyPatternFill[
            ConstantArray[Daniel`ARC`Private`$nonImageColor, {9, 17}],
            <|
                "Type" -> "PatternFill",
                "Pattern" -> Daniel`ARC`ARCScene[
                    {
                        {-1, -1, 8, -1, -1, -1, 8, -1, -1, -1, 8, -1, -1, -1, 8, -1, -1},
                        {-1, 8, -1, 8, -1, 8, -1, 8, -1, 8, -1, 8, -1, 8, -1, 8, -1},
                        {8, -1, -1, -1, 8, -1, -1, -1, 8, -1, -1, -1, 8, -1, -1, -1, 8}
                    }
                ],
                "List" -> {
                    <|
                        "StartY" -> 1,
                        "StartX" -> 1,
                        "TrajectoryY" -> Activate[Inactive[Plus][Inactive[Times][3, 2], -2]],
                        "TrajectoryX" -> 0
                    |>,
                    <|
                        "StartY" -> 3,
                        "StartX" -> 1,
                        "TrajectoryY" -> Activate[Inactive[Plus][Inactive[Times][3, 2], -2]],
                        "TrajectoryX" -> 0,
                        "Transform" -> <|"Type" -> "Flip", "Direction" -> "Vertical"|>
                    |>
                }
            |>
        ]
    ]
    ,
    Daniel`ARC`ARCScene[
        {
            {-1, -1, 8, -1, -1, -1, 8, -1, -1, -1, 8, -1, -1, -1, 8, -1, -1},
            {-1, 8, -1, 8, -1, 8, -1, 8, -1, 8, -1, 8, -1, 8, -1, 8, -1},
            {8, -1, -1, -1, 8, -1, -1, -1, 8, -1, -1, -1, 8, -1, -1, -1, 8},
            {-1, 8, -1, 8, -1, 8, -1, 8, -1, 8, -1, 8, -1, 8, -1, 8, -1},
            {-1, -1, 8, -1, -1, -1, 8, -1, -1, -1, 8, -1, -1, -1, 8, -1, -1},
            {-1, 8, -1, 8, -1, 8, -1, 8, -1, 8, -1, 8, -1, 8, -1, 8, -1},
            {8, -1, -1, -1, 8, -1, -1, -1, 8, -1, -1, -1, 8, -1, -1, -1, 8},
            {-1, 8, -1, 8, -1, 8, -1, 8, -1, 8, -1, 8, -1, 8, -1, 8, -1},
            {-1, -1, 8, -1, -1, -1, 8, -1, -1, -1, 8, -1, -1, -1, 8, -1, -1}
        }
    ]
    ,
    TestID -> "ARCApplyPatternFill-20221008-RUQLH3"
]

Test[
    Daniel`ARC`ARCScene[
        Daniel`ARC`ARCApplyPatternFill[
            ConstantArray[Daniel`ARC`Private`$nonImageColor, {12, 12}],
            <|
                "Type" -> "PatternFill",
                "Pattern" -> Daniel`ARC`ARCScene[{{6, 6, -1}, {-1, 6, 6}, {-1, -1, 6}}],
                "StartY" -> 1,
                "StartX" -> 1,
                "TrajectoryY" -> 0,
                "TrajectoryX" -> 3,
                "Wrapped" -> True,
                "Count" -> 5
            |>
        ]
    ]
    ,
    Daniel`ARC`ARCScene[
        {
            {6, 6, -1, 6, 6, -1, 6, 6, -1, 6, 6, -1},
            {-1, 6, 6, -1, 6, 6, -1, 6, 6, -1, 6, 6},
            {-1, -1, 6, -1, -1, 6, -1, -1, 6, -1, -1, 6},
            {6, 6, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
            {-1, 6, 6, -1, -1, -1, -1, -1, -1, -1, -1, -1},
            {-1, -1, 6, -1, -1, -1, -1, -1, -1, -1, -1, -1},
            {-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
            {-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
            {-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
            {-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
            {-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
            {-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}
        }
    ]
    ,
    TestID -> "ARCApplyPatternFill-20221008-VIWJAO"
]

Test[
    Daniel`ARC`ARCScene[
        Daniel`ARC`ARCApplyPatternFill[
            ConstantArray[Daniel`ARC`Private`$nonImageColor, {12, 12}],
            <|
                "Type" -> "PatternFill",
                "Pattern" -> Daniel`ARC`ARCScene[{{6, 6, -1}, {-1, 6, 6}, {-1, -1, 6}}],
                "StartY" -> 1,
                "StartX" -> 1,
                "TrajectoryY" -> 0,
                "TrajectoryX" -> 3,
                "Count" -> 2
            |>
        ]
    ]
    ,
    Daniel`ARC`ARCScene[
        {
            {6, 6, -1, 6, 6, -1, -1, -1, -1, -1, -1, -1},
            {-1, 6, 6, -1, 6, 6, -1, -1, -1, -1, -1, -1},
            {-1, -1, 6, -1, -1, 6, -1, -1, -1, -1, -1, -1},
            {-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
            {-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
            {-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
            {-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
            {-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
            {-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
            {-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
            {-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
            {-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}
        }
    ]
    ,
    TestID -> "ARCApplyPatternFill-20221008-1QFCUX"
]