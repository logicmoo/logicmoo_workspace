(*
    Tests for: Daniel`ARC`ARCApplyConclusion
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCApplyConclusion]
    
    Author: danielb
*)

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCApplyConclusion[
            "Position",
            <|"RelativePosition" -> {1, 1}|>,
            <|"Position" -> {1, 1}|>,
            <|"Image" -> "..."|>,
            <||>
        ]
    ]
    ,
    <|"Image" -> "...", "Position" -> {2, 2}|>
    ,
    TestID -> "ARCApplyConclusion-20220722-1Q3JKR"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCApplyConclusion["Image", "...", <|"a" -> 1|>, <|"b" -> 2|>, <||>]
    ]
    ,
    <|"b" -> 2, "Image" -> "..."|>
    ,
    TestID -> "ARCApplyConclusion-20220722-581JUT"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCApplyConclusion[
            "Transform",
            <|"Type" -> "Rotation", "Angle" -> 90|>,
            <|"Image" -> {{1, 0}, {0, 1}}|>,
            <|"Image" -> {{1, 0}, {0, 1}}|>,
            <||>
        ]
    ]
    ,
    <|"Image" -> {{0, 1}, {1, 0}}|>
    ,
    TestID -> "ARCApplyConclusion-20220722-TWJDQR"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCApplyConclusion[
            "Transform",
            <|"Type" -> "Move", "Position" -> <|"Y" -> 9|>|>,
            <|
                "UUID" -> "8b35d32a-3dde-48d7-9243-b50c19f0f138",
                "Image" -> Daniel`ARC`ARCScene[{{4, 4, 4}, {4, 4, 4}}],
                "PixelPositions" -> {{1, 8}, {1, 9}, {1, 10}, {2, 8}, {2, 9}, {2, 10}},
                "Colors" -> {4},
                "Width" -> 3,
                "Height" -> 2,
                "Position" -> {1, 8},
                "Y" -> 1,
                "X" -> 8,
                "AspectRatio" -> 3/2,
                "Area" -> 6,
                "FilledArea" -> 6
            |>,
            <||>,
            <|"Objects" -> {<|"Colors" -> {1}, "Y" -> 9|>}|>
        ]
    ]
    ,
    <|"Image" -> Daniel`ARC`ARCScene[{{4, 4, 4}, {4, 4, 4}}], "Position" -> {9, 8}|>
    ,
    TestID -> "ARCApplyConclusion-20220724-1CDE03"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Block[
            {scene},
            Daniel`ARC`ARCApplyConclusion[
                <|
                    "UUID" -> "baa4f5ca-31c6-4abe-81ac-61d6a7c8dfde",
                    "Image" -> Daniel`ARC`ARCScene[{{2}}],
                    "PixelPositions" -> {{1, 2}},
                    "Shapes" -> {
                        <|"Image" -> Daniel`ARC`ARCScene[{{2}}]|>,
                        <|"Name" -> "Pixel"|>,
                        <|"Name" -> "Square"|>,
                        <|"Name" -> "Rectangle"|>
                    },
                    "Colors" -> {2},
                    "Position" -> {1, 2},
                    "Width" -> 1,
                    "Height" -> 1,
                    "Y" -> 1,
                    "X" -> 2,
                    "AspectRatio" -> 1,
                    "Area" -> 1,
                    "FilledArea" -> 1
                |>,
                <|
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
                    "Position" -> <|"RelativePosition" -> {0, -1}|>,
                    "Examples" -> {1, 2},
                    "ExampleCount" -> 2,
                    "UseCount" -> 3
                |>,
                scene = <|
                    "Background" -> "Black",
                    "Width" -> 10,
                    "Height" -> 6,
                    "Objects" -> {
                        <|
                            "UUID" -> "baa4f5ca-31c6-4abe-81ac-61d6a7c8dfde",
                            "Image" -> Daniel`ARC`ARCScene[{{2}}],
                            "PixelPositions" -> {{1, 2}},
                            "Shapes" -> {
                                <|"Image" -> Daniel`ARC`ARCScene[{{2}}]|>,
                                <|"Name" -> "Pixel"|>,
                                <|"Name" -> "Square"|>,
                                <|"Name" -> "Rectangle"|>
                            },
                            "Colors" -> {2},
                            "Position" -> {1, 2},
                            "Width" -> 1,
                            "Height" -> 1,
                            "Y" -> 1,
                            "X" -> 2,
                            "AspectRatio" -> 1,
                            "Area" -> 1,
                            "FilledArea" -> 1
                        |>,
                        <|
                            "UUID" -> "c4cd6001-0f00-4430-99b1-8d69451861ea",
                            "Image" -> Daniel`ARC`ARCScene[{{8}}],
                            "PixelPositions" -> {{1, 6}},
                            "Shapes" -> {
                                <|"Image" -> Daniel`ARC`ARCScene[{{8}}]|>,
                                <|"Name" -> "Pixel"|>,
                                <|"Name" -> "Square"|>,
                                <|"Name" -> "Rectangle"|>
                            },
                            "Colors" -> {8},
                            "Position" -> {1, 6},
                            "Width" -> 1,
                            "Height" -> 1,
                            "Y" -> 1,
                            "X" -> 6,
                            "AspectRatio" -> 1,
                            "Area" -> 1,
                            "FilledArea" -> 1
                        |>
                    },
                    "Scene" -> Daniel`ARC`ARCScene[
                        {
                            {0, 2, 0, 0, 0, 8, 0, 0, 0, 0},
                            {0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
                            {0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
                            {0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
                            {0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
                            {0, 0, 0, 0, 0, 0, 0, 0, 0, 0}
                        }
                    ]
                |>,
                scene
            ]
        ]
    ]
    ,
    <|
        "Image" -> Daniel`ARC`ARCScene[
            {{-1, 2, -1}, {2, -1, 2}, {-1, 2, -1}, {2, -1, 2}, {-1, 2, -1}, {2, -1, 2}}
        ],
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
        "Position" -> {1, 1},
        "Colors" -> {2},
        "Width" -> 1,
        "Height" -> 1
    |>
    ,
    TestID -> "ARCApplyConclusion-20220725-7GPVUC"
]