(*
    Tests for: Daniel`ARC`ARCInferLineFill
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCInferLineFill]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ARCInferLineFill[
        <|
            "Image" -> Daniel`ARC`ARCScene[
                {{1, 0, 0, 0}, {0, 2, 0, 0}, {0, 0, 1, 0}, {0, 0, 0, 2}}
            ],
            "Shape" -> <|"Name" -> "Line", "Angle" -> 135|>
        |>
    ]
    ,
    {1, 2}
    ,
    TestID -> "ARCInferLineFill-20221010-IANMUD"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCInferLineFill[
            <|
                "Image" -> Daniel`ARC`ARCScene[
                    {{1, 0, 0, 0}, {0, 2, 0, 0}, {0, 0, 3, 0}, {0, 0, 0, 1}}
                ],
                "Shape" -> <|"Name" -> "Line", "Angle" -> 135|>
            |>
        ]
    ]
    ,
    {1, 2, 3}
    ,
    TestID -> "ARCInferLineFill-20221010-PFDEEF"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCInferLineFill[
            <|
                "Image" -> Daniel`ARC`ARCScene[{{1, 1, 1, 1}}],
                "Shape" -> <|"Name" -> "Line", "Angle" -> 0|>,
                "Colors" -> {1}
            |>
        ]
    ]
    ,
    Missing["NotFound", "FillPattern"]
    ,
    TestID -> "ARCInferLineFill-20221010-LEE0SR"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCInferLineFill[
            <|
                "Image" -> Daniel`ARC`ARCScene[{{2, 2}, {8, 8}, {2, 2}, {8, 8}, {2, 2}, {8, 8}}],
                "Shape" -> <|"Name" -> "Rectangle"|>,
                "Width" -> 2,
                "Height" -> 6
            |>
        ]
    ]
    ,
    <|"Pattern" -> {{2, 2}, {8, 8}}, "Orientation" -> "Vertical"|>
    ,
    TestID -> "ARCInferLineFill-20221023-CAQ7WT"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCInferLineFill[
            <|
                "Image" -> Daniel`ARC`ARCScene[
                    {{1, 2, 1}, {2, 1, 2}, {1, 2, 1}, {2, 1, 2}, {1, 2, 1}, {2, 1, 2}}
                ],
                "Shape" -> <|"Name" -> "Rectangle"|>,
                "Width" -> 2,
                "Height" -> 6
            |>
        ]
    ]
    ,
    <|"Pattern" -> {{1, 2, 1}, {2, 1, 2}}, "Orientation" -> "Vertical"|>
    ,
    TestID -> "ARCInferLineFill-20221023-9MUU45"
]

Test[
    Daniel`ARC`ARCInferLineFill[
        <|
            "Image" -> Daniel`ARC`ARCScene[{{2, 8, 2, 8, 2, 8}, {2, 8, 2, 8, 2, 8}}],
            "Shape" -> <|"Name" -> "Rectangle"|>,
            "Width" -> 2,
            "Height" -> 6
        |>
    ]
    ,
    {{2, 2}, {8, 8}}
    ,
    TestID -> "ARCInferLineFill-20221023-FBQVW1"
]

Test[
    Daniel`ARC`ARCInferLineFill[
        <|
            "Image" -> Daniel`ARC`ARCScene[
                {{1, -1, 1}, {-1, 1, -1}, {1, -1, 1}, {-1, 1, -1}, {1, -1, 1}, {-1, 1, -1}}
            ],
            "Shape" -> <|"Name" -> "Rectangle"|>,
            "Width" -> 2,
            "Height" -> 6
        |>
    ]
    ,
    <|"Pattern" -> {{10, -1, 10}, {-1, 10, -1}}, "Orientation" -> "Vertical"|>
    ,
    TestID -> "ARCInferLineFill-20221024-LOAXTX"
]