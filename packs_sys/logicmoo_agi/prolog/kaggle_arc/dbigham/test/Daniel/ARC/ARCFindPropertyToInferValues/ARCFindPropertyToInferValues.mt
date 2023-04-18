(*
    Tests for: Daniel`ARC`ARCFindPropertyToInferValues
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCFindPropertyToInferValues]
    
    Author: danielb
*)

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCFindPropertyToInferValues[
            {"Y"},
            {
                <|
                    "UUID" -> "07168399-d36c-489c-8e57-23d9422eddae",
                    "Image" -> Daniel`ARC`ARCScene[{{1, 1}, {1, 1}}],
                    "PixelPositions" -> {{2, 8}, {2, 9}, {3, 8}, {3, 9}},
                    "Colors" -> {1},
                    "Width" -> 2,
                    "Height" -> 2,
                    "Position" -> {2, 8},
                    "Y" -> 2,
                    "X" -> 8,
                    "AspectRatio" -> 1,
                    "Area" -> 4,
                    "FilledArea" -> 4
                |>,
                <|
                    "UUID" -> "2e58011a-8ea1-4bf7-a0b9-32d5c8f701fa",
                    "Image" -> Daniel`ARC`ARCScene[{{1, 1, 1}, {1, 1, 1}}],
                    "PixelPositions" -> {{6, 5}, {6, 6}, {6, 7}, {7, 5}, {7, 6}, {7, 7}},
                    "Colors" -> {1},
                    "Width" -> 3,
                    "Height" -> 2,
                    "Position" -> {6, 5},
                    "Y" -> 6,
                    "X" -> 5,
                    "AspectRatio" -> 3/2,
                    "Area" -> 6,
                    "FilledArea" -> 6
                |>,
                <|
                    "UUID" -> "caa2dd3e-f071-45eb-94f2-919fd2dad742",
                    "Image" -> Daniel`ARC`ARCScene[{{1}, {1}}],
                    "PixelPositions" -> {{3, 2}, {4, 2}},
                    "Colors" -> {1},
                    "Width" -> 1,
                    "Height" -> 2,
                    "Position" -> {3, 2},
                    "Y" -> 3,
                    "X" -> 2,
                    "AspectRatio" -> 1/2,
                    "Area" -> 2,
                    "FilledArea" -> 2
                |>
            },
            {2, 6, 3}
        ]
    ]
    ,
    {"Y"}
    ,
    TestID -> "ARCFindPropertyToInferValues-20220724-U9ITMB"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCFindPropertyToInferValues[
            {"Y"},
            {
                <|
                    "Image" -> Daniel`ARC`ARCScene[{{1, 1}, {1, 1}}],
                    "PixelPositions" -> {{2, 8}, {2, 9}, {3, 8}, {3, 9}},
                    "Colors" -> {1},
                    "Width" -> 2,
                    "Height" -> 2,
                    "Position" -> {2, 8},
                    "Y" -> 1,
                    "X" -> 8,
                    "AspectRatio" -> 1,
                    "Area" -> 4,
                    "FilledArea" -> 4
                |>,
                <|
                    "Image" -> Daniel`ARC`ARCScene[{{1, 1, 1}, {1, 1, 1}}],
                    "PixelPositions" -> {{6, 5}, {6, 6}, {6, 7}, {7, 5}, {7, 6}, {7, 7}},
                    "Colors" -> {1},
                    "Width" -> 3,
                    "Height" -> 2,
                    "Position" -> {6, 5},
                    "Y" -> 1,
                    "X" -> 5,
                    "AspectRatio" -> 3/2,
                    "Area" -> 6,
                    "FilledArea" -> 6
                |>,
                <|
                    "Image" -> Daniel`ARC`ARCScene[{{1}, {1}}],
                    "PixelPositions" -> {{3, 2}, {4, 2}},
                    "Colors" -> {1},
                    "Width" -> 1,
                    "Height" -> 2,
                    "Position" -> {3, 2},
                    "Y" -> 2,
                    "X" -> 2,
                    "AspectRatio" -> 1/2,
                    "Area" -> 2,
                    "FilledArea" -> 2
                |>,
                <|
                    "Image" -> Daniel`ARC`ARCScene[{{1}, {1}}],
                    "PixelPositions" -> {{3, 2}, {4, 2}},
                    "Colors" -> {1},
                    "Width" -> 1,
                    "Height" -> 2,
                    "Position" -> {3, 2},
                    "Y" -> 2,
                    "X" -> 2,
                    "AspectRatio" -> 1/2,
                    "Area" -> 2,
                    "FilledArea" -> 2
                |>
            },
            {True, True, False, False}
        ]
    ]
    ,
    <|"Y" -> 1|>
    ,
    TestID -> "ARCFindPropertyToInferValues-20220911-SRFP0H"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCFindPropertyToInferValues[
            {"Y"},
            {<|"Y" -> 1|>, <|"Y" -> 1|>, <|"Y" -> 2|>, <|"Y" -> 2|>},
            {1., 1., 2., 2.}
        ]
    ]
    ,
    {"Y"}
    ,
    TestID -> "ARCFindPropertyToInferValues-20220917-H9TKY7"
]