(*
    Tests for: Daniel`ARC`ARCCombineDividersIntoGrid
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCCombineDividersIntoGrid]
    
    Author: danielb
*)

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`SimplifyObjects["ExtraKeys" -> {"Components", "GridOrDivider"}][
                Daniel`ARC`ARCCombineDividersIntoGrid[
                    Daniel`ARC`ARCParseFile["8e5a5113"]["Train", 1]["Input"],
                    {
                        <|
                            "Image" -> Daniel`ARC`ARCScene[{{5}, {5}, {5}}],
                            "Color" -> 5,
                            "Y" -> 1,
                            "X" -> 4,
                            "GridOrDivider" -> <|
                                "Type" -> "Divider",
                                "Orientation" -> "Vertical"
                            |>,
                            "PixelPositions" -> {{1, 4}, {2, 4}, {3, 4}},
                            "Width" -> 1,
                            "Height" -> 3
                        |>,
                        <|
                            "Image" -> Daniel`ARC`ARCScene[{{5}, {5}, {5}}],
                            "Color" -> 5,
                            "Y" -> 1,
                            "X" -> 8,
                            "GridOrDivider" -> <|
                                "Type" -> "Divider",
                                "Orientation" -> "Vertical"
                            |>,
                            "PixelPositions" -> {{1, 8}, {2, 8}, {3, 8}},
                            "Width" -> 1,
                            "Height" -> 3
                        |>
                    }
                ]
            ]
        ]
    ]
    ,
    {
        <|
            "Image" -> Daniel`ARC`ARCScene[
                {
                    {-1, -1, -1, 5, -1, -1, -1, 5, -1, -1, -1},
                    {-1, -1, -1, 5, -1, -1, -1, 5, -1, -1, -1},
                    {-1, -1, -1, 5, -1, -1, -1, 5, -1, -1, -1}
                }
            ],
            "Position" -> {1, 1},
            "Components" -> {
                <|
                    "Image" -> Daniel`ARC`ARCScene[{{5}, {5}, {5}}],
                    "GridOrDivider" -> <|"Type" -> "Divider", "Orientation" -> "Vertical"|>
                |>,
                <|
                    "Image" -> Daniel`ARC`ARCScene[{{5}, {5}, {5}}],
                    "GridOrDivider" -> <|"Type" -> "Divider", "Orientation" -> "Vertical"|>
                |>
            },
            "GridOrDivider" -> <|
                "Type" -> "Grid",
                "RowCount" -> 1,
                "ColumnCount" -> 3,
                "Color" -> 5,
                "Cells" -> {
                    {
                        <|"Y" -> 1, "X" -> 1, "Width" -> 3, "Height" -> 3|>,
                        <|"Y" -> 1, "X" -> 5, "Width" -> 3, "Height" -> 3|>,
                        <|"Y" -> 1, "X" -> 9, "Width" -> 3, "Height" -> 3|>
                    }
                }
            |>
        |>
    }
    ,
    TestID -> "ARCCombineDividersIntoGrid-20220910-B7HY1Y"
]