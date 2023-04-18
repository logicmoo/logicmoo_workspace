(*
    Tests for: Daniel`ARC`ARCConstructObject
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCConstructObject]
    
    Author: danielb
*)

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCConstructObject[
            <|
                "Outward" -> True,
                "Shape" -> <|"Name" -> "Line", "Angle" -> 90|>,
                "Direction" -> {-1, 0},
                "Color" -> 2,
                "X" -> 5
            |>,
            "Parent" -> <|"Y" -> 5, "Height" -> 10|>,
            "Scene" -> <|"Width" -> 20, "Height" -> 20|>
        ]
    ]
    ,
    <|
        "Image" -> Daniel`ARC`ARCScene[{{2}, {2}, {2}, {2}}],
        "Shape" -> <|"Name" -> "Line", "Angle" -> 90|>,
        "Color" -> 2,
        "Position" -> {1, 5},
        "Width" -> 1,
        "Height" -> 4
    |>
    ,
    TestID -> "ARCConstructObject-20220821-7U6G39"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCConstructObject[
            <|
                "Outward" -> True,
                "Shape" -> <|"Name" -> "Line", "Angle" -> 90|>,
                "Direction" -> {1, 0},
                "Color" -> 2,
                "X" -> 5
            |>,
            "Parent" -> <|"Y" -> 5, "Height" -> 10|>,
            "Scene" -> <|"Width" -> 20, "Height" -> 20|>
        ]
    ]
    ,
    <|
        "Image" -> Daniel`ARC`ARCScene[{{2}, {2}, {2}, {2}, {2}, {2}}],
        "Shape" -> <|"Name" -> "Line", "Angle" -> 90|>,
        "Color" -> 2,
        "Position" -> {15, 5},
        "Width" -> 1,
        "Height" -> 6
    |>
    ,
    TestID -> "ARCConstructObject-20220821-O83D8J"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCConstructObject[
            <|
                "Outward" -> True,
                "Shape" -> <|"Name" -> "Line", "Angle" -> 90|>,
                "Direction" -> {0, -1},
                "Color" -> 2,
                "Y" -> 5
            |>,
            "Parent" -> <|"X" -> 5, "Width" -> 10|>,
            "Scene" -> <|"Width" -> 20, "Height" -> 20|>
        ]
    ]
    ,
    <|
        "Image" -> Daniel`ARC`ARCScene[{{2, 2, 2, 2}}],
        "Shape" -> <|"Name" -> "Line", "Angle" -> 90|>,
        "Color" -> 2,
        "Position" -> {5, 1},
        "Width" -> 4,
        "Height" -> 1
    |>
    ,
    TestID -> "ARCConstructObject-20220821-0D1DR9"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCConstructObject[
            <|"Shapes" -> {<|"Name" -> "L"|>}, "Color" -> 2, "Width" -> 1, "Height" -> 3|>,
            "Scene" -> <|"Width" -> 10, "Height" -> 10|>
        ]
    ]
    ,
    <|
        "Image" -> Daniel`ARC`ARCScene[{{2}, {2}, {2}}],
        "Shapes" -> {<|"Name" -> "L"|>},
        "Color" -> 2,
        "Width" -> 1,
        "Height" -> 3
    |>
    ,
    TestID -> "ARCConstructObject-20220821-MJNBRC"
]