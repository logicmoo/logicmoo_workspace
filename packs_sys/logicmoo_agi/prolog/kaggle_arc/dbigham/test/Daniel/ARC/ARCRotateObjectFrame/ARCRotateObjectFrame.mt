(*
    Tests for: Daniel`ARC`ARCRotateObjectFrame
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCRotateObjectFrame]
    
    Author: danielb
*)

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCRotateObjectFrame[
            <|
                "Image" -> Daniel`ARC`ARCScene[{{1, 1, 1}}],
                "Shape" -> <|"Name" -> "Line"|>,
                "Shapes" -> {<|"Name" -> "Line"|>},
                "Position" -> {2, 3},
                "Y" -> 2,
                "X" -> 3,
                "Y2" -> 2,
                "X2" -> 5,
                "Width" -> 3,
                "Height" -> 1
            |>,
            -90,
            10,
            10,
            10,
            10
        ]
    ]
    ,
    <|
        "Image" -> Daniel`ARC`ARCScene[{{1}, {1}, {1}}],
        "Shape" -> <|"Name" -> "Line", "Angle" -> 90|>,
        "Shapes" -> {
            <|"Image" -> Daniel`ARC`ARCScene[{{10}, {10}, {10}}]|>,
            <|
                "Image" -> Daniel`ARC`ARCScene[{{10, 10, 10}}],
                "Transform" -> <|"Type" -> "Rotation", "Angle" -> 270|>
            |>,
            <|
                "Image" -> Daniel`ARC`ARCScene[{{10, 10, 10}}],
                "Transform" -> <|"Type" -> "Rotation", "Angle" -> 90|>
            |>,
            <|
                "Image" -> Daniel`ARC`ARCScene[
                    {{10, 10}, {10, 10}, {10, 10}, {10, 10}, {10, 10}, {10, 10}}
                ],
                "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.5|>
            |>,
            <|
                "Image" -> Daniel`ARC`ARCScene[
                    {
                        {10, 10, 10},
                        {10, 10, 10},
                        {10, 10, 10},
                        {10, 10, 10},
                        {10, 10, 10},
                        {10, 10, 10},
                        {10, 10, 10},
                        {10, 10, 10},
                        {10, 10, 10}
                    }
                ],
                "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.3333333333333333|>
            |>,
            <|"Name" -> "Line"|>,
            <|"Name" -> "Rectangle"|>,
            <|"Name" -> "Line", "Angle" -> 90|>,
            <|"Name" -> "Rectangle", "Filled" -> True|>
        },
        "Position" -> {6, 2},
        "Y" -> 6,
        "X" -> 2,
        "Y2" -> 8,
        "X2" -> 2,
        "Width" -> 1,
        "Height" -> 3
    |>
    ,
    TestID -> "ARCRotateObjectFrame-20220816-RSXRIU"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCRotateObjectFrame[
            <|
                "Image" -> Daniel`ARC`ARCScene[{{1, 1, 1}}],
                "Shapes" -> {<|"Name" -> "Line"|>},
                "Position" -> {2, 3},
                "Y" -> 2,
                "X" -> 3,
                "Y2" -> 2,
                "X2" -> 5,
                "Width" -> 3,
                "Height" -> 1
            |>,
            -180,
            10,
            10,
            10,
            10
        ]
    ]
    ,
    <|
        "Image" -> Daniel`ARC`ARCScene[{{1, 1, 1}}],
        "Shapes" -> {
            <|"Image" -> Daniel`ARC`ARCScene[{{10, 10, 10}}]|>,
            <|
                "Image" -> Daniel`ARC`ARCScene[{{10}, {10}, {10}}],
                "Transform" -> <|"Type" -> "Rotation", "Angle" -> 270|>
            |>,
            <|
                "Image" -> Daniel`ARC`ARCScene[{{10}, {10}, {10}}],
                "Transform" -> <|"Type" -> "Rotation", "Angle" -> 90|>
            |>,
            <|
                "Image" -> Daniel`ARC`ARCScene[
                    {{10, 10, 10, 10, 10, 10}, {10, 10, 10, 10, 10, 10}}
                ],
                "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.5|>
            |>,
            <|
                "Image" -> Daniel`ARC`ARCScene[
                    {
                        {10, 10, 10, 10, 10, 10, 10, 10, 10},
                        {10, 10, 10, 10, 10, 10, 10, 10, 10},
                        {10, 10, 10, 10, 10, 10, 10, 10, 10}
                    }
                ],
                "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.3333333333333333|>
            |>,
            <|"Name" -> "Line"|>,
            <|"Name" -> "Rectangle"|>,
            <|"Name" -> "Line", "Angle" -> 0|>,
            <|"Name" -> "Rectangle", "Filled" -> True|>
        },
        "Position" -> {9, 6},
        "Y" -> 9,
        "X" -> 6,
        "Y2" -> 9,
        "X2" -> 8,
        "Width" -> 3,
        "Height" -> 1
    |>
    ,
    TestID -> "ARCRotateObjectFrame-20220816-5ELAO6"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCRotateObjectFrame[
            <|
                "Image" -> Daniel`ARC`ARCScene[{{1, 1, 1}}],
                "Shapes" -> {<|"Name" -> "Line"|>},
                "Position" -> {2, 3},
                "Y" -> 2,
                "X" -> 3,
                "Y2" -> 2,
                "X2" -> 5,
                "Width" -> 3,
                "Height" -> 1
            |>,
            -270,
            10,
            10,
            10,
            10
        ]
    ]
    ,
    <|
        "Image" -> Daniel`ARC`ARCScene[{{1}, {1}, {1}}],
        "Shapes" -> {
            <|"Image" -> Daniel`ARC`ARCScene[{{10}, {10}, {10}}]|>,
            <|
                "Image" -> Daniel`ARC`ARCScene[{{10, 10, 10}}],
                "Transform" -> <|"Type" -> "Rotation", "Angle" -> 270|>
            |>,
            <|
                "Image" -> Daniel`ARC`ARCScene[{{10, 10, 10}}],
                "Transform" -> <|"Type" -> "Rotation", "Angle" -> 90|>
            |>,
            <|
                "Image" -> Daniel`ARC`ARCScene[
                    {{10, 10}, {10, 10}, {10, 10}, {10, 10}, {10, 10}, {10, 10}}
                ],
                "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.5|>
            |>,
            <|
                "Image" -> Daniel`ARC`ARCScene[
                    {
                        {10, 10, 10},
                        {10, 10, 10},
                        {10, 10, 10},
                        {10, 10, 10},
                        {10, 10, 10},
                        {10, 10, 10},
                        {10, 10, 10},
                        {10, 10, 10},
                        {10, 10, 10}
                    }
                ],
                "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.3333333333333333|>
            |>,
            <|"Name" -> "Line"|>,
            <|"Name" -> "Rectangle"|>,
            <|"Name" -> "Line", "Angle" -> 90|>,
            <|"Name" -> "Rectangle", "Filled" -> True|>
        },
        "Position" -> {3, 9},
        "Y" -> 3,
        "X" -> 9,
        "Y2" -> 5,
        "X2" -> 9,
        "Width" -> 1,
        "Height" -> 3
    |>
    ,
    TestID -> "ARCRotateObjectFrame-20220816-765IWD"
]

Test[
    Daniel`ARC`ARCRotateObjectFrame[
        <|
            "Image" -> Daniel`ARC`ARCScene[{{1}}],
            "Position" -> <|
                "RelativePosition" -> <|"Y" -> 2, "X" -> 3, "YInverse" -> 1, "XInverse" -> 1|>
            |>,
            "Y" -> 3,
            "X" -> 5,
            "Width" -> 1,
            "Height" -> 1
        |>,
        -90,
        3,
        2,
        10,
        10
    ]
    ,
    <|
        "Image" -> Daniel`ARC`ARCScene[{{1}}],
        "Position" -> <|
            "RelativePosition" -> <|"Y" -> 1, "X" -> 2, "YInverse" -> 3, "XInverse" -> 1|>
        |>,
        "Y" -> 6,
        "X" -> 3,
        "Width" -> 1,
        "Height" -> 1
    |>
    ,
    TestID -> "ARCRotateObjectFrame-20220816-H5J8TM"
]

Test[
    Daniel`ARC`ARCRotateObjectFrame[
        <|
            "Image" -> Daniel`ARC`ARCScene[{{1}}],
            "Position" -> <|
                "RelativePosition" -> <|"Y" -> 2, "X" -> 3, "YInverse" -> 1, "XInverse" -> 1|>
            |>,
            "Y" -> 3,
            "X" -> 5,
            "Width" -> 1,
            "Height" -> 1
        |>,
        -180,
        3,
        2,
        10,
        10
    ]
    ,
    <|
        "Image" -> Daniel`ARC`ARCScene[{{1}}],
        "Position" -> <|
            "RelativePosition" -> <|"Y" -> 1, "X" -> 1, "YInverse" -> 2, "XInverse" -> 3|>
        |>,
        "Y" -> 8,
        "X" -> 6,
        "Width" -> 1,
        "Height" -> 1
    |>
    ,
    TestID -> "ARCRotateObjectFrame-20220816-FRYHA1"
]

Test[
    Daniel`ARC`ARCRotateObjectFrame[
        <|
            "Image" -> Daniel`ARC`ARCScene[{{1}}],
            "Position" -> <|
                "RelativePosition" -> <|"Y" -> 2, "X" -> 3, "YInverse" -> 1, "XInverse" -> 1|>
            |>,
            "Y" -> 3,
            "X" -> 5,
            "Width" -> 1,
            "Height" -> 1
        |>,
        -270,
        3,
        2,
        10,
        10
    ]
    ,
    <|
        "Image" -> Daniel`ARC`ARCScene[{{1}}],
        "Position" -> <|
            "RelativePosition" -> <|"Y" -> 3, "X" -> 1, "YInverse" -> 1, "XInverse" -> 2|>
        |>,
        "Y" -> 5,
        "X" -> 8,
        "Width" -> 1,
        "Height" -> 1
    |>
    ,
    TestID -> "ARCRotateObjectFrame-20220816-3HGV3V"
]

Test[
    Daniel`ARC`ARCRotateObjectFrame[
        <|
            "Position" -> {3, 3},
            "Width" -> 3,
            "Height" -> 1,
            "Outward" -> True,
            "Direction" -> {0, 1}
        |>,
        -90,
        5,
        5,
        5,
        5
    ]
    ,
    <|
        "Position" -> {1, 3},
        "Width" -> 1,
        "Height" -> 3,
        "Outward" -> True,
        "Direction" -> {-1, 0}
    |>
    ,
    TestID -> "ARCRotateObjectFrame-20220816-FT1UZE"
]

Test[
    Daniel`ARC`ARCRotateObjectFrame[
        <|
            "Position" -> {3, 3},
            "Width" -> 3,
            "Height" -> 1,
            "Outward" -> True,
            "Direction" -> {0, 1}
        |>,
        -180,
        5,
        5,
        5,
        5
    ]
    ,
    <|
        "Position" -> {3, 1},
        "Width" -> 3,
        "Height" -> 1,
        "Outward" -> True,
        "Direction" -> {0, -1}
    |>
    ,
    TestID -> "ARCRotateObjectFrame-20220816-1V9WP7"
]

Test[
    Daniel`ARC`ARCRotateObjectFrame[
        <|
            "Position" -> {3, 3},
            "Width" -> 3,
            "Height" -> 1,
            "Outward" -> True,
            "Direction" -> {0, 1}
        |>,
        -270,
        5,
        5,
        5,
        5
    ]
    ,
    <|
        "Position" -> {3, 3},
        "Width" -> 1,
        "Height" -> 3,
        "Outward" -> True,
        "Direction" -> {1, 0}
    |>
    ,
    TestID -> "ARCRotateObjectFrame-20220816-G19XT4"
]