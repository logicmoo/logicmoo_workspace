(*
    Tests for: Daniel`ARC`ARCInferObjectImage
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCInferObjectImage]
    
    Author: danielb
*)

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCInferObjectImage[
            <|
                "Shapes" -> {<|"Name" -> "Square", "Filled" -> True|>},
                "Color" -> 2,
                "Width" -> 3,
                "Height" -> 3
            |>,
            <||>
        ]
    ]
    ,
    Daniel`ARC`ARCScene[{{2, 2, 2}, {2, 2, 2}, {2, 2, 2}}]
    ,
    TestID -> "ARCInferObjectImage-20220810-1UPZXG"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCInferObjectImage[
            <|
                "Shapes" -> {<|"Name" -> "Square", "Filled" -> False|>},
                "Color" -> 2,
                "Width" -> 3,
                "Height" -> 3
            |>,
            <||>
        ]
    ]
    ,
    Daniel`ARC`ARCScene[{{2, 2, 2}, {2, -1, 2}, {2, 2, 2}}]
    ,
    TestID -> "ARCInferObjectImage-20220810-QDW3F4"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCInferObjectImage[
            <|
                "Shapes" -> {<|"Name" -> "Square"|>},
                "Color" -> 2,
                "Width" -> 3,
                "Height" -> 3
            |>,
            <||>
        ]
    ]
    ,
    Daniel`ARC`ARCScene[{{2, 2, 2}, {2, -1, 2}, {2, 2, 2}}]
    ,
    TestID -> "ARCInferObjectImage-20220810-Y4ZFW8"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCInferObjectImage[
            <|
                "Shapes" -> {<|"Name" -> "Pixel"|>},
                "Color" -> 2,
                "Width" -> 1,
                "Height" -> 1
            |>,
            <||>
        ]
    ]
    ,
    Daniel`ARC`ARCScene[{{2}}]
    ,
    TestID -> "ARCInferObjectImage-20220810-JRVKGT"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCInferObjectImage[
            <|
                "Shapes" -> {<|"Name" -> "Rectangle"|>},
                "Color" -> 2,
                "Width" -> 5,
                "Height" -> 3
            |>,
            <||>
        ]
    ]
    ,
    Daniel`ARC`ARCScene[{{2, 2, 2, 2, 2}, {2, -1, -1, -1, 2}, {2, 2, 2, 2, 2}}]
    ,
    TestID -> "ARCInferObjectImage-20220811-URGH02"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCInferObjectImage[
            <|"Shapes" -> {<|"Name" -> "L"|>}, "Color" -> 2, "Width" -> 3, "Height" -> 3|>,
            <||>
        ]
    ]
    ,
    Daniel`ARC`ARCScene[{{2, -1, -1}, {2, -1, -1}, {2, 2, 2}}]
    ,
    TestID -> "ARCInferObjectImage-20220811-GLZ3XL"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCInferObjectImage[
            <|
                "Shapes" -> {
                    <|
                        "Name" -> "L",
                        "Transform" -> <|"Type" -> "Rotation", "Angle" -> 90|>
                    |>
                },
                "Color" -> 2,
                "Width" -> 3,
                "Height" -> 3
            |>,
            <||>
        ]
    ]
    ,
    Daniel`ARC`ARCScene[{{2, 2, 2}, {2, -1, -1}, {2, -1, -1}}]
    ,
    TestID -> "ARCInferObjectImage-20220811-45W1N3"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCInferObjectImage[
            <|
                "Shapes" -> {<|"Name" -> "Line"|>},
                "Color" -> 2,
                "Width" -> 1,
                "Height" -> 3
            |>,
            <||>
        ]
    ]
    ,
    Daniel`ARC`ARCScene[{{2}, {2}, {2}}]
    ,
    TestID -> "ARCInferObjectImage-20220812-RO2AJV"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCInferObjectImage[
            <|
                "Shapes" -> {<|"Name" -> "Line"|>},
                "Color" -> 2,
                "Width" -> 3,
                "Height" -> 1
            |>,
            <||>
        ]
    ]
    ,
    Daniel`ARC`ARCScene[{{2, 2, 2}}]
    ,
    TestID -> "ARCInferObjectImage-20220812-EY6ET2"
]

Test[
    Daniel`ARC`ARCInferObjectImage[
        <|
            "Shapes" -> {<|"Name" -> "Rectangle", "Filled" -> True|>},
            "Color" -> 2,
            "Width" -> 5,
            "Height" -> 3
        |>,
        <||>
    ]
    ,
    Daniel`ARC`ARCScene[{{2, 2, 2, 2, 2}, {2, 2, 2, 2, 2}, {2, 2, 2, 2, 2}}]
    ,
    TestID -> "ARCInferObjectImage-20220904-VCU0OW"
]

Test[
    Daniel`ARC`ARCInferObjectImage[
        <|
            "Shape" -> <|
                "Name" -> "Rectangle",
                "Filled" -> True,
                "Interior" -> <|"Color" -> 1|>,
                "Border" -> <|"Color" -> 4|>
            |>,
            "Width" -> 6,
            "Height" -> 4
        |>,
        <||>
    ]
    ,
    Daniel`ARC`ARCScene[
        {{4, 4, 4, 4, 4, 4}, {4, 1, 1, 1, 1, 4}, {4, 1, 1, 1, 1, 4}, {4, 4, 4, 4, 4, 4}}
    ]
    ,
    TestID -> "ARCInferObjectImage-20220904-RENOKC"
]

Test[
    Daniel`ARC`ARCInferObjectImage[
        <|
            "Shapes" -> {
                <|"Name" -> "L", "Transform" -> <|"Type" -> "Rotation", "Angle" -> 90|>|>
            },
            "Color" -> 2,
            "Width" -> 3,
            "Height" -> 4
        |>,
        <||>
    ]
    ,
    Daniel`ARC`ARCScene[{{2, 2, 2}, {2, -1, -1}, {2, -1, -1}, {2, -1, -1}}]
    ,
    TestID -> "ARCInferObjectImage-20220924-EKCAFL"
]

Test[
    Daniel`ARC`ARCInferObjectImage[
        <|
            "Shapes" -> {<|"Name" -> "Triangle"|>},
            "Color" -> 2,
            "Width" -> 3,
            "Height" -> 2
        |>,
        <||>
    ]
    ,
    Daniel`ARC`ARCScene[{{-1, 2, -1}, {2, 2, 2}}]
    ,
    TestID -> "ARCInferObjectImage-20220924-HTUWXF"
]

Test[
    Daniel`ARC`ARCInferObjectImage[
        <|
            "Shapes" -> {<|"Name" -> "Triangle"|>},
            "Color" -> 2,
            "Width" -> 5,
            "Height" -> 3
        |>,
        <||>
    ]
    ,
    Daniel`ARC`ARCScene[{{-1, -1, 2, -1, -1}, {-1, 2, 2, 2, -1}, {2, 2, 2, 2, 2}}]
    ,
    TestID -> "ARCInferObjectImage-20220924-BYL47K"
]

Test[
    Daniel`ARC`ARCInferObjectImage[
        <|
            "Shapes" -> {
                <|
                    "Name" -> "Triangle",
                    "Transform" -> <|"Type" -> "Rotation", <|"Angle" -> 90|>|>
                |>
            },
            "Color" -> 2,
            "Width" -> 3,
            "Height" -> 5
        |>,
        <||>
    ]
    ,
    Daniel`ARC`ARCScene[{{2, -1, -1}, {2, 2, -1}, {2, 2, 2}, {2, 2, -1}, {2, -1, -1}}]
    ,
    TestID -> "ARCInferObjectImage-20220924-VHAX85"
]

Test[
    Daniel`ARC`ARCInferObjectImage[
        <|
            "Shapes" -> {
                <|
                    "Name" -> "Triangle",
                    "Transform" -> <|"Type" -> "Rotation", <|"Angle" -> -90|>|>
                |>
            },
            "Color" -> 2,
            "Width" -> 3,
            "Height" -> 5
        |>,
        <||>
    ]
    ,
    Daniel`ARC`ARCScene[{{-1, -1, 2}, {-1, 2, 2}, {2, 2, 2}, {-1, 2, 2}, {-1, -1, 2}}]
    ,
    TestID -> "ARCInferObjectImage-20220924-DQ9YLS"
]

Test[
    Daniel`ARC`ARCInferObjectImage[
        <|
            "Shape" -> <|"Name" -> "Line", "Angle" -> 135, "Fill" -> {1, 2, 3}|>,
            "Width" -> 3,
            "Height" -> 3
        |>,
        <||>
    ]
    ,
    Daniel`ARC`ARCScene[{{1, -1, -1}, {-1, 2, -1}, {-1, -1, 3}}]
    ,
    TestID -> "ARCInferObjectImage-20221010-42PBX6"
]

Test[
    Daniel`ARC`ARCInferObjectImage[
        <|
            "Shape" -> <|"Name" -> "Line", "Angle" -> 45, "Fill" -> {1, 2, 3}|>,
            "Width" -> 3,
            "Height" -> 3
        |>,
        <||>
    ]
    ,
    Daniel`ARC`ARCScene[{{-1, -1, 3}, {-1, 2, -1}, {1, -1, -1}}]
    ,
    TestID -> "ARCInferObjectImage-20221010-VLP7EN"
]

Test[
    Daniel`ARC`ARCInferObjectImage[
        <|
            "Shape" -> <|"Name" -> "Line", "Angle" -> 0, "Fill" -> {1, 2, 3}|>,
            "Width" -> 3,
            "Height" -> 1
        |>,
        <||>
    ]
    ,
    Daniel`ARC`ARCScene[{{1, 2, 3}}]
    ,
    TestID -> "ARCInferObjectImage-20221010-39OX58"
]

Test[
    Daniel`ARC`ARCInferObjectImage[
        <|
            "Shape" -> <|
                "Name" -> "Rectangle",
                "Filled" -> True,
                "Fill" -> <|"Pattern" -> {{2, 2}, {8, 8}}, "Orientation" -> "Vertical"|>
            |>,
            "Width" -> 2,
            "Height" -> 6
        |>,
        <||>
    ]
    ,
    Daniel`ARC`ARCScene[{{2, 2}, {8, 8}, {2, 2}, {8, 8}, {2, 2}, {8, 8}}]
    ,
    TestID -> "ARCInferObjectImage-20221023-N80B9K"
]