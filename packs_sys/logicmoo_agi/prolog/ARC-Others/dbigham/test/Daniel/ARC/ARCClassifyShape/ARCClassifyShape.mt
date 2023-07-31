(*
    Tests for: Daniel`ARC`ARCClassifyShape
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCClassifyShape]
    
    Author: danielb
*)

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCClassifyShape[{{1, 1}, {1, 1}}]
    ]
    ,
    {
        <|"Name" -> "Square"|>,
        <|"Name" -> "Rectangle"|>,
        <|"Name" -> "Square", "Filled" -> True|>,
        <|"Name" -> "Rectangle", "Filled" -> True|>
    }
    ,
    TestID -> "ARCClassifyShape-20220717-FUY20Y"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCClassifyShape[{{-1, -1}, {-1, -1}}]
    ]
    ,
    {}
    ,
    TestID -> "ARCClassifyShape-20220717-1LA5BB"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCClassifyShape[{{1}}]
    ]
    ,
    {
        <|"Name" -> "Pixel"|>,
        <|"Name" -> "Line"|>,
        <|"Name" -> "Square"|>,
        <|"Name" -> "Rectangle"|>,
        <|"Name" -> "Line", "Angle" -> 0|>,
        <|"Name" -> "Line", "Angle" -> 90|>,
        <|"Name" -> "Line", "Angle" -> 135|>,
        <|"Name" -> "Line", "Angle" -> 45|>,
        <|"Name" -> "Square", "Filled" -> True|>,
        <|"Name" -> "Rectangle", "Filled" -> True|>
    }
    ,
    TestID -> "ARCClassifyShape-20220717-RFJHHZ"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCClassifyShape[{{1, -1, -1}, {1, -1, -1}, {1, 1, 1}}]
    ]
    ,
    {<|"Name" -> "L"|>}
    ,
    TestID -> "ARCClassifyShape-20220717-0R6OB7"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCClassifyShape[{{1, 1, 1}, {1, -1, -1}, {1, -1, -1}}]
    ]
    ,
    {
        <|"Name" -> "L"|>,
        <|"Name" -> "L", "Transform" -> <|"Type" -> "Rotation", "Angle" -> 90|>|>,
        <|"Name" -> "L", "Transform" -> <|"Type" -> "Flip", "Direction" -> "Vertical"|>|>
    }
    ,
    TestID -> "ARCClassifyShape-20220717-R15V60"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCClassifyShape[{{1, 1, 1}}]
    ]
    ,
    {
        <|"Name" -> "Line"|>,
        <|"Name" -> "Rectangle"|>,
        <|"Name" -> "Line", "Angle" -> 0|>,
        <|"Name" -> "Rectangle", "Filled" -> True|>
    }
    ,
    TestID -> "ARCClassifyShape-20220717-2BXB00"
]

Test[
    Daniel`ARC`ARCClassifyShape[{{-1, 1, -1}, {1, 1, 1}}]
    ,
    {<|"Name" -> "Triangle"|>}
    ,
    TestID -> "ARCClassifyShape-20220813-8IPO5B"
]

Test[
    Daniel`ARC`ARCClassifyShape[Daniel`ARC`RotateImage[{{-1, 1, -1}, {1, 1, 1}}, 90]]
    ,
    {
        <|"Name" -> "Triangle"|>,
        <|"Name" -> "Triangle", "Transform" -> <|"Type" -> "Rotation", "Angle" -> 90|>|>
    }
    ,
    TestID -> "ARCClassifyShape-20220813-CT1QS3"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCClassifyShape[
            {{1, -1, -1}, {1, -1, -1}, {1, 1, 1}},
            "IncludeImageShapes" -> True
        ]
    ]
    ,
    {
        {
            <|"Image" -> Daniel`ARC`ARCScene[{{10, -1, -1}, {10, -1, -1}, {10, 10, 10}}]|>,
            <|
                "Image" -> Daniel`ARC`ARCScene[{{10, 10, 10}, {10, -1, -1}, {10, -1, -1}}],
                "Transform" -> <|"Type" -> "Rotation", "Angle" -> 270|>
            |>,
            <|
                "Image" -> Daniel`ARC`ARCScene[{{10, 10, 10}, {-1, -1, 10}, {-1, -1, 10}}],
                "Transform" -> <|"Type" -> "Rotation", "Angle" -> 180|>
            |>,
            <|
                "Image" -> Daniel`ARC`ARCScene[{{-1, -1, 10}, {-1, -1, 10}, {10, 10, 10}}],
                "Transform" -> <|"Type" -> "Rotation", "Angle" -> 90|>
            |>,
            <|
                "Image" -> Daniel`ARC`ARCScene[{{10, 10, 10}, {10, -1, -1}, {10, -1, -1}}],
                "Transform" -> <|"Type" -> "Flip", "Direction" -> "Vertical"|>
            |>,
            <|
                "Image" -> Daniel`ARC`ARCScene[{{-1, -1, 10}, {-1, -1, 10}, {10, 10, 10}}],
                "Transform" -> <|"Type" -> "Flip", "Direction" -> "Horizontal"|>
            |>,
            <|
                "Image" -> Daniel`ARC`ARCScene[
                    {
                        {10, 10, -1, -1, -1, -1},
                        {10, 10, -1, -1, -1, -1},
                        {10, 10, -1, -1, -1, -1},
                        {10, 10, -1, -1, -1, -1},
                        {10, 10, 10, 10, 10, 10},
                        {10, 10, 10, 10, 10, 10}
                    }
                ],
                "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.5|>
            |>,
            <|
                "Image" -> Daniel`ARC`ARCScene[
                    {
                        {10, 10, 10, -1, -1, -1, -1, -1, -1},
                        {10, 10, 10, -1, -1, -1, -1, -1, -1},
                        {10, 10, 10, -1, -1, -1, -1, -1, -1},
                        {10, 10, 10, -1, -1, -1, -1, -1, -1},
                        {10, 10, 10, -1, -1, -1, -1, -1, -1},
                        {10, 10, 10, -1, -1, -1, -1, -1, -1},
                        {10, 10, 10, 10, 10, 10, 10, 10, 10},
                        {10, 10, 10, 10, 10, 10, 10, 10, 10},
                        {10, 10, 10, 10, 10, 10, 10, 10, 10}
                    }
                ],
                "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.3333333333333333|>
            |>
        },
        <|"Name" -> "L"|>
    }
    ,
    TestID -> "ARCClassifyShape-20220816-BZP8Q9"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCClassifyShape[
            {{1, 1, 1, 1}, {1, 2, 2, 1}, {1, 2, 2, 1}, {1, 1, 1, 1}}
        ]
    ]
    ,
    {
        <|"Name" -> "Square"|>,
        <|"Name" -> "Rectangle"|>,
        <|
            "Name" -> "Square",
            "Filled" -> True,
            "Interior" -> <|"Color" -> 2|>,
            "Border" -> <|"Color" -> 1|>
        |>,
        <|
            "Name" -> "Rectangle",
            "Filled" -> True,
            "Interior" -> <|"Color" -> 2|>,
            "Border" -> <|"Color" -> 1|>
        |>
    }
    ,
    TestID -> "ARCClassifyShape-20220904-D3DVF3"
]