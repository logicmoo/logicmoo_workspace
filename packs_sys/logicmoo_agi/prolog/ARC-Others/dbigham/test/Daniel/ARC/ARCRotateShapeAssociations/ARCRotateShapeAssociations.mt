(*
    Tests for: Daniel`ARC`ARCRotateShapeAssociations
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCRotateShapeAssociations]
    
    Author: danielb
*)

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Module[
                {},
                object = Daniel`ARC`ARCImageRegionToObject[
                    <|
                        "Color" -> 2,
                        "Position" -> {1, 1},
                        "Image" -> {{2, 2, 2}, {2, -1, -1}, {2, -1, -1}},
                        "PixelPositions" -> {{1, 1}, {1, 2}, {1, 3}, {2, 1}, {3, 1}}
                    |>,
                    10,
                    10
                ];
                <|
                    "Image" -> object["Image"],
                    "Shapes" -> object["Shapes"],
                    "ImageRotatedBy90" -> Daniel`ARC`RotateImage[object["Image"], 90],
                    "ShapesRotatedBy90" -> Daniel`ARC`ARCRotateShapeAssociations[
                        object["Shapes"],
                        90,
                        object["Image"]
                    ]
                |>
            ]
        ]
    ]
    ,
    <|
        "Image" -> Daniel`ARC`ARCScene[{{2, 2, 2}, {2, -1, -1}, {2, -1, -1}}],
        "Shapes" -> {
            <|"Image" -> Daniel`ARC`ARCScene[{{10, 10, 10}, {10, -1, -1}, {10, -1, -1}}]|>,
            <|
                "Image" -> Daniel`ARC`ARCScene[{{10, 10, 10}, {-1, -1, 10}, {-1, -1, 10}}],
                "Transform" -> <|"Type" -> "Rotation", "Angle" -> 270|>
            |>,
            <|
                "Image" -> Daniel`ARC`ARCScene[{{-1, -1, 10}, {-1, -1, 10}, {10, 10, 10}}],
                "Transform" -> <|"Type" -> "Rotation", "Angle" -> 180|>
            |>,
            <|
                "Image" -> Daniel`ARC`ARCScene[{{10, -1, -1}, {10, -1, -1}, {10, 10, 10}}],
                "Transform" -> <|"Type" -> "Rotation", "Angle" -> 90|>
            |>,
            <|
                "Image" -> Daniel`ARC`ARCScene[{{10, -1, -1}, {10, -1, -1}, {10, 10, 10}}],
                "Transform" -> <|"Type" -> "Flip", "Direction" -> "Vertical"|>
            |>,
            <|
                "Image" -> Daniel`ARC`ARCScene[{{10, 10, 10}, {-1, -1, 10}, {-1, -1, 10}}],
                "Transform" -> <|"Type" -> "Flip", "Direction" -> "Horizontal"|>
            |>,
            <|
                "Image" -> Daniel`ARC`ARCScene[
                    {
                        {10, 10, 10, 10, 10, 10},
                        {10, 10, 10, 10, 10, 10},
                        {10, 10, -1, -1, -1, -1},
                        {10, 10, -1, -1, -1, -1},
                        {10, 10, -1, -1, -1, -1},
                        {10, 10, -1, -1, -1, -1}
                    }
                ],
                "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.5|>
            |>,
            <|
                "Image" -> Daniel`ARC`ARCScene[
                    {
                        {10, 10, 10, 10, 10, 10, 10, 10, 10},
                        {10, 10, 10, 10, 10, 10, 10, 10, 10},
                        {10, 10, 10, 10, 10, 10, 10, 10, 10},
                        {10, 10, 10, -1, -1, -1, -1, -1, -1},
                        {10, 10, 10, -1, -1, -1, -1, -1, -1},
                        {10, 10, 10, -1, -1, -1, -1, -1, -1},
                        {10, 10, 10, -1, -1, -1, -1, -1, -1},
                        {10, 10, 10, -1, -1, -1, -1, -1, -1},
                        {10, 10, 10, -1, -1, -1, -1, -1, -1}
                    }
                ],
                "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.3333333333333333|>
            |>,
            <|"Name" -> "L"|>,
            <|"Name" -> "L", "Transform" -> <|"Type" -> "Rotation", "Angle" -> 90|>|>,
            <|
                "Name" -> "L",
                "Transform" -> <|"Type" -> "Flip", "Direction" -> "Vertical"|>
            |>
        },
        "ImageRotatedBy90" -> Daniel`ARC`ARCScene[{{2, 2, 2}, {-1, -1, 2}, {-1, -1, 2}}],
        "ShapesRotatedBy90" -> <|
            "Shape" -> <|
                "Name" -> "L",
                "Transform" -> <|"Type" -> "Rotation", "Angle" -> 180|>
            |>,
            "Shapes" -> {
                <|
                    "Image" -> Daniel`ARC`ARCScene[{{10, 10, 10}, {-1, -1, 10}, {-1, -1, 10}}]
                |>,
                <|
                    "Image" -> Daniel`ARC`ARCScene[{{-1, -1, 10}, {-1, -1, 10}, {10, 10, 10}}],
                    "Transform" -> <|"Type" -> "Rotation", "Angle" -> 270|>
                |>,
                <|
                    "Image" -> Daniel`ARC`ARCScene[{{10, -1, -1}, {10, -1, -1}, {10, 10, 10}}],
                    "Transform" -> <|"Type" -> "Rotation", "Angle" -> 180|>
                |>,
                <|
                    "Image" -> Daniel`ARC`ARCScene[{{10, 10, 10}, {10, -1, -1}, {10, -1, -1}}],
                    "Transform" -> <|"Type" -> "Rotation", "Angle" -> 90|>
                |>,
                <|
                    "Image" -> Daniel`ARC`ARCScene[{{-1, -1, 10}, {-1, -1, 10}, {10, 10, 10}}],
                    "Transform" -> <|"Type" -> "Flip", "Direction" -> "Vertical"|>
                |>,
                <|
                    "Image" -> Daniel`ARC`ARCScene[{{10, 10, 10}, {10, -1, -1}, {10, -1, -1}}],
                    "Transform" -> <|"Type" -> "Flip", "Direction" -> "Horizontal"|>
                |>,
                <|
                    "Image" -> Daniel`ARC`ARCScene[
                        {
                            {10, 10, 10, 10, 10, 10},
                            {10, 10, 10, 10, 10, 10},
                            {-1, -1, -1, -1, 10, 10},
                            {-1, -1, -1, -1, 10, 10},
                            {-1, -1, -1, -1, 10, 10},
                            {-1, -1, -1, -1, 10, 10}
                        }
                    ],
                    "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.5|>
                |>,
                <|
                    "Image" -> Daniel`ARC`ARCScene[
                        {
                            {10, 10, 10, 10, 10, 10, 10, 10, 10},
                            {10, 10, 10, 10, 10, 10, 10, 10, 10},
                            {10, 10, 10, 10, 10, 10, 10, 10, 10},
                            {-1, -1, -1, -1, -1, -1, 10, 10, 10},
                            {-1, -1, -1, -1, -1, -1, 10, 10, 10},
                            {-1, -1, -1, -1, -1, -1, 10, 10, 10},
                            {-1, -1, -1, -1, -1, -1, 10, 10, 10},
                            {-1, -1, -1, -1, -1, -1, 10, 10, 10},
                            {-1, -1, -1, -1, -1, -1, 10, 10, 10}
                        }
                    ],
                    "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.3333333333333333|>
                |>,
                <|"Name" -> "L"|>,
                <|"Name" -> "L", "Transform" -> <|"Type" -> "Rotation", "Angle" -> 180|>|>
            }
        |>
    |>
    ,
    TestID -> "ARCRotateShapeAssociations-20220816-Q8BICY"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Module[
                {},
                object = Daniel`ARC`ARCImageRegionToObject[
                    <|
                        "Color" -> 2,
                        "Position" -> {1, 1},
                        "Image" -> {{-1, 1, -1}, {1, 1, 1}},
                        "PixelPositions" -> {{1, 2}, {2, 1}, {2, 2}, {2, 3}}
                    |>,
                    10,
                    10
                ];
                <|
                    "Image" -> object["Image"],
                    "Shapes" -> object["Shapes"],
                    "ImageRotatedBy90" -> Daniel`ARC`RotateImage[object["Image"], 90],
                    "ShapesRotatedBy90" -> Daniel`ARC`ARCRotateShapeAssociations[
                        object["Shapes"],
                        90,
                        object["Image"]
                    ]
                |>
            ]
        ]
    ]
    ,
    <|
        "Image" -> Daniel`ARC`ARCScene[{{-1, 2, -1}, {2, 2, 2}}],
        "Shapes" -> {
            <|"Image" -> Daniel`ARC`ARCScene[{{-1, 10, -1}, {10, 10, 10}}]|>,
            <|
                "Image" -> Daniel`ARC`ARCScene[{{10, -1}, {10, 10}, {10, -1}}],
                "Transform" -> <|"Type" -> "Rotation", "Angle" -> 270|>
            |>,
            <|
                "Image" -> Daniel`ARC`ARCScene[{{10, 10, 10}, {-1, 10, -1}}],
                "Transform" -> <|"Type" -> "Rotation", "Angle" -> 180|>
            |>,
            <|
                "Image" -> Daniel`ARC`ARCScene[{{-1, 10}, {10, 10}, {-1, 10}}],
                "Transform" -> <|"Type" -> "Rotation", "Angle" -> 90|>
            |>,
            <|
                "Image" -> Daniel`ARC`ARCScene[{{10, 10, 10}, {-1, 10, -1}}],
                "Transform" -> <|"Type" -> "Flip", "Direction" -> "Vertical"|>
            |>,
            <|
                "Image" -> Daniel`ARC`ARCScene[
                    {
                        {-1, -1, 10, 10, -1, -1},
                        {-1, -1, 10, 10, -1, -1},
                        {10, 10, 10, 10, 10, 10},
                        {10, 10, 10, 10, 10, 10}
                    }
                ],
                "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.5|>
            |>,
            <|
                "Image" -> Daniel`ARC`ARCScene[
                    {
                        {-1, -1, -1, 10, 10, 10, -1, -1, -1},
                        {-1, -1, -1, 10, 10, 10, -1, -1, -1},
                        {-1, -1, -1, 10, 10, 10, -1, -1, -1},
                        {10, 10, 10, 10, 10, 10, 10, 10, 10},
                        {10, 10, 10, 10, 10, 10, 10, 10, 10},
                        {10, 10, 10, 10, 10, 10, 10, 10, 10}
                    }
                ],
                "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.3333333333333333|>
            |>,
            <|"Name" -> "Triangle"|>
        },
        "ImageRotatedBy90" -> Daniel`ARC`ARCScene[{{2, -1}, {2, 2}, {2, -1}}],
        "ShapesRotatedBy90" -> <|
            "Shape" -> <|
                "Name" -> "Triangle",
                "Transform" -> <|"Type" -> "Rotation", "Angle" -> 90|>
            |>,
            "Shapes" -> {
                <|"Image" -> Daniel`ARC`ARCScene[{{10, -1}, {10, 10}, {10, -1}}]|>,
                <|
                    "Image" -> Daniel`ARC`ARCScene[{{10, 10, 10}, {-1, 10, -1}}],
                    "Transform" -> <|"Type" -> "Rotation", "Angle" -> 270|>
                |>,
                <|
                    "Image" -> Daniel`ARC`ARCScene[{{-1, 10}, {10, 10}, {-1, 10}}],
                    "Transform" -> <|"Type" -> "Rotation", "Angle" -> 180|>
                |>,
                <|
                    "Image" -> Daniel`ARC`ARCScene[{{-1, 10, -1}, {10, 10, 10}}],
                    "Transform" -> <|"Type" -> "Rotation", "Angle" -> 90|>
                |>,
                <|
                    "Image" -> Daniel`ARC`ARCScene[{{-1, 10}, {10, 10}, {-1, 10}}],
                    "Transform" -> <|"Type" -> "Flip", "Direction" -> "Horizontal"|>
                |>,
                <|
                    "Image" -> Daniel`ARC`ARCScene[
                        {
                            {10, 10, -1, -1},
                            {10, 10, -1, -1},
                            {10, 10, 10, 10},
                            {10, 10, 10, 10},
                            {10, 10, -1, -1},
                            {10, 10, -1, -1}
                        }
                    ],
                    "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.5|>
                |>,
                <|
                    "Image" -> Daniel`ARC`ARCScene[
                        {
                            {10, 10, 10, -1, -1, -1},
                            {10, 10, 10, -1, -1, -1},
                            {10, 10, 10, -1, -1, -1},
                            {10, 10, 10, 10, 10, 10},
                            {10, 10, 10, 10, 10, 10},
                            {10, 10, 10, 10, 10, 10},
                            {10, 10, 10, -1, -1, -1},
                            {10, 10, 10, -1, -1, -1},
                            {10, 10, 10, -1, -1, -1}
                        }
                    ],
                    "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.3333333333333333|>
                |>,
                <|"Name" -> "Triangle"|>,
                <|
                    "Name" -> "Triangle",
                    "Transform" -> <|"Type" -> "Rotation", "Angle" -> 90|>
                |>
            }
        |>
    |>
    ,
    TestID -> "ARCRotateShapeAssociations-20220816-1XXG5M"
]