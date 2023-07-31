(*
    Tests for: Daniel`ARC`ARCChooseTransform
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCChooseTransform]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ARCChooseTransform[
        {
            <|"Transforms" -> {<|"Type" -> "Rotation", "Angle" -> 90|>}|>,
            <|"Transforms" -> {<|"Type" -> "Rotation", "Angle" -> 90|>}|>,
            <|"Transforms" -> {<|"Type" -> "Rotation", "Angle" -> 90|>}|>
        }
    ]
    ,
    {
        <|"Transform" -> <|"Type" -> "Rotation", "Angle" -> 90|>|>,
        <|"Transform" -> <|"Type" -> "Rotation", "Angle" -> 90|>|>,
        <|"Transform" -> <|"Type" -> "Rotation", "Angle" -> 90|>|>
    }
    ,
    TestID -> "ARCChooseTransform-20220907-5DE5EL"
]

Test[
    Daniel`ARC`ARCChooseTransform[
        {
            <|"Transforms" -> {<|"Type" -> "Rotation", "Angle" -> 90|>}|>,
            <|
                "Transforms" -> {
                    <|"Type" -> "Rotation", "Angle" -> 270|>,
                    <|"Type" -> "Rotation", "Angle" -> 90|>
                }
            |>,
            <|"Transforms" -> {<|"Type" -> "Rotation", "Angle" -> 90|>}|>
        }
    ]
    ,
    {
        <|"Transform" -> <|"Type" -> "Rotation", "Angle" -> 90|>|>,
        <|"Transform" -> <|"Type" -> "Rotation", "Angle" -> 90|>|>,
        <|"Transform" -> <|"Type" -> "Rotation", "Angle" -> 90|>|>
    }
    ,
    TestID -> "ARCChooseTransform-20220907-5Z93ZK"
]

Test[
    Daniel`ARC`ARCChooseTransform[
        {
            <|"Transforms" -> {<|"Type" -> "Rotation", "Angle" -> 90|>}|>,
            <|
                "Transforms" -> {
                    <|"Type" -> "Flip", "Direction" -> "Horizontal"|>,
                    <|"Type" -> "Rotation", "Angle" -> 180|>
                }
            |>,
            <|"Transforms" -> {<|"Type" -> "Rotation", "Angle" -> 270|>}|>
        }
    ]
    ,
    {
        <|"Transform" -> <|"Type" -> "Rotation", "Angle" -> 90|>|>,
        <|"Transform" -> <|"Type" -> "Rotation", "Angle" -> 180|>|>,
        <|"Transform" -> <|"Type" -> "Rotation", "Angle" -> 270|>|>
    }
    ,
    TestID -> "ARCChooseTransform-20220907-BHIPKF"
]

Test[
    Daniel`ARC`ARCChooseTransform[
        {
            <|"Transforms" -> {<|"Type" -> "Rotation", "Angle" -> 90|>}|>,
            <|
                "Transforms" -> {
                    <|"Type" -> "Flip", "Direction" -> "Horizontal"|>,
                    <|"Type" -> "AnotherType"|>
                }
            |>,
            <|"Transforms" -> {<|"Type" -> "Scale", "Factor" -> 2|>}|>
        }
    ]
    ,
    {
        <|"Transform" -> <|"Type" -> "Rotation", "Angle" -> 90|>|>,
        <|"Transform" -> <|"Type" -> "Flip", "Direction" -> "Horizontal"|>|>,
        <|"Transform" -> <|"Type" -> "Scale", "Factor" -> 2|>|>
    }
    ,
    TestID -> "ARCChooseTransform-20220907-WNATXN"
]

Test[
    Daniel`ARC`ARCChooseTransform[
        {
            <|"Transforms" -> {<|"Type" -> "Rotation", "Angle" -> 90|>}|>,
            <|
                "Transforms" -> {
                    <|"Type" -> "Flip", "Direction" -> "Horizontal"|>,
                    <|"Type" -> "AnotherType"|>
                }
            |>,
            <|"MyKey" -> 1|>
        }
    ]
    ,
    {
        <|"Transform" -> <|"Type" -> "Rotation", "Angle" -> 90|>|>,
        <|"Transform" -> <|"Type" -> "Flip", "Direction" -> "Horizontal"|>|>,
        <|"MyKey" -> 1|>
    }
    ,
    TestID -> "ARCChooseTransform-20220907-85MLU8"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`SimplifyObjects[
            Daniel`ARC`ARCChooseTransform[
                {
                    <|
                        "Image" -> Daniel`ARC`ARCScene[{{8, 6}, {6, 8}}],
                        "Colors" -> {6, 8},
                        "Input" -> <|"Image" -> Daniel`ARC`ARCScene[{{8, 6}, {6, 8}}]|>
                    |>,
                    <|
                        "Image" -> Daniel`ARC`ARCScene[{{8, 8, 8}, {8, 7, 7}, {8, 7, 7}}],
                        "Transforms" -> {<|"Type" -> "Rotation", "Angle" -> 180|>}
                    |>,
                    <|
                        "Image" -> Daniel`ARC`ARCScene[{{4, 4, 6}, {4, 4, 6}, {9, 9, 6}}],
                        "Transforms" -> {<|"Type" -> "Rotation", "Angle" -> 180|>}
                    |>
                }
            ]
        ]
    ]
    ,
    {
        <|
            "Image" -> Daniel`ARC`ARCScene[{{8, 6}, {6, 8}}],
            "Transform" -> <|"Type" -> "Rotation", "Angle" -> 180|>
        |>,
        <|
            "Image" -> Daniel`ARC`ARCScene[{{8, 8, 8}, {8, 7, 7}, {8, 7, 7}}],
            "Transform" -> <|"Type" -> "Rotation", "Angle" -> 180|>
        |>,
        <|
            "Image" -> Daniel`ARC`ARCScene[{{4, 4, 6}, {4, 4, 6}, {9, 9, 6}}],
            "Transform" -> <|"Type" -> "Rotation", "Angle" -> 180|>
        |>
    }
    ,
    TestID -> "ARCChooseTransform-20220908-BTLAWV"
]