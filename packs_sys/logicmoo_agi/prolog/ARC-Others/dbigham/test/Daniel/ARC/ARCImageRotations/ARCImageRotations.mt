(*
    Tests for: Daniel`ARC`ARCImageRotations
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCImageRotations]
    
    Author: danielb
*)

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCImageRotations[{{1, -1, 1}, {-1, 1, -1}, {-1, -1, 1}}]
    ]
    ,
    {
        <|
            "Image" -> Daniel`ARC`ARCScene[{{-1, -1, 1}, {-1, 1, -1}, {1, -1, 1}}],
            "Transform" -> <|"Type" -> "Rotation", "Angle" -> 270|>
        |>,
        <|
            "Image" -> Daniel`ARC`ARCScene[{{1, -1, -1}, {-1, 1, -1}, {1, -1, 1}}],
            "Transform" -> <|"Type" -> "Rotation", "Angle" -> 180|>
        |>,
        <|
            "Image" -> Daniel`ARC`ARCScene[{{1, -1, 1}, {-1, 1, -1}, {1, -1, -1}}],
            "Transform" -> <|"Type" -> "Rotation", "Angle" -> 90|>
        |>
    }
    ,
    TestID -> "ARCImageRotations-20220722-ATWYCZ"
]

Test[
    Daniel`ARC`ARCImageRotations[{{1}}]
    ,
    {}
    ,
    TestID -> "ARCImageRotations-20220908-VZ1NV1"
]

Test[
    Daniel`ARC`ARCImageRotations[{{1}}, "IncludeNoopTransforms" -> True]
    ,
    {
        <|
            "Image" -> Daniel`ARC`ARCScene[{{1}}],
            "Transform" -> <|"Type" -> "Rotation", "Angle" -> 270|>
        |>,
        <|
            "Image" -> Daniel`ARC`ARCScene[{{1}}],
            "Transform" -> <|"Type" -> "Rotation", "Angle" -> 180|>
        |>,
        <|
            "Image" -> Daniel`ARC`ARCScene[{{1}}],
            "Transform" -> <|"Type" -> "Rotation", "Angle" -> 90|>
        |>
    }
    ,
    TestID -> "ARCImageRotations-20220908-N1IEU0"
]