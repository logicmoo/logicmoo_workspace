(*
    Tests for: Daniel`ARC`ARCImageFlipPlusRotations
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCImageFlipPlusRotations]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ARCImageFlipPlusRotations[
        Daniel`ARC`ARCParseFile["74dd1130"]["Train", 1][["Input", 1]]
    ]
    ,
    {
        <|
            "Image" -> Daniel`ARC`ARCScene[{{2, 1, 1}, {2, 5, 2}, {5, 1, 2}}],
            "Transform" -> {
                <|"Type" -> "Rotation", "Angle" -> 90|>,
                <|"Type" -> "Flip", "Direction" -> "Vertical"|>
            }
        |>,
        <|
            "Image" -> Daniel`ARC`ARCScene[{{1, 2, 2}, {1, 5, 1}, {2, 2, 5}}],
            "Transform" -> {
                <|"Type" -> "Rotation", "Angle" -> 180|>,
                <|"Type" -> "Flip", "Direction" -> "Vertical"|>
            }
        |>,
        <|
            "Image" -> Daniel`ARC`ARCScene[{{2, 1, 5}, {2, 5, 2}, {1, 1, 2}}],
            "Transform" -> {
                <|"Type" -> "Rotation", "Angle" -> 270|>,
                <|"Type" -> "Flip", "Direction" -> "Vertical"|>
            }
        |>,
        <|
            "Image" -> Daniel`ARC`ARCScene[{{5, 2, 2}, {1, 5, 1}, {2, 2, 1}}],
            "Transform" -> {
                <|"Type" -> "Rotation", "Angle" -> 180|>,
                <|"Type" -> "Flip", "Direction" -> "Horizontal"|>
            }
        |>
    }
    ,
    TestID -> "ARCImageFlipPlusRotations-20220913-QUZ64J"
]