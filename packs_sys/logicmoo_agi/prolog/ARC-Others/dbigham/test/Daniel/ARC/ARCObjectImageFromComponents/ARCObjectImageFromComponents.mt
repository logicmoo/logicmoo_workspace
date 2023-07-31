(*
    Tests for: Daniel`ARC`ARCObjectImageFromComponents
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCObjectImageFromComponents]
    
    Author: danielb
*)

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCObjectImageFromComponents[
            <|
                "Image" -> Daniel`ARC`ARCScene[{{5, 5, 5}, {5, -1, 5}, {5, 5, 5}}],
                "Position" -> {2, 2},
                "Y" -> 2,
                "X" -> 2,
                "Components" -> {
                    <|
                        "Image" -> Daniel`ARC`ARCScene[{{5, 5, 5}, {5, -1, 5}, {5, 5, 5}}],
                        "Position" -> {2, 2}
                    |>,
                    <|
                        "Image" -> Daniel`ARC`ARCScene[{{2}}],
                        "Position" -> <|"RelativePosition" -> {-1, 0}|>
                    |>
                }
            |>
        ]
    ]
    ,
    <|
        "Image" -> Daniel`ARC`ARCScene[{{2, -1, -1}, {5, 5, 5}, {5, -1, 5}, {5, 5, 5}}],
        "Position" -> {1, 2}
    |>
    ,
    TestID -> "ARCObjectImageFromComponents-20220807-LZ6PPJ"
]