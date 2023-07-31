(*
    Tests for: Daniel`ARC`ARCSceneObject
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCSceneObject]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ARCSceneObject[
        Daniel`ARC`ARCParseFile["d13f3404"]["Train", 1, "Input"],
        0,
        Missing[]
    ]
    ,
    <|
        "Background" -> 0,
        "Width" -> 3,
        "Height" -> 3,
        "Scene" -> Daniel`ARC`ARCScene[{{6, 1, 0}, {3, 0, 0}, {0, 0, 0}}],
        "Colors" -> {1, 3, 6},
        "ColorCount" -> 3,
        "YMiddle" -> 2,
        "XMiddle" -> 2,
        "Length" -> 3,
        "PrimarySizeDimension" -> "None",
        "AspectRatio" -> 1,
        "Area" -> 9,
        "FilledArea" -> 3,
        "FilledProportion" -> 0.3333333333333333,
        "SurfacePixelCount" -> 3,
        "VerticalLineSymmetry" -> False,
        "HorizontalLineSymmetry" -> False,
        "VerticalAndHorizontalLineSymmetry" -> False,
        "HollowCount" -> 0
    |>
    ,
    TestID -> "ARCSceneObject-20221004-DCTMOS"
]