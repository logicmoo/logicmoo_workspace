(*
    Tests for: Daniel`ARC`ARCInferBackgroundColor
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCInferBackgroundColor]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ARCInferBackgroundColor[Daniel`ARC`ARCParseFile["007bbfb7"]]
    ,
    0
    ,
    TestID -> "ARCInferBackgroundColor-20220806-8EYVVV"
]

Test[
    Daniel`ARC`ARCInferBackgroundColor[Daniel`ARC`ARCParseFile["ihiz27k2n"]]
    ,
    5
    ,
    TestID -> "ARCInferBackgroundColor-20220806-Z81G6V"
]

Test[
    Daniel`ARC`ARCInferBackgroundColor[
        Daniel`ARC`ARCParseFile["ihiz27k2n"]["Test", 1, "Input"]
    ]
    ,
    5
    ,
    TestID -> "ARCInferBackgroundColor-20220806-28PU9I"
]

Test[
    Daniel`ARC`ARCInferBackgroundColor[
        Daniel`ARC`ARCScene[{{1, 5, 5, 1, 5}, {8, 1, 5, 5, 5}, {8, 5, 1, 5, 8}}],
        "FormMultiColorCompositeObjects" -> False
    ]
    ,
    0
    ,
    TestID -> "ARCInferBackgroundColor-20220819-226TGE"
]

Test[
    Daniel`ARC`ARCInferBackgroundColor[
        Daniel`ARC`ARCParseFile["007bbfb7"],
        "Return" -> "Details"
    ]
    ,
    <|
        "Background" -> 0,
        "BlackAsFallback" -> False,
        "Ratio" -> 3.65,
        "ColorCounts" -> {0 -> 358, 7 -> 98, 2 -> 42, 6 -> 30, 4 -> 12}
    |>
    ,
    TestID -> "ARCInferBackgroundColor-20221001-ZAK5Q4"
]

Test[
    Daniel`ARC`ARCInferBackgroundColor[
        Daniel`ARC`ARCScene[{{1, 5, 5, 1, 5}, {8, 1, 5, 5, 5}, {8, 5, 1, 5, 8}}],
        "FormMultiColorCompositeObjects" -> False,
        "Return" -> "Details"
    ]
    ,
    <|
        "Background" -> 0,
        "BlackAsFallback" -> True,
        "Ratio" -> 2.,
        "ColorCounts" -> {5 -> 8, 1 -> 4, 8 -> 3}
    |>
    ,
    TestID -> "ARCInferBackgroundColor-20221001-2KEBO0"
]