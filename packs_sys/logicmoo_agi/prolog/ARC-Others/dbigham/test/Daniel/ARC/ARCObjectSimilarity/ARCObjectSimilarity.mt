(*
    Tests for: Daniel`ARC`ARCObjectSimilarity
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCObjectSimilarity]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ARCObjectSimilarity[
        <|"Image" -> {{1}}, "Color" -> 1, "Colors" -> {1}, "Y" -> 1, "X" -> 1|>,
        <|"Image" -> {{1}}, "Color" -> 1, "Colors" -> {1}, "Y" -> 1, "X" -> 1|>
    ]
    ,
    0.
    ,
    TestID -> "ARCObjectSimilarity-20221109-CQLANV"
]

Test[
    Daniel`ARC`ARCObjectSimilarity[
        <|
            "Image" -> {{1}},
            "Color" -> 1,
            "Colors" -> {1},
            "Shape" -> <|"Name" -> "Pixel"|>,
            "Y" -> 1,
            "X" -> 1
        |>,
        <|
            "Image" -> {{2}},
            "Color" -> 2,
            "Colors" -> {2},
            "Shape" -> <|"Name" -> "Pixel"|>,
            "Y" -> 1,
            "X" -> 1
        |>
    ]
    ,
    1.
    ,
    TestID -> "ARCObjectSimilarity-20221109-R5KT2O"
]

Test[
    Daniel`ARC`ARCObjectSimilarity[
        <|
            "Image" -> {{1}},
            "Color" -> 1,
            "Colors" -> {1},
            "Shape" -> <|"Name" -> "Pixel"|>,
            "Y" -> 1,
            "X" -> 1
        |>,
        <|
            "Image" -> {{1}},
            "Color" -> 1,
            "Colors" -> {1},
            "Shape" -> <|"Name" -> "Pixel"|>,
            "Y" -> 5,
            "X" -> 5
        |>
    ]
    ,
    0.5656854249492381
    ,
    TestID -> "ARCObjectSimilarity-20221109-F7PC05"
]