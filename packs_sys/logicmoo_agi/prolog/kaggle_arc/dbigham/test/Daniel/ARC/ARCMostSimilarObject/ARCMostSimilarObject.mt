(*
    Tests for: Daniel`ARC`ARCMostSimilarObject
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCMostSimilarObject]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ARCMostSimilarObject[
        <|
            "Image" -> {{1}},
            "Color" -> 1,
            "Colors" -> {1},
            "Shape" -> <|"Name" -> "Pixel"|>,
            "Y" -> 1,
            "X" -> 1
        |>,
        {
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
        }
    ]
    ,
    <|
        "Image" -> {{1}},
        "Color" -> 1,
        "Colors" -> {1},
        "Shape" -> <|"Name" -> "Pixel"|>,
        "Y" -> 1,
        "X" -> 1
    |>
    ,
    TestID -> "ARCMostSimilarObject-20221109-XQU1US"
]