(*
    Tests for: Daniel`ARC`ARCMappingToObjectWithOverlappingFilledInPixels
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCMappingToObjectWithOverlappingFilledInPixels]
    
    Author: danielb
*)

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCMappingToObjectWithOverlappingFilledInPixels[
            <|"PixelPositions" -> {{4, 3}}, "Position" -> {4, 3}|>,
            {
                <|
                    "Image" -> Daniel`ARC`ARCScene[
                        {
                            {2, -1, -1, -1, 2},
                            {-1, 2, -1, 2, -1},
                            {-1, -1, 2, -1, -1},
                            {-1, 2, -1, 2, -1},
                            {2, -1, -1, -1, 2}
                        }
                    ],
                    "PixelPositions" -> {
                        {2, 1},
                        {2, 5},
                        {3, 2},
                        {3, 4},
                        {4, 3},
                        {5, 2},
                        {5, 4},
                        {6, 1},
                        {6, 5}
                    },
                    "Position" -> {2, 1}
                |>,
                <|
                    "PixelPositions" -> {
                        {6, 7},
                        {6, 11},
                        {7, 8},
                        {7, 10},
                        {8, 9},
                        {9, 8},
                        {9, 10},
                        {10, 7},
                        {10, 11}
                    }
                |>
            }
        ]
    ]
    ,
    <|"PixelPositions" -> {{4, 3}}, "Position" -> {4, 3}|> -> <|
        "Image" -> Daniel`ARC`ARCScene[
            {
                {2, -1, -1, -1, 2},
                {-1, 2, -1, 2, -1},
                {-1, -1, 2, -1, -1},
                {-1, 2, -1, 2, -1},
                {2, -1, -1, -1, 2}
            }
        ],
        "PixelPositions" -> {
            {2, 1},
            {2, 5},
            {3, 2},
            {3, 4},
            {4, 3},
            {5, 2},
            {5, 4},
            {6, 1},
            {6, 5}
        },
        "Position" -> <|"RelativePosition" -> {-2, -2}, "Y" -> 2, "X" -> 1|>
    |>
    ,
    TestID -> "ARCMappingToObjectWithOverlappingFilledInPixels-20220822-16LMWW"
]