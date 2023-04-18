(*
    Tests for: Daniel`ARC`ARCSurroundingPixels
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCSurroundingPixels]
    
    Author: danielb
*)

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCSurroundingPixels[
            <|"X" -> 2, "Y" -> 2, "X2" -> 2, "Y2" -> 2|>,
            {{1, 2, 3}, {4, 5, 6}, {7, 8, 9}}
        ]
    ]
    ,
    <|
        {1, 1} -> 1,
        {1, 2} -> 2,
        {1, 3} -> 3,
        {2, 1} -> 4,
        {2, 3} -> 6,
        {3, 1} -> 7,
        {3, 2} -> 8,
        {3, 3} -> 9
    |>
    ,
    TestID -> "ARCSurroundingPixels-20220904-IJ2SCS"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCSurroundingPixels[
            <|"X" -> 1, "Y" -> 1, "X2" -> 1, "Y2" -> 1|>,
            {{1, 2, 3}, {4, 5, 6}, {7, 8, 9}}
        ]
    ]
    ,
    <|
        {0, 0} -> Missing["OutOfBounds"],
        {0, 1} -> Missing["OutOfBounds"],
        {0, 2} -> Missing["OutOfBounds"],
        {1, 0} -> Missing["OutOfBounds"],
        {1, 2} -> 2,
        {2, 0} -> Missing["OutOfBounds"],
        {2, 1} -> 4,
        {2, 2} -> 5
    |>
    ,
    TestID -> "ARCSurroundingPixels-20220904-T8TTP9"
]