(*
    Tests for: Daniel`ARC`ARCTwoDiagonalRectanglesQ
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCTwoDiagonalRectanglesQ]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ARCTwoDiagonalRectanglesQ[
        <|"Shape" -> <|"Name" -> "Rectangle"|>, "X" -> 6, "Y" -> 6, "X2" -> 10, "Y2" -> 10|>,
        <|"Shape" -> <|"Name" -> "Rectangle"|>, "X2" -> 5, "Y2" -> 5|>
    ]
    ,
    True
    ,
    TestID -> "ARCTwoDiagonalRectanglesQ-20220918-I8GVBS"
]