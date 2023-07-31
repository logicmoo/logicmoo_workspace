(*
    Tests for: Daniel`ARC`ARCPixelColor
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCPixelColor]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ARCPixelColor[{{1, 2}, {3, 4}}, {1, 1}]
    ,
    1
    ,
    TestID -> "ARCPixelColor-20220830-2WY68H"
]

Test[
    Daniel`ARC`ARCPixelColor[{{1, 2}, {3, 4}}, {2, 2}]
    ,
    4
    ,
    TestID -> "ARCPixelColor-20220830-WIO05H"
]

Test[
    Daniel`ARC`ARCPixelColor[{{1, 2}, {3, 4}}, {1, 3}]
    ,
    Missing["OutOfBounds"]
    ,
    TestID -> "ARCPixelColor-20220830-VU5Q2F"
]

Test[
    Daniel`ARC`ARCPixelColor[{{1, 2}, {3, 4}}, {3, 1}]
    ,
    Missing["OutOfBounds"]
    ,
    TestID -> "ARCPixelColor-20220830-6FZZ2N"
]

Test[
    Daniel`ARC`ARCPixelColor[{{1, 2}, {3, 4}}, {0, 1}]
    ,
    Missing["OutOfBounds"]
    ,
    TestID -> "ARCPixelColor-20220830-6WXHSW"
]

Test[
    Daniel`ARC`ARCPixelColor[{{1, 2}, {3, 4}}, {1, 0}]
    ,
    Missing["OutOfBounds"]
    ,
    TestID -> "ARCPixelColor-20220830-2H2LY5"
]