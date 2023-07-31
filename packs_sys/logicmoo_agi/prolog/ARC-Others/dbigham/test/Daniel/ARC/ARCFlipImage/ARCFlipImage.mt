(*
    Tests for: Daniel`ARC`ARCFlipImage
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCFlipImage]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ARCFlipImage[{{1, 0}, {0, 1}}, "Vertical"]
    ,
    {{0, 1}, {1, 0}}
    ,
    TestID -> "ARCFlipImage-20220907-QPC0AY"
]

Test[
    Daniel`ARC`ARCFlipImage[{{1, 0}, {0, 1}}, "Horizontal"]
    ,
    {{0, 1}, {1, 0}}
    ,
    TestID -> "ARCFlipImage-20220907-ZS9QIP"
]

Test[
    Daniel`ARC`ARCFlipImage[Daniel`ARC`ARCScene[{{1, 0}, {0, 1}}], "Vertical"]
    ,
    Daniel`ARC`ARCScene[{{0, 1}, {1, 0}}]
    ,
    TestID -> "ARCFlipImage-20220907-M9GPX1"
]