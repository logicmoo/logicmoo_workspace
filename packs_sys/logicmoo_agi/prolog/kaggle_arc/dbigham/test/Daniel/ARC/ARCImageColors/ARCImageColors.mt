(*
    Tests for: Daniel`ARC`ARCImageColors
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCImageColors]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ARCImageColors[{{1, 2, 0}, {0, 0, 3}}]
    ,
    {0, 1, 2, 3}
    ,
    TestID -> "ARCImageColors-20220904-0EHFXC"
]

Test[
    Daniel`ARC`ARCImageColors[Daniel`ARC`ARCScene[{{1, 2, 0}, {0, 0, 3}}]]
    ,
    {0, 1, 2, 3}
    ,
    TestID -> "ARCImageColors-20220904-E71RLE"
]