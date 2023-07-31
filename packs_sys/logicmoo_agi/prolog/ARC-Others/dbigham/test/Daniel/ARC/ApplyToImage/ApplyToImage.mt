(*
    Tests for: Daniel`ARC`ApplyToImage
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ApplyToImage]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ApplyToImage[
        Daniel`ARC`ARCScene[{{2, -1, -1}, {2, -1, -1}, {2, 2, 2}}],
        Reverse
    ]
    ,
    Daniel`ARC`ARCScene[{{2, 2, 2}, {2, -1, -1}, {2, -1, -1}}]
    ,
    TestID -> "ApplyToImage-20220811-BZQUGI"
]

Test[
    Daniel`ARC`ApplyToImage[{{2, -1, -1}, {2, -1, -1}, {2, 2, 2}}, Reverse]
    ,
    {{2, 2, 2}, {2, -1, -1}, {2, -1, -1}}
    ,
    TestID -> "ApplyToImage-20220811-PU4JIY"
]