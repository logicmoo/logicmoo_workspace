(*
    Tests for: Daniel`ARC`Transform
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`Transform]
    
    Author: danielb
*)

Test[
    Daniel`ARC`Transform[
        Daniel`ARC`ARCScene[{{1, 1}, {-1, -1}}],
        <|"Type" -> "Rotation", "Angle" -> 270|>
    ]
    ,
    Daniel`ARC`ARCScene[{{1, -1}, {1, -1}}]
    ,
    TestID -> "Transform-20220912-TF3623"
]

Test[
    Daniel`ARC`Transform[
        Daniel`ARC`ARCScene[{{1, 1}, {-1, -1}}],
        <|"Type" -> "Flip", "Direction" -> "Vertical"|>
    ]
    ,
    Daniel`ARC`ARCScene[{{-1, -1}, {1, 1}}]
    ,
    TestID -> "Transform-20220912-A06WFY"
]

Test[
    Daniel`ARC`Transform[
        Daniel`ARC`ARCScene[{{1, 1}, {-1, -1}}],
        <|"Type" -> "Scaled", "Factor" -> 2|>
    ]
    ,
    Daniel`ARC`ARCScene[{{1, 1, 1, 1}, {1, 1, 1, 1}, {-1, -1, -1, -1}, {-1, -1, -1, -1}}]
    ,
    TestID -> "Transform-20220912-CKFJH8"
]