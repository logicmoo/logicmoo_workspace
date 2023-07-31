(*
    Tests for: Daniel`ARC`ARCApplyImageTransforms
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCApplyImageTransforms]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ARCApplyImageTransforms[
        Daniel`ARC`ARCScene[{{1, -1, -1}, {1, -1, -1}, {1, 1, 1}}],
        <|"Type" -> "Rotation", "Angle" -> 90|>
    ]
    ,
    Daniel`ARC`ARCScene[{{1, 1, 1}, {1, -1, -1}, {1, -1, -1}}]
    ,
    TestID -> "ARCApplyImageTransforms-20220811-AI03IN"
]

Test[
    Daniel`ARC`ARCApplyImageTransforms[
        Daniel`ARC`ARCScene[{{1, -1, -1}, {1, -1, -1}, {1, 1, 1}}],
        <|"Type" -> "Flip", "Direction" -> "Vertical"|>
    ]
    ,
    Daniel`ARC`ARCScene[{{1, 1, 1}, {1, -1, -1}, {1, -1, -1}}]
    ,
    TestID -> "ARCApplyImageTransforms-20220811-7XQL6N"
]

Test[
    Daniel`ARC`ARCApplyImageTransforms[
        Daniel`ARC`ARCScene[{{1, -1, -1}, {1, -1, -1}, {1, 1, 1}}],
        <|"Type" -> "Flip", "Direction" -> "Horizontal"|>
    ]
    ,
    Daniel`ARC`ARCScene[{{-1, -1, 1}, {-1, -1, 1}, {1, 1, 1}}]
    ,
    TestID -> "ARCApplyImageTransforms-20220811-HG7Q29"
]