(*
    Tests for: Daniel`ARC`ARCClassifyRotatedImage
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCClassifyRotatedImage]
    
    Author: danielb
*)

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCClassifyRotatedImage[
            m = {{1, 1, 1, 1}, {1, -1, -1, -1}, {1, -1, -1, -1}},
            Daniel`ARC`ARCClassifyL
        ]
    ]
    ,
    {<|"Name" -> "L", "Transform" -> <|"Type" -> "Rotation", "Angle" -> 90|>|>}
    ,
    TestID -> "ARCClassifyRotatedImage-20220717-6KO2ZP"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCClassifyRotatedImage[
            m = {{1, 1, 1}, {-1, -1, 1}, {-1, -1, 1}, {-1, -1, 1}},
            Daniel`ARC`ARCClassifyL
        ]
    ]
    ,
    {<|"Name" -> "L", "Transform" -> <|"Type" -> "Rotation", "Angle" -> 180|>|>}
    ,
    TestID -> "ARCClassifyRotatedImage-20220717-NG9CC2"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCClassifyRotatedImage[
            m = {{-1, -1, -1, 1}, {-1, -1, -1, 1}, {1, 1, 1, 1}},
            Daniel`ARC`ARCClassifyL
        ]
    ]
    ,
    {<|"Name" -> "L", "Transform" -> <|"Type" -> "Rotation", "Angle" -> 270|>|>}
    ,
    TestID -> "ARCClassifyRotatedImage-20220717-NI7Q2Q"
]