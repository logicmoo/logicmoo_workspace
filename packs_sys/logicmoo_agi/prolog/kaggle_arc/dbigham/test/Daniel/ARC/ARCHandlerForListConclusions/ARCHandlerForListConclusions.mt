(*
    Tests for: Daniel`ARC`ARCHandlerForListConclusions
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCHandlerForListConclusions]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ARCHandlerForListConclusions[
        "Transform",
        {
            <|"Type" -> "Rotation", "Angle" -> 90|>,
            <|"Type" -> "Flip", "Direction" -> "Vertical"|>
        },
        Function[
            {object2, key2, value2},
            Daniel`ARC`ARCApplyConclusion[key2, value2, object2, <||>, <||>]
        ],
        <|"Image" -> Daniel`ARC`ARCScene[{{1, 2}, {0, 1}}]|>
    ]
    ,
    <|"Image" -> Daniel`ARC`ARCScene[{{1, 2}, {0, 1}}]|>
    ,
    TestID -> "ARCHandlerForListConclusions-20220913-85PDR7"
]