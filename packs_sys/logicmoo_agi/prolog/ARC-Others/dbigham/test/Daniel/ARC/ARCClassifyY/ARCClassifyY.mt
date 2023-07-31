(*
    Tests for: Daniel`ARC`ARCClassifyY
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCClassifyY]
    
    Author: danielb
*)

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCClassifyY[
            Replace[#1, 0 -> -1, {2}] & [
                {
                    {1, 0, 0, 0, 1},
                    {1, 0, 0, 0, 1},
                    {1, 1, 1, 1, 1},
                    {0, 0, 1, 0, 0},
                    {0, 0, 1, 0, 0},
                    {0, 0, 1, 0, 0}
                }
            ]
        ]
    ]
    ,
    <|"Name" -> "Y", "StemHeight" -> 3|>
    ,
    TestID -> "ARCClassifyY-20221110-7KASAQ"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCClassifyY[
            Replace[#1, 0 -> -1, {2}] & [
                {
                    {1, 0, 0, 0, 1},
                    {1, 0, 0, 0, 1},
                    {1, 1, 1, 1, 1},
                    {0, 1, 0, 0, 0},
                    {0, 1, 0, 0, 0},
                    {0, 1, 0, 0, 0}
                }
            ]
        ]
    ]
    ,
    {}
    ,
    TestID -> "ARCClassifyY-20221110-8N1MY3"
]

Test[
    Daniel`ARC`ARCClassifyY[
        Replace[#1, 0 -> -1, {2}] & [
            {
                {1, 0, 0, 0, 1},
                {1, 0, 0, 0, 1},
                {1, 1, 1, 1, 1},
                {0, 0, 1, 0, 0},
                {0, 0, 1, 0, 0},
                {0, 0, 1, 0, 0}
            }
        ]
    ]
    ,
    {<|"Name" -> "Y", "Angle" -> 0, "StemHeight" -> 3|>}
    ,
    TestID -> "ARCClassifyY-20221111-RDHCH3"
]