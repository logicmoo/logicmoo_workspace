(*
    Tests for: Daniel`ARC`ARCClassifyTriangle
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCClassifyTriangle]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ARCClassifyTriangle[{{-1, 1, -1}, {1, 1, 1}}]
    ,
    <|"Name" -> "Triangle"|>
    ,
    TestID -> "ARCClassifyTriangle-20220813-NI8ZAK"
]

Test[
    Daniel`ARC`ARCClassifyTriangle[
        {{-1, -1, 1, -1, -1}, {-1, 1, 1, 1, -1}, {1, 1, 1, 1, 1}}
    ]
    ,
    <|"Name" -> "Triangle"|>
    ,
    TestID -> "ARCClassifyTriangle-20220813-8IVY3K"
]

Test[
    ToString[Daniel`ARC`ARCClassifyTriangle[{{1, 1, 1}, {1, 1, 1}, {1, 1, 1}}]]
    ,
    "Nothing"
    ,
    TestID -> "ARCClassifyTriangle-20220813-P6YPEG"
]

Test[
    ToString[Daniel`ARC`ARCClassifyTriangle[{{1}}]]
    ,
    "Nothing"
    ,
    TestID -> "ARCClassifyTriangle-20220813-SJQGXP"
]