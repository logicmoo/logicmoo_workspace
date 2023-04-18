(*
    Tests for: Daniel`ARC`ShapeQ
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ShapeQ]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ShapeQ[<|"Shape" -> <|"Name" -> "Line"|>|>, "Line"]
    ,
    True
    ,
    TestID -> "ShapeQ-20220826-90L0XI"
]

Test[
    Daniel`ARC`ShapeQ[<|"Shape" -> <|"Name" -> "Line"|>|>, "Square"]
    ,
    False
    ,
    TestID -> "ShapeQ-20220826-DO5MLA"
]