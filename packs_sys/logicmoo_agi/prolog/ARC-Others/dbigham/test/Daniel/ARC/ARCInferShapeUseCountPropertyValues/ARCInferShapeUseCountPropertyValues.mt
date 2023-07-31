(*
    Tests for: Daniel`ARC`ARCInferShapeUseCountPropertyValues
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCInferShapeUseCountPropertyValues]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ARCInferShapeUseCountPropertyValues[
        {<|"Shape" -> "A"|>, <|"Shape" -> "A"|>, <|"Shape" -> "B"|>}
    ]
    ,
    {
        <|"Shape" -> "A", "ShapeUseCount" -> 2|>,
        <|"Shape" -> "A", "ShapeUseCount" -> 2|>,
        <|"Shape" -> "B", "ShapeUseCount" -> 1|>
    }
    ,
    TestID -> "ARCInferShapeUseCountPropertyValues-20220909-U18NML"
]