(*
    Tests for: Daniel`ARC`ARCInferGeneralShapeUseCountPropertyValues
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCInferGeneralShapeUseCountPropertyValues]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ARCInferGeneralShapeUseCountPropertyValues[
        {
            <|"Shape" -> <|"Name" -> "Rectangle"|>|>,
            <|"Shape" -> <|"Name" -> "Rectangle", "Filled" -> True|>|>,
            <|"Shape" -> "B"|>
        }
    ]
    ,
    {
        <|"Shape" -> <|"Name" -> "Rectangle"|>, "GeneralShapeUseCount" -> 2|>,
        <|
            "Shape" -> <|"Name" -> "Rectangle", "Filled" -> True|>,
            "GeneralShapeUseCount" -> 2
        |>,
        <|"Shape" -> "B", "GeneralShapeUseCount" -> 1|>
    }
    ,
    TestID -> "ARCInferGeneralShapeUseCountPropertyValues-20220909-NM0L0M"
]