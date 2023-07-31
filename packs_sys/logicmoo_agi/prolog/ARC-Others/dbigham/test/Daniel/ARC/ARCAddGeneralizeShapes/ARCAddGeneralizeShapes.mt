(*
    Tests for: Daniel`ARC`ARCAddGeneralizeShapes
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCAddGeneralizeShapes]
    
    Author: danielb
*)

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCAddGeneralizeShapes[
            {
                <|"Name" -> "Pixel"|>,
                <|"Name" -> "Square", "Filled" -> True|>,
                <|"Name" -> "Rectangle", "Filled" -> True|>
            }
        ]
    ]
    ,
    {
        <|"Name" -> "Pixel"|>,
        <|"Name" -> "Square"|>,
        <|"Name" -> "Rectangle"|>,
        <|"Name" -> "Square", "Filled" -> True|>,
        <|"Name" -> "Rectangle", "Filled" -> True|>
    }
    ,
    TestID -> "ARCAddGeneralizeShapes-20220810-4EP2Z6"
]