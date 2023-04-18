(*
    Tests for: Daniel`ARC`ARCOutputObjectsNotMappedTo
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCOutputObjectsNotMappedTo]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ARCOutputObjectsNotMappedTo[
        {<|"UUID" -> 1|>, <|"UUID" -> 2|>},
        <|<|"UUID" -> 3|> -> <|"UUID" -> 1|>|>
    ]
    ,
    {<|"UUID" -> 2|>}
    ,
    TestID -> "ARCOutputObjectsNotMappedTo-20220820-0AQ65Y"
]