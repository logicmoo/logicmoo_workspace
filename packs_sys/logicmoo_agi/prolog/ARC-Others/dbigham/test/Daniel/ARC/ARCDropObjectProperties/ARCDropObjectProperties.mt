(*
    Tests for: Daniel`ARC`ARCDropObjectProperties
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCDropObjectProperties]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ARCDropObjectProperties[
        {<|"Input" -> <|"Objects" -> {<|"X" -> 1, "XInverse" -> 2|>}|>|>},
        {"XInverse"}
    ]
    ,
    {<|"Input" -> <|"Objects" -> {<|"X" -> 1|>}|>|>}
    ,
    TestID -> "ARCDropObjectProperties-20220903-VA97FL"
]