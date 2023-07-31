(*
    Tests for: Daniel`ARC`ARCInferObjectPropertiesForRendering
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCInferObjectPropertiesForRendering]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ARCInferObjectPropertiesForRendering[<|"YInverse" -> 1|>, <|"Height" -> 10|>]
    ,
    <|"YInverse" -> 1, "Y" -> 10|>
    ,
    TestID -> "ARCInferObjectPropertiesForRendering-20220924-Y6SQ9E"
]

Test[
    Daniel`ARC`ARCInferObjectPropertiesForRendering[<|"XInverse" -> 1|>, <|"Width" -> 10|>]
    ,
    <|"XInverse" -> 1, "X" -> 10|>
    ,
    TestID -> "ARCInferObjectPropertiesForRendering-20220924-2O6P8J"
]

Test[
    Daniel`ARC`ARCInferObjectPropertiesForRendering[
        <|"Shape" -> <|"Name" -> "Pixel"|>, "Y2Inverse" -> 1|>,
        <|"Height" -> 10|>
    ]
    ,
    <|
        "Shape" -> <|"Name" -> "Pixel"|>,
        "Y2Inverse" -> 1,
        "Y2" -> 10,
        "Width" -> 1,
        "Height" -> 1,
        "Y" -> 10
    |>
    ,
    TestID -> "ARCInferObjectPropertiesForRendering-20220924-09KXNM"
]