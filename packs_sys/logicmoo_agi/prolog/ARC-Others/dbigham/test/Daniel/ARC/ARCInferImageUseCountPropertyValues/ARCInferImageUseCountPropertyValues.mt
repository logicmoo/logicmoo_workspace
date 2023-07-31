(*
    Tests for: Daniel`ARC`ARCInferImageUseCountPropertyValues
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCInferImageUseCountPropertyValues]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ARCInferImageUseCountPropertyValues[
        {
            <|"Image" -> "<my image>"|>,
            <|"Image" -> "<my image>"|>,
            <|"Image" -> "<my image 2>"|>
        }
    ]
    ,
    {
        <|"Image" -> "<my image>", "ImageUseCount" -> 2|>,
        <|"Image" -> "<my image>", "ImageUseCount" -> 2|>,
        <|"Image" -> "<my image 2>", "ImageUseCount" -> 1|>
    }
    ,
    TestID -> "ARCInferImageUseCountPropertyValues-20220909-NHQHI6"
]