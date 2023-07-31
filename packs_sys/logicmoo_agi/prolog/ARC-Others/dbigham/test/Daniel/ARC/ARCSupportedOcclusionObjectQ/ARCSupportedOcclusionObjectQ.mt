(*
    Tests for: Daniel`ARC`ARCSupportedOcclusionObjectQ
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCSupportedOcclusionObjectQ]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ARCSupportedOcclusionObjectQ[
        <|"Shape" -> <|"Name" -> "Square", "Filled" -> True|>|>
    ]
    ,
    True
    ,
    TestID -> "ARCSupportedOcclusionObjectQ-20220829-1TX0HX"
]

Test[
    Daniel`ARC`ARCSupportedOcclusionObjectQ[
        <|"Shape" -> <|"Name" -> "Square", "Filled" -> False|>|>
    ]
    ,
    False
    ,
    TestID -> "ARCSupportedOcclusionObjectQ-20220829-5RBYXC"
]

Test[
    Daniel`ARC`ARCSupportedOcclusionObjectQ[<|"Shape" -> <|"Name" -> "Line"|>|>]
    ,
    True
    ,
    TestID -> "ARCSupportedOcclusionObjectQ-20220829-V6S76J"
]

Test[
    Daniel`ARC`ARCSupportedOcclusionObjectQ[
        <|"Shape" -> <|"Name" -> "Line", "Fill" -> {1, 2}|>|>
    ]
    ,
    False
    ,
    TestID -> "ARCSupportedOcclusionObjectQ-20221013-BW87FI"
]