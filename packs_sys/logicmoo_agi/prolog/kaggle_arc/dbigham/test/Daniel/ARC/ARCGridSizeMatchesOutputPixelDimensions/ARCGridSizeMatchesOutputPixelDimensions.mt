(*
    Tests for: Daniel`ARC`ARCGridSizeMatchesOutputPixelDimensions
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCGridSizeMatchesOutputPixelDimensions]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ARCGridSizeMatchesOutputPixelDimensions[
        Daniel`ARC`ARCParseInputAndOutputScenes[
            Daniel`ARC`ARCParseFile["6773b310"]["Train"]
        ]
    ]
    ,
    True
    ,
    TestID -> "ARCGridSizeMatchesOutputPixelDimensions-20220910-S4VBKW"
]