(*
    Tests for: Daniel`ARC`ARCAllExamplesUseGridInInputAndOutput
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCAllExamplesUseGridInInputAndOutput]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ARCAllExamplesUseGridInInputAndOutput[
        Daniel`ARC`ARCParseInputAndOutputScenes[
            Daniel`ARC`ARCParseFile["272f95fa"]["Train"]
        ]
    ]
    ,
    True
    ,
    TestID -> "ARCAllExamplesUseGridInInputAndOutput-20220910-YYJ2LB"
]