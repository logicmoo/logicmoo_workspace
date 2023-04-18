(*
    Tests for: Daniel`ARC`ARCExamplesSegmentedByRowOrColumn
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCExamplesSegmentedByRowOrColumn]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ARCExamplesSegmentedByRowOrColumn[
        Daniel`ARC`ARCParseInputAndOutputScenes[
            Daniel`ARC`ARCParseFile["1e0a9b12"]["Train"]
        ]
    ]
    ,
    "Columns"
    ,
    TestID -> "ARCExamplesSegmentedByRowOrColumn-20221029-UHECL2"
]