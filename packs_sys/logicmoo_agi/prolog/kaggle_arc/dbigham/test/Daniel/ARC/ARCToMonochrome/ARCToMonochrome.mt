(*
    Tests for: Daniel`ARC`ARCToMonochrome
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCToMonochrome]
    
    Author: danielb
*)

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCToMonochrome[Daniel`ARC`ARCScene[{{2, 0}, {0, 2}}], 0]
    ]
    ,
    Daniel`ARC`ARCScene[{{10, 0}, {0, 10}}]
    ,
    TestID -> "ARCToMonochrome-20220725-K4RMMA"
]

Test[
    Daniel`ARC`ARCToMonochrome[{{2, 0}, {0, 2}}, 0]
    ,
    {{10, 0}, {0, 10}}
    ,
    TestID -> "ARCToMonochrome-20220811-JBK20L"
]