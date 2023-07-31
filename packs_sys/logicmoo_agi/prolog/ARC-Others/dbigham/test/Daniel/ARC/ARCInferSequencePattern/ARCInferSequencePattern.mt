(*
    Tests for: Daniel`ARC`ARCInferSequencePattern
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCInferSequencePattern]
    
    Author: danielb
*)

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCInferSequencePattern[{1, 2, 1, 2}]
    ]
    ,
    {1, 2}
    ,
    TestID -> "ARCInferSequencePattern-20221010-FSOFFV"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCInferSequencePattern[{1, 2, 1}]
    ]
    ,
    {1, 2}
    ,
    TestID -> "ARCInferSequencePattern-20221010-BLMPL3"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCInferSequencePattern[{1, 2, 1, 2, 1, 2}]
    ]
    ,
    {1, 2}
    ,
    TestID -> "ARCInferSequencePattern-20221010-6T7VQP"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCInferSequencePattern[{1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1}]
    ]
    ,
    {1, 2}
    ,
    TestID -> "ARCInferSequencePattern-20221010-PN3W79"
]

Test[
    Daniel`ARC`ARCInferSequencePattern[{}]
    ,
    Missing["NotFound", "Patterns"]
    ,
    TestID -> "ARCInferSequencePattern-20221010-BV03ZG"
]

Test[
    Daniel`ARC`ARCInferSequencePattern[{{2, 2}, {8, 8}, {2, 2}, {8, 8}, {2, 2}, {8, 8}}]
    ,
    {{2, 2}, {8, 8}}
    ,
    TestID -> "ARCInferSequencePattern-20221023-JQIPW0"
]

Test[
    Daniel`ARC`ARCInferSequencePattern[
        {
            {1, 1, 1, 1, 1},
            {1, -1, -1, -1, 1},
            {1, -1, 1, -1, 1},
            {1, -1, -1, -1, 1},
            {1, 1, 1, 1, 1}
        }
    ]
    ,
    Missing["NotFound", "Patterns"]
    ,
    TestID -> "ARCInferSequencePattern-20221023-28V8NS"
]