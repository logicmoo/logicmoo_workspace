(*
    Tests for: Daniel`ARC`ARCInferPropertiesThatRequireFullObjectList
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCInferPropertiesThatRequireFullObjectList]
    
    Author: danielb
*)

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCInferPropertiesThatRequireFullObjectList[
            {<|"Height" -> 1|>, <|"Height" -> 2|>}
        ]
    ]
    ,
    {
        <|"Height" -> 1, "Height.Rank" -> 2, "Height.InverseRank" -> 1|>,
        <|"Height" -> 2, "Height.Rank" -> 1, "Height.InverseRank" -> 2|>
    }
    ,
    TestID -> "ARCInferPropertiesThatRequireFullObjectList-20220820-2OXRIV"
]