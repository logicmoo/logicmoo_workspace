(*
    Tests for: Daniel`ARC`ARCFindPropertyToInferListValues
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCFindPropertyToInferListValues]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ARCFindPropertyToInferListValues[
        {"Shape", "Fill"},
        <|"Color" -> {2, 9, 9, 9, 3, 3}|>,
        {{2, 4}, {9, 4}, {9, 4}, {9, 4}, {3, 4}, {3, 4}}
    ]
    ,
    {Daniel`ARC`ObjectValue[Daniel`ARC`Private`TODO, "Color"], 4}
    ,
    TestID -> "ARCFindPropertyToInferListValues-20221011-DWHW0K"
]