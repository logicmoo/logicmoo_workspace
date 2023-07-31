(*
    Tests for: Daniel`ARC`PropertyEntityType
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`PropertyEntityType]
    
    Author: danielb
*)

Test[
    Daniel`ARC`PropertyEntityType["Shapes"]
    ,
    "Shape"
    ,
    TestID -> "PropertyEntityType-20220810-NWJYJS"
]

Test[
    Daniel`ARC`PropertyEntityType["Colors"]
    ,
    "Color"
    ,
    TestID -> "PropertyEntityType-20220810-ZGMTJL"
]

Test[
    Daniel`ARC`PropertyEntityType["Width"]
    ,
    "Integer"
    ,
    TestID -> "PropertyEntityType-20220810-CY1PPE"
]