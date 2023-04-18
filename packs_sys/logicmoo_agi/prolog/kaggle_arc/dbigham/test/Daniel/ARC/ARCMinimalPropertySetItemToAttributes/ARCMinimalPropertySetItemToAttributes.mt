(*
    Tests for: Daniel`ARC`ARCMinimalPropertySetItemToAttributes
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCMinimalPropertySetItemToAttributes]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ARCMinimalPropertySetItemToAttributes[
        "MyProperty",
        <|"MyProperty" -> <|"A" -> 1|>|>
    ]
    ,
    "MyProperty" -> <|"A" -> 1|>
    ,
    TestID -> "ARCMinimalPropertySetItemToAttributes-20220824-IHX4VW"
]

Test[
    Daniel`ARC`ARCMinimalPropertySetItemToAttributes[
        "MyProperty2",
        <|"MyProperty" -> <|"A" -> 1|>|>
    ]
    ,
    "MyProperty2" -> <||>
    ,
    TestID -> "ARCMinimalPropertySetItemToAttributes-20220824-J9JTHC"
]

Test[
    Daniel`ARC`ARCMinimalPropertySetItemToAttributes[
        "MyProperty" | "MyProperty2",
        <|"MyProperty" -> <|"A" -> 1|>|>
    ]
    ,
    ("MyProperty" -> <|"A" -> 1|>) | ("MyProperty2" -> <||>)
    ,
    TestID -> "ARCMinimalPropertySetItemToAttributes-20220824-R5K59O"
]