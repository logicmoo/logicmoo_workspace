(*
    Tests for: Daniel`ARC`ARCRotateShapeAssociation
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCRotateShapeAssociation]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ARCRotateShapeAssociation[
        <|"Name" -> "MyShape", "Transform" -> <|"Type" -> "Rotation", "Angle" -> 90|>|>,
        90
    ]
    ,
    <|"Name" -> "MyShape", "Transform" -> <|"Type" -> "Rotation", "Angle" -> 180|>|>
    ,
    TestID -> "ARCRotateShapeAssociation-20220816-JD1H93"
]

Test[
    Daniel`ARC`ARCRotateShapeAssociation[
        <|"Name" -> "MyShape", "Transform" -> <|"Type" -> "Rotation", "Angle" -> 90|>|>,
        -90
    ]
    ,
    <|"Name" -> "MyShape"|>
    ,
    TestID -> "ARCRotateShapeAssociation-20220816-XYKF2U"
]

Test[
    Daniel`ARC`ARCRotateShapeAssociation[<|"Name" -> "MyShape"|>, 90]
    ,
    <|"Name" -> "MyShape", "Transform" -> <|"Type" -> "Rotation", "Angle" -> 90|>|>
    ,
    TestID -> "ARCRotateShapeAssociation-20220816-IAS89Q"
]

Test[
    Daniel`ARC`ARCRotateShapeAssociation[<|"Name" -> "MyShape99"|>, 90]
    ,
    <|"Name" -> "MyShape99"|>
    ,
    TestID -> "ARCRotateShapeAssociation-20220816-EAUD2G"
]