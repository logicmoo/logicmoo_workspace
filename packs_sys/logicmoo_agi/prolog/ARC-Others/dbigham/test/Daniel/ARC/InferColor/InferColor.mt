(*
    Tests for: Daniel`ARC`InferColor
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`InferColor]
    
    Author: danielb
*)

Test[
    Daniel`ARC`InferColor[{1}]
    ,
    1
    ,
    TestID -> "InferColor-20220809-QNIWYK"
]

Test[
    Daniel`ARC`InferColor[{1, 2}]
    ,
    Missing["NotApplicable", "MultipleColors"]
    ,
    TestID -> "InferColor-20220809-7GN3L9"
]

Test[
    Daniel`ARC`InferColor[{}]
    ,
    Missing["None"]
    ,
    TestID -> "InferColor-20220809-0EXJPW"
]

Test[
    Daniel`ARC`InferColor[<|"Colors" -> {1}|>]
    ,
    1
    ,
    TestID -> "InferColor-20220809-TRWYC0"
]

Test[
    Daniel`ARC`InferColor["Color" -> <|"Colors" -> {1}|>]
    ,
    "Color" -> 1
    ,
    TestID -> "InferColor-20220809-9X0NJQ"
]

Test[
    ToString[Daniel`ARC`InferColor["Color" -> <|"Colors" -> {1, 2}|>]]
    ,
    "Nothing"
    ,
    TestID -> "InferColor-20220809-3Z0CIR"
]