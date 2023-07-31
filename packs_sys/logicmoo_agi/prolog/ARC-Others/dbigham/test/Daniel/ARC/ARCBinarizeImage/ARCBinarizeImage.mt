(*
    Tests for: Daniel`ARC`ARCBinarizeImage
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCBinarizeImage]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ARCBinarizeImage[Daniel`ARC`ARCScene[{{1, 2, 3}, {0, 0, 2}}], 0]
    ,
    {{1, 1, 1}, {0, 0, 1}}
    ,
    TestID -> "ARCBinarizeImage-20220925-1PTH6R"
]