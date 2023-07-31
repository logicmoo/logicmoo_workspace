(*
    Tests for: Daniel`ARC`ARCColorToInteger
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCColorToInteger]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ARCColorToInteger["Black"]
    ,
    0
    ,
    TestID -> "ARCColorToInteger-20220718-AM5YVM"
]

Test[
    Daniel`ARC`ARCColorToInteger[0]
    ,
    0
    ,
    TestID -> "ARCColorToInteger-20220718-SPCQUU"
]