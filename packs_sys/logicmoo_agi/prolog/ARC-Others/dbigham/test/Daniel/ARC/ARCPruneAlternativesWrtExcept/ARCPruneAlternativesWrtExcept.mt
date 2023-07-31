(*
    Tests for: Daniel`ARC`ARCPruneAlternativesWrtExcept
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCPruneAlternativesWrtExcept]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ARCPruneAlternativesWrtExcept[1 | Except[1 | 2] | Except[3 | 4]]
    ,
    Except[2 | 3 | 4]
    ,
    TestID -> "ARCPruneAlternativesWrtExcept-20220819-BM8LOM"
]

Test[
    Daniel`ARC`ARCPruneAlternativesWrtExcept[Alternatives[1]]
    ,
    1
    ,
    TestID -> "ARCPruneAlternativesWrtExcept-20220819-D75H2K"
]

Test[
    Daniel`ARC`ARCPruneAlternativesWrtExcept[Alternatives[Except[<|"Name" -> "Pixel"|>]]]
    ,
    Except[<|"Name" -> "Pixel"|>]
    ,
    TestID -> "ARCPruneAlternativesWrtExcept-20220819-180VBZ"
]