(*
    Tests for: Daniel`ARC`ARCPixelsAsScenes
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCPixelsAsScenes]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ARCPixelsAsScenes[Daniel`ARC`ARCScene[{{1, 2, 3}, {4, 5, 6}, {7, 8, 9}}]]
    ,
    {
        {Daniel`ARC`ARCScene[{{1}}], Daniel`ARC`ARCScene[{{2}}], Daniel`ARC`ARCScene[{{3}}]},
        {Daniel`ARC`ARCScene[{{4}}], Daniel`ARC`ARCScene[{{5}}], Daniel`ARC`ARCScene[{{6}}]},
        {Daniel`ARC`ARCScene[{{7}}], Daniel`ARC`ARCScene[{{8}}], Daniel`ARC`ARCScene[{{9}}]}
    }
    ,
    TestID -> "ARCPixelsAsScenes-20220910-V9A3ZY"
]