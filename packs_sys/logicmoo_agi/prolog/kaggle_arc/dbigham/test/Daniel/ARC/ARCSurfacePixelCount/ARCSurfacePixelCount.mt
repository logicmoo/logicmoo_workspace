(*
    Tests for: Daniel`ARC`ARCSurfacePixelCount
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCSurfacePixelCount]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ARCSurfacePixelCount[{{-1, 1, -1}, {1, -1, 1}, {-1, 1, -1}}]
    ,
    4
    ,
    TestID -> "ARCSurfacePixelCount-20220905-9OA29B"
]

Test[
    Daniel`ARC`ARCSurfacePixelCount[{{1, 1, 1}, {1, -1, 1}, {1, 1, 1}}]
    ,
    8
    ,
    TestID -> "ARCSurfacePixelCount-20220905-UH3YNY"
]

Test[
    Daniel`ARC`ARCSurfacePixelCount[{{1, 1, 1}}]
    ,
    3
    ,
    TestID -> "ARCSurfacePixelCount-20220905-TWJ00T"
]

Test[
    Daniel`ARC`ARCSurfacePixelCount[{{1}, {1}, {1}}]
    ,
    3
    ,
    TestID -> "ARCSurfacePixelCount-20220905-XEO2DB"
]