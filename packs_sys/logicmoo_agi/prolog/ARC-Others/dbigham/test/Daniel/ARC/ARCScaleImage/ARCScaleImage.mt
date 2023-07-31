(*
    Tests for: Daniel`ARC`ARCScaleImage
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCScaleImage]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ARCScene[Daniel`ARC`ARCScaleImage[{{1, -1, 1}, {-1, 1, -1}, {-1, -1, 1}}, 2]]
    ,
    Daniel`ARC`ARCScene[
        {
            {1, 1, -1, -1, 1, 1},
            {1, 1, -1, -1, 1, 1},
            {-1, -1, 1, 1, -1, -1},
            {-1, -1, 1, 1, -1, -1},
            {-1, -1, -1, -1, 1, 1},
            {-1, -1, -1, -1, 1, 1}
        }
    ]
    ,
    TestID -> "ARCScaleImage-20220903-3PUXEM"
]

Test[
    Daniel`ARC`ARCScene[Daniel`ARC`ARCScaleImage[{{1, -1, 1}, {-1, 1, -1}, {-1, -1, 1}}, 3]]
    ,
    Daniel`ARC`ARCScene[
        {
            {1, 1, 1, -1, -1, -1, 1, 1, 1},
            {1, 1, 1, -1, -1, -1, 1, 1, 1},
            {1, 1, 1, -1, -1, -1, 1, 1, 1},
            {-1, -1, -1, 1, 1, 1, -1, -1, -1},
            {-1, -1, -1, 1, 1, 1, -1, -1, -1},
            {-1, -1, -1, 1, 1, 1, -1, -1, -1},
            {-1, -1, -1, -1, -1, -1, 1, 1, 1},
            {-1, -1, -1, -1, -1, -1, 1, 1, 1},
            {-1, -1, -1, -1, -1, -1, 1, 1, 1}
        }
    ]
    ,
    TestID -> "ARCScaleImage-20220903-T754SJ"
]

Test[
    Daniel`ARC`ARCScene[
        Daniel`ARC`ARCScaleImage[
            Daniel`ARC`ARCScaleImage[{{1, -1, 1}, {-1, 1, -1}, {-1, -1, 1}}, 2],
            0.5
        ]
    ]
    ,
    Daniel`ARC`ARCScene[{{1, -1, 1}, {-1, 1, -1}, {-1, -1, 1}}]
    ,
    TestID -> "ARCScaleImage-20220903-IZOR8I"
]

Test[
    Daniel`ARC`ARCScaleImage[Daniel`ARC`ARCScene[{{1, -1, 1}, {-1, 1, -1}, {-1, -1, 1}}], 2]
    ,
    Daniel`ARC`ARCScene[
        {
            {1, 1, -1, -1, 1, 1},
            {1, 1, -1, -1, 1, 1},
            {-1, -1, 1, 1, -1, -1},
            {-1, -1, 1, 1, -1, -1},
            {-1, -1, -1, -1, 1, 1},
            {-1, -1, -1, -1, 1, 1}
        }
    ]
    ,
    TestID -> "ARCScaleImage-20220903-BBVQZ9"
]