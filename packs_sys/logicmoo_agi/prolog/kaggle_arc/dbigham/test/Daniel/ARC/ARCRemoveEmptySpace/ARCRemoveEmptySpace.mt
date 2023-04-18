(*
    Tests for: Daniel`ARC`ARCRemoveEmptySpace
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCRemoveEmptySpace]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ARCRemoveEmptySpace[
        Daniel`ARC`ARCScene[{{1, 0, 0}, {2, 0, 0}, {1, 0, 0}}],
        0
    ]
    ,
    Daniel`ARC`ARCScene[{{1}, {2}, {1}}]
    ,
    TestID -> "ARCRemoveEmptySpace-20220828-FCA25A"
]

Test[
    Daniel`ARC`ARCRemoveEmptySpace[
        Daniel`ARC`ARCScene[{{3, 4, 6}, {0, 0, 0}, {0, 0, 0}}],
        0
    ]
    ,
    Daniel`ARC`ARCScene[{{3, 4, 6}}]
    ,
    TestID -> "ARCRemoveEmptySpace-20220828-6QR2OW"
]

Test[
    Daniel`ARC`ARCRemoveEmptySpace[
        Daniel`ARC`ARCScene[
            {
                {-1, -1, -1, -1, -1, -1},
                {-1, -1, -1, -1, -1, -1},
                {-1, 5, 5, -1, -1, -1},
                {-1, 5, 5, -1, -1, -1},
                {-1, 6, 6, -1, -1, -1},
                {-1, -1, -1, -1, -1, -1},
                {-1, -1, -1, -1, -1, -1}
            }
        ],
        -1,
        "PruneLeftAndAbove" -> True
    ]
    ,
    Daniel`ARC`ARCScene[{{5, 5}, {5, 5}, {6, 6}}]
    ,
    TestID -> "ARCRemoveEmptySpace-20220902-T16Y3X"
]

Test[
    Daniel`ARC`ARCRemoveEmptySpace[
        Daniel`ARC`ARCScene[{{-1, -1, -1}, {-1, -1, -1}, {-1, -1, -1}}],
        -1,
        "PruneLeftAndAbove" -> True
    ]
    ,
    Daniel`ARC`ARCScene[{{-1, -1, -1}, {-1, -1, -1}, {-1, -1, -1}}]
    ,
    TestID -> "ARCRemoveEmptySpace-20220903-XYLKS7"
]