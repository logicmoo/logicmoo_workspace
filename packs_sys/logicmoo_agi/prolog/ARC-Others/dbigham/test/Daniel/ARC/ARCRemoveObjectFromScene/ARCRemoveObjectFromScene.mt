(*
    Tests for: Daniel`ARC`ARCRemoveObjectFromScene
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCRemoveObjectFromScene]
    
    Author: danielb
*)

Test[
    ERPTesting`NormalizeOutput[
        Module[
            {parsedScene, object},
            parsedScene = Daniel`ARC`ARCParseScene[
                Daniel`ARC`ARCParseFile["28e73c20"]["Train", 1, "Output"]
            ];
            object = parsedScene[["Objects", 1]];
            Daniel`ARC`ARCRemoveObjectFromScene[
                Replace[parsedScene["Scene"], 0 -> Daniel`ARC`Private`$nonImageColor, {3}],
                object
            ]
        ]
    ]
    ,
    Daniel`ARC`ARCScene[
        {
            {-1, -1, -1, -1, -1, -1},
            {-1, -1, -1, -1, -1, -1},
            {-1, -1, -1, -1, -1, -1},
            {-1, -1, -1, -1, -1, -1},
            {-1, -1, -1, -1, -1, -1},
            {-1, -1, -1, -1, -1, -1}
        }
    ]
    ,
    TestID -> "ARCRemoveObjectFromScene-20220916-QDZARJ"
]