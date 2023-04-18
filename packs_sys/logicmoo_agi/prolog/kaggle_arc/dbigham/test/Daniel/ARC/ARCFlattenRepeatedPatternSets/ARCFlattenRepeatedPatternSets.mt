(*
    Tests for: Daniel`ARC`ARCFlattenRepeatedPatternSets
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCFlattenRepeatedPatternSets]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ARCFlattenRepeatedPatternSets[
        {
            {
                <|
                    "Type" -> "PatternFill",
                    "List" -> {
                        <|
                            "StartY" -> 1,
                            "StartX" -> 1,
                            "TrajectoryY" -> 4,
                            "TrajectoryX" -> 0
                        |>,
                        <|
                            "StartY" -> 3,
                            "StartX" -> 1,
                            "TrajectoryY" -> 4,
                            "TrajectoryX" -> 0,
                            "Transforms" -> {
                                <|"Type" -> "Rotation", "Angle" -> 180|>,
                                <|"Type" -> "Flip", "Direction" -> "Vertical"|>
                            }
                        |>
                    }
                |>
            },
            {
                <|
                    "Type" -> "PatternFill",
                    "List" -> {
                        <|
                            "StartY" -> 1,
                            "StartX" -> 1,
                            "TrajectoryY" -> 6,
                            "TrajectoryX" -> 0
                        |>,
                        <|
                            "StartY" -> 4,
                            "StartX" -> 1,
                            "TrajectoryY" -> 6,
                            "TrajectoryX" -> 0,
                            "Transforms" -> {
                                <|"Type" -> "Rotation", "Angle" -> 180|>,
                                <|"Type" -> "Flip", "Direction" -> "Vertical"|>
                            }
                        |>
                    }
                |>
            }
        }
    ]
    ,
    {
        {
            <|
                "Type" -> "PatternFill",
                "List" -> {
                    <|"StartY" -> 1, "StartX" -> 1, "TrajectoryY" -> 4, "TrajectoryX" -> 0|>,
                    <|
                        "StartY" -> 3,
                        "StartX" -> 1,
                        "TrajectoryY" -> 4,
                        "TrajectoryX" -> 0,
                        "Transform" -> <|"Type" -> "Flip", "Direction" -> "Vertical"|>
                    |>
                }
            |>,
            <|
                "Type" -> "PatternFill",
                "List" -> {
                    <|"StartY" -> 1, "StartX" -> 1, "TrajectoryY" -> 6, "TrajectoryX" -> 0|>,
                    <|
                        "StartY" -> 4,
                        "StartX" -> 1,
                        "TrajectoryY" -> 6,
                        "TrajectoryX" -> 0,
                        "Transform" -> <|"Type" -> "Flip", "Direction" -> "Vertical"|>
                    |>
                }
            |>
        }
    }
    ,
    TestID -> "ARCFlattenRepeatedPatternSets-20221007-CTJIPK"
]