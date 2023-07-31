(*
    Tests for: Daniel`ARC`ARCTaskNotesDirectory
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCTaskNotesDirectory]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ARCTaskNotesDirectory["my-task"]
    ,
    "TaskNotes/my-task"
    ,
    TestID -> "ARCTaskNotesDirectory-20220808-08LJBT"
]