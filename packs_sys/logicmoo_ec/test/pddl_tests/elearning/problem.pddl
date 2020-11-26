(define (problem midominio)
(:domain midominio)
(:customization
 (= :time-format "%d/%m/%Y %H:%M")
(= :time-horizon-relative 10000)
(= :time-start "21/09/2009 8:00")
(= :time-unit :hours)
)

(:objects
 Emilio JAB Storres Arturo FMoreno Miguel - participant
)
(:init
(value Optimize true)
(belongs_to_lane Emilio Training)
(belongs_to_lane JAB GraphicDesign)
(belongs_to_lane Storres Authoring)
(belongs_to_lane Arturo Administration)
(belongs_to_lane FMoreno Quality)
(belongs_to_lane Miguel Formatting)
)
(:tasks-goal
:tasks(
(BlockSB2)
))

)