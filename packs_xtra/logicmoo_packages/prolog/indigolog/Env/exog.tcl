set id 0
label .l_exog  -text "Exogenous Event:"
label .l_hist  -text "Past events:"
entry .entry_exog -width 30 -relief sunken -textvariable cmd
button .b_exit -text "Exit" -command { exit }
pack .l_exog -padx 1m -pady 1m
pack .entry_exog -padx 1m -pady 1m
pack .b_exit -padx 1m -pady 1m
pack .l_hist -padx 1m -pady 1m


bind .entry_exog <Return> {
set id [expr $id + 1]
if {$id > 10} {
destroy .b[expr $id - 5]
}
button .b$id -command "puts stdout $cmd." \
-text $cmd
pack .b$id -fill x
.b$id invoke
.entry_exog delete 0 end
}



