#------------------------------------------------------------------------------
# rcxoption - The script that manages the options for this package
#
# Revision History
#
# R. Hempel 14May00 - Original
#------------------------------------------------------------------------------
# Unitl I get around to setting up options dialogs, this is he place where
# you modify any defaults.
#
# Feel free to improve on or add to this mechanism...
#------------------------------------------------------------------------------

package provide rcxOption 2.0

namespace eval ::rcxOption:: {

variable scriptDir [file join /put/your/script/path/here]
variable srecDir   [file join /put/your/srec/path/here]
variable xmodemDir [file join /put/your/xmodem/path/here]

variable towerPort ""
}

if { [file exists "myOptions.tcl"] } {
  source "myOptions.tcl"
}
