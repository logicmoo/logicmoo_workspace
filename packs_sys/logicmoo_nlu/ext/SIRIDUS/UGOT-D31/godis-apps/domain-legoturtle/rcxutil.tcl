#------------------------------------------------------------------------------
# rcxutil - Utility routines for the RCX serial port, files, etc
#
# Revision History
#
# R. Hempel 12May00 - Original
#------------------------------------------------------------------------------

lappend auto_path {.}

package require rcxOption 2.0

package provide rcxutil   2.0

namespace eval ::rcxutil:: {

  variable cmdShowLog   "puts"
  variable cmdShowError "puts"
}

#------------------------------------------------------------------------------
# proc ::rcxutil::setShowLog { }
#
# proc ::rcxutil::showLog { s }
#
#------------------------------------------------------------------------------

proc ::rcxutil::setShowLog { cmd } {
  set ::rcxutil::cmdShowLog $cmd
}

proc ::rcxutil::showLog { s } {
  eval [$::rcxutil::cmdShowLog $s]
}
    
#------------------------------------------------------------------------------
# proc ::rcxutil::setShowError { }
#
# proc ::rcxutil::showError { s }
#
#------------------------------------------------------------------------------

proc ::rcxutil::setShowError { cmd } {
  set ::rcxutil::cmdShowError $cmd
}

proc ::rcxutil::showError { s } {
  $::rcxutil::cmdShowError $s
}

#-----------------------------------------------------------------------------
