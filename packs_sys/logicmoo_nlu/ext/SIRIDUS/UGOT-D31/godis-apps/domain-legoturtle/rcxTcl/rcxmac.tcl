#------------------------------------------------------------------------------
# rcxmac - Routines that use the mac OSAX for serial commas
# Revision History
#
# R. Hempel 13Oct01 - Original
#------------------------------------------------------------------------------

package require Tclapplescript

package provide rcxMacComm 2.0

namespace eval ::rcxMacComm:: {
  variable type "serial"
}

#-----------------------------------------------------------------------------
# proc ::rcxMacComm::setPort { name }
#-----------------------------------------------------------------------------

proc ::rcxMacComm::setPort { name } {

  set ::rcxComm::cmdPortList  {::rcxMacComm::portList }
  set ::rcxComm::cmdPortOpen  {::rcxMacComm::portOpen }
  set ::rcxComm::cmdPortClose {::rcxMacComm::portClose}
  set ::rcxComm::cmdPortRead  {::rcxMacComm::portRead }
  set ::rcxComm::cmdPortWrite {::rcxMacComm::portWrite}

  set ::rcxComm::sentBytesVisible  1
  set ::rcxOption::towerPort $name
}

#-----------------------------------------------------------------------------
# proc ::rcxMacComm::portList { }
#-----------------------------------------------------------------------------

proc ::rcxMacComm::portList {} {
  set portList [AppleScript execute "serial port list" ]
  
  regsub -all {, *}      $portList "," portList
  regsub -all {[\{\}\"]} $portList ""  portList
  
  return [list [split $portList ","]]
}

#-----------------------------------------------------------------------------
# proc ::rcxMacComm::portOpen { name baud parity }
#-----------------------------------------------------------------------------

proc ::rcxMacComm::portOpen { name baud parity } {

  switch -- $parity {
    n   {set parity none}
    e   {set parity even}
    o   {set parity odd }
  }
  
  set ::rcxComm::portName   $name
  set ::rcxComm::portBaud   $baud
  
  set ::rcxComm::sentBytesVisible 1
  
  set ::rcxComm::portID [AppleScript execute "open serial port \"$name\" baud rate $baud parity $parity"]
  regsub -all {["]} $::rcxComm::portID ""  ::rcxComm::portID
  }

#-----------------------------------------------------------------------------
# proc ::rcxMacComm::portClose { portID }
#-----------------------------------------------------------------------------

proc ::rcxMacComm::portClose { portID } {

  set ::rcxComm::portID [AppleScript execute "close serial port \"$portID\""]
}

#-----------------------------------------------------------------------------
# proc ::rcxMacComm::portRead { portID }
#-----------------------------------------------------------------------------

proc ::rcxMacComm::portRead { portID } {
  set result [AppleScript execute "serial port read \"$portID\" as data"]
  regsub {^.data rdat(.*).} $result {\1} result
  return [binary format "H*" $result]
}

#-----------------------------------------------------------------------------
# proc ::rcxMacComm::portWrite { portID data }
#-----------------------------------------------------------------------------

proc ::rcxMacComm::portWrite { portID data } {

  binary scan $data H* value
  
  AppleScript execute "serial port write «data rdat$value» to \"$portID\""
}

#-----------------------------------------------------------------------------