#------------------------------------------------------------------------------
# rcxwin - Routines that use the windows serial for serial commas
# Revision History
#
# R. Hempel 26Oct01 - Original
#------------------------------------------------------------------------------

package provide rcxWinComm 2.0
             
lappend auto_path {.}

package require rcxWinCommSerial 2.0
package require rcxWinCommUSB    2.0

namespace eval ::rcxWinComm:: {
  variable type "USB"
}

#-----------------------------------------------------------------------------
# proc ::rcxWinComm::setPort { name }
#-----------------------------------------------------------------------------

proc ::rcxWinComm::setPort { name } {

  set ::rcxOption::towerPort $name
  
  switch -regexp -- $name {
    {com*}          { ::rcxWinCommSerial::setPort $name } \
    {.*LEGOTOWER.*} { ::rcxWinCommUSB::setPort    $name } \
  }
}

#-----------------------------------------------------------------------------
# proc ::rcxWinComm::portList { }
#-----------------------------------------------------------------------------

proc ::rcxWinComm::portList {} {
  return [list [::rcxWinCommSerial::portList] [::rcxWinCommUSB::portList]]
}

#-----------------------------------------------------------------------------
# proc ::rcxWinComm::portOpen { name baud parity }
#-----------------------------------------------------------------------------

proc ::rcxWinComm::portOpen { name baud parity } {
  switch -- $::rcxWinComm::type {
    serial  { ::rcxWinCommSerial::portOpen $name $baud $parity }
    USB     { ::rcxWinCommUSB::portOpen    $name $baud $parity }
  }
}

#-----------------------------------------------------------------------------
# proc ::rcxWinComm::portClose { portID }
#-----------------------------------------------------------------------------

proc ::rcxWinComm::portClose { portID } {
  switch -- $::rcxWinComm::type {
    serial  { ::rcxWinCommSerial::portClose $portID }
    USB     { ::rcxWinCommUSB::portClose    $portID }
  }
}

#-----------------------------------------------------------------------------
# proc ::rcxWinComm::portRead { portID }
#-----------------------------------------------------------------------------

proc ::rcxWinComm::portRead { portID } {
  if { [catch { read $portID } inData] } {
    set inData ""
  }
  return $inData
}

#-----------------------------------------------------------------------------
# proc ::rcxWinComm::portWrite { portID data }
#-----------------------------------------------------------------------------

proc ::rcxWinComm::portWrite { portID data } {

  puts -nonewline $portID $data
  flush $portID
}

#-----------------------------------------------------------------------------