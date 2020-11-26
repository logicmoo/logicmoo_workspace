#------------------------------------------------------------------------------
# rcxwin-usb - Routines that use the windows USB port for serial commas
# Revision History
#
# R. Hempel 2002-07-06 - Fix error in portOpen that caused bad error message
# R. Hempel 2002-03-26 - Original
#------------------------------------------------------------------------------

package provide rcxWinCommUSB 2.0

namespace eval ::rcxWinCommUSB:: {
}

#-----------------------------------------------------------------------------
# proc ::rcxWinCommUSB::setPort { name }
#-----------------------------------------------------------------------------

proc ::rcxWinCommUSB::setPort { name } {
  set ::rcxComm::cmdPortList  {::rcxWinCommUSB::portList }
  set ::rcxComm::cmdPortOpen  {::rcxWinCommUSB::portOpen }
  set ::rcxComm::cmdPortClose {::rcxWinCommUSB::portClose}
  set ::rcxComm::cmdPortRead  {::rcxWinCommUSB::portRead }
  set ::rcxComm::cmdPortWrite {::rcxWinCommUSB::portWrite}

  set ::rcxComm::sentBytesVisible  1
  set ::rcxOption::towerPort $name
}

#-----------------------------------------------------------------------------
# proc ::rcxWinCommUSB::portList { }
#-----------------------------------------------------------------------------

proc ::rcxWinCommUSB::portList {} {
  return [list "//./LEGOTOWER1"]
}

#-----------------------------------------------------------------------------
# proc ::rcxWinCommUSB::portOpen { name baud parity }
#-----------------------------------------------------------------------------

proc ::rcxWinCommUSB::portOpen { name baud parity } {
  set ::rcxComm::portName   $name
  set ::rcxComm::portBaud   $baud
  set ::rcxComm::portParity $parity
  
  set ::rcxComm::sentBytesVisible 0

  catch { close $::rcxComm::portID }

  if { [ catch { open $name RDWR } portID ] } {
    ::rcxutil::showError "Could not open $::rcxComm::portName"
    return [set ::rcxComm::portID ""]
  } else {
    set ::rcxComm::portID $portID
  }

  if { [ catch { fconfigure $::rcxComm::portID         \
                          -blocking 0                  \
                          -translation {binary binary} \
                          -buffersize 1024             \
                          -eofchar {}                  \
                          -buffering none } result ] } {

     ::rcxutil::showError "Could not configure $::rcxComm::portID"
     return ""
  }
}

#-----------------------------------------------------------------------------
# proc ::rcxWinCommUSB::portClose { portID }
#-----------------------------------------------------------------------------

proc ::rcxWinCommUSB::portClose { portID } {

  catch { close $portID }
}

#-----------------------------------------------------------------------------
# proc ::rcxWinCommUSB::portRead { portID }
#-----------------------------------------------------------------------------

proc ::rcxWinCommUSB::portRead { portID } {
  if { [catch { read $portID } inData] } {
    set inData ""
  }
  return $inData
}

#-----------------------------------------------------------------------------
# proc ::rcxWinCommUSB::portWrite { portID data }
#-----------------------------------------------------------------------------

proc ::rcxWinCommUSB::portWrite { portID data } {

  puts -nonewline $portID $data
  flush $portID
}

#-----------------------------------------------------------------------------
