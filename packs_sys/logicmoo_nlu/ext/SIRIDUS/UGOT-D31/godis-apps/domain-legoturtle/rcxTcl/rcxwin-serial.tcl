#------------------------------------------------------------------------------
# rcxwin-serial - Routines that use the windows serial for serial coms
# Revision History
#
# R. Hempel 26Oct01 - Original
#------------------------------------------------------------------------------

package provide rcxWinCommSerial 2.0

namespace eval ::rcxWinCommSerial:: {
}

#-----------------------------------------------------------------------------
# proc ::rcxWinCommSerial::setPort { name }
#-----------------------------------------------------------------------------

proc ::rcxWinCommSerial::setPort { name } {
  set ::rcxComm::cmdPortList  {::rcxWinCommSerial::portList }
  set ::rcxComm::cmdPortOpen  {::rcxWinCommSerial::portOpen }
  set ::rcxComm::cmdPortClose {::rcxWinCommSerial::portClose}
  set ::rcxComm::cmdPortRead  {::rcxWinCommSerial::portRead }
  set ::rcxComm::cmdPortWrite {::rcxWinCommSerial::portWrite}

  set ::rcxComm::sentBytesVisible  1
  set ::rcxOption::towerPort $name
  }

#-----------------------------------------------------------------------------
# proc ::rcxWinCommSerial::portList { }
#-----------------------------------------------------------------------------

proc ::rcxWinCommSerial::portList {} {
  return [list com1: com2: com3: com4:]
}

#-----------------------------------------------------------------------------
# proc ::rcxWinCommSerial::portOpen { name baud parity }
#-----------------------------------------------------------------------------

proc ::rcxWinCommSerial::portOpen { name baud parity } {
  set ::rcxComm::portName   $name
  set ::rcxComm::portBaud   $baud
  set ::rcxComm::portParity $parity

  set ::rcxComm::sentBytesVisible 1

  catch { close $::rcxComm::portID }

  if { [ catch { open $name RDWR } portID ] } {
    ::rcxutil::showError "Could not open $::rcxComm::portName"
    return [set ::rcxComm::portID ""]
  } else {
    set ::rcxComm::portID $portID
  }

  if { [ catch { fconfigure $::rcxComm::portID         \
                          -mode "$baud,$parity,8,1"    \
                          -blocking 0                  \
                          -translation {binary binary} \
                          -buffersize 1024             \
                          -eofchar {}                  \
                          -buffering none } result ] } {

     ::rcxutil::showError "Could not configure $::rcxComm::portID"
     return [set ::rcxComm::portID ""]
  }
}

#-----------------------------------------------------------------------------
# proc ::rcxWinCommSerial::portClose { portID }
#-----------------------------------------------------------------------------

proc ::rcxWinCommSerial::portClose { portID } {

 catch { close $::rcxComm::portID }
}

#-----------------------------------------------------------------------------
# proc ::rcxWinCommSerial::portRead { portID }
#-----------------------------------------------------------------------------

proc ::rcxWinCommSerial::portRead { portID } {
  if { [catch { read $portID } inData] } {
    set inData ""
  }
  return $inData
}

#-----------------------------------------------------------------------------
# proc ::rcxWinCommSerial::portWrite { portID data }
#-----------------------------------------------------------------------------

proc ::rcxWinCommSerial::portWrite { portID data } {

  puts -nonewline $portID $data
  flush $portID
}

#-----------------------------------------------------------------------------