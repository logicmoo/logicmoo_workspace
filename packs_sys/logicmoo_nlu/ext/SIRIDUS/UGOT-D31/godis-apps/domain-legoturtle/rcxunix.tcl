#------------------------------------------------------------------------------
# rcxlinux - Routines that use the linux serial for serial commas
# Revision History
#
# R. Hempel 26Oct01 - Original
#------------------------------------------------------------------------------

package provide rcxUnixComm 2.0

namespace eval ::rcxUnixComm:: {
  variable type "serial"
}

#-----------------------------------------------------------------------------
# proc ::rcxUnixComm::setPort { name }
#-----------------------------------------------------------------------------

proc ::rcxUnixComm::setPort { name } {

  set ::rcxComm::cmdPortList  {::rcxUnixComm::portList }
  set ::rcxComm::cmdPortOpen  {::rcxUnixComm::portOpen }
  set ::rcxComm::cmdPortClose {::rcxUnixComm::portClose}
  set ::rcxComm::cmdPortRead  {::rcxUnixComm::portRead }
  set ::rcxComm::cmdPortWrite {::rcxUnixComm::portWrite}

  set ::rcxComm::sentBytesVisible  1
  set ::rcxOption::towerPort $name
}

#-----------------------------------------------------------------------------
# proc ::rcxUnixComm::portList { }
#-----------------------------------------------------------------------------

proc ::rcxUnixComm::portList {} {
  return [list [glob /dev/ttyS?]]
}

#-----------------------------------------------------------------------------
# proc ::rcxUnixComm::portOpen { name baud parity }
#-----------------------------------------------------------------------------

proc ::rcxUnixComm::portOpen { name baud parity } {
  set ::rcxComm::portName   $name
  set ::rcxComm::portBaud   $baud
  set ::rcxComm::portParity $parity
  
  set ::rcxComm::sentBytesVisible 1

  catch { close ::rcxComm::portID }

  if { [ catch { open $name RDWR } portID ] } {
    ::rcxutil::showError "Could not open $::rcxComm::portName"
    return [set ::rcxComm::portID ""]
  } else {
    set ::rcxComm::portID $portID
  }

  if { [ catch { fconfigure $::rcxComm::portID          \
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
# proc ::rcxUnixComm::portClose { portID }
#-----------------------------------------------------------------------------

proc ::rcxUnixComm::portClose { portID } {

 catch { close $::rcxComm::portID }
}

#-----------------------------------------------------------------------------
# proc ::rcxUnixComm::portRead { portID }
#-----------------------------------------------------------------------------

proc ::rcxUnixComm::portRead { portID } {
  if { [catch { read $portID } inData] } {
    set inData ""
  }
  return $inData
}

#-----------------------------------------------------------------------------
# proc ::rcxUnixComm::portWrite { portID data }
#-----------------------------------------------------------------------------

proc ::rcxUnixComm::portWrite { portID data } {

  puts -nonewline $portID $data
  flush $portID
}

#-----------------------------------------------------------------------------
