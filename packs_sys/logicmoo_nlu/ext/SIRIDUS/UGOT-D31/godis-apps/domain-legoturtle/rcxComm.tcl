#------------------------------------------------------------------------------
# rcxComm - Wrappers for the communications routines
# Revision History
#
# R. Hempel 2002-04-05 - Original
#------------------------------------------------------------------------------

package provide rcxComm 2.0
              
lappend auto_path {.}

package require rcxOption    2.0

switch -regexp $tcl_platform(platform) {
  [Ww]indows   { package require rcxWinComm  2.0 }
  [Uu]nix      { package require rcxUnixComm 2.0 }
  [Mm]acintosh { package require rcxMacComm  2.0 }
}

namespace eval ::rcxComm:: {
  variable portID     ""
  variable portName   ""
  variable portBaud   ""
  variable portParity ""
  
  variable sentBytesVisible 1

  variable cmdPortList  ""
  variable cmdPortOpen  ""
  variable cmdPortClose ""
  variable cmdPortRead  ""
  variable cmdPortWrite ""    
  
  variable sPortError "Set tower port using port menu!"
}

#-----------------------------------------------------------------------------
# proc ::rcxComm::setPort { }
#-----------------------------------------------------------------------------

proc ::rcxComm::setPort { name } {

  puts $name
  
  catch { ::rcxComm::portClose "$::rcxComm::portID" }
  
  switch -regexp $::tcl_platform(platform) {
     [Ww]indows   { ::rcxWinComm::setPort   $name }
     [Mm]acintosh { ::rcxMacComm::setPort   $name }
     [Uu]nix      { ::rcxUnixComm::setPort $name }
  }
  
::rcxtk::Console 
}


#-----------------------------------------------------------------------------
# proc ::rcxComm::portList { }
#-----------------------------------------------------------------------------

proc ::rcxComm::portList {} {

  if { "" == $::rcxComm::cmdPortList } {
    ::rcxutil::showLog $::rcxComm::sPortError
  } else {
    eval $::rcxComm::cmdPortList
  }
}

#-----------------------------------------------------------------------------
# proc ::rcxComm::portOpen { name baud parity }
#-----------------------------------------------------------------------------

proc ::rcxComm::portOpen { name baud parity } {

  if { "" == $name } {
    ::rcxutil::showLog $::rcxComm::sPortError
    return ""
  }

  catch [::rcxComm::portClose $::rcxComm::portID]

  if { "" == $::rcxComm::cmdPortOpen } {
    ::rcxutil::showLog $::rcxComm::sPortError
  } else {
  
    puts "->$name<->$baud<->$parity<-"

    eval $::rcxComm::cmdPortOpen {"$name"} $baud $parity
  }
}

#-----------------------------------------------------------------------------
# proc ::rcxComm::portClose { portID }
#-----------------------------------------------------------------------------

proc ::rcxComm::portClose { portID } {

  if { "" == $portID } {
    ::rcxutil::showLog $::rcxComm::sPortError
    return ""
  }
    
  if { "" == $::rcxComm::cmdPortClose } {
    ::rcxutil::showLog $::rcxComm::sPortError
  } else {
    eval $::rcxComm::cmdPortClose $portID
  }
}

#-----------------------------------------------------------------------------
# proc ::rcxComm::portRead { portID }
#-----------------------------------------------------------------------------

proc ::rcxComm::portRead { portID } {

  if { "" == $portID } {
    ::rcxutil::showLog $::rcxComm::sPortError
    return ""
  }
    
  if { "" == $::rcxComm::cmdPortRead } {
    ::rcxutil::showLog $::rcxComm::sPortError
  } else {
    eval $::rcxComm::cmdPortRead $portID
  }
}

#-----------------------------------------------------------------------------
# proc ::rcxComm::portWrite { portID data }
#-----------------------------------------------------------------------------

proc ::rcxComm::portWrite { portID data } {

  if { "" == $portID } {
    ::rcxutil::showLog $::rcxComm::sPortError
    return ""
  }

  if { "" == $::rcxComm::cmdPortWrite } {
    ::rcxutil::showLog $::rcxComm::sPortError
  } else {
    eval {$::rcxComm::cmdPortWrite $portID "$data"}
  }

}

#-----------------------------------------------------------------------------
