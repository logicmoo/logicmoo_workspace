#------------------------------------------------------------------------------
# rcxterm.tcl - Package to handle communications with pbForth
#
# Revision History
#
# R. Hempel 17May2002 - Comment stripper is now less aggressive 
# R. Hempel 05May2001 - Original
#------------------------------------------------------------------------------

lappend auto_path {.}

package provide rcxterm 2.0

namespace eval ::rcxterm:: {

  variable rcvHandlerAID 0
  variable rcvHandlerCount 0
  variable rcvEvent ""
  variable inMsg    ""
}

#------------------------------------------------------------------------------
# proc ::rcxterm::rcvHandler { }
#
# The console receive handler runs every 50 msec and will check the comm port
# for new characters.
#------------------------------------------------------------------------------

proc ::rcxterm::rcvHandler { } {

  #---------------------------------------------------------------------------
  # Reset the rcv character timer
  
  after cancel $::rcxterm::rcvHandlerAID

  #---------------------------------------------------------------------------
  # Read any available chars into a local string first

  set inData [ rcxComm::portRead $::rcxComm::portID ]
  
  if { 0 == [string length $inData] } {
    if { 100 == [incr rcxterm::rcvHandlerCount] } {
      set rcxterm::rcvEvent TIMEOUT
      }
    if { 0 != [string length $::rcxComm::portID] } {
      set ::rcxterm::rcvHandlerAID [after 40 {::rcxterm::rcvHandler}]
    }
    return
  } else {
    set rcxterm::rcvHandlerCount 0
  }    
  
#  binary scan $inData H* s
#  puts "\"$s\" is [string length $inData] chars long"

  append rcxterm::inMsg $inData

  #---------------------------------------------------------------------------
  # This lets us know when we are done

  if { 0 != [regexp {\x11} $inData] } {
    set ::rcxterm::rcvEvent XON
  } else {
    set ::rcxterm::rcvEvent RXDATA
  }

  if { 0 != [string length $::rcxComm::portID] } {
    set ::rcxterm::rcvHandlerAID [after 40 {::rcxterm::rcvHandler}]
  }
}

#------------------------------------------------------------------------------
# proc ::rcxterm::openConsole { }
#
#------------------------------------------------------------------------------

proc ::rcxterm::openConsole { } {

  ::rcxComm::portOpen $::rcxOption::towerPort 2400 o

  set ::rcxterm::rcvHandlerAID [after 40 {::rcxterm::rcvHandler}]
  
  return 0
}

#------------------------------------------------------------------------------
# proc ::rcxterm::closeConsole {}
#
#------------------------------------------------------------------------------

proc ::rcxterm::closeConsole {} {

  after cancel $::rcxterm::rcvHandlerAID

  ::rcxComm::portClose $::rcxComm::portID
}

#------------------------------------------------------------------------------
# proc ::rcxterm::warmupConsole {}
#
#------------------------------------------------------------------------------

proc ::rcxterm::warmupConsole {} {
  rcxComm::portWrite $::rcxComm::portID [binary format H2 "00"]
   
  after 500

  set ::rcxterm::inMsg ""
}

#------------------------------------------------------------------------------
# proc ::rcxterm::putConsole { s }
#
#------------------------------------------------------------------------------

proc ::rcxterm::putConsole { s } {
  rcxComm::portWrite $::rcxComm::portID $s

  if { 0 == $::rcxComm::sentBytesVisible } {
    append rcxterm::inMsg $s
    set ::rcxterm::rcvEvent TXDATA
  }
}

#------------------------------------------------------------------------------
# proc ::rcxterm::readConsole {}
#
#------------------------------------------------------------------------------

proc ::rcxterm::readConsole {} {
  set s $::rcxterm::inMsg
  set ::rcxterm::inMsg ""
  return $s
}

#------------------------------------------------------------------------------
# proc ::rcxterm::stripComments { inLine }
#
#------------------------------------------------------------------------------

proc ::rcxterm::stripComments { inLine } {
  set outLine ""

#   while { 0 != [ string length $inLine ] } {
 
    # Get rid of leading white space, if any

    set inLine [string trimleft $inLine]

    if { [regexp {^\\ } $inLine] } {
      set inLine ""
    }
    
    if { [regexp {^\\$} $inLine] } {
      set inLine ""
    }

  set outLine $inLine
  
#     if { 0 == [string length $inLine] } {
#       continue
#     }
# 
#     # Match the next token, which is non-whitespace chars followed by space
#     
#     regexp {([^ \t]*)(.*)} $inLine match token inLine
# 
# #   switch -exact -- $token {
# #     "\\"  { set token  "" ; 
# #             set inLine "" }
# #     "("   { set token "" ;
# #             regsub {[^)]*(\)|$)} $inLine {} inLine }
# #     "S\"" { regexp {([^"]*")?(.*)} $inLine match quote inLine ;
# #             append token $quote}
# #     ".\"" { regexp {([^"]*")?(.*)} $inLine match quote inLine ;
# #             append token $quote }
# #    }
#     
#     # Ok, we're ready to add a token or quote, get rid of extra trailing
#     # spaces - remember to leave at least one!
# 
#     set token [string trimright $token]
#    
#     append outLine "$token "
#   }

  return $outLine
}

#------------------------------------------------------------------------------