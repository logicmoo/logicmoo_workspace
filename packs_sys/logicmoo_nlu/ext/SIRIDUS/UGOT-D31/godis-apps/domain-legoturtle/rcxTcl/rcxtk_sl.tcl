#------------------------------------------------------------------------------
# rcxtk - The script that manages the user interface for the RCX
#
# Revision History
#
# R. Hempel 07Jul02 - Fixed up possible error catch in downloadXmodem
# R. Hempel 17May02 - Added "\ #include filename" parsing
# R. Hempel 27Apr02 - Re-enable comment stripping
# R. Hempel 12May00 - Added support for channels
# R. Hempel 05May00 - Modified to go with new package model
# R. Hempel 26Jul99 - Original
#------------------------------------------------------------------------------
# This little program gives us a nice interface to the rcxtcl.tcl functions.
#
# It provides a nice GUI wrapper with the following menus and functions:
#
# For more information on pbFORTH, check out the WEB at:
#
# <http://www.hempeldesigngroup.com/lego/pbForth>
#------------------------------------------------------------------------------
#!/bin/sh
# Run wish from the user's path \
exec wish -f "$0" ${1+"$@"}

eval destroy [winfo child .]

wm title . "pbForth Console 2.1.3"

bind . <Destroy> {if {"%W" == "."} { ::rcxtk::closeApplication }}

lappend auto_path {.}
# this needs to be edited by hand... don't know why it has to be there, but it
# works. SL
lappend auto_path {C:/MyCVS/godis/dist/prolog/godis/domain-legoturtle/rcxTcl}

package require rcxComm   2.0
package require rcxOption 2.0
package require rcxfirm   2.0
package require rcxterm   2.0
package require rcxutil   2.0
package require xmodem    2.0

package provide rcxtk 2.0

namespace eval ::rcxtk:: {

  variable wMenu
  variable wFrame
  variable wText
  variable wScroll
  variable wLog

  variable portName   ""
  variable portList   ""
}

#------------------------------------------------------------------------------

proc ::rcxtk::closeApplication { } {
  ::rcxComm::portClose $::rcxComm::portID
  
  set optionFileID [open "myOptions.tcl" {CREAT RDWR TRUNC}]
 
  puts $optionFileID "set ::rcxOption::scriptDir  \"$::rcxOption::scriptDir\""
  puts $optionFileID "set ::rcxOption::srecDir    \"$::rcxOption::srecDir\""
  puts $optionFileID "set ::rcxOption::xmodemDir  \"$::rcxOption::xmodemDir\""
  puts $optionFileID "set ::rcxOption::towerPort  \"$::rcxOption::towerPort\""
  
  close $optionFileID
} 

#------------------------------------------------------------------------------
# Let's set up all of the widgets we'll need here
#
# Set up the main application menu bar...

wm protocol . WM_DELETE_WINDOW { exit }

set ::rcxtk::wMenu   [menu  .menubar]
. config -menu $::rcxtk::wMenu

foreach m {File pbForth Port} {
  set $m [menu $::rcxtk::wMenu.m$m -tearoff 0]
  $::rcxtk::wMenu add cascade -label $m -menu $::rcxtk::wMenu.m$m
}

# $::rcxtk::wMenu.mFile add command -label "Options"  \
#                                   -command {::rcxtk::setOptions}

# $::rcxtk::wMenu.mFile add separator
$::rcxtk::wMenu.mFile add command  -label "Exit"             \
                                   -command { ::rcxtk::closeApplication ; exit }

$::rcxtk::wMenu.mpbForth add command -label "Upload Firmware"  \
                                     -command {::rcxtk::uploadFirmware slow}
                                     
# $::rcxtk::wMenu.mpbForth add command -label "Fastload Firmware"  \
#                                      -command {::rcxtk::uploadFirmware fast}

$::rcxtk::wMenu.mpbForth add separator
$::rcxtk::wMenu.mpbForth add command -label "Upload Script"  \
                                     -command {::rcxtk::uploadScript}
$::rcxtk::wMenu.mpbForth add separator
$::rcxtk::wMenu.mpbForth add command -label "XMODEM Download"  \
                                     -command {::rcxtk::downloadXmodem}

$::rcxtk::wMenu.mpbForth add separator
$::rcxtk::wMenu.mpbForth add command -label "Console"        \
                                     -command {::rcxtk::Console}

switch -regexp $tcl_platform(platform) {
     [Ww]indows   { set ::rcxtk::portList [::rcxWinComm::portList]   }
     [Mm]acintosh { set ::rcxtk::portList [::rcxMacComm::portList]   }
     [Uu]nix      { set ::rcxtk::portList [::rcxUnixComm::portList] }
}

foreach t $::rcxtk::portList {
  foreach p $t { 
    $::rcxtk::wMenu.mPort add command -label "$p"  \
                                      -command "::rcxComm::setPort {$p}"
  }
}

# Set up the application window contents

set ::rcxtk::wFrame  [frame .f]

set ::rcxtk::wText   [text  $::rcxtk::wFrame.t        \
  -yscrollcommand [list $::rcxtk::wFrame.yscroll set] \
  ]

switch -regexp $tcl_platform(platform) {
     [Ww]indows   { $::rcxtk::wText tag configure normal -font {courier 10} }
     [Mm]acintosh { $::rcxtk::wText tag configure normal -font {courier 10} }
     [Uu]nix      { $::rcxtk::wText tag configure normal -font {courier 12} }
}

set ::rcxtk::wScroll [scrollbar $::rcxtk::wFrame.yscroll \
  -orient vertical                                       \
  -command [list $::rcxtk::wText yview]                  \
  ]

set ::rcxtk::wLog  [entry $::rcxtk::wFrame.e \
  -state disabled                            \
  ]

grid $::rcxtk::wText $::rcxtk::wScroll -sticky news
grid $::rcxtk::wLog                    -sticky news

grid rowconfigure    $::rcxtk::wFrame 0 -weight 1
grid columnconfigure $::rcxtk::wFrame 0 -weight 1

pack $::rcxtk::wFrame -fill both -expand true

#------------------------------------------------------------------------------
# Now we can set up the commands that the menu implements here...
#-----------------------------------------------------------------------------
# proc ::rcxtk::setOptions { }
#
#-----------------------------------------------------------------------------
 
proc ::rcxtk::setOptions { } {
  tk_dialog . Options Options {} 0 {}
}

#-----------------------------------------------------------------------------
# proc ::rcxtk::uploadFirmware { speed }
#
#-----------------------------------------------------------------------------
 
proc ::rcxtk::uploadFirmware { speed } {
  
  catch {::rcxterm::closeConsole}

  set n [ tk_getOpenFile -initialdir $::rcxOption::srecDir \
                         -title "Select a pbFORTH SREC file" \
                         -filetypes {{"SREC File" {.srec}}
                                     {"SREC File" {.sre} }} ]

  set ::rcxOption::srecDir [file join [pwd] [file dirname $n]]
  
  if { [catch { open $n {RDONLY} } fileID] } {
    ::rcxtk::Console
    return -1
  }

  if { [regexp -- "fast" $speed] } {
    catch {::rcxfirm::uploadImage $::rcxfirm::fastload "slow"}
  }
  
  catch {::rcxfirm::uploadImage [read $fileID] $speed}

  catch {close $fileID}

  ::rcxtk::Console
  
  ::rcxtk::showInfo "Finished uploading firmware"
}

#-----------------------------------------------------------------------------
# proc ::rcxtk::sendScript { s }
#
#-----------------------------------------------------------------------------


proc ::rcxtk::sendScript { fileName } {
# set fileName "test.txt"
# puts $fileName
  if { [catch { open $fileName {RDONLY} } scriptID] } {
    ::rcxtk::Console
    return -1
  }
  
  # Assuming the port opened up, lets break the script into chunks we can
  # send up to the RCX
  
  set idx 0

  foreach s [split [read $scriptID] \n] {
#set s [split [read $scriptID] \n]
#puts $s
#s is a line
      set s [::rcxterm::stripComments $s]
      
      if { [regexp {^\[ \t]+#include (.*)$} $s dummy n] } {
	  puts "matched $n"
	  puts "expanding [file join [file dirname $fileName] $n]"
	  
	  ::rcxtk::sendScript [file join [pwd] [file dirname $n]]
	  
	  continue
      }
      
      append s [binary format H2 "0D"]
      
      if { 0 == [string length [string trimright $s]] } {
	  continue
      }
      
      ::rcxutil::showLog "Uploading line $idx from $fileName..."
      incr idx
      
      puts "sending $s"
      
      ::rcxterm::putConsole $s
      
      set ::rcxterm::rcvEvent RECEIVING
      
      while { 0 == [regexp {TIMEOUT|EOF|XON} $::rcxterm::rcvEvent] } {
	  vwait ::rcxterm::rcvEvent
      }
      
      set s [::rcxterm::readConsole]
      
      regsub -all {\x11} $s "" s    
      regsub -all {\x13} $s "" s    
      regsub -all {\x0d} $s "" s    
      
      $::rcxtk::wText insert end $s {normal}
      
      $::rcxtk::wText yview moveto 1.0
      $::rcxtk::wText delete 0.0 "end -500 lines"
      
  }
  
  close $scriptID
}   

#-----------------------------------------------------------------------------
# proc ::rcxtk::sendLine { s }
# Staffan Larsson 2003-07-11 sl@ling.gu.se
#-----------------------------------------------------------------------------

# example use:
# ::rcxtk::sendLine "7 3 0 MOTOR_SET"

proc ::rcxtk::sendLine { s } {

    if { [regexp {^\[ \t]+#include (.*)$} $s dummy n] } {
	puts "matched $n"
	puts "expanding [file join [file dirname $fileName] $n]"
	
	::rcxtk::sendScript [file join [pwd] [file dirname $n]]
	
	continue
    }
    
    append s [binary format H2 "0D"]
    
    if { 0 == [string length [string trimright $s]] } {
	continue
    }
    
    puts "sending $s"
    
    ::rcxterm::putConsole $s
    
    set ::rcxterm::rcvEvent RECEIVING
    
    while { 0 == [regexp {TIMEOUT|EOF|XON} $::rcxterm::rcvEvent] } {
	vwait ::rcxterm::rcvEvent
    }
    
    set s [::rcxterm::readConsole]
# puts "recieved $s"

    binary scan $s H* inString
    puts "$rcxterm::rcvEvent -> $inString"

    if { 0 != [string length $s] } {
	foreach c [split $s {}] {
	    switch -exact -- $c {
		"\b"    { $::rcxtk::wText delete "end -2 chars" end }
		"\r"    { }
		"\n"    { $::rcxtk::wText insert end $c }
		"\x11"  { }
		"\x13"  { }
		default { $::rcxtk::wText insert end $c {normal}}
	    }
	}
	$::rcxtk::wText yview moveto 1.0
	$::rcxtk::wText delete 0.0 "end -500 lines"
	$::rcxtk::wText mark set insert end
	
	update idletasks
    }

 
return $s
}

#-----------------------------------------------------------------------------
# proc ::rcxtk::uploadScript { }
#
#-----------------------------------------------------------------------------

proc ::rcxtk::uploadScript { } {
  
  set n [ tk_getOpenFile -initialdir $::rcxOption::scriptDir   \
                         -title "Select a pbFORTH Script file" \
                         -filetypes {{"TXT File" {.txt}}
                                     {"FTH File" {.fth}}
                                     {"All Files" {*}  }} ]

  set ::rcxOption::scriptDir [file join [pwd] [file dirname $n]]

  ::rcxterm::warmupConsole

  ::rcxtk::sendScript $n
    
  ::rcxtk::showInfo "Finished uploading script"
}

#-----------------------------------------------------------------------------
# proc ::rcxtk::downloadXmodem { }
#-----------------------------------------------------------------------------

proc ::rcxtk::downloadXmodem { } {
  
  catch {::rcxterm::closeConsole}

  set n [ tk_getSaveFile -initialdir $::rcxOption::srecDir   \
                         -title "Select a Download File"     \
                         -filetypes {{"SREC File" {.srec}}
                                     {"SREC File" {.sre} }
                                     {"All Files" {*}  }} ]

  set ::rcxOption::xmodemDir [file join [pwd] [file dirname $n]]

  if { [catch { open "$n.srec" {CREAT RDWR TRUNC} } fileID] } {
    ::rcxtk::Console
    return -1
  }
   
  ::xmodem::rcvFile $fileID

  close $fileID

  ::rcxtk::Console
  
  ::rcxtk::showInfo "Finished XMODEM download"
}


#------------------------------------------------------------------------------
# proc ::rcxtk::keyBind { k a }
#
# The default binding for key presses in the console window. Basically it
# ignores any key press that is not an ASCII character, and also keeps
# carriage returns from messing up the regular expression in the key fileevent
# handler
#------------------------------------------------------------------------------

proc ::rcxtk::keyBind { k a } {

  # puts "Key Binding is $k-[string length $k] $a-[string length $a]"
    
  if { 0 != [string length $a] } {
    ::rcxterm::putConsole $a
  }
}

#-----------------------------------------------------------------------------
# proc ::rcxtk::Console { }
#
#-----------------------------------------------------------------------------

proc ::rcxtk::Console { } {
    
    if { 0 != [::rcxterm::openConsole] } {
	::rcxutil::showLog "pbForth console is not ready..."
	return
    } else {
	::rcxutil::showLog "pbForth console is ready..."
    }
    
#    focus -force $::rcxtk::wText
    
    # Assuming the port opened up, let's also take care of setting up bindings
    # for all of the terminal keys.
    
    bind consoleKey <Return>    { ::rcxterm::putConsole "\r" ; break }
    bind consoleKey <BackSpace> { ::rcxterm::putConsole "\b" ; break }
    bind consoleKey <Key>       { ::rcxtk::keyBind %K %A ; break }
    
#    bindtags $::rcxtk::wText [list consoleKey Text]
    
#  while {1} {
#    vwait ::rcxterm::rcvEvent

#    set s [::rcxterm::readConsole]

#    binary scan $s H* inString
#    puts "$rcxterm::rcvEvent -> $inString"
#
#    if { 0 != [string length $s] } {
#      foreach c [split $s {}] {
#        switch -exact -- $c {
#          "\b"    { $::rcxtk::wText delete "end -2 chars" end }
#          "\r"    { }
#          "\n"    { $::rcxtk::wText insert end $c }
#          "\x11"  { }
#          "\x13"  { }
#          default { $::rcxtk::wText insert end $c {normal}}
#        }
#      }
#
#      $::rcxtk::wText yview moveto 1.0
#      $::rcxtk::wText delete 0.0 "end -500 lines"
#      $::rcxtk::wText mark set insert end

#    update idletasks
#    }
# }
}   

#-----------------------------------------------------------------------------
# proc ::rcxtk::showError { text }
#-----------------------------------------------------------------------------

proc ::rcxtk::showError { text } {

  set i [tk_dialog .showError "Error" $text {} {} {OK}]
}

#-----------------------------------------------------------------------------
# proc ::rcxtk::showInfo { text }
#-----------------------------------------------------------------------------

proc ::rcxtk::showInfo { text } {

  set i [tk_dialog .showError "Information" $text {} {} {OK}]
}

#-----------------------------------------------------------------------------
# proc ::rcxtk::showLog { text }
#-----------------------------------------------------------------------------

proc ::rcxtk::showLog { text } {

  $::rcxtk::wLog config -state normal

  $::rcxtk::wLog delete 0 end
  $::rcxtk::wLog  insert end $text

  $::rcxtk::wLog config -state disabled

  update idletasks
}

# option add *Dialog.msg.font {courier 10} userDefault
# option add *Error.msg.font  {courier 10} userDefault

::rcxutil::setShowError {::rcxtk::showError}

::rcxutil::setShowLog {::rcxtk::showLog}

# console show

::rcxComm::setPort $::rcxOption::towerPort

::rcxtk::Console

#-----------------------------------------------------------------------------
