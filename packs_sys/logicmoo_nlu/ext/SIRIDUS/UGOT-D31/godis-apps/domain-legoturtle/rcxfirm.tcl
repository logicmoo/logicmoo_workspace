#------------------------------------------------------------------------------
# rcxfirm.tcl - The script that manages the data manipulation for the RCX
#
# Revision History
#
# R. Hempel 25Jun99 - Original
# R. Hempel 18Jul99 - Break apart into tcl and tk components
#                   - Use binary strings better
# R. Hempel 27Jul99 - Improved guiFeedback operation
# R. Hempel 01Aug99 - Added dummy parameter to rcxNullCmd
# R. Hempel 16Mar00 - Reorganize for cross-platform distribution
# R. Hempel 12May00 - Use channels
# R. Hempel 20Oct01 - Added rvcHandler instead of filevent
#------------------------------------------------------------------------------

lappend auto_path {.}

package require hexrec  2.0
package require rcxutil 2.0

package provide rcxfirm 2.0

namespace eval ::rcxfirm:: {

  variable portID          ""
  variable rcvHandlerAID   0
  variable rcvHandlerCount 0
  variable inMsg            
  variable inLen
  variable rcvEvent
  variable imgIdx
  variable complementBytes  1
  variable maxBytes

  variable fastload
}

set ::rcxfirm::fastload "\
S11880007906000F6B86EE807906EE646DF67906EE745E003BCC\n\
S11880159A0B877906EE5E5E0006887FD87250FE673ED918EED4\n\
S118802A6A8EEF51FE026A8EEF06FE0D6A8EEE5E5470446F2032\n\
S118803F796F7520627974652C207768656E2049206B6E6F63C5\n\
S10780546B3F00007A\n\
S90380007C"

#------------------------------------------------------------------------------
# proc ::rcxfirm::rcvHandler { }
#
# The firmware receive handler runs every 50 msec and will check the comm port
# for new characters. If there are no new characters after at most 200 msec,
# then the rcvEvent is set to TIMEOUT.
#
# If all of the expected characters are received, then the rcvEvent is set
# to RXDONE.
#
# Finally, if more than the expected characters are received, then the rcvEvent
# is set to RXOVERRUN.
#------------------------------------------------------------------------------

proc ::rcxfirm::rcvHandler { } {

  #---------------------------------------------------------------------------
  # Reset the rcv character timer to 150 milliseconds
  
  after cancel $::rcxfirm::rcvHandlerAID

  #---------------------------------------------------------------------------
  # Read any available chars into a local string first

  set inData [ rcxComm::portRead $::rcxComm::portID ]
  
  if { 0 == [string length $inData] } {
    if { 4 == [incr rcxfirm::rcvHandlerCount] } {
      set rcxfirm::rcvEvent TIMEOUT
      return
    }
  } else {
    set rcxfirm::rcvHandlerCount 0
  }    
   
  if { $rcxfirm::inLen <= [string length $rcxfirm::inMsg] } {
    set rcxfirm::rcvEvent RXOVERRUN 
    return
  }     

  append rcxfirm::inMsg $inData

  #---------------------------------------------------------------------------
  # This lets us know when we are done

  if { $rcxfirm::inLen <= [string length $rcxfirm::inMsg] } {
    set rcxfirm::rcvEvent RXDONE 
    return
  }

  set rcxfirm::rcvEvent RXDATA

  set ::rcxfirm::rcvHandlerAID [after 50 {::rcxfirm::rcvHandler}]
}

#------------------------------------------------------------------------------
# proc rcxfirm::sendMsg { msg rlen }
#
# sendMsg takes binary string and sends it out the serial port. The number
# of bytes expected in the reply is passed as well. The number of reply bytes
# expected is used to determine whenthe reply is done, instead of waiting
# for the full timeout.
#
# The return value is the binary string received from the serial port
#------------------------------------------------------------------------------
  
proc rcxfirm::sendMsg { msg rlen } {

  set rcxfirm::inMsg    ""
  set rcxfirm::inLen    $rlen
  set rcxfirm::rcvEvent SENDING

  set ::rcxfirm::rcvHandlerAID [after 50 {::rcxfirm::rcvHandler}]

  rcxComm::portWrite $::rcxComm::portID $msg
  
  while { 0 == [regexp {TIMEOUT|RXDONE|EOF} $rcxfirm::rcvEvent] } {
    vwait rcxfirm::rcvEvent
  }

  #---------------------------------------------------------------------------
  # Here's where we actually have a response from the RCX, or we have timed
  # out. In any case, the caller is waiting for us to return the binary string
  # that was sent by the RCX. Remember to strip out the chars that are simply
  # echoes from the tower itself.

  binary scan $rcxfirm::inMsg H* inString

  puts "$rcxfirm::rcvEvent -> $rlen -> $inString"

  return $inString
}

#------------------------------------------------------------------------------
# proc rcxfirm::formatMsg { s }
#
# Formats an RCX message from a string of characters to be sent. The message
# is ready to go, all it needs is to have each character inverted and
# appended. The prefix string is added next and then the whole ASCII string
# is converted to binary.
#
# This is an example of really cool data shimmering and assumes that if the
# input got this far...its OK
#------------------------------------------------------------------------------

proc rcxfirm::formatMsg { s } {
  append s [format "%02x" [expr [::hexrec::hexStr8Sum $s] % 256]]

  if { 1 == $::rcxfirm::complementBytes } { 
    regsub -all -nocase ($::hexrec::hex2) $s {\1[format %02x [expr (0x\1^0xFF)]]} s
  }
  return "55ff00[subst $s]"
}

#------------------------------------------------------------------------------
# proc rcxfirm::formatReply { s }
#
# Formats an RCX reply string. It's almost the same as the message, just the
# checksum is omitted...
#------------------------------------------------------------------------------

proc rcxfirm::formatReply { s } {
  if { 1 == $::rcxfirm::complementBytes } { 
    regsub -all -nocase ($::hexrec::hex2) $s {\1[format %02x [expr (0x\1^0xFF)]]} s
  }
  return "55ff00[subst $s]"
}

#-----------------------------------------------------------------------------
# proc rcxfirm::sendRCXMsg { sData sReply rLen retry }
#  
# This routine is the interface to the RCX sending mechanism. It
# expects the string already formatted including the checksum, the reply
# string, and the number of retries to execute.
#
# The outMsg is just the binary data that has to go out
# The rLen is the expected length of reply data, which is the original
# message, and the reply
#
# If the message is transmitted and received correctly within the specified
# number of retries, then it returns 1, otherwise 0.
#-----------------------------------------------------------------------------

proc rcxfirm::sendRCXMsg { sData sReply rLen retry } {

  set sData  [rcxfirm::formatMsg   $sData ]
  set sReply [rcxfirm::formatReply $sReply]

  if { 1 == $::rcxComm::sentBytesVisible } {
    if { 1 == $::rcxfirm::complementBytes } {
      set rLen [expr [string length ${sData}${sReply}]/2 + $rLen ]
    } else {
      set rLen [expr [string length ${sData}${sReply}]/2 + $rLen ]
    }
  } else {
    if { 1 == $::rcxfirm::complementBytes } {
      set rLen [expr [string length ${sReply}]/2 + $rLen ]
    } else {
      set rLen [expr [string length ${sReply}]/2 + $rLen ]
    }
  }

  set outMsg [binary format H* $sData]

  for { set idx 0 } { $idx < $retry } { incr idx } {
    if { 1 == [regexp -nocase (${sReply}) [rcxfirm::sendMsg $outMsg $rLen] ] } {
      return 1
    } else {
      continue;
    }
  }
  return 0
}

#-----------------------------------------------------------------------------
# proc rcxfirm::sendDeleteFirmwareMsg { }
#  
#-----------------------------------------------------------------------------

proc rcxfirm::sendDeleteFirmwareMsg { } {
  return [rcxfirm::sendRCXMsg "65010305070B" "9A" 2 3]
#  return [rcxfirm::sendRCXMsg "65010305070B" "00" 2 3]
}

#-----------------------------------------------------------------------------
# proc rcxfirm::sendStartDownloadMsg { dStart dSum }
#  
#-----------------------------------------------------------------------------

proc rcxfirm::sendStartDownloadMsg { dStart dSum } {

  set rcxfirm::imgIdx 0

  set dSum [expr $dSum%65536]

  set    outMsg "75"
  append outMsg [format %02x%02x [expr $dStart%256] [expr $dStart/256]]
  append outMsg [format %02x%02x [expr $dSum%256  ] [expr $dSum/256  ]]
  append outMsg "00"

  return [rcxfirm::sendRCXMsg $outMsg "8a008a" 0 3]
}

#-----------------------------------------------------------------------------
# proc rcxfirm::sendImageMsg { sData }
#  
#-----------------------------------------------------------------------------

proc rcxfirm::sendImageMsg { sData } {

  incr rcxfirm::imgIdx

  if { 1 == [ expr ( $rcxfirm::imgIdx % 2 ) ] } {
    set outMsg   "4D"
    set replyMsg "b200b2"
  } else {
    set outMsg   "45"
    set replyMsg "ba00ba"
  }
  
  set dSize [expr [string length $sData]/2]

  append outMsg [format %02x%02x [expr $rcxfirm::imgIdx%256] [expr $rcxfirm::imgIdx/256]]
  append outMsg [format %02x%02x [expr $dSize%256          ] [expr $dSize/256          ]]
  append outMsg $sData
  append outMsg [format "%02x"   [expr [::hexrec::hexStr8Sum $sData] % 256]]

  return [rcxfirm::sendRCXMsg $outMsg $replyMsg 0 5]
  }

#-----------------------------------------------------------------------------
# proc rcxfirm::sendUnlockFirmwareMsg { }
#  
#-----------------------------------------------------------------------------

proc rcxfirm::sendUnlockFirmwareMsg { } {
  
  binary scan "Just a bit off the block!" H* replyMsg

  return [rcxfirm::sendRCXMsg "A54C45474FAE" "5a${replyMsg}e8" 0 2]
}

#-----------------------------------------------------------------------------
# proc rcxfirm::uploadImage { image speed }
#
# Uses filename and baud rate to upload an image to the RCX.
#
# NOTE that this routine assumes that the SREC file you are using represents
# a contiguous memory map from 0x8000 to the end of your image, and that the
# records are in increasing address order.
#-----------------------------------------------------------------------------

proc rcxfirm::uploadImage { image speed } {
  
   set rcxfirm::maxBytes 192
  
   if { 0 != [regexp -- "fast" $speed] } {
     set rcxfirm::complementBytes 0
     ::rcxComm::portOpen $::rcxOption::towerPort 4800 n
   } else {
    set rcxfirm::complementBytes 1
     ::rcxComm::portOpen $::rcxOption::towerPort 2400 o
   }
 
   #---------------------------------------------------------------------------
   # open it and set it to the right baud rate...and raise an error if the
   # baud rate is goofy. Finally, set up a filevent to handle the readable
   # state of the serial port
 
   #---------------------------------------------------------------------------
   # Send out a dummy character and then wait about 1 second for the
   # RCX tower to warm up...
 
   rcxComm::portWrite $::rcxComm::portID [binary format H2 "00"]
   
   after 500
 
   #---------------------------------------------------------------------------
   # First, delete any firmware that may be in the RCX.
 
   ::rcxutil::showLog "Deleting Firmware..."

  if { 0 == [rcxfirm::sendDeleteFirmwareMsg] } {
    ::rcxComm::portClose $::rcxComm::portID
    ::rcxutil::showError "Could not delete firmware"
    return -1
  }
   
   #---------------------------------------------------------------------------
   # Now open and read the SREC file we're going to use...
 
   if { [ catch {::hexrec::readSREC $image } result ] } {
     ::rcxComm::portClose $::rcxComm::portID
     ::rcxutil::showErrror "Could not load image"
     return -2
   }
 
   #---------------------------------------------------------------------------
   # Now send the download initializing sequence
 
   ::rcxutil::showLog "Starting Download..."
 
   if { 0 == [rcxfirm::sendStartDownloadMsg 32768 [::hexrec::getImageSum]] } {
     ::rcxComm::portClose $::rcxComm::portID
     ::rcxutil::showErrror "Could not start download"
     return -3
   }

   #---------------------------------------------------------------------------
   # Now start sending the actual image. We'll just grab one Srecord after
   # another until they are all gone. The routine that gets image items
   # just gives us a list which includes the address, length, and sum of
   # the data

   set imgItem [::hexrec::getFirstImageItem]
   set imgData ""

   while { 3 == [llength $imgItem] } {
     foreach {sAddr sData dSum} $imgItem {
       break
     }
    
     append imgData $sData
 
     if { $::rcxfirm::maxBytes <= [string length $imgData] } {

       ::rcxutil::showLog "Sending record $::rcxfirm::imgIdx..."

       if { 0 == [rcxfirm::sendImageMsg [string range $imgData \
                                         0 [expr ($::rcxfirm::maxBytes - 1)]]] } {
         ::rcxComm::portClose $::rcxComm::portID
         ::rcxutil::showErrror "Could not send record $::rcxfirm::imgIdx"
         return -4
       }
    
     set imgData [string range $imgData $rcxfirm::maxBytes end]
     }

     set imgItem [::hexrec::getNextImageItem]
   }

   #---------------------------------------------------------------------------
   # Now send the last little bit of data left in the buffer and unlock the
   # firmware. The unlock command will always fail, so don't raise an error.
   # Don't forget to close the port when we return...

   ::rcxutil::showLog "Sending record $rcxfirm::imgIdx..."

   if { 0 == [rcxfirm::sendImageMsg [string range $imgData 0 end]] } {
     ::rcxComm::portClose $::rcxComm::portID
     ::rcxutil::showErrror"Could not send image record $idx"
     return -4
  }    

  ::rcxutil::showLog "Unlocking Firmware..."

  ::rcxfirm::sendUnlockFirmwareMsg

  ::rcxComm::portClose $::rcxComm::portID
  
  return 0
}

#-----------------------------------------------------------------------------
