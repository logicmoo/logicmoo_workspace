#------------------------------------------------------------------------------
# xmodem.tcl - The script that manages XMODEM protocol transfers
#
# Revision History
#
# R. Hempel 29Mar00 - Original master side transfer
# R. Hempel 12May00 - Converted to use pre-qualified channels and files
#------------------------------------------------------------------------------

lappend auto_path {./}

package provide xmodem 2.0

package require hexrec 2.0

namespace eval ::xmodem:: {

  variable rcvHandlerAID 0
  variable rcvHandlerCount 0
  variable rcvEvent ""
  variable inMsg    ""
}

#------------------------------------------------------------------------------
# proc ::xmodem::rcvHandler { }
#
# The xmodem receive handler runs every 50 msec and will check the comm port
# for new characters.
#------------------------------------------------------------------------------

proc ::xmodem::rcvHandler { } {

  #---------------------------------------------------------------------------
  # Reset the rcv character timer
  
  after cancel $::xmodem::rcvHandlerAID

  #---------------------------------------------------------------------------
  # Read any available chars into a local string first

  set inData [ rcxComm::portRead $::rcxComm::portID ]
  
  if { 0 == [string length $inData] } {
    incr ::xmodem::rcvHandlerCount

    if { ("RXDATA" == $::xmodem::rcvEvent) && (3 == $::xmodem::rcvHandlerCount) } {
      set xmodem::rcvEvent RXDONE
    } elseif { 100 == $::xmodem::rcvHandlerCount } {
      set xmodem::rcvEvent TIMEOUT
    }

    if { 0 != [string length $::rcxComm::portID] } {
      set ::xmodem::rcvHandlerAID [after 40 {::xmodem::rcvHandler}]
    }

    return
  } else {
    set xmodem::rcvHandlerCount 0
  }    
  
  binary scan $inData H* s
  puts "$inData \"$s\" is [string length $inData] chars long"

  append xmodem::inMsg $inData

  #---------------------------------------------------------------------------
  # This lets us know when we are done

  set ::xmodem::rcvEvent RXDATA

  if { 0 != [string length $::rcxComm::portID] } {
    set ::xmodem::rcvHandlerAID [after 40 {::xmodem::rcvHandler}]
  }
}

#-----------------------------------------------------------------------------
# proc ::xmodem::sendMsg { msg }
#  
#-----------------------------------------------------------------------------

proc ::xmodem::sendMsg { msg } {
  rcxComm::portWrite $::rcxComm::portID $msg
}

#-----------------------------------------------------------------------------
# proc ::xmodem::sendACKMsg { }
#  
#-----------------------------------------------------------------------------

proc ::xmodem::sendACKMsg { } {
  ::xmodem::sendMsg [binary format H* "06"]
}

#-----------------------------------------------------------------------------
# proc ::xmodem::sendNACKMsg { }
#  
#-----------------------------------------------------------------------------

proc ::xmodem::sendNACKMsg { } {
  ::xmodem::sendMsg [binary format H* "15"]
}

#-----------------------------------------------------------------------------
# proc ::xmodem::sendCANMsg { }
#  
#-----------------------------------------------------------------------------

proc ::xmodem::sendCANMsg { } {
  ::xmodem::sendMsg [binary format H* "18"]
}

#-----------------------------------------------------------------------------
# proc ::xmodem::sendEOTMsg { }
#  
#-----------------------------------------------------------------------------

proc ::xmodem::sendEOTMsg { } {
  ::xmodem::sendMsg [binary format H* "04"]
}

#-----------------------------------------------------------------------------
# proc ::xmodem::sendDATAMsg { data sequence }
# 
# Note: This routine assumes that data is a binary string that is not more
#       than 128 bytes long. It is padded with EOF (1A) to a length if 128
#       bytes if necessary, and truncated to 128 if longer.
#-----------------------------------------------------------------------------
# 
# proc ::xmodem::sendDATAMsg { data sequence } {
# 
#   set sLen [string length $data]
# 
#   # Pad the string to 128 chars with EOF (1A) and then truncate to 128 chars
# 
#   while { 128 > [string length $data] } {
#     append data [binary format H* "1A"]
#   }
# 
#   set data [string range $data 0 127]
# 
#   # Now build the rest of the string around the data
# 
#   binary scan $data c* bytes
#   set sum 0
#   foreach byte $bytes {
#     incr sum $byte
#   }
#   
#   set data $data[binary format c $sum ]
# 
#   set data [binary format c [ expr ($sequence^0xFF)]]$data
#   set data [binary format c         $sequence       ]$data
# 
#   ::xmodem::sendMsg [binary format H* "01"]$data
# }
# 
# #-----------------------------------------------------------------------------
# # proc ::xmodem::sndFile { fileID portID }
# #
# # Uses filename and baud rate to upload a file
# #-----------------------------------------------------------------------------
# 
# proc ::xmodem::sndFile { fileID portID } {
#   
#   fileevent $portID readable [list  ::xmodem::rcvFileEvent $portID ]
# 
#   #---------------------------------------------------------------------------
#   # Here's where the transmit part of the protocol actually happens. It waits
#   # for about 11 seconds for a NAK packet from the receiver. It is just a
#   # big state machine which waits for a packet at the top, and then decodes
#   # it and tries to do the right thing...
#   #
#   # The transmitter states are IDLE,GOT_NACK,GOT_ACK,GOT_CAN,DONE
# 
#   set fileData [read $fileID 128]
#   set state    IDLE
#   set timeout  0
#   set sequence 1
# 
#   while { 0 == [regexp {DONE} $state] } {
#     set ::xmodem::afterID  [after 11000  {set ::xmodem::rcvEvent TIMEOUT}]
# 
#     set ::xmodem::inMsg    ""
#     set ::xmodem::rcvEvent RECEIVING
# #    set ::xmodem::inLen 1
# 
#     while { 0 == [regexp {TIMEOUT|RXDONE|EOF} $::xmodem::rcvEvent] } {
#       vwait ::xmodem::rcvEvent
#     }
# 
#     if { 0 == [string compare $::xmodem::rcvEvent "TIMEOUT"] } {
#       incr timeout
#       if { 2 < $timeout } {
#         set state DONE
#       }
#       continue
#     }
#     
#     # Now decode the byte we got from the receiver and figure out what
#     # to do...
# 
#     if { 1 != [string length $::xmodem::inMsg] } {
#        continue
#     }
#      
#     binary scan $::xmodem::inMsg "H2" inString
# 
#     # If we get a NACK, resend the current frame...
# 
#     if { 0 == [string compare $inString "15"] } {
#       set timeout 0 
#       set state GOT_NACK
#       ::xmodem::sendDATAMsg $fileData $sequence
#       continue
#     }
# 
#     # If we get an ACK, get a new frame, increment the sequnce and
#     # send the data...unless there is no more to send
# 
#     if { 0 == [string compare $inString "06"] } {
#       set timeout 0 
#       if { 0 == [eof $fileID] } {
#         set state GOT_ACK
#         set fileData [read $fileID 128]
#         set sequence [expr (($sequence + 1) % 256 )]
#         ::xmodem::sendDATAMsg $fileData $sequence
#       } else {
#         ::xmodem::sendEOTMsg
#         if { 0 == [string compare $state "GOT_EOT"] } {
#           set state DONE
#         } else {
#           set state GOT_EOT
#         }
#       }
# 
#       continue
#     }
# 
#     # If we get a CAN, set the state to GOT_CAN and if we were in that state
#     # already, then set the state to DONE
# 
#     if { 0 == [string compare $inString "18"] } {
#       set timeout 0 
#       if { 0 == [string compare $state "GOT_CAN"] } {
#         set state DONE
#       } else {
#         set state GOT_CAN
#       }
#       continue
#     }
#   }
# 
#   after cancel $::xmodem::afterID
# 
#   fileevent $portID readable ""
# }

#-----------------------------------------------------------------------------
# proc ::xmodem::rcvFile { fileID }
#
# Uses filename and baud rate to download a file
#-----------------------------------------------------------------------------

proc ::xmodem::rcvFile { fileID } {
  
  ::rcxComm::portOpen $::rcxOption::towerPort 2400 o

  set ::xmodem::rcvHandlerAID [after 40 {::xmodem::rcvHandler}]

  #---------------------------------------------------------------------------
  # Here's where the receive part of the protocol actually happens. It sends
  # a NAK up to 10 times 10 seconds apart to get the protocol going. It is 
  # just a big state machine which receives packets and figures out what to
  # with them...
  #
  # The receiver states are IDLE,DATA_GOOD,DATA_BAD,GOT_CAN,DONE

  set state    IDLE
  set timeout  0
  set error    0
  set sequence 1

  ::xmodem::sendNACKMsg

  while { 0 == [regexp {DONE} $state] } {
    set ::xmodem::inMsg    ""
    set ::xmodem::rcvEvent RECEIVING

    while { 0 == [regexp {TIMEOUT|RXDONE|EOF} $::xmodem::rcvEvent] } {
      vwait ::xmodem::rcvEvent
    }
    
    if { 0 == [string compare $::xmodem::rcvEvent "TIMEOUT"] } {
      incr timeout
      if { 10 < $timeout } {
        set state DONE
      }
    }
    
    # Now decode the bytes we got from the transmitter and figure out what
    # to do...

    binary scan $::xmodem::inMsg "H*" inString
    
    puts "$::xmodem::rcvEvent -> $inString"
     
    set type "FF"

    while { 0 != [string length $inString] } {
      regexp {(..)(.*)} $inString match type inString
  
      if { 0 != [regexp {(01|04|18)} $type] } {
        break
      }
    }

    puts "String length is [string length $inString]"

    if {1 == $::rcxComm::sentBytesVisible} {
      set replyLen 262
    } else {
      set replyLen 132
    }
    
    if { 262 == [string length $inString] } {
      regexp {(..)(..)((..)*)(..)} $inString match seq notseq data dummy sum
      puts "$type $seq $notseq $data $sum"
    } 

    if { 0 == [string compare $type "01"] } {
      set state GOT_DATA
      set timeout 0 
      set seq [expr 0x$seq]

      if { $seq == $sequence } {

        set s [expr [::hexrec::hexStr8Sum $data] & 0xFF]
        
        if { [expr 0x$sum] == $s } {

          ::rcxutil::showLog "Got packet $sequence..."

          puts -nonewline $fileID [binary format H* $data]
          set sequence [expr (($sequence + 1) % 256 )]
          ::xmodem::sendACKMsg
        } else {

         ::rcxutil::showLog "Error in packet $sequence..."

          incr error
          if { 3 < $error } {
            set state DONE
          }
          ::xmodem::sendNACKMsg
        }
      }
      continue
    }

    # If we get an EOT?

    if { 0 == [string compare $type "04"] } {

      ::rcxutil::showLog "Got EOT packet..."

      set timeout 0 
      set state DONE
      ::xmodem::sendACKMsg
      continue
    }

    # If we get a CAN, set the state to GOT_CAN and if we were in that state
    # already, then set the state to DONE

    if { 0 == [string compare $type "18"] } {
      ::rcxutil::showLog "Got CAN packet..."

      set timeout 0 
      set state DONE
      ::xmodem::sendACKMsg
      continue
    }

    # If we get here, there's no telling what we got, so send a NACK!

    ::rcxutil::showLog "Got bad packet...$type"

    ::xmodem::sendNACKMsg

    incr timeout
    if { 3 < $timeout } {
      set state DONE
    }
 
  }
 
  ::rcxutil::showLog "Finished XMODEM transfer..."

  after cancel $::xmodem::rcvHandlerAID

  ::rcxComm::portClose $::rcxComm::portID
}

#-----------------------------------------------------------------------------
