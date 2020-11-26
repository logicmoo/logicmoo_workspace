#------------------------------------------------------------------------------
# hexrec.tcl - A library of routines for managing Motorola hex records
#
# # Revision History
#
# R.Hempel 25Jun99 - Original
# R.Hempel 14Mar00 - Optimized the record splitting
#                  - Made into a proper package
#------------------------------------------------------------------------------

lappend auto_path {.}

package provide hexrec 2.0

namespace eval ::hexrec:: {

  # Set up the regular expression variables we'll use later
  
  variable hex1   "\[0123456789abcdef]"
  variable hex2   "$hex1$hex1"
  variable hex4   "$hex2$hex2"
  variable s19Pat "^(S\[019])($hex2)($hex4)(($hex2)+)?($hex2)$"

  variable imageList {}
  variable imageSum  0
  variable imageIdx  0

  namespace export hexStr8Sum
  namespace export processSREC
  namespace export readSREC
  namespace export getImageSum
  namespace export numImageItems
  namespace export getImageItem
  namespace export hex2
}

#----------------------------------------------------------------------------
# proc ::hexrec::hexStr8Sum { s }
#
# Returns the sum of the values of the bytes made up of pairs of ASCII
# hexadecimal digits. Useful for summing hex records.
#
# The string may be a closely packed sequence of pair of hex digits, or they
# may be separated by whitespace.
#----------------------------------------------------------------------------

proc ::hexrec::hexStr8Sum { s } {
  regsub -all -nocase ($::hexrec::hex2) $s {0x\1+} s
  regsub {\+ *$} $s {} s
  return [expr $s]
  }

#----------------------------------------------------------------------------
# proc processSREC { s }
#
# Returns a list of useful substrings from a valid S-record, or a list
# of error messages as follows:
#
# 0 RecordType or an error string
# 1 Address as string of hex chars
# 2 Data as string of hex chars
# 3 DataSum as unsigned integer
#----------------------------------------------------------------------------

proc ::hexrec::processSREC { s } {
  
  set s [string trim $s]

  if { 0 != [ regexp -nocase $::hexrec::s19Pat $s match sType sLen sAddr sData x sSum ] } {
    set dSum [::hexrec::hexStr8Sum 00$sData]

    if { 255 == [ expr ( ($dSum + [ ::hexrec::hexStr8Sum 00$sLen$sAddr$sSum ]) % 256 ) ] } {
      return [ list $sType $sAddr $sData $dSum ]
    } else {
      return [ list {Error} {Bad Checksum} $s ]
    }
  } else {
    return [ list {Error} {Not an S-record} $s ]
  }
}

#------------------------------------------------------------------------------
# proc ::hexrec::readSREC { srecData }
#
# An internal counter is also incremented so that an external program
# can vwait on it and inspect counters, etc
#------------------------------------------------------------------------------

proc ::hexrec::readSREC { srecData } {
        
  set ::hexrec::imageSum  0
  set ::hexrec::imageList {}

  foreach srec [split $srecData] {

    # Start spinning through the lines in the SREC file - ignore blank ones

    if { 0 == [string length $srec] } {
      continue
    }

    foreach {sCode sAddr sData dSum} [::hexrec::processSREC $srec] {break}

    # Now check the result of the parsing, and only increment the image sum
    # for S1 records...

    switch -- $sCode {
      "Error" {
      }
      "S1" {
        incr ::hexrec::imageSum $dSum
        lappend ::hexrec::imageList [list $sAddr $sData $dSum]
      }
    }
  }
}

#------------------------------------------------------------------------------
# proc ::hexrec::getImageSum { }
#
# Returns the sum of the characters in the image
#------------------------------------------------------------------------------

proc ::hexrec::getImageSum { } {
  return $::hexrec::imageSum
}

#------------------------------------------------------------------------------
# proc ::hexrec::numImageItems { }
#
# Returns the number of records in the image
#------------------------------------------------------------------------------

proc ::hexrec::numImageItems { } {
  return [llength $::hexrec::imageList]
}

#------------------------------------------------------------------------------
# proc ::hexrec::getNextImageItem { }
#
# Returns the next image item
#------------------------------------------------------------------------------

proc ::hexrec::getNextImageItem { } {
  incr ::hexrec::imageIdx
  return [lindex $::hexrec::imageList $::hexrec::imageIdx]
}

#------------------------------------------------------------------------------
# proc ::hexrec::getFirstImageItem { }
#
# Returns the first image item
#------------------------------------------------------------------------------

proc ::hexrec::getFirstImageItem { } {
  set ::hexrec::imageIdx 0
  return [lindex $::hexrec::imageList $::hexrec::imageIdx]
}

#------------------------------------------------------------------------------