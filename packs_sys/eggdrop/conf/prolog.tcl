
#need to convert " to \" and put quotes arround it
proc  escapeString {escapeme} {
 return "\"[string map {\" \\\" \\ \\\\} $escapeme]\""
}

proc put1idx {nick message} {
 if {[set idx [hand2idx $nick]] != -1} {
  putidx $idx $message
 }
}

proc get3react {message} {
 put1idx "dmiles" $message
 put1idx "swipl" $message
 put1idx "logicmoo" $message
}

proc get2react {type message} {
 get3react "get2react(\['$type',$message\])."
}


#PUB 
#proc react-pub {nick uhost hand channel args} { get2react "pub" "[escapeString $nick],[escapeString $uhost],[escapeString $hand],[escapeString $channel],[escapeString $args]"}
# bind pub - * react-pub

#SPLT (stackable) 
proc react-splt {nick uhost hand channel} { get2react "split" "[escapeString $nick],[escapeString $uhost],[escapeString $hand],[escapeString $channel]"  }

#REJN (stackable) 
proc react-rejn {nick uhost hand channel} {  get2react "rejoin" "[escapeString $nick],[escapeString $uhost],[escapeString $hand],[escapeString $channel]" }

#NEED (stackable) 
proc react-need {channel which} { 
    if {$which=="op"} { return 1}
    get2react "need" "[escapeString $channel],[escapeString $which]" 
}

proc react-join {nick uhost hand channel} { get2react "join" "[escapeString $nick],[escapeString $uhost],[escapeString $hand],[escapeString $channel]"}

proc react-leave {nick uhost hand channel {message ""}} { get2react "part" "[escapeString $nick],[escapeString $uhost],[escapeString $hand],[escapeString $channel],[escapeString $message]" }

proc react-kick {nick uhost hand channel target reason} { get2react "kick" "[escapeString $nick],[escapeString $uhost],[escapeString $hand],[escapeString $channel],[escapeString $target],[escapeString $reason]"}

proc react-nick {nick uhost hand channel new_nick} { get2react "nick" "[escapeString $nick],[escapeString $uhost],[escapeString $hand],[escapeString $channel],[escapeString $new_nick]"}

proc react-mode {nick uhost hand channel mode_change victim} {get2react "mode" "[escapeString $nick],[escapeString $uhost],[escapeString $hand],[escapeString $channel],[escapeString $mode_change],[escapeString $victim]" }

proc react-topc {nick uhost hand channel topic} {  get2react "topc" "[escapeString $nick],[escapeString $uhost],[escapeString $hand],[escapeString $channel],[escapeString $topic]" }

proc react-pubm {nick uhost hand channel text} { get2react "pubm" "[escapeString $nick],[escapeString $uhost],[escapeString $hand],[escapeString $channel],[escapeString $text]"}

proc react-msgm {nick uhost hand text} { get2react "msgm" "[escapeString $nick],[escapeString $uhost],[escapeString $hand],[escapeString $nick],[escapeString $text]"}

proc react-notc {nick uhost hand text {dest ""}} { get2react "notc" "[escapeString $nick],[escapeString $uhost],[escapeString $hand],[escapeString $text],[escapeString $dest]"}

proc react-flud {nick uhost hand type chan} { get2react "flud" "[escapeString $nick],[escapeString $uhost],[escapeString $hand],[escapeString $type],[escapeString $chan]"
 return 1 }

proc react-ctcp {nick uhost hand dest key arg} { get2react "ctcp" "[escapeString $nick],[escapeString $uhost],[escapeString $hand],[escapeString $dest],[escapeString $key],[escapeString $arg]"
 return 0 }

proc react-ctcr {nick uhost hand dest key arg} { get2react "ctcr" "[escapeString $nick],[escapeString $uhost],[escapeString $hand],[escapeString $dest],[escapeString $key],[escapeString $arg]"
 return 0 }

proc react-raw {from keyword arg} { get2react "raw" "[escapeString $from],[escapeString $keyword],[escapeString $arg]"
 return 0 }

proc react-bot {from keyword arg} { get2react "bot" "[escapeString $from],[escapeString $keyword],[escapeString $arg]"
 return 0 }

proc react-fil {hand idx txt} { get2react "fil" "[escapeString $hand],[escapeString $idx],[escapeString $txt]"
 return 0 }

proc react-chon {hand idx} { get2react "chon" "[escapeString $hand],[escapeString $idx]"
 return 0 }

proc react-wall {hand msg} { get2react "wall" "[escapeString $hand],[escapeString $msg]"
 return 0 }

proc react-chof {hand idx} { get2react "chof" "[escapeString $hand],[escapeString $idx]"
 return 0 }

proc react-evnt {type} { get2react "evnt" "[escapeString $type]"
 return 0 }

proc donothing {} {
    return 1
}

# unbind dcc o|o say *dcc:say 
bind dcc - mysay *dcc:say
bind flud - * react-flud
bind topc - * react-topc
bind msgm - * react-msgm
bind notc - * react-notc
bind pubm - * react-pubm
bind mode - * react-mode
bind kick - * react-kick
bind nick - * react-nick
bind splt - * react-splt
bind rejn - * react-rejn
bind need - * react-need
bind join - * react-join
bind part - * react-leave
bind sign - * react-leave
bind ctcp - * react-ctcp
bind ctcr - * react-ctcr
#bind fil - * react-fil
bind chon - * react-chon
bind chof - * react-chof
bind bot - * react-bot
bind evnt - * react-evnt
bind wall - * react-wall

#bind ctcp - CLIENTINFO react-ctcp
#bind ctcp - USERINFO react-ctcp
#bind ctcp - VERSION react-ctcp
#bind ctcp - FINGER react-ctcp
#bind ctcp - ERRMSG react-ctcp
#bind ctcp - ECHO react-ctcp
#bind ctcp - INVITE react-ctcp
#bind ctcp - WHOAMI react-ctcp
#bind ctcp - OP react-ctcp
#bind ctcp - OPS react-ctcp
#bind ctcp - UNBAN react-ctcp
#bind ctcp - PING react-ctcp
#bind ctcp - TIME react-ctcp
#bind raw - NOTICE react-raw
#bind raw - PRIVMSG react-raw
#bind raw - *  react-raw

