;; Style sheet for grammar to elisp translation          -*- lisp -*-

(
 ("GRAMMAR" default-top 1 default-bottom 1)
 ("TITLE" block t before (text ";;;; "))
 ("NAME" block t
  before (text ";; "))
 ("PROD"
  block t
  top 2 
  default-top 0 default-bottom 0
  before (left 0 text (format "(defnt \"%s\"" (fs-attval "N")))
  left 2
  after (text ")"))
 ("DELIM"
  before (text "(delim \"")
  after (text "\") "))
 ("GROUP"
  block t
  before (block t left (- (fs-char 'left) 2)
	  text (format "(var %s (%s (%s (%s "
		       (fs-attval "VAR")
		       (fs-attval "STATUS")
		       (fs-attval "OCCUR")
		       (fs-attval "ORDER")))
  after (text "))))")
  left (+ (fs-char 'left) 2))
 ("TOKEN"
  before (text "\"")
  after (text "\" "))
 ("NT"
  text (format "(nt \"%s\") " (fs-attval "N")))
 (t text "")
 )

