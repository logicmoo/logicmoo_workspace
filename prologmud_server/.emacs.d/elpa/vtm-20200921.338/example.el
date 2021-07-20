(
 ;; Buffer key name, default to "" (filename).
 :name ""

 ;; Commands to send, default to nil.
 :commands
 (

  ;; ;; First command.
  ;; (
   ;; ;; Sleep before command, default to 0.
   ;; :sleep 0
   ;; ;; String to send, default to "".
   ;; :string ""
   ;; ;; Additional control character to send.
   ;; ;; Possible values:
   ;; ;; - "key" in command ~vterm-send-{key}~: send ~key~
   ;; ;; - "": send ~return~.
   ;; ;; - nil: do not send control character.
   ;; ;; Default to "" (send ~return~).
   ;; :control ""
   ;; )

  ;; ;; Second command.
  ;; (:sleep 0 :string "" :control "")

  ;; Third command.
  (:string "{CURSOR}")

  ))
