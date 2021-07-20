(define-package "impatient-mode" "1.1" "Serve buffers live over HTTP"
  '((cl-lib "0.3")
    (simple-httpd "1.4.0")
    (htmlize "1.40"))
  :commit "96c068d5add95595dc5be42115d100cf99f908ba" :authors
  '(("Brian Taylor" . "el.wubo@gmail.com"))
  :maintainer
  '("Brian Taylor" . "el.wubo@gmail.com")
  :url "https://github.com/netguy204/imp.el")
;; Local Variables:
;; no-byte-compile: t
;; End:
