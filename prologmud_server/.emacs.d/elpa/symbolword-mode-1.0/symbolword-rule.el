;; -*- lexical-binding: t -*-
;; pure

(defun symbolword/div-word? (left-char curr-char)
  "div word rule"
  (or
   (not (characterp left-char))
   (not (characterp curr-char))
   (let ((left-class (symbolword/unicode-class left-char))
         (curr-class (symbolword/unicode-class curr-char)))
     (or
      (and
       (not (or ;without
             (and
              (eq left-class 'space)
              (eq curr-class 'space))
             (and
              (eq left-class 'upper-case)
              (eq curr-class 'lower-case))
             (and
              (eq left-class 'lower-case)
              (eq curr-class 'lower-case))
             (and
              (eq left-class 'upper-case)
              (eq curr-class 'upper-case))
             (and
              (eq left-class 'digit)
              (eq curr-class 'digit))))
       (not (and
             (eq left-class curr-class)
             (symbolword/equal-syntax? left-char curr-char))))))))

(defun symbolword/equal-syntax? (a b)
  "Depend on language syntax"
  (=
   (char-syntax a)
   (char-syntax b)))

(defun symbolword/unicode-class (ucs)
  "XML like unicode character class"
  (cond
   ((or
     (= ucs 32)
     (= ucs 160)
     (= ucs 8199)
     (= ucs 8203)
     (= ucs 8288)
     (= ucs 12288)
     (= ucs 65279)
     (= ucs 9);tab
     )
    'space)
   ((= ucs 10) 'newline)
   ((and (>= ucs 48) (<= ucs 57))       'digit)
   ((and (>= ucs 65) (<= ucs 90))       'upper-case)
   ((and (>= ucs 97) (<= ucs 122))      'lower-case)
   ((and (>= ucs 12352) (<= ucs 12447)) 'hiragana)
   ((and (>= ucs 12448) (<= ucs 12543)) 'katakana)
   ((and (>= ucs 19968) (<= ucs 40959)) 'kanji)
   ((and (>= ucs 13312) (<= ucs 19902)) 'kanji)
   ((and (>= ucs 63744) (<= ucs 64255)) 'kanji)
   (t 'otherwise)))

(provide 'symbolword-rule)
