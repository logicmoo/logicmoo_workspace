;;; lex-parse-re.el --- Parse Emacs regexps using Lex

;; Copyright (C) 2008,2013  Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This exports lex-parse-re, but it also defines lex--parse-charset which is
;; used internally by lex-compile to handle charsets specified as a string.

;;; Code:

(require 'lex)

;;; Regexp parsers.

(defun lex--tokenizer (lex string)
  (let ((tokens ())
        (i 0)
        tmp)
    (while (and (< i (length string))
                (setq tmp (lex-match-string lex string i)))
      (push (cons (car tmp) (substring string i (setq i (cadr tmp)))) tokens))
    (nreverse tokens)))

(defun lex--parse-charset (string)
  (let ((i 0)
        (ranges ()))
    (when (eq (aref string i) ?^)
      (push 'not ranges)
      (setq i (1+ i)))
    (let ((op nil)
          (case-fold-search nil))
      (while (not (eq op 'stop))
        (lex-case string i
          ((seq "[:" (0+ (char (?a . ?z) (?A . ?Z))) ":]")
           (push (intern (substring string (+ 2 (match-beginning 0))
                                    (- (match-end 0) 2)))
                 ranges))
          ((seq anything "-" anything)
           (push (cons (aref string (match-beginning 0))
                       (aref string (1- (match-end 0))))
                 ranges))
          (anything (push (aref string (1- (match-end 0))) ranges))
          (eob (setq op 'stop))))

      `(char ,@(nreverse ranges)))))

(defconst lex--parse-re-lexspec
  '(((or "*" "+" "?" "*?" "+?" "??") . suffix)
    ((seq "[" (opt "^") (opt "]")
          (0+ (or (seq (char not ?\]) "-" (char not ?\]))
                  (seq "[:" (1+ (char (?a . ?z) (?A . ?Z))) ":]")
                  (char not ?\]))) "]") . charset)
    ((seq "\\c" anything) . category)
    ((seq "\\C" anything) . not-category)
    ((seq "\\s" anything) . syntax)
    ((seq "\\S" anything) . not-syntax)
    ((seq "\\" (char (?1 . ?9))) . backref)
    ("\\'" . eob)
    ("\\`" . bob)
    ("." . dot)
    ("^" . bol)
    ("$" . eol)
    ("." . dot)
    ("\\<" . bow)
    ("\\>" . eow)
    ("\\_<" . symbol-start)
    ("\\_>" . symbol-end)
    ("\\w" . wordchar)
    ("\\W" . not-wordchar)
    ("\\b" . word-boundary)
    ("\\B" . not-word-boundary)
    ("\\=" . point)
    ((or (seq ?\\ anything) anything) . char)))


(defconst lex--parse-ere-lexer
  (let ((case-fold-search nil))
    (lex-compile
     (append '(("(?:" . shy-group)
               ("|"  . or)
               ((seq "{" (0+ (char (?0 . ?9)))
                     (opt (seq "," (0+ (char (?0 . ?9))))) "}") . repeat)
               ((or ")" eob) . stop)
               ("(" . group))
             lex--parse-re-lexspec))))

(defconst lex--parse-bre-lexer
  (let ((case-fold-search nil))
    (lex-compile
     (append '(("\\(?:" . shy-group)
               ("\\|"  . or)
               ((seq "\\{" (0+ (char (?0 . ?9)))
                     (opt (seq "," (0+ (char (?0 . ?9))))) "\\}") . repeat)
               ((or "\\)" eob) . stop)
               ("\\(" . group))
             lex--parse-re-lexspec))))

(defun lex--parse-re (string i lexer)
  (let ((stack ())
        (op nil)
        (res nil)
        tmp)
    (while (and (not (eq op 'stop))
                (setq tmp (lex-match-string lexer string i)))
      (pcase (car tmp)
        (`shy-group
         (setq tmp (lex--parse-re string (cadr tmp) lexer))
         (unless (eq (aref string (1- (cadr tmp))) ?\))
           (error "Unclosed shy-group"))
         (push (car tmp) res))
        (`group
         (setq tmp (lex--parse-re string (cadr tmp) lexer))
         (unless (eq (aref string (1- (cadr tmp))) ?\))
           (error "Unclosed group"))
         (push (list 'group (car tmp)) res))
        (`suffix
         (if (null res) (error "Non-prefixed suffix operator")
           (setq res (cons (list (cdr (assoc (substring string i (cadr tmp))
                                             '(("*" . 0+)
                                               ("+" . 1+)
                                               ("?" . opt)
                                               ("*?" . *\?)
                                               ("+?" . +\?)
                                               ("??" . \?\?))))
                                 (car res))
                           (cdr res)))))
        (`or (push `(or (seq ,@(nreverse res))) stack)
             (setq res nil))
        (`charset
         (push (lex--parse-charset (substring string (1+ i) (1- (cadr tmp))))
               res))
        (`repeat
         ;; Here we would like to have sub-matches :-(
         (let* ((min (string-to-number
                      (substring string (+ i (if (eq (aref string i) ?\\) 2 1))
                                 (cadr tmp))))
                (max (let ((comma (string-match "," string i)))
                       (if (not (and comma (< comma (cadr tmp))))
                           min
                         (if (= comma (- (cadr tmp) 2))
                             nil
                           (string-to-number (substring string (1+ comma))))))))
           (if (null res) (error "Non-prefixed repeat operator")
             (setq res (cons `(repeat ,min ,max ,(car res)) (cdr res))))))
        (`stop (setq op 'stop))
        ((or `syntax `category `not-syntax `not-category)
         (push (list (car tmp) (aref string (1- (cadr tmp)))) res))
        (`backref
         (push (list (car tmp) (- (aref string (1- (cadr tmp))) ?0)) res))
        (`char
         (push (aref string (1- (cadr tmp))) res))
        (_ (push (car tmp) res)))
      (setq i (cadr tmp)))
    (let ((re `(seq ,@(nreverse res))))
      (while stack (setq re (nconc (pop stack) (list re))))
      (list re i))))

;;;###autoload
(defun lex-parse-re (string &optional lexer)
  "Parse STRING as a regular expression.
LEXER specifies the regexp syntax to use.  It can be `ere', or `bre'
and it defaults to `bre'."
  (setq lexer (cond ((eq lexer 'ere) lex--parse-ere-lexer)
                    ((memq lexer '(bre re nil)) lex--parse-bre-lexer)
                    (t lexer)))
  (let ((res (lex--parse-re string 0 lexer)))
    (if (< (cadr res) (length string))
        (error "Regexp parsing failed around %d: ...%s..."
               (cadr res) (substring string (1- (cadr res)) (1+ (cadr res))))
      (car res))))


;; (defun lex--parse-re (string i)
;;   (let ((stack ())
;;         (op nil)
;;         (res nil))
;;     (while (and (not (eq op 'stop)))
;;       (lex-case string i
;;         ("(?:"                          ;shy-group.
;;          (let ((tmp (lex--parse-re string i)))
;;            (setq i (car tmp))
;;            (unless (eq (aref string (1- i)) ?\)) (error "Unclosed shy-group"))
;;            (push (cdr tmp) res)))
;;         ((or "*?" "+?" "??")
;;          (error "Greediness control unsupported `%s'" (match-string 0 string)))
;;         ((or "*" "+" "?")
;;          (if (null res) (error "Non-prefixed suffix operator")
;;            (setq res (cons (list (cdr (assq (aref string (1- i))
;;                                             '((?* . 0+)
;;                                               (?+ . 1+)
;;                                               (?? . opt))))
;;                                  (car res))
;;                            (cdr res)))))
;;         ("|" (push `(or (seq ,@(nreverse res))) stack)
;;          (setq res nil))
;;         ((seq "[" (opt "^") (opt "]")
;;                    (0+ (or (seq (char not ?\]) "-" (char not ?\]))
;;                            (seq "[:" (1+ (char (?a . ?z) (?A . ?Z))) ":]")
;;                            (char not ?\]))) "]")
;;          (push (lex--parse-charset
;;                 (substring string (1+ (match-beginning 0))
;;                            (1- (match-end 0))))
;;                res))
;;         ((seq "{" (0+ (char (?0 . ?9)))
;;                    (opt (seq "," (0+ (char (?0 . ?9))))) "}")
;;          ;; Here we would like to have sub-matches :-(
;;          (let* ((min (string-to-number (substring string
;;                                                   (1+ (match-beginning 0))
;;                                                   (match-end 0))))
;;                 (max (let ((comma (string-match "," string (match-beginning 0))))
;;                        (if (not (and comma (< comma (match-end 0))))
;;                            min
;;                          (if (= comma (- (match-end 0) 2))
;;                              nil
;;                            (string-to-number (substring string (1+ comma))))))))
;;            (if (null res) (error "Non-prefixed repeat operator")
;;              (setq res (cons `(repeat ,min ,max ,(car res)) (cdr res))))))
;;         ((or ")" eob) (setq op 'stop))
;;         ("\\'" (push 'eob res))
;;         ("\\`" (push 'bob res))
;;         ("^" (push 'bol res))
;;         ("$" (push 'eol res))
;;         ("." (push 'dot res))

;;         ((or "(" "\\<" "\\>" "\\_<" "\\_>" "\\c" "\\s" "\\C" "\\S" "\\w" "\\W"
;;              "\\b" "\\B" "\\=" (seq "\\" (char (?1 . ?9))))
;;          (error "Unsupported construct `%s'" (match-string 0 string)))

;;         ((or (seq ?\\ anything) anything)
;;          (push (aref string (1- (match-end 0))) res))
;;         ("" (error "This should not be reachable"))))
;;     (let ((re `(seq ,@(nreverse res))))
;;       (while stack (setq re (nconc (pop stack) (list re))))
;;       (cons i re))))





(provide 'lex-parse-re)
;;; lex-parse-re.el ends here
