;;; zenscript-language.el --- ZenScript language module -*- lexical-binding: t -*-

;; Copyright (c) 2020 Eutro

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; This module of zenscript-mode uses data from ZenScript
;; dumps and the ZenScript parser to provide syntax checking
;; and data for code completion.

;;; Code:

(require 'zenscript-common)
(require 'zenscript-parser)

(defun zenscript--java-type-to-ztype (symbol)
  "Convert a Java type to a ZenType.

SYMBOL should be a java class name to be looked up in dumpzs."
  (car
   (seq-find (lambda (entry)
               (equal (cadr entry) symbol))
             (cdr (assoc "Types" (cdr (zenscript-get-dumpzs)))))))

(defun zenscript--symbol-to-type (symbol)
  "Get the ZenType from a stringified binding object SYMBOL.

If SYMBOL is the string:

 \"SymbolJavaStaticField: public static zenscript.Type ZenScriptGlobals.global\"

Then its ZenType will be resolved by looking up the zsPath of \"zenscript.Type\"."
  (when (string-match "SymbolJavaStatic\\(?:Field\\|\\(Method: JavaMethod\\)\\): public static \\(.+\\) .+$" symbol)
    (concat (if (match-string 1) "=>" "") (zenscript--java-type-to-ztype (match-string 2 symbol)))))

(defun zenscript--buffer-vals ()
  "Get a list of resolvable values in a buffer.

Returns a list of values of the form:

 (name type)

name:

  The name of the value by which it can be referenced.

type:

  The ZenType of the value, its `zsPath` from dumpzs, or nil if unknown."
  (append
   (mapcar (lambda (el)
             (list (car el)
                   (zenscript--symbol-to-type (cadr el))))
           (cdr (assoc "Globals" (cdr (zenscript-get-dumpzs)))))
   (mapcar (lambda (import)
             (or (nth 2 import)
                 (last (car import))))
           (cadr zenscript--parse-buffer-cache))))

(defun zenscript--get-importables-1 (nodes)
  "Get a list of types or static members below NODES in the tree."
  (apply #'append
         (mapcar (lambda (node)
                   (if (stringp node)
                       (list node)
                     (let ((name (car node)))
                       ;; This operates on the assumption that type names start
                       ;; with capital letters.
                       (if (string= "Lu" (get-char-code-property (string-to-char name)
                                                                 'general-category))
                           (cons name
                                 (mapcar (lambda (member)           ; "[STATIC] "
                                           (concat name "." (substring member 9)))
                                         (seq-filter (lambda (member)
                                                       (string-match-p "\\[STATIC\\] .+" member))
                                                     (mapcar (lambda (node)
                                                               (if (stringp node)
                                                                   node
                                                                 (car node)))
                                                             (cdr node)))))
                         (mapcar (lambda (importable)
                                   (concat name "." importable))
                                 (zenscript--get-importables-1 (cdr node)))))))
                 nodes)))

(defun zenscript--get-members (&optional types)
  "Get the known members of the ZenTypes TYPES, or just all known members.

Returns a list of members of the following format:

 (name . extra-info)

name:

  The name of the member.

extra-info:

  A list (possibly nil) of extra information relating to the member."
  (if types
      ()
    (apply #'append
           (mapcar (lambda (type)
                     (cdr (assoc 'members type)))
                   (cdr (assoc 'zenTypeDumps (car (zenscript-get-dumpzs))))))))

(defun zenscript--get-importables ()
  "Get a list of all things that can be imported: static members and types.

Returns a list of type names that can be imported."
  (zenscript--get-importables-1 (cdr (assoc "Root (Symbol Package)" (cdr (zenscript-get-dumpzs))))))

(defvar zenscript--parse-buffer-cache ()
  "This is the cache maintained by `zenscript-parse-buffer'.")

(defcustom zenscript-buffer-parse-idle-period 0.5
  "How long after idling should the buffer be parsed.

See `zenscript-parse-buffer'."
  :group 'zenscript
  :type #'numberp)

(defvar zenscript--language-overlays ()
  "The list of overlays from `zenscript-parse-buffer'.")

(defun zenscript-default-error-overlay (overlay message)
  "The default `zenscript-make-error-overlay-function'.

Sets the face of OVERLAY to `font-lock-warning-face', and adds the help-echo MESSAGE."
  (overlay-put overlay 'face 'font-lock-warning-face)
  (overlay-put overlay 'help-echo message))

(defcustom zenscript-make-error-overlay-function #'zenscript-default-error-overlay
  "The function to call to set overlay properties for syntax errors.

The function is called with two arguments, the overlay and the error message."
  :group 'zenscript
  :type #'functionp)

(defun zenscript--highlight-error (message token)
  "Show MESSAGE as the error for TOKEN."
  (let* ((start (if token
                    (nth 2 token)
                  (1- (point-max))))
         (end (if token
                  (+ start
                     (length (cadr token)))
                (point-max)))
         (overlay (make-overlay start end
                                () t)))
    (push overlay zenscript--language-overlays)
    (funcall zenscript-make-error-overlay-function overlay message)))

;;;###autoload
(add-hook 'zenscript-parse-error-hook #'zenscript--highlight-error)

(defun zenscript--get-bindings ()
  "Get an alist of variables in-scope at `point'."
  (apply #'append
         (mapcar (lambda (overlay)
                   (overlay-get overlay 'zenscript-bindings))
                 (overlays-at (if (eobp) (1- (point)) (point))))))1

(defun zenscript--make-bindings-overlay (start end bindings)
  "Make an overlay to inform code completion between START and END of any BINDINGS."
  (let ((overlay (make-overlay start end (current-buffer) t t)))
    (push overlay zenscript--language-overlays)
    (overlay-put overlay 'zenscript-bindings bindings)))

(defun zenscript--make-func-arguments-overlay (arguments start end)
  "Make a binding overlay for ARGUMENTS from START to END.

ARGUMENTS is the list of bindings as returned by `zenscript--parse-function-arguments'."
  (zenscript--make-bindings-overlay
   start end
   (mapcar (lambda (argument)
             (let ((token (car argument)))
               (list (cadr token)
                     (nth 2 token))))
           arguments)))

(defun zenscript--traverse-expression (expression)
  "Traverse the syntax tree from an EXPRESSION node.

See `zenscript--parse-expression'."
  (pcase (car expression)
    ((or 'E_ASSIGN 'E_CONDITIONAL 'E_OR2 'E_AND2 'E_OR 'E_XOR 'E_AND)
     (mapc #'zenscript--traverse-expression (cdr expression)))
    ('E_OPASSIGN
     (zenscript--traverse-expression (nth 2 expression))
     (zenscript--traverse-expression (nth 3 expression)))
    ((or 'E_COMPARE 'E_BINARY)
     (zenscript--traverse-expression (nth 1 expression))
     (zenscript--traverse-expression (nth 2 expression)))
    ((or 'E_UNARY 'E_MEMBER 'E_INDEX 'E_CAST 'E_INSTANCEOF)
     (zenscript--traverse-expression (nth 1 expression)))
    ('E_INDEX_SET
     (zenscript--traverse-expression (nth 1 expression))
     (zenscript--traverse-expression (nth 3 expression)))
    ('E_CALL
     (zenscript--traverse-expression (nth 1 expression))
     (mapc #'zenscript--traverse-expression (nth 2 expression)))
    ('E_FUNCTION
     (let ((arguments (nth 1 expression))
           (statements (nth 3 expression))
           (start (nth 4 expression))
           (end (nth 5 expression)))
       (zenscript--make-func-arguments-overlay arguments start end)
       (mapc (lambda (statement)
               (zenscript--traverse-statement statement end))
             statements)))
    ('E_LIST
     (mapc #'zenscript--traverse-expression (nth 1 expression)))
    ('E_MAP
     (mapc #'zenscript--traverse-expression (nth 1 expression))
     (mapc #'zenscript--traverse-expression (nth 2 expression)))))

(defun zenscript--traverse-statement (statement &optional scope-end)
  "Traverse the syntax tree from a STATEMENT node.

Any local variables are to be in scope until SCOPE-END, or 1 + `point-max'

See `zenscript--parse-statement'."
  (let ((scope-end (or scope-end (1+ (point-max))))
        (start (cadr statement))
        (after (nth 2 statement))
        (value (cdddr statement)))
    (pcase (car statement)
      ((or 'S_EXPRESSION 'S_RETURN)
       (zenscript--traverse-expression (car value)))
      ('S_BLOCK
       (let ((statements (car value))
             (end (cadr value)))
         (mapc (lambda (statement)
                 (zenscript--traverse-statement statement end))
               statements)))
      ('S_VAR
       (zenscript--make-bindings-overlay
        start
        scope-end
        (let ((token (car value)))
          (list (list (cadr token)
                      (nth 2 token)))))
       (let ((initializer (nth 2 value)))
         (when initializer
           (zenscript--traverse-expression initializer))))
      ('S_IF
       (zenscript--traverse-expression (car value))
       (mapc (lambda (statement)
               (when statement (zenscript--traverse-statement statement after)))
             (cdr value)))
      ('S_FOR
       (let ((names (car value))
             (expression (cadr value))
             (statement (nth 2 value)))
         (zenscript--make-bindings-overlay
          start
          scope-end
          (mapcar (lambda (token)
                    (list (cadr token)
                          (nth 2 token)))
                  names))
         (zenscript--traverse-expression expression)
         (zenscript--traverse-statement statement after)))
      ('S_WHILE
       (zenscript--traverse-expression (car value))
       (zenscript--traverse-statement (cadr value) after)))))

(defun zenscript--traverse-tree ()
  "Traverse `zenscript--parse-buffer-cache', indexing local bindings and such."
  (let* ((parsed (cdr zenscript--parse-buffer-cache))
         (globals
          (append
           (mapcar (lambda (import)
                     (list (or (nth 2 import)
                               (car (last (car import))))
                           (cadr import)))
                   (car parsed))
           (mapcar (lambda (func)
                     (let ((token (car func))
                           (arguments (cadr func))
                           (statements (nth 3 func))
                           (start (nth 4 func))
                           (end (nth 5 func)))
                       (zenscript--make-func-arguments-overlay arguments start end)
                       (mapc (lambda (statement)
                               (zenscript--traverse-statement statement end))
                             statements)
                       (list (cadr token)
                             (nth 2 token))))
                   (cadr parsed))
           (mapcar
            (lambda (class)
              (let ((token (car class))
                    (fields (cadr class))
                    (constructors (nth 2 class))
                    (methods (nth 3 class))
                    (start (nth 4 class))
                    (end (nth 5 class)))
                (let ((class-bindings
                       (append
                        (mapcar
                         (lambda (method)
                           (let ((token (car method))
                                 (arguments (cadr method))
                                 (statements (nth 3 method))
                                 (start (nth 4 method))
                                 (end (nth 5 method)))
                             (zenscript--make-func-arguments-overlay arguments start end)
                             (mapc (lambda (statement)
                                     (zenscript--traverse-statement statement end))
                                   statements)
                             (list (cadr token)
                                   (nth 2 token))))
                         methods)
                        (mapcar
                         (lambda (field)
                           (let ((token (car field))
                                 (init (nth 2 field)))
                             (when init (zenscript--parse-expression init))
                             (list (cadr token)
                                   (nth 2 token))))
                         fields))))
                  (zenscript--make-bindings-overlay class-bindings start end))
                (mapc
                 (lambda (constructor)
                   (let ((token (car constructor))
                         (arguments (cadr constructor))
                         (statements (nth 3 constructor))
                         (start (nth 4 constructor))
                         (end (nth 5 constructor)))
                     (zenscript--make-func-arguments-overlay arguments start end)
                     (mapc (lambda (statement)
                             (zenscript--traverse-statement statement end))
                           statements)))
                 constructors)
                (list (cadr token)
                      (nth 2 token))))
            (nth 2 parsed))
           (mapcar
            (lambda (global)
              (let ((token (car global))
                    (value (nth 2 global)))
                (zenscript--traverse-expression value)
                (list (cadr token)
                      (nth 2 token))))
            (nth 4 parsed)))))
    (zenscript--make-bindings-overlay (point-min) (point-max) globals)
    (mapc #'zenscript--traverse-statement
          (nth 3 parsed))
    nil))

(defun zenscript-parse-buffer (buffer &optional timer)
  "Parse the buffer BUFFER, refreshing the cache.

This is run periodically while in `zenscript-mode', on the idle TIMER.

Internally, this uses `zenscript--parse-tokens' and `zenscript--tokenize-buffer'."
  (if (not (buffer-live-p buffer))
      (when timer (cancel-timer timer))
    (with-current-buffer buffer
      (if (not (eq major-mode 'zenscript-mode))
          (when timer (cancel-timer timer))
        (let ((hash (secure-hash 'md5 buffer)))
          (unless (string= hash (car zenscript--parse-buffer-cache))
            (while zenscript--language-overlays
              (delete-overlay (pop zenscript--language-overlays)))
            (setq zenscript--parse-buffer-cache
                  (cons hash
                        (zenscript--parse-tokens
                         (save-excursion
                           (let ((caught
                                  (catch 'zenscript-unrecognized-token
                                    (zenscript--tokenize-buffer))))
                             (if (listp caught)
                                 caught
                               (catch 'zenscript-parse-error
                                 (zenscript--throw-parse-error
                                  "Unrecognized token"
                                  (list 'T_UNKNOWN (char-to-string (char-after))
                                        (point))))
                               ()))))))
            (zenscript--traverse-tree)))))))

(defun zenscript--identifier-p (c)
  "Return whether C is a valid character in an identifier.

Returns t if C is valid even at the start, 'not-start if C is not valid
at the start, and nil if C is not valid anywhere."
  (or (<= ?a c ?z)
      (<= ?A c ?Z)
      (= c ?_)
      (and (<= ?0 c ?9)
           'not-start)))

;;;###autoload
(defun forward-zenscript-identifier (&optional arg)
  "Move forward until encountering the end of a ZenScript identifier.
With argument ARG, do this that many times.
If ARG is omitted or nil, move point forward one identifier."
  (interactive "^p")
  (let ((times (or arg 1)))
    (cond
     ((< arg 0)
      (backward-zenscript-identifier (- arg)))
     ((= arg 0))
     (t
      (while (and (not (eobp))
                  (> times 0))
        (while (and (not (eobp))
                    (not (eq t (zenscript--identifier-p (char-after)))))
          (forward-char))
        (while (and (not (eobp))
                    (zenscript--identifier-p (char-after)))
          (forward-char))
        (setq times (1- times)))))))

;;;###autoload
(defun backward-zenscript-identifier (&optional arg)
  "Move backward until encountering the beginning of a ZenScript identifier.
With argument ARG, do this that many times.
If ARG is omitted or nil, move point backward one identifier."
  (interactive "^p")
  (let ((times (or arg 1)))
    (if (<= times 0)
        (forward-zenscript-identifier (- arg))
      (while (and (not (bobp))
                  (> times 0))
        (while (and (not (bobp))
                    (not (zenscript--identifier-p (char-before))))
          (backward-char))
        (let (earliest)
          (while (and (not (bobp))
                      (let ((id (zenscript--identifier-p (char-before))))
                        (if (eq t id)
                            (setq earliest (1- (point)))
                          id)))
            (backward-char))
          (when earliest
            (goto-char earliest)
            (setq times (1- times))))))))

(defun zenscript-goto-definition (symbol)
  "Navigate to where SYMBOL is defined in the current buffer."
  (interactive
   (let* ((bindings (zenscript--get-bindings))
          (found
           (save-excursion
             (backward-zenscript-identifier)
             (let ((bounds (bounds-of-thing-at-point 'zenscript-identifier)))
               (when bounds
                 (let ((sym (buffer-substring-no-properties (car bounds)
                                                            (cdr bounds))))
                   (car (and (assoc sym bindings)
                             (assoc sym (zenscript--get-bindings))))))))))
     (list (completing-read (if found
                                (format "Find symbol (default %s): " found)
                              "Find symbol: ")
                            (mapcar #'car bindings)
                            (lambda (s) (assoc s bindings))
                            t nil nil
                            (if found found)))))
  (let ((binding (assoc symbol (zenscript--get-bindings))))
    (if binding
        (goto-char (cadr binding))
      (error "Couldn't find definition of %s" symbol))))

(defun zenscript-goto-definition-at-point ()
  "Navigate to the definition of the ZenScript identifier around point."
  (interactive)
  (zenscript-goto-definition
   (let ((bounds (bounds-of-thing-at-point 'zenscript-identifier)))
     (if bounds
         (buffer-substring-no-properties (car bounds)
                                         (cdr bounds))
       (call-interactively #'zenscript-goto-definition)))))

(defun zenscript--init-language ()
  "Initialize the language module."
  (make-local-variable 'zenscript--parse-buffer-cache)
  (make-local-variable 'zenscript--warnings)
  (make-local-variable 'zenscript-buffer-parse-timer)
  ;; `run-with-idle-timer', but passing the timer to the function
  (let ((timer (timer-create)))
    (timer-set-function timer #'zenscript-parse-buffer (list (current-buffer) timer))
    (timer-set-idle-time timer zenscript-buffer-parse-idle-period t)
    (timer-activate-when-idle timer t)))

(provide 'zenscript-language)
;;; zenscript-language.el ends here
