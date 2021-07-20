;;; toc-mode-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "toc-mode" "toc-mode.el" (0 0 0 0))
;;; Generated autoloads from toc-mode.el

(autoload 'toc-gen-set-level "toc-mode" "\
Define the text properties of the heading level.
In a pdf-view buffer select a single word in the headline of a
certain level. Then run `toc-gen-set-level' to write the text
properties to the recipe.toml file that is created in the
document's directory. You will be prompted to enter the LEVEL
number. The highest level should have number 1, the next leve
number 2 etc.

\(fn LEVEL)" t nil)

(autoload 'toc-extract-pages "toc-mode" "\
Extract text from text layer of current document and cleanup.
Extract from STARTPAGE to ENDPAGE. Use with the universal
ARG (\\[universal-argument]) omits cleanup to get the unprocessed
text.

\(fn ARG)" t nil)

(autoload 'toc-extract-pages-ocr "toc-mode" "\
Extract via OCR text of current document and cleanup.
Extract from STARTPAGE to ENDPAGE. Use with the universal
ARG (\\[universal-argument]) omits cleanup to get the
unprocessed text.

\(fn ARG)" t nil)

(autoload 'toc-extract-outline "toc-mode" "\
Extract Table of Contents attached to current document." t nil)

(autoload 'toc-extract-only "toc-mode" "\
Just extract text via OCR without further processing.
Prompt for startpage and endpage and print OCR output to new buffer." t nil)

(register-definition-prefixes "toc-mode" '("toc-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; toc-mode-autoloads.el ends here
