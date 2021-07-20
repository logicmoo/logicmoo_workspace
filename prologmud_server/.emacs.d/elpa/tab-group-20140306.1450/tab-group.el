;;; tab-group.el --- Grouped tabs and their tabbar

;; Author: INA Lintaro <tarao.gnn at gmail.com>
;; URL: http://github.com/tarao/tab-group-el
;; Package-Version: 20140306.1450
;; Package-Commit: 5a290ec2608e4100fb188fd60ecb77affcc3465b
;; Version: 0.1
;; Keywords: convenience, tabs

;; This file is NOT part of GNU Emacs.

;;; License:
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(eval-when-compile (require 'cl))


;;; customization

(defgroup tab-group nil
  "Grouped tabs and their tabbar."
  :group 'convenience)

(defcustom tab-group:show-tabbar 'header-line
  "Place to show tabbar.

The following symbols are available:
  header-line : the tabbar is shown as the header line
  mode-line   : the tabbar is shown as the mode line
  manual      : the tabbar is not to be shown automatically

When the value of the variable is the symbol `manual', you have
to manually set `tab-group:tabbar' somewhere in
`header-line-format' or `mode-line-format'."
  :type '(choice (const :tag "header line" header-line)
                 (const :tag "mode line" mode-line)
                 (const :tag "manual" manual))
  :group 'tab-group)

(defcustom tab-group:show-group-name t
  "t means to show group name on the tabbar."
  :type 'boolean
  :group 'tab-group)

(defcustom tab-group:show-scroll-button 'on-demand
  "Specify when to show the scroll buttons.
`on-demand' means that the scroll buttons appear when there is a
space to scroll.  `always' means that the scroll buttons always
appear even if they are disabled.  `never' means that the scroll
buttons never appear."
  :type '(choice (const :tag "on demand" on-demand)
                 (const :tag "always" always)
                 (const :tag "never" nil))
  :group 'tab-group)

(defcustom tab-group:scroll t
  "Automatically scroll tabbar to make selected tab visible."
  :type 'boolean
  :group 'tab-group)

(defcustom tab-group:truncate nil
  "Truncate tab name if there isn't enough space."
  :type 'boolean
  :group 'tab-group)

(defcustom tab-group:no-truncation-for-current-tab t
  "Do not truncate the current tab."
  :type 'boolean
  :group 'tab-group)

(defcustom tab-group:tabbar-width nil
  "Max width of tabbar.
nil means the window width is used.  Otherwise, tabs in the
tabbar are truncated or scrolled according to the values of
`tab-group:truncated' and `tab-group:scroll' to fit in the width
but this does not mean to hide some part of tabbar longer than
the width.  To restring length of the tabbar, use the form for
example '(20 tab-group:tabbar) in `header-line-format' or
`mode-line-format'."
  :type '(choice integer (const :tag "window width" nil))
  :group 'tab-group)

(defcustom tab-group:ellipsis (string #x2026)
  "Ellipsis string used for truncation."
  :type 'string
  :group 'tab-group)

(defcustom tab-group:tab-separator " "
  "Separator string of tabs."
  :type 'string
  :group 'tab-group)

(defcustom tab-group:show-tab-index 'on-demand
  "Specify when to show the tab index.
`on-demand' means that the index appears during tab selection.
`always' means that the index always appears.  `never' means that
the index never appears."
  :type '(choice (const :tag "on demand" 'on-demand)
                 (const :tag "always" 'always)
                 (const :tag "never" 'never))
  :group 'tab-group)

(defcustom tab-group:tab-index 'relative
  "The way to show the index of a tab.
`relative' means that the index is the position (starting from 0)
in the tabbar.  `fixed' means that the index is the least unused
number when the tab is created."
  :type '(choice (const :tag "relative" 'relative)
                 (const :tag "fixed" 'fixed))
  :group 'tab-group)

(defcustom tab-group:tab-index-format "%d:"
  "Format of tab number.
This is used when selecting tab by `tab-group:select'."
  :type 'string
  :group 'tab-group)

(defcustom tab-group:move-target (string #x2304)
  "Target indicator of tab movement."
  :type 'string
  :group 'tab-group)

(defcustom tab-group:scroll-left-symbol (string #xab)
  "Symbol of scroll left button."
  :type 'string
  :group 'tab-group)

(defcustom tab-group:scroll-right-symbol (string #xbb)
  "Symbol of scroll right button."
  :type 'string
  :group 'tab-group)

(defcustom tab-group:select-single-match nil
  "Automatically select matched tab in `tab-group:select' when
there is only one such tab."
  :type 'boolean
  :group 'tab-group)

(defcustom tab-group:prefix "C-x t"
  "Prefix for tab group keymap `tab-group:local-mode-map'.
This variable must be set before loading tab-group.el."
  :type '(choice string vector)
  :group 'tab-group)

(defface tab-group:tabbar
  '((t (:inherit header-line :underline nil)))
  "Face of tabbar."
  :group 'tab-group)

(defface tab-group:tab-separator
  '((t (:inherit tab-group:tabbar)))
  "Face of tab separator."
  :group 'tab-group)

(defface tab-group:group
  '((t (:inherit tab-group:tabbar :foreground "#8c8ce8")))
  "Face of group name."
  :group 'tab-group)

(defface tab-group:scroll-button
  '((t (:inherit tab-group:tabbar)))
  "Face of scroll button."
  :group 'tab-group)

(defface tab-group:scroll-button:hover
  '((t (:inherit tab-group:tab:active)))
  "Face of scroll button under the mouse."
  :group 'tab-group)

(defface tab-group:scroll-button:disabled
  '((t (:inherit tab-group:scroll-button :foreground "gray")))
  "Face of disabled scroll button."
  :group 'tab-group)

(defface tab-group:tab
  '((t (:inherit tab-group:tabbar)))
  "Face of unselected tabs."
  :group 'tab-group)

(defface tab-group:tab:active
  '((t (:inherit tab-group:tab
        :background "SteelBlue" :foreground "white" :bold t)))
  "Face of the selected tab."
  :group 'tab-group)

(defface tab-group:tab:hover
  '((t (:inherit tab-group:tab
        :background "LightSkyBlue" :foreground "white")))
  "Face of the tab under the mouse."
  :group 'tab-group)

(defface tab-group:match
  '((t (:foreground "tomato" :underline t)))
  "Face for matched text."
  :group 'tab-group)

(defface tab-group:move-target
  '((t (:inherit tab-group:tab)))
  "Face of target of moving tab."
  :group 'tab-group)

(defgroup tab-group:group-buffer nil
  "Show a single buffer for each group."
  :group 'tab-group)

(defcustom tab-group:group-buffer-mode nil
  "t means to automatically enable group buffer mode.
See the documentation of `tab-group:group-buffer-local-mode' for
the detail.  This variable must be set before creating a new tab
group, i.e., to make this variable take effect in the default
group, it must be set before loading tab-group.el."
  :type 'boolean
  :group 'tab-group:group-buffer)

(defcustom tab-group:group-buffer-prefix nil
  "Put prefix on buffer names of group buffers."
  :type 'string
  :group 'tab-group:group-buffer)

(defgroup tab-group:auto nil
  "Automatically put buffers into groups."
  :group 'tab-group)

(defcustom tab-group:auto-process-modes
  '(comint-mode compilation-mode)
  "Modes which is put into \"Process\" group."
  :type '(list symbol)
  :group 'tab-group:auto)

(defcustom tab-group:auto-help-modes
  '(help-mode apropos-mode Info-mode Man-mode)
  "Modes which is put into \"Help\" group."
  :type '(list symbol)
  :group 'tab-group:auto)

(defcustom tab-group:auto-mail-modes
  '(rmail-mode rmail-edit-mode
    vm-summary-mode vm-mode
    mail-mode mh-letter-mode mh-show-mode mh-folder-mode
    gnus-summary-mode message-mode gnus-group-mode
    gnus-article-mode score-mode gnus-browse-killed-mode
    mew-summary-mode mew-header-mode mew-message-mode mew-draft-mode
    mew-virtual-mode mew-addrbook-mode mew-refile-view-mode
    wl-summary-mode wl-message-mode wl-original-message-mode wl-draft-mode
    wl-view-mode wl-template-mode wl-addrbook-mode wl-addrmgr-mode
    wl-news-mode wl-plugged-mode)
  "Modes which is put into \"Mail\" group."
  :type '(list symbol)
  :group 'tab-group:auto)

(defcustom tab-group:auto-common-buffers '("*scratch*" "*Messages*")
  "Buffer names of which buffer is put into \"Common\" group."
  :type '(list string)
  :group 'tab-group:auto)

(defcustom tab-group:auto-exclude-modes
  '(completion-list-mode Buffer-menu-mode)
  "Modes which is not put into any group."
  :type '(list symbol)
  :group 'tab-group:auto)

(defcustom tab-group:auto-exclude-buffers '("*Completions*")
  "Buffer names of which buffer is not put into any group."
  :type '(list string)
  :group 'tab-group:auto)


;;; internal variables, structures and functions

;; global variable
(defvar tab-group:groups nil)
(defvar tab-group:default-group nil)
(defvar tab-group:tab-id 0)
(defvar tab-group:tab-list-function 'tab-group:tab-list
  "Function used to make a tab label list in the tabbar.")

(defvar tab-group:update-search-function 'tab-group:update-search)
(defvar tab-group:start-search-function 'tab-group:start-search)
(defvar tab-group:end-search-function 'tab-group:end-search)
(defvar tab-group:search-result nil)
(defvar tab-group:search-group nil)
(defvar tab-group:search-current-buffer nil)
(defvar tab-group:search-pattern nil)
(defvar tab-group:search-scroll-restore nil)

(defvar tab-group:move-current-tab nil)
(defvar tab-group:move-tab-list-restore nil)

(defvar tab-group:tab-list-tab+size-width 26)
(defvar tab-group:tab-list-column 4)
(defvar tab-group:tab-list-process-name-width 25)
(defvar tab-group:tab-list-mode-remap nil)

;; local variable
(defvar tab-group:current-tab nil
  "Buffer local value of the active tab of the current buffer.")
(make-variable-buffer-local 'tab-group:current-tab)
(put 'tab-group:current-tab 'permanent-local t)
(defvar tab-group:buffer-tabs nil
  "Tabs that the current buffer belongs to.")
(make-variable-buffer-local 'tab-group:buffer-tabs)
(put 'tab-group:buffer-tabs 'permanent-local t)
(defvar tab-group:tabbar nil
  "Tabbar construct which can be used in `header-line-format' or
  `mode-line-format'.")
(put 'tab-group:tabbar 'risky-local-variable t)
(make-variable-buffer-local 'tab-group:tabbar)
(put 'tab-group:tabbar 'permanent-local t)
(defvar tab-group:scroll-buttons (list ""))
(put 'tab-group:scroll-buttons 'risky-local-variable t)
(make-variable-buffer-local 'tab-group:scroll-buttons)
(put 'tab-group:scroll-buttons 'permanent-local t)
(defvar tab-group:restore-tabbar nil)
(make-variable-buffer-local 'tab-group:restore-tabbar)
(put 'tab-group:restore-tabbar 'permanent-local t)
(defvar tab-group:last-scroll-pos nil)
(make-variable-buffer-local 'tab-group:last-scroll-pos)
(put 'tab-group:last-scroll-pos 'permanent-local t)
(defvar tab-group:tablist-cache nil)
(make-variable-buffer-local 'tab-group:tablist-cache)
(put 'tab-group:tablist-cache 'permanent-local t)

(defvar tab-group:buffer-original-name nil)
(make-variable-buffer-local 'tab-group:buffer-original-name)
(put 'tab-group:buffer-original-name 'permanent-local t)
(defvar tab-group:buffer-original-id nil)
(make-variable-buffer-local 'tab-group:buffer-original-id)
;; don't put permanent-local to tab-group:buffer-original-id
(defvar tab-group:buffer-original-file nil)
(make-variable-buffer-local 'tab-group:buffer-original-file)
(put 'tab-group:buffer-original-file 'permanent-local t)
(defvar tab-group:buffer-last-mtime nil)
(make-variable-buffer-local 'tab-group:buffer-last-mtime)
(put 'tab-group:buffer-last-mtime 'permanent-local t)

(defvar tab-group:buffer-auto-groups-function 'tab-group:buffer-auto-groups
  "Function which determines names of groups which the current
buffer should be put into.")
(defvar tab-group:buffer-auto-p-function 'tab-group:buffer-auto-p
  "Function which determines if the current buffer is
automatically put into some groups when `tab-group:auto-mode' is
enabled.")

;; hooks
(defvar tab-group:local-mode-entery-hook nil
  "Hook runs just after initialization of `tab-group:local-mode'.")
(defvar tab-group:local-mode-exit-hook nil
  "Hook runs just before finalization of `tab-group:local-mode'.")
(defvar tab-group:select-tab-hook nil
  "Hook runs just after selecting a tab.
The current buffer and `tab-group:current-tab' are those of the
selected tab in the invocation of the hook.")
(defvar tab-group:unselect-tab-hook nil
  "Hook runs when another tab is to be selected or the current
  tab is going to be popped out from the tabbar.  This hook must
  take two arguments TAB and GROUP.  TAB is the tab being
  unselected and GROUP is the group of TAB (possibly nil).")

;; keymaps
(defvar tab-group:keymap
  (let ((map (make-sparse-keymap)))
   (define-key map " " #'tab-group:next)
   (define-key map (kbd "n") #'tab-group:next)
   (define-key map (kbd "p") #'tab-group:prev)
   (define-key map (kbd "s") #'tab-group:select)
   (define-key map (kbd "g") #'tab-group:switch)
   (define-key map (kbd "N") #'tab-group:new)
   (define-key map (kbd "P") #'tab-group:pop)
   (define-key map (kbd "r") #'tab-group:rename)
   (define-key map (kbd "l") #'tab-group:list)
   (define-key map [left] #'tab-group:scroll-left)
   (define-key map [right] #'tab-group:scroll-right)
   (define-key map [home] #'tab-group:scroll-begin)
   (define-key map [end] #'tab-group:scroll-end)
   (define-key map (kbd "<") #'tab-group:move-tab-left)
   (define-key map (kbd ">") #'tab-group:move-tab-right)
   (define-key map (kbd "[") #'tab-group:move-tab-begin)
   (define-key map (kbd "]") #'tab-group:move-tab-end)
   map))
(defvar tab-group:local-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map
      (let ((prefix tab-group:prefix))
        (if (stringp prefix) (read-kbd-macro prefix) prefix))
      tab-group:keymap)
    map)
  "Local keymap for `tab-group:local-mode'.")

(defalias 'tab-group:define-key (symbol-function 'define-key))
(defmacro tab-group:define-button-map (bind &rest body)
  (declare (indent 1))
  (let ((map (nth 0 bind)))
    `(let ((,map (make-sparse-keymap)))
       (dolist (prefix '(nil (header-line) (mode-line)))
         (flet ((define-key (map key def)
                  (tab-group:define-key map (vconcat prefix key) def)))
           ,@body))
       ,map)))
(defvar tab-group:tab-button-map
  (tab-group:define-button-map (map)
    (define-key map [mouse-1] #'tab-group:mouse-select)
    (define-key map [mouse-2] #'tab-group:mouse-kill)
    (define-key map [double-mouse-1] #'tab-group:rename)
    (define-key map [double-mouse-3] #'tab-group:pop)
    (define-key map [down-mouse-1] #'tab-group:move-start)
    (define-key map [drag-mouse-1] #'tab-group:mouse-move))
  "Keymap (mouse event map) for tabbar button.")
(defvar tab-group:group-button-map
  (tab-group:define-button-map (map)
    (define-key map [mouse-1] #'tab-group:next-group)
    (define-key map [mouse-3] #'tab-group:prev-group)
    (define-key map [double-mouse-1] #'tab-group:list))
  "Keymap (mouse event map) for tabbar group button.")
(defvar tab-group:scroll-left-map
  (tab-group:define-button-map (map)
    (define-key map [mouse-1] #'tab-group:scroll-left)
    (define-key map [double-mouse-1] #'tab-group:scroll-begin))
  "Keymap (mouse event map) for tabbar scroll left button.")
(defvar tab-group:scroll-right-map
  (tab-group:define-button-map (map)
    (define-key map [mouse-1] #'tab-group:scroll-right)
    (define-key map [double-mouse-1] #'tab-group:scroll-end))
  "Keymap (mouse event map) for tabbar scroll right button.")

;; constants
(defconst tab-group:zero-width-space
  (propertize (string #x200b)
              'face 'tab-group:tabbar 'display '(space :width 0)))
(defconst tab-group:scroll-button-left
  (propertize tab-group:scroll-left-symbol
              'face 'tab-group:scroll-button
              'mouse-face 'tab-group:scroll-button:hover
              'help-echo "Scroll left"
              'local-map tab-group:scroll-left-map))
(defconst tab-group:scroll-button-right
  (propertize tab-group:scroll-right-symbol
              'face 'tab-group:scroll-button
              'mouse-face 'tab-group:scroll-button:hover
              'help-echo "Scroll right"
              'local-map tab-group:scroll-right-map))

;; structures
(defstruct (tab-group:group (:constructor tab-group:make-group))
  name tabs tabbar list binding)
(defstruct (tab-group:tab (:constructor tab-group:make-tab))
  name id group buffer tab index)

;; internal functions

(defun tab-group:to-name (obj &optional class)
  (let* ((class (and class (tab-group:to-name class)))
         (classp (and class (intern (concat "tab-group:" class "-p")))))
    (cond
     ((symbolp obj) (symbol-name obj))
     ((and class (funcall classp obj))
      (funcall (intern (concat "tab-group:" class "-name")) obj))
     (t obj))))

(defsubst tab-group:group (group)
  (if (tab-group:group-p group)
      group
    (cdr (assoc group tab-group:groups))))
(defsubst tab-group:tabbar-sym (group)
  (let ((group (tab-group:to-name group 'group)))
    (intern (concat "tab-group:tabbar:" group))))
(defsubst tab-group:tabbar (group)
  (symbol-value (tab-group:tabbar-sym group)))
(defsubst  tab-group:tab-sym (group tab-id)
  (let ((group (tab-group:to-name group 'group))
        (id (if (tab-group:tab-p tab-id) (tab-group:tab-id tab-id) tab-id)))
    (intern (format "tab-group:tab:%s:%d" group id))))
(defsubst tab-group:tab (group tab-id)
  (symbol-value (tab-group:tab-sym group tab-id)))
(defsubst tab-group:current-group ()
  (and tab-group:current-tab (tab-group:tab-group tab-group:current-tab)))
(defsubst tab-group:get (key &optional group)
  (let ((group (or group (tab-group:current-group))))
    (and group (cdr (assq key (tab-group:group-binding group))))))
(defsubst tab-group:set (key value &optional group)
  (let* ((group (or group (tab-group:current-group)))
         (binding (and group (tab-group:group-binding group)))
         (binding (delq (assq key binding) binding)))
    (when group
      (setf (tab-group:group-binding group) (push (cons key value) binding)))))
(defsubst tab-group:tab-group-name (tab)
  (tab-group:group-name (tab-group:tab-group tab)))
(defsubst tab-group:tab-selected-p (tab)
  (and tab (eq (tab-group:tab-buffer tab) (current-buffer))))
(defsubst tab-group:last-tab-p (tab)
  (let ((group (and tab (tab-group:tab-group tab))))
    (and tab (eq tab (car-safe (last (tab-group:group-tabs group)))))))
(defsubst tab-group:fill-face (face)
  (let ((background (face-attribute face :background nil face)))
    `(:inherit ,face :foreground ,background)))

(defun tab-group:split (elt list)
  "Return a pair of values indicating adjacent elements of ELT in LIST.
The first value of the pair is a list of elements before ELT in
LIST in reversed order.  The second value of the pair is a list
of elements after ELT in LIST."
  (let (left right)
    (while list
      (if (eq elt (car list))
          (setq right (cdr list) list nil)
        (setq left (cons (car list) left) list (cdr list))))
    (cons left right)))

(defun tab-group:inheriting-propertize (str parent props)
  (let ((args (list str)))
    (dolist (prop props)
      (setq args (append args (list prop (get-text-property 0 prop parent)))))
    (apply 'propertize args)))

(defun tab-group:window-total-width (&optional window)
  (if (fboundp 'window-total-width)
      (window-total-width window)
    (let ((e (window-edges window))) (- (nth 2 e) (nth 0 e)))))

(eval-and-compile
  (defalias 'tab-group:display-update
    (if (fboundp 'force-window-update)
        #'(lambda () (force-window-update (selected-window)))
      #'force-mode-line-update)))


;;; local mode

(define-minor-mode tab-group:local-mode
  "Minor mode for tab-group buffer.
Quitting this mode pops out every tab of the buffer from its
tabbar and restores `header-line-format' or `mode-line-format' if
it is overridden by the tabbar.

\\{tab-group:local-mode-map}"
  :keymap 'tab-group:local-mode-map
  :group 'tab-group
  (if tab-group:local-mode
      ;; on
      (progn
        (add-hook 'after-revert-hook #'tab-group:restore-local-mode nil t)
        (add-hook 'after-change-major-mode-hook
                  #'tab-group:restore-local-mode nil t)
        (add-hook 'kill-buffer-hook #'tab-group:killing-buffer nil t)
        (when (called-interactively-p 'any)
          (call-interactively 'tab-group:new))
        (run-hooks 'tab-group:local-mode-entry-hook))
    ;; off
    (run-hooks 'tab-group:local-mode-exit-hook)
    (remove-hook 'after-revert-hook 'tab-group:restore-local-mode t)
    (remove-hook 'after-change-major-mode-hook 'tab-group:restore-local-mode t)
    (remove-hook 'kill-buffer-hook 'tab-group:killing-buffer t)
    (mapc #'tab-group:pop tab-group:buffer-tabs)
    (when tab-group:restore-tabbar (tab-group:restore-tabbar))
    (setq tab-group:current-tab nil
          tab-group:buffer-tabs nil
          tab-group:tabbar nil
          tab-group:restore-tabbar nil
          tab-group:last-scroll-pos nil
          tab-group:tablist-cache nil)))

(defun tab-group:restore-local-mode ()
  "Re-enable `tab-group:local-mode'."
  (when (and tab-group:current-tab
             (or (and (numberp tab-group:local-mode)
                      (<= tab-group:local-mode 0))
                 (null tab-group:local-mode)))
    (let ((group (tab-group:current-group)))
      (tab-group:local-mode 1)
      (tab-group:show-tabbar)
      (when (< 0 (tab-group:get 'group-buffer-mode))
        (tab-group:group-buffer-local-mode-on)))))
(put 'tab-group:restore-local-mode 'permanent-local-hook t)

(defun tab-group:killing-buffer ()
  "Select one of other tabs in the tabbar and disable
`tab-group:local-mode' before killing the current buffer.  If
`tab-group:current-tab' is the last tab in the tabbar, select the
previous tab.  Otherwise, select the next tab."
  (let ((buffer (current-buffer)))
    (when (tab-group:tab-selected-p tab-group:current-tab)
      (let ((buffer (current-buffer)))
        (if (tab-group:last-tab-p tab-group:current-tab)
            (tab-group:prev)
          (tab-group:next))
        (switch-to-buffer buffer)))
    (with-current-buffer buffer
      (tab-group:local-mode 0))))


;;; tabbar

(defsubst tab-group:tab-separator (&optional tab)
  (propertize tab-group:tab-separator
              'face 'tab-group:tab-separator
              'tab-group:tab tab))

(defsubst tab-group:format-width (&rest formats)
  (string-width (format-mode-line (cons "" formats))))

(defsubst tab-group:tabbar-default-width ()
  (or tab-group:tabbar-width (tab-group:window-total-width)))

(defsubst tab-group:tab-button-label (tab label)
  (propertize label
              'tab-group:tab tab
              'face 'tab-group:tab
              'mouse-face 'tab-group:tab:hover
              'local-map tab-group:tab-button-map
              'help-echo (tab-group:tab-name tab)))

(defsubst tab-group:active-tab-button-label (tab label)
  (propertize (tab-group:tab-button-label tab label)
              'face 'tab-group:tab:active
              'mouse-face 'tab-group:tab:active))

(defsubst tab-group:group-button-label (group)
  (propertize (tab-group:group-name group)
              'face 'tab-group:group
              'mouse-face 'tab-group:group
              'local-map tab-group:group-button-map))

(defun tab-group:truncate (str max-width)
  "Truncate STR to fit in MAX-WIDTH.
Use `tab-group:ellipsis' in the middle of string to indicate
omitted part.  If there is no room to put the ellipsis in the
middle, put it at the end instead, or put no ellipsis at all."
  (let* ((len (length str)) (w 0) (i 0)
         (width (string-width str))
         (ellipsis tab-group:ellipsis)
         (ellipsis-width (string-width ellipsis)))
    (cond
     ((<= width max-width) str)
     ((and (< ellipsis-width max-width)
           (< 0 (setq width (/ (- max-width ellipsis-width) 2))))
      ;; ellipsis in the middle
      (while (< w width)
        (setq w (+ w (char-width (aref str i)))
              i (1+ i)))
      (setq w (+ w ellipsis-width))
      (while (< w max-width)
        (setq len (1- len)
              w (+ w (char-width (aref str len)))))
      (concat (substring str 0 i) ellipsis (substring str len)))
     (t
      (while (< w max-width)
        (setq w (+ w (char-width (aref str i)))
              i (1+ i)))
      (let ((str (substring str 0 i)))
        (if (<= max-width (+ w ellipsis-width))
            ;; no room for the ellipsis
            str
          ;; ellipsis at the end
          (concat str ellipsis)))))))

(defun tab-group:set-label (tab label &optional selected)
  "Set the value of tab label symbol of TAB to LABEL.
If SELECTED is non-nil, it specifies the value for selected tab."
  (let ((tab-sym (tab-group:tab-tab tab))
        (buffer (tab-group:tab-buffer tab)))
    (put tab-sym 'risky-local-variable t)
    (set-default tab-sym (tab-group:tab-button-label tab label))
    (with-current-buffer buffer
      (set (make-local-variable tab-sym)
           (tab-group:active-tab-button-label tab (or selected label)))
      (put tab-sym 'permanent-local t))))

(defun tab-group:tab-label (tab &optional index width)
  "Return a single TAB label and update a symbol for the label of
TAB.  If INDEX is non-nil, then it is used for the index number
of TAB formatted by `tab-group:tab-index-format'.  If WIDTH is
non-nill, it specifies the max width, which the label and a
single separator of `tab-group:tab-separator' can occupy, and
the label is truncated to the width."
  (let* ((face 'tab-group:tabbar)
         (sep (tab-group:tab-separator tab))
         (tab-sym (tab-group:tab-tab tab)))
    (when index
      (setq index (let ((index (format tab-group:tab-index-format index)))
                    (tab-group:inheriting-propertize
                     index (symbol-value tab-sym)
                     '(face mouse-face tab-group:tab local-map)))))
    (setq index (or index ""))
    (when width
      (let* ((sep-width (tab-group:format-width index sep))
             (width (max 1 (- width sep-width)))
             (name (tab-group:tab-name tab))
             (truncated (tab-group:truncate name width))
             (selected (and tab-group:no-truncation-for-current-tab name)))
        (tab-group:set-label tab truncated selected)))
    (list sep index tab-sym)))

(defun tab-group:make-tab-list (tabs &optional num start width)
  "Make a list of labels of TABS.
If NUM is non-nil, then the label is formatted with its index
number (0 for the first element of TABS).  If START is non-nil,
it specifies the scroll position; the returned list starts from
the START-th tab in TABS.  If WIDTH is non-nil, it specifies the
max width of a single tab label."
  (let ((tablist (list "")) (start (or start 0)) (i 0))
    (dolist (tab tabs)
      (when (>= i start)
        (let* ((index (tab-group:tab-index tab))
               (i (or (and (eq tab-group:tab-index 'fixed) index) i))
               (show tab-group:show-tab-index)
               (show (and (not (eq show 'never)) (or num (eq show 'always))))
               (label (tab-group:tab-label tab (and show i) width)))
          (nconc tablist (list label))))
      (setq i (1+ i)))
    (cdr tablist)))

(defun tab-group:adjust-tab-list (tablist)
  "Make the separator of the first element of TABLIST transparent.
TABLIST is a list of tab labels.  If the current frame is on a
terminal and `tab-group:show-group-name' is nil, then the
separator is simply removed."
  (let* ((first (car-safe tablist))
         (fill-face (tab-group:fill-face 'tab-group:tab-separator)))
    (when first
      (if (or window-system tab-group:show-group-name)
          (setcar first (propertize (car first) 'face fill-face))
        (setcar first ""))))
  tablist)

(defun tab-group:tab-list (tabs &optional num start width)
  "Return a adjusted list of labels of TABS.
The meaning of arguments are the same in
`tab-group:make-tab-list'."
  (let ((tablist (tab-group:make-tab-list tabs num start width)))
    (tab-group:adjust-tab-list tablist)))

(defun tab-group:truncated-tab-list (tabs &optional num start width)
  "Return a list of truncated labels of TABS.
The meaning of arguments are the same in `tab-group:tab-list'
except WIDTH, which specifies the total width of the tab labels.
If WIDTH is nil, the window width is used."
  (let ((width (or width (tab-group:tabbar-default-width))) label-width)
    (when tab-group:truncate
      (setq label-width (/ width (max (- (length tabs) (or start 0)) 1))))
    (funcall tab-group:tab-list-function tabs num start label-width)))

(defun tab-group:scroll-limit (tab tabs formats width)
  "Return lower and upper bound of scroll position to make TAB
visible.  TABS are the list of tabs to be scrolled and FORMATS
are label constructs corresponding to TABS.  WIDTH specifies the
total width of the tab label list."
  (let ((i 0) left)
    (while tabs
      (setq left (cons (car formats) left))
      (if (eq tab (car tabs))
          (setq tabs nil)
        (setq tabs (cdr tabs) formats (cdr formats) i (1+ i))))
    (let ((j (1+ i)) (w 0))
      (while left
        (if (<= (setq w (+ w (tab-group:format-width (car left)))) width)
            (setq left (cdr left) j (1- j))
          (setq left nil)))
      (cons (min i j) i))))

(defun tab-group:tabbar-scroll-limit (tabs &optional num start width)
  "Calculate scroll limit of tabbar i.e. the lower and upper
bound of scroll position to make the last element in TABS
visible.  See `tab-group:scroll-limit' for the detail."
  (let* ((tabbar (list ""))
         (fringe-width (nth 0 (window-inside-edges)))
         (fringe-width (- fringe-width (string-width tab-group:tab-separator)))
         (fringe-width (max 0 fringe-width))
         (fringe (make-string fringe-width ? ))
         (group (tab-group:current-group)))
    (when group
      (when tab-group:show-scroll-button
        (push (list tab-group:scroll-button-left
                    tab-group:scroll-button-right) tabbar))
      (if tab-group:show-group-name
          (push (tab-group:group-name group) tabbar)
        (push fringe tabbar))
      (let* ((start (or start 0))
             (width (or width (tab-group:tabbar-default-width)))
             (width (- width (tab-group:format-width tabbar)))
             (tablist (tab-group:truncated-tab-list tabs num 0 width))
             (last (car-safe (last tabs))))
        (tab-group:scroll-limit last tabs tablist width)))))

(defun tab-group:scroll-button (which enabled)
  "Return a propertized scroll button.
WHICH must be `left' or `right'.  If ENABLED is non-nil, then the
button has enabled face.  Otherwise, the button has disabled
face.  If `tab-group:show-scroll-button' is nil, or,
`tab-group:show-scroll-button' is `on-demand' and the button has
disabled face, then an empty string is returned."
  (let* ((sym-name (concat "tab-group:scroll-button-" (symbol-name which)))
         (button (symbol-value (intern sym-name))))
    (cond
     ((and tab-group:show-scroll-button enabled)
      button)
     ((and (not (eq tab-group:show-scroll-button 'on-demand)) (not enabled))
      (let ((face 'tab-group:scroll-button:disabled))
        (propertize button 'face face 'mouse-face face 'local-map nil)))
     (t ""))))

(defun tab-group:update-scroll-buttons (pos limit)
  "Update `tab-group:scroll-buttons' according to the scroll
limit of tabbar."
  (let* ((left (tab-group:scroll-button 'left (> pos 0)))
         (right (tab-group:scroll-button 'right (< pos (car limit)))))
    (setq tab-group:scroll-buttons (list left right))))

(defun tab-group:scroll (pos &optional update)
  "Scroll and update visibility.
POS specifies the position to scroll.  If UPDATE is non-nil, call
`tab-group:update-tabbar'."
  (let* ((group (tab-group:current-group))
         (tabs (and group (tab-group:group-tabs group)))
         (limit (tab-group:tabbar-scroll-limit tabs)))
    (when group
      (let ((pos (min (max 0 pos) (car limit))))
        (if update
            (tab-group:update-tabbar group nil pos)
          (tab-group:update-scroll-buttons pos limit))))))

(defun tab-group:force-recalculate-scroll-limit (group)
  "Recalculate scroll limit and update tabbar."
  (when (and tab-group:scroll tab-group:show-scroll-button)
    ;; force recalculate scroll limit
    (tab-group:update-tabbar group)
    (format-mode-line (tab-group:group-tabbar group))))

(defun tab-group:scrolled-tab-list (tabs &optional num start width)
  "Return a list of scrolled labels of TABS.
The meaning of arguments are the same in
`tab-group:truncated-tab-list'.  The returned list is scrolled to
make the current tab visible (i.e. this function takes account of
the current buffer).  This function also makes buffer's local
cache of the tab list."
  (when tab-group:current-tab
    (let* ((width (or width (tab-group:tabbar-default-width)))
           (tab tab-group:current-tab)
           (group (and (stringp tabs) (tab-group:group tabs)))
           (group (or group (tab-group:tab-group tab)))
           (cache tab-group:tablist-cache))
      (when (not (eq (tab-group:tab-group tab) group))
        (setq group (tab-group:tab-group tab)))
      (or cache
          (let* ((tabs (if (stringp tabs) (tab-group:group-tabs group) tabs))
                 (tablist (tab-group:truncated-tab-list tabs num 0 width))
                 (scroll (or start 0)))
            (when (not start)
              (let* ((limit (tab-group:scroll-limit tab tabs tablist width))
                     (last (or tab-group:last-scroll-pos 0)))
                (setq scroll
                      (cond ((<= (cdr limit) last) (cdr limit))
                            ((<= last (car limit)) (car limit))
                            (t last)))))
            (setq tablist (nthcdr scroll tablist))
            (setq tab-group:last-scroll-pos scroll)
            (tab-group:scroll scroll)
            (setq tab-group:tablist-cache tablist) ; make cache
            tablist)))))

(defun tab-group:update-tabbar (group &optional num start width)
  "Reconstruct and redisplay tabbar of GROUP.
If NUM is non-nil, then the tab label is formatted with its index
number (0 for the first element of the tabs of GROUP).  If START
is non-nil, it specifies the scroll position; the returned list
starts from the START-th tab in the tabs of GROUP.  If WIDTH is
non-nil, it specifies the max width of the tabbar."
  (let* ((tabbar (tab-group:group-tabbar group))
         (name (tab-group:group-name group))
         (group-name (funcall (or (tab-group:get 'group-label group)
                                  'tab-group:group-button-label) group))
         (fill-face (tab-group:fill-face 'tab-group:tabbar))
         (fill (propertize "%-" 'face fill-face))
         (fringe-width (nth 0 (window-inside-edges)))
         (fringe-width (- fringe-width (string-width tab-group:tab-separator)))
         (fringe-width (max 0 fringe-width))
         (fringe (propertize (make-string fringe-width ? ) 'face fill-face))
         (tabs (tab-group:group-tabs group)))
    (set tabbar (list ""))
    (when tab-group:show-scroll-button
      (when start (tab-group:scroll start))
      (push 'tab-group:scroll-buttons (symbol-value tabbar)))
    (if tab-group:show-group-name
        (push group-name (symbol-value tabbar))
      (push fringe (symbol-value tabbar)))
    (let* ((fringe-width (tab-group:format-width tabbar))
           (width (or width (tab-group:tabbar-default-width)))
           (width (- width fringe-width))
           (tablist (tab-group:truncated-tab-list tabs num start width))
           (sfun `(tab-group:scrolled-tab-list ,name ,num ,start ,width))
           (efun `(:eval (format-mode-line ,sfun)))
           (tab (or tab-group:current-tab (car-safe tabs)))
           (buffer (if tab (tab-group:tab-buffer tab) (current-buffer))))
      (with-current-buffer buffer
        (setq tab-group:tablist-cache nil)) ; clear cache
      (setq tablist `(tab-group:scroll ,efun ,tablist))
      (nconc (symbol-value tabbar) (list tablist fill))))
  (tab-group:display-update))

(defun tab-group:show-tabbar-sym (&optional show)
  (let ((show (or show tab-group:show-tabbar)))
    (cond
     ((eq show 'header-line) 'header-line-format)
     ((eq show 'mode-line) 'mode-line-format)
     (t nil))))

(defun tab-group:show-tabbar ()
  "Show tabbar according to the value of symbol
`tab-group:show-tabbar'."
  (let ((sym (tab-group:show-tabbar-sym)))
    (unless tab-group:restore-tabbar
      (setq tab-group:restore-tabbar
            (cons tab-group:show-tabbar (symbol-value sym))))
    (when sym (set sym 'tab-group:tabbar))))

(defun tab-group:restore-tabbar ()
  "Restore the saved value of `header-line-format' or
`mode-line-format'."
  (when tab-group:restore-tabbar
    (let ((sym (tab-group:show-tabbar-sym (car tab-group:restore-tabbar))))
      (when sym (set sym (cdr tab-group:restore-tabbar)))
      (setq tab-group:restore-tabbar nil))))


;;; tab search

(defun tab-group:search-pattern (pattern string)
  "Search PATTERN in STRING and put text properties to STRING to
highlight PATTERN in STRING."
  (let* ((patterns (split-string pattern)) (match t)
         (face (get-text-property 0 'face string))
         (face (if (symbolp face) face 'tab-group:tab))
         (face `(:inherit ,face :inherit tab-group:match)))
    (while patterns
      (let ((pat (car patterns)))
        (when (setq match (and match (string-match (regexp-quote pat) string)))
          (let ((beg (match-beginning 0)) (end (match-end 0)))
            (put-text-property beg end 'face face string))))
      (setq patterns (cdr patterns)))
    (and match string)))

(defun tab-group:search-tab-list (group &optional num start width)
  "Return a list of tab labels which match with current search pattern.
The meaning of arguments are the same in `tab-group:tab-list'."
  (let ((list (tab-group:make-tab-list group num start width)) result)
    (while list
      (let* ((buffer tab-group:search-current-buffer)
             (format (cdar list))
             (label (format-mode-line format nil nil buffer))
             (label (tab-group:search-pattern tab-group:search-pattern label)))
        (when label (push (list (caar list) label) result)))
      (setq list (cdr list)))
    (setq tab-group:search-result
          (tab-group:adjust-tab-list (nreverse result)))))

(defun tab-group:update-search (beg end len)
  "Filter tabbar by the current search pattern.
The values of arguments are not in use."
  (let ((tab-group:search-pattern (minibuffer-contents)))
    (with-current-buffer tab-group:search-current-buffer
      (let ((tab-group:tab-list-function 'tab-group:search-tab-list)
            (tab-group:show-scroll-button nil))
        (tab-group:update-tabbar tab-group:search-group t))))
  (when (and tab-group:select-single-match
             (= 1 (length tab-group:search-result)))
    (exit-minibuffer)))

(defun tab-group:start-search ()
  "Setup hooks in the search minibuffer."
  (remove-hook 'minibuffer-setup-hook tab-group:start-search-function)
  (add-hook 'after-change-functions tab-group:update-search-function nil t)
  (with-current-buffer tab-group:search-current-buffer
    (setq tab-group:search-scroll-restore
          (if (local-variable-p 'tab-group:scroll)
              tab-group:scroll
            'local))
    (set (make-local-variable 'tab-group:scroll) nil)
    (let ((tab-group:show-scroll-button nil))
      (tab-group:update-tabbar tab-group:search-group t))))

(defun tab-group:end-search ()
  "Clean hooks in the search minibuffer."
  (remove-hook 'minibuffer-exit-hook tab-group:end-search-function)
  (remove-hook 'after-change-functions tab-group:update-search-function t)
  (with-current-buffer tab-group:search-current-buffer
    (if (eq tab-group:search-scroll-restore 'local)
        (kill-local-variable 'tab-group:scroll)
      (setq tab-group:scroll tab-group:search-scroll-restore))
    (tab-group:update-tabbar tab-group:search-group)))

(defsubst tab-group:search-result-1 (x)
  (let ((x (if (listp x) (car x) x)))
    (get-text-property 0 'tab-group:tab x)))

(defun tab-group:search-result ()
  (mapcar #'tab-group:search-result-1 tab-group:search-result))

(defun tab-group:search (group prompt)
  "Search tabs in GROUP and return a list of matched tabs.
PROMPT is a prompt for asking search patterns."
  (let ((tab-group:search-group group)
        (tab-group:search-current-buffer (current-buffer)))
    (add-hook 'minibuffer-setup-hook tab-group:start-search-function)
    (add-hook 'minibuffer-exit-hook tab-group:end-search-function)
    (condition-case err
        (read-string prompt)
      (quit
       (funcall tab-group:end-search-function)
       (signal (car err) (cdr err))))
    (tab-group:search-result)))

(defun tab-group:find (pattern)
  "Find tabs in GROUP whose name matches with PATTERN."
  (let ((group (tab-group:current-group)))
    (let ((tab-group:search-current-buffer (current-buffer))
          (tab-group:search-pattern pattern)
          (tab-group:tab-list-function 'tab-group:search-tab-list)
          (tab-group:scroll nil) (tab-group:show-scroll-button nil))
      (tab-group:update-tabbar group t))
    (let ((tabs (tab-group:search-result)))
      (tab-group:update-tabbar group)
      tabs)))


;;; tab movement

(defsubst tab-group:move-separator ()
  (let ((face 'tab-group:tab-separator))
    (propertize (make-string (string-width tab-group:tab-separator) ? )
                'face (tab-group:fill-face face))))

(defun tab-group:move-format (label &optional after)
  "Put a move target indicator to LABEL.
LABEL is a tab label construct.  If AFTER is non-nil, the
indicator appears at the end.  Otherwise, the indicator appears
at the beginning."
  (let* ((m-face 'tab-group:move-target)
         (ins-face (tab-group:fill-face 'tab-group:tabbar))
         (ins (propertize tab-group:move-target
                          'face ins-face 'mouse-face m-face))
         (sep (tab-group:move-separator))
         (zw tab-group:zero-width-space))
    (if (< (length sep) (string-width ins))
        (setq sep "")
      (setq sep (substring sep (string-width ins))))
    (setq label (propertize label 'mouse-face m-face))
    (if after
        (concat label ins zw sep)
      (concat sep zw ins label))))

(defun tab-group:move-tab-list (group &optional num start width)
  "Return a list of tab labels with move target indicators.
The meaning of arguments are the same in `tab-group:tab-list'."
  (let ((list (tab-group:make-tab-list group num start width)) result after)
    (while list
      (let* ((buffer (current-buffer))
             (label (format-mode-line (cdr (car list)) nil nil buffer))
             (sep (tab-group:move-separator)))
        (when label
          (if (eq (get-text-property 0 'tab-group:tab label)
                  tab-group:move-current-tab)
              (setq after t label (concat sep label sep))
            (setq label (tab-group:move-format label after)))
          (push label result)))
      (setq list (cdr list)))
    (nreverse result)))

(defun tab-group:move-update-tabbar (window)
  "Update tabbars on WINDOW."
  (with-selected-window window
    (with-current-buffer (window-buffer window)
      (let ((group (tab-group:current-group)))
        (when group (tab-group:update-tabbar group))))))

(defun tab-group:move-start (event)
  "Start tab movement mode."
  (interactive "@e")
  (let ((tab (tab-group:event-tab event)))
    (setq tab-group:move-tab-list-restore tab-group:tab-list-function
          tab-group:tab-list-function 'tab-group:move-tab-list
          tab-group:move-current-tab tab)
    (track-mouse
      (let ((event (read-event)))
        (if (mouse-movement-p event)
            (walk-windows #'tab-group:move-update-tabbar nil t)
          ;; push back event (for click event)
          (push event unread-command-events))))))

(defun tab-group:move-end ()
  "Stop tab movement mode."
  (interactive)
  (setq tab-group:tab-list-function
        (or tab-group:move-tab-list-restore 'tab-group:tab-list)
        tab-group:move-tab-list-restore nil
        tab-group:move-current-tab nil)
  (walk-windows #'tab-group:move-update-tabbar nil t))


;;; tab

(defun tab-group:set-current-tab (tab)
  (setq tab-group:current-tab tab
        tab-group:tabbar
        (and tab (tab-group:group-tabbar (tab-group:current-group))))
  tab)

(defun tab-group:default-name (buffer)
  "Return a name for a newly created tab.  It is inherited from
an existing tab which shares the BUFFER, or the buffer name is
used if there is no such tab."
  (with-current-buffer buffer
    (cond
     (tab-group:current-tab (tab-group:tab-name tab-group:current-tab))
     (tab-group:buffer-original-name)
     (t (buffer-name)))))

(defun tab-group:buffer-tab-in-group (group)
  "Return a tab which is owned by the current tab and whose group
is GROUP or nil if there is no such tab."
  (let ((tabs tab-group:buffer-tabs) result)
    (while (and tabs (not result))
      (let ((tab (car tabs)))
        (when (eq (tab-group:tab-group tab) group) (setq result tab))
        (setq tabs (cdr tabs))))
    result))

(defun tab-group:tab-position (tab)
  "Return the position of TAB in the tabbar starting from 0."
  (let* ((group (tab-group:tab-group tab))
         (tabs (tab-group:group-tabs group)) (i 0))
    (while tabs
      (if (eq tab (car tabs)) (setq tabs nil) (setq tabs (cdr tabs) i (1+ i))))
    i))

(defmacro tab-group:with-splitting-tabs (tab bind &rest body)
  (declare (indent 2))
  (let ((group (nth 0 bind)) (tabs (nth 1 bind))
        (left (nth 2 bind)) (right (nth 3 bind)))
    `(when ,tab
       (let* ((,group (tab-group:tab-group ,tab))
              (,tabs (tab-group:group-tabs ,group))
              (pair (tab-group:split ,tab ,tabs)))
         (let ((,left (car-safe pair)) (,right (cdr-safe pair)))
           (when ,tabs ,@body))))))

(defun tab-group:pop-tab (&optional tab)
  "Pop TAB from its group.
It handles updating tabbar of the group."
  (let* ((tab (or tab tab-group:current-tab))
         (group (tab-group:tab-group tab))
         (buffer (tab-group:tab-buffer tab)))
    (when group
      (setf (tab-group:group-tabs group)
            (delq tab (tab-group:group-tabs group)))
      (setf (tab-group:tab-group tab) nil)
      (let ((tabs (tab-group:group-tabs group)))
        (dolist (tab tabs)
          (with-current-buffer (tab-group:tab-buffer tab)
            (setq tab-group:tablist-cache nil)))) ; clear cache
      (with-current-buffer buffer
        (tab-group:update-tabbar group)))
    (with-current-buffer (tab-group:tab-buffer tab)
      (setq tab-group:buffer-tabs (delq tab tab-group:buffer-tabs))
      (tab-group:set-current-tab (car-safe tab-group:buffer-tabs)))
    tab))

(defun tab-group:push-tab (tab group &optional position)
  "Push TAB into GROUP.
If the tab is already in some group, then it is popped from the
group.  If POSITION is non-nil, it specifies the position where
TAB is inserted."
  (tab-group:pop-tab tab)
  (setf (tab-group:tab-group tab) group)
  (let* ((tabs (tab-group:group-tabs group)) (len (length tabs))
         (position (min len (max 0 (or position len))))
         (tabs (cons nil tabs)) (tail (nthcdr position tabs)))
    (setcdr tail (cons tab (cdr tail)))
    (setf (tab-group:group-tabs group) (cdr tabs))
    (dolist (tab (cdr tabs))
      (with-current-buffer (tab-group:tab-buffer tab)
        (setq tab-group:tablist-cache nil)))) ; clear cache
  (with-current-buffer (tab-group:tab-buffer tab)
    (let ((name (tab-group:tab-name tab)))
      (tab-group:rename tab name))
    (tab-group:update-tabbar group))
  tab)

(defun tab-group:move-tab (tab position &optional current-tab group)
  "Move TAB to POSITION.
If GROUP is non-nil, it specifies the target group that TAB is
inserted.  If CURRENT-TAB is non-nil, it also sets TAB as the
current tab of the current buffer."
  (let ((group (or group (tab-group:tab-group tab))))
    (tab-group:pop-tab tab)
    (tab-group:push-tab tab group position)
    (with-current-buffer (tab-group:tab-buffer tab)
      (when current-tab (tab-group:set-current-tab tab))
      (add-to-list 'tab-group:buffer-tabs tab))
    (tab-group:show-tabbar)))

(defun tab-group:prepare-new-tab (&optional name buffer group)
  "Make a new tab of BUFFER named NAME in GROUP.
If BUFFER is nil, the current buffer is used.  If NAME is nil, a
default name (normally the buffer name) is used (see the
documentation of `tab-group:default-name' for the detail).  If
GROUP is nil, the value of `tab-group:default-group' is used."
  (let* ((buffer (or buffer (current-buffer)))
         (name (or name (tab-group:default-name buffer)))
         (group (or (tab-group:group group) tab-group:default-group))
         (id (setq tab-group:tab-id (1+ tab-group:tab-id))))
    (with-current-buffer buffer
      (when (not tab-group:local-mode) (tab-group:local-mode 1))
      (tab-group:set-current-tab
       (tab-group:make-tab :name name :id id :buffer buffer :group group)))))

(defun tab-group:new-tab (&optional name buffer group position)
  "Add a new tab of BUFFER named NAME to GROUP.
If POSITION is non-nil, it specifies the position where the tab
is inserted.  See the documentation of
`tab-group:prepare-new-tab' for the default values of the other
arguments when they are omitted.

This function enables `tab-group:local-mode' for BUFFER."
  (let* ((tab (tab-group:prepare-new-tab name buffer group))
         (group (tab-group:tab-group tab))
         (buffer (tab-group:tab-buffer tab))
         (tab-sym (tab-group:tab-sym group tab))
         (index (tab-group:get 'index group)))
    (setf (tab-group:tab-index tab) index)
    (tab-group:set 'index (1+ index) group)
    (with-current-buffer buffer
      (setf (tab-group:tab-tab tab) tab-sym)
      (tab-group:move-tab tab position nil group)
      (when (< 0 (tab-group:get 'group-buffer-mode group))
        (tab-group:group-buffer-local-mode-on)))
    tab))

(defun tab-group:unselect (tab &optional group)
  "Invoke hook `tab-group:unselect-tab-hook' with the arguments
if TAB was selected.  If GROUP is nil, the group of TAB is used."
  (when (tab-group:tab-selected-p tab)
    (let ((group (or group (and tab (tab-group:tab-group tab)))))
      (run-hook-with-args 'tab-group:unselect-tab-hook tab group))))


;;; tab list

(define-derived-mode tab-group:tab-list-mode Buffer-menu-mode
  "Tab List"
  "Major mode for Tab List.
Tab List is a list of buffers similar to that in
`Buffer-menu-mode' but tab names instead of buffer names are
shown in the list and buffers are restricted to those in a
specific group."
  ;; use our own refresh method
  (remove-hook 'tabulated-list-revert-hook 'list-buffers--refresh t)
  (add-hook 'tabulated-list-revert-hook #'tab-group:tab-list-refresh nil t)
  (when (fboundp 'Buffer-menu-revert-function) ; for older versions
    (set (make-local-variable 'revert-buffer-function)
         #'(lambda (ignore1 ignore2) (tab-group:tab-list-refresh))))
  ;; activate tab mode
  (tab-group:local-mode 1))

;; redefine Buffer-menu commands - this is needed to ensure invocation
;; of `tab-group:select-tab-hook' and `tab-group:unselect-tab-hook'
(defmacro tab-group:with-selecting-this-window (&rest body)
  (declare (indent 0))
  `(progn
     (let ((group (tab-group:current-group)))
       ,@body
       (let ((tabs (cons tab-group:current-tab tab-group:buffer-tabs)) tab)
         (while tabs
           (when (and (car tabs) (eq (tab-group:tab-group (car tabs)) group))
             (setq tab (car tabs) tabs nil))
           (setq tabs (cdr tabs)))
         (when tab (tab-group:select tab))))))
(defmacro tab-group:define-select-this-window (sym)
  (let ((fun (intern (concat "tab-group:menu-" (symbol-name sym))))
        (original (intern (concat "Buffer-menu-" (symbol-name sym)))))
    `(progn
       (push (cons ',original ',fun) tab-group:tab-list-mode-remap)
       (defun ,fun ()
         ,(documentation original)
         (interactive)
         (tab-group:with-selecting-this-window
           (call-interactively ',original))))))
(tab-group:define-select-this-window select)
(tab-group:define-select-this-window 1-window)
(tab-group:define-select-this-window 2-window)
(tab-group:define-select-this-window this-window)
(tab-group:define-select-this-window other-window)
(tab-group:define-select-this-window mouse-select)
(tab-group:define-select-this-window view)
(tab-group:define-select-this-window view-other-window)
(let ((map tab-group:tab-list-mode-map))
  (dolist (elt tab-group:tab-list-mode-remap)
    (define-key map (vector 'remap (car elt)) (cdr elt))))

(defsubst tab-group:tab-list-pretty-name (name)
  (propertize name
              'font-lock-face 'buffer-menu-buffer
              'mouse-face 'highlight))

(defsubst tab-group:tab-list-tab+size-width ()
  (or (and (boundp 'Buffer-menu-name-width)
           (boundp 'Buffer-menu-size-width)
           (+ Buffer-menu-name-width Buffer-menu-size-width))
      (and (boundp 'Buffer-menu-name+size-width)
           Buffer-menu-name+size-width)
      tab-group:tab-list-tab+size-width))

(defun tab-group:tab-list-buffer-name-and-file (&optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (when tab-group:current-tab
      (let* ((name (tab-group:tab-name tab-group:current-tab))
             (file (or buffer-file-name tab-group:buffer-original-file))
             (file (and file (abbreviate-file-name file)))
             (proc (get-buffer-process (current-buffer)))
             (pid (and proc (process-id proc)))
             (tty (and proc (process-tty-name proc)))
             (pname (and proc (process-name proc)))
             (pname (and pname (format "%d %s %s" pid tty pname)))
             (cmd (and proc (process-command proc)))
             (cmd (and cmd (mapconcat #'identity cmd " ")))
             (align (+ (tab-group:tab-list-tab+size-width)
                       Buffer-menu-mode-width
                       tab-group:tab-list-process-name-width 3 4))
             (space (propertize " " 'display `(space . (:align-to ,align))))
             (cmd (and cmd (concat pname space " (" cmd ")"))))
        (cons (tab-group:tab-list-pretty-name name) (or file cmd ""))))))

(defun tab-group:tab-list-set-entry-tab-name (entry)
  (let* ((buffer (car entry)) (slot (cadr entry))
         (name-and-file (tab-group:tab-list-buffer-name-and-file buffer)))
    (aset slot 3 (or (car-safe name-and-file) ""))
    (aset slot 6 (or (cdr name-and-file) ""))))

;; dummy
(defun tab-group:tab-list-refresh (&optional group old-buffer) nil)
(defun tab-group:tab-list-tab+size (name size &optional nprops sprops) nil)

(eval-and-compile
  (if (and (fboundp 'list-buffers--refresh)
           (fboundp 'tabulated-list-init-header)
           (boundp 'Buffer-menu-name-width)
           (boundp 'Buffer-menu-size-width)
           (boundp 'tabulated-list-format)
           (boundp 'tabulated-list-entries))
      (defun tab-group:tab-list-refresh (&optional group old-buffer)
        "Make tab list buffer content.
This function uses internal functions of `Buffer-menu-mode' and
`tabulated-list-mode', which are introduced in Emacs 24 (devel)."
        (let* ((group (or group (tab-group:current-group)))
               (tabs (tab-group:group-tabs group))
               (buffer-list (mapcar #'tab-group:tab-buffer tabs)))
          (when Buffer-menu-files-only
            ;; filter out non-file buffers
            (let* ((head (cons nil buffer-list)) (rest head))
              (while (cdr rest)
                (let* ((buffer (cadr rest))
                       (file (with-current-buffer buffer
                               (or buffer-file-name
                                   tab-group:buffer-original-file))))
                  (if (not file)
                      (setcdr rest (cddr rest))
                    (setq rest (cdr rest)))))
              (setq buffer-list (cdr head))))
          ;; make buffer list
          (list-buffers--refresh buffer-list old-buffer)
          ;; reformat entries
          ;; - Rename "Buffer" column header to "Tab"
          ;; - show tab name instead of buffer name
          (let ((name-width Buffer-menu-name-width)
                (size-width Buffer-menu-size-width) width)
            (if (and (boundp 'Buffer-menu-buffer+size-width)
                     (symbol-value 'Buffer-menu-buffer+size-width))
                (setq name-width (- (symbol-value
                                     'Buffer-menu-buffer+size-width)
                                    size-width)))
            (aset tabulated-list-format 3 `("Tab" ,name-width t))
            (aset tabulated-list-format 6 '("File/Process" 1 t)))
          (mapc #'tab-group:tab-list-set-entry-tab-name tabulated-list-entries)
          ;; initialize header
          (tabulated-list-init-header)))
    (defun tab-group:tab-list-tab+size (name size &optional nprops sprops)
      (let ((width (tab-group:tab-list-tab+size-width)))
        (when (> (+ (string-width name) (string-width size) 2) width)
          (setq name
                (tab-group:truncate name (- width
                                            (max (string-width size) 3) 2))))
        (setq name (apply 'propertize (cons name nprops)))
        (setq size (apply 'propertize (cons size sprops)))
        (let* ((name+space-width (- width (string-width size)))
               (space-width (- name+space-width (string-width name))))
          (concat name
                  (propertize (make-string space-width ?\s)
                              'display `(space :align-to
                                               ,(+ 4 name+space-width)))
                  size))))
    (defun tab-group:tab-list-refresh (&optional group old-buffer)
      "Make tab list buffer content.
This function gives least functionalities of tab list for the
older versions of Emacs."
      (let* ((group (or group (tab-group:current-group)))
             (tabs (and group (tab-group:group-tabs group)))
             (pos 0) (mode-width Buffer-menu-mode-width)
             (mode-end (make-string (- mode-width 2) ?\s))
             (header (concat "CRM "
                             (tab-group:tab-list-tab+size "Tab" "Size")
                             "  Mode" mode-end "File/Process"))
             (inhibit-read-only t) point)
        (erase-buffer)
        (while (string-match "[ \t\n]+" header pos)
          (setq pos (match-end 0))
          (put-text-property (match-beginning 0) pos
                             'display `(space :align-to ,pos) header))
        (put-text-property 0 3 'face 'fixed-pitch header)
        (setq header
              (concat (propertize " " 'display '(space :align-to 0)) header))
        (setq header-line-format header)
        (when tabs
          (dolist (tab tabs)
            (let ((buffer (tab-group:tab-buffer tab)) bits file name mode)
              (with-current-buffer buffer
                (setq bits (string (if (eq buffer old-buffer) ?. ?\s)
                                   (if buffer-read-only ?% ?\s)
                                   (if (buffer-modified-p) ?* ?\s)
                                   ?\s))
                (let ((tab-and-file (tab-group:tab-list-buffer-name-and-file)))
                  (setq file (cdr tab-and-file)))
                (setq mode (format-mode-line mode-name nil nil buffer))
                (when mode-line-process
                  (let ((process
                         (format-mode-line mode-line-process nil nil buffer)))
                    (setq mode (concat mode process))))
                (setq name (tab-group:tab-name tab)))
              (when (or (not Buffer-menu-files-only) file)
                (when (eq buffer old-buffer) (setq point (point)))
                (setq name
                      (propertize (tab-group:tab-list-pretty-name name)
                                  'buffer-name (buffer-name buffer)
                                  'buffer buffer))
                (when (> (string-width mode) mode-width)
                  (setq mode (truncate-string-to-width mode mode-width)))
                (insert bits
                        (tab-group:tab-list-tab+size
                         (tab-group:tab-list-pretty-name name)
                         (int-to-string (buffer-size buffer))
                         `(buffer-name ,(buffer-name buffer) buffer ,buffer))
                       "  " mode)
                (when file
                  (indent-to (+ tab-group:tab-list-column
                                (tab-group:tab-list-tab+size-width)
                                mode-width 4) 1)
                  (insert (abbreviate-file-name file)))
                (insert "\n"))))
          (when point (goto-char point))))))

  (if (fboundp 'tabulated-list-print)
      (defun tab-group:tab-list-print () (tabulated-list-print))
    (defun tab-group:tab-list-print () nil)))

(defun tab-group:tab-list-tab (group)
  "Make a tab (and a buffer) of Tab List for GROUP.
The tab is not shown in the tabbar."
  (let* ((name (tab-group:group-name group))
         (list-name (concat "*" name " tabs*"))
         (tab (tab-group:group-list group))
         (buffer (and tab (tab-group:tab-buffer tab)))
         (buffer (or (and (buffer-live-p buffer) buffer)
                     (get-buffer-create list-name)))
         (tab (or tab (tab-group:prepare-new-tab name buffer group)))
         (old-buffer (current-buffer)))
   (with-current-buffer buffer
     (tab-group:tab-list-mode)
     ;; rename to *<group name> tabs*
     (rename-buffer list-name)
     ;; tab setup
     (tab-group:set-current-tab tab)
     (setf (tab-group:tab-buffer tab) buffer)
     (tab-group:group-buffer-local-mode
      (tab-group:get 'group-buffer-mode group))
     ;; show list
     (setq Buffer-menu-files-only nil)
     (tab-group:tab-list-refresh group old-buffer)
     (tab-group:tab-list-print))
   (setf (tab-group:group-list group) tab)
   tab))

(defun tab-group:list (group)
  "Show list of tabs in GROUP."
  (interactive (list (tab-group:current-group)))
  (when group (tab-group:select (tab-group:tab-list-tab group))))


;;; group buffer mode

(define-minor-mode tab-group:group-buffer-local-mode
  "Minor mode for group buffer.
Group buffer is a singleton buffer for a specific group, i.e.,
buffers in the group except the selected buffer are hidden and
the selected buffer is identified by the group name in the buffer
list.  Selecting other tabs in the group automatically changes
the group's selected buffer.  If there is no buffer suitable for
the selected buffer (this happens when a buffer has multiple tabs
in different groups, or the selected buffer is popped without
selecting other tabs), the tab list buffer is selected.  With
this mode, the selected tab list buffer cannot be killed.

Do not invoke this function directly.  Use
`tab-group:group-buffer-mode' to enable/disable group buffer
mode."
  :group 'tab-group:group-buffer
  (if tab-group:group-buffer-local-mode
      ;; on
      (unless (or tab-group:buffer-original-id
                  (not (tab-group:group-buffer-p)))
        (let* ((name (buffer-name))
               (tab tab-group:current-tab)
               (buffer (and tab (tab-group:tab-buffer tab))))
          (setq tab-group:buffer-original-id mode-line-buffer-identification)
          (unless tab-group:buffer-original-name
            (setq tab-group:buffer-original-name name))
          (unless tab-group:buffer-original-file
            (setq tab-group:buffer-original-file (buffer-file-name)))
          (setq mode-line-buffer-identification
                (propertized-buffer-identification
                 tab-group:buffer-original-name))
          (add-hook 'tab-group:local-mode-exit-hook
                    #'tab-group:group-buffer-local-mode-off nil t)
          (add-hook 'tab-group:select-tab-hook
                    #'tab-group:group-buffer-select nil t)
          (add-hook 'tab-group:unselect-tab-hook
                    #'tab-group:group-buffer-unselect nil t)
          (when (eq major-mode 'tab-group:tab-list-mode)
            (add-hook 'kill-buffer-query-functions
                      #'tab-group:group-buffer-prevent-kill-buffer nil t))
          (if (and tab (eq buffer (current-buffer)))
              (tab-group:group-buffer-select)
            (tab-group:group-buffer-rename tab nil))))
    ;; off
    (when tab-group:buffer-original-id
      (remove-hook 'tab-group:local-mode-exit-hook
                   'tab-group:group-buffer-local-mode-off t)
      (remove-hook 'tab-group:select-tab-hook
                   'tab-group:group-buffer-select t)
      (remove-hook 'tab-group:unselect-tab-hook
                   'tab-group:group-buffer-unselect t)
      (remove-hook 'kill-buffer-query-functions
                   'tab-group:group-buffer-prevent-kill-buffer t)
      (tab-group:group-buffer-set-name-and-file tab-group:buffer-original-name
                                                tab-group:buffer-original-file)
      (setq mode-line-buffer-identification tab-group:buffer-original-id
            tab-group:buffer-original-id nil
            tab-group:buffer-original-name nil
            tab-group:buffer-original-file nil
            tab-group:buffer-last-mtime nil))))

(defun tab-group:group-buffer-p ()
  (not (string= (buffer-name) "*Messages*")))

(defun tab-group:group-buffer-set-name-and-file (name file)
  (let ((flag (buffer-modified-p))
        (change-major-mode-with-file-name nil))
    (when (and (null file) (buffer-file-name))
      (set (make-local-variable 'tab-group:buffer-last-mtime)
           (visited-file-modtime)))
    (when (or (and (null file) (buffer-file-name))
              (and file (null (buffer-file-name))))
      (set-visited-file-name file t))
    (set-buffer-modified-p flag)
    (when (and (buffer-file-name) tab-group:buffer-last-mtime)
      (set-visited-file-modtime tab-group:buffer-last-mtime))
    (when (tab-group:group-buffer-p) (rename-buffer name t))))

(defsubst tab-group:group-buffer-name (group)
  (tab-group:group-name group))

(defun tab-group:group-buffer-rename (tab selected)
  (let ((group (and tab (tab-group:tab-group tab)))
        (buffer (and tab (tab-group:tab-buffer tab))))
    (when buffer
      (with-current-buffer buffer
        (when (and tab-group:group-buffer-local-mode
                   (eq tab-group:current-tab tab))
          ;; if TAB is not equal to `tab-group:current-tab', it means
          ;; that `tab-group:current-tab' is a tab in another group
          ;; than TAB's group; in that case, we must not change the
          ;; buffer name because the buffer can be the selected buffer
          ;; of the another group.
          (let (name (file tab-group:buffer-original-file))
            (cond
             ((or (not group) (>= 0 (tab-group:get 'group-buffer-mode group)))
              ;; we are not in group buffer mode in this group
              (setq name tab-group:buffer-original-name))
             (selected
              ;; we are in group buffer mode and the buffer is selected
              (setq name (concat (or (tab-group:get 'group-buffer-prefix group)
                                     tab-group:group-buffer-prefix "")
                                 (tab-group:group-buffer-name group))))
             (t
              ;; we are in group buffer mode and the buffer is not selected
              (setq name (concat " " tab-group:buffer-original-name)
                    file nil)))
            (tab-group:group-buffer-set-name-and-file name file)))))))

(defun tab-group:group-buffer-select ()
  (let* ((tab tab-group:current-tab)
         (group (and tab (tab-group:tab-group tab)))
         (tabs (and group (tab-group:group-tabs group)))
         (list (tab-group:group-list group)))
    (when (and tab group)
      (dolist (elt (if list (cons list tabs) tabs))
        ;; hide other buffers
        (tab-group:group-buffer-rename elt nil))
      ;; show selected buffer
      (tab-group:group-buffer-rename tab t))))

(defun tab-group:group-buffer-unselect (tab group)
  (when (and tab group (< 0 (tab-group:get 'group-buffer-mode group)))
    (tab-group:group-buffer-rename tab nil)
    (let ((tabs (tab-group:group-tabs group))
          (list (tab-group:tab-list-tab group)))
      (dolist (elt (cons tab tabs))
        ;; hide all buffers
        (tab-group:group-buffer-rename elt nil))
      ;; show the tab list buffer
      (tab-group:group-buffer-rename list t))))

(defun tab-group:group-buffer-prevent-kill-buffer ()
  nil)

(defun tab-group:group-buffer-local-mode-on ()
  (tab-group:group-buffer-local-mode 1))

(defun tab-group:group-buffer-local-mode-off ()
  (tab-group:group-buffer-local-mode 0))

(defun tab-group:group-buffer-carefully-disable ()
  "Disable group buffer mode if all groups of tabs owned by the
current buffer are not in group buffer mode."
  (let ((tabs tab-group:buffer-tabs) (disable t))
    (while tabs
      (let* ((tab (car tabs)) (group (tab-group:tab-group tab)))
        (when (< 0 (tab-group:get 'group-buffer-mode group))
          (setq disable nil tabs nil))
        (setq tabs (cdr tabs))))
    (when disable (tab-group:group-buffer-local-mode-off))
    (when (<= (tab-group:get 'group-buffer-mode) 0)
      (tab-group:group-buffer-rename tab-group:current-tab nil))))

(defun tab-group:group-buffer-mode (&optional arg)
  "Toggle group buffer mode for the group of the current tab.  It
enables/disables `tab-group:group-buffer-local-mode' in all
buffers of tabs in the group."
  (interactive)
  (let* ((tab tab-group:current-tab)
         (group (and tab (tab-group:tab-group tab)))
         (tabs (and group (tab-group:group-tabs group)))
         (list (and group (tab-group:group-list group))))
    (when group
      (let ((mode (or arg (- (or (tab-group:get 'group-buffer-mode) -1)))))
        (tab-group:set 'group-buffer-mode mode)
        (dolist (tab (if list (cons list tabs) tabs))
          (with-current-buffer (tab-group:tab-buffer tab)
            (if (< 0 mode)
                (tab-group:group-buffer-local-mode mode)
              (tab-group:group-buffer-carefully-disable))))
        (tab-group:group-buffer-select)))))


;;; auto-group mode

(defun tab-group:mode-derived-p (mode parents)
  "Return non-nil if MODE derives a mode in PARENTS."
  (let (derived)
    (while (and (not derived) mode)
      (if (memq mode parents)
          (setq derived t)
        (setq mode (get mode 'derived-mode-parent))))
    derived))

(defun tab-group:buffer-auto-group ()
  "Return a group name for the current buffer.
The name is determined by the major mode or the buffer name."
  (cond
   ((or (get-buffer-process (current-buffer))
        (tab-group:mode-derived-p major-mode tab-group:auto-process-modes))
    "Process")
   ((member (buffer-name) tab-group:auto-common-buffers)
    "Common")
   ((memq major-mode tab-group:auto-help-modes)
    "Help")
   ((memq major-mode tab-group:auto-mail-modes)
    "Mail")
   ((and (stringp mode-name) (string-match-p "[^ \r\n\t]" mode-name))
    mode-name)
   (t (let ((mode (symbol-name major-mode)))
        (if (string-match-p "-mode$" mode)
            (substring mode (- (length mode) (length "-mode")))
          mode)))))

(defun tab-group:buffer-auto-groups ()
  "Return a singleton list of a value retuend by
`tab-group:buffer-auto-group'."
  (list (tab-group:buffer-auto-group)))

(defun tab-group:buffer-auto-p ()
  "Return t if the current buffer is neither a minibuffer or
header-lined locally or in a major mode listed in
`tab-group:auto-exclude-modes' or named as listed in
`tab-group:auto-exclude-buffers' or named as uninteresting
non-file buffer i.e. the name starts with \" *\"."
  (not (or (minibufferp)
           (let ((sym (tab-group:show-tabbar-sym)))
             (and sym (buffer-local-value sym (current-buffer))))
           (memq major-mode tab-group:auto-exclude-modes)
           (member (buffer-name) tab-group:auto-exclude-buffers)
           (string-match-p "^ \\*" (buffer-name)))))

(defun tab-group:auto-new-group (name)
  "Create a new group named NAME if none exists.
The group is marked as auto-grouped group.  Tabs in auto-grouped
groups are automatically popped when
`tab-group:auto-group-local-mode' is disabled."
  (or (tab-group:group name)
      (let ((group (tab-group:new name)))
        (tab-group:set 'auto t group)
        group)))

(defsubst tab-group:auto-group-tab-p (tab)
  (tab-group:get 'auto (tab-group:tab-group tab)))

(define-minor-mode tab-group:auto-group-local-mode
  "Minor mode of (major) modewise grouping of tabs.
The buffer will automatically tabbed and the tab is put into some
groups unless `tab-group:buffer-auto-p-function' returns nil.
The names of the groups are determined by
`tab-group:buffer-auto-groups-function'."
  :group 'tab-group:auto
  (if tab-group:auto-group-local-mode
      ;; on
      (when (funcall tab-group:buffer-auto-p-function)
        (dolist (group-name (funcall tab-group:buffer-auto-groups-function))
          (let ((group (tab-group:auto-new-group group-name)))
            (unless (tab-group:buffer-tab-in-group group)
              (tab-group:unselect tab-group:current-tab)
              (tab-group:select (tab-group:new-tab nil nil group) t)))))
    ;; off
    (dolist (tab tab-group:buffer-tabs)
      (when (tab-group:auto-group-tab-p tab) (tab-group:pop tab)))))

(define-globalized-minor-mode tab-group:auto-mode
  tab-group:auto-group-local-mode tab-group:auto-group-local-mode
  :group 'tab-group:auto)


;;; commands

(defsubst tab-group:normalize-group-name (name)
  (if (or (not name) (string-match-p "^[\r\n\t ]*$" name))
      (let* ((initial (or (tab-group:current-group) tab-group:default-group))
             (initial (and initial (tab-group:group-name initial))))
        (or initial "default"))
    name))

(defun tab-group:new (&optional group)
  "Create a new tab group named GROUP.
If GROUP is nil, either the group of `tab-group:current-tab' or
`tab-group:default-group' is used.

When called interactively, create a new tab for the group by
`tab-group:new-tab'."
  (interactive (list (completing-read "Group: " tab-group:groups)))
  (setq group (tab-group:normalize-group-name group))
  (let* ((name group) (defined (tab-group:group name))
         (group (or defined (tab-group:make-group :name name :tabs '())))
         (tabbar-sym (tab-group:tabbar-sym name)))
    (unless defined
      (setf (tab-group:group-tabbar group) tabbar-sym)
      (set-default tabbar-sym '(""))
      (put tabbar-sym 'risky-local-variable t)
      (add-to-list 'tab-group:groups (cons name group))
      (tab-group:set 'index 0 group)
      (let* ((mode tab-group:group-buffer-mode)
             (mode (if mode (or (and (integerp mode) mode) 1) -1)))
        (tab-group:set 'group-buffer-mode mode group)))
    (when (and (called-interactively-p 'any)
               (not (eq major-mode 'tab-group:tab-list-mode)))
      (tab-group:unselect tab-group:current-tab)
      (tab-group:select (tab-group:new-tab nil nil group)))
    group))
(setq tab-group:default-group (tab-group:new))

(defun tab-group:remove (&optional group)
  "Remove a group named GROUP.
All tabs in the group are popped."
  (interactive (list (completing-read "Group: " tab-group:groups)))
  (when (tab-group:group-p group) (setq group (tab-group:group-name group)))
  (setq group (tab-group:normalize-group-name group))
  (let* ((name group) (group (tab-group:group name))
         (sym (tab-group:group-tabbar group))
         (tabs (tab-group:group-tabs group))
         (list (tab-group:group-list group)))
    (mapc #'tab-group:pop tabs)
    (when (and list (buffer-live-p (tab-group:tab-buffer list)))
      (with-current-buffer (tab-group:tab-buffer list)
        (remove-hook 'kill-buffer-query-functions
                     'tab-group:group-buffer-prevent-kill-buffer t)
        (kill-buffer)))
    (setq tab-group:groups
          (delq (assoc name tab-group:groups) tab-group:groups))
    (when (boundp sym) (makunbound sym))
    group))

(defun tab-group:switch (group)
  "Switch to GROUP.
GROUP must include a tab which the current buffer belongs to."
  (interactive
   (let ((collection (mapcar 'tab-group:tab-group-name tab-group:buffer-tabs)))
     (list (completing-read "Switch to group: " collection))))
  (let* ((group (tab-group:group group))
         (tab (tab-group:buffer-tab-in-group (tab-group:group group))))
    (when tab (tab-group:select tab))
    group))

(defun tab-group:next-group ()
  "Switch to the group previous to the group of the current tab."
  (interactive)
  (let* ((tab tab-group:current-tab)
         (tabs tab-group:buffer-tabs)
         (pair (tab-group:split tab tabs))
         (next (car-safe (car-safe pair))))
    (tab-group:select (or next (car-safe (last tabs))))))

(defun tab-group:prev-group ()
  "Switch to the group previous to the group of the current tab."
  (interactive)
  (let* ((tab tab-group:current-tab)
         (tabs tab-group:buffer-tabs)
         (pair (tab-group:split tab tabs))
         (prev (car-safe (cdr pair))))
    (tab-group:select (or prev (car-safe tabs)))))

(defun tab-group:select (tab &optional no-switch)
  "Select TAB.
TAB can be in any group.  If NO-SWITCH is nil, the buffer of the
tab is displayed."
  (interactive
   (let ((group (tab-group:current-group)))
     (when group (list (car-safe (tab-group:search group "Select tab: "))))))
  (when (and tab (tab-group:tab-p tab))
    (let* ((old-tab tab-group:current-tab)
           (buffer (tab-group:tab-buffer tab))
           (group (tab-group:tab-group tab))
           (current-group (tab-group:current-group))
           (scroll (and (eq group current-group) tab-group:last-scroll-pos)))
      (tab-group:unselect old-tab)
      (unless no-switch (switch-to-buffer buffer))
      (with-current-buffer buffer
        (when (and old-tab (not (eq (tab-group:tab-buffer old-tab) buffer)))
          (tab-group:unselect tab-group:current-tab))
        (tab-group:set-current-tab tab)
        (setq tab-group:last-scroll-pos scroll)
        (tab-group:force-recalculate-scroll-limit (tab-group:tab-group tab))
        (tab-group:update-tabbar (tab-group:tab-group tab))
        (run-hooks 'tab-group:select-tab-hook))))
  tab)

(defun tab-group:next (&optional tab)
  "Select a tab next to TAB in the tabbar.
If TAB is nil, the `tab-group:current-tab' is used."
  (interactive)
  (unless tab (setq tab tab-group:current-tab))
  (tab-group:with-splitting-tabs tab (group tabs left right)
    (tab-group:select (or (car-safe right) (car tabs)))))

(defun tab-group:prev (&optional tab)
  "Select a tab previous to TAB in the tabbar.
If TAB is nil, the `tab-group:current-tab' is used."
  (interactive)
  (unless tab (setq tab tab-group:current-tab))
  (tab-group:with-splitting-tabs tab (group tabs left right)
    (tab-group:select (or (car-safe left) (car (last tabs))))))

(defun tab-group:pop (&optional tab)
  "Pop TAB from the tabbar.
If TAB is nil, the `tab-group:current-tab' is used."
  (interactive)
  (unless tab (setq tab tab-group:current-tab))
  (when tab
    (tab-group:unselect tab)
    (tab-group:pop-tab tab)
    (with-current-buffer (tab-group:tab-buffer tab)
      (let ((sym (tab-group:tab-tab tab)))
        (when (and (boundp sym) (local-variable-p sym))
          (kill-local-variable sym))
        (when (boundp sym) (makunbound sym)))
      (when (null tab-group:buffer-tabs) (tab-group:local-mode 0)))))

(defun tab-group:rename (tab name)
  "Rename TAB to NAME."
  (interactive
   (let ((tab tab-group:current-tab))
     (list tab
           (read-string "Name: " (and tab (tab-group:tab-name tab))))))
  (when (and tab name)
    (setf (tab-group:tab-name tab) name)
    (tab-group:set-label tab name))
  name)

(defun tab-group:move-tab-command (tab pos &optional group)
  (tab-group:move-tab tab pos t group)
  (let* ((group (tab-group:tab-group tab))
         (tabs (tab-group:group-tabs group)))
    (dolist (tab tabs)
      (with-current-buffer (tab-group:tab-buffer tab)
        (tab-group:force-recalculate-scroll-limit group)
        (tab-group:update-tabbar group)))))

(defun tab-group:move-tab-left (&optional tab)
  "Move tab left in the tabbar.
If TAB is nil, the `tab-group:current-tab' is used."
  (interactive)
  (unless tab (setq tab tab-group:current-tab))
  (when tab
    (let ((pos (tab-group:tab-position tab)))
      (tab-group:move-tab-command tab (1- pos)))))

(defun tab-group:move-tab-right (&optional tab)
  "Move tab right in the tabbar.
If TAB is nil, the `tab-group:current-tab' is used."
  (interactive)
  (unless tab (setq tab tab-group:current-tab))
  (when tab
    (let ((pos (tab-group:tab-position tab)))
      (tab-group:move-tab-command tab (1+ pos)))))

(defun tab-group:move-tab-begin (&optional tab)
  "Move tab to the beginning of the tabbar.
If TAB is nil, the `tab-group:current-tab' is used."
  (interactive)
  (unless tab (setq tab tab-group:current-tab))
  (when tab (tab-group:move-tab-command tab 0)))

(defun tab-group:move-tab-end (&optional tab)
  "Move tab to the end of the tabbar.
If TAB is nil, the `tab-group:current-tab' is used."
  (interactive)
  (unless tab (setq tab tab-group:current-tab))
  (when tab
    (let ((tabs (tab-group:group-tabs (tab-group:tab-group tab))))
      (tab-group:move-tab-command tab (length tabs)))))

(defun tab-group:scroll-left ()
  "Scroll the tabbar left."
  (interactive)
  (let ((group (tab-group:current-group))
        (scroll (max 0 (1- (or tab-group:last-scroll-pos 0)))))
    (tab-group:scroll scroll t)))

(defun tab-group:scroll-right ()
  "Scroll the tabbar right."
  (interactive)
  (let* ((group (tab-group:current-group))
         (last (and group (1- (length (tab-group:group-tabs group)))))
         (scroll (min (or last 0) (1+ (or tab-group:last-scroll-pos 0)))))
    (tab-group:scroll scroll t)))

(defun tab-group:scroll-begin ()
  "Scroll the tabbar to the beginning."
  (interactive)
  (let ((group (tab-group:current-group)))
    (tab-group:scroll 0 t)))

(defun tab-group:scroll-end ()
  "Scroll the tabbar to the end."
  (interactive)
  (let* ((group (tab-group:current-group))
         (last (and group (1- (length (tab-group:group-tabs group))))))
    (tab-group:scroll last t)))


;;; mouse commands

(defun tab-group:event-tab (event &optional end)
  (let* ((target (posn-string (if end (event-end event) (event-start event))))
         (pos (cdr target)))
    (when pos
      (get-text-property pos 'tab-group:tab (car target)))))

(defun tab-group:mouse-select (event)
  "Select tab indicated by EVENT."
  (interactive "@e")
  (tab-group:move-end)
  (let ((tab (tab-group:event-tab event)))
    (when (and tab (tab-group:tab-p tab))
      (tab-group:select tab))))

(defun tab-group:mouse-kill (event)
  "Kill tab indicated by EVENT."
  (interactive "@e")
  (let ((tab (tab-group:event-tab event)))
    (when (and tab (tab-group:tab-p tab))
      (kill-buffer (tab-group:tab-buffer tab)))))

(defun tab-group:mouse-move (event)
  "Move tab indicated by EVENT."
  (interactive "@e")
  (tab-group:move-end)
  (let* ((tab (tab-group:event-tab event))
         (other (tab-group:event-tab event t))
         (window (posn-window (event-end event))))
    (when (and tab (tab-group:tab-p tab) other (tab-group:tab-p other))
      (let ((group (tab-group:tab-group other))
            (pos (tab-group:tab-position other)))
        (with-selected-window window
          (tab-group:move-tab-command tab pos group))))))

(provide 'tab-group)
;;; tab-group.el ends here
