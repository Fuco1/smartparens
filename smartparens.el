;;; smartparens.el --- Automatic insertion, wrapping and paredit-like navigation with user defined pairs.

;; Copyright (C) 2012-2014 Matus Goljer

;; Author: Matus Goljer <matus.goljer@gmail.com>
;; Maintainer: Matus Goljer <matus.goljer@gmail.com>
;; Created: 17 Nov 2012
;; Keywords: abbrev convenience editing
;; URL: https://github.com/Fuco1/smartparens

;; This file is not part of GNU Emacs.

;;; License:

;; This file is part of Smartparens.

;; Smartparens is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; Smartparens is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Smartparens.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Smartparens is minor mode for Emacs that deals with parens pairs
;; and tries to be smart about it.  It started as a unification effort
;; to combine functionality of several existing packages in a single,
;; compatible and extensible way to deal with parentheses, delimiters,
;; tags and the like.  Some of these packages include autopair,
;; textmate, wrap-region, electric-pair-mode, paredit and others.  With
;; the basic features found in other packages it also brings many
;; improvements as well as completely new features.

;; For a basic overview, see github readme at
;; https://github.com/Fuco1/smartparens

;; For the complete documentation visit the documentation wiki located
;; at https://github.com/Fuco1/smartparens/wiki

;; If you like this project, you can donate here:
;; https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=CEYP5YVHDRX8C

;;; Code:

(eval-when-compile (require 'cl)) ; for `lexical-let'
(require 'cl-lib)
(require 'dash)
(require 'thingatpt)

(eval-when-compile (defvar cua--region-keymap))
(declare-function cua-replace-region "cua-base")
(declare-function cua--pre-command-handler "cua-base")
(declare-function delete-selection-pre-hook "delsel")

;;;###autoload
(defun sp-cheat-sheet (&optional arg)
  "Generate a cheat sheet of all the smartparens interactive functions.

Without a prefix argument, print only the short documentation and examples.

With non-nil prefix argument, show the full documentation for each function.

You can follow the links to the function or variable help page.
To get back to the full list, use \\[help-go-back].

You can use `beginning-of-defun' and `end-of-defun' to jump to
the previous/next entry.

Examples are fontified using the `font-lock-string-face' for
better orientation."
  (interactive "P")
  (setq arg (not arg))
  (require 'help-mode) ;; for help-xref-following #85
  (let ((do-not-display '(
                          smartparens-mode
                          smartparens-global-mode
                          turn-on-smartparens-mode
                          turn-off-smartparens-mode
                          sp--cua-replace-region
                          sp-wrap-cancel
                          sp-remove-active-pair-overlay
                          sp--self-insert-command
                          sp-wrap-tag-beginning
                          sp-wrap-tag-end
                          sp-wrap-tag-done
                          sp-splice-sexp-killing-around ;; is aliased to `sp-raise-sexp'
                          show-smartparens-mode
                          show-smartparens-global-mode
                          turn-on-show-smartparens-mode
                          turn-off-show-smartparens-mode
                          ))
        (do-not-display-with-arg '(
                                   sp-use-paredit-bindings
                                   sp-use-smartparens-bindings
                                   ))
        (commands (cl-loop for i in (cdr (assoc-string (file-truename (locate-library "smartparens")) load-history))
                           if (and (consp i) (eq (car i) 'defun) (commandp (cdr i)))
                           collect (cdr i))))
    (with-current-buffer (get-buffer-create "*Smartparens cheat sheet*")
      (let ((standard-output (current-buffer))
            (help-xref-following t))
        (read-only-mode -1)
        (erase-buffer)
        (help-mode)
        (smartparens-mode 1)
        (help-setup-xref (list #'sp-cheat-sheet)
                         (called-interactively-p 'interactive))
        (read-only-mode -1)
        (--each (--remove (or (memq it do-not-display)
                              (and arg (memq it do-not-display-with-arg)))
                          commands)
          (unless (equal (symbol-name it) "advice-compilation")
            (let ((start (point)) kill-from)
              (insert (propertize (symbol-name it) 'face 'font-lock-function-name-face))
              (insert " is ")
              (describe-function-1 it)
              (save-excursion
                (when arg
                  (goto-char start)
                  (forward-paragraph 1)
                  (forward-line 1)
                  (if (looking-at "^It is bound")
                      (forward-paragraph 2)
                    (forward-paragraph 1))
                  (setq kill-from (point))
                  (when (re-search-forward "^Examples:" nil t)
                    (delete-region kill-from
                                   (save-excursion
                                     (forward-line 1)
                                     (point))))))
              (insert (propertize (concat
                                   "\n\n"
                                   (make-string 72 ?―)
                                   "\n\n") 'face 'font-lock-function-name-face)))))
        (goto-char (point-min))
        (while (re-search-forward "\\(->\\|​\\)" nil t)
          (let ((thing (bounds-of-thing-at-point 'line)))
            (put-text-property (car thing) (cdr thing) 'face 'font-lock-string-face)))
        (goto-char (point-min))
        (while (re-search-forward "|" nil t)
          (put-text-property (1- (point)) (point) 'face 'font-lock-warning-face))
        (goto-char (point-min))
        (while (re-search-forward "^It is bound to \\(.*?\\)\\." nil t)
          (put-text-property (match-beginning 1) (match-end 1) 'face 'font-lock-keyword-face))
        (goto-char (point-min))
        (while (re-search-forward ";;.*?$" nil t)
          (put-text-property (match-beginning 0) (match-end 0) 'face 'font-lock-comment-face))
        (help-make-xrefs)
        (goto-char (point-min))))
    (pop-to-buffer "*Smartparens cheat sheet*")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables

;;;###autoload
(defvar sp-keymap (make-sparse-keymap)
  "Keymap used for `smartparens-mode'.")
(defvaralias 'smartparens-mode-map 'sp-keymap)

(defvar sp-paredit-bindings '(
                              ("C-M-f" . sp-forward-sexp) ;; navigation
                              ("C-M-b" . sp-backward-sexp)
                              ("C-M-u" . sp-backward-up-sexp)
                              ("C-M-d" . sp-down-sexp)
                              ("C-M-p" . sp-backward-down-sexp)
                              ("C-M-n" . sp-up-sexp)
                              ("M-s" . sp-splice-sexp) ;; depth-changing commands
                              ("M-<up>" . sp-splice-sexp-killing-backward)
                              ("M-<down>" . sp-splice-sexp-killing-forward)
                              ("M-r" . sp-splice-sexp-killing-around)
                              ("C-)" . sp-forward-slurp-sexp) ;; barf/slurp
                              ("C-<right>" . sp-forward-slurp-sexp)
                              ("C-}" . sp-forward-barf-sexp)
                              ("C-<left>" . sp-forward-barf-sexp)
                              ("C-(" . sp-backward-slurp-sexp)
                              ("C-M-<left>" . sp-backward-slurp-sexp)
                              ("C-{" . sp-backward-barf-sexp)
                              ("C-M-<right>" . sp-backward-barf-sexp)
                              ("M-S" . sp-split-sexp) ;; misc
                              )
  "Alist containing the default paredit bindings to corresponding
smartparens functions.")

(defun sp--populate-keymap (bindings)
  "Populates the `sp-keymap' from the BINDINGS alist."
  (--each bindings
    (define-key sp-keymap (read-kbd-macro (car it)) (cdr it))))

;;;###autoload
(defun sp-use-paredit-bindings ()
  "Initiate `sp-keymap' with paredit-compatible bindings for
corresponding functions provided by smartparens.  See variable
`sp-paredit-bindings'."
  (interactive)
  (sp--populate-keymap sp-paredit-bindings))

(defvar sp-smartparens-bindings '(
                                  ("C-M-f" . sp-forward-sexp)
                                  ("C-M-b" . sp-backward-sexp)
                                  ("C-M-d" . sp-down-sexp)
                                  ("C-M-a" . sp-backward-down-sexp)
                                  ("C-S-d" . sp-beginning-of-sexp)
                                  ("C-S-a" . sp-end-of-sexp)
                                  ("C-M-e" . sp-up-sexp)
                                  ("C-M-u" . sp-backward-up-sexp)
                                  ("C-M-n" . sp-next-sexp)
                                  ("C-M-p" . sp-previous-sexp)
                                  ("C-M-k" . sp-kill-sexp)
                                  ("C-M-w" . sp-copy-sexp)
                                  ("M-<delete>" . sp-unwrap-sexp)
                                  ("M-<backspace>" . sp-backward-unwrap-sexp)
                                  ("C-<right>" . sp-forward-slurp-sexp)
                                  ("C-<left>" . sp-forward-barf-sexp)
                                  ("C-M-<left>" . sp-backward-slurp-sexp)
                                  ("C-M-<right>" . sp-backward-barf-sexp)
                                  ("M-D" . sp-splice-sexp)
                                  ("C-M-<delete>" . sp-splice-sexp-killing-forward)
                                  ("C-M-<backspace>" . sp-splice-sexp-killing-backward)
                                  ("C-S-<backspace>" . sp-splice-sexp-killing-around)
                                  ("C-]" . sp-select-next-thing-exchange)
                                  ("C-M-]" . sp-select-next-thing)
                                  ("M-F" . sp-forward-symbol)
                                  ("M-B" . sp-backward-symbol)
                                  )
  "Alist containing the default smartparens bindings.")

;;;###autoload
(defun sp-use-smartparens-bindings ()
  "Initiate `sp-keymap' with smartparens bindings for navigation functions.
See variable `sp-smartparens-bindings'."
  (interactive)
  (sp--populate-keymap sp-smartparens-bindings))

(defun sp--set-base-key-bindings (&optional symbol value)
  "Set up the default keymap based on `sp-base-key-bindings'.

This function is also used as a setter for this customize value."
  (when symbol (set-default symbol value))
  (cond
   ((eq sp-base-key-bindings 'sp)
    (sp-use-smartparens-bindings))
   ((eq sp-base-key-bindings 'paredit)
    (sp-use-paredit-bindings))))

(defun sp--update-override-key-bindings (&optional symbol value)
  "Override the key bindings with values from `sp-override-key-bindings'.

This function is also used as a setter for this customize value."
  (when symbol (set-default symbol value))
  ;; this also needs to reload the base set, if any is present.
  (sp--set-base-key-bindings)
  (sp--populate-keymap sp-override-key-bindings))

(defcustom sp-base-key-bindings nil
  "A default set of key bindings for commands provided by smartparens.

Paredit binding adds the bindings in `sp-paredit-bindings' to the
corresponding smartparens commands. It does not add bindings to
any other commands, or commands that do not have a paredit
counterpart.

Smartparens binding adds the bindings in
`sp-smartparens-bindings' to most common smartparens commands.
These are somewhat inspired by paredit, but in many cases differ.

Note that neither \"paredit\" nor \"smartparens\" bindings add a
binding for all the provided commands."
  :type '(radio
          (const :tag "Don't use any default set of bindings" nil)
          (const :tag "Use smartparens set of bindings" sp)
          (const :tag "Use paredit set of bindings" paredit))
  :set 'sp--set-base-key-bindings
  :group 'smartparens)

(defcustom sp-override-key-bindings nil
  "An alist of bindings and commands that should override the base key set.

If you wish to override a binding from the base set, set the
value for the binding to the `kbd' recognizable string constant
and command to the command symbol you wish to bind there.

If you wish to disable a binding from the base set, set the value
for the command to nil.

Examples:
 (\"C-M-f\" . sp-forward-sexp)
 (\"C-<right>\" . nil)

See `sp-base-key-bindings'."
  :type '(alist
          :key-type string
          :value-type symbol)
  :set 'sp--update-override-key-bindings
  :group 'smartparens)

(defvar sp-escape-char nil
  "Character used to escape quotes inside strings.")
(make-variable-buffer-local 'sp-escape-char)

(defvar sp-comment-char nil
  "Character used to start comments.")
(make-variable-buffer-local 'sp-comment-char)

(defvar sp-pair-list nil
  "List of pairs for autoinsertion or wrapping.

Maximum length of opening or closing pair is
`sp-max-pair-length-c' characters.")
(make-variable-buffer-local 'sp-pair-list)

(defvar sp-local-pairs nil
  "List of pair definitions used for current buffer.")
(make-variable-buffer-local 'sp-local-pairs)

(defvar sp-last-operation nil
  "Symbol holding the last successful operation.")
(make-variable-buffer-local 'sp-last-operation)

(defvar sp-previous-point -1
  "Location of point before last command.

This is only updated when some pair-overlay is active.  Do not
rely on the value of this variable anywhere else!")
(make-variable-buffer-local 'sp-previous-point)

(defvar sp-wrap-point nil
  "Save the value of point before attemt to wrap a region.

Used for restoring the original state if the wrapping is
cancelled.")
(make-variable-buffer-local 'sp-wrap-point)

(defvar sp-wrap-mark nil
  "Save the value of mark before attemt to wrap a region.

Used for restoring the original state if the wrapping is
cancelled.")
(make-variable-buffer-local 'sp-wrap-mark)

(defvar sp-last-inserted-characters ""
  "Characters typed during the wrapping selection.

If wrapping is cancelled, these characters are re-inserted to the
location of point before the wrapping.")
(make-variable-buffer-local 'sp-last-inserted-characters)

(defvar sp-last-inserted-pair nil
  "Last inserted pair.")
(make-variable-buffer-local 'sp-last-inserted-pair)

(defvar sp-delayed-pair nil
  "A pair whose insertion is delayed to be carried out in
`sp--post-command-hook-handler'. The format is (opening delim
. beg of the opening delim)")
(make-variable-buffer-local 'sp-delayed-pair)

(defvar sp-last-wrapped-region nil
  "Information about the last wrapped region.
The format is the same as returned by `sp-get-sexp'.")
(make-variable-buffer-local 'sp-last-wrapped-region)

(defvar sp-point-inside-string nil
  "Non-nil if point is inside a string.

Used to remember the state from before `self-insert-command' is
run.")

(defvar sp-buffer-modified-p nil
  "Non-nil if buffer was modified before the advice on
`self-insert-command' executed.")

(defconst sp-max-pair-length-c 10
  "Maximum length of an opening or closing delimiter.

Only the pairs defined by `sp-pair' are considered.  Tag pairs
can be of any length.")

(defvar sp-pairs '((t
                    .
                    ((:open "\\\\(" :close "\\\\)" :actions (insert wrap autoskip navigate))
                     (:open "\\{"   :close "\\}"   :actions (insert wrap autoskip navigate))
                     (:open "\\("   :close "\\)"   :actions (insert wrap autoskip navigate))
                     (:open "\\\""  :close "\\\""  :actions (insert wrap autoskip navigate))
                     (:open "\""    :close "\""    :actions (insert wrap autoskip navigate))
                     (:open "'"     :close "'"     :actions (insert wrap autoskip navigate))
                     (:open "("     :close ")"     :actions (insert wrap autoskip navigate))
                     (:open "["     :close "]"     :actions (insert wrap autoskip navigate))
                     (:open "{"     :close "}"     :actions (insert wrap autoskip navigate))
                     (:open "`"     :close "`"     :actions (insert wrap autoskip navigate)))))
  "List of pair definitions.

Maximum length of opening or closing pair is
`sp-max-pair-length-c' characters.")

(defvar sp-tags nil
  "List of tag definitions.  See `sp-local-tag' for more information.")

(defvar sp-prefix-tag-object nil
  "If non-nil, only consider tags while searching for next thing.")

(defvar sp-prefix-pair-object nil
  "If non-nil, only consider pairs while searching for next thing.

Pairs are defined as expressions delimited by pairs from
`sp-pair-list'.")

(defvar sp-prefix-symbol-object nil
  "If non-nil, only consider symbols while searching for next thing.

Symbol is defined as a chunk of text recognized by
`sp-forward-symbol'.")

(defvar sp-recent-keys nil
  "Last 20 typed keys, registered via `self-insert-command'.")

(defvar sp--lisp-modes '(emacs-lisp-mode
                         inferior-emacs-lisp-mode
                         lisp-interaction-mode
                         scheme-mode
                         scheme-interaction-mode
                         inferior-scheme-mode
                         geiser-repl-mode
                         lisp-mode
                         eshell-mode
                         slime-repl-mode
                         cider-repl-mode
                         nrepl-repl-mode
                         clojure-mode
                         common-lisp-mode)
  "List of Lisp modes.")

(defvar sp--html-modes '(
                         sgml-mode
                         html-mode
                         rhtml-mode
                         nxhtml-mode
                         nxml-mode
                         web-mode
                         jinja2-mode
                         html-erb-mode
                         )
  "List of HTML modes.")

(defvar sp-message-alist
  '((:unmatched-expression
     "Search failed. This means there is unmatched expression somewhere or we are at the beginning/end of file."
     "Unmatched expression.")
    (:delimiter-in-string
     "Opening or closing pair is inside a string or comment and matching pair is outside (or vice versa). Ignored.")
    (:no-matching-tag
     "Search failed. No matching tag found."
     "No matching tag.")
    (:invalid-context-prev
     "Invalid context: previous h-sexp ends after the next one."
     "Invalid context.")
    (:invalid-context-cur
     "Invalid context: current h-sexp starts after the next one."
     "Invalid context.")
    (:no-structure-found
     "Previous sexp starts after current h-sexp or no structure was found."
     "No valid structure found.")
    (:invalid-structure
     "This operation would result in invalid structure. Ignored."
     "Ignored because of invalid structure.")
    (:cant-slurp
     "We can't slurp without breaking strictly balanced expression. Ignored."
     "Can't slurp without breaking balance.")
    (:blank-sexp
     "Point is in blank sexp, nothing to barf."
     "Point is in blank sexp.")
    (:point-not-deep-enough
     "Point has to be at least two levels deep to swap the enclosing delimiters."
     "Point has to be at least two levels deep."
     "Point not deep enough.")
    (:different-type
     "The expressions to be joined are of different type."
     "Expressions are of different type."))
  "List of predefined messages to be displayed by `sp-message'.

Each element is a list consisting of a keyword and one or more
strings, which are chosen based on the `sp-message-width'
variable. If the latter is `t', the first string is chosen as
default, which should be the most verbose option available.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customize & Mode definitions

(defgroup smartparens ()
  "Smartparens minor mode."
  :group 'editing
  :prefix "sp-")

;;;###autoload
(define-minor-mode smartparens-mode
  "Toggle smartparens mode.

You can enable pre-set bindings by customizing
`sp-base-key-bindings' variable.  The current content of
`sp-keymap' is:

 \\{sp-keymap}"
  :init-value nil
  :lighter (" SP" (:eval (if smartparens-strict-mode "/s" "")))
  :group 'smartparens
  :keymap sp-keymap
  (if smartparens-mode
      (progn
        (sp--init)
        (when (sp--delete-selection-p)
          (sp--init-delete-selection-mode-emulation))
        (run-hooks 'smartparens-enabled-hook))
    (run-hooks 'smartparens-disabled-hook)))

(defvar smartparens-strict-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap delete-char] 'sp-delete-char)
    (define-key map [remap delete-forward-char] 'sp-delete-char)
    (define-key map [remap backward-delete-char-untabify] 'sp-backward-delete-char)
    (define-key map [remap backward-delete-char] 'sp-backward-delete-char)
    (define-key map [remap delete-backward-char] 'sp-backward-delete-char)
    (define-key map [remap kill-word] 'sp-kill-word)
    (define-key map [remap kill-line] 'sp-kill-hybrid-sexp)
    (define-key map [remap backward-kill-word] 'sp-backward-kill-word)
    map)
  "Keymap used for `smartparens-strict-mode'.")

;;;###autoload
(define-minor-mode smartparens-strict-mode
  "Toggle the strict smartparens mode.

When strict mode is active, `delete-char', `kill-word' and their
backward variants will skip over the pair delimiters in order to
keep the structure always valid (the same way as `paredit-mode'
does).  This is accomplished by remapping them to
`sp-delete-char' and `sp-kill-word'.  There is also function
`sp-kill-symbol' that deletes symbols instead of words, otherwise
working exactly the same (it is not bound to any key by default).

When strict mode is active, this is indicated with \"/s\"
after the smartparens indicator in the mode list."
  :init-value nil
  :group 'smartparens
  (if smartparens-strict-mode
      (progn
        (unless smartparens-mode
          (smartparens-mode 1))
        (unless (-find-indices (lambda (it) (eq (car it) 'smartparens-strict-mode)) minor-mode-overriding-map-alist)
          (setq minor-mode-overriding-map-alist
                (cons `(smartparens-strict-mode . ,smartparens-strict-mode-map) minor-mode-overriding-map-alist)))
        (setq sp-autoskip-closing-pair 'always))
    (setq minor-mode-overriding-map-alist
          (-remove (lambda (it) (eq (car it) 'smartparens-strict-mode)) minor-mode-overriding-map-alist))
    (let ((std-val (car (plist-get (symbol-plist 'sp-autoskip-closing-pair) 'standard-value)))
          (saved-val (car (plist-get (symbol-plist 'sp-autoskip-closing-pair) 'saved-value))))
      (setq sp-autoskip-closing-pair (eval (or saved-val std-val))))))

;;;###autoload
(define-globalized-minor-mode smartparens-global-strict-mode
  smartparens-strict-mode
  turn-on-smartparens-strict-mode
  :group 'smartparens)

;;;###autoload
(defun turn-on-smartparens-strict-mode ()
  "Turn on `smartparens-strict-mode'."
  (interactive)
  (unless (or (member major-mode sp-ignore-modes-list)
              (and (not (derived-mode-p 'comint-mode))
                   (eq (get major-mode 'mode-class) 'special)))
    (smartparens-strict-mode 1)))

(defun sp--init ()
  "Initialize the buffer local pair bindings and other buffer
local variables that depend on the active `major-mode'."
  ;; setup local pair replacements
  (sp--update-local-pairs)
  ;; set the escape char
  (dotimes (char 256)
    (unless sp-escape-char
      (when (= ?\\ (char-syntax char))
        (setq sp-escape-char (string char))))
    (unless sp-comment-char
      (when (= ?< (char-syntax char))
        (setq sp-comment-char (string char))))))

(defun sp--maybe-init ()
  "Initialize the buffer if it is not already initialized. See `sp--init'."
  (unless sp-pair-list
    (sp--init)))

(defvar sp-trigger-keys nil
  "List of trigger keys.")

(defun sp--update-trigger-keys (&optional remove)
  "Update the trigger keys in `sp-keymap'.

Trigger key is any character present in any pair's opening or
closing delimiter.  Each trigger key must map to
`sp--self-insert-command'.

The optional argument REMOVE is a string of trigger keys to
remove.  If non-nil, remove the trigger keys defined by this
string.  After the removal, all the pairs are re-checked."
  (when remove
    (--each (split-string remove "" t)
      (define-key sp-keymap it nil)))

  (setq sp-trigger-keys nil)
  (dolist (mode-pairs sp-pairs)
    (dolist (pair (cdr mode-pairs))
      (let ((open (plist-get pair :open))
            (close (plist-get pair :close)))
        (when open
          (setq sp-trigger-keys (append (split-string open "" t) sp-trigger-keys))
          (--each (split-string open "" t)
            (define-key sp-keymap it 'sp--self-insert-command)))
        (when close
          (setq sp-trigger-keys (append (split-string close "" t) sp-trigger-keys))
          (--each (split-string close "" t)
            (define-key sp-keymap it 'sp--self-insert-command))))))

  (dolist (mode-tags sp-tags)
    (dolist (tag (cdr mode-tags))
      (let ((trig (plist-get tag :trigger)))
        (setq sp-trigger-keys (append (split-string trig "" t) sp-trigger-keys))
        (--each (split-string trig "" t)
          (define-key sp-keymap it 'sp--self-insert-command)))))

  (setq sp-trigger-keys (-distinct sp-trigger-keys)))

(defun sp--keybinding-fallback (&optional key-sequence)
  "Return the fall-back command as if `smartparens-mode' were disabled."
  (let ((smartparens-mode nil)
        (keys (or key-sequence (car sp-recent-keys))))
    ;; HACK: why and when this happens, I can't figure it out!!!
    (if keys (key-binding keys t) 'self-insert-command)))

(defun sp--update-local-pairs ()
  "Update local pairs after removal or at mode initialization."
  (setq sp-local-pairs
        (->> (sp--merge-with-local major-mode)
          (--filter (plist-get it :actions))))
  ;; update the `sp-pair-list'.  This is a list only containing
  ;; (open.close) cons pairs for easier querying.  We also must order
  ;; it by length of opening delimiter in descending order (first
  ;; value is the longest)
  (setq sp-pair-list
        (->> sp-local-pairs
          (--map (cons (plist-get it :open) (plist-get it :close)))
          (-sort (lambda (x y) (> (length (car x)) (length (car y))))))))

(defun sp--update-local-pairs-everywhere (&rest modes)
  "Run `sp--update-local-pairs' in all buffers.

This is necessary to update all the buffer-local definitions.  If
MODES is non-nil, only update buffers with `major-mode' equal to
MODES."
  (setq modes (-flatten modes))
  (--each (buffer-list)
    (with-current-buffer it
      (when (and smartparens-mode
                 (or (not modes)
                     (memq major-mode modes)))
        (sp--update-local-pairs)))))

(defcustom smartparens-enabled-hook nil
  "Called after `smartparens-mode' is turned on."
  :type 'hook
  :group 'smartparens)

(defcustom smartparens-disabled-hook nil
  "Called after `smartparens-mode' is turned off."
  :type 'hook
  :group 'smartparens)

;; global custom
(defcustom sp-ignore-modes-list '(
                                  minibuffer-inactive-mode
                                  )
  "Modes where smartparens mode is inactive if allowed globally."
  :type '(repeat symbol)
  :group 'smartparens)

;;;###autoload
(define-globalized-minor-mode smartparens-global-mode
  smartparens-mode
  turn-on-smartparens-mode)

;;;###autoload
(defun turn-on-smartparens-mode ()
  "Turn on `smartparens-mode'."
  (interactive)
  (unless (or (member major-mode sp-ignore-modes-list)
              (and (not (derived-mode-p 'comint-mode))
                   (eq (get major-mode 'mode-class) 'special)))
    (smartparens-mode t)))

;;;###autoload
(defun turn-off-smartparens-mode ()
  "Turn off `smartparens-mode'."
  (interactive)
  (smartparens-mode -1))

;; insert custom
(defcustom sp-autoinsert-pair t
  "If non-nil, autoinsert pairs.  See `sp-insert-pair'."
  :type 'boolean
  :group 'smartparens)

(defcustom sp-autoinsert-if-followed-by-same 3
  "Customizes behaviour of pair insertion if the point is followed by
the same opening pair as currently inserted pair.

The first option does not change the insertion behaviour and pairs are
inserted normally.  For example |() followed by ( would produce (|)().

The second option inserts the pair only if the opening pair
following point is not the same as currently inserted pair.  For
example |() followed by ( would produce (|().  If next character
isn't part of any pair, insert normally.

The third option behaves as second, but if the opening and closing
pairs are the same, and we are looking at the closing pair, insert the
whole pair.  For example \"|\" followed by \" produce \"\"|\"\".  This
is useful in modes where pairs of same characters have special
meaning, such as `markdown-mode' and * for italics and ** for bold.

The forth option is a combination of first and third.  The pairs
where opening and closing pair are different are always inserted
normally.  The pairs with same opening and closing delimiter are
only inserted if the enclosing expression is empty (for nested
quotations etc.), otherwise the closing delimiter is skipped
instead."
  :type '(radio
          (const :tag "Insert the pair normally" 0)
          (const :tag "Insert the pair only if not followed by same" 1)
          (const :tag "Insert the pair only if not followed by same, but if the closing pair is the same as opening, insert new pair (useful for nested quote insertion)" 2)
          (const :tag "Insert the pair if opening and closing pair is the same and the containing expression is empty and always insert other pairs normally." 3))
  :group 'smartparens)

(defcustom sp-autoinsert-quote-if-followed-by-closing-pair nil
  "If non-nil, autoinsert string quote pair even if the point is followed by closing pair.

This option only changes behaviour of the insertion process if
point is inside a string.  In other words, if string is not
closed and next character is a closing pair.

For example, in a situation like this:

  [\"some text|]

after pressing \", one would probably want to insert the closing
quote, not a nested pair (\\\"\\\"), to close the string literal
in the array.  To enable such behaviour, set this variable to
nil.

Note: the values of this varible seem to be backward, i.e. it is
\"enabled\" when the value is nil.  This was an unfortunate
choice of wording.  It is kept this way to preserve backward
compatibility.  The intended meaning is \"insert the pair if
followed by closing pair?\", t = yes."
  :type 'boolean
  :group 'smartparens)

(defcustom sp-autoskip-closing-pair 'always-end
  "If t, skip the following closing pair if the expression is
active (that is right after insertion).  This is controlled by
`sp-cancel-autoskip-on-backward-movement'.

If set to \"always-end\", skip the closing pair even if the
expression is not active and point is at the end of the
expression.  This only works for expressions with
single-character delimiters.  If the expression is a string-like
expression, these must be enabled in current major-mode to work
with this setting, see `sp-navigate-consider-stringlike-sexp'.

If set to \"always\", `sp-up-sexp' is called whenever the closing
delimiter is typed inside a sexp of the same type.  This is the
paredit-like behaviour.  This setting only works for
single-character delimiters and does not work for string-like
delimiters.

See `sp-autoskip-opening-pair' for similar setting for
string-like delimiters.

See also `sp-skip-closing-pair'."
  :type '(radio
          (const :tag "Never skip closing delimiter" nil)
          (const :tag "Skip closing delimiter in active expressions" t)
          (const :tag "Always skip closing delimiter if at the end of sexp" always-end)
          (const :tag "Always skip closing delimiter" always))
  :group 'smartparens)
(make-variable-buffer-local 'sp-autoskip-closing-pair)

(defcustom sp-autoskip-opening-pair nil
  "If non-nil, skip into the following string-like expression
instead of inserting a new pair."
  :type 'boolean
  :group 'smartparens)
(make-variable-buffer-local 'sp-autoskip-opening-pair)

(defcustom sp-cancel-autoskip-on-backward-movement t
  "If non-nil, autoskip of closing pair is cancelled not only
when point is moved outside of the pair, but also if the point
moved backwards.  See `sp-skip-closing-pair' for more info."
  :type 'boolean
  :group 'smartparens)

;; delete custom
(defcustom sp-autodelete-pair t
  "If non-nil, auto delete pairs.  See `sp-delete-pair'."
  :type 'boolean
  :group 'smartparens)

(defcustom sp-autodelete-closing-pair t
  "If non-nil, auto delete the whole closing-pair.  See `sp-delete-pair'."
  :type 'boolean
  :group 'smartparens)

(defcustom sp-autodelete-opening-pair t
  "If non-nil, auto delete the whole opening-pair.  See `sp-delete-pair'."
  :type 'boolean
  :group 'smartparens)

(defcustom sp-undo-pairs-separately nil
  "If non-nil, put an `undo-boundary' before each inserted pair.

Calling undo after smartparens complete a pair will remove only
the pair before undoing any previous insertion.

WARNING: This option is implemented by hacking the
`buffer-undo-list'.  Turning this option on might have
irreversible consequences on the buffer's undo information and in
some cases might remove important information.  Usage of package
`undo-tree' is recommended if you ever need to revert to a state
unreachable by undo."
  :type 'boolean
  :group 'smartparens)

(defcustom sp-successive-kill-preserve-whitespace 1
  "Control the behaviour of `sp-kill-sexp' on successive kills.

In the description, we consider more than one space
\"superfluous\", however, newlines are preserved."
  :type '(radio
          (const :tag "Always preserve the whitespace" 0)
          (const :tag "Remove superfluous whitespace after last kill" 1)
          (const :tag "Remove superfluous whitespace after all kills" 2))
  :group 'smartparens)

;; wrap custom
(defcustom sp-autowrap-region t
  "If non-nil, wrap the active region with pair.

See `sp-wrap-region' and `sp-wrap-region-init'."
  :type 'boolean
  :group 'smartparens)

(defcustom sp-autodelete-wrap t
  "If non-nil, auto delete both opening and closing pair of most recent wrapping.

Deletion command must be the very first command after the
insertion, otherwise normal behaviour is applied."
  :type 'boolean
  :group 'smartparens)

(defcustom sp-wrap-repeat-last 1
  "Context in which smartparens repeats the last wrap.

If the last operation was a wrap and we insert another pair at
the beginning or end of the last wrapped region, repeat the
wrap on this region with current pair."
  :type '(radio
          (const :tag "Do not repeat wrapping" 0)
          (const :tag "Only repeat if current tag is the same as the last one" 1)
          (const :tag "Always repeat if the point is after the opening/closing delimiter of last wrapped region" 2))
  :group 'smartparens)

(defcustom sp-wrap-entire-symbol nil
  "If non-nil, do NOT wrap the entire symbol, only the part after point.

If set to \"Enable globally\", smart symbol wrapping is active
everywhere.  This is the default option.

If set to \"Disable globally\", smart symbol wrapping is disabled
everywhere.

Otherwise, a list of major modes where smart symbol wrapping is
*disabled* can be supplied.

Examples:

 foo-ba|r-baz -> (|foo-bar-baz) ;; if enabled

 foo-ba|r-baz -> foo-ba(|r-baz) ;; if disabled"
  :type '(choice
          (const :tag "Enable globally" nil)
          (const :tag "Disable globally" globally)
          (repeat :tag "Disable in these major modes" symbol))
  :group 'smartparens)

(defcustom sp-wrap-from-point nil
  "If non-nil, do not wrap from the beginning of next expression but from point.

However, if the point is inside a symbol/word, the entire
symbol/word is wrapped.  To customize this behaviour, see
variable `sp-wrap-entire-symbol'."
  :type 'boolean
  :group 'smartparens)

;; escaping custom
(defcustom sp-autoescape-string-quote t
  "If non-nil, autoescape string quotes if typed inside string."
  :type 'boolean
  :group 'smartparens)

(defcustom sp-autoescape-string-quote-if-empty '(
                                                 python-mode
                                                 )
  "List of modes where the string quotes aren't escaped if the string is empty.

You can list modes where multiple quote characters are used for
multi-line strings, such as `python-mode' to make the insertion
less annoying (that is, three times pressing \" would insert
\"\"\"|\"\"\" instead of \"\\\"\\\"|\\\"\\\"\")."
  :type '(repeat symbol)
  :group 'smartparens)

;; navigation & manip custom
(defcustom sp-navigate-consider-sgml-tags '(
                                            html-mode
                                            )
  "List of modes where sgml tags are considered to be sexps."
  :type '(repeat symbol)
  :group 'smartparens)

(defcustom sp-navigate-consider-stringlike-sexp '(
                                                  latex-mode
                                                  )
  "List of modes where string-like sexps are considered to be sexps.

A string-like sexp is an expression where opening and closing
delimeter is the same sequence of characters. For example: *...*,
$...$.

Warning: these are problematic in modes where the symbol might
have multiple functions, such as * in markdown, where it denotes
start of list item (unary) OR emphatic text (binary)."
  :type '(repeat symbol)
  :group 'smartparens)

(defcustom sp-navigate-consider-symbols t
  "If non-nil, consider symbols outside balanced expressions as such.

Symbols are recognized by function `sp-forward-symbol'.  This
setting affect all the navigation and manipulation functions
where it make sense.

Also, special handling of strings is enabled, where the whole
string delimited with \"\" is considered as one token.

WARNING: This is a legacy setting and changing its value to NIL
may break many things.  It is kept only for backward
compatibility and will be removed in the next major release."
  :type 'boolean
  :group 'smartparens)

(defcustom sp-navigate-comments-as-sexps t
  "If non-nil, consider comments as sexps in `sp-get-enclosing-sexp'.

If this option is enabled, unbalanced expressions in comments are
never automatically closed (see `sp-navigate-close-if-unbalanced')."
  :type 'boolean
  :group 'smartparens)

(defcustom sp-navigate-skip-match `(
                                    (,sp--lisp-modes . sp--elisp-skip-match)
                                    )
  "Alist of list of major-modes and a function used to skip over matches in
`sp-get-paired-expression'.  This function takes three arguments:
the currently matched delimiter, beginning of match and end of
match.  If this function returns true, the current match will be
skipped.

You can use this to skip over expressions that serve multiple
functions, such as if/end pair or unary if in Ruby or * in
markdown when it signifies list item instead of emphasis.  If the
exception is only relevant to one pair, you should rather
use :skip-match option in `sp-local-pair'."
  :type '(alist
          :key-type (repeat symbol)
          :value-type symbol)
  :group 'smartparens)

(defcustom sp-navigate-reindent-after-up `(
                                           (interactive
                                            ,@sp--lisp-modes
                                            )
                                           )
  "Modes where sexps should be reindented after jumping out of them with `sp-up-sexp'.

The whitespace between the closing delimiter and last \"thing\"
inside the expression is removed.  It works analogically for the
`sp-backward-up-sexp'.

If the mode is in the list \"interactive\", only reindent the sexp
if the command was called interactively.  This is recommended for
general use.

If the mode is in the list \"always\", reindend the sexp even if the
command was called programatically."
  :type '(alist
          :options (interactive always)
          :value-type (repeat symbol))
  :group 'smartparens)

(defcustom sp-navigate-close-if-unbalanced nil
  "If non-nil, insert the closing pair of the un-matched pair on `sp-up-sexp'.

The closing delimiter is inserted after the symbol at
point (using `sp-previous-sexp')."
  :type 'boolean
  :group 'smartparens)

(defcustom sp-sexp-prefix nil
  "Alist of major-mode specific prefix specification.

Each item is a list with three properties:
- major mode
- a constant symbol 'regexp or 'syntax
- a regexp or a string containing syntax class codes.

If the second argument is 'regexp, the third argument is
interpreted as a regexp to search backward from the start of an
expression.

If the second argument is 'syntax, the third argument is
interpreted as string containing syntax codes that will be
skipped.

You can also override this property locally for a specific pair
by specifying its :prefix property."
  :type '(repeat
          (list symbol
                (choice
                 (const :tag "Regexp" regexp)
                 (const :tag "Syntax class codes" syntax))
                string))
  :group 'smartparens)

(defcustom sp-sexp-suffix nil
  "Alist of major-mode specific suffix specification.

Each item is a list with three properties:
- major mode
- a constant symbol 'regexp or 'syntax
- a regexp or a string containing syntax class codes.

If the second argument is 'regexp, the third argument is
interpreted as a regexp to search forward from the end of an
expression.

If the second argument is 'syntax, the third argument is
interpreted as string containing syntax codes that will be
skipped.

You can also override this property locally for a specific pair
by specifying its :suffix property."
  :type '(repeat
          (list symbol
                (choice
                 (const :tag "Regexp" regexp)
                 (const :tag "Syntax class codes" syntax))
                string))
  :group 'smartparens)

;; hybrid lines
(defcustom sp-hybrid-kill-excessive-whitespace nil
  "If non-nil, `sp-kill-hybrid-sexp' will kill all whitespace up
until next hybrid sexp if the point is at the end of line or on a
blank line."
  :type 'boolean
  :group 'smartparens)

(defcustom sp-hybrid-kill-entire-symbol nil
  "Governs how symbols under point are treated by `sp-kill-hybrid-sexp'.

If t, always kill the symbol under point.

If nil, never kill the entire symbol and only kill the part after point.

If a function, this should be a zero-arg predicate. When it
returns non-nil value, we should kill from point."
  :type '(radio
          (const :tag "Always kill entire symbol" t)
          (const :tag "Always kill from point" nil)
          (const :tag "Kill from point only inside strings" sp-point-in-string)
          (function :tag "Custom predicate"))
  :group 'smartparens)

(defcustom sp-comment-string nil
  "String that is inserted after calling `sp-comment'.

It is an alist of list of major modes to a string.

The value of `comment-start' is used if the major mode is not found."
  :type '(alist
          :key-type (repeat symbol)
          :value-type string)
  :group 'smartparens)

;; ui custom
(defcustom sp-highlight-pair-overlay t
  "If non-nil, autoinserted pairs are highlighted while point is inside the pair."
  :type 'boolean
  :group 'smartparens)

(defcustom sp-highlight-wrap-overlay t
  "If non-nil, wrap overlays are highlighted during editing of the wrapping pair."
  :type 'boolean
  :group 'smartparens)

(defcustom sp-highlight-wrap-tag-overlay t
  "If non-nil, wrap tag overlays are highlighted during editing of the wrapping tag pair."
  :type 'boolean
  :group 'smartparens)

(defcustom sp-message-width 'frame
  "Length of information and error messages to display. If set to
'frame (the default), messages are chosen based of the frame
width. `t' means chose the default (verbose) message, `nil' means
mute. Integers specify the maximum width."
  :type '(choice (const :tag "Fit to frame" frame)
                 (const :tag "Verbose" t)
                 (const :tag "Mute" nil)
                 (integer :tag "Max width"))
  :group 'smartparens)

(defcustom sp-use-subword nil
  "If non-nill, `sp-kill-word' and `sp-backward-kill-word' only
  kill \"subwords\" when `subword-mode' is active."
  :type 'boolean
  :group 'smartparens)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Selection mode emulation

(defun sp--delete-selection-p ()
  "Return t if `delete-selection-mode' or `cua-delete-selection' is enabled."
  (or (and (boundp 'delete-selection-mode) delete-selection-mode)
      (and (boundp 'cua-delete-selection) cua-delete-selection cua-mode)))

(defun sp--cua-replace-region (&optional arg)
  "If `smartparens-mode' is on, emulate `self-insert-command',
else call `cua-replace-region'"
  (interactive "p")
  (setq this-original-command 'self-insert-command)
  (if smartparens-mode
      (self-insert-command (or arg 1))
    (cua-replace-region)))

(defun sp--init-delete-selection-mode-emulation ()
  "Initialize smartparens delete selection emulation.  The
original hooks are removed and handled by sp's pre-command
handler."
  ;; make sure the `delete-selection-pre-hook' is not active and that
  ;; delsel is actually loaded.  We need the delete-selection-pre-hook
  ;; command!
  (when delete-selection-mode
    (remove-hook 'pre-command-hook 'delete-selection-pre-hook))
  ;; if cua-mode is active, replace the `self-insert-command' binding
  ;; and the cua--pre-command-handler hook.
  (when cua-mode
    (define-key cua--region-keymap [remap self-insert-command] 'sp--cua-replace-region)
    (remove-hook 'pre-command-hook 'cua--pre-command-handler)))

(defadvice cua-mode (after cua-mode-fix-selection activate)
  (when (and cua-mode)
    (define-key cua--region-keymap [remap self-insert-command] 'sp--cua-replace-region)
    (define-key cua--region-keymap [remap sp-backward-delete-char] 'cua-delete-region)
    (define-key cua--region-keymap [remap sp-delete-char] 'cua-delete-region)
    (remove-hook 'pre-command-hook 'cua--pre-command-handler)))

(defadvice delete-selection-mode (after delete-selection-mode-fix-selection activate)
  (when (and delete-selection-mode)
    (remove-hook 'pre-command-hook 'delete-selection-pre-hook)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc/Utility functions

(defmacro sp-with-modes (arg &rest forms)
  "Add ARG as first argument to each form in FORMS.

This can be used with `sp-local-pair' calls to automatically
insert the modes."
  (declare (indent 1)
           (debug (form body)))
  `(progn
     ,@(mapcar (lambda (form) (append (list (car form) arg) (cdr form))) forms)))

(font-lock-add-keywords 'emacs-lisp-mode `((,(concat "("
                                                     (regexp-opt '("sp-with-modes"
                                                                   "sp-get"
                                                                   "sp-compare-sexps") t)
                                                     "\\>")
                                            (1 font-lock-keyword-face))))

(defun sp--evil-normal-state-p ()
  "Checks to see if the current `evil-state' is in normal mode."
  (and (fboundp 'evil-normal-state-p) (evil-normal-state-p)))

(defun sp--evil-visual-state-p ()
  "Checks to see if the current `evil-state' is in visual mode."
  (and (fboundp 'evil-visual-state-p) (evil-visual-state-p)))

(defun sp--reverse-string (str)
  "Reverse the string STR."
  (concat (reverse (append str nil))))

(defun sp-point-in-blank-line (&optional p)
  "Return non-nil if line at point is blank (whitespace only).

If optional argument P is present test this instead of point."
  (save-excursion
    (when p (goto-char p))
    (beginning-of-line)
    (looking-at "[ \t]*$")))

(defun sp-point-in-blank-sexp (&optional p)
  "Return non-nil if point is inside blank (whitespace only) sexp.

If optional argument P is present test this instead of point.

Warning: it is only safe to call this when point is inside a
sexp, otherwise the call may be very slow."
  (save-excursion
    (when p (goto-char p))
    (-when-let (enc (sp-get-enclosing-sexp))
      (sp-get enc (string-match-p
                   "\\`[ \t\n]*\\'"
                   (buffer-substring-no-properties :beg-in :end-in))))))

(defun sp-point-in-string (&optional p)
  "Return non-nil if point is inside string or documentation string.

If optional argument P is present test this instead of point."
  (ignore-errors
    (save-excursion
      (nth 3 (syntax-ppss p)))))

(defun sp-point-in-comment (&optional p)
  "Return non-nil if point is inside comment.

If optional argument P is present test this instead off point."
  (setq p (or p (point)))
  (ignore-errors
    (save-excursion
      (or (nth 4 (syntax-ppss p))
          ;; this also test opening and closing comment delimiters... we
          ;; need to chack that it is not newline, which is in "comment
          ;; ender" class in elisp-mode, but we just want it to be
          ;; treated as whitespace
          (and (< p (point-max))
               (memq (char-syntax (char-after p)) '(?< ?>))
               (not (eq (char-after p) ?\n)))))))

(defun sp-point-in-string-or-comment (&optional p)
  "Return non-nil if point is inside string, documentation string or a comment.

If optional argument P is present, test this instead of point."
  (or (sp-point-in-string p)
      (sp-point-in-comment p)))

(defun sp-point-in-symbol (&optional p)
  "Return non-nil if point is inside symbol.

Point is inside symbol if characters on both sides of the point
are in either word or symbol class."
  (setq p (or p (point)))
  (save-excursion
    (goto-char p)
    (and (memq (char-syntax (following-char)) '(?w ?_))
         (memq (char-syntax (preceding-char)) '(?w ?_)))))

(defun sp--single-key-description (event)
  "Return a description of the last event.  Replace all the function
key symbols with garbage character (ň).

TODO: fix this!"
  (let ((original (single-key-description event)))
    (cond
     ((string-match-p "<.*?>" original) "ň")
     ((string-match-p "SPC" original) " ")
     (t original))))

(defun sp--split-string (string by)
  "Split STRING on BY.  This simply calls `split-string' and if it
returns a list of length one, empty string is inserted to the
beginning."
  (let ((sp (split-string string by)))
    (if (not (cdr sp)) (cons "" sp) sp)))

;; see https://github.com/Fuco1/smartparens/issues/125#issuecomment-20356176
(defun sp--current-indentation ()
  "Get the indentation offset of the current line."
  (save-excursion
    (back-to-indentation)
    (current-column)))

(defun sp--calculate-indentation-offset (old-column old-indentation)
  "Calculate correct indentation after re-indent."
  (let ((indentation (sp--current-indentation)))
    (cond
     ;; Point was in code, so move it along with the re-indented code
     ((>= old-column old-indentation)
      (+ old-column (- indentation old-indentation)))
     ;; Point was indentation, but would be in code now, so move to
     ;; the beginning of indentation
     ((<= indentation old-column) indentation)
     ;; Point was in indentation, and still is, so leave it there
     (:else old-column))))

(defun sp--back-to-indentation (old-column old-indentation)
  (let ((offset (sp--calculate-indentation-offset old-column old-indentation)))
    (move-to-column offset)))

(defmacro sp--keep-indentation (&rest body)
  "Execute BODY and restore the indentation."
  (declare (indent 0)
           (debug (body)))
  (let ((c (make-symbol "c"))
        (i (make-symbol "i")))
    `(let ((,c (current-column))
           (,i (sp--current-indentation)))
       ,@body
       (sp--back-to-indentation ,c ,i))))

(defun sp--this-command-self-insert-p ()
  "Return non-nil if `this-command' is some sort of `self-insert-command'."
  (memq this-command '(self-insert-command
                       org-self-insert-command
                       sp--self-insert-command)))

(defun sp--this-original-command-self-insert-p ()
  "Return non-nil if `this-original-command' is some sort of `self-insert-command'."
  (memq this-original-command '(self-insert-command
                                org-self-insert-command
                                sp--self-insert-command)))

(defun sp--signum (x)
  "Return 1 if X is positive, -1 if negative, 0 if zero."
  (cond ((> x 0) 1) ((< x 0) -1) (t 0)))

(cl-eval-when (compile eval load)
  (defun sp--get-substitute (struct list)
    "Only ever call this from sp-get!  This function do the
replacement of all the keywords with actual calls to sp-get."
    (if (listp list)
        (if (eq (car list) 'sp-get)
            list
          (mapcar (lambda (x) (sp--get-substitute struct x))
                  (let ((command (car list)))
                    (cond
                     ((eq command 'sp-do-move-op)
                      (let ((argument (make-symbol "--sp-argument--")))
                        `(let ((,argument ,(cadr list)))
                           (if (< ,argument :beg-prf)
                               (progn
                                 (goto-char :beg-prf)
                                 (delete-char (+ :op-l :prefix-l))
                                 (goto-char ,argument)
                                 (insert :prefix :op))
                             (goto-char ,argument)
                             (insert :prefix :op)
                             (goto-char :beg-prf)
                             (delete-char (+ :op-l :prefix-l))))))
                     ((eq command 'sp-do-move-cl)
                      (let ((argument (make-symbol "--sp-argument--")))
                        `(let ((,argument ,(cadr list)))
                           (if (> ,argument :end-in)
                               (progn
                                 (goto-char ,argument)
                                 (insert :cl :suffix)
                                 (goto-char :end-in)
                                 (delete-char (+ :cl-l :suffix-l)))
                             (goto-char :end-in)
                             (delete-char (+ :cl-l :suffix-l))
                             (goto-char ,argument)
                             (insert :cl :suffix)))))
                     ((eq command 'sp-do-del-op)
                      `(progn
                         (goto-char :beg-prf)
                         (delete-char (+ :op-l :prefix-l))))
                     ((eq command 'sp-do-del-cl)
                      `(progn
                         (goto-char :end-in)
                         (delete-char (+ :cl-l :suffix-l))))
                     ((eq command 'sp-do-put-op)
                      `(progn
                         (goto-char ,(cadr list))
                         (insert :prefix :op)))
                     ((eq command 'sp-do-put-cl)
                      `(progn
                         (goto-char ,(cadr list))
                         (insert :cl :suffix)))
                     (t list)))))
      (if (keywordp list)
          (sp--get-replace-keyword struct list)
        list)))

  (defun sp--get-replace-keyword (struct keyword)
    (cl-case keyword
      ;; point in buffer before the opening delimiter
      (:beg         `(plist-get ,struct :beg))
      ;; point in the buffer after the closing delimiter
      (:end         `(plist-get ,struct :end))
      ;; point in buffer after the opening delimiter
      (:beg-in      `(+ (plist-get ,struct :beg) (length (plist-get ,struct :op))))
      ;; point in buffer before the closing delimiter
      (:end-in      `(- (plist-get ,struct :end) (length (plist-get ,struct :cl))))
      ;; point in buffer before the prefix of this expression
      (:beg-prf     `(- (plist-get ,struct :beg) (length (plist-get ,struct :prefix))))
      ;; point in the buffer after the suffix of this expression
      (:end-suf     `(+ (plist-get ,struct :end) (length (plist-get ,struct :suffix))))
      ;; opening delimiter
      (:op          `(plist-get ,struct :op))
      ;; closing delimiter
      (:cl          `(plist-get ,struct :cl))
      ;; length of the opening pair
      (:op-l        `(length (plist-get ,struct :op)))
      ;; length of the closing pair
      (:cl-l        `(length (plist-get ,struct :cl)))
      ;; length of the entire expression, including enclosing
      ;; delimiters and the prefix and suffix
      (:len         `(- (plist-get ,struct :end)
                        (plist-get ,struct :beg)
                        (- (length (plist-get ,struct :prefix)))
                        (- (length (plist-get ,struct :suffix)))))
      ;; length of the the pair ignoring the prefix, including delimiters
      (:len-out     `(- (plist-get ,struct :end) (plist-get ,struct :beg)))
      ;; length of the pair inside the delimiters
      (:len-in      `(- (plist-get ,struct :end)
                        (plist-get ,struct :beg)
                        (length (plist-get ,struct :op))
                        (length (plist-get ,struct :cl))))
      ;; expression prefix
      (:prefix      `(plist-get ,struct :prefix))
      ;; expression prefix length
      (:prefix-l    `(length (plist-get ,struct :prefix)))
      (:suffix      `(plist-get ,struct :suffix))
      (:suffix-l    `(length (plist-get ,struct :suffix)))
      ;; combined op/cl and suffix/prefix
      (:opp         `(concat (plist-get ,struct :prefix)
                             (plist-get ,struct :op)))
      (:opp-l       `(+ (length (plist-get ,struct :prefix))
                        (length (plist-get ,struct :op))))
      (:cls         `(concat (plist-get ,struct :cl)
                             (plist-get ,struct :suffix)))
      (:cls-l       `(+ (length (plist-get ,struct :cl))
                        (length (plist-get ,struct :suffix))))
      (t keyword))))

;; The structure returned by sp-get-sexp is a plist with following properties:
;;
;; :beg    - point in the buffer before the opening delimiter (ignoring prefix)
;; :end    - point in the buffer after the closing delimiter
;; :op     - opening delimiter
;; :cl     - closing delimiter
;; :prefix - expression prefix
;;
;; This structure should never be accessed directly and should only be
;; exposed by the sp-get macro.  This way, we can later change the
;; internal representation without much trouble.

(defmacro sp-get (struct &rest forms)
  "Get a property from a structure.

STRUCT is a plist with the format as returned by `sp-get-sexp'.
Which means this macro also works with `sp-get-symbol',
`sp-get-string' and `sp-get-thing'.

FORMS is an attribute we want to query.  Currently supported
attributes are:

:beg       - point in buffer before the opening delimiter
:end       - point in the buffer after the closing delimiter
:beg-in    - point in buffer after the opening delimiter
:end-in    - point in buffer before the closing delimiter
:beg-prf   - point in buffer before the prefix of this expression
:end-suf   - point in buffer after the suffix of this expression
:op        - opening delimiter
:cl        - closing delimiter
:op-l      - length of the opening pair
:cl-l      - length of the closing pair
:len       - length of the entire expression, including enclosing
             delimiters, the prefix and the suffix
:len-out   - length of the the pair ignoring the prefix and suffix,
             including delimiters
:len-in    - length of the pair inside the delimiters
:prefix    - expression prefix
:prefix-l  - expression prefix length
:suffix    - expression suffix
:suffix-l  - expression suffix length

These special \"functions\" are expanded to do the selected
action in the context of currently queried pair:

Nullary:
\(sp-do-del-op) - remove prefix and opening delimiter
\(sp-do-del-cl) - remove closing delimiter and suffix

Unary:
\(sp-do-move-op p) - move prefix and opening delimiter to point p
\(sp-do-move-cl p) - move closing delimiter and suffix to point p
\(sp-do-put-op p) - put prefix and opening delimiter at point p
\(sp-do-put-cl p) - put closing delimiter and suffix at point p

In addition to these simple queries and commands, this macro
understands arbitrary forms where any of the aforementioned
attributes are used.  Therefore, you can for example query for
\"(+ :op-l :cl-l)\".  This query would return the sum of lengths
of opening and closing delimiter.  A query
\"(concat :prefix :op)\" would return the string containing
expression prefix and the opening delimiter.

Special care is taken to only evaluate the STRUCT argument once."
  (declare (indent 1)
           (debug (form body)))
  (let ((st (make-symbol "struct")))
    (sp--get-substitute st `(let ((,st ,struct)) ,@forms))))

(defmacro sp-compare-sexps (a b &optional fun what-a what-b)
  "Return non-nil if the expressions A and B are equal.

Two expressions are equal if their :beg property is the same.

If optional argument WHAT is non-nil, use it as a keyword on
which to do the comparsion."
  (declare (debug (form form &optional functionp keywordp keywordp)))
  (setq fun (or fun 'equal))
  (setq what-a (or what-a :beg))
  (setq what-b (or what-b what-a))
  `(,fun (sp-get ,a ,what-a) (sp-get ,b ,what-b)))

(defun sp-message (key)
  "Display a message. The argument is either a string or list of
strings, or a keyword, in which case the string list is looked up
in `sp-message-alist'. The string to be displayed is chosen based
on the `sp-message-width' variable."
  (let ((msgs (cond ((listp key) key)
                    ((stringp key) (list key))
                    (t (cdr (assq key sp-message-alist))))))
    (when (and msgs sp-message-width)
      (if (eq sp-message-width t)
          (message (car msgs))
        (let ((maxlen (if (eq sp-message-width 'frame)
                          (frame-width)
                        sp-message-width))
              (s nil))
          (dolist (msg msgs)
            (if (and (<= (length msg) maxlen)
                     (> (length msg) (length s)))
                (setf s msg)))
          (when s
            (message s)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Adding/removing of pairs/bans/allows etc.

(defun sp--merge-prop (old-pair new-pair prop)
  "Merge a property PROP from NEW-PAIR into OLD-PAIR.
The list OLD-PAIR must not be nil."
  (let ((new-val (plist-get new-pair prop)))
    (cl-case prop
      (:close (plist-put old-pair :close new-val))
      (:prefix (plist-put old-pair :prefix new-val))
      (:suffix (plist-put old-pair :suffix new-val))
      (:skip-match (plist-put old-pair :skip-match new-val))
      (:trigger (plist-put old-pair :trigger new-val))
      ((:actions :when :unless :pre-handlers :post-handlers)
       (cl-case (car new-val)
         (:add (plist-put old-pair prop (-union (plist-get old-pair prop) (cdr new-val))))
         (:rem (plist-put old-pair prop (-difference (plist-get old-pair prop) (cdr new-val))))
         (t
          (cond
           ;; this means we have ((:add ...) (:rem ...)) argument
           ((and new-val
                 (listp (car new-val))
                 (memq (caar new-val) '(:add :rem)))
            (let ((a (assq :add new-val))
                  (r (assq :rem new-val)))
              (plist-put old-pair prop (-union (plist-get old-pair prop) (cdr a)))
              (plist-put old-pair prop (-difference (plist-get old-pair prop) (cdr r)))))
           (t
            (plist-put old-pair prop (plist-get new-pair prop))))))))))

(defun sp--merge-pairs (old-pair new-pair)
  "Merge OLD-PAIR and NEW-PAIR.
This modifies the OLD-PAIR by side effect."
  (let ((ind 0))
    (--each new-pair
      (when (= 0 (% ind 2))
        (sp--merge-prop old-pair new-pair it))
      (setq ind (1+ ind))))
  old-pair)

(defun sp--update-pair (old-pair new-pair)
  "Copy properties from NEW-PAIR to OLD-PAIR.
The list OLD-PAIR must not be nil."
  (let ((ind 0))
    (--each new-pair
      (when (= 0 (% ind 2))
        (when (or (not (plist-get old-pair it))
                  ;; HACK: we don't want to overwrite list properties
                  ;; that aren't just :add with :add because this
                  ;; would break the "idempotency".
                  (not (equal '(:add) (plist-get new-pair it))))
          (plist-put old-pair it (plist-get new-pair it))))
      (setq ind (1+ ind))))
  old-pair)

(defun sp--update-pair-list (pair mode)
  "Update the PAIR for major mode MODE.  If this pair is not
defined yet for this major mode, add it.  If this pair is already
defined, replace all the properties in the old definition with
values from PAIR."
  ;; get the structure relevant to mode.  t means global setting
  (let ((struct (--first (eq mode (car it)) sp-pairs)))
    (if (not struct)
        (!cons (cons mode (list pair)) sp-pairs)
      ;; this does NOT merge changes, only replace the values at
      ;; properties.  Open delimiter works as ID as usual.
      (let ((old-pair (--first (equal (plist-get pair :open)
                                      (plist-get it :open))
                               (cdr struct))))
        (if (not old-pair)
            (setcdr struct (cons pair (cdr struct)))
          (sp--update-pair old-pair pair)))))
  sp-pairs)

(defun sp--get-pair (open list)
  "Get the pair with id OPEN from list LIST."
  (--first (equal open (plist-get it :open)) list))

(defun sp--get-pair-definition (open list &optional prop)
  "Get the definition of a pair identified by OPEN from list LIST.

If PROP is non-nil, return the value of that property instead."
  (let ((pair (sp--get-pair open list)))
    (if prop
        (cond
         ((eq prop :op-l)
          (length (plist-get pair :open)))
         ((eq prop :cl-l)
          (length (plist-get pair :close)))
         ((eq prop :len)
          (+ (length (plist-get pair :open)) (length (plist-get pair :close))))
         ((eq prop :post-handlers)
          (--filter (not (listp it)) (plist-get pair prop)))
         ((eq prop :post-handlers-cond)
          (--filter (listp it) (plist-get pair :post-handlers)))
         ((eq prop :when)
          (--filter (not (listp it)) (plist-get pair :when)))
         ((eq prop :when-cond)
          (-flatten (-concat (--filter (listp it) (plist-get pair :when)))))
         (t (plist-get pair prop)))
      pair)))

(defun sp-get-pair-definition (open mode &optional prop)
  "Get the definition of pair identified by OPEN (opening
delimiter) for major mode MODE (or global definition if MODE is
t).

If PROP is non-nil, return the value of that property instead."
  (sp--get-pair-definition open (cdr (assq mode sp-pairs)) prop))

(defun sp-get-pair (open &optional prop)
  "Return the current value of pair defined by OPEN in the
current buffer, querying the variable `sp-local-pairs'.

If PROP is non-nil, return the value of that property instead."
  (sp--get-pair-definition open sp-local-pairs prop))

(defun sp--merge-with-local (mode)
  "Merge the global pairs definitions with definitions for major mode MODE."
  (let* ((global (cdr (assq t sp-pairs)))
         (local (cdr (assq mode sp-pairs)))
         (result nil))
    ;; copy the pairs on global list first.  This creates new plists
    ;; so we can modify them without changing the global "template"
    ;; values.
    (dolist (old-pair global)
      (!cons (list :open (plist-get old-pair :open)) result))

    ;; merge the global list with result.  This basically "deep copy"
    ;; global list.  We use `sp--merge-pairs' because it also clones
    ;; the list properties (actions, filters etc.)
    (dolist (new-pair global)
      (let ((old-pair (sp--get-pair (plist-get new-pair :open) result)))
        (sp--merge-pairs old-pair new-pair)))

    ;; for each local pair, merge it into the global definition
    (dolist (new-pair local)
      (let ((old-pair (sp--get-pair (plist-get new-pair :open) result)))
        (if old-pair
            (sp--merge-pairs old-pair new-pair)
          ;; pair does not have global definition, simply copy it over
          (!cons
           ;; this "deep copy" the new-pair
           (sp--merge-pairs (list :open (plist-get new-pair :open)) new-pair)
           ;; TODO: remove the nil lists from the definitions
           result))))
    result))

(defun sp-wrap-with-pair (pair)
  "Wrap the following expression with PAIR.

This function is non-interactive helper.  To use this function
interactively, bind the following lambda to a key:

 (lambda (&optional arg) (interactive \"P\") (sp-wrap-with-pair \"(\"))

This lambda accepts the same prefix arguments as
`sp-select-next-thing'.

If region is active and `use-region-p' returns true, the region
is wrapped instead.  This is useful with selection functions in
`evil-mode' to wrap regions with pairs."
  (let* ((arg (or current-prefix-arg 1))
         (sel (and (not (use-region-p))
                   (sp-select-next-thing-exchange
                    arg
                    (cond
                     ;; point is inside symbol and smart symbol wrapping is disabled
                     ((and (sp-point-in-symbol)
                           (or (eq sp-wrap-entire-symbol 'globally)
                               (memq major-mode sp-wrap-entire-symbol)))
                      (point))
                     ;; wrap from point, not the start of the next expression
                     ((and sp-wrap-from-point
                           (not (sp-point-in-symbol)))
                      (point))))))
         (active-pair (--first (equal (car it) pair) sp-pair-list))
         (rb (region-beginning))
         (re (region-end)))
    (goto-char re)
    (insert (cdr active-pair))
    (goto-char rb)
    (insert (car active-pair))
    (if (use-region-p)
        (indent-region rb re)
      (sp-get sel (indent-region :beg :end)))))

(cl-defun sp-pair (open
                   close
                   &key
                   trigger
                   (actions '(wrap insert autoskip navigate))
                   when
                   unless
                   pre-handlers
                   post-handlers
                   wrap
                   bind
                   insert)
  "Add a pair definition.

OPEN is the opening delimiter.  Every pair is uniquely determined
by this string.

CLOSE is the closing delimiter.  You can use nil for this
argument if you are updating an existing definition.  In this
case, the old value is retained.

TRIGGER is an optional trigger for the pair.  The pair will be
inserted if either OPEN or TRIGGER is typed.  This is usually
used as a shortcut for longer pairs or for pairs that can't be
typed easily.

ACTIONS is a list of actions that smartparens will perform with
this pair.  Possible values are:

- insert  - autoinsert the closing pair when opening pair is
  typed.
- wrap    - wrap an active region with the pair defined by opening
  delimiter if this is typed while region is active.
- autoskip - if the sexp is active or `sp-autoskip-closing-pair' is
  set to 'always, skip over the closing delimiter if user types its
  characters in order.
- navigate - enable this pair for navigation/highlight and strictness
  checks

If the ACTIONS argument has value :rem, the pair is removed.
This can be used to remove default pairs you don't want to use.
For example: (sp-pair \"[\" nil :actions :rem)

WHEN is a list of predicates that test whether the action
should be performed in current context.  The values in the list
should be names of the predicates (that is symbols, not
lambdas!).  They should accept three arguments: opening
delimiter (which uniquely determines the pair), action and
context.  The context argument can have values:

- string  - if point is inside string.
- comment - if point is inside comment.
- code    - if point is inside code.  This context is only
  recognized in programming modes that define string semantics.

If *any* filter returns t, the action WILL be performed. A number
of filters are predefined: `sp-point-after-word-p',
`sp-point-before-word-p', `sp-in-string-p',
`sp-point-before-eol-p' etc.

When clause also supports a special format for delayed insertion.
The condition is a list with commands, predicates (with three
arguments as regular when form) or strings specifying the last
event.  All three types can be combined in one list.  The pair
will be inserted *after* the next command if it matches the any
command on the list, if the last event matches any string on the
list or if any predicate returns true.  If the pair's :when
clause contains this special form, it will never be immediately
inserted and will always test for delayed insertion.

UNLESS is a list of predicates.  The conventions are the same as
for the WHEN list.  If *any* filter on this list returns t, the
action WILL NOT be performed.  The predicates in the WHEN list
are checked first, and if any of them succeeds, the UNLESS list
is not checked.

Note: the functions on the WHEN/UNLESS lists are also called
\"filters\" in the documentation.

All the filters are run *after* the trigger character is
inserted.

PRE-HANDLERS is a list of functions that are called before there
has been some action caused by this pair.  The arguments are the
same as for filters.  Context is relative to the point *before*
the last inserted character.  Because of the nature of the
wrapping operation, this hook is not called if the action is
wrapping.

POST-HANDLERS is a list of functions that are called after there
has been some action caused by this pair.  The arguments are the
same as for filters.  Context is relative to current position of
point *after* the closing pair was inserted.

After a wrapping action, the point might end on either side of
the wrapped region, depending on the original direction.  You can
use the variable `sp-last-wrapped-region' to retrieve information
about the wrapped region and position the point to suit your
needs.

A special syntax for conditional execution of hooks is also
supported.  If the added item is a list (function command1
command2...), where funciton is a 3 argument function described
above and command(s) can be either name of a command or a string
representing an event.  If the last command the event as
described by `single-key-description' matches any on the list,
the hook will be executed.  This means these hooks are run not
after the insertion, but after the *next* command is executed.

Example:
  ((lambda (id act con)
     (save-excursion
       (newline))) \"RET\" newline)

This function will move the closing pair on its own line only if
the next command is `newline' or is triggered by RET.  Otherwise
the pairs stay on the same line.

WRAP is a key binding to which a \"wrapping\" action is bound.
The key should be in format that is accepted by `kbd'.  This
option binds a lambda form:

  `(lambda (&optional arg)
     (interactive \"P\")
     (sp-wrap-with-pair ,OPEN))

to the specified key sequence.  The binding is added to global
keymap.  When executed, it wraps ARG (default 1) expressions with
this pair (like `paredit-wrap-round' and friends).  Additionally,
it accepts the same prefix arguments as `sp-select-next-thing'.

BIND is equivalent to WRAP.  It is a legacy setting and will be
removed soon.

INSERT is a key binding to which an \"insert\" action is bound.
The key should be in format that is accepted by `kbd'.  This is
achieved by binding a lambda form:

 (lambda () (interactive) (sp-insert-pair \"pair-id\"))

to the supplied key, where pair-id is the open delimiter of the
pair.  The binding is added to the global map.  You can also bind
a similar lambda manually.  To only bind this in specific major
modes, use this property on `sp-local-pair' instead."
  (if (eq actions :rem)
      (let ((remove (concat
                     (sp-get-pair-definition open t :open)
                     (sp-get-pair-definition open t :close)))
            (global-list (assq t sp-pairs)))
        (setcdr global-list (--remove (equal (plist-get it :open) open) (cdr global-list)))
        (sp--update-trigger-keys remove))
    (let ((pair nil))
      (setq pair (plist-put pair :open open))
      (when close (plist-put pair :close close))
      (when trigger (plist-put pair :trigger trigger))
      (dolist (arg '((:actions . actions)
                     (:when . when)
                     (:unless . unless)
                     (:pre-handlers . pre-handlers)
                     (:post-handlers . post-handlers)))
        ;; We only consider "nil" as a proper value if the property
        ;; already exists in the pair.  In that case, we will set it to
        ;; nil.  This allows for removing properties in global
        ;; definitions.
        (when (or (eval (cdr arg))
                  (sp-get-pair-definition open t (car arg)))
          (plist-put pair (car arg) (eval (cdr arg)))))
      (sp--update-pair-list pair t))
    (sp--update-trigger-keys)
    (when (or wrap bind) (global-set-key (read-kbd-macro (or wrap bind))
                                         `(lambda (&optional arg)
                                            (interactive "P")
                                            (sp-wrap-with-pair ,open))))
    (when insert (global-set-key (kbd insert) `(lambda () (interactive) (sp-insert-pair ,open)))))
  (sp--update-local-pairs-everywhere)
  sp-pairs)

(cl-defun sp-local-pair (modes
                         open
                         close
                         &key
                         trigger
                         (actions '(:add))
                         (when '(:add))
                         (unless '(:add))
                         (pre-handlers '(:add))
                         (post-handlers '(:add))
                         wrap
                         bind
                         insert
                         prefix
                         suffix
                         skip-match)
  "Add a local pair definition or override a global definition.

MODES can be a single mode or a list of modes where these settings
should be applied.

PREFIX is a regular expression matching an optional prefix for
this pair in the specified major modes.  If not specified, the
characters of expression prefix syntax class are automatically
considered instead.  This can be used to attach custom prefixes
to pairs, such as prefix \"\\function\" in \\function{arg} in
`LaTeX-mode'.

SUFFIX is a regular expression matching an optional suffix for
this pair in the specified major modes.  If not specified, the
characters of punctuation syntax class are automatically
considered instead.

The rest of the arguments have same semantics as in `sp-pair'.

If the pair is not defined globally, ACTIONS defaults to (wrap
insert) instead of (:add) (which inherits global settings)

The pairs are uniquely identified by the opening delimiter.  If you
replace the closing one with a different string in the local
definition, this will override the global closing delimiter.

The list arguments can optionally be of form starting with
\":add\" or \":rem\" when these mean \"add to the global list\"
and \"remove from the global list\" respectivelly.  Otherwise,
the global list is replaced.  If you wish to both add and remove
things with single call, use \"((:add ...) (:rem ...))\" as an
argument.  Therefore,

  :when '(:add my-test)

would mean \"use the global settings for this pair, but also this
additional test\". If no value is provided for list arguments,
they default to \"(:add)\" which means they inherit the list from
the global definition.

To disable a pair in a major mode, simply set its actions set to
nil. This will ensure the pair is not even loaded when the mode is
activated.

If WRAP is non-nil, the binding is added into major mode keymap
called \"foo-mode-map\".  If the mode does not follow this
convention, you will need to bind the function manually (see
`sp-pair' to how the function is named for each particular pair).
The bindings are not added into `smartparens-mode-map' to prevent
clashes between different modes.

BIND is equivalent to WRAP.  It is a legacy setting and will be
removed soon.

The binding for INSERT follows the same convention as BIND.  See
`sp-pair' for more info.

You can provide a function SKIP-MATCH, that will take three
arguments: the currently matched delimiter, beginning of match
and end of match.  If this function returns true, the
`sp-get-paired-expression' matcher will ignore this match.  You
can use this to skip over expressions that serve multiple
functions, such as if/end pair or unary if in Ruby or * in
markdown when it signifies list item instead of emphasis.  In
addition, there is a global per major-mode option, see
`sp-navigate-skip-match'."
  (if (eq actions :rem)
      (let ((remove ""))
        (dolist (m (-flatten (list modes)))
          (setq remove (concat remove
                               (sp-get-pair-definition open m :open)
                               (sp-get-pair-definition open m :close)))
          (let ((mode-pairs (assq m sp-pairs)))
            (setcdr mode-pairs
                    (--remove (equal (plist-get it :open) open)
                              (cdr mode-pairs)))))
        (sp--update-trigger-keys remove))
    (dolist (m (-flatten (list modes)))
      (let* ((pair nil))
        (setq pair (plist-put pair :open open))
        (when close (plist-put pair :close close))
        (when trigger (plist-put pair :trigger trigger))
        (when prefix (plist-put pair :prefix prefix))
        (when suffix (plist-put pair :suffix suffix))
        (when skip-match (plist-put pair :skip-match skip-match))
        (when (and (not (sp-get-pair-definition open t))
                   (equal actions '(:add)))
          (setq actions '(wrap insert autoskip navigate)))
        (plist-put pair :actions actions)
        (plist-put pair :when when)
        (plist-put pair :unless unless)
        (plist-put pair :pre-handlers pre-handlers)
        (plist-put pair :post-handlers post-handlers)
        (sp--update-pair-list pair m)
        (-when-let* ((symbol (intern (concat (symbol-name m) "-map")))
                     (map (and (boundp symbol) (symbol-value symbol))))
          (when (or wrap bind) (define-key map
                                 (read-kbd-macro (or wrap bind))
                                 `(lambda (&optional arg)
                                    (interactive "P")
                                    (sp-wrap-with-pair ,open))))
          (when insert (define-key map
                         (kbd insert)
                         `(lambda () (interactive) (sp-insert-pair ,open)))))))
    (sp--update-trigger-keys))
  (sp--update-local-pairs-everywhere (-flatten (list modes)))
  sp-pairs)

(cl-defun sp-local-tag (modes trig open close &key
                              (transform 'identity)
                              (actions '(wrap insert))
                              post-handlers)
  "Add a tag definition.

MODES is a mode or a list of modes where this tag should
activate.  It is impossible to define global tags.

TRIG is the trigger sequence.  It can be a string of any length.
If more triggers share a common prefix, the shortest trigger is
executed.

OPEN is the format of the opening tag.  This is inserted before
the active region.

CLOSE is the format of the closing tag.  This is inserted after
the active region.

Opening and closing tags can optionally contain the _ character.

If the opening tag contains the _ character, after you type the
trigger, the region is wrapped with \"skeleton\" tags and a
special tag editing mode is entered.  The text you now type is
substituted for the _ character in the opening tag.

If the closing tag contains the _ character, the text from the
opening pair is mirrored to the closing pair and substituted for
the _ character.

TRANSFORM is a function name (symbol) that is called to perform a
transformation of the opening tag text before this is inserted to
the closing tag.  For example, in html tag it might simply select
the name of the tag and cut off the tag attributes (like
class/style etc.).  Defaults to identity.

ACTIONS is a list of actions this tag should support. Currently,
only \"wrap\" action is supported.  Usually, you don't need to
specify this argument.

POST-HANDLERS is a list of functions that are called after the
tag is inserted.  If the tag does contain the _ character, these
functions are called after the tag editing mode is exited.  Each
function on this list should accept two arguments: the trigger
string and the action."
  (dolist (mode (-flatten (list modes)))
    (let* ((tag-list (assq mode sp-tags))
           (tag (--first (equal trig (plist-get it :trigger)) (cdr tag-list)))
           (new-tag nil))
      (setq new-tag (plist-put new-tag :trigger trig))
      (plist-put new-tag :open open)
      (plist-put new-tag :close close)
      (when transform (plist-put new-tag :transform transform))
      (when actions (plist-put new-tag :actions actions))
      (when post-handlers (plist-put new-tag :post-handlers post-handlers))
      (if tag-list
          (if (not actions)
              (setcdr tag-list (--remove (equal trig (plist-get it :trigger)) (cdr tag-list)))
            (if (not tag)
                (setcdr tag-list (cons new-tag (cdr tag-list)))
              (sp--update-pair tag new-tag)))
        ;; mode doesn't exist
        (when actions
          (!cons (cons mode (list new-tag)) sp-tags)))))
  (sp--update-trigger-keys))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Overlay management

;; burlywood4
(defface sp-pair-overlay-face
  '((t (:inherit highlight)))
  "The face used to highlight pair overlays."
  :group 'smartparens)

(defface sp-wrap-overlay-face
  '((t (:inherit sp-pair-overlay-face)))
  "The face used to highlight wrap overlays."
  :group 'smartparens)

(defface sp-wrap-tag-overlay-face
  '((t (:inherit sp-pair-overlay-face)))
  "The face used to highlight wrap tag overlays."
  :group 'smartparens)

(defvar sp-pair-overlay-list '()
  "List of overlays used for tracking inserted pairs.

When a pair is inserted, an overlay is created over it.  When the
user starts typing the closing pair we will not insert it again.
If user leaves the overlay, it is canceled and the insertion
works again as usual.")
(make-variable-buffer-local 'sp-pair-overlay-list)

(defvar sp-wrap-overlays nil
  "Cons pair of wrap overlays.")
(make-variable-buffer-local 'sp-wrap-overlays)

(defvar sp-wrap-tag-overlays nil
  "Cons pair of tag wrap overlays.")
(make-variable-buffer-local 'sp-wrap-tag-overlays)

(defvar sp-pair-overlay-keymap (make-sparse-keymap)
  "Keymap for the pair overlays.")
(define-key sp-pair-overlay-keymap (kbd "C-g") 'sp-remove-active-pair-overlay)

(defvar sp-wrap-overlay-keymap (make-sparse-keymap)
  "Keymap for the wrap overlays.")
(define-key sp-wrap-overlay-keymap (kbd "C-g") 'sp-wrap-cancel)

(defvar sp-wrap-tag-overlay-keymap (make-sparse-keymap)
  "Keymap for the wrap tag overlays.")
(define-key sp-wrap-tag-overlay-keymap (kbd "C-g") 'sp-wrap-tag-done)
(define-key sp-wrap-tag-overlay-keymap (kbd "C-a") 'sp-wrap-tag-beginning)
(define-key sp-wrap-tag-overlay-keymap (kbd "C-e") 'sp-wrap-tag-end)

(defun sp--overlays-at (&optional pos)
  "Simple wrapper of `overlays-at' to get only overlays from
smartparens.  Smartparens functions must use this function
instead of `overlays-at' directly."
  (--filter (overlay-get it 'type) (overlays-at (or pos (point)))))

(defun sp--point-in-overlay-p (overlay)
  "Return t if point is in OVERLAY."
  (and (< (point) (overlay-end overlay))
       (> (point) (overlay-start overlay))))

(defun sp--get-overlay-length (overlay)
  "Compute the length of OVERLAY."
  (- (overlay-end overlay) (overlay-start overlay)))

(defun sp--get-active-overlay (&optional type)
  "Get active overlay.  Active overlay is the shortest overlay at
point.  Optional argument TYPE restrict overlays to only those
with given type."
  (let ((overlays (sp--overlays-at)))
    (when type
      (setq overlays (--filter (eq (overlay-get it 'type) type) overlays)))
    (cond
     ((not overlays) nil)
     ((not (cdr overlays)) (car overlays))
     (t
      (--reduce (if (< (sp--get-overlay-length it) (sp--get-overlay-length acc)) it acc) overlays)))))

(defun sp--pair-overlay-create (start end id)
  "Create an overlay over the currently inserted pair for
tracking the position of the point.  START and END are the
boundaries of the overlay, ID is the id of the pair."
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'priority 100)
    (overlay-put overlay 'keymap sp-pair-overlay-keymap)
    (overlay-put overlay 'pair-id id)
    (overlay-put overlay 'type 'pair)
    (!cons overlay sp-pair-overlay-list)
    (sp--pair-overlay-fix-highlight)
    (add-hook 'post-command-hook 'sp--pair-overlay-post-command-handler nil t)))

(defun sp-wrap-cancel (&optional can-delete)
  "Cancel the active wrapping.

If optional argument CAN-DELETE is non-nil the
`sp-last-inserted-characters' string is inserted at `sp-wrap-point'."
  (interactive)
  (let ((oleft (car sp-wrap-overlays))
        (oright (cdr sp-wrap-overlays)))
    ;; kill the insides of the "pair" if `delete-selection-mode'
    ;; emulation is enabled
    (when (and (sp--delete-selection-p) can-delete)
      (kill-region (overlay-end oleft) (overlay-start oright))
      (setq sp-wrap-point (overlay-start oleft)))
    (delete-region (overlay-start oleft) (overlay-end oleft))
    (delete-region (overlay-start oright) (overlay-end oright))
    (delete-overlay oleft)
    (delete-overlay oright)
    (setq sp-wrap-overlays nil)
    (setq sp-previous-point -1)

    (goto-char sp-wrap-point)
    (when can-delete
      (insert sp-last-inserted-characters))))

(defun sp--pair-overlay-fix-highlight ()
  "Fix highlighting of the pair overlays.  Only the active overlay
should be highlighted."
  (--each (sp--overlays-at) (overlay-put it 'face nil))
  (let* ((active (sp--get-active-overlay))
         (type (and active (overlay-get active 'type))))
    (if active
        (cond
         ((eq 'wrap-tag type)
          (when sp-highlight-wrap-tag-overlay
            (overlay-put active 'face 'sp-wrap-tag-overlay-face)))
         ((eq 'pair type)
          (when sp-highlight-pair-overlay
            (overlay-put active 'face 'sp-pair-overlay-face))))
      ;; edge case where we're at the end of active overlay.  If
      ;; there is a wrap-tag overlay, restore it's face
      (when sp-wrap-tag-overlays
        (overlay-put (car sp-wrap-tag-overlays) 'face 'sp-wrap-tag-overlay-face)))))

(defun sp--pair-overlay-post-command-handler ()
  "Remove all pair overlays that doesn't have point inside them,
are of zero length, or if point moved backwards."
  ;; if the point moved backwards, remove all overlays
  (if (and sp-cancel-autoskip-on-backward-movement
           (< (point) sp-previous-point))
      (dolist (o sp-pair-overlay-list) (sp--remove-overlay o))
    ;; else only remove the overlays where point is outside them or
    ;; their length is zero
    (dolist (o (--remove (and (sp--point-in-overlay-p it)
                              (> (sp--get-overlay-length it) 0))
                         sp-pair-overlay-list))
      (sp--remove-overlay o)))
  (when sp-pair-overlay-list
    (setq sp-previous-point (point))))

(defun sp-remove-active-pair-overlay ()
  "Deactivate the active overlay.  See `sp--get-active-overlay'."
  (interactive)
  (-when-let (active-overlay (sp--get-active-overlay 'pair))
    (sp--remove-overlay active-overlay)))

(defun sp--remove-overlay (overlay)
  "Remove OVERLAY."
  ;; if it's not a pair overlay, nothing happens here anyway
  (setq sp-pair-overlay-list (--remove (equal it overlay) sp-pair-overlay-list))
  ;; if we have zero pair overlays, remove the post-command hook
  (when (not sp-pair-overlay-list)
    (remove-hook 'post-command-hook 'sp--pair-overlay-post-command-handler t)
    ;; this is only updated when sp--pair-overlay-post-command-handler
    ;; is active.  Therefore, we need to reset this to 1.  If not, newly
    ;; created overlay could be removed right after creation - if
    ;; sp-previous-point was greater than actual point
    (setq sp-previous-point -1))
  (delete-overlay overlay)
  (sp--pair-overlay-fix-highlight))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Action predicates

(defun sp-in-string-p (id action context)
  "Return t if point is inside string or comment, nil otherwise."
  (eq context 'string))

(defun sp-in-code-p (id action context)
  "Return t if point is inside code, nil otherwise."
  (eq context 'code))

(defun sp-in-comment-p (id action context)
  "Return t if point is inside comment, nil otherwise."
  (eq context 'comment))

(defun sp-in-math-p (id action context)
  "Return t if point is inside code, nil otherwise."
  (when (functionp 'texmathp)
    (texmathp)))

(defun sp-point-before-eol-p (id action context)
  "Return t if point is followed by optional white spaces and end of line, nil otherwise.
This predicate is only tested on \"insert\" action."
  (when (eq action 'insert)
    (looking-at "\\s-*$")))

(defun sp-point-after-bol-p (id action context)
  "Return t if point follows beginning of line and possibly white spaces, nil otherwise.
This predicate is only tested on \"insert\" action."
  (when (eq action 'insert)
    (looking-back (concat "^\\s-*" (regexp-quote id)))))

(defun sp-point-at-bol-p (id action context)
  "Return t if point is at the beginning of line, nil otherwise.
This predicate is only tested on \"insert\" action."
  (when (eq action 'insert)
    (looking-back (concat "^" (regexp-quote id)))))

(defun sp-point-before-symbol-p (id action context)
  "Return t if point is followed by a symbol, nil otherwise.
This predicate is only tested on \"insert\" action."
  (when (eq action 'insert)
    (looking-at "\\s_")))

(defun sp-point-before-word-p (id action context)
  "Return t if point is followed by a word, nil otherwise.
This predicate is only tested on \"insert\" action."
  (when (eq action 'insert)
    (looking-at "\\sw\\|\\s_")))

(defun sp-point-after-word-p (id action context)
  "Return t if point is after a word, nil otherwise.
This predicate is only tested on \"insert\" action."
  (when (eq action 'insert)
    (looking-back (concat "\\(\\sw\\|\\s_\\)" (regexp-quote id)))))

(defun sp-point-before-same-p (id action context)
  "Return t if point is followed by ID, nil otherwise.
This predicate is only tested on \"insert\" action."
  (when (eq action 'insert)
    (sp--looking-at (regexp-quote id))))

(defun sp-point-in-empty-line-p (id action context)
  "Return t if point is on an empty line, nil otherwise"
  (and (looking-at "\\s-*$")
       (looking-back (concat "^\\s-*" (regexp-quote id)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pair insertion/deletion/skipping

(defun sp--do-action-p (id action &optional use-inside-string)
  "Return t if action ACTION can be performed with pair ID.

If ACTION is a list, return t if at least one action from the
list can be performed.

If USE-INSIDE-STRING is non-nil, use value of
`sp-point-inside-string' instead of testing with
`sp-point-in-string-or-comment'."
  (setq action (-flatten (list action)))
  (let* ((actions (sp-get-pair id :actions))
         (when-l (sp-get-pair id :when))
         (unless-l (sp-get-pair id :unless))
         (in-string (if use-inside-string
                        sp-point-inside-string
                      (sp-point-in-string)))
         (context (cond
                   (in-string 'string)
                   ((sp-point-in-comment) 'comment)
                   (t 'code)))
         a r)
    (while (and action (not r))
      (setq a (car action))
      (setq r (when (memq a actions)
                ;;(and (when-clause) (not (unless-clause)))
                (and (or (not when-l)
                         (ignore-errors (run-hook-with-args-until-success 'when-l id a context)))
                     (or (not unless-l)
                         (not (ignore-errors (run-hook-with-args-until-success 'unless-l id a context)))))))
      (!cdr action))
    r))

(defun sp--get-handler-context (type)
  "Return the context constant.  TYPE is type of the handler."
  (let ((in-string (cl-case type
                     (:pre-handlers
                      (save-excursion
                        (backward-char 1)
                        (sp-point-in-string-or-comment)))
                     (:post-handlers
                      (sp-point-in-string-or-comment)))))
    (if in-string 'string 'code)))

(defun sp--get-context (&optional point in-string in-comment)
  "Return the context of POINT.

If the optional arguments IN-STRING or IN-COMMENT non-nil, their
value is used instead of a test."
  (save-excursion
    (goto-char (or point (point)))
    (cond
     ((or in-string (sp-point-in-string)) 'string)
     ((or in-comment (sp-point-in-comment)) 'comment)
     (t 'code))))

(defun sp--parse-insertion-spec (fun)
  "Parse the insertion specification FUN and return a form to evaluate."
  (let ((spec nil)
        (after nil)
        (last 1))
    (cl-labels ((push-non-empty
                 (what)
                 (unless (equal (cadr what) "")
                   ;; relies on dynamic binding
                   (push what spec))))
      (with-temp-buffer
        (insert fun)
        (goto-char (point-min))
        (while (re-search-forward "\\(|\\|\\[\\)" nil t)
          (cond
           ((equal (match-string 0) "[")
            (if (save-excursion (backward-char 1) (eq (preceding-char) 92))
                (push-non-empty `(insert ,(concat (buffer-substring-no-properties last (- (point) 2)) "[")))
              (push-non-empty `(insert ,(buffer-substring-no-properties last (1- (point)))))
              (let* ((p (point))
                     (fun-end (progn
                                (re-search-forward "]" nil t)
                                (1- (point))))
                     (fun-spec (buffer-substring-no-properties p fun-end))
                     (instruction (cond
                                   ((equal fun-spec "i")
                                    '(indent-according-to-mode)))))
                (when instruction (push instruction spec)))))
           ((equal (match-string 0) "|")
            (cond
             ((save-excursion (backward-char 1) (eq (preceding-char) 92))
              (push-non-empty `(insert ,(concat (buffer-substring-no-properties last (- (point) 2)) "|"))))
             (t
              (push-non-empty `(insert ,(buffer-substring-no-properties last (1- (point)))))
              (push 'save-excursion spec)
              (when (eq (following-char) 124)
                (forward-char 1)
                (setq after '(indent-according-to-mode)))))))
          (setq last (point)))
        (push-non-empty `(insert ,(buffer-substring-no-properties last (point-max)))))
      (let* ((specr (nreverse spec))
             (specsplit (--split-with (not (eq it 'save-excursion)) specr))
             (re (-concat (car specsplit) (if (cadr specsplit) (cdr specsplit) nil))))
        (cons 'progn (if after (-snoc re after) re))))))

(defun sp--run-function-or-insertion (fun id action context)
  "Run a function or insertion.

If FUN is a function, call it with `funcall' with ID, ACTION and
CONTEXT as arguments.

If FUN is a string, interpret it as \"insertion specification\",
see `sp-pair' for description."
  (cond
   ((functionp fun)
    (funcall fun id action context))
   ((stringp fun)
    (eval (sp--parse-insertion-spec fun)))))

(defun sp--run-hook-with-args (id type action)
  "Run all the hooks for pair ID of type TYPE on action ACTION."
  (ignore-errors
    (let ((hook (sp-get-pair id type))
          (context (sp--get-handler-context type)))
      (if hook
          (--each hook (sp--run-function-or-insertion it id action context))
        (let ((tag-hook (plist-get
                         (--first (string-match-p
                                   (replace-regexp-in-string "_" ".*?" (plist-get it :open))
                                   id)
                                  (cdr (assq 'html-mode sp-tags)))
                         type)))
          (run-hook-with-args 'tag-hook id action context))))))

;; TODO: add a test for a symbol property that would tell this handler
;; not to re=set `sp-last-operation'. Useful for example in "macro
;; funcions" like `my-wrap-with-paren'.
(defun sp--post-command-hook-handler ()
  "Handle the situation after some command has executed."
  (ignore-errors
    (when smartparens-mode
      ;; handle the wrap overlays
      (when sp-wrap-overlays
        (let* ((overlay (car sp-wrap-overlays))
               (start (overlay-start overlay))
               (end (overlay-end overlay))
               (p (point)))
          (when (or (< p sp-previous-point)
                    (> p end)
                    (< p start))
            (sp-wrap-cancel))))
      (when sp-wrap-overlays
        (setq sp-previous-point (point)))

      ;; Here we run the delayed hooks. See issue #80
      (when (and (not (eq sp-last-operation 'sp-insert-pair))
                 sp-last-inserted-pair)
        (let ((hooks (sp-get-pair sp-last-inserted-pair :post-handlers-cond)))
          (--each hooks
            (let ((fun (car it))
                  (conds (cdr it)))
              (when (or (--any? (eq this-command it) conds)
                        (--any? (equal (single-key-description last-command-event) it) conds))
                (sp--run-function-or-insertion
                 fun sp-last-inserted-pair 'insert
                 (sp--get-handler-context :post-handlers))))))
        (setq sp-last-inserted-pair nil))

      ;; Here we run the delayed insertion. Some details in issue #113
      (when (and (not (eq sp-last-operation 'sp-insert-pair-delayed))
                 sp-delayed-pair)
        (let* ((pair (car sp-delayed-pair))
               (beg (cdr sp-delayed-pair))
               (conds (sp-get-pair pair :when-cond))
               (open-pair pair)
               (close-pair (sp-get-pair pair :close)))
          (when (and conds
                     (--any? (cond
                              ((and (commandp it)
                                    (not (stringp it)))
                               (eq this-command it))
                              ((stringp it)
                               (equal (single-key-description last-command-event) it))
                              ((ignore-errors (funcall it pair 'insert (sp--get-handler-context :post-handlers))))) conds))
            ;; TODO: refactor this and the same code in
            ;; `sp-insert-pair' to a separate function
            (sp--run-hook-with-args open-pair :pre-handlers 'insert)
            (insert close-pair)
            (backward-char (length close-pair))
            (sp--pair-overlay-create beg
                                     (+ (point) (length close-pair))
                                     open-pair)
            ;; no auto-escape here? Should be fairly safe
            (sp--run-hook-with-args open-pair :post-handlers 'insert)
            (setq sp-last-inserted-pair open-pair)
            (setq sp-recent-keys nil)
            (setq sp-last-operation 'sp-insert-pair)))
        (setq sp-delayed-pair nil))

      (when (eq sp-last-operation 'sp-insert-pair-delayed)
        (setq sp-last-operation nil))

      (unless (sp--this-command-self-insert-p)
        ;; unless the last command was a self-insert, remove the
        ;; information about the last wrapped region.  It is only used
        ;; for: 1. deleting the wrapping immediately after the wrap,
        ;; 2. re-wrapping region immediatelly after a sucessful wrap.
        ;; Therefore,t he deletion should have no ill-effect.  If the
        ;; necessity will arise, we can add a different flag.
        (setq sp-last-wrapped-region nil)
        (setq sp-last-operation nil)
        (setq sp-recent-keys nil))

      (when show-smartparens-mode
        (if (member this-command sp-show-enclosing-pair-commands)
            (sp-show--pair-enc-function)
          (when (not (eq this-command 'sp-highlight-current-sexp))
            (sp-show--pair-delete-enc-overlays)))))))

(defmacro sp--setaction (action &rest forms)
  (declare (debug (form body)))
  `(unless action
     (setq action (progn ,@forms))))

(defun sp--self-insert-command (arg)
  "This command is a wrapper around `self-insert-command'.

If the just-typed key is a possible trigger for any pair,
`self-insert-command' is called and the special behaviours are
handled in its advice provided by `smartparens-mode'.  If the
just-typed key is not a trigger, fall back to the command that
would execute if smartparens-mode were disabled."
  (interactive "p")
  (if (and (member (sp--single-key-description last-command-event) sp-trigger-keys)
           (not buffer-read-only))
      (progn
        (setq this-command 'self-insert-command)
        (self-insert-command arg))
    (sp--call-fallback-command)))

(defun sp--call-fallback-command ()
  "Call the command bound to last key sequence as if SP were disabled."
  (let ((com (sp--keybinding-fallback
              (when buffer-read-only
                (single-key-description last-command-event))))
        (smartparens-mode nil))
    (when (and com (commandp com))
      (setq this-original-command com)
      (call-interactively com))))

(defadvice self-insert-command (around self-insert-command-adviced activate)
  (setq sp-point-inside-string (sp-point-in-string))
  (setq sp-buffer-modified-p (buffer-modified-p))

  ad-do-it

  (when smartparens-mode
    (setq sp-recent-keys (cons
                          (sp--single-key-description last-command-event)
                          (-take 19 sp-recent-keys)))
    (let (op action)
      (if (= 1 (ad-get-arg 0))
          (progn
            (setq op sp-last-operation)
            (cond
             ((region-active-p)
              (sp-wrap-region-init))
             (sp-wrap-overlays
              (sp-wrap-region))
             (t
              (sp--setaction action (sp-insert-pair))
              (sp--setaction action (sp-skip-closing-pair))
              ;; try to call the fallback function bound to this key.
              ;; That is a function that would normally run if SP was
              ;; inactive. TODO: should this be customizable?
              (when (not action)
                (let ((fb-fun (sp--keybinding-fallback)))
                  (when (and (not (eq fb-fun 'self-insert-command))
                             (lookup-key sp-keymap (vector last-command-event)))
                    (delete-char -1)
                    (sp--call-fallback-command)
                    (setq action t))))
              ;; if nothing happened, we just inserted a character, so
              ;; set the apropriate operation.  We also need to check
              ;; for `sp--self-insert-no-escape' not to overwrite
              ;; it.  See `sp-autoinsert-quote-if-followed-by-closing-pair'.
              (when (and (not action)
                         (not (eq sp-last-operation 'sp-self-insert-no-escape)))
                (setq sp-last-operation 'sp-self-insert))
              ;; if it was a quote, escape it
              (when (and (eq sp-last-operation 'sp-self-insert)
                         sp-point-inside-string
                         sp-autoescape-string-quote
                         (or (and (eq (preceding-char) ?\")
                                  (eq sp-point-inside-string ?\"))
                             (and (eq (preceding-char) ?')
                                  (eq sp-point-inside-string ?'))))
                (save-excursion
                  (backward-char 1)
                  (insert sp-escape-char))))))
        (setq sp-last-operation 'sp-self-insert)))))

(defun sp--delete-selection-mode-handle (&optional from-wrap)
  "Call the original `delete-selection-pre-hook'."
  (if smartparens-mode
      (cond
       ;; try the cua-mode emulation with `cua-delete-selection'
       ((and (boundp 'cua-mode) cua-mode
             (or (not (sp--this-original-command-self-insert-p))
                 (not sp-autowrap-region)))
        ;; if sp-autowrap-region is disabled, we need to translate
        ;; `sp--cua-replace-region' back to `self-insert-command'
        ;; because this is *pre* command hook
        ;; TODO: why do we need sp-cua-replace-region?
        (when (and (not sp-autowrap-region)
                   (eq this-command 'sp--cua-replace-region))
          (setq this-command 'self-insert-command))
        (cua--pre-command-handler))
       ;; this handles the special case after `self-insert-command' if
       ;; `sp-autowrap-region' is t.
       ((and (boundp 'cua-mode) cua-mode from-wrap)
        (setq this-command this-original-command)
        (cua-replace-region))
       ;; if not self-insert, just run the hook from
       ;; `delete-selection-mode'
       ((and (boundp 'delete-selection-mode) delete-selection-mode
             (or from-wrap
                 (not sp-autowrap-region)
                 (not (sp--this-original-command-self-insert-p))))
        (delete-selection-pre-hook)))
    ;; this handles the callbacks properly if the smartparens mode is
    ;; disabled.  Smartparens-mode adds advices on cua-mode and
    ;; delete-selection-mode that automatically remove the callbacks
    (cond
     ((and (bound-and-true-p cua-mode)
           (not (member 'cua--pre-command-handler pre-command-hook)))
      (cua--pre-command-handler))
     ((and (bound-and-true-p delete-selection-mode)
           (not (member 'delete-selection-pre-hook pre-command-hook)))
      (delete-selection-pre-hook)))))

(defun sp--pre-command-hook-handler ()
  "Main handler of pre-command-hook.

Handle the `delete-selection-mode' or `cua-delete-selection'
stuff here."
  (sp--delete-selection-mode-handle))

(defun sp--get-recent-keys ()
  "Return 10 recent keys in reverse order (most recent first) as a string."
  (apply #'concat sp-recent-keys))

(defun sp--get-pair-list ()
  "Return all pairs that are recognized in this
`major-mode' and do not have same opening and closing delimiter.
This is used for navigation functions."
  (--filter (not (string= (car it) (cdr it))) sp-pair-list))

(defun sp--get-stringlike-list ()
  "Return all pairs that are recognized in this `major-mode' that
have same opening and closing delimiter."
  (--filter (string= (car it) (cdr it)) sp-pair-list))

(defun sp--get-allowed-pair-list ()
  "Return all pairs that are recognized in this
`major-mode', do not have same opening and closing delimiter and
are allowed in the current context.  See also
`sp--get-pair-list'."
  (--filter (and (sp--do-action-p (car it) 'navigate)
                 (not (equal (car it) (cdr it)))) sp-pair-list))

(defun sp--get-allowed-stringlike-list ()
  "Return all pairs that are recognized in this `major-mode',
have the same opening and closing delimiter and are allowed in
the current context."
  (--filter (and (sp--do-action-p (car it) 'navigate)
                 (equal (car it) (cdr it))) sp-pair-list))

(defun sp--get-pair-list-context (&optional action)
  "Return all pairs that are recognized in this `major-mode' and
are allowed in the current context."
  (setq action (or action 'insert))
  (--filter (sp--do-action-p (car it) action) sp-pair-list))

(defun sp--get-pair-list-wrap ()
  "Return the list of all pairs that can be used for wrapping."
  (--filter (sp--do-action-p (car it) 'wrap) sp-pair-list))

(defun sp--wrap-regexp (string start end)
  "Wraps regexp with start and end boundary conditions to avoid
matching symbols in symbols."
  (concat "\\(?:" (when start "\\<") string (when end "\\>") "\\)"))

(defun sp--regexp-for-group (parens &rest strings)
  "Generates an optimized regexp matching all string, but with
extra boundary conditions depending on parens."
  (let* ((start (car parens))
         (end (cadr parens)))
    (sp--wrap-regexp (regexp-opt strings) start end)))

(defun sp--strict-regexp-opt (strings &optional ignored)
  "Like regexp-opt, but with extra boundary conditions to ensure
that the strings are not matched in-symbol."
  (--> strings
    (-group-by (lambda (string)
                 (list (and (string-match-p "\\`\\<" string) t)
                       (and (string-match-p "\\>\\'" string) t)))
               it)
    (mapconcat (lambda (g) (apply 'sp--regexp-for-group g)) it "\\|")
    (concat "\\(?:" it "\\)")))

(defun sp--strict-regexp-quote (string)
  "Like regexp-quote, but make sure that the string is not
matched in-symbol."
  (sp--wrap-regexp (regexp-quote string)
                   (string-match-p "\\`\\<" string)
                   (string-match-p "\\>\\'" string)))

(cl-defun sp--get-opening-regexp (&optional (pair-list (sp--get-pair-list)))
  "Return regexp matching any opening pair."
  (sp--strict-regexp-opt (--map (car it) pair-list)))

(cl-defun sp--get-closing-regexp (&optional (pair-list (sp--get-pair-list)))
  "Return regexp matching any closing pair."
  (sp--strict-regexp-opt (--map (cdr it) pair-list)))

(cl-defun sp--get-allowed-regexp (&optional (pair-list (sp--get-allowed-pair-list)))
  "Return regexp matching any opening or closing
delimiter for any pair allowed in current context."
  (sp--strict-regexp-opt (--mapcat (list (car it) (cdr it)) pair-list)))

(cl-defun sp--get-stringlike-regexp (&optional (pair-list (sp--get-allowed-stringlike-list)))
  (regexp-opt (--map (car it) pair-list)))

(defun sp-pair-is-stringlike-p (delim)
  (--first (equal delim (car it)) (sp--get-allowed-stringlike-list)))

(defun sp--get-last-wraped-region (beg end open close)
  "Return `sp-get-sexp' style plist about the last wrapped region.

Note: this function does not retrieve the actual value of
`sp-last-wrapped-region', it merely construct the plist from the
provided values."
  (let ((b (make-marker))
        (e (make-marker)))
    (set-marker b beg)
    (set-marker e end)
    (set-marker-insertion-type e t)
    `(:beg ,b :end ,e :op ,open :cl ,close :prefix "")))

(defun sp-wrap-region-init ()
  "Initialize the region wrapping."
  (when sp-autowrap-region
    ;; if we can't possibly form a wrap, just insert the char and do
    ;; nothing.  If `sp--delete-selection-p' is true, run
    ;; `sp--delete-selection-mode-handle' with t that means it was
    ;; called from withing wrapping procedure
    (if (--none? (string-prefix-p (sp--single-key-description last-command-event) (car it)) (sp--get-pair-list-wrap))
        (let ((p (1- (point)))
              (m (mark)))
          ;; test if we can at least start a tag wrapping.  If not,
          ;; delete the region if apropriate
          (unless (sp-wrap-tag-region-init)
            (sp--delete-selection-mode-handle t)
            (when (and (sp--delete-selection-p)
                       (< m p)
                       (= (length (sp--single-key-description last-command-event)) 1))
              (insert (sp--single-key-description last-command-event)))))
      (let* ((p (1- (point))) ;; we want the point *before* the
             ;; insertion of the character
             (m (mark))
             (ostart (if (> p m) m p))
             (oend (if (> p m) p m))
             (last-keys (sp--get-recent-keys))
             ;;(last-keys "\"\"\"\"\"\"\"\"")
             (active-pair (--first (string-prefix-p (sp--reverse-string (car it)) last-keys) (sp--get-pair-list-wrap))))

        (deactivate-mark)
        ;; if we can wrap right away, do it without creating overlays,
        ;; we can save ourselves a lot of needless trouble :)
        (if active-pair
            (unless (sp-wrap-tag-region-init)
              (let* ((len (+ (length (car active-pair)) (length (cdr active-pair))))
                     (strbound))
                (if (< p m)
                    (progn
                      ;; we delete the opening here to determine the
                      ;; string bounds... not pretty, but there's
                      ;; probably no better solution.
                      (delete-char -1)
                      (setq strbound (sp-get-quoted-string-bounds))
                      ;; and insert it right back
                      (insert (car active-pair))
                      (save-excursion
                        (goto-char m)
                        (insert (cdr active-pair))))
                  (delete-char (- 1))
                  (setq strbound (sp-get-quoted-string-bounds))
                  (insert (cdr active-pair))
                  (goto-char m)
                  (insert (car active-pair))
                  (goto-char (+ len p)))
                (setq sp-last-operation 'sp-wrap-region)
                (setq sp-last-wrapped-region
                      (if (< p m)
                          (sp--get-last-wraped-region
                           p (+ len m -1)
                           (car active-pair) (cdr active-pair))
                        (sp--get-last-wraped-region
                         m (+ len p)
                         (car active-pair) (cdr active-pair))))
                ;; only autoescape "" pair, so it has to be one-char
                ;; length, therefore we can handle it here
                (when (and (equal (car active-pair) "\"")
                           (equal (cdr active-pair) "\""))
                  (sp--wrap-region-autoescape strbound))
                sp-last-wrapped-region)

              (sp--run-hook-with-args (car active-pair) :post-handlers 'wrap))

          ;; save the position and point so we can restore it on cancel.
          (setq sp-wrap-point p)
          (setq sp-wrap-mark m)

          ;; We need to remember what was removed in case wrap is
          ;; cancelled.  Then these characters are re-inserted.
          (setq sp-last-inserted-characters (sp--single-key-description last-command-event))

          ;; if point > mark, we need to remove the character at the end and
          ;; insert it to the front.
          (when (> p m)
            (delete-char (- 1))
            (goto-char ostart)
            (insert sp-last-inserted-characters)
            (setq oend (1+ oend)))

          (let* ((oleft (make-overlay ostart (1+ ostart) nil nil t))
                 (oright (make-overlay oend oend nil nil t)))

            ;; insert the possible pair into end overlay
            (let ((close-pair (cdr (--last (string-prefix-p
                                            sp-last-inserted-characters
                                            (car it))
                                           (sp--get-pair-list-wrap)))))
              (when close-pair
                (save-excursion
                  (goto-char oend)
                  (insert close-pair))))

            (setq sp-wrap-overlays (cons oleft oright))
            (when sp-highlight-wrap-overlay
              (overlay-put oleft 'face 'sp-wrap-overlay-face)
              (overlay-put oright 'face 'sp-wrap-overlay-face))
            (overlay-put oleft 'priority 100)
            (overlay-put oright 'priority 100)
            (overlay-put oleft 'keymap sp-wrap-overlay-keymap)
            (overlay-put oleft 'type 'wrap)

            (goto-char (1+ ostart))))))))

(defun sp-wrap-region ()
  "Wrap region."
  ;; this method is only called if there's an active region.  It should
  ;; never be called manually!
  (when sp-autowrap-region
    (let* ((oleft (car sp-wrap-overlays))
           (oright (cdr sp-wrap-overlays)))
      (setq sp-last-inserted-characters
            (concat sp-last-inserted-characters
                    (sp--single-key-description last-command-event)))
      (let* ((active-pair (--last (string-prefix-p
                                   sp-last-inserted-characters
                                   (car it))
                                  (sp--get-pair-list-wrap)))
             (open-pair (car active-pair))
             (close-pair (cdr active-pair)))

        ;; call sp-wrap-tag-region-init here.  See if we can extend the
        ;; current wrap-beginning into a tag
        (unless (sp-wrap-tag-region-init)
          ;; update the close pair
          (if close-pair
              (save-excursion
                (delete-region (overlay-start oright) (overlay-end oright))
                (goto-char (overlay-start oright))
                (insert close-pair))
            ;; if we don't have any, it means there is no way to
            ;; complete the pair...  abort
            (sp-wrap-cancel t))

          ;; we've completed a pairing!
          (when (equal sp-last-inserted-characters open-pair)
            (let ((s (overlay-start oleft))
                  (e (overlay-end oright))
                  (oplen (length open-pair))
                  (cplen (length close-pair)))
              (delete-overlay oleft)
              (delete-overlay oright)
              (setq sp-wrap-overlays nil)
              (setq sp-previous-point -1)
              (if (< sp-wrap-point sp-wrap-mark)
                  (goto-char (+ sp-wrap-point oplen))
                (goto-char (+ sp-wrap-point oplen cplen)))
              ;; update info for possible following delete
              (setq sp-last-operation 'sp-wrap-region)
              (setq sp-last-wrapped-region
                    (sp--get-last-wraped-region s e open-pair close-pair))

              (sp--run-hook-with-args open-pair :post-handlers 'wrap))))))))

(defun sp--get-active-tag (recent)
  "Return the first tag that matches its trigger to
the prefix of RECENT and is allowed in current mode.  Such a tag
should be unique."
  ;; extract all the triggers that are prefix of the "recent"
  ;; vector, then sort them by length and return the shortest one.
  (let* ((tag-list (assq major-mode sp-tags))
         (triggers-list (--map (plist-get it :trigger) (cdr tag-list)))
         (triggers (--filter (string-prefix-p recent it) triggers-list)))
    (setq triggers (sort triggers (lambda (x y) (< (length x) (length y)))))
    (when (car triggers)
      (--first (equal (car triggers) (plist-get it :trigger)) (cdr tag-list)))))

(defun sp-wrap-tag-region-init ()
  "Init a region wrapping with a tag pair.
This is called from `sp-wrap-region-init' or
`sp-wrap-region' (usually on failure) to see if the currently
entered \"wrap\" can be extended as a tag.  The tag always gets
priority from the regular wrap."
  (when sp-autowrap-region
    ;; we can either enter tagwrapping from already present wrap or
    ;; from nothing (if the wrap-init failed to find any usable wrap)
    ;; or at failure (the entered wrap doesn't match any pair)
    (if sp-wrap-overlays ;; called from within the wrap-mode
        (let* ((oleft (car sp-wrap-overlays))
               (oright (cdr sp-wrap-overlays))
               (active-tag (sp--get-active-tag sp-last-inserted-characters)))
          (when active-tag
            ;; if we've found a tag trigger, enter the tag editing mode
            (if (eq (length sp-last-inserted-characters) (length (plist-get active-tag :trigger)))
                (progn
                  (delete-region (overlay-start oright) (overlay-end oright))
                  (sp--wrap-tag-create-overlays active-tag
                                                (overlay-start oleft)
                                                (-
                                                 (overlay-start oright)
                                                 (sp--get-overlay-length oleft)))
                  (delete-overlay oleft)
                  (delete-overlay oright)
                  (setq sp-wrap-overlays nil)
                  (setq sp-previous-point -1)) ;; do we need this?
              t) ;; return t as it is possible to extend current wrap
            ;; into a tag insertion mode
            ))
      ;; here we need to look at the last inserted character
      (let* ((p (1- (point)))
             (m (mark))
             (ostart (if (> p m) m p))
             (oend (if (> p m) p m))
             (active-tag (sp--get-active-tag
                          (sp--single-key-description last-command-event))))
        (when active-tag
          (setq sp-last-inserted-characters (sp--single-key-description last-command-event))
          (setq sp-wrap-point p)
          (setq sp-wrap-mark m)
          (when (> p m)
            (delete-char (- 1))
            (goto-char ostart)
            (insert sp-last-inserted-characters))
          (when (> m p) (setq oend (1- oend)))
          (if (= 1 (length (plist-get active-tag :trigger)))
              ;; the tag is only 1 character long, we can enter
              ;; insertion mode right away
              (sp--wrap-tag-create-overlays active-tag ostart oend)
            ;; we don't have a wrap, but we can maybe start a tag
            ;; wrap.  So just init the wrapping overlays as usual, and
            ;; let `sp-wrap-region' handle it
            (let* ((oleft (make-overlay ostart (1+ ostart) nil nil t))
                   (oright (make-overlay oend oend nil nil t)))
              (setq sp-wrap-overlays (cons oleft oright))
              (when sp-highlight-wrap-overlay
                (overlay-put oleft 'face 'sp-wrap-overlay-face)
                (overlay-put oright 'face 'sp-wrap-overlay-face))
              (overlay-put oleft 'priority 100)
              (overlay-put oright 'priority 100)
              (overlay-put oleft 'keymap sp-wrap-overlay-keymap)
              (overlay-put oleft 'type 'wrap)
              (goto-char (1+ ostart)))))))))

(defun sp--wrap-tag-create-overlays (tag ostart oend &optional no-cleanup)
  "Create the wrap tag overlays.

TAG is the tag definition from `sp-tags'.

OSTART is the start of the modified area, including the pair
trigger string.

OEND is the end of the modified area, that is the end of the
wrapped region, exluding any existing possible wrap."
  (let* ((tag-open (sp--split-string (plist-get tag :open) "_"))
         (tag-close (sp--split-string (plist-get tag :close) "_"))
         (o (apply #'+ (mapcar #'length tag-open)))
         (c (apply #'+ (mapcar #'length tag-close))))
    ;; setup the wrap pairs
    ;; opening one
    (goto-char ostart)
    (unless no-cleanup (delete-char (length (plist-get tag :trigger))))
    (insert (apply #'concat tag-open))
    (backward-char (length (cadr tag-open)))

    ;; closing one
    (save-excursion
      (goto-char (+ oend o))
      (insert (apply #'concat tag-close)))

    (if (cdr (split-string (plist-get tag :open) "_"))
        (let ((oleft (make-overlay
                      (+ ostart (length (car tag-open)))
                      (+ ostart (length (car tag-open)))
                      nil nil t))
              (oright (make-overlay
                       (+ oend o (length (car tag-close)))
                       (+ oend o (length (car tag-close)))
                       nil nil t)))
          (setq sp-wrap-tag-overlays (cons oleft oright))
          (when sp-highlight-wrap-tag-overlay
            (overlay-put oleft 'face 'sp-wrap-tag-overlay-face)
            (overlay-put oright 'face 'sp-wrap-tag-overlay-face))
          (overlay-put oleft 'priority 100)
          (overlay-put oright 'priority 100)
          (overlay-put oleft 'keymap sp-wrap-tag-overlay-keymap)
          (overlay-put oleft 'type 'wrap-tag)
          (overlay-put oleft 'active-tag tag)
          (overlay-put oleft 'modification-hooks '(sp--wrap-tag-update))
          (overlay-put oleft 'insert-in-front-hooks '(sp--wrap-tag-update))
          (overlay-put oleft 'insert-behind-hooks '(sp--wrap-tag-update))
          (add-hook 'post-command-hook 'sp--wrap-tag-post-command-handler))
      ;; if the tag didn't have any substitution, that means we only
      ;; insert the "brackets" and not enter the tag-insertion mode.
      ;; Therefore we move the point to the original position, so it
      ;; behaves just like normal wrap
      (if (> sp-wrap-mark sp-wrap-point)
          (goto-char (+ sp-wrap-point o))
        (goto-char (+ sp-wrap-point o c)))
      (let ((post-handlers (plist-get tag :post-handlers)))
        (run-hook-with-args 'post-handlers (plist-get tag :trigger) 'wrap)))
    (setq sp-last-operation 'sp-wrap-tag)))

(defun sp--wrap-tag-update (overlay after? beg end &optional length)
  "Called after any modification inside wrap tag overlay."
  (let* ((oleft (car sp-wrap-tag-overlays))
         (oright (cdr sp-wrap-tag-overlays))
         (active-tag (overlay-get oleft 'active-tag))
         (transform (plist-get active-tag :transform))
         (open (buffer-substring (overlay-start oleft) (overlay-end oleft))))
    (when (string-match-p "_" (plist-get active-tag :close))
      (save-excursion
        (delete-region (overlay-start oright) (overlay-end oright))
        (goto-char (overlay-start oright))
        (insert (funcall transform open))))))

(defun sp--wrap-tag-post-command-handler ()
  "Terminate the tag insertion mode if the point jumps out of the tag overlay."
  (if (or (not sp-wrap-tag-overlays)
          (< (point) (overlay-start (car sp-wrap-tag-overlays)))
          (> (point) (overlay-end (car sp-wrap-tag-overlays))))
      (sp-wrap-tag-done)))

(defun sp-match-sgml-tags (tag)
  "Split the html tag TAG at the first space and return its name."
  (let* ((split (split-string tag " "))
         (close (car split)))
    close))

(defun sp-wrap-tag-beginning ()
  "Move point to the beginning of the wrap tag editation area."
  (interactive)
  (goto-char (overlay-start (car sp-wrap-tag-overlays))))

(defun sp-wrap-tag-end ()
  "Move point to the end of the wrap tag editation area."
  (interactive)
  (goto-char (overlay-end (car sp-wrap-tag-overlays))))

(defun sp-wrap-tag-done ()
  "Finish editing of tag."
  (interactive)
  (let* ((oleft (car sp-wrap-tag-overlays))
         (oright (cdr sp-wrap-tag-overlays))
         (active-tag (overlay-get oleft 'active-tag))
         (post-handlers (plist-get active-tag :post-handlers)))
    (delete-overlay oleft)
    (delete-overlay oright)
    (setq sp-wrap-tag-overlays nil)
    (remove-hook 'post-command-hook 'sp--wrap-tag-post-command-handler)
    (run-hook-with-args 'post-handlers (plist-get active-tag :trigger) 'wrap)))

(defun sp--wrap-region-autoescape (strbound)
  "If we wrap a region with \"\" quotes, and the whole region was
inside a string, automatically escape the enclosing quotes.  If
we wrap a region that wasn't a string, automatically quote any
string quotes inside it.

This is internal function and should be only called after a
wrapping."
  (when sp-autoescape-string-quote
    (let ((b (sp-get sp-last-wrapped-region :beg))
          (e (sp-get sp-last-wrapped-region :end))
          was-beg)
      (cond
       ((and strbound
             (> b (car strbound))
             (< e (cdr strbound)))
        ;; the wrapped region is inside a string, escape the enclosing
        ;; quotes
        (save-excursion
          (goto-char b)
          (insert sp-escape-char)
          (goto-char (1- e))
          (insert sp-escape-char))
        ;; update the sp-last-wrapped-region info to \" pair
        (setq sp-last-wrapped-region
              (sp--get-last-wraped-region b e "\\\"" "\\\"")))
       (t
        (setq was-beg (< (point) e))
        (goto-char b)
        (while (sp--search-forward-regexp "\\([^\\]\\)\"" (1- e) t)
          (replace-match "\\1\\\\\"" t))
        (setq sp-last-wrapped-region
              (sp--get-last-wraped-region b e "\"" "\""))
        (if was-beg (goto-char (1+ b)) (goto-char e)))))))

(defun sp--is-number-cons (c)
  (and (consp c) (numberp (car c)) (numberp (cdr c))))

;; TODO: more research is needed
(defun sp--undo-pop-to-last-insertion-node ()
  "Pop all undo info until an insertion node (beg . end) is found.

This can potentially remove some undo important information."
  (while (and buffer-undo-list
              (or (null (car buffer-undo-list)) ;; is nil
                  ;; is not undo action we're interested in
                  (not (sp--is-number-cons (car buffer-undo-list)))))
    (pop buffer-undo-list)))

;; modified from: https://github.com/Fuco1/smartparens/issues/90#issuecomment-18800369
(defun sp--split-last-insertion-undo (len)
  "Split the last insertion node in the `buffer-undo-list' to
include separate pair node."
  (sp--undo-pop-to-last-insertion-node)
  (when buffer-undo-list
    (let* ((previous-undo-actions (cdr buffer-undo-list))
           (beg (caar buffer-undo-list))
           (end (cdar buffer-undo-list))
           first-action second-action)
      (unless (< beg (- end len))
        ;; We need to go back more than one action.  Given the pairs
        ;; are limited to 10 chars now and the chunks seem to be 20
        ;; chars, we probably wouldn't need more.
        (pop buffer-undo-list)
        (sp--undo-pop-to-last-insertion-node)
        (when buffer-undo-list
          (setq beg (caar buffer-undo-list))
          (setq previous-undo-actions (cdr buffer-undo-list))))
      (setq first-action (cons beg (- end len)))
      (setq second-action (cons (- end len) end))
      (setq buffer-undo-list
            (append (list nil second-action nil first-action)
                    previous-undo-actions)))))

(defun sp--string-empty-p (delimeter)
  "Return t if point is inside an empty string."
  (and (equal (char-after (1+ (point))) delimeter)
       (equal (char-after (- (point) 2)) delimeter)))

(defun sp-insert-pair (&optional pair)
  "Automatically insert the closing pair if it is allowed in current context.

If PAIR is provided, use this as pair ID instead of looking
through the recent history of pressed keys.

You can disable this feature completely for all modes and all pairs by
setting `sp-autoinsert-pair' to nil.

You can globally disable insertion of closing pair if point is
followed by the matching opening pair.  It is disabled by
default.  See `sp-autoinsert-if-followed-by-same' for more info."
  (let* ((last-keys (or (and pair (sp--reverse-string pair)) (sp--get-recent-keys)))
         ;; (last-keys "\"\"\"\"\"\"\"\"\"\"\"\"")
         ;; we go through all the opening pairs and compare them to
         ;; last-keys.  If the opair is a prefix of last-keys, insert
         ;; the closing pair.  We also check the :trigger pair
         ;; properties here.
         (trig (--first (and (plist-get it :trigger)
                             (string-prefix-p (sp--reverse-string (plist-get it :trigger)) last-keys))
                        sp-local-pairs))
         (active-pair (or (when trig (cons (plist-get trig :open) (plist-get trig :close)))
                          (--first (string-prefix-p (sp--reverse-string (car it)) last-keys) sp-pair-list)))
         (open-pair (car active-pair))
         (close-pair (cdr active-pair)))
    ;; Test "repeat last wrap" here.  If we wrap a region and then
    ;; type in a pair, wrap again around the last active region.  This
    ;; should probably be tested in the `self-insert-command'
    ;; advice... but we're lazy :D
    (setq trig (or (and trig (plist-get trig :trigger)) open-pair))
    (if (and sp-autowrap-region
             active-pair
             (sp--wrap-repeat-last active-pair))
        sp-last-operation
      (if (not (unwind-protect
                   (progn
                     (when pair (insert pair))
                     (and sp-autoinsert-pair
                          active-pair
                          (if (eq sp-autoskip-closing-pair 'always)
                              (or (not (equal open-pair close-pair))
                                  (not (sp-skip-closing-pair nil t)))
                            t)
                          (sp--do-action-p open-pair 'insert t)
                          (if sp-autoinsert-quote-if-followed-by-closing-pair t
                            (if (and (eq (char-syntax (preceding-char)) ?\")
                                     ;; this is called *after* the character is
                                     ;; inserted.  Therefore, if we are not in string, it
                                     ;; must have been closed just now
                                     (not (sp-point-in-string)))
                                (let ((pattern (sp--get-closing-regexp)))
                                  ;; If we simply insert closing ", we also
                                  ;; don't want to escape it.  Therefore, we
                                  ;; need to set `sp-last-operation'
                                  ;; accordingly to be checked in
                                  ;; `self-insert-command' advice.
                                  (if (sp--looking-at pattern)
                                      (progn (setq sp-last-operation 'sp-self-insert-no-escape) nil)
                                    t))
                              t))
                          (cond
                           ((eq sp-autoinsert-if-followed-by-same 0) t)
                           ((eq sp-autoinsert-if-followed-by-same 1)
                            (not (sp--looking-at (sp--strict-regexp-quote open-pair))))
                           ((eq sp-autoinsert-if-followed-by-same 2)
                            (or (not (sp--looking-at (sp--strict-regexp-quote open-pair)))
                                (and (equal open-pair close-pair)
                                     (eq sp-last-operation 'sp-insert-pair)
                                     (save-excursion
                                       (backward-char 1)
                                       (sp--looking-back (sp--strict-regexp-quote open-pair))))))
                           ((eq sp-autoinsert-if-followed-by-same 3)
                            (or (not (sp--get-active-overlay 'pair))
                                (not (sp--looking-at (sp--strict-regexp-quote open-pair)))
                                (and (equal open-pair close-pair)
                                     (eq sp-last-operation 'sp-insert-pair)
                                     (save-excursion
                                       (backward-char (length trig))
                                       (sp--looking-back (sp--strict-regexp-quote open-pair))))
                                (not (equal open-pair close-pair)))))))
                 (when pair (delete-char (- (length pair))))))
          ;; if this pair could not be inserted, we try the procedure
          ;; again with this pair removed from sp-pair-list to give
          ;; chance to other pairs sharing a common suffix (for
          ;; example \[ and [)
          (let ((new-sp-pair-list (--remove (equal (car it) open-pair) sp-pair-list)))
            (when (> (length sp-pair-list) (length new-sp-pair-list))
              (let ((sp-pair-list new-sp-pair-list))
                (sp-insert-pair))))
        ;; setup the delayed insertion here.
        (if (sp-get-pair open-pair :when-cond)
            (progn
              (setq sp-delayed-pair (cons open-pair (- (point) (length open-pair))))
              (setq sp-last-operation 'sp-insert-pair-delayed))
          (unless pair (delete-char (- (length trig))))
          (insert open-pair)
          (sp--run-hook-with-args open-pair :pre-handlers 'insert)
          (insert close-pair)
          (backward-char (length close-pair))
          (sp--pair-overlay-create (- (point) (length open-pair))
                                   (+ (point) (length close-pair))
                                   open-pair)

          ;; we only autoescape if the pair is a single character string
          ;; delimiter.  More elaborate pairs are probably already
          ;; escaped.  We leave the responsibility to the user, since
          ;; it's not that common and the usecases might vary -> there's
          ;; no good "default" case.
          (when (and sp-autoescape-string-quote
                     sp-point-inside-string
                     (or
                      (and (equal open-pair "\"") (equal close-pair "\"")
                           (eq sp-point-inside-string ?\"))
                      (and (equal open-pair "'") (equal close-pair "'")
                           (eq sp-point-inside-string ?')))
                     (or (not (memq major-mode sp-autoescape-string-quote-if-empty))
                         ;; Test if the string is empty here, by which
                         ;; we mean the point is surrounded by the
                         ;; string delimiters.  This enables us to
                         ;; write e.g. """""" in python docs.
                         (cl-labels ((check-quote (delimiter)
                                                  (and (equal (char-after (1+ (point))) delimiter)
                                                       (equal (char-before (1- (point))) delimiter))))
                           (not (or (check-quote ?\")
                                    (check-quote ?'))))))
            (save-excursion
              (backward-char 1)
              (insert sp-escape-char)
              (forward-char 1)
              (insert sp-escape-char))
            (overlay-put (sp--get-active-overlay 'pair) 'pair-id "\\\""))

          (when sp-undo-pairs-separately
            (sp--split-last-insertion-undo (+ (length open-pair) (length close-pair)))
            ;; TODO: abc\{abc\} undo undo \{asd\} . next undo removes the
            ;; entire \{asd\} if we do not insert two nils here.
            ;; Normally, repeated nils are ignored so it shouldn't
            ;; matter.  It would still be useful to inspect further.
            (push nil buffer-undo-list)
            (push nil buffer-undo-list))
          (sp--run-hook-with-args open-pair :post-handlers 'insert)
          (setq sp-last-inserted-pair open-pair)
          (setq sp-recent-keys nil)
          (setq sp-last-operation 'sp-insert-pair))))))

(defun sp--wrap-repeat-last (active-pair)
  "If the last operation was a wrap and `sp-wrap-repeat-last' is
non-nil, repeat the wrapping with this pair around the last
active region."
  (unless (= 0 sp-wrap-repeat-last)
    (when sp-last-wrapped-region
      (let* ((b (sp-get sp-last-wrapped-region :beg))
             (e (sp-get sp-last-wrapped-region :end))
             (op (sp-get sp-last-wrapped-region :op))
             (oplen (length op))
             (cllen (sp-get sp-last-wrapped-region :cl-l))
             (acolen (length (car active-pair))))
        (when (and
               (cond
                ((= 1 sp-wrap-repeat-last)
                 (equal (car active-pair) op))
                ((= 2 sp-wrap-repeat-last) t))
               (memq sp-last-operation '(sp-self-insert sp-wrap-region))
               (or (= (point) (+ b oplen acolen))
                   (= (point) e)))
          ;; TODO: this does not handle escaping of "double quote", that
          ;; is if we repeat quote wrap after quote wrap.  I think it is
          ;; reasonable to assume this will never happen, or very very
          ;; rarely. (same goes for option 2)
          (delete-char (- acolen))
          (if (< (point) e)
              (progn (goto-char (+ b oplen))
                     (insert (car active-pair))
                     (goto-char (- e cllen))
                     (insert (cdr active-pair))
                     (setq sp-last-wrapped-region
                           (sp--get-last-wraped-region
                            (+ b oplen) (point)
                            (car active-pair) (cdr active-pair)))
                     (goto-char (+ b oplen acolen)))
            (goto-char b)
            (insert (car active-pair))
            (goto-char e)
            (insert (cdr active-pair))
            (setq sp-last-wrapped-region
                  (sp--get-last-wraped-region
                   b e (car active-pair) (cdr active-pair))))
          (setq sp-last-operation 'sp-wrap-region)
          (sp--run-hook-with-args (car active-pair) :post-handlers 'wrap)
          sp-last-operation)))))

(defun sp--char-is-part-of-stringlike (char)
  "Return non-nil if CHAR is part of a string-like delimiter of length 1."
  (->> (sp--get-stringlike-list)
    (--filter (= 1 (length (cdr it))))
    (-map 'car)
    (--any? (string-match-p (regexp-quote char) it))))

(defun sp--char-is-part-of-closing (char)
  "Return non-nil if CHAR is part of a pair delimiter of length 1."
  (->> (sp--get-pair-list)
    (--filter (= 1 (length (cdr it))))
    (-map 'cdr)
    (--any? (string-match-p (regexp-quote char) it))))

;; TODO: this only supports single-char delimiters.  Maybe it should
;; that that way.
(defun sp-skip-closing-pair (&optional last test-only)
  "Automatically skip the closing delimiters of pairs.

If point is inside an inserted pair, and the user only moved
forward with point (that is, only inserted text), if the closing
pair is typed, we shouldn't insert it again but skip forward.  We
call this state \"active sexp\".  The setting
`sp-cancel-autoskip-on-backward-movement' controls when an active
expression become inactive.

For example, pressing ( is followed by inserting the pair (|).  If
we then type 'word' and follow by ), the result should be (word)|
instead of (word)|).

This behaviour can be customized by various settings of
`sp-autoskip-closing-pair' and `sp-autoskip-opening-pair'.

Additionally, this behaviour can be selectively disabled for
specific pairs by removing their \"autoskip\" action.  You can
achieve this by using `sp-pair' or `sp-local-pair' with
\":actions '(:rem autoskip)\"."
  (when (or (and (eq sp-autoskip-closing-pair t)
                 sp-pair-overlay-list
                 (sp--get-active-overlay 'pair))
            (memq sp-autoskip-closing-pair '(always always-end)))
    ;; these two are pretty hackish ~_~
    (cl-labels ((get-sexp
                 ()
                 (delete-char -1)
                 (insert " ")
                 (prog1 (sp-get-sexp)
                   (delete-char -1)
                   (insert last)))
                (get-enclosing-sexp
                 ()
                 (delete-char -1)
                 (insert " ")
                 (prog1 (sp-get-enclosing-sexp)
                   (delete-char -1)
                   (insert last))))
      (let ((last (or last (sp--single-key-description last-command-event))))
        (-when-let (active-sexp
                    (cond
                     ((-when-let* ((ov (sp--get-active-overlay 'pair))
                                   (op (overlay-get ov 'pair-id))
                                   (cl (cdr (assoc op sp-pair-list))))
                        ;; if the sexp is active, we are inside it.
                        (when (and (= 1 (length op))
                                   (equal last cl))
                          (list :beg (overlay-start ov)
                                :end (overlay-end ov)
                                :op op
                                :cl cl
                                :prefix ""
                                :suffix ""))))
                     ((sp--char-is-part-of-stringlike last)
                      ;; a part of closing delimiter is typed. There are four
                      ;; options now:
                      ;; - we are inside the sexp, at its end
                      ;; - we are inside the sexp, somewhere in the middle
                      ;; - we are outside, in front of a sexp
                      ;; - we are outside, somewhere between sexps
                      (cond
                       ((and (sp--looking-at (sp--get-stringlike-regexp))
                             (not (sp--skip-match-p (match-string-no-properties 0)
                                                    (match-beginning 0)
                                                    (match-end 0))))
                        ;; if we're looking at the delimiter, and it is valid in
                        ;; current context, get the sexp.
                        (get-sexp))
                       ;; here comes the feature when we're somewhere in the
                       ;; middle of the sexp (or outside), if ever supported.
                       ))
                     ((sp--char-is-part-of-closing last)
                      (cond
                       ((and (sp--looking-at (sp--get-closing-regexp))
                             (not (sp--skip-match-p (match-string-no-properties 0)
                                                    (match-beginning 0)
                                                    (match-end 0))))
                        (get-sexp))
                       ;; here comes the feature when we're somewhere in the
                       ;; middle of the sexp (or outside), if ever supported.
                       ((eq sp-autoskip-closing-pair 'always)
                        (get-enclosing-sexp))))))
          (when (and active-sexp
                     (equal (sp-get active-sexp :cl) last)
                     (sp--do-action-p (sp-get active-sexp :op) 'autoskip))
            (-when-let (re (cond
                            ((= (point) (sp-get active-sexp :beg))
                             ;; we are in front of a string-like sexp
                             (when sp-autoskip-opening-pair
                               (if test-only t
                                 (delete-char -1)
                                 (forward-char)
                                 (setq sp-last-operation 'sp-skip-closing-pair))))
                            ((= (point) (sp-get active-sexp :end-in))
                             (if test-only t
                               (delete-char 1)
                               (setq sp-last-operation 'sp-skip-closing-pair)))
                            ((sp-get active-sexp
                               (and (> (point) :beg-in)
                                    (< (point) :end-in)))
                             (if test-only t
                               (delete-char -1)
                               (sp-up-sexp)))))
              (unless (or test-only
                          sp-buffer-modified-p)
                (set-buffer-modified-p nil))
              re)))))))

(defun sp-delete-pair (&optional arg)
  "Automatically delete opening or closing pair, or both, depending on
position of point.

If the point is inside an empty pair, automatically delete both.  That
is, [(|) turns to [|, [\{|\} turns to [|.  Can be disabled by setting
`sp-autodelete-pair' to nil.

If the point is behind a closing pair or behind an opening pair delete
it as a whole.  That is, \{\}| turns to \{|, \{| turns to |.  Can be
disabled by setting `sp-autodelete-closing-pair' and
`sp-autodelete-opening-pair' to nil.

If the last operation was a wrap and `sp-autodelete-wrap' is
enabled, invoking this function will unwrap the expression, that
is remove the just added wrapping."
  ;; NOTE: Only use delete-char inside this function, so we
  ;; don't activate the advice recursively!

  ;; only activate if argument is 1 (this is 0-th argument of the
  ;; delete-backward-char), otherwise the user wants to delete
  ;; multiple character, so let him do that
  (when (and (= arg 1)
             smartparens-mode)
    (if (and sp-autodelete-wrap
             (eq sp-last-operation 'sp-wrap-region))
        (let ((p (point))
              (b (sp-get sp-last-wrapped-region :beg))
              (e (sp-get sp-last-wrapped-region :end))
              (o (sp-get sp-last-wrapped-region :op-l))
              (c (sp-get sp-last-wrapped-region :cl-l)))
          ;; if the last operation was `sp-wrap-region', and we are at
          ;; the position of either opening or closing pair, delete the
          ;; just-inserted pair
          (when (or (= p (+ b o))
                    (= p e))
            (insert "x") ;dummy char to account for the regularly deleted one
            (save-excursion
              (goto-char e)
              (delete-char (- c))
              (goto-char b)
              (delete-char o))
            (setq sp-last-operation 'sp-delete-pair-wrap)))
      (let ((p (point))
            (inside-pair (--first (and (sp--looking-back (sp--strict-regexp-quote (car it)))
                                       (sp--looking-at (concat "[ \n\t]*" (sp--strict-regexp-quote (cdr it)))))
                                  sp-pair-list))
            (behind-pair (--first (sp--looking-back (sp--strict-regexp-quote (cdr it))) sp-pair-list))
            (opening-pair (--first (sp--looking-back (sp--strict-regexp-quote (car it))) sp-pair-list)))

        (cond
         ;; we're just before the closing quote of a string.  If there
         ;; is an opening or closing pair behind the point, remove
         ;; it.  This is only really relevant if the pair ends in the
         ;; same character as string quote.  We almost never want to
         ;; delete it as an autopair (it would "open up the string").
         ;; So, word\"|" and <backspace> should produce word\|" or
         ;; word|" (if \" is autopair) instead of word\|.
         ((and (sp-point-in-string)
               (not (sp-point-in-string (1+ p)))
               (sp-point-in-string (1- p))) ;; the string isn't empty
          (cond ;; oh, you ugly duplication :/
           ((and behind-pair sp-autodelete-closing-pair)
            (delete-char (- (1- (length (car behind-pair)))))
            (setq sp-last-operation 'sp-delete-pair-closing))
           ((and opening-pair sp-autodelete-opening-pair)
            (delete-char (- (1- (length (car opening-pair)))))
            (setq sp-last-operation 'sp-delete-pair-opening))))
         ;; we're inside a pair
         ((and inside-pair sp-autodelete-pair)
          (let* ((end (save-excursion
                        (search-forward (cdr inside-pair))))
                 (cs (sp--get-context p))
                 (ce (sp--get-context end)))
            (when (or (not (eq cs 'comment)) ;; a => b <=> ~a v b
                      (eq ce 'comment))
              (delete-char (- end p))
              (delete-char (- (1- (length (car inside-pair)))))
              (setq sp-last-operation 'sp-delete-pair))))
         ;; we're behind a closing pair
         ((and behind-pair sp-autodelete-closing-pair)
          (delete-char (- (1- (length (cdr behind-pair)))))
          (setq sp-last-operation 'sp-delete-pair-closing))
         ;; we're behind an opening pair and there's no closing pair
         ((and opening-pair sp-autodelete-opening-pair)
          (delete-char (- (1- (length (car opening-pair)))))
          (setq sp-last-operation 'sp-delete-pair-opening)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Navigation

(defun sp--looking-at (regexp)
  "Like `looking-at', but always case sensitive."
  (let ((case-fold-search nil))
    (looking-at regexp)))

(defun sp--looking-at-p (regexp)
  "Like `looking-at-p', but always case sensitive."
  (let ((case-fold-search nil))
    (looking-at-p regexp)))

(defun sp--looking-back (regexp &optional limit not-greedy)
  "Return non-nil if text before point matches regular expression REGEXP.

With optional argument LIMIT search only that many characters
backward.  If LIMIT is nil, default to `sp-max-pair-length-c'.

If optional argument NON-GREEDY is t search for any matching
sequence, not necessarily the longest possible."
  (setq limit (or limit sp-max-pair-length-c))
  (let ((case-fold-search nil)
        (from (max 1 (- (point) limit)))
        (to (point))
        (greedy (not not-greedy))
        has-match)
    (set-match-data '(0 0))
    (if greedy
        (save-excursion
          (goto-char from)
          (while (and (not has-match) (< (point) to))
            (sp--looking-at regexp)
            (if (= (match-end 0) to)
                (setq has-match t)
              (forward-char 1)))
          has-match)
      (save-excursion
        (not (null (search-backward-regexp (concat "\\(?:" regexp "\\)\\=") from t)))))))

(defun sp--looking-back-p (regexp &optional limit not-greedy)
  "Same as `sp--looking-back' but do not change the match data."
  (save-match-data
    (sp--looking-back regexp limit not-greedy)))

(defun sp--search-backward-regexp (regexp &optional bound noerror count)
  "Works just like `search-backward-regexp', but returns the
longest possible match.  That means that searching for
\"defun|fun\" backwards would return \"defun\" instead of
\"fun\", which would be matched first.

This is an internal function.  Only use this for searching for
pairs!"
  (setq count (or count 1))
  (let ((case-fold-search nil) r)
    (while (> count 0)
      (when (search-backward-regexp regexp bound noerror)
        (goto-char (match-end 0))
        (sp--looking-back regexp)
        (setq r (goto-char (match-beginning 0))))
      (setq count (1- count)))
    r))

(defun sp--search-forward-regexp (regexp &optional bound noerror count)
  "Just like `search-forward-regexp', but always case sensitive."
  (let ((case-fold-search nil))
    (search-forward-regexp regexp bound noerror count)))

(defun sp-get-quoted-string-bounds ()
  "If the point is inside a quoted string, return its bounds."
  (when (nth 3 (syntax-ppss))
    (let ((open (save-excursion
                  (while (and (not (bobp))
                              (nth 3 (syntax-ppss)))
                    (backward-char 1))
                  (point)))
          (close (save-excursion
                   (while (and (not (eobp))
                               (nth 3 (syntax-ppss)))
                     (forward-char 1))
                   (point))))
      (cons open close))))

;; TODO: the repeated conditions are ugly, refactor this!
(defun sp-get-comment-bounds ()
  "If the point is inside a comment, return its bounds."
  (when (or (sp-point-in-comment)
            (looking-at "[[:space:]]+\\s<"))
    (let ((open (save-excursion
                  (while (and (not (bobp))
                              (or (sp-point-in-comment)
                                  (save-excursion
                                    (backward-char 1)
                                    (looking-at "[[:space:]]+\\s<"))))
                    (backward-char 1))
                  (when (not (or (bobp)
                                 (or (sp-point-in-comment)
                                     (save-excursion
                                       (backward-char 1)
                                       (looking-at "[[:space:]]+\\s<")))))
                    (forward-char))
                  (point)))
          (close (save-excursion
                   (while (and (not (eobp))
                               (or (sp-point-in-comment)
                                   (looking-at "[[:space:]]+\\s<")))
                     (forward-char 1))
                   (when (not (or (eobp)
                                  (or (sp-point-in-comment)
                                      (looking-at "[[:space:]]+\\s<"))))
                     (backward-char 1))
                   (point))))
      (cons open close))))

(defun sp--get-string-or-comment-bounds ()
  "Get the bounds of string or comment the point is in."
  (or (sp-get-quoted-string-bounds)
      (sp-get-comment-bounds)))

(defmacro sp--search-and-save-match (search-fn pattern bound res beg end str)
  "Save the last match info."
  `(progn
     (setq ,res (funcall ,search-fn ,pattern ,bound t))
     (when ,res
       (setq ,beg (match-beginning 0))
       (setq ,end (match-end 0))
       (setq ,str (match-string 0)))))

(cl-defun sp--skip-match-p (ms mb me
                               &key
                               (global-skip (cdr (--first (memq major-mode (car it)) sp-navigate-skip-match)))
                               (pair-skip (sp-get-pair ms :skip-match)))
  "Return non-nil if this match should be skipped.

This function uses two tests, one specified in
`sp-navigate-skip-match' (this is global setting for all pairs in
given major mode) and by a function specified in :skip-match
property of the pair.

If you are calling this function in a heavy loop, you can supply
the test functions as keyword arguments to speed up the lookup."
  (save-match-data
    (or (when global-skip (funcall global-skip ms mb me))
        (when pair-skip (funcall pair-skip ms mb me)))))

(defmacro sp--valid-initial-delimiter-p (form)
  "Test the last match using `sp--skip-match-p'.  The form should
be a function call that sets the match data."
  (declare (debug (form)))
  `(and ,form
        (not (sp--skip-match-p
              (match-string 0)
              (match-beginning 0)
              (match-end 0)))))

(defun sp--elisp-skip-match (ms mb me)
  "Function used to test for escapes in lisp modes.

Non-nil return value means to skip the result."
  (and ms
       (> mb 1)
       (save-excursion
         (goto-char mb)
         (save-match-data
           (or (and (sp--looking-back "\\\\" 1 t)
                    ;; it might be a part of ?\\ token
                    (not (sp--looking-back "\\?\\\\\\\\" 3 t)))
               (and (not (sp-point-in-string-or-comment))
                    (sp--looking-back "\\?" 1 t) ;;TODO surely we can do better
                    (not (sp--looking-back "\\\\\\?" 2 t))
                    (not (sp--looking-back "\\s_\\?" 2 t))
                    (not (sp--looking-back "\\sw\\?" 2 t))))))))

(defun sp--backslash-skip-match (ms mb me)
  (and ms
       (save-excursion
         (goto-char mb)
         (sp--looking-back "\\\\" 1 t))))

;; TODO: since this function is used for all the navigation, we should
;; optimaze it a lot! Get some elisp profiler! Also, we should split
;; this into smaller functions (esp. the "first expression search"
;; business)
(defun sp-get-paired-expression (&optional back)
  "Find the nearest balanced pair expression after point.

The expressions considered are those delimited by pairs on
`sp-pair-list'."
  (save-excursion
    (let* ((search-fn (if (not back) 'sp--search-forward-regexp 'sp--search-backward-regexp))
           (global-skip-fn (cdr (--first (memq major-mode (car it)) sp-navigate-skip-match)))
           (pair-list (sp--get-allowed-pair-list))
           ;; TODO UGLY HACK!!!  When the situation is:
           ;; ..)|;; comment
           ;; the context the point gets is the comment.  But if we
           ;; are searching backward, that is incorrect, because in
           ;; that case we want the context of the closing pair.
           ;; Therefore, if the direction is backward, we need to move
           ;; one point backward, then test the comment/string thing,
           ;; then compute the correct bounds, and then restore the
           ;; point so the search will pick up the )
           (in-string-or-comment (if back
                                     (save-excursion
                                       (backward-char)
                                       (sp-point-in-string-or-comment))
                                   (sp-point-in-string-or-comment)))
           (string-bounds (and in-string-or-comment (sp--get-string-or-comment-bounds)))
           (fw-bound (if in-string-or-comment (cdr string-bounds) (point-max)))
           (bw-bound (if in-string-or-comment (car string-bounds) (point-min)))
           s e active-pair forward mb me ms r done
           possible-pairs possible-interfering-pairs possible-ops possible-cls)
      (while (not done)
        ;; search for the first opening pair.  Here, only consider tags
        ;; that are allowed in the current context.
        (sp--search-and-save-match search-fn
                                   (sp--get-allowed-regexp)
                                   (if back bw-bound fw-bound)
                                   r mb me ms)
        (unless (sp--skip-match-p ms mb me :global-skip global-skip-fn)
          (when (not (if (not back)
                         (sp-point-in-string-or-comment (1- (point)))
                       (sp-point-in-string-or-comment)))
            (setq in-string-or-comment nil))
          ;; if the point originally wasn't inside of a string or comment
          ;; but now is, jump out of the string/comment and only search
          ;; the code.  This ensures that the comments and strings are
          ;; skipped if we search inside code.
          (if (and (not in-string-or-comment)
                   (if (not back)
                       (sp-point-in-string-or-comment (1- (point)))
                     (sp-point-in-string-or-comment)))
              (-if-let (bounds (sp--get-string-or-comment-bounds))
                  (let ((jump-to (if back (1- (car bounds)) (1+ (cdr bounds)))))
                    (goto-char jump-to))
                (setq done t))
            (setq done t))))
      (when r
        (setq possible-pairs (--filter (or (equal ms (car it))
                                           (equal ms (cdr it)))
                                       pair-list))
        (setq possible-ops (-map 'car possible-pairs))
        (setq possible-cls (-map 'cdr possible-pairs))
        (setq pair-list (-difference pair-list possible-pairs))
        (setq possible-interfering-pairs pair-list)
        (while possible-interfering-pairs
          (setq possible-interfering-pairs
                (--filter (or (-contains? possible-ops (car it))
                              (-contains? possible-cls (cdr it)))
                          pair-list))
          (setq pair-list (-difference pair-list possible-interfering-pairs))
          (setq possible-ops (append possible-ops (-map 'car possible-interfering-pairs)))
          (setq possible-cls (append possible-cls (-map 'cdr possible-interfering-pairs))))
        (when (--any? (equal ms it) possible-ops)
          (setq forward t)
          (setq s mb)
          (when back
            (forward-char (length ms))))
        (when (--any? (equal ms it) possible-cls)
          (setq forward nil)
          (setq e me)
          (when (not back)
            (backward-char (length ms))))
        (let* ((opens (if forward possible-ops possible-cls))
               (closes (if forward possible-cls possible-ops))
               (needle (sp--strict-regexp-opt (append possible-ops possible-cls)))
               (search-fn (if forward 'sp--search-forward-regexp 'sp--search-backward-regexp))
               (depth 1)
               (eof (if forward 'eobp 'bobp))
               (b (if forward fw-bound bw-bound))
               (open (substring-no-properties ms))
               (close (substring-no-properties ms))
               (failure (funcall eof))
               (skip-match-pair-fns (->> possible-ops
                                      (--mapcat (-when-let (smf (sp-get-pair it :skip-match))
                                                  (list (cons it smf) (cons (sp-get-pair it :close) smf)))))))
          (while (and (> depth 0) (not (funcall eof)))
            (sp--search-and-save-match search-fn needle b r mb me ms)
            (if r
                (unless (or (and (not in-string-or-comment)
                                 (if forward (save-excursion
                                               (backward-char)
                                               (sp-point-in-string-or-comment))
                                   (sp-point-in-string-or-comment)))
                            ;; check the individual pair skipper.  We
                            ;; need to test all the possible-ops,
                            ;; which makes it a bit ugly :/
                            (let ((skip-match-pair-fn
                                   (cdr (--first (equal (car it) ms) skip-match-pair-fns))))
                              (sp--skip-match-p ms mb me :global-skip global-skip-fn :pair-skip skip-match-pair-fn)))
                  (when (--any? (equal ms it) opens) (setq depth (1+ depth)))
                  (when (--any? (equal ms it) closes) (setq depth (1- depth))))
              (unless (minibufferp)
                (sp-message :unmatched-expression))
              (setq depth -1)
              (setq failure t)))
          (if forward
              (setq e me)
            (setq s mb))
          (setq close (substring-no-properties ms))
          (if (or failure
                  (/= depth 0))
              (progn
                (unless (minibufferp)
                  (sp-message :unmatched-expression))
                nil)
            (let ((end-in-cos (sp-point-in-string-or-comment (1- e)))) ;; fix the "point on comment" issue
              (cond
               ((or (and (sp-point-in-string-or-comment s) (not end-in-cos))
                    (and (not (sp-point-in-string-or-comment s)) end-in-cos))
                (unless (minibufferp)
                  (sp-message :delimiter-in-string))
                nil)
               (t
                (let* ((op (if forward open close)))
                  (list :beg s
                        :end e
                        :op op
                        :cl (if forward close open)
                        :prefix (sp--get-prefix s op)
                        :suffix (sp--get-suffix e op))))))))))))

;; TODO: this does not consider unbalanced quotes in comments!!!
(defun sp--find-next-stringlike-delimiter (needle search-fn-f &optional limit skip-fn)
  "Find the next string-like delimiter, considering the escapes
and the skip-match predicate."
  (let (hit match)
    (while (and (not hit)
                (funcall search-fn-f needle limit t))
      (save-match-data
        (setq match (match-string-no-properties 0))
        (unless (or (save-match-data
                      (save-excursion
                        (goto-char (match-beginning 0))
                        (or (looking-back "\\\\") ;; assumes \ is always the escape... bad?
                            (and (eq major-mode 'emacs-lisp-mode)
                                 (not (sp-point-in-string))
                                 (looking-back "?")))))
                    ;; TODO: HACK: global-skip is hack here!!!
                    (sp--skip-match-p match (match-beginning 0) (match-end 0) :pair-skip skip-fn :global-skip nil))
          (setq hit t))))
    hit))

(defun sp-get-stringlike-expression (&optional back)
  "Find the nearest string-like expression after point.

String-like expression is expression enclosed with the same
opening and closing delimiter, such as *...*, \"...\", `...` etc."
  (save-excursion
    (let ((needle (sp--get-stringlike-regexp))
          (search-fn-f (if (not back) 'sp--search-forward-regexp 'sp--search-backward-regexp))
          (search-fn-b (if back 'sp--search-forward-regexp 'sp--search-backward-regexp))
          (count 0)
          m b e skip-match-fn limit ok)
      (when (not (equal needle ""))
        (when (sp--find-next-stringlike-delimiter needle search-fn-f)
          ;; assumes \ is always the escape... bad?
          (setq m (match-string-no-properties 0))
          (setq needle (regexp-quote m))
          (setq skip-match-fn (sp-get-pair m :skip-match))
          (cond
           ((sp-point-in-string)
            (setq limit (sp-get-quoted-string-bounds)))
           ((sp-point-in-comment)
            (setq limit (sp-get-comment-bounds))))
          (save-excursion
            (while (sp--find-next-stringlike-delimiter needle 'search-backward-regexp (car limit) skip-match-fn)
              (setq count (1+ count))))
          (when (= (mod count 2) 0)
            (sp--find-next-stringlike-delimiter needle search-fn-b nil))
          (save-excursion
            (setq ok (sp--find-next-stringlike-delimiter needle 'sp--search-backward-regexp (car limit)))
            (setq e (match-beginning 0)))
          (setq ok (and ok (sp--find-next-stringlike-delimiter needle 'search-forward-regexp (cdr limit))))
          (setq b (match-end 0))
          (when ok
            (let ((mb b) (me e))
              (setq b (min mb me))
              (setq e (max mb me)))
            (list :beg b :end e :op m :cl m :prefix (sp--get-prefix b m) :suffix (sp--get-suffix e m))))))))

(defun sp-get-expression (&optional back)
  "Find the nearest balanced expression of any kind.

See also: `sp-navigate-consider-stringlike-sexp'."
  (if (memq major-mode sp-navigate-consider-stringlike-sexp)
      (let ((pre (sp--get-allowed-regexp))
            (sre (sp--get-stringlike-regexp))
            (search-fn (if (not back) 'sp--search-forward-regexp 'sp--search-backward-regexp))
            (ps (if back (1- (point-min)) (1+ (point-max))))
            (ss (if back (1- (point-min)) (1+ (point-max)))))
        (setq ps (or (save-excursion (funcall search-fn pre nil t)) ps))
        (setq ss (or (save-excursion (funcall search-fn sre nil t)) ss))
        (if (or (and (not back) (< ps ss))
                (and back (> ps ss)))
            (sp-get-paired-expression back)
          ;; performance hack. If the delimiter is a character in
          ;; syntax class 34, grab the string-like expression using
          ;; `sp-get-string'
          (if (and (= (length (match-string 0)) 1)
                   (eq (char-syntax (string-to-char (match-string 0))) 34))
              (sp-get-string back)
            (sp-get-stringlike-expression back))))
    (sp-get-paired-expression back)))

(defun sp-get-sexp (&optional back)
  "Find the nearest balanced expression that is after (before) point.

Search backward if BACK is non-nil.  This also means, if the
point is inside an expression, this expression is returned.

If `major-mode' is member of `sp-navigate-consider-sgml-tags',
sgml tags will also be considered as sexps in current buffer.  If
`major-mode' is member of `sp-navigate-consider-stringlike-sexp',
expressions where the opening and closing delimiters are the same
will also be considered as sexps.

If the search starts outside a comment, all subsequent comments
are skipped.

If the search starts inside a string or comment, it tries to find
the first balanced expression that is completely contained inside
the string or comment.  If no such expression exist, a warning is
raised (for example, when you comment out imbalanced expression).
However, if you start a search from within a string and the next
complete sexp lies completely outside, this is returned.  Note
that this only works in modes where strings and comments are
properly defined via the syntax tables.

The return value is a plist with following keys:

  :beg    - point in the buffer before the opening
  delimiter (ignoring prefix)
  :end    - point in the buffer after the closing delimiter
  :op     - opening delimiter
  :cl     - closing delimiter
  :prefix - expression prefix

However, you should never access this structure directly as it is
subject to change.  Instead, use the macro `sp-get' which also
provide shortcuts for many commonly used queries (such as length
of opening/closing delimiter or prefix)."
  (sp--maybe-init)
  (cond
   (sp-prefix-tag-object
    (sp-get-sgml-tag back))
   (sp-prefix-pair-object
    (sp-get-paired-expression back))
   ((memq major-mode sp-navigate-consider-sgml-tags)
    (let ((paired (sp-get-expression back)))
      (if (and paired
               (equal "<" (sp-get paired :op)))
          ;; if the point is inside the tag delimiter, return the pair.
          (if (sp-get paired (and (<= :beg-in (point)) (>= :end-in (point))))
              paired
            ;; if the tag can't be completed, we can at least return
            ;; the <> pair
            (or (sp-get-sgml-tag back) paired))
        ;; we can still try the tag if the first < or > is closer than
        ;; the pair.  This is a bit too complicated... seems like a
        ;; more clever solution would be needed in the future, esp if
        ;; we add the python hack.
        (cond
         ((and (not back)
               (< (save-excursion
                    (or (search-forward "<" nil t) (point-max)))
                  (or (sp-get paired :beg) (point-max))))
          (or (sp-get-sgml-tag) paired))
         ((and back
               (> (save-excursion
                    (or (search-backward ">" nil t) (point-min)))
                  (or (sp-get paired :end) (point-max))))
          (or (sp-get-sgml-tag t) paired))
         (t paired)))))
   (t (sp-get-expression back))))

(defun sp--get-hybrid-sexp-beg ()
  "Get the beginning of hybrid sexp.
See `sp-get-hybrid-sexp' for definition."
  (save-excursion
    (cl-labels ((indent-or-beg-of-line
                 (lb)
                 (if (sp-point-in-blank-line)
                     lb
                   (back-to-indentation)
                   (point))))
      (let ((p (progn (when (sp-point-in-symbol) (sp-backward-sexp)) (point)))
            (lb (line-beginning-position))
            (cur (--if-let (save-excursion (sp-backward-sexp)) it (list :end 0))) ;hack
            last)
        (if (< (sp-get cur :end) lb)
            ;; if the line is not empty, we move the beg to the indent
            (indent-or-beg-of-line lb)
          (while (sp-get cur
                   (and cur
                        (> :end lb)
                        (<= :end p)))
            (setq last cur)
            (setq cur (sp-backward-sexp)))
          (if last
              (sp-get last :beg-prf)
            ;; happens when there is no sexp before the opening delim of
            ;; the enclosing sexp.  In case it is on line above, we take
            ;; the maximum wrt lb.
            (sp-get cur (max :beg-in (indent-or-beg-of-line lb)))))))))

(defun sp--narrow-to-line ()
  "Narrow to the current line."
  (narrow-to-region (line-beginning-position) (line-end-position)))

(defun sp--get-hybrid-sexp-end ()
  "Get the end of hybrid sexp.
See `sp-get-hybrid-sexp' for definition."
  (save-excursion
    (cl-labels ((skip-prefix-backward
                 (p)
                 (save-excursion
                   (goto-char p)
                   (save-restriction
                     (sp--narrow-to-line)
                     (skip-syntax-backward " .")
                     (point)))))
      (let ((p (progn (when (sp-point-in-symbol) (sp-backward-sexp)) (point)))
            (le (line-end-position))
            (cur (--if-let (save-excursion (sp-forward-sexp)) it (list :beg (1+ (point-max))))) ;hack
            last)
        (if (> (sp-get cur :beg) le)
            (if (sp-point-in-blank-line) le (skip-prefix-backward le))
          (while (sp-get cur
                   (and cur
                        (< :beg le)
                        (>= :beg p)))
            (setq last cur)
            (setq cur (sp-forward-sexp)))
          (let ((r (skip-prefix-backward
                    (if last
                        (sp-get last :end)
                      ;; happens when there is no sexp before the closing delim of
                      ;; the enclosing sexp.  In case it is on line below, we take
                      ;; the minimum wrt le.
                      (sp-get cur (min :end-in le))))))
            (goto-char r)
            ;; fix the situation when point ends in comment
            (cond
             ((sp-point-in-comment)
              (if (= (line-number-at-pos p)
                     (line-number-at-pos r))
                  (line-end-position)
                (goto-char p)
                (line-end-position)))
             (t r))))))))

(defun sp--get-hybrid-suffix (p)
  "Get the hybrid sexp suffix, which is any punctuation after
the end, possibly preceeded by whitespace."
  (save-excursion
    (goto-char p)
    (buffer-substring-no-properties
     p
     (save-restriction
       (sp--narrow-to-line)
       (skip-syntax-forward " ")
       (if (not (looking-at "\\s."))
           p
         (skip-syntax-forward ".")
         (point))))))

(defun sp-get-hybrid-sexp ()
  "Return the hybrid sexp around point.

A hybrid sexp is defined as the smallest balanced region containing
the point while not expanding further than the current line.  That is,
any hanging sexps will be included, but the expansion stops at the
enclosing list boundaries or line boundaries."
  (let ((end (sp--get-hybrid-sexp-end)))
    (list :beg (sp--get-hybrid-sexp-beg)
          :end end
          :op ""
          :cl ""
          :prefix ""
          :suffix (sp--get-hybrid-suffix end))))

(defun sp-get-enclosing-sexp (&optional arg)
  "Return the balanced expression that wraps point at the same level.

With ARG, ascend that many times.  This funciton expect positive
argument."
  (setq arg (or arg 1))
  (save-excursion
    (let ((n arg)
          (ok t)
          (okr))
      (while (and (> n 0) ok)
        (setq ok t)
        (setq okr nil)
        ;; if we are inside string, get the string bounds and "string
        ;; expression"
        (when (sp-point-in-string)
          (setq okr (sp-get-string)))
        ;; get the "normal" expression defined by pairs
        (let ((p (point)))
          (setq ok (sp-get-sexp))
          (cond
           ((and ok (= (sp-get ok :beg) p))
            (goto-char (sp-get ok :end))
            (setq n (1+ n)))
           ((and ok (< (sp-get ok :beg) p))
            (goto-char (sp-get ok :end)))
           (t
            (while (and ok (>= (sp-get ok :beg) p))
              (setq ok (sp-get-sexp))
              (when ok (goto-char (sp-get ok :end)))))))
        ;; if the pair expression is completely enclosed inside a
        ;; string, return the pair expression, otherwise return the
        ;; string expression
        (when okr
          (unless (and ok
                       (sp-compare-sexps ok okr >)
                       (sp-compare-sexps ok okr < :end))
            (setq ok okr)
            (goto-char (sp-get ok :end))))
        (setq n (1- n)))
      (if (not (and (not ok)
                    sp-navigate-comments-as-sexps))
          ok
        (when (sp-point-in-comment)
          (let* ((cb (sp-get-comment-bounds))
                 (b (save-excursion
                      (goto-char (car cb))
                      (sp-skip-backward-to-symbol t)
                      (point)))
                 (e (save-excursion
                      (goto-char (cdr cb))
                      (sp-skip-forward-to-symbol t)
                      (point))))
            (list :beg b :end e :op "" :cl "" :prefix sp-comment-char)))))))

(defun sp-get-list-items (&optional lst)
  "Return the information about expressions inside LST.

LST should be a data structure in format as returned by
`sp-get-sexp'.

The return value is a list of such structures in order as they
occur inside LST describing each expression, with LST itself
prepended to the front.

If LST is nil, the list at point is used (that is the list
following point after `sp-backward-up-sexp' is called)."
  (let ((r nil))
    (save-excursion
      (unless lst
        (setq lst (sp-backward-up-sexp)))
      (when lst
        (goto-char (sp-get lst :beg-in))
        (while (< (point) (sp-get lst :end))
          (!cons (sp-forward-sexp) r))
        (cons lst (nreverse (cdr r)))))))

(cl-defun sp--get-prefix (&optional (p (point)) op)
  "Get the prefix of EXPR.

Prefix is any continuous sequence of characters in \"expression
prefix\" syntax class.  You can also specify a set of syntax code
characters or a regexp for a specific major mode.  See
`sp-sexp-prefix'.

If the prefix property is defined for OP, the associated regexp
is used to retrieve the prefix instead of the global setting."
  (let ((pref (sp-get-pair op :prefix)))
    (save-excursion
      (goto-char p)
      (if pref
          (when (sp--looking-back pref)
            (match-string-no-properties 0))
        (-if-let (mmode-prefix (cdr (assoc major-mode sp-sexp-prefix)))
            (cond
             ((eq (car mmode-prefix) 'regexp)
              (sp--looking-back (cadr mmode-prefix))
              (match-string-no-properties 0))
             ((eq (car mmode-prefix) 'syntax)
              (skip-syntax-backward (cadr mmode-prefix))
              (buffer-substring-no-properties (point) p)))
          (skip-syntax-backward "'")
          (buffer-substring-no-properties (point) p))))))

(cl-defun sp--get-suffix (&optional (p (point)) op)
  "Get the suffix of EXPR.

Prefix is any continuous sequence of characters in \"punctuation
prefix\" syntax class.  You can also specify a set of syntax code
characters or a regexp for a specific major mode.  See
`sp-sexp-suffix'.

If the prefix property is defined for OP, the associated regexp
is used to retrieve the prefix instead of the global setting."
  (let ((suff (sp-get-pair op :suffix)))
    (save-excursion
      (goto-char p)
      (if suff
          (when (sp--looking-at suff)
            (match-string-no-properties 0))
        (-if-let (mmode-suffix (cdr (assoc major-mode sp-sexp-suffix)))
            (cond
             ((eq (car mmode-suffix) 'regexp)
              (sp--looking-at (cadr mmode-suffix))
              (match-string-no-properties 0))
             ((eq (car mmode-suffix) 'syntax)
              (skip-syntax-forward (cadr mmode-suffix))
              (buffer-substring-no-properties p (point))))
          (skip-syntax-forward ".")
          (buffer-substring-no-properties p (point)))))))

(defun sp-get-symbol (&optional back)
  "Find the nearest symbol that is after point, or before point if BACK is non-nil.

This also means, if the point is inside a symbol, this symbol is
returned.  Symbol is defined as a chunk of text recognized by
`sp-forward-symbol'.

The return value is a plist with the same format as the value
returned by `sp-get-sexp'."
  (sp--maybe-init)
  (let (b e prefix last-or-first)
    (save-excursion
      (if back
          (progn
            (sp-skip-backward-to-symbol)
            (when (= (point) (point-min)) (setq last-or-first t))
            (sp-forward-symbol -1)
            (setq b (point))
            (sp-forward-symbol 1)
            (setq e (point)))
        (sp-skip-forward-to-symbol)
        (when (= (point) (point-max)) (setq last-or-first t))
        (sp-forward-symbol 1)
        (setq e (point))
        (sp-forward-symbol -1)
        (setq b (point))))
    (unless last-or-first
      (list :beg b :end e :op "" :cl "" :prefix (sp--get-prefix b) :suffix (sp--get-suffix e)))))

;; this +/- 1 nonsense comes from sp-get-quoted-string-bounds. That
;; should go to hell after the parser rewrite
(defun sp--get-string (bounds)
  "Return the `sp-get-sexp' format info about the string.

This function simply transforms BOUNDS, which is a cons (BEG
. END) into format compatible with `sp-get-sexp'."
  (let* ((bob (= (point-min) (car bounds)))
         (eob (= (point-max) (cdr bounds)))
         ;; if the closing and opening isn't the same token, we should
         ;; return nil
         (op (char-to-string (char-after (car bounds))))
         (cl (char-to-string (char-before (cdr bounds)))))
    (when (equal op cl)
      (list :beg (car bounds)
            :end (cdr bounds)
            :op cl
            :cl cl
            :prefix ""
            :suffix ""))))

(defun sp-get-string (&optional back)
  "Find the nearest string after point, or before if BACK is non-nil.

This also means if the point is inside a string, this string is
returned.  If there are another symbols between point and the
string, nil is returned.  That means that this funciton only
return non-nil if the string is the very next meaningful
expression.

The return value is a plist with the same format as the value
returned by `sp-get-sexp'."
  (sp--maybe-init)
  (if (sp-point-in-comment)
      (sp-get-stringlike-expression back)
    (if (sp-point-in-string)
        (let ((r (sp-get-quoted-string-bounds)))
          (sp--get-string r))
      (save-excursion
        (sp-skip-into-string back)
        (--when-let (sp-get-quoted-string-bounds)
          (sp--get-string it))))))

(defun sp-get-whitespace ()
  "Get the whitespace around point.

Whitespace here is defined as any of the characters: space, tab
and newline."
  (list :beg (save-excursion (skip-chars-backward " \t\n") (point))
        :end (save-excursion (skip-chars-forward " \t\n") (point))
        :op ""
        :cl ""
        :prefix ""
        :suffix ""))

(defun sp--sgml-get-tag-name (match)
  (let ((sub (if (equal "/" (substring match 1 2))
                 (substring match 2)
               (substring match 1))))
    (car (split-string sub "\\( \\|>\\)"))))

(defun sp--sgml-opening-p (tag)
  (not (equal "/" (substring tag 1 2))))

(defun sp--sgml-ignore-tag (tag)
  "Return non-nil if tag should be ignored in search, nil otherwise."
  (member tag '("!--" "!DOCTYPE")))

(defun sp-get-sgml-tag (&optional back)
  (sp--maybe-init)
  (save-excursion
    (let ((search-fn (if (not back) 'sp--search-forward-regexp 'search-backward-regexp))
          (case-fold-search nil)
          tag tag-name needle
          open-start open-end
          close-start close-end)
      (when (and (funcall search-fn "</?.*?\\s-?.*?>" nil t)
                 (progn
                   (setq tag (substring-no-properties (match-string 0)))
                   (setq tag-name (sp--sgml-get-tag-name tag))
                   (not (sp--sgml-ignore-tag tag-name))))
        (setq needle (concat "</?" tag-name))
        (let* ((forward (sp--sgml-opening-p tag))
               (search-fn (if forward 'sp--search-forward-regexp 'search-backward-regexp))
               (depth 1))
          (save-excursion
            (if (not back)
                (progn
                  (setq open-end (point))
                  (search-backward-regexp "<" nil t)
                  (setq open-start (point)))
              (setq open-start (point))
              (search-forward-regexp ">" nil t)
              (setq open-end (point))))
          (cond
           ((and (not back) (not forward))
            (goto-char (match-beginning 0)))
           ((and back forward)
            (goto-char (match-end 0))))
          (while (> depth 0)
            (if (funcall search-fn needle nil t)
                (if (sp--sgml-opening-p (match-string 0))
                    (if forward (setq depth (1+ depth)) (setq depth (1- depth)))
                  (if forward (setq depth (1- depth)) (setq depth (1+ depth))))
              (setq depth -1)))
          (if (eq depth -1)
              (progn (sp-message :no-matching-tag) nil)
            (save-excursion
              (if forward
                  (progn
                    (setq close-start (match-beginning 0))
                    (search-forward-regexp ">" nil t)
                    (setq close-end (point)))
                (setq close-start (point))
                (search-forward-regexp ">" nil t)
                (setq close-end (point))))
            (let ((op (buffer-substring-no-properties open-start open-end))
                  (cl (buffer-substring-no-properties close-start close-end)))
              (list :beg (if forward open-start close-start)
                    :end (if forward close-end open-end)
                    :op (if forward op cl)
                    :cl (if forward cl op)
                    :prefix ""
                    :suffix ""))))))))

(defun sp--end-delimiter-closure (pairs pair-list)
  "Compute the \"end-delimiter\" closure of set PAIRS.

PAIRS can be:
- single pair ID
- single cons with opening and closing delimiter
- list of pair IDs
- list of conses of opening and closing delimiters

For example, if we have pairs (if . end) and (def . end), then
the closure of \"if\" pair are both of these because they share
the closing delimiter.  Therefore, in the navigation functions,
both have to be considered by the parser."
  (let* ((pairs (-flatten (list pairs)))
         (pairs (if (consp (car pairs)) (-map 'car pairs) pairs))
         (pairs (--filter (member (car it) pairs) pair-list))
         (closure (-mapcat
                   (lambda (x)
                     (--filter (equal (cdr x) (cdr it)) pair-list))
                   pairs)))
    closure))

(defun sp-restrict-to-pairs (pairs function)
  "Call the FUNCTION restricted to PAIRS.

PAIRS is either an opening delimiter of a list of opening
delimiters.

FUNCTION is a function symbol.

For example, you can restrict function `sp-down-sexp' to the
pair (\"{\" . \"}\") for easier navigation of blocks in C-like
languages."
  (let* ((pairs (-flatten (list pairs)))
         (new-pairs (--filter (member (car it) pairs) sp-pair-list))
         (sp-pair-list (sp--end-delimiter-closure new-pairs sp-pair-list)))
    (call-interactively function)))

(defun sp-restrict-to-object (object function)
  "Call the FUNCTION restricted to OBJECT.

OBJECT is one of following symbols (you have to quote it!):
- `sp-prefix-pair-object'
- `sp-prefix-tag-object'
- `sp-prefix-symbol-object'

This function will enable this prefix and then call FUNCTION.

FUNCTION is a function symbol.

This function is equivalent to doing:

  (let ((sp-prefix-object t))
    (call-interactively function))

For example, you can restrict function `sp-forward-sexp' to just
the pairs for easier navigation of blocks in C-like languages."
  (cl-letf (((symbol-value object) t))
    (call-interactively function)))

;; TODO: add shorter alias?
(defun sp-restrict-to-pairs-interactive (pairs function)
  "Return an interactive lambda that calls FUNCTION restricted to PAIRS.

See `sp-restrict-to-pairs'.

This function implements a \"decorator pattern\", that is, you
can apply another scoping function to the output of this function
and the effects will added together. In particular, you can
combine it with:

- `sp-restrict-to-object-interactive'

You can also bind the output of this function directly to a key, like:

  (global-set-key (kbd ...) (sp-restrict-to-pairs-interactive \"{\" 'sp-down-sexp))

This will be a function that descends down only into { } pair,
ignoring all others."
  (lexical-let ((pairs pairs)
                (function function))
    (lambda (&optional arg)
      (interactive "P")
      (sp-restrict-to-pairs pairs function))))

(defun sp-restrict-to-object-interactive (object function)
  "Return an interactive lambda that calls FUNCTION restricted to OBJECT.

See `sp-restrict-to-object'.

This function implements a \"decorator pattern\", that is, you
can apply another scoping function to the output of this function
and the effects will added together. In particular, you can
combine it with:

- `sp-restrict-to-pairs-interactive'

You can also bind the output of this function directly to a key, like:

  (global-set-key (kbd ...) (sp-restrict-to-object-interactive
                             'sp-prefix-pair-object
                             'sp-forward-sexp))

This will be a function that navigates only by using paired
expressions, ignoring strings and sgml tags."
  (lexical-let ((object object)
                (function function))
    (lambda (&optional arg)
      (interactive "P")
      (sp-restrict-to-object object function))))

(defun sp-prefix-tag-object (&optional arg)
  "Read the command and invoke it on the next tag object.

If you specify a regular emacs prefix argument this is passed to
the executed command.  Therefore, executing
\"\\[universal-argument] 2 \\[sp-prefix-tag-object] \\[sp-forward-sexp]\" will move two tag
expressions forward, ignoring possible symbols or paired
expressions inbetween.

Tag object is anything delimited by sgml tag."
  (interactive "P")
  (let* ((cmd (read-key-sequence "" t))
         (com (key-binding cmd))
         (sp-prefix-tag-object t))
    (if (commandp com)
        (call-interactively com)
      (execute-kbd-macro cmd))))

(defun sp-prefix-pair-object (&optional arg)
  "Read the command and invoke it on the next pair object.

If you specify a regular emacs prefix argument this is passed to
the executed command.  Therefore, executing
\"\\[universal-argument] 2 \\[sp-prefix-pair-object] \\[sp-forward-sexp]\" will move two paired
expressions forward, ignoring possible symbols inbetween.

Pair object is anything delimited by pairs from `sp-pair-list'."
  (interactive "P")
  (let* ((cmd (read-key-sequence "" t))
         (com (key-binding cmd))
         (sp-prefix-pair-object t))
    (if (commandp com)
        (call-interactively com)
      (execute-kbd-macro cmd))))

(defun sp-prefix-symbol-object (&optional arg)
  "Read the command and invoke it on the next pair object.

If you specify a regular emacs prefix argument this is passed to
the executed command.  Therefore, executing
\"\\[universal-argument] 2 \\[sp-prefix-symbol-object] \\[sp-forward-sexp]\" will move two symbols
forward, ignoring any structure.

Symbol is defined as a chunk of text recognized by
`sp-forward-symbol'."
  (interactive "P")
  (let* ((cmd (read-key-sequence "" t))
         (com (key-binding cmd))
         (sp-prefix-symbol-object t))
    (if (commandp com)
        (call-interactively com)
      (execute-kbd-macro cmd))))

(defun sp-prefix-save-excursion (&optional arg)
  "Execute the command keeping the point fixed.

If you specify a regular emacs prefix argument this is passed to
the executed command."
  (interactive "P")
  (let* ((cmd (read-key-sequence "" t))
         (com (key-binding cmd)))
    (save-excursion
      (if (commandp com)
          (call-interactively com)
        (execute-kbd-macro cmd)))))

(defun sp-get-thing (&optional back)
  "Find next thing after point, or before if BACK is non-nil.

Thing is either symbol (`sp-get-symbol'),
string (`sp-get-string') or balanced expression recognized by
`sp-get-sexp'.

If `sp-navigate-consider-symbols' is nil, only balanced
expressions are considered."
  (sp--maybe-init)
  (cond
   (sp-prefix-tag-object (sp-get-sgml-tag back))
   (sp-prefix-pair-object (sp-get-paired-expression back))
   (sp-prefix-symbol-object (sp-get-symbol back))
   (t
    (if back
        (if (not sp-navigate-consider-symbols)
            (sp-get-sexp t)
          (save-excursion
            (cond
             ((sp-point-in-empty-string)
              (sp-get-string t))
             (t
              (sp-skip-backward-to-symbol t nil t)
              (cond
               ;; this is an optimization, we do not need to look up
               ;; the "pair" expression first. If this fails, follow
               ;; up with regular sexps
               ((and (memq major-mode sp-navigate-consider-sgml-tags)
                     (sp--looking-back ">")
                     (sp-get-sgml-tag t)))
               ((sp--valid-initial-delimiter-p (sp--looking-back (sp--get-closing-regexp (sp--get-allowed-pair-list)) nil))
                (sp-get-sexp t))
               ((sp--valid-initial-delimiter-p (sp--looking-back (sp--get-opening-regexp (sp--get-allowed-pair-list)) nil))
                (sp-get-sexp t))
               ((eq (char-syntax (preceding-char)) 34)
                (sp-get-string t))
               ((and (memq major-mode sp-navigate-consider-stringlike-sexp)
                     (sp--valid-initial-delimiter-p (sp--looking-back (sp--get-stringlike-regexp) nil))
                     (sp-get-stringlike-expression t)))
               (t (sp-get-symbol t)))))))
      (if (not sp-navigate-consider-symbols)
          (sp-get-sexp nil)
        (save-excursion
          (cond
           ((sp-point-in-empty-string)
            (sp-get-string nil))
           (t
            (sp-skip-forward-to-symbol t nil t)
            (cond
             ((and (memq major-mode sp-navigate-consider-sgml-tags)
                   (looking-at "<")
                   (sp-get-sgml-tag)))
             ((sp--valid-initial-delimiter-p (sp--looking-at (sp--get-opening-regexp (sp--get-allowed-pair-list))))
              (sp-get-sexp nil))
             ((sp--valid-initial-delimiter-p (sp--looking-at (sp--get-closing-regexp (sp--get-allowed-pair-list))))
              (sp-get-sexp nil))
             ((eq (char-syntax (following-char)) 34)
              (sp-get-string nil))
             ((and (memq major-mode sp-navigate-consider-stringlike-sexp)
                   (sp--valid-initial-delimiter-p (sp--looking-at (sp--get-stringlike-regexp)))
                   (sp-get-stringlike-expression nil)))
             ;; it can still be that we are looking at a /prefix/ of a
             ;; sexp.  We should skip a symbol forward and check if it
             ;; is a sexp, and then maybe readjust the output.
             (t (let* ((sym (sp-get-symbol nil))
                       (sym-string (and sym (sp-get sym (buffer-substring-no-properties :beg :end)))))
                  (when sym-string
                    (goto-char (sp-get sym :end))
                    (if (sp--looking-at (sp--get-opening-regexp))
                        (let* ((ms (match-string 0))
                               (pref (sp--get-prefix (point) ms)))
                          (if (and pref
                                   (string-prefix-p
                                    (sp--reverse-string sym-string)
                                    (sp--reverse-string pref)))
                              (sp-get-sexp nil)
                            sym))
                      sym)))))))))))))

(defun sp-narrow-to-sexp (arg)
  "Make text outside current balanced expression invisible.
A numeric arg specifies to move up by that many enclosing expressions.

See also `narrow-to-region' and `narrow-to-defun'."
  (interactive "p")
  (-when-let (enc (sp-get-enclosing-sexp arg))
    (sp-get enc (narrow-to-region :beg-prf :end))))

(defun sp-forward-sexp (&optional arg)
  "Move forward across one balanced expression.

With ARG, do it that many times.  Negative arg -N means move
backward across N balanced expressions.  If there is no forward
expression, jump out of the current one (effectively doing
`sp-up-sexp').

With `sp-navigate-consider-symbols' symbols and strings are also
considered balanced expressions.

Examples: (prefix arg in comment)

  |(foo bar baz)   -> (foo bar baz)|

  (|foo bar baz)   -> (foo| bar baz)

  (|foo bar baz)   -> (foo bar| baz) ;; 2

  (foo (bar baz|)) -> (foo (bar baz)|)"
  (interactive "p")
  (setq arg (or arg 1))
  (if (< arg 0)
      (sp-backward-sexp (- arg))
    (let* ((n arg)
           (ok t))
      (while (and ok (> n 0))
        (setq ok (sp-get-thing))
        (setq n (1- n))
        (when ok (goto-char (sp-get ok :end))))
      ok)))

(defun sp-backward-sexp (&optional arg)
  "Move backward across one balanced expression (sexp).

With ARG, do it that many times.  Negative arg -N means move
forward across N balanced expressions.  If there is no previous
expression, jump out of the current one (effectively doing
`sp-backward-up-sexp').

With `sp-navigate-consider-symbols' symbols and strings are also
considered balanced expressions.

Examples: (prefix arg in comment)

  (foo bar baz)|   -> |(foo bar baz)

  (foo| bar baz)   -> (|foo bar baz)

  (foo bar| baz)   -> (|foo bar baz) ;; 2

  (|(foo bar) baz) -> ((|foo bar) baz)"
  (interactive "p")
  (setq arg (or arg 1))
  (if (< arg 0)
      (sp-forward-sexp (- arg))
    (let* ((n arg)
           (ok t))
      (while (and ok (> n 0))
        (setq ok (sp-get-thing t))
        (setq n (1- n))
        (when ok (goto-char (sp-get ok :beg))))
      ok)))

(defun sp-next-sexp (&optional arg)
  "Move forward to the beginning of next balanced expression.

With ARG, do it that many times.  If there is no next expression
at current level, jump one level up (effectively doing
`sp-backward-up-sexp').  Negative arg -N means move to the
beginning of N-th previous balanced expression.

With `sp-navigate-consider-symbols' symbols and strings are also
considered balanced expressions.

Examples:

  ((foo) |bar (baz quux)) -> ((foo) bar |(baz quux))

  ((foo) bar |(baz quux)) -> |((foo) bar (baz quux))"
  (interactive "p")
  (setq arg (or arg 1))
  (if (> arg 0)
      (if (= arg 1)
          (-when-let (ok (sp-get-thing))
            (if (= (point) (sp-get ok :beg))
                (progn (sp-forward-sexp 2)
                       (sp-backward-sexp))
              (goto-char (sp-get ok :beg))
              ok))
        (sp-forward-sexp arg)
        (sp-backward-sexp))
    (sp-backward-sexp (- arg))))

(defun sp-previous-sexp (&optional arg)
  "Move backward to the end of previous balanced expression.

With ARG, do it that many times.  If there is no next
expression at current level, jump one level up (effectively
doing `sp-up-sexp').  Negative arg -N means move to the end of
N-th following balanced expression.

With `sp-navigate-consider-symbols' symbols and strings are also
considered balanced expressions.

Examples:

  ((foo) bar| (baz quux)) -> ((foo)| bar (baz quux))

  ((foo)| bar (baz quux)) -> ((foo) bar (baz quux))|"
  (interactive "p")
  (setq arg (or arg 1))
  (if (> arg 0)
      (if (= arg 1)
          (-when-let (ok (sp-get-thing t))
            (if (= (point) (sp-get ok :end))
                (progn (sp-backward-sexp 2)
                       (sp-forward-sexp))
              (goto-char (sp-get ok :end))
              ok))
        (sp-backward-sexp arg)
        (sp-forward-sexp))
    (sp-forward-sexp (- arg))))

(defun sp--raw-argument-p (arg)
  "Return t if ARG represents raw argument, that is a non-empty list."
  (and (listp arg) (car arg)))

(defun sp--negate-argument (arg)
  "Return the argument ARG but negated.

If the argument is a raw prefix argument (cons num nil) return a
list with its car negated.  If the argument is just the - symbol,
return 1.  If the argument is nil, return -1.  Otherwise negate
the input number."
  (cond
   ((sp--raw-argument-p arg) (list (- (car arg))))
   ((eq arg '-) 1)
   ((not arg) -1)
   (t (- arg))))

(defun sp-down-sexp (&optional arg)
  "Move forward down one level of sexp.

With ARG, do this that many times.  A negative argument -N means
move backward but still go down a level.

If ARG is raw prefix argument \\[universal-argument], descend forward as much as
possible.

If ARG is raw prefix argument \\[universal-argument] \\[universal-argument], jump to the beginning of
current list.

If the point is inside sexp and there is no down expression to
descend to, jump to the beginning of current one.  If moving
backwards, jump to end of current one.

Examples:

  |foo (bar (baz quux)) -> foo (|bar (baz quux))

  |foo (bar (baz quux)) -> foo (bar (|baz quux)) ;; 2

  |foo (bar (baz (quux) blab)) -> foo (bar (baz (|quux) blab)) ;; \\[universal-argument]

  (foo (bar baz) |quux) -> (|foo (bar baz) quux)

  (blab foo |(bar baz) quux) -> (|blab foo (bar baz) quux) ;; \\[universal-argument] \\[universal-argument]"
  (interactive "P")
  (let* ((raw (sp--raw-argument-p arg))
         (arg (prefix-numeric-value arg))
         (n (abs arg))
         (ok t)
         (last-point -1))
    (if (and raw (= (abs arg) 16))
        ;; jump to the beginning/end of current list
        (-when-let (enc (sp-get-enclosing-sexp))
          (if (> arg 0)
              (goto-char (sp-get enc :beg-in))
            (goto-char (sp-get enc :end-in)))
          (setq ok enc))
      ;; otherwise descend normally
      (while (and ok (> n 0))
        (setq ok (sp-get-sexp (< arg 0)))
        ;; if the prefix was C-u, we do not decrease n and instead set
        ;; it to -1 when (point) == "last ok"
        (if raw
            (when (= (point) last-point)
              (setq n -1))
          (setq n (1- n)))
        (when ok
          (setq last-point (point))
          (if (< arg 0)
              (goto-char (sp-get ok :end-in))
            (goto-char (sp-get ok :beg-in))))))
    ok))

(defun sp-backward-down-sexp (&optional arg)
  "Move backward down one level of sexp.

With ARG, do this that many times.  A negative argument -N means
move forward but still go down a level.

If ARG is raw prefix argument \\[universal-argument], descend backward as much as
possible.

If ARG is raw prefix argument \\[universal-argument] \\[universal-argument], jump to the end of current
list.

If the point is inside sexp and there is no down expression to
descend to, jump to the end of current one.  If moving forward,
jump to beginning of current one.

Examples:

  foo (bar (baz quux))| -> foo (bar (baz quux)|)

  (bar (baz quux)) foo| -> (bar (baz quux|)) foo ;; 2

  foo (bar (baz (quux) blab))| -> foo (bar (baz (quux|) blab)) ;; \\[universal-argument]

  (foo| (bar baz) quux) -> (foo (bar baz) quux|)

  (foo (bar baz) |quux blab) -> (foo (bar baz) quux blab|) ;; \\[universal-argument] \\[universal-argument]"
  (interactive "P")
  (sp-down-sexp (sp--negate-argument arg)))

(defun sp-beginning-of-sexp (&optional arg)
  "Jump to beginning of the sexp the point is in.

The beginning is the point after the opening delimiter.

With no argument, this is the same as calling
\\[universal-argument] \\[universal-argument] `sp-down-sexp'

With ARG positive N > 1, move forward out of the current
expression, move N-2 expressions forward and move down one level
into next expression.

With ARG negative -N < 1, move backward out of the current
expression, move N-1 expressions backward and move down one level
into next expression.

With ARG raw prefix argument \\[universal-argument] move out of the current expressions
and then to the beginning of enclosing expression.

Examples:

  (foo (bar baz) quux| (blab glob)) -> (|foo (bar baz) quux (blab glob))

  (foo (bar baz|) quux (blab glob)) -> (foo (|bar baz) quux (blab glob))

  (|foo) (bar) (baz quux) -> (foo) (bar) (|baz quux) ;; 3

  (foo bar) (baz) (quux|) -> (|foo bar) (baz) (quux) ;; -3

  ((foo bar) (baz |quux) blab) -> (|(foo bar) (baz quux) blab) ;; \\[universal-argument]"
  (interactive "P")
  (let* ((raw (sp--raw-argument-p arg))
         (arg (prefix-numeric-value arg))
         (re (cond
              ((and raw (= arg 4))
               (sp-up-sexp)
               (sp-beginning-of-sexp))
              ((= arg 1)
               (sp-down-sexp '(16)))
              ((< arg 0)
               (sp-backward-up-sexp)
               (sp-forward-sexp (1+ arg))
               (sp-down-sexp))
              ((> arg 0)
               (sp-up-sexp)
               (sp-forward-sexp (- arg 2))
               (sp-down-sexp)))))
    (sp--run-hook-with-args (sp-get re :op) :post-handlers 'beginning-of-sexp)
    re))

(defun sp-end-of-sexp (&optional arg)
  "Jump to end of the sexp the point is in.

The end is the point before the closing delimiter.

With no argument, this is the same as calling
\\[universal-argument] \\[universal-argument] `sp-backward-down-sexp'.

With ARG positive N > 1, move forward out of the current
expression, move N-1 expressions forward and move down backward
one level into previous expression.

With ARG negative -N < 1, move backward out of the current
expression, move N-2 expressions backward and move down backward
one level into previous expression.

With ARG raw prefix argument \\[universal-argument] move out of the current expressions
and then to the end of enclosing expression.

Examples:

  (foo |(bar baz) quux (blab glob)) -> (foo (bar baz) quux (blab glob)|)

  (foo (|bar baz) quux (blab glob)) -> (foo (bar baz|) quux (blab glob))

  (|foo) (bar) (baz quux) -> (foo) (bar) (baz quux|) ;; 3

  (foo bar) (baz) (quux|) -> (foo bar|) (baz) (quux) ;; -3

  ((foo |bar) (baz quux) blab) -> ((foo bar) (baz quux) blab|) ;; \\[universal-argument]"
  (interactive "P")
  (let* ((raw (sp--raw-argument-p arg))
         (arg (prefix-numeric-value arg))
         (re (cond
              ((and raw (= arg 4))
               (sp-up-sexp)
               (sp-end-of-sexp))
              ((= arg 1)
               (sp-down-sexp '(-16)))
              ((< arg 0)
               (sp-backward-up-sexp)
               (sp-forward-sexp (+ 2 arg))
               (sp-backward-down-sexp))
              ((> arg 0)
               (sp-up-sexp)
               (sp-forward-sexp (1- arg))
               (sp-backward-down-sexp)))))
    (sp--run-hook-with-args (sp-get re :op) :post-handlers 'end-of-sexp)
    re))

(defun sp-beginning-of-next-sexp (&optional arg)
  "Jump to the beginning of next sexp on the same depth.

This acts exactly as `sp-beginning-of-sexp' but adds 1 to the
numeric argument.

Examples:

  (f|oo) (bar) (baz) -> (foo) (|bar) (baz)

  (f|oo) (bar) (baz) -> (foo) (bar) (|baz) ;; 2"
  (interactive "P")
  (if (sp--raw-argument-p arg)
      (sp-beginning-of-sexp arg)
    (let ((arg (prefix-numeric-value arg)))
      (if (> arg 0)
          (sp-beginning-of-sexp (1+ arg))
        (sp-beginning-of-sexp (1- arg))))))

(defun sp-beginning-of-previous-sexp (&optional arg)
  "Jump to the beginning of previous sexp on the same depth.

This acts exactly as `sp-beginning-of-sexp' with negative
argument but subtracts 1 from it.

Examples:

  (foo) (b|ar) (baz) -> (|foo) (bar) (baz)

  (foo) (bar) (b|az) -> (|foo) (bar) (baz) ;; 2"
  (interactive "P")
  (if (sp--raw-argument-p arg)
      (sp-beginning-of-sexp (sp--negate-argument arg))
    (let ((arg (prefix-numeric-value arg)))
      (if (> arg 0)
          (sp-beginning-of-sexp (- (1+ arg)))
        (sp-beginning-of-sexp (- (1- arg)))))))

(defun sp-end-of-next-sexp (&optional arg)
  "Jump to the end of next sexp on the same depth.

This acts exactly as `sp-end-of-sexp' but adds 1 to the
numeric argument.

Examples:

  (f|oo) (bar) (baz) -> (foo) (bar|) (baz)

  (f|oo) (bar) (baz) -> (foo) (bar) (baz|) ;; 2"
  (interactive "P")
  (if (sp--raw-argument-p arg)
      (sp-end-of-sexp arg)
    (let ((arg (prefix-numeric-value arg)))
      (if (> arg 0)
          (sp-end-of-sexp (1+ arg))
        (sp-end-of-sexp (1- arg))))))

(defun sp-end-of-previous-sexp (&optional arg)
  "Jump to the end of previous sexp on the same depth.

This acts exactly as `sp-end-of-sexp' with negative
argument but subtracts 1 from it.

Examples:

  (foo) (b|ar) (baz) -> (foo|) (bar) (baz)

  (foo) (bar) (b|az) -> (foo|) (bar) (baz) ;; 2"
  (interactive "P")
  (if (sp--raw-argument-p arg)
      (sp-end-of-sexp (sp--negate-argument arg))
    (let ((arg (prefix-numeric-value arg)))
      (if (> arg 0)
          (sp-end-of-sexp (- (1+ arg)))
        (sp-end-of-sexp (- (1- arg)))))))

(defun sp-up-sexp (&optional arg interactive)
  "Move forward out of one level of parentheses.

With ARG, do this that many times.  A negative argument means
move backward but still to a less deep spot.

The argument INTERACTIVE is for internal use only.

If called interactively and `sp-navigate-reindent-after-up' is
enabled for current major-mode, remove the whitespace between end
of the expression and the last \"thing\" inside the expression.

If `sp-navigate-close-if-unbalanced' is non-nil, close the
unbalanced expressions automatically.

Examples:

  (foo |(bar baz) quux blab) -> (foo (bar baz) quux blab)|

  (foo (bar |baz) quux blab) -> (foo (bar baz) quux blab)| ;; 2

  (foo bar |baz              -> (foo bar baz)| ;; re-indent the expression
​   )

  (foo  |(bar baz)           -> (foo)| (bar baz) ;; close unbalanced expr."
  (interactive "p\np")
  (setq arg (or arg 1))
  (let ((ok (sp-get-enclosing-sexp (abs arg))))
    (if ok
        (progn
          (if (> arg 0)
              (goto-char (sp-get ok :end))
            (goto-char (sp-get ok :beg)))
          (when (and (= (abs arg) 1)
                     (not (equal (sp-get ok :prefix) sp-comment-char))
                     (or (memq major-mode (assq 'always sp-navigate-reindent-after-up))
                         (and (memq major-mode (assq 'interactive sp-navigate-reindent-after-up))
                              interactive)))
            ;; TODO: this needs different indent rules for different
            ;; modes.  Should we concern with such things?  Lisp rules are
            ;; funny in HTML... :/
            (save-excursion
              (if (> arg 0)
                  (progn
                    (goto-char (sp-get ok :end-in))
                    (let ((prev (sp-get-thing t)))
                      ;; if the expression is empty remove everything inside
                      (if (sp-compare-sexps ok prev)
                          (sp-get ok (delete-region :beg-in :end-in))
                        (when (save-excursion
                                (skip-chars-backward " \t\n")
                                (= (point) (sp-get prev :end-suf)))
                          (delete-region (sp-get prev :end-suf) (point))))))
                (goto-char (sp-get ok :beg-in))
                (let ((next (sp-get-thing)))
                  (if (sp-compare-sexps ok next)
                      (sp-get ok (delete-region :beg-in :end-in))
                    (when (save-excursion
                            (skip-chars-forward " \t\n")
                            (= (point) (sp-get next :beg-prf)))
                      (delete-region (point) (sp-get next :beg-prf)))))))))
      ;; on forward up, we can detect that the pair was not closed.
      ;; Therefore, jump sexps backwards until we hit the error, then
      ;; extract the opening pair and insert it at point.  Only works
      ;; for pairs defined in `sp-pair-list'.
      (when (and (> arg 0)
                 sp-navigate-close-if-unbalanced)
        (let (active-pair)
          (save-excursion
            ;; add support for SGML tags here
            (while (sp-backward-sexp))
            (sp-skip-backward-to-symbol t)
            (when (sp--looking-back (sp--get-opening-regexp))
              (let* ((op (match-string 0)))
                (setq active-pair (assoc op sp-pair-list)))))
          (when active-pair
            (sp-backward-sexp)
            (sp-forward-sexp)
            (insert (cdr active-pair))))))
    ok))

(defun sp-backward-up-sexp (&optional arg interactive)
  "Move backward out of one level of parentheses.

With ARG, do this that many times.  A negative argument means
move forward but still to a less deep spot.

The argument INTERACTIVE is for internal use only.

If called interactively and `sp-navigate-reindent-after-up' is
enabled for current major-mode, remove the whitespace between
beginning of the expression and the first \"thing\" inside the
expression.

Examples:

  (foo (bar baz) quux| blab) -> |(foo (bar baz) quux blab)

  (foo (bar |baz) quux blab) -> |(foo (bar baz) quux blab) ;; 2

  (                  -> |(foo bar baz)
​    foo |bar baz)"
  (interactive "p\np")
  (setq arg (or arg 1))
  (sp-up-sexp (- arg) interactive))

(defvar sp-last-kill-whitespace nil
  "Save the whitespace cleaned after the last kill.

If the next command is `sp-kill-sexp', append the whitespace
between the successive kills.")

(defun sp-kill-sexp (&optional arg dont-kill)
  "Kill the balanced expression following point.

If point is inside an expression and there is no following
expression, kill the topmost enclosing expression.

With ARG being positive number N, repeat that many times.

With ARG being Negative number -N, repeat that many times in
backward direction.

With ARG being raw prefix \\[universal-argument], kill all the expressions from
point up until the end of current list.  With raw prefix \\[negative-argument] \\[universal-argument],
kill all the expressions from beginning of current list up until
point.  If point is inside a symbol, this is also killed.  If
there is no expression after/before the point, just delete the
whitespace up until the closing/opening delimiter.

With ARG being raw prefix \\[universal-argument] \\[universal-argument], kill current list (the list
point is inside).

With ARG numeric prefix 0 (zero) kill the insides of the current
list, that is everything from after the opening delimiter to
before the closing delimiter.

If ARG is nil, default to 1 (kill single expression forward)

If second optional argument DONT-KILL is non-nil, save the to be
killed region in the kill ring, but do not kill the region from
buffer.

With `sp-navigate-consider-symbols', symbols and strings are also
considered balanced expressions.

Examples:

 (foo |(abc) bar)  -> (foo | bar) ;; nil, defaults to 1

 (foo (bar) | baz) -> |           ;; 2

 (foo |(bar) baz)  -> |           ;; \\[universal-argument] \\[universal-argument]

 (1 |2 3 4 5 6)    -> (1|)        ;; \\[universal-argument]

 (1 |2 3 4 5 6)    -> (1 | 5 6)   ;; 3

 (1 2 3 4 5| 6)    -> (1 2 3 | 6) ;; -2

 (1 2 3 4| 5 6)    -> (|5 6)      ;; - \\[universal-argument]

 (1 2 |   )        -> (1 2|)      ;; \\[universal-argument], kill useless whitespace

 (1 2 3 |4 5 6)    -> (|)         ;; 0

Note: prefix argument is shown after the example in
\"comment\". Assumes `sp-navigate-consider-symbols' equal to t."
  (interactive "P")
  (let* ((raw (sp--raw-argument-p arg))
         (arg (prefix-numeric-value arg))
         (n (abs arg))
         (ok t)
         (b (point-max))
         (e (point-min))
         (kill-fn (if dont-kill 'copy-region-as-kill 'kill-region)))
    (cond
     ;; kill to the end or beginning of list
     ((and raw
           (= n 4))
      (let ((next (sp-get-thing (< arg 0)))
            (enc (sp-get-enclosing-sexp)))
        (if (sp-compare-sexps next enc)
            (when (not dont-kill)
              (let ((del (sp-get-whitespace)))
                (sp-get del (delete-region :beg :end))))
          (if (> arg 0)
              (funcall kill-fn (sp-get next :beg-prf) (sp-get enc :end-in))
            (funcall kill-fn (sp-get next :end) (sp-get enc :beg-in)))
          (when (not dont-kill)
            (let ((del (sp-get-whitespace)))
              (sp-get del (delete-region :beg :end)))))))
     ;; kill the enclosing list
     ((and raw
           (= n 16))
      (let ((lst (sp-backward-up-sexp)))
        (sp-get lst (funcall kill-fn :beg-prf :end))))
     ;; kill inside of sexp
     ((= n 0)
      (let ((e (sp-get-enclosing-sexp)))
        (when e
          (sp-get e (funcall kill-fn :beg-in :end-in)))))
     ;; regular kill
     (t
      (save-excursion
        (while (and (> n 0) ok)
          (setq ok (sp-forward-sexp (sp--signum arg)))
          (sp-get ok
            (when (< :beg-prf b) (setq b :beg-prf))
            (when (> :end e) (setq e :end)))
          (setq n (1- n))))
      (when ok
        (let ((bm (set-marker (make-marker) b)))
          (if (eq last-command 'kill-region)
              (progn
                (when (member sp-successive-kill-preserve-whitespace '(1 2))
                  (kill-append sp-last-kill-whitespace nil))
                (funcall kill-fn (if (> b (point)) (point) b) e))
            (funcall kill-fn b e))
          ;; kill useless junk whitespace, but only if we're actually
          ;; killing the region
          (when (not dont-kill)
            (sp--cleanup-after-kill)
            ;; kill useless newlines
            (when (string-match-p "\n" (buffer-substring-no-properties bm (point)))
              (setq sp-last-kill-whitespace
                    (concat sp-last-kill-whitespace
                            (buffer-substring-no-properties bm (point))))
              (delete-region bm (point)))
            (when (= 0 sp-successive-kill-preserve-whitespace)
              (kill-append sp-last-kill-whitespace nil)))))))))

(defun sp--cleanup-after-kill ()
  (unless (looking-back "^[\t\s]+")
    (let ((bdel (save-excursion
                  (when (looking-back " ")
                    (skip-chars-backward " \t")
                    (when (not (looking-back (sp--get-opening-regexp)))
                      (forward-char)))
                  (point)))
          (edel (save-excursion
                  (when (looking-at " ")
                    (skip-chars-forward " \t")
                    (when (not (or (sp--looking-at (sp--get-closing-regexp))
                                   (looking-at "$")))
                      (backward-char)))
                  (point))))
      (when (eq this-command 'kill-region)
        (setq sp-last-kill-whitespace
              (if (/= 2 sp-successive-kill-preserve-whitespace)
                  (buffer-substring-no-properties bdel edel)
                "")))
      (delete-region bdel edel)))
  (if (memq major-mode sp--lisp-modes)
      (indent-according-to-mode)
    (save-excursion
      (indent-region (line-beginning-position) (line-end-position)))
    (when (> (save-excursion
               (back-to-indentation)
               (current-indentation))
             (current-column))
      (back-to-indentation))))

(defun sp-backward-kill-sexp (&optional arg dont-kill)
  "Kill the balanced expression preceding point.

This is exactly like calling `sp-kill-sexp' with minus ARG.
In other words, the direction of all commands is reversed.  For
more information, see the documentation of `sp-kill-sexp'.

Examples:

  (foo (abc)| bar)           -> (foo | bar)

  blab (foo (bar baz) quux)| -> blab |

  (1 2 3 |4 5 6)             -> (|4 5 6) ;; \\[universal-argument]"
  (interactive "P")
  (sp-kill-sexp (sp--negate-argument arg) dont-kill))

(defun sp-copy-sexp (&optional arg)
  "Copy the following ARG expressions to the kill-ring.

This is exactly like calling `sp-kill-sexp' with second argument
t.  All the special prefix arguments work the same way."
  (interactive "P")
  (save-excursion
    (sp-kill-sexp arg t)))

(defun sp-backward-copy-sexp (&optional arg)
  "Copy the previous ARG expressions to the kill-ring.

This is exactly like calling `sp-backward-kill-sexp' with second argument
t.  All the special prefix arguments work the same way."
  (interactive "P")
  (save-excursion
    (sp-kill-sexp (sp--negate-argument arg) t)))

(defun sp-clone-sexp ()
  (interactive)
  (-when-let (ok (sp-get-thing))
    (sp-get ok
      (goto-char :end-suf)
      (sp-newline)
      (insert (buffer-substring-no-properties :beg-prf :end-suf)))))

(defun sp-kill-hybrid-sexp (arg)
  "Kill a line as if with `kill-line', but respecting delimiters.

With ARG being raw prefix \\[universal-argument] \\[universal-argument], kill the hybrid sexp
the point is in (see `sp-get-hybrid-sexp').

With ARG numeric prefix 0 (zero) just call `kill-line'.

You can customize the behaviour of this command by toggling
`sp-hybrid-kill-excessive-whitespace'.

Examples:

  foo | bar baz               -> foo |               ;; nil

  foo (bar | baz) quux        -> foo (bar |) quux    ;; nil

  foo | bar (baz              -> foo |               ;; nil
             quux)

  foo \"bar |baz quux\" quack   -> foo \"bar |\" quack   ;; nil

  foo (bar
       baz) qu|ux (quack      ->   foo | hoo         ;; \\[universal-argument] \\[universal-argument]
                   zaq) hoo

  foo | (bar                  -> foo |               ;; C-0
         baz)                          baz)"
  (interactive "P")
  (let* ((raw (sp--raw-argument-p arg))
         (arg (prefix-numeric-value arg))
         (orig-indent (save-excursion
                        (back-to-indentation)
                        (current-column))))
    (cond
     ((= arg 0) (kill-line))
     ((and raw (= arg 16))
      (let ((hl (sp-get-hybrid-sexp)))
        (sp-get hl (kill-region :beg-prf :end-suf))))
     (t
      (let ((hl (sp-get-hybrid-sexp)))
        (save-excursion
          (when (and (or (eq sp-hybrid-kill-entire-symbol t)
                         (and (functionp sp-hybrid-kill-entire-symbol)
                              (not (funcall sp-hybrid-kill-entire-symbol))))
                     (sp-point-in-symbol))
            (sp-backward-sexp))
          (sp-get hl
            (kill-region (point) (min (point-max) (if (looking-at "[ \t]*$") (1+ :end-suf) :end-suf)))
            (when sp-hybrid-kill-excessive-whitespace
              (cond
               ((sp-point-in-blank-line)
                (while (and (not (eobp))
                            (sp-point-in-blank-line))
                  (delete-region (line-beginning-position) (min (point-max) (1+ (line-end-position))))))
               ((looking-at "[ \t]*$")
                (delete-blank-lines)))))))
      (sp--cleanup-after-kill)
      ;; if we've killed the entire line, do *not* contract the indent
      ;; to just one space
      (when (sp-point-in-blank-line)
        (delete-region (line-beginning-position) (line-end-position))
        (let ((need-indent (- orig-indent (current-column))))
          (when (> need-indent 0)
            (insert (make-string need-indent ?\ )))))))))

(defun sp--transpose-objects (first second)
  "Transpose FIRST and SECOND object while preserving the
whitespace between them."
  (save-excursion
    (goto-char (sp-get second :beg-prf))
    (let ((ins (sp-get second (delete-and-extract-region :beg-prf :end-suf)))
          (between (delete-and-extract-region (sp-get first :end-suf) (point))))
      (goto-char (sp-get first :beg-prf))
      (insert ins between))))

(defun sp-transpose-sexp (&optional arg)
  "Transpose the expressions around point.

The operation will move the point after the transposed block, so
the next transpose will \"drag\" it forward.

With arg positive N, apply that many times, dragging the
expression forward.

With arg negative -N, apply N times backward, pushing the word
before cursor backward.  This will therefore not transpose the
expressions before and after point, but push the expression
before point over the one before it.

Examples:

  foo |bar baz     -> bar foo| baz

  foo |bar baz     -> bar baz foo| ;; 2

  (foo) |(bar baz) -> (bar baz) (foo)|

  (foo bar)        ->    (baz quux)   ;; keeps the formatting
​    |(baz quux)            |(foo bar)

  foo bar baz|     -> foo baz| bar ;; -1"
  (interactive "P")
  (let* ((raw (sp--raw-argument-p arg))
         (arg (prefix-numeric-value arg))
         (n (abs arg)))
    ;; if we're inside a symbol, we need to move out of it first
    (when (> arg 0)
      (when (sp-point-in-symbol)
        (sp-forward-symbol)))
    (while (> n 0)
      (when (< arg 0) (sp-backward-sexp))
      (let* ((next (save-excursion (sp-forward-sexp)))
             (prev (save-excursion (goto-char (sp-get next :beg-prf)) (sp-backward-sexp))))
        (sp--transpose-objects prev next)
        (when (< arg 0)
          (goto-char (+ (sp-get prev :beg-prf) (sp-get next :len))))
        (setq n (1- n))))))

(defun sp-transpose-hybrid-sexp (&optional arg)
  "Transpose the hybrid sexps around point.

`sp-backward-sexp' is used to enter the previous hybrid sexp.

With ARG numeric prefix call `transpose-lines' with this
argument.

The operation will move the point at the next line after the
transposed block if it is at the end of line already.

Examples:

  foo bar            baz (quux
  |baz (quux   ->         quack)
        quack)       foo bar\\n|


  [(foo) (bar) -> [(baz)
  |(baz)]          (foo) (bar)|]

  foo bar baz  -> quux flux
  |quux flux      foo bar baz\\n|"
  (interactive "P")
  (if (numberp arg)
      (transpose-lines arg)
    (let* ((next (save-excursion
                   (sp-forward-sexp)
                   (sp-backward-sexp)
                   (sp-get-hybrid-sexp)))
           (prev (save-excursion
                   (goto-char (sp-get next :beg))
                   (sp-backward-sexp)
                   (sp-get-hybrid-sexp))))
      (if (sp-compare-sexps prev next > :end)
          (sp-message :invalid-context-prev)
        (sp--transpose-objects prev next))
      (when (looking-at "[\n\t ]+")
        (forward-line)
        (back-to-indentation)))))

(defun sp-push-hybrid-sexp ()
  "Push the hybrid sexp after point over the following one.

`sp-forward-sexp' is used to enter the following hybrid sexp.

Examples:

  |x = big_function_call(a,    |(a,
                         b)      b) = read_user_input()
                           ->
  (a,                          x = big_function_call(a,
   b) = read_user_input()                            b)"
  (interactive)
  (let* ((cur (sp-get-hybrid-sexp))
         (next (save-excursion
                 (goto-char (sp-get cur :end))
                 (sp-forward-sexp)
                 (sp-get-hybrid-sexp))))
    (if (sp-compare-sexps cur next >)
        (sp-message :invalid-context-cur)
      (sp--transpose-objects cur next))))

;; The following two functions are inspired by "adjust-parens.el"
;; package available at
;; http://elpa.gnu.org/packages/adjust-parens-1.0.el
(defun sp-indent-adjust-sexp ()
  "Add the hybrid sexp at line into previous sexp.  All forms
between the two are also inserted.  Specifically, if the point is
on empty line, move the closing delimiter there, so the next
typed text will become the last item of the previous sexp.

This acts similarly to `sp-add-to-previous-sexp' but with special
handling of empty lines."
  (interactive)
  (let* ((hsexp (sp-get-hybrid-sexp))
         (prev-sexp (save-excursion
                      (goto-char (sp-get hsexp :beg))
                      (sp-get-sexp t))))
    (if (not (and prev-sexp hsexp
                  (sp-compare-sexps prev-sexp hsexp < :end :beg)))
        (sp-message :no-structure-found)
      (save-excursion
        (sp-get prev-sexp
          (goto-char (sp-get hsexp :end))
          (insert :cl)
          (goto-char :end-in)
          (delete-char :cl-l)))
      (sp-get (sp-get-enclosing-sexp) (indent-region :beg :end))
      (indent-according-to-mode)
      (sp--run-hook-with-args (sp-get prev-sexp :op) :post-handlers 'indent-adjust-sexp))))

(defun sp-dedent-adjust-sexp ()
  "Remove the hybrid sexp at line from previous sexp.  All
sibling forms after it are also removed (not deleted, just placed
outside of the enclosing list).  Specifically, if the point is on
empty line followed by closing delimiter of enclosing list, move
the closing delimiter after the last item in the list.

This acts similarly to `sp-forward-barf-sexp' but with special
handling of empty lines."
  (interactive)
  (-when-let (enc (sp-get-enclosing-sexp))
    (save-excursion
      ;; if we're looking at whitespace and end of sexp, move the
      ;; closing paren over the whitespace but *after* the last item
      ;; in the list (barf would also go *before* the last item)
      (sp-skip-forward-to-symbol t)
      (if (= (point) (sp-get enc :end-in))
          (let ((prev-sexp (sp-get-thing t)))
            (sp-get enc
              (delete-char :cl-l)
              (goto-char (sp-get prev-sexp :end))
              ;; see next TODO
              (save-restriction
                (sp--narrow-to-line)
                (skip-syntax-forward " ")
                (skip-syntax-forward "."))
              (insert :cl)))
        ;; otherwise just C-u barf
        (sp-skip-backward-to-symbol t)
        (sp-forward-barf-sexp '(4))
        ;; we need to take special care of any hanging
        ;; punctuation. TODO: this should be a sexp suffix? HACK until
        ;; we fix barf to get the info.
        (save-restriction
          (sp-get (sp-backward-down-sexp)
            (goto-char :end)
            (delete-char (- :cl-l))
            (sp--narrow-to-line)
            (skip-syntax-forward " ")
            (skip-syntax-forward ".")
            (insert :cl)))
        (sp-get enc (indent-region :beg :end))))
    (indent-according-to-mode)
    (sp--run-hook-with-args (sp-get enc :op) :post-handlers 'dedent-adjust-sexp)))

;;  "When the hook is called point is *after* the just moved closing delimiter."
;; TODO: add hook
(defun sp-slurp-hybrid-sexp ()
  "Add hybrid sexp following the current list in it by moving the
closing delimiter.

This is philosophically similar to `sp-forward-slurp-sexp' but
works better in \"line-based\" languages like C or Java.

Because the structure is much looser in these languages, this
command currently does not support all the prefix argument
triggers that `sp-forward-slurp-sexp' does."
  (interactive)
  (-if-let* ((enc (sp-get-enclosing-sexp))
             (bsexp (save-excursion
                      (sp-get enc (goto-char :end))
                      (when (sp-compare-sexps (sp-forward-sexp) enc >)
                        (sp-get-hybrid-sexp)))))
      (save-excursion
        (sp-get enc
          (goto-char :end-suf)
          (delete-char (- (+ :cl-l :suffix-l)))
          ;; TODO: move to hook
          (when (sp-point-in-blank-line)
            (delete-region (line-beginning-position) (1+ (line-end-position))))
          (sp-forward-sexp)
          (sp-get (sp-get-hybrid-sexp) (goto-char :end-suf))
          (insert :cl :suffix))
        ;; TODO: move to hook
        (sp-get (sp--next-thing-selection -1)
          (save-excursion
            (if (save-excursion
                  (goto-char :beg-in)
                  (looking-at "[ \t]*$"))
                (progn
                  (goto-char :end-in)
                  (newline))
              ;; copy the whitespace after opening delim and put it in
              ;; front of the closing. This will ensure pretty { foo }
              ;; or {foo}
              (goto-char :end-in)
              (insert (buffer-substring-no-properties
                       :beg-in
                        (+ :beg-in (save-excursion
                                     (goto-char :beg-in)
                                     (skip-syntax-forward " ")))))))
          (unless (or (looking-at "[ \t]*$")
                      (looking-at (sp--get-stringlike-regexp))
                      (looking-at (sp--get-closing-regexp)))
            (newline)))
        (sp-get (sp--next-thing-selection -1) (indent-region :beg :end))
        ;; we need to call this again to get the new structure after
        ;; indent.
        (sp--next-thing-selection -1))
    (sp-message :invalid-structure)
    nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; "paredit" operations

(defun sp-forward-slurp-sexp (&optional arg)
  "Add sexp following the current list in it by moving the closing delimiter.

If the current list is the last in a parent list, extend that
list (and possibly apply recursively until we can extend a list
or end of file).

If ARG is N, apply this function that many times.

If ARG is negative -N, extend the opening pair instead (that is,
backward).

If ARG is raw prefix \\[universal-argument], extend all the way to the end of the parent list.

If both the current expression and the expression to be slurped
are strings, they are joined together.

Examples:

  (foo |bar) baz        -> (foo |bar baz)

  [(foo |bar)] baz      -> [(foo |bar) baz]

  [(foo |bar) baz]      -> [(foo |bar baz)]

  ((|foo) bar baz quux) -> ((|foo bar baz quux)) ;; with \\[universal-argument]

  \"foo| bar\" \"baz quux\" -> \"foo| bar baz quux\""
  (interactive "P")
  (if (> (prefix-numeric-value arg) 0)
      (let ((n (abs (prefix-numeric-value arg)))
            (enc (sp-get-enclosing-sexp))
            (ins-space 0)
            next-thing ok)
        (when enc
          (save-excursion
            (if (sp--raw-argument-p arg)
                (progn
                  (goto-char (sp-get enc :end-suf))
                  (setq next-thing (sp-get-enclosing-sexp))
                  (when next-thing
                    (goto-char (sp-get next-thing :end-in))
                    (sp--run-hook-with-args (sp-get enc :op) :pre-handlers 'slurp-forward)
                    (sp-get enc (insert :cl :suffix))
                    (goto-char (sp-get enc :end-suf))
                    (delete-char (sp-get enc (- (+ :cl-l :suffix-l))))
                    (indent-region (sp-get enc :beg-prf) (sp-get next-thing :end))
                    (sp--run-hook-with-args (sp-get enc :op) :post-handlers 'slurp-forward)))
              (while (> n 0)
                (goto-char (sp-get enc :end-suf))
                (setq ok enc)
                (setq next-thing (sp-get-thing nil))
                (setq ins-space 0)
                (while (sp-compare-sexps next-thing ok <)
                  (goto-char (sp-get next-thing :end-suf))
                  (setq ok next-thing)
                  (setq next-thing (sp-get-thing nil)))
                (if ok
                    (progn
                      (if (and (equal (sp-get next-thing :cl) "\"")
                               (equal (sp-get ok :cl) "\""))
                          (progn
                            (sp--join-sexp ok next-thing)
                            (goto-char (- (sp-get next-thing :end) 2))
                            (plist-put enc :end (- (sp-get next-thing :end) 2)))
                        (delete-char (sp-get ok (- (+ :cl-l :suffix-l))))
                        (when (and (sp-get ok (/= :len-in 0))
                                   (= (sp-get ok :end-suf) (sp-get next-thing :beg-prf)))
                          (insert " ")
                          (setq ins-space -1))
                        ;; this calculation corrects the absence of already deleted cls
                        (goto-char (- (sp-get next-thing :end-suf) (sp-get ok (+ :cl-l :suffix-l)) ins-space))
                        (sp--run-hook-with-args (sp-get enc :op) :pre-handlers 'slurp-forward)
                        (sp-get ok (insert :cl :suffix))
                        (indent-region (sp-get ok :beg-prf) (point))
                        ;; HACK: update the "enc" data structure if ok==enc
                        (when (= (sp-get enc :beg) (sp-get ok :beg)) (plist-put enc :end (point)))
                        (sp--run-hook-with-args (sp-get enc :op) :post-handlers 'slurp-forward))
                      (setq n (1- n)))
                  (sp-message :cant-slurp)
                  (setq n -1)))))))
    (sp-backward-slurp-sexp (sp--negate-argument arg))))

(defun sp-backward-slurp-sexp (&optional arg)
  "Add the sexp preceding the current list in it by moving the opening delimiter.

If the current list is the first in a parent list, extend that
list (and possibly apply recursively until we can extend a list
or beginning of file).

If arg is N, apply this function that many times.

If arg is negative -N, extend the closing pair instead (that is,
forward).

If ARG is raw prefix \\[universal-argument], extend all the way to the beginning of the parent list.

If both the current expression and the expression to be slurped
are strings, they are joined together.

Examples:

  foo (bar| baz)        -> (foo bar| baz)

  foo [(bar| baz)]      -> [foo (bar| baz)]

  [foo (bar| baz)]      -> [(foo bar| baz)]

  (foo bar baz (|quux)) -> ((foo bar baz |quux)) ;; with \\[universal-argument]

  \"foo bar\" \"baz |quux\" -> \"foo bar baz |quux\""
  (interactive "P")
  (if (> (prefix-numeric-value arg) 0)
      (let ((n (abs (prefix-numeric-value arg)))
            (enc (sp-get-enclosing-sexp))
            next-thing ok)
        (when enc
          (save-excursion
            (if (sp--raw-argument-p arg)
                (progn
                  (goto-char (sp-get enc :beg-prf))
                  (setq next-thing (sp-get-enclosing-sexp))
                  (when next-thing
                    (delete-char (sp-get enc (+ :op-l :prefix-l)))
                    (goto-char (sp-get next-thing :beg-in))
                    (sp--run-hook-with-args (sp-get enc :op) :pre-handlers 'slurp-backward)
                    (sp-get enc (insert :prefix :op))
                    (indent-region (sp-get next-thing :beg-in) (sp-get enc :end))
                    (sp--run-hook-with-args (sp-get enc :op) :post-handlers 'slurp-backward)))
              (while (> n 0)
                (goto-char (sp-get enc :beg-prf))
                (setq ok enc)
                (setq next-thing (sp-get-thing t))
                (while (sp-compare-sexps next-thing ok > :end)
                  (goto-char (sp-get next-thing :beg-prf))
                  (setq ok next-thing)
                  (setq next-thing (sp-get-thing t)))
                (if ok
                    (progn
                      (if (and (equal (sp-get next-thing :cl) "\"")
                               (equal (sp-get ok :cl) "\""))
                          (progn
                            (sp--join-sexp next-thing ok)
                            (goto-char (sp-get next-thing :beg-prf))
                            (plist-put enc :beg (sp-get next-thing :beg)))
                        (delete-char (sp-get ok (+ :op-l :prefix-l)))
                        (when (and (sp-get ok (/= :len-in 0))
                                   (= (sp-get ok :beg-prf) (sp-get next-thing :end-suf)))
                          (insert " "))
                        (goto-char (sp-get next-thing :beg-prf))
                        (sp--run-hook-with-args (sp-get enc :op) :pre-handlers 'slurp-backward)
                        (sp-get ok (insert :prefix :op))
                        (indent-region (point) (sp-get ok :end))
                        ;; HACK: update the "enc" data structure if ok==enc
                        (when (sp-compare-sexps enc ok) (plist-put enc :beg (- (point) (sp-get ok :op-l))))
                        (sp--run-hook-with-args (sp-get enc :op) :post-handlers 'slurp-backward))
                      (setq n (1- n)))
                  (sp-message :cant-slurp)
                  (setq n -1)))))))
    (sp-forward-slurp-sexp (sp--negate-argument arg))))

(defun sp-add-to-previous-sexp (&optional arg)
  "Add the expression around point to the first list preceding point.

With ARG positive N add that many expressions to the preceding
list.

If ARG is raw prefix argument \\[universal-argument] add all expressions until
the end of enclosing list to the previous list.

If ARG is raw prefix argument \\[universal-argument] \\[universal-argument] add the current
list into the previous list.

Examples:

  (foo bar) |baz quux        -> (foo bar |baz) quux

  (foo bar) |baz quux        -> (foo bar |baz quux) ;; 2

  (blab (foo bar) |baz quux) -> (blab (foo bar |baz quux)) ;; \\[universal-argument]

  (foo bar) (baz |quux)      -> (foo bar (baz |quux)) ;; \\[universal-argument] \\[universal-argument]"
  (interactive "P")
  (save-excursion
    (cond
     ((equal arg '(16))
      (sp-backward-up-sexp)
      (sp-backward-down-sexp)
      (sp-forward-slurp-sexp))
     (t
      (sp-backward-down-sexp)
      (sp-forward-slurp-sexp arg))))
  (indent-according-to-mode))

(defun sp-add-to-next-sexp (&optional arg)
  "Add the expressions around point to the first list following point.

With ARG positive N add that many expressions to the following
list.

If ARG is raw prefix argument \\[universal-argument] add all expressions until
the beginning of enclosing list to the following list.

If ARG is raw prefix argument \\[universal-argument] \\[universal-argument] add the current
list into the following list.

Examples:

  foo bar| (baz quux)        -> foo (bar| baz quux)

  foo bar| (baz quux)        -> (foo bar| baz quux) ;; 2

  (foo bar |(bar quux) blab) -> ((foo bar |bar quux) blab) ;; \\[universal-argument]

  (foo |bar) (baz quux)      -> ((foo |bar) baz quux) ;; \\[universal-argument] \\[universal-argument]"
  (interactive "P")
  (save-excursion
    (cond
     ((equal arg '(16))
      (sp-up-sexp)
      (sp-down-sexp)
      (sp-backward-slurp-sexp))
     (t
      (sp-down-sexp)
      (sp-backward-slurp-sexp arg)))))

(defun sp-forward-barf-sexp (&optional arg)
  "Remove the last sexp in the current list by moving the closing delimiter.

If ARG is positive number N, barf that many expressions.

If ARG is negative number -N, contract the opening pair instead.

If ARG is raw prefix \\[universal-argument], barf all expressions from the one after
point to the end of current list and place the point before the
closing delimiter of the list.

If the current list is empty, do nothing.

Examples: (prefix arg in comment)

  (foo bar| baz)   -> (foo bar|) baz   ;; nil (defaults to 1)

  (foo| [bar baz]) -> (foo|) [bar baz] ;; 1

  (1 2 3| 4 5 6)   -> (1 2 3|) 4 5 6   ;; \\[universal-argument] (or numeric prefix 3)

  (foo bar| baz)   -> foo (bar| baz)   ;; -1"
  (interactive "P")
  (let* ((raw (sp--raw-argument-p arg))
         (old-arg arg)
         (arg (prefix-numeric-value arg)))
    (if (> arg 0)
        (if (sp-point-in-blank-sexp)
            (sp-message :blank-sexp)
          (save-excursion
            (let ((enc (sp-get-enclosing-sexp)))
              (sp-get enc
                (cond
                 ((and raw (= arg 4))
                  (sp-get (sp-get-thing t)
                    (goto-char :end-suf)))
                 (t
                  (goto-char :end-in)
                  (sp-backward-sexp arg)
                  (when (<= (point) :beg)
                    (goto-char :beg-in))))
                ;; we know for sure there is at least one thing in the list
                (let ((back (sp-get-thing t)))
                  (if (sp-compare-sexps back enc)
                      (goto-char :beg-in)
                    (goto-char (sp-get back :end-suf))))
                (sp--run-hook-with-args :op :pre-handlers 'barf-forward))
              (sp-get (sp-get-enclosing-sexp)
                (sp-do-move-cl (point))
                (indent-region :beg :end)
                (sp--run-hook-with-args :op :post-handlers 'barf-forward)))))
      (sp-backward-barf-sexp (sp--negate-argument old-arg)))))

(defun sp-backward-barf-sexp (&optional arg)
  "This is exactly like calling `sp-forward-barf-sexp' with minus ARG.
In other words, instead of contracting the closing pair, the
opening pair is contracted.  For more information, see the
documentation of `sp-forward-barf-sexp'.

Examples:

  (foo bar| baz) -> foo (bar| baz)

  ([foo bar] |baz) -> [foo bar] (|baz)

  (1 2 3 |4 5 6) -> 1 2 3 (|4 5 6) ;; \\[universal-argument] (or 3)"
  (interactive "P")
  (let* ((raw (sp--raw-argument-p arg))
         (old-arg arg)
         (arg (prefix-numeric-value arg)))
    (if (> arg 0)
        (if (sp-point-in-blank-sexp)
            (sp-message :blank-sexp)
          (save-excursion
            (let ((enc (sp-get-enclosing-sexp)))
              (sp-get enc
                (cond
                 ((and raw (= arg 4))
                  (sp-get (sp-get-thing)
                    (goto-char :beg-prf)))
                 (t
                  (goto-char :beg-in)
                  (sp-forward-sexp arg)
                  (when (>= (point) :end)
                    (goto-char :end-in))))
                ;; we know for sure there is at least one thing in the list
                (let ((next (sp-get-thing)))
                  (if (sp-compare-sexps next enc)
                      (goto-char :end-in)
                    (goto-char (sp-get next :beg-prf))))
                (sp--run-hook-with-args :op :pre-handlers 'barf-backward))
              (sp-get (sp-get-enclosing-sexp)
                (sp-do-move-op (point))
                (indent-region :beg :end)
                (sp--run-hook-with-args :op :post-handlers 'barf-backward)))))
      (sp-forward-barf-sexp (sp--negate-argument old-arg)))))

;; TODO: get rid of the macro anyway, it's stupid!
(defmacro sp--skip-to-symbol-1 (forward)
  "Generate `sp-skip-forward-to-symbol' or `sp-skip-backward-to-symbol'."
  (let ((inc (if forward '1+ '1-))
        (dec (if forward '1- '1+))
        (forward-fn (if forward 'forward-char 'backward-char))
        (next-char-fn (if forward 'following-char 'preceding-char))
        (looking (if forward 'sp--looking-at 'sp--looking-back))
        (eob-test (if forward '(eobp) '(bobp)))
        (comment-bound (if forward 'cdr 'car)))
    `(let ((in-comment (sp-point-in-comment))
           ;; HACK: if we run out of current context this might skip a
           ;; pair that was not allowed before.  However, such a call is
           ;; never made in SP, so it's OK for now
           (allowed-pairs (sp--get-allowed-regexp))
           (allowed-strings (sp--get-stringlike-regexp)))
       (while (and (not (or ,eob-test
                            (and stop-after-string
                                 (not (sp-point-in-string))
                                 (sp-point-in-string (,dec (point))))
                            (and stop-at-string
                                 (not (sp-point-in-string))
                                 (sp-point-in-string (,inc (point))))
                            (and stop-inside-string
                                 (sp-point-in-string)
                                 (not (sp-point-in-string (,inc (point)))))
                            (,looking allowed-pairs)
                            (and (memq major-mode sp-navigate-consider-stringlike-sexp)
                                 (,looking allowed-strings))))
                   (or (member (char-syntax (,next-char-fn)) '(?< ?> ?! ?| ?\ ?\\ ?\" ?' ?.))
                       (unless in-comment (sp-point-in-comment))))
         (when (and (not in-comment)
                    (sp-point-in-comment))
           (goto-char (,comment-bound (sp-get-comment-bounds))))
         (when (not ,eob-test) (,forward-fn 1))))))

(defun sp-skip-forward-to-symbol (&optional stop-at-string stop-after-string stop-inside-string)
  "Skip whitespace and comments moving forward.

If STOP-AT-STRING is non-nil, stop before entering a string (if
not already in a string).

If STOP-AFTER-STRING is non-nil, stop after exiting a string.

If STOP-INSIDE-STRING is non-nil, stop before exiting a string.

Examples:

  foo|   bar -> foo   |bar

  foo|   [bar baz] -> foo   |[bar baz]"
  (interactive)
  (sp--skip-to-symbol-1 t))

(defun sp-skip-backward-to-symbol (&optional stop-at-string stop-after-string stop-inside-string)
  "Skip whitespace and comments moving backward.
If STOP-AT-STRING is non-nil, stop before entering a string (if
not already in a string).

If STOP-AFTER-STRING is non-nil, stop after exiting a string.

If STOP-INSIDE-STRING is non-nil, stop before exiting a string.

Examples:

  foo   |bar -> foo|   bar

  [bar baz]   |foo -> [bar baz]|   foo"
  (interactive)
  (sp--skip-to-symbol-1 nil))

(defun sp-skip-into-string (&optional back)
  "Move the point into the next string.

With BACK non-nil, move backwards."
  (if back
      (while (not (sp-point-in-string))
        (backward-char))
    (while (not (sp-point-in-string))
      (forward-char))))

;; TODO: in ruby, "foo |if bar" now moves correctly, but there's a
;; noticable lag before it jumps over "if".  This is probably caused
;; by :skip-match handlers.  Investigate!
(defun sp-forward-symbol (&optional arg)
  "Move point to the next position that is the end of a symbol.

With ARG being positive number N, repeat that many times.

With ARG being Negative number -N, repeat that many times in
backward direction.

A symbol is any sequence of characters that are in either the
word constituent or symbol constituent syntax class.  Current
symbol only extend to the possible opening or closing delimiter
as defined by `sp-add-pair' even if part of this delimiter
would match \"symbol\" syntax classes.

Examples:

  |foo bar baz          -> foo| bar baz

  |foo (bar (baz))      -> foo (bar| (baz)) ;; 2

  |foo (bar (baz) quux) -> foo (bar (baz) quux|) ;; 4"
  (interactive "p")
  (setq arg (or arg 1))
  (let* ((n (abs arg))
         (fw (> arg 0))
         (allowed (sp--get-allowed-pair-list))
         (open (sp--get-opening-regexp allowed))
         (close (sp--get-closing-regexp allowed)))
    (if fw
        (while (> n 0)
          ;; First we need to get to the beginning of a symbol.  This means
          ;; skipping all whitespace and pair delimiters until we hit
          ;; something in \sw or \s_
          (while (cond
                  ((eobp) nil)
                  ((not (memq (char-syntax (following-char)) '(?w ?_)))
                   (forward-char)
                   t)
                  ;; if allowed is empty, the regexp matches anything
                  ;; and we go into infinite loop, cf. Issue #400
                  ((and allowed (sp--valid-initial-delimiter-p (sp--looking-at open)))
                   (goto-char (match-end 0)))
                  ((and allowed (sp--valid-initial-delimiter-p (sp--looking-at close)))
                   (goto-char (match-end 0)))))
          (while (and (not (eobp))
                      (or (not allowed)
                          (not (or (sp--valid-initial-delimiter-p (sp--looking-at open))
                                   (sp--valid-initial-delimiter-p (sp--looking-at close)))))
                      (memq (char-syntax (following-char)) '(?w ?_)))
            (forward-char))
          (setq n (1- n)))
      (sp-backward-symbol n))))

(defun sp-backward-symbol (&optional arg)
  "Move point to the next position that is the beginning of a symbol.

With ARG being positive number N, repeat that many times.

With ARG being Negative number -N, repeat that many times in
forward direction.

A symbol is any sequence of characters that are in either the word
constituent or symbol constituent syntax class.  Current symbol only
extend to the possible opening or closing delimiter as defined by
`sp-add-pair' even if part of this delimiter would match \"symbol\"
syntax classes.

Examples:

  foo bar| baz            -> foo |bar baz

  ((foo bar) baz)|        -> ((foo |bar) baz) ;; 2

  (quux ((foo) bar) baz)| -> (|quux ((foo) bar) baz) ;; 4"
  (interactive "p")
  (setq arg (or arg 1))
  (let ((n (abs arg))
        (fw (> arg 0))
        (open (sp--get-opening-regexp (sp--get-allowed-pair-list)))
        (close (sp--get-closing-regexp (sp--get-allowed-pair-list))))
    (if fw
        (while (> n 0)
          (while (cond
                  ((bobp) nil)
                  ((not (memq (char-syntax (preceding-char)) '(?w ?_)))
                   (backward-char)
                   t)
                  ((sp--valid-initial-delimiter-p (sp--looking-back open))
                   (goto-char (match-beginning 0)))
                  ((sp--valid-initial-delimiter-p (sp--looking-back close))
                   (goto-char (match-beginning 0)))))
          (while (and (not (bobp))
                      (not (or (sp--valid-initial-delimiter-p (sp--looking-back open))
                               (sp--valid-initial-delimiter-p (sp--looking-back close))))
                      (memq (char-syntax (preceding-char)) '(?w ?_)))
            (backward-char))
          (setq n (1- n)))
      (sp-forward-symbol n))))

(defun sp-rewrap-sexp (&optional arg)
  "Rewrap the enclosing expression with a different pair.

The new pair is specified in minibuffer by typing the *opening*
delimiter, same way as with pair wrapping.

With raw prefix argument \\[universal-argument] do not remove the
old delimiters.

With numeric prefix argument 0 (zero) ignore rewrapping with
tags. This is sometimes useful because the tags always take
precedence over regular pairs.

Examples:

  (foo |bar baz) -> [foo |bar baz]   ;; [

  (foo |bar baz) -> [(foo |bar baz)] ;; \\[universal-argument] [

  \"foo |bar\"     -> <|>foo bar</>    ;; < in `html-mode'"
  (interactive "P")
  (let ((raw (sp--raw-argument-p arg))
        (pair "")
        (done nil)
        ev ac at)
    (while (not done)
      (setq ev (read-event (format "Rewrap with: %s" pair) t))
      (setq pair (concat pair (format-kbd-macro (vector ev))))
      (setq ac (--first (equal pair (car it)) (sp--get-pair-list-context)))
      (setq at (sp--get-active-tag pair))
      (cond
       ((and (/= (prefix-numeric-value arg) 0)
             (eq (length pair) (length (plist-get at :trigger))))
        (setq done t)
        (let ((enc (sp-get-enclosing-sexp)))
          (sp--unwrap-sexp enc t)
          (sp-get enc (sp--wrap-tag-create-overlays at :beg-prf (- :end :op-l :cl-l :prefix-l) t))))
       (ac
        (setq done t)
        (let ((enc (sp-get-enclosing-sexp)))
          (when enc
            (save-excursion
              (sp-get enc
                (goto-char :end)
                (unless raw
                  (delete-char (- :cl-l))))
              (insert (cdr ac))
              (sp-get enc
                (goto-char :beg)
                (insert (car ac))
                (unless raw
                  (delete-char :op-l)))))))))))

(defun sp-swap-enclosing-sexp (&optional arg)
  "Swap the enclosing delimiters of this and the parent expression.

With N > 0 numeric argument, ascend that many levels before
swapping.

Examples:

  (foo [|bar] baz)              -> [foo (|bar) baz] ;; 1

  (foo {bar [|baz] quux} quack) -> [foo {bar (|baz) quux} quack] ;; 2"
  (interactive "p")
  (let ((enc (sp-get-enclosing-sexp))
        (encp (sp-get-enclosing-sexp (1+ arg))))
    (if (and enc encp)
        (save-excursion
          (sp-get encp
            (goto-char :end)
            (delete-char (- :cl-l)))
          (sp-get enc
            (insert :cl)
            (goto-char :end)
            (delete-char (- :cl-l)))
          (sp-get encp (insert :cl))
          (sp-get enc (goto-char :beg-prf))
          (sp-get encp (insert :prefix :op))
          (sp-get enc (delete-char (+ :op-l :prefix-l)))
          (sp-get encp (goto-char :beg-prf))
          (sp-get enc (insert :prefix :op))
          (sp-get encp (delete-char (+ :op-l :prefix-l))))
      (sp-message :point-not-deep-enough))))

(defun sp--unwrap-sexp (sexp &optional no-cleanup)
  "Unwrap expression defined by SEXP.

Warning: this function remove possible empty lines and reindents
the unwrapped sexp, so the SEXP structure will no longer
represent a valid object in a buffer!"
  (sp-get sexp
    (delete-region :end-in :end)
    (delete-region :beg-prf :beg-in))
  ;; if the delimiters were the only thing on the line, we should also
  ;; get rid of the (possible) empty line that will be the result of
  ;; their removal.  This is especially nice in HTML mode or
  ;; long-running tags like \[\] in latex.
  (unless no-cleanup
    (let ((new-start (sp-get sexp :beg-prf))
          (new-end (sp-get sexp (- :end-in :op-l :prefix-l)))
          indent-from indent-to)
      (save-excursion
        (goto-char new-end)
        (when (string-match-p "^[\n\t ]+\\'" (thing-at-point 'line))
          (let ((b (bounds-of-thing-at-point 'line)))
            (delete-region (car b) (cdr b))))
        (setq indent-to (point))
        (goto-char new-start)
        (when (string-match-p "^[\n\t ]+\\'" (thing-at-point 'line))
          (let ((b (bounds-of-thing-at-point 'line)))
            (delete-region (car b) (cdr b))))
        (setq indent-from (point)))
      (sp--keep-indentation
        (indent-region indent-from indent-to)))))

(defun sp-unwrap-sexp (&optional arg)
  "Unwrap the following expression.

With ARG N, unwrap Nth expression as returned by
`sp-forward-sexp'.  If ARG is negative -N, unwrap Nth expression
backwards as returned by `sp-backward-sexp'.

Return the information about the just unwrapped expression.  Note
that this structure does not represent a valid expression in the
buffer.

Examples:

  |(foo bar baz)     -> |foo bar baz

  (foo bar| baz)     -> foo bar| baz

  |(foo) (bar) (baz) -> |(foo) bar (baz) ;; 2"
  (interactive "p")
  (setq arg (or arg 1))
  (let ((sp-navigate-consider-symbols nil))
    (let ((ok (save-excursion (sp-forward-sexp arg))))
      (when ok (sp--unwrap-sexp ok))
      ok)))

(defun sp-backward-unwrap-sexp (&optional arg)
  "Unwrap the previous expression.

With ARG N, unwrap Nth expression as returned by
`sp-backward-sexp'.  If ARG is negative -N, unwrap Nth expression
forward as returned by `sp-forward-sexp'.

Examples:

  (foo bar baz)|     -> foo bar baz|

  (foo bar)| (baz)   -> foo bar| (baz)

  (foo) (bar) (baz)| -> foo (bar) (baz) ;; 3"
  (interactive "p")
  (sp-unwrap-sexp (- (or arg 1))))

(defun sp-splice-sexp (&optional arg)
  "Unwrap the current list.

With ARG N, unwrap Nth list as returned by applying `sp-up-sexp'
N times.  This function expect positive arg.

Examples:

  (foo (bar| baz) quux) -> (foo bar| baz quux)

  (foo |(bar baz) quux) -> foo |(bar baz) quux

  (foo (bar| baz) quux) -> foo (bar| baz) quux ;; 2"
  (interactive "p")
  (setq arg (or arg 1))
  (-when-let (ok (sp-get-enclosing-sexp arg))
    (if (equal ";" (sp-get ok :prefix))
        (sp-get ok
          (goto-char :beg)
          (-when-let (enc (sp-get-enclosing-sexp arg))
            (sp--unwrap-sexp enc)))
      (sp--unwrap-sexp ok))))

(defun sp--splice-sexp-do-killing (beg end expr &optional jump-end)
  "Save the text in the region between BEG and END inside EXPR,
then delete EXPR and insert the saved text.

If optional argument JUPM-END is equal to the symbol 'end move
the point after the re-inserted text."
  (let (str p)
    (setq str (buffer-substring-no-properties beg end))
    (delete-region (sp-get expr :beg-prf) (sp-get expr :end))
    (save-excursion
      (insert str)
      (indent-region (sp-get expr :beg-prf) (point))
      (setq p (point)))
    (when (eq jump-end 'end) (goto-char p))))

;; The following two functions could be very simply implemented using
;; `sp-splice-sexp-killing-around' but these are more efficient
;; implementations.  With sufficiently big lists the difference is
;; noticable.
(defun sp-splice-sexp-killing-backward (&optional arg)
  "Unwrap the current list and kill all the expressions
between start of this list and the point.

With the optional argument ARG, repeat that many times.  This
argument should be positive number.

Examples:

  (foo (let ((x 5)) |(sqrt n)) bar)  -> (foo |(sqrt n) bar)

​  (when ok|                             |(perform-operation-1)
​    (perform-operation-1)            ->  (perform-operation-2)
​    (perform-operation-2))

​  (save-excursion                    -> |(awesome-stuff-happens) ;; 2
​    (unless (test)
​      |(awesome-stuff-happens)))

Note that to kill only the content and not the enclosing
delimiters you can use \\[universal-argument] \\[sp-backward-kill-sexp].
See `sp-backward-kill-sexp' for more information."
  (interactive "p")
  (while (> arg 0)
    (let ((ok (sp-get-enclosing-sexp 1)))
      (if ok
          (let ((next (sp-get-thing)))
            (if (sp-compare-sexps next ok)
                (sp-kill-sexp '(16))
              (sp--splice-sexp-do-killing
               (sp-get next :beg-prf)
               (sp-get ok :end-in)
               ok)))
        (setq arg -1)))
    (setq arg (1- arg))))

(defun sp-splice-sexp-killing-forward (&optional arg)
  "Unwrap the current list and kill all the expressions between
the point and the end of this list.

With the optional argument ARG, repeat that many times.  This
argument should be positive number.

Examples:

  (a (b c| d e) f) -> (a b c| f)

  (+ (x |y z) w)   -> (+ x| w)

Note that to kill only the content and not the enclosing
delimiters you can use \\[universal-argument] \\[sp-kill-sexp].
See `sp-kill-sexp' for more information."
  (interactive "p")
  (while (> arg 0)
    (let ((ok (sp-get-enclosing-sexp 1)))
      (if ok
          (let ((next (sp-get-thing t)))
            (if (sp-compare-sexps next ok)
                (sp-kill-sexp '(16))
              (sp--splice-sexp-do-killing
               (sp-get next :end) ;search backward
               (sp-get ok :beg-in)
               ok 'end)))
        (setq arg -1)))
    (setq arg (1- arg))))

(defun sp-splice-sexp-killing-around (&optional arg)
  "Unwrap the current list and kill everything inside except next expression.

With ARG save that many next expressions.  With ARG negative -N,
save that many expressions backward.

If ARG is raw prefix argument \\[universal-argument] this function behaves exactly
the same as `sp-splice-sexp-killing-backward'.

If ARG is negative raw prefix argument \\[negative-argument] \\[universal-argument] this function
behaves exactly the same as `sp-splice-sexp-killing-forward'.

Note that the behaviour with the prefix argument seems to be
reversed.  This is because the backward variant is much more
common and hence deserve shorter binding.

If ARG is raw prefix argument \\[universal-argument] \\[universal-argument] raise the expression the point
is inside of.  This is the same as `sp-backward-up-sexp' followed by
`sp-splice-sexp-killing-around'.

Examples:

  (a b |(c d) e f)      -> |(c d)     ;; with arg = 1

  (a b |c d e f)        -> |c d       ;; with arg = 2

  (- (car x) |a 3)      -> (car x)|   ;; with arg = -1

  (foo (bar |baz) quux) -> |(bar baz) ;; with arg = \\[universal-argument] \\[universal-argument]"
  (interactive "P")
  (cond
   ((equal arg '(4))
    (sp-splice-sexp-killing-backward 1))
   ((equal arg '(-4))
    (sp-splice-sexp-killing-forward 1))
   (t
    (if (equal arg '(16))
        (progn
          (sp-backward-up-sexp)
          (setq arg 1))
      (setq arg (prefix-numeric-value arg)))
    (let ((ok (sp-get-enclosing-sexp)) str)
      (when ok
        (sp-select-next-thing-exchange arg)
        (sp--splice-sexp-do-killing
         (region-beginning)
         (region-end)
         ok (if (> arg 0) nil 'end)))))))

(defalias 'sp-raise-sexp 'sp-splice-sexp-killing-around)

(defun sp-convolute-sexp (&optional arg)
  "Convolute balanced expressions.

Save the expressions preceding point and delete them.  Then
splice the resulting expression.  Wrap the current enclosing list
with the delimiters of the spliced list and insert the saved
expressions.

With ARG positive N, move up N lists before wrapping.

Examples:

We want to move the `while' before the `let'.

​  (let ((stuff 1)             (while (we-are-good)
​        (other 2))              (let ((stuff 1)
​    (while (we-are-good)  ->          (other 2))
​     |(do-thing 1)               |(do-thing 1)
​      (do-thing 2)                (do-thing 2)
​      (do-thing 3)))              (do-thing 3)))

  (forward-char (sp-get env |:op-l)) -> (sp-get env (forward-char |:op-l))"
  (interactive "p")
  (save-excursion
    (let* ((old-buffer-size (buffer-size))
           (enc (sp-get-enclosing-sexp))
           (inner-close (sp-get enc (delete-and-extract-region
                                     (save-excursion
                                       (goto-char :end-in)
                                       (sp-backward-whitespace))
                                     :end)))
           (inner-raise (sp-get enc (delete-and-extract-region
                                     :beg-prf
                                      (save-excursion
                                        (sp-forward-whitespace)))))
           (whitespace (sp-get enc
                         ;; this happens when the entire inside sexp was removed.
                         (when (= old-buffer-size (+ (buffer-size) :len))
                           (delete-and-extract-region
                            (save-excursion
                              (goto-char :beg-prf)
                              (max (line-beginning-position) (sp-backward-whitespace)))
                            :beg-prf))))
           (encp (sp-get-enclosing-sexp arg)))
      (sp-get encp
        (goto-char :end)
        (insert inner-close)
        (goto-char :beg-prf)
        (insert inner-raise (if whitespace whitespace ""))
        (sp-get (sp-get-enclosing-sexp)
          (indent-region :beg :end)))))
  (indent-according-to-mode))

(defun sp-absorb-sexp (&optional arg)
  "Absorb previous expression.

Save the expressions preceding point and delete them.  Then slurp
an expression backward and insert the saved expressions.

With ARG positive N, absorb that many expressions.

Examples:

​  (do-stuff 1)         (save-excursion
​  (save-excursion  ->   |(do-stuff 1)
​   |(do-stuff 2))        (do-stuff 2))

  foo bar (concat |baz quux) -> (concat |foo bar baz quux) ;; 2"
  (interactive "p")
  (sp-forward-whitespace)
  (let* ((old (point))
         (raise (progn
                  (sp-beginning-of-sexp)
                  (buffer-substring (point) old))))
    (delete-region (point) old)
    (sp-backward-slurp-sexp arg)
    (sp-forward-whitespace)
    (sp-beginning-of-sexp)
    (insert raise)
    (save-excursion
      (sp-backward-up-sexp)
      (indent-sexp)))
  (sp-forward-whitespace))

(defun sp-emit-sexp (&optional arg)
  "Move all expression preceding point except the first one out of the current list.

With ARG positive N, keep that many expressions from the start of
the current list.

This is similar as `sp-backward-barf-sexp' but it also drags the
first N expressions with the delimiter.

Examples:

​  (save-excursion     ​(do-stuff 1)
​    (do-stuff 1)      (do-stuff 2)
​    (do-stuff 2)  ->  (save-excursion
​   |(do-stuff 3))      |(do-stuff 3))

​  (while not-done-yet       (execute-only-once)
​    (execute-only-once) ->  (while not-done-yet    ;; arg = 2
​   |(execute-in-loop))       |(execute-in-loop))"
  (interactive "p")
  (let (save-text)
    (save-excursion
      (sp-beginning-of-sexp)
      (let* ((start (point)))
        (sp-forward-sexp arg)
        (sp-skip-forward-to-symbol t)
        (setq save-text (buffer-substring start (point)))
        (delete-region start (point))))
    (save-excursion (sp-backward-barf-sexp '(4)))
    (sp-down-sexp)
    (insert save-text)
    (save-excursion
      (sp-backward-up-sexp)
      (indent-sexp))))

(defun sp-extract-before-sexp (&optional arg)
  "Move the expression after point before the enclosing balanced expression.

The point moves with the extracted expression.

With ARG positive N, extract N expressions after point.

With ARG negative -N, extract N expressions before point.

With ARG being raw prefix argument \\[universal-argument], extract all the expressions
up until the end of enclosing list.

If the raw prefix is negative, this behaves as \\[universal-argument] `sp-backward-barf-sexp'."
  (interactive "P")
  (if (equal arg '(-4))
      (sp-backward-barf-sexp '(4))
    (sp-select-next-thing arg)
    (let ((enc (sp-get-enclosing-sexp))
          save-text b e nl)
      (save-excursion
        ;; TODO: extract this use pattern into general "get X things
        ;; with or without surrounding whitespace."
        (setq b (region-beginning))
        (setq e (region-end))
        (goto-char (sp-get enc :end-in))
        (if (looking-back "\n[ \t]*")
            (let ((whitespace (sp-get-whitespace)))
              (sp-get whitespace (when (= :beg e)
                                   (delete-region :beg :end))))
          (setq nl t))
        (setq save-text (delete-and-extract-region b e))
        (when nl
          (let ((whitespace (sp-get-whitespace)))
            (sp-get whitespace (delete-region :beg :end))))
        (goto-char (sp-get enc :beg-prf))
        (insert save-text "\n")
        (sp-get enc (indent-region :beg-prf :end)))
      ;; if we're at an empty line, remove it
      (when (string-match-p "^[\n\t ]+\\'" (thing-at-point 'line))
        (let ((b (bounds-of-thing-at-point 'line)))
          (delete-region (car b) (cdr b))))
      (goto-char (sp-get enc :beg-prf)))))

(defun sp-extract-after-sexp (&optional arg)
  "Move the expression after point after the enclosing balanced expression.

The point moves with the extracted expression.

With ARG positive N, extract N expressions after point.

With ARG negative -N, extract N expressions before point.

With ARG being raw prefix argument \\[universal-argument], extract all the
expressions up until the end of enclosing list.

With ARG being negative raw prefix argument \\[negative-argument] \\[universal-argument], extract all the
expressions up until the start of enclosing list."
  ;; this is uch uglier than the "before" version, since the
  ;; calculations forward have to account for the deleted text. Figure
  ;; out a way to make it smoother.
  (interactive "P")
  (sp-select-next-thing arg)
  (let ((enc (sp-get-enclosing-sexp))
        (dws 0) ;length of deleted whitespace
        save-text b e nl)
    (save-excursion
      (setq b (region-beginning))
      (setq e (region-end))
      (goto-char (sp-get enc :end-in))
      (if (looking-back "\n[ \t]*")
          (let ((whitespace (sp-get-whitespace)))
            (sp-get whitespace
              (when (= :beg e)
                (delete-region :beg :end)
                (setq dws (- :end :beg)))))
        (setq nl t))
      (setq save-text (delete-and-extract-region b e))
      (when nl
        (let ((whitespace (sp-get-whitespace)))
          (sp-get whitespace (delete-region :beg :end))
          (sp-get whitespace (setq dws (+ dws (- :end :beg))))))
      (sp-get enc (goto-char (- :end (length save-text) dws)))
      (insert "\n" save-text)
      (sp-get enc (indent-region :beg-prf :end))
      (setq e (point)))
    ;; if we're at an empty line, remove it
    (setq dws 0) ; variable reuse, ugly :/
    (when (string-match-p "^[\n\t ]+\\'" (thing-at-point 'line))
      (let ((b (bounds-of-thing-at-point 'line)))
        (delete-region (car b) (cdr b))
        (setq dws (- (cdr b) (car b)))))
    (when (sp--looking-back (sp--get-opening-regexp) nil t)
      (let ((whitespace (sp-get-whitespace)))
        (sp-get whitespace
          (delete-region :beg :end)
          (setq dws (- :end :beg)))))
    (goto-char (- e dws))))

(defun sp-forward-whitespace (&optional arg)
  "Skip forward past the whitespace characters.
With non-nil ARG return number of characters skipped."
  (interactive "P")
  (let ((rel-move (skip-chars-forward " \t\n")))
    (if arg rel-move (point))))

(defun sp-backward-whitespace (&optional arg)
  "Skip backward past the whitespace characters.
With non-nil ARG return number of characters skipped."
  (interactive "P")
  (let ((rel-move (skip-chars-backward " \t\n")))
    (if arg rel-move (point))))

(defun sp-split-sexp (arg)
  "Split the list or string the point is on into two.

If ARG is a raw prefix \\[universal-argument] split all the sexps in current expression
in separate lists enclosed with delimiters of the current
expression.

Examples:

  (foo bar |baz quux)   -> (foo bar) |(baz quux)

  \"foo bar |baz quux\"   -> \"foo bar\" |\"baz quux\"

  ([foo |bar baz] quux) -> ([foo] |[bar baz] quux)

  (foo bar| baz quux) -> (foo) (bar|) (baz) (quux) ;; \\[universal-argument]"
  (interactive "P")
  (cond
   ((equal arg '(4))
    (-when-let (items (sp-get-list-items))
      (let ((op (sp-get (car items) :op))
            (cl (sp-get (car items) :cl))
            (beg (sp-get (car items) :beg))
            (end (sp-get (car items) :end)))
        (!cdr items)
        (setq items (nreverse items))
        (save-excursion
          (goto-char end)
          (delete-char (- (length cl)))
          (while items
            (sp-get (car items)
              (goto-char :end)
              (insert cl)
              (goto-char :beg)
              (insert op))
            (!cdr items))
          (goto-char beg)
          (delete-char (length op))))))
   (t
    (-when-let (ok (sp-get-enclosing-sexp 1))
      (forward-char (- (prog1 (sp-backward-whitespace t) (insert (sp-get ok :cl)))))
      (save-excursion (sp-forward-whitespace) (insert (sp-get ok :op)))))))

(defun sp--join-sexp (prev next)
  "Join the expressions PREV and NEXT if they are of the same type.

The expression with smaller :beg is considered the previous one,
so the input order does not actually matter.

Return the information about resulting expression."
  (if (and (sp-compare-sexps prev next equal :op)
           (sp-compare-sexps prev next equal :cl))
      ;; if there's some prefix on the second expression, remove it.
      ;; We do not move it to the first expression, it is assumed
      ;; there's one already
      (progn
        (if (sp-compare-sexps prev next >)
            (let ((tmp prev))
              (setq prev next)
              (setq next tmp)))
        (sp-get next (delete-region :beg-prf :beg-in))
        (sp-get prev (delete-region :end-in :end))
        (list :beg (sp-get prev :beg)
              :end (- (sp-get next (- :end :op-l :prefix-l)) (sp-get prev :cl-l))
              :op (sp-get prev :op)
              :cl (sp-get prev :cl)
              :prefix (sp-get prev :prefix)))
    (sp-message :different-type)))

(defun sp-join-sexp (&optional arg)
  "Join the sexp before and after point if they are of the same type.

If ARG is positive N, join N expressions after the point with the
one before the point.

If ARG is negative -N, join N expressions before the point with
the one after the point.

If ARG is a raw prefix \\[universal-argument] join all the things up until the end
of current expression.

The joining stops at the first expression of different type.

Examples:

  (foo bar) |(baz)                    -> (foo bar |baz)

  (foo) |(bar) (baz)                  -> (foo |bar baz) ;; 2

  [foo] [bar] |[baz]                  -> [foo bar |baz] ;; -2

  (foo bar (baz)| (quux) (blob bluq)) -> (foo bar (baz| quux blob bluq)) ;; \\[universal-argument]"
  (interactive "P")
  (let* ((raw (sp--raw-argument-p arg))
         (arg (prefix-numeric-value arg))
         (n (abs arg))
         (prev (save-excursion (sp-backward-sexp (sp--signum arg))))
         next)
    (save-excursion
      (cond
       ((and raw (= n 4))
        (setq next (sp-forward-sexp (sp--signum arg)))
        (while (cond
                ((> arg 0)
                 (sp-compare-sexps next prev > :beg :end))
                ((< arg 0)
                 (sp-compare-sexps next prev < :end :beg)))
          (setq prev (sp--join-sexp prev next))
          (setq next (sp-forward-sexp (sp--signum arg)))))
       (t (while (> n 0)
            (setq next (sp-forward-sexp (sp--signum arg)))
            (setq prev (sp--join-sexp prev next))
            (setq n (1- n)))))
      prev)))

(defun sp--next-thing-selection (&optional arg point)
  "Return the bounds of selection over next thing.

See `sp-select-next-thing' for the meaning of ARG.

If POINT is non-nil, it is assumed it's a point inside the buffer
from which the selection extends, either forward or backward,
depending on the value of ARG.

The return value has the same format as `sp-get-sexp'.  This does
not necessarily represent a valid balanced expression!"
  (save-excursion
    (let* ((raw (sp--raw-argument-p arg))
           (arg (prefix-numeric-value arg))
           (dir (sp--signum arg))
           (beg point) (end point)
           (op "") (cl "")
           (prefix "")
           (suffix ""))
      (cond
       ;; select up until end of list
       ((and raw (= arg 4))
        (let ((enc (sp-get-enclosing-sexp)))
          (if (not enc)
              (error "No enclosing expression")
            (save-excursion
              (goto-char (sp-get enc :end-in))
              (-when-let (ok (sp-get-thing t))
                (sp-get ok
                  (setq end :end)
                  (setq cl :cl)
                  (setq suffix :suffix))))))
        (unless point
          (-when-let (ok (sp-get-thing))
            (sp-get ok
              (setq beg :beg)
              (setq op :op)
              (setq prefix :prefix)))))
       ;; select up until beg of list
       ((and raw (= arg -4))
        (let ((enc (sp-get-enclosing-sexp)))
          (if (not enc)
              (error "No enclosing expression")
            (save-excursion
              (goto-char (sp-get enc :beg-in))
              (-when-let (ok (sp-get-thing))
                (sp-get ok
                  (setq beg :beg)
                  (setq op :op)
                  (setq prefix :prefix))))))
        (unless point
          (-when-let (ok (sp-get-thing t))
            (sp-get ok
              (setq end :end)
              (setq cl :cl)
              (setq suffix :suffix)))))
       ;; select the enclosing expression
       ((and raw (= (abs arg) 16))
        (let ((enc (sp-get-enclosing-sexp)))
          (if (not enc)
              (error "No enclosing expression")
            (sp-get enc (setq beg :beg) (setq end :end)
                    (setq op :op) (setq cl :cl)
                    (setq prefix :prefix)
                    (setq suffix :suffix)))))
       ;; normal selection, select N expressions
       ((> arg 0)
        (let* ((first (sp-forward-sexp))
               (last first))
          (setq arg (1- arg))
          (setq beg (or point (sp-get first :beg)))
          (while (and (> arg 0) last)
            (setq last (sp-forward-sexp))
            (let ((nb (sp-get last :beg))) (when (< nb beg)
                                             (setq first last)
                                             (setq beg nb)))
            (setq arg (1- arg)))
          (unless (and point (= point beg))
            (sp-get first
              (setq beg :beg)
              (setq op :op)
              (setq prefix :prefix)))
          (sp-get last
            (setq end :end)
            (setq cl :cl)
            (setq suffix :suffix))))
       ;; normal select, select -N expressions
       ((< arg 0)
        (let* ((first (sp-backward-sexp))
               (last first))
          (setq arg (1+ arg))
          (setq end (or point (sp-get first :end)))
          (while (and (< arg 0) last)
            (setq last (sp-backward-sexp))
            (let ((ne (sp-get last :end))) (when (> ne end)
                                             (setq first last)
                                             (setq end ne)))
            (setq arg (1+ arg)))
          (sp-get last
            (setq beg :beg)
            (setq op :op)
            (setq prefix :prefix))
          (unless (and point (= point end))
            (sp-get first
              (setq end :end)
              (setq cl :cl)
              (setq suffix :suffix)))))
       ;; N = 0, select insides
       ((= arg 0)
        (let ((enc (sp-get-enclosing-sexp)))
          (if (not enc)
              (error "No enclosing expression")
            (save-excursion
              (goto-char (sp-get enc :beg-in))
              (-when-let (ok (sp-get-thing))
                (sp-get ok
                  (setq beg :beg)
                  (setq op :op)
                  (setq prefix :prefix))))
            (save-excursion
              (goto-char (sp-get enc :end-in))
              (-when-let (ok (sp-get-thing t))
                (sp-get ok
                  (setq end :end)
                  (setq cl :cl)
                  (setq suffix :suffix))))))))
      (list :beg beg :end end :op op :cl cl :prefix prefix :suffix suffix))))

(defun sp-select-next-thing (&optional arg point)
  "Set active region over next thing as recognized by `sp-get-thing'.

If ARG is positive N, select N expressions forward.

If ARG is negative -N, select N expressions backward.

If ARG is a raw prefix \\[universal-argument] select all the things up until the
end of current expression.

If ARG is a raw prefix \\[universal-argument] \\[universal-argument] select the current expression (as
if doing `sp-backward-up-sexp' followed by
`sp-select-next-thing').

If ARG is number 0 (zero), select all the things inside the
current expression.

If POINT is non-nil, it is assumed it's a point inside the buffer
from which the selection extends, either forward or backward,
depending on the value of ARG.

If the currently active region contains a balanced expression,
following invocation of `sp-select-next-thing' will select the
inside of this expression .  Therefore calling this function
twice with no active region will select the inside of the next
expression.

If the point is right in front of the expression any potential
prefix is ignored.  For example, '|(foo) would only select (foo)
and not include ' in the selection.  If you wish to also select
the prefix, you have to move the point backwards.

With `sp-navigate-consider-symbols' symbols and strings are also
considered balanced expressions."
  (interactive "P")
  (let* ((selection (sp--next-thing-selection arg point))
         (p (point))
         (b (sp-get selection :beg))
         (e (sp-get selection :end))
         contracted)
    ;; if region is active and ready to use, check if this selection
    ;; == old selection.  If so, reselect the insides
    (when (region-active-p)
      (let ((rb (region-beginning))
            (re (region-end)))
        (when (and (sp-get selection
                     (or (= rb :beg)
                         (= rb :beg-prf)))
                   (= re (sp-get selection :end)))
          (sp-get selection
            (setq b :beg-in)
            (setq e :end-in))
          (setq contracted t))))
    ;; if we moved forward check if the old-point was in front of an
    ;; expression and after a prefix. If so, remove the prefix from
    ;; the selection
    (unless (and (> (prefix-numeric-value arg) 0)
                 (not (sp--raw-argument-p arg))
                 (= b p))
      (unless contracted (setq b (sp-get selection :beg-prf))))
    (push-mark b t t)
    (goto-char e)
    selection))

(defun sp-select-previous-thing (&optional arg point)
  "Set active region over ARG previous things as recognized by `sp-get-thing'.

If ARG is negative -N, select that many expressions forward.

With `sp-navigate-consider-symbols' symbols and strings are also
considered balanced expressions."
  (interactive "P")
  (sp-select-next-thing (sp--negate-argument arg) point))

(defun sp-select-next-thing-exchange (&optional arg point)
  "Just like `sp-select-next-thing' but run `exchange-point-and-mark' afterwards."
  (interactive "P")
  (prog1
      (sp-select-next-thing arg point)
    (exchange-point-and-mark)))

(defun sp-select-previous-thing-exchange (&optional arg point)
  "Just like `sp-select-previous-thing' but run `exchange-point-and-mark' afterwards."
  (interactive "P")
  (prog1
      (sp-select-previous-thing arg point)
    (exchange-point-and-mark)))

(defun sp-delete-char (&optional arg)
  "Delete a character forward or move forward over a delimiter.

If on an opening delimiter, move forward into balanced expression.

If on a closing delimiter, refuse to delete unless the balanced
expression is empty, in which case delete the entire expression.

If the delimiter does not form a balanced expression, it will be
deleted normally.

With a numeric prefix argument N > 0, delete N characters forward.

With a numeric prefix argument N < 0, delete N characters backward.

With a numeric prefix argument N = 0, simply delete a character
forward, without regard for delimiter balancing.

If ARG is raw prefix argument \\[universal-argument], delete
characters forward until a closing delimiter whose deletion would
break the proper pairing is hit.

Examples:

 (quu|x \"zot\") -> (quu| \"zot\")

 (quux |\"zot\") -> (quux \"|zot\") -> (quux \"|ot\")

 (foo (|) bar) -> (foo | bar)

 |(foo bar) -> (|foo bar)"
  (interactive "P")
  (let* ((raw (sp--raw-argument-p arg))
         ;; if you edit 10 gigabyte files in Emacs, you're gonna have
         ;; a bad time.
         (n (if raw 100000000
              (prefix-numeric-value arg))))
    (cond
     ((> n 0)
      (while (> n 0)
        (cond
         ((let ((ok (sp-point-in-empty-sexp)))
            (when ok
              (backward-char (length (car ok)))
              (delete-char (+ (length (car ok)) (length (cdr ok)))))
            ok)
          ;; make this customizable
          (setq n (1- n)))
         ((and (sp-point-in-string)
               (save-excursion (forward-char) (not (sp-point-in-string))))
          (setq n 0))
         ((sp--looking-at (sp--get-opening-regexp (sp--get-pair-list-context 'navigate)))
          (if (save-match-data (sp-get-thing))
              (goto-char (match-end 0))
            (delete-char (length (match-string 0))))
          ;; make this customizable
          (setq n (1- n)))
         ((and (not (sp-point-in-string))
               (save-excursion (forward-char) (sp-point-in-string)))
          (forward-char)
          ;; make this customizable
          (setq n (1- n)))
         ((sp--looking-at (sp--get-closing-regexp (sp--get-pair-list-context 'navigate)))
          (if (save-match-data (sp-get-thing))
              ;; make this customizable -- maybe we want to skip and
              ;; continue deleting
              (setq n 0)
            (delete-char (length (match-string 0)))
            (setq n (1- n))))
         (t
          (delete-char 1)
          (setq n (1- n))))))
     ((= n 0) (delete-char 1))
     (t (sp-backward-delete-char (sp--negate-argument arg))))))

(defun sp-backward-delete-char (&optional arg)
  "Delete a character backward or move backward over a delimiter.

If on a closing delimiter, move backward into balanced expression.

If on a opening delimiter, refuse to delete unless the balanced
expression is empty, in which case delete the entire expression.

If the delimiter does not form a balanced expression, it will be
deleted normally.

With a numeric prefix argument N > 0, delete N characters backward.

With a numeric prefix argument N < 0, delete N characters forward.

With a numeric prefix argument N = 0, simply delete a character
backward, without regard for delimiter balancing.

If ARG is raw prefix argument \\[universal-argument], delete
characters backward until a opening delimiter whose deletion would
break the proper pairing is hit.

Examples:

 (\"zot\" q|uux) -> (\"zot\" |uux)

 (\"zot\"| quux) -> (\"zot|\" quux) -> (\"zo|\" quux)

 (foo (|) bar) -> (foo | bar)

 (foo bar)| -> (foo bar|)"
  (interactive "P")
  (let* ((raw (sp--raw-argument-p arg))
         ;; if you edit 10 gigabyte files in Emacs, you're gonna have
         ;; a bad time.
         (n (if raw 100000000
              (prefix-numeric-value arg))))
    (cond
     ((> n 0)
      (while (> n 0)
        (cond
         ((let ((ok (sp-point-in-empty-sexp)))
            (when ok
              (backward-char (length (car ok)))
              (delete-char (+ (length (car ok)) (length (cdr ok)))))
            ok)
          ;; make this customizable
          (setq n (1- n)))
         ((and (sp-point-in-string)
               (save-excursion (backward-char) (not (sp-point-in-string))))
          (setq n 0))
         ((sp--looking-back (sp--get-closing-regexp (sp--get-pair-list-context 'navigate)))
          (if (save-match-data (sp-get-thing t))
              (goto-char (match-beginning 0))
            (delete-char (- (length (match-string 0)))))
          ;; make this customizable
          (setq n (1- n)))
         ((and (not (sp-point-in-string))
               (save-excursion (backward-char) (sp-point-in-string)))
          (backward-char)
          ;; make this customizable
          (setq n (1- n)))
         ((sp--looking-back (sp--get-opening-regexp (sp--get-pair-list-context 'navigate)))
          (if (save-match-data (sp-get-thing t))
              ;; make this customizable -- maybe we want to skip and
              ;; continue deleting
              (setq n 0)
            (delete-char (- (length (match-string 0))))
            (setq n (1- n))))
         (t
          (delete-char -1)
          (setq n (1- n))))))
     ((= n 0) (delete-char -1))
     (t (sp-delete-char (sp--negate-argument arg))))))

(put 'sp-backward-delete-char 'delete-selection 'supersede)
(put 'sp-delete-char 'delete-selection 'supersede)

(defun sp-point-in-empty-sexp (&optional pos)
  "Return non-nil if point is in empty sexp or string.

The return value is active cons pair of opening and closing sexp
delimiter enclosing this sexp."
  (setq pos (or pos (point)))
  (let (op act)
    (cond
     ((sp--looking-back (sp--get-opening-regexp (sp--get-pair-list-context 'navigate)))
      (setq op (match-string 0))
      (setq act (--first (equal (car it) op) sp-pair-list))
      (when (sp--looking-at (regexp-quote (cdr act))) act))
     ((sp-point-in-empty-string pos)))))

(defun sp-point-in-empty-string (&optional pos)
  "Return non-nil if point is in empty sexp or string.

The return value is actually cons pair of opening and closing
string delimiter enclosing this string."
  (setq pos (or pos (point)))
  (when (and (sp-point-in-string)
             (save-excursion (forward-char) (not (sp-point-in-string)))
             (save-excursion (backward-char) (not (sp-point-in-string))))
    (save-excursion
      (let ((c (char-to-string (nth 3 (syntax-ppss pos)))))
        (cons c c)))))

(defun sp-zap-syntax (syntax &optional back)
  "Delete characters forward until they match syntax class SYNTAX.

If BACK is non-nil, delete backward."
  (let ((p (point)))
    (if back
        (skip-syntax-backward syntax)
      (skip-syntax-forward syntax))
    (delete-region p (point))))

(defun sp--use-subword ()
  "Return non-nil if word killing commands should kill subwords.
This is the case if `subword-mode' is enabled and
`sp-use-subword' is non-nil."
  (and sp-use-subword (bound-and-true-p subword-mode)))

(declare-function subword-kill "subword")
(declare-function subword-forward "subword")
(declare-function subword-backward "subword")

(defun sp--kill-word (&optional n)
  "Kill N words or subwords."
  (let ((n (or n 1)))
    (if (sp--use-subword)
        (subword-kill n)
      (kill-word n))))

(defun sp--forward-word (&optional n)
  "Move forward N words or subwords."
  (let ((n (or n 1)))
    (if (sp--use-subword)
        (subword-forward n)
      (forward-word n))))

(defun sp--backward-word (&optional n)
  "Move backward N words or subwords."
  (let ((n (or n 1)))
    (if (sp--use-subword)
        (subword-backward n)
      (backward-word n))))

(defun sp-kill-symbol (&optional arg word)
  "Kill a symbol forward, skipping over any intervening delimiters.

With ARG being positive number N, repeat that many times.

With ARG being Negative number -N, repeat that many times in
backward direction.

See `sp-forward-symbol' for what constitutes a symbol."
  (interactive "p")
  (if (> arg 0)
      (while (> arg 0)
        (if (and word (sp-point-in-symbol))
            (sp--kill-word 1)
          (let ((s (sp-get-symbol))
                (p (point)))
            (when s
              (sp-get s
                (let ((delims (buffer-substring :beg-prf p)))
                  (if (string-match-p "\\`\\(\\s.\\|\\s-\\)*\\'" delims)
                      (if word
                          (kill-region p (save-excursion (sp--forward-word) (point)))
                        (kill-region p :end))
                    (let ((kill-from (if (> p :beg-prf) :beg :beg-prf)))
                      (goto-char kill-from)
                      (if word
                          (kill-region kill-from (save-excursion (sp--forward-word) (point)))
                        (kill-region kill-from :end)))))))))
        (sp--cleanup-after-kill)
        (setq arg (1- arg)))
    (sp-backward-kill-symbol (sp--negate-argument arg) word)))

(defun sp-kill-word (&optional arg)
  "Kill a word forward, skipping over intervening delimiters.

With ARG being positive number N, repeat that many times.

With ARG being Negative number -N, repeat that many times in
backward direction."
  (interactive "p")
  (sp-kill-symbol arg t))

(defun sp-backward-kill-symbol (&optional arg word)
  "Kill a symbol backward, skipping over any intervening delimiters.

With ARG being positive number N, repeat that many times.

With ARG being Negative number -N, repeat that many times in
forward direction.

See `sp-backward-symbol' for what constitutes a symbol."
  (interactive "p")
  (if (> arg 0)
      (while (> arg 0)
        (if (and word (sp-point-in-symbol))
            (sp--kill-word -1)
          (let ((s (sp-get-symbol t))
                (p (point)))
            (when s
              (sp-get s
                (let ((delims (buffer-substring :end p)))
                  (if (string-match-p "\\`\\(\\s.\\|\\s-\\)*\\'" delims)
                      (if word
                          (kill-region (save-excursion (sp--backward-word) (point)) p)
                        (kill-region :beg-prf p))
                    (goto-char :end)
                    (if word
                        (kill-region (save-excursion (sp--backward-word) (point)) :end)
                      (kill-region :beg-prf :end))))))))
        (sp--cleanup-after-kill)
        (setq arg (1- arg)))
    (sp-kill-symbol (sp--negate-argument arg) word)))

(defun sp-backward-kill-word (&optional arg)
  "Kill a word backward, skipping over intervening delimiters.

With ARG being positive number N, repeat that many times.

With ARG being Negative number -N, repeat that many times in
backward direction."
  (interactive "p")
  (sp-backward-kill-symbol arg t))

(defun sp-indent-defun (arg)
  "Reindent the current defun.

If point is inside a string or comment, fill the current
paragraph instead, and with ARG, justify as well.

Otherwise, reindent the current defun, and adjust the position
of the point."
  (interactive "P")
  (if (sp-point-in-string-or-comment)
      (fill-paragraph arg)
    (let ((column (current-column))
          (indentation (sp--current-indentation)))
      (save-excursion
        (end-of-defun)
        (beginning-of-defun)
        (indent-sexp))
      (sp--back-to-indentation column indentation))))

(defun sp-region-ok-p (start end)
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (let ((r t))
        (while (and r (not (eobp)))
          (setq r (sp-forward-sexp)))
        r))))

(defun sp-newline ()
  "Insert a newline and indent it.

This is like `newline-and-indent', but it not only indents the
line that the point is on but also the S-expression following the
point, if there is one.

If in a string, just insert a literal newline.

If in a comment and if followed by invalid structure, call
`indent-new-comment-line' to keep the invalid structure in a
comment."
  (interactive)
  (cond
   ((sp-point-in-string)
    (newline))
   ((sp-point-in-comment)
    (if (sp-region-ok-p (point) (point-at-eol))
        (progn (newline-and-indent) (ignore-errors (indent-sexp)))
      (indent-new-comment-line)))
   (t
    (newline-and-indent)
    (ignore-errors (indent-sexp)))))

(defun sp-comment ()
  "Insert the comment character and adjust hanging sexps such
  that it doesn't break structure."
  (interactive)
  (if (sp-point-in-comment)
      (when (= 1 (length (single-key-description last-command-event))) ;; pretty hacky
        (insert (single-key-description last-command-event)))
    (let ((old-point (point))
          (column (current-column))
          (indentation (sp--current-indentation))
          (old-line (line-number-at-pos))
          (hsexp (sp-get-hybrid-sexp))
          (newline-inserted 0))
      (goto-char (sp-get hsexp :end))
      (if (sp--looking-at (sp--get-closing-regexp))
          (progn
            (newline)
            (setq newline-inserted (1+ (- (line-end-position) (point)))))
        (when (/= old-line (line-number-at-pos))
          (sp-backward-sexp)
          (newline)
          (setq newline-inserted (- (line-end-position) (point)))))
      ;; @{ indenting madness
      (goto-char old-point)
      (sp-get hsexp (indent-region :beg (+ :end newline-inserted)))
      (sp--back-to-indentation column indentation)
      ;; @}
      (let ((comment-delim (or (cdr (--first (memq major-mode (car it)) sp-comment-string))
                               comment-start)))
        (insert comment-delim)
        (when (/= newline-inserted 0)
          (save-excursion
            (forward-line 1)
            (indent-according-to-mode)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; show-smartparens-mode

(defgroup show-smartparens nil
  "Show smartparens minor mode."
  :group 'smartparens)

(defcustom sp-show-pair-delay 0.125
  "Time in seconds to delay before showing a matching pair."
  :type '(number :tag "seconds")
  :group 'show-smartparens)

(defcustom sp-show-enclosing-pair-commands '(
                                             sp-show-enclosing-pair
                                             sp-forward-slurp-sexp
                                             sp-backward-slurp-sexp
                                             sp-forward-barf-sexp
                                             sp-backward-barf-sexp
                                             )
  "List of commands after which the enclosing pair is highlighted.

After the next command the pair will automatically disappear."
  :type '(repeat symbol)
  :group 'show-smartparens)

(defcustom sp-show-pair-from-inside nil
  "If non-nil, highlight the enclosing pair if immediately after
the opening delimiter or before the closing delimiter."
  :type 'boolean
  :group 'show-smartparens)

(defface sp-show-pair-match-face
  '((((class color) (background light))
     :background "turquoise")       ; looks OK on tty (becomes cyan)
    (((class color) (background dark))
     :background "steelblue3")      ; looks OK on tty (becomes blue)
    (((background dark))
     :background "grey50")
    (t
     :background "gray"))
  "`show-smartparens-mode' face used for a matching pair."
  :group 'show-smartparens)

(defface sp-show-pair-mismatch-face
  '((((class color)) (:foreground "white" :background "purple"))
    (t (:inverse-video t)))
  "`show-smartparens-mode' face used for a mismatching pair."
  :group 'show-smartparens)

(defface sp-show-pair-enclosing
  '((t (:inherit highlight)))
  "The face used to highlight pair overlays."
  :group 'show-smartparens)

(defvar sp-show-pair-idle-timer nil)

(defvar sp-show-pair-overlays nil)

(defvar sp-show-pair-enc-overlays nil)

;;;###autoload
(define-minor-mode show-smartparens-mode
  "Toggle visualization of matching pairs.  When enabled, any
matching pair is highlighted after `sp-show-pair-delay' seconds
of Emacs idle time if the point is immediately in front or after
a pair.  This mode works similarly to `show-paren-mode', but
support custom pairs."
  :init-value nil
  :group 'show-smartparens
  (if show-smartparens-mode
      (unless sp-show-pair-idle-timer
        (setq sp-show-pair-idle-timer
              (run-with-idle-timer sp-show-pair-delay t
                                   'sp-show--pair-function)))
    (when sp-show-pair-overlays
      (sp-show--pair-delete-overlays))))

;;;###autoload
(define-globalized-minor-mode show-smartparens-global-mode
  show-smartparens-mode
  turn-on-show-smartparens-mode)

;;;###autoload
(defun turn-on-show-smartparens-mode ()
  "Turn on `show-smartparens-mode'."
  (interactive)
  (unless (or (member major-mode sp-ignore-modes-list)
              (and (not (derived-mode-p 'comint-mode))
                   (eq (get major-mode 'mode-class) 'special)))
    (show-smartparens-mode t)))

;;;###autoload
(defun turn-off-show-smartparens-mode ()
  "Turn off `show-smartparens-mode'."
  (interactive)
  (show-smartparens-mode -1))

(defun sp-show-enclosing-pair ()
  "Highlight the enclosing pair around point."
  (interactive))

(defun sp-highlight-current-sexp (arg)
  "Highlight the expression returned by the next command, preserving point position."
  (interactive "P")
  (let* ((cmd (read-key-sequence "" t))
         (com (key-binding cmd)))
    (if (commandp com)
        (save-excursion
          (let ((ok (call-interactively com)))
            (sp-show--pair-enc-function ok)))
      (execute-kbd-macro cmd))))

(defun sp-show--pair-function ()
  "Display the show pair overlays."
  (when show-smartparens-mode
    (save-match-data
      (cl-labels ((create-forward
                   (match)
                   ;; we can use `sp-get-thing' here because we *are* at some
                   ;; pair opening, and so only the tag or the sexp can trigger.
                   (-if-let (ok (sp-get-thing))
                       (sp-get ok (sp-show--pair-create-overlays :beg :end :op-l :cl-l))
                     (sp-show--pair-create-mismatch-overlay (point) (length match))))
                  (create-backward
                   (match)
                   (-if-let (ok (sp-get-thing t))
                       (sp-get ok (sp-show--pair-create-overlays :beg :end :op-l :cl-l))
                     (sp-show--pair-create-mismatch-overlay (- (point) (length match))
                                                            (length match)))))
        (let* ((pair-list (sp--get-allowed-pair-list))
               (opening (sp--get-opening-regexp pair-list))
               (closing (sp--get-closing-regexp pair-list))
               (allowed (and sp-show-pair-from-inside (sp--get-allowed-regexp)))
               match)
          (cond
           ;; if we are in a situation "()|", we should highlight the
           ;; regular pair and not the string pair "from inside"
           ((and (not (sp--evil-normal-state-p))
                 (not (sp--evil-visual-state-p))
                 (sp--looking-back (if sp-show-pair-from-inside allowed closing)))
            (create-backward (match-string 0)))
           ((or (sp--looking-at (if sp-show-pair-from-inside allowed opening))
                (and (memq major-mode sp-navigate-consider-stringlike-sexp)
                     (looking-at (sp--get-stringlike-regexp)))
                (and (memq major-mode sp-navigate-consider-sgml-tags)
                     (looking-at "<")))
            (create-forward (match-string 0)))
           ((or (sp--looking-back (if sp-show-pair-from-inside allowed closing))
                (and (memq major-mode sp-navigate-consider-stringlike-sexp)
                     (sp--looking-back (sp--get-stringlike-regexp)))
                (and (memq major-mode sp-navigate-consider-sgml-tags)
                     (sp--looking-back ">")))
            (create-backward (match-string 0)))
           (sp-show-pair-overlays
            (sp-show--pair-delete-overlays))))))))

(defun sp-show--pair-enc-function (&optional thing)
  "Display the show pair overlays for enclosing expression."
  (when show-smartparens-mode
    (-when-let (enc (or thing (sp-get-enclosing-sexp)))
      (sp-get enc (sp-show--pair-create-enc-overlays :beg :end :op-l :cl-l)))))

(defun sp-show--pair-create-overlays (start end olen clen)
  "Create the show pair overlays."
  (when sp-show-pair-overlays
    (sp-show--pair-delete-overlays))
  (let* ((oleft (make-overlay start (+ start olen) nil t nil))
         (oright (make-overlay (- end clen) end nil t nil)))
    (setq sp-show-pair-overlays (cons oleft oright))
    (overlay-put oleft 'face 'sp-show-pair-match-face)
    (overlay-put oright 'face 'sp-show-pair-match-face)
    (overlay-put oleft 'priority 1000)
    (overlay-put oright 'priority 1000)
    (overlay-put oleft 'type 'show-pair)))

(defun sp-show--pair-create-enc-overlays (start end olen clen)
  "Create the show pair enclosing overlays"
  (when sp-show-pair-enc-overlays
    (sp-show--pair-delete-enc-overlays))
  (let* ((oleft (make-overlay start (+ start olen) nil t nil))
         (oright (make-overlay (- end clen) end nil t nil)))
    (setq sp-show-pair-enc-overlays (cons oleft oright))
    (overlay-put oleft 'face 'sp-show-pair-enclosing)
    (overlay-put oright 'face 'sp-show-pair-enclosing)
    (overlay-put oleft 'priority 1000)
    (overlay-put oright 'priority 1000)
    (overlay-put oleft 'type 'show-pair-enc)))

(defun sp-show--pair-create-mismatch-overlay (start len)
  "Create the mismatch pair overlay."
  (when sp-show-pair-overlays
    (sp-show--pair-delete-overlays))
  (let ((o (make-overlay start (+ start len) nil t nil)))
    (setq sp-show-pair-overlays (cons o nil))
    (overlay-put o 'face 'sp-show-pair-mismatch-face)
    (overlay-put o 'priority 1000)
    (overlay-put o 'type 'show-pair)))

(defun sp-show--pair-delete-overlays ()
  "Remove both show pair overlays."
  (when sp-show-pair-overlays
    (when (car sp-show-pair-overlays)
      (delete-overlay (car sp-show-pair-overlays)))
    (when (cdr sp-show-pair-overlays)
      (delete-overlay (cdr sp-show-pair-overlays)))
    (setq sp-show-pair-overlays nil)))

(defun sp-show--pair-delete-enc-overlays ()
  "Remove both show pair enclosing overlays."
  (when sp-show-pair-enc-overlays
    (when (car sp-show-pair-enc-overlays)
      (delete-overlay (car sp-show-pair-enc-overlays)))
    (when (cdr sp-show-pair-enc-overlays)
      (delete-overlay (cdr sp-show-pair-enc-overlays)))
    (setq sp-show-pair-enc-overlays nil)))


;; global initialization
(sp--update-trigger-keys)
(defadvice delete-backward-char (before sp-delete-pair-advice activate)
  (save-match-data
    (sp-delete-pair (ad-get-arg 0))))
(defadvice haskell-indentation-delete-backward-char (before sp-delete-pair-advice activate)
  (save-match-data
    (sp-delete-pair (ad-get-arg 0))))
(add-hook 'post-command-hook 'sp--post-command-hook-handler)
(add-hook 'pre-command-hook 'sp--pre-command-hook-handler)
(sp--set-base-key-bindings)
(sp--update-override-key-bindings)

(defadvice ac-complete (after sp-auto-complete-advice activate)
  "If `smartparens-mode' is active, we check if the completed string
has a pair definition.  If so, we insert the closing pair."
  (when (and smartparens-mode ad-return-value) ; `ac-complete' returns nil if there are no completion candidates.
    (setq sp-recent-keys (reverse (split-string ad-return-value "")))
    (sp-insert-pair))
  ad-return-value)

(defadvice company--insert-candidate (after sp-company--insert-candidate activate)
  "If `smartparens-mode' is active, we check if the completed string
has a pair definition.  If so, we insert the closing pair."
  (when smartparens-mode
    (setq sp-recent-keys (reverse (split-string (ad-get-arg 0) "")))
    (sp-insert-pair))
  ad-return-value)

(defadvice hippie-expand (after sp-auto-complete-advice activate)
  (when smartparens-mode
    (setq sp-recent-keys (reverse (split-string (buffer-substring-no-properties he-string-beg he-string-end) "")))
    (sp-insert-pair)))

(defvar sp--mc/cursor-specific-vars
  '(
    sp-wrap-point
    sp-wrap-mark
    sp-last-wrapped-region
    sp-pair-overlay-list
    sp-wrap-overlays
    sp-wrap-tag-overlays
    sp-last-operation
    sp-previous-point
    )
  "A list of vars that need to be tracked on a per-cursor basis.")

(defvar mc/cursor-specific-vars)
(eval-after-load 'multiple-cursors
  '(dolist (it sp--mc/cursor-specific-vars)
     (add-to-list 'mc/cursor-specific-vars it)))

(provide 'smartparens)

;; Local Variables:
;; coding: utf-8
;; eval: (font-lock-add-keywords nil `((,(concat "(" (regexp-opt '("sp-do-move-op" "sp-do-move-cl" "sp-do-put-op" "sp-do-put-cl" "sp-do-del-op" "sp-do-del-cl") t) "\\_>") 1 'font-lock-variable-name-face)))
;; End:

;;; smartparens.el ends here
