;;; smartparens.el --- Autoinsert pairs of defined brackets and wrap regions

;; Copyright (C) 2012 Matus Goljer

;; Author: Matus Goljer <matus.goljer@gmail.com>
;; Maintainer: Matus Goljer <matus.goljer@gmail.com>
;; Created: 17 Nov 2012
;; Version: 1.2
;; Keywords: abbrev convenience editing
;; Package-Requires: ((dash "1.0.2"))
;; URL: https://github.com/Fuco1/smartparens

;; This file is not part of GNU Emacs.

;;; License:

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

;; See github readme at https://github.com/Fuco1/smartparens

;;; Code:

(require 'dash)
(eval-when-compile (require 'cl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables

;;;###autoload
(defvar sp-keymap (make-sparse-keymap)
  "Keymap used for smartparens-mode.  Remaps all the trigger keys
to `self-insert-command'.  This means we lose some functionality
in some modes (like c-electric keys).")

(defvar sp-escape-char nil
  "Character used to escape quotes inside strings.")
(make-variable-buffer-local 'sp-escape-char)

(defvar sp-local-ban-insert-pair '()
  "For pairs on this list auto insertion is locally disabled in
specified modes.

List of elements of type (command . '(list of modes)).")

(defvar sp-global-ban-insert-pair '()
  "For pairs on this list auto insertion is disabled globally.

List of pair IDs.")

(defvar sp-local-allow-insert-pair '()
  "For pairs on this list auto insertion is locally enabled in
specified modes.  It is disabled in all other modes automatically.

List of elements of type (command . '(list of modes)).")

(defvar sp-local-ban-insert-pair-in-string '()
  "For pairs on this list auto insertion is locally disabled in
specific modes if the point is inside string, docstring or
comment.

List of elements of type (command . '(list of modes)).")

(defvar sp-global-ban-insert-pair-in-string '()
  "For pairs on this list auto insertion is disabled globally if
the point is inside string, docstring or comment.

List of pair IDs.")

(defvar sp-local-allow-insert-pair-in-string '()
  "For pairs on this list auto insertion is locally enabled in
specific modes if the point is inside string, docstring or
comment.  It is disabled in all other modes automatically.

List of elements of type (command . '(list of modes)).")

(defvar sp-local-ban-insert-pair-in-code '()
  "For pairs on this list auto insertion is locally disabled in
specific modes if the point is inside code.

List of elements of type (command . '(list of modes)).")

(defvar sp-global-ban-insert-pair-in-code '()
  "For pairs on this list auto insertion is disabled globally if
the point is inside code.

List of pair IDs.")

(defvar sp-local-allow-insert-pair-in-code '()
  "For pairs on this list auto insertion is locally enabled in
specific modes if the point is inside code.  It is disabled in all
other modes automatically.

List of elements of type (command . '(list of modes)).")

(defvar sp-pair-list '(
                       ("\\\\(" . "\\\\)") ;; emacs regexp parens
                       ("\\{"   . "\\}")
                       ("\\("   . "\\)")
                       ("\\\""  . "\\\"")
                       ("/*"    . "*/")
                       ("\""    . "\"")
                       ("'"     . "'")
                       ("("     . ")")
                       ("["     . "]")
                       ("{"     . "}")
                       ("`"     . "'") ;; tap twice for tex double quotes
                       )
  "List of pairs for auto-insertion or wrapping.  Maximum length
of opening or closing pair is `sp-max-pair-length-c' characters.")
(make-variable-buffer-local 'sp-pair-list)

(defvar sp-local-pair-list '()
  "List of pairs specific to a specific mode.  The pairs on this list
are not enabled globally.  Pairs in this list can override global
definitons.  For example default `' can be overriden with `` in
`markdown-mode'.

List of elements of type ('(open . close) modes).")

(defvar sp-tag-pair-list '(
                           ("<" . (
                                   ((sgml-mode html-mode) "<_>" "</_>" sp-match-sgml-tags)
                                   ))
                           ("\\b" . (
                                     ((latex-mode tex-mode) "\\begin{_}" "\\end{_}" identity)
                                     ))
                           )

  "List of tag pairs.  Some languages use more elaborate tag pairs,
such as html \"<span class=\"x\">something</span>\".  For these,
the standard wrap mechanism isn't sufficient.  Here, user can
define the syntax of opening and closing pair.  If they share the
same keyword, this is substituted for _ in the tag
definitions.  The transform-function can further transform the
substitution for closing tab, for example cutting out everything
after a space in html-tag.  Only one _ per tag is allowed.  If no _
is present in the closing tag, nothing is mirrored there.  If no _
is present in the opening tag, tag insertion mode is not entered
and the tags are simply inserted as text.

The internal format is a list of (open-trigger ((modes) open-tag
close-tag transform-funciton)) where:

open-trigger - the trigger that starts the tag insertion.

open-tag - format of the opening tag.  _ is replaced by the entered
text.

close-tag - format of the closing tag.  _ is replaced by the entered
text.

modes - list of modes where this wrapping is allowed.

transform-function - this function is called to perform a
transformation on text that will be substituted to closing tag.")

(defvar sp-last-operation nil
  "Symbol holding the last successful operation.")
(make-variable-buffer-local 'sp-last-operation)

(defvar sp-previous-point -1
  "Location of point before last command.  This is only updated
when some pair-overlay is active.  Do not rely on the value of
this variable anywhere else!")
(make-variable-buffer-local 'sp-previous-point)

(defvar sp-wrap-point nil
  "Save the value of point before attemt to wrap a region.  Used
for restoring the original state if the wrapping is
cancelled.")
(make-variable-buffer-local 'sp-wrap-point)

(defvar sp-wrap-mark nil
  "Save the value of point before attemt to wrap a region.  Used
for restoring the original state if the wrapping is
cancelled.")
(make-variable-buffer-local 'sp-wrap-mark)

(defvar sp-last-inserted-characters ""
  "If wrapping is cancelled, these characters are re-inserted to
the location of point before the wrapping.")
(make-variable-buffer-local 'sp-last-inserted-characters)

(defvar sp-last-wrapped-region nil
  "List containing info about last wrapped region.  The content of the list is:
\(start-of-the-wrapped-region end-of-the-wrapped-region
length-of-opening-pair length-of-closing-pair\).  Start and end
positions include the newly added wrapping pair.")
(make-variable-buffer-local 'sp-last-wrapped-region)

(defvar sp-point-inside-string nil
  "t if point is inside a string.")

(defconst sp-max-pair-length-c 10
  "Specifies the maximum length of an opening or closing
delimiter of a pair managed by `sp-add-pair'.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customize & Mode definitions

(defgroup smartparens ()
  "Smartparens minor mode."
  :group 'editor)

;;;###autoload
(define-minor-mode smartparens-mode
  "Toggle smartparens mode"
  :init-value nil
  :lighter " SP"
  :group 'smartparens
  :keymap sp-keymap
  (if smartparens-mode
      (progn
        ;; setup local pair replacements
        (sp-update-local-pairs)
        (sp-update-pair-triggers)
        ;; set the escape char
        (dotimes (char 256)
          (unless sp-escape-char
            (if (= ?\\ (char-syntax char))
                (setq sp-escape-char (string char)))))
        (when (sp-delete-selection-p)
          (sp-init-delete-selection-mode-emulation))
        (run-hooks 'smartparens-enabled-hook))
    (run-hooks 'smartparens-disabled-hook)))

(defun sp-get-trigger-keys (pair-list)
  "Return a list of unique trigger keys for pairs in a format of
`sp-pair-list'.

For example, for list (\"\\\\(\",\"\\}\"), (\"\\\",
\"(\", \"}\") is returned."
  (-distinct (split-string
              (apply #'concat
                     (--reduce-from (cons (car it) (cons (cdr it) acc))
                                    nil pair-list))
              "" t)))

(defun sp-update-pair-triggers (&optional open)
  "Update the `sp-keymap' to include all trigger keys.  Trigger
key is any character present in any pair.  Each trigger key must
map to `self-insert-command'.

The optional argument OPEN is a key to remove.  If non-nil,
remove the trigger keys defined by this key and then re-insert
the rest from `sp-pair-list'."
  ;; remove all the bindings to the `sp-self-insert-command' from
  ;; trigger keys in open
  (when open
    (--each (split-string open "" t)
      (define-key sp-keymap it nil)))
  ;; update the bindings
  (let ((triggers (sp-get-trigger-keys (default-value 'sp-pair-list))))
    (--each triggers (define-key sp-keymap it 'sp-self-insert-command))))

(defun sp-self-insert-command (arg)
  "This command is a wrapper around `self-insert-command'.  If
the just-typed key is a possible trigger for any pair,
`self-insert-command' is called and the special behaviours are
handled in its advice provided by `smartparens-mode'.  If the
just-typed key is not a trigger, fall back to the commant that
would execute if smartparens-mode were disabled."
  (interactive "p")
  (let ((triggers (sp-get-trigger-keys
                   (--filter (sp-insert-pair-p
                              (car it) major-mode) sp-pair-list))))
    (if (member (single-key-description last-command-event) triggers)
        (progn
          (setq this-command 'self-insert-command)
          (self-insert-command arg))
      (let ((com (sp-keybinding-fallback-1)))
        (setq this-original-command com)
        (call-interactively com)))
    ))

(defun sp-keybinding-fallback-1 ()
  "Internal.  Return the fall-back command as if
`smartparens-mode' were disabled."
  (let ((smartparens-mode nil)
        (keys (this-single-command-keys)))
    (key-binding keys t)))

;; TODO: we could remove pairs that are absolutely not allowed in this
;; mode here (those that are banned or only allowed elsewhere).  It
;; will save a lot on the various filtering tasks.  However, this will
;; be an issue only with hundreds of pairs
(defun sp-update-local-pairs ()
  "Update local pairs after removal or at mode initialization."
  (setq sp-pair-list (default-value 'sp-pair-list))
  (--each sp-local-pair-list
    (when (member major-mode (cdr it))
      (let ((open (caar it))
            (close (cdar it)))
        (setq sp-pair-list
              (--remove (equal open (car it)) sp-pair-list))
        (setq sp-pair-list
              (sp-add-to-ordered-list (car it) sp-pair-list #'sp-order-pairs))))))

(defun sp-update-local-pairs-everywhere ()
  "Run `sp-update-local-pairs' in all buffers.  This is necessary
to update all the buffer-local definitions."
  (--each (buffer-list)
    (with-current-buffer it
      (when smartparens-mode
        (sp-update-local-pairs)))))

(defvar smartparens-enabled-hook nil
  "Called after `smartparens-mode' is turned on.")

(defvar smartparens-disabled-hook nil
  "Called after `smartparens-mode' is turned off.")

;; global custom
(defcustom sp-ignore-modes-list '(
                                  calc-mode
                                  dired-mode
                                  gnus-article-mode
                                  gnus-group-mode
                                  gnus-summary-mode
                                  ibuffer-mode
                                  magit-branch-manager-mode
                                  magit-commit-mode
                                  magit-diff-mode
                                  magit-key-mode
                                  magit-log-mode
                                  magit-reflog-mode
                                  magit-stash-mode
                                  magit-status-mode
                                  magit-wazzup-mode
                                  minibuffer-inactive-mode
                                  monky-mode
                                  sr-mode
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
  (unless (member major-mode sp-ignore-modes-list)
    (smartparens-mode t)))

;;;###autoload
(defun turn-off-smartparens-mode ()
  "Turn off `smartparens-mode'."
  (interactive)
  (smartparens-mode nil))

;; insert custom
(defcustom sp-autoinsert-pair t
  "If non-nil, auto insert pairs.  See `sp-insert-pair'."
  :type 'boolean
  :group 'smartparens)

(defcustom sp-autoinsert-if-followed-by-same 2
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
meaning, such as `markdown-mode' and * for italics and ** for bold."
  :type '(radio
          (const :tag "Insert the pair normally" 0)
          (const :tag "Insert the pair only if not followed by same" 1)
          (const :tag "Insert the pair only if not followed by same, but if the closing pair is the same as opening, insert new pair (useful for nested quote insertion)" 2)
          )
  :group 'smartparens)

(defcustom sp-autoinsert-if-followed-by-word nil
  "If non-nil, auto insert the whole pair even if point is followed by word.

For example |word followed by ( would produce (|)word.  If nil,
it would produce (|word."
  :type 'boolean
  :group 'smartparens)

(defcustom sp-autoinsert-quote-if-followed-by-closing-pair nil
  "If non-nil, auto insert string quote pair even if the point is
followed by (any) closing pair.  This option only changes
behaviour of the insertion process if point is inside a string.
In other words, if string is not closed and next character is a
closing pair.

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

(defcustom sp-autoinsert-inhibit-functions nil
  "List of functions to call before auto inserting a pair.  If
any of these return t, the pair is not inserted.  The functions
take two arguments: current opening pair and a boolean value
indicating if the point is inside string or comment."
  :type 'hook
  :group 'smartparens)

(defcustom sp-autoskip-closing-pair t
  "If non-nil, skip the following closing pair.  See
`sp-skip-closing-pair' for more info."
  :type 'boolean
  :group 'smartparens)

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

;; wrap custom
(defcustom sp-autowrap-region t
  "If non-nil, wrap the active region with pair.  See `sp-wrap-region' and `sp-wrap-region-init'"
  :type 'boolean
  :group 'smartparens)

(defcustom sp-autodelete-wrap t
  "If non-nil, auto delete both opening and closing pair of most
recent wrapping.  Deletion command must be the very first
command after the insertion, otherwise normal behaviour is
applied."
  :type 'boolean
  :group 'smartparens)

(defcustom sp-wrap-repeat-last 1
  "If the last operation was a wrap and we insert another pair at
the beginning or end of the last wrapped region, repeat the
wrap on this region with current pair."
  :type '(radio
          (const :tag "Do not repeat wrapping" 0)
          (const :tag "Only repeat if current tag is the same as the last one" 1)
          (const :tag "Always repeat if the point is after the opening/closing delimiter of last wrapped region" 2))
  :group 'smartparens)

;; escaping custom
(defcustom sp-autoescape-string-quote t
  "If non-nil, autoescape string quotes if typed inside string."
  :type 'boolean
  :group 'smartparens)

(defcustom sp-autoescape-string-quote-if-empty '(
                                                 python-mode
                                                 )
  "List of modes where the string quotes aren't escaped if the
string we're in is empty.  You can list modes where multiple
quote characters are used for multi-line strings, such as
`python-mode' to make the insertion less annoying (that is, three
times pressing \" would insert \"\"\"|\"\"\" instead of
\"\\\"\\\"|\\\"\\\"\")."
  :type '(repeat symbol)
  :group 'smartparens)

;; navigation & manip custom
(defcustom sp-navigate-consider-symbols t
  "If non-nil, consider symbols outside balanced expressions as
such.  Symbols are recognized by function `sp-forward-symbol'.
This setting affect all the navigation and manipulation functions
where it make sense.

Also, special handling of strings is enabled, where the whole
string delimited with \"\" is considered as one token."
  :type 'boolean
  :group 'smartparens)

;; ui custom
(defcustom sp-highlight-pair-overlay t
  "If non-nil, auto-inserted pairs are highlighted until point
doesn't leave them."
  :type 'boolean
  :group 'smartparens)

(defcustom sp-highlight-wrap-overlay t
  "If non-nil, wrap overlays are highlighted during the process
of editing the wrapping pair."
  :type 'boolean
  :group 'smartparens)

(defcustom sp-highlight-wrap-tag-overlay t
  "If non-nil, wrap tag overlays are highlighted during the
process of editing the wrapping tag pair."
  :type 'boolean
  :group 'smartparens)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Selection mode emulation

(defun sp-delete-selection-p ()
  "Return t if `delete-selection-mode' or `cua-delete-selection' is enabled."
  (or (and (boundp 'delete-selection-mode) delete-selection-mode)
      (and (boundp 'cua-delete-selection) cua-delete-selection)))

(defun sp-cua-replace-region (&optional arg)
  "If `smartparens-mode' is on, emulate `self-insert-command',
else call `cua-replace-region'"
  (interactive "p")
  (if smartparens-mode
      (progn
        (setq this-command 'self-insert-command)
        (self-insert-command (or arg 1)))
    (cua-replace-region)))

(defun sp-init-delete-selection-mode-emulation ()
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
    (define-key cua--region-keymap [remap self-insert-command] 'sp-cua-replace-region)
    (remove-hook 'pre-command-hook 'cua--pre-command-handler)))

(defadvice cua-mode (after cua-mode-fix-selection activate)
  (when (and cua-mode)
    (define-key cua--region-keymap [remap self-insert-command] 'sp-cua-replace-region)
    (remove-hook 'pre-command-hook 'cua--pre-command-handler)))

(defadvice delete-selection-mode (after delete-selection-mode-fix-selection activate)
  (when (and delete-selection-mode)
    (remove-hook 'pre-command-hook 'delete-selection-pre-hook)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc functions

(defmacro !delete (elm list)
  "Destructive: Sets LIST to (delete ELM LIST)."
  `(setq ,list (delete ,elm ,list)))

(defmacro !cddr (list)
  "Destructive: Sets LIST to the cdr of LIST."
  `(setq ,list (cddr ,list)))

(defmacro !plist-add (plist prop val)
  "Modify PLIST by adding property PROP with value VAL and set
  PLIST to the new value."
  `(setq ,plist (plist-put ,plist ,prop ,val)))

(defmacro sp-with (arg &rest forms)
  "Adds ARG as last argument to each form.  This can be used on
most of the permission functions to automatically supply the mode
where we operate."
  (declare (indent 1))
  `(progn
     ,@(mapcar (lambda (form) (append form (list arg))) forms)))

(defmacro sp-with-tag (arg &rest forms)
  "Adds ARG as first argument to each form.  This can be used on
most of the permission functions to automatically supply the
tag on which we want to operate."
  (declare (indent 1))
  `(progn
     ,@(mapcar (lambda (form) (append (list (car form) arg) (cdr form))) forms)))

(font-lock-add-keywords 'emacs-lisp-mode '(("\\<sp-with\\>" . font-lock-keyword-face)) 'append)
(font-lock-add-keywords 'emacs-lisp-mode '(("\\<sp-with-tag\\>" . font-lock-keyword-face)) 'append)

(defmacro --last (form list)
  "Anaphoric form of `-last'."
  (let ((n (make-symbol "needle")))
    `(let (,n)
       (--each ,list
         (when ,form (setq ,n it)))
       ,n)))

(defun -last (pred list)
  "Return the last x in LIST where (PRED x) is non-nil, else nil."
  (--last (funcall pred it) list))

(defun sp-reverse-string (str)
  "Reverse the string STR."
  (concat (reverse (append str nil))))

(defun sp-point-in-string (&optional p)
  "Return t if point is inside string or documentation string.  If
optional argument P is present, test this instead of point."
  (save-excursion
    (nth 3 (syntax-ppss p))))

(defun sp-point-in-comment (&optional p)
  "Return t if point is inside comment.  If optional argument P is
present, test this instead off point."
  (setq p (or p (point)))
  (save-excursion
    (or (nth 4 (syntax-ppss p))
        ;; this also test opening and closing comment delimiters... we
        ;; need to chack that it is not newline, which is in "comment
        ;; ender" class in elisp-mode, but we just want it to be
        ;; treated as whitespace
        (and (< p (point-max))
             (memq (char-syntax (char-after p)) '(?< ?>))
             (not (eq (char-after p) ?\n))))))

(defun sp-point-in-string-or-comment (&optional p)
  "Return t if point is inside string, documentation string or a
comment.  If optional argument P is present, test this instead
of point."
  (or (sp-point-in-string p)
      (sp-point-in-comment p)))

(defun sp-single-key-description (event)
  "Return a description of the last event.  Replace all the function
key symbols with garbage character (ň).

TODO: fix this!"
  (let ((original (single-key-description event)))
    (if (string-match-p "<.*?>" original)
        "ň" original)))

(defun sp-split-string (string by)
  "Split STRING on BY.  This simply calls `split-string' and if it
returns a list of length one, empty string is inserted to the
beginning."
  (let ((sp (split-string string by)))
    (if (not (cdr sp)) (cons "" sp) sp)))

(defun sp-this-command-self-insert-p ()
  "Return t if `this-command' is some sort of
`self-insert-command'."
  (memq this-command '(self-insert-command
                       org-self-insert-command
                       sp-self-insert-command)))

(defun sp--signum (x)
  "Return 1 if X is positive, -1 if negative, 0 if zero."
  (cond ((> x 0) 1) ((< x 0) -1) (t 0)))

(eval-when (compile eval load)
  (defun sp--get-substitute (list)
    "Internal.  Only ever call this from sp-get!  This function do
the replacement of all the keywords with actual calls to sp-get."
    (if (listp list)
        (mapcar 'sp--get-substitute list)
      (if (memq list keyword-list)
          `(sp-get ,struct ,list)
        list))))

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

(defmacro sp-get (struct attr)
  "Get a property from a structure.

STRUCT is a plist with the format as returned by `sp-get-sexp'.
Which means this macro also works with `sp-get-symbol',
`sp-get-string' and `sp-get-thing'.

ATTR is an attribute we want to query.  Currently supported
attributes are:

:beg       - point in buffer before the opening delimiter
:end       - point in the buffer after the closing delimiter
:beg-in    - point in buffer after the opening delimiter
:end-in    - point in buffer before the closing delimiter
:beg-prf   - point in buffer before the prefix of this expression
:op        - opening delimiter
:cl        - closing delimiter
:op-l      - length of the opening pair
:cl-l      - length of the closing pair
:len       - length of the entire expression, including enclosing
delimiters and the prefix
:len-out   - length of the the pair ignoring the prefix, including
delimiters
:len-in    - length of the pair inside the delimiters
:prefix    - expression prefix
:prefix-l  - expression prefix length

In addition to simple queries, this macro understands arbitrary
forms where any of the aforementioned attributes are used.
Therefore, you can for example query for
\"(+ :op-l :cl-l)\".  This query would return the sum of lengths
of opening and closing delimiter.  A query
\"(concat :prefix :op)\" would return the string containing
expression prefix and the opening delimiter.

This replacement is considered any time when the ATTR argument is
a list and not a single keyword.
"
  (let ((keyword-list '(:beg :end :beg-in :end-in :beg-prf
                             :op :cl :op-l :cl-l :len :len-out :len-in
                             :prefix :prefix-l)))
    (cond
     ;; if the attr is a list, we replace all the tags with appropriate
     ;; calls to sp-get. Example: (sp-get ok (- :end :beg))
     ((listp attr)
      (sp--get-substitute attr))
     (t
      (case attr
        ;; point in buffer before the opening delimiter
        (:beg         `(plist-get ,struct :beg))
        ;; point in the buffer after the closing delimiter
        (:end         `(plist-get ,struct :end))
        ;; point in buffer after the opening delimiter
        (:beg-in      `(+ (plist-get ,struct :beg) (length (plist-get ,struct :op))))
        ;; point in buffer before the closing delimiter
        (:end-in      `(- (plist-get ,struct :end) (length (plist-get ,struct :cl))))
        ;; point in buffer before the prefix of this expression
        (:beg-prf     `(- (plist-get ,struct :beg) (length (plist-get, struct :prefix))))
        ;; opening delimiter
        (:op          `(plist-get ,struct :op))
        ;; closing delimiter
        (:cl          `(plist-get ,struct :cl))
        ;; length of the opening pair
        (:op-l        `(length (plist-get ,struct :op)))
        ;; length of the closing pair
        (:cl-l        `(length (plist-get ,struct :cl)))
        ;; length of the entire expression, including enclosing delimiters and the prefix
        (:len         `(- (plist-get ,struct :end)
                          (plist-get ,struct :beg)
                          (- (length (plist-get ,struct :prefix)))))
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
        (:prefix-l    `(length (plist-get ,struct :prefix))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Adding/removing of pairs/bans/allows etc.

(defmacro sp-add-to-permission-list (open list &rest modes)
  "Add MODES to the pair with id OPEN in the LIST.  See
permissions system for more details."
  (let ((m (make-symbol "new-modes")))
    `(let ((,m (-flatten ,@modes)))
       (when ,m
         (let ((current (--first (equal ,open (car it)) ,list)))
           (if current
               (setcdr current (-union (cdr current) ,m))
             (!cons (cons ,open ,m) ,list)))))))

(defmacro sp-remove-from-permission-list (open list &rest modes)
  "Removes MODES from the pair with id OPEN in the LIST.  See
permissions system for more details.  If modes is nil, remove the
pair entirely."
  (let ((m (make-symbol "new-modes")))
    `(let ((,m (-flatten ,@modes)))
       (if ,m
           (let ((current (--first (equal ,open (car it)) ,list)))
             (when current
               (setcdr current (-difference (cdr current) ,m))
               (unless (cdr current)
                 (setq ,list (--remove (equal ,open (car it)) ,list)))))
         (setq ,list (--remove (equal ,open (car it)) ,list))))))

(defun sp-add-to-ordered-list (elm list order)
  "Add ELM to the LIST ordered by comparator ORDER.  The list is
ordered in descending order."
  (if (not list) (list elm)
    (if (funcall order elm (car list))
        (cons elm list)
      (cons (car list) (sp-add-to-ordered-list elm (cdr list) order)))))

(defun sp-order-pairs (a b)
  "Compare two pairs A and B by open pair length.  Return t if A
is leq to B."
  (>= (length (car a)) (length (car b))))

(defun sp-order-tag-pairs (a b)
  "Compare two tag pairs A and B by trigger length.  Return t if A is geq to B."
  (<= (length (car a)) (length (car b))))

(defun sp-add-pair (open close &rest banned-modes)
  "Add a pair formed by OPEN and CLOSE to the pair list.  See
variable `sp-pair-list' for current list.

Additional arguments are interpreted as modes where this pair
should be banned by default.  BANNED-MODES can also be a list."
  (unless (--any? (equal open (car it)) sp-pair-list)
    (setq-default sp-pair-list
                  (sp-add-to-ordered-list (cons open close) sp-pair-list #'sp-order-pairs))
    (sp-add-local-ban-insert-pair open banned-modes)
    (sp-update-pair-triggers)
    ;; we need to update local versions of sp-pair-list in all the buffers
    (sp-update-local-pairs-everywhere)))

(defun sp-remove-pair (open)
  "Remove a pair from the pair list.  See variable `sp-pair-list'
for current list."
  (setq-default sp-pair-list
                (--remove (equal open (car it)) sp-pair-list))
  (sp-remove-local-ban-insert-pair open)
  (sp-remove-local-allow-insert-pair open)
  (sp-update-pair-triggers open)
  (sp-update-local-pairs-everywhere))

(defun sp-add-local-pair (open close &rest modes)
  "Add a pair to the local pair list.  Use this only if you need
to overload a global pair with the same ID.  If you wish to
limit a pair to a certain mode, add it globally and then set
the permissions with `sp-add-local-allow-insert-pair'."
  (prog1 (sp-add-to-permission-list (cons open close) sp-local-pair-list modes)
    (sp-update-local-pairs-everywhere)))

(defun sp-remove-local-pair (open &rest modes)
  "Remove a pair from the local pair list."
  (let ((m (-flatten modes)))
    (--each sp-local-pair-list
      (when (equal open (caar it))
        (setcdr it (-difference (cdr it) m))))
    (setq sp-local-pair-list (--remove (not (cdr it)) sp-local-pair-list))
    (sp-update-local-pairs-everywhere)))

(defun sp-add-tag-pair (trig open close transform mode &rest modes)
  "Add a tag pair.  This tag pair is triggered on TRIG in modes MODE,
wraps with OPEN and CLOSE.  If the CLOSE tag contains _ the
content of the opening tag is first transformed with the
TRANSFORM function.  If the TRANSFORM variable is nil, it
defaults to `identity'.

See `sp-tag-pair-list' for more info."
  (setq transform (or transform 'identity))
  ;; check if trigger is already present
  (let ((trigger (--first (equal trig (car it)) sp-tag-pair-list))
        (m (-flatten (cons mode modes))))
    (if trigger
        ;; we need to look if there is a pair with open/close already
        ;; defined.  If yes, add the modes to its list.  If not, we need
        ;; to create new entry
        (let ((current (--first
                        (and (equal open   (nth 1 it))
                             (equal close  (nth 2 it))
                             (eq transform (nth 3 it)))
                        (cdr trigger))))
          (if current
              (setcar current (-union (car current) m))
            ;; create new entry
            (setcdr trigger (cons (list m open close transform) (cdr trigger)))))
      ;; no trigger, add the whole structure
      (let ((s (cons trig (list (list m open close transform)))))
        (setq sp-tag-pair-list
              (sp-add-to-ordered-list s sp-tag-pair-list #'sp-order-tag-pairs))
        ))))

(defun sp-remove-tag-pair (trig mode &rest modes)
  "Remove a tag pair."
  (let ((trigger (--first (equal trig (car it)) sp-tag-pair-list))
        (m (-flatten (cons mode modes))))
    (when trigger
      ;; for each pair, we need to remove the modes from its mode list.
      (--each (cdr trigger)
        (setcar it (-difference (car it) m)))
      ;; remove the pairs with empty mode lists
      (let ((newpairs (--remove (not (car it)) (cdr trigger))))
        (if newpairs
            (setcdr trigger newpairs)
          ;; if there are no pairs left at all, remove the trigger too
          (setq sp-tag-pair-list (--remove (equal trig (car it)) sp-tag-pair-list)))
        ))))

(defun sp-add-ban-insert-pair (&rest open)
  "Add the pairs with ids in OPEN to the global insertion
banlist.  That means that these pairs will never be used for auto
insertion.  They can still be used for wrapping."
  (setq sp-global-ban-insert-pair (-union sp-global-ban-insert-pair (-flatten open))))

(defun sp-remove-ban-insert-pair (&rest open)
  "Remove the pairs with ids in OPEN from the global insertion
banlist."
  (setq sp-global-ban-insert-pair (-difference sp-global-ban-insert-pair (-flatten open))))

(defun sp-add-ban-insert-pair-in-string (&rest open)
  "Add the pairs with ids in OPEN to the global \"in string\"
insertion banlist.  That means that these pairs will never be used
for auto insertion if the point is inside string.  They can still
be used for wrapping."
  (setq sp-global-ban-insert-pair-in-string (-union sp-global-ban-insert-pair-in-string (-flatten open))))

(defun sp-remove-ban-insert-pair-in-string (&rest open)
  "Remove the pairs with ids in OPEN from the global \"in
string\" insertion banlist."
  (setq sp-global-ban-insert-pair-in-string (-difference sp-global-ban-insert-pair-in-string (-flatten open))))

(defun sp-add-ban-insert-pair-in-code (&rest open)
  "Add the pairs with ids in OPEN to the global \"in code\"
insertion banlist.  That means that these pairs will never be used
for auto insertion if the point is inside code.  They can still
be used for wrapping."
  (setq sp-global-ban-insert-pair-in-code (-union sp-global-ban-insert-pair-in-code (-flatten open))))

(defun sp-remove-ban-insert-pair-in-code (&rest open)
  "Remove the pairs with ids in OPEN from the global \"in
code\" insertion banlist."
  (setq sp-global-ban-insert-pair-in-code (-difference sp-global-ban-insert-pair-in-code (-flatten open))))

(defun sp-add-local-ban-insert-pair (open &rest modes)
  "Ban autoinsertion of pair with id OPEN in modes MODES.  See
`sp-insert-pair'."
  (sp-add-to-permission-list open sp-local-ban-insert-pair modes))

(defun sp-add-local-allow-insert-pair (open &rest modes)
  "Allow autoinsertion of pair with id OPEN in modes MODES.  See
`sp-insert-pair'."
  (sp-add-to-permission-list open sp-local-allow-insert-pair modes))

(defun sp-add-local-ban-insert-pair-in-string (open &rest modes)
  "Ban autoinsertion of pair with id OPEN in modes MODES if point
is inside string, docstring or comment.  See `sp-insert-pair'."
  (sp-add-to-permission-list open sp-local-ban-insert-pair-in-string modes))

(defun sp-add-local-allow-insert-pair-in-string (open &rest modes)
  "Allow autoinsertion og pair with id OPEN in MODES if point is
inside string, docstring or comment.  See `sp-insert-pair'."
  (sp-add-to-permission-list open sp-local-allow-insert-pair-in-string modes))

(defun sp-add-local-ban-insert-pair-in-code (open &rest modes)
  "Ban autoinsertion of pair with id OPEN in modes MODES if point
is inside code.  See `sp-insert-pair'."
  (sp-add-to-permission-list open sp-local-ban-insert-pair-in-code modes))

(defun sp-add-local-allow-insert-pair-in-code (open &rest modes)
  "Allow autoinsertion og pair with id OPEN in MODES if point is
inside code.  See `sp-insert-pair'."
  (sp-add-to-permission-list open sp-local-allow-insert-pair-in-code modes))

(defun sp-remove-local-ban-insert-pair (open &rest modes)
  "Remove previously set restriction on pair with id OPEN in
modes MODES.  If MODES is nil, remove all the modes."
  (sp-remove-from-permission-list open sp-local-ban-insert-pair modes))

(defun sp-remove-local-allow-insert-pair (open &rest modes)
  "Remove previously set restriction on pair with id OPEN in
modes MODES.  If MODES is nil, remove all the modes"
  (sp-remove-from-permission-list open sp-local-allow-insert-pair modes))

(defun sp-remove-local-ban-insert-pair-in-string (open &rest modes)
  "Remove previously set restriction on pair with id OPEN in
modes MODES if the point is inside string, docstring or
comment.  If MODES is nil, remove all the modes."
  (sp-remove-from-permission-list open sp-local-ban-insert-pair-in-string modes))

(defun sp-remove-local-allow-insert-pair-in-string (open &rest modes)
  "Remove previously set restriction on pair with id OPEN in
modes MODES if the point is inside string, docstring or
comment.  If MODES is nil, remove all the modes"
  (sp-remove-from-permission-list open sp-local-allow-insert-pair-in-string modes))

(defun sp-remove-local-ban-insert-pair-in-code (open &rest modes)
  "Remove previously set restriction on pair with id OPEN in
modes MODES if the point is inside code.  If MODES is nil,
remove all the modes."
  (sp-remove-from-permission-list open sp-local-ban-insert-pair-in-code modes))

(defun sp-remove-local-allow-insert-pair-in-code (open &rest modes)
  "Remove previously set restriction on pair with id OPEN in
modes MODES if the point is inside code.  If MODES is nil,
remove all the modes"
  (sp-remove-from-permission-list open sp-local-allow-insert-pair-in-code modes))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Overlay management

;; burlywood4
(defface sp-pair-overlay-face
  '((t (:background "#004a5d")))
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
  "List of overlays used for tracking inserted pairs.  When a pair
is inserted, an overlay is created over it.  When the user starts
typing the closing pair we will not insert it again.  If user
leaves the overlay, it is canceled and the insertion works again
as usual.")
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

(defun sp-point-in-overlay-p (overlay)
  "Return t if point is in OVERLAY."
  (and (< (point) (overlay-end overlay))
       (> (point) (overlay-start overlay))))

(defun sp-get-overlay-length (overlay)
  "Compute the length of OVERLAY."
  (- (overlay-end overlay) (overlay-start overlay)))

(defun sp-get-active-overlay (&optional type)
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
      (--reduce (if (< (sp-get-overlay-length it) (sp-get-overlay-length acc)) it acc) overlays)
      ))))

(defun sp-pair-overlay-create (start end id)
  "Create an overlay over the currently inserted pair for
tracking the position of the point."
  (let ((overlay (make-overlay start end nil nil t)))
    (overlay-put overlay 'priority 100)
    (overlay-put overlay 'keymap sp-pair-overlay-keymap)
    (overlay-put overlay 'pair-id id)
    (overlay-put overlay 'type 'pair)
    (!cons overlay sp-pair-overlay-list)
    (sp-pair-overlay-fix-highlight)
    (add-hook 'post-command-hook 'sp-pair-overlay-post-command-handler nil t)))

(defun sp-wrap-cancel (&optional can-delete)
  "Cancel the active wrapping."
  (interactive)
  (let ((oleft (car sp-wrap-overlays))
        (oright (cdr sp-wrap-overlays)))
    ;; kill the insides of the "pair" if `delete-selection-mode'
    ;; emulation is enabled
    (when (and (sp-delete-selection-p) can-delete)
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

(defun sp-pair-overlay-fix-highlight ()
  "Fix highlighting of the pair overlays.  Only the active overlay
should be highlighted."
  (--each (sp--overlays-at) (overlay-put it 'face nil))
  (let* ((active (sp-get-active-overlay))
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
        (overlay-put (car sp-wrap-tag-overlays) 'face 'sp-wrap-tag-overlay-face))
      )))

(defun sp-pair-overlay-post-command-handler ()
  "Remove all pair overlays that doesn't have point inside them,
are of zero length, or if point moved backwards."
  ;; if the point moved backwards, remove all overlays
  (if (and sp-cancel-autoskip-on-backward-movement
           (< (point) sp-previous-point))
      (dolist (o sp-pair-overlay-list) (sp-remove-overlay o))
    ;; else only remove the overlays where point is outside them or
    ;; their length is zero
    (dolist (o (--remove (and (sp-point-in-overlay-p it)
                              (> (sp-get-overlay-length it) 0))
                         sp-pair-overlay-list))
      (sp-remove-overlay o)))
  (when sp-pair-overlay-list
    (setq sp-previous-point (point))))

(defun sp-remove-active-pair-overlay ()
  "Deactivate the active overlay.  See `sp-get-active-overlay'."
  (interactive)
  (let ((active-overlay (sp-get-active-overlay 'pair)))
    (when active-overlay
      (sp-remove-overlay active-overlay))))

(defun sp-remove-overlay (overlay)
  "Remove OVERLAY."
  ;; if it's not a pair overlay, nothing happens here anyway
  (!delete overlay sp-pair-overlay-list)
  ;; if we have zero pair overlays, remove the post-command hook
  (when (not sp-pair-overlay-list)
    (remove-hook 'post-command-hook 'sp-pair-overlay-post-command-handler t)
    ;; this is only updated when sp-pair-overlay-post-command-handler
    ;; is active.  Therefore, we need to reset this to 1.  If not, newly
    ;; created overlay could be removed right after creation - if
    ;; sp-previous-point was greater than actual point
    (setq sp-previous-point -1))
  (delete-overlay overlay)
  (sp-pair-overlay-fix-highlight))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pair insertion/deletion/skipping

(defun sp-insert-pair-in-string-p (id mode)
  "Return t if we can insert pair ID in MODE inside string,
docstring or comment.  See `sp-insert-pair' for more info."
  (let ((local-ban-in-string (member mode (cdr (assoc id sp-local-ban-insert-pair-in-string))))
        (local-allow-in-string (cdr (assoc id sp-local-allow-insert-pair-in-string))))
    (cond
     (local-allow-in-string (member mode local-allow-in-string))
     ((member id sp-global-ban-insert-pair-in-string) nil)
     (local-ban-in-string nil)
     (t t))))

(defun sp-insert-pair-in-code-p (id mode)
  "Return t if we can insert pair ID in MODE inside code.  See
`sp-insert-pair' for more info."
  (let ((local-ban-in-code (member mode (cdr (assoc id sp-local-ban-insert-pair-in-code))))
        (local-allow-in-code (cdr (assoc id sp-local-allow-insert-pair-in-code))))
    (cond
     (local-allow-in-code (member mode local-allow-in-code))
     ((member id sp-global-ban-insert-pair-in-code) nil)
     (local-ban-in-code nil)
     (t t))))

(defun sp-insert-pair-p (id mode &optional use-inside-string)
  "Return t if we can insert pair ID in MODE.  If
USE-INSIDE-STRING is non-nil, use value of
`sp-point-inside-string' instead of testing with
`sp-point-in-string-or-comment'.  See `sp-insert-pair' for more
info."
  (let ((local-ban (member mode (cdr (assoc id sp-local-ban-insert-pair))))
        (local-allow (cdr (assoc id sp-local-allow-insert-pair)))
        (in-string (if use-inside-string
                       ;; if we're not inside a string, we can still
                       ;; be inside a comment!
                       (or sp-point-inside-string (sp-point-in-comment))
                     (sp-point-in-string-or-comment))))
    (cond
     ;; if locally allowed, allow it.  If it's on local-allow list
     ;; automatically disable it in all non-specified modes
     (local-allow
      (if (member mode local-allow)
          (if in-string
              (sp-insert-pair-in-string-p id mode)
            (sp-insert-pair-in-code-p id mode))
        nil))
     ;; if globally disabled, disable
     ((member id sp-global-ban-insert-pair) nil)
     ;; if locally disabled, disable
     (local-ban nil)
     ;; test the "in string bans"
     (in-string
      (sp-insert-pair-in-string-p id mode))
     ;; if not in string, we must be in code
     (t (sp-insert-pair-in-code-p id mode)))))

;; TODO: add a test for a symbol property that would tell this handler
;; not to re=set `sp-last-operation'. Useful for example in "macro
;; funcions" like `my-wrap-with-paren'.
(defun sp-post-command-hook-handler ()
  "Handle the situation after some command has executed."
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

    (unless (sp-this-command-self-insert-p)
      (setq sp-last-operation nil))

    ;; unless the last command was a self-insert, remove the
    ;; information about the last wrapped region.  It is only used
    ;; for: 1. deleting the wrapping immediately after the wrap,
    ;; 2. re-wrapping region immediatelly after a sucessful wrap.
    ;; Therefore,t he deletion should have no ill-effect.  If the
    ;; necessity will arise, we can add a different flag.
    (unless (sp-this-command-self-insert-p)
      (setq sp-last-wrapped-region nil))))

(defmacro sp-setaction (action &rest forms)
  `(if (not action)
       (setq action (progn ,@forms))
     (progn ,@forms)))

(defadvice self-insert-command (around self-insert-command-adviced activate)
  (setq sp-point-inside-string (sp-point-in-string))

  ad-do-it

  (when smartparens-mode
    (setq sp-recent-keys (cons
                          (single-key-description last-command-event)
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
              (sp-setaction action (sp-insert-pair))
              (sp-setaction action (sp-skip-closing-pair))
              ;; if nothing happened, we just inserted a character, so
              ;; set the apropriate operation.  We also need to check
              ;; for `sp-self-insert-no-escape' not to overwrite
              ;; it.  See `sp-autoinsert-quote-if-followed-by-closing-pair'.
              (when (and (not action)
                         (not (eq sp-last-operation 'sp-self-insert-no-escape)))
                (setq sp-last-operation 'sp-self-insert))
              ;; if it was a quote, escape it
              (when (and (eq sp-last-operation 'sp-self-insert)
                         sp-point-inside-string
                         sp-autoescape-string-quote
                         (eq (preceding-char) ?\"))
                (save-excursion
                  (backward-char 1)
                  (insert sp-escape-char)))
              )))
        (setq sp-last-operation 'sp-self-insert)))))

(defun sp-delete-selection-mode-handle (&optional from-wrap)
  "Call the original `delete-selection-pre-hook'."
  (if smartparens-mode
      (cond
       ;; try the cua-mode emulation with `cua-delete-selection'
       ((and (boundp 'cua-mode) cua-mode
             (or (not (sp-this-command-self-insert-p))
                 (not sp-autowrap-region)))
        ;; if sp-autowrap-region is disabled, we need to translate
        ;; `sp-cua-replace-region' back to `self-insert-command'
        ;; because this is *pre* command hook
        ;; TODO: why do we need sp-cua-replace-region?
        (when (and (not sp-autowrap-region)
                   (eq this-command 'sp-cua-replace-region))
          (setq this-command 'self-insert-command))
        (cua--pre-command-handler))
       ;; this handles the special case after `self-insert-command' if
       ;; `sp-autowrap-region' is t.
       ((and (boundp 'cua-mode) cua-mode from-wrap)
        (cua-replace-region))
       ;; if not self-insert, just run the hook from
       ;; `delete-selection-mode'
       ((and (boundp 'delete-selection-mode) delete-selection-mode
             (or from-wrap
                 (not sp-autowrap-region)
                 (not (sp-this-command-self-insert-p))))
        (delete-selection-pre-hook)))
    ;; this handles the callbacks properly if the smartparens mode is
    ;; disabled.  Smartparens-mode adds advices on cua-mode and
    ;; delete-selection-mode that automatically remove the callbacks
    (cond
     ((and (boundp 'cua-mode) cua-mode
           (not (member 'pre-command-hook 'cua--pre-command-handler)))
      (cua--pre-command-handler))
     ((and (boundp 'delete-selection-mode) delete-selection-mode
           (not (member 'pre-command-hook 'delete-selection-pre-hook)))
      (delete-selection-pre-hook)))))

(defun sp-pre-command-hook-handler ()
  "Main handler of pre-command-hook.  Handle the
`delete-selection-mode' or `cua-delete-selection' stuff here."
  (sp-delete-selection-mode-handle))

(defvar sp-recent-keys nil
  "Last 20 typed keys, registered via `self-insert-command'.")

(defun sp-get-recent-keys-1 ()
  "Internal.  Return 10 recent keys in reverse order (most recent
is first) as a string."
  (apply #'concat sp-recent-keys))

(defmacro sp-get-pair-list-1 ()
  "Internal.  Return all pairs that can be inserted in this
`major-mode' and do not have same opening and closing delimiter.
This is used for navigation functions."
  `(--filter (and (sp-insert-pair-p (car it) major-mode)
                  ;; ignore pairs with same open/close
                  (not (string= (car it) (cdr it)))) sp-pair-list))

(defun sp-get-opening-regexp-1 ()
  "Internal.  Return regexp matching any opening pair."
  (regexp-opt (--map (car it) (sp-get-pair-list-1))))

(defun sp-get-closing-regexp-1 ()
  "Internal.  Return regexp matching any closing pair."
  (regexp-opt (--map (cdr it) (sp-get-pair-list-1))))

(defun sp-get-pair-regexp-1 ()
  "Return a regexp that matches any opening or closing pair delimiter."
  (regexp-opt (let (s) (--each (sp-get-pair-list-1) (!cons (cdr it) s) (!cons (car it) s)) s)))

(defun sp-wrap-region-init ()
  "Initialize the region wrapping."
  (when sp-autowrap-region
    ;; if we can't possibly form a wrap, just insert the char and do
    ;; nothing.  If `sp-delete-selection-p' is true, run
    ;; `sp-delete-selection-mode-handle' with t that means it was
    ;; called from withing wrapping procedure
    (if (--none? (string-prefix-p (sp-single-key-description last-command-event) (car it)) sp-pair-list)
        (let ((p (1- (point)))
              (m (mark)))
          ;; test if we can at least start a tag wrapping.  If not,
          ;; delete the region if apropriate
          (unless (sp-wrap-tag-region-init)
            (sp-delete-selection-mode-handle t)
            (when (and (sp-delete-selection-p)
                       (< m p)
                       (= (length (sp-single-key-description last-command-event)) 1))
              (insert (sp-single-key-description last-command-event)))))
      (let* ((p (1- (point))) ;; we want the point *before* the
             ;; insertion of the character
             (m (mark))
             (ostart (if (> p m) m p))
             (oend (if (> p m) p m))
             (last-keys (sp-get-recent-keys-1))
             ;;(last-keys "\"\"\"\"\"\"\"\"")
             (active-pair (--first (string-prefix-p (sp-reverse-string (car it)) last-keys) sp-pair-list)))

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
                          (list p (+ len m -1) (car active-pair) (cdr active-pair))
                        (list m (+ len p) (car active-pair) (cdr active-pair))))
                ;; only autoescape "" pair, so it has to be one-char
                ;; length, therefore we can handle it here
                (when (and (equal (car active-pair) "\"")
                           (equal (cdr active-pair) "\""))
                  (sp-wrap-region-autoescape strbound))
                sp-last-wrapped-region))

          ;; save the position and point so we can restore it on cancel.
          (setq sp-wrap-point p)
          (setq sp-wrap-mark m)

          ;; We need to remember what was removed in case wrap is
          ;; cancelled.  Then these characters are re-inserted.
          (setq sp-last-inserted-characters (sp-single-key-description last-command-event))

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
                                           sp-pair-list))))
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

            (goto-char (1+ ostart))
            ))))))

(defun sp-wrap-region ()
  "Wrap region."
  ;; this method is only called if there's an active region.  It should
  ;; never be called manually!
  (when sp-autowrap-region
    (let* ((oleft (car sp-wrap-overlays))
           (oright (cdr sp-wrap-overlays)))
      (setq sp-last-inserted-characters
            (concat sp-last-inserted-characters
                    (sp-single-key-description last-command-event)))
      (let* ((active-pair (--last (string-prefix-p
                                   sp-last-inserted-characters
                                   (car it))
                                  sp-pair-list))
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
              (setq sp-last-wrapped-region (list s e open-pair close-pair))
              )))))))

;; TODO: maybe it'd be cleaner to change the format of
;; `sp-tag-pair-list' instead of this mapcat map orgy.
(defun sp-get-active-tag-1 (recent)
  "Internal.  Return the first tag that matches its trigger to
the prefix of RECENT and is allowed in current mode.  Such a tag
should be unique."
  (--first (member major-mode (cadr it))
           (-mapcat (lambda (x) (--map (-concat (list (car x)) it) (cdr x)))
                    (--filter (string-prefix-p recent (car it)) sp-tag-pair-list))))

(defun sp-wrap-tag-region-init ()
  "Init a region wrapping with a tag pair.  This is called from
`sp-wrap-region-init' or `sp-wrap-region' (usually on failure) to
see if the currently entered \"wrap\" can be extended as a
tag.  The tag always gets priority from the regular wrap."
  (when sp-autowrap-region
    ;; we can either enter tagwrapping from already present wrap or
    ;; from nothing (if the wrap-init failed to find any usable wrap)
    ;; or at failure (the entered wrap doesn't match any pair)
    (if sp-wrap-overlays ;; called from within the wrap-mode
        (let* ((oleft (car sp-wrap-overlays))
               (oright (cdr sp-wrap-overlays))
               (active-tag (sp-get-active-tag-1 sp-last-inserted-characters)))
          (when active-tag
            ;; if we've found a tag trigger, enter the tag editing mode
            (if (eq (length sp-last-inserted-characters) (length (car active-tag)))
                (progn
                  (delete-region (overlay-start oright) (overlay-end oright))
                  (sp-wrap-tag-create-overlays (car active-tag) (cddr active-tag)
                                               (overlay-start oleft)
                                               (-
                                                (overlay-start oright)
                                                (sp-get-overlay-length oleft)))
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
             (active-tag (sp-get-active-tag-1
                            (sp-single-key-description last-command-event))))
        (when active-tag
          (setq sp-last-inserted-characters (sp-single-key-description last-command-event))
          (setq sp-wrap-point p)
          (setq sp-wrap-mark m)
          (when (> p m)
            (delete-char (- 1))
            (goto-char ostart)
            (insert sp-last-inserted-characters)
            (setq oend (1+ oend)))
          (if (= 1 (length (car active-tag)))
              ;; the tag is only 1 character long, we can enter
              ;; insertion mode right away
              ;; I don't know why it needs 1- here, but it does :D
              (sp-wrap-tag-create-overlays (car active-tag) (cddr active-tag) ostart (1- oend))
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
              (goto-char (1+ ostart)))
            ))))))

(defun sp-wrap-tag-create-overlays (tag active-tag ostart oend)
  "Create the wrap tag overlays.

OSTART is the start of the modified area, including the pair trigger string.

OEND is the end of the modified area, that is the end of the
wrapped region, exluding any existing possible wrap."
  (let* ((tag-open (sp-split-string (nth 0 active-tag) "_"))
         (tag-close (sp-split-string (nth 1 active-tag) "_"))
         (o (apply #'+ (mapcar #'length tag-open)))
         (c (apply #'+ (mapcar #'length tag-close))))
    ;; setup the wrap pairs
    ;; opening one
    (goto-char ostart)
    (delete-char (length tag))
    (insert (apply #'concat tag-open))
    (backward-char (length (cadr tag-open)))

    ;; closing one
    (save-excursion
      (goto-char (+ oend o))
      (insert (apply #'concat tag-close)))

    (if (cdr (split-string (nth 0 active-tag) "_"))
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
          (overlay-put oleft 'active-tag active-tag)
          (overlay-put oleft 'modification-hooks '(sp-wrap-tag-update))
          (overlay-put oleft 'insert-in-front-hooks '(sp-wrap-tag-update))
          (overlay-put oleft 'insert-behind-hooks '(sp-wrap-tag-update))
          (add-hook 'post-command-hook 'sp-wrap-tag-post-command-handler))
      ;; if the tag didn't have any substitution, that means we only
      ;; insert the "brackets" and not enter the tag-insertion mode.
      ;; Therefore we move the point to the original position, so it
      ;; behaves just like normal wrap
      (if (> sp-wrap-mark sp-wrap-point)
          (goto-char (+ sp-wrap-point o))
        (goto-char (+ sp-wrap-point o c))))
    (setq sp-last-operation 'sp-wrap-tag)))

(defun sp-wrap-tag-update (overlay after? beg end &optional length)
  (let* ((oleft (car sp-wrap-tag-overlays))
         (oright (cdr sp-wrap-tag-overlays))
         (active-tag (overlay-get oleft 'active-tag))
         (transform (nth 2 active-tag))
         (open (buffer-substring (overlay-start oleft) (overlay-end oleft))))
    (when (string-match-p "_" (nth 1 active-tag))
      (save-excursion
        (delete-region (overlay-start oright) (overlay-end oright))
        (goto-char (overlay-start oright))
        (insert (funcall transform open))))))

(defun sp-wrap-tag-post-command-handler ()
  "Terminate the tag insertion mode if the point jumps out of the
tag overlay."
  (if (or (not sp-wrap-tag-overlays)
          (< (point) (overlay-start (car sp-wrap-tag-overlays)))
          (> (point) (overlay-end (car sp-wrap-tag-overlays))))
      (sp-wrap-tag-done)))

(defun sp-match-sgml-tags (tag)
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
  (let ((oleft (car sp-wrap-tag-overlays))
        (oright (cdr sp-wrap-tag-overlays)))
    (delete-overlay oleft)
    (delete-overlay oright)
    (setq sp-wrap-tag-overlays nil)
    (remove-hook 'post-command-hook 'sp-wrap-tag-post-command-handler)))

(defun sp-wrap-region-autoescape (strbound)
  "If we wrap a region with \"\" quotes, and the whole region was
inside a string, automatically escape the enclosing quotes.  If
we wrap a region that wasn't a string, automatically quote any
string quotes inside it.

This is internal function and should be only called after a
wrapping."
  (when sp-autoescape-string-quote
    (let ((b (car sp-last-wrapped-region))
          (e (cadr sp-last-wrapped-region))
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
          (goto-char e)
          (insert sp-escape-char))
        ;; update the sp-last-wrapped-region info to \" pair
        (setq sp-last-wrapped-region (list b (+ 2 e) "\\\"" "\\\"")))
       (t
        (setq was-beg (< (point) e))
        (let ((num 0))
          (goto-char b)
          (while (search-forward-regexp "\\([^\\]\\)\"" (+ e -1 num) t)
            (setq num (1+ num))
            (replace-match "\\1\\\\\"" t))
          (setq sp-last-wrapped-region (list b (+ num e) "\"" "\""))
          (if was-beg
              (goto-char (1+ b))
            (goto-char (+ e num)))
          ))))))

(defun sp-insert-pair ()
  "Automatically insert the closing pair if it is allowed in current
context.  It is determined in this order:

1. Global allow - all pairs are allowed by default in every mode.

2. Local ban - you can ban specific pair in specific modes.  See
`sp-add-local-ban-insert-pair'.

3. Global ban - you can globally ban specific pair.

4. Local allow - you can allow specific pair in specific
modes.  It is disabled in all other modes (as if you globally
banned it first).  See `sp-add-local-allow-insert-pair'.

Additionally, there is a possibility to disable insertion of
pairs inside strings, docstrings and comments or inside code.  The
order in which these are determined is following:

1. Is the pair allowed in this mode? (by the above mechanism)

2. Local ban inside strings - `sp-add-local-ban-insert-pair-in-string'.

3. Global ban - `sp-add-ban-insert-pair-in-string'.

4. Local allow - `sp-add-local-allow-insert-pair-in-string'.

The semantics are the same as above.  First, point position is
tested to determine if it is inside string.  If yes, the rules for
string restrictions are evaluated.  If not, restrictions for code
are evaluated.  The code versions end with \"in-code\" instead of
\"in-string\".

You can disable this feature completely for all modes and all pairs by
setting `sp-autoinsert-pair' to nil.

You can globally disable insertion of closing pair if point is
followed by the matching opening pair.  It is disabled by
default.  See `sp-autoinsert-if-followed-by-same' for more info.

You can globally disable insertion of closing pair if point is
followed by word.  It is disabled by default.  See
`sp-autoinsert-if-followed-by-word' for more info."
  (let* ((last-keys (sp-get-recent-keys-1))
         ;; (last-keys "\"\"\"\"\"\"\"\"\"\"\"\"")
         ;; we go through all the opening pairs and compare them to
         ;; last-keys.  If the opair is a prefix of last-keys, insert
         ;; the closing pair
         (active-pair (--first (string-prefix-p (sp-reverse-string (car it)) last-keys) sp-pair-list))
         (open-pair (car active-pair))
         (close-pair (cdr active-pair)))
    ;; Test "repeat last wrap" here.  If we wrap a region and then
    ;; type in a pair, wrap again around the last active region.  This
    ;; should probably be tested in the `self-insert-command'
    ;; advice... but we're lazy :D
    (if (and sp-autowrap-region
             active-pair
             (sp-wrap-repeat-last-1 active-pair))
        sp-last-operation
      (when (and sp-autoinsert-pair
                 active-pair
                 (not (eq sp-last-operation 'sp-skip-closing-pair))
                 (sp-insert-pair-p open-pair major-mode t)
                 (if sp-autoinsert-if-followed-by-word t
                   (or (= (point) (point-max))
                       (not (and (eq (char-syntax (following-char)) ?w)
                                 (not (eq (following-char) ?\'))))))
                 (if sp-autoinsert-quote-if-followed-by-closing-pair t
                   (if (and (eq (char-syntax (preceding-char)) ?\")
                            ;; this is called *after* the character is
                            ;; inserted.  Therefore, if we are not in string, it
                            ;; must have been closed just now
                            (not (sp-point-in-string)))
                       (let* ((pair-list (sp-get-pair-list-1))
                              (pattern (regexp-opt (--map (cdr it) pair-list))))
                         ;; If we simply insert closing ", we also
                         ;; don't want to escape it.  Therefore, we
                         ;; need to set `sp-last-operation'
                         ;; accordingly to be checked in
                         ;; `self-insert-command' advice.
                         (if (looking-at pattern)
                             (progn (setq sp-last-operation 'sp-self-insert-no-escape) nil)
                           t))
                     t))
                 (cond
                  ((eq sp-autoinsert-if-followed-by-same 0) t)
                  ((eq sp-autoinsert-if-followed-by-same 1)
                   (not (looking-at (regexp-quote open-pair))))
                  ((eq sp-autoinsert-if-followed-by-same 2)
                   (or (not (looking-at (regexp-quote open-pair)))
                       (and (equal open-pair close-pair)
                            (eq sp-last-operation 'sp-insert-pair)
                            (save-excursion
                              (backward-char 1)
                              (sp-looking-back (regexp-quote open-pair) sp-max-pair-length-c))
                            ))))
                 (not (run-hook-with-args-until-success
                       'sp-autoinsert-inhibit-functions
                       open-pair
                       (or sp-point-inside-string (sp-point-in-comment))))
                 )
        (insert close-pair)
        (backward-char (length close-pair))
        (sp-pair-overlay-create (- (point) (length open-pair))
                                (+ (point) (length close-pair))
                                open-pair)

        ;; we only autoescape if the pair is a single character string
        ;; delimiter.  More elaborate pairs are probably already
        ;; escaped.  We leave the responsibility to the user, since
        ;; it's not that common and the usecases might vary -> there's
        ;; no good "default" case.
        (when (and sp-autoescape-string-quote
                   sp-point-inside-string
                   (equal open-pair "\"")
                   (equal close-pair "\"")
                   (or (not (memq major-mode sp-autoescape-string-quote-if-empty))
                       ;; test if the string is empty here
                       (not (and (equal (char-after (1+ (point))) ?\")
                                 (equal (char-after (- (point) 2)) ?\")))))
          (save-excursion
            (backward-char 1)
            (insert sp-escape-char)
            (forward-char 1)
            (insert sp-escape-char))
          (overlay-put (sp-get-active-overlay 'pair) 'pair-id "\\\""))

        (setq sp-last-operation 'sp-insert-pair)))))

(defmacro sp-wrap-repeat-last-2 (same-only)
  "Internal."
  `(when sp-last-wrapped-region
     (let* ((b (car sp-last-wrapped-region))
            (e (cadr sp-last-wrapped-region))
            (op (nth 2 sp-last-wrapped-region))
            (oplen (length op))
            (cllen (length (nth 3 sp-last-wrapped-region)))
            (acolen (length (car active-pair)))
            (acclen (length (cdr active-pair)))
            (inside-region (< (point) e)))
       (when (and ,(if same-only '(equal (car active-pair) op) t)
                  (memq sp-last-operation '(sp-self-insert sp-wrap-region))
                  (or (= (point) (+ b oplen acolen))
                      (= (point) (+ e acolen))))
         ;; TODO: this does not handle escaping of "double quote", that
         ;; is if we repeat quote wrap after quote wrap.  I think it is
         ;; reasonable to assume this will never happen, or very very
         ;; rarely. (same goes for option 2)
         (delete-char (- acolen))
         (if inside-region
             (progn (goto-char (+ b oplen))
                    (insert (car active-pair))
                    (goto-char (- (+ e acolen) cllen))
                    (insert (cdr active-pair))
                    (goto-char (+ b oplen acolen))
                    (setq sp-last-wrapped-region
                          (list (+ b oplen) (+ e acolen acclen (- cllen))
                                (car active-pair) (cdr active-pair))))
           (goto-char b)
           (insert (car active-pair))
           (goto-char (+ e acolen))
           (insert (cdr active-pair))
           (setq sp-last-wrapped-region
                 (list b (+ e acolen acclen)
                       (car active-pair) (cdr active-pair))))
         (setq sp-last-operation 'sp-wrap-region)))))

(defun sp-wrap-repeat-last-1 (active-pair)
  "Internal.  If the last operation was a wrap and
`sp-wrap-repeat-last' is non-nil, repeat the wrapping with this
pair around the last active region."
  (cond
   ((= 0 sp-wrap-repeat-last) nil)
   ((= 1 sp-wrap-repeat-last)
    (sp-wrap-repeat-last-2 t))
   ((= 2 sp-wrap-repeat-last)
    (sp-wrap-repeat-last-2 nil))))

(defun sp-skip-closing-pair ()
  "If point is inside an inserted pair, and the user only moved forward
with point (that is, only inserted text), if the closing pair is
typed, we shouldn't insert it again but skip forward.

For example, pressing ( is followed by inserting the pair (|).  If
we then type 'word' and follow by ), the result should be (word)|
instead of (word)|).

If the user moved backwards or outside the
pair, this behaviour is cancelled.  This behaviour can be globally
disabled by setting `sp-cancel-autoskip-on-backward-movement' to
nil.

This behaviour can be globally disabled by setting
`sp-autoskip-closing-pair' to nil."
  (when (and sp-autoskip-closing-pair
             sp-pair-overlay-list
             (sp-get-active-overlay 'pair))
    (let* ((overlay (sp-get-active-overlay 'pair))
           (open-pair (overlay-get overlay 'pair-id))
           (close-pair (cdr (assoc open-pair sp-pair-list)))
           ;; how many chars have we already typed
           (already-skipped (- (length close-pair) (- (overlay-end overlay) (point))))
           )
      ;; only if we're at the closing pair or inside it
      (when (>= already-skipped 0)
        ;; rest of yet-untyped close-pair
        (let ((close-pair-rest (substring close-pair already-skipped))
              (last last-command-event))
          (when (and (looking-at (regexp-quote close-pair-rest))
                     ;; start deletion only if point is not right
                     ;; after the opening pair *after* the potential
                     ;; closing character was inserted (if opening
                     ;; pair and closing pair are the same, it would
                     ;; delete it right after the insertion otherwise)
                     (> (- (point) (overlay-start overlay)) (length open-pair)))
            (if (equal (sp-single-key-description last) (substring close-pair-rest 0 1))
                (progn
                  (forward-char 1)
                  (delete-char (- 1))
                  (setq sp-last-operation 'sp-skip-closing-pair))
              ;; Charactar that is not part of the closing pair was
              ;; typed.  Only remove overlays if we're inside the
              ;; closing pair.  If we are at the beginning, we are
              ;; allowed to type other characters
              (when (> already-skipped 0)
                (dolist (o sp-pair-overlay-list) (sp-remove-overlay o)))
              )))))))

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
              (s (nth 0 sp-last-wrapped-region))
              (e (nth 1 sp-last-wrapped-region))
              (o (length (nth 2 sp-last-wrapped-region)))
              (c (length (nth 3 sp-last-wrapped-region))))
          ;; if the last operation was `sp-wrap-region', and we are at
          ;; the position of either opening or closing pair, delete the
          ;; just-inserted pair
          (cond
           ((= p (+ s o))
            (save-excursion
              (delete-char (- (1- o)))
              (goto-char (- e o -1))
              (delete-char (- c)))
            (setq sp-last-operation 'sp-delete-pair-wrap))
           ((= p e)
            (save-excursion
              (delete-char (- (1- c)))
              (goto-char s)
              (delete-char o))
            (setq sp-last-operation 'sp-delete-pair-wrap))))
      (let* ((p (point))
             (inside-pair (--first (and (sp-looking-back (regexp-quote (car it)) sp-max-pair-length-c)
                                        (looking-at (regexp-quote (cdr it))))
                                   sp-pair-list))
             (behind-pair (--first (sp-looking-back (regexp-quote (cdr it)) sp-max-pair-length-c) sp-pair-list))
             (opening-pair (--first (sp-looking-back (regexp-quote (car it)) sp-max-pair-length-c) sp-pair-list)))
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
          (delete-char (length (cdr inside-pair)))
          (delete-char (- (1- (length (car inside-pair)))))
          (setq sp-last-operation 'sp-delete-pair))
         ;; we're behind a closing pair
         ((and behind-pair sp-autodelete-closing-pair)
          (delete-char (- (1- (length (cdr behind-pair)))))
          (setq sp-last-operation 'sp-delete-pair-closing))
         ;; we're behind an opening pair and there's no closing pair
         ((and opening-pair sp-autodelete-opening-pair)
          (delete-char (- (1- (length (car opening-pair)))))
          (setq sp-last-operation 'sp-delete-pair-opening))
         )))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Navigation

(defun sp-looking-back (regexp &optional limit greedy)
  "Works just like `looking-back', but LIMIT specifies how many
characters backward we should be looking instead of a buffer
position.  If the point is less than LIMIT chars from the start
of the buffer, it replaces LIMIT with (point)."
  (when (and limit
             (< (point) limit))
    (setq limit (point)))
  (setq limit (or limit (point)))
  (looking-back regexp (- (point) limit) greedy))

(defun sp-search-backward-regexp (regexp &optional bound noerror)
  "Works just like `search-backward-regexp', but returns the
longest possible match.  That means that searching for
\"defun|fun\" backwards would return \"defun\" instead of
\"fun\", which would be matched first.

This is an internal function.  Only use this for searching for
pairs!"
  (when (search-backward-regexp regexp bound noerror)
    (goto-char (match-end 0))
    (sp-looking-back regexp sp-max-pair-length-c t)
    (goto-char (match-beginning 0))))

(defun sp-get-quoted-string-bounds ()
  "If the point is inside a quoted string, return its bounds."
  (when (nth 3 (syntax-ppss))
    (let ((open (save-excursion
                  (while (nth 3 (syntax-ppss))
                    (forward-char -1))
                  (1+ (point))))
          (close (save-excursion
                   (while (nth 3 (syntax-ppss))
                     (forward-char 1))
                   (1- (point)))))
      (cons open close))))

(defmacro sp-search-and-save-match (search-fn pattern bound res beg end str)
  "Save the last match info."
  `(progn
     (setq ,res (funcall ,search-fn ,pattern ,bound t))
     (when ,res
       (setq ,beg (match-beginning 0))
       (setq ,end (match-end 0))
       (setq ,str (match-string 0)))))

;; TODO: since this function is used for all the navigation, we should
;; optimaze it a lot! Get some elisp profiler!
(defun sp-get-sexp (&optional back)
  "Find the nearest balanced expression that is after point, or
before point if BACK is non-nil.  This also means, if the point
is inside an expression, this expression is returned.

For the moment, this function (ignores) pairs where the opening and
closing pair is the same, as it is impossible to correctly
determine the opening/closing relation without keeping track of
the content of the entire buffer.

If the search starts outside a comment, all subsequent comments
are skipped.

If the search starts inside a string or comment, it tries to find
the first balanced expression that is completely contained inside
the string or comment.  If no such expression exist, a warning is
raised (for example, when you comment out imbalanced expression).
However, if you start a search from within a string and the next
complete sexp lies completely outside, this is returned.

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
  (let* ((search-fn (if (not back) 'search-forward-regexp 'sp-search-backward-regexp))
         (pair-list (sp-get-pair-list-1))
         (pattern (regexp-opt (-flatten (--map (list (car it) (cdr it)) pair-list))))
         (in-string-or-comment (sp-point-in-string-or-comment))
         (string-bounds (and in-string-or-comment (sp-get-quoted-string-bounds)))
         (fw-bound (point-max))
         (bw-bound (point-min))
         (s nil) (e nil) (active-pair nil) (forward nil) (failure nil)
         (mb nil) (me nil) (ms nil) (r nil))
    (save-excursion
      (sp-search-and-save-match search-fn pattern (if back bw-bound fw-bound)
                                r mb me ms)
      (when (not (sp-point-in-string-or-comment))
        (setq in-string-or-comment nil))
      (unless in-string-or-comment
        (while (sp-point-in-string-or-comment)
          (sp-search-and-save-match search-fn pattern (if back bw-bound fw-bound)
                                    r mb me ms)))
      (when r
        (setq active-pair (--first (equal ms (car it)) pair-list))
        (if active-pair
            (progn
              (setq forward t)
              (setq s mb)
              (when back
                (forward-char (length (car active-pair)))))
          (setq active-pair (--first (equal ms (cdr it)) pair-list))
          (setq e me)
          (when (not back)
            (backward-char (length (cdr active-pair)))))
        (let* ((open (if forward (car active-pair) (cdr active-pair)))
               (close (if forward (cdr active-pair) (car active-pair)))
               (needle (regexp-opt (list (car active-pair) (cdr active-pair))))
               (search-fn (if forward 'search-forward-regexp 'search-backward-regexp))
               (depth 1)
               (eof (if forward 'eobp 'bobp))
               (b (if forward fw-bound bw-bound)))
          (while (and (> depth 0) (not (funcall eof)))
            (sp-search-and-save-match search-fn needle b r mb me ms)
            (if r
                (unless (or (and (not in-string-or-comment)
                                 (sp-point-in-string-or-comment))
                            ;; we need to check if the match isn't
                            ;; preceded by escape sequence.  This is a
                            ;; bit tricky to do right, so for now we
                            ;; just handle emacs-lisp ?\ character
                            ;; prefix
                            (and (member major-mode '(emacs-lisp-mode inferior-emacs-lisp-mode lisp-mode))
                                 (equal (buffer-substring (max 1 (- mb 2)) mb) "?\\")))
                  (if (equal ms open)
                      (setq depth (1+ depth))
                    (setq depth (1- depth))))
              (message "Search failed.  This means there is unmatched expression somewhere or we are at the beginning/end of file.")
              (setq depth -1)
              (setq failure t)))
          (if forward
              (setq e me)
            (setq s mb))
          (unless failure
            (cond
             ((or (and (sp-point-in-string-or-comment s) (not (sp-point-in-string-or-comment e)))
                  (and (not (sp-point-in-string-or-comment s)) (sp-point-in-string-or-comment e)))
              (message "Opening or closing pair is inside a string or comment and matching pair is outside (or vice versa).  Ignored.")
              nil)
             (t (list :beg s
                      :end e
                      :op (if forward open close)
                      :cl (if forward close open)
                      :prefix (sp--get-prefix s))))
            ))))))

(defun sp-get-enclosing-sexp (&optional arg)
  "Return the balanced expression that wraps point at the same
level.  With ARG, ascend that many times.  This funciton expect
positive argument."
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
                       (> (sp-get ok :beg) (sp-get okr :beg))
                       (< (sp-get ok :end) (sp-get okr :end)))
            (setq ok okr)
            (goto-char (sp-get ok :end))))
        (setq n (1- n)))
      ok)))

(defun sp-get-list-items (&optional lst)
  "Return the information about expressions inside LST.  LST
should be a data structure in format as returned by
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

(defun* sp--get-prefix (&optional (p (point)))
  "Internal.  Get the prefix of EXPR. Prefix is any continuous
sequence of characters in \"expression prefix\" syntax class."
  (save-excursion
    (goto-char p)
    (skip-syntax-backward "'")
    (buffer-substring-no-properties (point) p)))

(defun sp-get-symbol (&optional back)
  "Find the nearest symbol that is after point, or before point if
BACK is non-nil.  This also means, if the point is inside a symbol,
this symbol is returned.  Symbol is defined as a chunk of text
recognized by `sp-forward-symbol'.

The return value is a plist with the same format as the value
returned by `sp-get-sexp'."
  (let (b e prefix)
    (save-excursion
      (if back
          (progn
            (sp-skip-backward-to-symbol)
            (sp-forward-symbol -1)
            (setq b (point))
            (sp-forward-symbol 1)
            (setq e (point)))
        (sp-skip-forward-to-symbol)
        (sp-forward-symbol 1)
        (setq e (point))
        (sp-forward-symbol -1)
        (setq b (point))))
    (list :beg b :end e :op " " :cl " " :prefix (sp--get-prefix b))))

(defun sp--get-string (bounds)
  "Internal.  Return the `sp-get-sexp' format info about the
string."
  (list :beg (1- (car bounds))
        :end (1+ (cdr bounds))
        :op (char-to-string (char-after (cdr bounds)))
        :cl (char-to-string (char-after (cdr bounds)))
        :prefix ""))

(defun sp-get-string (&optional back)
  "Find the nearest string after point, or before if BACK is
non-nil.  This also means if the point is inside a string, this
string is returned.  If there are another symbols between point
and the string, nil is returned.  That means that this funciton
only return non-nil if the string is the very next meaningful
expression.

The return value is a plist with the same format as the value
returned by `sp-get-sexp'."
  (let (b e)
    (if (sp-point-in-string)
        (let ((r (sp-get-quoted-string-bounds)))
          (sp--get-string r))
      (save-excursion
        (if back
            (sp-skip-backward-to-symbol)
          (sp-skip-forward-to-symbol))
        (let ((r (sp-get-quoted-string-bounds)))
          (when r (sp--get-string r)))))))

(defmacro sp--get-thing (back)
  "Internal."
  `(if (not sp-navigate-consider-symbols)
       (sp-get-sexp ,back)
     (save-excursion
       ,(if back '(sp-skip-backward-to-symbol t)
          '(sp-skip-forward-to-symbol t))
       (cond
        (,(if back '(sp-looking-back (sp-get-closing-regexp-1) sp-max-pair-length-c)
            '(looking-at (sp-get-opening-regexp-1)))
         (sp-get-sexp ,back))
        (,(if back '(sp-looking-back (sp-get-opening-regexp-1) sp-max-pair-length-c)
            '(looking-at (sp-get-closing-regexp-1)))
         (sp-get-sexp ,back))
        ((eq (char-syntax ,(if back '(preceding-char) '(following-char))) ?\" )
         (sp-get-string ,back))
        (t (sp-get-symbol ,back))))))

(defun sp-get-thing (&optional back)
  "Find next thing after point, or before if BACK is non-nil.  Thing
is either symbol (`sp-get-symbol'), string (`sp-get-string') or
balanced expression recognized by `sp-get-sexp'.

If `sp-navigate-consider-symbols' is nil, only balanced
expressions are considered."
  (if back
      (sp--get-thing t)
    (sp--get-thing nil)))

(defun sp-forward-sexp (&optional arg)
  "Move forward across one balanced expression.  With ARG, do it
that many times.  Negative arg -N means move backward across N
balanced expressions.  If there is no forward expression, jump
out of the current one (effectively doing `sp-up-sexp').

With `sp-navigate-consider-symbols' symbols and strings are also
considered balanced expressions."
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
considered balanced expressions."
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
considered balanced expressions."
  (interactive "p")
  (setq arg (or arg 1))
  (if (> arg 0)
      (if (= arg 1)
          (let ((ok (sp-get-thing)))
            (when ok
              (if (= (point) (sp-get ok :beg))
                  (progn (sp-forward-sexp 2)
                         (sp-backward-sexp))
                (goto-char (sp-get ok :beg))
                ok)))
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
considered balanced expressions."
  (interactive "p")
  (setq arg (or arg 1))
  (if (> arg 0)
      (if (= arg 1)
          (let ((ok (sp-get-thing t)))
            (when ok
              (if (= (point) (sp-get ok :end))
                  (progn (sp-backward-sexp 2)
                         (sp-forward-sexp))
                (goto-char (sp-get ok :end))
                ok)))
        (sp-backward-sexp arg)
        (sp-forward-sexp))
    (sp-forward-sexp (- arg))))

(defun sp-raw-argument-p-1 (arg)
  "Internal.  Return t if ARG represents raw argument, that is a
non-empty list."
  (and (listp arg) (car arg)))

(defun sp-down-sexp (&optional arg)
  "Move forward down one level of sexp.  With ARG, do this that
many times.  A negative argument -N means move backward but still
go down a level.

If ARG is raw prefix argument C-u, descend forward as much as
possible.

If ARG is raw prefix argument C-u C-u, jump to the beginning of
current list.

If the point is inside sexp and there is no down expression to
descend to, jump to the beginning of current one.  If moving
backwards, jump to end of current one."
  (interactive "P")
  (setq arg (prefix-numeric-value arg))
  (let ((n (abs arg))
        (ok t)
        (raw (sp-raw-argument-p-1 current-prefix-arg))
        (last-point -1))
    (if (and raw (= (abs arg) 16))
        ;; jump to the beginning/end of current list
        (let ((enc (sp-get-enclosing-sexp)))
          (when enc
            (if (> arg 0)
                (goto-char (sp-get enc :beg-in))
              (goto-char (sp-get enc :end-in)))
            enc))
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
  "Move backward down one level of sexp.  With ARG, do this that
many times.  A negative argument -N means move forward but still
go down a level.

If ARG is raw prefix argument C-u, descend backward as much as
possible.

If ARG is raw prefix argument C-u C-u, jump to the end of current
list.

If the point is inside sexp and there is no down expression to
descend to, jump to the end of current one.  If moving forward,
jump to beginning of current one."
  (interactive "P")
  (setq arg (prefix-numeric-value arg))
  (sp-down-sexp (- arg)))

(defun sp-up-sexp (&optional arg)
  "Move forward out of one level of parentheses.  With ARG, do
this that many times.  A negative argument means move backward
but still to a less deep spot."
  (interactive "p")
  (setq arg (or arg 1))
  (let ((ok (sp-get-enclosing-sexp (abs arg))))
    (when ok
      (if (> arg 0) (goto-char (sp-get ok :end))
        (goto-char (sp-get ok :beg))))
    ok))

(defun sp-backward-up-sexp (&optional arg)
  "Move backward out of one level of parentheses.  With ARG, do
this that many times.  A negative argument means move forward but
still to a less deep spot."
  (interactive "p")
  (setq arg (or arg 1))
  (sp-up-sexp (- arg)))

(defun sp-kill-sexp (&optional arg)
  "Kill the balanced expression following point.  If point is
inside an expression and there is no following expression, kill
the topmost enclosing expression.

With ARG being positive number N, repeat that many times.

With ARG being Negative number -N, repeat that many times in
backward direction.

With ARG being raw prefix C-u, kill all the expressions from
point up until the end of current list.  With raw prefix - C-u,
kill all the expressions from beginning of current list up until
point.  If point is inside a symbol, this is also killed.  If
there is no expression after/before the point, just delete the
whitespace up until the closing/opening delimiter.

With ARG being raw prefix C-u C-u, kill current list (the list
point is inside).

If ARG is nil, default to 1 (kill single expression forward)

With `sp-navigate-consider-symbols', symbols and strings are also
considered balanced expressions.

Examples. Prefix argument is shown after the example in
\"comment\". Assumes `sp-navigate-consider-symbols' equal to t:

 (foo |(abc) bar)  -> (foo bar) ;; nil, defaults to 1

 (foo (bar) | baz) -> |         ;; 2

 (foo |(bar) baz)  -> |         ;; C-u C-u

 (1 2 |3 4 5 6)    -> (1 2)     ;; C-u

 (1 |2 3 4 5 6)    -> (1 |5 6)  ;; 3

 (1 2 3 4 5| 6)    -> (1 2 3 6) ;; -2

 (1 2 3 4| 5 6)    -> (5 6)     ;; - C-u

 (1 2 |   )        -> (1 2|)    ;; C-u, kill useless whitespace
"
  (interactive "P")
  (setq arg (prefix-numeric-value arg))
  (let ((n (abs arg))
        (raw (sp-raw-argument-p-1 current-prefix-arg))
        (ok t)
        (b (point-max))
        (e (point-min)))
    (cond
     ;; kill to the end or beginning of list
     ((and raw
           (= n 4))
      (let* ((lst (sp-get-list-items))
             (last nil)
             (enc (car lst))
             (beg-in (sp-get enc :beg-in))
             (end-in (sp-get enc :end-in)))
        (!cdr lst)
        (if (> arg 0)
            (progn
              (while (and lst (>= (point) (sp-get (car lst) :end)))
                (setq last (car lst))
                (!cdr lst))
              (kill-region (if last (sp-get last :end) beg-in) end-in))
          (while (and lst (> (point) (sp-get (car lst) :beg-prf))) (!cdr lst))
          (kill-region (if lst (sp-get (car lst) :beg-prf) end-in) beg-in))))
     ;; kill the enclosing list
     ((and raw
           (= n 16))
      (let ((lst (sp-backward-up-sexp)))
        (kill-region (sp-get lst :beg-prf)
                     (sp-get lst :end))))
     ;; regular kill
     (t
      (save-excursion
        (while (and (> n 0) ok)
          (setq ok (sp-forward-sexp (sp--signum arg)))
          (when (< (sp-get ok :beg-prf) b) (setq b (sp-get ok :beg-prf)))
          (when (> (sp-get ok :end) e) (setq e (sp-get ok :end)))
          (setq n (1- n))))
      (when ok
        (kill-region b e)
        ;; kill useless junk whitespace.
        (append-next-kill)
        (kill-region (point)
                     (progn
                       (if (> arg 0)
                           (progn
                             (skip-chars-forward " \t")
                             (when (looking-back " ")
                               (backward-char)))
                         (skip-chars-backward " \t")
                         (when (looking-at " ") (forward-char)))
                       (point)))
        (indent-according-to-mode))))))

(defun sp-backward-kill-sexp (&optional arg)
  "This is exactly like calling `sp-kill-sexp' with -ARG.  In
other words, the direction of all commands is reversed.  For more
information, see the documentation of sp-kill-sexp."
  (interactive "P")
  (setq arg (prefix-numeric-value arg))
  (sp-kill-sexp (- arg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; "paredit" operations

(defmacro sp--slurp-sexp (fw-1)
  "Internal.  Generate forward/backward slurp functions."
  `(let ((n (abs arg))
         (fw (> arg 0)))
     (if fw
         (while (> n 0)
           (let* ((ok (sp-get-enclosing-sexp))
                  (b (and ok (sp-get ok :beg)))
                  (e (and ok (sp-get ok :end)))
                  (ins-space 0)
                  ,@(when (not fw-1) '((prefix-len) prefix))
                  next-thing)
             (if ok
                 (save-excursion
                   (goto-char ,(if fw-1 'e 'b))
                   (setq next-thing (sp-get-thing ,(if fw-1 'nil 't)))
                   (while ,(if fw-1 '(< (sp-get next-thing :beg) b) '(> (sp-get next-thing :end) e))
                     (goto-char (sp-get next-thing ,(if fw-1 :end :beg)))
                     (setq ok next-thing)
                     (setq next-thing (sp-get-thing ,(if fw-1 'nil 't))))
                   (goto-char (sp-get ok ,(if fw-1 :end :beg-prf)))
                   (delete-char
                    ,(if fw-1 '(sp-get ok (- :cl-l)) '(sp-get ok (+ :op-l :prefix-l))))
                   (when ,(if fw-1
                              '(= (sp-get ok :end) (sp-get next-thing :beg-prf))
                            '(= (sp-get ok :beg-prf) (sp-get next-thing :end)))
                     (insert " ")
                     (setq ins-space -1))
                   (goto-char ,(if fw-1
                                   '(sp-get next-thing (- :end-in ins-space))
                                 '(sp-get next-thing :beg-prf)))
                   (insert ,@(if fw-1 '((sp-get ok :cl)) '((sp-get ok :prefix) (sp-get ok :op))))
                   (setq n (1- n)))
               (message "We can't slurp without breaking strictly balanced expression. Ignored.")
               (setq n -1))))
       (,(if fw-1 'sp-backward-slurp-sexp 'sp-forward-slurp-sexp) n))))

(defun sp-forward-slurp-sexp (&optional arg)
  "Add the S-expression following the current list into that list
by moving the closing delimiter.  If the current list is the last
in a parent list, extend that list (and possibly apply
recursively until we can extend a list or end of file).  If arg
is N, apply this function that many times.  If arg is negative
-N, extend the opening pair instead (that is, backward).

Examples:

 (foo |bar) baz   -> (foo |bar baz)

 [(foo |bar)] baz -> [(foo |bar) baz]

 [(foo |bar) baz] -> [(foo |bar baz)]"
  (interactive "p")
  (setq arg (or arg 1))
  (sp--slurp-sexp t))

(defun sp-backward-slurp-sexp (&optional arg)
  "Add the S-expression preceding the current list into that list
by moving the opening delimiter.  If the current list is the
first in a parent list, extend that list (and possibly apply
recursively until we can extend a list or beginning of file).  If
arg is N, apply this function that many times.  If arg is
negative -N, extend the closing pair instead (that is, forward).

Examples:

 foo (bar| baz)   -> (foo bar| baz)

 foo [(bar| baz)] -> [foo (bar| baz)]

 [foo (bar| baz)] -> [(foo bar| baz)]"
  (interactive "p")
  (setq arg (or arg 1))
  (sp--slurp-sexp nil))

(defmacro sp--barf-sexp (fw-1)
  "Internal.  Generate forward/backward barf functions."
  `(let ((n (abs arg)))
     (while (> n 0)
       (let* ((ok (sp-get-enclosing-sexp))
              ,@(when (not fw-1) '((prefix nil) (prefix-len 0)))
              next-thing)
         (if ok
             (save-excursion
               (goto-char ,(if fw-1 '(sp-get ok :end-in)
                             '(sp-get ok :beg-in)))
               ;; NOTE: sp-get-thing is "reversed", if we barf forward
               ;; we search from end of list backward for the thing to
               ;; barf.
               (setq next-thing (sp-get-thing ,fw-1))
               (if (and next-thing
                        (/= (sp-get next-thing :beg) (sp-get ok :beg))) ;; ok == next-thing
                   (progn
                     (delete-char ,(if fw-1 '(sp-get ok :cl-l)
                                     '(sp-get ok (- (+ :op-l :prefix-l)))))
                     (goto-char ,(if fw-1 '(sp-get next-thing :beg)
                                   '(- (sp-get next-thing :end)
                                       (sp-get ok (+ :op-l :prefix-l)))))
                     ,(if fw-1 '(sp-skip-backward-to-symbol t)
                        ;; skip the prefix backward.  We don't have
                        ;; info about this prefix, since it is the
                        ;; "following-sexp" to the one being jumped
                        ;; over -- next-thing
                        '(progn
                           (sp-skip-forward-to-symbol t)
                           (skip-syntax-backward "'")))
                     (insert ,(if fw-1 '(sp-get ok :cl)
                                '(sp-get ok (concat :prefix :op))))
                     (setq n (1- n)))
                 (message "The expression is empty.")
                 (setq n -1)))
           (message "We can't barf without breaking strictly balanced expression. Ignored.")
           (setq n -1))))))

(defun sp-forward-barf-sexp (&optional arg)
  "Remove the last S-expression in the current list by moving the
closing delimiter.

If ARG is positive number N, barf that many expressions.

If ARG is negative number -N, contract the opening pair instead.

If ARG is raw prefix C-u, barf all expressions from the one after
point to the end of current list and place the point before the
closing delimiter of the list.

If the current list is empty, do nothing.

Examples (prefix arg in comment):

  (foo bar| baz)   -> (foo bar|) baz   ;; nil (defaults to 1)

  (foo| [bar baz]) -> (foo|) [bar baz] ;; 1

  (1 2 3| 4 5 6)   -> (1 2 3|) 4 5 6   ;; C-u (or numeric prefix 3)

  (foo bar| baz)   -> foo (bar| baz)   ;; -1"
  (interactive "P")
  (setq arg (prefix-numeric-value arg))
  (if (> arg 0)
      (let ((raw (sp-raw-argument-p-1 current-prefix-arg)))
        (if raw
            (let* ((lst (sp-get-list-items))
                   (last nil))
              (!cdr lst)
              (while (and lst (>= (point) (sp-get (car lst) :beg))) (setq last (car lst)) (!cdr lst))
              (setq arg (length lst))
              (when (/= arg 0)
                (sp--barf-sexp t)
                (when (> (point) (sp-get last :end)) (goto-char (sp-get last :end)))))
          (sp--barf-sexp t)))
    (sp-backward-barf-sexp (- arg))))

(defun sp-backward-barf-sexp (&optional arg)
  "This is exactly like calling `sp-forward-barf-sexp' with -ARG.
In other words, instead of contracting the closing pair, the
opening pair is contracted.  For more information, see the
documentation of sp-forward-barf-sexp."
  (interactive "P")
  (setq arg (prefix-numeric-value arg))
  (if (> arg 0)
      (let ((raw (sp-raw-argument-p-1 current-prefix-arg)))
        (if raw
            (let* ((lst (sp-get-list-items))
                   (n 0))
              (!cdr lst)
              (while (and lst (> (point) (sp-get (car lst) :end))) (!cdr lst) (setq n (1+ n)))
              (setq arg n)
              (when (/= arg 0)
                (sp--barf-sexp nil)
                (when (< (point) (sp-get (car lst) :beg-prf)) (goto-char (sp-get (car lst) :beg-prf)))))
          (sp--barf-sexp nil)))
    (sp-forward-barf-sexp (- arg))))

;; TODO: since most pair characters are in \( class, they are not
;; skipped by this function.  But in some modes, maybe they are
;; considered punctuation or something else. We should test if we look
;; at a pair opener/closer too.
(defmacro sp-skip-to-symbol-1 (forward)
  "Internal.  Generate `sp-skip-forward-to-symbol' or
`sp-skip-backward-to-symbol'."
  (let ((inc (if forward '1+ '1-))
        (dec (if forward '1- '1+))
        (forward-fn (if forward 'forward-char 'backward-char))
        (next-char-fn (if forward 'following-char 'preceding-char)))
    `(let ((in-comment (sp-point-in-comment)))
       (while (and (not (or (eobp)
                            (and stop-after-string
                                 (not (sp-point-in-string))
                                 (sp-point-in-string (,dec (point))))
                            (and stop-at-string
                                 (not (sp-point-in-string))
                                 (sp-point-in-string (,inc (point))))
                            ;; HACK -- fix ` inside strings in emacs modes
                            (and (sp-point-in-string)
                                 (eq (char-syntax (,next-char-fn)) ?')
                                 (member (,next-char-fn) '(?` ?')))))
                   (or (member (char-syntax (,next-char-fn)) '(?< ?> ?! ?| ?\ ?\" ?' ?.))
                       (unless in-comment (sp-point-in-comment))))
         (,forward-fn 1)))))

(defun sp-skip-forward-to-symbol (&optional stop-at-string stop-after-string)
  "Skip whitespace and comments moving forward.  If STOP-AT-STRING is
non-nil, stop before entering a string (if not already in a string).
If STOP-AFTER-STRING is non-nil, stop after exiting a string."
  (sp-skip-to-symbol-1 t))

(defun sp-skip-backward-to-symbol (&optional stop-at-string stop-after-string)
  "Skip whitespace and comments moving backward.  If STOP-AT-STRING is
non-nil, stop before entering a string (if not already in a string).
If STOP-AFTER-STRING is non-nil, stop after exiting a string."
  (sp-skip-to-symbol-1 nil))

(defmacro sp-forward-symbol-1 (fw-1)
  (let ((goto-where (if fw-1 '(match-end 0) '(match-beginning 0)))
        (look-at-open (if fw-1 '(looking-at open) '(sp-looking-back open sp-max-pair-length-c t)))
        (look-at-close (if fw-1 '(looking-at close) '(sp-looking-back close sp-max-pair-length-c t))))
    `(let ((n (abs arg))
           (fw (> arg 0))
           (open (sp-get-opening-regexp-1))
           (close (sp-get-closing-regexp-1)))
       (if fw
           (while (> n 0)
             ;; First we need to get to the beginning of a symbol.  This means
             ;; skipping all whitespace and pair delimiters until we hit
             ;; something in \sw or \s_
             (while (cond ((not (memq
                                 (char-syntax (,(if fw-1 'following-char 'preceding-char)))
                                 '(?w ?_)))
                           (,(if fw-1 'forward-char 'backward-char)) t)
                          (,look-at-open
                           (goto-char ,goto-where))
                          (,look-at-close
                           (goto-char ,goto-where))))
             (while (and ,(if fw-1 '(not (eobp)) '(not (bobp)))
                         (not (or ,look-at-open
                                  ,look-at-close))
                         (memq (char-syntax
                                (,(if fw-1 'following-char 'preceding-char)))
                               '(?w ?_)))
               (,(if fw-1 'forward-char 'backward-char)))
             (setq n (1- n)))
         (,(if fw-1 'sp-backward-symbol 'sp-forward-symbol) n)
         ))))

(defun sp-forward-symbol (&optional arg)
  "Move point to the next position that is the end of a symbol.
A symbol is any sequence of characters that are in either the
word constituent or symbol constituent syntax class.  Current
symbol only extend to the possible opening or closing delimiter
as defined by `sp-add-pair' even if part of this delimiter
would match \"symbol\" syntax classes."
  (interactive "p")
  (setq arg (or arg 1))
  (sp-forward-symbol-1 t))

(defun sp-backward-symbol (&optional arg)
  "Move point to the next position that is the beginning of a symbol.
A symbol is any sequence of characters that are in either the word
constituent or symbol constituent syntax class.  Current symbol only
extend to the possible opening or closing delimiter as defined by
`sp-add-pair' even if part of this delimiter would match \"symbol\"
syntax classes."
  (interactive "p")
  (setq arg (or arg 1))
  (sp-forward-symbol-1 nil))

(defun sp--unwrap-sexp (sexp)
  "Internal.  Unwraps expression."
  (delete-region
   (sp-get sexp :beg-prf)
   (sp-get sexp :beg-in))
  (delete-region
   (sp-get sexp (- :end-in :op-l :prefix-l))
   (sp-get sexp (- :end :op-l :prefix-l))))

(defun sp-unwrap-sexp (&optional arg)
  "Unwrap the following expression.  With arg N, unwrap Nth expression
as returned by `sp-forward-sexp'.  If arg is negative -N, unwrap Nth
expression backwards as returned by `sp-backward-sexp'."
  (interactive "p")
  (setq arg (or arg 1))
  (let ((sp-navigate-consider-symbols nil))
    (let ((ok (save-excursion (sp-forward-sexp arg))))
      (when ok (sp--unwrap-sexp ok)))))

(defun sp-backward-unwrap-sexp (&optional arg)
  "Unwrap the previous expression.  With arg N, unwrap Nth expression
as returned by `sp-backward-sexp'.  If arg is negative -N, unwrap Nth
expression forward as returned by `sp-forward-sexp'."
  (interactive "p")
  (sp-unwrap-sexp (- (or arg 1))))

(defun sp-splice-sexp (&optional arg)
  "Unwrap the current list.  With arg N, unwrap Nth list as
returned by applying `sp-up-sexp' N times.  This function
expect positive arg."
  (interactive "p")
  (setq arg (or arg 1))
  (let ((ok (sp-get-enclosing-sexp arg)))
    (when ok (sp--unwrap-sexp ok))))

;; TODO: add some sane automatic re-indentation/deletion of whitespace
(defun sp-splice-sexp-killing-backward ()
  "Unwrap the current list and also kill all the content between
start of this list and the point.

Examples:

  (foo (let ((x 5)) | (sqrt n)) bar) -> (foo | (sqrt n) bar)"
  (interactive)
  (let* ((ok (sp-get-enclosing-sexp 1)))
    (when ok
      (sp--unwrap-sexp ok)
      (delete-region (sp-get ok :beg-prf) (point)))))

(defun sp-splice-sexp-killing-forward ()
  "Unwrap the current list and also kill all the content between the
point and the end of this list.

Examples:

  (a (b c| d e) f) -> (a b c| f)"
  (interactive)
  (let* ((ok (sp-get-enclosing-sexp 1)))
    (when ok
      (sp--unwrap-sexp ok)
      (delete-region
       (point)
       (sp-get ok (- :end-in :op-l :prefix-l))))))

(defun sp-splice-sexp-killing-around (&optional arg)
  "Unwrap the current list and also kill everything inside save
for ARG next expressions.  With ARG negative N, save that many
expressions backward.

Examples:

  (a b |(c d) e f) -> |(c d)   ;; with arg = 1
  (a b |c d e f)   -> |c d     ;; with arg = 2
  (- (car x) |a 3) -> (car x)| ;; with arg = -1"
  (interactive "p")
  (setq arg (or arg 1))
  (let ((ok (sp-get-enclosing-sexp)) str)
    (when ok
      (sp-select-next-thing-exchange arg)
      (setq str (buffer-substring-no-properties (region-beginning) (region-end)))
      (delete-region (sp-get ok :beg-prf) (sp-get ok :end))
      (save-excursion (insert str)))))

(defun sp-forward-whitespace ()
  "Skip forward past the whitespace characters."
  (interactive)
  (skip-chars-forward " \t\n"))

(defun sp-backward-whitespace ()
  "Skip backward past the whitespace characters."
  (interactive)
  (skip-chars-backward " \t\n"))

(defun sp-split-sexp ()
  "Split the list or string the point is on into two."
  (interactive)
  (let ((ok (sp-get-enclosing-sexp 1)))
    (when ok
      (forward-char (- (prog1 (sp-backward-whitespace) (insert (sp-get ok :cl)))))
      (save-excursion (sp-forward-whitespace) (insert (sp-get ok :op))))))

(defun sp-select-next-thing (&optional arg)
  "Set active region over ARG next things as recognized by
`sp-get-thing'.  If ARG is negative -N, select that many
expressions backward.

If ARG is a raw prefix C-u select all the things up until the
end of current expression.

If ARG is a raw prefix C-u C-u select the current expression (as
if doing `sp-backward-up-sexp' followed by
`sp-select-next-thing').

With `sp-navigate-consider-symbols' symbols and strings are also
considered balanced expressions."
  (interactive "P")
  (setq arg (prefix-numeric-value arg))
  (let* ((raw (sp-raw-argument-p-1 current-prefix-arg))
         (first (sp-forward-sexp (sp--signum arg)))
         (last first)
         (b (if first
                (if (> arg 0) (sp-get first :beg-prf) (sp-get first :end))
              (error "No following expressions.")))
         (e (cond
             ;; select things up until the end of current list,
             ;; ignoring whitespace and possible comments inside
             ((and raw
                   (= (abs arg) 4))
              (let ((enc (sp-get-enclosing-sexp)))
                (if enc
                    (save-excursion
                      (if (> arg 0)
                          (progn
                            (goto-char (sp-get enc :end-in))
                            (sp-skip-backward-to-symbol t)
                            (point))
                        (goto-char (sp-get enc :beg-in))
                        (sp-skip-forward-to-symbol t)
                        (point)))
                  (error "No enclosing expression."))))
             ;; select current list
             ((and raw
                   (= (abs arg) 16))
              (let ((enc (sp-get-enclosing-sexp)))
                ;; UGLY! We override b here. (can we even do that
                ;; safely in elisp?)
                (if (not enc)
                    (error "No enclosing expression.")
                  (setq b (sp-get enc :beg-prf))
                  (sp-get enc :end))))
             ;; regular select, just select ARG things
             (t
              (when (> (abs arg) 1)
                (setq last (sp-forward-sexp (* (sp--signum arg) (1- (abs arg))))))
              (if (> arg 0) (sp-get last :end) (sp-get last :beg-prf))))))
    (push-mark nil t)
    (set-mark b)
    (goto-char e)))

(defun sp-select-previous-thing (&optional arg)
  "Set active region over ARG previous things as recognized by
`sp-get-thing'.  If ARG is negative -N, select that many
expressions forward.

With `sp-navigate-consider-symbols' symbols and strings are also
considered balanced expressions."
  (interactive "p")
  (sp-select-next-thing (- (or arg 1))))

(defun sp-select-next-thing-exchange (&optional arg)
  "Just like `sp-select-next-thing' but run
`exchange-point-and-mark' afterwards."
  (interactive "p")
  (sp-select-next-thing arg)
  (exchange-point-and-mark))

(defun sp-select-previous-thing-exchange (&optional arg)
  "Just like `sp-select-previous-thing' but run
`exchange-point-and-mark' afterwards."
  (interactive "p")
  (sp-select-previous-thing arg)
  (exchange-point-and-mark))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; show-smartparens-mode

(defgroup show-smartparens nil
  "Show smartparens minor mode."
  :group 'smartparens)

(defcustom sp-show-pair-delay 0.125
  "Time in seconds to delay before showing a matching pair."
  :type '(number :tag "seconds")
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

(defvar sp-show-pair-idle-timer nil)

(defvar sp-show-pair-overlays nil)

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
                                   'sp-show-pair-function)))
    (when sp-show-pair-overlays
      (sp-show-pair-delete-overlays))))

;;;###autoload
(define-globalized-minor-mode show-smartparens-global-mode
  show-smartparens-mode
  turn-on-show-smartparens-mode)

;;;###autoload
(defun turn-on-show-smartparens-mode ()
  "Turn on `show-smartparens-mode'."
  (interactive)
  (unless (member major-mode sp-ignore-modes-list)
    (show-smartparens-mode t)))

;;;###autoload
(defun turn-off-show-smartparens-mode ()
  "Turn off `show-smartparens-mode'."
  (interactive)
  (show-smartparens-mode nil))

(defun sp-show-pair-function ()
  "Display the show pair overlays."
  (when show-smartparens-mode
    (let* ((pair-list (sp-get-pair-list-1))
           (opening (regexp-opt (--map (car it) pair-list)))
           (closing (regexp-opt (--map (cdr it) pair-list)))
           ok match)
      (cond
       ((looking-at opening)
        (setq match (match-string 0))
        (setq ok (sp-get-sexp))
        (if ok
            (sp-get ok (sp-show-pair-create-overlays :beg :end :op-l :cl-l))
          (sp-show-pair-create-mismatch-overlay (point) (length match))))
       ((sp-looking-back closing sp-max-pair-length-c t)
        (setq match (match-string 0))
        (setq ok (sp-get-sexp t))
        (if ok
            (sp-get ok (sp-show-pair-create-overlays :beg :end :op-l :cl-l))
          (sp-show-pair-create-mismatch-overlay (- (point) (length match))
                                                (length match))))
       (sp-show-pair-overlays
        (sp-show-pair-delete-overlays))))))

(defun sp-show-pair-create-overlays (start end olen clen)
  "Create the show pair overlays."
  (when sp-show-pair-overlays
    (sp-show-pair-delete-overlays))
  (let* ((oleft (make-overlay start (+ start olen) nil t nil))
         (oright (make-overlay (- end clen) end nil t nil)))
    (setq sp-show-pair-overlays (cons oleft oright))
    (overlay-put oleft 'face 'sp-show-pair-match-face)
    (overlay-put oright 'face 'sp-show-pair-match-face)
    (overlay-put oleft 'priority 1000)
    (overlay-put oright 'priority 1000)
    (overlay-put oleft 'type 'show-pair)))

(defun sp-show-pair-create-mismatch-overlay (start len)
  "Create the mismatch pair overlay."
  (when sp-show-pair-overlays
    (sp-show-pair-delete-overlays))
  (let ((o (make-overlay start (+ start len) nil t nil)))
    (setq sp-show-pair-overlays (cons o nil))
    (overlay-put o 'face 'sp-show-pair-mismatch-face)
    (overlay-put o 'priority 1000)
    (overlay-put o 'type 'show-pair)))

(defun sp-show-pair-delete-overlays ()
  "Remove both show pair overlays."
  (when sp-show-pair-overlays
    (when (car sp-show-pair-overlays)
      (delete-overlay (car sp-show-pair-overlays)))
    (when (cdr sp-show-pair-overlays)
      (delete-overlay (cdr sp-show-pair-overlays)))
    (setq sp-show-pair-overlays nil)))

;; global initialization
(sp-update-pair-triggers)
(defadvice delete-backward-char (before sp-delete-pair-advice activate)
  (sp-delete-pair (ad-get-arg 0)))
(add-hook 'post-command-hook 'sp-post-command-hook-handler)
(add-hook 'pre-command-hook 'sp-pre-command-hook-handler)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Default configuration

;; ban insertion of apostrophe (') in strings, as it is used as
;; contraction marker in many natural languages
(sp-add-ban-insert-pair-in-string "'")

;; Also disable it in common text modes
(sp-add-local-ban-insert-pair "'" '(
                                    fundamental-mode
                                    text-mode
                                    tex-mode
                                    plain-tex-mode
                                    latex-mode
                                    markdown-mode
                                    gfm-mode
                                    rst-mode
                                    org-mode
                                    log-edit-mode
                                    ))

;; emacs is lisp hacking enviroment, so we set up some most common
;; lisp modes too
(sp-with '(
           emacs-lisp-mode
           inferior-emacs-lisp-mode
           lisp-interaction-mode
           scheme-mode
           common-lisp-mode
           )
  ;; disable ' everywhere, it's the quote character!
  (sp-add-local-ban-insert-pair "'")
  ;; also disable the pseudo-quote inside code.  We keep it in
  ;; commends and strings for hyperlinks
  (sp-add-local-ban-insert-pair-in-code "`"))

;; markdown based modes
(sp-with '(
           markdown-mode
           gfm-mode
           rst-mode
           )
  ;; overload the `' pair with ``, which is used for inline
  ;; code in markdown
  (sp-add-local-pair "`" "`"))

;; LaTeX modes
(sp-add-pair "$" "$")
(sp-with '(
           tex-mode
           plain-tex-mode
           latex-mode
           )
  ;; allow the dollar pair only in LaTeX related modes.  It
  ;; often marks a variable elsewhere
  (sp-add-local-allow-insert-pair "$"))

(provide 'smartparens)

;;; smartparens.el ends here
