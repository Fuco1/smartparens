;;; smartparens.el --- Autoinsert pairs of defined brackets and wrap regions

;; Copyright (C) 2012 Matus Goljer

;; Author: Matus Goljer <matus.goljer@gmail.com>
;; Maintainer: Matus Goljer <matus.goljer@gmail.com>
;; Created: 17 Nov 2012
;; Version: 0.7
;; Keywords: abbrev convenience editing
;; Package-Requires: ((dash "1.0"))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customize & Mode definitions

(defgroup smartparens ()
  "Smartparens minor mode"
  :group 'editor)

(defvar sp-keymap (make-sparse-keymap)
  "Keymap used for smartparens-mode. Remaps all the trigger keys
to `self-insert-command'. This means we lose some functionality
in some modes (like c-electric keys).")

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
        ;; set the escape char
        (dotimes (char 256)
          (unless sp-escape-char
            (if (= ?\\ (char-syntax char))
                (setq sp-escape-char (string char)))))
        (run-hooks 'smartparens-enabled-hook))
    (run-hooks 'smartparens-disabled-hook)
    ))

(defun sp-update-pair-triggers ()
  "Update the `sp-keymap' to include all trigger keys. Trigger
key is any character present in any pair. Each trigger key must
map to `self-insert-command'."
  (setcdr sp-keymap nil)
  (let ((triggers (-distinct
            (split-string
             (apply #'concat
                    (--reduce-from (cons (car it)
                                         (cons (cdr it) acc))
                                   nil sp-pair-list))
             "" t))))
    (--each triggers (define-key sp-keymap it 'self-insert-command))))

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

(defvar smartparens-enabled-hook nil
  "Called after `smartparens-mode' is turned on.")

(defvar smartparens-disabled-hook nil
  "Called after `smartparens-mode' is turned off.")

;;;###autoload
(define-globalized-minor-mode smartparens-global-mode
  smartparens-mode
  turn-on-smartparens-mode)

;;;###autoload
(defun turn-on-smartparens-mode ()
  "Turn on `smartparens-mode'."
  (interactive)
  (unless (or (member major-mode sp-ignore-modes-list)
              (minibufferp))
    (smartparens-mode t)))

;;;###autoload
(defun turn-off-smartparens-mode ()
  "Turn off `smartparens-mode'."
  (interactive)
  (smartparens-mode nil))

;; global custom
(defcustom sp-ignore-modes-list '(
                                  calc-mode
                                  dired-mode
                                  ibuffer-mode
                                  minibuffer-inactive-mode
                                  sr-mode
                                  )
  "Modes where smartparens mode is inactive if allowed globally."
  :type '(repeat symbol)
  :group 'smartparens)

;; insert custom
(defcustom sp-autoinsert-pair t
  "If non-nil, auto insert pairs.  See `sp-insert-pair'."
  :type 'boolean
  :group 'smartparens)

(defcustom sp-autoinsert-if-followed-by-same 2
  "Customizes behaviour of pair insertion if the point is followed by
the same opening pair as currently inserted pair.

The first option does not change the insertion behaviour and pairs are
inserted normally. For example |() followed by ( would produce (|)().

The second option inserts the pair only if the opening pair
following point is not the same as currently inserted pair. For
example |() followed by ( would produce (|(). If next character
isn't part of any pair, insert normally.

The third option behaves as second, but if the opening and closing
pairs are the same, and we are looking at the closing pair, insert the
whole pair. For example \"|\" followed by \" produce \"\"|\"\". This
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

(defcustom sp-autoskip-closing-pair t
  "If non-nil, skip the following closing pair. See
`sp-skip-closing-pair' for more info."
  :type 'boolean
  :group 'smartparens)

(defcustom sp-cancel-autoskip-on-backward-movement t
  "If non-nil, autoskip of closing pair is cancelled not only
when point is moved outside of the pair, but also if the point
moved backwards. See `sp-skip-closing-pair' for more info."
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
  "If non-nil, wrap the active region with pair. See `sp-wrap-region' and `sp-wrap-region-init'"
  :type 'boolean
  :group 'smartparens)

(defcustom sp-autodelete-wrap t
  "If non-nil, auto delete both opening and closing pair of most
recent wrapping. Deletion command must be the very first
command after the insertion, otherwise normal behaviour is
applied."
  :type 'boolean
  :group 'smartparens)

(defvar sp-delete-selection-mode nil
  "If non-nil, smartparens will emulate
  `delete-selection-mode'. You must also disable the
  `delete-selection-mode' for this to work correctly! The emulation
  mode works exactly the same, indeed, it simply calls the
  original `delete-selection-pre-hook' function. However, if there's
  a possibility of wrapping, it wraps the active region
  instead.")
(defun sp-turn-on-delete-selection-mode ()
  (interactive)
  (setq sp-delete-selection-mode t)
  ;; make sure the `delete-selection-pre-hook' is not active and that
  ;; delsel is actually loaded. We need the delete-selection-pre-hook
  ;; command!
  (require 'delsel)
  (remove-hook 'pre-command-hook 'delete-selection-pre-hook)
  )
(defun sp-turn-off-delete-selection-mode ()
  (interactive)
  (setq sp-delete-selection-mode nil)
  )

;; escaping custom
(defcustom sp-autoescape-string-quote t
  "If non-nil, autoescape string quotes if typed inside string."
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
;; Variables

(defvar sp-local-ban-insert-pair '()
  "For pairs on this list auto insertion is locally disabled in
specified modes.

List of elements of type (command . '(list of modes)).")

(defvar sp-global-ban-insert-pair '()
  "For pairs on this list auto insertion is disabled globally.

List of pair IDs.")

(defvar sp-local-allow-insert-pair '()
  "For pairs on this list auto insertion is locally enabled in
specified modes. It is disabled in all other modes automatically.

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
comment. It is disabled in all other modes automatically.

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
specific modes if the point is inside code. It is disabled in all
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
                       ("<"     . ">")
                       ("`"     . "'") ;; tap twice for tex double quotes
                       )
  "List of pairs for auto-insertion or wrapping. Maximum length
of opening or closing pair is 10 characters.")
(make-variable-buffer-local 'sp-pair-list)

(defvar sp-local-pair-list '()
  "List of pairs specific to a specific mode. The pairs on this list
are not enabled globally. Pairs in this list can override global
definitons. For example default `' can be overriden with `` in
markdown-mode.

List of elements of type (mode . '((open . close))).")

(defvar sp-tag-pair-list '(
                           ("<" . (
                                   ((sgml-mode html-mode emacs-lisp-mode) "<_>" "</_>" sp-match-sgml-tags)
                                   ))
                           )

  "List of tag pairs. Some languages use more elaborate tag pairs,
such as html \"<span class=\"x\">something</span>\". For these,
the standard wrap mechanism isn't sufficient. Here, user can
define the syntax of opening and closing pair. If they share the
same keyword, this is substituted for _ in the tag
definitions. The matching-function can further transform the
substitution for closing tab, for example cutting out everything
after a space in html-tag. Only one _ per tag is allowed.

The internal format is a list of (open-trigger ((modes) open-tag
close-tag matching-funciton)) where:

open-trigger - the trigger that starts the tag insertion.

open-tag - format of the opening tag. _ is replaced by the entered
text.

close-tag - format of the closing tag. _ is replaced by the entered
text.

modes - list of modes where this wrapping is allowed.

matching-function - this function is called to perform a
transformation on text that will be substituted to closing tag.")

(defvar sp-last-operation nil
  "Symbol holding the last successful operation.")
(make-variable-buffer-local 'sp-last-operation)

(defvar sp-escape-char nil
  "Character used to escape quotes inside strings.")
(make-variable-buffer-local 'sp-escape-char)

(defvar sp-previous-point -1
  "Location of point before last command. This is only updated
when some pair-overlay is active. Do not rely on the value of
this variable anywhere else!")
(make-variable-buffer-local 'sp-previous-point)

(defvar sp-wrap-point nil
  "Save the value of point before attemt to wrap a region. Used
for restoring the original state if the wrapping is
cancelled.")
(make-variable-buffer-local 'sp-wrap-point)

(defvar sp-wrap-mark nil
  "Save the value of point before attemt to wrap a region. Used
for restoring the original state if the wrapping is
cancelled.")
(make-variable-buffer-local 'sp-wrap-mark)

(defvar sp-last-inserted-character ""
  "If wrapping is cancelled, these character are re-inserted to
the location of point before the wrapping.")
(make-variable-buffer-local 'sp-last-inserted-character)

(defvar sp-last-wrapped-region nil
  "List containing info about last wrapped region. The content of the list is:
\(start-of-the-wrapped-region end-of-the-wrapped-region
length-of-opening-pair length-of-closing-pair\). Start and end
positions include the newly added wrapping pair.")
(make-variable-buffer-local 'sp-last-wrapped-region)

(defvar sp-point-inside-string nil
  "t if point is inside a string.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc functions

(defmacro !delete (elm list)
  "Destructive: Sets LIST to (delete ELM LIST)."
  `(setq ,list (delete ,elm ,list)))

(defmacro sp-with (arg &rest forms)
  `(progn
     ,@(mapcar (lambda (form) (append form (list arg))) forms)))
(font-lock-add-keywords 'emacs-lisp-mode '(("\\<sp-with\\>" . font-lock-keyword-face)) 'append)

(defun reverse-string (str)
  "Reverse the string STR."
  (concat (reverse (append str nil))))

(defun sp-point-in-string (&optional p)
  "Return t if point is inside string or documentation string. If
optional argument P is present, test this instead of point."
  (let ((face (plist-get (text-properties-at (if p p (point))) 'face)))
    (or (eq 'font-lock-string-face face)
        (eq 'font-lock-doc-face face) ;; I wonder how this works
                                      ;; outside emacs-lisp-mode
        )))

(defun sp-point-in-string-or-comment (&optional p)
  "Return t if point is inside string, documentation string or a
comment. If optional argument P is present, test this instead
of point."
  (let ((face (plist-get (text-properties-at (if p p (point))) 'face)))
    (or (eq 'font-lock-string-face face)
        (eq 'font-lock-doc-face face)
        (eq 'font-lock-comment-face face))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Adding/removing of pairs/bans/allows etc.

(defun sp-add-to-ordered-list (elm list order)
  "Add ELM to the LIST ordered by comparator ORDER. The list is
ordered in descending order."
  (if (not list) (list elm)
  (if (funcall order elm (car list))
      (cons elm list)
      (cons (car list) (sp-add-to-ordered-list elm (cdr list) order)))))

(defun sp-order-pairs (a b)
  "Compare two pairs A and B by open pair length. Return t if A
is leq to B."
  (let ((la (length (car a)))
        (lb (length (car b))))
    (>= la lb)))

(defun sp-add-pair (open close &rest banned-modes)
  "Add a pair formed by OPEN and CLOSE to the pair list. See
variable `sp-pair-list' for current list.

Additional arguments are interpreted as modes where this pair
should be banned by default. BANNED-MODES can also be a list."
  (unless (--any? (equal open (car it)) sp-pair-list)
    (setq-default sp-pair-list
                 (sp-add-to-ordered-list (cons open close) sp-pair-list #'sp-order-pairs))
    (sp-add-local-ban-insert-pair open banned-modes)
    (sp-update-pair-triggers)
  ))

(defun sp-remove-pair (open)
  "Remove a pair from the pair list. See variable `sp-pair-list'
for current list."
  (setq-default sp-pair-list
               (--remove (equal open (car it)) sp-pair-list))
  (sp-remove-local-ban-insert-pair open)
  (sp-remove-local-allow-insert-pair open)
  (sp-update-pair-triggers))

(defun sp-add-local-pair (pair mode)
  "Add a pair to the local pair list. Use this only if you need
to overload a global pair with the same ID. If you wish to
limit a pair to a certain mode, add it globally and then set
the permissions with `sp-add-local-allow-insert-pair'."
  (sp-add-to-permission-list pair sp-local-pair-list mode)
  (sp-update-local-pairs))

(defun sp-remove-local-pair (open mode)
  "Remove a pair from the local pair list."
  (sp-remove-from-permission-list (assoc open sp-pair-list) sp-local-pair-list mode)
  (sp-update-local-pairs))

(defun sp-add-ban-insert-pair (&rest open)
  "Add the pairs with ids in OPEN to the global insertion
banlist. That means that these pairs will never be used for auto
insertion. They can still be used for wrapping."
  (setq sp-global-ban-insert-pair (-union sp-global-ban-insert-pair (-flatten open))))

(defun sp-remove-ban-insert-pair (&rest open)
  "Remove the pairs with ids in OPEN from the global insertion
banlist."
  (setq sp-global-ban-insert-pair (-difference sp-global-ban-insert-pair (-flatten open))))

(defun sp-add-ban-insert-pair-in-string (&rest open)
  "Add the pairs with ids in OPEN to the global \"in string\"
insertion banlist. That means that these pairs will never be used
for auto insertion if the point is inside string. They can still
be used for wrapping."
  (setq sp-global-ban-insert-pair-in-string (-union sp-global-ban-insert-pair-in-string (-flatten open))))

(defun sp-remove-ban-insert-pair-in-string (&rest open)
  "Remove the pairs with ids in OPEN from the global \"in
string\" insertion banlist."
  (setq sp-global-ban-insert-pair-in-string (-difference sp-global-ban-insert-pair-in-string (-flatten open))))

(defun -union (list1 list2)
  "Return a new list containing the elements of LIST1 and
elements of LIST2 that were not present in LIST1. The test for
equality is done with `equal', or with `-compare-fn' if that's
non-nil."
  (let ((result (nreverse list1)))
    (--each list2 (when (not (-contains? result it)) (!cons it result)))
    (nreverse result)))

(defmacro --last (form list)
  "Anaphoric form of `-last'."
  (let ((n (make-symbol "needle")))
    `(let (,n)
       (--each ,list (not ,n)
         (when ,form (setq ,n it)))
       ,n)))

(defun -last (pred list)
  "Return the first x in LIST where (PRED x) is non-nil, else nil."
  (--first (funcall pred it) list))

(defmacro sp-add-to-permission-list (open list &rest modes)
  "Add MODES to the pair with id OPEN in the LIST. See
permissions system for more details."
  (let ((m (make-symbol "new-modes")))
    `(let ((,m (-flatten ,@modes)))
       (when ,m
         (let ((current (--first (equal ,open (car it)) ,list)))
           (if current
               (setcdr current (-union (cdr current) ,m))
             (!cons (cons ,open ,m) ,list)))))))

(defun sp-add-local-ban-insert-pair (open &rest modes)
  "Ban autoinsertion of pair with id OPEN in modes MODES. See
`sp-insert-pair'."
  (sp-add-to-permission-list open sp-local-ban-insert-pair modes))

(defun sp-add-local-allow-insert-pair (open &rest modes)
  "Allow autoinsertion of pair with id OPEN in modes MODES. See
`sp-insert-pair'."
  (sp-add-to-permission-list open sp-local-allow-insert-pair modes))

(defun sp-add-local-ban-insert-pair-in-string (open &rest modes)
  "Ban autoinsertion of pair with id OPEN in modes MODES if point
is inside string, docstring or comment. See `sp-insert-pair'."
  (sp-add-to-permission-list open sp-local-ban-insert-pair-in-string modes))

(defun sp-add-local-allow-insert-pair-in-string (open &rest modes)
  "Allow autoinsertion og pair with id OPEN in MODES if point is
inside string, docstring or comment. See `sp-insert-pair'."
  (sp-add-to-permission-list open sp-local-allow-insert-pair-in-string modes))

(defun sp-add-local-ban-insert-pair-in-code (open &rest modes)
  "Ban autoinsertion of pair with id OPEN in modes MODES if point
is inside code. See `sp-insert-pair'."
  (sp-add-to-permission-list open sp-local-ban-insert-pair-in-code modes))

(defun sp-add-local-allow-insert-pair-in-code (open &rest modes)
  "Allow autoinsertion og pair with id OPEN in MODES if point is
inside code. See `sp-insert-pair'."
  (sp-add-to-permission-list open sp-local-allow-insert-pair-in-code modes))

(defmacro sp-remove-from-permission-list (open list &rest modes)
  "Removes MODES from the pair with id OPEN in the LIST. See
permissions system for more details. If modes is nil, remove the
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

(defun sp-remove-local-ban-insert-pair (open &rest modes)
  "Remove previously set restriction on pair with id OPEN in
modes MODES. If MODES is nil, remove all the modes."
  (sp-remove-from-permission-list open sp-local-ban-insert-pair modes))

(defun sp-remove-local-allow-insert-pair (open &rest modes)
  "Remove previously set restriction on pair with id OPEN in
modes MODES. If MODES is nil, remove all the modes"
  (sp-remove-from-permission-list open sp-local-allow-insert-pair modes))

(defun sp-remove-local-ban-insert-pair-in-string (open &rest modes)
  "Remove previously set restriction on pair with id OPEN in
modes MODES if the point is inside string, docstring or
comment. If MODES is nil, remove all the modes."
  (sp-remove-from-permission-list open sp-local-ban-insert-pair-in-string modes))

(defun sp-remove-local-allow-insert-pair-in-string (open &rest modes)
  "Remove previously set restriction on pair with id OPEN in
modes MODES if the point is inside string, docstring or
comment. If MODES is nil, remove all the modes"
  (sp-remove-from-permission-list open sp-local-allow-insert-pair-in-string modes))

(defun sp-remove-local-ban-insert-pair-in-code (open &rest modes)
  "Remove previously set restriction on pair with id OPEN in
modes MODES if the point is inside code. If MODES is nil,
remove all the modes."
  (sp-remove-from-permission-list open sp-local-ban-insert-pair-in-code modes))

(defun sp-remove-local-allow-insert-pair-in-code (open &rest modes)
  "Remove previously set restriction on pair with id OPEN in
modes MODES if the point is inside code. If MODES is nil,
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
  "List of overlays used for tracking inserted pairs. When a pair
is inserted, an overlay is created over it. When the user starts
typing the closing pair we will not insert it again. If user
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
(define-key sp-pair-overlay-keymap (kbd "C-g") 'sp-remove-active-overlay)

(defvar sp-wrap-overlay-keymap (make-sparse-keymap)
  "Keymap for the wrap overlays.")
(define-key sp-wrap-overlay-keymap (kbd "C-g") 'sp-wrap-cancel)

(defvar sp-wrap-tag-overlay-keymap (make-sparse-keymap)
  "Keymap for the wrap tag overlays.")
(define-key sp-wrap-tag-overlay-keymap (kbd "C-g") 'sp-wrap-tag-done)
(define-key sp-wrap-tag-overlay-keymap (kbd "C-a") 'sp-wrap-tag-beginning)
(define-key sp-wrap-tag-overlay-keymap (kbd "C-e") 'sp-wrap-tag-end)


(defun sp-point-in-overlay-p (overlay)
  "Return t if point is in OVERLAY."
  (and (< (point) (overlay-end overlay))
       (> (point) (overlay-start overlay))))

(defun sp-get-overlay-length (overlay)
  "Compute the length of OVERLAY."
  (- (overlay-end overlay) (overlay-start overlay)))

(defun sp-get-active-overlay ()
  "Get active overlay. Active overlay is the shortest overlay at
point."
  (let ((overlays (overlays-at (point))))
    (cond
     ((not overlays) nil)
     ((not (cdr overlays)) (car overlays))
     (t
      (--reduce (if (< (sp-get-overlay-length it) (sp-get-overlay-length acc)) it acc) overlays)))))

(defun sp-pair-overlay-create (start end id)
  "Create an overlay over the currently inserted pair for
tracking the position of the point."
  (let ((overlay (make-overlay start end nil nil t)))
    (overlay-put overlay 'priority 100)
    (overlay-put overlay 'keymap sp-pair-overlay-keymap)
    (overlay-put overlay 'pair-id id)
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
    (when (and sp-delete-selection-mode can-delete)
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
      (insert sp-last-inserted-character))
  ))

(defun sp-pair-overlay-fix-highlight ()
  "Fix highlighting of the pair overlays. Only the active overlay
should be highlighted."
  (when sp-highlight-pair-overlay
    (--each (overlays-at (point)) (overlay-put it 'face nil))
    (let ((active (sp-get-active-overlay)))
      (if active
          (if (eq 'wrap-tag (overlay-get active 'type))
              (when sp-highlight-wrap-tag-overlay
                (overlay-put active 'face 'sp-wrap-tag-overlay-face))
            (overlay-put active 'face 'sp-pair-overlay-face))
        ;; edge case where we're at the end of active overlay. If
        ;; there is a wrap-tag overlay, restore it's face
        (when sp-wrap-tag-overlays
          (overlay-put (car sp-wrap-tag-overlays) 'face 'sp-wrap-tag-overlay-face))
        ))))

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

(defun sp-remove-active-overlay ()
  "Deactivate the active overlay.  See `sp-get-active-overlay'."
  (interactive)
  (let ((active-overlay (sp-get-active-overlay)))
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
    ;; is active. Therefore, we need to reset this to 1. If not, newly
    ;; created overlay could be removed right after creation - if
    ;; sp-previous-point was greater than actual point
    (setq sp-previous-point -1))
  (message "Removing pair-insertion overlay %s" overlay)
  (delete-overlay overlay)
  (sp-pair-overlay-fix-highlight))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pair insertion/deletion/skipping

(defun sp-insert-pair-in-string-p (id mode)
  "Return t if we can insert pair ID in MODE inside string,
docstring or comment. See `sp-insert-pair' for more info."
  (let ((local-ban-in-string (member mode (cdr (assoc id sp-local-ban-insert-pair-in-string))))
        (local-allow-in-string (cdr (assoc id sp-local-allow-insert-pair-in-string))))
    (cond
     (local-allow-in-string (member mode local-allow-in-string))
     ((member id sp-global-ban-insert-pair-in-string) nil)
     (local-ban-in-string nil)
     (t t))))

(defun sp-insert-pair-in-code-p (id mode)
  "Return t if we can insert pair ID in MODE inside code. See
`sp-insert-pair' for more info."
  (let ((local-ban-in-code (member mode (cdr (assoc id sp-local-ban-insert-pair-in-code))))
        (local-allow-in-code (cdr (assoc id sp-local-allow-insert-pair-in-code))))
    (cond
     (local-allow-in-code (member mode local-allow-in-code))
     ((member id sp-global-ban-insert-pair-in-code) nil)
     (local-ban-in-code nil)
     (t t))))

(defun sp-insert-pair-p (id mode)
  "Return t if we can insert pair ID in MODE. See
`sp-insert-pair' for more info."
  (let ((local-ban (member mode (cdr (assoc id sp-local-ban-insert-pair))))
        (local-allow (cdr (assoc id sp-local-allow-insert-pair))))
    (cond
     ;; if locally allowed, allow it. If it's on local-allow list
     ;; automatically disable it in all non-specified modes
     (local-allow
      (if (member mode local-allow)
          (if (sp-point-in-string-or-comment)
              (sp-insert-pair-in-string-p id mode)
            (sp-insert-pair-in-code-p id mode))
        nil))
     ;; if globally disabled, disable
     ((member id sp-global-ban-insert-pair) nil)
     ;; if locally disabled, disable
     (local-ban nil)
     ;; test the "in string bans"
     ((sp-point-in-string-or-comment)
      (sp-insert-pair-in-string-p id mode))
     ;; if not in string, we must be in code
     (t (sp-insert-pair-in-code-p id mode))
      )))

(defun sp-post-command-hook-handler ()
  "Main handler of post-self-insert events."
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

    (unless (eq this-command 'self-insert-command)
      (setq sp-last-operation nil))))

(defadvice self-insert-command (before self-insert-command-pre-hook activate)
  (setq sp-point-inside-string (sp-point-in-string)))

(defmacro setaction (action &rest forms)
  `(if (not action)
       (setq action (progn ,@forms))
     (progn ,@forms)))

(defadvice self-insert-command (after self-insert-command-post-hook activate)
  (when smartparens-mode
    (let (op action)
      (if (= 1 (ad-get-arg 0))
          (progn
            (setq op sp-last-operation)
            (cond
             ((region-active-p)
              (sp-wrap-region-init))
             (sp-wrap-overlays
              (sp-wrap-region))
             (sp-wrap-tag-overlays t) ;; do nothing, the modification
                                      ;; hook will take care of that
             (t
              (setaction action (sp-insert-pair))
              (setaction action (sp-skip-closing-pair))
              ;; if nothing happened, we just inserted a character, so set
              ;; the apropriate operation
              (when (not action)
                (setq sp-last-operation 'sp-self-insert))
              ;; if it was a quote, escape it
              (when (and (eq sp-last-operation 'sp-self-insert)
                         sp-autoescape-string-quote
                         (eq (preceding-char) ?\"))
                (save-excursion
                  (backward-char 1)
                  (insert sp-escape-char)))
              ))

            )
        (setq sp-last-operation 'sp-self-insert)
        ))))

(defun sp-delete-selection-mode-handle ()
  "Call the original `delete-selection-pre-hook'."
  (when sp-delete-selection-mode
    (let ((delete-selection-mode t))
      (delete-selection-pre-hook))))

(defun sp-pre-command-hook-handler ()
  "Main handler of pre-command-hook. Handle the
delete-selection-mode stuff here."
  (when (and smartparens-mode
             (not (eq this-command 'self-insert-command)))
    ;; if not self-insert, just run the hook from
    ;; delete-selection-mode if enabled
    (sp-delete-selection-mode-handle)))

(defun sp-wrap-region-init ()
  "Initialize the region wrapping."
  ;; only do anything if the last command was self-insert-command
  (when sp-autowrap-region
    ;; if we can't possibly form a wrap, just insert the char and do
    ;; nothing. If delete-selection-mode is enabled, run
    ;; delete-selection-pre-hook
    (if (--none? (string-prefix-p (single-key-description last-command-event) (car it)) sp-pair-list)
        (let ((p (1- (point)))
              (m (mark)))
          ;; test if we can at least start a tag wrapping. If not,
          ;; delete the region if apropriate
          (unless (sp-wrap-tag-region-init)
            (sp-delete-selection-mode-handle)
            (when (< m p) (insert (single-key-description last-command-event)))))
      (let* ((p (1- (point))) ;; we want the point *before* the
                              ;; insertion of the character
             (m (mark))
             (ostart (if (> p m) m p))
             (oend (if (> p m) p m))
             (keys (mapcar 'single-key-description (recent-keys)))
             (last-keys (apply #'concat (-take 10 (reverse keys))))
             (active-pair (--first (string-prefix-p (reverse-string (car it)) last-keys) sp-pair-list))
             )

        (deactivate-mark)
        ;; if we can wrap right away, do it without creating overlays,
        ;; we can save ourselves a lot of needless trouble :)
        (if active-pair
            (unless (sp-wrap-tag-region-init)
              (let* ((oplen (length (car active-pair)))
                     (cplen (length (cdr active-pair)))
                     (len (+ oplen cplen)))
                (if (< p m)
                    (save-excursion
                      (goto-char m)
                      (insert (cdr active-pair)))
                  (delete-forward-char (- 1))
                  (insert (cdr active-pair))
                  (goto-char m)
                  (insert (car active-pair))
                  (goto-char (+ len p)))
                (setq sp-last-operation 'sp-wrap-region)
                (setq sp-last-wrapped-region
                      (if (< p m)
                          (list p (+ len m) oplen cplen)
                        (list m (+ len p) oplen cplen)))))

          ;; save the position and point so we can restore it on cancel.
          (setq sp-wrap-point p)
          (setq sp-wrap-mark m)

          ;; We need to remember what was removed in case wrap is
          ;; cancelled. Then these characters are re-inserted.
          (setq sp-last-inserted-character (single-key-description last-command-event))

          ;; if point > mark, we need to remove the character at the end and
          ;; insert it to the front.
          (when (> p m)
            (delete-forward-char (- 1))
            (goto-char ostart)
            (insert sp-last-inserted-character)
            (setq oend (1+ oend)))

          (let* ((oleft (make-overlay ostart (1+ ostart) nil nil t))
                 (oright (make-overlay oend oend nil nil t))
                 )

            ;; insert the possible pair into end overlay
            (let ((close-pair (cdr (--last (string-prefix-p
                                             sp-last-inserted-character
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
  ;; this method is only called if there's an active region. It should
  ;; never be called manually!
  (when sp-autowrap-region
    (let* ((keys (mapcar 'single-key-description (recent-keys)))
           (last-keys (apply #'concat (-take 10 (reverse keys))))
           (oleft (car sp-wrap-overlays))
           (oright (cdr sp-wrap-overlays))
           )
      (setq sp-last-inserted-character
            (concat sp-last-inserted-character
                    (single-key-description last-command-event)))
      (let* ((active-pair (--last (string-prefix-p
                                   sp-last-inserted-character
                                   (car it))
                                  sp-pair-list))
            (open-pair (car active-pair))
            (close-pair (cdr active-pair)))

        ;; TODO: call sp-wrap-tag-region-init here. See if we can
        ;; extend the current wrap-beginning into a tag

        ;; update the close pair
        (if close-pair
            (save-excursion
              (delete-region (overlay-start oright) (overlay-end oright))
              (goto-char (overlay-start oright))
              (insert close-pair))
          ;; if we don't have any, it means there is no way to
          ;; complete the pair... abort
          (sp-wrap-cancel t))

        ;; we've completed a pairing!
        (when (equal sp-last-inserted-character open-pair)
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
                (list s e oplen cplen))
          ))))))

(defun sp-wrap-tag-region-init ()
  "Init a region wrapping with a tag pair. This is called from
`sp-wrap-region-init' or `sp-wrap-region' (usually on failure) to
see if the currently entered \"wrap\" can be extended as a
tag. The tag always gets priority from the regular wrap."
  (when sp-autowrap-region
    ;; we can either enter tagwrapping from already present wrap or
    ;; from nothing (if the wrap-init failed to find any usable wrap)
    ;; or at failure (the entered wrap doesn't match any pair)
    (if sp-wrap-overlays ;; called from within the wrap-mode
        (let* ((oleft (car sp-wrap-overlays))
               (oright (cdr sp-wrap-overlays))
               (open (buffer-substring (overlay-start oleft) (overlay-end oleft)))
               (active-tag (assoc open sp-tag-pair-list))
               )
          (when active-tag

            )
          )
      ;; here we need to look at the last inserted character
      (let* ((p (1- (point)))
             (m (mark))
             (ostart (if (> p m) m p))
             (oend (if (> p m) p m))
             (possible-tag (--first (string-prefix-p
                                     (single-key-description last-command-event)
                                     (car it))
                                    sp-tag-pair-list))
             (active-tag (cdr (--first (member major-mode (car it)) (cdr possible-tag)))))
        (when active-tag
          (if (= 1 (length (car possible-tag)))
              ;; the tag is only 1 character long, we can enter
              ;; insertion mode right away
              (progn
                (when (> p m)
                  (delete-forward-char (- 1))
                  (goto-char ostart)
                  (insert (single-key-description last-command-event))
                  (setq oend (1+ oend))
                  )
                (sp-wrap-tag-create-overlays possible-tag active-tag ostart oend)
                )
            ())
          )
        )
      )
    ))

(defun sp-wrap-tag-create-overlays (possible-tag active-tag ostart oend)
  "Create the wrap tag overlays.

OSTART is the start of the modified area, including the pair trigger string.

OEND is the end of the modified area, that is the end of the
wrapped region, exluding any existing possible wrap."
  (let* ((tag-open (split-string (nth 0 active-tag) "_"))
         (tag-close (split-string (nth 1 active-tag) "_"))
         (oleft (progn
                  (goto-char ostart)
                  (delete-char (length (car possible-tag)))
                  (insert (apply #'concat tag-open))
                  (backward-char (length (cadr tag-open)))
                  (make-overlay
                   (+ ostart (length (car tag-open)))
                   (+ ostart (length (car tag-open)))
                   nil nil t)))
         (oright (let ((o (1- (length (car active-tag)))))
                   (save-excursion
                     (goto-char (+ oend o))
                     (insert (apply #'concat tag-close)))
                   (make-overlay
                    (+ oend o (- 1) (length (car tag-close)))
                    (+ oend o (- 1) (length (car tag-close)))
                    nil nil t))))
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
    ))


(defun sp-wrap-tag-update (overlay after? beg end &optional length)
  (let* ((oleft (car sp-wrap-tag-overlays))
         (oright (cdr sp-wrap-tag-overlays))
         (active-tag (overlay-get oleft 'active-tag))
         (transform (nth 2 active-tag))
         (open (buffer-substring (overlay-start oleft) (overlay-end oleft)))
         )
    (save-excursion
      (delete-region (overlay-start oright) (overlay-end oright))
      (goto-char (overlay-start oright))
      (insert (funcall transform open)))
    )
  )

(defun sp-match-sgml-tags (tag)
  (let* ((split (split-string tag " "))
         (close (car split)))
    close))

(defun sp-wrap-tag-region ()
  "Wrap region with tag."

  )

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
    )
  )

(defun sp-insert-pair ()
  "Automatically insert the closing pair if it is allowed in current
context. It is determined in this order:

1. Global allow - all pairs are allowed by default in every mode.

2. Local ban - you can ban specific pair in specific modes. See
`sp-add-local-ban-insert-pair'.

3. Global ban - you can globally ban specific pair.

4. Local allow - you can allow specific pair in specific
modes. It is disabled in all other modes (as if you globally
banned it first). See `sp-add-local-allow-insert-pair'.

Additionally, there is a possibility to disable insertion of
pairs inside strings, docstrings and comments or inside code. The
order in which these are determined is following:

1. Is the pair allowed in this mode? (by the above mechanism)

2. Local ban inside strings - `sp-add-local-ban-insert-pair-in-string'.

3. Global ban - `sp-add-ban-insert-pair-in-string'.

4. Local allow - `sp-add-local-allow-insert-pair-in-string'.

The semantics are the same as above. First, point position is
tested to determine if it is inside string. If yes, the rules for
string restrictions are evaluated. If not, restrictions for code
are evaluated. The code versions end with \"in-code\" instead of
\"in-string\".

You can disable this feature completely for all modes and all pairs by
setting `sp-autoinsert-pair' to nil.

You can globally disable insertion of closing pair if point is
followed by the matching opening pair. It is disabled by
default. See `sp-autoinsert-if-followed-by-same' for more info.

You can globally disable insertion of closing pair if point is
followed by word. It is disabled by default. See
`sp-autoinsert-if-followed-by-word' for more info."
  (when sp-autoinsert-pair
    (let* ((keys (mapcar 'single-key-description (recent-keys)))
           (last-keys (apply #'concat (-take 10 (reverse keys))))
           ;; we go through all the opening pairs and compare them to
           ;; last-keys. If the opair is a prefix of last-keys, insert
           ;; the closing pair
           (active-pair (--first (string-prefix-p (reverse-string (car it)) last-keys) sp-pair-list))
           (open-pair (car active-pair))
           (close-pair (cdr active-pair))
           )
      (when (and active-pair
                 (not (eq sp-last-operation 'sp-skip-closing-pair))
                 (sp-insert-pair-p open-pair major-mode)
                 (if sp-autoinsert-if-followed-by-word t
                     (not (and (eq (char-syntax (following-char)) ?w)
                               (not (eq (following-char) ?\')))))
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
                              (looking-back (regexp-quote open-pair) (- (point) 10)))
                            )))
                  ))
        (insert close-pair)
        (backward-char (length close-pair))
        (sp-pair-overlay-create (- (point) (length open-pair))
                                (+ (point) (length close-pair))
                                open-pair)

        ;; we only autoescape if the pair is a single character string
        ;; delimiter. More elaborate pairs are probably already
        ;; escaped. We leave the responsibility to the user, since
        ;; it's not that common and the usecases might vary -> there's
        ;; no good "default" case.
        (when (and sp-autoescape-string-quote
                   sp-point-inside-string
                   (eq (string-to-char open-pair) ?\")
                   (eq (string-to-char close-pair) ?\")
                   (= 1 (length open-pair))
                   (= 1 (length close-pair)))
          (save-excursion
            (backward-char 1)
            (insert sp-escape-char)
            (forward-char 1)
            (insert sp-escape-char))
          (overlay-put (sp-get-active-overlay) 'pair-id "\\\""))

        (setq sp-last-operation 'sp-insert-pair)
        ))))

(defun sp-skip-closing-pair ()
  "If point is inside an inserted pair, and the user only moved forward
with point (that is, only inserted text), if the closing pair is
typed, we shouldn't insert it again but skip forward.

For example, pressing ( is followed by inserting the pair (|). If
we then type 'word' and follow by ), the result should be (word)|
instead of (word)|).

If the user moved backwards or outside the
pair, this behaviour is cancelled. This behaviour can be globally
disabled by setting `sp-cancel-autoskip-on-backward-movement' to
nil.

This behaviour can be globally disabled by setting
`sp-autoskip-closing-pair' to nil."
  (when (and sp-autoskip-closing-pair
             sp-pair-overlay-list
             (sp-get-active-overlay))
    (let* ((overlay (sp-get-active-overlay))
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
            (if (equal (single-key-description last) (substring close-pair-rest 0 1))
                (progn
                  (forward-char 1)
                  (delete-forward-char (- 1))
                  (setq sp-last-operation 'sp-skip-closing-pair))
              ;; Charactar that is not part of the closing pair was
              ;; typed. Only remove overlays if we're inside the
              ;; closing pair. If we are at the beginning, we are
              ;; allowed to type other characters
              (when (> already-skipped 0)
                (dolist (o sp-pair-overlay-list) (sp-remove-overlay o)))
              )))))))

(defun sp-delete-pair (&optional arg)
  "Automatically delete opening or closing pair, or both, depending on
position of point.

If the point is inside an empty pair, automatically delete both. That
is, [(|) turns to [|, [\{|\} turns to [|. Can be disabled by setting
`sp-autodelete-pair' to nil.

If the point is behind a closing pair or behind an opening pair delete
it as a whole. That is, \{\}| turns to \{|, \{| turns to |. Can be
disabled by setting `sp-autodelete-closing-pair' and
`sp-autodelete-opening-pair' to nil."
  ;; NOTE: Only use delete-forward-char inside this function, so we
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
              (o (nth 2 sp-last-wrapped-region))
              (c (nth 3 sp-last-wrapped-region)))
          ;; if the last operation was `sp-wrap-region', and we are at
          ;; the position of either opening or closing pair, delete the
          ;; just-inserted pair
          (cond
           ((= p (+ s o))
            (save-excursion
              (delete-forward-char (- (1- o)))
              (goto-char (- e o))
              (delete-forward-char (- c)))
            (setq sp-last-operation 'sp-delete-pair-wrap))
           ((= p e)
            (save-excursion
              (delete-forward-char (- (1- c)))
              (goto-char s)
              (delete-forward-char o))
            (setq sp-last-operation 'sp-delete-pair-wrap))))
      (let* ((p (point))
             (inside-pair (--first (and (looking-back (regexp-quote (car it)) (- p 10))
                                        (looking-at (regexp-quote (cdr it))))
                                   sp-pair-list))
             (behind-pair (--first (looking-back (regexp-quote (cdr it)) (- p 10)) sp-pair-list))
             (opening-pair (--first (looking-back (regexp-quote (car it)) (- p 10)) sp-pair-list)))
        (cond
         ;; we're just before the closing quote of a string. If there
         ;; is an opening or closing pair behind the point, remove
         ;; it. This is only really relevant if the pair ends in the
         ;; same character as string quote. We almost never want to
         ;; delete it as an autopair (it would "open up the string").
         ;; So, word\"|" and <backspace> should produce word\|" or
         ;; word|" (if \" is autopair) instead of word\|.
         ((and font-lock-mode ;; who doesn't use this? but... to be
                              ;; sure, since this is not a
                              ;; customizable option
               (sp-point-in-string)
               (not (sp-point-in-string (1+ p)))
               (sp-point-in-string (- p 2))) ;; the string isn't empty
          (cond ;; oh, you ugly duplication :/
           ((and behind-pair sp-autodelete-closing-pair)
            (delete-forward-char (- (1- (length (car behind-pair)))))
            (setq sp-last-operation 'sp-delete-pair-closing))
           ((and opening-pair sp-autodelete-opening-pair)
            (delete-forward-char (- (1- (length (car opening-pair)))))
            (setq sp-last-operation 'sp-delete-pair-opening))))
         ;; we're inside a pair
         ((and inside-pair sp-autodelete-pair)
          (delete-forward-char (length (cdr inside-pair)))
          (delete-forward-char (- (1- (length (car inside-pair)))))
          (setq sp-last-operation 'sp-delete-pair))
         ;; we're behind a closing pair
         ((and behind-pair sp-autodelete-closing-pair)
          (delete-forward-char (- (1- (length (cdr behind-pair)))))
          (setq sp-last-operation 'sp-delete-pair-closing))
         ;; we're behind an opening pair and there's no closing pair
         ((and opening-pair sp-autodelete-opening-pair)
          (delete-forward-char (- (1- (length (car opening-pair)))))
          (setq sp-last-operation 'sp-delete-pair-opening))
         )))))

;; global initialization
(sp-update-pair-triggers)
(defadvice delete-backward-char (before sp-delete-pair-advice activate)
  (sp-delete-pair (ad-get-arg 0)))
(add-hook 'post-command-hook 'sp-post-command-hook-handler)
(add-hook 'pre-command-hook 'sp-pre-command-hook-handler)

(provide 'smartparens)

;;; smartparens.el ends here
