;;; smartparens.el --- Autoinsert pairs of defined brackets and wrap regions

;; Copyright (C) 2012 Matus Goljer

;; Author: Matus Goljer <matus.goljer@gmail.com>
;; Maintainer: Matus Goljer <matus.goljer@gmail.com>
;; Created: 17 Nov 2012
;; Version: 0.1
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

;;;###autoload
(define-minor-mode smartparens-mode
  "Toggle smartparens mode"
  :init-value nil
  :lighter " SP"
  :group 'smartparens
  :keymap sp-keymap
  (if smartparens-mode
      (progn
        (sp-update-pair-triggers)
        (defadvice delete-backward-char (before sp-delete-pair-advice activate)
          (sp-delete-pair (ad-get-arg 0)))
        (add-hook 'post-command-hook 'sp-post-command-hook-handler nil t)
        (add-hook 'pre-command-hook 'sp-pre-command-hook-handler nil t)
        (run-hooks 'smartparens-enabled-hook))
    (remove-hook 'post-command-hook 'sp-post-command-hook-handler t)
    (remove-hook 'pre-command-hook 'sp-pre-command-hook-handler t)
    (run-hooks 'smartparens-disabled-hook)
    ))

(defvar sp-keymap (make-sparse-keymap)
  "Keymap used for smartparens-mode. Remaps all the trigger keys
to `self-insert-command'. This means we lose some functionality
in some modes (like c-electric keys).")

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
    (--each triggers (define-key sp-keymap it 'self-insert-command))
    ))

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
  (unless (member major-mode sp-ignore-modes-list)
    (smartparens-mode t)))

;;;###autoload
(defun turn-off-smartparens-mode ()
  "Turn off `smartparens-mode'."
  (interactive)
  (smartparens-mode -1))

;; global custom
(defcustom sp-ignore-modes-list '(calc-mode dired-mode minibuffer-inactive-mode)
  "Modes where smartparens mode is inactive if allowed globally."
  :type '(repeat symbol)
  :group 'smartparens)

;; function custom
(defcustom sp-autoinsert-pair t
  "If non-nil, auto insert pairs.  See `sp-insert-pair'."
  :type 'boolean
  :group 'smartparens)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables

(defvar sp-local-ban-insert-pair '(("'" . (emacs-lisp-mode)))
  "Pairs on this list are locally disabled in specified modes.

List of elements of type (command . '(list of modes)).")

(defvar sp-global-ban-insert-pair '()
  "Pairs on this list are disabled globally.

List of pair IDs.")

(defvar sp-local-allow-insert-pair '()
  "Pairs on this list are locally enabled in specified modes. They are
disabled in other modes automatically.

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
  "List of pairs for auto-insertion or wrapping. Maximum length
of opening or closing pair is 10 characters.")

(defvar sp-last-operation nil
  "Symbol holding the last successful operation.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc functions

(defmacro !delete (elm list)
  "Destructive: Sets LIST to (delete ELM LIST)."
  `(setq ,list (delete ,elm ,list)))


(defun reverse-string (str)
  "Reverse the string STR."
  (concat (reverse (append str nil))))

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
  "Adds a pair formed by OPEN and CLOSE to the pair list. See
variable `sp-pair-list' for current list.

Additional arguments are interpreted as modes where this pair
should be banned by default. BANNED-MODES can also be a list."
  (unless (--any? (equal open (car it)) sp-pair-list)
    (setq sp-pair-list
          (sp-add-to-ordered-list (cons open close) sp-pair-list #'sp-order-pairs))
    (sp-add-local-ban-insert-pair open banned-modes)
    (sp-update-pair-triggers)
  ))

(defun sp-remove-pair (open)
  "Remove a pair from the pair list. See variable `sp-pair-list'
for current list."
  (setq sp-pair-list
        (--remove (equal open (car it)) sp-pair-list))
  (sp-remove-local-ban-insert-pair open)
  (sp-remove-local-allow-insert-pair open)
  (sp-update-pair-triggers))

;; sp-global-ban-insert-pair

(defun -union (list1 list2)
  "Return a new list containing the elements of LIST1 and
elements of LIST2 that were not present in LIST1. The test for
equality is done with `equal', or with `-compare-fn' if that's
non-nil."
  (let ((result (nreverse list1)))
    (--each list2 (when (not (-contains? result it)) (!cons it result)))
    (nreverse result)))

(defmacro sp-add-pair-to-permission-list (open list &rest modes)
  "Add MODES to the pair with id OPEN in the LIST. See
permissions system for more details."
  (let ((m (make-symbol "new-modes")))
    `(let ((,m (-flatten modes)))
       (when ,m
         (let ((current (--first (equal ,open (car it)) ,list)))
           (if current
               (setcdr current (-union (cdr current) ,m))
             (!cons (cons ,open ,m) ,list)))))))

(defun sp-add-local-ban-insert-pair (open &rest modes)
  "Ban autoinsertion of pair with id OPEN in modes MODES. See
`sp-insert-pair'."
  (sp-add-pair-to-permission-list open sp-local-ban-insert-pair modes))

(defun sp-add-local-allow-insert-pair (open &rest modes)
  "Allow autoinsertion of pair with id OPEN in modes MODES. See
`sp-insert-pair'."
  (sp-add-pair-to-permission-list open sp-local-allow-insert-pair modes))

(defmacro sp-remove-pair-from-permission-list (open list &rest modes)
  "Removes MODES from the pair with id OPEN in the LIST. See
permissions system for more details. If modes is nil, remove the
pair entirely."
  (let ((m (make-symbol "new-modes")))
    `(let ((,m (-flatten modes)))
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
  (sp-remove-pair-from-permission-list open sp-local-ban-insert-pair modes))

(defun sp-remove-local-allow-insert-pair (open &rest modes)
  "Remove previously set restriction on pair with id OPEN in
modes MODES. If MODES is nil, remove all the modes"
  (sp-remove-pair-from-permission-list open sp-local-allow-insert-pair modes))

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

(defvar sp-pair-overlay-keymap (make-sparse-keymap)
  "Keymap for the pair overlays.")
(define-key sp-pair-overlay-keymap (kbd "C-g") 'sp-remove-active-overlay)
(defvar sp-wrap-overlay-keymap (make-sparse-keymap)
  "Keymap for the wrap overlays.")
(define-key sp-wrap-overlay-keymap (kbd "C-g") 'sp-wrap-cancel)

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

(defun sp-wrap-overlay-post-command-handler ()
  "Cancel the wrap editing if the point moved outside the left
overlay or if point moved backwards."
  ;; sanity check
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
    (setq sp-previous-point (point))))

(defun sp-wrap-cancel ()
  "Cancel the active wrapping."
  (interactive)
  (remove-hook 'post-command-hook 'sp-wrap-overlay-post-command-handler t)
  (let ((oleft (car sp-wrap-overlays))
        (oright (cdr sp-wrap-overlays)))
    (delete-region (overlay-start oleft) (overlay-end oleft))
    (delete-region (overlay-start oright) (overlay-end oright))
    (delete-overlay oleft)
    (delete-overlay oright)
    (setq sp-wrap-overlays nil)
    (setq sp-previous-point -1)
    ;;(message "Removing wrap-overlays")
    ;; restore the original state
    (goto-char sp-wrap-point)
    (set-mark sp-wrap-mark)
    (activate-mark)

    ;;(set-mark-command 1)
    ;;(pop-to-mark-command)
    ;;
    ;;(setq sp-wrap-point nil)
    ;;(setq sp-wrap-mark nil)
  ))

(defun sp-pair-overlay-fix-highlight ()
  "Fix highlighting of the pair overlays. Only the active overlay
should be highlighted."
  (when sp-highlight-pair-overlay
    (--each (overlays-at (point)) (overlay-put it 'face nil))
    (when (sp-get-active-overlay)
      (overlay-put (sp-get-active-overlay) 'face 'sp-pair-overlay-face))))

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

(defun sp-insert-pair-p (id mode)
  "Return t if we can insert pair ID in MODE. See
`sp-insert-pair' for more info."
  (let ((local-ban (member mode (cdr (assoc id sp-local-ban-insert-pair))))
        (local-allow (cdr (assoc id sp-local-allow-insert-pair))))
    (cond
     ;; if locally allowed, allow it. If it's on local-allow list
     ;; automatically disable it in all non-specified modes
     (local-allow (member mode local-allow))
     ;; if globally disabled, disable
     ((member id sp-global-ban-insert-pair) nil)
     ;; if locally disabled, disable
     (local-ban nil)
     ;; otherwise allow
     (t t))))

(defun sp-post-command-hook-handler ()
  "Main handler of post-self-insert events."
  (when smartparens-mode
    (if (eq this-command 'self-insert-command)
        (cond
         ((region-active-p)
          (sp-wrap-region-init))
         (sp-wrap-overlays
          (sp-wrap-region))
         (t
          (sp-insert-pair)
          (sp-skip-closing-pair)))
      (setq sp-last-operation nil)
      )))

(defun sp-pre-command-hook-handler ()
  "Main handler of pre-command-hook. Handle the
delete-selection-mode stuff here."
  (if (not (eq this-command 'self-insert-command))
      ;; if not self-insert, just run the hook from
      ;; delete-selection-mode if enabled
      (when sp-delete-selection-mode
        (setq delete-selection-mode t)
        (delete-selection-pre-hook)
        (setq delete-selection-mode nil)
        (setq sp-attempt-wrap nil))
    ))

(defvar sp-last-inserted-character ""
  "If wrapping is cancelled, these character are re-inserted to
the location of point before the wrapping.")
(make-variable-buffer-local 'sp-last-inserted-character)

(defun sp-wrap-region-init ()
  "Initialize the region wrapping."
  ;; only do anything if the last command was self-insert-command
  (when (and sp-autowrap-region
             (eq this-command 'self-insert-command))
    ;; if we can't possibly form a wrap, just insert the char and do
    ;; nothing. If delete-selection-mode is enabled, run
    ;; delete-selection-pre-hook
    (when t
      (let* ((p (1- (point))) ;; we want the point *before* the
                              ;; insertion of the character
             (m (mark))
             (ostart (if (> p m) m p))
             (oend (if (> p m)  p m))
             (keys (mapcar 'single-key-description (recent-keys)))
             (last-keys (apply #'concat (-take 10 (reverse keys))))
             (active-pair (--first (string-prefix-p (reverse-string (car it)) last-keys) sp-pair-list))
             )

        (deactivate-mark)
        ;; if we can wrap right away, do it without creating overlays,
        ;; we can save ourselves a lot of needless trouble :)
        (if active-pair
            (let* ((oplen (length (car active-pair)))
                   (cplen (length (cdr active-pair)))
                   (len (+ oplen cplen)))
              (if (< p m)
                  (save-excursion
                    (goto-char m)
                    (insert (cdr active-pair)))
                (delete-backward-char 1)
                (insert (cdr active-pair))
                (goto-char m)
                (insert (car active-pair))
                (goto-char (+ len p)))
              (setq sp-last-operation 'sp-wrap-region)
              (setq sp-last-wrapped-region
                    (if (< p m)
                        (list p (+ len m) oplen cplen)
                      (list m (+ len p) oplen cplen))))

          ;; save the position and point so we can restore it on cancel.
          (setq sp-wrap-point p)
          (setq sp-wrap-mark m)

          ;; We need to remember what was removed in case wrap is
          ;; cancelled. Then these characters are re-inserted.
          (setq sp-last-inserted-character (single-key-description last-command-event))

          ;; if point > mark, we need to remove the character at the end and
          ;; insert it to the front.
          (when (> p m)
            (delete-backward-char 1)
            (goto-char ostart)
            (insert sp-last-inserted-character))

          (let* ((oleft (make-overlay ostart (1+ ostart) nil nil t))
                 (oright (make-overlay oend oend nil nil t))
                 )
            ;; insert the possible pair into end overlay
            ;; ()

            (setq sp-wrap-overlays (cons oleft oright))
            (when sp-highlight-wrap-overlay
              (overlay-put oleft 'face 'sp-wrap-overlay-face)
              (overlay-put oright 'face 'sp-wrap-overlay-face))
            (overlay-put oleft 'priority 100)
            (overlay-put oright 'priority 100)
            (overlay-put oleft 'keymap sp-wrap-overlay-keymap)
            (overlay-put oleft 'type 'wrap)

            (goto-char (1+ ostart))
            (add-hook 'post-command-hook 'sp-wrap-overlay-post-command-handler nil t)
            ))))))

(defun sp-wrap-region ()
  "Wrap region."
  ;; this method is only called if there's an active region. It should
  ;; never be called manually!
  (when sp-autowrap-region
    (let* ((keys (mapcar 'single-key-description (recent-keys)))
           (last-keys (apply #'concat (-take 10 (reverse keys))))

           )
      )
    ))

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
                 (sp-insert-pair-p open-pair major-mode)
                 (if sp-autoinsert-if-followed-by-word t
                     (not (eq (char-syntax (following-char)) ?w)))
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

(provide 'smartparens)

;;; smartparens.el ends here
