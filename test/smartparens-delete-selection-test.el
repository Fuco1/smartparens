;; Tests for delete-selection-mode and strict mode integration

(defmacro sp-test-delsel (initial &rest forms)
  (declare (indent 1))
  `(sp-test-with-delete-selection-mode
     (sp-test-with-temp-elisp-buffer ,initial
       (smartparens-strict-mode 1)
       ,@forms)))

;; Overwriting with letters
(ert-deftest sp-test-delete-selection-mode-self-insert-can-not-kill-invalid-region ()
  (sp-test-delsel "(fo|o) bMar"
    (execute-kbd-macro "x")
    (sp-buffer-equals "(fo|o) bMar")))

(ert-deftest sp-test-delete-selection-mode-self-insert-can-kill-valid-region ()
  (sp-test-delsel "(fo|o bMar)"
    (execute-kbd-macro "x")
    (sp-buffer-equals "(fox|ar)")))

(ert-deftest sp-test-delete-selection-mode-nonstrict-self-insert-can-kill-invalid-region ()
  (sp-test-delsel "(fo|o) bMar"
    (smartparens-strict-mode -1)
    (execute-kbd-macro "x")
    (sp-buffer-equals "(fox|ar")))

(ert-deftest sp-test-delete-selection-mode-nonstrict-self-insert-can-kill-valid-region ()
  (sp-test-delsel "(fo|o bMar)"
    (smartparens-strict-mode -1)
    (execute-kbd-macro "x")
    (sp-buffer-equals "(fox|ar)")))

;; Calling sp-delete-char
(ert-deftest sp-test-delete-selection-mode-delete-char-can-not-kill-invalid-region ()
  (sp-test-delsel "(fo|o) bMar"
    (sp-test-with-temp-binding ("d" 'sp-backward-delete-char)
      (execute-kbd-macro "d"))
    (sp-buffer-equals "(fo|o) bMar")))

(ert-deftest sp-test-delete-selection-mode-delete-char-can-kill-valid-region ()
  (sp-test-delsel "(fo|o bMar)"
    (sp-test-with-temp-binding ("d" 'sp-delete-char)
      (execute-kbd-macro "d"))
    (sp-buffer-equals "(fo|ar)")))

(ert-deftest sp-test-delete-selection-mode-nonstrict-delete-char-can-kill-invalid-region ()
  (sp-test-delsel "(fo|o) bMar"
    (smartparens-strict-mode -1)
    (sp-test-with-temp-binding ("d" 'sp-backward-delete-char)
      (execute-kbd-macro "d"))
    (sp-buffer-equals "(fo|ar")))

(ert-deftest sp-test-delete-selection-mode-nonstrict-delete-char-can-kill-valid-region ()
  (sp-test-delsel "(fo|o bMar)"
    (smartparens-strict-mode -1)
    (sp-test-with-temp-binding ("d" 'sp-delete-char)
      (execute-kbd-macro "d"))
    (sp-buffer-equals "(fo|ar)")))

;; Calling sp-backward-delete-char
(ert-deftest sp-test-delete-selection-mode-backward-delete-char-can-not-kill-invalid-region ()
  (sp-test-delsel "(fo|o) bMar"
    (sp-test-with-temp-binding ("d" 'sp-backward-delete-char)
      (execute-kbd-macro "d"))
    (sp-buffer-equals "(fo|o) bMar")))

(ert-deftest sp-test-delete-selection-mode-backward-delete-char-can-kill-valid-region ()
  (sp-test-delsel "(fo|o bMar)"
    (sp-test-with-temp-binding ("d" 'sp-backward-delete-char)
      (execute-kbd-macro "d"))
    (sp-buffer-equals "(fo|ar)")))

(ert-deftest sp-test-delete-selection-mode-nonstrict-backward-delete-char-can-kill-invalid-region ()
  (sp-test-delsel "(fo|o) bMar"
    (smartparens-strict-mode -1)
    (sp-test-with-temp-binding ("d" 'sp-backward-delete-char)
      (execute-kbd-macro "d"))
    (sp-buffer-equals "(fo|ar")))

(ert-deftest sp-test-delete-selection-mode-nonstrict-backward-delete-char-can-kill-valid-region ()
  (sp-test-delsel "(fo|o bMar)"
    (smartparens-strict-mode -1)
    (sp-test-with-temp-binding ("d" 'sp-backward-delete-char)
      (execute-kbd-macro "d"))
    (sp-buffer-equals "(fo|ar)")))
