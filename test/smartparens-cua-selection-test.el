;; Tests for cua-selection-mode and smartparens integration

(defmacro sp-test-cuasel (initial &rest forms)
  (declare (indent 1))
  `(unwind-protect
       (progn
         (cua-selection-mode 1)
         (sp-test-with-temp-elisp-buffer ,initial
           (smartparens-strict-mode 1)
           ,@forms))
     (cua-selection-mode -1)))

(ert-deftest sp-test-cua-selection-mode-delete-region-strict-valid ()
  (sp-test-cuasel "(fo|o bMar)"
    (execute-kbd-macro "x")
    (sp-buffer-equals "(fox|ar)")))

(ert-deftest sp-test-cua-selection-mode-delete-region-strict-invalid ()
  (sp-test-cuasel "(fo|o) bMar"
    (execute-kbd-macro "x")
    (sp-buffer-equals "(fo|o) bMar")))

(ert-deftest sp-test-cua-selection-mode-delete-region-nonstrict-valid ()
  (sp-test-cuasel "(fo|o bMar)"
    (smartparens-strict-mode -1)
    (execute-kbd-macro "x")
    (sp-buffer-equals "(fox|ar)")))

(ert-deftest sp-test-cua-selection-mode-delete-region-nonstrict-invalid ()
  (sp-test-cuasel "(fo|o) bMar"
    (smartparens-strict-mode -1)
    (execute-kbd-macro "x")
    (sp-buffer-equals "(fox|ar")))

;; Calling sp-delete-char
(ert-deftest sp-test-cua-selection-mode-delete-char-can-not-kill-invalid-region ()
  (sp-test-cuasel "(fo|o) bMar"
    (sp-test-with-temp-binding ("d" 'sp-backward-delete-char)
      (execute-kbd-macro "d"))
    (sp-buffer-equals "(fo|o) bMar")))

(ert-deftest sp-test-cua-selection-mode-delete-char-can-kill-valid-region ()
  (sp-test-cuasel "(fo|o bMar)"
    (sp-test-with-temp-binding ("d" 'sp-delete-char)
      (execute-kbd-macro "d"))
    (sp-buffer-equals "(fo|ar)")))

(ert-deftest sp-test-cua-selection-mode-nonstrict-delete-char-can-kill-invalid-region ()
  (sp-test-cuasel "(fo|o) bMar"
    (smartparens-strict-mode -1)
    (sp-test-with-temp-binding ("d" 'sp-backward-delete-char)
      (execute-kbd-macro "d"))
    (sp-buffer-equals "(fo|ar")))

(ert-deftest sp-test-cua-selection-mode-nonstrict-delete-char-can-kill-valid-region ()
  (sp-test-cuasel "(fo|o bMar)"
    (smartparens-strict-mode -1)
    (sp-test-with-temp-binding ("d" 'sp-delete-char)
      (execute-kbd-macro "d"))
    (sp-buffer-equals "(fo|ar)")))

;; Calling sp-backward-delete-char
(ert-deftest sp-test-cua-selection-mode-backward-delete-char-can-not-kill-invalid-region ()
  (sp-test-cuasel "(fo|o) bMar"
    (sp-test-with-temp-binding ("d" 'sp-backward-delete-char)
      (execute-kbd-macro "d"))
    (sp-buffer-equals "(fo|o) bMar")))

(ert-deftest sp-test-cua-selection-mode-backward-delete-char-can-kill-valid-region ()
  (sp-test-cuasel "(fo|o bMar)"
    (sp-test-with-temp-binding ("d" 'sp-backward-delete-char)
      (execute-kbd-macro "d"))
    (sp-buffer-equals "(fo|ar)")))

(ert-deftest sp-test-cua-selection-mode-nonstrict-backward-delete-char-can-kill-invalid-region ()
  (sp-test-cuasel "(fo|o) bMar"
    (smartparens-strict-mode -1)
    (sp-test-with-temp-binding ("d" 'sp-backward-delete-char)
      (execute-kbd-macro "d"))
    (sp-buffer-equals "(fo|ar")))

(ert-deftest sp-test-cua-selection-mode-nonstrict-backward-delete-char-can-kill-valid-region ()
  (sp-test-cuasel "(fo|o bMar)"
    (smartparens-strict-mode -1)
    (sp-test-with-temp-binding ("d" 'sp-backward-delete-char)
      (execute-kbd-macro "d"))
    (sp-buffer-equals "(fo|ar)")))
