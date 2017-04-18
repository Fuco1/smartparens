(require 'evil)

(ert-deftest sp-test-evil-enabled-copy-sexp ()
  "When `evil-mode' is enabled, copying sexp copies to 0 register."
  (sp-test-with-temp-elisp-buffer "|(a b c)"
    (evil-set-register ?0 nil)
    (evil-mode)
    (sp-kill-sexp 1 t)
    (evil-mode -1)
    (should (equal (buffer-string) (evil-get-register ?0)))))

(ert-deftest sp-test-evil-disabled-copy-sexp ()
  "When `evil-mode' is disabled, copying sexp doesn't modify 0 register."
  (sp-test-with-temp-elisp-buffer "|(a b c)"
    (evil-set-register ?0 nil)
    (evil-mode -1)
    (sp-kill-sexp 1 t)
    (should (not (equal (buffer-string) (evil-get-register ?0))))))

(ert-deftest sp-test-evil-enabled-copy-sexp-with-register ()
  "When `evil-mode' is enabled, copying a sexp with register set will
copy the sexp into that register."
  (sp-test-with-temp-elisp-buffer "|(a b c)"
    (evil-mode)
    (setq evil-this-register ?a)
    (sp-kill-sexp 1 t)
    (evil-mode -1)
    (should (equal (buffer-string) (evil-get-register ?a)))))

(ert-deftest sp-test-evil-disabled-copy-sexp-with-register ()
  "When `evil-mode' is disabled, copying a sexp with register set will not
copy the sexp into that register."
  (sp-test-with-temp-elisp-buffer "|(a b c)"
    (evil-mode -1)
    (setq evil-this-register ?c)
    (sp-kill-sexp 1 t)
    (should (not (equal (buffer-string) (evil-get-register ?c))))))

(ert-deftest sp-test-evil-enabled-kill-sexp-with-register ()
  "When `evil-mode' is enabled, killing a sexp with register set will
copy the sexp into that register."
  (sp-test-with-temp-elisp-buffer "|(a b c)"
    (evil-mode)
    (setq evil-this-register ?b)
    (sp-kill-sexp 1 nil)
    (evil-mode -1)
    (should (equal "(a b c)" (evil-get-register ?b)))))

(ert-deftest sp-test-evil-disabled-kill-sexp-with-register ()
  "When `evil-mode' is disabled, killing a sexp with register set will not
copy the sexp into that register."
  (sp-test-with-temp-elisp-buffer "|(a b c)"
    (evil-mode -1)
    (setq evil-this-register ?d)
    (sp-kill-sexp 1 nil)
    (should (not (equal "(a b c)" (evil-get-register ?d))))))
