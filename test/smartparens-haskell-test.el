(require 'haskell-mode)
(require 'smartparens-haskell)

;; #710
(ert-deftest sp-test-haskell-allow-deleting-quote-at-end-of-symbol ()
  (sp-test-with-temp-buffer "myFunction'|"
      (haskell-mode)
    (smartparens-strict-mode 1)
    (sp-backward-delete-char)
    (sp-buffer-equals "myFunction|")))

;; #710
(ert-deftest sp-test-haskell-ignore-apostrophe-when-looking-for-sexp-when-it-is-used-as-suffix ()
  (sp-test-with-temp-buffer "test c d = myFunction' c + (myFunct|ion' d)"
      (haskell-mode)
    (sp-get (sp-get-sexp)
      (should (equal :op "(")))
    (sp-get (sp-get-enclosing-sexp)
      (should (equal :op "(")))))

;; #710
(ert-deftest sp-test-haskell-splice-sexp-when-there-is-trailing-quote ()
  (sp-test-with-temp-buffer "test c d = myFunction' c + (myFunct|ion' d)"
      (haskell-mode)
    (sp-splice-sexp)
    (sp-buffer-equals "test c d = myFunction' c + myFunct|ion' d")))

;; #710
(ert-deftest sp-test-haskell-unwrap-sexp-when-there-is-trailing-quote ()
  (sp-test-with-temp-buffer "test c d = myFunction' c + (myFunct|ion' d)"
      (haskell-mode)
    (sp-unwrap-sexp)
    (sp-buffer-equals "test c d = myFunction' c + myFunct|ion' d")))

;; #710
(ert-deftest sp-test-haskell-splice-char-sexp ()
  (sp-test-with-temp-buffer "'|a'"
      (haskell-mode)
    (sp-splice-sexp)
    (sp-buffer-equals "|a")))

;; #710
(ert-deftest sp-test-haskell-unwrap-char-sexp ()
  (sp-test-with-temp-buffer "'|a'"
      (haskell-mode)
    (sp-unwrap-sexp)
    (sp-buffer-equals "|a")))

(ert-deftest sp-test-haskell-sp-backward-kill-words-respects-pairs-in-strict-mode ()
  (sp-test-with-temp-buffer "foo a = foldr (+) []| b"
      (haskell-mode)
    (smartparens-strict-mode 1)
    (sp-backward-kill-word 1)
    (sp-buffer-equals "foo a = | (+) [] b")))
