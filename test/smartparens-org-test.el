(require 'smartparens-org)


;;; star pair
(ert-deftest sp-test-org-insert-new-top-level-headline-item ()
  (sp-test-with-temp-buffer "|"
      (org-mode)
    (execute-kbd-macro "* foo bar")
    (sp-buffer-equals "* foo bar|")))

(ert-deftest sp-test-org-insert-new-nested-headline-item ()
  (sp-test-with-temp-buffer "|"
      (org-mode)
    (execute-kbd-macro "** foo bar")
    (sp-buffer-equals "** foo bar|")))

(ert-deftest sp-test-org-insert-star-pair ()
  (sp-test-with-temp-buffer "|"
      (org-mode)
    (execute-kbd-macro "foo *bar")
    (sp-buffer-equals "foo *bar|*")))

(ert-deftest sp-test-org-do-not-insert-star-pair-after-word ()
  (sp-test-with-temp-buffer "|"
      (org-mode)
    (execute-kbd-macro "foo bar* baz")
    (sp-buffer-equals "foo bar* baz|")))

(ert-deftest sp-test-org-skip-over-star-pair ()
  (sp-test-with-temp-buffer "|"
      (org-mode)
    (execute-kbd-macro "foo *bar* baz")
    (sp-buffer-equals "foo *bar* baz|")))

(ert-deftest sp-test-org-wrap-star-pair ()
  (sp-test-with-temp-buffer "foo |barM baz"
      (org-mode)
    (execute-kbd-macro "*")
    (sp-buffer-equals "foo *|barM* baz")))

(ert-deftest sp-test-org-skip-headline-asterisk ()
  (sp-test-with-temp-buffer "|*** Foo"
      (org-mode)
    (should-not (sp-get-sexp))))

(ert-deftest sp-test-org-do-not-skip-emphasis-asterisk ()
  (sp-test-with-temp-buffer "some |*foo* text"
      (org-mode)
    (should (sp-get-sexp))))

(ert-deftest sp-test-org-do-not-skip-emphasis-asterisk-at-bol ()
  (sp-test-with-temp-buffer "|*foo* text"
      (org-mode)
    (should (sp-get-sexp))))

(ert-deftest sp-test-org-do-not-skip-emphasis-asterisk-at-bol-backwards ()
  (sp-test-with-temp-buffer "*foo*| text"
      (org-mode)
    (should (sp-get-sexp t))))


;;; slash pair
(ert-deftest sp-test-org-insert-slash-pair ()
  (sp-test-with-temp-buffer "|"
      (org-mode)
    (execute-kbd-macro "foo /bar")
    (sp-buffer-equals "foo /bar|/")))

(ert-deftest sp-test-org-do-not-insert-star-pair-after-word ()
  (sp-test-with-temp-buffer "|"
      (org-mode)
    (execute-kbd-macro "foo bar/ baz")
    (sp-buffer-equals "foo bar/ baz|")))

(ert-deftest sp-test-org-skip-over-slash-pair ()
  (sp-test-with-temp-buffer "|"
      (org-mode)
    (execute-kbd-macro "foo /bar/ baz")
    (sp-buffer-equals "foo /bar/ baz|")))

(ert-deftest sp-test-org-wrap-slash-pair ()
  (sp-test-with-temp-buffer "foo |barM baz"
      (org-mode)
    (execute-kbd-macro "/")
    (sp-buffer-equals "foo /|barM/ baz")))

(ert-deftest sp-test-org-unpair-slash-after-space ()
  (sp-test-with-temp-buffer "|"
      (org-mode)
    (execute-kbd-macro "foo / bar")
    (sp-buffer-equals "foo / bar")))


;;; tilde pair
(ert-deftest sp-test-org-insert-tilde-pair ()
  (sp-test-with-temp-buffer "|"
      (org-mode)
    (execute-kbd-macro "foo ~bar")
    (sp-buffer-equals "foo ~bar|~")))

(ert-deftest sp-test-org-do-not-insert-star-pair-after-word ()
  (sp-test-with-temp-buffer "|"
      (org-mode)
    (execute-kbd-macro "foo bar~ baz")
    (sp-buffer-equals "foo bar~ baz|")))

(ert-deftest sp-test-org-skip-over-tilde-pair ()
  (sp-test-with-temp-buffer "|"
      (org-mode)
    (execute-kbd-macro "foo ~bar~ baz")
    (sp-buffer-equals "foo ~bar~ baz|")))

(ert-deftest sp-test-org-wrap-tilde-pair ()
  (sp-test-with-temp-buffer "foo |barM baz"
      (org-mode)
    (execute-kbd-macro "~")
    (sp-buffer-equals "foo ~|barM~ baz")))

(ert-deftest sp-test-org-unpair-tilde-after-space ()
  (sp-test-with-temp-buffer "|"
      (org-mode)
    (execute-kbd-macro "foo ~ bar")
    (sp-buffer-equals "foo ~ bar")))


;;; equals pair
(ert-deftest sp-test-org-insert-equals-pair ()
  (sp-test-with-temp-buffer "|"
      (org-mode)
    (execute-kbd-macro "foo =bar")
    (sp-buffer-equals "foo =bar|=")))

(ert-deftest sp-test-org-do-not-insert-star-pair-after-word ()
  (sp-test-with-temp-buffer "|"
      (org-mode)
    (execute-kbd-macro "foo bar= baz")
    (sp-buffer-equals "foo bar= baz|")))

(ert-deftest sp-test-org-skip-over-equals-pair ()
  (sp-test-with-temp-buffer "|"
      (org-mode)
    (execute-kbd-macro "foo =bar= baz")
    (sp-buffer-equals "foo =bar= baz|")))

(ert-deftest sp-test-org-wrap-equals-pair ()
  (sp-test-with-temp-buffer "foo |barM baz"
      (org-mode)
    (execute-kbd-macro "=")
    (sp-buffer-equals "foo =|barM= baz")))

(ert-deftest sp-test-org-unpair-equals-after-space ()
  (sp-test-with-temp-buffer "|"
      (org-mode)
    (execute-kbd-macro "foo = bar")
    (sp-buffer-equals "foo = bar")))
