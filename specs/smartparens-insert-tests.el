;; -*- lexical-binding: t -*-

(require 'smartparens)
(require 'smartparens-buttercup-helpers)

(require 'test-helper)

(defun sp-test-insertion (initial keys result)
  (sp-test-with-temp-elisp-buffer initial
    (execute-kbd-macro keys)
    (sp-bc-buffer-equals result)))

(defun sp-test-basic-insertion (initial keys result)
  (let ((sp-pairs sp--test-basic-pairs))
    (sp-test-insertion initial keys result)))

(describe "inserting pairs"

  (describe "basic pairs"

    (it "of parens"
      (sp-test-basic-insertion "|" "(" "()"))

    (it "of nested parens"
      (sp-test-basic-insertion "|" "((" "(())"))

    (it "after closing some of the inserted parens"
      (sp-test-basic-insertion "|" "(()(" "(()())"))

    (it "of braces"
      (sp-test-basic-insertion "|" "[[" "[[]]"))

    (it "after closing some of the inserted braces"
      (sp-test-basic-insertion "|" "[[][" "[[][]]"))

    (it "of pairs defined as text"
      (sp-test-basic-insertion "|" "OPEN" "OPENCLOSE"))

    (it "of pairs defined as text starting with a backslash"
      (sp-test-basic-insertion "|" "\\langle" "\\langle\\rangle"))

    (it "while preceeded by non-pair text"
      (sp-test-basic-insertion "|" "foo \\langle" "foo \\langle\\rangle"))

    (it "with a trigger"
      (sp-test-basic-insertion "foo |" "\\b" "foo \\big(\\big)"))

    (it "of multiple characters"
      (sp-test-basic-insertion "|" "\\{" "\\{\\}"))))
