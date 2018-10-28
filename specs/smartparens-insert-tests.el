;; -*- lexical-binding: t -*-

(require 'smartparens)
(require 'smartparens-buttercup-helpers)

(require 'test-helper)

(defvar sp--test-pairs nil)

(defun sp-test-insertion (initial keys result)
  (let ((sp-pairs sp--test-pairs))
    (sp-test-with-temp-elisp-buffer initial
      (execute-kbd-macro keys)
      (sp-bc-buffer-equals result))))

(defun sp-test-insertion-keep-pairs (initial keys result)
  (sp-test-with-temp-elisp-buffer initial
    (execute-kbd-macro keys)
    (sp-bc-buffer-equals result)))

(describe "Inserting pairs"

  (describe "basic pairs"

    (before-all
      (setq sp--test-pairs sp--test-basic-pairs))

    (it "of parens"
      (sp-test-insertion "|" "(" "()"))

    (it "of nested parens"
      (sp-test-insertion "|" "((" "(())"))

    (it "after closing some of the inserted parens"
      (sp-test-insertion "|" "(()(" "(()())"))

    (it "of braces"
      (sp-test-insertion "|" "[[" "[[]]"))

    (it "after closing some of the inserted braces"
      (sp-test-insertion "|" "[[][" "[[][]]"))

    (it "of pairs defined as text"
      (sp-test-insertion "|" "OPEN" "OPENCLOSE"))

    (it "of pairs defined as text starting with a backslash"
      (sp-test-insertion "|" "\\langle" "\\langle\\rangle"))

    (it "while preceeded by non-pair text"
      (sp-test-insertion "|" "foo \\langle" "foo \\langle\\rangle"))

    (it "with a trigger"
      (sp-test-insertion "foo |" "\\b" "foo \\big(\\big)"))

    (it "of multiple characters"
      (sp-test-insertion "|" "\\{" "\\{\\}")))


  (describe "shared prefix"

    (before-all
      (setq
       sp--test-pairs
       '((t . ((:open "`" :close "'" :actions (insert wrap autoskip navigate))
               (:open "``" :close "''" :actions (insert wrap autoskip navigate))
               (:open "." :close "-" :actions (insert wrap autoskip navigate))
               (:open ".;" :close "-+" :actions (insert wrap autoskip navigate))
               (:open ".;:" :close "-+_" :actions (insert wrap autoskip navigate)))))))

    (describe "where the opening character is repeated"

      (it "single pair"
        (sp-test-insertion "|" "`" "`'"))

      (it "same pair repeated twice"
        (sp-test-insertion "|" "``" "``''"))

      (it "same pair repeated three times"
        (sp-test-insertion "|" "```" "```'''"))

      (it "same pair repeated four times"
        (sp-test-insertion "|" "````" "````''''"))

      (it "inserting a pair from within itself"
        (sp-test-insertion "`|'" "`" "``''"))

      (it "inserting a pair from within itself nested multiple times"
        (sp-test-insertion "```|'''" "`" "````''''")))

    (describe "where the opening character is not repeated"

      (it "single pair"
        (sp-test-insertion "|" "." ".-"))

      (it "inserting two pairs with shared prefix and suffix"
        (sp-test-insertion "|" ".;" ".;-+"))

      (it "inserting three pairs with shared prefix and suffix"
        (sp-test-insertion "|" ".;:" ".;:-+_"))

      (it "inserting three pairs with shared prefix and suffix with two already present"
        (sp-test-insertion ".;|-+" ":" ".;:-+_"))))

  (describe "opening same as closing"

    ;; the deal here is that we have pairs of one or two stars and so
    ;; the nesting needs to be smart about how many stars to insert

    (before-all
      (setq
       sp--test-pairs
       '((t . ((:open "*" :close "*" :actions (insert wrap autoskip navigate))
               (:open "**" :close "**" :actions (insert wrap autoskip navigate)))))))

    (it "single pair"
      (sp-test-insertion "|" "*" "**"))

    (it "single pair repeated"
      (sp-test-insertion "|" "**" "****"))

    (it "single pair repeated three times"
      (sp-test-insertion "|" "***" "******"))

    (it "single pair repeated four times"
      (sp-test-insertion "|" "****" "********"))

    (it "inserting a pair inside a pair"
      (sp-test-insertion "*|*" "*" "****"))

    (it "inserting a single pair inside the longer pair"
      (sp-test-insertion "***|***" "*" "********"))

    (it "double pair when there is one unpaired star already present"
      (sp-test-insertion "*|" "*" "****")))

  (describe "shared suffix of open with prefix of closed"

    (before-all
      (setq
       sp--test-pairs
       '((t . ((:open "{" :close "}" :actions (insert wrap autoskip navigate))
               (:open "{-" :close "-}" :actions (insert wrap autoskip navigate))
               (:open "{--" :close "--}" :actions (insert wrap autoskip navigate) :trigger "a"))))))


    (it "single pair"
      (sp-test-insertion "|" "{" "{}"))

    (it "inserting nested pair"
      (sp-test-insertion "|" "{-" "{--}"))

    (it "inserting nested pair twice"
      (sp-test-insertion "|" "{--" "{----}"))

    (it "extending nested pair once"
      (sp-test-insertion "{-|-}" "-" "{----}")))

  (it "with spaces"
    (let ((sp--test-pairs '((t . ((:open "[ " :close " ]" :actions (insert wrap autoskip navigate)))))))
      (sp-test-insertion "|" "[ " "[ | ]")))

  (describe "with conditions"

    (describe "when"

      (before-all
        (setq
         sp--test-pairs
         '((t . ((:open "[" :close "]"
                  :actions (insert wrap autoskip navigate)
                  :when (sp-in-string-p)))))))

      (it "in string should insert a pair"
        (sp-test-insertion "\"foo | bar\"" "[" "\"foo [|] bar\""))

      (it "not in string should not insert a pair"
        (sp-test-insertion "foo | bar" "[" "foo [| bar")))

    (describe "unless"

      (before-all
        (setq
         sp--test-pairs
         '((t . ((:open "[" :close "]"
                  :actions (insert wrap autoskip navigate)
                  :unless (sp-in-string-p)))))))

      (it "in string do not insert a pair"
        (sp-test-insertion "\"foo | bar\"" "[" "\"foo [| bar\""))

      (it "not in string insert a pair"
        (sp-test-insertion "foo | bar" "[" "foo [|] bar"))))

  (it "sp-test-insertion-pair-when-in-string-override-setting"
    (let ((sp-pairs (-clone sp-pairs)))
      (sp-local-pair '(emacs-lisp-mode lisp-mode) "%" "$")
      (sp-test-insertion-keep-pairs "foo | bar" "%" "foo %|$ bar")
      (sp-local-pair 'emacs-lisp-mode "%" nil :when '(sp-in-string-p))
      (sp-test-insertion-keep-pairs "\"foo | bar\"" "%" "\"foo %|$ bar\"")
      (sp-test-insertion-keep-pairs "foo | bar" "%" "foo %| bar")))

  (it "sp-test-insertion-pair-replace-existing-ending-for-pair-with-shared-prefix"
    ;; If the to-be-inserted pair has a prefix which is also a pair we
    ;; migth be extending the opener of a sexp with this opener.  In
    ;; which case we should probably rewrap.
    (let ((sp-pairs (-clone sp-pairs)))
      (sp-test-with-temp-elisp-buffer "{| foo }"
        (sp-update-local-pairs '(:open "{" :close "}" :actions (insert navigate)))
        (sp-update-local-pairs '(:open "{-" :close "-}" :actions (insert navigate)))
        (execute-kbd-macro "-")
        (sp-buffer-equals "{-| foo -}"))))

  (describe "in latex"

    (before-all
      (shut-up (load "auctex-autoloads"))
      (setq
       sp--test-pairs
       '((latex-mode
          (:open "$" :close "$" :actions (insert wrap autoskip navigate))
          (:open "\\[" :close "\\]" :actions (insert wrap autoskip navigate))
          (:open "\\bigl(" :close "\\bigr)" :actions (insert wrap autoskip navigate))
          (:open "[" :close "]" :actions (insert wrap autoskip navigate))
          (:open "``" :close "''" :trigger "\"" :actions (insert wrap autoskip navigate))
          (:open "`" :close "'" :actions (insert wrap autoskip navigate))))))

    (defun sp-test-latex-insertion (initial keys result)
      (let ((sp-undo-pairs-separately nil)
            (sp-pairs sp--test-pairs))
        (sp-test-with-temp-buffer initial
            (latex-mode)
          (shut-up (execute-kbd-macro keys))
          (sp-bc-buffer-equals result))))


    (it "simple string-like pair"
      (sp-test-latex-insertion "|" "$" "$$"))

    (it "quote pair"
      (sp-test-latex-insertion "|" "`" "`'"))

    (it "simple string-like pair and then type closing delimiter again"
      (sp-test-latex-insertion "|" "$$" "$$"))

    (it "test skipping out of active sexp"
      (sp-test-latex-insertion "|" "$foo$$foo" "$foo$$foo$"))

    (it "test pairing in non-empty buffer"
      (sp-test-latex-insertion "foo |" "$" "foo $$"))

    (it "multi character pair"
      (sp-test-latex-insertion "|" "\\[" "\\[\\]"))

    (it "extending single inserted character to a multi character pair"
      (sp-test-latex-insertion "\\|" "[" "\\[\\]"))

    (it "simple regular pair"
      (sp-test-latex-insertion "|" "[" "[]"))

    (it "a word pair"
      (sp-test-latex-insertion "foo | bar" "\\bigl(" "foo \\bigl(\\bigr) bar"))

    (it "should nest two backticks into a string pair"
      (sp-test-latex-insertion "foo | bar" "``" "foo ``'' bar"))

    (it "should insert ``'' when a quote \" is triggered"
      (sp-test-latex-insertion "foo | bar" "\"" "foo ``'' bar")))

  )
