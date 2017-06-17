;; Tests for configuration, pair merging, `sp-local-pairs' updates and so on.

(require 'smartparens)

(defun sp-test-config--pairs-comparator (a b)
  (string< (car a) (car b)))

(defun sp-test-config--sort-pairs (pairs)
  (-sort 'sp-test-config--pairs-comparator pairs))

(defun sp-test-config--equal-sets (this other)
  (equal (sp-test-config--sort-pairs this)
         (sp-test-config--sort-pairs other)))

(ert-deftest sp-test-config--update-local-pairs-with-override ()
  (let ((sp-local-pairs nil)
        (sp-pair-list nil)
        (sp-pairs '((t (:open "[" :close "]" :actions (insert))
                       (:open "(" :close ")" :actions (insert))
                       (:open "`" :close "'" :actions (insert)))
                    (python-mode (:open "`" :close "`" :actions (insert))
                                 (:open "<" :close ">" :actions (insert))))))
    (with-temp-buffer
      (shut-up (python-mode))
      (sp--update-local-pairs)
      (should (sp-test-config--equal-sets
               sp-pair-list
               '(("[" . "]") ("(" . ")") ("`" . "`") ("<" . ">")))))))

(ert-deftest sp-test-config--update-local-pairs-without-override ()
  (let ((sp-local-pairs nil)
        (sp-pair-list nil)
        (sp-pairs '((t (:open "[" :close "]" :actions (insert))
                       (:open "(" :close ")" :actions (insert))
                       (:open "`" :close "'" :actions (insert))))))
    (with-temp-buffer
      (shut-up (python-mode))
      (sp--update-local-pairs)
      (should (sp-test-config--equal-sets
               sp-pair-list
               '(("[" . "]") ("(" . ")") ("`" . "'")))))))

(ert-deftest sp-test-config--update-local-pairs-with-parent-override ()
  (let ((sp-local-pairs nil)
        (sp-pair-list nil)
        (sp-pairs '((t (:open "[" :close "]" :actions (insert))
                       (:open "(" :close ")" :actions (insert))
                       (:open "`" :close "'" :actions (insert)))
                    (prog-mode (:open "`" :close "`" :actions (insert))
                               (:open "<" :close ">" :actions (insert))))))
    (with-temp-buffer
      (shut-up (python-mode))
      (sp--update-local-pairs)
      (should (sp-test-config--equal-sets
               sp-pair-list
               '(("[" . "]") ("(" . ")") ("`" . "`") ("<" . ">")))))))

(ert-deftest sp-test-config--drop-local-pairs-without-actions ()
  (let ((sp-local-pairs nil)
        (sp-pair-list nil)
        (sp-pairs '((t (:open "[" :close "]" :actions (insert))
                       (:open "(" :close ")" :actions (insert))
                       (:open "`" :close "'" :actions (insert)))
                    (prog-mode (:open "[" :close "]" :actions nil))
                    (python-mode (:open "`" :close "`" :actions (insert))
                                 (:open "(" :close ")" :actions nil)))))
    (with-temp-buffer
      (shut-up (python-mode))
      (sp--update-local-pairs)
      (should (sp-test-config--equal-sets sp-pair-list '(("`" . "`")))))))

(ert-deftest sp-test-config--change-after-redefinition ()
  (let ((sp-local-pairs nil)
        (sp-pair-list nil)
        (sp-pairs '((t (:open "[" :close "]" :actions (insert))
                       (:open "(" :close ")" :actions (insert))
                       (:open "`" :close "'" :actions (insert)))
                    (foo (:open "[" :close "]" :actions nil))
                    (python-mode (:open "`" :close "`" :actions (insert))
                                 (:open "(" :close ")" :actions nil))))
        (buffer-a (generate-new-buffer " *temp*"))
        (buffer-b (generate-new-buffer " *temp*")))
    (with-current-buffer buffer-a
      (shut-up (python-mode))
      (smartparens-mode 1)
      (should (sp-test-config--equal-sets sp-pair-list '(("[" . "]") ("`" . "`")))))
    (with-current-buffer buffer-b
      (shut-up (python-mode))
      (smartparens-mode 1)
      (sp-update-local-pairs 'foo)
      (should (sp-test-config--equal-sets sp-pair-list '(("`" . "`")))))

    (sp-local-pair 'python-mode "`" "'")

    (with-current-buffer buffer-a
      (should (sp-test-config--equal-sets sp-pair-list
                                          '(("[" . "]") ("`" . "'")))))
    (with-current-buffer buffer-b
      (should (sp-test-config--equal-sets sp-pair-list '(("`" . "'")))))))

(ert-deftest sp-test-config--change-after-redefinition-of-parent ()
  (let ((sp-local-pairs nil)
        (sp-pair-list nil)
        (sp-pairs '((t (:open "[" :close "]" :actions (insert))
                       (:open "(" :close ")" :actions (insert))
                       (:open "`" :close "'" :actions (insert)))
                    (prog-mode (:open "`" :close "`" :actions (insert))
                               (:open "(" :close ")" :actions nil))))
        (buffer-a (generate-new-buffer " *temp*"))
        (buffer-b (generate-new-buffer " *temp*")))
    (with-current-buffer buffer-a
      (shut-up (python-mode))
      (smartparens-mode 1)
      (should (sp-test-config--equal-sets sp-pair-list '(("[" . "]") ("`" . "`")))))
    (with-current-buffer buffer-b
      (shut-up (python-mode))
      (smartparens-mode 1)
      (should (sp-test-config--equal-sets sp-pair-list '(("[" . "]") ("`" . "`")))))

    (sp-local-pair 'prog-mode "`" "'")

    (with-current-buffer buffer-a
      (should (sp-test-config--equal-sets sp-pair-list '(("[" . "]") ("`" . "'")))))
    (with-current-buffer buffer-b
      (should (sp-test-config--equal-sets sp-pair-list '(("[" . "]") ("`" . "'")))))))

(ert-deftest sp-test-config--update-local-pairs-with-symbol ()
  (let ((sp-local-pairs nil)
        (sp-pair-list nil)
        (sp-pairs '((t (:open "[" :close "]" :actions (insert))
                       (:open "(" :close ")" :actions (insert))
                       (:open "`" :close "'" :actions (insert)))
                    (foo-configuration (:open "`" :close "`" :actions (insert))
                                       (:open "(" :close ")" :actions nil)))))
    (with-temp-buffer
      (shut-up (python-mode))
      (sp--update-local-pairs)
      (sp-update-local-pairs 'foo-configuration)
      (should (sp-test-config--equal-sets
               sp-pair-list '(("[" . "]") ("`" . "`")))))))

(ert-deftest sp-test-config--update-local-pairs-single-anonymous ()
  (let ((sp-local-pairs nil)
        (sp-pair-list nil)
        (sp-pairs '((t (:open "[" :close "]" :actions (insert))
                       (:open "(" :close ")" :actions (insert))
                       (:open "`" :close "'" :actions (insert))))))
    (with-temp-buffer
      (shut-up (python-mode))
      (sp--update-local-pairs)
      (sp-update-local-pairs (list :open "(" :actions nil))
      (should (sp-test-config--equal-sets
               sp-pair-list '(("[" . "]") ("`" . "'")))))))

(ert-deftest sp-test-config--update-local-pairs-multiple-anonymous ()
  (let ((sp-local-pairs nil)
        (sp-pair-list nil)
        (sp-pairs '((t (:open "[" :close "]" :actions (insert))
                       (:open "(" :close ")" :actions (insert))
                       (:open "`" :close "'" :actions (insert))))))
    (with-temp-buffer
      (shut-up (python-mode))
      (sp--update-local-pairs)
      (sp-update-local-pairs (list (list :open "(" :actions nil)
                                   (list :open "`" :close "`")))
      (should (sp-test-config--equal-sets
               sp-pair-list '(("[" . "]") ("`" . "`")))))))

(ert-deftest sp-test-config--update-after-major-mode-activation ()
  (let ((sp-local-pairs nil)
        (sp-pair-list nil)
        (sp-pairs '((python-mode (:open "`" :close "`" :actions (insert))
                                 (:open "<" :close ">" :actions (insert))))))
    (with-temp-buffer
      (shut-up (python-mode))
      (smartparens-mode 1)
      (should (sp-test-config--equal-sets
               sp-pair-list
               '(("<" . ">") ("`" . "`")))))))

(ert-deftest sp-test-config--update-after-major-mode-change ()
  (let ((sp-local-pairs nil)
        (sp-pair-list nil)
        (sp-pairs '((python-mode (:open "`" :close "`" :actions (insert))
                                 (:open "<" :close ">" :actions (insert)))
                    (text-mode (:open "[" :close "]" :actions (insert))
                               (:open "(" :close ")" :actions (insert))))))
    (with-temp-buffer
      (shut-up (python-mode))
      (smartparens-mode 1)
      (should (sp-test-config--equal-sets
               sp-pair-list
               '(("<" . ">") ("`" . "`"))))
      (shut-up (text-mode))
      (smartparens-mode 1)
      (should (sp-test-config--equal-sets
               sp-pair-list
               '(("[" . "]") ("(" . ")")))))))

(ert-deftest sp-test-config--update-custom-pairs-definition ()
  (let ((sp-local-pairs nil)
        (sp-pair-list nil)
        (sp-pairs nil))
    ;; custom
    (sp-local-pair 'foo "(" ")")
    ;; major-mode
    (sp-local-pair 'python-mode "[" "]")
    ;; parent-mode
    (sp-local-pair 'prog-mode "'" "'")
    (with-temp-buffer
      (shut-up (python-mode))
      (smartparens-mode 1)
      (should (sp-test-config--equal-sets
               sp-pair-list '(("[" . "]") ("'" . "'"))))
      (sp-update-local-pairs 'foo)
      (should (sp-test-config--equal-sets
               sp-pair-list '(("(" . ")") ("[" . "]") ("'" . "'")))))))
