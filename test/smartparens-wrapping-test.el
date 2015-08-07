(require 'smartparens-config)

(defun sp-test-wrapping (initial keys result)
  (sp-test-with-temp-elisp-buffer initial
    (-each (-list keys) 'execute-kbd-macro)
    (should (equal (buffer-string) (replace-regexp-in-string "[|]" "" result)))
    (should (= (1+ (string-match-p "|" result)) (point)))))

(defun sp-test-wrapping (initial keys result)
  (sp-test-with-temp-elisp-buffer initial
    (-each (-list keys) 'execute-kbd-macro)
    (should (equal (buffer-string) (replace-regexp-in-string "[|]" "" result)))
    (should (= (1+ (string-match-p "|" result)) (point)))))

(ert-deftest sp-test-wrap-basic nil
  (let ((sp-pairs sp--test-basic-pairs))
    (sp-test-wrapping "|aM" "(" "(|a)")
    (sp-test-wrapping "Ma|" "(" "(a)|")

    (sp-test-wrapping "|aM" "[" "[|a]")
    (sp-test-wrapping "Ma|" "[" "[a]|")

    (sp-test-wrapping "|aM" "\\{" "\\{|a\\}")
    (sp-test-wrapping "Ma|" "\\{" "\\{a\\}|")

    (sp-test-wrapping "|aM" "\\\"" "\\\"|a\\\"")
    (sp-test-wrapping "Ma|" "\\\"" "\\\"a\\\"|")

    (sp-test-wrapping "|aM" "\\langle" "\\langle|a\\rangle")
    (sp-test-wrapping "Ma|" "\\langle" "\\langlea\\rangle|")))


(ert-deftest sp-test-wrap-with-closing nil
  (let ((sp-pairs sp--test-basic-pairs))
    (sp-test-wrapping "|aM" "]" "[a]|")
    (sp-test-wrapping "Ma|" "]" "[a]|")
    (sp-test-wrapping "|aM" "\\}" "\\{a\\}|")))

(ert-deftest sp-test-wrap-repeated nil
  (let ((sp-pairs sp--test-basic-pairs))
    (sp-test-wrapping "|aM" "[[" "[[|a]]")
    (sp-test-wrapping "Ma|" "[[" "[[a]]|")))
