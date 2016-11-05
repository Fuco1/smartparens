(require 'smartparens-python)

(defmacro sp-test-with-temp-python-buffer (initial &rest forms)
  (declare (indent 1)
           (debug (form body)))
  `(sp-test-with-temp-buffer ,initial
       (shut-up (python-mode))
     ,@forms))

(ert-deftest sp-test--python-get-thing-which-is-string-inside-string ()
  (sp-test-with-temp-python-buffer "\"foo bar | 'baz qux' fux\""
    (should (equal (sp-get-thing) '(:beg 11 :end 20 :op "'" :cl "'" :prefix "" :suffix ""))))

  (sp-test-with-temp-python-buffer "\"foo bar 'baz qux' | fux\""
    (should (equal (sp-get-thing t) '(:beg 10 :end 19 :op "'" :cl "'" :prefix "" :suffix "")))))

(ert-deftest sp-test--python-get-expression-which-is-string-inside-string ()
  (sp-test-with-temp-python-buffer "\"foo bar | 'baz qux' fux\""
    (should (equal (sp-get-expression) '(:beg 11 :end 20 :op "'" :cl "'" :prefix "" :suffix ""))))

  (sp-test-with-temp-python-buffer "\"foo bar 'baz qux' | fux\""
    (should (equal (sp-get-expression t) '(:beg 10 :end 19 :op "'" :cl "'" :prefix "" :suffix "")))))

(ert-deftest sp-test--python-get-stringlike-expression-inside-string ()
  (sp-test-with-temp-python-buffer "\"foo bar | 'baz qux' fux\""
    (should (equal (sp-get-stringlike-expression) '(:beg 11 :end 20 :op "'" :cl "'" :prefix "" :suffix ""))))

  (sp-test-with-temp-python-buffer "\"foo bar 'baz qux' | fux\""
    (should (equal (sp-get-stringlike-expression t) '(:beg 10 :end 19 :op "'" :cl "'" :prefix "" :suffix "")))))

(ert-deftest sp-test--python-get-stringlike-or-textmode-expression-inside-string ()
  (sp-test-with-temp-python-buffer "\"foo bar | 'baz qux' fux\""
    (should (equal (sp-get-stringlike-or-textmode-expression) '(:beg 11 :end 20 :op "'" :cl "'" :prefix "" :suffix ""))))

  (sp-test-with-temp-python-buffer "\"foo bar 'baz qux' | fux\""
    (should (equal (sp-get-stringlike-or-textmode-expression t) '(:beg 10 :end 19 :op "'" :cl "'" :prefix "" :suffix "")))))
