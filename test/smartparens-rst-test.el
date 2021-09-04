(require 'smartparens-rst)
(require 'rst)

(ert-deftest sp-test-rst-insert-quote-dont-escape-quote nil
  "In text modes where ' and \" are not string syntax, do not
escape them on the top level."
  (let ((sp-pairs
         '((t
            (:open "\"" :close "\""
             :actions (insert wrap autoskip navigate)
             :post-handlers (sp-escape-quotes-after-insert))
            (:open "[" :close "]" :actions (insert wrap autoskip navigate))))))
    (sp-test-with-temp-buffer "foo | bar"
        (rst-mode)
      (execute-kbd-macro "\"")
      (sp-buffer-equals "foo \"|\" bar"))))

(ert-deftest sp-test-rst-insert-quote-dont-escape-in-contraction nil
  "Do not escape ' after a word when it is used as a contraction"
  (let ((sp-pairs
         '((t
            (:open "'" :close "'"
             :actions (insert wrap autoskip navigate escape)
             :unless (sp-in-string-quotes-p sp-point-after-word-p)
             :post-handlers (sp-escape-wrapped-region sp-escape-quotes-after-insert))
            (:open "[" :close "]" :actions (insert wrap autoskip navigate))))))
    (sp-test-with-temp-buffer "foo| bar"
        (rst-mode)
      (execute-kbd-macro "'s")
      (sp-buffer-equals "foo's| bar"))))

(ert-deftest sp-test-rst-no-double-underscore-after-ref nil
  "When turning the backtick reference to a link by appending an
underscore, do not double the underscore automatically."
  (sp-test-with-temp-buffer "foo `bar`|"
      (rst-mode)
    (execute-kbd-macro "_")
    (sp-buffer-equals "foo `bar`_|")))
