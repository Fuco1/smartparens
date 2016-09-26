(require 'ert)
(require 'smartparens)
(require 'lua-mode)



(ert-deftest sp-test-lua-complete-block-in-code ()
  "When inserting the beginning of a block, insert the matching end"
  (sp-test-with-temp-buffer "|"
    (lua-mode)
  (execute-kbd-macro "for ")
  (should (equal (buffer-string) "for  do
end"))))


(ert-deftest sp-test-lua-no-complete-block-in-comment ()
  "When inserting a beginning of block keyword in a comment, don't autocomplete"
  (sp-test-with-temp-buffer "-- |"
    (lua-mode)
  (execute-kbd-macro "for ")
  (should (equal (buffer-string) "-- for "))))
