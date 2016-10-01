;; Tests which don't fit anywhere else

(ert-deftest sp-test-sp-point-in-symbol-at-the-eob ()
  "When the point is `eobp' it should not be in symbol."
  (sp-test-with-temp-elisp-buffer "foo-bar|"
    (should (not (sp-point-in-symbol)))))

(ert-deftest sp-test-sp-point-in-symbol-inside-symbol ()
  "When the point is inside symbol it should be in symbol."
  (sp-test-with-temp-elisp-buffer "foo-|bar"
    (should (sp-point-in-symbol)))
  (sp-test-with-temp-elisp-buffer "foo-b|ar"
    (should (sp-point-in-symbol))))
