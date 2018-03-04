(ert-deftest sp-test-elisp-invalid-hyperlink/point-before-contraction ()
  (sp-test-with-temp-elisp-buffer "; foo|'bar"
    (should (sp-lisp-invalid-hyperlink-p nil 'navigate nil))))

(ert-deftest sp-test-elisp-invalid-hyperlink/point-after-contraction ()
  (sp-test-with-temp-elisp-buffer "; foo'|bar"
    (should (sp-lisp-invalid-hyperlink-p nil 'navigate nil))))

(ert-deftest sp-test-elisp-invalid-hyperlink/point-before-before-punctuation ()
  (sp-test-with-temp-elisp-buffer "; foo|'."
    (should-not (sp-lisp-invalid-hyperlink-p nil 'navigate nil))))

(ert-deftest sp-test-elisp-invalid-hyperlink/point-after-before-punctuation ()
  (sp-test-with-temp-elisp-buffer "; foo'|."
    (should-not (sp-lisp-invalid-hyperlink-p nil 'navigate nil))))
