;; #399
(ert-deftest sp-test-get-comment-bounds-c ()
  (sp-test-with-temp-buffer
      "int foo; /* foo|bar */"
      (progn
        (c++-mode)
        ;; because emacs doesn't return proper info on the very first
        ;; call of syntax-ppss, we must call it before we do actual
        ;; parsing .____________.
        (save-excursion (syntax-ppss 1)))
    (should (equal (sp-get-comment-bounds) '(10 . 22))))
  (sp-test-with-temp-buffer
      "int foo; |/* foobar */"
      (progn
        (c++-mode)
        ;; because emacs doesn't return proper info on the very first
        ;; call of syntax-ppss, we must call it before we do actual
        ;; parsing .____________.
        (save-excursion (syntax-ppss 1)))
    (should (equal (sp-get-comment-bounds) '(10 . 22))))
  (sp-test-with-temp-buffer
      "int foo; // foo|bar"
      (progn
        (c++-mode)
        (save-excursion (syntax-ppss 1)))
    (should (equal (sp-get-comment-bounds) '(10 . 19)))))

(ert-deftest sp-test-get-comment-bounds-elisp ()
  (sp-test-with-temp-elisp-buffer
      "(foo bar)   ;; foo| bar"
    (should (equal (sp-get-comment-bounds) '(13 . 23)))
    )
  (sp-test-with-temp-elisp-buffer
      "(foo bar)
    ;; foo| bar
    ;; foo bar
(bar)"
   (should (equal (sp-get-comment-bounds) '(11 . 40))))
(sp-test-with-temp-elisp-buffer
      "(foo bar)
    ;; foo bar
    ;; foo| bar
(bar)"
   (should (equal (sp-get-comment-bounds) '(11 . 40)))))

(ert-deftest sp-test-get-comment-bounds-elisp-with-comment-starting-at-bobp ()
  (sp-test-with-temp-elisp-buffer ";; |foobar"
    (should (equal (sp-get-comment-bounds) '(1 . 10)))))

(ert-deftest sp-test-get-comment-bounds-pascal ()
  (sp-test-with-temp-buffer
      "var foo:integer; (* foo|bar *)"
      (progn
        (pascal-mode)
        (save-excursion (syntax-ppss 1)))
    (should (equal (sp-get-comment-bounds) '(18 . 30))))
  (sp-test-with-temp-buffer
      "var foo:integer; { foo|bar }"
      (progn
        (pascal-mode)
        (save-excursion (syntax-ppss 1)))
    (should (equal (sp-get-comment-bounds) '(18 . 28)))))

(ert-deftest sp-test-get-comment-bounds-pascal-with-comment-starting-at-bobp ()
  (sp-test-with-temp-buffer "(* foo|bar *)"
      (progn
        (pascal-mode)
        (save-excursion (syntax-ppss 1)))
    (should (equal (sp-get-comment-bounds) '(1 . 13)))))
