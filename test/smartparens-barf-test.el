(require 'smartparens)

(ert-deftest sp-test-sp-forward-barf-sexp-634 ()
  (let ((sp-barf-move-point-with-delimiter nil))
    (sp-test-with-temp-elisp-buffer "(let ((a 4)\n      ;; (fail)\n      |(+ 1)\n      ))\n"
      (call-interactively 'sp-forward-barf-sexp)
      (sp-buffer-equals "(let ((a 4))\n  ;; (fail)\n  (+ 1)\n  )\n")
      (call-interactively 'sp-forward-barf-sexp)
      (sp-buffer-equals "(let ((a 4)))\n;; (fail)\n|(+ 1)\n\n"))))

(ert-deftest sp-test-sp-forward-barf-918-move-point-with-closing-enabled ()
  (let ((sp-barf-move-point-with-delimiter t))
    (sp-test-with-temp-elisp-buffer "(hello world\n\n     |  what\n       is this)"
      (call-interactively 'sp-forward-barf-sexp)
      (sp-buffer-equals "(hello world\n\n     |  what\n       is) this")
      (call-interactively 'sp-forward-barf-sexp)
      (sp-buffer-equals "(hello world\n\n     |  what)\nis this")
      (call-interactively 'sp-forward-barf-sexp)
      (sp-buffer-equals "(hello world|)\n\nwhat\nis this"))))

(ert-deftest sp-test-sp-forward-barf-918-move-point-with-closing-disabled ()
  (let ((sp-barf-move-point-with-delimiter nil))
    (sp-test-with-temp-elisp-buffer "(hello world\n\n     |  what\n       is this)"
      (call-interactively 'sp-forward-barf-sexp)
      (sp-buffer-equals "(hello world\n\n     |  what\n       is) this")
      (call-interactively 'sp-forward-barf-sexp)
      (sp-buffer-equals "(hello world\n\n     |  what)\nis this")
      (call-interactively 'sp-forward-barf-sexp)
      (sp-buffer-equals "(hello world)\n\n|what\nis this"))))

(ert-deftest sp-test-sp-backward-barf-918-move-point-with-opening-enabled ()
  (let ((sp-barf-move-point-with-delimiter t))
    (sp-test-with-temp-elisp-buffer "(hello world\n\n     |  what\n       is this)"
      (call-interactively 'sp-backward-barf-sexp)
      (sp-buffer-equals "hello (world\n\n     |  what\n       is this)")
      (call-interactively 'sp-backward-barf-sexp)
      (sp-buffer-equals "hello world\n\n(|what\n is this)")
      (call-interactively 'sp-backward-barf-sexp)
      (sp-buffer-equals "hello world\n\nwhat\n(|is this)"))))

(ert-deftest sp-test-sp-backward-barf-918-move-point-with-opening-enabled-with-prefix ()
  (let ((sp-barf-move-point-with-delimiter t))
    (sp-test-with-temp-elisp-buffer ",@(hello world\n\n       |  what\n         is this)"
      (call-interactively 'sp-backward-barf-sexp)
      (sp-buffer-equals "hello ,@(world\n\n       |  what\n         is this)")
      (call-interactively 'sp-backward-barf-sexp)
      (sp-buffer-equals "hello world\n\n,@(|what\n   is this)")
      (call-interactively 'sp-backward-barf-sexp)
      (sp-buffer-equals "hello world\n\nwhat\n,@(|is this)"))))

(ert-deftest sp-test-sp-backward-barf-918-move-point-with-opening-disabled ()
  (let ((sp-barf-move-point-with-delimiter nil))
    (sp-test-with-temp-elisp-buffer "(hello world\n\n     |  what\n       is this)"
      (call-interactively 'sp-backward-barf-sexp)
      (sp-buffer-equals "hello (world\n\n     |  what\n       is this)")
      (call-interactively 'sp-backward-barf-sexp)
      (sp-buffer-equals "hello world\n\n|(what\n is this)"))))
