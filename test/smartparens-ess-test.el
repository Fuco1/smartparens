(require 'smartparens-ess)
(require 'f)


;; ESS load helpers
(defvar sp-ess-root-path
  (f-parent (f-dirname load-file-name)))

(defvar sp-ess-lisp-path
  (f-join (f-dirname (locate-library "ess-autoloads")) "lisp"))

(add-to-list 'load-path sp-ess-root-path)
(add-to-list 'load-path sp-ess-lisp-path)


(require 'ess-site)


(defun sp-test--ess-mode ()
  (shut-up (R-mode)))

(ert-deftest sp-test-ess-slurp-forward ()
  (sp-test-with-temp-buffer
   "(|)v[1, 2]  ,3"
   (sp-test--ess-mode)
   (sp-forward-slurp-sexp 3)
   (should (equal (buffer-string) "(v[1, 2], 3)"))))

(ert-deftest sp-test-ess-slurp-backward ()
  (sp-test-with-temp-buffer
   "v [1, 2] ,3 (|)"
   (sp-test--ess-mode)
   (sp-backward-slurp-sexp 3)
   (should (equal (buffer-string) "(v[1, 2], 3)"))))
