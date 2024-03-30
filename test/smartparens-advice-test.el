;;; Tests for advices and compat with external packages

(require 'yasnippet)
(require 'hippie-exp)

;; These macros are copied from yasnippet-tests.el
(progn
  (defmacro yas-with-snippet-dirs (dirs &rest body)
    (declare (indent defun) (debug t))
    `(yas-call-with-snippet-dirs
      ,dirs #'(lambda () ,@body)))

  (defun yas-make-file-or-dirs (ass)
    (let ((file-or-dir-name (car ass))
          (content (cdr ass)))
      (cond ((listp content)
             (make-directory file-or-dir-name 'parents)
             (let ((default-directory (concat default-directory "/" file-or-dir-name)))
               (mapc #'yas-make-file-or-dirs content)))
            ((stringp content)
             (with-temp-buffer
               (insert content)
               (write-region nil nil file-or-dir-name nil 'nomessage)))
            (t
             (message "[yas] oops don't know this content")))))

  (defun yas-call-with-snippet-dirs (dirs fn)
    (let* ((default-directory (make-temp-file "yasnippet-fixture" t))
           (yas-snippet-dirs (mapcar (lambda (d) (expand-file-name (car d))) dirs)))
      (with-temp-message ""
        (unwind-protect
            (progn
              (mapc #'yas-make-file-or-dirs dirs)
              (funcall fn))
          (when (>= emacs-major-version 24)
            (delete-directory default-directory 'recursive)))))))

(ert-deftest sp-test-advice--hippie-no-pairing-if-sexp-already-exists ()
  "Test that after hippie expand expands a yasnippet template it
won't add an extra closing paren if the snippet already provides
it."
  (yas-with-snippet-dirs
    '((".emacs.d/snippets"
       ("emacs-lisp-mode" ("defun" . "(defun hello ($0))"))))
    (let ((hippie-expand-try-functions-list
           '(yas-hippie-try-expand)))
      (sp-test-with-temp-elisp-buffer "defun|"
        (yas-reload-all)
        (yas-minor-mode 1)
        (call-interactively 'hippie-expand)
        (sp-buffer-equals "(defun hello (|))")))))
