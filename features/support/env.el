;; This is an example of how you could set up this file. This setup
;; requires a directory called util in the project root and that the
;; util directory contains the testing tools ert and espuds.

(let* ((features-directory
        (file-name-directory
         (directory-file-name (file-name-directory load-file-name))))
       (project-directory
        (file-name-directory
         (directory-file-name features-directory))))
  (setq smartparens-root-path project-directory)
  (setq smartparens-util-path (expand-file-name "util" smartparens-root-path)))

(add-to-list 'load-path smartparens-root-path)
(add-to-list 'load-path (expand-file-name "espuds" smartparens-util-path))
(add-to-list 'load-path (expand-file-name "ert" smartparens-util-path))

(require 'smartparens)
(require 'smartparens-config)
(require 'espuds)
(require 'ert)


(Setup
 ;; Before anything has run
 )

(Before
 ;; Before each scenario is run
 (switch-to-buffer
  (get-buffer-create "*smartparens*"))
 (erase-buffer))

(After
 (let ((buf (get-buffer "*new*")))
   (when buf (kill-buffer buf)))
 ;; Disable smartparens-mode
 (smartparens-mode -1)
 (smartparens-global-mode -1)
 (show-smartparens-mode -1)
 (show-smartparens-global-mode -1))

(Teardown
 ;; After when everything has been run
 )
