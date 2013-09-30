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
 (setq my-counter 1)
 (global-set-key (kbd "C-c s") 'sp-split-sexp)
 )

(Before
 ;; Load the default config
 (load-file "smartparens-config.el")
 (load-file "smartparens-latex.el")

 ;; Before each scenario is run
 (switch-to-buffer
  (get-buffer-create "*smartparens*"))
 (erase-buffer)
 (fundamental-mode))

(After
 (let ((buf (get-buffer "*new*")))
   (when buf (kill-buffer buf)))

 ;; Reset all the customization done to sp
 (dolist (var (get 'smartparens 'custom-group))
   (when (equal 'custom-variable (cadr var))
     (set (car var) (car (get (car var) 'standard-value)))))

 ;; Reset the pairs
 (setq sp-pairs '((t
                    .
                    ((:open "\\\\(" :close "\\\\)" :actions (insert wrap))
                     (:open "\\{"   :close "\\}"   :actions (insert wrap))
                     (:open "\\("   :close "\\)"   :actions (insert wrap))
                     (:open "\\\""  :close "\\\""  :actions (insert wrap))
                     (:open "/*"    :close "*/"    :actions (insert wrap))
                     (:open "\""    :close "\""    :actions (insert wrap))
                     (:open "'"     :close "'"     :actions (insert wrap))
                     (:open "("     :close ")"     :actions (insert wrap))
                     (:open "["     :close "]"     :actions (insert wrap))
                     (:open "{"     :close "}"     :actions (insert wrap))
                     (:open "`"     :close "`"     :actions (insert wrap))))))

 ;; reset the state between scenarios to not interfere
 (setq sp-escape-char nil)
 (setq sp-pair-list nil)
 (setq sp-local-pairs nil)
 (setq sp-last-operation nil)
 (setq sp-previous-point -1)
 (setq sp-wrap-point nil)
 (setq sp-wrap-mark nil)
 (setq sp-last-inserted-characters "")
 (setq sp-last-wrapped-region nil)
 (setq sp-point-inside-string nil)
 (setq sp-tags nil)
 (setq sp-pair-overlay-list '())
 (setq sp-wrap-overlays nil)
 (setq sp-wrap-tag-overlays nil)
 (setq sp-recent-keys nil)

 ;; Disable smartparens-mode
 (smartparens-mode -1)
 (smartparens-global-mode -1)
 (show-smartparens-mode -1)
 (show-smartparens-global-mode -1))

(Teardown
 ;; After when everything has been run
 )
