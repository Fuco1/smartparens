;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.

(When "^I exchange point and mark$"
      (lambda ()
	(exchange-point-and-mark)))

(Then "^last operation should be \"\\([^\"]+\\)\"$"
      (lambda (value)
	(let ((message "sp-last-action should be %s, but is %s")
	      (last-op (symbol-name sp-last-operation)))
	  (assert (equal last-op value) nil message value last-op))))

(Then "^last wrapped region should be non-nil$"
      (lambda ()
	(assert (not (null sp-last-wrapped-region)) nil "sp-last-wrapped-region should be non-nil, but is nil")))

(Given "^I turn on smartparens globally$"
       (lambda ()
         (smartparens-global-mode 1)))

(Given "^I turn on smartparens$"
       (lambda ()
         (smartparens-mode 1)))

(Given "^I turn off smartparens$"
       (lambda ()
         (smartparens-mode -1)))

(Given "^I add a local pair \"\\([^\"]+\\)\" on \"\\([^\"]+\\)\" ?\\(.*\\)$"
       (lambda (pair modes-1 modifier)
         (let ((args (split-string pair "/"))
               (modes (mapcar #'intern (split-string modes-1 ","))))
           (cond
            ((equal "enabled only in string" modifier)
             (setq args (append args '(:when (sp-in-string-p)))))
	    ((equal "enabled only in code" modifier)
	     (setq args (append args '(:when (sp-in-code-p)))))
	    ((equal "disabled only in string" modifier)
	     (setq args (append args '(:unless (sp-in-string-p)))))
	    ((equal "disabled only in code" modifier)
	     (setq args (append args '(:unless (sp-in-code-p))))))
           (apply #'sp-local-pair modes args))))
