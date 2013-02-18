;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.


(Given "^I turn on smartparens globally$"
       (lambda ()
         (smartparens-global-mode 1)))

(Given "^I turn on smartparens$"
       (lambda ()
         (smartparens-mode 1)))

(Given "^I turn off smartparens$"
       (lambda ()
         (smartparens-mode -1)))
