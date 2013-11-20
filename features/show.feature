Feature: Show

  Scenario: Emacs Lisp hang bug
    Given I insert:
      """
      (add-to-list 'auto-mode-alist '("\\.html?\\'" . html-mode))
      """
    And I turn on emacs-lisp-mode
    And I turn on smartparens
    And I turn on show smartparens
    When I go to point "45"
    And I press "C-b"
