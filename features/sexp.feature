Feature: Sexp manipulation

  Background:
    Given I turn on emacs-lisp-mode
    And I turn on smartparens

#  Scenario: Split a sexp
#    When I insert "(foo bar baz)"
#    And I go to the end of the word "bar"
#    And I press "C-c s"
#    Then I should see "(foo bar) (baz)"
#
#  Scenario: Split an enclosing sexp
#    When I insert "(foo bar baz)"
#    And I go to the end of the word "bar"
#    And I press "C-u C-c s"
#    Then I should see "(foo) (bar) (baz)"

  Scenario: Split a string
    When I insert "\"foo bar baz\""
    And I go to the end of the word "bar"
    And I enable string sexps in "emacs-lisp-mode"
    And I press "C-c s"
    Then I should see "\"foo bar\" \"baz\""

  Scenario: Split an enclosing string
    When I insert "\"foo bar baz\""
    And I go to the end of the word "bar"
    And I enable string sexps in "emacs-lisp-mode"
    And I press "C-u C-c s"
    Then I should see "\"foo\" \"bar\" \"baz\""
