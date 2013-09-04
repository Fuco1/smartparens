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

#  Scenario: Slurp a token into empty expression from right
#    When I insert "()bar baz"
#    And I press "C-a"
#    And I press "C-f"
#    And I slurp forward
#    Then I should see "(bar) baz"
#    When I slurp forward
#    Then I should see "(bar baz)"
#
#  Scenario: Slurp a token into empty expression from left
#    When I insert "bar baz()"
#    And I press "C-e"
#    And I press "C-b"
#    And I slurp backward
#    Then I should see "bar (baz)"
#    When I slurp backward
#    Then I should see "(bar baz)"
#
#  Scenario: Slurp a token into nonempty expression from right
#    When I insert "(bar)baz"
#    And I press "C-a"
#    And I press "C-f"
#    And I slurp forward
#    Then I should see "(bar baz)"
#
#  Scenario: Slurp a token into nonempty expression from left
#    When I insert "bar(baz)"
#    And I press "C-e"
#    And I press "C-b"
#    And I slurp backward
#    Then I should see "(bar baz)"