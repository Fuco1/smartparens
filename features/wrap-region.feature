Feature: Wrap Region
  In order to put text between puctuations and tags
  As an Emacs user
  I want to wrap it

  Scenario: Wrap with pair, point after region
    Given I turn on smartparens
    When I insert "This is some text"
    And I select "is some"
    And I exchange point and mark
    And I type "("
    Then I should see "This (is some) text"
    Then the cursor should be at point "15"

  Scenario: Wrap with pair, point before region
    Given I turn on smartparens
    When I insert "This is some text"
    And I select "is some"
    And I type "("
    Then I should see "This (is some) text"
    Then the cursor should be at point "7"

  Scenario: Wrap with multichar pair, point after region
    Given I turn on smartparens
    When I insert "This is some text"
    And I select "is some"
    And I exchange point and mark
    And I type "\{"
    Then I should see "This \{is some\} text"
    Then the cursor should be at point "17"

  Scenario: Wrap with multichar pair, point before region
    Given I turn on smartparens
    When I insert "This is some text"
    And I select "is some"
    And I type "\{"
    Then I should see "This \{is some\} text"
    Then the cursor should be at point "8"

  Scenario: Wrap in mode with customized pairs where global value should be used
    Given I turn on smartparens
    When I insert "This is some text"
    And I select "is some"
    And I type "`"
    Then I should see "This `is some` text"

  Scenario: Wrap in mode with customized pairs where local value should be used
    Given I turn on latex-mode
    And I turn on smartparens
    When I insert "This is some text"
    And I select "is some"
    And I type "`"
    Then I should see "This `is some' text"


Scenario: Repeatedly wrap with pair, point after region
Given I turn on smartparens
When I insert "This is some text"
And I select "is some"
And I exchange point and mark
And I type "(("
Then I should see "This ((is some)) text"
Then the cursor should be at point "17"

Scenario: Repeatedly wrap with pair, point before region
Given I turn on smartparens
When I insert "This is some text"
And I select "is some"
And I type "(("
Then I should see "This ((is some)) text"
Then the cursor should be at point "8"

Scenario: Repeatedly wrap with multichar pair, point after region
Given I turn on smartparens
When I insert "This is some text"
And I select "is some"
And I exchange point and mark
And I type "\{\{"
Then I should see "This \{\{is some\}\} text"
Then the cursor should be at point "21"

Scenario: Repeatedly Wrap with multichar pair, point before region
Given I turn on smartparens
When I insert "This is some text"
And I select "is some"
And I type "\{\{"
Then I should see "This \{\{is some\}\} text"
Then the cursor should be at point "10"


Scenario: Repeatedly wrap with pair with disabled repeat, point after region
Given I turn on smartparens
And I set sp-wrap-repeat-last to 0
When I insert "This is some text"
And I select "is some"
And I exchange point and mark
And I type "(("
Then I should see "This (is some)() text"
Then the cursor should be at point "16"

Scenario: Repeatedly wrap with pair with disabled repeat, point before region
Given I turn on smartparens
And I set sp-wrap-repeat-last to 0
And I set sp-autoinsert-if-followed-by-word to t
When I insert "This is some text"
And I select "is some"
And I type "(("
Then I should see "This (()is some) text"
Then the cursor should be at point "8"

Scenario: Repeatedly wrap with multichar pair with disabled repeat, point after region
Given I turn on smartparens
And I set sp-wrap-repeat-last to 0
When I insert "This is some text"
And I select "is some"
And I exchange point and mark
And I type "\{\{"
Then I should see "This \{is some\}\{\} text"
Then the cursor should be at point "19"

Scenario: Repeatedly Wrap with multichar pair with disabled repeat, point before region
Given I turn on smartparens
And I set sp-wrap-repeat-last to 0
And I set sp-autoinsert-if-followed-by-word to t
When I insert "This is some text"
And I select "is some"
And I type "\{\{"
Then I should see "This \{\{\}is some\} text"
Then the cursor should be at point "10"
