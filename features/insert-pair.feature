Feature: Autoinsert pairs
  In order to type less
  As a user
  I want to insert pairs automatically

  Scenario: Insert pair
    Given I turn on rst-mode
      And I turn on smartparens
     When I type "`"
     Then I should see "``"

  Scenario: Insert multichar pair
    Given I turn on smartparens
     When I type "\{"
     Then I should see "\{\}"

Scenario: Insert a pair and skip closing
Given I turn on smartparens
When I type "("
And I type "abc"
And I type ")"
Then I should see "(abc)"
And the cursor should be at point "6"

Scenario: Insert a pair and skip closing
Given I turn on smartparens
When I type "\{"
And I type "abc"
And I type "\}"
Then I should see "\{abc\}"
And the cursor should be at point "8"
