Feature: Autoinsert pairs
  In order to type less
  As a user
  I want to insert pairs automatically

  Scenario: Insert pair
    Given I turn on rst-mode
      And I turn on smartparens
     When I press "`"
     Then I should see "``"

  Scenario: Wrap pair
    Given I turn on rst-mode
      And I turn on smartparens
     When I insert "This is some text"
      And I select "is some"
      And I press "`"
     Then I should see "This `is some` text"
