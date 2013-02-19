Feature: Autoinsert pairs
  In order to type less
  As a user
  I want to insert pairs automatically

  Scenario: Add local pair
    Given I add a local pair "'/!" on "rst-mode"
      And I turn on rst-mode
      And I turn on smartparens
     When I press "'"
     Then I should see "'!"
    Given I switch to buffer "*new*"
      And I turn on text-mode
      And I turn on smartparens
     When I press "'"
     Then I should see "''"

  Scenario: Modify pair option of sub-group of modes previously added
    Given I add a local pair "*/*" on "rst-mode,python-mode"
      And I add a local pair "*/*" on "python-mode" enabled only in string
      And I turn on rst-mode
      And I turn on smartparens
     When I press "*"
     Then I should see "**"
    Given I switch to buffer "*new*"
      And I turn on python-mode
      And I turn on smartparens
     When I press "*"
     Then I should not see "**"
     When I insert "'This is some text'"
      And I go to word "some"
      And I press "SPC C-b"
      And I press "*"
     Then I should see "**"
