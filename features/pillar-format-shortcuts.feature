Feature: Format shortcuts
  In order to edit text format easily
  As an author using Pillar
  I want to have keyboard shortcuts

  Background:
    Given I am in buffer "foo.pillar"
    And I clear the buffer
    And I turn on pillar-mode

  Scenario: A popup with formats
    When I insert "Some text"
    And I press "C-c C-f"
    Then I should see "Monospace"
    And I should see "Italic"
    And I should see "Superscript"
    And I should see "Bold"
