context("ID error test")

test_that("entering wrong arguments gives failure", {
  expect_error(ID("English Premier Leageu"), "You may have mispelt your league/cup name. Please try again with the correct league/cup name")
  expect_error(ID("Englsh Premie League"), "You may have mispelt your league/cup name. Please try again with the correct league/cup name")
  expect_error(ID(1234), "Your input for league_name needs to be a string. Please input a string.")
  expect_error(ID("English Premier League", 1234), "Your input for team_name needs to be a string. Please input a string.")
  expect_error(ID("English Premier League", "Arsen"), "You may have mispelt your team name. Please try again with the correct team name")
})




