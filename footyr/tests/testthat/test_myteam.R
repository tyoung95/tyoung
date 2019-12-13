context("myteam error test")

test_that("entering wrong arguments gives failure", {
  expect_error(myteam("English Premier League"), "Please check your leagueID, it must be of the form: Numeric")
  expect_error(myteam(1639, "FC Barcelona"), "Your team is not in this league. Input the correct leagueID / team_name")
  expect_error(myteam(1639, "Arsena"), "You may have mispelt your team name. Please try again with the correct team name")
  expect_error(myteam(1639, "Arsenal", 123), "Your input for the past_results argument should either yes or no only, please change your response.")
  expect_error(myteam(1639, "Arsenal", "test"), "Your input for the past_results argument should either yes or no only, please change your response.")
  expect_error(myteam(1639, "Arsenal", "yes", "five"), "The input of the future argument is wrong, it must be of the form: Numeric")
})
