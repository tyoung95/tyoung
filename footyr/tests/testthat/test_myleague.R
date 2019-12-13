context("myleague error test")

test_that("entering wrong arguments gives failure", {
  expect_error(myleague("English Premier League"), "Please check your leagueID, it must be of the form: Numeric")
  expect_error(myleague(1639, 1234), "Your input for the table argument should either yes or no only, please change your response.")
  expect_error(myleague(1639, "test"), "Your input for the table argument should either yes or no only, please change your response.")
})







