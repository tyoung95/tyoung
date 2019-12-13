context("mytopstats error test")

test_that("entering wrong arguments gives failure", {
  expect_error(mytopstats("English Premier League"), "Please check your leagueID, it must be of the form: Numeric")
  expect_error(mytopstats(1639, "Goal"), "You may have mistyped your query or it is not the list of available queries. Please try again.")
  expect_error(mytopstats(1639, 1234), "You may have mistyped your query or it is not the list of available queries. Please try again.")
  expect_error(mytopstats(1639, "Goals", "test"), "Your input for the past_results argument should either yes or no only, please change your response.")
})
