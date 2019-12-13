context("myplayer error test")

test_that("entering wrong arguments gives failure", {
  expect_error(myplayer("Arsenal"), "Please check your teamID, it must be of the form: Numeric")
  expect_error(myplayer(19, 1234), "Please check your player_name, it must be of the form: Character")
  expect_error(myplayer(19, "Neymar"), "Your player is not in this team. Please input the correct player name / team ID.")
})

