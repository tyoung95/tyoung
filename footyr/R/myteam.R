#' Get detailed information on football team of your choice
#'
#' This function that allows you to retrieve key statistics on
#' a football team of your choice, including past results and
#' future fixtures.
#'
#' This function pulls key statistics about a team, including
#' founding date, manager name, stadium name and capacity and logo. There
#' are two additional, optional arguments that will allow you to
#' toggle if you would like to view past and/or future fixtures.
#' You would be able to view the past 6 results from matches
#' your team played, and you can also specify the number of
#' future fixtures that you would like to view. In particular, the
#' team_name argument in this function will match the name
#' with the team ID that will be then used to query the API, thus
#' saving the user a step from looking up the ID for their desired
#' team.
#'
#' @param leagueID The ID number associated with the league/cup as
#'   defined by the API. This number can be obtained using the ID()
#'   function also in this package. Required, no default value.
#' @param team_name (string) A parameter where you input the name
#'   of the team that you want to view. This will output a dataframe
#'   containing data about their chosen football club. Also prints
#'   an image of the logo of the club. Required, no
#'   default value for this argument.
#' @param past_results (string) A binary parameter taking values "yes" or "no".
#'   Will display the past 6 match results that your team had played
#'   if "yes" is chosen. Results are displayed in the last six columns
#'   of the team data frame. Optional. Defaults to "no".
#' @param future An argument taking double-precision values. This argument
#'   specifies the number of future fixtures that you would like to view.
#'   Eg. 5 will show you the 5 upcoming matches for your team, for both home
#'   and away games. Optional argument, defaults to NA.
#' @importFrom magrittr %>%
#' @importFrom utils head
#' @importFrom magick image_read
#' @keywords league, results, fixtures, team
#' @return A dataframe with team information and past and future match results/fixtures.
#' @examples
#' myteam(1639, "Arsenal", "yes", 5)
#'
#' myteam(1134, "Real Madrid", "no", 2)
#' @export

myteam <- function(leagueID, team_name, past_results = "no", future = NA) {
  base_url <- "http://api.isportsapi.com/sport/football/"
  call_url_team <- stringr::str_c(base_url, "team?", "leagueId=", leagueID, "&api_key=", Sys.getenv("ISPORT_KEY"))

  # Checking if the leagueID is entered as a numeric
  if (is.numeric(leagueID) == FALSE) {
    stop("Please check your leagueID, it must be of the form: Numeric")
  }

  #Retrieving team and league info
  team_query <- httr::GET(call_url_team)
  league_team_df <- as.data.frame(jsonlite::fromJSON(jsonlite::toJSON(httr::content(team_query)))) %>%
    tidyr::unnest(cols = c(data.teamId, data.leagueId, data.name, data.logo, data.foundingDate,
                           data.address, data.area, data.venue, data.capacity, data.coach,
                           data.website))

  # Checking the query status
  if (team_query$status_code != 200) {
    stop ("Error when calling API. Please check your function arguments.")
  }

  #Checking if team_name is in the correct league
  team <- stringr::str_detect(league_team_df$data.name, team_name)

  if (any(team) == TRUE) {
    league_team_df <- league_team_df %>%
      dplyr::filter(data.name == team_name)
    team_id <- league_team_df[,3]
    if (nrow(team_id) == 0) {
      stop("You may have mispelt your team name. Please try again with the correct team name")
    }
  } else {
    stop("Your team is not in this league. Input the correct leagueID / team_name")
  }


  call_url_teamdata <- stringr::str_c(base_url, "team?", "leagueId=", leagueID, "&teamId=",
                                      team_id, "&api_key=", Sys.getenv("ISPORT_KEY"))
  message("Calling ", call_url_teamdata)

  #Retrieving data about team
  team_query1 <- httr::GET(call_url_teamdata)
  team_df1 <- as.data.frame(jsonlite::fromJSON(jsonlite::toJSON(httr::content(team_query1)))) %>%
    tidyr::unnest(cols = c(data.teamId, data.leagueId, data.name, data.logo,
                           data.foundingDate, data.address, data.area, data.venue,
                           data.capacity, data.coach, data.website)) %>%
    dplyr::select(TeamID = data.teamId, LeagueID = data.leagueId, Club_Name = data.name,
                  Founding_Date = data.foundingDate, Area = data.area, Stadium = data.venue,
                  Stadium_Capacity = data.capacity, Manager = data.coach,
                  Website = data.website, Logo = data.logo)

  # Checking the query status
  if (team_query1$status_code != 200) {
    stop ("Error when calling API. Please check your function arguments.")
  }

  if (past_results == "yes") {
    call_url_results <- stringr::str_c(base_url, "standing/league?leagueId=",
                                       leagueID, "&api_key=", Sys.getenv("ISPORT_KEY"))

    #Retrieving data on past results
    past_query <- httr::GET(call_url_results)
    past_list <- jsonlite::fromJSON(jsonlite::toJSON(httr::content(past_query)))
    past_df <- as.data.frame(past_list$data[4]) %>%
      tidyr::unnest(cols = c(totalStandings.rank, totalStandings.teamId, totalStandings.winRate,
                             totalStandings.drawRate, totalStandings.loseRate, totalStandings.winAverage,
                             totalStandings.loseAverage, totalStandings.deduction,
                             totalStandings.deductionExplain, totalStandings.recentFirstResult, totalStandings.recentSecondResult,
                             totalStandings.recentThirdResult, totalStandings.recentFourthResult,
                             totalStandings.recentFifthResult, totalStandings.recentSixthResult,
                             totalStandings.color, totalStandings.red, totalStandings.totalCount,
                             totalStandings.winCount, totalStandings.drawCount, totalStandings.loseCount,
                             totalStandings.getScore, totalStandings.loseScore, totalStandings.goalDifference,
                             totalStandings.integral))

    # Checking the query status
    if (past_query$status_code != 200) {
      stop ("Error when calling API. Response: Please check your function arguments.")
    }

    past_df <- past_df %>%
      dplyr::select(TeamID = totalStandings.teamId, "1" = totalStandings.recentFirstResult,
                    "2" = totalStandings.recentSecondResult, "3" = totalStandings.recentThirdResult,
                    "4" = totalStandings.recentFourthResult, "5" = totalStandings.recentFifthResult,
                    "6" = totalStandings.recentSixthResult)

    past_df[2:7] <- past_df[2:7] %>%
      lapply(function(x) factor(x, levels = c(0,1,2,3), labels = c("W", "D", "L", NA)))

    team_df2 <- dplyr::left_join(team_df1, past_df, by = "TeamID")

    team_df2_final <- dplyr::select(team_df2, -Logo)

    print(team_df2_final)

    photo_url_check <- stringr::str_detect(team_df2$Logo, "^http")
    if (photo_url_check == TRUE) {
      print(magick::image_read(team_df2$Logo))
    }

  } else if (past_results == "no") {

    team_df1_final <- dplyr::select(team_df1, -Logo)

    print(team_df1_final)

    photo_url_check <- stringr::str_detect(team_df1$Logo, "^http")
    if (photo_url_check == TRUE) {
      print(magick::image_read(team_df1$Logo))
    }

  } else {
    stop("Your input for the past_results argument should either yes or no only, please change your response.")
  }

  check <- is.numeric(future)
  if (check ==  TRUE) {
    call_url_future <- stringr::str_c(base_url, "schedule?leagueId=", leagueID, "&api_key=", Sys.getenv("ISPORT_KEY"))
    message("Calling ", call_url_future)

    # Retrieving data on future fixtures
    future_query <- httr::GET(call_url_future)
    future_list <- jsonlite::fromJSON(jsonlite::toJSON(httr::content(future_query)))

    # Checking the query status
    if (future_query$status_code != 200) {
      stop ("Error when calling API. Response: Please check your function arguments.")
    }

    future_df <- as.data.frame(future_list) %>%
      jsonlite::flatten() %>%
      tidyr::unnest(cols = c(data.matchId, data.leagueType, data.leagueId, data.leagueName,
                             data.leagueShortName, data.subLeagueId, data.subLeagueName,
                             data.matchTime, data.halfStartTime, data.status, data.homeId,
                             data.homeName, data.awayId, data.awayName, data.homeScore,
                             data.awayScore, data.homeHalfScore, data.awayHalfScore, data.homeRed,
                             data.awayRed, data.homeYellow, data.awayYellow, data.homeCorner,
                             data.awayCorner, data.homeRank, data.awayRank, data.season,
                             data.round, data.group, data.location, data.weather, data.temperature,
                             data.explain, data.hasLineup, data.neutral, data.extraExplain.kickOff,
                             data.extraExplain.minute, data.extraExplain.homeScore, data.extraExplain.awayScore,
                             data.extraExplain.extraTimeStatus, data.extraExplain.extraHomeScore,
                             data.extraExplain.extraAwayScore, data.extraExplain.penHomeScore,
                             data.extraExplain.penAwayScore, data.extraExplain.twoRoundsHomeScore,
                             data.extraExplain.twoRoundsAwayScore, data.extraExplain.winner))

    future_df1 <- future_df %>%
      dplyr::filter(data.status != -1) %>%
      dplyr::filter(data.homeName == team_name | data.awayName == team_name) %>%
      dplyr::select(MatchID = data.matchId, League_Name = data.leagueName, Home = data.homeName,
                    Away = data.awayName, Location = data.location)

    print(head(future_df1, future))

  } else if (check == FALSE) {
    if (is.na(future) == FALSE) {
      stop("The input of the future argument is wrong, it must be of the form: Numeric")
    }
  }

}









