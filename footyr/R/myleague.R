#' Get football league data and standings tables
#'
#' This function that allows you to retrieve key information on
#' football leagues/cups, including the league standing tables.
#'
#' This function pulls key statistics about a league/cup, including
#' current round, total rounds, current season. There is an optional argument
#' that will allow you to see the current league table that displays
#' the standings of all the current football teams in the league. The
#' table contains stats like number of wins, losses, draws, etc., ranking
#' the teams in terms of number of points gained in the league.
#'
#' @param leagueID The ID number associated with the league/cup as
#'   defined by the API. This number can be obtained using the ID()
#'   function also in this package. Required, no default value.
#' @param table (string) A binary parameter taking values "yes" or "no".
#'   Will display the current league standings table if "yes" is chosen.
#'   Optional. Defaults to "no".
#' @importFrom magrittr %>%
#' @keywords table, league, team
#' @return A dataframe with league/cup information and league table (optional).
#' @examples
#' myleague(1639, "yes")
#'
#' myleague(188, "no")
#' @export

myleague <- function(leagueID, table = "no") {
  base_url <- "http://api.isportsapi.com/sport/football/standing/league?leagueId="
  call_url <- stringr::str_c(base_url, leagueID, "&api_key=", Sys.getenv("ISPORT_KEY"))
  message("Calling http://api.isportsapi.com/sport/football/standing/league?leagueId=", leagueID, "&api_key=YOUR_API_KEY")

  #Retrieving league data
  req <- httr::GET(call_url)
  message(httr::http_status(req))

  if (req$status_code != 200) {
    stop ("Error when calling API. Response: ", httr::content(req))
  }

  league_list <- jsonlite::fromJSON(jsonlite::toJSON(httr::content(req)))

  #Parsing data on league table
  if (table == "yes") {
    league_df <- as.data.frame(league_list$data[1]) %>%
      tidyr::unnest(cols = c())

    league_df_final <- league_df %>%
      dplyr::select(LeagueID = leagueInfo.leagueId, Name = leagueInfo.name,
                    Abbreviation = leagueInfo.shortName,
                    Total_Rounds = leagueInfo.totalRound, Current_Round = leagueInfo.currentRound,
                    Current_Season = leagueInfo.currentSeason)

    team_info_df <- as.data.frame(league_list$data[3])
    team_info_df <- team_info_df %>%
      tidyr::unnest(cols = c(teamInfos.teamId, teamInfos.name, teamInfos.logo, teamInfos.area)) %>%
      dplyr::select(totalStandings.teamId = teamInfos.teamId, teamInfos.name)

    standings_df <- as.data.frame(league_list$data[4]) %>%
      tidyr::unnest(cols = c(totalStandings.rank, totalStandings.teamId, totalStandings.winRate,
                             totalStandings.drawRate, totalStandings.loseRate, totalStandings.winAverage,
                             totalStandings.loseAverage, totalStandings.deduction,
                             totalStandings.deductionExplain, totalStandings.recentFirstResult,
                             totalStandings.recentSecondResult,
                             totalStandings.recentThirdResult, totalStandings.recentFourthResult,
                             totalStandings.recentFifthResult, totalStandings.recentSixthResult,
                             totalStandings.color, totalStandings.red, totalStandings.totalCount,
                             totalStandings.winCount, totalStandings.drawCount, totalStandings.loseCount,
                             totalStandings.getScore, totalStandings.loseScore, totalStandings.goalDifference,
                             totalStandings.integral))

    league_table <- dplyr::left_join(standings_df, team_info_df, by = "totalStandings.teamId")

    league_table <- league_table %>%
      dplyr::select(Rank = totalStandings.rank, Club = teamInfos.name, Matches_Played = totalStandings.totalCount,
                    Points = totalStandings.integral, Wins = totalStandings.winCount, Losses = totalStandings.loseCount,
                    Draws = totalStandings.drawCount, Goals_For = totalStandings.getScore,
                    Goals_Against =totalStandings.loseScore, Goal_Diff = totalStandings.goalDifference)


    print(league_df_final)
    print(league_table)


  } else if (table == "no") {
    league_df <- as.data.frame(league_list$data[1]) %>%
      jsonlite::flatten() %>%
      tidyr::unnest(cols = c())

    league_df_final <- league_df %>%
      dplyr::select(LeagueID = leagueInfo.leagueId, Name = leagueInfo.name, Abbreviation = leagueInfo.shortName,
                    Total_Rounds = leagueInfo.totalRound, Current_Round = leagueInfo.currentRound,
                    Current_Season = leagueInfo.currentSeason)

    print(league_df_final)
  }

}

