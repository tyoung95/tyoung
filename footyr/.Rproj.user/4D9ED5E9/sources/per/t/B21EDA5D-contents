#' Get data and statistics of leagues' best players
#'
#' This function that allows you to retrieve complete statistics of
#' the top players within a league, including additional stats not
#' natively from the ISports API.
#'
#' This function pulls key statistics about the top players within
#' the league. This could include top goalscorers, most assists, most
#' shots, etc. The user simply specifies the league using the leagueID
#' as the argument, followed by the statistic/key variable that
#' they would like to focus on, and input this as the second argument
#' in the function. The user could also choose to view additional
#' statistics if they so desire by using the third binary argument.
#'
#' @param leagueID The ID number associated with the league/cup as
#'   defined by the API. This number can be obtained using the ID()
#'   function also in this package. Argument required, no default value.
#' @param stat (string) A parameter where you input the stat
#'   that you would like to predominantly focus on. The chosen stat
#'   will result in the dataframe output to be ordered based
#'   on that stat. Available stats would include: "Goals",
#'   "Shots", "Shots_on_Target", "Assists", "Key_Passes". This
#'   argument is required, with no default.
#' @param more_details (string) A binary parameter taking
#'   values "yes" or "no". If argument input is "yes",
#'   additional stats would be included in the dataframe output,
#'   including shot accuracy, mins per goal, etc. Argument is
#'   optional, with default set to "no".
#' @importFrom magrittr %>%
#' @importFrom utils head
#' @keywords top, stats, league, goals, assists
#' @return A dataframe with players with the top statistics in the league.
#' @examples
#' mytopstats(1639, "Goals", "yes")
#'
#' mytopstats(188, "Assists")
#' @export

mytopstats <- function (leagueID, stat, more_details = "no") {
  base_url <- "http://api.isportsapi.com/sport/football/"
  call_url_top <- stringr::str_c(base_url, "topscorer?leagueId=",
                                 leagueID, "&api_key=", Sys.getenv("ISPORT_KEY"))

  #Obtain DF for top scorers in league
  top_call <- httr::GET(call_url_top)

  if (top_call$status_code != 200) {
    stop ("Error when calling API. Response: ", content(top_call))
  }

  top_df <- as.data.frame(jsonlite::fromJSON(jsonlite::toJSON(httr::content(top_call)))) %>%
    tidyr::unnest(cols = c(data.playerId, data.playerName, data.teamId, data.teamName,
                           data.country, data.goalsCount, data.homeGoals, data.awayGoals,
                           data.homePenalty, data.awayPenalty)) %>%
    dplyr::select(PlayerID = data.playerId, Player_Name = data.playerName, TeamID = data.teamId,
                  Team_Name = data.teamName,Country = data.country, Total_Goals = data.goalsCount)



  #Obtain more stats for players in league
  call_url_stats <- stringr::str_c(base_url, "playerstats/league?leagueId=",
                                   leagueID, "&api_key=", Sys.getenv("ISPORT_KEY"))

  stats_call <- httr::GET(call_url_stats)

  if (stats_call$status_code != 200) {
    stop ("Error when calling API. Response: ", content(stats_call))
  }

  stats_df <- as.data.frame(jsonlite::fromJSON(jsonlite::toJSON(httr::content(stats_call)))) %>%
    tidyr::unnest(cols = c(data.playerId, data.teamId, data.leagueId, data.season, data.appearanceCount,
                           data.substituteCount, data.playingTime, data.goals, data.penaltyGoals,
                           data.shotCount, data.shotTargetCount, data.offsideCount,
                           data.bestCount, data.rating, data.passCount, data.passSuccessCount,
                           data.keyPassCount, data.assistCount, data.longPassCount,
                           data.longPassSuccessCount, data.throughPassCount, data.throughPassSuccessCount,
                           data.dribblesSuccessCount, data.crossPassCount, data.crossPassSuccessCount,
                           data.tackleCount, data.interceptionCount, data.clearanceCount,
                           data.dispossessedCount, data.shotBlockedCount, data.aerialSuccessCount,
                           data.foulsCount, data.redCount, data.yellowCount, data.turnOverCount,
                           data.modifyTime, data.homeTeam)) %>%
    dplyr::select(PlayerID = data.playerId, Season = data.season, Num_App = data.appearanceCount,
                  Minutes = data.playingTime, Shots = data.shotCount, Shots_on_Target = data.shotTargetCount,
                  Assists = data.assistCount, Key_Passes = data.keyPassCount, data.homeTeam)

  full_df <- dplyr::left_join(top_df, stats_df, by = "PlayerID")

  away_df <- dplyr::filter(full_df, data.homeTeam == FALSE) %>%
    dplyr::select(PlayerID, Season, A.Num_App = Num_App,
                  A.Minutes = Minutes, A.Shots = Shots, A.Shots_on_Target = Shots_on_Target,
                  A.Assists = Assists, A.Key_Passes = Key_Passes, data.homeTeam)

  home_df <- dplyr::filter(full_df, data.homeTeam == TRUE)

  total_df <- dplyr::inner_join(home_df, away_df, by = "PlayerID")

  total_df$Total_Apps <- total_df$Num_App + total_df$A.Num_App
  total_df$Total_Mins <- total_df$Minutes + total_df$A.Minutes
  total_df$Total_Shots <- total_df$Shots + total_df$A.Shots
  total_df$Total_Shots_Target <- total_df$Shots_on_Target + total_df$A.Shots_on_Target
  total_df$Total_Assists <- total_df$Assists + total_df$A.Assists
  total_df$Total_Key_Passes <- total_df$Key_Passes + total_df$A.Key_Passes

  total_df <- total_df %>%
    dplyr::select(PlayerID, Player_Name, Team_Name, Season = Season.x, Total_Goals, Total_Apps,
                  Total_Mins, Total_Shots, Total_Shots_Target, Total_Assists, Total_Key_Passes)

  if (more_details == "yes") {
    total_df$Mins_Goal <- total_df$Total_Mins / total_df$Total_Goals
    total_df$Goals_App <- total_df$Total_Goals / total_df$Total_Apps
    total_df$Mins_Assist <- total_df$Total_Mins / total_df$Total_Assists
    total_df$Goal_Conversion_Rate <- total_df$Total_Goals / total_df$Total_Shots
    total_df$Shot_Accuracy <- total_df$Total_Shots_Target / total_df$Total_Shots

    if (stat == "Goals") {
      goal <- total_df %>%
        dplyr::arrange(dplyr::desc(Total_Goals))
      print(head(goal, 10))
    } else if (stat == "Shots") {
      shots <- total_df %>%
        dplyr::arrange(dplyr::desc(Total_Shots))
      print(head(shots, 10))
    } else if (stat == "Shots_on_Target") {
      shots_target <- total_df %>%
        dplyr::arrange(dplyr::desc(Total_Shots_Target))
      print(head(shots_target, 10))
    } else if (stat == "Assists") {
      assists <- total_df %>%
        dplyr::arrange(dplyr::desc(Total_Assists))
      print(head(assists, 10))
    } else if (stat == "Key_Passes") {
      key_pass <- total_df %>%
        dplyr::arrange(dplyr::desc(Total_Key_Passes))
      print(head(key_pass, 10))
    } else if (stat != c("Goals", "Shots", "Shots_on_Target", "Assists", "Key_Passes")) {
      stop("You may have mistyped your query or it is not the list of available queries. Please try again.")
    }



  } else if (more_details == "no"){

    if (stat == "Goals") {
      goal <- total_df %>%
        dplyr::arrange(dplyr::desc(Total_Goals))
      print(head(goal, 10))
    } else if (stat == "Shots") {
      shots <- total_df %>%
        dplyr::arrange(dplyr::desc(Total_Shots))
      print(head(shots, 10))
    } else if (stat == "Shots_on_Target") {
      shots_target <- total_df %>%
        dplyr::arrange(dplyr::desc(Total_Shots_Target))
      print(head(shots_target, 10))
    } else if (stat == "Assists") {
      assists <- total_df %>%
        dplyr::arrange(dplyr::desc(Total_Assists))
      print(head(assists, 10))
    } else if (stat == "Key_Passes") {
      key_pass <- total_df %>%
        dplyr::arrange(dplyr::desc(Total_Key_Passes))
      print(head(key_pass, 10))
    } else if (stat != c("Goals", "Shots", "Shots_on_Target", "Assists", "Key_Passes")) {
      stop("You may have mistyped your query or it is not the list of available queries. Please try again.")
    }
  }
}
