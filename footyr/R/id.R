#' Get ID numbers for football leagues and teams.
#'
#' This is a basic function that allows you to retrieve the ID numbers of
#' leagues/cups and teams.
#'
#' This function serves as the basis for the rest of the functions in the
#' package. Upon input of strings for league and team names, the function outputs
#' the ID numbers of the league and teams, that you will need to use in other
#' functions in this package, as the API uses these ID numbers for queries.
#'
#' @param league_name (string) The name of the league/cup/competition that you
#'   are looking to retrieve the ID number for. Required. No default.
#' @param team_name (string) The name of the team in the league that you
#'   want to pull the team ID number for. Optional. Defaults to NA.
#' @importFrom magrittr %>%
#' @keywords football, ID, league, team
#' @return A dataframe with league and team (optional) ID numbers.
#' @examples
#' ID("English Premier League", "Arsenal")
#'
#' ID("Spanish La Liga", "FC Barcelona")
#' @export

ID <- function(league_name, team_name = NA) {
  leagues_url <- stringr::str_c("http://api.isportsapi.com/sport/football/league/basic?api_key=", Sys.getenv("ISPORT_KEY"))

  if (is.character(league_name) == FALSE) {
    stop ("Your input for league_name needs to be a string. Please input a string.")
  }

  #Retrieving data on leagues via league_name
  leagues <- httr::GET(leagues_url)

  # Checking the query status
  if (leagues$status_code != 200) {
    stop ("Error when calling API. Please check your function arguments.")
  }

  leagues1 <- jsonlite::fromJSON(jsonlite::toJSON(httr::content(leagues)))
  leagues_df <- as.data.frame(leagues1) %>%
    jsonlite::flatten() %>%
    tidyr::unnest(cols = c(data.leagueId, data.name, data.shortName, data.type, data.subLeagueName))

  leagues_df <- leagues_df %>%
    dplyr::select(LeagueID = data.leagueId, League_Name = data.name,
                  League_Shortform = data.shortName, Sub_League_Name = data.subLeagueName)

  league_detect <- stringr::str_detect(leagues_df$League_Name, league_name)

  # Checking the league_name is spelt correctly
  if (any(league_detect) == TRUE) {
    leagues_df <- leagues_df %>%
      dplyr::filter(League_Name == league_name)
    league_info <- leagues_df[,1:2]
    if (nrow(league_info) == 0) {
      stop("You may have mispelt your league/cup name. Please try again with the correct league/cup name")
    } else {
      print(league_info)
    }

  } else {
    stop("You may have mispelt your league/cup name. Please try again with the correct league/cup name")
  }



  if (is.na(team_name) == FALSE) {
    if (is.character(team_name) == FALSE) {
      stop ("Your input for team_name needs to be a string. Please input a string.")
    }

    league_id2 <- league_info[,1]
    leagues_url2 <- stringr::str_c("http://api.isportsapi.com/sport/football/team?leagueId=",
                                  league_id2, "&api_key=", Sys.getenv("ISPORT_KEY"))

    #Retrieving data on teams in league
    leagues2_query <- httr::GET(leagues_url2)
    leagues2 <- jsonlite::fromJSON(jsonlite::toJSON(httr::content(leagues2_query)))
    leagues2_df <- as.data.frame(leagues2) %>%
      jsonlite::flatten() %>%
      tidyr::unnest(cols = c(data.teamId, data.leagueId, data.name, data.logo, data.foundingDate,
                            data.address, data.area, data.venue, data.capacity, data.coach,
                            data.website))
    leagues2_df <- leagues2_df %>%
      dplyr::select(TeamID = data.teamId, Team_Name = data.name)


    team_detect <- stringr::str_detect(leagues2_df$Team_Name, team_name)

    # Checking the team_name is spelt correctly
    if (any(team_detect) == TRUE) {
      leagues2_df <- leagues2_df %>%
        dplyr::filter(Team_Name == team_name)

      team_info <- leagues2_df[, 1:2]
      if (nrow(team_info) == 0) {
        stop("You may have mispelt your team name. Please try again with the correct team name")
      } else {
        print(leagues2_df)
      }

    } else {
      stop("You may have mispelt your team name OR Your team does not play in this league. Please try again with the correct team name")
   }
  }
}

