#' Get squad information of your football team
#'
#' This function that allows you to retrieve squad statistics of
#' a football team of your choice, including individual player and
#' coaching staff statistics.
#'
#' This function pulls key data about a team's squad, including
#' full team list, squad numbers, players' birthdays and more. There
#' is one additional, optional argument that will allow you to
#' conduct a focussed search for an individual player within
#' that squad, which is useful if you know which player in particular
#' that you are searching for.
#'
#' @param teamID The ID number associated with the team as
#'   defined by the API. This number can be obtained using the ID()
#'   function also in this package. Will output squad info
#'   if this is only argument used. Required, no default value.
#' @param player_name (string) A parameter where you input the name
#'   of the player whose data you want to view. This will output
#'   a dataframe containing data about the chosen player only,
#'   instead of displaying the entire squad info. Optional argument,
#'   defaults to NA.
#' @importFrom magrittr %>%
#' @keywords team, squad, player
#' @return A dataframe with team and player information.
#' @examples
#' myplayer(19, "Mesut Ozil")
#'
#' myplayer(25)
#' @export

myplayer <- function(teamID, player_name = NA){
  base_url <- "http://api.isportsapi.com/sport/football/"
  call_url_player <- stringr::str_c(base_url, "player?", "teamId=", teamID,
                                    "&api_key=", Sys.getenv("ISPORT_KEY"))

  #Obtain DF for squad info of team
  team_call <- httr::GET(call_url_player)

  if (team_call$status_code != 200) {
    stop ("Error when calling API. Response: ", content(team_call))
  }

  team_df <- as.data.frame(jsonlite::fromJSON(jsonlite::toJSON(httr::content(team_call)))) %>%
    tidyr::unnest(cols = c(data.recordId, data.playerId, data.name, data.birthday, data.height,
                           data.country, data.feet, data.weight, data.photo, data.value,
                           data.teamId, data.position, data.number, data.introduce,
                           data.contractEndDate)) %>%
    dplyr::select(TeamID = data.teamId, PlayerID = data.playerId, Name = data.name, Birthday = data.birthday,
                  Height = data.height, Country = data.country, Dominant_Foot = data.feet,
                  Weight = data.weight, Value = data.value, Player_Position = data.position,
                  Jersey_Number = data.number, Contract_Expiry = data.contractEndDate)



  #Retrieving info about a particular player
  is_na <- is.na(player_name)

  if (is_na == FALSE) {
    team1 <- stringr::str_detect(team_df$Name, player_name)

    if (any(team1) == TRUE) {
      team_df1 <- team_df %>%
        dplyr::filter(Name == player_name)

      print(team_df1)
    } else if (all(team1) == FALSE){
      stop("Your player is not in this team. Please input the correct player name / team ID.")
    }

  } else if (is_na == TRUE) {
    print(team_df)
  }

}
