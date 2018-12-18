#' @title The Basic Statistics of Football Team's Performance
#'
#' @description This function returns the basic statistics of football team's performance variables
#'
#' @param data the data set of merged data
#' @param teams the team names of given teams
#' @param perfrm_vars the performance variables
#' @param all If all is true, it will give the summary statistics of all teams.
#' The default value of all is FALSE.
#'
#' @return the basic statistics of football team Performance
#'
#' @examples # Example get the summary statistics of points, won, and loss of all teams
#' summary_football(data = football.teams, perfrm_vars = c("points", "won", "loss"), all = TRUE)

#' @export summary_football

summary_football <- function(data, perfrm_vars, teams, all = FALSE) {
  if (all == FALSE) {
    rownames(data) <- data$team_name_ENG
    data <- data[teams, ]
  } else {
    data <- data
  }
  return(summary(football.teams[perfrm_vars]))
}

