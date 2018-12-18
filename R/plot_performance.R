#' @title Graph the Team Performance of Different Teams.
#'
#' @description This function plots the team performance of different teams from England.
#'
#' @param data the data set of merged data
#' @param teams the team names of given teams
#' @param perfrm_vars the performance variables that want to compare
#' @param all If all is true, it will give the performance comparison of all teams.
#' The default value of all is FALSE.
#' @return NULL
#'
#' @examples
#' # Example: Graph the Performance of Different Teams
#'
#' teams <- c("Liverpool", "Chelsea", "Arsenal", "Chelsea", "everton", "fulham")
#' plot_performance(football.teams, teams, perfrm_vars = c("points", "won", "loss"))
#'
#' @importFrom graphics axis legend lines plot
#' @importFrom stats na.omit
#'
#' @export plot_performance

plot_performance <- function(data, perfrm_vars, teams, all = FALSE) {
  data <- na.omit(data)
  if (all == FALSE) {
    rownames(data) <- data$team_name_ENG
    data <- data[teams, ]
  }
  colbars <- c("blue", "red", "black", "darkreen", "yellow")
  yrange <- range(data[, perfrm_vars])
  num <- nrow(data)
  n <- length(perfrm_vars)
  if (n == 1) {
    plot(1:num, data[, perfrm_vars], type = "l",
         col = colbars[1], ylim = yrange,
         xlab = "Team Names", ylab = "Performance",
         main = "Performance of Different Teams")
    axis(side = 1, at = 1:num, labels = as.character(data$team_name_ENG))
    legend("topright", legend = perfrm_vars)
  } else {
    plot(1:num, data[, perfrm_vars[1]], type = "l",
         col = colbars[1], ylim = yrange, xaxt = "n",
         xlab = "Team Names", ylab = "Performance",
         main = "Performance of Different Teams")
    axis(side = 1, at = 1:num, labels = as.character(data$team_name_ENG))
    for (i in 2:n) {
      lines(1:num, data[, perfrm_vars[i]], type = "l", col = colbars[i])
    }
    legend("topright", legend = perfrm_vars, lty = 1, col = colbars[1:n])
  }
}

