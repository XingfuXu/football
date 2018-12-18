#' @title Get the Team Performance of Football Teams from England
#'
#' @description The function returns the team performance of football teams in England using API collection.
#' The data is from https://api.football-data.org/v2/teams/86/matches?status=SCHEDULED.
#' Given specific team names, it will return the performance of these teams.
#'
#' @param teams the team names of given teams
#' @param all If all is true, it will return the basic information of all.
#' The default value of all is FALSE.
#' @return the performance of football teams
#'
#' @examples
#' # Example 1: Get the team performance of Liverpool, Chelsea
#' perfrm_sample <- read_api_team(teams = c("Liverpool", "Chelsea"))
#' print(perfrm_sample)
#'
#' # Example 2: Get the team performance of all football teams from England
#' perfrm_all <- read_api_team(all = TRUE)
#' print(perfrm_all)
#'
#' @export read_api_team

read_api_team <- function(teams, all = FALSE) {
  url = 'https://dc.qiumibao.com/shuju/public/index.php?_url=/data/index&league=%E8%8B%B1%E8%B6%85&tab=%E7%A7%AF%E5%88%86%E6%A6%9C&year=[year]'
  appKey <- "77eb81e8bb09b8de"
  salt <- 2
  se <- '3HugxZSRBL8itkrq96Dy9nWKdnm3Orfs'
  data_json = RCurl::getURL(url)
  team_rank = rjson::fromJSON(data_json)$data
  team_score = data.frame()
  for (i in 1:length(team_rank)) {
    team_rank_1 = team_rank[[i]][1]
    team_name = team_rank[[i]][3]
    sign = paste(appKey, team_name, salt, se, sep = '')
    team_name_md5 = openssl::md5(sign)
    team_name_trans = RCurl::getURL(
      paste0(
        'http://openapi.youdao.com/api?q=',team_name,
        '&from=auto&to=auto&appKey=77eb81e8bb09b8de&salt=2&sign=',
        team_name_md5))
    team_name_ENG = rjson::fromJSON(team_name_trans)$translation
    team_match = as.numeric(team_rank[[i]][4])
    team_win = as.numeric(team_rank[[i]][5])
    team_loss = as.numeric(team_rank[[i]][7])
    team_draw = as.numeric(team_rank[[i]][6])
    team_total_score = as.numeric(team_rank[[i]][10])
    team_win = data.frame(team_name_ENG, team_total_score,
                          team_match, team_win, team_loss, team_draw)
    team_score = rbind(team_score, team_win)
  }
  colnames(team_score)[2:6] <- c("points", "played", "won", "loss", "drawn")
  rownames(team_score) <- team_score$team_name_ENG
  if (all == TRUE) {
    return(team_score)
  } else {
    return(team_score[teams, ])
  }
}
