#' @title Get the Basic Information of Football Teams from England
#'
#' @description The function gets the basic information of a football team from England.
#' The data is got from https://m.qtx.com/dbsoccer/stading_92_2018.html using web sraping.
#' The baisc information includes country, home, city and fundation time.
#'
#' @param teams the team names of given teams
#' @param all If all is true, it will return the basic information of all.The default value
#' of all is FALSE.
#' @return the basic information of football teams,
#' including country, home, city and fundation time.
#'
#' @examples
#' # Example 1: Get the basic information of Liverpool, Chelsea
#' info_sample <- read_html_team(teams = c("Liverpool", "Chelsea"))
#' print(info_sample)
#' # Example 2: Get the basic information of all football teams from England
#' info_all <- read_html_team(all = TRUE)
#' print(info_all)
#'
#' @export read_html_team

read_html_team <- function(teams, all = FALSE) {
  url <- 'https://m.qtx.com/dbsoccer/stading_92_2018.html'
  webpage <- xml2::read_html(url)
  team_url <- rvest::html_attr(rvest::html_nodes(webpage,'td>a'), 'href')
  appKey <- "77eb81e8bb09b8de"
  salt <- 2
  se <- '3HugxZSRBL8itkrq96Dy9nWKdnm3Orfs'
  team_list <- data.frame()
  for (i in 1:length(team_url)){
    temp = xml2::read_html(team_url[i])
    team_name0 = rvest::html_nodes(temp, 'div>div.tit1')
    team_name <- stringr::str_match(rvest::html_text(team_name0)[4],
                                    pattern = "[\u4e00-\u9fa5]+")
    sign = paste(appKey, team_name, salt, se, sep = '')
    team_name_md5 = openssl::md5(sign)
    team_name_trans = RCurl::getURL(
      paste0(
        'http://openapi.youdao.com/api?q=', team_name,
        '&from=auto&to=auto&appKey=77eb81e8bb09b8de&salt=2&sign=',
        team_name_md5))
    team_name_ENG = rjson::fromJSON(team_name_trans)$translation
    info = rvest::html_nodes(temp, 'div>div.con1')
    info = rvest::html_text(info)
    country <- stringr::str_match(info, pattern = "国家.(.+)\\n")[1, 2]
    sign = paste(appKey, country, salt, se, sep = '')
    country_md5 = openssl::md5(sign)
    country_trans = RCurl::getURL(
      paste0(
        'http://openapi.youdao.com/api?q=',country,
        '&from=auto&to=auto&appKey=77eb81e8bb09b8de&salt=2&sign=',
        country_md5))
    country_ENG = rjson::fromJSON(country_trans)$translation
    home = stringr::str_match(info, pattern = "主场.(.+)")[1, 2]
    sign = paste(appKey, home, salt, se, sep = '')
    home_md5 = openssl::md5(sign)
    home_trans = RCurl::getURL(
      paste0(
        'http://openapi.youdao.com/api?q=', home,
        '&from=auto&to=auto&appKey=77eb81e8bb09b8de&salt=2&sign=',
        home_md5))
    home_ENG = rjson::fromJSON(home_trans)$translation
    city = stringr::str_match(info, pattern = "城市.(.+)")[1, 2]
    sign = paste(appKey, city, salt, se, sep = '')
    city_md5 = openssl::md5(sign)
    city_trans = RCurl::getURL(paste0(
      'http://openapi.youdao.com/api?q=',
      city, '&from=auto&to=auto&appKey=77eb81e8bb09b8de&salt=2&sign=',
      city_md5))
    city_ENG = rjson::fromJSON(city_trans)$translation
    fund_time = stringr::str_match(info, pattern = "成立时间.(.+)")[1, 2]
    team_info = data.frame(team_name_ENG, country_ENG, home_ENG, city_ENG, fund_time)
    team_list = rbind(team_list, team_info)
  }
  rownames(team_list) <- team_list$team_name_ENG
  if (all == TRUE) {
    return(team_list)
  } else {
    return(team_list[teams, ])
  }
}
