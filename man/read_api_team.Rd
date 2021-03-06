% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_api_team.R
\name{read_api_team}
\alias{read_api_team}
\title{Get the Team Performance of Football Teams from England}
\usage{
read_api_team(teams, all = FALSE)
}
\arguments{
\item{teams}{the team names of given teams}

\item{all}{If all is true, it will return the basic information of all.
The default value of all is FALSE.}
}
\value{
the performance of football teams
}
\description{
The function returns the team performance of football teams in England using API collection.
The data is from https://api.football-data.org/v2/teams/86/matches?status=SCHEDULED.
Given specific team names, it will return the performance of these teams.
}
\examples{
# Example 1: Get the team performance of Liverpool, Chelsea
perfrm_sample <- read_api_team(teams = c("Liverpool", "Chelsea"))
print(perfrm_sample)

# Example 2: Get the team performance of all football teams from England
perfrm_all <- read_api_team(all = TRUE)
print(perfrm_all)

}
