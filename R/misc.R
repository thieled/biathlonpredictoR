# Eliminating 'no visible binding' note
utils::globalVariables(c("Behind",
                         "Comp",
                         "Comp.DisciplineId",
                         "Comp.StartTime",
                         "Comp.catId",
                         "Comp.km",
                         "Event.EventId",
                         "Event.OrganizerId",
                         "Event.SeasonId",
                         "IBUId",
                         "IsResult",
                         "Level",
                         "Nat",
                         "PursuitStartDistance",
                         "RaceId",
                         "Rank",
                         "ResultOrder",
                         "Season",
                         "ShootingTotal",
                         "Shootings",
                         "ShortName",
                         "StartOrder",
                         "TotalTime",
                         "age",
                         "bday",
                         "behind",
                         "behind_s",
                         "coef",
                         "count_helper",
                         "data",
                         "disc",
                         "disc_longterm_performance",
                         "disc_longterm_rank",
                         "disc_longterm_shooting",
                         "disc_shortterm_performance",
                         "disc_shortterm_rank",
                         "disc_shortterm_shooting",
                         "disc_slope_rank",
                         "disc_slope_time",
                         "is_result",
                         "lag_behind_s",
                         "lag_race_date",
                         "lag_rank",
                         "lag_shooting_errors",
                         "longterm_performance",
                         "longterm_rank",
                         "longterm_shooting",
                         "pu_starting_lag",
                         "race_date",
                         "race_start",
                         "races_count",
                         "result_order",
                         "season",
                         "shooting_errors",
                         "short_name",
                         "shortterm_performance",
                         "shortterm_rank",
                         "shortterm_shooting",
                         "slope_rank",
                         "slope_time",
                         "starts_with",
                         "time_last_race"))





#' Returns the season of a given date.
#'
#' @param date A date in character, numeric, or date format.
#' @return A character string representing the season in the format of 'YYYY',
#' where the first two digits represent the starting year and the last two digits
#' represent the ending year of the season.
#' @examples
#' get_season("2021-06-01")
#' @export
get_season <- function(date) {
  date <- lubridate::as_date(date)
  y <- ifelse(lubridate::quarter(date) == 4, lubridate::year(date),
    lubridate::year(date) - 1
  ) %>%
    stringr::str_sub(3L) %>%
    as.numeric()
  season <- paste0(sprintf("%02d", y), sprintf("%02d", y + 1))
  return(season)
}


#' Returns the previous seasons of a given date.
#'
#' @param date A date in character, numeric, or date format.
#' @param minus_years Number of years to go back from the input date (default is 1)
#' @return A character string representing the season in the format of 'YYYY',
#'  where the first two digits represent the starting year and the last two digits
#'  represent the ending year of the season.
#' @examples
#' get_previous_seasons("2021-06-01", 3)
#' @export
get_previous_seasons <- function(date, minus_years = 1) {
  date <- lubridate::as_date(date)
  y <- ifelse(lubridate::quarter(date) == 4, lubridate::year(date),
    lubridate::year(date) - 1
  ) %>%
    stringr::str_sub(3L) %>%
    as.numeric()
  season <- paste0(
    sprintf("%02d", y - minus_years),
    sprintf("%02d", y + 1 - minus_years)
  )
  return(season)
}
