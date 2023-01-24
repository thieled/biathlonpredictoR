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
                         "time_last_race",
                         "CupId",
                         "IsTeam",
                         "is_team",
                         "pred",
                         "pred_rank",
                         "EndDate",
                         "ScheduleStatus",
                         "StartTime",
                         "DisciplineId"
                         ))




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



#' @title Get the next scheduled event
#' @description The function returns the next scheduled event, filtered by WorldCup level and schedule status.
#' @return Returns a dataframe with information about the next scheduled World Cup event.
#' @export
#' @import biathlonResults
#' @examples
#' get_next_event()
get_next_event <- function(){

  current_season <-get_season(lubridate::today())

  events <- biathlonResults::get_events(current_season) %>%
    dplyr::filter(Level == 1L) %>%
    dplyr::mutate(end_in = as.numeric(lubridate::as_date(EndDate) - lubridate::today())
    )

  next_event <- biathlonResults::get_competition(events[which.max(1/events$end_in),]$EventId) %>%
    dplyr::mutate(diff_to_today = as.numeric(lubridate::as_date(StartTime) - lubridate::today())) %>%
    dplyr::filter(ScheduleStatus == "SCHEDULED")

  return(next_event)

}


#' @title Get the next scheduled race
#' @description The function returns the next scheduled race, filtered by level and schedule status.
#' @return Returns a dataframe with information about the next scheduled World Cup race.
#' @export
#' @examples
#' get_next_event()
get_next_race <- function(){

  next_event <- get_next_event()

  next_race <- next_event[which.max(1/next_event$diff_to_today),]

  return(next_race)

}


#' @title Get the after n next scheduled race
#' @description The function returns the next scheduled race plus n, filtered by level and schedule status.
#' For the moment, team events are omitted here.
#' @return Returns a dataframe with information about the next scheduled World Cup race. Note: only for the
#' event.
#' @param n Jump to n after next race. Default is 0.
#' @export
#' @examples
#' get_next_race_plus_n()
get_next_race_plus_n <- function(n=0){

  next_event <- get_next_event() %>% dplyr::filter(!DisciplineId %in% c("RL", "SR"))

  if(n >= nrow(next_event)) {stop(paste("Not more than", n, "races scheduled for next event.", sep = " "))}

  next_race_plus_n <- next_event[which.max(1/next_event$diff_to_today)+n,]

  return(next_race_plus_n)

}
