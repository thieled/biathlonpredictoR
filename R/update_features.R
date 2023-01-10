#' Update features for a list of athletes
#'
#' @description Update features for a list of athletes by getting all the
#' results of the athletes from the biathlonResults API wrapper. ... ADD DESCRIPTION
#' @param athlete_list A list of athlete IBUIds
#' @param cutoff_datetime A date time up to which the features need to be updated. Default is current date time
#' @param n_long number of long races to consider in the features
#' @param n_long_disc number of races of each discipline to consider in the features
#' @param n_short number of short races to consider in the features
#' @param n_slope number of slope races to consider in the features
#' @return A dataframe with the updated features for each athlete
#' @examples
#' update_features(c("BTNOR11605199301"), n_long = 2, n_slope = 2)
#' @export
update_features <- function(athlete_list,
                            cutoff_datetime = NULL,
                            n_long = 26,
                            n_long_disc = 10,
                            n_short = 3,
                            n_slope = 3) {
  # Get races of athletes
  message("Get all races per athlete...")

  ath_results <- pbapply::pblapply(
    athlete_list,
    biathlonResults::get_results
  ) %>%
    data.table::rbindlist(fill = T)

  # Check cutoff date
  if (is.null(cutoff_datetime)) cutoff_datetime <- lubridate::now()
  cutoff_datetime <- lubridate::as_datetime(cutoff_datetime)

  # Determine seasons to take into account: Current and last 2 seasons
  sss <- c(get_season(cutoff_datetime),
           get_previous_seasons(cutoff_datetime, 1),
           get_previous_seasons(cutoff_datetime, 2))


  # Keep the past 3 season per athlete
  ath_results <- ath_results %>%
    dplyr::mutate(Season = stringr::str_remove(Season, "/")) %>%
    dplyr::filter(Season %in% sss,
                  Level %in% c("WC", "WCH", "OWG"),
                  Comp %in% c("SP", "IN", "MS", "PU"))

  # Get races
  race_list <- ath_results %>%
    dplyr::distinct(RaceId) %>%
    dplyr::pull()

  # Get complete results of these races
  message("Get complete results of these races...")

  race_df <- pbapply::pblapply(race_list, biathlonResults::get_race_results) %>%
            dplyr::bind_rows()

  # Select
  race_df %<>% dplyr::select(RaceId,
                      EventId = Event.EventId,
                      is_result = IsResult,
                      km = Comp.km,
                      disc = Comp.DisciplineId,
                      cat = Comp.catId,
                      race_start = Comp.StartTime,
                      season = Event.SeasonId,
                      location = Event.OrganizerId,
                      start_order = StartOrder,
                      result_order = ResultOrder,
                      IBUId,
                      short_name = ShortName,
                      nat = Nat,
                      rank = Rank,
                      shootings = Shootings,
                      shooting_errors = ShootingTotal,
                      total_time = TotalTime,
                      behind = Behind,
                      pu_starting_lag = PursuitStartDistance
  )

  # Clean, prepare variables
  race_df %<>%
    dplyr::filter(
      is_result == T,
      result_order < 1000,
      !disc %in% c("SI", "RL", "SR"),
      !is.na(behind),
      !is.na(shooting_errors)
    ) %>%
    dplyr::mutate(
      race_date = lubridate::as_date(race_start),
      shooting_errors = as.numeric(shooting_errors),
      behind = stringr::str_remove(behind, pattern = "\\+"),
      behind_s = ifelse(stringr::str_detect(behind, "\\:") == F, paste0("0:", behind), behind) %>%
        lubridate::ms() %>%
        lubridate::period_to_seconds(),
      count_helper = 1,
      race_start = lubridate::as_datetime(race_start)
    ) %>%
    dplyr::filter(race_date <= cutoff_datetime) %>%
    dplyr::arrange(race_start) %>%
    dplyr::group_by(IBUId, season) %>%
    dplyr::mutate(races_count = cumsum(count_helper)) %>%
    dplyr::select(-count_helper) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(IBUId, race_date) %>%
    dplyr::group_by(IBUId) %>%
    dplyr::mutate(
      lag_race_date = dplyr::lag(race_date),
      time_last_race = as.numeric(race_date - lag_race_date)
    ) %>%
    dplyr::ungroup()


  # Get age of athletes
  message("Merge birthdays of athletes in those races...")

  data("birthdays")

  race_df %<>%
    dplyr::left_join(birthdays) %>%
    dplyr::mutate(
      bday = lubridate::as_date(bday),
      age = as.numeric(lubridate::as_date(race_date) - lubridate::as_date(bday)),
      age = tidyr::replace_na(age, mean(age, na.rm = T))
    )

  ### ADD: Check if any missings - if so, get them from API


  # Standardize behind, age, and shooting errors on competition level
  race_df %<>%
    dplyr::group_by(RaceId) %>%
    dplyr::mutate(
      dplyr::across(
        c(
          behind_s,
          shooting_errors,
          age
        ),
        ~ ((.x - mean(.x)) / sd(.x))
      )
    ) %>%
    dplyr::ungroup()


  # Compute features

  # Rolling slope function
  rollingSlope.lm.fit <- function(vector) {
    s <- stats::coef(stats::.lm.fit(cbind(1, seq(vector)), vector))[2]
    return(s)
  }


  ## Switch between historical data and new data

  if(lubridate::as_date(cutoff_datetime) <= lubridate::today()){

    message("Race date is in the past or today. Using lagged data for computing features...")

    # Calculate features
    race_df %<>%
      dplyr::arrange(IBUId, race_date) %>%
      dplyr::group_by(IBUId) %>%
      dplyr::mutate(
        lag_behind_s = dplyr::lag(behind_s),
        lag_shooting_errors = dplyr::lag(shooting_errors),
        lag_rank = dplyr::lag(result_order),#
        longterm_performance = roll::roll_mean(lag_behind_s, width = n_long, complete_obs = T, min_obs = 2),
        shortterm_performance = roll::roll_mean(lag_behind_s, width = n_short, complete_obs = T, min_obs = 2),
        longterm_shooting = roll::roll_mean(lag_shooting_errors, width = n_long, complete_obs = T, min_obs = 2),
        shortterm_shooting = roll::roll_mean(lag_shooting_errors, width = n_short, complete_obs = T, min_obs = 2),
        longterm_rank = roll::roll_mean(lag_rank, width = n_long, complete_obs = T, min_obs = 2),
        shortterm_rank = roll::roll_mean(lag_rank, width = n_short, complete_obs = T, min_obs = 2)
      ) %>%
      dplyr::filter(!is.na(lag_rank)) %>%
      dplyr::arrange(IBUId, race_date) %>%
      dplyr::mutate(
        slope_time = zoo::rollapply(lag_behind_s, width = n_slope, FUN = rollingSlope.lm.fit, fill = NA, align = "right"),
        slope_rank = zoo::rollapply(lag_rank, width = n_slope, FUN = rollingSlope.lm.fit, fill = NA, align = "right")
      ) %>%
      dplyr::ungroup()

    # Drop observationas with NAs (except pursuit vars)
    race_df %<>% dplyr::filter(!is.na(slope_rank))

    # Split race_df in discipline dfs
    disc_splits <- split(race_df, race_df$disc)

    # Map mutate functions on all dataframes
    disc_splits %<>%
      purrr::map(~ .x %>%
                   dplyr::arrange(IBUId, race_date) %>%
                   dplyr::group_by(IBUId) %>%
                   dplyr::mutate(
              disc_lag_behind_s = dplyr::lag(behind_s),
              disc_lag_shooting_errors = dplyr::lag(shooting_errors),
              disc_lag_rank = dplyr::lag(result_order),
              disc_longterm_performance = roll::roll_mean(disc_lag_behind_s, width = n_long_disc, complete_obs = T, min_obs = 2),
              disc_shortterm_performance = roll::roll_mean(disc_lag_behind_s, width = n_short, complete_obs = T, min_obs = 2),
              disc_longterm_shooting = roll::roll_mean(disc_lag_shooting_errors, width = n_long_disc, complete_obs = T, min_obs = 2),
              disc_shortterm_shooting = roll::roll_mean(disc_lag_shooting_errors, width = n_short, complete_obs = T, min_obs = 2),
              disc_longterm_rank = roll::roll_mean(disc_lag_rank, width = n_long_disc, complete_obs = T, min_obs = 2),
              disc_shortterm_rank = roll::roll_mean(disc_lag_rank, width = n_short, complete_obs = T, min_obs = 2)
            ) %>%
              dplyr::filter(!is.na(disc_lag_rank)) %>%
              dplyr::arrange(IBUId, race_date) %>%
              dplyr::mutate(
              disc_slope_time = zoo::rollapply(disc_lag_behind_s, width = n_slope, FUN = rollingSlope.lm.fit, fill = NA, align = "right"),
              disc_slope_rank = zoo::rollapply(disc_lag_rank, width = n_slope, FUN = rollingSlope.lm.fit, fill = NA, align = "right")
            ) %>%
              dplyr::ungroup()
      )



    disc_df <- dplyr::bind_rows(disc_splits) %>%
      dplyr::filter(!is.na(disc_slope_rank)) %>%
      dplyr::select(RaceId, IBUId, race_start, pu_starting_lag, starts_with("disc"))


  }else{
    message("Race date is in the future. Using the latest available data for computing features...")

    race_df %<>%
      dplyr::filter(!is.na(result_order)) %>%
      dplyr::arrange(IBUId, race_date) %>%
      dplyr::group_by(IBUId) %>%
      dplyr::mutate(
        longterm_performance = roll::roll_mean(behind_s, width = n_long, complete_obs = T, min_obs = 2),
        longterm_shooting = roll::roll_mean(shooting_errors, width = n_long, complete_obs = T, min_obs = 2),
        longterm_rank = roll::roll_mean(result_order, width = n_long, complete_obs = T, min_obs = 2),
        shortterm_performance = roll::roll_mean(behind_s, width = n_short, complete_obs = T, min_obs = 2),
        shortterm_shooting = roll::roll_mean(shooting_errors, width = n_short, complete_obs = T, min_obs = 2),
        shortterm_rank = roll::roll_mean(result_order, width = n_short, complete_obs = T, min_obs = 2)
      ) %>%
      dplyr::arrange(IBUId, race_date) %>%
      dplyr::group_by(IBUId) %>%
      dplyr::mutate(
        slope_time = zoo::rollapply(behind_s, width = n_slope, FUN = rollingSlope.lm.fit, fill = NA, align = "right"),
        slope_rank = zoo::rollapply(rank, width = n_slope, FUN = rollingSlope.lm.fit, fill = NA, align = "right")
      ) %>%
      dplyr::ungroup()

    # Drop observationas with NAs (except pursuit vars)
    race_df %<>% dplyr::filter(!is.na(slope_rank))

    # Split race_df in discipline dfs
    disc_splits <- split(race_df, race_df$disc)

    # Map mutate functions on all dataframes
    disc_splits %<>%
      purrr::map(~ .x %<>%
                   dplyr::arrange(IBUId, race_date) %>%
                   dplyr::group_by(IBUId) %>%
                   dplyr::mutate(
                     disc_longterm_performance = roll::roll_mean(behind_s, width = n_long_disc, complete_obs = T, min_obs = 2),
                     disc_longterm_shooting = roll::roll_mean(shooting_errors, width = n_long_disc, complete_obs = T, min_obs = 2),
                     disc_longterm_rank = roll::roll_mean(result_order, width = n_long_disc, complete_obs = T, min_obs = 2),
                     disc_shortterm_performance = roll::roll_mean(behind_s, width = n_short, complete_obs = T, min_obs = 2),
                     disc_shortterm_shooting = roll::roll_mean(shooting_errors, width = n_short, complete_obs = T, min_obs = 2),
                     disc_shortterm_rank = roll::roll_mean(result_order, width = n_short, complete_obs = T, min_obs = 2)
                   ) %>%
                   dplyr::filter(!is.na(result_order)) %>%
                   dplyr::arrange(IBUId, race_date) %>%
                   dplyr::mutate(
                     disc_slope_time = zoo::rollapply(behind_s, width = n_slope, FUN = rollingSlope.lm.fit, fill = NA, align = "right"),
                     disc_slope_rank = zoo::rollapply(result_order, width = n_slope, FUN = rollingSlope.lm.fit, fill = NA, align = "right")
                   ) %>%
                   dplyr::ungroup())


    disc_df <- dplyr::bind_rows(disc_splits) %>%
      dplyr::filter(!is.na(disc_slope_rank)) %>%
      dplyr::select(RaceId, IBUId, race_start, pu_starting_lag, starts_with("disc"))

  }

  # Standardize on race level
  race_df %<>%
    dplyr::filter(!is.na(slope_rank)) %>%
    dplyr::group_by(RaceId) %>%
    dplyr::mutate(
      dplyr::across(
        c(
          longterm_performance,
          shortterm_performance,
          slope_time,
          slope_rank,
          longterm_rank,
          shortterm_rank,
          longterm_shooting,
          shortterm_shooting,
          age
        ),
        ~ ((.x - mean(.x, na.rm = T)) / stats::sd(.x, na.rm = T))
      )
    ) %>%
    dplyr::ungroup()


  disc_df %<>%
    dplyr::filter(!is.na(disc_slope_rank)) %>%
    dplyr::group_by(RaceId) %>%
    dplyr::mutate(
      dplyr::across(
        c(
          disc_longterm_performance,
          disc_shortterm_performance,
          disc_slope_time,
          disc_slope_rank,
          disc_longterm_rank,
          disc_shortterm_rank,
          disc_longterm_shooting,
          disc_shortterm_shooting,
          pu_starting_lag
        ),
        ~ ((.x - mean(.x, na.rm = T)) / stats::sd(.x, na.rm = T))
      )
    ) %>%
    dplyr::ungroup()

  # Keep only latest features
  overall_latest <- race_df %>%
    dplyr::group_by(IBUId) %>%
    dplyr::arrange(dplyr::desc(lubridate::as_datetime(race_start))) %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      IBUId,
      short_name,
      race_date,
      season,
      cat,
      age,
      longterm_performance,
      longterm_shooting,
      longterm_rank,
      shortterm_performance,
      shortterm_shooting,
      shortterm_rank,
      slope_time,
      slope_rank,
      time_last_race,
      races_count
    )

  disc_latest <- disc_df %>%
    dplyr::group_by(IBUId, disc) %>%
    dplyr::arrange(dplyr::desc(lubridate::as_datetime(race_start))) %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      IBUId,
      dplyr::starts_with("disc"),
      pu_starting_lag
    )

  latest_features <- dplyr::right_join(overall_latest, disc_latest)

  ## Fill in NAs
  latest_features %<>% dplyr::mutate(
    pu_starting_lag = tidyr::replace_na(pu_starting_lag, 0),
    is_pursuit = ifelse(disc == "PU", 1, 0)
  )

  # Drop missings, keep only athletes from query
  latest_features %<>% dplyr::filter(
    IBUId %in% athlete_list,
    !is.na(slope_rank),
    !is.na(disc_slope_rank)
  )


  return(latest_features)
}
