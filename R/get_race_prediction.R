
#' @title Get predictions of a biathlon race
#' @description The function returns predictions of a Biathlon race, by calling the update_features function and a gbm model.
#' @param race_id A string representing the race id.
#' @param gbm_model A gbm model object.
#' @param n_long The number races to consider for computing long-term features. Default is 26.
#' @param n_long_disc The number races to consider for computing long-term, discipline-specific features. Default is 10.
#' @param n_short The number races to consider for computing short-term features. Default is 3.
#' @param n_slope The number races to consider for computing the slope of the trend in previous performances.
#' @return Returns a dataframe with predictions of the race.
#' @export
#' @import biathlonResults
#' @examples
#' data("gbm.ndcg5")
#' get_race_prediction("BT2122SWRLCP04SWSP", gbm.ndcg5)
get_race_prediction <- function(race_id,
                                gbm_model,
                                n_long = 26,
                                n_long_disc = 10,
                                n_short = 3,
                                n_slope = 3) {
  # Get race info
  race_info <- biathlonResults::get_race_results(race_id)

  # Determine season, cat, disc, and location from race_id, resp. from race_info
  season_str <- stringr::str_sub(race_id, 3L, 6L)
  category_str <- stringr::str_sub(race_id, -4L, -3L)
  discipline_str <- stringr::str_sub(race_id, -2L)

  # Get location and date
  location_str <- race_info$Event.OrganizerId[1]
  racedate <- race_info$Comp.StartTime[1] %>% lubridate::as_datetime() # save as racedate for legacy reasons
  cutoff_datetime <- racedate

  # Check if race_info contains athlete list
  if (race_info$IsResult[1] == TRUE || race_info$IsStartList[1] == TRUE) {
    message("Start list available, use actual start list...")
    ath_list <- race_info$IBUId
  } else {
    message("No start list available, use World Cup standings of that season...")

    ath_list <- biathlonResults::get_cups(season_str) %>%
      dplyr::filter(stringr::str_detect(CupId, paste0("WRLCP__", category_str, "TS"))) %>%
      dplyr::pull(CupId) %>%
      lapply(biathlonResults::get_cup_results) %>%
      dplyr::bind_rows() %>%
      dplyr::pull(IBUId)
  }

  # Get latest features
  new_features <- update_features(ath_list,
    n_long = n_long,
    n_long_disc = n_long_disc,
    n_short = n_short,
    n_slope = n_slope,
    cutoff_datetime = cutoff_datetime
  )

  # Filter and set new vars
  new_features %<>% dplyr::filter(disc == discipline_str) %>%
    dplyr::mutate(
      location = factor(location_str),
      cat = factor(category_str)
    ) %>%
    dplyr::distinct(IBUId, .keep_all = T) %>%
    dplyr::filter(IBUId %in% ath_list)

  # Use actual pursuit lag if available
  if (race_info$IsStartList[1] == TRUE && discipline_str == "PU") {
    message("Use actual starting distance for pursuit race...")

    new_features %<>% dplyr::select(-pu_starting_lag)

    pu_starting_lag_df <- race_info %>%
      dplyr::select(IBUId, pu_starting_lag = PursuitStartDistance) %>%
      dplyr::mutate(pu_starting_lag = ((pu_starting_lag - mean(pu_starting_lag)) / stats::sd(pu_starting_lag))) %>%
      dplyr::filter(IBUId %in% ath_list)

    new_features %<>% dplyr::left_join(pu_starting_lag_df)
  }

  # Replace race date with scheduled date
  new_features$race_date <- race_info$Comp.StartTime[1] %>% lubridate::as_date()

  # And set race_id
  new_features$RaceId <- race_id

  # Get predictions
  message("Get predictions...")

  suppressWarnings(
    new_features$pred <- gbm::predict.gbm(gbm_model, new_features, which.min(gbm_model$cv.error))
  )

  # Compute proper ranks
  new_features %<>% dplyr::mutate(pred_rank = order(order(pred, decreasing = T))) %>%
    dplyr::arrange(pred_rank)

  return(new_features)

  }

