
rm(list = ls())

library(tidyverse)
library(biathlonResults)
library(magrittr)
library(lubridate)
library(roll)
library(zoo)
library(gbm)


# Data download -----------------------------------------------------------


# Get events of the four seasons

year <- c(10:22)
seasons <- paste0(sprintf("%02d",year), sprintf("%02d",year+1))
events_df <- lapply(seasons, get_events, level = 1) %>% bind_rows()


# Get competitions

comp_df <- lapply(events_df$EventId, get_competition) %>%
  bind_rows() %>%
  filter(!DisciplineId %in% c("RL", "SR", "SI"))

# Get race results for these races
race_df <- pbapply::pblapply(comp_df$RaceId, get_race_results) %>% bind_rows()


# Get age of athletes

ath_df <- race_df %>% distinct(IBUId)

ath_rec <- pbapply::pblapply(ath_df$IBUId, biathlonResults::get_recent) %>%
  bind_rows()

ath_birthdays <- ath_rec %>%
  bind_rows() %>%
  distinct(IBUId, .keep_all = T) %>%
  select(IBUId, bday = Birthdate)

data("birthdays")
ath_birthdays <- birthdays


# Feature engineering -----------------------------------------------------


# Read raw data
race_df <- race_df %>%
  filter(!Comp.DisciplineId %in% c("SI", "RL", "SR"))

# Select and rename vars

race_df %<>% select(RaceId,
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


# Cleaning

# Drop races that are not result, missings, DNFs, DSQs etc
race_df %<>% filter(
  is_result == T,
  !is.na(shooting_errors),
  result_order < 1000
)


# Date operations

# Get race date
race_df %<>% mutate(race_date = lubridate::as_date(race_start),
                    shooting_errors = as.numeric(shooting_errors),
                    behind = str_remove(behind, pattern = "\\+"),
                    behind_s = ifelse(str_detect(behind, "\\:") == F, paste0("0:", behind), behind) %>%
                      lubridate::ms() %>%
                      lubridate::period_to_seconds()
)

# Days since last race
race_df %<>%
  arrange(IBUId, race_date) %>%
  group_by(IBUId) %>%
  mutate(
    lag_race_date = lag(race_date),
    time_last_race = as.numeric(race_date - lag_race_date)
  ) %>%
  ungroup()


# Count races per athlete and season

race_df %<>%
  mutate(count_helper = 1,
         race_start = lubridate::as_datetime(race_start)) %>%
  arrange(IBUId,race_start) %>%
  group_by(IBUId, season) %>%
  mutate(races_count = cumsum(count_helper)) %>%
  select(-count_helper) %>%
  ungroup()



# Race-level standardization - before aggregation

# Standardize behind and shooting errors on competition level
race_df %<>%
  group_by(RaceId) %>%
  mutate(
    across(
      c(
        behind_s,
        shooting_errors
      ),
      ~ ((.x - mean(.x)) / sd(.x))
    )
  ) %>%
  ungroup()


# Compute Athlete-Race level scores

# Set number of races to take into account:
n_long <- 26 # ~ 1 season
n_long_disc <- 10
n_short <- 3
n_slope <- 3

# Function to calculate rolling (linear regression) slope
rollingSlope.lm.fit <- function(vector) {
  a <- coef(.lm.fit(cbind(1, seq(vector)), vector))[2]
  return(a)
}


# Calculate features
race_df %<>%
  arrange(IBUId, race_date) %>%
  group_by(IBUId) %>%
  mutate(
    lag_behind_s = lag(behind_s),
    lag_shooting_errors = lag(shooting_errors),
    lag_rank = lag(result_order),#
    longterm_performance = roll::roll_mean(lag_behind_s, width = n_long, complete_obs = T, min_obs = 2),
    shortterm_performance = roll::roll_mean(lag_behind_s, width = n_short, complete_obs = T, min_obs = 2),
    longterm_shooting = roll::roll_mean(lag_shooting_errors, width = n_long, complete_obs = T, min_obs = 2),
    shortterm_shooting = roll::roll_mean(lag_shooting_errors, width = n_short, complete_obs = T, min_obs = 2),
    longterm_rank = roll::roll_mean(lag_rank, width = n_long, complete_obs = T, min_obs = 2),
    shortterm_rank = roll::roll_mean(lag_rank, width = n_short, complete_obs = T, min_obs = 2)
  ) %>%
  filter(!is.na(lag_rank)) %>%
  arrange(IBUId, race_date) %>%
  mutate(
    slope_time = rollapply(lag_behind_s, width = n_slope, FUN = rollingSlope.lm.fit, fill = NA, align = "right"),
    slope_rank = rollapply(lag_rank, width = n_slope, FUN = rollingSlope.lm.fit, fill = NA, align = "right")
  ) %>%
  ungroup()


# Gender
race_df %<>% mutate(
  male = as.numeric(cat == "SM")
)

# Discipline
race_df$disc %<>%
  factor()


# Get disc specific stats

# Split dataframe into discipline-specific dfs
disc_splits <- split(race_df, race_df$disc)

# Map mutate functions on all dataframes
disc_splits %<>%
  map(~ .x %>%
        arrange(IBUId, race_date) %>%
        group_by(IBUId) %>%
        mutate(
          disc_lag_behind_s = lag(behind_s),
          disc_lag_shooting_errors = lag(shooting_errors),
          disc_lag_rank = lag(result_order),
          disc_longterm_performance = roll::roll_mean(disc_lag_behind_s, width = n_long_disc, complete_obs = T, min_obs = 2),
          disc_shortterm_performance = roll::roll_mean(disc_lag_behind_s, width = n_short, complete_obs = T, min_obs = 2),
          disc_longterm_shooting = roll::roll_mean(disc_lag_shooting_errors, width = n_long_disc, complete_obs = T, min_obs = 2),
          disc_shortterm_shooting = roll::roll_mean(disc_lag_shooting_errors, width = n_short, complete_obs = T, min_obs = 2),
          disc_longterm_rank = roll::roll_mean(disc_lag_rank, width = n_long_disc, complete_obs = T, min_obs = 2),
          disc_shortterm_rank = roll::roll_mean(disc_lag_rank, width = n_short, complete_obs = T, min_obs = 2)
        ) %>%
        filter(!is.na(disc_lag_rank)) %>%
        arrange(IBUId, race_date) %>%
        mutate(
          disc_slope_time = rollapply(disc_lag_behind_s, width = n_slope, FUN = rollingSlope.lm.fit, fill = NA, align = "right"),
          disc_slope_rank = rollapply(disc_lag_rank, width = n_slope, FUN = rollingSlope.lm.fit, fill = NA, align = "right")
        ) %>%
        ungroup()
  )


# Bind, filter, join

disc_df <- bind_rows(disc_splits) %>%
  filter(!is.na(disc_slope_rank)) %>%
  select(RaceId, IBUId, starts_with("disc_")) #%>%
#mutate(pu_starting_lag = replace_na(pu_starting_lag, 0))

race_df %<>% right_join(disc_df)


# Merge
race_df %<>% left_join(ath_birthdays) %>%
  mutate(bday = lubridate::as_date(bday),
         age = as.numeric(race_date - bday))


# Standardize on race level

race_df %<>%
  group_by(RaceId) %>%
  mutate(
    across(
      c(longterm_performance,
        shortterm_performance,

        slope_time,
        slope_rank,

        longterm_rank,
        shortterm_rank,

        longterm_shooting,
        shortterm_shooting,

        disc_longterm_performance,
        disc_shortterm_performance,

        disc_longterm_rank,
        disc_shortterm_rank,

        disc_slope_time,
        disc_slope_rank,

        pu_starting_lag, # also don't standardize this - already done above

        # races_count,    # Keep these as they are due to sometimes lacking variance on race-level
        # time_last_race

        age

      ),
      ~ ((.x - mean(.x, na.rm = T)) / sd(.x, na.rm = T))
    )
  ) %>%
  ungroup()


# fill in pu_starting_lag
race_df %<>% mutate(pu_starting_lag = replace_na(pu_starting_lag, 0),
                    is_pursuit = ifelse(disc == "PU", 1, 0))



## Drop NAs
race_df %<>% filter(!is.na(slope_rank),
                    !is.na(disc_longterm_performance))



# Reverse ranking - 1st = higest score

minmaxscale <- function(x) (x - min(x)) / (max(x) - min(x))

race_df %<>% group_by(RaceId) %>%
  mutate(rank_weight = minmaxscale((max(result_order)+1)-result_order)) %>%
  ungroup()


# Factorize
race_df$cat %<>% factor()
race_df$disc %<>% factor()
race_df$location %<>% factor()


# Split train/test data ---------------------------------------------------

## Randomly pick races for training
set.seed(123)
races_train <- race_df %>% distinct(RaceId) %>% sample_frac(.9)
races_test <- race_df  %>% distinct(RaceId) %>% filter(!RaceId %in% races_train$RaceId)

# Split df
train_df <- race_df %>% filter(RaceId %in% races_train$RaceId)
test_df <- race_df %>% filter(RaceId %in% races_test$RaceId)



# Train gbm model ---------------------------------------------------------

# Formula

m_formula <- rank_weight ~

  # longterm_performance+
  # shortterm_performance+

  longterm_rank+
  shortterm_rank+

  # slope_time+
  slope_rank+

  longterm_shooting+
  shortterm_shooting+

  # disc_longterm_performance * disc+
  # disc_shortterm_performance * disc+

  disc_longterm_rank * disc+
  #  disc_shortterm_rank * disc+

  # disc_slope_time * disc+
  disc_slope_rank * disc+

  pu_starting_lag*is_pursuit+

  cat+
  disc+
  location+
  age+

  time_last_race +
  races_count



# Train final model with more folds ---------------------------------------

gbm.ndcg5_small <- gbm(
  formula = m_formula,
  data = train_df,
  distribution = list(
    name='pairwise',   # pairwise
    metric='ndcg',     # ranking metric: ndcg
    group="RaceId",
    max.rank = 5),
  n.trees = 2000,
  shrinkage = 0.01,
  interaction.depth = 7,
  n.minobsinnode = 5,
  cv.folds = 10,
  n.cores = 8,
  keep.data = FALSE
)


data("gbm.ndcg5")
# estimate number of trees
best <- gbm.perf(gbm.ndcg5, method='cv')
best_sml<- gbm.perf(gbm.ndcg5_small, method='cv')

# 1858 previously

# get MSE and compute RMSE
sqrt(gbm.ndcg5$cv.error[best])
sqrt(gbm.ndcg5_small$cv.error[best_sml])


gbm.ndcg5 <- gbm.ndcg5_small


library(pryr)
bit <- unlist(lapply(gbm.ndcg5_small,object_size))
round(bit/sum(bit),3)

# Use  --------------------------------------------------------------------

# Load previously trained model
gbm.ndcg5 <- readRDS("../Biathlon/models/gbm_ndcg5_season_1011_2223_20230106.rds")

usethis::use_data(gbm.ndcg5, overwrite = T, compress = "xz")




