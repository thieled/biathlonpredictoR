## code to prepare `birthdays` dataset goes here


library(tidyverse)
library(biathlonResults)
library(magrittr)
library(lubridate)


# Get events of the four seasons ------------------------------------------

year <- c(10:22)
seasons <- paste0(sprintf("%02d",year), sprintf("%02d",year+1))
events_df <- lapply(seasons, get_events, level = 1) %>% bind_rows()


# Get competitions --------------------------------------------------------

comp_df <- lapply(events_df$EventId, get_competition) %>%
  bind_rows() %>%
  filter(!DisciplineId %in% c("RL", "SR", "SI"))

# Get race results for these races ------------------------------------

race_df <- pbapply::pblapply(comp_df$RaceId, get_race_results) %>% bind_rows()


# Get age of athletes -----------------------------------------------------

ath_df <- race_df %>% distinct(IBUId)

ath_rec <- pbapply::pblapply(ath_df$IBUId, biathlonResults::get_recent) %>%
  bind_rows()

birthdays <- ath_rec %>%
  bind_rows() %>%
  distinct(IBUId, .keep_all = T) %>%
  select(IBUId, bday = Birthdate)

usethis::use_data(birthdays, overwrite = T)
