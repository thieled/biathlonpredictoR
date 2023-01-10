## code to prepare `DATASET` dataset goes here


birthdays <- readRDS("C:/Users/Daniel/ucloud/Dissertation/3_Projects/Biathlon/data/ath_birthdays_20230104.rds")

usethis::use_data(birthdays, overwrite = T)
