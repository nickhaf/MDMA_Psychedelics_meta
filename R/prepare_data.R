library(dplyr)
library(here)
library(metafor)
library(dmetar)
library(psych)
library(grateful)

file_sources <- list.files(
  path = here::here("data"),
  pattern = "*.csv"
)

my_dat <- lapply(here::here("data", file_sources), read.csv)
my_dat <- lapply(my_dat, function(x){
  x %>%
    mutate(var_es = se_es^2)
})
names(my_dat) <- file_sources



# Overview ----------------------------------------------------------------
summary_tables <- lapply(my_dat[1:3], function(x){
  describeBy(x[, c("N", "es", "se_es")], skew = FALSE, group = x$drugType)
})

summary_tables[[4]] <- describe(my_dat[[4]][, c("N", "es", "se_es")], skew = FALSE)
names(summary_tables) <- file_sources
