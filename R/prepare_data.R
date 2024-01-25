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
