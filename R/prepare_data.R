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

my_dat[["20240115_MABerlin_attention.csv"]][my_dat[["20240115_MABerlin_attention.csv"]]$Study == "Kuypers & Samyn & Ramaekers (2006)", "Study"] <- "Kuypers, Samyn & Ramaekers (2006)"
