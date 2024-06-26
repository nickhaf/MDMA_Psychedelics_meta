dat_names <- list.files(
  path = here::here("data"),
  pattern = "*.csv"
)

dat_maBerlin <- lapply(here::here("data", dat_names), read.csv)
dat_maBerlin <- lapply(dat_maBerlin, function(x){
  x %>%
    mutate(var_es = se_es^2)
})
names(dat_maBerlin) <- dat_names

dat_maBerlin[["20240115_MABerlin_attention.csv"]][dat_maBerlin[["20240115_MABerlin_attention.csv"]]$Study == "Kuypers & Samyn & Ramaekers (2006)", "Study"] <- "Kuypers, Samyn & Ramaekers (2006)"


saveRDS(dat_maBerlin, file = here::here("data", "dat_maBerlin.RDS"))
