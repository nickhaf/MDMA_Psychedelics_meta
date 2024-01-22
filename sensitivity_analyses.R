sapply(here::here("R", c("functions.R", "prepare_data.R")), source)

sensitivity_analyses <- lapply(my_dat[1:3], fit_sensitivity)
