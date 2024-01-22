sapply(here::here("R", c("functions.R", "prepare_data.R")), source)

# Fit the model
models <- lapply(my_dat, fit_rma.mv)

## Model with level 3 variance set to zero, so we assume all effect sizes to be independent
models_2 <- lapply(my_dat[1:3], function(df){
  rma.mv(yi = es,
         V = var_es, # Squared standard error of the effect size
         data = df, # data set
         random = ~1|Study/es.id, #Nesting the effect size within the Studys
         test = "t", # similar to Knapp-Hartung method, recommended
         dfs = "contain",
         method = "REML", # Restricted maximum-likelihood, recommended
         mods = ~ drugType # Subgroup analysis, adds regression weight of the predictor variable drug type to the model
  )
})


## Without moderator for the last data set:
models_cognitive[[4]] <- rma.mv(yi = es,
                                V = var_es, # Squared standard error of the effect size
                                data = my_dat[[4]], # data set
                                random = ~1|Study/es.id, #Nesting the effect size within the Studys
                                test = "t", # similar to Knapp-Hartung method, recommended
                                dfs = "contain",
                                method = "REML" # Restricted maximum-likelihood, recommended
)


## Model with level 3 variance set to zero, so we assume all effect sizes to be independent
models_cognitive_2 <- lapply(my_dat[1:3], function(df){
  rma.mv(yi = es,
         V = var_es, # Squared standard error of the effect size
         data = df, # data set
         random = ~1|Study/es.id, #Nesting the effect size within the Studys
         test = "t", # similar to Knapp-Hartung method, recommended
         dfs = "contain",
         method = "REML", # Restricted maximum-likelihood, recommended
         mods = ~ drugType, # Subgroup analysis, adds regression weight of the predictor variable drug type to the model
         sigma2 = c(0, NA)
  )
})


## Without moderator for the last data set:
models_cognitive_2[[4]] <- rma.mv(yi = es,
                                  V = var_es, # Squared standard error of the effect size
                                  data = my_dat[[4]], # data set
                                  random = ~1|Study/es.id, #Nesting the effect size within the Studys
                                  test = "t", # similar to Knapp-Hartung method, recommended
                                  dfs = "contain",
                                  method = "REML", # Restricted maximum-likelihood, recommended
                                  sigma2 = c(0, NA)
)

names(models_cognitive) <- file_sources
names(models_cognitive_2) <- file_sources

models_summary <- lapply(models_cognitive, summary)
models_2_summary <- lapply(models_cognitive_2, summary)
i2 <- lapply(models_cognitive, var.comp)
