fit_rma.mv <- function(df, ...){
  rma.mv(yi = es,
         V = var_es, # Squared standard error of the effect size
         data = df, # data set
         random = ~1|Study/es.id, #Nesting the effect size within the Studys
         test = "t", # similar to Knapp-Hartung method, recommended
         dfs = "contain",
         method = "REML", # Restricted maximum-likelihood, recommended
         ...
         )
}

fit_sensitivity <- function(df, ...){

  sensitivity_cols <- colnames(df)[grep("sensitivity", colnames(df))]

  fitted_models <- lapply(sensitivity_cols, function(subset_col){

    subset_model <- fit_rma.mv(df,
                               subset = as.logical(df[, subset_col]),
                               ...
    )
    return(subset_model)
  })

  names(fitted_models) <- sensitivity_cols

  return(fitted_models)

}
