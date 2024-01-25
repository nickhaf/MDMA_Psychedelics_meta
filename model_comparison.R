## Model comparison
Now we can test if our more complex model (the one with the extra varaince level) performs better than the less complex model:

  ```{r}
#| echo: false

## Model with level 3 variance set to zero, so we assume all effect sizes to be independent
models_2 <- lapply(my_dat[1:3], fit_rma.mv, mods = ~ drugType, sigma2 = c(0, NA))

## Without moderator for the last data set:
models_2[[4]] <- fit_rma.mv( my_dat[[4]], sigma2 = c(0, NA))


names(models) <- file_sources
names(models_2) <- file_sources

models_2_summary <- lapply(models_2, summary)
```


```{r, eval = FALSE}
#| echo: false

for(i in 1:length(models)){
  print(anova(models[[i]], models_2[[i]], refit = TRUE))
}
```
The nested model doesn't perfom better, so the effects within studies are largely homogneous. However, we know the effects are not independent, so we can keep the three-level model.

