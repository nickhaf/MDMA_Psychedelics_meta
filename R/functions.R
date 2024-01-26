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

fit_contrasts <- function(models){

  lapply(models, function(model){

    contrast <- anova(model, X = rbind(c(-1, 1)))

    contrast_df <- as.data.frame(contrast)
    ## KI
    contrast_df$ci.lb <- contrast$Xb + qt(p = 0.025, df = contrast$ddf)*contrast$se
    contrast_df$ci.ub <- contrast$Xb - qt(p = 0.025, df = contrast$ddf)*contrast$se

    return(contrast_df)

  })
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

plot_forestplot <- function(plot_dat){

  x_min <- floor(min(plot_dat$ci_lb, na.rm = TRUE))
  x_max <- ceiling(max(plot_dat$ci_ub, na.rm = TRUE))


  ggplot(
    data = plot_dat,
    mapping = aes(
      x = es,
      y = es.id,
      colour = drugType
    )
  ) +
    ## background stripes --------------------
  ggplot2::geom_tile(
    ggplot2::aes(
      width = Inf,
      height = 1
    ),
    colour = NA,
    fill = plot_dat$background_colour
  ) +
    geom_vline(xintercept = 0, colour = "black", linetype = "dotted") +
    ## Confidence intervals ------------------
  geom_segment(
    aes(
      x = ci_lb,
      xend = ci_ub,
      y = es.id,
      yend = es.id
    ),
    colour = linecolour
  ) +
    geom_segment(
      aes(
        x = ci_lb,
        xend = ci_lb,
        y = es.id - 0.1,
        yend = es.id + 0.1
      ),
      colour = linecolour
    ) +
    geom_segment(
      aes(
        x = ci_ub,
        xend = ci_ub,
        y = es.id - 0.1,
        yend = es.id + 0.1
      ),
      colour = linecolour
    ) +
    # Points ------------------
  geom_point(aes(size = N)) + # , shape = drug
    # Point (and CI) labels --------------------
  geom_text(aes(label = round(es, 2)),
            size = 3,
            colour = "black",
            nudge_y = 0.3
  ) +
    geom_text(aes(x = ci_lb, label = round(ci_lb, 2)),
              size = 2.5,
              nudge_x = - 0.45) +
    geom_text(aes(x = ci_ub, label = round(ci_ub, 2)),
              size = 2.5,
              nudge_x =  0.45) +
    # Column text -----------------
  geom_richtext(aes(label = Study, x = -22),
                colour = "black",
                hjust = 0,
                size = 3.5,
                fill = NA,
                label.color = NA,
                label.padding = grid::unit(rep(0, 4), "pt") # remove padding
  ) +
    geom_richtext(aes(label = Test, x = -13),
                  colour = "black",
                  hjust = 0.5,
                  size = 3.5,
                  fill = NA,
                  label.color = NA,
                  label.padding = grid::unit(rep(0, 4), "pt") # remove padding
    ) +
    geom_richtext(
      aes(label = N_label, x = -10),
      colour = "black",
      hjust = 0.5,
      size = 3.5,
      fill = NA,
      label.color = NA,
      label.padding = grid::unit(rep(0, 4), "pt") # remove padding
    ) +
    ## Header box --------------------------
  ggplot2::annotate("segment",
                    x = -Inf,
                    xend = Inf,
                    y = max(plot_dat$es.id) + 0.5,
                    yend = max(plot_dat$es.id) + 0.5,
                    linewidth = 0.1
  ) +
    ggplot2::annotate("segment",
                      x = -Inf,
                      xend = Inf,
                      y = max(plot_dat$es.id) - 0.5,
                      yend = max(plot_dat$es.id) - 0.5,
                      linewidth = 0.1
    ) +
    ## x axis line ---------
  annotate("segment", x = x_min, xend = x_max, y = 0, yend = 0) +
    # Meta Analysis results ------------
  annotate("text", x = -12.5, y = 1.75, label = "Three-Level-Meta-Analysis- Model", size = 4, fontface = 2) +
    geom_polygon(
      data = diamond(
        side_length = abs(models[[1]]$ci.lb-models[[1]]$ci.ub)[1],
        center_x = models[[1]]$beta["drugTypemdma", ],
        center_y = 2
      ),
      mapping = aes(x = x_coords, y = y_coords, colour = NULL), fill = mdma_col, colour = "black"
    ) +
    geom_polygon(
      data = diamond(
        side_length = abs(models[[1]]$ci.lb-models[[1]]$ci.ub)[2],
        center_x = models[[1]]$beta["drugTypepsychedelic", ],
        center_y = 1
      ),
      mapping = aes(x = x_coords, y = y_coords, colour = NULL), fill = psych_col, colour = "black"
    ) +
    annotate("text",
             x = round(models[[1]]$beta["drugTypemdma", ], 2),
             y = 2.5,
             label = format(round(models[[1]]$beta["drugTypemdma", ], 2), nsmall = 2),
             size = 3.5) +
    annotate("text",
             x = round(models[[1]]$beta["drugTypepsychedelic", ], 2),
             y = 1.5,
             label = format(round(models[[1]]$beta["drugTypepsychedelic", ], 2), nsmall = 2),
             size = 3.5) +
    ## Set scales, remove padding around some plot borders -------------
  scale_colour_manual(values = c(mdma_col, psych_col)) +
    scale_x_continuous(breaks = seq(x_min, x_max, 1), expand = c(0, 0.2)) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_size_continuous(range = c(1, 3)) +
    ## Set themes -------------------
  theme_classic() +
    theme(
      # text = element_text(family="serif"),
      axis.line = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title = element_blank(),
      axis.text.y = element_blank()
    ) +
    NULL
}
