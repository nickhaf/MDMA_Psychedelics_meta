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

plot_forestplot <- function(plot_dat, model_res, scale_min, scale_max){

  x_min <- floor(scale_min)
  x_max <- ceiling(scale_max)


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
  geom_richtext(aes(label = Study,
                    x = (x_min - 13)),
                colour = "black",
                hjust = 0,
                size = 3.5,
                fill = NA,
                label.color = NA,
                label.padding = grid::unit(rep(0, 4), "pt") # remove padding
  ) +
    geom_richtext(aes(label = Test,
                      x = (x_min - 6)),
                  colour = "black",
                  hjust = 0.5,
                  size = 3.5,
                  fill = NA,
                  label.color = NA,
                  label.padding = grid::unit(rep(0, 4), "pt") # remove padding
    ) +
    geom_richtext(
      aes(label = N_label,
          x = (x_min - 3)),
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
  annotate("text",
           x = (x_min - 13),
           y = 1.75,
           label = "Three-Level-Meta-Analysis- Model",
           size = 4,
           fontface = 2,
           hjust = 0) +
    geom_polygon(
      data =
        as.data.frame(rbind(
        diamond(
        side_length = abs(model_res$ci_lb - model_res$ci_ub)[1],
        center_x = model_res["drugTypemdma", "estimate" ],
        center_y = 2
      ),
      diamond(
        side_length = abs(model_res$ci_lb - model_res$ci_ub)[1],
        center_x = model_res["drugTypepsychedelic", "estimate" ],
        center_y = 1
      )
      )) %>%
        mutate(drugType = c(rep("MDMA", 4), rep("Psychedelic", 4))
               ),
      mapping = aes(x = x_coords, y = y_coords, fill = drugType),
      colour = "black"
    ) +
    annotate("text",
             x = model_res["drugTypemdma", "estimate"],
             y = 2.5,
             label = format(round(model_res["drugTypemdma", "estimate"], 2), nsmall = 2),
             size = 3.5) +
    annotate("text",
             x = model_res["drugTypepsychedelic", "estimate"],
             y = 1.5,
             label = format(round(model_res["drugTypepsychedelic", "estimate"], 2), nsmall = 2),
             size = 3.5) +
    # annotate("text",
    #          x = round(models[[1]]$beta["drugTypepsychedelic", ], 2),
    #          y = 1.5,
    #          label = format(round(models[[1]]$beta["drugTypepsychedelic", ], 2), nsmall = 2),
    #          size = 3.5) +
    ## Set scales, remove padding around some plot borders -------------
  scale_colour_manual(values = c(mdma_col, psych_col)) +
    scale_fill_manual(values = c(mdma_col, psych_col)) +
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
