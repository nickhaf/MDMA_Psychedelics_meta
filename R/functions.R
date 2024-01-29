fit_rma.mv <- function(df, ...) {
  rma.mv(
    yi = es,
    V = var_es, # Squared standard error of the effect size
    data = df, # data set
    random = ~ 1 | Study / es.id, # Nesting the effect size within the Studys
    test = "t", # similar to Knapp-Hartung method, recommended
    dfs = "contain",
    method = "REML", # Restricted maximum-likelihood, recommended
    ...
  )
}

fit_contrasts <- function(models) {
  lapply(models, function(model) {
    contrast <- anova(model, X = rbind(c(-1, 1)))

    contrast_df <- as.data.frame(contrast)
    ## KI
    contrast_df$ci.lb <- contrast$Xb + qt(p = 0.025, df = contrast$ddf) * contrast$se
    contrast_df$ci.ub <- contrast$Xb - qt(p = 0.025, df = contrast$ddf) * contrast$se

    return(contrast_df)
  })
}

fit_sensitivity <- function(df, ...) {
  sensitivity_cols <- colnames(df)[grep("sensitivity", colnames(df))]

  fitted_models <- lapply(sensitivity_cols, function(subset_col) {
    subset_model <- fit_rma.mv(df,
      subset = as.logical(df[, subset_col]),
      ...
    )
    return(subset_model)
  })

  names(fitted_models) <- sensitivity_cols

  return(fitted_models)
}

prep_dat <- function(dat, background_stripes) {
  my_dat <- dat %>%
    ## Wald-type CIs, often sufficient, as not the main gist.
    mutate(ci_lb = es - 1.96 * se_es) %>%
    mutate(ci_ub = es + 1.96 * se_es) %>%
    arrange(desc(drugType), desc(Study), desc(Test), ) %>%
    mutate(es.id = 1:nrow(.) + 4) %>% # the +3 makes some place so i can plot the total effect size there
    mutate(background_colour = background_stripes) %>%
    mutate(drugType = fct_recode(
      drugType,
      "Psychedelic" = "psychedelic",
      "MDMA" = "mdma"
    )) %>%
    dplyr::select(Study, Test, drugType, N, es, es.id, ci_lb, ci_ub, background_colour) %>%
    mutate(N_label = as.character(N)) %>%
    add_row(es.id = 3, drugType = "Psychedelic") %>%
    add_row(es.id = 2, drugType = "Psychedelic") %>%
    add_row(es.id = 1, drugType = "Psychedelic") %>%
    ## Column Headers
    add_row(
      es.id = max(.$es.id) + 1,
      Study = "**Study**",
      Test = "**Test**",
      N_label = "**N**",
      drugType = "Psychedelic"
    ) %>%
    dplyr::arrange(es.id)
}


plot_forestplot <- function(plot_dat, model_res, model_obj, contrast = NULL, i2, scale_min, scale_max) {
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
    annotate("segment", x = 0, xend = 0, y = 0, yend = max(plot_dat$es.id) - 0.5, colour = "darkgrey", linetype = "dotted") +
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
      size = 2.5,
      colour = "black",
      nudge_y = 0.3
    ) +
    # geom_text(aes(x = ci_lb, label = round(ci_lb, 2)),
    #   size = 2.5,
    #   colour = "black",
    #   nudge_x = -0.45
    # ) +
    # geom_text(aes(x = ci_ub, label = round(ci_ub, 2)),
    #   size = 2.5,
    #   colour = "black",
    #   nudge_x = 0.45
    # ) +
    # Column text -----------------
    geom_richtext(
      aes(
        label = Study,
        x = (x_min - 12)
      ),
      colour = "black",
      hjust = 0,
      size = 3.5,
      fill = NA,
      label.color = NA,
      label.padding = grid::unit(rep(0, 4), "pt") # remove padding
    ) +
    geom_richtext(
      aes(
        label = Test,
        x = (x_min - 4)
      ),
      colour = "black",
      hjust = 0.5,
      size = 3.5,
      fill = NA,
      label.color = NA,
      label.padding = grid::unit(rep(0, 4), "pt") # remove padding
    ) +
    geom_richtext(
      aes(
        label = N_label,
        x = (x_min - 1)
      ),
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
      x = (x_min - 12),
      y = 3.5,
      label = "Three-Level-Meta-Analysis- Model",
      size = 4,
      fontface = 2,
      hjust = 0
    ) +
    geom_richtext(
      x = x_min - 11,
      y = 2,
      label = paste0(
        "I<sup>2</sup><sub>Level 3</sub> = ", i2$results["Level 3", "I2"], "%<br>",
        "I<sup>2</sup><sub>Level 2</sub> = ", i2$results["Level 2", "I2"], "%"
      ),
      label.color = NA,
      label.padding = grid::unit(rep(0, 4), "pt"), # remove padding
      colour = "black",
      hjust = 0
    ) +
    add_model_res(model_res, contrast, i2, x_min, x_max) +
    ## Set scales, remove padding around some plot borders -------------
    scale_colour_manual(values = c("MDMA" = mdma_col, "Psychedelic" = psych_col)) +
    scale_fill_manual(values = c("MDMA" = mdma_col, "Psychedelic" = psych_col)) +
    scale_x_continuous(breaks = seq(x_min, x_max, 1), expand = c(0, 0.2)) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_size_continuous(range = c(1, 2.5)) +
    ## Set themes -------------------
    theme_classic() +
    theme(
      # text = element_text(family="serif"),
      axis.line = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title = element_blank(),
      axis.text.y = element_blank()
    ) +
    guides(fill=guide_legend(title="Drug Type")) +
    guides(
      color = guide_legend(
        title = "Drug Type")
) +
    NULL
}

add_model_res <- function(model_res, contrast, i2,  x_min, x_max) {
  if (!is.null(contrast)) {
    list(
      geom_brace(
        aes(
          c(
            model_res["drugTypemdma", "estimate"],
            model_res["drugTypepsychedelic", "estimate"]
          ),
          c(1, 1.5)
        ),
        inherit.data = F,
        rotate = 180
      ),
      geom_richtext(
        x = model_res["drugTypepsychedelic", "estimate"] - (model_res["drugTypemdma", "estimate"]) / 2,
        y = 0.7,
        colour = "black",
        label = paste0(
          round(contrast$estimate, 2),
          " , *t*(", contrast$df, ") = ", round(contrast$tval, 2),
          "; *p* = ", p_format(contrast$pval, leading.zero = FALSE)
        ),
        size = 3,
        hjust = 0.3,
        label.color = NA,
        label.padding = grid::unit(rep(0, 4), "pt") # remove padding
      ),
      geom_polygon(
        data =
          as.data.frame(rbind(
            diamond(
              center_x = model_res["drugTypemdma", "estimate"],
              center_y = 3,
              ci_ub = model_res["drugTypemdma", "ci_ub"],
              ci_lb = model_res["drugTypemdma", "ci_lb"]
            ),
            diamond(
              center_x = model_res["drugTypepsychedelic", "estimate"],
              center_y = 2,
              ci_ub = model_res["drugTypepsychedelic", "ci_ub"],
              ci_lb = model_res["drugTypepsychedelic", "ci_lb"]
            )
          )) %>%
            mutate(drugType = c(rep("MDMA", 4), rep("Psychedelic", 4))),
        mapping = aes(x = x_coords, y = y_coords, fill = drugType),
        colour = "black"
      ),
      annotate("text",
        x = model_res["drugTypemdma", "estimate"],
        y = 3.5,
        label = format(round(model_res["drugTypemdma", "estimate"], 2), nsmall = 2),
        size = 3.5
      ),
      annotate("text",
        x = model_res["drugTypepsychedelic", "estimate"],
        y = 2.5,
        label = format(round(model_res["drugTypepsychedelic", "estimate"], 2), nsmall = 2),
        size = 3.5
      )
    )
  } else {
    list(
      geom_polygon(
        data =
          as.data.frame(
            diamond(
              center_x = model_res[model_res$drugType == "Psychedelic", "estimate"],
              center_y = 3,
              ci_ub = model_res$ci_ub[1],
              ci_lb = model_res$ci_lb[1]
          )) %>%
            mutate(drugType = c(rep("Psychedelic", 4))),
        mapping = aes(x = x_coords, y = y_coords, fill = drugType),
        colour = "black"
      ),
      annotate("text",
        x = model_res[model_res$drugType == "Psychedelic", "estimate"],
        y = 3.5,
        label = format(round(model_res[model_res$drugType == "Psychedelic", "estimate"], 2), nsmall = 2),
        size = 3.5
      )
    )
  }
}


save_plot <- function(p, filename, width = 300, height = 255, scaling = 1, ...) {
  width_inch <- width / 25.4
  height_inch <- height / 25.4

  grDevices::cairo_pdf(
    file = filename,
    width = width_inch * scaling,
    height = height_inch * scaling,
    ...
  )
  plot(p)
  grDevices::dev.off()
}
