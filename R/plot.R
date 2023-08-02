#' Plot method for class triptych
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
plot.triptych <- function(x, ...) {
  p <- ggplot2::autoplot(x, ...)
  print(p)
}


#' @importFrom ggplot2 autoplot
#' @export
ggplot2::autoplot


#' Plots a triptych
#'
#' @param object Object of class 'triptych'
#' @param plot_type Determines which kind of plot should be displayed.
#'  options are "default", "triptych", "ROC", "Murphy", "ReliabilityDiagram", or "MCBDSC".
#' @param PAV_ROC TRUE (default) if the ROC curve should be PAV-recalibrated (concave).
#' @param RelDiag_joint FALSE (default) if the reliability diagrams are plotted as separate panels.
#' @param RelDiag_breaks Manual histogram breaks in the reliability diagrams (default is NULL).
#' @param RelDiag_hist_factor Maximum value (among all panels) of the histograms in the reliability diagram (default is 0.2).
#' @param Murphy_scoretype Quantity that should be plotted on the y-axis of the Murphy diagram;
#'  Options are "score" (default), "recalibrated", "MCB", "DSC", or "MCB-DSC".
#' @param Murphy_benchmark Forecast name with respect to which the Murphy diagram is normalized (default is NA, i.e., no normalization).
#' @param Murphy_RelDiag_range bivariate vector specifying the displayed range of the x-axis in the Murphy diagram and reliability diagram
#' @param MCBDSC_score scoring rule (default "brier") used in the MCB-DSC plots
#' @param MCBDSC_lines Approximate amounts of score isolines in the MCB-DSC plot
#' @param MCBDSC_point_cols color of the points/text labels in the MCB-DSC plot
#' @param MCBDSC_repel TRUE if a "repel" geom should be used for better visibility of the text labels in the MCB-DSC plot
#' @param MCBDSC_MCB_xlim bivariate vector specifying x-axis limits in the MCB-DSC plot
#' @param MCBDSC_DSC_ylim bivariate vector specifying y-axis limits in the MCB-DSC plot
#' @param MCBDSC_UNC_hjust position (between 0 and 1) of the UNC label
#' @param plot_cols Named vector (with the forecasts) specifying the plot colors in the triptych. Default NA uses a standard color palette.
#' @param plot_linetypes Linetypes in the triptych plots
#' @param plot_linewidth Linewidth in the triptych plots
#' @param plot_margins Margins between the plots (default is grid::unit(c(0,0.02,0,0.02), "npc"))
#' @param plot_theme ggplot theme
#' @param plot_legend_title Title of the joint legend (default: "Forecast")
#' @param size_title size (default 14) of the individuel plot titles
#' @param size_axislabels size (default 12) of the axis labels
#' @param size_legend size (default 12) of the legend
#' @param size_axisticks size (default 10) of the axis ticks
#'
#' @rdname plot.triptych
#'
#' @export
autoplot.triptych <- function(object,
                              plot_type = c("default", "triptych", "ROC", "Murphy", "ReliabilityDiagram", "MCBDSC"),
                              PAV_ROC = TRUE,
                              RelDiag_joint = FALSE,
                              RelDiag_breaks = NULL,
                              RelDiag_hist_factor = 0.2,
                              Murphy_scoretype = c("score", "MCB-DSC", "MCB", "DSC", "recalibrated"),
                              Murphy_benchmark = NA,
                              Murphy_RelDiag_range = c(0, 1),
                              MCBDSC_score = "brier",
                              MCBDSC_lines = 10,
                              MCBDSC_point_cols = "red",
                              MCBDSC_repel = FALSE,
                              MCBDSC_MCB_xlim = NA,
                              MCBDSC_DSC_ylim = NA,
                              MCBDSC_UNC_hjust = 0.85,
                              plot_cols = NA,
                              plot_linetypes = "solid",
                              plot_linewidth = 0.5,
                              plot_margins = grid::unit(c(0, 0.02, 0, 0.02), "npc"),
                              plot_theme = ggplot2::theme_bw(),
                              plot_legend_title = "Forecast",
                              size_title = 14,
                              size_legend = 12,
                              size_axislabels = 12,
                              size_axisticks = 10) {
  ### Preliminaries
  FC_names <- object$FC_names
  m <- length(FC_names)
  confidence = ifelse(is.null(object$mcb_dsc_samples), FALSE,TRUE)

  # Set nice tick labels in the reliability diagram. First try, could be more elaborate
  Murphy_RelDiag_range_labels <- c(
    ceiling(10 * Murphy_RelDiag_range[1]) / 10,
    floor(10 * Murphy_RelDiag_range[2]) / 10
  )

  # Specify which plot is returned and issue messages
  plot_type <- match.arg(plot_type)
  if (plot_type == "default") {
    if (m <= 4) {
      plot_type <- "triptych"
      message("The triptych is plotted as default for no more than 4 forecasts.")
    } else {
      plot_type <- "MCBDSC"
      message(paste0(
        "The MCB-DSC display is plotted as default for more than 4 forecasts.\n",
        "Use the option plot_type='triptych' to obtain a triptych nevertheless."
      ))
    }
  }
  Murphy_scoretype <- match.arg(Murphy_scoretype)


  # Use a standard color palette if plot.cols is NA
  if (any(is.na(plot_cols)) | !(length(plot_cols) %in% c(1, m))) plot_cols <- gg_color_hue(m)
  if (length(plot_cols) == 1) plot_cols <- rep(plot_cols, m)
  names(plot_cols) <- FC_names


  # Use standard linetypes if plot_linetypes is NA
  if (any(is.na(plot_linetypes)) | !(length(plot_linetypes) %in% c(1, m))) {
    plot_linetypes <- rep(
      x = c("solid", "dashed", "dotted", "twodash", "longdash", "dotdash"),
      times = ceiling(m / 6)
    )[1:m]
  }
  if (length(plot_linetypes) == 1) plot_linetypes <- rep(plot_linetypes, m)
  names(plot_linetypes) <- FC_names


  ###############################################################################################
  ### Plot Reliability Diagrams

  # ToDo: Suppress the printed text "Scale for 'linetype' is already present"
  p_RelDiag <- quote({
    x_range <- Murphy_RelDiag_range_labels
    if (RelDiag_joint == TRUE) {
      autoplot(object$RelDiag, params_CEPline = list(size = plot_linewidth))
    } else {
      # Determine number of rows in the plot
      nr_rows <- dplyr::case_when(
        m >= 7 ~ 3,
        m >= 2 ~ 2,
        TRUE ~ 1
      )

      if (is.null(RelDiag_breaks)) {
        RelDiag_breaks <- seq(0, 1, length.out = 31)
      }
      params_histogram_set <- list(yscale = 0.2, colour = "black", fill = NA)
      if (!is.null(RelDiag_breaks)) {
        params_histogram_set$breaks <- RelDiag_breaks
      }

      ### Histograms with the same area
      r_cases <- lapply(object$RelDiag, function(l) l$cases) %>%
        dplyr::bind_rows(.id = "forecast")

      # maximum histogram value for equal histogram areas
      max_density <- r_cases %>%
        group_by(forecast) %>%
        summarize(density = hist(x, RelDiag_breaks, plot = FALSE)$density) %>%
        pull(density) %>%
        max()

      # Plot
      autoplot(object$RelDiag, params_diagonal = NA, params_CEPline = NA) +
        geom_histogram(
          mapping = aes(
            x = x,
            y = RelDiag_hist_factor / max_density * after_stat(density)
          ),
          data = r_cases,
          breaks = RelDiag_breaks,
          colour = "black",
          fill = NA
        ) +
        autolayer(
          object = object$RelDiag,
          params_ribbon = list(fill = "grey50", alpha = 0.3),
          params_diagonal = NULL,
          params_CEPline = list(size = plot_linewidth)
        ) +
        facet_wrap(. ~ factor(forecast, levels = FC_names), nrow = nr_rows)
    }
  } +
    # Augment reliability diagram plot
    ggplot2::scale_color_manual(values = plot_cols, limits = FC_names) +
    ggplot2::scale_linetype_manual(values = plot_linetypes, limits = FC_names) +
    ggtitle("Reliability Diagram") +
    plot_theme +
    coord_cartesian(xlim = Murphy_RelDiag_range) +
    scale_y_continuous(breaks = c(0, 0.5, 1)) +
    scale_x_continuous(breaks = c(x_range[1], mean(x_range), x_range[2])) +
    theme(
      strip.background = element_blank(),
      strip.text.x = element_blank(),
      legend.title = element_text(size = size_legend),
      legend.text = element_text(size = size_legend),
      plot.title = element_text(hjust = 0.5, size = size_title),
      axis.title = element_text(size = size_axislabels),
      axis.text.x = element_text(size = size_axisticks),
      axis.text.y = element_text(size = size_axisticks),
      legend.position = "bottom",
      legend.key.size = grid::unit(2, "lines"),
      aspect.ratio = 1
    ) +
    guides(
      colour = guide_legend(paste(plot_legend_title), nrow = 1),
      linetype = guide_legend(paste(plot_legend_title), nrow = 1)
    ))

  ###############################################################################################
  ### Plot ROC Curves
  p_ROC <- quote({
    df_roc <- object$roc
    if (PAV_ROC) {
      df_roc <- filter(df_roc, PAV)
    }
    proc = ggplot2::ggplot(df_roc, aes(x = 1 - specificities, y = sensitivities)) +
      geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), linewidth = plot_linewidth / 3, colour = "black") +
      geom_path(aes(color = forecast, linetype = forecast), linewidth = plot_linewidth)
    if(confidence == TRUE){
      polygon_data = data.frame(forecast = c(df_roc$forecast,rev(df_roc$forecast)), sens = c(df_roc$sensitivities_upper,rev(df_roc$sensitivities_lower)), spec = c(df_roc$specificities_upper,rev(df_roc$specificities_lower)))
      proc = proc + geom_polygon(data = polygon_data, aes(x = 1 - spec, y = sens, fill = forecast), alpha = 0.1, show.legend = FALSE) +
        facet_wrap(. ~ factor(forecast, levels = FC_names), nrow = nr_rows) +
        scale_x_continuous(breaks = c(0,0.5,1)) +
        scale_y_continuous(breaks = c(0,0.5,1)) +
        scale_fill_manual(values = plot_cols)
    }
    proc = proc +
      ggplot2::scale_color_manual(values = plot_cols) +
      ggplot2::scale_linetype_manual(values = plot_linetypes) +
      xlab("FAR") +
      ylab("HR") +
      ggtitle("ROC Curve") +
      plot_theme +
      theme(
        legend.title = element_text(size = size_legend),
        legend.text = element_text(size = size_legend),
        plot.title = element_text(hjust = 0.5, size = size_title),
        axis.title = element_text(size = size_axislabels),
        axis.text.x = element_text(size = size_axisticks),
        axis.text.y = element_text(size = size_axisticks),
        legend.position = "bottom",
        legend.key.size = grid::unit(2, "lines"),
        aspect.ratio = 1,
        strip.background = element_blank(),
        strip.text.x = element_blank()
      ) +
      guides(
        colour = guide_legend(paste(plot_legend_title), nrow = 1),
        linetype = guide_legend(paste(plot_legend_title), nrow = 1)
      )
    if(confidence == TRUE){
      polygon_data = data.frame(forecast = c(df_roc$forecast,rev(df_roc$forecast)), sens = c(df_roc$sensitivities_upper,rev(df_roc$sensitivities_lower)), spec = c(df_roc$specificities_upper,rev(df_roc$specificities_lower)))
      proc = proc + geom_polygon(data = polygon_data, aes(x = 1 - spec, y = sens, fill = forecast), alpha = 0.4, show.legend = FALSE) +
        facet_wrap(. ~ factor(forecast, levels = FC_names), nrow = nr_rows) +
        theme(
          strip.background = element_blank(),
          strip.text.x = element_blank()
        ) +
        scale_x_continuous(breaks = c(0,0.5,1)) +
        scale_y_continuous(breaks = c(0,0.5,1))
      }
    proc
  })



  ###############################################################################################
  ### Plot the Murphy Diagram

  p_Murphy <- quote({
    # Choices which score component should be plotted
    score_type_used <- switch(Murphy_scoretype,
      "score" = "elem_score",
      "recalibrated" = "elem_score_recal",
      "MCB-DSC" = "elem_MCBmDSC",
      "MCB" = "elem_MCB",
      "DSC" = "elem_DSC"
    )
    # Murphy y-lab
    Murphy_ylabel <- switch(Murphy_scoretype,
      "score" = "Mean elementary score",
      "recalibrated" = "Mean elementary recal. score",
      "MCB-DSC" = "Mean elementary MCB-DSC",
      "MCB" = "Mean elementary MCB",
      "DSC" = "Mean elementary DSC"
    )

    # Plot Murphy diagrams relative to a benchmark?
    Murphy_trans <- object$Murphy
    if (Murphy_benchmark %in% FC_names) {
      Murphy_trans <- object$Murphy %>%
        mutate(
          elem_score = elem_score - filter(., forecast == Murphy_benchmark)$elem_score,
          elem_MCB = elem_MCB - filter(., forecast == Murphy_benchmark)$elem_MCB,
          elem_DSC = elem_DSC - filter(., forecast == Murphy_benchmark)$elem_DSC,
          elem_MCBmDSC = elem_MCBmDSC - filter(., forecast == Murphy_benchmark)$elem_MCBmDSC
        )
    }

    # Murphy plot
    murphy = ggplot2::ggplot(
      data = Murphy_trans,
      mapping = aes(
        x = theta,
        y = get(paste(score_type_used)),
        color = forecast,
        linetype = forecast
      )
    ) +
      geom_line(linewidth = plot_linewidth)
    if(confidence == TRUE ){
      murphy = murphy + geom_ribbon(aes(x = theta, ymin = elem_score_lower, ymax = elem_score_upper, fill = after_scale(color)), alpha = 0.4,show.legend = FALSE) +
        facet_wrap(. ~ factor(forecast, levels = FC_names), nrow = nr_rows) +
        scale_x_continuous(breaks = c(0,0.5,1))
    }
    murphy = murphy +
      ggplot2::scale_color_manual(values = plot_cols) +
      ggplot2::scale_linetype_manual(values = plot_linetypes) +
      xlab(expression("Threshold " * theta)) +
      ylab(Murphy_ylabel) +
      ggtitle("Murphy Curve") +
      coord_cartesian(xlim = Murphy_RelDiag_range) +
      plot_theme +
      theme(
        legend.position = "bottom",
        legend.key.size = grid::unit(2, "lines"),
        legend.title = element_text(size = size_legend),
        legend.text = element_text(size = size_legend),
        plot.title = element_text(hjust = 0.5, size = size_title),
        axis.title = element_text(size = size_axislabels),
        axis.text.x = element_text(size = size_axisticks),
        axis.text.y = element_text(size = size_axisticks),
        aspect.ratio = 1,
        strip.background = element_blank(),
        strip.text.x = element_blank()
      ) +
      guides(
        colour = guide_legend(paste(plot_legend_title), nrow = 1),
        linetype = guide_legend(paste(plot_legend_title), nrow = 1)
      )
    murphy
  })


  ###############################################################################################
  ### Plot the MCBDSC Plot Options:
  # - Automatic (manual through input) selection of axis limits
  # - Automatic selection of the number of isolines: exact implementation via 'pretty' function
  # - Infinite score (MCB forecasts) are plotted on the right axis with rugs.

  p_MCBDSC <- quote({
    MCB_lim <- MCBDSC_MCB_xlim
    DSC_lim <- MCBDSC_DSC_ylim
    # Score decomposition
    score_decomp <- summary(object$RelDiag, score = MCBDSC_score) %>%
      tibble::as_tibble() %>%
      rename(
        MCB = miscalibration,
        DSC = discrimination,
        UNC = uncertainty
      )


    # Set to default values if NA
    pick_limits <- function(x){
      a = ifelse(confidence, 1.3,1.1)
      c(0, a * max(x[is.finite(x)]))
    }

    if (anyNA(MCB_lim)) {
      MCB_lim <- pick_limits(score_decomp$MCB)
    }
    if (anyNA(DSC_lim)) {
      DSC_lim <- pick_limits(score_decomp$DSC)
    }

    # Assign labels "within", "outside", and "infinity" depending on where the points are!
    is_in_range <- function(x, xrange) x >= xrange[1] & x <= xrange[2]
    expr1 <- quote({
      dplyr::mutate(
        .data = score_decomp,
        DSC_type = ifelse(is_in_range(DSC, DSC_lim), "within", "outside"),
        MCB_type = ifelse(!is_in_range(MCB, MCB_lim) & !is.finite(MCB), "infty", "within"),
        x_geom_text = ifelse(MCB_type == "infty", MCB_lim[2], MCB),
        is_within = DSC_type == "within" & MCB_type == "within"
      )
    })
    df_MCBDSC <- eval(expr1)

    # Check that the plot is not empty of points!
    if (!length(df_MCBDSC$is_within)) {
      warning(paste(
        "The given limits for the MCB-DSC plot exclude all forecasts.",
        "The default choices are used instead."
      ))
      MCB_lim <- pick_limits(score_decomp$MCB)
      DSC_lim <- pick_limits(score_decomp$DSC)
      df_MCBDSC <- eval(expr1)
    }

    # Reasonable score values for isolines
    unc <- df_MCBDSC$UNC[1]
    if (any(abs(df_MCBDSC$UNC - unc) / unc > 1e-8)) {
      warning("All 'uncertainty' values in the score decomposition should be equal.")
    }
    df_abline <- data.frame(
      slope = 1,
      score = pretty(
        x = unc - c(-1.1 * MCB_lim[2], DSC_lim[2]),
        n = MCBDSC_lines
      )
    ) %>%
      mutate(intercept = unc - score, label = score)



    # Remove a line if its intercept is too close to zero (less than 1/5 times the line distance).
    line_distance <- -diff(df_abline$intercept)[1]
    if (min(abs(df_abline$intercept)) < line_distance / 5) {
      df_abline <- df_abline %>% slice(-which.min(abs(intercept)))
    }

    # df_UNC for the zero DSC (or UNC) line
    df_UNC <- tibble(
      intercept = 0,
      slope = 1,
      label = paste("UNC:", prettyNum(unc, digits = 3))
    )

    # MCB-DSC points
    df_MCBDSC <- arrange(df_MCBDSC, mean_score, DSC)
    df_MCBDSC_within <- filter(df_MCBDSC, is_within)
    df_MCBDSC_MCBinfty <- filter(df_MCBDSC, MCB_type == "infty")

    # replicate MCBDSC_point_cols if it has length 1
    if (length(MCBDSC_point_cols) == 1L & nrow(df_MCBDSC) > 1L) {
      MCBDSC_point_cols <- rep(MCBDSC_point_cols, nrow(df_MCBDSC))
    }

    # Report a message for the missings in the MCBDSC Plot
    FCs_missing <- score_decomp %>%
      dplyr::filter(MCB > MCB_lim[2]) %>%
      dplyr::pull(forecast)
    if (plot_type == "MCBDSC" & length(FCs_missing) > 0) {
      message(paste(
        "The following forecasts are not included in the MCB-DSC plot as their",
        "miscalibration measure is outside the plot limits:",
        paste(FCs_missing, collapse = ", ")
      ))
    }
    if(confidence == TRUE){
      compute_r = function(theta,value,coma){
        r_2 = (value^2 * det(coma))/(coma[2,2] * cos(theta)^2 + coma[1,1] * sin(theta)^2 - 2 * sin(theta) * cos(theta) * coma[1,2])
        return(sqrt(r_2))
      }
      generate_mahalonibus_ellipse = function(samples,center, confidence = 0.5){
        sample_m = data.matrix(samples[,c("MCB_S","DSC_S")])
        coma = cov(sample_m)
        dist_quant = sqrt(as.numeric(quantile(mahalanobis(x = sample_m,center = center,cov = coma),confidence)))
        polar_coord = data.frame(theta = seq(0, 2 * pi, length.out = 1000))
        polar_coord$r = compute_r(polar_coord$theta, value = dist_quant, coma = coma)
        ellipse_df = data.frame(x = center[1] + cos(polar_coord$theta) * polar_coord$r, y = center[2] + sin(polar_coord$theta) * polar_coord$r)
        return(ellipse_df)
      }
      ellipse_df = tibble()
      for(name in object$FC_names){
        center = df_MCBDSC_within[df_MCBDSC_within$forecast == name, c("MCB", "DSC")]
        temp = generate_mahalonibus_ellipse(object$mcb_dsc_samples |> subset(forecast == name),center =  unlist(center))
        temp$forecast = name
        ellipse_df = rbind(ellipse_df,temp)
      }
    }

    # MCB-DSC plot
    mcbdsc = ggplot() +
      geom_segment(
        data = tibble(max_val = 2 * max(MCB_lim, DSC_lim)),
        mapping = aes(x = 0, y = 0, xend = max_val, yend = max_val),
        colour = gg_color_hue(5)[3],
        linewidth = 1
      ) +
      geom_point(
        mapping = aes(x = 0, y = 0),
        colour = gg_color_hue(5)[3],
        fill = gg_color_hue(5)[3],
        size = 2,
        shape = 15
      ) +
      geomtextpath::geom_labelabline(
        mapping = aes(
          intercept = 0,
          slope = 1,
          label = paste("UNC:", prettyNum(unc, digits = 3))
        ),
        colour = gg_color_hue(5)[3],
        hjust = MCBDSC_UNC_hjust,
        size = 7 * 0.36,
        text_only = TRUE,
        boxcolour = NA,
        straight = TRUE
      ) +
      geom_abline(
        data = df_abline,
        mapping = aes(intercept = intercept, slope = slope),
        colour = "gray50"
      ) +
      geomtextpath::geom_labelabline(
        data = df_abline,
        mapping = aes(intercept = intercept, slope = slope, label = label),
        colour = "gray50",
        hjust = 0.85,
        size = 7 * 0.36,
        text_only = TRUE,
        boxcolour = NA,
        straight = TRUE
      ) +
      geom_point(
        data = df_MCBDSC_within,
        mapping = aes(x = MCB, y = DSC, colour = forecast)
      ) +
      {
        if (MCBDSC_repel == FALSE) {
          geom_text(
            data = df_MCBDSC_within,
            mapping = aes(x = MCB, y = DSC, label = forecast, colour = forecast),
            size = 3,
            vjust = 0,
            hjust = 0,
            check_overlap = TRUE,
            position = position_nudge(
              x = diff(MCB_lim) / 80,
              y = -diff(DSC_lim) / 40
            )
          )
        } else if (MCBDSC_repel == TRUE) {
          ggrepel::geom_text_repel(
            data = df_MCBDSC_within,
            mapping = aes(x = MCB, y = DSC, label = forecast, colour = forecast),
            size = 3
          )
        }
      } +
      geom_rug(
        data = df_MCBDSC_MCBinfty,
        aes(x = MCB, y = DSC, colour = forecast),
        sides = "r",
        linewidth = 2
      ) +
      geom_text(
        data = df_MCBDSC_MCBinfty,
        aes(x = x_geom_text, y = DSC, label = forecast, colour = forecast),
        size = 3,
        hjust = 1,
        check_overlap = TRUE
      ) +
      #scale_colour_manual(values = MCBDSC_point_cols) +
      scale_x_continuous(oob = scales::oob_squish_infinite) +
      coord_cartesian(xlim = MCB_lim, ylim = DSC_lim) +
      labs(x = "MCB", y = "DSC") +
      plot_theme +
      theme(
        legend.position = "none",
        axis.title = element_text(size = size_axislabels),
        axis.text.x = element_text(size = size_axisticks),
        axis.text.y = element_text(size = size_axisticks),
        aspect.ratio = 1,
        panel.border = element_rect(
          colour = "black",
          fill = NA,
          linewidth = 1
        )
      )
    if(confidence == TRUE){
      mcbdsc = mcbdsc + geom_path(data = ellipse_df, aes(x = x, y = y, colour = forecast))
    }
    mcbdsc
  })


  ###############################################################################################
  ### Arrange Plots

  # Comment: For the "RelDiag_joint=FALSE" option, the legend and titles are on the top and bottom ends of the printed plot,
  # whenever the reliabilitydiags are not aligned in a square (or square with missings), i.e., when the following "if" condition is TRUE.
  # The problem is how to assign three plots with unequal relative width and a common legend.

  p_triptych <- quote({
    p_RelDiag <- eval(p_RelDiag)
    p_ROC <- eval(p_ROC)
    p_Murphy <- eval(p_Murphy)
    if (m %in% c(2, 5, 6) & RelDiag_joint == FALSE) {
      width <- ifelse(m == 2, 0.5, 1.5)
      (p_Murphy + ggplot2::theme(aspect.ratio = NULL)) +
        (p_RelDiag + ggplot2::theme(aspect.ratio = NULL) + theme(plot.margin = plot_margins)) +
        (p_ROC + ggplot2::theme(aspect.ratio = NULL)) +
        plot_layout(guides = "collect", width = c(1, width, 1)) & theme(legend.position = "bottom") +
        ggplot2::theme(aspect.ratio = 1)
    } else {
      (p_Murphy ) + (p_RelDiag +theme(plot.margin = plot_margins)) + (p_ROC ) +
        plot_layout(guides = "collect") & theme(legend.position = "bottom")
    }
  })

  switch(plot_type,
    "triptych" = eval(p_triptych),
    "ROC" = eval(p_ROC),
    "Murphy" = eval(p_Murphy),
    "ReliabilityDiagram" = eval(p_RelDiag),
    "MCBDSC" = eval(p_MCBDSC)
  )
}
