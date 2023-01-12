


#' Plot method for class triptych
#'
#' @param obj
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
plot.triptych <- function(obj, ...) {
  p <- autoplot(obj, ...)
  print(p)
}



#' @importFrom ggplot2 autoplot
#' @export
ggplot2::autoplot




#' Plots a triptych
#'
#' @param obj Object of class 'triptych'
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
autoplot.triptych <- function(obj,
                              plot_type="default",
                              PAV_ROC=TRUE,
                              RelDiag_joint=FALSE,
                              RelDiag_breaks=NULL,
                              RelDiag_hist_factor=0.2,
                              Murphy_scoretype="score",
                              Murphy_benchmark=NA,
                              Murphy_RelDiag_range=c(0,1),
                              MCBDSC_score="brier",
                              MCBDSC_lines=10,
                              MCBDSC_point_cols="red",
                              MCBDSC_repel=FALSE,
                              MCBDSC_MCB_xlim=NA,
                              MCBDSC_DSC_ylim=NA,
                              plot_cols=NA,
                              plot_linetypes="solid",
                              plot_linewidth=0.5,
                              plot_margins=grid::unit(c(0,0.02,0,0.02), "npc"),
                              plot_theme=ggplot2::theme_bw(),
                              plot_legend_title="Forecast",
                              size_title=14,
                              size_legend=12,
                              size_axislabels=12,
                              size_axisticks=10) {


  ### Preliminaries
  FC_names <- obj$FC_names
  m <- length(FC_names)

  # Set nice tick labels in the reliability diagram. First try, could be more elaborate
  Murphy_RelDiag_range_labels <- c(ceiling(10*Murphy_RelDiag_range[1])/10,
                                floor(10*Murphy_RelDiag_range[2])/10)

  # Specify which plot is returned and issue messages
  plot_type_choices <- c("default", "triptych", "ROC", "Murphy", "ReliabilityDiagram", "MCBDSC")
  plot_type1 <- match.arg(plot_type, plot_type_choices)
  if(plot_type1=="default"){
    if (m<=4) {
      plot_type1 <- "triptych"
      message("The triptych is plotted as default for no more than 4 forecasts." )
    } else {
      plot_type1 <- "MCBDSC"
      message(paste0("The MCB-DSC displkay is plotted as default for  more than 4 forecasts. \n Use the option plot.type=", "'", "triptych", "'", " to obtain a triptych nevertheless."))
    }
  }


  # Use a standard color palette if plot.cols is NA
  if (any(is.na(plot_cols)) | !(length(plot_cols) %in% c(1,m)) )   plot_cols <- gg_color_hue(m)
  if (length(plot_cols) == 1)   plot_cols <- rep(plot_cols, m)
  names(plot_cols) <- FC_names


  # Use standard linetypes if plot_linetypes is NA
  if (any(is.na(plot_linetypes)) | !(length(plot_linetypes) %in% c(1,m)) ) {
    plot_linetypes <- rep(c("solid", "dashed", "dotted", "twodash", "longdash", "dotdash"), ceiling(m/6))[1:m]
  }
  if (length(plot_linetypes) == 1)  plot_linetypes <- rep(plot_linetypes, m)
  names(plot_linetypes) <- FC_names


  ###############################################################################################
  ### Plot Reliability Diagrams

  # ToDo: Suppress the printed text "Scale for 'linetype' is already present" somehow. Alex, any ideas?
  if (RelDiag_joint==TRUE){
    p_RelDiag <- autoplot(obj$RelDiag,
                          params_CEPline=list(size = plot_linewidth))
  } else {
    # Determine number of rows in the plot
    nr_rows <- 1
    if (m >= 2) {nr_rows <- 2}
    if (m >= 7) {nr_rows <- 3}

    # IMPROVE: this is an ugly fix to externally fix the breaks of the histograms.
    # But something more flexible should be added, maybe even on the level of the reliabilitydiag package?
    if (is.null(RelDiag_breaks) == TRUE){params_histogram_set = list(yscale = 0.2, colour = "black", fill = NA)}
    else {params_histogram_set = list(yscale = 0.2, colour = "black", fill = NA, breaks=RelDiag_breaks)}


    ### Old version without histograms with the same area in one facet
    # p_RelDiag <- autoplot(obj$RelDiag,
    #                       region.level = 0.9,
    #                       params_ribbon = list(fill = "grey50", alpha = 0.3),
    #                       params_CEPline=list(size = plot_linewidth),
    #                       # params_histogram=list(yscale = 0.2, colour = "black", fill = NA),
    #                       params_histogram=params_histogram_set) +
    #   facet_wrap(.~factor(forecast, levels=FC_names), nrow=nr_rows) +
    #   theme(strip.background = element_blank(),
    #         strip.text.x = element_blank())


    ### Histograms with the same area
    r_cases <- dplyr::bind_rows(lapply(obj$RelDiag, function(l) l$cases), .id = "forecast")

    # Manual histogram computations to set a good default
    hist_manual <- r_cases %>%
      group_by(forecast) %>%
      summarize(breaks = tail(RelDiag_breaks,-1),
                bin_height = cut(x, breaks=RelDiag_breaks, include.lowest=TRUE) %>% table() %>% as.numeric() / (n()* diff(RelDiag_breaks)))


    # Plot
    p_RelDiag <- autoplot(obj$RelDiag, params_diagonal = NA, params_CEPline = NA) +
      geom_histogram(aes(x, y = RelDiag_hist_factor/max(hist_manual$bin_height)*after_stat(density)),
                     data = r_cases,
                     breaks=RelDiag_breaks,
                     colour = "black",
                     fill = NA) +
      autolayer(
        object = obj$RelDiag,
        params_ribbon = list(fill = "grey50", alpha = 0.3),
        params_diagonal = NULL,
        params_CEPline=list(size = plot_linewidth)) +
      facet_wrap(.~factor(forecast, levels=FC_names), nrow=nr_rows) +
      theme(strip.background = element_blank(),
            strip.text.x = element_blank())
  }

  # Augment reliability diagram plot
  p_RelDiag <- p_RelDiag +
    ggplot2::scale_color_manual(values = plot_cols, limits=FC_names) +
    ggplot2::scale_linetype_manual(values = plot_linetypes, limits=FC_names) +
    ggtitle("Reliability Diagram") +
    plot_theme +
    coord_cartesian(xlim = Murphy_RelDiag_range) +
    scale_y_continuous(breaks = c(0,0.5,1)) +
    scale_x_continuous(breaks = c(Murphy_RelDiag_range_labels[1],mean(Murphy_RelDiag_range_labels),Murphy_RelDiag_range_labels[2])) +
    theme(strip.background = element_blank(),
          strip.text.x = element_blank(),
          legend.title = element_text(size=size_legend),
          legend.text = element_text(size=size_legend),
          plot.title = element_text(hjust = 0.5, size = size_title),
          axis.title = element_text(size=size_axislabels),
          axis.text.x = element_text(size=size_axisticks),
          axis.text.y = element_text(size=size_axisticks),
          legend.position="bottom",
          legend.key.size = grid::unit(2, "lines"),
          aspect.ratio = 1) +
    guides(colour = guide_legend(paste(plot_legend_title), nrow = 1),
           linetype = guide_legend(paste(plot_legend_title), nrow = 1))



  ###############################################################################################
  ### Plot ROC Curves
  p_ROC <- ggplot2::ggplot(obj$roc %>% dplyr::filter(PAV==PAV_ROC), aes(x=1-specificities, y=sensitivities)) +
    geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), size=plot_linewidth/3, colour="black") +
    geom_path(aes(col=forecast, linetype=forecast), size=plot_linewidth) +
    ggplot2::scale_color_manual(values = plot_cols) +
    ggplot2::scale_linetype_manual(values = plot_linetypes) +
    xlab("FAR") +
    ylab("HR") +
    ggtitle("ROC Curve") +
    plot_theme +
    theme(legend.title = element_text(size=size_legend),
          legend.text = element_text(size=size_legend),
          plot.title = element_text(hjust = 0.5, size = size_title),
          axis.title = element_text(size=size_axislabels),
          axis.text.x = element_text(size=size_axisticks),
          axis.text.y = element_text(size=size_axisticks),
          legend.position="bottom",
          legend.key.size = grid::unit(2, "lines"),
          aspect.ratio = 1) +
    guides(colour = guide_legend(paste(plot_legend_title), nrow = 1),
           linetype = guide_legend(paste(plot_legend_title), nrow = 1))


  ###############################################################################################
  ### Plot the Murphy Diagram

  # Choices which score component should be plotted
  Murphy_scoretype_choices <- c("MCB-DSC", "score", "MCB", "DSC", "recalibrated")
  Murphy_scoretype1 <- match.arg(Murphy_scoretype, Murphy_scoretype_choices)
  score_type_used <- switch(Murphy_scoretype1,
                            "score" = "elem_score",
                            "recalibrated" = "elem_score_recal",
                            "MCB-DSC" = "elem_MCBmDSC",
                            "MCB" = "elem_MCB",
                            "DSC" = "elem_DSC")


  # Plot Murphy diagrams relative to a benchmark?
  Murphy_trans <- obj$Murphy
  if (Murphy_benchmark %in% FC_names){
    Murphy_trans <- obj$Murphy %>%
      mutate(elem_score = elem_score - filter(.,forecast==Murphy_benchmark)$elem_score,
             elem_MCB = elem_MCB - filter(.,forecast==Murphy_benchmark)$elem_MCB,
             elem_DSC = elem_DSC - filter(.,forecast==Murphy_benchmark)$elem_DSC,
             elem_MCBmDSC = elem_MCBmDSC - filter(.,forecast==Murphy_benchmark)$elem_MCBmDSC)
  }

  # Murphy y-lab
  Murphy_ylabel <- switch(Murphy_scoretype1,
                          "score" = "Mean elementary score",
                          "recalibrated" = "Mean elementary recal. score",
                          "MCB-DSC" = "Mean elementary MCB-DSC",
                          "MCB" = "Mean elementary MCB",
                          "DSC" = "Mean elementary DSC")

  # Murphy plot
  p_Murphy <- ggplot(Murphy_trans, aes(x=theta, y=get(paste(score_type_used)), color=forecast, linetype=forecast)) +
    geom_line(size=plot_linewidth) +
    ggplot2::scale_color_manual(values = plot_cols) +
    ggplot2::scale_linetype_manual(values = plot_linetypes) +
    xlab(expression("Parameter "*theta)) +
    ylab(Murphy_ylabel) +
    ggtitle("Murphy Curve") +
    coord_cartesian(xlim = Murphy_RelDiag_range) +
    plot_theme +
    theme(legend.position = "bottom",
          legend.key.size = grid::unit(2, "lines"),
          legend.title = element_text(size=size_legend),
          legend.text = element_text(size=size_legend),
          plot.title = element_text(hjust = 0.5, size = size_title),
          axis.title = element_text(size=size_axislabels),
          axis.text.x = element_text(size=size_axisticks),
          axis.text.y = element_text(size=size_axisticks),
          aspect.ratio = 1) +
    guides(colour = guide_legend(paste(plot_legend_title), nrow = 1),
           linetype = guide_legend(paste(plot_legend_title), nrow = 1))



  ###############################################################################################
  ### Plot the MCBDSC Plot Options:
  # - Automatic (manual through input) selection of axis limits
  # - Automatic selection of the number of isolines: exact implementation via 'pretty' function
  # - Infinite score (MCB forecasts) are plotted on the right axis with rugs.

  # Score decomposition
  score_decomp <- summary(obj$RelDiag, score=MCBDSC_score) %>%
    tibble::as_tibble()

  # Set to default values if NA
  if(is.na(tail(MCBDSC_MCB_xlim,1))){MCBDSC_MCB_xlim <- c(0, 1.1*max(score_decomp$miscalibration[is.finite(score_decomp$miscalibration)]))}
  if(is.na(tail(MCBDSC_DSC_ylim,1))){MCBDSC_DSC_ylim <- c(0, 1.1*max(score_decomp$discrimination[is.finite(score_decomp$discrimination)]))}

  # Assign labels "within", "outside", and "infinity" depending on where the points are!
  FCs_MCBDSC_plot <- score_decomp %>%
    dplyr::mutate(DSC_type = ifelse(discrimination >= MCBDSC_DSC_ylim[1] & discrimination <= MCBDSC_DSC_ylim[2], "within", "outside"),
                  MCB_type = ifelse(miscalibration >= MCBDSC_MCB_xlim[1] & miscalibration <= MCBDSC_MCB_xlim[2], "within",
                                    ifelse(is.finite(miscalibration), "within", "infty")),
                  x_geom_text = ifelse(MCB_type=="infty", MCBDSC_MCB_xlim[2], miscalibration))


  # Check that the plot is not empty of points!
  if (nrow(FCs_MCBDSC_plot)==0){
    warning("The given limits for the MCB-DSC plot exclude all forecasts. The default choices are used instead.")

    MCBDSC_MCB_xlim <- c(0, 1.1*max(score_decomp$miscalibration[is.finite(score_decomp$miscalibration)]))
    MCBDSC_DSC_ylim <- c(0, 1.1*max(score_decomp$discrimination[is.finite(score_decomp$discrimination)]))

    FCs_MCBDSC_plot <- score_decomp %>%
      dplyr::mutate(DSC_type = ifelse(discrimination >= MCBDSC_DSC_ylim[1] & discrimination <= MCBDSC_DSC_ylim[2], "within", "outside"),
                    MCB_type = ifelse(miscalibration >= MCBDSC_MCB_xlim[1] & miscalibration <= MCBDSC_MCB_xlim[2], "within",
                                      ifelse(is.finite(miscalibration), "within", "infty")),
                    x_geom_text =ifelse(MCB_type=="infty", MCBDSC_MCB_xlim[2], miscalibration))
  }


  # Reasonable score values for isolines
  df_abline <- data.frame(slope = 1,
                          score = pretty(unique(FCs_MCBDSC_plot$uncertainty) - c(-1.1*MCBDSC_MCB_xlim[2], MCBDSC_DSC_ylim[2]), n=MCBDSC_lines)) %>%
    mutate(intercept = unique(FCs_MCBDSC_plot$uncertainty) - score,
           label=score)


  # replicate MCBDSC_point_cols if it has length 1
  if (length(MCBDSC_point_cols)==1 & nrow(FCs_MCBDSC_plot) > 1){MCBDSC_point_cols <- rep(MCBDSC_point_cols,nrow(FCs_MCBDSC_plot))}


  # MCB-DSC plot
  p_MCBDSC <- ggplot(data=FCs_MCBDSC_plot
                     %>% arrange(mean_score, discrimination)) +
    geom_abline(data=df_abline,
                aes(intercept=intercept, slope=slope), colour="gray50") +
    geomtextpath::geom_labelabline(data = df_abline,
                                   aes(intercept = intercept, slope = slope, label = label),
                                   colour="gray50", hjust = 0.85, size = 7 * 0.36, text_only = TRUE, boxcolour = NA, straight = TRUE) +
    geom_point(data=.%>%filter(DSC_type=="within", MCB_type=="within"),
               aes(x=miscalibration, y=discrimination, colour=forecast)) +
    {if(MCBDSC_repel==FALSE) geom_text(data=.%>%filter(DSC_type=="within", MCB_type=="within"),
              aes(x=miscalibration, y=discrimination, label=forecast, colour=forecast),
              size=3, vjust=0, hjust=0, check_overlap=TRUE,
              position = position_nudge(x=diff(MCBDSC_MCB_xlim)/80, y = -diff(MCBDSC_DSC_ylim)/40)) } +
    {if(MCBDSC_repel==TRUE) ggrepel::geom_text_repel(data=.%>%filter(DSC_type=="within", MCB_type=="within"),
                                                     aes(x=miscalibration, y=discrimination, label=forecast, colour=forecast), size=3)} +
    geom_rug(data=.%>%filter(MCB_type=="infty"),
             aes(x=miscalibration, y=discrimination, colour=forecast),
             sides = "r", size=2) +
    geom_text(data=.%>%filter(MCB_type=="infty"),
              aes(x=x_geom_text, y=discrimination, label=forecast, colour=forecast),
              size=3, hjust=1, check_overlap=TRUE) +
    scale_colour_manual(values=MCBDSC_point_cols) +
    scale_x_continuous(oob=scales::oob_squish_infinite) +
    coord_cartesian(xlim = MCBDSC_MCB_xlim,
                    ylim = MCBDSC_DSC_ylim) +
    xlab("MCB") +
    ylab("DSC") +
    plot_theme +
    theme(legend.position = "none",
          axis.title = element_text(size=size_axislabels),
          axis.text.x = element_text(size=size_axisticks),
          axis.text.y = element_text(size=size_axisticks),
          aspect.ratio=1,
          panel.border = element_rect(colour = "black", fill=NA, size=1))


  # Report a message for the missings in the MCBDSC Plot
  FCs_missing <- score_decomp %>%
    dplyr::filter(miscalibration > MCBDSC_MCB_xlim[2]) %>%
    dplyr::pull(forecast)
  if (plot_type1=="MCBDSC" & length(FCs_missing) > 0){
    message(paste0("The following forecasts are not included in the MCB-DSC plot as their miscalibration measure is outside the plot limits: ", paste(FCs_missing, collapse = ', ')))
  }




  ###############################################################################################
  ### Arrange Plots

  # Comment: For the "RelDiag_joint=FALSE" option, the legend and titles are on the top and bottom ends of the printed plot,
  # whenever the reliabilitydiags are not aligned in a square (or square with missings), i.e., when the following "if" condition is TRUE.
  # The problem is how to assign three plots with unequal relative width and a common legend.

  if (m %in% c(2,5,6) & RelDiag_joint==FALSE){
    width <- ifelse(m==2, 0.5, 1.5)
    p_triptych <- (p_Murphy + ggplot2::theme(aspect.ratio = NULL)) +
      (p_RelDiag + ggplot2::theme(aspect.ratio = NULL) + theme(plot.margin = plot_margins)) +
      (p_ROC + ggplot2::theme(aspect.ratio = NULL)) +
      plot_layout(guides = "collect", width=c(1,width,1)) & theme(legend.position = 'bottom') +
      ggplot2::theme(aspect.ratio = 1)
  } else {
    p_triptych <- p_Murphy + (p_RelDiag + theme(plot.margin=plot_margins)) + p_ROC +
      plot_layout(guides = "collect") & theme(legend.position = 'bottom')
  }


  p <- switch(plot_type1,
              "triptych" = p_triptych,
              "ROC" = p_ROC,
              "Murphy" = p_Murphy,
              "ReliabilityDiagram" = p_RelDiag,
              "MCBDSC" = p_MCBDSC)

  p
}

