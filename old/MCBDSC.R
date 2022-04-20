
MCBDSC.plot <- function(df.score_decomp, max.slope=3, n.lines = 15, table.length=15) {

  df.score_decomp <- trpt.evict.full$score_decomposition
  n.lines <- 15
  max.slope <- 4
  table.length <- 20

  # Compute maximal x and y axis limits
  y.lim.max <- 1.05 * max(df.score_decomp$discrimination)
  x.lim.max <- 1.05 * min(max.slope * max(df.score_decomp$discrimination),
                         max(df.score_decomp$miscalibration))

  df.abline <- data.frame(slope=1,
                          intercept=seq(-1.1* x.lim.max,
                                        y.lim.max,
                                        length.out=n.lines))



  p.DSC.MSC <- ggplot(data=df.score_decomp) +
    theme_classic() +
    geom_abline(data=df.abline, aes(intercept=intercept, slope=slope, color=intercept)) +
    scale_colour_gradient(low = "white", high = "black", name="UNC - Brier Score", guide = "colourbar") +
    guides(color = guide_colourbar(barwidth = 15, barheight = 0.5)) +
    geom_point(aes(x=miscalibration, y=discrimination), col="red") +
    geom_text(aes(x=miscalibration, y=discrimination, label=forecast), size=3, vjust=-0.1, hjust=-0.1, check_overlap=TRUE) +
    theme(legend.position = "bottom",
          panel.border = element_rect(colour = "black", fill=NA, size=1))


  p.DSC.MSC <- p.DSC.MSC +
    ylim(c(0, y.lim.max)) +
    xlim(c(0, x.lim.max)) +
    theme(aspect.ratio=1,
          panel.border = element_rect(colour = "black", fill=NA, size=1))

  FCs.missing <- df.score_decomp %>%
    dplyr::filter(miscalibration > x.lim.max) %>%
    dplyr::pull(forecast)

  warning(paste0("The following forecasts are not included in the MCB-DSC plot as their miscalibration measure is outside the plot limits: ", paste(FCs.missing, collapse = ', ')))


  ### Test: Plot a table for the score decomposition
  df.decomp <- summary(RelDiag) %>%
    dplyr::rename(Forecast=forecast,
                  Score=mean_score,
                  MSC=miscalibration,
                  DSC=discrimination,
                  UNC=uncertainty)
  PowerOf10 <- df.decomp %>%
    select(-Forecast) %>%
    max() %>% log(.,10) %>% floor()
  df.decomp <- df.decomp %>%
    #     dplyr::mutate_at(vars(-("Forecast")), funs(. / 10^PowerOf10)) %>%
    dplyr::mutate_at(vars(-("Forecast")), round, max(3,3-PowerOf10))


  p.table <- df.score_decomp %>%
    dplyr::rename(Forecast=forecast,
                  Score=mean_score,
                  MSC=miscalibration,
                  DSC=discrimination,
                  UNC=uncertainty) %>%
    dplyr::mutate_at(vars(-("Forecast")), round, 4,) %>%
    dplyr::arrange(Score) %>%
    slice(1:min(table.length,nrow(.))) %>%
    gridExtra::tableGrob(theme=gridExtra::ttheme_default(core = list(fg_params=list(cex = 1)),
                                                         colhead = list(fg_params=list(cex = 1)),
                                                         rowhead = list(fg_params=list(cex = 1))),
                         rows=NULL)


  table <- p.table
  title <- textGrob("Score  Decomposition", gp=gpar(fontsize=20))
  footnote <- textGrob(paste0("Note: All values are multiplied by ", 10), x=0, hjust=0,
                       gp=gpar(fontface="italic"))
  padding <- unit(0.5,"line")
  table <- gtable_add_rows(table,
                           heights = grobHeight(title) + padding,
                           pos = 0)
  table <- gtable_add_rows(table,
                           heights = grobHeight(footnote)+ padding)
  table <- gtable_add_grob(table, list(title, footnote),
                           t=c(1, nrow(table)), l=c(1,1),
                           r=ncol(table))

  plot(table)


  plot(p.table)

  p.DSC.MSC + p.table +
    plot_layout(widths = c(1, 1), heights = 1)

  # Put together
  p.DSC.MSC + ggplotify::as.ggplot(p.table) +
    plot_layout(widths = c(1, 1), heights = 1)

  gridExtra::grid.arrange(p.DSC.MSC, p.table, nrow=1)

  return(list(DSCMSC=p.DSC.MSC, score_table=p.table, DSCMSC_Table=DSCMSC_Table))
}
