
ClassificationPlot_own <- function(model_list,y,df, thresholds=seq(0,1,length.out=101)){
  df_plot <- data.frame()

  for (model_name in model_list){
    ecdf_0 <- ecdf(df %>% filter(y==0) %>% pull(model_name))
    ecdf_1 <- ecdf(df %>% filter(y==1) %>% pull(model_name))

    df_plot <- rbind(df_plot,
                     data.frame(t=thresholds, forecast=model_name, outcome=0, values=ecdf_0(thresholds)),
                     data.frame(t=thresholds, forecast=model_name, outcome=1, values=ecdf_1(thresholds)))
  }

  df_plot <- df_plot %>%
    mutate(outcome=as.factor(outcome))

  p <- ggplot(df_plot, aes(x=t, y=values)) +
    geom_line(aes(color=forecast, linetype=outcome)) +
    theme_bw() +
    theme(legend.position = "bottom")

  plot(p)
  invisible(list(p, df_plot))
}
