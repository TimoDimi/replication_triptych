

# devtools::install_github("janverbakel/ClassificationPlot")
library(ClassificationPlot)

# simulated data from the Verbakel (2020) supplement
X = replicate(4, rnorm(5e2))
p0true = binomial()$linkinv(cbind(1, X)%*%c(0.1, 0.5, 1.2, -0.75, 0.8))
y = rbinom(5e2, 1, p0true)
Df = data.frame(y,X)

FitLog = glm(y~., Df, family=binomial)
Pred = binomial()$linkinv(cbind(1, X)%*%coef(FitLog))
Df2 = cbind.data.frame(Pred = Pred, Outcome = y)


ClassificationPlot(Pred, outcome = Outcome, data=Df2)

FitLog2 = glm(y~., Df[,1:3], family=binomial)
Pred2 = binomial()$linkinv(cbind(1, X[,1:2])%*%coef(FitLog2))
Df3 = cbind.data.frame(Model1 = Pred, Model2 = Pred2, Outcome = y)

#' Classification plot
ClassificationPlot(Model1, Model2, Outcome, Df3)
ClassificationPlot(Model1, Model2, Outcome, Df3, RiskSet = "both")
ClassificationPlot(Model1, Model2, Outcome, Df3, RiskSet = "both", UtilityMeasures = "utility")



### test classification plot in our applications:


###########################################################################
###    C1 Flares
###########################################################################

library(triptych)
load("applications/data/SF.FC.M1.rda")
load("applications/data/SF.FC.C1.rda")

df_C1 <- SF.FC.C1 %>%
  as_tibble() %>%
  dplyr::select(c("rlz.C1", "NOAA", "DAFFS","SIDC","CLIM120")) %>%
  mutate_all(funs(replace(., .<0, NA))) %>%
  na.omit() %>%
  rename(y=rlz.C1)

# C1 Triptych
trpt_C1 <- triptych(df_C1)

p0 <- ClassificationPlot(pmin(DAFFS,0.999), SIDC, y, df_C1)
ClassificationPlot(NOAA, CLIM120, y, df_C1)


plot(ecdf(df_C1 %>% filter(y==0) %>% pull(SIDC)))
plot(ecdf(df_C1 %>% filter(y==1) %>% pull(SIDC)))


ecdf_fun_0 <- ecdf(df_C1 %>% filter(y==0) %>% pull(SIDC))
plot(ecdf_fun_0(df_C1 %>% filter(y==0) %>% pull(SIDC) %>% sort()))

ecdf_fun_1 <- ecdf(df_C1 %>% filter(y==1) %>% pull(SIDC))
ecdf_obj$



ClassificationPlot_own <- function(m1,m2,y,df, thresholds=seq(0,1,length.out=101)){
  ecdf_m1_0 <- ecdf(df %>% filter(y==0) %>% pull(paste(m1)))
  ecdf_m1_1 <- ecdf(df %>% filter(y==1) %>% pull(paste(m1)))
  ecdf_m2_0 <- ecdf(df %>% filter(y==0) %>% pull(paste(m2)))
  ecdf_m2_1 <- ecdf(df %>% filter(y==1) %>% pull(paste(m2)))

  df_plot <- rbind(data.frame(t=thresholds, model=m1, outcome=0, values=ecdf_m1_0(thresholds)),
                   data.frame(t=thresholds, model=m1, outcome=1, values=ecdf_m1_1(thresholds)),
                   data.frame(t=thresholds, model=m2, outcome=0, values=ecdf_m2_0(thresholds)),
                   data.frame(t=thresholds, model=m2, outcome=1, values=ecdf_m2_1(thresholds))) %>%
    mutate(outcome=as.factor(outcome))

  p <- ggplot(df_plot, aes(x=t, y=values)) +
    geom_line(aes(color=model, linetype=outcome)) +
    theme_bw() +
    theme(legend.position = "bottom")

  plot(p)
  invisible(list(p, df_plot))
}




