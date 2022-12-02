library(lubridate)
library(triptych)

load("applications/data/spf.gdp.long.rda")


# Find forecasters with many issued forecasts
spf.gdp.long %>%
  tibble::as_tibble() %>%
  group_by(ID,FC.Horizon) %>%
  summarize(n=n()) %>%
  arrange(desc(n)) %>%
  print(n=40)


SPF_individual <- 65

# Clean SPF forecast-realization tibble
h_set <- 0:4

SPF_clean <- spf.gdp.long %>%
  tibble::as_tibble() %>%
  dplyr::filter(ID %in% c(0, SPF_individual) & FC.Horizon %in% h_set) %>%
  dplyr::select(DATE.issued, DATE.FC.due, FC.Horizon, ID, Prob.Forecast, gdp.first.recess) %>%
  dplyr::mutate(ID = as.factor(ID))


################################################################################
# Part (A) Compare the SPF average on different forecast horizons

h_set_avg <- c(0,1,2,4)

# FCs in wide tibble format including the climatology
SPFavg_wide <- SPF_clean %>%
  dplyr::filter(ID==0, FC.Horizon %in% h_set_avg) %>%
  select(-c("DATE.issued", "ID")) %>%
  tidyr::pivot_wider(names_from = FC.Horizon, values_from = Prob.Forecast) %>%
  arrange(DATE.FC.due) %>%
  dplyr::filter(DATE.FC.due >= as_date("1973-04-01")) %>%  # Before that, even many NAs in the avg 4Q ahead forecasts!
  na.omit()

# Triptych and plot
trpt_SPFavg_horizons <- triptych(SPFavg_wide %>%
                                   dplyr::select(-DATE.FC.due) %>%
                                   rename(y=gdp.first.recess))

summary(trpt_SPFavg_horizons)

ggsave(paste0("applications/plots/SPF_Average_Horizons.pdf"),
       autoplot(trpt_SPFavg_horizons,
                plot_linetypes = "solid",
                plot_legend_title = "forecast horizon"),
       width=24, height=10.5, units="cm")




################################################################################
# Part (B): Compare SPF average, SPF #84 and a dynamic climatology on fixed horizons

# ToDo: Shouldnt all horizons have the same UNC?

# Compute a dynamic climatological forecast
n_rolling <- 20

SPF_Clim <- SPF_clean %>%
  dplyr::filter(FC.Horizon==0, ID==0) %>%
  arrange(DATE.issued) %>%
  dplyr::mutate(Climatology_expanding = cumsum(gdp.first.recess) / seq_along(gdp.first.recess),
                Climatology_rolling = slider::slide_sum(gdp.first.recess, before = n_rolling, after=-1, complete=T) / n_rolling) %>%
  na.omit() %>%
  select(c("DATE.issued","Climatology_expanding", "Climatology_rolling"))

# FCs in wide tibble format including the climatology
SPF_wide <- SPF_clean %>%
  tidyr::pivot_wider(names_from = ID, values_from = Prob.Forecast) %>%
  inner_join(SPF_Clim, by="DATE.issued")


# Filter data such that we only use Date.FC.due for which all horizon forecasts
# in h_set_plot are available!
h_set_plot <- c(1,2,4)

SPF_wide_complete_cases <- SPF_wide %>%
  dplyr::filter(FC.Horizon %in% h_set_plot) %>%
  na.omit() %>%
  arrange(DATE.FC.due) %>%
  group_by(DATE.FC.due) %>%
  mutate(n_horizons=n()) %>%
  dplyr::filter(n_horizons==length(h_set_plot)) %>%
  ungroup()


# Compare SPF average against individual forecasts
SPF_score_decomp <- tibble()
for (h in h_set_plot){
  df_SPF_trpt <- SPF_wide_complete_cases %>%
    dplyr::filter(FC.Horizon==h, DATE.FC.due >= as_date("1973-04-01")) %>%
    dplyr::rename(y=gdp.first.recess, "SPF Average"="0", "SPF #65"="65") %>%
    dplyr::select(-c("DATE.issued","DATE.FC.due", "FC.Horizon","n_horizons"))

  trpt_SPF <- triptych(df_SPF_trpt)
  SPF_score_decomp <- bind_rows(SPF_score_decomp,
                                summary(trpt_SPF$RelDiag) %>% mutate(h=h))

  ggsave(paste0("applications/plots/SPF_",h,"StepAhead.pdf"),
         autoplot(trpt_SPF,
                  Murphy_scoretype = "score",
                  plot_cols= gg_color_hue(3)[c(2,1,3)],
                  MCBDSC_maxslope=100),                     #ToDo: Edit this: this is currently a bug in the MCB-DSC plot function!
         width=24, height=10.5, units="cm")
}

SPF_score_decomp














### OLD


# Compare average SPF among forecasting horizons
h.set <- c(0,1,2,4)
forecaster.ID <- 0

df.SPF.horizons <- spf.gdp.long %>%
  tibble::as_tibble() %>%
  dplyr::filter(ID == forecaster.ID & FC.Horizon %in% h.set) %>%
  dplyr::select(DATE.FC.due, Prob.Forecast, gdp.first.recess, FC.Horizon) %>%
  dplyr::mutate(FC.Horizon = as.factor(FC.Horizon)) %>%
  dcast(DATE.FC.due + gdp.first.recess ~ FC.Horizon, value.var="Prob.Forecast") %>%
  tibble::as_tibble() %>%
  na.omit() %>%
  dplyr::rename(y=gdp.first.recess)
dim(df.SPF.horizons)[1]

trpt.SPF.horizons <- triptych(df.SPF.horizons %>%  dplyr::select(-DATE.FC.due))
summary(trpt.SPF.horizons)
ggsave(paste0("applications/plots/SPF_Average_Horizons.pdf"),
       autoplot(trpt.SPF.horizons,
                plot_linetypes = "solid",
                plot_legend_title = "forecast horizon"),
       width=24, height=10.5, units="cm")



autoplot(trpt.SPF.horizons,
         plot_linetypes = "solid") +
  guides(col=guide_legend("Horizon"),
         linetype=guide_legend("Horizon"),
         linetype=guide_legend("Horizon"))




# When does #65 actually issue forecasts
spf.gdp.long %>%
  tibble::as_tibble() %>%
  group_by(ID,FC.Horizon) %>%
  filter(ID==65, FC.Horizon==0) %>% pull(DATE.FC.due)
















###########################################################################
###    OLD
###########################################################################



# Check how many forecasts they issued
df.FC.issued <- spf.gdp %>% group_by(ID) %>% summarise(n.FC = sum(!is.na(RECESS1))) %>% arrange(-n.FC)
FC.IDs <- df.FC.issued$ID[2:3] # list of 10 most productive ID's (not the combined one...) 84  65 421 426 433 446 484  20 463 407

h.ahead_set <- c("RECESS1", "RECESS2", "RECESS3", "RECESS4", "RECESS5")
# generate plots for the 10 most productive forecasters
for (ind.ID in FC.IDs){
  spf.avg <- filter(spf.gdp, ID==0)
  spf.ind <- subset(spf.gdp, ID==ind.ID)

  # loop over h-ahead forecasts
  for (h.ahead.index in 1:length(h.ahead_set)){
    h.ahead <- h.ahead_set[h.ahead.index]
    # Filter out the dates were forecasts were issued
    spf.ind.FCdates <- spf.ind[!is.na(as.numeric(spf.ind[[h.ahead]])),]$DATE.issued

    # Match dates of the ind & avg forecasts with the gdp realizations.
    # Notice that RECESS1 corresponds to nowcasts for the current quarter and RECESS5 are 4-quarter ahead forecasts
    spf.ind.subset <- filter(spf.ind, DATE.issued %in% spf.ind.FCdates & DATE.issued %in% (gdp$DATE - months(3*(h.ahead.index-1))))
    spf.avg.subset <- filter(spf.avg, DATE.issued %in% spf.ind.FCdates & DATE.issued %in% (gdp$DATE - months(3*(h.ahead.index-1))))
    gdp.subset <- filter(gdp, DATE %in% (spf.ind.FCdates + months(3*(h.ahead.index-1))) & DATE %in% gdp$DATE)

    # FC data frame for triptych package
    FC.df <- data.frame(FC.ind=spf.ind.subset[[h.ahead]], FC.avg=spf.avg.subset[[h.ahead]], FC.clim = cumsum(gdp.subset$gdp.first.recess) / seq_along(gdp.subset$gdp.first.recess))

    bins <- c(0,0.01,0.3,0.05,0.1,0.2,0.5,1)
    # run the triptych function and save the file
    p.triptych <- triptych(rlz=gdp.subset$gdp.first.recess, FC=FC.df, bins.list=list(bins,bins,bins), PAV=FALSE)
    ggsave(paste0("./applications/plots/GDP_SPF",ind.ID,"_",h.ahead.index-1,"Qahead.pdf"), p.triptych, width = 45, height = 15, units = "cm")
    # triptych with PAVA
    p.triptych <- triptych(rlz=gdp.subset$gdp.first.recess, FC=FC.df, bins.list=list(bins,bins,bins), PAV=TRUE)
    ggsave(paste0("./applications/plots/GDP_SPF",ind.ID,"_",h.ahead.index-1,"Qahead_PAVA.pdf"), p.triptych, width = 45, height = 15, units = "cm")
  }
}






triptych(rlz=gdp.subset$gdp.first.recess, FC=FC.df, PAV=TRUE, bins.rel=FALSE)

triptych(rlz=gdp.subset$gdp.last.recess, FC=FC.df, PAV=TRUE, bins.rel=FALSE)


triptych(rlz=gdp.subset$gdp.first.recess, FC=FC.df, PAV=FALSE, rel.bins=FALSE)


# realizations starting from 1969 Q1, 1-step ahead forecasts starting from issuing date 1968 Q4











triptych(rlz=gdp$gdp.first.recess, FC=FC.df, PAV=c(0,0,1,1), bins.rel = c(1,1,0,0))


asdf <- as.numeric(spf$RECESS1)

spf84 <- subset(spf, ID==84)
spf65 <- subset(spf, ID==65)


# spf.avg2 <- spf %>% group_by(YEAR, QUARTER) %>% summarise(mean(as.numeric(RECESS1)), na.rm = TRUE)
# spf.avg2


  filter(n==n.cur,inst=="(1,x)",dgp==dgp.cur, functional != "mean-median", skew == skew.cur, new_row == 1) %>%
  group_by(inst,n,theta1,theta2,functional,new_row) %>%

FC.matrix <- cbind(recid$logitpredprobs, recid$gbmpredprobs, recid$compas_decile_score/10, recid$mturkpredprobs)

FC.df <- data.frame(logit=recid$logitpredprobs, gbm=recid$gbmpredprobs, compas=recid$compas_decile_score/10, mturk=recid$mturkpredprobs)

triptych(rlz=recid$two_year_recid,FC=FC.df, PAV=c(0,0,1,1), bins.rel = c(1,1,0,0))

