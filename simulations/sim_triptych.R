
library(triptych)

###########################################################################
###   Sim Design (A): Perfect Discrimination

set.seed(1)
n <- 10000

p0 <- runif(n)
rlz <- rbinom(n, 1, p0)

p1 <- 3/8 + 1/4*p0
p2 <- p0
p.df <- data.frame(y=rlz, uncalibrated=p1, calibrated=p2)

trpt_SimA <- triptych(p.df)
ggsave(paste0("./simulations/plots/SimA.pdf"),
       autoplot(trpt_SimA,
                Murphy_scoretype="score"),
       width = 24, height = 10.5, units = "cm")


###########################################################################
###   Sim Design (B): Perfect Calibration and 1 ROC/Murphy Crossing Point

set.seed(1)
n <- 10000

p0 <- runif(n)
rlz <- rbinom(n, 1, p0)

p1 <- ifelse((p0 >= 1/4 & p0 <= 3/4), 1/2, p0)

p0_hlp <- ifelse((p0 < 1/4), 1/8, p0)
p2 <- ifelse((p0_hlp > 3/4), 7/8, p0_hlp)


p.df <- data.frame(y=rlz, p1=p1, p2=p2)

trpt_SimB <- triptych(p.df)

p_test <- autoplot(trpt_SimB,
                   Murphy_scoretype="score")

ggsave(paste0("./simulations/plots/SimB.pdf"),
       width = 24, height = 10.5, units = "cm")


### Manual plots with crossing points
p_hlp_Murphy <- autoplot(trpt_SimB,
                         Murphy_scoretype="score",
                         plot_type="Murphy") +
  geom_point(data = .%>%filter(forecast=="p1", theta %in% c(0.25,0.75)),
             aes(x=theta, y=elem_score), col="black")

p_hlp_ROC <- autoplot(trpt_SimB,
                      Murphy_scoretype="score",
                      plot_type="ROC") +
  geom_point(data = .%>%filter(forecast=="p1", row_number() %in% c(21,22)),
             aes(x=1-specificities, y=sensitivities), col="black")
# Manually identify the values 0.43471306 and 0.93561288 for specificities

p_hlp_RelDiag <- autoplot(trpt_SimB,
                      Murphy_scoretype="score",
                      plot_type="ReliabilityDiagram")

width <- 0.5
plot_margins <- grid::unit(c(0,0.02,0,0.02), "npc")
p_triptych <- (p_hlp_Murphy + ggplot2::theme(aspect.ratio = NULL)) +
  (p_hlp_RelDiag + ggplot2::theme(aspect.ratio = NULL) + theme(plot.margin = plot_margins)) +
  (p_hlp_ROC + ggplot2::theme(aspect.ratio = NULL)) +
  plot_layout(guides = "collect", width=c(1,width,1)) & theme(legend.position = 'bottom') +
  ggplot2::theme(aspect.ratio = 1)

ggsave(paste0("./simulations/plots/SimB_points.pdf"),
       p_triptych,
       width = 24, height = 10.5, units = "cm")

###########################################################################
###   Sim Design (C): Perfect Calibration

set.seed(1)
n <- 10000
a1 <- rnorm(n)
a2 <- rnorm(n)
a3 <- rnorm(n)
a4 <- rnorm(n)

p0 <- pnorm(a1+a2+a3+a4)
rlz <- rbinom(n, 1, p0)

p1 <- pnorm( a1/sqrt(4) )
p2 <- pnorm( (a1+a2)/sqrt(3) )
p3 <- pnorm( (a1+a2+a3)/sqrt(2) )
p4 <- pnorm( (a1+a2+a3+a4)/sqrt(1) )

p.df.B <- data.frame(y=rlz, p1=p1, p2=p2, p3=p3, p4=p4)
trpt.SimB <- triptych(p.df.B)
ggsave(paste0("./simulations/plots/SimC.pdf"),
       autoplot(trpt.SimB,
                Murphy_scoretype="score"),
       width = 24, height = 10.5, units = "cm")





###########################################################################
###########################################################################
###########################################################################
# OLD




###########################################################################
###   Sim Design (C): Partially Perfect Economic Value

set.seed(1)
n <- 10000
beta.set <- c(0.5,1,2)

p0 <- runif(n)
rlz <- rbinom(n, 1, p0)

for (i.beta in 1:length(beta.set)){
  beta <- beta.set[i.beta]

  p.matrix <- matrix(NA,n,4)
  for (i in 1:4){
    p.matrix[,i] <- pmax(p0^beta, (i-1)/4)
  }

  p.df <- data.frame(p.matrix)
  colnames(p.df) <- c("p1", "p2", "p3", "p4")
  p.df$y <- rlz

  trpt.SimC <- triptych(p.df)
  ggsave(paste0("./simulations/plots/SimC_beta",beta,".pdf"),
         autoplot(trpt.SimC,
                  plot_linetypes="solid"),
         width = 24, height = 10.5, units = "cm")
}




###########################################################################
###   Sim Design (D1): Perfect Calibration and 1 ROC/Murphy Crossing Point

set.seed(1)
n <- 10000
beta <- 1

p0 <- runif(n)
rlz <- rbinom(n, 1, p0)

p1 <- pmax(p0^beta, 1/2)
p1[p1 == 1/2] = 1/4
p2 <- pmin(p0^beta, 1/2)
p2[p2 == 1/2] = 3/4
p.df <- data.frame(y=rlz, p1=p1, p2=p2)

trpt.SimD1 <- triptych(p.df)

ggsave(paste0("./simulations/plots/SimD1.pdf"),
       autoplot(trpt.SimD1,
                plot.linetypes="solid"),
       width = 24, height = 10.5, units = "cm")



### Sim Design (D2): Perfect Calibration and 2 ROC/Murphy Crossing Points

p1 <- pmax(1/3,pmin(p0^beta, 2/3))
p1[p1 == 1/3] = 1/6
p1[p1 == 2/3] = 5/6

p2 <- p0
p2[(p2>1/3 & p2<2/3)] <- 1/2

p.df <- data.frame(y=rlz, p1=p1, p2=p2)

trpt.SimD2 <- triptych(p.df)

ggsave(paste0("./simulations/plots/SimD2.pdf"),
       autoplot(trpt.SimD2,
                plot.linetypes="solid"),
       width = 24, height = 10.5, units = "cm")



###########################################################################
###   Classification Plots for all simulation designs

# Design A
alpha <- 1
p1 <- 3/8 + 1/4*p0^alpha
p2 <- p0^alpha
p.df <- data.frame(y=rlz, p1=p1, p2=p2)

p.CP.A <- ClassificationPlot_own(model_list=list("p1", "p2"),
                       y=y,
                       df=p.df)

# Design B
p.CP.B <- ClassificationPlot_own(model_list=list("p1", "p2"),
                       y=y,
                       df=p.df.B)

# Design C
p.matrix <- matrix(NA,n,4)
for (i in 1:4){
  p.matrix[,i] <- pmax(p0, (i-1)/4)
}

p.df <- data.frame(p.matrix)
colnames(p.df) <- c("p1", "p2", "p3", "p4")
p.df$y <- rlz

p.CP.C <- ClassificationPlot_own(model_list=list("p1", "p2"),
                                 y=y,
                                 df=p.df)

# Merge the Plots
p <- (p.CP.A[[1]] + theme(aspect.ratio=1) + ggtitle("Simulation Setup A")) +
(p.CP.B[[1]] + theme(aspect.ratio=1) + ggtitle("Simulation Setup B")) +
(p.CP.C[[1]] + theme(aspect.ratio=1) + ggtitle("Simulation Setup C")) +
  plot_layout(guides = "collect") & theme(legend.position = 'bottom')

ggsave(paste0("./simulations/plots/SimA-C_ClassificationPlots.pdf"),
       p,
       width = 24, height = 10.5, units = "cm")



# Sim setting (A), (B) and a DSC Murphy diagram
p <- (p.CP.A[[1]] + theme(aspect.ratio=1) + ggtitle("(a) Simulation Setup (A)") + theme(plot.title = element_text(hjust = 0.5)) + theme(legend.position = 'none')) +
  (p.CP.B[[1]] + theme(aspect.ratio=1) + ggtitle("(b) Simulation Setup (B)")+ theme(plot.title = element_text(hjust = 0.5)) + theme(legend.position = 'none')) +
  (triptych(p.df.B %>% select(y,p1,p2)) %>% plot(plot_type="Murphy", Murphy_scoretype="DSC") + ggtitle("(c) DSC Murphy Diagram") + theme(legend.position = 'none') )
# + plot_layout(guides = "collect") & theme(legend.position = 'bottom')
p

ggsave(paste0("./simulations/plots/SimA-B_ClassificationPlots_DSCMurphy.pdf"),
       p,
       width = 24, height = 9, units = "cm")



###########################################################################
# TEST:  Plot ROC curve WITH threshold values!
library(plotROC)
ggplot(data=data.frame(y=rlz, p1=p1, p2=p2) %>%
         melt(measure.vars=c("p1", "p2")) %>% as_tibble() ) +
  geom_roc(aes(d=y, m=value, color=variable), n.cuts = 20) +
  facet_wrap(~variable)
xlab("False alarm rate") +
  ylab("Hit rate") +
  theme(legend.position = "bottom")


