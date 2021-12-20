
library(triptych)

###########################################################################
###   Sim Design (A): Perfect Discrimination

set.seed(1)
n <- 10000
alpha.set <- c(0.1,0.5,1,2,5)

p0 <- runif(n)
rlz <- rbinom(n, 1, p0)

for (i.alpha in 1:length(alpha.set)){
  alpha <- alpha.set[i.alpha]
  p1 <- 3/8 + 1/4*p0^alpha
  p2 <- p0^alpha
  p.df <- data.frame(y=rlz, uncalibrated=p1, calibrated=p2)

  trpt.SimA <- triptych(p.df)
  ggsave(paste0("./simulations/plots/SimA_alpha",alpha,".pdf"),
         autoplot(trpt.SimA,
                  plot_linetypes="solid"),
         width = 24, height = 10.5, units = "cm")
}





###########################################################################
###   Sim Design (B): Perfect Calibration

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
ggsave(paste0("./simulations/plots/SimB.pdf"),
       autoplot(trpt.SimB,
                plot_linetypes="solid"),
       width = 24, height = 10.5, units = "cm")






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


