
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
                  plot.linetypes="solid"),
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

p.df <- data.frame(y=rlz, p1=p1, p2=p2, p3=p3, p4=p4)
trpt.SimB <- triptych(p.df)
ggsave(paste0("./simulations/plots/SimB.pdf"),
       autoplot(trpt.SimB,
                plot.linetypes="solid"),
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
         autoplot(trpt.SimB,
                  plot.linetypes="solid"),
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
# TEST:  Plot ROC curve WITH threshold values!
library(plotROC)
ggplot(data=data.frame(y=rlz, p1=p1, p2=p2) %>%
         melt(measure.vars=c("p1", "p2")) %>% as_tibble() ) +
  geom_roc(aes(d=y, m=value, color=variable), n.cuts = 20) +
  facet_wrap(~variable)
xlab("False alarm rate") +
  ylab("Hit rate") +
  theme(legend.position = "bottom")


