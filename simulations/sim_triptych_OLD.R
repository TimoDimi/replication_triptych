
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








##############################################################################################################
###    OLD
##############################################################################################################


# Sim Design (D3): Three Times Crossing ROC Curves
n <- 10000
beta <- 1

p0 <- runif(n)
rlz <- rbinom(n, 1, p0)

p1 <- p0
p1[(p1>3/4)] <- 7/8
p1[(p1>1/4 & p1<1/2)] <- 3/8

p2 <- p0
p2[(p2<1/4)] <- 1/8
p2[(p2>1/2 & p2<3/4)] <- 5/8

p.df <- data.frame(p1,p2)
colnames(p.df) <- c("p1", "p2")
p.triptych <- triptych(rlz, p.df, PAV=TRUE)
ggsave(paste0("./simulations/plots/SimD4",beta,".pdf"), p.triptych, width = 37.5, height = 15, units = "cm")




# Sim Design (E): Try to contradict the k-Crossing Theory
n <- 1000
p0 <- runif(n)
rlz <- rbinom(n, 1, p0)
p1 <- qbeta(p0,2,5)
p2 <- qbeta(p0,5,2)

p.df <- data.frame(p1,p2)
colnames(p.df) <- c("p1", "p2")
p.triptych <- triptych(rlz, p.df)
p.triptych




# Sim Design for Reliability Diagrams
n <- 200
p0 <- runif(n)
rlz <- rbinom(n, 1, p0)

# Standard binning rel diag
rel.diag.individual(rlz,p0, bins=10)


# PAV binning?
a <- gpava(p0, rlz, ties="secondary")
b <- sort(unique(a$x))
rel.diag.individual(rlz,p0, bins=c(0,b[-length(b)] + diff(b)/2,1))





# Tilmann Slides Example
p <- c(1,2,3,3,4,4,4,5,5,5,6,7)/7
rlz <- c(0,1,0,0,0,0,1,0,1,1,1,1)
res.pava <- gpava(p, rlz, ties="secondary")

df.pava <- data.frame(FC=res.pava$z, rlz=rlz, FC.pava=res.pava$x)
df.pava <- df.pava %>% group_by(FC) %>% mutate(CEP = mean(rlz)) # Compute the conditional event probabilities (CEP)
df.pava

rel.diag.individual(rlz,p, bins=FALSE)

## Sim Example
n <- 200
FC <- c(runif(n), sample(4,n, replace=TRUE)/4-1/8)
rlz <- rbinom(2*n, 1, FC)

p.rel <- rel.diag.PAV(rlz,FC)
ggsave(paste0("./simulations/plots/Sim_RelDiag_BinPAV.pdf"), p.rel, width = 15, height = 15, units = "cm")

p.rel <- rel.diag.individual(rlz, FC, bins=10)
ggsave(paste0("./simulations/plots/Sim_RelDiag_BinEquidist.pdf"), p.rel, width = 15, height = 15, units = "cm")


## Sim Example Non-uniform
n <- 1024

n_set <- 4^(c(3,4,5,6))
FC <- c(rbeta(n/2,1,5), runif(n/2))
#FC <- runif(n)
plot(density(FC))
rlz <- rbinom(n, 1, FC)
rel.diag.PAV(rlz,FC)

rel.diag.individual(rlz, FC, bins=10)



## Sim Example Non-uniform
n <- 1024
FC <- rbeta(n,0.1,0.1)
plot(density(FC))
rlz <- rbinom(n, 1, FC)
rel.diag.PAV(rlz,FC)



# ggsave(paste0("./simulations/plots/Sim_RelDiag_BinPAV.pdf"), p.rel, width = 15, height = 15, units = "cm")

p.rel <- rel.diag.individual(rlz, FC, bins=10)
# ggsave(paste0("./simulations/plots/Sim_RelDiag_BinEquidist.pdf"), p.rel, width = 15, height = 15, units = "cm")




#
res.pava <- gpava(FC,rlz)
df.pava <- data.frame(FC.pava=res.pava$x, FC=res.pava$z)
df.pava <- arrange(df.pava, FC)
df.pava



#######################################################
# Classical Reliability Diagrams
set.seed(7)
n <- 128
FC <- c(rbeta(n/2,1,5), runif(n/2))
rlz <- rbinom(n, 1, FC)

p.rel <- rel.diag.individual(rlz, FC, bins=10)
p.rel
ggsave(paste0("./simulations/plots/RelDiagPresentation1",".pdf"), p.rel, width = 15, height = 15, units = "cm")

p.rel <- rel.diag.PAV(rlz,FC)
p.rel
ggsave(paste0("./simulations/plots/RelDiagPresentation2",".pdf"), p.rel, width = 15, height = 15, units = "cm")

#######################################################
# ROC Illustration
res.pava <- gpava(FC, rlz, ties = "secondary")
FC.df <- data.frame(FC=FC, FC.PAVA=res.pava$x)
p.ROC <- ROC.plot(rlz, FC.df)
p.ROC
ggsave(paste0("./simulations/plots/ROC_Illustration",".pdf"), p.ROC[[1]], width = 15, height = 15, units = "cm")



#######################################################
# Reliability Diagrams Example Highly Skewed Data
set.seed(5)
n <- 500
FC <- rbeta(n,0.7,5)
rlz <- rbinom(n, 1, FC)

p.rel <- rel.diag.individual(rlz, FC, bins="quantiles")
p.rel
ggsave(paste0("./simulations/plots/RelDiagSkew3",".pdf"), p.rel, width = 15, height = 15, units = "cm")

p.rel <- rel.diag.PAV(rlz,FC)
p.rel
ggsave(paste0("./simulations/plots/RelDiagSkew4",".pdf"), p.rel, width = 15, height = 15, units = "cm")



#######################################################
# Classical Reliability Diagrams, discrete distribution
set.seed(1)
n <- 128
FC <- c(runif(n/2), sample(4,n/2, replace=TRUE)/4-1/8)
rlz <- rbinom(n, 1, FC)

p.rel <- rel.diag.individual(rlz, FC, bins=10)
p.rel
ggsave(paste0("./simulations/plots/RelDiagPresentation_Discrete1",".pdf"), p.rel, width = 15, height = 15, units = "cm")

p.rel <- rel.diag.PAV(rlz,FC,plot.density=FALSE)
p.rel
ggsave(paste0("./simulations/plots/RelDiagPresentation_Discrete2",".pdf"), p.rel, width = 15, height = 15, units = "cm")




#######################################################
# Sim Design Real.Diag.PAV: Different Sample sizes
n.set <- 4^(c(2,3,4,5,6,7))
for (i.n in 1:length(n.set)){
  n <- n.set[i.n]
  FC <- c(rbeta(n/2,1,5), runif(n/2))
  #FC <- runif(n)
  plot(density(FC))
  rlz <- rbinom(n, 1, FC)
  p.rel.diag.HistPAV <- rel.diag.PAV(rlz,FC)
  p.rel.diag.HistEquidist <- rel.diag.PAV(rlz,FC, hist.bins = "equidistant")
  p.rel.diag <- grid.arrange(p.rel.diag.HistPAV, p.rel.diag.HistEquidist, ncol=2,  heights=1)
  p.rel.diag
  # ggsave(paste0("./simulations/plots/RelDiagPAV_n_HistPAV",n,".pdf"), p.rel.diag.HistPAV, width = 15, height = 15, units = "cm")
  # ggsave(paste0("./simulations/plots/RelDiagPAV_n_HistEqui",n,".pdf"), p.rel.diag.HistEquidist, width = 15, height = 15, units = "cm")
  ggsave(paste0("./simulations/plots/RelDiagPAV_n",n,".pdf"), p.rel.diag, width = 30, height = 15, units = "cm")
}


#######################################################
# Compare the Classical Reliability Diagram against the new PAVA Reliability Diagram
n.set <- 4^(c(2,3,4,5,6,7))
for (i.n in 1:length(n.set)){
  n <- n.set[i.n]
  FC <- c(rbeta(n/2,1,5), runif(n/2))
  #FC <- runif(n)
  plot(density(FC))
  rlz <- rbinom(n, 1, FC)
  p.rel.diag.HistPAV <- rel.diag.PAV(rlz,FC, unite.bins=TRUE)
  p.rel.diag.classic <- rel.diag.individual(rlz,FC, bins=10)
  p.rel.diag <- grid.arrange(p.rel.diag.HistPAV, p.rel.diag.classic, ncol=2,  heights=1)
  p.rel.diag
  # ggsave(paste0("./simulations/plots/RelDiagPAV_n_HistPAV",n,".pdf"), p.rel.diag.HistPAV, width = 15, height = 15, units = "cm")
  # ggsave(paste0("./simulations/plots/RelDiagPAV_n_HistEqui",n,".pdf"), p.rel.diag.HistEquidist, width = 15, height = 15, units = "cm")
  ggsave(paste0("./simulations/plots/RelDiagPAV_Comp_n",n,".pdf"), p.rel.diag, width = 30, height = 15, units = "cm")
}



# Sim Design Real.Diag.PAV: With and Without "unite.bins"
n.set <- 4^(c(2,3,4,5,6,7))
for (i.n in 1:length(n.set)){
  n <- n.set[i.n]
  FC <- c(rbeta(n/2,1,5), runif(n/2))
  #FC <- runif(n)
  plot(density(FC))
  rlz <- rbinom(n, 1, FC)
  p.rel.diag.UniteBins <- rel.diag.PAV(rlz,FC, unite.bins=TRUE)
  p.rel.diag.AllBins <- rel.diag.PAV(rlz,FC)
  p.rel.diag <- grid.arrange(p.rel.diag.UniteBins, p.rel.diag.AllBins, ncol=2,  heights=1)
  p.rel.diag
  ggsave(paste0("./simulations/plots/RelDiagPAV_UniteBins_n",n,".pdf"), p.rel.diag, width = 30, height = 15, units = "cm")
}



# Sim Design Real.Diag.PAV: Different Sample sizes for mixed continuous/descrete distribution
n.set <- 4^(c(3,4,5,6,7))
for (i.n in 1:length(n.set)){
  n <- n.set[i.n]
  FC <- c(runif(n/2), sample(4,n/2, replace=TRUE)/4-1/8)
  plot(density(FC))
  rlz <- rbinom(n, 1, FC)
  p.rel.diag.HistPAV <- rel.diag.PAV(rlz,FC)
  p.rel.diag.HistEquidist <- rel.diag.PAV(rlz,FC, hist.bins = "equidistant")
  p.rel.diag <- grid.arrange(p.rel.diag.HistPAV, p.rel.diag.HistEquidist, ncol=2,  heights=1)
  p.rel.diag
  ggsave(paste0("./simulations/plots/RelDiagPAV_ContDisc_n",n,".pdf"), p.rel.diag, width = 30, height = 15, units = "cm")
}






# Sim Design (A.PAV): Perfect Discrimination
n <- 500
alpha <- 1
p0 <- runif(n)
rlz <- rbinom(n, 1, p0)
# Not Calibrated
p1 <- 1/4 + 1/2*p0^alpha
p1.rel.diag <- rel.diag.PAV(rlz, p1)
ggsave(paste0("./simulations/plots/RelDaigPAV_SimA_NotCal.pdf"), p1.rel.diag, width = 15, height = 15, units = "cm")
# Not Calibrated
p2 <- p0^alpha
p2.rel.diag <- rel.diag.PAV(rlz, p2)
ggsave(paste0("./simulations/plots/RelDaigPAV_SimA_Cal.pdf"), p2.rel.diag, width = 15, height = 15, units = "cm")


for (i.alpha in 1:length(alpha.set)){
  alpha <- alpha.set[i.alpha]
  p1 <- 3/8 + 1/4*p0^alpha
  p2 <- p0^alpha
  p.df <- data.frame(p1=p1, p2=p2)
  p.rel.diag. <- rel.diag.PAV(rlz, p.df)
  ggsave(paste0("./simulations/plots/RelDaigPAV_SimA_alpha",alpha,".pdf"), p.rel.diag, width = 15, height = 15, units = "cm")
}



## Sim Example Continuous and Discrete
n <- 20
ratio <-5
FC <- c(runif(n), sample(4,ratio*n, replace=TRUE)/4-1/8)
rlz <- rbinom((ratio+1)*n, 1, FC)

p.rel <- rel.diag.PAV(rlz,FC)
p.rel

p.rel <- rel.diag.individual(rlz, FC, bins=10)
p.rel




# Short check for histogram
n <- 5000
# FC <- c(rbeta(n/2,1,5), runif(n/2))
FC <- runif(n)
# FC <- rbeta(n,1,5)
rlz <- rbinom(n, 1, FC)
df.test <- data.frame(FC=FC,rlz=rlz)

p <- ggplot(data=df.test, mapping=aes(x=FC, y=CEP)) + ylab("") + xlab("") + xlim(0,1) + ylim(0,1)

p + geom_histogram(data=df.test, mapping=aes(x=FC, y = ..ndensity../5), inherit.aes=FALSE, color="black", fill="white", alpha = 0.2, breaks=c(0,0.1,0.4,0.96,1))

p + geom_histogram(data=df.test, mapping=aes(x=FC, y = ..ncount../5), inherit.aes=FALSE, color="black", fill="white", alpha = 0.2, breaks=c(0,0.1,0.4,0.96,1))

