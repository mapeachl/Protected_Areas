# Multi-species analysis of the effect of different levels of land protection
# on forest bird colonization and extinction using a single visit dynamic
# occupancy model.

# Read in the data
# SPP DATA
data_80 <- read.csv('C:/Users/LocalUser/Desktop/WorkSpace/Research/Protected_Areas/Multi_Species/data_80.csv', header=TRUE)
data_00 <- read.csv('C:/Users/LocalUser/Desktop/WorkSpace/Research/Protected_Areas/Multi_Species/data_00.csv', header=TRUE)

library(tidyr)
library(dplyr)

new_80 <- data_80 %>%
  filter(State=="PA") %>%
  select(BGGN,BAWW,AMWO)

new_00 <- data_00 %>%
  filter(State=="PA") %>%
  select(BGGN,BAWW,AMWO)

library(abind)
y <- abind(new_80,new_00,along=3)

# ENVIRONMENTAL COVARIATES
enviro_vars <- read.csv("C:/Users/LocalUser/Desktop/WorkSpace/Research/Protected_Areas/Multi_Species/enviro_vars.csv",header=TRUE)
x <- enviro_vars[,c(5,6,7,8,9,10,19,20,22,23,25,26,27,28)]
x$eff1 <- data_80$Effort
x$eff2 <- data_00$Effort

x_temp <- x %>%
  filter(data_80$State=="PA")

effort <- x %>%
  select(eff1,eff2) %>%
  filter(data_80$State=="PA")

nsite <- dim(y)[1]
nspp <- dim(y)[2]
nyear <- dim(y)[3]

# Load library
library(rjags)

# Specify model in BUGS language
sink("multispp.txt")
cat("
    model {
    
    # Specify priors
    # Random effect parameters for the different explanatory variables

    muO1~dnorm(0,0.1)
    sigmaO1~dunif(0,100)
    tauO1 <- 1/(sigmaO1*sigmaO1)
    muO2~dnorm(0,0.1)
    sigmaO2~dunif(0,100)
    tauO2 <- 1/(sigmaO2*sigmaO2)
    muO3~dnorm(0,0.1)
    sigmaO3~dunif(0,100)
    tauO3 <- 1/(sigmaO3*sigmaO3)
    muO4~dnorm(0,0.1)
    sigmaO4~dunif(0,100)
    tauO4 <- 1/(sigmaO4*sigmaO4)
    muO5~dnorm(0,0.1)
    sigmaO5~dunif(0,100)
    tauO5 <- 1/(sigmaO5*sigmaO5)
    muO6~dnorm(0,0.1)
    sigmaO6~dunif(0,100)
    tauO6 <- 1/(sigmaO6*sigmaO6)
    muO7~dnorm(0,0.1)
    sigmaO7~dunif(0,100)
    tauO7 <- 1/(sigmaO7*sigmaO7)
    muO8~dnorm(0,0.1)
    sigmaO8~dunif(0,100)
    tauO8 <- 1/(sigmaO8*sigmaO8)
    muO9~dnorm(0,0.1)
    sigmaO9~dunif(0,100)
    tauO9 <- 1/(sigmaO9*sigmaO9)
    muO10~dnorm(0,0.1)
    sigmaO10~dunif(0,100)
    tauO10 <- 1/(sigmaO10*sigmaO10)
    muO11~dnorm(0,0.1)
    sigmaO11~dunif(0,100)
    tauO11 <- 1/(sigmaO11*sigmaO11)
    muO12~dnorm(0,0.1)
    sigmaO12~dunif(0,100)
    tauO12 <- 1/(sigmaO12*sigmaO12)
    
    
    muC1~dnorm(0,0.1)
    sigmaC1~dunif(0,100)
    tauC1 <- 1/(sigmaC1*sigmaC1)
    muC2~dnorm(0,0.1)
    sigmaC2~dunif(0,100)
    tauC2 <- 1/(sigmaC2*sigmaC2)
    muC3~dnorm(0,0.1)
    sigmaC3~dunif(0,100)
    tauC3 <- 1/(sigmaC3*sigmaC3)
    muC4~dnorm(0,0.1)
    sigmaC4~dunif(0,100)
    tauC4 <- 1/(sigmaC4*sigmaC4)
    muC5~dnorm(0,0.1)
    sigmaC5~dunif(0,100)
    tauC5 <- 1/(sigmaC5*sigmaC5)
    muC6~dnorm(0,0.1)
    sigmaC6~dunif(0,100)
    tauC6 <- 1/(sigmaC6*sigmaC6)
    muC7~dnorm(0,0.1)
    sigmaC7~dunif(0,100)
    tauC7 <- 1/(sigmaC7*sigmaC7)
    muC8~dnorm(0,0.1)
    sigmaC8~dunif(0,100)
    tauC8 <- 1/(sigmaC8*sigmaC8)
    muC9~dnorm(0,0.1)
    sigmaC9~dunif(0,100)
    tauC9 <- 1/(sigmaC9*sigmaC9)
    muC10~dnorm(0,0.1)
    sigmaC10~dunif(0,100)
    tauC10 <- 1/(sigmaC10*sigmaC10)
    muC11~dnorm(0,0.1)
    sigmaC11~dunif(0,100)
    tauC11 <- 1/(sigmaC11*sigmaC11)
    muC12~dnorm(0,0.1)
    sigmaC12~dunif(0,100)
    tauC12 <- 1/(sigmaC12*sigmaC12)
    
    muE1~dnorm(0,0.1)
    sigmaE1~dunif(0,100)
    tauE1 <- 1/(sigmaE1*sigmaE1)
    muE2~dnorm(0,0.1)
    sigmaE2~dunif(0,100)
    tauE2 <- 1/(sigmaE2*sigmaE2)
    muE3~dnorm(0,0.1)
    sigmaE3~dunif(0,100)
    tauE3 <- 1/(sigmaE3*sigmaE3)
    muE4~dnorm(0,0.1)
    sigmaE4~dunif(0,100)
    tauE4 <- 1/(sigmaE4*sigmaE4)
    muE5~dnorm(0,0.1)
    sigmaE5~dunif(0,100)
    tauE5 <- 1/(sigmaE5*sigmaE5)
    muE6~dnorm(0,0.1)
    sigmaE6~dunif(0,100)
    tauE6 <- 1/(sigmaE6*sigmaE6)
    muE7~dnorm(0,0.1)
    sigmaE7~dunif(0,100)
    tauE7 <- 1/(sigmaE7*sigmaE7)
    muE8~dnorm(0,0.1)
    sigmaE8~dunif(0,100)
    tauE8 <- 1/(sigmaE8*sigmaE8)
    muE9~dnorm(0,0.1)
    sigmaE9~dunif(0,100)
    tauE9 <- 1/(sigmaE9*sigmaE9)
    muE10~dnorm(0,0.1)
    sigmaE10~dunif(0,100)
    tauE10 <- 1/(sigmaE10*sigmaE10)
    muE11~dnorm(0,0.1)
    sigmaE11~dunif(0,100)
    tauE11 <- 1/(sigmaE11*sigmaE11)
    muE12~dnorm(0,0.1)
    sigmaE12~dunif(0,100)
    tauE12 <- 1/(sigmaE12*sigmaE12)
    
      for (j in 1:nspp){

      # Occupancy
    BO1[j] ~ dnorm(muO1,tauO1)
    BO2[j] ~ dnorm(muO2,tauO2)
    BO3[j] ~ dnorm(muO3,tauO3)
    BO4[j] ~ dnorm(muO4,tauO1)
    BO5[j] ~ dnorm(muO5,tauO2)
    BO6[j] ~ dnorm(muO6,tauO3)
    BO7[j] ~ dnorm(muO7,tauO1)
    BO8[j] ~ dnorm(muO8,tauO2)
    BO9[j] ~ dnorm(muO9,tauO3)
    BO10[j] ~ dnorm(muO10,tauO1)
    BO11[j] ~ dnorm(muO11,tauO2)
    BO12[j] ~ dnorm(muO12,tauO3)
    
    
    # Colonization
    BC1[j] ~ dnorm(muC1,tauC1)
    BC2[j] ~ dnorm(muC2,tauC2)
    BC3[j] ~ dnorm(muC3,tauC3)
    BC4[j] ~ dnorm(muC4,tauC1)
    BC5[j] ~ dnorm(muC5,tauC2)
    BC6[j] ~ dnorm(muC6,tauC3)
    BC7[j] ~ dnorm(muC7,tauC1)
    BC8[j] ~ dnorm(muC8,tauC2)
    BC9[j] ~ dnorm(muC9,tauC3)
    BC10[j] ~ dnorm(muC10,tauC1)
    BC11[j] ~ dnorm(muC11,tauC2)
    BC12[j] ~ dnorm(muC12,tauC3)
    
    # Extinction
    BE1[j] ~ dnorm(muE1,tauE1)
    BE2[j] ~ dnorm(muE2,tauE2)
    BE3[j] ~ dnorm(muE3,tauE3)
    BE4[j] ~ dnorm(muE4,tauE1)
    BE5[j] ~ dnorm(muE5,tauE2)
    BE6[j] ~ dnorm(muE6,tauE3)
    BE7[j] ~ dnorm(muE7,tauE1)
    BE8[j] ~ dnorm(muE8,tauE2)
    BE9[j] ~ dnorm(muE9,tauE3)
    BE10[j] ~ dnorm(muE10,tauE1)
    BE11[j] ~ dnorm(muE11,tauE2)
    BE12[j] ~ dnorm(muE12,tauE3)
   
    # Detection
    BD1[j]~dunif(-20,20)
  } 
    
    # Ecological submodel: Define state conditional on parameters
    for (j in 1:nspp){
      for(i in 1:nsite){
    
    z[i,j,1] ~ dbern(psi1[i,j])
    psi1[i,j] <- 1/(1+exp(-logitpsi[i,j]))
    logitpsi[i,j] <- BO1[j] + BO2[j]*MinT1[i] + BO3[j]*BSAveT1[i] + BO4[j]*TotP1[i] +
                      BO5[j]*Forest92[i] + BO6[j]*Open92[i] +
                      BO7[j]*GAP12[i] + BO8[j]*GAP3[i] + BO9[j]*GAP12[i]*GAP3[i] +
                      BO10[j]*GAP12[i]*Forest92[i] + BO11[j]*GAP3[i]*Forest92[i] +
                      BO12[j]*GAP12[i]*GAP3[i]*Forest92[i]
                      

    z[i,j,2] ~ dbern(psi2[i,j])
    psi2[i,j] <- z[i,j,1]*extant[i,j] + (1-z[i,j,1])*col[i,j]
    
    logitcol[i,j] <- BC1[j] + BC2[j]*MinT2[i] + BC3[j]*BSAveT2[i] + BC4[j]*TotP2[i] +
                      BC5[j]*Forest01[i] + BC6[j]*Open01[i] +
                      BC7[j]*GAP12[i] + BC8[j]*GAP3[i] + BC9[j]*GAP12[i]*GAP3[i] +
                      BC10[j]*GAP12[i]*Forest01[i] + BC11[j]*GAP3[i]*Forest01[i] +
                      BC12[j]*GAP12[i]*GAP3[i]*Forest01[i]
    col[i,j] <- 1/(1+exp(-logitcol[i,j]))

    logitext[i,j] <- BE1[j] + BE2[j]*MinT2[i] + BE3[j]*BSAveT2[i] + BE4[j]*TotP2[i] +
                      BE5[j]*Forest01[i] + BE6[j]*Open01[i] +
                      BE7[j]*GAP12[i] + BE8[j]*GAP3[i] + BE9[j]*GAP12[i]*GAP3[i] +
                      BE10[j]*GAP12[i]*Forest01[i] + BE11[j]*GAP3[i]*Forest01[i] +
                      BE12[j]*GAP12[i]*GAP3[i]*Forest01[i]
    ext[i,j] <- 1/(1+exp(-logitext[i,j]))
    extant[i,j] <- 1-ext[i,j]
      }
    }


    # Observation model
  for (j in 1:nspp){
      for(i in 1:nsite){    
        for (k in 1:nyear){
    
    y[i,j,k] ~ dbern(muy[i,j,k])
    muy[i,j,k] <- z[i,j,k]*pstar[i,j,k]
    pstar[i,j,k] <- 1-(1-logitp[i,j,k])^effort[i,k]
    logitp[i,j,k] <- 1/(1+exp(-BetaD1[j]))
      }
      }
    }
    }
    ",fill = TRUE)
sink()

# Bundle data

win.data <- list(y = y, nsite = nsite, nyear = nyear,nspp = nspp, 
                 effort=effort,MinT1=x_temp$MinT1,BSAveT1=x_temp$BSAveT1,TotP1=x_temp$TotP1,
                 Forest92=x_temp$Forest92,Open92=x_temp$Open92,GAP12=x_temp$P_Gap12,GAP3=x_temp$P_Gap3,
                 MinT2=x_temp$MinT2,BSAveT2=x_temp$BSAveT2,TotP2=x_temp$TotP2,Forest01=x_temp$Forest01,
                 Open01=x_temp$Open01)
# Initial values
#zst <- apply(y, c(1, 3), max)       # Observed occurrence as inits for z

inits <- function(){list(z = y,BO1=rnorm(3,0,0.001),BO2=rnorm(3,0,0.001),BO3=rnorm(3,0,0.001),
                         BO4=rnorm(3,0,0.001),BO5=rnorm(3,0,0.001),BO6=rnorm(3,0,0.001),
                         BO7=rnorm(3,0,0.001),BO8=rnorm(3,0,0.001),BO9=rnorm(3,0,0.001),
                         BO10=rnorm(3,0,0.001),BO11=rnorm(3,0,0.001),BO12=rnorm(3,0,0.001),
                         BC1=rnorm(3,0,0.001),BC2=rnorm(3,0,0.001),BC3=rnorm(3,0,0.001),
                         BC4=rnorm(3,0,0.001),BC5=rnorm(3,0,0.001),BC6=rnorm(3,0,0.001),
                         BC7=rnorm(3,0,0.001),BC8=rnorm(3,0,0.001),BC9=rnorm(3,0,0.001),
                         BC10=rnorm(3,0,0.001),BC11=rnorm(3,0,0.001),BC12=rnorm(3,0,0.001),
                         BE1=rnorm(3,0,0.001),BE2=rnorm(3,0,0.001),BE3=rnorm(3,0,0.001),
                         BE4=rnorm(3,0,0.001),BE5=rnorm(3,0,0.001),BE6=rnorm(3,0,0.001),
                         BE7=rnorm(3,0,0.001),BE8=rnorm(3,0,0.001),BE9=rnorm(3,0,0.001),
                         BE10=rnorm(3,0,0.001),BE11=rnorm(3,0,0.001),BE12=rnorm(3,0,0.001),
                         BD1=rnorm(3,0,0.001))}

# Parameters monitored
params <- c("BO1","BO2","BO3","BO4","BO5","BO6","BO7","BO8","BO9","BO10","BO11","BO12",
            "BC1","BC2","BC3","BC4","BC5","BC6","BC7","BC8","BC9","BC10","BC11","BC12",
            "BE1","BE2","BE3","BE4","BE5","BE6","BE7","BE8","BE9","BE10","BE11","BE12",
            "sigmaO1","sigmaO2","sigmaO3","sigmaO4","sigmaO5","sigmaO6","sigmaO7","sigmaO8","sigmaO9","sigmaO10","sigmaO11","sigmaO12",
            "sigmaC1","sigmaC2","sigmaC3","sigmaC4","sigmaC5","sigmaC6","sigmaC7","sigmaC8","sigmaC9","sigmaC10","sigmaC11","sigmaC12",
            "sigmaE1","sigmaE2","sigmaE3","sigmaE4","sigmaE5","sigmaE6","sigmaE7","sigmaE8","sigmaE9","sigmaE10","sigmaE11","sigmaE12")

# MCMC settings
ni <- 1000
nt <- 4
nb <- 100
nc <- 3

ptm <- proc.time()
test1.glmm <- jags.model("multispp.txt", win.data, inits, n.chain=nc, n.adapt=100)
out <- coda.samples(test1.glmm, params, n.iter=ni, thin=nt,n.burning=nb)
proc.time() - ptm
#save(out,file="CAWAoutall50v4.saved")
#load("CAWAoutall50v3.saved")
#write.csv(summary(outallv2)$statistics,file="C:/Users/LocalUser/Desktop/WorkSpace/Research/Occupancy/test/CAWA_preds_all.csv")

#library(mcmcplots)
#mcmcplot(outallv3)

