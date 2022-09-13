# Example Model - Avocet Energy Expenditure

library(boot)
library(jagsUI)

ni <- 5000 # iterations
nt <- 5 # thin
nb <- 2000 # burnin
nc <- 3 # chains

# For this manuscript, the same model was run separately on 2 data sets
sink("el8_all.jags")
cat("
    # likelihood
    
    model{
    for (i in 1:N){
    odba[i] ~ dnorm(mu[i],tau)
    mu[i] <- lc[landcover[i]] + n[night[i]] + t*tide[i] + yd*yday[i] +txlc[landcover[i]]*tide[i] 
    + id*individual[i] + alpha 
    }
    #alpha ~ dnorm(0, 0.001)
    t ~ dnorm(0,0.001)
    n[1] <- 0 # [1] is day 
    lc[4] <- 0 # [4] is beach
    txlc[4] <- 0
    yd ~ dnorm(0,0.001)
    id ~ dnorm(0,tau.id)
    lc[5] ~ dnorm(0, 0.001)
    txlc[5] ~ dnorm(0, 0.001)
    for (k in 1:3) {lc[k] ~ dnorm(0, 0.001)}
    for (j in 2:4) {n[j] ~ dnorm(0, 0.001)}
    for (p in 1:3) {txlc[p] ~ dnorm(0, 0.001)}
    
    tau.id <- pow(sig, -2)
    sig.id ~ dunif(0, 10)
    
    tau <- pow(sig, -2)
    sig ~ dunif(0, 10)
    }
    ",fill = TRUE)
sink()


# Bundle data
jags.data <- list(landcover=landcover, night=night, tide=tide_s,
                  odba=log_odba, yday=yday_s,individual=individual, N = nrow(birds1))


# Initial values
inits <- function (){list(lc=c(0,0,0,NA,0), txlc=c(0,0,0,NA,0),n=c(NA,0,0,0), alpha = 0)}

# Parameters monitored
parameters <- c("lc","t","n", "yd","txlc","id","odba","alpha","sig")

# Call jags from R
el8_all <- jags(data=jags.data, inits=inits, parameters.to.save=parameters, model.file="el8_all.jags", 
                n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, parallel=T)
