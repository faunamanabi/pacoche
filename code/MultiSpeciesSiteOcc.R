


#### code from: http://www.esajournals.org/doi/abs/10.1890/0012-9658%282006%2987%5B842%3AESRAAB%5D2.0.CO%3B2
#### full article here: http://www.uvm.edu/rsenr/vtcfwru/spreadsheets/occupancy/Occupancy%20Exercises/Exercise15/Dorazio_et_al_2006.pdf
#### example here http://www.esapubs.org/Archive/ecol/E087/050/suppl-1.htm 


library(R2WinBUGS)

# The model
sink("MultiSpeciesSiteOccModel")
cat("
model {

omega ~ dunif(0,1)
    
    psi.mean ~ dunif(0,1)
    alpha <- log(psi.mean) - log(1-psi.mean)
    
    theta.mean ~ dunif(0,1)
    beta <- log(theta.mean) - log(1-theta.mean)
    
    tau.u ~ dgamma(0.1,0.1)
    tau.v ~ dgamma(0.1,0.1)
    rho ~ dunif(-1,1)
    var.eta <- tau.v/(1.-pow(rho,2))
    
    sigma.u <- 1/sqrt(tau.u)
    sigma.v <- 1/sqrt(tau.v)
    
    
    for (i in 1:(n+nzeroes)) {
    w[i] ~ dbin(omega, 1)
    phi[i] ~ dnorm(alpha, tau.u)
    
    mu.eta[i] <- beta + (rho*sigma.v/sigma.u)*(phi[i] - alpha)
    eta[i] ~ dnorm(mu.eta[i], var.eta)
    
    
    logit(psi[i]) <- phi[i]
    logit(theta[i]) <- eta[i]
    
    mu.psi[i] <- psi[i]*w[i]
    for (j in 1:J) {
    Z[i,j] ~ dbin(mu.psi[i], 1)
    mu.theta[i,j] <- theta[i]*Z[i,j]
    X[i,j] ~ dbin(mu.theta[i,j], K)
    }
    }
    
    n0 <- sum(w[(n+1):(n+nzeroes)])
    N <- n + n0
    }
    ", fill=TRUE)
sink()

############################

MultiSpeciesSiteOcc <- function(nrepls, X) {
  
  start.time = Sys.time()
  
  # augment data matrix with an arbitrarily large number of zero row vectors
  nzeroes = 100
  n = dim(X)[1]
  nsites = dim(X)[2]
  Xaug = rbind(X, matrix(0, nrow=nzeroes, ncol=nsites))
  
  # create arguments for bugs()
  sp.data = list(n=n, nzeroes=nzeroes, J=nsites, K=nrepls, X=Xaug)
  
  sp.params = list('alpha', 'beta', 'rho', 'sigma.u', 'sigma.v', 'omega', 'N')
  
  sp.inits = function() {
    omegaGuess = runif(1, n/(n+nzeroes), 1)
    psi.meanGuess = runif(1, .25,1)
    theta.meanGuess = runif(1, .25,1)
    rhoGuess = runif(1, 0,1)
    sigma.uGuess = 1
    sigma.vGuess = 1
    list(omega=omegaGuess, psi.mean=psi.meanGuess, theta.mean=theta.meanGuess, tau.u=1/(sigma.uGuess^2), tau.v=1/(sigma.vGuess^2), rho=rhoGuess,
         w=c(rep(1, n), rbinom(nzeroes, size=1, prob=omegaGuess)),
         phi=rnorm(n+nzeroes, log(psi.meanGuess/(1.-psi.meanGuess)), sigma.uGuess),
         eta=rnorm(n+nzeroes, log(theta.meanGuess/(1.-theta.meanGuess)), sigma.vGuess),
         Z = matrix(rbinom((n+nzeroes)*nsites, size=1, prob=psi.meanGuess), nrow=(n+nzeroes))
    )
  }
  
  ## fit model to data using WinBUGS code
  library(R2WinBUGS)
  fit = bugs(sp.data, sp.inits, sp.params,
             model.file='MultiSpeciesSiteOccModel',
             debug=F, n.chains=4, n.iter=5500, n.burnin=500, n.thin=50)

#   library(R2jags)  
#   fit <- jags.model(sp.data, inits=sp.inits,  params = sp.params, 
#                    "MultiSpeciesSiteOccModel", n.chains = 3, n.thin = 50, n.iter = 5500, n.burnin = 500)
#   
  
  end.time = Sys.time()
  elapsed.time = difftime(end.time, start.time, units='mins')
  cat(paste(paste('Posterior computed in ', elapsed.time, sep=''), ' minutes\n', sep=''))
  
  list(fit=fit, data=sp.data, X=X)
}

##############################
# Cum Num Species Present ###
###############################

CumNumSpeciesPresent = function(nsites, alpha, sigmaU, N) {
  
  # Computes a sample of the posterior-predictive distribution of the (cumulative) number of species present at nsites.
  
  # compute posterior predictions of species occurrence probabilities
  ndraws = length(alpha)
  Nmax = max(N)
  logitPsi = matrix(NA, nrow=ndraws, ncol=Nmax)
  psi = logitPsi
  for (i in 1:ndraws) {
    logitPsi[i,1:N[i]] = rnorm(N[i], mean=alpha[i], sd=sigmaU[i])
    psi[i, 1:N[i]] = 1/(1+exp(-logitPsi[i,1:N[i]]))
  }
  
  # compute posterior predictions of species presence at each site
  z = array(NA, dim=c(ndraws, Nmax, nsites))
  for (i in 1:ndraws) {
    for (j in 1:N[i]) {
      z[i,j, ] = rbinom(nsites, size=1, prob=psi[i,j])
    }
  }
  
  # compute posterior predictions of cumulative number of species present
  M = matrix(NA, nrow=ndraws, ncol=nsites)
  for (i in 1:ndraws) {
    for (j in 1:nsites) {
      zsum = rep(NA, N[i])
      if (j>1) {
        zsum = apply(z[i, 1:N[i], 1:j], 1, sum)
      }
      else {
        zsum = z[i, 1:N[i], 1]
      }
      M[i,j] = sum(zsum>0)
    }
  }
  
  # compute summary stats for plotting
  nSpeciesPresent = matrix(NA, nrow=3, ncol=nsites)
  for (j in 1:nsites) {
    x = M[,j]
    nSpeciesPresent[1, j] = mean(x)
    nSpeciesPresent[2:3, j] = quantile(x, probs=c(.05, .95))
  }
  
  # plot results
  ylimits = c(min(nSpeciesPresent[2,]), max(nSpeciesPresent[3,]))
  plot(1:nsites, nSpeciesPresent[1,], pch=16, ylim=ylimits, type='b',
       xlab='Number of sampled sites', ylab='Number of species', las=1, cex.axis=1.2, cex.lab=1.5, cex=1.5)
  segments(1:nsites, nSpeciesPresent[2,], 1:nsites, nSpeciesPresent[3,]) #original xlab='Number of sampled plots'
  
  list(meanAndquantiles=nSpeciesPresent, summaryStats=summary(M))
}

##########################
#  apply model ###########
#########################

# X1 = as.matrix(read.csv("data/algas.csv"))
# nrepls = 12
# algas = MultiSpeciesSiteOcc(nrepls, X1)
# 
# summary(algas$fit$sims.matrix)
# 
# alpha.post = algas$fit$sims.matrix[,"alpha"]
# sigmaU.post = algas$fit$sims.matrix[,"sigma.u"]
# N.post = algas$fit$sims.matrix[,"N"]
# 
# nsites = 30
# CumNumSpeciesPresent(nsites, alpha.post, sigmaU.post, N.post)

