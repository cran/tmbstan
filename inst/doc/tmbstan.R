## ----setup, include=FALSE, message=FALSE--------------------------------------
##library(knitr)
library(RTMB)
library(tmbstan)
set.seed(1)
formals(MakeADFun)$silent <- TRUE

## -----------------------------------------------------------------------------
## Data
data(ChickWeight)

## Parameters and random effects
parameters <- list(
    mua=0,          ## Mean slope
    sda=1,          ## Std of slopes
    mub=0,          ## Mean intercept
    sdb=1,          ## Std of intercepts
    sdeps=1,        ## Residual Std
    a=rep(0, 50),   ## Random slope by chick
    b=rep(0, 50)    ## Random intercept by chick
)

## Negative log likelihood
f <- function(parms) {
    getAll(ChickWeight, parms, warn=FALSE)
    ## Optional (enables extra RTMB features)
    weight <- OBS(weight)
    ## Initialize joint negative log likelihood
    nll <- 0
    ## Random slopes
    nll <- nll - sum(dnorm(a, mean=mua, sd=sda, log=TRUE))
    ## Random intercepts
    nll <- nll - sum(dnorm(b, mean=mub, sd=sdb, log=TRUE))
    ## Data
    predWeight <- a[Chick] * Time + b[Chick]
    nll <- nll - sum(dnorm(weight, predWeight, sd=sdeps, log=TRUE))
    ## Get predicted weight uncertainties
    ADREPORT(predWeight)
    ## Return
    nll
}

## -----------------------------------------------------------------------------
obj <- MakeADFun(f, parameters, random=c("a", "b"))

## -----------------------------------------------------------------------------
out <- tmbstan(obj, chains=1, seed=1)

## -----------------------------------------------------------------------------
TapeConfig(vectorize="enable")  ## Enable vectorization
obj_vect <- MakeADFun(f, parameters, random=c("a", "b"))
TapeConfig(vectorize="disable") ## Restore previous settings

## -----------------------------------------------------------------------------
## Timing without vectorization
t <- GetTape(obj)$timer(rep=1e4,total=TRUE)
## Timing with vectorization
t_vect <- GetTape(obj_vect)$timer(rep=1e4,total=TRUE)
## speedup ?
c(speedup = unname(t / t_vect))

## -----------------------------------------------------------------------------
system.time(out <- tmbstan(obj, chains=1, seed=1, refresh = 0))
system.time(out_vect <- tmbstan(obj_vect, chains=1, seed=1, refresh = 0))

