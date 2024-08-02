load("student_and_tutor-grades.RData")
require(RTMB)
## discrete beta PMF with support [0, 14]
discrete_beta <- function(params){
    getAll(data, params)
    ## constrain mu is bounded between 0 and 1
    mu <- exp(logit_mu)/(1 + exp(logit_mu))
    ## constrain sigma between 0 and 1 (analytically sigma < mu)
    sigma2 <- (exp(logit_sigma2)/(1 + exp(logit_sigma2)))
    ## define beta shape params in terms of mu and sigma2
    shape1 <-  (((1 - mu) / sigma2) - (1 / mu)) * mu^2
    shape2 <- shape1 * ((1 / mu) - 1)
    ddbeta <- function(x, shape1, shape2){
        n <- length(x)
        res <- advector(numeric(n))
        for (i in 1:n){
            range <- c(max(c(0, x[i] - 0.125)), min(14, x[i] + 0.125))/14
            res[i] <- pbeta(range[2], shape1, shape2) -
                pbeta(range[1], shape1, shape2)
        }
        return(res)
    }
    ## Negative log-likelihood
    ## initlalise
    nll <- 0
    ## data
    nll <-  nll - sum(log(ddbeta(y, exp(shape1), exp(shape2))))
    ## mu and sigma2
    mu <- exp(shape1)/(exp(shape1) + exp(shape2))
    sigma2 <- (exp(shape1) * exp(shape2))/((exp(shape1) + exp(shape2) + 1) * (exp(shape1) + exp(shape2))^2)
    ## report estimated parameters
    ADREPORT(shape1)
    ADREPORT(shape2)
    ADREPORT(mu)
    ADREPORT(sigma2)
    return(nll)
}
## data and parameters
data <- list(y = peer_grades$score,
             grader = as.factor(peer_grades$assessor_id),
             submission = as.factor(peer_grades$file_name))
mean(data$y) ## 12.0
mean(data$y/14) ## 0.8628687
sd(data$y/14)^2 ## 0.02
params <- list(logit_mu = 2, logit_sigma2 = -2)
## RTMB
discrete_beta(params = params)
obj <- MakeADFun(discrete_beta, params)
opt <- nlminb(obj$par, obj$fn, obj$gr)
pars <- summary(sdreport(obj), "report")
pars

###########
## PLOT ###
###########

## require(ggplot2)
## ggplot(peer_grades, aes(x = score/14, y = after_stat(density))) +
##     geom_histogram(binwidth = 0.05) +
##     stat_function(fun = dbeta, args = list(shape1 = exp(pars[1,1]), shape2 = exp(pars[2,1])),
##                   inherit.aes = FALSE)


## lme4 just for syntax comparision
lmer4_mod <- lme4::lmer(score ~ (1|file_name) + (1|assessor_id), data = peer_grades)
summary(lmer4_mod)
## equatiomatic::extract_eq(lmer4_mod)

require(glmmTMB)
mod <- glmmTMB((score + 0.1)/14.5 ~ (1|file_name) + (1|assessor_id), data = peer_grades, family = beta_family())
summary(mod)
## modb <- glm((peer_grades$score + 0.1)/14.5 ~ 1 + rnorm(nrow(peer_grades)), family = beta_family())
## summary(modb)
## equatiomatic::extract_eq(modb)


######################
##### RANDOM #########
######################

discrete_beta_random <- function(params){
    getAll(data, params, warn = FALSE)
    y <- OBS(y)
    sda <- exp(log_sda)
    sdb <- exp(log_sdb)
    ## Initialize joint negative log likelihood
    nll <- 0
    ## Random effect grader
    nll <- nll - sum(dnorm(a, mean = 0, sd = sda, log = TRUE))
    ## Random effect submission
    nll <- nll - sum(dnorm(b, mean = 0, sd = sdb, log = TRUE))
    ## Data
    logit_mu <-  beta0 + a[grader]  + b[submission]
    mu <- exp(logit_mu)/(1 + exp(logit_mu))
    precision <- exp(log_precision)
    ## browser()
    ## discrete beta likelinhood function
    ddbeta <- function(x, mu, phi){
        n <- length(x)
        res <- advector(numeric(n))
        for (i in 1:n){
            range <- c(max(c(0, x[i] - 0.125)), min(14, x[i] + 0.125))/14
            res[i] <- pbeta(range[2], shape1 = mu[i] * phi,  shape2 = (1 - mu[i]) * phi) -
                pbeta(range[1], shape1 = mu[i] * phi,  shape2 = (1 - mu[i]) * phi)
        }
        return(res)
    }
    nll <- nll - sum(log(ddbeta(y, mu = mu, phi = precision)))
    ADREPORT(sda^2)
    ADREPORT(sdb^2)
    ADREPORT(beta0)
    ## ADREPORT(precision)
    ## ADREPORT(mu)
    return(nll)
}
## data and parameters
## data <- list(y = peer_grades$score,
##              grader = as.factor(peer_grades$assessor_id),
##              submission = as.factor(peer_grades$file_name))

data <- list(y = tutor_grades$total,
             grader = as.factor(tutor_grades$grader),
             submission = as.factor(tutor_grades$file_name))


params <- list(beta0 = 2,
               log_sda = -0.615,          
               log_sdb = -0.015,
               log_precision = 1.9,        
               a = rep(0, length(table(data$grader))),
               b = rep(0, length(table(data$submission))))

## RTMB
discrete_beta_random(params = params)
obj <- MakeADFun(discrete_beta_random, params, random = c("a", "b"))
opt <- optim(obj$par, obj$fn, obj$gr)
pars <- summary(sdreport(obj), "report")
pars

#### SIMULATE
set.seed(1234)
y <- plyr::round_any(14*rbeta(nrow(peer_grades), 8, 1), 0.25)

data <- list(y = y,
             grader = as.factor(peer_grades$assessor_id),
             submission = as.factor(peer_grades$file_name))
params <- list(beta0 = 2,
               log_sda = log(0.2),          
               log_sdb = log(0.9),
               log_precision = log(1.2),        
               a = rep(0, length(table(data$grader))),
               b = rep(0, length(table(data$submission))))

discrete_beta_random(params = params)
obj <- MakeADFun(discrete_beta_random, params, random = c("a", "b"))
opt <- optim(obj$par, obj$fn, obj$gr)
pars <- summary(sdreport(obj), "report")
pars


#### LME4 equivalent

f <- function(parms) {
    getAll(data, parms, warn = FALSE)
    y <- OBS(y)
    ## Initialize joint negative log likelihood
    nll <- 0
    ## Random grader
    nll <- nll - sum(dnorm(a, mean = 0, sd = sda, log=TRUE))
    ## Random submission
    nll <- nll - sum(dnorm(b, mean = 0, sd = sdb, log=TRUE))
    ## Data
    mu <- beta0 + a[grader] + b[submission]
    nll <- nll - sum(dnorm(y, mu, sd = sigma, log = TRUE))
    ADREPORT(mu)
    ADREPORT(sda^2)
    ADREPORT(sdb^2)
    ADREPORT(sigma^2)
    ## Return
    nll
}

parameters <- list(
    beta0 = 12,          
    sda = 1,          
    sdb = 1,          
    sigma = 1,        
    a = rep(0, length(table(data$grader))),   
    b = rep(0, length(table(data$submission)))    
)
obj <- MakeADFun(f, parameters, random = c("a", "b"))
opt <- nlminb(obj$par, obj$fn, obj$gr)
sdreport(obj)
pars <- summary(sdreport(obj), "report") ## matches lme4
pars
