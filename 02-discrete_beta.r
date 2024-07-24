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

require(ggplot2)
ggplot(peer_grades, aes(x = score/14, y = after_stat(density))) +
    geom_histogram(binwidth = 0.05) +
    stat_function(fun = dbeta, args = list(shape1 = exp(pars[1,1]), shape2 = exp(pars[2,1])),
                  inherit.aes = FALSE)


## lme4 just for syntax comparision
## lmer4_mod <- lme4::lmer(score ~ (1|file_name) + (1|assessor_id), data = peer_grades)
## summary(lmer4_mod)
## equatiomatic::extract_eq(lmer4_mod)

## require(glmmTMB)
## mod <- glmmTMB((score + 0.1)/14.5 ~ (1|file_name) + (1|assessor_id), data = peer_grades, family = beta_family())
## summary(mod)
## modb <- glm((peer_grades$score + 0.1)/14.5 ~ 1 + rnorm(nrow(peer_grades)), family = beta_family())
## summary(modb)
## equatiomatic::extract_eq(modb)
