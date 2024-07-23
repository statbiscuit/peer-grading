load("student_and_tutor-grades.RData")
require(RTMB)
## discrete beta PMF with support [0, 14]
discrete_beta <- function(params){
    getAll(data, params)
    mu <- exp(logit_mu)/(1 + exp(logit_mu))
    shape1 <-  ((1 - mu) / sigma2 - 1 / mu) * mu^2
    shape2 <- shape1 * (1 / mu - 1)
    
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
    nll <-  -sum(log(ddbeta(y, exp(shape1), exp(shape2))))
    ADREPORT(shape1)
    ADREPORT(shape2)
    ADREPORT(mu)
    ADREPORT(sigma2)
    return(nll)
}
## data and parameters
data <- list(y = peer_grades$score)
mean(data$y) ## 12.0
 mean(data$y/14) ## 0.8628687
params <- list(logit_mu = 0.8)
## RTMB
discrete_beta(params = params)
obj <- MakeADFun(discrete_beta, params)
opt <- nlminb(obj$par, obj$fn, obj$gr,
              lower = c(0,0), upper = c(1, 0.25))
summary(sdreport(obj), "report")
