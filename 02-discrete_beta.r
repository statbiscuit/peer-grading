load("student_and_tutor-grades.RData")
require(RTMB)

#########################
#### Data summaries #####
#########################
### peers ##
mean(peer_grades$score)
## 12.08016
mean(peer_grades$score)/14
## 0.8628687
sd((peer_grades$score)/14)
## 0.1606932
1/sd((peer_grades$score)/14)
## 6.223038
### tutors ##
mean(tutor_grades$total)
## 12.93266
mean(tutor_grades$total)/14
## 0.9237613
sd((tutor_grades$total)/14)
##  0.1199469
1/sd((tutor_grades$total)/14)
##  8.337019

##########################
#### LME4 equivalent #####
##########################
## NOT a sensible model ##
## for EXAMPLE only ######
##########################

##### LME4 ###############
## peers ##
lme4::lmer(score ~ (1|file_name) + (1|assessor_id), data = peer_grades) |>
 summary()
## tutors ##
lme4::lmer(total ~ (1|file_name) + (1|grader), data = tutor_grades) |>
summary()
## both estimate variance grader < submission
##### RTMB ###############
## function that returns negative log likelihood
f <- function(parms) {
    getAll(data, parms, warn = FALSE)
    y <- OBS(y)
    ## Initialize joint negative log likelihood
    nll <- 0
    ## Random grader
    nll <- nll - sum(dnorm(a, mean = 0, sd = sda, log = TRUE))
    ## Random submission
    nll <- nll - sum(dnorm(b, mean = 0, sd = sdb, log = TRUE))
    ## Data
    mu <- beta0 + a[grader] + b[submission]
    nll <- nll - sum(dnorm(y, mu, sd = sigma, log = TRUE))
    grader_variance <- sda^2
    submission_variance <- sdb^2
    residual_variance <- sigma^2
    ## report estimated parameters
    ADREPORT(beta0)
    ADREPORT(grader_variance)
    ADREPORT(submission_variance)
    ADREPORT(residual_variance)
    return(nll)
}
## peers ##
data <- list(y = peer_grades$score,
             grader = as.factor(peer_grades$assessor_id),
             submission = as.factor(peer_grades$file_name))
parameters <- list(beta0 = 12, sda = 1,  sdb = 1,         
    sigma = 1, a = rep(0, length(table(data$grader))),  
    b = rep(0, length(table(data$submission))))
obj <- MakeADFun(f, parameters, random = c("a", "b"), silent = TRUE)
opt <- optim(obj$par, obj$fn, obj$gr)
summary(sdreport(obj), "report")
## tutors ##
data <- list(y = tutor_grades$total,
             grader = as.factor(tutor_grades$grader),
             submission = as.factor(tutor_grades$file_name))
parameters <- list(beta0 = 12, sda = 1,  sdb = 1,         
    sigma = 1, a = rep(0, length(table(data$grader))),  
    b = rep(0, length(table(data$submission))))
obj <- MakeADFun(f, parameters, random = c("a", "b"), silent = TRUE)
opt <- optim(obj$par, obj$fn, obj$gr)
summary(sdreport(obj), "report") 

#################################################
##### discrete Beta models ######################
#################################################
### LME4 above for example only #################
#################################################


#########################
## discrete Beta (1) ####
#########################
## discrete beta PMF with support [0, 14]
## parameterised in terms of mu and sigma^2

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
    ADREPORT(mu)
    ADREPORT(sqrt(sigma2))
    return(nll)
}

#########################
## discrete Beta (2) ####
#########################
## discrete beta PMF with support [0, 14]
## parameterised in terms of mu and phi (precision)

discrete_beta_02 <- function(params){
    getAll(data, params)
    ## constrain mu is bounded between 0 and 1
    mu <- exp(logit_mu)/(1 + exp(logit_mu))
    ## constrain phi > 0
    phi <- exp(log_phi)
    ## define beta shape params in terms of mu and phi
    shape1 <-  mu * phi
    shape2 <- (1 - mu)  * phi
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
    ## mu and phi
    mu <- exp(shape1)/(exp(shape2) + exp(shape1))
    phi <- exp(shape2) + exp(shape1)
    ## report estimated parameters
    ADREPORT(mu)
    ADREPORT(phi)
    return(nll)
}


#################################################
##### discrete Beta with random effects (3) #####
#################################################
### Note (1) and (2) for example only ###########
#################################################

discrete_beta_random <- function(params){
    getAll(data, params)
    #y <- OBS(y)
    sda <- exp(log_sda) ## sd grader
    sdb <- exp(log_sdb) ## sd submission
    ## linear predictor
    logit_mu <-  beta0 + a[grader]  + b[submission]
    ## constrain mu is bounded between 0 and 1
    mu <- exp(logit_mu)/(1 + exp(logit_mu))
    ## constrain phi > 0
    phi <- exp(log_phi)
    ddbeta <- function(x, mu, phi){
        n <- length(x)
        res <- advector(numeric(n))
        for (i in 1:n){
            range <- c(max(c(0, x[i] - 0.125)), min(14, x[i] + 0.125))/14
            res[i] <- pbeta(range[2], mu[i] * phi, (1 - mu[i])  * phi) -
                pbeta(range[1],  mu[i] * phi, (1 - mu[i])  * phi)
        }
        return(log(res))
    }
    ## Negative log-likelihood
    ## Initialize joint negative log likelihood
    nll <- 0
    ## Random effect grader
    nll <- nll - sum(dnorm(a, mean = 0, sd = sda, log = TRUE))
    ## Random effect submission
    nll <- nll - sum(dnorm(b, mean = 0, sd = sdb, log = TRUE))
    ## data contribution
    nll <-  nll - sum(ddbeta(y, mu, phi))
    ## report estimated parameters
    ADREPORT(mu)
    ADREPORT(phi)
    ADREPORT(sda)
    ADREPORT(sdb)
    ADREPORT(beta0)
    return(nll)
}

#############################
##### Parameter estimation ##
#############################

## peers
## (1)
data <- list(y = peer_grades$score)
params <- list(logit_mu = 2, logit_sigma2 = -2)
obj <- MakeADFun(discrete_beta, params, silent = TRUE)
opt <- nlminb(obj$par, obj$fn, obj$gr)
summary(sdreport(obj), "report") |> round(3)
## (2)
params <- list(logit_mu = 2, log_phi = -2)
obj <- MakeADFun(discrete_beta_02, params, silent = TRUE)
opt <- nlminb(obj$par, obj$fn, obj$gr)
summary(sdreport(obj), "report")  |> round(3)
## (3)
data <- list(y = peer_grades$score,
             grader = as.factor(peer_grades$assessor_id),
             submission = as.factor(peer_grades$file_name))
params <- list(beta0 = 3,
               log_sda = 1.7,          
               log_sdb = 1.5,
               log_phi = log(6),        
               a = rep(0, length(table(data$grader))),
               b = rep(0, length(table(data$submission))))
obj <- MakeADFun(discrete_beta_random, params, random = c("a", "b"), silent = TRUE)
opt <-  optim(obj$par, obj$fn, obj$gr)
effsp <- as.list(sdreport(obj), "Est")
musp <- as.list(sdreport(obj), "Est", report = TRUE)
c(musp$beta0, musp$sda, musp$sdb, musp$phi) |> round(3)
## standard errors by Hessian
## and then the delta method
hess <- optimHess(opt$par, obj$fn)
vcov <- solve(hess)
## SEs transformed variables
ses <- sqrt(diag(vcov))
## beta0
ses[1] |> round(3)
## SEs back transformed variables
## by delta method, note
## d(exp(.))/dx = exp(.)
## sda
(exp(effsp$log_sda) %*% vcov[2,2] %*% exp(effsp$log_sda)) |> round(3)
## sdb
(exp(effsp$log_sdb) %*% vcov[3,3] %*% exp(effsp$log_sdb)) |> round(3)
## phi
(exp(effsp$log_phi) %*% vcov[4,4] %*% exp(effsp$log_phi)) |> round(3)

## tutors
## (1)
data <- list(y = tutor_grades$total)
params <- list(logit_mu = 2, logit_sigma2 = -2)
obj <- MakeADFun(discrete_beta, params, silent = TRUE)
opt <- nlminb(obj$par, obj$fn, obj$gr)
summary(sdreport(obj), "report") |> round(3)
## (2)
params <- list(logit_mu = 2, log_phi = -2)
obj <- MakeADFun(discrete_beta_02, params, silent = TRUE)
opt <- nlminb(obj$par, obj$fn, obj$gr)
summary(sdreport(obj), "report")  |> round(3)
## (3)
data <- list(y = tutor_grades$total,
             grader = as.factor(tutor_grades$grader),
             submission = as.factor(tutor_grades$file_name))
params <- list(beta0 = 3,
               log_sda = 1.7,          
               log_sdb = 1.5,
               log_phi = log(6),        
               a = rep(0, length(table(data$grader))),
               b = rep(0, length(table(data$submission))))
obj <- MakeADFun(discrete_beta_random, params, random = c("a", "b"), silent = TRUE)
opt <-  optim(obj$par, obj$fn, obj$gr)
effst <- as.list(sdreport(obj), "Est")
must <- as.list(sdreport(obj), "Est", report = TRUE)
c(must$beta0, must$sda, must$sdb, must$phi) |> round(3)
## standard errors by Hessian
## and then the delta method
hess <- optimHess(opt$par, obj$fn)
vcov <- solve(hess)
## SEs transformed variables
ses <- sqrt(diag(vcov))
## beta0
ses[1] |> round(3)
## SEs back transformed variables
## by delta method, again
## d(exp(.))/dx = exp(.)
## sda
(exp(effst$log_sda) %*% vcov[2,2] %*% exp(effst$log_sda)) |> round(3)
## sdb
(exp(effst$log_sdb) %*% vcov[3,3] %*% exp(effst$log_sdb)) |> round(3)
## phi
(exp(effst$log_phi) %*% vcov[4,4] %*% exp(effst$log_phi)) |> round(3)


#############################
########## PLOTS ############
#############################

require(ggplot2)
## peers
p <- ggplot(data.frame(x = seq(-4,4, length.out = 1000)), aes(x = x)) +
    stat_function(fun = dnorm, args = list(mean = 0, sd = musp$sda), col = "#1B9E77", linewidth = 2) +
    geom_dotplot(data = data.frame(x = effsp$a),  binwidth = 0.125, fill = "#1B9E77", col = NA) +
    stat_function(fun = dnorm, args = list(mean = 0, sd = musp$sdb), col = "#E7298A",  linewidth = 2) +
    geom_dotplot(data = data.frame(x = effsp$b),  binwidth = 0.125, fill = "#E7298A", alpha = 0.8, col = NA) +
    theme_classic() + xlab("") + ylab("") +
    annotate("text", x = -3, y = 1, label = expression(a[i] %~% N(0,0.5^2)), col = "#1B9E77") +
    annotate("text", x = -3.09, y = 0.95, label = expression(b[i] %~% N(0,1^2)), col = "#E7298A") 

## tutors
t <- ggplot(data.frame(x = seq(-4,4, length.out = 1000)), aes(x = x)) +
    stat_function(fun = dnorm, args = list(mean = 0, sd = must$sda), col = "#1B9E77", linewidth = 2) +
    geom_dotplot(data = data.frame(x = effst$a),  binwidth = 0.125, fill = "#1B9E77", col = NA) +
    stat_function(fun = dnorm, args = list(mean = 0, sd = must$sdb), col = "#E7298A",  linewidth = 2) +
    geom_dotplot(data = data.frame(x = effst$b),  binwidth = 0.09, fill = "#E7298A", alpha = 0.8, col = NA) +
    theme_classic() + xlab("") + ylab("") +
    annotate("text", x = -3, y = 1, label = expression(a[i] %~% N(0,0.5^2)), col = "#1B9E77") +
    annotate("text", x = -3, y = 0.95, label = expression(b[i] %~% N(0,1.3^2)), col = "#E7298A")

patchwork::wrap_plots(p,t) + patchwork::plot_annotation(tag_levels = 'A', tag_suffix = ")")
ggsave("plots/random_effects.png", width = 10, height = 5)



peers <- data.frame(y = c(peer_grades$score/14,  musp$mu),
                    x = rep(c("observed", "predicted"), nrow(peer_grades)))
p <- ggplot(peers, aes(x = x, y = y, col = x)) +
    geom_violin() +
    geom_jitter(alpha = 0.8) +
    scale_x_discrete("", c(0, 1),
                     labels = c(expression(paste(frac(X,14))), expression(hat(mu)))) +
    ylab("") + theme_bw() +
    scale_color_manual(values = c("#1B9E77", "#A6761D")) + 
    theme(legend.position = "none") + expand_limits(y = 0)
tutors <- data.frame(y = c(tutor_grades$total/14,  must$mu),
                    x = rep(c("observed", "predicted"), nrow(tutor_grades)))
t <- ggplot(tutors, aes(x = x, y = y, col = x)) +
    geom_violin() +
    geom_jitter(alpha = 0.7) +
     scale_x_discrete("", c(0, 1),
                      labels = c(expression(paste(frac(X,14))), expression(hat(mu)))) +
    ylab("") + theme_bw() +
    scale_color_manual(values = c("#1B9E77", "#A6761D")) + 
    theme(legend.position = "none")  +  expand_limits(y = 0)

patchwork::wrap_plots(p,t) + patchwork::plot_annotation(tag_levels = 'A', tag_suffix = ")")
ggsave("plots/fitted.png", width = 10, height = 5)
