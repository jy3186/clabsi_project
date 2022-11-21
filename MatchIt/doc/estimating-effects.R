## ---- include = FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, eval=T)
options(width = 200, digits= 4)

me_ok <- requireNamespace("marginaleffects", quietly = TRUE) &&
  requireNamespace("sandwich", quietly = TRUE)
su_ok <- requireNamespace("survival", quietly = TRUE)
boot_ok <- requireNamespace("boot", quietly = TRUE)

#Generating data similar to Austin (2009) for demonstrating treatment effect estimation
gen_X <- function(n) {
  X <- matrix(rnorm(9 * n), nrow = n, ncol = 9)
  X[,5] <- as.numeric(X[,5] < .5)
  X
}

#~20% treated
gen_A <- function(X) {
  LP_A <- - 1.2 + log(2)*X[,1] - log(1.5)*X[,2] + log(2)*X[,4] - log(2.4)*X[,5] + log(2)*X[,7] - log(1.5)*X[,8]
  P_A <- plogis(LP_A)
  rbinom(nrow(X), 1, P_A)
}

# Continuous outcome
gen_Y_C <- function(A, X) {
  2*A + 2*X[,1] + 2*X[,2] + 2*X[,3] + 1*X[,4] + 2*X[,5] + 1*X[,6] + rnorm(length(A), 0, 5)
}
#Conditional:
#  MD: 2
#Marginal:
#  MD: 2

# Binary outcome
gen_Y_B <- function(A, X) {
  LP_B <- -2 + log(2.4)*A + log(2)*X[,1] + log(2)*X[,2] + log(2)*X[,3] + log(1.5)*X[,4] + log(2.4)*X[,5] + log(1.5)*X[,6]
  P_B <- plogis(LP_B)
  rbinom(length(A), 1, P_B)
}
#Conditional:
#  OR:   2.4
#  logOR: .875
#Marginal:
#  RD:    .144
#  RR:   1.54
#  logRR: .433
#  OR:   1.92
#  logOR  .655

# Survival outcome
gen_Y_S <- function(A, X) {
  LP_S <- -2 + log(2.4)*A + log(2)*X[,1] + log(2)*X[,2] + log(2)*X[,3] + log(1.5)*X[,4] + log(2.4)*X[,5] + log(1.5)*X[,6]
  sqrt(-log(runif(length(A)))*2e4*exp(-LP_S))
}
#Conditional:
#  HR:   2.4
#  logHR: .875
#Marginal:
#  HR:   1.57
#  logHR: .452

set.seed(19599)

n <- 2000
X <- gen_X(n)
A <- gen_A(X)

Y_C <- gen_Y_C(A, X)
Y_B <- gen_Y_B(A, X)
Y_S <- gen_Y_S(A, X)

d <- data.frame(A, X, Y_C, Y_B, Y_S)

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
head(d)

## ----message=FALSE,warning=FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------------------
library("MatchIt")

## ----message=FALSE,warning=FALSE, eval=me_ok----------------------------------------------------------------------------------------------------------------------------------------------------------
library("marginaleffects")

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Optimal full matching on the PS for the ATT
mF <- matchit(A ~ X1 + X2 + X3 + X4 + X5 + 
                 X6 + X7 + X8 + X9, data = d,
               method = "full", estimand = "ATT")
mF

#Extract matched data
md <- match.data(mF)

head(md)

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Linear model with covariates
fit1 <- lm(Y_C ~ A * (X1 + X2 + X3 + X4 + X5 + 
                        X6 + X7 + X8 + X9),
           data = md, weights = weights)

## ---- eval=me_ok--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
comp1 <- comparisons(fit1,
                     variables = "A",
                     vcov = ~subclass,
                     newdata = subset(md, A == 1),
                     wts = "weights")
summary(comp1)

## ---- eval=me_ok--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
pred1 <- predictions(fit1,
                     variables = "A",
                     vcov = ~subclass,
                     newdata = subset(md, A == 1),
                     wts = "weights",
                     by = "A")

summary(pred1)

## ---- eval=me_ok--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Subclassification on the PS for the ATT
mS <- matchit(A ~ X1 + X2 + X3 + X4 + X5 + 
                 X6 + X7 + X8 + X9, data = d,
               method = "subclass", estimand = "ATT")

#Extract matched data
md <- match.data(mS)

fitS <- lm(Y_C ~ subclass * (A * (X1 + X2 + X3 + X4 + X5 + 
                                    X6 + X7 + X8 + X9)),
           data = md)

compS <- comparisons(fitS,
                     variables = "A",
                     vcov = "HC3",
                     newdata = subset(md, A == 1))
summary(compS)

## ---- eval=me_ok--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Logistic regression model with covariates
fit2 <- glm(Y_B ~ A * (X1 + X2 + X3 + X4 + X5 + 
                        X6 + X7 + X8 + X9),
            data = md, weights = weights,
            family = quasibinomial())

#Compute effects
comp2 <- comparisons(fit2,
                     variables = "A",
                     vcov = ~subclass,
                     newdata = subset(md, A == 1),
                     wts = "weights",
                     transform_pre = "lnratioavg")

#Log RR, standard error, and Z value
summary(comp2)

#RR and confidence interval
summary(comp2, transform_avg = exp)

## ---- eval=su_ok--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library("survival")

#Cox Regression for marginal HR
coxph(Surv(Y_S) ~ A, data = md, robust = TRUE, 
      weights = weights, cluster = subclass)

## ---- eval = F----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  #get_matches() after matching with replacement
#  gm <- get_matches(mR)
#  
#  #Austin & Cafri's (2020) SE estimator
#  fs <- coxph(Surv(Y_S) ~ A, data = gm, robust = TRUE,
#        weights = weights, cluster = subclass)
#  Vs <- fs$var
#  ks <- nlevels(gm$subclass)
#  
#  fi <- coxph(Surv(Y_S) ~ A, data = gm, robust = TRUE,
#        weights = weights, cluster = id)
#  Vi <- fi$var
#  ki <- length(unique(gm$id))
#  
#  fc <- coxph(Surv(Y_S) ~ A, data = gm, robust = TRUE,
#        weights = weights)
#  Vc <- fc$var
#  kc <- nrow(gm)
#  
#  #Compute the variance and sneak it back into the fit object
#  fc$var <- (ks/(ks-1))*Vs + (ki/(ki-1))*Vi - (kc/(kc-1))*Vc
#  
#  fc

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
boot_fun <- function(data, i) {
  boot_data <- data[i,]
  
  #Do 1:1 PS matching with replacement
  m <- matchit(A ~ X1 + X2 + X3 + X4 + X5 + 
                 X6 + X7 + X8 + X9,
               data = boot_data,
               replace = TRUE)
  
  #Extract matched dataset
  md <- match.data(m, data = boot_data)
  
  #Fit outcome model
  fit <- glm(Y_B ~ A * (X1 + X2 + X3 + X4 + X5 + 
                 X6 + X7 + X8 + X9),
             data = md, weights = weights,
             family = quasibinomial())
  
  ## G-computation ##
  #Subset to treated units for ATT; skip for ATE
  md1 <- subset(md, A == 1)
  
  #Estimated potential outcomes under treatment
  p1 <- predict(fit, type = "response",
                newdata = transform(md1, A = 1))
  Ep1 <- weighted.mean(p1, md1$weights)
  
  #Estimated potential outcomes under control
  p0 <- predict(fit, type = "response",
                newdata = transform(md1, A = 0))
  Ep0 <- weighted.mean(p0, md1$weights)
  
  #Risk ratio
  return(Ep1 / Ep0)
}

## ---- eval = boot_ok, message=F, warning=F------------------------------------------------------------------------------------------------------------------------------------------------------------
library("boot")
set.seed(54321)
boot_out <- boot(d, boot_fun, R = 199)

boot_out
boot.ci(boot_out, type = "perc")

## ---- include = FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
b <- {
  if (boot_ok) boot.ci(boot_out, type = "perc")
  else list(t0 = 1.347, percent = c(0, 0, 0, 1.144, 1.891))
}

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
mNN <- matchit(A ~ X1 + X2 + X3 + X4 + X5 + 
                 X6 + X7 + X8 + X9, data = d)
mNN

md <- match.data(mNN)

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Unique pair IDs
pair_ids <- levels(md$subclass)

#Unit IDs, split by pair membership
split_inds <- split(seq_len(nrow(md)), md$subclass)

cluster_boot_fun <- function(pairs, i) {
  
  #Extract units corresponding to selected pairs
  ids <- unlist(split_inds[pairs[i]])
  
  #Subset md with block bootstrapped indices
  boot_md <- md[ids,]
  
  #Fit outcome model
  fit <- glm(Y_B ~ A * (X1 + X2 + X3 + X4 + X5 + 
                 X6 + X7 + X8 + X9),
             data = boot_md, weights = weights,
             family = quasibinomial())
  
  ## G-computation ##
  #Subset to treated units for ATT; skip for ATE
  md1 <- subset(boot_md, A == 1)
  
  #Estimated potential outcomes under treatment
  p1 <- predict(fit, type = "response",
                newdata = transform(md1, A = 1))
  Ep1 <- weighted.mean(p1, md1$weights)
  
  #Estimated potential outcomes under control
  p0 <- predict(fit, type = "response",
                newdata = transform(md1, A = 0))
  Ep0 <- weighted.mean(p0, md1$weights)
  
  #Risk ratio
  return(Ep1 / Ep0)
}

## ---- eval = boot_ok, message=F, warning=F------------------------------------------------------------------------------------------------------------------------------------------------------------
library("boot")
set.seed(54321)
cluster_boot_out <- boot(pair_ids, cluster_boot_fun,
                         R = 199)

cluster_boot_out
boot.ci(cluster_boot_out, type = "perc")

## ---- include = FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
b <- {
  if (boot_ok) boot.ci(cluster_boot_out, type = "perc")
  else list(t0 = 1.588, percent = c(0,0,0, 1.348, 1.877))
} 

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
mP <- matchit(A ~ X1 + X2 + X5*X3 + X4 + 
                X5*X6 + X7 + X5*X8 + X9, data = d,
              exact = ~X5, method = "nearest")
mP

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
mdP <- match.data(mP)

fitP <- lm(Y_C ~ A * X5, data = mdP, weights = weights)

## ---- eval=me_ok--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
comp <- comparisons(fitP, variables = "A",
                    vcov = ~subclass,
                    newdata = subset(md, A == 1),
                    wts = "weights",
                    by = "X5")
summary(comp)

## ---- eval=me_ok--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
comp <- comparisons(fitP, variables = "A",
                    vcov = ~subclass,
                    newdata = subset(md, A == 1),
                    wts = "weights",
                    by = "X5",
                    hypothesis = "pairwise")
summary(comp)

## ---- eval = FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  #Generating data similar to Austin (2009) for demonstrating treatment effect estimation
#  gen_X <- function(n) {
#    X <- matrix(rnorm(9 * n), nrow = n, ncol = 9)
#    X[,5] <- as.numeric(X[,5] < .5)
#    X
#  }
#  
#  #~20% treated
#  gen_A <- function(X) {
#    LP_A <- - 1.2 + log(2)*X[,1] - log(1.5)*X[,2] + log(2)*X[,4] - log(2.4)*X[,5] + log(2)*X[,7] - log(1.5)*X[,8]
#    P_A <- plogis(LP_A)
#    rbinom(nrow(X), 1, P_A)
#  }
#  
#  # Continuous outcome
#  gen_Y_C <- function(A, X) {
#    2*A + 2*X[,1] + 2*X[,2] + 2*X[,3] + 1*X[,4] + 2*X[,5] + 1*X[,6] + rnorm(length(A), 0, 5)
#  }
#  #Conditional:
#  #  MD: 2
#  #Marginal:
#  #  MD: 2
#  
#  # Binary outcome
#  gen_Y_B <- function(A, X) {
#    LP_B <- -2 + log(2.4)*A + log(2)*X[,1] + log(2)*X[,2] + log(2)*X[,3] + log(1.5)*X[,4] + log(2.4)*X[,5] + log(1.5)*X[,6]
#    P_B <- plogis(LP_B)
#    rbinom(length(A), 1, P_B)
#  }
#  #Conditional:
#  #  OR:   2.4
#  #  logOR: .875
#  #Marginal:
#  #  RD:    .144
#  #  RR:   1.54
#  #  logRR: .433
#  #  OR:   1.92
#  #  logOR  .655
#  
#  # Survival outcome
#  gen_Y_S <- function(A, X) {
#    LP_S <- -2 + log(2.4)*A + log(2)*X[,1] + log(2)*X[,2] + log(2)*X[,3] + log(1.5)*X[,4] + log(2.4)*X[,5] + log(1.5)*X[,6]
#    sqrt(-log(runif(length(A)))*2e4*exp(-LP_S))
#  }
#  #Conditional:
#  #  HR:   2.4
#  #  logHR: .875
#  #Marginal:
#  #  HR:   1.57
#  #  logHR: .452
#  
#  set.seed(19599)
#  
#  n <- 2000
#  X <- gen_X(n)
#  A <- gen_A(X)
#  
#  Y_C <- gen_Y_C(A, X)
#  Y_B <- gen_Y_B(A, X)
#  Y_S <- gen_Y_S(A, X)
#  
#  d <- data.frame(A, X, Y_C, Y_B, Y_S)

