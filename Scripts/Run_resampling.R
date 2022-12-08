# T1.2: Function to run resampling for parameter uncertainty #

# Takes input of mean matrix and uncertainty %

# runs all combinations of uncertainty and produces a combined dataframe of output

################################################################################

#### Set up ####

## load packages ####

library(tidyverse)

## source scripts ####

source("./Functions/parametric_bootstrap.R")

## set up matrices ####

matrix1 <- matrix(c(0.1378489, 3.5, 3.5,
                    0.01347, 0.169622, 0,
                    0.006739, 0.012918, 0.171508), byrow = TRUE, nrow = 3)


matrix2 <- matrix(c(0, 0.58125, 0.678125,
                    0.141, 0, 0,
                    0, 0.80400, 0.831), byrow = TRUE, nrow = 3)


matrix1 <- matrix(c(0.310025, 0.78410, 12,
                    0.179125, 0.54465, 0.1678,
                    0.01850, 0.02875, 0.5747), byrow = TRUE, nrow = 3)

## set up uncertainty ####

fecundity_uncertainty <- c(0.09, 0.22, 0.4)
survival_uncertainty <- c(0.005, 0.13, 0.38)


################################################################################

#### SCENARIO 1: Full propagation ####

# matrx 1: minimum uncertainty
full_prop_min1 <- rerun(1000, parametric_bootstrap_function(mean_matrix = matrix3,
                              uncertainty_fecundity = fecundity_uncertainty[1],
                              uncertainty_survival = survival_uncertainty[1]))

# matrx 1: minimum uncertainty
full_prop_max1 <- rerun(1000, parametric_bootstrap_function(mean_matrix = matrix3,
                              uncertainty_fecundity = fecundity_uncertainty[3],
                              uncertainty_survival = survival_uncertainty[3]))

#### SCENARIO 2: Fecundity propagation ####

# matrx 1: minimum uncertainty
f_prop_min1 <- rerun(1000, parametric_bootstrap_function(mean_matrix = matrix3,
                           uncertainty_fecundity = fecundity_uncertainty[1],
                           uncertainty_survival = NULL))

# matrx 1: minimum uncertainty
f_prop_max1 <- rerun(1000, parametric_bootstrap_function(mean_matrix = matrix3,
                           uncertainty_fecundity = fecundity_uncertainty[3],
                           uncertainty_survival = NULL))

#### SCENARIO 3: Survival propagation ####

# matrx 1: minimum uncertainty
s_prop_min1 <- rerun(1000, parametric_bootstrap_function(mean_matrix = matrix3,
                              uncertainty_fecundity = NULL,
                              uncertainty_survival = survival_uncertainty[1]))

# matrx 1: minimum uncertainty
s_prop_max1 <- rerun(1000, parametric_bootstrap_function(mean_matrix = matrix3,
                           uncertainty_fecundity = NULL,
                           uncertainty_survival = survival_uncertainty[3]))

################################################################################

#### SUMMARISE RESULTS ####

# calculate mean matrix results

w <- eigen(matrix3)$vectors
v <- Conj(solve(w))

senmat <- Re(v[1,] %*% t(w[,1])) # calculate sensitivity
emat <- (1/(Re(lambda))) * senmat * matrix3

elasticity <- which(emat == max(emat))

results_max1 <- results_min1 <- data.frame(model = "mean_matrix",
                      lambda = popdemo::eigs(matrix3, what = "lambda"),
                      elasticity = elasticity)

results_min1[2:1001,2] <- unlist(lapply(full_prop_min1, "[[", 1)) # extract lambdas
results_min1[1002:2001,2] <- unlist(lapply(f_prop_min1, "[[", 1)) # extract lambdas
results_min1[2002:3001,2] <- unlist(lapply(s_prop_min1, "[[", 1)) # extract lambdas

results_min1[2:3001, 1] <- rep(c("full", "F_only", "S_only"), each = 1000)

results_min1[2:1001,3] <- unlist(map(.x = lapply(full_prop_min1, "[[", 2), 
                               ~{x <- length(which(.x == elasticity)) > 0})) # extract elasticity
results_min1[1002:2001,3] <- unlist(map(.x = lapply(f_prop_min1, "[[", 2), 
                                ~{x <- length(which(.x == elasticity)) > 0})) # extract elasticity
results_min1[2002:3001,3] <- unlist(map(.x = lapply(s_prop_min1, "[[", 2), 
                                   ~{x <- length(which(.x == elasticity)) > 0})) # extract elasticity

results_max1[2:1001,2] <- unlist(lapply(full_prop_max1, "[[", 1)) # extract lambdas
results_max1[1002:2001,2] <- unlist(lapply(f_prop_max1, "[[", 1)) # extract lambdas
results_max1[2002:3001,2] <- unlist(lapply(s_prop_max1, "[[", 1)) # extract lambdas

results_max1[2:3001, 1] <- rep(c("full", "F_only", "S_only"), each = 1000)

results_max1[2:1001,3] <- unlist(map(.x = lapply(full_prop_max1, "[[", 2), 
                                ~{x <- length(which(.x == elasticity)) > 0})) # extract elasticity
results_max1[1002:2001,3] <- unlist(map(.x = lapply(f_prop_max1, "[[", 2), 
                                   ~{x <- length(which(.x == elasticity)) > 0})) # extract elasticity
results_max1[2002:3001,3] <- unlist(map(.x = lapply(s_prop_max1, "[[", 2), 
                                   ~{x <- length(which(.x == elasticity)) > 0})) # extract elasticity

################################################################################

#### Plot ####

# join datasets

results_joined <- bind_rows(results_min1, results_max1)
results_joined$scenario <- rep(c("min", "max"), each = 3001)

ggplot(data = results_joined, aes(x = model, y = lambda, colour = model))+
  geom_boxplot() +
  geom_point(data = filter(results_joined, model == "mean_matrix"), 
             aes(x = model, y = lambda)) +
  facet_wrap(~scenario, scales = "free_y")+
  theme_minimal()
