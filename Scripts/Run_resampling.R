# T1.2: Function to run resampling for parameter uncertainty #

# Takes input of mean matrix and uncertainty %

# runs all combinations of uncertainty and produces a combined dataframe of output

################################################################################

#### Set up ####

## load packages ####

library(tidyverse)

## source scripts ####

source("./Functions/parametric_bootstrap_function.R")
source("./Functions/run_multiple_resampling.R")

## set up matrices ####

# 2x2 
load("./Data files/twobytwo_breed_once.RData")
two_by_two_one <- to_save
load("./Data files/twobytwo_breed_all.RData")
two_by_two_mult <- to_save

# 3x3
load("./Data files/threebythree_breed_once.RData")
three_by_three_one <- to_save
load("./Data files/threebythree_breed_all.RData")
three_by_three_mult <- to_save

# 5x5
load("./Data files/fivebyfive_breed_once.RData")
five_by_five_one <- to_save
load("./Data files/fivebyfive_breed_all.RData")
five_by_five_mult <- to_save

## set up uncertainty ####

uncertainty <- read.csv("./Data files/Uncertainty_summary.csv")

fecundity_uncertainty <- uncertainty[2,2:4]
survival_uncertainty <- uncertainty[1,2:4]

################################################################################

#### SCENARIO 1: 2x2 matrices ####

# can probably do this using pmap: matrix_number, breeding_stages, mean_matrix,
# uncertainty_fecundity, uncertainty_survival, uncertainty_level

inputs <- list(matrix_number = as.list(rep(rep(1:5, each = 10), 2)),
               breeding_stages = as.list(rep(c("one", "multiple"),
                                       each = 50)),
               mean_matrix = c(rep(two_by_two_one, each = 10),
                               rep(two_by_two_mult, each = 10)),
               uncertainty_fecundity = as.list(rep(c("NULL",
                                         as.numeric(fecundity_uncertainty),
                                         rep("NULL", 3),
                                         as.numeric(fecundity_uncertainty)),10)),
               uncertainty_survival = as.list(rep(c(rep("NULL", 4),
                                        rep(as.numeric(survival_uncertainty),
                                            2)),10)),
               uncertainty_level = as.list(rep(c("none", 
                                     rep(c("low", "mid", "high"), 3)),10)))

two_by_two_results <- pmap(inputs, .f = run_multiple_resampling) %>% 
  bind_rows

# save out
save(two_by_two_results, file = "./Data files/two_by_two_results.RData")

#### SCENARIO 2: 3x3 matrices ####

# can probably do this using pmap: matrix_number, breeding_stages, mean_matrix,
# uncertainty_fecundity, uncertainty_survival, uncertainty_level

inputs <- list(matrix_number = as.list(rep(rep(1:5, each = 10), 2)),
               breeding_stages = as.list(rep(c("one", "multiple"),
                                             each = 50)),
               mean_matrix = c(rep(three_by_three_one, each = 10),
                               rep(three_by_three_mult, each = 10)),
               uncertainty_fecundity = as.list(rep(c("NULL",
                                                     as.numeric(fecundity_uncertainty),
                                                     rep("NULL", 3),
                                                     as.numeric(fecundity_uncertainty)),10)),
               uncertainty_survival = as.list(rep(c(rep("NULL", 4),
                                                    rep(as.numeric(survival_uncertainty),
                                                        2)),10)),
               uncertainty_level = as.list(rep(c("none", 
                                                 rep(c("low", "mid", "high"), 3)),10)))


three_by_three_results <- pmap(inputs, .f = run_multiple_resampling) %>% 
  bind_rows

# save out
save(three_by_three_results, file = "./Data files/three_by_three_results.RData")

#### SCENARIO 3: 5x5 matrices ####

# can probably do this using pmap: matrix_number, breeding_stages, mean_matrix,
# uncertainty_fecundity, uncertainty_survival, uncertainty_level

inputs <- list(matrix_number = as.list(rep(rep(1:5, each = 10), 2)),
               breeding_stages = as.list(rep(c("one", "multiple"),
                                             each = 50)),
               mean_matrix = c(rep(five_by_five_one, each = 10),
                               rep(five_by_five_mult, each = 10)),
               uncertainty_fecundity = as.list(rep(c("NULL",
                                                     as.numeric(fecundity_uncertainty),
                                                     rep("NULL", 3),
                                                     as.numeric(fecundity_uncertainty)),10)),
               uncertainty_survival = as.list(rep(c(rep("NULL", 4),
                                                    rep(as.numeric(survival_uncertainty),
                                                        2)),10)),
               uncertainty_level = as.list(rep(c("none", 
                                                 rep(c("low", "mid", "high"), 3)),10)))


five_by_five_results <- pmap(inputs, .f = run_multiple_resampling) %>% 
  bind_rows

# save out
save(five_by_five_results, file = "./Data files/five_by_five_results.RData")

################################################################################

#### SUMMARISE RESULTS ####

# calculate mean matrix results

w <- eigen(matrix3)$vectors
v <- Conj(solve(w))

senmat <- Re(v[1,] %*% t(w[,1])) # calculate sensitivity
emat <- (1/(Re(lambda))) * senmat * matrix3

elasticity <- which(emat == max(emat))


################################################################################

