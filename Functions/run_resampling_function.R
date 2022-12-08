# T1.2: Function to run resampling for parameter uncertainty #

# Takes input of mean matrix (3x3) and uncertainty % for each
# survival and fecundity (vector of 3, min, mean, max)
# and matrix name

# runs all combinations of uncertainty and produces a combined dataframe of output

################################################################################

#### Set up ####

## load packages ####

library(tidyverse)

## source scripts ####

source("./Functions/parametric_bootstrap_function.R")
source("./Functions/resample_summary_function.R")

################################################################################

#### FUNCTION ####

run_resampling_function <- function(mean_matrix,
                            uncertainty_fecundity,
                            uncertainty_survival,
                            matrix_name){
  
  # calculate mean matrix results: CODE FROM COMPADRE TUTORIAL
  
  lambda = popdemo::eigs(mean_matrix, what = "lambda")
  
  w <- eigen(mean_matrix)$vectors
  v <- Conj(solve(w))
  
  senmat <- Re(v[1,] %*% t(w[,1])) # calculate sensitivity
  emat <- (1/(Re(lambda))) * senmat * mean_matrix
  
  elasticity <- which(emat == max(emat))
  
  results <- data.frame(matrix_name = matrix_name,
                        lambda = popdemo::eigs(mean_matrix, what = "lambda"),
                        elasticity = elasticity,
                        propagation_type = "mean_matrix",
                        uncertainty_level = NA,
                        in_CI = NA)
  
  #### SCENARIO 1: Full propagation ####
  
  # minimum uncertainty
  full_prop_min <- rerun(1000, parametric_bootstrap_function(mean_matrix = mean_matrix,
                               uncertainty_fecundity = uncertainty_fecundity[1],
                               uncertainty_survival = uncertainty_survival[1])) %>%
                   resample_summary_function(mean_matrix = mean_matrix,
                                             uncertainty_level = "min",
                                             propagation_type = "full",
                                             matrix_name = matrix_name,
                                             elasticity = elasticity)
  
  # mean uncertainty
  full_prop_mean <- rerun(1000, parametric_bootstrap_function(mean_matrix = mean_matrix,
                               uncertainty_fecundity = uncertainty_fecundity[2],
                               uncertainty_survival = uncertainty_survival[2]))%>%
    resample_summary_function(mean_matrix = mean_matrix,
                              uncertainty_level = "mean",
                              propagation_type = "full",
                              matrix_name = matrix_name,
                              elasticity = elasticity)
  
  # maximum uncertainty
  full_prop_max <- rerun(1000, parametric_bootstrap_function(mean_matrix = mean_matrix,
                               uncertainty_fecundity = uncertainty_fecundity[3],
                               uncertainty_survival = uncertainty_survival[3])) %>%
    resample_summary_function(mean_matrix = mean_matrix,
                              uncertainty_level = "max",
                              propagation_type = "full",
                              matrix_name = matrix_name,
                              elasticity = elasticity)
  
  #### SCENARIO 2: Fecundity propagation ####
  
  # minimum uncertainty
  f_prop_min <- rerun(1000, parametric_bootstrap_function(mean_matrix = mean_matrix,
                            uncertainty_fecundity = uncertainty_fecundity[1],
                            uncertainty_survival = NULL)) %>%
    resample_summary_function(mean_matrix = mean_matrix,
                              uncertainty_level = "min",
                              propagation_type = "f_only",
                              matrix_name = matrix_name,
                              elasticity = elasticity)
  
  # mean uncertainty
  f_prop_mean <- rerun(1000, parametric_bootstrap_function(mean_matrix = mean_matrix,
                            uncertainty_fecundity = uncertainty_fecundity[2],
                            uncertainty_survival = NULL)) %>%
    resample_summary_function(mean_matrix = mean_matrix,
                              uncertainty_level = "mean",
                              propagation_type = "f_only",
                              matrix_name = matrix_name,
                              elasticity = elasticity)
  
  # maximum uncertainty
  f_prop_max <- rerun(1000, parametric_bootstrap_function(mean_matrix = mean_matrix,
                            uncertainty_fecundity = uncertainty_fecundity[3],
                            uncertainty_survival = NULL)) %>%
    resample_summary_function(mean_matrix = mean_matrix,
                              uncertainty_level = "max",
                              propagation_type = "f_only",
                              matrix_name = matrix_name,
                              elasticity = elasticity)
  
  
  #### SCENARIO 3: Survival propagation ####
  
  # minimum uncertainty
  s_prop_min <- rerun(1000, parametric_bootstrap_function(mean_matrix = mean_matrix,
                            uncertainty_fecundity = NULL,
                            uncertainty_survival = uncertainty_survival[1])) %>%
    resample_summary_function(mean_matrix = mean_matrix,
                              uncertainty_level = "min",
                              propagation_type = "s_only",
                              matrix_name = matrix_name,
                              elasticity = elasticity)
  
  # mean uncertainty
  s_prop_mean <- rerun(1000, parametric_bootstrap_function(mean_matrix = mean_matrix,
                            uncertainty_fecundity = NULL,
                            uncertainty_survival = uncertainty_survival[2])) %>%
    resample_summary_function(mean_matrix = mean_matrix,
                              uncertainty_level = "mean",
                              propagation_type = "s_only",
                              matrix_name = matrix_name,
                              elasticity = elasticity)
  
  # maximum uncertainty
  s_prop_max <- rerun(1000, parametric_bootstrap_function(mean_matrix = mean_matrix,
                            uncertainty_fecundity = NULL,
                            uncertainty_survival = uncertainty_survival[3])) %>%
    resample_summary_function(mean_matrix = mean_matrix,
                              uncertainty_level = "max",
                              propagation_type = "s_only",
                              matrix_name = matrix_name,
                              elasticity = elasticity)
  
  #### SUMMARISE RESULTS ####
  
  # then combine summaries
  
  complete_results <- bind_rows(results, full_prop_min, full_prop_mean, 
                                full_prop_max, f_prop_min, f_prop_mean,
                                f_prop_max, s_prop_min, s_prop_mean,
                                s_prop_max)
  
  return(complete_results)
  
}


################################################################################



