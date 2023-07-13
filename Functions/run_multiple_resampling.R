# T1.2: Run all bootstrap scenarios for a given matrix #

# takes mean matrix and defined uncertainty/variance and resamples

################################################################################

#### Set up ####

# input is matrix, fecundity uncertainty and survival uncertainty 
# (as proportions but representing standard error)

# output is joined dataframe of all resample results

# source scripts

source("./Functions/parametric_bootstrap_function.R")

################################################################################

run_multiple_resampling <- function(mean_matrix,
                                    uncertainty_fecundity,
                                    uncertainty_survival,
                                    matrix_number,
                                    breeding_stages = c("multiple", 
                                                        "one"),
                                    uncertainty_level = c("none", "low", 
                                                          "mid", 
                                                          "high")){
  
  output <- map_df(1:10000, ~parametric_bootstrap_function(mean_matrix = mean_matrix,
                                  uncertainty_fecundity = uncertainty_fecundity,
                                  uncertainty_survival = uncertainty_survival,
                                  matrix_number = matrix_number,
                                  breeding_stages = breeding_stages,
                                  uncertainty_level = uncertainty_level))
  return(output)
  
}
  
