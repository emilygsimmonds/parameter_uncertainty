#### Checks ####

#### Source scripts ####

source("./Functions/parametric_bootstrap_function.R")
source("./Functions/run_multiple_resampling.R")

#### Set up example matrices of each size ####

## 2x2

mean_matrix_22 <- matrix(c(1,1,
                           0.5, 0.5), byrow = TRUE, nrow = 2)

## 3x3

mean_matrix_33 <- matrix(c(1,1,1,
                        0.5, 0, 0,
                        0, 0.5, 0.5), byrow = TRUE, nrow = 3)

## 5x5

mean_matrix_55 <- matrix(c(0,1,1,1,1,
                           0.5, 0, 0, 0, 0,
                           0, 0.5, 0, 0, 0,
                           0, 0, 0.5, 0, 0,
                           0, 0, 0, 0.5, 0.5), byrow = TRUE, nrow = 5)

#### if run with uncertainty = 0 get mean matrix back ####

popdemo::eigs(mean_matrix_22, what = "lambda")

summary(parametric_bootstrap_function(mean_matrix = mean_matrix_22,
                        uncertainty_fecundity = 0,
                        uncertainty_survival = 0,
                        matrix_number = "matrix1",
                        breeding_stages = "one",
                        uncertainty_level = "none"))

popdemo::eigs(mean_matrix_33, what = "lambda")

summary(parametric_bootstrap_function(mean_matrix = mean_matrix_33,
                                      uncertainty_fecundity = 0,
                                      uncertainty_survival = 0,
                                      matrix_number = "matrix1",
                                      breeding_stages = "one",
                                      uncertainty_level = "none"))

popdemo::eigs(mean_matrix_55, what = "lambda")

summary(parametric_bootstrap_function(mean_matrix = mean_matrix_55,
                                      uncertainty_fecundity = 0,
                                      uncertainty_survival = 0,
                                      matrix_number = "matrix1",
                                      breeding_stages = "one",
                                      uncertainty_level = "none"))

#### if run with uncertainty = 1 get wide range ####

run_multiple_resampling(mean_matrix = mean_matrix_22,
                                uncertainty_fecundity = 1,
                                uncertainty_survival = 1,
                                matrix_number = "matrix1",
                                breeding_stages = "one",
                                uncertainty_level = "none") %>%
  summary()

run_multiple_resampling(mean_matrix = mean_matrix_33,
                                uncertainty_fecundity = 1,
                                uncertainty_survival = 1,
                                matrix_number = "matrix1",
                                breeding_stages = "one",
                                uncertainty_level = "none") %>%
  summary()

run_multiple_resampling(mean_matrix = mean_matrix_55,
                                uncertainty_fecundity = 1,
                                uncertainty_survival = 1,
                                matrix_number = "matrix1",
                                breeding_stages = "one",
                                uncertainty_level = "none") %>%
  summary()

#### check basal function ####

set.seed(1)
lnorms(1, mean = 1, var = (1*0.5)^2) # 0.665
set.seed(1)
betaval(0.5, 0.4*0.5) #0.4257379

# reference lambda 22
popdemo::eigs(matrix(c(0.665, 0.665,
                       0.4257379, 0.4257379), byrow = TRUE, nrow = 2), what = "lambda")

# reference lambda 33
eigen(matrix(c(0.665, 0.665, 0.665,
               0.4257379, 0, 0, 
               0, 0.4257379, 0.4257379), byrow = TRUE, nrow = 3))$value[1]

# reference lambda 55
eigen(matrix(c(0, 0.665, 0.665, 0.665, 0.665,
               0.4257379, 0, 0, 0, 0, 
               0, 0.4257379, 0, 0, 0,
               0, 0, 0.4257379, 0, 0,
               0, 0, 0, 0.4257379, 0.4257379), byrow = TRUE, nrow = 5))$value[1]

set.seed(1)
parametric_bootstrap_function(mean_matrix = mean_matrix_22, 
                              uncertainty_fecundity = 0.5,
                              uncertainty_survival = 0.4,
                              matrix_number = "matrix1",
                              breeding_stages = "one",
                              uncertainty_level = "none",
                              seed = 1) %>%
  summary()

parametric_bootstrap_function(mean_matrix = mean_matrix_33, 
                              uncertainty_fecundity = 0.5,
                              uncertainty_survival = 0.4,
                              matrix_number = "matrix1",
                              breeding_stages = "one",
                              uncertainty_level = "none",
                              seed = 1) %>%
  summary()

parametric_bootstrap_function(mean_matrix = mean_matrix_55, 
                              uncertainty_fecundity = 0.5,
                              uncertainty_survival = 0.4,
                              matrix_number = "matrix1",
                              breeding_stages = "one",
                              uncertainty_level = "none",
                              seed = 1) %>%
  summary()

