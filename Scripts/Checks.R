#### Checks

# if run with uncertainty = 0 get mean matrix back

mean_matrix <- matrix(c(1,1,1,
                        0.5, 0, 0,
                        0, 0.5, 0.5), byrow = TRUE, nrow = 3)

summary(run_resampling_function(mean_matrix = mean_matrix,
                        uncertainty_fecundity = c(0,0,0),
                        uncertainty_survival = c(0,0,0),
                        matrix_name = "matrix1"))

# if run with uncertainty = 1 get wide range

summary(run_resampling_function(mean_matrix = mean_matrix,
                                uncertainty_fecundity = c(1,1,1),
                                uncertainty_survival = c(1,1,1),
                                matrix_name = "matrix1"))

# check basal function

set.seed(1)
lnorms(1, mean = 1, var = (1*0.5)^2) # 0.665
set.seed(1)
betaval(0.5, 0.5*0.5) #0.3988

eigen(matrix(c(0.665, 0.665, 0.665,
               0.3988, 0, 0, 
               0, 0.3988, 0.3988), byrow = TRUE, nrow = 3))$value[1]

set.seed(1)
parametric_bootstrap_function(mean_matrix, 
                              uncertainty_fecundity = 0.5,
                              uncertainty_survival = 0.5)


