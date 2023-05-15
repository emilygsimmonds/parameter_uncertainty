# T1.2: Function to summarise resampled results #

# Takes output of resampling, matrix name, mean matrix, 
# uncertainty level (min, mean, max), 
# and type of propagation (full, f_only, s_only), 
# and "most important matrix element"

# Produces a combined dataframe with colnames: matrix_name, lambda, elasticity,
# propagation_type, uncertainty_level

################################################################################


resample_summary_function <- function(output, 
                                      mean_matrix, 
                                      uncertainty_level,
                                      propagation_type,
                                      matrix_name,
                                      elasticity){
  
  # set up dataframe to store results in
  results <- data.frame(matrix_name = rep(matrix_name, 10000),
                        lambda = NA,
                        elasticity = NA,
                        propagation_type = rep(propagation_type, 10000),
                        uncertainty_level = rep(uncertainty_level, 10000),
                        in_CI = NA)

  results[1:1000,"lambda"] <- unlist(lapply(output, "[[", 1)) # extract lambdas
  
  # mark if these results would be in a CI
  for(i in 1:1000){
  if(results[i,"lambda"] > sort(results[1:10000, "lambda"])[(0.025*10000)] &
     results[i,"lambda"] < sort(results[1:10000, "lambda"])[(0.975*10000)]){
  results[i, "in_CI"] <- TRUE}else{results[i, "in_CI"] <- FALSE}}

  # check if elasticity matches that identified in the mean matrix
  results[1:10000,"elasticity"] <- unlist(map(.x = lapply(output, "[[", 2), 
                     ~{x <- length(which(.x == elasticity)) > 0}))

  return(results)

}
