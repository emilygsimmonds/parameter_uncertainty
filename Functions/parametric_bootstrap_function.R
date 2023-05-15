# T1.2: Function to do parametric bootstrap of matrix elements #

# takes mean matrix and defined uncertainty/variance and resamples

# for each iteration calculates lambda and elasticity (outputs most important matrix element)

################################################################################

#### Set up ####

# load packages

library(tidyverse)
library(Rcompadre)
library(popbio)

# input is 3 x 3 matrix, fecundity uncertainty and survival uncertainty (as proportions but representing standard error)

# output is estimates of lambda and most important matrix elements (elasticity)

# use Beta for survival (betaval in popbio) and log norm for reproduction

################################################################################

parametric_bootstrap_function <- function(mean_matrix,
                                          uncertainty_fecundity,
                                          uncertainty_survival,
                                          matrix_number,
                                          breeding_stages = c("multiple", 
                                                              "one"),
                                          uncertainty_level = c("none", "low", 
                                                                "mid", 
                                                                "high"),
                                          seed = NULL){
# set up new matrix to store results
new_matrix <- mean_matrix

# first sample fecundity values but make sure not to if matrix element 0
# only want to run if uncertainty_fecundity is set
if(uncertainty_fecundity != "NULL"){
  
# identify which elements of matrix have non-zero entries for fecundity
marker <- which(mean_matrix[1,] != 0)
  
if(!is.null(seed)){set.seed(seed)}
new_matrix[1,marker] <- lnorms(1, 
                         mean_matrix[1,marker], 
                         (mean_matrix[1,marker]*as.numeric(uncertainty_fecundity))^2)}


# sample survival but make sure not to if matrix element 0 
if(uncertainty_survival != "NULL"){

# identify which survival elements are non-zero    
marker2 <- which(mean_matrix[-1,] != 0) 

uncertainty_survival <- as.numeric(uncertainty_survival)

# do this in a loop so we can check each entry
for(i in 1:length(marker2)){
if(uncertainty_survival^2 >= (1-mean_matrix[-1,][marker2[i]])*mean_matrix[-1,][marker2[i]]) # need to check stdev not too high
{uncertainty_survival2 <- sqrt(((1-mean_matrix[-1,][marker2[i]])*mean_matrix[-1,][marker2[i]]))-0.1}else{ # if it is, set to max possible
uncertainty_survival2 <- uncertainty_survival}

if(!is.null(seed)){set.seed(seed)}  
  
  new_matrix[2:length(mean_matrix[,1]),][marker2[i]] <- 
    betaval(mean_matrix[-1,][marker2[i]], 
            mean_matrix[-1,][marker2[i]]*uncertainty_survival2)}}
  
lambda <- popdemo::eigs(new_matrix, what = "lambda")

#### CODE FROM COMAPDRE TUTORIAL ###############################################

w <- eigen(new_matrix)$vectors
v <- Conj(solve(w))

senmat <- Re(v[1,] %*% t(w[,1])) # calculate sensitivity
emat <- (1/(Re(lambda))) * senmat * new_matrix

elasticity <- which(emat == max(emat))

if(uncertainty_survival == "NULL" & uncertainty_fecundity == "NULL"){
  prop_scenario <- "none"}
if(uncertainty_survival != "NULL" & uncertainty_fecundity != "NULL"){
  prop_scenario <- "full"}
if(uncertainty_survival == "NULL" & uncertainty_fecundity != "NULL"){
  prop_scenario <- "f_only"}
if(uncertainty_survival != "NULL" & uncertainty_fecundity == "NULL"){
  prop_scenario <- "s_only"}

output <- data.frame(matrix_number = matrix_number,
                     breeding_stages = breeding_stages,
                     uncertainty_level = uncertainty_level,
                     lambda = lambda,
                     elasticity = elasticity,
                     prop_scenario = prop_scenario)
  
return(output)

}


