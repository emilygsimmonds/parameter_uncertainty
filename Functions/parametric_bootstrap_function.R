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
                                          uncertainty_fecundity = NULL,
                                          uncertainty_survival = NULL){
# set up new matrix to store results
new_matrix <- mean_matrix
  
# first sample fecundity values but make sure not to if matrix element 0
# only want to run if uncertainty_fecundity is set
if(!is.null(uncertainty_fecundity)){
if(mean_matrix[1,1] != 0){new_matrix[1,1] <- lnorms(1, 
                                               mean_matrix[1,1], 
                                               (mean_matrix[1,1]*uncertainty_fecundity)*2)}
if(mean_matrix[1,2] != 0){new_matrix[1,2] <- lnorms(1, 
                                               mean_matrix[1,2], 
                                               (mean_matrix[1,2]*uncertainty_fecundity)*2)}
if(mean_matrix[1,3] != 0){new_matrix[1,3] <- lnorms(1, 
                                               mean_matrix[1,3], 
                                               (mean_matrix[1,3]*uncertainty_fecundity)*2)}}

# sample survival but make sure not to if matrix element 0 

if(!is.null(uncertainty_survival)){
if(mean_matrix[2,1] != 0){
  if(uncertainty_survival^2 >= (1-mean_matrix[2,1])*mean_matrix[2,1]) # need to check stdev not too high
  {uncertainty_survival2 <- sqrt(((1-mean_matrix[2,1])*mean_matrix[2,1]))-0.1}else{ # if it is, set to max possible
      uncertainty_survival2 <- uncertainty_survival}
  new_matrix[2,1] <- betaval(mean_matrix[2,1], 
  mean_matrix[2,1]*uncertainty_survival2)}
if(mean_matrix[2,2] != 0){
    if(uncertainty_survival^2 >= (1-mean_matrix[2,2])*mean_matrix[2,2])
    {uncertainty_survival2 <- sqrt(((1-mean_matrix[2,2])*mean_matrix[2,2]))-0.1}else{
      uncertainty_survival2 <- uncertainty_survival}
  new_matrix[2,2] <- betaval(mean_matrix[2,2], 
  mean_matrix[2,2]*uncertainty_survival2)}
if(mean_matrix[2,3] != 0){
    if(uncertainty_survival^2 >= (1-mean_matrix[2,3])*mean_matrix[2,3])
    {uncertainty_survival2 <- sqrt(((1-mean_matrix[2,3])*mean_matrix[2,3]))-0.1}else{
      uncertainty_survival2 <- uncertainty_survival}
    new_matrix[2,3] <- betaval(mean_matrix[2,3], 
    mean_matrix[2,3]*uncertainty_survival2)}
if(mean_matrix[3,1] != 0){
    if(uncertainty_survival^2 >= (1-mean_matrix[3,1])*mean_matrix[3,1])
    {uncertainty_survival2 <- sqrt(((1-mean_matrix[3,1])*mean_matrix[3,1]))-0.1}else{
      uncertainty_survival2 <- uncertainty_survival}
  new_matrix[3,1] <- betaval(mean_matrix[3,1], 
  mean_matrix[3,1]*uncertainty_survival2)}
if(mean_matrix[3,2] != 0){
    if(uncertainty_survival^2 >= (1-mean_matrix[3,2])*mean_matrix[3,2])
    {uncertainty_survival2 <- sqrt(((1-mean_matrix[3,2])*mean_matrix[3,2]))-0.1}else{
      uncertainty_survival2 <- uncertainty_survival}
  new_matrix[3,2] <- betaval(mean_matrix[3,2], 
  mean_matrix[3,2]*uncertainty_survival2)}
if(mean_matrix[3,3] != 0){
    if(uncertainty_survival^2 >= (1-mean_matrix[3,3])*mean_matrix[3,3])
    {uncertainty_survival2 <- sqrt(((1-mean_matrix[3,3])*mean_matrix[3,3]))-0.1}else{
     uncertainty_survival2 <- uncertainty_survival}
  new_matrix[3,3] <- betaval(mean_matrix[3,3], 
  mean_matrix[3,3]*uncertainty_survival2)}
  }
  
lambda <- popdemo::eigs(new_matrix, what = "lambda")

#### CODE FROM COMAPDRE TUTORIAL ###############################################

w <- eigen(new_matrix)$vectors
v <- Conj(solve(w))

senmat <- Re(v[1,] %*% t(w[,1])) # calculate sensitivity
emat <- (1/(Re(lambda))) * senmat * new_matrix

elasticity <- which(emat == max(emat))
  
return(list(lambda = lambda, 
            elasticity = elasticity))

}


