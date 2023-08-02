# T1.2: Function to split A matrix into U and F #

# takes A matrix and splits into a list of U and F matrices

################################################################################

# split into U and F matrices

split_matrix <- function(matA){
  
  mat_F <- mat_U <- matrix(data = 0, nrow = nrow(matA),
                           ncol = ncol(matA))
  mat_F[1,] <- matA[1,]
  mat_U[2:nrow(matA),] <- matA[2:nrow(matA),]
  output <- list(mat_F, mat_U)
  names(output) <- c("mat_F", "mat_U")
  return(output)
}