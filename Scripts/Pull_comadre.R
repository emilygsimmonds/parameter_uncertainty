# SCRIPT TO PULL MATRICES FROM COMADRE #

#### Set up ####

# load packages

library(tidyverse)

#install.packages("Rcompadre")

library(Rcompadre)
library(dplyr)
library(popdemo)
library(popbio)
library(maps)     # for plotting world map

#fetch the comadre database

comadre <- cdb_fetch('comadre')

#### Subset to useful matrices ####

# criteria: published since 2010 

comadre_reduced <- comadre %>% filter(as.numeric(YearPublication) > 2010,
                              !is.na(DOI_ISBN),
                              DOI_ISBN != "NA")

# remove any matrices that are flagged with issues

comadre_reduced <- comadre_reduced %>% cdb_flag() %>%
  filter(check_NA_A == FALSE)

# look at 2010 itself too

comadre_2010 <- comadre %>% filter(as.numeric(YearPublication) == 2010,
                                      !is.na(DOI_ISBN),
                                      DOI_ISBN != "NA")

# remove any matrices that are flagged with issues

comadre_2010 <- comadre_2010 %>% cdb_flag() %>%
  filter(check_NA_A == FALSE)

#### Save working data set of COMADRE ####

comadre_combined <- comadre %>% filter(as.numeric(YearPublication) > 2009,
                                      !is.na(DOI_ISBN),
                                      DOI_ISBN != "NA") %>% 
                            cdb_flag() %>%
                            filter(check_NA_A == FALSE)

save(comadre_combined, file = "./Data files/working_comadre.RData")

#### Get DOIs ####

# number of DOIs - 104 papers to look at

DOI_summary <- comadre_reduced %>% 
  filter(as.numeric(StudyDuration) > 0) %>%
  group_by(DOI_ISBN) %>% 
  summarize(n_populations = length(unique(MatrixPopulation))) %>% 
  arrange(desc(n_populations))

write.csv(DOI_summary, "./Data files/DOIs.csv")

DOI_summary_2010 <- comadre_2010 %>% 
  filter(as.numeric(StudyDuration) > 0) %>%
  group_by(DOI_ISBN) %>% 
  summarize(n_populations = length(unique(MatrixPopulation))) %>% 
  arrange(desc(n_populations)) # 18 additional papers from 2010

write.csv(DOI_summary_2010, "./Data files/DOIs_2010.csv")

#### CHOOSE DATA SIMULATION MATRICES ####

load("./Data files/working_comadre.RData")

## subset to right size: size 2 x 2 for data uncertainty simulations

# which matrices have size = 2?
comadre_2 <- comadre_combined %>% filter(MatrixDimension == 2)

# extract info from those matrices
matrices_data <- map_df(.x = as.list(1:nrow(comadre_2@data)), ~{
  
  matrixA <- matA(comadre_2[.x]$mat)[[1]]
  stages <- matrixClass(comadre_2[.x]$mat)[[1]]$MatrixClassAuthor
  
  output <- data.frame(vital_rate = c("fecundity_j",
                            "fecundity_a", 
                            "survival_j",
                            "survival_a"),
             author_stage = c(stages[1],
                       stages[2],
                       stages[1],
                       stages[2]),
             value = c(matrixA[1,1],
                       matrixA[1,2],
                       matrixA[2,1],
                       matrixA[2,2]),
             matrix_number = .x)
  
  # remove any with 0 for either adult or juvenile fecundity
  if(matrixA[1,1] == 0 | matrixA[1,2] == 0){output <- data.frame(vital_rate = NA,
                                                  author_stage = NA,
                                                  value = NA)}
  
  return(output)
  
}) %>% drop_na()

# summarise
summary_matrices <- matrices_data %>% 
  group_by(vital_rate) %>%
  summarise(mean = mean(value),
            max = max(value),
            min = min(value))

# find matrix that had highest adult fecundity
which(matrices_data$value > 5)

max_data_no0 <- matrices_data[113:116,] # highest fecundity matrix

which(matrices_data$value < 0.03)

min_data_no0 <- matrices_data[161:164,] # lowest juvenile fecundity

## when one fecundity is 0
matrices_all <- map_df(.x = as.list(1:nrow(comadre_2@data)), ~{
  
  matrixA <- matA(comadre_2[.x]$mat)[[1]]
  stages <- matrixClass(comadre_2[.x]$mat)[[1]]$MatrixClassAuthor
  
  output <- data.frame(vital_rate = c("fecundity_j",
                                      "fecundity_a", 
                                      "survival_j",
                                      "survival_a"),
                       author_stage = c(stages[1],
                                        stages[2],
                                        stages[1],
                                        stages[2]),
                       value = c(matrixA[1,1],
                                 matrixA[1,2],
                                 matrixA[2,1],
                                 matrixA[2,2]),
                       matrix_number = .x)
  
  return(output)
  
})

# summarise
summary_matrices <- matrices_all %>% 
  group_by(vital_rate) %>%
  summarise(mean = mean(value),
            max = max(value),
            min = min(value))

which(matrices_all$value > 90)

max_data_0 <- matrices_all[349:352,] # highest fecundity

#### check matrices ####

eigen(matrix(min_data_no0$value, nrow = 2, byrow = TRUE)) # 0.75

eigen(matrix(max_data_no0$value, nrow = 2, byrow = TRUE)) # 6.19

eigen(matrix(max_data_0$value, nrow = 2, byrow = TRUE)) # 3.92

eigen(matrix(c(0.6, 0.7, 0.4, 0.6), nrow = 2, byrow= TRUE)) # 1.13 (mean)


#### CHOOSE PARAMETER SIMULATION MATRICES ####

## identify modal size for parameter uncertainty
# want to summarise by paper/population and dimension so stop skewing by
# a few studies

size_by_paper <- comadre_combined %>% 
  group_by(DOI_ISBN, MatrixDimension) %>% summarise(count = n())

counts <- size_by_paper %>% group_by(MatrixDimension) %>% summarise(count = n())

# 3x3 is most common after correcting for multiple per paper

# which matrices have size = 3?
comadre_3 <- comadre_combined %>% filter(MatrixDimension == 3)

# then extract the full matrix
matrices <- matA(comadre_3)

# summarise with row sums for elasticity
# want to get matrices with different balance of importance of vital rates
summaries <- map_df(.x = matrices, ~{
  
  # calculate elasticities for each matrix
  elasticities <- elasticity(.x)
  
  # want the row sums
  row_sums <- rowSums(elasticities)
  
  # calculate ratio of fecundity to survival
  ratio <- (row_sums[1]/length(which(elasticities[1,] > 0)))/
    (sum(row_sums[2:3])/length(which(elasticities[2:3,] > 0)))
  
})

# some do not run as have all 0 fecundity etc - exclude these but keep numbering
# some go to infinity too - also remove

summaries2 <- summaries %>% mutate(number = rownames(summaries)) %>% drop_na

# find the 1st and 3rd quantiles fecundity and survival

summary(summaries2$A1) # ratios from 0 to infinity

# take matrices that = 1, <0.0.2537 and >1

# take median of those above 1.1
summary(summaries2[which(summaries2$A1 > 1.1 & summaries2$A1 != "Inf"),1]) # 1.431

summaries2[which(summaries2$A1 > 1.43 & summaries2$A1 < 1.432),]

# 211
matrices[211]

# take median of those below 0.2537
summary(summaries2[which(summaries2$A1 < 0.2537),1]) # 0.16169

summaries2[which(summaries2$A1 > 0.155 & summaries2$A1 < 0.162),]

# 167
matrices[167]

# find those that = 1
summaries2[which(summaries2$A1 < 1.01 & summaries2$A1 > 0.999),]

# 103
matrices[103]

#### SAVE OUT META DATA FOR CHOSEN MATRICES ####

## data uncertainty matrices

# object = comadre_2, numbers = max no 0: 121, min no 0: 153, max 0: 88

c(comadre_2[121]$SpeciesAccepted, comadre_2[121]$DOI_ISBN, 
  comadre_2[121]$CensusType, comadre_2[121]$mat)

c(comadre_2[153]$SpeciesAccepted, comadre_2[153]$DOI_ISBN, 
  comadre_2[153]$CensusType, comadre_2[153]$mat)

c(comadre_2[88]$SpeciesAccepted, comadre_2[88]$DOI_ISBN, 
  comadre_2[88]$CensusType, comadre_2[88]$mat)

## parameter uncertainty matrices

# object = comadre_3, numbers = mat1: 211, mat2: 167, mat 3: 103

c(comadre_3[211]$SpeciesAccepted, comadre_3[211]$DOI_ISBN, 
  comadre_3[211]$CensusType, comadre_3[211]$mat)

c(comadre_3[167]$SpeciesAccepted, comadre_3[167]$DOI_ISBN, 
  comadre_3[167]$CensusType, comadre_3[167]$mat)

c(comadre_3[103]$SpeciesAccepted, comadre_3[103]$DOI_ISBN, 
  comadre_3[103]$CensusType, comadre_3[103]$mat)


