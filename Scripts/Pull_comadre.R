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

#### Subset matrices to correct size ####

load("./Data files/working_comadre.RData")

## size 2 x 2 for data uncertainty simulations

# which matrices have size = 2?
marker <- as.list(which(comadre_combined$MatrixDimension == 2))

# reduce to these matrices

matrices <- map_df(.x = marker, ~{
  matrixA <- matA(comadre_combined[.x]$mat)[[1]]
  stages <- matrixClass(comadre_combined[.x]$mat)[[1]]$MatrixClassAuthor
  
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
                       matrixA[2,2]))
  
  if(matrixA[1,1] == 0 | matrixA[1,2] == 0){output <- data.frame(vital_rate = NA,
                                                  author_stage = NA,
                                                  value = NA)}
  
  return(output)
  
}) %>% drop_na()

# summarise

summary_matrices <- matrices %>% 
  group_by(vital_rate) %>%
  summarise(mean = mean(value),
            max = max(value),
            min = min(value))

# find matrix that had highest adult fecundity

which(matrices$value > 5)

max_no0 <- matrices[113:116,] # highest fecundity matrix

which(matrices$value < 0.04)

min_no0 <- matrices[161:164,] # lowest juvenile fecundity

## when one fecundity is 0

matrices_all <- map_df(.x = marker, ~{
  matrixA <- matA(comadre_combined[.x]$mat)[[1]]
  stages <- matrixClass(comadre_combined[.x]$mat)[[1]]$MatrixClassAuthor
  
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
                                 matrixA[2,2]))
  
  return(output)
  
})

# summarise

summary_matrices <- matrices_all %>% 
  group_by(vital_rate) %>%
  summarise(mean = mean(value),
            max = max(value),
            min = min(value))

which(matrices_all$value > 90)

max_0 <- matrices_all[349:352,]

#### check matrices ####

eigen(matrix(min_no0$value, nrow = 2, byrow = TRUE)) # 0.75

eigen(matrix(max_no0$value, nrow = 2, byrow = TRUE)) # 6.19

eigen(matrix(max_0$value, nrow = 2, byrow = TRUE)) # 3.92

eigen(matrix(c(0.6, 0.7, 0.4, 0.6), nrow = 2, byrow= TRUE)) # 1.13 (mean)

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

# summarise with row means
summaries <- map_df(.x = matrices, ~{
  
  # calculate elasticities for each matrix
  elasticities <- elasticity(.x)
  
  # want the row sums
  row_sums <- rowSums(elasticities)
  
  # calculate ratio of fecundity to survival
  ratio <- row_sums[1]/mean(c(row_sums[2], row_sums[3]))
  
})

# some do not run as have all 0 fecundity etc - exclude these but keep numbering

summaries2 <- summaries %>% mutate(number = rownames(summaries)) %>% drop_na

# find the 1st and 3rd quantiles fecundity and survival

summary(summaries2$A1) # ratios from 0 to infinity

# take matrices that = 1, <0.1782 and >1

# take median of those above 1.1
summary(summaries2[which(summaries2$A1 > 1.1 & summaries2$A1 != "Inf"),1]) # 1.738

summaries2[which(summaries2$A1 > 1.735 & summaries2$A1 < 1.74),]

# pick at random
set.seed(1)
sample(44:45, 1) # 44

# take median of those below 0.1782
summary(summaries2[which(summaries2$A1 < 0.1782),1]) # 0.113765

summaries2[which(summaries2$A1 > 0.113 & summaries2$A1 < 0.114),]

# pick at random
set.seed(1)
sample(c(112,130,132), 1) # 112

matrices[112]

# find those that = 1
summaries2[which(summaries2$A1 < 1.01 & summaries2$A1 > 0.999),]

# pick at random
set.seed(1)
sample(c(36,103), 1) # 103

matrices[112]
