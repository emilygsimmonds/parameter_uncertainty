# T1.2: Script to pull matrices and DOI from COMADRE #

################################################################################

#### Set up ####

# load packages

library(tidyverse)

#install.packages("Rcompadre")

library(Rcompadre)
library(dplyr)
library(popdemo)
library(popbio)
library(Rage)

# fetch the comadre database

comadre <- cdb_fetch("./Data files/COMADRE_v.4.21.8.0.RData")

# load functions

source("./Functions/split_matrix_function.R")

################################################################################

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

length(unique(comadre_combined$DOI_ISBN))

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

#### File of paper information for reviewed papers ####

# combine two DOI data frames
DOIs_all <- bind_rows(DOI_summary, DOI_summary_2010)[,1] 
# remove n_populations column

comadre_df <- comadre_combined@data[,-1] %>% 
  filter(DOI_ISBN %in% DOIs_all$DOI_ISBN)
  
comadre_df <- comadre_df[,c(17,18,21,22)] %>%
  distinct() %>%
  arrange(DOI_ISBN)

# remove rows 12 and 30 as wrong info and duplicate DOI

comadre_df <- comadre_df[-c(12,30),]

# save out

write.csv(comadre_df,"./Figures/review_papers.csv")

################################################################################
  
#### CHOOSE PARAMETER SIMULATION MATRICES ####

load("./Data files/working_comadre.RData")

#### 2x2 matrices ####

# which matrices have size = 2?
comadre_22 <- comadre_combined %>% filter(MatrixDimension == 2)

# then extract the full matrix
matrices_22 <- matA(comadre_22)

# summarise with row sums for elasticity
# want to get matrices with different balance of importance of vital rates
summaries_22 <- map2_df(.x = matrices_22,
                        .y = as.list(seq(1, length(matrices_22), 1)), ~{
                          
                          # calculate elasticities for each matrix
                          elasticities <- elasticity(.x)
                          
                          # want the row sums
                          row_sums <- rowSums(elasticities, na.rm = TRUE)
                          
                          # calculate ratio of fecundity to survival
                          ratio <- (row_sums[1]/length(which(elasticities[1,] > 0)))/
                            (sum(row_sums[2])/length(which(elasticities[2,] > 0)))
                          
                          # calculate if species breeds at all ages or just one
                          all <- length(which(.x[1,] > 0)) == 2
                          one <- length(which(.x[1,] > 0)) == 1
                          
                          return(data.frame(ratio = ratio,
                                            all = all,
                                            one = one,
                                            index = .y))
                          
                        })

# some do not run as have all 0 fecundity etc - exclude these but keep numbering
# some go to infinity too - also remove

summaries_222 <- summaries_22 %>% drop_na

# split into those with all = TRUE and those with one = TRUE

breed_once_22 <- summaries_222 %>% filter(one == TRUE) %>% arrange(ratio)
breed_all_22 <- summaries_222 %>% filter(all == TRUE) %>% arrange(ratio)

# want 5 matrices from each. 12.5th, 25th, 50th, 75th and 87.5th percentiles approx

markers <- c(0.125, 0.25, 0.5, 0.75, 0.875)

breed_once_matrices_22 <- breed_once_22[round(length(breed_once_22[,1])*markers),]$index

breed_all_matrices_22 <- breed_all_22[round(length(breed_all_22[,1])*markers),]$index

to_save <- matrices_22[breed_all_matrices_22]

save(to_save, 
     file = "./Data files/twobytwo_breed_all.RData")

to_save <- matrices_22[breed_once_matrices_22]

save(to_save, 
     file = "./Data files/twobytwo_breed_once.RData")

#### create table of final matrices and their ratio

matrix_summary_2x2 <- bind_rows(data.frame(matrix_number = 1:5, 
           reproductionsurvival_ratio = 
             breed_all_22[round(length(breed_all_22[,1])*markers),1],
           reproduction_strategy = "multiple",
           matrix_size = "2x2"),
           data.frame(matrix_number = 1:5, 
                      reproductionsurvival_ratio = 
                        breed_once_22[round(length(breed_once_22[,1])*markers),1],
                      reproduction_strategy = "once",
                      matrix_size = "2x2") )

#### 3x3 matrices ####

# which matrices have size = 3?
comadre_33 <- comadre_combined %>% filter(MatrixDimension == 3)

# then extract the full matrix
matrices_33 <- matA(comadre_33)

# summarise with row sums for elasticity
# want to get matrices with different balance of importance of vital rates
summaries_33 <- map2_df(.x = matrices_33,
                     .y = as.list(seq(1, length(matrices_33), 1)), ~{
  
  # calculate elasticities for each matrix
  elasticities <- elasticity(.x)
  
  # want the row sums
  row_sums <- rowSums(elasticities, na.rm = TRUE)
  
  # calculate ratio of fecundity to survival
  ratio <- (row_sums[1]/length(which(elasticities[1,] > 0)))/
    (sum(row_sums[2:3])/length(which(elasticities[2:3,] > 0)))
  
  # calculate if species breeds at all ages or just one
  all <- length(which(.x[1,] > 0)) == 3
  one <- length(which(.x[1,] > 0)) == 1
  
  return(data.frame(ratio = ratio,
                    all = all,
                    one = one,
                    index = .y))
  
})

# some do not run as have all 0 fecundity etc - exclude these but keep numbering
# some go to infinity too - also remove

summaries_332 <- summaries_33 %>% drop_na

# split into those with all = TRUE and those with one = TRUE

breed_once_33 <- summaries_332 %>% filter(one == TRUE) %>% arrange(ratio)
breed_all_33 <- summaries_332 %>% filter(all == TRUE) %>% arrange(ratio)

# want 5 matrices from each. 12.5th, 25th, 50th, 75th and 87.5th percentiles approx

markers <- c(0.125, 0.25, 0.5, 0.75, 0.875)

breed_once_matrices_33 <- breed_once_33[round(length(breed_once_33[,1])*markers),]$index

breed_all_matrices_33 <- breed_all_33[round(length(breed_all_33[,1])*markers),]$index

to_save <- matrices_33[breed_all_matrices_33]

save(to_save, 
     file = "./Data files/threebythree_breed_all.RData")

to_save <- matrices_33[breed_once_matrices_33]
  
save(to_save, 
     file = "./Data files/threebythree_breed_once.RData")

#### create table of final matrices and their ratio

matrix_summary_3x3 <- bind_rows(data.frame(matrix_number = 1:5, 
                                 reproductionsurvival_ratio = 
                                   breed_all_33[round(length(breed_all_33[,1])*markers),1],
                                 reproduction_strategy = "multiple",
                                 matrix_size = "3x3"),
                      data.frame(matrix_number = 1:5, 
                                 reproductionsurvival_ratio = 
                                   breed_once_33[round(length(breed_once_33[,1])*markers),1],
                                 reproduction_strategy = "once",
                                 matrix_size = "3x3") )


#### 5x5 ####

# which matrices have size = 5?
comadre_55 <- comadre_combined %>% filter(MatrixDimension == 5)

# then extract the full matrix
matrices_55 <- matA(comadre_55)

# summarise with row sums for elasticity
# want to get matrices with different balance of importance of vital rates
summaries_55 <- map2_df(.x = matrices_55,
                        .y = as.list(seq(1, length(matrices_55), 1)), ~{
                          
                          # calculate elasticities for each matrix
                          elasticities <- elasticity(.x)
                          
                          # want the row sums
                          row_sums <- rowSums(elasticities, na.rm = TRUE)
                          
                          # calculate ratio of fecundity to survival
                          ratio <- (row_sums[1]/length(which(elasticities[1,] > 0)))/
                            (sum(row_sums[2:5])/length(which(elasticities[2:5,] > 0)))
                          
                          # calculate if species breeds at all ages or just one
                          all <- length(which(.x[1,] > 0)) >= 3
                          one <- length(which(.x[1,] > 0)) == 1
                          
                          return(data.frame(ratio = ratio,
                                            all = all,
                                            one = one,
                                            index = .y))
                          
                        })

# some do not run as have all 0 fecundity etc - exclude these but keep numbering
# some go to infinity too - also remove

summaries_552 <- summaries_55 %>% drop_na

# split into those with all = TRUE and those with one = TRUE

breed_once_55 <- summaries_552 %>% filter(one == TRUE) %>% arrange(ratio)
breed_all_55 <- summaries_552 %>% filter(all == TRUE) %>% arrange(ratio)

# want 5 matrices from each. 12.5th, 25th, 50th, 75th and 87.5th percentiles approx

markers <- c(0.125, 0.25, 0.5, 0.75, 0.875)

breed_once_matrices_55 <- breed_once_55[c(1,round(length(breed_once_55[,1])*markers[2:5])),]$index

breed_all_matrices_55 <- breed_all_55[round(length(breed_all_55[,1])*markers),]$index

to_save <- matrices_55[breed_all_matrices_55]

save(to_save, 
     file = "./Data files/fivebyfive_breed_all.RData")

to_save <- matrices_55[breed_once_matrices_55] # use matrix 13 not 69 as 69 seems to have error as has survival of 1

save(to_save, 
     file = "./Data files/fivebyfive_breed_once.RData")

#### create table of final matrices and their ratio

matrix_summary_5x5 <- bind_rows(data.frame(matrix_number = 1:5, 
                                reproductionsurvival_ratio = 
                                  breed_all_55[round(length(breed_all_55[,1])*markers),1],
                                reproduction_strategy = "multiple",
                                matrix_size = "5x5"),
                     data.frame(matrix_number = 1:5, 
                                reproductionsurvival_ratio = 
                                  breed_once_55[c(1,
                                                round(length(breed_once_55[,1])*markers[2:5]))
                                                ,1],
                                reproduction_strategy = "once",
                                matrix_size = "5x5"))

#### combine all summary tables and save out

matrix_summary_all <- bind_rows(matrix_summary_2x2,
                                matrix_summary_3x3,
                                matrix_summary_5x5)

write.csv(matrix_summary_all, "./Figures/matrix_ratios.csv")

################################################################################

#### Calculate generation times ####

# load matrices

load("./Data files/twobytwo_breed_all.RData")
two_by_two <- to_save
load("./Data files/twobytwo_breed_once.RData")
two_by_two_all <- c(two_by_two, to_save)


load("./Data files/threebythree_breed_all.RData")
three_by_three <- to_save
load("./Data files/threebythree_breed_once.RData")
three_by_three_all <- c(three_by_three, to_save)


load("./Data files/fivebyfive_breed_all.RData")
five_by_five <- to_save
load("./Data files/fivebyfive_breed_once.RData")
five_by_five_all <- c(five_by_five, to_save)

two_by_two_split <- map(.x = two_by_two_all,
                          .f = split_matrix)

three_by_three_split <- map(.x = three_by_three_all,
                          .f = split_matrix)

five_by_five_split <- map(.x = five_by_five_all,
                          .f = split_matrix)

# calculate generation time

unlist(map(.x = two_by_two_split,
    ~{gen_time(matR = .x[["mat_F"]],
               matU = .x[["mat_U"]], "age_diff")}))

map(.x = three_by_three_split,
    ~{gen_time(matR = .x[["mat_F"]],
               matU = .x[["mat_U"]], "age_diff")})

map(.x = five_by_five_split,
    ~{gen_time(matR = .x[["mat_F"]],
               matU = .x[["mat_U"]], "age_diff")})

# then combine with matrix ratios and re-export
