# SCRIPT TO FORMAT DATA FROM REVIEW #

#### Set up ####

## load packages ####

library(tidyverse)

## import data ####

# tidy up by removing blank lines

general_questions <- read.csv('./Data files/general_questions.csv', 
                              header = TRUE, 
                              na.strings = c("", "NA")) # general questions

general_questions <- general_questions[rowSums(is.na(general_questions))
                                       != ncol(general_questions),] # remove all NA rows

# re-save 

write.csv(general_questions, "./Data files/general_questions_cleaned.csv",
          row.names = FALSE)


#### extra bit to pull out papers for students ####

# pull out papers that have no uncertainty
DOIs <- filter(general_questions, Number == 5, 
       Answer1 == "no")$DOI # 18 papers

general_questions_reduced <- filter(general_questions,
                                    DOI %in% DOIs)

# check comments regarding sample size  
general_questions_reduced$Comments

# re save

write.csv(general_questions_reduced, "./Data files/general_questions_reduced.csv",
          row.names = FALSE)
