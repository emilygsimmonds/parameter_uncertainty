# T1.2: Script to format data from review ready for analysis #

################################################################################

#### Set up ####

# load packages

library(tidyverse)

# import data

# tidy up by removing blank lines

general_questions <- read.csv('./Data files/general_questions.csv', 
                              header = TRUE, 
                              na.strings = c("", "NA")) # general questions

general_questions <- general_questions[rowSums(is.na(general_questions))
                                       != ncol(general_questions),] # remove all NA rows

# also remove those that were not peer reviewed
general_questions_cleaned <- filter(general_questions, 
                            DOI != "not peer reviewed")

# re-save 

write.csv(general_questions_cleaned, "./Data files/general_questions_cleaned.csv",
          row.names = FALSE)

