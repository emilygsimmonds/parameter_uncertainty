# SCRIPT TO MAKE SUMMARY PLOTS AND NUMBERS #

#### Set up ####

## load packages ####

library(tidyverse)

## Source required scripts ####

source('./Scripts/theme_script.R')

## Load data ####

general_questions <- read.csv('./Data files/general_questions_cleaned.csv',
                              header = TRUE)
vital_rates <- read.csv('./Data files/vital_rates.csv', header = TRUE)

# remove any experimental data

#marker <- general_questions$DOI[which(general_questions$Answer2 == "experiment")]

#general_questions <- filter(general_questions, 
#                            DOI != marker[1] &
#                              DOI != marker[2] &
#                              DOI != marker[3])

#### Summaries ####

length(unique(general_questions$DOI)) # 86 

number_papers <- general_questions %>%
  filter(Number == 1,
         Answer1 == "yes") 

### HOW MANY REPORT ANY UNCERTAINTY? ####

# filter the data, group, and summarise as %
# QU # = 5

general_questions %>%
  filter(Number == 5,
         Answer1 == "yes" | Answer1 == "no") %>%
  group_by(Answer1) %>%
  dplyr::summarize(count = n()) %>%
  dplyr::mutate(Perc = count/sum(count)) %>%
  ungroup() %>%
  mutate(position = (Perc/2))

# 18.7 no, 81.3 yes


### WAS UNCERTAINTY COMPLETE? ####

# filter the data, group, and summarise as %
# QU # = 4

general_questions %>%
  filter(Number == 6,
         Answer1 == "yes" | Answer1 == "no") %>%
  group_by(Answer1) %>%
  dplyr::summarize(count = n()) %>%
  dplyr::mutate(Perc = count/sum(count)) %>%
  ungroup()   %>%
  mutate(position = (Perc/2))

# 46.8 no, 53.2 yes

### WHICH WERE MISSED? ####

general_questions %>%
  filter(Number == 6,
         Answer1 == "no") %>%
  mutate(Missing_F = case_when(str_detect(Answer2, "reproduction") ~ TRUE,
                               TRUE ~ FALSE),
         Missing_S = case_when(str_detect(Answer2, "survival") ~ TRUE,
                               TRUE ~ FALSE)) %>%
  group_by(Missing_F, Missing_S) %>%
  dplyr::summarize(count = n())

# most missing both (13/29)
# then missing S and F almost equal 7 vs 8 

general_questions %>%
  filter(Number == 3,
         Answer1 == "yes" |
           Answer1 == "no") %>%
  group_by(Answer1) %>%
  dplyr::summarize(count = n()) %>%
  mutate(percentage = count/sum(count))

# % with survival observation process 47.8%

general_questions %>%
  filter(Number == 4,
         Answer1 == "yes" |
           Answer1 == "no") %>%
  group_by(Answer1) %>%
  dplyr::summarize(count = n()) %>%
  mutate(percentage = count/sum(count))

# % with fecundity observation process 11.4%

### WAS IT PROPAGATED ####

# use vital rate data
# filter the data, group, and summarise as %

# by vital rate
vital_rates %>%
  filter(!is.na(Uncertainty1)) %>%
  mutate(Propagated.to.final.matrix. = 
           case_when(Propagated.to.final.matrix. == "bootstrapped" ~ "yes",
                     Propagated.to.final.matrix. == "not sure" ~ "can't tell",
                     TRUE ~ Propagated.to.final.matrix.)) %>%
  filter(Propagated.to.final.matrix. == "yes" |
           Propagated.to.final.matrix. == "no") %>%
  group_by(Propagated.to.final.matrix.) %>%
  dplyr::summarize(count = n()) %>%
  dplyr::mutate(Perc = count/sum(count)) %>%
  ungroup() %>%
  mutate(position = (Perc/2))

# 25.4 no, 74.6 yes

# by paper
Propagated_uncertainty_paper <- vital_rates %>%
  filter(!is.na(Uncertainty1)) %>%
  mutate(Propagated.to.final.matrix. = 
           case_when(Propagated.to.final.matrix. == "bootstrapped" ~ "yes",
                     Propagated.to.final.matrix. == "not sure" ~ "can't tell",
                     TRUE ~ Propagated.to.final.matrix.)) %>%
  filter(Propagated.to.final.matrix. == "yes" |
           Propagated.to.final.matrix. == "no") %>%
  group_by(DOI, Propagated.to.final.matrix.) %>%
  summarise(paper_count = n()) %>%
  mutate(percent = paper_count/sum(paper_count)) %>%
  ungroup() %>%
  complete(Propagated.to.final.matrix., nesting(DOI), # add in rows that are missing
           fill = list(paper_count = 0)) %>%
  group_by(DOI, Propagated.to.final.matrix.) %>%
  filter(Propagated.to.final.matrix. == "yes") 

length(which(Propagated_uncertainty_paper$paper_count == 0))/
  length(Propagated_uncertainty_paper$paper_count)

# 20% propagate no uncertainty

#### HOW MANY LAMBDAS WITH UNCERTAINTY CROSS 1? ####

lambda_checks <- vital_rates %>%
  filter(!Estimate %in% c("not reported", "Can't find it") &
         !Uncertainty1 %in% c("not reported", "Can't find it") &
         Uncertainty2 != "Can't find it",
         Vital.rate == "lambda") %>%
  mutate(Estimate = as.numeric(Estimate),
         Uncertainty1 = as.numeric(Uncertainty1),
         Uncertainty2 = as.numeric(Uncertainty2)) %>%
  mutate(cross1 = case_when(Uncertainty1 < 1 & Uncertainty2 > 1
                               ~ TRUE,
                            TRUE ~ FALSE)) %>%
  group_by(cross1) %>%
  summarise(count = n()) # 19/(19+37) = 33.9 %

# and what uncertainty went into them? I.e. split by complete, F or S

# do the elasticities change if you take lower and upper CI instead of mean?






