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

length(unique(general_questions$DOI)) # 117 

number_papers <- general_questions %>%
  filter(Number == 1,
         Answer1 == "yes")  

length(unique(number_papers$DOI))

number_papers <- general_questions %>%
  filter(Number == 1,
         Answer1 == "no") 

length(unique(number_papers$DOI))

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

# 19.2 no, 80.8 yes


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

# 49.4 no, 50.6 yes

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

# most missing both (22/42)
# then missing S and F almost equal 9 vs 10 

general_questions %>%
  filter(Number == 3,
         Answer1 == "yes" |
           Answer1 == "no") %>%
  group_by(Answer1) %>%
  dplyr::summarize(count = n()) %>%
  mutate(percentage = count/sum(count))

# % with survival observation process 43.5%

general_questions %>%
  filter(Number == 4,
         Answer1 == "yes" |
           Answer1 == "no") %>%
  group_by(Answer1) %>%
  dplyr::summarize(count = n()) %>%
  mutate(percentage = count/sum(count))

# % with fecundity observation process 10.4%

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

# 20.4 no, 79.6 yes

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

# 19.7% propagate no uncertainty

#### HOW MANY LAMBDAS WITH UNCERTAINTY CROSS 1? ####

lambda_checks <- vital_rates %>%
  filter(!Estimate %in% c("not reported", "Can't find it") &
           !Uncertainty1 %in% c("not reported", "Can't find it") &
           Uncertainty2 != "Can't find it",
         Uncertainty.type == "standard error" | 
           Uncertainty.type == "confidence interval" |
           Uncertainty.type == "credible interval" |
           Uncertainty.type != "standard deviation",
         Uncertainty1 > 0,
         Vital.rate == "lambda") %>%
  mutate(Estimate = as.numeric(Estimate),
         Uncertainty1 = as.numeric(Uncertainty1),
         Uncertainty2 = case_when(Uncertainty.type == "standard error" ~ Estimate + (2*Uncertainty1),
                                  TRUE ~ as.numeric(Uncertainty2)),
         Uncertainty1 = case_when(Uncertainty.type == "standard error" ~ Estimate - (2*Uncertainty1),
                                  TRUE ~ as.numeric(Uncertainty1))) %>%
  mutate(cross1 = case_when(Uncertainty1 < 1 & Uncertainty2 > 1
                            ~ TRUE,
                            Uncertainty1 < 1 & Uncertainty2 < 1 
                            ~ FALSE,
                            Uncertainty1 > 1 & Uncertainty2 > 1 
                            ~ FALSE,
                            TRUE ~ NA))

lambda_checks %>%
  group_by(cross1) %>%
  summarise(count = n()) 

lambda_checks # 37/(37+50) = 42.5 %

# and what uncertainty went into them? I.e. split by complete, F or S

lambda_checks$DOI[which(lambda_checks$cross1 == TRUE)]

reduced_general_questions <- filter(general_questions, Number == 6)

reduced_general_questions[reduced_general_questions$DOI %in% lambda_checks$DOI[which(lambda_checks$cross1 == TRUE)],]

# vast majority have complete uncertainty

#### Uncertainty types ####

Uncertainty_types <- vital_rates %>%
  filter(!is.na(Uncertainty.type),
         Uncertainty.type != "not reported" &
           Uncertainty.type != "can't tell" &
           Uncertainty.type != "can't find it" &
           Uncertainty.type != "missing" &
           Uncertainty.type != "none but could have been - components used to estimate it had SE" &
           Uncertainty.type != "not reported: maybe standard deviation") %>%
group_by(Uncertainty.type) %>%
  summarise(count = n()) %>%
  mutate(percentage = count/sum(count))

# SE = 37.7%, Confidence Interval = 24%, standard deviation = 21.9%,
# credible interval 6%, process variance = 3.7%


general_questions %>%
  filter(Number == 10) %>%
  group_by(Answer1) %>%
  dplyr::summarize(count = n()) %>%
  dplyr::mutate(Perc = count/sum(count)) %>%
  ungroup()   %>%
  mutate(position = (Perc/2))
