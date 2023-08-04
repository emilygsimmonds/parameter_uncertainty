# T1.2: Script to summarise the results of the review #

################################################################################

#### Set up ####

# load packages

library(tidyverse)

# source required scripts 

source('./Scripts/theme_script.R')

# load data 

general_questions <- read.csv('./Data files/general_questions_cleaned.csv',
                              header = TRUE)
vital_rates <- read.csv('./Data files/vital_rates.csv', header = TRUE)

################################################################################

#### Initial high level summaries ####

length(unique(general_questions$DOI)) # 116 number of DOIs

number_papers <- general_questions %>%
  filter(Number == 5,
         Answer1 == "yes")  

length(unique(number_papers$DOI)) # number of papers with uncertainty

number_papers <- general_questions %>%
  filter(Number == 5,
         Answer1 == "no") # number of papers without uncertainty

length(unique(number_papers$DOI))

length(unique(vital_rates$DOI)) # check number of papers in vital rates =
# number with uncertainty: now both match at 82

number_papers <- general_questions %>%
  filter(Number == 5,
         Answer1 == "yes") 

length(unique(number_papers$DOI))

################################################################################

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

# 21.9 no, 78.1 yes

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

# 50 no, 50 yes

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

# most missing both (23/39)
# then missing S and F almost equal 8 vs 9 

general_questions %>%
  filter(Number == 3,
         Answer1 == "yes" |
           Answer1 == "no") %>%
  group_by(Answer1) %>%
  dplyr::summarize(count = n()) %>%
  mutate(percentage = count/sum(count))

# % with survival observation process 44%

general_questions %>%
  filter(Number == 4,
         Answer1 == "yes" |
           Answer1 == "no") %>%
  group_by(Answer1) %>%
  dplyr::summarize(count = n()) %>%
  mutate(percentage = count/sum(count))

# % with fecundity observation process 10.5%

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

# 21 no, 79 yes

# by paper - yes and no only
Propagated_uncertainty_paper <- vital_rates %>%
  filter(!is.na(Uncertainty1)) %>%
  mutate(Propagated.to.final.matrix. = 
           case_when(Propagated.to.final.matrix. == "bootstrapped" ~ "yes",
                     Propagated.to.final.matrix. == "not sure" ~ "can't tell",
                     TRUE ~ Propagated.to.final.matrix.)) %>%
  filter(Propagated.to.final.matrix. == "yes" |
           Propagated.to.final.matrix. == "no" |
           Propagated.to.final.matrix. == "can't tell",
         Vital.rate != str_detect(Vital.rate, "lambda") &
           Vital.rate != str_detect(Vital.rate, "population")) %>% # remove all entries that are NA and remove all derived quantities
  group_by(DOI, Propagated.to.final.matrix.) %>%
  summarise(paper_count = n()) %>%
  mutate(percent = paper_count/sum(paper_count)) %>%
  ungroup() %>%
  complete(Propagated.to.final.matrix., nesting(DOI), # add in rows that are missing
           fill = list(percent = 0,
                       paper_count = 0)) %>%
  group_by(DOI, Propagated.to.final.matrix.) %>%
  filter(Propagated.to.final.matrix. == "yes") %>%
  mutate(Propagation.type = case_when(percent == 1 ~ "all",
                                       percent < 1 & percent > 0 ~ "some",
                                       percent == 0 ~ "none", 
                                       TRUE ~ "none"))

# get a list of papers for which we can't tell
Unclear <- vital_rates %>%
  filter(!is.na(Uncertainty1)) %>%
  mutate(Propagated.to.final.matrix. = 
           case_when(Propagated.to.final.matrix. == "bootstrapped" ~ "yes",
                     Propagated.to.final.matrix. == "not sure" ~ "can't tell",
                     TRUE ~ Propagated.to.final.matrix.)) %>%
  filter(Propagated.to.final.matrix. == "yes" |
           Propagated.to.final.matrix. == "no" |
           Propagated.to.final.matrix. == "can't tell",
         Vital.rate != str_detect(Vital.rate, "lambda") &
           Vital.rate != str_detect(Vital.rate, "population")) %>% # remove all entries that are NA and remove all derived quantities
  group_by(DOI, Propagated.to.final.matrix.) %>%
  summarise(paper_count = n()) %>%
  mutate(percent = paper_count/sum(paper_count)) %>%
  ungroup() %>%
  complete(Propagated.to.final.matrix., nesting(DOI), # add in rows that are missing
           fill = list(paper_count = 0)) %>%
  group_by(DOI, Propagated.to.final.matrix.) %>%
  filter(Propagated.to.final.matrix. == "can't tell" &
           percent > 0) %>%
  mutate(Propagation.type = "can't tell")

# rename columns of unclear so it can be joined
Unclear <- Unclear[,-c(1,3,4)]
colnames(Unclear) <- c("DOI", "Clarity")

# combine with general questions data
combined_data <- left_join(general_questions,
                           Propagated_uncertainty_paper,
                           by = c("DOI"))
combined_data <- left_join(combined_data,
                           Unclear,
                           by = c("DOI")) %>%
  mutate(Propagation.type = case_when(Clarity == "can't tell" ~ Clarity,
                                      TRUE ~ Propagation.type))

length(which(Propagated_uncertainty_paper$paper_count == 0))/
  length(Propagated_uncertainty_paper$paper_count)

#### How many papers that have complete uncertainty propagate it

combined_data %>%
  filter(Number == 6,
         Answer1 == "yes") %>%
  drop_na(Propagation.type) %>%
  group_by(Propagation.type) %>%
  dplyr::summarize(count = n()) %>%
  dplyr::mutate(Perc = count/sum(count)) %>%
  ungroup() %>%
  mutate(position = (Perc/2))

# 82.5% all, 7.5% none, 2.5% some, 7.5 % can't tell

# or with incomplete uncertainty

combined_data %>%
  filter(Number == 6 &
         Answer1 == "no") %>%
  drop_na(Propagation.type) %>%
  group_by(Propagation.type) %>%
  dplyr::summarize(count = n()) %>%
  dplyr::mutate(Perc = count/sum(count)) %>%
  ungroup() %>%
  mutate(position = (Perc/2))

# 39.1% all, 39.1% none, 13% some, 8.7% can't tell # of 16 as some will be missing all vital rates

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

reduced_general_questions[reduced_general_questions$DOI %in% 
                            lambda_checks$DOI[which(lambda_checks$cross1 == TRUE)],]

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

#### Elasticity summary ####

## load results ##

load("./Data files/two_by_two_results.RData")
load("./Data files/three_by_three_results.RData")
load("./Data files/five_by_five_results.RData")

# % time the elasticity element changed

reference_2x2 <- two_by_two_results %>% 
  filter(prop_scenario == "none") %>%
  group_by(matrix_number, breeding_stages) %>%
  summarise(reference = mean(elasticity))

elasticity_results_2x2 <- left_join(two_by_two_results,reference_2x2) %>% 
  filter(prop_scenario != "none") %>%
  mutate(check = elasticity == reference) %>%
  group_by(breeding_stages, prop_scenario, matrix_number, uncertainty_level) %>%
  summarise(percentage = 1-(sum(check)/10000))

write.csv(elasticity_results_2x2, "./Data files/elasticity_results_2x2.csv", 
          row.names = FALSE)

reference_3x3 <- three_by_three_results %>% 
  filter(prop_scenario == "none") %>%
  group_by(matrix_number, breeding_stages) %>%
  summarise(reference = mean(elasticity))

elasticity_results_3x3 <- left_join(three_by_three_results,reference_3x3) %>% 
  filter(prop_scenario != "none") %>%
  mutate(check = elasticity == reference) %>%
  group_by(breeding_stages, prop_scenario, matrix_number, uncertainty_level) %>%
  summarise(percentage = 1-(sum(check)/10000))

write.csv(elasticity_results_3x3, "./Data files/elasticity_results_3x3.csv", 
          row.names = FALSE)

reference_5x5 <- five_by_five_results %>% 
  filter(prop_scenario == "none") %>%
  group_by(matrix_number, breeding_stages) %>%
  summarise(reference = mean(elasticity))

elasticity_results_5x5 <- left_join(five_by_five_results,reference_5x5) %>% 
  filter(prop_scenario != "none") %>%
  mutate(check = elasticity == reference) %>%
  group_by(breeding_stages, prop_scenario, matrix_number, uncertainty_level) %>%
  summarise(percentage = 1-(sum(check)/10000))

write.csv(elasticity_results_5x5, "./Data files/elasticity_results_5x5.csv", 
          row.names = FALSE)

