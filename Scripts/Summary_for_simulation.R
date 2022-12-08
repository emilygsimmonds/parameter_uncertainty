# T1.2: SCRIPT TO SUMMARISE REVIEW FOR SIMULATIONS #

################################################################################

#### Set up ####

## load packages ####

library(tidyverse)

## load data ####

# vital rates
vital_rates <- read.csv("./Data files/vital_rates.csv", header = TRUE)

# comadre working data

load("./Data files/working_comadre.RData")

#### restricting to 3x3 reduces sample size a lot ####

#### try summarising by matrix position instead ####

# summarise uncertainty by paper, model, type, and matrix position

test <- vital_rates %>% filter(Vital.rate != "lambda",
                               Uncertainty1 != "can't tell") %>% 
  drop_na(Matrix.position) %>%
  group_by(Uncertainty.type, Matrix.position) %>%
  summarise(count = n())

# detect which vital rates relate to fecundity

vital_rates_editted <- vital_rates %>% 
  mutate(Fecundity = str_detect(Matrix.position, "1,")) %>%
  mutate(Fecundity = case_when(str_detect(Vital.rate, "reproduction") ~ TRUE,
                               str_detect(Vital.rate, "recruitment") ~ TRUE,
                               str_detect(Vital.rate, "fecundity") ~ TRUE,
                               str_detect(Vital.rate, "fertility") ~ TRUE,
                               str_detect(Vital.rate, "survival") ~ FALSE,
                               Fecundity == TRUE ~ TRUE,
                               TRUE ~ NA)) %>%
  drop_na(Fecundity) %>%
  filter(Uncertainty.type == "standard error" | 
         Uncertainty.type == "confidence interval" |
         Uncertainty.type == "standard deviation",
         Uncertainty1 != "Can't find it" & Uncertainty1 != "not reported",
         Uncertainty1 > 0) 

# next, want to change all confidence intervals into standard error (CHECK)

vital_rates_editted2 <- vital_rates_editted %>% 
  mutate(Uncertainty1 = as.numeric(Uncertainty1),
         Uncertainty2 = as.numeric(Uncertainty2),
         Estimate = as.numeric(Estimate)) %>%
  mutate(check = case_when(Uncertainty.type == "confidence interval" ~ 
                             (Uncertainty2 - Estimate),
                           TRUE ~ Estimate),
         check2 = case_when(Uncertainty.type == "confidence interval" ~ 
                             round((Uncertainty1+check)-Estimate, 2),
                           TRUE ~ 0)) %>%
  mutate(Uncertainty1 = case_when(Uncertainty.type == "confidence interval" ~ 
                                   check/2,
                                  TRUE ~ Uncertainty1)) %>% # turn Uncertainty1 into a standard error for CI
  filter(check2 < 0.06 & check2 > -0.06) %>% # margin of tolerance of 0.05 otherwise exclude
  mutate(proportion = Uncertainty1/Estimate) 

which(vital_rates_editted2$proportion > 1)
vital_rates_editted2[238,]

# then take the mean

# then calculate summaries of the proportional error
vital_rates_editted2 %>% # then calculate each as a proportion of the mean estimate
  group_by(Fecundity) %>% summarise(min = min(proportion, na.rm =TRUE), 
                                    mean = mean(proportion, na.rm =TRUE), 
                                    max = max(proportion, na.rm =TRUE),
                                    count = n()) # take 1st and 3rd quartiles 
# Survival = 0.005% to 38% mean 13% n = 61
