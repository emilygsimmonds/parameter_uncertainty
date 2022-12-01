# SCRIPT TO FORMAT DATA FROM REVIEW #

#### Set up ####

# load packages

library(tidyverse)

# import data

general_questions <- read.csv('./Data files/general_questions.csv', 
                              header = TRUE, 
                              na.strings = c("", "NA")) # general questions

general_questions <- general_questions[rowSums(is.na(general_questions))
                                       != ncol(general_questions),] # remove all NA rows

# vital rates

vital_rates <- read.csv('./Data files/vital_rates.csv', header = TRUE)

#### Format data to be used ####

# make question column a factor

number_questions <- general_questions %>% mutate(number = 
                                                   as.numeric(as.factor(Question)))
# note that numbers for questions are alphabetical!

length(unique(number_questions$DOI))
length(unique(vital_rates$DOI))

#### Source required scripts ####

source('./Scripts/theme_script.R')

# set colour codes for plotting 

colour_code <- viridis::viridis(1, begin = 0.7, end = 0.7)

colfunc <- colorRampPalette(c("white", colour_code))

colour_code_venn <- colfunc(10)

text_code <- viridis::inferno(1, begin = 0, end = 0)


colours <- c("#482677FF", 
             "#DCE319FF", 
             "#55C667FF")

#### Summaries ####

### HOW MANY REPORT ANY UNCERTAINTY? ####

# filter the data, group, and summarise as %
# QU # = 13

Any_uncertainty <- number_questions %>%
  filter(number == 13,
         Answer1 == "yes" | Answer1 == "no") %>%
  group_by(Answer1) %>%
  dplyr::summarize(count = n()) %>%
  dplyr::mutate(Perc = count/sum(count)) %>%
  ungroup() %>%
  mutate(position = (Perc/2))

Any_uncertainty$position[1] <- Any_uncertainty$position[1] +
  Any_uncertainty$Perc[2]

# 14.5 no, 85.5 yes

## PLOT

ggplot(data = Any_uncertainty, aes(x="", y=Perc, fill=Answer1)) +
  geom_bar(stat="identity", width=1, color="white") +
  plain_theme() +
  scale_fill_manual(values = colours, guide = "none") +
  geom_text(aes(y = position, label = paste0(round(Perc*100), "% ", Answer1)), 
            colour = rev(colours[1:2])) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.justification = "top",
        #axis.text.y = element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        strip.text.y.left = element_text(angle = 0, hjust = 1, face = "bold"),
        strip.background = element_blank())+
  labs(title = "Was any uncertainty reported?")


### WAS UNCERTAINTY COMPLETE? ####

# filter the data, group, and summarise as %
# QU # = 4

Complete_uncertainty <- number_questions %>%
  filter(number == 4,
         Answer1 == "yes" | Answer1 == "no") %>%
  group_by(Answer1) %>%
  dplyr::summarize(count = n()) %>%
  dplyr::mutate(Perc = count/sum(count)) %>%
  ungroup()   %>%
  mutate(position = (Perc/2))

Complete_uncertainty$position[1] <- Complete_uncertainty$position[1] +
  Complete_uncertainty$Perc[2]

ggplot(data = Complete_uncertainty, aes(x="", y=Perc, fill=Answer1)) +
  geom_bar(stat="identity", width=1, color="white") +
  plain_theme() +
  scale_fill_manual(values = colours, guide = "none") +
  geom_text(aes(y = position, label = paste0(round(Perc*100), "% ", Answer1)), 
            colour = rev(colours[1:2])) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.justification = "top",
        #axis.text.y = element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        strip.text.y.left = element_text(angle = 0, hjust = 1, face = "bold"),
        strip.background = element_blank())+
  labs(title = "Was uncertainty complete for all matrix elements?")

# 44.4 no, 55.6 yes

### WAS IT PROPAGATED ####

# use vital rate data
# filter the data, group, and summarise as %

# by vital rate
Propagated_uncertainty <- vital_rates %>%
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

Propagated_uncertainty$position[1] <- Propagated_uncertainty$position[1] +
  Propagated_uncertainty$Perc[2]

ggplot(data = Propagated_uncertainty, aes(x="", y=Perc, 
                                          fill=Propagated.to.final.matrix.)) +
  geom_bar(stat="identity", width=1, color="white") +
  plain_theme() +
  scale_fill_manual(values = colours, guide = "none") +
  geom_text(aes(y = position, label = paste0(round(Perc*100), "% ", 
                                             Propagated.to.final.matrix.)), 
            colour = rev(colours[1:2])) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.justification = "top",
        #axis.text.y = element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        strip.text.y.left = element_text(angle = 0, hjust = 1, face = "bold"),
        strip.background = element_blank())+
  labs(title = "Was uncertainty propagated to final predictions? \n(by vital rate)")

# 22.9 no, 77.1 yes

# by paper
Propagated_uncertainty_paper <- vital_rates %>%
  mutate(Propagated.to.final.matrix. = 
           case_when(Propagated.to.final.matrix. == "bootstrapped" ~ "yes",
                     Propagated.to.final.matrix. == "not sure" ~ "can't tell",
                     TRUE ~ Propagated.to.final.matrix.)) %>%
  filter(Propagated.to.final.matrix. == "yes" |
           Propagated.to.final.matrix. == "no") %>%
  group_by(DOI, Propagated.to.final.matrix.) %>%
  summarise(paper_count = n()) %>%
  ungroup() %>%
  complete(Propagated.to.final.matrix., nesting(DOI), # add in rows that are missing
           fill = list(paper_count = 0)) %>%
  filter(Propagated.to.final.matrix. == "yes") 

length(which(Propagated_uncertainty_paper$paper_count == 0))/
  length(Propagated_uncertainty_paper$paper_count)

# 23% propagate no uncertainty







