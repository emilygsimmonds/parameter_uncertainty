# T1.2: Script to create resampled datasets and plot #

# Takes input of mean matrix and uncertainty %

# runs all combinations of uncertainty and produces a combined dataframe of output

################################################################################

## load packages ####

library(tidyverse)
library(popbio)
library(popdemo)

## source scripts ####

source("./Scripts/theme_script.R")

## import results ####

load("./Data files/two_by_two_results.RData")
load("./Data files/three_by_three_results.RData")
load("./Data files/five_by_five_results.RData")

################################################################################

#### Plot ####

colours <- c("#feff54","#FFFFFF", "#30666B", "#1E2E39")

################################################################################

#### PLOTS 2x2 ####

# re-order the factor levels
plot_results_temp <- bind_rows(filter(two_by_two_results,
                                      uncertainty_level != "none",
                                      breeding_stages == "one"), 
                          filter(two_by_two_results,
                          uncertainty_level == "none",
                          breeding_stages == "one",
                          matrix_number == 1)[1:30,],
                          filter(two_by_two_results,
                                 uncertainty_level == "none",
                                 breeding_stages == "one",
                                 matrix_number == 2)[1:30,],
                          filter(two_by_two_results,
                                 uncertainty_level == "none",
                                 breeding_stages == "one",
                                 matrix_number == 3)[1:30,],
                          filter(two_by_two_results,
                                 uncertainty_level == "none",
                                 breeding_stages == "one",
                                 matrix_number == 4)[1:30,],
                          filter(two_by_two_results,
                                 uncertainty_level == "none",
                                 breeding_stages == "one",
                                 matrix_number == 5)[1:30,])

plot_results_temp[(nrow(plot_results_temp)-149):nrow(plot_results_temp),
                  "uncertainty_level"] <-
  rep(rep(c("high", "mid", "low"), each = 10), 5)

plot_results <- plot_results_temp %>%
  dplyr::mutate(prop_scenario = as.factor(prop_scenario),
                prop_scenario = plyr::revalue(prop_scenario, 
                                           c("f_only"="fecundity \nonly", 
                                             "s_only"="survival \nonly",
                                             "full" = "full \npropagation",
                                             "none" = "no \npropagation")),
                prop_scenario = fct_relevel(prop_scenario, 
                                     "no \npropagation",
                                     "full \npropagation",
                                     "fecundity \nonly",
                                     "survival \nonly"),
                uncertainty_level = as.factor(uncertainty_level),
                uncertainty_level = fct_relevel(uncertainty_level,
                                                "low",
                                                "mid",
                                                "high")) %>%
  filter(uncertainty_level != "none") 



#### FIGURE 1: 2x2 - breed once ####
  
Figure_1 <- ggplot() +
  geom_hline(yintercept = c(quantile(filter(plot_results,
                                            prop_scenario == "full \npropagation")$lambda,
                                     probs = c(0.025, 0.5, 0.975))),
             colour = "grey50", linewidth = 1) + 
  geom_hline(yintercept = 1, linetype = "dashed", colour = "blue") +
  geom_violin(data = filter(plot_results,
                            breeding_stages == "one"), 
              aes(x = prop_scenario, 
                  y = lambda, fill = prop_scenario,
                  colour = prop_scenario),
              scale = "width") +
  scale_fill_manual(values = colours[1:4]) +
  scale_color_manual(values = c(colours[1], "black", colours[3:4]))+
  labs(x = "",
       y = "Population growth rate") +
  facet_grid(rows = vars(matrix_number),
             cols = vars(uncertainty_level), scales = "free_y") +
  plain_theme() +
  theme(legend.position = "none") +
  ylim(0,3)

Figure_1

ggsave("Figure_2x2_breed_once.png", last_plot(), width = 15, height = 25, units = "cm", 
       dpi = 300)

#### FIGURE 2: 2x2 - breed multiple ####

# re-order the factor levels
plot_results_temp <- bind_rows(filter(two_by_two_results,
                                      uncertainty_level != "none",
                                      breeding_stages == "multiple"), 
                               filter(two_by_two_results,
                                      uncertainty_level == "none",
                                      breeding_stages == "multiple",
                                      matrix_number == 1)[1:30,],
                               filter(two_by_two_results,
                                      uncertainty_level == "none",
                                      breeding_stages == "multiple",
                                      matrix_number == 2)[1:30,],
                               filter(two_by_two_results,
                                      uncertainty_level == "none",
                                      breeding_stages == "multiple",
                                      matrix_number == 3)[1:30,],
                               filter(two_by_two_results,
                                      uncertainty_level == "none",
                                      breeding_stages == "multiple",
                                      matrix_number == 4)[1:30,],
                               filter(two_by_two_results,
                                      uncertainty_level == "none",
                                      breeding_stages == "multiple",
                                      matrix_number == 5)[1:30,])

plot_results_temp[(nrow(plot_results_temp)-149):nrow(plot_results_temp),
                  "uncertainty_level"] <-
  rep(rep(c("high", "mid", "low"), each = 10), 5)

plot_results <- plot_results_temp %>%
  dplyr::mutate(prop_scenario = as.factor(prop_scenario),
                prop_scenario = plyr::revalue(prop_scenario, 
                                              c("f_only"="fecundity \nonly", 
                                                "s_only"="survival \nonly",
                                                "full" = "full \npropagation",
                                                "none" = "no \npropagation")),
                prop_scenario = fct_relevel(prop_scenario, 
                                            "no \npropagation",
                                            "full \npropagation",
                                            "fecundity \nonly",
                                            "survival \nonly"),
                uncertainty_level = as.factor(uncertainty_level),
                uncertainty_level = fct_relevel(uncertainty_level,
                                                "low",
                                                "mid",
                                                "high")) %>%
  filter(uncertainty_level != "none") 

Figure_2 <- ggplot() +
  geom_hline(yintercept = c(quantile(filter(plot_results,
                                            prop_scenario == "full \npropagation")$lambda,
                                     probs = c(0.025, 0.5, 0.975))),
             colour = "grey50", linewidth = 1) + 
  geom_hline(yintercept = 1, linetype = "dashed", colour = "blue") +
  geom_violin(data = filter(plot_results,
                            breeding_stages == "multiple"), 
              aes(x = prop_scenario, 
                  y = lambda, fill = prop_scenario,
                  colour = prop_scenario),
              scale = "width") +
  scale_fill_manual(values = colours[1:4]) +
  scale_color_manual(values = c(colours[1], "black", colours[3:4]))+
  labs(x = "",
       y = "Population growth rate") +
  facet_grid(rows = vars(matrix_number),
             cols = vars(uncertainty_level)) +
  plain_theme() +
  theme(legend.position = "none") +
  ylim(0,4)

Figure_2

ggsave("Figure_2x2_breed_multiple.png", last_plot(), width = 15, height = 25, units = "cm", 
       dpi = 300)


#### PLOTS 3x3 ####

# re-order the factor levels
plot_results_temp <- bind_rows(filter(three_by_three_results,
                                      uncertainty_level != "none",
                                      breeding_stages == "one"), 
                               filter(three_by_three_results,
                                      uncertainty_level == "none",
                                      breeding_stages == "one",
                                      matrix_number == 1)[1:30,],
                               filter(three_by_three_results,
                                      uncertainty_level == "none",
                                      breeding_stages == "one",
                                      matrix_number == 2)[1:30,],
                               filter(three_by_three_results,
                                      uncertainty_level == "none",
                                      breeding_stages == "one",
                                      matrix_number == 3)[1:30,],
                               filter(three_by_three_results,
                                      uncertainty_level == "none",
                                      breeding_stages == "one",
                                      matrix_number == 4)[1:30,],
                               filter(three_by_three_results,
                                      uncertainty_level == "none",
                                      breeding_stages == "one",
                                      matrix_number == 5)[1:30,])

plot_results_temp[(nrow(plot_results_temp)-149):nrow(plot_results_temp),
                  "uncertainty_level"] <-
  rep(rep(c("high", "mid", "low"), each = 10), 5)

plot_results <- plot_results_temp %>%
  dplyr::mutate(prop_scenario = as.factor(prop_scenario),
                prop_scenario = plyr::revalue(prop_scenario, 
                                              c("f_only"="fecundity \nonly", 
                                                "s_only"="survival \nonly",
                                                "full" = "full \npropagation",
                                                "none" = "no \npropagation")),
                prop_scenario = fct_relevel(prop_scenario, 
                                            "no \npropagation",
                                            "full \npropagation",
                                            "fecundity \nonly",
                                            "survival \nonly"),
                uncertainty_level = as.factor(uncertainty_level),
                uncertainty_level = fct_relevel(uncertainty_level,
                                                "low",
                                                "mid",
                                                "high")) %>%
  filter(uncertainty_level != "none") 



#### FIGURE 1: 3x3 - breed once ####

Figure_1 <- ggplot() +
  geom_hline(yintercept = c(quantile(filter(plot_results,
                                            prop_scenario == "full \npropagation")$lambda,
                                     probs = c(0.025, 0.5, 0.975))),
             colour = "grey50", linewidth = 1) + 
  geom_hline(yintercept = 1, linetype = "dashed", colour = "blue") +
  geom_violin(data = filter(plot_results,
                            breeding_stages == "one"), 
              aes(x = prop_scenario, 
                  y = lambda, fill = prop_scenario,
                  colour = prop_scenario),
              scale = "width") +
  scale_fill_manual(values = colours[1:4]) +
  scale_color_manual(values = c(colours[1], "black", colours[3:4]))+
  labs(x = "",
       y = "Population growth rate") +
  facet_grid(rows = vars(matrix_number),
             cols = vars(uncertainty_level)) +
  plain_theme() +
  theme(legend.position = "none") +
  ylim(0,3)

Figure_1

ggsave("Figure_3x3_breed_once.png", last_plot(), width = 15, height = 25, units = "cm", 
       dpi = 300)

#### FIGURE 2: 3x3 - breed multiple ####

# re-order the factor levels
plot_results_temp <- bind_rows(filter(three_by_three_results,
                                      uncertainty_level != "none",
                                      breeding_stages == "multiple"), 
                               filter(three_by_three_results,
                                      uncertainty_level == "none",
                                      breeding_stages == "multiple",
                                      matrix_number == 1)[1:30,],
                               filter(three_by_three_results,
                                      uncertainty_level == "none",
                                      breeding_stages == "multiple",
                                      matrix_number == 2)[1:30,],
                               filter(three_by_three_results,
                                      uncertainty_level == "none",
                                      breeding_stages == "multiple",
                                      matrix_number == 3)[1:30,],
                               filter(three_by_three_results,
                                      uncertainty_level == "none",
                                      breeding_stages == "multiple",
                                      matrix_number == 4)[1:30,],
                               filter(three_by_three_results,
                                      uncertainty_level == "none",
                                      breeding_stages == "multiple",
                                      matrix_number == 5)[1:30,])

plot_results_temp[(nrow(plot_results_temp)-149):nrow(plot_results_temp),
                  "uncertainty_level"] <-
  rep(rep(c("high", "mid", "low"), each = 10), 5)

plot_results <- plot_results_temp %>%
  dplyr::mutate(prop_scenario = as.factor(prop_scenario),
                prop_scenario = plyr::revalue(prop_scenario, 
                                              c("f_only"="fecundity \nonly", 
                                                "s_only"="survival \nonly",
                                                "full" = "full \npropagation",
                                                "none" = "no \npropagation")),
                prop_scenario = fct_relevel(prop_scenario, 
                                            "no \npropagation",
                                            "full \npropagation",
                                            "fecundity \nonly",
                                            "survival \nonly"),
                uncertainty_level = as.factor(uncertainty_level),
                uncertainty_level = fct_relevel(uncertainty_level,
                                                "low",
                                                "mid",
                                                "high")) %>%
  filter(uncertainty_level != "none") 

Figure_2 <- ggplot() +
  geom_hline(yintercept = c(quantile(filter(plot_results,
                                            prop_scenario == "full \npropagation")$lambda,
                                     probs = c(0.025, 0.5, 0.975))),
             colour = "grey50", linewidth = 1) + 
  geom_hline(yintercept = 1, linetype = "dashed", colour = "blue") +
  geom_violin(data = filter(plot_results,
                            breeding_stages == "multiple"), 
              aes(x = prop_scenario, 
                  y = lambda, fill = prop_scenario,
                  colour = prop_scenario),
              scale = "width") +
  scale_fill_manual(values = colours[1:4]) +
  scale_color_manual(values = c(colours[1], "black", colours[3:4]))+
  labs(x = "",
       y = "Population growth rate") +
  facet_grid(rows = vars(matrix_number),
             cols = vars(uncertainty_level)) +
  plain_theme() +
  theme(legend.position = "none") +
  ylim(0,8)

Figure_2

ggsave("Figure_3x3_breed_multiple.png", last_plot(), width = 15, height = 25, units = "cm", 
       dpi = 300)


#### PLOTS 5x5 ####

# re-order the factor levels
plot_results_temp <- bind_rows(filter(five_by_five_results,
                                      uncertainty_level != "none",
                                      breeding_stages == "one"), 
                               filter(five_by_five_results,
                                      uncertainty_level == "none",
                                      breeding_stages == "one",
                                      matrix_number == 1)[1:30,],
                               filter(five_by_five_results,
                                      uncertainty_level == "none",
                                      breeding_stages == "one",
                                      matrix_number == 2)[1:30,],
                               filter(five_by_five_results,
                                      uncertainty_level == "none",
                                      breeding_stages == "one",
                                      matrix_number == 3)[1:30,],
                               filter(five_by_five_results,
                                      uncertainty_level == "none",
                                      breeding_stages == "one",
                                      matrix_number == 4)[1:30,],
                               filter(five_by_five_results,
                                      uncertainty_level == "none",
                                      breeding_stages == "one",
                                      matrix_number == 5)[1:30,])

plot_results_temp[(nrow(plot_results_temp)-149):nrow(plot_results_temp),
                  "uncertainty_level"] <-
  rep(rep(c("high", "mid", "low"), each = 10), 5)

plot_results <- plot_results_temp %>%
  dplyr::mutate(prop_scenario = as.factor(prop_scenario),
                prop_scenario = plyr::revalue(prop_scenario, 
                                              c("f_only"="fecundity \nonly", 
                                                "s_only"="survival \nonly",
                                                "full" = "full \npropagation",
                                                "none" = "no \npropagation")),
                prop_scenario = fct_relevel(prop_scenario, 
                                            "no \npropagation",
                                            "full \npropagation",
                                            "fecundity \nonly",
                                            "survival \nonly"),
                uncertainty_level = as.factor(uncertainty_level),
                uncertainty_level = fct_relevel(uncertainty_level,
                                                "low",
                                                "mid",
                                                "high")) %>%
  filter(uncertainty_level != "none") 



#### FIGURE 1: 5x5 - breed once ####

Figure_1 <- ggplot() +
  geom_hline(yintercept = c(quantile(filter(plot_results,
                                            prop_scenario == "full \npropagation")$lambda,
                                     probs = c(0.025, 0.5, 0.975))),
             colour = "grey50", linewidth = 1) + 
  geom_hline(yintercept = 1, linetype = "dashed", colour = "blue") +
  geom_violin(data = filter(plot_results,
                            breeding_stages == "one"), 
              aes(x = prop_scenario, 
                  y = lambda, fill = prop_scenario,
                  colour = prop_scenario),
              scale = "width") +
  scale_fill_manual(values = colours[1:4]) +
  scale_color_manual(values = c(colours[1], "black", colours[3:4]))+
  labs(x = "",
       y = "Population growth rate") +
  facet_grid(rows = vars(matrix_number),
             cols = vars(uncertainty_level)) +
  plain_theme() +
  theme(legend.position = "none") +
  ylim(0,3)

Figure_1

ggsave("Figure_5x5_breed_once.png", last_plot(), width = 15, height = 25, units = "cm", 
       dpi = 300)

#### FIGURE 2: 5x5 - breed multiple ####

# re-order the factor levels
plot_results_temp <- bind_rows(filter(five_by_five_results,
                                      uncertainty_level != "none",
                                      breeding_stages == "multiple"), 
                               filter(five_by_five_results,
                                      uncertainty_level == "none",
                                      breeding_stages == "multiple",
                                      matrix_number == 1)[1:30,],
                               filter(five_by_five_results,
                                      uncertainty_level == "none",
                                      breeding_stages == "multiple",
                                      matrix_number == 2)[1:30,],
                               filter(five_by_five_results,
                                      uncertainty_level == "none",
                                      breeding_stages == "multiple",
                                      matrix_number == 3)[1:30,],
                               filter(five_by_five_results,
                                      uncertainty_level == "none",
                                      breeding_stages == "multiple",
                                      matrix_number == 4)[1:30,],
                               filter(five_by_five_results,
                                      uncertainty_level == "none",
                                      breeding_stages == "multiple",
                                      matrix_number == 5)[1:30,])

plot_results_temp[(nrow(plot_results_temp)-149):nrow(plot_results_temp),
                  "uncertainty_level"] <-
  rep(rep(c("high", "mid", "low"), each = 10), 5)

plot_results <- plot_results_temp %>%
  dplyr::mutate(prop_scenario = as.factor(prop_scenario),
                prop_scenario = plyr::revalue(prop_scenario, 
                                              c("f_only"="fecundity \nonly", 
                                                "s_only"="survival \nonly",
                                                "full" = "full \npropagation",
                                                "none" = "no \npropagation")),
                prop_scenario = fct_relevel(prop_scenario, 
                                            "no \npropagation",
                                            "full \npropagation",
                                            "fecundity \nonly",
                                            "survival \nonly"),
                uncertainty_level = as.factor(uncertainty_level),
                uncertainty_level = fct_relevel(uncertainty_level,
                                                "low",
                                                "mid",
                                                "high")) %>%
  filter(uncertainty_level != "none") 

Figure_2 <- ggplot() +
  geom_hline(yintercept = c(quantile(filter(plot_results,
                                            prop_scenario == "full \npropagation")$lambda,
                                     probs = c(0.025, 0.5, 0.975))),
             colour = "grey50", linewidth = 1) + 
  geom_hline(yintercept = 1, linetype = "dashed", colour = "blue") +
  geom_violin(data = filter(plot_results,
                            breeding_stages == "multiple"), 
              aes(x = prop_scenario, 
                  y = lambda, fill = prop_scenario,
                  colour = prop_scenario),
              scale = "width") +
  scale_fill_manual(values = colours[1:4]) +
  scale_color_manual(values = c(colours[1], "black", colours[3:4]))+
  labs(x = "",
       y = "Population growth rate") +
  facet_grid(rows = vars(matrix_number),
             cols = vars(uncertainty_level)) +
  plain_theme() +
  theme(legend.position = "none") +
  ylim(0,3)

Figure_2

ggsave("Figure_5x5_breed_multiple.png", last_plot(), width = 15, height = 25, units = "cm", 
       dpi = 300)

################################################################################

#### Elasticity summary ####

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
