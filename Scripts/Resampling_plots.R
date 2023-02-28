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

################################################################################

#### Plot ####

colours <- c("#feff54","#FFFFFF", "#30666B", "#1E2E39")

################################################################################

#### PLOTS ###

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
             cols = vars(uncertainty_level)) +
  plain_theme() +
  theme(legend.position = "none") +
  ylim(0,5)

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
  ylim(0,5)

Figure_2

ggsave("Figure_2x2_breed_multiple.png", last_plot(), width = 15, height = 25, units = "cm", 
       dpi = 300)

#### BES FIGURE2: matrix 2: survival dominant results ####

reference <- plot_results$lambda[which(plot_results$matrix_name == "matrix2" &
                                         plot_results$propagation_type == "no \npropagation")] >= 1

filter(plot_results, matrix_name == "matrix2",
       uncertainty_level == "mean") %>%
  mutate(checker = lambda >= 1,
         checker2 = checker == reference) %>%
  group_by(propagation_type, checker2) %>%
  summarise(count = (n()/1000)*100)


Figure_2_data <- filter(plot_results,
                        uncertainty_level == "mean" |
                          is.na(uncertainty_level),
                        matrix_name == "matrix2") %>%
  bind_rows(filter(plot_results,
                   propagation_type == "no \npropagation",
                   matrix_name == "matrix2"), filter(plot_results,
                                         propagation_type == "no \npropagation",
                                         matrix_name == "matrix2"))

Figure_2A <- ggplot() +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "white")+
  geom_violin(data = Figure_2_data, 
              aes(x = propagation_type, 
                  y = lambda, fill = propagation_type, 
                  colour = propagation_type),
              #draw_quantiles = c(0.025, 0.5, 0.975),
              scale = "width") +
  scale_fill_manual(values = c(colours[1], "transparent", "transparent",
                               "transparent")) +
  scale_color_manual(values = c(colours[1], "transparent", "transparent", 
                                "transparent"))+
  labs(x = "",
       y = "Population growth rate") +
  ylim(c(0.4,1.6)) +
  BES_theme() +
  theme(legend.position = "none")

Figure_2A.1 <- ggplot() +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "white")+
  geom_violin(data = Figure_2_data, 
              aes(x = propagation_type, 
                  y = lambda, fill = propagation_type, 
                  colour = propagation_type),
              draw_quantiles = c(0.025, 0.5, 0.975),
              scale = "width") +
  scale_fill_manual(values = c(colours[1], "transparent", 
                               "transparent", colours[4])) +
  scale_color_manual(values = c(colours[1], "transparent","transparent", 
                                "white"))+
  labs(x = "",
       y = "Population growth rate") +
  ylim(c(0.4, 1.6)) +
  BES_theme() +
  theme(legend.position = "none")

Figure_2A.2 <- ggplot() +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "white")+
  geom_violin(data = Figure_2_data, 
              aes(x = propagation_type, 
                  y = lambda, fill = propagation_type, 
                  colour = propagation_type),
              draw_quantiles = c(0.025, 0.5, 0.975),
              scale = "width") +
  scale_fill_manual(values = c(colours[1], "transparent", colours[3:4])) +
  scale_color_manual(values = c(colours[1], "transparent", "white", "white"))+
  labs(x = "",
       y = "Population growth rate") +
  ylim(c(0.4, 1.6)) +
  BES_theme() +
  theme(legend.position = "none")

Figure_2B <- ggplot() +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "white")+
  geom_hline(yintercept = c(quantile(filter(Figure_2_data,
                                            propagation_type == "full \npropagation")$lambda,
                                     probs = c(0.025, 0.5, 0.975))),
             colour = "grey50", size = 2) + 
  geom_violin(data = Figure_2_data, 
              aes(x = propagation_type, 
                  y = lambda, fill = propagation_type, 
                  colour = propagation_type),
              draw_quantiles = c(0.025, 0.5, 0.975),
              scale = "width") +
  scale_fill_manual(values = colours[1:4]) +
  scale_color_manual(values = c(colours[1], "black", "white", "white"))+
  labs(x = "",
       y = "Population growth rate") +
  ylim(c(0.4,1.6)) +
  BES_theme() +
  theme(legend.position = "none")

Figure_2A

ggsave("BES_Fig2A.png", last_plot(), width = 15, height = 15, units = "cm", 
       dpi = 300)

Figure_2A.1

ggsave("BES_Fig2A.1.png", last_plot(), width = 15, height = 15, units = "cm", 
       dpi = 300)

Figure_2A.2

ggsave("BES_Fig2A.2.png", last_plot(), width = 15, height = 15, units = "cm", 
       dpi = 300)

Figure_2B

ggsave("BES_Fig2B.png", last_plot(), width = 15, height = 15, units = "cm", 
       dpi = 300)


#### BES FIGURE3: matrix 3 results ####

reference <- plot_results$lambda[which(plot_results$matrix_name == "matrix3" &
                                         plot_results$propagation_type == "no \npropagation")] >1

filter(plot_results, matrix_name == "matrix3",
       uncertainty_level == "mean") %>%
  mutate(checker = lambda > 1,
         checker2 = checker == reference) %>%
  group_by(propagation_type, checker2) %>%
  summarise(count = (n()/1000)*100)

Figure_3_data <- filter(plot_results,
                        uncertainty_level == "mean" |
                          is.na(uncertainty_level),
                        matrix_name == "matrix3") %>%
  bind_rows(filter(plot_results,
                   propagation_type == "no \npropagation",
                   matrix_name == "matrix3"), filter(plot_results,
                                         propagation_type == "no \npropagation",
                                         matrix_name == "matrix3"))

Figure_3A <- ggplot() +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "white")+
  geom_violin(data = Figure_3_data, 
              aes(x = propagation_type, 
                  y = lambda, fill = propagation_type, 
                  colour = propagation_type),
              draw_quantiles = c(0.025, 0.5, 0.975),
              scale = "width") +
  scale_fill_manual(values = c(colours[1:4])) +
  scale_color_manual(values = c(colours[1], "black", "white", 
                                "white"))+
  labs(x = "",
       y = "Population growth rate") +
  ylim(c(0.8, 2.2)) +
  BES_theme() +
  theme(legend.position = "none")


Figure_3B <- ggplot() +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "white")+
  geom_hline(yintercept = c(quantile(filter(Figure_3_data,
                                            propagation_type == "full \npropagation")$lambda,
                                     probs = c(0.025, 0.5, 0.975))),
             colour = "grey50", size = 2) + 
  geom_violin(data = Figure_3_data, 
              aes(x = propagation_type, 
                  y = lambda, fill = propagation_type, 
                  colour = propagation_type),
              draw_quantiles = c(0.025, 0.5, 0.975),
              scale = "width") +
  scale_fill_manual(values = colours[1:4]) +
  scale_color_manual(values = c(colours[1],"black",  "white", "white"))+
  labs(x = "",
       y = "Population growth rate") +
  ylim(c(0.8,2.2)) +
  BES_theme() +
  theme(legend.position = "none")

Figure_3A

ggsave("BES_Fig3A.png", last_plot(), width = 15, height = 15, units = "cm", 
       dpi = 300)

Figure_3B

ggsave("BES_Fig3B.png", last_plot(), width = 15, height = 15, units = "cm", 
       dpi = 300)


################################################################################

#### Elasticity summary ####

# % time the elasticity element changed

elasticity_results <- full_results %>% filter(propagation_type != "mean_matrix") %>%
  group_by(propagation_type, matrix_name, uncertainty_level) %>%
  summarise(percentage = 1-(sum(elasticity)/1000))

# can probably just look at full

filter(elasticity_results, propagation_type == "full", 
       uncertainty_level == "mean")

# changed in 41 %, 0.5 % and 47 % of cases
