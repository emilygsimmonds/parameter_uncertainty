# T1.2: Script to create conference or publication standard figures #

################################################################################
# load packages
library(tidyverse)
library(patchwork)

# source script

source("./Scripts/theme_script.R")

# load all summary data

load("./Data files/two_by_two_results.RData")
load("./Data files/three_by_three_results.RData")
load("./Data files/five_by_five_results.RData")

# join together all datasets
full_data <- bind_rows(two_by_two_results,
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
                              matrix_number == 5)[1:30,],
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
                              matrix_number == 5)[1:30,],
                       three_by_three_results,
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
                              matrix_number == 5)[1:30,],
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
                              matrix_number == 5)[1:30,],
                       five_by_five_results,
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
                              matrix_number == 5)[1:30,],
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
                              matrix_number == 5)[1:30,]) %>%
  mutate(matrix_size = c(rep("size 2", length(two_by_two_results[,1])+300),
                         rep("size 3", length(three_by_three_results[,1])+300),
                         rep("size 5", length(five_by_five_results[,1])+300)))

# then add labels for low, mid, high uncertainty to those added rows
full_data[c(seq(1005882, 1005882+299, 1), 
            seq(1005882+299+1005426, 1005882+299+1005426+299, 1),
            seq(1005882+299+1005426+299+1028936, 
                1005882+299+1005426+299+1028936+299, 1)),
          "uncertainty_level"] <-
  rep(rep(rep(c("high", "mid", "low"), each = 10), 5),6)

# then rename factors
full_data <- full_data %>%
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

###############################################################################

colours_scenario <- c("#540B0E","#E8E8E8","#444E60","#A1Acc0")

colfunc<-colorRampPalette(c("white", "orangered"))

colours_size <- colfunc(5)

################################################################################

#### CONFERENCE ####

#### FIGURE 1: Propagation scenarios - matrix 1, multiple breed, mid uncert ####

Figure_1A <- ggplot() +
  geom_violin(data = filter(full_data,
                            uncertainty_level == "mid",
                            matrix_size == "size 3",
                            breeding_stages == "multiple",
                            matrix_number == 1), 
              aes(x = prop_scenario, 
                  y = lambda, fill = prop_scenario,
                  colour = prop_scenario),
              scale = "width",
              draw_quantiles = c(0.025,0.975)) +
  scale_fill_manual(values = colours_scenario[1:4]) +
  scale_color_manual(values = c(colours_scenario[1], "black", "white", "black"))+
  labs(x = "",
       y = "Population growth rate",
       title = "Survival dominant") +
  BES_theme() +
  theme(legend.position = "none") +
  ylim(0,2.5)

Figure_1B <- ggplot() +
#  geom_hline(yintercept = 1, linetype = "dashed", colour = "blue") +
  geom_rect(aes(ymin = 1, ymax = 0, xmin = -Inf, xmax = Inf),
            fill = "grey70")+
  geom_violin(data = filter(full_data,
                            uncertainty_level == "mid",
                            matrix_size == "size 3",
                            breeding_stages == "multiple",
                            matrix_number == 1), 
              aes(x = prop_scenario, 
                  y = lambda, fill = prop_scenario,
                  colour = prop_scenario),
              scale = "width",
              draw_quantiles = c(0.025,0.975)) +
  scale_fill_manual(values = colours_scenario[1:4]) +
  scale_color_manual(values = c(colours_scenario[1], "black", "white", "black"))+
  labs(x = "",
       y = "Population growth rate",
       title = "Survival dominant") +
  BES_theme() +
  theme(legend.position = "none") +
  ylim(0,2.5)

ggsave("conf_Figure1A.png", Figure_1A, width = 20, height = 15, units = "cm")
ggsave("conf_Figure1B.png", Figure_1B, width = 20, height = 15, units = "cm")

#### FIGURE 2: Propagation scenarios - matrix 3, multiple breed, mid uncert ####

Figure_2A <- ggplot() +
  geom_violin(data = filter(full_data,
                            uncertainty_level == "mid",
                            matrix_size == "size 3",
                            breeding_stages == "multiple",
                            matrix_number == 3), 
              aes(x = prop_scenario, 
                  y = lambda, fill = prop_scenario,
                  colour = prop_scenario),
              scale = "width",
              draw_quantiles = c(0.025,0.975)) +
  scale_fill_manual(values = colours_scenario[1:4]) +
  scale_color_manual(values = c(colours_scenario[1], "black", "white", "black"))+
  labs(x = "",
       y = "Population growth rate",
       title = "Balanced vital rates") +
  BES_theme() +
  theme(legend.position = "none") +
  ylim(0,2.5)

Figure_2B <- ggplot() +
  #  geom_hline(yintercept = 1, linetype = "dashed", colour = "blue") +
  geom_rect(aes(ymin = 1, ymax = 0, xmin = -Inf, xmax = Inf),
            fill = "grey70")+
  geom_violin(data = filter(full_data,
                            uncertainty_level == "mid",
                            matrix_size == "size 3",
                            breeding_stages == "multiple",
                            matrix_number == 3), 
              aes(x = prop_scenario, 
                  y = lambda, fill = prop_scenario,
                  colour = prop_scenario),
              scale = "width",
              draw_quantiles = c(0.025,0.975)) +
  scale_fill_manual(values = colours_scenario[1:4]) +
  scale_color_manual(values = c(colours_scenario[1], "black", "white", "black"))+
  labs(x = "",
       y = "Population growth rate",
       title = "Balanced vital rates") +
  BES_theme() +
  theme(legend.position = "none") +
  ylim(0,2.5)

ggsave("conf_Figure2A.png", Figure_2A, width = 20, height = 15, units = "cm")
ggsave("conf_Figure2B.png", Figure_2B, width = 20, height = 15, units = "cm")

#### FIGURE 3: Propagation scenarios - matrix 5, multiple breed, mid uncert ####

Figure_3A <- ggplot() +
  geom_violin(data = filter(full_data,
                            uncertainty_level == "mid",
                            matrix_size == "size 3",
                            breeding_stages == "multiple",
                            matrix_number == 5), 
              aes(x = prop_scenario, 
                  y = lambda, fill = prop_scenario,
                  colour = prop_scenario),
              scale = "width",
              draw_quantiles = c(0.025,0.975)) +
  scale_fill_manual(values = colours_scenario[1:4]) +
  scale_color_manual(values = c(colours_scenario[1], "black", "white", "black"))+
  labs(x = "",
       y = "Population growth rate",
       title = "Fecundity dominant") +
  BES_theme() +
  theme(legend.position = "none") +
  ylim(0,2.5)

Figure_3B <- ggplot() +
  #  geom_hline(yintercept = 1, linetype = "dashed", colour = "blue") +
  geom_rect(aes(ymin = 1, ymax = 0, xmin = -Inf, xmax = Inf),
            fill = "grey70")+
  geom_violin(data = filter(full_data,
                            uncertainty_level == "mid",
                            matrix_size == "size 3",
                            breeding_stages == "multiple",
                            matrix_number == 5), 
              aes(x = prop_scenario, 
                  y = lambda, fill = prop_scenario,
                  colour = prop_scenario),
              scale = "width",
              draw_quantiles = c(0.025,0.975)) +
  scale_fill_manual(values = colours_scenario[1:4]) +
  scale_color_manual(values = c(colours_scenario[1], "black", "white", "black"))+
  labs(x = "",
       y = "Population growth rate",
       title = "Fecundity dominant") +
  BES_theme() +
  theme(legend.position = "none") +
  ylim(0,2.5)

ggsave("conf_Figure3A.png", Figure_3A, width = 20, height = 15, units = "cm")
ggsave("conf_Figure3B.png", Figure_3B, width = 20, height = 15, units = "cm")


#### PUBLICATION ####
