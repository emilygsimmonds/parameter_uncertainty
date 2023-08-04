# T1.2: Script to create conference or publication standard figures #

################################################################################

# load packages
library(tidyverse)
library(ggsankey)
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
full_data[c(seq(1005960, 1005960+299, 1), 
            seq(1005960+299+1005358, 1005960+299+1005358+299, 1),
            seq(1005960+299+1005358+299+1028940, 
                1005960+299+1005358+299+1028940+299, 1)),
          "uncertainty_level"] <-
  rep(rep(rep(c("high", "mid", "low"), each = 10), 5),6)

# then rename factors
full_data <- full_data %>%
  dplyr::mutate(prop_scenario = as.factor(prop_scenario),
                prop_scenario = plyr::revalue(prop_scenario, 
                                              c("f_only"="reproduction \nonly", 
                                                "s_only"="survival \nonly",
                                                "full" = "full",
                                                "none" = "none")),
                prop_scenario = fct_relevel(prop_scenario, 
                                            "none",
                                            "full",
                                            "reproduction \nonly",
                                            "survival \nonly"),
                uncertainty_level = as.factor(uncertainty_level),
                uncertainty_level = fct_relevel(uncertainty_level,
                                                "low",
                                                "mid",
                                                "high"),
                breeding_stages = as.factor(breeding_stages),
                breeding_stages = plyr::revalue(breeding_stages, 
                                              c("one"="reproduce in one stage", 
                                                "multiple"="reproduce in multiple stages"))) %>%
  filter(uncertainty_level != "none")

# load elasticity data
elasticity_2x2 <- read.csv("./Data files/elasticity_results_2x2.csv")
elasticity_3x3 <- read.csv("./Data files/elasticity_results_3x3.csv")
elasticity_5x5 <- read.csv("./Data files/elasticity_results_5x5.csv")

# then join

elasticity_all <- bind_rows(elasticity_2x2,
                            elasticity_3x3,
                            elasticity_5x5) %>%
  mutate(matrix_size = c(rep(c("size 2",
                               "size 3",
                               "size 5"), each = 90))) # add column to indicate matrix size

# then rename factors
elasticity_all <- elasticity_all %>%
  dplyr::mutate(prop_scenario = as.factor(prop_scenario),
                prop_scenario = plyr::revalue(prop_scenario, 
                                              c("f_only"="reproduction \nonly", 
                                                "s_only"="survival \nonly",
                                                "full" = "full")),
                prop_scenario = fct_relevel(prop_scenario, 
                                            "full",
                                            "reproduction \nonly",
                                            "survival \nonly"),
                uncertainty_level = as.factor(uncertainty_level),
                uncertainty_level = fct_relevel(uncertainty_level,
                                                "low",
                                                "mid",
                                                "high"),
                breeding_stages = as.factor(breeding_stages),
                breeding_stages = plyr::revalue(breeding_stages, 
                                                c("one"="reproduce \nin one stage", 
                                                  "multiple"="reproduce \nin multiple stages")))

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
                            breeding_stages == "breed multiple stages",
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
                            breeding_stages == "breed multiple stages",
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
                            breeding_stages == "breed multiple stages",
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
                            breeding_stages == "breed multiple stages",
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
                            breeding_stages == "breed multiple stages",
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
                            breeding_stages == "breed multiple stages",
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
#### FIGURE 1: Pipeline (Sankey plot) of uncertainty omissions ####

# need to create a dataframe of start points and nodes
# then series of stages that take you to final NA

Sankey_data <- data.frame(ID = rep(1:100, 3),
                          stage = rep(c(1,2,3),
                                      each = 100),
                          next_stage = rep(c(2,3,NA),
                                           each = 100),
                          node = c(rep("uncertainty \nreported", 78),
                                   rep("no \nuncertainty", 22),
                                   rep("uncertainty \ncomplete", round(78*0.50)),
                                   rep("uncertainty \nincomplete", round(78*0.50)),
                                   rep(NA, 22),
                                   rep("full \npropagation", round(39*0.825)),
                                   rep("partial \npropagation", round(39*0.025)),
                                   rep("no \npropagation", round(39*0.075)),
                                   rep("can't \ntell", round(39*0.075)),
                                   rep("partial \npropagation", 
                                       round((23*0.39)+(23*0.13))), # this includes those that propagated all they had
                                   rep("no \npropagation", round(23*0.39)), 
                                   rep("can't \ntell", 
                                       round((23*0.087)+16)), # this includes those will all vital rates missing
                                   rep(NA, 22)), # set up all initial nodes
                          next_node = c(rep("uncertainty \ncomplete", round(78*0.50)),
                                        rep("uncertainty \nincomplete", round(78*0.50)),
                                        rep(NA, 22),
                                        rep("full \npropagation", round(39*0.825)),
                                        rep("partial \npropagation", round(39*0.025)),
                                        rep("no \npropagation", round(39*0.075)),
                                        rep("can't \ntell", round(39*0.075)),
                                        rep("partial \npropagation", 
                                            round((23*0.391)+(23*0.13))), # this includes those that propagated all they had
                                        rep("no \npropagation", round(23*0.391)), 
                                        rep("can't \ntell", 
                                            round((23*0.087)+16)), # this includes those will all vital rates missing
                                        rep(NA, 22),
                                        rep(NA, 100)),
                          label = c(rep("yes", 78),
                                    rep("no", 22),
                                    rep("yes", round(78*0.5)),
                                    rep("no", round(78*0.5)),
                                    rep(NA, 22),
                                    rep("full", round(39*0.825)),
                                    rep("partial", round(39*0.025)),
                                    rep("none", round(39*0.075)),
                                    rep("can't \ntell", round(39*0.075)),
                                    rep("partial", 
                                        round((23*0.391)+(23*0.13))), # this includes those that propagated all they had
                                    rep("none", round(23*0.391)), 
                                    rep("can't \ntell", round((23*0.087)+16)), # this includes those will all vital rates missing
                                    rep(NA, 22)), # set up all initial nodes
                          next_label = c(rep("yes", round(78*0.5)),
                                        rep("no", round(78*0.5)),
                                        rep(NA, 22),
                                        rep("full", round(39*0.825)),
                                        rep("partial", round(39*0.025)),
                                        rep("none", round(39*0.075)),
                                        rep("can't \ntell", round(39*0.075)),
                                        rep("partial",
                                            round((23*0.391)+(23*0.13))), # this includes those that propagated all they had
                                        rep("none", round(23*0.391)), 
                                        rep("can't \ntell", round((23*0.087)+16)), # this includes those will all vital rates missing
                                        rep(NA, 22),
                                        rep(NA, 100))) %>% # set up all second nodes 
  dplyr::mutate(node = as.factor(node),
                node = fct_relevel(node,
                                   "no \nuncertainty",
                                   "uncertainty \nreported",
                                   "uncertainty \nincomplete",
                                   "uncertainty \ncomplete", 
                                   "can't \ntell",
                                   "no \npropagation",
                                   "partial \npropagation",
                                   "full \npropagation"),
                next_node = as.factor(next_node),
                next_node = fct_relevel(next_node,
                                        "uncertainty \nincomplete",
                                        "uncertainty \ncomplete", 
                                        "can't \ntell",
                                        "no \npropagation",
                                        "partial \npropagation",
                                        "full \npropagation"),
                label = as.factor(label),
                label = fct_relevel(label,
                                   "no",
                                   "yes",
                                   "can't \ntell",
                                   "none",
                                   "partial",
                                   "full"),
                next_label = as.factor(next_label),
                next_label = fct_relevel(next_label,
                                        "no",
                                        "yes", 
                                        "can't \ntell",
                                        "none",
                                        "partial",
                                        "full"),
                stage = as.factor(stage),
                stage = plyr::revalue(stage,
                                      c("1" = "uncertainty \nreported?",
                                        "2" = "uncertainty \ncomplete?",
                                        "3" = "propagation \ntype")))


# remove NA rows for stages 2 and 3
Sankey_data <- Sankey_data[-c(179:200, 279:300),]

# start with all papers (105) classified into report uncert or not (yes 78.1%, no 21.9%)
# then move them to uncert complete or not and missing (complete 52.4%, not 47.6%)
# then move to uncert propagated or not (complete - full 79.1%, partial 2.3%, none 7%, can't tell 11.6%)
# (incomplete - full 39.1%, partial 13%, none 3.9%, can't tell 8.7% (41% no vital rates))

Figure1_publication <- ggplot(Sankey_data, 
       aes(x = stage,
           next_x = next_stage,
           node = node,
           next_node = next_node,
           fill = factor(node),
           label = label)) +
  geom_sankey(flow.alpha = 0.6, node.color = 1) +
  scale_fill_viridis_d(option = "B", alpha = 0.95) +
  geom_sankey_label(color = 1, fill = "white",
                    hjust = 0.5, alpha = 0.8,
                    size = 5) +
  scale_x_discrete(position = "top") +
  theme_sankey(base_size = 18) +
  theme(legend.position = "none",
        #axis.text.x = element_blank(),
        axis.title.x = element_blank())

Figure1_publication

# 31% have full propagation

# 12% partial, 11% none, 20% can't tell

ggsave("./Figures/pub_Figure1.png", Figure1_publication, width = 20, 
       height = 15, units = "cm")

#### FIGURE 2: Life history and propagation, mid uncertainty, 3x3 matrix ####

# create small dataframe to add letters to each panel
text_numbers <- data.frame(breeding_stages = c(rep("reproduce in multiple stages", 5),
                                               rep("reproduce in one stage", 5)),
                           matrix_number = c(rep(1:5, 2)),
                           text = c("a", "b", "c", "d", "e",
                                    "f", "g", "h", "i", "j"))

Figure2_publication <- ggplot() +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "blue") +
  geom_violin(data = filter(full_data,
                            uncertainty_level == "mid",
                            matrix_size == "size 3"),
                            #matrix_number == 5),
              aes(x = prop_scenario, 
                  y = lambda, fill = prop_scenario,
                  colour = prop_scenario),
              scale = "width") +
  geom_text(data = text_numbers,
            aes(x = 0.6, y = 2.3,
                label = text), size = 4) +
  scale_fill_manual(values = colours_scenario[1:4]) +
  scale_color_manual(values = c(colours_scenario[1], rep("black",3)))+
  labs(x = "",
       y = "Population growth rate",
       title = "Population growth rate estimates under different 
       propagation scenarios") +
  facet_grid(rows = vars(matrix_number),
#  facet_grid(rows = vars(matrix_size),
             cols = vars(breeding_stages)) +
  plain_theme() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90,
                                   vjust = 0.3)) +
  ylim(0,3.5)

Figure2_publication

ggsave("./Figures/pub_Figure2.png", 
       Figure2_publication, width = 16.5, height = 15.5, units = "cm")

#### FIGURE 3: conclusion changes, mid uncertainty, 3x3 matrix ####

# create small dataframe to add letters to each panel
text_numbers <- data.frame(breeding_stages = c(rep("reproduce in multiple stages", 5),
                                               rep("reproduce in one stage", 5)),
                           matrix_number = c(rep(1:5, 2)),
                           text = c("a", "b", "c", "d", "e",
                                    "f", "g", "h", "i", "j"))

# create reference conclusion dataframe
conclusion_reference <- full_data %>%
  filter(prop_scenario == "none",
         uncertainty_level == "mid") %>%
  group_by(matrix_size, breeding_stages, matrix_number, lambda) %>%
  summarise(reference = mean(lambda)) %>%
  mutate(conclusion = case_when(reference > 0.98 & reference < 1.02 ~ "stable",
                                reference > 1.01 ~ "increasing",
                                reference < 0.99 ~ "decreasing")) %>%
  select(-"lambda")

# summarise the number of different conclusion values for each matrix and each
# scenario

conclusion_summary <- full_data %>% left_join(conclusion_reference) %>%
  mutate(lambda_trend = case_when(lambda > 0.98 & lambda < 1.02 ~ "stable",
                                  lambda > 1.01 ~ "increasing",
                                  lambda < 0.99 ~ "decreasing"),
         conclusion_change = lambda_trend == conclusion) %>%
  group_by(matrix_size,
           breeding_stages,
           matrix_number,
           prop_scenario,
           uncertainty_level,
           conclusion_change) %>%
  summarise(count = n()) %>%
  mutate(percentage = round((count/sum(count))*100,2)) %>%
  filter(conclusion_change == TRUE) %>%
  ungroup()

Figure3_publication <- ggplot()+
  geom_col(data = filter(conclusion_summary,
                         uncertainty_level == "mid",
                         matrix_size == "size 3"),
           aes(y = 100-percentage,
               x = prop_scenario,
               colour = prop_scenario,
               fill = prop_scenario))+
  facet_grid(rows = vars(matrix_number),
             cols = vars(breeding_stages))+
  geom_text(data = text_numbers,
            aes(x = 0.6, y = 94,
                label = text)) +
  scale_fill_manual(values = colours_scenario[1:4]) +
  scale_color_manual(values = c(colours_scenario[1], rep("black",3)))+
  labs(x = "",
       y = "Percentage of times conclusion changed",
       title = "Conclusion changes under different propagation scenarios") +
  plain_theme() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90,
                                   vjust = 0.3)) +
  ylim(0,100)

Figure3_publication 

ggsave("./Figures/pub_Figure3.png", Figure3_publication, 
       width = 16.5, height = 15.5, units = "cm")



#### SUPPORTING INFORMATION ####

# rename factors for multiple plotting
full_data <- full_data %>%
  dplyr::mutate(breeding_stages = as.factor(breeding_stages),
                breeding_stages = plyr::revalue(breeding_stages, 
                                                c("reproduce in one stage" = "reproduce \nin one stage", 
                                                  "reproduce in multiple stages" =
                                                    "reproduce \nin multiple stages"))) 

#### PROPAGATION 2x2, Life history and propagation, mid uncertainty ####

# create small dataframe to add letters to each panel
text_numbers <- data.frame(breeding_stages = c(rep("reproduce \nin multiple stages", 5),
                                               rep("reproduce \nin one stage", 5)),
                           matrix_number = c(rep(1:5, 2)),
                           text = c("a", "b", "c", "d", "e",
                                    "f", "g", "h", "i", "j"))

Propagation_2x2_mid <- ggplot() +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "blue") +
  geom_violin(data = filter(full_data,
                            uncertainty_level == "mid",
                            matrix_size == "size 2"),
              #matrix_number == 5),
              aes(x = prop_scenario, 
                  y = lambda, fill = prop_scenario,
                  colour = prop_scenario),
              scale = "width") +
  geom_text(data = text_numbers,
            aes(x = 0.6, y = 2.3,
                label = text)) +
  scale_fill_manual(values = colours_scenario[1:4]) +
  scale_color_manual(values = c(colours_scenario[1], rep("black",3)))+
  labs(x = "",
       y = "Population growth rate",
       title = "Uncertainty in population growth rate (2x2 matrix)") +
  facet_grid(rows = vars(matrix_number),
             #  facet_grid(rows = vars(matrix_size),
             cols = vars(breeding_stages)) +
  plain_theme() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90)) +
  ylim(0,3.5)

Propagation_2x2_mid

ggsave("./Figures/SOM_Propagation_2x2_mid.png", 
       Propagation_2x2_mid, width = 19, height = 20, units = "cm")

#### PROPAGATION 5x5, Life history and propagation, mid uncertainty ####

# create small dataframe to add letters to each panel
text_numbers <- data.frame(breeding_stages = c(rep("reproduce \nin multiple stages", 5),
                                               rep("reproduce \nin one stage", 5)),
                           matrix_number = c(rep(1:5, 2)),
                           text = c("a", "b", "c", "d", "e",
                                    "f", "g", "h", "i", "j"))

Propagation_5x5_mid <- ggplot() +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "blue") +
  geom_violin(data = filter(full_data,
                            uncertainty_level == "mid",
                            matrix_size == "size 5"),
              #matrix_number == 5),
              aes(x = prop_scenario, 
                  y = lambda, fill = prop_scenario,
                  colour = prop_scenario),
              scale = "width") +
  geom_text(data = text_numbers,
            aes(x = 0.6, y = 3,
                label = text)) +
  scale_fill_manual(values = colours_scenario[1:4]) +
  scale_color_manual(values = c(colours_scenario[1], rep("black",3)))+
  labs(x = "",
       y = "Population growth rate",
       title = "Uncertainty in population growth rate (5x5 matrix)") +
  facet_grid(rows = vars(matrix_number),
             #  facet_grid(rows = vars(matrix_size),
             cols = vars(breeding_stages)) +
  plain_theme() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 0)) +
  ylim(0,3.5)

Propagation_5x5_mid

ggsave("./Figures/SOM_Propagation_5x5_mid.png", 
       Propagation_5x5_mid, width = 19, height = 20, units = "cm")


#### CONCLUSION CHANGES 2x2 AND 5x5, mid uncertainty ####

# create small dataframe to add letters to each panel
text_numbers <- data.frame(breeding_stages = c(rep("reproduce \nin multiple stages", 5),
                                               rep("reproduce \nin one stage", 5)),
                           matrix_number = c(rep(1:5, 2)),
                           text = c("a", "b", "c", "d", "e",
                                    "f", "g", "h", "i", "j"))

# create reference conclusion dataframe
conclusion_reference <- full_data %>%
  filter(prop_scenario == "none",
         uncertainty_level == "mid") %>%
  group_by(matrix_size, breeding_stages, matrix_number, lambda) %>%
  summarise(reference = mean(lambda)) %>%
  mutate(conclusion = case_when(reference > 0.98 & reference < 1.02 ~ "stable",
                                reference > 1.01 ~ "increasing",
                                reference < 0.99 ~ "decreasing")) %>%
  select(-"lambda")

# summarise the number of different conclusion values for each matrix and each
# scenario

conclusion_summary <- full_data %>% left_join(conclusion_reference) %>%
  mutate(lambda_trend = case_when(lambda > 0.98 & lambda < 1.02 ~ "stable",
                                  lambda > 1.01 ~ "increasing",
                                  lambda < 0.99 ~ "decreasing"),
         conclusion_change = lambda_trend == conclusion) %>%
  group_by(matrix_size,
           breeding_stages,
           matrix_number,
           prop_scenario,
           uncertainty_level,
           conclusion_change) %>%
  summarise(count = n()) %>%
  mutate(percentage = round((count/sum(count))*100,2)) %>%
  filter(conclusion_change == TRUE) %>%
  ungroup()

Conclusion_2x2_mid <- ggplot()+
  geom_col(data = filter(conclusion_summary,
                         uncertainty_level == "mid",
                         matrix_size == "size 2"),
           aes(y = 100-percentage,
               x = prop_scenario,
               colour = prop_scenario,
               fill = prop_scenario))+
  facet_grid(rows = vars(matrix_number),
             cols = vars(breeding_stages))+
  geom_text(data = text_numbers,
            aes(x = 0.6, y = 94,
                label = text)) +
  scale_fill_manual(values = colours_scenario[1:4]) +
  scale_color_manual(values = c(colours_scenario[1], rep("black",3)))+
  labs(x = "",
       y = "Percentage of times conclusion changed",
       title = "Conclusion changes (2x2 matrix) under different propagation scenarios") +
  plain_theme() +
  theme(legend.position = "none") +
  ylim(0,100)

Conclusion_2x2_mid 

ggsave("./Figures/SOM_Conclusion_2x2_mid.png", Conclusion_2x2_mid,
       width = 19, height = 20, 
       units = "cm")

Conclusion_5x5_mid <- ggplot()+
  geom_col(data = filter(conclusion_summary,
                         uncertainty_level == "mid",
                         matrix_size == "size 5"),
           aes(y = 100-percentage,
               x = prop_scenario,
               colour = prop_scenario,
               fill = prop_scenario))+
  facet_grid(rows = vars(matrix_number),
             cols = vars(breeding_stages))+
  geom_text(data = text_numbers,
            aes(x = 0.6, y = 94,
                label = text)) +
  scale_fill_manual(values = colours_scenario[1:4]) +
  scale_color_manual(values = c(colours_scenario[1], rep("black",3)))+
  labs(x = "",
       y = "Percentage of times conclusion changed",
       title = "Conclusion changes (5x5 matrix) under different propagation scenarios") +
  plain_theme() +
  theme(legend.position = "none") +
  ylim(0,100)

Conclusion_5x5_mid 

ggsave("./Figures/SOM_Conclusion_5x5_mid.png", Conclusion_5x5_mid,
       width = 19, height = 20, 
       units = "cm")

#### PROPAGATION ALL SIZES, Life history and propagation, LOW uncertainty ####

Propagation_2x2_low <- ggplot() +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "blue") +
  geom_violin(data = filter(full_data,
                            uncertainty_level == "low",
                            matrix_size == "size 2"),
              aes(x = prop_scenario, 
                  y = lambda, fill = prop_scenario,
                  colour = prop_scenario),
              scale = "width") +
  scale_fill_manual(values = colours_scenario[1:4]) +
  scale_color_manual(values = c(colours_scenario[1], rep("black",3)))+
  labs(x = "",
       y = "Population growth rate",
       title = "2x2 matrix") +
  facet_grid(rows = vars(matrix_number),
             cols = vars(breeding_stages)) +
  plain_theme() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90),
        strip.text.y = element_blank()) +
  ylim(0,3.5)

Propagation_3x3_low <- ggplot() +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "blue") +
  geom_violin(data = filter(full_data,
                            uncertainty_level == "low",
                            matrix_size == "size 3"),
              aes(x = prop_scenario, 
                  y = lambda, fill = prop_scenario,
                  colour = prop_scenario),
              scale = "width") +
  scale_fill_manual(values = colours_scenario[1:4]) +
  scale_color_manual(values = c(colours_scenario[1], rep("black",3)))+
  labs(x = "",
       y = "",
       title = "3x3 matrix") +
  facet_grid(rows = vars(matrix_number),
             cols = vars(breeding_stages)) +
  plain_theme() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90),
        strip.text.y = element_blank(),
        axis.text.y = element_blank()) +
  ylim(0,3.5)

Propagation_5x5_low <- ggplot() +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "blue") +
  geom_violin(data = filter(full_data,
                            uncertainty_level == "low",
                            matrix_size == "size 5"),
              aes(x = prop_scenario, 
                  y = lambda, fill = prop_scenario,
                  colour = prop_scenario),
              scale = "width") +
  scale_fill_manual(values = colours_scenario[1:4]) +
  scale_color_manual(values = c(colours_scenario[1], rep("black",3)))+
  labs(x = "",
       y = "",
       title = "5x5 matrix") +
  facet_grid(rows = vars(matrix_number),
             cols = vars(breeding_stages)) +
  plain_theme() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90),
        axis.text.y = element_blank()) +
  ylim(0,3.5)

Propagation_2x2_low +
Propagation_3x3_low +
Propagation_5x5_low +
  plot_annotation(title = 
                    "Uncertainty in population growth rate 
                  (low-level uncertainty) under different propagation scenarios")

ggsave("./Figures/SOM_Propagation_low_ALL.png", 
       last_plot(), width = 25, height = 20, units = "cm")


#### PROPAGATION ALL SIZES, Life history and propagation, HIGH uncertainty ####

Propagation_2x2_high <- ggplot() +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "blue") +
  geom_violin(data = filter(full_data,
                            uncertainty_level == "high",
                            matrix_size == "size 2"),
              aes(x = prop_scenario, 
                  y = lambda, fill = prop_scenario,
                  colour = prop_scenario),
              scale = "width") +
  scale_fill_manual(values = colours_scenario[1:4]) +
  scale_color_manual(values = c(colours_scenario[1], rep("black",3)))+
  labs(x = "",
       y = "Population growth rate",
       title = "2x2 matrix") +
  facet_grid(rows = vars(matrix_number),
             cols = vars(breeding_stages)) +
  plain_theme() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90),
        strip.text.y = element_blank()) +
  ylim(0,10)

Propagation_3x3_high <- ggplot() +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "blue") +
  geom_violin(data = filter(full_data,
                            uncertainty_level == "high",
                            matrix_size == "size 3"),
              aes(x = prop_scenario, 
                  y = lambda, fill = prop_scenario,
                  colour = prop_scenario),
              scale = "width") +
  scale_fill_manual(values = colours_scenario[1:4]) +
  scale_color_manual(values = c(colours_scenario[1], rep("black",3)))+
  labs(x = "",
       y = "",
       title = "3x3 matrix") +
  facet_grid(rows = vars(matrix_number),
             cols = vars(breeding_stages)) +
  plain_theme() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90),
        strip.text.y = element_blank(),
        axis.text.y = element_blank()) +
  ylim(0,10)

Propagation_5x5_high <- ggplot() +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "blue") +
  geom_violin(data = filter(full_data,
                            uncertainty_level == "high",
                            matrix_size == "size 5"),
              aes(x = prop_scenario, 
                  y = lambda, fill = prop_scenario,
                  colour = prop_scenario),
              scale = "width") +
  scale_fill_manual(values = colours_scenario[1:4]) +
  scale_color_manual(values = c(colours_scenario[1], rep("black",3)))+
  labs(x = "",
       y = "",
       title = "5x5 matrix") +
  facet_grid(rows = vars(matrix_number),
             cols = vars(breeding_stages)) +
  plain_theme() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90),
        axis.text.y = element_blank()) +
  ylim(0,10)

Propagation_2x2_high +
  Propagation_3x3_high +
  Propagation_5x5_high +
  plot_annotation(title = "Uncertainty in population growth rate (high-level uncertainty) 
                  under different propagation scenarios")

ggsave("./Figures/SOM_Propagation_high_ALL.png", 
       last_plot(), width = 25, height = 20, units = "cm")

#### CONCLUSION CHANGES ALL, low uncertainty ####

# create reference conclusion dataframe
conclusion_reference <- full_data %>%
  filter(prop_scenario == "none",
         uncertainty_level == "low") %>%
  group_by(matrix_size, breeding_stages, matrix_number, lambda) %>%
  summarise(reference = mean(lambda)) %>%
  mutate(conclusion = case_when(reference > 0.98 & reference < 1.02 ~ "stable",
                                reference > 1.01 ~ "increasing",
                                reference < 0.99 ~ "decreasing")) %>%
  select(-"lambda")

# summarise the number of different conclusion values for each matrix and each
# scenario

conclusion_summary <- full_data %>% left_join(conclusion_reference) %>%
  mutate(lambda_trend = case_when(lambda > 0.98 & lambda < 1.02 ~ "stable",
                                  lambda > 1.01 ~ "increasing",
                                  lambda < 0.99 ~ "decreasing"),
         conclusion_change = lambda_trend == conclusion) %>%
  group_by(matrix_size,
           breeding_stages,
           matrix_number,
           prop_scenario,
           uncertainty_level,
           conclusion_change) %>%
  summarise(count = n()) %>%
  mutate(percentage = round((count/sum(count))*100,2)) %>%
  filter(conclusion_change == TRUE) %>%
  ungroup()

Conclusion_2x2_low <- ggplot()+
  geom_col(data = filter(conclusion_summary,
                         uncertainty_level == "low",
                         matrix_size == "size 2"),
           aes(y = 100-percentage,
               x = prop_scenario,
               colour = prop_scenario,
               fill = prop_scenario))+
  facet_grid(rows = vars(matrix_number),
             cols = vars(breeding_stages))+
  scale_fill_manual(values = colours_scenario[1:4]) +
  scale_color_manual(values = c(colours_scenario[1], rep("black",3)))+
  labs(x = "",
       y = "Percentage of times conclusion changed",
       title = "Conclusion changes (2x2 matrix) under different propagation scenarios") +
  plain_theme() +
  theme(legend.position = "none") +
  ylim(0,100)

Conclusion_2x2_low 

ggsave("./Figures/SOM_Conclusion_2x2_low.png", Conclusion_2x2_low,
       width = 19, height = 20, 
       units = "cm")

Conclusion_3x3_low <- ggplot()+
  geom_col(data = filter(conclusion_summary,
                         uncertainty_level == "low",
                         matrix_size == "size 3"),
           aes(y = 100-percentage,
               x = prop_scenario,
               colour = prop_scenario,
               fill = prop_scenario))+
  facet_grid(rows = vars(matrix_number),
             cols = vars(breeding_stages))+
  scale_fill_manual(values = colours_scenario[1:4]) +
  scale_color_manual(values = c(colours_scenario[1], rep("black",3)))+
  labs(x = "",
       y = "Percentage of times conclusion changed",
       title = "Conclusion changes (3x3 matrix) under different propagation scenarios") +
  plain_theme() +
  theme(legend.position = "none") +
  ylim(0,100)

Conclusion_3x3_low 

ggsave("./Figures/SOM_Conclusion_3x3_low.png", Conclusion_3x3_low,
       width = 19, height = 20, 
       units = "cm")


Conclusion_5x5_low <- ggplot()+
  geom_col(data = filter(conclusion_summary,
                         uncertainty_level == "low",
                         matrix_size == "size 5"),
           aes(y = 100-percentage,
               x = prop_scenario,
               colour = prop_scenario,
               fill = prop_scenario))+
  facet_grid(rows = vars(matrix_number),
             cols = vars(breeding_stages))+
  scale_fill_manual(values = colours_scenario[1:4]) +
  scale_color_manual(values = c(colours_scenario[1], rep("black",3)))+
  labs(x = "",
       y = "Percentage of times conclusion changed",
       title = "Conclusion changes (5x5 matrix) under different propagation scenarios") +
  plain_theme() +
  theme(legend.position = "none") +
  ylim(0,100)

Conclusion_5x5_low

ggsave("./Figures/SOM_Conclusion_5x5_low.png", Conclusion_5x5_low,
       width = 19, height = 20, 
       units = "cm")

#### CONCLUSION CHANGES ALL, high uncertainty ####

# create reference conclusion dataframe
conclusion_reference <- full_data %>%
  filter(prop_scenario == "none",
         uncertainty_level == "high") %>%
  group_by(matrix_size, breeding_stages, matrix_number, lambda) %>%
  summarise(reference = mean(lambda)) %>%
  mutate(conclusion = case_when(reference > 0.98 & reference < 1.02 ~ "stable",
                                reference > 1.01 ~ "increasing",
                                reference < 0.99 ~ "decreasing")) %>%
  select(-"lambda")

# summarise the number of different conclusion values for each matrix and each
# scenario

conclusion_summary <- full_data %>% left_join(conclusion_reference) %>%
  mutate(lambda_trend = case_when(lambda > 0.98 & lambda < 1.02 ~ "stable",
                                  lambda > 1.01 ~ "increasing",
                                  lambda < 0.99 ~ "decreasing"),
         conclusion_change = lambda_trend == conclusion) %>%
  group_by(matrix_size,
           breeding_stages,
           matrix_number,
           prop_scenario,
           uncertainty_level,
           conclusion_change) %>%
  summarise(count = n()) %>%
  mutate(percentage = round((count/sum(count))*100,2)) %>%
  filter(conclusion_change == TRUE) %>%
  ungroup()

Conclusion_2x2_high <- ggplot()+
  geom_col(data = filter(conclusion_summary,
                         uncertainty_level == "high",
                         matrix_size == "size 2"),
           aes(y = 100-percentage,
               x = prop_scenario,
               colour = prop_scenario,
               fill = prop_scenario))+
  facet_grid(rows = vars(matrix_number),
             cols = vars(breeding_stages))+
  scale_fill_manual(values = colours_scenario[1:4]) +
  scale_color_manual(values = c(colours_scenario[1], rep("black",3)))+
  labs(x = "",
       y = "Percentage of times conclusion changed",
       title = "Conclusion changes (2x2 matrix) under different propagation scenarios") +
  plain_theme() +
  theme(legend.position = "none") +
  ylim(0,100)

Conclusion_2x2_high 

ggsave("./Figures/SOM_Conclusion_2x2_high.png", Conclusion_2x2_high,
       width = 19, height = 20, 
       units = "cm")

Conclusion_3x3_high <- ggplot()+
  geom_col(data = filter(conclusion_summary,
                         uncertainty_level == "high",
                         matrix_size == "size 3"),
           aes(y = 100-percentage,
               x = prop_scenario,
               colour = prop_scenario,
               fill = prop_scenario))+
  facet_grid(rows = vars(matrix_number),
             cols = vars(breeding_stages))+
  scale_fill_manual(values = colours_scenario[1:4]) +
  scale_color_manual(values = c(colours_scenario[1], rep("black",3)))+
  labs(x = "",
       y = "Percentage of times conclusion changed",
       title = "Conclusion changes (3x3 matrix) under different propagation scenarios") +
  plain_theme() +
  theme(legend.position = "none") +
  ylim(0,100)

Conclusion_3x3_high 

ggsave("./Figures/SOM_Conclusion_3x3_high.png", Conclusion_3x3_high,
       width = 19, height = 20, 
       units = "cm")

Conclusion_5x5_high <- ggplot()+
  geom_col(data = filter(conclusion_summary,
                         uncertainty_level == "high",
                         matrix_size == "size 5"),
           aes(y = 100-percentage,
               x = prop_scenario,
               colour = prop_scenario,
               fill = prop_scenario))+
  facet_grid(rows = vars(matrix_number),
             cols = vars(breeding_stages))+
  scale_fill_manual(values = colours_scenario[1:4]) +
  scale_color_manual(values = c(colours_scenario[1], rep("black",3)))+
  labs(x = "",
       y = "Percentage of times conclusion changed",
       title = "Conclusion changes (5x5 matrix) under different propagation scenarios") +
  plain_theme() +
  theme(legend.position = "none") +
  ylim(0,100)

Conclusion_5x5_high

ggsave("./Figures/SOM_Conclusion_5x5_high.png", Conclusion_5x5_high,
       width = 19, height = 20, 
       units = "cm")

#### ELASTICITY PLOTS ####

### LOW UNCERTAINTY ###

# create a plot of the different elasticity values
Elasticity_2x2_low <- ggplot()+
  geom_col(data = filter(elasticity_all,
                         uncertainty_level == "low",
                         matrix_size == "size 2"),
           aes(y = percentage*100,
               x = prop_scenario,
               colour = prop_scenario,
               fill = prop_scenario))+
  facet_grid(rows = vars(matrix_number),
             cols = vars(breeding_stages))+
  scale_fill_manual(values = colours_scenario[2:4]) +
  scale_color_manual(values = c(rep("black",3)))+
  labs(x = "",
       y = "Percentage of times conclusion changed",
       title = "2x2 matrix") +
  plain_theme() +
  theme(legend.position = "none",
        strip.text.y = element_blank(),
        axis.text.x = element_text(angle = 90)) +
  ylim(0,100)

Elasticity_2x2_low 

# create a plot of the different elasticity values
Elasticity_3x3_low <- ggplot()+
  geom_col(data = filter(elasticity_all,
                         uncertainty_level == "low",
                         matrix_size == "size 3"),
           aes(y = percentage*100,
               x = prop_scenario,
               colour = prop_scenario,
               fill = prop_scenario))+
  facet_grid(rows = vars(matrix_number),
             cols = vars(breeding_stages))+
  scale_fill_manual(values = colours_scenario[2:4]) +
  scale_color_manual(values = c(rep("black",3)))+
  labs(x = "",
       y = "",
       title = "3x3 matrix") +
  plain_theme() +
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        strip.text.y = element_blank(),
        axis.text.x = element_text(angle = 90)) +
  ylim(0,100)

Elasticity_3x3_low 

# create a plot of the different elasticity values
Elasticity_5x5_low <- ggplot()+
  geom_col(data = filter(elasticity_all,
                         uncertainty_level == "low",
                         matrix_size == "size 5"),
           aes(y = percentage*100,
               x = prop_scenario,
               colour = prop_scenario,
               fill = prop_scenario))+
  facet_grid(rows = vars(matrix_number),
             cols = vars(breeding_stages))+
  scale_fill_manual(values = colours_scenario[2:4]) +
  scale_color_manual(values = c(rep("black",3)))+
  labs(x = "",
       y = "",
       title = "5x5 matrix") +
  plain_theme() +
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        axis.text.x = element_text(angle = 90)) +
  ylim(0,100)

Elasticity_5x5_low 

Elasticity_2x2_low + Elasticity_3x3_low + Elasticity_5x5_low +
  plot_annotation(title = "Dominant elasticity changes (low-level uncertainty) 
                  under different propagation scenarios")

ggsave("./Figures/SOM_Elasticity_low.png", last_plot(),
       width = 25, height = 20, 
       units = "cm")

### MID UNCERTAINTY ###

# create a plot of the different elasticity values
Elasticity_2x2_mid <- ggplot()+
  geom_col(data = filter(elasticity_all,
                         uncertainty_level == "mid",
                         matrix_size == "size 2"),
           aes(y = percentage*100,
               x = prop_scenario,
               colour = prop_scenario,
               fill = prop_scenario))+
  facet_grid(rows = vars(matrix_number),
             cols = vars(breeding_stages))+
  scale_fill_manual(values = colours_scenario[2:4]) +
  scale_color_manual(values = c(rep("black",3)))+
  labs(x = "",
       y = "Percentage of times conclusion changed",
       title = "2x2 matrix") +
  plain_theme() +
  theme(legend.position = "none",
        strip.text.y = element_blank(),
        axis.text.x = element_text(angle = 90)) +
  ylim(0,100)

Elasticity_2x2_mid 

# create a plot of the different elasticity values
Elasticity_3x3_mid <- ggplot()+
  geom_col(data = filter(elasticity_all,
                         uncertainty_level == "mid",
                         matrix_size == "size 3"),
           aes(y = percentage*100,
               x = prop_scenario,
               colour = prop_scenario,
               fill = prop_scenario))+
  facet_grid(rows = vars(matrix_number),
             cols = vars(breeding_stages))+
  scale_fill_manual(values = colours_scenario[2:4]) +
  scale_color_manual(values = c(rep("black",3)))+
  labs(x = "",
       y = "",
       title = "3x3 matrix") +
  plain_theme() +
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        strip.text.y = element_blank(),
        axis.text.x = element_text(angle = 90)) +
  ylim(0,100)

Elasticity_3x3_mid 

# create a plot of the different elasticity values
Elasticity_5x5_mid <- ggplot()+
  geom_col(data = filter(elasticity_all,
                         uncertainty_level == "mid",
                         matrix_size == "size 5"),
           aes(y = percentage*100,
               x = prop_scenario,
               colour = prop_scenario,
               fill = prop_scenario))+
  facet_grid(rows = vars(matrix_number),
             cols = vars(breeding_stages))+
  scale_fill_manual(values = colours_scenario[2:4]) +
  scale_color_manual(values = c(rep("black",3)))+
  labs(x = "",
       y = "",
       title = "5x5 matrix") +
  plain_theme() +
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        axis.text.x = element_text(angle = 90)) +
  ylim(0,100)

Elasticity_5x5_mid 

Elasticity_2x2_mid + Elasticity_3x3_mid + Elasticity_5x5_mid +
  plot_annotation(title = "Dominant elasticity changes (mid-level uncertainty)
                   under different propagation scenarios")

ggsave("./Figures/SOM_Elasticity_mid.png", last_plot(),
       width = 25, height = 20, 
       units = "cm")

### HIGH UNCERTAINTY ###

# create a plot of the different elasticity values
Elasticity_2x2_high <- ggplot()+
  geom_col(data = filter(elasticity_all,
                         uncertainty_level == "high",
                         matrix_size == "size 2"),
           aes(y = percentage*100,
               x = prop_scenario,
               colour = prop_scenario,
               fill = prop_scenario))+
  facet_grid(rows = vars(matrix_number),
             cols = vars(breeding_stages))+
  scale_fill_manual(values = colours_scenario[2:4]) +
  scale_color_manual(values = c(rep("black",3)))+
  labs(x = "",
       y = "Percentage of times conclusion changed",
       title = "2x2 matrix") +
  plain_theme() +
  theme(legend.position = "none",
        strip.text.y = element_blank(),
        axis.text.x = element_text(angle = 90)) +
  ylim(0,100)

Elasticity_2x2_high 

# create a plot of the different elasticity values
Elasticity_3x3_high <- ggplot()+
  geom_col(data = filter(elasticity_all,
                         uncertainty_level == "high",
                         matrix_size == "size 3"),
           aes(y = percentage*100,
               x = prop_scenario,
               colour = prop_scenario,
               fill = prop_scenario))+
  facet_grid(rows = vars(matrix_number),
             cols = vars(breeding_stages))+
  scale_fill_manual(values = colours_scenario[2:4]) +
  scale_color_manual(values = c(rep("black",3)))+
  labs(x = "",
       y = "",
       title = "3x3 matrix") +
  plain_theme() +
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        strip.text.y = element_blank(),
        axis.text.x = element_text(angle = 90)) +
  ylim(0,100)

Elasticity_3x3_high 

# create a plot of the different elasticity values
Elasticity_5x5_high <- ggplot()+
  geom_col(data = filter(elasticity_all,
                         uncertainty_level == "high",
                         matrix_size == "size 5"),
           aes(y = percentage*100,
               x = prop_scenario,
               colour = prop_scenario,
               fill = prop_scenario))+
  facet_grid(rows = vars(matrix_number),
             cols = vars(breeding_stages))+
  scale_fill_manual(values = colours_scenario[2:4]) +
  scale_color_manual(values = c(rep("black",3)))+
  labs(x = "",
       y = "",
       title = "5x5 matrix") +
  plain_theme() +
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        axis.text.x = element_text(angle = 90)) +
  ylim(0,100)

Elasticity_5x5_high 

Elasticity_2x2_high + Elasticity_3x3_high + Elasticity_5x5_high +
  plot_annotation(title = "Dominant elasticity changes (high-level uncertainty)
                   under different propagation scenarios")

ggsave("./Figures/SOM_Elasticity_high.png", last_plot(),
       width = 25, height = 20, 
       units = "cm")

