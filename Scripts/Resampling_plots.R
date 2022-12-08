# T1.2: Script to create resampled datasets and plot #

# Takes input of mean matrix and uncertainty %

# runs all combinations of uncertainty and produces a combined dataframe of output

################################################################################

## load packages ####

library(tidyverse)
library(popbio)
library(popdemo)

## source scripts ####

source("./Functions/run_resampling_function.R")
source("./Scripts/theme_script.R")

## set up matrices ####

matrix1 <- matrix(c(0.1378489, 3.5, 3.5,
                    0.01347, 0.169622, 0,
                    0.006739, 0.012918, 0.171508), byrow = TRUE, nrow = 3)


matrix2 <- matrix(c(0, 0.58125, 0.678125,
                    0.141, 0, 0,
                    0, 0.80400, 0.831), byrow = TRUE, nrow = 3)


matrix3 <- matrix(c(0.310025, 0.78410, 12,
                    0.179125, 0.54465, 0.1678,
                    0.01850, 0.02875, 0.5747), byrow = TRUE, nrow = 3)

## set up uncertainty ####

uncertainty_fecundity <- c(0.09, 0.22, 0.4)
uncertainty_survival <- c(0.005, 0.13, 0.38)


################################################################################

#### RUN RESAMPLING ####

results_matrix1 <- run_resampling_function(mean_matrix = matrix1,
                                   uncertainty_fecundity = uncertainty_fecundity,
                                   uncertainty_survival = uncertainty_survival,
                                   matrix_name = "matrix1")

results_matrix2 <- run_resampling_function(mean_matrix = matrix2,
                                           uncertainty_fecundity = uncertainty_fecundity,
                                           uncertainty_survival = uncertainty_survival,
                                           matrix_name = "matrix2")

results_matrix3 <- run_resampling_function(mean_matrix = matrix3,
                                           uncertainty_fecundity = uncertainty_fecundity,
                                           uncertainty_survival = uncertainty_survival,
                                           matrix_name = "matrix3")

################################################################################

#### Plot ####

# join datasets

full_results <- bind_rows(results_matrix1, results_matrix2,
                          results_matrix3)

colours <- c("#feff54", "#FFFFFF", "#30666B", "#1E2E39")

################################################################################

#### BES PLOTS ###

# will focus on max uncertainty. 

#### BES FIGURE1: matrix 1 results ####

# re-order the factor levels
plot_results <- full_results %>%
  dplyr::mutate(propagation_type = as.factor(propagation_type),
                propagation_type = plyr::revalue(propagation_type, 
                                           c("f_only"="fecundity \nonly", 
                                             "s_only"="survival \nonly",
                                             "full" = "full propagation",
                                             "mean_matrix" = "mean_matrix")),
                propagation_type = fct_relevel(propagation_type, 
                                     "full propagation",
                                     "fecundity \nonly",
                                     "survival \nonly",
                                     "mean_matrix"))

ggplot(data = filter(plot_results,
                     propagation_type != "mean_matrix",
                     uncertainty_level == "max",
                     matrix_name == "matrix1",
                     in_CI = TRUE), 
       aes(x = propagation_type, 
           y = lambda, fill = propagation_type, 
           colour = propagation_type))+
  geom_hline(aes(yintercept = 1), 
            colour = "white") +
  geom_hline(aes(yintercept = filter(full_results, matrix_name == "matrix1",
                                     propagation_type == "mean_matrix")$lambda), 
             colour = colours[1]) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75),
              scale = "width") +
  #geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = colours[2:4])+
  scale_color_manual(values = c("black", "grey20", "grey70"))+
  #facet_wrap(~uncertainty_level)+
  labs(x = "",
       y = "Lambda") +
  #ylim(c(0.2,1)) +
  BES_theme() +
  theme(legend.position = "none")

# then zoom in

ggplot(data = filter(plot_results,
                     propagation_type != "mean_matrix",
                     uncertainty_level == "max",
                     matrix_name == "matrix1",
                     in_CI = TRUE), 
       aes(x = propagation_type, 
           y = lambda, fill = propagation_type,
           colour = propagation_type))+
  geom_hline(aes(yintercept = 1), 
             colour = "white") +
  geom_hline(aes(yintercept = filter(full_results, matrix_name == "matrix1",
                                     propagation_type == "mean_matrix")$lambda), 
             colour = colours[1]) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75),
              scale = "width") +
  #geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = colours[2:4])+
  scale_color_manual(values = c("black", "grey20", "grey70"))+
  #facet_wrap(~uncertainty_level)+
  labs(x = "",
       y = "Lambda") +
  ylim(c(0.2,1.1)) +
  BES_theme() +
  theme(legend.position = "none")

ggsave("BES_Fig1.png", last_plot(), width = 15, height = 15, units = "cm", 
       dpi = 300)

#### BES FIGURE2: matrix 2 results ####

ggplot(data = filter(plot_results,
                     propagation_type != "mean_matrix",
                     uncertainty_level == "max",
                     matrix_name == "matrix2",
                     in_CI = TRUE), 
       aes(x = propagation_type, 
           y = lambda, fill = propagation_type,
           colour = propagation_type))+
  geom_hline(aes(yintercept = 1), 
             colour = "white") +
  geom_hline(aes(yintercept = filter(full_results, matrix_name == "matrix2",
                                     propagation_type == "mean_matrix")$lambda), 
             colour = colours[1]) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75),
              scale = "width") +
  #geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = colours[2:4])+
  scale_color_manual(values = c("black", "grey20", "grey70"))+
  #facet_wrap(~uncertainty_level)+
  labs(x = "",
       y = "Lambda") +
  #ylim(c(0.2,1)) +
  BES_theme() +
  theme(legend.position = "none")

ggsave("BES_Fig2.png", last_plot(), width = 15, height = 15, units = "cm", 
       dpi = 300)


#### BES FIGURE3: matrix 3 results ####

ggplot(data = filter(plot_results,
                     propagation_type != "mean_matrix",
                     uncertainty_level == "max",
                     matrix_name == "matrix3",
                     in_CI = TRUE), 
       aes(x = propagation_type, 
           y = lambda, fill = propagation_type,
           colour = propagation_type))+
  geom_hline(aes(yintercept = 1), 
             colour = "white") +
  geom_hline(aes(yintercept = filter(full_results, matrix_name == "matrix3",
                                     propagation_type == "mean_matrix")$lambda), 
             colour = colours[1]) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75),
              scale = "width") +
  #geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = colours[2:4])+
  scale_color_manual(values = c("black", "grey20", "grey70"))+
  #facet_wrap(~uncertainty_level)+
  labs(x = "",
       y = "Lambda") +
  #ylim(c(0.2,1)) +
  BES_theme() +
  theme(legend.position = "none")

# and zoom in

ggplot(data = filter(plot_results,
                     propagation_type != "mean_matrix",
                     uncertainty_level == "max",
                     matrix_name == "matrix3",
                     in_CI = TRUE), 
       aes(x = propagation_type, 
           y = lambda, fill = propagation_type,
           colour = propagation_type))+
  geom_hline(aes(yintercept = 1), 
             colour = "white") +
  geom_hline(aes(yintercept = filter(full_results, matrix_name == "matrix3",
                                     propagation_type == "mean_matrix")$lambda), 
             colour = colours[1]) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75),
              scale = "width") +
  #geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = colours[2:4])+
  scale_color_manual(values = c("black", "grey20", "grey70"))+
  #facet_wrap(~uncertainty_level)+
  labs(x = "",
       y = "Lambda") +
  ylim(c(0.6,2)) +
  BES_theme() +
  theme(legend.position = "none")

ggsave("BES_Fig3.png", last_plot(), width = 15, height = 15, units = "cm", 
       dpi = 300)


################################################################################

#### Elasticity summary ####

# % time the elasticity element changed

elasticity_results <- full_results %>% filter(propagation_type != "mean_matrix") %>%
  group_by(propagation_type, matrix_name, uncertainty_level) %>%
  summarise(percentage = sum(elasticity)/1000)

# can probably just look at full

filter(elasticity_results, propagation_type == "full")
