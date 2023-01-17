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

uncertainty_fecundity <- c(0.01, 0.122, 2.05)
uncertainty_survival <- c(0.006, 0.167, 1.58)


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

save(full_results, file = "full_results_resampling.RData")

load("full_results_resampling.RData")

colours <- c("#feff54","#FFFFFF", "#30666B", "#1E2E39")

################################################################################

#### BES PLOTS ###

# will focus on median uncertainty. 

# re-order the factor levels
plot_results <- full_results %>%
  dplyr::mutate(propagation_type = as.factor(propagation_type),
                propagation_type = plyr::revalue(propagation_type, 
                                           c("f_only"="fecundity \nonly", 
                                             "s_only"="survival \nonly",
                                             "full" = "full \npropagation",
                                             "mean_matrix" = "no \npropagation")),
                propagation_type = fct_relevel(propagation_type, 
                                     "no \npropagation",
                                     "full \npropagation",
                                     "fecundity \nonly",
                                     "survival \nonly"))


#### DISTRIBUTION EXAMPLES ####

samples <- data.frame(samples = dbeta(seq(0, 1, length.out = 100),
                 shape1 = 1.5, shape2 = 3),
                 x = seq(0, 1, length.out = 100))

ggplot(data = samples, aes(x = x,
           y = samples))+
  geom_line(colour = "yellow") +
  labs(y = "density",
       x = "probability") +
  BES_theme()


ggsave("BES_beta.png", last_plot(), width = 10, height = 10, units = "cm", 
       dpi = 300)

logvar <- log(((3.5*0.22)^2)/(3.5^2)+1)

samples <- data.frame(samples = dlnorm(seq(1, 10, length.out = 1000),
                                      meanlog = log(3.5), sdlog=logvar),
                      x = seq(0, 10, length.out = 1000))

ggplot(data = samples, aes(x = x,
                           y = samples))+
  geom_line(colour = "yellow") +
  labs(y = "density",
       x = "lambda") +
  BES_theme()


ggsave("BES_lnorm.png", last_plot(), width = 10, height = 10, units = "cm", 
       dpi = 300)

# how many of the full propagation results are different direction to mean?

reference <- plot_results$lambda[which(plot_results$matrix_name == "matrix1" &
                                         plot_results$propagation_type == "no \npropagation")] >= 1

filter(plot_results, matrix_name == "matrix1",
       uncertainty_level == "mean") %>%
  mutate(checker = lambda >= 1,
         checker2 = checker == reference) %>%
  group_by(propagation_type, checker2) %>%
  summarise(count = (n()/1000)*100)

#### BES FIGURE1: matrix 1: Fecundity biased results ####

Figure_1_data <- filter(plot_results,
                        uncertainty_level == "mean" |
                          is.na(uncertainty_level),
                        matrix_name == "matrix1") %>%
  bind_rows(filter(plot_results,
                   propagation_type == "no \npropagation",
                   matrix_name == "matrix1"), filter(plot_results,
                                       propagation_type == "no \npropagation",
                                       matrix_name == "matrix1"))

Figure_1A <- ggplot() +
#  geom_hline(yintercept = c(quantile(filter(Figure_1_data,
#                                            propagation_type == "full \npropagation")$lambda,
#                                     probs = c(0.25, 0.5, 0.75))),
#             colour = "grey50") + 
  geom_hline(yintercept = 1, linetype = "dashed", colour = "white") +
  geom_violin(data = Figure_1_data, 
              aes(x = propagation_type, 
                  y = lambda, fill = propagation_type, 
                  colour = propagation_type),
              draw_quantiles = c(0.025, 0.5, 0.975),
              scale = "width") +
  scale_fill_manual(values = c(colours[1], "transparent",
                    "transparent", "transparent")) +
  scale_color_manual(values = c(colours[1], "transparent", 
                                "transparent", "transparent"))+
  labs(x = "",
       y = "Population growth rate") +
  ylim(c(0.3,1.1)) +
  BES_theme() +
  theme(legend.position = "none")
  
Figure_1B <- ggplot() +
  geom_hline(yintercept = c(quantile(filter(Figure_1_data,
                                            propagation_type == "full \npropagation")$lambda,
                                     probs = c(0.025, 0.5, 0.975))),
             colour = "grey50", size = 2) + 
  geom_hline(yintercept = 1, linetype = "dashed", colour = "white") +
  geom_violin(data = Figure_1_data, 
              aes(x = propagation_type, 
                  y = lambda, fill = propagation_type, 
                  colour = propagation_type),
              draw_quantiles = c(0.025, 0.5, 0.975),
              scale = "width") +
  scale_fill_manual(values = colours[1:4]) +
  scale_color_manual(values = c(colours[1],"black",  "white", "white"))+
  labs(x = "",
       y = "Population growth rate") +
  ylim(c(0.3,1.1)) +
  BES_theme() +
  theme(legend.position = "none")

Figure_1A

ggsave("BES_Fig1A.png", last_plot(), width = 15, height = 15, units = "cm", 
       dpi = 300)

Figure_1B

ggsave("BES_Fig1B.png", last_plot(), width = 15, height = 15, units = "cm", 
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
