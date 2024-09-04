# Intro -----------------------------

"
This script makes some intuition tables for the CF section of the paper
"

# Imports ------------------

library(randomForest)
library(tidyverse)

output_path <- "C:/Users/nickm/OneDrive/Acer (new laptop)/Documents/PhD/Tulane University/Projects/Charter School Heterogeneity/Charter_School_Heterogeneity_Project/analysis/output/figures"


# Example of Random Forest smoothing ----------------------

# Generate some example data
set.seed(123)
X <- seq(0, 10, length.out = 100)
Y <- sin(X) + rnorm(100, sd = 0.2)

# Create a data frame
df <- data.frame(X = X, Y = Y)

# Train a random forest with 10 trees
rf <- randomForest(Y ~ X, data = df, ntree = 1000)

plot <- ggplot(df, aes(x = X, y = Y)) +
  geom_point(color = "black", size = 2, aes(shape = "Observed Data")) +
  geom_line(aes(y = sin(X), color = "Actual Sine Wave"), size = 1.0) +
  labs(x = "X", y = "Y") +
  theme_classic()

# Add the first few trees in the random forest
tree_predictions <- predict(rf, df, type = "response", predict.all = TRUE)$individual

for (i in 1:5) {
  plot <- plot + geom_line(aes(y = tree_predictions[, i], color = "Tree Predictions"), size = 0.5)
}

# Add the average prediction of all trees
plot <- plot + geom_line(aes(y = predict(rf, df), color = "Forest Prediction"), size = 1.5)

# Customize colors and labels for the legend
plot <- plot + 
  scale_color_manual(values = c("Actual Sine Wave" = "black", "Tree Predictions" = "lightblue", "Forest Prediction" = "blue")) +
  labs(color = "Legend", shape = "")

# Add the average prediction of all trees
plot <- plot + geom_line(aes(y = predict(rf, df)), color = "blue", size = 1.5)

ggsave(filename = file.path(output_path, "rf_smoothing.png"), plot = plot, 
       width = 8, height = 6, dpi = 300)


# Number of parameters growing rapidly with covariates ---------------------

calculate_parameters <- function(p) {
  # Basic terms: 1 intercept + 1 treatment effect + p main effects
  basic_terms <- 1 + 1 + p
  # Treatment-covariate interactions: p terms
  treatment_covariate_interactions <- p
  
  # Sum of covariate interactions and treatment-covariate interactions
  interaction_terms <- 0
  for (k in 2:p) {
    interaction_terms <- interaction_terms + choose(p, k) + choose(p, k)
  }
  
  total_parameters <- basic_terms + treatment_covariate_interactions + interaction_terms
  return(total_parameters)
}

# Generate data for the plot
max_p <- 10
data <- data.frame(
  p = 1:max_p,
  parameters = sapply(1:max_p, calculate_parameters)
)

# Create the plot
plot <- ggplot(data, aes(x = p, y = parameters)) +
  geom_line() +
  geom_point() +
  labs(
    x = "Number of Variables in X",
    y = "Number of Parameters"
  ) +
  scale_x_continuous(breaks = 1:max_p) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "grey", size = 0.5), 
    panel.grid.minor = element_blank(), 
    panel.grid.major.y = element_line(color = "grey", size = 0.5), 
    panel.grid.minor.y = element_blank()
  )

ggsave(filename = file.path(output_path, "parameters_vs_variables.png"), plot = plot, 
       width = 8, height = 6, dpi = 300)


# Example Matching Algorithms Graphs -------------------

set.seed(42)
n <- 50
data <- data.frame(
  X1 = runif(n, 0, 10),
  X2 = runif(n, 0, 10),
  Treatment = sample(c("Control", "Treatment"), n, replace = TRUE)
)

# causal tree
plot <- ggplot(data, aes(x = X1, y = X2, shape = Treatment)) +
  geom_point(size = 3, aes(color = Treatment)) +
  scale_shape_manual(values = c(16, 17)) +
  geom_segment(aes(x = 5, y = 0, xend = 5, yend = 10), linetype = "dashed", color = "black") + 
  geom_segment(aes(x = 0, y = 5, xend = 5, yend = 5), linetype = "dashed", color = "black") + 
  geom_segment(aes(x = 5, y = 7, xend = 10, yend = 7), linetype = "dashed", color = "black") + 
  geom_segment(aes(x = 7.5, y = 7, xend = 7.5, yend = 0), linetype = "dashed", color = "black") +
  annotate("text", x = 5.5, y = 5, label = "X", size = 6, color = "black") +  
  labs(
    x = "X1",
    y = "X2"
  ) +
  theme_classic() +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )

ggsave(filename = file.path(output_path, "sample_causal_tree.png"), plot = plot, 
       width = 8, height = 6, dpi = 300)

# KNN Euclidean Matching
plot <- ggplot(data, aes(x = X1, y = X2, shape = Treatment)) +
  geom_point(size = 3, aes(color = Treatment)) +
  scale_shape_manual(values = c(16, 17)) +
  annotate("text", x = 5.5, y = 5, label = "X", size = 6, color = "black") +
  annotate("point", x = 5.5, y = 5, shape = 21, size = 90, color = "black", fill = NA, stroke = 1) +  
  labs(
    x = "X1",
    y = "X2"
  ) +
  theme_classic() +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )

ggsave(filename = file.path(output_path, "sample_euclidean.png"), plot = plot, 
       width = 8, height = 6, dpi = 300)

# Example causal forest defined weighting ------------------------

# raw data
plot <- ggplot(data, aes(x = X1, y = X2, shape = Treatment)) +
  geom_point(size = 3, aes(color = Treatment)) +
  scale_shape_manual(values = c(16, 17)) +
  annotate("text", x = 5.5, y = 5, label = "X", size = 6, color = "black") + 
  labs(
    x = "X1",
    y = "X2"
  ) +
  theme_classic() +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )

ggsave(filename = file.path(output_path, "cf_agg_0.png"), plot = plot, 
       width = 8, height = 6, dpi = 300)

# tree 1
plot <- ggplot(data, aes(x = X1, y = X2, shape = Treatment)) +
  geom_point(size = 3, aes(color = Treatment)) +
  scale_shape_manual(values = c(16, 17)) +
  geom_segment(aes(x = 5, y = 0, xend = 5, yend = 10), linetype = "dashed", color = "black") + 
  geom_segment(aes(x = 0, y = 5, xend = 5, yend = 5), linetype = "dashed", color = "black") + 
  geom_segment(aes(x = 5, y = 7, xend = 10, yend = 7), linetype = "dashed", color = "black") + 
  geom_segment(aes(x = 7.5, y = 7, xend = 7.5, yend = 0), linetype = "dashed", color = "black") +
  annotate("text", x = 5.5, y = 5, label = "X", size = 6, color = "black") +  
  labs(
    x = "X1",
    y = "X2"
  ) +
  theme_classic() +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )

ggsave(filename = file.path(output_path, "cf_agg_1.png"), plot = plot, 
       width = 8, height = 6, dpi = 300)

data <- data %>%
  mutate(tree_neighborhood_1 = ifelse(X1 > 5 & X1 < 7.5 & X2 < 7.5, 1, 0))

# tree 2
plot <- ggplot(data, aes(x = X1, y = X2, shape = Treatment)) +
  geom_point(size = 3, aes(color = Treatment)) +
  scale_shape_manual(values = c(16, 17)) +
  geom_segment(aes(x = 2.5, y = 0, xend = 2.5, yend = 10), linetype = "dashed", color = "black") + 
  geom_segment(aes(x = 0, y = 2.5, xend = 2.5, yend = 2.5), linetype = "dashed", color = "black") + 
  geom_segment(aes(x = 2.5, y = 7.5, xend = 10, yend = 7.5), linetype = "dashed", color = "black") + 
  geom_segment(aes(x = 7, y = 0, xend = 7, yend = 7.5), linetype = "dashed", color = "black") +
  annotate("text", x = 5.5, y = 5, label = "X", size = 6, color = "black") +  
  labs(
    x = "X1",
    y = "X2"
  ) +
  theme_classic() +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )

data <- data %>%
  mutate(tree_neighborhood_2 = ifelse(X1 > 2.5 & X1 < 7 & X2 < 7.5, 1, 0))

ggsave(filename = file.path(output_path, "cf_agg_2.png"), plot = plot, 
       width = 8, height = 6, dpi = 300)

# tree 3
plot <- ggplot(data, aes(x = X1, y = X2, shape = Treatment)) +
  geom_point(size = 3, aes(color = Treatment)) +
  scale_shape_manual(values = c(16, 17)) +
  geom_segment(aes(x = 0, y = 6, xend = 10, yend = 6), linetype = "dashed", color = "black") + 
  geom_segment(aes(x = 3, y = 0, xend = 3, yend = 6), linetype = "dashed", color = "black") + 
  geom_segment(aes(x = 2.5, y = 6, xend = 2.5, yend = 10), linetype = "dashed", color = "black") + 
  geom_segment(aes(x = 3, y = 2, xend = 10, yend = 2), linetype = "dashed", color = "black") +
  annotate("text", x = 5.5, y = 5, label = "X", size = 6, color = "black") +  
  labs(
    x = "X1",
    y = "X2"
  ) +
  theme_classic() +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )

data <- data %>%
  mutate(tree_neighborhood_3 = ifelse(X1 > 3 & X2 < 6 & X2 < 2.5, 1, 0),
         tree_neighborhood_agg = tree_neighborhood_1 + tree_neighborhood_2 + tree_neighborhood_3)

ggsave(filename = file.path(output_path, "cf_agg_3.png"), plot = plot, 
       width = 8, height = 6, dpi = 300)

# tree defined kernel
plot <- ggplot(data, aes(x = X1, y = X2, shape = Treatment, color = Treatment)) +
  geom_point(size = 3, aes(alpha = tree_neighborhood_agg)) +
  scale_shape_manual(values = c(16, 17)) +
  scale_alpha_continuous(range = c(0.2, 1.5)) +  
  annotate("text", x = 5.5, y = 5, label = "X", size = 6, color = "black") + 
  labs(
    x = "X1",
    y = "X2"
  ) +
  theme_classic() +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )

ggsave(filename = file.path(output_path, "cf_kernel.png"), plot = plot, 
       width = 8, height = 6, dpi = 300)














