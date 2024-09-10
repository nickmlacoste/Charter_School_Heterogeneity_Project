# Intro -----------------------------

"
This script produces the causal forest estimates of treatment effect heterogeneity for CHL(2024). 
"

# Imports ------------------

rm(list = ls())

library(tidyverse)
library(haven)
library(grf)
library(xtable)

data_path <- "C:/Users/nickm/OneDrive/Acer (new laptop)/Documents/PhD/Tulane University/Projects/Charter School Heterogeneity/data"
output_path <- "C:/Users/nickm/OneDrive/Acer (new laptop)/Documents/PhD/Tulane University/Projects/Charter School Heterogeneity/Charter_School_Heterogeneity_Project/analysis/output"

# Import graduation rate data
#charter_afgr2 <- read_dta(file.path(data_path, "charter_afgr2.dta"))
charter_afgr2 <- read_dta(file.path(data_path, "charter_afgr2_c.dta")) #this one is fully cleaned

# Import test score data
#charter_seda <- read_dta(file.path(data_path, "charter_seda.dta"))
charter_seda <- read_dta(file.path(data_path, "charter_seda_c.dta"))
# NOTE: MAY WANT TO DO THESE ONE-AT-A-TIME TO SAVE RAM



# Estimate Causal Forest on graduation rate data -------------------------

X_covariates <- c("logenroll", "perwht", "perblk", "perhsp", "perfrl", 
                  "perspeced", "urban", "suburb", "town", "rural", 
                  "p_rev", "p_exp", "str", "tea_salary", "num_magnet", "charter_eff")

outcomes <- c("afgr", "st_math", "st_math")

# remove rows with missing data for Y or W
charter_afgr2_clean <- charter_afgr2 %>%
  filter(!is.na(afgr) & !is.na(inter))

# estimate the model
X_matrix <- as.matrix(charter_afgr2_clean[, X_covariates])
Y_vector <- as.vector(charter_afgr2_clean$afgr)
W_vector <- as.vector(charter_afgr2_clean$lag_share)
weight_vector <- as.vector(charter_afgr2_clean$eweight)

cf_model <- causal_forest(X = X_matrix,
                          Y = Y_vector,
                          W = W_vector,
                          Y.hat = NULL,
                          W.hat = NULL,
                          num.trees = 100,
                          sample.weights = weight_vector,
                          clusters = as.factor(charter_afgr2_clean$district),
                          equalize.cluster.weights = FALSE,
                          sample.fraction = 0.5,
                          mtry = min(ceiling(sqrt(ncol(X_matrix)) + 20), ncol(X_matrix)),
                          min.node.size = 5,
                          honesty = TRUE,
                          honesty.fraction = 0.5,
                          honesty.prune.leaves = TRUE,
                          alpha = 0.05,
                          imbalance.penalty = 0,
                          stabilize.splits = TRUE,
                          ci.group.size = 2,
                          tune.parameters = "none",
                          tune.num.trees = 200,
                          tune.num.reps = 50,
                          tune.num.draws = 1000,
                          compute.oob.predictions = TRUE,
                          num.threads = NULL,
                          seed = runif(1, 0, .Machine$integer.max)
                          )

# Save (pickle) the causal forest model so we can import it in external files
saveRDS(cf_model, file = paste0(output_path, "/cf_model_afgr.rda"))
# this reads in the saved model
cf_model <- readRDS(file.path(output_path, "cf_model_afgr.rda"))

###########                        ################
###########Graduation Rate results ################
###########                       #################

# generate CATE estimates on training data (i.e. the full sample) using leave-one-out estimates ---------
afgr_cates <- predict(cf_model,
                      newdata = NULL,
                      estimate.variance = TRUE)

afgr_cates <- afgr_cates %>%
  mutate(
    p_value = 2 * (1 - pnorm(abs(predictions / sqrt(variance.estimates)))),  
    significant = if_else(p_value <= 0.1, 1, 0)
  )

# causal forest doubly-robust estimate of the average treatment effect:
afgr_ate <- average_treatment_effect(cf_model)

# variable importance factors plot ----------------
afgr_vif <- variable_importance(cf_model)
afgr_ranked_vars <- order(afgr_vif, decreasing = TRUE)

covariate_names <- c(
  logenroll = "Log of Enrollment",
  perwht = "Percent White",
  perblk = "Percent Black",
  perhsp = "Percent Hispanic",
  perfrl = "Percent Free/Reduced Lunch",
  perspeced = "Percent Special Ed",
  urban = "Urban",
  suburb = "Suburb",
  town = "Town",
  rural = "Rural",
  p_rev = "Per Pupil Revenue",
  p_exp = "Per Pupil Expenditure",
  str = "Student-Teacher Ratio",
  tea_salary = "Teacher Salary",
  num_magnet = "Number of Magnet Schools",
  charter_eff = "Charter Effectiveness"
)

vif_df <- data.frame(
  Variable = X_covariates,
  VIF_Score = as.vector(afgr_vif)
)

vif_df$Variable <- covariate_names[vif_df$Variable]

plot <- ggplot(vif_df, aes(x = reorder(Variable, VIF_Score), y = VIF_Score)) +
  geom_bar(stat = "identity", fill = "steelblue") +    
  coord_flip() +                                       
  labs(title = "Variable Importance (VIF Scores)", 
       x = "Variable", 
       y = "Variable Importance Score") +
  theme_minimal() 

ggsave(filename = file.path(output_path, "vif_scores.png"), plot = plot, 
       width = 8, height = 6, dpi = 300)





# best linear projection  --------------------

# (regresses T(X) = B_0 + A B_1, i.e. regress top 5 VIF variables on treatment
# effect to see which ones are associated with higher/lower treatment effects
afgr_blp <- best_linear_projection(cf_model, X_matrix[,afgr_ranked_vars[1:5]])



# Distribution of treatment effects plot

plot <- ggplot(afgr_cates, aes(x = predictions, fill = as.factor(significant))) +
  geom_histogram(binwidth = 0.1, color = "black", alpha = 0.7) + 
  scale_fill_manual(values = c("0" = "lightblue", "1" = "darkblue"), 
                    name = "P-Value <= 0.1") +  # Custom colors for 0 and 1
  labs(title = "Distribution of Estimated Treatment Effects", 
       x = "Treatment Effect", 
       y = "Frequency") +
  xlim(-2.5, 2.5) +  
  geom_vline(aes(xintercept = afgr_ate[1]), color = "red", linetype = "dashed", size = 1) + 
  geom_text(aes(x = afgr_ate[1], y = 20000, label = paste("ATE =", round(afgr_ate[1], 2))), 
            vjust = -0.5, hjust = 1.2, color = "red", size = 5) +
  theme_minimal()

ggsave(filename = file.path(output_path, "afgr_cate_dist.png"), plot = plot, 
       width = 8, height = 6, dpi = 300)


# covariate averages for significantly positive vs. negative districts: ---------------------
charter_afgr2_clean <- charter_afgr2_clean %>%
  mutate(index = row_number())
afgr_cates <- afgr_cates %>%
  mutate(index = row_number())
charter_afgr2_clean <- left_join(charter_afgr2_clean, afgr_cates, by = "index")

negative_effects <- charter_afgr2_clean %>%
  filter(predictions < 0 & significant == 1)

positive_effects <- charter_afgr2_clean %>%
  filter(predictions > 0 & significant == 1)

negative_means <- negative_effects %>%
  summarise(across(all_of(X_covariates), mean, na.rm = TRUE))

positive_means <- positive_effects %>%
  summarise(across(all_of(X_covariates), mean, na.rm = TRUE))

difference <- positive_means - negative_means

n_positive <- nrow(positive_effects)
n_negative <- nrow(negative_effects)

summary_table <- bind_cols(
  Covariate = covariate_names, 
  `Significantly Positive` = as.numeric(positive_means),
  `Significantly Negative` = as.numeric(negative_means),
  `Difference (Positive - Negative)` = as.numeric(difference)
) %>% 
  add_row(Covariate = "Number of Observations", 
          `Significantly Positive` = n_positive, 
          `Significantly Negative` = n_negative, 
          `Difference (Positive - Negative)` = n_positive + n_negative)

summary_table <- xtable(summary_table)
print(summary_table,
      file = file.path(output_path, "/tables/cov_means_table.tex"),
      include.rownames = TRUE,
      floating = FALSE)


# Group Average Treatment Effect Table --------------------

average_treatment_effect(cf_model, 
                         target.sample = "all",
                         method = "AIPW",
                         subset = X_matrix[,"urban"] == 1)












