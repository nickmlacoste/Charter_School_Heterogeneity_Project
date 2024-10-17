# Intro -----------------------------

"
This script produces the causal forest estimates of treatment effect heterogeneity for CHL(2024). 
"

###########                        ################
###########Graduation Rate results ################
###########                       #################

rm(list = ls())

library(tidyverse)
library(haven)
library(grf)
library(xtable)
library(knitr)
library(kableExtra)
library(plm)

data_path <- "C:/Users/nickm/OneDrive/Acer (new laptop)/Documents/PhD/Tulane University/Projects/Charter School Heterogeneity/data"
output_path <- "C:/Users/nickm/OneDrive/Acer (new laptop)/Documents/PhD/Tulane University/Projects/Charter School Heterogeneity/Charter_School_Heterogeneity_Project/analysis/output"
cf_output_path <- "C:/Users/nickm/OneDrive/Acer (new laptop)/Documents/PhD/Tulane University/Projects/Charter School Heterogeneity/large_output"

# Import graduation rate data
#charter_afgr2 <- read_dta(file.path(data_path, "charter_afgr2.dta"))
charter_afgr2 <- read_dta(file.path(data_path, "charter_afgr2_c.dta")) #this one is fully cleaned

# Estimate Causal Forest on graduation rate data -------------------------

X_covariates <- c("logenroll", "perwht", "perblk", "perhsp", "perfrl", 
                  "perspeced", "urban", "suburb", "town", "rural", 
                  "p_rev", "p_exp", "str", "tea_salary", "num_magnet", "charter_eff")

# remove rows with missing data for Y or W
charter_afgr2_clean <- charter_afgr2 %>%
  filter(!is.na(afgr) & !is.na(inter))

# global within transformation
charter_afgr2_panel <- pdata.frame(charter_afgr2_clean,
                                   index = c("district","stateyear"))

X_matrix <- as.data.frame(lapply(X_covariates, function(var) {
  Within(charter_afgr2_panel[[var]], effect = "twoway")
}))
colnames(X_matrix) <- X_covariates
X_matrix <- as.matrix(X_matrix)
Y_vector <- as.vector(Within(charter_afgr2_panel$afgr, effect = "twoway"))
W_vector <- as.vector(Within(charter_afgr2_panel$lag_share, "twoway"))
weight_vector <- as.vector(charter_afgr2_panel$eweight)


# # estimate the model (without within-transform)
# X_matrix <- as.matrix(charter_afgr2_clean[, X_covariates])
# Y_vector <- as.vector(charter_afgr2_clean$afgr)
# W_vector <- as.vector(charter_afgr2_clean$lag_share)
# weight_vector <- as.vector(charter_afgr2_clean$eweight)

# Toggle to TRUE if you want to re-train the causal forest, if FALSE it will load the saved model
train_grad_rate_model <- FALSE

if (train_grad_rate_model) {
  
  # Estimate the causal forest model
  cf_model <- causal_forest(X = X_matrix,
                            Y = Y_vector,
                            W = W_vector,
                            Y.hat = NULL,
                            W.hat = NULL,
                            num.trees = 1000,
                            sample.weights = weight_vector,
                            clusters = as.factor(charter_afgr2_panel$district),
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
  
  # Save the causal forest model
  saveRDS(cf_model, file = paste0(cf_output_path, "/cf_model_afgr.rda"))
  
} else {
  
  # Load the previously saved model
  cf_model <- readRDS(file.path(cf_output_path, "cf_model_afgr.rda"))
}

# generate CATE estimates on training data (i.e. the full sample) using leave-one-out estimates ---------
afgr_cates <- predict(cf_model,
                      newdata = NULL,
                      estimate.variance = TRUE)

afgr_cates <- afgr_cates %>%
  mutate(
    p_value = 2 * (1 - pnorm(abs(predictions / sqrt(variance.estimates)))),  
    significant = if_else(p_value <= 0.1, 1, 0),
    dr_score = get_scores(cf_model)
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
  labs(title = "Variable Importance (VIF Scores) - Graduation Rates", 
       x = "Variable", 
       y = "Variable Importance Score") +
  theme_minimal() 

ggsave(filename = file.path(output_path, "/figures/vif_scores_afgr.png"), plot = plot, 
       width = 8, height = 6, dpi = 300)





# best linear projection  --------------------

# (regresses T(X) = B_0 + A B_1, i.e. regress top 5 VIF variables on treatment
# effect to see which ones are associated with higher/lower treatment effects
afgr_blp <- best_linear_projection(cf_model, X_matrix[,afgr_ranked_vars[1:5]])

blp_results <- data.frame(
  Variable = c("(Intercept)", "logenroll", "perhsp", "perfrl", "perwht", "str"),
  Estimate = afgr_blp[, 1],  
  `Std. Error` = afgr_blp[, 2],  
  `t value` = afgr_blp[, 3],  
  `Pr(>|t|)` = afgr_blp[, 4]  
)

blp_results <- xtable(blp_results, 
                    caption = "Best Linear Projection of the Conditional Average Treatment Effect",
                    align = c("l", "l", "r", "r", "r", "r"))

print(blp_results,
      file = file.path(output_path, "/tables/blp_table_afgr.tex"),
      include.rownames = FALSE,
      floating = FALSE,
      hline.after = c(-1, 0, nrow(blp_results))
      )

# Distribution of treatment effects plot -------------------

plot <- ggplot(afgr_cates, aes(x = predictions, fill = as.factor(significant))) +
  geom_histogram(binwidth = 0.1, color = "black", alpha = 0.7) + 
  scale_fill_manual(values = c("0" = "lightblue", "1" = "darkblue"), 
                    name = "P-Value <= 0.1") +  
  labs(title = "Distribution of Estimated Treatment Effects", 
       x = "Treatment Effect", 
       y = "Frequency") +
  xlim(-1, 1) +  
  geom_vline(aes(xintercept = afgr_ate[1]), color = "red", linetype = "dashed", size = 1) + 
  geom_text(aes(x = afgr_ate[1], y = 20000, label = paste("ATE =", round(afgr_ate[1], 2))), 
            vjust = -0.5, hjust = 1.2, color = "red", size = 5) +
  theme_minimal()

ggsave(filename = file.path(output_path, "/figures/cate_dist_afgr.png"), plot = plot, 
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
      file = file.path(output_path, "/tables/cov_means_table_afgr.tex"),
      include.rownames = TRUE,
      floating = FALSE)

# District-level treatment effect distribution -----------------------

# aggregate treatment effects to district level
district_cates <- charter_afgr2_clean %>%
  group_by(district) %>%
  summarize(
    mean_prediction = mean(predictions, na.rm = TRUE),
    mean_variance = mean(variance.estimates, na.rm = TRUE),
    dr_score = mean(dr_score, na.rm = TRUE)
  ) %>%
  mutate(
    p_value = 2 * (1 - pnorm(abs(mean_prediction / sqrt(mean_variance)))),
    significant = if_else(p_value <= 0.1, 1, 0)
  )

# Create the distribution plot of district-level treatment effects
plot <- ggplot(district_cates, aes(x = mean_prediction, fill = as.factor(significant))) +
  geom_histogram(binwidth = 0.01, color = "black", alpha = 0.7) + 
  scale_fill_manual(values = c("0" = "lightblue", "1" = "darkblue"), 
                    name = "P-Value <= 0.1") +  
  labs(title = "Distribution of District-Level Estimated Treatment Effects", 
       x = "Treatment Effect", 
       y = "Frequency") +
  xlim(-0.25, 0.5) +  
  geom_vline(aes(xintercept = afgr_ate[1]), color = "red", linetype = "dashed", size = 1) + 
  geom_text(aes(x = afgr_ate[1], y = 750, label = paste("ATE =", round(afgr_ate[1], 2))), 
            vjust = -0.5, hjust = 1.2, color = "red", size = 5) +
  theme_minimal()

ggsave(filename = file.path(output_path, "figures/cate_dist_district_afgr.png"), 
       plot = plot, 
       width = 8, height = 6, dpi = 300)


# Group Average Treatment Effect Table --------------------

# # enter subgroups of interest in this list
# # this applies to the other, slower, commented out code
# subgroup_conditions <- list(
#   "Urban" = X_matrix[,"urban"] == 1,
#   "Suburban" = X_matrix[,"suburb"] == 1,
#   "Rural" = X_matrix[,"rural"] == 1,
#   "Percent Free Lunch > 20%" = X_matrix[,"perfrl"] > 0.20
# )

subgroup_conditions <- list(
  "Urban" = charter_afgr2_clean[,"urban"] == 1,
  "Suburban" = charter_afgr2_clean[,"suburb"] == 1,
  "Rural" = charter_afgr2_clean[,"rural"] == 1,
  "Percent Free Lunch > 20%" = charter_afgr2_clean[,"perfrl"] > 0.20
)

# initializes the table to store results
gate_results_table <- data.frame(
  Group = character(),
  GATE = numeric(),
  SE = numeric(),
  `p-value` = numeric(),
  `% N` = numeric(),
  stringsAsFactors = FALSE
)

# calculate GATES

for (group_name in names(subgroup_conditions)) {
  
  condition <- subgroup_conditions[[group_name]]
  
  ## this commented code works but is very slow because it re-estimate dr_scores for each condition
  # gate_result <- average_treatment_effect(cf_model, 
  #                                         target.sample = "all",
  #                                         method = "AIPW",
  #                                         subset = condition)
  
  # gate_estimate <- gate_result[[1]]
  # gate_se <- gate_result[[2]]
  
  filtered_df <- charter_afgr2_clean[condition, ]
  
  gate_estimate <- mean(filtered_df$dr_score, na.rm = TRUE)
  gate_se <- sd(filtered_df$dr_score, na.rm = TRUE) / sqrt(nrow(filtered_df))
  
  z_score <- gate_estimate / gate_se
  p_value <- 2 * (1 - pnorm(abs(z_score)))
  
  proportion_N <- mean(condition)
  
  new_row <- data.frame(
    Group = group_name,
    GATE = gate_estimate,
    SE = gate_se,
    `p-value` = p_value,
    `Share of N` = proportion_N,
    stringsAsFactors = FALSE
  )
  
  gate_results_table <- rbind(gate_results_table, new_row)
}

gate_results_table <- xtable(gate_results_table)
print(gate_results_table,
      file = file.path(output_path, "/tables/gates_table_afgr.tex"),
      include.rownames = TRUE,
      floating = FALSE)

# GATE table within states --------------------

# Initialize the table to store results
state_results_table <- data.frame(
  State = character(),
  GATE = numeric(),
  SE = numeric(),
  `p-value` = numeric(),
  `Share of N` = numeric(),
  stringsAsFactors = FALSE
)

# Get unique state names
unique_states <- unique(charter_afgr2_clean$statename)

# Loop through each state and calculate the required statistics
for (state in unique_states) {
  
  condition <- charter_afgr2_clean$statename == state
  filtered_df <- charter_afgr2_clean[condition, ]
  
  gate_estimate <- mean(filtered_df$dr_score, na.rm = TRUE)
  gate_se <- sd(filtered_df$dr_score, na.rm = TRUE) / sqrt(nrow(filtered_df))
  
  z_score <- gate_estimate / gate_se
  p_value <- 2 * (1 - pnorm(abs(z_score)))
  
  proportion_N <- mean(condition)
  
  # Determine significance stars
  stars <- ifelse(p_value < 0.01, "***",
                  ifelse(p_value < 0.05, "**",
                         ifelse(p_value < 0.1, "*", "")))
  
  # Create GATE with stars
  gate_with_stars <- paste0(round(gate_estimate, 3), stars)
  
  new_row <- data.frame(
    State = state,
    GATE = gate_with_stars,
    SE = round(gate_se, 3),
    `p-value` = round(p_value, 3),
    `Share of N` = round(proportion_N, 3),
    stringsAsFactors = FALSE
  )
  
  state_results_table <- rbind(state_results_table, new_row)
}

# Create the LaTeX table
kable(state_results_table, 
      caption = "GATEs within States (Graduation Rates)",
      format = "latex", booktabs = TRUE, 
      linesep = "", escape = FALSE, label = "state_gates_afgr") %>%
  kable_styling(latex_options = c("striped", "hold_position")) %>%
  save_kable(file = file.path(output_path, "tables/state_gates_table_afgr.tex"))

# GATE bar graph at different dosage levels (grad rates) -----------------------

# Step 1: Calculate the treatment_dose_change variable and create a new column for the lag of lag_share
charter_afgr2_dosage <- charter_afgr2_clean %>%
  group_by(district) %>%
  arrange(district, stateyear) %>%
  mutate(
    lag_lag_share = Hmisc::Lag(lag_share),
    treatment_dose_change = lag_share - lag_lag_share
  ) %>%
  ungroup()

# Step 2: Filter the data to include only units where treatment_dose_change is non-zero
charter_afgr2_dosage <- charter_afgr2_dosage %>%
  filter(treatment_dose_change != 0)

# Step 3: Group into deciles based on the lag of lag_share
charter_afgr2_dosage <- charter_afgr2_dosage %>%
  mutate(lag_share_decile = ntile(lag_lag_share, 10))

# Calculate decile thresholds
decile_thresholds <- quantile(charter_afgr2_dosage$lag_lag_share, 
                              probs = seq(0, 1, 0.1), na.rm = TRUE)

# Step 4: Estimate GATEs by average CATE predictions from the current year
decile_results_table <- data.frame(
  Decile = integer(),
  GATE = numeric(),
  SE = numeric(),
  `p-value` = numeric(),
  stringsAsFactors = FALSE
)

for (decile in 1:10) {
  
  condition <- charter_afgr2_dosage$lag_share_decile == decile
  filtered_df <- charter_afgr2_dosage[condition, ]
  
  gate_estimate <- mean(filtered_df$dr_score, na.rm = TRUE)
  gate_se <- sd(filtered_df$dr_score, na.rm = TRUE) / sqrt(nrow(filtered_df))
  
  z_score <- gate_estimate / gate_se
  p_value <- 2 * (1 - pnorm(abs(z_score)))
  
  new_row <- data.frame(
    Decile = decile,
    GATE = gate_estimate,
    SE = gate_se,
    `p-value` = p_value,
    stringsAsFactors = FALSE
  )
  
  decile_results_table <- rbind(decile_results_table, new_row)
}

# Step 5: Plot the bar graph
plot <- ggplot(decile_results_table, aes(x = factor(Decile, labels = round(decile_thresholds[-1], 2)), y = GATE)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_errorbar(aes(ymin = GATE - SE, ymax = GATE + SE), width = 0.2) +
  labs(x = "Lag Share Decile Threshold", y = "GATE", title = "GATE within Different Deciles of Lag Share") +
  theme_minimal()

# Save the plot
ggsave(filename = file.path(output_path, "figures/gate_deciles_afgr.png"), 
       plot = plot, 
       width = 8, height = 6, dpi = 300)



###########                        ################
###########  Test Score results    ################
###########                        ################

rm(list = ls())

library(tidyverse)
library(haven)
library(grf)
library(xtable)
library(knitr)
library(kableExtra)
library(plm)

data_path <- "C:/Users/nickm/OneDrive/Acer (new laptop)/Documents/PhD/Tulane University/Projects/Charter School Heterogeneity/data"
output_path <- "C:/Users/nickm/OneDrive/Acer (new laptop)/Documents/PhD/Tulane University/Projects/Charter School Heterogeneity/Charter_School_Heterogeneity_Project/analysis/output"
cf_output_path <- "C:/Users/nickm/OneDrive/Acer (new laptop)/Documents/PhD/Tulane University/Projects/Charter School Heterogeneity/large_output"

# Import test score data
#charter_seda <- read_dta(file.path(data_path, "charter_seda.dta"))
charter_seda <- read_dta(file.path(data_path, "charter_seda_c.dta")) #this one is fully cleaned

# Estimate Causal Forest on test score data (MATH) -------------------------

X_covariates <- c("logenroll", "perwht", "perblk", "perhsp", "perfrl", 
                  "perspeced", "urban", "suburb", "town", "rural", 
                  "p_rev", "p_exp", "str", "tea_salary", "num_magnet", "charter_eff")

# remove rows with missing data for Y or W
charter_seda_math <- charter_seda %>%
  filter(!is.na(st_math) & !is.na(inter))

# global within transformation
charter_seda_math_panel <- pdata.frame(charter_seda_math,
                                   index = c("district","sgyear"))

X_matrix <- as.data.frame(lapply(X_covariates, function(var) {
  Within(charter_seda_math_panel[[var]], effect = "twoway")
}))
colnames(X_matrix) <- X_covariates
X_matrix <- as.matrix(X_matrix)
Y_vector <- as.vector(Within(charter_seda_math_panel$st_math, effect = "twoway"))
W_vector <- as.vector(Within(charter_seda_math_panel$lag_grade, "twoway"))
weight_vector <- as.vector(charter_seda_math_panel$eweight)

# # estimate the model - without within-transform
# X_matrix <- as.matrix(charter_seda_math[, X_covariates])
# Y_vector <- as.vector(charter_seda_math$st_math)
# W_vector <- as.vector(charter_seda_math$lag_grade)
# weight_vector <- as.vector(charter_seda_math$eweight)

# Toggle to TRUE if you want to re-train the causal forest, if FALSE it will load the saved model
train_math_model <- FALSE

if (train_math_model) {
  
  # Estimate the causal forest model
  cf_model <- causal_forest(X = X_matrix,
                            Y = Y_vector,
                            W = W_vector,
                            Y.hat = NULL,
                            W.hat = NULL,
                            num.trees = 1000,
                            sample.weights = weight_vector,
                            clusters = as.factor(charter_seda_math_panel$district),
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
  
  # Save the causal forest model
  saveRDS(cf_model, file = paste0(cf_output_path, "/cf_model_math.rda"))
  
} else {
  
  # Load the previously saved model
  cf_model <- readRDS(file.path(cf_output_path, "cf_model_math.rda"))
}

# generate CATE estimates on training data (i.e. the full sample) using leave-one-out estimates ---------
math_cates <- predict(cf_model,
                      newdata = NULL,
                      estimate.variance = TRUE)

math_cates <- math_cates %>%
  mutate(
    p_value = 2 * (1 - pnorm(abs(predictions / sqrt(variance.estimates)))),  
    significant = if_else(p_value <= 0.1, 1, 0),
    dr_score = get_scores(cf_model)
  )

# causal forest doubly-robust estimate of the average treatment effect:
math_ate <- average_treatment_effect(cf_model,
                                     target.sample = "all",
                                     method = "AIPW")

# variable importance factors plot ----------------
math_vif <- variable_importance(cf_model)
math_ranked_vars <- order(math_vif, decreasing = TRUE)

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
  VIF_Score = as.vector(math_vif)
)

vif_df$Variable <- covariate_names[vif_df$Variable]

plot <- ggplot(vif_df, aes(x = reorder(Variable, VIF_Score), y = VIF_Score)) +
  geom_bar(stat = "identity", fill = "steelblue") +    
  coord_flip() +                                       
  labs(title = "Variable Importance (VIF Scores) - Math", 
       x = "Variable", 
       y = "Variable Importance Score") +
  theme_minimal() 

ggsave(filename = file.path(output_path, "/figures/vif_scores_math.png"), plot = plot, 
       width = 8, height = 6, dpi = 300)

# best linear projection  --------------------

# (regresses T(X) = B_0 + A B_1, i.e. regress top 5 VIF variables on treatment
# effect to see which ones are associated with higher/lower treatment effects
math_blp <- best_linear_projection(cf_model, X_matrix[,math_ranked_vars[1:5]])

blp_results <- data.frame(
  Variable = c("(Intercept)", "logenroll", "perhsp", "perfrl", "perwht", "str"),
  Estimate = math_blp[, 1],  
  `Std. Error` = math_blp[, 2],  
  `t value` = math_blp[, 3],  
  `Pr(>|t|)` = math_blp[, 4]  
)

blp_results <- xtable(blp_results, 
                      caption = "Best Linear Projection of the CATE - Math",
                      align = c("l", "l", "r", "r", "r", "r"))

print(blp_results,
      file = file.path(output_path, "/tables/blp_table_math.tex"),
      include.rownames = FALSE,
      floating = FALSE,
      hline.after = c(-1, 0, nrow(blp_results))
)

# Distribution of treatment effects plot -------------------

plot <- ggplot(math_cates, aes(x = predictions, fill = as.factor(significant))) +
  geom_histogram(binwidth = 0.1, color = "black", alpha = 0.7) + 
  scale_fill_manual(values = c("0" = "lightblue", "1" = "darkblue"), 
                    name = "P-Value <= 0.1") +  # Custom colors for 0 and 1
  labs(title = "Distribution of Estimated Treatment Effects - Math", 
       x = "Treatment Effect", 
       y = "Frequency") +
  xlim(-2, 2) +  
  geom_vline(aes(xintercept = math_ate[1]), color = "red", linetype = "dashed", size = 1) + 
  geom_text(aes(x = math_ate[1], y = 20000, label = paste("ATE =", round(math_ate[1], 2))), 
            vjust = -0.5, hjust = 1.2, color = "red", size = 5) +
  theme_minimal()

ggsave(filename = file.path(output_path, "/figures/cate_dist_math.png"), plot = plot, 
       width = 8, height = 6, dpi = 300)

# covariate averages for significantly positive vs. negative districts: ---------------------
charter_seda_math <- charter_seda_math %>%
  mutate(index = row_number())
math_cates <- math_cates %>%
  mutate(index = row_number())
charter_seda_math <- left_join(charter_seda_math, math_cates, by = "index")

negative_effects <- charter_seda_math %>%
  filter(predictions < 0 & significant == 1)

positive_effects <- charter_seda_math %>%
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
      file = file.path(output_path, "/tables/cov_means_table_math.tex"),
      include.rownames = TRUE,
      floating = FALSE)

# District-level treatment effect distribution (Math) -----------------------

# aggregate treatment effects to district level
district_cates <- charter_seda_math %>%
  group_by(district) %>%
  summarize(
    mean_prediction = mean(predictions, na.rm = TRUE),
    mean_variance = mean(variance.estimates, na.rm = TRUE),
    dr_score = mean(dr_score, na.rm = TRUE)
  ) %>%
  mutate(
    p_value = 2 * (1 - pnorm(abs(mean_prediction / sqrt(mean_variance)))),
    significant = if_else(p_value <= 0.1, 1, 0)
  )

# Create the distribution plot of district-level treatment effects
plot <- ggplot(district_cates, aes(x = mean_prediction, fill = as.factor(significant))) +
  geom_histogram(binwidth = 0.01, color = "black", alpha = 0.7) + 
  scale_fill_manual(values = c("0" = "lightblue", "1" = "darkblue"), 
                    name = "P-Value <= 0.1") +  
  labs(title = "Distribution of District-Level Estimated Treatment Effects", 
       x = "Treatment Effect", 
       y = "Frequency") +
  xlim(-0.5, 0.5) +  
  geom_vline(aes(xintercept = math_ate[1]), color = "red", linetype = "dashed", size = 1) + 
  geom_text(aes(x = afgr_ate[1], y = 750, label = paste("ATE =", round(math_ate[1], 2))), 
            vjust = -0.5, hjust = 1.2, color = "red", size = 5) +
  theme_minimal()

ggsave(filename = file.path(output_path, "figures/cate_dist_district_math.png"), 
       plot = plot, 
       width = 8, height = 6, dpi = 300)


# Group Average Treatment Effect Table --------------------

# # enter subgroups of interest in this list
# # this is old, slower code
# subgroup_conditions <- list(
#   "Urban" = X_matrix[,"urban"] == 1,
#   "Suburban" = X_matrix[,"suburb"] == 1,
#   "Rural" = X_matrix[,"rural"] == 1,
#   "Percent Free Lunch > 20%" = X_matrix[,"perfrl"] > 0.20
# )

subgroup_conditions <- list(
  "Urban" = charter_seda_math[,"urban"] == 1,
  "Suburban" = charter_seda_math[,"suburb"] == 1,
  "Rural" = charter_seda_math[,"rural"] == 1,
  "Percent Free Lunch > 20%" = charter_seda_math[,"perfrl"] > 0.20
)

# initializes the table to store results
gate_results_table <- data.frame(
  Group = character(),
  GATE = numeric(),
  SE = numeric(),
  `p-value` = numeric(),
  `% N` = numeric(),
  stringsAsFactors = FALSE
)

# calculate GATES
for (group_name in names(subgroup_conditions)) {
  
  condition <- subgroup_conditions[[group_name]]
  
  ## this commented code works but is very slow because it re-estimate dr_score for each condition
  # gate_result <- average_treatment_effect(cf_model, 
  #                                         target.sample = "all",
  #                                         method = "AIPW",
  #                                         subset = condition)
  
  # gate_estimate <- gate_result[[1]]
  # gate_se <- gate_result[[2]]
  
  filtered_df <- charter_seda_math[condition, ]
  
  gate_estimate <- mean(filtered_df$dr_score, na.rm = TRUE)
  gate_se <- sd(filtered_df$dr_score, na.rm = TRUE) / sqrt(nrow(filtered_df))
  
  z_score <- gate_estimate / gate_se
  p_value <- 2 * (1 - pnorm(abs(z_score)))
  
  proportion_N <- mean(condition)
  
  new_row <- data.frame(
    Group = group_name,
    GATE = gate_estimate,
    SE = gate_se,
    `p-value` = p_value,
    `Share of N` = proportion_N,
    stringsAsFactors = FALSE
  )
  
  gate_results_table <- rbind(gate_results_table, new_row)
}

gate_results_table <- xtable(gate_results_table)
print(gate_results_table,
      file = file.path(output_path, "/tables/gates_table_math.tex"),
      include.rownames = TRUE,
      floating = FALSE)

# GATE table within states (MATH) ------------------

# Initialize the table to store results
state_results_table <- data.frame(
  State = character(),
  GATE = numeric(),
  SE = numeric(),
  `p-value` = numeric(),
  `Share of N` = numeric(),
  stringsAsFactors = FALSE
)

# Get unique state names
unique_states <- unique(charter_seda_math$statename)

# Loop through each state and calculate the required statistics
for (state in unique_states) {
  
  condition <- charter_seda_math$statename == state
  filtered_df <- charter_seda_math[condition, ]
  
  gate_estimate <- mean(filtered_df$dr_score, na.rm = TRUE)
  gate_se <- sd(filtered_df$dr_score, na.rm = TRUE) / sqrt(nrow(filtered_df))
  
  z_score <- gate_estimate / gate_se
  p_value <- 2 * (1 - pnorm(abs(z_score)))
  
  proportion_N <- mean(condition)
  
  # Determine significance stars
  stars <- ifelse(p_value < 0.01, "***",
                  ifelse(p_value < 0.05, "**",
                         ifelse(p_value < 0.1, "*", "")))
  
  # Create GATE with stars
  gate_with_stars <- paste0(round(gate_estimate, 3), stars)
  
  new_row <- data.frame(
    State = state,
    GATE = gate_with_stars,
    SE = round(gate_se, 3),
    `p-value` = round(p_value, 3),
    `Share of N` = round(proportion_N, 3),
    stringsAsFactors = FALSE
  )
  
  state_results_table <- rbind(state_results_table, new_row)
}

# Create the LaTeX table
kable(state_results_table, 
      caption = "GATEs within States (Math Scores)",
      format = "latex", booktabs = TRUE, 
      linesep = "", escape = FALSE, label = "state_gates_math") %>%
  kable_styling(latex_options = c("striped", "hold_position")) %>%
  save_kable(file = file.path(output_path, "tables/state_gates_table_math.tex"))

# GATE bar graph at different dosage levels (MATH) -----------------------

# Step 1: Calculate the treatment_dose_change variable and create a new column for the lag of lag_share
charter_seda_math_dosage <- charter_seda_math %>%
  group_by(district) %>%
  arrange(district, stateyear) %>%
  mutate(
    lag_lag_share = Hmisc::Lag(lag_share),
    treatment_dose_change = lag_share - lag_lag_share
  ) %>%
  ungroup()

# Step 2: Filter the data to include only units where treatment_dose_change is non-zero
charter_seda_math_dosage <- charter_seda_math_dosage %>%
  filter(treatment_dose_change != 0)

# Step 3: Group into deciles based on the lag of lag_share
charter_seda_math_dosage <- charter_seda_math_dosage %>%
  mutate(lag_share_decile = ntile(lag_lag_share, 10))

# Calculate decile thresholds
decile_thresholds <- quantile(charter_seda_math_dosage$lag_lag_share, 
                              probs = seq(0, 1, 0.1), na.rm = TRUE)

# Step 4: Estimate GATEs by average CATE predictions from the current year
decile_results_table <- data.frame(
  Decile = integer(),
  GATE = numeric(),
  SE = numeric(),
  `p-value` = numeric(),
  stringsAsFactors = FALSE
)

for (decile in 1:10) {
  
  condition <- charter_seda_math_dosage$lag_share_decile == decile
  filtered_df <- charter_seda_math_dosage[condition, ]
  
  gate_estimate <- mean(filtered_df$dr_score, na.rm = TRUE)
  gate_se <- sd(filtered_df$dr_score, na.rm = TRUE) / sqrt(nrow(filtered_df))
  
  z_score <- gate_estimate / gate_se
  p_value <- 2 * (1 - pnorm(abs(z_score)))
  
  new_row <- data.frame(
    Decile = decile,
    GATE = gate_estimate,
    SE = gate_se,
    `p-value` = p_value,
    stringsAsFactors = FALSE
  )
  
  decile_results_table <- rbind(decile_results_table, new_row)
}

# Step 5: Plot the bar graph
plot <- ggplot(decile_results_table, aes(x = factor(Decile, labels = round(decile_thresholds[-1], 2)), y = GATE)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_errorbar(aes(ymin = GATE - SE, ymax = GATE + SE), width = 0.2) +
  labs(x = "Lag Share Decile Threshold", y = "GATE", 
       title = "GATE within Different Deciles of Lag Share (MATH)") +
  theme_minimal()

# Save the plot
ggsave(filename = file.path(output_path, "figures/gate_deciles_math.png"), 
       plot = plot, 
       width = 8, height = 6, dpi = 300)



# Estimate the Causal Forest on test score data (ELA) ------------------------

X_covariates <- c("logenroll", "perwht", "perblk", "perhsp", "perfrl", 
                  "perspeced", "urban", "suburb", "town", "rural", 
                  "p_rev", "p_exp", "str", "tea_salary", "num_magnet", "charter_eff")

charter_seda_ela <- charter_seda %>%
  filter(!is.na(st_ela) & !is.na(inter))

# global within transformation
charter_seda_ela_panel <- pdata.frame(charter_seda_ela,
                                       index = c("district","sgyear"))

X_matrix <- as.data.frame(lapply(X_covariates, function(var) {
  Within(charter_seda_ela_panel[[var]], effect = "twoway")
}))
colnames(X_matrix) <- X_covariates
X_matrix <- as.matrix(X_matrix)
Y_vector <- as.vector(Within(charter_seda_ela_panel$st_ela, effect = "twoway"))
W_vector <- as.vector(Within(charter_seda_ela_panel$lag_grade, "twoway"))
weight_vector <- as.vector(charter_seda_ela_panel$eweight)

# # estimate the model - without within-transform
# X_matrix <- as.matrix(charter_seda_ela[, X_covariates])
# Y_vector <- as.vector(charter_seda_ela$st_ela)
# W_vector <- as.vector(charter_seda_ela$lag_grade)
# weight_vector <- as.vector(charter_seda_ela$eweight)

# Toggle to TRUE if you want to re-train the causal forest, if FALSE it will load the saved model
train_ela_model <- FALSE

if (train_ela_model) {
  
  # Estimate the causal forest model
  cf_model <- causal_forest(X = X_matrix,
                            Y = Y_vector,
                            W = W_vector,
                            Y.hat = NULL,
                            W.hat = NULL,
                            num.trees = 1000,
                            sample.weights = weight_vector,
                            clusters = as.factor(charter_seda_ela_panel$district),
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
  
  # Save the causal forest model
  saveRDS(cf_model, file = paste0(cf_output_path, "/cf_model_ela.rda"))
  
} else {
  
  # Load the previously saved model
  cf_model <- readRDS(file.path(cf_output_path, "cf_model_ela.rda"))
}

# generate CATE estimates on training data (i.e. the full sample) using leave-one-out estimates ---------
ela_cates <- predict(cf_model,
                      newdata = NULL,
                      estimate.variance = TRUE)

ela_cates <- ela_cates %>%
  mutate(
    p_value = 2 * (1 - pnorm(abs(predictions / sqrt(variance.estimates)))),  
    significant = if_else(p_value <= 0.1, 1, 0),
    dr_score = get_scores(cf_model)
  )

# causal forest doubly-robust estimate of the average treatment effect:
ela_ate <- average_treatment_effect(cf_model,
                                     target.sample = "all",
                                     method = "AIPW")

# variable importance factors plot ----------------
ela_vif <- variable_importance(cf_model)
ela_ranked_vars <- order(ela_vif, decreasing = TRUE)

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
  VIF_Score = as.vector(ela_vif)
)

vif_df$Variable <- covariate_names[vif_df$Variable]

plot <- ggplot(vif_df, aes(x = reorder(Variable, VIF_Score), y = VIF_Score)) +
  geom_bar(stat = "identity", fill = "steelblue") +    
  coord_flip() +                                       
  labs(title = "Variable Importance (VIF Scores) - ELA", 
       x = "Variable", 
       y = "Variable Importance Score") +
  theme_minimal() 

ggsave(filename = file.path(output_path, "/figures/vif_scores_ela.png"), plot = plot, 
       width = 8, height = 6, dpi = 300)

# best linear projection  --------------------

# (regresses T(X) = B_0 + A B_1, i.e. regress top 5 VIF variables on treatment
# effect to see which ones are associated with higher/lower treatment effects
ela_blp <- best_linear_projection(cf_model, X_matrix[,ela_ranked_vars[1:5]])

blp_results <- data.frame(
  Variable = c("(Intercept)", "logenroll", "perhsp", "perfrl", "perwht", "str"),
  Estimate = ela_blp[, 1],  
  `Std. Error` = ela_blp[, 2],  
  `t value` = ela_blp[, 3],  
  `Pr(>|t|)` = ela_blp[, 4]  
)

blp_results <- xtable(blp_results, 
                      caption = "Best Linear Projection of the CATE - ELA",
                      align = c("l", "l", "r", "r", "r", "r"))

print(blp_results,
      file = file.path(output_path, "/tables/blp_table_ela.tex"),
      include.rownames = FALSE,
      floating = FALSE,
      hline.after = c(-1, 0, nrow(blp_results))
)

# Distribution of treatment effects plot -------------------

plot <- ggplot(ela_cates, aes(x = predictions, fill = as.factor(significant))) +
  geom_histogram(binwidth = 0.1, color = "black", alpha = 0.7) + 
  scale_fill_manual(values = c("0" = "lightblue", "1" = "darkblue"), 
                    name = "P-Value <= 0.1") +  # Custom colors for 0 and 1
  labs(title = "Distribution of Estimated Treatment Effects - ELA", 
       x = "Treatment Effect", 
       y = "Frequency") +
  xlim(-1.5, 1.5) +  
  geom_vline(aes(xintercept = ela_ate[1]), color = "red", linetype = "dashed", size = 1) + 
  geom_text(aes(x = ela_ate[1], y = 20000, label = paste("ATE =", round(ela_ate[1], 2))), 
            vjust = -0.5, hjust = 1.2, color = "red", size = 5) +
  theme_minimal()

ggsave(filename = file.path(output_path, "/figures/cate_dist_ela.png"), plot = plot, 
       width = 8, height = 6, dpi = 300)

# covariate averages for significantly positive vs. negative districts: ---------------------
charter_seda_ela <- charter_seda_ela %>%
  mutate(index = row_number())
ela_cates <- ela_cates %>%
  mutate(index = row_number())
charter_seda_ela <- left_join(charter_seda_ela, ela_cates, by = "index")

negative_effects <- charter_seda_ela %>%
  filter(predictions < 0 & significant == 1)

positive_effects <- charter_seda_ela %>%
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
      file = file.path(output_path, "/tables/cov_means_table_ela.tex"),
      include.rownames = TRUE,
      floating = FALSE)


# District-level treatment effect distribution (ELA) -----------------------

# aggregate treatment effects to district level
district_cates <- charter_seda_ela %>%
  group_by(district) %>%
  summarize(
    mean_prediction = mean(predictions, na.rm = TRUE),
    mean_variance = mean(variance.estimates, na.rm = TRUE),
    dr_score = mean(dr_score, na.rm = TRUE)
  ) %>%
  mutate(
    p_value = 2 * (1 - pnorm(abs(mean_prediction / sqrt(mean_variance)))),
    significant = if_else(p_value <= 0.1, 1, 0)
  )

# Create the distribution plot of district-level treatment effects
plot <- ggplot(district_cates, aes(x = mean_prediction, fill = as.factor(significant))) +
  geom_histogram(binwidth = 0.01, color = "black", alpha = 0.7) + 
  scale_fill_manual(values = c("0" = "lightblue", "1" = "darkblue"), 
                    name = "P-Value <= 0.1") +  
  labs(title = "Distribution of District-Level Estimated Treatment Effects", 
       x = "Treatment Effect", 
       y = "Frequency") +
  xlim(-0.5, 0.5) +  
  geom_vline(aes(xintercept = ela_ate[1]), color = "red", linetype = "dashed", size = 1) + 
  geom_text(aes(x = afgr_ate[1], y = 750, label = paste("ATE =", round(ela_ate[1], 2))), 
            vjust = -0.5, hjust = 1.2, color = "red", size = 5) +
  theme_minimal()

ggsave(filename = file.path(output_path, "figures/cate_dist_district_ela.png"), 
       plot = plot, 
       width = 8, height = 6, dpi = 300)

# Group Average Treatment Effect Table --------------------

# # enter subgroups of interest in this list
# # this is for the old, slow code
# subgroup_conditions <- list(
#   "Urban" = X_matrix[,"urban"] == 1
#   #"Suburban" = X_matrix[,"suburb"] == 1,
#   #"Rural" = X_matrix[,"rural"] == 1,
#   #"Percent Free Lunch > 20%" = X_matrix[,"perfrl"] > 0.20
# )

subgroup_conditions <- list(
  "Urban" = charter_seda_ela[,"urban"] == 1,
  "Suburban" = charter_seda_ela[,"suburb"] == 1,
  "Rural" = charter_seda_ela[,"rural"] == 1,
  "Percent Free Lunch > 20%" = charter_seda_ela[,"perfrl"] > 0.20
)

# initializes the table to store results
gate_results_table <- data.frame(
  Group = character(),
  GATE = numeric(),
  SE = numeric(),
  `p-value` = numeric(),
  `% N` = numeric(),
  stringsAsFactors = FALSE
)

# calculate GATES
for (group_name in names(subgroup_conditions)) {
  
  condition <- subgroup_conditions[[group_name]]
  
  ## this commented code works but is very slow because it re-estimate dr_score for each condition
  # gate_result <- average_treatment_effect(cf_model, 
  #                                         target.sample = "all",
  #                                         method = "AIPW",
  #                                         subset = condition)
  
  # gate_estimate <- gate_result[[1]]
  # gate_se <- gate_result[[2]]
  
  filtered_df <- charter_seda_ela[condition, ]
  
  gate_estimate <- mean(filtered_df$dr_score, na.rm = TRUE)
  gate_se <- sd(filtered_df$dr_score, na.rm = TRUE) / sqrt(nrow(filtered_df))
  
  z_score <- gate_estimate / gate_se
  p_value <- 2 * (1 - pnorm(abs(z_score)))
  
  proportion_N <- mean(condition)
  
  new_row <- data.frame(
    Group = group_name,
    GATE = gate_estimate,
    SE = gate_se,
    `p-value` = p_value,
    `Share of N` = proportion_N,
    stringsAsFactors = FALSE
  )
  
  gate_results_table <- rbind(gate_results_table, new_row)
}

gate_results_table <- xtable(gate_results_table)
print(gate_results_table,
      file = file.path(output_path, "/tables/gates_table_ela.tex"),
      include.rownames = TRUE,
      floating = FALSE)

# GATE table within states (ELA) ------------------

# Initialize the table to store results
state_results_table <- data.frame(
  State = character(),
  GATE = numeric(),
  SE = numeric(),
  `p-value` = numeric(),
  `Share of N` = numeric(),
  stringsAsFactors = FALSE
)

# Get unique state names
unique_states <- unique(charter_seda_ela$statename)

# Loop through each state and calculate the required statistics
for (state in unique_states) {
  
  condition <- charter_seda_ela$statename == state
  filtered_df <- charter_seda_ela[condition, ]
  
  gate_estimate <- mean(filtered_df$dr_score, na.rm = TRUE)
  gate_se <- sd(filtered_df$dr_score, na.rm = TRUE) / sqrt(nrow(filtered_df))
  
  z_score <- gate_estimate / gate_se
  p_value <- 2 * (1 - pnorm(abs(z_score)))
  
  proportion_N <- mean(condition)
  
  # Determine significance stars
  stars <- ifelse(p_value < 0.01, "***",
                  ifelse(p_value < 0.05, "**",
                         ifelse(p_value < 0.1, "*", "")))
  
  # Create GATE with stars
  gate_with_stars <- paste0(round(gate_estimate, 3), stars)
  
  new_row <- data.frame(
    State = state,
    GATE = gate_with_stars,
    SE = round(gate_se, 3),
    `p-value` = round(p_value, 3),
    `Share of N` = round(proportion_N, 3),
    stringsAsFactors = FALSE
  )
  
  state_results_table <- rbind(state_results_table, new_row)
}

# Create the LaTeX table
kable(state_results_table, 
      caption = "GATEs within States (ELA Scores)",
      format = "latex", booktabs = TRUE, 
      linesep = "", escape = FALSE, label = "state_gates_ela") %>%
  kable_styling(latex_options = c("striped", "hold_position")) %>%
  save_kable(file = file.path(output_path, "tables/state_gates_table_ela.tex"))


# GATE bar graph at different dosage levels (ELA) -----------------------

# Step 1: Calculate the treatment_dose_change variable and create a new column for the lag of lag_share
charter_seda_ela_dosage <- charter_seda_ela %>%
  group_by(district) %>%
  arrange(district, stateyear) %>%
  mutate(
    lag_lag_share = Hmisc::Lag(lag_share),
    treatment_dose_change = lag_share - lag_lag_share
  ) %>%
  ungroup()

# Step 2: Filter the data to include only units where treatment_dose_change is non-zero
charter_seda_ela_dosage <- charter_seda_ela_dosage %>%
  filter(treatment_dose_change != 0)

# Step 3: Group into deciles based on the lag of lag_share
charter_seda_ela_dosage <- charter_seda_ela_dosage %>%
  mutate(lag_share_decile = ntile(lag_lag_share, 10))

# Calculate decile thresholds
decile_thresholds <- quantile(charter_seda_ela_dosage$lag_lag_share, 
                              probs = seq(0, 1, 0.1), na.rm = TRUE)

# Step 4: Estimate GATEs by average CATE predictions from the current year
decile_results_table <- data.frame(
  Decile = integer(),
  GATE = numeric(),
  SE = numeric(),
  `p-value` = numeric(),
  stringsAsFactors = FALSE
)

for (decile in 1:10) {
  
  condition <- charter_seda_ela_dosage$lag_share_decile == decile
  filtered_df <- charter_seda_ela_dosage[condition, ]
  
  gate_estimate <- mean(filtered_df$dr_score, na.rm = TRUE)
  gate_se <- sd(filtered_df$dr_score, na.rm = TRUE) / sqrt(nrow(filtered_df))
  
  z_score <- gate_estimate / gate_se
  p_value <- 2 * (1 - pnorm(abs(z_score)))
  
  new_row <- data.frame(
    Decile = decile,
    GATE = gate_estimate,
    SE = gate_se,
    `p-value` = p_value,
    stringsAsFactors = FALSE
  )
  
  decile_results_table <- rbind(decile_results_table, new_row)
}

# Step 5: Plot the bar graph
plot <- ggplot(decile_results_table, aes(x = factor(Decile, labels = round(decile_thresholds[-1], 2)), y = GATE)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_errorbar(aes(ymin = GATE - SE, ymax = GATE + SE), width = 0.2) +
  labs(x = "Lag Share Decile Threshold", y = "GATE", 
       title = "GATE within Different Deciles of Lag Share (ELA)") +
  theme_minimal()

# Save the plot
ggsave(filename = file.path(output_path, "figures/gate_deciles_ela.png"), 
       plot = plot, 
       width = 8, height = 6, dpi = 300)









































