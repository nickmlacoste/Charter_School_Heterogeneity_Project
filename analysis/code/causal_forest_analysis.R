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

# Create any derived variables needed for estimation -----------------------

charter_afgr2 <- charter_afgr2 %>%
  arrange(district, year) %>%  
  group_by(district) %>%       
  mutate(L.afgr = Hmisc::Lag(afgr, 1)) %>%  # Create variable for 1-period lag of graduation rates
  ungroup()



# Estimate Causal Forest on graduation rate data -------------------------

X_covariates <- c("logenroll", "perwht", "perblk", "perhsp", "perfrl", 
                  "perspeced", "urban", "suburb", "town", "rural", 
                  "p_rev", "p_exp", "str", "tea_salary", "num_magnet", "charter_eff",
                  "L.afgr")

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

charter_afgr2_clean <- charter_afgr2_clean %>%
  mutate(index = row_number())
afgr_cates <- afgr_cates %>%
  mutate(index = row_number())
charter_afgr2_clean <- left_join(charter_afgr2_clean, afgr_cates, by = "index")

# save the graduation rates data with CATE predictions
write.csv(charter_afgr2_clean, 
          file = file.path(data_path, "afgr_post_ML.csv"), row.names = FALSE)


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

# GATE bar graph at different dosage levels (grad rates) -----------------------

# Step 1: Define the custom threshold levels for `lag_lag_share`
thresholds <- c(0.05, 0.1, 0.15, 0.2, 0.25, 0.3)

# Step 2: Create a new column that groups `lag_lag_share` based on these thresholds
charter_afgr2_dosage <- charter_afgr2_clean %>%
  group_by(district) %>%
  arrange(district, stateyear) %>%
  mutate(
    lag_lag_share = Hmisc::Lag(lag_share),
    treatment_dose_change = lag_share - lag_lag_share
  ) %>%
  ungroup() %>%
  filter(treatment_dose_change != 0) %>%
  mutate(
    lag_share_group = cut(
      lag_lag_share,
      breaks = c(-Inf, thresholds, Inf),
      labels = c("≤5%", "≤10%", "≤15%", "≤20%", "≤25%", "≤30%", ">30%")
    )
  )

# Step 3: Estimate GATEs by average CATE predictions for each threshold group
group_results_table <- data.frame(
  Group = character(),
  GATE = numeric(),
  SE = numeric(),
  `p-value` = numeric(),
  stringsAsFactors = FALSE
)

for (group in unique(charter_afgr2_dosage$lag_share_group)) {
  filtered_df <- charter_afgr2_dosage %>%
    filter(lag_share_group == group)
  
  gate_estimate <- mean(filtered_df$dr_score, na.rm = TRUE)
  gate_se <- sd(filtered_df$dr_score, na.rm = TRUE) / sqrt(nrow(filtered_df))
  
  z_score <- gate_estimate / gate_se
  p_value <- 2 * (1 - pnorm(abs(z_score)))
  
  new_row <- data.frame(
    Group = as.character(group),
    GATE = gate_estimate,
    SE = gate_se,
    `p-value` = p_value,
    stringsAsFactors = FALSE
  )
  
  group_results_table <- rbind(group_results_table, new_row)
}

# Step 4: Plot the bar graph
plot <- ggplot(group_results_table, aes(x = Group, y = GATE)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_errorbar(aes(ymin = GATE - SE, ymax = GATE + SE), width = 0.2) +
  labs(x = "Lag Share Group Threshold", y = "GATE", title = "GATE within Different Thresholds of Lag Share") +
  theme_minimal()

# Save the plot
ggsave(filename = file.path(output_path, "figures/gate_dosage_afgr.png"), 
       plot = plot, 
       width = 8, height = 6, dpi = 300)



###########                        ################
###########  Test Score results    ################
###########                        ################

#clears the memory of prior results except for the ATE estimate
rm(list = setdiff(ls(), "afgr_ate")) 

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

# Create any derived columns needed for estimation ------------------------

charter_seda <- charter_seda %>%
  arrange(district, year) %>%  
  group_by(district) %>%       
  mutate(L.st_math = Hmisc::Lag(st_math, 1),
         L.st_ela = Hmisc::Lag(st_ela, 1)) %>%  # Create variables for 1-period lags of math and ELA
  ungroup()

# Estimate Causal Forest on test score data (MATH) -------------------------

X_covariates <- c("logenroll", "perwht", "perblk", "perhsp", "perfrl", 
                  "perspeced", "urban", "suburb", "town", "rural", 
                  "p_rev", "p_exp", "str", "tea_salary", "num_magnet", "charter_eff",
                  "L.st_math")

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

charter_seda_math <- charter_seda_math %>%
  mutate(index = row_number())
math_cates <- math_cates %>%
  mutate(index = row_number())
charter_seda_math <- left_join(charter_seda_math, math_cates, by = "index")

write.csv(charter_seda_math, 
          file = file.path(data_path, "math_post_ML.csv"), row.names = FALSE)


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
                  "p_rev", "p_exp", "str", "tea_salary", "num_magnet", "charter_eff",
                  "L.st_ela")

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

charter_seda_ela <- charter_seda_ela %>%
  mutate(index = row_number())
ela_cates <- ela_cates %>%
  mutate(index = row_number())
charter_seda_ela <- left_join(charter_seda_ela, ela_cates, by = "index")

write.csv(charter_seda_ela, 
          file = file.path(data_path, "ela_post_ML.csv"), row.names = FALSE)

# causal forest doubly-robust estimate of the average treatment effect:
ela_ate <- average_treatment_effect(cf_model,
                                     target.sample = "all",
                                     method = "AIPW")

# save the causal forest ATE estimates:
cf_ATE_estimates <- data.frame(
  model = c("afgr", "math", "ela"),
  ATE = c(afgr_ate["estimate"], math_ate["estimate"], ela_ate["estimate"]),
  SE = c(afgr_ate["std.err"], math_ate["std.err"], ela_ate["std.err"])
)

write.csv(cf_ATE_estimates, 
          file = file.path(data_path, "cf_ATE_estimates.csv"), row.names = FALSE)

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









































