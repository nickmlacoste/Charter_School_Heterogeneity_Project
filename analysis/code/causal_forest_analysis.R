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
library(broom)

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
                  "p_rev", "p_exp", "charter_exp", "str", "tea_salary", "num_magnet",
                  "napcs14_nocaps", "napcs14_perf", "napcs14_transpar",
                  "napcs14_non_renew", "napcs14_exem", "napcs14_eq_funding",
                  "zcoolcitypop", "zcoolcityuniv","w_pp_exp_percdiff", "L.afgr")

# remove rows with missing data for Y or W
charter_afgr2_clean <- charter_afgr2 %>%
  filter(!is.na(afgr) & !is.na(inter))

# global within transformation
charter_afgr2_panel <- pdata.frame(charter_afgr2_clean,
                                   index = c("district","stateyear"))

# # Transforms the X-matrix: this block will ignore NAs when calculatng group mean 

# within_transform <- function(var, group1, group2) {
#   # First, de-mean within the primary group (e.g., district)
#   demeaned_var <- var - ave(var, group1, FUN = function(x) mean(x, na.rm = TRUE))
#   # Then, de-mean within the secondary group (e.g., year), ignoring NAs
#   demeaned_var <- demeaned_var - ave(demeaned_var, group2, FUN = function(x) mean(x, na.rm = TRUE))
#   
#   return(demeaned_var)
# }
# 
# # Apply the transformation to each variable in X_covariates
# X_matrix <- as.data.frame(lapply(X_covariates, function(var) {
#   within_transform(charter_afgr2_panel[[var]], 
#                    charter_afgr2_panel$district, 
#                    charter_afgr2_panel$year)
# }))

## This block will not ignore NAs when calculating group means:

# X_matrix <- as.data.frame(lapply(X_covariates, function(var) {
#   Within(charter_afgr2_panel[[var]], effect = "twoway")
# }))

X_matrix <- as.matrix(charter_afgr2_panel[X_covariates])
colnames(X_matrix) <- X_covariates
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
  
  cat("Training Causal Forest -- Graduation Rates. \n")
  
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
  
  cat("Causal Forest Trained -- Graduation Rates. \n")
  
  # Save the causal forest model
  saveRDS(cf_model, file = paste0(cf_output_path, "/cf_model_afgr.rda"))
  
} else {
  
  # Load the previously saved model
  cf_model <- readRDS(file.path(cf_output_path, "cf_model_afgr.rda"))
  
  cat("Causal Forest Imported -- Graduation Rates. \n")
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
  perwht = "White (%)",
  perblk = "Black (%)",
  perhsp = "Hispanic (%)",
  perfrl = "Free/Reduced Lunch (%)",
  perspeced = "Special Ed (%)",
  L.afgr = "Baseline Performance",
  urban = "Urban",
  suburb = "Suburb",
  town = "Town",
  rural = "Rural",
  num_magnet = "Magnet Schools (%)",
  zcoolcitypop = "City Population (standardized)",
  zcoolcityuniv = "University in City",
  p_rev = "Per Pupil Revenue",
  str = "Student-Teacher Ratio",
  tea_salary = "Teacher Salary",
  w_pp_exp_percdiff = "TPS-Charter Spending (% diff)",
  p_exp = "Total Spending (per-pupil)",
  charter_exp = "Charter Spending (per-pupil)",
  napcs14_eq_funding = "Equitable Funding",
  napcs14_nocaps = "No Caps on CS Growth",
  napcs14_perf = "Performance-Based Contracts",
  napcs14_transpar = "Transparent Charter Startup Policies",
  napcs14_non_renew = "Clear Charter Renewal Policies",
  napcs14_exem = "Exempt from State/District Regs"
)

covariate_names_latex <- lapply(covariate_names, 
                                function(x) gsub("%", "\\%", x, fixed = TRUE))

vif_df <- data.frame(
  Variable = X_covariates,
  VIF_Score = as.vector(afgr_vif)
)

vif_df$Variable <- covariate_names[vif_df$Variable]

plot <- ggplot(vif_df, aes(x = reorder(Variable, VIF_Score), y = VIF_Score)) +
  geom_bar(stat = "identity", fill = "steelblue") +    
  coord_flip() +                                       
  labs(#title = "Variable Importance (VIF Scores) - Graduation Rates", 
       x = "Variable", 
       y = "Variable Importance Score") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14),   
    axis.title.y = element_text(size = 14),   
    axis.text.x = element_text(size = 12),    
    axis.text.y = element_text(size = 12),    
    plot.title = element_text(size = 16, face = "bold")  
  )

ggsave(filename = file.path(output_path, "/figures/vif_scores_afgr.png"), plot = plot, 
       width = 8, height = 6, dpi = 300)





# best linear projection  --------------------

# (regresses T(X) = B_0 + A B_1, i.e. regress top 10 VIF variables on treatment
# effect to see which ones are associated with higher/lower treatment effects
afgr_blp <- best_linear_projection(cf_model, X_matrix[,afgr_ranked_vars[1:10]])
afgr_blp_df <- tidy(afgr_blp)

# Add significance stars based on p-values
afgr_blp_df <- afgr_blp_df %>%
  mutate(
    stars = ifelse(p.value < 0.01, "***",
                   ifelse(p.value < 0.05, "**",
                          ifelse(p.value < 0.1, "*", ""))),
    Estimate_with_stars = paste0(round(estimate, 3), stars),
    std.error = round(std.error, 3),
    statistic = round(statistic, 3),
    p.value = round(p.value, 3)
  )

# change the variable names to make them descriptive
afgr_blp_df <- afgr_blp_df %>%
  mutate(term = ifelse(term == "(Intercept)", "(Intercept)", covariate_names_latex[term]))

# Select and rename columns for LaTeX table
afgr_blp_table <- afgr_blp_df %>%
  select(
    Term = term,
    `Estimate` = Estimate_with_stars,
    `Std. Error` = std.error,
    `t-stat` = statistic,
    `p-value` = p.value
  )

# Create the LaTeX table with kable
kable(afgr_blp_table, 
      caption = "Best Linear Projection: Graduation Rates",
      format = "latex", booktabs = TRUE, 
      align = "lcccc",
      linesep = "", escape = FALSE, label = "blp_afgr") %>%
  kable_styling(latex_options = c("striped", "hold_position")) %>%
  save_kable(file = file.path(output_path, "tables/blp_table_afgr.tex"))


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
library(broom)

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
                  "p_rev", "p_exp", "charter_exp", "str", "tea_salary", "num_magnet",
                  "napcs14_nocaps", "napcs14_perf", "napcs14_transpar",
                  "napcs14_non_renew", "napcs14_exem", "napcs14_eq_funding",
                  "zcoolcitypop", "zcoolcityuniv", "w_pp_exp_percdiff", "L.st_math")

# remove rows with missing data for Y or W
charter_seda_math <- charter_seda %>%
  filter(!is.na(st_math) & !is.na(inter))

# global within transformation
charter_seda_math_panel <- pdata.frame(charter_seda_math,
                                   index = c("district","sgyear"))

# X_matrix <- as.data.frame(lapply(X_covariates, function(var) {
#   Within(charter_seda_math_panel[[var]], effect = "twoway")
# }))
X_matrix <- as.matrix(charter_seda_math_panel[X_covariates])
colnames(X_matrix) <- X_covariates
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
  
  cat("Training Causal Forest -- Math Scores. \n")
  
  # Estimate the causal forest model
  cf_model <- causal_forest(X = X_matrix,
                            Y = Y_vector,
                            W = W_vector,
                            Y.hat = NULL,
                            W.hat = NULL,
                            num.trees = 100,
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
  
  cat("Causal Forest Trained -- Math Scores. \n")
  
} else {
  
  # Load the previously saved model
  cf_model <- readRDS(file.path(cf_output_path, "cf_model_math.rda"))
  
  cat("Causal Forest Imported -- Math Scores. \n")
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

# variable importance factors plot (math) ----------------
math_vif <- variable_importance(cf_model)
math_ranked_vars <- order(math_vif, decreasing = TRUE)

covariate_names <- c(
  logenroll = "Log of Enrollment",
  perwht = "White (%)",
  perblk = "Black (%)",
  perhsp = "Hispanic (%)",
  perfrl = "Free/Reduced Lunch (%)",
  perspeced = "Special Ed (%)",
  L.st_math = "Baseline Performance",
  urban = "Urban",
  suburb = "Suburb",
  town = "Town",
  rural = "Rural",
  num_magnet = "Magnet Schools (%)",
  zcoolcitypop = "City Population (standardized)",
  zcoolcityuniv = "University in City",
  p_rev = "Per Pupil Revenue",
  str = "Student-Teacher Ratio",
  tea_salary = "Teacher Salary",
  w_pp_exp_percdiff = "TPS-Charter Spending (% diff)",
  p_exp = "Total Spending (per-pupil)",
  charter_exp = "Charter Spending (per-pupil)",
  napcs14_eq_funding = "Equitable Funding",
  napcs14_nocaps = "No Caps on CS Growth",
  napcs14_perf = "Performance-Based Contracts",
  napcs14_transpar = "Transparent Charter Startup Policies",
  napcs14_non_renew = "Clear Charter Renewal Policies",
  napcs14_exem = "Exempt from State/District Regs"
)

covariate_names_latex <- lapply(covariate_names, 
                                function(x) gsub("%", "\\%", x, fixed = TRUE))

vif_df <- data.frame(
  Variable = X_covariates,
  VIF_Score = as.vector(math_vif)
)

vif_df$Variable <- covariate_names[vif_df$Variable]

plot <- ggplot(vif_df, aes(x = reorder(Variable, VIF_Score), y = VIF_Score)) +
  geom_bar(stat = "identity", fill = "steelblue") +    
  coord_flip() +                                       
  labs(#title = "Variable Importance (VIF Scores) - Math", 
       x = "Variable", 
       y = "Variable Importance Score") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14),   
    axis.title.y = element_text(size = 14),   
    axis.text.x = element_text(size = 12),    
    axis.text.y = element_text(size = 12),    
    plot.title = element_text(size = 16, face = "bold")  
  )

ggsave(filename = file.path(output_path, "/figures/vif_scores_math.png"), plot = plot, 
       width = 8, height = 6, dpi = 300)

# best linear projection (math) --------------------

# (regresses T(X) = B_0 + A B_1, i.e. regress top 10 VIF variables on treatment
# effect to see which ones are associated with higher/lower treatment effects
math_blp <- best_linear_projection(cf_model, X_matrix[,math_ranked_vars[1:10]])
math_blp_df <- tidy(math_blp)

# Add significance stars based on p-values
math_blp_df <- math_blp_df %>%
  mutate(
    stars = ifelse(p.value < 0.01, "***",
                   ifelse(p.value < 0.05, "**",
                          ifelse(p.value < 0.1, "*", ""))),
    Estimate_with_stars = paste0(round(estimate, 3), stars),
    std.error = round(std.error, 3),
    statistic = round(statistic, 3),
    p.value = round(p.value, 3)
  )

# change the variable names to make them descriptive
math_blp_df <- math_blp_df %>%
  mutate(term = ifelse(term == "(Intercept)", "(Intercept)", covariate_names_latex[term]))

# Select and rename columns for LaTeX table
math_blp_table <- math_blp_df %>%
  select(
    Term = term,
    `Estimate` = Estimate_with_stars,
    `Std. Error` = std.error,
    `t-stat` = statistic,
    `p-value` = p.value
  )

# Create the LaTeX table with kable
kable(math_blp_table, 
      caption = "Best Linear Projection: Math Scores",
      format = "latex", booktabs = TRUE, 
      align = "lcccc",
      linesep = "", escape = FALSE, label = "blp_math") %>%
  kable_styling(latex_options = c("striped", "hold_position")) %>%
  save_kable(file = file.path(output_path, "tables/blp_table_math.tex"))


# Estimate the Causal Forest on test score data (ELA) ------------------------

X_covariates <- c("logenroll", "perwht", "perblk", "perhsp", "perfrl", 
                  "perspeced", "urban", "suburb", "town", "rural", 
                  "p_rev", "p_exp", "charter_exp", "str", "tea_salary", "num_magnet",
                  "napcs14_nocaps", "napcs14_perf", "napcs14_transpar",
                  "napcs14_non_renew", "napcs14_exem", "napcs14_eq_funding",
                  "zcoolcitypop", "zcoolcityuniv", "w_pp_exp_percdiff", "L.st_ela")

charter_seda_ela <- charter_seda %>%
  filter(!is.na(st_ela) & !is.na(inter))

# global within transformation
charter_seda_ela_panel <- pdata.frame(charter_seda_ela,
                                       index = c("district","sgyear"))

# X_matrix <- as.data.frame(lapply(X_covariates, function(var) {
#   Within(charter_seda_ela_panel[[var]], effect = "twoway")
# }))
X_matrix <- as.matrix(charter_seda_ela_panel[X_covariates])
colnames(X_matrix) <- X_covariates
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
  
  cat("Training Causal Forest -- ELA Scores. \n")
  
  # Estimate the causal forest model
  cf_model <- causal_forest(X = X_matrix,
                            Y = Y_vector,
                            W = W_vector,
                            Y.hat = NULL,
                            W.hat = NULL,
                            num.trees = 100,
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
  
  cat("Causal Forest Trained -- ELA Scores. \n")
  
} else {
  
  # Load the previously saved model
  cf_model <- readRDS(file.path(cf_output_path, "cf_model_ela.rda"))
  
  cat("Causal Forest Imported -- ELA Scores. \n")
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

# variable importance factors plot (ela) ----------------
ela_vif <- variable_importance(cf_model)
ela_ranked_vars <- order(ela_vif, decreasing = TRUE)

covariate_names <- c(
  logenroll = "Log of Enrollment",
  perwht = "White (%)",
  perblk = "Black (%)",
  perhsp = "Hispanic (%)",
  perfrl = "Free/Reduced Lunch (%)",
  perspeced = "Special Ed (%)",
  L.st_ela = "Baseline Performance",
  urban = "Urban",
  suburb = "Suburb",
  town = "Town",
  rural = "Rural",
  num_magnet = "Magnet Schools (%)",
  zcoolcitypop = "City Population (standardized)",
  zcoolcityuniv = "University in City",
  p_rev = "Per Pupil Revenue",
  str = "Student-Teacher Ratio",
  tea_salary = "Teacher Salary",
  w_pp_exp_percdiff = "TPS-Charter Spending (% diff)",
  p_exp = "Total Spending (per-pupil)",
  charter_exp = "Charter Spending (per-pupil)",
  napcs14_eq_funding = "Equitable Funding",
  napcs14_nocaps = "No Caps on CS Growth",
  napcs14_perf = "Performance-Based Contracts",
  napcs14_transpar = "Transparent Charter Startup Policies",
  napcs14_non_renew = "Clear Charter Renewal Policies",
  napcs14_exem = "Exempt from State/District Regs"
)

covariate_names_latex <- lapply(covariate_names, 
                                function(x) gsub("%", "\\%", x, fixed = TRUE))

vif_df <- data.frame(
  Variable = X_covariates,
  VIF_Score = as.vector(ela_vif)
)

vif_df$Variable <- covariate_names[vif_df$Variable]

plot <- ggplot(vif_df, aes(x = reorder(Variable, VIF_Score), y = VIF_Score)) +
  geom_bar(stat = "identity", fill = "steelblue") +    
  coord_flip() +                                       
  labs(#title = "Variable Importance (VIF Scores) - ELA", 
       x = "Variable", 
       y = "Variable Importance Score") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14),   
    axis.title.y = element_text(size = 14),   
    axis.text.x = element_text(size = 12),    
    axis.text.y = element_text(size = 12),    
    plot.title = element_text(size = 16, face = "bold")  
  )

ggsave(filename = file.path(output_path, "/figures/vif_scores_ela.png"), plot = plot, 
       width = 8, height = 6, dpi = 300)

# best linear projection (ela)  --------------------

# (regresses T(X) = B_0 + A B_1, i.e. regress top 10 VIF variables on treatment
# effect to see which ones are associated with higher/lower treatment effects
ela_blp <- best_linear_projection(cf_model, X_matrix[,ela_ranked_vars[1:10]])
ela_blp_df <- tidy(ela_blp)

# Add significance stars based on p-values
ela_blp_df <- ela_blp_df %>%
  mutate(
    stars = ifelse(p.value < 0.01, "***",
                   ifelse(p.value < 0.05, "**",
                          ifelse(p.value < 0.1, "*", ""))),
    Estimate_with_stars = paste0(round(estimate, 3), stars),
    std.error = round(std.error, 3),
    statistic = round(statistic, 3),
    p.value = round(p.value, 3)
  )

# change the variable names to make them descriptive
ela_blp_df <- ela_blp_df %>%
  mutate(term = ifelse(term == "(Intercept)", "(Intercept)", covariate_names_latex[term]))

# Select and rename columns for LaTeX table
ela_blp_table <- ela_blp_df %>%
  select(
    Term = term,
    `Estimate` = Estimate_with_stars,
    `Std. Error` = std.error,
    `t-stat` = statistic,
    `p-value` = p.value
  )

# Create the LaTeX table with kable
kable(ela_blp_table, 
      caption = "Best Linear Projection: ELA Scores",
      format = "latex", booktabs = TRUE, 
      align = "lcccc",
      linesep = "", escape = FALSE, label = "blp_ela") %>%
  kable_styling(latex_options = c("striped", "hold_position")) %>%
  save_kable(file = file.path(output_path, "tables/blp_table_ela.tex"))
