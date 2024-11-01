# Intro -----------------------------

"
This script imports the causal forest estimates of the CATEs and does analysis.
"

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

# Import data with predictions
charter_afgr2 <- read.csv(file.path(data_path, "afgr_post_ML.csv"))
charter_seda_math <- read.csv(file.path(data_path, "math_post_ML.csv"))
charter_seda_ela <- read.csv(file.path(data_path, "ela_post_ML.csv"))

# # Import causal forests
# cf_model_afgr <- readRDS(file.path(cf_output_path, "cf_model_afgr.rda"))
# cf_model_math <- readRDS(file.path(cf_output_path, "cf_model_math.rda"))
# cf_model_ela <- readRDS(file.path(cf_output_path, "cf_model_ela.rda"))

# pull-in ATEs from causal forests ---------------------

ATEs <- read.csv(file.path(data_path, "cf_ATE_estimates.csv"))

# Distribution of treatment effects plots (district x year estimates) -------------------

# Graduation Rates
plot <- ggplot(charter_afgr2, aes(x = predictions, fill = as.factor(significant))) +
  geom_histogram(binwidth = 0.1, color = "black", alpha = 0.7) + 
  scale_fill_manual(values = c("0" = "lightblue", "1" = "darkblue"), 
                    name = "P-Value <= 0.1") +  
  labs(title = "Distribution of Estimated Treatment Effects -- Graduation Rates", 
       x = "Treatment Effect", 
       y = "Frequency") +
  xlim(-0.5, 1) +  
  geom_vline(aes(xintercept = ATEs$ATE[1]), color = "red", linetype = "dashed", size = 1) + 
  geom_text(aes(x = ATEs$ATE[1], y = 20000, label = paste("ATE =", round(ATEs$ATE[1], 2))), 
            vjust = -0.5, hjust = 1.2, color = "red", size = 5) +
  theme_minimal()

ggsave(filename = file.path(output_path, "/figures/cate_dist_afgr.png"), plot = plot, 
       width = 8, height = 6, dpi = 300)

# Math scores
plot <- ggplot(charter_seda_math, aes(x = predictions, fill = as.factor(significant))) +
  geom_histogram(binwidth = 0.1, color = "black", alpha = 0.7) + 
  scale_fill_manual(values = c("0" = "lightblue", "1" = "darkblue"), 
                    name = "P-Value <= 0.1") +  
  labs(title = "Distribution of Estimated Treatment Effects -- Math", 
       x = "Treatment Effect", 
       y = "Frequency") +
  xlim(-2, 2) +  
  geom_vline(aes(xintercept = ATEs$ATE[2]), color = "red", linetype = "dashed", size = 1) + 
  geom_text(aes(x = ATEs$ATE[2], y = 20000, label = paste("ATE =", round(ATEs$ATE[2], 2))), 
            vjust = -0.5, hjust = 1.2, color = "red", size = 5) +
  theme_minimal()

ggsave(filename = file.path(output_path, "/figures/cate_dist_math.png"), plot = plot, 
       width = 8, height = 6, dpi = 300)

# ELA scores
plot <- ggplot(charter_seda_ela, aes(x = predictions, fill = as.factor(significant))) +
  geom_histogram(binwidth = 0.1, color = "black", alpha = 0.7) + 
  scale_fill_manual(values = c("0" = "lightblue", "1" = "darkblue"), 
                    name = "P-Value <= 0.1") +  
  labs(title = "Distribution of Estimated Treatment Effects -- ELA", 
       x = "Treatment Effect", 
       y = "Frequency") +
  xlim(-1.5, 1.5) +  
  geom_vline(aes(xintercept = ATEs$ATE[3]), color = "red", linetype = "dashed", size = 1) + 
  geom_text(aes(x = ATEs$ATE[3], y = 20000, label = paste("ATE =", round(ATEs$ATE[3], 2))), 
            vjust = -0.5, hjust = 1.2, color = "red", size = 5) +
  theme_minimal()

ggsave(filename = file.path(output_path, "/figures/cate_dist_ela.png"), plot = plot, 
       width = 8, height = 6, dpi = 300)

# Distribution of treatment effects plots (district-level aggregated) -----------------------

# aggregate treatment effects to district level
district_cates_afgr <- charter_afgr2 %>%
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

district_cates_math <- charter_seda_math %>%
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

district_cates_ela <- charter_seda_ela %>%
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

# Graduation Rates:
plot <- ggplot(district_cates_afgr, aes(x = mean_prediction, fill = as.factor(significant))) +
  geom_histogram(binwidth = 0.01, color = "black", alpha = 0.7) + 
  scale_fill_manual(values = c("0" = "lightblue", "1" = "darkblue"), 
                    name = "P-Value <= 0.1") +  
  labs(title = "Distribution of District-Level Estimated Treatment Effects", 
       x = "Treatment Effect", 
       y = "Frequency") +
  xlim(-0.25, 0.5) +  
  geom_vline(aes(xintercept = ATEs$ATE[1]), color = "red", linetype = "dashed", size = 1) + 
  geom_text(aes(x = ATEs$ATE[1], y = 600, label = paste("ATE =", round(ATEs$ATE[1], 2))), 
            vjust = -0.5, hjust = 1.2, color = "red", size = 5) +
  theme_minimal()

ggsave(filename = file.path(output_path, "figures/cate_dist_district_afgr.png"), 
       plot = plot, 
       width = 8, height = 6, dpi = 300)

# Math Scores:
plot <- ggplot(district_cates_math, aes(x = mean_prediction, fill = as.factor(significant))) +
  geom_histogram(binwidth = 0.01, color = "black", alpha = 0.7) + 
  scale_fill_manual(values = c("0" = "lightblue", "1" = "darkblue"), 
                    name = "P-Value <= 0.1") +  
  labs(title = "Distribution of District-Level Estimated Treatment Effects", 
       x = "Treatment Effect", 
       y = "Frequency") +
  xlim(-0.5, 0.5) +  
  geom_vline(aes(xintercept = ATEs$ATE[2]), color = "red", linetype = "dashed", size = 1) + 
  geom_text(aes(x = ATEs$ATE[2], y = 300, label = paste("ATE =", round(ATEs$ATE[2], 2))), 
            vjust = -0.5, hjust = 1.2, color = "red", size = 5) +
  theme_minimal()

ggsave(filename = file.path(output_path, "figures/cate_dist_district_math.png"), 
       plot = plot, 
       width = 8, height = 6, dpi = 300)

# ELA Scores:
plot <- ggplot(district_cates_ela, aes(x = mean_prediction, fill = as.factor(significant))) +
  geom_histogram(binwidth = 0.01, color = "black", alpha = 0.7) + 
  scale_fill_manual(values = c("0" = "lightblue", "1" = "darkblue"), 
                    name = "P-Value <= 0.1") +  
  labs(title = "Distribution of District-Level Estimated Treatment Effects", 
       x = "Treatment Effect", 
       y = "Frequency") +
  xlim(-0.5, 0.5) +  
  geom_vline(aes(xintercept = ATEs$ATE[3]), color = "red", linetype = "dashed", size = 1) + 
  geom_text(aes(x = ATEs$ATE[3], y = 300, label = paste("ATE =", round(ATEs$ATE[3], 2))), 
            vjust = -0.5, hjust = 1.2, color = "red", size = 5) +
  theme_minimal()

ggsave(filename = file.path(output_path, "figures/cate_dist_district_ela.png"), 
       plot = plot, 
       width = 8, height = 6, dpi = 300)


# Covariate averages for significantly positive vs. negative districts: ---------------------

X_covariates_afgr <- c("logenroll", "perwht", "perblk", "perhsp", "perfrl", 
                       "perspeced", "urban", "suburb", "town", "rural", 
                       "p_rev", "p_exp", "str", "tea_salary", "num_magnet", "charter_eff",
                       "L.afgr")

X_covariates_math <- c("logenroll", "perwht", "perblk", "perhsp", "perfrl", 
                       "perspeced", "urban", "suburb", "town", "rural", 
                       "p_rev", "p_exp", "str", "tea_salary", "num_magnet", "charter_eff",
                       "L.st_math")
X_covariates_ela <- c("logenroll", "perwht", "perblk", "perhsp", "perfrl", 
                       "perspeced", "urban", "suburb", "town", "rural", 
                       "p_rev", "p_exp", "str", "tea_salary", "num_magnet", "charter_eff",
                       "L.st_ela")

covariate_names_afgr <- c(
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
  charter_eff = "Charter Effectiveness",
  L.afgr = "Baseline Performance"
)

covariate_names_math <- c(
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
  charter_eff = "Charter Effectiveness",
  L.st_math = "Baseline Performance"
)

covariate_names_ela <- c(
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
  charter_eff = "Charter Effectiveness",
  L.st_ela = "Baseline Performance"
)

# Graduation rates:
negative_effects_afgr <- charter_afgr2 %>%
  filter(predictions < 0 & significant == 1)

positive_effects_afgr <- charter_afgr2 %>%
  filter(predictions > 0 & significant == 1)

negative_means_afgr <- negative_effects_afgr %>%
  summarise(across(all_of(X_covariates_afgr), mean, na.rm = TRUE))

positive_means_afgr <- positive_effects_afgr %>%
  summarise(across(all_of(X_covariates_afgr), mean, na.rm = TRUE))

difference_afgr <- round(positive_means_afgr - negative_means_afgr, 2)

n_positive_afgr <- nrow(positive_effects_afgr)
n_negative_afgr <- nrow(negative_effects_afgr)

# Perform t-tests and determine significance levels
p_values <- sapply(X_covariates_afgr, function(var) {
  t_test <- t.test(positive_effects_afgr[[var]], negative_effects_afgr[[var]], var.equal = TRUE)
  t_test$p.value
})

# Add significance stars based on p-values
significance_stars <- ifelse(p_values < 0.01, "***",
                             ifelse(p_values < 0.05, "**",
                                    ifelse(p_values < 0.1, "*", "")))

summary_table <- bind_cols(
  Covariate = covariate_names_afgr, 
  `Significantly \n Positive` = round(as.numeric(positive_means_afgr), 2),
  `Significantly \n Negative` = round(as.numeric(negative_means_afgr), 2),
  `Difference \n (Positive - Negative)` = paste0(difference_afgr, significance_stars)
) %>% 
  add_row(Covariate = "Number of Observations", 
          `Significantly \n Positive` = n_positive_afgr, 
          `Significantly \n Negative` = n_negative_afgr, 
          `Difference \n (Positive - Negative)` = as.character(n_positive_afgr + n_negative_afgr))

kable(summary_table, 
      caption = "Comparing Covariate Means: Graduation Rates",
      format = "latex", booktabs = TRUE, 
      linesep = "", escape = FALSE, label = "cov_means_afgr") %>%
  kable_styling(latex_options = c("striped", "hold_position")) %>%
  row_spec(nrow(summary_table) - 1, hline_after = TRUE) %>%
  save_kable(file = file.path(output_path, "tables/cov_means_table_afgr.tex"))

# Math Scores:
negative_effects_math <- charter_seda_math %>%
  filter(predictions < 0 & significant == 1)

positive_effects_math <- charter_seda_math %>%
  filter(predictions > 0 & significant == 1)

negative_means_math <- negative_effects_math %>%
  summarise(across(all_of(X_covariates_math), mean, na.rm = TRUE))

positive_means_math <- positive_effects_math %>%
  summarise(across(all_of(X_covariates_math), mean, na.rm = TRUE))

difference_math <- round(positive_means_math - negative_means_math, 2)

n_positive_math <- nrow(positive_effects_math)
n_negative_math <- nrow(negative_effects_math)

# Perform t-tests and determine significance levels
p_values <- sapply(X_covariates_math, function(var) {
  t_test <- t.test(positive_effects_math[[var]], negative_effects_math[[var]], var.equal = TRUE)
  t_test$p.value
})

# Add significance stars based on p-values
significance_stars <- ifelse(p_values < 0.01, "***",
                             ifelse(p_values < 0.05, "**",
                                    ifelse(p_values < 0.1, "*", "")))

summary_table <- bind_cols(
  Covariate = covariate_names_math, 
  `Significantly \n Positive` = round(as.numeric(positive_means_math), 2),
  `Significantly \n Negative` = round(as.numeric(negative_means_math), 2),
  `Difference \n (Positive - Negative)` = paste0(difference_math, significance_stars)
) %>% 
  add_row(Covariate = "Number of Observations", 
          `Significantly \n Positive` = n_positive_math, 
          `Significantly \n Negative` = n_negative_math, 
          `Difference \n (Positive - Negative)` = as.character(n_positive_math + n_negative_math))

kable(summary_table, 
      caption = "Comparing Covariate Means: Math",
      format = "latex", booktabs = TRUE, 
      linesep = "", escape = FALSE, label = "cov_means_math") %>%
  kable_styling(latex_options = c("striped", "hold_position")) %>%
  row_spec(nrow(summary_table) - 1, hline_after = TRUE) %>%
  save_kable(file = file.path(output_path, "tables/cov_means_table_math.tex"))


# ELA Scores:
negative_effects_ela <- charter_seda_ela %>%
  filter(predictions < 0 & significant == 1)

positive_effects_ela <- charter_seda_ela %>%
  filter(predictions > 0 & significant == 1)

negative_means_ela <- negative_effects_ela %>%
  summarise(across(all_of(X_covariates_ela), mean, na.rm = TRUE))

positive_means_ela <- positive_effects_ela %>%
  summarise(across(all_of(X_covariates_ela), mean, na.rm = TRUE))

difference_ela <- round(positive_means_ela - negative_means_ela, 2)

n_positive_ela <- nrow(positive_effects_ela)
n_negative_ela <- nrow(negative_effects_ela)

# Perform t-tests and determine significance levels
p_values <- sapply(X_covariates_ela, function(var) {
  t_test <- t.test(positive_effects_ela[[var]], negative_effects_ela[[var]], var.equal = TRUE)
  t_test$p.value
})

# Add significance stars based on p-values
significance_stars <- ifelse(p_values < 0.01, "***",
                             ifelse(p_values < 0.05, "**",
                                    ifelse(p_values < 0.1, "*", "")))

summary_table <- bind_cols(
  Covariate = covariate_names_ela, 
  `Significantly \n Positive` = round(as.numeric(positive_means_ela), 2),
  `Significantly \n Negative` = round(as.numeric(negative_means_ela), 2),
  `Difference \n (Positive - Negative)` = paste0(difference_ela, significance_stars)
) %>% 
  add_row(Covariate = "Number of Observations", 
          `Significantly \n Positive` = n_positive_ela, 
          `Significantly \n Negative` = n_negative_ela, 
          `Difference \n (Positive - Negative)` = as.character(n_positive_ela + n_negative_ela))

# Create the LaTeX table
kable(summary_table, 
      caption = "Comparing Covariate Means: ELA",
      format = "latex", booktabs = TRUE, 
      linesep = "", escape = FALSE, label = "cov_means_ela") %>%
  kable_styling(latex_options = c("striped", "hold_position")) %>%
  row_spec(nrow(summary_table) - 1, hline_after = TRUE) %>%
  save_kable(file = file.path(output_path, "tables/cov_means_table_ela.tex"))


# Group Average Treatment Effect Tables --------------------

# GATEs: Graduation Rates
subgroup_conditions <- list(
  "Urban" = charter_afgr2[,"urban"] == 1,
  "Suburban" = charter_afgr2[,"suburb"] == 1,
  "Rural" = charter_afgr2[,"rural"] == 1,
  "Percent Free Lunch > 20%" = charter_afgr2[,"perfrl"] > 0.20
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

for (group_name in names(subgroup_conditions)) {
  
  condition <- subgroup_conditions[[group_name]]
  
  filtered_df <- charter_afgr2[condition, ]
  
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

# GATEs: Math Scores
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

for (group_name in names(subgroup_conditions)) {
  
  condition <- subgroup_conditions[[group_name]]
  
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


# GATEs: ELA Scores
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

for (group_name in names(subgroup_conditions)) {
  
  condition <- subgroup_conditions[[group_name]]
  
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

# GATE table within states -----------------------------------------------

# Function to calculate GATE with stars and share of N for each dataset
calculate_gate <- function(dataset, state_column, dr_score_column) {
  unique_states <- unique(dataset[[state_column]])
  results <- data.frame(
    State = character(),
    GATE = character(),
    `Share of N` = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (state in unique_states) {
    condition <- dataset[[state_column]] == state
    filtered_df <- dataset[condition, ]
    
    gate_estimate <- mean(filtered_df[[dr_score_column]], na.rm = TRUE)
    gate_se <- sd(filtered_df[[dr_score_column]], na.rm = TRUE) / sqrt(nrow(filtered_df))
    
    z_score <- gate_estimate / gate_se
    p_value <- 2 * (1 - pnorm(abs(z_score)))
    
    proportion_N <- mean(condition)
    
    # Determine significance stars
    stars <- ifelse(p_value < 0.01, "***",
                    ifelse(p_value < 0.05, "**",
                           ifelse(p_value < 0.1, "*", "")))
    
    # Create GATE with stars
    gate_with_stars <- paste0(round(gate_estimate, 3), stars)
    
    # Append to results
    results <- rbind(results, data.frame(
      State = state,
      GATE = gate_with_stars,
      `Share of N` = round(proportion_N, 3),
      stringsAsFactors = FALSE
    ))
  }
  
  return(results)
}

# Calculate GATE for each dataset
gate_afgr <- calculate_gate(charter_afgr2, "statename", "dr_score") %>%
  rename(`Grad Rate` = GATE, `Grad Rate Share` = `Share.of.N`)
gate_math <- calculate_gate(charter_seda_math, "statename", "dr_score") %>%
  rename(`Math` = GATE, `Math Share` = `Share.of.N`)
gate_ela <- calculate_gate(charter_seda_ela, "statename", "dr_score") %>%
  rename(`ELA` = GATE, `ELA Share` = `Share.of.N`)

# Merge results by State
state_results_table <- gate_afgr %>%
  left_join(gate_math, by = "State") %>%
  left_join(gate_ela, by = "State") %>%
  select(
    State,
    `Grad Rate`, `Math`, `ELA`,
    `Grad Rate Share`, `Math Share`, `ELA Share`
  )

# Create the LaTeX table
kable(state_results_table, 
      caption = "GATEs within States",
      format = "latex", booktabs = TRUE, 
      linesep = "", escape = FALSE, label = "state_gates_combined") %>%
  add_header_above(c(" " = 1, "GATE Estimates" = 3, "Proportion of N" = 3)) %>%
  kable_styling(latex_options = c("striped", "hold_position")) %>%
  save_kable(file = file.path(output_path, "tables/state_gates_table.tex"))

# GATE bar graph at different dosage levels (grad rates) -----------------------

# Step 1: Define the custom threshold levels for `lag_lag_share`
thresholds <- c(0.05, 0.1, 0.15, 0.2, 0.25, 0.3)

# Step 2: Create a new column that groups `lag_lag_share` based on these thresholds

# Graduation Rates
charter_afgr2_dosage <- charter_afgr2 %>%
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

# Math Scores
charter_seda_math_dosage <- charter_seda_math %>%
  group_by(district) %>%
  arrange(district, sgyear) %>%
  mutate(
    lag_lag_grade = Hmisc::Lag(lag_grade),
    treatment_dose_change = lag_grade - lag_lag_grade
  ) %>%
  ungroup() %>%
  filter(treatment_dose_change != 0) %>%
  mutate(
    lag_grade_group = cut(
      lag_lag_grade,
      breaks = c(-Inf, thresholds, Inf),
      labels = c("≤5%", "≤10%", "≤15%", "≤20%", "≤25%", "≤30%", ">30%")
    )
  )

# ELA Scores
charter_seda_ela_dosage <- charter_seda_ela %>%
  group_by(district) %>%
  arrange(district, sgyear) %>%
  mutate(
    lag_lag_grade = Hmisc::Lag(lag_grade),
    treatment_dose_change = lag_grade - lag_lag_grade
  ) %>%
  ungroup() %>%
  filter(treatment_dose_change != 0) %>%
  mutate(
    lag_grade_group = cut(
      lag_lag_grade,
      breaks = c(-Inf, thresholds, Inf),
      labels = c("≤5%", "≤10%", "≤15%", "≤20%", "≤25%", "≤30%", ">30%")
    )
  )

# Step 3: Estimate GATEs by average CATE predictions for each threshold group

group_results_table_afgr <- data.frame(
  Group = character(),
  GATE = numeric(),
  SE = numeric(),
  `p-value` = numeric(),
  stringsAsFactors = FALSE
)

group_results_table_math <- data.frame(
  Group = character(),
  GATE = numeric(),
  SE = numeric(),
  `p-value` = numeric(),
  stringsAsFactors = FALSE
)

group_results_table_ela <- data.frame(
  Group = character(),
  GATE = numeric(),
  SE = numeric(),
  `p-value` = numeric(),
  stringsAsFactors = FALSE
)

#Graduation rates
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
  
  group_results_table_afgr <- rbind(group_results_table_afgr, new_row)
}

# Math scores
for (group in unique(charter_seda_math_dosage$lag_grade_group)) {
  group_char <- as.character(group)
  filtered_df <- charter_seda_math_dosage %>%
    filter(lag_grade_group == group_char)
  
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
  
  group_results_table_math <- rbind(group_results_table_math, new_row)
}

# ELA scores
for (group in unique(charter_seda_ela_dosage$lag_grade_group)) {
  group_char <- as.character(group)
  filtered_df <- charter_seda_ela_dosage %>%
    filter(lag_grade_group == group_char)
  
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
  
  group_results_table_ela <- rbind(group_results_table_ela, new_row)
}

# ensure the groups are in ascending order
group_results_table_afgr$Group <- factor(group_results_table_afgr$Group, levels = c("≤5%", "≤10%", "≤15%", "≤20%", "≤25%", "≤30%", ">30%"))
group_results_table_math$Group <- factor(group_results_table_math$Group, levels = c("≤5%", "≤10%", "≤15%", "≤20%", "≤25%", "≤30%", ">30%"))
group_results_table_ela$Group <- factor(group_results_table_ela$Group, levels = c("≤5%", "≤10%", "≤15%", "≤20%", "≤25%", "≤30%", ">30%"))


# Step 4: Plot the bar graphs

# Graduation rates
plot <- ggplot(group_results_table_afgr, aes(x = Group, y = GATE)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_errorbar(aes(ymin = GATE - SE, ymax = GATE + SE), width = 0.2) +
  labs(x = "Lag Share Group Threshold", y = "GATE", title = "GATE within Different Thresholds of Lag Share") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 14)  # Increase x-axis label size
  )

# Save the plot
ggsave(filename = file.path(output_path, "figures/gate_dosage_afgr.png"), 
       plot = plot, 
       width = 8, height = 6, dpi = 300)

# Math scores
plot <- ggplot(group_results_table_math, aes(x = Group, y = GATE)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_errorbar(aes(ymin = GATE - SE, ymax = GATE + SE), width = 0.2) +
  labs(x = "Lag Share Group Threshold", y = "GATE", title = "GATE within Different Thresholds of Lag Share") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 14)  # Increase x-axis label size
  )

# Save the plot
ggsave(filename = file.path(output_path, "figures/gate_dosage_math.png"), 
       plot = plot, 
       width = 8, height = 6, dpi = 300)

# ELA scores
plot <- ggplot(group_results_table_math, aes(x = Group, y = GATE)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_errorbar(aes(ymin = GATE - SE, ymax = GATE + SE), width = 0.2) +
  labs(x = "Lag Share Group Threshold", y = "GATE", title = "GATE within Different Thresholds of Lag Share") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 14)  # Increase x-axis label size
  )

# Save the plot
ggsave(filename = file.path(output_path, "figures/gate_dosage_ela.png"), 
       plot = plot, 
       width = 8, height = 6, dpi = 300)


















