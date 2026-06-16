#Policy Objective: Government grants subsidising consulting and software adoption to spur local economic growth.
# Author: Umer Zeeshan Ijaz (http://userweb.eng.gla.ac.uk/umer.ijaz)

# 1. ARCHITECTURE & DEPENDENCY MANAGEMENT
required_packages <- c("grf", "policytree", "ggplot2", "scales", "patchwork", "lmtest", "sandwich")
invisible(lapply(required_packages, function(pkg) {
  if(!require(pkg, character.only=TRUE, quiet=TRUE)) {
    install.packages(pkg, quiet=TRUE)
    library(pkg, character.only=TRUE)
  }
}))

set.seed(2026)

# STEP 2: METADATA & CONFIGURATION PANEL 
program_cost    <- 10000   # Cost of digital transformation consulting/software
budget_capacity <- 0.10    # 10% adoption rate due to limited budget

outcome_var   <- "Metric_Y"
treatment_var <- "Action_D"
covariates    <- c("Feature_BusinessAge", "Feature_Sector", "Feature_NumEmployees", "Feature_CurrentTechLevel")

N_samples <- 1500
Feature_BusinessAge <- round(runif(N_samples, 1, 40))
Feature_Sector <- sample(c("Retail", "Manufacturing", "Services"), N_samples, replace = TRUE)
Feature_NumEmployees <- rpois(N_samples, 10)
Feature_CurrentTechLevel <- sample(c("Low", "Medium", "High"), N_samples, replace = TRUE)

propensity <- 1 / (1 + exp(-(-1.5 + 0.1 * Feature_NumEmployees)))
Action_D <- rbinom(N_samples, 1, propensity)

true_effect <- 5000 + (1000 * Feature_NumEmployees) + ifelse(Feature_CurrentTechLevel == "Low", 3000, 0)
Metric_Y <- 50000 + (500 * Feature_BusinessAge) + (Action_D * true_effect) + rnorm(N_samples, 0, 5000)

raw_data <- data.frame(Metric_Y, Action_D, Feature_BusinessAge, Feature_Sector = as.factor(Feature_Sector), 
                       Feature_NumEmployees, Feature_CurrentTechLevel = as.factor(Feature_CurrentTechLevel))
cat("Business Digitalization Cohort Loaded.\n")


# STEP 3: PRE-FLIGHT TELEMETRY & PRODUCTION DATA QUALITY GATE

pre_flight_telemetry <- function(data, outcome, treatment, covs) {
  cat("\nExecuting Data Quality Gate Pre-Flight Diagnostics...\n")
  
  missing_keys <- c(outcome, treatment, covs)[!c(outcome, treatment, covs) %in% names(data)]
  if(length(missing_keys) > 0) {
    stop(sprintf("CRITICAL STRUCTURAL MISMATCH: The following configured keys are missing from the dataset: [%s].", 
                 paste(missing_keys, collapse = ", ")))
  }
  
  initial_rows <- nrow(data)
  data <- data[complete.cases(data[, c(outcome, treatment, covs)]), ]
  dropped_rows <- initial_rows - nrow(data)
  if(dropped_rows > 0) {
    cat(sprintf("  ℹ Notice: Purged %d incomplete observation rows containing NA values.\n", dropped_rows))
  }
  
  data[[treatment]] <- as.integer(data[[treatment]])
  unique_treatments <- unique(data[[treatment]])
  if(!all(unique_treatments %in% c(0, 1)) || length(unique_treatments) != 2) {
    stop("CRITICAL TYPING FAULT: Treatment vector must contain exactly binary integers (0 or 1).")
  }
  
  for(col in covs) {
    if(is.character(data[[col]]) || is.logical(data[[col]])) {
      data[[col]] <- as.factor(data[[col]])
    }
  }
  
  for(col in covs) {
    if(length(unique(data[[col]])) < 2) {
      stop(sprintf("CRITICAL VARIANCE FAILURE: Covariate [%s] exhibits zero variance.", col))
    }
  }
  
  cat(sprintf("✔ Pre-Flight Complete: %d observations validated. Initializing Causal Engine.\n\n", nrow(data)))
  return(data)
}

raw_data <- pre_flight_telemetry(raw_data, outcome_var, treatment_var, covariates)


# STEP 4: COVARIATE MATRIX PROCESSING & MODEL FITTING

Y <- as.numeric(raw_data[[outcome_var]])
D <- as.integer(raw_data[[treatment_var]])

X_formula <- as.formula(paste("~", paste(covariates, collapse = " + "), "- 1"))
X_matrix  <- model.matrix(X_formula, data = raw_data)

# Fit Core Structural Elements
c_forest <- causal_forest(X_matrix, Y, D, num.trees = 750, seed = 2026)
p_forest <- regression_forest(X_matrix, D, num.trees = 500, seed = 2026)

cf_predictions <- predict(c_forest, X_matrix, estimate.variance = TRUE)
raw_data$CATE_Estimate    <- cf_predictions$predictions
raw_data$CATE_SE          <- sqrt(cf_predictions$variance.estimates)
raw_data$Propensity_Score <- predict(p_forest)$predictions


# STEP 5: REGIME OPTIMIZATION MECHANICS

total_slots <- round(nrow(raw_data) * budget_capacity)

# Regime A: Resource Lottery
raw_data$Decision_Lottery <- 0
raw_data$Decision_Lottery[sample(1:nrow(raw_data), total_slots)] <- 1

# Regime B: AI Greedy Budget Maximizer
raw_data <- raw_data[order(-raw_data$CATE_Estimate), ]
raw_data$Decision_AI_Greedy <- 0
raw_data$Decision_AI_Greedy[1:total_slots] <- 1
raw_data <- raw_data[order(as.numeric(rownames(raw_data))), ] 

# Regime C: AI Double-Robust Optimal Policy Tree (Depth-2)
dr_scores     <- double_robust_scores(c_forest)
reward_matrix <- cbind(Control = dr_scores[, 1], Treated = dr_scores[, 2] - program_cost)
optimal_tree  <- policy_tree(X = X_matrix, Gamma = reward_matrix, depth = 2)
raw_data$Decision_AI_Tree <- ifelse(predict(optimal_tree, X_matrix) == 2, 1, 0)


# STEP 6: ADVANCED INFERENCE & STATISTICAL AUDITING MODULE

cat("====================================================================\n")
cat("STATISTICAL INFERENCE ENGINE: REGIME PERFORMANCE METRICS\n")
cat("====================================================================\n")

evaluate_policy_statistics <- function(decision_vector, regime_name) {
  # Compute individual counterfactual point valuations
  individual_values <- (1 - decision_vector) * dr_scores[, 1] + decision_vector * (dr_scores[, 2] - program_cost)
  mean_val <- mean(individual_values)
  se_val   <- sd(individual_values) / sqrt(length(individual_values))
  
  # Confidence Bounds
  lower_ci <- mean_val - (1.96 * se_val)
  upper_ci <- mean_val + (1.96 * se_val)
  total_treated <- sum(decision_vector)
  
  cat(sprintf("  %-26s | EV: $%0.2f (SE: %0.2f) | 95%% CI: [$%0.2f, $%0.2f] | N: %d\n", 
              regime_name, mean_val, se_val, lower_ci, upper_ci, total_treated))
  
  return(list(name = regime_name, values = individual_values, mean = mean_val, se = se_val))
}

stat_lottery <- evaluate_policy_statistics(raw_data$Decision_Lottery, "Random Allocation Lottery")
stat_greedy  <- evaluate_policy_statistics(raw_data$Decision_AI_Greedy, "AI Resource Maximization")
stat_tree    <- evaluate_policy_statistics(raw_data$Decision_AI_Tree, "AI Structural Policy Tree")

# Formal Hypothesis Testing: Compares AI regimes against Random Lottery using a paired t-test
compute_p_value <- function(ai_stats, lottery_stats) {
  delta <- ai_stats$values - lottery_stats$values
  t_stat <- mean(delta) / (sd(delta) / sqrt(length(delta)))
  p_val <- 2 * pt(-abs(t_stat), df = length(delta) - 1)
  sig_flag <- cut(p_val, breaks=c(-Inf, 0.001, 0.01, 0.05, Inf), labels=c("***", "**", "*", "n.s."))
  return(list(p = p_val, flag = sig_flag))
}

test_greedy <- compute_p_value(stat_greedy, stat_lottery)
test_tree   <- compute_p_value(stat_tree, stat_lottery)

cat("\n====================================================================\n")
cat("DIAGNOSTIC CRITERIA: OMNIBUS HETEROGENEITY CALIBRATION\n")
cat("====================================================================\n")
calibration_stats <- test_calibration(c_forest)
print(calibration_stats)


# STEP 7: SCIENTIFIC BRIEFING ENGINE (AUTOMATED POLICY BRIEF)

generate_policy_brief <- function() {
  cat("\n")
  cat("=========================================================================================\n")
  cat("                      SCIENTIFIC BRIEF & STRATEGIC ALLOCATION REPORT                     \n")
  cat("=========================================================================================\n")
  cat(sprintf(" Evaluation Date: %s | Sample Cohort (N): %d | Program Cost Target: $%d\n", 
              Sys.Date(), nrow(raw_data), program_cost))
  cat(sprintf(" Configured Resource Ceiling (Budget Capacity): %0.1f%%\n", budget_capacity * 100))
  cat("-----------------------------------------------------------------------------------------\n\n")
  
  cat("1. CAUSAL MODEL FIDELITY AUDIT\n")
  cat(sprintf("   - Mean Baseline Heterogeneity Coefficient: %0.4f (p = %0.4f)\n", 
              calibration_stats[2,1], calibration_stats[2,4]))
  if(calibration_stats[2,4] < 0.05) {
    cat("     ✔ STATUS: Highly significant treatment effect heterogeneity confirmed. Targeting recommended.\n")
  } else {
    cat("     ⚠ STATUS: Marginal heterogeneity detected. Uniform allocation might limit marginal returns.\n")
  }
  
  cat("\n2. ALLOCATIVE METRIC SURPLUS ANALYSIS\n")
  cat(sprintf("   - Baseline Economic Return (Random Lottery)  : $%0.2f per capita\n", stat_lottery$mean))
  cat(sprintf("   - Optimized Strategic Return (AI Maximization) : $%0.2f per capita (Sig: %s)\n", stat_greedy$mean, test_greedy$flag))
  cat(sprintf("   - Structural Rule Return (AI Policy Tree)    : $%0.2f per capita (Sig: %s)\n", stat_tree$mean, test_tree$flag))
  
  macro_lift <- (stat_greedy$mean - stat_lottery$mean) * nrow(raw_data)
  cat(sprintf("\n3. MACRO FISCAL VALUE IMPACT\n"))
  cat(sprintf("   - Deploying the AI Resource Maximization strategy yields an aggregate public value\n"))
  cat(sprintf("     surplus of $%s compared to conventional randomized distribution.\n", 
              format(round(macro_lift), big.mark=",")))
  cat("-----------------------------------------------------------------------------------------\n")
  cat(" Significance Codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 'n.s.' 1\n")
  cat("=========================================================================================\n\n")
}
generate_policy_brief()


# STEP 8: PRODUCTION GRAPHICS SUITE (VISUAL INTERFACE)

dashboard_data <- data.frame(
  Regime = c("Resource Lottery", "AI Greedy Maximizer", "AI Policy Tree"),
  Per_Capita_Value = c(stat_lottery$mean, stat_greedy$mean, stat_tree$mean),
  Total_Societal_Value = c(stat_lottery$mean * nrow(raw_data), stat_greedy$mean * nrow(raw_data), stat_tree$mean * nrow(raw_data)),
  Slots_Used = c(sum(raw_data$Decision_Lottery), sum(raw_data$Decision_AI_Greedy), sum(raw_data$Decision_AI_Tree))
)

# PANEL 1: Total Societal Return Profiler
p_dash_roi <- ggplot(dashboard_data, aes(x = reorder(Regime, Total_Societal_Value), y = Total_Societal_Value / 1e6, fill = Regime)) +
  geom_bar(stat = "identity", color = "black", width = 0.55, alpha = 0.85) +
  geom_text(aes(label = sprintf("$%0.2fM", Total_Societal_Value / 1e6)), hjust = -0.2, fontface = "bold", size = 3.5) +
  coord_flip() +
  scale_y_continuous(labels = dollar_format(suffix = "M"), expand = expansion(mult = c(0, 0.2))) +
  scale_fill_manual(values = c("Resource Lottery" = "#cccccc", "AI Greedy Maximizer" = "#4292c6", "AI Policy Tree" = "#238b45")) +
  labs(title = "Total Projected Net Societal Value", x = NULL, y = "Net Public Welfare Value ($ Millions)") +
  theme_minimal(base_size = 11) + theme(legend.position = "none", panel.grid.minor = element_blank())

# PANEL 2: Point Return Allocative Efficiency
p_dash_eff <- ggplot(dashboard_data, aes(x = reorder(Regime, Per_Capita_Value), y = Per_Capita_Value, color = Regime, group = 1)) +
  geom_path(color = "#969696", linetype = "dashed", size = 0.8) +
  geom_point(size = 4, stroke = 1.5, fill = "white", shape = 21) +
  geom_text(aes(label = sprintf("$%0.2f", Per_Capita_Value)), vjust = -1.2, fontface = "bold", size = 3.5) +
  scale_color_manual(values = c("Resource Lottery" = "#969696", "AI Greedy Maximizer" = "#4292c6", "AI Policy Tree" = "#238b45")) +
  scale_y_continuous(labels = dollar_format(), expand = expansion(mult = c(0.1, 0.2))) +
  labs(title = "Per Capita Public Return Efficiency", x = NULL, y = "Expected Economic Return ($ per Capita)") +
  theme_minimal(base_size = 11) + theme(legend.position = "none", panel.grid.minor = element_blank())

# PANEL 3: Equity Mapping Interface
equity_plot_list <- list()
categorical_vars <- covariates[sapply(raw_data[covariates], function(x) is.factor(x) || is.character(x))]

# Force the decision to be a factor with specific levels 0 and 1. 
# This prevents the "missing column" error when a category has 0% or 100% treatment.
decision_factor <- factor(raw_data$Decision_AI_Tree, levels = c(0, 1))

for(cat_var in categorical_vars) {
  # Calculate proportions, ensuring the decision_factor guarantees columns 0 and 1 exist
  tbl <- table(raw_data[[cat_var]], decision_factor)
  rates <- prop.table(tbl, 1)
  
  # Extract the treatment rate (column "1")
  rate_vals <- rates[, "1"]
  
  equity_plot_list[[cat_var]] <- data.frame(
    Variable = cat_var, 
    Cohort = names(rate_vals), 
    Allocation_Rate = as.numeric(rate_vals)
  )
}
master_equity_df <- do.call(rbind, equity_plot_list)

p_dash_eq <- ggplot(master_equity_df, aes(x = Cohort, y = Allocation_Rate, fill = Variable)) +
  geom_bar(stat = "identity", color = "black", width = 0.5, alpha = 0.8) +
  facet_wrap(~Variable, scales = "free_x") +
  geom_hline(yintercept = budget_capacity, linetype = "dashed", color = "#e31a1c", linewidth = 0.8) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  scale_fill_brewer(palette = "Pastel1") +
  labs(title = "Demographic Parity Mapping vs. Budget Constraints", x = NULL, y = "AI Selection Rate (%)") +
  theme_minimal(base_size = 10) + 
  theme(legend.position = "none", strip.text = element_text(face="bold"), panel.grid.minor = element_blank())

policymaker_briefing_dashboard <- (p_dash_roi / p_dash_eff / p_dash_eq) +
  plot_annotation(
    title = "EXECUTIVE TARGETING BRIEFING & POLICY ACTION MATRIX",
    subtitle = sprintf("Strategic optimization overview mapping institutional returns across budget constraints (%0.0f%% capacity threshold)", budget_capacity * 100),
    theme = theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
                  plot.subtitle = element_text(size = 11, face = "italic", hjust = 0.5, color = "#525252"))
  )


# SUITE B: DETAILED COVARIATE RETURN DECOMPOSITION ENGINE

covariate_plots <- list()
for (feature in covariates) {
  if (is.numeric(raw_data[[feature]])) {
    p_cov <- ggplot(raw_data, aes(x = .data[[feature]], y = CATE_Estimate)) +
      geom_point(alpha = 0.15, color = "#9ecae1", size = 1) +
      geom_smooth(method = "loess", color = "#02818a", fill = "#bdc9e1", size = 1.1, alpha = 0.4, formula = y ~ x) +
      geom_hline(yintercept = program_cost, linetype = "dashed", color = "#e31a1c", linewidth = 0.8) +
      scale_y_continuous(labels = dollar_format()) +
      labs(title = sprintf("Impact Vector: %s", feature), x = feature, y = "CATE Estimate ($)") +
      theme_minimal(base_size = 10) + theme(plot.title = element_text(face="bold"))
  } else {
    p_cov <- ggplot(raw_data, aes(x = .data[[feature]], y = CATE_Estimate, fill = .data[[feature]])) +
      geom_violin(alpha = 0.6, color = "black", draw_quantiles = 0.5) +
      geom_hline(yintercept = program_cost, linetype = "dashed", color = "#e31a1c", linewidth = 0.8) +
      scale_y_continuous(labels = dollar_format()) +
      scale_fill_brewer(palette = "Blues") +
      labs(title = sprintf("Cohort Disaggregation: %s", feature), x = NULL, y = "Estimated Benefit ($)") +
      theme_minimal(base_size = 10) + theme(plot.title = element_text(face="bold"), legend.position = "none")
  }
  covariate_plots[[feature]] <- p_cov
}
covariate_matrix_layout <- wrap_plots(covariate_plots, ncol = 2) +
  plot_annotation(
    title = "SCIENTIFIC AUDIT: COVARIATE RETURN DECOMPOSITION ENGINE",
    subtitle = "Isolating conditional average treatment effect distributions and structural inflection points for every model attribute",
    theme = theme(plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
                  plot.subtitle = element_text(size = 10, face = "italic", hjust = 0.5, color = "#525252"))
  )


# SUITE C: SCIENTIFIC VALIDATION LAB (OVERLAP & UPLIFT METHODOLOGY)

cat("Assembling Scientific Validation Lab Metrics...\n")

# 1. Propensity Support Audit (Overlap Density)
p_overlap <- ggplot(raw_data, aes(x = Propensity_Score, fill = as.factor(Action_D))) +
  geom_density(alpha = 0.5, color = "black") +
  scale_fill_manual(values = c("0" = "#ef3b2c", "1" = "#08519c"), labels = c("Control", "Treated")) +
  labs(title = "Propensity Common Support Validation", subtitle = "Auditing unconfoundedness overlap metrics across experimental treatment arms", x = "Estimated Propensity Score", y = "Density", fill = "Group") +
  theme_minimal() + theme(plot.title = element_text(face="bold"), legend.position = "top")

# 2. Cumulative Uplift Optimization Gain Curve
raw_data_sorted <- raw_data[order(-raw_data$CATE_Estimate), ]
raw_data_sorted$Percentile <- (1:nrow(raw_data_sorted)) / nrow(raw_data_sorted)

# Compute cumulative performance transformations
dr_ordered <- double_robust_scores(c_forest)[order(-raw_data$CATE_Estimate), ]
raw_data_sorted$Cum_AI <- cumsum(dr_ordered[, 2] - program_cost) / 1e3
raw_data_sorted$Cum_Rand <- cumsum(sample(dr_scores[, 2] - program_cost)) / 1e3

p_uplift <- ggplot(raw_data_sorted, aes(x = Percentile)) +
  geom_line(aes(y = Cum_AI, color = "AI Allocation Strategy"), size = 1.2) +
  geom_line(aes(y = Cum_Rand, color = "Random Selection Baseline"), size = 1, linetype = "dashed") +
  geom_vline(xintercept = budget_capacity, linetype = "dotted", color = "#e31a1c", size = 1) +
  scale_color_manual(values = c("AI Allocation Strategy" = "#238b45", "Random Selection Baseline" = "#969696")) +
  scale_x_continuous(labels = percent_format()) +
  scale_y_continuous(labels = dollar_format(suffix = "k")) +
  labs(title = "Cumulative Uplift (Gain) Curve", subtitle = "Integrated net value discovery tracking diminishing marginal returns across population capacity", x = "Population Fraction Enrolled (%)", y = "Cumulative Net Policy Value ($ Thousands)", color = "Regime") +
  theme_minimal() + theme(plot.title = element_text(face="bold"), legend.position = "top")

# 3. Traditional ordered distribution curve
raw_data_ordered <- raw_data[order(-raw_data$CATE_Estimate), ]
raw_data_ordered$Index <- 1:nrow(raw_data_ordered)
p_inflection <- ggplot(raw_data_ordered, aes(x = Index, y = CATE_Estimate)) +
  geom_ribbon(aes(ymin = CATE_Estimate - 1.96 * CATE_SE, ymax = CATE_Estimate + 1.96 * CATE_SE), fill = "#9ecae1", alpha = 0.4) +
  geom_line(color = "#08519c", size = 1.1) +
  geom_hline(yintercept = program_cost, linetype = "dashed", color = "#e31a1c", linewidth = 0.9) +
  geom_vline(xintercept = total_slots, linetype = "dotted", color = "black", linewidth = 0.9) +
  scale_y_continuous(labels = dollar_format()) +
  labs(title = "Rank-Ordered Treatment Impact Vector", subtitle = "Point-by-point individual CATE curve bounded by 95% confidence intervals", x = "Ranked Individual Observations (Highest to Lowest Return)", y = "Estimated Benefit Return ($)") +
  theme_minimal() + theme(plot.title = element_text(face="bold"))

scientific_validation_suite <- (p_overlap + p_uplift) / p_inflection


# SUITE D: THE PLAIN-ENGLISH EXECUTIVE SUMMARY CANVAS (POLICY BRIEF PANELS)

cat("Synthesizing Plain-English Executive Summary Canvas...\n")

# 1. Calculate high-level metrics for the plain-text KPI dashboard
aggregate_surplus <- (stat_greedy$mean - stat_lottery$mean) * nrow(raw_data)

# Handle potential NaN/NA p-values gracefully if data has zero-variance drops
p_val_clean     <- if(is.na(test_greedy$p)) 1.0 else test_greedy$p
confidence_text <- if(p_val_clean < 0.001) "99.9% (Highly Significant)" else if(p_val_clean < 0.05) "95% (Significant)" else "Low (Not Statistically Significant)"

# FIXED: Changed test_greedy$mean to stat_greedy$mean
verdict_text    <- if(stat_greedy$mean > stat_lottery$mean && p_val_clean < 0.05) "APPROVED: Deploy AI Targeting" else "HOLD: Maintain Baseline Lottery"

kpi_df <- data.frame(
  x = c(1, 1, 1),
  y = c(3, 2, 1),
  Label = c("STRATEGIC RECOMMENDATION", "PROJECTED PUBLIC WELFARE SURPLUS", "STATISTICAL RIGOUR SCORE"),
  Value = c(verdict_text, sprintf("$%s Net Gain", format(round(aggregate_surplus), big.mark=",")), confidence_text),
  Color = c("#238b45", "#08519c", "#525252")
)

p_brief_kpi <- ggplot(kpi_df, aes(x = x, y = y)) +
  geom_rect(aes(xmin = 0.5, xmax = 1.5, ymin = y - 0.4, ymax = y + 0.4), fill = "#f7f7f7", color = "#d9d9d9", size = 0.5) +
  geom_text(aes(label = Label), vjust = -1.5, fontface = "bold", size = 3, color = "#737373") +
  geom_text(aes(label = Value, color = Color), fontface = "bold", size = 4.5) +
  scale_color_identity() +
  xlim(0.4, 1.6) + ylim(0.4, 3.6) +
  labs(title = "1. Plain-English Directives", subtitle = "Core operational mandates derived from causal inference modeling") +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 11, margin = margin(b=4)),
    plot.subtitle = element_text(size = 9, face = "italic", color = "#525252", margin = margin(b=10))
  )

# 2. Dynamic Operational Rules Extraction (Plain English Priority Profiling)
# Segment the population by the AI's top recommendations to see who drives the benefit
raw_data$AI_Tier <- ifelse(raw_data$Decision_AI_Greedy == 1, "High-Yield Priority Group", "Low-Yield / Deficit Group")

# Dynamically separate numeric and categorical covariates from the configured list
num_covs <- covariates[sapply(raw_data[covariates], is.numeric)]
cat_covs <- covariates[sapply(raw_data[covariates], function(x) is.factor(x) || is.character(x))]

profile_traits <- c()
profile_values <- c()

# Extract the dominant target for up to 2 categorical traits
for (i in seq_along(cat_covs)) {
  if (i > 2) break
  top_val <- names(sort(table(raw_data[[cat_covs[i]]][raw_data$Decision_AI_Greedy == 1]), decreasing = TRUE))[1]
  clean_name <- gsub("Feature_", "", cat_covs[i])
  profile_traits <- c(profile_traits, paste("Primary Target", clean_name))
  profile_values <- c(profile_values, as.character(top_val))
}

# Extract the mean for the first numeric trait
if (length(num_covs) >= 1) {
  mean_val <- round(mean(raw_data[[num_covs[1]]][raw_data$Decision_AI_Greedy == 1], na.rm = TRUE), 1)
  clean_name <- gsub("Feature_", "", num_covs[1])
  profile_traits <- c(profile_traits, paste("Target Group Average", clean_name))
  profile_values <- c(profile_values, as.character(mean_val))
}

# Fallback padding in case the dataset has fewer than 3 total covariates
while(length(profile_traits) < 3) {
  profile_traits <- c(profile_traits, paste("Additional Trait", length(profile_traits) + 1))
  profile_values <- c(profile_values, "N/A")
}

profile_df <- data.frame(
  Trait = profile_traits[1:3],
  Value = profile_values[1:3],
  Rank  = c(3, 2, 1)
)

p_brief_profile <- ggplot(profile_df, aes(x = reorder(Trait, Rank), y = 1)) +
  geom_bar(stat = "identity", fill = "#4292c6", width = 0.6, alpha = 0.15, color = "#1c9099") +
  geom_text(aes(label = sprintf("%s: %s", Trait, Value)), hjust = 0, x = as.numeric(reorder(profile_df$Trait, profile_df$Rank)) - 0.1, y = 0.02, fontface = "bold", size = 3.2, color = "#016450") +
  coord_flip() +
  labs(title = "2. Priority Target Profiles", subtitle = "Characteristics of individuals where funds generate maximum impact", x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", size = 11),
    plot.subtitle = element_text(size = 9, face = "italic", color = "#525252")
  )

# 3. Fiscal Efficiency Bar (Wasted vs Saved Capital)
# Calculate "misallocated funds" under lottery (spending cost on people with negative true impact)
lottery_treated_idx <- which(raw_data$Decision_Lottery == 1)
wasted_lottery_funds <- sum(dr_scores[lottery_treated_idx, 2] - dr_scores[lottery_treated_idx, 1] < program_cost) * program_cost

greedy_treated_idx <- which(raw_data$Decision_AI_Greedy == 1)
wasted_ai_funds <- sum(dr_scores[greedy_treated_idx, 2] - dr_scores[greedy_treated_idx, 1] < program_cost) * program_cost

efficiency_df <- data.frame(
  Strategy = c("Standard Random Lottery", "AI-Optimized Strategy"),
  Waste = c(wasted_lottery_funds, wasted_ai_funds)
)

p_brief_efficiency <- ggplot(efficiency_df, aes(x = Strategy, y = Waste / 1e3, fill = Strategy)) +
  geom_bar(stat = "identity", width = 0.5, color = "black", alpha = 0.8) +
  geom_text(aes(label = sprintf("$%0.0fk Misallocated", Waste / 1e3)), vjust = -0.5, fontface = "bold", size = 3) +
  scale_fill_manual(values = c("Standard Random Lottery" = "#ef3b2c", "AI-Optimized Strategy" = "#238b45")) +
  scale_y_continuous(labels = dollar_format(suffix = "k"), expand = expansion(mult = c(0, 0.2))) +
  labs(title = "3. Fiscal Risk Exposure", subtitle = "Public capital inadvertently spent on non-responsive applicants", x = NULL, y = "Inadvertent Capital Waste ($ Thousands)") +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 11),
    plot.subtitle = element_text(size = 9, face = "italic", color = "#525252"),
    axis.text.x = element_text(face = "bold", color = "black")
  )

# Assemble Suite D into a unified visual briefing paper
executive_summary_canvas <- (p_brief_kpi | p_brief_profile) / p_brief_efficiency +
  plot_annotation(
    title = "NON-TECHNICAL EXECUTIVE SUMMARY CANVAS",
    subtitle = "A plain-English operational roadmap translation of the high-dimensional causal forest model architecture",
    theme = theme(
      plot.title = element_text(size = 13, face = "bold", hjust = 0.5, color = "#252525"),
      plot.subtitle = element_text(size = 10, face = "italic", hjust = 0.5, color = "#636363")
    )
  )




# STEP 9: DISPLAY ENGINE EXECUTION

# New: Render the plain-English executive briefing paper first
print(executive_summary_canvas)

# Retain existing expert scientific portfolios intact
print(policymaker_briefing_dashboard)
print(covariate_matrix_layout)
print(scientific_validation_suite)