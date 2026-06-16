# Tutorial 1: CART and Causal Metric Functions
# Author: Umer Zeeshan Ijaz (http://userweb.eng.gla.ac.uk/umer.ijaz)
set.seed(42)
N <- 1000

# 1. Synthesize data streams
X <- runif(N, 0, 100)  # Treatment Lift Driver
Z <- runif(N, 0, 100)  # Main Effect Driver
D <- rbinom(N, 1, 0.5) # Random Treatment Assignment

baseline <- ifelse(Z < 50, 10, 60)
true_tau <- ifelse(X < 50, 30, 0)
Y        <- baseline + D * true_tau + rnorm(N, mean=0, sd=2)

df <- data.frame(X=X, Z=Z, D=D, Y=Y)
var_parent <- var(Y) * (N-1)/N # Population variance helper

# ---- Scoring Functions ----
get_cart_metric <- function(feature,t) {
  left  <- df[df[[feature]] < t, ]
  right <- df[df[[feature]] >= t, ]
  n_l <- nrow(left); n_r <- nrow(right)
  var_l <- var(left$Y)*(n_l-1)/n_l; var_r <- var(right$Y)*(n_r-1)/n_r
  return(var_parent - ((n_l/N)*var_l + (n_r/N)*var_r))
}

get_causal_metric <- function(feature,t) {
  left  <- df[df[[feature]] < t, ]
  right <- df[df[[feature]] >= t, ]
  n_l <- nrow(left); n_r <- nrow(right)
  tau_l <- mean(left$Y[left$D==1]) - mean(left$Y[left$D==0])
  tau_r <- mean(right$Y[right$D==1]) - mean(right$Y[right$D==0])
  return((n_l * n_r / (N^2)) * ((tau_l - tau_r)^2))
}

# Automated Grid Search Sweep Engine
thresholds <- seq(10, 90, by = 1)

cart_Z_scores   <- sapply(thresholds, function(t) get_cart_metric("Z", t))
causal_X_scores <- sapply(thresholds, function(t) get_causal_metric("X", t))

opt_cart   <- thresholds[which.max(cart_Z_scores)]
opt_causal <- thresholds[which.max(causal_X_scores)]



# Tutorial 2: Growing the Tree Branches Forward

# Step 1: Physically partition the parent data frame using Layer 1 splits
df_cart_left    <- df[df$Z < opt_cart, ]
df_cart_right   <- df[df$Z >= opt_cart, ]

df_causal_left  <- df[df$X < opt_causal, ]
df_causal_right <- df[df$X >= opt_causal, ]

# Step 2: Define localized Layer 2 scoring functions bound to child data
get_cart_metric_l2 <- function(sub_df, feature, t) {
  n_sub <- nrow(sub_df)
  if (n_sub < 20) return(0) # Stopping rule: skip if parent subset is too small
  
  # Total population variance helper localized to this node
  var_sub <- var(sub_df$Y) * (n_sub - 1) / n_sub
  
  left  <- sub_df[sub_df[[feature]] < t, ]
  right <- sub_df[sub_df[[feature]] >= t, ]
  n_l <- nrow(left); n_r <- nrow(right)
  
  if (n_l < 10 || n_r < 10) return(0) # Stopping rule: reject tiny child nodes
  
  var_l <- var(left$Y) * (n_l - 1) / n_l
  var_r <- var(right$Y) * (n_r - 1) / n_r
  return(var_sub - ((n_l / n_sub) * var_l + (n_r / n_sub) * var_r))
}

get_causal_metric_l2 <- function(sub_df, feature, t) {
  n_sub <- nrow(sub_df)
  left  <- sub_df[sub_df[[feature]] < t, ]
  right <- sub_df[sub_df[[feature]] >= t, ]
  n_l <- nrow(left); n_r <- nrow(right)
  
  if (n_l < 10 || n_r < 10) return(0) # Stopping rule
  
  # Causal Stratification Check: Ensure treated and controls exist in both proposed paths
  if (sum(left$D == 1) < 2  || sum(left$D == 0) < 2 || 
      sum(right$D == 1) < 2 || sum(right$D == 0) < 2) return(0)
  
  tau_l <- mean(left$Y[left$D == 1]) - mean(left$Y[left$D == 0])
  tau_r <- mean(right$Y[right$D == 1]) - mean(right$Y[right$D == 0])
  return((n_l * n_r / (n_sub^2)) * ((tau_l - tau_r)^2))
}

# Step 3: Run Layer 2 optimization sweeps inside the Left Child Nodes
cart_l2_Z_score <- max(sapply(thresholds, function(t) get_cart_metric_l2(df_cart_left, "Z", t)))
cart_l2_X_score <- max(sapply(thresholds, function(t) get_cart_metric_l2(df_cart_left, "X", t)))

causal_l2_Z_score <- max(sapply(thresholds, function(t) get_causal_metric_l2(df_causal_left, "Z", t)))
causal_l2_X_score <- max(sapply(thresholds, function(t) get_causal_metric_l2(df_causal_left, "X", t)))

# Step 4: Display Structural Diagnostic Insights
cat("\n--- LAYER 2 OPTIMIZATION INSIGHTS ---\n")
cat(sprintf("CART Left Branch Split Options  -> Peak Score on Z: %.2f | Peak Score on X: %.2f\n", 
            cart_l2_Z_score, cart_l2_X_score))
cat(sprintf("Causal Left Branch Split Options -> Peak Score on Z: %.2f | Peak Score on X: %.2f\n", 
            causal_l2_Z_score, causal_l2_X_score))


# Tutorial 3: Breaking Causal Trees with Confounding Bias

set.seed(42)
N <- 1000

X <- runif(N, 0, 100) 
Z <- runif(N, 0, 100) 

# Inject Confounding: High Z individuals are highly likely to get treated
prob_treatment <- ifelse(Z < 50, 0.1, 0.8)
D <- rbinom(N, 1, prob_treatment)

baseline <- ifelse(Z < 50, 10, 60) # High Z has a huge baseline outcome shift
true_tau <- ifelse(X < 50, 30, 0)  # X remains the true treatment driver
Y        <- baseline + D * true_tau + rnorm(N, mean=0, sd=2)

df_confounded <- data.frame(X=X, Z=Z, D=D, Y=Y)

# Evaluate a split on Feature Z using the simple difference-in-means metric
left_Z  <- df_confounded[df_confounded$Z < 50, ]
right_Z <- df_confounded[df_confounded$Z >= 50, ]

tau_hat_left_Z  <- mean(left_Z$Y[left_Z$D==1]) - mean(left_Z$Y[left_Z$D==0])
tau_hat_right_Z <- mean(right_Z$Y[right_Z$D==1]) - mean(right_Z$Y[right_Z$D==0])

cat("\n--- THE CONFOUNDING CRISIS ---\n")
cat(sprintf("Hallucinated Leaf Effect on Low Z: %.2f (True Effect is uniform across Z)\n", tau_hat_left_Z))
cat(sprintf("Hallucinated Leaf Effect on High Z: %.2f\n", tau_hat_right_Z))


# Robinson's Orthogonalisation (Residualization)

# 1. Regress Y on Z to strip out baseline outcome shifts
m_x <- lm(Y ~ Z, data=df_confounded)
Y_tilde <- residuals(m_x)

# 2. Regress D on Z to strip out propensity selection bias
e_x <- glm(D ~ Z, data=df_confounded, family=binomial)
D_tilde <- residuals(e_x)

# 3. Evaluate the true driver (X) using the unconfounded residuals
df_residualized <- data.frame(X=X, Y_tilde=Y_tilde, D_tilde=D_tilde)