# Define relevant variables
X = ppe$yycali_97  # Running variable
Y_post = ppe$y_post  # Outcome after implementation
cutoff = ppe$cut  # Cutoff points

# Define the Epanechnikov kernel function
epanechnikov_kernel = function(u) {
  0.75 * (1 - u^2) * (abs(u) <= 1)
}

# Corrected Silverman bandwidth estimator
silverman_bandwidth = function(X) {
  n = length(X)
  h = 0.9 * min(sd(X), IQR(X) / 1.34) * n^(-1/5)
  return(h)
}

# Estimate coefficients for Treat = 1 and Treat = 0
local_linear_regression = function(X, Y, cutoff, h, treat) {
  # Filter X and Y for the group of interest
  X_treat = X[treat == 1]
  Y_treat = Y[treat == 1]
  
  # Create design matrix
  X_tilde = cbind(1, X_treat - cutoff)
  
  # Kernel weights
  weights = epanechnikov_kernel((X_treat - cutoff) / h)
  
  # Weighted matrix
  W = diag(weights)
  
  # Weighted regression
  beta = solve(t(X_tilde) %*% W %*% X_tilde) %*% (t(X_tilde) %*% W %*% Y_treat)
  return(beta)
}

# Calculate beta_RDD
estimate_beta_rdd = function(X, Y, cutoff, treat) {
  h = silverman_bandwidth(X)
  
  # Estimates for Treat = 1 and Treat = 0
  beta_1 = local_linear_regression(X, Y, cutoff, h, treat = (X <= cutoff))
  beta_0 = local_linear_regression(X, Y, cutoff, h, treat = (X > cutoff))
  
  # First element of the difference
  beta_rdd = beta_1[1] - beta_0[1]
  return(beta_rdd)
}

# Apply the function to estimate beta_RDD
beta_rdd = estimate_beta_rdd(X, Y_post, cutoff, ppe$Treat)

# Sensitivity to changes in h
h = silverman_bandwidth(X)
beta_rdd_h_increase = estimate_beta_rdd(X, Y_post, cutoff, ppe$Treat, h * 1.5)
beta_rdd_h_decrease = estimate_beta_rdd(X, Y_post, cutoff, ppe$Treat, h * 0.5)

# Present results
results = ppe.frame(
  Scenario = c("Default bandwidth", "Increased bandwidth", "Decreased bandwidth"),
  Beta_RDD = c(beta_rdd, beta_rdd_h_increase, beta_rdd_h_decrease)
)

print(results)
