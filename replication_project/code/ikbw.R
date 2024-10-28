covars = c("income", "gini", "latitude", "longitude", "illiter", "less4", "less8", "population91", "population00", "urbanization")
ikbw = c()
for (var in covars) {
  rd = rdd_data(x = munic$dep, y = munic[[var]], cutpoint = 0)
  bwo = rdd_bw_ik(rd, kernel = "Uniform")
  ikbw = append(ikbw, ifelse(as.numeric(bwo) > 20000, 20000, floor(as.numeric(bwo))))
}
obs = c(558, 377, 412, 345, 389, 372, 558, 558, 454, 558)
ikbw_df = data.frame(covars, ikbw, obs)

# Initialize lists to store coefficients and robust standard errors
coef_list = list()
se_list = list()
intercept_list = list()
intercept_se_list = list()

# Calculate means and standard deviations for each variable
mean_sd_list = list()

# Loop through each variable and estimate models with three bandwidths
for (i in 1:nrow(ikbw_df)) {
  var = ikbw_df$covars[i]
  
  # Regression with IK optimal bandwidth
  model1 = lm(as.formula(paste(var, "~ treat + dep + deptreat")), data = subset(munic, bw < ikbw_df$ikbw[i]))
  robust1 = coeftest(model1, vcov = vcovHC(model1, type = "HC1"))
  coef_list[[paste0(var, "_bw1")]] = robust1["treat", "Estimate"]
  se_list[[paste0(var, "_bw1")]] = robust1["treat", "Std. Error"]
  intercept_list[[paste0(var, "_bw1")]] = robust1["(Intercept)", "Estimate"]
  intercept_se_list[[paste0(var, "_bw1")]] = robust1["(Intercept)", "Std. Error"]
  
  # Regression with bandwidth < 10000
  model2 = lm(as.formula(paste(var, "~ treat + dep + deptreat")), data = subset(munic, bw < 10000))
  robust2 = coeftest(model2, vcov = vcovHC(model2, type = "HC1"))
  coef_list[[paste0(var, "_bw2")]] = robust2["treat", "Estimate"]
  se_list[[paste0(var, "_bw2")]] = robust2["treat", "Std. Error"]
  
  # Regression with bandwidth < 5000
  model3 = lm(as.formula(paste(var, "~ treat + dep + deptreat")), data = subset(munic, bw < 5000))
  robust3 = coeftest(model3, vcov = vcovHC(model3, type = "HC1"))
  coef_list[[paste0(var, "_bw3")]] = robust3["treat", "Estimate"]
  se_list[[paste0(var, "_bw3")]] = robust3["treat", "Std. Error"]
  
  # Calculate mean and standard deviation for the variable
  mean_val = mean(munic[[var]], na.rm = TRUE)
  sd_val = sd(munic[[var]], na.rm = TRUE)
  mean_sd_list[[var]] = c(mean_val, sd_val)
}

# Create a matrix to display in stargazer
output_table = matrix("", nrow = length(covars) * 2 + 2, ncol = 7)

# Fill the first column with variable names, "bandwidth", and "obs"
output_table[seq(1, length(covars) * 2, 2), 1] = covars
output_table[length(covars) * 2 + 1, 1] = "bandwidth"
output_table[length(covars) * 2 + 2, 1] = "obs"

# Fill the remaining columns with coefficients, robust standard errors, means, standard deviations, bandwidths, and observations
for (j in 1:length(covars)) {
  var = covars[j]
  
  # Fill coefficients and robust standard errors for each specification
  output_table[2 * j - 1, 3] = sprintf("%.3f", intercept_list[[paste0(var, "_bw1")]])
  output_table[2 * j, 3] = sprintf("(%.3f)", intercept_se_list[[paste0(var, "_bw1")]])
  
  output_table[2 * j - 1, 5] = sprintf("%.3f", coef_list[[paste0(var, "_bw1")]])
  output_table[2 * j, 5] = sprintf("(%.3f)", se_list[[paste0(var, "_bw1")]])
  
  output_table[2 * j - 1, 6] = sprintf("%.3f", coef_list[[paste0(var, "_bw2")]])
  output_table[2 * j, 6] = sprintf("(%.3f)", se_list[[paste0(var, "_bw2")]])
  
  output_table[2 * j - 1, 7] = sprintf("%.3f", coef_list[[paste0(var, "_bw3")]])
  output_table[2 * j, 7] = sprintf("(%.3f)", se_list[[paste0(var, "_bw3")]])
  
  # Fill mean and standard deviation
  output_table[2 * j - 1, 2] = sprintf("%.3f", mean_sd_list[[var]][1])
  output_table[2 * j, 2] = sprintf("(%.3f)", mean_sd_list[[var]][2])
  
  # Fill ikbw and obs
  output_table[2 * j - 1, 4] = ikbw_df$ikbw[j]
  output_table[2 * j, 4] = ikbw_df$obs[j]
}

# Add bandwidth and observations to the last two rows
output_table[length(covars) * 2 + 1, 2:7] = c("—", "—", "—", "IKBW", "10000", "5000")
output_table[length(covars) * 2 + 2, 2:7] = c("5281", "—", "—", "—", "229", "116")

# Convert the matrix to a data frame and set column names
output_df = as.data.frame(output_table)
colnames(output_df) = c("", "Mean (SD)", "Pre-Treat. (SE)", "IKBW (Obs)", "(1)", "(2)", "(3)")

# Display the table with stargazer
stargazer(
  output_df,
  type = "text",
  summary = FALSE,
  title = "Table I",
  rownames = FALSE
)
