# Load necessary libraries
library(dplyr)
library(purrr)
library()  # for tidy() to extract residuals

# Assuming 'df_state' is your df_state frame that contains the variables

df_state = read.dta(here("replication_project/data", "state.dta")) %>%
  # Generate d_inst and l_inst
  mutate(d_inst = c(NA, diff(inst)),
    l_inst = lag(inst),
    l_inst = ifelse(d_inst < 0, NA, l_inst),
    d_inst = ifelse(d_inst < 0, NA, d_inst),
    rest_inst = inst,
    d_inst98e = c(NA, diff(inst98e)),
    d_inst02e = c(NA, diff(inst02e)),
    d_util_rate = c(NA, diff(util_rate)),
    d_right = c(NA, diff(right)),
    d_ag_ltotal = c(NA, diff(ag_ltotal)),
    d_ag_ssaude = c(NA, diff(ag_ssaude)),
    d_ag_lsaude = c(NA, diff(ag_lsaude))
  ) %>%
  # Replace rest_inst for the year 2002
  mutate(rest_inst = ifelse(year == 2002, (l_inst - d_inst), rest_inst))

# List of variables as a character vector
var_list1 <- c("d_util_rate", "d_right", "d_ag_ltotal", "d_ag_ssaude", "d_ag_lsaude")
var_list2 = c("d_inst98e", "d_inst02e", "inst", "rest_inst")
covars = colnames(df_state %>% 
  select(starts_with("reg") | starts_with("delec")))
# Function to calculate residuals and store them in the data
df_state_98 = df_state %>%
  filter(year == 1998)
model1 <- lm(as.formula(paste0(var_list2[1], "~", paste0(covars, collapse = " + "))), 
             data = df_state_98)
df_state_98$z = residuals(model1)
model2 <- lm(as.formula(paste0(var_list1[1], "~", paste0(covars, collapse = " + "))), 
             data = df_state_98)
df_state_98$y_z = residuals(model2)

model3 = lm(y_z ~ z, data = df_state_98)
clustered = coeftest(model3, vcov = vcovCL, cluster = ~ uf)
for (i in 1:length(var_list1)) {
  var = var_list1[i]
  model2 <- lm(as.formula(paste(var, "~", paste(covars, collapse = " + "))), 
               data = df_state_98)
  df_state_98$y_z = residuals(model2)
  model3 = lm(y_z ~ z, data = df_state_98)
  clustered = coeftest(model3, vcov = vcovCL, cluster = ~ uf)
  coef_list[[paste0("y_", var)]] = clustered["z", "Estimate"]
  se_list[[paste0("y_", var)]] = clustered["z", "Std. Error"]
  p_value_list[[paste0("y_", var)]] = clustered["z", "Std. Error"]
}

B = 1000
set.seed(1000)
t = lm.boot(model3, R = B)
summary(lm.boot(model3, R = B))
# Creating a matrix to store bootstrap coefficients
bootstrap_estimates = matrix(NA, nrow = B, ncol = 1)

for (b in 1:B) {
  # Resampling data indices with replacement
  sample_indices = sample(1:nrow(df_state_98), replace = TRUE)
  bootstrap_sample = df_state_98[sample_indices, ]
  
  # Fitting the model to the bootstrap sample
  bootstrap_model = lm(y_z ~ z, data = bootstrap_sample)
  
  # Clustering
  bootstrap_clust = coeftest(bootstrap_model, vcov = vcovCL, cluster = ~ uf)
  
  # sum model
  sum_model = summary(bootstrap_model)
  
  # Storing the estimated coefficients
  bootstrap_estimates[b, 1] = sum_model$coefficients[2]
  # bootstrap_estimates[b, 2] = sum_model$coefficients[4]
  # bootstrap_estimates[b, 3] = sum_model$coefficients[8]
  bootstrap_df= data.frame(bootstrap_estimates)
  colnames(bootstrap_df) = c("coef")
}
boots_mean = mean(bootstrap_df$coef)
boots_se = sqrt((1/(B-1)*sum((bootstrap_df$coef - boots_mean)^2)))
wald_boots = 
mean(bootstrap_df$coef)
mean(bootstrap_df$se)
mean(bootstrap_df$`p-value`)

calculate_residuals <- function(dep_var, predictors, data, filter_year) {
  # Filter data for the specified year
  data_filtered <- data %>% filter(year == filter_year)
  
  # Fit regression model with the specified dependent variable and predictors
  model1 <- lm(as.formula(paste0(dep_var, "~", paste0(predictors, collapse = " + "))), 
               data = data_filtered)
  
  # Extract residuals from the model
  residuals1 <- residuals(model1)
  
  # Add residuals as a new column to the filtered data
  data_filtered$z <- residuals
  
  for (i in 1:length(var_list1)) {
    model2 = lm(as.formula(paste0(var_list1[i], "~", paste0(predictors, collapse = " + "))), data = data_filtered)
    # Extract residuals from the model
    residuals2 <- residuals(model2)
    # Add residuals as a new column to the filtered data
    data_filtered$z <- residuals
  }
  
  return(data_filtered)
}
# library(plm)
# plm()
# Define predictors and list of variables for the process
predictors <- "reg1 + delec1 + delec2 + ..."  # Replace with actual predictors
list_vars <- c("util_rate", "right", "ag_ltotal", "ag_ssaude", ...)  # Replace with actual variables

# Step 1: Initial model fit to generate `z` residuals
teste <- calculate_residuals(var_list2[1], covars, df_state, filter_year = 1998)

# Step 2: Loop through each variable in `list_vars`
results <- list()  # List to store results for each variable
for (var in list_vars) {
  # Generate differentiated variable name by appending "d." prefix
  diff_var <- paste0("d.", var)
  
  # Fit regression for the differentiated variable
  data <- calculate_residuals(diff_var, predictors, data)
  
  # Regress residuals of `diff_var` on `z` residuals from the first model
  model <- lm(data$residuals ~ data$z, data = data %>% filter(year == 1998))
  
  # Perform Wild Bootstrap
  boot_result <- boot(df_state_98, statistic = function(df_state_98, indices) {
    # Resample data with indices for clustering by `uf`
    #sample_data <- data[indices, ]
    model_boot <- lm(y_z ~ z)
    sum_boot = summary(model_boot)
    # Extract coefficients from the bootstrap model
  }, R = 1000, sim = "ordinary")  # `ordinary` is a generic example, adapt as needed
  
  # Store results for each variable in the `results` list
  results[[var]] <- list(model = model, boot_result = boot_result)
}