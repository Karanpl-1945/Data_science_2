# Load necessary packages

library(quantreg)

# Load your data
data <- read.csv("weight-height.csv")
Bdata <- data[1:1000, 2:3]  # Use first 1000 rows and columns 2 and 3 for Height and Weight
x <- Bdata$Height
y <- Bdata$Weight

# 1. Fit the model using LSE (Ordinary Least Squares)
model_lse <- lm(Weight ~ Height, data = Bdata)

# 2. Fit the model using LAD (Least Absolute Deviations)
model_lad <- rq(Weight ~ Height, data = Bdata)

# 3. Extract coefficients for both models
beta0_lse <- coef(model_lse)[1]
beta1_lse <- coef(model_lse)[2]
beta0_lad <- coef(model_lad)[1]
beta1_lad <- coef(model_lad)[2]

# 4. Plot the scatter plot and regression lines
plot(Bdata$Height, Bdata$Weight, 
     main = "Height vs Weight with Regression Lines", 
     xlab = "Height", 
     ylab = "Weight", 
     pch = 19, col = "gray", cex = 0.5)

# Add LSE regression line
abline(model_lse, col = "blue", lwd = 2)


# Add LAD regression line
lines(Bdata$Height, predict(model_lad), col = "red", lwd = 2)

# Add a legend
legend("topleft", legend = c("LSE Line", "LAD Line"), col = c("blue", "red"), lwd = 2)






#graph
residuals_lse <- Bdata$Weight - predict(model_lse)

residuals_lad <- Bdata$Weight - predict(model_lad)

# 6. Numerically compare the goodness of fit
# Sum of squared residuals for LSE (RSS)
rss_lse <- sum(residuals_lse^2)

# Sum of absolute residuals for LAD (LAD-RSS)
lad_rss <- sum(abs(residuals_lad))

# Sum of squared residuals for LSE (RSS)
rss_lse <- sum(residuals_lse^2)

# Sum of absolute residuals for LAD (LAD-RSS)
lad_rss <- sum(abs(residuals_lad))

# Create a table with the results
results_table <- data.frame(
  Metric = c("Beta0 (Intercept)", "Beta1 (Slope)", "Sum of Squared Residuals (RSS)", "Sum of Absolute Residuals (LAD-RSS)"),
  LSE = c(beta0_lse, beta1_lse, rss_lse, NA),
  LAD = c(beta0_lad, beta1_lad, NA, lad_rss)
)

# Print the table
cat("\n--- Results Table ---\n")
print(results_table)













# 5. Plot the residuals for LSE and LAD side by side
par(mfrow = c(1, 2))  # Set up a 1x2 plotting layout

# Plot residuals for LSE

plot(Bdata$Height, residuals_lse, 
     main = "Residuals for LSE", 
     xlab = "Height", ylab = "Residuals", 
     pch = 19, col = "blue")
abline(h = 0, col = "red")  # Add a horizontal line at 0 for reference

# Plot residuals for LAD

plot(Bdata$Height, residuals_lad, 
     main = "Residuals for LAD", 
     xlab = "Height", ylab = "Residuals", 
     pch = 19, col = "green")
abline(h = 0, col = "red")  # Add a horizontal line at 0 for reference

# Reset plotting layout
par(mfrow = c(1, 1))






















# Adding outliers
set.seed(42)
Bdata_with_outliers <- rbind(Bdata, data.frame(Height = c(190, 195), Weight = c(150, 160)))

# Fit models with the outlier data
model_lse_outliers <- lm(Weight ~ Height, data = Bdata_with_outliers)
model_lad_outliers <- rq(Weight ~ Height, data = Bdata_with_outliers)

# Plot the data with outliers
plot(Bdata_with_outliers$Height, Bdata_with_outliers$Weight, 
     main = "Height vs Weight with Outliers", 
     xlab = "Height", ylab = "Weight", pch = 19, col = "gray", cex = 0.5)

# Add LSE regression line with outliers
abline(model_lse_outliers, col = "blue", lwd = 2)

# Add LAD regression line with outliers
lines(Bdata_with_outliers$Height, predict(model_lad_outliers), col = "red", lwd = 2)

# Add a legend
legend("topleft", legend = c("LSE Line", "LAD Line"), col = c("blue", "red"), lwd = 2)




