# Number of elements in each random vector (n is fixed to 1000)
n <- 1000

# Vector m representing different values for the number of random vectors to generate (from small to large)
m <- c(50, 100, 200, 500, 1000, 5000, 10000, 20000, 50000, 100000)

# Create an empty vector to store the integral estimates for each value of m
Ans <- numeric(length(m))

# Loop through each value of m
for(i in 1:length(m)) {
  
  # Get the current value of N (number of random vectors to generate for the current m)
  N <- m[i]
  
  # Create an empty vector to store the calculated integral estimates for each random vector
  b <- numeric(1000)
  
  # Loop through 1000 times (Monte Carlo simulations) to calculate the integral estimate
  for(j in 1:1000) {
    
    # Generate a random vector of size N with values uniformly distributed between 0 and 1
    a <- runif(N)
    
    # Apply the integrand: (sum of a_i^101) / (sum of a_i) for the generated vector a
    b[j] <- sum(a^101) / sum(a)
  }
  
  # Take the average of all 1000 simulations for the current value of N
  Ans[i] <- sum(b) / 1000
}

# Print the calculated estimates for each value of m
print(Ans)

# Plot the estimates of the integral as a function of m
plot(m, Ans, type = "b", col = "blue", pch = 19, 
     xlab = "Number of Random Vectors (m)", 
     ylab = "Estimated Integral", 
     main = "Convergence of Integral Estimate as m Increases",
     log = "x")  # log scale on x-axis for better visualization



# Add a horizontal line at y = 1/51
abline(h = 1/51, col = "red", lwd = 2, lty = 2)

# Adding grid lines for better clarity
grid()