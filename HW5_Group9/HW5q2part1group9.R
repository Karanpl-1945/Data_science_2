library(MASS)
library(parallel)

# Settings
dims <- c(2, 4, 6, 8, 10, 15, 20, 30, 40, 50, 60, 80, 100, seq(200, 1000, by=100))
n_sim <- 10000
rho <- 0.6
n_cores <- min(6, detectCores())

# Start timing
start_time <- proc.time()

# Quiet worker function
worker <- function(d_sublist) {
  results <- matrix(NA, nrow = length(d_sublist), ncol = 2)
  for (i in seq_along(d_sublist)) {
    d <- d_sublist[i]
    tryCatch({
      Sigma <- matrix(rho, d, d)
      diag(Sigma) <- 1
      X <- mvrnorm(n_sim, mu = rep(0, d), Sigma = Sigma)
      prob <- mean(sqrt(rowSums(X^2)) > 0.75)
      results[i, ] <- c(d, prob)
      rm(X, Sigma)
      gc()
    }, error = function(e) {
      results[i, ] <- c(d, NA)
    })
  }
  results
}

# Split dimensions into chunks
dim_chunks <- split(dims, sort(rep(1:n_cores, length.out = length(dims))))

# Run in parallel
results_list <- mclapply(dim_chunks, worker, mc.cores = n_cores)

# Stop timing
end_time <- proc.time()
time_taken <- end_time - start_time

# Combine results
results_mat <- do.call(rbind, results_list)
colnames(results_mat) <- c("d", "prob")
results_df <- as.data.frame(results_mat)

# Plot
plot(
  results_df$d, results_df$prob,
  type = "b", pch = 19, col = "darkblue",
  xlab = "Dimension (d)",
  ylab = expression(P(paste("||X||") > 0.75)),
  main = expression(paste("Estimated ", P(paste("||X||") > 0.75), " vs Dimension")),
  cex.lab = 1.2, cex.main = 1.3
)
grid()

# Time taken
cat("\nTime taken (in seconds):\n")
print(time_taken["elapsed"])