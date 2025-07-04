# Load necessary libraries
library(parallel)
library(doParallel)
library(foreach)

# ----------- Setup Parallel Environment -----------
available_cores <- detectCores() - 1  # Reserve one core for system
cluster <- makeCluster(available_cores)
registerDoParallel(cluster)

# ----------- Configuration Parameters -----------
n_total <- 1e8       # Total number of random values
chunk_size <- 1e7    # Number of values to generate per chunk
n_chunks <- ceiling(n_total / chunk_size)  # Total number of chunks

# ----------- Dry Run: Timing Norm Computation -----------
system.time({
  partial_sums <- foreach(i = 1:n_chunks, .combine = c) %dopar% {
    # Generate and process a chunk of N(0,1) numbers
    data <- rnorm(chunk_size)
    sum(data^2)
  }
})

stopCluster(cluster)  # End cluster after dry run

# ----------- Estimate Probability via Simulations -----------
n_simulations <- 10
exceed_count <- 0

for (iter in 1:n_simulations) {
  
  # Restart cluster for each simulation to avoid stale workers
  cluster <- makeCluster(available_cores)
  registerDoParallel(cluster)
  
  chunk_sums <- foreach(i = 1:n_chunks, .combine = c) %dopar% {
    values <- rnorm(chunk_size)
    sum(values^2)
  }
  
  total_squared_norm <- sum(chunk_sums) / n_total
  euclidean_norm <- sqrt(total_squared_norm)
  
  exceed_count <- exceed_count + as.integer(euclidean_norm > 0.75)
  
  stopCluster(cluster)
}

# ----------- Report Final Probability Estimate -----------
estimated_prob <- exceed_count / n_simulations
cat("Estimated P(||X|| > 0.75):", estimated_prob, "\n")


