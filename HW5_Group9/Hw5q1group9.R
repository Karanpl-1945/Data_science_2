
library(doParallel)
library(foreach)

# Parameters
total_n <- 1e9 ##Key Variavle
chunk_size <- 1e7
n_chunks <- total_n / chunk_size  # 10,000
num_cores <- 6

# Start cluster
cl <- makeCluster(num_cores)
registerDoParallel(cl)

start_time <- Sys.time()

# Generate numbers in parallel without storing them
invisible(
  foreach(i = 1:n_chunks, .combine = 'c', .packages = "stats", .options.snow = list(preschedule = TRUE)) %dopar% {
    set.seed(Sys.getpid() + i)  # unique seed per chunk
    rnorm(chunk_size)
    NULL  # discard output
  }
)

stopCluster(cl)

end_time <- Sys.time()
cat("✅ Successfully generated", format(total_n, scientific =TRUE), "standard normal numbers\n")
cat("⏱️ Time taken:", round(difftime(end_time, start_time, units = "mins"), 2), "minutes\n")