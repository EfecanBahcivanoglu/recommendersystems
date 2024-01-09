data <- readRDS("data.rds")

set.seed(123)  # Setting a seed for reproducibility
total_rows <- nrow(data)
shuffled_indices <- sample(1:total_rows)

# Step 2: Determine the number of data points per subsample and divide the indices
points_per_sample <- ceiling(total_rows / 10)
subsample_indices <- split(shuffled_indices, rep(1:10, each=points_per_sample, length.out=total_rows))

# Step 3: Create the subsamples
subsamples_obs <- lapply(subsample_indices, function(indices) {
  data[indices, ]
})


saveRDS(subsamples_obs, "subsamples_random.rds")
