data <- readRDS("data.rds")


set.seed(2)
# Step 1: Split the data by unique values in 'group'
# Step 1: Get the unique group identifiers
unique_groups <- unique(data$group)

# Step 2: Determine the number of groups per sample
num_groups <- length(unique_groups)
groups_per_sample <- ceiling(num_groups / 10)  # Use ceiling to ensure you cover all groups

# Step 3: Create the subsamples
subsamples <- lapply(1:10, function(i) {
  # Randomly select group identifiers for this subsample
  sampled_groups <- sample(unique_groups, groups_per_sample)
  
  # Subset the original dataframe based on the sampled groups
  subset(data, group %in% sampled_groups)
})


saveRDS(subsamples, "subsamples_uniquie_user.rds")