library("methods", quietly=TRUE)
library("lme4", quietly=TRUE, warn.conflicts=FALSE)
library("nlme", quietly=TRUE, warn.conflicts=FALSE)
library("optimx", quietly=TRUE, warn.conflicts=FALSE)


data <- readRDS("data_3_Jan.rds")

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

#Needed for HPF comparison
for(i in seq_along(subsamples)) {
  file_name <- paste0("unique_userid_sample_", i, ".csv")
  write.csv(subsamples[[i]], file_name, row.names = FALSE)
}


rm("data") #remove original data to remove up the memory


control <- glmerControl(calc.derivs = FALSE,
                        check.nobs.vs.nlev = "ignore",
                        check.nlev.gtr.1 = "ignore",
                        check.nobs.vs.nRE = "ignore",
                        check.rankX = "stop.deficient",
                        check.scaleX = "warning")
nAGQ <- 0L


all_results <- list()
for(i in 1:length(subsamples)) {
  subsample <- subsamples[[i]]
  
  # Start timing this iteration
  iter_start_time <- proc.time()
  
  model <- with(subsample, glmer(y ~ x - 1 + (z - 1 | group), family=binomial,
                                 control=control, nAGQ=nAGQ))
  
  fixef <- fixef(model)
  fixef.vcov <- as.matrix(vcov(model))
  ranef <- as.matrix(ranef(model)[["group"]])
  ranef.cov <- VarCorr(model)[["group"]]
  attr(ranef.cov, "stddev") <- NULL
  attr(ranef.cov, "correlation") <- NULL
  yhat <- fitted.values(model)
  converged <- is.null(model@optinfo$conv$lme4)
  iter <- model@optinfo$feval
  
  all_results[[i]] <- list(fixef=fixef, fixef.vcov=fixef.vcov,
                           ranef=ranef, ranef.cov=ranef.cov, yhat=yhat,
                           converged=converged, iter=iter)
  
  saveRDS(all_results[[i]], paste0("lme4_subsample_", i, ".rds"))
  
  # Calculate and print the time taken for this iteration
  iter_time_taken <- proc.time() - iter_start_time
  cat("Iteration", i, "is done. Time taken:", iter_time_taken['elapsed'], "seconds\n")
}

# Save all results to a single file
saveRDS(all_results, "lme4_all_subsamples.rds")

