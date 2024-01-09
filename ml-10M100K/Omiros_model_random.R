library("methods", quietly=TRUE)
library("lme4", quietly=TRUE, warn.conflicts=FALSE)
library("nlme", quietly=TRUE, warn.conflicts=FALSE)
library("optimx", quietly=TRUE, warn.conflicts=FALSE)


data <- readRDS("data_omiros.rds")


# Step 1: Shuffle the row indices
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

# Now subsamples is a list of 10 dataframes, each a random subsample of the original dataframe.
rm("data") #remove original data to remove up the memory


control <- glmerControl(calc.derivs = FALSE,
                        check.nobs.vs.nlev = "ignore",
                        check.nlev.gtr.1 = "ignore",
                        check.nobs.vs.nRE = "ignore",
                        check.rankX = "stop.deficient",
                        check.scaleX = "warning")
nAGQ <- 0L

all_results_obs <- list()

for(i in 1:length(subsamples_obs)) {
  # Extract the subsample
  subsample <- subsamples_obs[[i]]
  
  # Fit the model
  time <- system.time({
    model <- with(subsample, glmer(y ~ x - 1 + (z - 1 | group), family=binomial,
                                   control=control, nAGQ=nAGQ))
  })
  
  # Extract the results
  fixef <- fixef(model)
  fixef.vcov <- as.matrix(vcov(model))
  ranef <- as.matrix(ranef(model)[["group"]])
  ranef.cov <- VarCorr(model)[["group"]]
  attr(ranef.cov, "stddev") <- NULL
  attr(ranef.cov, "correlation") <- NULL
  yhat <- fitted.values(model)
  converged <- is.null(model@optinfo$conv$lme4)
  iter <- model@optinfo$feval
  
  # Store the results
  all_results_obs[[i]] <- list(time=time, fixef=fixef, fixef.vcov=fixef.vcov,
                           ranef=ranef, ranef.cov=ranef.cov, yhat=yhat,
                           converged=converged, iter=iter)
  
  # Optional: Save individual results to files
  saveRDS(all_results_obs[[i]], paste0("lme4_subsample__obs", i, ".rds"))
}

# Save all results to a single file
saveRDS(all_results_obs, "lme4_all_subsamples_obs.rds")

