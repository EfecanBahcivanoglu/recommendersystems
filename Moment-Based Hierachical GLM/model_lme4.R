library("methods", quietly=TRUE)
library("lme4", quietly=TRUE, warn.conflicts=FALSE)
library("nlme", quietly=TRUE, warn.conflicts=FALSE)
library("optimx", quietly=TRUE, warn.conflicts=FALSE)


#subsamples <- readRDS("subsamples_unique_user.rds")
subsamples <- readRDS("subsamples_random.rds")



control <- glmerControl(calc.derivs = FALSE,
                        check.nobs.vs.nlev = "ignore",
                        check.nlev.gtr.1 = "ignore",
                        check.nobs.vs.nRE = "ignore",
                        check.rankX = "stop.deficient",
                        check.scaleX = "warning")
nAGQ <- 0L


all_results <- list()
time_results <- list() 
for(i in 1:length(subsamples)) {
  subsample <- subsamples[[i]]
  
  # Start timing this iteration
  iter_start_time <- proc.time()
  
  time_results[[i]] <- system.time({
    model <- with(subsample, glmer(y ~ x - 1 + (z - 1 | group), family=binomial,
                                   control=control, nAGQ=nAGQ))
  })
  
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
                           converged=converged, iter=iter, time=time_results[[i]])
  
  saveRDS(all_results[[i]], paste0("lme4_subsample_unique_user", i, ".rds"))
  
  # Calculate and print the time taken for this iteration
  iter_time_taken <- proc.time() - iter_start_time
  cat("Iteration", i, "is done. Time taken:", iter_time_taken['elapsed'], "seconds\n")
}

# Save all results to a single file
#saveRDS(all_results, "lme4_all_subsamples.rds")
saveRDS(all_results, "lme4_all_subsamples_random.rds")


