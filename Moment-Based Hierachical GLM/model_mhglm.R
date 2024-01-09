library('mbest')
library("devtools", quietly=TRUE)
library("methods", quietly=TRUE)


subsamples <- readRDS("subsamples_unique_user.rds")
#subsamples <- readRDS("subsamples_random.rds")

set.seed(31337)
time <- list()

nobs <- list()
ngroup <- list()


get.time <- function(t) {
    with(as.list(summary(t)), user + system)
}

all_results <- list()
for(i in 1:length(subsamples)) {
    subsample <- subsamples[[i]]
    nobs[[i]] <- nrow(subsample)
    ngroup[[i]] <- nlevels(subsample$group)
    
    iter_start_time <- proc.time()
    
    time[[i]] <- system.time({
      model <- mhglm(y ~ x - 1 + (z - 1 | group), data = subsample,
                     family=binomial)
    })
    ## random effects 
    ## a file with everything
    
    fixef <- fixef(model)
    fixef.vcov <- as.matrix(vcov(model))
    ranef <- as.matrix(ranef(model)[["group"]])
    ranef.cov <- VarCorr(model)[["group"]]
    attr(ranef.cov, "stddev") <- NULL
    attr(ranef.cov, "correlation") <- NULL
    yhat <- fitted.values(model)
    
    # Calculate and print the time taken for this iteration
    iter_time_taken <- proc.time() - iter_start_time
    cat("Iteration", i, "is done. Time taken:", iter_time_taken['elapsed'], "seconds\n")
    
    
    
    
    all_results[[i]] <- list(fixef=fixef, fixef.vcov=fixef.vcov,
                             ranef=ranef, ranef.cov=ranef.cov, yhat=yhat,
                             time=time[[i]], nobs = nobs[[i]], ngroup = ngroup[[i]])
}

saveRDS(all_results, "mhglm_all_subsamples.rds")
#saveRDS(all_results, "mhglm_all_subsamples_random.rds")

