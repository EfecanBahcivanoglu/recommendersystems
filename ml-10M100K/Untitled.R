library("methods", quietly=TRUE)
library("lme4", quietly=TRUE, warn.conflicts=FALSE)
library("nlme", quietly=TRUE, warn.conflicts=FALSE)
library("optimx", quietly=TRUE, warn.conflicts=FALSE)


subsamples <- readRDS("subsamples_unique_user.rds")

set.seed(2)

subsample = subsamples[[1]]

create_validation_set <- function(df, val_size = 0.2) {
  # Identify unique users and items - though they are not used directly in this function
  users <- unique(df$UserId)
  items <- unique(df$ItemId)
  
  # Create a mask for duplicated UserId and ItemId
  mask <- duplicated(df$UserId) | duplicated(df$UserId, fromLast = TRUE) &
    duplicated(df$ItemId) | duplicated(df$ItemId, fromLast = TRUE)
  temp_df <- df[mask, ]
  
  # Determine the number of validation samples
  val_count <- as.integer(nrow(temp_df) * val_size)
  
  # Split the data into validation and training sets
  validation_set <- head(temp_df, val_count)
  training_set <- dplyr::bind_rows(df, validation_set) %>%
    dplyr::distinct(.keep_all = TRUE)
  
  # Ensure validation set only contains UserId and ItemId in training set
  validation_set <- validation_set[validation_set$UserId %in% training_set$UserId, ]
  validation_set <- validation_set[validation_set$ItemId %in% training_set$ItemId, ]
  
  # Update the training set by removing any overlap with the validation set
  training_set <- dplyr::bind_rows(training_set, validation_set) %>%
    dplyr::distinct(.keep_all = TRUE)
  
  return(list(training_set = training_set, validation_set = validation_set))
}

training, validation <- create_validation_set(subsample)






#rm("data") #remove original data to remove up the memory

# Your existing control settings
control <- glmerControl(calc.derivs = FALSE,
                        check.nobs.vs.nlev = "ignore",
                        check.nlev.gtr.1 = "ignore",
                        check.nobs.vs.nRE = "ignore",
                        check.rankX = "stop.deficient",
                        check.scaleX = "warning")
nAGQ <- 0L  # Adjusted Gauss-Quadrature approximation; using 0 implies the Laplace approximation

# subsample <- subsamples[[1]]
# subsample2 <- subsamples[[2]]
# write.csv(subsample, "subsample1_comparison.csv", row.names = FALSE)
# write.csv(subsample2, "subsample2_comparison.csv", row.names = FALSE)
# subsample$movie <- NULL
# subsample2$movie <- NULL


time <- system.time({
  model <- glmer(count ~ x.genrechildren + x.genrecomedy + x.genredrama - 1 +
                   (z.genrechildren + z.genrecomedy + z.genredrama | group), 
                 data = data, family = poisson(link = "log"), control = control, nAGQ = nAGQ)
})


fixef_results <- fixef(model)
ranef_results <- ranef(model)

yhat <- fitted.values(model)
yhat <- as.data.frame(yhat)
write.csv(yhat, "yhat.csv", row.names = FALSE)

subsample2 <- subsamples[[2]]
predicted_outcome_subsample2 <- predict(model, newdata = subsample2, re.form = NA)  # marginal predictions

# Calculate residuals (actual - predicted)
residuals_subsample2 <- subsample2$y - predicted_outcome_subsample2

# Calculate Mean Squared Error
mse <- mean(residuals_subsample2^2)

# Print the Mean Squared Error
print(mse)


# Assuming 'subsample1' and 'subsample2' are R data frames, here's how you'd save them as CSV files in R.

# Save subsample1 as a CSV file


# This code will create two files named 'subsample1.csv' and 'subsample2.csv' in the current working directory.
# The 'row.names = FALSE' argument is used to prevent writing row names to the file.



# Optionally, save the results
saveRDS(all_results_obs, "glmm_results.rds")





