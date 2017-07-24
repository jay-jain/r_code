
mice_rmse_per <- function(dat, n_iter) {
  require(mice)
  init = mice(dat,maxit=50,method="pmm", print = FALSE)
  meth = init$method
  predM = init$predictorMatrix
  
  imputed <- mice(data = dat, method="pmm", m=n_iter, predictorMatrix = predM, print = FALSE) 
  
  # Extract values from the mice object and calculate standard errors for confidence intervals.
  imputed_raw <- imputed$imp[-1] # element 1 is removed because it was a column of species names.
  
  # Function to extract summary stats (row-wise mean and variance)
  get_mice_stats <- function(dat) {
    data.frame(mean = apply(dat, 1, mean),
               var = apply(dat, 1, var))
  }
  
  mice_summstats <- lapply(imputed_raw, get_mice_stats)
  
  
  # Put the imputed values into the missing data frame
  mice_complete <- complete(imputed)
  
  # Generate data frame with all true and imputed values side-by-side so that we can compare them.
  
  imputed_traits <- mice_complete[,-1]
  true_traits <- species_traits
  is_missing <- is.na(dat[,-c(1)])
  
  # The variances can be concatenated from the list of results
  mice_imputed_variances <- do.call(c, lapply(mice_summstats, '[', , 'var'))
  
  
  # Dataframe with comparison of values.
  comparisondf <- data.frame(imputed_trait =(imputed_traits[is_missing]),
                             imputed_variance = mice_imputed_variances,
                             true_trait = true_traits[, -1][is_missing],
                             trait_id = col(is_missing)[is_missing],  # Vector of column numbers with missing data 
                             species_id = row(is_missing)[is_missing])
  
  return(comparisondf)
}

comparisondf_mice <- mice_rmse_per(data_training,100)

plot_mice <- ggplot(comparisondf_mice, aes(x = species_id)) + facet_wrap(~ trait_id, scales = 'free',labeller = labeller(trait_id = traitNames)) +
  geom_pointrange(aes(y = imputed_trait, 
                      ymin = imputed_trait - 1.96 * sqrt(imputed_variance), 
                      ymax = imputed_trait + 1.96 * sqrt(imputed_variance))) +
  geom_point(aes(y = true_trait), color = 'red', shape = 19) + theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),plot.title = element_text(hjust = 0.5))+
  ggtitle("Imputations with 95% CI at 25% Missing Values using MICE Method\n") +
  labs( x = "Species id", y = "Trait values") + theme_bw() + theme(plot.title = element_text(hjust = 0.5,size=42),legend.position = "none",) 

plot_mice
