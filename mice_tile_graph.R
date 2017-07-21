comparisondf$delta <- abs((comparisondf$true_trait - comparisondf$imputed_trait)/comparisondf$true_trait)


mice_rmse_graph <- function(dat, n_iter) {
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
  #mice_complete <- complete(imputed)
  
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



for (i in 1:length(missing_datasets)){
  temp <- as.data.frame(missing_datasets[i])
  mice_complete[i] <- mice_rmse_graph(temp,100)
  
}

#comparison_df_list <- list(comparisondf1, comparisondf2)

comparison_df_all <- do.call(rbind, as.data.frame(mice_complete))
comparison_df_all <- as.data.frame(comparison_df_all)

comparison_means <- comparison_df_all %>%
  mutate(delta = abs((true_trait-imputed_trait)/imputed_trait)) %>%
  group_by(trait_id, species_id) %>%
  summarize(delta = mean(delta))

colorRampPalette(c('navyblue','red'))(10)

ggplot(comparison_means, aes(x=trait_id, y=species_id, fill=delta)) + 
  geom_tile() + 
  theme_bw() +
  scale_fill_continuous(low='navyblue', high='red')+
  scale_x_continuous(labels=c("Bark thickness","Wood density","SLA","Plant height","Plant lifespan","Seed dry mass"),breaks=1:6)
