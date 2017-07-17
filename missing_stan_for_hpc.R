# remove dot in trait names because STAN does not like it
colnames(species_traits) <- c("species","Bark_thickness", "Wood_density","SLA", "Plant_height","Plant_lifespan","Seed_dry_mass")

# take log of dataset
species_data <- species_traits[,2:7]

species_traits[2:7] <- log(species_data)

######## 0% missing 
trait_data_list_1 <- make_standatalist(traitdat = species_traits, predictors = species_covariates, phy = species_phylogeny, evolution_model = 'ou')

with(trait_data_list_1, stan_rdump(names(trait_data_list), file = '/home/jay/Desktop/trait_data_list/trait_data_list1.R'))



######## 25% missing 
species_traits_2 <- removeValues(dataframe =  species_traits,proportion = 0.25)

trait_data_list_2 <- make_standatalist_missing(traitdat = species_traits_2, predictors = species_covariates, phy = species_phylogeny, evolution_model = 'ou')


traitmissing_model_2 <- stan_model(file = '/home/jay/Desktop/imputation/phylo_spatial_missing.stan') # Throws a warning but still works!
trait_fit_missing_2 <- sampling(traitmissing_model_2, data = trait_data_list_2, chains = 1, iter = 200, warmup = 100, thin = 1, init = 1) # works!
with(trait_data_list_2, stan_rdump(names(trait_data_list_2), file = '/home/jay/Desktop/trait_data_list/trait_data_list2.R'))

###### Plots
summ_fit_2 <- summary(trait_fit_missing_2)

# Display the summary info for some of the parameters: beta only
summ_fit_2$summary[grep('beta', row.names(summ_fit_2$summary)),]

head(summ_fit_2$summary)

# Diagnostic plots to make sure the models converged
stan_diag(trait_fit_missing_2)

# Must load bayesplot because ggplot2 is no longer compatible with rstan.
library(bayesplot)
draws <- as.array(trait_fit_missing_2, pars="beta")
mcmc_trace(draws)



######## 50% missing
species_traits_3 <- removeValues(dataframe =  species_traits,proportion = 0.5)

trait_data_list_3 <- make_standatalist_missing(traitdat = species_traits_3, predictors = species_covariates, phy = species_phylogeny, evolution_model = 'ou')

traitmissing_model_3 <- stan_model(file = '/home/jay/Desktop/imputation/phylo_spatial_missing.stan') # Throws a warning but still works!
trait_fit_missing_3 <- sampling(traitmissing_model_3, data = trait_data_list_3, chains = 1, iter = 200, warmup = 100, thin = 1, init = 1) # works!

with(trait_data_list_3, stan_rdump(names(trait_data_list_3), file = '/home/jay/Desktop/trait_data_list/trait_data_list3.R'))



######## 75% missing
species_traits_4 <- removeValues(dataframe =  species_traits,proportion = 0.75)

trait_data_list_4 <- make_standatalist_missing(traitdat = species_traits_4, predictors = species_covariates, phy = species_phylogeny, evolution_model = 'ou')
 
traitmissing_model_4 <- stan_model(file = '/home/jay/Desktop/imputation/phylo_spatial_missing.stan') # Throws a warning but still works!
trait_fit_missing_4 <- sampling(traitmissing_model_4, data = trait_data_list_4, chains = 1, iter = 200, warmup = 100, thin = 1, init = 1) # works!

with(trait_data_list_4, stan_rdump(names(trait_data_list_4), file = '/home/jay/Desktop/trait_data_list/trait_data_list4.R'))

