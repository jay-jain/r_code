# Demos of imputation packages

#### Rphylopars: phylogenetic imputation.

library(Rphylopars)

# Simulate a bunch of species with some traits per species, data are missing completely at random.
# For now don't use intraspecific variation.
set.seed(81224)
fakedata <- simtraits(ntaxa=15, ntraits=5, nreps=1, nmissing=10, intraspecific=0)
str(fakedata)

plot(fakedata$tree)

fakephy <- phylopars(fakedata$trait_data, fakedata$tree, model = 'BM') # Outputs variance-covariance matrix for traits under Brownian motion model.

# See https://github.com/ericgoolsby/Rphylopars/wiki/Example-2:-Missing-Data-Imputation-and-Ancestral-State-Reconstruction
# View imputed missing data 

str(fakephy)
fakephy$anc_recon # Trait values for tips as well as reconstruction of ancestral nodes.
fakephy$anc_var # Variance around the imputed trait values.

# Compare imputed values from the ancestral state reconstruction with the true means from which the fake data were generated.

imputed_traits <- fakephy$anc_recon[1:15,] # Get only rows from existing species, not the ancestral nodes.
imputed_variances <- fakephy$anc_var[1:15,]
is_missing <- is.na(fakedata$trait_data[,-1])
true_traits <- fakedata$original_X

# Dataframe with comparison of values.
comparisondf <- data.frame(imputed_trait = imputed_traits[is_missing],
                           imputed_variance = imputed_variances[is_missing],
                           true_trait = true_traits[is_missing],
                           trait_id = col(is_missing)[is_missing],
                           species_id = row(is_missing)[is_missing])

# Plot results. The red point is the "true" value. It should fall inside the confidence interval around the black point. It seems to perform pretty well for the fake data.
# Confidence intervals are based on assumption of normal distribution.
library(cowplot)
ggplot(comparisondf, aes(x = interaction(species_id, trait_id))) +
  geom_pointrange(aes(y = imputed_trait, 
                      ymin = imputed_trait - 1.96 * sqrt(imputed_variance), 
                      ymax = imputed_trait + 1.96 * sqrt(imputed_variance))) +
  geom_point(aes(y = true_trait), color = 'red', shape = 1)

