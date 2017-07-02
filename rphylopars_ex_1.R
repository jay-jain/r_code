# TODO: Make trait variance matrix for mvOU method
# Create visualizations for MICE and RPhylopars methods 

#### Rphylopars: phylogenetic imputation.

#setwd("/home/jay/Desktop/")

library(Rphylopars)
load("pnwphylo_potter.r")
# Simulate a bunch of species with some traits per species, data are missing completely at random.
# For now don't use intraspecific variation.
# set.seed(81224)
# fakedata <- simtraits(ntaxa=15, ntraits=5, nreps=1, nmissing=10, intraspecific=0)
# str(fakedata)

# geospatial
#fia_traitmeans <- read.csv('/home/jay/Desktop/try_location_trait_byobs.csv',head = TRUE, sep = ',', check.names = FALSE)

# traitmeans
fia_traitmeans <- read.csv('/home/jay/Desktop/try_fia_traitmeans.csv',head = TRUE, sep = ',', check.names = FALSE)

#training dataset
training <- read.csv('/home/jay/Desktop/trait_stevens_training.csv',head = TRUE, sep = ',', check.names = FALSE)

View(training)

colnames(fia_traitmeans)[colnames(fia_traitmeans) == 'Scientific_Name'] <- 'species'

# change AccSpeciesName to species 
colnames(fia_traitmeans)[colnames(fia_traitmeans) == 'AccSpeciesName'] <- 'species'

# change species names to same format as phylogenetic tree species' names (i.e. underscores between genus and species)
fia_traitmeans <- transform(fia_traitmeans, species = gsub('\\ ', '_', species))

# Logical vector of whether traitmeans species are in phylogenetic tree
spmatch <- fia_traitmeans$species %in% pnwphylo$tip.label

# Which species are not in phylogenetic tree
fia_traitmeans$species[!spmatch]

# change populus to correct name in trait mean df
fia_traitmeans$species[fia_traitmeans$species=='Populus_trichocarpa'] <- "Populus_balsamifera_trichocarpa"

trait_input <- fia_traitmeans[fia_traitmeans$species != 'Abies_shastensis', c(1,3,16,17,18,19) ]

# remove Abies shastensis from traitmeans and impute values using 5 traits
try_phy <- phylopars(trait_input,pnwphylo,model='OU')

# How many values are negative:
sum(try_phy$anc_recon < 0)

# Sample data from traitmeans
test_data <- fia_traitmeans[sample(nrow(fia_traitmeans), 5), c(1, 16, 18, 19)]
# Drop species from phylogenetic tree that are not in trait_means
test_tree <- drop.tip(pnwphylo, tip = pnwphylo$tip.label[!pnwphylo$tip.label %in% test_data$species])
# Run imputation on test_data
try_phy_test <- phylopars(test_data,test_tree,model='OU')

plot(try_phy$tree)

# See https://github.com/ericgoolsby/Rphylopars/wiki/Example-2:-Missing-Data-Imputation-and-Ancestral-State-Reconstruction


# View imputed missing data 
#str(try_phy)
#try_phy$anc_recon # Trait values for tips as well as reconstruction of ancestral nodes.
#try_phy$anc_var # Variance around the imputed trait values.

# Compare imputed values from the ancestral state reconstruction with the true means from which the fake data were generated.

imputed_traits <- try_phy$anc_recon[1:97,] # Get only rows from existing species, not the ancestral nodes.
imputed_variances <- try_phy$anc_var[1:97,]
is_missing <- is.na(try_phy$trait_data[,-c(1)])
true_traits <- fia_traitmeans[,-c(1)] # Not used 

# Dataframe with comparison of values.
comparisondf <- data.frame(imputed_trait = imputed_traits[is_missing],
                           imputed_variance = imputed_variances[is_missing],
                           true_trait = trait_input[,-1][is_missing],
                           trait_id = col(is_missing)[is_missing],
                           species_id = row(is_missing)[is_missing])

# Plot results. The red point is the "true" value. It should fall inside the confidence interval around the black point. It seems to perform pretty well for the fake data.
# Confidence intervals are based on assumption of normal distribution.
library(cowplot)
ggplot(comparisondf, aes(x = species_id),labs = (x="Species")) +
  facet_wrap(~ trait_id, scales = 'free') +
  geom_pointrange(aes(y = imputed_trait, 
                      ymin = imputed_trait - 1.96 * sqrt(imputed_variance), 
                      ymax = imputed_trait + 1.96 * sqrt(imputed_variance))) +
  geom_point(aes(y = true_trait), color = 'red', shape = 1)

