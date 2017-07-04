# TODO: Make trait variance matrix for mvOU method
# Create visualizations for MICE and RPhylopars methods 

#### Rphylopars: phylogenetic imputation.

#setwd("/home/jay/Desktop/")

library(Rphylopars)
#load("pnwphylo_potter.r")

# Read in stevens data  phylogenetic tree
library(ape)
stevens_tree <-read.tree('/home/jay/Desktop/tree_all_final_031716.txt')


# Simulate a bunch of species with some traits per species, data are missing completely at random.
# For now don't use intraspecific variation.
# set.seed(81224)
# fakedata <- simtraits(ntaxa=15, ntraits=5, nreps=1, nmissing=10, intraspecific=0)
# str(fakedata)

# geospatial
#fia_traitmeans <- read.csv('/home/jay/Desktop/try_location_trait_byobs.csv',head = TRUE, sep = ',', check.names = FALSE)

# traitmeans
#fia_traitmeans <- read.csv('/home/jay/Desktop/try_fia_traitmeans.csv',head = TRUE, sep = ',', check.names = FALSE)

#training dataset
training <- read.csv('/home/jay/Desktop/trait_stevens_training.csv',head = TRUE, sep = ',', check.names = FALSE, stringsAsFactors = FALSE)

View(missingTraining)

colnames(missingTraining)[colnames(missingTraining) == 'Scientific_Name'] <- 'species'


# change species names to same format as phylogenetic tree species' names (i.e. underscores between genus and species)
missingTraining<- transform(training, species = gsub('\\ ', '_', species))

# Logical vector of whether traitmeans species are in phylogenetic tree
spmatch <- missingTraining$species %in% stevens_tree$tip.label

# Which species are not in phylogenetic tree
missingTraining$species[!spmatch]

# change populus to correct name in trait mean df
#training$species[training$species=='Populus_trichocarpa'] <- "Populus_balsamifera_trichocarpa"

#trait_input <- fia_traitmeans[fia_traitmeans$species != 'Abies_shastensis', c(1,3,16,17,18,19) ]



# remove Abies shastensis from traitmeans and impute values using 5 traits

test_tree <- drop.tip(stevens_tree, tip = stevens_tree$tip.label[!stevens_tree$tip.label %in% missingTraining$species])

# make tree multi2di (collapse or resolve multichotomies in phylogenetic trees)
test_tree <- multi2di(test_tree)

# Brownian Motion: 37 negative values
phy_training_BM <- phylopars(missingTraining,test_tree,model='BM')

# OU Method: 9 negative values
phy_training_OU <- phylopars(missingTraining,test_tree,model='OU')

# mvOU Method: throws error: $ operator is invalid for atomic vectors
phy_training_mvOU <- phylopars(missingTraining,test_tree,model='mvOU')

# lambda:16 missing values
phy_training_lambda <- phylopars(missingTraining,test_tree,model='lambda')

# kappa: 17 negative values
phy_training_kappa <- phylopars(missingTraining,test_tree,model='kappa')

# delta: throws Error: NaNs produced
phy_training_delta <- phylopars(missingTraining,test_tree,model='delta')

# EB: 37 misisng values
phy_training_EB <- phylopars(missingTraining,test_tree,model='EB')

# star: 9 negative values
phy_training_star <- phylopars(missingTraining,test_tree,model='star')

# How many values are negative: 
sum(phy_training_star$anc_recon < 0)

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

imputed_traits <- phy_training$anc_recon[1:83,] # Get only rows from existing species, not the ancestral nodes species
imputed_variances <- phy_training$anc_var[1:83,]
is_missing <- is.na(phy_training$trait_data[,-c(1)])
true_traits <- read.csv('/home/jay/Desktop/trait_stevens_training.csv',head = TRUE, sep = ',', check.names = FALSE, stringsAsFactors = FALSE)


# Dataframe with comparison of values.
comparisondf <- data.frame(imputed_trait = imputed_traits[is_missing],
                           imputed_variance = imputed_variances[is_missing],
                           true_trait = true_traits[-1][is_missing],
                           trait_id = col(is_missing)[is_missing],  # Vector of column numbers with missing data 
                           species_id = row(is_missing)[is_missing])

# Plot results. The red point is the "true" value. It should fall inside the confidence interval around the black point. It seems to perform pretty well for the fake data.
# Confidence intervals are based on assumption of normal distribution.
library(cowplot)
ggplot(comparisondf, aes(x = species_id)) +
  facet_wrap(~ trait_id, scales = 'free') +
  geom_pointrange(aes(y = imputed_trait, 
                      ymin = imputed_trait - 1.96 * sqrt(imputed_variance), 
                      ymax = imputed_trait + 1.96 * sqrt(imputed_variance))) +
  geom_point(aes(y = true_trait), color = 'red', shape = 3)


