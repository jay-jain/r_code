# This script uses a training data set and omits values completely at random. A phylogenetic tree is used to impute the missing
# values. 

#setwd("/home/jay/Desktop/")

library(Rphylopars)
#load("pnwphylo_potter.r")

# Read in stevens dataphylogenetic tree
library(ape)
stevens_tree <-read.tree('/home/jay/Desktop/r_code/tree_all_final_031716.txt')
plot(stevens_tree)


# geospatial
#fia_traitmeans <- read.csv('/home/jay/Desktop/try_location_trait_byobs.csv',head = TRUE, sep = ',', check.names = FALSE)

# traitmeans
#fia_traitmeans <- read.csv('/home/jay/Desktop/try_fia_traitmeans.csv',head = TRUE, sep = ',', check.names = FALSE)

# stevens training dataset
training <- read.csv('/home/jay/Desktop/trait_stevens_training.csv',head = TRUE, sep = ',', check.names = FALSE, stringsAsFactors = FALSE)

## Change first column names to 'species'
colnames(missingTraining)[colnames(missingTraining) == 'Scientific_Name'] <- 'species'
# change species names to same format as phylogenetic tree species' names (i.e. underscores between genus and species)
missingTraining<- transform(training, species = gsub('\\ ', '_', species))

# Logical vector of whether traitmeans species are in phylogenetic tree
spmatch <- missingTraining$species %in% stevens_tree$tip.label

# Which species are not in phylogenetic tree
missingTraining$species[!spmatch]

# Drop species from phylogenetic tree that are not in dataset
test_tree <- drop.tip(stevens_tree, tip = stevens_tree$tip.label[!stevens_tree$tip.label %in% missingLogTraining$species])

# make tree multi2di (collapse or resolve multichotomies in phylogenetic trees)
test_tree <- multi2di(test_tree)

# Make dataset that is log of training set
log_training <- training
log_training[,-1] <- log(log_training[,-1])

# Remove values from log_Training
missingLogTraining <- removeValues(log_training, proportion = 0.2)

## Change first column names to 'species'
colnames(missingLogTraining)[colnames(missingLogTraining) == 'Scientific_Name'] <- 'species'
# change species names to same format as phylogenetic tree species' names (i.e. underscores between genus and species)
missingLogTraining<- transform(missingLogTraining, species = gsub('\\ ', '_', species))

# Show number of NA's in each trait column
colSums(is.na(missingLogTraining[,2:7]))

phy_training_OU <- phylopars(missingLogTraining,test_tree,model='OU')


####################################################################################
########  Imputations with using different techniques 
####################################################################################
# Brownian Motion: 37 negative values
# phy_training_BM <- phylopars(missingTraining,test_tree,model='BM')
# 
# # OU Method: 9 negative values
# phy_training_OU <- phylopars(missingTraining,test_tree,model='OU')
# 
# # mvOU Method: throws error: $ operator is invalid for atomic vectors
# phy_training_mvOU <- phylopars(missingTraining,test_tree,model='mvOU')
# 
# # lambda:16 missing values
# phy_training_lambda <- phylopars(missingTraining,test_tree,model='lambda')
# 
# # kappa: 17 negative values
# phy_training_kappa <- phylopars(missingTraining,test_tree,model='kappa')
# 
# # delta: throws Error: NaNs produced
# phy_training_delta <- phylopars(missingTraining,test_tree,model='delta')
# 
# # EB: 37 misisng values
# phy_training_EB <- phylopars(missingTraining,test_tree,model='EB')
# 
# # star: 9 negative values
# phy_training_star <- phylopars(missingTraining,test_tree,model='star')
# 
# ####################################################################################
# 
# # How many values from imputed set are negative: 
# sum(phy_training_OU$anc_recon < 0)

# Compare imputed values from the ancestral state reconstruction with the true means from which the fake data were generated.
imputed_traits <- phy_training_OU$anc_recon[1:83,] # Get only rows from existing species, not the ancestral nodes species
imputed_variances <- phy_training_OU$anc_var[order(rownames(phy_training_OU$anc_var[1:83,])),] # Order the species names alphabetically
ordered_trait_data <- phy_training_OU$trait_data[order(phy_training_OU$trait_data$species),] # Order trait data species names alphabetically
is_missing <- is.na(ordered_trait_data[,-c(1)])
true_traits <- read.csv('/home/jay/Desktop/trait_stevens_training.csv',head = TRUE, sep = ',', check.names = FALSE, stringsAsFactors = FALSE)


# Dataframe with comparison of values.
comparisondf <- data.frame(imputed_trait =(imputed_traits[is_missing]),
                           imputed_variance = (imputed_variances[is_missing]),
                           true_trait = log(true_traits[-1][is_missing]),
                           trait_id = col(is_missing)[is_missing],  # Vector of column numbers with missing data 
                           species_id = row(is_missing)[is_missing])

# Convert species_id to actual species name by retrieving index from comparisondf
for (i in 1:length(comparisondf$species_id)){
  #speciesName <- vector(mode = "double", length = length(comparisondf$species_id))
  speciesName <- ordered_trait_data$species[comparisondf$species_id] 
}

traitNames <- c('1'='Bark thickness', '2'='Wood density', '3'='SLA','4' = 'Plant height','5' = 'Plant lifespan','6' = 'Seed dry mass')

# Plot results. The red point is the "true" value. It should fall inside the confidence interval around the black point. It seems to perform pretty well for the fake data.
# Confidence intervals are based on assumption of normal distribution.
library(ggplot2)
library(cowplot)

all_traits <- ggplot(comparisondf, aes(x = speciesName)) + facet_wrap(~ trait_id, scales = 'free',labeller = labeller(trait_id = traitNames)) +
  geom_pointrange(aes(y = imputed_trait, 
                      ymin = imputed_trait - 1.96 * sqrt(imputed_variance), 
                      ymax = imputed_trait + 1.96 * sqrt(imputed_variance))) +
  geom_point(aes(y = true_trait), color = 'red', shape = 19) + theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),plot.title = element_text(hjust = 0.5))+
  ggtitle("Imputations with 95% CI at 5% Missing Values using Training Dataset\n") +
  labs( x = "Species names", y = "Trait values") 

### Trait #1 (Bark thickness) graph
g1 <- ggplot(data = comparisondf[comparisondf$trait_id==1,],aes(x = speciesName[comparisondf$trait_id == 1])) + 
  geom_pointrange(aes(y = imputed_trait[trait_id==1], 
                      ymin = imputed_trait[trait_id==1] - 1.96 * sqrt(imputed_variance[trait_id==1]), 
                      ymax = imputed_trait[trait_id==1] + 1.96 * sqrt(imputed_variance[trait_id==1]))) +
  geom_point(aes(y = true_trait[trait_id==1]), color = 'red', shape = 19) +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5),axis.title.y=element_blank(),axis.title.x=element_blank()) +
  labs(title = "Bark thickness") 

### Trait #2 (Wood Density) graph
g2 <- ggplot(data = comparisondf[comparisondf$trait_id==2,],aes(x = speciesName[comparisondf$trait_id == 2])) + 
  geom_pointrange(aes(y = imputed_trait, 
                      ymin = imputed_trait - 1.96 * sqrt(imputed_variance), 
                      ymax = imputed_trait + 1.96 * sqrt(imputed_variance))) +
  geom_point(aes(y = true_trait), color = 'red', shape = 19) +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5),axis.title.y=element_blank(),axis.title.x=element_blank()) +
  labs(title = "Wood density")

### Trait #3 (SLA) graph
g3 <- ggplot(data = comparisondf[comparisondf$trait_id==3,],aes(x = speciesName[comparisondf$trait_id == 3])) + 
  geom_pointrange(aes(y = imputed_trait, 
                      ymin = imputed_trait - 1.96 * sqrt(imputed_variance), 
                      ymax = imputed_trait + 1.96 * sqrt(imputed_variance))) +
  geom_point(aes(y = true_trait), color = 'red', shape = 19) +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5),axis.title.y=element_blank(),axis.title.x=element_blank()) +
  labs(title = "SLA") 

### Trait #4 (Plant height) graph
g4 <- ggplot(data = comparisondf[comparisondf$trait_id==4,],aes(x = speciesName[comparisondf$trait_id == 4])) + 
  geom_pointrange(aes(y = imputed_trait, 
                      ymin = imputed_trait - 1.96 * sqrt(imputed_variance), 
                      ymax = imputed_trait + 1.96 * sqrt(imputed_variance))) +
  geom_point(aes(y = true_trait), color = 'red', shape = 19) +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 15, hjust = 1),axis.title.y=element_blank(),axis.title.x=element_blank()) +
  labs(title = "Plant height") 

### Trait #5 (Plant lifespan) graph
g5 <- ggplot(data = comparisondf[comparisondf$trait_id==5,],aes(x = speciesName[comparisondf$trait_id == 5])) + 
  geom_pointrange(aes(y = imputed_trait, 
                      ymin = imputed_trait - 1.96 * sqrt(imputed_variance), 
                      ymax = imputed_trait + 1.96 * sqrt(imputed_variance))) +
  geom_point(aes(y = true_trait), color = 'red', shape = 19) +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5),axis.title.x=element_blank()) +
  labs(title = "Plant lifespan", y = "Years") 

### Trait #6 (Seed dry mass) graph
g6<- ggplot(data = comparisondf[comparisondf$trait_id==6,],aes(x = speciesName[comparisondf$trait_id == 6])) + 
  geom_pointrange(aes(y = imputed_trait, 
                      ymin = imputed_trait - 1.96 * sqrt(imputed_variance), 
                      ymax = imputed_trait + 1.96 * sqrt(imputed_variance))) +
  geom_point(aes(y = true_trait), color = 'red', shape = 19) +
  theme_bw() + theme(axis.text.x = element_text(angle = 20, hjust = 1),plot.title = element_text(hjust = 0.5),
                     axis.title.y=element_blank(),axis.title.x=element_blank(),text = element_text(size=10),legend.position="bottom") +
  labs(title = "Seed dry mass") 


library(grid)
library(gridExtra)

grid.arrange(g1, g2,g3,g4,g5,g6, ncol = 2, top = "Imputations with 95% CI at 90% Missing Values using Training Dataset\n",
             bottom = "Species names", left = "Trait values")
#plot.new()
#  legend(x = "bottom",inset = 0,
#       legend = c("Known Value", "Imputed Value"), 
#       col=c("black","red"), lwd=5, cex=.5, horiz = TRUE)

#### Original
# ggplot(comparisondf, aes(x = interaction(species_id, trait_id))) +
#   geom_pointrange(aes(y = imputed_trait, 
#                       ymin = imputed_trait - 1.96 * sqrt(imputed_variance), 
#                       ymax = imputed_trait + 1.96 * sqrt(imputed_variance))) +
#   geom_point(aes(y = true_trait), color = 'red', shape = 1)


# Compute number of imputed values not in 95% confidence interval

inBounds <- vector(mode = "logical",length = nrow(comparisondf))

inBounds = 0
outBounds = 0
for(i in 1:nrow(comparisondf)){
  ymin = comparisondf$imputed_trait[i] - 1.96 * sqrt(comparisondf$imputed_variance[i]) 
  ymax = comparisondf$imputed_trait[i] + 1.96 * sqrt(comparisondf$imputed_variance[i])
  
  if(comparisondf$true_trait[i] > ymin & comparisondf$true_trait[i] < ymax){
    inBounds = inBounds + 1
  }else{
    outBounds = outBounds + 1
  }
}

outBounds/(inBounds + outBounds) * 100 

