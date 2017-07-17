# Jay Jain
# July 10, 2017
# MICE imputations on training data set
# The structure of this script is taken heavily from the following source: 
# https://datascienceplus.com/handling-missing-data-with-mice-package-a-simple-approach/

library(mice)
training <- read.csv('/home/jay/Desktop/trait_stevens_training.csv',head = TRUE, sep = ',', check.names = FALSE, stringsAsFactors = FALSE)

# Take log
miceLogTraining <- training
miceLogTraining[,-1] <- log(miceLogTraining[,-1])

# Remove values at random
miceLogTraining <- removeValues(miceLogTraining, proportion = 0.2)



# Most missing values
miceLogTraining[which.max(sapply(miceLogTraining, function(x) sum(is.na(x))))]

# Least missing values
miceLogTraining[which.min(sapply(miceLogTraining, function(x) sum(is.na(x))))]

hist(sapply(miceLogTraining, function(x) sum(is.na(x))),main='Distribution of missing values for tree traits', 
     xlab = 'Percent missing values', ylab = 'Trait columns',border = 'black',col='pink')

training_aggr = aggr(miceLogTraining[,-1], col=mdc(1:2), numbers=TRUE, sortVars=TRUE, labels=names(miceLogTraining[,-1]), cex.axis=.7, gap=3, ylab=c("Proportion of missingness","Missingness Pattern"))


# Use the following command to determine which traits have least missing values: 
sort(sapply(miceLogTraining, function(x) sum(is.na(x))), decreasing = FALSE)


init = mice(miceLogTraining,maxit=50,method="pmm")
meth = init$method
predM = init$predictorMatrix
set.seed(103)

# Imputes missing values using Bayesian linear regression with 100 imputations
imputed <- mice(data = miceLogTraining, method="pmm", m=5,predictorMatrix = predM) 

# converts mids object to vector form
imputed <- complete(imputed)

# Check for missing data in dataset
sapply(imputed, function(x) sum(is.na(x)))

# hist(x = imputed[,-1], main='Distribution of missing values for tree traits after imputation', 
#      xlab = 'Percent missing values', ylab = 'Trait columns', border = 'black', col='pink')

#plot(imputed[,-1])

imputed_traits <- imputed # Get only rows from existing species, not the ancestral nodes species
imputed_variances <- phy_training_OU$anc_var[order(rownames(phy_training_OU$anc_var[1:83,])),] # Order the species names alphabetically
ordered_trait_data <- phy_training_OU$trait_data[order(phy_training_OU$trait_data$species),] # Order trait data species names alphabetically
is_missing <- is.na(ordered_trait_data[,-c(1)])
true_traits <- read.csv('/home/jay/Desktop/trait_stevens_training.csv',head = TRUE, sep = ',', check.names = FALSE, stringsAsFactors = FALSE)

plot(x = training[,-1], y = imputed[,-1])

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



mice_cdf <-data.frame(known=double(99),imputed=double(99))
test <-training[,-1]
mice_cdf$known <- test[missing]
imputed <- imputed[,-1]
mice_cdf$imputed <- imputed[missin]g
