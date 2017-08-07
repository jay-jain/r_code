# Test stan summary output

summ4 <- read.csv('/home/jay/Desktop/r_code//summ_fit.csv', row.names = 1)


# Loop through the list of loaded stan summaries and output a comparisondf for each one.

true_traits <- species_traits
is_missing <- is.na(missing_datasets[[4]][,-c(1)])

y_miss <- summ4[grepl('Y_mis', row.names(summ4)), ]

imputed_traits <- y_miss$summary.50.
imputed_ci_min <- y_miss$summary.2.5.
imputed_ci_max <- y_miss$summary.97.5.

# Dataframe with comparison of values.

comparisondf_stan <- data.frame(imputed_trait =imputed_traits,
                           imputed_ci_min = imputed_ci_min,
                           imputed_ci_max = imputed_ci_max,
                           true_trait = true_traits[, -1][is_missing],
                           trait_id = col(is_missing)[is_missing],  # Vector of column numbers with missing data 
                           species_id = row(is_missing)[is_missing])

RMSE_eachtrait(comparisondf)

library(ggplot2)
library(cowplot)

plot_stan <- ggplot(comparisondf_stan, aes(x = species_id)) + facet_wrap(~ trait_id, scales = 'free',labeller = labeller(trait_id = traitNames)) +
  geom_pointrange(aes(y = imputed_trait, 
                      ymin = imputed_ci_min, 
                      ymax = imputed_ci_max )) +
  geom_point(aes(y = true_trait), color = 'red', shape = 19) + theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),plot.title = element_text(hjust = 0.5,size=32))+
  ggtitle("Imputations with 95% CI at 25% Missing Values using Hierarchical Model\n") +
  labs( x = "Species id", y = "Trait values") 

plot_stan
