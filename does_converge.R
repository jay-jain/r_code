library(rstan)
setwd('/home/jay/Desktop/shorter/')
read_stan_csv(c('stan_output_9_1.csv','stan_output_9_2.csv','stan_output_9_3.csv'))

summ_fit <- summary(trait_fit)

# Display the summary info for some of the parameters: beta only
summ_fit$summary[grep('beta', row.names(summ_fit$summary)),]

head(summ_fit$summary)


# Diagnostic plots to make sure the models converged
stan_diag(trait_fit)

# Must load bayesplot because ggplot2 is no longer compatible with rstan.
library(bayesplot)
draws <-as.array(model,pars=c("Y_mis[1]",'Y_mis[2]','Y_mis[3]'))
mcmc_trace(draws)
