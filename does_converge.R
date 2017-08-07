library(rstan)
setwd('/home/jay/Desktop/shorter/')

# Read in stan output
model <- read_stan_csv(c('stan_output_4_1.csv','stan_output_4_2.csv','stan_output_4_3.csv'))

# Summarize output
summ_fit <- summary(model)

# Display the summary info for Y(missing values) and beta
summ_fit$summary[grep('Y_mis', row.names(summ_fit$summary)),]
summ_fit$summary[grep('beta', row.names(summ_fit$summary)),]


head(summ_fit$summary)


# Diagnostic plots to make sure the models converged
stan_diag(trait_fit)

# Must load bayesplot because ggplot2 is no longer compatible with rstan.
library(bayesplot)
# First three values for Y_mis and beta
draws <-as.array(model,pars=c("Y_mis[1]",'Y_mis[2]','Y_mis[3]'))
draws <-as.array(model,pars=c("beta[1]",'beta[2]','beta[3]'))
mcmc_trace(draws)+ggtitle("Monte Carlo Method Convergence \n") +
  labs( x = "Number of samples", y = "Log transformed trait values") + theme_bw() + theme(plot.title = element_text(hjust = 0.5,size=32)) 
