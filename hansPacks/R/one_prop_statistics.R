#' Generate Bayesian statistics for one proportion, includes test against a hypothetical value
#'
#' This function allows you to quickly generate summary bayesian statistics, visualize the beta distribution, and compare against a hypothetical value
#' @param Yes
#' @keywords bayes, ab test, proportion
#' @export
#' @examples
#' visualize_two_props()

one_prop_statistics <- function(prior_alpha,prior_beta,proportion,sample_size,prop_to_test,lower_ci,upper_ci) {

sim_trials = 5000000

alpha <- round(proportion*sample_size,digits=0)
beta <- sample_size-alpha

## generate 5 million random draws from beta
sim <- rbeta(sim_trials,prior_alpha+alpha,prior_beta+beta)

## calculate probability
probability_estimate = sum(sim > prop_to_test)/sim_trials

## credible intervals
lower = qbeta(lower_ci,alpha,beta)
upper = qbeta(upper_ci,alpha,beta)

## quantiles
lower_alt = quantile(sim,lower_ci)
upper_alt = quantile(sim,upper_ci)

## create summary statistics
summary_stats <- data.frame(proportion,lower,upper,lower_alt,upper_alt,probability_estimate)

## visualize distribution
sim_df <- data.frame(value=sim)
view <- ggplot(sim_df,aes(x=value)) + geom_density() + geom_vline(xintercept=prop_to_test,linetype='dashed',colour='red') + geom_vline(xintercept=lower,linetype='dashed',colour='blue') + geom_vline(xintercept=upper,linetype='dashed',colour='blue') + geom_vline(xintercept=upper_alt,linetype='dashed',colour='blue') + geom_vline(xintercept=lower_alt,linetype='dashed',colour='blue') + scale_x_continuous(label=scales::percent) + xlab('Range of Possible Proportions')

return(list(summary_stats,view))
}
