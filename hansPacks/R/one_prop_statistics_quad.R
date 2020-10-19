#' generates summary statistics for two proportions using quadratic approximation within a bayesian framework
#'
#' This function allows you to quickly generate posterior distributions for a proportions given its sample size, calculate bayesian summary statistics, and visualize the distribution against a hypothetical proportion
#' @param Yes
#' @keywords bayes, ab test, proportion
#' @export
#' @examples
#' one_prop_statistics_quad(0.21,0.22,500,250,10000,0.025,0.975)

one_prop_statistics_quad <- function(p1,n1,simulation_volume,lower_bound,upper_bound,hypothetical_proportion) {

  ## construct p-grid
  p_grid <- seq(0,1,length.out=1000)
  prob_p <- rep(1,1000)

  ## construct posterior for p1
  successes = round(p1*n1,digits=0)
  trials = n1
  failures = trials-successes

  prob_data <- dbinom(successes,trials,prob=p_grid)
  posterior <- prob_data*prob_p

  ## calculate intervals for simulated approximation of posterior distrubtion
  lower_ci <- quantile(posterior,lower_bound)
  upper_ci <- quantile(posterior,upper_bound)

  ## calculate intervals from theoretical beta distribution
  lower_ci_alt = qbeta(successes,failures,lower_bound)
  upper_ci_alt = qbeta(successes,failures,upper_bound)

  ## define proportions
  proportion = p1

  ## define sample size
  sample_size = n1

  ## generate samples with 10k draws from posterior
  samples1 <- sample(p_grid,prob=posterior,size=simulation_volume,replace=TRUE)
  data <- data.frame(value=samples1)

  ## calculate probability that a is greater than b
  prob = sum(samples1 > hypothetical_proportion)/simulation_volume

  ## combine summary stats
  summary_stats <- data.frame(proportion,sample_size,prob,lower_ci,upper_ci,lower_ci_alt,upper_ci_alt)

  ## generate view
  visual <- ggplot(data,aes(x=value)) + geom_density(alpha=0.2) + theme(legend.position='bottom',legend.title=element_blank()) + ylab('Density') + xlab('Range of Hypothetical Proportion Values') + scale_x_continuous(label=scales::percent) + geom_vline(xintercept = hypothetical_proportion,linetype='dashed',colour='red')

  return(list(summary_stats,visual,data))
}
