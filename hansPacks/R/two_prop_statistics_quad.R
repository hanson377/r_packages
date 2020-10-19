#' generates summary statistics for two proportions using quadratic approximation within a bayesian framework
#'
#' This function allows you to quickly generate posterior distributions for two proportions given their sample size, calculate summary statistics, and visualize the two distribuitons.
#' @param Yes
#' @keywords bayes, ab test, proportion
#' @export
#' @examples
#' one_prop_statistics_quad(0.21,0.22,500,250,10000,0.025,0.975)

two_prop_statistics_quad <- function(p1,p2,n1,n2,simulation_volume,lower_bound,upper_bound) {

  ## construct p-grid
  p_grid <- seq(0,1,length.out=1000)
  prob_p <- rep(1,1000)

  ## construct posterior for p1
  successes1 = round(p1*n1,digits=0)
  trials1 = n1

  prob_data <- dbinom(successes1,trials1,prob=p_grid)
  posterior1 <- prob_data*prob_p

  ## construct posterior for p2
  successes2 = round(p2*n2,digits=0)
  trials2 = n2

  prob_data <- dbinom(successes2,trials2,prob=p_grid)
  posterior2 <- prob_data*prob_p

  ## calculate intervals for proportion 1
  lower_ci_1 <- quantile(posterior1,lower_bound)
  upper_ci_1 <- quantile(posterior1,upper_bound)

  ## calculate intervals for proportion 2
  lower_ci_2 <- quantile(posterior2,lower_bound)
  upper_ci_2 <- quantile(posterior2,upper_bound)

  ## define proportions
  proportion1 = p1
  proportion2 = p2

  ## define sample size
  sample_size_1 = n1
  sample_size_2 = n2

  ## generate samples with 10k draws from posterior
  samples1 <- sample(p_grid,prob=posterior1,size=simulation_volume,replace=TRUE)
  samples2 <- sample(p_grid,prob=posterior2,size=simulation_volume,replace=TRUE)
  data <- data.frame(p1=samples1,p2=samples2)

  ## calculate probability that a is greater than b
  prob = sum(samples2 > samples1)/simulation_volume

  ## combine summary stats
  summary_stats <- data.frame(proportion1,proportion2,sample_size_1,sample_size_2,prob,lower_ci_1,upper_ci_1,lower_ci_2,upper_ci_2)

  ## generate view
  samples1 <- data.frame(value=samples1)
  samples1$sample <- 'proportion 1'

  samples2 <- data.frame(value=samples2)
  samples2$sample <- 'proportion 2'

  samples <- rbind(samples1,samples2)

  visual <- ggplot(samples,aes(x=value,fill=sample)) + geom_density(alpha=0.2) + theme(legend.position='bottom',legend.title=element_blank()) + ylab('Density') + xlab('Hypothetical Proportion Values') + scale_x_continuous(label=scales::percent)

  return(list(summary_stats,visual,data))
}
